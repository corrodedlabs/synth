package dev.synth.app

import android.content.Context
import android.os.Handler
import android.os.Looper
import android.util.Log
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateListOf
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.setValue
import dev.synth.core.AbandonChoice
import dev.synth.core.ArrangementPreset
import dev.synth.core.BidValue
import dev.synth.core.CardModel
import dev.synth.core.EMOTE_COOLDOWN_MS
import dev.synth.core.GameAction
import dev.synth.core.GameModel
import dev.synth.core.GamePacing
import dev.synth.core.GamePacingProfile
import dev.synth.core.GamePhase
import dev.synth.core.GameSession
import dev.synth.core.LeaderboardRow
import dev.synth.core.MatchSide
import dev.synth.core.PendingRequest
import dev.synth.core.PlayerIndex
import dev.synth.core.RoomInfo
import dev.synth.core.SessionCallbacks
import dev.synth.core.Suit
import dev.synth.core.actingHostEmail
import dev.synth.core.arrangedCardIds
import dev.synth.core.canExposeTrump
import dev.synth.core.gameReducer
import dev.synth.core.isLegalPlay
import dev.synth.core.memberDisplayName
import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.SupervisorJob

/** A short table message, such as "You take 4". */
const val TRUMP_REVEAL_MS = 2_300L

data class TableMoment(val title: String, val detail: String = "")
data class BidNote(val seat: Int, val text: String)
data class EmoteNote(val seat: Int, val glyph: String, val key: Long)
data class TrumpReveal(val suit: Suit, val shownBy: String)
data class TrickWin(val seat: Int, val key: Long)

/**
 * Owns the session and the model, as GameState.ts does on the web: every action updates the model, then
 * the Compose screens (through state) and the native table (through commands). The server decides
 * everything; this only presents it. Lives on the main thread.
 */
class GameController(context: Context) : SessionCallbacks {
    private val store = PrefsStore(context)
    private val handler = Handler(Looper.getMainLooper())
    private val scope = CoroutineScope(SupervisorJob() + Dispatchers.Main.immediate)
    private val gamePacing = GamePacing()

    var serverUrl: String = BuildConfig.SERVER_URL
        private set
    private var session: GameSession = newSession()

    private var current = GameModel()

    // What the screens read.
    var uiModel by mutableStateOf(GameModel())
        private set
    var statusText by mutableStateOf("")
        private set
    var rooms by mutableStateOf<List<RoomInfo>>(emptyList())
        private set
    var showingRooms by mutableStateOf(false)
        private set
    var standings by mutableStateOf<List<LeaderboardRow>?>(null)
        private set
    var isReconnecting by mutableStateOf(false)
        private set
    var updateRequiredVersion by mutableStateOf<Int?>(null)
        private set
    var abandonedName by mutableStateOf<String?>(null)
        private set
    var moment by mutableStateOf<TableMoment?>(null)
        private set
    var muted by mutableStateOf(store.muted)
        private set
    var joinTarget by mutableStateOf<String?>(null)
    val bidNotes = mutableStateListOf<BidNote>()
    val emotes = mutableStateListOf<EmoteNote>()
    // Just sent one: the server would drop another this soon.
    var emoteCooling by mutableStateOf(false)
        private set
    var arrangement by mutableStateOf<ArrangementPreset?>(null)
        private set
    // The verdict of the hand just finished, for the between-hands panel. Null until it arrives.
    var handResult by mutableStateOf<GameAction.HandResult?>(null)
        private set
    // How many hands the finished match took, as the server counted them.
    var matchHands by mutableStateOf<Int?>(null)
        private set
    var reduceMotion by mutableStateOf(store.reduceMotion)
        private set
    // A short vibration each time this ticks.
    var thumps by mutableStateOf(0)
        private set
    var trumpReveal by mutableStateOf<TrumpReveal?>(null)
        private set
    var trickWin by mutableStateOf<TrickWin?>(null)
        private set
    private var dealOrder: List<String> = emptyList()
    private var trickKey = 0L
    // Play with bots: seats the bots and starts by itself once the table exists.
    private var quickStart = false
    private var botsAskedAt = -1
    private var emoteKey = 0L

    val playerName: String get() = store.playerName ?: ""
    val selfEmail: String get() = session.email
    val isActingHost: Boolean get() = actingHostEmail(uiModel.members) == session.email
    val canExpose: Boolean get() = canExposeTrump(uiModel)
    val canArrange: Boolean get() = uiModel.hand.size > 1 && uiModel.phase != GamePhase.IDLE

    init {
        TableBridge.onPlay = { token -> playToken(token) }
    }

    private fun newSession() = GameSession(this, OkHttpSocketFactory(), store, serverUrl, scope)

    /** The test machine's server, for debug builds. */
    fun useServer(url: String) {
        if (url == serverUrl) return
        session.destroy()
        serverUrl = url
        session = newSession()
    }

    /** The app came up: back to a running match if there was one. */
    fun start() {
        command("mute ${if (muted) 1 else 0}")
        val match = session.storedActiveMatch()
        if (match != null && current.phase == GamePhase.IDLE) session.rejoinMatch(match.name, match.email)
    }

    /** The native table is new (or newly listening): tell it everything. */
    fun resync() {
        command("mute ${if (muted) 1 else 0}")
        sendPacing()
        command("reset")
        if (current.hand.isNotEmpty()) command("hand " + current.hand.joinToString(" ", transform = ::token))
        current.playedCards.forEach { command("place ${it.playerIndex} ${token(it.card)}") }
        current.activePlayer?.let { command("active $it") }
        sendLegal()
    }

    fun foreground() = session.nudge()

    // What the screens can ask for.
    fun createTable(name: String, target: Int) { click(); quickStart = false; session.createTable(name, target) }
    fun playWithBots(name: String, target: Int) {
        click()
        session.createTable(name, target)
        quickStart = true
        botsAskedAt = -1
    }
    fun browse(name: String) { click(); showingRooms = true; session.browseTables(name) }
    fun closeRooms() { click(); showingRooms = false; session.goHome() }
    fun refreshRooms() = session.refreshRooms()
    fun join(room: String) { click(); showingRooms = false; session.join(room) }
    fun joinByLink(name: String, room: String) { click(); joinTarget = null; session.joinByLink(name, room) }
    fun showStandings(name: String) { click(); session.showStandings(name) }
    fun closeStandings() { click(); standings = null; session.goHome() }
    fun addBot(seat: Int? = null) { click(); session.addBot(seat) }
    fun startGame() { click(); session.startGame() }
    fun leaveTable() { click(); session.leaveRoom() }
    fun leaveMatch() { click(); session.leaveMatch() }
    fun kick(member: String) { click(); session.kick(member) }
    fun bid(value: Int) { click(); session.bid(value) }
    fun pass() { click(); session.pass() }
    fun chooseTrump(suit: Suit) { click(); session.chooseTrump(suit) }
    fun exposeTrump() { click(); session.exposeTrump() }
    fun nextHand() { click(); session.nextHand() }
    fun resolveAbandon(choice: AbandonChoice) { click(); abandonedName = null; session.resolveAbandon(choice) }
    fun abandonInterrupted() { click(); session.abandonInterruptedMatch() }
    fun sendEmote(id: String) {
        if (!session.sendEmote(id)) return
        emoteCooling = true
        handler.postDelayed({ emoteCooling = false }, EMOTE_COOLDOWN_MS)
    }
    fun goHome() { click(); statusText = ""; session.goHome() }
    fun displayName(member: String): String = memberDisplayName(member)

    fun thump() { thumps++ }

    fun toggleReduceMotion() {
        reduceMotion = !reduceMotion
        store.reduceMotion = reduceMotion
    }

    fun toggleMute() {
        muted = !muted
        store.muted = muted
        command("mute ${if (muted) 1 else 0}")
    }

    fun arrange(preset: ArrangementPreset) {
        click()
        arrangement = preset
        val ids = arrangedCardIds(current.hand, preset, current.trumpSuit, dealOrder)
        val byId = current.hand.associateBy { it.id }
        command("order " + ids.mapNotNull { byId[it] }.joinToString(" ", transform = ::token))
    }

    private fun click() = command("sound click")

    private fun playToken(token: String) {
        val card = current.hand.firstOrNull { token(it) == token } ?: return
        session.playCard(card)
    }

    // SessionCallbacks
    override fun getModel(): GameModel = current
    override fun getPacing(): GamePacingProfile = gamePacing.profile()
    override fun status(text: String) { statusText = text }
    override fun rooms(rooms: List<RoomInfo>) { this.rooms = rooms }
    override fun seatAbandoned(name: String) { abandonedName = name }
    override fun seatResolved() { abandonedName = null }
    override fun leaderboard(rows: List<LeaderboardRow>) { standings = rows }
    override fun reconnecting(active: Boolean) { isReconnecting = active }
    override fun updateRequired(minVersion: Int) { updateRequiredVersion = minVersion }

    override fun emote(seat: PlayerIndex, emote: String) {
        command("sound emote")
        val note = EmoteNote(seat, emote, emoteKey++)
        emotes.add(note)
        handler.postDelayed({ emotes.remove(note) }, 2_400)
    }

    override fun dispatch(action: GameAction) {
        val before = current
        current = gameReducer(current, action)
        uiModel = current
        present(action, before)
        continueQuickStart()
    }

    private fun continueQuickStart() {
        if (!quickStart || current.phase != GamePhase.LOBBY || !current.isHost) return
        val seated = current.members.size
        if (seated >= 4) {
            quickStart = false
            session.startGame()
        } else if (seated != botsAskedAt) {
            botsAskedAt = seated
            session.addBot()
        }
    }

    private fun clearMoments() {
        moment = null
        trumpReveal = null
        trickWin = null
    }

    private fun present(action: GameAction, before: GameModel) {
        when (action) {
            is GameAction.PhaseChanged -> {
                if (action.phase == GamePhase.IDLE) {
                    command("reset")
                    abandonedName = null
                    bidNotes.clear()
                    arrangement = null
                    clearMoments()
                    handResult = null
                    matchHands = null
                    quickStart = false
                }
            }
            is GameAction.RoomLeft -> {
                command("reset")
                abandonedName = null
                bidNotes.clear()
                arrangement = null
                clearMoments()
                handResult = null
                matchHands = null
                quickStart = false
            }
            is GameAction.RoomEntered -> showingRooms = false
            is GameAction.HandDealt -> {
                val first = current.hand.size == action.cards.size
                if (first) {
                    command("reset")
                    bidNotes.clear()
                    arrangement = null
                }
                sendPacing()
                command("sound deal")
                command("deal " + action.cards.joinToString(" ", transform = ::token))
                if (current.hand.size == 8) dealOrder = current.hand.map { it.id }
            }
            is GameAction.CardPlayed -> {
                // The auction is over once cards are down, and its seals would lie on top of them.
                bidNotes.clear()
                command("play ${action.playerIndex} ${token(action.card)}")
                // The sound and the vibration belong to the landing.
                handler.postDelayed({
                    command("sound card")
                    if (action.playerIndex == 0) thump()
                }, getPacing().cardTravelMs.toLong())
            }
            is GameAction.ActivePlayerChanged -> {
                command("active ${action.playerIndex ?: -1}")
                // Their last call is stale now, and that place shows that it's their turn.
                bidNotes.removeAll { it.seat == action.playerIndex && it.seat != 0 }
            }
            is GameAction.BidPlaced -> {
                command("sound bid")
                bidNotes.removeAll { it.seat == action.playerIndex }
                bidNotes.add(BidNote(action.playerIndex, if (action.bid is BidValue.Amount) "${(action.bid as BidValue.Amount).value}" else "Pass"))
            }
            is GameAction.BidWon -> showMoment(
                TableMoment((if (action.playerIndex == 0) "You" else seatName(action.playerIndex)) + " won the bid at ${action.bid}"),
                2_000,
            )
            is GameAction.TrumpRevealed -> {
                command("sound trump")
                val reveal = TrumpReveal(action.suit, if (action.playerIndex == 0) "Shown by you" else "Shown by ${seatName(action.playerIndex)}")
                trumpReveal = reveal
                handler.postDelayed({ if (trumpReveal === reveal) trumpReveal = null }, TRUMP_REVEAL_MS)
            }
            is GameAction.RequestReceived -> {
                command("sound turn")
                statusText = when (action.request) {
                    is PendingRequest.Trump -> ""
                    is PendingRequest.Play -> "Your turn"
                    else -> statusText
                }
                sendLegal()
            }
            is GameAction.RequestCleared -> sendLegal()
            is GameAction.TrickCleared -> {
                command("clear")
                moment = null
            }
            is GameAction.TrickEnded -> {
                statusText = ""
                val winner = action.winner
                if (winner != null) {
                    val taker = if (winner == 0) "You take" else "${seatName(winner)} takes"
                    showMoment(TableMoment("$taker ${action.points}"), getPacing().trickResolveMs.toLong())
                    trickWin = TrickWin(winner, trickKey++)
                    handler.postDelayed({ command("sound sweep") }, getPacing().winnerRevealMs.toLong())
                }
                command("trick ${winner ?: -1}")
            }
            is GameAction.HandResult -> {
                handResult = action
                command(if (action.made == (action.bidder % 2 == 0)) "sound win" else "sound lose")
            }
            is GameAction.HandReset -> {
                handResult = null
                clearMoments()
                command("reset")
                bidNotes.clear()
                arrangement = null
            }
            is GameAction.MatchOver -> {
                matchHands = action.hands
                command(if (action.winner == MatchSide.US) "sound fanfare_win" else "sound fanfare_lose")
            }
            is GameAction.SnapshotRestored -> {
                handResult = null
                clearMoments()
                resync()
            }
            else -> Unit
        }
    }

    private fun showMoment(value: TableMoment, milliseconds: Long) {
        moment = value
        handler.postDelayed({ if (moment === value) moment = null }, milliseconds)
    }

    private fun sendPacing() {
        val p = getPacing()
        command("pacing ${p.cardTravelMs} ${p.winnerRevealMs} ${p.cardCollectMs} ${p.dealCardStaggerMs}")
    }

    private fun sendLegal() {
        val legal = if (current.pendingRequest is PendingRequest.Play) current.hand.filter { isLegalPlay(current, it) } else emptyList()
        command("legal " + legal.joinToString(" ", transform = ::token))
    }

    private fun seatName(seat: Int): String = current.seatNames?.getOrNull(seat) ?: "Seat $seat"
    private fun token(card: CardModel): String = "${card.suit.id}-${card.rank.id}"
    private fun command(line: String) = TableBridge.command(line)

    // For the debug bridge.
    fun dropSocket() = session.dropConnection()

    fun logState() {
        val m = current
        Log.i("synth", "state phase=${m.phase.name} room=${m.roomName} members=${m.members.size} hand=${m.hand.size} " +
            "played=${m.playedCards.size} active=${m.activePlayer} pending=${m.pendingRequest?.javaClass?.simpleName} " +
            "minBid=${(m.pendingRequest as? PendingRequest.Bid)?.minBid} updateRequired=$updateRequiredVersion " +
            "match=${m.matchUs}-${m.matchThem} winner=${m.matchWinner} reconnecting=$isReconnecting status='$statusText'")
    }
}
