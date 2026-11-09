// Port of app/src/game/GameSession.ts, plus the session lifecycle that lives
// in app/src/game/GameState.ts on the web (start or reuse a session, rejoin
// retry behind the reconnecting state, leave match). Rendering is not ported.
// Keep in step with both.
package dev.synth.core

import kotlinx.coroutines.CancellationException
import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.Job
import kotlinx.coroutines.channels.Channel
import kotlinx.coroutines.channels.ReceiveChannel
import kotlinx.coroutines.coroutineScope
import kotlinx.coroutines.delay
import kotlinx.coroutines.launch

// --- the outside world ---

class SocketError(message: String) : Exception(message)

fun interface GameSocketFactory {
    /**
     * Opens a connection and returns once it is usable. Throws when it cannot
     * connect ([SocketError] for a message worth showing). Must time out on its
     * own and must be cancellable: a retry waits on this call.
     */
    suspend fun connect(url: String): GameSocket
}

interface GameSocket {
    /**
     * Incoming text frames, in order. Closing this channel (with or without a
     * cause) is the failure signal: frames already buffered are still read
     * first, then the session sees the disconnect. Use an unbounded channel,
     * so that a burst of frames is never dropped.
     */
    val incoming: ReceiveChannel<String>
    val isOpen: Boolean

    /** Queues a text frame. On a dead connection this drops it and must not throw. */
    fun send(text: String)

    /** Closes on purpose, flushing queued frames. [incoming] need not close: nobody reads it after this. */
    fun close()

    /** Severs the wire as a phone's OS would: [incoming] closes, so the session walks the disconnect path. */
    fun drop()
}

data class StoredMatch(val email: String, val name: String)

// Replaces localStorage. Identity persists across launches so a reconnecting
// app presents the same email and gets its seat back; activeMatch tells the
// next launch that there is a match worth rejoining.
interface SessionStore {
    var playerId: String?
    var playerName: String?
    var activeMatch: String?
}

interface SessionCallbacks {
    // Must apply gameReducer synchronously: getModel is read straight after.
    fun dispatch(action: GameAction)
    fun getModel(): GameModel
    fun getPacing(): GamePacingProfile
    fun status(text: String)
    fun rooms(rooms: List<RoomInfo>)

    // a table-mate's emote, already translated to a view seat
    fun emote(seat: PlayerIndex, emote: String)

    // someone abandoned (display name) — show the replace-or-end choice
    fun seatAbandoned(name: String)

    // the abandoned seat got a bot (or the decision resolved elsewhere)
    fun seatResolved()

    // the standings arrived
    fun leaderboard(rows: List<LeaderboardRow>)

    // Hold the player on a screen with no create/join actions while true: a
    // second table under the same identity would corrupt both matches.
    fun reconnecting(active: Boolean)

    // The server refuses this client. Retries have stopped; the stored match
    // is kept so the updated app can still rejoin it.
    fun updateRequired(minVersion: Int)
}

// What the session should do once the socket is up.
enum class SessionIntent { CREATE_ROOM, BROWSE_ROOMS, REJOIN, JOIN, STANDINGS }

enum class AbandonChoice { REPLACE, CLOSE }

// App-level liveness ping so hosting proxies (fly.io) don't cull a quiet lobby socket.
const val PING_INTERVAL_MS = 25_000L

// connect-user is acked with an unparseable #<void>; give the server a beat
// so the user exists before the room is created.
const val CONNECT_BEAT_MS = 300L

const val REJOIN_RETRY_MS = 3_000L

// A little over the server's gap between one player's emotes. Two taps this far apart can still reach it
// closer together on a bad connection, and it would drop the second after we had already shown it.
const val EMOTE_GAP_MS = 300L

private const val MAX_PLAYERS = 4

private fun randomPlayerId(): String {
    val alphabet = "abcdefghijklmnopqrstuvwxyz0123456789"
    return (1..6).map { alphabet.random() }.joinToString("")
}

/**
 * One player's connection to the game server, across reconnects.
 *
 * Not thread-safe, like the JS it mirrors: call it from one thread and give it
 * a [scope] confined to that thread (on Android, a scope on Dispatchers.Main).
 * Callbacks arrive there too.
 */
class GameSession(
    private val callbacks: SessionCallbacks,
    private val socketFactory: GameSocketFactory,
    private val store: SessionStore,
    private val serverUrl: String,
    private val scope: CoroutineScope,
    private val newPlayerId: () -> String = ::randomPlayerId,
) {
    private var socket: GameSocket? = null
    private var sessionJob: Job? = null
    private var retryJob: Job? = null
    private var emoteCooldown: Job? = null
    // The web's `session !== null`: true from start until the session dies.
    private var live = false
    private var reconnecting = false
    private var requiredVersion: Int? = null

    private var myServerIndex = 0
    private var inRoom = false
    private var joinedRoomName: String? = null
    private var cardsInTrick = 0
    private var trickPoints = 0
    private var pointsTaken = mutableListOf(0, 0, 0, 0) // accumulated per view index
    private val locallyPlayed = mutableSetOf<String>()
    private var chosenTrump: Suit? = null
    private var justExposed = false // we called for the trump, must trump if able

    var email: String = ""
        private set
    var displayName: String = ""
        private set
    var roomName: String = ""
        private set

    // game points the table we create will play to (quick 2 / classic 6)
    var tableTarget = 6

    // --- stored identity ---

    // the match (email + display name) the previous launch was playing, if any
    fun storedActiveMatch(): StoredMatch? {
        val stored = store.activeMatch
        if (stored.isNullOrEmpty()) return null
        return StoredMatch(stored, store.playerName ?: "Player")
    }

    fun storedPlayerName(): String? = store.playerName

    // the player gives up on an interrupted match: the seat is left to expire
    // in its server-side grace
    fun forgetActiveMatch() {
        store.activeMatch = null
    }

    private fun persistentPlayerId(): String {
        val existing = store.playerId
        if (!existing.isNullOrEmpty()) return existing
        return newPlayerId().also { store.playerId = it }
    }

    // --- starting a session ---

    fun createTable(playerName: String, target: Int = 6) =
        startSession(playerName, SessionIntent.CREATE_ROOM, tableTarget = target)

    fun browseTables(playerName: String) = startSession(playerName, SessionIntent.BROWSE_ROOMS)

    // the launch found an interrupted match: reconnect under the exact email
    // that holds the seat and ask for the snapshot
    fun rejoinMatch(playerName: String, email: String) =
        startSession(playerName, SessionIntent.REJOIN, rejoinEmail = email)

    // An invite link: connect and sit straight down at the named table.
    fun joinByLink(playerName: String, roomName: String) =
        startSession(playerName, SessionIntent.JOIN, joinRoom = roomName)

    // The standings panel: ask over the live session, or connect for it.
    fun showStandings(playerName: String) {
        if (live) requestLeaderboard() else startSession(playerName, SessionIntent.STANDINGS)
    }

    private fun startSession(
        playerName: String,
        intent: SessionIntent,
        joinRoom: String? = null,
        rejoinEmail: String? = null,
        tableTarget: Int? = null,
    ) {
        requiredVersion?.let {
            callbacks.updateRequired(it)
            return
        }
        if (live) {
            // already connected (e.g. browsing) — just issue the new intent
            if (intent == SessionIntent.CREATE_ROOM) {
                if (tableTarget != null) this.tableTarget = tableTarget
                createRoom()
            } else if (intent == SessionIntent.JOIN && joinRoom != null) {
                join(joinRoom)
            } else {
                refreshRooms()
            }
            return
        }

        retryJob?.cancel()
        live = true
        myServerIndex = 0
        inRoom = false
        joinedRoomName = null
        resetHandState()

        val id = persistentPlayerId()
        val name = playerName.trim { isJsSpace(it) }
        val slug = name.lowercase()
            .replace(Regex("[^a-z0-9]+"), "-")
            .replace(Regex("^-+|-+\\z"), "")
            .ifEmpty { "player" }
        displayName = name.ifEmpty { "Player" }
        // a rejoining client must present exactly the email that holds the seat
        email = rejoinEmail ?: "$slug-$id@game.local"
        roomName = "$displayName's table #${id.take(3)}"
        store.playerName = displayName
        this.tableTarget = tableTarget ?: 6

        sessionJob = scope.launch { program(intent, joinRoom) }
    }

    // Stops everything for good, keeping the stored match: for when the owner
    // of this session goes away.
    fun destroy() {
        retryJob?.cancel()
        teardown()
        setReconnecting(false)
    }

    private fun teardown() {
        live = false
        sessionJob?.cancel()
        sessionJob = null
        socket?.close()
        socket = null
    }

    private fun setReconnecting(active: Boolean) {
        if (reconnecting == active) return
        reconnecting = active
        callbacks.reconnecting(active)
    }

    // --- lobby input (called from the UI) ---

    fun createRoom() {
        if (socket == null || inRoom) return
        send(ClientCommand.MakeRoom(email, roomName, tableTarget))
    }

    fun refreshRooms() {
        if (socket == null || inRoom) return
        send(ClientCommand.GetActiveRooms)
    }

    fun join(roomName: String) {
        if (socket == null || inRoom) return
        joinedRoomName = roomName
        send(ClientCommand.JoinRoom(roomName, email))
        callbacks.status("Joining the table…")
    }

    fun leaveRoom() {
        val room = callbacks.getModel().roomName
        if (socket == null || !inRoom || room == null) return
        send(ClientCommand.LeaveRoom(room, email))
    }

    fun kick(targetEmail: String) {
        val model = callbacks.getModel()
        val room = model.roomName
        if (socket == null || !model.isHost || room == null) return
        if (targetEmail == email) return
        send(ClientCommand.KickFromRoom(room, email, targetEmail))
    }

    fun addBot(seat: Int? = null) {
        val model = callbacks.getModel()
        if (socket == null || !model.isHost || model.members.size >= MAX_PLAYERS) return
        send(ClientCommand.AddBot(roomName, seat))
    }

    fun startGame() {
        val model = callbacks.getModel()
        if (socket == null || !model.isHost || model.members.size != MAX_PLAYERS) return
        send(ClientCommand.StartGame(roomName))
        callbacks.status("Starting…")
    }

    // --- player input (called from the UI when the server is waiting on us) ---

    fun submitBid(bid: BidValue) {
        if (callbacks.getModel().pendingRequest !is PendingRequest.Bid || socket == null) return
        send(ClientCommand.PutBid(email, bid))
        callbacks.dispatch(GameAction.RequestCleared)
    }

    fun bid(value: Int) = submitBid(BidValue.Amount(value))

    fun pass() = submitBid(BidValue.Pass)

    fun chooseTrump(suit: Suit) {
        if (callbacks.getModel().pendingRequest !is PendingRequest.Trump || socket == null) return
        chosenTrump = suit
        send(ClientCommand.SelectTrump(email, suit))
        callbacks.dispatch(GameAction.RequestCleared)
    }

    fun playCard(card: CardModel): Boolean {
        if (socket == null || !isLegalPlay(callbacks.getModel(), card)) return false
        send(ClientCommand.PlayCard(email, card))
        locallyPlayed.add(card.id)
        // Render our own play immediately; the server's broadcast is deduped below.
        callbacks.dispatch(GameAction.CardPlayed(card, 0))
        callbacks.dispatch(GameAction.RequestCleared)
        return true
    }

    fun playCardById(cardId: String): Boolean {
        val card = callbacks.getModel().hand.firstOrNull { it.id == cardId } ?: return false
        return playCard(card)
    }

    fun legalCards(): List<String> {
        val model = callbacks.getModel()
        return model.hand.filter { isLegalPlay(model, it) }.map { it.id }
    }

    fun exposeTrump() {
        if (socket == null || !canExposeTrump(callbacks.getModel())) return
        justExposed = true
        send(ClientCommand.ExposeTrump(email))
        callbacks.dispatch(GameAction.RequestCleared)
    }

    // Acting host only, between hands: ask the server to deal the next one.
    // (The duty moves to the next human if the original host's seat was
    // replaced by a bot.)
    fun nextHand() {
        val model = callbacks.getModel()
        if (
            socket == null ||
            actingHostEmail(model.members) != email ||
            model.phase != GamePhase.HAND_FINISHED
        ) {
            return
        }
        send(ClientCommand.NextHand(email))
    }

    fun requestLeaderboard() {
        send(ClientCommand.GetLeaderboard)
    }

    // Answer the abandon gate: keep playing with a bot, or end the match.
    fun resolveAbandon(choice: AbandonChoice) {
        if (socket == null) return
        send(
            when (choice) {
                AbandonChoice.REPLACE -> ClientCommand.ReplaceWithBot(email)
                AbandonChoice.CLOSE -> ClientCommand.CloseGame(email)
            }
        )
    }

    // Walking out on purpose: the server aborts the match for everyone at
    // once instead of granting our seat a reconnect grace window. Then reset
    // to the start screen; the next create/browse builds a fresh connection.
    fun leaveMatch() {
        if (!live) return
        leaveGame()
        retryJob?.cancel()
        teardown()
        callbacks.dispatch(GameAction.RoomLeft)
        callbacks.status("You left the match.")
        setReconnecting(false)
    }

    // Give up on an interrupted match from the reconnecting screen. A retry
    // may be mid-flight (its socket may even be live again) — turn it into an
    // explicit leave; otherwise just forget the stored seat and let it expire
    // in its server-side grace window.
    fun abandonInterruptedMatch() {
        retryJob?.cancel()
        if (live) {
            leaveGame()
            teardown()
        } else {
            forgetActiveMatch()
        }
        callbacks.dispatch(GameAction.RoomLeft)
        callbacks.status("You left the match.")
        setReconnecting(false)
    }

    private fun leaveGame() {
        store.activeMatch = null
        send(ClientCommand.LeaveGame(email))
    }

    // The match is over: back to the start screen on a fresh connection. The
    // web reloads the page here.
    fun goHome() {
        retryJob?.cancel()
        teardown()
        callbacks.dispatch(GameAction.RoomLeft)
        setReconnecting(false)
    }

    // Test bridge: sever the websocket the way a backgrounded phone's OS
    // does — the disconnect still arrives (unlike destroy()), so the session
    // walks the real disconnected → reconnect-screen → retry path.
    fun dropConnection() {
        socket?.drop()
    }

    // An app returning to the foreground pings at once: if the OS killed the
    // connection while we slept, the send surfaces the close now instead of
    // leaving a half-open socket lingering until the heartbeat.
    fun nudge() {
        if (socket?.isOpen == true) send(ClientCommand.Ping)
    }

    // A quick table emote. The server validates it, and we only need to be in a running match for it to
    // mean anything. It shows here at once, so the tap answers without waiting on the network. Returns
    // whether it went.
    fun sendEmote(emote: String): Boolean {
        val inMatch = when (callbacks.getModel().phase) {
            GamePhase.BIDDING, GamePhase.CHOOSING_TRUMP, GamePhase.PLAYING, GamePhase.HAND_FINISHED -> true
            else -> false
        }
        if (socket == null || !inMatch || emoteCooldown?.isActive == true) return false
        send(ClientCommand.SendEmote(email, emote))
        callbacks.emote(0, emote)
        emoteCooldown = scope.launch { delay(EMOTE_GAP_MS) }
        return true
    }

    private fun showEmote(event: ServerEvent.EmotePlayed) {
        // until we are seated (rejoin still restoring), myServerIndex is
        // a default and the bubble would land at the wrong seat
        if (!inRoom) return
        val seat = toView(event.seat)
        // Our own showed when we sent it.
        if (seat != 0) callbacks.emote(seat, event.emote)
    }

    private fun send(command: ClientCommand) {
        socket?.send(encodeCommand(command))
    }

    private fun resetHandState() {
        cardsInTrick = 0
        trickPoints = 0
        pointsTaken = mutableListOf(0, 0, 0, 0)
        locallyPlayed.clear()
        chosenTrump = null
        justExposed = false
    }

    // Back to the start screen: reset lobby state and refresh the table list
    // so the user can immediately join or host again on the same connection.
    private fun exitRoom(message: String) {
        store.activeMatch = null
        inRoom = false
        joinedRoomName = null
        myServerIndex = 0
        resetHandState()
        callbacks.dispatch(GameAction.RoomLeft)
        callbacks.status(message)
        refreshRooms()
    }

    // The session died: phase idle wipes the model, and the connection is
    // dropped. If our match is still live somewhere, hold the player on the
    // reconnecting state and keep retrying (GameState's idle handling).
    private fun goIdle() {
        callbacks.dispatch(GameAction.PhaseChanged(GamePhase.IDLE))
        teardown()
        if (requiredVersion == null && storedActiveMatch() != null) {
            setReconnecting(true)
            scheduleRejoinRetry()
        } else {
            setReconnecting(false)
        }
    }

    // The connection died while a match of ours is still stored (a deploy,
    // a server blip, flaky wifi): keep trying to rejoin every few seconds.
    // The loop ends by itself — a successful snapshot restores the table,
    // and a dead match answers no-running-game, which clears the stored key.
    private fun scheduleRejoinRetry() {
        val interrupted = storedActiveMatch() ?: return
        callbacks.status("Connection lost — trying to rejoin…")
        retryJob?.cancel()
        retryJob = scope.launch {
            delay(REJOIN_RETRY_MS)
            if (!live && storedActiveMatch() != null) {
                rejoinMatch(interrupted.name, interrupted.email)
            }
        }
    }

    // --- seat translation: server index <-> view index (0 = us at the bottom) ---

    private fun toView(serverIndex: Int): PlayerIndex = (serverIndex - myServerIndex + 4) % 4

    private fun toServer(viewIndex: Int): Int = (viewIndex + myServerIndex) % 4

    private fun seatPresentation(members: List<String>): GameAction.SeatNamesSet {
        val seatMembers = (0..3).map { view -> members.getOrNull(toServer(view)) ?: "" }
        return GameAction.SeatNamesSet(seatMembers, seatMembers.map { memberDisplayName(it, members) })
    }

    // --- session program ---

    private suspend fun program(intent: SessionIntent, joinRoom: String?) = coroutineScope {
        callbacks.dispatch(GameAction.PhaseChanged(GamePhase.CONNECTING))
        callbacks.status("Connecting…")

        val connected = try {
            socketFactory.connect(serverUrl)
        } catch (error: CancellationException) {
            throw error
        } catch (error: Exception) {
            callbacks.status(
                (error as? SocketError)?.message
                    ?: "could not connect to $serverUrl — is the game server running?"
            )
            goIdle()
            return@coroutineScope
        }
        socket = connected

        // Never wait for the answer: a server that predates client-hello
        // replies invalid-request, which decodes to Ignored.
        send(ClientCommand.ClientHello(CLIENT_PLATFORM, PROTOCOL_VERSION))
        send(ClientCommand.ConnectUser(email, "none"))
        delay(CONNECT_BEAT_MS)

        // Frames that came in during the beat. Read here rather than after the
        // intent, so a refused client never creates a table it would abandon.
        val early = ArrayDeque<ServerEvent>()
        while (true) {
            val frame = connected.incoming.tryReceive()
            if (!frame.isSuccess) break
            decodeServerEvent(frame.getOrThrow())?.let(early::addLast)
        }

        // heartbeat — dies with this coroutine when the session ends
        val heartbeat = launch {
            while (true) {
                delay(PING_INTERVAL_MS)
                send(ClientCommand.Ping)
            }
        }

        if (early.none { it is ServerEvent.UpdateRequired }) {
            issueIntent(intent, joinRoom)
        }

        // Frames are read as they come, apart from the loop below, which holds each event for the table's
        // pacing. An emote is a live reaction rather than part of the game, so it never waits in that line.
        val events = Channel<ServerEvent>(Channel.UNLIMITED)
        val reader = launch {
            while (true) {
                val frame = connected.incoming.receiveCatching()
                if (frame.isClosed) {
                    events.trySend(ServerEvent.Disconnected)
                    break
                }
                when (val event = decodeServerEvent(frame.getOrThrow())) {
                    null -> Unit
                    is ServerEvent.EmotePlayed -> showEmote(event)
                    else -> events.trySend(event)
                }
            }
        }

        try {
            while (true) {
                val event = early.removeFirstOrNull() ?: events.receive()
                if (handleEvent(event)) break
            }
        } finally {
            heartbeat.cancel()
            reader.cancel()
        }
    }

    private fun issueIntent(intent: SessionIntent, joinRoom: String?) {
        if (intent == SessionIntent.CREATE_ROOM) {
            createRoom()
        } else if (intent == SessionIntent.REJOIN) {
            callbacks.status("Rejoining your match…")
            send(ClientCommand.Rejoin(email))
        } else if (intent == SessionIntent.JOIN && joinRoom != null) {
            // an invite link: go straight to that table
            join(joinRoom)
        } else if (intent == SessionIntent.STANDINGS) {
            callbacks.status("")
            requestLeaderboard()
        } else {
            refreshRooms()
        }
    }

    // winner: the seat that takes the trick (the next leader), when known.
    private suspend fun finishTrickIfComplete(winner: PlayerIndex?) {
        if (cardsInTrick < 4) return
        val pacing = callbacks.getPacing()
        // Let people read the completed four-card tableau before announcing a
        // winner. The following wait keeps the queued next lead from drawing
        // over the winner highlight and collection sweep.
        delay(pacing.trickReadMs)
        callbacks.dispatch(GameAction.TrickEnded(winner, trickPoints))
        if (winner != null) pointsTaken[winner] += trickPoints
        cardsInTrick = 0
        trickPoints = 0
        delay(pacing.trickResolveMs)
    }

    // Server events can arrive back-to-back, especially when bots answer
    // immediately. Present one event and hold the queue long enough for the
    // player to perceive it before the next event is rendered.
    private suspend fun presentThenPause(action: GameAction, durationMs: Long) {
        callbacks.dispatch(action)
        delay(durationMs)
    }

    // Returns true when the event loop is finished.
    private suspend fun handleEvent(event: ServerEvent): Boolean {
        val dispatch = callbacks::dispatch
        when (event) {
            ServerEvent.RoomCreated -> {
                inRoom = true
                myServerIndex = 0
                dispatch(GameAction.RoomEntered(roomName, listOf(email), isHost = true, target = tableTarget))
                callbacks.status("Your table is ready — fill the seats, then start.")
            }

            is ServerEvent.ActiveRooms -> callbacks.rooms(event.rooms)

            is ServerEvent.RoomMembers -> {
                myServerIndex = event.members.indexOf(email)
                if (inRoom) {
                    dispatch(GameAction.MembersChanged(event.members, event.target, event.seats))
                } else if (myServerIndex >= 0) {
                    // our join went through — this broadcast is our welcome
                    inRoom = true
                    dispatch(
                        GameAction.RoomEntered(joinedRoomName ?: "table", event.members, isHost = false, target = event.target)
                    )
                    if (event.seats != null) dispatch(GameAction.MembersChanged(event.members, seats = event.seats))
                    callbacks.status("") // we're in, and only the host hears "game started" to replace it
                }
            }

            ServerEvent.GameStarted -> callbacks.status("Dealing…")

            ServerEvent.RoomLeft -> exitRoom("You left the table.")

            ServerEvent.RoomClosed -> if (inRoom) exitRoom("The host closed the table.")

            ServerEvent.RemovedFromRoom -> if (inRoom) exitRoom("The host removed you from the table.")

            ServerEvent.GameAborted -> if (inRoom) exitRoom("A player disconnected — the game was abandoned.")

            is ServerEvent.StartGameFailed -> callbacks.status(
                if (event.reason == "room-not-ready") {
                    "The table needs 4 players before the game can start."
                } else {
                    "This game has already started."
                }
            )

            is ServerEvent.HandDealt -> {
                // mark the match rejoinable for a future launch
                store.activeMatch = email
                // A deal landing between hands starts the next hand of the match:
                // clear the per-hand session state before the model reset. Played
                // card ids recur across hands, so a stale locallyPlayed entry
                // would swallow a future broadcast.
                if (callbacks.getModel().phase == GamePhase.HAND_FINISHED) {
                    resetHandState()
                    dispatch(GameAction.HandReset)
                }
                // First deal: seats are final now, pin everyone's display name.
                val model = callbacks.getModel()
                if (model.seatNames == null && model.members.size == 4) {
                    dispatch(seatPresentation(model.members))
                }
                dispatch(GameAction.HandDealt(event.cards))
                delay(callbacks.getPacing().dealCompleteMs)
            }

            is ServerEvent.Turn -> {
                // After a complete trick, the next leader is the trick's winner.
                finishTrickIfComplete(toView(event.playerIndex))
                dispatch(GameAction.ActivePlayerChanged(toView(event.playerIndex)))
            }

            is ServerEvent.BidRequested -> {
                dispatch(GameAction.PhaseChanged(GamePhase.BIDDING))
                dispatch(GameAction.RequestReceived(PendingRequest.Bid(event.minBid)))
            }

            is ServerEvent.BidPlaced -> presentThenPause(
                GameAction.BidPlaced(toView(event.playerIndex), event.bid),
                callbacks.getPacing().bidPauseMs,
            )

            is ServerEvent.BidResult -> dispatch(GameAction.BidWon(toView(event.playerIndex), event.bid))

            ServerEvent.ChooseTrumpRequested -> {
                // No turn comes with this, and the last bidder would stay lit as "bidding…".
                dispatch(GameAction.ActivePlayerChanged(0))
                dispatch(GameAction.RequestReceived(PendingRequest.Trump))
            }

            ServerEvent.TrumpSelected -> dispatch(GameAction.TrumpSet(chosenTrump, exposed = false))

            is ServerEvent.TrumpExposed -> dispatch(GameAction.TrumpRevealed(event.suit, toView(event.playerIndex)))

            is ServerEvent.CardPlayed -> {
                cardsInTrick++
                trickPoints += cardPoints(event.card)
                if (!locallyPlayed.remove(event.card.id)) { // otherwise already rendered
                    presentThenPause(
                        GameAction.CardPlayed(event.card, toView(event.playerIndex)),
                        callbacks.getPacing().cardPauseMs,
                    )
                }
            }

            is ServerEvent.PlayRequested -> {
                dispatch(GameAction.PhaseChanged(GamePhase.PLAYING))
                if (event.trumpSuit != null) {
                    dispatch(GameAction.TrumpSet(event.trumpSuit, exposed = true))
                }
                val mustTrump = justExposed
                justExposed = false
                dispatch(
                    GameAction.RequestReceived(PendingRequest.Play(event.firstSuit, event.trumpSuit, mustTrump))
                )
            }

            is ServerEvent.PointsWon -> {
                val points = (0..3).map { view -> event.points[toServer(view)] ?: 0 }
                // No turn broadcast follows the last trick — derive its winner from
                // the gap between the final totals and what we attributed so far.
                val lastWinner =
                    if (trickPoints > 0) (0..3).firstOrNull { view -> points[view] - pointsTaken[view] == trickPoints }
                    else null
                finishTrickIfComplete(lastWinner)
                // only the hand is over — hand-result (and possibly match-over)
                // follow on the same socket, so the loop keeps running
                dispatch(GameAction.HandFinished(points))
            }

            is ServerEvent.HandResult -> {
                // evens is the team of even *server* seats; ours iff we sit even
                val usEvens = myServerIndex % 2 == 0
                dispatch(
                    GameAction.HandResult(
                        bidder = toView(event.bidder),
                        bid = event.bid,
                        made = event.made,
                        us = if (usEvens) event.evens else event.odds,
                        them = if (usEvens) event.odds else event.evens,
                        target = event.target,
                    )
                )
            }

            is ServerEvent.MatchOver -> {
                store.activeMatch = null
                val usEvens = myServerIndex % 2 == 0
                dispatch(
                    GameAction.MatchOver(
                        winner = if ((event.winner == Team.EVENS) == usEvens) MatchSide.US else MatchSide.THEM,
                        us = if (usEvens) event.evens else event.odds,
                        them = if (usEvens) event.odds else event.evens,
                        hands = event.hands,
                    )
                )
                return true
            }

            is ServerEvent.PlayerDisconnected ->
                // our own drop notice can only reach us after we reconnected
                if (event.email != email) {
                    val name = memberDisplayName(event.email, callbacks.getModel().members)
                    callbacks.status("$name lost connection — holding their seat up to ${event.graceSeconds}s…")
                }

            is ServerEvent.PlayerReconnected ->
                if (event.email != email) {
                    callbacks.status("${memberDisplayName(event.email, callbacks.getModel().members)} is back.")
                }

            // Only one that came in during the connect beat gets here: see the reader in program.
            is ServerEvent.EmotePlayed -> showEmote(event)

            is ServerEvent.SeatAbandoned -> {
                val name = memberDisplayName(event.email)
                callbacks.status("$name abandoned the match.")
                callbacks.seatAbandoned(name)
            }

            is ServerEvent.SeatReplaced -> {
                // the seat's new identity reaches everyone: refresh members and
                // the pinned seat names, then carry on playing
                val model = callbacks.getModel()
                dispatch(GameAction.MembersChanged(event.members))
                if (model.seatNames != null) dispatch(seatPresentation(event.members))
                callbacks.status("${memberDisplayName(event.email, event.members)} takes over the seat.")
                callbacks.seatResolved()
            }

            is ServerEvent.Leaderboard -> callbacks.leaderboard(event.rows)

            ServerEvent.NoRunningGame -> {
                // the match we tried to rejoin is gone — back to a fresh start
                store.activeMatch = null
                callbacks.status("That match has ended.")
                goIdle()
                return true
            }

            is ServerEvent.GameSnapshot -> {
                restoreSnapshot(event.snapshot)
                setReconnecting(false)
                callbacks.status("Reconnected — back at the table.")
                // between hands, the verdict panel comes back up
                val lastResult = event.snapshot.lastResult
                if (lastResult != null && event.snapshot.stage == SnapshotStage.BETWEEN_HANDS) {
                    handleEvent(lastResult)
                }
                // and if the server was waiting on us, the original request re-arms
                // the bid/trump/play UI exactly as if it had just arrived
                event.snapshot.request?.let { handleEvent(it) }
            }

            is ServerEvent.ServerError -> return handleServerError(event.message)

            ServerEvent.Disconnected -> {
                callbacks.status("Disconnected from the game server.")
                goIdle()
                return true
            }

            is ServerEvent.UpdateRequired -> {
                requiredVersion = event.minVersion
                retryJob?.cancel()
                callbacks.updateRequired(event.minVersion)
                goIdle()
                return true
            }

            is ServerEvent.ProtocolOk, ServerEvent.RoomJoined, is ServerEvent.Ignored -> {}
        }
        return false
    }

    private fun restoreSnapshot(s: MatchSnapshot) {
        inRoom = true
        myServerIndex = s.yourSeat
        cardsInTrick = s.trick.size
        trickPoints = s.trick.sumOf { cardPoints(it.card) }
        pointsTaken = (0..3).map { view -> s.points[toServer(view)] ?: 0 }.toMutableList()
        locallyPlayed.clear()
        chosenTrump = null
        justExposed = false
        store.activeMatch = email

        val phase = when (s.stage) {
            SnapshotStage.BETWEEN_HANDS -> GamePhase.HAND_FINISHED
            SnapshotStage.CHOOSING_TRUMP -> GamePhase.CHOOSING_TRUMP
            SnapshotStage.PLAYING -> GamePhase.PLAYING
            SnapshotStage.BIDDING -> GamePhase.BIDDING
        }
        val seats = seatPresentation(s.members)
        val tricksByView = (0..3).map { view -> s.tricks[toServer(view)] ?: 0 }
        val usEvens = s.yourSeat % 2 == 0
        callbacks.dispatch(
            GameAction.SnapshotRestored(
                initialGameModel.copy(
                    phase = phase,
                    roomName = s.room,
                    members = s.members,
                    // the host created the room first, so they sit at the end of
                    // the server's newest-first member list
                    isHost = s.members.lastOrNull() == email,
                    seatMembers = seats.members,
                    seatNames = seats.names,
                    hand = s.yourHand,
                    playedCards = s.trick.map { PlayedCardModel(it.card, toView(it.seat)) },
                    activePlayer = s.awaiting?.let { toView(it) },
                    score = pointsTaken[0] + pointsTaken[2],
                    tricks = tricksByView[0] + tricksByView[2],
                    theirScore = pointsTaken[1] + pointsTaken[3],
                    theirTricks = tricksByView[1] + tricksByView[3],
                    currentBid = s.bidResult?.value ?: s.highBid?.value ?: 16,
                    bidsPlaced = if (s.bidResult != null || s.highBid != null) 1 else 0,
                    bidWinner = s.bidResult?.let { toView(it.seat) },
                    finalBid = s.bidResult?.value,
                    trumpSuit = s.trumpSuit,
                    trumpExposed = s.trumpExposed,
                    matchUs = if (usEvens) s.evens else s.odds,
                    matchThem = if (usEvens) s.odds else s.evens,
                    matchTarget = s.target,
                    handNumber = s.handNumber,
                )
            )
        )
    }

    private fun handleServerError(message: String): Boolean {
        val friendly = linkedMapOf(
            "already-in-match" to "You're still in a match — taking you back to it…",
            "no-such-room" to "That table is no longer open.",
            "room-full" to "That table is already full.",
            "must-open-bid" to "You open the bidding — pick a bid of 16 or more.",
            "invalid-bid" to "That bid is no longer high enough.",
            "invalid-card" to "You must follow suit while you can.",
            "must-play-trump" to "You called for the trump, so you must play one.",
            "invalid-expose" to "You can only call for the trump when you cannot follow suit.",
        )
        val match = friendly.keys.firstOrNull { message.contains(it) }
        callbacks.status(if (match != null) friendly.getValue(match) else "Server rejected that: $message")
        // the server refused to seat us elsewhere while our match still
        // runs — go claim that seat instead of arguing
        if (match == "already-in-match") {
            send(ClientCommand.Rejoin(email))
            return false
        }
        // a failed entry (vanished or full table — e.g. a stale invite
        // link) must hand back a working start screen, not a bare felt
        if (!inRoom && (match == "no-such-room" || match == "room-full")) {
            goIdle()
            return true
        }
        return false
    }
}
