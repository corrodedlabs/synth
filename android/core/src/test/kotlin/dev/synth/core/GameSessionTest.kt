package dev.synth.core

import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.ExperimentalCoroutinesApi
import kotlinx.coroutines.channels.Channel
import kotlinx.coroutines.channels.ReceiveChannel
import kotlinx.coroutines.test.TestScope
import kotlinx.coroutines.test.advanceTimeBy
import kotlinx.coroutines.test.runCurrent
import kotlinx.coroutines.test.runTest
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFalse
import kotlin.test.assertNull
import kotlin.test.assertTrue

// No TS counterpart: on the web this path is covered by the Playwright suites.
// Everything runs on virtual time against a fake socket.

private class FakeSocket : GameSocket {
    private val frames = Channel<String>(Channel.UNLIMITED)
    val sent = mutableListOf<String>()
    var closed = false

    override val incoming: ReceiveChannel<String> get() = frames
    override val isOpen: Boolean get() = !closed

    override fun send(text: String) {
        if (!closed) sent.add(text)
    }

    override fun close() {
        closed = true
    }

    override fun drop() {
        closed = true
        frames.close()
    }

    fun receive(vararg wire: String) = wire.forEach { frames.trySend(it) }
}

private class FakeStore : SessionStore {
    override var playerId: String? = null
    override var playerName: String? = null
    override var activeMatch: String? = null
}

private class Harness(scope: CoroutineScope) : SessionCallbacks {
    val sockets = mutableListOf<FakeSocket>()
    val connectUrls = mutableListOf<String>()
    var failConnects = 0
    val store = FakeStore()

    var state = initialGameModel
    val actions = mutableListOf<GameAction>()
    val statuses = mutableListOf<String>()
    val roomLists = mutableListOf<List<RoomInfo>>()
    val emotes = mutableListOf<Pair<Int, String>>()
    val abandoned = mutableListOf<String>()
    var resolved = 0
    val boards = mutableListOf<List<LeaderboardRow>>()
    val reconnectStates = mutableListOf<Boolean>()
    val updates = mutableListOf<Int>()
    val profile = gamePacing()

    val session = GameSession(
        callbacks = this,
        socketFactory = { url ->
            connectUrls.add(url)
            if (failConnects > 0) {
                failConnects--
                throw SocketError("could not connect to $url")
            }
            FakeSocket().also { sockets.add(it) }
        },
        store = store,
        serverUrl = "ws://test",
        scope = scope,
        newPlayerId = { "abc123" },
    )

    val socket get() = sockets.last()

    override fun dispatch(action: GameAction) {
        actions.add(action)
        state = gameReducer(state, action)
    }

    override fun getModel() = state
    override fun getPacing() = profile
    override fun status(text: String) { statuses.add(text) }
    override fun rooms(rooms: List<RoomInfo>) { roomLists.add(rooms) }
    override fun emote(seat: PlayerIndex, emote: String) { emotes.add(seat to emote) }
    override fun seatAbandoned(name: String) { abandoned.add(name) }
    override fun seatResolved() { resolved++ }
    override fun leaderboard(rows: List<LeaderboardRow>) { boards.add(rows) }
    override fun reconnecting(active: Boolean) { reconnectStates.add(active) }
    override fun updateRequired(minVersion: Int) { updates.add(minVersion) }
}

private const val ME = "asha-abc123@game.local"
private const val HELLO = "(client-hello \"android\" 1)"
private const val CONNECT = "(connect-user \"$ME\" \"none\")"
private const val MIN_DEAL = "(hand \"$ME\" (#s(card jack 7 club 3) #s(card seven 0 heart 0)))"

@OptIn(ExperimentalCoroutinesApi::class)
class GameSessionTest {
    private fun TestScope.harness() = Harness(backgroundScope)

    private fun TestScope.settle(ms: Long = 0) {
        if (ms > 0) advanceTimeBy(ms)
        runCurrent()
    }

    // A human at server seat 2 of a running match, hand dealt and paced through.
    private fun TestScope.seatedAtTwo(h: Harness) {
        h.session.joinByLink("Asha", "their table")
        settle(CONNECT_BEAT_MS)
        h.socket.receive("(room-members \"their table\" (\"g@x\" bot-1 \"$ME\" \"host@x\"))", "(game-started)", MIN_DEAL)
        settle()
        settle(h.profile.dealCompleteMs)
    }

    // --- client-hello ---

    @Test
    fun `says hello first and does not wait for an answer`() = runTest {
        val h = harness()
        h.session.browseTables("Asha")
        settle()
        assertEquals(listOf(HELLO, CONNECT), h.socket.sent)
        // the beat passes with no reply at all, as with an old server
        settle(CONNECT_BEAT_MS)
        assertEquals(listOf(HELLO, CONNECT, "(get-active-rooms)"), h.socket.sent)
    }

    @Test
    fun `an old server's invalid-request reply changes nothing`() = runTest {
        val h = harness()
        h.session.createTable("Asha")
        settle(CONNECT_BEAT_MS)
        h.socket.receive("invalid-request", "#<void>", "room-created")
        settle()
        assertEquals(GamePhase.LOBBY, h.state.phase)
        assertEquals(emptyList(), h.updates)
    }

    @Test
    fun `protocol-ok is a quiet no-op`() = runTest {
        val h = harness()
        h.session.createTable("Asha")
        settle()
        val before = h.actions.toList()
        h.socket.receive("(protocol-ok 1)")
        settle(CONNECT_BEAT_MS)
        assertEquals(before, h.actions)
        assertEquals(listOf(HELLO, CONNECT, "(make-room \"$ME\" \"Asha's table #abc\" 6)"), h.socket.sent)
    }

    // --- identity ---

    @Test
    fun `builds and stores the same identity as the web`() = runTest {
        val h = harness()
        h.session.createTable("  Asha B!  ", target = 2)
        settle()
        assertEquals("asha-b-abc123@game.local", h.session.email)
        assertEquals("Asha B!", h.session.displayName)
        assertEquals("Asha B!'s table #abc", h.session.roomName)
        assertEquals(2, h.session.tableTarget)
        assertEquals("abc123", h.store.playerId)
        assertEquals("Asha B!", h.session.storedPlayerName())
    }

    @Test
    fun `an existing player id is kept and an empty name falls back`() = runTest {
        val h = harness()
        h.store.playerId = "zzz999"
        h.session.browseTables("   ")
        settle()
        assertEquals("player-zzz999@game.local", h.session.email)
        assertEquals("Player", h.session.displayName)
    }

    // --- lobby to deal ---

    @Test
    fun `lobby to deal`() = runTest {
        val h = harness()
        h.session.createTable("Asha", target = 2)
        settle(CONNECT_BEAT_MS)
        assertEquals("(make-room \"$ME\" \"Asha's table #abc\" 2)", h.socket.sent.last())

        h.socket.receive("room-created")
        settle()
        assertEquals(GamePhase.LOBBY, h.state.phase)
        assertTrue(h.state.isHost)
        assertEquals(2, h.state.matchTarget)
        assertEquals("Your table is ready — fill the seats, then start.", h.statuses.last())

        h.session.addBot()
        assertEquals("(add-bot-to-room \"Asha's table #abc\")", h.socket.sent.last())

        // not enough players yet: the start guard holds
        val sentBefore = h.socket.sent.size
        h.session.startGame()
        assertEquals(sentBefore, h.socket.sent.size)

        h.socket.receive("(room-members \"Asha's table #abc\" (bot-3 bot-2 bot-1 \"$ME\") 2)")
        settle()
        assertEquals(listOf("bot-3", "bot-2", "bot-1", ME), h.state.members)

        // a full table: no more bots, and it can start
        h.session.addBot()
        assertEquals(sentBefore, h.socket.sent.size)
        h.session.startGame()
        assertEquals("(start-game \"Asha's table #abc\")", h.socket.sent.last())

        h.socket.receive("(game-started)", MIN_DEAL, "(request-bid 16)")
        settle()
        assertEquals(GamePhase.BIDDING, h.state.phase)
        assertEquals(listOf("clubs-J", "hearts-7"), h.state.hand.map { it.id })
        assertEquals(ME, h.store.activeMatch)
        // seat 3 of the server is us, so view 1 is server seat 0
        assertEquals(listOf(ME, "bot-3", "bot-2", "bot-1"), h.state.seatMembers)
        assertEquals(listOf("asha", "Tara", "Arjun", "Mira"), h.state.seatNames)
        // the bid request waits for the deal to settle
        assertNull(h.state.pendingRequest)
        settle(h.profile.dealCompleteMs)
        runCurrent()
        assertEquals(PendingRequest.Bid(16), h.state.pendingRequest)

        h.session.bid(17)
        assertEquals("(put-bid \"$ME\" 17)", h.socket.sent.last())
        assertNull(h.state.pendingRequest)
    }

    @Test
    fun `browse then join on the same connection`() = runTest {
        val h = harness()
        h.session.browseTables("Asha")
        settle(CONNECT_BEAT_MS)
        h.socket.receive("(active-rooms (((host . \"h@x\") (name . \"t\") (members \"h@x\"))))")
        settle()
        assertEquals(listOf(RoomInfo("h@x", "t", listOf("h@x"), 6)), h.roomLists.last())

        h.session.join("t")
        assertEquals("(join-room \"t\" \"$ME\")", h.socket.sent.last())
        h.socket.receive("(room-members \"t\" (\"$ME\" \"h@x\") 2)")
        settle()
        assertEquals(1, h.sockets.size)
        assertEquals("t", h.state.roomName)
        assertFalse(h.state.isHost)
        assertEquals(2, h.state.matchTarget)

        h.session.leaveRoom()
        assertEquals("(leave-room \"t\" \"$ME\")", h.socket.sent.last())
        h.socket.receive("room-left")
        settle()
        assertEquals(GamePhase.IDLE, h.state.phase)
        assertEquals("You left the table.", h.statuses.last())
        assertEquals("(get-active-rooms)", h.socket.sent.last())
    }

    @Test
    fun `standings ask over a live session or connect for it`() = runTest {
        val h = harness()
        h.session.showStandings("Asha")
        settle(CONNECT_BEAT_MS)
        assertEquals("(get-leaderboard)", h.socket.sent.last())
        h.socket.receive("(leaderboard ((\"asha\" 3 2)))")
        settle()
        assertEquals(listOf(LeaderboardRow("asha", 3, 2)), h.boards.last())
        h.session.showStandings("Asha")
        assertEquals(1, h.sockets.size)
        assertEquals(2, h.socket.sent.count { it == "(get-leaderboard)" })
    }

    // --- seat translation ---

    @Test
    fun `translates server seats to view seats`() = runTest {
        val h = harness()
        seatedAtTwo(h)
        assertEquals(listOf(ME, "host@x", "g@x", "bot-1"), h.state.seatMembers)

        h.socket.receive("(turn 2)")
        settle()
        assertEquals(0, h.state.activePlayer)
        h.socket.receive("(turn 3)")
        settle()
        assertEquals(1, h.state.activePlayer)
        h.socket.receive("(turn 0)")
        settle()
        assertEquals(2, h.state.activePlayer)
        h.socket.receive("(turn 1)")
        settle()
        assertEquals(3, h.state.activePlayer)

        h.socket.receive("(bid-result (19 . 1))")
        settle()
        assertEquals(3, h.state.bidWinner)

        h.socket.receive("(emote-played 3 laugh)")
        settle()
        assertEquals(1 to "laugh", h.emotes.last())

        h.socket.receive("(played 0 #s(card ace 5 spade 1))")
        settle()
        assertEquals(PlayedCardModel(CardModel("spades-A", Suit.SPADES, Rank.A), 2), h.state.playedCards.last())
    }

    @Test
    fun `reports match scores from our team's side`() = runTest {
        val odd = harness()
        odd.session.joinByLink("Asha", "t")
        settle(CONNECT_BEAT_MS)
        odd.socket.receive(
            "(room-members \"t\" (\"g@x\" \"$ME\" bot-1 \"host@x\"))",
            "(hand-result ((hand . 1) (bidder . 2) (bid . 20) (made . #t) (delta . 2) (evens . 2) (odds . -1) (target . 6)))",
            "(match-over ((winner . odds) (evens . 2) (odds . 6) (hands . 7)))",
        )
        settle()
        assertEquals(MatchSide.US, odd.state.matchWinner)
        assertEquals(6, odd.state.matchUs)
        assertEquals(2, odd.state.matchThem)
        assertEquals(GameAction.HandResult(1, 20, true, -1, 2, 6), odd.actions.filterIsInstance<GameAction.HandResult>().last())
    }

    // --- pacing ---

    @Test
    fun `holds the queue through each pacing pause`() = runTest {
        val h = harness()
        seatedAtTwo(h)
        val pacing = h.profile

        h.socket.receive("(bid-placed 3 17)", "(bid-placed 0 pass)")
        settle()
        assertEquals(1, h.state.bidsPlaced)
        settle(pacing.bidPauseMs - 1)
        assertEquals(1, h.state.bidsPlaced)
        settle(1)
        assertEquals(2, h.state.bidsPlaced)
        settle(pacing.bidPauseMs)

        h.socket.receive("(played 3 #s(card ace 5 spade 1))", "(played 0 #s(card king 3 spade 0))")
        settle()
        assertEquals(1, h.state.playedCards.size)
        settle(pacing.cardPauseMs - 1)
        assertEquals(1, h.state.playedCards.size)
        settle(1)
        assertEquals(2, h.state.playedCards.size)
    }

    @Test
    fun `resolves a full trick on the next turn, read time then winner then sweep`() = runTest {
        val h = harness()
        seatedAtTwo(h)
        val pacing = h.profile
        h.socket.receive(
            "(played 0 #s(card ace 5 spade 1))",
            "(played 1 #s(card king 3 spade 0))",
            "(played 3 #s(card nine 6 spade 2))",
            "(played 2 #s(card seven 0 spade 0))",
            "(turn 3)",
            "(turn 2)",
        )
        settle()
        settle(pacing.cardPauseMs * 4)
        assertEquals(4, h.state.playedCards.size)

        settle(pacing.trickReadMs)
        // the winner is the next leader: server 3 is view 1, their team
        assertEquals(0, h.state.playedCards.size)
        assertEquals(1, h.state.theirTricks)
        assertEquals(3, h.state.theirScore)
        assertNull(h.state.activePlayer) // still sweeping

        settle(pacing.trickResolveMs)
        assertEquals(0, h.state.activePlayer)
    }

    @Test
    fun `our own card renders at once and its broadcast is not rendered twice`() = runTest {
        val h = harness()
        seatedAtTwo(h)
        h.socket.receive("(play-card (cards-played) (game-state (trump-suit . #f) (first-suit . #f)))")
        settle()
        assertEquals(listOf("clubs-J", "hearts-7"), h.session.legalCards())
        assertTrue(h.session.playCardById("clubs-J"))
        assertEquals("(card-played \"$ME\" #s(card jack 7 club 3))", h.socket.sent.last())
        assertEquals(1, h.state.playedCards.size)
        assertEquals(listOf("hearts-7"), h.state.hand.map { it.id })
        assertNull(h.state.pendingRequest)

        h.socket.receive("(played 2 #s(card jack 7 club 3))")
        settle()
        assertEquals(1, h.state.playedCards.size)
    }

    @Test
    fun `refuses an illegal play and makes the caller of the exposure trump`() = runTest {
        val h = harness()
        seatedAtTwo(h)
        h.socket.receive("(play-card (cards-played #s(card ace 5 club 1)) (game-state (trump-suit . #f) (first-suit . club)))")
        settle()
        val sent = h.socket.sent.size
        assertFalse(h.session.playCardById("hearts-7"))
        assertEquals(sent, h.socket.sent.size)

        // void in the suit led, so the call is on offer
        h.socket.receive("(play-card (cards-played #s(card ace 5 spade 1)) (game-state (trump-suit . #f) (first-suit . spade)))")
        settle()
        h.session.exposeTrump()
        assertEquals("(card-played \"$ME\" expose-trump)", h.socket.sent.last())
        h.socket.receive("(play-card (cards-played #s(card ace 5 spade 1)) (game-state (trump-suit . heart) (first-suit . spade)))")
        settle()
        assertEquals(PendingRequest.Play(Suit.SPADES, Suit.HEARTS, mustTrump = true), h.state.pendingRequest)
        assertEquals(listOf("hearts-7"), h.session.legalCards())
    }

    @Test
    fun `derives the last trick's winner from the final points`() = runTest {
        val h = harness()
        seatedAtTwo(h)
        h.socket.receive(
            "(played 0 #s(card ace 5 spade 1))",
            "(played 1 #s(card king 3 spade 0))",
            "(played 3 #s(card nine 6 spade 2))",
            "(played 2 #s(card seven 0 spade 0))",
            "(points-won #hash((0 . 0) (1 . 0) (2 . 3) (3 . 0)))",
        )
        settle()
        settle(h.profile.cardPauseMs * 4 + h.profile.trickReadMs + h.profile.trickResolveMs)
        assertTrue(GameAction.TrickEnded(0, 3) in h.actions)
        assertEquals(GamePhase.HAND_FINISHED, h.state.phase)
        assertEquals(listOf(3, 0, 0, 0), h.state.points)
    }

    @Test
    fun `a deal between hands resets the hand and keeps the match`() = runTest {
        val h = harness()
        seatedAtTwo(h)
        h.socket.receive(
            "(points-won #hash((0 . 5) (1 . 5) (2 . 9) (3 . 9)))",
            "(hand-result ((hand . 1) (bidder . 2) (bid . 16) (made . #t) (delta . 1) (evens . 1) (odds . 0) (target . 6)))",
        )
        settle()
        assertEquals(1, h.state.matchUs)

        // only the acting host may deal; here that is host@x
        val sent = h.socket.sent.size
        h.session.nextHand()
        assertEquals(sent, h.socket.sent.size)

        h.socket.receive(MIN_DEAL)
        settle()
        assertEquals(GamePhase.BIDDING, h.state.phase)
        assertEquals(2, h.state.handNumber)
        assertEquals(2, h.state.hand.size)
        assertEquals(1, h.state.matchUs)
    }

    // --- heartbeat ---

    @Test
    fun `pings every 25 seconds until the match ends`() = runTest {
        val h = harness()
        seatedAtTwo(h)
        val pings = { h.socket.sent.count { it == "(ping)" } }
        assertEquals(0, pings())
        settle(PING_INTERVAL_MS - h.profile.dealCompleteMs - 1)
        assertEquals(0, pings())
        settle(1)
        assertEquals(1, pings())
        settle(PING_INTERVAL_MS)
        assertEquals(2, pings())

        h.socket.receive("(match-over ((winner . evens) (evens . 6) (odds . 0) (hands . 7)))")
        settle()
        settle(PING_INTERVAL_MS * 3)
        assertEquals(2, pings())
        assertEquals(GamePhase.FINISHED, h.state.phase)
        assertNull(h.store.activeMatch)
    }

    @Test
    fun `nudge pings an open socket right away`() = runTest {
        val h = harness()
        h.session.browseTables("Asha")
        settle(CONNECT_BEAT_MS)
        h.session.nudge()
        assertEquals("(ping)", h.socket.sent.last())
    }

    // --- socket drop and rejoin ---

    private val midHandSnapshot =
        "(game-snapshot ((room . \"their table\") (members \"g@x\" bot-1 \"$ME\" \"host@x\") (your-seat . 2)" +
            " (hand-number . 2) (evens . -2) (odds . 1) (target . 6) (stage . playing)" +
            " (high-bid . #f) (bid-result 17 . 1) (trump . #f) (trump-exposed . #f)" +
            " (your-hand #s(card jack 7 club 3) #s(card seven 0 heart 0)) (trick-leader . 0)" +
            " (trick (0 . #s(card ace 5 heart 1))) (points (0 . 3) (1 . 0) (2 . 4) (3 . 0))" +
            " (tricks (0 . 1) (1 . 0) (2 . 1) (3 . 0)) (last-result . #f) (awaiting . 2)" +
            " (request play-card (cards-played #s(card ace 5 heart 1))" +
            " (game-state (trump-suit . #f) (first-suit . heart)))))"

    @Test
    fun `socket drop retries about every 3s until the snapshot restores`() = runTest {
        val h = harness()
        seatedAtTwo(h)
        val first = h.socket
        assertEquals(emptyList(), h.reconnectStates)

        h.session.dropConnection()
        settle()
        // the model is wiped, the holding state is up, the seat is remembered
        assertEquals(GamePhase.IDLE, h.state.phase)
        assertEquals(emptyList(), h.state.hand)
        assertEquals(listOf(true), h.reconnectStates)
        assertEquals(ME, h.store.activeMatch)
        assertEquals("Connection lost — trying to rejoin…", h.statuses.last())
        assertEquals(1, h.sockets.size)

        // two attempts fail to connect, each 3s apart
        h.failConnects = 2
        settle(REJOIN_RETRY_MS - 1)
        assertEquals(1, h.connectUrls.size)
        settle(1)
        assertEquals(2, h.connectUrls.size)
        settle(REJOIN_RETRY_MS)
        assertEquals(3, h.connectUrls.size)
        assertEquals(1, h.sockets.size)

        // the third gets through and asks under the same email
        settle(REJOIN_RETRY_MS)
        val second = h.socket
        assertTrue(second !== first)
        assertEquals(listOf(HELLO, CONNECT), second.sent)
        settle(CONNECT_BEAT_MS)
        assertEquals("(rejoin \"$ME\")", second.sent.last())
        assertEquals("Rejoining your match…", h.statuses.last())

        second.receive(midHandSnapshot)
        settle()
        assertEquals(GamePhase.PLAYING, h.state.phase)
        assertEquals("their table", h.state.roomName)
        assertEquals(listOf("clubs-J", "hearts-7"), h.state.hand.map { it.id })
        assertEquals(listOf(ME, "host@x", "g@x", "bot-1"), h.state.seatMembers)
        assertEquals(2, h.state.playedCards.single().playerIndex) // server seat 0
        assertEquals(0, h.state.activePlayer)
        assertEquals(7, h.state.score) // seats 2 and 0, our team
        assertEquals(2, h.state.tricks)
        assertEquals(17, h.state.finalBid)
        assertEquals(3, h.state.bidWinner)
        assertEquals(-2, h.state.matchUs)
        assertEquals(1, h.state.matchThem)
        assertEquals(PendingRequest.Play(Suit.HEARTS, null), h.state.pendingRequest)
        assertEquals(listOf(true, false), h.reconnectStates)
        assertEquals("Reconnected — back at the table.", h.statuses.last())

        // the retrying stops, and the restored session is fully live
        settle(REJOIN_RETRY_MS * 5)
        assertEquals(2, h.sockets.size)
        assertTrue(h.session.playCardById("hearts-7"))
    }

    @Test
    fun `a between-hands snapshot brings the verdict back without a new chip`() = runTest {
        val h = harness()
        h.store.activeMatch = ME
        h.store.playerId = "abc123"
        h.session.rejoinMatch("Asha", ME)
        settle(CONNECT_BEAT_MS)
        h.socket.receive(
            "(game-snapshot ((room . r) (members \"j3\" \"j2\" \"g@x\" \"$ME\") (your-seat . 3)" +
                " (hand-number . 1) (evens . 1) (odds . 0) (target . 6) (stage . between-hands)" +
                " (high-bid 16 . 0) (bid-result 16 . 0) (trump . diamond) (trump-exposed . #t)" +
                " (your-hand) (trick-leader . 3) (trick) (points (0 . 10) (1 . 6) (2 . 9) (3 . 3))" +
                " (tricks (0 . 3) (1 . 2) (2 . 2) (3 . 1))" +
                " (last-result (hand . 1) (bidder . 0) (bid . 16) (made . #t) (delta . 1)" +
                " (evens . 1) (odds . 0) (target . 6)) (awaiting . #f) (request . #f)))"
        )
        settle()
        assertEquals(GamePhase.HAND_FINISHED, h.state.phase)
        assertTrue(h.state.isHost)
        assertEquals(0, h.state.matchUs)
        assertEquals(1, h.state.matchThem)
        assertEquals(1, h.actions.count { it is GameAction.HandResult })
        assertEquals(emptyList(), h.state.handResults)
        h.session.nextHand()
        assertEquals("(next-hand \"$ME\")", h.socket.sent.last())
    }

    @Test
    fun `frames already received are handled before the disconnect`() = runTest {
        val h = harness()
        seatedAtTwo(h)
        h.socket.receive("(turn 3)")
        h.session.dropConnection()
        settle()
        assertTrue(GameAction.ActivePlayerChanged(1) in h.actions)
        assertEquals(GamePhase.IDLE, h.state.phase)
    }

    @Test
    fun `a dead match ends the retries`() = runTest {
        val h = harness()
        seatedAtTwo(h)
        h.session.dropConnection()
        settle()
        settle(REJOIN_RETRY_MS + CONNECT_BEAT_MS)
        h.socket.receive("(no-running-game)")
        settle()
        assertNull(h.store.activeMatch)
        assertEquals(listOf(true, false), h.reconnectStates)
        assertEquals("That match has ended.", h.statuses.last())
        settle(REJOIN_RETRY_MS * 3)
        assertEquals(2, h.sockets.size)
    }

    @Test
    fun `a drop with no match in progress does not retry`() = runTest {
        val h = harness()
        h.session.browseTables("Asha")
        settle(CONNECT_BEAT_MS)
        h.session.dropConnection()
        settle()
        settle(REJOIN_RETRY_MS * 3)
        assertEquals(1, h.sockets.size)
        assertEquals(emptyList(), h.reconnectStates)
        assertEquals("Disconnected from the game server.", h.statuses.last())

        // and a fresh start works
        h.session.browseTables("Asha")
        settle()
        assertEquals(2, h.sockets.size)
    }

    @Test
    fun `a failed connect with no match reports and stays idle`() = runTest {
        val h = harness()
        h.failConnects = 1
        h.session.browseTables("Asha")
        settle()
        assertEquals("could not connect to ws://test", h.statuses.last())
        assertEquals(GamePhase.IDLE, h.state.phase)
        settle(REJOIN_RETRY_MS * 3)
        assertEquals(1, h.connectUrls.size)
    }

    @Test
    fun `already-in-match rejoins instead of arguing`() = runTest {
        val h = harness()
        h.session.createTable("Asha")
        settle(CONNECT_BEAT_MS)
        h.socket.receive("(error already-in-match)")
        settle()
        assertEquals("(rejoin \"$ME\")", h.socket.sent.last())
    }

    @Test
    fun `a stale invite hands back a working start screen`() = runTest {
        val h = harness()
        h.session.joinByLink("Asha", "gone")
        settle(CONNECT_BEAT_MS)
        h.socket.receive("(error \"no-such-room\")")
        settle()
        assertEquals(GamePhase.IDLE, h.state.phase)
        assertEquals("That table is no longer open.", h.statuses.last())
        assertTrue(h.socket.closed)
        assertEquals(emptyList(), h.reconnectStates)
    }

    // --- leaving ---

    @Test
    fun `leaving the match tells the server and never retries`() = runTest {
        val h = harness()
        seatedAtTwo(h)
        val socket = h.socket
        h.session.leaveMatch()
        assertEquals("(leave-game \"$ME\")", socket.sent.last())
        assertTrue(socket.closed)
        assertNull(h.store.activeMatch)
        assertEquals(initialGameModel, h.state)
        assertEquals("You left the match.", h.statuses.last())
        settle(REJOIN_RETRY_MS * 3)
        assertEquals(1, h.sockets.size)
    }

    @Test
    fun `abandoning during the retry gap forgets the seat`() = runTest {
        val h = harness()
        seatedAtTwo(h)
        h.session.dropConnection()
        settle()
        h.session.abandonInterruptedMatch()
        assertNull(h.store.activeMatch)
        assertEquals(listOf(true, false), h.reconnectStates)
        settle(REJOIN_RETRY_MS * 3)
        assertEquals(1, h.sockets.size)
    }

    @Test
    fun `abandoning while a retry is connected is an explicit leave`() = runTest {
        val h = harness()
        seatedAtTwo(h)
        h.session.dropConnection()
        settle()
        settle(REJOIN_RETRY_MS + CONNECT_BEAT_MS)
        val retry = h.socket
        h.session.abandonInterruptedMatch()
        assertEquals("(leave-game \"$ME\")", retry.sent.last())
        assertTrue(retry.closed)
        settle(REJOIN_RETRY_MS * 3)
        assertEquals(2, h.sockets.size)
    }

    @Test
    fun `destroy stops the retries but keeps the stored match`() = runTest {
        val h = harness()
        seatedAtTwo(h)
        h.session.dropConnection()
        settle()
        h.session.destroy()
        settle(REJOIN_RETRY_MS * 3)
        assertEquals(1, h.sockets.size)
        assertEquals(ME, h.session.storedActiveMatch()?.email)
        assertEquals("asha", h.session.storedActiveMatch()?.name?.lowercase())
    }

    @Test
    fun `going home after a match allows a fresh connection`() = runTest {
        val h = harness()
        seatedAtTwo(h)
        h.socket.receive("(match-over ((winner . evens) (evens . 6) (odds . 0) (hands . 7)))")
        settle()
        val finished = h.socket
        h.session.goHome()
        assertTrue(finished.closed)
        assertEquals(initialGameModel, h.state)
        h.session.browseTables("Asha")
        settle()
        assertEquals(2, h.sockets.size)
    }

    // --- abandonment gate ---

    @Test
    fun `the abandon gate reports and resolves`() = runTest {
        val h = harness()
        seatedAtTwo(h)
        h.socket.receive("(seat-abandoned 3 \"host-abc999@game.local\")")
        settle()
        assertEquals("host", h.abandoned.last())
        h.session.resolveAbandon(AbandonChoice.REPLACE)
        assertEquals("(replace-with-bot \"$ME\")", h.socket.sent.last())
        h.socket.receive("(seat-replaced 3 bot-7 (\"g@x\" bot-1 \"$ME\" bot-7))")
        settle()
        assertEquals(1, h.resolved)
        assertEquals(listOf(ME, "bot-7", "g@x", "bot-1"), h.state.seatMembers)
        assertEquals("Asha", h.state.seatNames?.get(1))
    }

    @Test
    fun `kick and emote send the right frames`() = runTest {
        val h = harness()
        h.session.createTable("Asha")
        settle(CONNECT_BEAT_MS)
        h.socket.receive("room-created")
        settle()
        h.session.kick("bot-1")
        assertEquals("(kick-from-room \"Asha's table #abc\" \"$ME\" \"bot-1\")", h.socket.sent.last())
        val sent = h.socket.sent.size
        h.session.kick(ME) // never ourselves
        h.session.sendEmote("fire") // not in a match yet
        assertEquals(sent, h.socket.sent.size)

        h.socket.receive("(room-members \"r\" (bot-3 bot-2 bot-1 \"$ME\"))", MIN_DEAL)
        settle()
        h.session.sendEmote("fire")
        assertEquals("(emote \"$ME\" fire)", h.socket.sent.last())
    }

    // --- update-required ---

    @Test
    fun `update-required mid-retry stops the retries and reports`() = runTest {
        val h = harness()
        seatedAtTwo(h)
        h.session.dropConnection()
        settle()
        settle(REJOIN_RETRY_MS + CONNECT_BEAT_MS)
        assertEquals(2, h.sockets.size)

        h.socket.receive("(update-required 2)")
        settle()
        assertEquals(listOf(2), h.updates)
        assertEquals(listOf(true, false), h.reconnectStates)
        assertEquals(GamePhase.IDLE, h.state.phase)
        assertTrue(h.socket.closed)
        settle(REJOIN_RETRY_MS * 10)
        assertEquals(2, h.sockets.size)
        // the seat stays stored, so the updated app can still rejoin
        assertEquals(ME, h.store.activeMatch)
    }

    @Test
    fun `an answer inside the connect beat stops the intent from being sent`() = runTest {
        val h = harness()
        h.session.createTable("Asha")
        settle()
        h.socket.receive("(update-required 3)")
        settle(CONNECT_BEAT_MS)
        assertEquals(listOf(HELLO, CONNECT), h.socket.sent)
        assertEquals(listOf(3), h.updates)
        assertTrue(h.socket.closed)
    }

    @Test
    fun `after update-required no new connection is attempted`() = runTest {
        val h = harness()
        h.session.browseTables("Asha")
        settle()
        h.socket.receive("(update-required 2)")
        settle(CONNECT_BEAT_MS)
        h.session.createTable("Asha")
        h.session.joinByLink("Asha", "t")
        settle()
        assertEquals(1, h.connectUrls.size)
        assertEquals(listOf(2, 2, 2), h.updates)
    }

    @Test
    fun `a socket closed by the server after update-required does not retry`() = runTest {
        val h = harness()
        seatedAtTwo(h)
        h.socket.receive("(update-required 2)")
        h.session.dropConnection()
        settle()
        settle(REJOIN_RETRY_MS * 3)
        assertEquals(1, h.sockets.size)
        assertEquals(emptyList(), h.reconnectStates)
        assertEquals(listOf(2), h.updates)
    }
}
