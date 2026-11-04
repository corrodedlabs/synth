package dev.synth.core

import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertIs
import kotlin.test.assertNull

// Port of app/src/net/protocol.test.ts, case for case.
class ProtocolTest {
    private fun card(suit: Suit, rank: Rank) = CardModel(cardId(suit, rank), suit, rank)

    // --- card mapping ---

    @Test
    fun `decodes server cards`() {
        val jack = decodeCard(parseSExpr("#s(card jack 7 club 3)"))
        assertEquals(CardModel("clubs-J", Suit.CLUBS, Rank.J), jack)
        val ten = decodeCard(parseSExpr("#s(card ten 4 heart 1)"))
        assertEquals(CardModel("hearts-10", Suit.HEARTS, Rank.TEN), ten)
    }

    @Test
    fun `re-encodes cards exactly as game rkt builds them`() {
        // ranks double as trick strength: J 9 A 10 K Q 8 7 → 7..0
        val samples = listOf(
            "#s(card jack 7 club 3)",
            "#s(card nine 6 diamond 2)",
            "#s(card ace 5 spade 1)",
            "#s(card ten 4 club 1)",
            "#s(card king 3 heart 0)",
            "#s(card queen 2 diamond 0)",
            "#s(card eight 1 heart 0)",
            "#s(card seven 0 spade 0)",
        )
        for (sample in samples) {
            assertEquals(parseSExpr(sample), encodeCard(decodeCard(parseSExpr(sample))))
        }
    }

    // --- decodeServerEvent ---

    @Test
    fun `ignores void frames`() {
        assertNull(decodeServerEvent("#<void>"))
    }

    @Test
    fun `decodes room flow`() {
        assertEquals(ServerEvent.RoomCreated, decodeServerEvent("room-created"))
        assertEquals(
            ServerEvent.RoomMembers(listOf("bot-2", "bot-1", "me@x")),
            decodeServerEvent("(room-members \"r\" (bot-2 bot-1 \"me@x\"))"),
        )
        // newer servers append what the table plays to
        assertEquals(
            ServerEvent.RoomMembers(listOf("me@x"), target = 2),
            decodeServerEvent("(room-members \"r\" (\"me@x\") 2)"),
        )
        assertEquals(ServerEvent.GameStarted, decodeServerEvent("(game-started)"))
    }

    @Test
    fun `decodes the lobby flow`() {
        assertEquals(ServerEvent.RoomJoined, decodeServerEvent("room-joined"))
        assertEquals(ServerEvent.RoomLeft, decodeServerEvent("room-left"))
        assertEquals(ServerEvent.RoomClosed, decodeServerEvent("(room-closed \"a's table\")"))
        assertEquals(ServerEvent.RemovedFromRoom, decodeServerEvent("(removed-from-room \"a's table\")"))
        assertEquals(ServerEvent.StartGameFailed("room-not-ready"), decodeServerEvent("(start-game-failed room-not-ready)"))
        // exact shape the server's game-room->list produces; rooms without a
        // target (older servers) read as classic
        assertEquals(
            ServerEvent.ActiveRooms(
                listOf(
                    RoomInfo("a@x", "a's table", listOf("bot-1", "a@x"), 2),
                    RoomInfo("b@x", "b's table", listOf("b@x"), 6),
                )
            ),
            decodeServerEvent(
                "(active-rooms (((host . \"a@x\") (name . \"a's table\") (members \"bot-1\" \"a@x\") (target . 2)) ((host . \"b@x\") (name . \"b's table\") (members \"b@x\"))))"
            ),
        )
        assertEquals(ServerEvent.ActiveRooms(emptyList()), decodeServerEvent("(active-rooms ())"))
    }

    @Test
    fun `decodes the bidding flow`() {
        assertEquals(ServerEvent.Turn(2), decodeServerEvent("(turn 2)"))
        assertEquals(ServerEvent.BidRequested(16), decodeServerEvent("(request-bid 16)"))
        assertEquals(ServerEvent.BidPlaced(1, BidValue.Amount(17)), decodeServerEvent("(bid-placed 1 17)"))
        assertEquals(ServerEvent.BidPlaced(2, BidValue.Pass), decodeServerEvent("(bid-placed 2 pass)"))
        assertEquals(ServerEvent.BidResult(19, 1), decodeServerEvent("(bid-result (19 . 1))"))
        assertEquals(ServerEvent.ChooseTrumpRequested, decodeServerEvent("(choose-trump)"))
        assertEquals(ServerEvent.TrumpSelected, decodeServerEvent("(trump-selected)"))
    }

    @Test
    fun `decodes hands and plays`() {
        assertEquals(
            ServerEvent.HandDealt(listOf(CardModel("clubs-J", Suit.CLUBS, Rank.J))),
            decodeServerEvent("(hand \"me@x\" (#s(card jack 7 club 3)))"),
        )
        assertEquals(
            ServerEvent.CardPlayed(3, CardModel("hearts-8", Suit.HEARTS, Rank.EIGHT)),
            decodeServerEvent("(played 3 #s(card eight 1 heart 0))"),
        )
        assertEquals(ServerEvent.TrumpExposed(3, Suit.CLUBS), decodeServerEvent("(played 3 expose-trump club)"))
    }

    @Test
    fun `decodes the end of a game`() {
        assertEquals(ServerEvent.GameAborted, decodeServerEvent("(game-aborted \"a's table\")"))
    }

    @Test
    fun `decodes play requests`() {
        assertEquals(
            ServerEvent.PlayRequested(listOf(CardModel("spades-A", Suit.SPADES, Rank.A)), null, Suit.SPADES),
            decodeServerEvent(
                "(play-card (cards-played #s(card ace 5 spade 1)) (game-state (trump-suit . #f) (first-suit . spade)))"
            ),
        )
        assertEquals(
            ServerEvent.PlayRequested(emptyList(), Suit.DIAMONDS, null),
            decodeServerEvent("(play-card (cards-played) (game-state (trump-suit . diamond) (first-suit . #f)))"),
        )
    }

    @Test
    fun `decodes points`() {
        val event = assertIs<ServerEvent.PointsWon>(
            decodeServerEvent("(points-won #hash((0 . 10) (1 . 10) (2 . 5) (3 . 3)))")
        )
        assertEquals(10, event.points[0])
        assertEquals(3, event.points[3])
    }

    @Test
    fun `decodes the match flow`() {
        assertEquals(
            ServerEvent.HandResult(2, 1, 19, true, 1, -2, 1, 6),
            decodeServerEvent(
                "(hand-result ((hand . 2) (bidder . 1) (bid . 19) (made . #t) (delta . 1) (evens . -2) (odds . 1) (target . 6)))"
            ),
        )
        assertEquals(
            ServerEvent.HandResult(1, 0, 20, false, -4, -4, 0, 6),
            decodeServerEvent(
                "(hand-result ((hand . 1) (bidder . 0) (bid . 20) (made . #f) (delta . -4) (evens . -4) (odds . 0) (target . 6)))"
            ),
        )
        assertEquals(
            ServerEvent.MatchOver(Team.ODDS, -2, 6, 7),
            decodeServerEvent("(match-over ((winner . odds) (evens . -2) (odds . 6) (hands . 7)))"),
        )
        // malformed bodies fall through instead of crashing the event loop
        assertEquals(ServerEvent.Ignored("(hand-result ((hand . 1)))"), decodeServerEvent("(hand-result ((hand . 1)))"))
        assertEquals(
            ServerEvent.Ignored("(match-over ((winner . nobody)))"),
            decodeServerEvent("(match-over ((winner . nobody)))"),
        )
    }

    @Test
    fun `decodes emotes`() {
        assertEquals(ServerEvent.EmotePlayed(2, "laugh"), decodeServerEvent("(emote-played 2 laugh)"))
        assertEquals(ServerEvent.Ignored("(emote-played x laugh)"), decodeServerEvent("(emote-played x laugh)"))
    }

    @Test
    fun `decodes the leaderboard`() {
        assertEquals(
            ServerEvent.Leaderboard(listOf(LeaderboardRow("asha", 3, 2), LeaderboardRow("badri", 3, 1))),
            decodeServerEvent("(leaderboard ((\"asha\" 3 2) (\"badri\" 3 1)))"),
        )
        assertEquals(ServerEvent.Leaderboard(emptyList()), decodeServerEvent("(leaderboard ())"))
        assertEquals("(get-leaderboard)", encodeCommand(ClientCommand.GetLeaderboard))
    }

    @Test
    fun `decodes the abandonment flow`() {
        assertEquals(ServerEvent.SeatAbandoned(2, "a@x"), decodeServerEvent("(seat-abandoned 2 \"a@x\")"))
        assertEquals(
            ServerEvent.SeatReplaced(2, "bot-7", listOf("bot-3", "bot-2", "bot-7", "host@x")),
            decodeServerEvent("(seat-replaced 2 bot-7 (bot-3 bot-2 bot-7 \"host@x\"))"),
        )
    }

    @Test
    fun `decodes the reconnection flow`() {
        assertEquals(ServerEvent.PlayerDisconnected("a@x", 45), decodeServerEvent("(player-disconnected \"a@x\" 45)"))
        assertEquals(ServerEvent.PlayerReconnected("a@x"), decodeServerEvent("(player-reconnected \"a@x\")"))
        assertEquals(ServerEvent.NoRunningGame, decodeServerEvent("(no-running-game)"))
    }

    @Test
    fun `decodes a mid-hand snapshot`() {
        // exact shape the server's rejoin-snapshot prints (verified via racket)
        val frame =
            "(game-snapshot ((room . my-room) (members \"j3\" \"j2\" \"me@x\" \"host@x\") (your-seat . 2)" +
                " (hand-number . 2) (evens . -2) (odds . 1) (target . 6) (stage . playing)" +
                " (high-bid . #f) (bid-result 17 . 1) (trump . #f) (trump-exposed . #f)" +
                " (your-hand #s(card jack 7 club 3) #s(card seven 0 heart 0)) (trick-leader . 0)" +
                " (trick (0 . #s(card ace 5 heart 1))) (points (0 . 3) (1 . 0) (2 . 4) (3 . 0))" +
                " (tricks (0 . 1) (1 . 0) (2 . 1) (3 . 0)) (last-result . #f) (awaiting . 2)" +
                " (request play-card (cards-played #s(card ace 5 heart 1))" +
                " (game-state (trump-suit . #f) (first-suit . heart)))))"
        val s = assertIs<ServerEvent.GameSnapshot>(decodeServerEvent(frame)).snapshot
        assertEquals("my-room", s.room)
        assertEquals(listOf("j3", "j2", "me@x", "host@x"), s.members)
        assertEquals(2, s.yourSeat)
        assertEquals(2, s.handNumber)
        assertEquals(-2, s.evens)
        assertEquals(1, s.odds)
        assertEquals(SnapshotStage.PLAYING, s.stage)
        assertNull(s.highBid)
        assertEquals(BidPair(17, 1), s.bidResult)
        assertNull(s.trumpSuit)
        assertEquals(false, s.trumpExposed)
        assertEquals(listOf("clubs-J", "hearts-7"), s.yourHand.map { it.id })
        assertEquals(listOf(TrickPlay(0, card(Suit.HEARTS, Rank.A))), s.trick)
        assertEquals(3, s.points[0])
        assertEquals(4, s.points[2])
        assertEquals(1, s.tricks[0])
        assertNull(s.lastResult)
        assertEquals(2, s.awaiting)
        assertEquals(
            ServerEvent.PlayRequested(listOf(card(Suit.HEARTS, Rank.A)), null, Suit.HEARTS),
            s.request,
        )
    }

    @Test
    fun `decodes a between-hands snapshot with its result`() {
        val frame =
            "(game-snapshot ((room . my-room) (members \"j3\" \"j2\" \"me@x\" \"host@x\") (your-seat . 3)" +
                " (hand-number . 1) (evens . 1) (odds . 0) (target . 6) (stage . between-hands)" +
                " (high-bid 16 . 0) (bid-result 16 . 0) (trump . diamond) (trump-exposed . #t)" +
                " (your-hand) (trick-leader . 3) (trick) (points (0 . 10) (1 . 6) (2 . 9) (3 . 3))" +
                " (tricks (0 . 3) (1 . 2) (2 . 2) (3 . 1))" +
                " (last-result (hand . 1) (bidder . 0) (bid . 16) (made . #t) (delta . 1)" +
                " (evens . 1) (odds . 0) (target . 6)) (awaiting . #f) (request . #f)))"
        val s = assertIs<ServerEvent.GameSnapshot>(decodeServerEvent(frame)).snapshot
        assertEquals(SnapshotStage.BETWEEN_HANDS, s.stage)
        assertEquals(emptyList(), s.yourHand)
        assertEquals(emptyList(), s.trick)
        assertEquals(Suit.DIAMONDS, s.trumpSuit)
        assertNull(s.awaiting)
        assertNull(s.request)
        assertEquals(ServerEvent.HandResult(1, 0, 16, true, 1, 1, 0, 6), s.lastResult)
    }

    @Test
    fun `passes unknown frames through as Ignored`() {
        assertEquals(ServerEvent.Ignored("(done)"), decodeServerEvent("(done)"))
        assertEquals(ServerEvent.Ignored("invalid-request"), decodeServerEvent("invalid-request"))
    }

    // --- encodeCommand ---

    @Test
    fun `encodes lobby commands`() {
        assertEquals("(join-room \"a's table\" \"b@x\")", encodeCommand(ClientCommand.JoinRoom("a's table", "b@x")))
        assertEquals("(leave-room \"a's table\" \"b@x\")", encodeCommand(ClientCommand.LeaveRoom("a's table", "b@x")))
        assertEquals(
            "(kick-from-room \"a's table\" \"a@x\" \"bot-1\")",
            encodeCommand(ClientCommand.KickFromRoom("a's table", "a@x", "bot-1")),
        )
        assertEquals("(get-active-rooms)", encodeCommand(ClientCommand.GetActiveRooms))
        assertEquals("(ping)", encodeCommand(ClientCommand.Ping))
    }

    @Test
    fun `encodes every command`() {
        assertEquals("(connect-user \"a@b\" \"p.png\")", encodeCommand(ClientCommand.ConnectUser("a@b", "p.png")))
        assertEquals("(make-room \"a@b\" \"r1\" 2)", encodeCommand(ClientCommand.MakeRoom("a@b", "r1", 2)))
        assertEquals("(add-bot-to-room \"r1\")", encodeCommand(ClientCommand.AddBot("r1")))
        assertEquals("(start-game \"r1\")", encodeCommand(ClientCommand.StartGame("r1")))
        assertEquals("(put-bid \"a@b\" 17)", encodeCommand(ClientCommand.PutBid("a@b", BidValue.Amount(17))))
        assertEquals("(put-bid \"a@b\" pass)", encodeCommand(ClientCommand.PutBid("a@b", BidValue.Pass)))
        assertEquals("(selected-trump \"a@b\" heart)", encodeCommand(ClientCommand.SelectTrump("a@b", Suit.HEARTS)))
        assertEquals(
            "(card-played \"a@b\" #s(card jack 7 club 3))",
            encodeCommand(ClientCommand.PlayCard("a@b", CardModel("clubs-J", Suit.CLUBS, Rank.J))),
        )
        assertEquals("(card-played \"a@b\" expose-trump)", encodeCommand(ClientCommand.ExposeTrump("a@b")))
        assertEquals("(next-hand \"a@b\")", encodeCommand(ClientCommand.NextHand("a@b")))
        assertEquals("(leave-game \"a@b\")", encodeCommand(ClientCommand.LeaveGame("a@b")))
        assertEquals("(rejoin \"a@b\")", encodeCommand(ClientCommand.Rejoin("a@b")))
        assertEquals("(emote \"a@b\" fire)", encodeCommand(ClientCommand.SendEmote("a@b", "fire")))
        assertEquals("(replace-with-bot \"a@b\")", encodeCommand(ClientCommand.ReplaceWithBot("a@b")))
        assertEquals("(close-game \"a@b\")", encodeCommand(ClientCommand.CloseGame("a@b")))
    }

    @Test
    fun `uses sym helper consistently`() {
        assertEquals("x", sym("x").name)
    }

    // --- not in the TS suite: the Android-only additions ---

    @Test
    fun `encodes the client hello`() {
        assertEquals(1, PROTOCOL_VERSION)
        assertEquals(
            "(client-hello \"android\" 1)",
            encodeCommand(ClientCommand.ClientHello(CLIENT_PLATFORM, PROTOCOL_VERSION)),
        )
    }

    @Test
    fun `malformed version answers are Ignored`() {
        assertEquals(ServerEvent.Ignored("(update-required soon)"), decodeServerEvent("(update-required soon)"))
        assertEquals(ServerEvent.Ignored("(protocol-ok)"), decodeServerEvent("(protocol-ok)"))
    }

    // The TS leaves these as garbage values or throws out of the socket handler.
    @Test
    fun `malformed cards and seats never throw out of the decoder`() {
        for (frame in listOf("(hand \"x\" (nonsense))", "(played 1 #s(card joker 9 club 0))", "(turn)", "(turn x)")) {
            assertEquals(ServerEvent.Ignored(frame), decodeServerEvent(frame))
        }
    }

    @Test
    fun `a bot can be asked for by seat`() {
        assertEquals("(add-bot-to-room \"r1\" 2)", encodeCommand(ClientCommand.AddBot("r1", 2)))
        assertEquals("(add-bot-to-room \"r1\")", encodeCommand(ClientCommand.AddBot("r1")))
    }

    @Test
    fun `room members carries the seats`() {
        assertEquals(
            ServerEvent.RoomMembers(listOf("a", "host"), 6, listOf("host", null, null, "a")),
            decodeServerEvent("(room-members \"r1\" (\"a\" \"host\") 6 (\"host\" \"\" \"\" \"a\"))"),
        )
    }
}
