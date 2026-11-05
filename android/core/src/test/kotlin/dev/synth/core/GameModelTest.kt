package dev.synth.core

import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFalse
import kotlin.test.assertNull
import kotlin.test.assertTrue

// Port of app/src/game/GameModel.test.ts, case for case.
class GameModelTest {
    private fun card(suit: Suit, rank: Rank) = CardModel("${suit.id}-${rank.id}", suit, rank)

    private val h8 = card(Suit.HEARTS, Rank.EIGHT)
    private val hK = card(Suit.HEARTS, Rank.K)
    private val c7 = card(Suit.CLUBS, Rank.SEVEN)
    private val cJ = card(Suit.CLUBS, Rank.J)
    private val s9 = card(Suit.SPADES, Rank.NINE)

    private val playing = initialGameModel.copy(phase = GamePhase.PLAYING)

    // --- isLegalPlay ---

    @Test
    fun `rejects everything when the server is not waiting on us`() {
        assertFalse(isLegalPlay(playing.copy(hand = listOf(h8), pendingRequest = null), h8))
    }

    @Test
    fun `allows any card when leading`() {
        val model = playing.copy(hand = listOf(h8, c7), pendingRequest = PendingRequest.Play(null, null))
        assertTrue(isLegalPlay(model, h8))
        assertTrue(isLegalPlay(model, c7))
    }

    @Test
    fun `requires following suit while we hold the suit led`() {
        val model = playing.copy(hand = listOf(hK, c7), pendingRequest = PendingRequest.Play(Suit.HEARTS, null))
        assertTrue(isLegalPlay(model, hK))
        assertFalse(isLegalPlay(model, c7))
    }

    @Test
    fun `never lets a trump excuse breaking suit`() {
        val model = playing.copy(hand = listOf(hK, cJ), pendingRequest = PendingRequest.Play(Suit.HEARTS, Suit.CLUBS))
        assertFalse(isLegalPlay(model, cJ))
        assertTrue(isLegalPlay(model, hK))
    }

    @Test
    fun `allows anything once void in the suit led`() {
        val model = playing.copy(hand = listOf(c7, s9), pendingRequest = PendingRequest.Play(Suit.HEARTS, null))
        assertTrue(isLegalPlay(model, c7))
        assertTrue(isLegalPlay(model, s9))
    }

    @Test
    fun `forces a trump right after we called for the exposure`() {
        val model = playing.copy(
            hand = listOf(c7, s9),
            pendingRequest = PendingRequest.Play(Suit.HEARTS, Suit.CLUBS, mustTrump = true),
        )
        assertTrue(isLegalPlay(model, c7))
        assertFalse(isLegalPlay(model, s9))
    }

    @Test
    fun `lets the exposer discard when they hold no trump`() {
        val model = playing.copy(
            hand = listOf(s9),
            pendingRequest = PendingRequest.Play(Suit.HEARTS, Suit.CLUBS, mustTrump = true),
        )
        assertTrue(isLegalPlay(model, s9))
    }

    // --- canExposeTrump ---

    @Test
    fun `expose is offered only when we cannot follow suit and trump is hidden`() {
        val base = playing.copy(hand = listOf(c7, s9), pendingRequest = PendingRequest.Play(Suit.HEARTS, null))
        assertTrue(canExposeTrump(base))
        assertFalse(canExposeTrump(base.copy(hand = listOf(hK, c7)))) // can follow
        assertFalse(canExposeTrump(base.copy(trumpExposed = true)))
        // leaders cannot call
        assertFalse(canExposeTrump(playing.copy(hand = listOf(c7), pendingRequest = PendingRequest.Play(null, null))))
    }

    // --- actingHostEmail ---

    @Test
    fun `keeps the duty with the original (last) host while human`() {
        assertEquals("host@x", actingHostEmail(listOf("bot-2", "bot-1", "guest@x", "host@x")))
    }

    @Test
    fun `passes the duty to the first human when the host seat is botted`() {
        assertEquals("guest@x", actingHostEmail(listOf("bot-2", "guest@x", "other@x", "bot-7")))
    }

    @Test
    fun `is nobody at an all-bot table`() {
        assertNull(actingHostEmail(listOf("bot-1", "bot-2", "bot-3", "bot-4")))
    }

    // --- gameReducer ---

    @Test
    fun `reveals the trump suit to every table view before another play request`() {
        val views = listOf(
            playing.copy(trumpSuit = Suit.CLUBS, trumpExposed = false), // bidder already knows
            playing.copy(trumpSuit = null, trumpExposed = false),
            playing.copy(trumpSuit = null, trumpExposed = false),
            playing.copy(trumpSuit = null, trumpExposed = false),
        )
        val revealed = views.mapIndexed { playerIndex, view ->
            gameReducer(view, GameAction.TrumpRevealed(Suit.CLUBS, playerIndex))
        }
        assertTrue(revealed.all { it.trumpSuit == Suit.CLUBS })
        assertTrue(revealed.all { it.trumpExposed })
        assertTrue(revealed.all { it.pendingRequest == null })
    }

    @Test
    fun `counts bids and passes through the auction`() {
        var model = initialGameModel
        model = gameReducer(model, GameAction.BidPlaced(1, BidValue.Amount(17)))
        model = gameReducer(model, GameAction.BidPlaced(2, BidValue.Pass))
        assertEquals(2, model.bidsPlaced)
        assertEquals(17, model.currentBid)
    }

    @Test
    fun `clears exactly one trick on TrickEnded, keeping an early next lead`() {
        var model = playing.copy(
            playedCards = listOf(
                PlayedCardModel(h8, 1),
                PlayedCardModel(hK, 2),
                PlayedCardModel(s9, 3),
                PlayedCardModel(c7, 0),
                PlayedCardModel(cJ, 0), // already led to the next trick
            ),
        )
        model = gameReducer(model, GameAction.TrickEnded(0, 2))
        assertEquals(listOf(PlayedCardModel(cJ, 0)), model.playedCards)
        assertEquals(2, model.score)
        assertEquals(1, model.tricks)
    }

    @Test
    fun `attributes trick points to the right team`() {
        val model = gameReducer(playing, GameAction.TrickEnded(3, 5))
        assertEquals(0, model.score)
        assertEquals(5, model.theirScore)
        assertEquals(1, model.theirTricks)
    }

    @Test
    fun `resets everything when leaving a room mid-game`() {
        val midGame = playing.copy(
            hand = listOf(h8),
            playedCards = listOf(PlayedCardModel(c7, 2)),
            score = 9,
            trumpSuit = Suit.CLUBS,
            trumpExposed = true,
            seatNames = listOf("a", "b", "c", "d"),
            matchUs = 4,
            matchThem = -2,
            handNumber = 5,
        )
        assertEquals(initialGameModel, gameReducer(midGame, GameAction.RoomLeft))
    }

    @Test
    fun `pins raw seat identities with their public names`() {
        val model = gameReducer(
            initialGameModel,
            GameAction.SeatNamesSet(
                members = listOf("me@x", "bot-6", "friend@x", "bot-7"),
                names = listOf("Me", "Dev", "Friend", "Asha"),
            ),
        )
        assertEquals(listOf("me@x", "bot-6", "friend@x", "bot-7"), model.seatMembers)
        assertEquals(listOf("Me", "Dev", "Friend", "Asha"), model.seatNames)
    }

    @Test
    fun `wipes everything when the session dies (phase to idle)`() {
        // a dropped socket mid-match: nothing of the hand may survive into a
        // game started later on a fresh session (the 20-card bug)
        val midGame = playing.copy(
            hand = listOf(h8, hK, c7),
            playedCards = listOf(PlayedCardModel(cJ, 1)),
            score = 12,
            trumpSuit = Suit.CLUBS,
            matchUs = 3,
            handNumber = 4,
        )
        assertEquals(initialGameModel, gameReducer(midGame, GameAction.PhaseChanged(GamePhase.IDLE)))
    }

    @Test
    fun `never carries cards into a freshly entered room`() {
        val stale = playing.copy(hand = listOf(h8, hK), playedCards = listOf(PlayedCardModel(c7, 3)))
        val lobby = gameReducer(stale, GameAction.RoomEntered("table", listOf("me@game.local"), true, 6))
        assertEquals(emptyList(), lobby.hand)
        assertEquals(emptyList(), lobby.playedCards)
    }

    @Test
    fun `totals team points when a hand finishes`() {
        val model = gameReducer(playing, GameAction.HandFinished(listOf(10, 6, 9, 3)))
        assertEquals(GamePhase.HAND_FINISHED, model.phase)
        assertEquals(19, model.score)
        assertEquals(9, model.theirScore)
    }

    @Test
    fun `records the match score from a hand result`() {
        val model = gameReducer(playing, GameAction.HandResult(1, 17, false, 2, -2, 6))
        assertEquals(2, model.matchUs)
        assertEquals(-2, model.matchThem)
        assertEquals(6, model.matchTarget)
    }

    @Test
    fun `accumulates per-hand swings for the match-over chips`() {
        // hand 1: we made +2; hand 2: they made +1 (a −1 swing for us)
        val afterHand1 = gameReducer(playing, GameAction.HandResult(0, 20, true, 2, 0, 6))
        val afterHand2 = gameReducer(afterHand1, GameAction.HandResult(1, 16, true, 2, 1, 6))
        assertEquals(listOf(2, -1), afterHand2.handResults)
        // a restored snapshot re-announcing the same scores adds no chip
        val replayed = gameReducer(afterHand2, GameAction.HandResult(1, 16, true, 2, 1, 6))
        assertEquals(listOf(2, -1), replayed.handResults)
    }

    @Test
    fun `resets the hand but preserves the room and the match on HandReset`() {
        val betweenHands = initialGameModel.copy(
            phase = GamePhase.HAND_FINISHED,
            roomName = "table",
            members = listOf("a", "b", "c", "d"),
            isHost = true,
            seatNames = listOf("You", "b", "c", "d"),
            hand = listOf(h8),
            playedCards = listOf(PlayedCardModel(c7, 2)),
            activePlayer = 2,
            score = 19,
            tricks = 5,
            theirScore = 9,
            theirTricks = 3,
            currentBid = 21,
            bidsPlaced = 4,
            bidWinner = 1,
            finalBid = 21,
            trumpSuit = Suit.CLUBS,
            trumpExposed = true,
            pendingRequest = PendingRequest.Trump,
            points = listOf(10, 6, 9, 3),
            matchUs = 2,
            matchThem = -2,
            matchTarget = 6,
            handNumber = 3,
        )
        assertEquals(
            initialGameModel.copy(
                phase = GamePhase.BIDDING,
                roomName = "table",
                members = listOf("a", "b", "c", "d"),
                isHost = true,
                seatNames = listOf("You", "b", "c", "d"),
                matchUs = 2,
                matchThem = -2,
                handNumber = 4,
            ),
            gameReducer(betweenHands, GameAction.HandReset),
        )
    }

    @Test
    fun `ends the match`() {
        val model = gameReducer(
            initialGameModel.copy(phase = GamePhase.HAND_FINISHED, matchUs = 4, matchThem = -2),
            GameAction.MatchOver(MatchSide.US, 6, -2, 7),
        )
        assertEquals(GamePhase.FINISHED, model.phase)
        assertEquals(MatchSide.US, model.matchWinner)
        assertEquals(6, model.matchUs)
        assertEquals(-2, model.matchThem)
    }
}
