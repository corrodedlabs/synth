// Port of app/src/game/GameModel.ts. Keep the two in step.
package dev.synth.core

enum class Suit(val id: String) {
    HEARTS("hearts"), DIAMONDS("diamonds"), CLUBS("clubs"), SPADES("spades"),
}

// Declaration order is trick strength, as in the TS RANKS array.
enum class Rank(val id: String) {
    J("J"), NINE("9"), A("A"), TEN("10"), K("K"), Q("Q"), EIGHT("8"), SEVEN("7"),
}

val SUITS: List<Suit> = Suit.entries
val RANKS: List<Rank> = Rank.entries

// 0 = us (bottom), 1 = right, 2 = top (partner), 3 = left.
typealias PlayerIndex = Int

data class CardModel(val id: String, val suit: Suit, val rank: Rank)

data class PlayedCardModel(val card: CardModel, val playerIndex: PlayerIndex)

enum class GamePhase(val id: String) {
    IDLE("idle"),
    CONNECTING("connecting"),
    LOBBY("lobby"),
    BIDDING("bidding"),
    CHOOSING_TRUMP("choosing-trump"),
    PLAYING("playing"),
    HAND_FINISHED("hand-finished"), // between hands: result up, waiting on the host's deal
    FINISHED("finished"), // the whole match is over
}

enum class MatchSide { US, THEM }

// What the server is currently waiting on from us.
sealed interface PendingRequest {
    data class Bid(val minBid: Int) : PendingRequest
    data object Trump : PendingRequest
    data class Play(
        val firstSuit: Suit?,
        val trumpSuit: Suit?,
        // we just called for the exposure, so we must trump if we can
        val mustTrump: Boolean = false,
    ) : PendingRequest
}

data class GameModel(
    val phase: GamePhase = GamePhase.IDLE,
    // Lobby state. Members are in *server* order (newest joiner first, host last).
    val roomName: String? = null,
    val members: List<String> = emptyList(),
    val isHost: Boolean = false,
    // Public identities by view index (0 = us), fixed once the hand is dealt.
    // The raw member ids stay available so the renderer can resolve avatars
    // without leaking the server's `bot-N` implementation names into the UI.
    val seatMembers: List<String>? = null,
    val seatNames: List<String>? = null,
    // All player indices in the model are *view* indices.
    val hand: List<CardModel> = emptyList(),
    val playedCards: List<PlayedCardModel> = emptyList(),
    val activePlayer: PlayerIndex? = null,
    val score: Int = 0, // our team's card points
    val tricks: Int = 0, // our team's tricks
    val theirScore: Int = 0,
    val theirTricks: Int = 0,
    val currentBid: Int = 16,
    val bidsPlaced: Int = 0, // bids and passes seen this auction
    val bidWinner: PlayerIndex? = null,
    val finalBid: Int? = null,
    val trumpSuit: Suit? = null, // known if we chose it or after exposure
    val trumpExposed: Boolean = false,
    val pendingRequest: PendingRequest? = null,
    val points: List<Int>? = null, // by view index, set at hand end
    // Match state — survives between hands, cleared only on RoomLeft.
    val matchUs: Int = 0, // our team's game points
    val matchThem: Int = 0,
    val matchTarget: Int = 6,
    // per-hand swing from our side: +N when our score moved up, −N when
    // theirs did — the match-over panel renders these as chips
    val handResults: List<Int> = emptyList(),
    val handNumber: Int = 1,
    val matchWinner: MatchSide? = null,
)

sealed interface GameAction {
    data class PhaseChanged(val phase: GamePhase) : GameAction
    data class RoomEntered(
        val roomName: String,
        val members: List<String>,
        val isHost: Boolean,
        val target: Int? = null, // what this table plays to, when known
    ) : GameAction
    data class MembersChanged(val members: List<String>, val target: Int? = null) : GameAction
    data object RoomLeft : GameAction
    data class SeatNamesSet(val members: List<String>, val names: List<String>) : GameAction
    data class HandDealt(val cards: List<CardModel>) : GameAction
    data class CardPlayed(val card: CardModel, val playerIndex: PlayerIndex) : GameAction
    data class ActivePlayerChanged(val playerIndex: PlayerIndex?) : GameAction
    data class TrickScored(val scoreDelta: Int, val tricksDelta: Int) : GameAction
    data class BidPlaced(val playerIndex: PlayerIndex, val bid: BidValue) : GameAction
    data class BidWon(val playerIndex: PlayerIndex, val bid: Int) : GameAction
    data class TrumpSet(val suit: Suit?, val exposed: Boolean) : GameAction
    data class TrumpRevealed(val suit: Suit, val playerIndex: PlayerIndex) : GameAction
    data class RequestReceived(val request: PendingRequest) : GameAction
    data object RequestCleared : GameAction
    data object TrickCleared : GameAction
    data class TrickEnded(val winner: PlayerIndex?, val points: Int) : GameAction
    data class HandFinished(val points: List<Int>) : GameAction

    // game points after a hand (already translated to our perspective)
    data class HandResult(
        val bidder: PlayerIndex,
        val bid: Int,
        val made: Boolean,
        val us: Int,
        val them: Int,
        val target: Int,
    ) : GameAction

    data object HandReset : GameAction // next hand is being dealt
    data class MatchOver(val winner: MatchSide, val us: Int, val them: Int, val hands: Int) : GameAction

    // a reconnect: the session rebuilds the entire model from the server's
    // mid-match snapshot and swaps it in wholesale
    data class SnapshotRestored(val model: GameModel) : GameAction
}

val initialGameModel: GameModel = GameModel()

fun gameReducer(state: GameModel, action: GameAction): GameModel = when (action) {
    // idle means the session died (socket drop, connect failure, server
    // gone): wipe everything so a stale hand can never leak into a game
    // started later on a fresh session
    is GameAction.PhaseChanged ->
        if (action.phase == GamePhase.IDLE) initialGameModel else state.copy(phase = action.phase)

    is GameAction.RoomEntered -> state.copy(
        phase = GamePhase.LOBBY,
        roomName = action.roomName,
        members = action.members,
        isHost = action.isHost,
        // a fresh table never inherits cards from a previous life
        hand = emptyList(),
        playedCards = emptyList(),
        matchTarget = action.target ?: state.matchTarget,
        handResults = emptyList(),
    )

    is GameAction.MembersChanged -> state.copy(
        members = action.members,
        matchTarget = action.target ?: state.matchTarget,
    )

    // back to the start screen — also wipes any in-progress game state
    // (hand, trick, scores) so an aborted game leaves nothing behind
    GameAction.RoomLeft -> initialGameModel

    is GameAction.SeatNamesSet -> state.copy(seatMembers = action.members, seatNames = action.names)

    is GameAction.HandDealt -> state.copy(
        // the first deal moves us from the lobby into the game
        phase = if (state.phase == GamePhase.LOBBY) GamePhase.BIDDING else state.phase,
        hand = state.hand + action.cards,
    )

    is GameAction.CardPlayed -> state.copy(
        hand = state.hand.filter { it.id != action.card.id },
        playedCards = state.playedCards + PlayedCardModel(action.card, action.playerIndex),
    )

    is GameAction.ActivePlayerChanged -> state.copy(activePlayer = action.playerIndex)

    is GameAction.TrickScored -> state.copy(
        score = state.score + action.scoreDelta,
        tricks = state.tricks + action.tricksDelta,
    )

    is GameAction.BidPlaced -> state.copy(
        currentBid = when (val bid = action.bid) {
            BidValue.Pass -> state.currentBid
            is BidValue.Amount -> bid.value
        },
        bidsPlaced = state.bidsPlaced + 1,
    )

    is GameAction.BidWon -> state.copy(
        bidWinner = action.playerIndex,
        finalBid = action.bid,
        phase = GamePhase.CHOOSING_TRUMP,
    )

    is GameAction.TrumpSet -> state.copy(
        trumpSuit = action.suit ?: state.trumpSuit,
        trumpExposed = action.exposed || state.trumpExposed,
        phase = if (state.phase == GamePhase.CHOOSING_TRUMP) GamePhase.PLAYING else state.phase,
    )

    is GameAction.TrumpRevealed -> state.copy(
        trumpSuit = action.suit,
        trumpExposed = true,
        phase = if (state.phase == GamePhase.CHOOSING_TRUMP) GamePhase.PLAYING else state.phase,
    )

    is GameAction.RequestReceived -> state.copy(pendingRequest = action.request)

    GameAction.RequestCleared -> state.copy(pendingRequest = null)

    GameAction.TrickCleared -> state.copy(playedCards = emptyList())

    is GameAction.TrickEnded -> {
        val ours = action.winner == 0 || action.winner == 2
        val theirs = !ours && action.winner != null
        state.copy(
            // only the completed trick leaves the table — a card already led to
            // the next trick (possible at bot speed) must survive
            playedCards = state.playedCards.drop(4),
            score = if (ours) state.score + action.points else state.score,
            tricks = if (ours) state.tricks + 1 else state.tricks,
            theirScore = if (theirs) state.theirScore + action.points else state.theirScore,
            theirTricks = if (theirs) state.theirTricks + 1 else state.theirTricks,
        )
    }

    is GameAction.HandFinished -> state.copy(
        phase = GamePhase.HAND_FINISHED,
        points = action.points,
        score = action.points[0] + action.points[2],
        theirScore = action.points[1] + action.points[3],
        activePlayer = null,
        pendingRequest = null,
    )

    is GameAction.HandResult -> {
        // exactly one team's score moves per hand; a zero swing is a restored
        // snapshot re-announcing a hand already counted
        val swing = action.us - state.matchUs - (action.them - state.matchThem)
        state.copy(
            matchUs = action.us,
            matchThem = action.them,
            matchTarget = action.target,
            handResults = if (swing == 0) state.handResults else state.handResults + swing,
        )
    }

    // everything a single hand owns goes back to its initial value;
    // the room, the seats, and the match score live on
    GameAction.HandReset -> state.copy(
        phase = GamePhase.BIDDING,
        hand = emptyList(),
        playedCards = emptyList(),
        activePlayer = null,
        score = 0,
        tricks = 0,
        theirScore = 0,
        theirTricks = 0,
        currentBid = 16,
        bidsPlaced = 0,
        bidWinner = null,
        finalBid = null,
        trumpSuit = null,
        trumpExposed = false,
        pendingRequest = null,
        points = null,
        handNumber = state.handNumber + 1,
    )

    is GameAction.MatchOver -> state.copy(
        phase = GamePhase.FINISHED,
        matchWinner = action.winner,
        matchUs = action.us,
        matchThem = action.them,
        activePlayer = null,
        pendingRequest = null,
    )

    is GameAction.SnapshotRestored -> action.model
}

// Mirrors the play rules in game.rkt — used only as a UX guard; the server
// remains the authority. Follow suit if you can; trumping never excuses
// breaking suit. After calling for the exposure you must trump if able.
fun isLegalPlay(model: GameModel, card: CardModel): Boolean {
    val request = model.pendingRequest as? PendingRequest.Play ?: return false
    if (
        request.mustTrump &&
        request.trumpSuit != null &&
        model.hand.any { it.suit == request.trumpSuit }
    ) {
        return card.suit == request.trumpSuit
    }
    if (request.firstSuit == null) return true
    if (card.suit == request.firstSuit) return true
    return model.hand.none { it.suit == request.firstSuit }
}

private val BOT_MEMBER = Regex("bot-[0-9]+")

// Host duties mid-match (the between-hands deal button) belong to the
// original host — the last member in seat order — while their seat is
// still human; a botted host seat passes the duty to the first human.
// Mirrors the server's acting-host.
fun actingHostEmail(members: List<String>): String? {
    fun isHuman(member: String) = !BOT_MEMBER.matches(member)
    val last = members.lastOrNull()
    if (last != null && isHuman(last)) return last
    return members.firstOrNull { isHuman(it) }
}

// Expose-trump is offered when we cannot follow suit and trump is still hidden.
fun canExposeTrump(model: GameModel): Boolean {
    val request = model.pendingRequest as? PendingRequest.Play ?: return false
    if (model.trumpExposed || request.trumpSuit != null) return false
    if (request.firstSuit == null) return false
    return model.hand.none { it.suit == request.firstSuit }
}
