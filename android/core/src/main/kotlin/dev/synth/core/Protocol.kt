// Port of app/src/net/protocol.ts. Keep the two in step.
//
// Typed encode/decode for the Racket server's s-expression protocol.
// Player indices in ServerEvents are *server* seat indices (positions in the
// server's players list); GameSession translates them to view indices.
package dev.synth.core

import kotlin.math.floor

// What this client speaks; announced by client-hello. Android only for now:
// the web client deploys with the server and never sends client-hello.
const val PROTOCOL_VERSION = 1
const val CLIENT_PLATFORM = "android"

// --- card mapping (mirrors deck generation in game.rkt) ---

private val RANK_TO_NAME = mapOf(
    Rank.J to "jack", Rank.Q to "queen", Rank.K to "king", Rank.A to "ace",
    Rank.TEN to "ten", Rank.NINE to "nine", Rank.EIGHT to "eight", Rank.SEVEN to "seven",
)

private val NAME_TO_RANK = RANK_TO_NAME.entries.associate { (rank, name) -> name to rank }

// game.rkt's numeric rank doubles as trick strength: J > 9 > A > 10 > K > Q > 8 > 7
private val RANK_TO_NUM = mapOf(
    Rank.J to 7, Rank.NINE to 6, Rank.A to 5, Rank.TEN to 4,
    Rank.K to 3, Rank.Q to 2, Rank.EIGHT to 1, Rank.SEVEN to 0,
)

private val RANK_TO_POINTS = mapOf(
    Rank.J to 3, Rank.NINE to 2, Rank.A to 1, Rank.TEN to 1,
    Rank.K to 0, Rank.Q to 0, Rank.EIGHT to 0, Rank.SEVEN to 0,
)

private val SERVER_TO_SUIT = mapOf(
    "heart" to Suit.HEARTS, "diamond" to Suit.DIAMONDS, "club" to Suit.CLUBS, "spade" to Suit.SPADES,
)

private val SUIT_TO_SERVER = SERVER_TO_SUIT.entries.associate { (name, suit) -> suit to name }

fun cardId(suit: Suit, rank: Rank): String = "${suit.id}-${rank.id}"

fun cardPoints(card: CardModel): Int = RANK_TO_POINTS.getValue(card.rank)

fun decodeCard(expr: SExpr): CardModel {
    if (expr !is Prefab || expr.name != "card") {
        throw IllegalArgumentException("protocol: not a card: ${writeSExpr(expr)}")
    }
    val name = expr.fields.getOrNull(0) as? Sym
    val suit = expr.fields.getOrNull(2) as? Sym
    if (name == null || suit == null) {
        throw IllegalArgumentException("protocol: malformed card: ${writeSExpr(expr)}")
    }
    val rank = NAME_TO_RANK[name.name]
    val suitName = SERVER_TO_SUIT[suit.name]
    if (rank == null || suitName == null) {
        throw IllegalArgumentException("protocol: unknown card ${name.name}/${suit.name}")
    }
    return CardModel(cardId(suitName, rank), suitName, rank)
}

fun encodeCard(card: CardModel): Prefab = Prefab(
    "card",
    listOf(
        sym(RANK_TO_NAME.getValue(card.rank)),
        SNum(RANK_TO_NUM.getValue(card.rank)),
        sym(SUIT_TO_SERVER.getValue(card.suit)),
        SNum(RANK_TO_POINTS.getValue(card.rank)),
    ),
)

fun decodeSuit(expr: SExpr?): Suit? = (expr as? Sym)?.let { SERVER_TO_SUIT[it.name] }

// --- server events ---

sealed interface BidValue {
    data object Pass : BidValue
    data class Amount(val value: Int) : BidValue
}

data class RoomInfo(
    val host: String,
    val name: String,
    val members: List<String>,
    val target: Int, // game points this table plays to
)

data class BidPair(val value: Int, val seat: Int)

data class TrickPlay(val seat: Int, val card: CardModel)

data class LeaderboardRow(val name: String, val played: Int, val won: Int)

enum class SnapshotStage(val id: String) {
    BIDDING("bidding"),
    CHOOSING_TRUMP("choosing-trump"),
    PLAYING("playing"),
    BETWEEN_HANDS("between-hands"),
}

enum class Team(val id: String) { EVENS("evens"), ODDS("odds") }

// Mid-match view for a reconnecting client, straight from the server's
// shadow of the running game. Seats are *server* indices throughout.
data class MatchSnapshot(
    val room: String,
    val members: List<String>,
    val yourSeat: Int,
    val handNumber: Int,
    val evens: Int,
    val odds: Int,
    val target: Int,
    val stage: SnapshotStage,
    val highBid: BidPair?,
    val bidResult: BidPair?,
    val trumpSuit: Suit?, // only revealed to the bidder pre-exposure
    val trumpExposed: Boolean,
    val yourHand: List<CardModel>,
    val trickLeader: Int,
    val trick: List<TrickPlay>,
    val points: Map<Int, Int>, // seat → card points taken
    val tricks: Map<Int, Int>, // seat → tricks taken
    val lastResult: ServerEvent?, // hand-result, between hands
    val awaiting: Int?, // whose answer the server wants
    val request: ServerEvent?, // the pending request, when it is ours
)

sealed interface ServerEvent {
    data object RoomCreated : ServerEvent
    data object RoomJoined : ServerEvent
    data object RoomLeft : ServerEvent
    data object RoomClosed : ServerEvent
    data object RemovedFromRoom : ServerEvent
    data class ActiveRooms(val rooms: List<RoomInfo>) : ServerEvent
    data class StartGameFailed(val reason: String) : ServerEvent
    data class RoomMembers(
        val members: List<String>,
        val target: Int? = null, // what the table plays to, when broadcast
        // who sits where, in the seat order the match will use: null is an empty seat
        val seats: List<String?>? = null,
    ) : ServerEvent
    data object GameStarted : ServerEvent
    data object GameAborted : ServerEvent
    data class HandDealt(val cards: List<CardModel>) : ServerEvent
    data class Turn(val playerIndex: Int) : ServerEvent
    data class BidRequested(val minBid: Int) : ServerEvent
    data class BidPlaced(val playerIndex: Int, val bid: BidValue) : ServerEvent
    data class BidResult(val bid: Int, val playerIndex: Int) : ServerEvent
    data object ChooseTrumpRequested : ServerEvent
    data object TrumpSelected : ServerEvent
    data class CardPlayed(val playerIndex: Int, val card: CardModel) : ServerEvent
    data class TrumpExposed(val playerIndex: Int, val suit: Suit) : ServerEvent
    data class PlayRequested(
        val cardsPlayed: List<CardModel>,
        val trumpSuit: Suit?,
        val firstSuit: Suit?,
    ) : ServerEvent
    data class PointsWon(val points: Map<Int, Int>) : ServerEvent

    // game points after a hand; bidder is a *server* seat index and
    // evens/odds are the server-side teams (seat parity)
    data class HandResult(
        val hand: Int,
        val bidder: Int,
        val bid: Int,
        val made: Boolean,
        val delta: Int,
        val evens: Int,
        val odds: Int,
        val target: Int,
    ) : ServerEvent
    data class MatchOver(val winner: Team, val evens: Int, val odds: Int, val hands: Int) : ServerEvent
    data class PlayerDisconnected(val email: String, val graceSeconds: Int) : ServerEvent
    data class PlayerReconnected(val email: String) : ServerEvent
    data class EmotePlayed(val seat: Int, val emote: String) : ServerEvent
    data class SeatAbandoned(val seat: Int, val email: String) : ServerEvent
    data class SeatReplaced(
        val seat: Int,
        val email: String, // the bot now in the seat
        val members: List<String>, // full seat list, post-replacement
    ) : ServerEvent
    data class GameSnapshot(val snapshot: MatchSnapshot) : ServerEvent
    data object NoRunningGame : ServerEvent
    data class Leaderboard(val rows: List<LeaderboardRow>) : ServerEvent
    data class ServerError(val message: String) : ServerEvent

    // Answers to client-hello. Not in the web client, which never says hello.
    data class ProtocolOk(val version: Int) : ServerEvent
    data class UpdateRequired(val minVersion: Int) : ServerEvent

    data object Disconnected : ServerEvent
    data class Ignored(val raw: String) : ServerEvent
}

private fun memberName(expr: SExpr?): String = when (expr) {
    is SStr -> expr.value
    is Sym -> expr.name
    else -> throw IllegalArgumentException(
        "protocol: unexpected room member ${expr?.let { writeSExpr(it) }}"
    )
}

private fun isName(expr: SExpr?): Boolean = expr is SStr || expr is Sym

// `typeof x === "number"` for the integers this protocol carries.
private fun asInt(expr: SExpr?): Int? {
    val value = (expr as? SNum)?.value ?: return null
    return if (value == floor(value) && value >= Int.MIN_VALUE && value <= Int.MAX_VALUE) value.toInt() else null
}

fun decodeServerEvent(frame: String): ServerEvent? {
    val trimmed = frame.trim { isJsSpace(it) }
    if (trimmed == "" || trimmed == "#<void>") return null

    val expr = try {
        parseSExpr(trimmed)
    } catch (_: Exception) {
        return ServerEvent.Ignored(trimmed)
    }
    // Malformed cards and members throw. On the web that throw escapes the
    // socket's message handler and just loses the frame; here it must not
    // kill the event loop, so the frame is Ignored, with the same effect.
    return try {
        decodeServerExpr(expr, trimmed)
    } catch (_: IllegalArgumentException) {
        ServerEvent.Ignored(trimmed)
    }
}

// Also used to decode messages embedded inside a game-snapshot (the pending
// request, the between-hands result).
fun decodeServerExpr(expr: SExpr, trimmed: String = writeSExpr(expr)): ServerEvent? {
    if (isSym(expr, "room-created")) return ServerEvent.RoomCreated
    if (isSym(expr, "room-joined")) return ServerEvent.RoomJoined
    if (isSym(expr, "room-left")) return ServerEvent.RoomLeft
    val ignored = ServerEvent.Ignored(trimmed)
    if (expr !is SList || expr.items.isEmpty()) return ignored
    val head = expr.items[0] as? Sym ?: return ignored
    val rest = expr.items.drop(1)

    when (head.name) {
        "room-members" -> {
            val members = rest.getOrNull(1) as? SList ?: return ignored
            val seats = (rest.getOrNull(3) as? SList)?.items?.map { seat -> if ((seat as? SStr)?.value == "") null else memberName(seat) }
            return ServerEvent.RoomMembers(members.items.map(::memberName), asInt(rest.getOrNull(2)), seats)
        }

        "game-started" -> return ServerEvent.GameStarted

        "game-aborted" -> return ServerEvent.GameAborted

        "room-closed" -> return ServerEvent.RoomClosed

        "removed-from-room" -> return ServerEvent.RemovedFromRoom

        "start-game-failed" ->
            return ServerEvent.StartGameFailed((rest.getOrNull(0) as? Sym)?.name ?: "unknown")

        "active-rooms" -> {
            val roomList = rest.getOrNull(0) as? SList ?: return ignored
            val rooms = roomList.items.mapNotNull { entry ->
                val host = assocValue(entry, "host")
                val name = assocValue(entry, "name")
                val members = assocValue(entry, "members")
                if (!isName(name)) return@mapNotNull null
                RoomInfo(
                    host = if (isName(host)) memberName(host) else "",
                    name = memberName(name),
                    members = (members as? SList)?.items?.map(::memberName) ?: emptyList(),
                    target = asInt(assocValue(entry, "target")) ?: 6,
                )
            }
            return ServerEvent.ActiveRooms(rooms)
        }

        "hand" -> {
            val cards = rest.getOrNull(1) as? SList ?: return ignored
            return ServerEvent.HandDealt(cards.items.map(::decodeCard))
        }

        "turn" -> return ServerEvent.Turn(asInt(rest.getOrNull(0)) ?: return ignored)

        // the server sends the minimum acceptable bid (16 to open, else high+1)
        "request-bid" -> return ServerEvent.BidRequested(asInt(rest.getOrNull(0)) ?: return ignored)

        "bid-placed" -> {
            val playerIndex = asInt(rest.getOrNull(0)) ?: return ignored
            val bid = rest.getOrNull(1)
            return ServerEvent.BidPlaced(
                playerIndex,
                if (isSym(bid, "pass")) BidValue.Pass else BidValue.Amount(asInt(bid) ?: return ignored),
            )
        }

        "bid-result" -> {
            val result = rest.getOrNull(0) as? SPair ?: return ignored
            return ServerEvent.BidResult(
                bid = asInt(result.car) ?: return ignored,
                playerIndex = asInt(result.cdr) ?: return ignored,
            )
        }

        "choose-trump" -> return ServerEvent.ChooseTrumpRequested

        "trump-selected" -> return ServerEvent.TrumpSelected

        "played" -> {
            val playerIndex = asInt(rest.getOrNull(0)) ?: return ignored
            val card = rest.getOrNull(1)
            if (isSym(card, "expose-trump")) {
                val suit = decodeSuit(rest.getOrNull(2)) ?: return ignored
                return ServerEvent.TrumpExposed(playerIndex, suit)
            }
            return ServerEvent.CardPlayed(playerIndex, decodeCard(card ?: return ignored))
        }

        "play-card" -> {
            val cardsPlayed = assocValue(expr, "cards-played")
            val gameState = assocValue(expr, "game-state")
            return ServerEvent.PlayRequested(
                cardsPlayed = (cardsPlayed as? SList)?.items?.map(::decodeCard) ?: emptyList(),
                trumpSuit = decodeSuit(assocValue(gameState, "trump-suit")),
                firstSuit = decodeSuit(assocValue(gameState, "first-suit")),
            )
        }

        "points-won" -> {
            val hash = rest.getOrNull(0) as? SHash ?: return ignored
            val points = LinkedHashMap<Int, Int>()
            for ((k, v) in hash.entries) {
                points[asInt(k) ?: return ignored] = asInt(v) ?: return ignored
            }
            return ServerEvent.PointsWon(points)
        }

        "hand-result" -> {
            val body = rest.getOrNull(0)
            fun num(key: String) = asInt(assocValue(body, key))
            return ServerEvent.HandResult(
                hand = num("hand") ?: return ignored,
                bidder = num("bidder") ?: return ignored,
                bid = num("bid") ?: return ignored,
                made = assocValue(body, "made") == SBool(true),
                delta = num("delta") ?: return ignored,
                evens = num("evens") ?: return ignored,
                odds = num("odds") ?: return ignored,
                target = num("target") ?: return ignored,
            )
        }

        "player-disconnected" -> {
            val email = rest.getOrNull(0)
            if (!isName(email)) return ignored
            return ServerEvent.PlayerDisconnected(memberName(email), asInt(rest.getOrNull(1)) ?: 0)
        }

        "player-reconnected" -> {
            val email = rest.getOrNull(0)
            if (!isName(email)) return ignored
            return ServerEvent.PlayerReconnected(memberName(email))
        }

        "no-running-game" -> return ServerEvent.NoRunningGame

        "leaderboard" -> {
            val body = rest.getOrNull(0) as? SList ?: return ignored
            val rows = body.items.mapNotNull { entry ->
                if (entry !is SList || entry.items.size < 3) return@mapNotNull null
                val name = entry.items[0]
                val played = asInt(entry.items[1])
                val won = asInt(entry.items[2])
                if (!isName(name) || played == null || won == null) return@mapNotNull null
                LeaderboardRow(memberName(name), played, won)
            }
            return ServerEvent.Leaderboard(rows)
        }

        "emote-played" -> {
            val seat = asInt(rest.getOrNull(0)) ?: return ignored
            val emote = rest.getOrNull(1) as? Sym ?: return ignored
            return ServerEvent.EmotePlayed(seat, emote.name)
        }

        "seat-abandoned" -> {
            val seat = asInt(rest.getOrNull(0)) ?: return ignored
            val email = rest.getOrNull(1)
            if (!isName(email)) return ignored
            return ServerEvent.SeatAbandoned(seat, memberName(email))
        }

        "seat-replaced" -> {
            val seat = asInt(rest.getOrNull(0)) ?: return ignored
            val email = rest.getOrNull(1)
            val members = rest.getOrNull(2) as? SList ?: return ignored
            if (!isName(email)) return ignored
            return ServerEvent.SeatReplaced(seat, memberName(email), members.items.map(::memberName))
        }

        "game-snapshot" -> {
            val snapshot = decodeSnapshot(rest.getOrNull(0)) ?: return ignored
            return ServerEvent.GameSnapshot(snapshot)
        }

        "match-over" -> {
            val body = rest.getOrNull(0)
            val winner = (assocValue(body, "winner") as? Sym)?.name
            return ServerEvent.MatchOver(
                winner = Team.entries.firstOrNull { it.id == winner } ?: return ignored,
                evens = asInt(assocValue(body, "evens")) ?: return ignored,
                odds = asInt(assocValue(body, "odds")) ?: return ignored,
                hands = asInt(assocValue(body, "hands")) ?: return ignored,
            )
        }

        "error" -> return ServerEvent.ServerError(writeSExpr(rest.getOrNull(0) ?: sym("unknown")))

        "protocol-ok" -> return ServerEvent.ProtocolOk(asInt(rest.getOrNull(0)) ?: return ignored)

        "update-required" -> return ServerEvent.UpdateRequired(asInt(rest.getOrNull(0)) ?: return ignored)

        else -> return ignored
    }
}

// (17 . 1) → BidPair; anything else → null
private fun decodeBidPair(value: SExpr?): BidPair? {
    if (value !is SPair) return null
    return BidPair(asInt(value.car) ?: return null, asInt(value.cdr) ?: return null)
}

// ((0 . 5) (1 . 0) …) → Map seat → number
private fun decodeSeatMap(value: SExpr?): Map<Int, Int> {
    val map = LinkedHashMap<Int, Int>()
    if (value is SList) {
        for (entry in value.items) {
            if (entry is SPair) {
                val seat = asInt(entry.car)
                val number = asInt(entry.cdr)
                if (seat != null && number != null) map[seat] = number
            }
        }
    }
    return map
}

private fun decodeSnapshot(body: SExpr?): MatchSnapshot? {
    if (body !is SList) return null
    fun num(key: String) = asInt(assocValue(body, key))
    val room = assocValue(body, "room")
    val members = assocValue(body, "members") as? SList ?: return null
    val stageName = (assocValue(body, "stage") as? Sym)?.name ?: return null
    val yourSeat = num("your-seat") ?: return null
    val handNumber = num("hand-number") ?: return null
    val evens = num("evens") ?: return null
    val odds = num("odds") ?: return null
    val target = num("target") ?: return null
    val trickLeader = num("trick-leader") ?: return null
    if (!isName(room)) return null
    val stage = SnapshotStage.entries.firstOrNull { it.id == stageName } ?: return null

    val yourHand = (assocValue(body, "your-hand") as? SList)?.items?.map(::decodeCard) ?: emptyList()

    val trick = mutableListOf<TrickPlay>()
    (assocValue(body, "trick") as? SList)?.items?.forEach { play ->
        val seat = (play as? SPair)?.let { asInt(it.car) }
        if (seat != null) trick.add(TrickPlay(seat, decodeCard(play.cdr)))
    }

    val lastResultRaw = assocValue(body, "last-result")
    val lastResult = if (lastResultRaw is SList) {
        decodeServerExpr(SList(listOf(sym("hand-result"), lastResultRaw)))
    } else {
        null
    }

    val requestRaw = assocValue(body, "request")
    val request = if (requestRaw is SList) decodeServerExpr(requestRaw) else null

    return MatchSnapshot(
        room = memberName(room),
        members = members.items.map(::memberName),
        yourSeat = yourSeat,
        handNumber = handNumber,
        evens = evens,
        odds = odds,
        target = target,
        stage = stage,
        highBid = decodeBidPair(assocValue(body, "high-bid")),
        bidResult = decodeBidPair(assocValue(body, "bid-result")),
        trumpSuit = decodeSuit(assocValue(body, "trump")),
        trumpExposed = assocValue(body, "trump-exposed") == SBool(true),
        yourHand = yourHand,
        trickLeader = trickLeader,
        trick = trick,
        points = decodeSeatMap(assocValue(body, "points")),
        tricks = decodeSeatMap(assocValue(body, "tricks")),
        lastResult = lastResult.takeUnless { it is ServerEvent.Ignored },
        awaiting = asInt(assocValue(body, "awaiting")),
        request = request.takeUnless { it is ServerEvent.Ignored },
    )
}

// --- client commands ---

sealed interface ClientCommand {
    data class ClientHello(val platform: String, val version: Int) : ClientCommand
    data class ConnectUser(val email: String, val picUrl: String) : ClientCommand
    data class MakeRoom(val hostEmail: String, val roomName: String, val target: Int) : ClientCommand
    data class JoinRoom(val roomName: String, val email: String) : ClientCommand
    data class LeaveRoom(val roomName: String, val email: String) : ClientCommand
    data class KickFromRoom(val roomName: String, val hostEmail: String, val targetEmail: String) : ClientCommand
    data object GetActiveRooms : ClientCommand
    data object Ping : ClientCommand
    data class AddBot(val roomName: String, val seat: Int? = null) : ClientCommand
    data class StartGame(val roomName: String) : ClientCommand
    data class PutBid(val email: String, val bid: BidValue) : ClientCommand
    data class SelectTrump(val email: String, val suit: Suit) : ClientCommand
    data class PlayCard(val email: String, val card: CardModel) : ClientCommand
    data class ExposeTrump(val email: String) : ClientCommand
    data class NextHand(val email: String) : ClientCommand
    data class LeaveGame(val email: String) : ClientCommand
    data class Rejoin(val email: String) : ClientCommand
    data class SendEmote(val email: String, val emote: String) : ClientCommand
    data class ReplaceWithBot(val email: String) : ClientCommand
    data class CloseGame(val email: String) : ClientCommand
    data object GetLeaderboard : ClientCommand
    /** The Android app's diagnostics: a JSON report the server keeps for fixing problems (privacy policy). */
    data class Diagnostics(val report: String) : ClientCommand
}

private fun message(head: String, vararg args: SExpr): String =
    writeSExpr(SList(listOf<SExpr>(sym(head)) + args))

fun encodeCommand(command: ClientCommand): String = when (command) {
    is ClientCommand.ClientHello -> message("client-hello", SStr(command.platform), SNum(command.version))
    is ClientCommand.ConnectUser -> message("connect-user", SStr(command.email), SStr(command.picUrl))
    is ClientCommand.MakeRoom ->
        message("make-room", SStr(command.hostEmail), SStr(command.roomName), SNum(command.target))
    is ClientCommand.JoinRoom -> message("join-room", SStr(command.roomName), SStr(command.email))
    is ClientCommand.LeaveRoom -> message("leave-room", SStr(command.roomName), SStr(command.email))
    is ClientCommand.KickFromRoom ->
        message("kick-from-room", SStr(command.roomName), SStr(command.hostEmail), SStr(command.targetEmail))
    ClientCommand.GetActiveRooms -> message("get-active-rooms")
    ClientCommand.Ping -> message("ping")
    is ClientCommand.AddBot -> if (command.seat == null) {
        message("add-bot-to-room", SStr(command.roomName))
    } else {
        message("add-bot-to-room", SStr(command.roomName), SNum(command.seat.toDouble()))
    }
    is ClientCommand.StartGame -> message("start-game", SStr(command.roomName))
    is ClientCommand.PutBid -> message(
        "put-bid",
        SStr(command.email),
        when (val bid = command.bid) {
            BidValue.Pass -> sym("pass")
            is BidValue.Amount -> SNum(bid.value)
        },
    )
    is ClientCommand.SelectTrump ->
        message("selected-trump", SStr(command.email), sym(SUIT_TO_SERVER.getValue(command.suit)))
    is ClientCommand.PlayCard -> message("card-played", SStr(command.email), encodeCard(command.card))
    is ClientCommand.ExposeTrump -> message("card-played", SStr(command.email), sym("expose-trump"))
    is ClientCommand.NextHand -> message("next-hand", SStr(command.email))
    is ClientCommand.LeaveGame -> message("leave-game", SStr(command.email))
    is ClientCommand.Rejoin -> message("rejoin", SStr(command.email))
    is ClientCommand.SendEmote -> message("emote", SStr(command.email), sym(command.emote))
    is ClientCommand.ReplaceWithBot -> message("replace-with-bot", SStr(command.email))
    is ClientCommand.CloseGame -> message("close-game", SStr(command.email))
    ClientCommand.GetLeaderboard -> message("get-leaderboard")
    is ClientCommand.Diagnostics -> message("diagnostics", SStr(command.report))
}
