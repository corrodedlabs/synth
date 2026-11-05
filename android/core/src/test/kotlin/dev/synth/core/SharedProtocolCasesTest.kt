package dev.synth.core

import kotlinx.serialization.json.Json
import kotlinx.serialization.json.JsonArray
import kotlinx.serialization.json.JsonElement
import kotlinx.serialization.json.JsonNull
import kotlinx.serialization.json.JsonObject
import kotlinx.serialization.json.JsonPrimitive
import kotlinx.serialization.json.buildJsonObject
import kotlinx.serialization.json.jsonArray
import kotlinx.serialization.json.jsonObject
import kotlinx.serialization.json.jsonPrimitive
import java.io.File
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertTrue

// The same cases run against the TS decoder in app/src/net/protocol.shared.test.ts.
// The file's "about" field describes the format. Gradle runs tests in android/core.
class SharedProtocolCasesTest {
    private val cases = Json.parseToJsonElement(File("../../app/test-data/protocol-cases.json").readText())
        .jsonObject.getValue("cases").jsonArray.map { it.jsonObject }

    @Test
    fun `every shared case decodes to its expected form`() {
        val failures = cases.mapNotNull { case ->
            val wire = case.getValue("wire").jsonPrimitive.content
            val expected = case.getValue("expected")
            val actual = decodeServerEvent(wire)?.let(::jsonForm) ?: JsonNull
            if (actual == expected) null else "$wire\n  expected $expected\n  actual   $actual"
        }
        assertEquals(emptyList(), failures, failures.joinToString("\n"))
    }

    @Test
    fun `the cases cover every event with a wire form`() {
        // Disconnected is raised by the socket, not sent by the server.
        val tags = ServerEvent::class.java.permittedSubclasses.map { it.simpleName }.toSet() - "Disconnected"
        assertTrue(tags.size > 30, "reflection found only $tags")
        val covered = cases.mapNotNull { (it["expected"] as? JsonObject)?.get("_tag")?.jsonPrimitive?.content }.toSet()
        assertEquals(emptySet(), tags - covered)
        assertEquals(emptySet(), covered - tags)
    }

    private fun str(value: String?) = JsonPrimitive(value)
    private fun num(value: Int?) = JsonPrimitive(value)
    private fun strings(values: List<String>) = JsonArray(values.map(::str))
    private fun seatMap(map: Map<Int, Int>) = JsonObject(map.entries.associate { (k, v) -> k.toString() to num(v) })
    private fun cards(values: List<CardModel>) = JsonArray(values.map(::cardForm))

    private fun cardForm(card: CardModel) = JsonObject(
        mapOf("id" to str(card.id), "suit" to str(card.suit.id), "rank" to str(card.rank.id))
    )

    private fun bidPair(pair: BidPair?): JsonElement =
        if (pair == null) JsonNull else JsonObject(mapOf("value" to num(pair.value), "seat" to num(pair.seat)))

    private fun tagged(tag: String, vararg fields: Pair<String, JsonElement>) = buildJsonObject {
        put("_tag", str(tag))
        for ((key, value) in fields) put(key, value)
    }

    private fun jsonForm(event: ServerEvent): JsonElement = when (event) {
        ServerEvent.RoomCreated -> tagged("RoomCreated")
        ServerEvent.RoomJoined -> tagged("RoomJoined")
        ServerEvent.RoomLeft -> tagged("RoomLeft")
        ServerEvent.RoomClosed -> tagged("RoomClosed")
        ServerEvent.RemovedFromRoom -> tagged("RemovedFromRoom")
        is ServerEvent.ActiveRooms -> tagged(
            "ActiveRooms",
            "rooms" to JsonArray(
                event.rooms.map {
                    JsonObject(
                        mapOf(
                            "host" to str(it.host),
                            "name" to str(it.name),
                            "members" to strings(it.members),
                            "target" to num(it.target),
                        )
                    )
                }
            ),
        )
        is ServerEvent.StartGameFailed -> tagged("StartGameFailed", "reason" to str(event.reason))
        is ServerEvent.RoomMembers -> {
            val members = "members" to strings(event.members)
            if (event.target == null) tagged("RoomMembers", members)
            else tagged("RoomMembers", members, "target" to num(event.target))
        }
        ServerEvent.GameStarted -> tagged("GameStarted")
        ServerEvent.GameAborted -> tagged("GameAborted")
        is ServerEvent.HandDealt -> tagged("HandDealt", "cards" to cards(event.cards))
        is ServerEvent.Turn -> tagged("Turn", "playerIndex" to num(event.playerIndex))
        is ServerEvent.BidRequested -> tagged("BidRequested", "minBid" to num(event.minBid))
        is ServerEvent.BidPlaced -> tagged(
            "BidPlaced",
            "playerIndex" to num(event.playerIndex),
            "bid" to when (val bid = event.bid) {
                BidValue.Pass -> str("pass")
                is BidValue.Amount -> num(bid.value)
            },
        )
        is ServerEvent.BidResult -> tagged("BidResult", "bid" to num(event.bid), "playerIndex" to num(event.playerIndex))
        ServerEvent.ChooseTrumpRequested -> tagged("ChooseTrumpRequested")
        ServerEvent.TrumpSelected -> tagged("TrumpSelected")
        is ServerEvent.CardPlayed -> tagged("CardPlayed", "playerIndex" to num(event.playerIndex), "card" to cardForm(event.card))
        is ServerEvent.TrumpExposed -> tagged("TrumpExposed", "playerIndex" to num(event.playerIndex), "suit" to str(event.suit.id))
        is ServerEvent.PlayRequested -> tagged(
            "PlayRequested",
            "cardsPlayed" to cards(event.cardsPlayed),
            "trumpSuit" to str(event.trumpSuit?.id),
            "firstSuit" to str(event.firstSuit?.id),
        )
        is ServerEvent.PointsWon -> tagged("PointsWon", "points" to seatMap(event.points))
        is ServerEvent.HandResult -> tagged(
            "HandResult",
            "hand" to num(event.hand),
            "bidder" to num(event.bidder),
            "bid" to num(event.bid),
            "made" to JsonPrimitive(event.made),
            "delta" to num(event.delta),
            "evens" to num(event.evens),
            "odds" to num(event.odds),
            "target" to num(event.target),
        )
        is ServerEvent.MatchOver -> tagged(
            "MatchOver",
            "winner" to str(event.winner.id),
            "evens" to num(event.evens),
            "odds" to num(event.odds),
            "hands" to num(event.hands),
        )
        is ServerEvent.PlayerDisconnected -> tagged(
            "PlayerDisconnected",
            "email" to str(event.email),
            "graceSeconds" to num(event.graceSeconds),
        )
        is ServerEvent.PlayerReconnected -> tagged("PlayerReconnected", "email" to str(event.email))
        is ServerEvent.EmotePlayed -> tagged("EmotePlayed", "seat" to num(event.seat), "emote" to str(event.emote))
        is ServerEvent.SeatAbandoned -> tagged("SeatAbandoned", "seat" to num(event.seat), "email" to str(event.email))
        is ServerEvent.SeatReplaced -> tagged(
            "SeatReplaced",
            "seat" to num(event.seat),
            "email" to str(event.email),
            "members" to strings(event.members),
        )
        is ServerEvent.GameSnapshot -> tagged("GameSnapshot", "snapshot" to snapshotForm(event.snapshot))
        ServerEvent.NoRunningGame -> tagged("NoRunningGame")
        is ServerEvent.Leaderboard -> tagged(
            "Leaderboard",
            "rows" to JsonArray(
                event.rows.map {
                    JsonObject(mapOf("name" to str(it.name), "played" to num(it.played), "won" to num(it.won)))
                }
            ),
        )
        is ServerEvent.ServerError -> tagged("ServerError", "message" to str(event.message))
        is ServerEvent.ProtocolOk -> tagged("ProtocolOk", "version" to num(event.version))
        is ServerEvent.UpdateRequired -> tagged("UpdateRequired", "minVersion" to num(event.minVersion))
        ServerEvent.Disconnected -> tagged("Disconnected")
        is ServerEvent.Ignored -> tagged("Ignored", "raw" to str(event.raw))
    }

    private fun snapshotForm(s: MatchSnapshot) = JsonObject(
        mapOf(
            "room" to str(s.room),
            "members" to strings(s.members),
            "yourSeat" to num(s.yourSeat),
            "handNumber" to num(s.handNumber),
            "evens" to num(s.evens),
            "odds" to num(s.odds),
            "target" to num(s.target),
            "stage" to str(s.stage.id),
            "highBid" to bidPair(s.highBid),
            "bidResult" to bidPair(s.bidResult),
            "trumpSuit" to str(s.trumpSuit?.id),
            "trumpExposed" to JsonPrimitive(s.trumpExposed),
            "yourHand" to cards(s.yourHand),
            "trickLeader" to num(s.trickLeader),
            "trick" to JsonArray(
                s.trick.map { JsonObject(mapOf("seat" to num(it.seat), "card" to cardForm(it.card))) }
            ),
            "points" to seatMap(s.points),
            "tricks" to seatMap(s.tricks),
            "lastResult" to (s.lastResult?.let(::jsonForm) ?: JsonNull),
            "awaiting" to num(s.awaiting),
            "request" to (s.request?.let(::jsonForm) ?: JsonNull),
        )
    )
}
