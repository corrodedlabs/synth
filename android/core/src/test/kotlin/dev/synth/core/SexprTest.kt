package dev.synth.core

import kotlin.test.Test
import kotlin.test.assertEquals

// Port of app/src/net/sexpr.test.ts, case for case.

// Builds the SExpr for a test the way the TS tests write array literals.
internal fun sx(value: Any): SExpr = when (value) {
    is SExpr -> value
    is Int -> SNum(value)
    is String -> SStr(value)
    is Boolean -> SBool(value)
    is List<*> -> SList(value.map { sx(it!!) })
    else -> error("no sexpr form for $value")
}

class SexprTest {
    private fun prefabCard(name: String, num: Int, suit: String, points: Int) =
        Prefab("card", listOf(sym(name), SNum(num), sym(suit), SNum(points)))

    @Test
    fun `parses atoms`() {
        assertEquals(SNum(42), parseSExpr("42"))
        assertEquals(SNum(-3), parseSExpr("-3"))
        assertEquals(SStr("hello"), parseSExpr("\"hello\""))
        assertEquals(SBool(true), parseSExpr("#t"))
        assertEquals(SBool(false), parseSExpr("#f"))
        assertEquals(sym("pass"), parseSExpr("pass"))
    }

    @Test
    fun `parses proper lists`() {
        assertEquals(sx(listOf(sym("request-bid"), 16)), parseSExpr("(request-bid 16)"))
        assertEquals(sx(listOf(sym("turn"), 2)), parseSExpr("(turn 2)"))
    }

    @Test
    fun `parses dotted pairs`() {
        assertEquals(
            sx(listOf(sym("bid-result"), SPair(SNum(17), SNum(1)))),
            parseSExpr("(bid-result (17 . 1))"),
        )
    }

    @Test
    fun `parses prefab card structs`() {
        assertEquals(prefabCard("jack", 7, "club", 3), parseSExpr("#s(card jack 7 club 3)"))
    }

    @Test
    fun `parses a hand message`() {
        assertEquals(
            sx(listOf(sym("hand"), "me@x", listOf(prefabCard("jack", 7, "club", 3), prefabCard("seven", 0, "heart", 0)))),
            parseSExpr("(hand \"me@x\" (#s(card jack 7 club 3) #s(card seven 0 heart 0)))"),
        )
    }

    @Test
    fun `parses the play-card request shape`() {
        val result = parseSExpr(
            "(play-card (cards-played #s(card ace 4 spade 1)) (game-state (trump-suit . #f) (first-suit . spade)))"
        ) as SList
        assertEquals(sym("play-card"), result.items[0])
        assertEquals(sx(listOf(prefabCard("ace", 4, "spade", 1))), assocValue(result, "cards-played"))
        val gameState = assocValue(result, "game-state")
        assertEquals(SBool(false), assocValue(gameState, "trump-suit"))
        assertEquals(sym("spade"), assocValue(gameState, "first-suit"))
    }

    @Test
    fun `parses empty cards-played`() {
        val result = parseSExpr("(play-card (cards-played) (game-state (trump-suit . #f) (first-suit . #f)))")
        assertEquals(SList(emptyList()), assocValue(result, "cards-played"))
    }

    @Test
    fun `parses hashes`() {
        assertEquals(
            sx(
                listOf(
                    sym("points-won"),
                    SHash(listOf(0 to 10, 1 to 10, 2 to 5, 3 to 3).map { (k, v) -> SNum(k) to SNum(v) }),
                )
            ),
            parseSExpr("(points-won #hash((0 . 10) (1 . 10) (2 . 5) (3 . 3)))"),
        )
    }

    @Test
    fun `parses room members with symbols and strings`() {
        assertEquals(
            sx(listOf(sym("room-members"), "room-1", listOf(sym("bot-3"), sym("bot-2"), sym("bot-1"), "me@x"))),
            parseSExpr("(room-members \"room-1\" (bot-3 bot-2 bot-1 \"me@x\"))"),
        )
    }

    @Test
    fun `writes commands the racket reader can parse back`() {
        assertEquals("(connect-user \"a@b\" \"pic.png\")", writeSExpr(sx(listOf(sym("connect-user"), "a@b", "pic.png"))))
        assertEquals("(put-bid \"a@b\" 17)", writeSExpr(sx(listOf(sym("put-bid"), "a@b", 17))))
        assertEquals("(put-bid \"a@b\" pass)", writeSExpr(sx(listOf(sym("put-bid"), "a@b", sym("pass")))))
        assertEquals(
            "(card-played \"a@b\" #s(card jack 7 club 3))",
            writeSExpr(sx(listOf(sym("card-played"), "a@b", prefabCard("jack", 7, "club", 3)))),
        )
    }

    @Test
    fun `round-trips`() {
        val samples = listOf(
            "(request-bid 16)",
            "(bid-result (17 . 1))",
            "#s(card ten 3 diamond 1)",
            "(hand \"x\" (#s(card nine 2 heart 2)))",
        )
        for (sample in samples) {
            assertEquals(sample, writeSExpr(parseSExpr(sample)))
        }
    }

    @Test
    fun `Sym compares structurally`() {
        assertEquals(Sym("a"), sym("a"))
    }

    // Not in the TS suite: the Kotlin-specific number writer, against what JS prints.
    @Test
    fun `writes numbers as JS does`() {
        assertEquals("1", writeSExpr(parseSExpr("1.0")))
        assertEquals("0.5", writeSExpr(parseSExpr(".5")))
        assertEquals("0.0001", writeSExpr(SNum(0.0001)))
        assertEquals("100", writeSExpr(SNum(100)))
        assertEquals("0", writeSExpr(SNum(-0.0)))
    }
}
