package dev.synth.core

import kotlin.test.Test
import kotlin.test.assertEquals

// Port of app/src/game/CardArrangement.test.ts, case for case.
class CardArrangementTest {
    private fun card(suit: Suit, rank: Rank) = CardModel(cardId(suit, rank), suit, rank)

    private val cards = listOf(
        card(Suit.HEARTS, Rank.SEVEN),
        card(Suit.SPADES, Rank.J),
        card(Suit.DIAMONDS, Rank.NINE),
        card(Suit.CLUBS, Rank.A),
        card(Suit.HEARTS, Rank.J),
        card(Suit.SPADES, Rank.SEVEN),
    )

    @Test
    fun `groups alternating-color suits and uses trick strength inside each suit`() {
        assertEquals(
            listOf("spades-J", "spades-7", "hearts-J", "hearts-7", "clubs-A", "diamonds-9"),
            arrangedCardIds(cards, ArrangementPreset.SUIT),
        )
    }

    @Test
    fun `orders all cards by trick strength`() {
        assertEquals(
            listOf("spades-J", "hearts-J", "diamonds-9", "clubs-A", "spades-7", "hearts-7"),
            arrangedCardIds(cards, ArrangementPreset.STRENGTH),
        )
    }

    @Test
    fun `places a known trump suit first`() {
        assertEquals(
            listOf("hearts-J", "hearts-7", "spades-J", "spades-7", "clubs-A", "diamonds-9"),
            arrangedCardIds(cards, ArrangementPreset.TRUMP, trumpSuit = Suit.HEARTS),
        )
    }

    @Test
    fun `falls back to ordinary suit groups while trump is unknown`() {
        assertEquals(
            arrangedCardIds(cards, ArrangementPreset.SUIT),
            arrangedCardIds(cards, ArrangementPreset.TRUMP, trumpSuit = null),
        )
    }

    @Test
    fun `restores the surviving cards to their original deal order`() {
        val remaining = cards.filter { it.id != "spades-J" }
        assertEquals(
            listOf("clubs-A", "hearts-7", "diamonds-9", "hearts-J", "spades-7"),
            arrangedCardIds(
                remaining,
                ArrangementPreset.DEAL,
                dealOrder = listOf("clubs-A", "spades-J", "hearts-7", "diamonds-9", "hearts-J", "spades-7"),
            ),
        )
    }
}
