// Port of app/src/game/CardArrangement.ts. Keep the two in step.
package dev.synth.core

enum class ArrangementPreset { SUIT, STRENGTH, TRUMP, DEAL }

enum class ArrangementMode { SUIT, STRENGTH, TRUMP, DEAL, CUSTOM }

// Alternating black and red groups makes the boundary between neighbouring
// suits readable even in the overlapped fan.
private val SUIT_ORDER = listOf(Suit.SPADES, Suit.HEARTS, Suit.CLUBS, Suit.DIAMONDS)

// Returns card ids instead of mutating the reducer's authoritative hand.
// The original input index is the final tie-breaker, keeping every preset
// deterministic and stable.
fun arrangedCardIds(
    cards: List<CardModel>,
    preset: ArrangementPreset,
    trumpSuit: Suit? = null,
    dealOrder: List<String>? = null,
): List<String> {
    val original = cards.withIndex().associate { (index, card) -> card.id to index }
    val deal = (dealOrder ?: cards.map { it.id }).withIndex().associate { (index, id) -> id to index }
    val suitOrder =
        if (preset == ArrangementPreset.TRUMP && trumpSuit != null) {
            listOf(trumpSuit) + SUIT_ORDER.filter { it != trumpSuit }
        } else {
            SUIT_ORDER
        }

    val byDeal = compareBy<CardModel> { deal[it.id] ?: Int.MAX_VALUE }.thenBy { original[it.id] ?: 0 }
    val byRank = compareBy<CardModel> { it.rank.ordinal }
    val bySuit = compareBy<CardModel> { suitOrder.indexOf(it.suit) }
    val byOriginal = compareBy<CardModel> { original[it.id] ?: 0 }

    val comparator = when (preset) {
        ArrangementPreset.DEAL -> byDeal
        ArrangementPreset.STRENGTH -> byRank.then(bySuit).then(byOriginal)
        ArrangementPreset.SUIT, ArrangementPreset.TRUMP -> bySuit.then(byRank).then(byOriginal)
    }
    return cards.sortedWith(comparator).map { it.id }
}
