import { RANKS } from "./GameModel";
import type { CardModel, Suit } from "./GameModel";

export type ArrangementPreset = "suit" | "strength" | "trump" | "deal";
export type ArrangementMode = ArrangementPreset | "custom";

// Alternating black and red groups makes the boundary between neighbouring
// suits readable even in the overlapped fan.
const SUIT_ORDER: readonly Suit[] = ["spades", "hearts", "clubs", "diamonds"];
const rankIndex = new Map(RANKS.map((rank, index) => [rank, index]));

function indexes(values: readonly string[]): Map<string, number> {
  return new Map(values.map((value, index) => [value, index]));
}

function compareNumber(left: number, right: number): number {
  return left < right ? -1 : left > right ? 1 : 0;
}

// Returns card ids instead of mutating the reducer's authoritative hand.
// The original input index is the final tie-breaker, keeping every preset
// deterministic and stable.
export function arrangedCardIds(
  cards: readonly CardModel[],
  preset: ArrangementPreset,
  options: {
    readonly trumpSuit?: Suit | null;
    readonly dealOrder?: readonly string[];
  } = {}
): string[] {
  const original = indexes(cards.map((card) => card.id));
  const deal = indexes(options.dealOrder ?? cards.map((card) => card.id));
  const suitOrder =
    preset === "trump" && options.trumpSuit
      ? [options.trumpSuit, ...SUIT_ORDER.filter((suit) => suit !== options.trumpSuit)]
      : [...SUIT_ORDER];
  const suits = indexes(suitOrder);

  return [...cards]
    .sort((left, right) => {
      if (preset === "deal") {
        return (
          compareNumber(deal.get(left.id) ?? Number.MAX_SAFE_INTEGER, deal.get(right.id) ?? Number.MAX_SAFE_INTEGER) ||
          compareNumber(original.get(left.id) ?? 0, original.get(right.id) ?? 0)
        );
      }

      const byRank = compareNumber(rankIndex.get(left.rank) ?? 0, rankIndex.get(right.rank) ?? 0);
      const bySuit = compareNumber(suits.get(left.suit) ?? 0, suits.get(right.suit) ?? 0);
      const byOriginal = compareNumber(original.get(left.id) ?? 0, original.get(right.id) ?? 0);

      return preset === "strength"
        ? byRank || bySuit || byOriginal
        : bySuit || byRank || byOriginal;
    })
    .map((card) => card.id);
}

