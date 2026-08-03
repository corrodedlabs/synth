import { describe, expect, it } from "vitest";
import type { CardModel } from "./GameModel";
import { arrangedCardIds } from "./CardArrangement";

const cards: readonly CardModel[] = [
  { id: "hearts-7", suit: "hearts", rank: "7" },
  { id: "spades-J", suit: "spades", rank: "J" },
  { id: "diamonds-9", suit: "diamonds", rank: "9" },
  { id: "clubs-A", suit: "clubs", rank: "A" },
  { id: "hearts-J", suit: "hearts", rank: "J" },
  { id: "spades-7", suit: "spades", rank: "7" },
];

describe("card arrangement presets", () => {
  it("groups alternating-color suits and uses trick strength inside each suit", () => {
    expect(arrangedCardIds(cards, "suit")).toEqual([
      "spades-J",
      "spades-7",
      "hearts-J",
      "hearts-7",
      "clubs-A",
      "diamonds-9",
    ]);
  });

  it("orders all cards by trick strength", () => {
    expect(arrangedCardIds(cards, "strength")).toEqual([
      "spades-J",
      "hearts-J",
      "diamonds-9",
      "clubs-A",
      "spades-7",
      "hearts-7",
    ]);
  });

  it("places a known trump suit first", () => {
    expect(arrangedCardIds(cards, "trump", { trumpSuit: "hearts" })).toEqual([
      "hearts-J",
      "hearts-7",
      "spades-J",
      "spades-7",
      "clubs-A",
      "diamonds-9",
    ]);
  });

  it("falls back to ordinary suit groups while trump is unknown", () => {
    expect(arrangedCardIds(cards, "trump", { trumpSuit: null })).toEqual(
      arrangedCardIds(cards, "suit")
    );
  });

  it("restores the surviving cards to their original deal order", () => {
    const remaining = cards.filter((card) => card.id !== "spades-J");
    expect(
      arrangedCardIds(remaining, "deal", {
        dealOrder: [
          "clubs-A",
          "spades-J",
          "hearts-7",
          "diamonds-9",
          "hearts-J",
          "spades-7",
        ],
      })
    ).toEqual(["clubs-A", "hearts-7", "diamonds-9", "hearts-J", "spades-7"]);
  });
});

