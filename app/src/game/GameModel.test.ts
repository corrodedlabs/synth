import { describe, expect, it } from "vitest";
import {
  CardModel,
  GameModel,
  canExposeTrump,
  gameReducer,
  initialGameModel,
  isLegalPlay,
} from "./GameModel";

const card = (suit: CardModel["suit"], rank: CardModel["rank"]): CardModel => ({
  id: `${suit}-${rank}`,
  suit,
  rank,
});

const h8 = card("hearts", "8");
const hK = card("hearts", "K");
const c7 = card("clubs", "7");
const cJ = card("clubs", "J");
const s9 = card("spades", "9");

function playing(model: Partial<GameModel>): GameModel {
  return { ...initialGameModel, phase: "playing", ...model };
}

describe("isLegalPlay", () => {
  it("rejects everything when the server is not waiting on us", () => {
    const model = playing({ hand: [h8], pendingRequest: null });
    expect(isLegalPlay(model, h8)).toBe(false);
  });

  it("allows any card when leading", () => {
    const model = playing({
      hand: [h8, c7],
      pendingRequest: { kind: "play", firstSuit: null, trumpSuit: null },
    });
    expect(isLegalPlay(model, h8)).toBe(true);
    expect(isLegalPlay(model, c7)).toBe(true);
  });

  it("requires following suit while we hold the suit led", () => {
    const model = playing({
      hand: [hK, c7],
      pendingRequest: { kind: "play", firstSuit: "hearts", trumpSuit: null },
    });
    expect(isLegalPlay(model, hK)).toBe(true);
    expect(isLegalPlay(model, c7)).toBe(false);
  });

  it("never lets a trump excuse breaking suit", () => {
    const model = playing({
      hand: [hK, cJ],
      pendingRequest: { kind: "play", firstSuit: "hearts", trumpSuit: "clubs" },
    });
    expect(isLegalPlay(model, cJ)).toBe(false);
    expect(isLegalPlay(model, hK)).toBe(true);
  });

  it("allows anything once void in the suit led", () => {
    const model = playing({
      hand: [c7, s9],
      pendingRequest: { kind: "play", firstSuit: "hearts", trumpSuit: null },
    });
    expect(isLegalPlay(model, c7)).toBe(true);
    expect(isLegalPlay(model, s9)).toBe(true);
  });

  it("forces a trump right after we called for the exposure", () => {
    const model = playing({
      hand: [c7, s9],
      pendingRequest: { kind: "play", firstSuit: "hearts", trumpSuit: "clubs", mustTrump: true },
    });
    expect(isLegalPlay(model, c7)).toBe(true);
    expect(isLegalPlay(model, s9)).toBe(false);
  });

  it("lets the exposer discard when they hold no trump", () => {
    const model = playing({
      hand: [s9],
      pendingRequest: { kind: "play", firstSuit: "hearts", trumpSuit: "clubs", mustTrump: true },
    });
    expect(isLegalPlay(model, s9)).toBe(true);
  });
});

describe("canExposeTrump", () => {
  it("is offered only when we cannot follow suit and trump is hidden", () => {
    const base = playing({
      hand: [c7, s9],
      pendingRequest: { kind: "play", firstSuit: "hearts", trumpSuit: null },
    });
    expect(canExposeTrump(base)).toBe(true);
    expect(canExposeTrump({ ...base, hand: [hK, c7] })).toBe(false); // can follow
    expect(canExposeTrump({ ...base, trumpExposed: true })).toBe(false);
    expect(
      canExposeTrump(
        playing({
          hand: [c7],
          pendingRequest: { kind: "play", firstSuit: null, trumpSuit: null },
        })
      )
    ).toBe(false); // leaders cannot call
  });
});

describe("gameReducer", () => {
  it("counts bids and passes through the auction", () => {
    let model = initialGameModel;
    model = gameReducer(model, { _tag: "BidPlaced", playerIndex: 1, bid: 17 });
    model = gameReducer(model, { _tag: "BidPlaced", playerIndex: 2, bid: "pass" });
    expect(model.bidsPlaced).toBe(2);
    expect(model.currentBid).toBe(17);
  });

  it("clears exactly one trick on TrickEnded, keeping an early next lead", () => {
    let model = playing({
      playedCards: [
        { card: h8, playerIndex: 1 },
        { card: hK, playerIndex: 2 },
        { card: s9, playerIndex: 3 },
        { card: c7, playerIndex: 0 },
        { card: cJ, playerIndex: 0 }, // already led to the next trick
      ],
    });
    model = gameReducer(model, { _tag: "TrickEnded", winner: 0, points: 2 });
    expect(model.playedCards).toEqual([{ card: cJ, playerIndex: 0 }]);
    expect(model.score).toBe(2);
    expect(model.tricks).toBe(1);
  });

  it("attributes trick points to the right team", () => {
    let model = playing({});
    model = gameReducer(model, { _tag: "TrickEnded", winner: 3, points: 5 });
    expect(model.score).toBe(0);
    expect(model.theirScore).toBe(5);
    expect(model.theirTricks).toBe(1);
  });

  it("resets everything when leaving a room mid-game", () => {
    const midGame = playing({
      hand: [h8],
      playedCards: [{ card: c7, playerIndex: 2 }],
      score: 9,
      trumpSuit: "clubs",
      trumpExposed: true,
      seatNames: ["a", "b", "c", "d"],
    });
    expect(gameReducer(midGame, { _tag: "RoomLeft" })).toEqual(initialGameModel);
  });

  it("totals team points when the game finishes", () => {
    const model = gameReducer(playing({}), {
      _tag: "GameFinished",
      points: [10, 6, 9, 3],
    });
    expect(model.phase).toBe("finished");
    expect(model.score).toBe(19);
    expect(model.theirScore).toBe(9);
  });
});
