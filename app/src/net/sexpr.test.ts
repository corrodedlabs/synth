import { describe, expect, it } from "vitest";
import { assocValue, Pair, parseSExpr, Prefab, SHash, Sym, sym, writeSExpr } from "./sexpr";

describe("parseSExpr", () => {
  it("parses atoms", () => {
    expect(parseSExpr("42")).toBe(42);
    expect(parseSExpr("-3")).toBe(-3);
    expect(parseSExpr('"hello"')).toBe("hello");
    expect(parseSExpr("#t")).toBe(true);
    expect(parseSExpr("#f")).toBe(false);
    expect(parseSExpr("pass")).toEqual(sym("pass"));
  });

  it("parses proper lists", () => {
    expect(parseSExpr("(request-bid 16)")).toEqual([sym("request-bid"), 16]);
    expect(parseSExpr("(turn 2)")).toEqual([sym("turn"), 2]);
  });

  it("parses dotted pairs", () => {
    const result = parseSExpr("(bid-result (17 . 1))");
    expect(result).toEqual([sym("bid-result"), new Pair(17, 1)]);
  });

  it("parses prefab card structs", () => {
    const result = parseSExpr("#s(card jack 7 club 3)");
    expect(result).toEqual(new Prefab("card", [sym("jack"), 7, sym("club"), 3]));
  });

  it("parses a hand message", () => {
    const result = parseSExpr('(hand "me@x" (#s(card jack 7 club 3) #s(card seven 0 heart 0)))');
    expect(result).toEqual([
      sym("hand"),
      "me@x",
      [
        new Prefab("card", [sym("jack"), 7, sym("club"), 3]),
        new Prefab("card", [sym("seven"), 0, sym("heart"), 0]),
      ],
    ]);
  });

  it("parses the play-card request shape", () => {
    const result = parseSExpr(
      "(play-card (cards-played #s(card ace 4 spade 1)) (game-state (trump-suit . #f) (first-suit . spade)))"
    ) as unknown[];
    expect(result[0]).toEqual(sym("play-card"));
    const cardsPlayed = assocValue(result as never, "cards-played");
    expect(cardsPlayed).toEqual([new Prefab("card", [sym("ace"), 4, sym("spade"), 1])]);
    const gameState = assocValue(result as never, "game-state");
    expect(assocValue(gameState as never, "trump-suit")).toBe(false);
    expect(assocValue(gameState as never, "first-suit")).toEqual(sym("spade"));
  });

  it("parses empty cards-played", () => {
    const result = parseSExpr(
      "(play-card (cards-played) (game-state (trump-suit . #f) (first-suit . #f)))"
    );
    expect(assocValue(result as never, "cards-played")).toEqual([]);
  });

  it("parses hashes", () => {
    const result = parseSExpr("(points-won #hash((0 . 10) (1 . 10) (2 . 5) (3 . 3)))");
    expect(result).toEqual([
      sym("points-won"),
      new SHash([
        [0, 10],
        [1, 10],
        [2, 5],
        [3, 3],
      ]),
    ]);
  });

  it("parses room members with symbols and strings", () => {
    const result = parseSExpr('(room-members "room-1" (bot-3 bot-2 bot-1 "me@x"))');
    expect(result).toEqual([sym("room-members"), "room-1", [sym("bot-3"), sym("bot-2"), sym("bot-1"), "me@x"]]);
  });
});

describe("writeSExpr", () => {
  it("writes commands the racket reader can parse back", () => {
    expect(writeSExpr([sym("connect-user"), "a@b", "pic.png"])).toBe('(connect-user "a@b" "pic.png")');
    expect(writeSExpr([sym("put-bid"), "a@b", 17])).toBe('(put-bid "a@b" 17)');
    expect(writeSExpr([sym("put-bid"), "a@b", sym("pass")])).toBe('(put-bid "a@b" pass)');
    expect(
      writeSExpr([sym("card-played"), "a@b", new Prefab("card", [sym("jack"), 7, sym("club"), 3])])
    ).toBe('(card-played "a@b" #s(card jack 7 club 3))');
  });

  it("round-trips", () => {
    const samples = [
      "(request-bid 16)",
      "(bid-result (17 . 1))",
      "#s(card ten 3 diamond 1)",
      '(hand "x" (#s(card nine 2 heart 2)))',
    ];
    for (const sample of samples) {
      expect(writeSExpr(parseSExpr(sample))).toBe(sample);
    }
  });
});

describe("Sym", () => {
  it("compares structurally", () => {
    expect(new Sym("a")).toEqual(sym("a"));
  });
});
