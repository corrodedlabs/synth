import { describe, expect, it } from "vitest";
import {
  bidderDelta,
  bidOwnerLabel,
  handResultCopy,
  lineFraction,
  lobbySeats,
  pipRow,
  progressLabel,
  scoreText,
  signedText,
} from "./HudModel";

describe("pipRow", () => {
  it("draws one pip per point to the target", () => {
    expect(pipRow(2, 6)).toEqual({ total: 6, filled: 2, number: "2" });
    expect(pipRow(1, 2)).toEqual({ total: 2, filled: 1, number: "1" });
    expect(pipRow(6, 6)).toEqual({ total: 6, filled: 6, number: "6" });
  });

  it("shows the number too when a score is negative", () => {
    expect(pipRow(-2, 6)).toEqual({ total: 6, filled: 0, number: "−2" });
    expect(pipRow(-4, 2)).toEqual({ total: 2, filled: 0, number: "−4" });
  });

  it("copes with a score past the target and with odd targets", () => {
    expect(pipRow(8, 6)).toEqual({ total: 6, filled: 6, number: "8" });
    expect(pipRow(0, 0).total).toBe(1);
    const long = pipRow(10, 40);
    expect(long.total).toBe(12);
    expect(long.filled).toBe(3);
    expect(long.number).toBe("10");
  });
});

describe("result wording", () => {
  it("signs points with a real minus", () => {
    expect(signedText(1)).toBe("+1");
    expect(signedText(-2)).toBe("−2");
    expect(scoreText(-2)).toBe("−2");
    expect(scoreText(3)).toBe("3");
  });

  it("turns our-side swings back into the bidders' change", () => {
    expect(bidderDelta(1, true)).toBe(1);
    expect(bidderDelta(-2, true)).toBe(-2);
    // their bid made: their score rose, the swing from our side is −1
    expect(bidderDelta(-1, false)).toBe(1);
    // their bid set: their score fell by 2, the swing from our side is +2
    expect(bidderDelta(2, false)).toBe(-2);
    expect(bidderDelta(0, false)).toBeNull();
  });

  it("says who the points go to", () => {
    const base = { bidderName: "You", bid: 18, took: 19, made: true };
    expect(handResultCopy({ ...base, delta: 1, bidderIsOurs: true })).toEqual({
      seal: "MADE",
      detail: "You bid 18 and took 19",
      points: "+1",
    });
    expect(
      handResultCopy({ ...base, took: 15, made: false, delta: -2, bidderIsOurs: true }).points
    ).toBe("−2");
    expect(
      handResultCopy({ ...base, bidderName: "Mira", delta: 1, bidderIsOurs: false })
    ).toEqual({ seal: "MADE", detail: "Mira bid 18 and took 19", points: "+1 to them" });
    expect(
      handResultCopy({ ...base, bidderName: "Mira", made: false, delta: -2, bidderIsOurs: false })
        .points
    ).toBe("−2 to them");
    expect(handResultCopy({ ...base, delta: null, bidderIsOurs: true }).points).toBeNull();
  });
});

describe("hand progress", () => {
  it("keeps the line on the table's 28 points", () => {
    expect(lineFraction(0)).toBe(0);
    expect(lineFraction(14)).toBe(0.5);
    expect(lineFraction(40)).toBe(1);
    expect(lineFraction(-3)).toBe(0);
  });

  it("speaks to the bid only when it is ours", () => {
    expect(progressLabel(9, 3, null, false)).toBe("");
    expect(progressLabel(9, 3, 18, true)).toBe("9 of 18");
    expect(progressLabel(19, 3, 18, true)).toBe("Bid made with 19");
    expect(progressLabel(9, 3, 18, false)).toBe("You 9 · Them 3");
  });

  it("names the owner of the bid", () => {
    expect(bidOwnerLabel(null, null)).toBe("");
    expect(bidOwnerLabel(0, "You")).toBe("your bid");
    expect(bidOwnerLabel(1, "Mira")).toBe("Mira's bid");
    expect(bidOwnerLabel(1, null)).toBe("their bid");
  });
});

describe("lobbySeats", () => {
  it("uses the server's seats when it has sent them, from our side of the table", () => {
    const seats = lobbySeats(["bot", "host"], "host", ["host", null, null, "bot"]);
    expect(seats.map((seat) => seat.member)).toEqual(["host", null, null, "bot"]);
    expect(seats.map((seat) => seat.seat)).toEqual([0, 1, 2, 3]);
    const guest = lobbySeats(["bot", "host"], "bot", ["host", null, null, "bot"]);
    expect(guest.map((seat) => seat.member)).toEqual(["bot", "host", null, null]);
    expect(guest[2].seat).toBe(1);
  });

  const occupied = (members: readonly string[], self: string) =>
    Object.fromEntries(
      lobbySeats(members, self)
        .filter((seat) => seat.member !== null)
        .map((seat) => [seat.view, seat.member])
    );

  it("seats a host alone at the bottom, the first arrival to their left", () => {
    const seats = lobbySeats(["host"], "host");
    expect(seats[0].member).toBe("host");
    expect(seats.find((seat) => seat.next)?.view).toBe(3);
  });

  it("matches the final seating at every stage", () => {
    // the final server order, as the deal will translate it for the host:
    // view = (position - myPosition) mod 4
    const final = ["c", "b", "a", "host"];
    expect(occupied(final, "host")).toEqual({ 0: "host", 1: "c", 2: "b", 3: "a" });
    expect(occupied(["a", "host"], "host")).toEqual({ 0: "host", 3: "a" });
    expect(occupied(["b", "a", "host"], "host")).toEqual({ 0: "host", 2: "b", 3: "a" });
  });

  it("gives a guest the same table from their own side", () => {
    expect(occupied(["c", "b", "a", "host"], "a")).toEqual({ 0: "a", 1: "host", 2: "c", 3: "b" });
    // already seated members keep those places before c arrives
    expect(occupied(["b", "a", "host"], "a")).toEqual({ 0: "a", 1: "host", 3: "b" });
  });

  it("points at the seat the next arrival takes", () => {
    const next = (members: readonly string[], self: string) =>
      lobbySeats(members, self).find((seat) => seat.next)?.view;
    expect(next(["a", "host"], "host")).toBe(2);
    expect(next(["b", "a", "host"], "host")).toBe(1);
    expect(next(["c", "b", "a", "host"], "host")).toBeUndefined();
    // for guest a, b sits at view 3: the next arrival (c) takes view 2
    expect(next(["b", "a", "host"], "a")).toBe(2);
  });
});
