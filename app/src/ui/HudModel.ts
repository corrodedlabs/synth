// The score HUD, the result wording and the lobby seating as plain data, with
// no DOM in sight, so the awkward cases (a negative score, a table to 2, a
// seat nobody has taken yet) can be tested directly.

const MINUS = "−";
// More pips than this stop reading as a row of points on a phone.
const MAX_PIPS = 12;

export function scoreText(value: number): string {
  return value < 0 ? `${MINUS}${-value}` : String(value);
}

export function signedText(value: number): string {
  if (value === 0) return "0";
  return value > 0 ? `+${value}` : `${MINUS}${-value}`;
}

export interface PipRow {
  readonly total: number;
  readonly filled: number;
  // A pip can't say "below nothing" or "past the end". The number is always
  // there, in its own column, so the rows stay lined up when a score goes negative.
  readonly number: string;
}

export function pipRow(score: number, target: number): PipRow {
  const goal = Number.isFinite(target) && target >= 1 ? Math.floor(target) : 1;
  const total = Math.min(goal, MAX_PIPS);
  const clamped = Math.max(0, Math.min(score, goal));
  const filled = goal <= MAX_PIPS ? clamped : Math.round((clamped / goal) * total);
  return { total, filled, number: scoreText(score) };
}

// There are 28 card points in a hand: the line runs 0 to 28 and never past it.
export const HAND_POINTS = 28;

export function lineFraction(points: number): number {
  return Math.max(0, Math.min(points / HAND_POINTS, 1));
}

export function progressLabel(
  ourPoints: number,
  theirPoints: number,
  bid: number | null,
  bidIsOurs: boolean
): string {
  if (bid === null) return "";
  if (!bidIsOurs) return `You ${ourPoints} · Them ${theirPoints}`;
  return ourPoints >= bid ? `Bid made with ${ourPoints}` : `${ourPoints} of ${bid}`;
}

// Whose the bid is, for the small caption under the contract.
export function bidOwnerLabel(bidder: number | null, name: string | null): string {
  if (bidder === null) return "";
  if (bidder === 0) return "your bid";
  return name ? `${name}'s bid` : "their bid";
}

// handResults holds the swing from our side (+N when our score moved up, −N
// when theirs did), and only the bidding side's score ever moves. Turn that
// back into what happened to the bidders. A restored snapshot re-announces a
// hand already counted with a swing of 0: there is no change to report.
export function bidderDelta(swing: number, bidderIsOurs: boolean): number | null {
  if (swing === 0) return null;
  return bidderIsOurs ? swing : -swing;
}

export interface HandResultCopy {
  readonly seal: "MADE" | "SET";
  readonly detail: string;
  readonly points: string | null;
}

export function handResultCopy(result: {
  readonly bidderName: string;
  readonly bid: number;
  readonly took: number;
  readonly made: boolean;
  readonly delta: number | null;
  readonly bidderIsOurs: boolean;
}): HandResultCopy {
  const { bidderName, bid, took, made, delta, bidderIsOurs } = result;
  const points =
    delta === null ? null : bidderIsOurs ? signedText(delta) : `${signedText(delta)} to them`;
  return {
    seal: made ? "MADE" : "SET",
    detail: `${bidderName} bid ${bid} and took ${took}`,
    points,
  };
}

export type ViewSeat = 0 | 1 | 2 | 3;

export interface LobbySeat {
  readonly view: ViewSeat;
  readonly member: string | null;
  // the seat the next arrival will sit in
  readonly next: boolean;
  // the seat's number at the table, to ask for it by; absent before the server has seated anyone
  readonly seat?: number;
}

// The server keeps members newest first with the host last, and seats them in
// that order when the match starts. Every later arrival pushes everyone along
// by one, so where a member sits relative to us never changes: it depends only
// on the gap between the two positions in the list. That lets the lobby show
// the same seats the deal will use.
export function lobbySeats(
  members: readonly string[],
  self: string,
  tableSeats?: readonly (string | null)[]
): readonly LobbySeat[] {
  // The server's own seating, once it has sent one: empty seats can be asked for by number.
  if (tableSeats && tableSeats.length === 4) {
    const mine = Math.max(0, tableSeats.indexOf(self));
    return [0, 1, 2, 3].map((view) => {
      const seat = (mine + view) % 4;
      return { view: view as ViewSeat, member: tableSeats[seat] ?? null, next: false, seat };
    });
  }
  const count = Math.min(members.length, 4);
  const found = members.indexOf(self);
  const mine = found >= 0 ? found : Math.max(0, count - 1);
  const seats: (string | null)[] = [null, null, null, null];
  members.slice(0, 4).forEach((member, position) => {
    seats[(position - mine + 8) % 4] = member;
  });
  // A newcomer goes to the front of the list, which also moves us back one.
  const nextView = count < 4 ? (0 - (mine + 1) + 8) % 4 : -1;
  return seats.map((member, view) => ({
    view: view as ViewSeat,
    member,
    next: view === nextView,
  }));
}
