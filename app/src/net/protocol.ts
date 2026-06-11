// Typed encode/decode for the Racket server's s-expression protocol.
// Player indices in ServerEvents are *server* seat indices (positions in the
// server's players list); GameSession translates them to view indices.

import { CardModel, Rank, Suit } from "../game/GameModel";
import { assocValue, isSym, Pair, parseSExpr, Prefab, SExpr, SHash, Sym, sym, writeSExpr } from "./sexpr";

// --- card mapping (mirrors deck generation in game.rkt) ---

const RANK_TO_NAME: Record<Rank, string> = {
  J: "jack", Q: "queen", K: "king", A: "ace",
  "10": "ten", "9": "nine", "8": "eight", "7": "seven",
};

const NAME_TO_RANK: Record<string, Rank> = Object.fromEntries(
  Object.entries(RANK_TO_NAME).map(([rank, name]) => [name, rank as Rank])
);

// game.rkt's numeric rank doubles as trick strength: J > 9 > A > 10 > K > Q > 8 > 7
const RANK_TO_NUM: Record<Rank, number> = {
  J: 7, "9": 6, A: 5, "10": 4, K: 3, Q: 2, "8": 1, "7": 0,
};

const RANK_TO_POINTS: Record<Rank, number> = {
  J: 3, "9": 2, A: 1, "10": 1, K: 0, Q: 0, "8": 0, "7": 0,
};

const SERVER_TO_SUIT: Record<string, Suit> = {
  heart: "hearts", diamond: "diamonds", club: "clubs", spade: "spades",
};

const SUIT_TO_SERVER: Record<Suit, string> = {
  hearts: "heart", diamonds: "diamond", clubs: "club", spades: "spade",
};

export function cardId(suit: Suit, rank: Rank): string {
  return `${suit}-${rank}`;
}

export function cardPoints(card: CardModel): number {
  return RANK_TO_POINTS[card.rank];
}

export function decodeCard(expr: SExpr): CardModel {
  if (!(expr instanceof Prefab) || expr.name !== "card") {
    throw new Error(`protocol: not a card: ${writeSExpr(expr)}`);
  }
  const [name, , suit] = expr.fields;
  if (!(name instanceof Sym) || !(suit instanceof Sym)) {
    throw new Error(`protocol: malformed card: ${writeSExpr(expr)}`);
  }
  const rank = NAME_TO_RANK[name.name];
  const suitName = SERVER_TO_SUIT[suit.name];
  if (!rank || !suitName) throw new Error(`protocol: unknown card ${name.name}/${suit.name}`);
  return { id: cardId(suitName, rank), suit: suitName, rank };
}

export function encodeCard(card: CardModel): Prefab {
  return new Prefab("card", [
    sym(RANK_TO_NAME[card.rank]),
    RANK_TO_NUM[card.rank],
    sym(SUIT_TO_SERVER[card.suit]),
    RANK_TO_POINTS[card.rank],
  ]);
}

export function decodeSuit(expr: SExpr): Suit | null {
  if (expr instanceof Sym && SERVER_TO_SUIT[expr.name]) return SERVER_TO_SUIT[expr.name];
  return null;
}

// --- server events ---

export type BidValue = number | "pass";

export interface RoomInfo {
  readonly host: string;
  readonly name: string;
  readonly members: readonly string[];
  readonly target: number; // game points this table plays to
}

// Mid-match view for a reconnecting client, straight from the server's
// shadow of the running game. Seats are *server* indices throughout.
export interface MatchSnapshot {
  readonly room: string;
  readonly members: readonly string[];
  readonly yourSeat: number;
  readonly handNumber: number;
  readonly evens: number;
  readonly odds: number;
  readonly target: number;
  readonly stage: "bidding" | "choosing-trump" | "playing" | "between-hands";
  readonly highBid: { readonly value: number; readonly seat: number } | null;
  readonly bidResult: { readonly value: number; readonly seat: number } | null;
  readonly trumpSuit: Suit | null; // only revealed to the bidder pre-exposure
  readonly trumpExposed: boolean;
  readonly yourHand: readonly CardModel[];
  readonly trickLeader: number;
  readonly trick: ReadonlyArray<{ readonly seat: number; readonly card: CardModel }>;
  readonly points: ReadonlyMap<number, number>; // seat → card points taken
  readonly tricks: ReadonlyMap<number, number>; // seat → tricks taken
  readonly lastResult: ServerEvent | null; // hand-result, between hands
  readonly awaiting: number | null; // whose answer the server wants
  readonly request: ServerEvent | null; // the pending request, when it is ours
}

export type ServerEvent =
  | { readonly _tag: "RoomCreated" }
  | { readonly _tag: "RoomJoined" }
  | { readonly _tag: "RoomLeft" }
  | { readonly _tag: "RoomClosed" }
  | { readonly _tag: "RemovedFromRoom" }
  | { readonly _tag: "ActiveRooms"; readonly rooms: readonly RoomInfo[] }
  | { readonly _tag: "StartGameFailed"; readonly reason: string }
  | {
      readonly _tag: "RoomMembers";
      readonly members: readonly string[];
      readonly target?: number; // what the table plays to, when broadcast
    }
  | { readonly _tag: "GameStarted" }
  | { readonly _tag: "GameAborted" }
  | { readonly _tag: "HandDealt"; readonly cards: readonly CardModel[] }
  | { readonly _tag: "Turn"; readonly playerIndex: number }
  | { readonly _tag: "BidRequested"; readonly minBid: number }
  | { readonly _tag: "BidPlaced"; readonly playerIndex: number; readonly bid: BidValue }
  | { readonly _tag: "BidResult"; readonly bid: number; readonly playerIndex: number }
  | { readonly _tag: "ChooseTrumpRequested" }
  | { readonly _tag: "TrumpSelected" }
  | { readonly _tag: "CardPlayed"; readonly playerIndex: number; readonly card: CardModel | "expose-trump" }
  | {
      readonly _tag: "PlayRequested";
      readonly cardsPlayed: readonly CardModel[];
      readonly trumpSuit: Suit | null;
      readonly firstSuit: Suit | null;
    }
  | { readonly _tag: "PointsWon"; readonly points: ReadonlyMap<number, number> }
  | {
      // game points after a hand; bidder is a *server* seat index and
      // evens/odds are the server-side teams (seat parity)
      readonly _tag: "HandResult";
      readonly hand: number;
      readonly bidder: number;
      readonly bid: number;
      readonly made: boolean;
      readonly delta: number;
      readonly evens: number;
      readonly odds: number;
      readonly target: number;
    }
  | {
      readonly _tag: "MatchOver";
      readonly winner: "evens" | "odds";
      readonly evens: number;
      readonly odds: number;
      readonly hands: number;
    }
  | { readonly _tag: "PlayerDisconnected"; readonly email: string; readonly graceSeconds: number }
  | { readonly _tag: "PlayerReconnected"; readonly email: string }
  | { readonly _tag: "EmotePlayed"; readonly seat: number; readonly emote: string }
  | { readonly _tag: "SeatAbandoned"; readonly seat: number; readonly email: string }
  | {
      readonly _tag: "SeatReplaced";
      readonly seat: number;
      readonly email: string; // the bot now in the seat
      readonly members: readonly string[]; // full seat list, post-replacement
    }
  | { readonly _tag: "GameSnapshot"; readonly snapshot: MatchSnapshot }
  | { readonly _tag: "NoRunningGame" }
  | {
      readonly _tag: "Leaderboard";
      readonly rows: ReadonlyArray<{
        readonly name: string;
        readonly played: number;
        readonly won: number;
      }>;
    }
  | { readonly _tag: "ServerError"; readonly message: string }
  | { readonly _tag: "Disconnected" }
  | { readonly _tag: "Ignored"; readonly raw: string };

function memberName(expr: SExpr): string {
  if (typeof expr === "string") return expr;
  if (expr instanceof Sym) return expr.name;
  throw new Error(`protocol: unexpected room member ${writeSExpr(expr)}`);
}

export function decodeServerEvent(frame: string): ServerEvent | null {
  const trimmed = frame.trim();
  if (trimmed === "" || trimmed === "#<void>") return null;

  let expr: SExpr;
  try {
    expr = parseSExpr(trimmed);
  } catch {
    return { _tag: "Ignored", raw: trimmed };
  }
  return decodeServerExpr(expr, trimmed);
}

// Also used to decode messages embedded inside a game-snapshot (the pending
// request, the between-hands result).
export function decodeServerExpr(expr: SExpr, trimmed = writeSExpr(expr)): ServerEvent | null {
  if (isSym(expr, "room-created")) return { _tag: "RoomCreated" };
  if (isSym(expr, "room-joined")) return { _tag: "RoomJoined" };
  if (isSym(expr, "room-left")) return { _tag: "RoomLeft" };
  if (!Array.isArray(expr) || expr.length === 0 || !(expr[0] instanceof Sym)) {
    return { _tag: "Ignored", raw: trimmed };
  }

  const [head, ...rest] = expr;
  switch ((head as Sym).name) {
    case "room-members": {
      const members = rest[1];
      if (!Array.isArray(members)) return { _tag: "Ignored", raw: trimmed };
      const target = rest[2];
      return {
        _tag: "RoomMembers",
        members: members.map(memberName),
        ...(typeof target === "number" ? { target } : {}),
      };
    }

    case "game-started":
      return { _tag: "GameStarted" };

    case "game-aborted":
      return { _tag: "GameAborted" };

    case "room-closed":
      return { _tag: "RoomClosed" };

    case "removed-from-room":
      return { _tag: "RemovedFromRoom" };

    case "start-game-failed": {
      const reason = rest[0];
      return { _tag: "StartGameFailed", reason: reason instanceof Sym ? reason.name : "unknown" };
    }

    case "active-rooms": {
      const roomList = rest[0];
      if (!Array.isArray(roomList)) return { _tag: "Ignored", raw: trimmed };
      const rooms = roomList.flatMap((entry): RoomInfo[] => {
        const host = assocValue(entry, "host");
        const name = assocValue(entry, "name");
        const members = assocValue(entry, "members");
        const target = assocValue(entry, "target");
        if (typeof name !== "string" && !(name instanceof Sym)) return [];
        return [{
          host: typeof host === "string" || host instanceof Sym ? memberName(host) : "",
          name: memberName(name),
          members: Array.isArray(members) ? members.map(memberName) : [],
          target: typeof target === "number" ? target : 6,
        }];
      });
      return { _tag: "ActiveRooms", rooms };
    }

    case "hand": {
      const cards = rest[1];
      if (!Array.isArray(cards)) return { _tag: "Ignored", raw: trimmed };
      return { _tag: "HandDealt", cards: cards.map(decodeCard) };
    }

    case "turn":
      return { _tag: "Turn", playerIndex: rest[0] as number };

    case "request-bid":
      // the server sends the minimum acceptable bid (16 to open, else high+1)
      return { _tag: "BidRequested", minBid: rest[0] as number };

    case "bid-placed": {
      const bid = rest[1];
      return {
        _tag: "BidPlaced",
        playerIndex: rest[0] as number,
        bid: isSym(bid, "pass") ? "pass" : (bid as number),
      };
    }

    case "bid-result": {
      const result = rest[0];
      if (!(result instanceof Pair)) return { _tag: "Ignored", raw: trimmed };
      return { _tag: "BidResult", bid: result.car as number, playerIndex: result.cdr as number };
    }

    case "choose-trump":
      return { _tag: "ChooseTrumpRequested" };

    case "trump-selected":
      return { _tag: "TrumpSelected" };

    case "played": {
      const card = rest[1];
      return {
        _tag: "CardPlayed",
        playerIndex: rest[0] as number,
        card: isSym(card, "expose-trump") ? "expose-trump" : decodeCard(card),
      };
    }

    case "play-card": {
      const cardsPlayed = assocValue(expr, "cards-played") ?? [];
      const gameState = assocValue(expr, "game-state") ?? [];
      const cards = Array.isArray(cardsPlayed) ? cardsPlayed.map(decodeCard) : [];
      return {
        _tag: "PlayRequested",
        cardsPlayed: cards,
        trumpSuit: decodeSuit(assocValue(gameState, "trump-suit") ?? false),
        firstSuit: decodeSuit(assocValue(gameState, "first-suit") ?? false),
      };
    }

    case "points-won": {
      const hash = rest[0];
      if (!(hash instanceof SHash)) return { _tag: "Ignored", raw: trimmed };
      const points = new Map<number, number>();
      for (const [k, v] of hash.entries) points.set(k as number, v as number);
      return { _tag: "PointsWon", points };
    }

    case "hand-result": {
      const body = rest[0];
      const num = (key: string) => assocValue(body, key);
      const hand = num("hand");
      const bidder = num("bidder");
      const bid = num("bid");
      const delta = num("delta");
      const evens = num("evens");
      const odds = num("odds");
      const target = num("target");
      if (
        typeof hand !== "number" || typeof bidder !== "number" || typeof bid !== "number" ||
        typeof delta !== "number" || typeof evens !== "number" || typeof odds !== "number" ||
        typeof target !== "number"
      ) {
        return { _tag: "Ignored", raw: trimmed };
      }
      return {
        _tag: "HandResult",
        hand, bidder, bid,
        made: assocValue(body, "made") === true,
        delta, evens, odds, target,
      };
    }

    case "player-disconnected": {
      const email = rest[0];
      const grace = rest[1];
      if (!(typeof email === "string" || email instanceof Sym)) {
        return { _tag: "Ignored", raw: trimmed };
      }
      return {
        _tag: "PlayerDisconnected",
        email: memberName(email),
        graceSeconds: typeof grace === "number" ? grace : 0,
      };
    }

    case "player-reconnected": {
      const email = rest[0];
      if (!(typeof email === "string" || email instanceof Sym)) {
        return { _tag: "Ignored", raw: trimmed };
      }
      return { _tag: "PlayerReconnected", email: memberName(email) };
    }

    case "no-running-game":
      return { _tag: "NoRunningGame" };

    case "leaderboard": {
      const body = rest[0];
      if (!Array.isArray(body)) return { _tag: "Ignored", raw: trimmed };
      const rows = body.flatMap((entry) => {
        if (!Array.isArray(entry) || entry.length < 3) return [];
        const [name, played, won] = entry;
        if (!(typeof name === "string" || name instanceof Sym)) return [];
        if (typeof played !== "number" || typeof won !== "number") return [];
        return [{ name: memberName(name), played, won }];
      });
      return { _tag: "Leaderboard", rows };
    }

    case "emote-played": {
      const seat = rest[0];
      const emote = rest[1];
      if (typeof seat !== "number" || !(emote instanceof Sym)) {
        return { _tag: "Ignored", raw: trimmed };
      }
      return { _tag: "EmotePlayed", seat, emote: emote.name };
    }

    case "seat-abandoned": {
      const seat = rest[0];
      const email = rest[1];
      if (typeof seat !== "number" || !(typeof email === "string" || email instanceof Sym)) {
        return { _tag: "Ignored", raw: trimmed };
      }
      return { _tag: "SeatAbandoned", seat, email: memberName(email) };
    }

    case "seat-replaced": {
      const seat = rest[0];
      const email = rest[1];
      const members = rest[2];
      if (
        typeof seat !== "number" ||
        !(typeof email === "string" || email instanceof Sym) ||
        !Array.isArray(members)
      ) {
        return { _tag: "Ignored", raw: trimmed };
      }
      return {
        _tag: "SeatReplaced",
        seat,
        email: memberName(email),
        members: members.map(memberName),
      };
    }

    case "game-snapshot": {
      const snapshot = decodeSnapshot(rest[0]);
      return snapshot ? { _tag: "GameSnapshot", snapshot } : { _tag: "Ignored", raw: trimmed };
    }

    case "match-over": {
      const body = rest[0];
      const winner = assocValue(body, "winner") ?? false;
      const evens = assocValue(body, "evens");
      const odds = assocValue(body, "odds");
      const hands = assocValue(body, "hands");
      if (
        !(isSym(winner, "evens") || isSym(winner, "odds")) ||
        typeof evens !== "number" || typeof odds !== "number" || typeof hands !== "number"
      ) {
        return { _tag: "Ignored", raw: trimmed };
      }
      return { _tag: "MatchOver", winner: (winner as Sym).name as "evens" | "odds", evens, odds, hands };
    }

    case "error":
      return { _tag: "ServerError", message: writeSExpr(rest[0] ?? sym("unknown")) };

    default:
      return { _tag: "Ignored", raw: trimmed };
  }
}

// (17 . 1) → { value, seat }; anything else → null
function decodeBidPair(value: SExpr | undefined): { value: number; seat: number } | null {
  if (value instanceof Pair && typeof value.car === "number" && typeof value.cdr === "number") {
    return { value: value.car, seat: value.cdr };
  }
  return null;
}

// ((0 . 5) (1 . 0) …) → Map seat → number
function decodeSeatMap(value: SExpr | undefined): Map<number, number> {
  const map = new Map<number, number>();
  if (Array.isArray(value)) {
    for (const entry of value) {
      if (entry instanceof Pair && typeof entry.car === "number" && typeof entry.cdr === "number") {
        map.set(entry.car, entry.cdr);
      }
    }
  }
  return map;
}

function decodeSnapshot(body: SExpr | undefined): MatchSnapshot | null {
  if (!Array.isArray(body)) return null;
  const num = (key: string): number | null => {
    const value = assocValue(body, key);
    return typeof value === "number" ? value : null;
  };
  const room = assocValue(body, "room");
  const members = assocValue(body, "members");
  const stage = assocValue(body, "stage");
  const yourSeat = num("your-seat");
  const handNumber = num("hand-number");
  const evens = num("evens");
  const odds = num("odds");
  const target = num("target");
  const trickLeader = num("trick-leader");
  if (
    !(typeof room === "string" || room instanceof Sym) ||
    !Array.isArray(members) || !(stage instanceof Sym) ||
    yourSeat === null || handNumber === null || evens === null ||
    odds === null || target === null || trickLeader === null
  ) {
    return null;
  }
  const stageName = stage.name;
  if (
    stageName !== "bidding" && stageName !== "choosing-trump" &&
    stageName !== "playing" && stageName !== "between-hands"
  ) {
    return null;
  }

  const yourHandRaw = assocValue(body, "your-hand");
  const yourHand: CardModel[] = [];
  if (Array.isArray(yourHandRaw)) {
    for (const card of yourHandRaw) yourHand.push(decodeCard(card));
  }

  const trickRaw = assocValue(body, "trick");
  const trick: Array<{ seat: number; card: CardModel }> = [];
  if (Array.isArray(trickRaw)) {
    for (const play of trickRaw) {
      if (play instanceof Pair && typeof play.car === "number") {
        trick.push({ seat: play.car, card: decodeCard(play.cdr) });
      }
    }
  }

  const lastResultRaw = assocValue(body, "last-result");
  const lastResult = Array.isArray(lastResultRaw)
    ? decodeServerExpr([sym("hand-result"), lastResultRaw])
    : null;

  const requestRaw = assocValue(body, "request");
  const request = Array.isArray(requestRaw) ? decodeServerExpr(requestRaw) : null;

  const awaitingRaw = assocValue(body, "awaiting");

  return {
    room: memberName(room),
    members: members.map(memberName),
    yourSeat,
    handNumber,
    evens,
    odds,
    target,
    stage: stageName,
    highBid: decodeBidPair(assocValue(body, "high-bid")),
    bidResult: decodeBidPair(assocValue(body, "bid-result")),
    trumpSuit: decodeSuit(assocValue(body, "trump") ?? false),
    trumpExposed: assocValue(body, "trump-exposed") === true,
    yourHand,
    trickLeader,
    trick,
    points: decodeSeatMap(assocValue(body, "points")),
    tricks: decodeSeatMap(assocValue(body, "tricks")),
    lastResult: lastResult && lastResult._tag !== "Ignored" ? lastResult : null,
    awaiting: typeof awaitingRaw === "number" ? awaitingRaw : null,
    request: request && request._tag !== "Ignored" ? request : null,
  };
}

// --- client commands ---

export type ClientCommand =
  | { readonly _tag: "ConnectUser"; readonly email: string; readonly picUrl: string }
  | {
      readonly _tag: "MakeRoom";
      readonly hostEmail: string;
      readonly roomName: string;
      readonly target: number;
    }
  | { readonly _tag: "JoinRoom"; readonly roomName: string; readonly email: string }
  | { readonly _tag: "LeaveRoom"; readonly roomName: string; readonly email: string }
  | {
      readonly _tag: "KickFromRoom";
      readonly roomName: string;
      readonly hostEmail: string;
      readonly targetEmail: string;
    }
  | { readonly _tag: "GetActiveRooms" }
  | { readonly _tag: "Ping" }
  | { readonly _tag: "AddBot"; readonly roomName: string }
  | { readonly _tag: "StartGame"; readonly roomName: string }
  | { readonly _tag: "PutBid"; readonly email: string; readonly bid: BidValue }
  | { readonly _tag: "SelectTrump"; readonly email: string; readonly suit: Suit }
  | { readonly _tag: "PlayCard"; readonly email: string; readonly card: CardModel }
  | { readonly _tag: "ExposeTrump"; readonly email: string }
  | { readonly _tag: "NextHand"; readonly email: string }
  | { readonly _tag: "LeaveGame"; readonly email: string }
  | { readonly _tag: "Rejoin"; readonly email: string }
  | { readonly _tag: "SendEmote"; readonly email: string; readonly emote: string }
  | { readonly _tag: "ReplaceWithBot"; readonly email: string }
  | { readonly _tag: "CloseGame"; readonly email: string }
  | { readonly _tag: "GetLeaderboard" };

export function encodeCommand(command: ClientCommand): string {
  switch (command._tag) {
    case "ConnectUser":
      return writeSExpr([sym("connect-user"), command.email, command.picUrl]);
    case "MakeRoom":
      return writeSExpr([sym("make-room"), command.hostEmail, command.roomName, command.target]);
    case "JoinRoom":
      return writeSExpr([sym("join-room"), command.roomName, command.email]);
    case "LeaveRoom":
      return writeSExpr([sym("leave-room"), command.roomName, command.email]);
    case "KickFromRoom":
      return writeSExpr([
        sym("kick-from-room"),
        command.roomName,
        command.hostEmail,
        command.targetEmail,
      ]);
    case "GetActiveRooms":
      return writeSExpr([sym("get-active-rooms")]);
    case "Ping":
      return writeSExpr([sym("ping")]);
    case "AddBot":
      return writeSExpr([sym("add-bot-to-room"), command.roomName]);
    case "StartGame":
      return writeSExpr([sym("start-game"), command.roomName]);
    case "PutBid":
      return writeSExpr([
        sym("put-bid"),
        command.email,
        command.bid === "pass" ? sym("pass") : command.bid,
      ]);
    case "SelectTrump":
      return writeSExpr([sym("selected-trump"), command.email, sym(SUIT_TO_SERVER[command.suit])]);
    case "PlayCard":
      return writeSExpr([sym("card-played"), command.email, encodeCard(command.card)]);
    case "ExposeTrump":
      return writeSExpr([sym("card-played"), command.email, sym("expose-trump")]);
    case "NextHand":
      return writeSExpr([sym("next-hand"), command.email]);
    case "LeaveGame":
      return writeSExpr([sym("leave-game"), command.email]);
    case "Rejoin":
      return writeSExpr([sym("rejoin"), command.email]);
    case "SendEmote":
      return writeSExpr([sym("emote"), command.email, sym(command.emote)]);
    case "ReplaceWithBot":
      return writeSExpr([sym("replace-with-bot"), command.email]);
    case "CloseGame":
      return writeSExpr([sym("close-game"), command.email]);
    case "GetLeaderboard":
      return writeSExpr([sym("get-leaderboard")]);
  }
}
