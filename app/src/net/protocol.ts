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

// game.rkt assigns ranks by position in '(jack queen king ace ten nine eight seven)
const RANK_TO_NUM: Record<Rank, number> = {
  J: 7, Q: 6, K: 5, A: 4, "10": 3, "9": 2, "8": 1, "7": 0,
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
}

export type ServerEvent =
  | { readonly _tag: "RoomCreated" }
  | { readonly _tag: "RoomJoined" }
  | { readonly _tag: "RoomLeft" }
  | { readonly _tag: "RoomClosed" }
  | { readonly _tag: "RemovedFromRoom" }
  | { readonly _tag: "ActiveRooms"; readonly rooms: readonly RoomInfo[] }
  | { readonly _tag: "StartGameFailed"; readonly reason: string }
  | { readonly _tag: "RoomMembers"; readonly members: readonly string[] }
  | { readonly _tag: "GameStarted" }
  | { readonly _tag: "HandDealt"; readonly cards: readonly CardModel[] }
  | { readonly _tag: "Turn"; readonly playerIndex: number }
  | { readonly _tag: "BidRequested"; readonly currentBid: number }
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
      return { _tag: "RoomMembers", members: members.map(memberName) };
    }

    case "game-started":
      return { _tag: "GameStarted" };

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
        if (typeof name !== "string" && !(name instanceof Sym)) return [];
        return [{
          host: typeof host === "string" || host instanceof Sym ? memberName(host) : "",
          name: memberName(name),
          members: Array.isArray(members) ? members.map(memberName) : [],
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
      return { _tag: "BidRequested", currentBid: rest[0] as number };

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

    case "error":
      return { _tag: "ServerError", message: writeSExpr(rest[0] ?? sym("unknown")) };

    default:
      return { _tag: "Ignored", raw: trimmed };
  }
}

// --- client commands ---

export type ClientCommand =
  | { readonly _tag: "ConnectUser"; readonly email: string; readonly picUrl: string }
  | { readonly _tag: "MakeRoom"; readonly hostEmail: string; readonly roomName: string }
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
  | { readonly _tag: "ExposeTrump"; readonly email: string };

export function encodeCommand(command: ClientCommand): string {
  switch (command._tag) {
    case "ConnectUser":
      return writeSExpr([sym("connect-user"), command.email, command.picUrl]);
    case "MakeRoom":
      return writeSExpr([sym("make-room"), command.hostEmail, command.roomName]);
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
  }
}
