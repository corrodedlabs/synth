export const SUITS = ["hearts", "diamonds", "clubs", "spades"] as const;
export const RANKS = ["J", "9", "A", "10", "K", "Q", "8", "7"] as const;

export type Suit = typeof SUITS[number];
export type Rank = typeof RANKS[number];
export type PlayerIndex = 0 | 1 | 2 | 3;

export interface CardModel {
  readonly id: string;
  readonly suit: Suit;
  readonly rank: Rank;
}

export interface PlayedCardModel {
  readonly card: CardModel;
  readonly playerIndex: PlayerIndex;
}

export type GamePhase =
  | "idle"
  | "connecting"
  | "lobby"
  | "bidding"
  | "choosing-trump"
  | "playing"
  | "hand-finished" // between hands: result up, waiting on the host's deal
  | "finished"; // the whole match is over

// What the server is currently waiting on from us.
export type PendingRequest =
  | { readonly kind: "bid"; readonly minBid: number }
  | { readonly kind: "trump" }
  | {
      readonly kind: "play";
      readonly firstSuit: Suit | null;
      readonly trumpSuit: Suit | null;
      // we just called for the exposure, so we must trump if we can
      readonly mustTrump?: boolean;
    };

export interface GameModel {
  readonly phase: GamePhase;
  // Lobby state. Members are in *server* order (newest joiner first, host last).
  readonly roomName: string | null;
  readonly members: readonly string[];
  readonly isHost: boolean;
  // Display names by view index (0 = us), fixed once the hand is dealt.
  readonly seatNames: readonly string[] | null;
  // All player indices in the model are *view* indices:
  // 0 = us (bottom), 1 = right, 2 = top (partner), 3 = left.
  readonly hand: readonly CardModel[];
  readonly playedCards: readonly PlayedCardModel[];
  readonly activePlayer: PlayerIndex | null;
  readonly score: number; // our team's card points
  readonly tricks: number; // our team's tricks
  readonly theirScore: number;
  readonly theirTricks: number;
  readonly currentBid: number;
  readonly bidsPlaced: number; // bids and passes seen this auction
  readonly bidWinner: PlayerIndex | null;
  readonly finalBid: number | null;
  readonly trumpSuit: Suit | null; // known if we chose it or after exposure
  readonly trumpExposed: boolean;
  readonly pendingRequest: PendingRequest | null;
  readonly points: readonly number[] | null; // by view index, set at hand end
  // Match state — survives between hands, cleared only on RoomLeft.
  readonly matchUs: number; // our team's game points
  readonly matchThem: number;
  readonly matchTarget: number;
  // per-hand swing from our side: +N when our score moved up, −N when
  // theirs did — the match-over panel renders these as chips
  readonly handResults: readonly number[];
  readonly handNumber: number;
  readonly matchWinner: "us" | "them" | null;
}

export type GameAction =
  | { readonly _tag: "PhaseChanged"; readonly phase: GamePhase }
  | {
      readonly _tag: "RoomEntered";
      readonly roomName: string;
      readonly members: readonly string[];
      readonly isHost: boolean;
      readonly target?: number; // what this table plays to, when known
    }
  | {
      readonly _tag: "MembersChanged";
      readonly members: readonly string[];
      readonly target?: number;
    }
  | { readonly _tag: "RoomLeft" }
  | { readonly _tag: "SeatNamesSet"; readonly names: readonly string[] }
  | { readonly _tag: "HandDealt"; readonly cards: readonly CardModel[] }
  | { readonly _tag: "CardPlayed"; readonly card: CardModel; readonly playerIndex: PlayerIndex }
  | { readonly _tag: "ActivePlayerChanged"; readonly playerIndex: PlayerIndex | null }
  | { readonly _tag: "TrickScored"; readonly scoreDelta: number; readonly tricksDelta: number }
  | { readonly _tag: "BidPlaced"; readonly playerIndex: PlayerIndex; readonly bid: number | "pass" }
  | { readonly _tag: "BidWon"; readonly playerIndex: PlayerIndex; readonly bid: number }
  | { readonly _tag: "TrumpSet"; readonly suit: Suit | null; readonly exposed: boolean }
  | { readonly _tag: "RequestReceived"; readonly request: PendingRequest }
  | { readonly _tag: "RequestCleared" }
  | { readonly _tag: "TrickCleared" }
  | {
      readonly _tag: "TrickEnded";
      readonly winner: PlayerIndex | null;
      readonly points: number;
    }
  | { readonly _tag: "HandFinished"; readonly points: readonly number[] }
  | {
      // game points after a hand (already translated to our perspective)
      readonly _tag: "HandResult";
      readonly bidder: PlayerIndex;
      readonly bid: number;
      readonly made: boolean;
      readonly us: number;
      readonly them: number;
      readonly target: number;
    }
  | { readonly _tag: "HandReset" } // next hand is being dealt
  | {
      readonly _tag: "MatchOver";
      readonly winner: "us" | "them";
      readonly us: number;
      readonly them: number;
      readonly hands: number;
    }
  // a reconnect: the session rebuilds the entire model from the server's
  // mid-match snapshot and swaps it in wholesale
  | { readonly _tag: "SnapshotRestored"; readonly model: GameModel };

export const initialGameModel: GameModel = {
  phase: "idle",
  roomName: null,
  members: [],
  isHost: false,
  seatNames: null,
  hand: [],
  playedCards: [],
  activePlayer: null,
  score: 0,
  tricks: 0,
  theirScore: 0,
  theirTricks: 0,
  currentBid: 16,
  bidsPlaced: 0,
  bidWinner: null,
  finalBid: null,
  trumpSuit: null,
  trumpExposed: false,
  pendingRequest: null,
  points: null,
  matchUs: 0,
  matchThem: 0,
  matchTarget: 6,
  handResults: [],
  handNumber: 1,
  matchWinner: null,
};

export function gameReducer(state: GameModel, action: GameAction): GameModel {
  switch (action._tag) {
    case "PhaseChanged":
      return { ...state, phase: action.phase };

    case "RoomEntered":
      return {
        ...state,
        phase: "lobby",
        roomName: action.roomName,
        members: action.members,
        isHost: action.isHost,
        matchTarget: action.target ?? state.matchTarget,
        handResults: [],
      };

    case "MembersChanged":
      return {
        ...state,
        members: action.members,
        matchTarget: action.target ?? state.matchTarget,
      };

    case "RoomLeft":
      // back to the start screen — also wipes any in-progress game state
      // (hand, trick, scores) so an aborted game leaves nothing behind
      return initialGameModel;

    case "SeatNamesSet":
      return { ...state, seatNames: action.names };

    case "HandDealt":
      return {
        ...state,
        // the first deal moves us from the lobby into the game
        phase: state.phase === "lobby" ? "bidding" : state.phase,
        hand: [...state.hand, ...action.cards],
      };

    case "CardPlayed":
      return {
        ...state,
        hand: state.hand.filter((card) => card.id !== action.card.id),
        playedCards: [
          ...state.playedCards,
          {
            card: action.card,
            playerIndex: action.playerIndex,
          },
        ],
      };

    case "ActivePlayerChanged":
      return {
        ...state,
        activePlayer: action.playerIndex,
      };

    case "TrickScored":
      return {
        ...state,
        score: state.score + action.scoreDelta,
        tricks: state.tricks + action.tricksDelta,
      };

    case "BidPlaced":
      return {
        ...state,
        currentBid: action.bid === "pass" ? state.currentBid : action.bid,
        bidsPlaced: state.bidsPlaced + 1,
      };

    case "BidWon":
      return {
        ...state,
        bidWinner: action.playerIndex,
        finalBid: action.bid,
        phase: "choosing-trump",
      };

    case "TrumpSet":
      return {
        ...state,
        trumpSuit: action.suit ?? state.trumpSuit,
        trumpExposed: action.exposed || state.trumpExposed,
        phase: state.phase === "choosing-trump" ? "playing" : state.phase,
      };

    case "RequestReceived":
      return { ...state, pendingRequest: action.request };

    case "RequestCleared":
      return { ...state, pendingRequest: null };

    case "TrickCleared":
      return { ...state, playedCards: [] };

    case "TrickEnded": {
      const ours = action.winner === 0 || action.winner === 2;
      return {
        ...state,
        // only the completed trick leaves the table — a card already led to
        // the next trick (possible at bot speed) must survive
        playedCards: state.playedCards.slice(4),
        score: ours ? state.score + action.points : state.score,
        tricks: ours ? state.tricks + 1 : state.tricks,
        theirScore: !ours && action.winner !== null ? state.theirScore + action.points : state.theirScore,
        theirTricks: !ours && action.winner !== null ? state.theirTricks + 1 : state.theirTricks,
      };
    }

    case "HandFinished":
      return {
        ...state,
        phase: "hand-finished",
        points: action.points,
        score: action.points[0] + action.points[2],
        theirScore: action.points[1] + action.points[3],
        activePlayer: null,
        pendingRequest: null,
      };

    case "HandResult": {
      // exactly one team's score moves per hand; a zero swing is a restored
      // snapshot re-announcing a hand already counted
      const swing = action.us - state.matchUs - (action.them - state.matchThem);
      return {
        ...state,
        matchUs: action.us,
        matchThem: action.them,
        matchTarget: action.target,
        handResults: swing === 0 ? state.handResults : [...state.handResults, swing],
      };
    }

    case "HandReset":
      // everything a single hand owns goes back to its initial value;
      // the room, the seats, and the match score live on
      return {
        ...state,
        phase: "bidding",
        hand: [],
        playedCards: [],
        activePlayer: null,
        score: 0,
        tricks: 0,
        theirScore: 0,
        theirTricks: 0,
        currentBid: 16,
        bidsPlaced: 0,
        bidWinner: null,
        finalBid: null,
        trumpSuit: null,
        trumpExposed: false,
        pendingRequest: null,
        points: null,
        handNumber: state.handNumber + 1,
      };

    case "MatchOver":
      return {
        ...state,
        phase: "finished",
        matchWinner: action.winner,
        matchUs: action.us,
        matchThem: action.them,
        activePlayer: null,
        pendingRequest: null,
      };

    case "SnapshotRestored":
      return action.model;
  }
}

// Mirrors the play rules in game.rkt — used only as a UX guard; the server
// remains the authority. Follow suit if you can; trumping never excuses
// breaking suit. After calling for the exposure you must trump if able.
export function isLegalPlay(model: GameModel, card: CardModel): boolean {
  const request = model.pendingRequest;
  if (!request || request.kind !== "play") return false;
  if (
    request.mustTrump &&
    request.trumpSuit !== null &&
    model.hand.some((handCard) => handCard.suit === request.trumpSuit)
  ) {
    return card.suit === request.trumpSuit;
  }
  if (request.firstSuit === null) return true;
  if (card.suit === request.firstSuit) return true;
  return !model.hand.some((handCard) => handCard.suit === request.firstSuit);
}

// Host duties mid-match (the between-hands deal button) belong to the
// original host — the last member in seat order — while their seat is
// still human; a botted host seat passes the duty to the first human.
// Mirrors the server's acting-host.
export function actingHostEmail(members: readonly string[]): string | null {
  const isHuman = (member: string) => !/^bot-\d+$/.test(member);
  const last = members[members.length - 1];
  if (last !== undefined && isHuman(last)) return last;
  return members.find(isHuman) ?? null;
}

// Expose-trump is offered when we cannot follow suit and trump is still hidden.
export function canExposeTrump(model: GameModel): boolean {
  const request = model.pendingRequest;
  if (!request || request.kind !== "play") return false;
  if (model.trumpExposed || request.trumpSuit !== null) return false;
  if (request.firstSuit === null) return false;
  return !model.hand.some((handCard) => handCard.suit === request.firstSuit);
}
