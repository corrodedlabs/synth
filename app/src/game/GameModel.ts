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
  | "bidding"
  | "choosing-trump"
  | "playing"
  | "finished";

// What the server is currently waiting on from us.
export type PendingRequest =
  | { readonly kind: "bid"; readonly currentBid: number }
  | { readonly kind: "trump" }
  | { readonly kind: "play"; readonly firstSuit: Suit | null; readonly trumpSuit: Suit | null };

export interface GameModel {
  readonly phase: GamePhase;
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
  readonly bidWinner: PlayerIndex | null;
  readonly finalBid: number | null;
  readonly trumpSuit: Suit | null; // known if we chose it or after exposure
  readonly trumpExposed: boolean;
  readonly pendingRequest: PendingRequest | null;
  readonly points: readonly number[] | null; // by view index, set at hand end
}

export type GameAction =
  | { readonly _tag: "PhaseChanged"; readonly phase: GamePhase }
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
  | { readonly _tag: "GameFinished"; readonly points: readonly number[] };

export const initialGameModel: GameModel = {
  phase: "idle",
  hand: [],
  playedCards: [],
  activePlayer: null,
  score: 0,
  tricks: 0,
  theirScore: 0,
  theirTricks: 0,
  currentBid: 16,
  bidWinner: null,
  finalBid: null,
  trumpSuit: null,
  trumpExposed: false,
  pendingRequest: null,
  points: null,
};

export function gameReducer(state: GameModel, action: GameAction): GameModel {
  switch (action._tag) {
    case "PhaseChanged":
      return { ...state, phase: action.phase };

    case "HandDealt":
      return {
        ...state,
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
        playedCards: [],
        score: ours ? state.score + action.points : state.score,
        tricks: ours ? state.tricks + 1 : state.tricks,
        theirScore: !ours && action.winner !== null ? state.theirScore + action.points : state.theirScore,
        theirTricks: !ours && action.winner !== null ? state.theirTricks + 1 : state.theirTricks,
      };
    }

    case "GameFinished":
      return {
        ...state,
        phase: "finished",
        points: action.points,
        score: action.points[0] + action.points[2],
        theirScore: action.points[1] + action.points[3],
        activePlayer: null,
        pendingRequest: null,
      };
  }
}

// Mirrors valid-card? in game.rkt — used only as a UX guard; the server
// remains the authority.
export function isLegalPlay(model: GameModel, card: CardModel): boolean {
  const request = model.pendingRequest;
  if (!request || request.kind !== "play") return false;
  if (request.firstSuit === null) return true;
  if (card.suit === request.firstSuit) return true;
  if (request.trumpSuit !== null && card.suit === request.trumpSuit) return true;
  return !model.hand.some((handCard) => handCard.suit === request.firstSuit);
}

// Expose-trump is offered when we cannot follow suit and trump is still hidden.
export function canExposeTrump(model: GameModel): boolean {
  const request = model.pendingRequest;
  if (!request || request.kind !== "play") return false;
  if (model.trumpExposed || request.trumpSuit !== null) return false;
  if (request.firstSuit === null) return false;
  return !model.hand.some((handCard) => handCard.suit === request.firstSuit);
}
