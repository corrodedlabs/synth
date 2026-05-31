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

export interface GameModel {
  readonly hand: readonly CardModel[];
  readonly playedCards: readonly PlayedCardModel[];
  readonly activePlayer: PlayerIndex | null;
  readonly score: number;
  readonly tricks: number;
}

export type GameAction =
  | { readonly _tag: "HandDealt"; readonly cards: readonly CardModel[] }
  | { readonly _tag: "CardPlayed"; readonly card: CardModel; readonly playerIndex: PlayerIndex }
  | { readonly _tag: "ActivePlayerChanged"; readonly playerIndex: PlayerIndex | null }
  | { readonly _tag: "TrickScored"; readonly scoreDelta: number; readonly tricksDelta: number };

export const initialGameModel: GameModel = {
  hand: [],
  playedCards: [],
  activePlayer: null,
  score: 0,
  tricks: 0,
};

export function gameReducer(state: GameModel, action: GameAction): GameModel {
  switch (action._tag) {
    case "HandDealt":
      return {
        ...state,
        hand: [...action.cards],
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
  }
}
