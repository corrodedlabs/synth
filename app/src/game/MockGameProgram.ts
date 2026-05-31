import { Effect, Random } from "effect";
import { CardModel, GameAction, PlayerIndex, RANKS, SUITS } from "./GameModel";

export type GameDispatch = (action: GameAction) => void;

const opponentPlayers = [1, 2, 3] as const;

function dispatchAction(dispatch: GameDispatch, action: GameAction) {
  return Effect.sync(() => dispatch(action));
}

function randomCard(id: string): Effect.Effect<CardModel> {
  return Effect.gen(function* () {
    const suitIndex = yield* Random.nextIntBetween(0, SUITS.length);
    const rankIndex = yield* Random.nextIntBetween(0, RANKS.length);

    return {
      id,
      suit: SUITS[suitIndex],
      rank: RANKS[rankIndex],
    };
  });
}

function randomHand(): Effect.Effect<readonly CardModel[]> {
  return Effect.gen(function* () {
    const cards: CardModel[] = [];
    for (let index = 0; index < 8; index++) {
      cards.push(yield* randomCard(`hand-${index}`));
    }
    return cards;
  });
}

function playRandomCard(dispatch: GameDispatch, playerIndex: PlayerIndex) {
  return Effect.gen(function* () {
    const card = yield* randomCard(`played-${playerIndex}`);
    yield* dispatchAction(dispatch, {
      _tag: "CardPlayed",
      card,
      playerIndex,
    });
  });
}

export function mockGameProgram(dispatch: GameDispatch): Effect.Effect<void> {
  return Effect.gen(function* () {
    const hand = yield* randomHand();

    yield* dispatchAction(dispatch, { _tag: "HandDealt", cards: hand });
    yield* dispatchAction(dispatch, { _tag: "ActivePlayerChanged", playerIndex: 0 });

    yield* Effect.sleep("800 millis");
    yield* playRandomCard(dispatch, 0);

    yield* Effect.sleep("1200 millis");
    yield* dispatchAction(dispatch, { _tag: "ActivePlayerChanged", playerIndex: 1 });

    for (const playerIndex of opponentPlayers) {
      yield* playRandomCard(dispatch, playerIndex);
      const nextPlayer = playerIndex < 3 ? ((playerIndex + 1) as PlayerIndex) : 0;
      yield* dispatchAction(dispatch, { _tag: "ActivePlayerChanged", playerIndex: nextPlayer });

      if (playerIndex < 3) {
        yield* Effect.sleep("1500 millis");
      }
    }

    yield* Effect.sleep("8000 millis");
    yield* dispatchAction(dispatch, {
      _tag: "TrickScored",
      scoreDelta: 7,
      tricksDelta: 1,
    });
    yield* dispatchAction(dispatch, { _tag: "ActivePlayerChanged", playerIndex: null });
  });
}
