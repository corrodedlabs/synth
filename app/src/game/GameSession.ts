import { Effect, Fiber, Queue } from "effect";
import { cardPoints, ServerEvent } from "../net/protocol";
import { connectSocket, GameSocket, SocketError } from "../net/SocketService";
import {
  CardModel,
  GameAction,
  GameModel,
  PlayerIndex,
  Suit,
  canExposeTrump,
  isLegalPlay,
} from "./GameModel";

export interface SessionCallbacks {
  readonly dispatch: (action: GameAction) => void;
  readonly getModel: () => GameModel;
  readonly status: (text: string) => void;
}

const SERVER_URL = "ws://localhost:8081/test";

// Bots answer instantly; these pauses pace the rendered game so turns stay readable.
const BID_PAUSE = "400 millis";
const CARD_PAUSE = "700 millis";
const TRICK_PAUSE = "1400 millis";

export class GameSession {
  private socket: GameSocket | null = null;
  private fiber: Fiber.RuntimeFiber<void, SocketError> | null = null;
  private myServerIndex = 0;
  private cardsInTrick = 0;
  private trickPoints = 0;
  private pointsTaken = [0, 0, 0, 0]; // accumulated per view index
  private locallyPlayed = new Set<string>();
  private chosenTrump: Suit | null = null;

  readonly email: string;
  readonly roomName: string;

  constructor(private callbacks: SessionCallbacks) {
    const id = Math.random().toString(36).slice(2, 8);
    this.email = `player-${id}@game.local`;
    this.roomName = `room-${id}`;
  }

  start() {
    this.fiber = Effect.runFork(
      this.program().pipe(
        Effect.catchAll((error) =>
          Effect.sync(() => {
            this.callbacks.status(error.message);
            this.callbacks.dispatch({ _tag: "PhaseChanged", phase: "idle" });
          })
        )
      )
    );
  }

  destroy() {
    if (this.fiber) {
      Effect.runFork(Fiber.interrupt(this.fiber));
      this.fiber = null;
    }
    this.socket?.close();
    this.socket = null;
  }

  // --- player input (called from the UI when the server is waiting on us) ---

  submitBid(bid: number | "pass") {
    const model = this.callbacks.getModel();
    if (model.pendingRequest?.kind !== "bid" || !this.socket) return;
    this.socket.send({ _tag: "PutBid", email: this.email, bid });
    this.callbacks.dispatch({ _tag: "RequestCleared" });
  }

  chooseTrump(suit: Suit) {
    const model = this.callbacks.getModel();
    if (model.pendingRequest?.kind !== "trump" || !this.socket) return;
    this.chosenTrump = suit;
    this.socket.send({ _tag: "SelectTrump", email: this.email, suit });
    this.callbacks.dispatch({ _tag: "RequestCleared" });
  }

  playCard(card: CardModel): boolean {
    const model = this.callbacks.getModel();
    if (!this.socket || !isLegalPlay(model, card)) return false;
    this.socket.send({ _tag: "PlayCard", email: this.email, card });
    this.locallyPlayed.add(card.id);
    // Render our own play immediately; the server's broadcast is deduped below.
    this.callbacks.dispatch({ _tag: "CardPlayed", card, playerIndex: 0 });
    this.callbacks.dispatch({ _tag: "RequestCleared" });
    return true;
  }

  exposeTrump() {
    const model = this.callbacks.getModel();
    if (!this.socket || !canExposeTrump(model)) return;
    this.socket.send({ _tag: "ExposeTrump", email: this.email });
    this.callbacks.dispatch({ _tag: "RequestCleared" });
  }

  // --- seat translation: server index <-> view index (0 = us at the bottom) ---

  private toView(serverIndex: number): PlayerIndex {
    return ((serverIndex - this.myServerIndex + 4) % 4) as PlayerIndex;
  }

  private toServer(viewIndex: number): number {
    return (viewIndex + this.myServerIndex) % 4;
  }

  // --- session program ---

  private program(): Effect.Effect<void, SocketError> {
    return Effect.gen(this, function* () {
      const { dispatch } = this.callbacks;
      dispatch({ _tag: "PhaseChanged", phase: "connecting" });
      this.callbacks.status("Joining a table…");

      const socket = yield* connectSocket(SERVER_URL);
      this.socket = socket;

      socket.send({ _tag: "ConnectUser", email: this.email, picUrl: "none" });
      // connect-user is acked with an unparseable #<void>; give the server a
      // beat so the user exists before the room is created.
      yield* Effect.sleep("300 millis");

      socket.send({ _tag: "MakeRoom", hostEmail: this.email, roomName: this.roomName });
      yield* this.waitFor(socket, (event) => event._tag === "RoomCreated");

      let members: readonly string[] = [];
      for (let expected = 2; expected <= 4; expected++) {
        socket.send({ _tag: "AddBot", roomName: this.roomName });
        const event = yield* this.waitFor(
          socket,
          (e): e is ServerEvent & { _tag: "RoomMembers" } =>
            e._tag === "RoomMembers" && e.members.length === expected
        );
        members = event.members;
        this.callbacks.status(`Bots seated: ${expected - 1} of 3`);
      }

      this.myServerIndex = members.indexOf(this.email);
      if (this.myServerIndex < 0) {
        return yield* Effect.fail(new SocketError("could not find our seat in the room"));
      }

      socket.send({ _tag: "StartGame", roomName: this.roomName });
      this.callbacks.status("Dealing…");

      for (;;) {
        const event = yield* Queue.take(socket.events);
        const finished = yield* this.handleEvent(event);
        if (finished) return;
      }
    });
  }

  private waitFor<E extends ServerEvent>(
    socket: GameSocket,
    predicate: (event: ServerEvent) => event is E
  ): Effect.Effect<E>;
  private waitFor(
    socket: GameSocket,
    predicate: (event: ServerEvent) => boolean
  ): Effect.Effect<ServerEvent>;
  private waitFor(socket: GameSocket, predicate: (event: ServerEvent) => boolean) {
    return Effect.gen(this, function* () {
      for (;;) {
        const event = yield* Queue.take(socket.events);
        if (predicate(event)) return event;
      }
    });
  }

  // winner: the seat that takes the trick (the next leader), when known.
  private finishTrickIfComplete(winner: PlayerIndex | null): Effect.Effect<void> {
    return Effect.gen(this, function* () {
      if (this.cardsInTrick < 4) return;
      yield* Effect.sleep(TRICK_PAUSE);
      this.callbacks.dispatch({ _tag: "TrickEnded", winner, points: this.trickPoints });
      if (winner !== null) this.pointsTaken[winner] += this.trickPoints;
      this.cardsInTrick = 0;
      this.trickPoints = 0;
    });
  }

  private handleEvent(event: ServerEvent): Effect.Effect<boolean> {
    const { dispatch } = this.callbacks;
    return Effect.gen(this, function* () {
      switch (event._tag) {
        case "HandDealt":
          dispatch({ _tag: "HandDealt", cards: event.cards });
          return false;

        case "Turn":
          // After a complete trick, the next leader is the trick's winner.
          yield* this.finishTrickIfComplete(this.toView(event.playerIndex));
          dispatch({ _tag: "ActivePlayerChanged", playerIndex: this.toView(event.playerIndex) });
          return false;

        case "BidRequested":
          dispatch({ _tag: "PhaseChanged", phase: "bidding" });
          dispatch({
            _tag: "RequestReceived",
            request: { kind: "bid", currentBid: event.currentBid },
          });
          return false;

        case "BidPlaced":
          dispatch({ _tag: "BidPlaced", playerIndex: this.toView(event.playerIndex), bid: event.bid });
          yield* Effect.sleep(BID_PAUSE);
          return false;

        case "BidResult":
          dispatch({ _tag: "BidWon", playerIndex: this.toView(event.playerIndex), bid: event.bid });
          return false;

        case "ChooseTrumpRequested":
          dispatch({ _tag: "RequestReceived", request: { kind: "trump" } });
          return false;

        case "TrumpSelected":
          dispatch({ _tag: "TrumpSet", suit: this.chosenTrump, exposed: false });
          return false;

        case "CardPlayed": {
          if (event.card === "expose-trump") {
            dispatch({ _tag: "TrumpSet", suit: null, exposed: true });
            return false;
          }
          this.cardsInTrick++;
          this.trickPoints += cardPoints(event.card);
          if (this.locallyPlayed.delete(event.card.id)) return false; // already rendered
          dispatch({ _tag: "CardPlayed", card: event.card, playerIndex: this.toView(event.playerIndex) });
          yield* Effect.sleep(CARD_PAUSE);
          return false;
        }

        case "PlayRequested":
          dispatch({ _tag: "PhaseChanged", phase: "playing" });
          if (event.trumpSuit !== null) {
            dispatch({ _tag: "TrumpSet", suit: event.trumpSuit, exposed: true });
          }
          dispatch({
            _tag: "RequestReceived",
            request: { kind: "play", firstSuit: event.firstSuit, trumpSuit: event.trumpSuit },
          });
          return false;

        case "PointsWon": {
          const points = [0, 1, 2, 3].map((view) => event.points.get(this.toServer(view)) ?? 0);
          // No turn broadcast follows the last trick — derive its winner from
          // the gap between the final totals and what we attributed so far.
          const lastWinner =
            this.trickPoints > 0
              ? (([0, 1, 2, 3] as const).find(
                  (view) => points[view] - this.pointsTaken[view] === this.trickPoints
                ) ?? null)
              : null;
          yield* this.finishTrickIfComplete(lastWinner);
          dispatch({ _tag: "GameFinished", points });
          return true;
        }

        case "ServerError":
          this.callbacks.status(`Server rejected that: ${event.message}`);
          return false;

        case "Disconnected":
          this.callbacks.status("Disconnected from the game server.");
          dispatch({ _tag: "PhaseChanged", phase: "idle" });
          return true;

        default:
          return false;
      }
    });
  }
}
