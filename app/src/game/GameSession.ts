import { Effect, Fiber, Queue } from "effect";
import { cardPoints, RoomInfo, ServerEvent } from "../net/protocol";
import { connectSocket, GameSocket, SocketError } from "../net/SocketService";
import { memberDisplayName } from "../ui/UiOverlay";
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
  readonly rooms: (rooms: readonly RoomInfo[]) => void;
}

// What the session should do once the socket is up.
export type SessionIntent = "create-room" | "browse-rooms";

// Server endpoint resolution, most specific wins:
// ?server=ws://… → ?port=9000 (localhost) → VITE_SERVER_URL (production build)
// → ws://localhost:8081/test (dev default).
function serverUrl(): string {
  const params = new URLSearchParams(window.location.search);
  const explicit = params.get("server");
  if (explicit) return explicit;
  const port = params.get("port");
  if (port) return `ws://localhost:${port}/test`;
  const configured = import.meta.env.VITE_SERVER_URL as string | undefined;
  if (configured) return configured;
  return "ws://localhost:8081/test";
}

// Bots answer instantly; these pauses pace the rendered game so turns stay readable.
const BID_PAUSE = "400 millis";
const CARD_PAUSE = "700 millis";
const TRICK_PAUSE = "1400 millis";

// App-level liveness ping so hosting proxies (fly.io, Netlify previews) don't
// cull a quiet lobby socket.
const PING_INTERVAL = "25 seconds";

const MAX_PLAYERS = 4;

export class GameSession {
  private socket: GameSocket | null = null;
  private fiber: Fiber.RuntimeFiber<void, SocketError> | null = null;
  private myServerIndex = 0;
  private inRoom = false;
  private joinedRoomName: string | null = null;
  private cardsInTrick = 0;
  private trickPoints = 0;
  private pointsTaken = [0, 0, 0, 0]; // accumulated per view index
  private locallyPlayed = new Set<string>();
  private chosenTrump: Suit | null = null;

  readonly email: string;
  readonly displayName: string;
  readonly roomName: string;

  constructor(private callbacks: SessionCallbacks, playerName: string) {
    const id = Math.random().toString(36).slice(2, 8);
    const slug =
      playerName.trim().toLowerCase().replace(/[^a-z0-9]+/g, "-").replace(/^-+|-+$/g, "") ||
      "player";
    this.displayName = playerName.trim() || "Player";
    this.email = `${slug}-${id}@game.local`;
    this.roomName = `${this.displayName}'s table #${id.slice(0, 3)}`;
  }

  start(intent: SessionIntent) {
    this.fiber = Effect.runFork(
      this.program(intent).pipe(
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

  // --- lobby input (called from the UI) ---

  createRoom() {
    if (!this.socket || this.inRoom) return;
    this.socket.send({ _tag: "MakeRoom", hostEmail: this.email, roomName: this.roomName });
  }

  refreshRooms() {
    if (!this.socket || this.inRoom) return;
    this.socket.send({ _tag: "GetActiveRooms" });
  }

  join(roomName: string) {
    if (!this.socket || this.inRoom) return;
    this.joinedRoomName = roomName;
    this.socket.send({ _tag: "JoinRoom", roomName, email: this.email });
    this.callbacks.status("Joining the table…");
  }

  leaveRoom() {
    const model = this.callbacks.getModel();
    if (!this.socket || !this.inRoom || model.roomName === null) return;
    this.socket.send({ _tag: "LeaveRoom", roomName: model.roomName, email: this.email });
  }

  kick(targetEmail: string) {
    const model = this.callbacks.getModel();
    if (!this.socket || !model.isHost || model.roomName === null) return;
    if (targetEmail === this.email) return;
    this.socket.send({
      _tag: "KickFromRoom",
      roomName: model.roomName,
      hostEmail: this.email,
      targetEmail,
    });
  }

  addBot() {
    const model = this.callbacks.getModel();
    if (!this.socket || !model.isHost || model.members.length >= MAX_PLAYERS) return;
    this.socket.send({ _tag: "AddBot", roomName: this.roomName });
  }

  startGame() {
    const model = this.callbacks.getModel();
    if (!this.socket || !model.isHost || model.members.length !== MAX_PLAYERS) return;
    this.socket.send({ _tag: "StartGame", roomName: this.roomName });
    this.callbacks.status("Starting…");
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

  // Back to the start screen: reset lobby state and refresh the table list
  // so the user can immediately join or host again on the same connection.
  private exitRoom(message: string) {
    this.inRoom = false;
    this.joinedRoomName = null;
    this.myServerIndex = 0;
    this.callbacks.dispatch({ _tag: "RoomLeft" });
    this.callbacks.status(message);
    this.refreshRooms();
  }

  // --- seat translation: server index <-> view index (0 = us at the bottom) ---

  private toView(serverIndex: number): PlayerIndex {
    return ((serverIndex - this.myServerIndex + 4) % 4) as PlayerIndex;
  }

  private toServer(viewIndex: number): number {
    return (viewIndex + this.myServerIndex) % 4;
  }

  // --- session program ---

  private program(intent: SessionIntent): Effect.Effect<void, SocketError> {
    return Effect.gen(this, function* () {
      const { dispatch } = this.callbacks;
      dispatch({ _tag: "PhaseChanged", phase: "connecting" });
      this.callbacks.status("Connecting…");

      const socket = yield* connectSocket(serverUrl());
      this.socket = socket;

      socket.send({ _tag: "ConnectUser", email: this.email, picUrl: "none" });
      // connect-user is acked with an unparseable #<void>; give the server a
      // beat so the user exists before the room is created.
      yield* Effect.sleep("300 millis");

      // heartbeat — dies with this fiber when the session ends
      yield* Effect.fork(
        Effect.forever(
          Effect.sleep(PING_INTERVAL).pipe(
            Effect.andThen(Effect.sync(() => this.socket?.send({ _tag: "Ping" })))
          )
        )
      );

      if (intent === "create-room") {
        this.createRoom();
      } else {
        this.refreshRooms();
      }

      for (;;) {
        const event = yield* Queue.take(socket.events);
        const finished = yield* this.handleEvent(event);
        if (finished) return;
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
        case "RoomCreated":
          this.inRoom = true;
          this.myServerIndex = 0;
          dispatch({
            _tag: "RoomEntered",
            roomName: this.roomName,
            members: [this.email],
            isHost: true,
          });
          this.callbacks.status("Your table is ready — fill the seats, then start.");
          return false;

        case "ActiveRooms":
          this.callbacks.rooms(event.rooms);
          return false;

        case "RoomMembers": {
          this.myServerIndex = event.members.indexOf(this.email);
          if (this.inRoom) {
            dispatch({ _tag: "MembersChanged", members: event.members });
          } else if (this.myServerIndex >= 0) {
            // our join went through — this broadcast is our welcome
            this.inRoom = true;
            dispatch({
              _tag: "RoomEntered",
              roomName: this.joinedRoomName ?? "table",
              members: event.members,
              isHost: false,
            });
          }
          return false;
        }

        case "GameStarted":
          this.callbacks.status("Dealing…");
          return false;

        case "RoomLeft":
          this.exitRoom("You left the table.");
          return false;

        case "RoomClosed":
          if (this.inRoom) this.exitRoom("The host closed the table.");
          return false;

        case "RemovedFromRoom":
          if (this.inRoom) this.exitRoom("The host removed you from the table.");
          return false;

        case "StartGameFailed":
          this.callbacks.status(
            event.reason === "room-not-ready"
              ? "The table needs 4 players before the game can start."
              : "This game has already started."
          );
          return false;

        case "HandDealt": {
          // First deal: seats are final now, pin everyone's display name.
          const model = this.callbacks.getModel();
          if (model.seatNames === null && model.members.length === 4) {
            const names = [0, 1, 2, 3].map((view) =>
              memberDisplayName(model.members[this.toServer(view)] ?? "")
            );
            dispatch({ _tag: "SeatNamesSet", names });
          }
          dispatch({ _tag: "HandDealt", cards: event.cards });
          return false;
        }

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
          this.callbacks.status(
            event.message.includes("no-such-room")
              ? "That table is no longer open."
              : `Server rejected that: ${event.message}`
          );
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
