import { Effect, Fiber } from "effect";
import { DragControls } from "../interaction/DragControls";
import { SceneManager } from "../scene/SceneManager";
import { Lighting } from "../scene/Lighting";
import { Table } from "../scene/Table";
import { SoundService } from "../utils/Sound";
import { ZenCardTextures } from "../utils/ZenCardTextures";
import { SEAT_NAMES, UiOverlay } from "../ui/UiOverlay";
import { Card } from "./Card";
import {
  GameAction,
  GameModel,
  CardModel,
  PlayerIndex,
  canExposeTrump,
  gameReducer,
  initialGameModel,
  isLegalPlay,
} from "./GameModel";
import { GameSession } from "./GameSession";
import { Hand } from "./Hand";
import { mockGameProgram } from "./MockGameProgram";
import { PlayArea } from "./PlayArea";

export class GameState {
  private sceneManager: SceneManager;
  private lighting: Lighting;
  private table: Table;
  private hand: Hand;
  private playArea: PlayArea;
  private zenTextures: ZenCardTextures;
  // @ts-ignore kept alive for event listeners
  private dragControls: DragControls;
  private ui: UiOverlay;
  private sound = new SoundService();
  private session: GameSession | null = null;
  private model: GameModel = initialGameModel;
  private mockFiber: Fiber.RuntimeFiber<void, never> | null = null;
  private mockMode = false;

  private scoreElement: HTMLElement | null = null;
  private tricksElement: HTMLElement | null = null;

  constructor(sceneManager: SceneManager) {
    this.sceneManager = sceneManager;

    this.lighting = new Lighting(this.sceneManager.scene);
    this.table = new Table(this.sceneManager.scene);
    this.hand = new Hand(this.sceneManager.scene, this.sceneManager.camera);
    this.playArea = new PlayArea(this.sceneManager.scene, this.sceneManager.camera);
    this.zenTextures = new ZenCardTextures();

    this.dragControls = new DragControls(
      this.sceneManager.camera,
      this.sceneManager.scene,
      this.sceneManager.renderer.domElement,
      this.hand,
      {
        canDrag: () => this.mockMode || this.model.pendingRequest?.kind === "play",
        tryPlay: (card) => this.tryPlayCard(card),
      }
    );

    this.ui = new UiOverlay({
      onCreate: (name) => this.createTable(name),
      onBrowse: (name) => this.browseTables(name),
      onJoin: (roomName) => this.session?.join(roomName),
      onRefreshRooms: () => this.session?.refreshRooms(),
      onAddBot: () => this.session?.addBot(),
      onStartGame: () => this.session?.startGame(),
      onLeave: () => this.session?.leaveRoom(),
      onKick: (member) => this.session?.kick(member),
      onBid: (bid) => this.session?.submitBid(bid),
      onPass: () => this.session?.submitBid("pass"),
      onTrump: (suit) => this.session?.chooseTrump(suit),
      onExpose: () => this.session?.exposeTrump(),
      onPlayAgain: () => window.location.reload(),
    });

    this.scoreElement = document.getElementById("score-value");
    this.tricksElement = document.getElementById("tricks-value");
    this.sound.bindToggle(document.getElementById("sound-button"));

    this.applyViewportLayout();
    window.addEventListener("resize", () => {
      this.applyViewportLayout();
      this.positionSeatLabels();
    });
  }

  // Portrait/narrow screens get a tighter hand fan.
  private applyViewportLayout() {
    const portrait = window.innerWidth < window.innerHeight;
    this.hand.setWidthScale(portrait ? 0.82 : 1);
  }

  // "You" for ourselves, the real player name (set at deal time) otherwise.
  private seatName(playerIndex: PlayerIndex): string {
    if (playerIndex === 0) return "You";
    return this.model.seatNames?.[playerIndex] ?? SEAT_NAMES[playerIndex];
  }

  // Pin opponent name labels under their enso markers.
  private positionSeatLabels() {
    const canvas = this.sceneManager.renderer.domElement;
    for (const seat of [1, 2, 3] as const) {
      const label = document.getElementById(`seat-label-${seat}`);
      if (!label) continue;
      const names = this.model.seatNames;
      if (!names) {
        label.classList.remove("visible");
        continue;
      }
      const screen = this.playArea.markerScreenPosition(seat, canvas);
      if (!screen) continue;
      label.textContent = names[seat];
      if (seat === 2) {
        const tag = document.createElement("span");
        tag.className = "partner-tag";
        tag.textContent = "your partner";
        label.append(tag);
      }
      // Keep clear of the played-card cross: partner's label sits above their
      // marker, side labels are pushed outward and down. Tighter on phones.
      const compact = window.innerWidth < 700;
      const out = compact ? 38 : 64;
      const down = compact ? 34 : 54;
      const offset =
        seat === 2
          ? { x: 0, y: compact ? -64 : -96 }
          : seat === 1
            ? { x: out, y: down }
            : { x: -out, y: down };
      label.style.left = `${screen.x + offset.x}px`;
      label.style.top = `${screen.y + offset.y}px`;
      label.classList.add("visible");
    }
  }

  public showStartScreen() {
    this.ui.showStartScreen();
  }

  public startMock() {
    this.mockMode = true;
    const program = Effect.gen(this, function* () {
      yield* this.loadTextures();
      yield* mockGameProgram((action) => this.dispatch(action));
    });
    this.mockFiber = Effect.runFork(program);
  }

  public createTable(playerName: string) {
    this.startSession(playerName, "create-room");
  }

  public browseTables(playerName: string) {
    this.startSession(playerName, "browse-rooms");
  }

  private startSession(playerName: string, intent: "create-room" | "browse-rooms") {
    if (this.session) {
      // already connected (e.g. browsing) — just issue the new intent
      if (intent === "create-room") this.session.createRoom();
      else this.session.refreshRooms();
      return;
    }
    this.mockMode = false;
    this.session = new GameSession(
      {
        dispatch: (action) => this.dispatch(action),
        getModel: () => this.model,
        status: (text) => this.ui.status(text),
        rooms: (rooms) => this.ui.showRooms(rooms),
      },
      playerName
    );

    const session = this.session;
    Effect.runFork(
      Effect.gen(this, function* () {
        yield* this.loadTextures();
        session.start(intent);
      })
    );
  }

  public getModel(): GameModel {
    return this.model;
  }

  // sound attempt counters + mute state, for the debug bridge and UI tests
  public soundState(): Record<string, number | boolean> {
    return { ...this.sound.played, muted: this.sound.isMuted() };
  }

  public getSession(): GameSession | null {
    return this.session;
  }

  // Plays a card from our hand by model id; used by the debug bridge.
  public playCardById(cardId: string): boolean {
    const card = this.model.hand.find((handCard) => handCard.id === cardId);
    if (!card || !this.session) return false;
    return this.session.playCard(card);
  }

  public legalCardIds(): string[] {
    return this.model.hand.filter((card) => isLegalPlay(this.model, card)).map((card) => card.id);
  }

  // Screen-space position of a hand card; used by automated UI tests to drive
  // real pointer gestures.
  public cardScreenPosition(cardId: string): { x: number; y: number } | null {
    const card = this.hand.getCards().find((handCard) => handCard.mesh.userData.modelId === cardId);
    if (!card) return null;
    const projected = card.mesh.position.clone().project(this.sceneManager.camera);
    const canvas = this.sceneManager.renderer.domElement;
    return {
      x: ((projected.x + 1) / 2) * canvas.clientWidth,
      y: ((1 - projected.y) / 2) * canvas.clientHeight,
    };
  }

  public update() {
    this.sceneManager.update();
  }

  public destroy() {
    if (this.mockFiber) {
      Effect.runFork(Fiber.interrupt(this.mockFiber));
      this.mockFiber = null;
    }
    this.session?.destroy();
    this.session = null;
  }

  public getLighting(): Lighting {
    return this.lighting;
  }

  public getHand(): Hand {
    return this.hand;
  }

  public getTable(): Table {
    return this.table;
  }

  private loadTextures() {
    return Effect.tryPromise({
      try: () => this.zenTextures.waitForLoad(),
      catch: (error) => error,
    }).pipe(Effect.catchAll(() => Effect.void));
  }

  // Called by DragControls when a card is dropped on the play area.
  private tryPlayCard(card: Card): boolean {
    if (this.mockMode) {
      this.hand.removeCard(card);
      this.playArea.playCard(card, 0);
      return true;
    }
    const cardId = card.mesh.userData.modelId as string | undefined;
    const model = cardId ? this.model.hand.find((handCard) => handCard.id === cardId) : undefined;
    if (!model || !this.session) return false;
    return this.session.playCard(model);
  }

  public dispatch(action: GameAction) {
    this.model = gameReducer(this.model, action);
    this.renderAction(action);
  }

  private renderAction(action: GameAction) {
    switch (action._tag) {
      case "PhaseChanged":
        if (action.phase === "connecting") this.ui.status("Connecting…");
        if (action.phase === "idle") this.ui.showStartScreen();
        break;

      case "RoomEntered":
      case "MembersChanged":
        this.ui.updateLobby(this.model, this.session?.email ?? "");
        break;

      case "RoomLeft":
        // tear down any in-progress game (aborted games land here mid-hand)
        for (const card of [...this.hand.getCards()]) {
          this.hand.removeCard(card);
          this.sceneManager.scene.remove(card.mesh);
        }
        this.playArea.clearTable();
        this.playArea.clearActivePlayer();
        this.ui.setTrump(null, false);
        this.ui.hideBidPanel();
        this.ui.hideTrumpPanel();
        this.ui.setExposeVisible(false);
        this.renderScore();
        this.ui.hideLobby();
        this.ui.showStartScreen();
        this.positionSeatLabels(); // hides them (seatNames reset)
        break;

      case "SeatNamesSet":
        this.positionSeatLabels();
        break;

      case "HandDealt":
        this.ui.hideLobby();
        this.sound.deal(action.cards.length);
        action.cards.forEach((card) => this.hand.addCard(this.createCard(card)));
        break;

      case "CardPlayed":
        this.sound.cardPlay();
        this.playCard(action.card, action.playerIndex);
        break;

      case "ActivePlayerChanged":
        if (action.playerIndex === null) {
          this.playArea.clearActivePlayer();
        } else {
          this.playArea.setActivePlayer(action.playerIndex);
        }
        break;

      case "TrickScored":
        this.renderScore();
        break;

      case "BidPlaced": {
        const seat = this.seatName(action.playerIndex);
        this.sound.bidTick();
        this.ui.status(action.bid === "pass" ? `${seat} pass${action.playerIndex === 0 ? "" : "es"}` : `${seat} bid${action.playerIndex === 0 ? "" : "s"} ${action.bid}`, true);
        break;
      }

      case "BidWon":
        this.ui.status(`${this.seatName(action.playerIndex)} won the bid at ${action.bid}`, true);
        break;

      case "TrumpSet":
        // suit:null + exposed is the one-time exposure broadcast; the later
        // TrumpSet dispatches from play requests repeat the suit
        if (action.exposed && action.suit === null) this.sound.trumpReveal();
        this.ui.setTrump(this.model.trumpSuit, this.model.trumpExposed);
        if (this.model.trumpExposed) this.ui.status("Trump is revealed", true);
        break;

      case "RequestReceived":
        this.sound.yourTurn();
        switch (action.request.kind) {
          case "bid":
            this.ui.showBidPanel(action.request.minBid, this.model.bidsPlaced === 0);
            break;
          case "trump":
            this.ui.showTrumpPanel();
            this.ui.status("You won the bid — choose trump");
            break;
          case "play":
            this.ui.status("Your turn");
            this.ui.setExposeVisible(canExposeTrump(this.model));
            break;
        }
        break;

      case "RequestCleared":
        this.ui.hideBidPanel();
        this.ui.hideTrumpPanel();
        this.ui.setExposeVisible(false);
        break;

      case "TrickCleared":
        this.playArea.clearTable();
        break;

      case "TrickEnded": {
        this.playArea.clearTable(action.winner);
        this.renderScore();
        if (action.winner !== null) {
          this.sound.trickSweep();
          const seat = this.seatName(action.winner);
          const taker = action.winner === 0 ? "You take" : `${seat} takes`;
          this.ui.status(
            action.points > 0 ? `${taker} the trick (+${action.points})` : `${taker} the trick`,
            true
          );
        }
        break;
      }

      case "GameFinished": {
        const points = action.points;
        const ourPoints = points[0] + points[2];
        const theirPoints = points[1] + points[3];
        this.renderScore();
        let detail = "";
        let weWon = ourPoints > theirPoints;
        if (this.model.finalBid !== null && this.model.bidWinner !== null) {
          const bidderOurs = this.model.bidWinner === 0 || this.model.bidWinner === 2;
          const bidderPoints = bidderOurs ? ourPoints : theirPoints;
          const made = bidderPoints >= this.model.finalBid;
          const bidderName = this.model.bidWinner === 0 ? "Your" : `${this.seatName(this.model.bidWinner)}'s`;
          detail = `${bidderName} team bid ${this.model.finalBid} and took ${bidderPoints} — ${made ? "made it" : "set"}`;
          weWon = bidderOurs ? made : !made;
        }
        this.sound.result(weWon);
        this.ui.status("");
        this.ui.showResult(ourPoints, theirPoints, detail);
        break;
      }
    }
  }

  private createCard(cardModel: CardModel) {
    const frontTexture = this.zenTextures.getCardTexture(cardModel.suit, cardModel.rank);
    const backTexture = this.zenTextures.getBackTexture();
    const card = new Card(cardModel.suit, cardModel.rank, frontTexture, backTexture);
    card.mesh.userData.modelId = cardModel.id;
    return card;
  }

  private playCard(cardModel: CardModel, playerIndex: PlayerIndex) {
    const card = this.findHandCard(cardModel) ?? this.createCard(cardModel);

    if (this.hand.getCards().includes(card)) {
      this.hand.removeCard(card);
    } else {
      const startPosition = this.startPositionForPlayer(playerIndex);
      card.setPosition(startPosition.x, startPosition.y, startPosition.z, true);
      this.sceneManager.scene.add(card.mesh);
    }

    this.playArea.playCard(card, playerIndex);
  }

  private findHandCard(cardModel: CardModel): Card | null {
    return this.hand.getCards().find((card) => card.mesh.userData.modelId === cardModel.id) ?? null;
  }

  private startPositionForPlayer(playerIndex: PlayerIndex) {
    switch (playerIndex) {
      case 0:
        return { x: 0, y: 0.5, z: 2.5 };
      case 1:
        return { x: 2.9, y: 0.5, z: -0.3 };
      case 2:
        return { x: 0, y: 0.5, z: -2.4 };
      case 3:
        return { x: -2.9, y: 0.5, z: -0.3 };
    }
  }

  private renderScore() {
    if (this.scoreElement) {
      this.scoreElement.textContent = `${this.model.score} — ${this.model.theirScore}`;
    }
    if (this.tricksElement) {
      this.tricksElement.textContent = `${this.model.tricks} — ${this.model.theirTricks}`;
    }
  }
}
