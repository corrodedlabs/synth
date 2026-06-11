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
  actingHostEmail,
  canExposeTrump,
  gameReducer,
  initialGameModel,
  isLegalPlay,
} from "./GameModel";
import { GameSession, storedActiveMatch } from "./GameSession";
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
  private dragControls: DragControls;
  // Phones render cards larger; applied to the hand fan and played cards.
  private cardScale = 1;
  private ui: UiOverlay;
  private sound = new SoundService();
  private session: GameSession | null = null;
  private model: GameModel = initialGameModel;
  private mockFiber: Fiber.RuntimeFiber<void, never> | null = null;
  private mockMode = false;

  private scoreElement: HTMLElement | null = null;
  private tricksElement: HTMLElement | null = null;
  private matchElement: HTMLElement | null = null;

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
        canPlay: (card) => this.cardIsPlayable(card),
        tryPlay: (card) => this.tryPlayCard(card),
      }
    );

    this.ui = new UiOverlay({
      onCreate: (name) => this.createTable(name),
      onBrowse: (name) => this.browseTables(name),
      onJoin: (roomName) => this.session?.join(roomName),
      onJoinLink: (name, roomName) => this.joinTable(name, roomName),
      onEmote: (emote) => this.session?.sendEmote(emote),
      onStandings: (name) => this.showStandings(name),
      onRefreshRooms: () => this.session?.refreshRooms(),
      onAddBot: () => this.session?.addBot(),
      onStartGame: () => this.session?.startGame(),
      onLeave: () => this.session?.leaveRoom(),
      onKick: (member) => this.session?.kick(member),
      onBid: (bid) => this.session?.submitBid(bid),
      onPass: () => this.session?.submitBid("pass"),
      onTrump: (suit) => this.session?.chooseTrump(suit),
      onExpose: () => this.session?.exposeTrump(),
      onNextHand: () => this.session?.nextHand(),
      onLeaveMatch: () => this.leaveMatch(),
      onReplaceBot: () => this.session?.resolveAbandon("replace"),
      onEndMatch: () => this.session?.resolveAbandon("close"),
      onPlayAgain: () => window.location.reload(),
    });

    this.scoreElement = document.getElementById("score-value");
    this.tricksElement = document.getElementById("tricks-value");
    this.matchElement = document.getElementById("match-value");
    this.sound.bindToggle(document.getElementById("sound-button"));

    this.applyViewportLayout();
    window.addEventListener("resize", () => {
      this.applyViewportLayout();
      this.positionSeatLabels();
    });
  }

  // Portrait/narrow screens get a tighter fan of larger cards so ranks
  // stay readable under a thumb.
  private applyViewportLayout() {
    const portrait = window.innerWidth < window.innerHeight;
    this.cardScale = portrait ? 1.18 : 1;
    this.hand.setWidthScale(portrait ? 0.78 : 1);
    this.hand.setCardScale(this.cardScale);
    this.playArea.setCardScale(this.cardScale);
    // re-laying out the fan flattens a raised card — it must not stay armed
    this.dragControls.clearSelection();
  }

  private cardIsPlayable(card: Card): boolean {
    if (this.mockMode) return true;
    const cardId = card.mesh.userData.modelId as string | undefined;
    const model = cardId ? this.model.hand.find((handCard) => handCard.id === cardId) : undefined;
    return model !== undefined && isLegalPlay(this.model, model);
  }

  // While the server waits on our play, unplayable cards fade; any other
  // time the whole hand sits at full strength.
  private renderLegality() {
    const playing = !this.mockMode && this.model.pendingRequest?.kind === "play";
    for (const card of this.hand.getCards()) {
      card.setDimmed(playing && !this.cardIsPlayable(card));
    }
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
          ? { x: 0, y: compact ? -78 : -96 } // clear of the larger phone trick cards
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

  // An invite link was opened: any start screen shown from here on leads
  // with that table (set even when a rejoin runs first, so its fallback
  // start screen still carries the invite).
  public setJoinTarget(roomName: string) {
    this.ui.setJoinTarget(roomName);
  }

  public prepareJoinLink(roomName: string) {
    this.ui.setJoinTarget(roomName);
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

  // Page load found an interrupted match in localStorage: reconnect under
  // the exact email that holds the seat and ask for the snapshot.
  public rejoinMatch(playerName: string, email: string) {
    this.startSession(playerName, "rejoin", undefined, email);
  }

  // An invite link: connect and sit straight down at the named table.
  public joinTable(playerName: string, roomName: string) {
    this.startSession(playerName, "join", roomName);
  }

  // The standings panel: ask over the live session, or connect for it.
  public showStandings(playerName: string) {
    if (this.session) this.session.requestLeaderboard();
    else this.startSession(playerName, "standings");
  }

  private startSession(
    playerName: string,
    intent: "create-room" | "browse-rooms" | "rejoin" | "join" | "standings",
    joinRoom?: string,
    rejoinEmail?: string
  ) {
    if (this.session) {
      // already connected (e.g. browsing) — just issue the new intent
      if (intent === "create-room") this.session.createRoom();
      else if (intent === "join" && joinRoom) this.session.join(joinRoom);
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
        emote: (seat, emote) => this.showEmote(seat, emote),
        leaderboard: (rows) => this.ui.showLeaderboard(rows),
        seatAbandoned: (name) => this.ui.showAbandonPanel(name),
        seatResolved: () => {
          this.ui.hideAbandonPanel();
          // the deal-button duty may just have moved to (or away from) us
          if (this.model.phase === "hand-finished") {
            this.ui.setNextHandRole(
              actingHostEmail(this.model.members) === (this.session?.email ?? "")
            );
          }
        },
      },
      playerName,
      rejoinEmail
    );

    const session = this.session;
    Effect.runFork(
      Effect.gen(this, function* () {
        yield* this.loadTextures();
        session.start(intent, joinRoom);
      })
    );
  }

  // Float an emote bubble by its sender's seat: opponents at their enso
  // markers, our own from the bottom edge (clear of the bid/result panels).
  private showEmote(seat: PlayerIndex, emote: string) {
    const canvas = this.sceneManager.renderer.domElement;
    const screen =
      seat === 0
        ? { x: canvas.clientWidth / 2, y: canvas.clientHeight - 150 }
        : this.playArea.markerScreenPosition(seat, canvas);
    if (!screen) return;
    this.sound.emote();
    this.ui.showEmoteAt(screen.x, screen.y, emote);
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

  // The connection died while a match of ours is still stored (a deploy,
  // a server blip, flaky wifi): keep trying to rejoin every few seconds.
  // The loop ends by itself — a successful snapshot restores the table,
  // and a dead match answers no-running-game, which clears the stored key.
  private scheduleRejoinRetry() {
    const interrupted = storedActiveMatch();
    if (!interrupted) return;
    this.ui.status("Connection lost — trying to rejoin…");
    window.setTimeout(() => {
      if (this.session === null && storedActiveMatch() !== null) {
        this.rejoinMatch(interrupted.name, interrupted.email);
      }
    }, 3000);
  }

  // Walk away from a running match: tell the server (an explicit leave
  // aborts for everyone at once — no reconnect grace), close our socket,
  // and reset to the start screen. The next create/browse builds a fresh
  // session.
  public leaveMatch() {
    if (!this.session) return;
    this.session.leaveGame();
    this.session.destroy();
    this.session = null;
    this.dispatch({ _tag: "RoomLeft" });
    this.ui.status("You left the match.");
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

  // visual state of the rendered hand, for the debug bridge and UI tests
  public handCardStates(): Array<{ id: string; dimmed: boolean }> {
    return this.hand.getCards().map((card) => ({
      id: (card.mesh.userData.modelId as string) ?? "",
      dimmed: card.isDimmed(),
    }));
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
        if (action.phase === "idle") {
          // only dead sessions dispatch idle (connect failure, server gone):
          // drop ours so the start screen's buttons build a fresh one, and
          // clear the now-unrefreshable table list
          if (this.session) {
            this.session.destroy();
            this.session = null;
          }
          this.ui.setLeaveMatchVisible(false);
          this.ui.resetRoomBrowser();
          this.ui.showStartScreen();
          this.scheduleRejoinRetry();
        }
        break;

      case "RoomEntered":
        this.ui.updateLobby(this.model, this.session?.email ?? "");
        break;

      case "MembersChanged":
        // mid-match this is a seat replacement, not a lobby change
        if (this.model.phase === "lobby") {
          this.ui.updateLobby(this.model, this.session?.email ?? "");
        }
        break;

      case "RoomLeft":
        // tear down any in-progress game (aborted games land here mid-hand
        // or while the between-hands result panel is up)
        this.dragControls.clearSelection();
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
        this.ui.hideResult();
        this.ui.hideAbandonPanel();
        this.ui.setLeaveMatchVisible(false);
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
        // a live table always offers a way out (not in ?mock=1 — no server)
        this.ui.setLeaveMatchVisible(this.session !== null);
        this.sound.deal(action.cards.length);
        action.cards.forEach((card) => this.hand.addCard(this.createCard(card)));
        this.renderLegality();
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
        this.renderLegality();
        break;

      case "RequestCleared":
        this.ui.hideBidPanel();
        this.ui.hideTrumpPanel();
        this.ui.setExposeVisible(false);
        this.dragControls.clearSelection();
        this.renderLegality();
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

      case "HandFinished":
        // the hand's card points are final; the panel waits for HandResult,
        // which carries the authoritative verdict and the match score
        this.renderScore();
        this.ui.hideBidPanel();
        this.ui.hideTrumpPanel();
        this.ui.setExposeVisible(false);
        // no turn broadcast follows the last trick, so the marker of the
        // hand's final player would keep pulsing through the result panel
        this.playArea.clearActivePlayer();
        break;

      case "HandResult": {
        this.renderScore(); // match HUD segment just changed
        // when a team has reached the target, match-over is already in
        // flight — its panel and fanfare replace the between-hands ones
        if (
          this.model.matchUs >= this.model.matchTarget ||
          this.model.matchThem >= this.model.matchTarget
        ) {
          break;
        }
        const bidderOurs = action.bidder === 0 || action.bidder === 2;
        const bidderPoints = bidderOurs ? this.model.score : this.model.theirScore;
        const bidderName = action.bidder === 0 ? "Your" : `${this.seatName(action.bidder)}'s`;
        const detail =
          `${bidderName} team bid ${action.bid} and took ${bidderPoints} — ${action.made ? "made it" : "set"}\n` +
          `Match: ${this.model.matchUs} — ${this.model.matchThem} (first to ${this.model.matchTarget})`;
        this.sound.result(bidderOurs ? action.made : !action.made);
        this.ui.status("");
        this.ui.showHandResult(
          this.model.score,
          this.model.theirScore,
          detail,
          actingHostEmail(this.model.members) === (this.session?.email ?? "")
        );
        break;
      }

      case "HandReset":
        // the next deal is landing: clear the table but keep the seats,
        // their name labels, and the match HUD
        this.dragControls.clearSelection();
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
        this.ui.hideResult();
        this.renderScore();
        break;

      case "MatchOver": {
        this.sound.result(action.winner === "us");
        this.ui.status("");
        // the match is decided — the panel's Play again takes it from here
        this.ui.setLeaveMatchVisible(false);
        this.ui.showMatchOver(action.us, action.them, action.winner === "us", action.hands);
        break;
      }

      case "SnapshotRestored": {
        // a reconnect: rebuild the whole scene from the restored model;
        // any pending request/result is re-dispatched right after and
        // brings its own panel back up
        this.dragControls.clearSelection();
        for (const card of [...this.hand.getCards()]) {
          this.hand.removeCard(card);
          this.sceneManager.scene.remove(card.mesh);
        }
        this.playArea.clearTable();
        this.playArea.clearActivePlayer();
        this.ui.hideLobby();
        this.ui.hideBidPanel();
        this.ui.hideTrumpPanel();
        this.ui.setExposeVisible(false);
        this.ui.hideResult();
        this.ui.hideAbandonPanel();
        this.model.hand.forEach((card) => this.hand.addCard(this.createCard(card)));
        for (const played of this.model.playedCards) {
          this.playCard(played.card, played.playerIndex);
        }
        if (this.model.activePlayer !== null) {
          this.playArea.setActivePlayer(this.model.activePlayer);
        }
        this.ui.setTrump(this.model.trumpSuit, this.model.trumpExposed);
        this.renderScore();
        this.positionSeatLabels();
        this.ui.setLeaveMatchVisible(this.session !== null);
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

    // table cards match the hand's phone-friendly sizing, never dimmed
    card.setDimmed(false);
    card.mesh.scale.setScalar(this.cardScale);
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
    if (this.matchElement) {
      this.matchElement.textContent = `${this.model.matchUs} — ${this.model.matchThem}`;
    }
  }
}
