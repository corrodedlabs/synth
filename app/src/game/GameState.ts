import { Effect, Fiber } from "effect";
import * as TWEEN from "@tweenjs/tween.js";
import { DragControls } from "../interaction/DragControls";
import { SceneManager } from "../scene/SceneManager";
import { Lighting } from "../scene/Lighting";
import { Table } from "../scene/Table";
import { SoundService } from "../utils/Sound";
import { ZenCardTextures } from "../utils/ZenCardTextures";
import { GameFeel } from "../ui/GameFeel";
import { bidderDelta, handResultCopy, scoreText } from "../ui/HudModel";
import { playerProfile } from "../ui/PlayerProfile";
import { ScoreHud } from "../ui/ScoreHud";
import type { HudTrump } from "../ui/ScoreHud";
import { createPlayerIdentity, SEAT_NAMES, trumpRevealCaption, UiOverlay } from "../ui/UiOverlay";
import { Card } from "./Card";
import { arrangedCardIds } from "./CardArrangement";
import type { ArrangementMode, ArrangementPreset } from "./CardArrangement";
import {
  GameAction,
  GameModel,
  CardModel,
  PlayerIndex,
  Suit,
  actingHostEmail,
  canExposeTrump,
  gameReducer,
  initialGameModel,
  isLegalPlay,
} from "./GameModel";
import { forgetActiveMatch, GameSession, storedActiveMatch } from "./GameSession";
import { Hand } from "./Hand";
import { mockGameProgram } from "./MockGameProgram";
import { GamePacing } from "./GamePacing";
import type { GamePacingProfile } from "./GamePacing";
import { PlayArea } from "./PlayArea";
import { objectScreenBounds, responsiveTableLayout, screenPointOnPlane } from "./TableLayout";
import type { ResponsiveTableLayout } from "./TableLayout";

// How long Play with bots may take to reach a deal before it says something's wrong.
const PLAY_NOW_LIMIT_MS = 15_000;

export class GameState {
  private sceneManager: SceneManager;
  private lighting: Lighting;
  private table: Table;
  private hand: Hand;
  private playArea: PlayArea;
  private pacing = new GamePacing();
  private zenTextures: ZenCardTextures;
  private dragControls: DragControls;
  private tableLayout: ResponsiveTableLayout = responsiveTableLayout(
    window.innerWidth,
    window.innerHeight
  );
  private turnLine = "";
  private ui: UiOverlay;
  private sound = new SoundService();
  private session: GameSession | null = null;
  private model: GameModel = initialGameModel;
  private mockFiber: Fiber.RuntimeFiber<void, never> | null = null;
  private mockMode = false;
  private trumpRevealAnnounced = false;
  private arrangementAvailable = false;
  private arranging = false;
  private arrangementMode: ArrangementMode = "deal";
  private dealOrderIds: string[] = [];

  private scoreElement: HTMLElement | null = null;
  private tricksElement: HTMLElement | null = null;
  private matchElement: HTMLElement | null = null;
  private feel: GameFeel;
  private hud = new ScoreHud();
  // What the HUD is showing, which can lag the model while something flies to it.
  private shownPoints = { us: 0, them: 0 };
  private shownMatch = { us: 0, them: 0 };
  private heldMatch: { us: number; them: number } | null = null;
  private heldTrump: HudTrump | null = null;
  // Trump has been chosen this hand, whether or not we know what it is.
  private trumpPlaced = false;
  // How the deciding hand went, for the match-over panel.
  private finalHandLine = "";
  // Play with bots, in progress: the number of members we last added a bot for.
  private playNow: { botAskedAt: number; watchdog: number } | null = null;
  private hudValues = { score: 0, theirScore: 0, tricks: 0, theirTricks: 0, matchUs: 0, matchThem: 0 };
  private scoreTween: TWEEN.Tween<typeof this.hudValues> | null = null;

  constructor(sceneManager: SceneManager) {
    this.sceneManager = sceneManager;

    this.lighting = new Lighting(this.sceneManager.scene);
    this.table = new Table(this.sceneManager.scene);
    this.hand = new Hand(this.sceneManager.scene, this.sceneManager.camera);
    this.playArea = new PlayArea(this.sceneManager.scene, this.sceneManager.camera);
    this.playArea.setPacing(this.pacing.profile());
    this.playArea.setOnCardLand(() => this.sound.cardLand());
    this.zenTextures = new ZenCardTextures();
    this.feel = new GameFeel(this.sound, () => this.pacing.profile());

    this.dragControls = new DragControls(
      this.sceneManager.camera,
      this.sceneManager.scene,
      this.sceneManager.renderer.domElement,
      this.hand,
      {
        mode: () =>
          this.arranging
            ? "arrange"
            : this.mockMode || this.model.pendingRequest?.kind === "play"
              ? "play"
              : "inactive",
        canPlay: (card) => this.cardIsPlayable(card),
        tryPlay: (card) => this.tryPlayCard(card),
        arrangementChanged: () => this.saveArrangement(),
      }
    );

    this.ui = new UiOverlay({
      onCreate: (name, target) => this.createTable(name, target),
      onPlayBots: (name, target) => this.startPlayNow(name, target),
      onBrowse: (name) => this.browseTables(name),
      onJoin: (roomName) => this.session?.join(roomName),
      onJoinLink: (name, roomName) => this.joinTable(name, roomName),
      onEmote: (emote) => this.session?.sendEmote(emote),
      onStandings: (name) => this.showStandings(name),
      onRefreshRooms: () => this.session?.refreshRooms(),
      onAddBot: (seat) => this.session?.addBot(seat),
      onStartGame: () => this.session?.startGame(),
      onLeave: () => this.session?.leaveRoom(),
      onKick: (member) => this.session?.kick(member),
      onBid: (bid) => this.session?.submitBid(bid),
      onPass: () => this.session?.submitBid("pass"),
      onTrump: (suit, cards) => this.chooseTrump(suit, cards),
      onExpose: () => this.session?.exposeTrump(),
      onNextHand: () => this.session?.nextHand(),
      onLeaveMatch: () => this.leaveMatch(),
      onReplaceBot: () => this.session?.resolveAbandon("replace"),
      onEndMatch: () => this.session?.resolveAbandon("close"),
      // a reload is the cleanest road home: the match is over, the stored
      // identity survives, and every scrap of scene state resets
      onGoHome: () => window.location.reload(),
      onCyclePacing: () => {
        const pacing = this.pacing.cycleSpeed();
        this.playArea.setPacing(pacing);
        return pacing;
      },
      onToggleMotion: () => {
        const pacing = this.pacing.toggleReduceMotion();
        this.applyMotionPreference(pacing);
        return pacing;
      },
      onArrangePreset: (preset) => this.applyArrangementPreset(preset),
      onArrangeCustom: () => this.startCustomArrangement(),
      onArrangeDone: () => this.finishCustomArrangement(),
      onAbandonReconnect: () => this.abandonInterruptedMatch(),
    });

    this.scoreElement = document.getElementById("score-value");
    this.tricksElement = document.getElementById("tricks-value");
    this.matchElement = document.getElementById("match-value");
    this.applyMotionPreference(this.pacing.profile());
    window.matchMedia?.("(prefers-reduced-motion: reduce)").addEventListener?.("change", () => {
      this.applyMotionPreference(this.pacing.profile());
    });
    this.ui.setHomeFan(this.homeFanCards());
    this.syncHud();
    this.sound.bindToggle(document.getElementById("sound-button"));
    // every button answers the finger with a tick (pointerdown beats click
    // so the sound lands on the press, not the release)
    document.addEventListener("pointerdown", (event) => {
      const target = event.target as HTMLElement | null;
      if (target?.closest?.("button")) this.sound.uiClick();
    });

    this.applyViewportLayout();
    window.addEventListener("resize", () => {
      this.applyViewportLayout();
    });
  }

  // Reduce motion is on by the player's choice or the device's: either way CSS,
  // the table's tweens and the effects all hear it here. Turning it on also
  // stops anything already running, which brings the HUD straight up to date.
  private applyMotionPreference(pacing: GamePacingProfile) {
    document.documentElement.classList.toggle("reduce-motion", pacing.reducedMotion);
    this.playArea.setPacing(pacing);
    this.ui.setPacing(pacing);
    this.ui.setMotion(pacing.reducedMotion, this.pacing.motionForcedByDevice());
    if (pacing.reducedMotion) this.feel.cancelAll();
  }

  // Four real cards for the home table: painted from the same art as the game
  // once it has loaded, face down until then.
  private homeFanCards(): HTMLCanvasElement[] {
    const cards = [
      { suit: "spades", rank: "J" },
      { suit: "hearts", rank: "A" },
      { suit: "diamonds", rank: "9" },
      { suit: "clubs", rank: "10" },
    ] as const;
    const faces = cards.map(() => document.createElement("canvas"));
    void this.zenTextures.waitForLoad().then(() => {
      cards.forEach(({ suit, rank }, index) => {
        const art = this.zenTextures.getCardTexture(suit, rank).image as HTMLCanvasElement;
        faces[index].width = art.width;
        faces[index].height = art.height;
        faces[index].getContext("2d")?.drawImage(art, 0, 0);
      });
    }, () => undefined);
    return faces;
  }

  // Seats, played-card lanes, and the hand all consume one screen-space
  // composition. The Three.js pieces are projected back onto the table plane.
  private applyViewportLayout() {
    this.tableLayout = responsiveTableLayout(window.innerWidth, window.innerHeight);
    this.hand.setWidthScale(this.tableLayout.handWidthScale);
    this.hand.setCardScale(this.tableLayout.handCardScale);
    this.hand.setScreenCenter(this.tableLayout.handCenter, this.tableLayout.viewport);
    this.playArea.setCardScale(this.tableLayout.tableCardScale);
    this.playArea.setViewportLayout(this.tableLayout);
    this.positionSeatAnchors();
    // The middle of the trick's cross, where a message covers the least.
    const { playedCards } = this.tableLayout;
    document.documentElement.style.setProperty(
      "--table-middle",
      `${(playedCards[0].y + playedCards[2].y) / 2}px`
    );
    // re-laying out the fan flattens a raised card — it must not stay armed
    this.dragControls.clearSelection();
  }

  private positionSeatAnchors() {
    for (const seat of [1, 2, 3] as const) {
      const label = document.getElementById(`seat-label-${seat}`);
      const anchor = this.tableLayout.opponentSeats[seat];
      if (!label) continue;
      label.style.left = `${anchor.x}px`;
      label.style.top = `${anchor.y}px`;
    }
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
    this.hand.setLifted(
      playing && !this.arranging ? this.legalCardIds() : [],
      this.pacing.profile().reducedMotion
    );
  }

  // "You" for ourselves, the real player name (set at deal time) otherwise.
  private seatName(playerIndex: PlayerIndex): string {
    if (playerIndex === 0) return "You";
    return this.model.seatNames?.[playerIndex] ?? SEAT_NAMES[playerIndex];
  }

  // Build each opponent as one projected seat: the avatar is centered inside
  // a fixed DOM enso, with identity and the auction action attached. Keeping
  // seats in screen space makes their size and card clearance deterministic.
  private positionSeatLabels() {
    for (const seat of [1, 2, 3] as const) {
      const label = document.getElementById(`seat-label-${seat}`);
      if (!label) continue;
      const names = this.model.seatNames;
      const members = this.model.seatMembers;
      if (!names || !members) {
        label.classList.remove("visible");
        continue;
      }
      const profile = playerProfile(members[seat] ?? "", members);
      const identity = createPlayerIdentity(profile, "table");
      const action = document.createElement("span");
      action.id = `seat-action-${seat}`;
      action.className = "seat-action";
      action.setAttribute("aria-live", "polite");
      action.setAttribute("aria-atomic", "true");
      label.replaceChildren(identity);
      if (seat === 2) {
        const tag = document.createElement("span");
        tag.className = "partner-tag";
        tag.textContent = "your partner";
        label.append(tag);
      }
      label.append(action);
      label.classList.add("visible");
    }
    this.positionSeatAnchors();
    this.setActiveSeat(this.model.activePlayer);
  }

  // Whose turn it is should read at a glance: the seat lights up and says what it's doing,
  // and the bottom line says it in words.
  private setActiveSeat(playerIndex: PlayerIndex | null) {
    const doing = this.turnWord();
    for (const seat of [1, 2, 3] as const) {
      const label = document.getElementById(`seat-label-${seat}`);
      label?.classList.toggle("active", seat === playerIndex);
      if (seat === playerIndex) {
        label?.setAttribute("data-turn", doing);
        // Their last call is stale now, and this is where the turn is shown.
        document.getElementById(`seat-action-${seat}`)?.classList.remove("visible");
      } else {
        label?.removeAttribute("data-turn");
      }
    }
    const line =
      playerIndex !== null && playerIndex !== 0 ? `${this.seatName(playerIndex)} is ${doing}` : "";
    if (this.turnLine) this.ui.clearStatus(this.turnLine);
    this.turnLine = line;
    if (line) this.ui.status(line);
  }

  private turnWord(): string {
    switch (this.model.phase) {
      case "bidding":
        return "bidding";
      case "choosing-trump":
        return "choosing trump";
      default:
        return "playing";
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

  public createTable(playerName: string, target = 6) {
    this.startSession(playerName, "create-room", undefined, undefined, target);
  }

  // Play with bots is ordinary lobby traffic run in sequence: create a table, ask for
  // a bot each time the members change, start when it's full. Every step already
  // has its answer from the server, so a failure stops short with a message and
  // leaves the player somewhere they can carry on from.
  private startPlayNow(playerName: string, target: number) {
    if (this.playNow) return;
    this.ui.setPlayBotsBusy(true);
    this.ui.status("Setting the table…");
    const watchdog = window.setTimeout(() => {
      this.stopPlayNow();
      this.ui.setPlayBotsBusy(false);
      if (this.model.phase !== "bidding") {
        this.ui.status("That took too long. Check your connection and try again.");
      }
    }, PLAY_NOW_LIMIT_MS);
    this.playNow = { botAskedAt: 0, watchdog };
    this.createTable(playerName, target);
  }

  private stopPlayNow() {
    if (!this.playNow) return;
    window.clearTimeout(this.playNow.watchdog);
    this.playNow = null;
  }

  private advancePlayNow() {
    if (!this.playNow || this.model.phase !== "lobby") return;
    const seated = this.model.members.length;
    if (seated >= 4) {
      this.session?.startGame();
    } else if (this.playNow.botAskedAt !== seated) {
      this.playNow.botAskedAt = seated;
      this.session?.addBot();
    }
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
    rejoinEmail?: string,
    tableTarget?: number
  ) {
    // The name was fixed when this connection was made: one edited since then
    // (after browsing, or after leaving a table) needs a connection of its own.
    if (
      this.session?.canRename() &&
      (intent === "create-room" || intent === "join") &&
      (playerName.trim() || "Player") !== this.session.displayName
    ) {
      this.session.destroy();
      this.session = null;
    }
    if (this.session) {
      // already connected (e.g. browsing) — just issue the new intent
      if (intent === "create-room") {
        if (tableTarget !== undefined) this.session.tableTarget = tableTarget;
        this.session.createRoom();
      } else if (intent === "join" && joinRoom) this.session.join(joinRoom);
      else this.session.refreshRooms();
      return;
    }
    this.mockMode = false;
    this.session = new GameSession(
      {
        dispatch: (action) => this.dispatch(action),
        getModel: () => this.model,
        getPacing: () => this.pacing.profile(),
        status: (text) => this.ui.status(text),
        rooms: (rooms) => {
          this.ui.clearStatus("Connecting…");
          this.ui.showRooms(rooms);
        },
        emote: (seat, emote) => this.showEmote(seat, emote),
        leaderboard: (rows) => {
          this.ui.clearStatus("Connecting…");
          this.ui.showLeaderboard(rows);
        },
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
    if (tableTarget !== undefined) this.session.tableTarget = tableTarget;

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
    const label = seat === 0 ? null : document.getElementById(`seat-label-${seat}`);
    const rect = label?.getBoundingClientRect();
    const screen = seat === 0
      ? { x: canvas.clientWidth / 2, y: canvas.clientHeight - 150 }
      : rect
        ? { x: rect.left + rect.width / 2, y: rect.top + rect.height / 2 }
        : null;
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

  public arrangementState() {
    return {
      available: this.arrangementAvailable,
      arranging: this.arranging,
      mode: this.arrangementMode,
      order: this.hand.orderIds(),
    };
  }

  public arrangeCards(mode: ArrangementMode): boolean {
    if (mode === "custom") return this.startCustomArrangement();
    return this.applyArrangementPreset(mode);
  }

  public finishArranging(): boolean {
    if (!this.arranging) return false;
    this.finishCustomArrangement();
    return true;
  }

  private applyArrangementPreset(preset: ArrangementPreset): boolean {
    if (!this.arrangementAvailable) return false;
    if (preset === "trump" && this.model.trumpSuit === null) return false;
    this.arranging = false;
    this.arrangementMode = preset;
    this.dragControls.clearSelection();
    this.ui.setArranging(false);
    this.ui.setArrangementSelection(preset);
    this.hand.setOrder(
      arrangedCardIds(this.model.hand, preset, {
        trumpSuit: this.model.trumpSuit,
        dealOrder: this.dealOrderIds,
      })
    );
    this.saveArrangement();
    return true;
  }

  private startCustomArrangement(): boolean {
    if (!this.arrangementAvailable) return false;
    this.dragControls.clearSelection();
    // A preset may still be tweening. Custom hit-testing must immediately
    // agree with the logical slot order before the first drag begins, so
    // nothing stays lifted out of the fan either.
    this.hand.setLifted([], true);
    this.hand.arrangeCards(null, true, 0);
    this.arranging = true;
    this.arrangementMode = "custom";
    this.ui.setArranging(true);
    this.ui.setArrangementSelection("custom");
    this.saveArrangement();
    return true;
  }

  private finishCustomArrangement() {
    this.dragControls.clearSelection();
    this.hand.arrangeCards();
    this.arranging = false;
    this.ui.setArranging(false);
    this.renderLegality(); // back to playing: the playable cards rise again
    this.saveArrangement();
  }

  private enableArrangement() {
    if (this.model.hand.length === 0) return;
    this.arrangementAvailable = true;
    if (this.dealOrderIds.length === 0) {
      this.dealOrderIds = this.model.hand.map((card) => card.id);
    }
    this.ui.setArrangeAvailable(true, this.model.trumpSuit !== null);
    this.saveArrangement();
  }

  private resetArrangement(clearStored = true) {
    this.arrangementAvailable = false;
    this.arranging = false;
    this.arrangementMode = "deal";
    this.dealOrderIds = [];
    this.dragControls.clearSelection();
    this.ui.setArrangeAvailable(false, false);
    this.ui.setArrangementSelection("deal");
    if (clearStored) this.clearStoredArrangement();
  }

  private saveArrangement() {
    if (!this.arrangementAvailable || !this.session || !this.model.roomName) return;
    try {
      localStorage.setItem(
        "game28-card-arrangement",
        JSON.stringify({
          email: this.session.email,
          room: this.model.roomName,
          handNumber: this.model.handNumber,
          mode: this.arrangementMode,
          dealOrder: this.dealOrderIds,
          visualOrder: this.hand.orderIds(),
        })
      );
    } catch {
      // Arrangement remains usable when browser storage is unavailable.
    }
  }

  private restoreArrangement(): boolean {
    if (!this.session || !this.model.roomName) return false;
    try {
      const raw = localStorage.getItem("game28-card-arrangement");
      if (!raw) return false;
      const stored = JSON.parse(raw) as {
        readonly email?: string;
        readonly room?: string;
        readonly handNumber?: number;
        readonly mode?: ArrangementMode;
        readonly dealOrder?: string[];
        readonly visualOrder?: string[];
      };
      const modes: readonly ArrangementMode[] = ["suit", "strength", "trump", "deal", "custom"];
      if (
        stored.email !== this.session.email ||
        stored.room !== this.model.roomName ||
        stored.handNumber !== this.model.handNumber ||
        !stored.mode ||
        !modes.includes(stored.mode)
      ) {
        return false;
      }
      this.arrangementMode = stored.mode;
      this.ui.setArrangementSelection(stored.mode);
      this.dealOrderIds = Array.isArray(stored.dealOrder)
        ? stored.dealOrder
        : this.model.hand.map((card) => card.id);
      if (stored.mode === "custom") {
        if (Array.isArray(stored.visualOrder)) this.hand.setOrder(stored.visualOrder);
      } else if (stored.mode !== "trump" || this.model.trumpSuit !== null) {
        this.hand.setOrder(
          arrangedCardIds(this.model.hand, stored.mode, {
            trumpSuit: this.model.trumpSuit,
            dealOrder: this.dealOrderIds,
          })
        );
      }
      return true;
    } catch {
      return false;
    }
  }

  private clearStoredArrangement() {
    try {
      localStorage.removeItem("game28-card-arrangement");
    } catch {
      // Nothing else depends on this optional local preference.
    }
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

  // Give up on an interrupted match from the reconnecting screen. A retry
  // session may be mid-flight (its socket may even be live again) — turn
  // it into an explicit leave; otherwise just forget the stored seat and
  // let it expire in its server-side grace window.
  private abandonInterruptedMatch() {
    if (this.session) {
      this.session.leaveGame();
      this.session.destroy();
      this.session = null;
    } else {
      forgetActiveMatch();
    }
    this.dispatch({ _tag: "RoomLeft" });
    this.ui.status("You left the match.");
  }

  // Sweep every scrap of an in-progress game out of the scene and the
  // overlay; the model reset (RoomLeft / idle) happens in the reducer.
  private clearTableScene(clearArrangementStorage = true) {
    this.feel.cancelAll();
    this.hand.cancelPendingDeals();
    this.resetArrangement(clearArrangementStorage);
    for (const card of [...this.hand.getCards()]) {
      this.hand.removeCard(card);
      this.sceneManager.scene.remove(card.mesh);
    }
    this.hand.setLifted([], true);
    this.playArea.clearTable();
    this.setActiveSeat(null);
    this.ui.setInMatch(false);
    this.trumpPlaced = false;
    this.syncHud();
    this.ui.hideBidPanel();
    this.ui.hideTrumpPanel();
    this.ui.setExposeVisible(false);
    this.ui.hideResult();
    this.ui.hideAbandonPanel();
    this.ui.hideTrickOutcome();
    this.ui.clearBidHistory();
    this.ui.setLeaveMatchVisible(false);
    this.renderScore();
    this.ui.hideLobby();
    this.positionSeatLabels(); // hides them (seatNames reset)
  }

  // The chosen suit card leaves for the HUD as the panel closes. The HUD's mini
  // card stays empty until it lands.
  private chooseTrump(suit: Suit, cards: ReadonlyArray<{ suit: Suit; box: DOMRect }>) {
    if (!this.session || this.model.pendingRequest?.kind !== "trump") return;
    if (this.feel.enabled) {
      this.heldTrump = { placed: false, suit: null, exposed: false };
      this.feel.pickTrump(cards, suit, this.hud.trumpCard(), () => this.releaseTrump());
    }
    this.session.chooseTrump(suit);
  }

  private releaseTrump() {
    this.heldTrump = null;
    this.syncHud();
  }

  // Trump comes up for the whole table. The HUD keeps its face-down card until
  // the big one has flown over; with effects off it just says so and turns over.
  private announceTrump(suit: Suit, revealer: string | null, immediateDuration: number) {
    if (this.feel.enabled) {
      this.heldTrump = { placed: true, suit: null, exposed: false };
      this.syncHud();
      this.feel.revealTrump(suit, trumpRevealCaption(revealer), this.hud.trumpCard(), () =>
        this.releaseTrump()
      );
    } else {
      this.syncHud();
      this.ui.showTrumpReveal(suit, revealer, immediateDuration);
    }
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

  // A page point where a tap picks this hand card, nearest its centre. In a
  // tight fan the next card covers the centre, so tests tap here instead. It is
  // in page coordinates, as taps are: Playwright can scroll the page to reach
  // a panel, and then the canvas no longer starts at the page's corner.
  public cardTapPoint(cardId: string): { x: number; y: number } | null {
    const card = this.hand.getCards().find((handCard) => handCard.mesh.userData.modelId === cardId);
    const centre = this.cardScreenPosition(cardId);
    if (!card || !centre) return null;
    const canvas = this.sceneManager.renderer.domElement;
    const rect = canvas.getBoundingClientRect();
    const bounds = objectScreenBounds(card.mesh, this.sceneManager.camera, canvas);
    const steps = 12;
    let best: { x: number; y: number; distance: number } | null = null;
    for (let i = 1; i < steps; i++) {
      for (let j = 1; j < steps; j++) {
        const x = bounds.left + ((bounds.right - bounds.left) * i) / steps;
        const y = bounds.top + ((bounds.bottom - bounds.top) * j) / steps;
        if (this.dragControls.cardAt(rect.left + x, rect.top + y) !== card) continue;
        const distance = Math.hypot(x - centre.x, y - centre.y);
        if (!best || distance < best.distance) best = { x, y, distance };
      }
    }
    return best && { x: rect.left + best.x, y: rect.top + best.y };
  }

  public playedCardScreenBounds() {
    return this.playArea.playedCardScreenBounds(this.sceneManager.renderer.domElement);
  }

  public handCardScreenBounds() {
    return this.hand.cardScreenBounds(this.sceneManager.renderer.domElement);
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
    // Keep Three.js motion in lockstep with the pacing values that guard
    // server-event presentation. This also responds to a live OS reduced
    // motion preference change without touching game state.
    this.playArea.setPacing(this.pacing.profile());
    switch (action._tag) {
      case "PhaseChanged":
        this.syncHud();
        if (action.phase === "connecting") this.ui.status("Connecting…");
        if (action.phase === "idle") {
          // only dead sessions dispatch idle (connect failure, server gone):
          // drop ours so the next screen's buttons build a fresh one, and
          // clear the now-unrefreshable table list. The reducer has already
          // wiped the model, so clear the scene to match.
          if (this.session) {
            this.session.destroy();
            this.session = null;
          }
          this.stopPlayNow();
          const reconnecting = storedActiveMatch() !== null;
          this.clearTableScene(!reconnecting);
          this.ui.resetRoomBrowser();
          if (reconnecting) {
            // our match is still live somewhere — hold the player on a
            // screen with no create/join actions (a second table under the
            // same identity would corrupt both matches) and keep retrying
            this.ui.showReconnecting();
            this.scheduleRejoinRetry();
          } else {
            this.ui.showStartScreen();
          }
        }
        break;

      case "RoomEntered":
        this.ui.updateLobby(this.model, this.session?.email ?? "");
        this.advancePlayNow();
        break;

      case "MembersChanged":
        // mid-match this is a seat replacement, not a lobby change
        if (this.model.phase === "lobby") {
          this.ui.updateLobby(this.model, this.session?.email ?? "");
          this.advancePlayNow();
        }
        break;

      case "RoomLeft":
        // tear down any in-progress game (aborted games land here mid-hand
        // or while the between-hands result panel is up)
        this.stopPlayNow();
        this.clearTableScene();
        this.ui.showStartScreen();
        break;

      case "SeatNamesSet":
        this.positionSeatLabels();
        break;

      case "HandDealt":
        this.stopPlayNow();
        this.ui.hideLobby();
        this.ui.setInMatch(true);
        this.ui.clearStatus("Dealing…", "Starting…");
        // a live table always offers a way out (not in ?mock=1 — no server)
        this.ui.setLeaveMatchVisible(this.session !== null);
        // The server deals exactly two packets of four. The model already
        // owns all received cards; this only stages their arrival in the fan.
        const firstPacket = this.model.hand.length === action.cards.length;
        if (firstPacket) {
          this.resetArrangement();
          this.trumpRevealAnnounced = false;
          this.ui.clearBidHistory();
        }
        this.sound.deal(action.cards.length);
        const pacing = this.pacing.profile();
        const fullHand = this.model.hand.length === 8;
        const arrangementDelay = Math.max(
          pacing.dealCompleteMs,
          pacing.dealCardStaggerMs * Math.max(0, action.cards.length - 1) + 400
        );
        if (fullHand) this.dealOrderIds = this.model.hand.map((card) => card.id);
        this.hand.dealCards(
          action.cards.map((card) => this.createCard(card)),
          pacing.dealCardStaggerMs,
          fullHand ? () => this.enableArrangement() : undefined,
          arrangementDelay
        );
        this.ui.showTableMoment(
          "",
          firstPacket ? "First four cards" : "Final four cards",
          firstPacket ? "Read your opening hand" : "Your full hand is ready",
          this.pacing.profile().reducedMotion || !firstPacket ? 1_200 : 3_000,
          true
        );
        this.renderLegality();
        break;

      case "CardPlayed":
        this.sound.cardPlay();
        this.playCard(action.card, action.playerIndex);
        this.saveArrangement();
        break;

      case "ActivePlayerChanged":
        this.setActiveSeat(action.playerIndex);
        break;

      case "TrickScored":
        this.renderScore(this.pacing.profile().scoreLandingMs);
        break;

      case "BidPlaced": {
        this.sound.bidTick();
        const seal = this.ui.showBidEvent(action.playerIndex, action.bid);
        if (seal) this.feel.stampSeal(seal);
        break;
      }

      case "BidWon":
        this.ui.clearBidHistory();
        this.ui.status(`${this.seatName(action.playerIndex)} won the bid at ${action.bid}`, true);
        this.syncHud();
        this.feel.stampHud(this.hud.bidSeal());
        break;

      case "TrumpSet":
        this.trumpPlaced = true;
        this.syncHud();
        this.ui.setArrangeAvailable(this.arrangementAvailable, this.model.trumpSuit !== null);
        // Rejoin snapshots and older servers may establish an already
        // exposed trump without a live TrumpRevealed event.
        if (
          this.model.trumpExposed &&
          this.model.trumpSuit !== null &&
          !this.trumpRevealAnnounced
        ) {
          this.trumpRevealAnnounced = true;
          this.announceTrump(
            this.model.trumpSuit,
            null,
            this.pacing.profile().reducedMotion ? 1_200 : 3_000
          );
        }
        break;

      case "TrumpRevealed":
        this.trumpPlaced = true;
        this.sound.trumpReveal();
        this.ui.setArrangeAvailable(this.arrangementAvailable, this.model.trumpSuit !== null);
        if (!this.trumpRevealAnnounced) {
          this.trumpRevealAnnounced = true;
          this.announceTrump(
            action.suit,
            this.seatName(action.playerIndex),
            this.pacing.profile().reducedMotion ? 1_200 : 3_000
          );
        } else {
          this.syncHud();
        }
        break;

      case "RequestReceived":
        this.sound.yourTurn();
        switch (action.request.kind) {
          case "bid":
            this.ui.showBidPanel(action.request.minBid, this.model.bidsPlaced === 0);
            break;
          case "trump":
            this.ui.clearBidHistory(); // the auction is over: its calls leave the table
            this.ui.showTrumpPanel();
            this.ui.status("You won the bid. Choose trump.");
            break;
          case "play":
            this.ui.yourTurn();
            this.ui.setExposeVisible(canExposeTrump(this.model));
            break;
        }
        this.renderLegality();
        break;

      case "RequestCleared":
        // whatever we were being asked for has been answered
        this.ui.clearStatus("You won the bid. Choose trump.", "Your turn");
        this.ui.hideBidPanel();
        this.ui.hideTrumpPanel();
        this.ui.setExposeVisible(false);
        this.dragControls.clearSelection();
        this.renderLegality();
        break;

      case "TrickCleared":
        this.playArea.clearTable();
        this.ui.hideTrickOutcome();
        break;

      case "TrickEnded": {
        // The prior turn prompt can otherwise say "Your turn" while another
        // player is visibly taking the trick, which undermines the outcome.
        this.ui.status("");
        if (action.winner !== null) {
          const seat = this.seatName(action.winner);
          const taker = action.winner === 0 ? "You take" : `${seat} takes`;
          const ours = action.winner === 0 || action.winner === 2;
          this.ui.showTrickOutcome(taker, action.points, ours);
          this.feel.avatarReact(
            document.querySelector<HTMLElement>(`#seat-label-${action.winner} .player-avatar`)
          );
        }
        this.playArea.resolveTrick(action.winner, {
          onCollect: () => {
            if (action.winner !== null) this.sound.trickSweep();
          },
          onComplete: () => {
            this.renderScore(this.pacing.profile().scoreLandingMs);
            this.ui.hideTrickOutcome();
          },
        });
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
        this.setActiveSeat(null);
        this.resetArrangement();
        break;

      case "HandResult": {
        // when a team has reached the target, match-over is already in
        // flight — its panel and fanfare replace the between-hands ones
        const decided =
          this.model.matchUs >= this.model.matchTarget ||
          this.model.matchThem >= this.model.matchTarget;
        const results = this.model.handResults;
        const matchSwing = results.length === 0 ? 0 : results[results.length - 1];
        const moved =
          this.shownMatch.us !== this.model.matchUs || this.shownMatch.them !== this.model.matchThem;
        // The pips wait for the flying points; the numbers in the menu do not.
        if (!decided && moved && this.feel.enabled) this.heldMatch = { ...this.shownMatch };
        this.renderScore(this.pacing.profile().scoreLandingMs); // match HUD segment just changed
        const bidderOurs = action.bidder === 0 || action.bidder === 2;
        const copy = handResultCopy({
          bidderName: this.seatName(action.bidder),
          bid: action.bid,
          took: bidderOurs ? this.model.score : this.model.theirScore,
          made: action.made,
          delta: action.delta ?? bidderDelta(matchSwing, bidderOurs),
          bidderIsOurs: bidderOurs,
        });
        if (decided) {
          this.finalHandLine = `${copy.seal === "MADE" ? "Made" : "Set"}: ${copy.detail}`;
          break;
        }
        const note =
          `This hand ${this.model.score} – ${this.model.theirScore} · ` +
          `match ${scoreText(this.model.matchUs)} – ${scoreText(this.model.matchThem)}, ` +
          `first to ${this.model.matchTarget}`;
        this.sound.result(bidderOurs ? action.made : !action.made);
        this.ui.status("");
        const parts = this.ui.showHandResult(
          copy,
          note,
          actingHostEmail(this.model.members) === (this.session?.email ?? "")
        );
        const team = bidderOurs ? "us" : "them";
        const land = () => {
          this.heldMatch = null;
          this.syncHud();
          if (moved) this.feel.popPip(this.hud.pipFor(team));
        };
        if (this.feel.enabled) {
          this.feel.handResult(parts, () => this.hud.pipFor(team), land);
        } else {
          land();
        }
        break;
      }

      case "HandReset":
        // the next deal is landing: clear the table but keep the seats,
        // their name labels, and the match HUD
        this.feel.cancelAll();
        this.hand.cancelPendingDeals();
        this.trumpRevealAnnounced = false;
        this.resetArrangement();
        for (const card of [...this.hand.getCards()]) {
          this.hand.removeCard(card);
          this.sceneManager.scene.remove(card.mesh);
        }
        this.playArea.clearTable();
        this.setActiveSeat(null);
        this.ui.hideTrickOutcome();
        this.ui.clearBidHistory();
        this.hand.setLifted([], true);
        this.trumpPlaced = false;
        this.syncHud();
        this.ui.hideBidPanel();
        this.ui.hideTrumpPanel();
        this.ui.setExposeVisible(false);
        this.ui.hideResult();
        this.renderScore();
        break;

      case "MatchOver": {
        this.feel.cancelAll();
        this.renderScore();
        this.resetArrangement();
        this.sound.fanfare(action.winner === "us");
        this.ui.status("");
        // the match is decided — the panel's Back to home takes it from here
        this.ui.setLeaveMatchVisible(false);
        const members = this.model.seatMembers;
        const team = members
          ? [members[0], members[2]]
              .filter((member) => member)
              .map((member) => playerProfile(member, members))
          : [];
        this.feel.matchOver(
          action.winner === "us",
          this.ui.showMatchOver(
            action.us,
            action.them,
            action.winner === "us",
            action.hands,
            this.model.matchTarget,
            this.model.handResults,
            team,
            this.finalHandLine
          )
        );
        break;
      }

      case "SnapshotRestored": {
        // a reconnect: rebuild the whole scene from the restored model;
        // any pending request/result is re-dispatched right after and
        // brings its own panel back up
        this.feel.cancelAll();
        this.stopPlayNow();
        this.ui.setInMatch(true);
        this.hand.setLifted([], true);
        this.hand.cancelPendingDeals();
        this.trumpRevealAnnounced = this.model.trumpExposed;
        this.resetArrangement(false);
        for (const card of [...this.hand.getCards()]) {
          this.hand.removeCard(card);
          this.sceneManager.scene.remove(card.mesh);
        }
        this.playArea.clearTable();
        this.setActiveSeat(null);
        this.ui.hideTrickOutcome();
        this.ui.hideLobby();
        this.ui.hideReconnecting();
        this.ui.hideBidPanel();
        this.ui.hideTrumpPanel();
        this.ui.setExposeVisible(false);
        this.ui.hideResult();
        this.ui.hideAbandonPanel();
        this.ui.clearBidHistory();
        this.model.hand.forEach((card) => this.hand.addCard(this.createCard(card)));
        for (const played of this.model.playedCards) {
          this.playCard(played.card, played.playerIndex);
        }
        this.renderScore();
        this.positionSeatLabels();
        this.ui.setLeaveMatchVisible(this.session !== null);
        if (this.model.phase === "playing" && this.model.hand.length > 0) {
          this.dealOrderIds = this.model.hand.map((card) => card.id);
          this.arrangementAvailable = true;
          this.restoreArrangement();
          this.ui.setArrangeAvailable(true, this.model.trumpSuit !== null);
          this.ui.setArranging(false);
          this.saveArrangement();
        }
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

    // Table cards use their own responsive scale; the hand can remain more
    // readable without making the bottom play collide with it.
    card.setDimmed(false);
    card.mesh.scale.setScalar(this.tableLayout.tableCardScale);
    this.playArea.playCard(card, playerIndex);
  }

  private findHandCard(cardModel: CardModel): Card | null {
    return this.hand.getCards().find((card) => card.mesh.userData.modelId === cardModel.id) ?? null;
  }

  private startPositionForPlayer(playerIndex: PlayerIndex) {
    const screen = playerIndex === 0
      ? this.tableLayout.handCenter
      : this.tableLayout.opponentSeats[playerIndex];
    return screenPointOnPlane(
      screen,
      0.5,
      this.sceneManager.camera,
      this.tableLayout.viewport
    );
  }

  private renderScore(duration = 0) {
    const target = {
      score: this.model.score,
      theirScore: this.model.theirScore,
      tricks: this.model.tricks,
      theirTricks: this.model.theirTricks,
      matchUs: this.model.matchUs,
      matchThem: this.model.matchThem,
    };
    this.scoreTween?.stop();
    this.scoreTween = null;
    this.shownPoints = { us: target.score, them: target.theirScore };
    this.syncHud();

    if (duration === 0) {
      Object.assign(this.hudValues, target);
      this.writeScore();
      return;
    }

    this.scoreTween = new TWEEN.Tween(this.hudValues)
      .to(target, duration)
      .easing(TWEEN.Easing.Quadratic.Out)
      .onUpdate(() => this.writeScore())
      .onComplete(() => {
        Object.assign(this.hudValues, target);
        this.scoreTween = null;
        this.writeScore();
      })
      .start();
  }

  // The top bar, from the model, except for whatever is still on its way there.
  private syncHud() {
    const model = this.model;
    const match = this.heldMatch ?? { us: model.matchUs, them: model.matchThem };
    this.shownMatch = match;
    const bid = model.finalBid;
    this.hud.render({
      matchUs: match.us,
      matchThem: match.them,
      target: model.matchTarget,
      ourPoints: this.shownPoints.us,
      theirPoints: this.shownPoints.them,
      bid,
      bidder: model.bidWinner,
      bidderName: model.bidWinner === null ? null : this.seatName(model.bidWinner),
      trump: this.heldTrump ?? {
        placed:
          bid !== null &&
          (this.trumpPlaced ||
            model.phase === "playing" ||
            model.phase === "hand-finished" ||
            model.phase === "finished"),
        suit: model.trumpSuit,
        exposed: model.trumpExposed,
      },
    });
  }

  private writeScore() {
    if (this.scoreElement) {
      this.scoreElement.textContent = `${Math.round(this.hudValues.score)} — ${Math.round(this.hudValues.theirScore)}`;
    }
    if (this.tricksElement) {
      this.tricksElement.textContent = `${Math.round(this.hudValues.tricks)} — ${Math.round(this.hudValues.theirTricks)}`;
    }
    if (this.matchElement) {
      this.matchElement.textContent = `${Math.round(this.hudValues.matchUs)} — ${Math.round(this.hudValues.matchThem)}`;
    }
  }
}
