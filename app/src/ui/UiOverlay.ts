import { GameModel, PlayerIndex, Suit } from "../game/GameModel";
import type { ArrangementMode, ArrangementPreset } from "../game/CardArrangement";
import type { GamePacingProfile } from "../game/GamePacing";
import { RoomInfo } from "../net/protocol";
import { lobbySeats, scoreText, signedText } from "./HudModel";
import type { HandResultCopy } from "./HudModel";
import { playerProfile } from "./PlayerProfile";
import type { PlayerProfile } from "./PlayerProfile";
import { suitGlyph } from "./ScoreHud";

export { memberDisplayName } from "./PlayerProfile";

export interface UiCallbacks {
  readonly onCreate: (playerName: string, target: number) => void;
  // the big button: a table of the chosen length, three bots, and straight in
  readonly onPlayBots: (playerName: string, target: number) => void;
  readonly onBrowse: (playerName: string) => void;
  readonly onJoin: (roomName: string) => void;
  readonly onJoinLink: (playerName: string, roomName: string) => void;
  readonly onEmote: (emote: string) => void;
  readonly onStandings: (playerName: string) => void;
  readonly onRefreshRooms: () => void;
  readonly onAddBot: (seat?: number) => void;
  readonly onStartGame: () => void;
  readonly onLeave: () => void;
  readonly onKick: (member: string) => void;
  readonly onBid: (bid: number) => void;
  readonly onPass: () => void;
  readonly onTrump: (suit: Suit, cards: ReadonlyArray<{ suit: Suit; box: DOMRect }>) => void;
  readonly onExpose: () => void;
  readonly onNextHand: () => void;
  readonly onLeaveMatch: () => void;
  readonly onReplaceBot: () => void;
  readonly onEndMatch: () => void;
  readonly onGoHome: () => void;
  readonly onCyclePacing: () => GamePacingProfile;
  readonly onToggleMotion: () => GamePacingProfile;
  readonly onArrangePreset: (preset: ArrangementPreset) => void;
  readonly onArrangeCustom: () => void;
  readonly onArrangeDone: () => void;
  // give up on reconnecting to an interrupted match
  readonly onAbandonReconnect: () => void;
}

const YOUR_TURN = "Your turn";

export const SEAT_NAMES = ["You", "Right", "Partner", "Left"] as const;

const MODE_KEY = "game28-match-length";
const MAX_BID = 28;

function storedMode(): number {
  try {
    return localStorage.getItem(MODE_KEY) === "6" ? 6 : 2;
  } catch {
    return 2;
  }
}

// the emote vocabulary; ids must match the server's whitelist
export const EMOTES: Record<string, string> = {
  nice: "👍",
  ouch: "😖",
  wow: "😮",
  think: "🤔",
  laugh: "😂",
  fire: "🔥",
};

export function createPlayerIdentity(
  profile: PlayerProfile,
  context: "lobby" | "table" = "lobby"
): HTMLElement {
  const identity = document.createElement("span");
  identity.className =
    `player-identity ${context}-player-identity ${profile.isBot ? "bot-player" : "human-player"}`;
  identity.style.setProperty("--avatar-accent", profile.accent);

  const avatar = document.createElement("span");
  avatar.className = "player-avatar";
  avatar.setAttribute("aria-hidden", "true");
  avatar.textContent = profile.avatar;

  const copy = document.createElement("span");
  copy.className = "player-copy";
  const name = document.createElement("span");
  name.className = "player-display-name";
  name.textContent = profile.displayName;
  copy.append(name);

  if (profile.isBot) {
    const botTag = document.createElement("span");
    botTag.className = "bot-tag";
    botTag.textContent = context === "table" ? "bot" : "BOT";
    copy.append(botTag);
  }

  if (context === "table") {
    const marker = document.createElement("span");
    marker.className = "seat-marker";
    marker.append(avatar);
    identity.append(marker, copy);
  } else {
    identity.append(avatar, copy);
  }
  return identity;
}

export class UiOverlay {
  private startScreen = element("start-screen");
  private playerNameInput = element("player-name") as HTMLInputElement;
  private roomBrowser = element("room-browser");
  private roomList = element("room-list");
  private lobbyPanel = element("lobby-panel");
  private lobbyTitle = element("lobby-title");
  private lobbySubtitle = element("lobby-subtitle");
  private seatList = element("seat-list");
  private addBotButton = element("add-bot") as HTMLButtonElement;
  private startGameButton = element("start-game") as HTMLButtonElement;
  private leaveTableButton = element("leave-table") as HTMLButtonElement;
  private lobbyHint = element("lobby-hint");
  private statusLine = element("status-line");
  private trickOutcome = element("trick-outcome");
  private tableMoment = element("table-moment");
  private tableMomentSymbol = element("table-moment-symbol");
  private tableMomentTitle = element("table-moment-title");
  private tableMomentDetail = element("table-moment-detail");
  private tableMomentHint = element("table-moment-hint");
  private bidPanel = element("bid-panel");
  private bidLabel = element("bid-label");
  private bidRaise = element("bid-raise") as HTMLButtonElement;
  private bidPass = element("bid-pass") as HTMLButtonElement;
  private trumpPanel = element("trump-panel");
  private exposeButton = element("expose-button") as HTMLButtonElement;
  private resultPanel = element("result-panel");
  private resultTitle = element("result-title");
  private resultDetail = element("result-detail");
  private resultDelta = element("result-delta");
  private resultHands = element("result-hands");
  private nextHandButton = element("next-hand") as HTMLButtonElement;
  private resultWait = element("result-wait");
  private goHomeButton = element("go-home") as HTMLButtonElement;
  private leaveMatchButton = element("leave-match") as HTMLButtonElement;
  private speedButton = element("speed-button") as HTMLButtonElement;
  private joinLinkButton = element("join-link") as HTMLButtonElement;
  private shareInviteButton = element("share-invite") as HTMLButtonElement;
  private emoteButton = element("emote-button") as HTMLButtonElement;
  private emoteStrip = element("emote-strip");
  private arrangeButton = element("arrange-button") as HTMLButtonElement;
  private arrangePanel = element("arrange-panel");
  private arrangeTrumpButton = element("arrange-trump") as HTMLButtonElement;
  private abandonPanel = element("abandon-panel");
  private abandonText = element("abandon-text");
  private reconnectPanel = element("reconnect-panel");
  private leaderboardPanel = element("leaderboard-panel");
  private leaderboardList = element("leaderboard-list");
  private uiLayer = element("ui-layer");
  private menuButton = element("menu-button") as HTMLButtonElement;
  private menuPanel = element("menu-panel");
  private motionButton = element("motion-button") as HTMLButtonElement;
  private playBotsButton = element("play-bots") as HTMLButtonElement;
  private homeAvatar = element("home-avatar");
  private homeFan = element("home-fan");
  private resultSeal = element("result-seal");
  private resultNote = element("result-note");
  private resultTeam = element("result-team");
  private bidChips = element("bid-chips");
  private lobbyControls = element("lobby-controls");
  private selectedTarget = storedMode();
  private seatedMembers: ReadonlySet<string> = new Set();
  private statusTimer: number | null = null;
  private tableMomentTimer: number | null = null;
  private leaveArmTimer: number | null = null;
  private shareResetTimer: number | null = null;
  private joinTarget: string | null = null;
  private currentRoomName: string | null = null;

  constructor(private callbacks: UiCallbacks) {
    // The opening-hand cue should be easy to read, without making the player
    // wait for it before bidding. Let their next tap/click dismiss it and
    // continue to the control they intended to use.
    document.addEventListener("pointerdown", () => {
      if (this.tableMoment.classList.contains("dismissible")) this.hideTableMoment();
    });

    // Play with friends opens a table to invite them to, of the length picked on the cards above it.
    element("create-button").addEventListener("click", () => {
      this.hide(this.startScreen);
      callbacks.onCreate(this.playerNameInput.value, this.selectedTarget);
    });
    this.playBotsButton.addEventListener("click", () => {
      callbacks.onPlayBots(this.playerNameInput.value, this.selectedTarget);
    });
    for (const card of document.querySelectorAll<HTMLButtonElement>(".mode-card")) {
      card.addEventListener("click", () => {
        this.selectMode(Number(card.dataset.target));
      });
    }
    this.selectMode(this.selectedTarget);
    this.playerNameInput.addEventListener("input", () => this.refreshHomeAvatar());
    element("browse-button").addEventListener("click", () => {
      this.show(this.roomBrowser);
      this.roomList.innerHTML = `<div id="room-empty">looking for tables…</div>`;
      this.roomBrowser.scrollIntoView({ block: "nearest" });
      callbacks.onBrowse(this.playerNameInput.value);
    });
    element("refresh-rooms").addEventListener("click", () => callbacks.onRefreshRooms());
    this.addBotButton.addEventListener("click", () => this.seatBot(this.addBotButton));
    this.startGameButton.addEventListener("click", () => callbacks.onStartGame());
    this.leaveTableButton.addEventListener("click", () => callbacks.onLeave());

    this.bidPass.addEventListener("click", () => callbacks.onPass());
    this.bidChips.addEventListener("click", (event) => {
      const chip = (event.target as HTMLElement).closest<HTMLButtonElement>("[data-bid]");
      const bid = Number(chip?.dataset.bid);
      if (chip && !Number.isNaN(bid)) callbacks.onBid(bid);
    });

    const suitCards = this.trumpPanel.querySelectorAll<HTMLButtonElement>("[data-suit]");
    suitCards.forEach((button) => {
      button.addEventListener("click", () => {
        // measured before the panel goes away, so the cards can leave from there
        const cards = [...suitCards].map((card) => ({
          suit: card.dataset.suit as Suit,
          box: card.getBoundingClientRect(),
        }));
        callbacks.onTrump(button.dataset.suit as Suit, cards);
      });
    });

    this.exposeButton.addEventListener("click", () => callbacks.onExpose());
    this.nextHandButton.addEventListener("click", () => {
      // one deal per gate: the button goes away as soon as it is used
      // (the server also drains stale repeats)
      this.hide(this.nextHandButton);
      this.resultWait.textContent = "dealing…";
      this.show(this.resultWait);
      callbacks.onNextHand();
    });
    this.goHomeButton.addEventListener("click", () => callbacks.onGoHome());

    // leaving abandons the match for all four seats, so the first click
    // only arms the button; a second within a few seconds confirms
    this.leaveMatchButton.addEventListener("click", () => {
      if (!this.leaveMatchButton.classList.contains("armed")) {
        this.leaveMatchButton.classList.add("armed");
        this.leaveMatchButton.textContent = "abandon the match?";
        this.leaveArmTimer = window.setTimeout(() => this.disarmLeaveMatch(), 4000);
        return;
      }
      this.disarmLeaveMatch();
      callbacks.onLeaveMatch();
    });

    // how-to-play overlay is pure UI — no game state involved
    const helpPanel = element("help-panel");
    element("help-button").addEventListener("click", () => this.show(helpPanel));
    element("menu-help").addEventListener("click", () => {
      this.closePopovers();
      this.show(helpPanel);
    });
    element("help-close").addEventListener("click", () => this.hide(helpPanel));

    // the standings: ask the server, the panel opens when rows arrive
    element("standings-button").addEventListener("click", () => {
      this.leaderboardList.innerHTML = `<li class="leaderboard-empty">fetching…</li>`;
      this.show(this.leaderboardPanel);
      callbacks.onStandings(this.playerNameInput.value);
    });
    element("leaderboard-close").addEventListener("click", () =>
      this.hide(this.leaderboardPanel)
    );

    // an invite link's one-tap entry into the named table; the offer is
    // one-shot so a failed join (or a later return to the start screen)
    // falls back to the ordinary create/browse actions
    this.joinLinkButton.addEventListener("click", () => {
      const target = this.joinTarget;
      if (target === null) return;
      this.setJoinTarget(null);
      this.hide(this.startScreen);
      callbacks.onJoinLink(this.playerNameInput.value, target);
    });

    this.shareInviteButton.addEventListener("click", () => {
      if (this.currentRoomName === null) return;
      const room = this.currentRoomName;
      const link = inviteLink(room);
      if (navigator.share) {
        navigator.share({
          title: "28 — join my table",
          text: `Join my 28 table "${room}"`,
          url: link,
        }).then(
          () => this.flashShared("shared ✓"),
          (error) => {
            // dismissing the share sheet is not a failure
            if ((error as DOMException | null)?.name !== "AbortError") this.copyLink(link);
          }
        );
      } else {
        this.copyLink(link);
      }
    });

    this.menuButton.addEventListener("click", () => this.togglePopover(this.menuPanel, this.menuButton));
    this.emoteButton.addEventListener("click", () => this.togglePopover(this.emoteStrip, this.emoteButton));
    this.arrangeButton.addEventListener("click", () => {
      if (this.arrangeButton.classList.contains("active")) {
        callbacks.onArrangeDone();
        return;
      }
      this.togglePopover(this.arrangePanel, this.arrangeButton);
    });
    this.arrangePanel
      .querySelectorAll<HTMLButtonElement>("[data-arrangement-preset]")
      .forEach((button) => {
        button.addEventListener("click", () => {
          this.hide(this.arrangePanel);
          this.arrangeButton.setAttribute("aria-expanded", "false");
          callbacks.onArrangePreset(button.dataset.arrangementPreset as ArrangementPreset);
        });
      });
    element("arrange-custom").addEventListener("click", () => {
      this.hide(this.arrangePanel);
      this.arrangeButton.setAttribute("aria-expanded", "false");
      callbacks.onArrangeCustom();
    });
    // A tap anywhere else closes whatever is folded out. Custom mode is
    // deliberately sticky until Done, so an ordinary card gesture cannot escape
    // into play: its panel is already closed and this leaves the mode alone.
    document.addEventListener("pointerdown", (event) => {
      const target = event.target as HTMLElement | null;
      const popover =
        "#menu-button, #menu-panel, #emote-button, #emote-strip, #arrange-button, #arrange-panel";
      if (!target?.closest?.(popover)) this.closePopovers();
    });
    element("replace-bot").addEventListener("click", () => {
      this.hideAbandonPanel();
      this.status("Calling in a bot…");
      callbacks.onReplaceBot();
    });
    element("end-match").addEventListener("click", () => {
      this.hideAbandonPanel();
      callbacks.onEndMatch();
    });
    element("reconnect-abandon").addEventListener("click", () => {
      this.hide(this.reconnectPanel);
      callbacks.onAbandonReconnect();
    });
    this.speedButton.addEventListener("click", () => this.setPacing(callbacks.onCyclePacing()));
    this.motionButton.addEventListener("click", () => this.setPacing(callbacks.onToggleMotion()));
    for (const [id, glyph] of Object.entries(EMOTES)) {
      const button = document.createElement("button");
      button.type = "button";
      button.textContent = glyph;
      button.dataset.emote = id;
      button.addEventListener("click", () => {
        this.hide(this.emoteStrip);
        callbacks.onEmote(id);
      });
      this.emoteStrip.append(button);
    }
    this.refreshHomeAvatar();
  }

  private togglePopover(panel: HTMLElement, trigger: HTMLElement) {
    const opening = panel.classList.contains("hidden");
    this.closePopovers();
    if (!opening) return;
    this.show(panel);
    trigger.setAttribute("aria-expanded", "true");
  }

  closePopovers() {
    this.hide(this.menuPanel);
    this.hide(this.emoteStrip);
    this.hide(this.arrangePanel);
    this.menuButton.setAttribute("aria-expanded", "false");
    this.emoteButton.setAttribute("aria-expanded", "false");
    this.arrangeButton.setAttribute("aria-expanded", "false");
  }

  private selectMode(target: number) {
    this.selectedTarget = target === 6 ? 6 : 2;
    document.querySelectorAll<HTMLButtonElement>(".mode-card").forEach((card) => {
      card.setAttribute("aria-pressed", String(Number(card.dataset.target) === this.selectedTarget));
    });
    try {
      localStorage.setItem(MODE_KEY, String(this.selectedTarget));
    } catch {
      // the choice just won't be remembered next time
    }
  }

  // Your seat at the home table wears your initial as soon as you type it.
  refreshHomeAvatar() {
    this.homeAvatar.textContent = this.playerNameInput.value.trim().slice(0, 1).toUpperCase();
  }

  // Lays out the small fan on the home table. The art is painted later, when
  // it has loaded; until then the cards simply lie face down.
  setHomeFan(faces: readonly HTMLCanvasElement[]) {
    const middle = (faces.length - 1) / 2;
    this.homeFan.replaceChildren(
      ...faces.map((face, index) => {
        const offset = index - middle;
        face.className = "fan-card";
        face.style.transform =
          `translate(${offset * 30}px, ${Math.abs(offset) * 4}px) rotate(${offset * 9}deg)`;
        return face;
      })
    );
  }

  // While the bots are being seated the button says so, and can't be pressed twice.
  setPlayBotsBusy(busy: boolean) {
    this.playBotsButton.disabled = busy;
    this.playBotsButton.textContent = busy ? "Setting the table…" : "Play with bots";
  }

  // Desktop fallback where no native share sheet exists: clipboard, with the
  // prompt as a last resort.
  private copyLink(link: string) {
    // optional chaining alone would silently skip BOTH branches when the
    // clipboard API is missing (http on a LAN, some webviews)
    if (navigator.clipboard?.writeText) {
      navigator.clipboard.writeText(link).then(
        () => this.flashShared("copied ✓"),
        () => window.prompt("Copy this invite link:", link)
      );
    } else {
      window.prompt("Copy this invite link:", link);
    }
  }

  private flashShared(feedback: string) {
    if (this.shareResetTimer !== null) window.clearTimeout(this.shareResetTimer);
    this.shareInviteButton.textContent = feedback;
    this.shareResetTimer = window.setTimeout(() => {
      this.shareInviteButton.textContent = "share invite link";
      this.shareResetTimer = null;
    }, 2000);
  }

  // The start screen collapses to a single "join this table" action when the
  // page was opened from an invite link; the usual actions stay underneath.
  setJoinTarget(roomName: string | null) {
    this.joinTarget = roomName;
    if (roomName === null) {
      this.hide(this.joinLinkButton);
    } else {
      this.joinLinkButton.textContent = `Join “${roomName}”`;
      this.show(this.joinLinkButton);
    }
  }

  // An emote bubble that pops near a seat and drifts away.
  showEmoteAt(x: number, y: number, emote: string) {
    const glyph = EMOTES[emote];
    if (!glyph) return;
    const bubble = document.createElement("div");
    bubble.className = "emote-bubble";
    bubble.dataset.emote = emote;
    bubble.textContent = glyph;
    bubble.style.left = `${x}px`;
    bubble.style.top = `${y}px`;
    this.uiLayer.append(bubble);
    window.setTimeout(() => bubble.remove(), 2400);
  }

  showStartScreen() {
    this.hide(this.lobbyPanel);
    this.hide(this.reconnectPanel);
    this.setPlayBotsBusy(false);
    this.refreshHomeAvatar();
    this.show(this.startScreen);
  }

  // The connection died mid-match: a holding screen while the session
  // retries the rejoin — deliberately NOT the start screen, whose create
  // and join actions could fork a second game under the same identity.
  showReconnecting() {
    this.hide(this.startScreen);
    this.hide(this.lobbyPanel);
    this.show(this.reconnectPanel);
  }

  hideReconnecting() {
    this.hide(this.reconnectPanel);
  }

  showRooms(rooms: readonly RoomInfo[]) {
    this.roomList.innerHTML = "";
    const open = rooms.filter((room) => room.members.length < 4);
    if (open.length === 0) {
      this.roomList.innerHTML = `<div id="room-empty">no open tables — create one</div>`;
      return;
    }
    for (const room of open) {
      const row = document.createElement("div");
      row.className = "room-row";
      const name = document.createElement("span");
      name.className = "room-name";
      name.textContent = room.name;
      const count = document.createElement("span");
      count.className = "room-count";
      count.textContent = `${room.members.length} of 4 · to ${room.target}`;
      const joinButton = document.createElement("button");
      joinButton.type = "button";
      joinButton.textContent = "Join";
      joinButton.addEventListener("click", () => {
        this.hide(this.startScreen);
        this.callbacks.onJoin(room.name);
      });
      row.append(name, count, joinButton);
      this.roomList.append(row);
    }
  }

  updateLobby(model: GameModel, selfEmail: string) {
    // entering a lobby always hides the start screen — sessions driven via
    // the debug bridge never go through the start-button click handlers
    this.hide(this.startScreen);
    this.currentRoomName = model.roomName;
    this.lobbyTitle.textContent = model.roomName ?? "";
    const kind = model.matchTarget === 2 ? "quick match" : "match";
    this.lobbySubtitle.textContent = model.isHost
      ? `You are hosting this table — ${kind} to ${model.matchTarget}`
      : `Waiting for the host to start — ${kind} to ${model.matchTarget}`;

    // Everyone sits where the deal will seat them, whoever is still to come.
    const host = model.members[model.members.length - 1];
    const seats = lobbySeats(model.members, selfEmail, model.lobbySeats);
    const full = model.members.length >= 4;
    this.seatList.innerHTML = "";
    let offered = false;
    for (const seat of seats) {
      const item = document.createElement("li");
      item.classList.add(`seat-view-${seat.view}`);
      const { member } = seat;
      if (member === null) {
        item.classList.add("empty-seat");
        if (model.isHost && (seat.seat !== undefined || seat.next)) {
          // each empty seat offers a bot for itself
          let button = this.addBotButton;
          if (offered) {
            button = document.createElement("button");
            button.type = "button";
            button.className = "add-bot-seat";
            button.addEventListener("click", () => this.seatBot(button));
          }
          offered = true;
          button.textContent = "Add a bot";
          button.classList.remove("hidden");
          button.disabled = false;
          if (seat.seat === undefined) delete button.dataset.seat;
          else button.dataset.seat = String(seat.seat);
          item.append(button);
        } else {
          const ring = document.createElement("span");
          ring.className = "empty-ring";
          item.append(ring, "Empty");
        }
      } else {
        const profile = playerProfile(member, model.members);
        const tags = [
          member === host ? "host" : null,
          member === selfEmail ? "you" : seat.view === 2 ? "partner" : null,
        ].filter(Boolean);
        const identity = createPlayerIdentity(profile);
        // The list is redrawn on every change: only a new arrival sits down.
        if (!this.seatedMembers.has(member)) identity.classList.add("just-seated");
        const label = document.createElement("span");
        label.className = "seat-label";
        label.textContent = tags.join(" · ");
        item.append(identity, label);
        // host can remove anyone but themselves
        if (model.isHost && member !== selfEmail) {
          const kick = document.createElement("button");
          kick.type = "button";
          kick.className = "kick-button";
          kick.title = `Remove ${profile.displayName}`;
          kick.setAttribute("aria-label", kick.title);
          kick.textContent = "✕";
          kick.addEventListener("click", () => this.callbacks.onKick(member));
          item.append(kick);
        }
      }
      this.seatList.append(item);
    }

    this.seatedMembers = new Set(model.members);
    if (full || !model.isHost) {
      this.addBotButton.classList.add("hidden");
      this.addBotButton.disabled = true;
      this.lobbyControls.append(this.addBotButton);
    }
    this.startGameButton.classList.toggle("hidden", !model.isHost);
    this.startGameButton.disabled = !full;
    const missing = 4 - model.members.length;
    this.startGameButton.textContent = full ? "Start game" : `Waiting for ${missing} more`;
    this.lobbyHint.textContent = model.isHost
      ? full
        ? "All seats taken. Ready when you are."
        : "Friends can join from their own browser, or tap the empty seat for a bot."
      : "";
    this.leaveTableButton.textContent = model.isHost ? "Close table" : "Leave table";

    this.show(this.lobbyPanel);
  }

  hideLobby() {
    this.hide(this.lobbyPanel);
    this.seatedMembers = new Set();
  }

  status(text: string, transient = false) {
    if (this.statusTimer !== null) {
      window.clearTimeout(this.statusTimer);
      this.statusTimer = null;
    }
    this.statusLine.textContent = text;
    this.statusLine.classList.toggle("visible", text.length > 0);
    this.statusLine.classList.toggle("your-turn", text === YOUR_TURN);
    if (transient && text.length > 0) {
      this.statusTimer = window.setTimeout(() => {
        this.statusLine.classList.remove("visible");
      }, 2600);
    }
  }

  // One tap, one bot: the seat stops offering until the server answers.
  private seatBot(button: HTMLButtonElement) {
    button.disabled = true;
    const seat = button.dataset.seat;
    this.callbacks.onAddBot(seat === undefined ? undefined : Number(seat));
  }

  yourTurn() {
    this.status(YOUR_TURN);
  }

  // Takes a message down if it's still the one showing, and leaves any other.
  clearStatus(...messages: readonly string[]) {
    if (messages.includes(this.statusLine.textContent ?? "")) this.status("");
  }

  // This sits over the table rather than in the busy HUD, so the player can
  // connect the winning card, seat, and point total in one glance.
  showTrickOutcome(taker: string, points: number, ours: boolean) {
    this.trickOutcome.textContent =
      points > 0 ? `${taker} the trick · +${points}` : `${taker} the trick`;
    this.trickOutcome.classList.toggle("ours", ours);
    this.trickOutcome.classList.add("visible");
  }

  hideTrickOutcome() {
    this.trickOutcome.classList.remove("visible", "ours");
  }

  // A call stays beside its player for the rest of the auction: a bid is a
  // seal, a pass is quiet text. Returns the new seal, for the effects to stamp.
  showBidEvent(playerIndex: PlayerIndex, bid: number | "pass"): HTMLElement | null {
    const action = document.getElementById(`seat-action-${playerIndex}`);
    if (!action) return null;
    document.querySelectorAll<HTMLElement>(".seat-action.raise").forEach((earlier) => {
      if (bid !== "pass") earlier.classList.add("superseded");
    });
    document.querySelectorAll(".seat-name-label.speaking").forEach((label) => {
      label.classList.remove("speaking");
    });
    action.classList.remove("pass", "raise", "superseded");
    action.classList.add("visible", bid === "pass" ? "pass" : "raise");
    const seat = action.closest(".seat-name-label");
    if (seat) {
      void (seat as HTMLElement).offsetWidth; // restart the avatar's reaction
      seat.classList.add("speaking");
    }
    if (bid === "pass") {
      action.textContent = "Pass";
      return null;
    }
    const seal = document.createElement("span");
    seal.className = "seal";
    seal.textContent = String(bid);
    action.replaceChildren(seal);
    return seal;
  }

  clearBidHistory() {
    document.querySelectorAll<HTMLElement>(".seat-action").forEach((action) => {
      action.classList.remove("visible", "raise", "pass", "superseded");
      action.replaceChildren();
      action.closest(".seat-name-label")?.classList.remove("speaking");
    });
  }

  // A table-wide punctuation mark for the deal and trump reveal. It never
  // accepts pointer input, so gameplay controls remain immediately usable.
  showTableMoment(
    symbol: string,
    title: string,
    detail: string,
    duration = 1_700,
    dismissible = false
  ) {
    if (this.tableMomentTimer !== null) window.clearTimeout(this.tableMomentTimer);
    this.tableMomentSymbol.textContent = symbol;
    this.tableMomentTitle.textContent = title;
    this.tableMomentDetail.textContent = detail;
    this.tableMomentHint.textContent = dismissible ? "tap anywhere to continue" : "";
    this.tableMoment.classList.toggle("trump", symbol !== "");
    this.tableMoment.classList.toggle("dismissible", dismissible);
    this.tableMoment.classList.remove("visible");
    void this.tableMoment.offsetWidth; // restart the entrance animation
    this.tableMoment.classList.add("visible");
    this.uiLayer.classList.add("table-announcing");
    this.tableMomentTimer = window.setTimeout(() => this.hideTableMoment(), duration);
  }

  showTrumpReveal(suit: Suit, revealer: string | null, duration: number) {
    const glyph = suitGlyph(suit);
    this.showTableMoment(glyph, trumpRevealCaption(revealer), `${glyph} is now live`, duration, true);
  }

  hideTableMoment() {
    if (this.tableMomentTimer !== null) {
      window.clearTimeout(this.tableMomentTimer);
      this.tableMomentTimer = null;
    }
    this.tableMoment.classList.remove("visible", "dismissible");
    this.uiLayer.classList.remove("table-announcing");
  }

  setPacing(pacing: GamePacingProfile) {
    this.speedButton.textContent = pacing.reducedMotion
      ? "motion reduced"
      : `speed: ${pacing.speed}`;
    this.speedButton.disabled = pacing.reducedMotion;
    this.speedButton.title = pacing.reducedMotion
      ? "Reduced motion keeps every animation short"
      : "Change animation speed";
  }

  // The toggle shows the setting in force; a device that asks for reduced
  // motion can't be overruled from here, so the toggle says so and stays put.
  setMotion(reduced: boolean, forcedByDevice: boolean) {
    this.motionButton.textContent = `Reduce motion: ${reduced ? "on" : "off"}`;
    this.motionButton.setAttribute("aria-checked", String(reduced));
    this.motionButton.disabled = forcedByDevice;
    this.motionButton.title = forcedByDevice ? "Your device asks for reduced motion" : "";
  }

  // minBid is the lowest bid the server will accept; the opener must bid.
  showBidPanel(minBid: number, isOpening: boolean) {
    this.bidLabel.textContent = isOpening ? "Your bid" : `Beat ${minBid - 1}`;
    // #bid-raise is the lowest bid that's allowed; any higher one is a chip beside it
    this.bidChips.querySelectorAll(".bid-extra").forEach((chip) => chip.remove());
    this.bidRaise.textContent = String(minBid);
    this.bidRaise.dataset.bid = String(minBid);
    this.bidRaise.setAttribute("aria-label", `Bid ${minBid}`);
    for (let value = minBid + 1; value <= MAX_BID; value++) {
      const chip = document.createElement("button");
      chip.type = "button";
      chip.className = "bid-chip bid-extra";
      chip.textContent = String(value);
      chip.dataset.bid = String(value);
      chip.setAttribute("aria-label", `Bid ${value}`);
      this.bidChips.insertBefore(chip, this.bidPass);
    }
    this.bidPass.classList.toggle("hidden", isOpening);
    this.show(this.bidPanel);
  }

  hideBidPanel() {
    this.hide(this.bidPanel);
  }

  showTrumpPanel() {
    this.show(this.trumpPanel);
  }

  hideTrumpPanel() {
    this.hide(this.trumpPanel);
  }

  setExposeVisible(visible: boolean) {
    this.exposeButton.classList.toggle("hidden", !visible);
  }

  // Between hands: the verdict, the wording of its points, and either the
  // host's deal button or a waiting note for everyone else. Returns the parts
  // the effects animate. Everything is in place straight away.
  showHandResult(copy: HandResultCopy, note: string, isHost: boolean) {
    this.resultPanel.classList.remove("match");
    this.hide(this.resultTeam);
    this.hide(this.resultTitle);
    this.resultSeal.textContent = copy.seal;
    this.resultDetail.textContent = copy.detail;
    this.resultDelta.textContent = copy.points ?? "";
    this.resultDelta.classList.toggle("hidden", copy.points === null);
    this.resultNote.textContent = note;
    this.hide(this.resultHands);
    this.hide(this.goHomeButton);
    if (isHost) {
      this.show(this.nextHandButton);
      this.hide(this.resultWait);
    } else {
      this.hide(this.nextHandButton);
      this.resultWait.textContent = "waiting for the host to deal…";
      this.show(this.resultWait);
    }
    this.showResultPanel();
    return {
      seal: this.resultSeal,
      detail: this.resultDetail,
      delta: copy.points === null ? null : this.resultDelta,
    };
  }

  showMatchOver(
    ourGamePoints: number,
    theirGamePoints: number,
    weWon: boolean,
    hands: number,
    target: number,
    handResults: readonly number[],
    team: readonly PlayerProfile[],
    finalHand = ""
  ) {
    this.resultPanel.classList.add("match");
    this.resultTeam.replaceChildren(
      ...team.map((profile) => {
        const ring = document.createElement("div");
        ring.className = `team-ring ${profile.isBot ? "bot" : "human"}`;
        ring.textContent = profile.avatar;
        ring.setAttribute("aria-label", profile.displayName);
        return ring;
      })
    );
    this.resultTeam.classList.toggle("hidden", team.length === 0);
    this.resultTitle.textContent = weWon ? "You win the match" : "Match lost";
    this.show(this.resultTitle);
    this.resultSeal.textContent = `${scoreText(ourGamePoints)} – ${scoreText(theirGamePoints)}`;
    this.resultDetail.textContent =
      `first to ${target} · ${hands} hand${hands === 1 ? "" : "s"} played`;
    this.resultNote.textContent = finalHand;
    // hand-by-hand swing chips — only when we saw every hand (a mid-match
    // reconnect starts the tally over)
    this.resultHands.innerHTML = "";
    if (handResults.length === hands && hands > 0) {
      for (const swing of handResults) {
        const chip = document.createElement("span");
        chip.className = `hand-chip ${swing > 0 ? "gain" : "loss"}`;
        chip.textContent = signedText(swing);
        this.resultHands.append(chip);
      }
      this.show(this.resultHands);
    } else {
      this.hide(this.resultHands);
    }
    this.hide(this.nextHandButton);
    this.hide(this.resultWait);
    this.hide(this.resultDelta);
    this.show(this.goHomeButton);
    this.showResultPanel();
    return { team: this.resultTeam, title: this.resultTitle, seal: this.resultSeal };
  }

  private showResultPanel() {
    this.hideTableMoment();
    this.show(this.resultPanel);
    this.uiLayer.classList.add("result-up");
  }

  hideResult() {
    this.hideTableMoment();
    this.hide(this.resultPanel);
    this.uiLayer.classList.remove("result-up");
  }

  // Someone walked out (or timed out of their reconnect grace): the
  // survivors choose between a bot replacement and ending the match.
  showAbandonPanel(name: string) {
    this.abandonText.textContent = `${name} abandoned the match.`;
    this.show(this.abandonPanel);
  }

  hideAbandonPanel() {
    this.hide(this.abandonPanel);
  }

  // Between hands, the deal duty can move when a seat is replaced: flip
  // the open result panel between the deal button and the waiting note.
  setNextHandRole(isActingHost: boolean) {
    if (this.resultPanel.classList.contains("hidden")) return;
    if (this.goHomeButton.classList.contains("hidden") === false) return; // match-over panel
    if (isActingHost) {
      this.show(this.nextHandButton);
      this.hide(this.resultWait);
    } else {
      this.hide(this.nextHandButton);
      this.resultWait.textContent = "waiting for the host to deal…";
      this.show(this.resultWait);
    }
  }

  // A dead session can't refresh the list: clear it so the next browse
  // starts fresh instead of showing stale tables.
  resetRoomBrowser() {
    this.hide(this.roomBrowser);
    this.roomList.innerHTML = "";
  }

  // Completed-match standings, best first.
  showLeaderboard(rows: ReadonlyArray<{ name: string; played: number; won: number }>) {
    this.leaderboardList.innerHTML = "";
    if (rows.length === 0) {
      this.leaderboardList.innerHTML =
        `<li class="leaderboard-empty">no completed matches yet — finish one!</li>`;
    }
    rows.forEach((row, index) => {
      const item = document.createElement("li");
      const rank = document.createElement("span");
      rank.className = "leaderboard-rank";
      rank.textContent = `${index + 1}.`;
      const name = document.createElement("span");
      name.className = "leaderboard-name";
      name.textContent = row.name;
      const score = document.createElement("span");
      score.className = "leaderboard-score";
      score.textContent = `${row.won} of ${row.played} won`;
      item.append(rank, name, score);
      this.leaderboardList.append(item);
    });
    this.show(this.leaderboardPanel);
  }

  // The in-game escape hatches (leave + emotes): visible only while a
  // match is in progress.
  setLeaveMatchVisible(visible: boolean) {
    this.disarmLeaveMatch();
    this.leaveMatchButton.classList.toggle("hidden", !visible);
    this.emoteButton.classList.toggle("hidden", !visible);
    if (!visible) this.hide(this.emoteStrip);
  }

  // The match HUD shows from the first deal until we're back at the home page.
  setInMatch(inMatch: boolean) {
    this.uiLayer.classList.toggle("in-match", inMatch);
    this.closePopovers();
  }

  setArrangeAvailable(available: boolean, trumpKnown: boolean) {
    this.arrangeButton.classList.toggle("hidden", !available);
    this.arrangeTrumpButton.disabled = !trumpKnown;
    this.arrangeTrumpButton.title = trumpKnown
      ? "Put the trump suit first"
      : "Available when trump is known";
    if (!available) {
      this.hide(this.arrangePanel);
      this.setArranging(false);
    }
  }

  setArranging(active: boolean) {
    this.arrangeButton.classList.toggle("active", active);
    const label = active ? "Finish arranging" : "Arrange cards";
    this.arrangeButton.setAttribute("aria-label", label);
    this.arrangeButton.title = label;
    this.arrangeButton.setAttribute("aria-expanded", "false");
    if (active) this.hide(this.arrangePanel);
  }

  setArrangementSelection(mode: ArrangementMode) {
    this.arrangePanel
      .querySelectorAll<HTMLButtonElement>("[data-arrangement-mode]")
      .forEach((button) => {
        const selected = button.dataset.arrangementMode === mode;
        button.classList.toggle("selected", selected);
        button.setAttribute("aria-checked", String(selected));
      });
  }

  private disarmLeaveMatch() {
    if (this.leaveArmTimer !== null) {
      window.clearTimeout(this.leaveArmTimer);
      this.leaveArmTimer = null;
    }
    this.leaveMatchButton.classList.remove("armed");
    this.leaveMatchButton.textContent = "leave match";
  }

  private show(el: HTMLElement) {
    el.classList.remove("hidden");
  }

  private hide(el: HTMLElement) {
    el.classList.add("hidden");
  }
}

function element(id: string): HTMLElement {
  const el = document.getElementById(id);
  if (!el) throw new Error(`UiOverlay: missing #${id}`);
  return el;
}

export function trumpRevealCaption(revealer: string | null): string {
  return revealer === null ? "Trump revealed" : `${revealer} revealed trump`;
}

// A shareable URL that drops a friend straight into this table. Dev-server
// params (?port/?server) ride along so local links keep working.
export function inviteLink(roomName: string): string {
  const params = new URLSearchParams();
  const current = new URLSearchParams(window.location.search);
  for (const key of ["server", "port"]) {
    const value = current.get(key);
    if (value) params.set(key, value);
  }
  params.set("join", roomName);
  return `${window.location.origin}${window.location.pathname}?${params.toString()}`;
}
