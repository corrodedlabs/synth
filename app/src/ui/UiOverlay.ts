import { GameModel, Suit } from "../game/GameModel";
import { RoomInfo } from "../net/protocol";

export interface UiCallbacks {
  readonly onCreate: (playerName: string, target: number) => void;
  readonly onBrowse: (playerName: string) => void;
  readonly onJoin: (roomName: string) => void;
  readonly onJoinLink: (playerName: string, roomName: string) => void;
  readonly onEmote: (emote: string) => void;
  readonly onStandings: (playerName: string) => void;
  readonly onRefreshRooms: () => void;
  readonly onAddBot: () => void;
  readonly onStartGame: () => void;
  readonly onLeave: () => void;
  readonly onKick: (member: string) => void;
  readonly onBid: (bid: number) => void;
  readonly onPass: () => void;
  readonly onTrump: (suit: Suit) => void;
  readonly onExpose: () => void;
  readonly onNextHand: () => void;
  readonly onLeaveMatch: () => void;
  readonly onReplaceBot: () => void;
  readonly onEndMatch: () => void;
  readonly onPlayAgain: () => void;
}

export const SEAT_NAMES = ["You", "Right", "Partner", "Left"] as const;

const SUIT_GLYPHS: Record<Suit, string> = {
  hearts: "♥",
  diamonds: "♦",
  clubs: "♣",
  spades: "♠",
};

// the emote vocabulary; ids must match the server's whitelist
export const EMOTES: Record<string, string> = {
  nice: "👍",
  ouch: "😖",
  wow: "😮",
  think: "🤔",
  laugh: "😂",
  fire: "🔥",
};

// "antash-x7k2p9@game.local" → "antash", "bot-2" → "Bot 2"
export function memberDisplayName(member: string): string {
  const botMatch = member.match(/^bot-(\d+)$/);
  if (botMatch) return `Bot ${botMatch[1]}`;
  return member.split("@")[0].replace(/-[a-z0-9]{6}$/, "");
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
  private bidPanel = element("bid-panel");
  private bidLabel = element("bid-label");
  private bidRaise = element("bid-raise") as HTMLButtonElement;
  private bidPass = element("bid-pass") as HTMLButtonElement;
  private trumpPanel = element("trump-panel");
  private exposeButton = element("expose-button") as HTMLButtonElement;
  private resultPanel = element("result-panel");
  private resultTitle = element("result-title");
  private resultDetail = element("result-detail");
  private nextHandButton = element("next-hand") as HTMLButtonElement;
  private resultWait = element("result-wait");
  private playAgainButton = element("play-again") as HTMLButtonElement;
  private leaveMatchButton = element("leave-match") as HTMLButtonElement;
  private joinLinkButton = element("join-link") as HTMLButtonElement;
  private copyInviteButton = element("copy-invite") as HTMLButtonElement;
  private emoteButton = element("emote-button") as HTMLButtonElement;
  private emoteStrip = element("emote-strip");
  private abandonPanel = element("abandon-panel");
  private abandonText = element("abandon-text");
  private createPanel = element("create-panel");
  private leaderboardPanel = element("leaderboard-panel");
  private leaderboardList = element("leaderboard-list");
  private uiLayer = element("ui-layer");
  private trumpIndicator = element("trump-indicator");
  private statusTimer: number | null = null;
  private leaveArmTimer: number | null = null;
  private copyResetTimer: number | null = null;
  private joinTarget: string | null = null;
  private currentRoomName: string | null = null;

  constructor(private callbacks: UiCallbacks) {
    // "Create a table" first asks what the table will play to; picking a
    // type is what actually creates it
    element("create-button").addEventListener("click", () => {
      this.show(this.createPanel);
    });
    element("create-cancel").addEventListener("click", () => {
      this.hide(this.createPanel);
    });
    for (const button of document.querySelectorAll<HTMLButtonElement>(
      "#create-panel .table-choice"
    )) {
      button.addEventListener("click", () => {
        const target = Number(button.dataset.target);
        this.hide(this.createPanel);
        this.hide(this.startScreen);
        callbacks.onCreate(
          this.playerNameInput.value,
          Number.isInteger(target) && target >= 1 ? target : 6
        );
      });
    }
    element("browse-button").addEventListener("click", () => {
      this.show(this.roomBrowser);
      this.roomList.innerHTML = `<div id="room-empty">looking for tables…</div>`;
      callbacks.onBrowse(this.playerNameInput.value);
    });
    element("refresh-rooms").addEventListener("click", () => callbacks.onRefreshRooms());
    this.addBotButton.addEventListener("click", () => callbacks.onAddBot());
    this.startGameButton.addEventListener("click", () => callbacks.onStartGame());
    this.leaveTableButton.addEventListener("click", () => callbacks.onLeave());

    this.bidPass.addEventListener("click", () => callbacks.onPass());
    this.bidRaise.addEventListener("click", () => {
      const bid = Number(this.bidRaise.dataset.bid);
      if (!Number.isNaN(bid)) callbacks.onBid(bid);
    });

    this.trumpPanel.querySelectorAll<HTMLButtonElement>("[data-suit]").forEach((button) => {
      button.addEventListener("click", () => callbacks.onTrump(button.dataset.suit as Suit));
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
    this.playAgainButton.addEventListener("click", () => callbacks.onPlayAgain());

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

    this.copyInviteButton.addEventListener("click", () => {
      if (this.currentRoomName === null) return;
      const link = inviteLink(this.currentRoomName);
      // optional chaining alone would silently skip BOTH branches when the
      // clipboard API is missing (http on a LAN, some webviews)
      if (navigator.clipboard?.writeText) {
        navigator.clipboard.writeText(link).then(
          () => this.flashCopied(),
          () => window.prompt("Copy this invite link:", link)
        );
      } else {
        window.prompt("Copy this invite link:", link);
      }
    });

    this.emoteButton.addEventListener("click", () => {
      this.emoteStrip.classList.toggle("hidden");
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
  }

  private flashCopied() {
    if (this.copyResetTimer !== null) window.clearTimeout(this.copyResetTimer);
    this.copyInviteButton.textContent = "copied ✓";
    this.copyResetTimer = window.setTimeout(() => {
      this.copyInviteButton.textContent = "copy invite link";
      this.copyResetTimer = null;
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
    this.hide(this.createPanel); // never a stale question over a fresh start
    this.show(this.startScreen);
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

    // members arrive in server order (newest first, host last); show host first
    const seated = [...model.members].reverse();
    this.seatList.innerHTML = "";
    for (let seat = 0; seat < 4; seat++) {
      const item = document.createElement("li");
      const member = seated[seat];
      if (member === undefined) {
        item.className = "empty-seat";
        item.textContent = "empty seat";
      } else {
        const tags = [
          seat === 0 ? "host" : null,
          member === selfEmail ? "you" : null,
        ].filter(Boolean);
        const name = document.createElement("span");
        name.textContent = memberDisplayName(member);
        const right = document.createElement("span");
        right.className = "seat-tags";
        const label = document.createElement("span");
        label.className = "seat-label";
        label.textContent = tags.join(" · ");
        right.append(label);
        // host can remove anyone but themselves
        if (model.isHost && member !== selfEmail) {
          const kick = document.createElement("button");
          kick.type = "button";
          kick.className = "kick-button";
          kick.title = `Remove ${memberDisplayName(member)}`;
          kick.textContent = "✕";
          kick.addEventListener("click", () => this.callbacks.onKick(member));
          right.append(kick);
        }
        item.append(name, right);
      }
      this.seatList.append(item);
    }

    const full = model.members.length === 4;
    this.addBotButton.classList.toggle("hidden", !model.isHost);
    this.addBotButton.disabled = full;
    this.startGameButton.classList.toggle("hidden", !model.isHost);
    this.startGameButton.disabled = !full;
    this.lobbyHint.textContent = model.isHost
      ? full
        ? "All seats taken — ready when you are."
        : "Friends can join from their own browser, or fill seats with bots."
      : "";
    this.leaveTableButton.textContent = model.isHost ? "Close table" : "Leave table";

    this.show(this.lobbyPanel);
  }

  hideLobby() {
    this.hide(this.lobbyPanel);
  }

  status(text: string, transient = false) {
    if (this.statusTimer !== null) {
      window.clearTimeout(this.statusTimer);
      this.statusTimer = null;
    }
    this.statusLine.textContent = text;
    this.statusLine.classList.toggle("visible", text.length > 0);
    if (transient && text.length > 0) {
      this.statusTimer = window.setTimeout(() => {
        this.statusLine.classList.remove("visible");
      }, 2600);
    }
  }

  // minBid is the lowest bid the server will accept; the opener must bid.
  showBidPanel(minBid: number, isOpening: boolean) {
    this.bidLabel.textContent = isOpening
      ? "Open the bidding"
      : `Bid stands at ${minBid - 1}`;
    this.bidRaise.textContent = `Bid ${minBid}`;
    this.bidRaise.dataset.bid = String(minBid);
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

  setTrump(suit: Suit | null, exposed: boolean) {
    if (suit === null && !exposed) {
      this.trumpIndicator.textContent = "";
      return;
    }
    const glyph = suit ? SUIT_GLYPHS[suit] : "?";
    const suitClass = suit === "hearts" || suit === "diamonds" ? "red-suit" : "black-suit";
    this.trumpIndicator.innerHTML = exposed
      ? `Trump: <span class="${suitClass}">${glyph}</span>`
      : suit
        ? `Trump (hidden): <span class="${suitClass}">${glyph}</span>`
        : "Trump: hidden";
  }

  // Between hands: the hand's card points, the verdict + match score, and
  // either the host's deal button or a waiting note for everyone else.
  showHandResult(ourPoints: number, theirPoints: number, detail: string, isHost: boolean) {
    this.resultTitle.textContent = `${ourPoints} — ${theirPoints}`;
    this.resultDetail.textContent = detail;
    this.hide(this.playAgainButton);
    if (isHost) {
      this.show(this.nextHandButton);
      this.hide(this.resultWait);
    } else {
      this.hide(this.nextHandButton);
      this.resultWait.textContent = "waiting for the host to deal…";
      this.show(this.resultWait);
    }
    this.show(this.resultPanel);
  }

  showMatchOver(ourGamePoints: number, theirGamePoints: number, weWon: boolean, hands: number) {
    this.resultTitle.textContent = weWon ? "Match won" : "Match lost";
    this.resultDetail.textContent =
      `${ourGamePoints} — ${theirGamePoints} in game points after ` +
      `${hands} hand${hands === 1 ? "" : "s"}`;
    this.hide(this.nextHandButton);
    this.hide(this.resultWait);
    this.show(this.playAgainButton);
    this.show(this.resultPanel);
  }

  hideResult() {
    this.hide(this.resultPanel);
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
    if (this.playAgainButton.classList.contains("hidden") === false) return; // match-over panel
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
