import { GameModel, Suit } from "../game/GameModel";
import { RoomInfo } from "../net/protocol";

export interface UiCallbacks {
  readonly onCreate: (playerName: string) => void;
  readonly onBrowse: (playerName: string) => void;
  readonly onJoin: (roomName: string) => void;
  readonly onRefreshRooms: () => void;
  readonly onAddBot: () => void;
  readonly onStartGame: () => void;
  readonly onLeave: () => void;
  readonly onKick: (member: string) => void;
  readonly onBid: (bid: number) => void;
  readonly onPass: () => void;
  readonly onTrump: (suit: Suit) => void;
  readonly onExpose: () => void;
  readonly onPlayAgain: () => void;
}

export const SEAT_NAMES = ["You", "Right", "Partner", "Left"] as const;

const SUIT_GLYPHS: Record<Suit, string> = {
  hearts: "♥",
  diamonds: "♦",
  clubs: "♣",
  spades: "♠",
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
  private trumpIndicator = element("trump-indicator");
  private statusTimer: number | null = null;

  constructor(private callbacks: UiCallbacks) {
    element("create-button").addEventListener("click", () => {
      this.hide(this.startScreen);
      callbacks.onCreate(this.playerNameInput.value);
    });
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
    element("play-again").addEventListener("click", () => callbacks.onPlayAgain());

    // how-to-play overlay is pure UI — no game state involved
    const helpPanel = element("help-panel");
    element("help-button").addEventListener("click", () => this.show(helpPanel));
    element("help-close").addEventListener("click", () => this.hide(helpPanel));
  }

  showStartScreen() {
    this.hide(this.lobbyPanel);
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
      count.textContent = `${room.members.length} of 4`;
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
    this.lobbyTitle.textContent = model.roomName ?? "";
    this.lobbySubtitle.textContent = model.isHost
      ? "You are hosting this table"
      : "Waiting for the host to start";

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

  showResult(ourPoints: number, theirPoints: number, detail: string) {
    this.resultTitle.textContent = `${ourPoints} — ${theirPoints}`;
    this.resultDetail.textContent = detail;
    this.show(this.resultPanel);
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
