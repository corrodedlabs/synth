import { Suit } from "../game/GameModel";

export interface UiCallbacks {
  readonly onStart: () => void;
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

export class UiOverlay {
  private startScreen = element("start-screen");
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

  constructor(callbacks: UiCallbacks) {
    element("play-button").addEventListener("click", () => {
      this.hide(this.startScreen);
      callbacks.onStart();
    });

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
  }

  showStartScreen() {
    this.show(this.startScreen);
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

  showBidPanel(currentBid: number) {
    const nextBid = Math.min(currentBid + 1, 28);
    this.bidLabel.textContent = `Bid stands at ${currentBid}`;
    this.bidRaise.textContent = `Bid ${nextBid}`;
    this.bidRaise.dataset.bid = String(nextBid);
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
