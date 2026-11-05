import type { Suit } from "../game/GameModel";
import { bidOwnerLabel, lineFraction, pipRow, progressLabel, scoreText, type PipRow } from "./HudModel";

const SUIT_GLYPHS: Record<Suit, string> = {
  hearts: "♥︎",
  diamonds: "♦︎",
  clubs: "♣︎",
  spades: "♠︎",
};

export const suitGlyph = (suit: Suit) => SUIT_GLYPHS[suit];
export const suitIsRed = (suit: Suit) => suit === "hearts" || suit === "diamonds";

export interface HudTrump {
  // nothing on the table yet: no trump has been chosen
  readonly placed: boolean;
  readonly suit: Suit | null;
  readonly exposed: boolean;
}

export interface HudView {
  readonly matchUs: number;
  readonly matchThem: number;
  readonly target: number;
  readonly ourPoints: number;
  readonly theirPoints: number;
  readonly bid: number | null;
  readonly bidder: number | null;
  readonly bidderName: string | null;
  readonly trump: HudTrump;
}

// The top bar's score, hand line and contract, drawn from plain values. Safe
// to call as often as you like: it only touches what changed, so a pip or the
// brush line can animate without being rebuilt underneath itself.
export class ScoreHud {
  private root = document.getElementById("score-display") as HTMLElement;
  private usRow = document.getElementById("pips-us") as HTMLElement;
  private themRow = document.getElementById("pips-them") as HTMLElement;
  private progress = document.getElementById("hand-progress") as HTMLElement;
  private fill = document.getElementById("brush-fill") as unknown as SVGPathElement;
  private notch = document.getElementById("brush-notch") as unknown as SVGPathElement;
  private label = document.getElementById("hand-progress-label") as HTMLElement;
  private contract = document.getElementById("contract") as HTMLElement;
  private seal = document.getElementById("bid-seal") as HTMLElement;
  private whose = document.getElementById("contract-whose") as HTMLElement;
  private mini = document.getElementById("trump-indicator") as HTMLElement;

  render(view: HudView) {
    this.renderRow(this.usRow, pipRow(view.matchUs, view.target));
    this.renderRow(this.themRow, pipRow(view.matchThem, view.target));

    const bidIsOurs = view.bidder === 0 || view.bidder === 2;
    const hasBid = view.bid !== null;
    this.progress.classList.toggle("hidden", !hasBid);
    if (hasBid) {
      const fraction = lineFraction(view.ourPoints);
      this.fill.setAttribute("stroke-dasharray", `${fraction} 1`);
      this.fill.classList.toggle("empty", fraction === 0);
      this.notch.style.visibility = bidIsOurs ? "visible" : "hidden";
      this.notch.setAttribute("d", `M${2 + lineFraction(view.bid ?? 0) * 172} 1v13`);
      this.label.textContent = progressLabel(view.ourPoints, view.theirPoints, view.bid, bidIsOurs);
    }

    this.root.setAttribute(
      "aria-label",
      `Match: you ${scoreText(view.matchUs)}, them ${scoreText(view.matchThem)}, first to ${view.target}.` +
        (hasBid ? ` This hand: you ${view.ourPoints}, them ${view.theirPoints}.` : "")
    );

    this.contract.classList.toggle("empty", !hasBid);
    this.seal.textContent = view.bid === null ? "" : String(view.bid);
    this.whose.textContent = bidOwnerLabel(view.bidder, view.bidderName);
    this.renderTrump(view.trump);
  }

  // The pip a point flying in from the result should aim for: the last one
  // filled on that team's row, or the row itself if none are.
  pipFor(team: "us" | "them"): HTMLElement {
    const row = team === "us" ? this.usRow : this.themRow;
    const filled = row.querySelectorAll<HTMLElement>(".pip.on");
    return filled[filled.length - 1] ?? row;
  }

  trumpCard(): HTMLElement {
    return this.mini;
  }

  bidSeal(): HTMLElement {
    return this.seal;
  }

  private renderRow(row: HTMLElement, pips: PipRow) {
    const existing = row.querySelectorAll<HTMLElement>(".pip");
    if (existing.length !== pips.total) {
      row.replaceChildren(
        ...Array.from({ length: pips.total }, () => {
          const pip = document.createElement("i");
          pip.className = "pip";
          return pip;
        })
      );
    }
    row.querySelectorAll<HTMLElement>(".pip").forEach((pip, index) => {
      pip.classList.toggle("on", index < pips.filled);
    });
    let number = row.querySelector<HTMLElement>(".pip-number");
    if (!number) {
      number = document.createElement("span");
      number.className = "pip-number";
      row.append(number);
    }
    number.textContent = pips.number;
  }

  private renderTrump({ placed, suit, exposed }: HudTrump) {
    this.mini.classList.toggle("unset", !placed);
    const faceUp = placed && exposed && suit !== null;
    this.mini.classList.toggle("face-down", placed && !faceUp);
    this.mini.replaceChildren();
    this.mini.removeAttribute("title");
    if (!placed) return;
    if (faceUp && suit) {
      const glyph = document.createElement("span");
      glyph.className = suitIsRed(suit) ? "red-suit" : "black-suit";
      glyph.textContent = suitGlyph(suit);
      this.mini.append(glyph);
      this.mini.setAttribute("aria-label", `Trump: ${suit}`);
    } else if (suit) {
      const note = document.createElement("span");
      note.className = `known-suit ${suitIsRed(suit) ? "red-suit" : "black-suit"}`;
      note.textContent = suitGlyph(suit);
      this.mini.append(note);
      this.mini.title = "Trump is hidden from the table: only you know it";
      this.mini.setAttribute("aria-label", `Trump: ${suit}, still hidden from the table`);
    } else {
      this.mini.setAttribute("aria-label", "Trump: hidden");
    }
  }
}
