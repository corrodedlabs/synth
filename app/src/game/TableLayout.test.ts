import { describe, expect, it } from "vitest";
import { responsiveTableLayout } from "./TableLayout";

const viewports = [
  { width: 1440, height: 900 },
  { width: 1280, height: 720 },
  { width: 1024, height: 600 },
  { width: 820, height: 600 },
  { width: 430, height: 932 },
  { width: 390, height: 844 },
  { width: 360, height: 740 },
  { width: 844, height: 390 },
  { width: 640, height: 360 },
];

describe("responsiveTableLayout", () => {
  it("gives back what the closer camera adds to the cards as a compact window squares up", () => {
    expect(responsiveTableLayout(390, 844).handCardScale).toBe(1.12);
    expect(responsiveTableLayout(360, 640).handCardScale).toBe(1.08);
    expect(responsiveTableLayout(673, 841).handCardScale).toBeCloseTo((1.12 * 0.65 * 841) / 673);
    expect(responsiveTableLayout(700, 700).tableCardScale).toBeCloseTo(1.08 * 0.7);
  });

  it("keeps the side lanes off a seat's name in wide compact windows, and phones where they were", () => {
    expect(responsiveTableLayout(700, 900).playedCards[1].x).toBeCloseTo(700 - 134);
    expect(responsiveTableLayout(700, 900).playedCards[3].x).toBeCloseTo(134);
    expect(responsiveTableLayout(412, 915).playedCards[3].x).toBe(125);
  });

  it("lifts the hand off the status line as a short window grows", () => {
    expect(responsiveTableLayout(1024, 600).handCenter.y).toBe(600 - 105);
    expect(responsiveTableLayout(841, 673).handCenter.y).toBeCloseTo(673 - 673 * 0.165);
  });

  it.each(viewports)("keeps side seats and their cards on one row at $width×$height", ({ width, height }) => {
    const layout = responsiveTableLayout(width, height);
    expect(layout.playedCards[1].y).toBe(layout.opponentSeats[1].y);
    expect(layout.playedCards[3].y).toBe(layout.opponentSeats[3].y);
    expect(layout.playedCards[1].x).toBeLessThan(layout.opponentSeats[1].x);
    expect(layout.playedCards[3].x).toBeGreaterThan(layout.opponentSeats[3].x);
  });

  it.each(viewports)("keeps the top lane centered and below its identity at $width×$height", ({ width, height }) => {
    const layout = responsiveTableLayout(width, height);
    expect(layout.playedCards[2].x).toBe(layout.opponentSeats[2].x);
    expect(layout.playedCards[2].y - layout.opponentSeats[2].y).toBeGreaterThanOrEqual(105);
  });

  it.each(viewports)("reserves a separate band between the bottom play and hand at $width×$height", ({ width, height }) => {
    const layout = responsiveTableLayout(width, height);
    const minimumBand = width > height && height <= 550 ? 65 : 100;
    expect(layout.handCenter.y - layout.playedCards[0].y).toBeGreaterThanOrEqual(minimumBand);
    expect(layout.handCenter.y).toBeLessThan(height);
  });
});
