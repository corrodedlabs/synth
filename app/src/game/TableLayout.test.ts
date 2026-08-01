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
