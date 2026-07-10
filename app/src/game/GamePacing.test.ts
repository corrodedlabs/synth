import { describe, expect, it } from "vitest";
import { gamePacing } from "./GamePacing";

describe("gamePacing", () => {
  it("keeps each normal-speed presentation stage in order", () => {
    const pacing = gamePacing();
    expect(pacing.cardPauseMs).toBeGreaterThan(pacing.cardTravelMs);
    expect(pacing.dealCompleteMs).toBeGreaterThan(pacing.dealCardStaggerMs * 3);
    expect(pacing.trickResolveMs).toBeGreaterThanOrEqual(
      pacing.winnerRevealMs + pacing.cardCollectMs + pacing.scoreLandingMs
    );
    expect(pacing.trickReadMs).toBeGreaterThan(1_000);
  });

  it("speeds up quick mode without removing the winner moment", () => {
    const standard = gamePacing("standard");
    const quick = gamePacing("quick");
    expect(quick.cardTravelMs).toBeLessThan(standard.cardTravelMs);
    expect(quick.winnerRevealMs).toBeGreaterThan(0);
    expect(quick.cardCollectMs).toBeGreaterThan(0);
  });

  it("keeps a readable outcome without movement for reduced motion", () => {
    const pacing = gamePacing("quick", true);
    expect(pacing.reducedMotion).toBe(true);
    expect(pacing.cardTravelMs).toBe(0);
    expect(pacing.cardCollectMs).toBe(0);
    expect(pacing.dealCompleteMs).toBe(0);
    expect(pacing.winnerRevealMs).toBeGreaterThanOrEqual(900);
  });
});
