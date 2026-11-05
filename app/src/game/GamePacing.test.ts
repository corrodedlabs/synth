import { afterEach, describe, expect, it, vi } from "vitest";
import { GamePacing, gamePacing } from "./GamePacing";

describe("gamePacing", () => {
  it("keeps each normal-speed presentation stage in order", () => {
    const pacing = gamePacing();
    expect(pacing.bidPauseMs).toBe(3_000);
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
    const relaxed = gamePacing("relaxed");
    expect(quick.bidPauseMs).toBe(2_160);
    expect(quick.bidPauseMs).toBeLessThan(standard.bidPauseMs);
    expect(standard.bidPauseMs).toBeLessThan(relaxed.bidPauseMs);
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
    expect(pacing.bidPauseMs).toBe(3_000);
    expect(pacing.winnerRevealMs).toBeGreaterThanOrEqual(900);
  });
});

describe("GamePacing reduce motion", () => {
  const store = new Map<string, string>();
  const stub = (deviceReduces: boolean) => {
    vi.stubGlobal("localStorage", {
      getItem: (key: string) => store.get(key) ?? null,
      setItem: (key: string, value: string) => void store.set(key, value),
    });
    vi.stubGlobal("window", { matchMedia: () => ({ matches: deviceReduces }) });
  };

  afterEach(() => {
    store.clear();
    vi.unstubAllGlobals();
  });

  it("turns effects off with the toggle and remembers it", () => {
    stub(false);
    const pacing = new GamePacing();
    expect(pacing.profile().effectScale).toBe(1);
    expect(pacing.toggleReduceMotion().effectScale).toBe(0);
    expect(new GamePacing().reducedMotion()).toBe(true);
    expect(pacing.toggleReduceMotion().reducedMotion).toBe(false);
    expect(new GamePacing().reducedMotion()).toBe(false);
  });

  it("honours the device setting whatever the toggle says", () => {
    stub(true);
    const pacing = new GamePacing();
    expect(pacing.motionForcedByDevice()).toBe(true);
    expect(pacing.reducedMotion()).toBe(true);
    pacing.toggleReduceMotion();
    pacing.toggleReduceMotion();
    expect(pacing.reducedMotion()).toBe(true);
  });

  it("scales effects with the speed setting", () => {
    expect(gamePacing("quick").effectScale).toBeLessThan(gamePacing("relaxed").effectScale);
    expect(gamePacing("relaxed", true).effectScale).toBe(0);
    expect(gamePacing("quick").cardLandMs).toBeGreaterThan(0);
    expect(gamePacing("quick", true).cardLandMs).toBe(0);
  });
});
