import { beforeEach, describe, expect, it, vi } from "vitest";
import { SoundService } from "./Sound";

// The vitest environment is node: no AudioContext, so the service runs in
// its no-op mode — these tests cover the state machinery around it
// (attempt counters, mute persistence, the toggle button contract).

let store: Map<string, string>;

beforeEach(() => {
  store = new Map<string, string>();
  vi.stubGlobal("localStorage", {
    getItem: (key: string) => store.get(key) ?? null,
    setItem: (key: string, value: string) => void store.set(key, value),
  });
  vi.stubGlobal("window", { addEventListener: () => undefined });
});

function fakeButton() {
  const button = {
    textContent: "",
    click: () => undefined as void,
    addEventListener: (_event: string, handler: () => void) => {
      button.click = handler;
    },
  };
  return button;
}

describe("SoundService", () => {
  it("counts every attempt even when no audio backend exists", () => {
    const sound = new SoundService();
    sound.cardPlay();
    sound.cardPlay();
    sound.deal(4);
    sound.trickSweep();
    sound.yourTurn();
    sound.bidTick();
    sound.trumpReveal();
    expect(sound.played).toEqual({
      cardPlay: 2,
      deal: 1,
      trickSweep: 1,
      yourTurn: 1,
      bidTick: 1,
      trumpReveal: 1,
    });
  });

  it("counts wins and losses separately", () => {
    const sound = new SoundService();
    sound.result(true);
    sound.result(false);
    sound.result(false);
    expect(sound.played.win).toBe(1);
    expect(sound.played.lose).toBe(2);
  });

  it("persists the mute preference across sessions", () => {
    const sound = new SoundService();
    expect(sound.isMuted()).toBe(false);
    sound.toggle();
    expect(sound.isMuted()).toBe(true);
    expect(store.get("game28-muted")).toBe("1");

    const next = new SoundService();
    expect(next.isMuted()).toBe(true);
    next.toggle();
    expect(new SoundService().isMuted()).toBe(false);
  });

  it("drives the toggle button label and reacts to its clicks", () => {
    const sound = new SoundService();
    const button = fakeButton();
    sound.bindToggle(button as unknown as HTMLElement);
    expect(button.textContent).toBe("sound on");

    button.click();
    expect(sound.isMuted()).toBe(true);
    expect(button.textContent).toBe("sound off");

    button.click();
    expect(sound.isMuted()).toBe(false);
    expect(button.textContent).toBe("sound on");
  });

  it("still counts attempts while muted", () => {
    const sound = new SoundService();
    sound.toggle();
    sound.cardPlay();
    expect(sound.played.cardPlay).toBe(1);
  });
});
