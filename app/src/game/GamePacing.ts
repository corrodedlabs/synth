export type GameSpeed = "relaxed" | "standard" | "quick";

export interface GamePacingProfile {
  readonly speed: GameSpeed;
  readonly reducedMotion: boolean;
  // Event-loop delays. These ensure a server event cannot visually overtake
  // the animation that explains the preceding event.
  readonly bidPauseMs: number;
  readonly cardPauseMs: number;
  readonly dealCardStaggerMs: number;
  readonly dealCompleteMs: number;
  readonly trickReadMs: number;
  readonly trickResolveMs: number;
  // Individual presentation milestones.
  readonly cardTravelMs: number;
  readonly winnerRevealMs: number;
  readonly cardCollectMs: number;
  readonly scoreLandingMs: number;
  // How long the landing bounce runs, once a card has arrived.
  readonly cardLandMs: number;
  // Multiplies the game-feel effects, so they follow the speed setting. Zero
  // means effects are off: show the outcome and skip the movement.
  readonly effectScale: number;
}

const multipliers: Record<GameSpeed, number> = {
  relaxed: 1.25,
  standard: 1,
  quick: 0.72,
};

const SPEED_KEY = "game28-animation-speed";
const REDUCE_MOTION_KEY = "game28-reduce-motion";
const speeds: readonly GameSpeed[] = ["relaxed", "standard", "quick"];

const round = (value: number) => Math.round(value);

// A pure profile factory keeps pacing consistent between the Effect event
// loop and Three.js presentation. Reduced motion preserves the outcome
// reading time while removing flights and sweeps.
export function gamePacing(speed: GameSpeed = "standard", reducedMotion = false): GamePacingProfile {
  if (reducedMotion) {
    return {
      speed,
      reducedMotion: true,
      // Removing motion must not remove the time needed to read an auction.
      bidPauseMs: 3_000,
      cardPauseMs: 160,
      dealCardStaggerMs: 0,
      dealCompleteMs: 0,
      trickReadMs: 900,
      trickResolveMs: 920,
      cardTravelMs: 0,
      winnerRevealMs: 900,
      cardCollectMs: 0,
      scoreLandingMs: 0,
      cardLandMs: 0,
      effectScale: 0,
    };
  }

  const scale = multipliers[speed];
  const cardTravelMs = round(560 * scale);
  const dealCardStaggerMs = round(82 * scale);
  const winnerRevealMs = round(620 * scale);
  const cardCollectMs = round(650 * scale);
  const scoreLandingMs = round(280 * scale);

  return {
    speed,
    reducedMotion: false,
    bidPauseMs: round(3_000 * scale),
    // A card must finish its flight before a bot can visually answer.
    cardPauseMs: Math.max(cardTravelMs + 80, round(700 * scale)),
    // The server sends two four-card deals. Let each fan settle before the
    // next auction or trump event is presented.
    dealCardStaggerMs,
    dealCompleteMs: dealCardStaggerMs * 3 + round(430 * scale),
    // All four cards remain readable before the winner is announced.
    trickReadMs: round(1_200 * scale),
    // Winner reveal plus collection must finish before the next lead.
    trickResolveMs: winnerRevealMs + cardCollectMs + scoreLandingMs + 80,
    cardTravelMs,
    winnerRevealMs,
    cardCollectMs,
    scoreLandingMs,
    cardLandMs: round(170 * scale),
    effectScale: scale,
  };
}

const prefersReducedMotion = () =>
  typeof window !== "undefined" && window.matchMedia?.("(prefers-reduced-motion: reduce)").matches;

const storedSpeed = (): GameSpeed => {
  try {
    const value = localStorage.getItem(SPEED_KEY);
    return speeds.includes(value as GameSpeed) ? (value as GameSpeed) : "standard";
  } catch {
    return "standard";
  }
};

const storedReduceMotion = (): boolean => {
  try {
    return localStorage.getItem(REDUCE_MOTION_KEY) === "1";
  } catch {
    return false;
  }
};

// This controller is presentation-only: changing speed cannot alter an
// authoritative card, score, or server turn.
export class GamePacing {
  private speed: GameSpeed = storedSpeed();
  private reduceMotionChosen = storedReduceMotion();

  profile(): GamePacingProfile {
    return gamePacing(this.speed, this.reducedMotion());
  }

  // Either the player's own choice or the device's setting turns motion down.
  reducedMotion(): boolean {
    return this.reduceMotionChosen || prefersReducedMotion();
  }

  // The device's setting can't be overruled from here, only added to.
  motionForcedByDevice(): boolean {
    return prefersReducedMotion();
  }

  toggleReduceMotion(): GamePacingProfile {
    this.reduceMotionChosen = !this.reduceMotionChosen;
    try {
      localStorage.setItem(REDUCE_MOTION_KEY, this.reduceMotionChosen ? "1" : "0");
    } catch {
      // Private browsing can still use the setting until the page closes.
    }
    return this.profile();
  }

  setSpeed(speed: GameSpeed) {
    this.speed = speed;
    try {
      localStorage.setItem(SPEED_KEY, speed);
    } catch {
      // Private browsing can still use the setting until the page closes.
    }
  }

  cycleSpeed(): GamePacingProfile {
    const index = speeds.indexOf(this.speed);
    this.setSpeed(speeds[(index + 1) % speeds.length]);
    return this.profile();
  }
}
