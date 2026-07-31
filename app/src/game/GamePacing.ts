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
}

const multipliers: Record<GameSpeed, number> = {
  relaxed: 1.25,
  standard: 1,
  quick: 0.72,
};

const SPEED_KEY = "game28-animation-speed";
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
      bidPauseMs: 4_000,
      cardPauseMs: 160,
      dealCardStaggerMs: 0,
      dealCompleteMs: 0,
      trickReadMs: 900,
      trickResolveMs: 920,
      cardTravelMs: 0,
      winnerRevealMs: 900,
      cardCollectMs: 0,
      scoreLandingMs: 0,
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
    bidPauseMs: round(4_000 * scale),
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

// This controller is presentation-only: changing speed cannot alter an
// authoritative card, score, or server turn.
export class GamePacing {
  private speed: GameSpeed = storedSpeed();

  profile(): GamePacingProfile {
    return gamePacing(this.speed, prefersReducedMotion());
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
