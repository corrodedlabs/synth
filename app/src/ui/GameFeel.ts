import type { Suit } from "../game/GameModel";
import type { GamePacingProfile } from "../game/GamePacing";
import type { SoundService } from "../utils/Sound";
import { suitGlyph, suitIsRed } from "./ScoreHud";

// Game feel: everything here is decoration laid over a game that has already
// moved on. Nothing the game does ever waits for an effect:
//  - the fx layer takes no touches, so controls stay live underneath;
//  - an effect that holds back a bit of the HUD does it through a `commit`, and
//    that commit always runs, on landing, on a time limit or on cancel;
//  - cancelAll() (a reconnect, a new deal, leaving) stops everything at once.
// With reduce motion on, commits run at once and only the information shows.

const INK_BLACK = "rgba(59, 47, 42, 0.8)";
const INK_RED = "rgba(163, 60, 46, 0.82)";

export class GameFeel {
  private layer = document.getElementById("fx-layer") as HTMLElement;
  private uiLayer = document.getElementById("ui-layer") as HTMLElement;
  private appRoot = document.getElementById("app") as HTMLElement;
  private timers = new Set<number>();
  private animations = new Set<Animation>();
  private pending = new Set<() => void>();
  private revealSkip: (() => void) | null = null;

  constructor(
    private sound: SoundService,
    private pacing: () => GamePacingProfile
  ) {
    // Any tap ends a trump reveal early, so it can never stand in the way.
    document.addEventListener("pointerdown", () => this.revealSkip?.());
  }

  get enabled(): boolean {
    return this.pacing().effectScale > 0;
  }

  private ms(base: number): number {
    return Math.round(base * this.pacing().effectScale);
  }

  // Stop everything and bring the HUD up to date. Safe to call at any time.
  cancelAll() {
    this.timers.forEach((timer) => window.clearTimeout(timer));
    this.timers.clear();
    const animations = [...this.animations];
    this.animations.clear();
    animations.forEach((animation) => animation.cancel());
    this.layer.replaceChildren();
    this.revealSkip = null;
    this.uiLayer.classList.remove("revealing");
    const pending = [...this.pending];
    this.pending.clear();
    pending.forEach((commit) => commit());
  }

  // --- small effects ---

  // A bid's seal coming down, with a small shake and ring as it hits.
  stampSeal(seal: HTMLElement) {
    if (!this.enabled) {
      this.sound.sealThump();
      return;
    }
    const duration = this.ms(300);
    this.thump(seal, 2.8, duration);
    this.after(duration * 0.58, () => {
      this.sound.sealThump();
      this.shake(1.6);
      this.ringAtElement(seal);
    });
  }

  // The smaller knock when the winning bid settles into the HUD.
  stampHud(seal: HTMLElement) {
    if (this.enabled) this.thump(seal, 2, this.ms(240));
  }

  shake(pixels: number) {
    if (!this.enabled) return;
    const keyframes = [
      { transform: "none" },
      { transform: `translate(${pixels}px, ${-pixels}px)` },
      { transform: `translate(${-pixels}px, ${pixels * 0.7}px)` },
      { transform: "none" },
    ];
    for (const element of [this.appRoot, this.uiLayer]) {
      this.play(element, keyframes, { duration: 150 });
    }
  }

  ringAtElement(element: HTMLElement) {
    const box = element.getBoundingClientRect();
    this.ringAt(box.left + box.width / 2, box.top + box.height / 2);
  }

  ringAt(x: number, y: number) {
    if (!this.enabled) return;
    const ring = document.createElement("div");
    ring.className = "fx-ring";
    ring.style.left = `${x}px`;
    ring.style.top = `${y}px`;
    this.layer.append(ring);
    this.play(
      ring,
      [
        { transform: "scale(0.5)", opacity: 1 },
        { transform: "scale(2.2)", opacity: 0 },
      ],
      { duration: this.ms(420), easing: "ease-out" },
      () => ring.remove()
    );
  }

  // A trick winner's face jumps.
  avatarReact(avatar: HTMLElement | null) {
    if (!avatar || !this.enabled) return;
    this.play(
      avatar,
      [
        { transform: "scale(1)" },
        { transform: "scale(1.5) rotate(-8deg)" },
        { transform: "scale(1)" },
      ],
      { duration: this.ms(560) }
    );
  }

  // A point landing on its pip.
  popPip(pip: HTMLElement) {
    this.sound.settle();
    if (!this.enabled) return;
    this.play(pip, [{ transform: "scale(2.4)" }, { transform: "scale(1)" }], {
      duration: this.ms(320),
      easing: "ease-out",
    });
  }

  // Ink flung from a point, drawn on a canvas that fades itself away.
  inkBurst(
    x: number,
    y: number,
    color: string,
    { particles = 70, spread = 260 }: { particles?: number; spread?: number } = {}
  ) {
    if (!this.enabled) return;
    const width = window.innerWidth;
    const height = window.innerHeight;
    const ratio = Math.min(window.devicePixelRatio || 1, 2);
    const canvas = document.createElement("canvas");
    canvas.width = Math.round(width * ratio);
    canvas.height = Math.round(height * ratio);
    const context = canvas.getContext("2d");
    if (!context) return;
    this.layer.append(canvas);
    context.scale(ratio, ratio);
    context.fillStyle = color;

    const drops = Array.from({ length: particles }, () => ({
      angle: Math.random() * Math.PI * 2,
      reach: (0.25 + Math.random() ** 1.7) * spread,
      size: 1 + Math.random() ** 3 * 12,
    }));
    const blob = Array.from({ length: 14 }, () => 22 + Math.random() * 16);
    const start = performance.now();
    const frame = (now: number) => {
      if (!canvas.isConnected) return;
      const progress = Math.min(1, (now - start) / 460);
      const eased = 1 - (1 - progress) ** 3;
      context.clearRect(0, 0, width, height);
      context.beginPath();
      const points = blob.map((radius, index) => {
        const angle = (index / blob.length) * Math.PI * 2;
        return [
          x + Math.cos(angle) * radius * eased * 1.3,
          y + Math.sin(angle) * radius * eased * 1.3,
        ];
      });
      points.forEach((point, index) => {
        const next = points[(index + 1) % points.length];
        if (index === 0) {
          const last = points[points.length - 1];
          context.moveTo((last[0] + point[0]) / 2, (last[1] + point[1]) / 2);
        }
        context.quadraticCurveTo(point[0], point[1], (point[0] + next[0]) / 2, (point[1] + next[1]) / 2);
      });
      context.closePath();
      context.fill();
      for (const drop of drops) {
        context.beginPath();
        context.arc(
          x + Math.cos(drop.angle) * drop.reach * eased,
          y + Math.sin(drop.angle) * drop.reach * eased,
          drop.size * (0.5 + 0.5 * eased),
          0,
          Math.PI * 2
        );
        context.fill();
      }
      if (progress < 1) requestAnimationFrame(frame);
    };
    requestAnimationFrame(frame);
    this.play(
      canvas,
      [
        { opacity: 1, offset: 0 },
        { opacity: 1, offset: 0.55 },
        { opacity: 0, offset: 1 },
      ],
      { duration: 1500 },
      () => canvas.remove()
    );
  }

  // --- big moments ---

  // The chosen suit card turns face down and flies to the HUD; the others drop
  // away. The real panel is already gone: these are stand-ins.
  pickTrump(
    cards: ReadonlyArray<{ suit: Suit; box: DOMRect }>,
    chosen: Suit,
    target: HTMLElement,
    commit: () => void
  ) {
    const settle = this.commitOnce(commit, 1500);
    if (!this.enabled) {
      settle();
      return;
    }
    for (const { suit, box } of cards) {
      const card = this.suitCard(suit, box.left, box.top, box.width, box.height);
      if (suit !== chosen) {
        this.play(
          card,
          [
            { opacity: 1, transform: "none" },
            { opacity: 0, transform: "translateY(30px)" },
          ],
          { duration: this.ms(200), fill: "forwards" },
          () => card.remove()
        );
        continue;
      }
      const goal = target.getBoundingClientRect();
      const dx = goal.left + goal.width / 2 - (box.left + box.width / 2);
      const dy = goal.top + goal.height / 2 - (box.top + box.height / 2);
      const scale = goal.width / box.width;
      this.play(
        card,
        [
          { transform: "perspective(700px)" },
          {
            transform: "perspective(700px) translateY(-22px) rotateY(180deg) scale(1.08)",
            offset: 0.45,
          },
          {
            transform: `perspective(700px) translate(${dx}px, ${dy}px) rotateY(180deg) scale(${scale})`,
          },
        ],
        { duration: this.ms(680), easing: "ease-in-out", fill: "forwards" },
        () => {
          card.remove();
          this.sound.settle();
          this.shake(1.2);
          settle();
        }
      );
    }
  }

  // The table stops: trump turns over large in the middle, ink bursts behind
  // it, then it flies to the HUD. A tap anywhere jumps straight to the end.
  revealTrump(suit: Suit, caption: string, target: HTMLElement, commit: () => void) {
    const reveal = this.ms(900);
    const hold = this.ms(650);
    const flight = this.ms(380);
    const settle = this.commitOnce(commit, reveal + hold + flight + 1000);

    const viewportHeight = window.innerHeight;
    const height = Math.min(222, viewportHeight * 0.5);
    const width = height * (150 / 222);
    const left = window.innerWidth / 2 - width / 2;
    const top = viewportHeight / 2 - height / 2;
    const card = this.suitCard(suit, left, top, width, height);
    const label = document.createElement("div");
    label.className = "fx-caption";
    label.textContent = caption;
    label.style.top = `${Math.min(top + height + 10, viewportHeight - 40)}px`;
    this.layer.append(label);
    this.uiLayer.classList.add("revealing");

    const finish = () => {
      if (this.revealSkip !== finish) return;
      this.revealSkip = null;
      this.uiLayer.classList.remove("revealing");
      card.remove();
      label.remove();
      settle();
    };
    this.revealSkip = finish;

    const rise = -viewportHeight * 0.3;
    this.play(
      card,
      [
        { transform: `perspective(900px) translateY(${rise}px) scale(0.3) rotateY(180deg)` },
        { transform: "perspective(900px) scale(1.1) rotateY(180deg)", offset: 0.45 },
        { transform: "perspective(900px) scale(1.1) rotateY(180deg)", offset: 0.6 },
        { transform: "perspective(900px) scale(1.12) rotateY(360deg)", offset: 0.95 },
        { transform: "perspective(900px) scale(1) rotateY(360deg)" },
      ],
      { duration: reveal, easing: "ease-in-out" }
    );
    this.after(reveal * 0.93, () => {
      this.inkBurst(window.innerWidth / 2, viewportHeight / 2, INK_BLACK);
      this.sound.bigThump();
      this.shake(4);
    });
    this.after(reveal + hold, () => {
      if (this.revealSkip !== finish) return;
      label.remove();
      this.uiLayer.classList.remove("revealing");
      const goal = target.getBoundingClientRect();
      const dx = goal.left + goal.width / 2 - window.innerWidth / 2;
      const dy = goal.top + goal.height / 2 - viewportHeight / 2;
      this.play(
        card,
        [
          { transform: "none", opacity: 1 },
          { transform: `translate(${dx}px, ${dy}px) scale(${goal.width / width})`, opacity: 0.6 },
        ],
        { duration: flight, easing: "ease-in", fill: "forwards" },
        () => {
          this.sound.settle();
          finish();
        }
      );
    });
  }

  // The verdict: seal, then the words, then the points fly up to their pip.
  // Every part of the panel is in place from the start, the timings only hold
  // it back visually, so the deal button under it can be pressed at any moment.
  handResult(
    parts: {
      readonly seal: HTMLElement;
      readonly detail: HTMLElement;
      readonly delta: HTMLElement | null;
    },
    target: () => HTMLElement,
    commit: () => void
  ) {
    const slam = this.ms(360);
    const wordsAt = slam + this.ms(340);
    const pointsAt = wordsAt + this.ms(380);
    const flyAt = pointsAt + this.ms(600);
    const flight = this.ms(520);
    const settle = this.commitOnce(commit, flyAt + flight + 1000);

    this.thump(parts.seal, 3.2, slam);
    this.after(slam * 0.57, () => {
      this.sound.bigThump();
      this.shake(4.5);
      const box = parts.seal.getBoundingClientRect();
      this.inkBurst(box.left + box.width / 2, box.top + box.height / 2, INK_RED, {
        particles: 44,
        spread: 170,
      });
    });
    this.fadeIn(parts.detail, wordsAt);
    const { delta } = parts;
    if (!delta) {
      this.after(flyAt, settle);
      return;
    }
    this.play(
      delta,
      [
        { transform: "scale(0.4)", opacity: 0 },
        { transform: "scale(1.2)", opacity: 1 },
        { transform: "none", opacity: 1 },
      ],
      { duration: this.ms(280), delay: pointsAt, fill: "backwards" }
    );
    this.after(flyAt, () => {
      const box = delta.getBoundingClientRect();
      const flying = document.createElement("div");
      flying.className = "fx-flying-points";
      flying.textContent = delta.textContent;
      flying.style.left = `${box.left}px`;
      flying.style.top = `${box.top}px`;
      flying.style.font = getComputedStyle(delta).font;
      this.layer.append(flying);
      const goal = target().getBoundingClientRect();
      const dx = goal.left + goal.width / 2 - (box.left + box.width / 2);
      const dy = goal.top + goal.height / 2 - (box.top + box.height / 2);
      this.play(
        flying,
        [
          { transform: "none", opacity: 1 },
          { transform: `translate(${dx}px, ${dy}px) scale(0.35)`, opacity: 0.5 },
        ],
        { duration: flight, easing: "ease-in", fill: "forwards" },
        () => {
          flying.remove();
          settle();
        }
      );
    });
  }

  // The match decided: a win takes the screen in ink, a loss just lands.
  matchOver(
    won: boolean,
    parts: { readonly team: HTMLElement; readonly title: HTMLElement; readonly seal: HTMLElement }
  ) {
    if (!this.enabled) return;
    if (won) {
      const width = window.innerWidth;
      const height = window.innerHeight;
      const spread = Math.max(width, height) * 0.45;
      this.inkBurst(width / 2, height * 0.4, INK_RED, { particles: 110, spread });
      this.sound.bigThump();
      this.shake(5);
      this.after(180, () => {
        this.inkBurst(width * 0.3, height * 0.57, INK_BLACK, { particles: 50, spread: spread * 0.6 });
        this.sound.bigThump();
      });
      this.after(380, () => {
        this.inkBurst(width * 0.7, height * 0.25, INK_BLACK, { particles: 40, spread: spread * 0.5 });
      });
    }
    Array.from(parts.team.children).forEach((ring, index) => {
      this.play(
        ring,
        [
          { transform: "scale(0) rotate(-20deg)" },
          { transform: "scale(1.12)" },
          { transform: "none" },
        ],
        { duration: this.ms(460), delay: this.ms(index * 110), easing: "ease-out", fill: "backwards" }
      );
    });
    this.fadeIn(parts.title, this.ms(260));
    this.thump(parts.seal, 3, this.ms(340));
  }

  // --- plumbing ---

  private thump(seal: HTMLElement, from: number, duration: number) {
    const lean = getComputedStyle(seal).getPropertyValue("--r").trim() || "-7deg";
    this.play(
      seal,
      [
        { transform: `rotate(${lean}) scale(${from})`, opacity: 0 },
        { transform: `rotate(${lean}) scale(0.9)`, opacity: 1, offset: 0.58, easing: "ease-out" },
        { transform: `rotate(${lean}) scale(1.05)`, offset: 0.8 },
        { transform: `rotate(${lean}) scale(1)` },
      ],
      { duration, easing: "cubic-bezier(0.5, 0, 0.8, 0.5)" }
    );
  }

  private fadeIn(element: HTMLElement, delay: number) {
    this.play(
      element,
      [
        { opacity: 0, transform: "translateY(8px)" },
        { opacity: 1, transform: "none" },
      ],
      { duration: this.ms(260), delay, fill: "backwards" }
    );
  }

  private suitCard(suit: Suit, left: number, top: number, width: number, height: number): HTMLElement {
    const card = document.createElement("div");
    card.className = "fx-suit-card";
    card.style.left = `${left}px`;
    card.style.top = `${top}px`;
    card.style.width = `${width}px`;
    card.style.height = `${height}px`;
    const front = document.createElement("div");
    front.className = `front ${suitIsRed(suit) ? "red-suit" : "black-suit"}`;
    front.textContent = suitGlyph(suit);
    front.style.fontSize = `${height * 0.44}px`;
    const back = document.createElement("div");
    back.className = "back";
    card.append(front, back);
    this.layer.append(card);
    return card;
  }

  private commitOnce(commit: () => void, limitMs: number): () => void {
    let done = false;
    const run = () => {
      if (done) return;
      done = true;
      this.pending.delete(run);
      commit();
    };
    if (!this.enabled) {
      run();
      return run;
    }
    this.pending.add(run);
    this.after(limitMs, run); // a hidden tab may never finish its animations
    return run;
  }

  private after(delay: number, action: () => void) {
    const timer = window.setTimeout(() => {
      this.timers.delete(timer);
      action();
    }, delay);
    this.timers.add(timer);
  }

  private play(
    element: Element,
    keyframes: Keyframe[],
    options: KeyframeAnimationOptions,
    done?: () => void
  ) {
    const animation = element.animate(keyframes, options);
    this.animations.add(animation);
    animation.onfinish = () => {
      this.animations.delete(animation);
      animation.cancel(); // lets go of any fill, so the element is itself again
      done?.();
    };
    animation.oncancel = () => {
      this.animations.delete(animation);
    };
    return animation;
  }
}
