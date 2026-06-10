// Synthesized game audio — Web Audio API only, no sample files.
// The palette stays inside the zen aesthetic: short breaths of filtered
// noise for cards (paper on felt), quiet sine chimes for attention.
// Every method is fire-and-forget and silently no-ops when audio is
// unavailable: muted, no AudioContext, or still suspended by the
// browser's autoplay policy (it unlocks on the first click or keypress).

const MUTE_KEY = "game28-muted";

interface SwishOptions {
  readonly delay?: number;
  readonly duration: number;
  readonly from: number; // bandpass sweep start, Hz
  readonly to: number; // bandpass sweep end, Hz
  readonly peak: number; // gain
}

interface ToneOptions {
  readonly delay?: number;
  readonly freq: number;
  readonly duration: number;
  readonly peak: number;
  readonly type?: OscillatorType;
}

export class SoundService {
  // attempts per sound, regardless of mute — read by tests and the debug bridge
  readonly played: Record<string, number> = {};

  private context: AudioContext | null = null;
  private master: GainNode | null = null;
  private noiseBuffer: AudioBuffer | null = null;
  private muted: boolean;
  private button: HTMLElement | null = null;

  constructor() {
    this.muted = this.readMuted();
    if (typeof window !== "undefined") {
      // autoplay policy: contexts start suspended until a user gesture
      const unlock = () => this.resume();
      window.addEventListener("pointerdown", unlock, { once: true });
      window.addEventListener("keydown", unlock, { once: true });
    }
  }

  // --- mute toggle ---

  isMuted(): boolean {
    return this.muted;
  }

  bindToggle(button: HTMLElement | null) {
    this.button = button;
    if (!button) return;
    button.addEventListener("click", () => this.toggle());
    this.renderButton();
  }

  toggle() {
    this.muted = !this.muted;
    try {
      localStorage.setItem(MUTE_KEY, this.muted ? "1" : "0");
    } catch {
      // storage can be unavailable (private mode); the toggle still works
    }
    if (!this.muted) this.resume();
    this.renderButton();
  }

  // --- game events ---

  // a quick riffle as cards land in the hand
  deal(cardCount: number) {
    this.count("deal");
    const ctx = this.ready();
    if (!ctx) return;
    for (let i = 0; i < Math.min(cardCount, 4); i++) {
      this.swish(ctx, { delay: i * 0.07, duration: 0.05, from: 2600, to: 1400, peak: 0.05 });
    }
  }

  // a card placed on the table: a short swish with a soft thump under it
  cardPlay() {
    this.count("cardPlay");
    const ctx = this.ready();
    if (!ctx) return;
    this.swish(ctx, { duration: 0.045, from: 2000, to: 900, peak: 0.07 });
    this.tone(ctx, { freq: 150, duration: 0.06, peak: 0.05 });
  }

  // the completed trick swept toward its winner
  trickSweep() {
    this.count("trickSweep");
    const ctx = this.ready();
    if (!ctx) return;
    this.swish(ctx, { duration: 0.3, from: 2400, to: 350, peak: 0.06 });
  }

  // gentle two-note chime: the server is waiting on us
  yourTurn() {
    this.count("yourTurn");
    const ctx = this.ready();
    if (!ctx) return;
    this.tone(ctx, { freq: 659.25, duration: 0.1, peak: 0.035 });
    this.tone(ctx, { delay: 0.09, freq: 880, duration: 0.16, peak: 0.03 });
  }

  // a tiny tick for each bid or pass in the auction
  bidTick() {
    this.count("bidTick");
    const ctx = this.ready();
    if (!ctx) return;
    this.tone(ctx, { freq: 980, duration: 0.04, peak: 0.03, type: "triangle" });
  }

  // the face-down trump turned over: a small rising strum
  trumpReveal() {
    this.count("trumpReveal");
    const ctx = this.ready();
    if (!ctx) return;
    [523.25, 659.25, 783.99].forEach((freq, i) =>
      this.tone(ctx, { delay: i * 0.07, freq, duration: 0.22, peak: 0.03 })
    );
  }

  // end of the hand: an ascending arpeggio for a win, two low tones for a loss
  result(won: boolean) {
    this.count(won ? "win" : "lose");
    const ctx = this.ready();
    if (!ctx) return;
    if (won) {
      [523.25, 659.25, 783.99, 1046.5].forEach((freq, i) =>
        this.tone(ctx, { delay: i * 0.12, freq, duration: 0.3, peak: 0.035 })
      );
    } else {
      this.tone(ctx, { freq: 196, duration: 0.4, peak: 0.04 });
      this.tone(ctx, { delay: 0.18, freq: 185, duration: 0.5, peak: 0.035 });
    }
  }

  // --- plumbing ---

  private count(name: string) {
    this.played[name] = (this.played[name] ?? 0) + 1;
  }

  private readMuted(): boolean {
    try {
      return localStorage.getItem(MUTE_KEY) === "1";
    } catch {
      return false;
    }
  }

  private renderButton() {
    if (this.button) this.button.textContent = this.muted ? "sound off" : "sound on";
  }

  private ready(): AudioContext | null {
    if (this.muted) return null;
    const ctx = this.ensure();
    if (!ctx) return null;
    if (ctx.state === "suspended") {
      // try to wake it; this sound is lost but the next one plays
      void ctx.resume().catch(() => undefined);
      return null;
    }
    return ctx.state === "running" ? ctx : null;
  }

  private resume() {
    const ctx = this.ensure();
    if (ctx && ctx.state === "suspended") void ctx.resume().catch(() => undefined);
  }

  private ensure(): AudioContext | null {
    if (this.context) return this.context;
    try {
      const Ctor =
        window.AudioContext ??
        (window as unknown as { webkitAudioContext?: typeof AudioContext }).webkitAudioContext;
      if (!Ctor) return null;
      this.context = new Ctor();
      this.master = this.context.createGain();
      this.master.gain.value = 0.6;
      this.master.connect(this.context.destination);
    } catch {
      this.context = null;
      return null;
    }
    return this.context;
  }

  private noise(ctx: AudioContext): AudioBuffer {
    if (!this.noiseBuffer) {
      const buffer = ctx.createBuffer(1, ctx.sampleRate, ctx.sampleRate);
      const data = buffer.getChannelData(0);
      for (let i = 0; i < data.length; i++) data[i] = Math.random() * 2 - 1;
      this.noiseBuffer = buffer;
    }
    return this.noiseBuffer;
  }

  // a short breath of band-passed noise with a falling sweep
  private swish(ctx: AudioContext, options: SwishOptions) {
    if (!this.master) return;
    const start = ctx.currentTime + (options.delay ?? 0);
    const source = ctx.createBufferSource();
    source.buffer = this.noise(ctx);
    const filter = ctx.createBiquadFilter();
    filter.type = "bandpass";
    filter.Q.value = 0.9;
    filter.frequency.setValueAtTime(options.from, start);
    filter.frequency.exponentialRampToValueAtTime(Math.max(options.to, 40), start + options.duration);
    const gain = ctx.createGain();
    gain.gain.setValueAtTime(0, start);
    gain.gain.linearRampToValueAtTime(options.peak, start + 0.008);
    gain.gain.exponentialRampToValueAtTime(0.0001, start + options.duration);
    source.connect(filter);
    filter.connect(gain);
    gain.connect(this.master);
    source.start(start);
    source.stop(start + options.duration + 0.05);
  }

  private tone(ctx: AudioContext, options: ToneOptions) {
    if (!this.master) return;
    const start = ctx.currentTime + (options.delay ?? 0);
    const osc = ctx.createOscillator();
    osc.type = options.type ?? "sine";
    osc.frequency.value = options.freq;
    const gain = ctx.createGain();
    gain.gain.setValueAtTime(0, start);
    gain.gain.linearRampToValueAtTime(options.peak, start + 0.01);
    gain.gain.exponentialRampToValueAtTime(0.0001, start + options.duration);
    osc.connect(gain);
    gain.connect(this.master);
    osc.start(start);
    osc.stop(start + options.duration + 0.05);
  }
}
