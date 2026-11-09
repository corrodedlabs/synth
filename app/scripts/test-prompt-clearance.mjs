// The bid and trump prompts against the hand, at every window size worth having. A real match is held on
// each prompt and the window is swept across a grid, with the media queries' edges on it, so a size nobody
// thought to list can't hide a prompt lying on the cards.
//
//   node scripts/test-prompt-clearance.mjs "http://localhost:5179/?port=8082"
//
//   SIZES=671x724,500x724   only these sizes
//   OUT=<dir>               where the shots of any faults go (default /tmp/game-shots-clearance)
import { chromium } from "playwright";
import { existsSync, mkdirSync } from "node:fs";
import { join } from "node:path";

const URL = process.argv[2] ?? "http://localhost:5173/";
if (!/^http:\/\/(localhost|127\.0\.0\.1)[:/]/.test(URL)) throw new Error("this plays a match: local servers only");
const OUT = process.env.OUT ?? "/tmp/game-shots-clearance";
mkdirSync(OUT, { recursive: true });

const WIDTHS = [280, 320, 360, 390, 412, 430, 480, 500, 540, 568, 600, 640, 667, 671, 700, 701, 736, 768, 820, 844, 900, 915, 1024, 1180, 1280, 1440, 1920];
const HEIGHTS = [320, 360, 375, 390, 412, 480, 500, 550, 551, 600, 650, 651, 699, 700, 724, 767, 768, 800, 844, 915, 1024, 1180];
// Skip the shapes no window takes: a sliver, or anything under 500 tall that isn't a phone on its side. The
// smallest of those is 568 wide, and the whole table can't fit a prompt in much less.
const sensible = ({ width, height }) =>
  width / height <= 3 && width / height >= 0.42 && !(height < 500 && (width < 568 || width / height < 1.5));
const SIZES = process.env.SIZES
  ? process.env.SIZES.split(",").map((text) => {
      const [width, height] = text.split("x").map(Number);
      return { width, height };
    })
  : WIDTHS.flatMap((width) => HEIGHTS.map((height) => ({ width, height }))).filter(sensible);

const browser = await chromium.launch({
  ...(existsSync("/usr/bin/chromium") ? { executablePath: "/usr/bin/chromium" } : { channel: "chrome" }),
  headless: !process.env.HEADED,
  args: process.platform === "darwin" ? [] : ["--enable-unsafe-swiftshader", "--use-angle=swiftshader"],
});
const page = await browser.newPage({ viewport: { width: 390, height: 844 } });
const errors = [];
page.on("console", (message) => {
  if (message.type() === "error") errors.push(message.text());
});
page.on("pageerror", (error) => errors.push(String(error)));

// Waits out the window's new layout and the fan's tween, then measures. A prompt may not touch a card, the
// partner, a side seat or the HUD, and must stay on the screen.
const measure = () =>
  page.evaluate(async () => {
    const box = (rect) => ({ left: rect.left, top: rect.top, right: rect.right, bottom: rect.bottom });
    const textBox = (el) => {
      const range = document.createRange();
      range.selectNodeContents(el);
      return box(range.getBoundingClientRect());
    };
    const visible = (el) => el && !el.classList.contains("hidden") && getComputedStyle(el).display !== "none";
    const promptRects = () => {
      const bid = document.getElementById("bid-panel");
      if (visible(bid)) return [{ name: "bid-panel", rect: box(bid.getBoundingClientRect()) }];
      return [...document.querySelectorAll("#trump-panel .panel-title, #trump-panel .suit-card")].map((el, index) => ({
        name: index === 0 ? "trump-title" : `suit-${index}`,
        // the title's box is as wide as the cards, but only its words can land on anything
        rect: index === 0 ? textBox(el) : box(el.getBoundingClientRect()),
      }));
    };
    const snapshot = () =>
      JSON.stringify([promptRects(), window.__game.handCardBounds()].flat(2).map((piece) => piece.rect ?? piece));
    let last = "";
    let same = 0;
    const deadline = performance.now() + 2500;
    while (same < 4 && performance.now() < deadline) {
      await new Promise(requestAnimationFrame);
      const now = snapshot();
      same = now === last ? same + 1 : 0;
      last = now;
    }

    const others = [];
    const add = (name, rect) => others.push({ name, rect });
    const addEl = (name, selector) => {
      const el = document.querySelector(selector);
      if (visible(el)) add(name, box(el.getBoundingClientRect()));
    };
    window.__game.handCardBounds().forEach((card, index) => add(`hand${index}`, box(card)));
    addEl("score", "#score-pips");
    addEl("menu", "#menu-button");
    addEl("help", "#help-button");
    for (const seat of [1, 2, 3]) {
      addEl(`seat${seat}-ring`, `#seat-label-${seat} .seat-marker`);
      const name = document.querySelector(`#seat-label-${seat} .player-copy`);
      if (visible(name)) add(`seat${seat}-name`, textBox(name));
    }
    addEl("partner-tag", "#seat-label-2 .partner-tag");

    const faults = [];
    for (const { name, rect } of promptRects()) {
      if (rect.top < 0 || rect.left < 0 || rect.right > innerWidth || rect.bottom > innerHeight) {
        faults.push({ kind: "off-screen", piece: name, by: 0 });
      }
      for (const other of others) {
        const deep = Math.min(rect.bottom, other.rect.bottom) - Math.max(rect.top, other.rect.top);
        const wide = Math.min(rect.right, other.rect.right) - Math.max(rect.left, other.rect.left);
        if (deep > 1 && wide > 1) faults.push({ kind: other.name.startsWith("hand") ? "on-hand" : "on-table", piece: `${name} x ${other.name}`, by: Math.round(deep) });
      }
    }
    return faults;
  });

const report = [];
async function sweep(prompt) {
  let shots = 0;
  for (const size of SIZES) {
    await page.setViewportSize(size);
    const faults = await measure();
    if (faults.length === 0) continue;
    const name = `${size.width}x${size.height}`;
    report.push({ prompt, size: name, faults });
    if (shots++ < 12) await page.screenshot({ path: join(OUT, `${prompt}-${name}.png`) });
  }
  console.log(`${prompt}: ${SIZES.length} sizes measured`);
}

await page.goto(URL);
await page.waitForSelector("#create-button", { state: "visible", timeout: 20000 });
await page.fill("#player-name", "Clearance");
await page.evaluate(() => {
  document.activeElement?.blur();
  document.getElementById("play-bots").click();
});

// The tallest panel a player can be asked with: a bid to beat, 14 chips and a Pass. Whoever speaks first here
// may get fewer, so the rest are added to measure the worst of it.
await page.waitForFunction(() => window.__game.state().pendingRequest?.kind === "bid", null, { timeout: 90000 });
await page.evaluate(() => {
  const chips = document.getElementById("bid-chips");
  const pass = document.getElementById("bid-pass");
  const number = () => chips.querySelectorAll(".bid-chip:not(.bid-pass)").length;
  while (number() < 14) chips.insertBefore(document.getElementById("bid-raise").cloneNode(true), pass);
  while (number() > 14) chips.querySelector(".bid-extra").remove();
  pass.classList.remove("hidden");
});
await sweep("bid");

// Bidding the most there is takes the auction, and with it the trump prompt.
await page.setViewportSize({ width: 390, height: 844 });
await page.evaluate(() => window.__game.bid(28));
await page.waitForFunction(() => window.__game.state().pendingRequest?.kind === "trump", null, { timeout: 60000 });
await sweep("trump");
await page.evaluate(() => window.__game.leaveMatch()).catch(() => {});
await browser.close();

const counts = {};
for (const { prompt, faults } of report) {
  for (const fault of faults) counts[`${prompt} ${fault.kind}`] = (counts[`${prompt} ${fault.kind}`] ?? 0) + 1;
}
for (const { prompt, size, faults } of report) {
  const worst = faults.toSorted((a, b) => b.by - a.by)[0];
  console.log(`FAULT ${prompt} ${size}: ${worst.piece} by ${worst.by}px${faults.length > 1 ? `, and ${faults.length - 1} more` : ""}`);
}
console.log(`${report.length} faulty size${report.length === 1 ? "" : "s"} of ${SIZES.length * 2} measured ${JSON.stringify(counts)}`);
if (errors.length) console.log(`console errors: ${JSON.stringify(errors.slice(0, 5))}`);
process.exit(report.length || errors.length ? 1 : 0);
