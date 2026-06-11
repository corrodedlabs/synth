// Mobile pass: runs the full game on a phone-sized touch viewport (390×844),
// verifying the hand fan, panels, and overlays stay inside the screen and
// that cards play via real taps. Usage: node scripts/test-mobile.mjs [url]
// Screenshots land in /tmp/game-shots-mobile.
import { chromium } from "playwright";
import { existsSync, mkdirSync } from "node:fs";

const URL = process.argv[2] ?? "http://localhost:5173/";
const SHOTS = "/tmp/game-shots-mobile";
mkdirSync(SHOTS, { recursive: true });

const VIEW = { width: 390, height: 844 };

// HEADED=1 opens a visible browser window.
const browser = await chromium.launch({
  ...(existsSync("/usr/bin/chromium")
    ? { executablePath: "/usr/bin/chromium" }
    : { channel: "chrome" }),
  headless: !process.env.HEADED,
  args: ["--enable-unsafe-swiftshader", "--use-angle=swiftshader"],
});
const context = await browser.newContext({
  viewport: VIEW,
  hasTouch: true,
  isMobile: true,
  deviceScaleFactor: 2,
  userAgent:
    "Mozilla/5.0 (iPhone; CPU iPhone OS 17_0 like Mac OS X) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/17.0 Mobile/15E148 Safari/604.1",
});
const page = await context.newPage();

const errors = [];
const failures = [];
page.on("console", (msg) => {
  if (msg.type() === "error") errors.push(msg.text());
});
page.on("pageerror", (error) => errors.push(String(error)));
const check = (label, ok) => {
  console.log(`${ok ? "ok" : "FAIL"} — ${label}`);
  if (!ok) failures.push(label);
};
const shot = (name) => page.screenshot({ path: `${SHOTS}/${name}.png` });

const state = () =>
  page.evaluate(() => {
    const model = window.__game.state();
    return {
      phase: model.phase,
      pending: model.pendingRequest?.kind ?? null,
      hand: model.hand.map((card) => card.id),
      legal: window.__game.legalCards(),
      points: model.points,
    };
  });

// every hand card's projected position must be inside the viewport
const checkHandFits = async (label) => {
  const { hand } = await state();
  const positions = await page.evaluate(
    (ids) => ids.map((id) => window.__game.cardScreenPos(id)),
    hand
  );
  const margin = 12;
  const inside = positions.every(
    (p) => p && p.x > margin && p.x < VIEW.width - margin && p.y > 0 && p.y < VIEW.height
  );
  check(`${label}: all ${hand.length} hand cards on screen`, inside);
  if (!inside) console.log("  positions:", JSON.stringify(positions));
};

// a visible element must sit fully inside the viewport
const checkFits = async (selector, label) => {
  const box = await page.locator(selector).boundingBox();
  const ok =
    box !== null &&
    box.x >= 0 &&
    box.y >= 0 &&
    box.x + box.width <= VIEW.width + 1 &&
    box.y + box.height <= VIEW.height + 1;
  check(`${label} fits viewport`, ok);
  if (!ok && box) console.log("  box:", JSON.stringify(box));
};

console.log(`navigating to ${URL} at ${VIEW.width}×${VIEW.height}`);
await page.goto(URL);
await page.waitForSelector("#create-button", { state: "visible", timeout: 15000 });
await shot("01-start");
await checkFits("#start-actions", "start actions");

// help overlay on a phone
await page.tap("#help-button");
await checkFits("#help-panel", "help panel");
await shot("02-help");
await page.tap("#help-close");

// lobby
await page.fill("#player-name", "Phone");
await page.tap("#create-button");
await page.tap('#create-panel .table-choice[data-target="6"]');
await page.waitForSelector("#lobby-panel:not(.hidden)", { timeout: 15000 });
await checkFits("#lobby-panel", "lobby panel");
for (let bots = 1; bots <= 3; bots++) {
  await page.tap("#add-bot");
  await page.waitForFunction(
    (expected) => window.__game.state().members.length === expected,
    bots + 1,
    { timeout: 10000 }
  );
}
await shot("03-lobby");
await page.tap("#start-game");

// play the hand: bid panel, then tap cards to play (two-step on touch)
let tappedPlays = 0;
let shotHand = false;
let shotTrick = false;
let checkedDimming = false;
let checkedArming = false;
const deadline = Date.now() + 180_000;
while (Date.now() < deadline) {
  const snapshot = await state();
  if (snapshot.phase === "hand-finished") break;

  if (snapshot.hand.length === 8 && !shotHand) {
    await page.waitForTimeout(700);
    await checkHandFits("full hand");
    await shot("04-hand");
    shotHand = true;
  }

  if (snapshot.pending === "bid") {
    await checkFits("#bid-panel", "bid panel");
    await page.tap("#bid-pass");
    await page.waitForTimeout(300);
    continue;
  }
  if (snapshot.pending === "trump") {
    await checkFits("#trump-panel", "trump panel");
    await page.tap('#trump-panel [data-suit="spades"]');
    await page.waitForTimeout(300);
    continue;
  }
  if (snapshot.pending === "play" && snapshot.legal[0]) {
    const exposable = await page.evaluate(() => {
      const button = document.getElementById("expose-button");
      return button && !button.classList.contains("hidden");
    });
    if (exposable) {
      await page.tap("#expose-button");
      await page.waitForTimeout(300);
      continue;
    }
    // unplayable cards must be dimmed while the server waits on us
    if (!checkedDimming) {
      const states = await page.evaluate(() => window.__game.cardStates());
      const wrong = states.filter(
        (s) => s.dimmed === snapshot.legal.includes(s.id)
      );
      check("illegal cards (and only those) are dimmed", wrong.length === 0);
      if (wrong.length) console.log("  mismatches:", JSON.stringify(wrong));
      checkedDimming = true;
    }
    // touch plays are two-step: the first tap arms the card…
    const cardId = snapshot.legal[0];
    const pos = await page.evaluate((id) => window.__game.cardScreenPos(id), cardId);
    if (pos) {
      await page.touchscreen.tap(pos.x, pos.y);
      await page.waitForTimeout(400);
      const armed = await state();
      if (!checkedArming) {
        check("first tap arms the card instead of playing it", armed.hand.includes(cardId));
        checkedArming = true;
      }
      // …and a second tap on the raised card commits it
      const raised = await page.evaluate((id) => window.__game.cardScreenPos(id), cardId);
      if (raised) await page.touchscreen.tap(raised.x, raised.y);
      await page.waitForTimeout(500);
      const after = await state();
      const played = !after.hand.includes(cardId);
      if (played) tappedPlays++;
      else await page.evaluate((id) => window.__game.play(id), cardId); // fallback
      if (!shotTrick && after.hand.length <= 5) {
        await shot("05-mid-game");
        shotTrick = true;
      }
    }
    continue;
  }
  await page.waitForTimeout(300);
}

const final = await state();
check("hand finished on mobile", final.phase === "hand-finished");
check(`cards played by real taps (${tappedPlays}/8)`, tappedPlays >= 6);
await checkFits("#result-panel", "result panel");
await shot("06-result");

if (errors.length) {
  console.log("CONSOLE ERRORS:");
  for (const error of errors) console.log("  " + error);
}
await browser.close();
if (failures.length || errors.length) {
  console.log(`\n${failures.length} check(s) failed`);
  process.exit(1);
}
console.log("\nall mobile checks passed");
process.exit(0);
