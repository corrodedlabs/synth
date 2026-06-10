// Drives a full game of 28 against the server bots through the browser UI.
// Exercises real pointer gestures (drag + click-to-play) for the first two
// plays, then the debug bridge. Usage: node scripts/play-game.mjs [url]
// Screenshots land in /tmp/game-shots.
import { chromium } from "playwright";
import { existsSync, mkdirSync } from "node:fs";

const URL = process.argv[2] ?? "http://localhost:5173/";
const SHOTS = "/tmp/game-shots";
mkdirSync(SHOTS, { recursive: true });

// Prefer the sandbox chromium when present (Linux CI), else system Chrome.
// HEADED=1 opens a visible browser window.
const browser = await chromium.launch({
  ...(existsSync("/usr/bin/chromium")
    ? { executablePath: "/usr/bin/chromium" }
    : { channel: "chrome" }),
  headless: !process.env.HEADED,
  args: ["--enable-unsafe-swiftshader", "--use-angle=swiftshader"],
});
const page = await browser.newPage({ viewport: { width: 1280, height: 800 } });

const errors = [];
page.on("console", (msg) => {
  if (msg.type() === "error") errors.push(msg.text());
});
page.on("pageerror", (error) => errors.push(String(error)));

const shot = (name) => page.screenshot({ path: `${SHOTS}/${name}.png` });

const getState = () =>
  page.evaluate(() => {
    const state = window.__game.state();
    return {
      phase: state.phase,
      pending: state.pendingRequest,
      hand: state.hand.map((card) => card.id),
      played: state.playedCards.length,
      tricks: state.tricks + state.theirTricks,
      statusText: document.getElementById("status-line")?.textContent ?? "",
      currentBid: state.currentBid,
      finalBid: state.finalBid,
      bidWinner: state.bidWinner,
      trumpSuit: state.trumpSuit,
      trumpExposed: state.trumpExposed,
      points: state.points,
      legal: window.__game.legalCards(),
    };
  });

console.log(`navigating to ${URL}`);
await page.goto(URL);
await page.waitForSelector("#create-button", { state: "visible", timeout: 15000 });
await shot("01-start");

// --- lobby flow: create a table, seat 3 bots, then explicitly start ---
await page.fill("#player-name", "Tester");
await page.click("#create-button");
await page.waitForSelector("#lobby-panel:not(.hidden)", { timeout: 15000 });
await shot("01b-lobby-empty");

const seatCount = () =>
  page.evaluate(() => window.__game.state().members.length);
const startDisabled = () =>
  page.evaluate(() => document.getElementById("start-game").disabled);

if (!(await startDisabled())) throw new Error("start button enabled with 1 player");
// the explicit-start gate: starting must do nothing before the table is full
await page.evaluate(() => window.__game.startGame());
await page.waitForTimeout(500);
if ((await page.evaluate(() => window.__game.state().phase)) !== "lobby") {
  throw new Error("game left the lobby before 4 players were seated");
}

for (let bots = 1; bots <= 3; bots++) {
  await page.click("#add-bot");
  await page.waitForFunction(
    (expected) => window.__game.state().members.length === expected,
    bots + 1,
    { timeout: 10000 }
  );
  console.log(`bots seated: ${bots}`);
}
await shot("01c-lobby-full");

if (await startDisabled()) throw new Error("start button still disabled with 4 players");
console.log(`seats full (${await seatCount()}), starting game`);
await page.click("#start-game");

let didBid = false;
let shotFullHand = false;
let shotTrick = false;
let shotTrickEnd = false;
let lastLog = "";
let playsMade = 0;
const gestureResults = [];
const deadline = Date.now() + 180_000;

const playByGesture = async (cardId, kind) => {
  const pos = await page.evaluate((id) => window.__game.cardScreenPos(id), cardId);
  if (!pos) return false;
  if (kind === "drag") {
    await page.mouse.move(pos.x, pos.y);
    await page.mouse.down();
    await page.mouse.move(pos.x, pos.y - 40, { steps: 4 });
    await page.mouse.move(640, 430, { steps: 12 });
    await page.mouse.up();
  } else {
    await page.mouse.click(pos.x, pos.y);
  }
  await page.waitForTimeout(400);
  const after = await getState();
  return !after.hand.includes(cardId);
};

while (Date.now() < deadline) {
  const state = await getState();

  const logLine = `phase=${state.phase} pending=${state.pending?.kind ?? "-"} hand=${state.hand.length} tricks=${state.tricks} trump=${state.trumpSuit ?? "?"}${state.trumpExposed ? "!" : ""}`;
  if (logLine !== lastLog) {
    console.log(logLine);
    lastLog = logLine;
  }

  if (state.hand.length === 8 && !shotFullHand) {
    await page.waitForTimeout(700); // let the deal animation settle
    await shot("04-hand-8cards");
    shotFullHand = true;
  }
  if (state.played >= 3 && !shotTrick) {
    await shot("05-trick");
    shotTrick = true;
  }
  if (state.statusText.includes("trick") && !shotTrickEnd) {
    await shot("05b-trick-end");
    console.log(`trick-end status: "${state.statusText}"`);
    shotTrickEnd = true;
  }

  // one full hand is this script's scope; the match gate (hand-finished)
  // is where it ends — test-match.mjs covers multi-hand play
  if (state.phase === "hand-finished") {
    await shot("07-result");
    console.log("HAND FINISHED", JSON.stringify({ points: state.points, finalBid: state.finalBid, bidWinner: state.bidWinner }));
    break;
  }

  if (state.pending?.kind === "bid") {
    if (!didBid) await shot("02-bid-panel");
    if (!didBid && state.pending.currentBid < 20) {
      console.log(`raising to ${state.pending.currentBid + 1}`);
      await page.click("#bid-raise");
      didBid = true;
    } else {
      console.log("passing");
      await page.click("#bid-pass");
    }
    await page.waitForTimeout(300);
    continue;
  }

  if (state.pending?.kind === "trump") {
    await shot("03-trump-panel");
    console.log("choosing trump: hearts");
    await page.click('#trump-panel [data-suit="hearts"]');
    await page.waitForTimeout(300);
    continue;
  }

  if (state.pending?.kind === "play") {
    const exposable = await page.evaluate(() => {
      const button = document.getElementById("expose-button");
      return button && !button.classList.contains("hidden");
    });
    if (exposable && !state.trumpExposed) {
      console.log("exposing trump");
      await page.click("#expose-button");
      await page.waitForTimeout(300);
      continue;
    }
    const cardId = state.legal[0];
    if (cardId) {
      playsMade++;
      if (playsMade === 1) {
        const ok = await playByGesture(cardId, "drag");
        gestureResults.push(`drag: ${ok ? "ok" : "FAILED"}`);
        console.log(`played ${cardId} via drag → ${ok ? "ok" : "FAILED"}`);
        if (!ok) await page.evaluate((id) => window.__game.play(id), cardId);
      } else if (playsMade === 2) {
        const ok = await playByGesture(cardId, "click");
        gestureResults.push(`click: ${ok ? "ok" : "FAILED"}`);
        console.log(`played ${cardId} via click → ${ok ? "ok" : "FAILED"}`);
        if (!ok) await page.evaluate((id) => window.__game.play(id), cardId);
      } else {
        console.log(`playing ${cardId} (${playsMade}/8)`);
        const ok = await page.evaluate((id) => window.__game.play(id), cardId);
        if (!ok) console.log(`!! play of ${cardId} was rejected`);
      }
      if (playsMade === 5) await shot("06-mid-game");
    }
    await page.waitForTimeout(400);
    continue;
  }

  // Hover across the hand while waiting (mimics a real player mousing
  // around) — this is exactly when the second deal lands, so it regression-
  // tests hover vs. re-arrange conflicts.
  if (state.phase === "bidding" || state.phase === "choosing-trump") {
    const x = 400 + ((Date.now() / 40) % 500);
    await page.mouse.move(x, 700, { steps: 2 });
  }

  await page.waitForTimeout(300);
}

const finalState = await getState();
console.log("gesture results:", gestureResults.join(", ") || "none");
if (finalState.phase !== "hand-finished") {
  console.log("TIMED OUT — final state:", JSON.stringify(finalState));
  await shot("99-timeout");
}
if (errors.length) {
  console.log("CONSOLE ERRORS:");
  for (const error of errors) console.log("  " + error);
}

await browser.close();
process.exit(finalState.phase === "hand-finished" && errors.length === 0 ? 0 : 1);
