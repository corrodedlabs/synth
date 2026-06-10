// Mid-match reconnection: one human + 3 bots play into the first hand, then
// the page reloads — localStorage identity auto-rejoins the seat, the
// snapshot restores the table (hand, trick, scores, seat labels, pending
// request), and the hand plays out to the between-hands panel.
// Usage: node scripts/test-reconnect.mjs [url]
// Screenshots land in /tmp/game-shots-reconnect.
import { chromium } from "playwright";
import { existsSync, mkdirSync } from "node:fs";

const URL = process.argv[2] ?? "http://localhost:5173/";
const SHOTS = "/tmp/game-shots-reconnect";
mkdirSync(SHOTS, { recursive: true });

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
      tricksTotal: model.tricks + model.theirTricks,
      handNumber: model.handNumber,
      matchUs: model.matchUs,
      matchThem: model.matchThem,
      seatNames: model.seatNames,
      roomName: model.roomName,
      isHost: model.isHost,
      legal: window.__game.legalCards(),
    };
  });

// pass every bid, choose hearts, play the first legal card
const act = async (snapshot) => {
  if (snapshot.pending === "bid") {
    await page.evaluate(() => window.__game.pass());
  } else if (snapshot.pending === "trump") {
    await page.evaluate(() => window.__game.trump("hearts"));
  } else if (snapshot.pending === "play" && snapshot.legal[0]) {
    await page.evaluate((id) => window.__game.play(id), snapshot.legal[0]);
  }
};

console.log(`navigating to ${URL}`);
await page.goto(URL);
await page.waitForSelector("#create-button", { state: "visible", timeout: 15000 });

// --- lobby: solo human + three bots ---
await page.fill("#player-name", "Returner");
await page.click("#create-button");
await page.waitForSelector("#lobby-panel:not(.hidden)", { timeout: 15000 });
for (let bots = 1; bots <= 3; bots++) {
  await page.click("#add-bot");
  await page.waitForFunction((n) => window.__game.state().members.length === n, bots + 1, {
    timeout: 10000,
  });
}
await page.click("#start-game");

// --- play into the middle of hand 1 (at least two tricks down) ---
let before = null;
{
  const deadline = Date.now() + 120_000;
  while (Date.now() < deadline) {
    const snapshot = await state();
    if (snapshot.phase === "playing" && snapshot.tricksTotal >= 2 && snapshot.pending === "play") {
      before = snapshot;
      break;
    }
    await act(snapshot);
    await page.waitForTimeout(300);
  }
}
check("reached mid-hand with tricks on the board", before !== null);
if (!before) {
  await browser.close();
  process.exit(1);
}
console.log(`before reload: ${before.hand.length} cards, ${before.tricksTotal} tricks done`);
await shot("01-before-reload");

// --- the refresh: identity and seat must survive ---
await page.reload();
await page.waitForFunction(
  () => {
    const model = window.__game?.state?.();
    return model && model.roomName !== null && model.phase !== "idle" && model.phase !== "connecting";
  },
  undefined,
  { timeout: 20000 }
);
const after = await state();
await shot("02-after-reload");

check("rejoined the same table", after.roomName === before.roomName);
check("still hand 1 of the match", after.handNumber === 1);
check(
  `hand restored (${after.hand.length} cards)`,
  after.hand.length === before.hand.length && after.hand.every((id) => before.hand.includes(id))
);
check("tricks taken survived", after.tricksTotal >= before.tricksTotal);
check("seat names restored", Array.isArray(after.seatNames) && after.seatNames.length === 4);
check("still the host", after.isHost === true);
check(
  "leave button is back after the restore",
  await page.evaluate(() => !document.getElementById("leave-match").classList.contains("hidden"))
);

// --- the hand must still play out to the between-hands panel ---
{
  const deadline = Date.now() + 120_000;
  let finished = false;
  while (Date.now() < deadline) {
    const snapshot = await state();
    if (snapshot.phase === "hand-finished") {
      finished = true;
      break;
    }
    await act(snapshot);
    await page.waitForTimeout(300);
  }
  check("hand played out to the between-hands panel after the rejoin", finished);
}
await shot("03-hand-finished");
check(
  "host sees the deal button",
  await page.evaluate(() => !document.getElementById("next-hand").classList.contains("hidden"))
);

// --- leaving clears the stored match so the next load starts fresh ---
await page.evaluate(() => window.__game.leaveMatch());
await page.waitForFunction(() => window.__game.state().phase === "idle", undefined, {
  timeout: 10000,
});
const remembered = await page.evaluate(() => localStorage.getItem("game28-active-match"));
check("leaving forgets the active match", remembered === null);
await page.reload();
await page.waitForSelector("#create-button", { state: "visible", timeout: 15000 });
check(
  "next load lands on the start screen with the name prefilled",
  (await page.evaluate(() => document.getElementById("player-name").value)) === "Returner"
);
await shot("04-fresh-start");

if (errors.length) {
  console.log("CONSOLE ERRORS:");
  for (const error of errors) console.log("  " + error);
}
await browser.close();
if (failures.length || errors.length) {
  console.log(`\n${failures.length} check(s) failed`);
  process.exit(1);
}
console.log("\nall reconnect checks passed");
process.exit(0);
