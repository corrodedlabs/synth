// Two humans at one table: host creates it, guest joins through the room
// browser, host fills the last seats with bots and starts. Both browsers then
// play the hand out via the debug bridge. Usage: node scripts/play-multiplayer.mjs [url]
// Screenshots land in /tmp/game-shots-mp.
import { chromium } from "playwright";
import { existsSync, mkdirSync } from "node:fs";

const URL = process.argv[2] ?? "http://localhost:5173/";
const SHOTS = "/tmp/game-shots-mp";
mkdirSync(SHOTS, { recursive: true });

// HEADED=1 opens visible browser windows.
const browser = await chromium.launch({
  ...(existsSync("/usr/bin/chromium")
    ? { executablePath: "/usr/bin/chromium" }
    : { channel: "chrome" }),
  headless: !process.env.HEADED,
  args: ["--enable-unsafe-swiftshader", "--use-angle=swiftshader"],
});

const errors = [];
const newPlayer = async (label) => {
  const page = await browser.newPage({ viewport: { width: 1280, height: 800 } });
  page.on("console", (msg) => {
    if (msg.type() === "error") errors.push(`[${label}] ${msg.text()}`);
  });
  page.on("pageerror", (error) => errors.push(`[${label}] ${error}`));
  await page.goto(URL);
  await page.waitForSelector("#create-button", { state: "visible", timeout: 15000 });
  return page;
};

const state = (page) =>
  page.evaluate(() => {
    const model = window.__game.state();
    return {
      phase: model.phase,
      pending: model.pendingRequest?.kind ?? null,
      members: model.members,
      isHost: model.isHost,
      points: model.points,
      legal: window.__game.legalCards(),
    };
  });

const host = await newPlayer("host");
const guest = await newPlayer("guest");

// --- host creates a table ---
await host.fill("#player-name", "Host");
await host.click("#create-button");
await host.waitForSelector("#lobby-panel:not(.hidden)", { timeout: 15000 });
const roomName = await host.evaluate(() => window.__game.roomName());
console.log(`host created "${roomName}"`);

// --- guest finds it in the room browser and joins ---
await guest.fill("#player-name", "Guest");
await guest.click("#browse-button");
await guest.waitForSelector(".room-row button", { timeout: 15000 });
await guest.screenshot({ path: `${SHOTS}/01-room-browser.png` });
const rows = await guest.$$eval(".room-row .room-name", (els) => els.map((el) => el.textContent));
console.log(`guest sees tables: ${rows.join(" | ")}`);
const row = guest.locator(".room-row", { hasText: roomName });
await row.locator("button").click();
await guest.waitForSelector("#lobby-panel:not(.hidden)", { timeout: 15000 });

// both lobbies should now show 2 members
for (const [label, page] of [["host", host], ["guest", guest]]) {
  await page.waitForFunction(() => window.__game.state().members.length === 2, undefined, { timeout: 10000 });
  console.log(`${label} lobby shows 2 players`);
}
await guest.screenshot({ path: `${SHOTS}/02-guest-lobby.png` });

// guest must not have host controls
const guestControls = await guest.evaluate(() => ({
  addBot: document.getElementById("add-bot").classList.contains("hidden"),
  start: document.getElementById("start-game").classList.contains("hidden"),
}));
if (!guestControls.addBot || !guestControls.start) {
  throw new Error("guest sees host-only lobby controls");
}

// --- host seats two bots and starts ---
await host.click("#add-bot");
await host.click("#add-bot");
for (const [label, page] of [["host", host], ["guest", guest]]) {
  await page.waitForFunction(() => window.__game.state().members.length === 4, undefined, { timeout: 10000 });
  console.log(`${label} lobby shows 4 players`);
}
await host.screenshot({ path: `${SHOTS}/03-host-lobby-full.png` });
await host.click("#start-game");

// --- both browsers play the hand out ---
const playLoop = async (label, page) => {
  const deadline = Date.now() + 240_000;
  let lastLog = "";
  while (Date.now() < deadline) {
    const snapshot = await state(page);
    const line = `${label}: phase=${snapshot.phase} pending=${snapshot.pending ?? "-"}`;
    if (line !== lastLog) {
      console.log(line);
      lastLog = line;
    }
    // the first hand ends at the match gate; test-match.mjs goes further
    if (snapshot.phase === "hand-finished") return snapshot;

    if (snapshot.pending === "bid") {
      await page.evaluate(() => window.__game.pass());
    } else if (snapshot.pending === "trump") {
      await page.evaluate(() => window.__game.trump("spades"));
    } else if (snapshot.pending === "play" && snapshot.legal[0]) {
      await page.evaluate((id) => window.__game.play(id), snapshot.legal[0]);
    }
    await page.waitForTimeout(350);
  }
  throw new Error(`${label} timed out in phase ${(await state(page)).phase}`);
};

const [hostEnd, guestEnd] = await Promise.all([playLoop("host", host), playLoop("guest", guest)]);
await host.screenshot({ path: `${SHOTS}/04-host-result.png` });
await guest.screenshot({ path: `${SHOTS}/05-guest-result.png` });

console.log("host result:", JSON.stringify(hostEnd.points));
console.log("guest result:", JSON.stringify(guestEnd.points));

// the two views describe the same hand: total points must match (rotated seats)
const total = (points) => points.reduce((a, b) => a + b, 0);
if (total(hostEnd.points) !== total(guestEnd.points)) {
  throw new Error("host and guest disagree on total points");
}

if (errors.length) {
  console.log("CONSOLE ERRORS:");
  for (const error of errors) console.log("  " + error);
}
await browser.close();
process.exit(errors.length === 0 ? 0 : 1);
