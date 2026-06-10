// Exercises every lobby membership control through the real UI:
// kick a bot, kick a human, guest leaves, host closes the table, and
// disconnect cleanup (a closed tab frees its room).
// Usage: node scripts/test-lobby-controls.mjs [url]
import { chromium } from "playwright";
import { existsSync, mkdirSync } from "node:fs";

const URL = process.argv[2] ?? "http://localhost:5173/";
const SHOTS = "/tmp/game-shots-lobby";
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
const failures = [];
const check = (label, ok) => {
  console.log(`${ok ? "ok" : "FAIL"} — ${label}`);
  if (!ok) failures.push(label);
};

const newPlayer = async (label, name) => {
  const page = await browser.newPage({ viewport: { width: 1280, height: 800 } });
  page.on("console", (msg) => {
    if (msg.type() === "error") errors.push(`[${label}] ${msg.text()}`);
  });
  page.on("pageerror", (error) => errors.push(`[${label}] ${error}`));
  await page.goto(URL);
  await page.waitForSelector("#create-button", { state: "visible", timeout: 15000 });
  await page.fill("#player-name", name);
  return page;
};

const members = (page) => page.evaluate(() => window.__game.state().members.length);
const waitMembers = (page, expected) =>
  page.waitForFunction((n) => window.__game.state().members.length === n, expected, {
    timeout: 10000,
  });
const atStartScreen = (page) =>
  page.evaluate(() => !document.getElementById("start-screen").classList.contains("hidden"));

// --- setup: host table with 1 bot, guest joins ---
const host = await newPlayer("host", "Host");
await host.click("#create-button");
await host.waitForSelector("#lobby-panel:not(.hidden)", { timeout: 15000 });
const roomName = await host.evaluate(() => window.__game.roomName());
await host.click("#add-bot");
await waitMembers(host, 2);

const guest = await newPlayer("guest", "Guest");
await guest.click("#browse-button");
await guest.waitForSelector(".room-row button", { timeout: 15000 });
await guest.locator(".room-row", { hasText: roomName }).locator("button").click();
await guest.waitForSelector("#lobby-panel:not(.hidden)", { timeout: 15000 });
await waitMembers(host, 3);
await waitMembers(guest, 3);
check("host + bot + guest seated", true);
await host.screenshot({ path: `${SHOTS}/01-host-lobby-kick-buttons.png` });

// guest has a leave button but no kick buttons
check(
  "guest sees Leave, no kick buttons",
  (await guest.evaluate(
    () =>
      document.getElementById("leave-table").textContent === "Leave table" &&
      document.querySelectorAll(".kick-button").length === 0
  ))
);

// --- host kicks the bot ---
await host.locator("#seat-list li", { hasText: "Bot" }).locator(".kick-button").click();
await waitMembers(host, 2);
await waitMembers(guest, 2);
check("bot kicked — both lobbies show 2", true);

// --- host kicks the guest ---
await host.locator("#seat-list li", { hasText: "guest" }).locator(".kick-button").click();
await waitMembers(host, 1);
await guest.waitForFunction(
  () => !document.getElementById("start-screen").classList.contains("hidden"),
  { timeout: 10000 }
);
check("kicked guest is back at the start screen", await atStartScreen(guest));
const guestStatus = await guest.evaluate(
  () => document.getElementById("status-line").textContent
);
check(`kicked guest sees a message ("${guestStatus}")`, guestStatus.includes("removed"));
await guest.screenshot({ path: `${SHOTS}/02-guest-kicked.png` });

// --- guest rejoins, then leaves voluntarily ---
await guest.click("#browse-button");
await guest.waitForSelector(".room-row button", { timeout: 15000 });
await guest.locator(".room-row", { hasText: roomName }).locator("button").click();
await waitMembers(host, 2);
await guest.click("#leave-table");
await waitMembers(host, 1);
await guest.waitForFunction(
  () => !document.getElementById("start-screen").classList.contains("hidden"),
  { timeout: 10000 }
);
check("guest left — host sees 1, guest at start screen", await atStartScreen(guest));

// --- second guest joins; host closes the table ---
await guest.click("#browse-button");
await guest.waitForSelector(".room-row button", { timeout: 15000 });
await guest.locator(".room-row", { hasText: roomName }).locator("button").click();
await waitMembers(guest, 2);
await host.click("#leave-table"); // host's button reads "Close table"
await guest.waitForFunction(
  () => !document.getElementById("start-screen").classList.contains("hidden"),
  { timeout: 10000 }
);
const closedStatus = await guest.evaluate(
  () => document.getElementById("status-line").textContent
);
check(`table closed — guest notified ("${closedStatus}")`, closedStatus.includes("closed"));
await host.waitForFunction(
  () => !document.getElementById("start-screen").classList.contains("hidden"),
  { timeout: 10000 }
);
check("host back at start screen", await atStartScreen(host));

// closed table must be gone from the browser list
await guest.click("#refresh-rooms");
await guest.waitForTimeout(800);
const stale = await guest.evaluate(
  (name) =>
    [...document.querySelectorAll(".room-row .room-name")].some((el) => el.textContent === name),
  roomName
);
check("closed table no longer listed", !stale);

// --- disconnect cleanup: host makes a new table, then the tab dies ---
await host.click("#create-button");
await host.waitForSelector("#lobby-panel:not(.hidden)", { timeout: 15000 });
const room2 = await host.evaluate(() => window.__game.roomName());
await guest.click("#refresh-rooms");
await guest.waitForFunction(
  (name) =>
    [...document.querySelectorAll(".room-row .room-name")].some((el) => el.textContent === name),
  room2,
  { timeout: 10000 }
);
await host.close(); // simulates a closed tab / lost connection
await guest.waitForTimeout(1500);
await guest.click("#refresh-rooms");
await guest.waitForTimeout(800);
const orphaned = await guest.evaluate(
  (name) =>
    [...document.querySelectorAll(".room-row .room-name")].some((el) => el.textContent === name),
  room2,
  );
check("disconnected host's table was cleaned up", !orphaned);

if (errors.length) {
  console.log("CONSOLE ERRORS:");
  for (const error of errors) console.log("  " + error);
}
await browser.close();
if (failures.length) {
  console.log(`\n${failures.length} check(s) failed`);
  process.exit(1);
}
console.log("\nall lobby-control checks passed");
process.exit(errors.length === 0 ? 0 : 1);
