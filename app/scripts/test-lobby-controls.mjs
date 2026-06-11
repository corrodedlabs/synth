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

// --- setup: host picks a QUICK table (first to 2), bot + guest join ---
const host = await newPlayer("host", "Host");
check(
  "classic is the default table type",
  await host.evaluate(
    () => document.querySelector("#table-type .selected")?.dataset.target === "6"
  )
);
await host.click('#table-type [data-target="2"]');
await host.click("#create-button");
await host.waitForSelector("#lobby-panel:not(.hidden)", { timeout: 15000 });
const roomName = await host.evaluate(() => window.__game.roomName());
const subtitle = await host.evaluate(() => document.getElementById("lobby-subtitle").textContent);
check(`host lobby names the table type ("${subtitle}")`, subtitle.includes("to 2"));
await host.click("#add-bot");
await waitMembers(host, 2);

const guest = await newPlayer("guest", "Guest");
await guest.click("#browse-button");
await guest.waitForSelector(".room-row button", { timeout: 15000 });
const rowText = await guest
  .locator(".room-row", { hasText: roomName })
  .locator(".room-count")
  .textContent();
check(`the open-tables row shows the type ("${rowText}")`, rowText.includes("to 2"));
await guest.locator(".room-row", { hasText: roomName }).locator("button").click();
await guest.waitForSelector("#lobby-panel:not(.hidden)", { timeout: 15000 });
await waitMembers(host, 3);
await waitMembers(guest, 3);
check(
  "the joining guest's lobby names the table type too",
  (await guest.evaluate(() => document.getElementById("lobby-subtitle").textContent)).includes(
    "to 2"
  )
);
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

// --- standings: the leaderboard panel answers from the start screen ---
await host.click("#standings-button");
await host.waitForSelector("#leaderboard-panel:not(.hidden)", { timeout: 15000 });
await host.waitForFunction(
  () => {
    const list = document.getElementById("leaderboard-list");
    return list.children.length >= 1 && !list.textContent.includes("fetching");
  },
  undefined,
  { timeout: 10000 }
);
check("standings panel opens with an answer", true);
await host.click("#leaderboard-close");

// --- invite links: a shared URL drops a friend straight into the table ---
await host.click("#create-button");
await host.waitForSelector("#lobby-panel:not(.hidden)", { timeout: 15000 });
const inviteUrl = await host.evaluate(() => window.__game.inviteLink());
console.log(`invite link: ${inviteUrl}`);
check("lobby offers a copy-invite button", await host.evaluate(
  () => !document.getElementById("copy-invite").classList.contains("hidden")
));
const invited = await browser.newPage({ viewport: { width: 1280, height: 800 } });
invited.on("pageerror", (error) => errors.push(`[invited] ${error}`));
await invited.goto(inviteUrl);
await invited.waitForSelector("#join-link:not(.hidden)", { timeout: 15000 });
const joinLabel = await invited.evaluate(() => document.getElementById("join-link").textContent);
const inviteRoom = await host.evaluate(() => window.__game.state().roomName);
check(`invite start screen names the table ("${joinLabel}")`, joinLabel.includes(inviteRoom));
await invited.fill("#player-name", "Linked");
await invited.click("#join-link");
await invited.waitForSelector("#lobby-panel:not(.hidden)", { timeout: 15000 });
await invited.waitForFunction(() => window.__game.state().members.length === 2, undefined, {
  timeout: 10000,
});
check("invited friend lands in the host's lobby", true);
await host.waitForFunction(() => window.__game.state().members.length === 2, undefined, {
  timeout: 10000,
});
await invited.close();
await host.click("#leave-table"); // close this table before the next section
await host.waitForSelector("#start-screen:not(.hidden)", { timeout: 10000 });

// --- a stale invite (table gone) must hand back a working start screen ---
const late = await browser.newPage({ viewport: { width: 1280, height: 800 } });
late.on("pageerror", (error) => errors.push(`[late] ${error}`));
await late.goto(inviteUrl);
await late.waitForSelector("#join-link:not(.hidden)", { timeout: 15000 });
await late.fill("#player-name", "Late");
await late.click("#join-link");
await late.waitForSelector("#start-screen:not(.hidden)", { timeout: 15000 });
check(
  "stale invite falls back to the normal start screen",
  await late.evaluate(() => !document.getElementById("start-screen").classList.contains("hidden"))
);
check(
  "the dead table's join button is gone",
  await late.evaluate(() => document.getElementById("join-link").classList.contains("hidden"))
);
const lateStatus = await late.evaluate(() => document.getElementById("status-line").textContent);
check(`the failure is explained ("${lateStatus}")`, lateStatus.includes("no longer open"));
await late.close();

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
