// Match play across hands: host + guest at one table with two bots.
// Hand 1 runs to the between-hands panel (the host gets the deal button,
// the guest a waiting note, match scores mirror); a guest's nextHand() is
// inert; the host's click deals hand 2 (table resets, match HUD and seat
// names persist, hand number bumps); after hand 2 the match score has
// accumulated; the guest then leaves with the corner button (two-step
// confirm) — the leaver gets a working start screen, the host is asked
// about the abandoned seat, replaces it with a bot, and plays hand 3 on;
// the last human leaving closes the match.
// Usage: node scripts/test-match.mjs [url]
// Screenshots land in /tmp/game-shots-match.
import { chromium } from "playwright";
import { existsSync, mkdirSync } from "node:fs";

const URL = process.argv[2] ?? "http://localhost:5173/";
const SHOTS = "/tmp/game-shots-match";
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
      isHost: model.isHost,
      points: model.points,
      handNumber: model.handNumber,
      matchUs: model.matchUs,
      matchThem: model.matchThem,
      matchTarget: model.matchTarget,
      matchWinner: model.matchWinner,
      seatNames: model.seatNames,
      legal: window.__game.legalCards(),
    };
  });

const visible = (page, id) =>
  page.evaluate((elId) => {
    const el = document.getElementById(elId);
    return el !== null && !el.classList.contains("hidden");
  }, id);

// drive a page (pass every bid, play the first legal card) to a phase
const playUntil = async (label, page, targetPhase) => {
  const deadline = Date.now() + 240_000;
  let lastLog = "";
  while (Date.now() < deadline) {
    const snapshot = await state(page);
    const line = `${label}: phase=${snapshot.phase} pending=${snapshot.pending ?? "-"} hand#${snapshot.handNumber}`;
    if (line !== lastLog) {
      console.log(line);
      lastLog = line;
    }
    if (snapshot.phase === targetPhase) return snapshot;
    if (snapshot.pending === "bid") {
      await page.evaluate(() => window.__game.pass());
    } else if (snapshot.pending === "trump") {
      await page.evaluate(() => window.__game.trump("hearts"));
    } else if (snapshot.pending === "play" && snapshot.legal[0]) {
      await page.evaluate((id) => window.__game.play(id), snapshot.legal[0]);
    }
    await page.waitForTimeout(350);
  }
  throw new Error(`${label} timed out waiting for ${targetPhase}`);
};

const host = await newPlayer("host");
const guest = await newPlayer("guest");

check("leave button hidden on the start screen", !(await visible(host, "leave-match")));

// --- lobby: host creates, guest joins, two bots fill the table ---
await host.fill("#player-name", "Host");
await host.click("#create-button");
await host.click('#create-panel .table-choice[data-target="6"]');
await host.waitForSelector("#lobby-panel:not(.hidden)", { timeout: 15000 });
check("leave button hidden in the lobby", !(await visible(host, "leave-match")));
const roomName = await host.evaluate(() => window.__game.roomName());
console.log(`host created "${roomName}"`);

await guest.fill("#player-name", "Guest");
await guest.click("#browse-button");
await guest.waitForSelector(".room-row button", { timeout: 15000 });
await guest.locator(".room-row", { hasText: roomName }).locator("button").click();
await guest.waitForSelector("#lobby-panel:not(.hidden)", { timeout: 15000 });

await host.click("#add-bot");
await host.click("#add-bot");
for (const [label, page] of [["host", host], ["guest", guest]]) {
  await page.waitForFunction(() => window.__game.state().members.length === 4, undefined, { timeout: 10000 });
  console.log(`${label} lobby shows 4 players`);
}
await host.click("#start-game");

// --- hand 1 → the between-hands panel ---
const [host1, guest1] = await Promise.all([
  playUntil("host", host, "hand-finished"),
  playUntil("guest", guest, "hand-finished"),
]);
await host.screenshot({ path: `${SHOTS}/01-hand1-host-panel.png` });
await guest.screenshot({ path: `${SHOTS}/02-hand1-guest-panel.png` });

check("hand number is 1 on both pages", host1.handNumber === 1 && guest1.handNumber === 1);
check("no match winner yet", host1.matchWinner === null && guest1.matchWinner === null);
check("leave button is offered during a match", await visible(host, "leave-match"));
const total = (points) => points.reduce((a, b) => a + b, 0);
check("both views agree on the hand's card points", total(host1.points) === total(guest1.points));
check(
  "match scores mirror between host and guest",
  host1.matchUs === guest1.matchThem && host1.matchThem === guest1.matchUs
);
check(
  "one team scored ±1/±2/±4 game points",
  [1, -2, 2, -4].includes(host1.matchUs) !== [1, -2, 2, -4].includes(host1.matchThem)
);
check("host sees the result panel", await visible(host, "result-panel"));
check("host sees the deal button", await visible(host, "next-hand"));
check("host does not see the waiting note", !(await visible(host, "result-wait")));
check("host does not see play-again between hands", !(await visible(host, "play-again")));
check("guest sees the result panel", await visible(guest, "result-panel"));
check("guest does not see the deal button", !(await visible(guest, "next-hand")));
check("guest sees the waiting note", await visible(guest, "result-wait"));
const hostHud = await host.evaluate(() => document.getElementById("match-value").textContent);
check(
  `host match HUD shows the score (${hostHud})`,
  hostHud === `${host1.matchUs} — ${host1.matchThem}`
);

// --- a guest cannot advance the match ---
await guest.evaluate(() => window.__game.nextHand());
await guest.waitForTimeout(2500);
check(
  "guest nextHand() is inert",
  (await state(guest)).phase === "hand-finished" && (await state(host)).phase === "hand-finished"
);

// --- the host deals hand 2 with a real click ---
await host.click("#next-hand");
for (const [label, page] of [["host", host], ["guest", guest]]) {
  await page.waitForFunction(() => window.__game.state().handNumber === 2, undefined, { timeout: 20000 });
  const snap = await state(page);
  check(`${label} starts hand 2 in the auction`, snap.phase === "bidding");
  check(`${label} per-hand results were reset`, snap.points === null);
  check(`${label} match score survived the reset`, snap.matchUs === (label === "host" ? host1 : guest1).matchUs);
  check(`${label} seat names survived the reset`, Array.isArray(snap.seatNames) && snap.seatNames.length === 4);
  check(`${label} result panel is gone`, !(await visible(page, "result-panel")));
}
await host.screenshot({ path: `${SHOTS}/03-hand2-host.png` });

// --- hand 2 → the match score accumulates ---
const [host2, guest2] = await Promise.all([
  playUntil("host", host, "hand-finished"),
  playUntil("guest", guest, "hand-finished"),
]);
await host.screenshot({ path: `${SHOTS}/04-hand2-host-panel.png` });

check("hand number is 2 on both pages", host2.handNumber === 2 && guest2.handNumber === 2);
check(
  "match scores still mirror",
  host2.matchUs === guest2.matchThem && host2.matchThem === guest2.matchUs
);
check(
  "hand 2 moved exactly one team's score",
  (host2.matchUs !== host1.matchUs) !== (host2.matchThem !== host1.matchThem)
);

// --- the guest walks out with the corner button (two-step confirm) ---
check("guest sees the leave button between hands", await visible(guest, "leave-match"));
await guest.click("#leave-match");
const armedText = await guest.evaluate(() => document.getElementById("leave-match").textContent);
check(`first click only arms the button ("${armedText}")`, armedText.includes("abandon"));
check("still in the match after one click", (await state(guest)).phase === "hand-finished");
await guest.click("#leave-match");
await guest.waitForFunction(() => window.__game.state().phase === "idle", undefined, { timeout: 5000 });
check("leaver lands back on the start screen", await visible(guest, "start-screen"));
check("leaving hides the between-hands panel", !(await visible(guest, "result-panel")));
check("leaving hides the leave button", !(await visible(guest, "leave-match")));
const guestAfter = await state(guest);
check("the leaver's match state was wiped", guestAfter.matchUs === 0 && guestAfter.matchThem === 0);
await guest.screenshot({ path: `${SHOTS}/05-guest-left.png` });

// --- the survivors choose: a bot takes the abandoned seat ---
await host.waitForSelector("#abandon-panel:not(.hidden)", { timeout: 15000 });
const abandonText = await host.evaluate(() => document.getElementById("abandon-text").textContent);
check(
  `the host is told who abandoned ("${abandonText}")`,
  abandonText.toLowerCase().includes("guest")
);
await host.screenshot({ path: `${SHOTS}/06-abandon-panel.png` });
await host.click("#replace-bot");
await host.waitForFunction(
  () => window.__game.state().members.filter((m) => /^bot-/.test(m)).length === 3,
  undefined,
  { timeout: 15000 }
);
check("a bot took the guest's seat", true);
check("the abandon panel is gone", !(await visible(host, "abandon-panel")));
const seatNames = await host.evaluate(() => window.__game.state().seatNames);
check(
  `the seat label now names the bot (${JSON.stringify(seatNames)})`,
  seatNames.some((name) => /^Bot \d+$/.test(name))
);

// --- the match carries on: the host deals and plays hand 3 vs three bots ---
await host.waitForSelector("#next-hand:not(.hidden)", { timeout: 10000 });
await host.click("#next-hand");
await host.waitForFunction(() => window.__game.state().handNumber === 3, undefined, {
  timeout: 20000,
});
const host3 = await playUntil("host", host, "hand-finished");
check("hand 3 played out with the replacement bot", host3.handNumber === 3);
await host.screenshot({ path: `${SHOTS}/07-hand3-with-bot.png` });

// --- the last human leaving closes the match outright ---
await host.evaluate(() => window.__game.leaveMatch());
await host.waitForFunction(() => window.__game.state().phase === "idle", undefined, {
  timeout: 10000,
});
check("the last human lands back on the start screen", await visible(host, "start-screen"));

// --- the leaver's page is still alive: a fresh table without a reload ---
await guest.click("#create-button");
await guest.click('#create-panel .table-choice[data-target="6"]');
await guest.waitForSelector("#lobby-panel:not(.hidden)", { timeout: 15000 });
const fresh = await state(guest);
check("leaver can host a new table on the same page", fresh.phase === "lobby" && fresh.isHost);
await guest.screenshot({ path: `${SHOTS}/07-leaver-new-table.png` });

if (errors.length) {
  console.log("CONSOLE ERRORS:");
  for (const error of errors) console.log("  " + error);
}
await browser.close();
if (failures.length || errors.length) {
  console.log(`\n${failures.length} check(s) failed`);
  process.exit(1);
}
console.log("\nall match checks passed");
process.exit(0);
