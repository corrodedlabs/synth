// LOCAL ONLY — not part of CI: kills and restarts the dev game server in
// the middle of a match to prove persistence end to end. The page must
// notice the dead socket, auto-retry its rejoin, land in the restored
// match (same room, same hand number, same match score; the interrupted
// hand re-deals), and play it out.
// Usage: node scripts/test-restart.mjs [url]   (servers on 8082/5179 up)
import { chromium } from "playwright";
import { execSync, spawn } from "node:child_process";
import { fileURLToPath } from "node:url";
import { dirname, resolve } from "node:path";

const URL = process.argv[2] ?? "http://localhost:5179/?port=8082";
const REPO = resolve(dirname(fileURLToPath(import.meta.url)), "../..");

const browser = await chromium.launch({
  channel: "chrome",
  headless: !process.env.HEADED,
  args: ["--enable-unsafe-swiftshader", "--use-angle=swiftshader"],
});
const page = await browser.newPage({ viewport: { width: 1280, height: 800 } });
const errors = [];
const failures = [];
page.on("pageerror", (error) => errors.push(String(error)));
const check = (label, ok) => {
  console.log(`${ok ? "ok" : "FAIL"} — ${label}`);
  if (!ok) failures.push(label);
};

const state = () =>
  page.evaluate(() => {
    const model = window.__game.state();
    return {
      phase: model.phase,
      pending: model.pendingRequest?.kind ?? null,
      legal: window.__game.legalCards(),
      roomName: model.roomName,
      handNumber: model.handNumber,
      matchUs: model.matchUs,
      matchThem: model.matchThem,
      tricksTotal: model.tricks + model.theirTricks,
    };
  });

const act = async (snapshot) => {
  if (snapshot.pending === "bid") await page.evaluate(() => window.__game.pass());
  else if (snapshot.pending === "trump") await page.evaluate(() => window.__game.trump("hearts"));
  else if (snapshot.pending === "play" && snapshot.legal[0]) {
    await page.evaluate((id) => window.__game.play(id), snapshot.legal[0]);
  }
};

const playUntil = async (target) => {
  const deadline = Date.now() + 180_000;
  while (Date.now() < deadline) {
    const snapshot = await state();
    if (target(snapshot)) return snapshot;
    await act(snapshot);
    await page.waitForTimeout(300);
  }
  throw new Error("timed out");
};

console.log(`navigating to ${URL}`);
await page.goto(URL);
await page.waitForSelector("#create-button", { state: "visible", timeout: 15000 });
await page.fill("#player-name", "Restarter");
await page.click("#create-button");
await page.waitForSelector("#lobby-panel:not(.hidden)", { timeout: 15000 });
for (let bots = 1; bots <= 3; bots++) {
  await page.click("#add-bot");
  await page.waitForFunction((n) => window.__game.state().members.length === n, bots + 1, {
    timeout: 10000,
  });
}
await page.click("#start-game");

// hand 1 ends, the host deals hand 2 so the match score is non-trivial
await playUntil((s) => s.phase === "hand-finished");
await page.click("#next-hand");
await page.waitForFunction(() => window.__game.state().handNumber === 2, undefined, {
  timeout: 20000,
});
const before = await playUntil((s) => s.phase === "playing" && s.tricksTotal >= 1);
console.log(
  `mid-hand 2 (${before.matchUs} — ${before.matchThem}); killing the server…`
);

// the deploy: kill the server, bring it back
execSync("pkill -f 'racket server.rkt --port 8082' || true", { shell: "/bin/bash" });
await page.waitForTimeout(1500);
spawn("racket", ["server.rkt", "--port", "8082"], {
  cwd: REPO,
  detached: true,
  stdio: "ignore",
}).unref();
console.log("server restarted; waiting for the page to find its way back…");

// the page notices, retries, and lands back in the restored match
await page.waitForFunction(
  () => {
    const model = window.__game.state();
    return (
      model.roomName !== null &&
      model.phase !== "idle" &&
      model.phase !== "connecting" &&
      model.phase !== "lobby"
    );
  },
  undefined,
  { timeout: 60000 }
);
const after = await state();
check("rejoined the same table", after.roomName === before.roomName);
check("still hand 2 (the interrupted hand re-deals)", after.handNumber === 2);
check(
  `match score survived the restart (${after.matchUs} — ${after.matchThem})`,
  after.matchUs === before.matchUs && after.matchThem === before.matchThem
);

// the restored hand must play out to its gate
await playUntil((s) => s.phase === "hand-finished");
check("the restored hand played out", true);

await page.evaluate(() => window.__game.leaveMatch());
await page.waitForFunction(() => window.__game.state().phase === "idle", undefined, {
  timeout: 10000,
});

if (errors.length) {
  console.log("PAGE ERRORS:");
  for (const error of errors) console.log("  " + error);
}
await browser.close();
if (failures.length || errors.length) {
  console.log(`\n${failures.length} check(s) failed`);
  process.exit(1);
}
console.log("\nall restart checks passed");
process.exit(0);
