// A phone and a browser finish a match together, through a real server, including a dropped socket and
// a trip to the background on the phone. The phone is driven with real taps over adb. Needs a debug APK
// installed, the Vite dev server, and a game server started with MATCH-TARGET=0, so the first hand ends the match:
//   node scripts/test-android-match.mjs "http://localhost:5179/?server=ws://localhost:8082/test"
// ADB_SERIAL picks the device and PHONE_SERVER is the server's address from the phone's side.
// Screenshots land in /tmp/game-shots-android. It exits non-zero on any failure.
import { execFileSync, spawn, spawnSync } from "node:child_process";
import { existsSync, mkdirSync, writeFileSync } from "node:fs";
import { chromium } from "playwright";

const URL = process.argv[2] ?? "http://localhost:5179/?server=ws://localhost:8082/test";
const SERIAL = process.env.ADB_SERIAL ?? "emulator-5580";
const PHONE_SERVER = process.env.PHONE_SERVER ?? "ws://10.0.2.2:8082/test";
const SHOTS = "/tmp/game-shots-android";
// The installed app. Its classes keep the Kotlin package dev.synth.app.
const APP = "com.respawnfi.twentyeight";
mkdirSync(SHOTS, { recursive: true });

const sleep = (ms) => new Promise((resolve) => setTimeout(resolve, ms));
const run = (...args) => execFileSync("adb", ["-s", SERIAL, ...args], { encoding: "utf8", maxBuffer: 1 << 26 });
// Real input goes to whatever is on screen, so it stops the test rather than touch another app.
const adb = (...args) => {
  if (args[0] === "shell" && args[1] === "input") {
    const top = run("shell", "dumpsys activity activities | grep topResumedActivity");
    if (!top.includes(APP)) {
      console.error("FAIL: another app is on screen, so the test stopped");
      process.exit(3);
    }
  }
  return run(...args);
};
const shot = (name) => {
  const png = spawnSync("adb", ["-s", SERIAL, "exec-out", "screencap", "-p"], { maxBuffer: 1 << 27 });
  writeFileSync(`${SHOTS}/${name}.png`, png.stdout);
};

let failed = false;
const fail = (message) => {
  console.error(`FAIL: ${message}`);
  failed = true;
};
const check = (ok, message) => (ok ? console.log(`ok: ${message}`) : fail(message));

// --- the phone ---

const phoneState = async () => {
  adb("logcat", "-c");
  adb("shell", "am", "broadcast", "-n", `${APP}/dev.synth.app.DebugBridge`, "--es", "cmd", "state");
  for (let i = 0; i < 20; i++) {
    const line = adb("logcat", "-d", "-s", "synth:*").split("\n").find((l) => l.includes("state phase="));
    if (line) {
      const fields = {};
      for (const m of line.matchAll(/(\w+)=([^ ]*)/g)) fields[m[1]] = m[2];
      return fields;
    }
    await sleep(100);
  }
  throw new Error("the phone didn't report its state");
};

const uiNodes = () => {
  const xml = adb("exec-out", "uiautomator", "dump", "/dev/tty");
  return [...xml.matchAll(/<node [^>]*>/g)].map((m) => {
    const node = m[0];
    const bounds = node.match(/bounds="\[(\d+),(\d+)\]\[(\d+),(\d+)\]"/);
    return {
      id: node.match(/resource-id="([^"]*)"/)?.[1] ?? "",
      text: node.match(/ text="([^"]*)"/)?.[1] ?? "",
      x: bounds ? (+bounds[1] + +bounds[3]) >> 1 : 0,
      y: bounds ? (+bounds[2] + +bounds[4]) >> 1 : 0,
    };
  });
};

const hasTag = (tag) => uiNodes().some((n) => n.id === tag);

const tapTag = async (tag, timeout = 20000) => {
  const end = Date.now() + timeout;
  while (Date.now() < end) {
    const nodes = uiNodes();
    // Android 16 shows a system compatibility dialog over debuggable apps that carry a 4 KB library.
    const notice = nodes.find((n) => n.text === "Got it") ??
      (nodes.some((n) => n.text === "Don't show again") ? nodes.find((n) => n.text === "OK") : undefined);
    if (notice) {
      adb("shell", "input", "tap", `${notice.x}`, `${notice.y}`);
      await sleep(500);
      continue;
    }
    const node = nodes.find((n) => n.id === tag);
    if (node) {
      adb("shell", "input", "tap", `${node.x}`, `${node.y}`);
      return;
    }
    await sleep(400);
  }
  throw new Error(`the phone never showed ${tag}`);
};

const fanCards = async () => {
  adb("logcat", "-c");
  adb("shell", "am", "broadcast", "-n", `${APP}/dev.synth.app.DebugBridge`, "--es", "cmd", "dump");
  await sleep(250);
  const cards = [];
  for (const line of adb("logcat", "-d", "-s", "apsis:*").split("\n")) {
    const m = line.match(/table: card (\S+) (\d+) (-?\d+) (-?\d+) legal=(\d) armed=(\d)/);
    if (m) cards.push({ name: m[1], x: +m[3], y: +m[4], legal: m[5] === "1", armed: m[6] === "1" });
  }
  return cards;
};

// A real two-step touch: raise it, look again since it moved, and commit.
const phonePlays = async () => {
  const legal = (await fanCards()).find((c) => c.legal);
  if (!legal) return false;
  adb("shell", "input", "tap", `${legal.x}`, `${legal.y}`);
  await sleep(500);
  const raised = (await fanCards()).find((c) => c.name === legal.name);
  if (!raised?.armed) throw new Error(`a tap didn't raise ${legal.name}`);
  adb("shell", "input", "tap", `${raised.x}`, `${raised.y}`);
  phoneCardsPlayed++;
  return true;
};
let phoneCardsPlayed = 0;

const phoneActs = async (state) => {
  switch (state.pending) {
    case "Bid":
      // It plays too plainly to make a bid, so it leaves bidding to the bots unless it must open.
      if (hasTag("bid-pass")) await tapTag("bid-pass", 8000);
      else await tapTag(`bid-${state.minBid}`, 8000);
      await sleep(600);
      return true;
    case "Trump":
      await tapTag("trump-spades", 8000);
      await sleep(600);
      return true;
    case "Play":
      if (await phonePlays()) {
        await sleep(600);
        return true;
      }
      return false;
    default:
      return false;
  }
};

// --- the browser ---

const browser = await chromium.launch({
  ...(existsSync("/usr/bin/chromium") ? { executablePath: "/usr/bin/chromium" } : { channel: "chrome" }),
  headless: !process.env.HEADED,
  args: ["--enable-unsafe-swiftshader", "--use-angle=swiftshader"],
});
const page = await browser.newPage({ viewport: { width: 1280, height: 800 } });
page.on("pageerror", (error) => fail(`browser: ${error}`));

const browserActs = () =>
  page.evaluate(() => {
    const model = window.__game.state();
    const pending = model.pendingRequest?.kind;
    if (pending === "bid") {
      if (model.bidsPlaced === 0) window.__game.bid(model.pendingRequest.minBid);
      else window.__game.pass();
    } else if (pending === "trump") {
      window.__game.trump("hearts");
    } else if (pending === "play") {
      const legal = window.__game.legalCards();
      if (legal.length) window.__game.play(legal[0]);
    }
    return { phase: model.phase, pending, us: model.matchUs, them: model.matchThem, winner: model.matchWinner };
  });

// --- the match ---

let engineLog = null;
let engineText = "";

try {
  adb("shell", "am", "force-stop", APP);
  adb("shell", "pm", "clear", APP);
  adb("logcat", "-b", "crash", "-c");
  adb("logcat", "-c");
  // Kept running, since the steps below clear the log to read their own answers.
  engineLog = spawn("adb", ["-s", SERIAL, "logcat", "-s", "apsis:*"]);
  engineLog.stdout.on("data", (chunk) => (engineText += chunk));
  adb("shell", "am", "start", "-n", `${APP}/dev.synth.app.GameActivity`, "--es", "server", PHONE_SERVER);
  await sleep(6000);
  const pid = adb("shell", "pidof", APP).trim();

  await tapTag("name-input", 40000);
  await sleep(500);
  adb("shell", "input", "text", "Phone");
  adb("shell", "input", "keyevent", "KEYCODE_ESCAPE");
  await sleep(500);
  await tapTag("target-quick").catch(() => undefined);
  await tapTag("create-button");
  await sleep(3000);
  if (hasTag("target-quick")) await tapTag("target-quick");
  for (let i = 0; i < 20 && (await phoneState()).phase !== "LOBBY"; i++) await sleep(500);
  const created = await phoneState();
  check(created.phase === "LOBBY", "the phone created a table");
  shot("1-lobby");

  await page.goto(URL);
  await page.waitForSelector("#create-button", { state: "visible", timeout: 20000 });
  await page.fill("#player-name", "Browser");
  await page.click("#browse-button");
  await page.waitForSelector(".room-row button", { timeout: 20000 });
  await page.click(".room-row button");
  await page.waitForFunction(() => window.__game.state().phase === "lobby", null, { timeout: 15000 });
  console.log("ok: the browser joined the phone's table");

  await tapTag("add-bot");
  await sleep(1500);
  await tapTag("add-bot");
  await sleep(1500);
  await tapTag("start-game");

  let dropped = false;
  let backgrounded = false;
  let handsSeen = 0;
  const started = Date.now();
  let phase = "";
  while (Date.now() - started < 15 * 60 * 1000) {
    const web = await browserActs();
    const phone = await phoneState();
    if (phone.phase !== phase) {
      phase = phone.phase;
      console.log(`phone: ${phase}, match ${phone.match}, browser ${web.phase} ${web.us}-${web.them}`);
      shot(`phase-${String(handsSeen).padStart(2, "0")}-${phase}`);
    }
    if (web.phase === "finished" && phone.phase === "FINISHED") break;

    // Mid-hand, once: the socket goes down as a phone's OS would cut it.
    if (!dropped && phone.phase === "PLAYING" && +phone.played >= 1) {
      dropped = true;
      const handBefore = +phone.hand;
      adb("shell", "am", "broadcast", "-n", `${APP}/dev.synth.app.DebugBridge`, "--es", "cmd", "drop");
      await sleep(1200);
      const during = await phoneState();
      check(during.reconnecting === "true", "the dropped socket shows the reconnecting state");
      shot("2-reconnecting");
      for (let i = 0; i < 60; i++) {
        const now = await phoneState();
        if (now.reconnecting === "false" && now.phase === "PLAYING") break;
        await sleep(500);
      }
      const back = await phoneState();
      check(back.reconnecting === "false" && back.phase !== "IDLE", "the phone rejoined its seat by itself");
      check(+back.hand <= handBefore && +back.hand > 0, `the hand came back (${back.hand} cards)`);
      shot("3-rejoined");
      continue;
    }
    // Later, once: the app goes to the background and comes back.
    if (dropped && !backgrounded && phone.phase === "PLAYING" && phone.pending === "null") {
      backgrounded = true;
      adb("shell", "input", "keyevent", "KEYCODE_HOME");
      await sleep(4000);
      adb("shell", "am", "start", "-n", `${APP}/dev.synth.app.GameActivity`);
      await sleep(3000);
      const after = await phoneState();
      check(after.phase !== "IDLE", "the match survived the background");
      shot("4-resumed");
      continue;
    }

    if (phone.phase === "HAND_FINISHED") {
      handsSeen++;
      if (hasTag("next-hand")) await tapTag("next-hand", 5000).catch(() => undefined);
      await sleep(1000);
      continue;
    }
    if (!(await phoneActs(phone))) await sleep(500);
  }

  const web = await page.evaluate(() => {
    const m = window.__game.state();
    return { phase: m.phase, us: m.matchUs, them: m.matchThem, winner: m.matchWinner };
  });
  const phone = await phoneState();
  shot("5-match-over");
  await page.screenshot({ path: `${SHOTS}/5-match-over-browser.png` });

  check(web.phase === "finished", `the browser finished the match (${web.phase})`);
  check(phone.phase === "FINISHED", `the phone finished the match (${phone.phase})`);
  // The phone is seat 0 and the browser seat 1 or 3, so they're opponents or partners by the server's seating.
  const [, us, them] = phone.match.match(/^(-?\d+)-(-?\d+)$/).map(Number);
  check((us === web.us && them === web.them) || (us === web.them && them === web.us), `both saw the same score (${phone.match}, ${web.us}-${web.them})`);
  check(phone.winner !== "null" && web.winner !== null, "both saw a winner");
  check(hasTag("match-over"), "the phone shows the match-over screen");
  check(phoneCardsPlayed > 0, `the phone played ${phoneCardsPlayed} cards by touch`);
  check(dropped && backgrounded, "the drop and the background both happened");

  check(adb("shell", "pidof", APP).trim() === pid, "the app stayed one process throughout");
  const crash = adb("logcat", "-d", "-b", "crash");
  check(!crash.includes(APP), "no crashes");
  check(engineText.includes("Vulkan validation: on"), "validation layers were on");
  check(!/VUID|Validation Error/.test(engineText), "no Vulkan validation messages");
} catch (error) {
  fail(String(error));
  shot("failure");
} finally {
  engineLog?.kill();
  await browser.close();
}
process.exit(failed ? 1 : 0);
