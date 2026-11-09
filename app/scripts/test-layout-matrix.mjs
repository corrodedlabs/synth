// Every screen of the web client, at real phone sizes, against a real server:
// a touch-emulated page is walked through each screen and frozen there while
// three scripted players sit at the same table, then every size is measured.
//
//   node scripts/test-layout-matrix.mjs "http://localhost:5179/?port=8082" ["http://localhost:5179/?port=8083"]
//
// The optional second URL is a server started with MATCH-TARGET=0: the first
// hand ends the match, which is how the match-over panel gets measured.
//
//   SIZES=390x844,568x320   only these sizes
//   ONLY=start,lobby,match  only these groups (start, browser, reconnect, lobby,
//                           match, opening, bots, over, reload)
//   OUT=<dir>               where the shots, sheets and report go (default /tmp/game-shots-matrix)
//   WEBKIT=<pw_run.sh>      use this WebKit build instead of Chrome
//
// Standings show rows only if the server's GAME-DB has completed matches.
// Each state gets a contact sheet in OUT: look at those as well as the report.
import { chromium, webkit } from "playwright";
import { existsSync, mkdirSync, writeFileSync } from "node:fs";
import { join } from "node:path";

const PAGE_URL = process.argv[2] ?? "http://localhost:5173/";
const OVER_URL = process.argv[3] ?? null;
const OUT = process.env.OUT ?? "/tmp/game-shots-matrix";
mkdirSync(OUT, { recursive: true });

const ALL_SIZES = [
  "280x653", "320x568", "360x640", "360x740", "360x800", "390x844", "412x915", "768x1024",
  "568x320", "640x360", "844x390", "915x412", "1024x768", "1280x800",
].map((text) => {
  const [width, height] = text.split("x").map(Number);
  return { width, height };
});
const SIZES = process.env.SIZES
  ? process.env.SIZES.split(",").map((text) => {
      const [width, height] = text.split("x").map(Number);
      return { width, height };
    })
  : ALL_SIZES;
const GROUPS = process.env.ONLY?.split(",") ?? null;
const wanted = (group) => GROUPS === null || GROUPS.includes(group);
const sizeName = ({ width, height }) => `${width}x${height}`;

const LONG_PLAYER = "Venkatasubramanian S"; // the input's limit is 20 characters
const LONG_ROOM = "Bhagyalakshmi Reddy's table #a1b";
const OPPONENTS = ["bhagyalakshmi reddy", "venkatasubramanian", "padmanabhan iyer"];

// WEBKIT=<path to pw_run.sh> walks the same screens in WebKit, Safari's engine.
const browser = process.env.WEBKIT
  ? await webkit.launch({ executablePath: process.env.WEBKIT, headless: !process.env.HEADED })
  : await chromium.launch({
      ...(existsSync("/usr/bin/chromium")
        ? { executablePath: "/usr/bin/chromium" }
        : { channel: "chrome" }),
      headless: !process.env.HEADED,
      // A Mac's Chrome has a GPU to use, and no WebGL at all if told to use SwiftShader.
      args: process.platform === "darwin" ? [] : ["--enable-unsafe-swiftshader", "--use-angle=swiftshader"],
    });

const faults = [];
const consoleErrors = [];
const statesSeen = new Map(); // state -> [{size, shot, faults}]

// ---------------------------------------------------------------- measuring

// Runs in the page. Collects the things a player reads and touches, and returns
// every rule they break. Rect checks use a 1px tolerance and overlaps ignore
// the last 2px: rotated buttons and cards report slightly generous boxes.
function measureScreen() {
  const W = window.innerWidth;
  const H = window.innerHeight;
  const found = [];
  const fault = (rule, subject, detail = {}) => found.push({ rule, subject, ...detail });
  const box = (rect) => ({
    left: Math.round(rect.left * 10) / 10,
    top: Math.round(rect.top * 10) / 10,
    right: Math.round(rect.right * 10) / 10,
    bottom: Math.round(rect.bottom * 10) / 10,
  });
  const shown = (el) => {
    if (!(el instanceof Element) || el.getClientRects().length === 0) return false;
    if (getComputedStyle(el).visibility !== "visible") return false;
    let opacity = 1;
    for (let node = el; node && node !== document.documentElement; node = node.parentElement) {
      opacity *= Number.parseFloat(getComputedStyle(node).opacity);
    }
    return opacity >= 0.05;
  };
  const $ = (selector) => document.querySelector(selector);
  const $$ = (selector) => [...document.querySelectorAll(selector)];
  const nameOf = (el) => {
    if (el.id) return `#${el.id}`;
    const owner = el.closest("[id]");
    const cls = el.classList?.[0];
    return `${owner ? `#${owner.id} ` : ""}${cls ? `.${cls}` : el.tagName.toLowerCase()}`;
  };
  const textBox = (el) => {
    const range = document.createRange();
    range.selectNodeContents(el);
    return range.getBoundingClientRect();
  };
  const intersects = (a, b, slack = 2) =>
    Math.min(a.right, b.right) - Math.max(a.left, b.left) > slack &&
    Math.min(a.bottom, b.bottom) - Math.max(a.top, b.top) > slack;

  // Opaque full-screen pages and modals: what is under them can't be seen, so
  // can't be in the way. Everything is measured against the top one.
  const fullPage = [$("#start-screen"), $("#lobby-panel")].find(shown) ?? null;
  const modal = [$("#help-panel"), $("#leaderboard-panel")].find(shown) ?? null;
  const abandon = shown($("#abandon-panel")) ? $("#abandon-panel") : null;
  const resultUp = shown($("#result-panel")) && !abandon;
  const underCover = (el) => {
    if (modal) return !modal.contains(el);
    // opaque, and only the table's bars are drawn over it
    if (abandon) return !abandon.contains(el) && !el.closest("#top-bar, #bottom-bar, #menu-panel");
    if (fullPage) {
      return !fullPage.contains(el) && !el.closest("#top-bar, #help-button, #menu-panel, #status-line");
    }
    return false;
  };

  // --- the pieces a player reads: none may land on another
  const pieces = [];
  const add = (name, group, rect, el = null) => {
    if (rect) pieces.push({ name, group, rect: box(rect), el });
  };
  const addEl = (name, group, selector) => {
    for (const el of $$(selector)) {
      if (shown(el) && !underCover(el)) add(name, group, el.getBoundingClientRect(), el);
    }
  };
  addEl("menu-button", "hud", "#menu-button");
  addEl("help-button", "hud", "#help-button");
  addEl("score", "hud", "#score-pips");
  addEl("progress", "hud", "#hand-progress");
  addEl("contract", "hud", "#contract-marks");
  if (shown($("#contract-whose")) && $("#contract-whose").textContent) {
    add("contract-whose", "hud", textBox($("#contract-whose")));
  }
  addEl("arrange-button", "bar", "#arrange-button");
  addEl("emote-button", "bar", "#emote-button");
  const statusLine = $("#status-line");
  if (shown(statusLine) && statusLine.textContent && !underCover(statusLine)) {
    add("status", "bar", textBox(statusLine));
  }
  for (const seat of [1, 2, 3]) {
    const label = document.getElementById(`seat-label-${seat}`);
    if (!shown(label) || underCover(label)) continue;
    const group = `seat${seat}`;
    addEl("marker", group, `#seat-label-${seat} .seat-marker`);
    const copy = label.querySelector(".player-copy");
    if (copy) add("name", group, textBox(copy));
    addEl("partner", group, `#seat-label-${seat} .partner-tag`);
    const call = label.querySelector(".seat-action");
    if (call && call.classList.contains("visible")) {
      add("call", group, call.classList.contains("pass") ? textBox(call) : call.getBoundingClientRect());
    }
    // the turn word is a pseudo-element placed beside the ring
    if (label.hasAttribute("data-turn")) {
      const after = getComputedStyle(label, "::after");
      if (after.display !== "none") {
        const labelRect = label.getBoundingClientRect();
        const left = labelRect.left + Number.parseFloat(after.left);
        const top = labelRect.top + Number.parseFloat(after.top);
        add("turn", group, {
          left,
          right: left + Number.parseFloat(after.width),
          top,
          bottom: top + Number.parseFloat(after.height),
        });
      }
    }
  }
  const ownCall = $("#seat-action-0");
  if (ownCall.classList.contains("visible") && shown(ownCall)) {
    add("own-call", "own", ownCall.classList.contains("pass") ? textBox(ownCall) : ownCall.getBoundingClientRect());
  }
  addEl("bid-panel", "bid", "#bid-panel");
  addEl("trump-title", "trump", "#trump-panel .panel-title");
  addEl("trump-card", "trump", "#trump-panel .suit-card");
  addEl("expose", "expose", "#expose-button");
  addEl("table-moment", "moment", "#table-moment");
  addEl("trick-outcome", "outcome", "#trick-outcome");
  $$(".emote-bubble").forEach((bubble, index) => {
    if (shown(bubble)) add(`bubble${index}`, "bubble", bubble.getBoundingClientRect());
  });
  addEl("abandon", "abandon", "#abandon-panel");
  addEl("reconnect", "reconnect", "#reconnect-panel");

  const game = window.__game;
  if (!fullPage && !modal && !abandon && !resultUp && shown($("canvas"))) {
    game?.handCardBounds?.().forEach((card, index) => add(`hand${index}`, "hand", card));
    game?.playedCardBounds?.().forEach((card) => add(`played${card.player}`, "played", card));
  }
  if (resultUp) {
    // the panel's own background fades out: its content is what must stay clear
    for (const child of $("#result-panel").children) {
      if (shown(child)) {
        const named = child.id === "next-hand" || child.id === "go-home" ? child.getBoundingClientRect() : textBox(child);
        add(child.id, "result", named.width ? named : child.getBoundingClientRect());
      }
    }
  }

  // Groups that are allowed to touch each other.
  const allowed = (a, b) => {
    if (a.group === b.group && ["hand", "played", "trump", "result", "hud", "bubble"].includes(a.group)) return true;
    if (a.el && b.el && (a.el.contains(b.el) || b.el.contains(a.el))) return true;
    // a bubble drifts over the table on purpose, and leaves on its own
    if (a.group === "bubble" || b.group === "bubble") return true;
    // a moment or trick outcome sits over the cards in the middle of the table, for a moment
    const message = (piece) => piece.group === "moment" || piece.group === "outcome";
    const card = (piece) => piece.group === "played" || piece.group === "hand";
    if ((message(a) && card(b)) || (message(b) && card(a))) return true;
    return false;
  };
  for (let i = 0; i < pieces.length; i++) {
    for (let j = i + 1; j < pieces.length; j++) {
      const a = pieces[i];
      const b = pieces[j];
      if (!allowed(a, b) && intersects(a.rect, b.rect)) {
        fault("overlap", `${a.group}/${a.name} x ${b.group}/${b.name}`, { a: a.rect, b: b.rect });
      }
    }
  }
  // Cards on the table may touch, but one must not bury another.
  const played = pieces.filter((piece) => piece.group === "played");
  for (let i = 0; i < played.length; i++) {
    for (let j = i + 1; j < played.length; j++) {
      const a = played[i].rect;
      const b = played[j].rect;
      const area = Math.max(0, Math.min(a.right, b.right) - Math.max(a.left, b.left)) *
        Math.max(0, Math.min(a.bottom, b.bottom) - Math.max(a.top, b.top));
      const smallest = Math.min((a.right - a.left) * (a.bottom - a.top), (b.right - b.left) * (b.bottom - b.top));
      if (area > smallest * 0.35) {
        fault("card-buried", `${played[i].name} x ${played[j].name}`, { a, b });
      }
    }
  }
  // A seat's call must read as that seat's.
  for (const seat of [1, 2, 3]) {
    const marker = pieces.find((piece) => piece.group === `seat${seat}` && piece.name === "marker");
    const call = pieces.find((piece) => piece.group === `seat${seat}` && piece.name === "call");
    if (marker && call) {
      const gapX = Math.max(0, marker.rect.left - call.rect.right, call.rect.left - marker.rect.right);
      const gapY = Math.max(0, marker.rect.top - call.rect.bottom, call.rect.top - marker.rect.bottom);
      if (Math.hypot(gapX, gapY) > 34) {
        fault("call-detached", `seat${seat}`, { marker: marker.rect, call: call.rect });
      }
    }
  }
  for (const piece of pieces) {
    const { left, top, right, bottom } = piece.rect;
    const off = left < -1 || top < -1 || right > W + 1 || bottom > H + 1;
    if (off && !piece.name.startsWith("bubble")) {
      fault("off-screen", `${piece.group}/${piece.name}`, { rect: piece.rect });
    }
  }
  for (const piece of pieces) {
    if (piece.name.startsWith("bubble")) {
      const { left, top, right, bottom } = piece.rect;
      if (left < -1 || top < -1 || right > W + 1 || bottom > H + 1) {
        fault("off-screen", "bubble", { rect: piece.rect });
      }
    }
  }

  // --- everything a finger can press
  // A list scrolls in its own box: what's outside that box isn't on the screen.
  const scrolledOut = (el, rect) => {
    const list = el.closest("#room-list, #help-rules, #leaderboard-list");
    if (!list) return false;
    const outer = list.getBoundingClientRect();
    return rect.top < outer.top - 1 || rect.bottom > outer.bottom + 1;
  };
  const controls = $$("#ui-layer button, #ui-layer input").filter((el) => shown(el) && !underCover(el));
  // With the open tables showing, the start screen has scrolled to them: its own
  // actions are a swipe away, under the buttons that float over the page.
  const browsing = shown($("#room-browser"));
  for (const el of controls) {
    const rect = el.getBoundingClientRect();
    if (scrolledOut(el, rect)) continue;
    if (browsing && el.closest("#start-screen") && !el.closest("#room-browser")) continue;
    if (Math.min(rect.width, rect.height) < 40) {
      fault("small-target", nameOf(el), {
        width: Math.round(rect.width),
        height: Math.round(rect.height),
        text: el.textContent?.trim().slice(0, 20),
      });
    }
    if (rect.left < -1 || rect.top < -1 || rect.right > W + 1 || rect.bottom > H + 1) {
      const scroller = el.closest("#start-screen, #lobby-panel, #help-panel, #leaderboard-panel, #menu-panel, #arrange-panel, #room-list, #result-panel");
      fault(rect.bottom > H + 1 && scroller ? "below-fold" : "off-screen-control", nameOf(el), { rect: box(rect), text: el.textContent?.trim().slice(0, 20) });
      continue;
    }
    // The status line lets a press through, but it's drawn over what it lands on.
    if (fullPage && fullPage.contains(el) && shown(statusLine) && statusLine.textContent && intersects(rect, textBox(statusLine))) {
      fault("status-over-control", nameOf(el), { rect: box(rect), status: box(textBox(statusLine)) });
    }
    // Press it in five places: all must reach it.
    for (const [fx, fy] of [[0.5, 0.5], [0.25, 0.3], [0.75, 0.3], [0.25, 0.7], [0.75, 0.7]]) {
      const hit = document.elementFromPoint(rect.left + rect.width * fx, rect.top + rect.height * fy);
      if (!hit || !el.contains(hit)) {
        // A popover is opened on purpose, and a tap closes it: what it hides can wait.
        if (hit?.closest("#menu-panel, #arrange-panel, #emote-strip")) continue;
        fault("covered", nameOf(el), {
          by: hit ? nameOf(hit) : null,
          at: [Math.round(rect.left + rect.width * fx), Math.round(rect.top + rect.height * fy)],
          rect: box(rect),
        });
        break;
      }
    }
  }

  // --- every piece of text
  const walker = document.createTreeWalker($("#ui-layer"), NodeFilter.SHOW_TEXT);
  const texts = [];
  for (let node = walker.nextNode(); node; node = walker.nextNode()) {
    if (!node.nodeValue.trim()) continue;
    const parent = node.parentElement;
    if (!shown(parent) || underCover(parent)) continue;
    if (resultUp && !parent.closest("#result-panel, #top-bar, #abandon-panel, #bottom-bar, #menu-panel")) continue;
    const range = document.createRange();
    range.selectNodeContents(node);
    const rect = range.getBoundingClientRect();
    if (rect.width === 0 || scrolledOut(parent, rect)) continue;
    // a bubble drifts over the table on purpose, and leaves on its own
    if (parent.closest(".emote-bubble")) continue;
    texts.push({ parent, rect, text: node.nodeValue.trim().slice(0, 28) });
    if (rect.left < -1 || rect.right > W + 1 || rect.top < -1 || rect.bottom > H + 1) {
      const scroller = parent.closest("#start-screen, #lobby-panel, #help-panel, #leaderboard-panel, #menu-panel, #arrange-panel, #room-list");
      if (!scroller) fault("text-off-screen", nameOf(parent), { text: node.nodeValue.trim().slice(0, 28), rect: box(rect) });
    }
    const style = getComputedStyle(parent);
    if (
      style.overflowX !== "visible" && style.overflowX !== "auto" && style.overflowX !== "scroll" &&
      parent.scrollWidth > parent.clientWidth + 1
    ) {
      fault(style.textOverflow === "ellipsis" ? "ellipsis" : "text-clipped", nameOf(parent), {
        text: parent.textContent.trim().slice(0, 28),
        scrollWidth: parent.scrollWidth,
        clientWidth: parent.clientWidth,
      });
    }
  }
  for (let i = 0; i < texts.length; i++) {
    for (let j = i + 1; j < texts.length; j++) {
      const a = texts[i];
      const b = texts[j];
      if (a.parent === b.parent) continue;
      if (a.parent.contains(b.parent) || b.parent.contains(a.parent)) continue;
      // A popover is opaque, on top, and opened on purpose: it hides what's under it.
      const popover = (el) => el.closest("#menu-panel, #arrange-panel, #emote-strip");
      if (popover(a.parent) !== popover(b.parent)) continue;
      if (intersects(a.rect, b.rect)) {
        fault("text-overlap", `${nameOf(a.parent)} x ${nameOf(b.parent)}`, { a: a.text, b: b.text });
      }
    }
  }
  // Text must not spill out of its panel either.
  for (const panel of $$("#bid-panel, #help-panel, #leaderboard-panel, #abandon-panel, #reconnect-panel, #menu-panel, #arrange-panel, #table-moment, #trick-outcome, .room-row, #join-link")) {
    if (!shown(panel) || underCover(panel)) continue;
    const outer = panel.getBoundingClientRect();
    for (const text of texts) {
      if (!panel.contains(text.parent)) continue;
      if (text.rect.left < outer.left - 1 || text.rect.right > outer.right + 1) {
        fault("text-outside-panel", nameOf(panel), { text: text.text, rect: box(text.rect), panel: box(outer) });
      }
    }
  }

  // --- scrolling
  const page = document.scrollingElement;
  if (page.scrollWidth > W + 1) fault("page-scroll-x", "document", { scrollWidth: page.scrollWidth, width: W });
  if (page.scrollHeight > H + 1) fault("page-scroll-y", "document", { scrollHeight: page.scrollHeight, height: H });
  if (window.scrollX !== 0 || window.scrollY !== 0) fault("page-moved", "document", { x: window.scrollX, y: window.scrollY });
  for (const el of $$("#start-screen, #lobby-panel, #help-panel, #leaderboard-panel, #room-list, #menu-panel, #arrange-panel, #result-panel")) {
    if (!shown(el) || underCover(el)) continue;
    if (el.scrollWidth > el.clientWidth + 1 && getComputedStyle(el).overflowX !== "hidden") {
      fault("container-scroll-x", `#${el.id}`, { scrollWidth: el.scrollWidth, clientWidth: el.clientWidth });
    }
    if (el.scrollHeight > el.clientHeight + 1 && el.id !== "room-list" && !(browsing && el.id === "start-screen")) {
      fault("needs-scroll", `#${el.id}`, { scrollHeight: el.scrollHeight, clientHeight: el.clientHeight });
    }
    const rect = el.getBoundingClientRect();
    if (rect.top < -1 || rect.bottom > H + 1 || rect.left < -1 || rect.right > W + 1) {
      fault("off-screen", `#${el.id}`, { rect: box(rect) });
    }
  }
  return { faults: found, pieces: pieces.map(({ name, group, rect }) => ({ name, group, rect })) };
}

// ---------------------------------------------------------------- recording

// Words that have outlived their moment, which no measurement of the layout can see.
function staleWords() {
  const { members, pendingRequest } = window.__game.state();
  const status = document.getElementById("status-line").textContent;
  const pending = pendingRequest?.kind;
  const stale = [];
  if (status === "Joining the table…" && members.length > 0) stale.push({ rule: "stale-status", subject: status });
  if (status === "Reconnected — back at the table." && pending === "bid") stale.push({ rule: "stale-status", subject: status });
  if (pending === "trump" && document.querySelector('[id^="seat-label-"][data-turn]')) {
    stale.push({ rule: "stale-turn", subject: "another seat lit while we choose trump" });
  }
  return stale;
}

async function record(page, state, size, extra = []) {
  const name = `${state}-${sizeName(size)}`;
  const { faults: measured, pieces } = await page.evaluate(measureScreen);
  const all = [...measured, ...(await page.evaluate(staleWords)), ...extra];
  const unique = [];
  const seen = new Set();
  for (const item of all) {
    const key = `${item.rule}|${item.subject}`;
    if (seen.has(key)) continue;
    seen.add(key);
    unique.push(item);
  }
  const shot = `${name}.png`;
  await page.screenshot({ path: join(OUT, shot) });
  for (const item of unique) faults.push({ state, size: sizeName(size), ...item });
  if (!statesSeen.has(state)) statesSeen.set(state, []);
  statesSeen.get(state).push({ size, shot, faults: unique, pieces });
  const tag = unique.length ? `${unique.length} fault(s): ${[...new Set(unique.map((f) => f.rule))].join(", ")}` : "clean";
  console.log(`  ${sizeName(size).padEnd(9)} ${tag}`);
}

// Measure one frozen state at every size.
async function sweep(page, state, { before = null, after = null, settle = 650, sizes = SIZES } = {}) {
  console.log(state);
  for (const size of sizes) {
    await page.setViewportSize(size);
    await page.waitForTimeout(settle);
    const extra = before ? (await before(size)) ?? [] : [];
    if (before) await page.waitForTimeout(350); // what it opened fades in, and isn't measured unseen
    await record(page, state, size, extra);
    if (after) await after(size);
  }
}

// Puts a message on the status line, as the game would. Empty takes it down.
function setStatus(page, text) {
  return page.evaluate((message) => {
    const line = document.getElementById("status-line");
    line.textContent = message;
    line.classList.toggle("visible", message.length > 0);
  }, text);
}

// A real finger on a control, wherever it is. No scrolling into view: a player
// can't scroll this page, and a control that can't be reached is itself a fault.
async function fingerTap(page, selector) {
  const target = await page.evaluate((query) => {
    const el = document.querySelector(query);
    if (!el) return null;
    const rect = el.getBoundingClientRect();
    return { x: rect.left + rect.width / 2, y: rect.top + rect.height / 2 };
  }, selector);
  if (!target) throw new Error(`no ${selector} to tap`);
  await page.touchscreen.tap(target.x, target.y);
}

async function newTouchPage({ gate = false } = {}) {
  const context = await browser.newContext({
    viewport: SIZES[0],
    hasTouch: true,
    isMobile: true,
    deviceScaleFactor: 1,
    userAgent:
      "Mozilla/5.0 (Linux; Android 14; Pixel 8) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/126.0.0.0 Mobile Safari/537.36",
  });
  await context.addInitScript(() => {
    try {
      localStorage.setItem("game28-animation-speed", "quick");
    } catch {
      // the walk is just slower
    }
  });
  const page = await context.newPage();
  page.on("console", (message) => {
    if (message.type() !== "error") return;
    const text = message.text();
    // the reconnecting screen is reached by a server that isn't there
    if (text.includes("WebSocket connection") || text.includes("ERR_CONNECTION_REFUSED")) return;
    consoleErrors.push(text);
  });
  page.on("pageerror", (error) => consoleErrors.push(String(error)));

  // Lets a play be held back from the server, which freezes a full trick.
  const holdBack = { on: false, frames: [], server: null };
  if (gate) {
    await page.routeWebSocket(/\/test$/, (socket) => {
      const server = socket.connectToServer();
      holdBack.server = server;
      socket.onMessage((frame) => {
        if (holdBack.on && String(frame).startsWith("(card-played")) holdBack.frames.push(frame);
        else server.send(frame);
      });
      server.onMessage((frame) => socket.send(frame));
    });
  }
  return { context, page, holdBack };
}

// ---------------------------------------------------------- scripted players

const SUITS_SERVER = { heart: "hearts", diamond: "diamonds", club: "clubs", spade: "spades" };
const parseCard = (text) => {
  const match = text.match(/#s\(card (\w+) (-?\d+) (\w+) (-?\d+)\)/);
  return match ? { text, rank: match[1], suit: match[3] } : null;
};
const uid = () => Math.random().toString(36).slice(2, 8).padEnd(6, "x");
const slug = (name) => name.toLowerCase().replace(/[^a-z0-9]+/g, "-");

// The opener has to bid, and nobody else needs to.
const openOrPass = (min) => (min === 16 ? 16 : "pass");

// A player at the real table, over a real socket. It answers the server with
// whatever the walk has told it to: bids, the suit to lead, when to speak.
class TablePlayer {
  constructor(name, port) {
    this.name = name;
    this.email = `${slug(name)}-${uid()}@game.local`;
    this.port = port;
    this.hand = [];
    this.onBid = openOrPass;
    this.onTrump = () => "spade";
    this.onLead = (hand) => hand[0];
    this.pendingPlay = null;
  }

  async connect() {
    this.ws = new WebSocket(`ws://localhost:${this.port}/test`);
    await new Promise((resolve, reject) => {
      this.ws.onopen = resolve;
      this.ws.onerror = reject;
    });
    this.ws.onmessage = (event) => this.onFrame(String(event.data));
    this.send(`(connect-user "${this.email}" "none")`);
    await new Promise((resolve) => setTimeout(resolve, 250));
    return this;
  }

  send(frame) {
    if (this.ws.readyState === WebSocket.OPEN) this.ws.send(frame);
  }

  async onFrame(frame) {
    if (frame.startsWith("(hand ")) {
      const cards = frame.match(/#s\(card [^)]*\)/g) ?? [];
      if (cards.length) this.hand.push(...cards.map(parseCard));
    } else if (frame.startsWith("(request-bid ")) {
      const min = Number(frame.match(/\(request-bid (\d+)\)/)[1]);
      const answer = await this.onBid(min);
      this.send(`(put-bid "${this.email}" ${answer})`);
    } else if (frame.startsWith("(choose-trump")) {
      this.send(`(selected-trump "${this.email}" ${await this.onTrump()})`);
    } else if (frame.startsWith("(play-card")) {
      const first = frame.match(/\(first-suit \. (\w+|#f)\)/)?.[1] ?? "#f";
      const follow = this.hand.filter((card) => card.suit === first);
      const card = first === "#f" ? await this.onLead(this.hand) : follow[0] ?? this.hand[0];
      this.hand = this.hand.filter((other) => other !== card);
      this.send(`(card-played "${this.email}" ${card.text})`);
    } else if (frame.startsWith("(game-aborted")) {
      this.hand = [];
    }
  }

  close() {
    this.ws.close();
  }
}

const open = (name, port) => new TablePlayer(name, port).connect();

const deferred = () => {
  let resolve;
  const promise = new Promise((done) => {
    resolve = done;
  });
  return { promise, resolve };
};

const waitPending = (page, kind, timeout = 90_000) =>
  page.waitForFunction((want) => window.__game.state().pendingRequest?.kind === want, kind, { timeout });

// The auction can come back round to us after a pass, so pass until the first card is asked for.
const passUntilPlay = (page) =>
  page.waitForFunction(
    () => {
      const game = window.__game;
      if (game.state().pendingRequest?.kind === "bid") game.pass();
      return game.state().pendingRequest?.kind === "play";
    },
    null,
    { timeout: 90_000, polling: 300 }
  );

const portOf = (url) => Number(new globalThis.URL(url).searchParams.get("port"));

// Fills the table round a page that's the host. Join order sets the seats: the
// host plays fourth in the first hand, so everyone has spoken by the time it's asked.
async function fillTable(page, port, names = OPPONENTS) {
  const room = await page.evaluate(() => window.__game.state().roomName);
  const players = [];
  for (const name of names) {
    const player = await open(name, port);
    player.send(`(join-room ${JSON.stringify(room)} "${player.email}")`);
    players.push(player);
    await page.waitForFunction((count) => window.__game.state().members.length === count, players.length + 1);
  }
  // By view: index 0 was seated left, 1 opposite, 2 right.
  return { room, left: players[0], partner: players[1], right: players[2], all: players };
}

// A suit the page can't follow, led by whoever holds it.
function leadWhatThePageLacks(page) {
  return async (hand) => {
    await page.waitForFunction(() => window.__game.state().hand.length >= 1, null, { timeout: 30_000 }).catch(() => {});
    const held = await page.evaluate(() => window.__game.state().hand.map((card) => card.suit));
    return hand.find((card) => !held.includes(SUITS_SERVER[card.suit])) ?? hand[0];
  };
}

const settledCards = (page) =>
  page.waitForFunction(() => window.__game.playedCardBounds().every((card) => !card.moving), null, { timeout: 8000 }).catch(() => {});

// Forces a table message on, as the game does for a moment at a time, so it can
// be measured at every size. Nothing here changes what the game thinks.
const showMoment = (page, title, detail, hint = "tap anywhere to continue") =>
  page.evaluate(([title, detail, hint]) => {
    const moment = document.getElementById("table-moment");
    document.getElementById("table-moment-title").textContent = title;
    document.getElementById("table-moment-detail").textContent = detail;
    document.getElementById("table-moment-hint").textContent = hint;
    moment.classList.add("visible", "dismissible");
    document.getElementById("ui-layer").classList.add("table-announcing");
  }, [title, detail, hint]);
const hideMoment = (page) =>
  page.evaluate(() => {
    document.getElementById("table-moment").classList.remove("visible", "dismissible");
    document.getElementById("ui-layer").classList.remove("table-announcing");
  });

// ---------------------------------------------------------------- the walk

async function walkStart() {
  const { context, page } = await newTouchPage();
  await page.goto(PAGE_URL);
  await page.waitForSelector("#create-button", { state: "visible", timeout: 20_000 });
  await page.fill("#player-name", "Asha");
  await page.evaluate(() => document.activeElement?.blur());
  await sweep(page, "start");
  // the status line stays up on the home page, over whatever is at the bottom
  await sweep(page, "start-status", {
    before: () => setStatus(page, "That took too long. Check your connection and try again."),
    after: () => setStatus(page, ""),
  });

  await sweep(page, "help", {
    before: () => page.evaluate(() => document.getElementById("help-button").click()),
    after: () => page.evaluate(() => document.getElementById("help-close").click()),
  });
  await context.close();

  // the invite: a table whose name is as long as names get
  const invite = await newTouchPage();
  const url = new globalThis.URL(PAGE_URL);
  url.searchParams.set("join", LONG_ROOM);
  await invite.page.goto(url.toString());
  await invite.page.waitForSelector("#join-link:not(.hidden)", { timeout: 20_000 });
  await sweep(invite.page, "start-invite");
  await invite.context.close();
}

async function walkBrowser() {
  const port = portOf(PAGE_URL);
  const hosts = [];
  for (const [index, table] of [
    "Ravi's table #r2d",
    LONG_ROOM,
    "Meena's table #m3n",
    "Kabir's table #k4b",
    "Tara's table #t5r",
  ].entries()) {
    const host = await open(`host ${index} ${uid()}`, port);
    host.send(`(make-room "${host.email}" ${JSON.stringify(table)} ${index % 2 ? 2 : 6})`);
    hosts.push(host);
  }
  const { context, page } = await newTouchPage();
  await page.goto(PAGE_URL);
  await page.waitForSelector("#create-button", { state: "visible", timeout: 20_000 });
  await page.fill("#player-name", "Asha");
  await page.evaluate(() => document.activeElement?.blur());
  await sweep(page, "browser", {
    before: async () => {
      // back to the top of the screen, and tap Join a table as a player would
      await page.evaluate(() => {
        document.getElementById("start-screen").scrollTop = 0;
        document.getElementById("browse-button").click();
      });
      await page.waitForSelector(".room-row", { timeout: 15_000 });
      await page.waitForTimeout(250);
      const reachable = await page.evaluate(() => {
        const row = document.querySelector(".room-row").getBoundingClientRect();
        return row.top >= 0 && row.bottom <= window.innerHeight;
      });
      return reachable ? [] : [{ rule: "off-screen", subject: "first .room-row after tapping Join a table" }];
    },
  });

  // Standings
  await sweep(page, "standings", {
    before: async () => {
      await page.evaluate(() => {
        document.getElementById("start-screen").scrollTop = 0;
        document.getElementById("standings-button").click();
      });
      await page.waitForSelector("#leaderboard-list li:not(.leaderboard-empty)", { timeout: 8000 }).catch(() => {});
      await page.waitForTimeout(150);
    },
    after: () => page.evaluate(() => document.getElementById("leaderboard-close").click()),
  });
  await context.close();
  hosts.forEach((host) => host.close());
}

async function walkReconnect() {
  const { context, page } = await newTouchPage();
  await context.addInitScript(() => {
    localStorage.setItem("game28-active-match", "asha-abcdef@game.local");
    localStorage.setItem("game28-player-name", "Asha");
  });
  // no server on this port: the retry loop holds the screen as long as we need.
  // (Not a low port: WebKit refuses those with an exception instead of failing the connection.)
  await page.goto(new globalThis.URL("/?port=59999", PAGE_URL).toString());
  await page.waitForSelector("#reconnect-panel:not(.hidden)", { timeout: 20_000 });
  await sweep(page, "reconnect");
  await context.close();
}

// The lobby and a whole hand: the page hosts, three players sit with it.
async function walkMatch() {
  const port = portOf(PAGE_URL);
  const { context, page, holdBack } = await newTouchPage({ gate: true });
  await page.goto(PAGE_URL);
  await page.waitForSelector("#create-button", { state: "visible", timeout: 20_000 });
  await page.fill("#player-name", LONG_PLAYER);
  await page.evaluate(() => document.activeElement?.blur());
  await fingerTap(page, '.mode-card[data-target="6"]');
  await page.evaluate(() => document.getElementById("create-button").click());
  await page.waitForSelector("#lobby-panel:not(.hidden)", { timeout: 15_000 });
  await sweep(page, "lobby-empty");
  const table = await fillTable(page, port);
  if (wanted("lobby")) {
    await sweep(page, "lobby-full");
    await sweep(page, "lobby-full-status", {
      before: () => setStatus(page, "padmanabhan-iyer left the table."),
      after: () => setStatus(page, ""),
    });
  }
  if (!wanted("match")) {
    await context.close();
    table.all.forEach((player) => player.close());
    return;
  }

  // Right opens; hold the partner, to see a seat that's thinking.
  const partnerThinks = deferred();
  table.right.onBid = () => 16;
  table.partner.onBid = async () => {
    await partnerThinks.promise;
    return "pass";
  };
  table.left.onBid = () => "pass";
  await page.evaluate(() => document.getElementById("start-game").click());
  await page.waitForFunction(() => window.__game.state().seatNames !== null, null, { timeout: 15_000 });
  await page.waitForFunction(() => window.__game.state().activePlayer === 2, null, { timeout: 30_000 });
  await page.waitForFunction(() => !document.getElementById("table-moment").classList.contains("visible"), null, { timeout: 8000 }).catch(() => {});
  await sweep(page, "others-bidding");
  partnerThinks.resolve();
  await waitPending(page, "bid");
  await page.waitForTimeout(400);
  await sweep(page, "bid-later");

  // The same panel with three seals, the widest calls there are.
  await page.evaluate(() => {
    for (const [seat, bid] of [[1, 26], [2, 27], [3, 28]]) {
      const action = document.getElementById(`seat-action-${seat}`);
      action.classList.remove("pass");
      action.classList.add("visible", "raise");
      const seal = document.createElement("span");
      seal.className = "seal";
      seal.textContent = String(bid);
      action.replaceChildren(seal);
    }
  });
  await sweep(page, "bid-later-seals");

  // We raise, and everyone thinks about it: our own call shows at the bottom.
  const rightThinks = deferred();
  table.right.onBid = async () => {
    await rightThinks.promise;
    return "pass";
  };
  table.partner.onBid = () => "pass";
  table.left.onBid = () => "pass";
  await page.evaluate(() => window.__game.bid(19));
  await page.waitForFunction(() => window.__game.state().activePlayer === 1, null, { timeout: 15_000 });
  await page.waitForTimeout(400);
  await sweep(page, "own-call");
  rightThinks.resolve();
  await waitPending(page, "trump");
  await page.waitForTimeout(600);
  await sweep(page, "trump");

  // Their first lead is a suit we can't follow, if they hold one.
  const lead = leadWhatThePageLacks(page);
  table.right.onLead = table.partner.onLead = table.left.onLead = lead;
  await page.evaluate(() => window.__game.trump("spades"));
  await page.waitForFunction(() => window.__game.state().hand.length === 8, null, { timeout: 15_000 });
  await waitPending(page, "play");
  await page.waitForFunction(() => window.__game.playedCardBounds().length === 3, null, { timeout: 15_000 });
  await settledCards(page);
  await page.waitForFunction(() => !document.getElementById("table-moment").classList.contains("visible"), null, { timeout: 8000 }).catch(() => {});
  await page.waitForTimeout(500);
  await sweep(page, "play");

  // The popovers, opened with a finger.
  const toggles = [
    ["emote-strip", "#emote-button", "#emote-strip"],
    ["arrange", "#arrange-button", "#arrange-panel"],
    ["menu", "#menu-button", "#menu-panel"],
  ];
  await page.waitForSelector("#arrange-button:not(.hidden)", { timeout: 10_000 });
  for (const [state, button, panel] of toggles) {
    await sweep(page, state, {
      before: async () => {
        await fingerTap(page, button);
        await page.waitForTimeout(150);
        const open = await page.evaluate((query) => !document.querySelector(query).classList.contains("hidden"), panel);
        return open ? [] : [{ rule: "tap-failed", subject: `${button} did not open ${panel}` }];
      },
      after: () =>
        page.evaluate(() => {
          document.getElementById("menu-panel").classList.add("hidden");
          document.getElementById("emote-strip").classList.add("hidden");
          document.getElementById("arrange-panel").classList.add("hidden");
        }),
    });
  }

  // How to play, from the menu, over the table.
  await sweep(page, "help-in-match", {
    before: () => page.evaluate(() => document.getElementById("menu-help").click()),
    after: () =>
      page.evaluate(() => {
        document.getElementById("help-close").click();
        document.getElementById("menu-panel").classList.add("hidden");
      }),
  });

  // A bubble at each seat, all at once, sent by the real players. Those left over
  // from the last size were placed for that size, so they don't count.
  const clearBubbles = () =>
    page.evaluate(() => document.querySelectorAll(".emote-bubble").forEach((bubble) => bubble.remove()));
  await sweep(page, "emote-bubbles", {
    settle: 2000, // the server allows one emote per player per 1.5s, and the client a little less
    before: async () => {
      await clearBubbles();
      await page.evaluate(() => window.__game.emote("laugh"));
      for (const player of table.all) player.send(`(emote "${player.email}" fire)`);
      await page.waitForFunction(() => document.querySelectorAll(".emote-bubble").length >= 4, null, { timeout: 3000 }).catch(() => {});
      await page.waitForTimeout(280);
      const bubbles = await page.evaluate(() => {
        const centre = (el) => {
          const rect = el.getBoundingClientRect();
          return { x: (rect.left + rect.right) / 2, y: (rect.top + rect.bottom) / 2 };
        };
        return {
          count: document.querySelectorAll(".emote-bubble").length,
          found: [...document.querySelectorAll(".emote-bubble")].map(centre),
          seats: [1, 2, 3].map((seat) => centre(document.querySelector(`#seat-label-${seat} .seat-marker`))),
        };
      });
      const extra = [];
      if (bubbles.count < 4) extra.push({ rule: "bubble-missing", subject: `${bubbles.count} of 4` });
      for (const [index, seat] of bubbles.seats.entries()) {
        const near = bubbles.found.some((bubble) => Math.hypot(bubble.x - seat.x, bubble.y - seat.y) < 40);
        if (!near) extra.push({ rule: "bubble-detached", subject: `seat ${index + 1}`, seat, bubbles: bubbles.found });
      }
      return extra;
    },
    after: clearBubbles,
  });

  // Messages over the table.
  await sweep(page, "moment-over-play", {
    before: () => showMoment(page, "Final four cards", "Your full hand is ready"),
    after: () => hideMoment(page),
  });
  await sweep(page, "status-long", {
    before: () =>
      page.evaluate(() => {
        const line = document.getElementById("status-line");
        line.dataset.was = line.textContent;
        line.textContent = "bhagyalakshmi-reddy lost connection — holding their seat up to 45s…";
        line.classList.remove("your-turn");
        line.classList.add("visible");
      }),
    after: () =>
      page.evaluate(() => {
        const line = document.getElementById("status-line");
        line.textContent = line.dataset.was;
        if (line.textContent === "Your turn") line.classList.add("your-turn");
      }),
  });

  // If the first lead already has us void, measure the reveal before we play.
  const voidNow = () =>
    page.evaluate(() => !document.getElementById("expose-button").classList.contains("hidden"));
  let revealed = false;
  if (await voidNow()) {
    await sweep(page, "play-reveal");
    revealed = true;
  }

  // Hold our card back from the server: all four stay on the table.
  holdBack.on = true;
  const firstCard = await page.evaluate(() => window.__game.legalCards()[0]);
  await page.evaluate((id) => window.__game.play(id), firstCard);
  await page.waitForFunction(() => window.__game.playedCardBounds().length === 4, null, { timeout: 5000 });
  await settledCards(page);
  await page.waitForTimeout(400);
  await sweep(page, "play-full-trick");
  await sweep(page, "trick-outcome", {
    before: () =>
      page.evaluate(() => {
        const pill = document.getElementById("trick-outcome");
        pill.textContent = "bhagyalakshmi-reddy takes the trick · +7";
        pill.classList.add("visible");
      }),
    after: () =>
      page.evaluate(() => document.getElementById("trick-outcome").classList.remove("visible")),
  });
  holdBack.on = false;
  holdBack.frames.splice(0).forEach((frame) => holdBack.server.send(frame));

  // Play on; reveal the first time we can't follow, so the whole hand gets played.
  const deadline = Date.now() + 240_000;
  for (;;) {
    if (Date.now() > deadline) throw new Error("the hand never finished");
    const now = await page.evaluate(() => {
      const model = window.__game.state();
      return {
        phase: model.phase,
        pending: model.pendingRequest?.kind ?? null,
        legal: window.__game.legalCards(),
        canReveal: !document.getElementById("expose-button").classList.contains("hidden"),
      };
    });
    if (now.phase === "hand-finished") break;
    if (now.pending === "play" && now.legal[0]) {
      if (now.canReveal && !revealed) {
        await settledCards(page);
        await page.waitForTimeout(300);
        await sweep(page, "play-reveal");
        revealed = true;
      }
      await page.evaluate((id) => window.__game.play(id), now.legal[0]);
    }
    await page.waitForTimeout(150);
  }
  if (!revealed) console.log("play-reveal: this deal never left us void; state not measured");

  await page.waitForSelector("#result-panel:not(.hidden)", { timeout: 15_000 });
  await page.waitForTimeout(1500);
  await sweep(page, "result");

  // A player walks out between hands: the gate opens over the result.
  table.left.send(`(leave-game "${table.left.email}")`);
  await page.waitForSelector("#abandon-panel:not(.hidden)", { timeout: 15_000 });
  await sweep(page, "abandon-over-result");
  await page.evaluate(() => window.__game.resolveAbandon("replace"));
  await page.waitForSelector("#abandon-panel", { state: "hidden", timeout: 15_000 });
  await page.waitForTimeout(300);

  // The next hand: another player walks out mid-play this time.
  table.right.onBid = openOrPass;
  table.partner.onBid = openOrPass;
  table.left.onBid = openOrPass;
  table.right.onLead = (hand) => hand[0];
  table.partner.onLead = (hand) => hand[0];
  table.left.onLead = (hand) => hand[0];
  table.all.forEach((player) => {
    player.hand = [];
  });
  await page.evaluate(() => window.__game.nextHand());
  await page.waitForFunction(
    () => window.__game.state().phase !== "hand-finished" && window.__game.state().hand.length > 0,
    null,
    { timeout: 15_000 }
  );
  const walkDeadline = Date.now() + 90_000;
  for (;;) {
    if (Date.now() > walkDeadline) throw new Error("hand two never reached play");
    const now = await page.evaluate(() => {
      const model = window.__game.state();
      return { pending: model.pendingRequest?.kind ?? null, legal: window.__game.legalCards() };
    });
    if (now.pending === "bid") {
      const opening = await page.evaluate(() => window.__game.state().bidsPlaced === 0);
      await page.evaluate((open) => (open ? window.__game.bid(16) : window.__game.pass()), opening);
    } else if (now.pending === "trump") {
      await page.evaluate(() => window.__game.trump("hearts"));
    } else if (now.pending === "play") {
      break;
    }
    await page.waitForTimeout(150);
  }
  await settledCards(page);
  table.partner.send(`(leave-game "${table.partner.email}")`);
  await page.waitForSelector("#abandon-panel:not(.hidden)", { timeout: 15_000 });
  await sweep(page, "abandon-over-play");
  await page.evaluate(() => window.__game.resolveAbandon("close"));
  await page.waitForSelector("#start-screen:not(.hidden)", { timeout: 15_000 }).catch(() => {});
  await context.close();
  table.all.forEach((player) => player.close());
}

// A guest's view: the lobby without host controls, then the seat that opens.
async function walkOpening() {
  const port = portOf(PAGE_URL);
  const host = await open("padmanabhan iyer", port);
  const room = `Padmanabhan's table #p${uid().slice(0, 2)}`;
  host.send(`(make-room "${host.email}" ${JSON.stringify(room)} 6)`);
  await new Promise((resolve) => setTimeout(resolve, 200));
  const guests = [await open("venkatasubramanian", port), await open("bhagyalakshmi reddy", port)];
  for (const guest of guests) {
    guest.send(`(join-room ${JSON.stringify(room)} "${guest.email}")`);
    await new Promise((resolve) => setTimeout(resolve, 150));
  }
  const { context, page } = await newTouchPage();
  const url = new globalThis.URL(PAGE_URL);
  url.searchParams.set("join", room);
  await page.goto(url.toString());
  await page.waitForSelector("#join-link:not(.hidden)", { timeout: 20_000 });
  await page.fill("#player-name", "Asha");
  await page.evaluate(() => {
    document.activeElement?.blur();
    document.getElementById("join-link").click();
  });
  await page.waitForSelector("#lobby-panel:not(.hidden)", { timeout: 15_000 });
  await page.waitForFunction(() => window.__game.state().members.length === 4, null, { timeout: 10_000 });
  await sweep(page, "lobby-guest");
  host.send(`(start-game ${JSON.stringify(room)})`);
  await waitPending(page, "bid");
  await page.waitForFunction(() => !document.getElementById("table-moment").classList.contains("visible"), null, { timeout: 8000 }).catch(() => {});
  await page.waitForTimeout(400);
  await sweep(page, "bid-open");
  await sweep(page, "bid-open-moment", {
    before: () => showMoment(page, "First four cards", "Read your opening hand"),
    after: () => hideMoment(page),
  });
  await page.evaluate(() => window.__game.leaveMatch());
  await context.close();
  host.close();
  guests.forEach((guest) => guest.close());
}

// What most people see: three bots at the table, nothing unusual about the names.
async function walkBots() {
  const { context, page } = await newTouchPage();
  let starts = 0;
  page.on("websocket", (socket) => {
    socket.on("framesent", ({ payload }) => {
      if (String(payload).includes("(start-game")) starts++;
    });
  });
  await page.goto(PAGE_URL);
  await page.waitForSelector("#create-button", { state: "visible", timeout: 20_000 });
  await page.fill("#player-name", "Asha");
  await page.evaluate(() => {
    document.activeElement?.blur();
    document.getElementById("play-bots").click();
  });
  await page.waitForFunction(() => window.__game.state().seatNames !== null, null, { timeout: 25_000 });
  await waitPending(page, "bid");
  await page.waitForFunction(() => !document.getElementById("table-moment").classList.contains("visible"), null, { timeout: 8000 }).catch(() => {});
  await page.waitForTimeout(400);
  // A second start only earns "This game has already started." on the player's screen.
  if (starts !== 1) throw new Error(`Play with bots asked for ${starts} starts`);
  await sweep(page, "bots-bid");
  await passUntilPlay(page);
  await settledCards(page);
  await page.waitForFunction(() => !document.getElementById("table-moment").classList.contains("visible"), null, { timeout: 8000 }).catch(() => {});
  await page.waitForTimeout(600);
  await sweep(page, "bots-play");
  await page.evaluate(() => window.__game.leaveMatch());
  await context.close();
}

// The match-over panel, on a server whose first hand ends the match.
async function walkOver() {
  if (!OVER_URL) {
    console.log("match-over: no MATCH-TARGET=0 server URL given; state not measured");
    return;
  }
  const port = portOf(OVER_URL);
  const { context, page } = await newTouchPage();
  await page.goto(OVER_URL);
  await page.waitForSelector("#create-button", { state: "visible", timeout: 20_000 });
  await page.fill("#player-name", "Asha");
  await page.evaluate(() => {
    document.activeElement?.blur();
    document.getElementById("create-button").click();
  });
  await page.waitForSelector("#lobby-panel:not(.hidden)", { timeout: 15_000 });
  const table = await fillTable(page, port);
  await page.evaluate(() => document.getElementById("start-game").click());
  const deadline = Date.now() + 240_000;
  for (;;) {
    if (Date.now() > deadline) throw new Error("the match never finished");
    const now = await page.evaluate(() => {
      const model = window.__game.state();
      return { phase: model.phase, pending: model.pendingRequest?.kind ?? null, legal: window.__game.legalCards() };
    });
    if (now.phase === "finished") break;
    if (now.pending === "bid") await page.evaluate(() => window.__game.pass());
    else if (now.pending === "play" && now.legal[0]) await page.evaluate((id) => window.__game.play(id), now.legal[0]);
    await page.waitForTimeout(150);
  }
  await page.waitForSelector("#go-home:not(.hidden)", { timeout: 15_000 });
  await page.waitForTimeout(2500);
  await sweep(page, "match-over");
  // A long match leaves a chip for every hand.
  await page.evaluate(() => {
    const row = document.getElementById("result-hands");
    row.replaceChildren(
      ...[1, 2, -2, 1, 1, -4, 2, 2, 1, -2, 1, 2, 1, -2, 1, 2].map((swing) => {
        const chip = document.createElement("span");
        chip.className = `hand-chip ${swing > 0 ? "gain" : "loss"}`;
        chip.textContent = swing > 0 ? `+${swing}` : `−${-swing}`;
        return chip;
      })
    );
    row.classList.remove("hidden");
    document.getElementById("result-note").textContent = "Set: bhagyalakshmi-reddy bid 24 and took 19";
    document.getElementById("result-detail").textContent = "first to 6 · 16 hands played";
  });
  await sweep(page, "match-over-long");
  await context.close();
  table.all.forEach((player) => player.close());
}

// A page load landing in the middle of a match, at each size: nothing here gets
// to rely on a resize having happened.
async function walkReload() {
  const port = portOf(PAGE_URL);
  const { context, page } = await newTouchPage();
  await page.goto(PAGE_URL);
  await page.waitForSelector("#create-button", { state: "visible", timeout: 20_000 });
  await page.fill("#player-name", "Asha");
  await page.evaluate(() => {
    document.activeElement?.blur();
    document.getElementById("create-button").click();
  });
  await page.waitForSelector("#lobby-panel:not(.hidden)", { timeout: 15_000 });
  const table = await fillTable(page, port);
  await page.evaluate(() => document.getElementById("start-game").click());
  await waitPending(page, "bid");
  // A prompt that comes back with the page has to make its room as well.
  console.log("bid-after-reload");
  for (const size of SIZES) {
    await page.setViewportSize(size);
    await page.reload();
    await waitPending(page, "bid", 30_000);
    await page.waitForFunction(() => !document.getElementById("table-moment").classList.contains("visible"), null, { timeout: 8000 }).catch(() => {});
    await page.waitForTimeout(500);
    await record(page, "bid-after-reload", size);
  }
  await passUntilPlay(page);
  console.log("play-after-reload");
  for (const size of SIZES) {
    await page.setViewportSize(size);
    await page.reload();
    await waitPending(page, "play", 30_000);
    await page.waitForFunction(() => window.__game.playedCardBounds().length === 3, null, { timeout: 15_000 }).catch(() => {});
    await settledCards(page);
    await page.waitForFunction(() => !document.getElementById("table-moment").classList.contains("visible"), null, { timeout: 8000 }).catch(() => {});
    await page.waitForTimeout(500);
    await record(page, "play-after-reload", size);
  }
  await page.evaluate(() => window.__game.leaveMatch());
  await context.close();
  table.all.forEach((player) => player.close());
}

// ------------------------------------------------------------- contact sheets

async function writeSheets() {
  const sheetPage = await browser.newPage({ viewport: { width: 1900, height: 1000 } });
  const links = [];
  for (const [state, tiles] of statesSeen) {
    const tile = ({ size, shot, faults: tileFaults }) => {
      const scale = Math.min(1, 560 / size.height, 452 / size.width);
      const rules = [...new Set(tileFaults.map((item) => item.rule))];
      return `<figure class="${rules.length ? "bad" : ""}">
        <figcaption><b>${sizeName(size)}</b> ${rules.length ? `${tileFaults.length}: ${rules.join(", ")}` : "clean"}</figcaption>
        <img src="${shot}" width="${Math.round(size.width * scale)}" height="${Math.round(size.height * scale)}">
      </figure>`;
    };
    const html = `<!doctype html><meta charset="utf-8"><title>${state}</title><style>
      body { margin: 10px; font: 12px/1.3 system-ui, sans-serif; background: #333; color: #eee; }
      h1 { font-size: 18px; margin: 0 0 8px; }
      main { display: flex; flex-wrap: wrap; gap: 10px; align-items: flex-start; }
      figure { margin: 0; padding: 4px; background: #555; max-width: 460px; }
      figure.bad { background: #8d2c20; }
      figcaption { margin-bottom: 3px; max-width: 100%; }
      img { display: block; background: #fff; }
    </style><h1>${state}</h1><main>${tiles.map(tile).join("")}</main>`;
    const htmlName = `sheet-${state}.html`;
    writeFileSync(join(OUT, htmlName), html);
    await sheetPage.goto(`file://${join(OUT, htmlName)}`);
    await sheetPage.screenshot({ path: join(OUT, `sheet-${state}.png`), fullPage: true });
    links.push(state);
  }
  writeFileSync(
    join(OUT, "index.html"),
    `<!doctype html><meta charset="utf-8"><title>layout matrix</title>${links
      .map((state) => `<p><a href="sheet-${state}.html">${state}</a></p>`)
      .join("")}`
  );
  await sheetPage.close();
}

// ------------------------------------------------------------------- the run

let walkFailed = null;
try {
  if (wanted("start")) await walkStart();
  if (wanted("browser")) await walkBrowser();
  if (wanted("reconnect")) await walkReconnect();
  if (wanted("lobby") || wanted("match")) await walkMatch();
  if (wanted("opening")) await walkOpening();
  if (wanted("bots")) await walkBots();
  if (wanted("over")) await walkOver();
  if (wanted("reload")) await walkReload();
} catch (error) {
  walkFailed = error;
  console.log(`walk stopped: ${error.stack ?? error}`);
}

await writeSheets();
writeFileSync(
  join(OUT, "report.json"),
  JSON.stringify({ faults, consoleErrors, states: Object.fromEntries(statesSeen) }, null, 1)
);
await browser.close();

const summary = new Map();
for (const item of faults) {
  const key = `${item.rule} | ${item.state} | ${item.subject}`;
  if (!summary.has(key)) summary.set(key, { sizes: [], sample: item });
  summary.get(key).sizes.push(item.size);
}
const lines = [...summary.entries()].sort().map(
  ([key, { sizes, sample }]) => {
    const { state, size, rule, subject, ...detail } = sample;
    return `${key}\n    ${sizes.join(" ")}\n    e.g. ${JSON.stringify(detail)}`;
  }
);
writeFileSync(join(OUT, "faults.txt"), lines.join("\n") + "\n");

console.log(`\n${faults.length} fault(s) across ${statesSeen.size} state(s); ${summary.size} distinct`);
console.log(`report: ${join(OUT, "faults.txt")}`);
console.log(`sheets: ${join(OUT, "index.html")}`);
if (consoleErrors.length) {
  console.log("CONSOLE ERRORS:");
  consoleErrors.forEach((error) => console.log("  " + error));
}
if (walkFailed || faults.length || consoleErrors.length) {
  console.log("\nlayout matrix failed");
  process.exit(1);
}
console.log("\nall layout matrix checks passed");
