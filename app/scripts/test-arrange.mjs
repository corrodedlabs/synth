// Verifies that card arrangement unlocks only after all eight cards arrive,
// that presets affect only visual order, and that Custom drags never play.
// Usage: HEADED=1 node scripts/test-arrange.mjs [url]
// Screenshots land in /tmp/game-shots-arrange.
import { chromium } from "playwright";
import { existsSync, mkdirSync } from "node:fs";

const URL = process.argv[2] ?? "http://localhost:5179/?port=8082";
const SHOTS = "/tmp/game-shots-arrange";
mkdirSync(SHOTS, { recursive: true });

const browser = await chromium.launch({
  ...(existsSync("/usr/bin/chromium")
    ? { executablePath: "/usr/bin/chromium" }
    : { channel: "chrome" }),
  headless: !process.env.HEADED,
  args: ["--enable-unsafe-swiftshader", "--use-angle=swiftshader"],
});
const page = await browser.newPage({ viewport: { width: 1280, height: 800 } });
const errors = [];
page.on("console", (message) => {
  if (message.type() === "error") errors.push(message.text());
});
page.on("pageerror", (error) => errors.push(String(error)));

const arrangement = () => page.evaluate(() => window.__game.arrangement());
const modelHand = () => page.evaluate(() => window.__game.state().hand.map((card) => card.id));

await page.goto(URL);
await page.waitForSelector("#create-button", { state: "visible", timeout: 15_000 });
await page.fill("#player-name", "Arranger");
await page.click("#create-button");
await page.click('#create-panel .table-choice[data-target="6"]');
await page.waitForSelector("#lobby-panel:not(.hidden)", { timeout: 15_000 });
for (let bots = 0; bots < 3; bots += 1) {
  await page.click("#add-bot");
  await page.waitForFunction((count) => window.__game.state().members.length === count, bots + 2);
}
await page.click("#start-game");

await page.waitForFunction(() => window.__game.state().hand.length === 4, undefined, {
  timeout: 15_000,
});
if (!(await page.locator("#arrange-button").evaluate((button) => button.classList.contains("hidden")))) {
  throw new Error("Arrange became visible during the first four-card deal");
}

const fullHandDeadline = Date.now() + 90_000;
while ((await modelHand()).length < 8 && Date.now() < fullHandDeadline) {
  const state = await page.evaluate(() => window.__game.state());
  if (state.pendingRequest?.kind === "bid") {
    if (state.bidsPlaced === 0) await page.evaluate((bid) => window.__game.bid(bid), state.pendingRequest.minBid);
    else await page.evaluate(() => window.__game.pass());
  } else if (state.pendingRequest?.kind === "trump") {
    await page.evaluate(() => window.__game.trump("hearts"));
  }
  await page.waitForTimeout(100);
}
if ((await modelHand()).length !== 8) throw new Error("The full eight-card hand never arrived");

await page.waitForFunction(() => window.__game.arrangement().available, undefined, {
  timeout: 10_000,
});
await page.waitForSelector("#arrange-button:not(.hidden)");
await page.screenshot({ path: `${SHOTS}/01-arrange-ready.png` });

const authoritativeBefore = await modelHand();
await page.click("#arrange-button");
await page.click('#arrange-panel [data-arrangement-preset="suit"]');
const suitOrder = await arrangement();
if (suitOrder.mode !== "suit" || suitOrder.arranging) {
  throw new Error(`Suit preset state is wrong: ${JSON.stringify(suitOrder)}`);
}
if (JSON.stringify(await modelHand()) !== JSON.stringify(authoritativeBefore)) {
  throw new Error("A visual preset changed the authoritative model hand");
}

const suitAndRank = await page.evaluate(() => {
  const model = window.__game.state();
  const byId = new Map(model.hand.map((card) => [card.id, card]));
  return window.__game.arrangement().order.map((id) => byId.get(id));
});
const suits = ["spades", "hearts", "clubs", "diamonds"];
const ranks = ["J", "9", "A", "10", "K", "Q", "8", "7"];
for (let index = 1; index < suitAndRank.length; index += 1) {
  const previous = suitAndRank[index - 1];
  const current = suitAndRank[index];
  const previousKey = suits.indexOf(previous.suit) * 10 + ranks.indexOf(previous.rank);
  const currentKey = suits.indexOf(current.suit) * 10 + ranks.indexOf(current.rank);
  if (currentKey < previousKey) throw new Error("Suit preset produced an incorrect visual order");
}

await page.click("#arrange-button");
await page.click("#arrange-custom");
const customBefore = await arrangement();
const movedId = customBefore.order.at(-1);
const leftId = customBefore.order[0];
const positions = await page.evaluate(
  ({ moved, left }) => ({
    moved: window.__game.handCardBounds().find((card) => card.id === moved),
    left: window.__game.handCardBounds().find((card) => card.id === left),
  }),
  { moved: movedId, left: leftId }
);
if (!positions.moved || !positions.left) throw new Error("Could not locate cards for Custom drag");
await page.mouse.move(positions.moved.right - 8, (positions.moved.top + positions.moved.bottom) / 2);
await page.mouse.down();
await page.mouse.move(positions.left.left + 8, (positions.left.top + positions.left.bottom) / 2, { steps: 14 });
await page.mouse.up();
await page.waitForTimeout(500);
const customAfter = await arrangement();
if (customAfter.order[0] !== movedId) {
  throw new Error(`Custom drag did not move ${movedId} to the first slot: ${customAfter.order}`);
}
if (JSON.stringify(await modelHand()) !== JSON.stringify(authoritativeBefore)) {
  throw new Error("Custom arrangement changed or played an authoritative card");
}

// Even a forward drag remains an arrangement gesture until Done is pressed.
const forwardId = customAfter.order.at(-1);
const forwardPosition = await page.evaluate((id) => window.__game.cardScreenPos(id), forwardId);
if (!forwardPosition) throw new Error("Could not locate a card for the no-play check");
await page.mouse.move(forwardPosition.x, forwardPosition.y);
await page.mouse.down();
await page.mouse.move(640, 360, { steps: 12 });
await page.mouse.up();
await page.waitForTimeout(400);
if (JSON.stringify(await modelHand()) !== JSON.stringify(authoritativeBefore)) {
  throw new Error("A forward drag played a card while Custom mode was active");
}

await page.click("#arrange-button");
if ((await arrangement()).arranging) throw new Error("Done did not leave Custom mode");

const savedCustomOrder = (await arrangement()).order;
await page.reload();
await page.waitForFunction(() => window.__game.arrangement().available, undefined, {
  timeout: 20_000,
});
const restoredHand = await modelHand();
const restoredOrder = (await arrangement()).order;
if (
  JSON.stringify(restoredOrder) !==
  JSON.stringify(savedCustomOrder.filter((id) => restoredHand.includes(id)))
) {
  throw new Error(
    `Reconnect did not restore Custom order: ${JSON.stringify({ savedCustomOrder, restoredOrder })}`
  );
}

await page.waitForFunction(
  () => window.__game.state().pendingRequest?.kind === "play",
  undefined,
  { timeout: 30_000 }
);
if (!(await page.locator("#arrange-button").isVisible())) {
  throw new Error("Arrange is not available during the player's own turn");
}

const beforePlay = (await arrangement()).order;
const legal = await page.evaluate(() => window.__game.legalCards()[0]);
await page.evaluate((cardId) => window.__game.play(cardId), legal);
await page.waitForFunction((cardId) => !window.__game.state().hand.some((card) => card.id === cardId), legal);
const afterPlay = (await arrangement()).order;
if (JSON.stringify(afterPlay) !== JSON.stringify(beforePlay.filter((id) => id !== legal))) {
  throw new Error("Remaining cards changed relative order after a play");
}

await page.setViewportSize({ width: 390, height: 844 });
await page.waitForTimeout(500);
const mobile = await page.evaluate(() => {
  const button = document.getElementById("arrange-button").getBoundingClientRect();
  return {
    withinViewport: button.left >= 0 && button.right <= innerWidth && button.top >= 0 && button.bottom <= innerHeight,
  };
});
if (!mobile.withinViewport) {
  throw new Error(`Arrange control does not fit the phone viewport: ${JSON.stringify(mobile)}`);
}
await page.screenshot({ path: `${SHOTS}/02-arrange-mobile.png` });

// A separate touch-emulated mock table verifies that Custom ordering uses
// the same safe mode on phones, without depending on mouse compatibility.
const touchPage = await browser.newPage({
  viewport: { width: 390, height: 844 },
  isMobile: true,
  hasTouch: true,
});
touchPage.on("console", (message) => {
  if (message.type() === "error") errors.push(`touch: ${message.text()}`);
});
touchPage.on("pageerror", (error) => errors.push(`touch: ${String(error)}`));
await touchPage.goto(`${URL}${URL.includes("?") ? "&" : "?"}mock=1`);
await touchPage.waitForFunction(() => window.__game.arrangement().available, undefined, {
  timeout: 10_000,
});
await touchPage.click("#arrange-button");
await touchPage.click("#arrange-custom");
const touchBefore = await touchPage.evaluate(() => window.__game.arrangement());
const touchModelBefore = await touchPage.evaluate(() => window.__game.state().hand.map((card) => card.id));
const touchMovedId = touchBefore.order.at(-1);
const touchLeftId = touchBefore.order[0];
const touchPositions = await touchPage.evaluate(
  ({ moved, left }) => ({
    moved: window.__game.handCardBounds().find((card) => card.id === moved),
    left: window.__game.handCardBounds().find((card) => card.id === left),
  }),
  { moved: touchMovedId, left: touchLeftId }
);
if (!touchPositions.moved || !touchPositions.left) {
  throw new Error("Could not locate cards for the touch Custom drag");
}
await touchPage.evaluate(({ from, to }) => {
  const canvas = document.querySelector("canvas");
  if (!(canvas instanceof HTMLCanvasElement)) throw new Error("Missing game canvas");
  const dispatch = (type, x, y, active) => {
    const touch = new Touch({
      identifier: 1,
      target: canvas,
      clientX: x,
      clientY: y,
      pageX: x,
      pageY: y,
    });
    canvas.dispatchEvent(
      new TouchEvent(type, {
        bubbles: true,
        cancelable: true,
        touches: active ? [touch] : [],
        targetTouches: active ? [touch] : [],
        changedTouches: [touch],
      })
    );
  };
  dispatch("touchstart", from.x, from.y, true);
  for (let step = 1; step <= 14; step += 1) {
    const progress = step / 14;
    dispatch(
      "touchmove",
      from.x + (to.x - from.x) * progress,
      from.y + (to.y - from.y) * progress,
      true
    );
  }
  dispatch("touchend", to.x, to.y, false);
}, {
  from: {
    x: touchPositions.moved.right - 8,
    y: (touchPositions.moved.top + touchPositions.moved.bottom) / 2,
  },
  to: {
    x: touchPositions.left.left + 8,
    y: (touchPositions.left.top + touchPositions.left.bottom) / 2,
  },
});
await touchPage.waitForTimeout(500);
const touchAfter = await touchPage.evaluate(() => window.__game.arrangement());
if (touchAfter.order[0] !== touchMovedId) {
  throw new Error(`Touch Custom drag did not move ${touchMovedId}: ${touchAfter.order}`);
}
if (
  JSON.stringify(await touchPage.evaluate(() => window.__game.state().hand.map((card) => card.id))) !==
  JSON.stringify(touchModelBefore)
) {
  throw new Error("Touch Custom arrangement changed the authoritative hand");
}
await touchPage.screenshot({ path: `${SHOTS}/03-arrange-touch.png` });
await touchPage.close();

await browser.close();
if (errors.length > 0) throw new Error(`Browser errors:\n${errors.join("\n")}`);
console.log("card arrangement browser test passed");
