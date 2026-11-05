// Mobile pass: runs the full game on a phone-sized touch viewport (390×844),
// verifying the hand fan, panels, and overlays stay inside the screen and
// that cards play via real taps. Usage: node scripts/test-mobile.mjs [url]
// Screenshots land in /tmp/game-shots-mobile.
import { chromium } from "playwright";
import { existsSync, mkdirSync } from "node:fs";

const URL = process.argv[2] ?? "http://localhost:5173/";
const SHOTS = "/tmp/game-shots-mobile";
mkdirSync(SHOTS, { recursive: true });

const VIEW = { width: 390, height: 844 };

// HEADED=1 opens a visible browser window.
const browser = await chromium.launch({
  ...(existsSync("/usr/bin/chromium")
    ? { executablePath: "/usr/bin/chromium" }
    : { channel: "chrome" }),
  headless: !process.env.HEADED,
  args: ["--enable-unsafe-swiftshader", "--use-angle=swiftshader"],
});
const context = await browser.newContext({
  viewport: VIEW,
  hasTouch: true,
  isMobile: true,
  deviceScaleFactor: 2,
  userAgent:
    "Mozilla/5.0 (iPhone; CPU iPhone OS 17_0 like Mac OS X) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/17.0 Mobile/15E148 Safari/604.1",
});
const page = await context.newPage();

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
      played: model.playedCards.length,
      legal: window.__game.legalCards(),
      points: model.points,
    };
  });

// every hand card's projected position must be inside the viewport
const checkHandFits = async (label) => {
  const { hand } = await state();
  const positions = await page.evaluate(
    (ids) => ids.map((id) => window.__game.cardScreenPos(id)),
    hand
  );
  const margin = 12;
  const inside = positions.every(
    (p) => p && p.x > margin && p.x < VIEW.width - margin && p.y > 0 && p.y < VIEW.height
  );
  check(`${label}: all ${hand.length} hand cards on screen`, inside);
  if (!inside) console.log("  positions:", JSON.stringify(positions));
};

// a visible element must sit fully inside the viewport
const checkFits = async (selector, label) => {
  const box = await page.locator(selector).boundingBox();
  const ok =
    box !== null &&
    box.x >= 0 &&
    box.y >= 0 &&
    box.x + box.width <= VIEW.width + 1 &&
    box.y + box.height <= VIEW.height + 1;
  check(`${label} fits viewport`, ok);
  if (!ok && box) console.log("  box:", JSON.stringify(box));
};

console.log(`navigating to ${URL} at ${VIEW.width}×${VIEW.height}`);
await page.goto(URL);
await page.waitForSelector("#create-button", { state: "visible", timeout: 15000 });
await shot("01-start");
await checkFits("#start-actions", "start actions");

// help overlay on a phone
await page.tap("#help-button");
await checkFits("#help-panel", "help panel");
await shot("02-help");
await page.tap("#help-close");

// lobby
await page.fill("#player-name", "Phone");
await page.tap('.mode-card[data-target="6"]');
await page.tap("#create-button");
await page.waitForSelector("#lobby-panel:not(.hidden)", { timeout: 15000 });
await checkFits("#lobby-panel", "lobby panel");
for (let bots = 1; bots <= 3; bots++) {
  await page.tap("#add-bot");
  await page.waitForFunction(
    (expected) => window.__game.state().members.length === expected,
    bots + 1,
    { timeout: 10000 }
  );
}
await shot("03-lobby");
const lobbyBots = await page.locator("#seat-list .bot-player").evaluateAll((players) =>
  players.map((player) => ({
    name: player.querySelector(".player-display-name")?.textContent ?? "",
    avatar: player.querySelector(".player-avatar")?.textContent ?? "",
  }))
);
check(
  `three distinct mobile bot personas (${JSON.stringify(lobbyBots)})`,
  lobbyBots.length === 3 &&
    new Set(lobbyBots.map((bot) => bot.name)).size === 3 &&
    new Set(lobbyBots.map((bot) => bot.avatar)).size === 3 &&
    lobbyBots.every((bot) => !/^Bot \d+$/.test(bot.name))
);
await page.tap("#start-game");
await page.waitForSelector(".seat-name-label.visible .seat-marker");
const seatPresentation = await page.evaluate(() =>
  [...document.querySelectorAll(".seat-name-label.visible")].map((seat) => {
    const marker = seat.querySelector(".seat-marker");
    const avatar = seat.querySelector(".player-avatar");
    if (!(marker instanceof HTMLElement) || !(avatar instanceof HTMLElement)) return null;
    const markerRect = marker.getBoundingClientRect();
    const avatarRect = avatar.getBoundingClientRect();
    const avatarStyle = getComputedStyle(avatar);
    return {
      markerFits:
        markerRect.left >= 0 && markerRect.top >= 0 &&
        markerRect.right <= innerWidth && markerRect.bottom <= innerHeight,
      avatarSmaller:
        avatarRect.width < markerRect.width * 0.65 && avatarRect.height < markerRect.height * 0.65,
      avatarBorderless:
        avatarStyle.borderTopWidth === "0px" && avatarStyle.backgroundColor === "rgba(0, 0, 0, 0)",
    };
  })
);
check(
  "seat circles fit and avatars are smaller, transparent, and borderless",
  seatPresentation.length === 3 &&
    seatPresentation.every((seat) =>
      seat?.markerFits && seat.avatarSmaller && seat.avatarBorderless
    )
);

// play the hand: bid panel, then tap cards to play (two-step on touch)
let tappedPlays = 0;
let shotHand = false;
let shotTrick = false;
let checkedDimming = false;
let checkedArming = false;
let checkedDealMoment = false;
let checkedSeatAction = false;
let checkedSeatClearance = false;
let checkedTrickOutcome = false;
const deadline = Date.now() + 180_000;
while (Date.now() < deadline) {
  const snapshot = await state();
  if (snapshot.phase === "hand-finished") break;

  const presentation = await page.evaluate(() => {
    const bidAction = document.querySelector(".seat-action.visible");
    return {
      dealMoment: document.getElementById("table-moment")?.classList.contains("visible") ?? false,
      bidActionId: bidAction?.id ?? null,
      bidActionAttached:
        bidAction?.id === "seat-action-0" || bidAction?.closest(".seat-name-label") !== null,
      trickOutcome: document.getElementById("trick-outcome")?.classList.contains("visible") ?? false,
    };
  });
  if (presentation.dealMoment && !checkedDealMoment) {
    await checkFits("#table-moment", "deal moment");
    const dealSeats = await page.evaluate(() => {
      const panel = document.getElementById("table-moment")?.getBoundingClientRect();
      const overlaps = (first, second) =>
        first && second &&
        first.left < second.right && first.right > second.left &&
        first.top < second.bottom && first.bottom > second.top;
      return [1, 2, 3].map((player) => {
        const root = document.getElementById(`seat-label-${player}`);
        const parts = [
          root?.querySelector(".seat-marker"),
          root?.querySelector(".player-copy"),
          root?.querySelector(".partner-tag"),
        ].filter((part) => part instanceof HTMLElement);
        return {
          visible: root instanceof HTMLElement && Number.parseFloat(getComputedStyle(root).opacity) >= 0.5,
          collision: parts.some((part) => overlaps(panel, part.getBoundingClientRect())),
        };
      });
    });
    check(
      "deal moment keeps all player seats visible and clear",
      dealSeats.every((seat) => seat.visible && !seat.collision),
    );
    checkedDealMoment = true;
  }
  if (presentation.bidActionId && !checkedSeatAction) {
    await checkFits(`#${presentation.bidActionId}`, "seat bid action");
    check("bid action is attached to a player seat", presentation.bidActionAttached);
    checkedSeatAction = true;
  }
  if (presentation.trickOutcome && !checkedTrickOutcome) {
    await checkFits("#trick-outcome", "trick outcome");
    checkedTrickOutcome = true;
  }

  if (snapshot.played >= 2 && !checkedSeatClearance) {
    await page.waitForTimeout(600);
    const layout = await page.evaluate(() => ({
      markers: [...document.querySelectorAll(".seat-marker")].map((marker) => {
        const rect = marker.getBoundingClientRect();
        return { left: rect.left, top: rect.top, right: rect.right, bottom: rect.bottom };
      }),
      cards: window.__game.playedCardBounds(),
    }));
    const overlaps = layout.markers.some((marker) =>
      layout.cards.some((card) =>
        card.left < marker.right && card.right > marker.left &&
        card.top < marker.bottom && card.bottom > marker.top
      )
    );
    check("played cards stay clear of every seat circle", !overlaps);
    if (overlaps) console.log("  table layout:", JSON.stringify(layout));
    checkedSeatClearance = true;
  }

  if (snapshot.hand.length === 8 && !shotHand) {
    await page.waitForTimeout(700);
    await checkHandFits("full hand");
    await shot("04-hand");
    shotHand = true;
  }

  if (snapshot.pending === "bid") {
    await checkFits("#bid-panel", "bid panel");
    await page.tap("#bid-pass");
    await page.waitForTimeout(300);
    continue;
  }
  if (snapshot.pending === "trump") {
    await checkFits("#trump-panel", "trump panel");
    await page.tap('#trump-panel [data-suit="spades"]');
    await page.waitForTimeout(300);
    continue;
  }
  if (snapshot.pending === "play" && snapshot.legal[0]) {
    const exposable = await page.evaluate(() => {
      const button = document.getElementById("expose-button");
      return button && !button.classList.contains("hidden");
    });
    if (exposable) {
      await page.tap("#expose-button");
      await page.waitForTimeout(300);
      continue;
    }
    // unplayable cards must be dimmed while the server waits on us
    if (!checkedDimming) {
      const states = await page.evaluate(() => window.__game.cardStates());
      const wrong = states.filter(
        (s) => s.dimmed === snapshot.legal.includes(s.id)
      );
      check("illegal cards (and only those) are dimmed", wrong.length === 0);
      if (wrong.length) console.log("  mismatches:", JSON.stringify(wrong));
      checkedDimming = true;
    }
    // touch plays are two-step: the first tap arms the card…
    const cardId = snapshot.legal[0];
    // In a full fan the next card covers a card's centre, so tap the part a player can see.
    const pos = await page.evaluate((id) => window.__game.cardTapPoint(id), cardId);
    if (pos) {
      await page.touchscreen.tap(pos.x, pos.y);
      await page.waitForTimeout(400);
      const armed = await state();
      if (!checkedArming) {
        check("first tap arms the card instead of playing it", armed.hand.includes(cardId));
        checkedArming = true;
      }
      // …and a second tap on the raised card commits it
      const raised = await page.evaluate((id) => window.__game.cardTapPoint(id), cardId);
      if (raised) await page.touchscreen.tap(raised.x, raised.y);
      await page.waitForTimeout(500);
      const after = await state();
      const played = !after.hand.includes(cardId);
      if (played) tappedPlays++;
      else await page.evaluate((id) => window.__game.play(id), cardId); // fallback
      if (!shotTrick && after.hand.length <= 5) {
        await shot("05-mid-game");
        shotTrick = true;
      }
    }
    continue;
  }
  await page.waitForTimeout(300);
}

const final = await state();
check("hand finished on mobile", final.phase === "hand-finished");
check(`cards played by real taps (${tappedPlays}/8)`, tappedPlays >= 6);
await page.waitForSelector("#result-panel:not(.hidden)", { timeout: 10000 });
await checkFits("#result-panel", "result panel");
await checkFits("#result-delta", "match-point result chip");
check("deal moment appeared", checkedDealMoment);
check("seat bid action appeared", checkedSeatAction);
check("seat circles stayed clear of played cards", checkedSeatClearance);
check("trick outcome appeared", checkedTrickOutcome);
await shot("06-result");

if (errors.length) {
  console.log("CONSOLE ERRORS:");
  for (const error of errors) console.log("  " + error);
}
await browser.close();
if (failures.length || errors.length) {
  console.log(`\n${failures.length} check(s) failed`);
  process.exit(1);
}
console.log("\nall mobile checks passed");
process.exit(0);
