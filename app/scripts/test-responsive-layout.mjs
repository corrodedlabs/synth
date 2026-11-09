// Responsive table regression: freezes a real match with all three opponent
// cards visible, then verifies the same state across wide, short, compact,
// and phone viewports. Usage: node scripts/test-responsive-layout.mjs [url]
// Screenshots land in /tmp/game-shots-responsive.
import { chromium } from "playwright";
import { existsSync, mkdirSync } from "node:fs";

const URL = process.argv[2] ?? "http://localhost:5173/";
const SHOTS = "/tmp/game-shots-responsive";
mkdirSync(SHOTS, { recursive: true });

const browser = await chromium.launch({
  ...(existsSync("/usr/bin/chromium")
    ? { executablePath: "/usr/bin/chromium" }
    : { channel: "chrome" }),
  headless: !process.env.HEADED,
  // SwiftShader yields no WebGL context on macOS; Chrome's default GL works there.
  args: process.platform === "darwin" ? [] : ["--enable-unsafe-swiftshader", "--use-angle=swiftshader"],
});
const page = await browser.newPage({ viewport: { width: 1280, height: 800 } });
const failures = [];
const errors = [];
page.on("console", (message) => {
  if (message.type() === "error") errors.push(message.text());
});
page.on("pageerror", (error) => errors.push(String(error)));

const check = (label, ok, detail = null) => {
  console.log(`${ok ? "ok" : "FAIL"} — ${label}`);
  if (!ok) {
    failures.push(label);
    if (detail !== null) console.log("  " + JSON.stringify(detail));
  }
};

await page.goto(URL);
await page.waitForSelector("#create-button", { state: "visible", timeout: 15000 });
await page.fill("#player-name", `Responsive-${Date.now().toString(36).slice(-5)}`);
await page.click('.mode-card[data-target="6"]');
await page.click("#create-button");
await page.waitForSelector("#lobby-panel:not(.hidden)", { timeout: 15000 });
for (let count = 2; count <= 4; count++) {
  await page.click("#add-bot");
  await page.waitForFunction((expected) => window.__game.state().members.length === expected, count);
}

// The status line is drawn over the lobby, whose last row runs to the foot of a
// small screen: that row has to make room while a message is up.
for (const viewport of [
  { width: 280, height: 653 },
  { width: 320, height: 568 },
  { width: 360, height: 640 },
  { width: 568, height: 320 },
  { width: 640, height: 360 },
]) {
  await page.setViewportSize(viewport);
  const clashes = await page.evaluate(() => {
    const line = document.getElementById("status-line");
    line.textContent = "padmanabhan-iyer left the table.";
    line.classList.add("visible");
    const range = document.createRange();
    range.selectNodeContents(line);
    const message = range.getBoundingClientRect();
    const hit = ["#start-game", "#share-invite", "#leave-table"].filter((selector) => {
      const control = document.querySelector(selector).getBoundingClientRect();
      return (
        Math.min(control.right, message.right) - Math.max(control.left, message.left) > 2 &&
        Math.min(control.bottom, message.bottom) - Math.max(control.top, message.top) > 2
      );
    });
    line.textContent = "";
    line.classList.remove("visible");
    return hit;
  });
  check(`${viewport.width}×${viewport.height}: a status message clears the lobby's controls`, clashes.length === 0, clashes);
}
await page.setViewportSize({ width: 1280, height: 800 });

await page.click("#start-game");
await page.waitForSelector("#table-moment.visible", { timeout: 15000 });
const dealAnnouncement = await page.evaluate(() => {
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
      player,
      visible: root instanceof HTMLElement && Number.parseFloat(getComputedStyle(root).opacity) >= 0.5,
      collision: parts.some((part) => overlaps(panel, part.getBoundingClientRect())),
    };
  });
});
check(
  "deal announcement keeps every opponent visible and unobstructed",
  dealAnnouncement.every((seat) => seat.visible && !seat.collision),
  dealAnnouncement
);

// The bid panel, with the other seats' calls showing, on every class of phone:
// it must clear the seats and the hand, and its calls must stay beside their
// players. scripts/test-layout-matrix.mjs checks every other screen.
async function checkBidPanelAtEverySize() {
  await page.waitForTimeout(2500); // the calls land one at a time
  for (const viewport of [
    { width: 280, height: 653 },
    { width: 320, height: 568 },
    { width: 360, height: 640 },
    { width: 390, height: 844 },
    { width: 412, height: 915 },
    { width: 768, height: 1024 },
    { width: 568, height: 320 },
    { width: 640, height: 360 },
    { width: 844, height: 390 },
    { width: 915, height: 412 },
    { width: 820, height: 600 },
    { width: 1024, height: 600 },
    { width: 1280, height: 800 },
  ]) {
    await page.setViewportSize(viewport);
    await page.waitForTimeout(450); // seats rise out of a prompt's way with a transition
    const prompt = await page.evaluate(() => {
      const box = (element) => {
        const { left, top, right, bottom } = element.getBoundingClientRect();
        return { left, top, right, bottom };
      };
      const panel = document.getElementById("bid-panel");
      const seats = [1, 2, 3].map((player) => {
        const root = document.getElementById(`seat-label-${player}`);
        const call = root.querySelector(".seat-action.visible");
        const parts = [root.querySelector(".seat-marker"), root.querySelector(".player-copy"), root.querySelector(".partner-tag"), call];
        return {
          player,
          marker: box(root.querySelector(".seat-marker")),
          call: call ? box(call) : null,
          parts: parts.filter(Boolean).map(box),
        };
      });
      return {
        panel: box(panel),
        seats,
        chips: [...panel.querySelectorAll(".bid-chip")].filter((chip) => chip.getClientRects().length).map(box),
        hand: window.__game.handCardBounds(),
        pageWidth: document.scrollingElement.scrollWidth,
      };
    });
    const overlaps = (first, second) =>
      Math.min(first.right, second.right) - Math.max(first.left, second.left) > 2 &&
      Math.min(first.bottom, second.bottom) - Math.max(first.top, second.top) > 2;
    const name = `${viewport.width}×${viewport.height}`;
    check(
      `${name}: bid panel fits the screen`,
      prompt.panel.left >= 0 && prompt.panel.right <= viewport.width && prompt.panel.top >= 0 && prompt.panel.bottom <= viewport.height,
      prompt.panel
    );
    check(
      `${name}: bid panel clears every seat's ring, name and call`,
      prompt.seats.every((seat) => seat.parts.every((part) => !overlaps(part, prompt.panel))),
      prompt.seats
    );
    check(
      `${name}: bid panel clears the hand`,
      prompt.hand.every((card) => !overlaps(card, prompt.panel)),
      { panel: prompt.panel, hand: prompt.hand }
    );
    check(
      `${name}: every call sits beside its ring`,
      prompt.seats.some((seat) => seat.call) &&
        prompt.seats.every((seat) => !seat.call || (seat.call.top < seat.marker.bottom && seat.call.bottom > seat.marker.top)),
      prompt.seats
    );
    // The smallest phones on their side give up a little of a chip to keep every bid on two rows.
    const finger = viewport.height <= 340 ? 36 : 40;
    check(
      `${name}: bid chips are a finger wide`,
      prompt.chips.length > 0 && prompt.chips.every((chip) => chip.right - chip.left >= finger && chip.bottom - chip.top >= finger),
      prompt.chips
    );
    check(`${name}: the page has no sideways scroll`, prompt.pageWidth <= viewport.width, prompt.pageWidth);
    await page.screenshot({ path: `${SHOTS}/${viewport.width}x${viewport.height}-bid.png` });
  }
  await page.setViewportSize({ width: 1280, height: 800 });
}

let bidPanelChecked = false;
let frozen = false;
const deadline = Date.now() + 180_000;
while (Date.now() < deadline) {
  const state = await page.evaluate(() => {
    const model = window.__game.state();
    return {
      phase: model.phase,
      pending: model.pendingRequest?.kind ?? null,
      legal: window.__game.legalCards(),
      played: model.playedCards.map((play) => play.playerIndex),
    };
  });

  if (state.pending === "bid" && !bidPanelChecked) {
    bidPanelChecked = true;
    await checkBidPanelAtEverySize();
  }
  if (state.pending === "play" && state.played.length === 3 && !state.played.includes(0)) {
    frozen = true;
    break;
  }
  if (state.pending === "bid") {
    await page.evaluate(() => window.__game.pass());
  } else if (state.pending === "trump") {
    await page.evaluate(() => window.__game.trump("spades"));
  } else if (state.pending === "play" && state.legal[0]) {
    const exposed = await page.evaluate(() => {
      const button = document.getElementById("expose-button");
      if (button && !button.classList.contains("hidden")) {
        window.__game.expose();
        return true;
      }
      return false;
    });
    if (!exposed) await page.evaluate((id) => window.__game.play(id), state.legal[0]);
  } else if (state.phase === "hand-finished") {
    break;
  }
  await page.waitForTimeout(100);
}
check("reached a trick with all three opponent cards", frozen);
if (!frozen) {
  await browser.close();
  process.exit(1);
}

// Let card flights and any deal announcement finish before measuring.
await page.waitForTimeout(3400);
await page.waitForFunction(
  () => !document.getElementById("ui-layer")?.classList.contains("table-announcing"),
  { timeout: 5000 }
);

const viewports = [
  { width: 1440, height: 900 },
  { width: 1280, height: 720 },
  { width: 1024, height: 600 },
  { width: 820, height: 600 },
  { width: 430, height: 932 },
  { width: 390, height: 844 },
  { width: 360, height: 740 },
  { width: 844, height: 390 },
  { width: 640, height: 360 },
];

for (const viewport of viewports) {
  await page.setViewportSize(viewport);
  await page.waitForTimeout(700);
  const layout = await page.evaluate(() => {
    const rect = (element) => {
      if (!(element instanceof HTMLElement)) return null;
      const bounds = element.getBoundingClientRect();
      return {
        left: bounds.left,
        top: bounds.top,
        right: bounds.right,
        bottom: bounds.bottom,
        cx: (bounds.left + bounds.right) / 2,
        cy: (bounds.top + bounds.bottom) / 2,
      };
    };
    const seats = [1, 2, 3].map((player) => {
      const root = document.getElementById(`seat-label-${player}`);
      return {
        player,
        visible: root instanceof HTMLElement && Number.parseFloat(getComputedStyle(root).opacity) >= 0.5,
        marker: rect(root?.querySelector(".seat-marker")),
        copy: rect(root?.querySelector(".player-copy")),
        partner: rect(root?.querySelector(".partner-tag")),
      };
    });
    return {
      width: innerWidth,
      height: innerHeight,
      canvas: rect(document.querySelector("canvas")),
      score: rect(document.getElementById("score-display")),
      emote: rect(document.getElementById("emote-button")),
      cards: window.__game.playedCardBounds(),
      hand: window.__game.handCardBounds(),
      seats,
    };
  });

  const overlaps = (first, second) =>
    first && second &&
    first.left < second.right && first.right > second.left &&
    first.top < second.bottom && first.bottom > second.top;
  const opponentCards = [1, 2, 3].map((player) =>
    layout.cards.find((card) => card.player === player)
  );
  const seatParts = layout.seats.flatMap((seat) => [seat.marker, seat.copy, seat.partner].filter(Boolean));
  const collisions = opponentCards.flatMap((card) =>
    seatParts.filter((part) => card && overlaps(card, part)).map((part) => ({ card, part }))
  );
  const sideAlignment = [1, 3].every((player) => {
    const card = layout.cards.find((candidate) => candidate.player === player);
    const marker = layout.seats.find((seat) => seat.player === player)?.marker;
    return card && marker && Math.abs(card.centerY - marker.cy) <= 3;
  });
  const topCard = layout.cards.find((card) => card.player === 2);
  const topSeat = layout.seats.find((seat) => seat.player === 2);
  const topContentBottom = Math.max(
    topSeat?.marker?.bottom ?? 0,
    topSeat?.copy?.bottom ?? 0,
    topSeat?.partner?.bottom ?? 0
  );
  const handFits = layout.hand.every((card) =>
    card.left >= 0 && card.right <= layout.width && card.top >= 0 && card.bottom <= layout.height - 25
  );
  const seatsFit = layout.seats.every((seat) =>
    seat.marker && seat.marker.left >= 0 && seat.marker.right <= layout.width &&
    seat.marker.top >= 0 && seat.marker.bottom <= layout.height
  );
  const seatsVisible = layout.seats.every((seat) => seat.visible);
  const canvasFits = layout.canvas &&
    Math.abs(layout.canvas.left) <= 1 && Math.abs(layout.canvas.top) <= 1 &&
    Math.abs(layout.canvas.right - layout.width) <= 1 &&
    Math.abs(layout.canvas.bottom - layout.height) <= 1;
  const chromeCollisions = layout.seats.flatMap((seat) =>
    [layout.score, layout.emote]
      .filter((part) => seat.marker && overlaps(seat.marker, part))
      .map((part) => ({ seat: seat.player, part }))
  );

  const name = `${viewport.width}×${viewport.height}`;
  check(`${name}: no opponent card intersects any seat content`, collisions.length === 0, collisions);
  check(`${name}: side cards share their seats' row`, sideAlignment, layout);
  check(
    `${name}: upper card is centered below all upper-seat text`,
    Boolean(topCard && topSeat?.marker && Math.abs(topCard.centerX - topSeat.marker.cx) <= 3 && topCard.top - topContentBottom >= 8),
    { topCard, topSeat, topContentBottom }
  );
  check(`${name}: seat circles fit the viewport`, seatsFit, layout.seats);
  check(`${name}: all opponent seats are visibly rendered`, seatsVisible, layout.seats);
  check(`${name}: game canvas fills the viewport without shifting`, Boolean(canvasFits), layout.canvas);
  check(`${name}: seats clear the score and emote controls`, chromeCollisions.length === 0, chromeCollisions);
  check(`${name}: local hand fits above the footer`, handFits, layout.hand);
  await page.screenshot({ path: `${SHOTS}/${viewport.width}x${viewport.height}.png` });
}

// The local fourth card must settle into its own band above the remaining hand.
await page.setViewportSize({ width: 390, height: 844 });
const legal = await page.evaluate(() => window.__game.legalCards()[0]);
await page.evaluate((cardId) => window.__game.play(cardId), legal);
await page.waitForTimeout(700);
const bottom = await page.evaluate(() => ({
  local: window.__game.playedCardBounds().find((card) => card.player === 0) ?? null,
  hand: window.__game.handCardBounds(),
}));
const handTop = Math.min(...bottom.hand.map((card) => card.top));
check(
  "390×844: local played card clears the remaining hand",
  bottom.local !== null && handTop - bottom.local.bottom >= 16,
  { local: bottom.local, handTop }
);
await page.screenshot({ path: `${SHOTS}/390x844-local-card.png` });

// Direct loads must initialize the camera projection without relying on a
// prior resize. Check the local play/hand bands in each tight layout class.
for (const viewport of [
  { width: 390, height: 844 },
  { width: 1024, height: 600 },
  { width: 844, height: 390 },
]) {
  const directPage = await browser.newPage({ viewport });
  const directUrl = new globalThis.URL(URL);
  directUrl.searchParams.set("mock", "1");
  await directPage.goto(directUrl.toString());
  await directPage.waitForFunction(
    () => window.__game.playedCardBounds().some((card) => card.player === 0),
    { timeout: 15000 }
  );
  await directPage.waitForTimeout(700);
  const direct = await directPage.evaluate(() => {
    const local = window.__game.playedCardBounds().find((card) => card.player === 0) ?? null;
    const hand = window.__game.handCardBounds();
    return {
      local,
      handTop: Math.min(...hand.map((card) => card.top)),
      handBottom: Math.max(...hand.map((card) => card.bottom)),
    };
  });
  check(
    `${viewport.width}×${viewport.height} direct load separates the local play and hand`,
    direct.local !== null &&
      direct.handTop - direct.local.bottom >= 12 &&
      direct.handBottom <= viewport.height - 20,
    direct
  );
  await directPage.screenshot({
    path: `${SHOTS}/${viewport.width}x${viewport.height}-direct-load.png`,
  });
  await directPage.close();
}

if (errors.length) {
  console.log("CONSOLE ERRORS:");
  errors.forEach((error) => console.log("  " + error));
}
await browser.close();
if (failures.length || errors.length) {
  console.log(`\n${failures.length} responsive layout check(s) failed`);
  process.exit(1);
}
console.log("\nall responsive table layout checks passed");
