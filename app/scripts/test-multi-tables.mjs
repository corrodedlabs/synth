// Proves multiple tables can exist at once: two hosts each create a table,
// a guest sees both in the room browser and joins one without disturbing the
// other. Usage: node scripts/test-multi-tables.mjs [url]
import { chromium } from "playwright";
import { existsSync, mkdirSync } from "node:fs";

const URL = process.argv[2] ?? "http://localhost:5173/";
const SHOTS = "/tmp/game-shots-tables";
mkdirSync(SHOTS, { recursive: true });

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

const createTable = async (page) => {
  await page.click("#create-button");
  await page.waitForSelector("#lobby-panel:not(.hidden)", { timeout: 15000 });
  return page.evaluate(() => window.__game.roomName());
};

// --- two hosts, two tables ---
const hostA = await newPlayer("hostA", "Asha");
const hostB = await newPlayer("hostB", "Birju");
const tableA = await createTable(hostA);
const tableB = await createTable(hostB);
console.log(`tables: "${tableA}" and "${tableB}"`);

// --- guest sees both ---
const guest = await newPlayer("guest", "Guest");
await guest.click("#browse-button");
await guest.waitForSelector(".room-row button", { timeout: 15000 });
const listed = await guest.$$eval(".room-row .room-name", (els) =>
  els.map((el) => el.textContent)
);
console.log(`guest sees: ${listed.join(" | ")}`);
check("both tables listed", listed.includes(tableA) && listed.includes(tableB));
await guest.screenshot({ path: `${SHOTS}/01-two-tables-listed.png` });

// --- guest joins table B; table A is untouched ---
await guest.locator(".room-row", { hasText: tableB }).locator("button").click();
await guest.waitForSelector("#lobby-panel:not(.hidden)", { timeout: 15000 });
await hostB.waitForFunction(() => window.__game.state().members.length === 2, undefined, {
  timeout: 10000,
});
check("guest joined table B (host B sees 2)", true);
check(
  "table A unaffected (still 1 member)",
  (await hostA.evaluate(() => window.__game.state().members.length)) === 1
);

if (errors.length) {
  console.log("CONSOLE ERRORS:");
  for (const error of errors) console.log("  " + error);
}
await browser.close();
if (failures.length || errors.length) {
  console.log(`\n${failures.length} check(s) failed`);
  process.exit(1);
}
console.log("\nall multi-table checks passed");
process.exit(0);
