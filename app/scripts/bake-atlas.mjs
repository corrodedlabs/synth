// Bakes the card art into one image for the Android app, using the web client's own texture code, so
// the painted J of spades and inked 9s come along. Rerun it when the art changes, with the dev server up:
//   npm run dev -- --port 5179
//   node scripts/bake-atlas.mjs http://localhost:5179/ ../native/assets/cards.png
// The grid must match table_atlas_uv in native/src/table.c.
import { writeFileSync } from "node:fs";
import { chromium } from "playwright";

const [url = "http://localhost:5179/", out = "../native/assets/cards.png"] = process.argv.slice(2);
const browser = await chromium.launch({
  headless: !process.env.HEADED,
  channel: process.env.CI ? undefined : "chrome",
});
const page = await browser.newPage();
page.on("pageerror", (error) => {
  console.error(error);
  process.exitCode = 1;
});
await page.goto(url);

const dataUrl = await page.evaluate(async () => {
  const { ZenCardTextures } = await import("/src/utils/ZenCardTextures.ts");
  const textures = new ZenCardTextures();
  await textures.waitForLoad();
  await document.fonts.ready;

  const SUITS = ["hearts", "diamonds", "clubs", "spades"];
  const RANKS = ["J", "9", "A", "10", "K", "Q", "8", "7"];
  const CELL_W = 208;
  const CELL_H = 308;
  const GUTTER = 4;
  const canvas = document.createElement("canvas");
  canvas.width = CELL_W * 6;
  canvas.height = CELL_H * 6;
  const ctx = canvas.getContext("2d");
  ctx.imageSmoothingQuality = "high";

  const draw = (image, cell) => {
    const x = (cell % 6) * CELL_W;
    const y = Math.floor(cell / 6) * CELL_H;
    // Stretched through the gutter first, so a mip's bleed picks up the card and not its neighbour.
    ctx.drawImage(image, x, y, CELL_W, CELL_H);
    ctx.drawImage(image, x + GUTTER, y + GUTTER, CELL_W - GUTTER * 2, CELL_H - GUTTER * 2);
  };
  SUITS.forEach((suit, s) =>
    RANKS.forEach((rank, r) => draw(textures.getCardTexture(suit, rank).image, s * 8 + r))
  );
  draw(textures.getBackTexture().image, 32);
  return canvas.toDataURL("image/png");
});

writeFileSync(out, Buffer.from(dataUrl.split(",")[1], "base64"));
console.log(`wrote ${out}`);
await browser.close();
