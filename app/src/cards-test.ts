/**
 * Card Texture Test Page
 * Renders all cards from the sprite sheet in a grid for visual verification
 */

import { ZenCardTextures } from './utils/ZenCardTextures';

const SUITS = ['hearts', 'diamonds', 'clubs', 'spades'];
const RANKS = ['J', '9', 'A', '10', 'K', 'Q', '8', '7'];

async function init() {
  const loadingEl = document.getElementById('loading')!;
  const containerEl = document.getElementById('cards-container')!;

  try {
    // First, load and display the raw sprite sheet with dimensions
    const rawSpriteSection = document.createElement('div');
    rawSpriteSection.className = 'suit-row';
    rawSpriteSection.innerHTML = '<div class="suit-title">Raw Sprite Sheet</div>';

    const spriteImg = new Image();
    spriteImg.src = '/textures/zen-cards-sprite.png';
    await new Promise<void>((resolve) => {
      spriteImg.onload = () => resolve();
    });

    const COLS = 10;  // J, 9, A, SUIT, 10, K, Q, 9b, 8, 7
    const ROWS = 4;
    const cellWidth = spriteImg.width / COLS;
    const cellHeight = spriteImg.height / ROWS;

    const infoDiv = document.createElement('div');
    infoDiv.style.cssText = 'margin-bottom: 10px; font-family: monospace;';
    infoDiv.innerHTML = `
      <p>Image dimensions: ${spriteImg.width} x ${spriteImg.height}</p>
      <p>Assuming ${COLS} cols x ${ROWS} rows:</p>
      <p>Cell size: ${cellWidth} x ${cellHeight}</p>
    `;
    rawSpriteSection.appendChild(infoDiv);

    // Draw sprite with grid overlay
    const overlayCanvas = document.createElement('canvas');
    overlayCanvas.width = spriteImg.width;
    overlayCanvas.height = spriteImg.height;
    overlayCanvas.style.cssText = 'max-width: 100%; border: 1px solid white;';
    const ctx = overlayCanvas.getContext('2d')!;
    ctx.drawImage(spriteImg, 0, 0);

    // Draw grid lines
    ctx.strokeStyle = 'red';
    ctx.lineWidth = 2;
    for (let col = 0; col <= COLS; col++) {
      const x = col * cellWidth;
      ctx.beginPath();
      ctx.moveTo(x, 0);
      ctx.lineTo(x, spriteImg.height);
      ctx.stroke();
    }
    for (let row = 0; row <= ROWS; row++) {
      const y = row * cellHeight;
      ctx.beginPath();
      ctx.moveTo(0, y);
      ctx.lineTo(spriteImg.width, y);
      ctx.stroke();
    }

    // Highlight what borderSize=2 would actually slice (first cell only)
    const borderSize = 2;
    const sliceWidth = cellWidth - borderSize * 2;
    const sliceHeight = cellHeight - borderSize * 2;
    ctx.strokeStyle = 'lime';
    ctx.lineWidth = 3;
    ctx.strokeRect(borderSize, borderSize, sliceWidth, sliceHeight);

    // Add slice info
    const sliceInfo = document.createElement('div');
    sliceInfo.style.cssText = 'margin-top: 10px; font-family: monospace;';
    sliceInfo.innerHTML = `
      <p style="color: lime;">Green rectangle shows slice for cell (0,0) with borderSize=${borderSize}:</p>
      <p>srcX: ${borderSize}, srcY: ${borderSize}</p>
      <p>sliceWidth: ${sliceWidth}, sliceHeight: ${sliceHeight}</p>
    `;

    rawSpriteSection.appendChild(overlayCanvas);
    rawSpriteSection.appendChild(sliceInfo);
    containerEl.appendChild(rawSpriteSection);

    // Manual slice test - directly slice first cell
    const manualSliceSection = document.createElement('div');
    manualSliceSection.className = 'suit-row';
    manualSliceSection.innerHTML = '<div class="suit-title">Manual Slice Test (cell 0,0)</div>';

    const testCanvas = document.createElement('canvas');
    testCanvas.width = sliceWidth;
    testCanvas.height = sliceHeight;
    testCanvas.style.cssText = 'border: 2px solid lime; max-width: 200px;';
    const testCtx = testCanvas.getContext('2d')!;
    testCtx.drawImage(
      spriteImg,
      borderSize, borderSize,  // source x, y
      sliceWidth, sliceHeight, // source width, height
      0, 0,                    // dest x, y
      sliceWidth, sliceHeight  // dest width, height
    );
    manualSliceSection.appendChild(testCanvas);

    const testLabel = document.createElement('div');
    testLabel.className = 'card-label';
    testLabel.textContent = `Direct slice from (${borderSize}, ${borderSize}) size ${sliceWidth}x${sliceHeight.toFixed(1)}`;
    manualSliceSection.appendChild(testLabel);

    containerEl.appendChild(manualSliceSection);

    // Load textures
    const textures = new ZenCardTextures();
    await textures.waitForLoad();

    loadingEl.style.display = 'none';

    // Debug: Show first card from ZenCardTextures for comparison
    const debugSection = document.createElement('div');
    debugSection.className = 'suit-row';
    debugSection.innerHTML = '<div class="suit-title">ZenCardTextures First Card (hearts-J which maps to row 0)</div>';

    const zenTexture = textures.getCardTexture('hearts', 'J');
    if (zenTexture.image instanceof HTMLCanvasElement) {
      const zenCanvas = document.createElement('canvas');
      zenCanvas.width = zenTexture.image.width;
      zenCanvas.height = zenTexture.image.height;
      zenCanvas.style.cssText = 'border: 2px solid orange; max-width: 200px;';
      const zenCtx = zenCanvas.getContext('2d')!;
      zenCtx.drawImage(zenTexture.image, 0, 0);
      debugSection.appendChild(zenCanvas);

      const zenInfo = document.createElement('div');
      zenInfo.className = 'card-label';
      zenInfo.textContent = `ZenCardTextures canvas size: ${zenTexture.image.width}x${zenTexture.image.height}`;
      debugSection.appendChild(zenInfo);
    }

    containerEl.appendChild(debugSection);

    // Render each suit as a row
    for (const suit of SUITS) {
      const suitSection = document.createElement('div');
      suitSection.className = 'suit-row';

      const title = document.createElement('div');
      title.className = 'suit-title';
      title.textContent = suit;
      suitSection.appendChild(title);

      const grid = document.createElement('div');
      grid.className = 'card-grid';

      for (const rank of RANKS) {
        const cardContainer = document.createElement('div');
        cardContainer.className = 'card-container';

        const texture = textures.getCardTexture(suit, rank);

        // Get the canvas from the texture
        if (texture.image instanceof HTMLCanvasElement) {
          const displayCanvas = document.createElement('canvas');
          displayCanvas.width = texture.image.width;
          displayCanvas.height = texture.image.height;
          const ctx = displayCanvas.getContext('2d')!;
          ctx.drawImage(texture.image, 0, 0);
          cardContainer.appendChild(displayCanvas);
        }

        const label = document.createElement('div');
        label.className = 'card-label';
        label.textContent = `${rank} of ${suit}`;
        cardContainer.appendChild(label);

        grid.appendChild(cardContainer);
      }

      suitSection.appendChild(grid);
      containerEl.appendChild(suitSection);
    }

    // Also show the back texture
    const backSection = document.createElement('div');
    backSection.className = 'suit-row back-section';

    const backTitle = document.createElement('div');
    backTitle.className = 'suit-title';
    backTitle.textContent = 'Card Back';
    backSection.appendChild(backTitle);

    const backContainer = document.createElement('div');
    backContainer.className = 'card-container';

    const backTexture = textures.getBackTexture();
    if (backTexture.image instanceof HTMLCanvasElement) {
      const displayCanvas = document.createElement('canvas');
      displayCanvas.width = backTexture.image.width;
      displayCanvas.height = backTexture.image.height;
      const ctx = displayCanvas.getContext('2d')!;
      ctx.drawImage(backTexture.image, 0, 0);
      backContainer.appendChild(displayCanvas);
    } else if (backTexture.image instanceof HTMLImageElement) {
      const displayCanvas = document.createElement('canvas');
      displayCanvas.width = backTexture.image.width;
      displayCanvas.height = backTexture.image.height;
      const ctx = displayCanvas.getContext('2d')!;
      ctx.drawImage(backTexture.image, 0, 0);
      backContainer.appendChild(displayCanvas);
    }

    const backLabel = document.createElement('div');
    backLabel.className = 'card-label';
    backLabel.textContent = 'Back (Enso)';
    backContainer.appendChild(backLabel);

    backSection.appendChild(backContainer);
    containerEl.appendChild(backSection);

  } catch (error) {
    loadingEl.textContent = `Error loading textures: ${error}`;
    console.error(error);
  }
}

init();
