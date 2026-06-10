import * as THREE from 'three';

/**
 * ZenCardTextures - Loads and slices the zen sprite sheet for card textures
 *
 * Sprite sheet layout (8x4 grid):
 * - Columns (ranks): J, 9, A, 10, K, Q, 8, 7
 * - Rows (suits): hearts, diamonds, clubs, spades
 */
export class ZenCardTextures {
  private spriteSheet: THREE.Texture | null = null;
  private backTexture: THREE.Texture | null = null;
  private cardTextures: Map<string, THREE.Texture> = new Map();
  private loadPromise: Promise<void>;
  
  // Sprite sheet dimensions
  private readonly COLS = 10;  // J, 9, A, SUIT, 10, K, Q, 9, 8, 7
  private readonly ROWS = 4;   // hearts, diamonds, clubs, spades
  
  // Rank order in sprite sheet (left to right)
  // Index 3 is the suit symbol (skipped during slicing).
  // Index 7 holds a second 9 — corrupted in the hearts row (its corner
  // indices read "8") and digit-less elsewhere, so it is never sliced.
  private readonly RANK_ORDER = ['J', '9', 'A', null, '10', 'K', 'Q', null, '8', '7'];

  // The spades row is laid out differently: there is no J♠ art at all —
  // cols 0 and 2 are plain pip cards and the ace sits at col 3.
  // 'J*' marks the pip card used as the base for a painted-on J.
  private readonly RANK_ORDER_SPADES = ['J*', '9', null, 'A', '10', 'K', 'Q', null, '8', '7'];
  
  // Suit order in sprite sheet (top to bottom)
  private readonly SUIT_ORDER = ['hearts', 'diamonds', 'clubs', 'spades'];

  constructor() {
    this.loadPromise = this.loadTextures();
  }

  private async loadTextures(): Promise<void> {
    const loader = new THREE.TextureLoader();
    
    // Load sprite sheet
    this.spriteSheet = await new Promise<THREE.Texture>((resolve, reject) => {
      loader.load(
        '/textures/zen-cards-sprite.png',
        (texture) => {
          texture.colorSpace = THREE.SRGBColorSpace;
          texture.minFilter = THREE.LinearFilter;
          texture.magFilter = THREE.LinearFilter;
          resolve(texture);
        },
        undefined,
        (error) => {
          console.error('Failed to load zen sprite sheet:', error);
          reject(error);
        }
      );
    });

    // Load back texture
    this.backTexture = await new Promise<THREE.Texture>((resolve, reject) => {
      loader.load(
        '/textures/zen-card-back.png',
        (texture) => {
          texture.colorSpace = THREE.SRGBColorSpace;
          texture.minFilter = THREE.LinearFilter;
          texture.magFilter = THREE.LinearFilter;
          resolve(texture);
        },
        undefined,
        (error) => {
          console.error('Failed to load zen card back:', error);
          reject(error);
        }
      );
    });

    // Pre-slice all card textures using canvas
    await this.sliceAllCards();
  }

  private async sliceAllCards(): Promise<void> {
    if (!this.spriteSheet || !this.spriteSheet.image) return;
    
    const img = this.spriteSheet.image as HTMLImageElement;

    // Wait for image to be fully loaded
    if (!img.complete) {
      await new Promise<void>((resolve) => {
        img.onload = () => resolve();
      });
    }

    // The painted J♠ uses the page's serif webfont
    await document.fonts.ready.catch(() => undefined);

    const cellWidth = img.width / this.COLS;
    const cellHeight = img.height / this.ROWS;
    
    // Trim edges to avoid bleeding from adjacent cards
    // Keep minimal to preserve corner rank/suit indicators
    const borderSize = 2; // pixels to trim from each edge
    const cardWidth = cellWidth - borderSize * 2;
    const cardHeight = cellHeight - borderSize * 2;

    for (let row = 0; row < this.ROWS; row++) {
      for (let col = 0; col < this.COLS; col++) {
        const suit = this.SUIT_ORDER[row];
        const rankOrder = suit === 'spades' ? this.RANK_ORDER_SPADES : this.RANK_ORDER;
        const rank = rankOrder[col];

        // Skip suit symbol cells (null entries in RANK_ORDER)
        if (rank === null) continue;

        // Normalize rank key (9b -> 9 for second 9 column, J* -> painted J)
        const rankKey = rank === '9b' ? '9' : rank === 'J*' ? 'J' : rank;
        const key = `${suit}-${rankKey}`;
        
        // Create canvas and slice the card (excluding borders)
        const canvas = document.createElement('canvas');
        canvas.width = cardWidth;
        canvas.height = cardHeight;
        const ctx = canvas.getContext('2d')!;
        
        // Source position: offset by borderSize to skip the border
        const srcX = col * cellWidth + borderSize;
        const srcY = row * cellHeight + borderSize;
        
        ctx.drawImage(
          img,
          srcX,                 // source x (skip left border)
          srcY,                 // source y (skip top border)
          cardWidth,            // source width (exclude borders)
          cardHeight,           // source height (exclude borders)
          0,                    // dest x
          0,                    // dest y
          cardWidth,            // dest width
          cardHeight            // dest height
        );
        
        // The J♠ has no art in the sheet — paint a rank over the pip card
        if (rank === 'J*') {
          this.paintJackOverlay(ctx, cardWidth, cardHeight);
        }

        // Only the hearts 9 has corner rank digits in the sheet; the other
        // suits' 9s carry bare pips — ink the missing digits in.
        if (rankKey === '9' && suit !== 'hearts') {
          this.paintNineIndices(ctx, cardWidth, cardHeight);
        }

        // Add rough brush-like edges for zen aesthetic
        this.addBrushEdges(ctx, cardWidth, cardHeight);

        const texture = new THREE.CanvasTexture(canvas);
        texture.colorSpace = THREE.SRGBColorSpace;
        texture.minFilter = THREE.LinearFilter;
        texture.magFilter = THREE.LinearFilter;
        
        this.cardTextures.set(key, texture);
      }
    }
  }

  /**
   * Wait for all textures to be loaded
   */
  public async waitForLoad(): Promise<void> {
    return this.loadPromise;
  }

  /**
   * Get texture for a specific card
   * @param suit - 'hearts', 'diamonds', 'clubs', or 'spades'
   * @param rank - 'J', '9', 'A', '10', 'K', 'Q', '8', or '7'
   */
  public getCardTexture(suit: string, rank: string): THREE.Texture {
    const key = `${suit}-${rank}`;
    const texture = this.cardTextures.get(key);
    
    if (!texture) {
      console.warn(`Card texture not found: ${key}, using fallback`);
      return this.createFallbackTexture(suit, rank);
    }
    
    return texture;
  }

  /**
   * Get the card back texture (enso design)
   */
  public getBackTexture(): THREE.Texture {
    if (!this.backTexture) {
      console.warn('Back texture not loaded, using fallback');
      return this.createFallbackBackTexture();
    }
    return this.backTexture;
  }

  /**
   * Turns the spades pip card into a J♠: covers the large centre pip with
   * the surrounding paper tone and paints an ink-style J, keeping the
   * hand-painted corner pips from the original art.
   */
  private paintJackOverlay(ctx: CanvasRenderingContext2D, width: number, height: number): void {
    // Sample the paper tone from a quiet strip near the top centre
    const sample = ctx.getImageData(Math.floor(width * 0.45), Math.floor(height * 0.05), 8, 8);
    let r = 0, g = 0, b = 0;
    const pixels = sample.data.length / 4;
    for (let i = 0; i < sample.data.length; i += 4) {
      r += sample.data[i];
      g += sample.data[i + 1];
      b += sample.data[i + 2];
    }
    ctx.save();
    ctx.fillStyle = `rgb(${Math.round(r / pixels)}, ${Math.round(g / pixels)}, ${Math.round(b / pixels)})`;
    ctx.beginPath();
    ctx.ellipse(width / 2, height * 0.52, width * 0.34, height * 0.40, 0, 0, Math.PI * 2);
    ctx.fill();

    // Ink J in the same painterly spirit as the other rank glyphs
    ctx.fillStyle = 'rgba(32, 29, 26, 0.92)';
    ctx.textAlign = 'center';
    ctx.textBaseline = 'middle';
    ctx.font = `italic ${Math.floor(height * 0.55)}px "Cormorant Garamond", "Times New Roman", serif`;
    ctx.fillText('J', width / 2, height * 0.52);
    ctx.restore();
  }

  /**
   * Paints corner "9" digits next to the bare corner pips, in the same
   * hand-inked style as the rest of the deck: below the top-left pip, and
   * rotated 180° above the bottom-right pip (standard card index convention).
   */
  private paintNineIndices(ctx: CanvasRenderingContext2D, width: number, height: number): void {
    ctx.save();
    ctx.fillStyle = 'rgba(32, 29, 26, 0.92)';
    ctx.textAlign = 'center';
    ctx.textBaseline = 'middle';
    ctx.font = `${Math.floor(height * 0.16)}px "Cormorant Garamond", "Times New Roman", serif`;
    ctx.fillText('9', width * 0.13, height * 0.285);
    ctx.translate(width * 0.87, height * 0.715);
    ctx.rotate(Math.PI);
    ctx.fillText('9', 0, 0);
    ctx.restore();
  }

  /**
   * Add rough, brush-like edges to give cards a hand-painted washi paper look
   */
  private addBrushEdges(ctx: CanvasRenderingContext2D, width: number, height: number): void {
    const edgeWidth = Math.max(3, Math.floor(width * 0.012));
    const inkColor = 'rgba(40, 35, 30, 0.7)'; // Dark ink, slightly transparent
    
    ctx.save();
    ctx.strokeStyle = inkColor;
    ctx.lineCap = 'round';
    ctx.lineJoin = 'round';
    
    // Draw rough strokes along each edge with varied thickness
    const drawBrushStroke = (
      startX: number, startY: number,
      endX: number, endY: number,
      isHorizontal: boolean
    ) => {
      const length = isHorizontal ? Math.abs(endX - startX) : Math.abs(endY - startY);
      const segments = Math.floor(length / 8);
      
      for (let i = 0; i < segments; i++) {
        const t = i / segments;
        const nextT = (i + 1) / segments;
        
        // Add subtle randomness to position
        const wobble = (Math.random() - 0.5) * 1.5;
        const nextWobble = (Math.random() - 0.5) * 1.5;
        
        let x1, y1, x2, y2;
        if (isHorizontal) {
          x1 = startX + t * (endX - startX);
          y1 = startY + wobble;
          x2 = startX + nextT * (endX - startX);
          y2 = startY + nextWobble;
        } else {
          x1 = startX + wobble;
          y1 = startY + t * (endY - startY);
          x2 = startX + nextWobble;
          y2 = startY + nextT * (endY - startY);
        }
        
        // Vary line thickness for brush effect
        ctx.lineWidth = edgeWidth * (0.5 + Math.random() * 0.8);
        
        // Occasional gaps for dry brush effect
        if (Math.random() > 0.1) {
          ctx.beginPath();
          ctx.moveTo(x1, y1);
          ctx.lineTo(x2, y2);
          ctx.stroke();
        }
      }
    };
    
    // Top edge
    drawBrushStroke(0, edgeWidth / 2, width, edgeWidth / 2, true);
    // Bottom edge
    drawBrushStroke(0, height - edgeWidth / 2, width, height - edgeWidth / 2, true);
    // Left edge
    drawBrushStroke(edgeWidth / 2, 0, edgeWidth / 2, height, false);
    // Right edge
    drawBrushStroke(width - edgeWidth / 2, 0, width - edgeWidth / 2, height, false);
    
    // Add corner emphasis with slightly thicker strokes
    const cornerSize = edgeWidth * 3;
    ctx.lineWidth = edgeWidth * 1.2;
    
    // Small corner accents
    const corners = [
      [0, 0, cornerSize, 0, 0, cornerSize],           // top-left
      [width, 0, width - cornerSize, 0, width, cornerSize], // top-right
      [0, height, cornerSize, height, 0, height - cornerSize], // bottom-left
      [width, height, width - cornerSize, height, width, height - cornerSize] // bottom-right
    ];
    
    corners.forEach(([cx, cy, x1, y1, x2, y2]) => {
      ctx.beginPath();
      ctx.moveTo(x1, y1);
      ctx.lineTo(cx, cy);
      ctx.lineTo(x2, y2);
      ctx.stroke();
    });
    
    ctx.restore();
  }

  private createFallbackTexture(suit: string, rank: string): THREE.Texture {
    const canvas = document.createElement('canvas');
    canvas.width = 256;
    canvas.height = 356;
    const ctx = canvas.getContext('2d')!;
    
    // Cream background (zen style)
    ctx.fillStyle = '#faf8f5';
    ctx.fillRect(0, 0, canvas.width, canvas.height);
    
    // Add brush edges instead of simple border
    this.addBrushEdges(ctx, canvas.width, canvas.height);
    
    // Suit color (ink style)
    const color = (suit === 'hearts' || suit === 'diamonds') ? '#8b0000' : '#1a1a1a';
    ctx.fillStyle = color;
    ctx.textAlign = 'center';
    
    // Rank
    ctx.font = 'bold 42px serif';
    ctx.fillText(rank, 40, 55);
    
    // Suit symbol
    ctx.font = '80px serif';
    let suitSymbol = '';
    switch (suit) {
      case 'hearts': suitSymbol = '\u2665'; break;
      case 'diamonds': suitSymbol = '\u2666'; break;
      case 'clubs': suitSymbol = '\u2663'; break;
      case 'spades': suitSymbol = '\u2660'; break;
    }
    ctx.fillText(suitSymbol, canvas.width / 2, canvas.height / 2 + 30);
    
    const texture = new THREE.CanvasTexture(canvas);
    texture.colorSpace = THREE.SRGBColorSpace;
    return texture;
  }

  /**
   * Fallback back texture
   */
  private createFallbackBackTexture(): THREE.Texture {
    const canvas = document.createElement('canvas');
    canvas.width = 256;
    canvas.height = 356;
    const ctx = canvas.getContext('2d')!;
    
    // Washi paper background
    ctx.fillStyle = '#f5f0e6';
    ctx.fillRect(0, 0, canvas.width, canvas.height);
    
    // Draw simple enso circle
    ctx.strokeStyle = '#333';
    ctx.lineWidth = 8;
    ctx.lineCap = 'round';
    ctx.beginPath();
    ctx.arc(canvas.width / 2, canvas.height / 2, 60, 0.3, Math.PI * 1.9);
    ctx.stroke();
    
    // Add brush edges
    this.addBrushEdges(ctx, canvas.width, canvas.height);
    
    const texture = new THREE.CanvasTexture(canvas);
    texture.colorSpace = THREE.SRGBColorSpace;
    return texture;
  }
}
