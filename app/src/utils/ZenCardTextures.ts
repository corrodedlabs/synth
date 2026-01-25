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
  private readonly COLS = 8;  // J, 9, A, 10, K, Q, 8, 7
  private readonly ROWS = 4;  // hearts, diamonds, clubs, spades
  
  // Rank order in sprite sheet (left to right)
  private readonly RANK_ORDER = ['J', '9', 'A', '10', 'K', 'Q', '8', '7'];
  
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
    
    const cardWidth = img.width / this.COLS;
    const cardHeight = img.height / this.ROWS;

    for (let row = 0; row < this.ROWS; row++) {
      for (let col = 0; col < this.COLS; col++) {
        const suit = this.SUIT_ORDER[row];
        const rank = this.RANK_ORDER[col];
        const key = `${suit}-${rank}`;
        
        // Create canvas and slice the card
        const canvas = document.createElement('canvas');
        canvas.width = cardWidth;
        canvas.height = cardHeight;
        const ctx = canvas.getContext('2d')!;
        
        ctx.drawImage(
          img,
          col * cardWidth,      // source x
          row * cardHeight,     // source y
          cardWidth,            // source width
          cardHeight,           // source height
          0,                    // dest x
          0,                    // dest y
          cardWidth,            // dest width
          cardHeight            // dest height
        );
        
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
   * Fallback texture if sprite sheet fails to load
   */
  private createFallbackTexture(suit: string, rank: string): THREE.Texture {
    const canvas = document.createElement('canvas');
    canvas.width = 256;
    canvas.height = 356;
    const ctx = canvas.getContext('2d')!;
    
    // Cream background (zen style)
    ctx.fillStyle = '#faf8f5';
    ctx.fillRect(0, 0, canvas.width, canvas.height);
    
    // Simple border
    ctx.strokeStyle = '#333';
    ctx.lineWidth = 2;
    ctx.strokeRect(8, 8, canvas.width - 16, canvas.height - 16);
    
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
    
    const texture = new THREE.CanvasTexture(canvas);
    texture.colorSpace = THREE.SRGBColorSpace;
    return texture;
  }
}
