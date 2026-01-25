import * as THREE from 'three';

export class CardTextureGenerator {
  private width = 256;
  private height = 356;
  private canvas: HTMLCanvasElement;
  private ctx: CanvasRenderingContext2D;

  constructor() {
    this.canvas = document.createElement('canvas');
    this.canvas.width = this.width;
    this.canvas.height = this.height;
    this.ctx = this.canvas.getContext('2d')!;
  }

  public createCardTexture(suit: string, rank: string): THREE.Texture {
    this.ctx.clearRect(0, 0, this.width, this.height);

    // Background (Parchment style)
    this.ctx.fillStyle = '#f4e4bc';
    this.ctx.fillRect(0, 0, this.width, this.height);
    
    // Border
    this.ctx.strokeStyle = '#8b4513'; // SaddleBrown
    this.ctx.lineWidth = 10;
    this.ctx.strokeRect(5, 5, this.width - 10, this.height - 10);

    // Suit Color
    const color = (suit === 'hearts' || suit === 'diamonds') ? '#d32f2f' : '#212121';
    this.ctx.fillStyle = color;
    this.ctx.textAlign = 'center';

    // Rank (Corner)
    this.ctx.font = 'bold 48px serif';
    this.ctx.fillText(rank, 40, 60);

    // Suit (Center)
    this.ctx.font = '100px serif';
    let suitSymbol = '';
    switch (suit) {
      case 'hearts': suitSymbol = '♥'; break;
      case 'diamonds': suitSymbol = '♦'; break;
      case 'clubs': suitSymbol = '♣'; break;
      case 'spades': suitSymbol = '♠'; break;
    }
    this.ctx.fillText(suitSymbol, this.width / 2, this.height / 2 + 35);
    
    // Bottom Rank (inverted)
    this.ctx.save();
    this.ctx.translate(this.width - 40, this.height - 60);
    this.ctx.rotate(Math.PI);
    this.ctx.fillText(rank, 0, 0);
    this.ctx.restore();

    // Weathering effect (simple noise overlay)
    this.addNoise();

    const texture = new THREE.CanvasTexture(this.canvas);
    texture.colorSpace = THREE.SRGBColorSpace;
    return texture;
  }

  public createBackTexture(): THREE.Texture {
    this.ctx.clearRect(0, 0, this.width, this.height);

    // Background
    this.ctx.fillStyle = '#2c3e50'; // Dark blue/slate
    this.ctx.fillRect(0, 0, this.width, this.height);

    // Pattern (Crosshatch)
    this.ctx.strokeStyle = '#34495e';
    this.ctx.lineWidth = 2;
    this.ctx.beginPath();
    for(let i=0; i<this.width; i+=20) {
      this.ctx.moveTo(i, 0);
      this.ctx.lineTo(i, this.height);
    }
    for(let i=0; i<this.height; i+=20) {
      this.ctx.moveTo(0, i);
      this.ctx.lineTo(this.width, i);
    }
    this.ctx.stroke();

    // Border
    this.ctx.strokeStyle = '#f1c40f'; // Gold
    this.ctx.lineWidth = 8;
    this.ctx.strokeRect(10, 10, this.width - 20, this.height - 20);

    const texture = new THREE.CanvasTexture(this.canvas);
    texture.colorSpace = THREE.SRGBColorSpace;
    return texture;
  }

  private addNoise() {
    const imageData = this.ctx.getImageData(0, 0, this.width, this.height);
    const data = imageData.data;
    for (let i = 0; i < data.length; i += 4) {
      const noise = (Math.random() - 0.5) * 10;
      data[i] = Math.min(255, Math.max(0, data[i] + noise));
      data[i+1] = Math.min(255, Math.max(0, data[i+1] + noise));
      data[i+2] = Math.min(255, Math.max(0, data[i+2] + noise));
    }
    this.ctx.putImageData(imageData, 0, 0);
  }
}
