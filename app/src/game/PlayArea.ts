import * as THREE from 'three';
import * as TWEEN from '@tweenjs/tween.js';
import { Card } from './Card';

export class PlayArea {
  private scene: THREE.Scene;
  private playedCards: Card[] = [];
  private positions: THREE.Vector3[] = []; // 4 positions for 4 players
  
  // Enso markers for left and right decoration (2 total)
  private ensoMarkers: THREE.Sprite[] = [];
  private ensoTexture: THREE.Texture | null = null;
  private activePulseTween: TWEEN.Tween<THREE.Vector3> | null = null;
  private activeMarkerIndex: number = -1;

  constructor(scene: THREE.Scene) {
    this.scene = scene;
    
    // Define positions for played cards (Center of table)
    // Cards positioned in clear cross/plus pattern centered on play area
    // 0: Bottom (Player) - closer to camera
    // 1: Right
    // 2: Top - further from camera  
    // 3: Left
    this.positions = [
      new THREE.Vector3(0, 0.15, 0.4),     // Player (bottom)
      new THREE.Vector3(0.55, 0.15, -0.15), // Right
      new THREE.Vector3(0, 0.15, -0.7),    // Top
      new THREE.Vector3(-0.55, 0.15, -0.15) // Left
    ];
    
    // Load enso texture and create markers
    this.loadEnsoTexture();
  }

  private loadEnsoTexture() {
    // Use canvas-drawn enso for guaranteed transparency
    this.createFallbackEnsoTexture();
    this.createEnsoMarkers();
  }

  private createFallbackEnsoTexture() {
    const canvas = document.createElement('canvas');
    canvas.width = 256;
    canvas.height = 256;
    const ctx = canvas.getContext('2d')!;
    
    // Transparent background
    ctx.clearRect(0, 0, canvas.width, canvas.height);
    
    // Draw enso circle with brush-like stroke
    const centerX = 128;
    const centerY = 128;
    const radius = 90;
    
    // Create gradient for ink-like effect (thicker at start, fading at end)
    ctx.lineCap = 'round';
    ctx.lineJoin = 'round';
    
    // Draw the enso as multiple arcs with varying thickness for brush effect
    const startAngle = Math.PI * 0.6;  // Start from bottom-left
    const endAngle = Math.PI * 2.4;    // End with slight overlap gap
    const steps = 60;
    
    for (let i = 0; i < steps; i++) {
      const t = i / steps;
      const angle1 = startAngle + (endAngle - startAngle) * t;
      const angle2 = startAngle + (endAngle - startAngle) * (t + 1/steps);
      
      // Brush width varies - thick at start, thin at end
      const width = 18 * (1 - t * 0.7);
      
      // Opacity also fades
      const opacity = 0.95 - t * 0.3;
      
      ctx.strokeStyle = `rgba(40, 40, 40, ${opacity})`;
      ctx.lineWidth = width;
      ctx.beginPath();
      ctx.arc(centerX, centerY, radius, angle1, angle2);
      ctx.stroke();
    }
    
    this.ensoTexture = new THREE.CanvasTexture(canvas);
    this.ensoTexture.colorSpace = THREE.SRGBColorSpace;
  }

  private createEnsoMarkers() {
    if (!this.ensoTexture) return;
    
    // Only 2 markers: left and right sides (positioned like in mockup)
    // Standing upright at mid-height, far to the sides
    const markerPositions = [
      new THREE.Vector3(2.5, 0.8, -0.5),   // Right side
      new THREE.Vector3(-2.5, 0.8, -0.5)   // Left side
    ];
    
    markerPositions.forEach((pos) => {
      const material = new THREE.SpriteMaterial({
        map: this.ensoTexture,
        transparent: true,
        opacity: 0.7,
        depthWrite: false,
        sizeAttenuation: true,
        blending: THREE.NormalBlending
      });
      
      const marker = new THREE.Sprite(material);
      marker.position.copy(pos);
      marker.scale.set(1.8, 1.8, 1); // Larger size for prominence like mockup
      
      this.scene.add(marker);
      this.ensoMarkers.push(marker);
    });
  }

  /**
   * Set the active player marker (triggers pulse animation)
   * @param playerIndex - 0 for local player (no marker), 1 for right opponent, 3 for left opponent
   */
  public setActivePlayer(playerIndex: number) {
    // Stop existing animation
    if (this.activePulseTween) {
      this.activePulseTween.stop();
      this.activePulseTween = null;
    }
    
    // Reset previous marker
    if (this.activeMarkerIndex >= 0 && this.activeMarkerIndex < this.ensoMarkers.length) {
      const prevMarker = this.ensoMarkers[this.activeMarkerIndex];
      prevMarker.scale.set(1.8, 1.8, 1);
      (prevMarker.material as THREE.SpriteMaterial).opacity = 0.7;
    }
    
    // Map player indices to 2 markers: 1 -> right (0), 3 -> left (1)
    // playerIndex 0 is local player (no marker), 2 is top (no marker)
    let markerIndex = -1;
    if (playerIndex === 1) markerIndex = 0; // Right
    else if (playerIndex === 3) markerIndex = 1; // Left
    
    this.activeMarkerIndex = markerIndex;
    
    if (markerIndex >= 0 && markerIndex < this.ensoMarkers.length) {
      const marker = this.ensoMarkers[markerIndex];
      (marker.material as THREE.SpriteMaterial).opacity = 0.9;
      
      // Pulse animation
      this.activePulseTween = new TWEEN.Tween(marker.scale)
        .to({ x: 2.0, y: 2.0, z: 1 }, 800)
        .easing(TWEEN.Easing.Sinusoidal.InOut)
        .yoyo(true)
        .repeat(Infinity)
        .start();
    }
  }

  /**
   * Clear active player indicator
   */
  public clearActivePlayer() {
    if (this.activePulseTween) {
      this.activePulseTween.stop();
      this.activePulseTween = null;
    }
    
    if (this.activeMarkerIndex >= 0 && this.activeMarkerIndex < this.ensoMarkers.length) {
      const marker = this.ensoMarkers[this.activeMarkerIndex];
      marker.scale.set(1.8, 1.8, 1);
      (marker.material as THREE.SpriteMaterial).opacity = 0.7;
    }
    
    this.activeMarkerIndex = -1;
  }

  public playCard(card: Card, playerIndex: number) {
    this.playedCards.push(card);
    
    // If card was not in scene (e.g. from opponent), add it
    if (!card.mesh.parent) {
      this.scene.add(card.mesh);
    }

    const pos = this.positions[playerIndex];
    
    // Add some randomness to look natural
    const randomOffset = 0.05;
    const rx = (Math.random() - 0.5) * randomOffset;
    const rz = (Math.random() - 0.5) * randomOffset;

    card.setPosition(pos.x + rx, pos.y + (this.playedCards.length * 0.01), pos.z + rz);
    
    // Rotate flat on table, face up (front face visible from above)
    // Card front is +Z face. Rotate to show front face pointing up toward camera
    const randomRot = (Math.random() - 0.5) * 0.15;
    card.setRotation(-Math.PI / 2, 0, randomRot);
  }

  public clearTable() {
    this.playedCards.forEach(card => {
      // Animate away or disappear
      // For now, just remove from scene
      this.scene.remove(card.mesh);
    });
    this.playedCards = [];
  }
}
