import * as THREE from 'three';
import * as TWEEN from '@tweenjs/tween.js';
import { Card } from './Card';

export class PlayArea {
  private scene: THREE.Scene;
  private playedCards: Card[] = [];
  private positions: THREE.Vector3[] = []; // 4 positions for 4 players
  
  // Enso markers for other players (3D meshes positioned in scene)
  private ensoMarkers: THREE.Mesh[] = [];
  private ensoTexture: THREE.Texture | null = null;
  private activePulseTween: TWEEN.Tween<THREE.Vector3> | null = null;
  private activeMarkerIndex: number = -1;

  constructor(scene: THREE.Scene) {
    this.scene = scene;
    
    // Define positions for played cards (Center of table)
    // Cards positioned in compact cross/plus pattern centered on play area
    // Matching mockup layout - tight cross with minimal spacing
    // Center point is around z=-0.5 (slightly toward back of view)
    // 0: Bottom (Player) - closest to camera
    // 1: Right
    // 2: Top - furthest from camera  
    // 3: Left
    const centerZ = -0.5;
    const spacingH = 0.30;  // Horizontal spacing (left-right) - tight like reference
    const spacingV = 0.32;  // Vertical spacing (front-back) - tight like reference
    this.positions = [
      new THREE.Vector3(0, 0.2, centerZ + spacingV),       // Player (bottom/front)
      new THREE.Vector3(spacingH, 0.2, centerZ),           // Right
      new THREE.Vector3(0, 0.2, centerZ - spacingV),       // Top (back)
      new THREE.Vector3(-spacingH, 0.2, centerZ)           // Left
    ];
    
    // Load enso texture and create markers
    this.loadEnsoTexture();
  }

  private loadEnsoTexture() {
    // Load the actual enso.png texture for authentic brush stroke appearance
    const loader = new THREE.TextureLoader();
    loader.load('/textures/enso.png', (texture) => {
      texture.colorSpace = THREE.SRGBColorSpace;
      texture.premultiplyAlpha = false;
      this.ensoTexture = texture;
      this.createEnsoMarkers();
    });
  }

  private createEnsoMarkers() {
    if (!this.ensoTexture) return;
    
    // Three markers positioned in 3D space representing other players
    // Positioned to the sides and back, tilted to face the camera naturally
    const markerData = [
      { 
        position: new THREE.Vector3(1.8, 0.05, -0.7),  // Right player
        rotation: new THREE.Euler(-Math.PI / 2.5, 0, 0),  // Tilted back
        scale: 1.1
      },
      { 
        position: new THREE.Vector3(-1.8, 0.05, -0.7), // Left player
        rotation: new THREE.Euler(-Math.PI / 2.5, 0, 0),  // Tilted back
        scale: 1.1
      },
      { 
        position: new THREE.Vector3(0, 0.05, -2.2),    // Top/back player
        rotation: new THREE.Euler(-Math.PI / 2.2, 0, 0),  // More tilted (further back)
        scale: 0.9
      }
    ];
    
    // Create plane geometry for the enso (square aspect ratio matching texture)
    const geometry = new THREE.PlaneGeometry(1, 1);
    
    markerData.forEach((data) => {
      const material = new THREE.MeshBasicMaterial({
        map: this.ensoTexture,
        transparent: true,
        opacity: 0.9,
        alphaTest: 0.5,
        side: THREE.DoubleSide,
        depthWrite: false
      });
      
      const marker = new THREE.Mesh(geometry, material);
      marker.position.copy(data.position);
      marker.rotation.copy(data.rotation);
      marker.scale.set(data.scale, data.scale, 1);
      marker.userData.baseScale = data.scale;
      
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
      const baseScale = prevMarker.userData.baseScale ?? 1.1;
      prevMarker.scale.set(baseScale, baseScale, 1);
      (prevMarker.material as THREE.MeshBasicMaterial).opacity = 0.7;
    }
    
    // Map player indices to 2 markers: 1 -> right (0), 3 -> left (1)
    // playerIndex 0 is local player (no marker), 2 is top (no marker)
    let markerIndex = -1;
    if (playerIndex === 1) markerIndex = 0; // Right
    else if (playerIndex === 3) markerIndex = 1; // Left
    
    this.activeMarkerIndex = markerIndex;
    
    if (markerIndex >= 0 && markerIndex < this.ensoMarkers.length) {
      const marker = this.ensoMarkers[markerIndex];
      const baseScale = marker.userData.baseScale ?? 1.1;
      (marker.material as THREE.MeshBasicMaterial).opacity = 0.95;
      
      // Pulse animation
      this.activePulseTween = new TWEEN.Tween(marker.scale)
        .to({ x: baseScale * 1.15, y: baseScale * 1.15, z: 1 }, 900)
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
      const baseScale = marker.userData.baseScale ?? 1.1;
      marker.scale.set(baseScale, baseScale, 1);
      (marker.material as THREE.MeshBasicMaterial).opacity = 0.7;
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
    
    // Precise positioning - no random offsets for clean cross pattern
    card.setPosition(pos.x, pos.y + (this.playedCards.length * 0.005), pos.z);
    
    // Cards lie flat, front face up - all same orientation like reference
    card.setRotation(0, 0, 0);
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
