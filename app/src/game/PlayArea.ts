import * as THREE from 'three';
import * as TWEEN from '@tweenjs/tween.js';
import { Card } from './Card';

export class PlayArea {
  private scene: THREE.Scene;
  private camera: THREE.Camera;
  private playedCards: Card[] = [];
  private positions: THREE.Vector3[] = []; // 4 positions for 4 players

  // Enso markers for other players (3D meshes positioned in scene)
  private ensoMarkers: THREE.Mesh[] = [];
  private ensoTexture: THREE.Texture | null = null;
  private activePulseTween: TWEEN.Tween<THREE.Vector3> | null = null;
  private activeMarkerIndex: number = -1;

  constructor(scene: THREE.Scene, camera: THREE.Camera) {
    this.scene = scene;
    this.camera = camera;

    // Define positions for played cards (Center of table)
    // Cards positioned in compact cross/plus pattern centered on play area
    // Matching mockup layout - tight cross with minimal spacing
    // Center point is around z=-0.5 (slightly toward back of view)
    // 0: Bottom (Player) - closest to camera
    // 1: Right
    // 2: Top - furthest from camera  
    // 3: Left
    const centerZ = -0.5;
    // Cards are camera-facing billboards, so the cross needs generous
    // spacing for the four cards to stay fully separated on screen.
    const spacingH = 1.15;
    const spacingV = 0.95;
    this.positions = [
      new THREE.Vector3(0, 0.2, centerZ + spacingV),       // Player (bottom/front)
      new THREE.Vector3(spacingH, 0.2, centerZ),           // Right
      new THREE.Vector3(0, 0.2, centerZ - spacingV),       // Top (back)
      new THREE.Vector3(-spacingH, 0.2, centerZ)           // Left
    ];
    
    // Load enso texture and create markers
    this.loadEnsoTexture();
  }

  // World anchors of the three opponent markers, keyed by view index.
  private markerWorldPositions: Record<number, THREE.Vector3> = {
    1: new THREE.Vector3(1.8, 0.05, -0.7),
    2: new THREE.Vector3(0, 0.05, -2.2),
    3: new THREE.Vector3(-1.8, 0.05, -0.7),
  };

  // Screen-space position of an opponent's marker, for HTML labels.
  public markerScreenPosition(
    playerIndex: number,
    canvas: HTMLElement
  ): { x: number; y: number } | null {
    const world = this.markerWorldPositions[playerIndex];
    if (!world) return null;
    const projected = world.clone().project(this.camera);
    return {
      x: ((projected.x + 1) / 2) * canvas.clientWidth,
      y: ((1 - projected.y) / 2) * canvas.clientHeight,
    };
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
    
    // Map player indices to markers: 1 -> right (0), 3 -> left (1), 2 -> top (2)
    // playerIndex 0 is the local player (no marker)
    let markerIndex = -1;
    if (playerIndex === 1) markerIndex = 0; // Right
    else if (playerIndex === 3) markerIndex = 1; // Left
    else if (playerIndex === 2) markerIndex = 2; // Top (partner)
    
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

    card.setPosition(pos.x, pos.y + (this.playedCards.length * 0.005), pos.z);

    // Face the camera head-on so ranks stay readable, with a slight
    // per-seat tilt so the trick reads like cards tossed onto a table.
    const toCamera = this.camera.position.clone().sub(pos);
    const rotX = -Math.atan2(toCamera.y, toCamera.z);
    const tilts = [0.05, -0.07, 0.04, 0.08];
    card.setRotation(rotX, 0, tilts[playerIndex] ?? 0);
  }

  // Sweeps the trick toward the winning seat, then removes the cards.
  public clearTable(winnerIndex: number | null = null) {
    const sweepTargets = [
      new THREE.Vector3(0, 0.4, 4.0),    // toward us
      new THREE.Vector3(4.5, 0.4, -0.5), // right
      new THREE.Vector3(0, 0.4, -4.0),   // top
      new THREE.Vector3(-4.5, 0.4, -0.5) // left
    ];
    const cards = this.playedCards;
    this.playedCards = [];

    if (winnerIndex === null) {
      cards.forEach(card => this.scene.remove(card.mesh));
      return;
    }

    const target = sweepTargets[winnerIndex];
    cards.forEach(card => {
      new TWEEN.Tween(card.mesh.position)
        .to({ x: target.x, y: target.y, z: target.z }, 550)
        .easing(TWEEN.Easing.Quadratic.In)
        .onComplete(() => this.scene.remove(card.mesh))
        .start();
    });
  }
}
