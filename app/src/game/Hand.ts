import * as THREE from 'three';
import { Card } from './Card';

export class Hand {
  private cards: Card[] = [];
  private scene: THREE.Scene;
  private camera: THREE.Camera;
  private handCenter = new THREE.Vector3(0, 0.3, 2.0);

  constructor(scene: THREE.Scene, camera: THREE.Camera) {
    this.scene = scene;
    this.camera = camera;
  }

  public addCard(card: Card) {
    this.cards.push(card);
    card.mesh.traverse((obj) => {
      if (obj instanceof THREE.Mesh) {
        obj.castShadow = true;
      }
    });
    this.scene.add(card.mesh);
    this.arrangeCards();
  }

  public removeCard(card: Card) {
    const index = this.cards.indexOf(card);
    if (index > -1) {
      this.cards.splice(index, 1);
      this.arrangeCards();
    }
  }

  public arrangeCards() {
    const count = this.cards.length;
    if (count === 0) return;

    // Bezier curve for elegant fan shape like zen-mockup-reference.png
    // Target: ~30-40% overlap, pronounced arc, smooth rotation
    const spreadX = 1.6; // Tighter spread for closer overlap
    const curveHeight = 0.65; // Higher midpoint for stronger curve
    
    const p0 = new THREE.Vector2(-spreadX, this.handCenter.y);
    const p1 = new THREE.Vector2(0, this.handCenter.y + curveHeight);
    const p2 = new THREE.Vector2(spreadX, this.handCenter.y);

    // Subtle depth step to reduce z-fighting
    const depthStep = 0.002;
    const cameraPosition = this.camera.position;
    const rampStrength = 0.6;

    this.cards.forEach((card, index) => {
      const t = count > 1 ? index / (count - 1) : 0.5;
      
      // Quadratic bezier position
      const oneMinusT = 1 - t;
      const x = oneMinusT * oneMinusT * p0.x + 2 * oneMinusT * t * p1.x + t * t * p2.x;
      const y = oneMinusT * oneMinusT * p0.y + 2 * oneMinusT * t * p1.y + t * t * p2.y;
      const z = this.handCenter.z + index * depthStep;
      
      const finalPos = new THREE.Vector3(x, y, z);
      const toCard = finalPos.clone().sub(cameraPosition);
      const rayDir = toCard.normalize();
      const tNorm = count > 1 ? index / (count - 1) : 0.5;
      const eased = Math.pow(tNorm, 4);
      const closeOffset = eased * rampStrength;
      if (closeOffset > 0) {
        finalPos.addScaledVector(rayDir, -closeOffset);
      }

      card.setPosition(finalPos.x, finalPos.y, finalPos.z, true);
      
      const renderOrder = index;
      card.mesh.traverse((obj) => {
        if (obj instanceof THREE.Mesh) {
          obj.renderOrder = renderOrder;
        }
      });

      // Gentle rotation for elegant fan
      const normalizedPos = (t - 0.5) * 2; // -1 to 1
      const maxRotation = 0.26; // ~15 degrees max for softer edges
      const rotZ = -normalizedPos * maxRotation;
      const rotX = Math.PI / 2 + Math.PI - 0.10;
      
      card.setRotation(rotX, 0, rotZ);
    });
  }
  
  public getCards(): Card[] {
    return this.cards;
  }
}
