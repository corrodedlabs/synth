import * as THREE from 'three';
import { Card } from './Card';

export class Hand {
  private cards: Card[] = [];
  private scene: THREE.Scene;
  private handRadius = 2.4; // Wider arc radius for elegant fan
  private handCenter = new THREE.Vector3(0, 0.5, 3.2); // Position for prominent hand display

  constructor(scene: THREE.Scene) {
    this.scene = scene;
  }

  public addCard(card: Card) {
    this.cards.push(card);
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

    // Elegant fan spread for zen aesthetic
    const angleStep = 0.15; // Slightly tighter for elegant fan
    const totalAngle = (count - 1) * angleStep;
    const startAngle = -totalAngle / 2;

    this.cards.forEach((card, index) => {
      const angle = startAngle + index * angleStep;
      
      // Calculate position on the arc
      // x = r * sin(theta)
      // z = r * (1 - cos(theta)) -> to curve slightly back
      
      const x = this.handCenter.x + this.handRadius * Math.sin(angle);
      const z = this.handCenter.z - this.handRadius * (1 - Math.cos(angle));
      
      // Floating height with slight variation for depth
      const finalY = this.handCenter.y + (index * 0.002);

      // Position
      card.setPosition(x, finalY, z);

      // Rotation
      // Card's front face is +Z. Camera is above and in front looking down.
      // Tilt card back to show face to camera, rotate for fan effect
      // Add PI to Z rotation to flip card right-side up (rank at top)
      const rotZ = angle * 0.7 + Math.PI; // Fan rotation + flip for correct orientation
      const rotX = -Math.PI / 3.5; // Tilt back to show card face to camera
      const rotY = 0; // Front face toward camera
      
      card.setRotation(rotX, rotY, rotZ);
    });
  }
  
  public getCards(): Card[] {
    return this.cards;
  }
}
