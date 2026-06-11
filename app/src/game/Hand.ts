import * as THREE from 'three';
import { Card } from './Card';

export class Hand {
  private cards: Card[] = [];
  private scene: THREE.Scene;
  private camera: THREE.Camera;
  private handCenter = new THREE.Vector3(0, 0.3, 2.0);
  // Narrow screens compress the fan (more overlap) so it stays in frame.
  private widthScale = 1;
  // Phones blow the cards up a little so ranks stay thumb-readable.
  private cardScale = 1;

  public setWidthScale(scale: number) {
    if (scale === this.widthScale) return;
    this.widthScale = scale;
    this.arrangeCards();
  }

  public setCardScale(scale: number) {
    if (scale === this.cardScale) return;
    this.cardScale = scale;
    this.cards.forEach((card) => card.mesh.scale.setScalar(scale));
  }

  constructor(scene: THREE.Scene, camera: THREE.Camera) {
    this.scene = scene;
    this.camera = camera;
  }

  // Tilt that points a card's face straight at the camera from a position
  private faceCameraRotX(position: THREE.Vector3): number {
    const toCamera = this.camera.position.clone().sub(position);
    return -Math.atan2(toCamera.y, toCamera.z);
  }

  public addCard(card: Card) {
    this.cards.push(card);
    card.mesh.scale.setScalar(this.cardScale);
    card.mesh.traverse((obj) => {
      if (obj instanceof THREE.Mesh) {
        obj.castShadow = true;
      }
    });
    // Spawn face-toward-camera just below the fan so dealt cards slide in
    // instead of flashing their backs while the rotation tween catches up.
    const spawn = new THREE.Vector3(this.handCenter.x, this.handCenter.y - 0.5, this.handCenter.z + 0.5);
    card.setPosition(spawn.x, spawn.y, spawn.z, true);
    card.setRotation(this.faceCameraRotX(spawn), 0, 0, true);
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

  // Canonical fan slot for a card index in a hand of `count` cards.
  // Fan with near-full card visibility: spacing adapts to hand size so
  // every rank stays readable (cards are 0.6 wide; leave ~0.07 overlap).
  private computeLayout(index: number, count: number): { position: THREE.Vector3; rotation: THREE.Euler } {
    const step = 0.53 * this.widthScale;
    const spreadX = Math.min((step * (count - 1)) / 2, 2.4 * this.widthScale);
    const curveHeight = 0.4; // gentle arc

    const p0 = new THREE.Vector2(-spreadX, this.handCenter.y);
    const p1 = new THREE.Vector2(0, this.handCenter.y + curveHeight);
    const p2 = new THREE.Vector2(spreadX, this.handCenter.y);

    // Step each card slightly toward the camera so render order and
    // physical order agree without visible depth differences.
    const depthStep = 0.012;
    const t = count > 1 ? index / (count - 1) : 0.5;

    // Quadratic bezier position
    const oneMinusT = 1 - t;
    const x = oneMinusT * oneMinusT * p0.x + 2 * oneMinusT * t * p1.x + t * t * p2.x;
    const y = oneMinusT * oneMinusT * p0.y + 2 * oneMinusT * t * p1.y + t * t * p2.y;
    const z = this.handCenter.z + index * depthStep;
    const position = new THREE.Vector3(x, y, z);

    // Gentle rotation for elegant fan; cards face the camera head-on so
    // ranks are never foreshortened.
    const normalizedPos = (t - 0.5) * 2; // -1 to 1
    const maxRotation = 0.16; // ~9 degrees max keeps neighbours readable
    const rotZ = -normalizedPos * maxRotation;
    const rotX = this.faceCameraRotX(position);

    return { position, rotation: new THREE.Euler(rotX, 0, rotZ) };
  }

  // Current fan slot of a card, or null if it is not in the hand.
  public layoutOf(card: Card): { position: THREE.Vector3; rotation: THREE.Euler } | null {
    const index = this.cards.indexOf(card);
    if (index < 0) return null;
    return this.computeLayout(index, this.cards.length);
  }

  public arrangeCards() {
    const count = this.cards.length;
    this.cards.forEach((card, index) => {
      const layout = this.computeLayout(index, count);
      card.setPosition(layout.position.x, layout.position.y, layout.position.z, false, 400);

      const renderOrder = index;
      card.mesh.traverse((obj) => {
        if (obj instanceof THREE.Mesh) {
          obj.renderOrder = renderOrder;
        }
      });

      card.setRotation(layout.rotation.x, layout.rotation.y, layout.rotation.z);
    });
  }
  
  public getCards(): Card[] {
    return this.cards;
  }
}
