import * as THREE from 'three';
import { Card } from './Card';
import { objectScreenBounds, screenPointOnPlane } from './TableLayout';
import type { ScreenPoint } from './TableLayout';

// World units a playable card rises by, at card scale 1 (a card is 0.9 tall).
const LIFT = 0.12;

export class Hand {
  private cards: Card[] = [];
  private scene: THREE.Scene;
  private camera: THREE.PerspectiveCamera;
  private handCenter = new THREE.Vector3(0, 0.3, 2.0);
  // Narrow screens compress the fan (more overlap) so it stays in frame.
  private widthScale = 1;
  // Phones blow the cards up a little so ranks stay thumb-readable.
  private cardScale = 1;
  private dealTimers = new Set<number>();
  // Cards standing proud of the fan: the ones that can be played right now.
  private liftedIds: ReadonlySet<string> = new Set();

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

  public setScreenCenter(
    center: ScreenPoint,
    viewport: { readonly width: number; readonly height: number }
  ) {
    const world = screenPointOnPlane(center, this.handCenter.y, this.camera, viewport);
    if (world.x === this.handCenter.x && world.z === this.handCenter.z) return;
    this.handCenter.x = world.x;
    this.handCenter.z = world.z;
    this.arrangeCards();
  }

  constructor(scene: THREE.Scene, camera: THREE.PerspectiveCamera) {
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

  // The server already sends four cards before the auction and four after
  // trump selection. Staggering each packet gives the hand a clear deal
  // rhythm without delaying the authoritative hand model.
  public dealCards(
    cards: readonly Card[],
    staggerMs: number,
    onComplete?: () => void,
    completeMs = staggerMs * Math.max(0, cards.length - 1) + 400
  ) {
    this.cancelPendingDeals();
    cards.forEach((card, index) => {
      const delay = staggerMs * index;
      if (delay === 0) {
        this.addCard(card);
        return;
      }
      const timer = window.setTimeout(() => {
        this.dealTimers.delete(timer);
        this.addCard(card);
      }, delay);
      this.dealTimers.add(timer);
    });
    if (onComplete) {
      if (completeMs <= 0) {
        onComplete();
      } else {
        const timer = window.setTimeout(() => {
          this.dealTimers.delete(timer);
          onComplete();
        }, completeMs);
        this.dealTimers.add(timer);
      }
    }
  }

  public cancelPendingDeals() {
    this.dealTimers.forEach((timer) => window.clearTimeout(timer));
    this.dealTimers.clear();
  }

  // Lift is part of a card's fan slot, so hovering, arming and dropping a card
  // all come back to the same place. Purely visual: the rules live elsewhere.
  public setLifted(ids: readonly string[], immediate = false) {
    const next = new Set(ids);
    if (next.size === this.liftedIds.size && ids.every((id) => this.liftedIds.has(id))) return;
    this.liftedIds = next;
    this.arrangeCards(null, immediate, 220);
  }

  public removeCard(card: Card) {
    const index = this.cards.indexOf(card);
    if (index > -1) {
      this.cards.splice(index, 1);
      this.arrangeCards();
    }
  }

  // Visual order is deliberately owned by Hand, not GameModel: arranging a
  // hand is private presentation and must never alter authoritative cards.
  public orderIds(): string[] {
    return this.cards.map((card) => (card.mesh.userData.modelId as string) ?? "");
  }

  public setOrder(cardIds: readonly string[]) {
    const byId = new Map(
      this.cards.map((card) => [(card.mesh.userData.modelId as string) ?? "", card])
    );
    const ordered: Card[] = [];
    const seen = new Set<Card>();
    for (const id of cardIds) {
      const card = byId.get(id);
      if (card && !seen.has(card)) {
        ordered.push(card);
        seen.add(card);
      }
    }
    for (const card of this.cards) {
      if (!seen.has(card)) ordered.push(card);
    }
    this.cards = ordered;
    this.arrangeCards();
  }

  // Move one card while leaving it under the pointer. Neighbours animate to
  // their new fan slots and the floating card settles on pointer-up.
  public moveCard(card: Card, targetIndex: number): boolean {
    const currentIndex = this.cards.indexOf(card);
    if (currentIndex < 0) return false;
    const clamped = Math.max(0, Math.min(this.cards.length - 1, targetIndex));
    if (currentIndex === clamped) return false;
    this.cards.splice(currentIndex, 1);
    this.cards.splice(clamped, 0, card);
    this.arrangeCards(card);
    return true;
  }

  public closestSlotIndex(clientX: number, canvas: HTMLElement): number {
    if (this.cards.length === 0) return 0;
    const rect = canvas.getBoundingClientRect();
    let closestIndex = 0;
    let closestDistance = Number.POSITIVE_INFINITY;
    for (let index = 0; index < this.cards.length; index += 1) {
      const projected = this.computeLayout(index, this.cards.length).position.clone().project(this.camera);
      const slotX = rect.left + ((projected.x + 1) / 2) * rect.width;
      const distance = Math.abs(clientX - slotX);
      if (distance < closestDistance) {
        closestDistance = distance;
        closestIndex = index;
      }
    }
    return closestIndex;
  }

  // Canonical fan slot for a card index in a hand of `count` cards.
  // Fan with near-full card visibility: spacing adapts to hand size so
  // every rank stays readable (cards are 0.6 wide; leave ~0.07 overlap).
  private computeLayout(index: number, count: number): { position: THREE.Vector3; rotation: THREE.Euler } {
    const layout = this.fanSlot(index, count);
    const id = this.cards[index]?.mesh.userData.modelId as string | undefined;
    if (id !== undefined && this.liftedIds.has(id)) {
      // Straight up the screen: a tilted card rising along its own axis would
      // slide under its neighbour, right where the player is about to tap it.
      layout.position.addScaledVector(
        new THREE.Vector3(0, 1, 0).applyEuler(new THREE.Euler(layout.rotation.x, 0, 0)),
        LIFT * this.cardScale
      );
    }
    return layout;
  }

  private fanSlot(index: number, count: number): { position: THREE.Vector3; rotation: THREE.Euler } {
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

  public arrangeCards(floatingCard: Card | null = null, immediate = false, duration = 400) {
    const count = this.cards.length;
    this.cards.forEach((card, index) => {
      const layout = this.computeLayout(index, count);
      if (card === floatingCard) {
        card.mesh.traverse((obj) => {
          if (obj instanceof THREE.Mesh) obj.renderOrder = 100;
        });
        return;
      }
      card.setPosition(layout.position.x, layout.position.y, layout.position.z, immediate, duration);

      const renderOrder = index;
      card.mesh.traverse((obj) => {
        if (obj instanceof THREE.Mesh) {
          obj.renderOrder = renderOrder;
        }
      });

      card.setRotation(layout.rotation.x, layout.rotation.y, layout.rotation.z, immediate, duration);
    });
  }

  public getCards(): Card[] {
    return this.cards;
  }

  public cardScreenBounds(canvas: HTMLElement) {
    return this.cards.map((card) => ({
      id: (card.mesh.userData.modelId as string) ?? "",
      ...objectScreenBounds(card.mesh, this.camera, canvas),
    }));
  }

  // Where the top of the fan is on screen with every card in its slot, whether or not it has got there yet.
  public fanScreenTop(canvas: HTMLElement): number | null {
    if (this.cards.length === 0) return null;
    return Math.min(
      ...this.cards.map((card, index) => {
        const { position, rotation } = this.computeLayout(index, this.cards.length);
        const slot = new THREE.Group();
        slot.position.copy(position);
        slot.rotation.copy(rotation);
        slot.scale.setScalar(this.cardScale);
        slot.add(...card.mesh.children.map((child) => child.clone()));
        return objectScreenBounds(slot, this.camera, canvas).top;
      })
    );
  }
}
