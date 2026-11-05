import * as THREE from 'three';
import { Card } from '../game/Card';
import { Hand } from '../game/Hand';

export interface DragGate {
  // Arrangement is explicit so its gestures can never become card plays.
  mode(): "inactive" | "play" | "arrange";
  // Whether this specific card would be a legal play right now.
  canPlay(card: Card): boolean;
  // Attempt to play the dropped card; false means it returns to the hand.
  tryPlay(card: Card): boolean;
  // The visual order changed and can now be persisted by GameState.
  arrangementChanged(): void;
}

export class DragControls {
  private camera: THREE.Camera;
  // @ts-ignore
  private scene: THREE.Scene;
  private canvas: HTMLCanvasElement;
  private hand: Hand;
  private gate: DragGate;

  private raycaster = new THREE.Raycaster();
  private mouse = new THREE.Vector2();
  // @ts-ignore
  private selectedObject: THREE.Object3D | null = null;
  private selectedCard: Card | null = null;
  private isDragging = false;
  // @ts-ignore
  private plane = new THREE.Plane(new THREE.Vector3(0, 1, 0), 0); // Ground plane for dragging intersection

  private hoveredCard: Card | null = null;
  private downPoint = new THREE.Vector2();
  private dragMoved = false;
  private selectedMode: "play" | "arrange" | null = null;
  // Touch plays are two-step: the first tap raises the card, a second tap
  // on the same card commits it. Mouse keeps direct click-to-play.
  private armedCard: Card | null = null;

  constructor(
    camera: THREE.Camera,
    scene: THREE.Scene,
    canvas: HTMLCanvasElement,
    hand: Hand,
    gate: DragGate
  ) {
    this.camera = camera;
    this.scene = scene;
    this.canvas = canvas;
    this.hand = hand;
    this.gate = gate;

    this.initEvents();
  }

  private initEvents() {
    // Touch events
    this.canvas.addEventListener('touchstart', this.onDown.bind(this), { passive: false });
    this.canvas.addEventListener('touchmove', this.onMove.bind(this), { passive: false });
    this.canvas.addEventListener('touchend', this.onUp.bind(this), { passive: false });

    // Mouse events (for desktop testing)
    this.canvas.addEventListener('mousedown', this.onDown.bind(this));
    this.canvas.addEventListener('mousemove', this.onMove.bind(this));
    this.canvas.addEventListener('mouseup', this.onUp.bind(this));
  }

  // The hand card a press at this page point would pick, by the same rules a
  // finger gets. Lets automated tests aim at the part of a card that shows.
  public cardAt(clientX: number, clientY: number): Card | null {
    const cardMeshes = this.hand.getCards().map((c) => c.mesh);
    return this.topmostCard(this.intersectsAt(clientX, clientY, cardMeshes));
  }

  private getIntersects(event: MouseEvent | TouchEvent, objects: THREE.Object3D[]) {
    let clientX, clientY;

    if (window.TouchEvent && event instanceof TouchEvent) {
      clientX = event.touches[0].clientX;
      clientY = event.touches[0].clientY;
    } else {
      clientX = (event as MouseEvent).clientX;
      clientY = (event as MouseEvent).clientY;
    }

    return this.intersectsAt(clientX, clientY, objects);
  }

  private intersectsAt(clientX: number, clientY: number, objects: THREE.Object3D[]) {
    const rect = this.canvas.getBoundingClientRect();
    this.mouse.x = ((clientX - rect.left) / rect.width) * 2 - 1;
    this.mouse.y = -((clientY - rect.top) / rect.height) * 2 + 1;

    this.raycaster.setFromCamera(this.mouse, this.camera);
    return this.raycaster.intersectObjects(objects, true);
  }

  // Raycaster distance does not account for renderOrder. In an overlapped
  // fan, choose the visually topmost intersected card (the highest hand
  // index) so the card the player sees is the card they pick up.
  private topmostCard(intersects: THREE.Intersection<THREE.Object3D>[]): Card | null {
    const handCards = this.hand.getCards();
    let topmost: Card | null = null;
    let topmostIndex = -1;
    for (const intersection of intersects) {
      let object = intersection.object;
      while (object.parent && object.parent.type !== "Scene" && !object.userData.card) {
        object = object.parent;
      }
      const card = object.userData.card as Card | undefined;
      // an armed card is drawn in front of the whole fan, whatever its index
      if (card && card === this.armedCard) return card;
      const index = card ? handCards.indexOf(card) : -1;
      if (card && index > topmostIndex) {
        topmost = card;
        topmostIndex = index;
      }
    }
    return topmost;
  }

  private onDown(event: MouseEvent | TouchEvent) {
    event.preventDefault();

    const mode = this.gate.mode();
    if (mode === "inactive") return;

    // Get all card meshes from the hand
    const cardMeshes = this.hand.getCards().map(c => c.mesh);
    const intersects = this.getIntersects(event, cardMeshes);

    if (intersects.length > 0) {
      const card = this.topmostCard(intersects);
      if (card) {
        this.selectedObject = card.mesh;
        this.selectedCard = card;
        this.isDragging = true;
        this.selectedMode = mode;
        this.dragMoved = false;
        this.downPoint.copy(this.pointerPosition(event));

        this.clearHover();
      }
    } else {
      // a tap on empty felt stands the armed card back down
      this.disarm();
    }
  }

  private pointerPosition(event: MouseEvent | TouchEvent): THREE.Vector2 {
    if (window.TouchEvent && event instanceof TouchEvent) {
      const touch = event.touches[0] ?? event.changedTouches[0];
      return new THREE.Vector2(touch.clientX, touch.clientY);
    }
    return new THREE.Vector2((event as MouseEvent).clientX, (event as MouseEvent).clientY);
  }

  private onMove(event: MouseEvent | TouchEvent) {
    if (this.gate.mode() === "play") this.updateHover(event);
    else this.clearHover();

    if (!this.isDragging || !this.selectedCard) return;
    event.preventDefault();

    // A press only becomes a drag after the pointer moves a little;
    // otherwise releasing counts as a tap (click-to-play).
    if (!this.dragMoved && this.pointerPosition(event).distanceTo(this.downPoint) < 8) return;
    this.dragMoved = true;

    if (this.selectedMode === "arrange") {
      const pointer = this.pointerPosition(event);
      const targetIndex = this.hand.closestSlotIndex(pointer.x, this.canvas);
      this.hand.moveCard(this.selectedCard, targetIndex);

      const layout = this.hand.layoutOf(this.selectedCard);
      if (layout) {
        const rect = this.canvas.getBoundingClientRect();
        this.mouse.x = ((pointer.x - rect.left) / rect.width) * 2 - 1;
        this.mouse.y = -((pointer.y - rect.top) / rect.height) * 2 + 1;
        this.raycaster.setFromCamera(this.mouse, this.camera);
        const dragPlane = new THREE.Plane(new THREE.Vector3(0, 1, 0), -(layout.position.y + 0.15));
        const target = new THREE.Vector3();
        if (this.raycaster.ray.intersectPlane(dragPlane, target)) {
          this.selectedCard.setPosition(target.x, layout.position.y + 0.15, layout.position.z - 0.18, true);
          this.selectedCard.setRotation(layout.rotation.x, layout.rotation.y, layout.rotation.z, true);
        }
      }
      return;
    }

    // Raycast to a virtual plane at card height to drag it around
    const rect = this.canvas.getBoundingClientRect();
    let clientX, clientY;
    if (window.TouchEvent && event instanceof TouchEvent) {
      clientX = event.touches[0].clientX;
      clientY = event.touches[0].clientY;
    } else {
      clientX = (event as MouseEvent).clientX;
      clientY = (event as MouseEvent).clientY;
    }

    this.mouse.x = ((clientX - rect.left) / rect.width) * 2 - 1;
    this.mouse.y = -((clientY - rect.top) / rect.height) * 2 + 1;
    
    this.raycaster.setFromCamera(this.mouse, this.camera);
    
    // Create a temporary plane at the card's current Y or slightly above table
    const dragPlane = new THREE.Plane(new THREE.Vector3(0, 1, 0), -0.5); 
    const target = new THREE.Vector3();
    this.raycaster.ray.intersectPlane(dragPlane, target);

    if (target) {
      // Limit drag area (simple bounds)
      target.x = Math.max(-2, Math.min(2, target.x));
      target.z = Math.max(-1, Math.min(4, target.z));
      
      this.selectedCard.setPosition(target.x, 0.5, target.z, true);
      // Keep the card facing the camera while dragging
      const toCamera = this.camera.position.clone().sub(this.selectedCard.mesh.position);
      this.selectedCard.setRotation(-Math.atan2(toCamera.y, toCamera.z), 0, 0, true);
    }
  }

  // Returns a hovered card to its slot in the fan. The slot is computed
  // fresh — the hand may have been re-arranged (e.g. by the second deal)
  // while the card was lifted.
  private unhover(card: Card) {
    const layout = this.hand.layoutOf(card);
    if (layout) {
      card.setPosition(layout.position.x, layout.position.y, layout.position.z, false, 200);
      const renderOrder = this.hand.getCards().indexOf(card);
      card.mesh.traverse((obj) => {
        if (obj instanceof THREE.Mesh) {
          obj.renderOrder = renderOrder;
        }
      });
    }
  }

  private clearHover() {
    if (this.hoveredCard) {
      this.unhover(this.hoveredCard);
      this.hoveredCard = null;
    }
  }

  private updateHover(event: MouseEvent | TouchEvent) {
    if (this.isDragging) return;

    const cardMeshes = this.hand.getCards().map(c => c.mesh);
    const intersects = this.getIntersects(event, cardMeshes);

    let newHovered: Card | null = null;
    if (intersects.length > 0) {
      newHovered = this.topmostCard(intersects);
    }

    if (newHovered !== this.hoveredCard) {
      if (this.hoveredCard) {
        this.unhover(this.hoveredCard);
      }

      this.hoveredCard = newHovered;

      if (this.hoveredCard) {
        // Lift from the card's canonical fan slot toward the camera
        const layout = this.hand.layoutOf(this.hoveredCard);
        const basePos = layout ? layout.position : this.hoveredCard.mesh.position.clone();
        const toCamera = this.camera.position.clone().sub(basePos).normalize();
        const targetPos = basePos.clone().addScaledVector(toCamera, 0.4);

        this.hoveredCard.setPosition(targetPos.x, targetPos.y, targetPos.z, false, 200);

        // Render on top while lifted
        this.hoveredCard.mesh.traverse((obj) => {
          if (obj instanceof THREE.Mesh) {
            obj.renderOrder = 100;
          }
        });
      }
    }
  }

  // Raise a card out of the fan so it reads as "about to be played".
  private raise(card: Card) {
    const layout = this.hand.layoutOf(card);
    const basePos = layout ? layout.position : card.mesh.position.clone();
    const toCamera = this.camera.position.clone().sub(basePos).normalize();
    const targetPos = basePos.clone().addScaledVector(toCamera, 0.55);
    card.setPosition(targetPos.x, targetPos.y, targetPos.z, false, 160);
    card.mesh.traverse((obj) => {
      if (obj instanceof THREE.Mesh) {
        obj.renderOrder = 100;
      }
    });
  }

  private disarm() {
    if (this.armedCard) {
      this.unhover(this.armedCard);
      this.armedCard = null;
    }
  }

  // The table moved on (request answered, new deal, room left): nothing
  // should stay raised or armed.
  public clearSelection() {
    if (this.selectedMode === "arrange") this.hand.arrangeCards();
    this.isDragging = false;
    this.selectedCard = null;
    this.selectedObject = null;
    this.selectedMode = null;
    this.disarm();
    this.clearHover();
  }

  private onUp(event: MouseEvent | TouchEvent) {
    if (!this.isDragging || !this.selectedCard) return;
    event.preventDefault();

    this.isDragging = false;
    const card = this.selectedCard;
    const mode = this.selectedMode;
    this.selectedCard = null;
    this.selectedObject = null;
    this.selectedMode = null;

    if (mode === "arrange") {
      this.hand.arrangeCards();
      if (this.dragMoved) this.gate.arrangementChanged();
      return;
    }

    if (!this.dragMoved) {
      const isTouch = window.TouchEvent && event instanceof TouchEvent;
      if (isTouch) {
        // fat fingers get a confirmation step: first tap arms, second plays
        if (this.armedCard === card) {
          this.armedCard = null;
          if (!this.gate.tryPlay(card)) this.hand.arrangeCards();
        } else if (this.gate.canPlay(card)) {
          this.disarm();
          this.armedCard = card;
          this.raise(card);
        } else {
          // unplayable (it is dimmed): stand everything back down
          this.disarm();
          this.hand.arrangeCards();
        }
        return;
      }
      // mouse click plays directly (legality is checked by the gate); a
      // failed click must not leave a touch-armed card primed and flat
      this.disarm();
      if (!this.gate.tryPlay(card)) {
        this.hand.arrangeCards();
      }
      return;
    }

    // Generous drop zone: anywhere forward of the hand counts as a play
    this.disarm();
    const currentPos = card.mesh.position;
    const inPlayArea = currentPos.z < 2.0 && currentPos.z > -1.8 && Math.abs(currentPos.x) < 2.4;
    if (!inPlayArea || !this.gate.tryPlay(card)) {
      // Not a play (or an illegal one) — return the card to the hand
      this.hand.arrangeCards();
    }
  }
}
