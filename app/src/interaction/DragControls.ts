import * as THREE from 'three';
import { Card } from '../game/Card';
import { Hand } from '../game/Hand';
import { PlayArea } from '../game/PlayArea';

export class DragControls {
  private camera: THREE.Camera;
  // @ts-ignore
  private scene: THREE.Scene;
  private canvas: HTMLCanvasElement;
  private hand: Hand;
  private playArea: PlayArea;
  
  private raycaster = new THREE.Raycaster();
  private mouse = new THREE.Vector2();
  // @ts-ignore
  private selectedObject: THREE.Object3D | null = null;
  private selectedCard: Card | null = null;
  private isDragging = false;
  // @ts-ignore
  private plane = new THREE.Plane(new THREE.Vector3(0, 1, 0), 0); // Ground plane for dragging intersection

  constructor(camera: THREE.Camera, scene: THREE.Scene, canvas: HTMLCanvasElement, hand: Hand, playArea: PlayArea) {
    this.camera = camera;
    this.scene = scene;
    this.canvas = canvas;
    this.hand = hand;
    this.playArea = playArea;

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

  private getIntersects(event: MouseEvent | TouchEvent, objects: THREE.Object3D[]) {
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
    return this.raycaster.intersectObjects(objects, true);
  }

  private onDown(event: MouseEvent | TouchEvent) {
    event.preventDefault();

    // Get all card meshes from the hand
    const cardMeshes = this.hand.getCards().map(c => c.mesh);
    const intersects = this.getIntersects(event, cardMeshes);

    if (intersects.length > 0) {
      // Find the root Group of the card
      let obj = intersects[0].object;
      while (obj.parent && obj.parent.type !== 'Scene' && !obj.userData.card) {
        obj = obj.parent;
      }
      
      if (obj.userData.card) {
        this.selectedObject = obj;
        this.selectedCard = obj.userData.card as Card;
        this.isDragging = true;
        
        // Lift card slightly
        // this.selectedCard.highlight(true);
      }
    }
  }

  private onMove(event: MouseEvent | TouchEvent) {
    if (!this.isDragging || !this.selectedCard) return;
    event.preventDefault();

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
      // Flatten rotation while dragging
      this.selectedCard.setRotation(-Math.PI/2, 0, 0, true);
    }
  }

  private onUp(event: MouseEvent | TouchEvent) {
    if (!this.isDragging || !this.selectedCard) return;
    event.preventDefault();

    this.isDragging = false;

    // Check if dropped in play area (e.g. z < 1.0)
    const currentPos = this.selectedCard.mesh.position;
    
    if (currentPos.z < 1.5 && currentPos.z > -1.5 && Math.abs(currentPos.x) < 2.0) {
      // Valid play
      this.hand.removeCard(this.selectedCard);
      this.playArea.playCard(this.selectedCard, 0); // Player is index 0
      this.selectedCard = null;
      this.selectedObject = null;
    } else {
      // Return to hand
      this.hand.arrangeCards();
      this.selectedCard = null;
      this.selectedObject = null;
    }
  }
}
