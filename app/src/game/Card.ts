import * as THREE from 'three';
import * as TWEEN from '@tweenjs/tween.js';

export class Card {
  public mesh: THREE.Group;
  public suit: string;
  public rank: string;
  public isSelected: boolean = false;
  public isDraggable: boolean = true;

  private frontMaterial: THREE.MeshStandardMaterial;
  private backMaterial: THREE.MeshStandardMaterial;
  private sideMaterial: THREE.MeshStandardMaterial;
  private dimmed = false;
  private positionTween: TWEEN.Tween<THREE.Vector3> | null = null;
  private rotationTween: TWEEN.Tween<THREE.Euler> | null = null;
  private winnerTween: TWEEN.Tween<THREE.Vector3> | null = null;
  private winnerBaseScale: THREE.Vector3 | null = null;

  constructor(suit: string, rank: string, frontTexture: THREE.Texture, backTexture: THREE.Texture) {
    this.suit = suit;
    this.rank = rank;
    this.mesh = new THREE.Group();

    // Thin paper-like geometry for zen aesthetic
    const geometry = new THREE.BoxGeometry(0.6, 0.9, 0.005);
    
    // Matte paper materials (roughness=1, metalness=0)
    this.frontMaterial = new THREE.MeshStandardMaterial({ 
      map: frontTexture,
      roughness: 1.0,
      metalness: 0.0
    });
    
    this.backMaterial = new THREE.MeshStandardMaterial({ 
      map: backTexture,
      roughness: 1.0,
      metalness: 0.0
    });

    // Cream colored sides to match zen style
    this.sideMaterial = new THREE.MeshStandardMaterial({
      color: 0xf5f0e6,
      roughness: 1.0,
      metalness: 0.0
    });

    // Mesh with multi-material
    const mesh = new THREE.Mesh(geometry, [
      this.sideMaterial, // Right
      this.sideMaterial, // Left
      this.sideMaterial, // Top
      this.sideMaterial, // Bottom
      this.frontMaterial, // Front
      this.backMaterial  // Back
    ]);
    
    // Enable shadow casting for floating card effect
    mesh.castShadow = true;
    mesh.receiveShadow = false; // Cards don't receive shadows on themselves
    
    this.mesh.add(mesh);
    this.mesh.userData = { card: this }; // Link back to Card instance
  }

  public setPosition(x: number, y: number, z: number, immediate = false, duration = 500) {
    // Cancel any in-flight tween so stale targets can't drag the card back
    if (this.positionTween) {
      this.positionTween.stop();
      this.positionTween = null;
    }
    if (immediate) {
      this.mesh.position.set(x, y, z);
    } else {
      this.positionTween = new TWEEN.Tween(this.mesh.position)
        .to({ x, y, z }, duration)
        .easing(TWEEN.Easing.Quadratic.Out)
        .onComplete(() => {
          this.positionTween = null;
        })
        .start();
    }
  }

  public setRotation(x: number, y: number, z: number, immediate = false, duration = 500) {
    if (this.rotationTween) {
      this.rotationTween.stop();
      this.rotationTween = null;
    }
    if (immediate) {
      this.mesh.rotation.set(x, y, z);
    } else {
      this.rotationTween = new TWEEN.Tween(this.mesh.rotation)
        .to({ x, y, z }, duration)
        .easing(TWEEN.Easing.Quadratic.Out)
        .onComplete(() => {
          this.rotationTween = null;
        })
        .start();
    }
  }

  public highlight(active: boolean) {
    if (active) {
      this.mesh.position.y += 0.15; // Subtle lift
    } else {
      this.mesh.position.y -= 0.15;
    }
  }

  // Unplayable cards fade into the felt while the table waits on us; the
  // material color multiplies the face texture, so no texture work needed.
  public setDimmed(dimmed: boolean) {
    if (this.dimmed === dimmed) return;
    this.dimmed = dimmed;
    const shade = dimmed ? 0x8f8a82 : 0xffffff;
    this.frontMaterial.color.setHex(shade);
    this.backMaterial.color.setHex(shade);
    this.sideMaterial.color.setHex(dimmed ? 0xa8a298 : 0xf5f0e6);
  }

  public isDimmed(): boolean {
    return this.dimmed;
  }

  // The winning card needs a brief, readable identity before its trick is
  // collected. Emissive material works with every suit and does not cover a
  // rank or depend on a color-only signal.
  public setTrickWinner(active: boolean, duration = 180) {
    if (this.winnerTween) {
      this.winnerTween.stop();
      this.winnerTween = null;
    }

    const highlighted = 0xdec77a;
    this.frontMaterial.emissive.setHex(active ? highlighted : 0x000000);
    this.backMaterial.emissive.setHex(active ? highlighted : 0x000000);
    this.sideMaterial.emissive.setHex(active ? highlighted : 0x000000);
    this.frontMaterial.emissiveIntensity = active ? 0.36 : 0;
    this.backMaterial.emissiveIntensity = active ? 0.2 : 0;
    this.sideMaterial.emissiveIntensity = active ? 0.14 : 0;

    if (active) {
      this.winnerBaseScale = this.mesh.scale.clone();
      const target = this.winnerBaseScale.clone().multiplyScalar(1.12);
      if (duration === 0) {
        this.mesh.scale.copy(target);
        return;
      }
      this.winnerTween = new TWEEN.Tween(this.mesh.scale)
        .to({ x: target.x, y: target.y, z: target.z }, duration)
        .easing(TWEEN.Easing.Back.Out)
        .onComplete(() => {
          this.winnerTween = null;
        })
        .start();
      return;
    }

    const target = this.winnerBaseScale;
    this.winnerBaseScale = null;
    if (target === null) return;
    if (duration === 0) {
      this.mesh.scale.copy(target);
      return;
    }
    this.winnerTween = new TWEEN.Tween(this.mesh.scale)
      .to({ x: target.x, y: target.y, z: target.z }, duration)
      .easing(TWEEN.Easing.Quadratic.Out)
      .onComplete(() => {
        this.winnerTween = null;
      })
      .start();
  }

  // Clears both card movement and the winner emphasis when a snapshot, leave,
  // or fresh deal replaces the current presentation.
  public cancelPresentation() {
    this.positionTween?.stop();
    this.rotationTween?.stop();
    this.winnerTween?.stop();
    this.positionTween = null;
    this.rotationTween = null;
    this.winnerTween = null;
    this.setTrickWinner(false, 0);
  }

  public flip() {
    new TWEEN.Tween(this.mesh.rotation)
      .to({ y: this.mesh.rotation.y + Math.PI }, 600)
      .easing(TWEEN.Easing.Back.Out)
      .start();
  }
}
