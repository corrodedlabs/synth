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
  private positionTween: TWEEN.Tween<THREE.Vector3> | TWEEN.Tween<{ progress: number }> | null = null;
  private bounceTween: TWEEN.Tween<{ progress: number }> | null = null;
  private bounceBaseScale = 1;
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

  // True while the card is travelling, bouncing or turning. Tests measure cards only once it's false.
  public isMoving(): boolean {
    return this.positionTween !== null || this.bounceTween !== null || this.rotationTween !== null;
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

  // A throw: the card rises off the table on its way across and comes down
  // onto the target. Stopping it (any later setPosition) simply leaves the card
  // to its new orders, so onLand only fires for a card that really landed.
  public arcTo(
    x: number,
    y: number,
    z: number,
    duration: number,
    height: number,
    onLand?: () => void
  ) {
    this.positionTween?.stop();
    const from = this.mesh.position.clone();
    const to = new THREE.Vector3(x, y, z);
    this.positionTween = new TWEEN.Tween({ progress: 0 })
      .to({ progress: 1 }, duration)
      .easing(TWEEN.Easing.Cubic.Out)
      .onUpdate(({ progress }) => {
        this.mesh.position.lerpVectors(from, to, progress);
        this.mesh.position.y += height * 4 * progress * (1 - progress);
      })
      .onComplete(() => {
        this.positionTween = null;
        this.mesh.position.copy(to);
        onLand?.();
      })
      .start();
  }

  // The small squash a card makes as it lands. Always ends back at the scale
  // it started from, including when it is stopped early.
  public landingBounce(duration: number) {
    this.stopBounce();
    if (duration <= 0) return;
    this.bounceBaseScale = this.mesh.scale.x;
    this.bounceTween = new TWEEN.Tween({ progress: 0 })
      .to({ progress: 1 }, duration)
      .onUpdate(({ progress }) => {
        const squash =
          progress < 0.4
            ? 1 + 0.07 * (progress / 0.4)
            : progress < 0.75
              ? 1.07 - 0.1 * ((progress - 0.4) / 0.35)
              : 0.97 + 0.03 * ((progress - 0.75) / 0.25);
        this.mesh.scale.setScalar(this.bounceBaseScale * squash);
      })
      .onComplete(() => {
        this.bounceTween = null;
        this.mesh.scale.setScalar(this.bounceBaseScale);
      })
      .start();
  }

  public stopBounce() {
    if (!this.bounceTween) return;
    this.bounceTween.stop();
    this.bounceTween = null;
    this.mesh.scale.setScalar(this.bounceBaseScale);
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
    this.stopBounce(); // the winner's scale must be measured from the true size
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
