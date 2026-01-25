import * as THREE from 'three';

export class Table {
  private scene: THREE.Scene;
  private shadowPlane: THREE.Mesh | null = null;

  constructor(scene: THREE.Scene) {
    this.scene = scene;
    this.createShadowPlane();
  }

  /**
   * Creates an invisible plane at y=0 that only receives shadows.
   * This gives floating cards their zen drop shadow effect.
   */
  private createShadowPlane() {
    // Large plane to catch all card shadows
    const geometry = new THREE.PlaneGeometry(20, 20);
    
    // Shadow-only material - subtle shadows for zen aesthetic
    const material = new THREE.ShadowMaterial({
      opacity: 0.15,
      color: 0x8b7355,
    });
    
    this.shadowPlane = new THREE.Mesh(geometry, material);
    this.shadowPlane.rotation.x = -Math.PI / 2; // Lay flat
    this.shadowPlane.position.y = -0.4; // Lower for floating card effect
    this.shadowPlane.receiveShadow = true;
    
    this.scene.add(this.shadowPlane);
  }

  public getShadowPlane(): THREE.Mesh | null {
    return this.shadowPlane;
  }
}
