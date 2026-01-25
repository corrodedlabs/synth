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
    
    // Shadow-only material - invisible but receives shadows
    const material = new THREE.ShadowMaterial({
      opacity: 0.15, // Subtle shadow for zen aesthetic
    });
    
    this.shadowPlane = new THREE.Mesh(geometry, material);
    this.shadowPlane.rotation.x = -Math.PI / 2; // Lay flat
    this.shadowPlane.position.y = 0; // At ground level
    this.shadowPlane.receiveShadow = true;
    
    this.scene.add(this.shadowPlane);
  }
}
