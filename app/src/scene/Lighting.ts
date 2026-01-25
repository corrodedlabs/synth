import * as THREE from 'three';

export class Lighting {
  private scene: THREE.Scene;
  private ambientLight: THREE.AmbientLight | null = null;
  private directionalLight: THREE.DirectionalLight | null = null;

  constructor(scene: THREE.Scene) {
    this.scene = scene;
    this.setupLights();
    this.setupBackground();
  }

  private setupLights() {
    // Soft white ambient light for zen aesthetic
    this.ambientLight = new THREE.AmbientLight(0xffffff, 0.8);
    this.scene.add(this.ambientLight);

    // Soft directional light for subtle shadows
    this.directionalLight = new THREE.DirectionalLight(0xffffff, 0.6);
    this.directionalLight.position.set(2, 8, 4);
    this.directionalLight.castShadow = true;
    
    // Shadow map configuration for soft card shadows
    this.directionalLight.shadow.mapSize.width = 2048;
    this.directionalLight.shadow.mapSize.height = 2048;
    this.directionalLight.shadow.camera.near = 0.5;
    this.directionalLight.shadow.camera.far = 20;
    this.directionalLight.shadow.camera.left = -5;
    this.directionalLight.shadow.camera.right = 5;
    this.directionalLight.shadow.camera.top = 5;
    this.directionalLight.shadow.camera.bottom = -5;
    this.directionalLight.shadow.bias = -0.0005;
    this.directionalLight.shadow.radius = 4; // Soft shadow edges
    
    this.scene.add(this.directionalLight);
  }

  private setupBackground() {
    // Cream background for zen aesthetic (#faf8f5)
    const creamColor = new THREE.Color(0xfaf8f5);
    this.scene.background = creamColor;
    // No fog - clean minimalist look
    this.scene.fog = null;
  }

  // Keeping this method for API compatibility, but it's a no-op now
  public setSpotlightTarget(_position: THREE.Vector3) {
    // No spotlight in zen style
  }
}
