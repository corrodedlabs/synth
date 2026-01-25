import * as THREE from 'three';

export class Lighting {
  private scene: THREE.Scene;
  private ambientLight: THREE.AmbientLight | null = null;
  private directionalLight: THREE.DirectionalLight | null = null;
  private hemisphereLight: THREE.HemisphereLight | null = null;

  constructor(scene: THREE.Scene) {
    this.scene = scene;
    this.setupLights();
    this.setupBackground();
  }

  private setupLights() {
    // Soft ambient fill - not too bright, allows shadows to have contrast
    this.ambientLight = new THREE.AmbientLight(0xfff8f0, 0.5);
    this.scene.add(this.ambientLight);

    // Hemisphere light for natural sky/ground gradient
    this.hemisphereLight = new THREE.HemisphereLight(0xffffff, 0xf0ebe0, 0.35);
    this.scene.add(this.hemisphereLight);

    // Main directional light - high above for soft, diffuse shadows
    this.directionalLight = new THREE.DirectionalLight(0xfffaf5, 0.5);
    this.directionalLight.position.set(0, 15, 2.5);
    this.directionalLight.castShadow = true;
    
    // Shadow configuration for soft, realistic shadows
    this.directionalLight.shadow.mapSize.width = 4096;
    this.directionalLight.shadow.mapSize.height = 4096;
    this.directionalLight.shadow.camera.near = 1;
    this.directionalLight.shadow.camera.far = 30;
    this.directionalLight.shadow.camera.left = -10;
    this.directionalLight.shadow.camera.right = 10;
    this.directionalLight.shadow.camera.top = 10;
    this.directionalLight.shadow.camera.bottom = -10;
    this.directionalLight.shadow.bias = -0.002;
    this.directionalLight.shadow.normalBias = 0.06;
    this.directionalLight.shadow.radius = 35;
    
    this.scene.add(this.directionalLight);
  }

  private setupBackground() {
    // Transparent background so CSS gradient shows through
    this.scene.background = null;
    this.scene.fog = null;
  }

  // Keeping this method for API compatibility, but it's a no-op now
  public setSpotlightTarget(_position: THREE.Vector3) {
    // No spotlight in zen style
  }

  public getDirectionalLight(): THREE.DirectionalLight | null {
    return this.directionalLight;
  }
}
