import * as THREE from 'three';
import * as TWEEN from '@tweenjs/tween.js';

export class SceneManager {
  public scene: THREE.Scene;
  public camera: THREE.PerspectiveCamera;
  public renderer: THREE.WebGLRenderer;
  private resizeObserver: ResizeObserver;

  constructor(canvasContainer: HTMLElement) {
    // Scene setup
    this.scene = new THREE.Scene();

    // Camera setup - Position to show hand prominently in lower third
    this.camera = new THREE.PerspectiveCamera(
      38, // Narrower FOV for more elegant framing like mockup
      window.innerWidth / window.innerHeight, // Aspect ratio
      0.1, // Near plane
      100 // Far plane
    );
    // Camera position to show hand arc prominently at bottom, play area in center
    // Raised and pulled back to show full scene composition
    this.updateCameraForViewport();

    // Renderer setup
    this.renderer = new THREE.WebGLRenderer({
      antialias: true,
      powerPreference: "high-performance",
      alpha: true // Allow CSS gradient background to show through
    });
    this.renderer.setSize(window.innerWidth, window.innerHeight);
    this.renderer.setPixelRatio(Math.min(window.devicePixelRatio, 2));
    this.renderer.setClearColor(0x000000, 0);
    
    // Enable shadow mapping for floating card shadows
    this.renderer.shadowMap.enabled = true;
    this.renderer.shadowMap.type = THREE.PCFSoftShadowMap;
    
    canvasContainer.appendChild(this.renderer.domElement);

    // Handle resize
    this.resizeObserver = new ResizeObserver(() => this.onResize());
    this.resizeObserver.observe(canvasContainer);
    window.addEventListener('resize', () => this.onResize());
  }

  private onResize() {
    const width = window.innerWidth;
    const height = window.innerHeight;

    this.updateCameraForViewport();

    this.renderer.setSize(width, height);
    this.renderer.setPixelRatio(Math.min(window.devicePixelRatio, 2));
  }

  // The default framing is tuned for landscape. Portrait screens get a wider
  // FOV and a camera pulled up/back so the hand fan and trick stay in frame.
  private updateCameraForViewport() {
    const aspect = window.innerWidth / window.innerHeight;
    this.camera.aspect = aspect;
    if (aspect < 1) {
      const portrait = THREE.MathUtils.clamp(1 / aspect, 1, 2.2);
      this.camera.fov = Math.min(38 * portrait, 70);
      this.camera.position.set(0, 5.9 + 0.8 * (portrait - 1), 6.4 + 1.6 * (portrait - 1));
    } else {
      this.camera.fov = 38;
      this.camera.position.set(0, 5.9, 6.4);
    }
    this.camera.lookAt(0, 0.15, 0.1);
    this.camera.updateProjectionMatrix();
  }

  public update() {
    TWEEN.update();
    this.renderer.render(this.scene, this.camera);
  }
}
