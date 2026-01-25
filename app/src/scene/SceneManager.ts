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
      50, // FOV for balanced view
      window.innerWidth / window.innerHeight, // Aspect ratio
      0.1, // Near plane
      100 // Far plane
    );
    // Camera position to show hand arc prominently at bottom, play area in center
    this.camera.position.set(0, 5, 4.5);
    this.camera.lookAt(0, 0, 0);

    // Renderer setup
    this.renderer = new THREE.WebGLRenderer({
      antialias: true,
      powerPreference: "high-performance",
      alpha: false // No transparency needed, cream background
    });
    this.renderer.setSize(window.innerWidth, window.innerHeight);
    this.renderer.setPixelRatio(Math.min(window.devicePixelRatio, 2));
    
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

    this.camera.aspect = width / height;
    this.camera.updateProjectionMatrix();

    this.renderer.setSize(width, height);
    this.renderer.setPixelRatio(Math.min(window.devicePixelRatio, 2));
  }

  public update() {
    TWEEN.update();
    this.renderer.render(this.scene, this.camera);
  }
}
