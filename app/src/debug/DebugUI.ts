import GUI from 'lil-gui';
import * as THREE from 'three';
import { Lighting } from '../scene/Lighting';
import { Table } from '../scene/Table';
import { Hand } from '../game/Hand';

export class DebugUI {
  private gui: GUI;
  private lighting: Lighting;
  private table: Table;
  private hand: Hand | null;

  constructor(lighting: Lighting, table: Table, hand?: Hand) {
    this.lighting = lighting;
    this.table = table;
    this.hand = hand || null;

    this.gui = new GUI({ title: 'Shadow Debug' });
    this.setupDirectionalLightFolder();
    this.setupTableShadowFolder();
    
    // Delay card folder setup since cards are loaded asynchronously
    if (this.hand) {
      setTimeout(() => this.setupLastCardFolder(), 1500);
    }
  }

  private setupDirectionalLightFolder() {
    const light = this.lighting.getDirectionalLight();
    if (!light) return;

    const folder = this.gui.addFolder('Directional Light');

    const posFolder = folder.addFolder('Position');
    posFolder.add(light.position, 'x', -20, 20, 0.1).name('X');
    posFolder.add(light.position, 'y', 0, 20, 0.1).name('Y');
    posFolder.add(light.position, 'z', -20, 20, 0.1).name('Z');

    const shadowFolder = folder.addFolder('Shadow');
    shadowFolder.add(light.shadow, 'radius', 0, 20, 0.1).name('Radius');
    shadowFolder.add(light.shadow, 'bias', -0.01, 0.01, 0.0001).name('Bias');
    shadowFolder.add(light.shadow, 'normalBias', 0, 0.1, 0.001).name('Normal Bias');
    shadowFolder.add(light, 'intensity', 0, 2, 0.01).name('Intensity');
  }

  private setupTableShadowFolder() {
    const shadowPlane = this.table.getShadowPlane();
    if (!shadowPlane) return;

    const folder = this.gui.addFolder('Table Shadow Plane');

    const material = shadowPlane.material as THREE.ShadowMaterial;
    if (material) {
      folder.add(material, 'opacity', 0, 1, 0.01).name('Opacity');
    }

    folder.add(shadowPlane.position, 'y', -1, 1, 0.001).name('Y Position');
    folder.add(shadowPlane, 'visible').name('Visible');
  }

  private setupLastCardFolder() {
    if (!this.hand) return;
    
    const cards = this.hand.getCards();
    if (cards.length === 0) return;
    
    const lastCard = cards[cards.length - 1];
    const folder = this.gui.addFolder('Last Card Position');
    
    folder.add(lastCard.mesh.position, 'x', -5, 5, 0.1).name('X');
    folder.add(lastCard.mesh.position, 'y', -5, 5, 0.1).name('Y');
    folder.add(lastCard.mesh.position, 'z', -10, 30, 0.1).name('Z');
  }

  public destroy() {
    this.gui.destroy();
  }
}
