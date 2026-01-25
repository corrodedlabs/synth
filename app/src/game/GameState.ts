import { SceneManager } from '../scene/SceneManager';
import { Lighting } from '../scene/Lighting';
import { Table } from '../scene/Table';
import { Hand } from './Hand';
import { PlayArea } from './PlayArea';
import { ZenCardTextures } from '../utils/ZenCardTextures';
import { Card } from './Card';
import { DragControls } from '../interaction/DragControls';
import * as THREE from 'three';

export class GameState {
  private sceneManager: SceneManager;
  // @ts-ignore
  private lighting: Lighting;
  // @ts-ignore
  private table: Table;
  private hand: Hand;
  private playArea: PlayArea;
  private zenTextures: ZenCardTextures;
  // @ts-ignore
  private dragControls: DragControls;
  
  // Score tracking
  private score: number = 0;
  private tricks: number = 0;
  
  // HTML score elements
  private scoreElement: HTMLElement | null = null;
  private tricksElement: HTMLElement | null = null;

  constructor(sceneManager: SceneManager) {
    this.sceneManager = sceneManager;
    
    // Initialize subsystems
    this.lighting = new Lighting(this.sceneManager.scene);
    this.table = new Table(this.sceneManager.scene);
    this.hand = new Hand(this.sceneManager.scene);
    this.playArea = new PlayArea(this.sceneManager.scene);
    this.zenTextures = new ZenCardTextures();

    // Controls
    this.dragControls = new DragControls(
      this.sceneManager.camera, 
      this.sceneManager.scene, 
      this.sceneManager.renderer.domElement,
      this.hand,
      this.playArea
    );

    // Get HTML score display elements
    this.scoreElement = document.getElementById('score-value');
    this.tricksElement = document.getElementById('tricks-value');
    
    // Wait for textures to load before starting game
    this.zenTextures.waitForLoad().then(() => {
      this.startMockGame();
    }).catch(() => {
      // Fallback textures will be used
      this.startMockGame();
    });
  }

  /**
   * Update score display
   */
  public setScore(score: number) {
    this.score = score;
    if (this.scoreElement) {
      this.scoreElement.textContent = score.toString();
    }
  }

  /**
   * Update tricks display
   */
  public setTricks(tricks: number) {
    this.tricks = tricks;
    if (this.tricksElement) {
      this.tricksElement.textContent = tricks.toString();
    }
  }

  /**
   * Update both score and tricks
   */
  public updateScoreDisplay(score: number, tricks: number) {
    this.setScore(score);
    this.setTricks(tricks);
  }

  private startMockGame() {
    // Generate Deck (28 card game - Indian trick-taking)
    const suits = ['hearts', 'diamonds', 'clubs', 'spades'];
    const ranks = ['J', '9', 'A', '10', 'K', 'Q', '8', '7'];
    
    // Deal 8 random cards to player
    const backTexture = this.zenTextures.getBackTexture();
    
    for (let i = 0; i < 8; i++) {
      const suit = suits[Math.floor(Math.random() * suits.length)];
      const rank = ranks[Math.floor(Math.random() * ranks.length)];
      const frontTexture = this.zenTextures.getCardTexture(suit, rank);
      
      const card = new Card(suit, rank, frontTexture, backTexture);
      this.hand.addCard(card);
    }

    // Set initial active player indicator
    this.playArea.setActivePlayer(1);

    // Simulate opponent play after a delay
    setTimeout(() => {
      this.simulateOpponentMove(1);
    }, 2000);
  }

  private simulateOpponentMove(playerIndex: number) {
    const suits = ['hearts', 'diamonds', 'clubs', 'spades'];
    const ranks = ['J', '9', 'A', '10', 'K', 'Q', '8', '7'];
    
    const suit = suits[Math.floor(Math.random() * suits.length)];
    const rank = ranks[Math.floor(Math.random() * ranks.length)];
    
    console.log(`Playing opponent card: ${rank} of ${suit} for player ${playerIndex}`);
    
    const frontTexture = this.zenTextures.getCardTexture(suit, rank);
    const backTexture = this.zenTextures.getBackTexture();
    
    const card = new Card(suit, rank, frontTexture, backTexture);
    
    // Start from opponent position
    let startPos = new THREE.Vector3();
    if (playerIndex === 1) startPos.set(1.5, 0.5, 0); // Right
    else if (playerIndex === 2) startPos.set(0, 0.5, -1.5); // Top
    else if (playerIndex === 3) startPos.set(-1.5, 0.5, 0); // Left
    
    card.setPosition(startPos.x, startPos.y, startPos.z, true);
    this.sceneManager.scene.add(card.mesh);
    
    // Play animation
    this.playArea.playCard(card, playerIndex);
    console.log(`Card added to scene and play area`);

    // Update active player indicator
    const nextPlayer = playerIndex < 3 ? playerIndex + 1 : 0;
    this.playArea.setActivePlayer(nextPlayer);

    // Chain next move
    if (playerIndex < 3) {
      setTimeout(() => this.simulateOpponentMove(playerIndex + 1), 1500);
    } else {
      // End of trick, update scores and clear table
      setTimeout(() => {
        // Simulate winning a trick (add 7 points)
        this.tricks += 1;
        this.score += 7;
        this.updateScoreDisplay(this.score, this.tricks);
        this.playArea.clearTable();
        this.playArea.clearActivePlayer();
        // Start new round logic could go here
      }, 3000);
    }
  }

  public update() {
    this.sceneManager.update();
  }
}
