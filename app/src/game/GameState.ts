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
    this.hand = new Hand(this.sceneManager.scene, this.sceneManager.camera);
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
    this.playArea.setActivePlayer(0); // Start with player

    // First play a card from the player (position 0) for demo
    setTimeout(() => {
      this.playDemoPlayerCard();
      // Then simulate opponent play after player card is placed
      setTimeout(() => {
        this.playArea.setActivePlayer(1);
        this.simulateOpponentMove(1);
      }, 1200);
    }, 800);
  }

  /**
   * Play a demo card for the player (position 0) to show complete cross pattern
   */
  private playDemoPlayerCard() {
    const suits = ['hearts', 'diamonds', 'clubs', 'spades'];
    const ranks = ['J', '9', 'A', '10', 'K', 'Q', '8', '7'];
    
    const suit = suits[Math.floor(Math.random() * suits.length)];
    const rank = ranks[Math.floor(Math.random() * ranks.length)];
    
    const frontTexture = this.zenTextures.getCardTexture(suit, rank);
    const backTexture = this.zenTextures.getBackTexture();
    
    const card = new Card(suit, rank, frontTexture, backTexture);
    
    // Start from player's hand area (bottom)
    card.setPosition(0, 0.5, 2.5, true);
    this.sceneManager.scene.add(card.mesh);
    
    // Play to position 0 (bottom of cross)
    this.playArea.playCard(card, 0);
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
    if (playerIndex === 1) startPos.set(2.9, 0.5, -0.3); // Right
    else if (playerIndex === 2) startPos.set(0, 0.5, -2.4); // Top
    else if (playerIndex === 3) startPos.set(-2.9, 0.5, -0.3); // Left
    
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
      // End of trick - keep cards visible longer for demo
      setTimeout(() => {
        // Simulate winning a trick (add 7 points)
        this.tricks += 1;
        this.score += 7;
        this.updateScoreDisplay(this.score, this.tricks);
        this.playArea.clearActivePlayer();
        // Start new round logic could go here
      }, 8000); // Longer delay to appreciate the cross pattern
    }
  }

  public update() {
    this.sceneManager.update();
  }

  public getLighting(): Lighting {
    return this.lighting;
  }

  public getHand(): Hand {
    return this.hand;
  }

  public getTable(): Table {
    return this.table;
  }
}
