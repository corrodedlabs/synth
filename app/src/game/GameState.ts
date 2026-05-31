import { Effect, Fiber } from "effect";
import { DragControls } from "../interaction/DragControls";
import { SceneManager } from "../scene/SceneManager";
import { Lighting } from "../scene/Lighting";
import { Table } from "../scene/Table";
import { ZenCardTextures } from "../utils/ZenCardTextures";
import { Card } from "./Card";
import { GameAction, GameModel, CardModel, PlayerIndex, gameReducer, initialGameModel } from "./GameModel";
import { Hand } from "./Hand";
import { mockGameProgram } from "./MockGameProgram";
import { PlayArea } from "./PlayArea";

export class GameState {
  private sceneManager: SceneManager;
  private lighting: Lighting;
  private table: Table;
  private hand: Hand;
  private playArea: PlayArea;
  private zenTextures: ZenCardTextures;
  // @ts-ignore
  private dragControls: DragControls;
  private model: GameModel = initialGameModel;
  private mockFiber: Fiber.RuntimeFiber<void, never> | null = null;

  private scoreElement: HTMLElement | null = null;
  private tricksElement: HTMLElement | null = null;

  constructor(sceneManager: SceneManager) {
    this.sceneManager = sceneManager;

    this.lighting = new Lighting(this.sceneManager.scene);
    this.table = new Table(this.sceneManager.scene);
    this.hand = new Hand(this.sceneManager.scene, this.sceneManager.camera);
    this.playArea = new PlayArea(this.sceneManager.scene);
    this.zenTextures = new ZenCardTextures();

    this.dragControls = new DragControls(
      this.sceneManager.camera,
      this.sceneManager.scene,
      this.sceneManager.renderer.domElement,
      this.hand,
      this.playArea
    );

    this.scoreElement = document.getElementById("score-value");
    this.tricksElement = document.getElementById("tricks-value");
    this.startMockRuntime();
  }

  public setScore(score: number) {
    this.model = { ...this.model, score };
    this.renderScore();
  }

  public setTricks(tricks: number) {
    this.model = { ...this.model, tricks };
    this.renderScore();
  }

  public updateScoreDisplay(score: number, tricks: number) {
    this.model = { ...this.model, score, tricks };
    this.renderScore();
  }

  public update() {
    this.sceneManager.update();
  }

  public destroy() {
    if (this.mockFiber) {
      Effect.runFork(Fiber.interrupt(this.mockFiber));
      this.mockFiber = null;
    }
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

  private startMockRuntime() {
    const loadTextures = Effect.tryPromise({
      try: () => this.zenTextures.waitForLoad(),
      catch: (error) => error,
    }).pipe(Effect.catchAll(() => Effect.void));

    const program = Effect.gen(this, function* () {
      yield* loadTextures;
      yield* mockGameProgram((action) => this.dispatch(action));
    });

    this.mockFiber = Effect.runFork(program);
  }

  private dispatch(action: GameAction) {
    this.model = gameReducer(this.model, action);
    this.renderAction(action);
  }

  private renderAction(action: GameAction) {
    switch (action._tag) {
      case "HandDealt":
        action.cards.forEach((card) => this.hand.addCard(this.createCard(card)));
        break;

      case "CardPlayed":
        this.playCard(action.card, action.playerIndex);
        break;

      case "ActivePlayerChanged":
        if (action.playerIndex === null) {
          this.playArea.clearActivePlayer();
        } else {
          this.playArea.setActivePlayer(action.playerIndex);
        }
        break;

      case "TrickScored":
        this.renderScore();
        break;
    }
  }

  private createCard(cardModel: CardModel) {
    const frontTexture = this.zenTextures.getCardTexture(cardModel.suit, cardModel.rank);
    const backTexture = this.zenTextures.getBackTexture();
    const card = new Card(cardModel.suit, cardModel.rank, frontTexture, backTexture);
    card.mesh.userData.modelId = cardModel.id;
    return card;
  }

  private playCard(cardModel: CardModel, playerIndex: PlayerIndex) {
    const card = this.findHandCard(cardModel) ?? this.createCard(cardModel);

    if (this.hand.getCards().includes(card)) {
      this.hand.removeCard(card);
    } else {
      const startPosition = this.startPositionForPlayer(playerIndex);
      card.setPosition(startPosition.x, startPosition.y, startPosition.z, true);
      this.sceneManager.scene.add(card.mesh);
    }

    this.playArea.playCard(card, playerIndex);
  }

  private findHandCard(cardModel: CardModel): Card | null {
    return this.hand.getCards().find((card) => card.mesh.userData.modelId === cardModel.id) ?? null;
  }

  private startPositionForPlayer(playerIndex: PlayerIndex) {
    switch (playerIndex) {
      case 0:
        return { x: 0, y: 0.5, z: 2.5 };
      case 1:
        return { x: 2.9, y: 0.5, z: -0.3 };
      case 2:
        return { x: 0, y: 0.5, z: -2.4 };
      case 3:
        return { x: -2.9, y: 0.5, z: -0.3 };
    }
  }

  private renderScore() {
    if (this.scoreElement) {
      this.scoreElement.textContent = this.model.score.toString();
    }
    if (this.tricksElement) {
      this.tricksElement.textContent = this.model.tricks.toString();
    }
  }
}
