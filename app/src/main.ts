import { SceneManager } from './scene/SceneManager';
import { GameState } from './game/GameState';

const container = document.getElementById('app')!;
const sceneManager = new SceneManager(container);
const gameState = new GameState(sceneManager);

function animate() {
  requestAnimationFrame(animate);
  gameState.update();
}

animate();
