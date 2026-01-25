import { SceneManager } from './scene/SceneManager';
import { GameState } from './game/GameState';
import { DebugUI } from './debug/DebugUI';

const container = document.getElementById('app')!;
const sceneManager = new SceneManager(container);
const gameState = new GameState(sceneManager);

// Initialize debug UI for shadow tuning
new DebugUI(
  gameState.getLighting(),
  gameState.getTable(),
  gameState.getHand()
);

function animate() {
  requestAnimationFrame(animate);
  gameState.update();
}

animate();
