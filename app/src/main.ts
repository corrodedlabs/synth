import { SceneManager } from './scene/SceneManager';
import { GameState } from './game/GameState';
import { storedActiveMatch, storedPlayerName } from './game/GameSession';
import { DebugUI } from './debug/DebugUI';

const container = document.getElementById('app')!;
const sceneManager = new SceneManager(container);
const gameState = new GameState(sceneManager);

const params = new URLSearchParams(window.location.search);

// Shadow/lighting tuning panel, only when explicitly requested
if (params.has('debug')) {
  new DebugUI(
    gameState.getLighting(),
    gameState.getTable(),
    gameState.getHand()
  );
}

// returning players get their name prefilled
const playerNameInput = document.getElementById('player-name') as HTMLInputElement | null;
const rememberedName = storedPlayerName();
if (playerNameInput && rememberedName) playerNameInput.value = rememberedName;

const interrupted = storedActiveMatch();
if (params.has('mock')) {
  gameState.startMock();
} else if (interrupted) {
  // the previous page load was mid-match: reconnect straight into it
  // (the server answers no-running-game if it has since ended)
  gameState.rejoinMatch(interrupted.name, interrupted.email);
} else {
  gameState.showStartScreen();
}

// Debug/automation bridge — lets tests drive the game without 3D drag gestures.
(window as unknown as { __game: unknown }).__game = {
  state: () => gameState.getModel(),
  legalCards: () => gameState.legalCardIds(),
  play: (cardId: string) => gameState.playCardById(cardId),
  cardScreenPos: (cardId: string) => gameState.cardScreenPosition(cardId),
  bid: (value: number) => gameState.getSession()?.submitBid(value),
  pass: () => gameState.getSession()?.submitBid('pass'),
  trump: (suit: 'hearts' | 'diamonds' | 'clubs' | 'spades') => gameState.getSession()?.chooseTrump(suit),
  expose: () => gameState.getSession()?.exposeTrump(),
  nextHand: () => gameState.getSession()?.nextHand(),
  // lobby controls
  createTable: (name: string) => gameState.createTable(name ?? 'tester'),
  browseTables: (name: string) => gameState.browseTables(name ?? 'tester'),
  joinTable: (roomName: string) => gameState.getSession()?.join(roomName),
  addBot: () => gameState.getSession()?.addBot(),
  startGame: () => gameState.getSession()?.startGame(),
  leaveTable: () => gameState.getSession()?.leaveRoom(),
  leaveMatch: () => gameState.leaveMatch(),
  kick: (member: string) => gameState.getSession()?.kick(member),
  roomName: () => gameState.getSession()?.roomName,
  sounds: () => gameState.soundState(),
};

function animate() {
  requestAnimationFrame(animate);
  gameState.update();
}

animate();
