import { SceneManager } from './scene/SceneManager';
import { GameState } from './game/GameState';
import { storedActiveMatch, storedPlayerName } from './game/GameSession';
import { inviteLink } from './ui/UiOverlay';
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
const joinTarget = params.get('join');
// an invite link arms every start screen this load may show — including
// the fallback one after a failed rejoin of an interrupted match
if (joinTarget) gameState.setJoinTarget(joinTarget);
if (params.has('mock')) {
  gameState.startMock();
} else if (interrupted) {
  // the previous page load was mid-match: reconnect straight into it
  // (the server answers no-running-game if it has since ended)
  gameState.rejoinMatch(interrupted.name, interrupted.email);
} else if (joinTarget) {
  // an invite link: the start screen leads with "Join <table>"
  gameState.prepareJoinLink(joinTarget);
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
  createTable: (name: string, target?: number) => gameState.createTable(name ?? 'tester', target ?? 6),
  browseTables: (name: string) => gameState.browseTables(name ?? 'tester'),
  joinTable: (roomName: string) => gameState.getSession()?.join(roomName),
  addBot: () => gameState.getSession()?.addBot(),
  startGame: () => gameState.getSession()?.startGame(),
  leaveTable: () => gameState.getSession()?.leaveRoom(),
  leaveMatch: () => gameState.leaveMatch(),
  kick: (member: string) => gameState.getSession()?.kick(member),
  roomName: () => gameState.getSession()?.roomName,
  sounds: () => gameState.soundState(),
  cardStates: () => gameState.handCardStates(),
  emote: (id: string) => gameState.getSession()?.sendEmote(id),
  resolveAbandon: (choice: 'replace' | 'close') => gameState.getSession()?.resolveAbandon(choice),
  standings: (name: string) => gameState.showStandings(name ?? 'viewer'),
  joinByLink: (name: string, roomName: string) => gameState.joinTable(name, roomName),
  inviteLink: () => {
    const room = gameState.getModel().roomName;
    return room ? inviteLink(room) : null;
  },
};

function animate() {
  requestAnimationFrame(animate);
  gameState.update();
}

animate();
