import * as THREE from 'three';
import * as TWEEN from '@tweenjs/tween.js';
import { Card } from './Card';
import { GamePacingProfile, gamePacing } from './GamePacing';
import { objectScreenBounds, screenPointOnPlane } from './TableLayout';
import type { ResponsiveTableLayout } from './TableLayout';

export class PlayArea {
  private scene: THREE.Scene;
  private camera: THREE.PerspectiveCamera;
  private playedCards: Card[] = [];
  // Cards keep their own stable list while a trick is being resolved, so a
  // snapshot/leave can cancel the presentation without touching a later play.
  private resolvingCards: Card[] = [];
  private resolutionTween: TWEEN.Tween<{ progress: number }> | null = null;
  private resolutionId = 0;
  private pacing: GamePacingProfile = gamePacing();
  private positions: THREE.Vector3[] = [
    new THREE.Vector3(),
    new THREE.Vector3(),
    new THREE.Vector3(),
    new THREE.Vector3(),
  ];

  constructor(scene: THREE.Scene, camera: THREE.PerspectiveCamera) {
    this.scene = scene;
    this.camera = camera;
  }

  public setViewportLayout(layout: ResponsiveTableLayout) {
    this.positions = ([0, 1, 2, 3] as const).map((playerIndex) =>
      screenPointOnPlane(
        layout.playedCards[playerIndex],
        0.2,
        this.camera,
        layout.viewport
      )
    );

    this.playedCards.forEach((card, index) => {
      const playerIndex = card.mesh.userData.playerIndex as number;
      this.placeCard(card, playerIndex, true, 0, index);
    });
  }

  // Keep anything already on the table in step when the viewport changes.
  public setCardScale(scale: number) {
    [...this.playedCards, ...this.resolvingCards].forEach((card) => card.mesh.scale.setScalar(scale));
  }

  // Exact screen rectangles for cards currently in the trick. Browser tests
  // compare these with the fixed DOM seat markers to prevent layout overlap.
  public playedCardScreenBounds(canvas: HTMLElement) {
    return this.playedCards.map((card) => {
      this.camera.updateMatrixWorld(true);
      const center = card.mesh.getWorldPosition(new THREE.Vector3()).project(this.camera);
      return {
        player: card.mesh.userData.playerIndex as number,
        centerX: ((center.x + 1) / 2) * canvas.clientWidth,
        centerY: ((1 - center.y) / 2) * canvas.clientHeight,
        ...objectScreenBounds(card.mesh, this.camera, canvas),
      };
    });
  }

  public setPacing(pacing: GamePacingProfile) {
    this.pacing = pacing;
  }

  public playCard(card: Card, playerIndex: number) {
    this.playedCards.push(card);
    card.mesh.userData.playerIndex = playerIndex;

    // If card was not in scene (e.g. from opponent), add it
    if (!card.mesh.parent) {
      this.scene.add(card.mesh);
    }

    const immediate = this.pacing.cardTravelMs === 0;
    this.placeCard(
      card,
      playerIndex,
      immediate,
      this.pacing.cardTravelMs,
      this.playedCards.length - 1
    );
  }

  private placeCard(
    card: Card,
    playerIndex: number,
    immediate: boolean,
    duration: number,
    stackIndex: number
  ) {
    const pos = this.positions[playerIndex];
    card.setPosition(pos.x, pos.y + ((stackIndex + 1) * 0.005), pos.z, immediate, duration);

    // Face the camera head-on so ranks stay readable, with a slight
    // per-seat tilt so the trick reads like cards tossed onto a table.
    const toCamera = this.camera.position.clone().sub(pos);
    const rotX = -Math.atan2(toCamera.y, toCamera.z);
    const tilts = [0.05, -0.07, 0.04, 0.08];
    card.setRotation(rotX, 0, tilts[playerIndex] ?? 0, immediate, duration);
  }

  // Resolves one completed trick as a presentation sequence: identify the
  // winner first, then collect the packet. The server still decides every
  // card, winner, and score; this only gives the table time to read it.
  public resolveTrick(
    winnerIndex: number | null,
    callbacks: { onCollect: () => void; onComplete: () => void }
  ) {
    if (winnerIndex === null || this.playedCards.length === 0) {
      this.clearTable();
      callbacks.onComplete();
      return;
    }

    this.cancelResolution(true);
    const cards = this.playedCards;
    this.playedCards = [];
    this.resolvingCards = cards;
    const resolution = ++this.resolutionId;
    const winningCard = cards.find((card) => card.mesh.userData.playerIndex === winnerIndex);
    winningCard?.setTrickWinner(true, this.pacing.reducedMotion ? 0 : 180);

    const reveal = new TWEEN.Tween({ progress: 0 })
      .to({ progress: 1 }, this.pacing.winnerRevealMs)
      .easing(TWEEN.Easing.Quadratic.Out)
      .onComplete(() => {
        if (resolution !== this.resolutionId) return;
        callbacks.onCollect();
        this.collectTrick(cards, winnerIndex, resolution, callbacks.onComplete);
      })
      .start();
    this.resolutionTween = reveal;
  }

  // Immediately removes cards. This is deliberately separate from
  // resolveTrick for resets, reconnect snapshots, and a defensive clear.
  public clearTable() {
    const cards = [...this.playedCards, ...this.resolvingCards];
    this.playedCards = [];
    this.cancelResolution(false);
    this.resolvingCards = [];
    cards.forEach((card) => {
      card.cancelPresentation();
      this.scene.remove(card.mesh);
    });
  }

  private collectTrick(
    cards: Card[],
    winnerIndex: number,
    resolution: number,
    onComplete: () => void
  ) {
    const sweepTargets = [
      new THREE.Vector3(0, 0.4, 4.0),    // toward us
      new THREE.Vector3(4.5, 0.4, -0.5), // right
      new THREE.Vector3(0, 0.4, -4.0),   // top
      new THREE.Vector3(-4.5, 0.4, -0.5) // left
    ];
    const target = sweepTargets[winnerIndex];
    let remaining = cards.length;
    cards.forEach((card) => {
      card.setTrickWinner(false, 120);
      card.setPosition(
        target.x,
        target.y,
        target.z,
        this.pacing.cardCollectMs === 0,
        this.pacing.cardCollectMs
      );
      // The position tween belongs to Card so incoming resets can cancel it.
      // A small companion tween gives all cards one collection completion.
      new TWEEN.Tween({ progress: 0 })
        .to({ progress: 1 }, this.pacing.cardCollectMs)
        .easing(TWEEN.Easing.Quadratic.In)
        .onComplete(() => {
          if (resolution !== this.resolutionId) return;
          card.cancelPresentation();
          this.scene.remove(card.mesh);
          remaining--;
          if (remaining === 0) {
            this.resolvingCards = [];
            this.resolutionTween = null;
            onComplete();
          }
        })
        .start();
    });
  }

  private cancelResolution(removeCards: boolean) {
    this.resolutionId++;
    this.resolutionTween?.stop();
    this.resolutionTween = null;
    if (removeCards) {
      this.resolvingCards.forEach((card) => {
        card.cancelPresentation();
        this.scene.remove(card.mesh);
      });
      this.resolvingCards = [];
    }
  }
}
