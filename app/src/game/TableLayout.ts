import * as THREE from "three";

export type TableSeat = 0 | 1 | 2 | 3;
export type OpponentSeat = 1 | 2 | 3;

export interface ScreenPoint {
  readonly x: number;
  readonly y: number;
}

export interface ResponsiveTableLayout {
  readonly viewport: { readonly width: number; readonly height: number };
  readonly opponentSeats: Record<OpponentSeat, ScreenPoint>;
  readonly playedCards: Record<TableSeat, ScreenPoint>;
  readonly handCenter: ScreenPoint;
  readonly handWidthScale: number;
  readonly handCardScale: number;
  readonly tableCardScale: number;
}

const clamp = (value: number, minimum: number, maximum: number) =>
  Math.min(maximum, Math.max(minimum, value));

// A single screen-space composition for the whole table. DOM identities and
// Three.js cards consume these same anchors, so resizing cannot make a seat
// drift away from the card lane that belongs to it.
export function responsiveTableLayout(width: number, height: number): ResponsiveTableLayout {
  const shortLandscape = height <= 550 && width > height;
  const compact = width <= 700;

  if (shortLandscape) {
    const markerSize = width <= 700 ? 60 : 64;
    const sideSeatX = markerSize / 2 + 18;
    const topSeatY = clamp(height * 0.2, 72, 88);
    const sideY = height * 0.52;
    const sideCardInset = sideSeatX + markerSize / 2 + 32;

    return {
      viewport: { width, height },
      opponentSeats: {
        1: { x: width - sideSeatX, y: sideY },
        2: { x: width / 2, y: topSeatY },
        3: { x: sideSeatX, y: sideY },
      },
      playedCards: {
        0: { x: width / 2, y: height * 0.633 },
        1: { x: width - sideCardInset, y: sideY },
        2: { x: width / 2, y: topSeatY + 105 },
        3: { x: sideCardInset, y: sideY },
      },
      handCenter: { x: width / 2, y: height * 0.825 },
      handWidthScale: clamp(width / 1100, 0.58, 0.78),
      handCardScale: 0.68,
      tableCardScale: 0.62,
    };
  }

  if (compact) {
    const markerSize = 66;
    const sideSeatX = markerSize / 2 + 19;
    const topSeatY = clamp(height * 0.16, 118, 150);
    const topCardY = topSeatY + 112;
    const sideY = clamp(height * 0.43, topSeatY + 155, height * 0.48);
    // Wide compact windows have taller cards, so the lanes keep clear of a seat's name.
    const sideCardInset = sideSeatX + markerSize / 2 + Math.max(40, width * 0.07);
    const handCenterY = height * 0.78;
    const bottomCardY = sideY + clamp(height * 0.11, 75, 102);
    // The camera comes closer as a window squares up, so the cards would take more of it. They give that back.
    const squaring = clamp((0.65 * height) / width, 0.7, 1);

    return {
      viewport: { width, height },
      opponentSeats: {
        1: { x: width - sideSeatX, y: sideY },
        2: { x: width / 2, y: topSeatY },
        3: { x: sideSeatX, y: sideY },
      },
      playedCards: {
        0: { x: width / 2, y: bottomCardY },
        1: { x: width - sideCardInset, y: sideY },
        2: { x: width / 2, y: topCardY },
        3: { x: sideCardInset, y: sideY },
      },
      handCenter: { x: width / 2, y: handCenterY },
      handWidthScale: clamp(width / 500, 0.7, 0.84) * squaring,
      handCardScale: (width <= 375 ? 1.08 : 1.12) * squaring,
      tableCardScale: 1.08 * squaring,
    };
  }

  const markerSize = 88;
  const sideSeatX = clamp(width * 0.12, 100, 220);
  const topSeatY = clamp(height * 0.14, 112, 138);
  const topCardY = topSeatY + 168;
  const short = height < 700;
  const sideY = short ? topCardY + 58 : Math.max(height * 0.46, topCardY + 70);
  // The cards grow with the window and the seats' names don't, so the lanes keep clear of a long name.
  const sideCardInset = sideSeatX + markerSize / 2 + Math.max(63, height * 0.075);
  // The fan's reach grows with the height, so it rises to stay off the status line under it.
  const handCenterY = short
    ? height - Math.max(105, height * 0.165)
    : height - clamp(height * 0.18, 110, 165);
  const bottomCardY = short ? sideY + 36 : sideY + clamp(height * 0.11, 68, 105);

  return {
    viewport: { width, height },
    opponentSeats: {
      1: { x: width - sideSeatX, y: sideY },
      2: { x: width / 2, y: topSeatY },
      3: { x: sideSeatX, y: sideY },
    },
    playedCards: {
      0: { x: width / 2, y: bottomCardY },
      1: { x: width - sideCardInset, y: sideY },
      2: { x: width / 2, y: topCardY },
      3: { x: sideCardInset, y: sideY },
    },
    handCenter: { x: width / 2, y: handCenterY },
    handWidthScale: clamp(width / 1024, 0.8, 1),
    handCardScale: short ? 0.86 : 1,
    tableCardScale: short ? 0.76 : 0.95,
  };
}

// Convert a CSS-pixel anchor into the Three.js table plane. The projected
// center of the resulting world point lands back on the exact screen anchor.
export function screenPointOnPlane(
  point: ScreenPoint,
  planeY: number,
  camera: THREE.PerspectiveCamera,
  viewport: { readonly width: number; readonly height: number }
): THREE.Vector3 {
  // This is called during boot, before the first renderer frame. Without an
  // explicit matrix update, unproject() can use the camera's constructor
  // transform and place a direct-loaded phone layout far above the viewport.
  camera.updateMatrixWorld(true);
  const ndc = new THREE.Vector3(
    (point.x / viewport.width) * 2 - 1,
    1 - (point.y / viewport.height) * 2,
    0.5
  );
  const direction = ndc.unproject(camera).sub(camera.position).normalize();
  const distance = (planeY - camera.position.y) / direction.y;
  return camera.position.clone().add(direction.multiplyScalar(distance));
}

// Project the actual vertices of every mesh instead of projecting a world
// axis-aligned Box3. The latter becomes inaccurate once a card is rotated in
// perspective and previously let real overlaps escape the browser tests.
export function objectScreenBounds(
  object: THREE.Object3D,
  camera: THREE.PerspectiveCamera,
  canvas: HTMLElement
) {
  camera.updateMatrixWorld(true);
  object.updateWorldMatrix(true, true);
  const projected: THREE.Vector3[] = [];

  object.traverse((child) => {
    if (!(child instanceof THREE.Mesh)) return;
    child.geometry.computeBoundingBox();
    const box = child.geometry.boundingBox;
    if (!box) return;
    for (const x of [box.min.x, box.max.x]) {
      for (const y of [box.min.y, box.max.y]) {
        for (const z of [box.min.z, box.max.z]) {
          projected.push(
            new THREE.Vector3(x, y, z)
              .applyMatrix4(child.matrixWorld)
              .project(camera)
          );
        }
      }
    }
  });

  const xs = projected.map((point) => ((point.x + 1) / 2) * canvas.clientWidth);
  const ys = projected.map((point) => ((1 - point.y) / 2) * canvas.clientHeight);
  return {
    left: Math.min(...xs),
    top: Math.min(...ys),
    right: Math.max(...xs),
    bottom: Math.max(...ys),
  };
}
