import { MeshBuilder } from "@babylonjs/core";

export function initGameScene() {
  const box = MeshBuilder.CreateBox("box", {
    height: 0.25,
    width: 1.2,
    depth: 1,
  });

  box.position.y = -0.25;
}
