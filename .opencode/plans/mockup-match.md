# Implementation Plan: Match Reference Mockup

## Summary of Changes Needed

Based on comparing the reference mockup with the current state, here are the key changes:

### 1. **Camera & Scene Adjustments** (`SceneManager.ts`)
- Adjust camera position and FOV to make the player's hand more prominent in the lower third of the screen
- Current: `camera.position.set(0, 4, 2)` 
- Need to pull camera back slightly and adjust angle to show hand more prominently

### 2. **Hand Position & Arc** (`Hand.ts`)
- Move hand center lower (closer to camera) so cards are more visible
- Adjust arc radius and spread for the elegant fan shown in mockup
- Current: `handCenter = (0, 0.5, 2.2)`, `handRadius = 1.5`
- Need: Position cards more prominently in view

### 3. **Enso Decorations** (`PlayArea.ts`)
- **Reduce from 3 to 2** - only left and right positions (remove top)
- **Increase size** - they're larger and more prominent in the mockup
- **Use sprites instead of plane meshes** for proper transparency
- Position them to the sides of the play area (not close to card positions)

### 4. **Trick Card Layout** (`PlayArea.ts`)
- Adjust positions for clearer cross/plus pattern
- Make sure all 4 positions (player, left, top, right) are clearly visible
- Current positions look slightly off

### 5. **Score UI** (`index.html` + new CSS + `GameState.ts`)
- Replace 3D sprites with **HTML overlay** in top-right corner
- Style: "Score: X | Tricks: Y" format
- Use serif font to match zen aesthetic

### 6. **Card Texture Note**
The reference mockup shows sumi-e art with Japanese calligraphy - this is different from the current Western card style. Creating new card art is likely out of scope, but I wanted to flag this difference.

---

## Detailed File Changes

| File | Changes |
|------|---------|
| `app/src/scene/SceneManager.ts` | Adjust camera position and FOV |
| `app/src/game/Hand.ts` | Modify hand center position, arc radius, and card arrangement |
| `app/src/game/PlayArea.ts` | Change from 3 to 2 enso markers, reposition them, increase size, use THREE.Sprite |
| `app/src/game/GameState.ts` | Remove 3D score sprites, add HTML score overlay |
| `app/index.html` | Add score display HTML element with styling |
