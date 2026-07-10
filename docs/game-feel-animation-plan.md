# Game Feel and Animation Plan

## Purpose

Make each play, trick, score change, and hand result easy to follow and
pleasant to watch, without changing the game rules or making the server wait
for a client animation. The game should feel deliberate at the table while
remaining responsive for people who prefer a quicker pace.

## Findings from the current implementation

The frontend already has the necessary foundations:

- `@tweenjs/tween.js` is updated from the frame loop.
- Played cards tween to their seat in `PlayArea` and completed tricks sweep
  toward their winner.
- The active player's marker pulses, and sound effects exist for card plays,
  trick sweeps, trump reveals, and results.
- `GameSession` adds fixed pauses for bids, cards, and completed tricks.

Those pieces are not perceived as one experience. They are independently
triggered, the visual outcome is brief, and the next server event can begin
as the prior sweep is ending. There is no persistent winner highlight, score
transfer, or clear visual hierarchy for the end of a trick.

## Design principles

1. **A completed trick is a moment.** Keep all four cards visible long enough
   to read before resolving it.
2. **Show cause before effect.** Identify the winning card and winner before
   moving cards or changing the score.
3. **Server authority stays unchanged.** Server events determine every state;
   client animation only controls presentation and pacing.
4. **Never trap the player.** Local input, reconnects, leaving a match, and
   new deals can cancel or skip pending presentation work safely.
5. **Motion must be optional.** Honour reduced-motion preferences and provide
   a speed choice without removing essential text and score feedback.

## Target trick timeline

The timings below are for the default **Standard** speed. They should be
defined as named constants/configuration, not scattered sleeps.

| Moment | Target duration | Visible feedback |
| --- | ---: | --- |
| Card commits | 450–600 ms | Card travels from the player seat to its place in the center cross. |
| Card settles | 200 ms | Small settle/tilt; the active-player marker moves to the next seat. |
| Four-card read | 1,100–1,400 ms | Completed trick remains on the table, with the winning card outlined. |
| Trick winner | 450–650 ms | Winner marker glows; a label says `You take the trick` or `<Name> takes the trick`, with `+N` points when applicable. |
| Collection | 550–700 ms | The cards sweep together toward the winning seat. |
| Score landing | 250–350 ms | The relevant tricks/points HUD counters animate to their new values. |
| Next lead | after collection | Turn indicator moves to the winner; the next card can then be presented. |

Suggested profiles:

- **Relaxed:** 1.25× Standard timing.
- **Standard (default):** timings in the table above.
- **Quick:** 0.7× Standard timing, retaining the winner label and score update.
- **Reduced motion:** short fades/highlights only; do not use card flights or
  sweeps, but keep the winner label visible for at least 900 ms.

## Implementation milestones

### 1. Define presentation state and animation contracts

- Add an `AnimationDirector` (or similarly named client-only service) that
  owns named animation sequences: deal, card play, trump reveal, trick
  resolution, score update, and result-panel entrance.
- Give each sequence a cancellation token/version. A snapshot restore, table
  reset, leave, or a new deal cancels outstanding sequences and immediately
  reconciles the rendered scene with the reducer model.
- Keep animation state outside the authoritative `GameModel` where possible.
  The model continues to describe the actual cards, turn, points, and phase;
  the director describes how that state is being presented.
- Replace the fixed `BID_PAUSE`, `CARD_PAUSE`, and `TRICK_PAUSE` waits in
  `GameSession` with well-named presentation milestones. The event queue must
  not advance visual play past a milestone that would make a trick ambiguous.

Primary files: `app/src/game/GameSession.ts`, `app/src/game/GameState.ts`,
and a new animation service under `app/src/game/`.

### 2. Make card play feel physical

- Animate a local card from its actual hand position to the table, rather than
  only removing it and assigning its destination.
- Spawn opponent cards at their seat/marker anchor and fly them into the
  trick. Preserve card orientation/readability as they land.
- Give the landing a small stagger, shadow/scale settle, and matching card
  sound; do not add heavy particle effects that obscure card ranks.
- Ensure a second local tap/drag cannot create a duplicate card while its
  first play is awaiting server confirmation.

Primary files: `app/src/game/PlayArea.ts`, `app/src/game/Card.ts`,
`app/src/game/Hand.ts`, and `app/src/game/GameState.ts`.

### 3. Create a clear trick-resolution sequence

- Determine the winner from the existing authoritative turn/points events,
  as the session already does.
- Freeze the four-card tableau for the read window, then elevate or outline
  the winning card using a high-contrast, suit-neutral treatment.
- Animate a glow/ring on the winning seat and show a centered transient
  outcome label: `You take the trick`, `Maya takes the trick`, and `+3
  points` when the trick contains points.
- Collect the cards as a cohesive packet toward the winner instead of four
  unrelated simultaneous removals.
- Start the score counter transition when the packet reaches the winner, not
  when the server event first arrives.
- Do not show a positive points chip for a zero-point trick; still announce
  its winner.

Primary files: `app/src/game/PlayArea.ts`, `app/src/game/GameState.ts`,
`app/src/ui/UiOverlay.ts`, and `app/index.html`.

### 4. Improve major game moments

- **Deal:** deal the hand in two readable batches of four, with a small
  dealer/hand-number cue. Avoid blocking the bidding UI after the deal ends.
- **Bidding:** animate a compact bid history/status chip so a pass or raised
  bid remains legible instead of disappearing from a single status line.
- **Trump reveal:** briefly reveal the suit with a table-level cue, update the
  trump indicator, and retain a concise `Trump revealed: ♥` status message.
- **Hand result:** animate the panel in after the final trick has fully
  resolved. State bidding team, bid, card points, `made it`/`set`, match-score
  change, and who may deal next. Add directional score-change chips such as
  `+1 match point` or `−2 match points`.
- **Match over:** retain the existing outcome panel and fanfare, but animate
  the final match score and reveal hand-history chips in sequence.

### 5. Support accessibility and user control

- Detect `prefers-reduced-motion` at startup and when the preference changes.
- Add a small settings control for Relaxed, Standard, and Quick pacing, saved
  in local storage. Reduced motion overrides movement-heavy profiles.
- Make every visual outcome available as text: winning player, trick points,
  team score, bid outcome, and current turn.
- Keep motion clear of the phone hand fan and ensure labels remain within the
  safe area at 390×844.

### 6. Verify behavior and visual quality

- Unit-test animation configuration selection, sequence cancellation, and
  the conversion of a completed trick to a winner presentation.
- Extend the Playwright full-game script to wait for and capture:
  card in flight, four-card tableau, winner highlight, score landing, and
  hand-result panel.
- Extend the mobile script with viewport checks for winner labels and the
  speed/settings control.
- Run `npm test`, `npm run build`, and the headed desktop/mobile browser
  suites with the Racket server running.
- Manually inspect screenshots for card overlap, readable rank faces, label
  collisions, and whether a player can identify the winning seat at a glance.

## Event-flow requirements

```text
Server CardPlayed
  -> reducer records card immediately
  -> AnimationDirector presents card travel
  -> next visual event may be queued, but does not visually overtake the play

Server Turn / PointsWon after the fourth card
  -> present completed tableau
  -> announce and highlight winner
  -> collect packet to winning seat
  -> animate score counters
  -> present winner as the next active player

Snapshot / leave / new hand
  -> cancel active presentation
  -> rebuild scene from authoritative model or clear it
```

The transport and reducer must still process events promptly. Only visual
presentation is serialized; no server acknowledgment or game-rule decision
depends on a local animation completing.

## Acceptance criteria

- At Standard speed, a player can identify all four cards, the winner, and
  trick points before the next lead appears.
- The next card never visually overlaps an unresolved trick collection.
- Scores change exactly once, when the completed trick visibly resolves.
- Reconnect, leave, match abort, and next-hand transitions do not leave
  orphaned cards, tweens, labels, or winner effects on screen.
- Reduced-motion users receive the same outcome information without long
  movement.
- Desktop and mobile browser tests pass, and visual screenshots show no HUD,
  label, panel, or hand-fan collisions.

## Rollout order

1. Animation director, timing profiles, and cancellation/reconciliation.
2. Card travel and completed-trick/winner/collection sequence.
3. Animated score HUD, bid/trump cues, and enhanced hand result.
4. Accessibility settings and reduced-motion behavior.
5. Automated visual-state coverage, tuned timings, and screenshot review.

This order yields a useful improvement after milestone 2, while ensuring the
more decorative enhancements never compromise authoritative gameplay or
reconnect behavior.

## Completion status

Implemented:

- Shared Standard, Relaxed, Quick, and reduced-motion pacing profiles.
- Cancellable card movement and trick resolution, including score landing
  only after the collected trick reaches its winner.
- Staged four-card deal packets, bid-history chips, trump-reveal table moment,
  winner outcome cue, and animated hand-result card with match-point swing.
- Persistent speed control, mobile safe-area styling, and Playwright coverage
  for deal moments, bid history, trick outcomes, result chips, and phone fit.

Verification remains part of every visual change: desktop and 390×844 mobile
flows must play a complete hand without console errors or overlay collisions.
