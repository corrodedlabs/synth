# How to Play Twenty-Eight

Twenty-Eight is a four-player trick-taking card game played in two fixed teams.
In this project, the local player is `Player 0` at the bottom of the table,
with opponents and partner arranged around the table.

## Players and Teams

- Four players play the game.
- Partners sit opposite each other.
- Player positions in the frontend:
  - `Player 0`: local player, bottom.
  - `Player 1`: right opponent.
  - `Player 2`: partner, top.
  - `Player 3`: left opponent.
- Turns move counter-clockwise around the table.

## Deck

Twenty-Eight uses 32 cards: eight cards from each suit.

Suits:

- Hearts
- Diamonds
- Clubs
- Spades

Ranks from highest to lowest:

1. Jack
2. Nine
3. Ace
4. Ten
5. King
6. Queen
7. Eight
8. Seven

The frontend model stores this order as:

```ts
["J", "9", "A", "10", "K", "Q", "8", "7"]
```

## Card Points

Only some cards are worth points:

| Card | Points |
| ---- | ------ |
| Jack | 3 |
| Nine | 2 |
| Ace | 1 |
| Ten | 1 |
| King | 0 |
| Queen | 0 |
| Eight | 0 |
| Seven | 0 |

There are 28 total card points in the deck, which gives the game its name.

## Deal

Each player receives eight cards.

The usual flow is:

1. Each player receives four cards.
2. Players bid using only those first four cards.
3. The winning bidder chooses the trump suit.
4. Each player receives the remaining four cards.
5. The play phase begins.

## Bidding

Bidding decides which team must try to win a minimum number of card points.

- The first bid must be at least 16.
- A player may bid higher than the current bid or pass.
- The highest bid can be 28.
- The auction ends when three players pass in succession.
- The highest bidder chooses the trump suit.
- The bidder's team must win at least the bid value in card points.

Example:

```text
Player 0 bids 16
Player 1 bids 18
Player 2 passes
Player 3 passes
Player 0 passes

Player 1 wins the bid at 18.
Player 1's team must win at least 18 card points.
```

## Trump

The winning bidder chooses one suit as trump.

At first, the trump suit is hidden from the other players. During play, a player
who cannot follow the suit that was led may call for trump to be exposed before
playing a card. Once exposed, the trump suit is visible and active for the rest
of the hand.

Important frontend states:

- Trump selected but hidden.
- Trump exposed and visible.
- Trump unavailable before the bidder has selected it.

## Playing Tricks

A trick is one round where each player plays one card.

1. The leading player plays any card from their hand.
2. The suit of that card is the led suit.
3. Each following player must play the led suit if they have one.
4. If a player cannot follow suit, they may discard another suit.
5. If trump has been exposed, a player who cannot follow suit may play trump.
6. The winner of the trick leads the next trick.

Before trump is exposed, trump-suit cards do not have special power. They behave
like ordinary cards until the suit is revealed.

After trump is exposed:

- The highest trump card in the trick wins.
- If no trump is played, the highest card of the led suit wins.

## Winning a Trick

Use this priority:

1. If exposed trump cards were played, the highest exposed trump wins.
2. Otherwise, the highest card of the led suit wins.
3. Cards from other suits cannot win unless they are exposed trump cards.

Rank order still applies inside a suit:

```text
J > 9 > A > 10 > K > Q > 8 > 7
```

## Scoring the Hand

After all eight tricks are complete:

1. Add up the point cards captured by each team.
2. The bidder's team wins if it captured at least the bid value.
3. The bidder's team loses if it captured fewer points than the bid value.

The frontend should display two related values clearly:

- Trick count: how many tricks the local side has won, or how many tricks have
  been completed depending on the selected UI mode.
- Card points: the point total earned from captured cards.

## What the Frontend Should Show

The player should be able to understand the game from the table state:

- Own hand is visible.
- Other players' cards are hidden.
- Active player is highlighted.
- Current trick is shown in the center.
- Led suit is visually clear once the first card is played.
- Legal cards are playable; illegal cards should stay in hand.
- Bid value and bidding team are visible after bidding.
- Trump is hidden until exposed, then visible for the rest of the hand.
- Completed tricks update the score display.
