import { describe, expect, it } from "vitest";
import { decodeCard, decodeServerEvent, encodeCard, encodeCommand } from "./protocol";
import { parseSExpr, Prefab, sym } from "./sexpr";

describe("card mapping", () => {
  it("decodes server cards", () => {
    expect(decodeCard(parseSExpr("#s(card jack 7 club 3)"))).toEqual({
      id: "clubs-J",
      suit: "clubs",
      rank: "J",
    });
    expect(decodeCard(parseSExpr("#s(card ten 4 heart 1)"))).toEqual({
      id: "hearts-10",
      suit: "hearts",
      rank: "10",
    });
  });

  it("re-encodes cards exactly as game.rkt builds them", () => {
    // ranks double as trick strength: J 9 A 10 K Q 8 7 → 7..0
    const samples = [
      "#s(card jack 7 club 3)",
      "#s(card nine 6 diamond 2)",
      "#s(card ace 5 spade 1)",
      "#s(card ten 4 club 1)",
      "#s(card king 3 heart 0)",
      "#s(card queen 2 diamond 0)",
      "#s(card eight 1 heart 0)",
      "#s(card seven 0 spade 0)",
    ];
    for (const sample of samples) {
      const card = decodeCard(parseSExpr(sample));
      expect(encodeCard(card)).toEqual(parseSExpr(sample) as Prefab);
    }
  });
});

describe("decodeServerEvent", () => {
  it("ignores void frames", () => {
    expect(decodeServerEvent("#<void>")).toBeNull();
  });

  it("decodes room flow", () => {
    expect(decodeServerEvent("room-created")).toEqual({ _tag: "RoomCreated" });
    expect(decodeServerEvent('(room-members "r" (bot-2 bot-1 "me@x"))')).toEqual({
      _tag: "RoomMembers",
      members: ["bot-2", "bot-1", "me@x"],
    });
    expect(decodeServerEvent("(game-started)")).toEqual({ _tag: "GameStarted" });
  });

  it("decodes the lobby flow", () => {
    expect(decodeServerEvent("room-joined")).toEqual({ _tag: "RoomJoined" });
    expect(decodeServerEvent("room-left")).toEqual({ _tag: "RoomLeft" });
    expect(decodeServerEvent('(room-closed "a\'s table")')).toEqual({ _tag: "RoomClosed" });
    expect(decodeServerEvent('(removed-from-room "a\'s table")')).toEqual({
      _tag: "RemovedFromRoom",
    });
    expect(decodeServerEvent("(start-game-failed room-not-ready)")).toEqual({
      _tag: "StartGameFailed",
      reason: "room-not-ready",
    });
    // exact shape the server's game-room->list produces
    expect(
      decodeServerEvent(
        '(active-rooms (((host . "a@x") (name . "a\'s table") (members "bot-1" "a@x")) ((host . "b@x") (name . "b\'s table") (members "b@x"))))'
      )
    ).toEqual({
      _tag: "ActiveRooms",
      rooms: [
        { host: "a@x", name: "a's table", members: ["bot-1", "a@x"] },
        { host: "b@x", name: "b's table", members: ["b@x"] },
      ],
    });
    expect(decodeServerEvent("(active-rooms ())")).toEqual({ _tag: "ActiveRooms", rooms: [] });
  });

  it("decodes the bidding flow", () => {
    expect(decodeServerEvent("(turn 2)")).toEqual({ _tag: "Turn", playerIndex: 2 });
    expect(decodeServerEvent("(request-bid 16)")).toEqual({ _tag: "BidRequested", minBid: 16 });
    expect(decodeServerEvent("(bid-placed 1 17)")).toEqual({ _tag: "BidPlaced", playerIndex: 1, bid: 17 });
    expect(decodeServerEvent("(bid-placed 2 pass)")).toEqual({ _tag: "BidPlaced", playerIndex: 2, bid: "pass" });
    expect(decodeServerEvent("(bid-result (19 . 1))")).toEqual({ _tag: "BidResult", bid: 19, playerIndex: 1 });
    expect(decodeServerEvent("(choose-trump)")).toEqual({ _tag: "ChooseTrumpRequested" });
    expect(decodeServerEvent("(trump-selected)")).toEqual({ _tag: "TrumpSelected" });
  });

  it("decodes hands and plays", () => {
    expect(decodeServerEvent('(hand "me@x" (#s(card jack 7 club 3)))')).toEqual({
      _tag: "HandDealt",
      cards: [{ id: "clubs-J", suit: "clubs", rank: "J" }],
    });
    expect(decodeServerEvent("(played 3 #s(card eight 1 heart 0))")).toEqual({
      _tag: "CardPlayed",
      playerIndex: 3,
      card: { id: "hearts-8", suit: "hearts", rank: "8" },
    });
    expect(decodeServerEvent("(played 3 expose-trump)")).toEqual({
      _tag: "CardPlayed",
      playerIndex: 3,
      card: "expose-trump",
    });
  });

  it("decodes the end of a game", () => {
    expect(decodeServerEvent('(game-aborted "a\'s table")')).toEqual({ _tag: "GameAborted" });
  });

  it("decodes play requests", () => {
    expect(
      decodeServerEvent(
        "(play-card (cards-played #s(card ace 5 spade 1)) (game-state (trump-suit . #f) (first-suit . spade)))"
      )
    ).toEqual({
      _tag: "PlayRequested",
      cardsPlayed: [{ id: "spades-A", suit: "spades", rank: "A" }],
      trumpSuit: null,
      firstSuit: "spades",
    });

    expect(
      decodeServerEvent("(play-card (cards-played) (game-state (trump-suit . diamond) (first-suit . #f)))")
    ).toEqual({
      _tag: "PlayRequested",
      cardsPlayed: [],
      trumpSuit: "diamonds",
      firstSuit: null,
    });
  });

  it("decodes points", () => {
    const event = decodeServerEvent("(points-won #hash((0 . 10) (1 . 10) (2 . 5) (3 . 3)))");
    expect(event?._tag).toBe("PointsWon");
    if (event?._tag === "PointsWon") {
      expect(event.points.get(0)).toBe(10);
      expect(event.points.get(3)).toBe(3);
    }
  });

  it("decodes the match flow", () => {
    expect(
      decodeServerEvent(
        "(hand-result ((hand . 2) (bidder . 1) (bid . 19) (made . #t) (delta . 1) (evens . -2) (odds . 1) (target . 6)))"
      )
    ).toEqual({
      _tag: "HandResult",
      hand: 2, bidder: 1, bid: 19, made: true, delta: 1, evens: -2, odds: 1, target: 6,
    });
    expect(
      decodeServerEvent(
        "(hand-result ((hand . 1) (bidder . 0) (bid . 20) (made . #f) (delta . -4) (evens . -4) (odds . 0) (target . 6)))"
      )
    ).toEqual({
      _tag: "HandResult",
      hand: 1, bidder: 0, bid: 20, made: false, delta: -4, evens: -4, odds: 0, target: 6,
    });
    expect(
      decodeServerEvent("(match-over ((winner . odds) (evens . -2) (odds . 6) (hands . 7)))")
    ).toEqual({ _tag: "MatchOver", winner: "odds", evens: -2, odds: 6, hands: 7 });
    // malformed bodies fall through instead of crashing the event loop
    expect(decodeServerEvent("(hand-result ((hand . 1)))")).toEqual({
      _tag: "Ignored",
      raw: "(hand-result ((hand . 1)))",
    });
    expect(decodeServerEvent("(match-over ((winner . nobody)))")).toEqual({
      _tag: "Ignored",
      raw: "(match-over ((winner . nobody)))",
    });
  });

  it("decodes emotes", () => {
    expect(decodeServerEvent("(emote-played 2 laugh)")).toEqual({
      _tag: "EmotePlayed",
      seat: 2,
      emote: "laugh",
    });
    expect(decodeServerEvent("(emote-played x laugh)")).toEqual({
      _tag: "Ignored",
      raw: "(emote-played x laugh)",
    });
  });

  it("decodes the abandonment flow", () => {
    expect(decodeServerEvent('(seat-abandoned 2 "a@x")')).toEqual({
      _tag: "SeatAbandoned",
      seat: 2,
      email: "a@x",
    });
    expect(decodeServerEvent('(seat-replaced 2 bot-7 (bot-3 bot-2 bot-7 "host@x"))')).toEqual({
      _tag: "SeatReplaced",
      seat: 2,
      email: "bot-7",
      members: ["bot-3", "bot-2", "bot-7", "host@x"],
    });
  });

  it("decodes the reconnection flow", () => {
    expect(decodeServerEvent('(player-disconnected "a@x" 45)')).toEqual({
      _tag: "PlayerDisconnected",
      email: "a@x",
      graceSeconds: 45,
    });
    expect(decodeServerEvent('(player-reconnected "a@x")')).toEqual({
      _tag: "PlayerReconnected",
      email: "a@x",
    });
    expect(decodeServerEvent("(no-running-game)")).toEqual({ _tag: "NoRunningGame" });
  });

  it("decodes a mid-hand snapshot", () => {
    // exact shape the server's rejoin-snapshot prints (verified via racket)
    const frame =
      '(game-snapshot ((room . my-room) (members "j3" "j2" "me@x" "host@x") (your-seat . 2)' +
      " (hand-number . 2) (evens . -2) (odds . 1) (target . 6) (stage . playing)" +
      " (high-bid . #f) (bid-result 17 . 1) (trump . #f) (trump-exposed . #f)" +
      " (your-hand #s(card jack 7 club 3) #s(card seven 0 heart 0)) (trick-leader . 0)" +
      " (trick (0 . #s(card ace 5 heart 1))) (points (0 . 3) (1 . 0) (2 . 4) (3 . 0))" +
      " (tricks (0 . 1) (1 . 0) (2 . 1) (3 . 0)) (last-result . #f) (awaiting . 2)" +
      " (request play-card (cards-played #s(card ace 5 heart 1))" +
      " (game-state (trump-suit . #f) (first-suit . heart)))))";
    const event = decodeServerEvent(frame);
    expect(event?._tag).toBe("GameSnapshot");
    if (event?._tag !== "GameSnapshot") return;
    const s = event.snapshot;
    expect(s.room).toBe("my-room");
    expect(s.members).toEqual(["j3", "j2", "me@x", "host@x"]);
    expect(s.yourSeat).toBe(2);
    expect(s.handNumber).toBe(2);
    expect(s.evens).toBe(-2);
    expect(s.odds).toBe(1);
    expect(s.stage).toBe("playing");
    expect(s.highBid).toBeNull();
    expect(s.bidResult).toEqual({ value: 17, seat: 1 });
    expect(s.trumpSuit).toBeNull();
    expect(s.trumpExposed).toBe(false);
    expect(s.yourHand.map((card) => card.id)).toEqual(["clubs-J", "hearts-7"]);
    expect(s.trick).toEqual([{ seat: 0, card: { id: "hearts-A", suit: "hearts", rank: "A" } }]);
    expect(s.points.get(0)).toBe(3);
    expect(s.points.get(2)).toBe(4);
    expect(s.tricks.get(0)).toBe(1);
    expect(s.lastResult).toBeNull();
    expect(s.awaiting).toBe(2);
    expect(s.request).toEqual({
      _tag: "PlayRequested",
      cardsPlayed: [{ id: "hearts-A", suit: "hearts", rank: "A" }],
      trumpSuit: null,
      firstSuit: "hearts",
    });
  });

  it("decodes a between-hands snapshot with its result", () => {
    const frame =
      '(game-snapshot ((room . my-room) (members "j3" "j2" "me@x" "host@x") (your-seat . 3)' +
      " (hand-number . 1) (evens . 1) (odds . 0) (target . 6) (stage . between-hands)" +
      " (high-bid 16 . 0) (bid-result 16 . 0) (trump . diamond) (trump-exposed . #t)" +
      " (your-hand) (trick-leader . 3) (trick) (points (0 . 10) (1 . 6) (2 . 9) (3 . 3))" +
      " (tricks (0 . 3) (1 . 2) (2 . 2) (3 . 1))" +
      " (last-result (hand . 1) (bidder . 0) (bid . 16) (made . #t) (delta . 1)" +
      " (evens . 1) (odds . 0) (target . 6)) (awaiting . #f) (request . #f)))";
    const event = decodeServerEvent(frame);
    expect(event?._tag).toBe("GameSnapshot");
    if (event?._tag !== "GameSnapshot") return;
    const s = event.snapshot;
    expect(s.stage).toBe("between-hands");
    expect(s.yourHand).toEqual([]);
    expect(s.trick).toEqual([]);
    expect(s.trumpSuit).toBe("diamonds");
    expect(s.awaiting).toBeNull();
    expect(s.request).toBeNull();
    expect(s.lastResult).toEqual({
      _tag: "HandResult",
      hand: 1, bidder: 0, bid: 16, made: true, delta: 1, evens: 1, odds: 0, target: 6,
    });
  });

  it("passes unknown frames through as Ignored", () => {
    expect(decodeServerEvent("(done)")).toEqual({ _tag: "Ignored", raw: "(done)" });
    expect(decodeServerEvent("invalid-request")).toEqual({ _tag: "Ignored", raw: "invalid-request" });
  });
});

describe("encodeCommand", () => {
  it("encodes lobby commands", () => {
    expect(encodeCommand({ _tag: "JoinRoom", roomName: "a's table", email: "b@x" })).toBe(
      '(join-room "a\'s table" "b@x")'
    );
    expect(encodeCommand({ _tag: "LeaveRoom", roomName: "a's table", email: "b@x" })).toBe(
      '(leave-room "a\'s table" "b@x")'
    );
    expect(
      encodeCommand({
        _tag: "KickFromRoom",
        roomName: "a's table",
        hostEmail: "a@x",
        targetEmail: "bot-1",
      })
    ).toBe('(kick-from-room "a\'s table" "a@x" "bot-1")');
    expect(encodeCommand({ _tag: "GetActiveRooms" })).toBe("(get-active-rooms)");
    expect(encodeCommand({ _tag: "Ping" })).toBe("(ping)");
  });

  it("encodes every command", () => {
    expect(encodeCommand({ _tag: "ConnectUser", email: "a@b", picUrl: "p.png" })).toBe(
      '(connect-user "a@b" "p.png")'
    );
    expect(encodeCommand({ _tag: "MakeRoom", hostEmail: "a@b", roomName: "r1" })).toBe('(make-room "a@b" "r1")');
    expect(encodeCommand({ _tag: "AddBot", roomName: "r1" })).toBe('(add-bot-to-room "r1")');
    expect(encodeCommand({ _tag: "StartGame", roomName: "r1" })).toBe('(start-game "r1")');
    expect(encodeCommand({ _tag: "PutBid", email: "a@b", bid: 17 })).toBe('(put-bid "a@b" 17)');
    expect(encodeCommand({ _tag: "PutBid", email: "a@b", bid: "pass" })).toBe('(put-bid "a@b" pass)');
    expect(encodeCommand({ _tag: "SelectTrump", email: "a@b", suit: "hearts" })).toBe(
      '(selected-trump "a@b" heart)'
    );
    expect(
      encodeCommand({
        _tag: "PlayCard",
        email: "a@b",
        card: { id: "clubs-J", suit: "clubs", rank: "J" },
      })
    ).toBe('(card-played "a@b" #s(card jack 7 club 3))');
    expect(encodeCommand({ _tag: "ExposeTrump", email: "a@b" })).toBe('(card-played "a@b" expose-trump)');
    expect(encodeCommand({ _tag: "NextHand", email: "a@b" })).toBe('(next-hand "a@b")');
    expect(encodeCommand({ _tag: "LeaveGame", email: "a@b" })).toBe('(leave-game "a@b")');
    expect(encodeCommand({ _tag: "Rejoin", email: "a@b" })).toBe('(rejoin "a@b")');
    expect(encodeCommand({ _tag: "SendEmote", email: "a@b", emote: "fire" })).toBe(
      '(emote "a@b" fire)'
    );
    expect(encodeCommand({ _tag: "ReplaceWithBot", email: "a@b" })).toBe('(replace-with-bot "a@b")');
    expect(encodeCommand({ _tag: "CloseGame", email: "a@b" })).toBe('(close-game "a@b")');
  });

  it("uses sym helper consistently", () => {
    expect(sym("x").name).toBe("x");
  });
});
