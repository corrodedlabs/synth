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
    expect(decodeCard(parseSExpr("#s(card ten 3 heart 1)"))).toEqual({
      id: "hearts-10",
      suit: "hearts",
      rank: "10",
    });
  });

  it("re-encodes cards exactly as game.rkt builds them", () => {
    const samples = [
      "#s(card jack 7 club 3)",
      "#s(card queen 6 diamond 0)",
      "#s(card king 5 heart 0)",
      "#s(card ace 4 spade 1)",
      "#s(card ten 3 club 1)",
      "#s(card nine 2 diamond 2)",
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
    expect(decodeServerEvent("(request-bid 16)")).toEqual({ _tag: "BidRequested", currentBid: 16 });
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

  it("decodes play requests", () => {
    expect(
      decodeServerEvent(
        "(play-card (cards-played #s(card ace 4 spade 1)) (game-state (trump-suit . #f) (first-suit . spade)))"
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
  });

  it("uses sym helper consistently", () => {
    expect(sym("x").name).toBe("x");
  });
});
