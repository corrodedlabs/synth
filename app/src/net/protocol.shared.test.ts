import { describe, expect, it } from "vitest";
import shared from "../../test-data/protocol-cases.json";
import { decodeServerEvent, ServerEvent } from "./protocol";

// The same cases run against the Kotlin decoder in
// android/core/src/test/kotlin/dev/synth/core/SharedProtocolCasesTest.kt.
// The file's "about" field describes the format.

interface ProtocolCase {
  readonly wire: string;
  readonly expected: unknown;
  readonly webExpected?: unknown;
}

const cases: readonly ProtocolCase[] = shared.cases;

// A Record over the union, so a new event without a tag here fails `tsc`.
const WIRE_TAGS: Record<Exclude<ServerEvent["_tag"], "Disconnected">, true> = {
  RoomCreated: true,
  RoomJoined: true,
  RoomLeft: true,
  RoomClosed: true,
  RemovedFromRoom: true,
  ActiveRooms: true,
  StartGameFailed: true,
  RoomMembers: true,
  GameStarted: true,
  GameAborted: true,
  HandDealt: true,
  Turn: true,
  BidRequested: true,
  BidPlaced: true,
  BidResult: true,
  ChooseTrumpRequested: true,
  TrumpSelected: true,
  CardPlayed: true,
  TrumpExposed: true,
  PlayRequested: true,
  PointsWon: true,
  HandResult: true,
  MatchOver: true,
  PlayerDisconnected: true,
  PlayerReconnected: true,
  EmotePlayed: true,
  SeatAbandoned: true,
  SeatReplaced: true,
  GameSnapshot: true,
  NoRunningGame: true,
  Leaderboard: true,
  ServerError: true,
  Ignored: true,
};

// Maps become objects keyed by string; JSON has no other way to spell them.
function jsonForm(value: unknown): unknown {
  return JSON.parse(
    JSON.stringify(value, (_key, item) => (item instanceof Map ? Object.fromEntries(item) : item))
  );
}

describe("shared protocol cases", () => {
  it.each(cases)("decodes $wire", ({ wire, expected, webExpected }) => {
    expect(jsonForm(decodeServerEvent(wire))).toEqual(webExpected ?? expected);
  });

  it("covers every event with a wire form", () => {
    const seen = new Set(
      cases.map((entry) => ((entry.webExpected ?? entry.expected) as { _tag?: string } | null)?._tag)
    );
    for (const tag of Object.keys(WIRE_TAGS)) {
      expect(seen, tag).toContain(tag);
    }
  });
});
