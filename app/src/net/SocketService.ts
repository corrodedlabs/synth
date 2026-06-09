import { Effect, Queue } from "effect";
import { ClientCommand, decodeServerEvent, encodeCommand, ServerEvent } from "./protocol";

export interface GameSocket {
  readonly events: Queue.Queue<ServerEvent>;
  readonly send: (command: ClientCommand) => void;
  readonly close: () => void;
}

export class SocketError {
  readonly _tag = "SocketError";
  constructor(readonly message: string) {}
}

export const connectSocket = (url: string): Effect.Effect<GameSocket, SocketError> =>
  Effect.gen(function* () {
    const events = yield* Queue.unbounded<ServerEvent>();

    const ws = yield* Effect.async<WebSocket, SocketError>((resume) => {
      const socket = new WebSocket(url);
      socket.onopen = () => resume(Effect.succeed(socket));
      socket.onerror = () =>
        resume(Effect.fail(new SocketError(`could not connect to ${url} — is the game server running?`)));
    });

    ws.onerror = null;
    ws.onmessage = (message) => {
      const event = decodeServerEvent(String(message.data));
      if (event) {
        console.debug("[ws] ←", event);
        Queue.unsafeOffer(events, event);
      }
    };
    ws.onclose = () => {
      Queue.unsafeOffer(events, { _tag: "Disconnected" });
    };

    return {
      events,
      send: (command: ClientCommand) => {
        const frame = encodeCommand(command);
        console.debug("[ws] →", frame);
        ws.send(frame);
      },
      close: () => {
        ws.onclose = null;
        ws.close();
      },
    };
  });
