/// <reference types="vite/client" />

interface ImportMetaEnv {
  /** WebSocket URL of the game server, e.g. wss://my-app.fly.dev/test */
  readonly VITE_SERVER_URL?: string;
}

interface ImportMeta {
  readonly env: ImportMetaEnv;
}
