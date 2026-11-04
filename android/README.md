# 28 on Android

The app has two halves. Kotlin holds the session, networking and menus. C17 draws the table through
the Apsis engine (the `apsis` submodule, a private repo). The server still holds every rule.

| Folder | What's there |
|---|---|
| `android/core` | The web client's protocol, reducer and session, in plain Kotlin |
| `android/app` | The activity, Compose screens, WebSocket client and saved identity |
| `native` | The table: card layout, animation, sound and the JNI bridge. Also runs on desktop. |
| `app/test-data` | Protocol cases that both the web and Kotlin tests read |

## Building

You need the Android SDK with NDK 27.1.12297006 and CMake 3.22.1, Java 21, and read access to Apsis.

```sh
git submodule update --init apsis
cd android
./gradlew :core:test              # the game core, no device needed
./gradlew installDebug            # arm64 phones
./gradlew installDebug -Psynth.abis=x86_64   # an x86_64 emulator
```

The table alone, on desktop (needs `glslc`):

```sh
cmake -S native -B build/native -G Ninja
cmake --build build/native && ctest --test-dir build/native
build/native/synth_table --demo
```

The native table takes the commands listed in `native/src/table.h`. Rerun `app/scripts/bake-atlas.mjs`
when the card art changes. The copied Java in `org/libsdl/app` must match the SDL version Apsis pins.

## Testing

`app/scripts/test-android-match.mjs` plays a whole match between the app and a browser through a real
server, with real taps, including a dropped socket and a trip to the background. Its header shows how
to run it.

An emulator proves behaviour and not speed: it draws with a software GPU. It needs a software GPU flag,
`-gpu swiftshader_indirect`, because `-gpu host` crashed at startup with `-no-window`. Timing belongs
on a real phone, with `adb shell setprop debug.apsis.validation 0`, since debug builds run with
validation on.

## The protocol version

The app sends `(client-hello "android" <version>)` when it connects, using `PROTOCOL_VERSION` in
`android/core`. If that's below the server's minimum, the server answers `update-required` and the app
shows a "please update" screen. Installed apps go stale while the server auto-deploys, so raise
`min-client-protocol` in `server.rkt` only when a change would break apps people already have, and only
once an updated app has been out long enough.

## Releasing

Nothing here has been released yet, and these steps need the owner:

1. Create the app in Play Console as `dev.synth.app`.
2. Create an upload key, and keep it out of this repo and out of sandboxes.
3. Add the secrets listed at the top of `.github/workflows/android.yml`.
4. Run the Android workflow by hand with a version code. It produces a signed bundle as an artifact.
5. Upload the bundle to the internal testing track. Play needs the first upload done by hand.

For invite links to open the app without asking, publish `/.well-known/assetlinks.json` on
`respawnfi.com` with the app's signing certificate. Until then Android asks which app to use.

The release app talks to `wss://twenty-eight-game.fly.dev/test`, so deploy the server's `client-hello`
before anyone installs a build. An older server answers `invalid-request`, which the app ignores.
