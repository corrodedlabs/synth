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

You need Android SDK platform 36, NDK 27.1.12297006 and CMake 3.22.1, Java 21, and read access to Apsis.

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

The upload key and all four GitHub Actions signing secrets were configured on 2026-11-05. The key
backup and credentials are kept outside this repository. Keep that backup: future uploads must use
the same upload key unless it is reset in Play Console. Never put the key in a build sandbox.

The app is `com.respawnfi.twentyeight` (its Kotlin package stays `dev.synth.app`), targets Android 16 (API 36), and builds for arm64 phones by default.
Version `0.1` starts at version code `1`; every later upload needs a higher version code.

To release through GitHub Actions:

1. Create the app in Play Console. The first upload fixes its ID as `com.respawnfi.twentyeight`.
2. Enable Play App Signing, letting Google create the app signing key. The configured key is the
   separate upload key, used to authenticate bundles sent to Play.
3. Run the Android workflow by hand on `master` with a version code. It publishes a GitHub pre-release,
   tagged `android-v<versionName>-<versionCode>`, with the signed bundle, its checksum and the commits since
   the last release. A run from any other branch only leaves the bundle as an artifact.
4. Upload the bundle to the internal testing track. Play needs the first upload done by hand.

For a cloud build, leave the `SYNTH_KEYSTORE*` variables unset and run `./gradlew :app:bundleRelease`.
Copy `app/build/outputs/bundle/release/app-release.aab` back to the trusted local machine, load the
signing environment there, and sign it with `jarsigner` (not `apksigner`):

```sh
jarsigner -keystore "$SYNTH_KEYSTORE" -storepass:env SYNTH_KEYSTORE_PASSWORD \
  -keypass:env SYNTH_KEY_PASSWORD -sigalg SHA256withRSA -digestalg SHA-256 \
  -signedjar synth-release.aab app-release.aab "$SYNTH_KEY_ALIAS"
```

Validate the signed AAB with Google's `bundletool validate` and check its signature with
`jarsigner -verify` before uploading. The release must not contain Vulkan validation layers.

Invite links open the app because `app/public/.well-known/assetlinks.json`, served at
`https://respawnfi.com/.well-known/assetlinks.json`, lists Play's app-signing certificate (Play Console →
App integrity → App signing). Builds signed another way, such as debug builds, aren't covered, so their
links open in the browser unless the app is allowed under Settings → Apps → 28 → Open by default.

The release app talks to `wss://twenty-eight-game.fly.dev/test`, so deploy the server's `client-hello`
before anyone installs a build. An older server answers `invalid-request`, which the app ignores.
