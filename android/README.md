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

## Phones without Vulkan 1.3

The table runs on Vulkan 1.3, and on 1.1 and 1.2 phones that have buffer addresses and scalar layouts,
through Apsis's fallback (`allow_vulkan_1_1` in `native/src/main.c`; the shaders build for `vulkan1.1`). The
fallback swaps dynamic rendering and synchronization2 for the original render passes and barriers. Bindless
textures (descriptor indexing) are optional: many Adreno 6xx and Mali-G76 phones lack them, and there Apsis
keeps a plain 32-slot texture table (`textures` in `native/src/main.c`). The manifest asks for Vulkan 1.1,
and Play can't tell which of those phones have the two features, so the app checks at launch:

- `GameActivity.onCreate` calls `TableBridge.gpuProblem()` before SDL starts. It runs Apsis's
  `gpu_check_support` with the table's own GPU settings (`native/src/gpu_desc.h`).
- A phone that fails sees "This phone can't run the table yet", the reason in small text, and **Play in
  browser**. That opens respawnfi.com in a Custom Tab, at the invited table when an invite link opened the
  app. SDL never starts, so nothing touches the GPU. The reason goes to logcat under `synth`, and nowhere else.

To test either way:

| Switch | What it does |
|---|---|
| `adb shell setprop debug.apsis.force_legacy 1` | A 1.3 phone takes the fallback, as if it were 1.1. A driver that has the features only as 1.2 ones, as the emulator's does, runs it at 1.2. |
| `adb shell setprop debug.apsis.force_no_indexing 1` | The phone is treated as lacking descriptor indexing, and the table uses the plain texture table. With `force_legacy` too, this is how an Adreno 620 or Mali-G76 phone runs. |
| `adb shell setprop debug.synth.force_unsupported 1` | In debug builds, any phone gets the "can't run the table" screen |
| `APSIS_FORCE_LEGACY=1 build/native/synth_table --demo` | The fallback on desktop. Add `APSIS_FORCE_NO_INDEXING=1` for the plain texture table. |

Set them back to 0 afterwards. The emulator images for Android 13 and older can't run the table on either
path: their GPU claims Vulkan 1.3 without its features, so the check sends them to the browser. Use Android
14 or later. `logcat -s apsis` shows which path a run took, on the `GPU path:` line.

## Diagnostics

The app sends a short JSON report to the game server, so problems on phones we don't have can be fixed. A
report holds the phone, its display, window and insets, the app's settings, and the table's own snapshot
from `native/src/diagnostics.h`: the GPU and its path, the swapchain, window and view sizes, the frame
rate, the last 12 taps with what each one hit, and the last 16 touches, including those the platform didn't
count as taps and why (moved too far, or held too long). `Diagnostics.kt` sends one 20 seconds after the table
starts, after each hand, and when the app goes to the background, at most once a minute. The "can't run
the table" screen sends one with the reason. No player ID or name goes in, and each report has a random
ID. Players turn reports off in the in-game menu (Diagnostics). The privacy policy
(`app/public/privacy/index.html`) lists what a report holds, so change the two together.

Reports go as `(diagnostics "<json>")` on a connection of their own. The server keeps them in SQLite for 30
days (`save-report!` in `storage.rkt`) and never logs one whole. To read the newest 20:

```sh
fly ssh console -a twenty-eight-game -C "racket /app/reports.rkt 20"
```

`synth_table --diagnostics` prints the table's snapshot on desktop. To see the app's reports without the
real server, run any WebSocket listener and start a debug build with
`adb shell am start -n com.respawnfi.twentyeight/dev.synth.app.GameActivity --es server ws://10.0.2.2:<port>`.

## The protocol version

The app sends `(client-hello "android" <version>)` when it connects, using `PROTOCOL_VERSION` in
`android/core`. If that's below the server's minimum, the server answers `update-required` and the app
shows a "please update" screen. Installed apps go stale while the server auto-deploys, so raise
`min-client-protocol` in `server.rkt` only when a change would break apps people already have, and only
once an updated app has been out long enough.

## The icon

`android/icon/make_icons.py` draws every icon from the app's ensō (`drawable-nodpi/enso.png`) and font:
the adaptive launcher icon with its themed (monochrome) layer, the Play Store icon and feature graphic in
`android/icon/store/`, and the website's icons and favicon. Edit the script and rerun it, rather than
editing the images. It needs Pillow.

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
