plugins {
    id("com.android.application")
    id("org.jetbrains.kotlin.android")
    id("org.jetbrains.kotlin.plugin.compose")
}

// Release signing comes from the environment, so keys never enter the repo: see android/README.md.
val keystore: String? = System.getenv("SYNTH_KEYSTORE")

android {
    namespace = "dev.synth.app"
    compileSdk = 36
    ndkVersion = "27.1.12297006"

    defaultConfig {
        // The ID Play and the phone see. It can never change after the first upload. The Kotlin
        // package (namespace) stays dev.synth.app, which the JNI names in native/src/jni.c depend on.
        applicationId = "com.respawnfi.twentyeight"
        minSdk = 29
        targetSdk = 36
        versionCode = (System.getenv("SYNTH_VERSION_CODE") ?: "1").toInt()
        versionName = "0.1"
        // Phones are arm64. Add x86_64 with -Psynth.abis=x86_64 to test on an emulator.
        ndk { abiFilters += (project.findProperty("synth.abis") as String? ?: "arm64-v8a").split(",") }
        externalNativeBuild {
            cmake {
                arguments += listOf("-DANDROID_STL=c++_static", "-DANDROID_SUPPORT_FLEXIBLE_PAGE_SIZES=ON")
                targets += listOf("game", "SDL3-shared")
            }
        }
        buildConfigField("String", "SERVER_URL", "\"wss://twenty-eight-game.fly.dev/test\"")
    }
    signingConfigs {
        if (keystore != null) {
            create("release") {
                storeFile = file(keystore)
                storePassword = System.getenv("SYNTH_KEYSTORE_PASSWORD")
                keyAlias = System.getenv("SYNTH_KEY_ALIAS")
                keyPassword = System.getenv("SYNTH_KEY_PASSWORD")
            }
        }
    }
    buildTypes {
        release {
            isMinifyEnabled = false
            if (keystore != null) signingConfig = signingConfigs.getByName("release")
        }
    }
    buildFeatures {
        compose = true
        buildConfig = true
    }
    compileOptions {
        sourceCompatibility = JavaVersion.VERSION_17
        targetCompatibility = JavaVersion.VERSION_17
    }
    kotlinOptions { jvmTarget = "17" }
    externalNativeBuild {
        cmake {
            path = file("../../native/CMakeLists.txt")
            version = "3.22.1"
        }
    }
    sourceSets {
        getByName("main").assets.srcDir("build/generated/synth/assets")
        getByName("debug").jniLibs.srcDir("build/generated/synth/layers")
    }
    packaging {
        // The Vulkan loader looks for layers as files in the app's library folder.
        jniLibs.useLegacyPackaging = true
    }
}

// Assets and layers appear when CMake configures, so the merge steps have to wait for it.
afterEvaluate {
    listOf("Debug", "Release").forEach { variant ->
        tasks.matching { it.name == "merge${variant}Assets" || it.name == "merge${variant}JniLibFolders" }
            .configureEach { dependsOn("externalNativeBuild$variant") }
    }
}

dependencies {
    implementation(project(":core"))
    implementation(platform("androidx.compose:compose-bom:2025.06.01"))
    implementation("androidx.compose.ui:ui")
    implementation("androidx.compose.foundation:foundation")
    implementation("androidx.compose.material3:material3")
    implementation("androidx.lifecycle:lifecycle-runtime-ktx:2.9.1")
    implementation("androidx.savedstate:savedstate-ktx:1.3.0")
    implementation("com.squareup.okhttp3:okhttp:4.12.0")
    implementation("org.jetbrains.kotlinx:kotlinx-coroutines-android:1.10.2")
    implementation("androidx.core:core-ktx:1.16.0")
}
