plugins {
    id("org.jetbrains.kotlin.jvm")
}
kotlin { jvmToolchain(21) }
dependencies {
    implementation("org.jetbrains.kotlinx:kotlinx-coroutines-core:1.10.2")
    testImplementation(kotlin("test"))
    testImplementation("org.jetbrains.kotlinx:kotlinx-coroutines-test:1.10.2")
    // parses app/test-data/protocol-cases.json; JsonElement needs no compiler plugin
    testImplementation("org.jetbrains.kotlinx:kotlinx-serialization-json:1.9.0")
}
tasks.test { useJUnitPlatform() }
