// Port of app/src/game/GamePacing.ts. Keep the two in step.
package dev.synth.core

import kotlin.math.max

enum class GameSpeed(val id: String) { RELAXED("relaxed"), STANDARD("standard"), QUICK("quick") }

data class GamePacingProfile(
    val speed: GameSpeed,
    val reducedMotion: Boolean,
    // Event-loop delays. These ensure a server event cannot visually overtake
    // the animation that explains the preceding event.
    val bidPauseMs: Long,
    val cardPauseMs: Long,
    val dealCardStaggerMs: Long,
    val dealCompleteMs: Long,
    val trickReadMs: Long,
    val trickResolveMs: Long,
    // Individual presentation milestones.
    val cardTravelMs: Long,
    val winnerRevealMs: Long,
    val cardCollectMs: Long,
    val scoreLandingMs: Long,
)

private fun multiplier(speed: GameSpeed): Double = when (speed) {
    GameSpeed.RELAXED -> 1.25
    GameSpeed.STANDARD -> 1.0
    GameSpeed.QUICK -> 0.72
}

// Math.round like JS (half goes up): kotlin.math.round rounds half to even,
// which would turn 82 * 1.25 into 102 here and 103 on the web.
private fun round(value: Double): Long = Math.round(value)

// A pure profile factory keeps pacing consistent between the event loop and
// presentation. Reduced motion preserves the outcome reading time while
// removing flights and sweeps.
fun gamePacing(speed: GameSpeed = GameSpeed.STANDARD, reducedMotion: Boolean = false): GamePacingProfile {
    if (reducedMotion) {
        return GamePacingProfile(
            speed = speed,
            reducedMotion = true,
            // Removing motion must not remove the time needed to read an auction.
            bidPauseMs = 3_000,
            cardPauseMs = 160,
            dealCardStaggerMs = 0,
            dealCompleteMs = 0,
            trickReadMs = 900,
            trickResolveMs = 920,
            cardTravelMs = 0,
            winnerRevealMs = 900,
            cardCollectMs = 0,
            scoreLandingMs = 0,
        )
    }

    val scale = multiplier(speed)
    val cardTravelMs = round(560 * scale)
    val dealCardStaggerMs = round(82 * scale)
    val winnerRevealMs = round(620 * scale)
    val cardCollectMs = round(650 * scale)
    val scoreLandingMs = round(280 * scale)

    return GamePacingProfile(
        speed = speed,
        reducedMotion = false,
        bidPauseMs = round(3_000 * scale),
        // A card must finish its flight before a bot can visually answer.
        cardPauseMs = max(cardTravelMs + 80, round(700 * scale)),
        // The server sends two four-card deals. Let each fan settle before the
        // next auction or trump event is presented.
        dealCardStaggerMs = dealCardStaggerMs,
        dealCompleteMs = dealCardStaggerMs * 3 + round(430 * scale),
        // All four cards remain readable before the winner is announced.
        trickReadMs = round(1_200 * scale),
        // Winner reveal plus collection must finish before the next lead.
        trickResolveMs = winnerRevealMs + cardCollectMs + scoreLandingMs + 80,
        cardTravelMs = cardTravelMs,
        winnerRevealMs = winnerRevealMs,
        cardCollectMs = cardCollectMs,
        scoreLandingMs = scoreLandingMs,
    )
}

// What a stored speed setting reads as: anything unknown is standard.
fun parseGameSpeed(value: String?): GameSpeed =
    GameSpeed.entries.firstOrNull { it.id == value } ?: GameSpeed.STANDARD

// This controller is presentation-only: changing speed cannot alter an
// authoritative card, score, or server turn. The platform supplies what the
// web version read from localStorage and matchMedia.
class GamePacing(
    initialSpeed: GameSpeed = GameSpeed.STANDARD,
    private val reducedMotion: () -> Boolean = { false },
    private val saveSpeed: (GameSpeed) -> Unit = {},
) {
    private var speed = initialSpeed

    fun profile(): GamePacingProfile = gamePacing(speed, reducedMotion())

    fun setSpeed(speed: GameSpeed) {
        this.speed = speed
        saveSpeed(speed)
    }

    fun cycleSpeed(): GamePacingProfile {
        val speeds = GameSpeed.entries
        setSpeed(speeds[(speeds.indexOf(speed) + 1) % speeds.size])
        return profile()
    }
}
