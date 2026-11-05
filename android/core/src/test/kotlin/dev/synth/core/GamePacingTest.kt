package dev.synth.core

import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertTrue

// Port of app/src/game/GamePacing.test.ts, case for case.
class GamePacingTest {
    @Test
    fun `keeps each normal-speed presentation stage in order`() {
        val pacing = gamePacing()
        assertEquals(3_000, pacing.bidPauseMs)
        assertTrue(pacing.cardPauseMs > pacing.cardTravelMs)
        assertTrue(pacing.dealCompleteMs > pacing.dealCardStaggerMs * 3)
        assertTrue(pacing.trickResolveMs >= pacing.winnerRevealMs + pacing.cardCollectMs + pacing.scoreLandingMs)
        assertTrue(pacing.trickReadMs > 1_000)
    }

    @Test
    fun `speeds up quick mode without removing the winner moment`() {
        val standard = gamePacing(GameSpeed.STANDARD)
        val quick = gamePacing(GameSpeed.QUICK)
        val relaxed = gamePacing(GameSpeed.RELAXED)
        assertEquals(2_160, quick.bidPauseMs)
        assertTrue(quick.bidPauseMs < standard.bidPauseMs)
        assertTrue(standard.bidPauseMs < relaxed.bidPauseMs)
        assertTrue(quick.cardTravelMs < standard.cardTravelMs)
        assertTrue(quick.winnerRevealMs > 0)
        assertTrue(quick.cardCollectMs > 0)
    }

    @Test
    fun `keeps a readable outcome without movement for reduced motion`() {
        val pacing = gamePacing(GameSpeed.QUICK, true)
        assertTrue(pacing.reducedMotion)
        assertEquals(0, pacing.cardTravelMs)
        assertEquals(0, pacing.cardCollectMs)
        assertEquals(0, pacing.dealCompleteMs)
        assertEquals(3_000, pacing.bidPauseMs)
        assertTrue(pacing.winnerRevealMs >= 900)
    }

    // Not in the TS suite: exact values printed by the TS gamePacing() under
    // node. Relaxed lands on x.5 values, where Kotlin's round() would differ.
    @Test
    fun `matches the web profile to the millisecond`() {
        assertEquals(RELAXED_WEB, gamePacing(GameSpeed.RELAXED).values())
        assertEquals(STANDARD_WEB, gamePacing(GameSpeed.STANDARD).values())
        assertEquals(QUICK_WEB, gamePacing(GameSpeed.QUICK).values())
    }

    @Test
    fun `cycles and saves the speed, and reads unknown stored values as standard`() {
        val saved = mutableListOf<GameSpeed>()
        val pacing = GamePacing(parseGameSpeed("nonsense"), saveSpeed = { saved.add(it) })
        assertEquals(GameSpeed.STANDARD, pacing.profile().speed)
        assertEquals(GameSpeed.QUICK, pacing.cycleSpeed().speed)
        assertEquals(GameSpeed.RELAXED, pacing.cycleSpeed().speed)
        assertEquals(listOf(GameSpeed.QUICK, GameSpeed.RELAXED), saved)
        assertEquals(GameSpeed.QUICK, parseGameSpeed("quick"))
    }

    private val RELAXED_WEB = listOf<Long>(3750, 875, 103, 847, 1500, 2018, 700, 775, 813, 350)
    private val STANDARD_WEB = listOf<Long>(3000, 700, 82, 676, 1200, 1630, 560, 620, 650, 280)
    private val QUICK_WEB = listOf<Long>(2160, 504, 59, 487, 864, 1196, 403, 446, 468, 202)

    private fun GamePacingProfile.values() = listOf(
        bidPauseMs, cardPauseMs, dealCardStaggerMs, dealCompleteMs, trickReadMs, trickResolveMs,
        cardTravelMs, winnerRevealMs, cardCollectMs, scoreLandingMs,
    )
}
