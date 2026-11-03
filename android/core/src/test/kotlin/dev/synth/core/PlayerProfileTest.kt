package dev.synth.core

import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFalse
import kotlin.test.assertNotEquals
import kotlin.test.assertTrue

// Port of app/src/ui/PlayerProfile.test.ts, case for case.
class PlayerProfileTest {
    @Test
    fun `replaces internal bot ids with a friendly persona`() {
        val profile = playerProfile("bot-6")
        assertEquals("Dev", profile.displayName)
        assertEquals("👨🏾‍🦳", profile.avatar)
        assertEquals(Gender.MAN, profile.gender)
        assertTrue(profile.isBot)
    }

    @Test
    fun `matches every bot name to the gender of its people avatar`() {
        val expected = listOf(
            "Mira" to Gender.WOMAN, "Arjun" to Gender.MAN, "Tara" to Gender.WOMAN, "Kabir" to Gender.MAN,
            "Leela" to Gender.WOMAN, "Dev" to Gender.MAN, "Asha" to Gender.WOMAN, "Nikhil" to Gender.MAN,
            "Zoya" to Gender.WOMAN, "Ravi" to Gender.MAN, "Maya" to Gender.WOMAN, "Ishan" to Gender.MAN,
        )
        expected.forEachIndexed { index, (name, gender) ->
            val profile = playerProfile("bot-${index + 1}")
            assertEquals(name, profile.displayName)
            assertEquals(gender, profile.gender)
        }
    }

    @Test
    fun `keeps bot personas unique within a table even when base slots collide`() {
        val profiles = playerProfiles(listOf("bot-13", "bot-1", "host-abc123@game.local"))
        assertEquals("Mira", profiles["bot-1"]?.displayName)
        assertEquals("Arjun", profiles["bot-13"]?.displayName)
        assertNotEquals(profiles["bot-1"]?.avatar, profiles["bot-13"]?.avatar)
    }

    @Test
    fun `is deterministic regardless of member ordering`() {
        val forward = playerProfiles(listOf("bot-13", "bot-1"))
        val reverse = playerProfiles(listOf("bot-1", "bot-13"))
        assertEquals(forward["bot-13"], reverse["bot-13"])
        assertEquals(forward["bot-1"], reverse["bot-1"])
    }

    @Test
    fun `preserves human display names and gives them a monogram`() {
        assertEquals("antash", memberDisplayName("antash-x7k2p9@game.local"))
        val profile = playerProfile("antash-x7k2p9@game.local")
        assertEquals("antash", profile.displayName)
        assertEquals("A", profile.avatar)
        assertFalse(profile.isBot)
    }
}
