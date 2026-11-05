// Port of the pure logic in app/src/ui/PlayerProfile.ts. Keep the two in step.
package dev.synth.core

import kotlin.math.max

enum class Gender { WOMAN, MAN }

data class PlayerProfile(
    val memberId: String,
    val displayName: String,
    val avatar: String,
    val accent: String,
    val isBot: Boolean,
    val gender: Gender?,
)

private data class BotPersona(val name: String, val avatar: String, val accent: String, val gender: Gender)

// Warm, game-like personas replace implementation ids such as `bot-6`.
// The catalog is deliberately larger than a table so collision resolution
// can always give every seated bot a different face and name.
private val BOT_PERSONAS = listOf(
    BotPersona("Mira", "👩🏽‍🦱", "#b65f45", Gender.WOMAN),
    BotPersona("Arjun", "👨🏽‍🦱", "#c17a32", Gender.MAN),
    BotPersona("Tara", "👩🏻‍🦰", "#795c9d", Gender.WOMAN),
    BotPersona("Kabir", "👨🏻‍🦰", "#4f625f", Gender.MAN),
    BotPersona("Leela", "👩🏾‍🦳", "#657da1", Gender.WOMAN),
    BotPersona("Dev", "👨🏾‍🦳", "#876343", Gender.MAN),
    BotPersona("Asha", "👩🏿‍🦱", "#a65368", Gender.WOMAN),
    BotPersona("Nikhil", "👨🏿‍🦱", "#526b82", Gender.MAN),
    BotPersona("Zoya", "👩🏻‍🦱", "#8f5c9a", Gender.WOMAN),
    BotPersona("Ravi", "👨🏻‍🦱", "#b88632", Gender.MAN),
    BotPersona("Maya", "👩🏼‍🦰", "#5f7771", Gender.WOMAN),
    BotPersona("Ishan", "👨🏼‍🦰", "#477b63", Gender.MAN),
)

private val BOT_ID = Regex("bot-([0-9]+)")
private val ID_SUFFIX = Regex("-[a-z0-9]{6}\\z")

// A Double, like JS Number(): the persona slot below is taken with the same maths.
private fun botNumber(member: String): Double? =
    BOT_ID.matchEntire(member)?.groupValues?.get(1)?.toDouble()

private fun humanName(member: String): String =
    member.substringBefore('@').replace(ID_SUFFIX, "").ifEmpty { "Player" }

private fun humanAccent(member: String): String {
    var hash = 0L
    var i = 0
    while (i < member.length) {
        // JS iterates code points but hashes only the first UTF-16 unit of each
        hash = (hash * 31 + member[i].code) and 0xFFFFFFFFL
        i += if (member[i].isHighSurrogate() && member.getOrNull(i + 1)?.isLowSurrogate() == true) 2 else 1
    }
    return "hsl(${hash % 360} 28% 42%)"
}

/**
 * Resolve every public identity together so bots whose numeric ids map to
 * the same base persona are moved to the next free one. Sorting by the
 * stable numeric id makes the result independent of each client's seat view.
 */
fun playerProfiles(members: List<String>): Map<String, PlayerProfile> {
    val profiles = LinkedHashMap<String, PlayerProfile>()
    val bots = members.distinct()
        .mapNotNull { member -> botNumber(member)?.let { member to it } }
        .sortedWith(compareBy({ it.second }, { it.first }))
    val usedPersonas = mutableSetOf<Int>()

    for ((member, number) in bots) {
        val preferred = (max(0.0, number - 1) % BOT_PERSONAS.size).toInt()
        var personaIndex = preferred
        for (offset in BOT_PERSONAS.indices) {
            val candidate = (preferred + offset) % BOT_PERSONAS.size
            if (candidate !in usedPersonas) {
                personaIndex = candidate
                break
            }
        }
        usedPersonas.add(personaIndex)
        val persona = BOT_PERSONAS[personaIndex]
        profiles[member] = PlayerProfile(
            memberId = member,
            displayName = persona.name,
            avatar = persona.avatar,
            accent = persona.accent,
            isBot = true,
            gender = persona.gender,
        )
    }

    for (member in members) {
        if (member in profiles) continue
        val displayName = humanName(member)
        profiles[member] = PlayerProfile(
            memberId = member,
            displayName = displayName,
            avatar = displayName.take(1).uppercase().ifEmpty { "•" },
            accent = humanAccent(member),
            isBot = false,
            gender = null,
        )
    }

    return profiles
}

fun playerProfile(member: String, members: List<String> = listOf(member)): PlayerProfile {
    val scope = if (member in members) members else members + member
    return playerProfiles(scope).getValue(member)
}

fun memberDisplayName(member: String, members: List<String> = listOf(member)): String =
    playerProfile(member, members).displayName
