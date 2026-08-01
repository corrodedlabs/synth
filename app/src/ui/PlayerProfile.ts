export interface PlayerProfile {
  readonly memberId: string;
  readonly displayName: string;
  readonly avatar: string;
  readonly accent: string;
  readonly isBot: boolean;
  readonly gender: "woman" | "man" | null;
}

interface BotPersona {
  readonly name: string;
  readonly avatar: string;
  readonly accent: string;
  readonly gender: "woman" | "man";
}

// Warm, game-like personas replace implementation ids such as `bot-6`.
// The catalog is deliberately larger than a table so collision resolution
// can always give every seated bot a different face and name.
const BOT_PERSONAS: readonly BotPersona[] = [
  { name: "Mira", avatar: "👩🏽‍🦱", accent: "#b65f45", gender: "woman" },
  { name: "Arjun", avatar: "👨🏽‍🦱", accent: "#c17a32", gender: "man" },
  { name: "Tara", avatar: "👩🏻‍🦰", accent: "#795c9d", gender: "woman" },
  { name: "Kabir", avatar: "👨🏻‍🦰", accent: "#4f625f", gender: "man" },
  { name: "Leela", avatar: "👩🏾‍🦳", accent: "#657da1", gender: "woman" },
  { name: "Dev", avatar: "👨🏾‍🦳", accent: "#876343", gender: "man" },
  { name: "Asha", avatar: "👩🏿‍🦱", accent: "#a65368", gender: "woman" },
  { name: "Nikhil", avatar: "👨🏿‍🦱", accent: "#526b82", gender: "man" },
  { name: "Zoya", avatar: "👩🏻‍🦱", accent: "#8f5c9a", gender: "woman" },
  { name: "Ravi", avatar: "👨🏻‍🦱", accent: "#b88632", gender: "man" },
  { name: "Maya", avatar: "👩🏼‍🦰", accent: "#5f7771", gender: "woman" },
  { name: "Ishan", avatar: "👨🏼‍🦰", accent: "#477b63", gender: "man" },
];

function botNumber(member: string): number | null {
  const match = member.match(/^bot-(\d+)$/);
  return match ? Number(match[1]) : null;
}

function humanName(member: string): string {
  return member.split("@")[0].replace(/-[a-z0-9]{6}$/, "") || "Player";
}

function humanAccent(member: string): string {
  let hash = 0;
  for (const character of member) hash = (hash * 31 + character.charCodeAt(0)) >>> 0;
  return `hsl(${hash % 360} 28% 42%)`;
}

/**
 * Resolve every public identity together so bots whose numeric ids map to
 * the same base persona are moved to the next free one. Sorting by the
 * stable numeric id makes the result independent of each client's seat view.
 */
export function playerProfiles(members: readonly string[]): ReadonlyMap<string, PlayerProfile> {
  const profiles = new Map<string, PlayerProfile>();
  const bots = [...new Set(members)]
    .map((member) => ({ member, number: botNumber(member) }))
    .filter((entry): entry is { member: string; number: number } => entry.number !== null)
    .sort((left, right) => left.number - right.number || left.member.localeCompare(right.member));
  const usedPersonas = new Set<number>();

  for (const { member, number } of bots) {
    const preferred = Math.max(0, number - 1) % BOT_PERSONAS.length;
    let personaIndex = preferred;
    for (let offset = 0; offset < BOT_PERSONAS.length; offset++) {
      const candidate = (preferred + offset) % BOT_PERSONAS.length;
      if (!usedPersonas.has(candidate)) {
        personaIndex = candidate;
        break;
      }
    }
    usedPersonas.add(personaIndex);
    const persona = BOT_PERSONAS[personaIndex];
    profiles.set(member, {
      memberId: member,
      displayName: persona.name,
      avatar: persona.avatar,
      accent: persona.accent,
      isBot: true,
      gender: persona.gender,
    });
  }

  for (const member of members) {
    if (profiles.has(member)) continue;
    const displayName = humanName(member);
    profiles.set(member, {
      memberId: member,
      displayName,
      avatar: displayName.slice(0, 1).toUpperCase() || "•",
      accent: humanAccent(member),
      isBot: false,
      gender: null,
    });
  }

  return profiles;
}

export function playerProfile(
  member: string,
  members: readonly string[] = [member]
): PlayerProfile {
  const scope = members.includes(member) ? members : [...members, member];
  return playerProfiles(scope).get(member) as PlayerProfile;
}

export function memberDisplayName(member: string, members?: readonly string[]): string {
  return playerProfile(member, members).displayName;
}
