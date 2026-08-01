import { describe, expect, it } from "vitest";
import { memberDisplayName, playerProfile, playerProfiles } from "./PlayerProfile";

describe("player profiles", () => {
  it("replaces internal bot ids with a friendly persona", () => {
    const profile = playerProfile("bot-6");
    expect(profile).toMatchObject({
      displayName: "Dev",
      avatar: "👨🏾‍🦳",
      gender: "man",
      isBot: true,
    });
    expect(profile.displayName).not.toMatch(/^Bot \d+$/);
  });

  it("matches every bot name to the gender of its people avatar", () => {
    const expected = [
      ["Mira", "woman"], ["Arjun", "man"], ["Tara", "woman"], ["Kabir", "man"],
      ["Leela", "woman"], ["Dev", "man"], ["Asha", "woman"], ["Nikhil", "man"],
      ["Zoya", "woman"], ["Ravi", "man"], ["Maya", "woman"], ["Ishan", "man"],
    ];
    expected.forEach(([displayName, gender], index) => {
      expect(playerProfile(`bot-${index + 1}`)).toMatchObject({ displayName, gender });
    });
  });

  it("keeps bot personas unique within a table even when base slots collide", () => {
    const members = ["bot-13", "bot-1", "host-abc123@game.local"];
    const profiles = playerProfiles(members);
    expect(profiles.get("bot-1")?.displayName).toBe("Mira");
    expect(profiles.get("bot-13")?.displayName).toBe("Arjun");
    expect(profiles.get("bot-1")?.avatar).not.toBe(profiles.get("bot-13")?.avatar);
  });

  it("is deterministic regardless of member ordering", () => {
    const forward = playerProfiles(["bot-13", "bot-1"]);
    const reverse = playerProfiles(["bot-1", "bot-13"]);
    expect(forward.get("bot-13")).toEqual(reverse.get("bot-13"));
    expect(forward.get("bot-1")).toEqual(reverse.get("bot-1"));
  });

  it("preserves human display names and gives them a monogram", () => {
    expect(memberDisplayName("antash-x7k2p9@game.local")).toBe("antash");
    expect(playerProfile("antash-x7k2p9@game.local")).toMatchObject({
      displayName: "antash",
      avatar: "A",
      isBot: false,
    });
  });
});
