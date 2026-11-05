#include "sound.h"

#include <math.h>
#include <stdlib.h>
#include <string.h>

#define PI 3.14159265358979f
#define RATE AUDIO_RATE

/* Sound.ts's master gain. Peaks below are the same numbers as its recipes. */
#define MASTER 0.6f

typedef struct {
    float *out;
    int count;
} Clip;

/* A rise over 10 ms, then an exponential fall to almost nothing by the end. */
static float envelope(float t, float attack, float duration, float peak)
{
    if (t < attack)
        return peak * t / attack;
    return peak * powf(0.0001f / peak, (t - attack) / (duration - attack));
}

static void tone(Clip *c, float delay, float freq, float duration, float peak, bool triangle)
{
    int start = (int)(delay * RATE), n = (int)(duration * RATE);
    for (int i = 0; i < n && start + i < c->count; i++) {
        float t = (float)i / RATE;
        float phase = freq * t;
        float wave = triangle ? 4 * fabsf(phase - floorf(phase + 0.5f)) - 1 : sinf(2 * PI * phase);
        c->out[start + i] += wave * envelope(t, 0.01f, duration, peak) * MASTER;
    }
}

/* Noise through a band-pass filter that falls from one frequency to another: paper on felt. */
static void swish(Clip *c, float delay, float duration, float from, float to, float peak)
{
    int start = (int)(delay * RATE), n = (int)(duration * RATE);
    float z1 = 0, z2 = 0;
    uint32_t seed = 0x28282828u + (uint32_t)(from * 7);
    for (int i = 0; i < n && start + i < c->count; i++) {
        float t = (float)i / RATE;
        float freq = from * powf(to / from, t / duration);
        float w = 2 * PI * freq / RATE, alpha = sinf(w) / (2 * 0.9f), a0 = 1 + alpha;
        float b0 = alpha / a0, a1 = -2 * cosf(w) / a0, a2 = (1 - alpha) / a0;
        seed = seed * 1664525u + 1013904223u;
        float x = (float)(seed >> 8) / 8388608.0f - 1;
        /* Transposed direct form II, keeping its state while the coefficients slide. */
        float y = b0 * x + z1;
        z1 = z2 - a1 * y;
        z2 = -b0 * x - a2 * y;
        c->out[start + i] += y * envelope(t, 0.008f, duration, peak) * MASTER;
    }
}

typedef void Recipe(Clip *c);

static void r_deal(Clip *c)
{
    for (int i = 0; i < 4; i++)
        swish(c, i * 0.07f, 0.05f, 2600, 1400, 0.05f);
}
static void r_card(Clip *c)
{
    swish(c, 0, 0.045f, 2000, 900, 0.07f);
    tone(c, 0, 150, 0.06f, 0.05f, false);
}
static void r_sweep(Clip *c)
{
    swish(c, 0, 0.3f, 2400, 350, 0.06f);
}
static void r_turn(Clip *c)
{
    tone(c, 0, 659.25f, 0.1f, 0.035f, false);
    tone(c, 0.09f, 880, 0.16f, 0.03f, false);
}
static void r_emote(Clip *c)
{
    tone(c, 0, 740, 0.07f, 0.03f, true);
    tone(c, 0.06f, 988, 0.09f, 0.025f, true);
}
static void r_bid(Clip *c)
{
    tone(c, 0, 980, 0.04f, 0.03f, true);
}
static void r_trump(Clip *c)
{
    const float notes[3] = {523.25f, 659.25f, 783.99f};
    for (int i = 0; i < 3; i++)
        tone(c, i * 0.07f, notes[i], 0.22f, 0.03f, false);
}
static void r_win(Clip *c)
{
    const float notes[4] = {523.25f, 659.25f, 783.99f, 1046.5f};
    for (int i = 0; i < 4; i++)
        tone(c, i * 0.12f, notes[i], 0.3f, 0.035f, false);
}
static void r_lose(Clip *c)
{
    tone(c, 0, 196, 0.4f, 0.04f, false);
    tone(c, 0.18f, 185, 0.5f, 0.035f, false);
}
static void r_click(Clip *c)
{
    tone(c, 0, 1180, 0.05f, 0.04f, true);
    tone(c, 0, 320, 0.04f, 0.025f, false);
}
static void r_fanfare_win(Clip *c)
{
    const float notes[4] = {523.25f, 659.25f, 783.99f, 1046.5f};
    for (int i = 0; i < 4; i++)
        tone(c, i * 0.16f, notes[i], i == 3 ? 0.7f : 0.24f, 0.06f, false);
    tone(c, 0.48f, 659.25f, 0.7f, 0.03f, false);
    tone(c, 0.48f, 783.99f, 0.7f, 0.03f, false);
}
static void r_fanfare_lose(Clip *c)
{
    tone(c, 0, 392, 0.45f, 0.045f, false);
    tone(c, 0.3f, 311.13f, 0.8f, 0.04f, false);
}

static int bake(Audio *audio, Recipe *recipe, float seconds)
{
    Clip c = {.count = (int)(seconds * RATE)};
    c.out = calloc((size_t)c.count, sizeof *c.out);
    if (!c.out)
        return -1;
    recipe(&c);
    int clip = audio_add_clip(audio, c.out, (uint32_t)c.count);
    free(c.out);
    return clip;
}

void table_sounds_init(TableSounds *s, Audio *audio)
{
    s->deal = bake(audio, r_deal, 0.3f);
    s->card = bake(audio, r_card, 0.08f);
    s->sweep = bake(audio, r_sweep, 0.32f);
    s->turn = bake(audio, r_turn, 0.27f);
    s->emote = bake(audio, r_emote, 0.17f);
    s->bid = bake(audio, r_bid, 0.05f);
    s->trump = bake(audio, r_trump, 0.38f);
    s->win = bake(audio, r_win, 0.68f);
    s->lose = bake(audio, r_lose, 0.7f);
    s->click = bake(audio, r_click, 0.06f);
    s->fanfare_win = bake(audio, r_fanfare_win, 1.25f);
    s->fanfare_lose = bake(audio, r_fanfare_lose, 1.15f);
}

bool table_sound_play(const TableSounds *s, Audio *audio, const char *name)
{
    const struct {
        const char *name;
        int clip;
    } table[] = {{"deal", s->deal}, {"card", s->card}, {"sweep", s->sweep}, {"turn", s->turn},
                 {"emote", s->emote}, {"bid", s->bid}, {"trump", s->trump}, {"win", s->win},
                 {"lose", s->lose}, {"click", s->click}, {"fanfare_win", s->fanfare_win},
                 {"fanfare_lose", s->fanfare_lose}};
    for (size_t i = 0; i < sizeof table / sizeof *table; i++) {
        if (!strcmp(table[i].name, name)) {
            audio_play(audio, table[i].clip, 1, 0);
            return true;
        }
    }
    return false;
}
