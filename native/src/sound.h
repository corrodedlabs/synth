#pragma once

#include "audio/audio.h"

/* The table's sounds, synthesised once at start from the same recipes as the web client's Sound.ts. */
typedef struct {
    int deal, card, sweep, turn, emote, bid, trump, win, lose, click, fanfare_win, fanfare_lose;
} TableSounds;

void table_sounds_init(TableSounds *sounds, Audio *audio);
/* name is one of the fields above, written as in "sound fanfare_win". Returns false if it isn't. */
bool table_sound_play(const TableSounds *sounds, Audio *audio, const char *name);
