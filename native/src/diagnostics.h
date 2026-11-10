#pragma once

#include <stddef.h>

/* The table's latest diagnostics, as one JSON object: the GPU and its path, the swapchain, window and
 * view sizes, the frame rate, and the last taps with what each hit. Any thread may call it; it copies
 * what the table's thread last wrote ("{}" before the first). */
void synth_diagnostics(char *out, size_t size);
