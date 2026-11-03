#pragma once

/* A queue of table commands from another thread: Kotlin's, on Android. */
void commands_push(const char *line);
/* Returns a malloc'd line for the caller to free, or NULL when empty. */
char *commands_pop(void);
