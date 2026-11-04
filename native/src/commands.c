#include "commands.h"

#include <SDL3/SDL.h>
#include <string.h>

#define MAX_COMMANDS 256

/* SDL's mutexes need SDL running, and Kotlin may speak before the game's main starts, so this uses
 * a spinlock that's fine from the first moment. */
static SDL_SpinLock lock;
static char *queue[MAX_COMMANDS];
static int head, count;

void commands_push(const char *line)
{
    char *copy = SDL_strdup(line);
    SDL_LockSpinlock(&lock);
    if (count < MAX_COMMANDS) {
        queue[(head + count++) % MAX_COMMANDS] = copy;
        copy = NULL;
    }
    SDL_UnlockSpinlock(&lock);
    SDL_free(copy);
}

char *commands_pop(void)
{
    char *line = NULL;
    SDL_LockSpinlock(&lock);
    if (count > 0) {
        line = queue[head];
        head = (head + 1) % MAX_COMMANDS;
        count--;
    }
    SDL_UnlockSpinlock(&lock);
    return line;
}
