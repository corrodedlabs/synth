#pragma once

/* Every touch on the table, tap or not, for diagnostics. The platform turns a touch into a tap only when
 * it stays within a few pixels and lifts quickly, and anything else never reaches table_tap. So the tap
 * record alone can't show a phone whose taps fail before they count; this can. */

#include <stdbool.h>
#include <stddef.h>

#define TOUCH_LOG_SIZE 16
#define TOUCH_POINTERS 4  /* PLATFORM_MAX_POINTERS */

typedef enum { TOUCH_DOWN, TOUCH_MOVE, TOUCH_UP, TOUCH_TAP, TOUCH_CANCEL } TouchAction;

typedef struct {
    double down_at, up_at;  /* seconds, on the caller's clock */
    float x0, y0, x1, y1;   /* where it went down and where it lifted, in view pixels */
    float travel;           /* the furthest it got from where it went down */
    bool finger;            /* rather than the mouse */
    bool tap;               /* the platform counted it as one */
    bool cancelled;         /* the app left the foreground while it was down */
} Touch;

typedef struct {
    Touch log[TOUCH_LOG_SIZE];
    unsigned total;         /* finished so far; the newest is log[(total - 1) % TOUCH_LOG_SIZE] */
    struct {
        bool down;
        Touch touch;
        unsigned finished;  /* its index in total, once it has lifted, for the tap that follows */
    } open[TOUCH_POINTERS];
} TouchLog;

/* Feed it every pointer event, in order. A tap comes right after its touch lifts, as the platform sends it. */
void touch_log_event(TouchLog *log, TouchAction action, int id, float x, float y, bool finger, double now);

/* The log as a JSON array, oldest first. A touch that wasn't a tap says why, by the platform's rule:
 * "moved" past slop_px, "held" for tap_seconds or longer, or "cancelled". Returns the length written,
 * which is less than size; an array that doesn't fit is cut short to "[]". */
size_t touch_log_json(const TouchLog *log, float slop_px, float tap_seconds, char *out, size_t size);
