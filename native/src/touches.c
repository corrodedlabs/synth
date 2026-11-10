#include "touches.h"

#include <math.h>
#include <stdio.h>

static void finish(TouchLog *log, int id, float x, float y, double now, bool cancelled)
{
    Touch *t = &log->open[id].touch;
    t->x1 = x;
    t->y1 = y;
    t->up_at = now;
    t->cancelled = cancelled;
    log->open[id].down = false;
    log->open[id].finished = log->total;
    log->log[log->total % TOUCH_LOG_SIZE] = *t;
    log->total++;
}

void touch_log_event(TouchLog *log, TouchAction action, int id, float x, float y, bool finger, double now)
{
    if (id < 0 || id >= TOUCH_POINTERS)
        return;
    Touch *open = &log->open[id].touch;
    switch (action) {
    case TOUCH_DOWN:
        log->open[id].down = true;
        *open = (Touch){.down_at = now, .x0 = x, .y0 = y, .finger = finger};
        break;
    case TOUCH_MOVE:
        if (log->open[id].down) {
            float d = hypotf(x - open->x0, y - open->y0);
            if (d > open->travel)
                open->travel = d;
        }
        break;
    case TOUCH_UP:
    case TOUCH_CANCEL:
        if (log->open[id].down)
            finish(log, id, x, y, now, action == TOUCH_CANCEL);
        break;
    case TOUCH_TAP: {
        /* The touch that just lifted, if the ring still holds it. */
        unsigned i = log->open[id].finished;
        if (log->total > i && log->total - i <= TOUCH_LOG_SIZE)
            log->log[i % TOUCH_LOG_SIZE].tap = true;
        break;
    }
    }
}

size_t touch_log_json(const TouchLog *log, float slop_px, float tap_seconds, char *out, size_t size)
{
    if (size < 3)
        return 0;
    unsigned count = log->total < TOUCH_LOG_SIZE ? log->total : TOUCH_LOG_SIZE;
    size_t n = 0;
    out[n++] = '[';
    for (unsigned k = 0; k < count; k++) {
        const Touch *t = &log->log[(log->total - count + k) % TOUCH_LOG_SIZE];
        double held = t->up_at - t->down_at;
        const char *why = t->tap ? "" : t->cancelled ? "cancelled" : t->travel > slop_px ? "moved"
                        : held >= tap_seconds ? "held" : "other";
        int w = snprintf(out + n, size - n,
                         "%s{\"t\":%.2f,\"down\":[%.0f,%.0f],\"up\":[%.0f,%.0f],\"travel\":%.1f,\"held\":%.3f,"
                         "\"finger\":%s,\"tap\":%s,\"why\":\"%s\"}",
                         k ? "," : "", t->down_at, t->x0, t->y0, t->x1, t->y1, t->travel, held,
                         t->finger ? "true" : "false", t->tap ? "true" : "false", why);
        if (w < 0 || (size_t)w >= size - n - 1) {
            snprintf(out, size, "[]");
            return 2;
        }
        n += (size_t)w;
    }
    out[n++] = ']';
    out[n] = 0;
    return n;
}
