#include "table.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "core/log.h"

static const char *const SUITS[4] = {"hearts", "diamonds", "clubs", "spades"};
static const char *const RANKS[8] = {"J", "9", "A", "10", "K", "Q", "8", "7"};

#define CARD_W 0.6f
#define CARD_H 0.9f
#define PLANE_Y 0.2f       /* the trick's height */
#define HAND_Y 0.3f

static float clampf(float v, float lo, float hi)
{
    return v < lo ? lo : v > hi ? hi : v;
}

int table_card_index(const char *name)
{
    for (int s = 0; s < 4; s++) {
        size_t len = strlen(SUITS[s]);
        if (strncmp(name, SUITS[s], len) == 0 && name[len] == '-') {
            for (int r = 0; r < 8; r++)
                if (strcmp(name + len + 1, RANKS[r]) == 0)
                    return s * 8 + r;
        }
    }
    return -1;
}

void table_card_name(int index, char out[16])
{
    snprintf(out, 16, "%s-%s", SUITS[index / 8], RANKS[index % 8]);
}

/* bake-atlas.mjs writes the same grid: six across, with a gutter so mips don't pick up a neighbour. */
void table_atlas_uv(int cell, float uv[4])
{
    const float cols = 6, rows = 6, pad_u = 4.0f / 208, pad_v = 4.0f / 308;
    int col = cell % 6, row = cell / 6;
    uv[0] = (col + pad_u) / cols;
    uv[1] = (row + pad_v) / rows;
    uv[2] = (col + 1 - pad_u) / cols;
    uv[3] = (row + 1 - pad_v) / rows;
}

TableLayout table_layout(float width, float height)
{
    bool short_landscape = height <= 550 && width > height;
    bool compact = width <= 700;
    TableLayout l;
    if (short_landscape) {
        float marker = compact ? 60 : 64, side_x = marker / 2 + 18, top_y = clampf(height * 0.2f, 72, 88);
        float side_y = height * 0.52f, inset = side_x + marker / 2 + 32;
        l = (TableLayout){
            .seats = {{0, 0}, {width - side_x, side_y}, {width / 2, top_y}, {side_x, side_y}},
            .played = {{width / 2, height * 0.633f}, {width - inset, side_y}, {width / 2, top_y + 105}, {inset, side_y}},
            .hand_center = {width / 2, height * 0.825f},
            .hand_width_scale = clampf(width / 1100, 0.58f, 0.78f),
            .hand_card_scale = 0.68f,
            .table_card_scale = 0.62f,
        };
    } else if (compact) {
        float marker = 66, side_x = marker / 2 + 19, top_y = clampf(height * 0.16f, 118, 150);
        float top_card_y = top_y + 112;
        float side_y = clampf(height * 0.43f, top_y + 155, height * 0.48f);
        float inset = side_x + marker / 2 + 40;
        float bottom_y = side_y + clampf(height * 0.11f, 75, 102);
        l = (TableLayout){
            .seats = {{0, 0}, {width - side_x, side_y}, {width / 2, top_y}, {side_x, side_y}},
            .played = {{width / 2, bottom_y}, {width - inset, side_y}, {width / 2, top_card_y}, {inset, side_y}},
            .hand_center = {width / 2, height * 0.78f},
            .hand_width_scale = clampf(width / 500, 0.7f, 0.84f),
            .hand_card_scale = width <= 375 ? 1.08f : 1.12f,
            .table_card_scale = 1.08f,
        };
    } else {
        float marker = 88, side_x = clampf(width * 0.12f, 100, 220), top_y = clampf(height * 0.14f, 112, 138);
        float top_card_y = top_y + 168;
        bool is_short = height <= 650;
        float tall_side = height * 0.46f > top_card_y + 70 ? height * 0.46f : top_card_y + 70;
        float side_y = is_short ? top_card_y + 58 : tall_side;
        float inset = side_x + marker / 2 + 63;
        float hand_y = is_short ? height - 105 : height - clampf(height * 0.18f, 110, 165);
        float bottom_y = is_short ? side_y + 36 : side_y + clampf(height * 0.11f, 68, 105);
        l = (TableLayout){
            .seats = {{0, 0}, {width - side_x, side_y}, {width / 2, top_y}, {side_x, side_y}},
            .played = {{width / 2, bottom_y}, {width - inset, side_y}, {width / 2, top_card_y}, {inset, side_y}},
            .hand_center = {width / 2, hand_y},
            .hand_width_scale = clampf(width / 1024, 0.8f, 1),
            .hand_card_scale = is_short ? 0.86f : 1,
            .table_card_scale = is_short ? 0.76f : 0.95f,
        };
    }
    return l;
}

/* The camera matches SceneManager.ts. Clip y runs down in Vulkan, and there's no depth buffer, so z only
 * has to stay inside the clip volume. */
static void update_camera(Table *t)
{
    float aspect = t->css_w / t->css_h;
    if (aspect < 1) {
        float portrait = clampf(1 / aspect, 1, 2.2f);
        float deg = 38 * portrait < 70 ? 38 * portrait : 70;
        t->fov = deg * NM_PI / 180;
        t->camera = v3(0, 5.9f + 0.8f * (portrait - 1), 6.4f + 1.6f * (portrait - 1));
    } else {
        t->fov = 38 * NM_PI / 180;
        t->camera = v3(0, 5.9f, 6.4f);
    }
    vec3 f = v3_norm(v3_sub(v3(0, 0.15f, 0.1f), t->camera));
    vec3 r = v3_norm(v3_cross(f, v3(0, 1, 0)));
    vec3 u = v3_cross(r, f);
    float tn = 1 / tanf(t->fov / 2);
    vec3 e = t->camera;
    /* Rows: x, y, z, w of clip space. */
    float row_x[4] = {r.x * tn / aspect, r.y * tn / aspect, r.z * tn / aspect, -v3_dot(r, e) * tn / aspect};
    float row_y[4] = {-u.x * tn, -u.y * tn, -u.z * tn, v3_dot(u, e) * tn};
    float row_w[4] = {f.x, f.y, f.z, -v3_dot(f, e)};
    for (int c = 0; c < 4; c++) {
        t->view_proj.m[c * 4] = row_x[c];
        t->view_proj.m[c * 4 + 1] = row_y[c];
        t->view_proj.m[c * 4 + 2] = row_w[c] * 0.5f;
        t->view_proj.m[c * 4 + 3] = row_w[c];
    }
}

/* TableLayout.ts's screenPointOnPlane: the world point on a height that lands on this CSS pixel. */
static vec3 screen_on_plane(const Table *t, TablePoint p, float plane_y)
{
    vec3 f = v3_norm(v3_sub(v3(0, 0.15f, 0.1f), t->camera));
    vec3 r = v3_norm(v3_cross(f, v3(0, 1, 0)));
    vec3 u = v3_cross(r, f);
    float aspect = t->css_w / t->css_h, half = tanf(t->fov / 2);
    float nx = p.x / t->css_w * 2 - 1, ny = 1 - p.y / t->css_h * 2;
    vec3 dir = v3_norm(v3_add(f, v3_add(v3_scale(r, nx * half * aspect), v3_scale(u, ny * half))));
    float dist = (plane_y - t->camera.y) / dir.y;
    return v3_add(t->camera, v3_scale(dir, dist));
}

static float face_camera_rx(const Table *t, vec3 p)
{
    vec3 to = v3_sub(t->camera, p);
    return -atan2f(to.y, to.z);
}

/* Hand.ts's fan: a gentle arc whose spacing adapts to the hand's size. */
static void fan_slot(const Table *t, int index, int count, vec3 *pos, float *rx, float *rz)
{
    float width = t->layout.hand_width_scale;
    float step = 0.53f * width, limit = 2.4f * width;
    float spread = step * (count - 1) / 2 < limit ? step * (count - 1) / 2 : limit;
    float y0 = t->hand_center.y, y1 = y0 + 0.4f;
    float a = count > 1 ? (float)index / (count - 1) : 0.5f, b = 1 - a;
    pos->x = t->hand_center.x + (b * b * -spread + a * a * spread);
    pos->y = b * b * y0 + 2 * b * a * y1 + a * a * y0;
    pos->z = t->hand_center.z + index * 0.012f;
    *rx = face_camera_rx(t, *pos);
    *rz = -(a - 0.5f) * 2 * 0.16f;
}

static void move_card(TableCard *c, vec3 p, float rx, float rz, float scale, double now, float delay, float seconds)
{
    anim_to(&c->x, p.x, now, delay, seconds, ANIM_QUAD_OUT);
    anim_to(&c->y, p.y, now, delay, seconds, ANIM_QUAD_OUT);
    anim_to(&c->z, p.z, now, delay, seconds, ANIM_QUAD_OUT);
    anim_to(&c->rx, rx, now, delay, seconds, ANIM_QUAD_OUT);
    anim_to(&c->rz, rz, now, delay, seconds, ANIM_QUAD_OUT);
    anim_to(&c->scale, scale, now, delay, seconds, ANIM_QUAD_OUT);
}

static void arrange_hand(Table *t, float seconds)
{
    for (int i = 0; i < t->hand_count; i++) {
        vec3 p;
        float rx, rz;
        fan_slot(t, i, t->hand_count, &p, &rx, &rz);
        TableCard *c = &t->cards[t->hand[i]];
        if (c->armed)
            p.y += 0.22f;
        if (c->state == CARD_DEALING) {
            /* It holds below the fan until its moment, then slides in. */
            float delay = (float)(c->show_at - t->now);
            move_card(c, p, rx, rz, t->layout.hand_card_scale, t->now, delay > 0 ? delay : 0, 0.4f);
        } else {
            move_card(c, p, rx, rz, t->layout.hand_card_scale, t->now, 0, seconds);
        }
    }
}

static void place_trick_card(Table *t, int card, int stack, bool immediate)
{
    static const float tilts[4] = {0.05f, -0.07f, 0.04f, 0.08f};
    TableCard *c = &t->cards[card];
    vec3 p = t->played[c->seat];
    float rx = face_camera_rx(t, p);
    p.y += (stack + 1) * 0.005f;
    move_card(c, p, rx, tilts[c->seat], t->layout.table_card_scale, t->now, 0, immediate ? 0 : t->travel_ms / 1000.0f);
}

static void relayout(Table *t)
{
    t->layout = table_layout(t->css_w, t->css_h);
    update_camera(t);
    vec3 hc = screen_on_plane(t, t->layout.hand_center, HAND_Y);
    t->hand_center = hc;
    for (int i = 0; i < 4; i++)
        t->played[i] = screen_on_plane(t, t->layout.played[i], PLANE_Y);
    arrange_hand(t, 0);
    for (int i = 0; i < t->trick_count; i++)
        place_trick_card(t, t->trick[i], i, true);
}

void table_init(Table *t)
{
    memset(t, 0, sizeof *t);
    t->travel_ms = 560;
    t->reveal_ms = 620;
    t->collect_ms = 650;
    t->stagger_ms = 82;
    t->active = -1;
    t->pending_winner = -1;
    table_resize(t, 1080, 2340, 3);
}

void table_resize(Table *t, float view_w, float view_h, float density)
{
    t->view_w = view_w;
    t->view_h = view_h;
    t->css_w = view_w / density;
    t->css_h = view_h / density;
    relayout(t);
}

static void remove_from_hand(Table *t, int card)
{
    for (int i = 0; i < t->hand_count; i++) {
        if (t->hand[i] == card) {
            memmove(&t->hand[i], &t->hand[i + 1], (size_t)(t->hand_count - i - 1) * sizeof *t->hand);
            t->hand_count--;
            return;
        }
    }
}

static void reset_table(Table *t)
{
    for (int i = 0; i < TABLE_DECK; i++) {
        t->cards[i].state = CARD_NONE;
        t->cards[i].armed = false;
        t->legal[i] = false;
    }
    t->hand_count = t->trick_count = 0;
    t->active = -1;
    t->pending_winner = -1;
}

static void add_to_hand(Table *t, int card, double show_at)
{
    if (t->hand_count == TABLE_MAX_HAND || t->cards[card].state == CARD_HAND || t->cards[card].state == CARD_DEALING)
        return;
    TableCard *c = &t->cards[card];
    *c = (TableCard){.state = show_at > t->now ? CARD_DEALING : CARD_HAND, .show_at = show_at};
    /* It starts just below the fan, already facing the camera. */
    vec3 spawn = v3_add(t->hand_center, v3(0, -0.5f, 0.5f));
    anim_set(&c->x, spawn.x);
    anim_set(&c->y, spawn.y);
    anim_set(&c->z, spawn.z);
    anim_set(&c->rx, face_camera_rx(t, spawn));
    anim_set(&c->rz, 0);
    anim_set(&c->scale, t->layout.hand_card_scale);
    anim_set(&c->glow, 0);
    t->hand[t->hand_count++] = card;
}

/* Cards still on the table when a new one lands or a snapshot arrives. */
static void finish_collect(Table *t)
{
    if (t->pending_winner >= 0)
        t->pending_winner = -1;
}

static void cmd_play(Table *t, int seat, int card, bool immediate)
{
    if (t->trick_count == TABLE_MAX_TRICK)
        return;  /* the last trick's cards were never cleared: a snapshot will put this right */
    finish_collect(t);
    TableCard *c = &t->cards[card];
    bool from_hand = c->state == CARD_HAND || c->state == CARD_DEALING;
    if (from_hand) {
        remove_from_hand(t, card);
        c->armed = false;
    } else {
        /* Someone else's card appears at their seat and flies in. */
        *c = (TableCard){0};
        vec3 start = seat == 0 ? t->hand_center : screen_on_plane(t, t->layout.seats[seat], PLANE_Y);
        anim_set(&c->x, start.x);
        anim_set(&c->y, start.y);
        anim_set(&c->z, start.z);
        anim_set(&c->rx, face_camera_rx(t, start));
        anim_set(&c->rz, 0);
        anim_set(&c->scale, t->layout.table_card_scale * 0.8f);
        anim_set(&c->glow, 0);
    }
    c->state = CARD_TRICK;
    c->seat = seat;
    t->trick[t->trick_count++] = card;
    place_trick_card(t, card, t->trick_count - 1, immediate);
    if (from_hand)
        arrange_hand(t, 0.4f);
}

static void start_collect(Table *t, int winner)
{
    static const vec3 targets[4] = {{0, 0.4f, 4}, {4.5f, 0.4f, -0.5f}, {0, 0.4f, -4}, {-4.5f, 0.4f, -0.5f}};
    for (int i = 0; i < t->trick_count; i++) {
        TableCard *c = &t->cards[t->trick[i]];
        c->state = CARD_COLLECT;
        c->gone_at = t->now + t->collect_ms / 1000.0;
        anim_to(&c->glow, 0, t->now, 0, 0.12f, ANIM_QUAD_OUT);
        float seconds = t->collect_ms / 1000.0f;
        anim_to(&c->x, targets[winner].x, t->now, 0, seconds, ANIM_QUAD_OUT);
        anim_to(&c->y, targets[winner].y, t->now, 0, seconds, ANIM_QUAD_OUT);
        anim_to(&c->z, targets[winner].z, t->now, 0, seconds, ANIM_QUAD_OUT);
    }
    t->trick_count = 0;
}

static void log_line(Table *t, const char *line)
{
    if (t->on_log)
        t->on_log(t->user, line);
    else
        nm_log("%s", line);
}

static void dump(Table *t)
{
    char line[128], name[16];
    for (int i = 0; i < t->hand_count; i++) {
        TablePose pose;
        if (!table_pose(t, t->hand[i], &pose))
            continue;
        TablePoint q[4];
        table_card_corners(t, &pose, q);
        table_card_name(t->hand[i], name);
        snprintf(line, sizeof line, "table: card %s %d %.0f %.0f legal=%d armed=%d", name, i,
                 (q[0].x + q[1].x + q[2].x + q[3].x) / 4, (q[0].y + q[1].y + q[2].y + q[3].y) / 4,
                 t->legal[t->hand[i]], t->cards[t->hand[i]].armed);
        log_line(t, line);
    }
    snprintf(line, sizeof line, "table: hand %d trick %d active %d", t->hand_count, t->trick_count, t->active);
    log_line(t, line);
}

void table_command(Table *t, const char *text)
{
    char buffer[512];
    snprintf(buffer, sizeof buffer, "%s", text);
    char *save = NULL;
    char *word = strtok_r(buffer, " ", &save);
    if (!word)
        return;

    if (!strcmp(word, "reset")) {
        reset_table(t);
    } else if (!strcmp(word, "seats")) {
        const char *rest = text + 6;
        for (int i = 0; i < 4; i++) {
            const char *bar = strchr(rest, '|');
            size_t len = bar ? (size_t)(bar - rest) : strlen(rest);
            if (len >= TABLE_NAME)
                len = TABLE_NAME - 1;
            memcpy(t->names[i], rest, len);
            t->names[i][len] = 0;
            if (!bar) {
                for (int j = i + 1; j < 4; j++)
                    t->names[j][0] = 0;
                break;
            }
            rest = bar + 1;
        }
    } else if (!strcmp(word, "deal") || !strcmp(word, "hand")) {
        bool staged = word[0] == 'd';
        if (!staged) {
            int keep[TABLE_MAX_TRICK];
            memcpy(keep, t->trick, sizeof keep);
            int trick_count = t->trick_count;
            for (int i = 0; i < t->hand_count; i++)
                t->cards[t->hand[i]].state = CARD_NONE;
            t->hand_count = 0;
            t->trick_count = trick_count;
        }
        int n = 0;
        while ((word = strtok_r(NULL, " ", &save))) {
            int card = table_card_index(word);
            if (card < 0) {
                nm_log("table: not a card: %s", word);
                continue;
            }
            add_to_hand(t, card, staged ? t->now + n * t->stagger_ms / 1000.0 : t->now);
            n++;
        }
        arrange_hand(t, staged ? 0.4f : 0);
    } else if (!strcmp(word, "order")) {
        int ordered[TABLE_MAX_HAND], n = 0;
        while ((word = strtok_r(NULL, " ", &save))) {
            int card = table_card_index(word);
            for (int i = 0; card >= 0 && i < t->hand_count; i++) {
                if (t->hand[i] == card && n < TABLE_MAX_HAND) {
                    ordered[n++] = card;
                    break;
                }
            }
        }
        /* Anything the list missed keeps its place at the end, so a late command can't lose a card. */
        for (int i = 0; i < t->hand_count; i++) {
            bool seen = false;
            for (int j = 0; j < n; j++)
                seen = seen || ordered[j] == t->hand[i];
            if (!seen && n < TABLE_MAX_HAND)
                ordered[n++] = t->hand[i];
        }
        memcpy(t->hand, ordered, (size_t)n * sizeof *ordered);
        arrange_hand(t, 0.25f);
    } else if (!strcmp(word, "legal")) {
        memset(t->legal, 0, sizeof t->legal);
        while ((word = strtok_r(NULL, " ", &save))) {
            int card = table_card_index(word);
            if (card >= 0)
                t->legal[card] = true;
        }
        bool moved = false;
        for (int i = 0; i < t->hand_count; i++) {
            TableCard *c = &t->cards[t->hand[i]];
            if (c->armed && !t->legal[t->hand[i]]) {
                c->armed = false;
                moved = true;
            }
        }
        if (moved)
            arrange_hand(t, 0.15f);
    } else if (!strcmp(word, "play") || !strcmp(word, "place")) {
        bool immediate = word[1] == 'l' && word[2] == 'a' && word[3] == 'c';
        char *seat_word = strtok_r(NULL, " ", &save);
        char *card_word = strtok_r(NULL, " ", &save);
        int seat = seat_word ? atoi(seat_word) : -1;
        int card = card_word ? table_card_index(card_word) : -1;
        if (seat < 0 || seat > 3 || card < 0)
            nm_log("table: bad command: %s", text);
        else
            cmd_play(t, seat, card, immediate);
    } else if (!strcmp(word, "active")) {
        char *seat = strtok_r(NULL, " ", &save);
        t->active = seat ? atoi(seat) : -1;
    } else if (!strcmp(word, "trick")) {
        char *seat_word = strtok_r(NULL, " ", &save);
        int winner = seat_word ? atoi(seat_word) : -1;
        if (winner < 0 || winner > 3) {
            for (int i = 0; i < t->trick_count; i++)
                t->cards[t->trick[i]].state = CARD_NONE;
            t->trick_count = 0;
            t->pending_winner = -1;
        } else {
            for (int i = 0; i < t->trick_count; i++) {
                TableCard *c = &t->cards[t->trick[i]];
                if (c->seat == winner) {
                    anim_to(&c->glow, 1, t->now, 0, 0.18f, ANIM_QUAD_OUT);
                    anim_to(&c->scale, t->layout.table_card_scale * 1.08f, t->now, 0, 0.18f, ANIM_BACK_OUT);
                }
            }
            t->pending_winner = winner;
            t->reveal_at = t->now + t->reveal_ms / 1000.0;
        }
    } else if (!strcmp(word, "clear")) {
        for (int i = 0; i < TABLE_DECK; i++)
            if (t->cards[i].state == CARD_TRICK || t->cards[i].state == CARD_COLLECT)
                t->cards[i].state = CARD_NONE;
        t->trick_count = 0;
        t->pending_winner = -1;
    } else if (!strcmp(word, "pacing")) {
        char *a = strtok_r(NULL, " ", &save), *b = strtok_r(NULL, " ", &save), *c = strtok_r(NULL, " ", &save),
             *d = strtok_r(NULL, " ", &save);
        if (a && b && c && d) {
            t->travel_ms = atoi(a);
            t->reveal_ms = atoi(b);
            t->collect_ms = atoi(c);
            t->stagger_ms = atoi(d);
        }
    } else if (!strcmp(word, "dump")) {
        dump(t);
    } else if (strcmp(word, "sound") && strcmp(word, "mute")) {
        nm_log("table: unknown command: %s", text);
    }
}

void table_update(Table *t, double now)
{
    t->now = now;
    for (int i = 0; i < t->hand_count; i++) {
        TableCard *c = &t->cards[t->hand[i]];
        if (c->state == CARD_DEALING && now >= c->show_at)
            c->state = CARD_HAND;
    }
    if (t->pending_winner >= 0 && now >= t->reveal_at) {
        int winner = t->pending_winner;
        t->pending_winner = -1;
        start_collect(t, winner);
    }
    for (int i = 0; i < TABLE_DECK; i++)
        if (t->cards[i].state == CARD_COLLECT && now >= t->cards[i].gone_at)
            t->cards[i].state = CARD_NONE;
}

bool table_pose(const Table *t, int card, TablePose *pose)
{
    const TableCard *c = &t->cards[card];
    if (c->state == CARD_NONE || (c->state == CARD_DEALING && t->now < c->show_at))
        return false;
    pose->position = v3(anim_value(&c->x, t->now), anim_value(&c->y, t->now), anim_value(&c->z, t->now));
    pose->rx = anim_value(&c->rx, t->now);
    pose->rz = anim_value(&c->rz, t->now);
    pose->scale = anim_value(&c->scale, t->now);
    pose->glow = anim_value(&c->glow, t->now);
    return true;
}

/* A unit quad to world space: scale, then Three.js's XYZ Euler order (rz first, then rx). */
static void card_world(const TablePose *pose, float width, float height, float out[16])
{
    float cx = cosf(pose->rx), sx = sinf(pose->rx), cz = cosf(pose->rz), sz = sinf(pose->rz);
    float w = width * pose->scale, h = height * pose->scale;
    /* Columns: the quad's x axis, y axis, normal, position. */
    float m[16] = {
        cz * w, cx * sz * w, sx * sz * w, 0,
        -sz * h, cx * cz * h, sx * cz * h, 0,
        0, -sx, cx, 0,
        pose->position.x, pose->position.y, pose->position.z, 1,
    };
    memcpy(out, m, sizeof m);
}

mat4 table_card_matrix(const Table *t, const TablePose *pose, float width, float height)
{
    mat4 world;
    card_world(pose, width, height, world.m);
    return m4_mul(t->view_proj, world);
}

TablePoint table_project(const Table *t, vec3 p)
{
    const float *m = t->view_proj.m;
    float x = m[0] * p.x + m[4] * p.y + m[8] * p.z + m[12];
    float y = m[1] * p.x + m[5] * p.y + m[9] * p.z + m[13];
    float w = m[3] * p.x + m[7] * p.y + m[11] * p.z + m[15];
    return (TablePoint){(x / w * 0.5f + 0.5f) * t->view_w, (y / w * 0.5f + 0.5f) * t->view_h};
}

void table_card_corners(const Table *t, const TablePose *pose, TablePoint out[4])
{
    float m[16];
    card_world(pose, CARD_W, CARD_H, m);
    static const float corners[4][2] = {{-0.5f, -0.5f}, {0.5f, -0.5f}, {0.5f, 0.5f}, {-0.5f, 0.5f}};
    for (int i = 0; i < 4; i++) {
        float a = corners[i][0], b = corners[i][1];
        out[i] = table_project(t, v3(m[0] * a + m[4] * b + m[12], m[1] * a + m[5] * b + m[13], m[2] * a + m[6] * b + m[14]));
    }
}

static bool inside(const TablePoint q[4], float x, float y)
{
    int sign = 0;
    for (int i = 0; i < 4; i++) {
        TablePoint a = q[i], b = q[(i + 1) % 4];
        float cross = (b.x - a.x) * (y - a.y) - (b.y - a.y) * (x - a.x);
        int s = cross > 0 ? 1 : cross < 0 ? -1 : 0;
        if (s && sign && s != sign)
            return false;
        if (s)
            sign = s;
    }
    return true;
}

void table_tap(Table *t, float x, float y, bool touch)
{
    /* Later cards in the fan lie on top, so they take the tap first. */
    for (int i = t->hand_count - 1; i >= 0; i--) {
        int card = t->hand[i];
        TablePose pose;
        if (!table_pose(t, card, &pose))
            continue;
        TablePoint q[4];
        table_card_corners(t, &pose, q);
        if (!inside(q, x, y))
            continue;
        TableCard *c = &t->cards[card];
        if (!t->legal[card]) {
            /* An unplayable card under the finger shouldn't leave another raised. */
            return;
        }
        if (touch && !c->armed) {
            for (int j = 0; j < t->hand_count; j++)
                t->cards[t->hand[j]].armed = false;
            c->armed = true;
            arrange_hand(t, 0.15f);
            return;
        }
        char name[16];
        table_card_name(card, name);
        c->armed = false;
        if (t->on_play)
            t->on_play(t->user, name);
        return;
    }
    /* A tap on the felt puts a raised card back. */
    bool moved = false;
    for (int j = 0; j < t->hand_count; j++) {
        moved = moved || t->cards[t->hand[j]].armed;
        t->cards[t->hand[j]].armed = false;
    }
    if (moved)
        arrange_hand(t, 0.15f);
}
