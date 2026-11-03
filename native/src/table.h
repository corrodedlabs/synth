#pragma once

/* The 28 table: where each card is and how it moves. Kotlin (or the desktop demo) drives it with text
 * commands, the same presentation steps the web client's GameState performs; the server's rules never
 * come near this. This file needs no GPU or window, so the tests run it directly.
 *
 * Commands, one per line:
 *   reset                                  clear the table
 *   seats <us>|<right>|<top>|<left>        names by view seat, '|'-separated
 *   deal <card> <card> ...                 cards arrive in the fan, staggered
 *   hand <card> ...                        the fan is exactly these, in place (a reconnect's snapshot)
 *   order <card> ...                       rearrange the fan
 *   legal <card> ...                       only these may be played; with none, nothing may
 *   play <seat> <card>                     a card goes to the trick, from the fan or from the seat
 *   place <seat> <card>                    the same, without travelling (a snapshot)
 *   active <seat>                          whose turn; -1 for nobody
 *   trick <seat>                           show the winner, then sweep to them; -1 just clears
 *   clear                                  the trick disappears at once
 *   pacing <travel> <reveal> <collect> <stagger>   milliseconds
 *   sound <name>, mute <0|1>               handled by sound.c, not here
 *   dump                                   logs where every fan card is on screen, for tests
 * A card is <suit>-<rank>, such as hearts-10. Anything malformed is logged and ignored. */

#include <stdbool.h>

#include "anim/anim.h"
#include "core/nm_math.h"

#define TABLE_DECK 32
#define TABLE_BACK 32      /* the atlas cell after the deck */
#define TABLE_MAX_HAND 8
#define TABLE_MAX_TRICK 4
#define TABLE_NAME 32

typedef enum { CARD_NONE, CARD_DEALING, CARD_HAND, CARD_TRICK, CARD_COLLECT } TableCardState;

typedef struct {
    TableCardState state;
    int seat;              /* who played it */
    bool armed;            /* first tap of a two-step touch play: raised and waiting for the second */
    AnimTween x, y, z, rx, rz, scale, glow;
    double show_at;        /* a dealt card appears then */
    double gone_at;        /* a collected card disappears then */
} TableCard;

typedef struct {
    float x, y;
} TablePoint;

/* The web client's responsive layout, in CSS pixels (TableLayout.ts). */
typedef struct {
    TablePoint seats[4];   /* seats[0] is unused: our identity is the hand */
    TablePoint played[4];
    TablePoint hand_center;
    float hand_width_scale, hand_card_scale, table_card_scale;
} TableLayout;

typedef void TableEventFn(void *user, const char *line);

typedef struct {
    TableCard cards[TABLE_DECK];
    int hand[TABLE_MAX_HAND];
    int hand_count;
    int trick[TABLE_MAX_TRICK];
    int trick_count;
    bool legal[TABLE_DECK];
    char names[4][TABLE_NAME];
    int active;

    int travel_ms, reveal_ms, collect_ms, stagger_ms;
    double now;
    int pending_winner;    /* of the trick being shown; collected when reveal_at passes */
    double reveal_at;
    int deal_count;        /* of the current packet, for the stagger */

    float css_w, css_h;    /* the view in CSS pixels */
    float view_w, view_h;  /* in real ones */
    TableLayout layout;
    vec3 camera;
    float fov;             /* radians */
    mat4 view_proj;
    vec3 hand_center;
    vec3 played[4];

    TableEventFn *on_play; /* "<suit>-<rank>" when a play is committed */
    TableEventFn *on_log;  /* test output */
    void *user;
} Table;

void table_init(Table *t);
TableLayout table_layout(float width, float height);
/* Sets the view, in pixels; density is pixels per CSS pixel. Cards go to their new places at once. */
void table_resize(Table *t, float view_w, float view_h, float density);
void table_command(Table *t, const char *line);
void table_update(Table *t, double now);
/* touch: a finger asks for two steps, raising the card first, so a stray tap can't play it. */
void table_tap(Table *t, float x, float y, bool touch);

int table_card_index(const char *name);   /* -1 if it isn't a card */
void table_card_name(int index, char out[16]);
/* The atlas cell of a card, or TABLE_BACK. uv is u0, v0, u1, v1. */
void table_atlas_uv(int cell, float uv[4]);
/* The card as it stands now. false when it isn't on the table. */
typedef struct {
    vec3 position;
    float rx, rz, scale, glow;
} TablePose;
bool table_pose(const Table *t, int card, TablePose *pose);
mat4 table_card_matrix(const Table *t, const TablePose *pose, float width, float height);
/* Where a card's corners are on screen, in view pixels, in order around it. */
void table_card_corners(const Table *t, const TablePose *pose, TablePoint out[4]);
TablePoint table_project(const Table *t, vec3 world);
