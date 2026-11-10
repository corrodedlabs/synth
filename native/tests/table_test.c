/* The table's logic with no GPU or window: layout, moves, the two-step play. */
#include <math.h>
#include <stdio.h>
#include <string.h>

#include "table.h"

static int failures;
#define CHECK(cond)                                                   \
    do {                                                              \
        if (!(cond)) {                                                \
            printf("%s:%d: failed: %s\n", __FILE__, __LINE__, #cond); \
            failures++;                                               \
        }                                                             \
    } while (0)

static char played[32];
static void on_play(void *user, const char *card)
{
    snprintf(played, sizeof played, "%s", card);
}

static TablePoint centre_of(Table *t, const char *name)
{
    TablePose pose;
    TablePoint q[4];
    bool ok = table_pose(t, table_card_index(name), &pose);
    CHECK(ok);
    table_card_corners(t, &pose, q);
    return (TablePoint){(q[0].x + q[1].x + q[2].x + q[3].x) / 4, (q[0].y + q[1].y + q[2].y + q[3].y) / 4};
}

/* The same numbers TableLayout.test.ts holds the web client to. */
static void test_layout(void)
{
    TableLayout phone = table_layout(390, 844);
    CHECK(fabsf(phone.seats[1].x - (390 - 52)) < 0.01f);
    CHECK(fabsf(phone.seats[2].x - 195) < 0.01f);
    CHECK(fabsf(phone.seats[2].y - 135.04f) < 0.01f);
    CHECK(fabsf(phone.hand_center.y - 844 * 0.78f) < 0.01f);
    CHECK(fabsf(phone.hand_card_scale - 1.12f) < 0.001f);
    TableLayout small = table_layout(360, 700);
    CHECK(fabsf(small.hand_card_scale - 1.08f) < 0.001f);
    TableLayout wide = table_layout(1280, 800);
    CHECK(fabsf(wide.hand_card_scale - 1) < 0.001f);
    CHECK(wide.table_card_scale < 1);
    TableLayout landscape = table_layout(844, 390);
    CHECK(fabsf(landscape.hand_card_scale - 0.68f) < 0.001f);
    /* A compact window squaring up gives back what the closer camera would add to its cards. */
    TableLayout folding = table_layout(673, 841);
    CHECK(fabsf(folding.hand_card_scale - 1.12f * 0.65f * 841 / 673) < 0.001f);
    TableLayout tall = table_layout(700, 900);
    CHECK(fabsf(tall.played[1].x - (700 - 134)) < 0.001f);
    TableLayout low = table_layout(841, 673);
    CHECK(fabsf(low.hand_center.y - (673 - 673 * 0.165f)) < 0.001f);
    TableLayout square = table_layout(700, 700);
    CHECK(fabsf(square.table_card_scale - 1.08f * 0.7f) < 0.001f);
}

static void test_names(void)
{
    CHECK(table_card_index("hearts-J") == 0);
    CHECK(table_card_index("spades-7") == 31);
    CHECK(table_card_index("clubs-10") == 2 * 8 + 3);
    CHECK(table_card_index("clubs-11") == -1);
    CHECK(table_card_index("hearts") == -1);
    char name[16];
    table_card_name(table_card_index("diamonds-Q"), name);
    CHECK(!strcmp(name, "diamonds-Q"));
    float uv[4];
    table_atlas_uv(TABLE_BACK, uv);
    CHECK(uv[0] > 2.0f / 6 && uv[2] < 3.0f / 6);
    CHECK(uv[1] > 5.0f / 6 && uv[3] < 1);
}

/* A point on the table lands back on the screen where the layout asked. */
static void test_projection(void)
{
    Table t;
    table_init(&t);
    table_resize(&t, 1080, 2340, 3);
    for (int i = 0; i < 4; i++) {
        TablePoint p = table_project(&t, t.played[i]);
        CHECK(fabsf(p.x - t.layout.played[i].x * 3) < 1);
        CHECK(fabsf(p.y - t.layout.played[i].y * 3) < 1);
    }
    table_resize(&t, 2340, 1080, 3);
    for (int i = 0; i < 4; i++) {
        TablePoint p = table_project(&t, t.played[i]);
        CHECK(fabsf(p.x - t.layout.played[i].x * 3) < 1);
        CHECK(fabsf(p.y - t.layout.played[i].y * 3) < 1);
    }
}

static void test_deal_and_trick(void)
{
    Table t;
    table_init(&t);
    table_resize(&t, 1080, 2340, 3);
    table_update(&t, 10);
    table_command(&t, "deal hearts-J spades-9 clubs-A diamonds-10");
    CHECK(t.hand_count == 4);
    TablePose pose;
    CHECK(table_pose(&t, table_card_index("hearts-J"), &pose));
    CHECK(!table_pose(&t, table_card_index("diamonds-10"), &pose));  /* still waiting its turn */
    table_update(&t, 12);
    CHECK(table_pose(&t, table_card_index("diamonds-10"), &pose));

    /* Fan runs left to right, and stays on screen. */
    TablePoint a = centre_of(&t, "hearts-J"), b = centre_of(&t, "diamonds-10");
    CHECK(a.x < b.x);
    CHECK(a.x > 0 && b.x < 1080);
    CHECK(a.y > 1400 && a.y < 2340);

    /* Our card leaves the fan; another's arrives. */
    table_command(&t, "play 0 hearts-J");
    CHECK(t.hand_count == 3);
    CHECK(t.trick_count == 1);
    table_command(&t, "play 1 hearts-A");
    CHECK(t.trick_count == 2);
    table_update(&t, 13);
    CHECK(t.cards[table_card_index("hearts-A")].state == CARD_TRICK);

    TablePoint at = centre_of(&t, "hearts-A");
    CHECK(fabsf(at.x - t.layout.played[1].x * 3) < 40);
    CHECK(fabsf(at.y - t.layout.played[1].y * 3) < 40);

    /* The winner shows first; then they're swept off; then they're gone. */
    table_command(&t, "trick 1");
    CHECK(t.trick_count == 2);
    table_update(&t, 13 + 0.3);
    CHECK(table_pose(&t, table_card_index("hearts-A"), &pose) && pose.glow > 0.5f);
    table_update(&t, 13 + 0.7);
    CHECK(t.trick_count == 0);
    CHECK(t.cards[table_card_index("hearts-A")].state == CARD_COLLECT);
    table_update(&t, 15);
    CHECK(t.cards[table_card_index("hearts-A")].state == CARD_NONE);

    /* Bad commands change nothing. */
    int hand = t.hand_count;
    table_command(&t, "play 7 hearts-A");
    table_command(&t, "play 1 nonsense");
    table_command(&t, "whatever 1 2");
    table_command(&t, "");
    CHECK(t.hand_count == hand);
    CHECK(t.trick_count == 0);

    table_command(&t, "reset");
    CHECK(t.hand_count == 0);
}

static void test_tap(void)
{
    Table t;
    table_init(&t);
    table_resize(&t, 1080, 2340, 3);
    t.on_play = on_play;
    table_update(&t, 1);
    table_command(&t, "hand hearts-J spades-9 clubs-A");
    table_update(&t, 2);

    played[0] = 0;
    TablePoint c = centre_of(&t, "spades-9");
    table_tap(&t, c.x, c.y, true);
    CHECK(!played[0]);  /* nothing is legal yet */
    CHECK(!t.cards[table_card_index("spades-9")].armed);

    CHECK(t.last_tap.result == TABLE_TAP_UNPLAYABLE);
    CHECK(t.last_tap.card == table_card_index("spades-9"));
    CHECK(t.taps == 1);
    /* The record holds the hand's bounds, which hold the tap. */
    CHECK(t.last_tap.fan[0] < c.x && c.x < t.last_tap.fan[2] && t.last_tap.fan[1] < c.y && c.y < t.last_tap.fan[3]);

    table_command(&t, "legal spades-9 hearts-J");
    table_tap(&t, c.x, c.y, true);
    CHECK(t.cards[table_card_index("spades-9")].armed);  /* first tap raises */
    CHECK(t.last_tap.result == TABLE_TAP_RAISED);
    CHECK(!played[0]);
    table_update(&t, 3);
    c = centre_of(&t, "spades-9");
    table_tap(&t, c.x, c.y, true);
    CHECK(!strcmp(played, "spades-9"));                  /* second commits */
    CHECK(t.last_tap.result == TABLE_TAP_PLAYED);

    /* A tap on an illegal card leaves things alone. */
    played[0] = 0;
    table_update(&t, 4);
    c = centre_of(&t, "clubs-A");
    table_tap(&t, c.x, c.y, true);
    CHECK(!played[0]);

    /* Raising one lowers another; a tap on the felt lowers it too. */
    TablePoint j = centre_of(&t, "hearts-J");
    table_tap(&t, j.x, j.y, true);
    CHECK(t.cards[table_card_index("hearts-J")].armed);
    table_tap(&t, 540, 200, true);
    CHECK(!t.cards[table_card_index("hearts-J")].armed);
    CHECK(t.last_tap.result == TABLE_TAP_LOWERED);
    CHECK(t.last_tap.card == -1);
    table_tap(&t, 540, 200, true);
    CHECK(t.last_tap.result == TABLE_TAP_FELT);

    /* If the turn passes while raised, it comes back down. */
    table_update(&t, 5);
    j = centre_of(&t, "hearts-J");
    table_tap(&t, j.x, j.y, true);
    CHECK(t.cards[table_card_index("hearts-J")].armed);
    table_command(&t, "legal");
    CHECK(!t.cards[table_card_index("hearts-J")].armed);

    /* The mouse plays in one go. */
    table_command(&t, "legal hearts-J");
    table_update(&t, 6);
    j = centre_of(&t, "hearts-J");
    table_tap(&t, j.x, j.y, false);
    CHECK(!strcmp(played, "hearts-J"));
}

static void test_order_and_snapshot(void)
{
    Table t;
    table_init(&t);
    table_update(&t, 1);
    table_command(&t, "hand hearts-J spades-9 clubs-A");
    table_command(&t, "order clubs-A hearts-J");
    CHECK(t.hand_count == 3);
    CHECK(t.hand[0] == table_card_index("clubs-A"));
    CHECK(t.hand[2] == table_card_index("spades-9"));  /* the list missed it, so it isn't lost */

    /* A snapshot replaces the hand in place, and puts the trick straight there. */
    table_command(&t, "reset");
    table_command(&t, "hand diamonds-7");
    table_command(&t, "place 2 clubs-J");
    CHECK(t.hand_count == 1);
    CHECK(t.trick_count == 1);
    table_update(&t, 1.01);
    TablePoint at = centre_of(&t, "clubs-J");
    CHECK(fabsf(at.x - t.layout.played[2].x * 3) < 40);

    /* The table's fifth card can't overflow anything. */
    table_command(&t, "clear");
    const char *five[] = {"play 0 clubs-7", "play 1 clubs-8", "play 2 clubs-9", "play 3 clubs-10", "play 0 clubs-K"};
    for (int i = 0; i < 5; i++)
        table_command(&t, five[i]);
    CHECK(t.trick_count == 4);
    table_command(&t, "trick -1");
    CHECK(t.trick_count == 0);
}

/* A played card arcs and bounces, and busy lasts until it has settled. */
static void test_flight(void)
{
    static Table t;
    table_init(&t);
    table_resize(&t, 1080, 2340, 2.81f);
    table_command(&t, "pacing 400 700 600 100");
    table_command(&t, "play 1 hearts-J");
    table_update(&t, 10);
    table_command(&t, "play 2 hearts-9");
    int card = t.trick[1];
    double start = t.cards[card].flight_start, end = t.cards[card].flight_end;
    CHECK(end > start);
    float settled_y = t.cards[card].y.to, settled_scale = t.cards[card].scale.to;

    TablePose mid;
    table_update(&t, start + (end - start) * 0.5);
    CHECK(table_pose(&t, card, &mid));
    /* Halfway it's higher than a straight line could put it, with the tween still short of its target. */
    CHECK(mid.position.y > anim_value(&t.cards[card].y, t.now) + 0.2f);

    TablePose landing;
    table_update(&t, end + 0.08);
    CHECK(table_pose(&t, card, &landing));
    CHECK(landing.scale < settled_scale);
    CHECK(fabsf(landing.position.y - settled_y) < 0.001f);
    CHECK(table_busy(&t, t.now));

    TablePose rest;
    table_update(&t, end + 1);
    CHECK(table_pose(&t, card, &rest));
    CHECK(fabsf(rest.scale - settled_scale) < 0.001f);
    CHECK(!table_busy(&t, t.now));
}

/* Only the cards you can play stand proud of the fan. */
static void test_legal_lift(void)
{
    static Table t;
    table_init(&t);
    table_resize(&t, 1080, 2340, 2.81f);
    table_command(&t, "deal hearts-J spades-9");
    table_update(&t, 10);
    int legal = t.hand[0], other = t.hand[1];
    float before = t.cards[legal].y.to, other_before = t.cards[other].y.to;
    table_command(&t, "legal hearts-J");
    CHECK(t.cards[legal].y.to > before + 0.05f);
    CHECK(fabsf(t.cards[other].y.to - other_before) < 0.001f);
    table_command(&t, "legal");
    CHECK(fabsf(t.cards[legal].y.to - before) < 0.001f);
}

/* The clip matrix of a shadow, back to where its centre lands on screen. */
static TablePoint shadow_centre(const Table *t, const TableShadow *shadow)
{
    const float *m = shadow->clip.m;
    return (TablePoint){(m[12] / m[15] * 0.5f + 0.5f) * t->view_w, (m[13] / m[15] * 0.5f + 0.5f) * t->view_h};
}

/* The shadows hug the cards like the web's, and a fan's lie side by side instead of stacking up. */
static void test_shadow(void)
{
    static Table t;
    table_init(&t);
    table_resize(&t, 1080, 2340, 2.81f);
    table_command(&t, "deal hearts-J spades-9 clubs-A diamonds-10");
    table_update(&t, 10);

    TablePose first, second, last;
    CHECK(table_pose(&t, t.hand[0], &first));
    CHECK(table_pose(&t, t.hand[1], &second));
    CHECK(table_pose(&t, t.hand[3], &last));
    TableShadow whole = table_card_shadow(&t, &last, NULL, true, 0.02f);
    TableShadow trimmed = table_card_shadow(&t, &first, &second, true, 0.02f);
    CHECK(fabsf(whole.half_w * 2 - 0.6f * last.scale) < 0.02f);
    CHECK(trimmed.half_w < whole.half_w * 0.8f);
    CHECK(trimmed.half_w > 0.05f);

    /* The trimmed shadow keeps to its card's left, and a card's shadow falls just below it on screen. */
    TableShadow untrimmed = table_card_shadow(&t, &first, NULL, true, 0.02f);
    CHECK(shadow_centre(&t, &trimmed).x < shadow_centre(&t, &untrimmed).x);
    TablePoint corners[4];
    table_card_corners(&t, &first, corners);
    float bottom = 0, top = 1e9f;
    for (int i = 0; i < 4; i++) {
        bottom = corners[i].y > bottom ? corners[i].y : bottom;
        top = corners[i].y < top ? corners[i].y : top;
    }
    float shadow_y = shadow_centre(&t, &untrimmed).y;
    CHECK(shadow_y > top + (bottom - top) * 0.5f);
    CHECK(shadow_y < bottom);

    /* A card still flying far from the one it lies over keeps its whole shadow. */
    TablePose far = second;
    far.position.x += 5;
    CHECK(fabsf(table_card_shadow(&t, &first, &far, true, 0.02f).half_w - untrimmed.half_w) < 0.001f);
}

/* An idle table draws nothing, so busy has to cover every move and end when the moves do. */
static void test_busy(void)
{
    static Table t;
    table_init(&t);
    table_update(&t, 1);
    CHECK(!table_busy(&t, 1));
    table_command(&t, "deal hearts-J spades-9");
    CHECK(table_busy(&t, 1));
    table_update(&t, 10);
    CHECK(!table_busy(&t, 10));
    table_command(&t, "play 1 clubs-A");
    CHECK(table_busy(&t, 10));
    table_update(&t, 20);
    CHECK(!table_busy(&t, 20));
    table_command(&t, "trick 1");
    CHECK(table_busy(&t, 20));
    table_update(&t, 30);  /* the reveal ends and the sweep begins */
    CHECK(table_busy(&t, 30));
    table_update(&t, 40);
    CHECK(!table_busy(&t, 40));
}

int main(void)
{
    test_busy();
    test_shadow();
    test_flight();
    test_legal_lift();
    test_layout();
    test_names();
    test_projection();
    test_deal_and_trick();
    test_tap();
    test_order_and_snapshot();
    printf(failures ? "%d failed\n" : "all passed\n", failures);
    return failures ? 1 : 0;
}
