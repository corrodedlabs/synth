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

    table_command(&t, "legal spades-9 hearts-J");
    table_tap(&t, c.x, c.y, true);
    CHECK(t.cards[table_card_index("spades-9")].armed);  /* first tap raises */
    CHECK(!played[0]);
    table_update(&t, 3);
    c = centre_of(&t, "spades-9");
    table_tap(&t, c.x, c.y, true);
    CHECK(!strcmp(played, "spades-9"));                  /* second commits */

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

int main(void)
{
    test_layout();
    test_names();
    test_projection();
    test_deal_and_trick();
    test_tap();
    test_order_and_snapshot();
    printf(failures ? "%d failed\n" : "all passed\n", failures);
    return failures ? 1 : 0;
}
