/* The native half of the Android app, and a desktop runner for the same table. On desktop, --demo plays a
 * scripted hand so the table can be seen and tested without a server. */
#include <SDL3/SDL_main.h>
#include <SDL3/SDL_timer.h>
#include <stb_image.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "commands.h"
#include "core/file.h"
#include "core/log.h"
#include "platform/platform.h"
#include "simple/simple.h"
#include "sound.h"
#include "table.h"

#ifdef __ANDROID__
#include <sys/system_properties.h>
void bridge_card_played(const char *card);  /* jni.c */
#endif

#define CARD_W 0.6f
#define CARD_H 0.9f

typedef struct {
    Table table;
    Audio audio;
    TableSounds sounds;
    Simple simple;
    SimpleFont font;
    uint32_t atlas_slot;
    float density;
} App;

static void on_pointer(void *user, const PointerEvent *e)
{
    App *app = user;
    if (e->action == POINTER_TAP)
        table_tap(&app->table, e->x, e->y, e->touch);
}

static void on_play(void *user, const char *card)
{
    App *app = user;
    nm_log("table: played %s", card);
#ifdef __ANDROID__
    bridge_card_played(card);
#else
    char line[64];
    snprintf(line, sizeof line, "play 0 %s", card);
    table_command(&app->table, line);
#endif
}

static GpuImage load_atlas(Gpu *gpu, const char *path)
{
    size_t size = 0;
    void *file = file_read(path, &size);
    if (!file)
        nm_fatal("missing %s", path);
    int w, h, channels;
    stbi_uc *pixels = stbi_load_from_memory(file, (int)size, &w, &h, &channels, 4);
    free(file);
    if (!pixels)
        nm_fatal("%s is not an image: %s", path, stbi_failure_reason());
    GpuImage image = gpu_texture_rgba8(gpu, (uint32_t)w, (uint32_t)h, pixels, true, "cards");
    stbi_image_free(pixels);
    return image;
}

static void run_command(App *app, const char *line)
{
    if (!strncmp(line, "sound ", 6)) {
        if (!table_sound_play(&app->sounds, &app->audio, line + 6))
            nm_log("table: no sound %s", line + 6);
    } else if (!strncmp(line, "mute ", 5)) {
        audio_set_muted(&app->audio, line[5] == '1');
    } else {
        table_command(&app->table, line);
    }
}

/* Each card's shadow lies flat on the table below it, as the directional light from above would cast it,
 * and spreads as the card rises. */
static void draw_shadow(App *app, const TablePose *pose)
{
    Table *t = &app->table;
    const float plane = -0.4f;
    float width = CARD_W * pose->scale, depth = CARD_H * pose->scale * fabsf(sinf(pose->rx));
    if (depth < 0.1f)
        depth = 0.1f;
    float soft = 0.1f + 0.08f * (pose->position.y - plane);
    float w = width + soft * 2, d = depth + soft * 2;
    float cz = cosf(pose->rz), sz = sinf(pose->rz);
    /* A flat quad on the plane: x across, y along the table towards the far side. */
    mat4 world = {{w * cz, 0, -w * sz, 0, -d * sz, 0, -d * cz, 0, 0, 1, 0, 0,
                   pose->position.x, plane, pose->position.z + 0.08f, 1}};
    mat4 clip = m4_mul(t->view_proj, world);
    simple_shadow(&app->simple, &clip, width * 0.5f, depth * 0.5f, 0.03f, soft,
                  (SimpleColour){0.545f * 0.25f, 0.451f * 0.25f, 0.333f * 0.25f, 0.2f});
}

static void draw_card(App *app, int card, const TablePose *pose, bool dimmed)
{
    Table *t = &app->table;
    mat4 clip = table_card_matrix(t, pose, CARD_W, CARD_H);
    SimpleMaterial material = {.corner = 0.08f, .aspect = CARD_W / CARD_H};
    table_atlas_uv(card, material.uv);
    float shade = dimmed ? 0.62f : 1.0f;
    shade += pose->glow * 0.2f;
    material.tint = (SimpleColour){shade, shade, shade * (dimmed ? 0.95f : 1), 1};
    simple_draw(&app->simple, &app->simple.quad, &clip, app->atlas_slot, &material);
}

static void draw_names(App *app)
{
    Table *t = &app->table;
    float px = 14 * app->density;
    for (int seat = 1; seat < 4; seat++) {
        const char *name = t->names[seat];
        if (!name[0])
            continue;
        float x = t->layout.seats[seat].x * app->density, y = t->layout.seats[seat].y * app->density;
        float w = simple_text_width(&app->font, px, name);
        float left = x - w / 2;
        if (left < 6 * app->density)
            left = 6 * app->density;
        if (left + w > t->view_w - 6 * app->density)
            left = t->view_w - 6 * app->density - w;
        bool active = t->active == seat;
        SimpleColour ink = active ? (SimpleColour){0.55f, 0.13f, 0.08f, 1} : (SimpleColour){0.10f, 0.09f, 0.08f, 0.85f};
        simple_text(&app->simple, &app->font, left, y, px, ink, name);
    }
}

static void draw_table(App *app)
{
    Table *t = &app->table;
    int order[TABLE_DECK], n = 0;
    /* Back to front: what's being swept, the trick, then the fan. */
    for (int i = 0; i < TABLE_DECK; i++)
        if (t->cards[i].state == CARD_COLLECT)
            order[n++] = i;
    for (int i = 0; i < t->trick_count; i++)
        order[n++] = t->trick[i];
    for (int i = 0; i < t->hand_count; i++)
        order[n++] = t->hand[i];

    for (int i = 0; i < n; i++) {
        TablePose pose;
        if (table_pose(t, order[i], &pose))
            draw_shadow(app, &pose);
    }
    draw_names(app);
    bool any_legal = false;
    for (int i = 0; i < t->hand_count; i++)
        any_legal = any_legal || t->legal[t->hand[i]];
    for (int i = 0; i < n; i++) {
        TablePose pose;
        if (table_pose(t, order[i], &pose)) {
            bool in_hand = t->cards[order[i]].state == CARD_HAND || t->cards[order[i]].state == CARD_DEALING;
            draw_card(app, order[i], &pose, in_hand && any_legal && !t->legal[order[i]]);
        }
    }
}

/* Scripted steps for --demo: seconds from the start, then the command. */
static const struct {
    double at;
    const char *command;
} DEMO[] = {
    {0.2, "seats You|Asha|Ravi|Meera"},
    {0.3, "deal hearts-J spades-9 clubs-A diamonds-10"},
    {0.31, "sound deal"},
    {1.2, "deal spades-K hearts-Q clubs-8 diamonds-7"},
    {1.21, "sound deal"},
    {2.2, "active 1"},
    {2.3, "play 1 hearts-9"},
    {2.31, "sound card"},
    {3.0, "active 2"},
    {3.1, "play 2 hearts-A"},
    {3.11, "sound card"},
    {3.8, "active 3"},
    {3.9, "play 3 hearts-7"},
    {3.91, "sound card"},
    {4.6, "active 0"},
    {4.61, "sound turn"},
    {4.7, "legal hearts-J hearts-Q"},
};

int main(int argc, char **argv)
{
    bool headless = false, no_validation = false, demo = false;
    int max_frames = 0;
    const char *screenshot = NULL;
    int width = 412, height = 892;
    for (int i = 1; i < argc; i++) {
        if (!strcmp(argv[i], "--headless"))
            headless = true;
        else if (!strcmp(argv[i], "--no-validation"))
            no_validation = true;
        else if (!strcmp(argv[i], "--demo"))
            demo = true;
        else if (!strcmp(argv[i], "--frames") && i + 1 < argc)
            max_frames = atoi(argv[++i]);
        else if (!strcmp(argv[i], "--screenshot") && i + 1 < argc)
            screenshot = argv[++i];
        else if (!strcmp(argv[i], "--size") && i + 2 < argc) {
            width = atoi(argv[++i]);
            height = atoi(argv[++i]);
        }
    }

    static App app;
    table_init(&app.table);
    app.table.on_play = on_play;
    app.table.user = &app;
    Platform platform;
    platform_init(&platform, &(PlatformDesc){
        .title = "28", .width = width, .height = height, .headless = headless, .on_pointer = on_pointer, .user = &app});
    app.density = platform.density;
#ifdef __ANDROID__
    {
        char value[PROP_VALUE_MAX] = "";
        __system_property_get("debug.apsis.validation", value);
#ifdef NDEBUG
        no_validation = strcmp(value, "1") != 0;
#else
        no_validation = strcmp(value, "0") == 0;
#endif
    }
#endif

    Gpu gpu;
    gpu_init(&gpu, &(GpuDesc){
        .window = platform.window,
        .app_name = "28",
        .shader_dir = SYNTH_SHADERS,
        .validation = !no_validation,
        .vsync = !headless,
        .paced = true,
    });
    audio_init(&app.audio);
    table_sounds_init(&app.sounds, &app.audio);
    platform_attach(&platform, &gpu, &app.audio);
    simple_init(&app.simple, &gpu);

    GpuImage atlas = load_atlas(&gpu, SYNTH_ASSETS "/cards.png");
    app.atlas_slot = gpu_texture_slot(&gpu);
    gpu_bind_texture(&gpu, app.atlas_slot, atlas.view, gpu.linear_clamp, VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL);
    if (!simple_font(&app.simple, &app.font, SYNTH_ASSETS "/DejaVuSans.ttf", 40))
        nm_fatal("missing the table's font");

    Uint64 start = SDL_GetTicksNS();
    size_t demo_step = 0;
    int frames = 0;
    uint32_t last_w = 0, last_h = 0;
    Uint64 report_start = start;
    int report_frames = 0;
    double report_gpu = 0;
    while (platform_poll(&platform)) {
        double now = (SDL_GetTicksNS() - start) / 1e9;
        if (demo) {
            /* A headless run has no clock worth trusting at load, so it steps in frames at 60 per second. */
            double t = headless ? frames / 60.0 : now;
            while (demo_step < sizeof DEMO / sizeof *DEMO && DEMO[demo_step].at <= t)
                run_command(&app, DEMO[demo_step++].command);
            now = t;
        }
        for (char *line; (line = commands_pop()); SDL_free(line))
            run_command(&app, line);
        table_update(&app.table, now);

        if (screenshot && frames == max_frames - 1) {
            gpu.capture_path = screenshot;
            screenshot = NULL;
        }
        VkCommandBuffer cmd = gpu_begin_frame(&gpu);
        if (!cmd) {
            SDL_Delay(5);
            report_start = SDL_GetTicksNS();
            report_frames = 0;
            report_gpu = 0;
            continue;
        }
        uint32_t view_w, view_h;
        gpu_view_size(&gpu, &view_w, &view_h);
        if (view_w != last_w || view_h != last_h) {
            table_resize(&app.table, (float)view_w, (float)view_h, app.density);
            last_w = view_w;
            last_h = view_h;
            nm_log("table: view %ux%u, density %.2f", view_w, view_h, app.density);
        }
        simple_begin(&app.simple, cmd, (SimpleColour){0.98f, 0.96f, 0.92f, 1});
        /* The web page's warm paper, from near white down to its edge. */
        simple_gradient(&app.simple, (SimpleColour){0.992f, 0.980f, 0.953f, 1}, (SimpleColour){0.937f, 0.898f, 0.847f, 1});
        draw_table(&app);
        simple_end(&app.simple);
        gpu_timestamp(&gpu, cmd, "table");
        gpu_end_frame(&gpu);

        frames++;
        report_frames++;
        report_gpu += gpu.gpu_ms;
        Uint64 clock = SDL_GetTicksNS();
        if ((clock - report_start) / 1e9 >= 5) {
            nm_log("table: %.1f fps, GPU %.2f ms", report_frames / ((clock - report_start) / 1e9),
                   report_gpu / report_frames);
            report_start = clock;
            report_frames = 0;
            report_gpu = 0;
        }
        if (max_frames && frames >= max_frames)
            break;
    }

    vkDeviceWaitIdle(gpu.device);
    simple_font_free(&app.simple, &app.font);
    gpu_image_free(&gpu, &atlas);
    simple_free(&app.simple);
    audio_free(&app.audio);
    gpu_shutdown(&gpu);
    platform_free(&platform);
    return 0;
}
