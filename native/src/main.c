/* The native half of the Android app, and a desktop runner for the same table. On desktop, --demo plays a
 * scripted hand so the table can be seen and tested without a server. */
#include <SDL3/SDL_atomic.h>
#include <SDL3/SDL_events.h>
#include <SDL3/SDL_main.h>
#include <SDL3/SDL_timer.h>
#include <stb_image.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "commands.h"
#include "core/file.h"
#include "core/log.h"
#include "diagnostics.h"
#include "gpu_desc.h"
#include "platform/platform.h"
#include "simple/simple.h"
#include "sound.h"
#include "table.h"

#ifdef __ANDROID__
#include <sys/system_properties.h>
void bridge_card_played(const char *card);  /* jni.c */
#endif

/* The table needs nothing beyond the engine's base, and runs on the Vulkan 1.1 path too, so it reaches
 * the most phones. Its shaders are built for vulkan1.1 to match (CMakeLists.txt). */
const GpuDesc synth_gpu_desc = {
    .app_name = "28",
    .shader_dir = SYNTH_SHADERS,
    .paced = true,
    .allow_vulkan_1_1 = true,
    /* The table holds the card atlas and the font. A small table fits phones without descriptor
     * indexing, such as Mali-G76 ones, which hold 128 textures, and Apple's GPUs. */
    .textures = SIMPLE_TEXTURES,
};

#define CARD_W 0.6f
#define CARD_H 0.9f
#define IDLE_NAP_MS 8        /* about one frame, so a touch on a still table feels no later */
#define IDLE_REDRAW_MS 1000
#define DIAGNOSTIC_TAPS 12
#define DRAWN_GAP_S 0.25     /* frames further apart than this had a nap between them */

typedef struct {
    Table table;
    Audio audio;
    TableSounds sounds;
    Simple simple;
    SimpleFont font;
    uint32_t atlas_slot;
    float density;

    /* For diagnostics (diagnostics.h). */
    Gpu *gpu;
    Platform *platform;
    TableTap taps[DIAGNOSTIC_TAPS];
    unsigned tap_total;
    double fps, fps_min, worst_frame_ms, gpu_ms;  /* while drawing: naps between frames don't count */
    int frames;
} App;

static SDL_SpinLock diagnostics_lock;
static char diagnostics[8192] = "{}";

void synth_diagnostics(char *out, size_t size)
{
    SDL_LockSpinlock(&diagnostics_lock);
    snprintf(out, size, "%s", diagnostics);
    SDL_UnlockSpinlock(&diagnostics_lock);
}

static void put(char *buffer, size_t size, size_t *used, const char *format, ...)
{
    if (*used >= size - 1)
        return;
    va_list args;
    va_start(args, format);
    int n = vsnprintf(buffer + *used, size - *used, format, args);
    va_end(args);
    if (n > 0)
        *used = *used + (size_t)n < size - 1 ? *used + (size_t)n : size - 1;
}

/* JSON-safe and plain ASCII, which Java's modified UTF-8 takes as it is. */
static void plain(const char *in, char *out, size_t size)
{
    size_t i = 0;
    for (; in[i] && i < size - 1; i++)
        out[i] = (in[i] < 32 || in[i] > 126 || in[i] == '"' || in[i] == '\\') ? '?' : in[i];
    out[i] = 0;
}

static void diagnostics_update(App *app)
{
    static const char *results[] = {"felt", "lowered", "unplayable", "raised", "played"};
    static const char *modes[] = {"IMMEDIATE", "MAILBOX", "FIFO", "FIFO_RELAXED"};
    const Gpu *gpu = app->gpu;
    char name[VK_MAX_PHYSICAL_DEVICE_NAME_SIZE];
    plain(gpu->props.deviceName, name, sizeof name);
    int window_w = 0, window_h = 0;
    SDL_GetWindowSizeInPixels(app->platform->window, &window_w, &window_h);
    uint32_t view_w = 0, view_h = 0;
    gpu_view_size(gpu, &view_w, &view_h);
    uint32_t driver = gpu->props.driverVersion;

    char json[sizeof diagnostics];
    size_t n = 0;
    put(json, sizeof json, &n,
        "{\"gpu\":\"%s\",\"vendor\":%u,\"driver\":\"%u.%u.%u\",\"driver_raw\":%u,\"vulkan\":\"%u.%u\","
        "\"path\":\"%s\",\"indexing\":%s,\"swapchain\":[%u,%u],\"transform\":%u,\"present\":\"%s\","
        "\"window\":[%d,%d],\"view\":[%u,%u],\"table_view\":[%.0f,%.0f],\"density\":%.3f,"
        "\"fps\":%.1f,\"fps_min\":%.1f,\"worst_frame_ms\":%.1f,\"gpu_ms\":%.2f,\"frames\":%d,\"taps\":[",
        name, gpu->props.vendorID, VK_API_VERSION_MAJOR(driver), VK_API_VERSION_MINOR(driver),
        VK_API_VERSION_PATCH(driver), driver, VK_API_VERSION_MAJOR(gpu->api_version),
        VK_API_VERSION_MINOR(gpu->api_version), gpu->legacy ? "1.1 fallback" : "1.3",
        (gpu->features & GPU_FEATURE_DESCRIPTOR_INDEXING) ? "true" : "false", gpu->extent.width,
        gpu->extent.height, (unsigned)gpu->surface_transform,
        gpu->present_mode <= VK_PRESENT_MODE_FIFO_RELAXED_KHR ? modes[gpu->present_mode] : "other", window_w,
        window_h, view_w, view_h, app->table.view_w, app->table.view_h, app->density, app->fps, app->fps_min,
        app->worst_frame_ms, app->gpu_ms, app->frames);
    unsigned count = app->tap_total < DIAGNOSTIC_TAPS ? app->tap_total : DIAGNOSTIC_TAPS;
    for (unsigned i = 0; i < count; i++) {
        const TableTap *tap = &app->taps[(app->tap_total - count + i) % DIAGNOSTIC_TAPS];
        char card[16] = "";
        if (tap->card >= 0)
            table_card_name(tap->card, card);
        put(json, sizeof json, &n,
            "%s{\"t\":%.2f,\"x\":%.0f,\"y\":%.0f,\"result\":\"%s\",\"card\":\"%s\",\"fan\":[%.0f,%.0f,%.0f,%.0f]}",
            i ? "," : "", tap->at, tap->x, tap->y, results[tap->result], card, tap->fan[0], tap->fan[1],
            tap->fan[2], tap->fan[3]);
    }
    put(json, sizeof json, &n, "]}");
    if (n >= sizeof json - 1)
        snprintf(json, sizeof json, "{\"error\":\"diagnostics too long\"}");

    SDL_LockSpinlock(&diagnostics_lock);
    memcpy(diagnostics, json, sizeof json);
    SDL_UnlockSpinlock(&diagnostics_lock);
}

static void on_pointer(void *user, const PointerEvent *e)
{
    App *app = user;
    if (e->action != POINTER_TAP)
        return;
    table_tap(&app->table, e->x, e->y, e->touch);
    app->taps[app->tap_total++ % DIAGNOSTIC_TAPS] = app->table.last_tap;
    diagnostics_update(app);
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

/* A pale, nearly sharp band, as the web's shadow material gives. The tint is linear: the swapchain is sRGB. */
#define SHADOW_SOFTNESS 0.012f

static void draw_shadow(App *app, const TablePose *pose, const TablePose *covering, bool in_fan)
{
    TableShadow shadow = table_card_shadow(&app->table, pose, covering, in_fan, SHADOW_SOFTNESS);
    simple_shadow(&app->simple, &shadow.clip, shadow.half_w, shadow.half_h, 0.012f, SHADOW_SOFTNESS,
                  (SimpleColour){0.26f, 0.15f, 0.03f, 0.2f});
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

    /* A fan's shadows are trimmed where the next card covers them, so they lay down one even band. */
    int fan_start = n - t->hand_count;
    for (int i = 0; i < n; i++) {
        TablePose pose, next;
        if (!table_pose(t, order[i], &pose))
            continue;
        bool in_fan = i >= fan_start;
        bool covered = in_fan && i + 1 < n && table_pose(t, order[i + 1], &next);
        draw_shadow(app, &pose, covered ? &next : NULL, in_fan);
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
    bool headless = false, no_validation = false, demo = false, print_diagnostics = false;
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
        else if (!strcmp(argv[i], "--diagnostics"))
            print_diagnostics = true;  /* the snapshot the Android app sends, printed at exit */
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
    GpuDesc desc = synth_gpu_desc;
    desc.window = platform.window;
    desc.validation = !no_validation;
    desc.vsync = !headless;
    gpu_init(&gpu, &desc);
    audio_init(&app.audio);
    table_sounds_init(&app.sounds, &app.audio);
    platform_attach(&platform, &gpu, &app.audio);
    app.gpu = &gpu;
    app.platform = &platform;
    diagnostics_update(&app);
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
    Uint64 last_drawn = 0;
    int drawn_frames = 0;
    double drawn_seconds = 0, drawn_worst = 0;
    /* A still table draws nothing. Scripted and headless runs step by frames, so they always draw. */
    bool always_draw = demo || headless || max_frames;
    bool dirty = true, was_busy = false;
    int idle_ms = 0;
    while (platform_poll(&platform)) {
        double now = (SDL_GetTicksNS() - start) / 1e9;
        if (demo) {
            /* A headless run has no clock worth trusting at load, so it steps in frames at 60 per second. */
            double t = headless ? frames / 60.0 : now;
            while (demo_step < sizeof DEMO / sizeof *DEMO && DEMO[demo_step].at <= t)
                run_command(&app, DEMO[demo_step++].command);
            now = t;
        }
        for (char *line; (line = commands_pop()); SDL_free(line)) {
            run_command(&app, line);
            dirty = true;
        }
        table_update(&app.table, now);
        bool busy = table_busy(&app.table, now);
        if (!always_draw && !dirty && !busy && !was_busy) {
            /* Nap until a touch, a command or a window change. SDL_WaitEventTimeout would spin a whole
             * core on Android. The slow redraw repairs anything that changed the surface unannounced. */
            SDL_Delay(IDLE_NAP_MS);
            idle_ms += IDLE_NAP_MS;
            SDL_PumpEvents();
            dirty = SDL_HasEvents(SDL_EVENT_FIRST, SDL_EVENT_LAST) || idle_ms >= IDLE_REDRAW_MS;
            continue;
        }

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
        dirty = false;
        was_busy = busy;
        idle_ms = 0;
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
        double gap = last_drawn ? (clock - last_drawn) / 1e9 : DRAWN_GAP_S;
        last_drawn = clock;
        if (gap < DRAWN_GAP_S) {
            drawn_frames++;
            drawn_seconds += gap;
            if (gap > drawn_worst)
                drawn_worst = gap;
        }
        if ((clock - report_start) / 1e9 >= 5) {
            nm_log("table: %.1f fps, GPU %.2f ms", report_frames / ((clock - report_start) / 1e9),
                   report_gpu / report_frames);
            if (drawn_frames >= 10) {
                app.fps = drawn_frames / drawn_seconds;
                if (app.fps_min == 0 || app.fps < app.fps_min)
                    app.fps_min = app.fps;
                app.worst_frame_ms = drawn_worst * 1000;
            }
            app.gpu_ms = report_gpu / report_frames;
            app.frames = frames;
            diagnostics_update(&app);
            report_start = clock;
            report_frames = 0;
            report_gpu = 0;
            drawn_frames = 0;
            drawn_seconds = drawn_worst = 0;
        }
        if (max_frames && frames >= max_frames)
            break;
    }

    if (print_diagnostics) {
        app.frames = frames;
        diagnostics_update(&app);
        char json[8192];
        synth_diagnostics(json, sizeof json);
        printf("diagnostics: %s\n", json);
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
