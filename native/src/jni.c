/* What Kotlin and the table say to each other. Commands come down as text lines; committed plays go up. */
#include <SDL3/SDL_system.h>
#include <jni.h>
#include <stdio.h>
#include <string.h>
#include <sys/system_properties.h>

#include "commands.h"
#include "core/log.h"
#include "gpu_desc.h"

static jclass bridge_class;
static jmethodID card_played_method;

JNIEXPORT void JNICALL Java_dev_synth_app_TableBridge_nativeAttach(JNIEnv *env, jclass cls)
{
    /* Kept here because a native thread's FindClass can't see the app's classes. */
    bridge_class = (*env)->NewGlobalRef(env, cls);
    card_played_method = (*env)->GetStaticMethodID(env, cls, "onCardPlayed", "(Ljava/lang/String;)V");
}

/* Null when this phone's GPU can run the table, else a short reason. The activity calls it before SDL
 * starts, so an unsupported phone gets a screen in place of closing. In debug builds,
 * `setprop debug.synth.force_unsupported 1` makes any phone fail it, to test that screen. */
JNIEXPORT jstring JNICALL Java_dev_synth_app_TableBridge_nativeGpuSupported(JNIEnv *env, jclass cls, jboolean debug)
{
    char why[256];
    bool ok = gpu_check_support(&synth_gpu_desc, why, sizeof why);
    char forced[PROP_VALUE_MAX] = "";
    if (debug)
        __system_property_get("debug.synth.force_unsupported", forced);
    if (!strcmp(forced, "1")) {
        ok = false;
        snprintf(why, sizeof why, "forced for testing (debug.synth.force_unsupported)");
    }
    return ok ? NULL : (*env)->NewStringUTF(env, why);
}

JNIEXPORT void JNICALL Java_dev_synth_app_TableBridge_nativeCommand(JNIEnv *env, jclass cls, jstring line)
{
    const char *text = (*env)->GetStringUTFChars(env, line, NULL);
    commands_push(text);
    (*env)->ReleaseStringUTFChars(env, line, text);
}

void bridge_card_played(const char *card)
{
    JNIEnv *env = SDL_GetAndroidJNIEnv();
    if (!env || !bridge_class)
        return;
    jstring text = (*env)->NewStringUTF(env, card);
    (*env)->CallStaticVoidMethod(env, bridge_class, card_played_method, text);
    (*env)->DeleteLocalRef(env, text);
    if ((*env)->ExceptionCheck(env)) {
        (*env)->ExceptionDescribe(env);
        (*env)->ExceptionClear(env);
    }
}
