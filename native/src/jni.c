/* What Kotlin and the table say to each other. Commands come down as text lines; committed plays go up. */
#include <SDL3/SDL_system.h>
#include <jni.h>

#include "commands.h"
#include "core/log.h"

static jclass bridge_class;
static jmethodID card_played_method;

JNIEXPORT void JNICALL Java_dev_synth_app_TableBridge_nativeAttach(JNIEnv *env, jclass cls)
{
    /* Kept here because a native thread's FindClass can't see the app's classes. */
    bridge_class = (*env)->NewGlobalRef(env, cls);
    card_played_method = (*env)->GetStaticMethodID(env, cls, "onCardPlayed", "(Ljava/lang/String;)V");
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
