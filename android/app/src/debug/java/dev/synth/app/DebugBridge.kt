package dev.synth.app

import android.content.BroadcastReceiver
import android.content.Context
import android.content.Intent

/**
 * Lets an end-to-end test reach what a finger can't, in debug builds only:
 *   adb shell am broadcast -a dev.synth.app.DEBUG --es cmd state
 * The answers go to logcat under the tag synth.
 */
class DebugBridge : BroadcastReceiver() {
    override fun onReceive(context: Context, intent: Intent) {
        val controller = (context.applicationContext as SynthApp).controller
        when (intent.getStringExtra("cmd")) {
            "state" -> controller.logState()
            "dump" -> TableBridge.command("dump")
            "drop" -> controller.dropSocket()
        }
    }
}
