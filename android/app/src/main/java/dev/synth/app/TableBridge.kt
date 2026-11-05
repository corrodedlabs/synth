package dev.synth.app

import android.os.Handler
import android.os.Looper

/** The line to the native table. Commands go down as text (native/src/table.h lists them). */
object TableBridge {
    private val main = Handler(Looper.getMainLooper())

    @Volatile
    var onPlay: ((String) -> Unit)? = null

    private var loaded = false

    /** The activity loads the native libraries before this is called. */
    fun attach() {
        nativeAttach()
        loaded = true
    }

    fun command(line: String) {
        if (loaded) nativeCommand(line)
    }

    /** From the native game thread: hop to the thread the session lives on. */
    @JvmStatic
    fun onCardPlayed(card: String) {
        main.post { onPlay?.invoke(card) }
    }

    @JvmStatic
    private external fun nativeAttach()

    @JvmStatic
    private external fun nativeCommand(line: String)
}
