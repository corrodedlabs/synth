package dev.synth.app

import android.os.Handler
import android.os.Looper
import android.util.Log

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

    /**
     * Null when this phone's GPU can run the table, else why not. It runs before SDL starts, so it
     * loads the native libraries itself; SDL loading them again later does nothing.
     */
    fun gpuProblem(): String? {
        try {
            System.loadLibrary("SDL3")
            System.loadLibrary("main")
        } catch (e: UnsatisfiedLinkError) {
            Log.w("synth", "native libraries didn't load for the GPU check: ${e.message}")
            return null  // SDL then reports the broken install itself.
        }
        return nativeGpuSupported(BuildConfig.DEBUG)
    }

    fun command(line: String) {
        if (loaded) nativeCommand(line)
    }

    /** The table's diagnostics snapshot as JSON (native/src/diagnostics.h), "{}" before it starts. */
    fun diagnostics(): String = if (loaded) nativeDiagnostics() else "{}"

    /** From the native game thread: hop to the thread the session lives on. */
    @JvmStatic
    fun onCardPlayed(card: String) {
        main.post { onPlay?.invoke(card) }
    }

    @JvmStatic
    private external fun nativeAttach()

    @JvmStatic
    private external fun nativeCommand(line: String)

    @JvmStatic
    private external fun nativeDiagnostics(): String

    /** Null when supported, else the reason. [debug] lets debug.synth.force_unsupported force a failure. */
    @JvmStatic
    private external fun nativeGpuSupported(debug: Boolean): String?
}
