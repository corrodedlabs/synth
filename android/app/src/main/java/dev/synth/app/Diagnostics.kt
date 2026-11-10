package dev.synth.app

import android.os.Build
import android.os.Handler
import android.os.Looper
import android.os.SystemClock
import android.util.DisplayMetrics
import android.util.Log
import androidx.core.view.WindowInsetsCompat
import dev.synth.core.ClientCommand
import dev.synth.core.encodeCommand
import java.util.UUID
import java.util.concurrent.TimeUnit
import okhttp3.OkHttpClient
import okhttp3.Request
import okhttp3.Response
import okhttp3.WebSocket
import okhttp3.WebSocketListener
import org.json.JSONArray
import org.json.JSONObject

private const val MIN_GAP_MS = 60_000L
private const val GIVE_UP_MS = 15_000L

/**
 * Reports what we need to fix problems on phones we don't have: the phone, its screen and window, and the
 * table's own snapshot (GPU path, sizes, frame rate, where taps landed). The privacy policy lists all of it.
 * No player ID or name goes in, and each report gets a new random ID, so reports can't be linked to a
 * player. The player can turn them off in the in-game menu. Runs on the main thread.
 */
class Diagnostics(private val activity: GameActivity, private val serverUrl: () -> String) {
    private val store = PrefsStore(activity)
    private val main = Handler(Looper.getMainLooper())
    private val client = OkHttpClient.Builder().connectTimeout(8, TimeUnit.SECONDS).build()
    private var lastSent = 0L

    /**
     * [kind] says why: "start", "hand", "background" or "unsupported" (with [reason]). Reports come at most
     * once a minute, except the one for a phone that can't run the table.
     */
    fun send(kind: String, reason: String? = null, settings: JSONObject? = null) {
        if (!store.diagnostics) return
        val now = SystemClock.elapsedRealtime()
        if (kind != "unsupported" && lastSent != 0L && now - lastSent < MIN_GAP_MS) return
        lastSent = now
        val report = try {
            build(kind, reason, settings).toString()
        } catch (e: Exception) {
            Log.w("synth", "diagnostics: couldn't build a report: ${e.message}")
            return
        }
        post(encodeCommand(ClientCommand.Diagnostics(report)))
    }

    private fun build(kind: String, reason: String?, settings: JSONObject?): JSONObject {
        val report = JSONObject()
            .put("report", 1)
            .put("id", UUID.randomUUID().toString())
            .put("kind", kind)
            .put("app", JSONObject()
                .put("version_code", BuildConfig.VERSION_CODE)
                .put("version", BuildConfig.VERSION_NAME)
                .put("debug", BuildConfig.DEBUG))
            .put("device", JSONObject()
                .put("manufacturer", Build.MANUFACTURER)
                .put("brand", Build.BRAND)
                .put("model", Build.MODEL)
                .put("device", Build.DEVICE)
                .put("android", Build.VERSION.RELEASE)
                .put("sdk", Build.VERSION.SDK_INT)
                .put("build", Build.DISPLAY)  // MIUI's version shows here, as V12.5.x
                .put("abi", Build.SUPPORTED_ABIS.firstOrNull()))
            .put("display", display())
            .put("window", window())
        reason?.let { report.put("reason", it) }
        settings?.let { report.put("settings", it) }
        val table = TableBridge.diagnostics()
        report.put("table", try { JSONObject(table) } catch (e: Exception) { table })
        return report
    }

    @Suppress("DEPRECATION")  // the display's real size, on Android 10 too
    private fun display(): JSONObject {
        val display = activity.windowManager.defaultDisplay
        val real = DisplayMetrics().also { display.getRealMetrics(it) }
        return JSONObject()
            .put("size", JSONArray(listOf(real.widthPixels, real.heightPixels)))
            .put("density", real.density.toDouble())
            .put("dpi", real.densityDpi)
            .put("refresh_hz", display.refreshRate.toDouble())
            .put("font_scale", activity.resources.configuration.fontScale.toDouble())
    }

    /** The window the app got, the surface SDL draws to, and what covers their edges. */
    private fun window(): JSONObject {
        val decor = activity.window.decorView
        val window = JSONObject().put("decor", JSONArray(listOf(decor.width, decor.height)))
        activity.surfaceSize()?.let { window.put("surface", JSONArray(listOf(it.first, it.second))) }
        decor.rootWindowInsets?.let { raw ->
            val insets = WindowInsetsCompat.toWindowInsetsCompat(raw, decor)
            fun edges(type: Int) = insets.getInsets(type).let { JSONArray(listOf(it.left, it.top, it.right, it.bottom)) }
            window.put("cutout", edges(WindowInsetsCompat.Type.displayCutout()))
            window.put("bars", edges(WindowInsetsCompat.Type.systemBars()))
            window.put("bars_visible", insets.isVisible(WindowInsetsCompat.Type.systemBars()))
        }
        return window
    }

    /** A connection of its own, so a report never mixes with the game's messages. */
    private fun post(message: String) {
        val socket = client.newWebSocket(
            Request.Builder().url(serverUrl()).build(),
            object : WebSocketListener() {
                override fun onOpen(webSocket: WebSocket, response: Response) {
                    webSocket.send(message)
                }

                override fun onMessage(webSocket: WebSocket, text: String) {
                    if (text != "(done)") Log.i("synth", "diagnostics: the server said $text")
                    webSocket.close(1000, null)
                }

                override fun onFailure(webSocket: WebSocket, t: Throwable, response: Response?) {
                    Log.i("synth", "diagnostics: not sent (${t.message})")
                }
            },
        )
        main.postDelayed({ socket.cancel() }, GIVE_UP_MS)
    }
}
