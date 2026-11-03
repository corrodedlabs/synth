package dev.synth.app

import android.content.Context
import dev.synth.core.GameSocket
import dev.synth.core.GameSocketFactory
import dev.synth.core.SessionStore
import dev.synth.core.SocketError
import java.util.concurrent.TimeUnit
import kotlin.coroutines.resume
import kotlin.coroutines.resumeWithException
import kotlinx.coroutines.channels.Channel
import kotlinx.coroutines.channels.ReceiveChannel
import kotlinx.coroutines.suspendCancellableCoroutine
import kotlinx.coroutines.withTimeoutOrNull
import okhttp3.OkHttpClient
import okhttp3.Request
import okhttp3.Response
import okhttp3.WebSocket
import okhttp3.WebSocketListener

/** The saved identity, the same things the web client keeps in localStorage. */
class PrefsStore(context: Context) : SessionStore {
    private val prefs = context.getSharedPreferences("synth", Context.MODE_PRIVATE)

    override var playerId: String?
        get() = prefs.getString("player-id", null)
        set(value) = prefs.edit().putString("player-id", value).apply()
    override var playerName: String?
        get() = prefs.getString("player-name", null)
        set(value) = prefs.edit().putString("player-name", value).apply()
    override var activeMatch: String?
        get() = prefs.getString("active-match", null)
        set(value) = prefs.edit().putString("active-match", value).apply()

    var muted: Boolean
        get() = prefs.getBoolean("muted", false)
        set(value) = prefs.edit().putBoolean("muted", value).apply()
}

private const val HANDSHAKE_MS = 12_000L

class OkHttpSocketFactory : GameSocketFactory {
    private val client = OkHttpClient.Builder()
        .connectTimeout(8, TimeUnit.SECONDS)
        .readTimeout(0, TimeUnit.SECONDS)
        .build()

    // OkHttp's connect timeout stops at TCP: a server that accepts and then says nothing would hang here.
    override suspend fun connect(url: String): GameSocket =
        withTimeoutOrNull(HANDSHAKE_MS) { open(url) } ?: throw SocketError("the server didn't answer")

    private suspend fun open(url: String): GameSocket = suspendCancellableCoroutine { continuation ->
        val incoming = Channel<String>(Channel.UNLIMITED)
        val socket = OkHttpGameSocket(incoming)
        val request = Request.Builder().url(url).build()
        val web = client.newWebSocket(request, object : WebSocketListener() {
            override fun onOpen(webSocket: WebSocket, response: Response) {
                socket.open = true
                if (continuation.isActive) continuation.resume(socket)
            }

            override fun onMessage(webSocket: WebSocket, text: String) {
                incoming.trySend(text)
            }

            override fun onClosing(webSocket: WebSocket, code: Int, reason: String) {
                webSocket.close(1000, null)
            }

            override fun onClosed(webSocket: WebSocket, code: Int, reason: String) {
                socket.open = false
                incoming.close()
            }

            override fun onFailure(webSocket: WebSocket, t: Throwable, response: Response?) {
                socket.open = false
                incoming.close(t)
                if (continuation.isActive) {
                    continuation.resumeWithException(SocketError(t.message ?: "could not connect"))
                }
            }
        })
        socket.web = web
        continuation.invokeOnCancellation { web.cancel() }
    }
}

private class OkHttpGameSocket(private val channel: Channel<String>) : GameSocket {
    @Volatile
    var open = false
    lateinit var web: WebSocket

    override val incoming: ReceiveChannel<String> get() = channel
    override val isOpen: Boolean get() = open

    override fun send(text: String) {
        if (open) web.send(text)
    }

    // OkHttp sends what's queued before the close frame.
    override fun close() {
        open = false
        web.close(1000, null)
    }

    override fun drop() {
        open = false
        web.cancel()
        channel.close()
    }
}
