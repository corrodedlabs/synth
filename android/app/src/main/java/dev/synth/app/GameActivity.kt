package dev.synth.app

import android.annotation.SuppressLint
import android.content.Intent
import android.os.Build
import android.os.Bundle
import android.util.Log
import android.view.KeyEvent
import android.view.ViewGroup
import android.widget.FrameLayout
import android.window.OnBackInvokedCallback
import android.window.OnBackInvokedDispatcher
import androidx.compose.runtime.Composable
import androidx.compose.ui.platform.ComposeView
import androidx.compose.ui.platform.ViewCompositionStrategy
import androidx.lifecycle.Lifecycle
import androidx.lifecycle.LifecycleOwner
import androidx.lifecycle.LifecycleRegistry
import androidx.lifecycle.setViewTreeLifecycleOwner
import androidx.savedstate.SavedStateRegistry
import androidx.savedstate.SavedStateRegistryController
import androidx.savedstate.SavedStateRegistryOwner
import androidx.savedstate.setViewTreeSavedStateRegistryOwner
import dev.synth.app.ui.GameScreens
import dev.synth.app.ui.UnsupportedScreen
import org.libsdl.app.SDLActivity

/**
 * SDL draws the table on its surface; Compose lays menus and panels over it. A touch that no panel
 * takes falls through to the table. SDLActivity is a plain Activity, so the owners Compose expects
 * are provided here.
 */
class GameActivity : SDLActivity(), LifecycleOwner, SavedStateRegistryOwner {
    private val registry = LifecycleRegistry(this)
    private val savedState = SavedStateRegistryController.create(this)
    private val backCallback = if (Build.VERSION.SDK_INT >= 33) {
        OnBackInvokedCallback { moveTaskToBack(true) }
    } else null

    override val lifecycle: Lifecycle get() = registry
    override val savedStateRegistry: SavedStateRegistry get() = savedState.savedStateRegistry

    private val controller: GameController get() = (application as SynthApp).controller

    /** Why this phone's GPU can't run the table, or null when it can. */
    private var gpuProblem: String? = null

    /** The table an invite link asked for, for "Play in browser" on a phone that can't run it. */
    private var inviteRoom: String? = null

    override fun getLibraries(): Array<String> = arrayOf("SDL3", "main")

    override fun onCreate(savedInstanceState: Bundle?) {
        // Before SDL: a phone that can't run the table must get a screen, not a closed app.
        gpuProblem = TableBridge.gpuProblem()
        savedState.performRestore(savedInstanceState)
        super.onCreate(savedInstanceState)
        if (Build.VERSION.SDK_INT >= 33) {
            backCallback?.let {
                onBackInvokedDispatcher.registerOnBackInvokedCallback(OnBackInvokedDispatcher.PRIORITY_DEFAULT, it)
            }
        }
        registry.handleLifecycleEvent(Lifecycle.Event.ON_CREATE)

        gpuProblem?.let { problem ->
            // Only in logcat: nothing leaves the phone.
            Log.i("synth", "this phone can't run the table: $problem")
            inviteRoom = roomFrom(intent)
            // This replaces SDL's view before the window first draws. SDL starts its native thread, and
            // with it the GPU, only once its surface exists, so neither ever starts.
            setContentView(composeView { UnsupportedScreen(problem) { openWebGame(this@GameActivity, inviteRoom) } })
            return
        }

        TableBridge.attach()
        addContentView(
            composeView { GameScreens(controller, ::shareInvite) },
            FrameLayout.LayoutParams(ViewGroup.LayoutParams.MATCH_PARENT, ViewGroup.LayoutParams.MATCH_PARENT),
        )

        if (BuildConfig.DEBUG) intent.getStringExtra("server")?.let(controller::useServer)
        controller.resync()
        handleJoin(intent)
        controller.start()
    }

    override fun onNewIntent(intent: Intent) {
        super.onNewIntent(intent)
        setIntent(intent)
        if (gpuProblem != null) roomFrom(intent)?.let { inviteRoom = it } else handleJoin(intent)
    }

    override fun onStart() {
        super.onStart()
        registry.handleLifecycleEvent(Lifecycle.Event.ON_START)
    }

    override fun onResume() {
        super.onResume()
        registry.handleLifecycleEvent(Lifecycle.Event.ON_RESUME)
        if (gpuProblem == null) controller.foreground()
    }

    override fun onPause() {
        registry.handleLifecycleEvent(Lifecycle.Event.ON_PAUSE)
        super.onPause()
    }

    override fun onStop() {
        registry.handleLifecycleEvent(Lifecycle.Event.ON_STOP)
        super.onStop()
    }

    override fun onDestroy() {
        if (Build.VERSION.SDK_INT >= 33) {
            backCallback?.let(onBackInvokedDispatcher::unregisterOnBackInvokedCallback)
        }
        registry.handleLifecycleEvent(Lifecycle.Event.ON_DESTROY)
        super.onDestroy()
    }

    override fun onSaveInstanceState(outState: Bundle) {
        super.onSaveInstanceState(outState)
        savedState.performSave(outState)
    }

    /**
     * Back leaves the app running, like Home: closing the activity would tear down the table. Older
     * Android versions send a key event; Android 13+ uses the callback registered in onCreate.
     */
    @SuppressLint("GestureBackNavigation") // Only the pre-33 fallback; newer versions use backCallback.
    override fun dispatchKeyEvent(event: KeyEvent): Boolean {
        if (Build.VERSION.SDK_INT >= 33) return super.dispatchKeyEvent(event)
        if (event.keyCode != KeyEvent.KEYCODE_BACK) return super.dispatchKeyEvent(event)
        if (event.action == KeyEvent.ACTION_UP && !event.isCanceled) moveTaskToBack(true)
        return true
    }

    private fun handleJoin(intent: Intent?) {
        roomFrom(intent)?.let { controller.joinTarget = it }
    }

    /** The table in an invite link: respawnfi.com/?join=ROOM or synth28://join/ROOM. */
    private fun roomFrom(intent: Intent?): String? {
        val data = intent?.data ?: return null
        val room = data.getQueryParameter("join") ?: data.lastPathSegment
        return room?.takeIf { it.isNotBlank() }
    }

    private fun composeView(content: @Composable () -> Unit) = ComposeView(this).apply {
        setViewTreeLifecycleOwner(this@GameActivity)
        setViewTreeSavedStateRegistryOwner(this@GameActivity)
        setViewCompositionStrategy(ViewCompositionStrategy.DisposeOnViewTreeLifecycleDestroyed)
        setContent(content)
    }

    private fun shareInvite(room: String) {
        val link = webGameLink(room)
        val send = Intent(Intent.ACTION_SEND).apply {
            type = "text/plain"
            putExtra(Intent.EXTRA_TEXT, "Join my 28 table: $link")
        }
        startActivity(Intent.createChooser(send, "Invite to the table"))
    }
}
