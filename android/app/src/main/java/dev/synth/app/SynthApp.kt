package dev.synth.app

import android.app.Application

/** The match outlives the activity: a game shouldn't end because Android rebuilt a screen. */
class SynthApp : Application() {
    val controller: GameController by lazy { GameController(this) }
}
