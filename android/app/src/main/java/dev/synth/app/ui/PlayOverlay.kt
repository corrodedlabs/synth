package dev.synth.app.ui

import androidx.compose.animation.core.Animatable
import androidx.compose.animation.core.FastOutSlowInEasing
import androidx.compose.animation.core.LinearEasing
import androidx.compose.animation.core.tween
import androidx.compose.foundation.Canvas
import androidx.compose.foundation.background
import androidx.compose.foundation.border
import androidx.compose.foundation.clickable
import androidx.compose.foundation.interaction.MutableInteractionSource
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.BoxScope
import androidx.compose.foundation.layout.BoxWithConstraints
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.ExperimentalLayoutApi
import androidx.compose.foundation.layout.FlowRow
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.RowScope
import androidx.compose.foundation.layout.Spacer
import androidx.compose.foundation.layout.WindowInsets
import androidx.compose.foundation.layout.absoluteOffset
import androidx.compose.foundation.layout.asPaddingValues
import androidx.compose.foundation.layout.captionBar
import androidx.compose.foundation.layout.displayCutout
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.heightIn
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.layout.size
import androidx.compose.foundation.layout.width
import androidx.compose.foundation.layout.widthIn
import androidx.compose.foundation.layout.windowInsetsPadding
import androidx.compose.foundation.rememberScrollState
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.foundation.text.BasicText
import androidx.compose.foundation.verticalScroll
import androidx.compose.runtime.Composable
import androidx.compose.runtime.LaunchedEffect
import androidx.compose.runtime.getValue
import androidx.compose.runtime.key
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.remember
import androidx.compose.runtime.setValue
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.alpha
import androidx.compose.ui.layout.onGloballyPositioned
import androidx.compose.ui.layout.positionInRoot
import androidx.compose.ui.platform.LocalDensity
import androidx.compose.ui.geometry.Offset
import androidx.compose.ui.draw.drawBehind
import androidx.compose.ui.graphics.Brush
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.StrokeCap
import androidx.compose.ui.graphics.StrokeJoin
import androidx.compose.ui.graphics.drawscope.Stroke
import androidx.compose.ui.graphics.drawscope.scale
import androidx.compose.ui.graphics.vector.PathParser
import androidx.compose.ui.graphics.graphicsLayer
import androidx.compose.ui.platform.testTag
import androidx.compose.ui.semantics.contentDescription
import androidx.compose.ui.semantics.semantics
import androidx.compose.ui.semantics.selected
import androidx.compose.ui.text.TextStyle
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.unit.Dp
import androidx.compose.ui.unit.LayoutDirection
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import dev.synth.app.GameController
import dev.synth.app.TrumpReveal
import dev.synth.core.AbandonChoice
import dev.synth.core.ArrangementPreset
import dev.synth.core.GameModel
import dev.synth.core.GamePhase
import dev.synth.core.MatchSide
import dev.synth.core.PendingRequest
import dev.synth.core.PlayerProfile
import dev.synth.core.SUITS
import dev.synth.core.Suit
import dev.synth.core.playerProfile
import kotlinx.coroutines.delay
import kotlinx.coroutines.launch

// ids must match the server's whitelist (UiOverlay.ts EMOTES)
private val EMOTES = listOf(
    "nice" to "👍",
    "ouch" to "😖",
    "wow" to "😮",
    "think" to "🤔",
    "laugh" to "😂",
    "fire" to "🔥",
)
private const val MAX_BID = 28
private const val LEAVE_ARM_MS = 4_000L
private const val RESULT_HOLD_MS = 700L
private val SHORT_LANDSCAPE = 550.dp  // seatSpots' and the table's short landscape: a phone on its side
private val SHORT_PORTRAIT = 700.dp   // below this, the prompts run out of room between the side seats and the fan
private val SHORT_RESULT = 420.dp     // below this, a phone on its side, the result panels tighten up to fit
private val TINY_PORTRAIT = 600.dp    // below this, the bids have to get flatter still to leave the side seats showing
private val BID_MIN_WIDTH = 36.dp     // the narrowest a bid button gets
private val BID_ONE_ROW = 760.dp      // a landscape screen this wide has room for every bid on one row
private const val LABEL_CELLS = 2     // how many bids' width the label takes, when it shares a row with them
private const val SIDEWAYS_FAN_TOP = 0.74f // a phone on its side: where the fan starts, as a fraction of the height
private val HANDLE_CLEARANCE = 16.dp  // below a tablet's window handle
private val PORTRAIT_RING = 66.dp
private val LANDSCAPE_RING = 52.dp

// A text presentation selector keeps the suits in ink rather than emoji colours.
private val Suit.glyph: String
    get() = when (this) {
        Suit.HEARTS -> "♥︎"
        Suit.DIAMONDS -> "♦︎"
        Suit.CLUBS -> "♣︎"
        Suit.SPADES -> "♠︎"
    }
private val Suit.ink: Color get() = if (this == Suit.HEARTS || this == Suit.DIAMONDS) Red else Ink

/** Where the table draws each seat, in dp. A port of table_layout's seat points in native/src/table.c. */
private data class Spot(val x: Float, val y: Float)

private fun seatSpots(width: Float, height: Float): List<Spot> {
    val shortLandscape = height <= 550 && width > height
    val compact = width <= 700
    return when {
        shortLandscape -> {
            val marker = if (compact) 60f else 64f
            val sideX = marker / 2 + 18
            val topY = (height * 0.2f).coerceIn(72f, 88f)
            val sideY = height * 0.52f
            listOf(Spot(width / 2, height * 0.56f), Spot(width - sideX, sideY), Spot(width / 2, topY), Spot(sideX, sideY))
        }
        compact -> {
            val sideX = 66f / 2 + 19
            val topY = (height * 0.16f).coerceIn(118f, 150f)
            val sideY = (height * 0.43f).coerceIn(topY + 155, maxOf(topY + 155, height * 0.48f))
            val bottomY = sideY + (height * 0.11f).coerceIn(75f, 102f)
            listOf(Spot(width / 2, bottomY), Spot(width - sideX, sideY), Spot(width / 2, topY), Spot(sideX, sideY))
        }
        else -> {
            val sideX = (width * 0.12f).coerceIn(100f, 220f)
            val topY = (height * 0.14f).coerceIn(112f, 138f)
            val topCardY = topY + 168
            val sideY = if (height < 700) topCardY + 58 else maxOf(height * 0.46f, topCardY + 70)
            val bottomY = if (height < 700) sideY + 36 else sideY + (height * 0.11f).coerceIn(68f, 105f)
            listOf(Spot(width / 2, bottomY), Spot(width - sideX, sideY), Spot(width / 2, topY), Spot(sideX, sideY))
        }
    }
}

/**
 * How far up the screen the top of the fan reaches in portrait, as a share of its height, with a little clearance.
 * The table's camera comes closer as the screen gets squarer, and the cards don't quite give all of that back: this
 * line is measured from the table at aspects up to 1. A table wider than seatSpots' compact one holds its hand lower.
 */
private fun fanTopFromBottom(width: Dp, height: Dp): Float =
    if (width > 700.dp) 0.31f else 0.3f + 0.06f * maxOf(0f, width / height - 0.55f)

/** On a phone on its side, the prompts start below the partner's name. The height is seatSpots' top seat. */
private fun sidewaysPromptTop(height: Dp): Dp = (height * 0.2f).coerceIn(72.dp, 88.dp) + 50.dp

private enum class Popover { NONE, MENU, SORT, EMOTE }

/**
 * Over the live table, in three bands: scores at the top, the table in the middle, and the turn and the
 * hand's tools at the bottom. Sound, sorting and reactions stay folded away until asked for. Nothing here
 * fills the screen with anything that takes touches, except the catcher behind an open popover: the boxes
 * are layout only, and each real control or panel takes its own.
 */
@Composable
internal fun PlayOverlay(controller: GameController) {
    BoxWithConstraints(Modifier.fillMaxSize()) {
        val width = maxWidth
        val height = maxHeight
        val landscape = width > height
        val model = controller.uiModel
        val pending = model.pendingRequest
        val spots = seatSpots(width.value, height.value)
        val showingResult = model.phase == GamePhase.HAND_FINISHED || model.phase == GamePhase.FINISHED
        var popover by remember { mutableStateOf(Popover.NONE) }
        val toggle = { which: Popover -> popover = if (popover == which) Popover.NONE else which }
        var flyingTrump by remember { mutableStateOf<Suit?>(null) }
        val thump = rememberThump()
        var thumpsSeen by remember { mutableStateOf(controller.thumps) }
        LaunchedEffect(controller.thumps) {
            if (controller.thumps != thumpsSeen) {
                thumpsSeen = controller.thumps
                thump()
            }
        }

        // Only the cutout: the app is immersive, and the default theme's hidden action bar reports hidden
        // bars as 158px/68px through the safe-drawing insets, which would push the HUD into the seats.
        val inset = WindowInsets.displayCutout.asPaddingValues()
        val insetLeft = inset.calculateLeftPadding(LayoutDirection.Ltr)
        val insetRight = inset.calculateRightPadding(LayoutDirection.Ltr)
        val safe = Modifier.fillMaxSize().windowInsetsPadding(WindowInsets.displayCutout)

        // A result card covers the middle of the table, so the names would only peek out around it.
        if (!showingResult) {
            for (seat in 1..3) {
                SeatMarker(controller, seat, spots[seat], landscape, width, insetLeft, insetRight)
            }
        }
        // A tablet draws its window handle at the top centre, over the score. The score only has to clear
        // the handle itself: dropping it the caption bar's full height would push it into the partner's seat.
        val handle = if (WindowInsets.captionBar.asPaddingValues().calculateTopPadding() > 0.dp) HANDLE_CLEARANCE else 0.dp
        Box(safe) {
            TopBar(model, landscape, Modifier.align(Alignment.TopCenter).padding(top = handle), onMenu = { toggle(Popover.MENU) })
            BottomBar(controller, landscape, Modifier.align(Alignment.BottomCenter), onSort = { toggle(Popover.SORT) }, onEmote = { toggle(Popover.EMOTE) })
        }

        // In portrait the prompts sit just above the fan, where the player is already looking. A landscape
        // fan leaves no room there. A phone on its side has none between the seats either, so they hang from
        // the top, below the partner's name. A tablet's taller table has its middle free: they sit there,
        // level with the side seats, as they do on the web.
        val roomy = landscape && height > SHORT_LANDSCAPE
        val promptTop = when {
            roomy -> spots[1].y.dp - 60.dp
            landscape -> sidewaysPromptTop(height)
            else -> null
        }
        val promptPlace = if (promptTop != null) {
            Modifier.align(Alignment.TopCenter).padding(top = promptTop)
        } else {
            Modifier.align(Alignment.BottomCenter).padding(bottom = height * fanTopFromBottom(width, height))
        }
        if (controller.canExpose) {
            PaperButton(
                "Reveal trump",
                { controller.exposeTrump() },
                // In landscape the prompts' place is over the trick, so this one waits beside the fan. On a phone
                // on its side the fan reaches that far, so it goes above the reactions, clear of the last card.
                (when {
                    roomy -> Modifier.align(Alignment.BottomEnd).padding(end = 72.dp + insetRight, bottom = 14.dp)
                    landscape -> Modifier.align(Alignment.BottomEnd).padding(end = 12.dp + insetRight, bottom = 60.dp)
                    else -> promptPlace
                }).testTag("expose-button"),
                fill = PaperLight.copy(alpha = 0.92f),
                spacing = 0.6f,
                horizontal = 22.dp,
            )
        }
        when (pending) {
            is PendingRequest.Bid -> BidPanel(
                minBid = pending.minBid,
                opening = model.bidsPlaced == 0,
                landscape = landscape,
                rowHeight = when {
                    landscape -> if (width < BID_ONE_ROW) 40.dp else 42.dp
                    height < TINY_PORTRAIT -> 34.dp
                    height < SHORT_PORTRAIT -> 40.dp
                    else -> 42.dp
                },
                modifier = promptPlace,
                maxWidth = width,
                onBid = controller::bid,
                onPass = controller::pass,
            )
            PendingRequest.Trump -> if (flyingTrump == null) {
                TrumpPanel(promptPlace, pickScale(width, height, landscape), onSuit = {
                    flyingTrump = it
                    controller.chooseTrump(it)
                })
            }
            else -> Unit
        }

        flyingTrump?.let { suit ->
            FlyingTrump(suit, pickScale(width, height, landscape), width, height, promptTop, onDone = {
                flyingTrump = null
                controller.thump()
            })
        }
        controller.trumpReveal?.let { TrumpRevealOverlay(it, width, height) }
        if (model.phase == GamePhase.HAND_FINISHED) HandResultPanel(controller, width, height)
        if (model.phase == GamePhase.FINISHED) MatchOverPanel(controller, width, height)

        // Announcements never take touches. They keep to whichever part of the table a prompt leaves free.
        controller.moment?.let { moment ->
            val top = when {
                // On a tablet the gap between the partner's card and yours, level with the side seats, is free.
                roomy && model.phase == GamePhase.PLAYING -> spots[1].y.dp - 20.dp
                landscape -> height * 0.24f
                pending != null || model.phase == GamePhase.BIDDING || model.phase == GamePhase.CHOOSING_TRUMP -> height * 0.30f
                else -> height * 0.62f
            }
            MomentBanner(moment.title, moment.detail, Modifier.align(Alignment.TopCenter).padding(top = top))
        }
        if (model.phase == GamePhase.BIDDING || model.phase == GamePhase.CHOOSING_TRUMP) {
            controller.bidNotes.forEach { note ->
                // Our own note would land on the prompt, which already says where the bid stands.
                if (note.seat != 0 || pending == null) {
                    key(note.seat, note.text) {
                        BidNoteAt(note.text, spots[note.seat], note.seat, landscape, width, height, insetLeft, insetRight, onLand = controller::thump)
                    }
                }
            }
        }
        controller.emotes.forEach { note ->
            key(note.key) { EmoteBubble(note.glyph, spots[note.seat], note.seat) }
        }

        controller.abandonedName?.let { name ->
            AbandonPanel(name, controller, Modifier.align(Alignment.TopCenter).padding(top = height * 0.06f).width(minOf(width * 0.86f, 420.dp)))
        }

        if (popover != Popover.NONE) {
            val source = remember { MutableInteractionSource() }
            Box(Modifier.fillMaxSize().clickable(source, indication = null) { popover = Popover.NONE })
            Box(safe) {
                val close = { popover = Popover.NONE }
                val belowTop = Modifier.align(Alignment.TopStart).padding(start = 12.dp, top = 58.dp)
                val aboveBottom = Modifier.padding(start = 12.dp, end = 12.dp, bottom = 66.dp)
                when (popover) {
                    Popover.MENU -> MenuPanel(controller, belowTop)
                    Popover.SORT -> Popped(aboveBottom.align(Alignment.BottomStart)) { ArrangeTiles(controller, close) }
                    Popover.EMOTE -> Popped(aboveBottom.align(Alignment.BottomEnd)) { EmoteStrip(controller, close) }
                    Popover.NONE -> Unit
                }
            }
        }
    }
}

// --- the HUD ---

@Composable
private fun TopBar(model: GameModel, landscape: Boolean, modifier: Modifier, onMenu: () -> Unit) {
    Row(modifier.fillMaxWidth().padding(horizontal = 12.dp, vertical = 6.dp), verticalAlignment = Alignment.Top) {
        if (landscape) {
            // A landscape table seats the partner high up in the top centre, so the score moves over beside the menu.
            RoundButton(onMenu, "Menu", "menu") { MenuIcon() }
            Spacer(Modifier.size(10.dp))
            ScoreBoard(model)
            Spacer(Modifier.weight(1f))
            Contract(model)
        } else {
            Box(Modifier.weight(1f), contentAlignment = Alignment.CenterStart) {
                RoundButton(onMenu, "Menu", "menu") { MenuIcon() }
            }
            ScoreBoard(model)
            Box(Modifier.weight(1f), contentAlignment = Alignment.CenterEnd) { Contract(model) }
        }
    }
}

/** Match pips for each side, then this hand's points as a stroke of ink that has the number to reach marked on it. */
@Composable
private fun ScoreBoard(model: GameModel) {
    val ours = model.bidWinner == 0 || model.bidWinner == 2
    val bid = model.finalBid
    // The bidders need their bid. The others need enough that the bidders can't reach it.
    val goal = bid?.let { if (ours) it else 29 - it }
    val words = when {
        goal == null -> "${model.score} – ${model.theirScore} this hand"
        ours && model.score >= goal -> "bid made with ${model.score}"
        !ours && model.score >= goal -> "bid set with ${model.score}"
        ours -> "${model.score} of $goal to make your bid"
        else -> "${model.score} of $goal to set them"
    }
    // On screen it's only numbers, with the stroke to read them against.
    val short = if (goal == null) "${model.score} – ${model.theirScore}" else "${model.score} / $goal"
    Column(
        Modifier.semantics(mergeDescendants = true) {
            contentDescription = "Match ${model.matchUs} to ${model.matchThem}, first to ${model.matchTarget}. $words"
        },
        horizontalAlignment = Alignment.CenterHorizontally,
        verticalArrangement = Arrangement.spacedBy(3.dp),
    ) {
        Column(horizontalAlignment = Alignment.End, verticalArrangement = Arrangement.spacedBy(3.dp)) {
            Pips("You", model.matchUs, model.matchTarget, Red)
            Pips("Them", model.matchThem, model.matchTarget, Ink)
        }
        Row(verticalAlignment = Alignment.CenterVertically, horizontalArrangement = Arrangement.spacedBy(7.dp)) {
            BrushLine(model.score, goal, Modifier.size(width = 104.dp, height = 11.dp))
            Txt(short, Modifier.testTag("hand-score"), size = 15, weight = FontWeight.W600, spacing = 0f, maxLines = 1)
        }
    }
}

/** The bid as a small seal over the trump: a face-down card while it's hidden, and the suit once it's out. */
@Composable
private fun Contract(model: GameModel) {
    val bid = model.finalBid ?: return
    val suit = model.trumpSuit
    val whose = when (val winner = model.bidWinner) {
        null -> "bid"
        0 -> "your bid"
        else -> (model.seatNames?.getOrNull(winner) ?: "their") + "'s bid"
    }
    val trumpWords = when {
        suit == null -> "Trump hidden"
        model.trumpExposed -> "Trump ${suit.id}"
        else -> "Trump ${suit.id}, still hidden"
    }
    Column(
        Modifier.testTag("contract").semantics(mergeDescendants = true) { contentDescription = "Bid $bid, $whose. $trumpWords" },
        horizontalAlignment = Alignment.CenterHorizontally,
        verticalArrangement = Arrangement.spacedBy(4.dp),
    ) {
        Seal("$bid", size = 17, horizontal = 7.dp, vertical = 3.dp, thump = false)
        Box(contentAlignment = Alignment.Center) {
            SuitCard(suit ?: Suit.SPADES, if (suit != null && model.trumpExposed) 0f else 1f, Modifier.size(width = 26.dp, height = 38.dp), glyphSize = 17)
            // A hidden trump we chose: the back says it's hidden, and the suit on it reminds us what it is.
            if (suit != null && !model.trumpExposed) {
                Box(Modifier.size(18.dp).background(PaperLight.copy(alpha = 0.9f), RoundedCornerShape(50)), contentAlignment = Alignment.Center) {
                    SuitGlyph(suit, 13)
                }
            }
        }
    }
}

private fun turnWord(phase: GamePhase): String = when (phase) {
    GamePhase.BIDDING -> "bidding"
    GamePhase.CHOOSING_TRUMP -> "choosing trump"
    else -> "playing"
}

/** The tools for the hand either side of whose turn it is. The result cards speak for themselves, so the turn is quiet then. */
@Composable
private fun BottomBar(controller: GameController, landscape: Boolean, modifier: Modifier, onSort: () -> Unit, onEmote: () -> Unit) {
    val model = controller.uiModel
    val pending = model.pendingRequest
    val yourTurn = pending is PendingRequest.Play
    val turn = model.activePlayer
    val status = when {
        yourTurn -> "Your turn"
        turn != null && turn != 0 && pending == null && model.phase != GamePhase.HAND_FINISHED && model.phase != GamePhase.FINISHED ->
            (model.seatNames?.getOrNull(turn) ?: "They") + " is " + turnWord(model.phase)
        pending != null -> ""
        controller.statusText == "Your turn" -> ""
        controller.statusText == "Dealing…" && model.hand.isNotEmpty() -> ""
        else -> controller.statusText
    }
    Row(modifier.fillMaxWidth().padding(start = 12.dp, end = 12.dp, bottom = 14.dp), verticalAlignment = Alignment.CenterVertically) {
        Box(Modifier.size(44.dp)) {
            if (controller.canArrange) {
                RoundButton(onSort, "Sort your hand", "sort") { SortIcon() }
            }
        }
        // In landscape the fan comes down to here, so the words keep beside it.
        Box(Modifier.weight(1f).padding(horizontal = 8.dp), contentAlignment = if (landscape) Alignment.CenterStart else Alignment.Center) {
            if (status.isNotEmpty()) {
                Txt(
                    status,
                    Modifier.testTag("status").widthIn(max = if (landscape) 150.dp else 280.dp),
                    size = if (yourTurn) 20 else 15,
                    weight = if (yourTurn) FontWeight.W600 else FontWeight.W400,
                    color = if (yourTurn) Green else Ink,
                    opacity = if (yourTurn) 1f else 0.75f,
                    maxLines = 2,
                )
            }
        }
        Box(Modifier.size(44.dp)) {
            if (model.phase != GamePhase.FINISHED) {
                RoundButton(onEmote, "Send a reaction", "emote") { BasicText("🙂", style = TextStyle(fontSize = 20.sp, color = Color.Black)) }
            }
        }
    }
}

@Composable
private fun RoundButton(onClick: () -> Unit, description: String, tag: String, content: @Composable BoxScope.() -> Unit) {
    Pressable(
        onClick,
        Modifier.size(44.dp).testTag(tag).semantics { contentDescription = description },
        shape = RoundedCornerShape(999.dp),
        fill = PaperLight.copy(alpha = 0.72f),
        border = Ink.copy(alpha = 0.3f),
        content = content,
    )
}

/** Three bars. Drawn rather than typed: a font's symbols sit off centre in a round button. */
@Composable
private fun MenuIcon() {
    Canvas(Modifier.size(18.dp)) {
        val line = 1.6.dp.toPx()
        val ink = Ink.copy(alpha = 0.8f)
        for (y in listOf(0.22f, 0.5f, 0.78f)) {
            drawLine(ink, Offset(size.width * 0.08f, size.height * y), Offset(size.width * 0.92f, size.height * y), line, StrokeCap.Round)
        }
    }
}

/** An arrow up beside an arrow down, balanced about the button's centre. */
@Composable
private fun SortIcon() {
    Canvas(Modifier.size(20.dp)) {
        val line = 1.6.dp.toPx()
        val ink = Ink.copy(alpha = 0.8f)
        val w = size.width
        val h = size.height
        val head = w * 0.16f
        fun arrow(x: Float, tip: Float, tail: Float) {
            val back = if (tip < tail) head else -head
            drawLine(ink, Offset(x, tip), Offset(x, tail), line, StrokeCap.Round)
            drawLine(ink, Offset(x, tip), Offset(x - head, tip + back), line, StrokeCap.Round)
            drawLine(ink, Offset(x, tip), Offset(x + head, tip + back), line, StrokeCap.Round)
        }
        arrow(w * 0.3f, h * 0.12f, h * 0.88f)
        arrow(w * 0.7f, h * 0.88f, h * 0.12f)
    }
}

/** A folded-away row of tools, opened from one of the bars. */
@Composable
private fun Popped(modifier: Modifier, content: @Composable () -> Unit) {
    Panel(modifier, fill = PaperLight, padding = 6.dp, entering = true) { content() }
}

@Composable
private fun MenuPanel(controller: GameController, modifier: Modifier) {
    val model = controller.uiModel
    Panel(modifier.width(210.dp), fill = PaperLight, padding = 10.dp, entering = true) {
        Row(Modifier.fillMaxWidth().padding(horizontal = 4.dp, vertical = 6.dp), horizontalArrangement = Arrangement.SpaceBetween) {
            Txt("Tricks", size = 16, opacity = 0.72f, maxLines = 1)
            Txt("${model.tricks} – ${model.theirTricks}", size = 16, weight = FontWeight.W600, spacing = 0f, maxLines = 1)
        }
        Spacer(Modifier.size(4.dp))
        Pressable(
            { controller.toggleMute() },
            Modifier.fillMaxWidth().heightIn(min = 44.dp).testTag("mute"),
            border = Ink.copy(alpha = 0.3f),
        ) {
            Row(verticalAlignment = Alignment.CenterVertically, horizontalArrangement = Arrangement.spacedBy(6.dp)) {
                SoundIcon(controller.muted)
                Txt(if (controller.muted) "Sound off" else "Sound on", size = 16, maxLines = 1)
            }
        }
        Spacer(Modifier.size(6.dp))
        Pressable(
            { controller.toggleReduceMotion() },
            Modifier.fillMaxWidth().heightIn(min = 44.dp).testTag("reduce-motion"),
            border = Ink.copy(alpha = 0.3f),
        ) {
            Txt(if (controller.reduceMotion) "Reduced motion: on" else "Reduced motion: off", size = 16, maxLines = 1)
        }
        if (model.phase != GamePhase.FINISHED) {
            Spacer(Modifier.size(6.dp))
            LeaveMatchChip(controller)
        }
    }
}

/**
 * `.seat-name-label`: the ensō with the player's face in it and their name below. Drawn here rather than by
 * the table so it keeps clear of a cutout and of the result cards. The ring breathes on their turn and the
 * face pops when they make a call.
 */
@Composable
private fun SeatMarker(controller: GameController, seat: Int, spot: Spot, landscape: Boolean, screenWidth: Dp, insetLeft: Dp, insetRight: Dp) {
    val model = controller.uiModel
    val member = model.seatMembers?.getOrNull(seat) ?: return
    val profile = playerProfile(member, model.members)
    val active = model.activePlayer == seat
    val ring = if (landscape) LANDSCAPE_RING else PORTRAIT_RING
    val cx = seatCentre(spot, ring, screenWidth, insetLeft, insetRight)
    val top = spot.y.dp - ring / 2

    Box(
        Modifier
            .absoluteOffset(cx - ring / 2, top)
            .size(ring)
            .testTag("seat-marker-$seat")
            .drawBehind {
                if (active) {
                    // Whose turn should read at a glance: a wash of red behind the ring.
                    drawCircle(
                        Brush.radialGradient(0.6f to Red.copy(alpha = 0.2f), 1f to Red.copy(alpha = 0f), radius = size.minDimension * 0.66f),
                        radius = size.minDimension * 0.66f,
                    )
                }
            },
        contentAlignment = Alignment.Center,
    ) {
        BrushedEnso(active)
        SeatFace(profile, controller.bidNotes.firstOrNull { it.seat == seat }?.text, controller.trickWin?.takeIf { it.seat == seat }?.key, landscape)
    }
    // A landscape table has no line to spare between the partner and the prompts.
    val partnerInline = landscape && seat == 2
    val nameWidth = if (partnerInline) 150.dp else if (landscape) 72.dp else 96.dp
    val nameX = cx.coerceIn(insetLeft + nameWidth / 2, screenWidth - insetRight - nameWidth / 2) - nameWidth / 2
    Txt(
        if (partnerInline) "${profile.displayName} · partner" else profile.displayName,
        Modifier.absoluteOffset(nameX, top + ring + 4.dp).width(nameWidth),
        size = 15,
        weight = if (active) FontWeight.W600 else FontWeight.W400,
        color = if (active) Red else Ink,
        opacity = if (active) 1f else 0.78f,
        maxLines = 1,
    )
    if (active && model.phase != GamePhase.HAND_FINISHED && controller.bidNotes.none { it.seat == seat }) {
        // A call takes over from this when it lands, beside the ring.
        val below = if (landscape) 21.dp else if (seat == 2) 40.dp else 23.dp
        val x = if (landscape && seat == 2) cx + ring / 2 + 4.dp else nameX
        Txt(
            turnWord(model.phase) + "…",
            Modifier.absoluteOffset(x, if (landscape && seat == 2) top + 6.dp else top + ring + below).width(nameWidth),
            size = 15,
            weight = FontWeight.W600,
            color = Red,
            maxLines = 1,
        )
    }
    if (seat == 2 && !landscape) {
        Txt(
            "partner",
            Modifier.absoluteOffset(nameX, top + ring + 23.dp).width(nameWidth),
            size = 12,
            opacity = 0.55f,
            maxLines = 1,
        )
    }
}

private fun seatCentre(spot: Spot, ring: Dp, screenWidth: Dp, insetLeft: Dp, insetRight: Dp): Dp =
    spot.x.dp.coerceIn(insetLeft + ring / 2 + 2.dp, screenWidth - insetRight - ring / 2 - 2.dp)

@Composable
private fun SeatFace(profile: PlayerProfile, call: String?, won: Long?, landscape: Boolean) {
    // `seat-avatar-speak`
    val scale = remember { Animatable(1f) }
    val turn = remember { Animatable(0f) }
    val reduce = LocalReduceMotion.current
    LaunchedEffect(call) {
        if (call != null && !reduce) {
            scale.snapTo(0.9f)
            scale.animateTo(1.09f, tween(286))
            scale.animateTo(1f, tween(234))
        }
    }
    // Taking a trick gets a bigger, happier bounce.
    LaunchedEffect(won) {
        if (won != null && !reduce) {
            launch { turn.animateTo(-9f, tween(170)); turn.animateTo(0f, tween(300)) }
            scale.animateTo(1.45f, tween(170))
            scale.animateTo(1f, tween(300))
        }
    }
    val modifier = Modifier.graphicsLayer {
        scaleX = scale.value
        scaleY = scale.value
        rotationZ = turn.value
    }
    if (profile.isBot) {
        BasicText(profile.avatar, modifier, style = TextStyle(fontSize = (if (landscape) 19 else 23).sp, color = Color.Black))
    } else {
        Txt(profile.avatar, modifier, size = if (landscape) 18 else 21, weight = FontWeight.W600, color = accentColor(profile.accent), spacing = 0f)
    }
}

@Composable
internal fun SuitGlyph(suit: Suit, size: Int) {
    BasicText(suit.glyph, style = TextStyle(color = suit.ink, fontSize = size.sp))
}

private val SPEAKER = PathParser().parsePathString("M4 9.4v5.2h3.1l3.9 3.4V6L7.1 9.4H4Z").toPath()
private val WAVES = PathParser().parsePathString("M14.6 9.6a3.4 3.4 0 0 1 0 4.8M16.9 7.4a6.5 6.5 0 0 1 0 9.2").toPath()
private val CROSS = PathParser().parsePathString("m14.6 9.8 5 5m0-5-5 5").toPath()

/** The web's sound-on and sound-off icons. */
@Composable
private fun SoundIcon(muted: Boolean) {
    Canvas(Modifier.size(20.dp)) {
        val unit = size.width / 24f
        val line = Stroke(width = 1.5f, cap = StrokeCap.Round, join = StrokeJoin.Round)
        scale(unit, pivot = Offset.Zero) {
            drawPath(SPEAKER, Ink.copy(alpha = 0.85f), style = line)
            drawPath(if (muted) CROSS else WAVES, Ink.copy(alpha = 0.85f), style = line)
        }
    }
}

/** Leaving abandons the match for all four seats: the first tap only arms the button. */
@Composable
private fun LeaveMatchChip(controller: GameController) {
    var armed by remember { mutableStateOf(false) }
    LaunchedEffect(armed) {
        if (armed) {
            delay(LEAVE_ARM_MS)
            armed = false
        }
    }
    if (armed) {
        Chip("abandon the match?", { armed = false; controller.leaveMatch() }, Modifier.testTag("leave-confirm"), color = Red, armed = true)
    } else {
        Chip("leave match", { armed = true }, Modifier.testTag("leave-match"))
    }
}

@Composable
private fun EmoteStrip(controller: GameController, close: () -> Unit) {
    Row {
        EMOTES.forEach { (id, glyph) ->
            Pressable(
                { controller.sendEmote(id); close() },
                Modifier.size(40.dp).testTag("emote-$id"),
                shape = RoundedCornerShape(50),
                border = Color.Transparent,
            ) {
                BasicText(glyph, style = TextStyle(fontSize = 21.sp, color = Color.Black))
            }
        }
    }
}

@Composable
private fun ArrangeTiles(controller: GameController, close: () -> Unit) {
    val trump = controller.uiModel.trumpSuit
    val selected = controller.arrangement
    val choose = { preset: ArrangementPreset -> controller.arrange(preset); close() }
    Row(horizontalArrangement = Arrangement.spacedBy(4.dp)) {
        ArrangeTile("♠\uFE0E♥\uFE0E", "suit", selected == ArrangementPreset.SUIT, true, "arrange-suit") { choose(ArrangementPreset.SUIT) }
        ArrangeTile("J→7", "strength", selected == ArrangementPreset.STRENGTH, true, "arrange-strength") { choose(ArrangementPreset.STRENGTH) }
        ArrangeTile(trump?.glyph ?: "✦", "trump", selected == ArrangementPreset.TRUMP, trump != null, "arrange-trump") { choose(ArrangementPreset.TRUMP) }
        ArrangeTile("↺", "deal", selected == ArrangementPreset.DEAL, true, "arrange-deal") { choose(ArrangementPreset.DEAL) }
    }
}

@Composable
private fun ArrangeTile(glyph: String, label: String, selected: Boolean, enabled: Boolean, tag: String, onClick: () -> Unit) {
    Pressable(
        onClick,
        Modifier.size(width = 56.dp, height = 46.dp).testTag(tag).semantics { this.selected = selected },
        enabled = enabled,
        shape = RoundedCornerShape(9.dp),
        fill = if (selected) Color(0xFFE7CEA9).copy(alpha = 0.8f) else PaperLight.copy(alpha = 0.72f),
        border = Color(0xFF5B392A).copy(alpha = if (selected) 0.52f else 0.25f),
    ) {
        Column(horizontalAlignment = Alignment.CenterHorizontally) {
            BasicText(glyph, style = TextStyle(color = Color(0xFF724734), fontSize = 14.sp, fontWeight = FontWeight.W600))
            Txt(label, size = 11, opacity = 0.75f, spacing = 0f, maxLines = 1)
        }
    }
}

// --- bid and trump ---

@OptIn(ExperimentalLayoutApi::class)
@Composable
private fun BidPanel(
    minBid: Int,
    opening: Boolean,
    landscape: Boolean,
    rowHeight: Dp,
    modifier: Modifier,
    maxWidth: Dp,
    onBid: (Int) -> Unit,
    onPass: () -> Unit,
) {
    val values = (minBid..MAX_BID).toList()
    val label = if (opening) "Your bid" else "Beat ${minBid - 1}"
    val short = rowHeight < 42.dp
    val padding = if (short) 5.dp else 8.dp
    val panelWidth = if (landscape) minOf(maxWidth - 240.dp, 660.dp) else minOf(maxWidth - 24.dp, 420.dp)
    // Every bid on one row when they fit at a size a thumb can hit, and two rows when they don't.
    val columns = if (landscape) minOf(13, ((panelWidth - padding * 2 + 4.dp) / (BID_MIN_WIDTH + 4.dp)).toInt()) else 7
    @Composable
    fun RowScope.cell(value: Int) {
        Pressable({ onBid(value) }, Modifier.weight(1f).heightIn(min = rowHeight).testTag("bid-$value")) {
            Txt("$value", size = 20, spacing = 0f, maxLines = 1)
        }
    }
    Panel(modifier.width(panelWidth), padding = padding, entering = true) {
        if (landscape && columns < 13) {
            // Too narrow for one row, and a phone on its side has no height to spare above the hand. So the label
            // and Pass share the rows with the bids, and the panel stays two rows tall.
            val first = values.take(columns - LABEL_CELLS)
            val rest = values.drop(first.size)
            val spare = columns - LABEL_CELLS - first.size
            val passBeside = !opening && rest.isEmpty() && spare > 0
            Row(Modifier.fillMaxWidth().padding(vertical = 1.dp), verticalAlignment = Alignment.CenterVertically, horizontalArrangement = Arrangement.spacedBy(4.dp)) {
                Box(Modifier.weight(LABEL_CELLS.toFloat()), contentAlignment = Alignment.Center) { Txt(label, size = 17, opacity = 0.8f, maxLines = 1) }
                first.forEach { cell(it) }
                if (passBeside) PassButton(onPass, Modifier.weight(spare.toFloat()))
                else if (spare > 0) Spacer(Modifier.weight(spare.toFloat()))
            }
            if (rest.isNotEmpty() || (!opening && !passBeside)) {
                Row(Modifier.fillMaxWidth().padding(vertical = 1.dp), verticalAlignment = Alignment.CenterVertically, horizontalArrangement = Arrangement.spacedBy(4.dp)) {
                    rest.forEach { cell(it) }
                    val left = columns - rest.size
                    if (!opening) PassButton(onPass, Modifier.weight(left.toFloat()))
                    else if (left > 0) Spacer(Modifier.weight(left.toFloat()))
                }
            }
            return@Panel
        }
        if (landscape) {
            Row(Modifier.fillMaxWidth(), verticalAlignment = Alignment.CenterVertically, horizontalArrangement = Arrangement.Center) {
                Txt(label, size = 18, opacity = 0.8f)
                if (!opening) {
                    Spacer(Modifier.width(16.dp))
                    PassButton(onPass, Modifier.widthIn(min = 120.dp))
                }
            }
        } else {
            Txt(label, size = if (short) 16 else 18, opacity = 0.8f, maxLines = 1)
        }
        Spacer(Modifier.size(if (short) 1.dp else 3.dp))
        val rows = values.chunked(columns)
        rows.forEachIndexed { index, row ->
            // Pass shares the last row when it has room, which keeps the panel low.
            val passHere = !landscape && !opening && index == rows.lastIndex && row.size < columns
            Row(Modifier.fillMaxWidth().padding(vertical = 1.dp), horizontalArrangement = Arrangement.spacedBy(4.dp)) {
                row.forEach { cell(it) }
                val spare = columns - row.size
                if (passHere) PassButton(onPass, Modifier.weight(spare.toFloat()))
                else repeat(spare) { Spacer(Modifier.weight(1f)) }
            }
        }
        if (!landscape && !opening && values.size % columns == 0) {
            Spacer(Modifier.size(4.dp))
            PassButton(onPass, Modifier.fillMaxWidth())
        }
    }
}

@Composable
private fun PassButton(onPass: () -> Unit, modifier: Modifier) {
    PaperButton("Pass", onPass, modifier.testTag("bid-pass"), size = 20, horizontal = 6.dp)
}

private val PICK_W = 72.dp
private val PICK_H = 106.dp
private val PICK_GAP = 6.dp

/** How much the trump cards shrink, so that four of them fit across the screen and stop below a side seat's name. */
private fun pickScale(width: Dp, height: Dp, landscape: Boolean): Float {
    val across = (width - 24.dp - PICK_GAP * 3) / (PICK_W * 4)
    val down = when {
        !landscape -> (height * (1 - fanTopFromBottom(width, height)) - 372.dp) / PICK_H
        height <= SHORT_LANDSCAPE -> (height * SIDEWAYS_FAN_TOP - sidewaysPromptTop(height) - 30.dp) / PICK_H
        else -> 1f
    }
    return minOf(1f, across, down.coerceAtLeast(0.72f))
}

/** Four suit cards to choose from, dealt up from the table. */
@Composable
private fun TrumpPanel(modifier: Modifier, scale: Float, onSuit: (Suit) -> Unit) {
    Column(modifier, horizontalAlignment = Alignment.CenterHorizontally) {
        Txt("Choose trump", size = 22, opacity = 0.85f)
        Spacer(Modifier.size(8.dp))
        Row(horizontalArrangement = Arrangement.spacedBy(PICK_GAP)) {
            SUITS.forEachIndexed { index, suit ->
                val turn = remember { Animatable(1f) }
                val reduce = LocalReduceMotion.current
                LaunchedEffect(Unit) {
                    if (reduce) {
                        turn.snapTo(0f)
                    } else {
                        delay(index * 70L)
                        turn.animateTo(0f, tween(340))
                    }
                }
                Pressable(
                    { onSuit(suit) },
                    Modifier.size(width = PICK_W * scale, height = PICK_H * scale).testTag("trump-${suit.id}"),
                    shape = RoundedCornerShape(6.dp),
                    border = Color.Transparent,
                ) {
                    SuitCard(suit, turn.value, Modifier.fillMaxSize(), glyphSize = (42 * scale).toInt())
                }
            }
        }
    }
}

/** The chosen suit turns face down and goes up to its place in the score, where nobody else can see it. */
@Composable
private fun BoxScope.FlyingTrump(suit: Suit, cardScale: Float, width: Dp, height: Dp, promptTop: Dp?, onDone: () -> Unit) {
    val reduce = LocalReduceMotion.current
    val turn = remember { Animatable(0f) }
    val fly = remember { Animatable(0f) }
    LaunchedEffect(Unit) {
        if (!reduce) {
            turn.animateTo(1f, tween(280))
            fly.animateTo(1f, tween(420, easing = FastOutSlowInEasing))
        }
        onDone()
    }
    val pickW = PICK_W * cardScale
    val pickH = PICK_H * cardScale
    val startX = width / 2 - pickW / 2
    // Where the chosen card sat in the prompt: below its title.
    val startY = promptTop?.plus(20.dp) ?: (height * (1 - fanTopFromBottom(width, height)) - pickH)
    val endX = width - 12.dp - pickW * 0.65f
    val endY = 2.dp
    val p = fly.value
    SuitCard(
        suit,
        turn.value,
        Modifier
            .absoluteOffset(startX + (endX - startX) * p, startY + (endY - startY) * p)
            .size(width = pickW, height = pickH)
            .graphicsLayer {
                val shrink = 1f - p * 0.64f
                scaleX = shrink
                scaleY = shrink
            },
        glyphSize = (42 * cardScale).toInt(),
    )
}

/** The table stops: the card flips large in the middle, with ink bursting out behind it. */
@Composable
private fun BoxScope.TrumpRevealOverlay(reveal: TrumpReveal, width: Dp, height: Dp) {
    val reduce = LocalReduceMotion.current
    val thump = rememberThump()
    val wash = remember(reveal) { Animatable(0f) }
    val turn = remember(reveal) { Animatable(if (reduce) 0f else 1f) }
    val scale = remember(reveal) { Animatable(if (reduce) 1f else 0.3f) }
    var burst by remember(reveal) { mutableStateOf(reduce) }
    LaunchedEffect(reveal) {
        launch { wash.animateTo(1f, tween(220)) }
        if (reduce) return@LaunchedEffect
        scale.animateTo(1.1f, tween(420, easing = FastOutSlowInEasing))
        delay(120)
        turn.animateTo(0f, tween(360))
        burst = true
        thump()
        scale.animateTo(1f, tween(160))
    }
    Box(Modifier.fillMaxSize().background(PaperLight.copy(alpha = 0.72f * wash.value)))
    if (burst) InkBurst(reveal, 0.5f, 0.45f, Ink.copy(alpha = 0.88f))
    val cardHeight = minOf(height * 0.42f, 222.dp)
    Column(Modifier.align(Alignment.Center), horizontalAlignment = Alignment.CenterHorizontally) {
        SuitCard(
            reveal.suit,
            turn.value,
            Modifier.size(width = cardHeight * 0.68f, height = cardHeight).graphicsLayer {
                scaleX = scale.value
                scaleY = scale.value
            },
            glyphSize = (cardHeight.value * 0.42f).toInt(),
        )
        Spacer(Modifier.size(10.dp))
        Txt(reveal.shownBy, size = 19, opacity = 0.85f * wash.value, maxLines = 1)
    }
}

// --- results ---

/** A quiet wash over the table, and the result on it. Taps it doesn't take still reach what's underneath. */
@Composable
private fun BoxScope.ResultStage(height: Dp, content: @Composable () -> Unit) {
    val wash = remember { Animatable(0f) }
    LaunchedEffect(Unit) { wash.animateTo(1f, tween(240)) }
    Box(Modifier.fillMaxSize().background(PaperLight.copy(alpha = 0.76f * wash.value)))
    Column(
        // The padding is inside the scroller, which would otherwise cut the corners off a tilted seal.
        Modifier.align(Alignment.Center).heightIn(max = height * 0.94f).verticalScroll(rememberScrollState()).padding(horizontal = 18.dp, vertical = if (height < SHORT_RESULT) 12.dp else 24.dp),
        horizontalAlignment = Alignment.CenterHorizontally,
    ) { content() }
}

/** Between hands: a huge seal, what happened, the points flying up to the score, and the host's deal button or a waiting note. */
@Composable
private fun BoxScope.HandResultPanel(controller: GameController, width: Dp, height: Dp) {
    val model = controller.uiModel
    val result = controller.handResult
    // The verdict follows HandFinished on the socket. If it decides the match, MatchOver replaces this panel.
    var held by remember(model.handNumber) { mutableStateOf(true) }
    LaunchedEffect(model.handNumber) {
        delay(RESULT_HOLD_MS)
        held = false
    }
    val decided = model.matchUs >= model.matchTarget || model.matchThem >= model.matchTarget
    if (decided || (result == null && held)) return

    var dealing by remember(model.handNumber) { mutableStateOf(false) }
    val swing = if (result != null) model.handResults.lastOrNull() ?: 0 else 0
    var stamped by remember(model.handNumber) { mutableStateOf(false) }
    val rise = remember(model.handNumber) { Animatable(0f) }
    var deltaY by remember { mutableStateOf(0f) }
    val reduce = LocalReduceMotion.current
    LaunchedEffect(stamped) {
        if (stamped && !reduce) {
            delay(1_100)
            rise.animateTo(1f, tween(520, easing = FastOutSlowInEasing))
        }
    }
    val thump = rememberThump()

    if (stamped) InkBurst(model.handNumber, 0.5f, 0.42f, Red.copy(alpha = 0.8f), drops = 44, spread = 170.dp)
    ResultStage(height) {
        if (result != null) {
            val ours = result.bidder == 0 || result.bidder == 2
            val bidder = if (result.bidder == 0) "You" else model.seatNames?.getOrNull(result.bidder) ?: "They"
            val took = if (ours) model.score else model.theirScore
            Seal(
                if (result.made) "MADE" else "SET",
                Modifier.testTag("result-seal"),
                size = 56,
                horizontal = 24.dp,
                vertical = 10.dp,
                onLand = {
                    stamped = true
                    thump()
                },
            )
            Spacer(Modifier.size(16.dp))
            Txt("$bidder bid ${result.bid} and took $took", size = 23)
            Spacer(Modifier.size(10.dp))
            // The points go to the bidding side, so a made bid by them is +1 to them, and a set one takes points from them.
            val size = kotlin.math.abs(swing)
            val delta = when {
                size == 0 -> "no change"
                else -> (if (result.made) "+$size" else "−$size") + if (ours) "" else " to them"
            }
            val density = LocalDensity.current.density
            Txt(
                delta,
                Modifier
                    .testTag("result-delta")
                    .onGloballyPositioned { deltaY = it.positionInRoot().y }
                    .graphicsLayer {
                        val p = rise.value
                        translationY = -(deltaY - 30f * density).coerceAtLeast(0f) * p
                        scaleX = 1f - p * 0.6f
                        scaleY = 1f - p * 0.6f
                        alpha = 1f - p
                    },
                size = 36,
                weight = FontWeight.W600,
                color = if (ours) Red else Ink,
            )
        } else {
            Txt("${model.score} – ${model.theirScore}", size = 36, weight = FontWeight.W500)
        }
        Spacer(Modifier.size(16.dp))
        when {
            dealing -> WaitingNote("dealing…")
            controller.isActingHost -> PaperButton(
                "Deal the next hand",
                { dealing = true; controller.nextHand() },
                Modifier.testTag("next-hand"),
                fill = PaperLight,
                size = 20,
            )
            else -> WaitingNote("waiting for the host to deal…")
        }
    }
}

@Composable
private fun WaitingNote(text: String) {
    Txt(text, Modifier.testTag("waiting-next"), size = 18, opacity = 0.6f, italic = true)
}

@OptIn(ExperimentalLayoutApi::class)
@Composable
private fun BoxScope.MatchOverPanel(controller: GameController, width: Dp, height: Dp) {
    val model = controller.uiModel
    val won = model.matchWinner == MatchSide.US
    val hands = controller.matchHands ?: model.handNumber
    if (won) {
        InkBurst("win-red", 0.5f, 0.38f, Red.copy(alpha = 0.85f), drops = 110, spread = 340.dp)
        InkBurst("win-left", 0.3f, 0.58f, Ink.copy(alpha = 0.75f), drops = 50, spread = 210.dp)
        InkBurst("win-right", 0.7f, 0.25f, Ink.copy(alpha = 0.75f), drops = 40, spread = 170.dp)
    }
    val members = model.seatMembers
    val thump = rememberThump()
    // A phone on its side has to fit all of this, down to the button, without scrolling.
    val tight = height < SHORT_RESULT
    ResultStage(height) {
        Column(Modifier.testTag("match-over"), horizontalAlignment = Alignment.CenterHorizontally) {
            // Our side of the table together.
            if (members != null && members.size == 4) {
                Row(horizontalArrangement = Arrangement.spacedBy(6.dp)) {
                    listOf(0, 2).forEach { seat ->
                        val profile = playerProfile(members[seat], model.members)
                        Box(Modifier.size(if (tight) 54.dp else 84.dp), contentAlignment = Alignment.Center) {
                            BrushedEnso(won)
                            if (profile.isBot) {
                                BasicText(profile.avatar, style = TextStyle(fontSize = (if (tight) 21 else 32).sp, color = Color.Black))
                            } else {
                                Txt(profile.avatar, size = if (tight) 20 else 30, weight = FontWeight.W600, color = accentColor(profile.accent), spacing = 0f)
                            }
                        }
                    }
                }
                Spacer(Modifier.size(if (tight) 2.dp else 10.dp))
            }
            Txt(if (won) "You win the match" else "Match lost", size = if (tight) 30 else 42, weight = FontWeight.W500)
            Spacer(Modifier.size(if (tight) 4.dp else 10.dp))
            Seal("${model.matchUs} – ${model.matchThem}", size = if (tight) 25 else 34, horizontal = if (tight) 14.dp else 18.dp, vertical = if (tight) 4.dp else 7.dp, onLand = thump)
            Spacer(Modifier.size(if (tight) 2.dp else 6.dp))
            Txt("first to ${model.matchTarget}", size = if (tight) 15 else 17, opacity = 0.7f)
            // Hand-by-hand chips only when we saw every hand: a mid-match reconnect starts the tally over.
            if (model.handResults.size == hands && hands > 0) {
                Spacer(Modifier.size(if (tight) 5.dp else 12.dp))
                FlowRow(horizontalArrangement = Arrangement.spacedBy(6.dp, Alignment.CenterHorizontally), verticalArrangement = Arrangement.spacedBy(6.dp)) {
                    model.handResults.forEach { swing -> HandChip(swing) }
                }
            }
            Spacer(Modifier.size(if (tight) 7.dp else 16.dp))
            PaperButton("Back to home", { controller.goHome() }, Modifier.testTag("match-home"), fill = PaperLight, size = 20)
        }
    }
}

@Composable
private fun HandChip(swing: Int) {
    val tone = if (swing > 0) Color(0xFF3F6F3F) else Red
    Box(Modifier.border(1.dp, tone.copy(alpha = 0.5f), RoundedCornerShape(999.dp)).padding(horizontal = 8.dp, vertical = 2.dp)) {
        Txt(if (swing > 0) "+$swing" else "−${-swing}", size = 14, color = tone)
    }
}

@OptIn(ExperimentalLayoutApi::class)
@Composable
private fun AbandonPanel(name: String, controller: GameController, modifier: Modifier) {
    Panel(modifier, fill = PaperLight, padding = 14.dp, entering = true) {
        Txt("$name abandoned the match.", size = 19, opacity = 0.85f)
        Spacer(Modifier.size(10.dp))
        // Side by side when they both fit, and one over the other on a narrow phone.
        FlowRow(horizontalArrangement = Arrangement.spacedBy(8.dp, Alignment.CenterHorizontally), verticalArrangement = Arrangement.spacedBy(8.dp)) {
            PaperButton("Replace with a bot", { controller.resolveAbandon(AbandonChoice.REPLACE) }, Modifier.testTag("abandon-replace"), size = 17, horizontal = 10.dp)
            PaperButton("End the match", { controller.resolveAbandon(AbandonChoice.CLOSE) }, Modifier.testTag("abandon-close"), size = 17, color = Red, horizontal = 10.dp)
        }
    }
}

// --- table notes (none of these take touches) ---

@Composable
private fun MomentBanner(title: String, detail: String, modifier: Modifier) {
    val ours = title.startsWith("You take")
    if (detail.isEmpty()) {
        // `#trick-outcome`
        val shape = RoundedCornerShape(999.dp)
        Box(
            modifier
                .entrance()
                .background(PaperLight.copy(alpha = 0.98f), shape)
                .border(1.dp, (if (ours) Green else Ink).copy(alpha = 0.5f), shape)
                .padding(horizontal = 14.dp, vertical = 7.dp),
        ) {
            Txt(title, size = 17, weight = FontWeight.W600, color = if (ours) Green else Ink, maxLines = 1)
        }
    } else {
        // `#table-moment`
        val shape = RoundedCornerShape(14.dp)
        Column(
            modifier
                .entrance()
                .widthIn(max = 300.dp)
                .background(PaperLight.copy(alpha = 0.97f), shape)
                .border(1.dp, Gold.copy(alpha = 0.5f), shape)
                .padding(horizontal = 18.dp, vertical = 9.dp),
            horizontalAlignment = Alignment.CenterHorizontally,
        ) {
            Txt(title.uppercase(), size = 16, weight = FontWeight.W600, spacing = 1.4f)
            Txt(detail, size = 14, opacity = 0.72f)
        }
    }
}

private val NOTE_WIDTH = 88.dp
private val NOTE_HEIGHT = 42.dp
private val NOTE_GAP = 5.dp

/**
 * A bid is a seal that thumps down by its player, and a pass stays quiet. Other players' calls sit beside the
 * ring, on the table's side, as on the web. Nothing else is there during an auction, and on a short phone the
 * prompt comes up to just below the name.
 */
@Composable
private fun BidNoteAt(text: String, spot: Spot, seat: Int, landscape: Boolean, screenWidth: Dp, screenHeight: Dp, insetLeft: Dp, insetRight: Dp, onLand: () -> Unit) {
    val ring = if (landscape) LANDSCAPE_RING else PORTRAIT_RING
    val centre = seatCentre(spot, ring, screenWidth, insetLeft, insetRight)
    val (x, y) = when {
        seat == 0 && landscape -> spot.x.dp - NOTE_WIDTH / 2 to spot.y.dp - 4.dp
        // Just above the fan, where our own bid panel was.
        seat == 0 -> spot.x.dp - NOTE_WIDTH / 2 to screenHeight * (1 - fanTopFromBottom(screenWidth, screenHeight)) - NOTE_HEIGHT + 4.dp
        seat == 1 -> centre - ring / 2 - NOTE_GAP - NOTE_WIDTH to spot.y.dp - NOTE_HEIGHT / 2
        else -> centre + ring / 2 + NOTE_GAP to spot.y.dp - NOTE_HEIGHT / 2
    }
    Box(
        Modifier.absoluteOffset(x, y).size(NOTE_WIDTH, NOTE_HEIGHT).testTag("bid-note-$seat"),
        contentAlignment = when (seat) {
            0 -> Alignment.Center
            1 -> Alignment.CenterEnd
            else -> Alignment.CenterStart
        },
    ) {
        if (text == "Pass") {
            Txt("Pass", size = 17, weight = FontWeight.W600, color = PassBrown, maxLines = 1)
        } else {
            Seal(text, size = 20, horizontal = 9.dp, vertical = 4.dp, onLand = onLand)
        }
    }
}

@Composable
private fun EmoteBubble(emote: String, spot: Spot, seat: Int) {
    val glyph = EMOTES.firstOrNull { it.first == emote }?.second ?: emote
    val progress = remember { Animatable(0f) }
    LaunchedEffect(Unit) { progress.animateTo(1f, tween(2_400, easing = LinearEasing)) }
    val size = 64.dp
    // `emote-float`: pops in, holds, then drifts up and fades.
    Box(
        Modifier
            .absoluteOffset(
                (spot.x.dp - size / 2).coerceAtLeast(0.dp),
                spot.y.dp - size / 2 - if (seat == 0) 0.dp else 26.dp,
            )
            .size(size)
            .graphicsLayer {
                val p = progress.value
                alpha = when {
                    p < 0.12f -> p / 0.12f
                    p < 0.72f -> 1f
                    else -> 1f - (p - 0.72f) / 0.28f
                }
                val scale = if (p < 0.12f) 0.4f + p / 0.12f * 0.78f else if (p < 0.22f) 1.18f - (p - 0.12f) / 0.1f * 0.18f else 1f
                scaleX = scale
                scaleY = scale
                translationY = -density * 48f * (if (p > 0.22f) (p - 0.22f) / 0.78f else 0f)
            },
        contentAlignment = Alignment.Center,
    ) {
        BasicText(glyph, style = TextStyle(fontSize = 34.sp, color = Color.Black))
    }
}
