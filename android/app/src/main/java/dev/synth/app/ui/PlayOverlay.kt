package dev.synth.app.ui

import androidx.compose.animation.core.Animatable
import androidx.compose.animation.core.LinearEasing
import androidx.compose.animation.core.RepeatMode
import androidx.compose.animation.core.animateFloat
import androidx.compose.animation.core.infiniteRepeatable
import androidx.compose.animation.core.rememberInfiniteTransition
import androidx.compose.animation.core.tween
import androidx.compose.foundation.Canvas
import androidx.compose.foundation.Image
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
import androidx.compose.foundation.layout.Spacer
import androidx.compose.foundation.layout.WindowInsets
import androidx.compose.foundation.layout.absoluteOffset
import androidx.compose.foundation.layout.asPaddingValues
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
import androidx.compose.ui.geometry.Offset
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.StrokeCap
import androidx.compose.ui.graphics.StrokeJoin
import androidx.compose.ui.graphics.drawscope.Stroke
import androidx.compose.ui.graphics.drawscope.scale
import androidx.compose.ui.graphics.vector.PathParser
import androidx.compose.ui.graphics.graphicsLayer
import androidx.compose.ui.platform.testTag
import androidx.compose.ui.res.painterResource
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
import dev.synth.app.R
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
private const val FAN_TOP_FROM_BOTTOM = 0.3f  // of the screen's height, in portrait
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
            val sideY = if (height <= 650) topCardY + 58 else maxOf(height * 0.46f, topCardY + 70)
            val bottomY = if (height <= 650) sideY + 36 else sideY + (height * 0.11f).coerceIn(68f, 105f)
            listOf(Spot(width / 2, bottomY), Spot(width - sideX, sideY), Spot(width / 2, topY), Spot(sideX, sideY))
        }
    }
}

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
        Box(safe) {
            TopBar(model, Modifier.align(Alignment.TopCenter), onMenu = { toggle(Popover.MENU) })
            BottomBar(controller, landscape, Modifier.align(Alignment.BottomCenter), onSort = { toggle(Popover.SORT) }, onEmote = { toggle(Popover.EMOTE) })
        }

        // In portrait the prompts sit just above the fan, where the player is already looking. A landscape
        // fan leaves no room there, so they hang from the top.
        val promptPlace = if (landscape) {
            Modifier.align(Alignment.TopCenter).padding(top = 130.dp)
        } else {
            Modifier.align(Alignment.BottomCenter).padding(bottom = height * FAN_TOP_FROM_BOTTOM)
        }
        if (controller.canExpose) {
            PaperButton(
                "Reveal trump",
                { controller.exposeTrump() },
                // In landscape the prompts' place is over the trick, so this one waits beside the fan.
                (if (landscape) Modifier.align(Alignment.BottomEnd).padding(end = 72.dp + insetRight, bottom = 14.dp) else promptPlace).testTag("expose-button"),
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
                modifier = promptPlace,
                maxWidth = width,
                onBid = controller::bid,
                onPass = controller::pass,
            )
            PendingRequest.Trump -> TrumpPanel(promptPlace, onSuit = controller::chooseTrump)
            else -> Unit
        }

        if (model.phase == GamePhase.HAND_FINISHED) HandResultPanel(controller, width, height)
        if (model.phase == GamePhase.FINISHED) MatchOverPanel(controller, width, height)

        // Announcements never take touches. They keep to whichever part of the table a prompt leaves free.
        controller.moment?.let { moment ->
            val top = when {
                landscape -> height * 0.24f
                pending != null -> height * 0.30f
                else -> height * 0.62f
            }
            MomentBanner(moment.title, moment.detail, Modifier.align(Alignment.TopCenter).padding(top = top))
        }
        if (model.phase == GamePhase.BIDDING || model.phase == GamePhase.CHOOSING_TRUMP) {
            controller.bidNotes.forEach { note ->
                // Our own note would land on the prompt, which already says where the bid stands.
                if (note.seat != 0 || pending == null) BidNoteAt(note.text, spots[note.seat], note.seat, landscape, width, height, insetLeft, insetRight)
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
private fun TopBar(model: GameModel, modifier: Modifier, onMenu: () -> Unit) {
    Row(modifier.fillMaxWidth().padding(horizontal = 12.dp, vertical = 6.dp), verticalAlignment = Alignment.CenterVertically) {
        Box(Modifier.weight(1f), contentAlignment = Alignment.CenterStart) {
            RoundButton(onMenu, "Menu", "menu") { MenuIcon() }
        }
        Row(horizontalArrangement = Arrangement.spacedBy(20.dp)) {
            Stat("${model.matchUs} – ${model.matchThem}", "match")
            Stat("${model.score} – ${model.theirScore}", "this hand")
        }
        Box(Modifier.weight(1f), contentAlignment = Alignment.CenterEnd) { Contract(model) }
    }
}

@Composable
private fun Stat(value: String, caption: String) {
    Column(horizontalAlignment = Alignment.CenterHorizontally) {
        Txt(value, size = 19, weight = FontWeight.W600, spacing = 0f, maxLines = 1)
        Txt(caption, size = 12, opacity = 0.62f, maxLines = 1)
    }
}

/** The bid and trump, once there's a bid: "16 ♥" over whose it is. A hidden trump is pale, or a "?" if we can't know it. */
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
    Column(Modifier.testTag("contract").semantics { contentDescription = "Bid $bid, $whose. $trumpWords" }, horizontalAlignment = Alignment.End) {
        Row(verticalAlignment = Alignment.CenterVertically, horizontalArrangement = Arrangement.spacedBy(5.dp)) {
            Txt("$bid", size = 19, weight = FontWeight.W600, spacing = 0f, maxLines = 1)
            when {
                suit == null -> Txt("?", size = 19, opacity = 0.55f, spacing = 0f)
                else -> Box(Modifier.alpha(if (model.trumpExposed) 1f else 0.5f)) { SuitGlyph(suit, 19) }
            }
        }
        Txt(whose, Modifier.widthIn(max = 104.dp), size = 12, opacity = 0.62f, maxLines = 1)
    }
}

/** The tools for the hand either side of whose turn it is. The result cards speak for themselves, so the turn is quiet then. */
@Composable
private fun BottomBar(controller: GameController, landscape: Boolean, modifier: Modifier, onSort: () -> Unit, onEmote: () -> Unit) {
    val model = controller.uiModel
    val pending = model.pendingRequest
    val yourTurn = pending is PendingRequest.Play
    val status = when {
        yourTurn -> "Your turn"
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

    Box(Modifier.absoluteOffset(cx - ring / 2, top).size(ring).testTag("seat-marker-$seat"), contentAlignment = Alignment.Center) {
        EnsoRing(active)
        SeatFace(profile, controller.bidNotes.firstOrNull { it.seat == seat }?.text, landscape)
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
private fun EnsoRing(active: Boolean) {
    val modifier = if (active) {
        // `seat-marker-turn`
        val transition = rememberInfiniteTransition(label = "turn")
        val pulse by transition.animateFloat(0.97f, 1.04f, infiniteRepeatable(tween(900), RepeatMode.Reverse), label = "turn")
        Modifier.graphicsLayer {
            scaleX = pulse
            scaleY = pulse
            alpha = 0.96f
        }
    } else {
        Modifier.alpha(0.72f)
    }
    Image(painterResource(R.drawable.enso), contentDescription = null, modifier.fillMaxSize())
}

@Composable
private fun SeatFace(profile: PlayerProfile, call: String?, landscape: Boolean) {
    // `seat-avatar-speak`
    val scale = remember { Animatable(1f) }
    LaunchedEffect(call) {
        if (call != null) {
            scale.snapTo(0.9f)
            scale.animateTo(1.09f, tween(286))
            scale.animateTo(1f, tween(234))
        }
    }
    val modifier = Modifier.graphicsLayer {
        scaleX = scale.value
        scaleY = scale.value
    }
    if (profile.isBot) {
        BasicText(profile.avatar, modifier, style = TextStyle(fontSize = (if (landscape) 19 else 23).sp, color = Color.Black))
    } else {
        Txt(profile.avatar, modifier, size = if (landscape) 18 else 21, weight = FontWeight.W600, color = accentColor(profile.accent), spacing = 0f)
    }
}

@Composable
private fun SuitGlyph(suit: Suit, size: Int) {
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
    modifier: Modifier,
    maxWidth: Dp,
    onBid: (Int) -> Unit,
    onPass: () -> Unit,
) {
    val values = (minBid..MAX_BID).toList()
    val label = if (opening) "Your bid" else "Beat ${minBid - 1}"
    val panelWidth = if (landscape) minOf(maxWidth - 240.dp, 660.dp) else minOf(maxWidth - 24.dp, 420.dp)
    val columns = if (landscape) 13 else 7
    Panel(modifier.width(panelWidth), padding = 8.dp, entering = true) {
        if (landscape) {
            Row(Modifier.fillMaxWidth(), verticalAlignment = Alignment.CenterVertically, horizontalArrangement = Arrangement.Center) {
                Txt(label, size = 18, opacity = 0.8f)
                if (!opening) {
                    Spacer(Modifier.width(16.dp))
                    PassButton(onPass, Modifier.widthIn(min = 120.dp))
                }
            }
        } else {
            Txt(label, size = 18, opacity = 0.8f)
        }
        Spacer(Modifier.size(3.dp))
        val rows = values.chunked(columns)
        rows.forEachIndexed { index, row ->
            // Pass shares the last row when it has room, which keeps the panel low.
            val passHere = !landscape && !opening && index == rows.lastIndex && row.size < columns
            Row(Modifier.fillMaxWidth().padding(vertical = 1.dp), horizontalArrangement = Arrangement.spacedBy(4.dp)) {
                row.forEach { value ->
                    Pressable({ onBid(value) }, Modifier.weight(1f).heightIn(min = 42.dp).testTag("bid-$value")) {
                        Txt("$value", size = 20, spacing = 0f, maxLines = 1)
                    }
                }
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

@Composable
private fun TrumpPanel(modifier: Modifier, onSuit: (Suit) -> Unit) {
    Panel(modifier, padding = 14.dp, entering = true) {
        Txt("Choose trump", size = 18, opacity = 0.8f)
        Spacer(Modifier.size(8.dp))
        Row(horizontalArrangement = Arrangement.spacedBy(6.dp)) {
            SUITS.forEach { suit ->
                Pressable({ onSuit(suit) }, Modifier.size(width = 54.dp, height = 60.dp).testTag("trump-${suit.id}")) {
                    SuitGlyph(suit, 30)
                }
            }
        }
    }
}

// --- results ---

@Composable
private fun BoxScope.ResultCard(width: Dp, height: Dp, tag: String?, content: @Composable () -> Unit) {
    Panel(
        Modifier
            .align(Alignment.Center)
            .width(minOf(width * 0.84f, 460.dp))
            .heightIn(max = height * 0.92f)
            .then(if (tag != null) Modifier.testTag(tag) else Modifier),
        fill = PaperLight,
        padding = 18.dp,
        entering = true,
    ) {
        Column(Modifier.verticalScroll(rememberScrollState()), horizontalAlignment = Alignment.CenterHorizontally) { content() }
    }
}

/** Between hands: the card points, the verdict and match score, and the host's deal button or a waiting note. */
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
    val tone = if (swing > 0) Green else if (swing < 0) Red else Ink

    ResultCard(width, height, tag = null) {
        if (result != null) {
            val ours = result.bidder == 0 || result.bidder == 2
            val bidder = if (result.bidder == 0) "You" else model.seatNames?.getOrNull(result.bidder) ?: "They"
            val took = if (ours) model.score else model.theirScore
            Txt(if (result.made) "Bid made" else "Bid set", size = 32, weight = FontWeight.W500, color = tone)
            Spacer(Modifier.size(4.dp))
            Txt("$bidder bid ${result.bid}, took $took", size = 17, opacity = 0.8f)
            Spacer(Modifier.size(10.dp))
            // The points go to the bidding side, so a made bid by them is +1 to them, and a set one takes points from them.
            val size = kotlin.math.abs(swing)
            val delta = when {
                size == 0 -> "no change"
                else -> (if (result.made) "+$size" else "−$size") + if (ours) "" else " to them"
            }
            Box(Modifier.border(1.dp, tone, RoundedCornerShape(999.dp)).padding(horizontal = 11.dp, vertical = 3.dp)) {
                Txt(delta, size = 15, weight = FontWeight.W600, color = tone)
            }
            Spacer(Modifier.size(10.dp))
            Txt("Match ${model.matchUs} – ${model.matchThem} · first to ${model.matchTarget}", size = 16, opacity = 0.72f)
        } else {
            Txt("${model.score} – ${model.theirScore}", size = 32, weight = FontWeight.W500)
        }
        Spacer(Modifier.size(14.dp))
        when {
            dealing -> WaitingNote("dealing…")
            controller.isActingHost -> PaperButton(
                "Deal the next hand",
                { dealing = true; controller.nextHand() },
                Modifier.testTag("next-hand"),
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
    ResultCard(width, height, tag = "match-over") {
        BasicText(if (won) "🏆" else "🍂", style = TextStyle(fontSize = 44.sp, color = Color.Black))
        Spacer(Modifier.size(4.dp))
        Txt(if (won) "You win the match!" else "Match lost", size = 36, weight = FontWeight.W500)
        Spacer(Modifier.size(6.dp))
        Txt(
            "${model.matchUs} – ${model.matchThem} · first to ${model.matchTarget}",
            size = 17,
            opacity = 0.8f,
        )
        // Hand-by-hand chips only when we saw every hand: a mid-match reconnect starts the tally over.
        if (model.handResults.size == hands && hands > 0) {
            Spacer(Modifier.size(12.dp))
            FlowRow(horizontalArrangement = Arrangement.spacedBy(6.dp, Alignment.CenterHorizontally), verticalArrangement = Arrangement.spacedBy(6.dp)) {
                model.handResults.forEach { swing -> HandChip(swing) }
            }
        }
        Spacer(Modifier.size(16.dp))
        PaperButton("Back to home", { controller.goHome() }, Modifier.testTag("match-home"))
    }
}

@Composable
private fun HandChip(swing: Int) {
    val tone = if (swing > 0) Color(0xFF3F6F3F) else Red
    Box(Modifier.border(1.dp, tone.copy(alpha = 0.5f), RoundedCornerShape(999.dp)).padding(horizontal = 8.dp, vertical = 2.dp)) {
        Txt(if (swing > 0) "+$swing" else "−${-swing}", size = 14, color = tone)
    }
}

@Composable
private fun AbandonPanel(name: String, controller: GameController, modifier: Modifier) {
    Panel(modifier, fill = PaperLight, padding = 14.dp, entering = true) {
        Txt("$name abandoned the match.", size = 19, opacity = 0.85f)
        Spacer(Modifier.size(10.dp))
        Row(horizontalArrangement = Arrangement.spacedBy(8.dp)) {
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

@Composable
private fun BidNoteAt(text: String, spot: Spot, seat: Int, landscape: Boolean, screenWidth: Dp, screenHeight: Dp, insetLeft: Dp, insetRight: Dp) {
    val pass = text == "Pass"
    val half = NOTE_WIDTH / 2
    val ring = if (landscape) LANDSCAPE_RING else PORTRAIT_RING
    val centre = seatCentre(spot, ring, screenWidth, insetLeft, insetRight)
    // Below the name, past the partner tag; a landscape partner has no room below, so it goes beside the ring.
    val (x, y) = when {
        seat == 0 && landscape -> spot.x.dp to spot.y.dp
        // Just above the fan, where our own bid panel was.
        seat == 0 -> spot.x.dp to screenHeight * (1 - FAN_TOP_FROM_BOTTOM) - 20.dp
        landscape && seat == 2 -> centre + ring / 2 + half + 6.dp to spot.y.dp - 12.dp
        seat == 2 -> centre to spot.y.dp + ring / 2 + 42.dp
        else -> centre to spot.y.dp + ring / 2 + 25.dp
    }
    Txt(
        if (pass) "— Pass" else "✦ Bid $text",
        Modifier.absoluteOffset(x.coerceIn(insetLeft + half, screenWidth - insetRight - half) - half, y).width(NOTE_WIDTH),
        size = 16,
        weight = FontWeight.W600,
        color = if (pass) PassBrown else Green,
        maxLines = 1,
    )
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
