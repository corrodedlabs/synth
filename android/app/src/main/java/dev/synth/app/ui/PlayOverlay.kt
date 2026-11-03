package dev.synth.app.ui

import androidx.compose.animation.core.Animatable
import androidx.compose.animation.core.LinearEasing
import androidx.compose.animation.core.tween
import androidx.compose.foundation.Canvas
import androidx.compose.foundation.background
import androidx.compose.foundation.border
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
import androidx.compose.ui.geometry.Offset
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
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import dev.synth.app.GameController
import dev.synth.core.AbandonChoice
import dev.synth.core.ArrangementPreset
import dev.synth.core.GameModel
import dev.synth.core.GamePhase
import dev.synth.core.MatchSide
import dev.synth.core.PendingRequest
import dev.synth.core.SUITS
import dev.synth.core.Suit
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

/**
 * Over the live table. Nothing here fills the screen with anything that takes touches: the boxes are
 * layout only, and each real control or panel takes its own.
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

        // Only the cutout: the app is immersive, and the default theme's hidden action bar reports hidden
        // bars as 158px/68px through the safe-drawing insets, which would push the HUD into the seats.
        Box(Modifier.fillMaxSize().windowInsetsPadding(WindowInsets.displayCutout)) {
            Hud(controller, landscape)
        }

        if (controller.canExpose) {
            // Above the fan: it is our turn and the cards must stay clear.
            PaperButton(
                "Reveal trump",
                { controller.exposeTrump() },
                Modifier
                    .align(Alignment.TopCenter)
                    .padding(top = height * 0.6f - 52.dp)
                    .testTag("expose-button"),
                fill = PaperLight.copy(alpha = 0.92f),
                spacing = 0.6f,
                horizontal = 22.dp,
            )
        }

        val panelTop = if (landscape) 104.dp else height * 0.193f
        when (pending) {
            is PendingRequest.Bid -> BidPanel(
                minBid = pending.minBid,
                opening = model.bidsPlaced == 0,
                landscape = landscape,
                modifier = Modifier.align(Alignment.TopCenter).padding(top = panelTop),
                maxWidth = width,
                onBid = controller::bid,
                onPass = controller::pass,
            )
            PendingRequest.Trump -> TrumpPanel(Modifier.align(Alignment.TopCenter).padding(top = panelTop), onSuit = controller::chooseTrump)
            else -> Unit
        }

        if (model.phase == GamePhase.HAND_FINISHED) HandResultPanel(controller, width, height)
        if (model.phase == GamePhase.FINISHED) MatchOverPanel(controller, width, height)

        // Announcements never take touches, so they can sit above the panels.
        controller.moment?.let { moment ->
            val top = if (landscape) height * 0.24f else height * 0.17f
            MomentBanner(moment.title, moment.detail, Modifier.align(Alignment.TopCenter).padding(top = top))
        }
        if (model.phase == GamePhase.BIDDING || model.phase == GamePhase.CHOOSING_TRUMP) {
            controller.bidNotes.forEach { note -> BidNoteAt(note.text, spots[note.seat], note.seat, width) }
        }
        controller.emotes.forEach { note ->
            key(note.key) { EmoteBubble(note.glyph, spots[note.seat], note.seat) }
        }

        controller.abandonedName?.let { name ->
            AbandonPanel(name, controller, Modifier.align(Alignment.TopCenter).padding(top = height * 0.06f).width(minOf(width * 0.86f, 420.dp)))
        }
    }
}

// --- the HUD ---

@Composable
private fun BoxScope.Hud(controller: GameController, landscape: Boolean) {
    val model = controller.uiModel
    val inMatch = model.phase != GamePhase.FINISHED
    Column(Modifier.align(Alignment.TopStart).fillMaxWidth().padding(horizontal = 12.dp, vertical = 8.dp)) {
        if (!landscape) {
            Box(Modifier.fillMaxWidth(), contentAlignment = Alignment.CenterEnd) { ScoreStrip(model) }
        }
        Row(Modifier.fillMaxWidth(), horizontalArrangement = Arrangement.SpaceBetween) {
            // Left: what's going on, then the quiet controls.
            Column(Modifier.weight(1f, fill = false), verticalArrangement = Arrangement.spacedBy(6.dp)) {
                Box(Modifier.heightIn(min = 20.dp), contentAlignment = Alignment.CenterStart) {
                    if (controller.statusText.isNotEmpty()) {
                        Txt(
                            controller.statusText,
                            Modifier.testTag("status").widthIn(max = if (landscape) 330.dp else 230.dp),
                            size = 16,
                            opacity = 0.85f,
                            align = TextAlign.Start,
                            maxLines = 2,
                        )
                    }
                }
                if (landscape) {
                    Row(horizontalArrangement = Arrangement.spacedBy(6.dp), verticalAlignment = Alignment.CenterVertically) {
                        MuteChip(controller)
                        if (inMatch) LeaveMatchChip(controller)
                        if (controller.canArrange) ArrangeTiles(controller, vertical = false)
                    }
                } else {
                    Row(horizontalArrangement = Arrangement.spacedBy(6.dp), verticalAlignment = Alignment.CenterVertically) {
                        MuteChip(controller)
                        if (inMatch) LeaveMatchChip(controller)
                    }
                    if (controller.canArrange) ArrangeTiles(controller, vertical = true)
                }
            }
            // Right: the score, the bid, trump, then the emotes.
            Column(horizontalAlignment = Alignment.End, verticalArrangement = Arrangement.spacedBy(4.dp)) {
                if (landscape) ScoreStrip(model)
                if (landscape) {
                    Row(horizontalArrangement = Arrangement.spacedBy(14.dp), verticalAlignment = Alignment.CenterVertically) { TrumpLine(model) }
                } else {
                    TrumpLine(model)
                }
                if (inMatch) {
                    Spacer(Modifier.size(2.dp))
                    EmoteStrip(controller, vertical = !landscape)
                }
            }
        }
    }
}

@Composable
private fun ScoreStrip(model: GameModel) {
    Row(verticalAlignment = Alignment.CenterVertically) {
        ScorePart("You — Them:", "${model.score} — ${model.theirScore}")
        ScoreDivider()
        ScorePart("Tricks:", "${model.tricks} — ${model.theirTricks}")
        ScoreDivider()
        ScorePart("Match:", "${model.matchUs} — ${model.matchThem}")
    }
}

@Composable
private fun ScorePart(label: String, value: String) {
    Txt(label, size = 15, opacity = 0.72f, maxLines = 1)
    Spacer(Modifier.width(3.dp))
    Txt(value, size = 15, weight = FontWeight.W600, maxLines = 1)
}

@Composable
private fun ScoreDivider() {
    Txt("|", Modifier.padding(horizontal = 5.dp), size = 15, opacity = 0.4f)
}

@Composable
private fun TrumpLine(model: GameModel) {
    val bid = model.finalBid
    val winner = model.bidWinner
    if (bid != null) {
        val who = when (winner) {
            null -> ""
            0 -> " · you"
            else -> " · " + (model.seatNames?.getOrNull(winner) ?: "")
        }
        Txt("Bid $bid$who", size = 16, opacity = 0.85f, maxLines = 1)
    }
    val suit = model.trumpSuit
    val known = suit != null || model.trumpExposed
    if (known || model.phase == GamePhase.PLAYING) {
        Row(verticalAlignment = Alignment.CenterVertically, horizontalArrangement = Arrangement.spacedBy(4.dp)) {
            when {
                suit == null -> Txt("Trump: hidden", size = 16, opacity = 0.85f, maxLines = 1)
                model.trumpExposed -> Txt("Trump:", size = 16, opacity = 0.85f, maxLines = 1)
                else -> Txt("Trump (hidden):", size = 16, opacity = 0.85f, maxLines = 1)
            }
            if (suit != null) SuitGlyph(suit, 18)
        }
    }
}

@Composable
private fun SuitGlyph(suit: Suit, size: Int) {
    BasicText(suit.glyph, style = TextStyle(color = suit.ink, fontSize = size.sp))
}

@Composable
private fun MuteChip(controller: GameController) {
    Pressable(
        { controller.toggleMute() },
        Modifier.size(40.dp).testTag("mute").semantics { contentDescription = if (controller.muted) "Turn sound on" else "Turn sound off" },
        shape = RoundedCornerShape(999.dp),
        fill = PaperLight.copy(alpha = 0.72f),
        border = Ink.copy(alpha = 0.3f),
    ) {
        SoundIcon(controller.muted)
    }
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
private fun EmoteStrip(controller: GameController, vertical: Boolean) {
    val shape = RoundedCornerShape(30.dp)
    val strip = Modifier
        .background(PaperLight.copy(alpha = 0.85f), shape)
        .border(1.dp, Ink.copy(alpha = 0.22f), shape)
        .padding(2.dp)
    val buttons: @Composable () -> Unit = {
        EMOTES.forEach { (id, glyph) ->
            Pressable(
                { controller.sendEmote(id) },
                Modifier.size(38.dp).testTag("emote-$id"),
                shape = RoundedCornerShape(50),
                border = Color.Transparent,
            ) {
                BasicText(glyph, style = TextStyle(fontSize = 20.sp, color = Color.Black))
            }
        }
    }
    if (vertical) Column(strip) { buttons() } else Row(strip) { buttons() }
}

@Composable
private fun ArrangeTiles(controller: GameController, vertical: Boolean) {
    val trump = controller.uiModel.trumpSuit
    val selected = controller.arrangement
    // A row has to clear the partner's name at the top centre, even beside a cutout.
    val tileWidth = if (vertical) 48.dp else 42.dp
    val tiles: @Composable () -> Unit = {
        ArrangeTile("♠\uFE0E♥\uFE0E", "suit", selected == ArrangementPreset.SUIT, true, "arrange-suit", tileWidth) { controller.arrange(ArrangementPreset.SUIT) }
        ArrangeTile("J→7", "strength", selected == ArrangementPreset.STRENGTH, true, "arrange-strength", tileWidth) { controller.arrange(ArrangementPreset.STRENGTH) }
        ArrangeTile(trump?.glyph ?: "✦", "trump", selected == ArrangementPreset.TRUMP, trump != null, "arrange-trump", tileWidth) { controller.arrange(ArrangementPreset.TRUMP) }
        ArrangeTile("↺", "deal", selected == ArrangementPreset.DEAL, true, "arrange-deal", tileWidth) { controller.arrange(ArrangementPreset.DEAL) }
    }
    if (vertical) {
        Column(verticalArrangement = Arrangement.spacedBy(4.dp)) { tiles() }
    } else {
        Row(horizontalArrangement = Arrangement.spacedBy(4.dp)) { tiles() }
    }
}

@Composable
private fun ArrangeTile(glyph: String, label: String, selected: Boolean, enabled: Boolean, tag: String, tileWidth: Dp, onClick: () -> Unit) {
    Pressable(
        onClick,
        Modifier.size(width = tileWidth, height = 42.dp).testTag(tag).semantics { this.selected = selected },
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
    val label = if (opening) "Open the bidding" else "Bid stands at ${minBid - 1}"
    val panelWidth = if (landscape) minOf(maxWidth - 208.dp, 660.dp) else minOf(maxWidth - 128.dp, 300.dp)
    val columns = if (landscape) 13 else 5
    Panel(modifier.width(panelWidth).entrance(), padding = 10.dp) {
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
        Spacer(Modifier.size(6.dp))
        val rows = values.chunked(columns)
        rows.forEachIndexed { index, row ->
            // Pass shares the last row when it has room, which keeps the panel clear of the side seats.
            val passHere = !landscape && !opening && index == rows.lastIndex && row.size < columns
            Row(Modifier.fillMaxWidth().padding(vertical = 2.dp), horizontalArrangement = Arrangement.spacedBy(4.dp)) {
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
    Panel(modifier.entrance(), padding = 14.dp) {
        Txt("Choose the trump suit", size = 18, opacity = 0.8f)
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
            // In portrait the emote strip runs down the right edge: settle below it.
            .then(if (height > width && tag == null) Modifier.padding(top = 130.dp) else Modifier)
            .width(minOf(width * 0.84f, 460.dp))
            .heightIn(max = height * 0.92f)
            .then(if (tag != null) Modifier.testTag(tag) else Modifier)
            .entrance(),
        fill = PaperLight,
        padding = 18.dp,
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
            Txt(if (swing > 0) "✦" else "◆", size = 46, color = tone)
        }
        Txt("${model.score} — ${model.theirScore}", size = 40, weight = FontWeight.W500)
        Spacer(Modifier.size(6.dp))
        if (result != null) {
            val ours = result.bidder == 0 || result.bidder == 2
            val bidder = if (result.bidder == 0) "Your" else "${model.seatNames?.getOrNull(result.bidder) ?: "Their"}'s"
            val took = if (ours) model.score else model.theirScore
            val swingLine = when {
                swing > 0 -> "Your team gains $swing match point${if (swing == 1) "" else "s"}."
                swing < 0 -> "Their team gains ${-swing} match point${if (swing == -1) "" else "s"}."
                else -> "The match score is unchanged."
            }
            Txt(
                "$bidder team bid ${result.bid} and took $took — ${if (result.made) "made it" else "set"}\n" +
                    "$swingLine\nMatch: ${model.matchUs} — ${model.matchThem} (first to ${model.matchTarget})",
                size = 17,
                opacity = 0.8f,
            )
            Spacer(Modifier.size(10.dp))
            val delta = when {
                swing > 0 -> "+$swing match point${if (swing == 1) "" else "s"}"
                swing < 0 -> "−${-swing} match point${if (swing == -1) "" else "s"}"
                else -> "match score unchanged"
            }
            Box(Modifier.border(1.dp, tone, RoundedCornerShape(999.dp)).padding(horizontal = 11.dp, vertical = 3.dp)) {
                Txt(delta, size = 15, weight = FontWeight.W600, color = tone)
            }
            Spacer(Modifier.size(12.dp))
        }
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
            "${model.matchUs} — ${model.matchThem} in game points\n" +
                "first to ${model.matchTarget} · $hands hand${if (hands == 1) "" else "s"} played",
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
    Panel(modifier.entrance(), fill = PaperLight, padding = 14.dp) {
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

private val NOTE_WIDTH = 120.dp

@Composable
private fun BidNoteAt(text: String, spot: Spot, seat: Int, screenWidth: Dp) {
    val pass = text == "Pass"
    val half = NOTE_WIDTH / 2
    val x = spot.x.dp.coerceIn(half, screenWidth - half) - half
    val y = spot.y.dp + if (seat == 0) 0.dp else 3.dp
    Txt(
        if (pass) "— Pass" else "✦ Bid $text",
        Modifier.absoluteOffset(x, y).width(NOTE_WIDTH),
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
