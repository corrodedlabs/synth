package dev.synth.app.ui

import android.content.ActivityNotFoundException
import android.content.Context
import android.content.Intent
import android.net.Uri
import androidx.compose.animation.core.Animatable
import androidx.compose.animation.core.RepeatMode
import androidx.compose.animation.core.animateFloat
import androidx.compose.animation.core.infiniteRepeatable
import androidx.compose.animation.core.rememberInfiniteTransition
import androidx.compose.animation.core.tween
import androidx.compose.foundation.background
import androidx.compose.foundation.border
import androidx.compose.foundation.clickable
import androidx.compose.foundation.interaction.MutableInteractionSource
import androidx.compose.foundation.interaction.collectIsFocusedAsState
import androidx.compose.foundation.interaction.collectIsPressedAsState
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.BoxWithConstraints
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.Spacer
import androidx.compose.foundation.layout.WindowInsets
import androidx.compose.foundation.layout.absoluteOffset
import androidx.compose.foundation.layout.height
import androidx.compose.foundation.layout.width
import androidx.compose.foundation.layout.captionBar
import androidx.compose.foundation.layout.displayCutout
import androidx.compose.foundation.layout.ime
import androidx.compose.foundation.layout.union
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.heightIn
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.layout.size
import androidx.compose.foundation.layout.widthIn
import androidx.compose.foundation.layout.windowInsetsPadding
import androidx.compose.foundation.lazy.LazyColumn
import androidx.compose.foundation.lazy.items
import androidx.compose.foundation.rememberScrollState
import androidx.compose.foundation.shape.CircleShape
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.foundation.text.BasicText
import androidx.compose.foundation.text.BasicTextField
import androidx.compose.foundation.text.KeyboardActions
import androidx.compose.foundation.text.KeyboardOptions
import androidx.compose.foundation.text.selection.LocalTextSelectionColors
import androidx.compose.foundation.text.selection.TextSelectionColors
import androidx.compose.foundation.verticalScroll
import androidx.compose.runtime.Composable
import androidx.compose.runtime.CompositionLocalProvider
import androidx.compose.runtime.LaunchedEffect
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.remember
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.runtime.setValue
import androidx.compose.ui.Alignment
import androidx.compose.ui.ExperimentalComposeUiApi
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.alpha
import androidx.compose.ui.draw.drawBehind
import androidx.compose.ui.geometry.Offset
import androidx.compose.ui.geometry.Size
import androidx.compose.ui.graphics.PathEffect
import androidx.compose.ui.graphics.drawscope.Stroke
import androidx.compose.ui.graphics.drawscope.translate
import androidx.compose.ui.graphics.graphicsLayer
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.SolidColor
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.platform.LocalFocusManager
import androidx.compose.ui.platform.testTag
import androidx.compose.ui.semantics.Role
import androidx.compose.ui.semantics.selected
import androidx.compose.ui.semantics.semantics
import androidx.compose.ui.semantics.testTagsAsResourceId
import androidx.compose.ui.text.TextStyle
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.input.ImeAction
import androidx.compose.ui.text.input.KeyboardCapitalization
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import dev.synth.app.BuildConfig
import dev.synth.app.GameController
import dev.synth.core.GamePhase
import dev.synth.core.LeaderboardRow
import dev.synth.core.PlayerProfile
import dev.synth.core.RoomInfo
import dev.synth.core.playerProfile
import kotlinx.coroutines.delay

private const val NAME_LIMIT = 20
private const val ROOM_POLL_MS = 4_000L

/**
 * Everything above the table. Only the menus and real panels take touches: during play nothing here is
 * full screen, so a tap that lands on no panel reaches the SDL surface below.
 */
@OptIn(ExperimentalComposeUiApi::class)
@Composable
fun GameScreens(controller: GameController, shareInvite: (String) -> Unit) {
    var name by rememberSaveable { mutableStateOf(controller.playerName) }
    var fetchingStandings by remember { mutableStateOf(false) }
    val phase = controller.uiModel.phase
    val standings = controller.standings

    CompositionLocalProvider(LocalReduceMotion provides controller.reduceMotion) {
    Box(Modifier.fillMaxSize().semantics { testTagsAsResourceId = true }) {
        when {
            controller.updateRequiredVersion != null -> UpdateRequiredScreen()
            controller.isReconnecting -> ReconnectingScreen(controller)
            standings != null || fetchingStandings -> StandingsScreen(standings) {
                fetchingStandings = false
                controller.closeStandings()
            }
            controller.showingRooms -> RoomsScreen(controller)
            phase == GamePhase.IDLE -> StartScreen(
                controller,
                name,
                onName = { name = it.take(NAME_LIMIT) },
                onStandings = {
                    fetchingStandings = true
                    controller.showStandings(name)
                },
            )
            phase == GamePhase.CONNECTING -> ConnectingScreen(controller)
            phase == GamePhase.LOBBY -> LobbyScreen(controller, shareInvite)
            else -> PlayOverlay(controller)
        }
    }
    }
}

// The caption bar is the window handle Android draws at the top of a tablet's screen. Phones report none.
private val MenuInsets: WindowInsets
    @Composable get() = WindowInsets.displayCutout.union(WindowInsets.captionBar).union(WindowInsets.ime)

/** Opaque paper for the menus, scrolled when a short screen or the keyboard leaves too little room. */
@Composable
private fun PaperScreen(modifier: Modifier = Modifier, content: @Composable () -> Unit) {
    Box(
        modifier
            .fillMaxSize()
            .paperBackground()
            .takesTouches()
            .windowInsetsPadding(MenuInsets),
        contentAlignment = Alignment.Center,
    ) { content() }
}

@Composable
private fun StatusNote(text: String, modifier: Modifier = Modifier) {
    if (text.isNotEmpty()) {
        Txt(text, modifier.testTag("status").padding(horizontal = 16.dp), size = 17, opacity = 0.75f)
    }
}

// --- start ---

/**
 * The way in is a table with your seat waiting at it. Two big buttons say plainly who you'll play with, and the
 * match types are cards to pick up first. Joining someone else's table stays a tap away.
 */
@Composable
private fun StartScreen(controller: GameController, name: String, onName: (String) -> Unit, onStandings: () -> Unit) {
    var target by rememberSaveable { mutableStateOf(2) }
    val focus = LocalFocusManager.current
    val invite = controller.joinTarget
    PaperScreen {
        Column(
            Modifier.fillMaxWidth().verticalScroll(rememberScrollState()).padding(vertical = 14.dp),
            horizontalAlignment = Alignment.CenterHorizontally,
        ) {
            Txt("28", size = 76, weight = FontWeight.W500, spacing = 6f)
            HomeTable(name)
            Spacer(Modifier.size(10.dp))
            NameField(name, onName, onDone = { focus.clearFocus() })
            Spacer(Modifier.size(12.dp))

            if (invite != null) {
                PaperButton(
                    "Join “$invite”",
                    { focus.clearFocus(); controller.joinByLink(name, invite) },
                    Modifier.testTag("invite-join").widthIn(min = 240.dp, max = 330.dp),
                    size = 19,
                    fill = PressTint,
                    spacing = 1f,
                )
                LinkButton("not this time", { controller.joinTarget = null }, Modifier.testTag("invite-dismiss"))
                Spacer(Modifier.size(10.dp))
            }

            Row(horizontalArrangement = Arrangement.spacedBy(12.dp), verticalAlignment = Alignment.CenterVertically) {
                TargetChoice("Quick", "first to 2", target == 2, -2f, Modifier.testTag("target-quick")) { target = 2 }
                TargetChoice("Classic", "first to 6", target == 6, 2f, Modifier.testTag("target-classic")) { target = 6 }
            }
            Spacer(Modifier.size(16.dp))
            // Friends is the headline; bots is just as easy to reach, in ink rather than red.
            BigSealButton("Play with friends", Modifier.testTag("create-button").padding(horizontal = 44.dp).widthIn(max = 330.dp).fillMaxWidth().height(62.dp), fontSize = 28) {
                focus.clearFocus()
                controller.createTable(name, target)
            }
            // The torn edges and tilts reach past each button's box, so they need a clear gap to look apart.
            Spacer(Modifier.size(22.dp))
            BigSealButton(
                "Play with bots",
                Modifier.testTag("play-bots").padding(horizontal = 44.dp).widthIn(max = 330.dp).fillMaxWidth().height(62.dp),
                fontSize = 28,
                fill = Ink,
                edge = Color(0xFF241C18),
                tilt = 1.1f,
            ) {
                focus.clearFocus()
                controller.playWithBots(name, target)
            }
            Spacer(Modifier.size(12.dp))
            Row(horizontalArrangement = Arrangement.spacedBy(18.dp), verticalAlignment = Alignment.CenterVertically) {
                LinkButton("Join a table", { focus.clearFocus(); controller.browse(name) }, Modifier.testTag("browse-button"), size = 18)
                LinkButton("Standings", { focus.clearFocus(); onStandings() }, Modifier.testTag("standings-button"), size = 18)
            }
            Spacer(Modifier.size(6.dp))
            // goHome leaves the last status behind: an abandoned connection attempt says nothing true here.
            StatusNote(controller.statusText.takeUnless { it == "Connecting…" || it == "Rejoining your match…" }.orEmpty())
        }
    }
}

/** Four seats around a small fan of real cards. Yours has your initial in it, and the others are waiting. */
@Composable
private fun HomeTable(name: String) {
    val atlas = rememberCardAtlas()
    Box(Modifier.size(width = 300.dp, height = 190.dp)) {
        fun place(x: Int, y: Int) = Modifier.absoluteOffset((x - 24).dp, (y - 24).dp).size(48.dp)
        Box(place(150, 22), contentAlignment = Alignment.Center) { BrushedEnso(false, Modifier.alpha(0.55f)) }
        Box(place(36, 92), contentAlignment = Alignment.Center) { BrushedEnso(false, Modifier.alpha(0.55f)) }
        Box(place(264, 92), contentAlignment = Alignment.Center) { BrushedEnso(false, Modifier.alpha(0.55f)) }
        Box(place(150, 164), contentAlignment = Alignment.Center) {
            BrushedEnso(false)
            Txt(name.trim().take(1).uppercase().ifEmpty { "?" }, size = 19, weight = FontWeight.W600, color = Red, spacing = 0f)
        }
        if (atlas != null) {
            // Jack of spades, ace of hearts, nine of diamonds, ten of clubs: the cards that carry the points.
            listOf(3 to 0, 0 to 2, 1 to 1, 2 to 3).forEachIndexed { i, (suit, rank) ->
                val spread = i - 1.5f
                AtlasCard(
                    atlas,
                    suit,
                    rank,
                    Modifier
                        .absoluteOffset((121 + spread * 27).dp, (48 + kotlin.math.abs(spread) * 4).dp)
                        .size(width = 58.dp, height = 86.dp)
                        .graphicsLayer { rotationZ = spread * 9f },
                )
            }
        }
    }
}

/** The loudest button in the game: a red seal that presses into the paper. */
@Composable
private fun BigSealButton(
    text: String,
    modifier: Modifier = Modifier,
    enabled: Boolean = true,
    fontSize: Int = 32,
    fill: Color = Red,
    edge: Color = Color(0xFF7C2C21),
    tilt: Float = -1.2f,
    onClick: () -> Unit,
) {
    val source = remember { MutableInteractionSource() }
    val pressed by source.collectIsPressedAsState()
    Box(
        modifier
            .graphicsLayer {
                rotationZ = tilt
                translationY = if (pressed) 3.dp.toPx() else 0f
            }
            .drawBehind {
                val outline = sealOutline(size, text.hashCode(), 9.dp.toPx(), 2.dp.toPx())
                if (!pressed) translate(top = 4.dp.toPx()) { drawPath(outline, edge) }
                drawPath(outline, fill)
            }
            .clickable(source, indication = null, enabled = enabled, role = Role.Button, onClick = onClick),
        contentAlignment = Alignment.Center,
    ) {
        Txt(text, size = fontSize, weight = FontWeight.W600, color = PaperLight, spacing = 0f, maxLines = 1)
    }
}

/** Text on a brushed line rather than in a form box. A first-time player sees only "Your name". */
@Composable
private fun NameField(name: String, onName: (String) -> Unit, onDone: () -> Unit) {
    val style = TextStyle(
        fontFamily = Cormorant,
        fontSize = 23.sp,
        color = Ink,
        textAlign = TextAlign.Center,
    )
    val source = remember { MutableInteractionSource() }
    val focused by source.collectIsFocusedAsState()
    CompositionLocalProvider(LocalTextSelectionColors provides TextSelectionColors(Ink, Ink.copy(alpha = 0.2f))) {
      BasicTextField(
        value = name,
        onValueChange = onName,
        modifier = Modifier.testTag("name-input").width(230.dp),
        singleLine = true,
        textStyle = style,
        cursorBrush = SolidColor(Ink),
        interactionSource = source,
        keyboardOptions = KeyboardOptions(capitalization = KeyboardCapitalization.Words, imeAction = ImeAction.Done),
        keyboardActions = KeyboardActions(onDone = { onDone() }),
        decorationBox = { field ->
            Box(
                Modifier
                    .drawBehind {
                        val y = size.height - 1.dp.toPx()
                        drawLine(if (focused) Red else Ink.copy(alpha = 0.35f), Offset(0f, y), Offset(size.width, y), 1.dp.toPx())
                    }
                    .padding(horizontal = 8.dp, vertical = 8.dp),
                contentAlignment = Alignment.Center,
            ) {
                if (name.isEmpty()) BasicText("Your name", style = style.copy(color = Ink.copy(alpha = 0.5f)))
                field()
            }
        },
      )
    }
}

/** A match type, as a card lying on the table. The one you pick lifts and straightens. */
@Composable
private fun TargetChoice(title: String, caption: String, selected: Boolean, tilt: Float, modifier: Modifier, onClick: () -> Unit) {
    Pressable(
        onClick,
        modifier
            .size(width = 132.dp, height = 84.dp)
            .graphicsLayer {
                rotationZ = if (selected) 0f else tilt
                translationY = if (selected) -4.dp.toPx() else 0f
            }
            .semantics { this.selected = selected },
        shape = RoundedCornerShape(7.dp),
        fill = PaperLight,
        border = if (selected) Red else Ink.copy(alpha = 0.25f),
    ) {
        Column(horizontalAlignment = Alignment.CenterHorizontally) {
            Txt(title, size = 22, weight = if (selected) FontWeight.W600 else FontWeight.W500)
            Txt(caption, size = 15, opacity = 0.65f)
        }
    }
}

// --- rooms ---

@Composable
private fun RoomsScreen(controller: GameController) {
    var waited by remember { mutableStateOf(false) }
    LaunchedEffect(Unit) {
        delay(1_500)
        waited = true
    }
    LaunchedEffect(Unit) {
        while (true) {
            delay(ROOM_POLL_MS)
            controller.refreshRooms()
        }
    }
    val open = controller.rooms.filter { it.members.size < 4 }
    PaperScreen {
        Column(Modifier.fillMaxSize().padding(vertical = 18.dp), horizontalAlignment = Alignment.CenterHorizontally) {
            Txt("Open tables", size = 26, weight = FontWeight.W500)
            Spacer(Modifier.size(14.dp))
            Box(Modifier.weight(1f).fillMaxWidth(0.88f).widthIn(max = 560.dp), contentAlignment = Alignment.TopCenter) {
                if (open.isEmpty()) {
                    Txt(if (waited) "no open tables — create one" else "looking for tables…", size = 19, opacity = 0.6f, italic = true)
                } else {
                    LazyColumn(verticalArrangement = Arrangement.spacedBy(8.dp)) {
                        items(open, key = { it.name }) { room ->
                            RoomRow(room, joinable = controller.uiModel.phase == GamePhase.CONNECTING || controller.uiModel.phase == GamePhase.IDLE) {
                                controller.join(room.name)
                            }
                        }
                    }
                }
            }
            StatusNote(controller.statusText.takeUnless { it == "Connecting…" }.orEmpty())
            Spacer(Modifier.size(8.dp))
            Row(horizontalArrangement = Arrangement.spacedBy(12.dp), verticalAlignment = Alignment.CenterVertically) {
                LinkButton("back", { controller.closeRooms() }, Modifier.testTag("rooms-back"), size = 18)
                PaperButton("Refresh", { controller.refreshRooms() }, Modifier.testTag("rooms-refresh"))
            }
        }
    }
}

@Composable
private fun RoomRow(room: RoomInfo, joinable: Boolean, onJoin: () -> Unit) {
    val shape = RoundedCornerShape(4.dp)
    Row(
        Modifier
            .fillMaxWidth()
            .background(PaperLight.copy(alpha = 0.92f), shape)
            .border(1.dp, Ink.copy(alpha = 0.2f), shape)
            .padding(horizontal = 12.dp, vertical = 7.dp),
        verticalAlignment = Alignment.CenterVertically,
        horizontalArrangement = Arrangement.spacedBy(10.dp),
    ) {
        Column(Modifier.weight(1f)) {
            Txt(room.name, size = 18, align = TextAlign.Start, maxLines = 1)
            Txt("${room.members.size} of 4 · to ${room.target}", size = 16, opacity = 0.6f, align = TextAlign.Start)
        }
        PaperButton("Join", onJoin, Modifier.testTag("room-join"), enabled = joinable, size = 18)
    }
}

// --- standings ---

@Composable
private fun StandingsScreen(rows: List<LeaderboardRow>?, onClose: () -> Unit) {
    PaperScreen {
        Panel(Modifier.fillMaxWidth(0.86f).widthIn(max = 460.dp).heightIn(max = 620.dp)) {
            Txt("Standings", size = 26, weight = FontWeight.W500)
            Spacer(Modifier.size(12.dp))
            Column(Modifier.weight(1f, fill = false).verticalScroll(rememberScrollState())) {
                when {
                    rows == null -> Txt("fetching…", Modifier.padding(4.dp), size = 19, opacity = 0.55f, italic = true)
                    rows.isEmpty() -> Txt("no completed matches yet — finish one!", Modifier.padding(4.dp), size = 19, opacity = 0.55f, italic = true)
                    else -> rows.forEachIndexed { index, row -> StandingRow(index, row, index == rows.lastIndex) }
                }
            }
            Spacer(Modifier.size(14.dp))
            PaperButton("Close", onClose, Modifier.testTag("standings-close"))
        }
    }
}

@Composable
private fun StandingRow(index: Int, row: LeaderboardRow, last: Boolean) {
    Column {
        Row(Modifier.fillMaxWidth().padding(vertical = 5.dp, horizontal = 2.dp), horizontalArrangement = Arrangement.spacedBy(12.dp), verticalAlignment = Alignment.CenterVertically) {
            Txt("${index + 1}.", Modifier.widthIn(min = 24.dp), size = 19, opacity = 0.5f, align = TextAlign.Start)
            Txt(row.name, Modifier.weight(1f), size = 19, align = TextAlign.Start, maxLines = 1)
            Txt("${row.won} of ${row.played} won", size = 17, opacity = 0.65f)
        }
        if (!last) Hairline()
    }
}

@Composable
private fun Hairline() {
    Box(Modifier.fillMaxWidth().size(width = 0.dp, height = 1.dp).background(Ink.copy(alpha = 0.12f)))
}

// --- connecting ---

@Composable
private fun ConnectingScreen(controller: GameController) {
    PaperScreen {
        Stack(gap = 14.dp) {
            Txt("28", size = 64, weight = FontWeight.W500, spacing = 4f)
            StatusNote(controller.statusText.ifEmpty { "Connecting…" })
            LinkButton("back", { controller.goHome() }, Modifier.testTag("connecting-back"))
        }
    }
}

// --- lobby ---

/** People and bots sit down at the table instead of joining a list. You're always at the near side. */
@Composable
private fun LobbyScreen(controller: GameController, shareInvite: (String) -> Unit) {
    val model = controller.uiModel
    val kind = if (model.matchTarget == 2) "Quick" else "Classic"
    val full = model.members.size == 4
    // Going round from our own seat, as the deal will. Before the server has seated anyone it's only us.
    val table = model.lobbySeats ?: listOf(controller.selfEmail, null, null, null)
    val me = table.indexOf(controller.selfEmail).coerceAtLeast(0)
    val around = List(4) { table[(me + it) % 4] }
    val host = model.members.lastOrNull()
    // A tap shows at once, and asks only once, however slow the answer is.
    var asked by remember { mutableStateOf(emptySet<Int>()) }
    LaunchedEffect(model.members.size) { asked = emptySet() }

    @Composable
    fun seat(view: Int, modifier: Modifier) {
        val member = around[view]
        if (member == null) {
            val number = (me + view) % 4
            OpenSeat(model.isHost, !full, number in asked, modifier) {
                if (number !in asked) {
                    asked = asked + number
                    controller.addBot(number)
                }
            }
        } else {
            TakenSeat(
                playerProfile(member, model.members),
                when {
                    member == controller.selfEmail -> "you"
                    member == host -> "host"
                    else -> null
                },
                if (model.isHost && member != controller.selfEmail) ({ controller.kick(member) }) else null,
                modifier,
            )
        }
    }

    PaperScreen {
        BoxWithConstraints(Modifier.fillMaxSize()) {
            val wide = maxWidth > maxHeight
            Column(
                Modifier.fillMaxSize().verticalScroll(rememberScrollState()).padding(vertical = 10.dp),
                horizontalAlignment = Alignment.CenterHorizontally,
            ) {
                Txt(model.roomName.orEmpty(), Modifier.padding(horizontal = 20.dp), size = 26, weight = FontWeight.W500, maxLines = 1)
                Txt("$kind, first to ${model.matchTarget}", size = 17, opacity = 0.65f)
                Spacer(Modifier.size(8.dp))
                if (wide) {
                    Row(horizontalArrangement = Arrangement.spacedBy(14.dp)) {
                        listOf(3, 2, 1, 0).forEach { seat(it, Modifier) }
                    }
                } else {
                    Box(
                        Modifier.size(width = 330.dp, height = 400.dp).drawBehind {
                            drawOval(
                                Ink.copy(alpha = 0.18f),
                                topLeft = Offset(30.dp.toPx(), 40.dp.toPx()),
                                size = Size(size.width - 60.dp.toPx(), size.height - 80.dp.toPx()),
                                style = Stroke(1.4.dp.toPx(), pathEffect = PathEffect.dashPathEffect(floatArrayOf(7f, 9f))),
                            )
                        },
                    ) {
                        seat(2, Modifier.align(Alignment.TopCenter))
                        seat(3, Modifier.align(Alignment.CenterStart))
                        seat(1, Modifier.align(Alignment.CenterEnd))
                        seat(0, Modifier.align(Alignment.BottomCenter))
                    }
                }
                Spacer(Modifier.size(10.dp))
                if (model.isHost) {
                    if (full) {
                        BigSealButton("Start the hand", Modifier.testTag("start-game").padding(horizontal = 44.dp).widthIn(max = 330.dp).fillMaxWidth().height(62.dp), fontSize = 28) {
                            controller.startGame()
                        }
                    } else {
                        Txt("Waiting for ${4 - model.members.size} more", Modifier.testTag("start-game"), size = 21, opacity = 0.6f)
                    }
                } else {
                    Txt("Waiting for the host to start", size = 21, opacity = 0.6f)
                }
                Spacer(Modifier.size(6.dp))
                Row(horizontalArrangement = Arrangement.spacedBy(18.dp), verticalAlignment = Alignment.CenterVertically) {
                    LinkButton("Share invite link", { model.roomName?.let(shareInvite) }, Modifier.testTag("invite-button"), size = 18)
                    LinkButton(if (model.isHost) "Close table" else "Leave table", { controller.leaveTable() }, Modifier.testTag("leave-table"), size = 18)
                }
                // We are seated by now, so the join notice has outlived its moment.
                StatusNote(controller.statusText.takeUnless { it == "Joining the table…" || it.startsWith("Your table is ready") }.orEmpty())
            }
        }
    }
}

@Composable
private fun OpenSeat(canFill: Boolean, open: Boolean, waiting: Boolean, modifier: Modifier, onFill: () -> Unit) {
    val body = if (canFill && open) modifier.testTag("add-bot").clickable(role = Role.Button, onClick = onFill) else modifier
    Column(body.width(104.dp), horizontalAlignment = Alignment.CenterHorizontally) {
        Box(Modifier.size(72.dp), contentAlignment = Alignment.Center) {
            BrushedEnso(false, Modifier.alpha(0.42f))
            Txt(if (waiting) "…" else "+", size = 30, opacity = 0.6f, spacing = 0f)
        }
        Txt(if (waiting) "Seating…" else "Empty", size = 17, opacity = 0.7f, maxLines = 1)
        if (canFill && !waiting) Txt("add a bot", size = 14, opacity = 0.6f, maxLines = 1)
    }
}

@Composable
private fun TakenSeat(profile: PlayerProfile, tag: String?, onKick: (() -> Unit)?, modifier: Modifier) {
    val reduce = LocalReduceMotion.current
    val arrive = remember { Animatable(if (reduce) 1f else 0.2f) }
    LaunchedEffect(Unit) {
        if (!reduce) {
            arrive.animateTo(1.14f, tween(230))
            arrive.animateTo(1f, tween(130))
        }
    }
    Column(modifier.width(104.dp), horizontalAlignment = Alignment.CenterHorizontally) {
        Box(
            Modifier.size(72.dp).graphicsLayer {
                scaleX = arrive.value
                scaleY = arrive.value
            },
            contentAlignment = Alignment.Center,
        ) {
            BrushedEnso(false)
            if (profile.isBot) {
                BasicText(profile.avatar, style = TextStyle(fontSize = 28.sp, color = Color.Black))
            } else {
                Txt(profile.avatar, size = 26, weight = FontWeight.W600, color = accentColor(profile.accent), spacing = 0f)
            }
        }
        Txt(profile.displayName, size = 17, maxLines = 1)
        if (tag != null) Txt(tag, size = 14, opacity = 0.6f, maxLines = 1)
        if (onKick != null) {
            Box(Modifier.size(40.dp).testTag("kick").clickable(role = Role.Button, onClick = onKick), contentAlignment = Alignment.Center) {
                Box(Modifier.size(22.dp).border(1.dp, Red.copy(alpha = 0.4f), CircleShape), contentAlignment = Alignment.Center) {
                    BasicText("✕", style = TextStyle(color = Red, fontSize = 12.sp))
                }
            }
        }
    }
}

/** PlayerProfile's accents are CSS colours: `#rrggbb` for bots, `hsl(h s% l%)` for people. */
internal fun accentColor(css: String): Color {
    if (css.startsWith("#")) return Color(android.graphics.Color.parseColor(css))
    val parts = css.removePrefix("hsl(").removeSuffix(")").split(' ').map { it.removeSuffix("%").toFloatOrNull() }
    val hue = parts.getOrNull(0) ?: return Ink
    val saturation = parts.getOrNull(1) ?: return Ink
    val lightness = parts.getOrNull(2) ?: return Ink
    return Color.hsl(hue, saturation / 100f, lightness / 100f)
}

// --- holding screens ---

/** Deliberately not the start screen: its actions could fork a second game under the same identity. */
@Composable
private fun ReconnectingScreen(controller: GameController) {
    val pulse = rememberInfiniteTransition(label = "reconnect")
    val titleAlpha by pulse.animateFloat(1f, 0.45f, infiniteRepeatable(tween(1_100), RepeatMode.Reverse), label = "title")
    PaperScreen(Modifier.testTag("reconnecting-screen")) {
        Panel(Modifier.fillMaxWidth(0.86f).widthIn(max = 420.dp)) {
            Txt("Connection lost", Modifier.alpha(titleAlpha), size = 22, weight = FontWeight.W500)
            Spacer(Modifier.size(10.dp))
            Txt("Your match is still running — getting you back to the table…", size = 19, opacity = 0.85f)
            Spacer(Modifier.size(14.dp))
            PaperButton("Abandon the match", { controller.abandonInterrupted() }, Modifier.testTag("reconnecting-abandon"), color = Red)
        }
    }
}

@Composable
private fun UpdateRequiredScreen() {
    val context = LocalContext.current
    PaperScreen(Modifier.testTag("update-screen")) {
        Stack(Modifier.padding(horizontal = 28.dp), gap = 12.dp) {
            Txt("28", size = 64, weight = FontWeight.W500, spacing = 4f)
            Txt("Time to update", size = 24, weight = FontWeight.W500)
            Txt("This version of the game can no longer reach the tables. Update it to keep playing.", size = 19, opacity = 0.8f)
            Spacer(Modifier.size(6.dp))
            PaperButton("Open the store", { openStore(context) }, Modifier.testTag("update-store"), size = 19, spacing = 1f, horizontal = 30.dp)
        }
    }
}

private fun openStore(context: Context) {
    fun open(link: String): Boolean = try {
        context.startActivity(Intent(Intent.ACTION_VIEW, Uri.parse(link)))
        true
    } catch (_: ActivityNotFoundException) {
        false
    }
    // Phones without a store app still have a browser.
    val id = BuildConfig.APPLICATION_ID
    if (!open("market://details?id=$id")) open("https://play.google.com/store/apps/details?id=$id")
}
