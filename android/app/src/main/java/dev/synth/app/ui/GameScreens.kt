package dev.synth.app.ui

import android.content.ActivityNotFoundException
import android.content.Context
import android.content.Intent
import android.net.Uri
import androidx.compose.animation.core.RepeatMode
import androidx.compose.animation.core.animateFloat
import androidx.compose.animation.core.infiniteRepeatable
import androidx.compose.animation.core.rememberInfiniteTransition
import androidx.compose.animation.core.tween
import androidx.compose.foundation.background
import androidx.compose.foundation.border
import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.BoxWithConstraints
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.Spacer
import androidx.compose.foundation.layout.WindowInsets
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
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.SolidColor
import androidx.compose.ui.graphics.lerp
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

private val MenuInsets: WindowInsets
    @Composable get() = WindowInsets.displayCutout.union(WindowInsets.ime)

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

@Composable
private fun StartScreen(controller: GameController, name: String, onName: (String) -> Unit, onStandings: () -> Unit) {
    var target by rememberSaveable { mutableStateOf(6) }
    val focus = LocalFocusManager.current
    val invite = controller.joinTarget
    PaperScreen {
        Column(
            Modifier.fillMaxWidth().verticalScroll(rememberScrollState()).padding(vertical = 18.dp),
            horizontalAlignment = Alignment.CenterHorizontally,
        ) {
            Txt("28", size = 64, weight = FontWeight.W500, spacing = 4f)
            Txt("a trick-taking game", size = 18, opacity = 0.75f)
            Spacer(Modifier.size(20.dp))
            NameField(name, onName, onDone = { focus.clearFocus() })
            Spacer(Modifier.size(18.dp))

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

            Txt("What kind of match?", size = 18, opacity = 0.75f)
            Spacer(Modifier.size(6.dp))
            Row(Modifier.widthIn(max = 330.dp), horizontalArrangement = Arrangement.spacedBy(8.dp)) {
                TargetChoice("Quick", "first to 2", target == 2, Modifier.weight(1f).testTag("target-quick")) { target = 2 }
                TargetChoice("Classic", "first to 6", target == 6, Modifier.weight(1f).testTag("target-classic")) { target = 6 }
            }
            Spacer(Modifier.size(14.dp))
            Stack(gap = 10.dp) {
                MenuButton("Create a table", Modifier.testTag("create-button")) {
                    focus.clearFocus()
                    controller.createTable(name, target)
                }
                MenuButton("Join a table", Modifier.testTag("browse-button")) {
                    focus.clearFocus()
                    controller.browse(name)
                }
            }
            Spacer(Modifier.size(10.dp))
            LinkButton("standings", { focus.clearFocus(); onStandings() }, Modifier.testTag("standings-button"), size = 17)
            Spacer(Modifier.size(8.dp))
            // goHome leaves the last status behind: an abandoned connection attempt says nothing true here.
            StatusNote(controller.statusText.takeUnless { it == "Connecting…" || it == "Rejoining your match…" }.orEmpty())
        }
    }
}

@Composable
private fun NameField(name: String, onName: (String) -> Unit, onDone: () -> Unit) {
    val shape = RoundedCornerShape(4.dp)
    val style = TextStyle(
        fontFamily = Cormorant,
        fontSize = 19.sp,
        color = Ink,
        letterSpacing = 0.6.sp,
        textAlign = TextAlign.Center,
    )
    CompositionLocalProvider(LocalTextSelectionColors provides TextSelectionColors(Ink, Ink.copy(alpha = 0.2f))) {
      BasicTextField(
        value = name,
        onValueChange = onName,
        modifier = Modifier.testTag("name-input").widthIn(min = 210.dp, max = 240.dp),
        singleLine = true,
        textStyle = style,
        cursorBrush = SolidColor(Ink),
        keyboardOptions = KeyboardOptions(capitalization = KeyboardCapitalization.Words, imeAction = ImeAction.Done),
        keyboardActions = KeyboardActions(onDone = { onDone() }),
        decorationBox = { field ->
            Box(
                Modifier
                    .background(PaperLight.copy(alpha = 0.9f), shape)
                    .border(1.dp, Ink.copy(alpha = 0.35f), shape)
                    .padding(horizontal = 14.dp, vertical = 10.dp),
                contentAlignment = Alignment.Center,
            ) {
                if (name.isEmpty()) BasicText("your name", style = style.copy(color = Ink.copy(alpha = 0.45f)))
                field()
            }
        },
      )
    }
}

@Composable
private fun TargetChoice(title: String, caption: String, selected: Boolean, modifier: Modifier, onClick: () -> Unit) {
    Pressable(
        onClick,
        modifier.semantics { this.selected = selected },
        fill = if (selected) PressTint else Color.Transparent,
        border = Ink.copy(alpha = if (selected) 0.7f else 0.3f),
    ) {
        Column(Modifier.padding(horizontal = 18.dp, vertical = 8.dp), horizontalAlignment = Alignment.CenterHorizontally) {
            Txt(title, size = 19, weight = if (selected) FontWeight.W600 else FontWeight.W400)
            Txt(caption, size = 14, opacity = 0.65f)
        }
    }
}

@Composable
private fun MenuButton(text: String, modifier: Modifier = Modifier, onClick: () -> Unit) {
    PaperButton(text, onClick, modifier.widthIn(min = 240.dp), size = 19, spacing = 2f, horizontal = 34.dp)
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

@Composable
private fun LobbyScreen(controller: GameController, shareInvite: (String) -> Unit) {
    val model = controller.uiModel
    val kind = if (model.matchTarget == 2) "quick match" else "match"
    val full = model.members.size == 4
    // Members arrive in server order (newest first, host last): show the host first.
    val seated = model.members.reversed()
    PaperScreen {
        BoxWithConstraints(Modifier.fillMaxSize(), contentAlignment = Alignment.Center) {
            val wide = maxWidth > maxHeight
            Column(
                Modifier.fillMaxWidth().verticalScroll(rememberScrollState()).padding(vertical = 12.dp),
                horizontalAlignment = Alignment.CenterHorizontally,
            ) {
                Panel(Modifier.fillMaxWidth(if (wide) 0.6f else 0.88f).widthIn(max = 480.dp), fill = PaperLight.copy(alpha = 0.98f)) {
                    Box(Modifier.fillMaxWidth()) {
                        LeaveTable(if (model.isHost) "Close table" else "Leave table", Modifier.align(Alignment.TopEnd)) {
                            controller.leaveTable()
                        }
                    }
                    Txt(model.roomName.orEmpty(), Modifier.padding(horizontal = 12.dp, vertical = 2.dp), size = 22, weight = FontWeight.W500)
                    Txt(
                        if (model.isHost) "You are hosting this table — $kind to ${model.matchTarget}"
                        else "Waiting for the host to start — $kind to ${model.matchTarget}",
                        size = 15,
                        opacity = 0.65f,
                    )
                    Spacer(Modifier.size(14.dp))
                    for (seat in 0 until 4) {
                        val member = seated.getOrNull(seat)
                        if (member == null) {
                            EmptySeat()
                        } else {
                            SeatRow(
                                profile = playerProfile(member, model.members),
                                tags = listOfNotNull(if (seat == 0) "host" else null, if (member == controller.selfEmail) "you" else null),
                                onKick = if (model.isHost && member != controller.selfEmail) ({ controller.kick(member) }) else null,
                            )
                        }
                        if (seat < 3) Hairline()
                    }
                    Spacer(Modifier.size(16.dp))
                    if (model.isHost) {
                        Row(horizontalArrangement = Arrangement.spacedBy(10.dp)) {
                            PaperButton("Add a bot", { controller.addBot() }, Modifier.testTag("add-bot"), enabled = !full)
                            PaperButton("Start game", { controller.startGame() }, Modifier.testTag("start-game"), enabled = full)
                        }
                        Spacer(Modifier.size(8.dp))
                        Txt(
                            if (full) "All seats taken — ready when you are."
                            else "Friends can join from their own phone, or fill seats with bots.",
                            size = 15,
                            opacity = 0.65f,
                        )
                    }
                    Spacer(Modifier.size(4.dp))
                    LinkButton("share invite link", { model.roomName?.let(shareInvite) }, Modifier.testTag("invite-button"))
                }
                Spacer(Modifier.size(12.dp))
                // We are seated by now, so the join notice has outlived its moment.
                StatusNote(controller.statusText.takeUnless { it == "Joining the table…" }.orEmpty())
            }
        }
    }
}

@Composable
private fun LeaveTable(text: String, modifier: Modifier, onClick: () -> Unit) {
    Pressable(onClick, modifier.testTag("leave-table").alpha(0.7f), border = Ink.copy(alpha = 0.25f)) {
        Txt(text, Modifier.padding(horizontal = 12.dp, vertical = 8.dp), size = 15)
    }
}

@Composable
private fun EmptySeat() {
    Box(Modifier.fillMaxWidth().heightIn(min = 46.dp).padding(horizontal = 4.dp), contentAlignment = Alignment.CenterStart) {
        Txt("empty seat", size = 18, opacity = 0.45f, italic = true)
    }
}

@Composable
private fun SeatRow(profile: PlayerProfile, tags: List<String>, onKick: (() -> Unit)?) {
    Row(
        Modifier.fillMaxWidth().heightIn(min = 46.dp).padding(horizontal = 4.dp, vertical = 3.dp),
        verticalAlignment = Alignment.CenterVertically,
        horizontalArrangement = Arrangement.spacedBy(8.dp),
    ) {
        Row(Modifier.weight(1f), verticalAlignment = Alignment.CenterVertically, horizontalArrangement = Arrangement.spacedBy(8.dp)) {
            Avatar(profile)
            Txt(profile.displayName, Modifier.weight(1f, fill = false), size = 18, align = TextAlign.Start, maxLines = 1)
            if (profile.isBot) BotTag(accentColor(profile.accent))
        }
        Txt(tags.joinToString(" · "), size = 16, opacity = 0.55f, maxLines = 1)
        if (onKick != null) {
            Box(
                Modifier.size(40.dp).testTag("kick").clickable(role = Role.Button, onClick = onKick),
                contentAlignment = Alignment.Center,
            ) {
                Box(Modifier.size(24.dp).border(1.dp, Red.copy(alpha = 0.4f), CircleShape), contentAlignment = Alignment.Center) {
                    BasicText("✕", style = TextStyle(color = Red, fontSize = 13.sp))
                }
            }
        }
    }
}

@Composable
private fun Avatar(profile: PlayerProfile) {
    val accent = accentColor(profile.accent)
    Box(
        Modifier
            .size(34.dp)
            .background(lerp(PaperLight, accent, 0.2f), CircleShape)
            .border(2.dp, lerp(PaperLight, accent, 0.7f), CircleShape),
        contentAlignment = Alignment.Center,
    ) {
        if (profile.isBot) {
            BasicText(profile.avatar, style = TextStyle(fontSize = 19.sp, color = Color.Black))
        } else {
            Txt(profile.avatar, size = 18, weight = FontWeight.W600, color = accent)
        }
    }
}

@Composable
private fun BotTag(accent: Color) {
    Box(Modifier.border(1.dp, accent.copy(alpha = 0.42f), RoundedCornerShape(999.dp)).padding(horizontal = 5.dp, vertical = 1.dp)) {
        BasicText("BOT", style = TextStyle(color = accent, fontSize = 9.sp, fontWeight = FontWeight.W700, letterSpacing = 0.8.sp))
    }
}

/** PlayerProfile's accents are CSS colours: `#rrggbb` for bots, `hsl(h s% l%)` for people. */
private fun accentColor(css: String): Color {
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
    if (!open("market://details?id=dev.synth.app")) open("https://play.google.com/store/apps/details?id=dev.synth.app")
}
