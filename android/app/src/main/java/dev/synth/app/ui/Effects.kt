package dev.synth.app.ui

import android.graphics.BitmapFactory
import android.view.HapticFeedbackConstants
import androidx.compose.animation.core.Animatable
import androidx.compose.animation.core.FastOutSlowInEasing
import androidx.compose.animation.core.LinearEasing
import androidx.compose.animation.core.tween
import androidx.compose.foundation.Canvas
import androidx.compose.foundation.Image
import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.layout.size
import androidx.compose.foundation.layout.width
import androidx.compose.foundation.layout.widthIn
import androidx.compose.runtime.Composable
import androidx.compose.runtime.LaunchedEffect
import androidx.compose.runtime.compositionLocalOf
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.remember
import androidx.compose.runtime.setValue
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.alpha
import androidx.compose.ui.draw.drawBehind
import androidx.compose.ui.draw.drawWithContent
import androidx.compose.ui.geometry.Offset
import androidx.compose.ui.geometry.Size
import androidx.compose.ui.graphics.BlendMode
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.CompositingStrategy
import androidx.compose.ui.graphics.ImageBitmap
import androidx.compose.ui.graphics.Path
import androidx.compose.ui.graphics.asImageBitmap
import androidx.compose.ui.graphics.drawscope.Stroke
import androidx.compose.ui.graphics.graphicsLayer
import androidx.compose.ui.graphics.StrokeCap
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.platform.LocalView
import androidx.compose.ui.res.painterResource
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.unit.Dp
import androidx.compose.ui.unit.IntOffset
import androidx.compose.ui.unit.IntSize
import androidx.compose.ui.unit.dp
import dev.synth.app.R
import dev.synth.core.Suit
import kotlinx.coroutines.launch
import kotlin.math.PI
import kotlin.math.cos
import kotlin.math.pow
import kotlin.math.sin
import kotlin.random.Random

/** Whether the player asked for less motion. Effects keep their information and drop the movement. */
internal val LocalReduceMotion = compositionLocalOf { false }

/** The short vibration behind a thump. Silent where the phone's settings say so. */
@Composable
internal fun rememberThump(): () -> Unit {
    val view = LocalView.current
    return remember(view) { { view.performHapticFeedback(HapticFeedbackConstants.CONTEXT_CLICK) } }
}

/**
 * The red ink seal: a rough-edged block that marks every big moment. The edge is a rounded box with its outline
 * nudged by noise, from a seed that belongs to the text, so a seal doesn't change shape when it redraws.
 */
internal fun sealOutline(size: Size, seed: Int, corner: Float, roughness: Float): Path {
    val random = Random(seed)
    val path = Path()
    val w = size.width
    val h = size.height
    val step = roughness * 2.6f
    var first = true
    fun point(x: Float, y: Float) {
        val px = x + (random.nextFloat() - 0.5f) * roughness
        val py = y + (random.nextFloat() - 0.5f) * roughness
        if (first) path.moveTo(px, py) else path.lineTo(px, py)
        first = false
    }
    fun side(x0: Float, y0: Float, x1: Float, y1: Float) {
        val length = kotlin.math.hypot(x1 - x0, y1 - y0)
        val count = (length / step).toInt().coerceAtLeast(1)
        for (i in 0 until count) point(x0 + (x1 - x0) * i / count, y0 + (y1 - y0) * i / count)
    }
    side(corner, 0f, w - corner, 0f)
    side(w, corner, w, h - corner)
    side(w - corner, h, corner, h)
    side(0f, h - corner, 0f, corner)
    path.close()
    return path
}

@Composable
internal fun Seal(
    text: String,
    modifier: Modifier = Modifier,
    size: Int = 22,
    horizontal: Dp = 10.dp,
    vertical: Dp = 5.dp,
    tilt: Float = -6f,
    thump: Boolean = true,
    onLand: () -> Unit = {},
) {
    val reduce = LocalReduceMotion.current
    val scale = remember { Animatable(if (thump && !reduce) 2.6f else 1f) }
    val alpha = remember { Animatable(if (thump && !reduce) 0f else 1f) }
    LaunchedEffect(Unit) {
        if (!thump) return@LaunchedEffect
        if (reduce) {
            onLand()
            return@LaunchedEffect
        }
        alpha.animateTo(1f, tween(110))
    }
    LaunchedEffect(Unit) {
        if (!thump || reduce) return@LaunchedEffect
        scale.animateTo(0.92f, tween(170, easing = FastOutSlowInEasing))
        onLand()
        scale.animateTo(1.05f, tween(70))
        scale.animateTo(1f, tween(70))
    }
    Box(
        modifier
            .graphicsLayer {
                rotationZ = tilt
                scaleX = scale.value
                scaleY = scale.value
                this.alpha = alpha.value
            }
            .drawBehind {
                val outline = sealOutline(this.size, text.hashCode(), 7.dp.toPx(), 1.6.dp.toPx())
                drawPath(outline, Red)
            }
            .padding(horizontal = horizontal, vertical = vertical),
        contentAlignment = Alignment.Center,
    ) {
        Txt(text, size = size, weight = FontWeight.W700, color = PaperLight, spacing = 0f, maxLines = 1)
    }
}

// Where enso.png's stroke begins: its heavy end, at the bottom, running clockwise.
private const val ENSO_STROKE_START = 92f
private const val ENSO_REST_ALPHA = 0.62f
private const val ENSO_INK_ALPHA = 0.9f

/**
 * The ensō always rests on the table. When its player's turn comes, fresh ink follows the brush round the stroke
 * from where it really starts, and the ring then breathes gently until the turn passes.
 */
@Composable
internal fun BrushedEnso(active: Boolean, modifier: Modifier = Modifier) {
    val reduce = LocalReduceMotion.current
    val sweep = remember { Animatable(1f) }
    val ink = remember { Animatable(0f) }
    val breath = remember { Animatable(1f) }
    LaunchedEffect(active, reduce) {
        if (!active) {
            launch { ink.animateTo(0f, tween(320)) }
            breath.animateTo(1f, tween(320))
            return@LaunchedEffect
        }
        ink.snapTo(1f)
        if (reduce) {
            sweep.snapTo(1f)
            breath.snapTo(1f)
            return@LaunchedEffect
        }
        sweep.snapTo(0f)
        sweep.animateTo(1f, tween(800, easing = FastOutSlowInEasing))
        while (true) {
            breath.animateTo(1.03f, tween(1_400, easing = FastOutSlowInEasing))
            breath.animateTo(1f, tween(1_400, easing = FastOutSlowInEasing))
        }
    }
    val art = painterResource(R.drawable.enso)
    Box(
        modifier.fillMaxSize().graphicsLayer {
            scaleX = breath.value
            scaleY = breath.value
        },
    ) {
        Image(art, contentDescription = null, Modifier.fillMaxSize().alpha(ENSO_REST_ALPHA))
        Image(
            art,
            contentDescription = null,
            Modifier
                .fillMaxSize()
                .graphicsLayer {
                    alpha = ink.value * ENSO_INK_ALPHA
                    compositingStrategy = CompositingStrategy.Offscreen
                }
                .drawWithContent {
                    drawContent()
                    if (sweep.value < 1f) {
                        // Rubs out what the brush hasn't reached.
                        drawArc(
                            Color.Black,
                            ENSO_STROKE_START + sweep.value * 360f,
                            (1f - sweep.value) * 360f,
                            true,
                            topLeft = Offset(-size.width / 2, -size.height / 2),
                            size = Size(size.width * 2, size.height * 2),
                            blendMode = BlendMode.DstOut,
                        )
                    }
                },
        )
    }
}

/** Ink flung from a point, which then fades. It never takes touches. */
@Composable
internal fun InkBurst(key: Any, x: Float, y: Float, color: Color, modifier: Modifier = Modifier, drops: Int = 70, spread: Dp = 260.dp) {
    if (LocalReduceMotion.current) return
    val progress = remember(key) { Animatable(0f) }
    LaunchedEffect(key) { progress.animateTo(1f, tween(1_500, easing = LinearEasing)) }
    val seeds = remember(key) {
        val random = Random(key.hashCode())
        List(drops) { Triple(random.nextFloat() * 2f * PI.toFloat(), 0.25f + random.nextFloat().pow(1.7f), 1f + random.nextFloat().pow(3f) * 12f) }
    }
    val blob = remember(key) {
        val random = Random(key.hashCode() + 1)
        List(14) { 22f + random.nextFloat() * 16f }
    }
    Canvas(modifier.fillMaxSize()) {
        val p = progress.value
        if (p >= 1f) return@Canvas
        val outward = 1f - (1f - (p / 0.3f).coerceAtMost(1f)).pow(3)
        val fade = if (p < 0.55f) 1f else 1f - (p - 0.55f) / 0.45f
        val ink = color.copy(alpha = color.alpha * fade)
        val cx = size.width * x
        val cy = size.height * y
        val px = spread.toPx()
        val unit = 1.dp.toPx()
        val points = blob.mapIndexed { i, r ->
            val angle = i / 14f * 2f * PI.toFloat()
            val radius = r * unit * outward * 1.3f
            Offset(cx + cos(angle) * radius, cy + sin(angle) * radius)
        }
        val path = Path()
        points.forEachIndexed { i, point ->
            val next = points[(i + 1) % points.size]
            if (i == 0) {
                val last = points.last()
                path.moveTo((last.x + point.x) / 2, (last.y + point.y) / 2)
            }
            path.quadraticBezierTo(point.x, point.y, (point.x + next.x) / 2, (point.y + next.y) / 2)
        }
        path.close()
        drawPath(path, ink)
        for ((angle, reach, radius) in seeds) {
            drawCircle(
                ink,
                radius * unit * (0.5f + 0.5f * outward),
                Offset(cx + cos(angle) * reach * px * outward, cy + sin(angle) * reach * px * outward),
            )
        }
    }
}

/** Real card art for the screens that have no table under them, cut from the table's own atlas. */
@Composable
internal fun rememberCardAtlas(): ImageBitmap? {
    val context = LocalContext.current
    return remember(context) {
        runCatching { context.assets.open("table/cards.png").use { BitmapFactory.decodeStream(it)?.asImageBitmap() } }.getOrNull()
    }
}

// The grid of table_atlas_uv in native/src/table.c.
private const val ATLAS_CELLS = 6
private const val ATLAS_PAD_X = 4f / 208
private const val ATLAS_PAD_Y = 4f / 308

/** One card by its suit (hearts, diamonds, clubs, spades) and rank in the atlas's order: J, 9, A, 10, K, Q, 8, 7. */
@Composable
internal fun AtlasCard(atlas: ImageBitmap, suit: Int, rank: Int, modifier: Modifier = Modifier) {
    val cell = suit * 8 + rank
    Canvas(modifier) {
        val w = atlas.width.toFloat() / ATLAS_CELLS
        val h = atlas.height.toFloat() / ATLAS_CELLS
        val col = cell % ATLAS_CELLS
        val row = cell / ATLAS_CELLS
        drawImage(
            atlas,
            srcOffset = IntOffset(((col + ATLAS_PAD_X) * w).toInt(), ((row + ATLAS_PAD_Y) * h).toInt()),
            srcSize = IntSize((w * (1 - 2 * ATLAS_PAD_X)).toInt(), (h * (1 - 2 * ATLAS_PAD_Y)).toInt()),
            dstSize = IntSize(size.width.toInt(), size.height.toInt()),
        )
    }
}

/** A card that shows a suit on one side and the card back on the other, turned by `turn` (0 face up, 1 face down). */
@Composable
internal fun SuitCard(suit: Suit, turn: Float, modifier: Modifier = Modifier, glyphSize: Int = 30) {
    val faceDown = turn > 0.5f
    Box(
        modifier
            .graphicsLayer {
                rotationY = turn * 180f
                cameraDistance = 12f * density
            },
        contentAlignment = Alignment.Center,
    ) {
        if (faceDown) {
            Image(
                painterResource(R.drawable.card_back),
                contentDescription = null,
                Modifier.fillMaxSize().graphicsLayer { rotationY = 180f },
            )
        } else {
            Box(
                Modifier.fillMaxSize().drawBehind {
                    drawRoundRect(PaperLight, cornerRadius = androidx.compose.ui.geometry.CornerRadius(5.dp.toPx()))
                    drawRoundRect(Ink.copy(alpha = 0.18f), cornerRadius = androidx.compose.ui.geometry.CornerRadius(5.dp.toPx()), style = Stroke(1.dp.toPx()))
                },
                contentAlignment = Alignment.Center,
            ) { SuitGlyph(suit, glyphSize) }
        }
    }
}

/** The match score as ink pips, one for each point to the target, with the number beside them: a score below zero has no pip to fill. */
@Composable
internal fun Pips(label: String, score: Int, target: Int, color: Color, modifier: Modifier = Modifier) {
    val pulse = score
    val pop = remember { Animatable(1f) }
    var seen by remember { mutableStateOf(pulse) }
    LaunchedEffect(pulse) {
        if (pulse != seen) {
            seen = pulse
            pop.snapTo(2.2f)
            pop.animateTo(1f, tween(320))
        }
    }
    // Three fixed columns, so the two rows line up whatever the scores are.
    androidx.compose.foundation.layout.Row(
        modifier,
        verticalAlignment = Alignment.CenterVertically,
        horizontalArrangement = androidx.compose.foundation.layout.Arrangement.spacedBy(4.dp),
    ) {
        // At least this wide, and wider for large type. The rows end level, so the pips still line up.
        Txt(label, Modifier.widthIn(min = 44.dp).padding(end = 3.dp), size = 14, opacity = 0.7f, spacing = 0f, align = TextAlign.End, maxLines = 1)
        for (i in 0 until target) {
            val filled = i < score
            val last = filled && i == score - 1
            Canvas(Modifier.size(11.dp).graphicsLayer { if (last) { scaleX = pop.value; scaleY = pop.value } }) {
                if (filled) drawCircle(color) else drawCircle(Ink.copy(alpha = 0.45f), style = Stroke(1.2.dp.toPx()))
            }
        }
        Txt(
            if (score < 0) "−${-score}" else "$score",
            Modifier.width(24.dp),
            size = 15,
            weight = FontWeight.W600,
            color = color,
            spacing = 0f,
            align = TextAlign.End,
            maxLines = 1,
        )
    }
}

/** The team's card points as a brush stroke, with a notch at the bid they must reach. */
@Composable
internal fun BrushLine(points: Int, notch: Int?, modifier: Modifier = Modifier) {
    val shown = remember { Animatable(points.toFloat()) }
    LaunchedEffect(points) { shown.animateTo(points.toFloat(), tween(420)) }
    Canvas(modifier) {
        val h = size.height
        fun stroke(end: Float, color: Color) {
            val path = Path()
            path.moveTo(h / 2, h * 0.55f)
            path.cubicTo(size.width * 0.25f, h * 0.42f, size.width * 0.7f, h * 0.68f, end, h * 0.5f)
            drawPath(path, color, style = Stroke(h * 0.38f, cap = StrokeCap.Round))
        }
        stroke(size.width - h / 2, Ink.copy(alpha = 0.14f))
        val amount = (shown.value / 28f).coerceIn(0f, 1f)
        if (amount > 0f) stroke(h / 2 + (size.width - h) * amount, Red)
        if (notch != null) {
            val x = h / 2 + (size.width - h) * (notch / 28f).coerceIn(0f, 1f)
            drawLine(Ink, Offset(x, 0f), Offset(x, h), 1.3.dp.toPx())
        }
    }
}
