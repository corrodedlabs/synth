package dev.synth.app.ui

import androidx.compose.animation.core.Animatable
import androidx.compose.animation.core.FastOutSlowInEasing
import androidx.compose.animation.core.tween
import androidx.compose.foundation.background
import androidx.compose.foundation.border
import androidx.compose.foundation.clickable
import androidx.compose.foundation.interaction.MutableInteractionSource
import androidx.compose.foundation.interaction.collectIsPressedAsState
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.BoxScope
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.ColumnScope
import androidx.compose.foundation.layout.defaultMinSize
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.foundation.text.BasicText
import androidx.compose.runtime.Composable
import androidx.compose.runtime.LaunchedEffect
import androidx.compose.runtime.getValue
import androidx.compose.runtime.remember
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.alpha
import androidx.compose.ui.draw.drawBehind
import androidx.compose.ui.draw.shadow
import androidx.compose.ui.geometry.Offset
import androidx.compose.ui.graphics.Brush
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.Shape
import androidx.compose.ui.graphics.graphicsLayer
import androidx.compose.ui.input.pointer.pointerInput
import androidx.compose.ui.semantics.Role
import androidx.compose.ui.text.ExperimentalTextApi
import androidx.compose.ui.text.TextStyle
import androidx.compose.ui.text.font.Font
import androidx.compose.ui.text.font.FontFamily
import androidx.compose.ui.text.font.FontStyle
import androidx.compose.ui.text.font.FontVariation
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.unit.Dp
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import dev.synth.app.R

// The web overlay's palette and shapes (app/index.html), kept as one small kit so no screen reaches for Material.
internal val Ink = Color(0xFF3B2F2A)
internal val Red = Color(0xFFA33C2E)
internal val Green = Color(0xFF2F6035)
internal val PassBrown = Color(0xFF765C35)
internal val PaperLight = Color(0xFFFDFAF3)
internal val PaperMid = Color(0xFFF7F1E7)
internal val PaperDark = Color(0xFFEFE5D8)
internal val Gold = Color(0xFF856832)
internal val PressTint = Ink.copy(alpha = 0.07f)

@OptIn(ExperimentalTextApi::class)
internal val Cormorant = FontFamily(
    Font(R.font.cormorant_garamond, FontWeight.W400, variationSettings = FontVariation.Settings(FontVariation.weight(400))),
    Font(R.font.cormorant_garamond, FontWeight.W500, variationSettings = FontVariation.Settings(FontVariation.weight(500))),
    Font(R.font.cormorant_garamond, FontWeight.W600, variationSettings = FontVariation.Settings(FontVariation.weight(600))),
)

internal fun serif(
    size: Int,
    weight: FontWeight = FontWeight.W400,
    color: Color = Ink,
    spacing: Float = 0.4f,
    italic: Boolean = false,
    align: TextAlign = TextAlign.Center,
) = TextStyle(
    fontFamily = Cormorant,
    fontSize = size.sp,
    fontWeight = weight,
    color = color,
    letterSpacing = spacing.sp,
    fontStyle = if (italic) FontStyle.Italic else FontStyle.Normal,
    fontFeatureSettings = "lnum",
    textAlign = align,
)

@Composable
internal fun Txt(
    text: String,
    modifier: Modifier = Modifier,
    size: Int = 18,
    weight: FontWeight = FontWeight.W400,
    color: Color = Ink,
    opacity: Float = 1f,
    spacing: Float = 0.4f,
    italic: Boolean = false,
    align: TextAlign = TextAlign.Center,
    maxLines: Int = Int.MAX_VALUE,
) {
    BasicText(
        text = text,
        modifier = modifier,
        style = serif(size, weight, color.copy(alpha = color.alpha * opacity), spacing, italic, align),
        maxLines = maxLines,
        overflow = if (maxLines == Int.MAX_VALUE) TextOverflow.Clip else TextOverflow.Ellipsis,
    )
}

/** A panel or paper screen owns its touches. Everything else is left for the table underneath. */
internal fun Modifier.takesTouches(): Modifier = pointerInput(Unit) {
    awaitPointerEventScope { while (true) awaitPointerEvent() }
}

/** The body background: warm paper, lighter around the upper middle. */
internal fun Modifier.paperBackground(): Modifier = drawBehind {
    val radius = maxOf(size.width, size.height) * 0.75f
    drawRect(
        Brush.radialGradient(
            0f to PaperLight,
            0.6f to PaperMid,
            1f to PaperDark,
            center = Offset(size.width / 2, size.height * 0.3f),
            radius = radius,
        ),
    )
}

/** Panels and results settle in gently; a modifier only, so it never affects touches. */
@Composable
internal fun Modifier.entrance(): Modifier {
    val progress = remember { Animatable(0f) }
    LaunchedEffect(Unit) { progress.animateTo(1f, tween(320, easing = FastOutSlowInEasing)) }
    return graphicsLayer {
        alpha = progress.value
        val scale = 0.94f + 0.06f * progress.value
        scaleX = scale
        scaleY = scale
    }
}

private val PanelShape = RoundedCornerShape(6.dp)

/** `.panel`: translucent paper, a hairline border, a soft shadow. */
@Composable
internal fun Panel(
    modifier: Modifier = Modifier,
    fill: Color = PaperLight.copy(alpha = 0.96f),
    padding: Dp = 16.dp,
    content: @Composable ColumnScope.() -> Unit,
) {
    Column(
        modifier
            .shadow(10.dp, PanelShape, clip = false, ambientColor = Ink, spotColor = Ink.copy(alpha = 0.5f))
            .background(fill, PanelShape)
            .border(1.dp, Ink.copy(alpha = 0.18f), PanelShape)
            .takesTouches()
            .padding(padding),
        horizontalAlignment = Alignment.CenterHorizontally,
        content = content,
    )
}

/** The shared press behaviour: no ripple, only the web's soft ink tint. */
@Composable
internal fun Pressable(
    onClick: () -> Unit,
    modifier: Modifier = Modifier,
    enabled: Boolean = true,
    shape: Shape = RoundedCornerShape(4.dp),
    fill: Color = Color.Transparent,
    border: Color = Ink.copy(alpha = 0.45f),
    contentAlignment: Alignment = Alignment.Center,
    content: @Composable BoxScope.() -> Unit,
) {
    val source = remember { MutableInteractionSource() }
    val pressed by source.collectIsPressedAsState()
    Box(
        modifier
            .alpha(if (enabled) 1f else 0.35f)
            .background(fill, shape)
            .background(if (pressed) PressTint else Color.Transparent, shape)
            .border(1.dp, border, shape)
            .clickable(source, indication = null, enabled = enabled, role = Role.Button, onClick = onClick),
        contentAlignment = contentAlignment,
        content = content,
    )
}

/** `.panel button` */
@Composable
internal fun PaperButton(
    text: String,
    onClick: () -> Unit,
    modifier: Modifier = Modifier,
    enabled: Boolean = true,
    size: Int = 18,
    color: Color = Ink,
    fill: Color = Color.Transparent,
    spacing: Float = 0.4f,
    horizontal: Dp = 16.dp,
) {
    val border = if (color == Ink) Ink.copy(alpha = 0.45f) else color.copy(alpha = 0.45f)
    Pressable(onClick, modifier.defaultMinSize(minHeight = 44.dp), enabled, fill = fill, border = border) {
        Txt(text, Modifier.padding(horizontal = horizontal, vertical = 6.dp), size = size, color = color, spacing = spacing)
    }
}

/** The quiet underlined actions: standings, share invite, back. */
@Composable
internal fun LinkButton(text: String, onClick: () -> Unit, modifier: Modifier = Modifier, size: Int = 16) {
    val source = remember { MutableInteractionSource() }
    val pressed by source.collectIsPressedAsState()
    Box(
        modifier
            .defaultMinSize(minHeight = 44.dp)
            .clickable(source, indication = null, role = Role.Button, onClick = onClick)
            .padding(horizontal = 8.dp),
        contentAlignment = Alignment.Center,
    ) {
        Txt(
            text,
            Modifier.drawBehind {
                drawLine(
                    Ink.copy(alpha = 0.35f),
                    Offset(0f, this.size.height),
                    Offset(this.size.width, this.size.height),
                    strokeWidth = 1.dp.toPx(),
                )
            },
            size = size,
            opacity = if (pressed) 1f else 0.65f,
            spacing = 0.8f,
        )
    }
}

/** `#sound-button, #leave-match`: the corner chips. */
@Composable
internal fun Chip(
    text: String,
    onClick: () -> Unit,
    modifier: Modifier = Modifier,
    color: Color = Ink,
    armed: Boolean = false,
    size: Int = 15,
) {
    Pressable(
        onClick,
        modifier.defaultMinSize(minWidth = 40.dp, minHeight = 40.dp),
        shape = RoundedCornerShape(999.dp),
        fill = PaperLight.copy(alpha = 0.72f),
        border = if (armed) color.copy(alpha = 0.6f) else Ink.copy(alpha = 0.3f),
    ) {
        Txt(text, Modifier.padding(horizontal = 12.dp), size = size, color = color, opacity = if (armed) 1f else 0.8f, spacing = 0.8f, maxLines = 1)
    }
}

/** A short centred stack with the web's breathing room. */
@Composable
internal fun Stack(modifier: Modifier = Modifier, gap: Dp = 8.dp, content: @Composable ColumnScope.() -> Unit) {
    Column(modifier, Arrangement.spacedBy(gap), Alignment.CenterHorizontally, content)
}
