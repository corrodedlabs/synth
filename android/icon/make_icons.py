"""Draws every app icon from the app's own ensō and font: the Android adaptive icon, the Play Store
icon and feature graphic, and the website's icons. Needs Pillow. Run from the repository root:

    python3 android/icon/make_icons.py
"""
from pathlib import Path
from PIL import Image, ImageChops, ImageDraw, ImageFont

ROOT = Path(__file__).resolve().parents[2]
RES = ROOT / "android/app/src/main/res"
FONT = RES / "font/cormorant_garamond.ttf"
ENSO = RES / "drawable-nodpi/enso.png"
OUT_STORE = ROOT / "android/icon/store"
OUT_WEB = ROOT / "app/public/icons"

INK = (0x3B, 0x2F, 0x2A)
RED = (0xA3, 0x3C, 0x2E)
PAPER = (0xF7, 0xF1, 0xE7)


def font(size, weight="Bold"):
    f = ImageFont.truetype(str(FONT), size)
    f.set_variation_by_name(weight)
    return f


def enso_alpha(size):
    """The brush circle as an alpha mask: dark paint becomes opaque."""
    image = Image.open(ENSO).convert("RGBA").resize((size, size), Image.LANCZOS)
    return ImageChops.multiply(ImageChops.invert(image.convert("L")), image.getchannel("A"))


def mark(size, enso_color=RED, text_color=INK):
    """The ensō with "28" inside, on a transparent square. It fills the square edge to edge."""
    image = Image.new("RGBA", (size, size), (0, 0, 0, 0))
    ring = Image.new("RGBA", (size, size), enso_color + (255,))
    ring.putalpha(enso_alpha(size))
    image.alpha_composite(ring)
    ImageDraw.Draw(image).text(
        (size / 2, size / 2 + size * 0.013), "28", font=font(round(size * 0.5)), fill=text_color, anchor="mm"
    )
    return image


def on_paper(size, mark_fraction):
    image = Image.new("RGBA", (size, size), PAPER + (255,))
    m = round(size * mark_fraction)
    image.alpha_composite(mark(m), ((size - m) // 2, (size - m) // 2))
    return image


def save_rgb(image, path):
    path.parent.mkdir(parents=True, exist_ok=True)
    image.convert("RGB").save(path, optimize=True)


def android():
    # Adaptive icon layers are 108 dp. Launchers crop to at most a 66 dp circle, so the mark stays inside it.
    densities = {"mdpi": 1, "hdpi": 1.5, "xhdpi": 2, "xxhdpi": 3, "xxxhdpi": 4}
    for name, scale in densities.items():
        px = round(108 * scale)
        m = round(62 * scale)
        offset = ((px - m) // 2, (px - m) // 2)
        folder = RES / f"mipmap-{name}"
        folder.mkdir(exist_ok=True)
        foreground = Image.new("RGBA", (px, px), (0, 0, 0, 0))
        foreground.alpha_composite(mark(m), offset)
        foreground.save(folder / "ic_launcher_foreground.png", optimize=True)
        # Themed icons (Android 13+) use only the alpha of this layer and tint it with the wallpaper colour.
        mono = Image.new("RGBA", (px, px), (0, 0, 0, 0))
        mono.alpha_composite(mark(m, (255, 255, 255), (255, 255, 255)), offset)
        mono.save(folder / "ic_launcher_monochrome.png", optimize=True)


def store():
    # Play's icon is a full square; Play rounds the corners itself.
    save_rgb(on_paper(512, 0.74), OUT_STORE / "play-icon-512.png")
    graphic = Image.new("RGBA", (1024, 500), PAPER + (255,))
    graphic.alpha_composite(mark(340), (70, 80))
    draw = ImageDraw.Draw(graphic)
    left, right = 470, 1024 - 60
    lines = [("Twenty-Eight", "Bold", INK, 196), ("The classic trick-taking card game", "Medium", INK, 272),
             ("Play with friends or bots", "Medium", RED, 322)]
    # Each line takes the biggest size that fits between the mark and the right margin, capped per line.
    for (text, weight, color, y), cap in zip(lines, (84, 34, 34)):
        size = cap
        while draw.textlength(text, font=font(size, weight)) > right - left:
            size -= 1
        draw.text((left, y), text, font=font(size, weight), fill=color, anchor="lm")
    save_rgb(graphic, OUT_STORE / "feature-graphic-1024x500.png")


def web():
    # Maskable web icons are cropped to a circle of 80% of the square, so the same 74% mark fits both uses.
    for size in (180, 192, 512):
        save_rgb(on_paper(size, 0.74), OUT_WEB / f"icon-{size}.png")
    save_rgb(on_paper(64, 0.9), OUT_WEB / "favicon-64.png")


if __name__ == "__main__":
    android()
    store()
    web()
