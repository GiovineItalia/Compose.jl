# Draw some unicodes to an image.
# https://github.com/GiovineItalia/Compose.jl/pull/360#issuecomment-539283765

using Compose
import Cairo, Fontconfig

c = compose(context(), text(0.5, 0.5, "𝑎 𝑏 𝑐 𝑑 𝑒 𝑓", hcenter, vcenter), font("Segoe UI"), fontsize(20pt) ) # Note: the string is composed with \ita \itb etc.

imgs = [
    PDF("unicode.pdf")
    SVG("unicode.svg")
    PNG("unicode.png")
]

draw.(imgs, [c])
