# Draw lines with various dash styles.

using Compose, Colors
import Cairo, Fontconfig

function draw_lines(joins)
    if length(joins) == 0
        context()
    else
        compose(compose(context(), line([(5mm, 5mm), (10mm, 10mm), (15mm, 5mm)]), strokelinejoin(joins[1])),
                compose(context(0, 5mm), draw_lines(joins[2:end])))
    end
end

joins = [Compose.LineJoinRound(), Compose.LineJoinMiter(), Compose.LineJoinBevel()]
c = draw_lines(joins)

imgs = [SVG("linejoins.svg", 2cm, 3cm),
        PDF("linejoins.pdf", 2cm, 3cm),
        SVGJS("linejoins.js", 2cm, 3cm)]
for img = imgs
    draw(img, compose(c, stroke("black"), linewidth(2mm), fill(nothing)))
end
