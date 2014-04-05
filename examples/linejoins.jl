#!/usr/bin/env julia

# Draw lines with various dash styles.

using Compose
using Color

function draw_lines(joins)
    if length(joins) == 0
        canvas()
    else
        compose(compose(canvas(), lines((5mm, 5mm), (10mm, 10mm), (15mm, 5mm)), strokelinejoin(joins[1])),
                compose(canvas(0, 5mm), draw_lines(joins[2:end])))
    end
end

joins = [LineJoinRound(), LineJoinMiter(), LineJoinBevel()]
c = draw_lines(joins)

imgs = [SVG("linejoins.svg", 2cm, 3cm),
        PDF("linejoins.pdf", 2cm, 3cm),
        D3("linejoins.js", 2cm, 3cm)]
for img = imgs
    draw(img, compose(c, stroke(color("black")), linewidth(2mm), fill(Nothing())))
end
