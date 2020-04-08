# Draw lines with various dash styles.

using Compose, Colors
import Cairo, Fontconfig

function draw_lines(caps)
    if length(caps) == 0
        context()
    else
        compose(compose(context(), line([(5mm, 5mm), (15mm, 5mm)]), strokelinecap(caps[1])),
                compose(context(0, 5mm), draw_lines(caps[2:end])))
    end
end

caps = [LineCapButt(), LineCapSquare(), LineCapRound()]
c = draw_lines(caps)

imgs = [SVG("linecaps.svg", 2cm, 3cm),
        PDF("linecaps.pdf", 2cm, 3cm)]
for img = imgs
    draw(img, compose(c, stroke("black"), linewidth(2mm)))
end
