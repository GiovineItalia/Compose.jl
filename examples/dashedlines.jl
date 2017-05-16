#!/usr/bin/env julia

# Draw lines with various dash styles.

using Compose
using Colors

function draw_lines(dash_patterns)
    if length(dash_patterns) == 0
        context()
    else
        compose(compose(context(), line([(0, 0), (1, 0)]), strokedash(dash_patterns[1])),
                compose(context(0, 0.1), draw_lines(dash_patterns[2:end])))
    end
end

patterns = Array[
    [5mm, 5mm],
    [5mm, 10mm],
    [10mm, 5mm],
    [5mm, 1mm],
    [1mm, 5mm],
    [0.9mm],
    [15mm, 10mm, 5mm],
    [15mm, 10mm, 5mm, 10mm],
    [15mm, 10mm, 5mm, 10mm, 15mm],
    [5mm, 5mm, 1mm, 5mm]]


c = draw_lines(patterns)

imgs = [PDF("dash.pdf", 4inch, 4(sqrt(3)/2)inch),
        PGF("dash.pdf", 4inch, 4(sqrt(3)/2)inch)]
for img = imgs
  draw(img, compose(c, stroke("black"), linewidth(1mm)))
end
