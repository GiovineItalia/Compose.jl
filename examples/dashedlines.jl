#!/usr/bin/env julia

# Draw lines with various dash styles.

using Compose
using Color

function draw_lines(dash_patterns)
    if length(dash_patterns) == 0
        canvas()
    else
        compose(compose(canvas(), lines((0, 0), (1, 0)), strokedash(dash_patterns[1])),
                compose(canvas(0, 0.1), draw_lines(dash_patterns[2:end])))
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

img = PDF("dash.pdf", 4inch, 4(sqrt(3)/2)inch)
draw(img, compose(c, stroke(color("black")), linewidth(1mm)))
