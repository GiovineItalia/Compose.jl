#!/usr/bin/env julia

load("compose.jl")

function sierpinski(n)
    if n == 0
        canvas() << polygon((1,1), (0,1), (1/2, 0))
    else
        t = sierpinski(n - 1)
        canvas() |
            (canvas(1/4,   0, 1/2, 1/2) | t) |
            (canvas(  0, 1/2, 1/2, 1/2) | t) |
            (canvas(1/2, 1/2, 1/2, 1/2) | t)
    end
end

@upon SVG("sierpinski.svg", 4inch, 4(sqrt(3)/2)inch) begin
    draw(sierpinski(8) << (linewidth(0.1mm) | fill(nothing)))
end

