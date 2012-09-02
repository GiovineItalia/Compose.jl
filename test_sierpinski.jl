#!/usr/bin/env julia

load("compose.jl")

function sierpinski(n)
    if n == 0; return Polygon((1, 1), (0, 1), (1/2, 0)); end

    t = sierpinski(n - 1)
    {Canvas(),
      {Canvas(1/4,   0, 1/2, 1/2), t},
      {Canvas(  0, 1/2, 1/2, 1/2), t},
      {Canvas(1/2, 1/2, 1/2, 1/2), t}}
end

@upon SVG("sierpinski.svg", 4inch, 4(sqrt(3)/2)inch) begin
    draw({sierpinski(6), LineWidth(0.1mm)})
end

