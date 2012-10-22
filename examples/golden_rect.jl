#!/usr/bin/env julia

insert(LOAD_PATH, 1, "../src/")
load("compose.jl")

const phi = (1 + sqrt(5)) / 2

function golden_rect(n::Int)
    if n == 0; return empty_canvas; end
    c = canvas() << (rectangle() << fill(LCHab(90, 80, 70 - 15n)))
    c | (canvas(0, -1/phi, 1h, 1/phi, Rotation(pi/2, 0, 1)) | golden_rect(n - 1))
end

@upon SVG("golden_rect.svg", phi * 3inch, 3inch) begin
    draw(golden_rect(10) << (fill(nothing) | stroke("white") | linewidth(0.2mm)))
end

