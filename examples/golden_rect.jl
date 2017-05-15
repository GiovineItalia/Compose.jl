#!/usr/bin/env julia

using Colors
using Compose

function golden_rect(n::Int)
    if n == 0 return context() end
    c = compose(context(), rectangle(), fill(LCHab(90, 80, 70 - 15n)))
    compose(c, context(units=UnitBox(0, -1/φ, 1h, 1/φ), rotation=Rotation(π/2, 0, 1)),  golden_rect(n - 1))
end

draw(SVG("golden_rect.svg", φ * 3inch, 3inch),
    compose(golden_rect(10), fill(nothing), stroke("white"), linewidth(0.2mm)))
