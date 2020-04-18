using Compose, Colors
using Base.MathConstants

function golden_rect(n::Int)
    poly_points = [(0, 0), (1, 0), (1, 1), (0, 1)]
    if n == 0 return context() end
    c = compose(context(), polygon(poly_points), fill(LCHab(90, 80, 70-20n)), stroke("black"))
    compose(c, (context(0,0,1/φ,1/φ, rotation=Rotation(-π/2,1,0)),  golden_rect(n-1)))
end

draw(SVG("golden_rect.svg", φ*3inch, 3inch),
     compose(golden_rect(8), linewidth(0.2mm)))
