#!/usr/bin/env julia

# A trivial example of deferred canvases.

require("Compose")
using Compose


function sierpinski(n)
    if n == 0
        canvas() << polygon((1,1), (0,1), (1/2, 0))
    else
        t = sierpinski(n - 1)
        compose(canvas(),
                (canvas(1/4,   0, 1/2, 1/2), t),
                (canvas(  0, 1/2, 1/2, 1/2), t),
                (canvas(1/2, 1/2, 1/2, 1/2), t))
    end
end


img = SVG("sierpinski.svg", 4inch, 4(sqrt(3)/2)inch)
draw(img, compose(canvas(), fill(nothing), linewidth(0.1mm),
                  deferredcanvas((box, unit) -> sierpinski(8))))
finish(img)
