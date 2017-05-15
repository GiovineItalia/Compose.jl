#!/usr/bin/env julia

# A trivial example of deferred contexts.
using Compose

function sierpinski(n::Int)
    if n == 0
        compose(context(), polygon([(1,1), (0,1), (1/2, 0)]))
    else
        t = sierpinski(n - 1)
        compose(context(),
                (context(1/4,   0, 1/2, 1/2), t),
                (context(  0, 1/2, 1/2, 1/2), t),
                (context(1/2, 1/2, 1/2, 1/2), t))
    end
end

img = SVG("sierpinski_deferred.svg", 4inch, 4(√3/2)inch)
draw(img, compose(context(), linewidth(0.1mm), fill(nothing), stroke("black"),
                  ctxpromise(c -> sierpinski(8))))
