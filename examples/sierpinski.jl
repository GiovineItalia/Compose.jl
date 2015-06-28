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

#img = SVG(open("/dev/null", "w"), 4inch, 4(√3/2)inch)
#img = PNG("sierpinski.png", 4inch, 4(√3/2)inch)
img = SVG("sierpinski.svg", 4inch, 4(√3/2)inch)
@time draw(img, compose(sierpinski(10), linewidth(0.1mm), fill(nothing), stroke("black")))
img = SVG("sierpinski.svg", 4inch, 4(√3/2)inch)
@time draw(img, compose(sierpinski(10), linewidth(0.1mm), fill(nothing), stroke("black")))

