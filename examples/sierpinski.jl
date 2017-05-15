using Compose, Colors

# const padding = 0.5mm

function sierpinski(n::Int)
    if n == 0
        compose(context(), polygon([(1,1), (0,1), (1/2, 0)]))
    else
        t = sierpinski(n - 1)
        compose(context(),
                (context(1/4,   0, 1/2, 1/2), t),
                (context(  0, 1/2, 1/2, 1/2), t),
                (context(1/2, 1/2, 1/2, 1/2), t))
        # compose(context(),
        #         (context(0.25w + padding, padding,        0.5w - 2*padding, 0.5h - 2*padding), t),
        #         (context(padding,         0.5h + padding, 0.5w - 2*padding, 0.5h - 2*padding), t),
        #         (context(0.5w + padding,  0.5h + padding, 0.5w - 2*padding, 0.5h - 2*padding), t))
    end
end

img = SVG("sierpinski.svg", 4inch, 4(√3/2)inch)
draw(img, compose(sierpinski(2), linewidth(0.1mm), fill(nothing), stroke("black")))

img = SVG("sierpinski.svg", 4inch, 4(√3/2)inch)
@time draw(img, compose(sierpinski(8), linewidth(0.1mm), fill(nothing), stroke("black")))

#img = SVG("sierpinski.svg", 4inch, 4(√3/2)inch)
#@profile draw(img, compose(sierpinski(10), linewidth(0.1mm), fill(nothing), stroke("black")))
#Profile.print()
