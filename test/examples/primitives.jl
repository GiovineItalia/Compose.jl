using Compose

rawimg = read(joinpath(@__DIR__,"smiley.png"));

compose(context(),
    rectangle(0.1,0.1,0.1,0.1),
    circle(0.3,0.15,0.05),
    polygon([(0.45,0.1),(0.4,0.2),(0.5,0.2)]),
    ellipse(0.65,0.15,0.1,0.05),
    (context(), stroke("black"),
        line([(0.1,0.3),(0.2,0.3)]),
        curve((0.25,0.35),(0.25,0.25),(0.35,0.25),(0.35,0.35))),
    bitmap("image/png",rawimg,0.4,0.25,0.1,0.1),
    mathjax(0.2, 0.4, 0.1, 0.1, "x+1\\over y-1"),
    rawsvg(raw"""<g xmlns="http://www.w3.org/2000/svg" transform="translate(84.85,30)">
  <g class="primitive">
    <text dy="-1em">hello</text>
  </g>
</g>"""),
    bitmap("image/png",rawimg,0.4,0.25,0.1,0.1),
    text(0.6,0.3,"hello")) |> SVG("primitives.svg")
