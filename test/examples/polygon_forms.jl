using Compose

compose(context(),
    ngon(0.15, 0.15, 0.08, 5),
    star(0.35, 0.15, 0.08, 5, 0.3),
    xgon(0.55, 0.15, 0.08, 5, 0.3)
) |> SVG("polygon_forms.svg", 3inch, 3inch)
