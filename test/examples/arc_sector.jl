using Compose
import Cairo, Fontconfig

imgs = [SVG("arc_sector.svg", 7cm, 7cm),
        PDF("arc_sector.pdf", 7cm, 7cm)]

a = range(-0.5π, stop=1.5π, length=13)
    colv = repeat(["white","black"], outer=6)
    
img = compose(context(),
    (context(), sector([0.5], [0.5], [0.4], a[1:12], a[2:13]), fill(colv)),
    (context(order=2), arc([0.5], [0.5], [0.2], [0, π], [π,0]), 
            fill(["black","white"]), stroke(["white","black"]), linewidth(6pt))
    ) 

draw.(imgs, [img])







