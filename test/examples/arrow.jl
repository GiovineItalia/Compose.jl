using Compose
import Cairo, Fontconfig

imgs = [SVG("arrow.svg", 5cm, 5cm),
        PDF("arrow.pdf", 5cm, 5cm)]

X =  [0.047 0.87 0.95 0.93;
0.22 0.01 0.21 0.7;
0.86 0.85 0.95 0.21]

point_array = [[(x1,y1), (x2,y2)] for (x1,y1,x2,y2) in zip(X[:,1],X[:,2],X[:,3],X[:,4])]
img = compose(context(),
    (context(), line(point_array), stroke(["red","green","deepskyblue"]), arrow())
)

draw.(imgs, [img])
