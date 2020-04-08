using Compose
using Test
using EzXML
using Colors
using Measures

@testset "Issue 267" begin
    global c = compose(context(), fill(["red", "blue"]),
                [context(), fill("green"), circle([0.25, 0.75], [0.5], [0.25])])
    img = SVG(8cm, 6cm, false)
    draw(img, c)
    svgxml = root(parsexml(String(img.out.data)))
    # get all grouped values that have the fill attribute
    fillcolors = nodecontent.(findall("//ns:g[@fill]/@fill", svgxml, ["ns"=>namespace(svgxml)]))
    # there should only be a single color (because green should clobber the red
    # and blue colors)
    @test length(fillcolors) == 1
    # make sure it's green
    @test fillcolors[1] == "#008000"
end
