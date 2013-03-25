
# An experimental d3 backend for compose.

export D3



# In d3 svg we use millimeters as the base measure.
type D3Measure <: NativeMeasure
    value::Float64
end

*(u::Float64, v::D3Measure) = D3Measure(u * v.value)
+(u::D3Measure, v::D3Measure) = D3Measure(u.value + v.value)
-(u::D3Measure, v::D3Measure) = D3Measure(u.value - v.value)
convert(::Type{Float64}, u::D3Measure) = u.value
convert(::Type{D3Measure}, u::Float64) = D3Measure(u)
function convert(::Type{SimpleMeasure{MillimeterUnit}}, u::D3Measure)
    SimpleMeasure{MillimeterUnit}(u.value)
end


function show(io::IO, u::D3Measure)
    @printf(io, "%smm", svg_fmt_float(u.value))
end


type D3 <: Backend
    # Image size in millimeters
    width::D3Measure
    height::D3Measure

    # Output stream
    out::IO

    # Should f be closed when the backend is finished?
    close_stream::Bool

    # Current level of indentation.
    indentation::Int

    # Keep track of which properties that are push are empty to we can avoid
    # printiing them.
    empty_properties::Vector{Bool}

    function D3(f::IO,
                width::MeasureOrNumber,
                height::MeasureOrNumber)
        img = new()
        img.width  = native_measure(width, img)
        img.height = native_measure(height, img)
        img.out = f
        img.close_stream = false
        img.indentation = 0
        img.empty_properties = Array(Bool, 0)

        width_value = svg_fmt_float(width.value)
        height_value = svg_fmt_float(height.value)
        write(img.out,
              """
              var g = d3.select("#chart")
                        .append("svg")
                          .attr("width", "$(width_value)mm")
                          .attr("height", "$(height_value)mm")
                          .attr("viewBox", "0 0 $(width_value) $(height_value)")
                          .attr("stroke-width", "0.5")
                          .attr("style", "stroke:black;fill:black");
              """)
        img
    end

    function D3(filename::String,
                width::MeasureOrNumber,
                height::MeasureOrNumber)
        f = open(filename, "w")
        img = D3(f, width, height)
        img.close_stream = true
        img
    end
end


function finish(img::D3)
    # TODO
    if img.close_stream
        close(img.out)
    end
end


# Conversion to D3 units (i.e. millimeters)
function native_measure(u::SimpleMeasure{PixelUnit},
                        img::D3)
    native_measure(convert(SimpleMeasure{MillimeterUnit}, u), img)
end


function native_measure(u::SimpleMeasure{MillimeterUnit},
                        img::D3)
    D3Measure(u.value)
end


function root_box(img::D3)
    NativeBoundingBox(
        D3Measure(0.0),
        D3Measure(0.0),
        img.width,
        img.height)
end


native_zero(::D3) = D3Measure(0.0)


function indent(img::D3)
    for i in 1:img.indentation
        write(img.out, "  ")
    end
end


# Nop catchall
function draw(img::D3, form::FormPrimitive)
    # Ignoring properties for the time being, we have to figure out a way to
    # make d3's vector-based syntax jive with composes's scalar-based syntax,
    # even if that means changing compose.

    # The simplest way to do this is to simply append the shapes that compose
    # declares. However, I don't want to totally disregard system for operating
    # on single data set. It seems then that the only solution is to add
    # vector-based form primitives to compose.

    # How should these be implemented so that we don't have to add a bunch of
    # code to the SVG or Cairo backends?

    # Actually, that's not so hard. We implement a catchall Draw function that
    # takes these and calls draw on each scalar. Then we implement a specialized
    # draw on the D3 backend that does fancy stuff.

end


function draw(img::D3, form::Polygon)
    n = length(form.points)
    if n <= 1; return; end

    indent(img)
    write(img.out, "g.append(\"svg:path\")\n")
    indent(img)
    write(img.out, "   .attr(\"d\", \"")
    @printf(img.out, "M %s %s L",
        svg_fmt_float(form.points[1].x.value),
        svg_fmt_float(form.points[1].y.value))
    for point in form.points[2:]
        @printf(img.out, " %s %s",
            svg_fmt_float(point.x.value),
            svg_fmt_float(point.y.value))
    end
    write(img.out, " z\");\n")
end


# Nop catchall
function push_property(img::D3, property::Property)
    if property === empty_property
        push!(img.empty_properties, true)
        return
    end
    push!(img.empty_properties, false)

    indent(img)
    write(img.out, "(function (g) {\n")
    img.indentation += 1

    # There can only be one property of each type. E.g, defining 'fill'
    # twice in the same element is not valid svg.
    properties = Dict{Type, PropertyPrimitive}()
    rawproperties = {}
    p = property
    while !is(p, empty_property)
        if typeof(p.primitive) == SVGAttribute
            push!(rawproperties, p.primitive)
        else
            properties[typeof(p.primitive)] = p.primitive
        end
        p = p.next
    end

    n = length(properties) + length(rawproperties)
    if n == 0
        return
    end

    indent(img)
    write(img.out, "g")
    for (i, p) in enumerate(Iterators.chain(values(properties), rawproperties))
        apply_property(img, p)
        if i < n
            write(img.out, "\n ")
            indent(img)
        end
    end
    write(img.out, ";\n");
end


function pop_property(img::D3)
    if pop!(img.empty_properties)
        return
    end

    println("pop_property")
    img.indentation -= 1;
    indent(img)
    write(img.out, "}(g.append(\"g\")));\n")
end


function apply_property(img::D3, p::PropertyPrimitive)

end


function apply_property(img::D3, p::Stroke)
    @printf(img.out, ".attr(\"stroke\", \"%s\")", svg_fmt_color(p.value))
end


function apply_property(img::D3, p::Fill)
    @printf(img.out, ".attr(\"fill\", \"%s\")", svg_fmt_color(p.value))
end


function apply_property(img::D3, p::LineWidth)
    @printf(img.out, ".attr(\"stroke-width\", \"%s\")",
            svg_fmt_float(p.value.value))
end



# TODO: more forms, motherfucker.




