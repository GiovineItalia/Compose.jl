
# An SVG backend for compose.

export SVG

require("Compose/src/backend.jl")
require("Compose/src/measure.jl")
require("Compose/src/color.jl")
require("Compose/src/form.jl")
require("Compose/src/json.jl")
require("Compose/src/pango.jl")
require("Compose/src/util.jl")

# Format a floating point number into a decimal string of reasonable precision.
function svg_fmt_float(x::Float64)
    # All svg (in our use) coordinates are in millimeters. This number gives the
    # largest deviation from the true position allowed in millimeters.
    const eps = 0.01
    a = @sprintf("%0.8f", round(x / eps) * eps)
    n = length(a)

    while a[n] == '0'
        n -= 1
    end

    if a[n] == '.'
        n -= 1
    end

    a[1:n]
end


# The SVG image we generate are going to use millimeters everywhere.
type SVGMeasure <: NativeMeasure
    value::Float64
end

*(u::Float64, v::SVGMeasure) = SVGMeasure(u * v.value)
+(u::SVGMeasure, v::SVGMeasure) = SVGMeasure(u.value + v.value)
-(u::SVGMeasure, v::SVGMeasure) = SVGMeasure(u.value - v.value)
convert(::Type{Float64}, u::SVGMeasure) = u.value
convert(::Type{SVGMeasure}, u::Float64) = SVGMeasure(u)


type SVG <: Backend
    # Image size in millimeters.
    width::SVGMeasure
    height::SVGMeasure

    # Output stream.
    f::IOStream

    # Should f be closed when the backend is finished?
    close_stream::Bool

    # Current level of indentation.
    indentation::Int

    # Javascript fragments are placed in a function with a unique name. This is
    # a map of unique function names to javascript code.
    scripts::Dict{String, String}

    # Keep track of which properties that are push are empty to we can avoid
    # printiing them.
    empty_properties::Vector{Bool}

    # Keep track of which properties have links, so we know when to insert
    # closing </a> tags.
    linked_properties::Vector{Bool}

    function SVG(f::IOStream,
                 width::MeasureOrNumber,
                 height::MeasureOrNumber)
        img = new()
        img.width  = native_measure(width,  img)
        img.height = native_measure(height, img)
        img.f = f
        img.close_stream = false
        img.indentation = 0
        img.scripts = Dict{String, String}()
        img.empty_properties = Array(Bool, 0)
        img.linked_properties = Array(Bool, 0)

        write(img.f, @sprintf(
"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<svg xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" version=\"1.1\" width=\"%smm\" height=\"%smm\" viewBox=\"0 0 %s %s\" style=\"stroke:black;fill:black\" stroke-width=\"0.5\">\n",
              svg_fmt_float(img.width.value), svg_fmt_float(img.height.value),
              svg_fmt_float(img.width.value), svg_fmt_float(img.height.value)))

        img
    end

    function SVG(filename::String,
                 width::MeasureOrNumber,
                 height::MeasureOrNumber)
        f = open(filename, "w")
        img = SVG(f, width, height)
        img.close_stream = true
        img
    end
end


function finish(img::SVG)
    if length(img.scripts) > 0
        write(img.f, "<script type=\"application/ecmascript\"><![CDATA[\n")
        for (fn_name, js) in img.scripts
            @printf(img.f, "function %s(evt) {\n%s\n}\n\n", fn_name, js)
        end
        write(img.f, "]]></script>")
    end

    write(img.f, "</svg>\n")
    if img.close_stream
        close(img.f)
    end
end


# We are going to make to do compound units, or at least, PX + MM

function root_box(img::SVG)
    NativeBoundingBox(
        SVGMeasure(0.0),
        SVGMeasure(0.0),
        img.width,
        img.height)
end


native_zero(::SVG) = SVGMeasure(0.0)


function indent(img::SVG)
    for i in 1:img.indentation
        write(img.f, "  ")
    end
end


function native_measure(u::SimpleMeasure{PixelUnit},
                        img::SVG)
    native_measure(convert(SimpleMeasure{MillimeterUnit}, u), img)
end


function native_measure(u::SimpleMeasure{MillimeterUnit},
                        img::SVG)
    SVGMeasure(u.value)
end


# Draw


function draw(img::SVG, form::Lines)
    n = length(form.points)
    if n <= 1; return; end

    indent(img)
    write(img.f, "<path d=\"")
    @printf(img.f, "M%s,%s L",
        svg_fmt_float(form.points[1].x.value),
        svg_fmt_float(form.points[1].y.value))
    for point in form.points[2:]
        @printf(img.f, " %s %s",
            svg_fmt_float(point.x.value),
            svg_fmt_float(point.y.value))
    end
    write(img.f, "\" />\n")
end


function draw(img::SVG, form::Polygon)
    n = length(form.points)
    if n <= 1; return; end

    indent(img)
    write(img.f, "<path d=\"")
    @printf(img.f, "M %s %s L",
        svg_fmt_float(form.points[1].x.value),
        svg_fmt_float(form.points[1].y.value))
    for point in form.points[2:]
        @printf(img.f, " %s %s",
            svg_fmt_float(point.x.value),
            svg_fmt_float(point.y.value))
    end
    write(img.f, " z\" />\n")
end


minmax(a, b) = a < b ? (a,b) : (b,a)


function draw(img::SVG, form::Ellipse)
    cx = form.center.x.value
    cy = form.center.y.value
    rx = sqrt((form.x_point.x.value - cx)^2 +
              (form.x_point.y.value - cy)^2)
    ry = sqrt((form.y_point.x.value - cx)^2 +
              (form.y_point.y.value - cy)^2)
    theta = radians2degrees(atan2(form.x_point.y.value - cy,
                                  form.x_point.x.value - cx))

    indent(img)
    eps = 1e-6

    if abs(rx - ry) < eps
        @printf(img.f, "<circle cx=\"%s\" cy=\"%s\" r=\"%s\" />\n",
                svg_fmt_float(cx), svg_fmt_float(cy), svg_fmt_float(rx))
    else
        @printf(img.f, "<ellipse cx=\"%s\" cy=\"%s\" rx=\"%s\" ry=\"%s\"",
                svg_fmt_float(cx), svg_fmt_float(cy),
                svg_fmt_float(rx), svg_fmt_float(ry))

        if abs(theta) >= eps
            @printf(img.f, " transform=\"rotate(%s %s %s)\"",
                    svg_fmt_float(theta),
                    svg_fmt_float(cx),
                    svg_fmt_float(cy))
        end

        write(img.f, " />\n")
    end
end


function draw(img::SVG, form::Text)
    indent(img)
    @printf(img.f, "<text x=\"%s\" y=\"%s\"",
            svg_fmt_float(form.pos.x.value),
            svg_fmt_float(form.pos.y.value))

    if is(form.halign, hcenter)
        print(img.f, " text-anchor=\"middle\"")
    elseif is(form.halign, hright)
        print(img.f, " text-anchor=\"end\"")
    end

    if is(form.valign, vcenter)
        print(img.f, " style=\"dominant-baseline:central\"")
    elseif is(form.valign, vtop)
        print(img.f, " style=\"dominant-baseline:text-before-edge\"")
    end

    if !isidentity(form.t)
        @printf(img.f, " transform=\"rotate(%s, %s, %s)\"",
                svg_fmt_float(radians2degrees(atan2(form.t.M[2,1], form.t.M[1,1]))),
                svg_fmt_float(form.pos.x.value),
                svg_fmt_float(form.pos.y.value))
    end

    # TODO: escape special characters
    @printf(img.f, ">%s</text>\n",
            pango_to_svg(form.value))
end


# Applying properties

function push_property(img::SVG, property::Property)
    if property === empty_property
        push(img.empty_properties, true)
        push(img.linked_properties, false)
    else
        push(img.empty_properties, false)
        indent(img)

        # There can only be one property of each type. E.g, defining 'fill'
        # twice in the same element is not valid svg.
        properties = Dict{Type, PropertyPrimitive}()
        p = property
        while !is(p, empty_property)
            properties[typeof(p.primitive)] = p.primitive
            p = p.next
        end

        # The link property demands special handling, since it's a seperate tag
        # outside of <g>.
        link_target = nothing
        if has(properties, SVGLink)
            link_target = properties[SVGLink].target
        end

        if !(link_target === nothing)
            write(img.f, @sprintf("<a xlink:href=\"%s\">", link_target))
            push(img.linked_properties, true)
        else
            push(img.linked_properties, false)
        end

        write(img.f, "<g")
        for p in values(properties)
            apply_property(img, p)
        end
        write(img.f, ">\n")

        img.indentation += 1
    end
end


function pop_property(img::SVG)
    is_linked = pop(img.linked_properties)
    if !pop(img.empty_properties)
        img.indentation -= 1
        indent(img)
        write(img.f, "</g>")
        if is_linked
            write(img.f, "</a>")
        end
        write(img.f, "\n")
    end
end


# Nop catchall
function apply_property(img::SVG, p::PropertyPrimitive)
end


function apply_property(img::SVG, p::Stroke)
    @printf(img.f, " stroke=\"%s\"", cssfmt(p.value))
end


function apply_property(img::SVG, p::Fill)
    @printf(img.f, " fill=\"%s\"", cssfmt(p.value))
end


function apply_property(img::SVG, p::LineWidth)
    @printf(img.f, " stroke-width=\"%s\"", svg_fmt_float(p.value.value))
end


function apply_property(img::SVG, p::SVGID)
    @printf(img.f, " id=\"%s\"", escape_string(p.value))
end

function apply_property(img::SVG, p::SVGClass)
    @printf(img.f, " class=\"%s\"", escape_string(p.value))
end

function apply_property(img::SVG, p::Font)
    @printf(img.f, " font-family=\"%s\"", escape_string(p.family))
end

function apply_property(img::SVG, p::FontSize)
    @printf(img.f, " font-size=\"%s\"", svg_fmt_float(p.value.value))
end


for event in events
    prop_name = lowercase(string(event))
    @eval begin
        function apply_property(img::SVG, p::($event))
            fn_name = add_script(img, p.value, object_id(p.value))
            @printf(img.f, " %s=\"%s(evt)\"", ($prop_name), fn_name)
        end
    end
end


# Add some javascript. Return the unique name generated for the wrapper function
# containing the given code.
function add_script(img::SVG, js::String, obj_id::Uint64)
    fn_name = @sprintf("js_chunk_%x", obj_id)
    if !has(img.scripts, fn_name)
        img.scripts[fn_name] = replace(js, r"^"m, "  ")
    end
    fn_name
end


