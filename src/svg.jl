
# An SVG backend for compose.

export SVG


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

# Format a color for SVG.
svg_fmt_color(c::ColorValue) = @sprintf("#%s", hex(c))
svg_fmt_color(c::Nothing) = "none"


# The SVG image we generate are going to use millimeters everywhere.
type SVGMeasure <: NativeMeasure
    value::Float64
end

*(u::Float64, v::SVGMeasure) = SVGMeasure(u * v.value)
+(u::SVGMeasure, v::SVGMeasure) = SVGMeasure(u.value + v.value)
-(u::SVGMeasure, v::SVGMeasure) = SVGMeasure(u.value - v.value)
convert(::Type{Float64}, u::SVGMeasure) = u.value
convert(::Type{SVGMeasure}, u::Float64) = SVGMeasure(u)
function convert(::Type{SimpleMeasure{MillimeterUnit}}, u::SVGMeasure)
    SimpleMeasure{MillimeterUnit}(u.value)
end


type SVG <: Backend
    # Image size in millimeters.
    width::SVGMeasure
    height::SVGMeasure

    # Output stream.
    f::IO

    # Should f be closed when the backend is finished?
    close_stream::Bool

    # Current level of indentation.
    indentation::Int

    # Javascript fragments are placed in a function with a unique name. This is
    # a map of unique function names to javascript code.
    scripts::Dict{String, String}

    # Clip-paths that need to be defined at the end of the document.
    clippaths::Vector{Vector{Point}}

    # Miscelaneous embedded objects included immediately before the </svg> tag,
    # such as extra javascript or css.
    embobj::Set{String}

    # Keep track of which properties that are push are empty to we can avoid
    # printiing them.
    empty_properties::Vector{Bool}

    # Keep track of which properties have links, so we know when to insert
    # closing </a> tags.
    linked_properties::Vector{Bool}

    # Keep track of mask objects.
    mask_properties::Vector{Bool}

    # Emit the graphic on finish when writing to a buffer.
    emit_on_finish::Bool

    function SVG(f::IO,
                 width::MeasureOrNumber,
                 height::MeasureOrNumber;
                 emit_on_finish::Bool=true)
        img = new()
        img.width  = native_measure(width,  img)
        img.height = native_measure(height, img)
        img.f = f
        img.close_stream = false
        img.indentation = 0
        img.clippaths = Array(Vector{Point}, 0)
        img.scripts = Dict{String, String}()
        img.embobj = Set{String}()
        img.empty_properties = Array(Bool, 0)
        img.linked_properties = Array(Bool, 0)
        img.mask_properties = Array(Bool, 0)
        img.emit_on_finish = emit_on_finish

        write(img.f, @sprintf("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<svg xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" version=\"1.1\" width=\"%smm\" height=\"%smm\" viewBox=\"0 0 %s %s\" style=\"stroke:black;fill:black\" stroke-width=\"0.5\">\n",
              svg_fmt_float(img.width.value), svg_fmt_float(img.height.value),
              svg_fmt_float(img.width.value), svg_fmt_float(img.height.value)))
        img
    end

    # Write to a file.
    function SVG(filename::String,
                 width::MeasureOrNumber,
                 height::MeasureOrNumber)
        f = open(filename, "w")
        img = SVG(f, width, height)
        img.close_stream = true
        img
    end

    # Write to buffer.
    function SVG(width::MeasureOrNumber,
                 height::MeasureOrNumber;
                 emit_on_finish::Bool=true)
        img = SVG(IOString(), width, height, emit_on_finish=emit_on_finish)
        img.close_stream = false
        img
    end
end


function finish(img::SVG)
    for obj in img.embobj
        write(img.f, obj)
        write(img.f, "\n")
    end

    if length(img.scripts) > 0
        write(img.f, "<script type=\"application/ecmascript\"><![CDATA[\n")
        for (fn_name, js) in img.scripts
            @printf(img.f, "function %s(evt) {\n%s\n}\n\n", fn_name, js)
        end
        write(img.f, "]]></script>\n")
    end

    if length(img.clippaths) > 0
        write(img.f, "<defs>\n")
        for (i, clippath) in enumerate(img.clippaths)
            write(img.f, "  <clipPath id=\"clippath$(i)\">\n    ")
            printpath(img.f, clippath)
            write(img.f, "  </clipPath>\n")
        end
        write(img.f, "</defs>\n")
    end

    write(img.f, "</svg>\n")
    if img.close_stream
        close(img.f)
    end

    # If we are writing to a buffer. Collect the string and emit it.
    if img.emit_on_finish && typeof(img.f) == IOString
        seek(img.f, 0)
        emit(Emitable("image/svg+xml", readall(img.f)))
        close(img.f)
    end
end


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


function printpath(out, points::Vector{Point})
    write(out, "<path d=\"")
    @printf(out, "M%s,%s L",
        svg_fmt_float(points[1].x.value),
        svg_fmt_float(points[1].y.value))
    for point in points[2:]
        @printf(out, " %s %s",
            svg_fmt_float(point.x.value),
            svg_fmt_float(point.y.value))
    end
    write(out, "\" />\n")
end


function draw(img::SVG, form::Lines)
    n = length(form.points)
    if n <= 1; return; end
    indent(img)
    printpath(img.f, form.points)
end


function draw(img::SVG, form::Curve)
    indent(img)
    @printf(img.f, "<path d=\"M%s,%s C%s,%s %s,%s %s,%s\" />\n",
        svg_fmt_float(form.anchor0.x.value),
        svg_fmt_float(form.anchor0.y.value),
        svg_fmt_float(form.ctrl0.x.value),
        svg_fmt_float(form.ctrl0.y.value),
        svg_fmt_float(form.ctrl1.x.value),
        svg_fmt_float(form.ctrl1.y.value),
        svg_fmt_float(form.anchor1.x.value),
        svg_fmt_float(form.anchor1.y.value))
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
        push!(img.empty_properties, true)
        push!(img.linked_properties, false)
        push!(img.mask_properties, false)
    else
        push!(img.empty_properties, false)
        indent(img)

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

        # Special-case for the mask property.
        if has(properties, SVGDefineMask)
            write(img.f, @sprintf("<mask id=\"%s\">", properties[SVGDefineMask].id))
            push!(img.mask_properties, true)
        else
            push!(img.mask_properties, false)
        end

        # Special-case for links.
        if has(properties, SVGLink)
            write(img.f, @sprintf("<a xlink:href=\"%s\">",
                  properties[SVGLink].target === nothing ? '#' : properties[SVGLink].target))
            push!(img.linked_properties, true)
        else
            push!(img.linked_properties, false)
        end

        write(img.f, "<g")
        for p in chain(values(properties), rawproperties)
            apply_property(img, p)
        end
        write(img.f, ">\n")

        img.indentation += 1
    end
end


function pop_property(img::SVG)
    is_linked = pop!(img.linked_properties)
    is_mask = pop!(img.mask_properties)
    if !pop!(img.empty_properties)
        img.indentation -= 1
        indent(img)
        write(img.f, "</g>")
        if is_linked
            write(img.f, "</a>")
        end
        if is_mask
            write(img.f, "</mask>")
        end
        write(img.f, "\n")
    end
end


# Nop catchall
function apply_property(img::SVG, p::PropertyPrimitive)
end


function apply_property(img::SVG, p::Stroke)
    @printf(img.f, " stroke=\"%s\"", svg_fmt_color(p.value))
end


function apply_property(img::SVG, p::Fill)
    @printf(img.f, " fill=\"%s\"", svg_fmt_color(p.value))
end


function apply_property(img::SVG, p::LineWidth)
    @printf(img.f, " stroke-width=\"%s\"", svg_fmt_float(p.value.value))
end


function apply_property(img::SVG, p::Visible)
    @printf(img.f, " visibility=\"%s\"",
            p.value ? "visible" : "hidden")
end


function apply_property(img::SVG, p::Opacity)
    @printf(img.f, " opacity=\"%s\"", fmt_float(p.value))
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

function apply_property(img::SVG, p::Clip)
    push!(img.clippaths, p.points)
    i = length(img.clippaths)
    write(img.f, " clip-path=\"url(#clippath$(i))\"")
end

function apply_property(img::SVG, p::SVGEmbed)
    add!(img.embobj, p.markup)
end


function apply_property(img::SVG, p::SVGMask)
    @printf(img.f, " mask=\"url(#%s)\"", p.id)
end


function apply_property(img::SVG, p::SVGAttribute)
    @printf(img.f, " %s=\"%s\"", p.attribute, p.value)
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
function add_script(img::SVG, js::String, obj_id::Integer)
    fn_name = @sprintf("js_chunk_%x", obj_id)
    if !has(img.scripts, fn_name)
        img.scripts[fn_name] = replace(js, r"^"m, "  ")
    end
    fn_name
end


