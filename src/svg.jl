
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


type SVG <: Backend
    # Image size in millimeters.
    width::Float64
    height::Float64

    # Output stream.
    out::IO

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

    # True when finish has been called and no more drawing should occur
    finished::Bool

    # Backend is responsible for opening/closing the file
    ownedfile::Bool

    # Filename when ownedfile is true
    filename::Union(String, Nothing)

    # Emit the graphic on finish when writing to a buffer.
    emit_on_finish::Bool

    function SVG(out::IO,
                 width,
                 height,
                 emit_on_finish::Bool=true)
        width = size_measure(width)
        height = size_measure(height)
        if !isabsolute(width) || !isabsolute(height)
            error("SVG image size must be specified in absolute units.")
        end

        img = new()
        img.width  = width.abs
        img.height = height.abs
        img.out = out
        img.indentation = 0
        img.clippaths = Array(Vector{Point}, 0)
        img.scripts = Dict{String, String}()
        img.embobj = Set{String}()
        img.empty_properties = Array(Bool, 0)
        img.linked_properties = Array(Bool, 0)
        img.mask_properties = Array(Bool, 0)
        img.finished = false
        img.emit_on_finish = emit_on_finish
        img.ownedfile = false
        img.filename = nothing
        writeheader(img)
        img
    end

    # Write to a file.
    function SVG(filename::String, width, height)
        f = open(filename, "w")
        img = SVG(f, width, height)
        img.ownedfile = true
        img.filename = filename
        img
    end

    # Write to buffer.
    function SVG(width::MeasureOrNumber, height::MeasureOrNumber,
                 emit_on_finish::Bool=true)
        img = SVG(IOBuffer(), width, height, emit_on_finish)
        img
    end
end


function writeheader(img::SVG)
    write(img.out, @sprintf("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<svg xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" version=\"1.1\" width=\"%smm\" height=\"%smm\" viewBox=\"0 0 %s %s\" style=\"stroke:black;fill:black\" stroke-width=\"0.5\">\n",
          svg_fmt_float(img.width), svg_fmt_float(img.height),
          svg_fmt_float(img.width), svg_fmt_float(img.height)))
    img
end


function reset(img::SVG)
    if img.ownedfile
        img.out = open(img.filename, "w")
    else
        try
            seekstart(img.out)
        catch
            error("Backend can't be reused, since the output stream is not seekable.")
        end
    end
    writeheader(img)
    img.finished = false
end


function finish(img::SVG)
    if img.finished
        return
    end

    for obj in img.embobj
        write(img.out, obj)
        write(img.out, "\n")
    end

    if length(img.scripts) > 0
        write(img.out, "<script type=\"application/ecmascript\"><![CDATA[\n")
        for (fn_name, js) in img.scripts
            @printf(img.out, "function %s(evt) {\n%s\n}\n\n", fn_name, js)
        end
        write(img.out, "]]></script>\n")
    end

    if length(img.clippaths) > 0
        write(img.out, "<defs>\n")
        for (i, clippath) in enumerate(img.clippaths)
            write(img.out, "<clipPath id=\"clippath$(i)\">\n  <path d=\"")
            print_svg_path(img.out, clippath)
            write(img.out, "\" />\n</clipPath\n>")
        end
        write(img.out, "</defs>\n")
    end

    write(img.out, "</svg>\n")
    if method_exists(flush, (typeof(img.out),))
        flush(img.out)
    end

    if img.ownedfile
        close(img.out)
    end

    img.finished = true

    # If we are writing to a buffer. Collect the string and emit it.
    if img.emit_on_finish && typeof(img.out) == IOBuffer
        display(img)
    end
end


function isfinished(img::SVG)
    img.finished
end


function writemime(io::IO, ::MIME"image/svg+xml", img::SVG)
    write(io, takebuf_string(img.out))
end


function root_box(img::SVG)
    AbsoluteBoundingBox(0.0, 0.0, img.width, img.height)
end


function indent(img::SVG)
    for i in 1:img.indentation
        write(img.out, "  ")
    end
end


# Draw

# Generate SVG path data from an array of points.
#
# Args:
#   out: Output stream.
#   points: points on the path
#   bridge_gaps: when true, remove non-finite values, rather than forming
#                separate lines.
#
# Returns:
#   A string containing SVG path data.
#
function print_svg_path(out, points::Vector{Point}, bridge_gaps::Bool=false)
    isfirst = true
    for point in points
        x, y = point.x.abs, point.y.abs
        if !(isfinite(x) && isfinite(y))
            isfirst = true
            continue
        end

        if isfirst
            isfirst = false
            @printf(out, "M%s,%s L",
                    svg_fmt_float(x),
                    svg_fmt_float(y))
        else
            @printf(out, " %s %s",
                    svg_fmt_float(x),
                    svg_fmt_float(y))
        end
    end
end


# Return array of paths to draw with printpath
# array is formed by splitting by NaN values
function make_paths(points::Vector{Point})
    paths = {}
    nans = find(xy -> isnan(xy[1]) || isnan(xy[2]),
                [(point.x.abs, point.y.abs) for point in points])

    if length(nans) == 0
        push!(paths, points)
    else
        nans = [0, nans, length(points) + 1]
        i, n = 1, length(nans)
        while i <= n-1
            if nans[i] + 1 < nans[i + 1]
                push!(paths, points[(nans[i]+1):(nans[i+1] - 1)])
            end
            i += 1
        end
    end
    paths
end


function draw(img::SVG, form::Lines)
    n = length(form.points)
    if n <= 1; return; end
    indent(img)
    paths = make_paths(form.points)
    for path in paths
        write(img.out, "<path d=\"")
        print_svg_path(img.out, path)
        write(img.out, "\" />\n")
    end
end


function draw(img::SVG, form::Curve)
    indent(img)
    @printf(img.out, "<path d=\"M%s,%s C%s,%s %s,%s %s,%s\" />\n",
        svg_fmt_float(form.anchor0.x.abs),
        svg_fmt_float(form.anchor0.y.abs),
        svg_fmt_float(form.ctrl0.x.abs),
        svg_fmt_float(form.ctrl0.y.abs),
        svg_fmt_float(form.ctrl1.x.abs),
        svg_fmt_float(form.ctrl1.y.abs),
        svg_fmt_float(form.anchor1.x.abs),
        svg_fmt_float(form.anchor1.y.abs))
end


function draw(img::SVG, form::Polygon)
    n = length(form.points)
    if n <= 1; return; end

    indent(img)
    write(img.out, "<path d=\"")
    print_svg_path(img.out, form.points, true)
    write(img.out, " z\" />\n")
end


minmax(a, b) = a < b ? (a,b) : (b,a)


function draw(img::SVG, form::Ellipse)
    cx = form.center.x.abs
    cy = form.center.y.abs
    rx = sqrt((form.x_point.x.abs - cx)^2 +
              (form.x_point.y.abs - cy)^2)
    ry = sqrt((form.y_point.x.abs - cx)^2 +
              (form.y_point.y.abs - cy)^2)
    theta = rad2deg(atan2(form.x_point.y.abs - cy,
                          form.x_point.x.abs - cx))

    if !all(isfinite([cx, cy, rx, ry, theta]))
        return
    end

    indent(img)
    eps = 1e-6

    if abs(rx - ry) < eps
        @printf(img.out, "<circle cx=\"%s\" cy=\"%s\" r=\"%s\" />\n",
                svg_fmt_float(cx), svg_fmt_float(cy), svg_fmt_float(rx))
    else
        @printf(img.out, "<ellipse cx=\"%s\" cy=\"%s\" rx=\"%s\" ry=\"%s\"",
                svg_fmt_float(cx), svg_fmt_float(cy),
                svg_fmt_float(rx), svg_fmt_float(ry))

        if abs(theta) >= eps
            @printf(img.out, " transform=\"rotate(%s %s %s)\"",
                    svg_fmt_float(theta),
                    svg_fmt_float(cx),
                    svg_fmt_float(cy))
        end

        write(img.out, " />\n")
    end
end


function draw(img::SVG, form::Text)
    indent(img)
    @printf(img.out, "<text x=\"%s\" y=\"%s\"",
            svg_fmt_float(form.pos.x.abs),
            svg_fmt_float(form.pos.y.abs))

    if is(form.halign, hcenter)
        print(img.out, " text-anchor=\"middle\"")
    elseif is(form.halign, hright)
        print(img.out, " text-anchor=\"end\"")
    end

    if is(form.valign, vcenter)
        print(img.out, " style=\"dominant-baseline:central\"")
    elseif is(form.valign, vtop)
        print(img.out, " style=\"dominant-baseline:text-before-edge\"")
    end

    if !isidentity(form.t)
        @printf(img.out, " transform=\"rotate(%s, %s, %s)\"",
                svg_fmt_float(rad2deg(atan2(form.t.M[2,1], form.t.M[1,1]))),
                svg_fmt_float(form.pos.x.abs),
                svg_fmt_float(form.pos.y.abs))
    end

    # TODO: escape special characters
    @printf(img.out, ">%s</text>\n",
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
        properties = Dict{Symbol, PropertyPrimitive}()
        rawproperties = {}
        p = property
        while !is(p, empty_property)
            if typeof(p.primitive) == SVGAttribute
                push!(rawproperties, p.primitive)
            else
                properties[symbol(string(typeof(p.primitive)))] = p.primitive
            end
            p = p.next
        end

        # Special-case for the mask property.
        if haskey(properties, :SVGDefineMask)
            write(img.out, @sprintf("<mask id=\"%s\">", properties[:SVGDefineMask].id))
            push!(img.mask_properties, true)
        else
            push!(img.mask_properties, false)
        end

        # Special-case for links.
        if haskey(properties, :SVGLink)
            write(img.out, @sprintf("<a xlink:href=\"%s\">",
                  properties[:SVGLink].target === nothing ? '#' : properties[:SVGLink].target))
            push!(img.linked_properties, true)
        else
            push!(img.linked_properties, false)
        end

        write(img.out, "<g")
        for p in chain(values(properties), rawproperties)
            apply_property(img, p)
        end
        write(img.out, ">\n")

        img.indentation += 1
    end
end


function pop_property(img::SVG)
    is_linked = pop!(img.linked_properties)
    is_mask = pop!(img.mask_properties)
    if !pop!(img.empty_properties)
        img.indentation -= 1
        indent(img)
        write(img.out, "</g>")
        if is_linked
            write(img.out, "</a>")
        end
        if is_mask
            write(img.out, "</mask>")
        end
        write(img.out, "\n")
    end
end


# Nop catchall
function apply_property(img::SVG, p::PropertyPrimitive)
end


function apply_property(img::SVG, p::Stroke)
    @printf(img.out, " stroke=\"%s\"", svg_fmt_color(p.value))
end


function apply_property(img::SVG, p::StrokeDash)
    @printf(img.out, " stroke-dasharray=\"%s\"", join(map(v -> svg_fmt_float(v.abs), p.value), ","))
end


function apply_property(img::SVG, p::Fill)
    @printf(img.out, " fill=\"%s\"", svg_fmt_color(p.value))
end


function apply_property(img::SVG, p::LineWidth)
    @printf(img.out, " stroke-width=\"%s\"", svg_fmt_float(p.value.abs))
end


function apply_property(img::SVG, p::Visible)
    @printf(img.out, " visibility=\"%s\"",
            p.value ? "visible" : "hidden")
end


function apply_property(img::SVG, p::Opacity)
    @printf(img.out, " opacity=\"%s\"", fmt_float(p.value))
end


function apply_property(img::SVG, p::StrokeOpacity)
    @printf(img.out, " stroke-opacity=\"%s\"", fmt_float(p.value))
end



function apply_property(img::SVG, p::SVGID)
    @printf(img.out, " id=\"%s\"", escape_string(p.value))
end

function apply_property(img::SVG, p::SVGClass)
    @printf(img.out, " class=\"%s\"", escape_string(p.value))
end

function apply_property(img::SVG, p::Font)
    @printf(img.out, " font-family=\"%s\"", escape_string(p.family))
end

function apply_property(img::SVG, p::FontSize)
    @printf(img.out, " font-size=\"%s\"", svg_fmt_float(p.value.abs))
end

function apply_property(img::SVG, p::Clip)
    push!(img.clippaths, p.points)
    i = length(img.clippaths)
    write(img.out, " clip-path=\"url(#clippath$(i))\"")
end

function apply_property(img::SVG, p::SVGEmbed)
    add!(img.embobj, p.markup)
end


function apply_property(img::SVG, p::SVGMask)
    @printf(img.out, " mask=\"url(#%s)\"", p.id)
end


function apply_property(img::SVG, p::SVGAttribute)
    @printf(img.out, " %s=\"%s\"", p.attribute, p.value)
end


for event in events
    prop_name = lowercase(string(event))
    @eval begin
        function apply_property(img::SVG, p::($event))
            fn_name = add_script(img, p.value, object_id(p.value))
            @printf(img.out, " %s=\"%s(evt)\"", ($prop_name), fn_name)
        end
    end
end


# Add some javascript. Return the unique name generated for the wrapper function
# containing the given code.
function add_script(img::SVG, js::String, obj_id::Integer)
    fn_name = @sprintf("js_chunk_%x", obj_id)
    if !haskey(img.scripts, fn_name)
        img.scripts[fn_name] = replace(js, r"^"m, "  ")
    end
    fn_name
end


