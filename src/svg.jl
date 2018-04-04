using Compat

const snapsvgjs = joinpath(dirname(@__FILE__), "..", "data", "snap.svg-min.js")

# Packages can insert extra XML namespaces here to be defined in the output
# SVG.
const xmlns = Dict()

# Format a floating point number into a decimal string of reasonable precision.
function svg_fmt_float(x::Fractional)
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

# A much faster version of svg_fmt_float. This does not allocate any
# temporary buffers, because it writes directly to the output.
function svg_print_float(io::IO, x::AbstractFloat)
    const ndig = 2
    const eps = 0.01
    #const eps = 1.0/10^ndig

    if isfinite(x)
        if x < 0
            write(io, '-')
            x = abs(x)
        end
        x = round(x/eps)*eps
        xt = trunc(UInt, x)
        dx = x - convert(Float64, xt)
        0 <= dx < 1 || error("Formatting overflow")
        svg_print_uint(io, xt, 1)  # width=1 prints 0.2 instead of .2
        dxi = round(UInt, dx/eps)
        if dxi != 0
            write(io, '.')
            svg_print_uint(io, dxi, ndig, true)
        end
    elseif isnan(x)
        write(io, "NaN")
    elseif x == Inf
        write(io, "Inf")
    elseif x == -Inf
        write(io, "-Inf")
    end
end

let a = Array{UInt8}(20)
    global svg_print_uint
    function svg_print_uint(io::IO, x::Unsigned, width = 0, drop = false)
        n = length(a)
        while x > 0 && n > 0
            x, r = divrem(x, 10)
            a[n] = r
            n -= 1
        end
        n == 0 && error("Formatting overflow")
        for i = 1:width-(length(a)-n)
            write(io, '0')
        end
        last = length(a)
        if drop
            while last > n && a[last] == 0
                last -= 1
            end
        end
        for i = n+1:last
            write(io, '0'+a[i])
        end
    end
end

# Format a color for SVG.
svg_fmt_color(c::Color) = string("#", hex(c))
svg_fmt_color(c::Nothing) = "none"

# Replace newlines in a string with the appropriate SVG tspan tags.
function svg_newlines(input::AbstractString, x::Float64)
    xpos = svg_fmt_float(x)
    newline_count = 0
    output = IOBuffer()
    lastpos = 1
    for mat in eachmatch(r"\n", input)
        write(output, input[lastpos:mat.offset-1])
        newline_count += 1
        write(output, """<tspan x="$(xpos)" dy="1.2em">""")
        lastpos = mat.offset + length(mat.match)
    end
    write(output, input[lastpos:end])
    for _ in 1:newline_count
        write(output, "</tspan>")
    end

    return String(take!(output))
end

# Javascript in a <script> tag in SVG needs to escape '"' and '<'.
#=function escape_script(js::AbstractString)=#
    #=return replace(replace(js, "&", "&amp;"), "<", "&lt;")=#
#=end=#

# Javascript contained to CDATA block needs to avoid ']]'
escape_script(js::AbstractString) = replace(js, "]]", "] ]")

# When subtree rooted at a context is drawn, it pushes its property children
# in the form of a property frame.
mutable struct SVGPropertyFrame
    # Vector properties in this frame.
    vector_properties::Dict{Type, Property}

    # True if this property frame has scalar properties. Scalar properties are
    # emitted as a group (<g> tag) that must be closed when the frame is popped.
    has_scalar_properties::Bool

    # True if this property frame includes a link (<a> tag) that needs
    # to be closed.
    has_link::Bool

    # True if the property frame include a mask (<mask> tag) than needs to be
    # closed.
    has_mask::Bool

    # True if the property frame has a scalar Clip property, which needs some
    # special handling.
    has_scalar_clip::Bool
end

SVGPropertyFrame() = SVGPropertyFrame(Dict{Type, Property}(), false, false, false, false)

mutable struct SVG <: Backend
    # Image size in millimeters.
    width::AbsoluteLength
    height::AbsoluteLength

    # Output stream.
    out::IO

    # Save output from IOBuffers to allow multiple calls to writemime
    cached_out::Union{AbstractString, Nothing}

    # Unique ID for the figure.
    id::AbstractString

    # Current level of indentation.
    indentation::Int

    # Stack of property frames (groups of properties) currently in effect.
    property_stack::Vector{SVGPropertyFrame}

    # SVG forbids defining the same property twice, so we have to keep track
    # of which vector property of which type is in effect. If two properties of
    # the same type are in effect, the one higher on the stack takes precedence.
    vector_properties::Dict{Type, Union{Property, Nothing}}

    # Clip-paths that need to be defined at the end of the document.
    clippaths::OrderedDict{ClipPrimitive, Compat.String}

    # Batched forms to be included within <def> tags.
    batches::Vector{Tuple{FormPrimitive, Compat.String}}

    # Embedded objects included immediately before the </svg> tag, such as extra
    # javascript or css.
    embobj::Set{AbstractString}

    # True when finish has been called and no more drawing should occur
    finished::Bool

    # Backend is responsible for opening/closing the file
    ownedfile::Bool

    # Filename when ownedfile is true
    filename::Union{AbstractString, Nothing}

    # Emit the graphic on finish when writing to a buffer.
    emit_on_finish::Bool

    # IDs of the SVG element currently being generated. `has_current_id` is
    # false if the element being drawn does not have an id.
    current_id::AbstractString
    has_current_id::Bool

    # A counter used to generate unique IDs
    id_count::Int

    # Filenames of javsacript to include before any JSCall code.
    jsheader::Set{AbstractString}

    # (Name, binding) pairs of javascript modules the embedded code depends on
    jsmodules::Set{Tuple{AbstractString, AbstractString}}

    # User javascript from JSCall attributes
    scripts::Vector{AbstractString}

    # Use javascript extensions to add interactivity, etc.
    withjs::Bool

    # What to do with javascript. One of:
    #    none: generate a static SVG without any javascript
    #    exclude: exclude external javascript libraries
    #    embed: embed external libraries
    #    linkabs: link to external libraries (absolute path)
    #    linkrel: link to external libraries (relative path)
    #
    jsmode::Symbol

    panelcoords::Tuple
end

function SVG(out::IO,
             width::AbsoluteLength,
             height::AbsoluteLength,
             emit_on_finish::Bool=true,
             jsmode::Symbol=:none;

             cached_out = nothing,
             id = string("img-", string(Base.Random.uuid4())[1:8]),
             indentation = 0,
             property_stack = Array{SVGPropertyFrame}(0),
             vector_properties = Dict{Type, Union{Property, Nothing}}(),
             clippaths = OrderedDict{ClipPrimitive, Compat.String}(),
             batches = Array{Tuple{FormPrimitive, Compat.String}}(0),
             embobj = Set{AbstractString}(),
             finished = false,
             ownedfile = false,
             filename = nothing,
             current_id = "",
             has_current_id = false,
             id_count = 0,
             jsheader = Set{AbstractString}(),
             jsmodules = Set{Tuple{AbstractString, AbstractString}}((("Snap.svg", "Snap"),)),  # @compat
             scripts = AbstractString[],
             withjs = jsmode != :none,
             panelcoords = ())

    img = SVG(width,
              height,
              out,
              cached_out,
              id,
              indentation,
              property_stack,
              vector_properties,
              clippaths,
              batches,
              embobj,
              finished,
              ownedfile,
              filename,
              emit_on_finish,
              current_id,
              has_current_id,
              id_count,
              jsheader,
              jsmodules,
              scripts,
              withjs,
              jsmode,
              panelcoords)

    writeheader(img)
    return img
end

# Write to a file.
SVG(filename::AbstractString, width=default_graphic_width, height=default_graphic_height,
        jsmode::Symbol=:none) =
        SVG(open(filename, "w"), width, height, true, jsmode; ownedfile=true, filename=filename)

# Write to buffer.
SVG(width::MeasureOrNumber=default_graphic_width, height::MeasureOrNumber=default_graphic_height,
        emit_on_finish::Bool=true, jsmode::Symbol=:none) =
        SVG(IOBuffer(), width, height, emit_on_finish, jsmode)

canbatch(img::SVG) = true
iswithjs(img::SVG) = img.withjs

function (img::SVG)(x)
    draw(img, x)
end

# Return the next unique element ID. Sort of like gensym for SVG elements.
function genid(img::SVG)
    img.id_count += 1
    return @sprintf("%s-%d", img.id, img.id_count)
end

# Constructors that turn javascript extensions on
SVGJS(out::IO, width=default_graphic_width, height=default_graphic_height,
        emit_on_finish::Bool=true; jsmode::Symbol=:embed) =
        SVG(out, width, height, emit_on_finish, jsmode)
SVGJS(filename::AbstractString, width=default_graphic_width, height=default_graphic_height;
        jsmode::Symbol=:embed) = SVG(filename, width, height, jsmode)
SVGJS(width::MeasureOrNumber=default_graphic_width, height::MeasureOrNumber=default_graphic_height,
        emit_on_finish::Bool=true, jsmode::Symbol=:embed) =
        SVG(width, height, emit_on_finish, jsmode)

function writeheader(img::SVG)
    widthstr = svg_fmt_float(img.width.value)
    heightstr = svg_fmt_float(img.height.value)
    write(img.out,
          """
          <?xml version="1.0" encoding="UTF-8"?>
          <svg xmlns="http://www.w3.org/2000/svg"
               xmlns:xlink="http://www.w3.org/1999/xlink"
          """)

    for (ns, uri) in xmlns
        write(img.out, """     xmlns:$(ns)="$(uri)"\n""")
    end

    write(img.out,
          """
               version="1.2"
               width="$(widthstr)mm" height="$(heightstr)mm" viewBox="0 0 $(widthstr) $(heightstr)"
               stroke="$(svg_fmt_color(default_stroke_color))"
               fill="$(svg_fmt_color(default_fill_color))"
               stroke-width="$(svg_fmt_float(default_line_width.value))"
               font-size="$(svg_fmt_float(default_font_size.value))"
          """)
    img.withjs && write(img.out, "\n     id=\"$(img.id)\"")
    write(img.out, ">\n")
    return img
end

function register_coords(backend::SVG, box, units, transform, property::SVGClass)
    if length(property.primitives) == 1 && property.primitives[1].value == "plotpanel"
        backend.panelcoords = (box, units, transform)
    end
    nothing
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
    img.finished && return

    while !isempty(img.property_stack)
        pop_property_frame(img)
    end

    for obj in img.embobj
        write(img.out, obj)
        write(img.out, "\n")
    end

    # defs
    if !isempty(img.clippaths) | !isempty(img.batches)
        write(img.out, "<defs>\n")
        img.indentation += 1
    end

    for (clippath, id) in img.clippaths
        indent(img)
        img.indentation += 1
        write(img.out, "<clipPath id=\"$(id)\">\n")
        indent(img)
        write(img.out, "<path d=\"")
        print_svg_path(img.out, clippath.points)
        write(img.out, "\" />\n")
        img.indentation -= 1
        indent(img)
        write(img.out, "</clipPath>\n")
    end

    for (primitive, id) in img.batches
        indent(img)
        print(img.out, "<g id=\"", id, "\">\n")
        img.indentation += 1
        draw(img, primitive, 1)
        img.indentation -= 1
        indent(img)
        print(img.out, "</g>\n")
    end

    if !isempty(img.clippaths) | !isempty(img.batches)
        img.indentation -= 1
        write(img.out, "</defs>\n")
    end

    if img.withjs
        if img.jsmode == :embed
            write(img.out,
                """
                <script> <![CDATA[
                $(escape_script(readstring(snapsvgjs)))
                ]]> </script>
                """)
        elseif img.jsmode == :linkabs
            write(img.out,
                """
                <script xlink:href="$(snapsvgjs)"></script>
                """)
        elseif img.jsmode == :linkrel
            write(img.out,
                """
                <script xlink:href="$(basename(snapsvgjs))"></script>
                """)
        end

        if !isempty(img.scripts) || !isempty(img.jsheader)
            if img.jsmode == :embed
                write(img.out, "<script> <![CDATA[\n")
                for script in img.jsheader
                    write(img.out, escape_script(readstring(script)), "\n")
                end
            elseif img.jsmode == :linkabs
                for script in img.jsheader
                    write(img.out,
                        """
                        <script xlink:href="$(script)"></script>
                        """)
                end
                write(img.out, "<script> <![CDATA[\n")
            elseif img.jsmode == :linkrel
                for script in img.jsheader
                    write(img.out,
                        """
                        <script xlink:href="$(basename(script))"></script>
                        """)
                end
                write(img.out, "<script> <![CDATA[\n")
            end

            mod_names = join([string("\"", name, "\"") for (name, binding) in img.jsmodules], ", ")
            mod_bindings = join([binding for (name, binding) in img.jsmodules], ", ")
            glob_mod_bindings = join([string("glob.", binding)
                                      for (name, binding) in img.jsmodules], ", ")

            write(img.out,
                """
                (function (glob, factory) {
                    // AMD support
                      if (typeof require === "function" && typeof define === "function" && define.amd) {
                        require([$(mod_names)], function ($(mod_bindings)) {
                            factory($(mod_bindings));
                        });
                      } else {
                          factory($(glob_mod_bindings));
                      }
                })(window, function ($(mod_bindings)) {
                    var fig = Snap(\"#$(img.id)\");
                """)

            for script in img.scripts
                write(img.out, escape_script(script), "\n")
            end

            write(img.out,
                """
                    });
                """)

            write(img.out, "]]> </script>\n")
        end

    end

    write(img.out, "</svg>\n")
    hasmethod(flush, (typeof(img.out),)) && flush(img.out)

    img.ownedfile && close(img.out)

    img.finished = true

    # If we are writing to a buffer. Collect the string and emit it.
    img.emit_on_finish && typeof(img.out) == IOBuffer && display(img)
end

isfinished(img::SVG) = img.finished

function show(io::IO, ::MIME"text/html", img::SVG)
    if img.cached_out === nothing
        img.cached_out = String(take!(img.out))
    end
    write(io, img.cached_out)
end

function show(io::IO, ::MIME"image/svg+xml", img::SVG)
    if img.cached_out === nothing
        img.cached_out = String(take!(img.out))
    end
    write(io, img.cached_out)
end

root_box(img::SVG) = BoundingBox(0mm, 0mm, img.width, img.height)

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
function print_svg_path(out, points::Vector{AbsoluteVec2},
                        bridge_gaps::Bool=false)
    isfirst = true
    for point in points
        x, y = point[1].value, point[2].value
        if !(isfinite(x) && isfinite(y))
            isfirst = true
            continue
        end

        if isfirst
            isfirst = false
            write(out, 'M')
            svg_print_float(out, x)
            write(out, ',')
            svg_print_float(out, y)
            write(out, " L")
        else
            write(out, ' ')
            svg_print_float(out, x)
            write(out, ' ')
            svg_print_float(out, y)
        end
    end
end

# TODO: This logic should be moved to Gadfly
# Return array of paths to draw with printpath
# array is formed by splitting by NaN values
#function make_paths(points::Vector{AbsoluteVec})
    #paths = Vector{AbsoluteVec}[]
    #nans = find(xy -> isnan(xy[1]) || isnan(xy[2]),
                #[(point[1].value, point[2].value) for point in points])
    #if length(nans) == 0
        #push!(paths, points)
    #else
        #nans = [0, nans..., length(points) + 1]
        #i, n = 1, length(nans)
        #while i <= n-1
            #if nans[i] + 1 < nans[i + 1]
                #push!(paths, points[(nans[i]+1):(nans[i+1] - 1)])
            #end
            #i += 1
        #end
    #end
    #paths
#end


# Property Printing
# -----------------

function print_property(img::SVG, property::StrokePrimitive)
    if property.color.alpha != 1.0
        @printf(img.out, " stroke=\"%s\" stroke-opacity=\"%0.3f\"",
                svg_fmt_color(color(property.color)), property.color.alpha)
    else
        @printf(img.out, " stroke=\"%s\"", svg_fmt_color(color(property.color)))
    end
end

function print_property(img::SVG, property::FillPrimitive)
    if property.color.alpha != 1.0
        @printf(img.out, " fill=\"%s\" fill-opacity=\"%0.3f\"",
                svg_fmt_color(color(property.color)), property.color.alpha)
    else
        @printf(img.out, " fill=\"%s\"", svg_fmt_color(color(property.color)))
    end
end

function print_property(img::SVG, property::StrokeDashPrimitive)
    if isempty(property.value)
        print(img.out, " stroke-dasharray=\"none\"")
    else
        print(img.out, " stroke-dasharray=\"")
        svg_print_float(img.out, property.value[1].value)
        for i in 2:length(property.value)
            print(img.out, ',')
            svg_print_float(img.out, property.value[i].value)
        end
        print(img.out, '"')
    end
end

# Format a line-cap specifier into the attribute string that SVG expects.
svg_fmt_linecap(::LineCapButt) = "butt"
svg_fmt_linecap(::LineCapSquare) = "square"
svg_fmt_linecap(::LineCapRound) = "round"

print_property(img::SVG, property::StrokeLineCapPrimitive) =
        @printf(img.out, " stroke-linecap=\"%s\"", svg_fmt_linecap(property.value))

# Format a line-join specifier into the attribute string that SVG expects.
svg_fmt_linejoin(::LineJoinMiter) = "miter"
svg_fmt_linejoin(::LineJoinRound) = "round"
svg_fmt_linejoin(::LineJoinBevel) = "bevel"

print_property(img::SVG, property::StrokeLineJoinPrimitive) =
        @printf(img.out, " stroke-linejoin=\"%s\"", svg_fmt_linejoin(property.value))

function print_property(img::SVG, property::LineWidthPrimitive)
    print(img.out, " stroke-width=\"")
    svg_print_float(img.out, property.value.value)
    print(img.out, '"')
end

function print_property(img::SVG, property::FillOpacityPrimitive)
    print(img.out, " opacity=\"")
    svg_print_float(img.out, property.value)
    print(img.out, '"')
end

function print_property(img::SVG, property::StrokeOpacityPrimitive)
    print(img.out, " stroke-opacity=\"")
    svg_print_float(img.out, property.value)
    print(img.out, '"')
end

print_property(img::SVG, property::VisiblePrimitive) =
        @printf(img.out, " visibility=\"%s\"", property.value ? "visible" : "hidden")

# I may end up applying the same clip path to many forms separately, so I
# shouldn't make a new one for each applicaiton. Where should that happen?
function print_property(img::SVG, property::ClipPrimitive)
    url = clippathurl(img, property)
    @printf(img.out, " clip-path=\"url(#%s)\"", url)
end

print_property(img::SVG, property::FontPrimitive) =
        @printf(img.out, " font-family=\"%s\"", escape_string(property.family))

function print_property(img::SVG, property::FontSizePrimitive)
    print(img.out, " font-size=\"")
    svg_print_float(img.out, property.value.value)
    print(img.out, '"')
end

print_property(img::SVG, property::SVGIDPrimitive) =
        @printf(img.out, " id=\"%s\"", escape_string(property.value))
print_property(img::SVG, property::SVGClassPrimitive) =
        @printf(img.out, " class=\"%s\"", escape_string(property.value))
print_property(img::SVG, property::SVGAttributePrimitive) =
        @printf(img.out, " %s=\"%s\"", property.attribute, escape_string(property.value))

function print_property(img::SVG, property::JSIncludePrimitive)
    push!(img.jsheader, property.value)
    if property.jsmodule != nothing
        push!(img.jsmodules, property.jsmodule)
    end
end

function print_property(img::SVG, property::JSCallPrimitive)
    @assert img.has_current_id
    push!(img.scripts, @sprintf("fig.select(\"#%s\")\n   .%s;", img.current_id, property.code))
end

# Print the property at the given index in each vector property
function print_vector_properties(img::SVG, idx::Int, supress_fill::Bool=false)
    if haskey(img.vector_properties, JSCall)
        if haskey(img.vector_properties, SVGID)
            img.current_id = img.vector_properties[SVGID].primitives[idx].value
        else
            img.current_id = genid(img)
            print_property(img, SVGIDPrimitive(img.current_id))
        end
        img.has_current_id = true
    end

    has_stroke_opacity = haskey(img.vector_properties, StrokeOpacity)
    has_fill_opacity = haskey(img.vector_properties, FillOpacity)

    for (propertytype, property) in img.vector_properties
        if property === nothing ||
           (propertytype == Fill && supress_fill)
            continue
        end

        idx > length(property.primitives) &&
                error("Vector form and vector property differ in length. Can't distribute.")

        # let the opacity primitives clobber the alpha value in fill and stroke
        if propertytype == Fill && has_fill_opacity
           print_property(img, FillPrimitive(RGBA{Float64}(property.primitives[idx].color, 1.0)))
        elseif propertytype == Stroke && has_stroke_opacity
           print_property(img, StrokePrimitive(RGBA{Float64}(property.primitives[idx].color, 1.0)))
        else
            print_property(img, property.primitives[idx])
        end
    end

    img.has_current_id = false
end


# Form Drawing
# ------------

function draw(img::SVG, form::Form{T}) where T
    for i in 1:length(form.primitives)
        draw(img, form.primitives[i], i)
    end
end

function draw(img::SVG, prim::RectanglePrimitive, idx::Int)
    # SVG will hide rectangles with zero height or width. We'd prefer to have
    # zero width/height rectangles stroked, so this is a work-around.
    width = max(prim.width, 0.01mm)
    height = max(prim.height, 0.01mm)

    x0 = prim.corner[1] + width/2
    y0 = prim.corner[2] + height/2
    translated_path = [(-width/2,-height/2), ( width/2,-height/2),
                       ( width/2, height/2), (-width/2, height/2)]

    indent(img)

    img.indentation += 1
    print(img.out, "<g transform=\"translate(")
    svg_print_float(img.out, x0.value)
    print(img.out, ",")
    svg_print_float(img.out, y0.value)
    print(img.out, ")\"")
    print_vector_properties(img, idx)
    print(img.out, ">\n")
    indent(img)

    print(img.out, "<path d=\"")
    print_svg_path(img.out, translated_path)
    write(img.out, " z\"")
    print(img.out, " class=\"primitive\"")
    print(img.out, "/>\n")

    img.indentation -= 1
    indent(img)
    print(img.out, "</g>\n")
end

function draw(img::SVG, prim::PolygonPrimitive, idx::Int)
    n = length(prim.points)
    n <= 1 && return

    x0, y0 = prim.points[1][1], prim.points[1][2]
    for p in prim.points[2:end]
        x0 += p[1]
        y0 += p[2]
    end
    x0, y0 = x0/n, y0/n
    translated_path = [(p[1]-x0,p[2]-y0) for p in prim.points]

    indent(img)

    img.indentation += 1
    print(img.out, "<g transform=\"translate(")
    svg_print_float(img.out, x0.value)
    print(img.out, ",")
    svg_print_float(img.out, y0.value)
    print(img.out, ")\"")
    print_vector_properties(img, idx)
    print(img.out, ">\n")
    indent(img)

    write(img.out, "<path d=\"")
    print_svg_path(img.out, translated_path, true)
    write(img.out, " z\"")
    print(img.out, " class=\"primitive\"")
    write(img.out, "/>\n")

    img.indentation -= 1
    indent(img)
    print(img.out, "</g>\n")
end

function draw(img::SVG, prim::ComplexPolygonPrimitive, idx::Int)
    Compose.write(img.out, "<path d=\"")
    for ring in prim.rings
        indent(img)
        print_svg_path(img.out, ring, true)
        write(img.out, " ")
    end
    write(img.out, " z\"")
    print_vector_properties(img, idx)
    write(img.out, "/>\n")
end

function draw(img::SVG, prim::CirclePrimitive, idx::Int)
    indent(img)

    img.indentation += 1
    print(img.out, "<g transform=\"translate(")
    svg_print_float(img.out, prim.center[1].value)
    print(img.out, ",")
    svg_print_float(img.out, prim.center[2].value)
    print(img.out, ")\"")
    print_vector_properties(img, idx)
    print(img.out, ">\n")
    indent(img)

    print(img.out, "<circle cx=\"0\" cy=\"0\" r=\"")
    svg_print_float(img.out, prim.radius.value)
    print(img.out, "\"")
    print(img.out, " class=\"primitive\"")
    print(img.out, "/>\n")

    img.indentation -= 1
    indent(img)
    print(img.out, "</g>\n")
end

function draw(img::SVG, prim::EllipsePrimitive, idx::Int)
    cx = prim.center[1].value
    cy = prim.center[2].value
    rx = sqrt((prim.x_point[1].value - cx)^2 +
              (prim.x_point[2].value - cy)^2)
    ry = sqrt((prim.y_point[1].value - cx)^2 +
              (prim.y_point[2].value - cy)^2)
    theta = rad2deg(atan2(prim.x_point[2].value - cy,
                          prim.x_point[1].value - cx))

    all(isfinite,[cx, cy, rx, ry, theta]) || return

    indent(img)

    img.indentation += 1
    print(img.out, "<g transform=\"translate(")
    svg_print_float(img.out, cx)
    print(img.out, ",")
    svg_print_float(img.out, cy)
    print(img.out, ")\"")
    print_vector_properties(img, idx)
    print(img.out, ">\n")
    indent(img)

    print(img.out, "<ellipse cx=\"0\" cy=\"0\" rx=\"")
    svg_print_float(img.out, rx)
    print(img.out, "\" ry=\"")
    svg_print_float(img.out, ry)
    print(img.out, '"')
    if abs(theta) > 1e-4
        print(img.out, " transform=\"rotate(")
        svg_print_float(img.out, theta)
        print(img.out, ")\"")

    end
    print(img.out, " class=\"primitive\"")
    print(img.out, "/>\n")

    img.indentation -= 1
    indent(img)
    print(img.out, "</g>\n")
end

function draw(img::SVG, prim::LinePrimitive, idx::Int)
    length(prim.points)<=1 && return

    x0, y0, n = prim.points[1][1], prim.points[1][2], 1
    for p in prim.points[2:end]
        if isfinite(p[1].value) && isfinite(p[2].value)
            x0 += p[1]
            y0 += p[2]
            n +=1
        end
    end
    x0, y0 = x0/n, y0/n
    translated_path = [(p[1]-x0,p[2]-y0) for p in prim.points]

    indent(img)

    img.indentation += 1
    print(img.out, "<g transform=\"translate(")
    svg_print_float(img.out, x0.value)
    print(img.out, ",")
    svg_print_float(img.out, y0.value)
    print(img.out, ")\"")
    print_vector_properties(img, idx, true)
    print(img.out, ">\n")
    indent(img)

    print(img.out, "<path fill=\"none\" d=\"")
    print_svg_path(img.out, translated_path, true)
    print(img.out, "\"")
    print(img.out, " class=\"primitive\"")
    print(img.out, "/>\n")

    img.indentation -= 1
    indent(img)
    print(img.out, "</g>\n")
end

function draw(img::SVG, prim::TextPrimitive, idx::Int)
    indent(img)

    img.indentation += 1
    print(img.out, "<g transform=\"translate(")
    svg_print_float(img.out, prim.position[1].value)
    print(img.out, ",")
    svg_print_float(img.out, prim.position[2].value)
    print(img.out, ")\"")
    print_vector_properties(img, idx, true)
    print(img.out, ">\n")
    indent(img)

    print(img.out, "<g class=\"primitive\">\n")
    img.indentation += 1
    indent(img)
    print(img.out, "<text")

    if prim.halign === hcenter
        print(img.out, " text-anchor=\"middle\"")
    elseif prim.halign === hright
        print(img.out, " text-anchor=\"end\"")
    end

    # NOTE: "dominant-baseline" is the correct way to vertically center text
    # in SVG, but implementations are pretty inconsistent (chrome in particular
    # does a really bad job). We fake it by shifting by some reasonable amount.
    if prim.valign === vcenter
        print(img.out, " dy=\"0.35em\"")
        #print(img.out, " style=\"dominant-baseline:central\"")
    elseif prim.valign === vtop
        print(img.out, " dy=\"0.6em\"")
        #print(img.out, " style=\"dominant-baseline:text-before-edge\"")
    end

    if abs(prim.rot.theta) > 1e-4 || sum(abs.(prim.offset)) > 1e-4mm
        print(img.out, " transform=\"")
        if abs(prim.rot.theta) > 1e-4
            print(img.out, "rotate(")
            svg_print_float(img.out, rad2deg(prim.rot.theta))
            print(img.out, ",")
            svg_print_float(img.out, prim.rot.offset[1].value-prim.position[1].value)
            print(img.out, ", ")
            svg_print_float(img.out, prim.rot.offset[2].value-prim.position[2].value)
            print(img.out, ")")
        end
        if sum(abs.(prim.offset)) > 1e-4mm
            print(img.out, "translate(")
            svg_print_float(img.out, prim.offset[1].value)
            print(img.out, ",")
            svg_print_float(img.out, prim.offset[2].value)
            print(img.out, ")")
        end
        print(img.out, "\"")
    end

    @printf(img.out, ">%s</text>\n",
            svg_newlines(pango_to_svg(prim.value), prim.position[1].value))

    img.indentation -= 1
    indent(img)
    print(img.out, "</g>\n")
    img.indentation -= 1
    indent(img)
    print(img.out, "</g>\n")
end

function draw(img::SVG, prim::CurvePrimitive, idx::Int)
    x0, y0 = prim.anchor0[1], prim.anchor0[2]
    x0, y0 += prim.ctrl0[1], prim.ctrl0[2]
    x0, y0 += prim.ctrl1[1], prim.ctrl1[2]
    x0, y0 += prim.anchor1[1], prim.anchor1[2]
    x0, y0 = x0/4, y0/4
    translated_anchor0 = [prim.anchor0...] - [x0,y0]
    translated_ctrl0 = [prim.ctrl0...] - [x0,y0]
    translated_ctrl1 = [prim.ctrl1...] - [x0,y0]
    translated_anchor1 = [prim.anchor1...] - [x0,y0]

    indent(img)

    img.indentation += 1
    print(img.out, "<g transform=\"translate(")
    svg_print_float(img.out, x0.value)
    print(img.out, ",")
    svg_print_float(img.out, y0.value)
    print(img.out, ")\"")
    print_vector_properties(img, idx, true)
    print(img.out, ">\n")
    indent(img)

    print(img.out, "<path fill=\"none\" d=\"M")
    svg_print_float(img.out, translated_anchor0[1].value)
    print(img.out, ',')
    svg_print_float(img.out, translated_anchor0[2].value)
    print(img.out, " C")
    svg_print_float(img.out, translated_ctrl0[1].value)
    print(img.out, ',')
    svg_print_float(img.out, translated_ctrl0[2].value)
    print(img.out, ' ')
    svg_print_float(img.out, translated_ctrl1[1].value)
    print(img.out, ',')
    svg_print_float(img.out, translated_ctrl1[2].value)
    print(img.out, ' ')
    svg_print_float(img.out, translated_anchor1[1].value)
    print(img.out, ',')
    svg_print_float(img.out, translated_anchor1[2].value)
    print(img.out, '"')
    print(img.out, " class=\"primitive\"")
    print(img.out, "/>\n")

    img.indentation -= 1
    indent(img)
    print(img.out, "</g>\n")
end

function draw(img::SVG, prim::BitmapPrimitive, idx::Int)
    indent(img)

    img.indentation += 1
    print(img.out, "<g transform=\"translate(")
    svg_print_float(img.out, prim.corner[1].value)
    print(img.out, ",")
    svg_print_float(img.out, prim.corner[2].value)
    print(img.out, ")\"")
    print_vector_properties(img, idx, true)
    print(img.out, ">\n")
    indent(img)

    print(img.out, "<image x=\"0\" y=\"0\" width=\"")
    svg_print_float(img.out, prim.width.value)
    print(img.out, "\" height=\"")
    svg_print_float(img.out, prim.height.value)
    print(img.out, '"')
    print_vector_properties(img, idx)

    print(img.out, " xlink:href=\"data:", prim.mime, ";base64,")
    b64pipe = Base64EncodePipe(img.out)
    write(b64pipe, prim.data)
    close(b64pipe)
    print(img.out, "\"")
    print(img.out, " class=\"primitive\"")
    print(img.out, "/>\n")

    img.indentation -= 1
    indent(img)
    print(img.out, "</g>\n")
end

function svg_print_path_op(io::IO, op::MoveAbsPathOp)
    print(io, 'M')
    svg_print_float(io, op.to[1].value)
    print(io, ' ')
    svg_print_float(io, op.to[2].value)
end

function svg_print_path_op(io::IO, op::MoveRelPathOp)
    print(io, 'm')
    svg_print_float(io, op.to[1].value)
    print(io, ' ')
    svg_print_float(io, op.to[2].value)
end

svg_print_path_op(io::IO, op::ClosePathOp) = print(io, 'z')

function svg_print_path_op(io::IO, op::LineAbsPathOp)
    print(io, 'L')
    svg_print_float(io, op.to[1].value)
    print(io, ' ')
    svg_print_float(io, op.to[2].value)
end

function svg_print_path_op(io::IO, op::LineRelPathOp)
    print(io, 'l')
    svg_print_float(io, op.to[1].value)
    print(io, ' ')
    svg_print_float(io, op.to[2].value)
end

function svg_print_path_op(io::IO, op::HorLineAbsPathOp)
    print(io, 'H')
    svg_print_float(io, op.x.value)
end

function svg_print_path_op(io::IO, op::HorLineRelPathOp)
    print(io, 'h')
    svg_print_float(io, op.Δx.value)
end

function svg_print_path_op(io::IO, op::VertLineAbsPathOp)
    print(io, 'V')
    svg_print_float(io, op.y.value)
end

function svg_print_path_op(io::IO, op::VertLineRelPathOp)
    print(io, 'v')
    svg_print_float(io, op.Δy.value)
end

function svg_print_path_op(io::IO, op::CubicCurveAbsPathOp)
    print(io, 'C')
    svg_print_float(io, op.ctrl1[1].value)
    print(io, ' ')
    svg_print_float(io, op.ctrl1[2].value)
    print(io, ' ')
    svg_print_float(io, op.ctrl2[1].value)
    print(io, ' ')
    svg_print_float(io, op.ctrl2[2].value)
    print(io, ' ')
    svg_print_float(io, op.to[1].value)
    print(io, ' ')
    svg_print_float(io, op.to[2].value)
end

function svg_print_path_op(io::IO, op::CubicCurveRelPathOp)
    print(io, 'c')
    svg_print_float(io, op.ctrl1[1].value)
    print(io, ' ')
    svg_print_float(io, op.ctrl1[2].value)
    print(io, ' ')
    svg_print_float(io, op.ctrl2[1].value)
    print(io, ' ')
    svg_print_float(io, op.ctrl2[2].value)
    print(io, ' ')
    svg_print_float(io, op.to[1].value)
    print(io, ' ')
    svg_print_float(io, op.to[2].value)
end

function svg_print_path_op(io::IO, op::CubicCurveShortAbsPathOp)
    print(io, 'S')
    svg_print_float(io, op.ctrl2[1].value)
    print(io, ' ')
    svg_print_float(io, op.ctrl2[2].value)
    print(io, ' ')
    svg_print_float(io, op.to[1].value)
    print(io, ' ')
    svg_print_float(io, op.to[2].value)
end

function svg_print_path_op(io::IO, op::CubicCurveShortRelPathOp)
    print(io, 's')
    svg_print_float(io, op.ctrl2[1].value)
    print(io, ' ')
    svg_print_float(io, op.ctrl2[2].value)
    print(io, ' ')
    svg_print_float(io, op.to[1].value)
    print(io, ' ')
    svg_print_float(io, op.to[2].value)
end

function svg_print_path_op(io::IO, op::QuadCurveAbsPathOp)
    print(io, 'Q')
    svg_print_float(io, op.ctrl1[1].value)
    print(io, ' ')
    svg_print_float(io, op.ctrl1[2].value)
    print(io, ' ')
    svg_print_float(io, op.to[1].value)
    print(io, ' ')
    svg_print_float(io, op.to[2].value)
end

function svg_print_path_op(io::IO, op::QuadCurveRelPathOp)
    print(io, 'q')
    svg_print_float(io, op.ctrl1[1].value)
    print(io, ' ')
    svg_print_float(io, op.ctrl1[2].value)
    print(io, ' ')
    svg_print_float(io, op.to[1].value)
    print(io, ' ')
    svg_print_float(io, op.to[2].value)
end

function svg_print_path_op(io::IO, op::QuadCurveShortAbsPathOp)
    print(io, 'T')
    svg_print_float(io, op.to[1].value)
    print(io, ' ')
    svg_print_float(io, op.to[2].value)
end

function svg_print_path_op(io::IO, op::QuadCurveShortRelPathOp)
    print(io, 't')
    svg_print_float(io, op.to[1].value)
    print(io, ' ')
    svg_print_float(io, op.to[2].value)
end

function svg_print_path_op(io::IO, op::ArcAbsPathOp)
    print(io, 'A')
    svg_print_float(io, op.rx.value)
    print(io, ' ')
    svg_print_float(io, op.ry.value)
    print(io, ' ')
    svg_print_float(io, op.rotation)
    print(io, ' ',
          op.largearc ? 1 : 0, ' ',
          op.sweep ? 1 : 0, ' ')
    svg_print_float(io, op.to[1].value)
    print(io, ' ')
    svg_print_float(io, op.to[2].value)
end

function svg_print_path_op(io::IO, op::ArcRelPathOp)
    print(io, 'a')
    svg_print_float(io, op.rx.value)
    print(io, ' ')
    svg_print_float(io, op.ry.value)
    print(io, ' ')
    svg_print_float(io, op.rotation)
    print(io, ' ',
          op.largearc ? 1 : 0, ' ',
          op.sweep ? 1 : 0, ' ')
    svg_print_float(io, op.to[1].value)
    print(io, ' ')
    svg_print_float(io, op.to[2].value)
end

function draw(img::SVG, prim::PathPrimitive, idx::Int)
    indent(img)
    print(img.out, "<path d=\"")
    for op in prim.ops
        svg_print_path_op(img.out, op)
    end
    print(img.out, '"')
    print_vector_properties(img, idx)
    print(img.out, "/>\n")
end


# FormBatch Drawing
# -----------------

function draw(img::SVG, batch::FormBatch)
    id = genid(img)
    push!(img.batches, (batch.primitive, id))
    for i in 1:length(batch.offsets)
        indent(img)
        print(img.out, "<use xlink:href=\"#", id, "\" x=\"")
        svg_print_float(img.out, batch.offsets[i][1].value)
        print(img.out, "\" y=\"")
        svg_print_float(img.out, batch.offsets[i][2].value)
        print(img.out, "\"")
        print_vector_properties(img, i)
        print(img.out, "/>\n")
    end
end


# Applying properties
# -------------------

# Return a URL corresponding to a ClipPrimitive
clippathurl(img::SVG, property::ClipPrimitive) = get!(() -> genid(img), img.clippaths, property)

function push_property_frame(img::SVG, properties::Vector{Property})
    isempty(properties) && return

    frame = SVGPropertyFrame()
    applied_properties = Set{Type}()
    scalar_properties = Array{Property}(0)
    isplotpanel = false
    for property in properties
        if !isrepeatable(property) && (typeof(property) in applied_properties)
            continue
        elseif isscalar(property) && isa(property, Clip)
            # clip-path needs to be in it's own group. Otherwise it can cause
            # problems if we apply a transform to the group.
            indent(img)
            img.indentation += 1
            write(img.out, "<g")
            print_property(img, property.primitives[1])
            write(img.out, ">\n")
            frame.has_scalar_clip = true
        elseif isscalar(property)
            push!(scalar_properties, property)
            push!(applied_properties, typeof(property))
            frame.has_scalar_properties = true
        else
            frame.vector_properties[typeof(property)] = property
            img.vector_properties[typeof(property)] = property
        end
        isplotpanel |= typeof(property)==SVGClass &&
                length(property.primitives)==1 && property.primitives[1].value=="plotpanel"
    end
    push!(img.property_stack, frame)
    isempty(scalar_properties) && return

    id_needed = any([isa(property, JSCall) for property in scalar_properties])
    for property in scalar_properties
        if isa(property, SVGID)
            img.current_id = property.primitives[1].value
            img.has_current_id = true
        end
    end

    if !img.has_current_id
        img.current_id = genid(img)
        push!(scalar_properties, svgid(img.current_id))
        img.has_current_id = true
    end

    indent(img)
    write(img.out, "<g")
    for property in scalar_properties
        print_property(img, property.primitives[1])
    end
    write(img.out, ">\n");
    img.has_current_id = false
    img.indentation += 1

    if isplotpanel
        indent(img)
        write(img.out, "<metadata>\n")
        boundingBox = string(img.panelcoords[1].x0[1], ' ', img.panelcoords[1].x0[2], ' ',
                          img.panelcoords[1].a[1], ' ', img.panelcoords[1].a[2])
        unitBox = string(img.panelcoords[2].x0, ' ', img.panelcoords[2].y0, ' ',
                          img.panelcoords[2].width, ' ', img.panelcoords[2].height)
        indent(img)
        write(img.out, "  <boundingbox value=\"$boundingBox\"/>\n")
        indent(img)
        write(img.out, "  <unitbox value=\"$unitBox\"/>\n")
        indent(img)
        write(img.out, "</metadata>\n")
    end
end

function pop_property_frame(img::SVG)
    @assert !isempty(img.property_stack)
    frame = pop!(img.property_stack)

    if frame.has_scalar_properties
        img.indentation -= 1
        indent(img)
        write(img.out, "</g>")
        frame.has_link && write(img.out, "</a>")
        frame.has_mask && write(img.out, "</mask>")
        write(img.out, "\n")
    end

    if frame.has_scalar_clip
        img.indentation -= 1
        indent(img)
        write(img.out, "</g>\n")
    end

    for (propertytype, property) in frame.vector_properties
        img.vector_properties[propertytype] = nothing
        for i in length(img.property_stack):-1:1
            if haskey(img.property_stack[i].vector_properties, propertytype)
                img.vector_properties[propertytype] =
                    img.property_stack[i].vector_properties[propertytype]
            end
        end
    end
end
