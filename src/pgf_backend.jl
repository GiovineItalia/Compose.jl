type PGFPropertyFrame
    # Vector properties in this frame.
    vector_properties::Dict{Type, Any}

    # True if this property frame has scalar properties. Scalar properties are
    # emitted as a group {scope} that must be closed when the frame is popped.
    has_scalar_properties::Bool

    function PGFPropertyFrame()
        return new(Dict{Type, Any}(), false)
    end
end

type PGF <: Backend
    # Image size in millimeters.
    width::AbsoluteLength
    height::AbsoluteLength

    # Output stream.
    out::IO

    # Fill properties cannot be "cleanly" applied to
    # multiple form primitives.  It must be applied
    # each time an object is drawn
    fill::Nullable{Color}
    fill_opacity::Float64

    stroke::Nullable{Color}
    stroke_opacity::Float64

    fontfamily::Nullable{AbstractString}
    fontsize::Float64

    # Current level of indentation.
    indentation::Int

    # Output buffer.  We want the ability to add to the beginning of
    buf::IOBuffer

    # Skip drawing if visible is false
    visible::Bool

    # Have not found an easy way to define color as
    # a draw parameter.  Whenever we encounter a color, we add it to the
    # color_set set.  That way, we can write out all the color
    # definitions at the same time.
    color_set::Set{Color}

    # Stack of property frames (groups of properties) currently in effect.
    property_stack::Vector{PGFPropertyFrame}

    # SVG forbids defining the same property twice, so we have to keep track
    # of which vector property of which type is in effect. If two properties of
    # the same type are in effect, the one higher on the stack takes precedence.
    vector_properties::Dict{Type, Nullable{PropertyNode}}

    # Clip-paths that need to be defined at the end of the document.
    # Not quite sure how to deal with clip paths yet
    clippath::Nullable{ClipPrimitive}
    # clippaths::Dict{ClipPrimitive, String}

    # True when finish has been called and no more drawing should occur
    finished::Bool

    # Backend is responsible for opening/closing the file
    ownedfile::Bool

    # Filename when ownedfile is true
    filename::Nullable{AbstractString}

    # Emit the graphic on finish when writing to a buffer.
    emit_on_finish::Bool

    # Emit only the tikzpicture environment
    only_tikz :: Bool

    # Use default TeX fonts instead of fonts specified by the theme.
    texfonts::Bool

    function PGF(out::IO,
                 width::AbsoluteLength,
                 height::AbsoluteLength,
                 emit_on_finish::Bool=true,
                 only_tikz = false;
                 texfonts = false)

        img = new()
        img.buf = IOBuffer()
        img.fill = default_fill_color
        img.stroke = default_stroke_color
        img.fill_opacity = 1.0
        img.stroke_opacity = 1.0
        img.width  = width
        img.height = height
        img.fontfamily = nothing
        img.clippath = nothing
        img.fontsize = 12.0
        img.indentation = 0
        img.out = out
        img.color_set = Set{Color}([colorant"black"])
        img.property_stack = Array(PGFPropertyFrame, 0)
        img.vector_properties = Dict{Type, Nullable{PropertyNode}}()
        # img.clippaths = Dict{ClipPrimitive, String}()
        img.visible = true
        img.finished = false
        img.emit_on_finish = emit_on_finish
        img.ownedfile = false
        img.filename = nothing
        img.only_tikz = only_tikz
        img.texfonts = texfonts
        return img
    end

    # Write to a file.
    function PGF(filename::AbstractString, width, height, only_tikz = false; texfonts = false)
        out = open(filename, "w")
        img = PGF(out, width, height, true, only_tikz, texfonts = texfonts)
        img.ownedfile = true
        img.filename = filename
        return img
    end

    # Write to buffer.
    function PGF(width::MeasureOrNumber, height::MeasureOrNumber,
                 emit_on_finish::Bool=true, only_tikz = false; texfonts = false)
        return PGF(IOBuffer(), width, height, emit_on_finish, only_tikz, texfonts = texfonts)
    end
end

function finish(img::PGF)
    if img.finished
        return
    end

    while !isempty(img.property_stack)
        pop_property_frame(img)
    end


    # if length(img.clippaths) > 0
    #     for (clippath, id) in img.clippaths
    #         write(img.out, "\\clip")
    #         print_svg_path(img.out, clippath.points)
    #         write(img.out, ";\n")
    #     end
    # end

    writeheader(img)
    writecolors(img)
    write(img.out, takebuf_array(img.buf))
    write(img.out,
        """
        \\end{tikzpicture}
        """
        )
    !img.only_tikz &&  write(img.out,
        """
        \\end{document}
        """
        )

    if method_exists(flush, (typeof(img.out),))
        flush(img.out)
    end
    close(img.buf)

    if img.ownedfile
        close(img.out)
    end

    img.finished = true

    # If we are writing to a buffer. Collect the string and emit it.
    if img.emit_on_finish && typeof(img.out) == IOBuffer
        display(img)
    end
end


function isfinished(img::PGF)
    img.finished
end

function root_box(img::PGF)
    BoundingBox(0mm, 0mm, img.width, img.height)
end

function writeheader(img::PGF)
    !img.only_tikz && write(img.out,
        """
        \\documentclass{minimal}
        \\usepackage{pgfplots}
        $(img.texfonts ? "" : "\\usepackage{fontspec}")
        \\usepackage{amsmath}
        \\usepackage[active,tightpage]{preview}
        \\PreviewEnvironment{tikzpicture}
        \\begin{document}
        """)
    write(img.out, """
        \\begin{tikzpicture}[x=1mm,y=-1mm]
        """)
    return img
end

function writecolors(img::PGF)
    for color in img.color_set
        @printf(img.out, "\\definecolor{mycolor%s}{rgb}{%s,%s,%s}\n",
            hex(color),
            svg_fmt_float(color.r),
            svg_fmt_float(color.g),
            svg_fmt_float(color.b)
            )
    end
end

function print_pgf_path(out::IO, points::Vector{AbsoluteVec2},
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
            @printf(out, " (%s,%s)",
                    svg_fmt_float(x),
                    svg_fmt_float(y))
        else
            @printf(out, " -- (%s,%s)",
                    svg_fmt_float(x),
                    svg_fmt_float(y))
        end
    end
end

function get_vector_properties(img::PGF, idx::Int)
    props_str = ASCIIString[]
    modifiers = ASCIIString[]
    for (propertytype, property) in img.vector_properties
        if isnull(property)
            continue
        end
        primitives = get(property).primitives
        if idx > length(primitives)
            error("Vector form and vector property differ in length. Can't distribute.")
        end
        push_property!(props_str, img, primitives[idx])
    end

    if !isnull(img.fill)
        push!(props_str, string("fill=mycolor",hex(get(img.fill))))
        if img.fill_opacity < 1.0
            push!(props_str, string("fill opacity=",svg_fmt_float(img.fill_opacity)))
        end
    end

    if !isnull(img.stroke)
        push!(props_str, string("draw=mycolor",hex(get(img.stroke))))
        if img.stroke_opacity < 1.0
            push!(props_str, string("draw opacity=",svg_fmt_float(img.stroke_opacity)))
        end
    end

    return modifiers, props_str
end

function push_property!(props_str, img::PGF, property::StrokeDashPrimitive)
    if isempty(property.value)
        return
    else
        push!(props_str, string("dash pattern=", join(map(v -> join(v, " "),zip(cycle(["on", "off"]),map(v -> string(svg_fmt_float(v.value), "mm"), property.value)))," ")))
    end
end

function push_property!(props_str, img::PGF, property::StrokePrimitive)
    if isa(property.color, TransparentColor)
        img.stroke = color(property.color)
        img.stroke_opacity = property.color.alpha
    else
        img.stroke = property.color
    end

    if !isnull(img.stroke)
        push!(img.color_set, convert(RGB, property.color))
    end
end

function push_property!(props_str, img::PGF, property::FillPrimitive)
    if isa(property.color, TransparentColor)
        img.fill = color(property.color)
        img.fill_opacity = property.color.alpha
    else
        img.fill = property.color
    end

    if !isnull(img.fill)
        push!(img.color_set, convert(RGB, property.color))
    end
end

function push_property!(props_str, img::PGF, property::VisiblePrimitive)
    img.visible = property.value
end

function push_property!(props_str, img::PGF, property::LineWidthPrimitive)
    push!(props_str, string("line width=", svg_fmt_float(property.value.value), "mm"))
end

pgf_fmt_linecap(::LineCapButt) = "butt"
pgf_fmt_linecap(::LineCapSquare) = "rect"
pgf_fmt_linecap(::LineCapRound) = "round"

function push_property!(props_str, img::PGF, property::StrokeLineCapPrimitive)
    push!(props_str, string("line cap=", pgf_fmt_linecap(property.value)))
end

function push_property!(props_str, img::PGF, property::StrokeLineJoinPrimitive)
    push!(props_str, string("line join=", svg_fmt_linejoin(property.value)))
end

function push_property!(props_str, img::PGF, property::FillOpacityPrimitive)
    img.fill_opacity = property.value
end

function push_property!(props_str, img::PGF, property::StrokeOpacityPrimitive)
    img.stroke_opacity = property.value
end

function push_property!(props_str, img::PGF, property::FontPrimitive)
    # Can only only work with one font family for now

    img.fontfamily = strip(split(escape_string(property.family),',')[1],'\'')
end

function push_property!(props_str, img::PGF, property::FontSizePrimitive)
    img.fontsize = property.value.value
end

function push_property!(props_str, img::PGF, property::ClipPrimitive)
    img.clippath = property
    # Not quite sure how to handle clipping yet, stub for now
end

# Stubs for SVG and JS specific properties
function push_property!(props_str, img::PGF, property::JSIncludePrimitive)
end

function push_property!(props_str, img::PGF, property::JSCallPrimitive)
end

function push_property!(props_str, img::PGF, property::SVGClassPrimitive)
end

function push_property!(props_str, img::PGF, property::SVGAttributePrimitive)
end

function iswithjs(img::PGF)
    return false
end

function iswithousjs(img::PGF)
    return true
end

function draw{F<:FormPrimitive}(img::PGF, form::AbstractArray{F})
    for (idx, primitive) in enumerate(form.primitives)
        draw(img, primitive, idx)
    end
end

function draw(img::PGF, form::FormPrimitive)
    draw(img, primitive, 1)
end

function draw(img::PGF, prim::LinePrimitive, idx::Int)

    n = length(prim.points)
    if n <= 1; return; end

    modifiers, props = get_vector_properties(img, idx)
    if !img.visible; return; end

    write(img.buf, join(modifiers))
    @printf(img.buf, "\\path [%s] ", join(props, ","));
    print_pgf_path(img.buf, prim.points, true)
    write(img.buf, ";\n")
end

function draw(img::PGF, prim::RectanglePrimitive, idx::Int)
    width = max(prim.width.value, 0.01)
    height = max(prim.height.value, 0.01)

    modifiers, props = get_vector_properties(img, idx)
    if !img.visible; return; end

    write(img.buf, join(modifiers))
    @printf(img.buf, "\\path [%s] ", join(props, ","));
    @printf(img.buf, "(%s,%s) rectangle +(%s,%s);\n",
            svg_fmt_float(prim.corner[1].value),
            svg_fmt_float(prim.corner[2].value),
            svg_fmt_float(width),
            svg_fmt_float(height))
end

function draw(img::PGF, prim::PolygonPrimitive, idx::Int)
    n = length(prim.points)
    if n <= 1; return; end

    modifiers, props = get_vector_properties(img, idx)
    if !img.visible; return; end

    write(img.buf, join(modifiers))
    @printf(img.buf, "\\path [%s] ", join(props, ","))
    print_pgf_path(img.buf, prim.points, true)
    write(img.buf, " -- cycle;\n")
end

function draw(img::PGF, prim::CirclePrimitive, idx::Int)
    modifiers, props = get_vector_properties(img, idx)
    if !img.visible; return; end
    write(img.buf, join(modifiers))
    @printf(img.buf, "\\path [%s] ", join(props, ","))
    @printf(img.buf, "(%s,%s) circle [radius=%s];\n",
        svg_fmt_float(prim.center[1].value),
        svg_fmt_float(prim.center[2].value),
        svg_fmt_float(prim.radius.value))
end

function draw(img::PGF, prim::EllipsePrimitive, idx::Int)

    modifiers, props = get_vector_properties(img, idx)
    if !img.visible; return; end

    cx = prim.center[1].value
    cy = prim.center[2].value
    rx = sqrt((prim.x_point[1].value - cx)^2 +
              (prim.x_point[2].value - cy)^2)
    ry = sqrt((prim.y_point[1].value - cx)^2 +
              (prim.y_point[2].value - cy)^2)
    theta = rad2deg(atan2(prim.x_point[2].value - cy,
                          prim.x_point[1].value - cx))


    if !all(isfinite([cx, cy, rx, ry, theta]))
        return
    end

    modifiers, props = get_vector_properties(img, idx)
    write(img.buf, join(modifiers))
    @printf(img.buf, "\\path [%s] ", join(props, ","))
    @printf(img.buf, "(%s,%s) circle [x radius=%s, y radius=%s",
        svg_fmt_float(cx),
        svg_fmt_float(cy),
        svg_fmt_float(rx),
        svg_fmt_float(ry))
    if abs(theta) > 1e-4
        @printf(img.buf, " rotate=%s", svg_fmt_float(theta))
    end
    write(img.buf, "];\n")
end

function draw(img::PGF, prim::CurvePrimitive, idx::Int)
    modifiers, props = get_vector_properties(img, idx)
    if !img.visible; return; end
    write(img.buf, join(modifiers))
    @printf(img.buf, "\\path [%s] ", join(props, ","))
    @printf(img.buf, "(%s,%s) .. controls (%s,%s) and (%s,%s) .. (%s,%s);\n",
        svg_fmt_float(prim.anchor0[1].value),
        svg_fmt_float(prim.anchor0[2].value),
        svg_fmt_float(prim.ctrl0[1].value),
        svg_fmt_float(prim.ctrl0[2].value),
        svg_fmt_float(prim.ctrl1[1].value),
        svg_fmt_float(prim.ctrl1[2].value),
        svg_fmt_float(prim.anchor1[1].value),
        svg_fmt_float(prim.anchor1[2].value))
end

function draw(img::PGF, prim::TextPrimitive, idx::Int)

    # Rotation direction is reversed!
    modifiers, props = get_vector_properties(img, idx)
    if !img.visible; return; end
    push!(props, string(
        "rotate around={",
            svg_fmt_float(-rad2deg(prim.rot.theta)),
            ": (",
            svg_fmt_float(prim.rot.offset[1].value - prim.position[1].value),
            ",",
            svg_fmt_float(prim.rot.offset[2].value - prim.position[2].value),
            ")}"))
    if is(prim.halign, hcenter)
        push!(props, "inner sep=0.0")
    elseif is(prim.halign, hright)
        push!(props, "left,inner sep=0.0")
    else
        push!(props, "right,inner sep=0.0")
    end
    write(img.buf, join(modifiers))
    @printf(img.buf, "\\draw (%s,%s) node [%s]{\\fontsize{%smm}{%smm}\\selectfont \$%s\$};\n",
        svg_fmt_float(prim.position[1].value),
        svg_fmt_float(prim.position[2].value),
        replace(join(props, ","), "fill", "text"),
        svg_fmt_float(img.fontsize),
        svg_fmt_float(1.2*img.fontsize),
        pango_to_pgf(prim.value)
    )
end


function indent(img::PGF)
    for i in 1:img.indentation
        write(img.buf, "  ")
    end
end


function add_to_frame{P<:PropertyPrimitive}(img::PGF, property::P, frame, scalar_properties, applied_properties)
    push!(scalar_properties, property)
    push!(applied_properties, P)
    frame.has_scalar_properties = true
end

function add_to_frame{P<:PropertyPrimitive}(img::PGF, property::AbstractArray{P}, frame, scalar_properties, applied_properties)
    frame.vector_properties[P] = property
    img.vector_properties[P] = property
end


function push_property_frame(img::PGF, properties::Vector)
    if isempty(properties)
        return
    end

    frame = PGFPropertyFrame()
    applied_properties = Set{Type}()
    scalar_properties = Array(PropertyNode, 0)
    for property in properties
        if !isrepeatable(property) && (proptype(property) in applied_properties)
            continue
        elseif isscalar(property)
            add_to_frame(img, property, frame, scalar_properties, applied_properties)
        end
    end
    push!(img.property_stack, frame)
    if isempty(scalar_properties)
        return
    end

    write(img.buf, "\\begin{scope}\n")
    prop_str = AbstractString[]
    for property in scalar_properties
        push_property!(prop_str, img, property.primitives[1])
    end
    if length(prop_str) > 0
        @printf(img.buf, "[%s]\n", join(prop_str, ","))
    end
    if !isnull(img.clippath)
        write(img.buf, "\\clip ")
        print_pgf_path(img.buf, get(img.clippath).points)
        write(img.buf, ";\n")
    end
   if !isnull(img.fontfamily) && !img.texfonts
       @printf(img.buf, "\\fontspec{%s}\n", img.fontfamily)
    end
end


function pop_property_frame(img::PGF)
    @assert !isempty(img.property_stack)
    frame = pop!(img.property_stack)

    if frame.has_scalar_properties
        write(img.buf, "\\end{scope}\n")
        # There should be a better way to to this:
        # Maybe put the applied properties on a stack then
        # pop them out here?
        # FIX ME!
        img.fill = default_fill_color
        img.stroke = default_stroke_color
        img.fill_opacity = 1.0
        img.stroke_opacity = 1.0
        img.fontfamily = nothing
        img.clippath = nothing
        img.visible = true
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

# Horrible abuse of Latex inline math mode just to
# get something working first.
# FIX ME!
function pango_to_pgf(text::AbstractString)
    pat = r"<(/?)\s*([^>]*)\s*>"
    input = convert(Array{UInt8}, text)
    output = IOBuffer()
    lastpos = 1
    for mat in eachmatch(pat, text)
        write(output, "\\text{")
        write(output, input[lastpos:mat.offset-1])
        write(output, "}")

        if mat.captures[2] == "sup"
            if mat.captures[1] == "/"
                write(output, "}")
            else
                write(output, "^{")
            end
        elseif mat.captures[2] == "sub"
            if mat.captures[1] == "/"
                write(output, "}")
            else
                write(output, "_{")
            end
        elseif mat.captures[2] == "i"
            if mat.captures[1] == "/"
                write(output, "}")
            else
                write(output, "\\textit{")
            end
        elseif mat.captures[2] == "b"
            if mat.captures[1] == "/"
                write(output, "}")
            else
                write(output, "\\textbf{")
            end
        end
        lastpos = mat.offset + length(mat.match)
    end
    write(output, "\\text{")
    write(output, input[lastpos:end])
    write(output, "}")

    # Cleanup characters
    str_out = bytestring(output)
    replace(str_out, "%", "\\%")
end


