mutable struct PGFPropertyFrame
    # Vector properties in this frame.
    vector_properties::Dict{Type, Property}

    # True if this property frame has scalar properties. Scalar properties are
    # emitted as a group {scope} that must be closed when the frame is popped.
    has_scalar_properties::Bool
end

PGFPropertyFrame() = PGFPropertyFrame(Dict{Type, Property}(), false)

mutable struct PGF <: Backend
    # Image size in millimeters.
    width::AbsoluteLength
    height::AbsoluteLength

    # Output stream.
    out::IO

    # Fill properties cannot be "cleanly" applied to
    # multiple form primitives.  It must be applied
    # each time an object is drawn
    fill::Union{Color, Nothing}
    fill_opacity::Float64

    stroke::Union{Color, Nothing}
    stroke_opacity::Float64

    fontfamily::Union{AbstractString, Nothing}
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
    vector_properties::Dict{Type, Union{Property, Nothing}}

    # Clip-paths that need to be defined at the end of the document.
    # Not quite sure how to deal with clip paths yet
    clippath::Union{ClipPrimitive, Nothing}
    # clippaths::Dict{ClipPrimitive, String}

    # True when finish has been called and no more drawing should occur
    finished::Bool

    # Backend is responsible for opening/closing the file
    ownedfile::Bool

    # Filename when ownedfile is true
    filename::Union{AbstractString, Nothing}

    # Emit the graphic on finish when writing to a buffer.
    emit_on_finish::Bool

    # Emit only the tikzpicture environment
    only_tikz::Bool

    # Use default TeX fonts instead of fonts specified by the theme.
    texfonts::Bool
end

function PGF(out::IO,
             width::AbsoluteLength,
             height::AbsoluteLength,
             emit_on_finish::Bool=true,
             only_tikz = false;

             texfonts = false,
             fill = default_fill_color,
             fill_opacity = 1.0,
             stroke = default_stroke_color,
             stroke_opacity = 1.0,
             fontfamily = nothing,
             fontsize = 12.0,
             indentation = 0,
             buf = IOBuffer(),
             visible = true,
             color_set = Set{Color}([colorant"black"]),
             property_stack = Array{PGFPropertyFrame}(undef, 0),
             vector_properties = Dict{Type, Union{Property, Nothing}}(),
             clippath = nothing,
             finished = false,
             ownedfile = false,
             filename = nothing)
    PGF(width, height, out,
        fill, fill_opacity,
        stroke, stroke_opacity,
        fontfamily, fontsize,
        indentation,
        buf,
        visible,
        color_set,
        property_stack,
        vector_properties,
        clippath,
        finished,
        ownedfile,
        filename,
        emit_on_finish,
        only_tikz,
        texfonts)
end

# Write to a file.
"""
    PGF([output::Union{IO,AbstractString}], width=√200cm, height=10cm, only_tikz=false; texfonts=false) -> Backend

Create a Portable Graphics Format backend.  The output is normally passed to
[`draw`](@ref).   Specify a filename using a string as the first argument.  If
`only_tikz` is true then the output is a "tikzpicture", otherwise the output is a
complete latex document with headers and footers.  If `texfonts` is false,
include "\\usepackage{fontspec}" in the headers.

# Examples
```
c = compose(context(), rectangle())
draw(PGF("myplot.tex", 10cm, 5cm, only_tikz=true, texfonts=true), c)
```
"""
PGF(filename::AbstractString, width=default_graphic_width, height=default_graphic_height,
            only_tikz=false; texfonts=false) =
        PGF(open(filename, "w"), width, height, true, only_tikz;
            texfonts = texfonts, ownedfile=true, filename=filename)

# Write to buffer.
PGF(width::MeasureOrNumber=default_graphic_width, height::MeasureOrNumber=default_graphic_height,
            emit_on_finish::Bool=true, only_tikz=false; texfonts=false) =
        PGF(IOBuffer(), width, height, emit_on_finish, only_tikz, texfonts=texfonts)

function (img::PGF)(x)
    draw(img, x)
end

function finish(img::PGF)
    img.finished && return

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
    write(img.out, take!(img.buf))
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

    hasmethod(flush, (typeof(img.out),)) && flush(img.out)
    close(img.buf)

    img.ownedfile && close(img.out)

    img.finished = true

    # If we are writing to a buffer. Collect the string and emit it.
    img.emit_on_finish && typeof(img.out) == IOBuffer && display(img)
end

isfinished(img::PGF) = img.finished
root_box(img::PGF) = BoundingBox(0mm, 0mm, img.width, img.height)

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
    props_str = String[]
    modifiers = String[]
    for (propertytype, property) in img.vector_properties
        (property === nothing) && continue
        primitives = property.primitives
        idx > length(primitives) &&
                error("Vector form and vector property differ in length. Can't distribute.")
        push_property!(props_str, img, primitives[idx])
    end

    if img.fill !== nothing
        push!(props_str, string("fill=mycolor",hex(img.fill)))
        img.fill_opacity < 1.0 &&
                push!(props_str, string("fill opacity=",svg_fmt_float(img.fill_opacity)))
    end

    if img.stroke !== nothing
        push!(props_str, string("draw=mycolor",hex(img.stroke)))
        img.stroke_opacity < 1.0 &&
                push!(props_str, string("draw opacity=",svg_fmt_float(img.stroke_opacity)))
    end

    return modifiers, props_str
end

push_property!(props_str, img::PGF, property::StrokeDashPrimitive) =
        isempty(property.value) ||
            push!(props_str, string("dash pattern=", join(map( v -> join(v, " "),
                zip(Iterators.cycle(["on", "off"]),
                map(v -> string(svg_fmt_float(v.value), "mm"), property.value)) )," ")))

function push_property!(props_str, img::PGF, property::StrokePrimitive)
    if isa(property.color, TransparentColor)
        img.stroke = color(property.color)
        img.stroke_opacity = property.color.alpha
    else
        img.stroke = property.color
    end

    (img.stroke === nothing) || push!(img.color_set, convert(RGB, property.color))
end

function push_property!(props_str, img::PGF, property::FillPrimitive)
    if isa(property.color, TransparentColor)
        img.fill = color(property.color)
        property.color.alpha<1.0 && (img.fill_opacity = property.color.alpha)
    else
        img.fill = property.color
    end

    (img.fill === nothing) || push!(img.color_set, convert(RGB, property.color))
end

push_property!(props_str, img::PGF, property::VisiblePrimitive) =
        img.visible = property.value
push_property!(props_str, img::PGF, property::LineWidthPrimitive) =
        push!(props_str, string("line width=", svg_fmt_float(property.value.value), "mm"))

pgf_fmt_linecap(::LineCapButt) = "butt"
pgf_fmt_linecap(::LineCapSquare) = "rect"
pgf_fmt_linecap(::LineCapRound) = "round"

push_property!(props_str, img::PGF, property::StrokeLineCapPrimitive) =
        push!(props_str, string("line cap=", pgf_fmt_linecap(property.value)))
push_property!(props_str, img::PGF, property::StrokeLineJoinPrimitive) =
        push!(props_str, string("line join=", svg_fmt_linejoin(property.value)))
push_property!(props_str, img::PGF, property::FillOpacityPrimitive) =
        img.fill_opacity = property.value
push_property!(props_str, img::PGF, property::StrokeOpacityPrimitive) =
        img.stroke_opacity = property.value
# Can only only work with one font family for now
push_property!(props_str, img::PGF, property::FontPrimitive) =
        img.fontfamily = strip(split(escape_string(property.family),',')[1],'\'')
push_property!(props_str, img::PGF, property::FontSizePrimitive) =
        img.fontsize = property.value.value
# Not quite sure how to handle clipping yet, stub for now
push_property!(props_str, img::PGF, property::ClipPrimitive) =
        img.clippath = property

# Stubs for SVG and JS specific properties
push_property!(props_str, img::PGF, property::JSIncludePrimitive) = nothing
push_property!(props_str, img::PGF, property::JSCallPrimitive) = nothing
push_property!(props_str, img::PGF, property::SVGClassPrimitive) = nothing
push_property!(props_str, img::PGF, property::SVGAttributePrimitive) = nothing
iswithjs(img::PGF) = false
iswithousjs(img::PGF) = true

push_property!(props_str, img::PGF, property::ArrowPrimitive) =
        push!(props_str, "arrows=->")


# Form Drawing
# ------------

function draw(img::PGF, form::Form)
    for (idx, primitive) in enumerate(form.primitives)
        draw(img, primitive, idx)
    end
end

function draw(img::PGF, prim::LinePrimitive, idx::Int)
    n = length(prim.points)
    n <= 1 && return

    modifiers, props = get_vector_properties(img, idx)
    img.visible || return

    write(img.buf, join(modifiers))
    @printf(img.buf, "\\path [%s] ", join(props, ","));
    print_pgf_path(img.buf, prim.points, true)
    write(img.buf, ";\n")
end

function draw(img::PGF, prim::RectanglePrimitive, idx::Int)
    width = max(prim.width.value, 0.01)
    height = max(prim.height.value, 0.01)

    modifiers, props = get_vector_properties(img, idx)
    img.visible || return

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
    n <= 1 && return

    modifiers, props = get_vector_properties(img, idx)
    img.visible || return

    write(img.buf, join(modifiers))
    @printf(img.buf, "\\path [%s] ", join(props, ","))
    print_pgf_path(img.buf, prim.points, true)
    write(img.buf, " -- cycle;\n")
end

function draw(img::PGF, prim::CirclePrimitive, idx::Int)
    modifiers, props = get_vector_properties(img, idx)
    img.visible || return
    write(img.buf, join(modifiers))
    @printf(img.buf, "\\path [%s] ", join(props, ","))
    @printf(img.buf, "(%s,%s) circle [radius=%s];\n",
        svg_fmt_float(prim.center[1].value),
        svg_fmt_float(prim.center[2].value),
        svg_fmt_float(prim.radius.value))
end

function draw(img::PGF, prim::EllipsePrimitive, idx::Int)
    modifiers, props = get_vector_properties(img, idx)
    img.visible || return

    cx = prim.center[1].value
    cy = prim.center[2].value
    rx = sqrt((prim.x_point[1].value - cx)^2 +
              (prim.x_point[2].value - cy)^2)
    ry = sqrt((prim.y_point[1].value - cx)^2 +
              (prim.y_point[2].value - cy)^2)
    theta = rad2deg(atan(prim.x_point[2].value - cy,
                         prim.x_point[1].value - cx))

    all(isfinite,[cx, cy, rx, ry, theta]) || return

    modifiers, props = get_vector_properties(img, idx)
    write(img.buf, join(modifiers))
    @printf(img.buf, "\\path [%s] ", join(props, ","))
    @printf(img.buf, "(%s,%s) circle [x radius=%s, y radius=%s",
        svg_fmt_float(cx),
        svg_fmt_float(cy),
        svg_fmt_float(rx),
        svg_fmt_float(ry))
    abs(theta) > 1e-4 && @printf(img.buf, " rotate=%s", svg_fmt_float(theta))
    write(img.buf, "];\n")
end

function draw(img::PGF, prim::CurvePrimitive, idx::Int)
    modifiers, props = get_vector_properties(img, idx)
    img.visible || return
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
    img.visible || return
    push!(props, string(
        "rotate around={",
            svg_fmt_float(-rad2deg(prim.rot.theta)),
            ": (",
            svg_fmt_float(prim.rot.offset[1].value - prim.position[1].value),
            ",",
            svg_fmt_float(prim.rot.offset[2].value - prim.position[2].value),
            ")}"))
    if prim.halign === hcenter
        push!(props, "inner sep=0.0")
    elseif prim.halign === hright
        push!(props, "left,inner sep=0.0")
    else
        push!(props, "right,inner sep=0.0")
    end
    write(img.buf, join(modifiers))
    @printf(img.buf, "\\draw (%s,%s) node [%s]{\\fontsize{%smm}{%smm}\\selectfont \$%s\$};\n",
        svg_fmt_float(prim.position[1].value),
        svg_fmt_float(prim.position[2].value),
        replace(join(props, ","), "fill"=>"text"),
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

function push_property_frame(img::PGF, properties::Vector{Property})
    isempty(properties) && return

    frame = PGFPropertyFrame()
    applied_properties = Set{Type}()
    scalar_properties = Array{Property}(undef, 0)
    for property in properties
        if !isrepeatable(property) && (typeof(property) in applied_properties)
            continue
        elseif isscalar(property)
            push!(scalar_properties, property)
            push!(applied_properties, typeof(property))
            frame.has_scalar_properties = true
            img.vector_properties[typeof(property)] = nothing
        else
            frame.vector_properties[typeof(property)] = property
            img.vector_properties[typeof(property)] = property
        end
    end
    push!(img.property_stack, frame)
    isempty(scalar_properties) && return

    write(img.buf, "\\begin{scope}\n")
    prop_str = AbstractString[]
    for property in scalar_properties
        push_property!(prop_str, img, property.primitives[1])
    end
    if length(prop_str) > 0
        @printf(img.buf, "[%s]\n", join(prop_str, ","))
    end
    if img.clippath !== nothing
        write(img.buf, "\\clip ")
        print_pgf_path(img.buf, img.clippath.points)
        write(img.buf, ";\n")
    end
    if (!img.texfonts && (img.fontfamily !== nothing))
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
    input = codeunits(escape_tex_chars(text))
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

    return String(take!(output))
end

function escape_tex_chars(text::AbstractString)
	# see http://www.cespedes.org/blog/85/how-to-escape-latex-special-characters
	escaped_str = text
	escaped_str = replace(escaped_str, "\\"=>"\\textbackslash{}")
	escaped_str = replace(escaped_str, "#"=>"\\#")
	escaped_str = replace(escaped_str, "\$"=>"\\\$")
	escaped_str = replace(escaped_str, "%"=>"\\%")
	escaped_str = replace(escaped_str, "&"=>"\\&")
	escaped_str = replace(escaped_str, "_"=>"\\_")
	escaped_str = replace(escaped_str, "{"=>"\\{")
	escaped_str = replace(escaped_str, "}"=>"\\}")
	escaped_str = replace(escaped_str, "^"=>"\\textasciicircum{}")
	escaped_str = replace(escaped_str, "~"=>"\\textasciitilde{}")
	escaped_str = replace(escaped_str, "\\textbackslash\\{\\}"=>"\\textbackslash{}")
end



function draw(img::PGF, prim::ArcPrimitive, idx::Int)
    
    angle2 = prim.angle2 + (prim.angle2<prim.angle1)*2π
    modifiers, props = get_vector_properties(img, idx)
    img.visible || return
    write(img.buf, join(modifiers))
    @printf(img.buf, "\\path [%s] (%s,%s) ",
        join(props, ","), 
        svg_fmt_float(prim.center[1].value),
        svg_fmt_float(prim.center[2].value) )
    prim.sector && write(img.buf, "-- ")
    @printf(img.buf, "+(%s:%s) ",
        svg_fmt_float(rad2deg(prim.angle1)),
        svg_fmt_float(prim.radius.value) )
    @printf(img.buf,  "arc [radius=%s, start angle=%s, end angle=%s]",
        svg_fmt_float(prim.radius.value),
        svg_fmt_float(rad2deg(prim.angle1)),
        svg_fmt_float(rad2deg(angle2)) )
    prim.sector && write(img.buf, " -- cycle")
    write(img.buf, ";\n")
end


function draw(img::PGF, prim::BezierPolygonPrimitive, idx::Int)
    modifiers, props = get_vector_properties(img, idx)
    img.visible || return
    write(img.buf, join(modifiers))
    @printf(img.buf, "\\path [%s] ", join(props, ","))
    @printf(img.buf, "(%s,%s)", svg_fmt_float(prim.anchor[1].value),
        svg_fmt_float(prim.anchor[2].value))
    currentpoint = prim.anchor
    for side in prim.sides
        if length(side)==1
            point = side[1]
            @printf(img.buf, " -- (%s,%s)", svg_fmt_float(point[1].value),
                svg_fmt_float(point[2].value))
        else
        cp1, cp2, ep = if length(side)==2
                controlpnt(currentpoint, side[1]),
                controlpnt(side[2], side[1]), side[2]
            elseif length(side)==3
                side[1], side[2], side[3]
            end
            @printf(img.buf, " .. controls (%s,%s) and (%s,%s) .. (%s,%s)",
            svg_fmt_float(cp1[1].value), svg_fmt_float(cp1[2].value),
            svg_fmt_float(cp2[1].value), svg_fmt_float(cp2[2].value),
            svg_fmt_float(ep[1].value), svg_fmt_float(ep[2].value))
        end
        currentpoint = side[end]
    end
    write(img.buf, " -- cycle;\n")
end

