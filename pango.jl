
# Pango/FT2 bindings for doing simple precomputation of text extents for SVGs.
# This is in some ways a hack: with javascript we could get text extents at
# render time and adjust layout accordingly, but that would mean generating a
# great deal of javascript, and sink any hope of displaying a reasonable graph
# without a javascript interpreter, which is an explicit goal.

require("measure.jl")

const libpango = dlopen("libpangoft2-1.0")

const PANGO_SCALE = 1024.0

type PangoLayout
    fm::Ptr{Void}
    ctx::Ptr{Void}
    layout::Ptr{Void}

    function PangoLayout()
        fm = ccall(dlsym(libpango, :pango_ft2_font_map_new),
                   Ptr{Void}, ())

        ctx = ccall(dlsym(libpango, :pango_font_map_create_context),
                    Ptr{Void}, (Ptr{Void},), fm)

        layout = ccall(dlsym(libpango, :pango_layout_new),
                       Ptr{Void}, (Ptr{Void},), ctx)

        new(fm, ctx, layout)
    end
end


function pango_set_font(pangolayout::PangoLayout, family::String, pts::Number)
    desc_str = @sprintf("%s %f", family, pts)
    desc = ccall(dlsym(libpango, :pango_font_description_from_string),
                 Ptr{Void}, (Ptr{Uint8},), bytestring(desc_str))

    ccall(dlsym(libpango, :pango_layout_set_font_description),
          Void, (Ptr{Void}, Ptr{Void}), pangolayout.layout, desc)

    ccall(dlsym(libpango, :pango_font_description_free),
          Void, (Ptr{Void},), desc)
end


function pango_text_extents(pangolayout::PangoLayout, text::String)
    ccall(dlsym(libpango, :pango_layout_set_text),
          Void, (Ptr{Void}, Ptr{Uint8}, Int32),
          pangolayout.layout, bytestring(text), length(text))
    
    extents = Array(Int32, 4)
    ccall(dlsym(libpango, :pango_layout_get_extents),
          Void, (Ptr{Void}, Ptr{Int32}, Ptr{Int32}),
          pangolayout.layout, extents, C_NULL)

    extents 

    width, height = (extents[3] / PANGO_SCALE)pt, (extents[4] / PANGO_SCALE)pt
end


