#
# Color
# A collection of utilities for handling color.
#

typealias ColorOrNothing Union(ColorValue, Nothing)

function cssfmt(c::ColorValue)
    string("#",hex(convert(RGB, c)))
end


function cssfmt(c::Nothing)
    "none"
end


function json(c::ColorValue)
    quote_string(cssfmt(c))
end



