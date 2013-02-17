#
# Color
# A collection of utilities for handling color.
#

export Color, color, ColorOrNothing, ColorStringOrNothing,
       Colour, colour, ColourOrNothing, ColourStringOrNothing,
       weighted_color_mean, hex,
       RGB, HLS, XYZ, LAB, LUV, LCHab, LCHuv, LMS,
       cie_color_match, protanopic, deuteranopic, tritanopic,
       colordiff, distinguishable_colors

abstract Color
typealias ColorOrNothing Union(Color, String, Nothing)
typealias ColorOrStringOrNothing Union(Color, String, Nothing)


# For our non-american pals.
typealias Colour Color
typealias ColourOrNothing ColorOrNothing
typealias ColourOrStringOrNothing ColorOrNothing


# sRGB (standard Red-Green-Blue)
type RGB <: Color
    r::Float64 # Red in [0,1]
    g::Float64 # Green in [0,1]
    b::Float64 # Blue in [0,1]

    function RGB(r::Number, g::Number, b::Number)
        new(r, g, b)
    end

    RGB() = RGB(0, 0, 0)
end


# HSV (Hue-Saturation-Value)
type HSV <: Color
    h::Float64 # Hue in [0,360]
    s::Float64 # Saturation in [0,1]
    v::Float64 # Value in [0,1]

    function HSV(h::Number, s::Number, v::Number)
        new(h, s, n)
    end

    HSV() = HSV(0, 0, 0)
end


# HLS (Hue-Lightness-Saturation)
type HLS <: Color
    h::Float64 # Hue in [0,360]
    l::Float64 # Lightness in [0,1]
    s::Float64 # Saturation in [0,1]

    function HLS(h::Number, l::Number, s::Number)
        new(h, l, s)
    end

    HLS() = HLS(0, 0, 0)
end


# XYZ (CIE 1931)
type XYZ <: Color
    x::Float64
    y::Float64
    z::Float64

    function XYZ(x::Number, y::Number, z::Number)
        new(x, y, z)
    end

    XYZ() = XYZ(0, 0, 0)
end


# LAB (CIELAB)
type LAB <: Color
    l::Float64 # Luminance in [0,1]
    a::Float64 # Red/Green in [-1,1]
    b::Float64 # Blue/Yellow in [-1,1]

    function LAB(l::Number, a::Number, b::Number)
        new(l, a, b)
    end

    LAB() = LAB(0, 0, 0)
end


# LCHab (Luminance-Chroma-Hue, Polar-LAB)
type LCHab <: Color
    l::Float64 # Luminance
    c::Float64 # Chroma
    h::Float64 # Hue

    function LCHab(l::Number, c::Number, h::Number)
        new(l, c, h)
    end

    LCHab() = LCHab(0, 0, 0)
end


# LUV (CIELUV)
type LUV <: Color
    l::Float64 # Luminance in [0,1]
    u::Float64 # Red/Green in [-1,1]
    v::Float64 # Blue/Yellow in [-1,1]

    function LUV(l::Number, u::Number, v::Number)
        new(l, u, v)
    end

    LUV() = LUV(0, 0, 0)
end


# LCHuv (Luminance-Chroma-Hue, Polar-LUV)
type LCHuv <: Color
    l::Float64 # Luminance
    c::Float64 # Chroma
    h::Float64 # Hue

    function LCHuv(l::Number, c::Number, h::Number)
        new(l, c, h)
    end

    LCHuv() = LCHuv(0, 0, 0)
end


# LMS (Long Medium Short)
type LMS <: Color
    l::Float64 # Long
    m::Float64 # Medium
    s::Float64 # Short

    function LMS(l::Number, m::Number, s::Number)
        new(l, m, s)
    end

    LMS() = LMS(0, 0, 0)
end


# Arbitrary ordering
# ------------------

isless(a::RGB, b::RGB) = (a.r, a.g, a.b) < (b.r, b.g, b.b)
isless(a::Color, b::Color) = convert(RGB, a) < convert(RGB, b)


# White-points
# ------------

const WP_A   = XYZ(1.09850, 1.00000, 0.35585)
const WP_B   = XYZ(0.99072, 1.00000, 0.85223)
const WP_C   = XYZ(0.98074, 1.00000, 1.18232)
const WP_D50 = XYZ(0.96422, 1.00000, 0.82521)
const WP_D55 = XYZ(0.95682, 1.00000, 0.92149)
const WP_D65 = XYZ(0.95047, 1.00000, 1.08883)
const WP_D75 = XYZ(0.94972, 1.00000, 1.22638)
const WP_E   = XYZ(1.00000, 1.00000, 1.00000)
const WP_F2  = XYZ(0.99186, 1.00000, 0.67393)
const WP_F7  = XYZ(0.95041, 1.00000, 1.08747)
const WP_F11 = XYZ(1.00962, 1.00000, 0.64350)
const WP_DEFAULT = WP_D65


# Chromatic Adaptation
# --------------------

# This is the Sharp cromatic adaptation method, which is purported to be
# slightly better than the Bradford method. (TODO: cite)
const chrom_adapt_sharp =
    [1.2694 -0.0988 -0.1706
    -0.8364  1.8006  0.0357
     0.0297 -0.0315  1.0018]


function chrom_adapt(c::XYZ, src_wp::XYZ, dest_wp::XYZ)
    z = [dest_wp.x dest_wp.y dest_wp.z] ./ [src_wp.x src_wp.y src_wp.z]
    ans = (inv(chrom_adapt_sharp) * diagm(z) * chrom_adapt_sharp) * [c.x, c.y, c.z]
    XYZ(ans[1], ans[2], ans[3])
end



# Utilities
# ---------

# Linear-interpolation in [a, b] where x is in [0,1],
# or coerced to be if not.
function lerp(x::Float64, a::Float64, b::Float64)
    a + (b - a) * max(min(x, 1.0), 0.0)
end




# RGB Functions
# -------------
function hex(c::RGB)
    @sprintf("#%02X%02X%02X",
             int(lerp(c.r, 0.0, 255.0)),
             int(lerp(c.g, 0.0, 255.0)),
             int(lerp(c.b, 0.0, 255.0)))
end

hex(c::Color) = hex(convert(RGB, c))


# Everything to RGB
# -----------------

function convert(::Type{RGB}, c::HSV)
    h = c.h / 60
    i = floor(h)
    f = h - i
    if int(i) & 1 == 0
        f = 1 - f
    end
    m = c.v * (1 - c.s)
    n = c.v * (1 - c.s * f)
    i = int(i)
    if i == 6 || i == 0; RGB(c.v, n, m)
    elseif i == 1;       RGB(n, c.v, m)
    elseif i == 2;       RGB(m, c.v, n)
    elseif i == 3;       RGB(m, n, c.v)
    elseif i == 4;       RGB(n, m, c.v)
    else;                RGB(c.v, m, n)
    end
end

function convert(::Type{RGB}, c::HLS)
    function qtrans(u::Float64, v::Float64, hue::Float64)
        if     hue > 360; hue -= 360
        elseif hue < 0;   hue += 360
        end

        if     hue < 60;  u + (v - u) * hue / 60
        elseif hue < 180; v
        elseif hue < 240; u + (v - u) * (240 - hue) / 60
        else;             u
        end
    end

    v = c.l <= 0.5 ? c.l * (1 + c.s) : c.l + c.s - (c.l * c.s)
    u = 2 * c.l - v

    if c.s == 0; RGB(c.l, c.l, c.l)
    else;        RGB(qtrans(u, v, c.h + 120),
                     qtrans(u, v, c.h),
                     qtrans(u, v, c.h - 120))
    end
end

const M_XYZ_RGB = [ 3.2404542 -1.5371385 -0.4985314
                   -0.9692660  1.8760108  0.0415560
                    0.0556434 -0.2040259  1.0572252 ]


function correct_gamut(c::RGB)
    c.r = min(1.0, max(0.0, c.r))
    c.g = min(1.0, max(0.0, c.g))
    c.b = min(1.0, max(0.0, c.b))
    c
end

function srgb_compand(v::Float64)
    v <= 0.0031308 ? 12.92v : 1.055v^(1/2.4) - 0.055
end

function convert(::Type{RGB}, c::XYZ)
    ans = M_XYZ_RGB * [c.x, c.y, c.z]
    correct_gamut(RGB(srgb_compand(ans[1]),
                      srgb_compand(ans[2]),
                      srgb_compand(ans[3])))
end

convert(::Type{RGB}, c::LAB)   = convert(RGB, convert(XYZ, c))
convert(::Type{RGB}, c::LCHab) = convert(RGB, convert(LAB, c))
convert(::Type{RGB}, c::LUV)   = convert(RGB, convert(XYZ, c))
convert(::Type{RGB}, c::LCHuv) = convert(RGB, convert(LUV, c))
convert(::Type{RGB}, c::LMS)   = convert(RGB, convert(XYZ, c))


# Everything to HSV
# -----------------

function convert(::Type{HSV}, c::RGB)
    c_min = min([c.r, c.g, c.b])
    c_max = max([c.r, c.g, c.b])
    if c_min == c_max
        return HSV(0.0, 0.0, c_max)
    end

    if c_min == c.r
        f = c.g - c.b
        i = 3.
    elseif c_min == c.g
        f = c.b - c.r
        i = 5.
    else
        f = c.r - c.g
        i = 1.
    end

    HSV(60 * (i - f / (c_max - c_min)),
        (c_max - c_min) / c_max,
        c_max)
end


convert(::Type{HSV}, c::Color) = convert(HSV, convert(RGB, c))


# Everything to HLS
# -----------------

function convert(::Type{HLS}, c::RGB)
    c_min = min([c.r, c.g, c.b])
    c_max = max([c.r, c.g, c.b])
    l = (c_max - c_min) / 2

    if c_max == c_min
        return HLS(0.0, l, 0.0)
    end

    if l < 0.5; s = (c_max - c_min) / (c_max + c_min)
    else;       s = (c_max - c_min) / (2.0 - c_max - c_min)
    end

    if c_max == c.r
        h = (c.g - c.b) / (c_max - c_min)
    elseif c_max == c.g
        h = 2.0 + (c.b - c.r) / (c_max - c_min)
    else
        h = 4.0 + (c.r - c.g) / (c_max - c_min)
    end

    h *= 60
    if h < 0
        h += 360
    elseif h > 360
        h -= 360
    end

    HLS(h,l,s)
end


convert(::Type{HLS}, c::Color) = convert(HLS, convert(RGB, c))


# Everything to XYZ
# -----------------

function invert_rgb_compand(v::Float64)
    v <= 0.04045 ? v/12.92 : ((v+0.055) /1.055)^2.4
end

const M_RGB_XYZ =
    [ 0.4124564  0.3575761  0.1804375
      0.2126729  0.7151522  0.0721750
      0.0193339  0.1191920  0.9503041 ]

function convert(::Type{XYZ}, c::RGB)
    v = [invert_rgb_compand(c.r),
         invert_rgb_compand(c.g),
         invert_rgb_compand(c.b)]
    ans = M_RGB_XYZ * v
    XYZ(ans[1], ans[2], ans[3])
end

convert(::Type{XYZ}, c::HSV) = convert(XYZ, convert(RGB, c))
convert(::Type{XYZ}, c::HLS) = convert(XYZ, convert(RGB, c))

const xyz_epsilon = 216. / 24389.
const xyz_kappa   = 24389. / 27.

function convert(::Type{XYZ}, c::LAB, wp::XYZ)
    fy = (c.l + 16) / 116
    fx = c.a / 500 + fy
    fz = fy - c.b / 200

    fx3 = fx^3
    fz3 = fz^3

    x = fx3 > xyz_epsilon ? fx3 : (116fx - 16) / xyz_kappa
    y = c.l > xyz_kappa * xyz_epsilon ? ((c. l+ 16) / 116)^3 : c.l / xyz_kappa
    z = fz3 > xyz_epsilon ? fz3 : (116fz - 16) / xyz_kappa

    XYZ(x*wp.x, y*wp.y, z*wp.z)
end

convert(::Type{XYZ}, c::LAB)   = convert(XYZ, c, WP_DEFAULT)
convert(::Type{XYZ}, c::LCHab) = convert(XYZ, convert(LAB, c))

function xyz_to_uv(c::XYZ)
    d = c.x + 15c.y + 3c.z
    u = (4. * c.x) / d
    v = (9. * c.y) / d
    return (u,v)
end

function convert(::Type{XYZ}, c::LUV, wp::XYZ)
    (u_wp, v_wp) = xyz_to_uv(wp)

    a = (52 * c.l / (c.u + 13 * c.l * u_wp) - 1) / 3
    y = c.l > xyz_kappa * xyz_epsilon ? ((c.l + 16) / 116)^3 : c.l / xyz_kappa
    b = -5y
    d = y * (39 * c.l / (c.v + 13 * c.l * v_wp) - 5)
    x = (d - b) / (a + (1./3.))
    z = a * x + b

    XYZ(x, y, z)
end

convert(::Type{XYZ}, c::LUV)   = convert(XYZ, c, WP_DEFAULT)
convert(::Type{XYZ}, c::LCHuv) = convert(XYZ, convert(LUV, c))

function convert(::Type{XYZ}, c::LMS)
    ans = CAT02_INV * [c.l, c.m, c.s]
    XYZ(ans[1], ans[2], ans[3])
end


# Everything to LAB
# -----------------

convert(::Type{LAB}, c::RGB) = convert(LAB, convert(XYZ, c))
convert(::Type{LAB}, c::HSV) = convert(LAB, convert(RGB, c))
convert(::Type{LAB}, c::HLS) = convert(LAB, convert(RGB, c))

function convert(::Type{LAB}, c::XYZ, wp::XYZ)
    function f(v::Float64)
        v > xyz_epsilon ? cbrt(v) : (xyz_kappa * v + 16) / 116
    end

    fx, fy, fz = f(c.x / wp.x), f(c.y / wp.y), f(c.z / wp.z)
    LAB(116fy - 16, 500(fx - fy), 200(fy - fz))
end

convert(::Type{LAB}, c::XYZ) = convert(LAB, c, WP_DEFAULT)

function convert(::Type{LAB}, c::LCHab)
    hr = degrees2radians(c.h)
    LAB(c.l, c.c * cos(hr), c.c * sin(hr))
end

convert(::Type{LAB}, c::Color) = convert(LAB, convert(XYZ, c))


# Everything to LUV
# -----------------

convert(::Type{LUV}, c::RGB) = convert(LUV, convert(XYZ, c))
convert(::Type{LUV}, c::HSV) = convert(LUV, convert(RGB, c))
convert(::Type{LUV}, c::HLS) = convert(LUV, convert(RGB, c))

function convert(::Type{LUV}, c::XYZ, wp::XYZ)
    (u_wp, v_wp) = xyz_to_uv(wp)
    (u_, v_) = xyz_to_uv(c)

    y = c.y / wp.y

    l = y > xyz_epsilon ? 116 * cbrt(y) - 16 : xyz_kappa * y
    u = 13 * l * (u_ - u_wp)
    v = 13 * l * (v_ - v_wp)

    LUV(l, u, v)
end

convert(::Type{LUV}, c::XYZ) = convert(LUV, c, WP_DEFAULT)

function convert(::Type{LUV}, c::LCHuv)
    hr = degrees2radians(c.h)
    LUV(c.l, c.c * cos(hr), c.c * sin(hr))
end

convert(::Type{LUV}, c::Color) = convert(LUV, convert(XYZ, c))


# Everything to LCHuv
# -------------------

function convert(::Type{LCHuv}, c::LUV)
    h = radians2degrees(atan2(c.v, c.u))
    while h > 360; h -= 360; end
    while h < 0;   h += 360; end
    LCHuv(c.l, sqrt(c.u^2 + c.v^2), h)
end

convert(::Type{LCHuv}, c::Color) = convert(LCHuv, convert(LUV, c))


# Everything to LCHab
# -------------------

function convert(::Type{LCHab}, c::LAB)
    h = radians2degrees(atan2(c.b, c.a))
    while h > 360; h -= 360; end
    while h < 0;   h += 360; end
    LCHab(c.l, sqrt(c.a^2 + c.b^2), h)
end

convert(::Type{LCHab}, c::Color) = convert(LCHab, convert(LAB, c))


# Weighted mean of some number of colors within the same space.
#
# Args:
#  cs: Colors.
#  ws: Weights of the same length as cs.
#
# Returns:
#   A weighted mean color of type T.
#
function weighted_color_mean{T <: Color, S <: Number}(
        cs::AbstractArray{T,1}, ws::AbstractArray{S,1})
    mu = T()
    sumws = sum(ws)
    for (c, w) in zip(cs, ws)
        w /= sumws
        for v in names(T)
            setfield(mu, v, getfield(mu, v) + w * getfield(c, v))
        end
    end

    mu
end


# Everything to LMS
# -----------------

# Chromatic adaptation from CIECAM97s
const CAT97s = [ 0.8562  0.3372 -0.1934
                -0.8360  1.8327  0.0033
                 0.0357 -0.0469  1.0112 ]

const CAT97s_INV = inv(CAT97s)

# Chromatic adaptation from CIECAM02
const CAT02 = [ 0.7328 0.4296 -0.1624
               -0.7036 1.6975  0.0061
                0.0030 0.0136  0.9834 ]

const CAT02_INV = inv(CAT02)


function convert(::Type{LMS}, c::XYZ)
    ans = CAT02 * [c.x, c.y, c.z]
    LMS(ans[1], ans[2], ans[3])
end


convert(::Type{LMS}, c::Color) = convert(LMS, convert(XYZ, c))


# Simulation of Colorblindness
# ----------------------------

# This method is due to:
# Brettel, H., Viénot, F., & Mollon, J. D. (1997).  Computerized simulation of
# color appearance for dichromats. Josa A, 14(10), 2647–2655.
#
# These functions add to Brettel's method a parameter p in [0, 1] giving the
# degree of photopigment loss. At p = 1, the complete loss of a particular
# photopigment is simulated, where 0 < p < 1 represents partial loss.


# This is supposed to be "the brightest possible metamer of an equal-energy
# stimulus". I'm punting a bit and just calling that RGB white.
const default_brettel_neutral = convert(LMS, RGB(1, 1, 1))


# Helper function for Brettel conversions.
function brettel_abc(neutral::LMS, anchor::LMS)
    a = neutral.m * anchor.s - neutral.s * anchor.m
    b = neutral.s * anchor.l - neutral.l * anchor.s
    c = neutral.l * anchor.m - neutral.m * anchor.l
    (a, b, c)
end


# Convert a color to simulate protanopic color blindness (lack of the
# long-wavelength photopigment).
function protanopic{T <: Color}(q::T, p::Float64, neutral::LMS)
    q = convert(LMS, q)
    anchor_wavelen = q.s / q.m < neutral.s / neutral.m ? 575 : 475
    anchor = cie_color_match(anchor_wavelen)
    anchor = convert(LMS, anchor)
    (a, b, c) = brettel_abc(neutral, anchor)

    q = LMS((1.0 - p) * q.l + p * (-(b*q.m + c*q.s)/a),
            q.m,
            q.s)
    convert(T, q)
end


# Convert a color to simulate deuteranopic color blindness (lack of the
# middle-wavelength photopigment.)
function deuteranopic{T <: Color}(q::T, p::Float64, neutral::LMS)
    q = convert(LMS, q)
    anchor_wavelen = q.s / q.l < neutral.s / neutral.l ? 575 : 475
    anchor = cie_color_match(anchor_wavelen)
    anchor = convert(LMS, anchor)
    (a, b, c) = brettel_abc(neutral, anchor)

    q = LMS(q.l,
            (1.0 - p) * q.m + p * (-(a*q.l + c*q.s)/b),
            q.s)
    convert(T, q)
end


# Convert a color to simulato tritanopic color blindness (lack of the
# short-wavelength photogiment)
function tritanopic{T <: Color}(q::T, p::Float64, neutral::LMS)
    q = convert(LMS, q)
    anchor_wavelen = q.m / q.l < neutral.m / neutral.l ? 660 : 485
    anchor = cie_color_match(anchor_wavelen)
    anchor = convert(LMS, anchor)
    (a, b, c) = brettel_abc(neutral, anchor)

    q = LMS(q.l,
            q.m,
            (1.0 - p) * q.l + p * (-(a*q.l + b*q.m)/c))
    convert(T, q)
end


protanopic(c::Color, p::Float64)   = protanopic(c, p, default_brettel_neutral)
deuteranopic(c::Color, p::Float64) = deuteranopic(c, p, default_brettel_neutral)
tritanopic(c::Color, p::Float64)   = tritanopic(c, p, default_brettel_neutral)

protanopic(c::Color)   = protanopic(c, 1.0)
deuteranopic(c::Color) = deuteranopic(c, 1.0)
tritanopic(c::Color)   = tritanopic(c, 1.0)
# ----------------

const x11_color_names = {
    "aliceblue"            => (240, 248, 255),
    "antiquewhite"         => (250, 235, 215),
    "antiquewhite1"        => (255, 239, 219),
    "antiquewhite2"        => (238, 223, 204),
    "antiquewhite3"        => (205, 192, 176),
    "antiquewhite4"        => (139, 131, 120),
    "aquamarine"           => (127, 255, 212),
    "aquamarine1"          => (127, 255, 212),
    "aquamarine2"          => (118, 238, 198),
    "aquamarine3"          => (102, 205, 170),
    "aquamarine4"          => ( 69, 139, 116),
    "azure"                => (240, 255, 255),
    "azure1"               => (240, 255, 255),
    "azure2"               => (224, 238, 238),
    "azure3"               => (193, 205, 205),
    "azure4"               => (131, 139, 139),
    "beige"                => (245, 245, 220),
    "bisque"               => (255, 228, 196),
    "bisque1"              => (255, 228, 196),
    "bisque2"              => (238, 213, 183),
    "bisque3"              => (205, 183, 158),
    "bisque4"              => (139, 125, 107),
    "black"                => (  0,   0,   0),
    "blanchedalmond"       => (255, 235, 205),
    "blue"                 => (  0,   0, 255),
    "blue1"                => (  0,   0, 255),
    "blue2"                => (  0,   0, 238),
    "blue3"                => (  0,   0, 205),
    "blue4"                => (  0,   0, 139),
    "blueviolet"           => (138,  43, 226),
    "brown"                => (165,  42,  42),
    "brown1"               => (255,  64,  64),
    "brown2"               => (238,  59,  59),
    "brown3"               => (205,  51,  51),
    "brown4"               => (139,  35,  35),
    "burlywood"            => (222, 184, 135),
    "burlywood1"           => (255, 211, 155),
    "burlywood2"           => (238, 197, 145),
    "burlywood3"           => (205, 170, 125),
    "burlywood4"           => (139, 115,  85),
    "cadetblue"            => ( 95, 158, 160),
    "cadetblue1"           => (152, 245, 255),
    "cadetblue2"           => (142, 229, 238),
    "cadetblue3"           => (122, 197, 205),
    "cadetblue4"           => ( 83, 134, 139),
    "chartreuse"           => (127, 255,   0),
    "chartreuse1"          => (127, 255,   0),
    "chartreuse2"          => (118, 238,   0),
    "chartreuse3"          => (102, 205,   0),
    "chartreuse4"          => ( 69, 139,   0),
    "chocolate"            => (210, 105,  30),
    "chocolate1"           => (255, 127,  36),
    "chocolate2"           => (238, 118,  33),
    "chocolate3"           => (205, 102,  29),
    "chocolate4"           => (139,  69,  19),
    "coral"                => (255, 127,  80),
    "coral1"               => (255, 114,  86),
    "coral2"               => (238, 106,  80),
    "coral3"               => (205,  91,  69),
    "coral4"               => (139,  62,  47),
    "cornflowerblue"       => (100, 149, 237),
    "cornsilk"             => (255, 248, 220),
    "cornsilk1"            => (255, 248, 220),
    "cornsilk2"            => (238, 232, 205),
    "cornsilk3"            => (205, 200, 177),
    "cornsilk4"            => (139, 136, 120),
    "cyan"                 => (  0, 255, 255),
    "cyan1"                => (  0, 255, 255),
    "cyan2"                => (  0, 238, 238),
    "cyan3"                => (  0, 205, 205),
    "cyan4"                => (  0, 139, 139),
    "darkblue"             => (  0,   0, 139),
    "darkcyan"             => (  0, 139, 139),
    "darkgoldenrod"        => (184, 134,  11),
    "darkgoldenrod1"       => (255, 185,  15),
    "darkgoldenrod2"       => (238, 173,  14),
    "darkgoldenrod3"       => (205, 149,  12),
    "darkgoldenrod4"       => (139, 101,   8),
    "darkgray"             => (169, 169, 169),
    "darkgreen"            => (  0, 100,   0),
    "darkgrey"             => (169, 169, 169),
    "darkkhaki"            => (189, 183, 107),
    "darkmagenta"          => (139,   0, 139),
    "darkolivegreen"       => ( 85, 107,  47),
    "darkolivegreen1"      => (202, 255, 112),
    "darkolivegreen2"      => (188, 238, 104),
    "darkolivegreen3"      => (162, 205,  90),
    "darkolivegreen4"      => (110, 139,  61),
    "darkorange"           => (255, 140,   0),
    "darkorange1"          => (255, 127,   0),
    "darkorange2"          => (238, 118,   0),
    "darkorange3"          => (205, 102,   0),
    "darkorange4"          => (139,  69,   0),
    "darkorchid"           => (153,  50, 204),
    "darkorchid1"          => (191,  62, 255),
    "darkorchid2"          => (178,  58, 238),
    "darkorchid3"          => (154,  50, 205),
    "darkorchid4"          => (104,  34, 139),
    "darkred"              => (139,   0,   0),
    "darksalmon"           => (233, 150, 122),
    "darkseagreen"         => (143, 188, 143),
    "darkseagreen1"        => (193, 255, 193),
    "darkseagreen2"        => (180, 238, 180),
    "darkseagreen3"        => (155, 205, 155),
    "darkseagreen4"        => (105, 139, 105),
    "darkslateblue"        => ( 72,  61, 139),
    "darkslategray"        => ( 47,  79,  79),
    "darkslategray1"       => (151, 255, 255),
    "darkslategray2"       => (141, 238, 238),
    "darkslategray3"       => (121, 205, 205),
    "darkslategray4"       => ( 82, 139, 139),
    "darkslategrey"        => ( 47,  79,  79),
    "darkturquoise"        => (  0, 206, 209),
    "darkviolet"           => (148,   0, 211),
    "deeppink"             => (255,  20, 147),
    "deeppink1"            => (255,  20, 147),
    "deeppink2"            => (238,  18, 137),
    "deeppink3"            => (205,  16, 118),
    "deeppink4"            => (139,  10,  80),
    "deepskyblue"          => (  0, 191, 255),
    "deepskyblue1"         => (  0, 191, 255),
    "deepskyblue2"         => (  0, 178, 238),
    "deepskyblue3"         => (  0, 154, 205),
    "deepskyblue4"         => (  0, 104, 139),
    "dimgray"              => (105, 105, 105),
    "dimgrey"              => (105, 105, 105),
    "dodgerblue"           => ( 30, 144, 255),
    "dodgerblue1"          => ( 30, 144, 255),
    "dodgerblue2"          => ( 28, 134, 238),
    "dodgerblue3"          => ( 24, 116, 205),
    "dodgerblue4"          => ( 16,  78, 139),
    "firebrick"            => (178,  34,  34),
    "firebrick1"           => (255,  48,  48),
    "firebrick2"           => (238,  44,  44),
    "firebrick3"           => (205,  38,  38),
    "firebrick4"           => (139,  26,  26),
    "floralwhite"          => (255, 250, 240),
    "forestgreen"          => ( 34, 139,  34),
    "gainsboro"            => (220, 220, 220),
    "ghostwhite"           => (248, 248, 255),
    "gold"                 => (255, 215,   0),
    "gold1"                => (255, 215,   0),
    "gold2"                => (238, 201,   0),
    "gold3"                => (205, 173,   0),
    "gold4"                => (139, 117,   0),
    "goldenrod"            => (218, 165,  32),
    "goldenrod1"           => (255, 193,  37),
    "goldenrod2"           => (238, 180,  34),
    "goldenrod3"           => (205, 155,  29),
    "goldenrod4"           => (139, 105,  20),
    "gray"                 => (190, 190, 190),
    "gray0"                => (  0,   0,   0),
    "gray1"                => (  3,   3,   3),
    "gray2"                => (  5,   5,   5),
    "gray3"                => (  8,   8,   8),
    "gray4"                => ( 10,  10,  10),
    "gray5"                => ( 13,  13,  13),
    "gray6"                => ( 15,  15,  15),
    "gray7"                => ( 18,  18,  18),
    "gray8"                => ( 20,  20,  20),
    "gray9"                => ( 23,  23,  23),
    "gray10"               => ( 26,  26,  26),
    "gray11"               => ( 28,  28,  28),
    "gray12"               => ( 31,  31,  31),
    "gray13"               => ( 33,  33,  33),
    "gray14"               => ( 36,  36,  36),
    "gray15"               => ( 38,  38,  38),
    "gray16"               => ( 41,  41,  41),
    "gray17"               => ( 43,  43,  43),
    "gray18"               => ( 46,  46,  46),
    "gray19"               => ( 48,  48,  48),
    "gray20"               => ( 51,  51,  51),
    "gray21"               => ( 54,  54,  54),
    "gray22"               => ( 56,  56,  56),
    "gray23"               => ( 59,  59,  59),
    "gray24"               => ( 61,  61,  61),
    "gray25"               => ( 64,  64,  64),
    "gray26"               => ( 66,  66,  66),
    "gray27"               => ( 69,  69,  69),
    "gray28"               => ( 71,  71,  71),
    "gray29"               => ( 74,  74,  74),
    "gray30"               => ( 77,  77,  77),
    "gray31"               => ( 79,  79,  79),
    "gray32"               => ( 82,  82,  82),
    "gray33"               => ( 84,  84,  84),
    "gray34"               => ( 87,  87,  87),
    "gray35"               => ( 89,  89,  89),
    "gray36"               => ( 92,  92,  92),
    "gray37"               => ( 94,  94,  94),
    "gray38"               => ( 97,  97,  97),
    "gray39"               => ( 99,  99,  99),
    "gray40"               => (102, 102, 102),
    "gray41"               => (105, 105, 105),
    "gray42"               => (107, 107, 107),
    "gray43"               => (110, 110, 110),
    "gray44"               => (112, 112, 112),
    "gray45"               => (115, 115, 115),
    "gray46"               => (117, 117, 117),
    "gray47"               => (120, 120, 120),
    "gray48"               => (122, 122, 122),
    "gray49"               => (125, 125, 125),
    "gray50"               => (127, 127, 127),
    "gray51"               => (130, 130, 130),
    "gray52"               => (133, 133, 133),
    "gray53"               => (135, 135, 135),
    "gray54"               => (138, 138, 138),
    "gray55"               => (140, 140, 140),
    "gray56"               => (143, 143, 143),
    "gray57"               => (145, 145, 145),
    "gray58"               => (148, 148, 148),
    "gray59"               => (150, 150, 150),
    "gray60"               => (153, 153, 153),
    "gray61"               => (156, 156, 156),
    "gray62"               => (158, 158, 158),
    "gray63"               => (161, 161, 161),
    "gray64"               => (163, 163, 163),
    "gray65"               => (166, 166, 166),
    "gray66"               => (168, 168, 168),
    "gray67"               => (171, 171, 171),
    "gray68"               => (173, 173, 173),
    "gray69"               => (176, 176, 176),
    "gray70"               => (179, 179, 179),
    "gray71"               => (181, 181, 181),
    "gray72"               => (184, 184, 184),
    "gray73"               => (186, 186, 186),
    "gray74"               => (189, 189, 189),
    "gray75"               => (191, 191, 191),
    "gray76"               => (194, 194, 194),
    "gray77"               => (196, 196, 196),
    "gray78"               => (199, 199, 199),
    "gray79"               => (201, 201, 201),
    "gray80"               => (204, 204, 204),
    "gray81"               => (207, 207, 207),
    "gray82"               => (209, 209, 209),
    "gray83"               => (212, 212, 212),
    "gray84"               => (214, 214, 214),
    "gray85"               => (217, 217, 217),
    "gray86"               => (219, 219, 219),
    "gray87"               => (222, 222, 222),
    "gray88"               => (224, 224, 224),
    "gray89"               => (227, 227, 227),
    "gray90"               => (229, 229, 229),
    "gray91"               => (232, 232, 232),
    "gray92"               => (235, 235, 235),
    "gray93"               => (237, 237, 237),
    "gray94"               => (240, 240, 240),
    "gray95"               => (242, 242, 242),
    "gray96"               => (245, 245, 245),
    "gray97"               => (247, 247, 247),
    "gray98"               => (250, 250, 250),
    "gray99"               => (252, 252, 252),
    "gray100"              => (255, 255, 255),
    "green"                => (  0, 255,   0),
    "green1"               => (  0, 255,   0),
    "green2"               => (  0, 238,   0),
    "green3"               => (  0, 205,   0),
    "green4"               => (  0, 139,   0),
    "greenyellow"          => (173, 255,  47),
    "grey"                 => (190, 190, 190),
    "grey0"                => (  0,   0,   0),
    "grey1"                => (  3,   3,   3),
    "grey2"                => (  5,   5,   5),
    "grey3"                => (  8,   8,   8),
    "grey4"                => ( 10,  10,  10),
    "grey5"                => ( 13,  13,  13),
    "grey6"                => ( 15,  15,  15),
    "grey7"                => ( 18,  18,  18),
    "grey8"                => ( 20,  20,  20),
    "grey9"                => ( 23,  23,  23),
    "grey10"               => ( 26,  26,  26),
    "grey11"               => ( 28,  28,  28),
    "grey12"               => ( 31,  31,  31),
    "grey13"               => ( 33,  33,  33),
    "grey14"               => ( 36,  36,  36),
    "grey15"               => ( 38,  38,  38),
    "grey16"               => ( 41,  41,  41),
    "grey17"               => ( 43,  43,  43),
    "grey18"               => ( 46,  46,  46),
    "grey19"               => ( 48,  48,  48),
    "grey20"               => ( 51,  51,  51),
    "grey21"               => ( 54,  54,  54),
    "grey22"               => ( 56,  56,  56),
    "grey23"               => ( 59,  59,  59),
    "grey24"               => ( 61,  61,  61),
    "grey25"               => ( 64,  64,  64),
    "grey26"               => ( 66,  66,  66),
    "grey27"               => ( 69,  69,  69),
    "grey28"               => ( 71,  71,  71),
    "grey29"               => ( 74,  74,  74),
    "grey30"               => ( 77,  77,  77),
    "grey31"               => ( 79,  79,  79),
    "grey32"               => ( 82,  82,  82),
    "grey33"               => ( 84,  84,  84),
    "grey34"               => ( 87,  87,  87),
    "grey35"               => ( 89,  89,  89),
    "grey36"               => ( 92,  92,  92),
    "grey37"               => ( 94,  94,  94),
    "grey38"               => ( 97,  97,  97),
    "grey39"               => ( 99,  99,  99),
    "grey40"               => (102, 102, 102),
    "grey41"               => (105, 105, 105),
    "grey42"               => (107, 107, 107),
    "grey43"               => (110, 110, 110),
    "grey44"               => (112, 112, 112),
    "grey45"               => (115, 115, 115),
    "grey46"               => (117, 117, 117),
    "grey47"               => (120, 120, 120),
    "grey48"               => (122, 122, 122),
    "grey49"               => (125, 125, 125),
    "grey50"               => (127, 127, 127),
    "grey51"               => (130, 130, 130),
    "grey52"               => (133, 133, 133),
    "grey53"               => (135, 135, 135),
    "grey54"               => (138, 138, 138),
    "grey55"               => (140, 140, 140),
    "grey56"               => (143, 143, 143),
    "grey57"               => (145, 145, 145),
    "grey58"               => (148, 148, 148),
    "grey59"               => (150, 150, 150),
    "grey60"               => (153, 153, 153),
    "grey61"               => (156, 156, 156),
    "grey62"               => (158, 158, 158),
    "grey63"               => (161, 161, 161),
    "grey64"               => (163, 163, 163),
    "grey65"               => (166, 166, 166),
    "grey66"               => (168, 168, 168),
    "grey67"               => (171, 171, 171),
    "grey68"               => (173, 173, 173),
    "grey69"               => (176, 176, 176),
    "grey70"               => (179, 179, 179),
    "grey71"               => (181, 181, 181),
    "grey72"               => (184, 184, 184),
    "grey73"               => (186, 186, 186),
    "grey74"               => (189, 189, 189),
    "grey75"               => (191, 191, 191),
    "grey76"               => (194, 194, 194),
    "grey77"               => (196, 196, 196),
    "grey78"               => (199, 199, 199),
    "grey79"               => (201, 201, 201),
    "grey80"               => (204, 204, 204),
    "grey81"               => (207, 207, 207),
    "grey82"               => (209, 209, 209),
    "grey83"               => (212, 212, 212),
    "grey84"               => (214, 214, 214),
    "grey85"               => (217, 217, 217),
    "grey86"               => (219, 219, 219),
    "grey87"               => (222, 222, 222),
    "grey88"               => (224, 224, 224),
    "grey89"               => (227, 227, 227),
    "grey90"               => (229, 229, 229),
    "grey91"               => (232, 232, 232),
    "grey92"               => (235, 235, 235),
    "grey93"               => (237, 237, 237),
    "grey94"               => (240, 240, 240),
    "grey95"               => (242, 242, 242),
    "grey96"               => (245, 245, 245),
    "grey97"               => (247, 247, 247),
    "grey98"               => (250, 250, 250),
    "grey99"               => (252, 252, 252),
    "grey100"              => (255, 255, 255),
    "honeydew"             => (240, 255, 240),
    "honeydew1"            => (240, 255, 240),
    "honeydew2"            => (224, 238, 224),
    "honeydew3"            => (193, 205, 193),
    "honeydew4"            => (131, 139, 131),
    "hotpink"              => (255, 105, 180),
    "hotpink1"             => (255, 110, 180),
    "hotpink2"             => (238, 106, 167),
    "hotpink3"             => (205,  96, 144),
    "hotpink4"             => (139,  58,  98),
    "indianred"            => (205,  92,  92),
    "indianred1"           => (255, 106, 106),
    "indianred2"           => (238,  99,  99),
    "indianred3"           => (205,  85,  85),
    "indianred4"           => (139,  58,  58),
    "ivory"                => (255, 255, 240),
    "ivory1"               => (255, 255, 240),
    "ivory2"               => (238, 238, 224),
    "ivory3"               => (205, 205, 193),
    "ivory4"               => (139, 139, 131),
    "khaki"                => (240, 230, 140),
    "khaki1"               => (255, 246, 143),
    "khaki2"               => (238, 230, 133),
    "khaki3"               => (205, 198, 115),
    "khaki4"               => (139, 134,  78),
    "lavender"             => (230, 230, 250),
    "lavenderblush"        => (255, 240, 245),
    "lavenderblush1"       => (255, 240, 245),
    "lavenderblush2"       => (238, 224, 229),
    "lavenderblush3"       => (205, 193, 197),
    "lavenderblush4"       => (139, 131, 134),
    "lawngreen"            => (124, 252,   0),
    "lemonchiffon"         => (255, 250, 205),
    "lemonchiffon1"        => (255, 250, 205),
    "lemonchiffon2"        => (238, 233, 191),
    "lemonchiffon3"        => (205, 201, 165),
    "lemonchiffon4"        => (139, 137, 112),
    "lightblue"            => (173, 216, 230),
    "lightblue1"           => (191, 239, 255),
    "lightblue2"           => (178, 223, 238),
    "lightblue3"           => (154, 192, 205),
    "lightblue4"           => (104, 131, 139),
    "lightcoral"           => (240, 128, 128),
    "lightcyan"            => (224, 255, 255),
    "lightcyan1"           => (224, 255, 255),
    "lightcyan2"           => (209, 238, 238),
    "lightcyan3"           => (180, 205, 205),
    "lightcyan4"           => (122, 139, 139),
    "lightgoldenrod"       => (238, 221, 130),
    "lightgoldenrod1"      => (255, 236, 139),
    "lightgoldenrod2"      => (238, 220, 130),
    "lightgoldenrod3"      => (205, 190, 112),
    "lightgoldenrod4"      => (139, 129,  76),
    "lightgoldenrodyellow" => (250, 250, 210),
    "lightgray"            => (211, 211, 211),
    "lightgreen"           => (144, 238, 144),
    "lightgrey"            => (211, 211, 211),
    "lightpink"            => (255, 182, 193),
    "lightpink1"           => (255, 174, 185),
    "lightpink2"           => (238, 162, 173),
    "lightpink3"           => (205, 140, 149),
    "lightpink4"           => (139,  95, 101),
    "lightsalmon"          => (255, 160, 122),
    "lightsalmon1"         => (255, 160, 122),
    "lightsalmon2"         => (238, 149, 114),
    "lightsalmon3"         => (205, 129,  98),
    "lightsalmon4"         => (139,  87,  66),
    "lightseagreen"        => ( 32, 178, 170),
    "lightskyblue"         => (135, 206, 250),
    "lightskyblue1"        => (176, 226, 255),
    "lightskyblue2"        => (164, 211, 238),
    "lightskyblue3"        => (141, 182, 205),
    "lightskyblue4"        => ( 96, 123, 139),
    "lightslateblue"       => (132, 112, 255),
    "lightslategray"       => (119, 136, 153),
    "lightslategrey"       => (119, 136, 153),
    "lightsteelblue"       => (176, 196, 222),
    "lightsteelblue1"      => (202, 225, 255),
    "lightsteelblue2"      => (188, 210, 238),
    "lightsteelblue3"      => (162, 181, 205),
    "lightsteelblue4"      => (110, 123, 139),
    "lightyellow"          => (255, 255, 224),
    "lightyellow1"         => (255, 255, 224),
    "lightyellow2"         => (238, 238, 209),
    "lightyellow3"         => (205, 205, 180),
    "lightyellow4"         => (139, 139, 122),
    "limegreen"            => ( 50, 205,  50),
    "linen"                => (250, 240, 230),
    "magenta"              => (255,   0, 255),
    "magenta1"             => (255,   0, 255),
    "magenta2"             => (238,   0, 238),
    "magenta3"             => (205,   0, 205),
    "magenta4"             => (139,   0, 139),
    "maroon"               => (176,  48,  96),
    "maroon1"              => (255,  52, 179),
    "maroon2"              => (238,  48, 167),
    "maroon3"              => (205,  41, 144),
    "maroon4"              => (139,  28,  98),
    "mediumaquamarine"     => (102, 205, 170),
    "mediumblue"           => (  0,   0, 205),
    "mediumorchid"         => (186,  85, 211),
    "mediumorchid1"        => (224, 102, 255),
    "mediumorchid2"        => (209,  95, 238),
    "mediumorchid3"        => (180,  82, 205),
    "mediumorchid4"        => (122,  55, 139),
    "mediumpurple"         => (147, 112, 219),
    "mediumpurple1"        => (171, 130, 255),
    "mediumpurple2"        => (159, 121, 238),
    "mediumpurple3"        => (137, 104, 205),
    "mediumpurple4"        => ( 93,  71, 139),
    "mediumseagreen"       => ( 60, 179, 113),
    "mediumslateblue"      => (123, 104, 238),
    "mediumspringgreen"    => (  0, 250, 154),
    "mediumturquoise"      => ( 72, 209, 204),
    "mediumvioletred"      => (199,  21, 133),
    "midnightblue"         => ( 25,  25, 112),
    "mintcream"            => (245, 255, 250),
    "mistyrose"            => (255, 228, 225),
    "mistyrose1"           => (255, 228, 225),
    "mistyrose2"           => (238, 213, 210),
    "mistyrose3"           => (205, 183, 181),
    "mistyrose4"           => (139, 125, 123),
    "moccasin"             => (255, 228, 181),
    "navajowhite"          => (255, 222, 173),
    "navajowhite1"         => (255, 222, 173),
    "navajowhite2"         => (238, 207, 161),
    "navajowhite3"         => (205, 179, 139),
    "navajowhite4"         => (139, 121,  94),
    "navy"                 => (  0,   0, 128),
    "navyblue"             => (  0,   0, 128),
    "oldlace"              => (253, 245, 230),
    "olivedrab"            => (107, 142,  35),
    "olivedrab1"           => (192, 255,  62),
    "olivedrab2"           => (179, 238,  58),
    "olivedrab3"           => (154, 205,  50),
    "olivedrab4"           => (105, 139,  34),
    "orange"               => (255, 165,   0),
    "orange1"              => (255, 165,   0),
    "orange2"              => (238, 154,   0),
    "orange3"              => (205, 133,   0),
    "orange4"              => (139,  90,   0),
    "orangered"            => (255,  69,   0),
    "orangered1"           => (255,  69,   0),
    "orangered2"           => (238,  64,   0),
    "orangered3"           => (205,  55,   0),
    "orangered4"           => (139,  37,   0),
    "orchid"               => (218, 112, 214),
    "orchid1"              => (255, 131, 250),
    "orchid2"              => (238, 122, 233),
    "orchid3"              => (205, 105, 201),
    "orchid4"              => (139,  71, 137),
    "palegoldenrod"        => (238, 232, 170),
    "palegreen"            => (152, 251, 152),
    "palegreen1"           => (154, 255, 154),
    "palegreen2"           => (144, 238, 144),
    "palegreen3"           => (124, 205, 124),
    "palegreen4"           => ( 84, 139,  84),
    "paleturquoise"        => (175, 238, 238),
    "paleturquoise1"       => (187, 255, 255),
    "paleturquoise2"       => (174, 238, 238),
    "paleturquoise3"       => (150, 205, 205),
    "paleturquoise4"       => (102, 139, 139),
    "palevioletred"        => (219, 112, 147),
    "palevioletred1"       => (255, 130, 171),
    "palevioletred2"       => (238, 121, 159),
    "palevioletred3"       => (205, 104, 137),
    "palevioletred4"       => (139,  71,  93),
    "papayawhip"           => (255, 239, 213),
    "peachpuff"            => (255, 218, 185),
    "peachpuff1"           => (255, 218, 185),
    "peachpuff2"           => (238, 203, 173),
    "peachpuff3"           => (205, 175, 149),
    "peachpuff4"           => (139, 119, 101),
    "peru"                 => (205, 133,  63),
    "pink"                 => (255, 192, 203),
    "pink1"                => (255, 181, 197),
    "pink2"                => (238, 169, 184),
    "pink3"                => (205, 145, 158),
    "pink4"                => (139,  99, 108),
    "plum"                 => (221, 160, 221),
    "plum1"                => (255, 187, 255),
    "plum2"                => (238, 174, 238),
    "plum3"                => (205, 150, 205),
    "plum4"                => (139, 102, 139),
    "powderblue"           => (176, 224, 230),
    "purple"               => (160,  32, 240),
    "purple1"              => (155,  48, 255),
    "purple2"              => (145,  44, 238),
    "purple3"              => (125,  38, 205),
    "purple4"              => ( 85,  26, 139),
    "red"                  => (255,   0,   0),
    "red1"                 => (255,   0,   0),
    "red2"                 => (238,   0,   0),
    "red3"                 => (205,   0,   0),
    "red4"                 => (139,   0,   0),
    "rosybrown"            => (188, 143, 143),
    "rosybrown1"           => (255, 193, 193),
    "rosybrown2"           => (238, 180, 180),
    "rosybrown3"           => (205, 155, 155),
    "rosybrown4"           => (139, 105, 105),
    "royalblue"            => ( 65, 105, 225),
    "royalblue1"           => ( 72, 118, 255),
    "royalblue2"           => ( 67, 110, 238),
    "royalblue3"           => ( 58,  95, 205),
    "royalblue4"           => ( 39,  64, 139),
    "saddlebrown"          => (139,  69,  19),
    "salmon"               => (250, 128, 114),
    "salmon1"              => (255, 140, 105),
    "salmon2"              => (238, 130,  98),
    "salmon3"              => (205, 112,  84),
    "salmon4"              => (139,  76,  57),
    "sandybrown"           => (244, 164,  96),
    "seagreen"             => ( 46, 139,  87),
    "seagreen1"            => ( 84, 255, 159),
    "seagreen2"            => ( 78, 238, 148),
    "seagreen3"            => ( 67, 205, 128),
    "seagreen4"            => ( 46, 139,  87),
    "seashell"             => (255, 245, 238),
    "seashell1"            => (255, 245, 238),
    "seashell2"            => (238, 229, 222),
    "seashell3"            => (205, 197, 191),
    "seashell4"            => (139, 134, 130),
    "sienna"               => (160,  82,  45),
    "sienna1"              => (255, 130,  71),
    "sienna2"              => (238, 121,  66),
    "sienna3"              => (205, 104,  57),
    "sienna4"              => (139,  71,  38),
    "skyblue"              => (135, 206, 235),
    "skyblue1"             => (135, 206, 255),
    "skyblue2"             => (126, 192, 238),
    "skyblue3"             => (108, 166, 205),
    "skyblue4"             => ( 74, 112, 139),
    "slateblue"            => (106,  90, 205),
    "slateblue1"           => (131, 111, 255),
    "slateblue2"           => (122, 103, 238),
    "slateblue3"           => (105,  89, 205),
    "slateblue4"           => ( 71,  60, 139),
    "slategray"            => (112, 128, 144),
    "slategray1"           => (198, 226, 255),
    "slategray2"           => (185, 211, 238),
    "slategray3"           => (159, 182, 205),
    "slategray4"           => (108, 123, 139),
    "slategrey"            => (112, 128, 144),
    "snow"                 => (255, 250, 250),
    "snow1"                => (255, 250, 250),
    "snow2"                => (238, 233, 233),
    "snow3"                => (205, 201, 201),
    "snow4"                => (139, 137, 137),
    "springgreen"          => (  0, 255, 127),
    "springgreen1"         => (  0, 255, 127),
    "springgreen2"         => (  0, 238, 118),
    "springgreen3"         => (  0, 205, 102),
    "springgreen4"         => (  0, 139,  69),
    "steelblue"            => ( 70, 130, 180),
    "steelblue1"           => ( 99, 184, 255),
    "steelblue2"           => ( 92, 172, 238),
    "steelblue3"           => ( 79, 148, 205),
    "steelblue4"           => ( 54, 100, 139),
    "tan"                  => (210, 180, 140),
    "tan1"                 => (255, 165,  79),
    "tan2"                 => (238, 154,  73),
    "tan3"                 => (205, 133,  63),
    "tan4"                 => (139,  90,  43),
    "thistle"              => (216, 191, 216),
    "thistle1"             => (255, 225, 255),
    "thistle2"             => (238, 210, 238),
    "thistle3"             => (205, 181, 205),
    "thistle4"             => (139, 123, 139),
    "tomato"               => (255,  99,  71),
    "tomato1"              => (255,  99,  71),
    "tomato2"              => (238,  92,  66),
    "tomato3"              => (205,  79,  57),
    "tomato4"              => (139,  54,  38),
    "turquoise"            => ( 64, 224, 208),
    "turquoise1"           => (  0, 245, 255),
    "turquoise2"           => (  0, 229, 238),
    "turquoise3"           => (  0, 197, 205),
    "turquoise4"           => (  0, 134, 139),
    "violet"               => (238, 130, 238),
    "violetred"            => (208,  32, 144),
    "violetred1"           => (255,  62, 150),
    "violetred2"           => (238,  58, 140),
    "violetred3"           => (205,  50, 120),
    "violetred4"           => (139,  34,  82),
    "wheat"                => (245, 222, 179),
    "wheat1"               => (255, 231, 186),
    "wheat2"               => (238, 216, 174),
    "wheat3"               => (205, 186, 150),
    "wheat4"               => (139, 126, 102),
    "white"                => (255, 255, 255),
    "whitesmoke"           => (245, 245, 245),
    "yellow"               => (255, 255,   0),
    "yellow1"              => (255, 255,   0),
    "yellow2"              => (238, 238,   0),
    "yellow3"              => (205, 205,   0),
    "yellow4"              => (139, 139,   0),
    "yellowgreen"          => (154, 205,  50)
}

const col_pat_hex1 = r"(#|0x)([[:xdigit:]])([[:xdigit:]])([[:xdigit:]])"
const col_pat_hex2 = r"(#|0x)([[:xdigit:]]{2})([[:xdigit:]]{2})([[:xdigit:]]{2})"
const col_pat_rgb  = r"rgb\((\d+),(\d+),(\d+)\)"

color(c::Color) = c

function color(desc::String)
    local desc_ = replace(desc, " ", "")

    mat = match(col_pat_hex2, desc_)
    if mat != nothing
        return RGB(parse_int(mat.captures[2], 16) / 255.,
                   parse_int(mat.captures[3], 16) / 255.,
                   parse_int(mat.captures[4], 16) / 255.)
    end

    local mat = match(col_pat_hex1, desc_)
    if mat != nothing
        return RGB((16 * parse_int(mat.captures[2], 16)) / 255.,
                   (16 * parse_int(mat.captures[3], 16)) / 255.,
                   (16 * parse_int(mat.captures[4], 16)) / 255.)
    end

    mat = match(col_pat_rgb, desc_)
    if mat != nothing
        return RGB(parse_int(mat.captures[1], 10) / 255.,
                   parse_int(mat.captures[2], 10) / 255.,
                   parse_int(mat.captures[3], 10) / 255.)
    end

    local c
    try
        c = x11_color_names[lowercase(desc_)]
    catch
        error("Unknown color: ", desc)
    end

    return RGB(c[1] / 255., c[2] / 255., c[3] / 255.)
end

colour = color


function cssfmt(c::Color)
    hex(convert(RGB, c))
end


function cssfmt(c::Nothing)
    "none"
end


function json(c::Color)
    quote_string(cssfmt(c))
end


# CIE standard observer, giving the response in XYZ to wavelengths in nanometer
# increments starting at 380nm.
const cie_color_match_table =
    [1.368000e-03  3.900000e-05  6.450001e-03;
     1.502050e-03  4.282640e-05  7.083216e-03;
     1.642328e-03  4.691460e-05  7.745488e-03;
     1.802382e-03  5.158960e-05  8.501152e-03;
     1.995757e-03  5.717640e-05  9.414544e-03;
     2.236000e-03  6.400000e-05  1.054999e-02;
     2.535385e-03  7.234421e-05  1.196580e-02;
     2.892603e-03  8.221224e-05  1.365587e-02;
     3.300829e-03  9.350816e-05  1.558805e-02;
     3.753236e-03  1.061361e-04  1.773015e-02;
     4.243000e-03  1.200000e-04  2.005001e-02;
     4.762389e-03  1.349840e-04  2.251136e-02;
     5.330048e-03  1.514920e-04  2.520288e-02;
     5.978712e-03  1.702080e-04  2.827972e-02;
     6.741117e-03  1.918160e-04  3.189704e-02;
     7.650000e-03  2.170000e-04  3.621000e-02;
     8.751373e-03  2.469067e-04  4.143771e-02;
     1.002888e-02  2.812400e-04  4.750372e-02;
     1.142170e-02  3.185200e-04  5.411988e-02;
     1.286901e-02  3.572667e-04  6.099803e-02;
     1.431000e-02  3.960000e-04  6.785001e-02;
     1.570443e-02  4.337147e-04  7.448632e-02;
     1.714744e-02  4.730240e-04  8.136156e-02;
     1.878122e-02  5.178760e-04  8.915364e-02;
     2.074801e-02  5.722187e-04  9.854048e-02;
     2.319000e-02  6.400000e-04  1.102000e-01;
     2.620736e-02  7.245600e-04  1.246133e-01;
     2.978248e-02  8.255000e-04  1.417017e-01;
     3.388092e-02  9.411600e-04  1.613035e-01;
     3.846824e-02  1.069880e-03  1.832568e-01;
     4.351000e-02  1.210000e-03  2.074000e-01;
     4.899560e-02  1.362091e-03  2.336921e-01;
     5.502260e-02  1.530752e-03  2.626114e-01;
     6.171880e-02  1.720368e-03  2.947746e-01;
     6.921200e-02  1.935323e-03  3.307985e-01;
     7.763000e-02  2.180000e-03  3.713000e-01;
     8.695811e-02  2.454800e-03  4.162091e-01;
     9.717672e-02  2.764000e-03  4.654642e-01;
     1.084063e-01  3.117800e-03  5.196948e-01;
     1.207672e-01  3.526400e-03  5.795303e-01;
     1.343800e-01  4.000000e-03  6.456000e-01;
     1.493582e-01  4.546240e-03  7.184838e-01;
     1.653957e-01  5.159320e-03  7.967133e-01;
     1.819831e-01  5.829280e-03  8.778459e-01;
     1.986110e-01  6.546160e-03  9.594390e-01;
     2.147700e-01  7.300000e-03  1.039050e+00;
     2.301868e-01  8.086507e-03  1.115367e+00;
     2.448797e-01  8.908720e-03  1.188497e+00;
     2.587773e-01  9.767680e-03  1.258123e+00;
     2.718079e-01  1.066443e-02  1.323930e+00;
     2.839000e-01  1.160000e-02  1.385600e+00;
     2.949438e-01  1.257317e-02  1.442635e+00;
     3.048965e-01  1.358272e-02  1.494803e+00;
     3.137873e-01  1.462968e-02  1.542190e+00;
     3.216454e-01  1.571509e-02  1.584881e+00;
     3.285000e-01  1.684000e-02  1.622960e+00;
     3.343513e-01  1.800736e-02  1.656405e+00;
     3.392101e-01  1.921448e-02  1.685296e+00;
     3.431213e-01  2.045392e-02  1.709874e+00;
     3.461296e-01  2.171824e-02  1.730382e+00;
     3.482800e-01  2.300000e-02  1.747060e+00;
     3.495999e-01  2.429461e-02  1.760045e+00;
     3.501474e-01  2.561024e-02  1.769623e+00;
     3.500130e-01  2.695857e-02  1.776264e+00;
     3.492870e-01  2.835125e-02  1.780433e+00;
     3.480600e-01  2.980000e-02  1.782600e+00;
     3.463733e-01  3.131083e-02  1.782968e+00;
     3.442624e-01  3.288368e-02  1.781700e+00;
     3.418088e-01  3.452112e-02  1.779198e+00;
     3.390941e-01  3.622571e-02  1.775867e+00;
     3.362000e-01  3.800000e-02  1.772110e+00;
     3.331977e-01  3.984667e-02  1.768259e+00;
     3.300411e-01  4.176800e-02  1.764039e+00;
     3.266357e-01  4.376600e-02  1.758944e+00;
     3.228868e-01  4.584267e-02  1.752466e+00;
     3.187000e-01  4.800000e-02  1.744100e+00;
     3.140251e-01  5.024368e-02  1.733559e+00;
     3.088840e-01  5.257304e-02  1.720858e+00;
     3.032904e-01  5.498056e-02  1.705937e+00;
     2.972579e-01  5.745872e-02  1.688737e+00;
     2.908000e-01  6.000000e-02  1.669200e+00;
     2.839701e-01  6.260197e-02  1.647529e+00;
     2.767214e-01  6.527752e-02  1.623413e+00;
     2.689178e-01  6.804208e-02  1.596022e+00;
     2.604227e-01  7.091109e-02  1.564528e+00;
     2.511000e-01  7.390000e-02  1.528100e+00;
     2.408475e-01  7.701600e-02  1.486111e+00;
     2.298512e-01  8.026640e-02  1.439521e+00;
     2.184072e-01  8.366680e-02  1.389880e+00;
     2.068115e-01  8.723280e-02  1.338736e+00;
     1.953600e-01  9.098000e-02  1.287640e+00;
     1.842136e-01  9.491755e-02  1.237422e+00;
     1.733273e-01  9.904584e-02  1.187824e+00;
     1.626881e-01  1.033674e-01  1.138761e+00;
     1.522833e-01  1.078846e-01  1.090148e+00;
     1.421000e-01  1.126000e-01  1.041900e+00;
     1.321786e-01  1.175320e-01  9.941976e-01;
     1.225696e-01  1.226744e-01  9.473473e-01;
     1.132752e-01  1.279928e-01  9.014531e-01;
     1.042979e-01  1.334528e-01  8.566193e-01;
     9.564000e-02  1.390200e-01  8.129501e-01;
     8.729955e-02  1.446764e-01  7.705173e-01;
     7.930804e-02  1.504693e-01  7.294448e-01;
     7.171776e-02  1.564619e-01  6.899136e-01;
     6.458099e-02  1.627177e-01  6.521049e-01;
     5.795001e-02  1.693000e-01  6.162000e-01;
     5.186211e-02  1.762431e-01  5.823286e-01;
     4.628152e-02  1.835581e-01  5.504162e-01;
     4.115088e-02  1.912735e-01  5.203376e-01;
     3.641283e-02  1.994180e-01  4.919673e-01;
     3.201000e-02  2.080200e-01  4.651800e-01;
     2.791720e-02  2.171199e-01  4.399246e-01;
     2.414440e-02  2.267345e-01  4.161836e-01;
     2.068700e-02  2.368571e-01  3.938822e-01;
     1.754040e-02  2.474812e-01  3.729459e-01;
     1.470000e-02  2.586000e-01  3.533000e-01;
     1.216179e-02  2.701849e-01  3.348578e-01;
     9.919960e-03  2.822939e-01  3.175521e-01;
     7.967240e-03  2.950505e-01  3.013375e-01;
     6.296346e-03  3.085780e-01  2.861686e-01;
     4.900000e-03  3.230000e-01  2.720000e-01;
     3.777173e-03  3.384021e-01  2.588171e-01;
     2.945320e-03  3.546858e-01  2.464838e-01;
     2.424880e-03  3.716986e-01  2.347718e-01;
     2.236293e-03  3.892875e-01  2.234533e-01;
     2.400000e-03  4.073000e-01  2.123000e-01;
     2.925520e-03  4.256299e-01  2.011692e-01;
     3.836560e-03  4.443096e-01  1.901196e-01;
     5.174840e-03  4.633944e-01  1.792254e-01;
     6.982080e-03  4.829395e-01  1.685608e-01;
     9.300000e-03  5.030000e-01  1.582000e-01;
     1.214949e-02  5.235693e-01  1.481383e-01;
     1.553588e-02  5.445120e-01  1.383758e-01;
     1.947752e-02  5.656900e-01  1.289942e-01;
     2.399277e-02  5.869653e-01  1.200751e-01;
     2.910000e-02  6.082000e-01  1.117000e-01;
     3.481485e-02  6.293456e-01  1.039048e-01;
     4.112016e-02  6.503068e-01  9.666748e-02;
     4.798504e-02  6.708752e-01  8.998272e-02;
     5.537861e-02  6.908424e-01  8.384531e-02;
     6.327000e-02  7.100000e-01  7.824999e-02;
     7.163501e-02  7.281852e-01  7.320899e-02;
     8.046224e-02  7.454636e-01  6.867816e-02;
     8.973996e-02  7.619694e-01  6.456784e-02;
     9.945645e-02  7.778368e-01  6.078835e-02;
     1.096000e-01  7.932000e-01  5.725001e-02;
     1.201674e-01  8.081104e-01  5.390435e-02;
     1.311145e-01  8.224962e-01  5.074664e-02;
     1.423679e-01  8.363068e-01  4.775276e-02;
     1.538542e-01  8.494916e-01  4.489859e-02;
     1.655000e-01  8.620000e-01  4.216000e-02;
     1.772571e-01  8.738108e-01  3.950728e-02;
     1.891400e-01  8.849624e-01  3.693564e-02;
     2.011694e-01  8.954936e-01  3.445836e-02;
     2.133658e-01  9.054432e-01  3.208872e-02;
     2.257499e-01  9.148501e-01  2.984000e-02;
     2.383209e-01  9.237348e-01  2.771181e-02;
     2.510668e-01  9.320924e-01  2.569444e-02;
     2.639922e-01  9.399226e-01  2.378716e-02;
     2.771017e-01  9.472252e-01  2.198925e-02;
     2.904000e-01  9.540000e-01  2.030000e-02;
     3.038912e-01  9.602561e-01  1.871805e-02;
     3.175726e-01  9.660074e-01  1.724036e-02;
     3.314384e-01  9.712606e-01  1.586364e-02;
     3.454828e-01  9.760225e-01  1.458461e-02;
     3.597000e-01  9.803000e-01  1.340000e-02;
     3.740839e-01  9.840924e-01  1.230723e-02;
     3.886396e-01  9.874182e-01  1.130188e-02;
     4.033784e-01  9.903128e-01  1.037792e-02;
     4.183115e-01  9.928116e-01  9.529306e-03;
     4.334499e-01  9.949501e-01  8.749999e-03;
     4.487953e-01  9.967108e-01  8.035200e-03;
     4.643360e-01  9.980983e-01  7.381600e-03;
     4.800640e-01  9.991120e-01  6.785400e-03;
     4.959713e-01  9.997482e-01  6.242800e-03;
     5.120501e-01  1.000000e+00  5.749999e-03;
     5.282959e-01  9.998567e-01  5.303600e-03;
     5.446916e-01  9.993046e-01  4.899800e-03;
     5.612094e-01  9.983255e-01  4.534200e-03;
     5.778215e-01  9.968987e-01  4.202400e-03;
     5.945000e-01  9.950000e-01  3.900000e-03;
     6.112209e-01  9.926005e-01  3.623200e-03;
     6.279758e-01  9.897426e-01  3.370600e-03;
     6.447602e-01  9.864444e-01  3.141400e-03;
     6.615697e-01  9.827241e-01  2.934800e-03;
     6.784000e-01  9.786000e-01  2.749999e-03;
     6.952392e-01  9.740837e-01  2.585200e-03;
     7.120586e-01  9.691712e-01  2.438600e-03;
     7.288284e-01  9.638568e-01  2.309400e-03;
     7.455188e-01  9.581349e-01  2.196800e-03;
     7.621000e-01  9.520000e-01  2.100000e-03;
     7.785432e-01  9.454504e-01  2.017733e-03;
     7.948256e-01  9.384992e-01  1.948200e-03;
     8.109264e-01  9.311628e-01  1.889800e-03;
     8.268248e-01  9.234576e-01  1.840933e-03;
     8.425000e-01  9.154000e-01  1.800000e-03;
     8.579325e-01  9.070064e-01  1.766267e-03;
     8.730816e-01  8.982772e-01  1.737800e-03;
     8.878944e-01  8.892048e-01  1.711200e-03;
     9.023181e-01  8.797816e-01  1.683067e-03;
     9.163000e-01  8.700000e-01  1.650001e-03;
     9.297995e-01  8.598613e-01  1.610133e-03;
     9.427984e-01  8.493920e-01  1.564400e-03;
     9.552776e-01  8.386220e-01  1.513600e-03;
     9.672179e-01  8.275813e-01  1.458533e-03;
     9.786000e-01  8.163000e-01  1.400000e-03;
     9.893856e-01  8.047947e-01  1.336667e-03;
     9.995488e-01  7.930820e-01  1.270000e-03;
     1.009089e+00  7.811920e-01  1.205000e-03;
     1.018006e+00  7.691547e-01  1.146667e-03;
     1.026300e+00  7.570000e-01  1.100000e-03;
     1.033983e+00  7.447541e-01  1.068800e-03;
     1.040986e+00  7.324224e-01  1.049400e-03;
     1.047188e+00  7.200036e-01  1.035600e-03;
     1.052467e+00  7.074965e-01  1.021200e-03;
     1.056700e+00  6.949000e-01  1.000000e-03;
     1.059794e+00  6.822192e-01  9.686400e-04;
     1.061799e+00  6.694716e-01  9.299200e-04;
     1.062807e+00  6.566744e-01  8.868800e-04;
     1.062910e+00  6.438448e-01  8.425600e-04;
     1.062200e+00  6.310000e-01  8.000000e-04;
     1.060735e+00  6.181555e-01  7.609600e-04;
     1.058444e+00  6.053144e-01  7.236800e-04;
     1.055224e+00  5.924756e-01  6.859200e-04;
     1.050977e+00  5.796379e-01  6.454400e-04;
     1.045600e+00  5.668000e-01  6.000000e-04;
     1.039037e+00  5.539611e-01  5.478667e-04;
     1.031361e+00  5.411372e-01  4.916000e-04;
     1.022666e+00  5.283528e-01  4.354000e-04;
     1.013048e+00  5.156323e-01  3.834667e-04;
     1.002600e+00  5.030000e-01  3.400000e-04;
     9.913675e-01  4.904688e-01  3.072533e-04;
     9.793314e-01  4.780304e-01  2.831600e-04;
     9.664916e-01  4.656776e-01  2.654400e-04;
     9.528479e-01  4.534032e-01  2.518133e-04;
     9.384000e-01  4.412000e-01  2.400000e-04;
     9.231940e-01  4.290800e-01  2.295467e-04;
     9.072440e-01  4.170360e-01  2.206400e-04;
     8.905020e-01  4.050320e-01  2.119600e-04;
     8.729200e-01  3.930320e-01  2.021867e-04;
     8.544499e-01  3.810000e-01  1.900000e-04;
     8.350840e-01  3.689184e-01  1.742133e-04;
     8.149460e-01  3.568272e-01  1.556400e-04;
     7.941860e-01  3.447768e-01  1.359600e-04;
     7.729540e-01  3.328176e-01  1.168533e-04;
     7.514000e-01  3.210000e-01  1.000000e-04;
     7.295836e-01  3.093381e-01  8.613333e-05;
     7.075888e-01  2.978504e-01  7.460000e-05;
     6.856022e-01  2.865936e-01  6.500000e-05;
     6.638104e-01  2.756245e-01  5.693333e-05;
     6.424000e-01  2.650000e-01  4.999999e-05;
     6.215149e-01  2.547632e-01  4.416000e-05;
     6.011138e-01  2.448896e-01  3.948000e-05;
     5.811052e-01  2.353344e-01  3.572000e-05;
     5.613977e-01  2.260528e-01  3.264000e-05;
     5.419000e-01  2.170000e-01  3.000000e-05;
     5.225995e-01  2.081616e-01  2.765333e-05;
     5.035464e-01  1.995488e-01  2.556000e-05;
     4.847436e-01  1.911552e-01  2.364000e-05;
     4.661939e-01  1.829744e-01  2.181333e-05;
     4.479000e-01  1.750000e-01  2.000000e-05;
     4.298613e-01  1.672235e-01  1.813333e-05;
     4.120980e-01  1.596464e-01  1.620000e-05;
     3.946440e-01  1.522776e-01  1.420000e-05;
     3.775333e-01  1.451259e-01  1.213333e-05;
     3.608000e-01  1.382000e-01  1.000000e-05;
     3.444563e-01  1.315003e-01  7.733333e-06;
     3.285168e-01  1.250248e-01  5.400000e-06;
     3.130192e-01  1.187792e-01  3.200000e-06;
     2.980011e-01  1.127691e-01  1.333333e-06;
     2.835000e-01  1.070000e-01  0.000000e+00;
     2.695448e-01  1.014762e-01  0.000000e+00;
     2.561184e-01  9.618864e-02  0.000000e+00;
     2.431896e-01  9.112296e-02  0.000000e+00;
     2.307272e-01  8.626485e-02  0.000000e+00;
     2.187000e-01  8.160000e-02  0.000000e+00;
     2.070971e-01  7.712064e-02  0.000000e+00;
     1.959232e-01  7.282552e-02  0.000000e+00;
     1.851708e-01  6.871008e-02  0.000000e+00;
     1.748323e-01  6.476976e-02  0.000000e+00;
     1.649000e-01  6.100000e-02  0.000000e+00;
     1.553667e-01  5.739621e-02  0.000000e+00;
     1.462300e-01  5.395504e-02  0.000000e+00;
     1.374900e-01  5.067376e-02  0.000000e+00;
     1.291467e-01  4.754965e-02  0.000000e+00;
     1.212000e-01  4.458000e-02  0.000000e+00;
     1.136397e-01  4.175872e-02  0.000000e+00;
     1.064650e-01  3.908496e-02  0.000000e+00;
     9.969044e-02  3.656384e-02  0.000000e+00;
     9.333061e-02  3.420048e-02  0.000000e+00;
     8.740000e-02  3.200000e-02  0.000000e+00;
     8.190096e-02  2.996261e-02  0.000000e+00;
     7.680428e-02  2.807664e-02  0.000000e+00;
     7.207712e-02  2.632936e-02  0.000000e+00;
     6.768664e-02  2.470805e-02  0.000000e+00;
     6.360000e-02  2.320000e-02  0.000000e+00;
     5.980685e-02  2.180077e-02  0.000000e+00;
     5.628216e-02  2.050112e-02  0.000000e+00;
     5.297104e-02  1.928108e-02  0.000000e+00;
     4.981861e-02  1.812069e-02  0.000000e+00;
     4.677000e-02  1.700000e-02  0.000000e+00;
     4.378405e-02  1.590379e-02  0.000000e+00;
     4.087536e-02  1.483718e-02  0.000000e+00;
     3.807264e-02  1.381068e-02  0.000000e+00;
     3.540461e-02  1.283478e-02  0.000000e+00;
     3.290000e-02  1.192000e-02  0.000000e+00;
     3.056419e-02  1.106831e-02  0.000000e+00;
     2.838056e-02  1.027339e-02  0.000000e+00;
     2.634484e-02  9.533311e-03  0.000000e+00;
     2.445275e-02  8.846157e-03  0.000000e+00;
     2.270000e-02  8.210000e-03  0.000000e+00;
     2.108429e-02  7.623781e-03  0.000000e+00;
     1.959988e-02  7.085424e-03  0.000000e+00;
     1.823732e-02  6.591476e-03  0.000000e+00;
     1.698717e-02  6.138485e-03  0.000000e+00;
     1.584000e-02  5.723000e-03  0.000000e+00;
     1.479064e-02  5.343059e-03  0.000000e+00;
     1.383132e-02  4.995796e-03  0.000000e+00;
     1.294868e-02  4.676404e-03  0.000000e+00;
     1.212920e-02  4.380075e-03  0.000000e+00;
     1.135916e-02  4.102000e-03  0.000000e+00;
     1.062935e-02  3.838453e-03  0.000000e+00;
     9.938846e-03  3.589099e-03  0.000000e+00;
     9.288422e-03  3.354219e-03  0.000000e+00;
     8.678854e-03  3.134093e-03  0.000000e+00;
     8.110916e-03  2.929000e-03  0.000000e+00;
     7.582388e-03  2.738139e-03  0.000000e+00;
     7.088746e-03  2.559876e-03  0.000000e+00;
     6.627313e-03  2.393244e-03  0.000000e+00;
     6.195408e-03  2.237275e-03  0.000000e+00;
     5.790346e-03  2.091000e-03  0.000000e+00;
     5.409826e-03  1.953587e-03  0.000000e+00;
     5.052583e-03  1.824580e-03  0.000000e+00;
     4.717512e-03  1.703580e-03  0.000000e+00;
     4.403507e-03  1.590187e-03  0.000000e+00;
     4.109457e-03  1.484000e-03  0.000000e+00;
     3.833913e-03  1.384496e-03  0.000000e+00;
     3.575748e-03  1.291268e-03  0.000000e+00;
     3.334342e-03  1.204092e-03  0.000000e+00;
     3.109075e-03  1.122744e-03  0.000000e+00;
     2.899327e-03  1.047000e-03  0.000000e+00;
     2.704348e-03  9.765896e-04  0.000000e+00;
     2.523020e-03  9.111088e-04  0.000000e+00;
     2.354168e-03  8.501332e-04  0.000000e+00;
     2.196616e-03  7.932384e-04  0.000000e+00;
     2.049190e-03  7.400000e-04  0.000000e+00;
     1.910960e-03  6.900827e-04  0.000000e+00;
     1.781438e-03  6.433100e-04  0.000000e+00;
     1.660110e-03  5.994960e-04  0.000000e+00;
     1.546459e-03  5.584547e-04  0.000000e+00;
     1.439971e-03  5.200000e-04  0.000000e+00;
     1.340042e-03  4.839136e-04  0.000000e+00;
     1.246275e-03  4.500528e-04  0.000000e+00;
     1.158471e-03  4.183452e-04  0.000000e+00;
     1.076430e-03  3.887184e-04  0.000000e+00;
     9.999493e-04  3.611000e-04  0.000000e+00;
     9.287358e-04  3.353835e-04  0.000000e+00;
     8.624332e-04  3.114404e-04  0.000000e+00;
     8.007503e-04  2.891656e-04  0.000000e+00;
     7.433960e-04  2.684539e-04  0.000000e+00;
     6.900786e-04  2.492000e-04  0.000000e+00;
     6.405156e-04  2.313019e-04  0.000000e+00;
     5.945021e-04  2.146856e-04  0.000000e+00;
     5.518646e-04  1.992884e-04  0.000000e+00;
     5.124290e-04  1.850475e-04  0.000000e+00;
     4.760213e-04  1.719000e-04  0.000000e+00;
     4.424536e-04  1.597781e-04  0.000000e+00;
     4.115117e-04  1.486044e-04  0.000000e+00;
     3.829814e-04  1.383016e-04  0.000000e+00;
     3.566491e-04  1.287925e-04  0.000000e+00;
     3.323011e-04  1.200000e-04  0.000000e+00;
     3.097586e-04  1.118595e-04  0.000000e+00;
     2.888871e-04  1.043224e-04  0.000000e+00;
     2.695394e-04  9.733560e-05  0.000000e+00;
     2.515682e-04  9.084587e-05  0.000000e+00;
     2.348261e-04  8.480000e-05  0.000000e+00;
     2.191710e-04  7.914667e-05  0.000000e+00;
     2.045258e-04  7.385800e-05  0.000000e+00;
     1.908405e-04  6.891600e-05  0.000000e+00;
     1.780654e-04  6.430267e-05  0.000000e+00;
     1.661505e-04  6.000000e-05  0.000000e+00;
     1.550236e-04  5.598187e-05  0.000000e+00;
     1.446219e-04  5.222560e-05  0.000000e+00;
     1.349098e-04  4.871840e-05  0.000000e+00;
     1.258520e-04  4.544747e-05  0.000000e+00;
     1.174130e-04  4.240000e-05  0.000000e+00;
     1.095515e-04  3.956104e-05  0.000000e+00;
     1.022245e-04  3.691512e-05  0.000000e+00;
     9.539445e-05  3.444868e-05  0.000000e+00;
     8.902390e-05  3.214816e-05  0.000000e+00;
     8.307527e-05  3.000000e-05  0.000000e+00;
     7.751269e-05  2.799125e-05  0.000000e+00;
     7.231304e-05  2.611356e-05  0.000000e+00;
     6.745778e-05  2.436024e-05  0.000000e+00;
     6.292844e-05  2.272461e-05  0.000000e+00;
     5.870652e-05  2.120000e-05  0.000000e+00;
     5.477028e-05  1.977855e-05  0.000000e+00;
     5.109918e-05  1.845285e-05  0.000000e+00;
     4.767654e-05  1.721687e-05  0.000000e+00;
     4.448567e-05  1.606459e-05  0.000000e+00;
     4.150994e-05  1.499000e-05  0.000000e+00]


# Evaluate the CIE standard observer color match function.
#
# Args:
#   wavelen: Wavelength of stimulus in nanometers.
#
# Returns:
#   XYZ value of perceived color.
#
function cie_color_match(wavelen::Real)
    a = floor(wavelen)
    ac = 380 <= a <= 780 ? cie_color_match_table[a - 380 + 1,:] : [0,0,0]

    if wavelen != a
        b = ceil(wavelen)
        bc = 380 <= b <= 780 ? cie_color_match_table[b - 380 + 1,:] : [0,0,0]
        p = wavelen - a
        ac = p * bc + (1.0 - p) * ac
    end
    XYZ(ac[1], ac[2], ac[3])
end



# Color Difference Metrics
# ------------------------


# Evaluate the CIEDE2000 color difference formula, implemented according to:
#   Klaus Witt, CIE Color Difference Metrics, Colorimetry: Understanding the CIE
#   System. 2007
#
# Args:
#   a, b: Any two colors.
#
# Returns:
#   The CIEDE2000 color difference metric evaluated between a and b.
#
function colordiff(a::Color, b::Color)
    a = convert(LAB, a)
    b = convert(LAB, b)

    ac, bc = sqrt(a.a^2 + a.b^2), sqrt(b.a^2 + b.b^2)
    mc = (ac + bc)/2
    g = (1 - sqrt(mc^7 / (mc^7 + 25^7))) / 2
    a.a *= 1 + g
    b.a *= 1 + g

    a = convert(LCHab, a)
    b = convert(LCHab, b)

    dl, dc, dh = (a.l - b.l), (a.c - b.c), (a.h - b.h)
    dh = 2 * sqrt(a.c * b.c) * sind(dh/2)

    ml, mc, mh = (a.l+b.l)/2, (a.c+b.c)/2, (a.h+b.h)/2

    # lightness weight
    mls = (ml - 50)^2
    sl = 1.0 + 0.015 * mls / sqrt(20 + mls)

    # chroma weight
    sc = 1 + 0.045mc

    # hue weight
    t = 1 - 0.17 * cosd(mh - 30) +
            0.24 * cosd(2mh) +
            0.32 * cosd(3mh + 6) -
            0.20 * cosd(4mh - 63)
    sh = 1 + 0.015 * mc * t

    # rotation term
    dtheta = 30 * exp(-((mh - 275)/25)^2)
    cr = 2 * sqrt(mc^7 / (mc^7 + 25^7))
    tr = -sind(2*dtheta) * cr

    sqrt((dl/sl)^2 + (dc/sc)^2 + (dh/sh)^2 +
         tr * (dc/sc) * (dh/sh))
end


#colordiff{T <: Color}(a::Color, bs::Vector{T}) = sum([colordiff(a, b) for b in bs])


#function colordiff(a::LCHab, b::LCHab)
    #dl, dc, dh = (a.l - b.l), (a.c - b.c), (a.h - b.h)
    #dh = 2 * sqrt(a.c * b.c) * sind(dh/2)
    #sqrt(dl^2 + dc^2 + dh^2)
#end


# Color Scale Generation
# ----------------------

# Compute the color difference matrix.
function pairwise_colordiff!{T <: Color}(D::Matrix{Float64}, cs::Vector{T})
    n = length(cs)
    for i in 1:n, j in (i+1):n
        D[i,j] = colordiff(cs[i], cs[j])
    end
end


# Update the color difference matrix.
function pairwise_colordiff_update!{T <: Color}(D::Matrix{Float64},
                                                cs::Vector{T},
                                                k::Integer)
    n = length(cs)
    for j in (k+1):n
        D[k,j] = colordiff(cs[k], cs[j])
    end

    for i in 1:(k-1)
        D[i,k] = colordiff(cs[k], cs[i])
    end
end


function min_pairwise(D)
    dmin = Inf
    n = size(D)[1]
    for i in 1:n, j in (i+1):n
        if D[i,j] < dmin
            dmin = D[i,j]
        end
    end
    dmin
end


# Generate a random palette of n distinguishable colors.
function distinguishable_colors(n::Integer, transform::Function)
    minl, maxl = 40.0, 90.0
    minc, maxc = 20.0, 70.0

    # Seed colors.
    cs = Color[convert(LAB, LCHab(minl + rand() * (maxl - minl),
                                  minc + rand() * (maxc - minc),
                                 360.0 * rand())) for i in 1:n]
    cst = Color[transform(c) for c in cs]

    # Pairwise difference matrix
    D = zeros(n, n)
    Dp = zeros(n, n)
    m = int(n * (n-1) / 2) # size of the triangular distance matrix
    pairwise_colordiff!(D, cst)
    d = min_pairwise(D)

    iterations = 5000
    for iteration in 1:iterations
        for i in 1:n
            c = cs[i]
            ct = cst[i]

            # propose
            cs[i] = LCHab(minl + rand() * (maxl - minl),
                          minc + rand() * (maxc - minc),
                          360.0 * rand())
            cst[i] = transform(cs[i])

            copy!(Dp, D)
            pairwise_colordiff_update!(Dp, cst, i)
            dp = min_pairwise(Dp)

            # accept/reject
            improvement = dp - d
            if improvement >= 0
                d = dp
                D, Dp = Dp, D
            else
                cs[i] = c
                cst[i] = ct
            end
        end
    end

    println(D)

    # TODO: just fix this in gadfly
    for c in cs
        println(c)
    end
    cs
end


