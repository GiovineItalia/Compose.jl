
ccall((:FcInit, :libfontconfig), Uint8, ())

# By default fontconfig on OSX does not include user fonts.
if OS_NAME == :Darwin
    ccall((:FcConfigAppFontAddDir, :libfontconfig),
          Uint8, (Ptr{Void}, Ptr{Uint8}),
          C_NULL, b"~/Library/Fonts")
end

const FcMatchPattern = uint32(0)
const FcMatchFont    = uint32(1)
const FcMatchScan    = uint32(2)

function fontconfig_match(desc::String)
    pat = ccall((:FcNameParse, :libfontconfig),
                Ptr{Void}, (Ptr{Uint8},), bytestring(desc))

    ccall((:FcConfigSubstitute, :libfontconfig),
          Uint8, (Ptr{Void}, Ptr{Void}, Int32),
          C_NULL, pat, FcMatchPattern)

    ccall((:FcDefaultSubstitute, :libfontconfig),
          Void, (Ptr{Void},), pat)

    result = Int32[0]
    mat = ccall((:FcFontMatch, :libfontconfig),
                Ptr{Void}, (Ptr{Void}, Ptr{Void}, Ptr{Int32}),
                C_NULL, pat, result)

    matstr = ccall((:FcPatternFormat, :libfontconfig),
                   Ptr{Uint8}, (Ptr{Void}, Ptr{Uint8}),
                   mat, b"%{family} %{style}")
    matstr = bytestring(matstr)

    ccall((:FcPatternDestroy, :libfontconfig),
          Void, (Ptr{Void},), pat)

    ccall((:FcPatternDestroy, :libfontconfig),
          Void, (Ptr{Void},), mat)

    matstr
end

