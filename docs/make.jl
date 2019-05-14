using Documenter, Compose
import Cairo

struct SVGJSWritable{T}
    x :: T
end
Base.show(io::IO, m::MIME"text/html", x::SVGJSWritable) = show(io, m, x.x)

makedocs(
    modules = [Compose],
    clean = false,
    sitename = "Compose.jl",
    pages = Any[
        "Home" => "index.md",
        "Tutorial" => "tutorial.md",
        "Gallery" => Any[
            "Forms" => "gallery/forms.md",
            "Properties"=> "gallery/properties.md",
            "Transformations"=> "gallery/transforms.md",
            ],
        "Library" => "library.md"
    ]
)

deploydocs(
    repo   = "github.com/GiovineItalia/Compose.jl.git",
)
