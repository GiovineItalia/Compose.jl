using Documenter, Compose

makedocs(
    modules = [Compose],
    clean = true,
    format = :html,
    sitename = "Compose.jl",
    pages = Any[
        "Home" => "index.md",
        "Tutorial" => "tutorial.md",
        "Gallery" => Any[
            "Forms" => "gallery/forms.md",
            ],
        "Library" => "library.md"
    ]
)

deploydocs(
    repo   = "github.com/GiovineItalia/Compose.jl.git",
    julia  = "1.0",
    osname = "linux",
    deps = nothing,
    make = nothing,
    target = "build"
)
