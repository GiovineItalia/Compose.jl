using Documenter, Compose

makedocs(
    modules = [Compose],
    clean = true,
    format = :html,
    sitename = "Compose.jl",
    pages = Any[
        "Home" => "index.md"
        "Library" => "library.md"
    ]
)

deploydocs(
    repo   = "github.com/GiovineItalia/Compose.jl.git",
    julia  = "0.5",
    osname = "linux",
    deps = nothing,
    make = nothing,
    target = "build"
)
