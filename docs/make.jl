using Documenter, Compose

makedocs(
    modules = [Compose],
    clean = true,
    format = Documenter.Formats.HTML,
    sitename = "Compose.jl",
    pages = Any[
        "Home" => "index.md"
    ]
)

deploydocs(
    repo   = "github.com/dcjones/Compose.jl.git",
    julia  = "0.5",
    osname = "linux",
    deps = nothing,
    make = nothing,
    target = "build"
)
