```@meta
Author = ["Daniel C. Jones", "Gio Borje", "Tamas Nagy"]
```

# Compose

Compose is a declarative vector graphics system written in Julia. It's designed
to simplify the creation of complex graphics and serves as the basis of the
[Gadfly](https://github.com/GiovineItalia/Gadfly.jl) data visualization package.


## Package features

- Renders publication quality graphics to SVG, PNG, Postscript, PDF and PGF
- Intuitive and consistent interface
- Works with [Jupyter](http://jupyter.org/) notebooks via [IJulia](https://github.com/JuliaLang/IJulia.jl) out of the box

## Installation

The latest release of **Compose** can be installed from the Julia REPL prompt with

```julia
julia> Pkg.add("Compose")
```

This installs the package and any missing dependencies.  From there, the
simplest of graphics can be rendered to your default internet browser with

```julia
julia> using Compose
julia> compose(context(), circle(), fill("gold"))
```

Now that you have it installed, check out the [Tutorial](@ref) and the [Forms](@ref forms_gallery) gallery.


## Influences

Compose is intended as a futuristic version of the R library
[grid](http://www.stat.auckland.ac.nz/~paul/grid/grid.html), and so takes a few
ideas from grid. The Compose canvas is roughly equivalent to a viewport in grid,
for example. Compose was also inspired by the admirable Haskell library
[Diagrams](http://projects.haskell.org/diagrams/).
