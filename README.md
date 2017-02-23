# Compose!

[![][docs-latest-img]][docs-latest-url][![][pkg-0.5-img]][pkg-0.5-url] [![][pkg-0.6-img]][pkg-0.6-url][![][travis-img]][travis-url]

Compose is a vector graphics library for Julia.
It forms the basis for the statistical graphics system
[Gadfly](https://github.com/GiovineItalia/Gadfly.jl).

## Synopsis

Unlike most vector graphics libraries, Compose is thoroughly declarative. Rather
than issue a sequence of drawing commands, graphics are formed by sticking
various things together and then letting the library figure out how to draw it.
The "things" in this case fall one of three types: Property, Form, and Canvas.
"Sticking together" is primary achieved with the `compose` function.

The semantics of composition are fairly simple, and once grasped provide a
consistent and powerful means of building vector graphics.

## Documentation

- [**LATEST**][docs-latest-url] &mdash; *in-development version of the documentation.*

[docs-latest-img]: https://img.shields.io/badge/docs-latest-blue.svg
[docs-latest-url]: https://giovineitalia.github.io/Compose.jl/latest

[pkg-0.5-img]: http://pkg.julialang.org/badges/Compose_0.5.svg
[pkg-0.5-url]: http://pkg.julialang.org/?pkg=Compose
[pkg-0.6-img]: http://pkg.julialang.org/badges/Compose_0.6.svg
[pkg-0.6-url]: http://pkg.julialang.org/?pkg=Compose

[travis-img]: http://img.shields.io/travis/GiovineItalia/Compose.jl.svg
[travis-url]: https://travis-ci.org/GiovineItalia/Compose.jl
