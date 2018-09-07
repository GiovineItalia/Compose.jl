# Compose!

[![][docs-latest-img]][docs-latest-url] [![][travis-img]][travis-url] [![][codecov-img]][codecov-url]

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

[travis-img]: http://img.shields.io/travis/GiovineItalia/Compose.jl.svg
[travis-url]: https://travis-ci.org/GiovineItalia/Compose.jl
[codecov-img]: https://codecov.io/gh/GiovineItalia/Compose.jl/branch/master/graph/badge.svg
[codecov-url]: https://codecov.io/gh/GiovineItalia/Compose.jl
