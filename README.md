# WebAssembly

[![Build Status](https://travis-ci.org/MikeInnes/WebAssembly.jl.svg?branch=master)](https://travis-ci.org/MikeInnes/WebAssembly.jl)

Tools for working with the [WebAssembly](http://webassembly.org/) format in Julia. In particular, this package exposes the IR as simple Julia data structures, allowing parsers and code generators to convert to/from the IR, as well as allowing optimisation passes directly on the IR in the vein of [binaryen](https://github.com/WebAssembly/binaryen).
