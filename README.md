# qux

[![Project Status: WIP - Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip)
[![Release](https://img.shields.io/github/release/hjwylde/qux.svg)](https://github.com/hjwylde/qux/releases)

Qux is an experimental language developed from the ground up with the aim of supporting extended
    static checks at compile time.

This project provides a binary for working with the Qux language.
It features:
* Compiling.
* Type checking.
* Dependency printing.
* Pretty printing.

For quick help on how to call `qux`, see `qux --help`.

### Installing

Installing qux is easiest done using either
    [stack](https://github.com/commercialhaskell/stack) (recommended) or
    [Cabal](https://github.com/haskell/cabal).

NB: LLVM (v3.5) is required on the system for compiling Qux files.

**Using stack:**

```bash
stack install qux
export PATH=$PATH:~/.local/bin
```

**Using Cabal:**

```bash
cabal-install qux
export PATH=$PATH:~/.cabal/bin
```

### Usage

The `qux` binary is designed to be modular and solely for manipulating Qux files.
It compiles a qux file down to LLVM which may then be either interpreted or further compiled down to
    machine code (using either `lli` or `llc`).

There are 5 commands available:

`build`           &mdash; the core command.  
`check`           &mdash; shortcut for `build --type-check`.  
`compile`         &mdash; shortcut for `build --compile --type-check`.  
`dependencies`    &mdash; prints out module dependencies.  
`print`           &mdash; pretty prints Qux files.

**build**:

Usage: `qux build [-c|--compile] [-d|--destination DIR] [-f|--format FORMAT] [-l|--libpath PATH] [--type-check] -- FILES...`

Builds all of the given Qux files.
By default this command does nothing except check that the files are parsable (this includes name
    and type resolution).

The `--libpath` option takes a path separated list of directories (e.g., `lib/:...`) to search for
    libraries referenced by imports.
This is used in name resolution to fully qualify all types and functions.

Adding the `--type-check` option will add the type checking analysis step to ensure types are used
    correctly.
If an error is found, then it is reported to standard error and the build process stops.

The `--compile` option adds compiling the Qux files into LLVM assembly or bitcode (specified by the
    `--format` option).
The organisation and naming of the input files does not matter, however the output files will be
    written out to the destination directory in appropriate module folders.
E.g., `qux build -d bin/ src/qux/lang/io.qux` will write to `bin/qux/lang/io.bc`.

**dependencies**:

Usage: `qux dependencies FILES...`

Prints out all the module dependencies (imported modules) of `FILES`.
This isn't hugely useful on it's own, but once library packages are supported it will help in
    determining whether one is required or unused.

**print**:

Usage: `qux print [-l|--line-length LENGTH] [-m|--mode MODE] [-r|--ribbons-per-line RIBBONS] FILE`

Pretty prints the file to standard output.
The pretty printing uses the Haskell [pretty](https://hackage.haskell.org/package/pretty-1.1.3.2)
    library based off _The Design of a Pretty Printing Library_ by John Hughes.
If you're interested in how it works have a read of the
    [paper](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.38.8777).
