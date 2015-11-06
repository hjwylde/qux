# qux

[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/1.0.0/active.svg)](http://www.repostatus.org/#active)
[![Build Status](https://travis-ci.org/qux-lang/qux.svg?branch=master)](https://travis-ci.org/qux-lang/qux)
[![Release](https://img.shields.io/github/release/qux-lang/qux.svg)](https://github.com/qux-lang/qux/releases)

Qux is an experimental language developed from the ground up with the aim of supporting extended
    static checks at compile time.

This project provides a binary for working with the Qux language.
It features:
* Compiling.
* Type checking.
* Pretty printing.
* Dependency printing.

For quick help on how to call `qux`, see `qux --help`.

### Installing

Installing `qux` is easiest done using either [stack](https://github.com/commercialhaskell/stack)
    (recommended) or [Cabal](https://github.com/haskell/cabal).

NB: LLVM (v3.5) is required on the system for compiling Qux files.

**Using stack**:

```bash
stack install qux
```

And make sure `~/.local/bin` is included on your `$PATH`.

**Using Cabal**:

```bash
cabal-install qux
```

And make sure `~/.cabal/bin` is included on your `$PATH`.

### Using

The `qux` binary is designed to be modular and solely for manipulating Qux files.
It is used in one part of the Qux build cycle&mdash;see the [meta](https://github.com/qux-lang/meta)
    build tool for how it can be used in conjunction with other compilers (e.g., `llc`, `gcc`, and
    `clang`).

There are 5 commands available:

`build`           &mdash; the core command.  
`check`           &mdash; shortcut for `build --type-check`.  
`compile`         &mdash; shortcut for `build --compile --type-check`.  
`dependencies`    &mdash; prints out module dependencies.  
`print`           &mdash; pretty prints Qux files.

