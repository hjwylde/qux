# qux

[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/1.0.0/active.svg)](http://www.repostatus.org/#active)
[![Release](https://img.shields.io/github/release/qux-lang/qux.svg)](https://github.com/qux-lang/qux/releases)

Qux is an experimental language developed from the ground up with the aim of supporting extended
    static checks at compile time.

This project provides a binary for working with the Qux language.
It features:
* Type checking.
* Pretty printing.
* Compiling.
* Running (DEPRECATED).

For help on how to call `qux`, see `qux --help`.

### Installing

Installing `qux` is easiest done using either [stack](https://github.com/commercialhaskell/stack)
    (recommended) or [Cabal](https://github.com/haskell/cabal).

NB: LLVM (v3.4) is required on the system for compiling Qux files.

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

