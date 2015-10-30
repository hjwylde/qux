# Changelog

### Upcoming

*Major*

* Added in support for import statements.
* Added in `dependencies` command that prints out a list of the file dependencies.
* Added in `--libpath` option on the `compile` and `build` commands for referencing extra libraries
  during compilation
* Added in `--verbose` and `--quiet` global options.
* Updated to use LLVM 3.5. ([#4](https://github.com/qux-lang/issues/4))

### v0.2.0.0

*Major*

* Added `--numeric-version` and `--qux-version` global options.
* Added ability to build and check multiple files at once.
* Added `compile` command to compile to LLVM IR.
* Removed `run` command.

*Revisions*

* Updated usage text for `check`.

### v0.1.0.0

This is the first release of the `qux` binary!
It provides a basic ability to parse, print, type check and run Qux files by using the [Qux language
    library](https://github.com/hjwylde/language-qux).
