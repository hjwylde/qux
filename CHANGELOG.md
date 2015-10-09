## Changelog

#### Upcoming in master

* Updated usage text for `qux check`.
* Added `--entry` option to `qux run`.
* Added `--numeric-version` and `--qux-version` global options.
* Added ability to parse any valid Qux value as an argument to `qux run`.
* Added ability to build and check multiple files at once.
* Added program checking as default to `qux run` and option to skip it, `--skip-checks`.
* Added `compile` command to enable compilation to LLVM IR.
* Deprecated `qux run` (to be removed in v1.0.0.0).

#### v0.1.0.0

This is the first release of the `qux` binary!
It provides a basic ability to parse, print, type check and run Qux files by using the [Qux language
    library](https://github.com/qux-lang/language-qux).

