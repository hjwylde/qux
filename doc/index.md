# index

### Qux

The `qux` binary is designed to be modular and solely for manipulating Qux files.
It is used in one part of the Qux build cycle&mdash;see the [meta](https://github.com/qux-lang/meta)
    build tool for how it can be used in conjunction with other build tools (e.g., `llc`, `gcc`, and
    `clang`).

There are 5 commands available:
* `build`           &mdash; the core command.
* `check`           &mdash; shortcut for `build --type-check`.
* `compile`         &mdash; shortcut for `build --compile --type-check`.
* `dependencies`    &mdash; prints out module dependencies.
* `print`           &mdash; pretty prints Qux files.

