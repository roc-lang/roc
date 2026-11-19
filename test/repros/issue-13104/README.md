# Issue 13104: forward method lookup through an alias

Issue: https://github.com/roc-lang/roc/issues/13104

`test/cli/Issue13104ForwardAliasMethod.roc` is the issue's program: a method
calls another method of the same type through a type alias (`Counter.bump`)
before that method's definition. It checks cleanly and prints `2`.

A lookup through a local alias that selects a binding of this module is that
binding's local lookup (design.md "Associated Item Lookup Through Aliases"):
it contributes a name-reference edge to the check order, and it is checked by
the same status-aware rule as a reference by name.

`src/check/test/issue_13104_test.zig` covers both declaration orders, an
unannotated generic target, mutually recursive methods, and a value cycle
through an alias. The CLI suite runs the program under both lowering
strategies ("issue 13104" in `src/cli/test/parallel_cli_runner.zig`).

## Focused validation

Run from the repository root:

```sh
zig build run-test-zig-module-check -Dtest-filter='issue 13104' -- --test-filter 'issue 13104'
zig build roc
zig-out/bin/roc --no-cache test/cli/Issue13104ForwardAliasMethod.roc
```
