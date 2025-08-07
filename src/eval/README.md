# Eval

Runtime evaluation and interpretation system for executing Roc programs during compilation.

## Validation and Testing

The primary method for validating eval behavior is through **REPL snapshots**. These are comprehensive integration tests that capture the complete evaluation pipeline from source code to final output.

#### Running REPL Snapshots

Run all REPL snapshots to check for any changes in expected output:
```bash
zig build snapshot
```

Run a specific REPL snapshot with trace evaluation for debugging:
```bash
zig build snapshot -- --trace-eval src/snapshots/repl/your_test.md
```

#### REPL Snapshot Format

REPL snapshots are markdown files with a specific structure:

```markdown
# META
~~~ini
description=your test description
type=repl
~~~
# SOURCE
~~~roc
» 1 + 1
» "Hello, World!"
» { foo: "bar" }.foo
~~~
# OUTPUT
2
---
"Hello, World!"
---
"bar"
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(e-binop @1.1-1.6 (op "add")
    (e-int @1.1-1.2 (value "1"))
    (e-int @1.5-1.6 (value "1")))
~~~
```

#### Creating New REPL Snapshots

1. Create a new `.md` file in `src/snapshots/repl/`
2. Add your test cases in the SOURCE section
3. Run `zig build snapshot` to generate expected outputs
4. Use `--trace-eval` for debugging specific scenarios
