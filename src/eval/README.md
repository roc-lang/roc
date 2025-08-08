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
description=your description here
type=repl
~~~
# SOURCE
~~~roc
» 1 + 1
» 0.1 + 0.2
» "Hello, World!"
» []
~~~
# OUTPUT
2
---
Evaluation error: error.LayoutError
---
"Hello, World!"
---
<list_of_zst>
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(e-binop @1.2-1.7 (op "add")
	(e-int @1.2-1.3 (value "1"))
	(e-int @1.6-1.7 (value "1")))
---
(e-binop @1.2-1.11 (op "add")
	(e-dec-small @1.2-1.5 (numerator "1") (denominator-power-of-ten "1") (value "0.1"))
	(e-dec-small @1.8-1.11 (numerator "2") (denominator-power-of-ten "1") (value "0.2")))
---
(e-string @1.2-1.17
	(e-literal @1.3-1.16 (string "Hello, World!")))
---
(e-empty_list @1.2-1.4)
~~~
# TYPES
~~~clojure
(expr @1.2-1.7 (type "Num(_size)"))
---
(expr @1.2-1.11 (type "Num(_size)"))
---
(expr @1.2-1.17 (type "Str"))
---
(expr @1.2-1.4 (type "List(_elem)"))
~~~
```

#### Creating New REPL Snapshots

1. Create a new `.md` file in `src/snapshots/repl/`
2. Add your test cases in the SOURCE section
3. Run `zig build snapshot` to generate expected outputs
4. Use `--trace-eval` for debugging specific scenarios
