# Rules

Each rule is a judgment about one compiled program, checked by running the program on the model's interpreter
(`interp.py`). The interpreter is the model's statement of what a Monotype Lifted program means; [Semantics](#semantics)
lists the choices it makes where design.md does not say.

## SpecConstr

### S1. SpecConstr preserves meaning

For every program the compiler builds and every root of it, the program after SpecConstr (`after_spec_constr`) and the
program it was given (`after_lift`) end the same way on the same inputs: the same value, or a crash with the same message,
and the same effect trace (hosted calls with their arguments, `dbg` values, failed `expect`s), in order.

design.md "SpecConstr And Loop Scalarization" (SpecConstr is an optimization: workers, call-pattern specialization,
inlining, case-of-case, let-of-case and loop scalarization replace computations by equal ones) and "Monotype Lifted IR"
(join points and jumps express shared control only). Enforced by nothing in the compiler: no stage compares the two
programs. Code: `src/postcheck/monotype_lifted/spec_constr.zig`, run from `prepareMonotypeToSolved` in
`src/lir/checked_pipeline.zig`.

Checked by `check.py`. A root either program cannot run on the interpreter (an operation it does not model, a stored
static list, fuel) is counted as skipped, not compared.

### S2. SpecConstr keeps the roots

Both programs have the same roots, in the same order, naming the same lifted functions. design.md "Monotype Lifted IR"
(roots refer to lifted function ids; SpecConstr adds functions after them and rewrites bodies). Checked by `check.py`.

### S3. Programs are well formed when run

A run never reads a local nothing bound, never reaches an `unreachable` marker, never jumps to a join point that is not
in scope, never lets `break`, `continue`, a jump or a `return` escape its function, and never calls a function with the
wrong number of arguments. A violation in
`after_lift` is a lifting bug; one only in `after_spec_constr` is a SpecConstr bug. design.md "Monotype Lifted IR" (join
point scope, sequenced operands) and Monotype "Operand Sequencing". The compiler checks the sequenced form in Debug builds
(`sequenced.zig`) but not these dynamic properties.

## Lambda sets

### L1. Every closure that reaches a call is in its lambda set

When a `call_value` runs, the function of the closure its callee evaluates to is one of the members direct LIR lowering
dispatches over at that call (the lifted functions of the callee type's callable variants), unless the callee is erased.

design.md "Lambda Solved IR" ("Finite lambda sets name the exact function symbols that may flow through a function
value"), "Lambda Set Instances" and "Lambda Mono Decisions" (a finite callable set is a generated tag union with one tag
per member; a call reads the discriminant and switches over the variants). Enforced in the compiler only by the
construction of the solved types: `lambda_solved/solve.zig` unifies callable slots through value flow,
`lambda_solved/instances.zig` reads them under instances, and `solved_lir_lower.zig` `valueCallStep` dispatches over
`FnVariant`s. A missing member makes the compiled call run the wrong function or hit the empty-callable
`runtime_error`.

Checked by `lsets.py` against the `c` records of `ROC_LAMBDA_SET_EXPORT`. A call lowered under several specializations
is checked against the union of their variant lists, so a member missing from one specialization's set but present in
another's is not caught (see README "Limits").

### L2. Captures are bound

Every function a run calls through a closure or a direct call gets a value for each of its capture slots: the closure
or call supplies a capture operand whose `CaptureId` equals the slot local's. design.md "Monotype Lifted IR" ("each
lifted function definition declares its capture symbols explicitly"), "Lambda Solving" (own capture slots, matched to
operands by `CaptureId`) and "Lambda Set Generalization". Compiler: `monotype_lifted/lift.zig` (free-variable analysis),
`monotype_lifted/own_captures.zig`. Checked on every program `check.py` and `lsets.py` run, as a violation.

### L3. Every value call that runs was lowered

Reported as a count (`not lowered`), not as a finding: a value call the interpreter runs but lowering never lowered can
be legitimate when lowering inlines a body or folds a known callee into a direct call.

## End to end

### E1. The compiled test program agrees with the interpreter

For a `roc test` run whose test program the interpreter ran completely, the `[dbg]` lines the compiled program printed
are the `dbg` texts of the interpreter's run of the after-SpecConstr test program (as multisets: the runner may order
tests differently), and the number of failed tests is the number of roots that did not return `True` or failed an
`expect`. This observes every stage after SpecConstr at once (Lambda Solved, Lambda Mono, LIR, ARC, the backend), so a
lambda set missing a member that L1 cannot see (a call inlined away, a member missing in one instance) still shows here
as a wrong value or a crash. design.md "Lambda Mono Decisions" (a call of a finite callable dispatches on the variant
the value was built with). Checked by `drive.py`.

## Semantics

Where the interpreter makes a choice design.md leaves open, both programs of a pair are run the same way, so the choice
cannot cause an S1 divergence unless SpecConstr itself depends on it.

- Values are structural: records by label, tuples, tags by name, lists, boxes, closures (a lifted function plus capture
  values by `CaptureId`). Nominal, `typed_boundary` and `row_widen` are identity. `Bool` is a Python boolean, whether it
  arrives as `int_lit` at `Bool` or as a `True`/`False` tag.
- Integer `+`, `-`, `*` and negation crash on overflow at the expression's primitive type; the `_wrap` forms wrap.
  Division by zero crashes.
- A hosted function records its id and arguments in the trace and returns a fixed value of its result type (the `Ok`
  tag when there is one).
- `uninitialized` and `uninitialized_payload` produce a marker; `if_initialized_payload` takes its initialized branch when
  its payload local holds a value. The flag masks are read but not used.
- A loop body that ends without `break` or `continue` ends the loop with its value (Lambda Solved types the body at the
  loop's result).
- `try_sequence` on `Err` jumps to its error join with the payload when it has one, and otherwise produces `Err(payload)`.
- A shaped `call_proc` (`demand_shaped 1`) passes records as tuples in label order and converts a tuple result back.
- String patterns match the prefix, then each delimiter's first occurrence; `exact` requires the string to end there.
- `structural_hash` and hashers are modeled only far enough to be deterministic.
