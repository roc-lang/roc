# Boxy compile-time evaluation model rules

Judgment: a value the compiler computes at compile time equals the value of the same expression computed at runtime.
The model is the program itself: every probe is one expression written twice, once where the compiler must evaluate
it while checking and once where no part of it can be, and the two must print the same thing in every configuration.
There is no reference evaluator, so the model cannot tell which side is wrong, only that they disagree.

## Model rules

| Rule | What it says | Design reference | Compiler site |
|---|---|---|---|
| CT-TOP | A top-level constant `c<i> = E` is evaluated at compile time, by the `.boxy` evaluator (or the checked-value writer, for a constant in value form), and its stored value is what every read of `c<i>` sees. | design.md "Compile-Time Evaluation And Static Storage", "Value-Form Roots"; docs/langref/compile-time.md | `src/eval/compile_time_finalization.zig` (`finalizeProgram`), `src/eval/const_store_writer.zig` (`storeRoot`, `storeCheckedRoot`) |
| CT-FN | A top-level constant whose value holds a function (a closure, or a record with a function field) is a compile-time root completed by the function-root evaluator of the build's strategy; applying it gives what applying the runtime closure gives. | "Values That Hold Functions", "Compile-Time Evaluation Strategy" | `src/eval/checked_root_graph.zig` (`rootHoldsFunction`), `src/postcheck/boxy/stored_fn.zig` |
| CT-GUARD | The same expression inside a branch on a runtime argument is a guarded hoisted root: evaluated at compile time like any root, and restored at runtime when the branch runs. Its value equals the top-level constant's. | design.md "Compile-Time Constants and Hoisted Roots" (the three positions in a runtime body) and "Body-Local Type Context" (guarded roots) | `src/check/Check.zig` (`exprHoistRoles`), `src/postcheck/boxy/body.zig` (guarded binding roots) |
| RT | An expression every leaf of which reads the runtime argument (`rt(z, leaf)`, an identity the compiler cannot see through while checking) has no compile-time root inside it, so it is computed by the runtime lowering. Its value equals the constant's. | "Root Selection During Checking" (runtime dependency is computed bottom-up from lambda arguments) | `src/check/Check.zig` (hoistability summary), `src/postcheck/boxy/lower.zig` |
| RESTORE | Reading a stored constant at runtime (`show_rt(z, c<i>)`, where `show_rt` inspects its argument behind a runtime branch) gives the value compile-time inspection of the same constant gives (`Str.inspect(c<i>)`, itself a hoisted root). | "Compile-Time Constants and Hoisted Roots" (lowering restores the checked value) | `src/postcheck/boxy/body.zig` (`restoreConstUse`), `src/postcheck/boxy/const_schema.zig` |
| IMPORT | A constant of an imported module is restored in the importer exactly as in its own module. Programs split their constants, helpers and nominal types into a type module `Lib`. | "Value-Form Roots" (an imported root from the schema's view of its module) | `src/postcheck/boxy/value_schema.zig`, `src/eval/compile_time_finalization.zig` |
| READ | A constant that reads an earlier constant (`c<j> = f(c<i>)`) has the value of the runtime expression that reads the earlier runtime value (`f(r<i>(z))`). | "Value-Form Roots" (readers complete after what they read), "Dependency-Ordered Compile-Time Evaluation" | `src/postcheck/comptime_root_graph.zig` (`build`), `src/eval/checked_root_graph.zig` |
| EXPECT | A top-level `expect c<i> == r<i>(0)` passes under `roc test`, with either `--specialize`. `r<i>(0)` is closed, so this compares the constant with a compile-time call of the runtime function. | "Test outcome files", "Demand-Driven Compile-Time Evaluation" | `src/eval/compile_time_finalization.zig` |
| AGREE | Every configuration (interpreter, dev in process, dev and LLVM executables, each with Boxy and with lambda-set specialization, and a cached rebuild) prints the same lines. | "Compile-Time Evaluation Strategy" (a checked module holds the same values whichever strategy completed it) | all of the above |
| REUSE | A rebuild that restores Boxy unit objects from the cache, under `ROC_VERIFY_BOXY_OBJECT_REUSE`, finds every restored object equal to a fresh build of it, and prints what the cold build printed. | design.md "Caching Under `.boxy`" (the `ROC_VERIFY_BOXY_OBJECT_REUSE` paragraphs) | `src/compile/boxy_interface.zig`, `src/compile/boxy_unit.zig` |
| TOTAL | The compiler does not panic, fail an assertion, or hang on any generated program, which `roc check` accepts. | AGENTS.md (compiler invariants are debug assertions) | everywhere |

## What makes the two sides comparable

- Every generated expression is total and has one value: integer arithmetic is `plus_wrap`, `minus_wrap`,
  `times_wrap`, `bitwise_and`, `bitwise_xor`, `shl_wrap` and `rem_by` a literal of at least 2; integer conversions
  are the lossless `to_<w>` or `to_<w>_wrap`; `Dec` only adds or subtracts a literal; float arithmetic may reach an
  infinity but has one IEEE result. Float-to-integer `_wrap` conversions are generated too; a disagreement there needs
  the language reference to say what the conversion of an out-of-range float is before it is a bug.
- Recursion counts are literals from 0 to 4, or up to 300 when the recursion step reads the accumulator once, so
  recursive helpers terminate and values stay linear in the count.
- Values are compared through `Str.inspect` at runtime, and through `==` at runtime when the type has structural
  equality (no float, no function, no nominal type).

## Checking that the generator reaches Boxy

`./run.sh reach SEED` runs `roc check` under `ROC_TRACE_BOXY_WORK` and `ROC_HOIST_ROOT_STATS` and lists the pending
compile-time roots. Every probe constant that is not in value form must be among them, and the runtime side of a
tainted probe must contribute none (the closed lambdas it creates are the only exception: a lambda with no captures
is a constant callable).
