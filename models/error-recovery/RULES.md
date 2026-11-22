# Rules

The oracle judges each mutant by the rules below. Line numbers are against upstream `main` at `abdd76afeb`.

## The contract

design.md "Every Rejection Is Explicit Recovery": `roc check`, `roc build` and `roc run` lower every checked program,
so post-check stages consume rejected code on every error path. A diagnostic is complete only when the rejected node is
explicit recovery data at the checked boundary: a checked runtime error, or a binder that binds nothing so every use of
it becomes one. Post-check stages never recover rejected code themselves; an invariant they hit on an error path is a
missing checker recovery. design.md "Checked Boundary" states what a checked artifact publishes to lowering.

## R1. No command crashes

Each of `check`, `test --opt=dev`, `test --opt=dev --specialize=yes`, `test --opt=speed`, `build --opt=dev`,
`build --opt=dev --specialize=yes` and `build --opt=speed` finishes without a panic, a signal or a timeout.

- Labels: `panic:<cmd>`, `signal:<cmd>`, `timeout:<cmd>`; `unknown:<cmd>` when the output has none of the summaries the
  oracle reads (a CLI change, or a crash with no message).
- Compiler side: the consumer-side asserts this rule turns into reports are
  `src/postcheck/monotype/lower.zig:12125` and `:28749` ("erroneous checked type reached Monotype lowering /
  instantiation"), `src/postcheck/boxy/types.zig:1661` ("an erroneous checked type reached Boxy lowering"), and the
  checked-artifact invariants (`checkedArtifactInvariant`, `CheckedModuleArtifact.verifyComplete` at
  `src/check/checked_artifact.zig:34964`). The producers are the recovery passes in `src/check/Check.zig`:
  `poisonErroneousValueUses` (43211), `valueIsErroneous` (43143) and `withdrawProblemsOfVoidRelations` (42842).
- On the `debug-check-error-fence` branch with `ROC_VERIFY_ERROR_FENCE=1`, `src/check/error_fence.zig` asserts the same
  contract at the seal: a violation becomes `panic:check` with "checked boundary invariant violated ... (error fence)".

## R2. Every command agrees that the program has errors

For every command that finished, "reported at least one error" is the same. `roc check` reports errors, `roc test`
prints `Compilation failed with N errors`, `roc build` prints `N errors and M warnings found`.

- Label: `disagree`.
- design.md: one checked program feeds every command ("Every Rejection Is Explicit Recovery"; the specialization and
  lowering mode is chosen after checking), so whether a program has errors cannot depend on the command or mode.
- Error counts are recorded in `results.tsv` but not compared (see the README's Limits).

## R3. An executable built from an erroneous program does not die of a signal

Each executable that a `build` produced is run. It may end normally or with a Roc runtime error at the rejected code.

- Label: `run-signal:<cmd>`, `run-timeout:<cmd>`.
- design.md: rejected code is a checked runtime error, which crashes through `roc_crashed`, never through a fault.

## R4. Generator soundness (not a compiler rule)

Every base program, before mutation, checks with no errors. A base program with errors is a generator bug, kept in
`out/fuzz/gen-errors/` and labeled `gen-error`.

## Not a failure

- `accepted`: `roc check` reports no errors for a mutant. A mutation can leave a program well typed (for example a
  changed body of an unannotated constant that only `Str.inspect` reads). If every command also accepts, R2 holds.
- Warnings, error counts that differ between commands, `expect` failures and Roc runtime errors.
