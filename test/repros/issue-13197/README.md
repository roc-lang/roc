# Optimized roc-signals compiler crashes

Issue: https://github.com/roc-lang/roc/issues/13197

## Immutable integration reproduction

The input is `lukewilliamboswell/roc-signals` at
`c24cd112ac06a6d309464f55b924b5af537c1fc0`. It uses the published
roc-fuzz 0.6.2, roc-cbor 0.6.3, parser and Unicode dependencies. The original
source and dependency versions were not changed for the confirmed failures.

Prepare the external source in a directory outside `.zig-cache` (the periodic
cache cleanup deletes idle `.zig-cache` contents):

```sh
fixture=$(mktemp -d)/roc-signals
mkdir -p "$fixture"
curl -fLsS https://api.github.com/repos/lukewilliamboswell/roc-signals/tarball/c24cd112ac06a6d309464f55b924b5af537c1fc0 |
  tar -xz --strip-components=1 -C "$fixture"
```

Then run the integration driver (requires the compiler and
the external fixture, and package downloads on a cold package cache):

```sh
sh test/repros/issue-13197/reproduce.sh \
  zig-out/bin/roc "$fixture" "$(mktemp -d)"
```

Both builds must succeed; a failed command fails the driver. This driver
preserves the original integration cases separately from the focused unit
regression below.

## Confirmed observations

With a locally built `release-fast-5ff11589` compiler:

- `roc check --opt=speed` accepts startup-placeholders with no errors.
- `roc build --opt=speed --timings` on startup-placeholders exits 139 in
  `post-check lambda_solve`.
- The same optimized build of wire exits 139. One symbol-bearing run stops
  in `getStructFieldPositionByOriginalIndex` (`src/layout/store.zig:2823`),
  called through `runBoundary` and `assignTypedBoundary` in solved-to-LIR
  lowering. Another run stops in lambda solving during compile-time
  respecialization, with `referenceFnType` (`lambda_solved/solve.zig:976`)
  on the stack.
- Startup-placeholders builds with `--opt=dev --specialize=yes`.
- The no-cache startup build did not finish and was stopped. Cache
  independence is not established.

A Debug compiler hits an earlier assertion while checking the dependency's
`XmlNs` module: `storedValueVar` (`src/check/Check.zig:32468`) sees a
generalized, non-error variable not classified as a binding scheme. That is
not evidence that this assertion causes the optimized post-check crashes.
Apple LLDB crashes while opening the compiler binary, before running it;
symbol-bearing compiler signal reports supplied the stack traces above.

## Focused regression and root cause

```sh
zig build run-test-zig-module-postcheck -j2 -Dtest-filter='issue 13197' \
  -- --test-filter 'issue 13197'
```

The test `issue 13197: retained ownership follows a substitutable tuple
projection` in `src/postcheck/monotype_lifted/spec_constr.zig` constructs a
legal substitution of a string binding with a projection of a runtime tuple.
It first asserts that SpecConstr accepts that expression as substitutable,
then asserts that cloning the continuation's retained ownership keeps the
tuple's runtime owner. It fails with:

```text
postcheck invariant violated: SpecConstr retained ownership leaf was neither a runtime local nor a static value
```

Read-only diagnostic printing under the existing developer-only
`ROC_LIR_DUMP` gate identifies that same invariant in the original wire
input, before the misleading downstream failures. ReleaseFast treats the
violated invariant as unreachable, so later crash locations do not reliably
identify the offending compiler stage.

`exprIsSubstitutable` (`spec_constr.zig:826-849`) explicitly accepts
function references with substitutable captures, field accesses, and tuple
accesses, in addition to literals and local reads. `Value.expr` can retain
those exact expressions; it is not restricted to terminal leaves.
`cloneRetainedLocals` (`spec_constr.zig:15150`) sends those values to
`appendRetainedValueLocals`, but its expression arm calls
`appendRetainedExprLocal` (`spec_constr.zig:15201`), whose narrower rule
accepts only locals and literals. The ownership transformation violates
SpecConstr's own substitution contract.

## Recommended fix

Make retained-ownership cloning cover the full explicit substitutable-read
grammar. Traverse symbolic values and embedded expression reads together:
local reads contribute their typed runtime owner; field/tuple projections
contribute the receiver's runtime owners; function references contribute
their explicit capture operands' owners; closed static values contribute
none. Runtime anchors use their runtime expression, not their symbolic
structure, so initializer-private locals cannot escape.

Share the expression dependency classification with substitutability
checking rather than maintaining two independently enumerated grammars.
Use an iterative traversal over the reachable DAG, visit shared expressions
once, and keep the existing touched-local compaction. Work and scratch space
should scale with visited substitutions and actual retained owners, not the
program's maximum local id. Preserve the producer's initialization
conditions on retained owners. Do not evaluate projections, reconstruct
aggregates, allocate boxes, or add runtime work merely to build ownership
metadata.

This fix belongs in SpecConstr's production of the cloned continuation.
Dropping unsupported leaves would lose liveness/cleanup obligations;
repairing lambda-solver types or LIR layouts would address only symptoms.

## Startup-placeholders: a separate cause

Startup-placeholders fails a different invariant, in lambda solving rather
than SpecConstr: `a scheme node was not a class root` (`substituteContent`,
reached from `instantiate` in `lambda_solved/solve.zig`). The SpecConstr fix
above does not address it.

Temporary instrumentation (since removed) traced it deterministically:

- `setLink` is called on a scheme node of group 469, after that group was
  generalized. The caller is `expectExprSlot` (through `beginExpr` and
  `inferExpr`), while solving group 507 (function 594). The two expressions
  involved are plain `local` reads whose `local_tys` entry is already a
  group-469 scheme node. Neither group shares an expression.
- In the same program, many `LocalId`s are read by functions in different
  binding groups. For example, locals 127 to 131 are read by function 98
  (group 434) and function 428 (group 455).
- The link happens on every run. Whether a later instantiation then crashes
  varies: one of three identical runs exited 139, the other two built.

Mechanism: `solveBodies` gives each `LocalId` one type, created `unowned`
before any group is solved, so the first group to relate a local claims it.
When another group's function reads the same `LocalId`, its body unifies
with that type. After the first group is generalized, that type can be a
scheme node, and the unification breaks design.md's rule that "Scheme nodes
are never related again once the group finishes". `generalizeGroup` already
keeps a group monomorphic when it shares an *expression* with another
group's body (`BindingGroups.shares_exprs`). It has no corresponding guard
for shared *locals*.

A likely source of shared locals is SpecConstr's specializations. Their
reserved functions copy `.captures = source_fn.captures` (`reserveSpecIds`
and `ensureCallPatternForValues` in `spec_constr.zig`). Their bodies are
cloned from the source body, and `cloneLocalRef` leaves reads of
unsubstituted locals, so a specialization reads its source function's
capture `LocalId`s. Arguments get fresh locals, and callable workers already
give captures fresh locals (`worker_captures`). That locals 127 to 131 are
those captures is not yet confirmed.

### Recommended fix

Make every lifted `LocalId` belong to exactly one function: give each
SpecConstr specialization fresh capture locals, as callable workers already
do. Bind them at the clone's entry with the cloner's substitution, so body
reads are renamed. Caller capture operands are positional and stay
unchanged. Then state the invariant in design.md, which currently permits
specialized siblings to share source `LocalId`s, and add a debug-only
verifier when Lambda Solved starts, next to `verifyDistinctFnSymbols`.

The other option is to extend the monomorphism rule to groups that share a
local. It is also correct, but it gives up lambda-set polymorphism for every
specialized function that has captures. Every caller would then share one
lambda set, and dispatch would be wider at runtime. The cost of fresh locals
is one `Local` entry per capture per specialization.

The cold `roc build --opt=speed` of this input (including `--no-cache`)
did not leave type checking within 28 minutes. That is a separate
observation, not yet explained.
