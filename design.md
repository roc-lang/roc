# Roc Post-Check Compiler Design

This document is the authoritative design for the compiler stages that run after
checking has completed and before backend code generation begins.

The design is deliberately narrow. It keeps the existing checked boundary:

- checked CIR
- the checked type store
- checked module caching
- all user-facing checking and static-dispatch reporting

It also keeps the existing LIR boundary:

- statement-only LIR
- explicit LIR ARC insertion
- backend, interpreter, LirImage, and glue consumers of LIR

Keeping the LIR boundary means preserving the public LIR contract, not keeping
dependencies on a second post-check representation. Hosted ABI metadata,
platform metadata, symbols, literal ids, layout stores, and LIR stores live in
neutral modules owned by their actual consumers.

Everything in between those boundaries is a Cor-style typed IR pipeline:

```text
checked modules
  -> Monotype IR
  -> Monotype Lifted IR
  -> Lambda Solved IR
  -> Lambda Mono decisions
  -> LIR
  -> ARC insertion
  -> backend, interpreter, or LirImage
```

There is no separate MIR layer. There is no separate stored layout IR between
Lambda Mono and LIR. Layout selection is owned by the direct Lambda Mono to LIR
builder. In optimized builds, Lambda Mono is represented by explicit callable
and procedure decision tables consumed by direct LIR lowering, not by a second
stored expression, pattern, and statement tree.

## Core Principles

Compiler stages after parsing and error reporting must not use workarounds,
fallbacks, heuristics, or best-effort reconstruction.

Every stage after checking consumes explicit data produced by earlier stages.
If a stage needs checked data that was not produced, the producer is
incomplete. A consumer must not recover missing data from source syntax, names,
body scans, display strings, runtime bytes, object symbols, backend state, or
incidental data structure shape.

All user-facing failures are reported during checking at the latest. Checking is
not complete until type checking, static-dispatch finalization, platform/app
relation output, compile-time constant evaluation, and checked module
output have all completed. After a checked module is output, every
violated assumption is a compiler bug:

```text
debug build: debug-only assertion
release build: unreachable
```

Post-check stages do not return user-facing checking errors. They do not emit
fallback code. They do not silently repair missing data. They do not add
release-build runtime checks for compiler invariants.

Checked identity and runtime encoding are separate data. A stable id, checked
id, symbol, type variable, procedure reference, callable member, or source row
id may identify what a value means. It must not also be treated as the integer
tag, variant slot, byte offset, ABI register class, object symbol, or memory
layout used to represent that value. The stage that commits a runtime encoding
outputs the explicit mapping from checked ids to runtime encodings, and all
later stages consume that mapping.

Backends do not reason about reference counting. They lower and execute the
explicit LIR `incref`, `decref`, and `free` statements emitted before backend
code generation. Each explicit RC statement carries the concrete RC helper
selected by LIR ARC insertion. Consumers may lazily cache code or interpreter
execution plans for that helper, but they must not select a different helper
from local layout data. Reference-counting policy belongs to LIR ARC insertion.

## Checking Effects And Const Roots

Checking owns Roc effect validation, compile-time evaluation eligibility, and
compile-time root selection. These are checked-stage responsibilities, not
post-check repairs. The checker must finish with explicit outputs for function
effect kinds, top-level effect errors, effectful `expect` errors, compile-time
diagnostics, and selected compile-time roots. Later stages consume those
outputs directly.

The pre-check CIR producer outputs CIR and checked identity inputs; it does not
own compile-time root selection. Root selection happens during checking because
checking already walks every expression, resolves local identity, computes
types, validates function effects, and receives static-dispatch results. The
checker must not perform a later whole-module expression walk merely to decide
which expressions are roots, and later stages must not recreate those answers.

The question "can this expression be evaluated at compile time?" depends only
on checked data dependency, checked control reachability, and effectfulness. It
does not depend on whether a call was direct or static-dispatch syntax, whether
an expression is a leaf, whether a value was written inline or named at the top
level, or whether the expression contains `crash`, `dbg`, or `expect`. Those
constructs are compile-time observable, and evaluating them at compile time is
required when the surrounding expression has no runtime data dependency, no
runtime control dependency, and no effectful call.

Control reachability is checked data, not source-shape guessing. An expression
can be a standalone compile-time root only when the source meaning evaluates it
unconditionally whenever the root is needed. Branch bodies, match
guards, and match branch values are control-dependent on the enclosing
conditional or match. They may contribute summaries to an enclosing `if` or
`match` root, but they must not add independent selected roots while their
enclosing control decision can be made at runtime. Otherwise an untaken branch
containing `crash`, `dbg`, or `expect` would run during `roc check`, which
would change the program's observable behavior. If the whole enclosing control
expression is compile-time-known and effect-free, the enclosing expression may
be selected as the root and the evaluator follows the same branch choices as
the source program.

Non-local control-transfer expressions such as `return` and `break` are not
standalone value roots and cannot cover child candidates by themselves. Their
payloads may still contribute to an enclosing eligible root or be selected as
ordinary child expressions when they are reached through checked control data.
Making the control-transfer expression itself a root would require an explicit
checked continuation representation; until that exists, selecting it as a
stored constant is a compiler bug, not an optimization choice.

An effectful call is one of:

- a direct call to a checked effectful function
- a call through a function-typed value whose checked function type is effectful
- a static-dispatch call whose selected implementation is checked effectful

Creating a function value is not an effectful call, even when that function's
body is effectful. The effect propagates only when the function value is called.
Negative effect answers are not durable until the relevant slot has finalized;
static dispatch can still turn an apparently pure call site into an effectful
call before checked output is produced.

### Effect Slots

Roc effect propagation is a directed dataflow problem over function bodies and
call sites. It must not be represented by one early boolean that is finalized
before static dispatch has resolved. The checker maintains sparse effect slots
for the places where effectfulness is part of the checked result:

- function and lambda bodies
- top-level value right-hand sides
- `expect` bodies
- compile-time root candidates whose effectfulness may depend on delayed
  dispatch

An effect slot becomes effectful when it contains a direct call to an effectful
function, when a delayed static-dispatch call watched by the slot resolves to an
effectful function, or when it calls another slot that is effectful. Ordinary
calls add directed dependencies from caller slot to callee slot. Static-dispatch
calls add watcher entries from the dispatch function variable to the active
slot. When static-dispatch resolution later proves the selected method is
effectful, the watcher marks or connects the owning slot before any checked
output is finalized.

Effect dependencies are directed. A caller depending on a callee must not be
represented as equality. Strongly connected recursive groups may be condensed
for solving, but unrelated caller and callee slots must remain one-way
dependencies.

Effects are not inferred from source spelling alone. A `!` name contributes to
identifier parsing and annotations, but the checked source of truth is the
resolved function type and dispatch result. A method call whose syntax appears
inside a pure-looking expression can still make that expression effectful after
dispatch resolution. Conversely, `crash`, `dbg`, and `expect` are not real
effectful calls. They must never be used as reasons to reject a compile-time
root.

Effect finalization runs after ordinary type constraints, literal defaulting,
and static-dispatch constraints for the relevant boundary have settled. It
computes final slot effectfulness with directed graph propagation. After
finalization, the checker uses the slot results to select `fn_pure`,
`fn_effectful`, or the equivalent checked function kind, to report invalid pure
annotations, to report effectful top-level values, and to report effectful
`expect` bodies. Checked module output must not contain unresolved effect
kinds.

The effect solver may cache positive effectfulness immediately. It must not
treat an unresolved negative answer as final while dispatch watchers or callee
slots can still change. Recursive groups are solved by directed propagation:
strongly connected groups can be condensed, marked effectful if any member is
effectful, and then propagated to callers. Ordinary caller-to-callee edges must
remain one-way.

Every static-dispatch call that is not resolved when the expression frame
finishes is represented by explicit checked state. The active effect slot owns
the watcher for that dispatch function variable. If the same expression is also
a compile-time root candidate, the root candidate records that it is waiting on
the same slot. When the dispatch result arrives, effect finalization updates the
slot once, and both the function-effect answer and the root-selection answer
consume that finalized slot. Root selection must not infer delayed dispatch by
re-reading syntax or by searching for unresolved method names.

### Root Selection During Checking

Compile-time root selection uses the same checker traversal that already walks
checked CIR expressions. There is no separate root-selection walk over every
expression. While checking an expression, the checker returns a small
transient summary to its parent:

```text
runtime dependency status
control reachability status
effect slot or delayed-effect status when needed
candidate stack interval owned by this expression frame
```

The summary is stack-local for ordinary nested expressions. The checker stores
only data needed after the current expression finishes: summaries for bindings
that later lookups may read, effect slots and dispatch watchers, tentative root
candidates, and final selected roots. It must not allocate a permanent
per-expression table merely to answer root eligibility.

Runtime dependency is computed bottom-up from checked CIR identity. Lambda
arguments, match-bound values, loop-bound values, mutable variables, and
reassignments are runtime-dependent. Immutable local definitions store the
summary of their right-hand side; later local lookups consume that stored
summary. Top-level checked values and imported checked values are checked
binding identities at the use site. Looking up a module-level binding is
compile-time-known as a reference to that checked binding; the initializer's
own evaluation, diagnostics, reachability, and static storage are handled by
the module-level checked outputs, not by replaying the initializer summary into
each lookup expression. Parent expressions combine child summaries directly.

The expression summary is not a second effect system. Effect slots remain the
owner of effectfulness. The expression summary only says whether the expression
is already known effect-free, already known effectful, or waiting on an effect
slot that can still be marked by delayed dispatch or callee propagation.

An expression that already produced a checking problem is poisoned for
compile-time root selection. Poison is not runtime dependency and it is not an
effect. It only prevents an erroneous parent value from becoming a selected
root while preserving the original diagnostic ownership, so a bad child reports
once instead of being hidden by hoisting or reported again by a parent root.
Static-dispatch failures, type errors, and other checker-owned problems must
feed this poison result explicitly through the same expression summary path.
Poison is local to the expression or dependency region that owns the checking
problem. It propagates only through explicit checked dependencies, such as a
lookup of an erroneous local or top-level value. It must never become a module,
package, or program flag. A checked module or checked program may contain
user-facing diagnostics and still produce hoisted roots for every independent
expression whose own dependency region is resolved and otherwise eligible. This
is required for Roc's recover-and-continue behavior: `roc check`, tests, and
program execution must keep doing all valid work that does not depend on the
erroneous code path.
No downstream compile-time-evaluation step may use a CheckedModule's
nonempty diagnostic list as a reason to skip independent roots; it must consume
the explicit root list and the per-root poisoned/dependency state produced by
checking.
Compiler implementation gaps are not poison. Once checking has accepted an
eligible expression, failure to evaluate, store, restore, or emit it correctly
is a compiler bug with a regression test, not a reason to demote the expression
from compile-time evaluation.

Root selection keeps maximal eligible expressions. Each expression frame
records the root-candidate stack length at entry. If the expression finishes as
compile-time-known, unconditionally reachable, and effect-free, it removes
child candidates added inside the frame and adds itself. If the expression is
not eligible because of runtime data dependency or effectfulness, its eligible
unconditionally reached child candidates remain. If the expression is not
eligible because it is control-dependent on a runtime branch or match decision,
children inside that conditional region do not add standalone selected roots;
they are evaluated only if an enclosing eligible control expression is
selected. If the expression has delayed effect sources, the checker stores a
tentative parent over its child candidates; effect finalization later keeps the
parent and drops the children when the parent resolves effect-free, or drops
the parent and keeps the children when the parent resolves effectful. This is
the only parent-child replacement rule. There are no special cases for leaves,
strings, numbers, empty lists, records, loops, or other data-expression shapes.
Control-transfer expressions and conditionally evaluated branch regions are
handled by explicit checked control reachability, not by pruning arbitrary
source shapes.

Delayed parents form intervals over the candidate stack, not source-tree
queries. Nested delayed parents finalize from explicit interval ownership: when
an outer delayed parent is kept, every child candidate in its interval is
removed, including delayed children. When an outer delayed parent is discarded,
the candidates in its interval keep their own finalized results. This preserves
the maximal-root rule without a second walk and without special pruning rules.

Root selection must be independent of how the source was arranged. A named
top-level value, a closed immutable local value, and an equivalent inline
expression must produce equivalent selected roots once checked dependencies,
checked control reachability, and effects are the same. Selecting a parent root
is the only reason to discard an already selected child root from an
unconditionally evaluated region; rejecting a parent for runtime data
dependency or effectfulness must preserve those eligible children. A
runtime-controlled branch body is different: its contents are not
unconditionally evaluated, so they cannot be selected independently without
explicit checked proof that doing so preserves compile-time observables.

### Checker Implementation Contract

The checker has one authoritative state for effect propagation and compile-time
root selection. This state is owned by checking, updated during the existing
`checkExpr` traversal, finalized before checked module output, and exported as
explicit checked data. Canonicalization may produce stable identities and source
structure, but it must not select compile-time roots or decide final
effectfulness. Post-check stages may consume checked roots and evaluated
constants, but they must not repair or reinterpret root eligibility.

Checking a module-level definition as a dependency is not a child expression of
the lookup that forced it. If a forward reference causes a different
module-level definition to be checked while an expression frame is active, the
checker detaches the root-frame and candidate stacks for that definition. The
definition still writes to the module's shared selected-root, delayed-root,
known-binding, effect-slot, and checked-output state, but its transient
expression frames must not bubble runtime dependency, child candidates, or
last-expression metadata into the forcing lookup. This keeps the result
independent of whether an equivalent top-level constant was checked before or
after the use site.

Each expression frame records the current root-candidate stack length when the
frame begins. The frame receives child expression summaries as checking
progresses and returns one transient summary to its parent. The summary records
only the data needed by the parent: runtime data dependency, checked control
reachability, and effect state. Ordinary summaries are stack-local. A summary is
stored past the current expression only when a later checked local lookup needs
it, when an effect slot or dispatch watcher must finalize later, or when a
tentative root candidate has been selected.

Effect propagation uses directed slots and edges:

- checking a function body, top-level value, `expect` body, or delayed root
  candidate creates an effect slot when that boundary needs a checked effect
  answer
- a direct call to a known effectful function marks the active slot effectful
- a direct call to a local function with its own slot adds a caller-to-callee
  edge
- a call through a function-typed value consumes the checked function effect
  kind
- an unresolved static-dispatch call records a watcher from the dispatch
  variable to the active slot
- dispatch resolution updates the watched slot from the selected checked method
  effect

Those edges are dependencies, not equality. Recursive groups may be condensed
while solving, but unrelated caller and callee slots must remain one-way. A
slot whose callees and dispatch watchers have not finalized cannot be reported
as definitely pure. Finalization runs after the relevant ordinary type,
literal-defaulting, and static-dispatch constraints have settled; checked
module output must not contain unresolved effect slots or unresolved root
candidates.

Root selection uses the same expression frames. When a frame finishes as
compile-time-known, unconditionally reached, and effect-free, it replaces the
candidate interval added by its children with the parent candidate. When a
frame finishes as runtime-dependent or effectful, it leaves eligible
unconditionally reached children in place. When a frame is in a branch body,
match guard, or match branch value controlled by a runtime decision, it does
not add selected roots from that conditional region. The enclosing
`if` or `match` may still be selected if the whole control expression becomes
compile-time-known and effect-free.

Delayed root candidates are tied to effect slots. The candidate stores the
owned interval of child candidates and is finalized from the slot result. If
the slot resolves effect-free, the parent candidate is kept and the child
interval is removed. If the slot resolves effectful, the parent candidate is
discarded and finalized children remain. This interval rule is the only
subsumption rule; implementation must not add leaf filters, observable-effect
filters, or source-shape pruning.

Compile-time observables are not effect blockers. `crash`, `dbg`, and `expect`
must be represented as ordinary checked expressions for root selection. When
their enclosing selected root is evaluated during checking, they run and report
their diagnostics. An untaken runtime-controlled branch containing those
constructs must not be independently selected, because that would change source
behavior by running compile-time observables that the program would not
evaluate.

### Compile-Time Evaluation And Static Storage

Compile-time evaluation must evaluate every checked top-level expression and
every selected compile-time root that can be evaluated without effectful calls
or runtime data. It must run `crash`, `dbg`, and `expect` during that
evaluation and output their diagnostics during `roc check`.

Evaluation and static storage are separate checked outputs. Unreachable
top-level values are still evaluated when eligible so their `crash`, `dbg`, and
`expect` behavior is reported, but successfully evaluated unreachable data does
not need to be stored in checked module data or target static data. Reachable
evaluated values that have a static representation should be stored once and
shared. Records that contain static lists should point at shared static list
bytes; equivalent named and inline constants should produce equivalent static
data.

Compile-time evaluation is allowed to fail with user diagnostics only during
checking. After checking, stored constant data is ordinary checked output. A
target static-data builder may decide which reachable evaluated values have a
target representation, but it consumes the checked roots and evaluated values
directly; it must not scan checked CIR or generated code to rediscover root
eligibility.

If a reachable evaluated value cannot yet be represented as target static data,
the missing representation is a compiler bug. The checked output or static-data
builder must make the missing explicit data assertable and testable; it must
not silently demote the value from compile-time evaluation. No backend may
rediscover or guess root eligibility by scanning source syntax, function
bodies, object symbols, or generated code.

## Backend Builtins

Backend builtin linking is part of backend code generation, not a later repair
step. Each backend consumes explicit builtin call symbols emitted from LIR and
uses the representation that matches that backend.

The dev object backend emits native object code directly. Its builtin calls are
ordinary object-symbol references resolved by linking the target's
`roc_builtins.o`. The dev backend keeps using target-specific builtin object
files because it does not produce LLVM bitcode.

The LLVM backend emits application LLVM bitcode. LLVM builds must not link
`roc_builtins.o`. Instead, the compiler selects builtin LLVM bitcode by the
target pointer width, links that builtin module with the application module
before LLVM optimization, and emits the object file from the merged module.
Roc supports only 32-bit and 64-bit target pointers here, so two builtin
bitcode payload families are sufficient: one for 32-bit targets and one for
64-bit targets. Each pointer-width family has a core payload for common
string/list/refcount/debug roots plus lightweight integer parse/format roots,
and a full payload for decimal, float parsing/formatting, wide-integer, and
other heavier roots. The LLVM backend selects the core payload only when every
explicit builtin declaration in the app module is in the core root set;
otherwise it selects the full payload. These pointer-width
payloads must contain Roc builtin definitions only; they must not bundle
compiler-rt or other target-specific runtime code, because that would make the
payload architecture-specific again. The payloads are built as freestanding
LLVM bitcode so compile-time OS and CPU branches cannot bake a native
platform's syscalls, inline assembly, or runtime support into a module that will
later be retargeted. LLVM object emission for targets that are not required to
link a platform C runtime disables target-library assumptions. Targets that
also lack native memory operations lower LLVM memory intrinsics to explicit
loops before target code generation. macOS and Windows keep target library calls
available because their final links include the platform runtime libraries.

Builtin definitions in the merged LLVM module are real definitions. They must
not be marked `available_externally`, because there is no later builtin object
file to provide non-inlined calls. After builtin call symbols are resolved,
builtin aliases and definitions that are not application exports may be made
internal so LLVM dead-code elimination and the final linker can remove unused
builtin code. Before merging, the LLVM backend roots builtin exports at the
explicit builtin declarations emitted by the application module, internalizes
all other builtin definitions, and runs LLVM global dead-code elimination on
the builtin module. After merging, it resolves builtin aliases to their
concrete definitions, internalizes the merged builtin definitions that are not
application exports, and runs LLVM global dead-code elimination again before
object emission. Pre-merge elimination keeps unused builtin IR out of the
expensive optimization and code-generation pipeline. Post-merge elimination
cleans up definitions and aliases whose final reachability is only visible
after app calls have been resolved. Both passes preserve real definitions for
builtin calls that the application can inline.
LLVM object emission must request function and data sections, and the final
target linker must use section garbage collection where the target format
supports it.

Static ownership reasoning lives in exactly one place: LIR ARC insertion.
ARC insertion computes a whole-program borrows-with-lifetimes solution and
emits explicit RC statements from it (see ARC Borrow Inference). No other
stage — checking, post-check lowering, backends, the interpreter, or LirImage —
contains a borrow, lifetime, uniqueness, or parameter-mode model. Everything
the solver computes is ARC-stage-local data; none of it appears in checked
modules, LirImage, or any consumer-visible structure. Runtime mutation still
uses `refcount == 1` to decide whether in-place mutation is allowed.

Roc functions have fixed arity. Roc functions are not automatically curried.
The compiler must not synthesize partial-application closures, curried call
chains, or missing-argument wrappers unless Roc source explicitly constructs a
function value that returns another function.

`Bool` is an ordinary nominal tag union at runtime. Lowering and backends must
not special-case Bool runtime representation. Internal scalar predicate values
may exist only as control-flow implementation details; when a Roc value is
stored, returned, boxed, passed, exported, or refcounted, Bool uses the ordinary
tag-union representation selected by layout lowering.

Implementation data must never use sentinel/default values that can be mistaken
for real results. If a value is valid only after a producer writes it, storage is
initialized to `undefined` or guarded by explicit presence/state metadata. Every
consumer must prove the producer ran before reading the value. A crash or
invariant violation must be reported as the actual crash or invariant violation;
it must never be disguised by reading a convenience default.

This rule applies everywhere: checked data, lowering data, backend data, cache
data, ARC data, LirImage data, and test reporting. If code uses a sentinel
as a placeholder for data that must be produced, stop and redesign the producer
ownership and presence model.

## Source Parsing Boundary

Parsing is a token-first stage. Tokenization produces the only cursor input for
the parser, and the parser walks that token buffer directly. The parser does not
use recursive grammar functions, and it does not keep source substrings as an
implicit parsing cursor. Source text may be consulted only through token
metadata, for diagnostics, literal decoding, and identifier interning.

The parser is a direct token-dispatch machine. Hot parser code is organized as
grammar kernels that walk the token buffer with local token dispatch and ordinary
lexical control flow. The hot path must not route grammar progress through a
central parser-state interpreter loop, even when the cases contain token
switches, because optimized code can lower that transition pattern to a central
indirect branch.

This mirrors simdjson stage 2 more closely than a generic labeled-state switch.
simdjson's stage-2 parser walks a precomputed structural stream with concrete
JSON grammar labels such as object-begin, object-continue, array-value, and
scope-end. Its depth stack stores only open JSON scope fields (`is_array`,
tape index, element count); it does not store "run this parser state next"
instructions. Roc parser kernels must follow the same split:
tokenization performs linear input discovery, parser kernels inspect the
current token directly, and parser-owned syntax state describes currently open
Roc syntax rather than queued control flow.

Zig has no arbitrary sibling `goto`, so Roc cannot literally copy simdjson's C++
label layout. The Zig equivalent is lexical grammar loops with local token
switches, explicit syntax-depth state for nested constructs, and direct
fallthrough/`continue`/`break` transitions inside the kernel. Where a grammar
transition cannot be expressed lexically without a generic context switch, the
hot alternatives are to duplicate a small token-dispatch block or to split out a
specialized grammar kernel whose body remains stack-safe and assembly-audited.
Using a wide parser-context switch is not accepted for hot expression, pattern,
statement, or type parsing unless ReleaseFast assembly proves that exact slice
has no central indirect branch and is faster than the lexical-kernel shape.

Parsing chunks are not considered structurally done until ReleaseFast assembly
has been checked for this shape: no recursive parser calls for the converted
grammar, no instruction-driver loop, no broad parser-context dispatch ladder,
and no unexpected indirect branch in the hot transition path. The expression
prefix/suffix/binary-operator kernel is the first required audit target because
it is the parse-heavy hot path.

Parsing conversion proceeds by grammar slices that can be assembly-audited.
Before expanding a slice, build a tiny Zig proof of the intended dispatch shape
and compare it with the analogous simdjson stage-2 parser shape: local token
tests, direct branches between parser states, and explicit syntax state only
where nested syntax requires it. After converting the real Roc slice, build the
ReleaseFast compiler with symbols and disassemble the converted parser symbol
directly, for example:

```sh
zig build roc -Doptimize=ReleaseFast -Dstrip=false
xcrun llvm-objdump --macho --disassemble --dis-symname <source-parsing-symbol> zig-out/bin/roc
```

The audit result must be recorded before moving to the next slice. If the
assembly shows a dense jump table, generic context dispatch loop, indirect
branch in the hot parser transition path, or revived recursive grammar call,
the slice is not accepted and must be reshaped before more grammar is converted.

Current parser audit result:

```text
commit: 27165e02fd Fix pattern root parser instantiation
binary: zig-out/bin/roc
version: release-fast-27165e02
build: zig build roc -Doptimize=ReleaseFast -Dstrip=false --summary all --color off
```

The parser entry wrappers for expression, statement, pattern, type annotation,
and associated statement blocks all enter `runExprStatementKernel` with an
explicit root mode. They are API wrappers, not separate recursive grammar
kernels. Static source and symbol checks must not find `OpenSyntaxKind`,
`ParserContext`, `TypeOpenSyntaxStack`, `runTypeAnnoDirect`,
`parseWhereClauseTokens`, or `parseWhereConstraintTokens`.

The current ReleaseFast audit disassembled these parser kernel instantiations:

```text
_Parser.runExprStatementKernel__anon_169991
_Parser.runExprStatementKernel__anon_175153
_Parser.runExprStatementKernel__anon_175404
```

Searching those disassemblies for indirect branch-table dispatch found no
`br xN` instructions. Remaining indirect instructions were `blr x8` allocator
calls in growth/copy paths, not parser-state transitions. This is the accepted
assembly shape for the current unified parser slice.

Nested Roc syntax uses explicit open-syntax state, like simdjson's open
container depth. This state records concrete syntax currently being parsed:
open lists, records, strings, blocks, matches, type applications, and similar
constructs. It is not a parser instruction stream and must not store "execute
this parser operation next" entries. When a syntactic construct closes, the
parser inspects the parent open syntax and branches directly to that parent's
lexical continuation inside the current grammar kernel, or returns a completed
result to the caller when the kernel's root syntax closes.

Open-syntax state is stored compactly. The hot state records syntax kind and
indexes into syntax-specific side storage when payload is unavoidable. The
parser must not store wide tagged unions as call records for grammar work, and
must not push generic parser instructions just to decide what token to inspect
next. Leaf token cases that do not open nested syntax must not push state.

The parser owns a small set of result registers. Expression, pattern, type,
statement, associated-item, header, collection, and token-span results are
written to registers as syntax closes. The parent open syntax documents which
register it consumes. Closing nested syntax means jumping to the parent's token
branch, not returning through a Zig call stack and not interpreting a queued
parser action. Leaf helpers may exist for non-grammar work such as token
inspection, literal decoding, declaration indexing, scratch-span construction,
and diagnostic output, but they must not parse nested Roc grammar by calling
another grammar entrypoint.

`NodeStore` is the parser's output builder. The parser may accumulate children
in parser-owned scratch spans while a syntactic collection is open, then commit
the final AST node when its closing token is consumed or when parser recovery
emits a malformed node. Declaration indexing is updated from committed
statements and headers as part of this same iterative walk, so later compiler
stages consume explicit parser output rather than inspecting source syntax.

Error recovery is part of parsing and error reporting. Recovery states are also
iterative token states: they advance to a known delimiter, line boundary, or
collection close token and then jump to the next documented open-syntax branch.
Recovery may use parser-local heuristics because parsing and error reporting are
the only compiler stages allowed to do so. Recovery must still output explicit
malformed AST nodes and diagnostics; later stages must not recover missing
syntax on their own.

The parser implementation must not keep the old recursive-descent or
per-subgrammar instruction-interpreter architecture. Old expression, pattern,
statement, block, and type-annotation parser entrypoints are forbidden
implementation details. Public package functions may continue to expose parsing
capabilities such as parsing a whole file, header, expression, or statement, but
inside the parser they must enter direct token dispatch with an explicit goal
context. Static verification for this invariant is part of parser work:
searches for the old architecture names and recursive parser entrypoint names
must come back empty before Zig is run.

Post-check names should be short and precise. Do not encode whole explanations
into long compound type or function names. Prefer a small local vocabulary such
as `FnSet`, `FnVariant`, `FnTemplate`, and `CaptureSlot`, then define the exact
meaning in the surrounding design or module comments. Longer names are reserved
for cases where two nearby concepts would otherwise be genuinely ambiguous.
Avoid vague compiler jargon when a plain name is available. The words `bridge`
and `projection` are banned in new post-check docs, APIs, modules, type names,
variable names, and comments. They may appear only in this ban. Say the
specific operation instead, such as conversion, field read, tag payload read,
capture slot, or wrapper function.

The terms `readback`, `reification`, `value graph`,
`compile-time value store`, and `representation repair` are also banned in new
post-check docs and code. Compile-time evaluation stores results in
`ConstStore`; later compilations restore cached consts from `ConstStore`.
There is no separate phase or store under the const-related names. If two
representations unexpectedly differ for the same value at the same boundary,
that is an invariant violation; fix the producer instead of adding a later
conversion.

Outside the existing Canonicalization phase, the word `canonical` is banned in
new post-check docs and code. Use the exact term instead: `authoritative` for source-of-truth documents,
`lexicographic order by name` for sorted rows, `payload position order` for tag
payloads, `TypeDef`/`NamedType` for named type definitions, and `TypeDigest` for
structural checked-type digests.

The suffixes `Key` and `Ref` are banned in new type names. Use `Id` for assigned
identities, `Digest` for structural hashes, and a concrete domain noun for
compound identities such as `TypeDef`, `FnDef`, `ProcTemplate`, or
`CheckedModuleId`.

The term `runtime image` is banned in new post-check docs and code. Use
`LirImage` for the contiguous, viewable ARC-inserted LIR image plus layout store
and entrypoint tables.

The word `publish` and vague data-owner terms are banned in new post-check docs
and code, including their common variants. Use `output` for phase output, or
use the exact owner/data name.

The word `physical` is banned in new post-check docs and code. Use `layout`
only for memory shape data such as size, alignment, field offsets, and payload
layout. Use `runtime encoding` for the broader category that includes layouts,
discriminants, callable variant encodings, erased callable code entries, ABI
shape, and runtime schemas.

Vague owner terms are banned in new post-check docs and code. Use the precise
owner instead: `CheckedModule`, `CheckedModuleBuilder`, `checked module cache`,
checked module data, platform relation data, or another exact producer/consumer
name.

The word `semantic` is banned in new post-check docs and code. Use the precise
term instead: checked data, checked type store, source meaning,
checked identity, source row position, `FnDef` identity, `FnSet` context, or
another exact stage-owned name.

The word `executable` is banned in new post-check docs and code except in the
phrases `executable binary` and `executable program`. Use the precise stage or
data name instead: Monotype, Lambda Mono, LIR, direct call target, lowered
value, runtime layout, or checked function template.

The word `obligation` is banned in new post-check docs and code. Use the exact
owner instead: checked dispatch plan, erased callable requirement,
specialization queue entry, or debug assertion.

## Canonicalization Stack Safety

Canonicalization is allowed to be referenced by name here because it is the
existing pre-check phase that produces CIR. This section is about the
implementation of that phase only; it does not change the boundary that
post-check compiler stages must consume Checked Modules rather than CIR.

Canonicalization must be fully stack-safe. Traversal code in `src/canonicalize`
must not use direct recursion, indirect recursion, or mutual recursion to walk
source syntax. Deep source nesting must consume explicit work storage allocated
with `std.heap.stackFallback` and then the general allocator, not the process
call stack. Nesting limits such as maximum parenthesis depth must not be used
to protect the implementation from recursion; the traversal shape must be
iterative.

The main expression, block, and associated-item path should be implemented as a
direct labeled-switch kernel rather than as a generic frame pop loop. The
public entry points can remain small wrappers such as `canonicalizeExpr` and
`canonicalizeExprOrMalformed`, but the internal worker should look like a state
machine:

```zig
const CanLabel = enum {
    expr_start,
    expr_complete,
    seq_next,
    block_next,
    block_finish,
    associated_next,
    associated_finish,
};

fn runExprKernel(self: *Self, root: AST.Expr.Idx) Allocator.Error!?CanExprResult {
    var fallback_state = std.heap.stackFallback(16 * 1024, self.env.gpa);
    const scratch_allocator = fallback_state.get();

    var scratch = CanKernelScratch{};
    defer scratch.deinit(scratch_allocator);
    errdefer scratch.cleanupActive(self);

    var expr_state = ExprState{ .ast = root };
    var last_expr: ?CanExprResult = null;

    can_kernel: switch (CanLabel.expr_start) {
        .expr_start => {
            // Inspect expr_state, schedule child work, and jump directly.
            continue :can_kernel .expr_complete;
        },
        .expr_complete => {
            // Use last_expr as the completed child result.
            return last_expr;
        },
        else => unreachable,
    }
}
```

Completed child results should be carried through typed return registers such
as `last_expr`, `last_pattern`, and `last_type_anno`. Avoid a generic result
stack for every child expression. The kernel should keep hot state in locals
and jump directly between labels with `continue :can_kernel .label`, following
the same performance model as the stack-safe parser.

Do not replace recursion with one large tagged union that stores every possible
continuation payload. That tends to copy the largest payload on every push and
pop. Instead, use typed continuation stacks with compact parent-kind enums. For
example, expression continuations can have a small parent-kind stack plus
specialized payload stacks for the cases that need extra data:

```zig
const ExprParentKind = enum(u16) {
    unary,
    bin_lhs,
    bin_rhs,
    list_item,
    tuple_item,
    apply_arg,
    method_receiver,
    method_arg,
    lambda_body,
    if_condition,
    if_then,
    if_else,
    match_cond,
    match_guard,
    match_body,
    while_cond,
    while_body,
    for_list,
    for_body,
    block_expr_stmt,
    block_final_expr,
    block_decl_body,
    block_var_body,
    block_reassign_body,
    block_expect_body,
    block_return_body,
    associated_decl_body,
};
```

Each payload type should live in the stack that matches its shape. Hot nested
constructs such as blocks, sequences, and associated-item groups should use a
current-plus-spill layout: keep the active item in a local or `current` field,
and move the previous active item to a spill stack only when entering another
item of the same kind. This avoids repeatedly copying large block state while
still supporting arbitrary nesting.

Blocks should have an explicit `BlockState` managed by a current-plus-spill
stack. A block state owns the statement slice, next statement index, saved
scratch tops, saved scope flags, pending result indexes, and any block-specific
bookkeeping needed to restore canonicalization state at block exit. Child
continuations must not carry copies of the whole block state. When a statement
schedules child expression work, it should push only the statement-specific
continuation data needed to resume the current block. The `block_next` label
advances statements one at a time, and `block_finish` performs local
forward-reference classification, constructs the block expression, exits
scopes, and restores saved state.

Lists, tuples, calls, method calls, tags, match branches, and other repeated
child forms should use sequence state instead of nested calls. A sequence state
tracks the source items, the next item index, output scratch ranges, and the
continuation to run when all items are complete. Each child result is appended
to the sequence output as it arrives in `last_expr` or the appropriate typed
return register.

Associated items need explicit ownership boundaries. The current
`enterAssociatedBlockState` and `exitAssociatedBlockState` responsibilities
should remain, but the expression kernel should model associated work as active
state rather than as cleanup hidden in pending generic frames.
`CanKernelScratch.cleanupActive(self)` must unwind every active block scope,
associated scope, type-variable scope, and owned alias sink on errors. Correct
cleanup must not depend on eventually popping a particular continuation frame.

Pattern and type-annotation canonicalization should use the same design. Keep
their public entry points, but implement each traversal as a direct
labeled-switch kernel with its own typed return register and typed continuation
stacks. The expression kernel may call those kernels for lambda arguments, loop
patterns, and type annotations because each call is itself nonrecursive and
stack-safe.

The recommended migration order is:

1. Add the expression kernel scratch state, typed stacks, current-plus-spill
   helpers, and active cleanup path.
2. Port expression leaves and small one-child or two-child forms.
3. Port sequence forms such as lists, tuples, calls, method calls, and tags.
4. Port block handling and remove copies of whole block state from continuation
   payloads.
5. Port associated-item integration and verify error cleanup.
6. Port pattern and type-annotation canonicalization kernels.
7. Audit `src/canonicalize` for direct, indirect, and mutual recursive
   traversal calls.
8. Verify with focused canonicalization tests, `zig build minici`, and
   parser/canonicalization benchmarks.

## Type Alias Invariant

Source type aliases are transparent views of their backing type. An alias root
in the checked type store records source spelling and alias arguments. It is not
a nominal type identity, and it is not the authoritative solved representative
for a concrete structure.

When unification relates an alias to a concrete structure, the checker must
unify the concrete structure with the alias backing variable directly. It must
not allocate a replacement alias, redirect the concrete structure to an alias
root, redirect the alias backing through the alias root, or otherwise make alias
preservation depend on union-find representative shape. The alias root may
remain as a transparent checked view whose backing variable carries the solved
structure.

This invariant also covers the degenerate case where the concrete structure
variable is already the alias backing variable. That unification is a no-op
after resolving the backing. Creating a fresh alias representative in that case
would make the alias backing resolve back to the alias itself, which is an
invalid self-referential type-store graph.

Any stage that needs alias spelling, source identity, or user-facing checked
type presentation must consume explicit checked data produced by checking. It
must not infer that presentation from a union-find representative chosen during
structural unification. This keeps the producer responsible for checked
presentation, keeps consumers simple, and keeps release builds fast: no
alias-content cloning, no substitution-map reconstruction, and no cycle-repair
walks are part of normal unification.

For an expression or definition with an explicit type annotation, checking first
proves that the body is compatible with the annotation. After that succeeds,
the checked root for the expression or definition is the annotation root. The
body may have constrained the annotation backing type, underscore variables, or
alias arguments, but references to the annotated value consume the annotation
root. This is how alias spelling from annotations is preserved without making
alias roots union-find representatives for concrete structures.

## Cache Boundary

The checked module cache is the only checked cache boundary in this design.
Checked module cache entries are trusted compiler-produced cache entries, not
adversarial inputs. Cache reads validate only the cache header, format version,
payload hash, key, serialized layout, and ordinary binary decoding. They must
not rerun checked validation, reselect hoisted roots, reconstruct checked data,
or walk checked expressions to prove that cached checked data is still complete.
Correctness belongs to the producer path that writes the cache entry, and
invalidation belongs to the cache key and explicit cache/selection format
versions.

The checked module cache id is target-independent:

```text
CheckedModuleId =
    source_hash
  + compiler_build_hash
  + module_identity
  + checking_context_identity
  + direct_import_checked_module_ids
```

The cache id does not include target ABI, pointer width, layout ids, field offsets,
alignment decisions, backend choice, object format, code-generation options, or
post-check specialization state.

Module boundaries are cache boundaries only. They must never change the final
runtime behavior or performance of the compiled program, except for debug
information. Compiling one large module and compiling the same code split across
imports must produce the same reachable specializations, callable
representations, layout decisions, ARC statements, and backend behavior.

The compiler does not cache Monotype IR, Monotype Lifted IR, Lambda Solved IR,
Lambda Mono decisions, LIR, or any callable/layout representation derived from
them as part of checked modules. Those structures are target/session products
of the current root compilation.

Monotype IR is target-independent, but it is still post-check and root-specific.
It depends on the roots requested for the current compilation, the reachable
monomorphic specializations, and the static-dispatch and source-loop lowering
performed for that compilation. `ConstStore` entries in checked modules are
therefore checked-stage stored constants, not Monotype nodes.

The checked module cache stores checked Roc values only. Roc language values are
target-independent except for pointer-sized Roc values if the language exposes
them to compile-time evaluation. Compiler runtime representation data are not
Roc values and must never enter `ConstStore`: runtime addresses, allocation
identity, layout ids, runtime discriminants, field offsets, LIR proc ids,
backend symbols, object-format details, and backend state are all outside the
checked value domain. Host interaction exists only at runtime, so host handles
and host results cannot be compile-time values. If Roc exposes pointer-sized
values to compile-time evaluation, their checked cache format must be an explicit
checked rule before such values may be output.

When the checker changes what checked data it emits, how hoisted roots are
selected, or how checked compile-time values are serialized, the checked module
cache format or the specific checked-data selection version must be bumped. A
cache hit with a matching key and version is consumed as already-checked output;
the compiler must not pay an extra pass to rediscover whether the cached output
is complete for the checked module.

## Checked Boundary

Checked CIR is the last source-level representation. It owns:

- source expression and statement shape
- source patterns
- checked expression and pattern types
- checked procedure templates
- resolved value references
- method registries
- normalized static-dispatch plans
- platform/app relation data
- `ConstStore` and dependency summaries
- checked module serialization
- all user-facing diagnostics

Checked CIR may contain source-level forms such as static-dispatch calls,
method equality, type-dispatch calls, and source `for` loops because those are
part of the checked source module.

Those forms do not survive Monotype IR lowering.

The checked boundary outputs immutable checked modules. A checked module is
either complete or unavailable to later stages. Later stages may read checked
modules but may not mutate checked source data, perform additional user-facing
checking, or derive missing checked data by scanning source syntax.

During checking finalization, compile-time evaluation may need to lower and run
checked roots before the checked module can be output. That work uses a
`CheckedModuleBuilder`, not a complete checked module. The builder is the only
mutable owner of the in-progress `ConstStore`, dependency summaries, and checked
root payloads.

Compile-time lowering during checking finalization receives a
`CheckingFinalizationView`. That view exposes checked data plus
builder-owned checked result sinks. It is not a `CheckedModule`, and it
is not visible to importers or later post-check stages.

The builder has one transition:

```text
CheckedModuleBuilder.finish() -> CheckedModule
```

After this transition, the checked module is immutable. Post-check runtime lowering,
importers, LirImage construction, and backends only consume
`CheckedModule`. Compile-time lowering during finalization may receive a
builder-owned sink for checked results, but that sink is not an alternate
post-check state and is never visible to importers.

The checked module may store checked-stage constant values in `ConstStore` and
checked procedure templates for promoted callables. It must not store post-check
representation data. In particular, the checked module does not contain runtime
type payloads, value conversion plans, callable-set descriptors, erased callable
ABI decisions, layout ids, runtime tag discriminants, or backend encodings.

This is a checked-boundary rule, not merely a pipeline rule. Any checked
module field whose only purpose is to feed post-check runtime representation is
not part of the checked boundary. If later lowering needs data, checking must
output it as target-independent checked data such as templates, dispatch
plans, method registry entries, platform relation data, hosted declarations, or
`ConstStore` entries. Runtime representation data is produced after checking.

The checked module may output checked data that later stages need, such
as:

- checked procedure templates
- `ConstStore` entries for compile-time constants
- checked dispatch plans
- method registries
- platform, hosted, and exposed function declarations
- opaque, nominal, alias, row, and builtin ownership data

Those data must remain target-independent and representation-free.

### Compile-Time Constants and Hoisted Roots

Compile-time constants are checked roots. A compile-time constant root may be an
ordinary top-level constant or a selected top-level-equivalent expression from a
runtime body. A top-level-equivalent expression is an expression whose checked
dependencies are all available without runtime arguments, mutable runtime state,
host interaction, or observable runtime effects. Its value is computed during
checking finalization and stored in `ConstStore`; later lowering restores that
checked value instead of emitting runtime work for the original expression.

Hoisting does not move source syntax. A hoisted root points at the existing
checked expression and its source region. User-facing compile-time diagnostics,
debug information, crash locations, and source maps must report the expression's
original source location. Synthetic root wrappers and ordering metadata are
compiler-internal only.

Compile-time crash diagnostics use checked source regions carried forward by
post-check lowering, not source text reconstruction. Monotype expressions and
statements carry checked regions beside resolved `SourceLoc` values; LIR stores
the checked region for each source-bearing statement; and the interpreter
captures the failed checked region directly. Compiler-owned or builtin frames
whose checked region is `Region.zero()` are explicit transparent implementation
frames and lower to `SourceLoc.none`, so a callee failure crossing such a frame
reports the checked caller site. Finalization must not recover a checked region
from module display names, source filenames, line/column offsets, or broadest
matching checked nodes.

Hoistability is computed while checking expressions, as part of the existing
recursive checking work that already determines types, resolved references, and
effect data. Checking may return temporary hoistability data from `checkExpr`
and keep temporary binding data in the active lexical scope, but it must not add
permanent hoistability summaries to every checked expression. The checked module
stores only selected hoisted roots plus sparse lookup indexes needed by later
lowering, such as checked-expression id to hoisted-root id and selected
local-binding id to hoisted-root id.

The hoistability decision must use explicit checked data, not source-name scans
or canonicalization guesses. Allowed dependencies include literals, already
known compile-time constants, selected hoisted constants, imported constants
whose checked modules have stored values, and pure checked callables whose
captures are themselves compile-time-known. Rejected dependencies include
function arguments, runtime pattern binders, mutable locals, runtime control
decisions, effectful calls, host calls, platform requirements whose values are
not available during checking finalization, and any static dispatch whose
checked plan does not identify a pure compile-time-evaluable operation.
Low-level operations may participate only through explicit checked purity and
totality metadata; they must never be allowed by whitelist, name, or backend
knowledge.

Checking errors are dependency-local for hoistability. A malformed expression,
unresolved static-dispatch call, type error, or other checker-owned diagnostic
poisons the expression that owns the error and any expression that explicitly
depends on it. It does not poison sibling definitions, unrelated top-level
values, unrelated imported modules, or the checked program as a whole. A checked
CheckedModule data that carries diagnostics is still a valid input to every independent
compile-time-evaluation decision whose expression/dependency region is
well-checked. If one definition is erroneous and another definition is
independently compile-time-known, the independent definition must still be
evaluated during checking and, when reachable, emitted as static data.
The CheckedModule data must therefore be able to contain both diagnostics and
successful compile-time root requests. The presence of diagnostics is not an
module-level root-selection failure.

The compiler must not create separate hoisted roots inside an ordinary top-level
constant body. The whole top-level constant body is already a compile-time root,
so nested hoisted roots would add metadata and scheduling work without removing
runtime work. However, ordinary top-level constants can still depend on selected
hoisted constants indirectly by calling pure checked functions whose bodies
restore selected hoisted locals. Therefore same-module compile-time roots are
emitted as one dependency-sorted request stream, not as permanently separated
top-level and hoisted groups.

Canonicalization's top-level dependency order remains an input for ordinary
top-level constants, and checking should prefer to emit selected hoisted roots
in dependency-first order as it proves and selects them. The request order is
then computed from explicit checked references across all same-module
compile-time roots: ordinary top-level constants, selected hoisted constants,
callable eval roots, and literal conversion roots. Sorting may build
temporary dependency edges while sorting, but it must discard those edges before
the checked module is finalized. The durable checked module data stores only the
roots, the sorted request stream, stored `ConstStore` payloads, and sparse
lookup indexes.

A checked module must not permanently store a hoisted-root dependency graph or
per-expression dependency metadata. The durable checked data is the compile-time
roots, their sorted compile-time request order, their `ConstStore` payloads, and
sparse root lookup indexes.

Checked module caches persist that same sorted selected-root list. On cache
miss, checking computes the list once from explicit checked data while it is
already traversing expressions. On cache hit, the cached list is decoded and
used directly after normal cache header, version, key, payload, and binary-shape
checks. Cache reads must not run a second hoistability analysis or validate
root-set maximality.

The compile-time finalizer consumes sorted root requests and validates that any
referenced same-module constant has already been filled before a root uses it.
That availability check is retained for the generic compile-time pipeline,
which also handles literal conversions, expects, callable roots, imported
constants, and platform-required values. It is not a scheduling graph for
hoisted constants, and it must not require storing dependency edges in the
checked module.

Hoisted-root scheduling is computed after checking has selected the sparse
hoisted roots, because only checked data can distinguish runtime captures,
effects, mutable locals, static dispatch behavior, platform availability,
same-module selected-hoisted const uses, and concrete compile-time types.

Runtime lowering restores a selected hoisted root by checked expression id. While
lowering the synthetic compile-time wrapper for that same root, lowering must
suppress restoration of the root currently being evaluated so the original
expression is evaluated exactly once by the compile-time finalizer. Nested uses
of other already-sorted compile-time constants may still restore their stored
`ConstStore` values.

Hoisted roots use the same compile-time constant rules as ordinary top-level
constants. A failure produced while evaluating a hoisted root is a checking-time
failure reported at the hoisted expression's original source region. If Roc ever
needs lazy-runtime-preserving hoists, that must be a separate checked root policy
with explicit totality and failure behavior; it must not be implemented as a
best-effort variant of top-level constant hoisting.

Imported checked modules must contain every checked procedure template and checked
body that may be instantiated by an importing root. This includes private helper
templates reachable from exported templates, static-dispatch targets, and
compile-time checked callable leaves. Privacy affects source
name lookup and diagnostics. It must not hide checked bodies from whole-program
post-check specialization.

### Function Definitions

`FnDef` is the checked identity of a callable body. It is used in
Monotype `fn_def` expressions and in compile-time checked callable leaves.

```zig
const FnDef = union(enum) {
    local_checked_template: ProcTemplate,
    imported_checked_template: ImportedProcTemplate,
    nested: NestedFn,
    local_hosted: HostedProcId,
    imported_hosted: ImportedHostedProc,
    checked_compiler_generated: GeneratedFn,
};

const ImportedProcTemplate = struct {
    module: CheckedModuleId,
    template: ProcTemplate,
};

const NestedFn = struct {
    owner: ProcTemplate,
    site: NestedProcSiteId,
    context_fn_key: TypeDigest,
};

const PromotedTemplate = struct {
    promoted: PromotedProc,
    template: CallableProcTemplate,
};

const ImportedPromotedTemplate = struct {
    module: CheckedModuleId,
    promoted: PromotedProc,
    template: CallableProcTemplate,
};

const ImportedHostedProc = struct {
    module: CheckedModuleId,
    hosted: HostedProcId,
};

const GeneratedFn = union(enum) {
    entry_wrapper: EntryWrapperId,
    intrinsic_wrapper: IntrinsicWrapperId,
};

const FnTemplate = struct {
    fn_def: FnDef,
    source_fn_ty: CheckedTypeId,
    source_fn_key: TypeDigest,
    mono_fn_ty: MonotypeTypeId,
};
```

A `FnDef` names the checked body/template to instantiate. It is not a
procedure value, LIR proc id, object symbol, erased ABI id, callable-set member,
layout id, or runtime code pointer. If a callable value captures data, the
captures are stored next to the `FnDef` by the value that contains it; the
`FnDef` itself remains capture-free.
`FnTemplate` is the checked callable template used by post-check function-value
flow. It pairs the checked function identity with the checked source function
type. Later stages must carry it forward instead of recovering the checked
function type from generated procedures, runtime layouts, or call sites.

Checked module output assigns a `NestedProcSiteId` to every
expression-position function inside each checked procedure template. A nested
function is identified by `(owner template, nested site, context function
digest)`. The site id is assigned from the checked body traversal before
post-check lowering starts. The context function digest is assigned by Monotype
from the `FnTemplate` whose body currently owns the nested function occurrence.
Monotype lowering carries that checked identity together with the checked source
function type and the lowered monomorphic function type. Post-check stages must
consume those checked data; they must not name nested functions by allocation
order, generated symbols, source display strings, body shape, capture shape,
runtime layout, or LIR procedure ids.

Monotype body lowering tracks two function-context digests:

- the owner function digest for local procedure sites output by the checked
  owner template
- the current function digest for expression-position lambdas and closures
  inside the body currently being lowered

References to local procedures use the owner function digest. That makes
recursive calls and sibling references inside one checked owner point at the
same nested function instance. Lambdas and closures use the current function
digest. That makes a lambda inside a nested local procedure belong to that
nested local procedure, so captures come from the correct body instance.

When Monotype has put a nested function in the nested definition table, that
table is the only owner of the function body. Later value occurrences of the
same `FnTemplate` are references to that nested definition; they do not rebuild
the body or recalculate captures from the occurrence site.

`local_checked_template` is checked-module-relative while the owning builder/checked module
is being processed. Importers refer to the same body through
`imported_checked_template` with an explicit checked module id. Complete imported
checked modules must contain the private checked templates reachable through these
references; consumers never recover imported callable bodies from source text,
display names, generated callable shapes, or runtime values.

`checked_compiler_generated` is only for checked-stage generated
templates that are part of the checked module contract. Functions generated
after checking, such as Lambda Mono specializations or adapters, use stage-local
symbols and are not `FnDef` values.

### Static Dispatch At The Checked Boundary

Checking reports all user-facing static-dispatch errors. This includes missing
methods, ambiguous constraints, illegal equality use, invalid iterator `for`
constraints, and any other error that should be shown to the programmer.
Checked diagnostics include warning severity: a literal (number or string)
defaulted at a generalization boundary that narrows the definition's inferred
type reports `LITERAL DEFAULTED` as a warning, not an error, worded per
literal kind.

The checked module outputs normalized dispatch plans. A dispatch plan is a
checked record, not lowered code:

```zig
const DispatchPlan = struct {
    site: DispatchSite,
    dispatcher: DispatchDispatcher,
    method: MethodNameId,
    dispatcher_ty: CheckedTypeId,
    callable_ty: CheckedTypeId,
    args: []const DispatchOperand,
    result_mode: DispatchResultMode,
};

const DispatchSite = union(enum) {
    checked_expr: CheckedExprId,
    checked_node: CheckedNodeId,
};

const DispatchDispatcher = union(enum) {
    arg: u32,
    type_only,
};

const DispatchOperand = union(enum) {
    checked_expr: CheckedExprId,
    compiler_loop_iterator_state: IteratorLoopStateId,
};

const DispatchResultMode = union(enum) {
    value,
    equality: struct {
        structural_allowed: bool,
        negated: bool,
    },
};
```

Source dispatch, type dispatch, method equality, and iterator `for` plans all
use this one shape. Iterator `for` contains two plans:

- call `.iter` on the source iterable value
- call `.next` on the compiler-created iterator state local

`site` identifies the source construct that produced the plan for debug
verification and source mapping. It is not the call receiver. Expression-shaped
dispatch uses `checked_expr`; source constructs such as iterator `for` that are
not themselves checked expressions use `checked_node`.

`args` are the runtime arguments in callable order. `dispatcher` says how to
find the method owner. For ordinary value dispatch, method equality, and
iterator `.iter`/`.next`, `dispatcher` is an argument index into `args`; iterator
`.next` uses the compiler-created iterator state operand in `args[0]`. For type
dispatch, `dispatcher` is `type_only`, and `dispatcher_ty` is the checked type
that determines the owner. The plan does not choose a concrete lowered call target
for every future monomorphic specialization. Concrete target selection needs
monomorphic type information, so it happens while producing Monotype IR.

The method registry is an exact table keyed by `(MethodOwner, MethodNameId)`.
It is not an owner-discovery mechanism. Post-check code may use it only after a
concrete monomorphic dispatcher type has already determined the owner.

### Optimized Callable-State And Control-Boundary Specialization

`Iter` and `Stream` are public Roc builtins whose methods remain ordinary Roc
functions. Their public representation is the same family:

```roc
Iter(item) :: {
	len_if_known : [Known(U64), Unknown],
	step : () -> [One({ item : item, rest : Iter(item) }), Skip({ rest : Iter(item) }), Done],
}

Stream(item) :: {
	len_if_known : [Known(U64), Unknown],
	step! : () => [One({ item : item, rest : Stream(item) }), Skip({ rest : Stream(item) }), Done],
}
```

`Iter.next` and `Stream.next!` return only `One`, `Skip`, or `Done`. There is no
public `Append` step, no private step variant with different public meaning,
and no compiler-private iterator type exposed to Roc source. The public model is
a length hint and a zero-argument step function. Adapters such as `append`,
`concat`, `map`, filters, ranges, and custom streams are ordinary Roc functions
that build ordinary records and ordinary step callables.

The optimized implementation target is Rust-like generated code: private cursor
state, direct stepping, and no heap allocation for adapter wrappers in consuming
hot paths. Rust gets that shape by making each adapter chain a distinct
monomorphized iterator type with a `next(&mut state)` method. Roc must not copy
that public typing design. Roc keeps the concrete public type `Iter(item)` or
`Stream(item)`, and branches that produce different adapter chains still unify
as that same Roc type.

Roc reaches the optimized shape through ordinary lambdas, lambda sets, captures,
known constructor values, and result demand. A step field is a normal callable.
Lambda-set solving already records finite callable targets and captures behind
the single public callable type. Optimized post-check lowering consumes those
ordinary facts and defunctionalizes reachable callable/capture graphs into
private state machines when the surrounding code only demands private state.
This is not an iterator source-meaning rule; `Iter` and `Stream` are important
clients of a general callable-state and control-boundary optimization.

The essential implementation contract is producer-under-demand lowering. When a
consumer demands only a field, tag, payload, callable call, or loop transition,
optimized lowering asks the producer for exactly that result. It does not first
construct the public record, public callable, public tag, or public iterator
wrapper and then erase it. For an iterator-consuming loop, demand reaches the
`step` callable, the callable demand reaches the finite lambda-set targets and
the captures those targets actually read, and each returned `One`, `Skip`, or
`Done` value is cloned under the loop continuation's demand. The public
`Iter(item)` record exists only when source code observes it as a public value.

This is the Roc analogue of Rust's optimized iterator lowering, but the private
state comes from Roc lambda-set facts instead of from public type erasure. Rust
puts adapter identity in the static type. Roc keeps adapter identity out of the
public type and keeps it in ordinary checked callable facts until optimized
lowering consumes those facts. The optimized state machine is therefore an
implementation artifact of `--opt=size` and `--opt=speed` lowering, not a new
source-level iterator representation.

The optimizer must use actual Roc lambdas and the existing lambda-set model. It
must not add an iterator-specific lambda-set variation, a second callable type
system, or a public type-level encoding of adapter chains. If a callable crosses
an erased or public boundary, ordinary public callable materialization is
selected by explicit materialization demand. That boundary is part of the source
program's public value behavior, not a deoptimization fallback.

The optimized callable-state path is an optimized-code-generation facility, not
a correctness mechanism. It is allowed to spend extra time specializing calls,
control-flow boundaries, and loop-carried private state only when the user has
asked for optimized generated code. Non-optimized lowering must remain the
straight public-value path; it must not construct optimized-demand data,
attempt the optimized path speculatively, or depend on optimized state to
preserve observable Roc behavior.

The optimization is enabled only for optimized code generation:

- on for `--opt=size`
- on for `--opt=speed`
- off for every other optimization mode
- off for `roc check`
- off for compile-time finalization
- off for interpreter builds
- off for dev builds

This is a hard allowlist, not a target policy. Wasm targets, native targets,
tests, package builds, and hosted builds do not get this optimizer unless the
selected build mode is `--opt=size` or `--opt=speed`. The post-check driver
receives explicit build-mode data and chooses either ordinary public-value
lowering or optimized callable-state specialization before constructing the
lowering context. Non-optimized modes must not allocate result-demand
structures, demanded-value arenas, private-state graphs, or optimized worker
queues just to discard them. The mode decision is explicit compiler input; no
stage may infer it from target triples, wasm output, backend choice, method
names, builtin names, generated symbols, object bytes, or backend output.

The gate should be represented as a small post-check lowering choice made
before lowering state exists. One branch constructs the ordinary public-value
lowering context. The other branch constructs the optimized callable-state
context. Those context types must not be the same struct with nullable
optimized fields, because that would let ordinary lowering accidentally pay
the optimizer's allocation cost or call optimized-only helpers. The explicit
build-mode decision is consumed at this boundary; after that, optimized helpers
are reachable only through the optimized context's API.

This gate is part of the optimizer's data-ownership model. Optimized demand
state is not a dormant field on ordinary lowering, and ordinary lowering must
not be able to manufacture an optimized context. `--opt=size` and `--opt=speed`
enter the same callable-state specialization entrypoint and use the same
producer-under-demand semantics; any later size-vs-speed differences belong to
backend optimization preferences, not to the callable-state optimizer.
Focused regressions for optimizer-owned facts must therefore run in both
optimized modes with the same expected private-state shape.

The optimized path is a different post-check lowering entrypoint, not a cleanup
pass after ordinary lowering. It may create extra private workers, private
state loops, and demand-specific direct calls while cloning optimized code. The
ordinary public-value path never constructs those optimized-only artifacts.
Conversely, optimized lowering must not first build public iterator/callable
wrappers and then try to remove them later; avoided materialization is the
design, not a post-pass improvement.

There is also no "try optimized, then fall back to public lowering" path inside
the optimized entrypoint. If optimized lowering needs a fact, that fact must be
explicit optimized input produced by checking, lambda-set solving, known-value
construction, result-demand propagation, or loop fixed-point solving. Missing
required optimized data is a compiler bug to fix at the producer; it is not a
reason to guess from source syntax, recognize a builtin name, inspect lowered
symbols, scan finished LIR, or silently materialize a public wrapper.

This is also not a new whole-program pass between Lambda Mono and LIR. The
optimized entrypoint may use local work queues, demand fixed points, and worker
queues as implementation structure, but those structures are owned by the
single optimized lowering operation. They are created only after the build mode
has selected `--opt=size` or `--opt=speed`, and they are consumed before LIR is
emitted. Ordinary lowering emits public values directly; optimized lowering
emits ordinary LIR after deciding, under demand, which public values never need
to be materialized.

The entrypoint gate is also the compile-time-cost gate. Result demand,
demanded-value arenas, private-state graphs, worker queues, and loop fixed-point
work are constructed only after the post-check driver has selected the optimized
entrypoint. `--opt=size` and `--opt=speed` use the same optimizer and differ
only in later optimization preferences. Dev, check, interpreter, and
compile-time-finalization paths do not build these structures and do not rely on
them for correctness.

Every correctness and generated-code invariant of this optimizer applies to
both optimized modes. A specialized shape proved only for `--opt=size` is not
landed; the same focused property must also be proved for `--opt=speed` unless
the test is explicitly about a later size-vs-speed backend preference. The
callable-state optimizer itself has no size-only or speed-only semantics.

The gate must be represented in the implementation as data ownership, not as a
boolean checked deep inside lowering. Ordinary public-value lowering owns no
result-demand arena, no demanded-value arena, no private-state graph, no
optimized worker queue, and no state-machine fixed-point storage. Optimized
lowering owns those structures and receives them only from the optimized
entrypoint. If a helper needs access to optimized state, its caller must already
be in the optimized lowering context; ordinary lowering must be unable to
construct a dormant optimized context by accident.

The optimized context owns all temporary specialization state:

- result demand and demanded known values
- sparse private-state keys and runtime leaves
- finite callable-state alternatives
- loop-parameter demand fixed points
- demand-keyed direct-call workers

None of those structures are checked output, LIR, ARC input, backend metadata,
or interpreter metadata. They are local implementation data for the optimized
entrypoint and must be consumed before LIR emission. The ordinary lowering
context must not have nullable copies of these fields, debug-disabled versions
of these fields, or lazy constructors for these fields.

This gate is an ownership boundary in the implementation, not just a conditional
inside the optimizer. Ordinary public-value lowering must not import, allocate,
initialize, or retain dormant optimized-demand state. The post-check driver
passes an explicit mode decision into lowering construction, and the optimized
entrypoint is the only place that may create demand tables, private-state
arenas, state-machine work queues, or demand-keyed optimized workers. If a
future build mode wants this generated-code optimization, it must be added to
the explicit allowlist; no consumer may infer it from target, backend, source
constructs, package metadata, or output format.

There is no iterator-plan value, adapter-chain IR, or separate elimination pass.
The only representation before LIR is ordinary Monotype/Lambda Mono data plus
optimized lowering's local demand, known-value, private-state, and worker
tables. Those tables are consumed while the optimized body is cloned and
lowered. The output is ordinary LIR control flow and ordinary LIR values.

This is not a source-level loop-to-recursive-function transform. Optimized
lowering may emit demand-specific private workers and state-machine joins, but
those are implementation details created while lowering already checked control
flow. A source loop remains source loop semantics: outer mutable variables,
branch conditions, guards, scrutinees, appended item expressions, stream
effects, `dbg`, `expect`, `crash`, `break`, and `return` keep their checked
evaluation order and control behavior. The optimizer may update only
compiler-created private cursor state; it must not turn a public Roc value or a
source mutable variable into hidden mutable iterator state.

Control-boundary specialization means optimized lowering may clone a producer
through the continuation that immediately consumes it. A branch expression
clones each branch result under the continuation's result demand. A match
clones the scrutinee under explicit tag and payload demand, then clones branch
results under the outer demand. A loop solves loop-parameter demand as a fixed
point over body observations and reachable `continue` edges. A direct call may
create an optimized worker keyed by callee identity, argument facts, and result
demand. These are all the same optimization family: defunctionalization and
specialization under exact demand. They are not separate rules for `for`, `if`,
`match`, or iterator builtins.

This design follows the same broad precedent as closure conversion,
defunctionalization, partial evaluation, and iterator or coroutine
state-machine lowering in optimizing compilers. The important Roc-specific
constraint is that the specialized state machine is never the public source
type. Public Roc values remain ordinary immutable values; compiler-created
private state exists only inside optimized lowering and disappears into ordinary
LIR before ARC and backend code generation.

The optimizer does not introduce a new global pass over finished code. It works
where the producer and consumer meet: a consumer creates exact demand, the
producer is cloned under that demand, and any private worker or private state
loop is emitted from that same cloning context. Later stages should never need
to infer that an iterator wrapper, callable wrapper, or public record was
accidentally materialized and should have been avoided.

The implementation may organize optimized lowering into helper phases, but those
phases are internal to the optimized entrypoint and operate on explicit demand
and known-value data as the body is cloned. They are not a second semantic IR, a
whole-program cleanup pass, or an analysis that ordinary lowering has to run and
then ignore.

The optimized entrypoint is the only place that may run the extra demand work.
If a helper would need result demand, demanded known values, sparse private
state, state-machine fixed points, or demand-keyed worker creation, that helper
belongs to the optimized lowering context. If ordinary public-value lowering
needs the same source program to compile, it must do so through the ordinary
materializing path without constructing dormant optimized state.

The optimized entrypoint may contain internal helper phases, but those phases
are ordered by data dependency, not by source syntax:

1. establish the consumer's result demand
2. clone the producer under that demand
3. refine finite callable/tag/direct-call facts exposed by that clone
4. solve any loop-carried demand fixed point created by reachable transitions
5. emit ordinary LIR from the resulting private state or materialized value

No phase may recover missing demand from names, lowered symbols, backend
output, wasm bytes, disassembly, or completed LIR. If a later helper needs a
fact, the earlier clone-under-demand step must produce it explicitly.

Current implementation work must therefore converge by replacing dense
public-wrapper paths with this producer-under-demand lowering. A partial
implementation that creates public wrappers, lowers them, and then removes them
later is not this design. A partial implementation that recognizes `Iter`,
`for`, `if`, `match`, wasm, Rocci Bird, or generated symbol names is also not
this design. The transition is complete only when the same demand machinery
explains optimized direct calls, branch results, match results, loop-carried
state, finite callable alternatives, and public materialization boundaries.

This boundary is about compiler cost and generated code quality, not
correctness. Checking, static-dispatch finalization, compile-time root
selection, compile-time evaluation, static data emission, and
`crash`/`dbg`/`expect` diagnostics are required in all modes and must not depend
on this optimizer. Compile-time evaluation may produce constants that optimized
lowering later consumes, but it must not construct optimized runtime private
state. Build modes may differ in generated code size, generated code speed, and
compile time, but never in observable Roc behavior.

The optimizer is opt-mode-only because it deliberately performs extra
post-check work: demand propagation, finite callable-state splitting,
demand-specific direct-call worker discovery, and loop-state fixed points. Those
costs are justified when the user asks for optimized generated code. They are
not justified for fast feedback paths. Dev builds should favor low compiler
latency and debuggable public-value code. `roc check` and compile-time
evaluation must report the same checking results without constructing private
runtime state. Interpreters and backend-independent consumers must see the
ordinary public lowering path unless they explicitly request optimized code.

Optimized callable-state specialization uses one set of generic compiler facts:

- direct-call targets
- finite lambda-set callable targets
- callable captures
- known records, tuples, tags, nominals, and primitive leaves
- checked type and layout decisions
- explicit result demand

The same mechanism applies to any ordinary Roc value whose producer and
consumer expose enough checked facts under demand. `Stream` does not get a
separate compiler rule, `Iter` does not get a separate compiler rule, and a
record wrapping a primitive does not get a more powerful rule than the primitive
itself. The optimizer specializes callable state because the checked program
contains finite callable facts, not because a value has a particular builtin
name.

The intended implementation therefore has two explicit post-check lowering
contexts. Ordinary lowering owns public-value construction and has no demand
arena, demanded-known-value table, sparse private-state table, worker queue, or
loop-state fixed-point storage. Optimized lowering owns those structures and is
constructible only from the `--opt=size`/`--opt=speed` entrypoint. Helpers that
create or consume optimized-only facts must take the optimized context
directly; ordinary lowering should be unable to call them by accident.

The pass must not recognize source `for`, source `if`, source `match`,
`Iter.append`, `Stream.next!`, wasm targets, Rocci Bird, public builtin names,
or generated symbol names as optimization triggers. Source `for` lowers through
the ordinary public `.iter` and `.next` meaning. Source `if` and `match` lower
as ordinary control flow. The optimizer observes the generic demand and
callable-state facts created by that lowered code; it never asks which source
construct produced them.

Control-flow precision comes from the checked/Lambda representation that is
already being lowered, not from source-shape rules. Branches merge demanded
results because the continuation demands a value from the branch expression.
Matches demand tag choices and payloads because the continuation observes those
facts. Loops demand parameters because body observations and reachable
`continue` edges consume those parameters. These are ordinary producer-consumer
relationships in the lowered program, so adding support for one control-flow
form must not add a separate rule for a builtin or syntax form.

Result demand is the optimizer's exact statement of how the current continuation
will use a value. Required demand forms are:

- materialize the ordinary public Roc value
- use a runtime leaf as private state
- read record fields by field name
- read tuple items by original item index
- unwrap nominal backing data
- inspect a tag and read demanded payloads by original payload index
- call a callable and read demanded captures by original capture index
- consume a direct-call result under another demand
- carry loop values through initial values and `continue` edges

Demand is threaded through cloning. It is not a late program scan and not a
cleanup pass. Field access clones the receiver under field demand. Tuple access
clones the receiver under item demand. A tag match clones the scrutinee under
tag demand. A call through a known callable clones the callable under call
demand, then clones the selected body under the caller's result demand. A loop
clones initial values, body observations, and `continue` edges under the
fixed-point demand for loop parameters. If the surrounding context needs an
ordinary Roc value, the demand is materialization and public lowering is used.

Demand propagation is the only source of private state shape. Optimized
lowering must not derive private state by starting with a dense public value and
dropping fields opportunistically. It must also not rediscover demand by
scanning completed LIR, symbol names, generated code, wasm disassembly, or
backend artifacts. If a producer needs a private shape, that requirement must be
visible as explicit demand at the point where the producer is cloned.

Known values are optimizer facts, not runtime values. They are not limited to
aggregate source syntax. Primitive leaves are first-class known values, so a
`U64` loop cursor must optimize the same way whether it appears directly or
inside a single-field record. Records and tuples are only one way to expose
children; wrapping a primitive in a record must never be required to make it
optimizable.

Known children must distinguish "unknown but carried" from "not demanded."
Dense child arrays cannot represent that distinction for tuple items, tag
payloads, or callable captures. The private-state representation therefore
stores demanded children sparsely by checked child identity: record field name,
tuple item index, tag payload index, nominal backing value, and callable capture
index. A missing child means private state does not carry it. A present child
whose fact is unknown means private state carries the runtime value but has no
more precise structure.

Sparse demanded private state must not be forced through the ordinary dense
public `Value` representation. Ordinary public values require all children
needed by the public layout. They cannot represent "capture 2 is present and
capture 0 is absent" or "the tag choice is present and payloads are absent"
without inventing fake children. Sparse private state may be converted to an
ordinary public value only at an explicit materialization boundary.

Every private state value is closed over the state boundary where it is used. A
state body may read only its state parameters, globals/constants, and locals
bound inside that state body. If a demanded value depends on a local introduced
by a branch, match payload, guard, pending let, or nested control region, the
optimizer must either keep the binding control inside the state body that uses
the local or turn the local's value into an explicit runtime leaf carried across
the state transition. It must never create a specialized state whose body refers
to a local that was bound only in a different branch or predecessor state.

This is a lowering invariant, not an ARC or backend repair opportunity. LIR
joins and blocks receive explicit parameters. ARC certifies ownership of those
parameters and locally bound values; it must not infer missing state parameters
from out-of-scope references. A scope-closure failure in optimized lowering is a
compiler bug that must be caught before ARC insertion.

Callable-state specialization is defunctionalization driven by result demand.
When a demanded callable has finite known targets, each target plus its demanded
captures is a private state alternative. A single known target can inline
directly. Multiple known targets become a state-machine dispatch before public
callable materialization. The selected target may change as the state machine
advances; for example, an append iterator can move from the append wrapper to
the source iterator and then to the empty iterator. Those finite alternatives
remain private states instead of being widened to an opaque callable merely
because different edges have different targets or capture shapes.

Loop-carried demand is a fixed point. The loop body creates demand on loop
parameters from observations such as field reads, tuple reads, tag matches,
callable calls, direct-call results, and public materialization. Each reachable
`continue` edge then clones the value for parameter `i` under the current demand
for parameter `i`; that clone may add demand to other loop parameters through
values it reads. The loop demand is complete only when reachable body
observations and reachable `continue` edges stop changing loop-parameter
demand.

The fixed point is over compiler-owned loop-carried private state, not over all
program variables in scope. A loop may read or write ordinary source variables,
call effectful stream steps, return, break, or evaluate diagnostics exactly as
the checked program says. Those operations remain in ordinary control flow.
Only values that the optimized lowering itself has split into private state
become private loop parameters. If a value must be observed through the public
Roc representation, materialization demand ends the private-state path for that
value.

The fixed point is exact, not heuristic. There is no state-count cutoff,
size-count cutoff, or fallback path. If the graph grows unexpectedly, the fix is
to make demand propagation more precise, share equivalent states, or remove
unnecessary source demand. Silently disabling the optimization for a large graph
would make post-check output depend on an arbitrary implementation limit and is
not allowed.

Loop values may include runtime leaves, known aggregate structure, known tags,
and known callable alternatives. Runtime leaves are loop parameters; they are
not finite-state dimensions. Known tag and callable choices are finite private
states only when demanded. This keeps an iterator phase change, such as an
append wrapper advancing to its source iterator, as an explicit private state
transition without making ordinary numeric cursor values explode the state
graph.

For `Iter` and `Stream`, a consuming loop that only steps the value demands the
step callable and the captures needed by that callable. It does not demand
`len_if_known`, so private loop state must not carry the public size-hint field
or execute size-hint overflow/sentinel bookkeeping. If source code reads the
size hint, returns the iterator, stores it, passes it to unspecialized code, or
directly observes the public step result, that use creates materialization or
field demand and the ordinary public fields are preserved.

Public iterator and stream values remain immutable and reusable. Source such as:

```roc
iter = [1, 2].iter()
saved = iter

for item in iter {
	{}
}

use(saved)
```

must not mutate `saved`. Any cursor mutation introduced by optimization belongs
to compiler-created private loop state derived from `iter`, not to the public
Roc value. This rule is the same for pure `Iter` and effectful `Stream`; stream
effects still occur exactly when the source program steps the stream.

Some uses require the public representation: storing or returning an iterator
as an ordinary value, passing it to unspecialized code, directly matching on the
public result of `Iter.next` or `Stream.next!`, or otherwise observing the
public callable/record value. At those boundaries the compiler builds the
ordinary public record, callable value, and step-result tag. That is ordinary
lowering selected by materialization demand, not a post-link cleanup pass.

Finite and infinite iterators use the same public model. An unbounded range or
custom Fibonacci-style iterator is just a step callable that may never produce
`Done` unless a later adapter does. Optimized finite consumers may become
bounded private loops when known values prove a bound; otherwise they remain
ordinary potentially nonterminating Roc computations.

The optimization creates extra direct-call workers only from explicit call
patterns discovered while cloning optimized code. Worker identity includes the
callee identity, split argument facts, result demand, and relevant type/layout
decisions. The original public-ABI body remains available. Extra workers are
implementation details for optimized direct calls and callable-state
specialization, not replacements for public callable boundaries.

Private state machines lower to existing LIR control flow before ARC and
backend code generation. Each private state becomes an ordinary join/block or
equivalent loop shape, and each transition becomes a direct jump with explicit
values. LIR, ARC, LirImage, the interpreter, LLVM, object, wasm, and Binaryen
code do not know iterator rules, stream rules, public step-callable layouts, or
reference-count policy for iterator wrappers. ARC remains the only owner of
reference-count insertion.

The implementation must move directly to this design. There is no intermediate
iterator-specific design to preserve, and no short-term fallback path to keep
around after the generic mechanism exists. The optimized lowering pipeline is
responsible for these pieces as one coherent system:

- an explicit `--opt=size`/`--opt=speed` entrypoint gate
- result demand as first-class optimizer data
- sparse demanded private state for records, tuples, tags, nominals, callables,
  and primitive leaves
- demand-aware cloning through calls, branches, matches, and loops
- finite callable-state defunctionalization from ordinary lambda-set facts
- loop-parameter demand fixed points over observations and reachable
  `continue` edges
- demand-keyed optimized direct-call workers
- explicit public materialization boundaries

If an optimized path needs information that is not available, the fix is to make
an earlier stage produce that information explicitly. It is not acceptable to
recover the fact from source syntax, builtin names, generated symbol names,
completed LIR, backend output, wasm bytes, object bytes, or disassembly. If an
ordinary public value must be observed, optimized lowering materializes it at
that point. If it does not need to be observed, optimized lowering avoids
constructing it in the first place.

The success condition is backend-neutral. A Rocci Bird `--opt=size` wasm build is
an important integration proof, but the invariant is stronger: focused compiler
tests must show that `.iter()` and direct-list source forms have equivalent
optimized hot-path state, no public iterator wrapper allocation, no
unobserved `len_if_known` work, and no iterator-specific rules in LIR, ARC, or
backends. Rust's iterator output is the performance reference shape, not the
source-language model Roc should copy.

### Structural Serialization Methods

Parsing and encoding are ordinary static-dispatch methods. Roc does not expose a
builtin `Parser`, `Decoder`, or `Encoding` interface type; the public model is
method-based.

The performance target is the same shape as hand-written systems parsers:
formats keep input state as cursors and slices, avoid runtime allocation during
parsing, receive the whole requested structural shape before scanning, and lower
to direct calls rather than callback tables, shape interpreters, or temporary
maps built for convenience. The compiler knows Roc structural shapes and method
requirements. It does not know JSON, HTTP headers, CSV, XML, or any other
serialized format.

A format is ordinary Roc code. Its type owns the methods that describe how that
format reads or writes each shape. Public modules expose small convenience
functions:

```roc
thing = Json.parse(json_str)?
thing = Json.Utf8.parse(json_bytes)?

json_str = Json.encode(thing)?
json_bytes = Json.Utf8.encode(thing)?

headers = Headers.parse(raw_headers)?
```

The convenience functions construct the format state directly, call the value or
type's ordinary method, validate the remaining state if the format requires it,
and return the final `Try`. They do not need a required `init`, `finish`, or
`default` hook. A header helper can build the initial state as ordinary Roc data:

```roc
state = Headers.{ raw }
```

The underlying parse method is public and callable. It is deliberately curried:

```roc
a.parser_for : encoding -> (state -> Try({ value : a, rest : state }, err))
a.encode_to : a, encoding -> (state -> Try(state, err))
```

`parser_for` is a method on the value type being produced. `encode_to` is a method
on the value being serialized. Structural types get these methods from the
compiler. Nominal types may define them explicitly, and structural derivation
uses those explicit nominal methods when a field, payload, list element, nested
value, or other sub-shape has that nominal type.

The `encoding` argument is the pure format/configuration value used to construct
the specialized parser. It may represent choices such as JSON object field
renaming, JSON tag representation, or a header matching mode. The `state`
argument is the runtime cursor or output state. Keeping these separate matters:
parser construction can transform the requested structural shape before the
runtime scan starts, while the returned runtime function threads only the cursor
state and parsed values.

For example, a public HTTP header helper has this shape:

```roc
Headers := { raw : Str }.{
	DecodeErr := [MissingRequired, BadHeader].{}

	parse : Str -> Try(output, DecodeErr)
		where [
			output.parser_for : HeaderEncoding -> (Headers -> Try({ value : output, rest : Headers }, DecodeErr)),
		]
	parse = |raw| {
		Output : output

		parse_output = Output.parser_for(HeaderEncoding.Caseless)
		parsed = parse_output(Headers.{ raw })?

		Ok(parsed.value)
	}
}
```

The exact shape of `HeaderEncoding` is format-owned; it is not a compiler
interface. The important split is that `Output.parser_for(HeaderEncoding.Caseless)`
constructs the concrete parser and `Headers.{ raw }` is the runtime input state.
Formats with no configurable behavior can still use a zero-sized encoding value.

The error type is inferred from the format methods. All `Try` errors in one
parse or encode operation unify with the public function's returned error type.
When a concrete operation cannot fail, its error type is empty, so an exhaustive
`Ok(value) = Json.encode(thing)` binding is accepted.

Checking derives structural methods by emitting ordinary static-dispatch
constraints. For example, deriving `a.parser_for` for a concrete shape asks the
encoding and state types for exactly the methods needed by that shape:

- `Str` calls the format's string method;
- records use compiler-generated field sets and the format's record-field
  method;
- tag unions call the format's tag-union method with a compiler-generated
  tag-union spec;
- lists, numbers, booleans, tuples, and other structural forms call the
  corresponding format methods;
- type aliases use their expanded structural shape;
- named nominal values call that nominal type's explicit method. If the method
  is missing, checking reports the missing static-dispatch requirement.

If a format does not support a shape, checking reports the missing method as a
static-dispatch error. Unsupported shapes are not represented as runtime parse
or encode failures. Runtime failures are reserved for input/output conditions
the format can only know while processing bytes or values, such as a malformed
header line, invalid JSON syntax, invalid UTF-8 in a byte input, or a
user-defined nominal method returning an error.

Compile-time evaluation uses the ordinary Roc constant machinery. The
serialization API does not add a special compile-time marker. A derived
`parser_for` constructs its transformed field sets and nested parsers before it
returns the runtime lambda. If that parser construction is evaluated during
checking, those transformed values are stored as checked constants and restored
later as ordinary Roc values. The returned runtime lambda then closes over only
the transformed field sets and nested parser functions. For a parser constructed
at compile time, original record field names that were renamed during
construction do not need to appear in the final runtime data.

Tag-union specs are opaque compiler values. They describe the concrete
structural shape being derived: tag names, payload shapes, and the concrete
payload result positions. They are not arity-specific user APIs, and userspace
code does not construct or pattern match on them. The compiler specializes every
use with the concrete tag-union type, so opaque spec operations lower to direct
tag code.

Userspace format code operates through safe Roc values, opaque specs, opaque
field values, iterators, and slice-returning string/list APIs. The compiler does
not expose raw field-slot indices, unsafe byte indexing, or unchecked memory
primitives as part of the serialization method surface.

Record parsing is driven by the compiler-generated structural `parser_for` method.
The compiler creates a `Str.FieldName.FieldNames(_shape)` value for each
concrete record shape:

```roc
Str.FieldName(_shape) : opaque
Str.FieldName.FieldNames(_shape) : opaque

Str.FieldName.FieldNames.rename_fields : Str.FieldName.FieldNames(_shape), (Str -> Str) -> Str.FieldName.FieldNames(_shape)
Str.FieldName.FieldNames.shortest_name : Str.FieldName.FieldNames(_shape) -> U64
Str.FieldName.FieldNames.longest_name : Str.FieldName.FieldNames(_shape) -> U64
Str.FieldName.FieldNames.iter : Str.FieldName.FieldNames(_shape) -> Iter(Str.FieldName(_shape))
Str.FieldName.FieldNames.for_size : Str.FieldName.FieldNames(_shape), U64 -> Iter(Str.FieldName(_shape))

Str.FieldName.name : Str.FieldName(_shape) -> Str
```

`Str.FieldName.FieldNames(_shape)` contains the requested field names and
compiler-owned result positions for one concrete record shape.
`Str.FieldName(_shape)` is an opaque handle to one field in that same shape. The
`_shape` parameter is a phantom type: it is not runtime data, but it ties a
field handle to the exact field set that created it. A parser for
`{ cache_control : Str, content_length : U64 }` cannot accept a
`Str.FieldName` produced from `{ foo : Str }`, because the phantom types do not
unify. That type-level tie is what lets generated record parsers avoid runtime
bounds checks on field handles. If the only way to obtain a
`Str.FieldName(_shape)` is from the matching
`Str.FieldName.FieldNames(_shape)`, then the compiler already knows every handle
is in range for that record. There is no user-exposed `U64` slot to validate at
runtime.

The derived `parser_for` constructs field metadata before returning the runtime
lambda:

```roc
renamed_fields = Str.FieldName.FieldNames.rename_fields(original_fields, |name| encoding.rename_field(name))
parse_nested = Nested.parser_for(encoding)
```

`encoding.rename_field(name)` is ordinary method-call syntax for a pure format
method whose first argument is the encoding value. Every encoding provides it;
identity is the normal implementation. Taking the encoding value as an argument
lets one encoding type store parser-construction configuration such as JSON
field naming style. `Str.FieldName.FieldNames.rename_fields` applies that
function to every requested record field, discards the original names from the
returned `Str.FieldName.FieldNames`, and rebuilds the length buckets used by
`Str.FieldName.FieldNames.for_size`, `Str.FieldName.FieldNames.shortest_name`,
and `Str.FieldName.FieldNames.longest_name`. If parser construction is
compile-time evaluated, the renaming work is also compile-time work. For JSON
camel-case decoding, the final runtime parser can contain only `camelCase`
field names. For HTTP header decoding, the final runtime parser can contain only
lowercase kebab-case header names such as `cache-control`.

Formats expose the methods needed for the shapes they support. A format that can
parse strings, `U64`, tag unions, and records uses these method shapes:

```roc
encoding.parse_str : encoding, state -> Try({ value : Str, rest : state }, err)
encoding.parse_u64 : encoding, state -> Try({ value : U64, rest : state }, err)
encoding.parse_tag_union : encoding, ParseTagUnionSpec(a), state -> Try({ value : a, rest : state }, err)

encoding.parse_record_field : encoding, Str.FieldName.FieldNames(_shape), state -> Try(
	[
		Field({ field : Str.FieldName(_shape), rest : state }),
		TryField({ name : Str, rest : state }),
		TryFieldCaseless({ name : Str, rest : state }),
		Continue({ rest : state }),
		Done({ rest : state }),
	],
	err,
)

encoding.skip_record_field : encoding, state -> Try(state, err)
encoding.missing_record_field : encoding, Str, state -> err
encoding.missing_optional_field : encoding, Str, state -> optional_err
encoding.rename_field : encoding, Str -> Str
```

For `Field`, `TryField`, and `TryFieldCaseless`, `rest` is the state positioned
at the field's value. If the field matches the target record, the generated
parser calls the parser for that field's type from that value-start state and
continues from the value parser's returned `rest`. This is what allows records
with different field shapes:

```roc
{
	content_length : U64,
	x_auth_token : Try(Str, [Missing]),
	cache_control : Str,
}
```

The record loop does not store every value as `Str` first. When it sees the
`content_length` field, it calls the `U64` parser from the value-start state and
continues from that parser's returned state. When it sees `cache_control`, it
calls the `Str` parser. The value parser owns value consumption.

`Field` means the format already matched the input field name against the
provided `Str.FieldName.FieldNames(_shape)`, usually by iterating
`Str.FieldName.FieldNames.for_size(fields, len)`
or another field iterator. `TryField` means the format parsed a field name and
asks the generated record parser to exact-match it against the transformed
fields. `TryFieldCaseless` is the same, but uses ASCII caseless matching. If a
`TryField` or `TryFieldCaseless` name does not match any target field, generated
code calls the format's `skip_record_field` method with the encoding and `rest`,
then continues with the returned state. This avoids scanning matched values
twice while still letting unknown fields be skipped correctly.

`Continue.rest` advances the record loop after the format has consumed input
that cannot be a relevant field. `Done.rest` is the state remaining after the
record ends. If the generated finisher sees that a required field was never
filled, it calls the format's `missing_record_field` method with the encoding,
field name, and final state to produce the format's concrete parse error value.
Optional fields are expressed by their field type, for example
`Try(Str, [Missing])`. If an optional field is absent, the generated finisher
calls the format's `missing_optional_field` method with the encoding, field
name, and final state at the optional field's error type and stores
`Err(missing)` in that field. This lets the format define the absence tag;
`Missing`, `Absent`, or any other tag name is ordinary userspace data, not a
compiler-known concept. A field annotated as `Try(Str, _)` can infer that error
type from the format method's return type.

Record-field dispatch is optimized around the assumption that serialized record
field names are overwhelmingly small. JSON object keys, HTTP headers, CSV
column names, XML attributes, environment variables, and similar schema fields
are expected to land in Roc's small-string representation almost all the time on
64-bit targets, and still most of the time on 32-bit targets. The optimization
strategy treats this as the hot path, not as a correctness requirement: long
field names remain supported, but generated code is arranged so that small names
take the shortest route.

Formats own conversion from Roc record field names to serialized field names.
HTTP header parsing can rename `cache_control` to `cache-control` at parser
construction time and then use `TryFieldCaseless("Cache-Control")` at runtime.
JSON camel-case parsing can rename `user_id` to `userId` at parser construction
time and then use `TryField("userId")` at runtime. The compiler does not know
those policies; it only knows that it has a transformed
`Str.FieldName.FieldNames(_shape)` value and a requested matching mode.

`Str.FieldName.FieldNames.shortest_name` and
`Str.FieldName.FieldNames.longest_name` are computed after renaming. Formats may
use them to skip impossible fields before doing more expensive work. For
example, if a header name is longer than
`Str.FieldName.FieldNames.longest_name(fields)` and the format's `rename_field`
never increases field length for headers, the format can consume the line and
return `Continue` without constructing any temporary field name. This is not a
parse failure: for formats such as HTTP headers and JSON objects, unknown fields
remain ordinary input according to that format's rules. If the target record
actually contains a long renamed field name, the long input field remains
matchable through the same `Str.FieldName.FieldNames` iteration APIs.

For small fields, generated record dispatch compares the packed small string
representation directly. Roc zeroes unused SSO bytes, so equality can use
fixed-width word comparisons without masking tail bytes. On 64-bit targets, the
generated dispatcher groups fields into 1-8, 9-16, and 17-23 byte size classes;
on 32-bit targets, the groups are scaled to that target's smaller SSO capacity.
The group selection can be implemented with a branchless or near-branchless
table lookup instead of a source-level length switch.

Within each size class, the compiler chooses the most discriminating word lane
for the concrete field set. For example, if several fields share the same first
eight bytes, the generated code can use the second or third word as the first
comparison instead. The hot miss path compares one machine word per candidate in
that class. Only after a discriminator hit does the code verify the full SSO key
with one, two, or three word comparisons and dispatch to the matched field's
already-constructed value parser. Collision-heavy classes may use another
discriminating lane or a generated perfect hash over the packed SSO words before
final verification.

This keeps the performance center on the common case: no heap allocation, no
runtime field map, no interpretation of a record plan, and no byte-by-byte
string comparison unless the selected format's field-name conversion itself
requires it. Long-field paths must preserve the same public behavior and memory
invariants. If a format must handle long fields without allocation, that path
must use field iteration and slice comparisons rather than constructing a
transformed heap `Str`; it is not allowed to make the SSO path slower for the
sake of generality.

Nested records follow the same construction/runtime split. The outer derived
`parser_for` method eagerly calls every nested parser constructor before
returning its runtime lambda. A nested record gets its own
`Str.FieldName.FieldNames(_nested_shape)` value, then renames and rebuckets that
field set through the same `encoding.rename_field` method. A custom nominal
field calls that nominal type's explicit `parser_for` method during parser
construction. At runtime the outer record parser dispatches to the
already-constructed field parser for the matched field shape.

Tag-union parsing follows the same separation. The format's tag-union method
receives the complete tag spec, identifies the input tag according to that
format's own rules, and uses opaque spec operations to parse and assemble the
selected payload. Recursive tag unions are ordinary recursive method calls
through the selected payload type. The compiler knows the Roc shape and the
static-dispatch requirements; it does not know any format-specific tag
representation. Tag-name renaming can use an analogous construction-time
transformation later; record field renaming does not require the compiler to
know any tag-union convention.

The generated code uses direct static calls. Tag spec matching is compiler-
generated exact matching over the concrete tag labels; userspace does not pass a
matcher function to spec operations. It does not pass user callbacks,
does not build a runtime interpretation plan, and does not route shape handling
through a central dispatch function. Generic userspace format code produces
record field events, iterates opaque field sets, and calls opaque tag spec
operations. The record loop and field dispatch are compiler-generated for the
concrete shape; tag spec operations are compiler primitives specialized for the
concrete tag-union shape and lower to direct code.

Input formats return seamless slices whenever the value being produced is a
slice of the original input. Parsing a `Str` from a larger `Str` or validated
byte buffer returns a slice into that buffer when the format can do so. The
format must validate bytes before producing `Str`; `Json.Utf8.parse` validates
string bytes from `List(U8)`, while `Json.parse` starts from an already-valid
`Str`. Hosts that pass request memory to Roc as `Str` must validate that memory
first and keep it alive for the duration of the request.

The HTTP header format receives only the raw header section, starting at the
first header line and ending before the blank line. Its record-field method
parses one CRLF-delimited line at a time. Each non-empty line must contain `:`;
otherwise the method returns the header format's bad-header error.

The header encoding's `rename_field` maps Roc field names to lowercase
kebab-case at parser construction time:

```roc
cache_control -> cache-control
content_length -> content-length
x_auth_token -> x-auth-token
```

At runtime the header parser parses the input line name as a seamless slice. It
may use `Str.FieldName.FieldNames.for_size` plus ASCII-caseless comparison
against `Str.FieldName.name` to match the transformed field set directly and
return `Field({ field, rest: value_start })`. It may also return
`TryFieldCaseless({ name, rest: value_start })` and let generated record
dispatch perform the ASCII-caseless match. If the name cannot match any target
field, the format consumes the line and returns `Continue({ rest: next_line })`.
Matching `Cache-Control`, `cache-control`, and `CACHE-CONTROL` against the
transformed `cache-control` field set does not require allocating a lowercased
copy. Header values are trimmed and passed to field parsing as seamless `Str`
slices. The format does not allocate a header map.

The JSON `Str` format receives valid UTF-8 text. The JSON `Utf8` format receives
bytes and validates UTF-8 before producing any `Str`. JSON record parsing scans
an object one field event at a time through the compiler-generated record loop,
so object key order does not affect performance beyond normal key matching. A
plain JSON encoding value can use identity `rename_field`. The same JSON
encoding type can carry a camel-case configuration value that renames Roc fields
at parser construction time:

```roc
user_id -> userId
cache_control -> cacheControl
```

The runtime JSON scanner can use `Str.FieldName.FieldNames.for_size` and exact
`Str.FieldName.name` comparison to match each object key against the
already-renamed field set and return `Field({ field, rest: value_start })` for
known keys. It may also return `TryField({ name, rest: value_start })` and let
generated record dispatch perform exact matching. For unknown keys, it skips the
JSON value according to JSON syntax and returns
`Continue({ rest: after_value })`. The matched field's parser consumes the JSON
value from `value_start`.

JSON tag unions use the externally tagged object representation:

```json
{ "Admin": { "name": "Sam" } }
```

This representation avoids collisions between tag names and ordinary record
field names. Other JSON conventions are represented by different JSON format
values with different methods; the compiler does not know any JSON-specific
syntax, null value, missing-field rule, or tag-union convention.

Parsing a Roc `Str` from JSON succeeds only for JSON string values. JSON `null`
and missing object fields are separate format conditions. They are surfaced only
through field or value types that request them, such as `Try(Str, [Null])` or
`Try(Str, [Missing])`; the plain `Str` method does not accept either condition.

Concrete HTTP header parser code has this public shape:

```roc
Headers := { raw : Str }.{
	DecodeErr := [MissingRequired, BadHeader].{}

	parser_for : () -> (Headers -> Try({ value : output, rest : Headers }, DecodeErr))
		where [
			output.parser_for : HeaderEncoding -> (Headers -> Try({ value : output, rest : Headers }, DecodeErr)),
		]
	parser_for = || {
		Output : output
		Output.parser_for(HeaderEncoding.Caseless)
	}

	parse : Str -> Try(output, DecodeErr)
		where [
			output.parser_for : HeaderEncoding -> (Headers -> Try({ value : output, rest : Headers }, DecodeErr)),
		]
	parse = |raw| {
		Output : output
		parse_output = Output.parser_for(HeaderEncoding.Caseless)
		parsed = parse_output(Headers.{ raw })?
		Ok(parsed.value)
	}
}

HeaderEncoding :: [Caseless].{
	rename_field : HeaderEncoding, Str -> Str
	rename_field = |_, name| underscores_to_dashes(name)

	parse_str : HeaderEncoding, Headers -> Try({ value : Str, rest : Headers }, Headers.DecodeErr)
	parse_str = |_, state| {
		value_parts = take_header_value(state.raw)?
		Ok({ value: value_parts.value, rest: { raw: value_parts.after } })
	}

	parse_u64 : HeaderEncoding, Headers -> Try({ value : U64, rest : Headers }, Headers.DecodeErr)
	parse_u64 = |_, state| {
		value_parts = take_header_value(state.raw)?

		match U64.from_str(value_parts.value) {
			Ok(value) => Ok({ value, rest: { raw: value_parts.after } })
			Err(_) => Err(Headers.DecodeErr.BadHeader)
		}
	}

	parse_record_field : HeaderEncoding, Str.FieldName.FieldNames(_shape), Headers -> Try(
		[
			Field({ field : Str.FieldName(_shape), rest : Headers }),
			TryField({ name : Str, rest : Headers }),
			TryFieldCaseless({ name : Str, rest : Headers }),
			Continue({ rest : Headers }),
			Done({ rest : Headers }),
		],
		Headers.DecodeErr,
	)
	parse_record_field = |_, fields, state|
		parse_record_field_from_headers(fields, state.raw)

	skip_record_field : HeaderEncoding, Headers -> Try(Headers, Headers.DecodeErr)
	skip_record_field = |_, state| {
		parts = take_header_value(state.raw)?
		Ok({ raw: parts.after })
	}

	missing_record_field : HeaderEncoding, Str, Headers -> Headers.DecodeErr
	missing_record_field = |_, _, _| Headers.DecodeErr.MissingRequired

	missing_optional_field : HeaderEncoding, Str, Headers -> [Missing]
	missing_optional_field = |_, _, _| Missing
}
```

The exact derived parser type for a header record with mixed field shapes is:

```roc
{
	cache_control : Str,
	content_length : U64,
	x_auth_token : Try(Str, [Missing]),
}.parser_for : HeaderEncoding -> (Headers -> Try(
	{
		value : {
			cache_control : Str,
			content_length : U64,
			x_auth_token : Try(Str, [Missing]),
		},
		rest : Headers,
	},
	Headers.DecodeErr,
))
```

Because `HeaderEncoding` does not define `parse_tag_union`, trying to parse a
header record that contains a tag union is a compile-time static-dispatch error:

```roc
bad : Try({ mode : [On, Off] }, Headers.DecodeErr)
bad = Headers.parse("mode: On\r\n")
```

The missing requirement is `HeaderEncoding.parse_tag_union`; the compiler does
not wait until runtime to discover that this format does not support tags.

Concrete JSON parser code has this shape:

```roc
JsonState := [Input(Str)]

JsonEncoding :: [Default, CamelCase].{
	rename_field : JsonEncoding, Str -> Str
	rename_field = |encoding, name|
		match encoding {
			Default => name
			CamelCase => snake_to_camel(name)
		}

	parse_str : JsonEncoding, JsonState -> Try({ value : Str, rest : JsonState }, Json.DecodeErr)
	parse_record_field : JsonEncoding, Str.FieldName.FieldNames(_shape), JsonState -> Try(
		[
			Field({ field : Str.FieldName(_shape), rest : JsonState }),
			TryField({ name : Str, rest : JsonState }),
			TryFieldCaseless({ name : Str, rest : JsonState }),
			Continue({ rest : JsonState }),
			Done({ rest : JsonState }),
		],
		Json.DecodeErr,
	)
	skip_record_field : JsonEncoding, JsonState -> Try(JsonState, Json.DecodeErr)
	missing_record_field : JsonEncoding, Str, JsonState -> Json.DecodeErr
	missing_optional_field : JsonEncoding, Str, JsonState -> [Missing]
	parse_tag_union : JsonEncoding, ParseTagUnionSpec(a), JsonState -> Try({ value : a, rest : JsonState }, Json.DecodeErr)
}

Json :: [].{
	DecodeErr := [MissingRequired, InvalidJson].{}

	Token := { raw : Str }.{
		parser_for : JsonEncoding -> (JsonState -> Try({ value : Token, rest : JsonState }, Json.DecodeErr))
		parser_for = |encoding| |state| {
			parsed = JsonEncoding.parse_str(encoding, state)?
			Ok({ value: { raw: "custom-token" }, rest: parsed.rest })
		}
	}

	parse : Str -> Try(a, Json.DecodeErr)
		where [
			a.parser_for : JsonEncoding -> (JsonState -> Try({ value : a, rest : JsonState }, Json.DecodeErr)),
		]
	parse = |json| {
		Shape : a
		parse_shape = Shape.parser_for(JsonEncoding.Default)
		parsed = parse_shape(JsonState.Input(json))?

		match parsed.rest {
			Input(rest) =>
				if Str.is_empty(Str.trim_start(rest)) {
					Ok(parsed.value)
				} else {
					Err(Json.DecodeErr.InvalidJson)
				}
		}
	}

	parser_camel : () -> (Str -> Try(a, Json.DecodeErr))
		where [
			a.parser_for : JsonEncoding -> (JsonState -> Try({ value : a, rest : JsonState }, Json.DecodeErr)),
		]
	parser_camel = || {
		Shape : a
		parse_shape = Shape.parser_for(JsonEncoding.CamelCase)

		|json| {
			parsed = parse_shape(JsonState.Input(json))?

			match parsed.rest {
				Input(rest) =>
					if Str.is_empty(Str.trim_start(rest)) {
						Ok(parsed.value)
					} else {
						Err(Json.DecodeErr.InvalidJson)
					}
			}
		}
	}
}
```

The exact derived parser type for a JSON record is:

```roc
{
	cache_control : Str,
	nested_record : { inner_value : Str },
	user_id : Str,
}.parser_for : JsonEncoding -> (JsonState -> Try(
	{
		value : {
			cache_control : Str,
			nested_record : { inner_value : Str },
			user_id : Str,
		},
		rest : JsonState,
	},
	Json.DecodeErr,
))
```

The exact derived parser type for an externally tagged JSON union is:

```roc
[Admin({ name : Str }), Guest].parser_for : JsonEncoding -> (JsonState -> Try(
	{
		value : [Admin({ name : Str }), Guest],
		rest : JsonState,
	},
	Json.DecodeErr,
))
```

With `JsonEncoding.Default`, this parses values like:

```json
{ "Admin": { "name": "Sam" } }
{ "Guest": {} }
```

A custom nominal type can define `parser_for` manually and remain polymorphic
over any encoding that supplies the methods it uses. This does not auto-derive
the nominal type; it is an ordinary method the user wrote:

```roc
Token := { raw : Str }.{
	parser_for : encoding -> (state -> Try({ value : Token, rest : state }, err))
		where [
			encoding.parse_str : encoding, state -> Try({ value : Str, rest : state }, err),
		]
	parser_for = |encoding| {
		Encoding : encoding

		|state| {
			parsed = Encoding.parse_str(encoding, state)?
			Ok({ value: Token.{ raw: parsed.value }, rest: parsed.rest })
		}
	}
}
```

An encoding type can also be the runtime state type. There is no requirement to
invent a separate `State` type if the format state naturally belongs in the
encoding value:

```roc
TinyText :: [Input(Str), Done].{
	rename_field : TinyText, Str -> Str
	rename_field = |_, name| name

	parse_str : TinyText, TinyText -> Try({ value : Str, rest : TinyText }, [MissingRequired])
	parse_str = |_, state|
		match state {
			Input(value) => Ok({ value, rest: Done })
			Done => Err(MissingRequired)
		}
}

parse_token : TinyText -> Try(Token, [MissingRequired])
parse_token = |input| {
	parse = Token.parser_for(input)
	parsed = parse(input)?
	Ok(parsed.value)
}
```

Encoding is symmetric. Structural `encode_to` methods call the format's output
methods for strings, records, tag unions, lists, and other shapes. A format's
output state owns whatever builder it needs. JSON encoding to `Str` allocates
the final string in the ordinary way, and JSON UTF-8 encoding produces
`List(U8)`. Formats whose serialization can fail express that through the same
inferred `Try` error type as parsing.

The public structural encode method has this exact shape:

```roc
value.encode_to : value, encoding -> (state -> Try(state, err))
```

For a concrete record, the compiler can derive:

```roc
{
	count : U64,
	foo_bar : Str,
}.encode_to : { count : U64, foo_bar : Str }, MyEncoding -> (MyEncoding -> Try(MyEncoding, MyErr))
```

The encoding type owns the output methods required by that shape:

```roc
MyEncoding :: [Out(Str)].{
	rename_field : MyEncoding, Str -> Str
	begin_record : MyEncoding -> Try(MyEncoding, MyErr)
	encode_record_field : Str, MyEncoding -> Try(MyEncoding, MyErr)
	end_record : MyEncoding -> Try(MyEncoding, MyErr)
	encode_str : Str, MyEncoding -> Try(MyEncoding, MyErr)
	encode_u64 : U64, MyEncoding -> Try(MyEncoding, MyErr)
}
```

### Compile-Time Literal Conversions

A numeric literal whose target type is a non-builtin nominal type converts
through that type's `from_numeral` method, and a string literal converts
through `from_quote` (receiving the literal's post-escape contents as `Str`).
Every such conversion with a concrete target type is a
compile-time root (`numeral_conversion` / `quote_conversion`), no matter
where the literal sits in the AST: checking finalization evaluates the raw
dispatch call, stores its `Try` result through `ConstStore`, unwraps `Ok` into
the literal's stored constant, and reports `Err(InvalidNumeral(msg))` /
`Err(BadQuotedBytes(msg))` as a checking problem carrying the implementation's
message. Runtime lowering restores the stored constant instead of emitting a
call. Conversions whose target type is still polymorphic (literals inside
generalized functions) keep the dispatch call per monomorphic specialization.

Unresolved literal-origin type variables default — numerals to `Dec`, quotes
to `Str`. Quote defaulting runs before numeric context resolution because a
still-flex string receiver blocks the method chains that give numeric literals
their context, and it also resolves generalized literal variables that no
instantiation can pin, which is the same resolution monomorphic specialization
would apply, made early enough for checking to resolve dependent dispatch.
Generalized function signatures are different: a still-flex literal-origin or
method-constrained argument in a polymorphic function is a valid contract, not a
def-site error. Checking must not validate those generalized constraints against
`Dec` or `Str` at the function definition. Each instantiated copy is defaulted
and validated only after call-site evidence has had a chance to pin it.

Literal patterns participate through the same machinery. A literal pattern on
a non-builtin number or string type carries a synthesized checked conversion
expression; match lowering binds the matched value and tests it against the
converted constant, dispatching to the type's `is_eq` method when it has one
and using derived `is_eq` otherwise — exactly mirroring `==`. Checking
attaches an `is_eq` constraint to the pattern's type so this lowering is
total. Literal patterns on builtin types keep their direct literal-pattern
encoding.

### String Interpolation

An interpolated string literal is its own CIR expression. It is not
desugared as receiver method-call syntax, because interpolation method
selection is owned by the expression result type, not by the first literal
segment. The interpolated expressions bind to locals in source order. Literal
segments are always builtin `Str` values, and the interpolation expression
passes the first segment plus an `Iter((interpolated, Str))` of the remaining
interpolated values paired with the literal segment that follows each one.

For an unsuffixed interpolation, checking gives the expression this type:

```roc
val where [
    val.from_interpolation : Str, Iter((_interpolated, Str)) -> val,
]
```

The static dispatch owner is `val`, the interpolation result type. If `val`
remains unconstrained, it defaults to `Str`, which selects:

```roc
Str.from_interpolation : Str, Iter((Str, Str)) -> Str
```

Types that want checked interpolation through `Try` implement their own
`from_interpolation` and rely on `Try` forwarding:

```roc
Try.from_interpolation : Str, Iter((interpolated, Str)) -> Try(ok, err)
    where [
        ok.from_interpolation : Str, Iter((interpolated, Str)) -> Try(ok, err),
    ]
```

For a suffixed interpolation such as `"a${x}b".Regex`, the suffix is not a
static-dispatch owner. It is a direct associated-function call to
`Regex.from_interpolation`; the function's argument types constrain the
literal segments and interpolated expressions, and the function's return type is
the type of the whole interpolation expression. Missing suffixed interpolation
functions are reported as missing associated functions on the resolved suffix
target.

Interpolation deliberately does not parameterize literal segments over an
arbitrary `literal` type with a `literal.from_quote` constraint. That design
would defer quoted-segment conversion errors until monomorphic specializations
are known. `roc check` must report all compile-time conversion errors without
monomorphizing the program, so interpolation segments use builtin `Str`
directly. Normal non-interpolated quoted literals still convert through
`from_quote` as described above.

## Shared Post-Check Model

Every post-check IR has a typed IR store:

```zig
pub const ExprId = enum(u32) { _ };
pub const PatId = enum(u32) { _ };
pub const StmtId = enum(u32) { _ };
pub const DefId = enum(u32) { _ };
pub const TypeId = enum(u32) { _ };

pub fn Span(comptime T: type) type {
    return extern struct {
        start: u32,
        len: u32,
    };
}
```

Type-store ownership is explicit at each stage boundary:

- Monotype IR owns the Monotype type store.
- Monotype Lifted IR uses the same Monotype type store because lifting does not
  change types.
- Lambda Solved IR owns a new type store with lambda-set variables.
- Lambda Mono owns a new type store with no function types.
- LIR owns committed layouts, not post-check type ids.

A later stage must not reinterpret an earlier stage's type ids unless the stage
contract says the type store is shared. When a stage changes type meaning, it
consumes the earlier program and produces a new program whose ids are meaningful
only in the new stage.

Symbols are stable compiler names for procedures, local binders, generated
functions, and compiler-created temporaries. A symbol is not a layout id, object
symbol, tag discriminant, or ABI identity.

Stage-local lookup tables are allowed when they are pure indices over data the
stage owns, such as:

- symbol to local environment
- source procedure to specialization record
- type id to already-lowered type id
- Lambda Mono type id to committed LIR layout id
- Lambda Mono procedure id to LIR procedure id

Those tables are not hidden checked-data side channels. They must not contain
data that are missing from the produced IR. If deleting a table would make it
impossible to understand what the output means, the table is an illicit
representation store and the design is wrong.

Stage-local algorithmic worklists are allowed for SCC traversal, unification,
pattern decision construction, layout graph traversal, and similar internal
algorithms. These worklists do not cross stage boundaries and do not output
checked data.

The only meaning-producing worklists in post-check compilation are stage-local
specialization queues. A specialization queue is driven by explicit calls or
roots discovered while lowering the previous stage. It is not a general
post-demand repair list.

## Monotype IR

Monotype IR is the first post-check typed representation. It keeps only the
expression and pattern forms that are meaningful after checking, and every
expression and pattern has a monomorphic type.

Monotype IR is produced from checked modules and explicit root requests. It
performs three jobs:

1. Clone and instantiate checked types into monomorphic type nodes.
2. Create exactly the monomorphic procedure/value specializations reachable
   from the requested roots.
3. Resolve checked static-dispatch plans to direct calls or checked structural
   equality before any source dispatch form can enter the output IR.

### Monotype Types

Monotype types contain no lambda sets.

```zig
const MonoType = union(enum) {
    primitive: Primitive,
    named: NamedType,
    func: Fn,
    record: Span(Field),
    tag_union: Span(Tag),
    tuple: Span(TypeId),
    list: TypeId,
    box: TypeId,
    erased: TypeDigest,
};

const CheckedModuleId = enum(u32) { _ };
const TypeDefId = enum(u32) { _ };
const TypeDigest = struct { bytes: [32]u8 };

const TypeDef = struct {
    module: CheckedModuleId,
    id: TypeDefId,
};

const Fn = struct {
    args: Span(TypeId),
    ret: TypeId,
};

const Field = struct {
    name: RecordFieldLabelId,
    ty: TypeId,
};

const Tag = struct {
    name: TagLabelId,
    payloads: Span(TypeId),
};

const NamedType = struct {
    def: TypeDef,
    kind: NamedKind,
    args: Span(TypeId),
    backing: ?NamedBacking,
};

const NamedBacking = struct {
    ty: TypeId,
    authority: BackingAuthority,
};

const NamedKind = enum {
    nominal,
    opaque,
    alias,
};

const BackingAuthority = union(enum) {
    inspectable,
    runtime_layout_only,
};

const DispatchOwnerHead = union(enum) {
    builtin: BuiltinOwner,
    type_def: TypeDef,
    none,
};
```

`DispatchOwnerHead` is not stored as a duplicate field on every type node. It is
the result of a total function over Monotype type content:

```zig
fn dispatchOwnerHead(types: *const TypeStore, ty: TypeId) DispatchOwnerHead
```

Builtin type content returns the corresponding builtin owner. `named` type
content returns `type_def` when the definition can own methods, even if
its runtime representation later uses a transparent backing. Anonymous
structural types and non-owning internal nodes return `none`.

Record fields and tag variants are stored in lexicographic order by name. Tag
payloads are stored in payload position order. The index of a field or tag
inside its span is the source row position; it is not a runtime discriminant
or layout slot. Runtime tag discriminants, payload layout, and field offsets are
not chosen until direct LIR lowering commits layouts.

Monotype IR does not need a separate row-finalization stage. Row closure,
nominal backing instantiation, and structural child ordering are completed
while constructing Monotype types from checked types. Numeric defaulting is
split by the checked `numeric_default_phase` data: checking defaults open
literals (the first candidate in the numeric default candidate order, `Dec` first, that
satisfies the literal's dispatch constraints), and Monotype commits only the
per-specialization residue of generalized literals.

### Monotype Instantiation

Monotype lowering is a specialization-time instantiation of checked type graphs.
This is the same core model as Cor/LSS: each reachable monomorphic
specialization starts with the checked function/value type graph, creates a
fresh stage-local instantiation for that specialization, constrains the root of
that instantiation to the requested monomorphic type, and lowers the body from
that constrained graph.

The long-term invariant is:

```text
one reachable specialization
  -> one Monotype instantiation context
  -> one cloned/constrained checked type graph
  -> one closed Monotype body
```

This is deliberately different from treating a checked expression id as a
globally reusable monomorphic expression. A checked expression belongs to the
checked module; a Monotype expression belongs to one concrete specialization of
that checked module. The same checked function template may therefore produce
many Monotype bodies, and the same checked nested lambda site may produce many
nested Monotype functions, each with a different monomorphic function type.

Each specialization owns an instantiation graph: union-find nodes with
explicit row-extension links, created by instantiating checked types on first
touch. Instantiation contexts cache nodes by `(checked module id, checked type
id)`. The address is the checked identity of the type variable/content in the
current specialization. It is not a structural digest, source name, runtime
layout, object symbol, or generated procedure id. Nodes begin unresolved. As
the specialization is lowered, explicit evidence from checked data unifies
those nodes:

- the requested root function/value type constrains the checked root type;
- lambda and closure expected function types constrain the nested function
  specialization they create;
- call arguments constrain the callee instantiation through the checked formal
  and actual type relation;
- call results constrain the callee return type and the caller result type;
- static-dispatch plans constrain dispatcher, callable, operand, and result
  types;
- numeric literals and checked numeric defaults constrain numeric type cells;
- named type uses constrain their declaration formals to the instantiated named
  arguments;
- pattern lowering constrains checked pattern types to the monomorphic value
  being matched.

Those constraints are not a fallback mechanism and are not best-effort
inference after checking. They are the Monotype-stage representation of checked
data that are already present in the checked module. If a required relation is
missing from checked output, the producer is incomplete and must be fixed.

Nominal instantiation relies on a stronger CheckedModule invariant than
"the same source name appears twice." A checked nominal declaration owns an
explicit declaration template:

- `formal_args` are the checked roots for the declaration header parameters;
- `backing` is the checked root for the declaration backing template;
- every rigid occurrence in the backing template that refers to a header
  parameter must point at the same checked root as that header formal.

This root identity is the long-term ideal because it makes nominal
instantiation dataflow explicit. `Codec(input, value)` does not require
Monotype, layout lowering, or a backend to rediscover that the `input` in
`run : input -> ...` is the first nominal parameter by reading source text or
matching display names. CheckedModule data stores that relation once, as
checked root identity. Monotype then constrains declaration formal roots to the
concrete named arguments for the current specialization and lowers the
declaration backing through those cells. The result is a backing type in which
every formal occurrence has the same monomorphic meaning as the named type
argument that instantiated it.

Monotype must use the declaration backing template for ordinary local nominal
declarations. For local declarations, the `backing` root on a nominal-use
payload is not the authority for declaration-template instantiation; the
declaration template is authoritative because it carries the formal roots that
connect header parameters to backing occurrences. For imported nominal
declarations, the current CheckedModule must contain the `backing` root that
`CheckedTypeProjector` writes on the nominal-use payload, so Monotype can
consume that root directly without reaching into the source module's
declaration template. Box payload capabilities remain separate explicit
representation authorities; their backing roots come from the capability entry
in checked module data instead of from declaration template lookup.

This solves two classes of bugs:

- generic nominal backings cannot accidentally swap, lose, or default one
  type parameter while the named node itself has the right arguments;
- post-check stages do not need syntax-name matching, declaration scanning, or
  layout inspection to recover how a named type's representation is
  instantiated.

The instantiation context must be the only owner of checked-type-to-Monotype
state inside a specialization. Later lowering code must ask the context for the
Monotype type of a checked type, or must add an explicit constraint to the
context. It must not recover types by scanning source syntax, comparing display
strings, deriving names, inspecting layouts, or using incidental expression
shape. It must also not attach a contextual monotype to a checked expression id
as if that checked expression were a reusable runtime value.

This distinction matters most for lambdas and closures. Expression-position
functions are checked templates. Lowering a lambda or closure at an expected
function type creates or reuses a nested Monotype function specialization keyed
by the checked nested site, the current function digest, the checked source
function type digest, and the monomorphic function type digest. The expected
function type is the root constraint for that nested specialization. It is not a
constraint on the parent expression id. This allows the same checked lambda site
to be specialized at multiple function types without corrupting the parent body
or depending on traversal order.

Structural equality follows the same rule. The checker has already established
that the operands are equality-compatible and has either emitted a dispatch plan
that permits derived `is_eq` to lower as structural equality or rewritten the
expression to an explicit structural equality node. Monotype lowering
constrains the two checked operand types to the same instantiation relation and
lowers both operands at that single Monotype operand type. It must not
independently lower the left and right operand types and then attempt to
reconcile the results. Independent operand lowering is order-sensitive: an
unconstrained operand can default to an uninhabited type before the other
operand provides evidence. A shared instantiated operand type preserves the
checked equality relation directly.

The reason this is the long-term design rather than a local implementation
detail is that it makes specialization, dispatch, lambda lowering, and equality
all obey the same ownership rule:

```text
checked stage owns meaning and relations
Monotype instantiation owns monomorphic type cells
later stages consume closed Monotype types only
```

That rule removes a class of bugs caused by contextual rebinding. In the old
failure mode, one traversal path could lower a checked type variable to an
empty tag union or one concrete function type, and a later traversal path could
encounter the same checked type under better evidence and try to assign a
different Monotype type. That is not a valid compiler state; it is evidence that
the stage was not lowering from one constrained specialization graph. The
instantiation model makes the intended data flow explicit, so the first
constraint and every later constraint meet in the same graph node before the
final Monotype body is emitted.

An unconstrained checked type variable that remains open after checking lowers
to the empty tag union in Monotype. This is not a default choice. It records the
invariant that no runtime value can be constructed at that type. Values such as `[]`
can still be represented as `List([ ])` because they contain no elements, and
code that would need an actual element value must have constrained the element
type earlier or must be unreachable at runtime.

During Monotype construction, an open checked variable is an unresolved graph
node carrying the variable's numeric and row defaults. Unification resolves it
when call-site arguments, expected lambda types, numeric literals, or checked
type relations provide concrete evidence; defaults apply only at
materialization. A Monotype is a materialized view of a solved node: it is
reserved at a stable id and its content is refilled in place when its node
gains evidence, so every holder of the id observes the solved type. This is
ordinary type solving inside one stage. Once Monotype IR is output, no
unresolved node remains reachable and no later stage may change a type.

A Monotype imported into another specialization's graph is a finished
snapshot, never a refreshable view: a specialization that needs more than its
requested type is a unification conflict, not a silent rewrite of another
specialization's final type. Procedure template body requests therefore defer
to the end of the requesting specialization, when its types are final and the
specialization key is stable. Nested functions are the exception: they share
the requester's graph, and an inferred local procedure's body pins signature
variables the requester's remaining body relies on, so nested bodies lower at
their request site.

Instantiation graph nodes are cached by the owning checked module id and the
exact checked type id. They are not cached by `TypeDigest`. A digest can
identify closed structural type content for specialization and comparison, but
it cannot distinguish two different open checked variables with the same shape.
Treating those variables as the same node is a compiler bug. Type digests are
alias-transparent and encode recursive back references, so structurally equal
types digest equally regardless of alias spelling or knot-tying ids.

Generated helper code for an empty tag union, such as an inspector requested
only because a container type mentions the empty tag union, has an unreachable
body. Reaching that helper means a runtime value of an uninhabited type existed,
which is a compiler or unsafe-runtime bug.

If Monotype lowering cannot construct a closed monomorphic type from checked
data, that is a compiler bug.

### Row, Nominal, Alias, And Opaque Authority

Monotype lowering is the sole owner of turning checked type data into closed
Monotype type nodes.

For records and tag unions, checking outputs the checked row ids and stored
spans. Record fields and tag variants use lexicographic order by name. Tag
payloads use payload position order. Monotype lowering copies those spans
directly. It does not sort by display text, declaration spelling, runtime
encoding, or incidental map iteration.

Nominal records additionally carry their declared field order as separate
explicit data, because their runtime layout follows declaration order rather
than the lexicographic row order (see Nominal Record Field Order). The
lexicographic row order remains the identity used for field-name resolution;
declared order feeds only layout. These stay two separate data.

For named types, checking outputs:

- the `TypeDef`
- whether the definition can own methods
- the checked type parameters
- the checked backing type and backing authority, when the compiler has a
  checked representation entry for this definition
- opacity/interface data controlling whether the backing may be inspected
  by Monotype or used only for runtime layout

Monotype lowering instantiates those data. It does not scan declarations to
rediscover a backing, owner, or field order. If a named type is opaque at the
current boundary, Monotype still preserves the named type node and therefore
the dispatch owner derivable from it. A `runtime_layout_only` backing may be
used by layout lowering to represent values, but it is not permission
for Monotype or static dispatch to inspect through the opaque boundary. If no
backing is present, any stage that needs the representation must consume a
separate explicit checked representation authority; it must not rediscover the
backing by scanning declarations.

This keeps static-dispatch ownership, source row order, and eventual runtime
layout as three separate data.

### Monotype Expressions

Monotype expressions preserve the post-check expression shape, not source syntax
that has already served checking.

```zig
const Expr = struct {
    ty: TypeId,
    data: ExprData,
};

const ExprData = union(enum) {
    var_: Symbol,
    int_lit: IntLiteral,
    frac_lit: FracLiteral,
    dec_lit: DecLiteral,
    str_lit: StringLiteralId,
    list: Span(ExprId),
    tuple: Span(ExprId),
    record: Span(FieldExpr),
    tag: TagExpr,
    nominal: ExprId,

    let_: struct { bind: TypedSymbol, body: ExprId, rest: ExprId },
    lambda: LambdaExpr,
    fn_def: FnDef,
    call_value: CallValue,
    call_proc: CallProc,
    low_level: LowLevelCall,

    match_: MatchExpr,
    if_: IfExpr,
    block: BlockExpr,
    loop_: LoopExpr,
    break_: ?ExprId,
    continue_: ContinueExpr,
    return_: ExprId,
    crash: StringLiteralId,
    dbg: ExprId,
    expect: ExprId,
};

const LoopExpr = struct {
    params: Span(TypedSymbol),
    initial: Span(ExprId),
    body: ExprId,
};

const ContinueExpr = struct {
    values: Span(ExprId),
};

const LambdaExpr = struct {
    args: Span(TypedSymbol),
    body: ExprId,
    source: FnTemplate,
};
```

`LoopExpr.params` and `LoopExpr.initial` have the same length. Each initial
value has the type of the corresponding parameter. Every `continue_` reached
from the loop body supplies exactly one value for every loop parameter, in the
same order and with the same types. Loops with no carried state use empty spans.
`break_` carries the loop result when the loop is value-producing and carries no
expression when the loop result is unit/control-only.

Monotype IR has no:

- source static-dispatch call
- source type-dispatch call
- source method-equality node
- source `for` node
- source row variable requiring closure
- uninstantiated checked type variable
- pending owner search

`FnDef` is the checked identity for a checked, imported, nested, hosted,
promoted, or checked-stage generated function. It does not contain a capture
record, closure layout, callable tag, erased ABI, or lowered call target.
Captures remain ordinary free variables until Monotype Lifted IR records them
on lifted function definitions.

### Monotype Specialization

Monotype specialization is root driven.

```zig
const MonoSpec = struct {
    proc_template: ProcTemplate,
    fn_ty: TypeId,
};
```

The Monotype builder starts from explicit root requests and checked procedure
templates. When lowering a call to a checked procedure template, it creates or
reuses the specialization keyed by the instantiated monomorphic function type.
The specialization queue continues until no reachable call requests a new
specialization.

Imported modules participate exactly like local modules: their checked modules
provide procedure templates, method registry entries, checked types,
and checked bodies. Module boundaries do not erase or hide callable information
from the final program.

The specialization key is intentionally based on checked callable identity plus
the requested Monotype function type, not on an incidental lowered body or
runtime layout. For top-level, imported, hosted, promoted, and platform-required
procedures, the checked callable identity is the checked procedure template. For
expression-position functions, the checked callable identity is the nested site
inside the checked owner template plus the current function digest. The
Monotype function type is part of the key because one checked callable can be
instantiated many times.

Creating a specialization performs root instantiation before body lowering:

```text
create fresh instantiation context
constrain checked source function type to requested Monotype function type
lower arguments and body through that context
emit a closed Monotype definition
```

Calls do not mutate the callee's checked module. A call creates or reuses a
callee specialization by constraining a fresh callee instantiation from the
caller's instantiated argument and result types. The caller and callee contexts
communicate only through explicit checked type relations and Monotype types.
This is why generic functions specialize predictably across module boundaries:
the checked body remains immutable, and every monomorphic specialization records
its own closed instantiation.

### Static Dispatch In Monotype

Static dispatch is resolved while producing Monotype IR.

For each checked dispatch plan in a body specialization:

1. Instantiate the checked dispatcher type into the current Monotype type
   store.
2. Instantiate the checked callable type into the current Monotype type store.
3. Lower every checked operand into the current body specialization.
4. Derive the concrete `MethodOwner` from the instantiated dispatcher type.
5. Look up `(MethodOwner, MethodNameId)` in the checked method registry.
6. Instantiate the target method callable type and verify it matches the plan's
   callable type.
7. Emit a direct `call_proc`, or emit structural equality when the checked plan
   explicitly permits it.

Any failure after checking is a compiler bug. Monotype lowering must not search
for possible owners by intersecting method registries or constraints.

Owner derivation is a type-content operation, not a registry operation.
Monotype type lowering preserves owner-bearing type identity in builtin and
named type nodes for every dispatcher type.

`type_def` covers named nominal, opaque, and alias definitions that can own
methods. Transparent backing representation is separate from this owner head.
Monotype lowering may later erase or reinterpret transparent wrappers for value
representation, but static dispatch reads the owner head before the dispatch
node is removed.

The owner algorithm is fixed:

1. Fully resolve the instantiated dispatcher type.
2. Compute its `DispatchOwnerHead` from Monotype type content.
3. If the head is `builtin`, use that builtin owner.
4. If the head is `type_def`, use that exact `TypeDef`.
5. If the head is `none` and the checked dispatch plan permits derived `is_eq`,
   emit structural equality.
6. Otherwise stop with a compiler invariant failure.

The algorithm never asks the method registry "which owners could match this
constraint?" The registry only answers exact lookups after the owner is known.

### Iterator `for`

Source `for` loops lower during Monotype construction. The output is ordinary
loop, match, and call structure with explicit loop-carried state.

Given:

```roc
for pattern in iterable {
    body
}
```

Monotype lowering emits the source-level meaning of:

```text
iterator = iterable.iter()
loop iterator:
    step = iterator.next()
    when step is
        Done -> break
        One item next_iterator ->
            match item with
                pattern -> body; continue next_iterator
        Skip { rest } ->
            continue rest
```

Monotype `loop_` carries named parameters and `continue_` supplies the next
values for those parameters. Iterator `for` uses one loop-carried parameter: the
current iterator value. There is no hidden assignment and no mutation-only loop
state.

The exact step tag names and payloads come from the checked/builtin `Iter`
definition and the monomorphic iterator type. The `.iter` and `.next` calls are
resolved through the same Monotype static-dispatch path described above.

A `Skip` carries only `rest`: it signals "advanced one position, produced no
item this step," which is what keeps adapters like `keep_if` non-recursive. A
plain source `for` loop binds nothing from it and simply continues with `rest`.

No `for` node exists after Monotype IR.

## Monotype Lifted IR

Monotype Lifted IR removes closures and local functions from expression
position. Its type store is the Monotype type store.

The expression language is intentionally close to Monotype IR, and the
implementation consumes Monotype expression storage in place. Expression,
pattern, statement, and side-array ids are preserved across the Monotype to
Monotype Lifted boundary. Patterns and statements are the same storage. Most
expressions are the same storage. Lifting rewrites only the expression variants
whose callable meaning changes:

- `lambda`, `def_ref`, and `fn_def` become `fn_ref`
- a direct-call callee changes from a Monotype function template to a lifted
  function id

This is a representation-sharing rule, not a license for later stages to accept
pre-lift callable forms. After lifting, a valid lifted program has no reachable
`lambda`, `def_ref`, `fn_def`, or template-callee `call_proc` expression. Those
variants may still exist in the shared Zig union because Monotype and Monotype
Lifted use one backing expression representation, but seeing one through the
Monotype Lifted API is a compiler bug.

The lifted stage output adds only the data that lifting owns:

- every function body is a top-level lifted definition
- each lifted function definition declares its capture symbols explicitly
- roots and layout requests refer to lifted function ids
- capture spans appended by lifting are stored in the shared typed-local side
  array

```zig
const LiftedDef = struct {
    symbol: Symbol,
    kind: LiftedDefKind,
};

const LiftedDefKind = union(enum) {
    fn: LiftedFn,
    value: ExprId,
    run: ExprId,
};

const LiftedFn = struct {
    args: Span(TypedSymbol),
    captures: Span(TypedSymbol),
    body: ExprId,
};
```

Function references remain ordinary values with function type. A function value
is not packed here. Captures are explicit metadata on the lifted function
definition; callable representation is not chosen until Lambda Mono.

The lifting pass owns free-variable analysis. It does not choose finite
callable representations, erased callable representations, closure object
layouts, or runtime tags.

Release builds must not allocate or fill a second expression, pattern,
statement, branch, field-expression, or span arena for Monotype Lifted. The
normal path may allocate lifted function metadata, capture spans, request
rewrites, and traversal scratch owned by lifting. Debug builds may materialize
the old copied lifted tree only as a verifier; the in-place lifted program
remains the source consumed by Lambda Solved and later stages.

## Lambda Solved IR

Lambda Solved IR introduces lambda sets into the stage-local type store and
solves callable flow.

This is where Roc intentionally follows Cor's data model for callable values:
callable representation information is type information in this post-check IR,
not a side representation store.

### Lambda Solved Types

```zig
const LambdaType = union(enum) {
    link: TypeVarId,
    unbound,
    forall,
    content: LambdaContent,
};

const LambdaContent = union(enum) {
    primitive: Primitive,
    record: Span(Field),
    tag_union: Span(Tag),
    tuple: Span(TypeVarId),
    list: TypeVarId,
    box: TypeVarId,
    named: NamedType,
    func: Fn,
    lambda_set: LambdaSet,
    erased: TypeDigest,
};

const Fn = struct {
    args: Span(TypeVarId),
    callable: TypeVarId,
    ret: TypeVarId,
};

const LambdaSet = struct {
    members: Span(LambdaSetMember),
};

const LambdaSetMember = struct {
    lambda: Symbol,
    captures: Span(Capture),
};

const Capture = struct {
    symbol: Symbol,
    ty: TypeVarId,
};
```

A function type has an explicit callable slot:

```text
args -- callable -- ret
```

The callable slot is either a finite lambda set or `erased`.

Finite lambda sets name the exact function symbols that may flow through a
function value and the exact captured values each symbol needs. Erased callable
types represent call sites that must use the erased callable ABI.

Function arity remains fixed. A multi-argument Roc function is represented by
one `func` node with all arguments in `args`. It is not represented as nested
unary functions unless the source type explicitly returns another function.
Lambda-set solving, erased callable ABI solving, and specialization identity all
use the full ordered argument list plus the result type.

### Lambda Solving

Lambda Solved IR keeps the Monotype Lifted expression storage and adds solved
type arrays beside it. Only the type store changes.

The solver:

- instantiates Monotype Lifted types into Lambda Solved type variables
- adds a fresh callable slot to every function type
- treats references to lifted function symbols as singleton lambda sets
- unifies callable slots through value flow and calls
- propagates erased callable requirements through the same type graph
- generalizes and instantiates polymorphic definitions
- solves recursive groups as groups, not by accidental declaration order

The solved type graph is the callable representation source of truth. There is
no descriptor replacement, no callable repointing, no post-demand payload
output, and no representation recovery later.

### Erased Callable Requirements

`erased` callable requirements are explicit data entering Lambda Solved IR.
They are not inferred from backend needs or recovered from runtime encodings.

The producers are:

- checked platform and hosted function declarations whose ABI requires erased
  function values
- exposed values whose public ABI requires erased callables
- checked builtin operations that are explicitly declared as erased-callable
  boundaries in `Builtin.roc` and the builtin method registry
- checked low-level operations whose signature explicitly contains an erased
  callable parameter or result
- checked root ABI metadata for values that will later be consumed by LirImage
  or glue, when that metadata explicitly names erased callable slots

Monotype lowering carries these requirements as typed checked annotations into
Lambda Solved IR. Lambda solving unifies them through the same function
`args/callable/ret` graph used for finite lambda sets. If a callable slot is
forced to `erased`, Lambda Mono lowering produces packed erased callable values
and indirect erased calls. If no explicit erased requirement reaches a callable
slot, finite lambda-set dispatch is used.

No ordinary source expression becomes erased because a later stage finds finite
dispatch inconvenient. Erasure is introduced only by one of the checked boundary
data above.

## Lambda Mono Decisions

Lambda Mono consumes Lambda Solved IR and chooses function-free callable,
procedure, capture, and type representation data. These decisions are explicit
stage output, but release builds do not store a full Lambda Mono expression,
pattern, or statement tree. The direct LIR builder consumes the Lambda Solved
lifted syntax together with Lambda Mono decision tables.

The Lambda Mono type store has no function type. Function values have already
become ordinary value representations:

- finite callable sets become generated tag-union values
- erased callables become packed erased callable values

```zig
const LambdaMonoType = union(enum) {
    primitive: Primitive,
    record: Span(Field),
    capture_record: Span(CaptureField),
    tag_union: Span(Tag),
    tuple: Span(TypeId),
    list: TypeId,
    box: TypeId,
    named: NamedType,
    callable: Span(FnVariant),
    erased_fn: ErasedFn,
};
```

A finite callable set is an ordinary generated tag union. Each lambda-set member
gets one generated tag. If the member captures values, the tag payload is a
generated record containing those captures. If it captures nothing, the tag is a
zero-payload variant.

The generated callable type carries the source member and the exact Lambda Mono
function target. The target is part of the type node. The LIR builder never
finds a function by scanning symbols or by rebuilding a specialization choice:

```zig
const FnVariant = struct {
    id: FnVariantId,
    source: Symbol,
    target: FnId,
    capture_record: ?TypeId,
};
```

`source` is the original lifted function symbol and is used only while lowering
a `fn_ref` expression into the correct callable variant. `target` is the exact
Lambda Mono function specialization to call for that variant. `capture_record`
is the exact payload type for finite callable values and the exact capture
argument type for erased callable entries.

When Lambda Mono lowers a function reference, it reads the capture span from the
Lambda Solved function value type at that expression site. It then builds a
capture record with those exact slots and stores it in the callable value. It
does not use the source function's own function type as a proxy for the
expression-site callable type.

The release-build Lambda Mono output contains only data that later stages must
consume explicitly:

- the function-free Lambda Mono type store
- queued function specializations keyed by exact lifted function id, solved
  function type, capture ABI, and capture shape
- callable variants, erased callable entries, capture-record types, and exact
  function targets
- root, layout, runtime-schema, and const-plan requests rewritten to Lambda Mono
  type and function ids
- per-function capture bindings used by direct LIR lowering

The output does not contain copies of lifted expressions, patterns, statements,
branches, field-expression spans, tuple spans, loop spans, or source statement
spans. When a lifted node is unchanged by Lambda Mono, direct LIR lowering reads
the original lifted node. When Lambda Mono changes behavior, direct LIR lowering
uses the explicit Lambda Mono decision associated with that expression, call,
function reference, captured local, or callable pattern.

This keeps Lambda Mono as a real compiler stage without making the normal
pipeline pay for a second syntax arena that mostly duplicates Monotype Lifted
IR.

### Logical Lambda Mono Expressions

```zig
const ExprData = union(enum) {
    var_: Symbol,
    unit,
    int_lit: IntLiteral,
    frac_lit: FracLiteral,
    dec_lit: DecLiteral,
    str_lit: StringLiteralId,
    list: Span(ExprId),
    tuple: Span(ExprId),
    record: Span(FieldExpr),
    tag: TagExpr,
    nominal: ExprId,

    let_: struct { bind: TypedSymbol, body: ExprId, rest: ExprId },
    direct_call: DirectCall,
    indirect_erased_call: IndirectErasedCall,
    packed_erased_fn: PackedErasedFn,
    low_level: LowLevelCall,

    match_: MatchExpr,
    if_: IfExpr,
    block: BlockExpr,
    loop_: LoopExpr,
    break_: ?ExprId,
    continue_: ContinueExpr,
    return_: ExprId,
    crash: StringLiteralId,
    dbg: ExprId,
    expect: ExprId,
};
```

The expression forms above define the logical Lambda Mono language. They do not
require release builds to store a contiguous `ExprData` array. The direct LIR
builder may synthesize one logical expression at a time while it lowers a
Lambda Solved lifted node. If it creates helper expressions for capture records,
callable payload patterns, or finite-call branch bodies, those helpers are
builder-local work data and must not become a stage boundary consumed by later
compiler stages.

Lambda Mono uses the same loop-carried `LoopExpr` and `ContinueExpr` shape as
Monotype. A pass that preserves loops must preserve explicit parameters,
initial values, and continue values. LIR lowering is the first stage allowed to
turn that state into concrete jumps, blocks, or backend-friendly loop control.

Logical Lambda Mono has no `call_value` node. A call through a finite lambda set is
lowered to a match over the generated callable tag union; each branch makes a
`direct_call` to the variant's `target`. A call through an erased callable
becomes `indirect_erased_call`.

Generated callable variants are stage-local ids created by Lambda Mono. The
runtime discriminant and variant slot are chosen later by LIR layout commitment
and then output explicitly in the LIR result.

Lambda Mono specialization is queued by exact lifted function id, solved
function type, callable ABI, and capture shape. The queue is driven only by
explicit callable flow in Lambda Solved IR. Each `FnVariant.target` names the
queued result directly, so later stages consume a direct function id instead of
looking up a symbol.

For a finite callable member with captures, the specialized function receives
the original Roc arguments followed by one compiler-created capture-record
argument. For an erased callable, the erased ABI contains the full ordered Roc
argument list and result layout. Neither path introduces currying or
partial-application wrappers.

Logical Lambda Mono has no generic conversion expression. Any operation that must
survive to statement lowering is represented by a concrete expression form
above. Differences that are only layout choices are handled by layout
selection while lowering those concrete expressions.

If the direct LIR builder sees that Lambda Mono decisions and the committed layouts
require incompatible representations for the same value, compilation has found a
compiler bug. The builder stops with an invariant failure. It must not invent a
conversion path, conversion table, wrapper, or reshaping step to continue.

Recursive boxing, list backing representation, transparent named-type
backing, and zero-sized representations are layout-lowering responsibilities
for ordinary concrete constructors, field reads, tag payload reads, calls, and
patterns. They are not modeled as conversion nodes. If the language exposes a
real runtime operation such as allocating or reading an explicit `Box`, that
operation must enter Lambda Mono as a named concrete expression or low-level
operation whose meaning is defined by its producer stage, not as an
after-the-result conversion.

## Direct LIR Lowering

LIR lowering consumes Lambda Solved lifted syntax plus Lambda Mono decision
tables directly. It is the only production path from Lambda Solved to LIR.

There is no separate stored layout IR. The Lambda Mono to LIR builder owns:

- a layout builder that interns and commits recursive layouts from
  Lambda Mono type nodes
- a procedure builder that maps Lambda Mono procedure ids to LIR procedure ids
- a local builder that allocates LIR locals from lifted binders plus Lambda Mono
  local, capture, and type decisions
- a pattern builder that consumes lifted patterns plus Lambda Mono callable
  pattern decisions and committed layouts, then emits LIR switches, joins, and
  bindings directly
- callable lowering that turns generated callable tag unions into ordinary LIR
  tag operations and erased callable values into explicit packed-erased-callable
  statements
- bool predicate creation from ordinary Bool tag-union layouts
- runtime value schema output from committed nominal layouts
- erased callable code map output from Lambda Mono callable/procedure data

These are builder responsibilities, not a separate meaning-carrying IR.

The builder may maintain temporary maps such as `TypeId -> layout.Idx`,
`LambdaMonoFnId -> LirProcSpecId`, `LiftedLocalId -> LirLocalId`, and
`LiftedExprId -> lowered logical expression` while lowering one function
specialization. These maps are caches of work the builder owns. They must not
contain checked data that are absent from Lambda Solved IR, Lambda Mono
decisions, or the LIR result.

Release builds must not allocate, fill, traverse, or validate a materialized
Lambda Mono expression, pattern, or statement tree. Release builds may allocate
only the Lambda Mono decision data needed by direct LIR lowering: function-free types,
function specializations, callable variants, capture records, root/layout/schema
requests, and builder-local scratch storage.

### Debug Lambda Mono Verification

Debug builds may additionally materialize the logical Lambda Mono tree for
verification. That tree is never an input to production lowering, never a
substitute result, and never a recovery path. The direct solved-to-LIR builder
always produces the LIR result first. The debug verifier then checks a
separately materialized Lambda Mono tree against the direct path.

The verifier must be guarded so that release builds pay nothing for it: no tree
allocation, no materialized Lambda Mono traversal, no verifier data structures,
and no old Lambda Mono-to-LIR run. The release branch must be compile-time dead
after Zig specializes the debug condition.

The debug verifier checks at least these explicit decisions:

- every direct function specialization has the same lifted function id, solved
  function type, capture ABI, capture span, capture record type, source
  metadata, argument list, and return type as the materialized Lambda Mono tree
- every finite callable type has the same variants, variant ids, source
  symbols, target functions, and capture payload types
- every erased callable type has the same entries, targets, source function
  digest, and capture payload types
- every function reference, direct call, value call, captured local access, and
  callable pattern uses the same target, payload, and capture binding decisions
- root, layout, runtime-schema, const-plan, and requested-layout outputs name
  the same checked ids and Lambda Mono types

The verifier may also lower the materialized Lambda Mono tree with the legacy
Lambda Mono-to-LIR builder and compare that LIR to the direct LIR result. This
comparison is a debug assertion only. A mismatch is a compiler bug. The compiler
must not continue by using the materialized Lambda Mono LIR.

### Direct Builder Internal Contracts

The direct LIR builder is one compiler stage, but its internal components have
explicit contracts so the stage does not become an implicit reconstruction layer:

- the layout builder consumes only Lambda Mono type nodes and emits committed
  LIR layouts plus explicit maps from checked ids to runtime encodings for
  direct-builder result data
- the procedure builder consumes only Lambda Mono function ids, root requests,
  and committed layouts, then emits LIR procedure ids and root metadata
- the local builder consumes lifted binder ids, Lambda Mono local types, capture
  bindings, and committed layouts, then emits LIR locals
- the pattern builder consumes lifted patterns, Lambda Mono callable patterns,
  and committed layouts, then emits LIR control flow
- callable lowering consumes generated callable type nodes and committed
  layouts, then emits ordinary tag operations or packed erased callable
  statements
- schema output consumes committed nominal layouts and checked
  nominal identities

No internal component may inspect source syntax, checked bodies, display names,
runtime bytes, backend symbols, or any data outside the direct-builder inputs.
Internal maps are work caches only. If an internal component needs data that
is not in Lambda Solved IR, Lambda Mono decisions, committed layouts, checked
identities explicitly passed to the builder, or the LIR result it is
constructing, the earlier stage contract is incomplete.
The direct builder must not invent conversion operations to repair a mismatch
between Lambda Mono decisions and committed layouts.

The direct builder returns one explicit output object:

```zig
const LirLowerOutput = struct {
    store: LirStore,
    layouts: LayoutStore,
    root_procs: Span(LirProcSpecId),
    root_metadata: Span(RootMetadata),
    requested_layouts: Span(RequestedLayout),
    runtime_schemas: RuntimeSchemaStore,
    fn_sets: Span(FnSet),
    erased_fns: Span(ErasedFns),
};
```

`store`, `layouts`, `root_procs`, and `root_metadata` are the normal LIR output
consumed by ARC and then by backends, the interpreter, and LirImage.
`requested_layouts` is for static data and provided data exports that asked for
layout decisions during the same lowering. `runtime_schemas` is for glue and
static data. `fn_sets` and `erased_fns` are temporary compile-time output
contexts used by `CheckedModuleBuilder` while storing function values in
`ConstStore`. Capture slots are stored inside the corresponding function
variant or erased-function entry.

The output owns all of these stores and spans. Consumers borrow the fields they
need and must not add their own side stores for the same data. `LirImage`
contains only the ARC-inserted LIR fields: `store`, `layouts`, `root_procs`,
platform entrypoints, and target usize.

### Layout Selection

Layout selection is the first stage that chooses runtime encodings:

- struct field order
- tag-union variant order and discriminants
- zero-sized representation
- boxed recursive slots
- list backing layout
- erased callable payload layout
- ABI-visible procedure argument and result layouts

Layout selection consumes Lambda Mono types and produces LIR layouts plus the
runtime schemas and function result data that later compile-time output,
static data export, and glue code need. Later stages consume those explicit
layouts, schemas, and function result data. They do not rediscover field order,
tag discriminants, callable member encodings, or erased callable payload shape.

When layout commitment assigns a runtime discriminant or field offset to a
generated function tag, the builder outputs the mapping from the stage-local
`FnVariantId`/`FnMember` to the runtime encoding in direct-builder result data
for `ConstStore` output and static data export. `LirImage` does not store
function runtime data. It contains only ARC-inserted LIR, committed layouts,
root proc ids, platform entrypoints, and target usize.

### Nominal Record Field Order

Structural record layout is order-insensitive: fields are sorted
lexicographically by name and then stably by descending alignment, so source
field order never affects memory. Nominal records instead lay out fields in
*declared* order, so a nominal record can be given the exact memory layout of a
chosen C struct and exchanged with a host with no per-field translation.

The padding invariant is unchanged: a committed struct never contains internal
alignment padding between fields. Because every layout's size is a multiple of
its alignment, descending-alignment order always satisfies this, and it is the
order structural records use. Declared order does not always satisfy it, so
nominal layout commit verifies the declared order and only repairs it when it
would introduce internal padding:

- Verify: walk fields in declared order; if every field is naturally aligned at
  its running offset, commit the declared order unchanged. This accepts
  hand-tuned layouts — including ones, as in many C structs, where a
  lower-alignment field validly precedes a higher-alignment one because earlier
  fields already advanced the offset to the needed boundary — without reordering
  them.
- Repair: when declared order would require padding, commit the no-padding order
  that is lexicographically closest to declared order. The longest valid
  declared prefix is kept, and at each forced break the earliest-declared field
  that still admits a no-padding completion is chosen. A completion always
  exists (descending-alignment witnesses one from offset zero), so repair is
  total and is never reported as an error: eliminating padding is the compiler's
  responsibility, not the programmer's.

Repair is a greedy walk with a memoized feasibility check. Feasibility depends
only on the running offset modulo the maximum field alignment and on the
multiset of remaining field shapes, of which there are few, so it is cheap and
needs no backtracking. Reordering never changes a struct's size or alignment —
every no-padding order shares both — so it only changes which field name lands
at which offset.

Nominal record declarations may contain unnamed fields, written `_` or
`_`-prefixed (`_reserved`). An unnamed field reserves the size of its type but
stores nothing, is not accessible, and imposes no alignment requirement on
itself (its bytes are uninitialized), which lets a declaration reproduce a C
struct's explicit padding without a dummy value to initialize. Layout treats
unnamed fields as alignment-one spacers, so they advance the offset by their
size yet repair may place them at any offset. They contribute their size but not
their alignment to the struct, so pure padding never inflates a struct's
alignment. Using an unnamed field in a structural record type is rejected during
canonicalization.

Declared field order is explicit data. Record rows are sorted lexicographically
by name at several stages (checking, Monotype row lowering, and Monotype
instantiation) because field-name resolution and digests depend on a single
fixed order, so the declared order is not recoverable from the lowered record
itself. Canonicalization preserves it — a nominal declaration's record
annotation keeps its fields in source order — and it is carried forward as a
datum on the nominal type, distinct from the (lexicographic) backing row, so
later stages consume it without rescanning declarations. The struct commit
applies it as the declared order described above; field-name resolution
continues to use the lexicographic row order, independent of the layout offset
map. The same datum is consumed by the interpreter's layout store, so all
backends agree.

### Pattern Lowering

Pattern decision construction is part of the direct LIR builder. It consumes
Lambda Mono patterns and committed layouts and emits LIR control flow. There is
no persisted pattern-decision IR.

### ARC

The direct LIR builder emits ownership-neutral LIR. ARC insertion runs after
LIR construction and emits explicit `incref`, `decref`, and `free` statements.
Each explicit RC statement carries the concrete RC helper selected by ARC.
Backends, the interpreter, and LirImage builders follow those statements
mechanically. The ARC algorithm is specified in ARC Borrow Inference below.

### Join-Parameter Scalarization

Between direct LIR lowering and ARC insertion, one normalization splits
struct-typed join parameters into per-field parameters when the parameter is
only ever read field-by-field and only ever initialized from single-use
struct literals. Each jump then passes the literal's operands directly and
the literal's build is deleted; field reads become local aliases. This is
required for refcounted loop state: without it, every jump pays a retain on
each refcounted field read whose wrapper dies at the jump, and ARC cannot
turn that into a move because the wrapper's release covers all fields at
once. After scalarization the state flows through pure alias chains that
borrow inference resolves to moves. Parameters with any whole-value use keep
their shape, and the pass iterates so nested wrappers dissolve.

## ARC Borrow Inference

ARC insertion computes a whole-program borrows-with-lifetimes solution over
ownership-neutral LIR, then emits explicit `incref`, `decref`, and `free`
statements from that solution. Roc's borrow inference system is based on
["Fully-Automatic Type Inference for Borrows with
Lifetimes"](https://theory.stanford.edu/~aiken/publications/papers/oopsla26.pdf)
by William Brandon, Benjamin Driscoll, Frank Dai, Jonathan Ragan-Kelley, Mae
Milano, and Alex Aiken (OOPSLA 2026). It adapts the paper's fully automatic
borrow inference for reference-counted pure functional programs, implemented in
the Morphic compiler, to Roc's statement-only LIR.

The motivation is RC traffic. With all-owned insertion, every non-final
occurrence of a refcounted value pays an atomic increment plus a matching
decrement, and read-heavy programs spend a large fraction of their runtime on
RC statements that a borrows-with-lifetimes typing proves unnecessary. Borrow
inference deletes those statements statically. It also keeps refcounts at 1
across read-only uses, which is what lets the runtime `refcount == 1` checks
in list and string operations mutate in place instead of copying.

The ARC stage contract does not change:

- input: ownership-neutral LIR containing no RC statements
- output: the same LIR statement language whose only ownership data is
  explicit RC statements carrying concrete RC helpers
- backends, the interpreter, and LirImage consume the output mechanically and
  make no ownership decisions
- no mode, lifetime, signature, or specialization table appears in checked
  modules, LirImage, or any consumer-visible structure; everything the solver
  computes is ARC-stage-local and is dropped when the stage finishes

Borrow inference runs after every other post-check transformation:
monomorphization, lifting, call-pattern specialization, lambda-set solving,
inlining decisions, and LIR lowering are all complete before solving starts.
This ordering is required, not incidental:

- inference attaches resources to refcounted positions of committed layouts,
  which exist only after LIR lowering commits them
- every specializing or restructuring pass changes which values exist and how
  calls are shaped, which invalidates an ownership solution; solving once,
  last, means the solution is never patched after a later transformation
- earlier specialization makes inference more precise: call-pattern
  specialization deletes refcounted aggregate intermediates outright and
  exposes per-position flow that one aggregate-typed parameter would hide

The dependency is one-directional. Upstream stages feed borrow inference;
the solution is consumed only by emission within the same ARC stage. No
earlier stage may consult, anticipate, or encode ownership decisions.

Borrow inference is not best-effort analysis. It is a least-fixed-point
computation over finite lattices: deterministic, total, and independent of
traversal order. Every mode and lifetime is the least solution of explicit
constraints generated from LIR statement structure, committed layouts, per-op
`RcEffect` data, and pinned ABI signatures. Every constraint system has a
solution, because the all-owned assignment satisfies all constraints; the
solver outputs the least one. There is no failure path and no recovery path.
An occurrence the solver leaves owned is emitted as a move or an `incref`,
exactly as all-owned insertion would emit it.

### Vocabulary

- `Resource`: one refcounted position of one local — the top-level value, or
  one nested rc position reachable through the local's committed layout.
- `Mode`: `borrowed` or `owned`. The mode lattice is `borrowed < owned`.
  A borrowed resource is an alias whose occurrences emit no RC statements. An
  owned resource is responsible for exactly one reference count: it is
  eventually moved exactly once or decremented exactly once on every path.
- `Lifetime`: a tree-shaped interval of one proc body recording, on each
  control-flow path, the last point at which a value must still be live.
  Lifetimes of values that flow through params and returns are summarized by
  lifetime variables in proc signatures.
- `RcSig`: the solved ownership signature of one proc — a mode for every
  refcounted param and return position, plus the lifetime relation between
  borrowed returns and the params they may borrow from.
- emission: the final walk that writes RC statements into statement chains.
  Emission consumes the solved modes and precise lifetimes; it makes no
  decisions of its own.

The paper's `dup` corresponds to LIR `incref`, its `drop` to `decref`/`free`,
and its moves to the absence of both at a final owned occurrence.

### Resources Over Layouts

A local participates in inference iff its layout contains refcounted data
(`layoutContainsRefcounted`). Each participating local owns one resource per
rc node reachable in its committed layout:

- the top-level value itself, when its layout is `str`, `list`, `list_of_zst`,
  `box`, `box_of_zst`, or `erased_callable`
- the element resource of a `list`
- the payload resource of a `box`
- one resource per refcounted field of a `struct_`
- one resource per refcounted payload position of each `tag_union` variant
- the captures resource of a `closure` / `erased_callable`

Rc positions are interned per `layout.Idx` as a stage-local place table. The
place graph is finite: committed layouts guard every recursive occurrence
behind a box (layout commit performs SCC analysis and materializes back-edges
as boxes), and a place path that re-enters a layout already on the path folds
into the earlier place. One place under a recursive box therefore stands for
every unrolled occurrence, which matches the typing rule below that nested
modes are uniform through an owning rc.

Nested resources carry two modes, following the paper's storage/access split:

- the storage mode is the mode the containing allocation stores at that
  position. Storage modes are equality-constrained along value flow: an
  `owned rc (borrowed rc t)` cannot exist, because dropping the outer rc to
  zero must be allowed to drop the inner rc. Newly created allocations store
  owned content, so in practice storage modes solve to owned everywhere; the
  constraint form is kept because it is what makes payload-read borrowing
  sound.
- the access mode records whether it is safe for a payload read at that
  position to produce a borrow. It is solved from where the read result
  flows, exactly like a top-level occurrence mode.

Top-level resources carry one binding mode plus one occurrence mode per use
site.

### Lifetimes Over Statement Structure

A program point is a position in a proc's statement structure: one step per
statement along `next` chains, alternation at `switch_stmt` branches, and one
region per `join` body and per `join` remainder. `jump` statements create
flow edges between regions, including back edges for loops.

A lifetime is a tree over this structure, built from:

- the empty lifetime (resource never needs to be live)
- a point (one occurrence)
- sequential composition (ends in the bound statement vs. later in the chain)
- alternating composition over switch branches, including one-sided forms for
  values used in only some branches

Lifetimes within one proc form a finite lattice ordered by containment, with
a least-upper-bound operation taken pointwise over branches. Finiteness is
bounded by the proc's statement count and branching depth, which is what
guarantees fixpoint termination. Join regions and back edges do not get
special lifetime constructors: constraints flow between regions through join
parameter resources (below), and the lattice's finiteness makes iteration
over back edges converge.

Lifetimes that cross proc boundaries are not represented as trees. A proc's
borrowed param positions carry lifetime variables; a borrowed return position
carries a join of the param lifetime variables it may borrow from. Callers
instantiate those variables with caller-side lifetimes at each call site.

### Constraints Per Statement Form

Inference lifts each proc body once, assigning fresh resource variables, and
generates constraints per statement:

- `assign_literal` (str), `assign_list`, `assign_struct`, `assign_tag` with
  refcounted payload, `assign_packed_erased_fn`: the target's top-level
  resource is a newly created reference count, so its binding mode is owned.
  Operand occurrences that are stored into the new allocation must be owned
  at that occurrence (storage constraint).
- `assign_ref` with `.field`, `.tag_payload`, `.tag_payload_struct`: a payload
  read. The result may be a borrow of the source. The source must be live as
  long as the result is used (lifetime constraint), and the access mode of
  the source's nested position bounds the result's mode: if the access mode
  is owned, the read emits an `incref` on the result; if borrowed, it emits
  nothing.
- `assign_ref` with `.local`, `.list_reinterpret`, `.nominal`, and
  `set_local`: pure flow. The use resource and binding resource are related
  by flow constraints in both directions (see the equations below). A final
  owned occurrence becomes a move.
- `assign_call`: instantiate the callee's `RcSig`. A borrowed param position
  constrains the argument to be live across the call and emits nothing. An
  owned param position consumes the argument occurrence: a move when the
  occurrence is final, an `incref` otherwise. A borrowed return position
  constrains the result's lifetime to the join of the lifetimes of the
  arguments it may borrow from; an owned return is a fresh owned resource.
- `assign_call_erased`: the erased-callable ABI is a pinned all-owned
  `RcSig`: refcounted args owned, captures owned by the callee, result owned.
  Inference does not flow modes through erased callable values.
- `assign_low_level`: constraints come from the op's `RcEffect`. Args in
  `consume_args` are owned occurrences. Args outside `consume_args` are
  borrowed occurrences whose lender must be live at the call. Args in
  `retain_args` are stored by the op, so the stored value's storage
  constraint applies. A new mask, `result_borrows_args`, names the args the
  result may alias without owning (for example `list_get_unsafe` results
  borrow arg 0); the result's mode is then solved like a payload read, with
  the lifetime constraint tied to those args. Ops whose results never alias
  a retained arg produce fresh owned results as today.
- `join` / `jump`: each join parameter's resources get modes and lifetime
  relations like an intra-proc signature. `set_local` with
  `initialize_join_param` followed by `jump` is a flow edge from the
  jump-site resource into the join-param resource. Back edges contribute the
  same constraints; the fixpoint handles them.
- `ret`: flow into the proc's `RcSig` return position.
- `expect`, `debug`: borrowed reads.
- `crash`, `runtime_error`: terminal; every live owned resource is dropped on
  that path by emission.
- `incref` / `decref` / `free` in the input: a compiler bug (the input
  contract is RC-free LIR), enforced by a debug assertion.

Ownership demands propagate transitively through pure same-value aliases
(`.local`, `.list_reinterpret`, `.nominal`): a consumed alias is a consumed
source, so the chain's single unit moves link by link to the consuming
occurrence instead of the alias paying a retain while the source's unit is
separately released. Payload reads do not propagate demands; borrowing the
container is exactly the win there.

The solver runs three equation groups to their least fixed points, in order,
following the paper's Figure 8 adapted to LIR vocabulary:

```text
approximate lifetimes (escape analysis, pessimistically deep):
  ltApprox(bind) >= if flow(bind, use) then ltApprox(use)

modes:
  access(bind) >= if flow(bind, use) then access(use)
  access(use)  >= if flow(bind, use)
                  and ltApprox(use) escapes scope(bind)
                  then access(bind)
  storage(bind) = storage(use) along flow
  access(r) = owned for every pinned-owned position

precise lifetimes (exact, given solved modes):
  lateralFlow(bind, use) <= flow(bind, use) and use is owned
  verticalFlow(parent, result) <= payload read of an owned position
                                  whose result stays borrowed
  ltPrec(a) >= if lateralFlow(a, b) or verticalFlow(a, b) then ltPrec(b)
```

Approximate lifetimes deliberately over-extend through nested rc positions so
that escape decisions are sound before modes exist. Precise lifetimes are
recomputed after modes are fixed and are the only lifetimes emission may use
for placing `decref` statements; approximate lifetimes are not sound for drop
placement and must not reach emission.

### Pinned Signatures

Some signatures are ABI contracts, not inference results. They are pinned
before solving and never weakened:

- root procs (`runtime_entrypoint`, `provided_export`,
  `platform_required_binding`, `hosted_export`, `test_expect`, `repl_expr`,
  `dev_expr`, and compile-time roots): every refcounted param owned on entry,
  every refcounted return position owned. This is the existing host ABI rule.
- hosted procs: every refcounted arg owned by the host, result owned. This
  keeps the LirImage And Hosted Functions contract unchanged.
- erased-callable procs (`ProcAbi.erased_callable`): all-owned, as above.
- low-level ops: their `RcEffect` is the signature; it is explicit static
  data on the op, never inferred.

### Interprocedural Solving

The proc call graph is derived from `assign_call` statements over
`LirStore.getProcSpecs()`. Signatures solve in two phases:

1. Parameter modes iterate globally to a fixpoint with returns treated as
   owned: non-pinned refcounted parameter positions start borrowed and flip
   to owned when any occurrence demands a unit under the current signatures.
   The borrowed set only shrinks, so iteration terminates.
2. With parameter modes final, a return becomes borrowed when every `ret`
   in the proc returns a borrow anchored on a borrowed parameter of that
   proc, with the parameter positions recorded as the return's lenders. A
   final binding solve then lets callers borrow such results: a call result
   whose lender mask names exactly one refcounted argument is borrow-capable
   in the caller, anchored on that argument.

Borrowed parameters anchor borrow groups of their own: they are live for the
whole call by ABI, so payload reads from them borrow without the callee
emitting any release for the group.

Tail calls need one rule so that borrow inference never blocks backend
tail-call lowering. LIR has no tail-call statement; a call is in tail
position when the next statement returns the call result. Call-graph SCCs
(computed once, iteratively) feed exactly this rule: a tail-position call to
a proc in the same SCC demands ownership of its refcounted arguments, so
emission never places a release after the call on that path. Calls that
leave the SCC keep borrowed positions, since the caller's drops precede the
tail call there only when the values genuinely die earlier.

### RC Statement Emission

Emission walks each proc once, consuming solved modes and precise lifetimes,
and rebuilds statement chains with the same insertion machinery used today:

- borrowed occurrence: no statements.
- owned occurrence that is not the final occurrence on its path: `incref`
  before the consuming statement. Adjacent increments of the same local
  coalesce into the `count` field.
- owned final occurrence: a move; no statements.
- owned binding whose precise lifetime ends without a move: `decref` at the
  earliest point its precise lifetime permits on each path. Early placement
  is required, not optional: it bounds liveness growth from borrowing and
  returns refcounts to 1 before later mutation points, preserving in-place
  mutation in the runtime uniqueness checks.
- owned binding that is never used: dropped immediately after creation.
- reassignable local write (`replace_existing`): the previous resource ends
  at the write (decremented unless moved), and the write starts a fresh
  resource. Borrows of the previous value cannot outlive the write; the
  scope-end constraint above forces such occurrences owned instead.
- caller-side adaptation at calls: passing an owned final occurrence to a
  borrowed param borrows it for the call and drops it at its precise
  lifetime end; needing an owned result from a borrowed return position
  emits one `incref` on the result.
- switch branches and join regions balance drops exactly as today: a value
  that dies in one branch and survives another is dropped on the dying
  branch.

Emission also emits `free` where it does today (intent marker for a value
the proc fully releases); `free` keeps its current meaning of decrement plus
deallocation with nested decrefs through the RC helper plan.

RC helper selection is unchanged: each emitted statement carries the helper
derived from the local's layout, and helper choice stays in this stage.

Emission decisions ask liveness questions with on-demand forward scans over
the ownership-neutral statement graph, the same shape the all-owned inserter
used, with more questions per statement (early drops check each refcounted
operand, and scans cover a binding's whole borrow group). If profiling ever
shows ARC insertion hot in compile times, the intended remedy is one
precomputed per-statement liveness table per proc consumed by the same
decision points — a mechanical swap that changes no decision — not weaker
scanning.

The debug borrow certifier deliberately spends more: it re-certifies join
bodies per distinct entry state and summarizes per statement for walk
deduplication. Release builds compile the certifier away entirely, so only
debug compiler builds pay, and any certifier slowness is fixed inside the
certifier, never by weakening what it checks.

### Mode Specialization

A proc's solved `RcSig` is the most-borrowed signature its body admits.
Callers can always adapt to it, but adaptation has a cost: passing an owned
value to a borrowed param keeps a caller-side drop that a move would have
deleted, and an owned use of a borrowed return pays an `incref`. Mode
specialization removes that adaptation cost by emitting one proc variant per
demanded mode vector.

A demand vector assigns each refcounted param position a mode at or above
the solved signature (pointwise more owned). Return positions are never
demanded: a borrowed return that the caller needs owned pays one retain, and
that retain costs the same whether it is emitted in the caller or inside an
owned-returning variant, so no variant exists to save it. Specialization is
a worklist keyed by `(proc, demand vector)`:

1. Every proc is emitted once at its solved signature (the base variant).
2. While emitting any proc, each `assign_call` site upgrades a borrowed
   position to an owned demand exactly when the argument is an owned final
   occurrence there: the upgrade turns a borrow-plus-later-drop into a move.
3. The call site targets the `(callee, vector)` variant, creating it if new
   and re-emitting it from the callee's ownership-neutral body under the
   demanded vector. Inside the variant, demanded positions override the
   solved borrowed binding to owned, and everything else solves identically.
4. The variant table is keyed by vector content, so identical demands share
   one variant deterministically, independent of discovery order.

Variant bodies are cloned with the existing statement-cloning machinery and
added with `LirStore.addProcSpec`. Root procs are never specialized; their
vectors are pinned. The variant count is bounded by realized demand vectors,
not by the theoretical vector space.

A build without mode specialization is the same worklist with every demand
vector forced to the solved `RcSig`, which yields exactly one variant per
proc. Dev builds (`--opt=dev`) and compile-time evaluation use that
single-variant form, because solving is the only new compile-time cost they
accept. `--opt=speed` and `--opt=size` both enable full specialization;
specialization clones proc bodies, but each variant carries fewer RC
statements, and variant counts are bounded by realized demand vectors. All
forms run the identical solver; they differ only in which demand vectors get
a variant, so build modes can never disagree about observable program
results — only RC statement placement and proc count differ.

### In-Place Mutation Interaction

Ops with `may_runtime_uniqueness_check_args` mutate in place when the
checked argument's refcount is 1. Borrow inference helps these checks
succeed by deleting increfs that would otherwise hold refcounts above 1
during read phases, and early drop placement returns counts to 1 before
mutation points.

One interaction is accepted and documented rather than solved here: a borrow
whose lifetime extends past a uniqueness-checked mutation of its lender's
allocation forces the runtime copy path for that mutation. The solution is
still sound and still RC-minimal under the constraint system; it is the
constraint system itself that does not yet weigh mutation points. Extending
the flow analysis to account for `may_runtime_uniqueness_check_args`
positions when choosing between a borrow and an owned move is future design
work and must be added to the equations, not patched in emission.

### Debug Borrow Certifier

Inference is implemented as a solver plus an independent certifier, because
RC misplacement is memory unsafety. Debug builds re-check every emitted proc
against the borrow typing rules:

- every owned resource is moved exactly once or decremented exactly once on
  every path, and never used after its move or drop
- every borrowed occurrence's lender is provably live at that point: the
  borrow's lifetime is contained in the lender's
- every join body holds under the entry state of each jump that reaches it:
  jump states are summarized over the names the body relies on (liveness,
  unit counts, alias partition, and borrow anchors), and the body is
  certified once per distinct summary, exactly as shared switch suffixes are
  re-walked per distinct inflowing state
- every call site satisfies the callee variant's signature, and every pinned
  signature holds

The certifier consumes only the emitted LIR and the stage-local signature
table. A certifier failure is a compiler bug and stops compilation. Release
builds compile the certifier away entirely, like every other debug-only
boundary check.

### Thread-Confined Reference Counts

Reference counts are atomic today because the host may share a Roc value
across threads. Roc code itself is single-threaded within one host call, so
an allocation needs atomic count updates only if a handle to it is ever
visible to the host: it flows into a hosted call, a root return, an erased
or address-escaped boundary — or it originated from one, as a root
parameter, a hosted-call result, or a payload read out of a host-visible
container. Every other allocation is confined to one thread for its whole
life, and its counts may use plain loads and stores.

Atomicity is a property of the allocation but is chosen per RC statement,
so every statement that can touch one allocation must agree. Agreement is
guaranteed by construction: host visibility is a may-property propagated to
a fixpoint over the complete value-flow graph, and two locals can only hold
the same allocation if a chain of those same flow edges connects them, so a
visible allocation marks every local that can hold it.

The analysis is one more monotone bit per local in the ARC solver, over
edges the solver already walks:

- seeds: parameters and returns of pinned procs (roots, hosted procs,
  erased-callable procs, procs whose address escapes)
- pure same-value aliases, in both directions
- containment, in both directions: aggregate and capture operands link to
  the constructed value, and payload reads link to their source — storing a
  visible value makes the container visible, and anything read out of a
  visible container is visible
- direct-call argument-to-parameter and return-to-result relations
- low-level ops, from explicit `RcEffect` data

Bidirectional containment keeps every reachable-value tree uniformly
visible or uniformly confined, so RC helper plans carry a single atomicity
flag rather than per-level flags.

`RcEffect` gains one more explicit mask, `result_shares_args`: the result
may contain handles into these arguments' allocations. Unit-accounting
masks already imply sharing for many ops (`result_aliases_consumed_args`,
`result_borrows_args`, `retain_args` all contribute edges directly), but
unit accounting does not describe handle sharing in general: `str_split_on`
allocates a fresh owned list whose string elements are seamless slices into
the argument's allocation, and the byte/string conversions and
prefix/suffix slicing ops are the same. Those ops set `result_shares_args`
explicitly. A refcounted result of an op whose masks say nothing receives a
conservative edge to every refcounted argument in both directions: visible
spreads further than strictly necessary, which only keeps counts atomic
that could have been plain, never the reverse. The mask is explicit
primitive data, exactly like the rest of `RcEffect`; the analysis never
guesses an op's sharing from its name or shape.

Emission attaches the chosen atomicity to each `incref`, `decref`, and
`free` statement as explicit data; backends and the interpreter follow it
mechanically, and helper plans are selected by op, layout, and atomicity.
The runtime builtins already contain both count-update families. Atomic is
always sound, so the analysis only downgrades allocations it proves
confined, and an all-atomic answer reproduces today's behavior exactly.

Beyond cheaper count updates, confinement feeds the optimizer: atomic
operations are opaque to LLVM, but plain count updates participate in its
redundancy elimination, so residual paired increments and decrements that
ownership solving legitimately cannot remove become foldable downstream.
Confined data is also where `refcount == 1` in-place mutation hits most,
and its uniqueness check gets cheaper.

The debug certifier mirrors the analysis with one more rule: no
single-thread RC statement may name a local that is flow-connected to a
host-visibility seed.

### Uniqueness Inference

Ops with `may_runtime_uniqueness_check_args` branch at runtime: when the
checked argument's count is 1 they mutate the allocation in place, and
otherwise they copy. Borrow inference already deletes the RC traffic that
would hold counts above 1 across read phases; uniqueness inference goes one
step further and deletes the check itself wherever the in-place path is the
only one reachable. The win per site is one count load and one branch, but
the sites are the mutation points of hot loops, and removing the branch
also lets LLVM optimize across what was an opaque control split.

A checked argument's check is deletable when three conditions hold at the
call:

- the value's outermost allocation was born unique in scope: an allocation
  statement, or the result of an op whose `RcEffect` marks its result
  unique
- its count is still 1 on every path from birth to the call: no surviving
  incref, no store into an aggregate, no owned use other than the call
  itself
- no borrow of it is live at the call, under the same lender/holder
  liveness rule the certifier evaluates

The first two conditions are one more monotone bit per local in the ARC
solver — born unique, destroyed by any flow edge that can add a holder —
over the same alias and call edges the solver already walks. The third is a
query against liveness state emission already maintains.

`RcEffect` gains one more explicit mask, `result_unique`: the result's
outermost allocation has count 1 on return. Mutating ops qualify on both of
their paths — in place keeps an allocation whose count was already 1, and
the copy path returns a fresh one — and so do the ops that always allocate
their outermost result, including the slicing ops whose inner elements
share (`result_shares_args` describes the inner sharing; uniqueness is a
property of the outermost allocation alone). As with the other masks, an op
without the mask contributes nothing and its results stay conservatively
non-unique; the analysis never guesses from an op's name or shape.

Interprocedurally, `RcSig` gains a unique bit on the return, solved
alongside `ret_mode` with the same pessimistic anchoring, and the mode
specialization demand vector gains a unique entry per owned parameter:
a call site that proves its dying argument unique may demand a variant
whose body elides the checks that parameter reaches. Dev and compile-time
builds stay single-variant and keep every runtime check, exactly as they
keep all-owned calls.

Emission lowers a uniqueness-checked op whose checked argument passes all
three conditions to the check-free entry of the builtin; helper plans are
selected by op, layout, atomicity, and uniqueness. The runtime check is
always sound, so the analysis only deletes checks it proves redundant, and
an all-checked answer reproduces today's behavior exactly.

The debug certifier mirrors the analysis with one more rule: at every
check-free mutation site, the checked value's unit balance is exactly 1,
its origin chain reaches a unique birth, and no borrow of it is live.

This sharpens the interaction documented under In-Place Mutation
Interaction: once the constraint system weighs mutation points when
choosing between a borrow and an owned move, the choice that keeps a
mutation check-free becomes visible to the solver rather than a lucky
outcome of emission order.

### In-Place List.map

`List.map` may overwrite a uniquely owned input list's buffer instead of
allocating an output list when the input and output element layouts are
interchangeable in one allocation: same stride, same allocation alignment
class, and the same refcounted-elements header shape. The hidden header in
front of a list's data and the alignment handed to the allocator both
derive from the element layout, so reusing an allocation across layouts
that disagree on either would make a later free reconstruct the wrong
allocation pointer.

The decision has a compile-time half and a runtime half. `List.map`'s body
in Builtin.roc matches on the `list_map_can_reuse` primitive, whose runtime
meaning is "uniquely owned and not a seamless slice" — a slice's buffer
points into the middle of an allocation whose header bookkeeping covers the
whole allocation, so a unique slice still copies. At direct LIR lowering,
where layouts exist, the primitive lowers to a constant 0 whenever the
layouts are not interchangeable (or the optimization is off), so the
runtime check never runs for a pair it could corrupt.

The in-place branch itself is dropped before it reaches LIR whenever the
element layouts are not interchangeable or the optimization is disabled
(`TargetConfig.list_in_place_map`, on for `--opt=size`/`--opt=speed`, off
for dev, interpreter, and compile-time evaluation), so ineligible map
specializations never carry dead in-place machinery and dev builds lower
exactly the copy loop. The fold uses the same layout-eligibility decision
as the primitive, so every interchangeable pair — including different
types that share one layout — keeps the branch. The debug Lambda Mono
materializer runs before layout selection and cannot recompute that
decision; instead, direct lowering records each statically resolved match
site as explicit data and the verifier replays the record, so the two
derivations demand the same set of functions without the materializer ever
consulting layouts. A wrong record can only misplace dead code, never a
runtime check — the primitive's own lowering independently gates the
runtime path — and a fold regression surfaces as a Debug stride assertion
in the backends rather than as silent dead code.

Inside the in-place loop, `list_map_extract_unsafe` moves one element's
ownership out of the buffer and `list_map_write_unsafe` moves the
transform's result into the vacated slot. Neither performs RC work: the
extracted element is an ordinary owned local, so ARC places its release
according to the transform's solved convention, and the certifier checks
the loop like any other code. Between the two ops the slot holds stale
bytes and the buffer is typed by the output element while later slots still
hold input elements; this window is unobservable because no cleanup path
walks live values — `crash` is fatal and leaks by design — and the loop
itself is the only holder of the buffer (the runtime count of 1 proved
there were no other counted handles, and a live borrow of the list would
have forced the copy path through an owned capture's incref).

### Destination-Passing Results and Allocation Reuse

Large result values should be lowered by destination demand rather than by
building a temporary value and then copying it into its final storage. A
destination demand is explicit LIR producer input, not backend policy. It
describes where a result should be written and which existing allocation, if
any, may be reused when ARC proves or checks uniqueness. Backends, the
interpreter, and LirImage consume the resulting LIR mechanically.

The direct LIR builder may create a small bounded set of result variants for a
proc:

- `return_slot(T)`: write a by-memory result into caller-provided `ptr(T)`.
- `reuse_box(T)`: consume `Box(T)` and use its payload storage as the result
  destination when uniqueness permits.
- `reuse_erased_callable`: consume an erased callable allocation and overwrite
  its function pointer, drop callback, and capture bytes when uniqueness and
  payload layout permit.
- `append_into(Str)` / `append_into(List(T))`: build a returned string or list
  by appending into a caller-provided unique accumulator.

These variants are keyed by proc id, result demand, and committed layouts.
Identical keys share one variant. Root procs and ABI-pinned procs keep their
ordinary signature; wrappers may call an internal destination variant, but the
ABI-facing signature is not changed by this optimization.

`return_slot(T)` is selected by layout representation, not by source syntax.
Scalar, pointer, and zero-sized result layouts keep ordinary returns.
By-memory result layouts may be emitted as `proc(out: ptr(T), args...) -> {}`
inside the LIR program. Existing ABI lowering remains responsible for adapting
that internal shape to whatever a target ABI requires at roots and host
boundaries.

`reuse_box(T)` models the common shape:

```roc
Box.box(f(Box.unbox(boxed)))
```

as an allocation-reuse operation over one consumed `Box(T)`. The operation's
RC metadata consumes the box argument, may runtime-check its uniqueness, and
returns the same outer allocation on the reuse path. ARC may set the statement's
`unique_args` bit when the runtime check is proven redundant by the existing
born-unique and no-live-borrow rules. When the check is not redundant, the
runtime check remains. If the box is not unique at runtime, the operation takes
the defined copy path and returns a fresh box. The payload move, replacement,
and old-payload release are all explicit in LIR or in the low-level operation's
documented RC effect; no backend may infer them from `Box` names or pointer
shapes.

`reuse_erased_callable` is the erased-callable counterpart. Erased callables are
not ordinary `Box(T)` payloads; their allocation stores a callable entry, an
optional drop callback, and inline capture bytes. Reuse is allowed only when:

- the old erased callable is consumed and unique, or its runtime uniqueness
  check succeeds
- the new callable payload has the same committed payload size and alignment
  class as the old allocation, in the initial design
- the old capture payload is released by the old drop callback before the
  allocation is overwritten
- the new callable entry, new drop callback, and new capture bytes are written
  before the result is returned

The first implementation should require same-size/same-alignment erased
callable payloads. Broader reuse across different capture layouts requires an
explicit capacity or size input; it must not be guessed from the erased function
type alone, because an arbitrary `Box(a -> b)` value does not identify the
stored capture layout.

Destination-aware aggregate construction is required for the full benefit of
box reuse. A record update or tag construction whose result is demanded in a
slot should write fields and discriminants into that slot rather than first
forming a whole temporary aggregate. If the destination aliases a consumed
input, lowering must preserve read-before-overwrite order: fields needed later
are moved or copied to temporaries before their slots are overwritten, and every
refcounted field is moved, retained, or released exactly once. This ordering is
part of LIR construction and ARC emission, not backend cleanup.

Append destinations are result demands for producer functions that return
`Str` or `List(T)`. Under `append_into(Str)`, string literals, string slices,
string concatenations, and direct calls to append-capable producers write into
the supplied unique string accumulator. Any expression that cannot append
directly is first lowered to an ordinary result and then appended as an
explicit step. `append_into(List(T))` follows the same rule for list builders.
These variants are created only for realized demands and are keyed by result
kind and element layout, so specialization is bounded by the distinct demands
the program actually uses.

Each stage fully replaces the previous behavior when it lands; there are no
parallel insertion paths at any point:

1. Certifier first, checking the current all-owned insertion output.
2. Intraprocedural inference: borrows for locals, payload reads, and
   low-level ops (including `result_borrows_args`), with every proc `RcSig`
   pinned all-owned.
3. Interprocedural `RcSig` solving over call-graph SCCs, single variant per
   proc.
4. Mode specialization in optimized builds.
5. Thread-confined reference counts: the host-visibility analysis, the
   `result_shares_args` audit of the low-level op table, dual-mode RC
   statements and helper plans, and the certifier rule.
6. Uniqueness inference: the born-unique bit, the `result_unique` audit of
   the low-level op table, the unique entries in `RcSig` and the
   specialization demand vector, check-free helper plans, and the certifier
   rule.

## Compile-Time Constants

Compile-time constants use the same post-check pipeline as runtime code while a
checked module is being finalized:

```text
checked CIR
  -> CheckedModuleBuilder during checking finalization
  -> Monotype IR
  -> Monotype Lifted IR
  -> Lambda Solved IR
  -> Lambda Mono decisions
  -> LIR
  -> ARC insertion
  -> LIR interpreter
  -> store eval result in ConstStore
```

The compile-time evaluator is an LIR interpreter. It does not interpret
Monotype IR, Lambda Solved IR, logical Lambda Mono expressions, or any
source-level IR.

Compile-time ARC insertion runs the same borrow-inference solver as runtime
ARC insertion in its single-variant form: one proc per solved `RcSig`, no
mode specialization. Compile-time evaluation pays for solving once per
evaluated root and never for variant cloning.

The LIR interpreter produces a runtime value. Checking then stores that eval
result as checked-stage data in the checked module's `ConstStore`. `ConstStore`
stores checked Roc values only. It does not contain Monotype nodes, Lambda
Solved data, Lambda Mono decision data, runtime addresses, allocation identity,
layout ids, runtime discriminants, field offsets, LIR locals, LIR procedure
ids, backend symbols, backend bytes, or host handles.

`ConstStore` uses node ids so stored constants can preserve sharing without
duplicating large values. Multiple fields may reference the same `ConstNodeId`.
Stored constants are acyclic. Roc source cannot define recursive non-function
values; checking reports those definitions as errors and records `Malformed`
source nodes instead. `Malformed` source nodes are never output as valid
`ConstStore` values. A cycle in output `ConstStore` node edges is therefore
a compiler bug, not a supported stored-constant representation.

```zig
const ConstStore = struct {
    nodes: []const ConstValue,
    roots: []const ConstNodeId,
};

const ConstValue = union(enum) {
    scalar: ConstScalar,
    string: StringLiteralId,
    list: Span(ConstNodeId),
    tuple: Span(ConstNodeId),
    record: Span(ConstField),
    tag: ConstTag,
    box: ConstNodeId,
    declared: ConstDeclared,
    fn_: ConstFn,
};
```

`ConstScalar` is a closed checked scalar representation:

```zig
const ConstScalar = union(enum) {
    signed_int: struct { width: IntWidth, bits: u128 },
    unsigned_int: struct { width: IntWidth, bits: u128 },
    f32_bits: u32,
    f64_bits: u64,
    dec: DecBits,
};

const IntWidth = enum {
    @"8",
    @"16",
    @"32",
    @"64",
    @"128",
};
```

`ConstScalar` does not contain target-sized integers, raw pointers, opaque
pointers, host handles, layout ids, field offsets, runtime discriminants, or
backend symbols. `bits` stores the checked literal bits for the declared width;
consumers interpret it only through the checked type attached to the const root.
If Roc later exposes pointer-sized numeric values to compile-time evaluation,
that value kind must be added explicitly here with a checked cache rule.

`ConstStore` may contain:

- scalar literals
- strings, lists, records, tuples, tags, boxes, and nominals
- references to other stored const nodes
- function values

Compile-time evaluation failures are owned by checking finalization because the
module has not been output yet. User-written compile-time crashes, invalid
compile-time host interaction, and unsupported compile-time operations become
checking diagnostics attached to the checked root being finalized. OOM remains
OOM. A post-check invariant failure while lowering or interpreting a
compile-time root is still a compiler bug, not a user-facing diagnostic.

While storing an eval result, the builder may reserve a `ConstNodeId` before
storing its children so repeated references to the same acyclic runtime value
can reuse the same stored node. The builder verifies that every reserved node
was filled exactly once and that stored value edges are acyclic. Restoring
cached consts and dependency summarization must memoize by `ConstNodeId`, so
sharing is preserved and traversal is linear in the stored node count. A
consumer must not recover stored-const identity by comparing node contents.

A stored function value keeps checked identity only:

```zig
const ConstFn = struct {
    fn_def: FnDef,
    source_fn_ty: CheckedTypeId,
    captures: Span(ConstCapture),
};

const CaptureId = union(enum) {
    binder: PatternBinderId,
    generated: u32,
};

const ConstCapture = struct {
    id: CaptureId,
    value: ConstNodeId,
};
```

`fn_def` names a checked, imported, nested, hosted, promoted, or checked-stage
generated procedure template that the checked module owns or references
explicitly.
`captures` bind the exact capture identities required by that function to
stored const nodes. Source lambdas use checked pattern binders. Compiler-
generated functions whose captures have no source pattern, such as structural
parser runtime functions, use explicit generated capture ids assigned by the
generator. A stored function does not store a lambda set, callable-set
descriptor, call specialization id, erased ABI, capture layout, runtime tag, or
LIR proc id.

During compile-time evaluation, the direct LIR builder also produces temporary
function result-store data. Storing a function result is scoped by `FnSet`
identity, not by layout alone. Layouts may
collapse zero-sized or same-shaped function values; they are used only for
validation, discriminant reads, and capture extraction.

```zig
const FnResult = union(enum) {
    finite: FnSetId,
    erased: ErasedFnsId,
};

const FnSet = struct {
    layout: LayoutId,
    variants: Span(FnVariant),
};

const FnVariant = struct {
    id: FnVariantId,
    discriminant: RuntimeDiscriminant,
    variant_index: RuntimeVariantIndex,
    payload_layout: LayoutId,
    template: FnTemplate,
    captures: Span(CaptureSlot),
};

const ErasedFns = struct {
    layout: LayoutId,
    entries: Span(ErasedFn),
};

const ErasedFn = struct {
    entry: LirProcSpecId,
    template: FnTemplate,
    captures: Span(CaptureSlot),
};

const FnTemplate = struct {
    fn_def: FnDef,
    source_fn_ty: CheckedTypeId,
};

const CaptureSlot = struct {
    id: CaptureId,
    slot: u32,
};
```

`FnSetId` and `ErasedFnsId` are direct-builder result contexts produced while
lowering the specific value being evaluated. They live only for that lowering
and const storage step. They are not stored in `ConstStore`, not serialized
in checked modules, and not stored in `LirImage`. For a finite singleton set,
storing the result selects the only `FnVariant`. For a finite multi-variant set,
storing the result reads the runtime discriminant and looks it up inside that
`FnSet`. For erased functions, storing the result reads the erased entry
procedure from the runtime value and looks it up inside the explicit
`ErasedFns` context.

`CaptureSlot` says which committed capture-payload slot contains the value for
one captured identity. For source lambdas, the identity is the checked binder.
For generated functions, the identity is a generator-assigned capture id with a
documented role in that generated function kind. The direct LIR builder outputs
these slots while lowering the generated function value. The `ConstStore`
writer recursively stores each captured runtime value, then stores the
resulting `ConstFn`.

Storing an eval result never uses a global id made only of layout,
discriminant, variant slot, byte pattern, display name, object symbol, or
payload shape. The function result-store data is temporary and is not serialized
as a checked module representation cache.

When a later compilation restores a cached const, Monotype lowering turns
`ConstStore` nodes into ordinary Monotype expressions:

- scalar and aggregate nodes become literal, record, tuple, list, tag, box, and
  nominal expressions
- zero-capture function values become `FnDef` expressions
- capturing function values become compiler-generated Monotype lambda
  expressions by cloning the checked template body referenced by `function`,
  alpha-renaming its parameters, and binding each captured symbol to the
  ordinary Monotype expression restored from the corresponding captured
  `ConstNodeId`
- generated parser runtime functions restore through their explicit generated
  function kind: Monotype lowering recovers the checked static-dispatch plan,
  restores generated captures such as transformed field-name strings by their
  generated capture ids, and regenerates the runtime parser lambda directly
  around those restored constants

Restoring a cached const does not synthesize a wrapper that calls an
already-packed runtime function value. It builds an ordinary Monotype callable
from the explicit checked template and stored const captures, so the later
lifting and lambda-set solving stages see the same kind of ordinary callable
flow they would have seen if the value had been local source.

After that, closure lifting, lambda solving, lambda mono lowering, layout
commitment, and ARC run normally. This is what keeps module boundaries from
changing runtime performance: imported compile-time callables participate in the
same whole-program callable-flow solving as local callables.

Compile-time dependency summaries are produced from explicit checked root data
and `ConstStore` dependencies. They are not discovered by a later stage scanning
bodies for missing data.

## LirImage And Hosted Functions

Platform-hosted functions called through `RocOps.hosted_fns` receive ownership
of every refcounted argument. LIR ARC insertion transfers that ownership at the
hosted-call boundary the same way it transfers an unused argument to an
ordinary Roc callee. Backends must not add their own ownership decisions; they
only lower the explicit LIR `incref`, `decref`, and `free` statements.

The host may read, store, return, or release an owned argument. It must account
for that ownership explicitly:

```text
read and discard: decref the argument when done
store past return: move the argument ownership into storage, or incref a stored
                   copy and then decref the call argument
return the same value: move the argument ownership into the return slot, or
                       incref/decref so exactly one returned ownership remains
```

The compiler must not infer different ownership behavior from hosted function
names, return types, or body absence. Hosted-argument ownership is an ABI rule,
and generated glue must document it for platform authors.

## Build Outputs And The Targets Header

A platform's `targets:` header section declares, per target, both the link
inputs and the output kind the build produces. The application author never
chooses the output kind; `roc build` produces what the platform declares for
the selected target, and there is no `--no-link` style flag. `--target` and
`--output` (the output path) remain per-build choices.

```text
targets: {
    inputs_dir: "targets/",
    arm64mac: { inputs: ["libhost.a", app], output: Shared },
    x64glibc: { inputs: ["libhost.a", app], output: Exe },
    wasm32: { inputs: ["host.wasm", app], output: Shared },
}
```

Each target name appears at most once, so target-to-output-kind is a
function. `output:` is one of:

```text
Exe:     linked executable binary. For wasm32, a command module (has an
         entry).
Archive: one static archive (.a, .lib) containing the declared host inputs,
         the compiled app, and the builtins, with input archives flattened
         in. Archive keeps its inputs because the host must provide
         roc_alloc and the other runtime symbols; the consumer receives a
         single self-contained archive and performs the final link in their
         own build, which extracts members lazily by symbol reference.
Shared:  shared library (.so, .dylib, .dll). For wasm32, a reactor module:
         no entry, the provides entrypoints exported, optionally composable
         into a component with wit-component.
```

The default `roc` command requires the selected target's entry to be
`output: Exe`; library and object platforms report that the output must be
linked or loaded by a host application instead.

The output that static archives previously stood in for on wasm (a linked,
loadable, no-entry module) is `Shared`, not `Archive`; `Archive` is never a
linked module.

## Host Symbol ABI

Hosts and compiled Roc code share symbols resolved at link time; there is no
host-facing struct of function pointers. `RocOps` survives only as an
interpreter-internal structure (the dev-build translation shim and
compiler-internal evaluation construct one); it is not part of any host ABI,
and glue never emits it.

The platform header maps linker symbols explicitly, symbol-string first, in
both directions:

```text
provides { "roc_main": main_for_host! }
hosted { "roc_stdout_line": Stdout.line!, "roc_stderr_line": Stderr.line! }
```

Compiled Roc code references each hosted symbol (and the fixed runtime set:
roc_alloc, roc_dealloc, roc_realloc, roc_dbg, roc_expect_failed, roc_crashed)
as a weak extern and calls it directly with the natural C ABI for its types.
Entrypoints are exported under their `provides` strings with natural C ABI
signatures. No context pointer is threaded through compiled code: hosts that
need per-call context (for example an arena) own its delivery out of band
(global or thread-local state), and must establish it on every thread that
executes Roc code — including threads that invoke stored boxed Roc closures.
Generated glue exposes closure invocation through helpers that set and restore
that state so the contract is enforced by signatures rather than remembered.

Weak linkage exists to break the app/host reference cycle without imposing
link order; COFF has no equivalent weak external, and needs none: the app
object participates in the link directly while host archives are searched on
demand, so a single pass resolves the app's references into the host and the
host's references back into the app. Missing host symbols are diagnosed
before linking by scanning the host inputs' symbol tables, not by changing
how the linker resolves symbols.

Because the app references host symbols directly, host inputs are linked
without whole-archive wrapping, and section GC (--gc-sections, -dead_strip,
/opt:ref) removes host functions, host constants, and host helpers that the
application never reaches. This dead-code elimination is a guaranteed,
regression-tested property on every supported target, including wasm32: tests
must verify that unused host functions, unused host constants, and helpers
reachable only from unused host functions are absent from the final binary
(by symbol table inspection and by content-pattern absence), and present when
actually used.

Shared-library output uses the same symbol ABI: the host objects and app
object are linked into one library, app/host resolution happens inside that
link, and dead-strip roots are the exported symbols. Internal `roc_*` symbols
must be hidden in shared libraries — on ELF, default-visibility exports are
preemptible, and two Roc-built libraries loaded into one process would
otherwise interpose each other's runtime symbols.

Interpreter execution (the default `roc` command, embedded interpreter builds,
REPL, compile-time constants, glue evaluation) keeps the same host objects: a
generated translation shim defines the exported entrypoints, marshals their
natural C ABI arguments into interpreter calls, and fills the interpreter's
internal dispatch table with the extern host symbols' addresses. Hosted
dispatch order for that table is the `hosted` section's declaration order.

## Relationship To Cor LSS

The post-check design mirrors Cor's LSS experiment after solving, adapted for
Roc's checked module boundary and existing LIR.

| Cor LSS stage | Roc stage |
| --- | --- |
| solved source IR | checked CIR plus checked type store |
| `monotype` | Monotype IR |
| `monotype_lifted` | Monotype Lifted IR |
| `lambdasolved` | Lambda Solved IR |
| `lambdamono` | Lambda Mono decisions |
| `ir` | direct Lambda Mono to LIR builder |
| `eval` | LIR interpreter for compile-time evaluation |

Roc intentionally keeps Cor's post-solve shape:

- Monotype IR is closed, monomorphic typed IR.
- Monotype Lifted IR has top-level lifted functions and explicit captures.
- Lambda Solved IR stores callable flow in function types.
- Lambda Mono removes function types by turning finite function values into
  ordinary generated tag unions and erased function values into packed erased
  callables.

Roc adds language and implementation data that Cor's experiment does not need:

- static dispatch and method registries
- checked module caches and imported checked bodies
- opaque, nominal, alias, builtin, platform, hosted, and exposed identities
- target-independent `ConstStore` values
- the existing statement-only LIR, ARC, LirImage, and backend boundaries

The main language difference is static dispatch. Roc keeps static dispatch
separate from checked types. Checking still reports every user-facing
static-dispatch error and outputs checked dispatch plans. Monotype IR lowering
uses those plans plus monomorphic type information to replace static dispatch
with direct calls before any later callable/lambda stage runs.

Lambda sets are not stored in the checked type store or checked cache.
They are introduced after Monotype Lifted IR, during Lambda Solved IR, exactly
where callable value flow is being solved for the current whole-program root.

Cor's experiment uses unary function types internally. Roc does not. Every pass
that corresponds to Cor's callable pipeline is generalized over the full ordered
argument list:

- Lambda Solved function types are `args/callable/ret`, not nested unary
  `arg/callable/ret` chains.
- Lambda-set unification connects the one callable slot for the whole function
  value.
- Lambda Mono direct calls pass the full argument list, plus a compiler-created
  capture record argument only when the selected finite member has captures.
- Erased callable ABIs contain the full ordered argument list and result layout.
- Specialization identity includes the full function type, not just one argument
  and a nested return function.

Cor's final `ir` is a post-lambda IR stored as a distinct data structure. Roc
does not keep that as a separate persisted stage because the existing LIR
boundary already serves the consumer side. Roc's direct LIR builder corresponds
to Cor's final lowering work, with explicit internal contracts instead of a
serialized Layout IR or extra middle layer.

Cor's experiment also performs some final field and tag lookup by source label
inside its final lowering. Roc does not copy that part. Roc's checked and
post-check stages output ordered spans and checked ids before direct LIR
lowering. Direct LIR lowering consumes those ids and span positions; it does not
look up record fields or tag variants by display label to recover missing row
relationships.

## Forbidden Shapes

The post-check pipeline must not contain:

- MIR as a separate compiler layer
- a persisted layout IR between Lambda Mono and LIR
- post-demand worklists
- alternate post-check lowering paths
- comparing against another lowering path to decide compiler behavior
- callable descriptor replacement
- callable value repointing
- late payload output
- generic conversion expressions, post-hoc conversion plan tables, or mismatch
  patching lowering paths
- checked-module runtime payloads, value conversion plans, callable-set
  descriptors, or erased ABI decisions
- owner discovery by method-registry intersection
- backend reference-counting decisions
- mode, lifetime, or RC-signature data stored in checked modules, LirImage,
  or any structure that outlives ARC insertion
- user-facing errors after checked module output
- release-build checks whose only purpose is maintaining compiler invariants

The allowed replacement is explicit stage ownership:

- checking owns user-facing diagnostics and checked data
- Monotype owns monomorphic specialization and static-dispatch elimination
- Monotype Lifted owns closure lifting
- Lambda Solved owns callable flow in the type graph
- Lambda Mono owns explicit callable value representation
- LIR lowering owns committed layouts and statement lowering
- ARC owns borrow inference, mode specialization, and reference-count
  insertion
- backends own only backend code generation from explicit LIR

## Debug Invariants

Every stage boundary has debug-only verification. In release builds, invariant
checks must compile away to nothing or to `unreachable` after inlining.

Minimum boundary checks:

- Monotype IR contains no checked static-dispatch, method-equality, type
  dispatch, or source `for` nodes.
- Monotype IR contains only closed monomorphic types.
- Monotype IR contains no runtime tag discriminants, layout ids, or callable
  representation ids.
- Monotype Lifted IR contains no reachable closure expressions, local function
  definitions in expression position, definition references in expression
  position, or direct calls whose callee is still a Monotype function template.
- Lambda Solved IR has every function type in `args/callable/ret` form.
- Lambda Solved IR has no unresolved callable slot before Lambda Mono lowering.
- Lambda Mono decisions contain no function type and no value-call node.
- Lambda Mono decisions contain no unresolved lambda set.
- Lambda Mono decisions contain no runtime tag discriminants or layout ids.
- Checked compile-time stores contain only `ConstStore` data.
- LIR lowering receives only Lambda Solved lifted syntax plus Lambda Mono
  decisions.
- ARC insertion receives LIR containing no RC statements.
- ARC output passes the debug borrow certifier.
- Backends receive only ARC-complete LIR.

If a boundary check fails, the compiler stops as a compiler bug.
