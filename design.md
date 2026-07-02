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

Identity provenance follows meaning provenance. An identity may be derived
from module content — the module name, the source bytes, and the identities
of imports — only for definitions whose entire meaning is determined by that
content. A binding whose meaning is partly supplied from outside the compiled
program is identified by its role at that outside boundary instead. Hosted
functions receive their implementations from the host, and `provides`
entrypoints are called by the host, both keyed by the symbol strings in the
platform header; their identities are those symbol strings and declaration
slots, never a content hash of a declaring module. Two hosted declarations
are two distinct identities even when their declaring modules are
byte-identical, and no deduplication, specialization, or merging step may
collapse two externally-bound identities into one.

Backends do not reason about reference counting. They lower and execute the
explicit LIR `incref`, `decref`, and `free` statements emitted before backend
code generation. Each explicit RC statement carries the concrete RC helper
selected by LIR ARC insertion. Consumers may lazily cache code or interpreter
execution plans for that helper, but they must not select a different helper
from local layout data. Reference-counting policy belongs to LIR ARC insertion.

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
link a platform C runtime disables target-library assumptions and lowers LLVM
memory intrinsics to explicit loops before target code generation. macOS and
Windows keep target library calls available because their final links include
the platform runtime libraries.

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

The words `publish` and `fact` are banned in new post-check docs and code,
including their common variants. Use `output` for phase output, or use the
exact owner/data name.

The word `physical` is banned in new post-check docs and code. Use `layout`
only for memory shape data such as size, alignment, field offsets, and payload
layout. Use `runtime encoding` for the broader category that includes layouts,
discriminants, callable variant encodings, erased callable code entries, ABI
shape, and runtime schemas.

The word `artifact` is banned in new post-check docs and code. Use the precise
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

## Canonicalization Policy Ownership

Canonicalization owns source-name scope policy before checking. Any rule that
decides whether a source type name is inserted, shadows another type, replaces
an auto-imported type, redeclares an existing type, or repeats the same external
type must live in one place. Callers may choose which source operation they are
performing, but they must not duplicate the type-binding collision matrix.

The `Scope.type_bindings` table has one ordinary mutation API for type names.
It accepts the full scope slice, the target scope index, the introduced name,
and the incoming binding:

```zig
const TypeBindingInput = union(enum) {
    local_nominal: CIR.Statement.Idx,
    local_alias: CIR.Statement.Idx,
    associated_nominal: CIR.Statement.Idx,
    external_nominal: Scope.ExternalTypeBinding,
};

const TypeBindingDecision = union(enum) {
    inserted,
    inserted_shadowing_parent: Scope.TypeBinding,
    replaced_current_external: Scope.ExternalTypeBinding,
    idempotent_current,
    rejected_current_conflict: Scope.TypeBinding,
    redeclared_current: Scope.TypeBinding,
};
```

The exact names may change with implementation, but the shape must remain:
one `Scope` function mutates `type_bindings`, and its return value carries the
old binding that caused any warning or error. `Scope` does not push diagnostics,
does not inspect source regions from `ModuleEnv`, and does not update import
display mappings. `Can` maps the returned decision to diagnostics and performs
the import-mapping side effects that are specific to external imports.

Parent-scope shadowing must be computed by the same type-binding API. The
function receives the scope slice and target index directly; it does not use an
untyped callback or a callback without context. The parent walk chooses the
nearest parent binding and returns that binding to the caller. This preserves
regions for both local statements and external imports without reconstructing
them at each call site.

The current-scope collision matrix is:

- A same statement already bound to the same name is idempotent.
- A same external module/original-name pair already bound to the same name is
  idempotent.
- A local declaration replacing an external binding succeeds and returns the
  replaced external binding so `Can` can report the shadowing region.
- An external binding colliding with any different current binding is rejected
  and returns the existing binding.
- A local alias colliding with a current local alias reports alias
  redeclaration.
- A local nominal colliding with a current local nominal or associated nominal
  reports type redeclaration.
- A local declaration colliding with the other local kind reports the diagnostic
  chosen by the existing binding, not by the incoming binding.

Direct writes to `Scope.type_bindings` are allowed only for explicit
initialization paths that prove the binding cannot collide, such as seeding the
compiler-owned builtin scope before source declarations are introduced. If that
proof stops being local and obvious, the initialization path must use the same
type-binding API.

Result suffix desugaring has a separate owner inside `Can`. The suffix forms
`expr?`, `expr ? handler`, and `expr ?? default` all lower to a match over the
same `Try` shape. They must share one concrete builder for the common structure:

- resolve the compiler-owned `Try` nominal target once,
- wrap `Ok` and `Err` tag patterns with that nominal target,
- append the `Ok(#ok) => #ok` branch,
- construct the `Err(...)` tag expression used by early return,
- create the final match with the caller-selected `is_try_suffix` value.

The builder must support both local and external nominal targets because the
Builtin module may refer to its own `Try`, while ordinary modules use the
compiler-owned external builtin. It must not silently fall back to bare `Ok` and
`Err` patterns when `Try` is missing. A missing compiler-owned `Try` target is a
compiler invariant violation after builtin setup, not an alternate
canonicalization mode.

The three suffix callers provide only the distinct error-branch body:

- `expr?` returns the original error payload from the enclosing function, or
  emits `e_expect_err` inside a top-level `expect`.
- `expr ? handler` transforms the error payload and then returns `Err(...)`, or
  emits `e_expect_err` inside a top-level `expect`.
- `expr ?? default` uses the default expression and does not mark the match as a
  try suffix.

Do not introduce a generic desugaring interpreter for these cases. The helper
should be a small set of concrete `Can` functions that emit the same CIR nodes
and scratch spans as the current hand-written paths. This keeps
canonicalization output explicit, keeps diagnostics in `Can`, and keeps release
builds fast: the work runs once per source suffix or type declaration, with no
runtime cost in the compiled program.

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

`ModuleEnv` contains `CommonEnv.strings`, a `base.StringLiteral.Store`. That
store is part of the checked module cache data. A cache hit materializes it as a
view of its byte buffer and stops. Cache reads must not scan the string entries,
rebuild a string interning table, check every string length header, check every
static refcount word, or check every entry alignment. This design adds no
store-specific release-build validation; cache reads perform only the existing
cache-entry admission and decode checks before trusting the blob. Once those
pass, the internal string buffer structure is a producer invariant. Debug builds
may assert this invariant while constructing fresh stores and in focused store
tests; optimized cache reads consume the store directly.

String literal deduplication is a build-time concern. The durable
`StringLiteral.Store` owns only the static-refcounted byte buffer plus `get` and
iteration by `StringLiteral.Idx`. It has no insert API and no dedup index.
Fresh construction uses `StringLiteral.Builder` state paired with a `Store`.
That state may live in a wrapper or in the build owner that owns the store, but
it is always transient. The builder index is never serialized, never stored in
LirImage, and never rebuilt on a cache hit. If a later phase needs a mutable
string-literal builder, it must request an explicit fresh builder from source
data or another builder-owned input; it must not reopen a cached store on the
normal cache path.

The byte interning algorithm has one owner shared by identifier names, checked
name stores, and string-literal builders. Storage policies own only id encoding,
text lookup, and append layout. For string literals, appending a new entry writes
exactly the current static-data layout:

```text
len: u32 | padding to isize | static refcount: isize(0) | bytes
```

and returns the content byte offset as `StringLiteral.Idx`. Duplicate input
bytes must return the existing content offset. The hash table is an accelerator
only: hash matches must still compare exact byte length and contents before an
existing id is reused. The shared interning algorithm is comptime-policy
specialized, so string literals, identifier names, and checked name stores do
not pay a runtime storage-kind branch.

The string-literal builder must reject impossible `u32` length or content-offset
overflow as a compiler invariant: debug builds assert or panic with the
invariant, and optimized builds mark the path unreachable. It must never silently
truncate a string length or offset.

The checked module cache id is target-independent:

```text
CheckedModuleId =
    source_hash
  + compiler_build_hash
  + module_identity
  + checking_context_identity
  + direct_import_checked_module_ids
```

`module_identity` includes the module's name. Canonicalization output is not
a function of source bytes alone — a type module's main type takes its name
from the module's file name — so no key or identity derived from module
content may be computed from source bytes without the module name.

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

A post-check specialization cache is a separate boundary named
`SpecializationCacheFile`. It is consumed only after immutable checked modules
and explicit root requests are available. It is not embedded in checked modules,
is not visible to importers as checked data, and does not change the checked
module cache id. Its validity id is computed from exactly the checked modules,
root requests, and Monotype configuration consumed by specialization. A
Monotype-only cache file excludes target ABI, pointer width, layout ids, field
offsets, backend choice, object format, ARC state, and code-generation options.
If a later-stage cache needs those inputs, it must use a separate file format
and a separate validity id.

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
function arguments, runtime pattern binders, mutable locals, effectful calls,
host calls, platform requirements whose values are not available during checking
finalization, and any static dispatch whose checked plan does not identify a
pure compile-time-evaluable operation. Low-level operations may participate only
through explicit checked purity and totality metadata; they must never be
allowed by whitelist, name, or backend knowledge.

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

Some method registry targets are generated structural targets rather than
procedure bodies. A nominal or opaque type can opt in to a compiler-derived
structural codec with an annotation-only associated method such as
`parser_for : _` or `encode_to : _`. Canonicalization may represent this marker
as `e_anno_only` or, for hosted/type-module processing, as a zero-argument
`e_hosted_lambda`; `CheckedModule.method_registry` records it explicitly as a
generated parser or generated encoder target. Post-check lowering must consume
that explicit target kind and lower the structural parser/encoder from the
dispatch plan's concrete callable type. It must not treat the marker as a
procedure body, synthesize a fake source function, or infer generated behavior
from a missing procedure template.

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
thing = Json.parse_trailing_commas(json_str)?
thing = Json.Utf8.parse(json_bytes)?

json_str = Json.encode(thing)?
json_bytes = Json.Utf8.encode(thing)?

headers = Encoding.HttpHeader.parse(raw_headers)?
```

The convenience functions construct the internal format state directly, call the
value or type's ordinary method, validate the remaining state if the format
requires it, and return the final `Try`. They do not need a required `init`,
`finish`, or `default` hook. The runtime cursor types are implementation
details of the builtin format module, not public `Json.State` or
`Encoding.HttpHeader.State` APIs.

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
renaming, whether JSON accepts trailing commas, JSON tag representation, or a
header matching mode. The `state`
argument is the runtime cursor or output state. Keeping these separate matters:
parser construction can transform the requested structural shape before the
runtime scan starts, while the returned runtime function threads only the cursor
state and parsed values.

For example, the builtin HTTP header helper inside `Builtin.Encoding` has this
shape:

```roc
HttpHeader := [MissingRequired, BadHeader].{
	parse : Str -> Try(output, HttpHeader)
		where [
			output.parser_for : HttpHeaderEncoding -> (HttpHeaderState -> Try({ value : output, rest : HttpHeaderState }, HttpHeader)),
		]
	parse = |raw| {
		Output : output

		parse_output = Output.parser_for(HttpHeaderEncoding.Caseless)
		parsed = parse_output(HttpHeaderState.{ raw })?

		Ok(parsed.value)
	}
}
```

The important split is that `Output.parser_for(HttpHeaderEncoding.Caseless)`
constructs the concrete parser and the hidden `HttpHeaderState.{ raw }` is the
runtime input state. Formats with no configurable behavior can still use a
zero-sized internal encoding value.

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
The compiler creates a `Encoding.FieldName.FieldNames(_shape)` value for each
concrete record shape:

```roc
Encoding.FieldName(_shape) : opaque
Encoding.FieldName.FieldNames(_shape) : opaque

Encoding.FieldName.FieldNames.rename_fields : Encoding.FieldName.FieldNames(_shape), (Str -> Str) -> Encoding.FieldName.FieldNames(_shape)
Encoding.FieldName.FieldNames.shortest_name : Encoding.FieldName.FieldNames(_shape) -> U64
Encoding.FieldName.FieldNames.longest_name : Encoding.FieldName.FieldNames(_shape) -> U64
Encoding.FieldName.FieldNames.iter : Encoding.FieldName.FieldNames(_shape) -> Iter(Encoding.FieldName(_shape))
Encoding.FieldName.FieldNames.for_size : Encoding.FieldName.FieldNames(_shape), U64 -> Iter(Encoding.FieldName(_shape))

Encoding.FieldName.name : Encoding.FieldName(_shape) -> Str
```

`Encoding.FieldName.FieldNames(_shape)` contains the requested field names and
compiler-owned result positions for one concrete record shape.
`Encoding.FieldName(_shape)` is an opaque handle to one field in that same shape. The
`_shape` parameter is a phantom type: it is not runtime data, but it ties a
field handle to the exact field set that created it. A parser for
`{ cache_control : Str, content_length : U64 }` cannot accept a
`Encoding.FieldName` produced from `{ foo : Str }`, because the phantom types do not
unify. That type-level tie is what lets generated record parsers avoid runtime
bounds checks on field handles. If the only way to obtain a
`Encoding.FieldName(_shape)` is from the matching
`Encoding.FieldName.FieldNames(_shape)`, then the compiler already knows every handle
is in range for that record. There is no user-exposed `U64` slot to validate at
runtime.

The derived `parser_for` constructs field metadata before returning the runtime
lambda:

```roc
renamed_fields = Encoding.FieldName.FieldNames.rename_fields(original_fields, |name| encoding.rename_field(name))
parse_nested = Nested.parser_for(encoding)
```

`encoding.rename_field(name)` is ordinary method-call syntax for a pure format
method whose first argument is the encoding value. Every encoding provides it;
identity is the normal implementation. Taking the encoding value as an argument
lets one encoding type store parser-construction configuration such as JSON
field naming style. `Encoding.FieldName.FieldNames.rename_fields` applies that
function to every requested record field, discards the original names from the
returned `Encoding.FieldName.FieldNames`, and rebuilds the length buckets used by
`Encoding.FieldName.FieldNames.for_size`, `Encoding.FieldName.FieldNames.shortest_name`,
and `Encoding.FieldName.FieldNames.longest_name`. If parser construction is
compile-time evaluated, the renaming work is also compile-time work. For JSON
camel-case decoding, the final runtime parser can contain only `camelCase`
field names. For HTTP header decoding, the final runtime parser can contain only
lowercase kebab-case header names such as `cache-control`.

Formats expose the methods needed for the shapes they support. A format that can
parse strings, `U64`, tag unions, and records uses these method shapes:

```roc
encoding.parse_str : encoding, state -> Try({ value : Str, rest : state }, err)
encoding.parse_u64 : encoding, state -> Try({ value : U64, rest : state }, err)
encoding.parse_tag_union : encoding, Encoding.ParseTagUnionSpec(a), state -> Try({ value : a, rest : state }, err)

encoding.parse_record_field : encoding, Encoding.FieldName.FieldNames(_shape), state -> Try(
	[
		Field({ field : Encoding.FieldName(_shape), rest : state }),
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
provided `Encoding.FieldName.FieldNames(_shape)`, usually by iterating
`Encoding.FieldName.FieldNames.for_size(fields, len)`
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
`Encoding.FieldName.FieldNames(_shape)` value and a requested matching mode.

`Encoding.FieldName.FieldNames.shortest_name` and
`Encoding.FieldName.FieldNames.longest_name` are computed after renaming. Formats may
use them to skip impossible fields before doing more expensive work. For
example, if a header name is longer than
`Encoding.FieldName.FieldNames.longest_name(fields)` and the format's `rename_field`
never increases field length for headers, the format can consume the line and
return `Continue` without constructing any temporary field name. This is not a
parse failure: for formats such as HTTP headers and JSON objects, unknown fields
remain ordinary input according to that format's rules. If the target record
actually contains a long renamed field name, the long input field remains
matchable through the same `Encoding.FieldName.FieldNames` iteration APIs.

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
`Encoding.FieldName.FieldNames(_nested_shape)` value, then renames and rebuckets that
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
may use `Encoding.FieldName.FieldNames.for_size` plus ASCII-caseless comparison
against `Encoding.FieldName.name` to match the transformed field set directly and
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

The runtime JSON scanner can use `Encoding.FieldName.FieldNames.for_size` and exact
`Encoding.FieldName.name` comparison to match each object key against the
already-renamed field set and return `Field({ field, rest: value_start })` for
known keys. It may also return `TryField({ name, rest: value_start })` and let
generated record dispatch perform exact matching. For unknown keys, it skips the
JSON value according to JSON syntax and returns
`Continue({ rest: after_value })`. The matched field's parser consumes the JSON
value from `value_start`.

JSON tag unions use the externally tagged representation:

```json
{ "Admin": { "name": "Sam" } }
```

Zero-payload tags encode as the tag string, one-payload tags encode as
`{"Tag":payload}`, and multi-payload tags encode as `{"Tag":[...]}`. This
representation avoids collisions between tag names and ordinary record field
names. Other JSON conventions are represented by different JSON format values
with different methods. The compiler receives the null, missing-field, and
tag-union rules through explicit format methods rather than through hard-coded
JSON syntax recovery.

Parsing a Roc `Str` from JSON succeeds only for JSON string values. JSON `null`
and missing object fields are separate format conditions. They are surfaced only
through field or value types that request them, such as `Try(Str, [Null])` or
`Try(Str, [Missing])`; the plain `Str` method does not accept either condition.
`Try(a, [Null])` is the nullable JSON value shape. A format's
`missing_optional_field` method chooses the record-field absence tag for
optional fields; JSON uses `Missing`, but another format may choose `Absent` or
any other tag. `Try(a, [Missing])` and `Try(a, [Missing, Null])` are JSON's
record-field-only shapes: missing fields parse as `Err(Missing)`, explicit
`null` parses as `Err(Null)` only when `Null` is in the row, and encoding
`Err(Missing)` omits the field. Missing fields and `Null` are never conflated.

JSON arrays are used for lists, tuples, and sets. Tuples parse with exact arity.
Sets preserve `Set` insertion order and parse by inserting the array elements.
JSON dictionaries use object representation only when the key type has a
lossless object-key codec: strings, bools, numeric types, and zero-payload tags.
Composite dictionary keys are rejected by static dispatch validation; there is
no automatic pair-array fallback. Dictionary and set encoders do not sort,
because Roc does not require keys or elements to be sortable.

Concrete HTTP header parser code has this shape inside `Builtin.Encoding`:

```roc
HttpHeaderState :: { raw : Str }

HttpHeaderEncoding :: [Caseless].{
	rename_field : HttpHeaderEncoding, Str -> Str
	parse_str : HttpHeaderEncoding, HttpHeaderState -> Try({ value : Str, rest : HttpHeaderState }, HttpHeader)
	parse_u64 : HttpHeaderEncoding, HttpHeaderState -> Try({ value : U64, rest : HttpHeaderState }, HttpHeader)

	parse_record_field : HttpHeaderEncoding, Encoding.FieldName.FieldNames(_shape), HttpHeaderState -> Try(
		[
			Field({ field : Encoding.FieldName(_shape), rest : HttpHeaderState }),
			TryField({ name : Str, rest : HttpHeaderState }),
			TryFieldCaseless({ name : Str, rest : HttpHeaderState }),
			Continue({ rest : HttpHeaderState }),
			Done({ rest : HttpHeaderState }),
		],
		HttpHeader,
	)

	skip_record_field : HttpHeaderEncoding, HttpHeaderState -> Try(HttpHeaderState, HttpHeader)
	missing_record_field : HttpHeaderEncoding, Str, HttpHeaderState -> HttpHeader
	missing_optional_field : HttpHeaderEncoding, Str, HttpHeaderState -> [Missing]
}

HttpHeader := [MissingRequired, BadHeader].{
	parser_for : () -> (Str -> Try(output, HttpHeader))
		where [
			output.parser_for : HttpHeaderEncoding -> (HttpHeaderState -> Try({ value : output, rest : HttpHeaderState }, HttpHeader)),
		]
	parser_for = || {
		Output : output
		parse_output = Output.parser_for(HttpHeaderEncoding.Caseless)

		|raw| {
			parsed = parse_output(HttpHeaderState.{ raw })?
			Ok(parsed.value)
		}
	}

	parse : Str -> Try(output, HttpHeader)
}
```

The exact derived parser type for a header record with mixed field shapes is:

```roc
{
	cache_control : Str,
	content_length : U64,
	x_auth_token : Try(Str, [Missing]),
}.parser_for : HttpHeaderEncoding -> (HttpHeaderState -> Try(
	{
		value : {
			cache_control : Str,
			content_length : U64,
			x_auth_token : Try(Str, [Missing]),
		},
		rest : HttpHeaderState,
	},
	Encoding.HttpHeader,
))
```

Because `Encoding.HttpHeader` does not define `parse_tag_union`, trying to parse a
header record that contains a tag union is a compile-time static-dispatch error:

```roc
bad : Try({ mode : [On, Off] }, Encoding.HttpHeader)
bad = Encoding.HttpHeader.parse("mode: On\r\n")
```

The missing requirement is `HttpHeaderEncoding.parse_tag_union`; the compiler
does not wait until runtime to discover that this format does not support tags.

Concrete JSON parser code has this shape:

```roc
JsonState :: [Input(Str)]

JsonEncoding :: [Default, CamelCase, TrailingCommas].{
	rename_field : JsonEncoding, Str -> Str
	rename_field = |encoding, name|
		match encoding {
			Default => name
			TrailingCommas => name
			CamelCase => snake_to_camel(name)
		}

	allows_trailing_commas : JsonEncoding -> Bool
	allows_trailing_commas = |encoding|
		match encoding {
			Default => False
			CamelCase => False
			TrailingCommas => True
		}

	parse_str : JsonEncoding, JsonState -> Try({ value : Str, rest : JsonState }, Json)
	parse_record_field : JsonEncoding, Encoding.FieldName.FieldNames(_shape), JsonState -> Try(
		[
			Field({ field : Encoding.FieldName(_shape), rest : JsonState }),
			TryField({ name : Str, rest : JsonState }),
			TryFieldCaseless({ name : Str, rest : JsonState }),
			Continue({ rest : JsonState }),
			Done({ rest : JsonState }),
		],
		Json,
	)
	skip_record_field : JsonEncoding, JsonState -> Try(JsonState, Json)
	missing_record_field : JsonEncoding, Str, JsonState -> Json
	missing_optional_field : JsonEncoding, Str, JsonState -> [Missing]
	parse_tag_union : JsonEncoding, Encoding.ParseTagUnionSpec(a), JsonState -> Try({ value : a, rest : JsonState }, Json)
}

Json := [MissingRequired, InvalidJson].{
	parse : Str -> Try(a, Json)
		where [
			a.parser_for : JsonEncoding -> (JsonState -> Try({ value : a, rest : JsonState }, Json)),
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
					Err(Json.InvalidJson)
				}
		}
	}

	parse_trailing_commas : Str -> Try(a, Json)
		where [
			a.parser_for : JsonEncoding -> (JsonState -> Try({ value : a, rest : JsonState }, Json)),
		]
	parse_trailing_commas = |json| {
		Shape : a
		parse_shape = Shape.parser_for(JsonEncoding.TrailingCommas)
		parsed = parse_shape(JsonState.Input(json))?

		match parsed.rest {
			Input(rest) =>
				if Str.is_empty(Str.trim_start(rest)) {
					Ok(parsed.value)
				} else {
					Err(Json.InvalidJson)
				}
		}
	}

	parser_camel : () -> (Str -> Try(a, Json))
		where [
			a.parser_for : JsonEncoding -> (JsonState -> Try({ value : a, rest : JsonState }, Json)),
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
						Err(Json.InvalidJson)
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
	Json,
))
```

The exact derived parser type for an externally tagged JSON union is:

```roc
[Admin({ name : Str }), Guest].parser_for : JsonEncoding -> (JsonState -> Try(
	{
		value : [Admin({ name : Str }), Guest],
		rest : JsonState,
	},
	Json,
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

Generated encoders compose child error rows. JSON helpers that cannot fail use a
named `_never_fails` row variable so they can sequence with encoders that can
fail. JSON `F32` and `F64` encoders are the deliberate failing scalar case:
finite values encode as JSON numbers, while `NaN`, positive infinity, and
negative infinity return `Err(NaN)`, `Err(Infinity)`, or
`Err(NegativeInfinity)`. They must not encode non-finite values as JSON `null`.

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
through that type's **own declared** `from_numeral` method, and a string literal
converts through its declared `from_quote` (receiving the literal's post-escape
contents as `Str`). The conversion is not inherited through the backing chain: a
transparent newtype that declares no `from_` does not accept a bare literal —
that is a type error; use explicit `Nominal.(value)` construction instead.
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

### Constructing Nominal Values

Nominal construction is expression-directed, not a unification side effect.
Explicit construction syntax — `Type.(value)`, `Type.(a, b)`, `Type.{ field }`,
`Type.Tag(payload)` — canonicalizes to a single `e_nominal` (or
`e_nominal_external`) expression whose `backing_type` records the surface form
(`value`, `tuple`, `record`, `tag`). Checking instantiates the nominal
declaration, unifies the user-written backing expression against the
declaration's backing variable, and gives the whole expression the nominal type.
The backing variable is read **only** during this unification; nominal identity
is defined by origin, name, and arguments, and two nominals of different identity
never unify. A value already typed as a different nominal or primitive therefore
does not silently lift into a nominal — the implicit path is reserved for
literals, which convert through the `from_numeral` / `from_quote` mechanism above
(walking transparent newtype chains to reach the builtin backing).

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
materialization. While solving is still active, users hold instantiation graph
nodes rather than final Monotype type ids. Materialization turns solved graph
nodes into immutable interned Monotype type nodes. Recursive groups may reserve
their ids inside the type interner while the group is being sealed, but no type
id that is visible in Monotype IR is later refilled or changed. This is
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

The specialization store must make this lookup direct. It must not scan all
specializations for a callable family and recompute recursive type digests while
lowering a body. A specialization request is identified by:

```zig
const SpecIdentity = struct {
    callable: CallableIdentity,
    source_fn_ty_digest: TypeDigest,
    mono_fn_ty_digest: TypeDigest,
    mono_fn_ty: TypeId,
};

const CallableIdentity = union(enum) {
    proc_template: struct {
        module: CheckedModuleId,
        proc: ProcTemplateId,
    },
    nested_site: struct {
        module: CheckedModuleId,
        owner_proc: ProcTemplateId,
        owner_fn_digest: FnDigest,
        site: NestedSiteId,
    },
    hosted: HostedId,
    generated: GeneratedId,
};

const SpecStatus = enum {
    reserved,
    lowering,
    ready,
};

const SpecRecord = struct {
    identity: SpecIdentity,
    fn: FnId,
    status: SpecStatus,
};
```

`source_fn_ty_digest` records the checked source function type after
instantiation into the requesting graph. `mono_fn_ty_digest` records the closed
requested function type. The digests make lookup fast, but they are not the only
correctness check. When a digest match is found, the store must also verify
the checked callable identity and exact structural equality of the closed
Monotype function type. Digest collisions are therefore harmless.

The in-memory builder owns a transient hash table from `SpecIdentity` to
`SpecId`, plus the append-only `SpecRecord` array. The output program owns the
records and the function bodies, not the hash table. A loaded cache file may
build a transient hash table over the mapped records, but the file itself stores
sorted records and fixed spans so it can be consumed without pointer fixups.

Monotype type construction must feed the specialization store with immutable
interned type nodes:

```zig
const MonoTypeStore = struct {
    nodes: []const MonoTypeNode,
    args: []const TypeId,
    fields: []const Field,
    tags: []const Tag,
    payloads: []const TypeId,
    declared_fields: []const DeclaredField,
    digests: []const TypeDigest,
};

const MonoTypeNode = extern struct {
    tag: MonoTypeTag,
    first: u32,
    len: u32,
    extra: u32,
};
```

The mutable instantiation graph may use union-find, row-extension links, and
work queues while solving one specialization. Its final output is an immutable
`TypeId` in `MonoTypeStore`. After that point, the type node is never refilled.
Rows are normalized once, with field and tag names in explicit sorted order,
and the type digest is stored beside the node when the node is interned. Parent
digests are computed from child digests, so structurally growing records and
function types do not repeatedly walk their whole prefix.

The type interner enforces exact equality:

```text
same digest
same tag
same child count and metadata
same ordered child ids, field names, tag names, and payload positions
```

The digest table is an acceleration structure only. Exact equality remains the
authority for type identity. This gives generic higher-order code the desired
shape: repeated calls at the same closed function type reuse one specialization
after the first request, and growing structural accumulator types add only the
new record/function nodes instead of redigesting every previous layer.

Open instantiation graphs do not write directly into final Monotype body
sections. While a specialization is being solved, lowering writes to a
`BodyDraft` owned by that specialization graph. A draft mirrors the final
Monotype sections enough for lowering to refer to expressions, patterns, locals,
definitions, nested definitions, side-pool spans, and function signatures, but
all type-bearing fields use a draft type cell:

```zig
const DraftTypeCell = union(enum) {
    graph_node: InstNodeId,
    sealed: TypeId,
};
```

`graph_node` is used for any type cell owned by the active instantiation graph:
expression types, pattern types, binder/local types, typed-local entries,
function arguments, function returns, lambda and nested function signatures,
specialization request function types, layout requests, and runtime schema
requests. `sealed` is used only for closed Monotype types that were already
materialized before this graph was opened, such as imported cache entries or
builder-global primitive and hosted ABI types. If a sealed type must participate
in the current specialization's constraints, the graph imports it and the draft
stores the imported node instead of the original `TypeId`.

A `BodyDraft` may contain ordinary lowering ids, spans, and side pools while it
is active, but those ids are draft-local. They are not cache ids and no later
post-check stage consumes them. The draft is sealed only after:

1. all checked type evidence for the specialization has been applied;
2. deferred procedure-template requests created by this graph have been drained
   or reserved with stable closed request types;
3. nested function bodies that share this graph have finished lowering;
4. every unresolved graph node can be closed from checked data, or can be
   proven to be a truly unconstrained empty tag union.

Sealing performs the only transition from graph nodes to final Monotype
`TypeId`s. It walks every draft type cell, materializes each graph node
through the Monotype type interner, preserves recursive groups privately inside
the interner, computes and stores type digests once, and then copies the fully
sealed records into `MonoProgramBuilder`. This copy also turns draft-local ids
and spans into final shard-local ids and spans. If sealing finds a graph node in
any completed record after this step, that is a compiler bug.

This split is required for future specialization caching. Cache files contain
only sealed `MonoProgramView` sections: fixed-width records, ids, spans, and
offsets into side pools. They never contain union-find nodes, mutable type
views, allocator-owned arrays, hash maps, or draft-local ids. Because every
interior relation in a sealed program is an id or span into the same shard, a
mapped cache file can be read back as a read-only `MonoProgramView` with only
top-level slice creation, shard assignment, and import-table resolution.

The program store is split into a builder and a read-only view:

```zig
const MonoProgramBuilder = struct {
    types: MonoTypeInterner,
    specs: SpecBuilder,
    fns: ArrayList(FnDef),
    exprs: ArrayList(Expr),
    pats: ArrayList(Pat),
    names: NameStoreBuilder,
};

const MonoProgramView = struct {
    types: MonoTypeStore,
    specs: []const SpecRecord,
    fns: []const FnDef,
    exprs: []const Expr,
    pats: []const Pat,
    names: NameStoreView,
};
```

Function slots are shard-aware so future cache files can be mapped directly:

```zig
const ShardId = enum(u32) { _ };
const FnSlot = union(enum) {
    local: FnId,
    imported: ImportedFnId,
};

const ImportedFn = extern struct {
    shard: ShardId,
    fn: FnId,
};
```

A newly built root program has one mutable local shard. A loaded specialization
file is a read-only shard. Calls inside a shard use local `FnId` values when
the target is stored in the same shard. Cross-shard calls use an `ImportedFnId`
into an imports table. Loading resolves each import table entry to
`ImportedFn { shard, fn }` once. Function bodies are not rewritten after the
file is mapped.

The durable format uses only plain old data records, offsets, lengths, and side
pools. Hash maps, union-find nodes, temporary worklists, and allocator-owned
arrays are transient builder data and are never written.

```zig
const SpecializationCacheHeader = extern struct {
    magic: [8]u8,
    format_version: u32,
    compiler_layout_hash: [32]u8,
    validity_id: [32]u8,

    names: FileSlice,
    type_nodes: FileSlice,
    type_args: FileSlice,
    fields: FileSlice,
    tags: FileSlice,
    payloads: FileSlice,
    declared_fields: FileSlice,
    type_digests: FileSlice,

    specs: FileSlice,
    fns: FileSlice,
    defs: FileSlice,
    nested_defs: FileSlice,
    exprs: FileSlice,
    pats: FileSlice,
    stmts: FileSlice,
    locals: FileSlice,
    expr_ids: FileSlice,
    pat_ids: FileSlice,
    typed_locals: FileSlice,
    stmt_ids: FileSlice,
    field_exprs: FileSlice,
    record_destructs: FileSlice,
    str_pattern_steps: FileSlice,
    branches: FileSlice,
    if_branches: FileSlice,
    string_literals: FileSlice,
    imports: FileSlice,
    roots: FileSlice,
    layout_requests: FileSlice,
    runtime_schema_requests: FileSlice,
    comptime_sites: FileSlice,
    source_files: FileSlice,
    expr_locs: FileSlice,
    expr_regions: FileSlice,
    stmt_locs: FileSlice,
    stmt_regions: FileSlice,
    local_names: FileSlice,
    debug_names: FileSlice,
};

const FileSlice = extern struct {
    offset: u64,
    len: u64,
};
```

Any current in-memory field that contains a process pointer or slice must be
converted to an offset record plus a byte or region side pool before it can be
written to these sections. This applies to string literals, source-file names,
local names, debug-name text, and compile-time site branch-region lists. A cache
file must never store process pointers from `[]const u8`, `[]const Region`, hash
maps, or allocator-owned arrays.

The loader validates the header, `format_version`, `compiler_layout_hash`,
`validity_id`, bounds, alignment, and section ordering. It then creates a
`MonoProgramView` by adding the mapped base address to each `FileSlice`. The
only required fixups are:

- converting top-level file slices to process slices;
- assigning a `ShardId` to the mapped file;
- resolving each import-table entry to a loaded shard and function id.

There are no per-expression, per-type, or per-function pointer rewrites. All
interior relations are ids or spans into the same shard.

`validity_id` for a Monotype specialization file includes:

- the format version and compiler layout hash;
- the root checked module id and all checked module ids read by the stored
  specializations;
- the explicit root request set;
- the Monotype configuration that can affect reachable specializations;
- builtin module data consumed by Monotype;
- the source callable identities and source function type digests for the
  stored specializations.

It does not include data that Monotype does not consume. In particular, it does
not include LIR layout decisions, ARC output, backend symbols, object-format
choices, or code-generation options.

Cache loading is an optimization of the same specialization store, not another
lowering path. A loaded `SpecRecord` must pass the same identity and exact type
checks as a freshly produced record before it can satisfy a request. If no
loaded record matches, the builder creates the specialization normally and may
append it to a new cache file after the program is complete.

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
  -> native dev backend on native compiler hosts
  -> store eval result in ConstStore
```

On native compiler hosts, every `compile_time_*` root uses the dev backend for
compile-time evaluation: ordinary constants, selected hoisted constants,
expects, callable eval roots, numeral conversions, and quote conversions. The
compile-time evaluator does not interpret Monotype IR, Lambda Solved IR,
logical Lambda Mono expressions, or any source-level IR.

The only interpreter path is for compiler hosts that cannot run generated native
code, currently wasm32 and freestanding compiler builds. This decision is made
from the compiler's own build target. The Roc program target selected by
`roc --target` does not affect the compile-time evaluation strategy. A native
compiler host without host dev-backend code generation support is unsupported for
compile-time evaluation; it must not silently use the interpreter.

Diagnostic behavior must be identical between the dev-backend evaluator and the
host-restricted LIR interpreter. A mismatch in crashes, expect failures,
empirical exhaustiveness diagnostics, branch coverage, literal conversion
diagnostics, or stored constants is a compiler bug in one of the evaluators or
in the data they consume.

Compile-time ARC insertion runs the same borrow-inference solver as runtime
ARC insertion in its single-variant form: one proc per solved `RcSig`, no
mode specialization. Compile-time evaluation pays for solving once per
evaluated root and never for variant cloning.

The evaluator produces a runtime value. Checking then stores that eval
result as checked-stage data in the checked module's `ConstStore`. `ConstStore`
stores checked Roc values only. It does not contain Monotype nodes, Lambda
Solved data, Lambda Mono decision data, runtime addresses, allocation identity,
layout ids, runtime discriminants, field offsets, LIR locals, LIR procedure
ids, backend symbols, backend bytes, or host handles.

Compile-time finalization evaluates dependency-ready roots in batches. Each
batch is lowered to LIR once, ARC is inserted once, the dev backend emits native
code for the reachable proc specs, and a generated wrapper is emitted for each
root. Roots in the same batch are independent pure computations and may run on a
work queue. The finalizer still commits stored values, diagnostics, coverage
updates, and root completion in sorted request order.

Each root job owns its compile-time host state: `RocOps`, a root-local arena for
Roc allocations, allocation tracking needed by RocOps, branch-hit records,
expect/dbg/crash event lists, failure region state, a call-region stack, a crash
boundary, and its result buffer. Roc runtime allocations are arena allocations
and are bulk-freed after the result has been copied into `ConstStore`. Host
events and failure state stay root-local until deterministic replay.

A crash or empirical exhaustiveness failure in one root records that root's
result and must not change the result of any other root in the same batch. Root
jobs do not write diagnostics, stderr output, checked problems, or `ConstStore`
entries directly. They write root-local event lists and branch-hit data; after
all jobs in the batch finish, checking finalization replays those lists in the
sorted root order.

Slow-root progress reporting observes the same root job state without changing
evaluation. By default, a root that has been running for more than three seconds
may be reported periodically on both TTY and non-TTY stderr. The message names
the one-line source snippet for the root, truncated with `…` if needed, plus the
module, line, column, and elapsed seconds. If the root belongs to a binding, the
snippet should include enough of that binding line to identify the binding.

Progress reporting must not add a fixed latency penalty to fast compile-time
evaluation. A reporting worker may be started when stderr/std_io reporting is
configured, but every wait in that worker must be interruptible by the
finalizer. The worker waits for the earliest running root's slow-report
deadline, normally `root_start + 3s`, or for a stop signal, whichever comes
first. When a root starts, the worker is signaled so it can recompute the next
deadline without polling. When finalization finishes, it signals the worker and
joins it immediately; it must never wait for a plain sleep interval to expire.
Roots that finish before the slow threshold therefore pay only monitor setup and
signaling overhead, not the reporting period. Roots that exceed the threshold
are reported, then the worker waits interruptibly until the next per-root report
deadline or finalization stop.

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

The symbol string is the identity of an externally-bound function. A hosted
call resolves to the entry at its declaration slot in the `hosted` section;
resolution never matches hosted declarations by signature, by
declaring-module content, or by any content-derived module identity. Two
byte-identical modules whose effectful declarations are wired to different
symbols stay distinct because the platform header that assigns those symbols
is the data that separates them. `provides` follows the same rule: the
exported symbol set is part of the platform relation, and two exports remain
two exports even when they name the same Roc function.

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

## Watch Mode For Check And Test

`roc check --watch` and `roc test --watch` are long-running compiler commands.
They run once immediately, then watch the exact source inputs discovered by that
run. A later filesystem event causes a new run only when at least one watched
input's source file state changes: its bytes change, it appears after being
missing, disappears after existing, or changes between readable and unreadable.
Metadata-only changes such as `touch` do not rerun checking or tests and do not
reprint diagnostics.

The watch set is exact-file based. The implementation may watch directories
because operating systems expose directory-level notification APIs, but directory
events are filtered against the explicit file input set. Files merely present in
a filesystem package or platform directory are not watched unless the compiler
read them as part of the current run. If a source edit adds a new import, the
importing file is already in the watch set; the next run discovers the new input
and refreshes the watch set. URL package files are excluded even when they live
in the local package cache, because their source identity is immutable.

Watch inputs are explicit compiler output. A watch consumer must not recover
module dependencies by scanning source text or reconstruct file imports from
diagnostics. `BuildEnv` owns the shared watch-input collection used by
`roc check --watch` and `roc test --watch`, because both commands already use
`BuildEnv` and because compilation results are transferred there after the
coordinator finishes. After every run, successful or failed, watch mode replaces
the active watch set with the newly discovered explicit input set. Early
failures still include the root source path and any other inputs discovered
before the failure. Missing file imports are included so creating the missing
file can trigger the next run.

File imports are stored in `ModuleEnv` as source-relative dependencies. The
stored path is the literal file-import path interpreted relative to that
module's source directory. It must not be an absolute path, a realpath, a
symlink-resolved path, a cwd-dependent path, or any other host-specific value.
Checked module cache entries include these relative file dependencies so a cache
hit can contribute watch inputs with string concatenation only:

```text
module_source_dir + cached_relative_file_dependency
```

Because file imports are source input to the checked module, their content
identity also participates in the checked module cache key. Changing an imported
file while the importing `.roc` source bytes stay unchanged must miss the
checked module cache and produce fresh checked module data. The cache key input is
the ordered source-relative dependency list plus each dependency's content
digest, never an absolute path or resolved filesystem identity.

When a watched file's parent directory no longer exists, or when a missing file's
parent directory does not exist yet, the watcher registers the nearest existing
ancestor directory and filters events by the unresolved relative suffix. This
keeps watch coverage for later directory creation without widening the logical
watch set.

Filesystem event bursts are debounced for 25ms before re-reading watched inputs.
If another filesystem event with changed bytes arrives while a check/test rerun
is in progress, the in-progress run is cancelled and superseded by the newest
run. Diagnostics and test output are printed for completed runs only. Repeated
runs print a separator before their output instead of clearing the terminal.

## Hot Loading For Default Dev-Shim Runs

The default `roc` command hot loads automatically on the dev backend execution
path: running `roc app.roc` watches the app's source inputs and reloads on
change, with no `--watch` flag and no run subcommand. Non-dev `--opt` levels keep
the existing one-shot behavior, as do apps that cannot use the shared-memory shim
(see below).

The initial compile lowers checked modules to LIR in the compiler process, then
serializes only the dev backend `RunImage` bytes into shared memory. LIR and
compiler IR are never allocated into the shared-memory allocator on this path.
The shared memory contains `RunImage` code section bytes, readonly data bytes,
entrypoint metadata, relocation records, symbol names, and hot-load metadata.
The fixed shared-memory header padding stores only the small atomic hot-load
control block: magic/version fields, the latest descriptor offset, the latest
generation, and host acknowledgement state. It must not contain a fixed table of
loaded-image slots. Loaded images are described by per-image descriptor slots in
the shared-memory mapping plus separately reclaimable image byte ranges, so the
number of retained old generations is limited only by shared-memory capacity.

The compiler launches the host shim first, then installs directory watches for
the exact file input set reported by the coordinator. Coordinator watch inputs
are normalized through the same watch-input collector used by `roc check
--watch` and `roc test --watch`, so relative module paths become logical
absolute paths before reaching the watcher. URL package files are excluded;
filesystem modules, platform/package files, and file imports discovered by the
latest compile are included. After each completed rebuild, the parent refreshes
the watch set from the rebuild worker's serialized watch-input file.

Rebuilds run in short-lived internal compiler child processes. This keeps
cancellation at a process boundary: when a byte-changing filesystem event
arrives while a rebuild is active, the parent kills that rebuild, discards its
captured stdout/stderr text and any uncommitted `RunImage` bytes, and starts a newer generation. A
successful rebuild validates that the checked host interface identity still
matches the already-linked host shim. If the interface changes, the rebuild
reports that the user must restart `roc --watch` and leaves the previous
`RunImage` active.

Successful rebuild workers write a fresh shared-memory image descriptor plus a
new dev `RunImage` into either a compiler-selected free image region or the
append position of the same mapping. Descriptor slots are managed separately
from image bytes and are reused only as descriptors, never as code or data. The
descriptor records the generation, `RunImage` header offset, image bound, image
allocation start/end, lifecycle state, and atomic reference count. The worker
commits the descriptor offset through the hot-load control block with
release/acquire atomics. The host shim checks that control
block at Roc entrypoint boundaries. If a newer generation is available, the shim
retains the latest descriptor, validates and relocates the replacement
`RunImage` in place, marks its code pages read/execute in the shared mapping,
swaps the active entrypoint reference to the new image under the runtime-state
mutex, and acknowledges the generation as accepted. If validation or loading
fails, it acknowledges rejection and keeps using the previous `RunImage`.

Loaded machine-code images are reference-counted by the host shim. Each active
image starts with one reference owned by the active entrypoint table. Entering a
host-callable Roc function increments that image's atomic live count, and
returning from that function decrements it. Swapping to a new image drops the
old image's active-entrypoint reference and moves the old process-local program
descriptor to a retired list. The shim never frees shared-memory image bytes; it
only retains and releases descriptor references. Calls that entered old code
before the swap keep executing old code safely while new entrypoint calls use
the new image.

The compiler parent process is the sole owner of shared-memory image-byte
reclamation. It keeps unbounded process-local lists of descriptor offsets,
reclaimed descriptor slots, and reclaimed image regions. After a rebuild commits
a descriptor, after the host acknowledges, and before choosing storage for
another rebuild, the parent sweeps all known descriptors. The current descriptor
remains live regardless of its reference count. A non-current descriptor with a
nonzero reference count is marked retired and left in place. A non-current
descriptor whose reference count is zero is marked reclaimed, removed from the
live descriptor list, its descriptor slot is returned to the descriptor-slot free
list, and its image allocation range is added to the image free-region list. The
parent coalesces free image regions and rewinds the shared-memory header's
used-size high-water mark to the highest still-live image allocation. New
rebuilds prefer suitably sized reclaimed image regions and otherwise append
below the descriptor-slot area. If filesystem changes arrive faster than the host
can enter the shim, the newest rebuild can still commit a descriptor as long as
shared memory has capacity; there is no small fixed "loaded slot" cap.

This lifetime rule also covers boxed Roc closures that cross the host boundary.
The dev backend generates real erased-callable procedures; it does not insert
trampolines. In shim execution mode, packed erased-callable payloads reserve a
small shim-only prefix before the ordinary capture bytes. The prefix stores a
reference to the owning loaded image and the original capture-drop callback.
Generated erased-callable procedures skip this prefix before reading their
capture, increment the owning image on entry, and decrement it before returning.
The payload's final-drop callback first runs the original capture-drop callback
with the adjusted capture pointer, then releases the payload's retained image
reference. That retained reference keeps an old image alive while a host stores
a boxed Roc closure and later calls it after one or more hot reloads.

Headerless default apps never hot reload. They compile through synthetic
temporary source files that are discarded after each run, so there is nothing
stable to reload; they always run once, even where the shared-memory shim is
available. Hot loading therefore applies only to apps with a real platform
header. Windows uses explicit shared-memory handle inheritance for both the host
shim child and the internal rebuild worker.

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
- No deduplication, specialization, or callable-merging step maps two
  `hosted` declarations or two `provides` exports to one identity, even when
  their declaring modules are byte-identical.

If a boundary check fails, the compiler stops as a compiler bug.
