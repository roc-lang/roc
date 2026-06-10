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
bitcode payloads are sufficient: one for 32-bit targets and one for 64-bit
targets. These pointer-width payloads must contain Roc builtin definitions only;
they must not bundle compiler-rt or other target-specific runtime code, because
that would make the payload architecture-specific again. The payloads are built
as freestanding LLVM bitcode so compile-time OS and CPU branches cannot bake a
native platform's syscalls, inline assembly, or runtime support into a module
that will later be retargeted.

Builtin definitions in the merged LLVM module are real definitions. They must
not be marked `available_externally`, because there is no later builtin object
file to provide non-inlined calls. After builtin call symbols are resolved,
builtin aliases and definitions that are not application exports may be made
internal so LLVM dead-code elimination and the final linker can remove unused
builtin code. Before merging, the LLVM backend roots builtin exports at the
explicit builtin declarations emitted by the application module and internalizes
all other builtin definitions. The normal LLVM optimization pipeline then sees
unused builtins as internal code and may delete them before object emission.
LLVM object emission must request function and data sections, and the final
target linker must use section garbage collection where the target format
supports it.

The compiler does not contain a static borrow, alias-permission, lifetime,
uniqueness, parameter-mode, or escape-summary model in this architecture.
Automatic reference counting is intentionally simple and mechanical. Runtime
mutation uses `refcount == 1` to decide whether in-place mutation is allowed.

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

## Parser Boundary

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

Parser chunks are not considered structurally done until ReleaseFast assembly
has been checked for this shape: no recursive parser calls for the converted
grammar, no instruction-driver loop, no broad parser-context dispatch ladder,
and no unexpected indirect branch in the hot transition path. The expression
prefix/suffix/binary-operator kernel is the first required audit target because
it is the parse-heavy hot path.

Parser conversion proceeds by grammar slices that can be assembly-audited.
Before expanding a slice, build a tiny Zig proof of the intended dispatch shape
and compare it with the analogous simdjson stage-2 parser shape: local token
tests, direct branches between parser states, and explicit syntax state only
where nested syntax requires it. After converting the real Roc slice, build the
ReleaseFast compiler with symbols and disassemble the converted parser symbol
directly, for example:

```sh
zig build roc -Doptimize=ReleaseFast -Dstrip=false
xcrun llvm-objdump --macho --disassemble --dis-symname _Parser.parseExposedCollectionTokens zig-out/bin/roc
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
numeric defaulting, nominal backing instantiation, and structural child ordering
are completed while constructing Monotype types from checked types.

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

An instantiation context owns stage-local type cells addressed by
`(checked module id, checked type id)`. The address is the checked identity of
the type variable/content in the current specialization. It is not a structural
digest, source name, runtime layout, object symbol, or generated procedure id.
Cells begin unresolved. As the specialization is lowered, explicit evidence from
checked data constrains those cells:

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
instantiation dataflow explicit. `Parser(input, value)` does not require
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
that permits structural equality or rewritten the expression to an explicit
structural equality node. Monotype lowering constrains the two checked operand
types to the same instantiation relation and lowers both operands at that single
Monotype operand type. It must not independently lower the left and right
operand types and then attempt to reconcile the results. Independent operand
lowering is order-sensitive: an unconstrained operand can default to an
uninhabited type before the other operand provides evidence. A shared
instantiated operand type preserves the checked equality relation directly.

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
constraint and every later constraint meet in the same cell before the final
Monotype body is emitted.

An unconstrained checked type variable that remains open after checking lowers
to the empty tag union in Monotype. This is not a default choice. It records the
invariant that no runtime value can be constructed at that type. Values such as `[]`
can still be represented as `List([ ])` because they contain no elements, and
code that would need an actual element value must have constrained the element
type earlier or must be unreachable at runtime.

During Monotype construction, an open checked variable is represented by a
stage-local type cell. The cell starts as the empty tag union, and it may be
completed with a concrete type while the same Monotype body is still being
constructed if call-site arguments, expected lambda types, numeric literals, or
checked type relations provide concrete evidence. This is ordinary type solving
inside one stage. Once Monotype IR is output, no open cell remains and no
later stage may change a type.

Monotype type cells are addressed by the owning checked module id and the exact
checked type id. They are not addressed by `TypeDigest`. A digest can identify
closed structural type content for specialization and comparison, but it cannot
distinguish two different open checked variables with the same shape. Treating
those variables as the same cell is a compiler bug.

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
5. If the head is `none` and the checked dispatch plan permits structural
   equality, emit structural equality.
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

### Pattern Lowering

Pattern decision construction is part of the direct LIR builder. It consumes
Lambda Mono patterns and committed layouts and emits LIR control flow. There is
no persisted pattern-decision IR.

### ARC

The direct LIR builder emits ownership-neutral LIR. ARC insertion runs after
LIR construction and emits explicit `incref`, `decref`, and `free` statements.
Each explicit RC statement carries the concrete RC helper selected by ARC.
Backends, the interpreter, and LirImage builders follow those statements
mechanically.

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
module has not been output yet. User-written compile-time crashes, exhausted
compile-time limits, invalid compile-time host interaction, and unsupported
compile-time operations become checking diagnostics attached to the checked root
being finalized. OOM remains OOM. A post-check invariant failure while lowering
or interpreting a compile-time root is still a compiler bug, not a user-facing
diagnostic.

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

const ConstCapture = struct {
    binder: PatternBinderId,
    value: ConstNodeId,
};
```

`fn_def` names a checked, imported, nested, hosted, promoted, or checked-stage
generated procedure template that the checked module owns or references
explicitly.
`captures` bind the exact checked pattern binders required by that function to
stored const nodes. A stored function does not store a lambda set, callable-set
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
    binder: PatternBinderId,
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
one captured checked binder. The direct LIR builder outputs these slots while
lowering the generated function value. The `ConstStore` writer recursively
stores each captured runtime value, then stores the resulting `ConstFn`.

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
- user-facing errors after checked module output
- release-build checks whose only purpose is maintaining compiler invariants

The allowed replacement is explicit stage ownership:

- checking owns user-facing diagnostics and checked data
- Monotype owns monomorphic specialization and static-dispatch elimination
- Monotype Lifted owns closure lifting
- Lambda Solved owns callable flow in the type graph
- Lambda Mono owns explicit callable value representation
- LIR lowering owns committed layouts and statement lowering
- ARC owns reference-count insertion
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
- Backends receive only ARC-complete LIR.

If a boundary check fails, the compiler stops as a compiler bug.
