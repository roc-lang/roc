# Plan: Replace The Post-Check Middle With Cor-Style IRs

This plan describes how to move from the current implementation to the design
in `design.md`.

The fixed boundaries are:

- keep checked CIR, the checked type store, checked module cache
  identity, and user-facing reporting behavior as they are
- keep LIR, LIR ARC insertion, backends, the LIR interpreter, LirImage
  consumers, and glue as they are
- replace everything between checked modules and LIR

The end state is:

```text
checked modules
  -> Monotype IR
  -> Monotype Lifted IR
  -> Lambda Solved IR
  -> Lambda Mono IR
  -> LIR
  -> ARC insertion
```

There is no production fallback to the old path at any point. During
development the new modules can exist beside old modules while focused tests are
being built, but the public lowering entrypoint must switch to one path only.
Once the new path is the public path, the old path is deleted.

The old path is never a source of truth. During development it may be useful as
historical context, but the new path must not compare against old output to
decide behavior, copy recovered facts from old output, or fall back to old
lowering when a new invariant fails.

## Non-Negotiable Rules

- No post-check stage reports user-facing checking errors.
- No post-check stage recovers missing information by scanning source syntax,
  names, declarations, registries, runtime bytes, or backend state.
- No backend makes reference-counting decisions.
- No release-build loops or checks exist only to maintain compiler invariants.
- No module boundary changes generated runtime behavior or performance.
- No monomorphized, lambda-solved, callable-representation, layout, or LIR data
  is added to checked module caches.
- Checked module caches store only checked Roc values in `ConstStore`. Compiler
  runtime representation facts and host/runtime handles never enter checked
  module caches. Pointer-sized Roc values are the only pointer-size-sensitive
  value category; if Roc exposes them to compile-time evaluation, their checked
  cache format must be explicit before publication.
- Checked module fields that exist only to serve the old post-check middle are
  transitional deletion targets, not part of the fixed boundary.
- No old middle output is used as a fallback, source of truth, or source of
  recovered facts.
- New post-check code uses short precise names instead of long compound names.
  Prefer a small defined vocabulary such as `FnSet`, `FnVariant`, `FnTemplate`,
  and `CaptureSlot`; put the explanation in docs or module comments instead of
  repeating it in every identifier.
- New post-check docs and code avoid vague compiler jargon when a plain name is
  available. The words `bridge` and `projection` are banned except in the ban
  text itself or when quoting pre-existing code scheduled for deletion. Say the
  exact operation, such as conversion, field read, tag payload read, capture
  slot, or wrapper function.
- The terms `readback`, `reification`, `value graph`,
  `compile-time value store`, and `representation repair` are banned except in
  the ban text itself or when quoting pre-existing code scheduled for deletion.
  Use `ConstStore`, store eval result, restore cached consts, explicit
  operations, and invariant failures.
- Outside the existing Canonicalization phase and existing files scheduled for
  rename or deletion, the word `canonical` is banned in new post-check docs and
  code. Use `authoritative`, `lexicographic order by name`,
  `payload position order`, `TypeDef`/`NamedType`, or `TypeDigest`.
- The suffixes `Key` and `Ref` are banned in new type names. Use `Id` for
  assigned identities, `Digest` for structural hashes, and concrete domain nouns
  such as `TypeDef`, `FnDef`, `ProcTemplate`, or `CheckedModuleId`.
- The term `runtime image` is banned except in the ban text itself or when
  quoting pre-existing code scheduled for deletion. Use `LirImage` for the
  contiguous, viewable ARC-inserted LIR image plus layout store and entrypoint
  tables.
- The word `physical` is banned except in the ban text itself or when quoting
  pre-existing code scheduled for deletion. Use `layout` only for memory shape
  facts, and use `runtime encoding` for the broader category that includes
  layouts, discriminants, callable variant encodings, erased callable code
  entries, ABI shape, and runtime schemas.
- The word `artifact` is banned except in the ban text itself or when quoting
  pre-existing code scheduled for deletion. Use `CheckedModule`,
  `CheckedModuleBuilder`, checked module cache, checked module data, platform
  relation data, or another exact producer/consumer name.
- The word `semantic` is banned except in the ban text itself or when quoting
  pre-existing code scheduled for deletion. Use checked data, checked facts,
  checked type store, source meaning, checked identity, source row position,
  `FnDef` identity, `FnSet` context, or another exact stage-owned name.
- The word `executable` is banned except in the ban text itself, when quoting
  pre-existing code scheduled for deletion, or in the phrases `executable
  binary` and `executable program`. Use the exact stage or data name instead.
- The word `obligation` is banned except in the ban text itself or when quoting
  pre-existing code scheduled for deletion. Use checked dispatch plan, erased
  callable requirement, specialization queue entry, debug assertion, or deletion
  target for old code.

## Current Deletion Targets

The current middle to remove includes these implementation areas:

- `src/mir/mono`
- `src/mir/mono_row`
- `src/mir/lifted`
- `src/mir/lambda_solved`
- `src/mir/executable` (pre-existing code scheduled for deletion)
- `src/mir/concrete_source_type.zig`
- `src/mir/lambda_solved/representation.zig`
- `src/mir/ids.zig` when no surviving owner remains
- `src/mir/artifact_names.zig` when no surviving owner remains (pre-existing code scheduled for deletion)
- `src/ir/ast.zig`
- `src/ir/lower.zig`
- `src/ir/layout.zig`
- `src/ir/mod.zig`
- `src/lir/lower_ir.zig`
- all imports of `mir` and `ir` from the post-check lowering path

Specific concepts to eliminate:

- `ConcreteSourceType`
- `RepresentationStore`
- callable-set descriptor replacement
- callable value repointing
- old middle runtime type payload stores
- old middle value conversion plans
- checked-module callable-set descriptor stores
- checked-module erased callable ABI stores
- finalized mono graph APIs that exist only to feed the old middle
- post-demand worklists
- separate stored final-call IR
- separate stored layout IR

Any remaining file that needs one of these concepts means the new IR is missing an
explicit fact and the replacement is not finished.

## Phase 0: Lock The Boundaries

Goal: make the fixed inputs and outputs explicit before adding replacement
code.

Work:

- Document the exact checked module fields the new Monotype builder consumes:
  checked bodies, checked expr/pattern/statement stores, checked type ids,
  procedure templates, resolved value refs, method registry, static dispatch
  plans, `ConstStore`, root requests, platform relation data,
  and hosted procedure metadata.
- Document the checked static-dispatch plan shape with an explicit source
  `site` and dispatcher origin. Expression-shaped dispatch sites use checked
  expression ids; non-expression sites such as iterator `for` use checked node
  ids. Value dispatch, method equality, and iterator dispatch identify the
  dispatcher by argument index. Type dispatch uses `type_only` and the checked
  dispatcher type.
- Classify current checked module fields into three groups: checked
  facts that remain, old middle representation baggage that can be deleted
  directly, and old middle fields that must first be replaced by new checked
  facts. `executable_type_payloads` and `executable_value_transforms` are pre-existing code scheduled for deletion, as are `callable_set_descriptors`,
  `erased_fn_abis`, and old middle value nodes. If current
  compile-time behavior needs information from one of those fields, replace it
  with `ConstStore` data, callable template refs, capture symbol bindings, or
  platform/hosted declarations before deleting the field.
- Write a checked module boundary checklist. Each existing checked module
  field gets exactly one entry with: current field, current producer, current
  consumers, decision (`keep`, `replace`, `move`, `delete`, or
  `add-before-delete`), new owner, new checked replacement if any, deletion
  phase, and verification grep.
- Document the exact LIR result fields the direct builder must produce:
  `LirStore`, layouts, root proc specs, root metadata, and requested-layout
  mappings. `LirImage` contains only ARC-inserted LIR, committed layouts, root
  proc ids, platform entrypoints, and target usize. Callable runtime data is not
  part of `LirImage`; the direct builder separately returns `FnSet`/`ErasedFns`
  data for `ConstStore` publication and static data export, plus runtime value
  schemas for glue and static data.
- Move hosted-proc metadata out of deleted middle namespaces before deletion.
  LIR may depend on a small hosted ABI module, but not on the old middle.
- Document the compile-time function result-storage contract before code
  changes: the `CheckedModuleBuilder` publication contract, stored function
  shape, exact `FnDef` variants, `ConstNodeId` identity/sharing,
  capture symbol bindings, generated function-set identity, exact `FnResult`
  selection rules, committed `CaptureSlot` data, and restoring cached consts
  without runtime wrappers.
- Document that imported checked modules must include every private checked
  template body reachable from exported templates, dispatch targets, promoted
  callables, and compile-time callable leaves. Privacy affects lookup and
  diagnostics only; it must not affect whole-program specialization.
- Add debug-only boundary assertions around the current public entrypoint so
  any accidental user-facing post-check error or release-build invariant check
  is visible before the replacement begins.

Checked module boundary checklist format:

- current field or module name
- current producer
- current consumers
- decision: `keep`, `replace`, `move`, `delete`, or `add-before-delete`
- new owner
- new checked replacement, when one exists
- deletion phase
- verification grep

Checked module boundary checklist:

- checked name store: keep; produced by checking; consumed by diagnostics,
  checked type publication, and method owners; new owner is `CheckedModule`.
- `module_identity`, `checking_context_identity`, and direct import checked
  module ids: keep; produced by checking publication; consumed by checked cache
  identity and import loading.
- checked CIR stores: keep `checked_types`, `checked_bodies`,
  `checked_const_bodies`, checked procedure templates, checked const templates,
  resolved value refs, nested proc sites, top-level values, root requests,
  compile-time roots, interface capabilities, exports, and provides/requires
  metadata; new owner is `CheckedModule`.
- static-dispatch data: keep method registry and checked dispatch plans; Monotype
  consumes them and removes all dispatch nodes.
- platform and hosted declarations: keep, then move hosted ABI metadata to a
  neutral module before deleting old middle namespaces.
- promoted callable wrappers and promoted body plans: replace with `FnDef`,
  `FnTemplate`, `ConstFn`, and explicit checked callable templates; delete old
  old-middle-only body-plan fields after `ConstStore` restore uses checked
  templates directly.
- `executable_type_payloads`: pre-existing code scheduled for deletion; replace
  with `ConstStore`, direct-builder `requested_layouts`, and runtime schemas;
  delete in Phase 6/7; verification grep `executable_type_payload` for pre-existing code scheduled for deletion.
- `executable_value_transforms`: pre-existing code scheduled for deletion;
  replace with ordinary `ConstStore` restore and concrete Lambda Mono
  operations; delete in Phase 6; verification grep `executable_value_transform` for pre-existing code scheduled for deletion.
- `callable_set_descriptors`: pre-existing code scheduled for deletion; replace
  with Lambda Solved function types, Lambda Mono generated callable tag unions,
  and direct-builder `fn_sets`; delete in Phase 5/6; verification grep
  `callable_set_descriptor`.
- `erased_fn_abis`: pre-existing code scheduled for deletion; replace with
  checked erased callable requirements and direct-builder `erased_fns`; delete
  in Phase 4/7; verification grep `erased_fn_abi`.
- compile-time plan stores and compile-time value stores: replace with
  `ConstStore`, `ConstNodeId`, `ConstFn`, `FnResult`, `FnSet`, `ErasedFns`, and
  `CaptureSlot`; delete old graph/plan stores in Phase 6; verification grep for
  old const graph and old compile-time value store names in pre-existing code scheduled for deletion.
- compile-time dependency summaries and const/callable instances: keep only as
  checked dependency and root publication facts; remove any field that mentions
  old middle runtime payloads, old value conversion, callable descriptors, or
  erased ABI stores.
- old owner names that include pre-existing checked boundary naming are
  scheduled for rename to `CheckedModule`, `CheckedModuleBuilder`, checked
  module cache, checked module data, or the exact store name.

Done:

- The checked input contract is a short list of checked-module-owned facts.
- The checked module no longer claims old middle callable/runtime representation
  state as part of the fixed boundary.
- Every old checked module representation field has an explicit replacement
  or deletion entry.
- The LIR output contract is a short list of LIR-owned facts.
- `design.md` documents the checked-only cache target, the builder-to-published
  checked module transition, private imported body availability, `ConstStore`
  identity, exact `FnDef` variants, and function result storage data.

## Phase 1: Add The New Post-Check Module Skeleton

Goal: create the new stage ownership without connecting it to production
lowering yet.

Add:

```text
src/postcheck/mod.zig
src/postcheck/common.zig
src/postcheck/monotype/ast.zig
src/postcheck/monotype/type.zig
src/postcheck/monotype/lower.zig
src/postcheck/monotype/specialize.zig
src/postcheck/monotype_lifted/ast.zig
src/postcheck/monotype_lifted/lift.zig
src/postcheck/lambda_solved/ast.zig
src/postcheck/lambda_solved/type.zig
src/postcheck/lambda_solved/solve.zig
src/postcheck/lambda_mono/ast.zig
src/postcheck/lambda_mono/type.zig
src/postcheck/lambda_mono/lower.zig
src/postcheck/lambda_mono/specialize.zig
src/postcheck/lir_lower.zig
```

Rules:

- Do not use `mir` in any new module name or public type name.
- Do not define a generic representation store.
- Do not add a layout IR module.
- Type-store sharing follows `design.md`: Monotype and Monotype Lifted share a
  type store; Lambda Solved and Lambda Mono own new type stores; every stage
  owns its own expression ids.
- Printers/debug dumps are allowed, but they consume the stage's explicit IR
  only.

Done:

- `zig build` can compile empty/new data structures without connecting them to
  the public lowering path.
- No new module imports old middle modules except in temporary focused tests
  that are deleted before the public switch.

## Phase 2: Implement Monotype IR

Goal: lower checked modules to closed monomorphic Monotype IR and eliminate
static dispatch/source `for`.

Data structures:

- `Monotype.TypeStore`
- `Monotype.TypeId`
- `Monotype.Expr`
- `Monotype.Pat`
- `Monotype.Stmt`
- `Monotype.Def`
- `Monotype.Program`
- `Monotype.LoopExpr`, with explicit loop parameters and initial values
- `Monotype.ContinueExpr`, with explicit next values for every loop parameter
- `Monotype.DispatchOwnerHead`
- `dispatchOwnerHead(types, ty)`, a total operation over Monotype type content
- `Monotype.Field` and `Monotype.Tag` ordered spans, where span index is the
  source row position
- `Monotype.FnDef`, a checked function definition with no capture record,
  layout, erased ABI, callable tag, or lowered call target
- `Monotype.NestedFn`, the checked `(owner template, nested site)` identity for
  every expression-position function
- `Monotype.Spec`
- `Monotype.SpecializationQueue`

Work:

- Clone checked types into monomorphic type nodes.
- Integrate row finalization into Monotype type construction:
  record fields and tag variants are emitted in lexicographic order by name;
  tag payloads are emitted in payload position order.
- Preserve static-dispatch owner identity in type content, not as a duplicate
  field on every type node: builtin content derives builtin owner heads,
  method-owning nominal/alias/opaque definitions carry exact `TypeDef` values,
  and non-owning structural nodes derive `none`.
- Remove old row-order storage where it duplicates span position. Row position
  is the field/tag index inside the stored span. Tag
  payload position is the payload index inside its tag. Runtime representation
  order is chosen later by LIR layout commitment.
- Instantiate nominal, alias, and opaque backing facts from checked type facts
  without declaration scans. Store backing authority explicitly:
  `inspectable` when later stages may inspect through the declaration,
  and `runtime_layout_only` when only layout lowering may use the backing.
  Preserve opacity/interface permission separately from static-dispatch owner
  identity.
- Instantiate checked procedure templates into `Monotype.Def` records.
- Require every expression-position lambda/closure lowered into Monotype to
  carry the checked nested function identity and checked source function type
  published during checked module publication. Post-check stages must not name
  nested functions from generated symbols, traversal order, body shape, capture
  shape, runtime layout, or LIR procedure ids.
- Build the root-driven specialization queue from explicit root requests.
- Lower checked expressions, patterns, and statements into Monotype nodes.
- Resolve static dispatch plans during lowering:
  derive `MethodOwner` from instantiated monomorphic dispatcher type, perform an
  exact method-registry lookup, verify the callable type, and emit direct calls
  or checked structural equality.
- Preserve owner-bearing type content while resolving dispatch: builtin content
  derives builtin owners, method-owning nominal/alias declarations stay exact
  `TypeDef` owners, and transparent backing representation remains
  separate from owner identity.
- Treat `FnDef` as a closed checked union: local checked
  template, imported checked template, nested function, local promoted callable
  template, imported promoted callable template, local hosted function, imported
  hosted function, or checked-stage generated function. Do not encode
  specialization, capture layout, LIR proc id, callable member, or runtime code
  identity in it. Post-check generated functions use stage-local symbols, not
  `FnDef`.
- Lower source `for` during Monotype construction:
  call resolved `.iter`, keep a compiler-created loop-carried iterator
  parameter, call resolved `.next`, and emit ordinary loop/match/control-flow
  nodes. `continue` supplies the next iterator value explicitly; there is no
  hidden assignment state.
- Resolve compile-time value references from `ConstStore`.
  Do not use old middle value requests.

Delete or change during this phase:

- Replace the pre-existing checked iterator-dispatch type scheduled for deletion
  with checked iterator dispatch plans. The checked module may still publish a
  plan, but the plan is not a post-check repair item.
- Remove any owner-intersection API from method registry consumers.
- Remove any duplicate owner-head field, duplicate row-position field,
  pre-layout runtime tag discriminants, or field offsets from Monotype type
  nodes. Monotype rows use only their stored span order.
- Ensure Monotype output has no dispatch, type dispatch, method equality, or
  source `for` variants.

Tests:

- static dispatch on builtins and nominals
- structural equality through checked equality plans
- method equality through checked equality plans
- iterator `for` over `List.iter`
- iterator `for` over numeric ranges
- imported method dispatch
- method dispatch on method-owning aliases/transparent named types
- dispatch through opaque named types without inspecting hidden backing
- generic static dispatch that specializes to multiple owners
- compile-time roots using dispatch and iterators
- split-vs-unsplit module dispatch tests proving owner identity and generated
  direct calls are unaffected by module boundaries

Done:

- Monotype lowering can produce closed IR for all focused post-check fixtures.
- Boundary verification proves no checked source dispatch or source `for`
  survives Monotype.
- Boundary verification proves every Monotype loop has matching parameter,
  initial-value, and continue-value arity and types.
- Boundary verification proves Monotype contains no runtime discriminants,
  layout ids, or callable representation ids.

## Phase 3: Implement Monotype Lifted IR

Goal: remove closures and local functions from expression position.

Data structures:

- `MonotypeLifted.Program`
- `MonotypeLifted.Def`
- `MonotypeLifted.Fn`
- `MonotypeLifted.Expr`
- capture spans on lifted function definitions

Work:

- Compute free variables for local lambdas and local functions.
- Lift local functions to explicit definitions with stable generated symbols.
- Replace closure expressions with references to lifted function symbols.
- Preserve multi-argument Roc function arity exactly.
- Preserve recursive local functions through explicit recursive groups.
- Keep function values as ordinary typed values. Do not pack captures here.

Tests:

- closure capturing locals
- closure capturing another closure
- recursive local function
- generic higher-order call
- iterator method implemented with a capturing closure

Done:

- Monotype Lifted IR has no lambda expressions and no local function
  definitions inside expression nodes.
- Every lifted function lists its captures explicitly.

## Phase 4: Implement Lambda Solved IR

Goal: put callable flow into the type graph, Cor-style.

Data structures:

- `LambdaSolved.TypeVarId`
- `LambdaSolved.TypeStore`
- `LambdaSolved.Content.func { args, callable, ret }`
- `LambdaSolved.Content.lambda_set`
- `LambdaSolved.Content.erased`
- `LambdaSolved.Expr`, same lifted shape as Monotype Lifted IR
- `LambdaSolved.Def`

Work:

- Instantiate Monotype Lifted types into Lambda Solved type variables.
- Add a fresh callable slot to every function type.
- Preserve Roc fixed arity: every function type stores the full ordered
  argument list plus one callable slot and one result type. Do not encode
  multi-argument functions as nested unary functions.
- Treat references to lifted function symbols as singleton lambda sets with
  capture maps.
- Unify callable slots through variable flow, let bindings, records, tags,
  tuples, lists, branches, and calls.
- Propagate erased callable requirements through the same type graph.
- Define the only allowed producers of erased callable requirements:
  checked platform or hosted declarations whose ABI metadata contains an
  erased callable slot, exposed ABI declarations explicitly marked erased by
  checked ABI metadata, checked builtin operations listed as erased-callable
  boundaries in builtin metadata, checked low-level operations whose signature
  explicitly contains erased callable parameters/results, and checked root ABI
  metadata for values later consumed by LirImage or glue when that metadata names
  erased callable slots.
- Generalize and instantiate polymorphic definitions.
- Solve recursive definition groups as groups.
- Publish solved Lambda Solved IR only after every callable slot is resolved.

Do not implement:

- descriptor replacement
- callable value repointing
- late payload publication
- a representation store
- a post-demand worklist

Tests:

- direct closure call
- finite higher-order call with one member
- finite higher-order call with multiple members
- erased callable call
- erased callable requirement from platform/hosted/exposed ABI facts
- captured recursive function
- generic higher-order function used at multiple concrete types
- lambda set flowing through records and tags

Done:

- Lambda Solved type graph contains the complete callable flow answer.
- There is no separate table needed to understand function value
  representation.

## Phase 5: Implement Lambda Mono IR

Goal: make callable representation explicit and remove function types.

Data structures:

- `LambdaMono.TypeStore`
- generated finite callable tag-union types
- generated capture record types
- `LambdaMono.Expr.direct_call`
- `LambdaMono.Expr.indirect_erased_call`
- `LambdaMono.Expr.packed_erased_fn`
- `LambdaMono.Expr.match_`
- `LambdaMono.Spec`

Work:

- Lower finite lambda sets to generated tag unions.
- Lower captured finite members to generated capture records in tag payloads.
- Lower erased callable values to packed erased callable values.
- Lower calls through finite lambda sets to matches over generated tag unions,
  with direct calls in every branch.
- Lower calls through erased callable values to indirect erased calls.
- Specialize functions by function symbol, solved function type, and capture
  shape.
- Ensure generated callable member identity lives in the generated type node as
  compiler-owned checked variant ids, not in a side descriptor table and not in
  display text.
- Do not add a generic conversion expression. Any runtime operation that must
  survive to statement lowering must be a concrete Lambda Mono expression. Pure
  layout choices are handled during direct LIR layout selection and expression
  lowering.
- Do not add generic conversion plan tables or post-hoc conversion lowering
  paths under any name. If a later stage sees a mismatch between Lambda Mono IR
  and committed layouts, that is an invariant failure; fix the producer earlier.
- Treat recursive boxing, list backing representation, transparent declared
  backing, and zero-sized representation as layout-lowering responsibilities
  for ordinary concrete constructors, field reads, tag payload reads, calls, and
  patterns. If a language operation really allocates or reads an explicit `Box`,
  lower it as a named concrete expression or low-level operation with
  meaning defined by its producer stage, not as an after-the-fact conversion.
- Keep runtime discriminants out of Lambda Mono types. Layout commitment assigns
  runtime variant slots and discriminants later.

Tests:

- finite callable singleton
- finite callable with two members
- captured finite callable
- zero-capture erased callable
- captured erased callable
- erased adapter from finite callable
- generic higher-order function producing distinct Lambda Mono
  specializations

Done:

- Lambda Mono IR has no function type.
- Lambda Mono IR has no value-call node.
- Lambda Mono IR has no generic conversion node.
- Lambda Mono IR has no generic conversion plan table.
- No callable descriptor table is needed to lower the program.
- Lambda Mono IR contains no runtime tag discriminants or layout ids.

## Phase 6: Implement ConstStore Eval Result Storage And Cached Const Restore

Goal: keep compile-time constants in checked form across cache boundaries while
still evaluating them through LIR.

Data structures:

- `ConstStore`
- `ConstValue`
- `ConstNodeId`
- `ConstFn`
- `ConstCapture { binder, value }`
- exact `FnDef` variants for local/imported checked templates,
  nested functions, local/imported promoted callable templates,
  local/imported hosted functions, and checked-stage generated functions
- temporary `FnResult`
- `FnSet`
- `FnVariant`
- `ErasedFns`
- `ErasedFn`
- `FnTemplate`
- `CaptureSlot`
- `ConstRestore`

Work:

- Keep compile-time finalization mutable only inside `CheckedModuleBuilder`.
  Compile-time lowering/evaluation may receive builder-owned checked result
  sinks, but it must not mutate a published `CheckedModule` or create a
  second post-check state.
- Reject any compile-time operation whose result cannot be represented as a
  checked Roc value in `ConstStore`. Compiler runtime representation facts such
  as runtime addresses, allocation identity, layout ids, runtime discriminants,
  field offsets, LIR proc ids, backend symbols, object-format details, and
  backend state are outside the checked value domain. Host interaction exists
  only at runtime, so host handles and host results cannot be compile-time
  values. Pointer-sized Roc values are the only pointer-size-sensitive value
  category; if Roc exposes them to compile-time evaluation, define their checked
  cache format before allowing publication.
- Define `ConstScalar` as a closed checked scalar representation with fixed-width
  signed integers, fixed-width unsigned integers, f32 bits, f64 bits, and Dec
  bits only. It must not contain target-sized integers, pointers, host handles,
  layout ids, runtime discriminants, field offsets, LIR proc ids, or backend
  symbols.
- Route compile-time evaluation failures through checking finalization while the
  module is unpublished. User-written compile-time crashes, exhausted
  compile-time limits, invalid compile-time host interaction, and unsupported
  compile-time operations become checking diagnostics. Post-check invariant
  failures during compile-time lowering or interpretation remain compiler bugs.
- Replace old middle runtime payload and callable descriptor data in compile-time
  stores with `ConstStore` nodes.
- Give stored consts explicit `ConstNodeId`s. Sharing is represented by multiple
  references to the same node id. The builder may reserve node ids before
  storing children so repeated references to the same acyclic runtime value use
  one `ConstNodeId`; every reserved node must be filled exactly once before
  publication. Recursive non-function source values are checking errors and
  never become valid `ConstStore` entries.
- Represent callable leaves as checked function references plus checked
  captured `ConstNodeId`s bound to exact checked pattern binders.
- Define `FnTemplate` as the pair of checked `FnDef` identity and checked source
  function type. Carry it from Monotype through lifting, Lambda Solved, Lambda
  Mono, and direct LIR lowering instead of making LIR recover checked function
  types from layouts or generated procedure ids.
- During compile-time lowering, have Lambda Mono/LIR lowering publish temporary
  `FnResult` data scoped by `FnSet` identity. Finite singleton sets
  select their only `FnVariant`; finite multi-variant sets read the runtime
  discriminant and look it up inside the explicit `FnSet`; erased sets read the
  erased entry procedure and look it up inside the explicit `ErasedFns`.
- Have the direct LIR builder publish committed `CaptureSlot` data for every
  generated callable representation that can be stored during compile-time
  evaluation. A capture slot identifies the checked binder whose value occupies
  the slot; it is not named by generated symbol or field position alone.
- Have the LIR interpreter use that temporary `FnResult` data to store function
  values in `ConstStore`.
- When restoring cached consts, make Monotype lowering turn `ConstStore` nodes
  into ordinary Monotype expressions.
- Restore zero-capture function values as `Monotype.FnDef`.
- Restore capturing function values as compiler-generated Monotype lambdas whose
  bodies are cloned from the checked callable template named by
  `FnDef`, whose parameters are alpha-renamed, and whose free variables
  are ordinary expressions generated from the captured `ConstNodeId`s.
- Load imported callable bodies from the referenced checked module templates,
  including private reachable helper bodies. Do not reconstruct imported bodies
  from source text, display names, runtime values, or generated callable shapes.
- Prohibit wrapper-style restore that calls an already-packed runtime function
  value. Restoring a cached const builds an ordinary Monotype callable from the
  explicit checked template and stored const captures, so lifting and lambda
  solving see ordinary callable flow.
- Remove compile-time restore plans that mention old middle runtime type
  payloads, old middle value conversion plans, callable-set descriptors, erased ABI payloads, or
  layout creation.

Tests:

- compile-time scalar, record, tuple, tag, list, box, and nominal values
- compile-time values containing direct function leaves
- compile-time values containing captured function leaves
- `ConstStore` DAG sharing
- recursive non-function values are rejected by checking, converted to
  `Malformed`, and never published as valid `ConstStore` entries
- storing finite and erased function eval results
- imported compile-time values containing callables
- compile-time callable values passed through records/tags/lists and called at
  runtime
- split-vs-unsplit compile-time callable tests proving cached imported
  callables produce the same Lambda Solved and Lambda Mono callable flow as
  local callables

Done:

- Checked modules cache only `ConstStore` checked-stage values.
- No checked compile-time store contains old middle runtime payloads, callable-set
  descriptors, erased callable ABI decisions, or layout data.
- Storing compile-time function eval results uses explicit `FnResult` contexts
  and `CaptureSlot` data published by LIR lowering.
- Imported compile-time callable values flow through Lambda Solved like local
  callable values.

## Phase 7: Implement Direct Lambda Mono IR To LIR Lowering

Goal: produce existing LIR directly from Lambda Mono IR.

Data structures:

- `PostcheckLirLower.Builder`
- `LirLowerOutput`
- `TypeLayoutMap`
- `ProcMap`
- `LocalEnv`
- `RuntimeValueSchemaBuilder`
- `ErasedCallableCodeMapBuilder`
- internal builders with explicit input/output contracts:
  `LayoutBuilder`, `ProcBuilder`, `LocalBuilder`, `PatternBuilder`,
  `CallableLowerer`, and `SchemaPublisher`

Work:

- Allocate LIR procedure placeholders for every reachable Lambda Mono
  procedure.
- Commit layouts from Lambda Mono types:
  primitives, records, tuples, lists, boxes, nominals, tag unions, generated
  finite callable tag unions, and erased callable payloads.
- Lower Lambda Mono expressions to statement-only LIR.
- Lower matches and patterns directly to LIR switches and joins.
- Assign runtime tag discriminants, field offsets, callable variant encodings,
  and erased callable payload layouts here and publish the results explicitly in
  LIR-owned outputs.
- Lower loops to LIR joins/jumps or loop control nodes.
- Lower direct calls to `assign_call`.
- Lower erased calls to `assign_call_erased`.
- Lower packed erased callables to `assign_packed_erased_fn` with explicit
  final-drop plan.
- Publish runtime value schemas from committed layouts and Lambda Mono nominal
  type nodes.
- Publish erased callable code map from Lambda Mono procedure/callable data.
- Preserve current LIR result shape so ARC/backends do not need a redesign.
- Return exactly one `LirLowerOutput` containing `LirStore`, layouts, root proc
  specs, root metadata, requested layouts, runtime schemas, `FnSet` data,
  `ErasedFns` data, `CaptureSlot` data, and erased callable entry data.
  Consumers borrow those fields and must not maintain alternate stores for the
  same facts.
- Enforce direct-builder internal contracts:
  each internal builder consumes only Lambda Mono IR, committed layouts,
  checked identities explicitly passed into the direct builder, and the
  LIR result being constructed. No internal builder may read checked source
  bodies, display names, runtime bytes, backend symbols, or old middle data.
- Publish the direct-builder result data needed outside normal LIR execution:
  runtime value schemas for glue/static data, requested-layout mappings for
  static data, `FnSet` data for finite function eval results, `ErasedFns` data
  for erased function eval results, and committed `CaptureSlot` data. None of
  this builder result data is stored in `LirImage`.
- Treat any mismatch between Lambda Mono IR and committed layouts as an
  invariant failure. The direct builder must not invent conversion operations to
  repair it.

Do not implement:

- a persisted layout IR
- source-to-runtime conversion tables
- layout recovery from runtime encodings
- callable member lookup by generated display names
- comparison against old `ir`/`lower_ir` output to decide new behavior

Tests:

- focused LIR snapshots for records, tags, matches, loops, lists, closures,
  erased callables, hosted calls, and platform entrypoints
- compile-time evaluation through the LIR interpreter
- LirImage construction for exported values with records, tags, functions,
  lists, and erased callables

Done:

- The direct builder returns the same public LIR result type consumed by ARC.
- `src/lir/lower_ir.zig` has no remaining purpose.

## Phase 8: Switch The Public Pipeline

Goal: make `src/lir/checked_pipeline.zig` use only the new path.

Work:

- Replace the current lowering chain with:

```text
postcheck.Monotype.lower
postcheck.MonotypeLifted.lift
postcheck.LambdaSolved.solve
postcheck.LambdaMono.lower
postcheck.LirLower.run
Arc.insert
```

- Route runtime lowering, compile-time lowering, compile-time dependency
  summaries, platform entrypoint lowering, and LirImage lowering through
  the same path.
- Remove any branch that can fall back to the old path.
- Keep checked module verification at the boundary.
- Keep LIR ARC insertion after LIR construction.

Done:

- There is one public checked-module-to-LIR lowering path.
- It contains no conditional fallback to the old middle.

## Phase 9: Delete The Old Middle

Goal: remove every last implementation trace between checked modules and LIR
that belongs to the old architecture.

Delete:

```text
src/mir
src/ir
src/lir/lower_ir.zig
```

Move before deleting the old middle:

- hosted ABI metadata to a small non-middle module consumed by LIR and
  post-check lowering
- reusable symbol helpers to a neutral module
- reusable literal ids to a neutral module

Remove imports and build references:

- `@import("mir")`
- `@import("ir")`
- `LowerIr`
- `ir.Lower`
- old middle modules in `build.zig`
- old middle module docs/tests
- any LIR import that exists only to reach old middle hosted/procedure metadata

Remove checked module fields only if they exist solely for the deleted
middle. Do not change checked CIR, checked types, checked cache ids,
or user-facing reporting behavior.

Required searches must return no old implementation references:

```text
rg "ConcreteSourceType" src
rg "RepresentationStore" src
rg "FinalizedMonoSpecializationGraph" src
rg "post_demand|PostDemand|Obligation" src # pre-existing code scheduled for deletion
rg "generic conversion|post-hoc conversion|after-the-fact conversion" src
rg "callable_set_descriptors" src
rg "CallableSetDescriptorStore" src
rg "ErasedFnAbiStore|erased_fn_abis" src
rg "executable_type_payload|session_executable" src # pre-existing code scheduled for deletion
rg "ExecutableTypePayloadStore|ExecutableValueTransform" src # pre-existing code scheduled for deletion
rg "lower_ir|LowerIr" src
rg "ir\\.Lower|fromExecutable" src # pre-existing code scheduled for deletion
rg "@import\\(\"mir\"\\)|@import\\(\"ir\"\\)" src
```

If a reference remains, either it is renamed/moved neutral infrastructure or the
deletion is incomplete.

## Phase 10: Verification

Goal: prove the new middle is the only middle and the compiler still works.

Run focused checks before full checks:

- Monotype stage unit tests
- static dispatch tests
- iterator `for` tests
- closure lifting tests
- lambda solving tests
- lambda mono callable tests
- direct LIR builder tests
- compile-time constant tests
- LirImage tests
- split-vs-unsplit module equivalence tests for callable flow, static dispatch,
  compile-time callables, generated callable representations, layouts, ARC, and
  LirImage metadata
- cached-import tests for compile-time values containing captured and
  zero-capture callables

Then run:

```text
roc check
roc test
zig build minici
```

Add debug-only boundary verification:

- Monotype contains no source dispatch/source `for`.
- Monotype contains no open checked type variables.
- Monotype loop parameters, initial values, and continue values match exactly.
- Monotype contains no runtime tag discriminants, layout ids, or callable
  representation ids.
- Monotype Lifted contains no closure expression.
- Lambda Solved has no unresolved callable slot.
- Lambda Mono has no function type and no value-call node.
- Lambda Mono contains no runtime tag discriminants or layout ids.
- Checked compile-time stores contain only `ConstStore` data.
- LIR lowering consumes only Lambda Mono IR.
- ARC-complete LIR is the only backend input.

Audit invariant helpers:

- every helper whose only purpose is compiler-maintenance validation is
  debug-only
- any validation loop over layouts, callable encodings, schemas, static
  dispatch plans, or stage IRs is absent from release builds
- release builds either execute no invariant code or reach `unreachable` only
  after an inlined condition proves an impossible state

Release-build invariant helpers must compile away. Anything that loops only to
validate compiler-maintenance invariants belongs behind debug-only checks.

Audit scope:

- During replacement work, the terminology audit applies to `design.md`,
  `plan.md`, and all new post-check roots.
- When the public pipeline switches, expand the audit to every module in the
  checked-module-to-LIR path, including renamed checked boundary modules,
  post-check modules, direct LIR lowering, compile-time finalization, and
  LirImage construction.
- After old middle deletion, no allowlist may remain for old middle terminology
  in the checked-module-to-LIR path.

## Phase 11: Documentation Cleanup

Goal: make the repository describe only the new architecture.

Work:

- Keep `design.md` as the authoritative post-check architecture.
- Remove docs that describe the deleted middle as current architecture.
- Update contributor docs and AGENTS references if they name the old middle.
- Update tests and debug dump stage names to use:
  `monotype`, `monotype_lifted`, `lambda_solved`, `lambda_mono`, and `lir`.
- Remove stale comments that mention old middle layout stages.

Done:

- A new contributor can read the docs and see one architecture:
  checked modules -> Cor-style post-check IRs -> LIR.
