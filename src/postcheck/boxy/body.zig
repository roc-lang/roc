//! Lowering one worker's checked body to LIR (design.md "`.boxy` Runtime
//! Lowering", "Dynamic Values In LIR").
//!
//! Statements are emitted in execution order. The builder always holds the
//! statement "hole" the next statement goes into: a placeholder statement
//! that emitting replaces in place, leaving a fresh hole as the new
//! statement's successor. Control flow keeps one hole per pending successor.
//!
//! Pending work lives on an explicit task stack: an expression task lowers a
//! checked expression and pushes the local holding its value onto the value
//! stack; continuation tasks pop the values their expression's operands
//! left. Source depth never becomes native call depth.
//!
//! Descriptors a body needs are emitted into the worker's entry chain, which
//! runs before the dynamic frame region is allocated and before the body.

const std = @import("std");
const collections = @import("collections");
const base = @import("base");
const can = @import("can");
const check = @import("check");
const layout = @import("layout");
const lir_core = @import("lir_core");

const types = @import("types.zig");
const lower = @import("lower.zig");
const Codec = @import("codec.zig");
const StoredFn = @import("stored_fn.zig");
const Match = @import("match.zig");

const Allocator = std.mem.Allocator;
const CIR = can.CIR;
const checked = check.CheckedModule;
const static_dispatch = check.StaticDispatchRegistry;
const NumeralLiteral = can.ModuleEnv.NumeralLiteral;
const LIR = lir_core.LIR;
const LirStore = lir_core.LirStore;
const CheckedArithmetic = lir_core.CheckedArithmetic;
const LocalId = LIR.LocalId;
const CFStmtId = LIR.CFStmtId;
const TypeId = types.TypeId;
const ModuleIndex = types.ModuleIndex;
const Program = lower.Program;
const StaticDescs = @import("static_descs.zig").StaticDescs;
const Tables = @import("tables.zig");
const WorkerId = lower.WorkerId;

/// A hidden word: a descriptor, dictionary, or literal thunk the body reads,
/// as a static word or as a recipe over the worker's inputs (`tables.zig`).
pub const Word = Tables.Operand;
const invariant = types.invariant;
const invariantFmt = types.invariantFmt;

const bodies_mod = check.CheckedBodies;
const ExprId = bodies_mod.ExprId;
const PatternId = bodies_mod.PatternId;
const StatementId = bodies_mod.StatementId;
const BinderId = bodies_mod.BinderId;

/// Lower worker `id`'s body into its procedure.
pub fn lowerWorker(program: *Program, id: WorkerId) Allocator.Error!void {
    var builder = try Builder.init(program, id);
    defer builder.deinit();
    // Statement emission records the procedure's calls and joins, from
    // which its finish proves the tail calls (design.md "Tail Return
    // Continuations").
    program.result.store.tail_call_builder = &builder.tail_calls;
    defer program.result.store.tail_call_builder = null;
    try builder.lowerBody();
    program.worker(id).needs = try builder.finishNeeds();
}

/// One unit of pending lowering work on the builder's task stack.
pub const Task = union(enum) {
    /// Lower an expression as its parent reads it: a row-coerced result
    /// through its coercion (`lowerRowCoercion`).
    expr: ExprId,
    /// Lower an expression's own value: the value a row coercion converts,
    /// or an expression a checked table names rather than a parent.
    own_expr: ExprId,
    stmt: StatementId,
    /// Lower the body of compile-time root `root` of this body's module
    /// (`lowerRootBody`).
    root_body: checked.ComptimeRootId,
    /// Pop the value of a hoisted pattern root's base expression, match it
    /// against the root's pattern, and push the root's value
    /// (`rootPatternMatch`).
    root_pattern_match: checked.ComptimeRootId,
    /// Push whether the value a custom literal pattern matches equals the
    /// pattern's literal converted at its type
    /// (`checked_bodies.LiteralPatternGuard`).
    literal_pattern_guard: PatternId,
    /// Push the value a custom literal pattern matches, which the pattern
    /// binds.
    literal_pattern_value: PatternId,
    /// Push a custom literal pattern's literal converted at the pattern's
    /// type.
    literal_pattern_conversion: PatternId,
    /// Pop the value of a `decl` statement's expression and bind its pattern.
    bind: struct { pattern: PatternId, ty: TypeId },
    /// Pop and drop one value.
    discard,
    /// Re-tag the popped ground value of type `from` into the open-row type
    /// `to`: each reached tag row's discriminant by its tag's name, under
    /// `to`'s descriptor.
    retag: struct { from: TypeId, to: TypeId },
    /// After a divergent statement: no statement is reached, and the value
    /// of its block, of this type or of no type when the checker gave it
    /// one holding the erroneous type, is never produced.
    diverged: ?TypeId,
    /// Crash as a dispatch that cannot run does; its value is of type
    /// `result`.
    dispatch_crash: struct { kind: lower.CrashDictionary, result: TypeId },
    /// Crash with a checked runtime error, whose value is of this type, or
    /// of no type when the checker gave it the erroneous type.
    runtime_error: ?TypeId,
    /// Pop the arguments of a call to `callee` and call it.
    /// `adapt` is set for a callee storing record fields of its signature
    /// in presence slots: the types its scheme takes and the arguments'.
    finish_call: struct { callee: WorkerId, hidden: []Word, bound: []const TypeId, arg_types: []const TypeId, ret: TypeId, result: TypeId },
    finish_erased_call: struct { fn_ty: TypeId, arg_count: u32, result: TypeId },
    finish_low_level: struct { op: LIR.LowLevel, arg_count: u32, result: TypeId, arg_types: []const TypeId = &.{} },
    /// Return to the enclosing expression's source region.
    restore_region: base.Region,
    finish_record: ExprId,
    finish_tuple: ExprId,
    finish_tag: ExprId,
    /// `finish_tag` at the type of the nominal the tag constructs.
    finish_tag_at: struct { expr: ExprId, ty: TypeId },
    finish_list: ExprId,
    finish_str: struct { count: u32 },
    finish_eq: struct { negated: bool, operand: TypeId },
    /// Pop a Bool and check it as an `expect`.
    finish_expect: ExprId,
    /// Pop the condition of if-branch `index` of `expr`.
    /// A null `join` is a construct in tail position: each branch returns.
    if_test: struct { expr: ExprId, index: u32, join: ?LIR.JoinPointId, merge: ?MergeId },
    /// Pop a value and jump to `join` with it.
    jump: LIR.JoinPointId,
    /// Jump to a join that carries no value (a loop's head or exit).
    jump_without_value: LIR.JoinPointId,
    /// Push the `Numeral` argument of a `from_numeral` call.
    numeral_arg: struct { literal: NumeralLiteral, ty: TypeId },
    /// Pop a `for` loop's iterable and run the loop.
    for_start: struct { loop: u32, pattern: PatternId, body: ExprId, plan: static_dispatch.IteratorForPlanId },
    /// Rebind a merge's binders to their values before its construct.
    restore_merge: MergeId,
    /// Rebind a merge's binders to its join's parameters.
    enter_merge: MergeId,
    /// Pop a loop condition: enter the body or leave the loop.
    loop_test: struct { body: ExprId, head: LIR.JoinPointId, exit: LIR.JoinPointId },
    end_loop,
    /// Leave the innermost scheme alias scope (`Builder.alias_scopes`).
    leave_alias_scope,
    /// Leave the frames a literal chain entered (`SavedScope`).
    restore_scope: SavedScope,
    /// A loop whose pattern checking rejected, its iterable evaluated:
    /// crash as the iterator dispatch that cannot run, or as rejected code.
    rejected_for: static_dispatch.IteratorForPlanId,
    /// Fill the slots of a recursive value declaration's binders, once the
    /// declaration's pattern bound them.
    fill_recursive_slots: PatternId,
    /// Continue emitting at `hole`.
    resume_at: CFStmtId,
    finish_field_access: ExprId,
    /// Pop a tag union value and compare its discriminant with `tag`'s.
    finish_discriminant_eq: struct { value_ty: TypeId, tag: check.CheckedNames.TagLabelId, negated: bool },
    finish_tuple_access: ExprId,
    /// Pop a value of this type, report it through `dbg`, and push it back.
    finish_dbg: TypeId,
    /// Restore stored constant `node` at `ty`. `root` is the compile-time
    /// root whose own value the node is, when the restore reads one: a read
    /// of a failed root's value then fails as that root did, reported once
    /// by the root (`CrashExtra.failed_value`).
    restore: struct { source: lower.ConstSource, node: checked.ConstNodeId, ty: TypeId, root: ?checked.ComptimeRootId = null },
    /// Push the function value a callable-eval binding stored.
    restore_callable_eval: struct { module: ModuleIndex, template: checked.CallableEvalTemplateId, ty: TypeId },
    /// Pop a restored closure's capture values and build it.
    /// `worker_ty` is the closure's type as its worker stores it, and `ty`
    /// the restored value's.
    /// Pop a stored function's captured values and pack them with its type
    /// table at `inputs` (`stored_fn.zig`).
    finish_stored_fn: struct { worker: WorkerId, shape: StoredFn.CaptureShape, inputs: []const Word, capture_types: []const TypeId, capture_descs: []const Word },
    finish_restore_closure: struct { worker: WorkerId, count: u32, descs: []const Word, dicts: []const Word, capture_types: []const TypeId, worker_ty: TypeId, ty: TypeId },
    finish_restore_struct: struct { ty: TypeId, count: u32 },
    finish_restore_tag: struct { ty: TypeId, name: types.NameId, count: u32 },
    finish_static_list: struct { ty: TypeId, count: u32, capacity: u64 },
    finish_static_box: struct { ty: TypeId, sequence_step_offset: ?u32 = null },
    remember_static_value: lower.Program.StoredValueKey,
    /// Push a value of `ty` for an expression whose evaluation diverged.
    unreachable_value: TypeId,
    /// Push the `{}` value of a construct whose value nothing reads.
    unreachable_or_unit,
    /// Pop a generalized local procedure call's explicit arguments and call
    /// its function value with them after its own scheme's words.
    /// `params` and `ret` are the procedure's signature, `arg_types` the
    /// call's arguments' types and `result` its result's.
    finish_local_call: struct { closure: LocalId, words: []const Word, params: []const TypeId, ret: TypeId, arg_types: []const TypeId, result: TypeId },
    /// Push an interpolation's literal segments as the `List(Str)` of `ty`.
    segments_arg: struct { expr: ExprId, ty: TypeId },
    /// Pop an interpolation's assembler and call it with the interpolated
    /// values.
    finish_interpolation: ExprId,
    /// Pop a literal conversion's `Try` result and push the literal's value.
    finish_literal_conversion: FinishLiteralConversion,
    finish_interpolation_call: struct { assembler: LocalId, assembler_ty: TypeId, plan: static_dispatch.IteratorForPlanId, count: u32, result: TypeId },
    /// Pop a `Str` interpolation's values and write them once with its
    /// non-empty segments (`str_from_parts`); its assembler is not called.
    finish_str_from_parts: struct { expr: ExprId, count: u32, result: TypeId },
    /// Pop a value and a hasher and hash the value structurally.
    finish_hash: struct { value: TypeId, result: TypeId },
    /// Pop an `expect`'s failing `?` value and fail the expect with it.
    finish_expect_err: struct { expr: ExprId, value: TypeId },
    /// Push the text of the string literal at this CIR node (an
    /// expression or a string literal pattern) as a `Str` value.
    push_value_quote: u32,
    /// Push `local` as a value.
    push_value: LocalId,
    /// Pop a value; the enclosing procedure returns it.
    ret,
    /// Pop the left operand of a short-circuit operator.
    short_circuit: struct { is_and: bool, rhs: ExprId, join: LIR.JoinPointId },
    /// Pop the scrutinee of a match and compile its branches.
    match_start: ExprId,
    /// Continue a match's decision-tree emission with the statement that
    /// answers its outstanding request (null on its first step).
    match_advance: struct { state: *Match.State, input: ?CFStmtId },
    /// Pop a match guard's value into `cond`, continuing at `next`.
    match_guard_done: struct { cond: LocalId, next: CFStmtId },
};

/// A literal conversion the compile-time evaluator runs where the literal
/// is: the literal, its conversion's `Try` type, and the literal's type.
pub const FinishLiteralConversion = struct { node: u32, kind: LIR.LiteralRejectionKind, try_ty: TypeId, result: TypeId };

/// A literal a body converts: a literal expression, converted at its own
/// type, or a custom literal pattern's literal, converted at the pattern's.
pub const LiteralAt = union(enum) {
    expr: ExprId,
    pattern: PatternId,

    /// The literal's CIR node.
    pub fn node(self: LiteralAt) u32 {
        return switch (self) {
            .expr => |expr| @backingInt(expr),
            .pattern => |pattern| @backingInt(pattern),
        };
    }
};

/// A control-flow construct's merge of the binders it reassigns (a checked
/// state merge plan): their values before it, and the join parameters that
/// carry their values after it.
pub const Merge = struct {
    binders: []const BinderId,
    pre: []LocalId,
    params: []LocalId,
};

/// Names a `Merge` by its index among the builder's merges.
pub const MergeId = u32;

/// What a join's parameters are: an optional value, then the binders of its
/// merge.
pub const JoinInfo = struct {
    value: ?LocalId,
    merge: ?MergeId,
};

/// A loop being lowered: its exit join, and whether anything leaves through
/// it (a failed condition, or a break).
pub const LoopExit = struct { join: LIR.JoinPointId, reached: bool };

/// A value a pattern tests, with the value's type.
pub const PatternItem = struct { pattern: PatternId, value: LocalId, ty: TypeId };

/// A scheme alias's scope as one use instantiates it: the checked evidence
/// parameters its dictionaries stand for, those dictionaries, and the
/// environment it extends.
const AliasScope = struct {
    params: []const u32,
    dicts: []const Word,
    /// The scope's literal arguments, one per literal requirement (design.md
    /// "Literal Arguments"), unless `deferred` says where they come from.
    literals: []const Word = &.{},
    literal_requirements: []const checked.LiteralRequirement = &.{},
    /// A literal chain's frame reads few of a scheme's literal arguments, so
    /// composing them all would make the chains of a scheme with n literals
    /// n squared: each read composes the one it names.
    deferred: ?DeferredLiterals = null,
    /// A literal chain's frame reads few of a scheme's dictionaries, and its
    /// other requirements can name types only the scopes the chain never
    /// entered bind: each read composes the one it names.
    deferred_dicts: ?DeferredDictionaries = null,
    /// The declaration context `dicts` come from, for the scope a local
    /// procedure's own scheme is resolved in when it is built from one: a
    /// slot still being built holds a null word there.
    declaration: ?*const Declaration = null,
    saved_env: types.Env,
};

/// Where a literal chain frame's dictionaries come from: the evidence of the
/// edge that entered it, read in the scope holding the edge.
const DeferredDictionaries = struct {
    evidence: []const static_dispatch.CheckedEvidence,
    /// The requirements `evidence` stands for, or null for an exact procedure
    /// alias's scope, whose evidence fills every parameter.
    context: ?Builder.ParamContext,
    module: ModuleIndex,
    view: *const types.ModuleView,
    env: types.Env,
    foreign_scopes: ?usize,
    /// The frames below the edge: `alias_scopes.items.len` where it is read.
    frames: usize,
    /// Each parameter's dictionary, once read.
    composed: []?Word,
    /// The template worker whose call instance the frame is, whose body can
    /// declare local procedures its dictionaries call (`Declaration`).
    declaring: ?WorkerId = null,
};

/// Where a literal chain frame's literal arguments come from.
const DeferredLiterals = union(enum) {
    /// The arguments of an edge, resolved in the frames below this one, in
    /// the scope that holds the edge.
    edge: struct {
        args: []const checked.LiteralArgSource,
        module: ModuleIndex,
        view: *const types.ModuleView,
        env: types.Env,
    },
    /// The thunks of `owner`'s literal requirements at the types `bound`.
    thunks: struct { owner: WorkerId, bound: []const TypeId },
};

/// The declaration context of a call instance of template worker `worker`
/// while its dictionaries are built (design.md "Dictionaries"): the hidden
/// descriptors, dictionaries, and literal arguments the instance passes, as
/// this body holds them. A dictionary such as a generated codec can call
/// local procedures of the callee's body, which is not this body's; their
/// function values are built from this context. A dictionary slot is built
/// when the instance or a procedure first reads it; a procedure reading a
/// slot still being built (the dictionary that captures the procedure)
/// captures its reserved recipe there, which is filled once the slot is built.
const Declaration = struct {
    worker: WorkerId,
    /// The callee's scheme types, in this body's variables.
    bound: []const TypeId,
    /// The descriptors and literal arguments; a literal chain frame's are
    /// composed when a procedure first reads them (`declarationWords`).
    words: ?Words,
    /// By hidden dictionary position; null until built.
    dicts: []?Word,
    /// Which dictionary slots are being built.
    building: []bool,
    source: Source,
    outer: ?*Declaration,

    const Source = union(enum) {
        /// The evidence of a call, read in `module`'s view `view`.
        call: struct {
            evidence: []const static_dispatch.CheckedEvidence,
            context: Builder.ParamContext,
            owner: ?*const types.ModuleView,
        },
        /// A literal chain frame's dictionaries, at its position in
        /// `alias_scopes`; `composed` is its `DeferredDictionaries.composed`,
        /// which identifies it.
        frame: struct { position: usize, composed: [*]?Word, scope: AliasScope },
        /// All inputs of an already stored checked function are explicit.
        restored,
    };

    const Words = struct { descs: []const Word, literals: []const Word };
};

/// A generated codec constructor being composed: its family at the types
/// `bound` of the family's variables.
pub const CodecComposition = struct { family: u32, bound: []const TypeId, recipe: Tables.RecipeId, declaration: ?*Declaration };

/// The scope a local procedure's function value is built in: the worker
/// declaring it, the procedure's worker there, and the declaring worker's
/// scheme types and hidden words as this body holds them.
const LocalProcScope = struct {
    declaring: WorkerId,
    worker: WorkerId,
    bound: []const TypeId,
    descs: []const Word,
    /// A reserved recipe stands for each slot still being built.
    dicts: []const Word,
    literals: []const Word,
    /// The declaration context the words come from, when the declaring
    /// body is not one this body is lowered in.
    declaration: ?*Declaration,
};

/// A `literal_argument` root of a module, whose thunk a body composes once.
const RootThunkKey = struct { module: ModuleIndex, root: checked.ComptimeRootId };

/// The lowering state a literal chain restores once its tasks have run.
const SavedScope = struct {
    module: ModuleIndex,
    view: *const types.ModuleView,
    env: types.Env,
    foreign_scopes: ?usize,
    thunk_frames: ?usize,
    alias_scopes: usize,
};

/// One exact procedure alias of an alias chain: its forwarded lookup in
/// `module`, and the quantified variables, requirements, and literal
/// requirements of the scope the alias owns.
const AliasStep = struct {
    module: ModuleIndex,
    lookup: ExprId,
    vars: []const checked.CheckedTypeId,
    params: []const static_dispatch.EvidenceParamRecord,
    literal_requirements: []const checked.LiteralRequirement = &.{},
    /// The alias's function type over `vars`, which relates them to a use
    /// checking recorded no instantiation for.
    fn_ty: ?checked.CheckedTypeId = null,
};

/// Where the literal arguments of the procedure an alias chain reaches come
/// from.
const AliasLiterals = union(enum) {
    /// The arguments of the edge naming the target's instantiation composed
    /// through the chain, resolved in the scope holding the edge.
    edge: []const Word,
    /// Each alias's forwarded lookup is the edge of the scheme it names, read
    /// in that alias's scope, whose own literal arguments the edge before it
    /// supplies.
    scopes,
    /// Checking recorded no edge: they derive from the target's instantiation.
    derived,
};

/// A walk of an alias chain (`Builder.aliasChainInstance`), entering the
/// scope of `first`, whose variables take `bound`.
const AliasChain = struct {
    callee: WorkerId,
    first: AliasStep,
    bound: []const TypeId,
    dicts: []const Word,
    literals: []const Word,
    /// The evidence filling `first`'s scope.
    evidence: []const static_dispatch.CheckedEvidence,
    literal_source: AliasLiterals,
    /// A literal chain's frames, which stay entered under the target's.
    chain_frame: bool,
    /// The function type of the use the chain serves. A dispatch edge's
    /// recorded substitutions fix every variable, so it names none.
    call_fn: ?TypeId,
};

const StaticWordContext = struct {
    pub fn hash(_: @This(), word: StaticDescs.CaptureWord) u64 {
        var hasher = std.hash.Wyhash.init(0);
        StaticDescs.hashCaptureWord(&hasher, word);
        return hasher.final();
    }
    pub fn eql(_: @This(), a: StaticDescs.CaptureWord, b: StaticDescs.CaptureWord) bool {
        return StaticDescs.captureWordsEqual(a, b);
    }
};

/// Lowers one worker's checked body to LIR.
pub const Builder = struct {
    program: *Program,
    allocator: Allocator,
    worker_id: WorkerId,
    module: ModuleIndex,
    view: *const types.ModuleView,
    env: types.Env,
    store: *LirStore,
    frame_locals: std.ArrayList(LocalId) = .empty,
    binders: collections.DenseMapUnmanaged(BinderId, LocalId) = .empty,
    /// The descriptor local of each scope variable, read from the worker's
    /// type table on first use (`inputLocal`).
    var_descs: []?LocalId = &.{},
    /// The dictionaries in scope (`Worker.scope_dictionary_indices` order),
    /// read from the type table on first use.
    dict_locals: []?LocalId = &.{},
    /// The literal arguments in scope (`Worker.literal_scopes` order): an
    /// immortal callable per literal requirement producing the literal's converted
    /// value (design.md "Literal Arguments"), read from the type table on
    /// first use.
    literal_locals: []?LocalId = &.{},
    /// The thunk composed for each `literal_argument` root this body reads.
    /// Set while `enterEdge` builds a frame's call instance, whose
    /// dictionaries the frame composes as it reads them, and whose
    /// descriptors no frame reads: its scheme's types are its `env`.
    chain_frame: bool = false,
    root_thunks: std.AutoHashMapUnmanaged(RootThunkKey, Word) = .empty,
    /// While lowering a literal thunk's or literal argument root's chain:
    /// where its frames start in `alias_scopes`. An edge argument or
    /// dictionary naming a scope outside them is one the chain never reads.
    thunk_frames: ?usize = null,
    /// The scheme alias scopes entered while lowering an alias's right-hand
    /// side at one of its uses, innermost last. Each is a generalized scope
    /// nested inside the worker's own scopes.
    alias_scopes: std.ArrayList(AliasScope) = .empty,
    /// The slot of each recursive value binder in scope (design.md
    /// "Recursive values"): a `Box` of a one-item tuple holding the value,
    /// which callables capture in place of the value.
    recursive_slots: collections.DenseMapUnmanaged(BinderId, LocalId) = .empty,
    /// What each lambda this body declares captured where it was declared:
    /// a `var`'s value then, whatever the `var` holds where the procedure is
    /// later used.
    declared_captures: collections.DenseMapUnmanaged(ExprId, []const LocalId) = .empty,
    /// While lowering an exact procedure alias's forwarded lookup in the
    /// alias's own module (`aliasTargetHidden`): where that alias's scopes
    /// start in `alias_scopes`. Its lookup reaches no scope outside them.
    foreign_scopes: ?usize = null,
    /// The innermost declaration context of the call instances whose
    /// dictionaries are being built (`Declaration`).
    declaration: ?*Declaration = null,
    /// The emission states the immortal restores being lowered replaced,
    /// innermost last.
    type_descs: collections.DenseMapUnmanaged(TypeId, LocalId) = .empty,
    product_shapes: collections.DenseMapUnmanaged(TypeId, LIR.StmtRowId(LIR.ProductShape)) = .empty,

    constant_slots: collections.DenseMapUnmanaged(LocalId, LIR.StaticDataId) = .empty,
    /// The recipes of the hidden words this body reads (`tables.zig`).
    recipes: Tables.Recipes,
    /// The local each recipe was materialized into (`materialize`).
    recipe_locals: collections.DenseMapUnmanaged(Tables.RecipeId, LocalId) = .empty,
    /// The local holding each static word other than a descriptor of a
    /// closed type (`type_descs`) or the null word (`zero_local`).
    static_word_locals: std.HashMapUnmanaged(StaticDescs.CaptureWord, LocalId, StaticWordContext, std.hash_map.default_max_load_percentage) = .empty,
    /// The local holding the null word, once materialized.
    zero_local: ?LocalId = null,
    /// The descriptor recipe of each type composed in this body.
    desc_recipes: collections.DenseMapUnmanaged(TypeId, Tables.RecipeId) = .empty,
    /// The override words of each nominal type this body composes a
    /// descriptor of (`overrideWords`).
    override_words: collections.DenseMapUnmanaged(TypeId, [3]?Word) = .empty,
    /// This worker's type table, its one hidden argument, for a worker with
    /// inputs (`beginTable`).
    table_local: ?LocalId = null,
    /// The recipe of each slot of this worker's tables after its inputs.
    slots: std.ArrayList(Tables.RecipeId) = .empty,
    /// Whether each recipe asked about reads an input (`readsInputs`).
    reads_inputs: collections.DenseMapUnmanaged(Tables.RecipeId, bool) = .empty,
    /// The evaluation of the recipes that read no input (`foldWord`).
    fold: ?Tables.Evaluation = null,
    /// The locals the entry chain defines, which every statement of the
    /// body can read.
    entry_locals: collections.DenseMapUnmanaged(LocalId, void) = .empty,
    /// Whether straight-line statements currently go to the entry chain
    /// (`emitIntoEntry`), whose end is then the current hole.
    emitting_entry: bool = false,
    /// The compile-time root whose entry wrapper this body is: its own
    /// hoisted expression computes its value here rather than reading it.
    own_root: ?checked.ComptimeRootId = null,
    /// Whether this body is a compile-time root's own expression (an entry
    /// wrapper's body that is not a literal's conversion). A failed pattern
    /// test here at a checked exhaustiveness site ends in
    /// `comptime_exhaustiveness_failed`, so compile-time evaluation reports
    /// the site's empirical failure exactly as it does for `.lss` lowering
    /// (`comptimeFailure`).
    comptime_root_body: bool = false,
    /// The generated codec constructors being composed, innermost last
    /// (`codec.composeConstructor`).
    codec_compositions: std.ArrayList(CodecComposition) = .empty,
    /// Active immutable callable constructions, reserved before capture edges.
    static_callables: std.ArrayList(StaticCallable) = .empty,
    /// Expressions whose value is built at a row coercion's destination
    /// type rather than their own (design.md "Row Coercion Primitive"), as
    /// their parents read them: for a row-coerced result, the destination of
    /// its coercion.
    destination_types: collections.DenseMapUnmanaged(ExprId, TypeId) = .empty,
    /// The destination each row-coerced result's own value is built at: the
    /// type its coercion produces (`lowerRowCoercion`).
    coerced_destinations: collections.DenseMapUnmanaged(ExprId, TypeId) = .empty,
    /// The expressions whose value is the procedure's result, as their
    /// parents read them.
    tail_exprs: collections.DenseMapUnmanaged(ExprId, void) = .empty,
    /// The procedure's calls and joins as emitted, for its tail calls.
    tail_calls: lir_core.TailCallBuilder,
    /// The statement emitted last, while nothing follows it.
    last_stmt: ?CFStmtId = null,
    /// How many branching statements (terminals and joins) this body has
    /// emitted.
    branch_count: u32 = 0,
    entry_hole: CFStmtId = undefined,
    hole: ?CFStmtId = null,
    join_points: std.ArrayList(LIR.JoinPoint) = .empty,
    /// What each join of this body carries, by join id.
    join_infos: std.ArrayList(JoinInfo) = .empty,
    merges: std.ArrayList(Merge) = .empty,
    /// The exit joins of the loops being lowered, innermost last.
    loops: std.ArrayList(LoopExit) = .empty,
    next_join: u32 = 0,
    values: std.ArrayList(LocalId) = .empty,
    tasks: std.ArrayList(Task) = .empty,
    region: base.Region = base.Region.zero(),
    result_local: ?LocalId = null,
    /// An erased worker's reuse argument: the erased-callable allocation a
    /// caller transferred to it (design.md "Destination-Passing Results
    /// and Allocation Reuse"), owned by this body until a packing consumes
    /// it; `reuse_available` says none has yet.
    reuse_arg: ?LocalId = null,
    reuse_available: bool = false,
    /// What the transferred allocation holds, which is what this worker's
    /// own function value holds: a dynamic capture record with its
    /// capacity, which a dynamic packing repacks, or the closed capture
    /// layout `static`, which a packing of that same layout repacks.
    reuse_kind: ReuseKind = .none,

    pub const ReuseKind = union(enum) { none, dynamic, static: layout.Idx };

    /// The reuse argument for a packing emitted in the body, consumed:
    /// later packings start fresh.
    pub fn takeReuse(self: *Builder, kind: enum { dynamic, static }) ?LocalId {
        if (!self.reuse_available or self.emitting_entry) return null;
        switch (self.reuse_kind) {
            .none => return null,
            .dynamic => if (kind != .dynamic) return null,
            .static => if (kind != .static) return null,
        }
        self.reuse_available = false;
        return self.reuse_arg;
    }

    /// The reuse parameter of an erased worker: the trailing argument the
    /// uniform erased-callable ABI passes, after the capture pointer.
    pub fn beginReuseArg(self: *Builder, kind: ReuseKind) Allocator.Error!LocalId {
        const reuse = try self.closedLocal(try self.erasedLayout());
        self.reuse_arg = reuse;
        self.reuse_kind = kind;
        self.reuse_available = kind != .none;
        return reuse;
    }

    /// Finish an erased worker's procedure: its arguments are `args`, then
    /// the capture pointer and the reuse argument, which the procedure's
    /// markers name.
    pub fn finishErasedProc(self: *Builder, entry: CFStmtId, body_start: CFStmtId, args: []const LocalId, ret_layout: layout.Idx, capture_ptr: LocalId) Allocator.Error!void {
        const worker = self.currentWorker().*;
        const reuse = self.reuse_arg orelse invariant("an erased worker finished without its reuse argument");
        const all = try std.mem.concat(self.allocator, LocalId, &.{ args, &.{ capture_ptr, reuse } });
        defer self.allocator.free(all);
        try self.finishProc(entry, body_start, all, ret_layout);
        const proc = self.store.getProcSpecPtr(worker.proc);
        proc.erased_capture_arg = capture_ptr;
        proc.erased_reuse_arg = reuse;
        proc.abi = .erased_record;
    }

    pub fn init(program: *Program, id: WorkerId) Allocator.Error!Builder {
        const worker = program.worker(id);
        try program.module(worker.module).bodies().demandIndex();
        const bound = try program.arena.allocator().alloc(TypeId, worker.scheme_vars.len);
        for (bound, 0..) |*slot, index| slot.* = try program.store.variable(@intCast(index));
        // An integer list worker's body, and its lambdas', is lowered with
        // the list item variable bound to its integer type (design.md
        // "Pre-Specialized Integer List Workers"): every value of the item
        // type is static.
        if (worker.list_int) |list_int| bound[list_int.slot] = try program.store.primitive(list_int.item.primitive());
        return .{
            .program = program,
            .allocator = program.allocator,
            .worker_id = id,
            .module = worker.module,
            .view = program.module(worker.module),
            .env = .{ .module = worker.module, .vars = worker.scheme_vars, .bound = bound },
            .store = &program.result.store,
            .tail_calls = lir_core.TailCallBuilder.init(program.allocator, worker.proc),
            .recipes = Tables.Recipes.init(program.allocator),
        };
    }

    pub fn deinit(self: *Builder) void {
        self.frame_locals.deinit(self.allocator);
        self.binders.deinit(self.allocator);
        self.alias_scopes.deinit(self.allocator);
        self.recursive_slots.deinit(self.allocator);
        self.declared_captures.deinit(self.allocator);
        self.root_thunks.deinit(self.allocator);
        self.type_descs.deinit(self.allocator);
        self.product_shapes.deinit(self.allocator);
        self.constant_slots.deinit(self.allocator);
        self.recipes.deinit();
        self.recipe_locals.deinit(self.allocator);
        self.static_word_locals.deinit(self.allocator);
        self.desc_recipes.deinit(self.allocator);
        self.override_words.deinit(self.allocator);
        self.slots.deinit(self.allocator);
        self.reads_inputs.deinit(self.allocator);
        if (self.fold) |*fold| fold.deinit();
        self.entry_locals.deinit(self.allocator);
        self.codec_compositions.deinit(self.allocator);
        self.static_callables.deinit(self.allocator);
        self.join_points.deinit(self.allocator);
        self.join_infos.deinit(self.allocator);
        self.destination_types.deinit(self.allocator);
        self.coerced_destinations.deinit(self.allocator);
        self.tail_exprs.deinit(self.allocator);
        self.tail_calls.deinit();
        self.merges.deinit(self.allocator);
        self.loops.deinit(self.allocator);
        self.values.deinit(self.allocator);
        self.tasks.deinit(self.allocator);
    }

    pub fn currentWorker(self: *Builder) *lower.Worker {
        return self.program.worker(self.worker_id);
    }

    pub fn bodies(self: *const Builder) checked.CheckedBodies {
        return self.view.bodies();
    }

    /// The inline-expect mode the program's divergence data is read in.
    pub fn inlineExpectMode(self: *const Builder) checked.InlineExpectMode {
        return switch (self.program.input.inline_expects) {
            .run => .run,
            .omit => .omit,
        };
    }

    // -- Locals --

    pub fn addLocal(self: *Builder, local: LIR.Local) Allocator.Error!LocalId {
        const id = try self.store.addLocal(local);
        try self.frame_locals.append(self.allocator, id);
        return id;
    }

    pub fn closedLocal(self: *Builder, layout_idx: layout.Idx) Allocator.Error!LocalId {
        return self.addLocal(.{ .layout_idx = layout_idx });
    }

    /// A local holding a value of `ty`: an ordinary local of its committed
    /// layout when `ty` is closed, otherwise a dynamic local of `storage`.
    pub fn localOf(self: *Builder, ty: TypeId, storage: LIR.DynStorage) Allocator.Error!LocalId {
        if (try self.closed(ty)) return self.closedLocal(try self.program.layouts.layoutOf(ty));
        const desc = try self.descOf(ty);
        return self.addLocal(.{ .layout_idx = try self.program.result.layouts.dynamicLayout(), .dyn = .{ .desc = desc, .storage = storage }, .product = try self.productShape(ty) });
    }

    /// Preserve checked product ownership without inventing a byte layout.
    /// The row is shared by locals of this type in this worker. Descriptors
    /// are already prepared at entry; ARC never discovers types or offsets.
    fn productShape(self: *Builder, ty: TypeId) Allocator.Error!LIR.StmtRowId(LIR.ProductShape) {
        if (self.product_shapes.get(ty)) |shape| return shape;
        const structural = try self.structuralType(ty);
        switch (self.program.store.get(structural)) {
            .record => |record| if (record.ext != null) return .none,
            .tuple => {},
            .primitive, .empty_record, .tag_union, .list, .box, .nominal, .function, .variable => return .none,
        }
        const components = try self.structComponentTypes(structural);
        defer self.allocator.free(components);
        const shape = try self.productShapeFields(0, components);
        try self.product_shapes.put(self.allocator, ty, shape);
        return shape;
    }

    /// Ownership fields of a complete product, in field-index order. Leading
    /// ABI words are opaque, non-owning fields; offsets still come from the
    /// producer's record descriptor, never from this ownership row.
    fn productShapeFields(self: *Builder, leading: usize, components: []const TypeId) Allocator.Error!LIR.StmtRowId(LIR.ProductShape) {
        const count = leading + components.len;
        const field_layouts = try self.allocator.alloc(u32, count);
        defer self.allocator.free(field_layouts);
        const descriptors = try self.allocator.alloc(LocalId, count);
        defer self.allocator.free(descriptors);
        const slots = try self.allocator.alloc(u32, count + 1);
        defer self.allocator.free(slots);
        slots[0] = 0;
        if (leading != 0) {
            const descriptor = try self.closedLocal(.opaque_ptr);
            const slot = try self.program.static_descs.opaqueWordSlot();
            try self.emitEntry(.{ .assign_literal = .{ .target = descriptor, .value = .{ .static_data = .{ .id = slot } }, .next = undefined } });
            try self.constant_slots.put(self.allocator, descriptor, slot);
            for (0..leading) |index| {
                field_layouts[index] = @backingInt(layout.Idx.opaque_ptr);
                descriptors[index] = descriptor;
                slots[index + 1] = slots[index];
            }
        }
        for (components, field_layouts[leading..], descriptors[leading..], leading..) |component, *field_layout, *descriptor, index| {
            const field = if (try self.closed(component)) try self.program.layouts.layoutOf(component) else try self.program.result.layouts.dynamicLayout();
            field_layout.* = @backingInt(field);
            slots[index + 1] = slots[index] + self.program.result.layouts.ownershipSlotCount(field);
            descriptor.* = try self.descOf(component);
        }
        return try self.store.addStmtRow(LIR.ProductShape, .{
            .layouts = try self.store.addU32Span(field_layouts),
            .slots = try self.store.addU32Span(slots),
            .descriptors = try self.store.addLocalSpan(descriptors),
        });
    }

    /// Whether values of `ty` have one committed layout (see
    /// `Converter.representationClosed`).
    pub fn closed(self: *Builder, ty: TypeId) Allocator.Error!bool {
        return self.program.converter().representationClosed(ty);
    }

    pub fn convert(self: *Builder, ty: checked.CheckedTypeId) Allocator.Error!TypeId {
        return self.program.converter().convert(self.module, ty, self.env);
    }

    /// The type of `expr`'s own value.
    pub fn exprType(self: *Builder, expr: ExprId) Allocator.Error!TypeId {
        const own = try self.convert(self.bodies().exprType(expr));
        // A value flowing to a destination of the same shape is built at the
        // destination's type; a diverging value's own type can differ in
        // shape, and it builds nothing. A value no construction builds
        // keeps its own type and is re-tagged into its destination
        // (`retagTarget`).
        if (self.ownDestination(expr)) |destination| {
            if (try self.retagTarget(expr, own) != null) return own;
            if (try types.compatible(self.allocator, &self.program.store, own, destination)) return destination;
        }
        return own;
    }

    /// The type `expr`'s parent reads its value at: the type its row
    /// coercion produces when checking recorded one, which converts the
    /// value as a read does (`retagInto`), and its own value's type
    /// otherwise.
    pub fn valueType(self: *Builder, expr: ExprId) Allocator.Error!TypeId {
        const wide = self.bodies().rowCoercion(expr) orelse return try self.exprType(expr);
        const own = try self.convert(wide);
        if (self.destination_types.get(expr)) |destination| {
            if (try self.retagInto(own, destination) != null) return own;
            if (try types.compatible(self.allocator, &self.program.store, own, destination)) return destination;
        }
        return own;
    }

    /// The destination `expr`'s own value is built at, if any: its
    /// coercion's type for a row-coerced result, and the destination its
    /// parent gives it otherwise.
    fn ownDestination(self: *Builder, expr: ExprId) ?TypeId {
        if (self.bodies().rowCoercion(expr) != null) return self.coerced_destinations.get(expr);
        return self.destination_types.get(expr);
    }

    /// Whether `expr`'s own value is the procedure's result. A row-coerced
    /// result's value is its coercion's, never the procedure's directly.
    pub fn isTail(self: *Builder, expr: ExprId) bool {
        if (self.bodies().rowCoercion(expr) != null) return false;
        return self.tail_exprs.contains(expr);
    }

    /// The destination a value of expression `expr` (of its own type `own`)
    /// is converted into: an expression that produces its value rather than
    /// building it (a read or a call) has the representation of its own
    /// type, which differs from its destination's where the destination
    /// widens a row of it (design.md "Row Coercion Primitive", "Sealed
    /// Result Rows").
    pub fn retagTarget(self: *Builder, expr: ExprId, own: TypeId) Allocator.Error!?TypeId {
        const destination = self.ownDestination(expr) orelse return null;
        if (destination == own) return null;
        switch (self.bodies().exprData(expr)) {
            .block, .if_, .match_, .nominal, .dbg, .tag, .zero_argument_tag, .record, .empty_record, .tuple, .list, .empty_list, .numeral, .str_segment, .str, .interpolation, .lambda, .closure, .runtime_error, .crash, .return_ => return null,
            .bytes_literal, .lookup_local, .lookup_external, .lookup_required, .call, .binop, .unary_minus, .field_access, .dispatch_call, .structural_eq, .structural_hash, .method_eq, .type_dispatch_call, .tuple_access, .expect_err, .expect, .ellipsis, .anno_only, .break_, .for_, .hosted_lambda, .run_low_level => {},
        }
        return try self.retagInto(own, destination);
    }

    /// The destination a value of type `own` produced rather than built is
    /// converted into, or null when the two need no conversion.
    fn retagInto(self: *Builder, own: TypeId, destination: TypeId) Allocator.Error!?TypeId {
        if (destination == own) return null;
        // Two closed types committing one layout need no conversion.
        if (try self.closed(own) and try self.closed(destination) and try self.program.layouts.layoutOf(own) == try self.program.layouts.layoutOf(destination)) return null;
        return destination;
    }

    /// The type a read of a stored constant restores its value at: the
    /// read's own, closed type, converted into its destination where that
    /// widens it, or else the type `exprType` gives it.
    pub fn constReadType(self: *Builder, expr: ExprId) Allocator.Error!TypeId {
        const own = try self.convert(self.bodies().exprType(expr));
        if (self.program.store.isClosed(own)) return own;
        return try self.exprType(expr);
    }

    /// The source region of the CIR node `raw_node` of this body's module.
    fn nodeRegion(self: *Builder, raw_node: u32) base.Region {
        return self.bodies().env.store.getNodeRegion(@fromBackingInt(raw_node));
    }

    pub fn patternType(self: *Builder, pattern: PatternId) Allocator.Error!TypeId {
        return self.convert(self.bodies().patternType(pattern));
    }

    // -- Statements --

    pub fn origin(self: *const Builder, kind: LIR.OriginKind) LIR.StmtOrigin {
        return .{
            .loc = self.program.sourceLoc(self.module, self.region),
            .region = self.region,
            .inline_scope = LIR.InlineScopeId.none,
            .kind = kind,
        };
    }

    /// What a failed pattern test at checked exhaustiveness site
    /// `checked_site` reports. In a compile-time root's own body, outside
    /// every function it builds, it fails as the empirical failure of the
    /// site, as Monotype's lowering of the body does, and the program records
    /// the site beside the procedure (design.md "Compile-Time Failure
    /// Sites"); elsewhere it is a runtime error.
    pub fn comptimeFailure(self: *Builder, checked_site: ?checked.CheckedExhaustivenessSiteId) Allocator.Error!?Match.ComptimeFailure {
        if (!self.comptime_root_body) return null;
        const owner = self.program.lowering_module_ids[@backingInt(self.module)];
        if (checked_site) |site| {
            const record = self.view.view.exhaustiveness_sites.get(site);
            const kind: LIR.ComptimeSiteKind = switch (record.kind) {
                .match => .match,
                .destructure => .destructure,
            };
            _ = try self.program.result.addComptimeSite(kind, owner, record.region, site, self.currentWorker().proc);
        }
        return .{ .owner = owner, .checked_site = checked_site };
    }

    pub fn newHole(self: *Builder) Allocator.Error!CFStmtId {
        return self.store.addCFStmt(.runtime_error, self.origin(.lowering_glue));
    }

    /// Replace `hole` with `stmt`.
    pub fn fill(self: *Builder, hole: CFStmtId, stmt: LIR.CFStmt, kind: LIR.OriginKind) Allocator.Error!void {
        try self.store.replaceCFStmt(hole, stmt, self.origin(kind));
        self.store.noteStmtShapes(stmt);
    }

    /// The hole the next statement goes into. Code after a diverging
    /// statement is unreachable; it is still emitted, into a hole nothing
    /// reaches.
    pub fn currentHole(self: *Builder) Allocator.Error!CFStmtId {
        if (self.hole) |hole| return hole;
        const hole = try self.newHole();
        self.hole = hole;
        return hole;
    }

    /// Emit a straight-line statement built by `make(next)`.
    pub fn emitLinear(self: *Builder, stmt: LIR.CFStmt) Allocator.Error!void {
        const hole = try self.currentHole();
        const next = try self.newHole();
        var linked = stmt;
        setNext(&linked, next);
        try self.fill(hole, linked, .source);
        self.hole = next;
        self.last_stmt = hole;
    }

    /// Emit a terminal statement; nothing follows it.
    pub fn emitTerminal(self: *Builder, stmt: LIR.CFStmt) Allocator.Error!void {
        const hole = try self.currentHole();
        try self.fill(hole, stmt, .source);
        self.hole = null;
        self.branch_count += 1;
    }

    pub fn emitEntry(self: *Builder, stmt: LIR.CFStmt) Allocator.Error!void {
        if (definedLocal(stmt)) |target| try self.entry_locals.put(self.allocator, target, {});
        if (self.emitting_entry) return try self.emitLinear(stmt);
        const next = try self.newHole();
        var linked = stmt;
        setNext(&linked, next);
        try self.fill(self.entry_hole, linked, .scaffold);
        self.entry_hole = next;
    }

    /// The local a straight-line statement defines, if any.
    fn definedLocal(stmt: LIR.CFStmt) ?LocalId {
        return switch (stmt) {
            inline .init_uninitialized, .assign_ref, .assign_literal, .assign_call, .assign_call_erased, .assign_packed_erased_fn, .assign_low_level, .assign_list, .assign_struct, .assign_tag, .store_struct, .store_tag, .set_local, .debug, .expect, .expect_err, .dyn, .runtime_error, .comptime_exhaustiveness_failed, .incref, .decref, .decref_if_initialized, .free, .switch_stmt, .switch_initialized_payload, .str_match, .str_match_set, .join, .jump, .ret, .crash => |payload| blk: {
                const Payload = @TypeOf(payload);
                if (@typeInfo(Payload) != .@"struct" or !@hasField(Payload, "target")) break :blk null;
                if (@TypeOf(payload.target) != LocalId) break :blk null;
                break :blk payload.target;
            },
        };
    }

    /// Run `emit` (a function of `self` emitting straight-line statements)
    /// with its statements going to the entry chain, and record the locals
    /// they define as entry locals.
    pub fn emitIntoEntry(self: *Builder, comptime emit: anytype, args: anytype) Allocator.Error!LocalId {
        if (self.emitting_entry) {
            const nested = try @call(.auto, emit, .{self} ++ args);
            try self.entry_locals.put(self.allocator, nested, {});
            return nested;
        }
        const saved_hole = self.hole;
        const saved_last = self.last_stmt;
        self.hole = self.entry_hole;
        self.emitting_entry = true;
        const result = try @call(.auto, emit, .{self} ++ args);
        self.emitting_entry = false;
        self.entry_hole = self.hole orelse invariant("an entry-chain emission diverged");
        self.hole = saved_hole;
        self.last_stmt = saved_last;
        try self.entry_locals.put(self.allocator, result, {});
        return result;
    }

    pub fn setNext(stmt: *LIR.CFStmt, next: CFStmtId) void {
        switch (stmt.*) {
            inline .init_uninitialized,
            .assign_ref,
            .assign_literal,
            .assign_call,
            .assign_call_erased,
            .assign_packed_erased_fn,
            .assign_low_level,
            .assign_list,
            .assign_struct,
            .assign_tag,
            .store_struct,
            .store_tag,
            .set_local,
            .debug,
            .expect,
            .incref,
            .decref,
            .decref_if_initialized,
            .free,
            .dyn,
            => |*s| s.next = next,
            .expect_err, .runtime_error, .comptime_exhaustiveness_failed, .switch_stmt, .switch_initialized_payload, .str_match, .str_match_set, .join, .jump, .ret, .crash => invariant("asked to link the successor of a non-linear statement"),
        }
    }

    pub fn emitLowLevel(self: *Builder, target: LocalId, op: LIR.LowLevel, args: []const LocalId) Allocator.Error!void {
        try self.emitLinear(.{ .assign_low_level = .{
            .target = target,
            .op = op,
            .rc_effect = try self.store.addRcEffect(op.rcEffect()),
            .args = try self.store.addLocalSpan(args),
            .next = undefined,
        } });
    }

    pub fn emitDyn(self: *Builder, target: ?LocalId, op: LIR.DynOp) Allocator.Error!void {
        try self.emitLinear(.{ .dyn = .{ .target = target, .data = try self.store.addStmtRow(LIR.DynOp, op), .next = undefined } });
    }

    pub fn freshJoin(self: *Builder) LIR.JoinPointId {
        const id: LIR.JoinPointId = @fromBackingInt(self.next_join);
        self.next_join += 1;
        return id;
    }

    /// How a merge ends: at a join whose parameters carry the binders' values
    /// out, or in tail position, where every branch returns and the merge
    /// only restores the binders' starting values for each branch.
    pub const MergeEnd = enum { joined, tail };

    /// Start a merge of `binders`, recording their current values.
    pub fn beginMerge(self: *Builder, plan_binders: []const BinderId, end: MergeEnd) Allocator.Error!?MergeId {
        // A binder the construct reassigns but that is not bound before it
        // is declared inside it, and goes out of scope with it.
        const arena = self.program.arena.allocator();
        var in_scope: std.ArrayList(BinderId) = .empty;
        for (plan_binders) |binder| {
            if (self.binders.contains(binder)) try in_scope.append(arena, binder);
        }
        const binders = in_scope.items;
        if (binders.len == 0) return null;
        const pre = try arena.alloc(LocalId, binders.len);
        for (binders, pre) |binder, *before| before.* = self.binders.get(binder).?;
        const params: []LocalId = switch (end) {
            .tail => &.{},
            .joined => blk: {
                const params = try arena.alloc(LocalId, binders.len);
                for (pre, params) |before, *param| {
                    var local = self.store.getLocal(before);
                    if (local.dyn) |*dyn| {
                        dyn.storage = .frame;
                    }
                    param.* = try self.addLocal(local);
                }
                break :blk params;
            },
        };
        const id: MergeId = @intCast(self.merges.items.len);
        try self.merges.append(self.allocator, .{ .binders = binders, .pre = pre, .params = params });
        return id;
    }

    /// Declare a join carrying an optional value and a merge's binders; the
    /// builder continues in its remainder. Returns the join's body hole.
    pub fn declareMergeJoin(self: *Builder, id: LIR.JoinPointId, value: ?LocalId, merge: ?MergeId) Allocator.Error!CFStmtId {
        var params: std.ArrayList(LocalId) = .empty;
        defer params.deinit(self.allocator);
        if (value) |local| try params.append(self.allocator, local);
        if (merge) |m| try params.appendSlice(self.allocator, self.merges.items[m].params);
        const index = @backingInt(id);
        while (self.join_infos.items.len <= index) try self.join_infos.append(self.allocator, .{ .value = null, .merge = null });
        self.join_infos.items[index] = .{ .value = value, .merge = merge };
        return try self.declareJoin(id, params.items);
    }

    pub fn restoreMerge(self: *Builder, merge: MergeId) Allocator.Error!void {
        const m = self.merges.items[merge];
        for (m.binders, m.pre) |binder, value| try self.binders.put(self.allocator, binder, value);
    }

    pub fn enterMerge(self: *Builder, merge: MergeId) Allocator.Error!void {
        const m = self.merges.items[merge];
        if (m.params.len != m.binders.len) invariant("a merge in tail position carries no binder out");
        for (m.binders, m.params) |binder, value| try self.binders.put(self.allocator, binder, value);
    }

    /// Declare join `id` with `params` here; continue in its remainder, and
    /// return the hole of its body.
    pub fn declareJoin(self: *Builder, id: LIR.JoinPointId, params: []const LocalId) Allocator.Error!CFStmtId {
        self.branch_count += 1;
        const hole = try self.currentHole();
        const body = try self.newHole();
        const remainder = try self.newHole();
        const span = try self.store.addLocalSpan(params);
        try self.fill(hole, .{ .join = .{ .id = id, .params = span, .body = body, .remainder = remainder } }, .lowering_glue);
        try self.join_points.append(self.allocator, .{ .id = id, .params = span, .body = body });
        self.hole = remainder;
        return body;
    }

    /// Jump to join `id`, assigning its parameters: `value` to its value
    /// parameter, and its merge's binders' current values to theirs.
    pub fn emitJump(self: *Builder, id: LIR.JoinPointId, value: ?LocalId) Allocator.Error!void {
        const index = @backingInt(id);
        const info: JoinInfo = if (index < self.join_infos.items.len) self.join_infos.items[index] else .{ .value = null, .merge = null };
        if (info.value) |param| {
            const source = value orelse invariant("a jump to a value join carried no value");
            try self.emitLinear(.{ .set_local = .{ .target = param, .value = source, .mode = .initialize_join_param, .next = undefined } });
        }
        if (info.merge) |merge| {
            const m = self.merges.items[merge];
            // The parameters take their values at once: a value that is one
            // of the parameters (a loop's carried value moving to another
            // carried position) or a view, which may point into one, is
            // read into a local of its own before any parameter is written.
            const sources = try self.allocator.alloc(LocalId, m.params.len);
            defer self.allocator.free(sources);
            var params: collections.DenseMapUnmanaged(LocalId, void) = .empty;
            defer params.deinit(self.allocator);
            for (m.params) |param| try params.put(self.allocator, param, {});
            for (m.binders, m.params, sources) |binder, param, *source| {
                const current = self.binders.get(binder) orelse invariant("a merged binder lost its value");
                source.* = current;
                if (current == param) continue;
                const dyn = self.store.localDyn(current);
                if (!params.contains(current) and (dyn == null or dyn.?.storage != .view)) continue;
                if (dyn) |dyn_info| {
                    const staged = try self.addLocal(.{ .layout_idx = self.store.getLocal(current).layout_idx, .dyn = .{ .desc = dyn_info.desc, .storage = .frame }, .product = self.store.getLocal(current).product });
                    try self.emitDyn(staged, .{ .copy = .{ .src = current, .desc = dyn_info.desc } });
                    source.* = staged;
                } else {
                    const staged = try self.closedLocal(self.store.getLocal(current).layout_idx);
                    try self.emitLinear(.{ .assign_ref = .{ .target = staged, .op = LIR.RefOp.local(current), .next = undefined } });
                    source.* = staged;
                }
            }
            for (m.params, sources) |param, source| {
                try self.emitLinear(.{ .set_local = .{ .target = param, .value = source, .mode = .initialize_join_param, .next = undefined } });
            }
        }
        try self.emitTerminal(.{ .jump = .{ .target = id } });
    }

    /// Emit `switch (cond) { 1 => then } else otherwise` and return the two
    /// holes.
    pub fn emitBoolSwitch(self: *Builder, cond: LocalId) Allocator.Error!struct { then: CFStmtId, otherwise: CFStmtId } {
        const then = try self.newHole();
        const otherwise = try self.newHole();
        const branches = try self.store.addCFSwitchBranches(&.{.{ .value = 1, .body = then }});
        try self.emitTerminal(.{ .switch_stmt = .{ .cond = cond, .branches = branches, .default_branch = otherwise } });
        return .{ .then = then, .otherwise = otherwise };
    }

    // -- Descriptors --

    /// The descriptor local of `root`, read in the entry chain on first
    /// use: a word (`descWord`), which is a static descriptor of a closed
    /// type, a scope variable's input, or a recipe of this worker's table
    /// (design.md "Type Tables").
    pub fn descOf(self: *Builder, root: TypeId) Allocator.Error!LocalId {
        if (self.type_descs.get(root)) |existing| return existing;
        const local = try self.materialize(try self.descWord(root));
        try self.type_descs.put(self.allocator, root, local);
        return local;
    }

    pub fn staticDesc(self: *Builder, ty: TypeId) Allocator.Error!LocalId {
        if (self.type_descs.get(ty)) |existing| return existing;
        const slot = try self.program.static_descs.descriptorSlot(ty);
        const local = try self.closedLocal(.opaque_ptr);
        try self.emitEntry(.{ .assign_literal = .{ .target = local, .value = .{ .static_data = .{ .id = slot } }, .next = undefined } });
        try self.type_descs.put(self.allocator, ty, local);
        try self.constant_slots.put(self.allocator, local, slot);
        return local;
    }

    // -- Positions and plans --

    /// The discriminant of the tag `name` under the tag union `ty`'s
    /// descriptor (through nominal wrappers), as an index operand.
    pub fn tagIndex(self: *Builder, ty: TypeId, name: types.NameId) Allocator.Error!LIR.DynIndex {
        return try self.tagIndexIn(try self.descWord(ty), name);
    }

    /// The discriminant of the tag `name` under the tag union the
    /// descriptor word `desc` describes, as an index operand.
    pub fn tagIndexIn(self: *Builder, desc: Word, name: types.NameId) Allocator.Error!LIR.DynIndex {
        return try self.indexOperand(.{ .tag_index = .{ .desc = desc, .name = name } });
    }

    /// The label-order index of the field `name` of the record `ty`'s
    /// descriptor (through nominal wrappers), as an index operand.
    pub fn fieldIndex(self: *Builder, ty: TypeId, name: types.NameId) Allocator.Error!LIR.DynIndex {
        return try self.indexOperand(.{ .field_index = .{ .desc = try self.descWord(ty), .name = name } });
    }

    /// The position recipe `recipe` as an index operand: its value where it
    /// reads no input, otherwise the `u32` local reading its slot of this
    /// worker's type table.
    fn indexOperand(self: *Builder, recipe: Tables.Recipe) Allocator.Error!LIR.DynIndex {
        const id = try self.recipes.intern(recipe);
        if (try self.readsInputs(id)) return .{ .local = try self.materialize(.{ .recipe = id }) };
        return switch (try self.foldWord(id)) {
            .int => |value| .{ .immediate = value },
            .desc, .scoped_desc, .callable, .table, .node, .zero => invariant("a position recipe's word is not a position"),
        };
    }

    /// Write the index operand `index` to the `u32` local `target`.
    pub fn assignIndex(self: *Builder, target: LocalId, index: LIR.DynIndex) Allocator.Error!void {
        switch (index) {
            .immediate => |value| try self.emitLinear(.{ .assign_literal = .{ .target = target, .value = .{ .i64_literal = .{ .value = value, .layout_idx = .u32 } }, .next = undefined } }),
            .local => |local| try self.emitLinear(.{ .assign_ref = .{ .target = target, .op = LIR.RefOp.local(local), .next = undefined } }),
        }
    }

    /// A `u32` local holding the index operand `index`.
    pub fn indexLocal(self: *Builder, index: LIR.DynIndex) Allocator.Error!LocalId {
        return switch (index) {
            .local => |local| local,
            .immediate => blk: {
                const local = try self.closedLocal(.u32);
                try self.assignIndex(local, index);
                break :blk local;
            },
        };
    }

    /// The local holding the coercion plan from `from`'s descriptor to
    /// `to`'s (`DynOp.adapt`).
    pub fn adaptPlan(self: *Builder, from: TypeId, to: TypeId) Allocator.Error!LocalId {
        return try self.adaptPlanIn(try self.descWord(from), try self.descWord(to));
    }

    /// The local holding the coercion plan between the descriptor words
    /// `from` and `to`.
    pub fn adaptPlanIn(self: *Builder, from: Word, to: Word) Allocator.Error!LocalId {
        return try self.materialize(.{ .recipe = try self.recipes.intern(.{ .adapt_plan = .{ .from = from, .to = to } }) });
    }

    /// The local holding the names block of `ty`'s descriptor, which an
    /// inspection of a value of `ty` reads its labels and tag names from.
    pub fn namesOf(self: *Builder, ty: TypeId) Allocator.Error!LocalId {
        return try self.namesIn(try self.descWord(ty));
    }

    /// The local holding the names block of the type the descriptor word
    /// `desc` describes.
    pub fn namesIn(self: *Builder, desc: Word) Allocator.Error!LocalId {
        return try self.materialize(.{ .recipe = try self.recipes.intern(.{ .names_block = .{ .desc = desc } }) });
    }

    /// Convert `src`, of type `from`, into `target`, of type `to`, by their
    /// coercion plan.
    pub fn emitAdapt(self: *Builder, target: LocalId, src: LocalId, from: TypeId, to: TypeId) Allocator.Error!void {
        try self.emitDyn(target, .{ .adapt = .{ .src = src, .plan = try self.adaptPlan(from, to) } });
    }

    // -- Words --

    /// Input word `index` of this worker: in input order, the descriptors of
    /// its scheme variables (`var_descs`), its dictionaries (`dict_locals`),
    /// then its literal arguments (`literal_locals`).
    pub fn inputWord(self: *Builder, index: usize) Allocator.Error!Word {
        return .{ .recipe = try self.recipes.intern(.{ .input = @intCast(index) }) };
    }

    /// The input word of `dict_locals[position]`.
    pub fn dictInputWord(self: *Builder, position: usize) Allocator.Error!Word {
        if (position >= self.dict_locals.len) invariant("a dictionary input names a dictionary its worker does not take");
        return try self.inputWord(self.var_descs.len + position);
    }

    /// The input word of `literal_locals[position]`.
    pub fn literalInputWord(self: *Builder, position: usize) Allocator.Error!Word {
        if (position >= self.literal_locals.len) invariant("a literal input names a literal argument its worker does not take");
        return try self.inputWord(self.var_descs.len + self.dict_locals.len + position);
    }

    /// The input words of descriptors `var_descs[start..end]`.
    pub fn descInputWords(self: *Builder, start: usize, end: usize) Allocator.Error![]Word {
        const words = try self.program.arena.allocator().alloc(Word, end - start);
        for (words, start..) |*word, index| word.* = try self.inputWord(index);
        return words;
    }

    /// Every input word of this worker, in input order.
    pub fn allInputWords(self: *Builder) Allocator.Error![]Word {
        const words = try self.program.arena.allocator().alloc(Word, self.program.inputCount(self.worker_id));
        for (words, 0..) |*word, index| word.* = try self.inputWord(index);
        return words;
    }

    /// The input words of `dict_locals[start..]` then `literal_locals[literal_start..]`.
    pub fn dictLiteralInputWords(self: *Builder, start: usize, literal_start: usize) Allocator.Error![]Word {
        const words = try self.program.arena.allocator().alloc(Word, self.dict_locals.len - start + self.literal_locals.len - literal_start);
        for (words[0 .. self.dict_locals.len - start], start..) |*word, position| word.* = try self.dictInputWord(position);
        for (words[self.dict_locals.len - start ..], literal_start..) |*word, position| word.* = try self.literalInputWord(position);
        return words;
    }

    /// The word of `ty`'s descriptor: a static descriptor of a closed type
    /// (unless a body must compose it, `Program.composedInBody`), a scope
    /// variable's input, or the recipe of a type over the worker's
    /// variables.
    pub fn descWord(self: *Builder, ty: TypeId) Allocator.Error!Word {
        if (try self.closed(ty) and !try self.program.composedInBody(ty)) return .{ .static = .{ .desc = ty } };
        switch (self.program.store.get(ty)) {
            .variable => |index| return try self.inputWord(index),
            .primitive, .empty_record, .record, .tuple, .tag_union, .list, .box, .nominal, .function => {},
        }
        return .{ .recipe = try self.descRecipe(ty) };
    }

    /// The descriptor words of `tys`.
    pub fn descWords(self: *Builder, tys: []const TypeId) Allocator.Error![]Word {
        const words = try self.program.arena.allocator().alloc(Word, tys.len);
        for (words, tys) |*word, ty| word.* = try self.descWord(ty);
        return words;
    }

    /// The descriptor recipe of `ty`, a type this body composes: with the
    /// override words of every nominal type its composition reaches, which
    /// only this body builds. It is reserved before those are built, since
    /// an override generic in its receiver captures this descriptor.
    fn descRecipe(self: *Builder, root: TypeId) Allocator.Error!Tables.RecipeId {
        if (self.desc_recipes.get(root)) |known| return known;
        const id = try self.recipes.reserve();
        try self.desc_recipes.put(self.allocator, root, id);
        var scoped: std.ArrayList(Tables.ScopedOverride) = .empty;
        defer scoped.deinit(self.allocator);
        var seen: collections.DenseMapUnmanaged(TypeId, void) = .empty;
        defer seen.deinit(self.allocator);
        var pending: std.ArrayList(TypeId) = .empty;
        defer pending.deinit(self.allocator);
        try pending.append(self.allocator, root);
        while (pending.pop()) |ty| {
            if ((try seen.getOrPut(self.allocator, ty)).found_existing) continue;
            // A component with a static descriptor holds its own overrides.
            if (ty != root and try self.closed(ty) and !try self.program.composedInBody(ty)) continue;
            if (self.program.store.get(ty) == .nominal) {
                if (try self.overrideWords(ty)) |methods| try scoped.append(self.allocator, .{ .nominal = ty, .methods = methods });
            }
            try self.program.appendDescriptorComponents(ty, &pending, self.allocator);
        }
        try self.recipes.set(id, .{ .desc = .{ .ty = root, .scoped = scoped.items } });
        return id;
    }

    /// The override callables of the nominal type `ty`
    /// (`Program.nominalOverrides`) as words of this body, in `Methods`
    /// order, or null when it has none. A procedure's override is its
    /// dictionary entry capturing the descriptors of the types its scheme
    /// takes, the dictionaries of its requirements resolved from those
    /// types, and literal thunks derived from them (a descriptor records no
    /// checked edge, design.md "Literal Arguments"); a method declared in a
    /// function body is its local procedure's function value at the method's
    /// interface. Each is reserved before its captures are built, since an
    /// override generic in its receiver captures `ty`'s descriptor.
    pub fn overrideWords(self: *Builder, ty: TypeId) Allocator.Error!?[3]?Word {
        if (self.override_words.get(ty)) |known| return known;
        const overrides = try self.program.nominalOverrides(ty);
        if (overrides[0] == null and overrides[1] == null and overrides[2] == null) return null;
        var reserved: [3]?Tables.RecipeId = .{ null, null, null };
        var words: [3]?Word = .{ null, null, null };
        for (overrides, &reserved, &words) |maybe, *id, *word| if (maybe != null) {
            id.* = try self.recipes.reserve();
            word.* = .{ .recipe = id.*.? };
        };
        try self.override_words.put(self.allocator, ty, words);
        for (overrides, reserved) |maybe, id| {
            const override = maybe orelse continue;
            switch (override) {
                .procedure => |procedure| {
                    const literal_count = self.program.worker(procedure.target).literal_params;
                    const hidden = try self.program.arena.allocator().alloc(Word, procedure.bound.len + procedure.dicts.len + literal_count);
                    for (procedure.bound, hidden[0..procedure.bound.len]) |bound_ty, *desc| desc.* = try self.descWord(bound_ty);
                    for (procedure.dicts, hidden[procedure.bound.len..][0..procedure.dicts.len]) |requirement, *dict| dict.* = try self.dictionaryFromType(requirement);
                    for (hidden[procedure.bound.len + procedure.dicts.len ..], 0..) |*literal, index| literal.* = try self.derivedLiteralArg(procedure.target, procedure.bound, @intCast(index));
                    try self.recipes.set(id.?, try self.dictionaryRecipe(procedure.entry, hidden));
                },
                // The local procedure's immortal callable is the override.
                .local => |local| {
                    if (local.module != self.module) invariant("a local-procedure override's descriptor is composed outside its declaring module");
                    const word = try self.localProcWord(local.binding, local.interface, null, null, &.{}, null);
                    self.recipes.setSame(id.?, word.recipe);
                },
            }
        }
        return words;
    }

    /// The immortal callable of `entry` capturing the hidden words
    /// `captures`, its inputs: an erased callable of the entry capturing
    /// the entry's type table at those inputs, or nothing for an entry with
    /// no inputs.
    pub fn composeDictionary(self: *Builder, entry: WorkerId, captures: []const Word) Allocator.Error!Word {
        return .{ .recipe = try self.recipes.intern(try self.dictionaryRecipe(entry, captures)) };
    }

    /// The recipe `composeDictionary` interns.
    fn dictionaryRecipe(self: *Builder, entry: WorkerId, inputs: []const Word) Allocator.Error!Tables.Recipe {
        if (self.program.inputCount(entry) != inputs.len) invariant("a dictionary's captures differ from its entry's inputs");
        if (inputs.len == 0) return .{ .callable = .{ .entry = entry, .captures = &.{} } };
        const captures = try self.program.arena.allocator().alloc(Word, 1);
        captures[0] = try self.tableWord(entry, inputs);
        return .{ .callable = .{ .entry = entry, .captures = captures } };
    }

    /// The word of `callee`'s type table at the inputs `inputs`.
    pub fn tableWord(self: *Builder, callee: WorkerId, inputs: []const Word) Allocator.Error!Word {
        if (self.program.inputCount(callee) != inputs.len) invariant("a type table's inputs differ from its worker's");
        return .{ .recipe = try self.recipes.intern(.{ .table = .{ .worker = callee, .inputs = inputs } }) };
    }

    /// A direct call's arguments: `explicit`, then `callee`'s type table at
    /// `inputs` when it takes one. Caller frees.
    pub fn callArgs(self: *Builder, explicit: []const LocalId, callee: WorkerId, inputs: []const Word) Allocator.Error![]LocalId {
        const table = try self.tableLocal(callee, inputs) orelse return try self.allocator.dupe(LocalId, explicit);
        return try std.mem.concat(self.allocator, LocalId, &.{ explicit, &.{table} });
    }

    /// The local holding `callee`'s type table at `inputs`, or null for a
    /// worker with no inputs, which takes no table.
    pub fn tableLocal(self: *Builder, callee: WorkerId, inputs: []const Word) Allocator.Error!?LocalId {
        if (inputs.len == 0 and self.program.inputCount(callee) == 0) return null;
        return try self.materialize(try self.tableWord(callee, inputs));
    }

    /// The local holding `word`, emitted in the entry chain on first use:
    /// an input read from where the worker received it; a recipe that
    /// reads no input, evaluated now and read from static data; any other
    /// recipe read from its slot of this worker's type table.
    pub fn materialize(self: *Builder, word: Word) Allocator.Error!LocalId {
        const named = switch (word) {
            .static => |static| return try self.staticWordLocal(static),
            .recipe => |named| named,
        };
        const id = self.recipes.resolve(named);
        if (self.recipe_locals.get(id)) |known| return known;
        const local = switch (self.recipes.get(id)) {
            .input => |index| return try self.inputLocal(index),
            .desc, .args_desc, .callable, .table, .tag_index, .field_index, .update_plan, .adapt_plan, .names_block => if (try self.readsInputs(id))
                try self.slotLocal(id)
            else
                try self.staticWordLocal(try self.foldWord(id)),
        };
        try self.recipe_locals.put(self.allocator, id, local);
        return local;
    }

    /// The locals of `words`.
    pub fn materializeAll(self: *Builder, words: []const Word) Allocator.Error![]LocalId {
        const locals = try self.allocator.alloc(LocalId, words.len);
        errdefer self.allocator.free(locals);
        for (locals, words) |*local, word| local.* = try self.materialize(word);
        return locals;
    }

    /// Whether recipe `root` reads one of this worker's inputs: an input,
    /// or a descriptor of a type over its variables, which their
    /// descriptors determine.
    fn readsInputs(self: *Builder, root: Tables.RecipeId) Allocator.Error!bool {
        if (self.reads_inputs.get(root)) |known| return known;
        var seen: collections.DenseMapUnmanaged(Tables.RecipeId, void) = .empty;
        defer seen.deinit(self.allocator);
        var pending: std.ArrayList(Tables.RecipeId) = .empty;
        defer pending.deinit(self.allocator);
        var operands: std.ArrayList(Word) = .empty;
        defer operands.deinit(self.allocator);
        try pending.append(self.allocator, root);
        const found = walk: while (pending.pop()) |named| {
            const id = self.recipes.resolve(named);
            if ((try seen.getOrPut(self.allocator, id)).found_existing) continue;
            if (self.reads_inputs.get(id)) |known| {
                if (known) break :walk true;
                continue;
            }
            const recipe = self.recipes.get(id);
            switch (recipe) {
                .input => break :walk true,
                .desc => |desc| if (!self.program.store.isClosed(desc.ty)) break :walk true,
                .args_desc, .callable, .table, .tag_index, .field_index, .update_plan, .adapt_plan, .names_block => {},
            }
            operands.clearRetainingCapacity();
            try Tables.appendOperands(self.allocator, &operands, recipe);
            for (operands.items) |operand| switch (operand) {
                .recipe => |next| try pending.append(self.allocator, next),
                .static => {},
            };
        } else false;
        // A recipe that reads no input reaches only recipes that read none.
        if (!found) {
            var reached = seen.keyIterator();
            while (reached.next()) |id| try self.reads_inputs.put(self.allocator, id.*, false);
        }
        try self.reads_inputs.put(self.allocator, root, found);
        return found;
    }

    /// The word recipe `id`, which reads no input, is: evaluated now.
    fn foldWord(self: *Builder, id: Tables.RecipeId) Allocator.Error!StaticDescs.CaptureWord {
        if (self.fold == null) self.fold = Tables.Evaluation.init(self.program, .{ .building = &self.recipes }, &.{}, 0);
        return try self.fold.?.word(id);
    }

    /// The local reading recipe `id`'s slot of this worker's type table,
    /// the slot given it now.
    fn slotLocal(self: *Builder, id: Tables.RecipeId) Allocator.Error!LocalId {
        const table = self.table_local orelse invariant("a worker without a type table read an input-dependent recipe");
        const slot: u32 = @intCast(self.program.inputCount(self.worker_id) + self.slots.items.len);
        try self.slots.append(self.allocator, id);
        const local = try self.closedLocal(switch (self.recipes.get(id)) {
            .callable => try self.erasedLayout(),
            .desc, .args_desc, .table, .update_plan, .adapt_plan, .names_block => .opaque_ptr,
            .tag_index, .field_index => .u32,
            .input => invariant("an input has no slot of its own"),
        });
        try self.emitEntry(.{ .dyn = .{ .target = local, .data = try self.store.addStmtRow(LIR.DynOp, .{ .table_entry = .{ .table = table, .index = slot } }), .next = undefined } });
        return local;
    }

    /// The local holding the static word `word`, read from static data.
    fn staticWordLocal(self: *Builder, word: StaticDescs.CaptureWord) Allocator.Error!LocalId {
        switch (word) {
            .desc => |ty| return try self.staticDesc(ty),
            .zero => {
                if (self.zero_local) |known| return known;
                const local = try self.nullWord();
                self.zero_local = local;
                return local;
            },
            .scoped_desc, .callable, .table, .node => {},
            .int => invariant("a position word is read as an operand, not from static data"),
        }
        if (self.static_word_locals.get(word)) |known| return known;
        const local = try self.closedLocal(if (word == .callable) try self.erasedLayout() else .opaque_ptr);
        const slot = try self.program.static_descs.wordSlot(word);
        try self.emitEntry(.{ .assign_literal = .{ .target = local, .value = .{ .static_data = .{ .id = slot } }, .next = undefined } });
        try self.constant_slots.put(self.allocator, local, slot);
        try self.static_word_locals.put(self.allocator, word, local);
        return local;
    }

    /// Take this worker's inputs from its type table `table`, its one
    /// hidden argument (`tables.zig`): the descriptors of its variables,
    /// its dictionaries, then its literal arguments, each read in the entry
    /// chain on first use (`inputLocal`).
    pub fn beginTable(self: *Builder, table: LocalId, dict_count: usize) Allocator.Error!void {
        const arena = self.program.arena.allocator();
        const input_count = self.program.inputCount(self.worker_id);
        const desc_count = self.program.descCount(self.worker_id);
        self.table_local = table;
        self.var_descs = try arena.alloc(?LocalId, desc_count);
        self.dict_locals = try arena.alloc(?LocalId, dict_count);
        self.literal_locals = try arena.alloc(?LocalId, input_count - desc_count - dict_count);
        @memset(self.var_descs, null);
        @memset(self.dict_locals, null);
        @memset(self.literal_locals, null);
    }

    /// The needs list of this worker's tables, once its body is lowered.
    pub fn finishNeeds(self: *Builder) Allocator.Error!?Tables.Needs {
        const input_count = self.program.inputCount(self.worker_id);
        if (input_count == 0) {
            if (self.slots.items.len != 0) invariant("a worker without inputs read a table slot");
            return null;
        }
        return try self.recipes.needs(self.program.arena.allocator(), self.allocator, input_count, self.program.descCount(self.worker_id), self.slots.items);
    }

    /// The local holding input `index` (`inputWord`), read from this
    /// worker's type table in the entry chain on first use.
    fn inputLocal(self: *Builder, index: u32) Allocator.Error!LocalId {
        const dict = index -% @as(u32, @intCast(self.var_descs.len));
        const literal = dict -% @as(u32, @intCast(self.dict_locals.len));
        const known = if (index < self.var_descs.len)
            &self.var_descs[index]
        else if (dict < self.dict_locals.len)
            &self.dict_locals[dict]
        else if (literal < self.literal_locals.len)
            &self.literal_locals[literal]
        else
            invariant("an input word names an input its worker does not take");
        if (known.*) |local| return local;
        const table = self.table_local orelse invariant("a worker without a type table read an input");
        const local = try self.closedLocal(if (index < self.var_descs.len) .opaque_ptr else try self.erasedLayout());
        try self.emitEntry(.{ .dyn = .{ .target = local, .data = try self.store.addStmtRow(LIR.DynOp, .{ .table_entry = .{ .table = table, .index = index } }), .next = undefined } });
        known.* = local;
        return local;
    }

    /// The local holding the descriptor of scope variable `index`.
    pub fn varDesc(self: *Builder, index: usize) Allocator.Error!LocalId {
        if (index >= self.var_descs.len) invariant("a scope variable's descriptor is not among its worker's inputs");
        return try self.inputLocal(@intCast(index));
    }

    // -- Body --

    pub fn lowerBody(self: *Builder) Allocator.Error!void {
        const worker = self.currentWorker().*;
        const template = switch (worker.key) {
            .template => |key| self.view.view.checked_procedure_templates.templates.items[@backingInt(key.template)],
            .lambda => |key| return try self.lowerLambdaBody(key.expr),
            .adapter => |key| return try self.lowerAdapterBody(key.from),
            .method_entry => |target| return try self.lowerMethodEntry(target),
            .structural => |method| return try self.lowerStructuralMethod(method),
            .structural_map => |map| return try self.lowerStructuralMap(map),
            .root => return try self.lowerRootWrapper(),
            .static_data => return try self.lowerStaticDataInitializer(),
            .default => |key| return try self.lowerDefaultBody(key.expr),
            .codec => |piece| return try Codec.lowerPiece(self, piece),
            .crash_dictionary => |kind| return try self.lowerCrashDictionary(kind),
            .callable_eval_entry => |binding| return try self.lowerCallableEvalEntry(binding),
            .convert => |key| return try self.lowerConvertBody(key.from, key.to),
            .literal_thunk => |key| return try self.lowerLiteralThunkBody(key),
            .root_slot => |key| return try self.lowerRootSlotThunkBody(key),
        };
        // Signature: explicit parameters, then the type table, which holds
        // the descriptors, dictionaries, and literal arguments.
        var args: std.ArrayList(LocalId) = .empty;
        defer args.deinit(self.allocator);
        self.entry_hole = try self.newHole();
        const entry = self.entry_hole;
        if (self.program.inputCount(self.worker_id) != 0) {
            const table = try self.closedLocal(.opaque_ptr);
            try self.beginTable(table, worker.dictionary_params.len);
        }
        const params = try self.allocator.alloc(LocalId, worker.params.len);
        defer self.allocator.free(params);
        for (params, worker.params) |*param, param_ty| param.* = try self.localOf(param_ty, .param);
        try args.appendSlice(self.allocator, params);
        if (self.table_local) |table| try args.append(self.allocator, table);
        var ret_layout: layout.Idx = undefined;
        if (try self.closed(worker.ret)) {
            ret_layout = try self.program.layouts.layoutOf(worker.ret);
        } else {
            ret_layout = try self.program.result.layouts.dynamicLayout();
            self.result_local = try self.localOf(worker.ret, .result);
        }

        const body_start = try self.newHole();
        self.hole = body_start;
        switch (template.body) {
            .checked_body => |root_expr| {
                switch (self.bodies().exprData(root_expr)) {
                    .lambda => |lambda| {
                        if (lambda.args.len != params.len) invariant("a template's lambda arity differs from its signature");
                        for (lambda.args, params, worker.params) |pattern, param, param_ty| try self.bindIrrefutable(pattern, param, param_ty);
                        try self.tasks.append(self.allocator, .ret);
                        // The body's value is built at the function's result
                        // type, which its rows join (design.md "Row Joins").
                        try self.destination_types.put(self.allocator, lambda.body, worker.ret);
                        try self.tail_exprs.put(self.allocator, lambda.body, {});
                        try self.tasks.append(self.allocator, .{ .expr = lambda.body });
                    },
                    .hosted_lambda => {
                        // A hosted procedure is an extern at its declaration's
                        // own type (design.md "Host ABI").
                        if (worker.dictionary_params.len != 0) invariant("a hosted declaration has no where requirements, which checking rejects");
                        const key = worker.key.template;
                        const hosted = self.program.hosted.get(.{ .module = key.module, .template = key.template }) orelse
                            invariant("a hosted procedure has no host binding");
                        if (hosted.dispatch_index) |dispatch_index| {
                            if (worker.scheme_vars.len == 0) {
                                const proc = self.store.getProcSpecPtr(worker.proc);
                                proc.args = try self.store.addLocalSpan(params);
                                proc.ret_layout = ret_layout;
                                proc.hosted = .{ .symbol = try self.store.insertString(hosted.symbol), .dispatch_index = dispatch_index };
                                return;
                            }
                            try self.callGenericExtern(hosted.symbol, dispatch_index, params, worker.params, worker.ret);
                        } else {
                            // The platform's hosted section omits it: it has
                            // no host slot, as checking reported.
                            const message = try self.store.insertString("a hosted function the platform does not provide was called");
                            try self.emitTerminal(.{ .crash = .{ .msg = .{ .literal = message }, .checked_error = true } });
                        }
                    },
                    .numeral, .str_segment, .str, .bytes_literal, .lookup_local, .lookup_external, .lookup_required, .list, .empty_list, .tuple, .match_, .if_, .call, .record, .empty_record, .block, .tag, .nominal, .zero_argument_tag, .closure, .binop, .unary_minus, .field_access, .dispatch_call, .interpolation, .structural_eq, .structural_hash, .method_eq, .type_dispatch_call, .tuple_access, .runtime_error, .crash, .dbg, .expect_err, .expect, .ellipsis, .anno_only, .break_, .return_, .for_, .run_low_level => {
                        if (params.len != 0) invariant("a non-lambda template body has parameters");
                        try self.tasks.append(self.allocator, .ret);
                        try self.tasks.append(self.allocator, .{ .own_expr = root_expr });
                    },
                }
            },
            .unimplemented => |unimplemented| {
                self.region = unimplemented.region;
                const message = try self.store.insertString(@import("../common.zig").unimplemented_declaration_crash);
                try self.emitTerminal(.{ .crash = .{ .msg = .{ .literal = message } } });
            },
            .intrinsic_wrapper => |wrapper| try Codec.lowerIntrinsic(self, self.view.view.intrinsic_wrappers.get(wrapper).intrinsic, params),
            .entry_wrapper => |wrapper_id| {
                const wrapper = self.view.view.entry_wrappers.get(wrapper_id);
                const root = self.view.view.compile_time_roots.root(wrapper.root);
                // A conversion root's body is its literal, whose own
                // conversion runs here (`restoreConversionRoot`); a
                // validation root's body is the destructure it validates,
                // whose unit result the evaluator discards.
                if (params.len != 0) invariant("an entry wrapper has parameters");
                self.own_root = wrapper.root;
                self.comptime_root_body = root.kind != .literal_argument and root.literalConversionKind() == null;
                try self.tasks.append(self.allocator, .ret);
                // Unit producers also supply factories for CTFE, and contextual
                // constants retain factory bodies. Polymorphic constants are
                // explicit evaluation templates, instantiated at their runtime
                // read rather than CTFE roots. A whole runtime program instead
                // promises a completed value for context-free roots.
                if (self.program.moduleUnit() == null and !self.program.input.compile_time and root.request_eligibility == .eligible and root.payloadFor(.boxy) == .pending) {
                    // Exact aliases also compute their already-proven
                    // procedure reference without an evaluator.
                    const forwards = for (self.view.view.callable_eval_templates.templates) |alias| {
                        if (alias.root == wrapper.root) break alias.forwarded_lookup != null;
                    } else false;
                    if (!forwards) invariant("a runtime entry wrapper has no completed Boxy value");
                }
                if (root.boxy_value) |stored| {
                    try self.push(.{ .restore = .{ .source = .{ .module = self.module }, .node = stored.node, .ty = self.currentWorker().ret } });
                } else switch (root.kind) {
                    .expect, .repl_expr, .callable_binding, .constant, .hoisted_constant, .numeral_conversion, .quote_conversion, .hoisted_validation => switch (root.payloadFor(.boxy)) {
                        // In a module's object, an evaluated root's wrapper
                        // produces the stored value: the object defines it
                        // for the other objects of a link that read the
                        // root. A program runs a root it is asked to run.
                        .const_node => |node| if (self.program.moduleUnit() != null)
                            try self.push(.{ .restore = .{ .source = .{ .module = self.module }, .node = node, .ty = self.currentWorker().ret, .root = wrapper.root } })
                        else
                            try self.tasks.append(self.allocator, .{ .root_body = wrapper.root }),
                        .fn_value => invariant("a Boxy root view exposed an LSS function"),
                        .pending, .unevaluated, .discarded, .expect, .runtime => try self.tasks.append(self.allocator, .{ .root_body = wrapper.root }),
                    },
                    // A literal argument's body is the conversion its root
                    // names, called on another module's literal.
                    .literal_argument => try self.lowerLiteralArgumentRoot(wrapper.root, root.literal_argument orelse invariant("a literal argument root names no literal")),
                }
            },
        }
        try self.run();

        self.tail_calls.loop_params = args.items;
        self.tail_calls.instance_start = params.len;
        defer self.tail_calls.loop_params = &.{};
        try self.finishProc(entry, body_start, args.items, ret_layout);
    }

    /// The body of a generic hosted worker: a direct call of the extern at
    /// the declaration's own type. A variable slot sits inside a `Box`, a
    /// pointer whatever the variable is, so each value crosses as the bytes
    /// the calling Roc code holds, and the extern takes no descriptor
    /// (design.md "Host ABI").
    fn callGenericExtern(self: *Builder, symbol: []const u8, dispatch_index: u32, params: []const LocalId, param_types: []const TypeId, ret_ty: TypeId) Allocator.Error!void {
        const extern_args = try self.allocator.alloc(LocalId, params.len);
        defer self.allocator.free(extern_args);
        const extern_params = try self.allocator.alloc(LocalId, params.len);
        defer self.allocator.free(extern_params);
        for (params, param_types, extern_args, extern_params) |param, param_ty, *arg, *extern_param| {
            const abi = try self.hostAbiLayout(param_ty);
            extern_param.* = try self.closedLocal(abi);
            if (try self.closed(param_ty)) {
                arg.* = param;
                continue;
            }
            arg.* = try self.closedLocal(abi);
            try self.emitDyn(arg.*, .{ .copy = .{ .src = param, .desc = try self.descOf(param_ty) } });
        }
        const ret_abi = try self.hostAbiLayout(ret_ty);
        const extern_proc = try self.store.addProcSpec(.{
            .name = self.store.freshSyntheticSymbol(),
            .identity = self.program.programLocalIdentity("boxy.hosted_extern", @backingInt(self.worker_id)),
            .emission_unit = .isolated,
            .args = try self.store.addLocalSpan(extern_params),
            .ret_layout = ret_abi,
            .hosted = .{ .symbol = try self.store.insertString(symbol), .dispatch_index = dispatch_index },
        }, base.SourceLoc.none);
        const result = try self.closedLocal(ret_abi);
        try self.emitLinear(.{ .assign_call = .{ .target = result, .proc = extern_proc, .args = try self.store.addLocalSpan(extern_args), .next = undefined } });
        if (try self.closed(ret_ty)) {
            try self.pushValue(result);
        } else {
            const value = try self.localOf(ret_ty, .frame);
            try self.emitDyn(value, .{ .copy = .{ .src = result, .desc = try self.descOf(ret_ty) } });
            try self.pushValue(value);
        }
        try self.step(.ret);
    }

    /// The layout a hosted declaration's type `root` has at the host: its
    /// committed layout where it is closed, and where a `Box` holds one of
    /// the declaration's variables, the opaque box `RocBox(RocUnknown)`
    /// (design.md "Host ABI"). The worker's own values of such a box are
    /// dynamic locals, and each crossing copies between the two, so a local
    /// of the opaque box is never the last owner of its allocation.
    fn hostAbiLayout(self: *Builder, root: TypeId) Allocator.Error!layout.Idx {
        const store = &self.program.store;
        const layouts = &self.program.result.layouts;
        var done: collections.DenseMapUnmanaged(TypeId, layout.Idx) = .empty;
        defer done.deinit(self.allocator);
        const Frame = struct { ty: TypeId, expanded: bool };
        var stack: std.ArrayList(Frame) = .empty;
        defer stack.deinit(self.allocator);
        var children: std.ArrayList(TypeId) = .empty;
        defer children.deinit(self.allocator);
        try stack.append(self.allocator, .{ .ty = root, .expanded = false });
        while (stack.pop()) |frame| {
            if (done.contains(frame.ty)) continue;
            if (try self.closed(frame.ty)) {
                try done.put(self.allocator, frame.ty, try self.program.layouts.layoutOf(frame.ty));
                continue;
            }
            children.clearRetainingCapacity();
            switch (store.get(frame.ty)) {
                .box => |payload| if (!try self.closed(payload)) {
                    try done.put(self.allocator, frame.ty, try layouts.insertBox(.opaque_ptr));
                    continue;
                } else try children.append(self.allocator, payload),
                .nominal => try children.append(self.allocator, try self.program.converter().backing(frame.ty)),
                .tuple => |items| try children.appendSlice(self.allocator, store.span(items)),
                .record => |record| {
                    if (record.ext != null) invariant("a hosted declaration's record row is closed, as checking requires");
                    const storage = try self.program.layouts.recordStorage(record);
                    defer self.allocator.free(storage);
                    try children.appendSlice(self.allocator, storage);
                },
                .tag_union => |tag_union| {
                    if (tag_union.ext != null) invariant("a hosted declaration's tag row is closed, as checking requires");
                    const tags = try self.allocator.dupe(types.Tag, store.tagSpan(tag_union.tags));
                    defer self.allocator.free(tags);
                    for (tags) |tag| {
                        const payloads = try self.allocator.dupe(TypeId, store.span(tag.payloads));
                        defer self.allocator.free(payloads);
                        if (try self.program.layouts.tagPayload(payloads)) |payload| try children.append(self.allocator, payload);
                    }
                },
                .variable => invariant("a hosted declaration's variable appears only inside a Box, as checking requires"),
                .list => invariant("a hosted declaration's variable inside a List is outside a Box, which checking rejects"),
                .function, .primitive, .empty_record => unreachable, // closed
            }
            if (!frame.expanded) {
                try stack.append(self.allocator, .{ .ty = frame.ty, .expanded = true });
                for (children.items) |child| if (!done.contains(child)) try stack.append(self.allocator, .{ .ty = child, .expanded = false });
                continue;
            }
            const child_layouts = try self.allocator.alloc(layout.Idx, children.items.len);
            defer self.allocator.free(child_layouts);
            for (children.items, child_layouts) |child, *child_layout| child_layout.* = done.get(child).?;
            const built: layout.Idx = switch (store.get(frame.ty)) {
                .box => try layouts.insertBox(child_layouts[0]),
                .nominal => child_layouts[0],
                .record => |record| blk: {
                    const fields = try self.allocator.alloc(layout.StructField, child_layouts.len);
                    defer self.allocator.free(fields);
                    const declared = record.declared orelse {
                        for (fields, child_layouts, 0..) |*field, child_layout, index| field.* = .{ .index = @intCast(index), .layout = child_layout };
                        break :blk try layouts.putStructFields(fields);
                    };
                    for (fields, store.declaredSlots(declared)) |*field, slot| {
                        field.* = .{ .index = slot, .layout = child_layouts[slot], .is_padding = slot >= record.fields.len };
                    }
                    break :blk try layouts.putNominalStructFields(fields);
                },
                .tuple => blk: {
                    const fields = try self.allocator.alloc(layout.StructField, child_layouts.len);
                    defer self.allocator.free(fields);
                    for (fields, child_layouts, 0..) |*field, child_layout, index| field.* = .{ .index = @intCast(index), .layout = child_layout };
                    break :blk try layouts.putStructFields(fields);
                },
                .tag_union => |tag_union| blk: {
                    const tags = try self.allocator.dupe(types.Tag, store.tagSpan(tag_union.tags));
                    defer self.allocator.free(tags);
                    const variants = try self.allocator.alloc(layout.Idx, tags.len);
                    defer self.allocator.free(variants);
                    var next: usize = 0;
                    for (tags, variants) |tag, *variant| {
                        if (tag.payloads.len == 0) {
                            variant.* = .zst;
                        } else {
                            variant.* = child_layouts[next];
                            next += 1;
                        }
                    }
                    break :blk try layouts.putTagUnion(variants);
                },
                .primitive, .empty_record, .list, .function, .variable => unreachable, // only the composite kinds above expand
            };
            try done.put(self.allocator, frame.ty, built);
        }
        return done.get(root).?;
    }

    /// Whether any local of this procedure lives in the dynamic frame region.
    fn hasDynamicFrame(self: *const Builder) bool {
        for (self.frame_locals.items) |local| {
            const dyn = self.store.localDyn(local) orelse continue;
            if (dyn.storage == .frame) return true;
        }
        return false;
    }

    /// Link the entry chain to the body and finish the procedure.
    pub fn finishProc(self: *Builder, entry: CFStmtId, body_start: CFStmtId, args: []const LocalId, ret_layout: layout.Idx) Allocator.Error!void {
        const worker = self.currentWorker().*;
        const has_dynamic_frame = self.hasDynamicFrame();
        // Entry chain, then the dynamic frame region, then the body.
        if (has_dynamic_frame) {
            try self.emitEntry(.{ .dyn = .{ .target = null, .data = try self.store.addStmtRow(LIR.DynOp, .frame_alloc), .next = undefined } });
        }
        // The entry chain's last hole continues into the body: it becomes a
        // copy of the body's first statement.
        const proc_body: CFStmtId = if (entry == self.entry_hole) body_start else blk: {
            try self.store.replaceCFStmt(self.entry_hole, self.store.getCFStmt(body_start), self.store.stmtOrigin(body_start));
            // A self-tail loop re-enters the body after the entry chain,
            // whose descriptors every iteration shares.
            self.tail_calls.loop_entry = self.entry_hole;
            break :blk entry;
        };

        std.mem.sort(LocalId, self.frame_locals.items, {}, struct {
            fn lessThan(_: void, a: LocalId, b: LocalId) bool {
                return @backingInt(a) < @backingInt(b);
            }
        }.lessThan);
        std.mem.sort(LIR.JoinPoint, self.join_points.items, {}, struct {
            fn lessThan(_: void, a: LIR.JoinPoint, b: LIR.JoinPoint) bool {
                return @backingInt(a.id) < @backingInt(b.id);
            }
        }.lessThan);
        const proc = self.store.getProcSpecPtr(worker.proc);
        proc.args = try self.store.addLocalSpan(args);
        proc.ret_layout = ret_layout;
        proc.dyn_result = self.result_local;
        proc.frame_locals = try self.store.addLocalSpan(self.frame_locals.items);
        proc.join_points = try self.store.addJoinPointSpan(self.join_points.items);
        proc.body = proc_body;
        proc.shapes = self.store.shapes;
        self.store.shapes = .{};
        if (has_dynamic_frame) self.tail_calls.self_calls = .replace_frame;
        proc.tail_calls = try self.tail_calls.finish(self.store);
    }

    /// Crash with a checked runtime error; the value of type `ty` it stands
    /// for is never produced. A value whose type holds the erroneous type
    /// has no type of its own, and no reachable statement reads the
    /// zero-sized local standing for it.
    pub fn runtimeErrorValue(self: *Builder, ty: ?TypeId) Allocator.Error!void {
        const literal = try self.store.insertString("runtime error");
        try self.emitTerminal(.{ .crash = .{ .msg = .{ .literal = literal }, .checked_error = true } });
        try self.pushValue(if (ty) |value_ty| try self.localOf(value_ty, .frame) else try self.closedLocal(.zst));
    }

    /// A lambda worker in the uniform erased-callable ABI: its one argument
    /// is the packed argument record, and its captures hold the captured
    /// values followed by the outer worker's descriptors.
    pub fn lowerLambdaBody(self: *Builder, lambda_expr: ExprId) Allocator.Error!void {
        const worker = self.currentWorker().*;
        const lambda = switch (self.bodies().exprData(lambda_expr)) {
            .lambda => |lambda| lambda,
            .numeral, .str_segment, .str, .bytes_literal, .lookup_local, .lookup_external, .lookup_required, .list, .empty_list, .tuple, .match_, .if_, .call, .record, .empty_record, .block, .tag, .nominal, .zero_argument_tag, .closure, .binop, .unary_minus, .field_access, .dispatch_call, .interpolation, .structural_eq, .structural_hash, .method_eq, .type_dispatch_call, .tuple_access, .runtime_error, .crash, .dbg, .expect_err, .expect, .ellipsis, .anno_only, .break_, .return_, .for_, .hosted_lambda, .run_low_level => invariant("a lambda worker's expression is not a lambda"),
        };
        self.entry_hole = try self.newHole();
        const entry = self.entry_hole;

        // Captures: values, then the type table, unless the procedure's
        // calls pass it (`Program.perCall`).
        const capture_ptr = try self.closedLocal(.opaque_ptr);
        const dict_count = worker.own_dicts + worker.capturedDictionaryCount();
        const per_call = self.program.perCall(self.worker_id);
        // A procedure whose calls pass its table reads it from its argument
        // record, before the captured values whose types it describes.
        const own_record: ?LocalId = if (per_call) try self.ownScopeArgs(worker, dict_count) else null;
        const captured_words: usize = if (self.program.inputCount(self.worker_id) != 0 and !per_call) 1 else 0;
        const capture_count = worker.capture_types.len + captured_words;
        var capture_values: []LocalId = &.{};
        if (!try self.allClosed(worker.capture_types)) {
            _ = try self.beginReuseArg(.dynamic);
            capture_values = try self.dynamicCaptures(worker, capture_ptr, captured_words);
        } else if (capture_count != 0) {
            const capture_layout = try self.captureLayout(worker.capture_types, captured_words);
            _ = try self.beginReuseArg(.{ .static = capture_layout });
            // A record of zero-sized captures has no storage to read.
            const record: ?LocalId = if (capture_layout == .zst) null else try self.closedLocal(capture_layout);
            if (record) |loaded| try self.emitEntry(.{ .assign_low_level = .{
                .target = loaded,
                .op = .erased_capture_load,
                .rc_effect = try self.store.addRcEffect(LIR.LowLevel.erased_capture_load.rcEffect()),
                .args = try self.store.addLocalSpan(&.{capture_ptr}),
                .next = undefined,
            } });
            capture_values = try self.allocator.alloc(LocalId, capture_count);
            for (capture_values, 0..) |*slot, index| {
                const field_layout = if (index < worker.capture_types.len)
                    try self.program.layouts.layoutOf(worker.capture_types[index])
                else
                    layout.Idx.opaque_ptr;
                slot.* = try self.closedLocal(field_layout);
                if (record) |loaded| {
                    try self.emitEntry(.{ .assign_ref = .{ .target = slot.*, .op = LIR.RefOp.field(loaded, @intCast(index)), .next = undefined } });
                } else {
                    if (field_layout != .zst) invariant("a zero-sized capture record holds a capture with storage");
                    try self.emitEntry(.{ .assign_struct = .{ .target = slot.*, .fields = LIR.LocalSpan.empty(), .next = undefined } });
                }
            }
        }
        if (captured_words != 0 and self.table_local == null) try self.beginTable(capture_values[worker.capture_types.len], dict_count);
        defer if (capture_count != 0) self.allocator.free(capture_values);
        for (worker.captures, worker.capture_types, 0..) |capture, capture_ty, index| {
            const binder = capture.pattern;
            if (self.bodies().binder(binder).recursive_value) {
                // A recursive value's slot: the value is read from it on
                // entry, and the slot stays in scope for the callables this
                // body builds that capture the binder too.
                try self.recursive_slots.put(self.allocator, binder, capture_values[index]);
                try self.binders.put(self.allocator, binder, try self.readRecursiveSlot(capture_values[index], capture_ty));
            } else {
                try self.binders.put(self.allocator, binder, capture_values[index]);
            }
        }

        // The argument record.
        var args: std.ArrayList(LocalId) = .empty;
        defer args.deinit(self.allocator);
        var record_param: ?LocalId = null;
        const own_words: u32 = if (per_call) 1 else 0;
        if (own_record) |own| {
            record_param = own;
            try args.append(self.allocator, own);
        } else if (worker.params.len != 0) {
            record_param = try self.erasedParamLocal(worker.params);
            try args.append(self.allocator, record_param.?);
        }
        var ret_layout: layout.Idx = undefined;
        if (try self.closed(worker.ret)) {
            ret_layout = try self.program.layouts.layoutOf(worker.ret);
        } else {
            ret_layout = try self.program.result.layouts.dynamicLayout();
            self.result_local = try self.localOf(worker.ret, .result);
        }

        const body_start = try self.newHole();
        self.hole = body_start;
        if (lambda.args.len != worker.params.len) invariant("a lambda's arity differs from its type");
        for (lambda.args, worker.params, 0..) |pattern, param_ty, index| {
            const value = if (own_words != 0)
                try self.argsRecordField(record_param.?, @intCast(own_words + index), param_ty)
            else
                try self.erasedParamValue(record_param.?, worker.params, @intCast(index));
            try self.bindIrrefutable(pattern, value, param_ty);
        }
        try self.tasks.append(self.allocator, .ret);
        try self.destination_types.put(self.allocator, lambda.body, worker.ret);
        try self.tail_exprs.put(self.allocator, lambda.body, {});
        try self.tasks.append(self.allocator, .{ .expr = lambda.body });
        try self.run();
        if (self.reuse_arg == null) _ = try self.beginReuseArg(.none);
        try self.finishErasedProc(entry, body_start, args.items, ret_layout, capture_ptr);
    }

    /// An adapter's body (`WorkerKey.adapter`): each argument adapted to the
    /// called function's storage, the call, and its result adapted back.
    pub fn lowerAdapterBody(self: *Builder, from: TypeId) Allocator.Error!void {
        const worker = self.currentWorker().*;
        self.entry_hole = try self.newHole();
        const entry = self.entry_hole;
        const capture_ptr = try self.closedLocal(.opaque_ptr);
        // Captures: the adapted function value, then the type table.
        const has_table = self.program.inputCount(self.worker_id) != 0;
        const capture_layout = try self.captureLayout(worker.capture_types, @intFromBool(has_table));
        const record = try self.closedLocal(capture_layout);
        try self.emitEntry(.{ .assign_low_level = .{
            .target = record,
            .op = .erased_capture_load,
            .rc_effect = try self.store.addRcEffect(LIR.LowLevel.erased_capture_load.rcEffect()),
            .args = try self.store.addLocalSpan(&.{capture_ptr}),
            .next = undefined,
        } });
        const inner = try self.closedLocal(try self.erasedLayout());
        try self.emitEntry(.{ .assign_ref = .{ .target = inner, .op = LIR.RefOp.field(record, 0), .next = undefined } });
        if (has_table) {
            const table = try self.closedLocal(.opaque_ptr);
            try self.emitEntry(.{ .assign_ref = .{ .target = table, .op = LIR.RefOp.field(record, 1), .next = undefined } });
            try self.beginTable(table, 0);
        }
        var args: std.ArrayList(LocalId) = .empty;
        defer args.deinit(self.allocator);
        const record_param: ?LocalId = if (worker.params.len != 0) try self.erasedParamLocal(worker.params) else null;
        if (record_param) |param| try args.append(self.allocator, param);
        var ret_layout: layout.Idx = undefined;
        if (try self.closed(worker.ret)) {
            ret_layout = try self.program.layouts.layoutOf(worker.ret);
        } else {
            ret_layout = try self.program.result.layouts.dynamicLayout();
            self.result_local = try self.localOf(worker.ret, .result);
        }
        const body_start = try self.newHole();
        self.hole = body_start;
        const signature = switch (self.program.store.get(from)) {
            .function => |function| function,
            .primitive, .empty_record, .record, .tuple, .tag_union, .list, .box, .nominal, .variable => invariant("an adapted function value's type is not a function type"),
        };
        const from_params = try self.allocator.dupe(TypeId, self.program.store.span(signature.args));
        defer self.allocator.free(from_params);
        const adapted = try self.allocator.alloc(LocalId, worker.params.len);
        defer self.allocator.free(adapted);
        for (worker.params, from_params, adapted, 0..) |param_ty, from_ty, *value, index| {
            const arg = try self.erasedParamValue(record_param.?, worker.params, @intCast(index));
            value.* = try self.adaptValue(arg, param_ty, from_ty);
        }
        const result = try self.emitErasedCall(inner, from, adapted, signature.ret);
        try self.pushValue(try self.adaptValue(result, signature.ret, worker.ret));
        try self.tasks.append(self.allocator, .ret);
        try self.run();
        _ = try self.beginReuseArg(.none);
        try self.finishErasedProc(entry, body_start, args.items, ret_layout, capture_ptr);
    }

    pub const StorageDifference = Program.StorageDifference;

    /// Where two types of one value differ (`Program.storageDifference`).
    pub fn storageDifference(self: *Builder, from_root: TypeId, to_root: TypeId) Allocator.Error!StorageDifference {
        return try self.program.storageDifference(from_root, to_root);
    }

    /// The value `value` of type `from` at type `to`, a type of the same
    /// value whose rows may be wider (design.md "Sealed Result Rows"):
    /// itself where the representations agree; converted between two closed
    /// types whose rows differ by the module's conversion worker; rebuilt by
    /// the descriptor-driven `adapt` where a row the body opens differs; and
    /// a function value adapting the calls where a function's signature does.
    pub fn adaptValue(self: *Builder, value: LocalId, from: TypeId, to: TypeId) Allocator.Error!LocalId {
        switch (try self.storageDifference(from, to)) {
            .none => {
                if (from != to and try self.closed(from) and try self.closed(to)) {
                    const from_layout = try self.program.layouts.layoutOf(from);
                    const to_layout = try self.program.layouts.layoutOf(to);
                    if (from_layout != to_layout) {
                        var from_text: std.Io.Writer.Allocating = .init(self.allocator);
                        defer from_text.deinit();
                        var to_text: std.Io.Writer.Allocating = .init(self.allocator);
                        defer to_text.deinit();
                        self.debugTypeText(from, &from_text.writer, 0) catch {};
                        self.debugTypeText(to, &to_text.writer, 0) catch {};
                        invariantFmt("types of one storage have different layouts: {s} vs {s}", .{ from_text.written(), to_text.written() });
                    }
                }
                return value;
            },
            .rows => {
                if (try self.closed(from) and try self.closed(to)) return try self.convertRows(value, from, to);
                const target = try self.localOf(to, .frame);
                try self.emitAdapt(target, value, from, to);
                return target;
            },
            // Records hold the difference at any depth, even below a list,
            // so it is rebuilt by descriptors whatever the types.
            .record_order => {
                const target = try self.localOf(to, .frame);
                try self.emitAdapt(target, value, from, to);
                return target;
            },
            .function => {
                const adapter = try self.program.adapterWorker(self.worker_id, from, to);
                const inputs = try self.descInputWords(0, self.program.descCount(adapter));
                const static_inputs = for (inputs) |word| {
                    if (word == .recipe and try self.readsInputs(word.recipe)) break false;
                } else true;
                if (static_inputs and self.constant_slots.contains(value)) return self.staticClosure(adapter, &.{value}, &.{from}, inputs, false, null);
                return try self.packClosure(adapter, &.{value}, &.{from}, inputs);
            },
        }
    }

    /// A produced value `value` of its own type `from` at its destination
    /// type `to` (`retagTarget`): between two closed types, `adaptValue`'s
    /// static conversion; otherwise a destination at least one of whose
    /// rows a type variable of the body opens, so the value is rebuilt by
    /// the descriptor-driven `adapt` in a local of the destination's kind,
    /// whatever the two types' difference (a closed value's bytes placed in
    /// dynamic storage, or a row re-tagged by name).
    pub fn retagValue(self: *Builder, value: LocalId, from: TypeId, to: TypeId) Allocator.Error!LocalId {
        if (try self.closed(from) and try self.closed(to)) return try self.adaptValue(value, from, to);
        const target = try self.localOf(to, .frame);
        try self.emitAdapt(target, value, from, to);
        return target;
    }

    /// `value` of the closed type `from` converted to the closed type `to`,
    /// whose rows widen `from`'s (design.md "Sealed Result Rows"): itself
    /// when the two types commit one layout, otherwise the result of this
    /// module's conversion worker for the pair.
    pub fn convertRows(self: *Builder, value: LocalId, from: TypeId, to: TypeId) Allocator.Error!LocalId {
        if (try self.program.layouts.layoutOf(from) == try self.program.layouts.layoutOf(to)) return value;
        const worker = try self.program.convertWorker(self.module, from, to);
        const target = try self.localOf(to, .frame);
        try self.emitLinear(.{ .assign_call = .{ .target = target, .proc = self.program.worker(worker).proc, .args = try self.store.addLocalSpan(&.{value}), .next = undefined } });
        return target;
    }

    /// A conversion worker's body (`WorkerKey.convert`): its one parameter
    /// converted to its result type along the coercion's reach.
    pub fn lowerConvertBody(self: *Builder, from: TypeId, to: TypeId) Allocator.Error!void {
        self.entry_hole = try self.newHole();
        const entry = self.entry_hole;
        self.var_descs = &.{};
        self.dict_locals = &.{};
        const param = try self.localOf(from, .param);
        const ret_layout = try self.program.layouts.layoutOf(to);
        const body_start = try self.newHole();
        self.hole = body_start;
        const result = try self.convertValue(param, from, to);
        if (self.hole != null) try self.emitTerminal(.{ .ret = .{ .value = result } });
        try self.finishProc(entry, body_start, &.{param}, ret_layout);
    }

    /// `value` of `from` rebuilt at `to`, one reached row at a time: a
    /// switch on the source discriminant whose arm for each source variant
    /// builds the destination's variant of the same name, with each payload
    /// whose types differ converted in turn (`adaptValue`, which reaches
    /// the worker for that pair). Positions outside the reach are identical
    /// on both sides by the checker's rule.
    fn convertValue(self: *Builder, value: LocalId, from_raw: TypeId, to_raw: TypeId) Allocator.Error!LocalId {
        const store = &self.program.store;
        const from = try self.structuralType(from_raw);
        const to = try self.structuralType(to_raw);
        const from_union = switch (store.get(from)) {
            .tag_union => |union_ty| union_ty,
            .primitive, .empty_record, .record, .tuple, .list, .box, .nominal, .function, .variable => invariant("a row conversion's source is not a tag union"),
        };
        switch (store.get(to)) {
            .tag_union => {},
            .primitive, .empty_record, .record, .tuple, .list, .box, .nominal, .function, .variable => invariant("a row conversion's destination is not a tag union"),
        }
        // Building payloads interns types, which may grow the store under
        // its own spans.
        const from_tags = try self.allocator.dupe(types.Tag, store.tagSpan(from_union.tags));
        defer self.allocator.free(from_tags);
        if (from_tags.len == 0) {
            // No value has the empty row, so no conversion of one runs.
            try self.emitTerminal(.runtime_error);
            return try self.localOf(to_raw, .frame);
        }
        const result = try self.localOf(to_raw, .frame);
        const join = self.freshJoin();
        const after = try self.declareMergeJoin(join, result, null);
        const committed = self.program.result.layouts.getLayout(try self.program.layouts.layoutOf(from));
        const arms = try self.allocator.alloc(CFStmtId, from_tags.len);
        defer self.allocator.free(arms);
        if (from_tags.len > 1) {
            if (committed.tag != .tag_union) invariant("a tag union of several variants has no discriminant storage");
            const discriminant = try self.closedLocal(.u32);
            try self.emitLinear(.{ .assign_ref = .{ .target = discriminant, .op = LIR.RefOp.discriminant(value), .next = undefined } });
            for (arms) |*arm| arm.* = try self.newHole();
            const branches = try self.allocator.alloc(LIR.CFSwitchBranch, from_tags.len - 1);
            defer self.allocator.free(branches);
            for (branches, 0..) |*branch, index| branch.* = .{ .value = @intCast(index), .body = arms[index] };
            try self.emitTerminal(.{ .switch_stmt = .{ .cond = discriminant, .branches = try self.store.addCFSwitchBranches(branches), .default_branch = arms[from_tags.len - 1] } });
        } else {
            arms[0] = try self.currentHole();
        }
        for (from_tags, arms, 0..) |tag, arm, variant| {
            self.hole = arm;
            const payload_types = try self.allocator.dupe(TypeId, store.span(tag.payloads));
            defer self.allocator.free(payload_types);
            const destination = self.ownTag(to, tag.name, payload_types.len);
            const destination_payloads = try self.allocator.dupe(TypeId, store.span(destination.payloads));
            defer self.allocator.free(destination_payloads);
            const values = try self.allocator.alloc(LocalId, payload_types.len);
            defer self.allocator.free(values);
            for (values, payload_types, destination_payloads, 0..) |*slot, payload_ty, destination_ty, index| {
                const payload = if (committed.tag != .tag_union) blk: {
                    // A union without storage has only zero-sized payloads.
                    const payload_local = try self.localOf(payload_ty, .frame);
                    if (self.store.getLocal(payload_local).layout_idx != .zst) invariant("a storage-free union has a payload with storage");
                    try self.emitLinear(.{ .assign_struct = .{ .target = payload_local, .fields = LIR.LocalSpan.empty(), .next = undefined } });
                    break :blk payload_local;
                } else try self.readTagPayload(value, from, variant, payload_types, index);
                slot.* = try self.adaptValue(payload, payload_ty, destination_ty);
            }
            const built = try self.buildTag(to_raw, tag.name, values);
            try self.emitJump(join, built);
        }
        self.hole = after;
        return result;
    }

    /// Dictionary `dictionary` of callable type `from` adapted to `to`: an
    /// immortal callable of the adapter worker (`WorkerKey.adapter`)
    /// capturing the dictionary and this worker's descriptors, as an adapted
    /// function value captures them, where the two types store the
    /// function's signature differently.
    pub fn adaptDictionary(self: *Builder, dictionary: Word, from: TypeId, to: TypeId) Allocator.Error!Word {
        switch (try self.storageDifference(from, to)) {
            .none => return dictionary,
            .function => {
                // The adapter captures the dictionary, then its table.
                const adapter = try self.program.adapterWorker(self.worker_id, from, to);
                const descs = try self.descInputWords(0, self.program.descCount(adapter));
                var captures: [2]Word = .{ dictionary, undefined };
                const count: usize = if (descs.len == 0) 1 else 2;
                if (descs.len != 0) captures[1] = try self.tableWord(adapter, descs);
                return .{ .recipe = try self.recipes.intern(.{ .callable = .{ .entry = adapter, .captures = captures[0..count] } }) };
            },
            .rows, .record_order => invariant("two callable types of a dictionary differ outside the signature of the function it is"),
        }
    }

    /// Read a dynamic capture record at `capture_ptr`: its descriptor word,
    /// the descriptor and dictionary words (which the captured values'
    /// descriptors compose from), then views of the captured values. Returns
    /// the capture values in the closed record's order: values, descriptors,
    /// dictionaries.
    /// The argument record of a generalized local procedure: its own
    /// scope's descriptors and dictionaries, which vary per call, then its
    /// explicit arguments. The leading words are read through a descriptor
    /// of just those words; the whole record's descriptor composes from them.
    /// The argument record of a procedure whose calls pass its type table
    /// (`Program.perCall`): the table, which this reads first, then the
    /// explicit arguments.
    pub fn ownScopeArgs(self: *Builder, worker: lower.Worker, dict_count: usize) Allocator.Error!LocalId {
        // The record is read through its own descriptor, which the table
        // determines; the table is read through the descriptor of just the
        // leading word first.
        const desc_hole = try self.closedLocal(.opaque_ptr);
        const record = try self.addLocal(.{ .layout_idx = try self.program.result.layouts.dynamicLayout(), .dyn = .{ .desc = desc_hole, .storage = .param } });
        const prefix_desc = try self.argsDesc(1, &.{});
        const table = try self.closedLocal(.opaque_ptr);
        try self.emitEntry(.{ .dyn = .{ .target = table, .data = try self.store.addStmtRow(LIR.DynOp, .{ .field_addr = .{ .base = record, .desc = prefix_desc, .field_index = .{ .immediate = 0 } } }), .next = undefined } });
        try self.beginTable(table, dict_count);
        const full_desc = try self.argsDesc(1, try self.descWords(worker.params));
        try self.emitEntry(.{ .assign_ref = .{ .target = desc_hole, .op = LIR.RefOp.local(full_desc), .next = undefined } });
        const product = try self.productShapeFields(1, worker.params);
        self.store.getLocalPtr(record).product = product;
        return record;
    }

    /// The captures of a dynamic capture record: its `word_count` words
    /// after its own descriptor and capacity, then its values.
    pub fn dynamicCaptures(self: *Builder, worker: lower.Worker, capture_ptr: LocalId, word_count: usize) Allocator.Error![]LocalId {
        const record_desc = try self.closedLocal(.opaque_ptr);
        try self.emitEntry(.{ .assign_low_level = .{
            .target = record_desc,
            .op = .erased_capture_load,
            .rc_effect = try self.store.addRcEffect(LIR.LowLevel.erased_capture_load.rcEffect()),
            .args = try self.store.addLocalSpan(&.{capture_ptr}),
            .next = undefined,
        } });
        // The record's bytes are in the allocation the reuse argument
        // holds, so reads of them count as uses of it: a packing that
        // reuses the allocation while a read is still to come retains it,
        // and the runtime then packs afresh.
        const record = try self.addLocal(.{ .layout_idx = try self.program.result.layouts.dynamicLayout(), .dyn = .{ .desc = record_desc, .storage = .view } });
        try self.emitEntry(.{ .dyn = .{ .target = record, .data = try self.store.addStmtRow(LIR.DynOp, .{ .view = .{ .address = capture_ptr, .owner = self.reuse_arg } }), .next = undefined } });
        const values = try self.allocator.alloc(LocalId, worker.capture_types.len + word_count);
        for (0..word_count) |index| {
            const word = try self.closedLocal(.opaque_ptr);
            try self.emitEntry(.{ .dyn = .{ .target = word, .data = try self.store.addStmtRow(LIR.DynOp, .{ .field_addr = .{ .base = record, .desc = record_desc, .field_index = .{ .immediate = @intCast(capture_record_leading_words + index) } } }), .next = undefined } });
            values[worker.capture_types.len + index] = word;
        }
        // The captured values' types are read from the table, so it is read
        // before them.
        if (word_count != 0) try self.beginTable(values[worker.capture_types.len], worker.own_dicts + worker.capturedDictionaryCount());
        for (worker.capture_types, 0..) |ty, index| {
            const local = try self.localOf(ty, .view);
            try self.emitEntry(.{ .dyn = .{ .target = local, .data = try self.store.addStmtRow(LIR.DynOp, .{ .field_addr = .{ .base = record, .desc = record_desc, .field_index = .{ .immediate = @intCast(capture_record_leading_words + word_count + index) } } }), .next = undefined } });
            values[index] = local;
        }
        return values;
    }

    pub fn erasedLayout(self: *Builder) Allocator.Error!layout.Idx {
        return self.program.result.layouts.insertErasedCallable();
    }

    /// Load an erased worker's capture record (closed fields of
    /// `field_layouts`) at the start of its entry chain, returning its fields.
    pub fn loadCaptures(self: *Builder, capture_ptr: LocalId, field_layouts: []const layout.Idx) Allocator.Error![]LocalId {
        const fields = try self.allocator.alloc(layout.StructField, field_layouts.len);
        defer self.allocator.free(fields);
        for (fields, field_layouts, 0..) |*field, field_layout, index| field.* = .{ .index = @intCast(index), .layout = field_layout };
        const record_layout = try self.program.result.layouts.putStructFields(fields);
        const values = try self.allocator.alloc(LocalId, field_layouts.len);
        if (record_layout == .zst) {
            // Zero-sized captures are stored nowhere; each is built empty.
            for (values, field_layouts) |*value, field_layout| {
                if (field_layout != .zst) invariant("a zero-sized capture record holds a capture with storage");
                value.* = try self.closedLocal(.zst);
                try self.emitEntry(.{ .assign_struct = .{ .target = value.*, .fields = LIR.LocalSpan.empty(), .next = undefined } });
            }
            return values;
        }
        const record = try self.closedLocal(record_layout);
        try self.emitEntry(.{ .assign_low_level = .{
            .target = record,
            .op = .erased_capture_load,
            .rc_effect = try self.store.addRcEffect(LIR.LowLevel.erased_capture_load.rcEffect()),
            .args = try self.store.addLocalSpan(&.{capture_ptr}),
            .next = undefined,
        } });
        for (values, field_layouts, 0..) |*value, field_layout, index| {
            value.* = try self.closedLocal(field_layout);
            try self.emitEntry(.{ .assign_ref = .{ .target = value.*, .op = LIR.RefOp.field(record, @intCast(index)), .next = undefined } });
        }
        return values;
    }

    /// Begin an erased-ABI worker whose captures are `field_layouts`: entry
    /// chain, capture pointer, argument record, and result. Returns what
    /// `finishErasedWorker` needs.
    pub const ErasedStart = struct {
        entry: CFStmtId,
        body_start: CFStmtId,
        capture_ptr: LocalId,
        captures: []LocalId,
        args: []LocalId,
        record: ?LocalId,
        ret_layout: layout.Idx,
    };

    pub fn beginErasedWorker(self: *Builder, field_layouts: []const layout.Idx) Allocator.Error!ErasedStart {
        self.entry_hole = try self.newHole();
        const entry = self.entry_hole;
        const capture_ptr = try self.closedLocal(.opaque_ptr);
        _ = try self.beginReuseArg(.none);
        const captures = if (field_layouts.len == 0) try self.allocator.alloc(LocalId, 0) else try self.loadCaptures(capture_ptr, field_layouts);
        return .{
            .entry = entry,
            .body_start = undefined,
            .capture_ptr = capture_ptr,
            .captures = captures,
            .args = &.{},
            .record = null,
            .ret_layout = undefined,
        };
    }

    /// The entry of a template worker reached through a dictionary.
    /// A root's host-visible wrapper: its checked arguments, passed to the
    /// root's target with the hidden arguments the root's instantiation
    /// names, or to the function value compile-time evaluation stored.
    /// A static data initializer's body: the requested stored constant,
    /// restored at its type.
    pub fn lowerStaticDataInitializer(self: *Builder) Allocator.Error!void {
        const worker = self.currentWorker().*;
        const request = worker.static_data_request.?;
        self.entry_hole = try self.newHole();
        const entry = self.entry_hole;
        self.var_descs = &.{};
        self.dict_locals = &.{};
        const ret_layout = try self.program.layouts.layoutOf(worker.ret);
        const body_start = try self.newHole();
        self.hole = body_start;
        const owner = self.program.moduleByKey(checked.constModuleId(request.const_locator));
        try self.tasks.append(self.allocator, .ret);
        if (rootOfConstRef(owner, request.const_locator)) |root_id| {
            if (owner.view.compile_time_roots.root(root_id).payloadFor(.boxy) == .pending) {
                try self.pendingValueRead(owner.index, root_id, worker.ret);
                try self.run();
                return try self.finishProc(entry, body_start, &.{}, ret_layout);
            }
        }
        const boxy_value = if (rootOfConstRef(owner, request.const_locator)) |root_id| owner.view.compile_time_roots.root(root_id).boxy_value else null;
        if (boxy_value) |stored| try self.tasks.append(self.allocator, .{ .restore = .{ .source = .{ .module = owner.index }, .node = stored.node, .ty = worker.ret } }) else switch (owner.view.const_templates.get(request.const_locator).state) {
            .stored_const => |stored| try self.tasks.append(self.allocator, .{ .restore = .{ .source = .{ .module = owner.index }, .node = request.node orelse stored.node, .ty = worker.ret } }),
            .unimplemented => |unimplemented| {
                self.region = unimplemented.region;
                try self.crashValue(@import("../common.zig").unimplemented_declaration_crash, worker.ret);
            },
            // A provided data export is a closed constant, which
            // compile-time evaluation stores before any runtime lowering.
            .eval_template, .reserved => invariant("a static data request names a constant compile-time evaluation did not store"),
        }
        try self.run();
        try self.finishProc(entry, body_start, &.{}, ret_layout);
    }

    pub fn lowerRootWrapper(self: *Builder) Allocator.Error!void {
        const worker = self.currentWorker().*;
        const request = worker.root_request.?;
        self.entry_hole = try self.newHole();
        const entry = self.entry_hole;
        self.var_descs = &.{};
        self.dict_locals = &.{};
        const params = try self.allocator.alloc(LocalId, worker.params.len);
        defer self.allocator.free(params);
        for (params, worker.params) |*param, param_ty| param.* = try self.localOf(param_ty, .param);
        const ret_layout = try self.program.layouts.layoutOf(worker.ret);
        const body_start = try self.newHole();
        self.hole = body_start;

        const fn_ty = try self.program.store.intern(.{ .function = .{ .args = worker.params, .ret = worker.ret, .effectful = false } });
        // Null for a rejected binding: checking reported its error, and the
        // root crashes with the checked runtime error when called.
        const callee: ?Callee = if (request.procedure_template) |template|
            .{ .worker = try self.program.templateRefWorker(template) }
        else if (request.procedure_binding) |binding| switch (self.view.view.top_level_procedure_bindings.bindings.items[@backingInt(binding)].body) {
            .direct_template => |direct| .{ .worker = try self.program.templateRefWorker(Program.directTemplate(direct.template)) },
            // An exact procedure alias calls what it names.
            .callable_eval_template => |template| if (self.view.view.callable_eval_templates.templates[@backingInt(template)].forwarded_lookup != null)
                .{ .alias = try self.aliasCallee(self.module, template) }
            else
                .{ .callable_eval = .{ .module = self.module, .template = template } },
            .checked_error => null,
        } else if (request.procedure_use) |use|
            try self.procedureUseCallee(use)
        else
            invariantFmt("a {s} root names no procedure", .{@tagName(request.kind)});

        try self.tasks.append(self.allocator, .ret);
        if (callee) |target_callee| switch (target_callee) {
            .worker => |target| {
                const target_worker = self.program.worker(target).*;
                const formal = try self.program.store.intern(.{ .function = .{ .args = target_worker.params, .ret = target_worker.ret, .effectful = false } });
                const bound = try self.relateVariables(formal, fn_ty, target_worker.scheme_vars.len);
                defer self.allocator.free(bound);
                const hidden = try self.allocator.alloc(Word, bound.len + target_worker.dictionary_params.len + target_worker.literal_params);
                defer self.allocator.free(hidden);
                for (bound, hidden[0..bound.len]) |ty, *desc| desc.* = try self.descWord(ty);
                try self.derivedLiteralArgs(target, bound, hidden[bound.len + target_worker.dictionary_params.len ..]);
                if (target_worker.dictionary_params.len != 0) {
                    const span = request.root_evidence orelse invariant("a root whose target takes dictionaries has no root evidence");
                    const owner = self.program.moduleByKey(span.checked_module);
                    if (owner.index != self.module) invariant("a root's evidence was recorded by another module");
                    const evidence = owner.view.static_dispatch_plans.evidence_refs[span.span.start..][0..span.span.len];
                    const context = self.templateParamContext(target, bound);
                    for (target_worker.dictionary_params, hidden[bound.len..][0..target_worker.dictionary_params.len]) |param_index, *dict| {
                        if (param_index >= evidence.len) invariant("a root's evidence does not cover its target's dictionaries");
                        dict.* = try self.paramDictionary(evidence, param_index, context);
                    }
                }
                for (params) |param| try self.pushValue(param);
                try self.finishCall(target, hidden, bound, worker.params, try self.program.substitute(self.allocator, target_worker.ret, bound), worker.ret);
            },
            .alias => |alias| {
                const evidence: []const static_dispatch.CheckedEvidence = if (request.root_evidence) |span| blk: {
                    const owner = self.program.moduleByKey(span.checked_module);
                    if (owner.index != self.module) invariant("a root's evidence was recorded by another module");
                    break :blk owner.view.static_dispatch_plans.evidence_refs[span.span.start..][0..span.span.len];
                } else &.{};
                const instance = try self.aliasUseInstance(alias, &.{}, evidence, fn_ty, .literal);
                defer self.allocator.free(instance.hidden);
                for (params) |param| try self.pushValue(param);
                try self.finishCall(alias.target, instance.hidden, instance.bound, worker.params, instance.ret, worker.ret);
            },
            .callable_eval => |eval| {
                try self.tasks.append(self.allocator, .{ .finish_erased_call = .{ .fn_ty = fn_ty, .arg_count = @intCast(params.len), .result = worker.ret } });
                var index = params.len;
                while (index > 0) {
                    index -= 1;
                    try self.tasks.append(self.allocator, .{ .push_value = params[index] });
                }
                try self.tasks.append(self.allocator, .{ .restore_callable_eval = .{ .module = eval.module, .template = eval.template, .ty = fn_ty } });
            },
        } else try self.runtimeErrorValue(worker.ret);
        try self.run();
        try self.finishProc(entry, body_start, params, ret_layout);
    }

    /// A default expression's worker: the expression, in the scope of its
    /// own type's variables, whose descriptors are its parameters.
    pub fn lowerDefaultBody(self: *Builder, expr: ExprId) Allocator.Error!void {
        const worker = self.currentWorker().*;
        self.entry_hole = try self.newHole();
        const entry = self.entry_hole;
        const has_table = self.program.inputCount(self.worker_id) != 0;
        var table_arg: [1]LocalId = undefined;
        if (has_table) {
            table_arg[0] = try self.closedLocal(.opaque_ptr);
            try self.beginTable(table_arg[0], 0);
        }
        var ret_layout: layout.Idx = undefined;
        if (try self.closed(worker.ret)) {
            ret_layout = try self.program.layouts.layoutOf(worker.ret);
        } else {
            ret_layout = try self.program.result.layouts.dynamicLayout();
            self.result_local = try self.localOf(worker.ret, .result);
        }
        const body_start = try self.newHole();
        self.hole = body_start;
        try self.tasks.append(self.allocator, .ret);
        try self.tasks.append(self.allocator, .{ .own_expr = expr });
        try self.run();
        try self.finishProc(entry, body_start, if (has_table) &table_arg else &.{}, ret_layout);
    }

    /// The value of the `??` default `default` for a field of type
    /// `field_ty`: its declaring module's default expression, evaluated now
    /// at the instantiation the field's type gives its variables.
    pub fn defaultValue(self: *Builder, default: checked.CheckedFieldDefault, field_ty: TypeId) Allocator.Error!LocalId {
        return self.fieldDefaultValue(self.program.modules.fieldDefault(self.module, default), field_ty);
    }

    /// Materialize a default whose declaring module is already program-owned.
    pub fn fieldDefaultValue(self: *Builder, default: types.FieldDefault, field_ty: TypeId) Allocator.Error!LocalId {
        const default_bodies = self.program.module(default.module).bodies();
        try default_bodies.demandIndex();
        const expr = default_bodies.defaultExpr(default.expr_node) orelse
            invariant("a field default's expression was not archived");
        const worker_id = try self.program.defaultWorker(default.module, expr);
        const worker = self.program.worker(worker_id).*;
        const fn_ty = try self.program.store.intern(.{ .function = .{ .args = &.{}, .ret = field_ty, .effectful = false } });
        const bound = try self.boundTypesAt(worker_id, &.{}, fn_ty);
        defer self.allocator.free(bound);
        const args = try self.callArgs(&.{}, worker_id, try self.descWords(bound));
        defer self.allocator.free(args);
        const ret = try self.program.substitute(self.allocator, worker.ret, bound);
        const target = try self.localOf(ret, .frame);
        try self.emitLinear(.{ .assign_call = .{ .target = target, .proc = worker.proc, .args = try self.store.addLocalSpan(args), .next = undefined } });
        return try self.adaptValue(target, ret, field_ty);
    }

    pub fn lowerMethodEntry(self: *Builder, target: WorkerId) Allocator.Error!void {
        const worker = self.currentWorker().*;
        const target_worker = self.program.worker(target).*;
        // The entry captures its type table, whose inputs are the target's:
        // its descriptors, then its dictionaries, then its literal
        // arguments.
        const has_table = self.program.inputCount(self.worker_id) != 0;
        var start = try self.beginErasedWorker(if (has_table) &.{.opaque_ptr} else &.{});
        defer self.allocator.free(start.captures);
        if (has_table) try self.beginTable(start.captures[0], target_worker.dictionary_params.len);
        const inputs = try self.allInputWords();
        try self.runErasedBody(&start, worker.params, worker.ret, .{ .call = .{ .target = target, .inputs = inputs } });
    }

    /// A structural method over the one descriptor its dictionary captures.
    pub fn lowerStructuralMethod(self: *Builder, method: lower.StructuralMethod) Allocator.Error!void {
        const worker = self.currentWorker().*;
        var start = try self.beginErasedWorker(&.{.opaque_ptr});
        defer self.allocator.free(start.captures);
        try self.beginTable(start.captures[0], 0);
        try self.runErasedBody(&start, worker.params, worker.ret, .{ .structural = method });
    }

    /// A derived map over the tag union its first captured descriptor
    /// describes: the selected variant's payloads are read, its selected
    /// payload transformed, and the variant rebuilt under the output
    /// union's descriptor; any other variant is re-tagged by name, its
    /// payloads unchanged.
    pub fn lowerStructuralMap(self: *Builder, map: lower.StructuralMap) Allocator.Error!void {
        const worker = self.currentWorker().*;
        var start = try self.beginErasedWorker(&.{.opaque_ptr});
        defer self.allocator.free(start.captures);
        try self.beginTable(start.captures[0], 0);
        try self.runErasedBody(&start, worker.params, worker.ret, .{ .structural_map = map });
    }

    /// The two branches of a derived map over `input` (of the structural
    /// type parameter `input_ty`) with `transform`: each returns the
    /// worker's result of type `output_ty`.
    fn structuralMapBranches(self: *Builder, map: lower.StructuralMap, input: LocalId, transform: LocalId, input_ty: TypeId, output_ty: TypeId) Allocator.Error!void {
        const store = &self.program.store;
        const input_desc = try self.varDesc(0);
        const output_desc = try self.varDesc(1);
        const discriminant = try self.closedLocal(.u32);
        try self.emitDyn(discriminant, .{ .tag_discriminant = .{ .base = input, .desc = input_desc } });
        const selected = try self.indexLocal(try self.tagIndexIn(try self.inputWord(0), map.name));
        const is_selected = try self.closedLocal(.bool);
        try self.emitLowLevel(is_selected, .num_is_eq, &.{ discriminant, selected });
        const holes = try self.emitBoolSwitch(is_selected);

        self.hole = holes.then;
        const payloads = try self.allocator.alloc(LocalId, map.arity);
        defer self.allocator.free(payloads);
        for (payloads, 0..) |*payload, index| {
            const payload_ty = try store.variable(@intCast(2 + index));
            const view = try self.localOf(payload_ty, .view);
            try self.emitDyn(view, .{ .tag_payload_addr = .{ .base = input, .desc = input_desc, .payload_index = @intCast(index) } });
            const copy = try self.localOf(payload_ty, .frame);
            try self.emitDyn(copy, .{ .copy = .{ .src = view, .desc = try self.varDesc(2 + index) } });
            payload.* = copy;
        }
        const transform_ty = try store.intern(.{ .function = .{
            .args = &.{try store.variable(2 + map.payload_index)},
            .ret = try store.variable(2 + map.arity),
            .effectful = map.effectful,
        } });
        payloads[map.payload_index] = try self.emitErasedCall(transform, transform_ty, &.{payloads[map.payload_index]}, try store.variable(2 + map.arity));
        const mapped = try self.localOf(output_ty, .frame);
        try self.emitDyn(mapped, .{ .tag = .{ .desc = output_desc, .discriminant = try self.tagIndexIn(try self.inputWord(1), map.name), .payloads = try self.store.addLocalSpan(payloads) } });
        try self.pushValue(mapped);
        try self.step(.ret);

        self.hole = holes.otherwise;
        const unchanged = try self.localOf(output_ty, .frame);
        try self.emitDyn(unchanged, .{ .adapt = .{ .src = input, .plan = try self.adaptPlanIn(try self.descWord(input_ty), try self.inputWord(1)) } });
        try self.pushValue(unchanged);
        try self.step(.ret);
    }

    /// A crashing dictionary method: it crashes as the dispatch it stands
    /// for would.
    pub fn lowerCrashDictionary(self: *Builder, kind: lower.CrashDictionary) Allocator.Error!void {
        const start = try self.beginErasedWorker(&.{});
        defer self.allocator.free(start.captures);
        self.var_descs = &.{};
        const body_start = try self.newHole();
        self.hole = body_start;
        try self.emitDispatchCrash(kind);
        try self.finishErasedProc(start.entry, body_start, &.{}, .zst, start.capture_ptr);
    }

    /// The crash of a dispatch that cannot run: a rejected one is checked-
    /// error code, an unreachable one an ordinary crash.
    pub fn emitDispatchCrash(self: *Builder, kind: lower.CrashDictionary) Allocator.Error!void {
        switch (kind) {
            .checked_error => {
                const literal = try self.store.insertString("method dispatch failed to check");
                try self.emitTerminal(.{ .crash = .{ .msg = .{ .literal = literal }, .checked_error = true } });
            },
            .unreachable_value => {
                const literal = try self.store.insertString("dispatch on a value that can never exist");
                try self.emitTerminal(.{ .crash = .{ .msg = .{ .literal = literal } } });
            },
        }
    }

    pub const ErasedBody = union(enum) {
        call: struct { target: WorkerId, inputs: []const Word },
        structural: lower.StructuralMethod,
        structural_map: lower.StructuralMap,
        /// A call of the function value of a computed callable binding.
        callable_eval: lower.CallableEvalBinding,
    };

    /// The erased entry of a method bound to a computed callable: it
    /// restores the binding's function value and calls it with its
    /// arguments (`WorkerKey.callable_eval_entry`).
    pub fn lowerCallableEvalEntry(self: *Builder, binding: lower.CallableEvalBinding) Allocator.Error!void {
        const worker = self.currentWorker().*;
        var start = try self.beginErasedWorker(&.{});
        defer self.allocator.free(start.captures);
        self.var_descs = &.{};
        try self.runErasedBody(&start, worker.params, worker.ret, .{ .callable_eval = binding });
    }

    pub fn runErasedBody(self: *Builder, start: *ErasedStart, params: []const TypeId, ret: TypeId, body: ErasedBody) Allocator.Error!void {
        var args: std.ArrayList(LocalId) = .empty;
        defer args.deinit(self.allocator);
        var record: ?LocalId = null;
        if (params.len != 0) {
            record = try self.erasedParamLocal(params);
            try args.append(self.allocator, record.?);
        }
        var ret_layout: layout.Idx = undefined;
        if (try self.closed(ret)) {
            ret_layout = try self.program.layouts.layoutOf(ret);
        } else {
            ret_layout = try self.program.result.layouts.dynamicLayout();
            self.result_local = try self.localOf(ret, .result);
        }
        const body_start = try self.newHole();
        self.hole = body_start;
        const values = try self.allocator.alloc(LocalId, params.len);
        defer self.allocator.free(values);
        for (values, 0..) |*value, index| value.* = try self.erasedParamValue(record.?, params, @intCast(index));
        const result: ?LocalId = switch (body) {
            .call => |call| blk: {
                const all = try self.callArgs(values, call.target, call.inputs);
                defer self.allocator.free(all);
                const target = try self.localOf(ret, .frame);
                try self.emitLinear(.{ .assign_call = .{ .target = target, .proc = self.program.worker(call.target).proc, .args = try self.store.addLocalSpan(all), .next = undefined } });
                break :blk target;
            },
            .structural => |method| blk: {
                const target = try self.localOf(ret, .frame);
                switch (method) {
                    .is_eq => try self.emitDyn(target, .{ .eq = .{ .lhs = values[0], .rhs = values[1], .desc = try self.varDesc(0) } }),
                    .to_hash => try self.emitDyn(target, .{ .hash = .{ .hasher = values[1], .value = values[0], .desc = try self.varDesc(0) } }),
                    .to_inspect => try self.emitDyn(target, .{ .inspect = .{ .value = values[0], .desc = try self.varDesc(0), .names = try self.namesIn(try self.inputWord(0)) } }),
                }
                break :blk target;
            },
            .callable_eval => |binding| blk: {
                const fn_ty = try self.program.store.intern(.{ .function = .{ .args = params, .ret = ret, .effectful = false } });
                const value = try self.callableEvalValueAt(binding, fn_ty);
                break :blk try self.emitErasedCall(value, fn_ty, values, ret);
            },
            .structural_map => |map| blk: {
                try self.structuralMapBranches(map, values[0], values[1], params[0], ret);
                break :blk null;
            },
        };
        if (result) |value| {
            try self.pushValue(value);
            try self.step(.ret);
        }
        try self.finishErasedProc(start.entry, body_start, args.items, ret_layout, start.capture_ptr);
    }

    /// The closed layout of an erased worker's capture record: the captured
    /// values, then `word_count` pointer-sized words (its type table), in
    /// that order of checked field indices.
    pub fn captureLayout(self: *Builder, capture_types: []const TypeId, word_count: usize) Allocator.Error!layout.Idx {
        const fields = try self.allocator.alloc(layout.StructField, capture_types.len + word_count);
        defer self.allocator.free(fields);
        for (fields, 0..) |*field, index| {
            field.* = .{
                .index = @intCast(index),
                .layout = if (index < capture_types.len) try self.program.layouts.layoutOf(capture_types[index]) else .opaque_ptr,
            };
        }
        return try self.program.result.layouts.putStructFields(fields);
    }

    /// An erased entry owns one complete argument block. A sole argument
    /// is already that block: use its own descriptor and ownership unit.
    pub fn erasedParamLocal(self: *Builder, params: []const TypeId) Allocator.Error!LocalId {
        std.debug.assert(params.len != 0);
        if (params.len == 1) return self.localOf(params[0], .param);
        return self.argsRecordLocal(params, .param);
    }

    /// Bind the source parameter directly when it is the entire block.
    pub fn erasedParamValue(self: *Builder, block: LocalId, params: []const TypeId, index: u32) Allocator.Error!LocalId {
        std.debug.assert(index < params.len);
        if (params.len == 1) return block;
        return self.argsRecordField(block, index, params[index]);
    }

    /// The packed erased-call argument record of `params`: a declared-order
    /// struct when every parameter type is closed, a dynamic record read
    /// through its argument record descriptor (`argsDesc`) otherwise.
    pub fn argsRecordLocal(self: *Builder, params: []const TypeId, storage: LIR.DynStorage) Allocator.Error!LocalId {
        if (try self.allClosed(params)) return self.closedLocal(try self.argsRecordLayout(params));
        const desc = try self.argsRecordDesc(params);
        const product = try self.productShapeFields(0, params);
        return self.addLocal(.{ .layout_idx = try self.program.result.layouts.dynamicLayout(), .dyn = .{ .desc = desc, .storage = storage }, .product = product });
    }

    pub fn allClosed(self: *Builder, tys: []const TypeId) Allocator.Error!bool {
        for (tys) |ty| {
            if (!try self.closed(ty)) return false;
        }
        return true;
    }

    pub fn argsRecordLayout(self: *Builder, params: []const TypeId) Allocator.Error!layout.Idx {
        const fields = try self.allocator.alloc(layout.StructField, params.len);
        defer self.allocator.free(fields);
        for (fields, params, 0..) |*field, param, index| field.* = .{ .index = @intCast(index), .layout = try self.program.layouts.layoutOf(param) };
        return try self.program.result.layouts.putNominalStructFields(fields);
    }

    pub fn argsRecordDesc(self: *Builder, params: []const TypeId) Allocator.Error!LocalId {
        return try self.argsDesc(0, try self.descWords(params));
    }

    /// The local holding the descriptor of an erased call's packed argument
    /// record: `leading` pointer-sized words, then values of the types
    /// whose descriptors `elems` are (`Tables.Recipe.args_desc`).
    fn argsDesc(self: *Builder, leading: u32, elems: []const Word) Allocator.Error!LocalId {
        return try self.materialize(.{ .recipe = try self.recipes.intern(.{ .args_desc = .{ .leading = leading, .elems = elems } }) });
    }

    /// Copy argument `index` into independent storage. The ABI transfers the
    /// contents, not ownership of the caller's argument buffer. ARC decides
    /// whether this complete field moves, is retained, or remains borrowed.
    pub fn argsRecordField(self: *Builder, record: LocalId, index: u32, param_ty: TypeId) Allocator.Error!LocalId {
        if (self.store.localDyn(record) != null and self.store.getLocal(record).product == .none)
            invariant("a complete dynamic argument record omitted its product ownership shape");
        const local = try self.localOf(param_ty, .frame);
        if (self.store.getLocal(record).layout_idx == .zst) {
            // A record of zero-sized arguments has no storage to read.
            try self.emitLinear(.{ .assign_struct = .{ .target = local, .fields = LIR.LocalSpan.empty(), .next = undefined } });
            return local;
        }
        try self.emitLinear(.{ .assign_ref = .{ .target = local, .op = LIR.RefOp.field(record, index), .next = undefined } });
        return local;
    }

    pub fn run(self: *Builder) Allocator.Error!void {
        while (self.tasks.pop()) |task| try self.step(task);
        if (self.values.items.len != 0) invariant("body lowering left values on its value stack");
    }

    pub fn push(self: *Builder, task: Task) Allocator.Error!void {
        try self.tasks.append(self.allocator, task);
    }

    pub fn popValue(self: *Builder) LocalId {
        return self.values.pop() orelse invariant("body lowering popped an empty value stack");
    }

    pub fn pushValue(self: *Builder, local: LocalId) Allocator.Error!void {
        try self.values.append(self.allocator, local);
    }

    pub fn step(self: *Builder, task: Task) Allocator.Error!void {
        switch (task) {
            // Code after a terminal is unreachable: nothing of it is
            // lowered, and an expression there stands for a value no
            // execution produces.
            .expr => |expr| if (self.hole == null) {
                const checked_ty = self.bodies().valueType(expr);
                try self.pushValue(if (try self.program.hasError(self.module, checked_ty)) try self.closedLocal(.zst) else try self.localOf(try self.valueType(expr), .frame));
            } else try self.stepExpr(expr),
            .own_expr => |expr| if (self.hole == null) {
                const checked_ty = self.bodies().exprType(expr);
                try self.pushValue(if (try self.program.hasError(self.module, checked_ty)) try self.closedLocal(.zst) else try self.localOf(try self.exprType(expr), .frame));
            } else try self.stepOwnExpr(expr),
            .root_body => |root| try self.lowerRootBody(root),
            .root_pattern_match => |root| try self.rootPatternMatch(root),
            .literal_pattern_guard => |pattern| if (self.hole == null) {
                const checked_ty = self.bodies().literalPatternGuard(pattern).?.equality_ty;
                try self.pushValue(if (try self.program.hasError(self.module, checked_ty)) try self.closedLocal(.zst) else try self.localOf(try self.convert(checked_ty), .frame));
            } else {
                try self.push(.{ .restore_region = self.region });
                try self.lowerLiteralPatternGuard(pattern);
            },
            .literal_pattern_value => |pattern| if (self.hole == null) {
                try self.pushValue(try self.literalPatternDeadValue(pattern));
            } else {
                try self.pushValue(self.binders.get(pattern) orelse invariant("a custom literal pattern's guard read a value the pattern never bound"));
            },
            .literal_pattern_conversion => |pattern| if (self.hole == null) {
                try self.pushValue(try self.literalPatternDeadValue(pattern));
            } else {
                try self.push(.{ .restore_region = self.region });
                self.region = self.nodeRegion(@backingInt(pattern));
                const conversion = self.bodies().literalConversion(@backingInt(pattern));
                switch (conversion.literal) {
                    .numeral => |literal| try self.lowerNumeral(.{ .pattern = pattern }, literal, conversion.plan, conversion.conversion_root),
                    .quote => |text| try self.lowerQuote(.{ .pattern = pattern }, text, conversion.plan, conversion.conversion_root),
                }
            },
            .stmt => |stmt| if (self.hole != null) try self.stepStmt(stmt),
            .bind => |bind| {
                const value = self.popValue();
                // A destructure that can miss has the checked site of the
                // pending diagnostic checking left it.
                const failure = if (self.view.view.exhaustiveness_sites.lookupByDestructurePattern(bind.pattern)) |site| try self.comptimeFailure(site) else null;
                const miss = try self.store.addCFStmt(Match.ComptimeFailure.terminal(failure), self.origin(.lowering_glue));
                try self.testPattern(bind.pattern, value, bind.ty, miss);
            },
            .discard => _ = self.popValue(),
            .runtime_error => |ty| try self.runtimeErrorValue(ty),
            .retag => |retag| {
                const value = self.popValue();
                try self.pushValue(try self.retagValue(value, retag.from, retag.to));
            },
            .diverged => |ty| {
                if (self.hole != null) try self.emitTerminal(.runtime_error);
                try self.pushValue(if (ty) |value_ty| try self.localOf(value_ty, .frame) else try self.closedLocal(.zst));
            },
            .dispatch_crash => |crash| {
                try self.emitDispatchCrash(crash.kind);
                try self.pushValue(try self.localOf(crash.result, .frame));
            },
            .push_value => |local| try self.pushValue(local),
            .unreachable_value => |ty| try self.pushValue(try self.localOf(ty, .frame)),
            .unreachable_or_unit => try self.pushValue(try self.unitValue()),
            .segments_arg => |arg| {
                const interpolation = self.bodies().exprData(arg.expr).interpolation;
                const elems = try self.allocator.alloc(LocalId, interpolation.segmentCount());
                defer self.allocator.free(elems);
                for (elems, 0..) |*elem, index| {
                    elem.* = switch (self.bodies().exprData(interpolation.segment(index))) {
                        .str_segment => |literal| try self.strLiteral(self.bodies().text(literal)),
                        .numeral, .str, .bytes_literal, .lookup_local, .lookup_external, .lookup_required, .list, .empty_list, .tuple, .match_, .if_, .call, .record, .empty_record, .block, .tag, .nominal, .zero_argument_tag, .closure, .lambda, .binop, .unary_minus, .field_access, .dispatch_call, .interpolation, .structural_eq, .structural_hash, .method_eq, .type_dispatch_call, .tuple_access, .runtime_error, .crash, .dbg, .expect_err, .expect, .ellipsis, .anno_only, .break_, .return_, .for_, .hosted_lambda, .run_low_level => invariant("an interpolation segment is not a string segment"),
                    };
                }
                const list = try self.localOf(arg.ty, .frame);
                try self.emitLinear(.{ .assign_list = .{ .target = list, .elems = try self.store.addLocalSpan(elems), .next = undefined } });
                try self.pushValue(list);
            },
            .finish_interpolation => |expr| try self.finishInterpolation(expr),
            .finish_literal_conversion => |conversion| try self.finishLiteralConversion(conversion),
            .finish_interpolation_call => |call| {
                const values = try self.popValues(call.count);
                defer self.allocator.free(values);
                // The iterator the assembler receives: `iter` on the list of
                // the values. Every closure Boxy builds is boxed, so an
                // iterator over one list costs one allocation and a box per
                // step, where appending each value onto an empty list's
                // iterator would nest a boxed closure per value per step.
                const plan = self.view.view.static_dispatch_plans.iterator_for_plans[@backingInt(call.plan)];
                const iterator_ty = try self.convert(plan.iterator_ty);
                const list = try self.buildList(try self.convert(plan.interpolationSite().seed_list_ty), values);
                const iterator = try self.emitDispatch(plan.iter.resolution, plan.iter.callable_ty, &.{list}, iterator_ty);
                try self.pushValue(try self.emitErasedCall(call.assembler, call.assembler_ty, &.{iterator}, call.result));
            },
            .finish_str_from_parts => |call| {
                const values = try self.popValues(call.count);
                defer self.allocator.free(values);
                const interpolation = self.bodies().exprData(call.expr).interpolation;
                var parts: std.ArrayList(LocalId) = .empty;
                defer parts.deinit(self.allocator);
                for (0..interpolation.segmentCount()) |i| {
                    const segment = interpolation.segment(i);
                    if (!self.bodies().isEmptyStrSegment(segment)) {
                        const text = switch (self.bodies().exprData(segment)) {
                            .str_segment => |literal| self.bodies().text(literal),
                            .numeral, .str, .bytes_literal, .lookup_local, .lookup_external, .lookup_required, .list, .empty_list, .tuple, .match_, .if_, .call, .record, .empty_record, .block, .tag, .nominal, .zero_argument_tag, .closure, .lambda, .binop, .unary_minus, .field_access, .dispatch_call, .interpolation, .structural_eq, .structural_hash, .method_eq, .type_dispatch_call, .tuple_access, .runtime_error, .crash, .dbg, .expect_err, .expect, .ellipsis, .anno_only, .break_, .return_, .for_, .hosted_lambda, .run_low_level => invariant("an interpolation segment is not a string segment"),
                        };
                        try parts.append(self.allocator, try self.strLiteral(text));
                    }
                    if (i < values.len) try parts.append(self.allocator, values[i]);
                }
                switch (parts.items.len) {
                    0 => try self.pushValue(try self.strLiteral("")),
                    1 => try self.pushValue(parts.items[0]),
                    else => {
                        const target = try self.localOf(call.result, .frame);
                        try self.emitLowLevel(target, .str_from_parts, parts.items);
                        try self.pushValue(target);
                    },
                }
            },
            .finish_local_call => |call| try self.finishLocalCall(call.closure, call.words, call.params, call.ret, call.arg_types, call.result),
            .finish_hash => |hash| {
                const hasher = self.popValue();
                const value = self.popValue();
                const target = try self.localOf(hash.result, .frame);
                try self.emitDyn(target, .{ .hash = .{ .hasher = hasher, .value = value, .desc = try self.descOf(hash.value) } });
                try self.pushValue(target);
            },
            .push_value_quote => |node| try self.pushValue(try self.textLiteral(self.bodies().quoteText(node))),

            .resume_at => |hole| self.hole = hole,
            .ret => {
                const value = self.popValue();
                // Every branch of a construct in tail position returned.
                if (self.hole == null) return;
                if (self.result_local) |result| {
                    // An immediately returned call or constructor writes
                    // straight into the caller's result storage.
                    if (!try self.retargetLastProducer(value, result)) {
                        try self.emitDyn(result, .{ .copy = .{ .src = value, .desc = self.store.localDyn(result).?.desc } });
                    }
                    try self.emitTerminal(.{ .ret = .{ .value = result } });
                } else {
                    try self.emitTerminal(.{ .ret = .{ .value = value } });
                }
            },
            .jump => |join| try self.emitJump(join, self.popValue()),
            .jump_without_value => |join| try self.emitJump(join, null),
            .restore_merge => |merge| try self.restoreMerge(merge),
            .enter_merge => |merge| try self.enterMerge(merge),
            .loop_test => |loop| try self.loopTest(loop.body, loop.head, loop.exit),
            .fill_recursive_slots => |pattern| try self.fillRecursiveSlots(pattern),
            .rejected_for => |plan_id| {
                _ = self.popValue();
                const plan = self.view.view.static_dispatch_plans.iterator_for_plans[@backingInt(plan_id)];
                switch (plan.iter.resolution) {
                    .checked_error => try self.emitDispatchCrash(.checked_error),
                    .@"unreachable" => try self.emitDispatchCrash(.unreachable_value),
                    .direct_pending, .direct_closed, .direct_parametric, .evidence_dependent, .structural => {
                        const literal = try self.store.insertString("runtime error");
                        try self.emitTerminal(.{ .crash = .{ .msg = .{ .literal = literal }, .checked_error = true } });
                    },
                }
                try self.pushValue(try self.closedLocal(.zst));
            },
            .leave_alias_scope => {
                const scope = self.alias_scopes.pop().?;
                self.env = scope.saved_env;
            },
            .restore_scope => |saved| {
                self.module = saved.module;
                self.view = saved.view;
                self.env = saved.env;
                self.foreign_scopes = saved.foreign_scopes;
                self.thunk_frames = saved.thunk_frames;
                self.alias_scopes.shrinkRetainingCapacity(saved.alias_scopes);
            },
            .end_loop => {
                // A loop nothing leaves never reaches what follows it.
                const loop = self.loops.pop().?;
                if (!loop.reached) try self.emitTerminal(.runtime_error);
            },
            .for_start => |for_| try self.forStart(for_.loop, for_.pattern, for_.body, for_.plan),
            .numeral_arg => |arg| try self.pushValue(try self.numeralArgument(arg.literal, arg.ty)),
            .finish_call => |call| {
                defer self.allocator.free(call.hidden);
                try self.finishCall(call.callee, call.hidden, call.bound, call.arg_types, call.ret, call.result);
            },
            .finish_low_level => |ll| try self.finishLowLevel(ll.op, ll.arg_count, ll.result, ll.arg_types),
            .restore_region => |region| self.region = region,
            .finish_record => |expr| try self.finishRecord(expr),
            .restore => |restore| try self.restoreStep(restore.source, restore.node, restore.ty, restore.root),
            .restore_callable_eval => |eval| {
                const owner = self.program.module(eval.module);
                const template = owner.view.callable_eval_templates.templates[@backingInt(eval.template)];
                const root = owner.view.compile_time_roots.root(template.root);
                if (root.boxy_value) |stored| return try self.push(.{ .restore = .{ .source = .{ .module = eval.module }, .node = stored.node, .ty = eval.ty } });
                const payload = root.payloadFor(.boxy);
                switch (payload) {
                    .fn_value => invariant("a Boxy callable root view exposed an LSS function"),
                    // Evaluation stored a value that is not a function: the
                    // crash a rejected binding is.
                    .const_node => |node| try self.push(.{ .restore = .{ .source = .{ .module = eval.module }, .node = node, .ty = eval.ty } }),
                    .pending, .unevaluated, .runtime => {
                        // An ineligible root is a checked instantiation
                        // template, not a value waiting for an evaluator.
                        // Its wrapper receives this use's type arguments.
                        // Eligible pending roots still require their producer.
                        if (payload == .pending and root.request_eligibility != .ineligible) {
                            return try self.pendingValueRead(eval.module, template.root, eval.ty);
                        }
                        const wrapper = self.program.wrapper_by_root.get(.{ .module = eval.module, .root = template.root }) orelse
                            invariant("a callable-eval binding's root has no entry wrapper");
                        const callee = try self.program.templateRefWorker(wrapper);
                        const callee_worker = self.program.worker(callee).*;
                        if (callee_worker.params.len != 0 or callee_worker.dictionary_params.len != 0) invariant("a callable-eval entry wrapper takes arguments");
                        const bound = try self.relateVariables(callee_worker.ret, eval.ty, callee_worker.scheme_vars.len);
                        defer self.allocator.free(bound);
                        const hidden = try self.allocator.alloc(Word, bound.len);
                        defer self.allocator.free(hidden);
                        for (bound, hidden) |ty, *desc| desc.* = try self.descWord(ty);
                        try self.finishCall(callee, hidden, bound, &.{}, try self.program.substitute(self.allocator, callee_worker.ret, bound), eval.ty);
                    },
                    .discarded, .expect => invariant("a callable-eval binding's root holds a value, which no expect or discarded root does"),
                }
            },
            .finish_stored_fn => |stored| {
                const values = try self.popValues(@intCast(stored.shape.values.len));
                defer self.allocator.free(values);
                if (stored.shape.composed) {
                    if (values.len != 0) invariant("a stored dictionary captures values");
                    try self.pushValue(try self.materialize(try self.composeDictionary(stored.worker, stored.inputs)));
                } else {
                    try self.pushValue(try self.staticClosure(stored.worker, values, stored.capture_types, stored.inputs, !try self.allClosed(stored.shape.values), stored.capture_descs));
                }
            },
            .finish_restore_closure => |closure| {
                const values = try self.popValues(closure.count);
                defer self.allocator.free(values);
                const worker = self.program.worker(closure.worker).*;
                const inputs: []const Word = if (self.program.perCall(closure.worker)) &.{} else try std.mem.concat(self.program.arena.allocator(), Word, &.{ closure.descs, closure.dicts });
                const packed_closure = try self.staticClosure(closure.worker, values, closure.capture_types, inputs, !try self.allClosed(worker.capture_types), null);
                try self.pushValue(try self.adaptValue(packed_closure, closure.worker_ty, closure.ty));
            },
            .finish_restore_struct => |restore| {
                const values = try self.popValues(restore.count);
                defer self.allocator.free(values);
                try self.pushValue(try self.staticPartsLocal(try self.program.layouts.layoutOf(restore.ty), .product, values));
            },
            .finish_restore_tag => |restore| {
                const values = try self.popValues(restore.count);
                defer self.allocator.free(values);
                const structural = try self.structuralType(restore.ty);
                const tags = self.program.store.tagSpan(self.program.store.get(structural).tag_union.tags);
                const variant = for (tags, 0..) |tag, index| {
                    if (tag.name == restore.name) break index;
                } else invariant("a stored tag has no declared variant");
                try self.pushValue(try self.staticPartsLocal(try self.program.layouts.layoutOf(restore.ty), .{ .tag = @intCast(variant) }, values));
            },
            .finish_static_list => |value| try self.finishStaticParts(value.ty, value.count, .{ .list = value.capacity }),
            .finish_static_box => |value| try self.finishStaticParts(value.ty, 1, if (value.sequence_step_offset) |offset| .{ .boxed_sequence = offset } else .box),
            .remember_static_value => |key| {
                const value = self.popValue();
                try self.program.stored_value_slots.put(self.allocator, key, try self.constantSlot(value));
                try self.pushValue(value);
            },
            .finish_field_access => |expr| try self.finishFieldAccess(expr),
            .finish_discriminant_eq => |eq| try self.finishDiscriminantEq(eq.value_ty, eq.tag, eq.negated),
            .finish_tuple_access => |expr| {
                const access = self.bodies().exprData(expr).tuple_access;
                const tuple = self.popValue();
                const tuple_ty = try self.structuralType(try self.valueType(access.tuple));
                const elems = switch (self.program.store.get(tuple_ty)) {
                    .tuple => |span| self.program.store.span(span),
                    .primitive, .empty_record, .record, .tag_union, .list, .box, .nominal, .function, .variable => invariant("a tuple access's receiver is not a tuple"),
                };
                try self.pushValue(try self.readField(tuple_ty, tuple, access.elem_index, elems[access.elem_index]));
            },
            .finish_expect_err => |fail| {
                // A `?` inside an `expect` that evaluated an `Err`: the
                // expect fails with the rendered value.
                const value = self.popValue();
                const expr = self.bodies().expr(fail.expr);
                const rendered = try self.closedLocal(.str);
                try self.emitDyn(rendered, .{ .inspect = .{ .value = value, .desc = try self.descOf(fail.value), .names = try self.namesOf(fail.value) } });
                const prefix_text = try std.fmt.allocPrint(self.allocator, "The `?` operator in `{s}` evaluated an `Err` inside an `expect`. The value was: Err(", .{self.bodies().text(expr.data.expect_err.snippet)});
                defer self.allocator.free(prefix_text);
                const with_value = try self.closedLocal(.str);
                try self.emitLowLevel(with_value, .str_append, &.{ try self.strLiteral(prefix_text), rendered });
                const message = try self.closedLocal(.str);
                try self.emitLowLevel(message, .str_append, &.{ with_value, try self.strLiteral(")") });
                try self.emitTerminal(.{ .expect_err = .{ .message = message, .region = expr.source_region } });
                try self.pushValue(try self.localOf(try self.exprType(fail.expr), .frame));
            },
            .finish_dbg => |ty| {
                const value = self.popValue();
                const message = try self.closedLocal(.str);
                try self.emitDyn(message, .{ .inspect = .{ .value = value, .desc = try self.descOf(ty), .names = try self.namesOf(ty) } });
                try self.emitLinear(.{ .debug = .{ .message = message, .next = undefined } });
                // `dbg` evaluates to `{}`.
                try self.pushValue(try self.unitValue());
            },
            .finish_erased_call => |call| try self.finishErasedCall(call.fn_ty, call.arg_count, call.result),
            .finish_tuple => |expr| try self.finishTuple(expr),
            .finish_tag => |expr| try self.finishTag(expr, null),
            .finish_tag_at => |at| try self.finishTag(at.expr, at.ty),
            .finish_list => |expr| try self.finishList(expr),
            .finish_str => |str| try self.finishStr(str.count),
            .finish_eq => |eq| try self.finishEq(eq.negated, eq.operand),
            .finish_expect => {
                const cond = self.popValue();
                try self.emitLinear(.{ .expect = .{ .condition = cond, .site = try self.expectSite(), .next = undefined } });
                try self.pushValue(try self.unitValue());
            },
            .if_test => |test_| try self.ifTest(test_.expr, test_.index, test_.join, test_.merge),
            .match_start => |expr| try self.matchStart(expr),
            .short_circuit => |sc| try self.shortCircuit(sc.is_and, sc.rhs, sc.join),
            .match_advance => |advance| try self.matchAdvance(advance.state, advance.input),
            .match_guard_done => |done| {
                const value = self.popValue();
                try self.fill(try self.currentHole(), .{ .assign_ref = .{ .target = done.cond, .op = LIR.RefOp.local(value), .next = done.next } }, .source);
                self.hole = null;
            },
        }
    }

    pub fn popValues(self: *Builder, count: u32) Allocator.Error![]LocalId {
        const result = try self.allocator.alloc(LocalId, count);
        var index = count;
        while (index > 0) {
            index -= 1;
            result[index] = self.popValue();
        }
        return result;
    }

    pub fn expectSite(self: *Builder) Allocator.Error!?LIR.ExpectSiteId {
        if (!self.program.observe_expects) return null;
        return try self.program.result.addExpectSite(self.program.sourceLoc(self.module, self.region), self.region);
    }

    /// The `{}` value.
    pub fn unitValue(self: *Builder) Allocator.Error!LocalId {
        const local = try self.closedLocal(.zst);
        try self.emitLinear(.{ .assign_struct = .{ .target = local, .fields = LIR.LocalSpan.empty(), .next = undefined } });
        return local;
    }

    pub fn boolValue(self: *Builder, value: bool) Allocator.Error!LocalId {
        const local = try self.closedLocal(.bool);
        const discriminant = @import("../common.zig").boolDiscriminant(value);
        try self.emitLinear(.{ .assign_tag = .{ .target = local, .variant_index = discriminant, .discriminant = discriminant, .payload = null, .next = undefined } });
        return local;
    }

    pub fn crashValue(self: *Builder, message: []const u8, ty: TypeId) Allocator.Error!void {
        try self.pushValue(try self.crashLocal(message, ty));
    }

    /// Crash with `message`; the returned local of `ty` stands for the value
    /// no execution reaches.
    pub fn crashLocal(self: *Builder, message: []const u8, ty: TypeId) Allocator.Error!LocalId {
        const literal = try self.store.insertString(message);
        try self.emitTerminal(.{ .crash = .{ .msg = .{ .literal = literal } } });
        return try self.localOf(ty, .frame);
    }

    /// Lower `expr` now, at the current statement position, and return its
    /// value: the tasks it pushes run before this returns.
    pub fn lowerNow(self: *Builder, task: Task) Allocator.Error!LocalId {
        const base_tasks = self.tasks.items.len;
        try self.push(task);
        while (self.tasks.items.len > base_tasks) try self.step(self.tasks.pop().?);
        return self.popValue();
    }

    // -- Expressions --

    /// The stored value of the hoisted root `expr_id` is, when this body
    /// reads it instead of computing it: not the root this body computes,
    /// not a root whose evaluation failed (it runs in place), and not a
    /// guarded binding's root, whose declaration keeps its right-hand side.
    fn storedHoisted(self: *Builder, expr_id: ExprId) Allocator.Error!?HoistedRead {
        const entry = self.view.view.hoisted_constants.lookupByExpr(expr_id) orelse return null;
        if (self.own_root) |own| if (own == entry.root) return null;
        const root = self.view.view.compile_time_roots.root(entry.root);
        if (root.boxy_value) |stored| return .{ .stored = stored };
        if (root.payloadFor(.boxy) == .pending and self.view.view.const_templates.get(entry.const_ref).state == .stored_const) return .{ .pending = entry.root };
        if (root.payloadFor(.boxy) == .runtime) return null;
        if (entry.pattern != null and root.guarded) return null;
        return switch (self.view.view.const_templates.get(entry.const_ref).state) {
            .stored_const => |stored| .{ .stored = stored },
            // A value this compile-time program evaluates is read from its
            // slot; a polymorphic hoisted value computes at its instance, as
            // its entry procedure would.
            .eval_template => if (try self.program.readsRootSlot(self.module, entry.root)) .{ .pending = entry.root } else null,
            // A compile-time evaluator reads a value its own level has not
            // completed only as a member of a group that reads itself.
            .reserved => if (self.program.input.compile_time) .{ .pending = entry.root } else invariant("a hoisted root reached runtime lowering without a stored value or an entry procedure"),
            .unimplemented => invariant("a hoisted root reached runtime lowering without a stored value or an entry procedure"),
        };
    }

    /// The compile-time root of a hoisted constant of this module, or null
    /// for a constant of another module or one no root computes.
    fn hoistedRootOfConstRef(self: *Builder, const_ref: checked.ConstRef) ?checked.ComptimeRootId {
        if (!std.mem.eql(u8, &const_ref.artifact.bytes, &self.view.key.bytes)) return null;
        return switch (const_ref.owner) {
            .hoisted_root => |hoisted| hoisted.root,
            .top_level_binding => |top_level| self.view.view.compile_time_roots.lookupIdByPattern(top_level.pattern),
        };
    }

    /// What a body reads for a hoisted root: its stored value, or, under a
    /// compile-time evaluator, a root the evaluator has not completed.
    const HoistedRead = union(enum) { stored: checked.StoredConstTemplate, pending: checked.ComptimeRootId };

    /// The read of a compile-time value the evaluator has not completed when
    /// the body is lowered: a root of the same compile-time program, read
    /// from its compile-time value slot (`Program.rootValueSlot`). The
    /// evaluator fills the slot when the root completes; until then a read
    /// fails as the recursion that evaluating the value in place would end
    /// in, and the evaluator reruns the reading root once the value
    /// completes (design.md "Compile-Time Evaluation Strategy").
    fn pendingValueRead(self: *Builder, module_index: ModuleIndex, root: checked.ComptimeRootId, ty: TypeId) Allocator.Error!void {
        if (!self.program.input.compile_time) invariant("a runtime body reads a root without its completed Boxy representation");
        if (self.program.evaluatesRoot(module_index, root)) {
            // The slot holds the value at the root's own type. A read in
            // another module is checked at that module's own copy of the
            // type, which can store the value differently (a record row
            // lifted into a padded nominal), so it converts the value as a
            // read of a stored value does (`pushStoredRestore`).
            const own = try self.program.rootValueType(module_index, root);
            if (!try self.closed(own)) invariant("a compile-time root's value has an open type");
            const slot = try self.program.rootValueSlot(module_index, root);
            const local = try self.closedLocal(try self.program.layouts.layoutOf(own));
            try self.emitLinear(.{ .assign_literal = .{ .target = local, .value = .{ .static_data = .{ .id = slot } }, .next = undefined } });
            try self.pushValue(if (own == ty) local else try self.retagValue(local, own, ty));
            return;
        }
        // A value no program evaluates: the read fails as the recursion
        // evaluating it in place would end in.
        const message = try self.store.insertString(base.stack_overflow.program_message);
        try self.emitTerminal(.{ .crash = .{
            .msg = .{ .literal = message },
            .extra = try self.store.addStmtRow(LIR.CrashExtra, .{ .failed_value = .{
                .owner = self.program.lowering_module_ids[@backingInt(module_index)],
                .producer = .{ .checked = root },
            } }),
        } });
        try self.pushValue(try self.localOf(ty, .frame));
    }

    /// Lower an expression. What its enclosing expression emits after it,
    /// such as the call taking it as an argument, belongs to the enclosing
    /// expression's region.
    pub fn stepExpr(self: *Builder, expr_id: ExprId) Allocator.Error!void {
        try self.push(.{ .restore_region = self.region });
        try self.lowerExpr(expr_id);
    }

    /// Lower an expression's own value. What its enclosing expression emits
    /// after it belongs to the enclosing expression's region.
    pub fn stepOwnExpr(self: *Builder, expr_id: ExprId) Allocator.Error!void {
        try self.push(.{ .restore_region = self.region });
        try self.lowerOwnExpr(expr_id);
    }

    /// Lower an expression as its parent reads it.
    fn lowerExpr(self: *Builder, expr_id: ExprId) Allocator.Error!void {
        if (self.bodies().rowCoercion(expr_id)) |wide| return try self.lowerRowCoercion(expr_id, wide);
        try self.lowerOwnExpr(expr_id);
    }

    /// The coercion of a row-coerced result into the widened type its
    /// parent reads it at: the value is built straight at the coercion's
    /// row (design.md "Row Coercion Primitive").
    fn lowerRowCoercion(self: *Builder, expr_id: ExprId, wide: checked.CheckedTypeId) Allocator.Error!void {
        self.region = self.nodeRegion(@backingInt(expr_id));
        if (self.destination_types.get(expr_id)) |destination| if (!try self.program.hasError(self.module, wide)) {
            const own = try self.convert(wide);
            if (try self.retagInto(own, destination)) |to| try self.push(.{ .retag = .{ .from = own, .to = to } });
        };
        try self.coerced_destinations.put(self.allocator, expr_id, try self.valueType(expr_id));
        try self.push(.{ .own_expr = expr_id });
    }

    fn lowerOwnExpr(self: *Builder, expr_id: ExprId) Allocator.Error!void {
        const expr = self.bodies().expr(expr_id);
        if (self.ownDestination(expr_id)) |destination| try self.passDestination(expr.data, destination);
        self.region = expr.source_region;
        if (self.ownDestination(expr_id) != null and !try self.program.hasError(self.module, expr.ty)) {
            const own = try self.convert(expr.ty);
            if (try self.retagTarget(expr_id, own)) |destination| try self.push(.{ .retag = .{ .from = own, .to = destination } });
        }
        // An expression compile-time evaluation hoisted and stored is read
        // (design.md "Runtime Programs Read Stored Values").
        if (try self.storedHoisted(expr_id)) |read| switch (read) {
            .stored => |stored| return try self.pushStoredRestore(.{ .module = self.module }, stored, try self.exprType(expr_id), if (self.view.view.hoisted_constants.lookupByExpr(expr_id)) |hoisted| hoisted.root else null),
            .pending => |root| return try self.pendingValueRead(self.module, root, try self.exprType(expr_id)),
        };
        const raw = @backingInt(expr_id);
        switch (expr.data) {
            .numeral => try self.lowerNumeral(.{ .expr = expr_id }, self.bodies().numeralLiteral(raw), self.bodies().numeralPlan(raw), self.bodies().literalConversionRoot(raw)),
            .str_segment => |literal| try self.pushValue(try self.strLiteral(self.bodies().text(literal))),
            .str => |segments| if (self.bodies().quoteIsCustom(raw))
                try self.lowerQuote(.{ .expr = expr_id }, .{ .quote = segments }, self.bodies().quotePlan(raw), self.bodies().literalConversionRoot(raw))
            else
                try self.lowerStrParts(.{ .rest = segments }),
            .lookup_local => |pattern| try self.lowerLookupLocal(expr_id, pattern, self.bodies().resolvedRef(expr_id)),
            .lookup_external => try self.lowerLookupExternal(expr_id, self.bodies().resolvedRef(expr_id) orelse invariant("an external lookup had no resolved reference")),
            .lookup_required => try self.lowerLookupExternal(expr_id, self.bodies().resolvedRef(expr_id) orelse invariant("a platform requirement lookup had no resolved reference")),
            .block => |block| {
                if (self.isTail(expr_id)) try self.tail_exprs.put(self.allocator, block.final_expr, {});
                // A divergent statement leaves the rest of the block
                // unreachable: nothing after it is lowered, and the block's
                // value is never produced.
                const mode = self.inlineExpectMode();
                const diverges_at: ?usize = for (block.statements, 0..) |stmt, position| {
                    if (try self.bodies().statementDiverges(stmt, mode)) break position;
                    // A declaration whose value diverges binds nothing, so
                    // nothing after it is reached either.
                    switch (self.bodies().statementData(stmt)) {
                        .decl => |decl| if (try self.bodies().exprDiverges(decl.expr, mode)) break position,
                        .var_ => |var_| if (try self.bodies().exprDiverges(var_.expr, mode)) break position,
                        .promoted_proc, .var_uninitialized, .reassign, .crash, .dbg, .expr, .expect, .for_, .while_, .infinite_loop, .breakable_loop, .break_, .return_, .rejected_method_decl, .import_, .alias_decl, .where_alias_decl, .nominal_decl, .type_anno, .type_var_alias, .runtime_error => {},
                    }
                } else null;
                var index = if (diverges_at) |position| position + 1 else block.statements.len;
                if (diverges_at == null) {
                    try self.push(.{ .expr = block.final_expr });
                } else {
                    const block_ty = self.bodies().exprType(expr_id);
                    try self.push(.{ .diverged = if (try self.program.hasError(self.module, block_ty)) null else try self.exprType(expr_id) });
                }
                while (index > 0) {
                    index -= 1;
                    try self.push(.{ .stmt = block.statements[index] });
                }
            },
            .call => |call| try self.lowerCall(expr_id, call),
            .dispatch_call => try self.lowerDispatch(expr_id, self.bodies().dispatchPlan(raw) orelse invariant("a dispatch call had no checked plan")),
            .method_eq => try self.lowerDispatch(expr_id, self.bodies().dispatchPlan(raw) orelse invariant("a method equality had no checked plan")),
            .type_dispatch_call => try self.lowerDispatch(expr_id, self.bodies().dispatchPlan(raw) orelse invariant("a type dispatch call had no checked plan")),
            .run_low_level => |run_ll| {
                const args = run_ll.args[0 .. run_ll.args.len - run_ll.op.typeOnlyOperands()];
                const arg_types = try self.program.arena.allocator().alloc(TypeId, args.len);
                for (args, arg_types) |arg, *arg_ty| arg_ty.* = try self.valueType(arg);
                try self.push(.{ .finish_low_level = .{ .op = run_ll.op, .arg_count = @intCast(args.len), .result = try self.exprType(expr_id), .arg_types = arg_types } });
                try self.pushExprsReversed(args);
            },
            // An empty literal at a record type of optional fields builds
            // every field's slot as missing.
            .empty_record => try self.push(.{ .finish_record = expr_id }),
            .record => |record| {
                try self.push(.{ .finish_record = expr_id });
                var index = record.fields.len;
                while (index > 0) {
                    index -= 1;
                    try self.push(.{ .expr = self.bodies().recordFieldValue(record.fields[index]) });
                }
                // An update evaluates its base first.
                if (record.ext) |base_expr| try self.push(.{ .expr = base_expr });
            },
            .tuple => |items| {
                try self.push(.{ .finish_tuple = expr_id });
                try self.pushExprsReversed(items);
            },
            .tag => |tag| {
                try self.push(.{ .finish_tag = expr_id });
                try self.pushExprsReversed(tag.args);
            },
            .zero_argument_tag => try self.finishTag(expr_id, null),
            .list => |items| {
                if (try self.packedNumeralList(expr_id, items)) |packed_list| return try self.pushValue(packed_list);
                try self.push(.{ .finish_list = expr_id });
                try self.pushExprsReversed(items);
            },
            .empty_list => try self.finishList(expr_id),
            .nominal => |nominal| {
                // A tag constructing a nominal is built at the nominal's
                // declared backing row (design.md "Nominal Constructor
                // Backing Relation"), not at the tag's own narrower row.
                const backing = self.bodies().exprData(nominal.backing_expr);
                switch (backing) {
                    .tag => |tag| {
                        const nominal_ty = try self.exprType(expr_id);
                        try self.passDestination(backing, nominal_ty);
                        try self.push(.{ .finish_tag_at = .{ .expr = nominal.backing_expr, .ty = nominal_ty } });
                        try self.pushExprsReversed(tag.args);
                    },
                    .zero_argument_tag => try self.push(.{ .finish_tag_at = .{ .expr = nominal.backing_expr, .ty = try self.exprType(expr_id) } }),
                    .numeral, .str_segment, .str, .bytes_literal, .lookup_local, .lookup_external, .lookup_required, .list, .empty_list, .tuple, .match_, .if_, .call, .record, .empty_record, .block, .nominal, .closure, .lambda, .binop, .unary_minus, .field_access, .dispatch_call, .interpolation, .structural_eq, .structural_hash, .method_eq, .type_dispatch_call, .tuple_access, .runtime_error, .crash, .dbg, .expect_err, .expect, .ellipsis, .anno_only, .break_, .return_, .for_, .hosted_lambda, .run_low_level => {
                        // The operand is the nominal's own backing, which a
                        // padded declaration lays out differently from the
                        // operand's record row.
                        try self.destination_types.put(self.allocator, nominal.backing_expr, try self.structuralType(try self.exprType(expr_id)));
                        try self.push(.{ .expr = nominal.backing_expr });
                    },
                }
            },
            .lambda => try self.lowerClosure(expr_id, expr_id, try self.program.siteCaptures(self.module, expr_id)),
            .closure => |closure| try self.lowerClosure(expr_id, closure.lambda, try self.program.siteCaptures(self.module, expr_id)),
            .structural_eq => |eq| {
                if (self.bodies().tagDiscriminantEquality(eq.lhs, eq.rhs)) |discriminant| {
                    try self.push(.{ .finish_discriminant_eq = .{ .value_ty = try self.valueType(discriminant.value), .tag = discriminant.tag, .negated = eq.negated } });
                    try self.push(.{ .expr = discriminant.value });
                    return;
                }
                try self.push(.{ .finish_eq = .{ .negated = eq.negated, .operand = try self.valueType(eq.lhs) } });
                try self.push(.{ .expr = eq.rhs });
                try self.push(.{ .expr = eq.lhs });
            },
            .expect => |cond| {
                try self.push(.{ .finish_expect = expr_id });
                try self.push(.{ .expr = cond });
            },
            .crash => |literal| try self.crashValue(self.bodies().text(literal), try self.exprType(expr_id)),
            .runtime_error => |runtime_error| {
                // A retired expression evaluates the operands it still
                // evaluates, each for its effects, then crashes.
                const checked_ty = self.bodies().exprType(expr_id);
                try self.push(.{ .runtime_error = if (try self.program.hasError(self.module, checked_ty)) null else try self.exprType(expr_id) });
                var index = runtime_error.evaluated.len;
                while (index > 0) {
                    index -= 1;
                    try self.push(.discard);
                    try self.push(.{ .expr = runtime_error.evaluated[index] });
                }
            },
            .if_ => |if_| {
                const first_cond = self.bodies().ifBranch(if_.branches[0]).cond;
                if (self.isTail(expr_id)) {
                    for (if_.branches) |branch| try self.tail_exprs.put(self.allocator, self.bodies().ifBranch(branch).body, {});
                    try self.tail_exprs.put(self.allocator, if_.final_else, {});
                    // Each branch returns; nothing reads the construct's
                    // own value, and each branch starts from the binder
                    // values the construct started with.
                    const merge = try self.beginMerge(try self.bodies().stateMergeBinders(expr_id), .tail);
                    try self.push(.unreachable_or_unit);
                    try self.push(.{ .if_test = .{ .expr = expr_id, .index = 0, .join = null, .merge = merge } });
                    try self.push(.{ .expr = first_cond });
                    return;
                }
                const result_ty = try self.exprType(expr_id);
                const result = try self.localOf(result_ty, .frame);
                const join = self.freshJoin();
                const merge = try self.beginMerge(try self.bodies().stateMergeBinders(expr_id), .joined);
                const after = try self.declareMergeJoin(join, result, merge);
                try self.push(.{ .push_value = result });
                if (merge) |m| try self.push(.{ .enter_merge = m });
                try self.push(.{ .resume_at = after });
                try self.push(.{ .if_test = .{ .expr = expr_id, .index = 0, .join = join, .merge = merge } });
                try self.push(.{ .expr = first_cond });
            },
            .match_ => |match| {
                // A match on a layout-resolved low-level whose answer is
                // known here is the branch that answer selects.
                if (try self.layoutResolvedBranch(match.cond, match.branches)) |value| {
                    try self.destination_types.put(self.allocator, value, try self.exprType(expr_id));
                    return try self.push(.{ .expr = value });
                }
                // A scrutinee that diverges leaves every branch unreachable;
                // checking relates no pattern to it, so it has no decision
                // tree.
                if (try self.bodies().exprDiverges(match.cond, self.inlineExpectMode())) {
                    const match_ty = self.bodies().expr(expr_id).ty;
                    try self.push(.{ .diverged = if (try self.program.hasError(self.module, match_ty)) null else try self.exprType(expr_id) });
                    try self.push(.discard);
                    return try self.push(.{ .expr = match.cond });
                }
                try self.push(.{ .match_start = expr_id });
                try self.push(.{ .expr = match.cond });
            },
            .binop => |binop| switch (binop.op) {
                .@"and", .@"or" => try self.lowerShortCircuit(binop.op == .@"and", binop.lhs, binop.rhs),
                .add, .sub, .mul, .div, .rem, .lt, .gt, .le, .ge, .eq, .ne, .div_trunc, .range_exclusive, .range_inclusive => invariantFmt("binary operator {s} reached Boxy lowering outside a dispatch", .{@tagName(binop.op)}),
            },
            .for_ => |for_| try self.pushForLoop(raw, for_.pattern, for_.expr, for_.body, self.bodies().iteratorForPlan(raw)),
            .field_access => |access| {
                try self.push(.{ .finish_field_access = expr_id });
                try self.push(.{ .expr = access.receiver });
            },
            .tuple_access => |access| {
                try self.push(.{ .finish_tuple_access = expr_id });
                try self.push(.{ .expr = access.tuple });
            },
            .dbg => |inner| {
                try self.push(.{ .finish_dbg = try self.valueType(inner) });
                try self.push(.{ .expr = inner });
            },
            .interpolation => |interpolation| if (self.bodies().interpolationIsBuiltinStr(expr_id))
                try self.lowerStrParts(interpolation.strParts())
            else
                try self.lowerInterpolation(expr_id),
            .expect_err => |expect_err| {
                try self.push(.{ .finish_expect_err = .{ .expr = expr_id, .value = try self.valueType(expect_err.expr) } });
                try self.push(.{ .expr = expect_err.expr });
            },
            .return_ => |ret| {
                // The expression itself never produces a value; its successor
                // is unreachable.
                try self.push(.{ .unreachable_value = try self.exprType(expr_id) });
                try self.push(.ret);
                try self.destination_types.put(self.allocator, ret.expr, self.currentWorker().ret);
                try self.push(.{ .expr = ret.expr });
            },
            .structural_hash => |hash| {
                try self.push(.{ .finish_hash = .{ .value = try self.valueType(hash.value), .result = try self.exprType(expr_id) } });
                try self.push(.{ .expr = hash.hasher });
                try self.push(.{ .expr = hash.value });
            },
            .bytes_literal => |literal| {
                const bytes = self.bodies().text(literal);
                const local = try self.localOf(try self.exprType(expr_id), .frame);
                const backing = try self.store.insertStringAligned(bytes, 16);
                try self.emitLinear(.{ .assign_literal = .{ .target = local, .value = .{ .bytes_literal = .{
                    .bytes = .{ .backing = backing, .offset = 0, .len = @intCast(bytes.len) },
                    .len = @intCast(bytes.len),
                } }, .next = undefined } });
                try self.pushValue(local);
            },
            .ellipsis => try self.crashValue("not implemented", try self.exprType(expr_id)),
            .break_ => {
                if (self.loops.items.len == 0) invariant("a break outside every loop reached Boxy lowering");
                const loop = &self.loops.items[self.loops.items.len - 1];
                loop.reached = true;
                try self.emitJump(loop.join, null);
                try self.push(.{ .unreachable_value = try self.exprType(expr_id) });
            },
            .unary_minus => invariant("a unary minus reached Boxy lowering without its checked dispatch"),
            .anno_only => invariant("an annotation-only declaration is read, which resolves to what it requests instead"),
            .hosted_lambda => invariant("a hosted lambda is a hosted template's body root, never a nested expression"),
        }
    }

    /// Hand an expression's destination type to the subexpressions whose
    /// value is its value.
    pub fn passDestination(self: *Builder, data: bodies_mod.ExprData, destination: TypeId) Allocator.Error!void {
        switch (data) {
            .block => |block| try self.destination_types.put(self.allocator, block.final_expr, destination),
            .if_ => |if_| {
                for (if_.branches) |branch| try self.destination_types.put(self.allocator, self.bodies().ifBranch(branch).body, destination);
                try self.destination_types.put(self.allocator, if_.final_else, destination);
            },
            .match_ => |match| for (match.branches) |branch| try self.destination_types.put(self.allocator, self.bodies().matchBranch(branch).value, destination),
            .nominal => |nominal| try self.destination_types.put(self.allocator, nominal.backing_expr, try self.structuralType(destination)),
            // A tag's payloads are reached positions of its destination's
            // row (design.md "Row Coercion Primitive").
            .tag => |tag| {
                const store = &self.program.store;
                const tag_union = switch (store.get(try self.structuralType(destination))) {
                    .tag_union => |value| value,
                    .primitive, .empty_record, .record, .tuple, .list, .box, .nominal, .function, .variable => return,
                };
                const name = try store.internName(self.view.nameStore().tagLabelText(self.bodies().tagLabel(tag.name)));
                for (store.tagSpan(tag_union.tags)) |candidate| {
                    if (candidate.name != name or candidate.payloads.len != tag.args.len) continue;
                    for (tag.args, store.span(candidate.payloads)) |arg, payload_ty| try self.destination_types.put(self.allocator, arg, payload_ty);
                    return;
                }
            },
            .numeral, .str_segment, .str, .bytes_literal, .lookup_local, .lookup_external, .lookup_required, .list, .empty_list, .tuple, .call, .record, .empty_record, .zero_argument_tag, .closure, .lambda, .binop, .unary_minus, .field_access, .dispatch_call, .interpolation, .structural_eq, .structural_hash, .method_eq, .type_dispatch_call, .tuple_access, .runtime_error, .crash, .dbg, .expect_err, .expect, .ellipsis, .anno_only, .break_, .return_, .for_, .hosted_lambda, .run_low_level => {},
        }
    }

    /// A string concatenated from its parts in order.
    fn lowerStrParts(self: *Builder, parts: bodies_mod.StrParts) Allocator.Error!void {
        // An empty literal segment writes nothing: it is not a part.
        var count: u32 = 0;
        for (0..parts.len()) |index| count += @intFromBool(!self.bodies().isEmptyStrSegment(parts.at(index)));
        try self.push(.{ .finish_str = .{ .count = count } });
        var index = parts.len();
        while (index > 0) {
            index -= 1;
            if (!self.bodies().isEmptyStrSegment(parts.at(index))) try self.push(.{ .expr = parts.at(index) });
        }
    }

    pub fn pushExprsReversed(self: *Builder, exprs: []const ExprId) Allocator.Error!void {
        var index = exprs.len;
        while (index > 0) {
            index -= 1;
            try self.push(.{ .expr = exprs[index] });
        }
    }

    pub fn strLiteral(self: *Builder, text: []const u8) Allocator.Error!LocalId {
        const local = try self.closedLocal(.str);
        const backing = try self.store.insertString(text);
        try self.emitLinear(.{ .assign_literal = .{
            .target = local,
            .value = .{ .str_literal = .{ .backing = backing, .offset = 0, .len = @intCast(text.len) } },
            .next = undefined,
        } });
        return local;
    }

    /// A `Str` literal's `count` lowered parts (its non-empty segments and
    /// its values, in order) written once by `str_from_parts`.
    pub fn finishStr(self: *Builder, count: u32) Allocator.Error!void {
        const parts = try self.popValues(count);
        defer self.allocator.free(parts);
        switch (parts.len) {
            0 => try self.pushValue(try self.strLiteral("")),
            1 => try self.pushValue(parts[0]),
            else => {
                const target = try self.closedLocal(.str);
                try self.emitLowLevel(target, .str_from_parts, parts);
                try self.pushValue(target);
            },
        }
    }

    /// A list of builtin numerals at a numeric primitive element type, as one
    /// literal of the elements' packed bytes, exactly as Monotype lowers it
    /// (`lowerPackedNumeralList`), so the list's code does not grow with its
    /// length. Null for any other list, which builds its elements one by one.
    fn packedNumeralList(self: *Builder, expr_id: ExprId, items: []const ExprId) Allocator.Error!?LocalId {
        if (items.len == 0) return null;
        const ty = try self.exprType(expr_id);
        if (!try self.closed(ty)) return null;
        const elem = switch (self.program.store.get(try self.structuralType(ty))) {
            .list => |elem| elem,
            .primitive, .empty_record, .record, .tuple, .tag_union, .box, .nominal, .function, .variable => invariant("a list's type is not a List"),
        };
        const primitive = switch (self.program.store.get(elem)) {
            .primitive => |primitive| primitive,
            .empty_record, .record, .tuple, .tag_union, .list, .box, .nominal, .function, .variable => return null,
        };
        const width: usize = switch (primitive) {
            .u8, .i8 => 1,
            .u16, .i16 => 2,
            .u32, .i32, .f32 => 4,
            .u64, .i64, .f64 => 8,
            .u128, .i128, .dec => 16,
            .bool, .str, .u8x16, .i8x16, .u16x8, .i16x8, .u32x4, .i32x4, .u64x2, .i64x2 => return null,
        };
        for (items) |item| switch (self.bodies().exprData(item)) {
            .numeral => if (self.bodies().numeralPlan(@backingInt(item)) != null or self.bodies().literalConversionRoot(@backingInt(item)) != null) return null,
            .str_segment, .str, .bytes_literal, .lookup_local, .lookup_external, .lookup_required, .list, .empty_list, .tuple, .match_, .if_, .call, .record, .empty_record, .block, .tag, .nominal, .zero_argument_tag, .closure, .lambda, .binop, .unary_minus, .field_access, .dispatch_call, .interpolation, .structural_eq, .structural_hash, .method_eq, .type_dispatch_call, .tuple_access, .runtime_error, .crash, .dbg, .expect_err, .expect, .ellipsis, .anno_only, .break_, .return_, .for_, .hosted_lambda, .run_low_level => return null,
        };
        const exact_numeral = checked.exact_numeral;
        const bytes = try self.allocator.alloc(u8, items.len * width);
        defer self.allocator.free(bytes);
        for (items, 0..) |item, index| {
            const exact = self.view.view.module_env.exactNumeral(self.bodies().numeralLiteral(@backingInt(item)));
            var encoded: [16]u8 = undefined;
            switch (primitive) {
                inline .u8, .i8, .u16, .i16, .u32, .i32, .u64, .i64, .u128, .i128 => |integer| {
                    const bits = exact_numeral.intBits(exact, @field(exact_numeral.Target, @tagName(integer))) orelse return null;
                    const wide: i128 = switch (bits) {
                        .i128 => |value| value,
                        .u128 => |value| @bitCast(value),
                    };
                    std.mem.writeInt(i128, &encoded, wide, .little);
                },
                .f32 => std.mem.writeInt(u32, encoded[0..4], @bitCast(try exact_numeral.floatBits(f32, self.allocator, exact)), .little),
                .f64 => std.mem.writeInt(u64, encoded[0..8], @bitCast(try exact_numeral.floatBits(f64, self.allocator, exact)), .little),
                .dec => std.mem.writeInt(i128, &encoded, (try exact_numeral.decBits(self.allocator, exact)) orelse return null, .little),
                .bool, .str, .u8x16, .i8x16, .u16x8, .i16x8, .u32x4, .i32x4, .u64x2, .i64x2 => unreachable, // the width switch admits only scalar numerics
            }
            @memcpy(bytes[index * width ..][0..width], encoded[0..width]);
        }
        const local = try self.localOf(ty, .frame);
        const backing = try self.store.insertStringAligned(bytes, @intCast(width));
        try self.emitLinear(.{ .assign_literal = .{ .target = local, .value = .{ .bytes_literal = .{
            .bytes = .{ .backing = backing, .offset = 0, .len = @intCast(bytes.len) },
            .len = @intCast(items.len),
        } }, .next = undefined } });
        return local;
    }

    /// A numeric literal at `at`: its conversion root's value, its
    /// conversion `plan`'s result, or a builtin number.
    pub fn lowerNumeral(self: *Builder, at: LiteralAt, literal: NumeralLiteral, plan: ?static_dispatch.StaticDispatchPlanId, conversion_root: ?checked.ComptimeRootId) Allocator.Error!void {
        if (conversion_root) |root| return try self.restoreConversionRoot(root, at, .numeral, plan);
        if (plan) |plan_id| return try self.lowerPlannedLiteral(at.node(), .numeral, plan_id, try self.literalType(at));
        const ty = try self.literalType(at);
        // A numeral at a non-builtin type with no conversion plan is one
        // whose conversion checking rejected and reported: the checked
        // module records no plan for it, and reaching it is a checked error.
        if (self.program.store.get(ty) != .primitive) return try self.rejectedLiteralValue(ty);
        try self.pushValue(try self.builtinNumeral(literal, ty));
    }

    /// A literal-only string at `at` whose target is custom or open: its
    /// conversion root's value, its conversion `plan`'s result, or the
    /// string itself at a `Str` target.
    pub fn lowerQuote(self: *Builder, at: LiteralAt, literal: bodies_mod.Text, plan: ?static_dispatch.StaticDispatchPlanId, conversion_root: ?checked.ComptimeRootId) Allocator.Error!void {
        if (conversion_root) |root| return try self.restoreConversionRoot(root, at, .quote, plan);
        if (plan) |plan_id| return try self.lowerPlannedLiteral(at.node(), .quote, plan_id, try self.literalType(at));
        const ty = try self.literalType(at);
        const is_str = switch (self.program.store.get(ty)) {
            .primitive => |primitive| primitive == .str,
            .empty_record, .record, .tuple, .tag_union, .list, .box, .nominal, .function, .variable => false,
        };
        if (!is_str) return try self.rejectedLiteralValue(ty);
        try self.pushValue(try self.textLiteral(literal));
    }

    /// The type a literal converts to: a literal expression's own, or a
    /// custom literal pattern's.
    fn literalType(self: *Builder, at: LiteralAt) Allocator.Error!TypeId {
        return switch (at) {
            .expr => |expr| try self.exprType(expr),
            .pattern => |pattern| try self.patternType(pattern),
        };
    }

    /// A `Str` value of `text`'s bytes.
    pub fn textLiteral(self: *Builder, text: bodies_mod.Text) Allocator.Error!LocalId {
        var bytes: std.ArrayList(u8) = .empty;
        defer bytes.deinit(self.allocator);
        try self.bodies().appendText(self.allocator, text, &bytes);
        return try self.strLiteral(bytes.items);
    }

    /// A literal whose conversion checking rejected: the crash a
    /// specialized program reaches there.
    fn rejectedLiteralValue(self: *Builder, ty: TypeId) Allocator.Error!void {
        const literal = try self.store.insertString("runtime error");
        try self.emitTerminal(.{ .crash = .{ .msg = .{ .literal = literal }, .checked_error = true } });
        try self.pushValue(try self.localOf(ty, .frame));
    }

    /// A literal whose conversion plan has no stored root value: converted
    /// by its instance's literal argument where the plan is open, in place
    /// where it is closed, and a crash where checking rejected the plan or
    /// found its value unreachable.
    fn lowerPlannedLiteral(self: *Builder, node: u32, kind: LIR.LiteralRejectionKind, plan_id: static_dispatch.StaticDispatchPlanId, lit_ty: TypeId) Allocator.Error!void {
        switch (self.dispatchPlan(plan_id).resolution) {
            .direct_closed => try self.lowerLiteralConversion(node, kind, plan_id, lit_ty),
            .direct_parametric, .evidence_dependent => try self.lowerInstanceLiteral(node, lit_ty),
            .checked_error => {
                try self.emitDispatchCrash(.checked_error);
                try self.pushValue(try self.localOf(lit_ty, .frame));
            },
            .@"unreachable" => {
                try self.emitDispatchCrash(.unreachable_value);
                try self.pushValue(try self.localOf(lit_ty, .frame));
            },
            .structural, .direct_pending => invariant("a literal's conversion plan has no callable resolution"),
        }
    }

    /// A numeric literal at the builtin numeric type `ty`.
    pub fn builtinNumeral(self: *Builder, literal: NumeralLiteral, ty: TypeId) Allocator.Error!LocalId {
        const primitive = switch (self.program.store.get(ty)) {
            .primitive => |primitive| primitive,
            .empty_record, .record, .tuple, .tag_union, .list, .box, .nominal, .function, .variable => invariant("a builtin numeral's type is not a primitive"),
        };
        const exact = self.view.view.module_env.exactNumeral(literal);
        const exact_numeral = checked.exact_numeral;
        const local = try self.closedLocal(@import("layouts.zig").primitiveLayout(primitive));
        const value: LIR.LiteralValue = switch (primitive) {
            .u8, .u16, .u32, .u64, .i8, .i16, .i32, .i64, .u128, .i128 => blk: {
                const target: exact_numeral.Target = switch (primitive) {
                    inline .u8, .i8, .u16, .i16, .u32, .i32, .u64, .i64, .u128, .i128 => |p| @field(exact_numeral.Target, @tagName(p)),
                    .bool, .str, .f32, .f64, .dec, .u8x16, .i8x16, .u16x8, .i16x8, .u32x4, .i32x4, .u64x2, .i64x2 => unreachable, // the enclosing prong admits only integers
                };
                const bits = exact_numeral.intBits(exact, target) orelse return try self.crashLocal("invalid numeric literal", ty);
                const wide: i128 = switch (bits) {
                    .i128 => |v| v,
                    .u128 => |v| @bitCast(v),
                };
                break :blk switch (primitive) {
                    .u128, .i128 => .{ .i128_literal = try self.store.addStmtRow(LIR.I128Literal, .{ .value = wide, .layout_idx = @import("layouts.zig").primitiveLayout(primitive) }) },
                    .bool, .str, .u8, .i8, .u16, .i16, .u32, .i32, .u64, .i64, .f32, .f64, .dec, .u8x16, .i8x16, .u16x8, .i16x8, .u32x4, .i32x4, .u64x2, .i64x2 => .{ .i64_literal = .{ .value = @truncate(wide), .layout_idx = @import("layouts.zig").primitiveLayout(primitive) } },
                };
            },
            .f32 => .{ .f32_literal = try exact_numeral.floatBits(f32, self.allocator, exact) },
            .f64 => .{ .f64_literal = try exact_numeral.floatBits(f64, self.allocator, exact) },
            .dec => .{ .dec_literal = (try exact_numeral.decBits(self.allocator, exact)) orelse return try self.crashLocal("invalid numeric literal", ty) },
            .bool, .str, .u8x16, .i8x16, .u16x8, .i16x8, .u32x4, .i32x4, .u64x2, .i64x2 => return try self.crashLocal("invalid numeric literal", ty),
        };
        try self.emitLinear(.{ .assign_literal = .{ .target = local, .value = value, .next = undefined } });
        return local;
    }

    /// Call a literal's conversion (`from_numeral`, `from_quote`,
    /// `from_interpolation`) at the instance the body has there and unwrap
    /// its `Try` result: the `Ok` payload is the literal's value, and an
    /// `Err` crashes as the literal's rejection with the conversion's
    /// message. Only the compile-time evaluator's program converts this way;
    /// every evidence parameter of its workers is a dictionary, so the
    /// conversion's dispatch has its instance in scope. The conversion
    /// produces `Try(result_ty, row)`: checking unifies the plan's `Ok`
    /// payload with the literal's type, which is `result_ty` here, and fixes
    /// its error row to closed tags carrying the message.
    fn lowerLiteralConversion(self: *Builder, node: u32, kind: LIR.LiteralRejectionKind, plan_id: static_dispatch.StaticDispatchPlanId, result_ty: TypeId) Allocator.Error!void {
        const call_plan = self.dispatchPlan(plan_id);
        if (call_plan.result_mode != .value) invariant("a literal conversion plan has a non-value result mode");
        const checked_types = self.view.types();
        var ret = switch (checked_types.payload(call_plan.callable_ty)) {
            .function => |function| function.ret,
            .pending, .field_kind, .err, .flex, .rigid, .alias, .record, .tuple, .nominal, .empty_record, .tag_union, .empty_tag_union => invariant("a literal conversion's callable type is not a function type"),
        };
        const row = while (true) switch (checked_types.payload(ret)) {
            .alias => |alias| ret = alias.backing,
            .nominal => |nominal| {
                if (nominal.args.len != 2) invariant("a literal conversion's result is not a Try type");
                break nominal.args[1];
            },
            .pending, .field_kind, .err, .flex, .rigid, .function, .record, .tuple, .empty_record, .tag_union, .empty_tag_union => invariant("a literal conversion's result is not a Try type"),
        };
        const try_ty = try self.program.converter().nominalAt(self.module, ret, &.{ result_ty, try self.convert(row) });
        try self.push(.{ .finish_literal_conversion = .{ .node = node, .kind = kind, .try_ty = try_ty, .result = result_ty } });
        try self.lowerDispatchAt(plan_id, try_ty);
    }

    /// Pop a literal conversion's `Try` result: push its `Ok` payload at
    /// the literal's type, or crash as the literal's rejection with the
    /// `Err` payload's message. The checker fixes every conversion's error
    /// row to tags carrying the message (`[InvalidNumeral(Str)]`,
    /// `[BadQuotedBytes(Str)]`, `[InvalidInterpolation(Str)]`), so the row
    /// is closed even where the `Ok` payload is an open instance type.
    fn finishLiteralConversion(self: *Builder, conversion: FinishLiteralConversion) Allocator.Error!void {
        const store = &self.program.store;
        const try_value = self.popValue();
        const try_ty = try self.structuralType(conversion.try_ty);
        const ok_name = try store.internName("Ok");
        const err_name = try store.internName("Err");
        // Whether the result is `Ok`: its discriminant against `Ok`'s.
        const actual = try self.closedLocal(.u32);
        const expected = try self.closedLocal(.u32);
        if (try self.closed(try_ty)) {
            const ok_variant = try self.closedVariant(try_ty, ok_name);
            const committed = self.program.result.layouts.getLayout(try self.program.layouts.layoutOf(try_ty));
            if (committed.tag == .tag_union) {
                try self.emitLinear(.{ .assign_ref = .{ .target = actual, .op = LIR.RefOp.discriminant(try_value), .next = undefined } });
            } else {
                try self.emitLinear(.{ .assign_literal = .{ .target = actual, .value = .{ .i64_literal = .{ .value = 0, .layout_idx = .u32 } }, .next = undefined } });
            }
            try self.emitLinear(.{ .assign_literal = .{ .target = expected, .value = .{ .i64_literal = .{ .value = @intCast(ok_variant.index), .layout_idx = .u32 } }, .next = undefined } });
        } else {
            try self.emitDyn(actual, .{ .tag_discriminant = .{ .base = try_value, .desc = try self.descOf(try_ty) } });
            try self.assignIndex(expected, try self.tagIndex(try_ty, ok_name));
        }
        const is_ok = try self.closedLocal(.bool);
        try self.emitLowLevel(is_ok, .num_is_eq, &.{ actual, expected });
        const ok_hole = try self.newHole();
        const err_hole = try self.newHole();
        try self.emitTerminal(.{ .switch_stmt = .{
            .cond = is_ok,
            .branches = try self.store.addCFSwitchBranches(&.{.{ .value = 1, .body = ok_hole }}),
            .default_branch = err_hole,
        } });
        // The `Err` arms crash, so what follows the conversion continues in
        // the `Ok` arm, reading the payload from the result it is part of.
        self.hole = err_hole;
        const err = try self.tagPayloadValue(try_value, conversion.try_ty, err_name, 0);
        const err_ty = try self.structuralType(err.ty);
        if (!try self.closed(err_ty)) invariant("a literal conversion's error row is open; the checker closes it");
        const tags = try self.allocator.dupe(types.Tag, switch (store.get(err_ty)) {
            .tag_union => |union_ty| store.tagSpan(union_ty.tags),
            .primitive, .empty_record, .record, .tuple, .list, .box, .nominal, .function, .variable => invariant("a literal conversion's error type is not a tag union"),
        });
        defer self.allocator.free(tags);
        if (tags.len == 0) invariant("a literal conversion's error row has no tag");
        const site: LIR.LiteralRejectionSite = .{
            .owner = self.program.lowering_module_ids[@backingInt(self.module)],
            .node = conversion.node,
            .kind = conversion.kind,
        };
        // Each tag of the row carries the message as its one payload.
        const holes = try self.allocator.alloc(CFStmtId, tags.len);
        defer self.allocator.free(holes);
        if (tags.len == 1) {
            holes[0] = self.hole.?;
        } else {
            for (holes) |*hole| hole.* = try self.newHole();
            const branches = try self.allocator.alloc(LIR.CFSwitchBranch, tags.len - 1);
            defer self.allocator.free(branches);
            for (branches, holes[0..branches.len], 0..) |*branch, hole, position| branch.* = .{ .value = position, .body = hole };
            const discriminant = try self.closedLocal(.u32);
            try self.emitLinear(.{ .assign_ref = .{ .target = discriminant, .op = LIR.RefOp.discriminant(err.value), .next = undefined } });
            try self.emitTerminal(.{ .switch_stmt = .{ .cond = discriminant, .branches = try self.store.addCFSwitchBranches(branches), .default_branch = holes[holes.len - 1] } });
        }
        for (tags, holes, 0..) |tag, hole, index| {
            self.hole = hole;
            const payloads = store.span(tag.payloads);
            if (payloads.len != 1) invariant("a literal conversion's error tag does not carry one message");
            const message = try self.readTagPayload(err.value, err_ty, index, payloads, 0);
            try self.emitTerminal(.{ .crash = .{
                .msg = .{ .local = message },
                .extra = try self.store.addStmtRow(LIR.CrashExtra, .{ .literal_rejection = site }),
            } });
        }
        self.hole = ok_hole;
        const ok = try self.tagPayloadValue(try_value, conversion.try_ty, ok_name, 0);
        try self.pushValue(try self.adaptValue(ok.value, ok.ty, conversion.result));
    }

    /// A literal with a conversion root of its own: the root's stored value
    /// (design.md "Runtime Programs Read Stored Values"). A root no
    /// evaluation completed is converted where the literal is, as an
    /// instance-dependent literal is: by the compile-time evaluator's own
    /// program, whose roots run before the discovery that evaluates
    /// conversion roots, or by a runtime program at the instance.
    // -- Literal arguments (design.md "Literal Arguments") --

    /// The literal argument in scope for literal requirement `index` (depth 0 is the
    /// innermost scope: an entered scheme's, then the worker's own): the
    /// immortal callable producing the literal's converted value.
    pub fn scopeLiteral(self: *Builder, index: static_dispatch.EvidenceChainIndex) Allocator.Error!Word {
        if (self.foreign_scopes) |first| {
            const scopes = self.alias_scopes.items[first..];
            // A frame of a literal thunk's chain that lies outside the
            // frames the thunk entered is one the chain never reads.
            if (index.depth >= scopes.len) {
                if (self.thunk_frames != null) return .{ .static = .zero };
                invariant("an exact procedure alias's lookup names a literal argument outside the alias's scope");
            }
            return try self.frameLiteral(first + scopes.len - 1 - index.depth, index.index);
        }
        const aliases = self.alias_scopes.items;
        if (index.depth < aliases.len) return try self.frameLiteral(aliases.len - 1 - index.depth, index.index);
        const depth = index.depth - aliases.len;
        const scopes = self.currentWorker().literal_scopes;
        if (depth >= scopes.len) invariant("an edge named a literal argument outside its worker's scopes");
        var offset: usize = 0;
        for (scopes[0..depth]) |scope| offset += scope.len;
        if (index.index >= scopes[depth].len) invariant("an edge named a literal argument its scope does not have");
        return try self.literalInputWord(offset + index.index);
    }

    /// Literal argument `index` of the frame at `position` of `alias_scopes`.
    /// A deferred argument is resolved in the scope holding the frame's edge.
    fn frameLiteral(self: *Builder, position: usize, index: u32) Allocator.Error!Word {
        return try self.scopeFrameLiteral(self.alias_scopes.items[position], position, index);
    }

    /// Literal argument `index` of `scope`, the frame at `position` of
    /// `alias_scopes`, whose frames below it are in place.
    fn scopeFrameLiteral(self: *Builder, scope: AliasScope, position: usize, index: u32) Allocator.Error!Word {
        const deferred = scope.deferred orelse {
            if (index >= scope.literals.len) invariant("an edge named a literal argument its scope does not have");
            return scope.literals[index];
        };
        if (index >= scope.literal_requirements.len) invariant("an edge named a literal argument its scope does not have");
        switch (deferred) {
            .thunks => |thunks| return try self.composeDictionary(try self.program.literalThunkWorker(thunks.owner, thunks.bound, index), &.{}),
            .edge => |edge| {
                const saved_module = self.module;
                const saved_view = self.view;
                const saved_env = self.env;
                const saved_scopes = self.alias_scopes.items.len;
                defer {
                    self.module = saved_module;
                    self.view = saved_view;
                    self.env = saved_env;
                    self.alias_scopes.items.len = saved_scopes;
                }
                self.module = edge.module;
                self.view = edge.view;
                self.env = edge.env;
                // The edge's forwarded arguments name the frames below it.
                self.alias_scopes.items.len = position;
                return switch (edge.args[index]) {
                    .root => |root| try self.composeRootThunk(root),
                    .forwarded => |forwarded| try self.scopeLiteral(.{ .depth = 0, .index = forwarded }),
                    .unbound => try self.unreadLiteralArg(),
                };
            },
        }
    }

    /// A word holding null, which no reference counting reads: the static
    /// word `zero`.
    fn nullWord(self: *Builder) Allocator.Error!LocalId {
        const local = try self.closedLocal(.opaque_ptr);
        try self.emitEntry(.{ .assign_literal = .{ .target = local, .value = .{ .static_data = .{ .id = try self.program.static_descs.nullWordSlot() } }, .next = undefined } });
        return local;
    }

    /// Fill `out` with the literal arguments of a call of `callee` at the
    /// edge `edge` of this body's module: one per literal requirement of the callee's
    /// scheme, from the checked edge arguments (`literalEdgeArgs`).
    pub fn fillLiteralArgs(self: *Builder, callee: ?WorkerId, edge: checked.LiteralEdge, out: []Word) Allocator.Error!void {
        if (out.len == 0) return;
        const args = self.view.view.checked_procedure_templates.literalEdgeArgs(edge);
        if (args.len != out.len) invariantFmt("an edge's {d} literal arguments do not cover its callee's {d} literal requirements (caller {s}, callee {s}{s})", .{
            args.len,
            out.len,
            self.workerName(self.worker_id),
            if (callee) |id| self.workerName(id) else "a local procedure",
            if (callee) |id| (if (self.program.worker(id).imported) " of an imported module" else "") else "",
        });
        for (args, out) |source, *word| word.* = try self.literalArg(source);
    }

    /// The source name of a template worker's procedure, for diagnostics.
    pub fn workerName(self: *Builder, id: WorkerId) []const u8 {
        return switch (self.program.worker(id).key) {
            .template => |key| blk: {
                const view = self.program.module(key.module);
                const template = view.view.checked_procedure_templates.templates.items[@backingInt(key.template)];
                const export_name = view.nameStore().procBase(template.proc_base).export_name orelse break :blk "a template";
                break :blk view.nameStore().exportNameText(export_name);
            },
            inline .adapter, .default, .lambda, .method_entry, .structural, .structural_map, .root, .codec, .static_data, .crash_dictionary, .convert, .literal_thunk, .root_slot, .callable_eval_entry => |_, kind| @tagName(kind),
        };
    }

    /// The literal argument an edge argument source denotes.
    fn literalArg(self: *Builder, source: checked.LiteralArgSource) Allocator.Error!Word {
        return switch (source) {
            .root => |root| try self.rootThunk(root),
            .forwarded => |index| try self.scopeLiteral(.{ .depth = 0, .index = index }),
            .unbound => try self.unreadLiteralArg(),
        };
    }

    /// The literal argument of an instantiation no use fixes: like a
    /// `from_scheme` dictionary, a crash nothing reads.
    fn unreadLiteralArg(self: *Builder) Allocator.Error!Word {
        return try self.composeDictionary(try self.program.crashDictionaryWorker(self.module, .unreachable_value), &.{});
    }

    /// The immortal callable of this body's module's `literal_argument`
    /// root `root`: its entry wrapper's dictionary entry, which restores the
    /// stored value, or converts the literal where none is stored.
    pub fn rootThunk(self: *Builder, root: checked.ComptimeRootId) Allocator.Error!Word {
        const key: RootThunkKey = .{ .module = self.module, .root = root };
        if (self.root_thunks.get(key)) |existing| return existing;
        const thunk = try self.composeRootThunk(root);
        try self.root_thunks.put(self.allocator, key, thunk);
        return thunk;
    }

    /// `rootThunk`'s callable.
    fn composeRootThunk(self: *Builder, root: checked.ComptimeRootId) Allocator.Error!Word {
        const compile_time_root = self.view.view.compile_time_roots.root(root);
        const argument = compile_time_root.literal_argument orelse invariant("a literal argument root records no literal argument");
        // In the compile-time program evaluating the root, a reader reads
        // its value from its slot once it completes; its wrapper converts
        // the literal only as the root's own evaluation. A validate-only
        // root stores no value, so its wrapper converts for every reader.
        const slot_read = compile_time_root.payloadFor(.boxy) == .pending and !argument.validate_only and self.program.evaluatesRoot(self.module, root);
        const entry = if (slot_read)
            try self.program.rootSlotThunkWorker(self.module, root, try self.convert(argument.ty))
        else blk: {
            const wrapper = self.program.wrapper_by_root.get(.{ .module = self.module, .root = root }) orelse
                invariant("a literal argument root has no entry wrapper");
            break :blk try self.program.methodEntryWorker(try self.program.templateRefWorker(wrapper));
        };
        return try self.composeDictionary(entry, &.{});
    }

    /// The literal arguments of `worker` whose scheme takes `bound`, for an
    /// instantiation checking recorded no edge for: a thunk per literal requirement
    /// of each of its scopes, converting the literal at those types
    /// (`WorkerKey.literal_thunk`), captured scopes' after its own.
    pub fn derivedLiteralArgs(self: *Builder, worker_id: WorkerId, bound: []const TypeId, out: []Word) Allocator.Error!void {
        if (out.len == 0) return;
        const entries = try self.program.literalThunks(self.allocator, worker_id, bound);
        defer self.allocator.free(entries);
        if (entries.len != out.len) invariant("a worker's literal arguments differ from its scopes' literal_requirements");
        for (entries, out) |entry, *literal| literal.* = try self.composeDictionary(entry, &.{});
    }

    /// Literal argument `index` of `worker_id` whose scheme takes `bound`,
    /// derived as `derivedLiteralArgs` derives every one.
    fn derivedLiteralArg(self: *Builder, worker_id: WorkerId, bound: []const TypeId, index: u32) Allocator.Error!Word {
        const entries = try self.program.literalThunks(self.allocator, worker_id, bound);
        defer self.allocator.free(entries);
        if (index >= entries.len) invariant("a worker's literal arguments differ from its scopes' literal_requirements");
        return try self.composeDictionary(entries[index], &.{});
    }

    /// The workers whose own scopes are the scopes of `worker_id`, innermost
    /// first: the worker itself where it has a scope of its own, then those
    /// of its outer chain. Each one's scheme variables are a prefix of the
    /// next inner one's.
    fn scopeOwners(self: *Builder, worker_id: WorkerId) Allocator.Error![]WorkerId {
        return try self.program.scopeOwners(self.allocator, worker_id);
    }

    /// A literal whose conversion depends on the instance the body runs at:
    /// the value its literal argument produces (design.md "Literal
    /// Arguments"). An interpolation's argument produces its assembler.
    pub fn lowerInstanceLiteral(self: *Builder, node: u32, lit_ty: TypeId) Allocator.Error!void {
        const index = self.ownLiteralRequirementIndex(node) orelse invariantFmt("a literal whose conversion depends on its instance has no literal_requirement in its scope (worker {s} #{d}, module {d}, expr {d}, alias scopes {d}, foreign {?}, literal scopes {d}, frames {?})", .{ @tagName(self.currentWorker().key), @backingInt(self.worker_id), @backingInt(self.module), node, self.alias_scopes.items.len, self.foreign_scopes, self.currentWorker().literal_scopes.len, self.thunk_frames });
        const thunk = try self.materialize(try self.scopeLiteral(.{ .depth = 0, .index = index }));
        const fn_ty = try self.program.store.intern(.{ .function = .{ .args = &.{}, .ret = lit_ty, .effectful = false } });
        try self.pushValue(try self.emitErasedCall(thunk, fn_ty, &.{}, lit_ty));
    }

    /// The literal requirement of the innermost scope whose literal is at
    /// CIR node `node`.
    fn ownLiteralRequirementIndex(self: *Builder, node: u32) ?u32 {
        const literal_requirements: []const checked.LiteralRequirement = if (self.foreign_scopes != null or self.alias_scopes.items.len != 0)
            self.alias_scopes.items[self.alias_scopes.items.len - 1].literal_requirements
        else if (self.currentWorker().literal_scopes.len != 0)
            self.currentWorker().literal_scopes[0]
        else
            &.{};
        for (literal_requirements, 0..) |literal_requirement, index| {
            if (literal_requirement.edge == .literal and literal_requirement.site.node == node and literal_requirement.site.module.eql(self.view.key)) return @intCast(index);
        }
        return null;
    }

    /// The body of compile-time root `root_id`'s entry wrapper, where it
    /// computes the root's value: the root's expression; for a literal
    /// conversion root, the conversion of its literal; or, for a hoisted
    /// pattern root, the construct its `hoisted_body` describes. A rejected
    /// destructure binder's root crashes; an extraction or validation root
    /// matches its base expression against its pattern, crashing where the
    /// pattern does not match (`rootPatternMatch`).
    fn lowerRootBody(self: *Builder, root_id: checked.ComptimeRootId) Allocator.Error!void {
        const root = self.view.view.compile_time_roots.root(root_id);
        if (root.literalNode()) |literal| return switch (literal) {
            .expr => |expr| try self.push(.{ .own_expr = expr }),
            .pattern => |pattern| try self.push(.{ .literal_pattern_conversion = pattern }),
        };
        if (root.evaluatedExpr()) |evaluated| return try self.push(.{ .own_expr = evaluated });
        const body = root.hoisted_body orelse invariant("a compile-time root that evaluates no expression has no hoisted body");
        switch (body) {
            .expr, .valueless_binding => invariant("a hoisted root evaluating its expression reached its pattern construct"),
            .pattern_error => |pattern| {
                try self.push(.{ .restore_region = self.region });
                self.region = self.nodeRegion(@backingInt(pattern));
                try self.push(.{ .runtime_error = if (try self.program.hasError(self.module, root.checked_type)) null else try self.convert(root.checked_type) });
            },
            .pattern_extraction => |extraction| try self.pushRootPatternMatch(root_id, extraction.scrutinee_pattern, extraction.base_expr),
            .pattern_validation => |validation| try self.pushRootPatternMatch(root_id, validation.scrutinee_pattern, validation.base_expr),
        }
    }

    /// Lower a hoisted pattern root's base expression, then match it
    /// against the root's pattern (`rootPatternMatch`).
    fn pushRootPatternMatch(self: *Builder, root_id: checked.ComptimeRootId, scrutinee_pattern: PatternId, base_expr: ExprId) Allocator.Error!void {
        try self.push(.{ .restore_region = self.region });
        self.region = self.nodeRegion(@backingInt(scrutinee_pattern));
        try self.push(.{ .root_pattern_match = root_id });
        try self.push(.{ .own_expr = base_expr });
    }

    /// Pop a hoisted pattern root's base value and match it against the
    /// root's pattern: where it matches, an extraction root's value is its
    /// result binder's, and a validation root's is `{}`; where it does not,
    /// the root crashes.
    fn rootPatternMatch(self: *Builder, root_id: checked.ComptimeRootId) Allocator.Error!void {
        const root = self.view.view.compile_time_roots.root(root_id);
        const body = root.hoisted_body orelse invariant("a hoisted pattern root has no hoisted body");
        const scrutinee_pattern, const base_expr, const result_pattern: ?PatternId = switch (body) {
            .pattern_extraction => |extraction| .{ extraction.scrutinee_pattern, extraction.base_expr, extraction.result_pattern },
            .pattern_validation => |validation| .{ validation.scrutinee_pattern, validation.base_expr, null },
            .expr, .valueless_binding, .pattern_error => invariant("a hoisted root without a pattern match reached its pattern match"),
        };
        const scrutinee = self.popValue();
        if (self.hole == null) {
            try self.pushValue(if (try self.program.hasError(self.module, root.checked_type)) try self.closedLocal(.zst) else try self.localOf(try self.convert(root.checked_type), .frame));
            return;
        }
        // A miss fails as the destructure's checked site reports
        // (`comptimeFailure`).
        const failure = try self.comptimeFailure(self.view.view.exhaustiveness_sites.lookupByDestructurePattern(scrutinee_pattern));
        const miss = try self.store.addCFStmt(Match.ComptimeFailure.terminal(failure), self.origin(.lowering_glue));
        try self.testPattern(scrutinee_pattern, scrutinee, try self.exprType(base_expr), miss);
        if (result_pattern) |pattern| {
            try self.pushValue(self.binders.get(pattern) orelse invariant("a hoisted extraction root's pattern never bound its result binder"));
        } else {
            try self.pushValue(try self.unitValue());
        }
    }

    /// The body of a `literal_argument` root's entry wrapper: the stored
    /// value restored, or, where evaluation stored none (the root is being
    /// evaluated, or its value holds a function), the literal's conversion
    /// at the root's edge.
    pub fn lowerLiteralArgumentRoot(self: *Builder, root_id: checked.ComptimeRootId, argument: checked.LiteralArgumentRoot) Allocator.Error!void {
        const worker = self.currentWorker().*;
        switch (self.view.view.compile_time_roots.root(root_id).payloadFor(.boxy)) {
            .const_node => |node| return try self.push(.{ .restore = .{ .source = .{ .module = self.module }, .node = node, .ty = worker.ret, .root = root_id } }),
            .pending, .unevaluated, .discarded => {},
            inline .fn_value, .expect, .runtime => |_, tag| invariantFmt("a literal argument root holding {s} reached lowering", .{@tagName(tag)}),
        }
        try self.beginLiteralChain();
        const literal_requirements = try self.enterEdge(argument.edge);
        try self.lowerLiteralChain(literal_requirements, argument.callee_requirement, worker.ret);
    }

    /// The body of a literal thunk (`WorkerKey.literal_thunk`): the
    /// worker's scopes entered at the thunk's types, then the literal requirement's
    /// literal converted there.
    pub fn lowerLiteralThunkBody(self: *Builder, key: @FieldType(lower.WorkerKey, "literal_thunk")) Allocator.Error!void {
        const worker = self.currentWorker().*;
        const start = try self.beginErasedWorker(&.{});
        defer self.allocator.free(start.captures);
        self.var_descs = &.{};
        self.dict_locals = &.{};
        self.literal_locals = &.{};
        var ret_layout: layout.Idx = undefined;
        if (try self.closed(worker.ret)) {
            ret_layout = try self.program.layouts.layoutOf(worker.ret);
        } else {
            ret_layout = try self.program.result.layouts.dynamicLayout();
            self.result_local = try self.localOf(worker.ret, .result);
        }
        const body_start = try self.newHole();
        self.hole = body_start;
        const target = self.program.worker(key.target).*;
        const bound = switch (self.program.store.get(key.bound)) {
            .tuple => |items| self.program.store.span(items),
            .primitive, .empty_record, .record, .tag_union, .list, .box, .nominal, .function, .variable => invariant("a literal thunk's types are not a tuple"),
        };
        if (bound.len != target.scheme_vars.len) invariant("a literal thunk's types differ from its target's scheme");
        // The thunk runs in the frames of every scope of the target, the
        // outermost entered first.
        try self.tasks.append(self.allocator, .ret);
        try self.beginLiteralChain();
        const owners = try self.scopeOwners(key.target);
        defer self.allocator.free(owners);
        var index = owners.len;
        while (index > 0) {
            index -= 1;
            const scope_owner = self.program.worker(owners[index]).*;
            try self.enterDerivedFrame(owners[index], bound[0..scope_owner.scheme_vars.len]);
        }
        if (target.literal_scopes.len == 0) invariant("a literal thunk's target has no scope");
        try self.lowerLiteralChain(target.literal_scopes[0], key.literal_requirement, worker.ret);
        try self.run();
        try self.finishErasedProc(start.entry, body_start, &.{}, ret_layout, start.capture_ptr);
    }

    /// The body of a root slot thunk (`WorkerKey.root_slot`): the read of
    /// the root's compile-time value slot (`pendingValueRead`).
    pub fn lowerRootSlotThunkBody(self: *Builder, key: @FieldType(lower.WorkerKey, "root_slot")) Allocator.Error!void {
        const worker = self.currentWorker().*;
        const start = try self.beginErasedWorker(&.{});
        defer self.allocator.free(start.captures);
        self.var_descs = &.{};
        self.dict_locals = &.{};
        self.literal_locals = &.{};
        const ret_layout = try self.program.layouts.layoutOf(worker.ret);
        const body_start = try self.newHole();
        self.hole = body_start;
        try self.tasks.append(self.allocator, .ret);
        try self.pendingValueRead(key.module, key.root, worker.ret);
        try self.run();
        try self.finishErasedProc(start.entry, body_start, &.{}, ret_layout, start.capture_ptr);
    }

    /// Begin a literal chain: the frames it enters are left once the
    /// pushed tasks have run (`restore_scope`), and an edge argument naming
    /// a scope outside them is unreachable.
    fn beginLiteralChain(self: *Builder) Allocator.Error!void {
        try self.push(.{ .restore_scope = .{
            .module = self.module,
            .view = self.view,
            .env = self.env,
            .foreign_scopes = self.foreign_scopes,
            .thunk_frames = self.thunk_frames,
            .alias_scopes = self.alias_scopes.items.len,
        } });
        self.thunk_frames = self.alias_scopes.items.len;
        self.foreign_scopes = self.alias_scopes.items.len;
    }

    /// Lower the conversion of literal requirement `index` of the innermost entered
    /// frame (`literal requirements`), following forwarded literal requirements into the
    /// frames of the schemes their edges instantiate, as the value of
    /// `result_ty`.
    fn lowerLiteralChain(self: *Builder, first: []const checked.LiteralRequirement, first_index: u32, result_ty: TypeId) Allocator.Error!void {
        var literal_requirements = first;
        var index = first_index;
        while (true) {
            if (index >= literal_requirements.len) invariant("a literal argument names an literal_requirement its scheme does not have");
            const literal_requirement = literal_requirements[index];
            switch (literal_requirement.edge) {
                .literal => {
                    if (!literal_requirement.site.module.eql(self.view.key)) invariant("a literal literal_requirement's literal is in another module than its scheme");
                    const node = literal_requirement.site.node;
                    // Forwarding can enter an imported scheme whose linked
                    // worker needed no body index. Reading its literal does.
                    try self.bodies().demandIndex();
                    // A custom literal pattern's literal converts at the
                    // pattern's type.
                    if (self.bodies().index().nodeKind(node) == .pattern) {
                        const conversion = self.bodies().literalConversion(node);
                        const kind: LIR.LiteralRejectionKind = switch (conversion.literal) {
                            .numeral => .numeral,
                            .quote => .quote,
                        };
                        return try self.lowerLiteralConversion(node, kind, conversion.plan orelse invariant("a literal literal_requirement's pattern literal has no conversion plan"), result_ty);
                    }
                    return switch (self.bodies().exprData(@fromBackingInt(node))) {
                        .numeral => try self.lowerLiteralConversion(node, .numeral, self.bodies().numeralPlan(node) orelse invariant("a literal literal_requirement's numeral has no conversion plan"), result_ty),
                        .str => try self.lowerLiteralConversion(node, .quote, self.bodies().quotePlan(node) orelse invariant("a literal literal_requirement's quote has no conversion plan"), result_ty),
                        .interpolation => try self.lowerLiteralConversion(node, .interpolation, self.bodies().dispatchPlan(node) orelse invariant("a literal literal_requirement's interpolation has no conversion plan"), result_ty),
                        .str_segment, .bytes_literal, .lookup_local, .lookup_external, .lookup_required, .list, .empty_list, .tuple, .match_, .if_, .call, .record, .empty_record, .block, .tag, .nominal, .zero_argument_tag, .closure, .lambda, .binop, .unary_minus, .field_access, .dispatch_call, .structural_eq, .structural_hash, .method_eq, .type_dispatch_call, .tuple_access, .runtime_error, .crash, .dbg, .expect_err, .expect, .ellipsis, .anno_only, .break_, .return_, .for_, .hosted_lambda, .run_low_level => invariant("a literal literal_requirement's literal is not a literal expression"),
                    };
                },
                .node, .use => {
                    literal_requirements = try self.enterEdge(literal_requirement.edge);
                    index = literal_requirement.callee_requirement;
                },
            }
        }
    }

    /// Enter the frame of the scheme the edge `edge` of this body's module
    /// instantiates, at the edge's instantiation: the scheme's module,
    /// variables, dictionaries, and literal arguments become this body's
    /// innermost scope. Returns the scheme's literal requirements.
    fn enterEdge(self: *Builder, edge: checked.LiteralEdge) Allocator.Error![]const checked.LiteralRequirement {
        switch (edge) {
            .literal => invariant("a literal literal_requirement's own literal is no edge"),
            .node => |node_id| {
                const table = self.view.view.static_dispatch_plans;
                const node = table.evidenceNode(node_id);
                const procedure = switch (node.target.kind) {
                    .procedure => |procedure| procedure,
                    .local_proc, .structural, .callable_binding => invariant("a literal argument's edge instantiates no procedure template"),
                };
                const callee = try self.program.templateRefWorker(procedure.template);
                const subst = table.site_substitutions[node.subst.start..][0..node.subst.len];
                var deferred = false;
                self.chain_frame = true;
                defer self.chain_frame = false;
                const instance = switch (node.instantiation) {
                    .alias => try self.aliasTargetInstance(node, callee, edge),
                    .monomorphic, .callable, .derived_from_callable => switch (node.nested) {
                        .resolved => blk: {
                            deferred = true;
                            break :blk try self.callInstanceAt(callee, subst, table.nestedEvidence(node), try self.instantiationCallable(node.instantiation), edge);
                        },
                        .from_callable => invariant("callable-derived evidence carries no substitution, so checking records no literal edge for it"),
                        .through_alias => invariant("alias-scope evidence accompanies only an alias instantiation"),
                    },
                };
                defer self.allocator.free(instance.hidden);
                return try self.enterWorkerFrame(callee, instance, if (deferred) edge else null);
            },
            .use => |expr| {
                if (self.view.view.resolved_value_refs.lookupIdByExpr(expr)) |ref_id| {
                    const record = self.view.view.resolved_value_refs.records[@backingInt(ref_id)];
                    switch (record.ref) {
                        .local_proc => |local| {
                            const scope_id = local.dispatch_scope orelse invariant("a literal argument's edge uses a local procedure without a scheme");
                            return try self.enterLocalScopeFrame(scope_id, expr);
                        },
                        inline .top_level_proc, .imported_proc, .hosted_proc, .promoted_top_level_proc, .platform_required_proc => |ref, tag| {
                            const use = if (tag == .platform_required_proc) ref.procedure else ref;
                            const callee = switch (try self.procedureUseCallee(use)) {
                                .worker => |worker_id| worker_id,
                                .alias => |alias| {
                                    if (tag == .platform_required_proc) invariant("a platform requirement's use is no literal argument edge");
                                    return try self.enterAliasFrame(alias, expr);
                                },
                                .callable_eval => invariant("a literal argument's edge uses a computed callable"),
                            };
                            const use_ty = try self.convert(self.bodies().exprType(expr));
                            self.chain_frame = true;
                            defer self.chain_frame = false;
                            const instance = if (tag == .platform_required_proc)
                                try self.requiredInstanceAt(callee, ref, self.siteSubstitution(expr), use_ty, edge)
                            else
                                try self.callInstanceAt(callee, self.siteSubstitution(expr), self.view.view.static_dispatch_plans.siteEvidence(expr) orelse &.{}, use_ty, edge);
                            defer self.allocator.free(instance.hidden);
                            return try self.enterWorkerFrame(callee, instance, edge);
                        },
                        .local_param, .local_value, .local_mutable_version, .pattern_binder, .selected_hoisted_const, .top_level_const, .imported_const, .platform_required_const, .platform_required_declaration, .platform_required_checked_error => invariant("a literal argument's edge uses no procedure"),
                    }
                }
                // A generalized local function's construction: the scope
                // the function owns.
                for (self.view.view.checked_procedure_templates.dispatch_scopes, 0..) |scope, scope_index| {
                    if (scope.checked_expr == expr) return try self.enterLocalScopeFrame(@fromBackingInt(@intCast(scope_index)), expr);
                }
                invariant("a literal argument's edge is neither a use nor a construction");
            },
        }
    }

    /// Enter the frame of the template worker `callee` called with
    /// `instance`.
    fn enterWorkerFrame(self: *Builder, callee: WorkerId, instance: CallInstance, deferred_edge: ?checked.LiteralEdge) Allocator.Error![]const checked.LiteralRequirement {
        const target = self.program.worker(callee).*;
        const arena = self.program.arena.allocator();
        const dicts: []const Word = if (instance.deferred_dicts != null or target.dictionary_params.len == 0) &.{} else instance.hidden[instance.bound.len..][0..target.dictionary_params.len];
        const literals = instance.hidden[instance.bound.len + target.dictionary_params.len ..];
        if (literals.len != target.literal_params) invariant("a call instance's literal arguments differ from its target's literal_requirements");
        const literal_requirements: []const checked.LiteralRequirement = if (target.literal_scopes.len == 0) &.{} else target.literal_scopes[0];
        const deferred: ?DeferredLiterals = if (deferred_edge) |edge| blk: {
            const args = self.view.view.checked_procedure_templates.literalEdgeArgs(edge);
            if (args.len != literals.len) invariant("an edge's literal arguments do not cover its callee's literal requirements");
            break :blk .{ .edge = .{ .args = args, .module = self.module, .view = self.view, .env = self.env } };
        } else null;
        self.module = target.module;
        self.view = self.program.module(target.module);
        self.env = .{ .module = target.module, .vars = target.scheme_vars, .bound = instance.bound };
        try self.alias_scopes.append(self.allocator, .{
            .params = target.dictionary_params,
            .dicts = try arena.dupe(Word, dicts),
            .literals = if (deferred == null) try arena.dupe(Word, literals) else &.{},
            .literal_requirements = literal_requirements,
            .deferred = deferred,
            .deferred_dicts = instance.deferred_dicts,
            .saved_env = self.env,
        });
        return literal_requirements;
    }

    /// Enter the scope of exact procedure alias `alias` as the site `site`
    /// of this body's module instantiates it. Its literal requirements are
    /// those its forwarded lookup's scheme gives it.
    fn enterAliasFrame(self: *Builder, alias: AliasCallee, site: ExprId) Allocator.Error![]const checked.LiteralRequirement {
        const alias_step = self.callableEvalAliasStep(alias.module, alias.template);
        const bound = try self.aliasScopeBound(alias_step, self.siteSubstitution(site), try self.exprType(site));
        const evidence = self.view.view.static_dispatch_plans.siteEvidence(site) orelse &.{};
        const env: types.Env = .{ .module = alias_step.module, .vars = alias_step.vars, .bound = bound };
        const deferred_dicts = try self.deferredDictionaries(evidence, .{ .module = alias_step.module, .params = alias_step.params, .env = env }, alias_step.params.len);
        const args = self.view.view.checked_procedure_templates.literalEdgeArgs(.{ .use = site });
        if (args.len != alias_step.literal_requirements.len) invariant("an alias's use has no literal arguments for its literal_requirements");
        const deferred: DeferredLiterals = .{ .edge = .{ .args = args, .module = self.module, .view = self.view, .env = self.env } };
        self.module = alias_step.module;
        self.view = self.program.module(alias_step.module);
        self.env = env;
        try self.alias_scopes.append(self.allocator, .{
            .params = try self.runtimeParams(alias_step.params),
            .dicts = &.{},
            .literal_requirements = alias_step.literal_requirements,
            .deferred = deferred,
            .deferred_dicts = deferred_dicts,
            .saved_env = self.env,
        });
        return alias_step.literal_requirements;
    }

    /// Enter the frame of the generalized local scope `scope_id` of this
    /// body's module as the site `site` instantiates it.
    fn enterLocalScopeFrame(self: *Builder, scope_id: checked.DispatchScopeId, site: ExprId) Allocator.Error![]const checked.LiteralRequirement {
        const arena = self.program.arena.allocator();
        const scheme = try self.localScheme(scope_id);
        const subst = self.siteSubstitution(site);
        if (subst.len != 0 and subst.len != scheme.scheme_vars.len) invariant("a local scope's site substitution differs from its scheme");
        const own_types = try arena.alloc(TypeId, scheme.scheme_vars.len);
        for (own_types, scheme.scheme_vars, 0..) |*own_ty, variable, index| {
            own_ty.* = if (index < subst.len and self.view.types().payload(subst[index]) != .err)
                try self.convert(subst[index])
            else
                try self.program.converter().sealedScheme(self.module, variable);
        }
        const context = try self.scopeParamContext(scope_id, scheme.scheme_vars, own_types);
        const evidence = self.view.view.static_dispatch_plans.siteEvidence(site) orelse &.{};
        const deferred_dicts = try self.deferredDictionaries(evidence, context, scheme.dictionaries.len);
        const args = self.view.view.checked_procedure_templates.literalEdgeArgs(.{ .use = site });
        if (args.len != scheme.literal_requirements.len) invariant("a local scope's site has no literal arguments for its literal_requirements");
        const deferred: DeferredLiterals = .{ .edge = .{ .args = args, .module = self.module, .view = self.view, .env = self.env } };
        self.env = context.env;
        try self.alias_scopes.append(self.allocator, .{
            .params = scheme.dictionaries,
            .dicts = &.{},
            .literal_requirements = scheme.literal_requirements,
            .deferred = deferred,
            .deferred_dicts = deferred_dicts,
            .saved_env = self.env,
        });
        return scheme.literal_requirements;
    }

    /// Enter the own scope of `owner` (a worker of a literal thunk's target
    /// chain) at the types `bound`, its dictionaries and literal arguments
    /// derived from those types.
    fn enterDerivedFrame(self: *Builder, owner: WorkerId, bound: []const TypeId) Allocator.Error!void {
        const worker = self.program.worker(owner).*;
        self.module = worker.module;
        self.view = self.program.module(worker.module);
        self.env = .{ .module = worker.module, .vars = worker.scheme_vars, .bound = bound };
        const literal_requirements: []const checked.LiteralRequirement = if (worker.literal_scopes.len == 0) &.{} else worker.literal_scopes[0];
        const params: []const u32 = if (worker.dict_scopes.len == 0) &.{} else worker.dict_scopes[0];
        const records: []const static_dispatch.EvidenceParamRecord = if (worker.dict_scope_params.len == 0) &.{} else worker.dict_scope_params[0];
        try self.alias_scopes.append(self.allocator, .{
            .params = params,
            .dicts = &.{},
            .literal_requirements = literal_requirements,
            .deferred = .{ .thunks = .{ .owner = owner, .bound = bound } },
            .deferred_dicts = try self.deferredDictionaries(&.{}, .{ .module = worker.module, .params = records, .env = self.env }, params.len),
            .saved_env = self.env,
        });
    }

    pub fn restoreConversionRoot(self: *Builder, root_id: checked.ComptimeRootId, at: LiteralAt, kind: LIR.LiteralRejectionKind, plan_id: ?static_dispatch.StaticDispatchPlanId) Allocator.Error!void {
        switch (self.view.view.compile_time_roots.root(root_id).payloadFor(.boxy)) {
            .const_node => |node| try self.push(.{ .restore = .{ .source = .{ .module = self.module }, .node = node, .ty = try self.literalType(at), .root = root_id } }),
            // The literal converts where it is reached: as the root's own
            // evaluation, or at a site no evaluation converts (a literal in
            // a branch). Any other body of the compile-time program
            // evaluating the root reads the value from the root's slot once
            // it completes.
            .pending, .unevaluated => if ((self.own_root != null and self.own_root.? == root_id) or !self.program.evaluatesRoot(self.module, root_id))
                try self.lowerLiteralConversion(at.node(), kind, plan_id orelse invariant("a literal with a conversion root has no conversion plan"), try self.literalType(at))
            else
                try self.pendingValueRead(self.module, root_id, try self.literalType(at)),
            inline .fn_value, .discarded, .expect, .runtime => |_, tag| invariantFmt("a literal conversion root holding {s} reached runtime lowering", .{@tagName(tag)}),
        }
    }

    pub fn lowerInterpolation(self: *Builder, expr_id: ExprId) Allocator.Error!void {
        const plan_id = self.bodies().dispatchPlan(@backingInt(expr_id)) orelse invariant("an interpolation has no checked conversion plan");
        const plan = self.dispatchPlan(plan_id);
        switch (plan.resolution) {
            .direct_closed, .direct_parametric, .evidence_dependent => {},
            .checked_error => {
                try self.emitDispatchCrash(.checked_error);
                return try self.pushValue(try self.localOf(try self.exprType(expr_id), .frame));
            },
            .@"unreachable" => {
                try self.emitDispatchCrash(.unreachable_value);
                return try self.pushValue(try self.localOf(try self.exprType(expr_id), .frame));
            },
            .structural, .direct_pending => invariant("an interpolation's conversion plan has no callable resolution"),
        }
        // A `Str` literal: its segments and values are written once by
        // `str_from_parts`, exactly as a literal the checker already proved
        // to be `Str` is, and the assembler is not called.
        const dispatcher_is_str = switch (self.program.store.get(try self.convert(plan.dispatcher_ty))) {
            .primitive => |primitive| primitive == .str,
            .empty_record, .record, .tuple, .tag_union, .list, .box, .nominal, .function, .variable => false,
        };
        if (dispatcher_is_str) {
            const interpolation = self.bodies().exprData(expr_id).interpolation;
            try self.push(.{ .finish_str_from_parts = .{ .expr = expr_id, .count = @intCast(interpolation.valueCount()), .result = try self.exprType(expr_id) } });
            var index = interpolation.valueCount();
            while (index > 0) {
                index -= 1;
                try self.push(.{ .expr = interpolation.value(index) });
            }
            return;
        }
        const try_ty = switch (self.program.store.get(try self.dispatchCallable(plan.resolution, plan.callable_ty))) {
            .function => |function| function.ret,
            .primitive, .empty_record, .record, .tuple, .tag_union, .list, .box, .nominal, .variable => invariant("an interpolation's conversion type is not a function type"),
        };
        // The conversion produces the assembler: the interpolation's literal
        // argument, or, where its type is fixed, its conversion root's
        // value (design.md "Literal Arguments").
        const assembler_ty = try self.tryOkPayload(try_ty);
        try self.push(.{ .finish_interpolation = expr_id });
        switch (plan.resolution) {
            .direct_closed => try self.lowerLiteralConversion(@backingInt(expr_id), .interpolation, plan_id, assembler_ty),
            .direct_parametric, .evidence_dependent => try self.lowerInstanceLiteral(@backingInt(expr_id), assembler_ty),
            .checked_error, .@"unreachable", .structural, .direct_pending => unreachable, // handled above
        }
    }

    /// The `Ok` payload type of the `Try` type `try_ty`.
    fn tryOkPayload(self: *Builder, try_ty: TypeId) Allocator.Error!TypeId {
        const store = &self.program.store;
        const ok = try store.internName("Ok");
        const union_ty = switch (store.get(try self.structuralType(try_ty))) {
            .tag_union => |union_ty| union_ty,
            .primitive, .empty_record, .record, .tuple, .list, .box, .nominal, .function, .variable => invariant("a conversion's Try type is not a tag union"),
        };
        for (store.tagSpan(union_ty.tags)) |tag| {
            if (tag.name == ok and tag.payloads.len == 1) return store.span(tag.payloads)[0];
        }
        invariant("a conversion's Try type has no Ok payload");
    }

    pub fn finishInterpolation(self: *Builder, expr_id: ExprId) Allocator.Error!void {
        const assembler = self.popValue();
        const interpolation = self.bodies().exprData(expr_id).interpolation;
        const assembler_ty = try self.convert(self.bodies().interpolationAssemblerType(expr_id));
        const plan = self.bodies().iteratorForPlan(@backingInt(expr_id)) orelse invariant("an interpolation has no checked iterator plan");
        // The values are lowered after the assembler, in order.
        try self.push(.{ .finish_interpolation_call = .{ .assembler = assembler, .assembler_ty = assembler_ty, .plan = plan, .count = @intCast(interpolation.valueCount()), .result = try self.exprType(expr_id) } });
        var index = interpolation.valueCount();
        while (index > 0) {
            index -= 1;
            try self.push(.{ .expr = interpolation.value(index) });
        }
    }

    /// The `Numeral` value `from_numeral` receives for `literal`, at the
    /// argument type `ty`: `Literal({ is_negative, digits_before_pt,
    /// digits_after_pt, digits_after_pt_count })`.
    pub fn numeralArgument(self: *Builder, literal: NumeralLiteral, ty: TypeId) Allocator.Error!LocalId {
        const store = &self.program.store;
        const backing = try self.structuralType(ty);
        const name = try store.internName("Literal");
        const variant = try self.closedVariant(backing, name);
        if (variant.payloads.len != 1) invariant("the Numeral Literal tag does not carry one record");
        const record_ty = try self.structuralType(variant.payloads[0]);
        const fields = switch (store.get(record_ty)) {
            .record => |record| try self.allocator.dupe(types.Field, store.fieldSpan(record.fields)),
            .primitive, .empty_record, .tuple, .tag_union, .list, .box, .nominal, .function, .variable => invariant("the Numeral Literal payload is not a record"),
        };
        defer self.allocator.free(fields);
        const env = self.view.view.module_env;
        const values = try self.allocator.alloc(LocalId, fields.len);
        defer self.allocator.free(values);
        for (fields, values) |field, *value| {
            const label = store.nameText(field.name);
            value.* = if (std.mem.eql(u8, label, "is_negative"))
                try self.boolValue(literal.isNegative())
            else if (std.mem.eql(u8, label, "digits_before_pt"))
                try self.byteList(env.numeralDigitsBefore(literal), field.ty)
            else if (std.mem.eql(u8, label, "digits_after_pt"))
                try self.byteList(env.numeralDigitsAfter(literal), field.ty)
            else if (std.mem.eql(u8, label, "digits_after_pt_count"))
                try self.intValue(literal.after_decimal_digit_count, field.ty)
            else
                invariant("the Numeral record has an unexpected field");
        }
        const record = try self.buildStruct(record_ty, values);
        return try self.buildTag(backing, name, &.{record});
    }

    /// A `List(U8)` (at `ty`) of `bytes`.
    pub fn byteList(self: *Builder, bytes: []const u8, ty: TypeId) Allocator.Error!LocalId {
        const elems = try self.allocator.alloc(LocalId, bytes.len);
        defer self.allocator.free(elems);
        for (bytes, elems) |byte, *elem| {
            elem.* = try self.closedLocal(.u8);
            try self.emitLinear(.{ .assign_literal = .{ .target = elem.*, .value = .{ .i64_literal = .{ .value = byte, .layout_idx = .u8 } }, .next = undefined } });
        }
        const list = try self.localOf(ty, .frame);
        try self.emitLinear(.{ .assign_list = .{ .target = list, .elems = try self.store.addLocalSpan(elems), .next = undefined } });
        return list;
    }

    /// An integer of the builtin integer type `ty`.
    pub fn intValue(self: *Builder, value: u64, ty: TypeId) Allocator.Error!LocalId {
        const primitive = switch (self.program.store.get(ty)) {
            .primitive => |primitive| primitive,
            .empty_record, .record, .tuple, .tag_union, .list, .box, .nominal, .function, .variable => invariant("an integer field's type is not a primitive"),
        };
        const prim_layout = @import("layouts.zig").primitiveLayout(primitive);
        const local = try self.closedLocal(prim_layout);
        const literal: LIR.LiteralValue = switch (primitive) {
            .u128, .i128 => .{ .i128_literal = try self.store.addStmtRow(LIR.I128Literal, .{ .value = value, .layout_idx = prim_layout }) },
            .bool, .str, .u8, .i8, .u16, .i16, .u32, .i32, .u64, .i64, .f32, .f64, .dec, .u8x16, .i8x16, .u16x8, .i16x8, .u32x4, .i32x4, .u64x2, .i64x2 => .{ .i64_literal = .{ .value = @bitCast(value), .layout_idx = prim_layout } },
        };
        try self.emitLinear(.{ .assign_literal = .{ .target = local, .value = literal, .next = undefined } });
        return local;
    }

    pub fn resolvedRef(self: *const Builder, id: checked.ResolvedValueRefId) checked.ResolvedValueRefRecord {
        return self.view.view.resolved_value_refs.records[@backingInt(id)];
    }

    pub fn lowerLookupLocal(self: *Builder, expr_id: ExprId, pattern: PatternId, resolved: ?checked.ResolvedValueRefId) Allocator.Error!void {
        if (resolved) |id| return try self.lowerResolvedValue(expr_id, self.resolvedRef(id).ref);
        try self.pushValue(self.binders.get(pattern) orelse invariant("a local lookup named a binder this body never bound"));
    }

    pub fn lowerLookupExternal(self: *Builder, expr_id: ExprId, resolved: checked.ResolvedValueRefId) Allocator.Error!void {
        try self.lowerResolvedValue(expr_id, self.resolvedRef(resolved).ref);
    }

    /// The value a lookup `expr_id` resolved to `ref` reads.
    fn lowerResolvedValue(self: *Builder, expr_id: ExprId, ref: checked.ResolvedValueRef) Allocator.Error!void {
        switch (ref) {
            .local_param, .local_value, .pattern_binder, .local_mutable_version => |binding| {
                return try self.pushValue(self.binders.get(binding.binder) orelse invariant("a local lookup named a binder this body never bound"));
            },
            .top_level_const, .imported_const => |use| return try self.restoreConstUse(expr_id, use, try self.constReadType(expr_id)),
            // A local binding compile-time evaluation hoisted is read as the
            // constant it stored; the root computing that constant reads
            // the binding its own body bound.
            .selected_hoisted_const => |selected| {
                if (self.own_root) |own| if (self.hoistedRootOfConstRef(selected.const_use.const_ref) == own) {
                    return try self.pushValue(self.binders.get(selected.local.binder) orelse invariant("a hoisted root's own binding was read before its body bound it"));
                };
                return try self.restoreConstUse(expr_id, selected.const_use, try self.constReadType(expr_id));
            },
            .local_proc => |local| {
                if (local.is_alias) return try self.lowerAliasUse(expr_id, local);
                return try self.pushValue(try self.localProcUseValue(expr_id, local));
            },
            .top_level_proc, .imported_proc, .hosted_proc, .promoted_top_level_proc => |use| return try self.procedureValue(expr_id, use, null),
            .platform_required_proc => |required| return try self.procedureValue(expr_id, required.procedure, required),
            .platform_required_const => |required| return try self.restoreConstUse(expr_id, required.const_use, try self.constReadType(expr_id)),
            .platform_required_checked_error => return try self.crashValue("runtime error", try self.exprType(expr_id)),
            .platform_required_declaration => invariant("a platform requirement reached lowering unpaired with its app binding"),
        }
    }

    /// A use `expr_id` of a scheme alias: the value its declaration bound,
    /// or, for an alias with a generalized scheme, its right-hand side
    /// lowered in the alias's scope as this use instantiates it (its
    /// variables bound to the use's types, its dictionaries to the use's
    /// evidence).
    fn lowerAliasUse(self: *Builder, expr_id: ExprId, local: checked.LocalProcedureBinding) Allocator.Error!void {
        if (self.binders.get(local.binder)) |value| return try self.pushValue(value);
        const scope_id = local.dispatch_scope orelse invariant("an alias without a generalized scheme is bound where it is declared");
        const scheme = try self.localScheme(scope_id);
        const outer_count = self.env.vars.len;
        const vars = try std.mem.concat(self.program.arena.allocator(), checked.CheckedTypeId, &.{ scheme.scheme_vars, self.env.vars });
        const generic_bound = try self.allocator.alloc(TypeId, vars.len);
        defer self.allocator.free(generic_bound);
        for (generic_bound[0..scheme.scheme_vars.len], 0..) |*slot, index| slot.* = try self.program.store.variable(@intCast(outer_count + index));
        for (generic_bound[scheme.scheme_vars.len..], 0..) |*slot, index| slot.* = try self.program.store.variable(@intCast(index));
        const alias_ty = self.bodies().exprType(local.expr);
        const generic_fn = try self.program.converter().convert(self.module, alias_ty, .{ .module = self.module, .vars = vars, .bound = generic_bound });
        const related = try self.relateVariablesPartial(&.{.{ generic_fn, try self.exprType(expr_id) }}, outer_count + scheme.scheme_vars.len);
        defer self.allocator.free(related);
        const bound = try self.program.arena.allocator().alloc(TypeId, vars.len);
        // A variable the use's type does not relate takes the use's checked
        // instantiation of it, or, the checker having fixed none, seals as a
        // variable no use relates.
        const subst = self.siteSubstitution(expr_id);
        for (bound[0..scheme.scheme_vars.len], related[outer_count..], scheme.scheme_vars, 0..) |*slot, ty, variable, index| slot.* = ty orelse
            if (index < subst.len and self.view.types().payload(subst[index]) != .err)
                try self.convert(subst[index])
            else
                try self.program.converter().sealedScheme(self.module, variable);
        @memcpy(bound[scheme.scheme_vars.len..], self.env.bound);
        const evidence = self.view.view.static_dispatch_plans.siteEvidence(expr_id) orelse &.{};
        const dicts = try self.program.arena.allocator().alloc(Word, scheme.dictionaries.len);
        const context: ParamContext = .{ .module = self.module, .params = self.scopeParams(scope_id), .env = .{ .module = self.module, .vars = vars, .bound = bound } };
        for (scheme.dictionaries, dicts) |param_index, *dict| {
            if (param_index >= evidence.len) invariant("an alias use's evidence does not cover its dictionaries");
            dict.* = try self.paramDictionary(evidence, param_index, context);
        }
        const literals = try self.program.arena.allocator().alloc(Word, scheme.literal_requirements.len);
        const literal_args = self.view.view.checked_procedure_templates.literalEdgeArgs(.{ .use = expr_id });
        if (literal_args.len != literals.len) invariant("an alias use has no literal arguments for its scheme's literal_requirements");
        for (literal_args, literals) |source, *word| word.* = try self.literalArg(source);
        try self.alias_scopes.append(self.allocator, .{ .params = scheme.dictionaries, .dicts = dicts, .literals = literals, .literal_requirements = scheme.literal_requirements, .saved_env = self.env });
        self.env = .{ .module = self.module, .vars = vars, .bound = bound };
        try self.push(.leave_alias_scope);
        try self.push(.{ .own_expr = local.expr });
    }

    /// The function value a use `expr_id` of a local procedure denotes. A
    /// generalized local procedure whose calls pass its own scheme's
    /// instantiation is, as a value, instantiated where it is used: the
    /// value captures the use's descriptors and dictionaries of its scheme.
    pub fn localProcUseValue(self: *Builder, expr_id: ExprId, local: checked.LocalProcedureBinding) Allocator.Error!LocalId {
        // A use checking recorded no evidence for (one inside a rejected
        // expression, evaluated only for its effects) resolves the
        // procedure's dictionaries from its types.
        return try self.localProcInstanceValue(local, try self.exprType(expr_id), self.view.view.static_dispatch_plans.siteEvidence(expr_id), .{ .use = expr_id }, self.siteSubstitution(expr_id), null);
    }

    /// What the function value of a local procedure at one use packs: the
    /// procedure's worker, the values it captures and their types, then the
    /// hidden words it captures (descriptors, then dictionaries and literal
    /// arguments). A generalized procedure instantiated at the use has the
    /// procedure's type there, `own_ty`, which the use's own type `use_ty`
    /// may store differently.
    pub const LocalProcClosure = struct {
        callee: WorkerId,
        values: []const LocalId,
        capture_types: []const TypeId,
        descs: []const Word,
        words: []const Word,
        adapt: ?struct { own_ty: TypeId, use_ty: TypeId } = null,
        /// Static methods capture only other checked local procedures. Their
        /// edges are traversed after reserving the callable, never as LIR values.
        static_scope: ?LocalProcScope = null,

        /// The callee's inputs: the descriptors, then the other words.
        /// Caller frees.
        pub fn inputs(self: LocalProcClosure, allocator: Allocator) Allocator.Error![]Word {
            return try std.mem.concat(allocator, Word, &.{ self.descs, self.words });
        }
    };

    const StaticCallable = struct {
        closure: LocalProcClosure,
        inputs: []const Word,
        recipe: Tables.RecipeId,
    };

    /// The function value of local procedure `local` used at the function
    /// type `use_ty`, whose own scheme's dictionaries `evidence` supplies
    /// (or, when null, each dispatcher's type resolves). `edge` is the
    /// checked edge the use is (design.md "Literal Arguments"), whose literal
    /// arguments the procedure's own scheme's literal requirements take;
    /// null derives them from the use's types, as the dictionaries are
    /// derived when `evidence` is null. `subst` is the use's checked
    /// instantiation of the procedure's scheme, when it recorded one, for
    /// the variables its type does not relate. `site` owns the evidence and
    /// substitution when a dispatch selects an imported body-local method.
    pub fn localProcInstanceValue(self: *Builder, local: checked.LocalProcedureBinding, use_ty: TypeId, evidence: ?[]const static_dispatch.CheckedEvidence, edge: ?checked.LiteralEdge, subst: []const checked.CheckedTypeId, site: ?*const types.ModuleView) Allocator.Error!LocalId {
        const closure = (try self.localProcClosureAt(local, use_ty, evidence, edge, subst, site, false)) orelse return try self.localProcValue(local);
        const inputs = try closure.inputs(self.allocator);
        defer self.allocator.free(inputs);
        const packed_closure = if (try self.allClosed(closure.capture_types))
            try self.packClosure(closure.callee, closure.values, closure.capture_types, inputs)
        else
            try self.packDynamicClosure(closure.callee, closure.values, closure.capture_types, inputs);
        const adapt = closure.adapt orelse return packed_closure;
        return try self.adaptValue(packed_closure, adapt.own_ty, adapt.use_ty);
    }

    /// The function value of local procedure `local` at the function type
    /// `use_ty` as an immortal callable: a method declared in a function
    /// body, which captures no values, as a descriptor's override or a
    /// dictionary holds it. `localProcInstanceValue` gives its arguments.
    pub fn localProcWord(self: *Builder, local: checked.LocalProcedureBinding, use_ty: TypeId, evidence: ?[]const static_dispatch.CheckedEvidence, edge: ?checked.LiteralEdge, subst: []const checked.CheckedTypeId, site: ?*const types.ModuleView) Allocator.Error!Word {
        const closure = (try self.localProcClosureAt(local, use_ty, evidence, edge, subst, site, true)) orelse try self.localProcClosureWithCaptures(local, true);
        return .{ .recipe = try self.immortalRecipe(closure) };
    }

    /// The immortal callable recipe of `closure`, adapted to its use's type.
    fn immortalRecipe(self: *Builder, closure: LocalProcClosure) Allocator.Error!Tables.RecipeId {
        if (closure.values.len != 0) invariant("an immortal function value captures a value, which methods never do");
        const scope = closure.static_scope orelse invariant("a static callable has no producer capture context");
        const arena = self.program.arena.allocator();
        const inputs = try closure.inputs(arena);
        for (self.static_callables.items) |active| {
            if (active.closure.callee != closure.callee or active.closure.static_scope.?.declaration != scope.declaration or
                !std.meta.eql(active.closure.adapt, closure.adapt) or !std.mem.eql(TypeId, active.closure.capture_types, closure.capture_types) or
                active.inputs.len != inputs.len) continue;
            const same_inputs = for (active.inputs, inputs) |a, b| {
                if (!std.meta.eql(a, b)) break false;
            } else true;
            if (same_inputs) return active.recipe;
        }
        const reserved = try self.recipes.reserve();
        try self.static_callables.append(self.allocator, .{ .closure = closure, .inputs = inputs, .recipe = reserved });
        defer _ = self.static_callables.pop();
        const worker = self.program.worker(closure.callee).*;
        const has_table = !self.program.perCall(closure.callee) and inputs.len != 0;
        const captures = try arena.alloc(Word, worker.captures.len + @intFromBool(has_table));
        for (worker.captures, closure.capture_types, captures[0..worker.captures.len]) |capture, ty, *word| {
            const local = (try self.program.localProcBinding(self.module, capture.pattern)) orelse
                invariant("an immortal local method captures invocation-specific data rather than a local procedure");
            const use_ty = try self.program.substitute(self.allocator, ty, scope.bound);
            word.* = try self.localProcWord(local, use_ty, null, null, &.{}, null);
        }
        if (has_table) captures[worker.captures.len] = try self.tableWord(closure.callee, inputs);
        const callable = try self.recipes.intern(.{ .callable = .{ .entry = closure.callee, .captures = captures } });
        const result = if (closure.adapt) |adapt| switch (try self.adaptDictionary(.{ .recipe = callable }, adapt.own_ty, adapt.use_ty)) {
            .recipe => |id| id,
            .static => invariant("an adapted recipe became static"),
        } else callable;
        self.recipes.setSame(reserved, result);
        return reserved;
    }

    /// `localProcInstanceValue`'s closure for a generalized local procedure
    /// whose calls pass its own scheme's instantiation, instantiated at the
    /// use; null for any other local procedure, whose function value is the
    /// one its declaration bound (`localProcClosure`).
    fn localProcClosureAt(self: *Builder, local: checked.LocalProcedureBinding, use_ty: TypeId, evidence: ?[]const static_dispatch.CheckedEvidence, edge: ?checked.LiteralEdge, subst: []const checked.CheckedTypeId, site: ?*const types.ModuleView, comptime static_captures: bool) Allocator.Error!?LocalProcClosure {
        const arena = self.program.arena.allocator();
        const lambda_expr = switch (self.bodies().exprData(local.expr)) {
            .lambda => local.expr,
            .closure => |closure| closure.lambda,
            .numeral, .str_segment, .str, .bytes_literal, .lookup_local, .lookup_external, .lookup_required, .list, .empty_list, .tuple, .match_, .if_, .call, .record, .empty_record, .block, .tag, .nominal, .zero_argument_tag, .binop, .unary_minus, .field_access, .dispatch_call, .interpolation, .structural_eq, .structural_hash, .method_eq, .type_dispatch_call, .tuple_access, .runtime_error, .crash, .dbg, .expect_err, .expect, .ellipsis, .anno_only, .break_, .return_, .for_, .hosted_lambda, .run_low_level => invariant("a local procedure's expression is not a lambda or closure"),
        };
        const scope_id = (try self.program.localScope(self.module, lambda_expr)) orelse return null;
        var scheme = try self.localScheme(scope_id);
        if (!scheme.per_call or scheme.isEmpty()) return null;
        const scope = try self.localProcWorker(local, try self.localProcDeclaration(lambda_expr));
        const scope_worker = scope.declaring;
        const declaring = self.program.worker(scope_worker).*;
        const per_call_worker = self.program.worker(scope.worker).*;
        const outer_count = declaring.scheme_vars.len;
        // Built from a declaration context, the procedure's types are read in
        // its callee's scope, whose dictionaries the evidence of its scheme
        // names, and its function value is a dictionary's method, an
        // immortal composition.
        const saved_env = self.env;
        const saved_foreign = self.foreign_scopes;
        const saved_scopes = self.alias_scopes.items.len;
        defer {
            self.env = saved_env;
            self.foreign_scopes = saved_foreign;
            self.alias_scopes.shrinkRetainingCapacity(saved_scopes);
        }
        if (scope.declaration != null) {
            self.env = .{ .module = self.module, .vars = declaring.scheme_vars, .bound = scope.bound };
            try self.alias_scopes.append(self.allocator, .{
                .params = declaring.dictionary_params,
                .dicts = scope.dicts,
                .literals = scope.literals,
                .literal_requirements = if (declaring.literal_scopes.len == 0) &.{} else declaring.literal_scopes[0],
                .declaration = scope.declaration,
                .saved_env = self.env,
            });
            self.foreign_scopes = saved_scopes;
        }
        const vars = try std.mem.concat(self.allocator, checked.CheckedTypeId, &.{ declaring.scheme_vars, scheme.scheme_vars });
        defer self.allocator.free(vars);
        const generic_bound = try self.allocator.alloc(TypeId, vars.len);
        defer self.allocator.free(generic_bound);
        for (generic_bound, 0..) |*slot, index| slot.* = try self.program.store.variable(@intCast(index));
        const lambda_ty = self.bodies().exprType(lambda_expr);
        const generic_fn = try self.program.converter().convert(self.module, lambda_ty, .{ .module = self.module, .vars = vars, .bound = generic_bound });
        // The procedure's own variables take the types the use's
        // instantiation gives them.
        const related = try self.relateVariablesPartial(&.{.{ generic_fn, use_ty }}, vars.len);
        defer self.allocator.free(related);
        // A variable the use's type does not reach, like the type of a
        // numeral only the body's methods fix, takes the type those methods
        // give it.
        for (related[0..outer_count], scope.bound) |*slot, ty| {
            if (slot.* == null) slot.* = ty;
        }
        try self.program.relateThroughRequirements(self.module, vars, scheme.params, related);
        scheme.per_call = false;
        const callee = try self.program.lambdaWorker(scope_worker, lambda_expr, per_call_worker.captures, per_call_worker.capture_types, generic_fn, scheme);
        const values = try arena.alloc(LocalId, if (static_captures) 0 else per_call_worker.captures.len);
        if (!static_captures) {
            const declared = self.declared_captures.get(lambda_expr);
            for (per_call_worker.captures, values, 0..) |capture, *value, index| {
                value.* = try self.localProcCaptureValue(capture, if (declared) |captured| captured[index] else null, scope.declaration);
            }
        }
        const own_types = try self.allocator.alloc(TypeId, scheme.scheme_vars.len);
        defer self.allocator.free(own_types);
        const declaring_module = self.module;
        const declaring_view = self.view;
        for (own_types, related[outer_count..], scheme.scheme_vars, 0..) |*own_ty, ty, variable, index| {
            own_ty.* = ty orelse if (index < subst.len and (site orelse declaring_view).types().payload(subst[index]) != .err) blk: {
                self.enterSiteView(site);
                defer self.leaveSiteView(declaring_module, declaring_view);
                break :blk try self.convert(subst[index]);
            } else try self.program.converter().sealedScheme(self.module, variable);
        }
        try self.resolveOwnKinds(lambda_ty, scheme, own_types);
        const descs = try std.mem.concat(arena, Word, &.{ scope.descs, try self.descWords(own_types) });
        const dicts = try arena.alloc(Word, scheme.dictionaries.len);
        const context = try self.scopeParamContext(scope_id, scheme.scheme_vars, own_types);
        for (scheme.dictionaries, dicts) |param_index, *dict| {
            if (evidence) |entries| {
                if (param_index >= entries.len) invariant("a local procedure use's evidence does not cover its dictionaries");
                self.enterSiteView(site);
                defer self.leaveSiteView(declaring_module, declaring_view);
                dict.* = try self.paramDictionary(entries, param_index, context);
            } else {
                dict.* = try self.derivedParamDictionary(param_index, context);
            }
        }
        // The procedure's own scheme's literal arguments, then the
        // declaring scope's.
        const literals = try arena.alloc(Word, scheme.literal_requirements.len);
        if (edge) |use_edge| {
            try self.fillLiteralArgs(null, use_edge, literals);
        } else {
            const bound_all = try std.mem.concat(self.allocator, TypeId, &.{ scope.bound, own_types });
            defer self.allocator.free(bound_all);
            try self.derivedLiteralArgs(callee, bound_all, literals);
        }
        const words = try std.mem.concat(arena, Word, &.{ dicts, scope.dicts, literals, scope.literals });
        // The procedure's type at this use, in this worker's variables.
        const use_bound = try self.allocator.alloc(TypeId, vars.len);
        defer self.allocator.free(use_bound);
        @memcpy(use_bound[0..outer_count], scope.bound);
        @memcpy(use_bound[outer_count..], own_types);
        const procedure_ty = try self.program.converter().convert(self.module, lambda_ty, .{ .module = self.module, .vars = vars, .bound = use_bound });
        return .{
            .callee = callee,
            .values = values,
            .capture_types = per_call_worker.capture_types,
            .descs = descs,
            .words = words,
            .adapt = .{ .own_ty = procedure_ty, .use_ty = use_ty },
            .static_scope = if (static_captures) scope else null,
        };
    }

    /// The function value of the local procedure a dispatch edge `node`
    /// selected, at the function type `fn_ty`. A method of a type declared
    /// in a function body is a local procedure of that body, which this
    /// body is, or is nested in.
    pub fn localMethodValue(self: *Builder, node: static_dispatch.EvidenceNode, fn_ty: TypeId) Allocator.Error!LocalId {
        const method = try self.localMethod(node);
        const site = self.view;
        const declaring = self.program.moduleByIdx(node.target.module_idx);
        self.enterSiteView(declaring);
        defer self.leaveSiteView(site.index, site);
        return try self.localProcInstanceValue(method.local, fn_ty, method.evidence, null, method.subst, site);
    }

    /// `localMethodValue` as an immortal callable, which a dictionary holds.
    pub fn localMethodWord(self: *Builder, node: static_dispatch.EvidenceNode, fn_ty: TypeId) Allocator.Error!Word {
        const method = try self.localMethod(node);
        const site = self.view;
        const declaring = self.program.moduleByIdx(node.target.module_idx);
        self.enterSiteView(declaring);
        defer self.leaveSiteView(site.index, site);
        return try self.localProcWord(method.local, fn_ty, method.evidence, null, method.subst, site);
    }

    /// The local procedure a dispatch edge `node` selected, and the edge's
    /// evidence and substitution of its scheme.
    fn localMethod(self: *Builder, node: static_dispatch.EvidenceNode) Allocator.Error!struct { local: checked.LocalProcedureBinding, evidence: ?[]const static_dispatch.CheckedEvidence, subst: []const checked.CheckedTypeId } {
        const target = switch (node.target.kind) {
            .local_proc => |target| target,
            .procedure, .structural, .callable_binding => invariant("a local method value names a procedure that is not local"),
        };
        const table = self.view.view.static_dispatch_plans;
        return .{
            .local = .{ .binder = target.binder, .expr = target.expr, .dispatch_scope = target.dispatch_scope },
            .evidence = switch (node.nested) {
                .resolved => table.nestedEvidence(node),
                .from_callable => null,
                .through_alias => invariant("alias-scope evidence accompanies only an alias instantiation"),
            },
            .subst = table.site_substitutions[node.subst.start..][0..node.subst.len],
        };
    }

    /// Read in `site`'s view, when there is one (`localProcInstanceValue`).
    fn enterSiteView(self: *Builder, site: ?*const types.ModuleView) void {
        const view = site orelse return;
        self.module = view.index;
        self.view = view;
    }

    fn leaveSiteView(self: *Builder, module: ModuleIndex, view: *const types.ModuleView) void {
        self.module = module;
        self.view = view;
    }

    /// The declaration context a function value of the local procedure
    /// whose lambda is `lambda_expr` is built from: the innermost one being
    /// built whose callee is the template owning the procedure's site. There
    /// is none when this body is, or is nested in, that template's body: a
    /// local procedure named in this body is then this body's own, even
    /// while it builds the dictionaries of a call of its own procedure.
    fn localProcDeclaration(self: *Builder, lambda_expr: ExprId) Allocator.Error!?*Declaration {
        if (self.declaration == null) return null;
        const owner = (try self.localProcOwner(lambda_expr)) orelse return null;
        var chain = self.worker_id;
        while (chain != owner) {
            switch (self.program.worker(chain).key) {
                .lambda => |key| chain = key.outer,
                .template, .adapter, .default, .method_entry, .structural, .structural_map, .root, .codec, .static_data, .crash_dictionary, .convert, .literal_thunk, .root_slot, .callable_eval_entry => return self.declarationOf(owner),
            }
        }
        return null;
    }

    /// The worker of the procedure template owning the site of the local
    /// procedure whose lambda is `lambda_expr`, when it has one.
    fn localProcOwner(self: *Builder, lambda_expr: ExprId) Allocator.Error!?WorkerId {
        const site_index = (try self.program.nestedProcSite(self.module, lambda_expr)) orelse
            invariant("a local procedure has no nested procedure site");
        const owner = switch (self.view.view.nested_proc_sites.sites[site_index].owner) {
            .template => |ref| ref,
            .default_root => return null,
        };
        const owner_module = self.program.moduleByKey(.{ .bytes = owner.artifact.bytes }).index;
        return self.program.worker_ids.get(.{ .template = .{ .module = owner_module, .template = owner.template } });
    }

    /// The worker of local procedure `lambda_expr` (of closure expression
    /// `closure_expr`, capturing `captures`) declared in `outer`'s body,
    /// created now when that body was never lowered here: it ran at compile
    /// time, and a closure it built was stored.
    fn declaredLambdaWorker(self: *Builder, outer: WorkerId, lambda_expr: ExprId, closure_ty: checked.CheckedTypeId, captures: []const checked.CheckedCapture) Allocator.Error!WorkerId {
        const scope = self.program.worker(outer).*;
        const own: ?lower.LocalScheme = if (try self.program.localScope(self.module, lambda_expr)) |scope_id| blk: {
            const scheme = try self.localScheme(scope_id);
            break :blk if (scheme.isEmpty()) null else scheme;
        } else null;
        const vars = if (own) |scheme| try std.mem.concat(self.allocator, checked.CheckedTypeId, &.{ scope.scheme_vars, scheme.scheme_vars }) else try self.allocator.dupe(checked.CheckedTypeId, scope.scheme_vars);
        defer self.allocator.free(vars);
        const bound = try self.allocator.alloc(TypeId, vars.len);
        defer self.allocator.free(bound);
        for (bound, 0..) |*slot, index| slot.* = try self.program.store.variable(@intCast(index));
        const env: types.Env = .{ .module = self.module, .vars = vars, .bound = bound };
        const fn_ty = try self.program.converter().convert(self.module, closure_ty, env);
        const capture_types = try self.allocator.alloc(TypeId, captures.len);
        defer self.allocator.free(capture_types);
        for (captures, capture_types) |capture, *capture_ty| capture_ty.* = try self.capturedType(capture.pattern, try self.program.converter().convert(self.module, self.bodies().patternType(capture.pattern), env));
        return try self.program.lambdaWorker(outer, lambda_expr, captures, capture_types, fn_ty, own);
    }

    /// The scope a function value of local procedure `local` is built in.
    /// From `declaration`, it is the context's callee's; otherwise it is the
    /// worker of this worker's chain that declares the procedure, whose
    /// body this body is or is nested in.
    fn localProcWorker(self: *Builder, local: checked.LocalProcedureBinding, declaration: ?*Declaration) Allocator.Error!LocalProcScope {
        const closure_expr = self.bodies().expr(local.expr);
        const lambda_expr = switch (closure_expr.data) {
            .lambda => local.expr,
            .closure => |closure| closure.lambda,
            .numeral, .str_segment, .str, .bytes_literal, .lookup_local, .lookup_external, .lookup_required, .list, .empty_list, .tuple, .match_, .if_, .call, .record, .empty_record, .block, .tag, .nominal, .zero_argument_tag, .binop, .unary_minus, .field_access, .dispatch_call, .interpolation, .structural_eq, .structural_hash, .method_eq, .type_dispatch_call, .tuple_access, .runtime_error, .crash, .dbg, .expect_err, .expect, .ellipsis, .anno_only, .break_, .return_, .for_, .hosted_lambda, .run_low_level => invariant("a local procedure's expression is not a lambda or closure"),
        };
        const captures = try self.program.siteCaptures(self.module, local.expr);
        const context = declaration orelse {
            var scope_worker = self.worker_id;
            const found = while (true) {
                if (self.program.worker_ids.get(.{ .lambda = .{ .module = self.module, .expr = lambda_expr, .outer = scope_worker } })) |id| break id;
                switch (self.program.worker(scope_worker).key) {
                    .lambda => |key| scope_worker = key.outer,
                    // The chain's procedure declares it but never lowered
                    // its declaration here: the chain is a stored closure
                    // of that procedure, whose body ran at compile time.
                    .template => if (try self.localProcOwner(lambda_expr) == scope_worker)
                        break try self.declaredLambdaWorker(scope_worker, lambda_expr, closure_expr.ty, captures)
                    else
                        invariant("a local procedure's function value is built outside its declaring body with no declaration context"),
                    .adapter, .default, .method_entry, .structural, .structural_map, .root, .codec, .static_data, .crash_dictionary, .convert, .literal_thunk, .root_slot, .callable_eval_entry => invariant("a local procedure's function value is built outside its declaring body with no declaration context"),
                }
            };
            const declaring = self.program.worker(scope_worker).*;
            var declaring_dicts: usize = 0;
            for (declaring.dict_scopes) |scope| declaring_dicts += scope.len;
            var declaring_literals: usize = 0;
            for (declaring.literal_scopes) |scope| declaring_literals += scope.len;
            const words = try self.dictLiteralInputWords(self.dict_locals.len - declaring_dicts, self.literal_locals.len - declaring_literals);
            return .{
                .declaring = scope_worker,
                .worker = found,
                .bound = self.env.bound[0..declaring.scheme_vars.len],
                .descs = try self.descInputWords(0, declaring.scheme_vars.len),
                .dicts = words[0..declaring_dicts],
                .literals = words[declaring_dicts..],
                .declaration = null,
            };
        };
        // A procedure built from a declaration context is a method of a type
        // its callee's body declares, outside every generalized local other
        // than its own scheme's. What it captures are the other local
        // procedures of that body it refers to (`localProcCaptureValue`).
        switch (self.view.view.nested_proc_sites.sites[(try self.program.nestedProcSite(self.module, lambda_expr)).?].lexical_scope) {
            .root => {},
            .generalized => |scope_id| if (try self.program.localScope(self.module, lambda_expr) != scope_id)
                invariant("a local procedure built from a declaration context is declared in a generalized local"),
        }
        const worker = try self.declaredLambdaWorker(context.worker, lambda_expr, closure_expr.ty, captures);
        const dicts = try self.program.arena.allocator().alloc(Word, context.dicts.len);
        for (dicts, 0..) |*dict, slot| dict.* = try self.declarationDictionary(context, slot);
        const words = try self.declarationWords(context);
        return .{
            .declaring = context.worker,
            .worker = worker,
            .bound = context.bound,
            .descs = words.descs,
            .dicts = dicts,
            .literals = words.literals,
            .declaration = context,
        };
    }

    /// What a function value of a local procedure captures for `capture`:
    /// the value its declaration captured (`declared`), when this body
    /// declared it; built from the declaration context `declaration`, the
    /// function value of the other local procedure of the callee's body it
    /// names, built from the same context, since a method of a type declared
    /// in a function body captures only the local procedures it refers to;
    /// otherwise the value visible here.
    fn localProcCaptureValue(self: *Builder, capture: checked.CheckedCapture, declared: ?LocalId, declaration: ?*Declaration) Allocator.Error!LocalId {
        if (declared) |value| return value;
        if (declaration != null) {
            const local = (try self.program.localProcBinding(self.module, capture.pattern)) orelse
                invariant("a local procedure built from a declaration context captures a value that is not a local procedure");
            return try self.localProcValue(local);
        }
        return self.capturedValue(capture.pattern) orelse invariant("a local procedure's capture is not visible where the procedure is used");
    }

    /// The type a callable captures `pattern`'s binder at: its value's type
    /// `ty`, or for a recursive value, the slot's.
    fn capturedType(self: *Builder, pattern: PatternId, ty: TypeId) Allocator.Error!TypeId {
        if (!self.bodies().binder(pattern).recursive_value) return ty;
        return try self.recursiveSlotType(ty);
    }

    /// What a callable captures for `pattern`'s binder: its value, or for a
    /// recursive value, its slot.
    fn capturedValue(self: *Builder, pattern: PatternId) ?LocalId {
        if (self.bodies().binder(pattern).recursive_value) return self.recursive_slots.get(pattern);
        return self.binders.get(pattern);
    }

    /// The slot type of a recursive value of type `ty`: a `Box` of the
    /// one-item tuple holding it.
    fn recursiveSlotType(self: *Builder, ty: TypeId) Allocator.Error!TypeId {
        return try self.program.store.intern(.{ .box = try self.program.store.intern(.{ .tuple = &.{ty} }) });
    }

    /// The value the recursive value slot `slot` (of type `slot_ty`) holds,
    /// read in the entry chain.
    fn readRecursiveSlot(self: *Builder, slot: LocalId, slot_ty: TypeId) Allocator.Error!LocalId {
        const tuple_ty = switch (self.program.store.get(slot_ty)) {
            .box => |payload| payload,
            .primitive, .empty_record, .record, .tuple, .tag_union, .list, .nominal, .function, .variable => invariant("a recursive value's slot is not a box"),
        };
        const value_ty = self.program.store.span(switch (self.program.store.get(tuple_ty)) {
            .tuple => |items| items,
            .primitive, .empty_record, .record, .tag_union, .list, .box, .nominal, .function, .variable => invariant("a recursive value's slot does not hold a tuple"),
        })[0];
        const payload = try self.localOf(tuple_ty, .view);
        try self.emitEntry(.{ .dyn = .{ .target = payload, .data = try self.store.addStmtRow(LIR.DynOp, .{ .box_payload_addr = .{ .box = slot, .desc = try self.descOf(tuple_ty) } }), .next = undefined } });
        const value = try self.localOf(value_ty, .view);
        try self.emitEntry(.{ .dyn = .{ .target = value, .data = try self.store.addStmtRow(LIR.DynOp, .{ .field_addr = .{ .base = payload, .desc = try self.descOf(tuple_ty), .field_index = .{ .immediate = 0 } } }), .next = undefined } });
        return value;
    }

    /// The recursive value binders `pattern` binds.
    fn recursiveBinders(self: *Builder, pattern: PatternId, out: *std.ArrayList(BinderId)) Allocator.Error!void {
        var stack: std.ArrayList(PatternId) = .empty;
        defer stack.deinit(self.allocator);
        try stack.append(self.allocator, pattern);
        while (stack.pop()) |current| {
            switch (self.bodies().patternData(current)) {
                .assign => if (self.bodies().binder(current).recursive_value) try out.append(self.allocator, current),
                .as => |as| {
                    if (self.bodies().binder(current).recursive_value) try out.append(self.allocator, current);
                    try stack.append(self.allocator, as.pattern);
                },
                .applied_tag => |tag| try stack.appendSlice(self.allocator, tag.args),
                .nominal => |nominal| try stack.append(self.allocator, nominal.backing_pattern),
                .record_destructure => |destructs| for (destructs) |destruct| try stack.append(self.allocator, self.bodies().recordDestructPattern(destruct)),
                .list => |list| {
                    try stack.appendSlice(self.allocator, list.patterns);
                    if (list.rest) |rest| if (rest.pattern) |sub| try stack.append(self.allocator, sub);
                },
                .tuple => |items| try stack.appendSlice(self.allocator, items),
                .numeral_literal, .str_literal, .str_interpolation, .underscore, .runtime_error => {},
            }
        }
    }

    /// Allocate a zeroed slot for each recursive value binder `pattern`
    /// binds.
    fn allocRecursiveSlots(self: *Builder, pattern: PatternId) Allocator.Error!void {
        var binders: std.ArrayList(BinderId) = .empty;
        defer binders.deinit(self.allocator);
        try self.recursiveBinders(pattern, &binders);
        for (binders.items) |binder| {
            const value_ty = try self.patternType(binder);
            const slot_ty = try self.recursiveSlotType(value_ty);
            const tuple_ty = try self.program.store.intern(.{ .tuple = &.{value_ty} });
            const slot = try self.localOf(slot_ty, .frame);
            try self.emitDyn(slot, .{ .slot_alloc = .{ .desc = try self.descOf(tuple_ty) } });
            try self.recursive_slots.put(self.allocator, binder, slot);
        }
    }

    /// Store each recursive value binder's value, now bound, in its slot.
    fn fillRecursiveSlots(self: *Builder, pattern: PatternId) Allocator.Error!void {
        var binders: std.ArrayList(BinderId) = .empty;
        defer binders.deinit(self.allocator);
        try self.recursiveBinders(pattern, &binders);
        for (binders.items) |binder| {
            const value = self.binders.get(binder) orelse invariant("a recursive value's binder was not bound by its declaration");
            const value_ty = try self.patternType(binder);
            const tuple_ty = try self.program.store.intern(.{ .tuple = &.{value_ty} });
            const tuple = try self.buildStruct(tuple_ty, &.{value});
            const filled = try self.localOf(try self.recursiveSlotType(value_ty), .frame);
            try self.emitDyn(filled, .{ .slot_store = .{
                .slot = self.recursive_slots.get(binder).?,
                .value = tuple,
                .desc = try self.descOf(tuple_ty),
            } });
            try self.recursive_slots.put(self.allocator, binder, filled);
        }
    }

    /// The function value of a local procedure: the closure its declaration
    /// bound, or, inside the procedure's own body, the same closure rebuilt
    /// from the captures the body received (`localProcClosure`).
    pub fn localProcValue(self: *Builder, local: checked.LocalProcedureBinding) Allocator.Error!LocalId {
        const declaration = if (local.is_alias) null else try self.localProcDeclaration(switch (self.bodies().expr(local.expr).data) {
            .lambda => local.expr,
            .closure => |closure| closure.lambda,
            .numeral, .str_segment, .str, .bytes_literal, .lookup_local, .lookup_external, .lookup_required, .list, .empty_list, .tuple, .match_, .if_, .call, .record, .empty_record, .block, .tag, .nominal, .zero_argument_tag, .binop, .unary_minus, .field_access, .dispatch_call, .interpolation, .structural_eq, .structural_hash, .method_eq, .type_dispatch_call, .tuple_access, .runtime_error, .crash, .dbg, .expect_err, .expect, .ellipsis, .anno_only, .break_, .return_, .for_, .hosted_lambda, .run_low_level => invariant("a local procedure's expression is not a lambda or closure"),
        });
        if (declaration == null) {
            // The entry chain reads only values the entry chain defines.
            if (self.binders.get(local.binder)) |value| {
                if (!self.emitting_entry or self.entry_locals.contains(value)) return value;
            }
        }
        if (local.is_alias) invariant("an alias is used only after its declaration binds it");
        const closure = try self.localProcClosure(local);
        // A procedure whose calls pass its table captures none.
        const inputs: []const Word = if (self.program.perCall(closure.callee)) &.{} else try closure.inputs(self.program.arena.allocator());
        // A value built from entry locals only is built once, in the entry
        // chain, for every reference; one reading a value the body computes
        // is built where it is referenced.
        const at_entry = for (closure.values) |value| {
            if (!self.entry_locals.contains(value)) break false;
        } else true;
        const all_closed = try self.allClosed(closure.capture_types);
        if (!at_entry) {
            return if (all_closed)
                try self.packClosure(closure.callee, closure.values, closure.capture_types, inputs)
            else
                try self.packDynamicClosure(closure.callee, closure.values, closure.capture_types, inputs);
        }
        const value = if (all_closed)
            try self.emitIntoEntry(packClosure, .{ closure.callee, closure.values, closure.capture_types, inputs })
        else
            try self.emitIntoEntry(packDynamicClosure, .{ closure.callee, closure.values, closure.capture_types, inputs });
        if (declaration == null) try self.binders.put(self.allocator, local.binder, value);
        return value;
    }

    /// What the function value of a local procedure packs, referenced
    /// inside its own body or a nested function: its captures, which the
    /// checker makes visible wherever the procedure is, and the scope it was
    /// declared in, whose descriptors and dictionaries this body holds.
    fn localProcClosure(self: *Builder, local: checked.LocalProcedureBinding) Allocator.Error!LocalProcClosure {
        return try self.localProcClosureWithCaptures(local, false);
    }

    fn localProcClosureWithCaptures(self: *Builder, local: checked.LocalProcedureBinding, comptime static_captures: bool) Allocator.Error!LocalProcClosure {
        const lambda_expr = switch (self.bodies().exprData(local.expr)) {
            .lambda => local.expr,
            .closure => |closure| closure.lambda,
            .numeral, .str_segment, .str, .bytes_literal, .lookup_local, .lookup_external, .lookup_required, .list, .empty_list, .tuple, .match_, .if_, .call, .record, .empty_record, .block, .tag, .nominal, .zero_argument_tag, .binop, .unary_minus, .field_access, .dispatch_call, .interpolation, .structural_eq, .structural_hash, .method_eq, .type_dispatch_call, .tuple_access, .runtime_error, .crash, .dbg, .expect_err, .expect, .ellipsis, .anno_only, .break_, .return_, .for_, .hosted_lambda, .run_low_level => invariant("a local procedure's expression is not a lambda or closure"),
        };
        const scope = try self.localProcWorker(local, try self.localProcDeclaration(lambda_expr));
        const target = scope.worker;
        const target_worker = self.program.worker(target).*;
        const values = try self.program.arena.allocator().alloc(LocalId, if (static_captures) 0 else target_worker.captures.len);
        if (!static_captures) {
            const declared = self.declared_captures.get(target_worker.key.lambda.expr);
            for (target_worker.captures, values, 0..) |capture, *value, index| {
                value.* = try self.localProcCaptureValue(capture, if (declared) |captured| captured[index] else null, scope.declaration);
            }
        }
        const descs = scope.descs;
        const dicts = try std.mem.concat(self.program.arena.allocator(), Word, &.{ scope.dicts, scope.literals });
        return .{
            .callee = target,
            .values = values,
            .capture_types = target_worker.capture_types,
            .descs = descs,
            .words = dicts,
            .static_scope = if (static_captures) scope else null,
        };
    }

    /// The own scheme of generalized-local scope `scope_id`: its quantified
    /// variables and its runtime evidence parameters.
    pub fn localScheme(self: *Builder, scope_id: checked.DispatchScopeId) Allocator.Error!lower.LocalScheme {
        return try self.localSchemeIn(self.view, scope_id);
    }

    pub fn localSchemeIn(self: *Builder, view: *const types.ModuleView, scope_id: checked.DispatchScopeId) Allocator.Error!lower.LocalScheme {
        const templates = view.view.checked_procedure_templates;
        const scope = templates.dispatch_scopes[@backingInt(scope_id)];
        const evidence_params = lower.poolSlice(static_dispatch.EvidenceParamRecord, templates.evidence_params_pool, scope.evidence_params);
        var dictionaries: std.ArrayList(u32) = .empty;
        for (evidence_params, 0..) |_, index| {
            try dictionaries.append(self.program.arena.allocator(), @intCast(index));
        }
        return .{
            .scheme_vars = lower.poolSlice(checked.CheckedTypeId, templates.scheme_vars_pool, scope.scheme_vars),
            .dictionaries = dictionaries.items,
            .params = evidence_params,
            .literal_requirements = templates.scopeLiteralRequirements(&scope),
            .per_call = try self.program.isLocalProc(view.index, scope.checked_expr),
        };
    }

    /// A call of a generalized local procedure: its function value, called
    /// with an argument record of the call's instantiation of the
    /// procedure's own scheme (descriptors, then dictionaries) followed by
    /// the explicit arguments.
    pub fn lowerGeneralizedLocalCall(self: *Builder, expr_id: ExprId, call: @FieldType(bodies_mod.ExprData, "call"), local: checked.LocalProcedureBinding, scheme: lower.LocalScheme) Allocator.Error!void {
        const closure = try self.localProcValue(local);
        const evidence = self.view.view.static_dispatch_plans.siteEvidence(call.func) orelse &.{};
        // The procedure's own variables take the types the call's
        // instantiation of its type gives them; the enclosing scope's stay as
        // they are here.
        const lambda_expr = switch (self.bodies().exprData(local.expr)) {
            .lambda => local.expr,
            .closure => |closure_expr| closure_expr.lambda,
            .numeral, .str_segment, .str, .bytes_literal, .lookup_local, .lookup_external, .lookup_required, .list, .empty_list, .tuple, .match_, .if_, .call, .record, .empty_record, .block, .tag, .nominal, .zero_argument_tag, .binop, .unary_minus, .field_access, .dispatch_call, .interpolation, .structural_eq, .structural_hash, .method_eq, .type_dispatch_call, .tuple_access, .runtime_error, .crash, .dbg, .expect_err, .expect, .ellipsis, .anno_only, .break_, .return_, .for_, .hosted_lambda, .run_low_level => invariant("a local procedure's expression is not a lambda or closure"),
        };
        const outer_count = self.env.vars.len;
        const vars = try std.mem.concat(self.allocator, checked.CheckedTypeId, &.{ scheme.scheme_vars, self.env.vars });
        defer self.allocator.free(vars);
        const generic_bound = try self.allocator.alloc(TypeId, vars.len);
        defer self.allocator.free(generic_bound);
        for (generic_bound[0..scheme.scheme_vars.len], 0..) |*slot, index| slot.* = try self.program.store.variable(@intCast(outer_count + index));
        for (generic_bound[scheme.scheme_vars.len..], 0..) |*slot, index| slot.* = try self.program.store.variable(@intCast(index));
        const lambda_ty = self.bodies().exprType(lambda_expr);
        const generic_fn = try self.program.converter().convert(self.module, lambda_ty, .{ .module = self.module, .vars = vars, .bound = generic_bound });
        const related = try self.relateVariablesPartial(&.{.{ generic_fn, try self.valueType(call.func) }}, outer_count + scheme.scheme_vars.len);
        defer self.allocator.free(related);
        const own_types = try self.allocator.alloc(TypeId, scheme.scheme_vars.len);
        defer self.allocator.free(own_types);
        // A variable the call's type does not relate takes the call's
        // checked instantiation of it, or, the checker having fixed none,
        // seals as a variable no use relates.
        const subst = self.siteSubstitution(call.func);
        for (own_types, related[outer_count..], scheme.scheme_vars, 0..) |*slot, ty, variable, index| slot.* = ty orelse
            if (index < subst.len and self.view.types().payload(subst[index]) != .err)
                try self.convert(subst[index])
            else
                try self.program.converter().sealedScheme(self.module, variable);
        try self.resolveOwnKinds(lambda_ty, scheme, own_types);
        const bound = try self.allocator.alloc(TypeId, vars.len);
        defer self.allocator.free(bound);
        @memcpy(bound[0..own_types.len], own_types);
        for (bound[own_types.len..], 0..) |*slot, index| slot.* = try self.program.store.variable(@intCast(index));
        const fn_ty = try self.program.converter().convert(self.module, lambda_ty, .{ .module = self.module, .vars = vars, .bound = bound });
        const params, const ret = switch (self.program.store.get(fn_ty)) {
            .function => |function| .{ try self.allocator.dupe(TypeId, self.program.store.span(function.args)), function.ret },
            .primitive, .empty_record, .record, .tuple, .tag_union, .list, .box, .nominal, .variable => invariant("a local procedure's type is not a function type"),
        };
        defer self.allocator.free(params);
        if (params.len != call.args.len) invariant("a local procedure call's arity differs from its type");
        const arg_types = try self.program.arena.allocator().alloc(TypeId, call.args.len);
        for (call.args, arg_types) |arg, *arg_ty| arg_ty.* = try self.valueType(arg);
        // The procedure's own scheme's descriptors, dictionaries and
        // literal arguments, which the call passes with its arguments.
        const own_words = scheme.scheme_vars.len + scheme.dictionaries.len + scheme.literal_requirements.len;
        const words = try self.allocator.alloc(Word, own_words);
        defer self.allocator.free(words);
        for (own_types, words[0..own_types.len]) |ty, *desc| desc.* = try self.descWord(ty);
        const scope_id = local.dispatch_scope orelse invariant("a generalized local procedure has no checked scheme scope");
        const context = try self.scopeParamContext(scope_id, scheme.scheme_vars, own_types);
        for (scheme.dictionaries, words[own_types.len..][0..scheme.dictionaries.len]) |param_index, *dict| {
            if (param_index >= evidence.len) invariant("a local procedure call's evidence does not cover its dictionaries");
            dict.* = try self.paramDictionary(evidence, param_index, context);
        }
        try self.fillLiteralArgs(null, .{ .use = call.func }, words[own_types.len + scheme.dictionaries.len ..]);
        // The call passes the procedure's table: its inputs are the
        // declaring scope's descriptors then its own, its own dictionaries
        // then the declaring scope's, and likewise its literal arguments.
        const declared = try self.localProcClosure(local);
        const target = self.program.worker(declared.callee).*;
        const outer_dicts = target.capturedDictionaryCount();
        if (target.own_descs != own_types.len or target.own_dicts != scheme.dictionaries.len or target.own_literals != scheme.literal_requirements.len) invariant("a local procedure call's own words differ from its worker's");
        const table_inputs = try std.mem.concat(self.program.arena.allocator(), Word, &.{
            declared.descs,
            words[0..own_types.len],
            words[own_types.len..][0..scheme.dictionaries.len],
            declared.words[0..outer_dicts],
            words[own_types.len + scheme.dictionaries.len ..],
            declared.words[outer_dicts..],
        });
        try self.push(.{ .finish_local_call = .{
            .closure = closure,
            .words = try self.program.arena.allocator().dupe(Word, &.{try self.tableWord(declared.callee, table_inputs)}),
            .params = try self.program.arena.allocator().dupe(TypeId, params),
            .ret = ret,
            .arg_types = arg_types,
            .result = try self.exprType(expr_id),
        } });
        try self.pushExprsReversed(call.args);
    }

    pub fn finishLocalCall(self: *Builder, closure: LocalId, word_values: []const Word, params: []const TypeId, ret: TypeId, arg_types: []const TypeId, result_ty: TypeId) Allocator.Error!void {
        const words = try self.materializeAll(word_values);
        defer self.allocator.free(words);
        const args = try self.popValues(@intCast(params.len));
        defer self.allocator.free(args);
        // The procedure stores record fields its own way where its kinds
        // are its own scheme's (design.md "Field Kinds").
        for (args, arg_types, params) |*arg, arg_ty, param_ty| arg.* = try self.adaptValue(arg.*, arg_ty, param_ty);
        if (words.len == 0 and args.len == 1 and self.canPassUnaryArgument(closure, args[0])) {
            const target = try self.callErased(closure, args[0], ret);
            return self.pushValue(try self.adaptValue(target, ret, result_ty));
        }
        const desc = try self.argsDesc(@intCast(words.len), try self.descWords(params));
        const product = try self.productShapeFields(words.len, params);
        const record = try self.addLocal(.{ .layout_idx = try self.program.result.layouts.dynamicLayout(), .dyn = .{ .desc = desc, .storage = .frame }, .product = product });
        const fields = try std.mem.concat(self.allocator, LocalId, &.{ words, args });
        defer self.allocator.free(fields);
        try self.emitDyn(record, .{ .record = .{ .desc = desc, .fields = try self.store.addLocalSpan(fields) } });
        const target = try self.callErased(closure, record, ret);
        try self.pushValue(try self.adaptValue(target, ret, result_ty));
    }

    /// A procedure used as a function value: an immortal callable of its
    /// dictionary entry, capturing the use site's hidden arguments.
    pub fn procedureValue(self: *Builder, expr_id: ExprId, use: checked.ProcedureUseTemplate, required: ?checked.PlatformRequiredProcedureResolvedRef) Allocator.Error!void {
        const callee = try self.procedureUseCallee(use);
        if (callee == .callable_eval) return try self.push(.{ .restore_callable_eval = .{ .module = callee.callable_eval.module, .template = callee.callable_eval.template, .ty = try self.exprType(expr_id) } });
        const use_ty = try self.exprType(expr_id);
        const target = callee.target();
        const instance = try self.procedureUseInstance(callee, expr_id, required, use_ty);
        defer self.allocator.free(instance.hidden);
        const value = try self.materialize(try self.composeDictionary(try self.program.methodEntryWorker(target), instance.hidden));
        // The procedure stores record fields its own way where its kinds are
        // its own scheme's (design.md "Field Kinds"); the value is adapted to
        // the representation its use calls it with.
        const target_worker = self.program.worker(target).*;
        const own_fn = try self.program.store.intern(.{ .function = .{ .args = target_worker.params, .ret = target_worker.ret, .effectful = false } });
        try self.pushValue(try self.adaptValue(value, try self.program.substitute(self.allocator, own_fn, instance.bound), use_ty));
    }

    /// The instantiation of the procedure `callee` that use `expr_id`, at
    /// function type `use_ty`, names: of a platform requirement `required`
    /// when it is one, whose call checking recorded no edge for, so its
    /// literal arguments derive from the use's instantiation as its
    /// dictionaries do.
    fn procedureUseInstance(self: *Builder, callee: Callee, expr_id: ExprId, required: ?checked.PlatformRequiredProcedureResolvedRef, use_ty: TypeId) Allocator.Error!CallInstance {
        const subst = self.siteSubstitution(expr_id);
        const evidence = self.view.view.static_dispatch_plans.siteEvidence(expr_id) orelse &.{};
        return switch (callee) {
            .worker => |worker_id| if (required) |requirement|
                try self.requiredInstanceAt(worker_id, requirement, subst, use_ty, .literal)
            else
                try self.callInstanceAt(worker_id, subst, evidence, use_ty, .{ .use = expr_id }),
            .alias => |alias| if (required) |requirement|
                try self.requiredAliasInstance(alias, requirement, use_ty)
            else
                try self.aliasUseInstance(alias, subst, evidence, use_ty, .{ .use = expr_id }),
            .callable_eval => invariant("a computed callable has no instantiation"),
        };
    }

    /// A read of a stored constant: its immutable representation at `ty`
    /// (design.md "Compile-Time Evaluation Strategy").
    /// Restore stored constant `stored` read at `ty`: at `ty` when it is
    /// closed, and otherwise at the value's own stored type, re-tagged to
    /// `ty`.
    fn pushStoredRestore(self: *Builder, source: lower.ConstSource, stored: checked.StoredConstTemplate, ty: TypeId, root: ?checked.ComptimeRootId) Allocator.Error!void {
        if (root) |id| if (self.program.module(source.module).view.compile_time_roots.root(id).needsEvaluation(.boxy)) invariant("a stored root read bypassed strategy readiness");
        if (self.program.store.isClosed(ty)) return try self.push(.{ .restore = .{ .source = source, .node = stored.node, .ty = ty, .root = root } });
        const stored_ty = try self.constType(source, stored.root_type);
        try self.push(.{ .retag = .{ .from = stored_ty, .to = ty } });
        try self.push(.{ .restore = .{ .source = source, .node = stored.node, .ty = stored_ty, .root = root } });
    }

    /// The compile-time root whose value a stored constant reference is.
    fn rootOfConstRef(owner: *const types.ModuleView, const_ref: checked.ConstRef) ?checked.ComptimeRootId {
        return switch (const_ref.owner) {
            .hoisted_root => |hoisted| hoisted.root,
            .top_level_binding => |top_level| owner.view.compile_time_roots.lookupIdByPattern(top_level.pattern),
        };
    }

    pub fn restoreConstUse(self: *Builder, expr_id: ExprId, use: checked.ConstUseTemplate, ty: TypeId) Allocator.Error!void {
        const owner = self.program.moduleByKey(use.const_ref.artifact);
        const template = owner.view.const_templates.get(use.const_ref);
        if (rootOfConstRef(owner, use.const_ref)) |root_id| {
            const root = owner.view.compile_time_roots.root(root_id);
            if (root.boxy_value) |stored| return try self.pushStoredRestore(.{ .module = owner.index }, stored, ty, root_id);
            if (root.payloadFor(.boxy) == .pending and template.state == .stored_const) return try self.pendingValueRead(owner.index, root_id, ty);
        }
        switch (template.state) {
            .stored_const => |stored| try self.pushStoredRestore(.{ .module = owner.index }, stored, ty, rootOfConstRef(owner, use.const_ref)),
            // A constant this compile-time program evaluates is read from
            // its slot. A polymorphic constant compile-time evaluation did
            // not store is its entry procedure's value at the read's
            // instantiation.
            .eval_template => |eval| {
                if (rootOfConstRef(owner, use.const_ref)) |root| if (try self.program.readsRootSlot(owner.index, root)) return try self.pendingValueRead(owner.index, root, ty);
                const callee = try self.program.templateRefWorker(eval.entry_template);
                const read_ty = try self.exprType(expr_id);
                const fn_ty = try self.program.store.intern(.{ .function = .{ .args = &.{}, .ret = read_ty, .effectful = false } });
                const instance = try self.callInstanceAt(callee, self.siteSubstitution(expr_id), self.view.view.static_dispatch_plans.siteEvidence(expr_id) orelse &.{}, fn_ty, .{ .use = expr_id });
                defer self.allocator.free(instance.hidden);
                try self.finishCall(callee, instance.hidden, instance.bound, &.{}, instance.ret, read_ty);
            },
            .unimplemented => |unimplemented| {
                self.region = unimplemented.region;
                try self.crashValue(@import("../common.zig").unimplemented_declaration_crash, ty);
            },
            .reserved => {
                if (!self.program.input.compile_time) invariant("a constant read at runtime was not stored by compile-time evaluation");
                const root = switch (use.const_ref.owner) {
                    .top_level_binding => |top_level| owner.view.compile_time_roots.lookupIdByPattern(top_level.pattern) orelse invariant("a reserved constant has no compile-time root"),
                    .hoisted_root => |hoisted| hoisted.root,
                };
                try self.pendingValueRead(owner.index, root, ty);
            },
        }
    }

    /// Restore stored constant `node` of `module` at `ty`, pushing tasks for
    /// its components first.
    pub fn restoreStep(self: *Builder, source: lower.ConstSource, node: checked.ConstNodeId, raw_ty: TypeId, root: ?checked.ComptimeRootId) Allocator.Error!void {
        const const_store = self.program.constStore(source);
        const store = &self.program.store;
        // Every restore starts at a closed type (`pushStoredRestore`), and a
        // closed type's components are closed.
        if (!store.isClosed(raw_ty)) invariant("a stored value is restored at an open type");
        const ty = try self.structuralType(raw_ty);
        const key: lower.Program.StoredValueKey = .{ .module = source.module, .node = node, .ty = raw_ty };
        const data = const_store.get(node);
        if (data != .scalar) if (self.program.stored_value_slots.get(key)) |slot| return self.pushValue(try self.staticRead(slot));
        if (data != .crash and data != .checked_error) try self.push(.{ .remember_static_value = key });
        switch (data) {
            .pending => invariant("a stored constant was never filled"),
            .scalar => |scalar| {
                const local = try self.localOf(raw_ty, .frame);
                const value: LIR.LiteralValue = switch (scalar) {
                    inline .i8, .i16, .i32, .i64, .u8, .u16, .u32 => |v| .{ .i64_literal = .{ .value = v, .layout_idx = self.store.getLocal(local).layout_idx } },
                    .u64 => |v| .{ .i64_literal = .{ .value = @bitCast(v), .layout_idx = self.store.getLocal(local).layout_idx } },
                    .i128 => |v| .{ .i128_literal = try self.store.addStmtRow(LIR.I128Literal, .{ .value = v, .layout_idx = self.store.getLocal(local).layout_idx }) },
                    .u128 => |v| .{ .i128_literal = try self.store.addStmtRow(LIR.I128Literal, .{ .value = @bitCast(v), .layout_idx = self.store.getLocal(local).layout_idx }) },
                    .f32_bits => |bits| .{ .f32_literal = @bitCast(bits) },
                    .f64_bits => |bits| .{ .f64_literal = @bitCast(bits) },
                    .dec_bits => |bits| .{ .dec_literal = bits },
                };
                try self.emitLinear(.{ .assign_literal = .{ .target = local, .value = value, .next = undefined } });
                const slot = try self.storedDataSlot(source, node, raw_ty);
                try self.constant_slots.put(self.allocator, local, slot);
                try self.pushValue(local);
            },
            .zst, .str => try self.pushValue(try self.storedDataLocal(source, node, raw_ty)),
            .crash => |message| {
                if (root) |failed| {
                    // The value is a root's whose evaluation failed and was
                    // reported: this read fails as that root did.
                    const literal = try self.store.insertString(const_store.strBytes(message));
                    try self.emitTerminal(.{ .crash = .{
                        .msg = .{ .literal = literal },
                        .extra = try self.store.addStmtRow(LIR.CrashExtra, .{ .failed_value = .{
                            .owner = self.program.lowering_module_ids[@backingInt(source.module)],
                            .producer = .{ .checked = failed },
                        } }),
                    } });
                    try self.pushValue(try self.localOf(raw_ty, .frame));
                } else {
                    try self.crashValue(const_store.strBytes(message), raw_ty);
                }
            },
            .checked_error => |message| {
                const literal = try self.store.insertString(const_store.strBytes(message));
                try self.emitTerminal(.{ .crash = .{ .msg = .{ .literal = literal }, .checked_error = true } });
                try self.pushValue(try self.localOf(raw_ty, .frame));
            },
            .boxy_fn => |function| try self.restoreStoredFn(source, const_store.strBytes(function.recipe), function.captures),
            .nominal => |nominal| try self.push(.{ .restore = .{ .source = source, .node = nominal.backing, .ty = ty } }),
            .box => |child| {
                const payload = switch (store.get(ty)) {
                    .box => |payload| payload,
                    .primitive, .empty_record, .record, .tuple, .tag_union, .list, .nominal, .function, .variable => invariant("a stored box restored at a non-Box type"),
                };
                const sequence_step_offset: ?u32 = if (store.isSequence(payload)) blk: {
                    const payload_layout = try self.program.layouts.layoutOf(payload);
                    const layouts = &self.program.result.layouts;
                    break :blk layouts.getStructFieldOffsetByOriginalIndex(layouts.getLayout(payload_layout).getStruct().idx, 1);
                } else null;
                try self.push(.{ .finish_static_box = .{ .ty = raw_ty, .sequence_step_offset = sequence_step_offset } });
                try self.push(.{ .restore = .{ .source = source, .node = child, .ty = payload } });
            },
            .tuple, .record => |children| {
                const component_types: []const TypeId = switch (store.get(ty)) {
                    .tuple => |items| store.span(items),
                    .record => |record| blk: {
                        const fields = store.fieldSpan(record.fields);
                        const storage = try self.program.arena.allocator().alloc(TypeId, fields.len);
                        // An optional field's stored value is its presence
                        // slot's tag (design.md "Field Kinds").
                        for (fields, storage) |field, *slot| slot.* = try self.program.layouts.fieldStorage(field);
                        break :blk storage;
                    },
                    .empty_record => &.{},
                    .primitive, .tag_union, .list, .box, .nominal, .function, .variable => invariant("a stored product restored at a non-product type"),
                };
                if (component_types.len != children.len) invariant("a stored product's arity differs from its type");
                try self.push(.{ .finish_restore_struct = .{ .ty = raw_ty, .count = @intCast(children.len) } });
                var index = children.len;
                while (index > 0) {
                    index -= 1;
                    try self.push(.{ .restore = .{ .source = source, .node = children[index], .ty = component_types[index] } });
                }
            },
            .tag => |tag| {
                const union_ty = switch (store.get(ty)) {
                    .tag_union => |union_ty| union_ty,
                    .primitive, .empty_record, .record, .tuple, .list, .box, .nominal, .function, .variable => invariant("a stored tag restored at a non-tag-union type"),
                };
                const name = try store.internName(tag.tag_name);
                const tags = store.tagSpan(union_ty.tags);
                const variant = for (tags, 0..) |candidate, index| {
                    if (candidate.name == name and candidate.payloads.len == tag.payloads.len) break index;
                } else invariant("a stored tag is absent from its type");
                const payload_types = store.span(tags[variant].payloads);
                try self.push(.{ .finish_restore_tag = .{ .ty = raw_ty, .name = name, .count = @intCast(tag.payloads.len) } });
                var index = tag.payloads.len;
                while (index > 0) {
                    index -= 1;
                    try self.push(.{ .restore = .{ .source = source, .node = tag.payloads[index], .ty = payload_types[index] } });
                }
            },
            .list => |list| {
                const elem = switch (store.get(ty)) {
                    .list => |elem| elem,
                    .primitive, .empty_record, .record, .tuple, .tag_union, .box, .nominal, .function, .variable => invariant("a stored list restored at a non-List type"),
                };
                switch (list.items) {
                    .nodes => |nodes| {
                        try self.push(.{ .finish_static_list = .{ .ty = raw_ty, .count = @intCast(nodes.len), .capacity = list.capacity } });
                        var index = nodes.len;
                        while (index > 0) {
                            index -= 1;
                            try self.push(.{ .restore = .{ .source = source, .node = nodes[index], .ty = elem } });
                        }
                    },
                    .empty, .packed_bytes => try self.pushValue(try self.storedDataLocal(source, node, raw_ty)),
                }
            },
            .fn_value => |fn_id| try self.restoreFn(source, fn_id, raw_ty),
        }
    }

    /// Restore stored function value `fn_id` of `module`'s const store as
    /// an erased callable.
    /// Restore a function value compile-time evaluation stored, used at the
    /// closed function type `fn_ty`.
    pub fn restoreFn(self: *Builder, source: lower.ConstSource, fn_id: check.ConstStore.ConstFnId, fn_ty: TypeId) Allocator.Error!void {
        const const_store = self.program.constStore(source);
        const stored = const_store.getFn(fn_id);
        switch (stored.fn_def) {
            // A procedure template's value, whether a checked body, a hosted
            // procedure, or a checked-generated procedure.
            .local_template, .imported_template, .local_hosted, .imported_hosted, .checked_generated => |ref| {
                const target = try self.program.templateRefWorker(ref);
                const target_worker = self.program.worker(target).*;
                const formal = try self.program.store.intern(.{ .function = .{ .args = target_worker.params, .ret = target_worker.ret, .effectful = false } });
                // A scheme variable only the target's requirements mention is
                // fixed by relating those requirements' methods.
                const bound = (try self.program.instantiateTarget(target, formal, fn_ty)).bound;
                const dicts = try self.storedDictionaries(source, stored, target, bound);
                defer self.allocator.free(dicts);
                const hidden = try self.allocator.alloc(Word, bound.len + dicts.len);
                defer self.allocator.free(hidden);
                for (bound, hidden[0..bound.len]) |ty, *desc| desc.* = try self.descWord(ty);
                @memcpy(hidden[bound.len..], dicts);
                const entry = try self.program.methodEntryWorker(target);
                try self.pushValue(try self.materialize(try self.composeDictionary(entry, hidden)));
            },
            .nested => |nested| {
                // A closure of a defaulted field's expression belongs to the
                // worker evaluating that default, which has no scheme and no
                // dictionaries of its own.
                const owner_worker = if (nested.default_root) |identity| blk: {
                    const declaring = self.program.modules.by_identity.get(identity.bytes) orelse invariant("a stored default-root closure's module is not in this lowering");
                    const declaring_view = self.program.module(declaring);
                    const default_site = declaring_view.view.nested_proc_sites.sites[@backingInt(nested.site)];
                    break :blk switch (default_site.owner) {
                        .default_root => |default_expr| try self.program.defaultWorker(declaring, default_expr),
                        .template => invariant("a stored default-root closure's site is owned by a template"),
                    };
                } else try self.program.templateRefScope(nested.owner);
                const owner = self.program.worker(owner_worker).*;
                const owner_view = self.program.module(owner.module);
                const site = owner_view.view.nested_proc_sites.sites[@backingInt(nested.site)];
                const site_expr = site.checked_expr orelse invariant("a stored closure's site has no checked expression");
                const expr = owner_view.bodies().expr(site_expr);
                const lambda_expr = switch (expr.data) {
                    .lambda => site_expr,
                    .closure => |closure| closure.lambda,
                    .numeral, .str_segment, .str, .bytes_literal, .lookup_local, .lookup_external, .lookup_required, .list, .empty_list, .tuple, .match_, .if_, .call, .record, .empty_record, .block, .tag, .nominal, .zero_argument_tag, .binop, .unary_minus, .field_access, .dispatch_call, .interpolation, .structural_eq, .structural_hash, .method_eq, .type_dispatch_call, .tuple_access, .runtime_error, .crash, .dbg, .expect_err, .expect, .ellipsis, .anno_only, .break_, .return_, .for_, .hosted_lambda, .run_low_level => invariant("a stored closure's site is not a lambda or closure"),
                };
                const captures = try self.program.runtimeCaptures(owner_view, @backingInt(nested.site));
                // The closure in its owner's scope (and, for a generalized
                // lambda, its own scheme's), whose variables the stored
                // value's type instantiates; the value captures them all.
                const own_scope = try self.program.localScope(owner.module, lambda_expr);
                const own: ?lower.LocalScheme = if (own_scope) |scope_id| blk: {
                    var scheme = try self.localSchemeIn(owner_view, scope_id);
                    if (scheme.isEmpty()) break :blk null;
                    scheme.per_call = false;
                    break :blk scheme;
                } else null;
                const scope_vars = if (own) |scheme| try std.mem.concat(self.allocator, checked.CheckedTypeId, &.{ owner.scheme_vars, scheme.scheme_vars }) else try self.allocator.dupe(checked.CheckedTypeId, owner.scheme_vars);
                defer self.allocator.free(scope_vars);
                const variables = try self.allocator.alloc(TypeId, scope_vars.len);
                defer self.allocator.free(variables);
                for (variables, 0..) |*variable, index| variable.* = try self.program.store.variable(@intCast(index));
                const generic: types.Env = .{ .module = owner.module, .vars = scope_vars, .bound = variables };
                const generic_fn = try self.program.converter().convert(owner.module, expr.ty, generic);
                const capture_types = try self.allocator.alloc(TypeId, captures.len);
                defer self.allocator.free(capture_types);
                const stored_captures = try self.allocator.alloc(check.ConstStore.ConstCapture, captures.len);
                defer self.allocator.free(stored_captures);
                // The variables relate through the function's type and
                // through the stored captures' types, which can mention
                // variables the function's type does not.
                var relations: std.ArrayList([2]TypeId) = .empty;
                defer relations.deinit(self.allocator);
                try relations.append(self.allocator, .{ generic_fn, fn_ty });
                for (captures, capture_types, stored_captures) |capture, *capture_ty, *stored_capture| {
                    capture_ty.* = try self.program.converter().convert(owner.module, owner_view.bodies().patternType(capture.pattern), generic);
                    stored_capture.* = for (stored.captures) |candidate| {
                        if (std.meta.eql(candidate.id, capture.capture_id)) break candidate;
                    } else invariant("a stored closure lacks one of its site's captures");
                    // A site's runtime captures are binders of enclosing
                    // frames; a recursive-binding capture holds a top-level
                    // root, which the restored closure reaches through its
                    // own top-level reference and its site never lists.
                    if (stored_capture.kind != .lexical) invariant("a closure site's runtime capture matched a stored recursive-binding capture");
                    // A capture of erased function type names no signature
                    // to relate; its type is its site's, at the variables
                    // the rest relates.
                    if (!try self.constTypeHasErased(source, stored_capture.ty)) {
                        try relations.append(self.allocator, .{ capture_ty.*, try self.constType(source, stored_capture.ty) });
                    }
                }
                // A scheme variable neither the value's type nor its captures
                // mention reaches no value of the restored closure from
                // outside; it seals as every scope seals a variable no use
                // relates.
                const related = try self.relateVariablesPartial(relations.items, scope_vars.len);
                defer self.allocator.free(related);
                // A variable only the closure's requirements mention (its own
                // scheme's, then its owner's) is fixed by relating those
                // requirements' methods.
                {
                    var params: std.ArrayList(static_dispatch.EvidenceParamRecord) = .empty;
                    defer params.deinit(self.allocator);
                    if (own) |scheme| {
                        const templates = owner_view.view.checked_procedure_templates;
                        const scope_params = lower.poolSlice(static_dispatch.EvidenceParamRecord, templates.evidence_params_pool, templates.dispatch_scopes[@backingInt(own_scope.?)].evidence_params);
                        for (scheme.dictionaries) |param_index| try params.append(self.allocator, scope_params[param_index]);
                    }
                    if (owner.dictionary_params.len != 0) {
                        const owner_params = self.templateEvidenceParams(owner_worker);
                        for (owner.dictionary_params) |param_index| try params.append(self.allocator, owner_params[param_index]);
                    }
                    // A derived codec's or map's instantiation is the
                    // callable its stored evidence recorded: its own
                    // scheme's frame, then its owner's.
                    var recorded: std.ArrayList([2]TypeId) = .empty;
                    defer recorded.deinit(self.allocator);
                    if (own) |scheme| if (scheme.dictionaries.len != 0) {
                        const head = stored.evidence_frame_head orelse invariant("a stored generalized lambda with dictionaries has no evidence");
                        const templates = owner_view.view.checked_procedure_templates;
                        const scope_params = lower.poolSlice(static_dispatch.EvidenceParamRecord, templates.evidence_params_pool, templates.dispatch_scopes[@backingInt(own_scope.?)].evidence_params);
                        try self.recordedDerivedCallables(source, stored, head, scope_params, owner.module, generic, &recorded);
                    };
                    if (owner.dictionary_params.len != 0) {
                        var frame_index = stored.evidence_frame_head orelse invariant("a stored function value with dictionaries has no evidence");
                        while (stored.evidence_frames[frame_index].parent) |parent| frame_index = parent;
                        try self.recordedDerivedCallables(source, stored, frame_index, self.templateEvidenceParams(owner_worker), owner.module, generic, &recorded);
                    }
                    const from_edges = try self.relateVariablesPartial(recorded.items, scope_vars.len);
                    defer self.allocator.free(from_edges);
                    for (related, from_edges) |*slot, determined| {
                        if (slot.* == null) slot.* = determined;
                    }
                    try self.program.relateThroughRequirements(owner.module, scope_vars, params.items, related);
                }
                const bound = try self.allocator.alloc(TypeId, scope_vars.len);
                defer self.allocator.free(bound);
                for (related, bound, scope_vars) |ty, *slot, variable| slot.* = ty orelse try self.program.converter().sealedScheme(owner.module, variable);
                try self.program.resolveKindSlots(try self.program.schemeKinds(&.{generic_fn}, scope_vars.len), bound);
                const concrete: types.Env = .{ .module = owner.module, .vars = scope_vars, .bound = bound };
                const concrete_capture_types = try self.allocator.alloc(TypeId, captures.len);
                defer self.allocator.free(concrete_capture_types);
                for (captures, concrete_capture_types) |capture, *concrete_ty| {
                    concrete_ty.* = try self.program.converter().convert(owner.module, owner_view.bodies().patternType(capture.pattern), concrete);
                }
                const worker = try self.program.lambdaWorker(owner_worker, lambda_expr, captures, capture_types, generic_fn, own);
                const descs = try self.program.arena.allocator().alloc(Word, bound.len);
                for (bound, descs) |ty, *desc| desc.* = try self.descWord(ty);
                const owner_dicts = if (nested.default_root != null) try self.allocator.alloc(Word, 0) else try self.storedDictionaries(source, stored, owner_worker, bound[0..owner.scheme_vars.len]);
                defer self.allocator.free(owner_dicts);
                // A generalized lambda's own dictionaries are the stored
                // evidence's innermost frame, its own scope's, ahead of its
                // owner's.
                const own_dicts: []Word = if (own) |scheme| own: {
                    if (scheme.dictionaries.len == 0) break :own try self.allocator.alloc(Word, 0);
                    const head = stored.evidence_frame_head orelse invariant("a stored generalized lambda with dictionaries has no evidence");
                    const frame = stored.evidence_frames[head];
                    if (frame.scope() == .root) invariant("a stored generalized lambda's innermost evidence frame is its procedure's");
                    const templates = owner_view.view.checked_procedure_templates;
                    const params = lower.poolSlice(static_dispatch.EvidenceParamRecord, templates.evidence_params_pool, templates.dispatch_scopes[@backingInt(own_scope.?)].evidence_params);
                    const roots = try self.storedEvidenceTrees(source, stored.evidence, frame.roots_start, frame.roots_len, params, concrete);
                    defer self.allocator.free(roots);
                    const out = try self.allocator.alloc(Word, scheme.dictionaries.len);
                    for (scheme.dictionaries, out) |param_index, *dict| dict.* = roots[param_index] orelse invariant("a stored dictionary's evidence denotes no dictionary");
                    break :own out;
                } else try self.allocator.alloc(Word, 0);
                defer self.allocator.free(own_dicts);
                // The closure's literal arguments, derived from its scopes'
                // types as a stored closure records no edge.
                const closure_worker = self.program.worker(worker).*;
                const literals = try self.allocator.alloc(Word, closure_worker.own_literals + closure_worker.capturedLiteralCount());
                defer self.allocator.free(literals);
                try self.derivedLiteralArgs(worker, bound, literals);
                const dicts = try std.mem.concat(self.allocator, Word, &.{ own_dicts, owner_dicts, literals });
                defer self.allocator.free(dicts);
                try self.push(.{ .finish_restore_closure = .{
                    .worker = worker,
                    .count = @intCast(captures.len),
                    .descs = descs,
                    .dicts = try self.program.arena.allocator().dupe(Word, dicts),
                    .capture_types = try self.program.arena.allocator().dupe(TypeId, concrete_capture_types),
                    .worker_ty = try self.program.converter().convert(owner.module, expr.ty, concrete),
                    .ty = fn_ty,
                } });
                var index = captures.len;
                while (index > 0) {
                    index -= 1;
                    try self.push(.{ .restore = .{ .source = source, .node = stored_captures[index].value, .ty = concrete_capture_types[index] } });
                }
            },
            inline .parser_runtime, .encoder_for_runtime => |runtime| try Codec.restoreRuntime(self, source, fn_id, runtime.owner, runtime.expr, fn_ty),
        }
    }

    /// Restore a completed function as immutable code and capture references.
    pub fn restoreStoredFn(self: *Builder, source: lower.ConstSource, recipe: []const u8, captures: []const checked.ConstNodeId) Allocator.Error!void {
        const decoded = try StoredFn.decode(self, recipe);
        const shape = StoredFn.captureShape(self.program, decoded.worker);
        const arena = self.program.arena.allocator();
        if (captures.len != shape.values.len) invariant("a stored function's captures differ from its worker's");
        if (!try self.allClosed(shape.values) and decoded.capture_descs.len != captures.len) invariant("a stored dynamic capture record has the wrong descriptor arity");
        const capture_types = try arena.alloc(TypeId, shape.values.len);
        for (shape.values, capture_types) |generic, *concrete| concrete.* = try self.program.substitute(self.allocator, generic, decoded.bound);
        try self.push(.{ .finish_stored_fn = .{ .worker = decoded.worker, .shape = shape, .inputs = decoded.inputs, .capture_types = capture_types, .capture_descs = decoded.capture_descs } });
        var index = shape.values.len;
        while (index > 0) {
            index -= 1;
            try self.push(.{ .restore = .{ .source = source, .node = captures[index], .ty = capture_types[index] } });
        }
    }

    /// The function values of generated codec family `family`'s local
    /// procedures, at its variables' types `bound`, for restoring stored
    /// generated runtime `stored` of procedure `owner`, into `out`. The
    /// procedures are local procedures of `owner`'s body, built in the
    /// declaration context of the instance of `owner` that built the
    /// runtime: its function type, which the value records, and its stored
    /// evidence.
    pub fn storedRuntimeLocals(self: *Builder, source: lower.ConstSource, stored: check.ConstStore.ConstFn, owner: check.CheckedNames.ProcTemplate, out: []LocalId, family: u32, bound: []const TypeId) Allocator.Error!void {
        const owner_worker = try self.program.templateRefWorker(owner);
        const target = self.program.worker(owner_worker).*;
        const owner_fn_ty = stored.owner_fn_ty orelse invariant("a stored generated runtime calling local procedures records no instance of its procedure");
        const formal = try self.program.store.intern(.{ .function = .{ .args = target.params, .ret = target.ret, .effectful = false } });
        const owner_bound = (try self.program.instantiateTarget(owner_worker, formal, try self.constType(source, owner_fn_ty))).bound;
        const descs = try self.allocator.alloc(Word, owner_bound.len);
        defer self.allocator.free(descs);
        for (owner_bound, descs) |ty, *desc| desc.* = try self.descWord(ty);
        const restored = try self.storedDictionaries(source, stored, owner_worker, owner_bound);
        defer self.allocator.free(restored);
        const dicts = try self.allocator.alloc(?Word, restored.len);
        defer self.allocator.free(dicts);
        for (dicts, restored) |*dict, dictionary| dict.* = dictionary;
        const building = try self.allocator.alloc(bool, restored.len);
        defer self.allocator.free(building);
        @memset(building, false);
        const literals = try self.allocator.alloc(Word, target.literal_params);
        defer self.allocator.free(literals);
        try self.derivedLiteralArgs(owner_worker, owner_bound, literals);
        var declaration: Declaration = .{
            .worker = owner_worker,
            .bound = owner_bound,
            .words = .{ .descs = descs, .literals = literals },
            .dicts = dicts,
            .building = building,
            .source = .restored,
            .outer = self.declaration,
        };
        self.declaration = &declaration;
        defer self.declaration = declaration.outer;
        try Codec.localValues(self, family, bound, out);
    }

    /// The lambda worker of expression `lambda_expr` of the worker
    /// `owner_worker`, as a stored function names it: the closure's scheme
    /// and capture types in its owner's variables, which every construction
    /// of it shares.
    pub fn storedLambdaWorker(self: *Builder, owner_worker: WorkerId, lambda_expr: ExprId, captured_scheme: bool) Allocator.Error!WorkerId {
        const owner = self.program.worker(owner_worker).*;
        const owner_view = self.program.module(owner.module);
        const expr = owner_view.bodies().expr(lambda_expr);
        const captures = try self.program.siteCaptures(owner.module, lambda_expr);
        const own: ?lower.LocalScheme = if (try self.program.localScope(owner.module, lambda_expr)) |scope_id| blk: {
            var scheme = try self.localSchemeIn(owner_view, scope_id);
            if (scheme.isEmpty()) break :blk null;
            if (captured_scheme) scheme.per_call = false;
            break :blk scheme;
        } else null;
        const scope_vars = if (own) |scheme| try std.mem.concat(self.allocator, checked.CheckedTypeId, &.{ owner.scheme_vars, scheme.scheme_vars }) else try self.allocator.dupe(checked.CheckedTypeId, owner.scheme_vars);
        defer self.allocator.free(scope_vars);
        const variables = try self.allocator.alloc(TypeId, scope_vars.len);
        defer self.allocator.free(variables);
        for (variables, 0..) |*variable, index| variable.* = try self.program.store.variable(@intCast(index));
        const generic: types.Env = .{ .module = owner.module, .vars = scope_vars, .bound = variables };
        const generic_fn = try self.program.converter().convert(owner.module, expr.ty, generic);
        const capture_types = try self.allocator.alloc(TypeId, captures.len);
        defer self.allocator.free(capture_types);
        for (captures, capture_types) |capture, *capture_ty| {
            capture_ty.* = try self.program.converter().convert(owner.module, owner_view.bodies().patternType(capture.pattern), generic);
        }
        return try self.program.lambdaWorker(owner_worker, lambda_expr, captures, capture_types, generic_fn, own);
    }

    /// The dictionaries a stored function value's procedure was
    /// instantiated with: for each of the worker `owner`'s dictionary
    /// parameters, the dictionary its stored evidence denotes, with the
    /// owner's scheme variables bound to `bound`.
    pub fn storedDictionaries(self: *Builder, source: lower.ConstSource, stored: check.ConstStore.ConstFn, owner: WorkerId, bound: []const TypeId) Allocator.Error![]Word {
        const owner_worker = self.program.worker(owner).*;
        const dicts = try self.allocator.alloc(Word, owner_worker.dictionary_params.len);
        errdefer self.allocator.free(dicts);
        if (dicts.len == 0) return dicts;
        // The procedure's evidence is the outermost frame's roots.
        var frame_index = stored.evidence_frame_head orelse invariant("a stored function value with dictionaries has no evidence");
        while (stored.evidence_frames[frame_index].parent) |parent| frame_index = parent;
        const frame = stored.evidence_frames[frame_index];
        if (frame.scope() != .root) invariant("a stored function value's outermost evidence frame is not its procedure's");
        const owner_record = self.program.worker(owner).*;
        const roots = try self.storedEvidenceTrees(source, stored.evidence, frame.roots_start, frame.roots_len, self.templateEvidenceParams(owner), .{ .module = owner_record.module, .vars = owner_record.scheme_vars, .bound = bound });
        defer self.allocator.free(roots);
        for (owner_worker.dictionary_params, dicts) |param_index, *dict| {
            dict.* = roots[param_index] orelse invariant("a stored dictionary's evidence denotes no dictionary");
        }
        return dicts;
    }

    /// Append, for each derived codec or map root of stored evidence frame
    /// `frame_index` (whose roots are `params`, over `env`), the parameter's
    /// callable type paired with the instantiated callable its stored edge
    /// recorded.
    fn recordedDerivedCallables(self: *Builder, source: lower.ConstSource, stored: check.ConstStore.ConstFn, frame_index: u32, params: []const static_dispatch.EvidenceParamRecord, module_index: types.ModuleIndex, env: types.Env, out: *std.ArrayList([2]TypeId)) Allocator.Error!void {
        const frame = stored.evidence_frames[frame_index];
        const roots = try self.storedEvidenceRoots(stored.evidence, frame.roots_start, frame.roots_len);
        defer self.allocator.free(roots);
        for (roots, 0..) |node_index, position| switch (stored.evidence.nodes[node_index]) {
            .structural => |index| if (stored.evidence.structural(index).checked) |checked_edge| {
                if (position >= params.len) invariant("stored evidence has more entries than its scope has evidence parameters");
                const edge = try Codec.storedEdge(self, source, checked_edge);
                const edge_callable = try self.program.converter().convert(edge.module, edge.callable_ty, .{ .module = edge.module, .vars = &.{}, .bound = &.{} });
                try out.append(self.allocator, .{ try self.program.converter().convert(module_index, params[position].callable_ty, env), edge_callable });
            },
            .target, .from_callable, .from_scheme, .unreachable_value, .checked_error => {},
        };
    }

    /// The node indices of `count` stored evidence trees starting at
    /// `start`: each tree's root, skipping its nested evidence and callable
    /// contracts.
    pub fn storedEvidenceRoots(self: *Builder, evidence: check.ConstStore.ConstFnEvidenceVector, start: u32, count: u32) Allocator.Error![]u32 {
        const roots = try self.allocator.alloc(u32, count);
        errdefer self.allocator.free(roots);
        // Entries left to read in each open vector; the outermost is the
        // roots.
        var open: std.ArrayList(u32) = .empty;
        defer open.deinit(self.allocator);
        try open.append(self.allocator, count);
        var cursor: u32 = start;
        var root_count: u32 = 0;
        while (open.items.len > 0) {
            const top = &open.items[open.items.len - 1];
            if (top.* == 0) {
                _ = open.pop();
                continue;
            }
            top.* -= 1;
            if (open.items.len == 1) {
                roots[root_count] = cursor;
                root_count += 1;
            }
            if (cursor >= evidence.nodes.len) invariant("stored evidence ended before its declared vector length");
            const node = evidence.nodes[cursor];
            cursor += 1;
            const contracts = evidence.callableContracts(node);
            if (contracts != 0) try open.append(self.allocator, contracts);
            switch (node) {
                .target => |target| switch (evidence.target(target).nested) {
                    .resolved => |nested| if (nested.count != 0) try open.append(self.allocator, nested.count),
                    .from_callable => {},
                },
                .structural, .from_callable, .from_scheme, .unreachable_value, .checked_error => {},
            }
        }
        return roots;
    }

    /// A template worker's evidence parameters.
    pub fn templateEvidenceParams(self: *Builder, worker_id: WorkerId) []const static_dispatch.EvidenceParamRecord {
        const key = switch (self.program.worker(worker_id).key) {
            .template => |key| key,
            .adapter, .default, .lambda, .method_entry, .structural, .structural_map, .root, .codec, .static_data, .crash_dictionary, .convert, .literal_thunk, .root_slot, .callable_eval_entry => invariant("evidence parameters of a worker that is not a procedure template's"),
        };
        const view = self.program.module(key.module).view;
        const template = view.checked_procedure_templates.templates.items[@backingInt(key.template)];
        return lower.poolSlice(static_dispatch.EvidenceParamRecord, view.checked_procedure_templates.evidence_params_pool, template.evidence_params);
    }

    /// The dictionaries of the stored evidence trees of `owner`'s evidence
    /// parameters, `count` pre-order trees starting at `start`: a target's
    /// nested vector is the evidence of the target's own parameters, and
    /// each entry's callable contracts, which no dictionary reads, follow
    /// it. A target takes the instantiation its parameter's callable type,
    /// bound by its owner's instantiation, relates it to.
    pub fn storedEvidenceTrees(self: *Builder, source: lower.ConstSource, evidence: check.ConstStore.ConstFnEvidenceVector, start: u32, count: u32, root_params: []const static_dispatch.EvidenceParamRecord, root_env: types.Env) Allocator.Error![]?Word {
        const Parsed = struct {
            node: u32,
            /// The parsed target whose nested vector holds this entry and
            /// the entry's position there; null for a root.
            parent: ?u32,
            position: u32,
            /// Whether the entry is a callable contract.
            contract: bool = false,
            children: std.ArrayList(u32) = .empty,
            target: ?WorkerId = null,
            bound: []const TypeId = &.{},
        };
        var parsed: std.ArrayList(Parsed) = .empty;
        defer {
            for (parsed.items) |*entry| {
                entry.children.deinit(self.allocator);
                self.allocator.free(entry.bound);
            }
            parsed.deinit(self.allocator);
        }
        // An open vector: its entries left to read and read so far, and the
        // parsed target whose nested evidence it is; contracts and roots
        // have no owner.
        const Open = struct { remaining: u32, read: u32 = 0, owner: ?u32, contracts: bool };
        var open: std.ArrayList(Open) = .empty;
        defer open.deinit(self.allocator);
        var roots: std.ArrayList(u32) = .empty;
        defer roots.deinit(self.allocator);
        try open.append(self.allocator, .{ .remaining = count, .owner = null, .contracts = false });
        var cursor: u32 = start;
        while (open.items.len > 0) {
            const top = &open.items[open.items.len - 1];
            if (top.remaining == 0) {
                _ = open.pop();
                continue;
            }
            top.remaining -= 1;
            const position = top.read;
            top.read += 1;
            const contracts_vector = top.contracts;
            const parent = top.owner;
            const is_root = open.items.len == 1;
            if (cursor >= evidence.nodes.len) invariant("stored evidence ended before its declared vector length");
            const node = evidence.nodes[cursor];
            cursor += 1;
            const index: u32 = @intCast(parsed.items.len);
            if (contracts_vector) {
                try parsed.append(self.allocator, .{ .node = cursor - 1, .parent = null, .position = 0, .contract = true });
            } else {
                try parsed.append(self.allocator, .{ .node = cursor - 1, .parent = parent, .position = position });
                if (parent) |owner_index| try parsed.items[owner_index].children.append(self.allocator, index);
                if (is_root) try roots.append(self.allocator, index);
            }
            const contracts = evidence.callableContracts(node);
            if (contracts != 0) try open.append(self.allocator, .{ .remaining = contracts, .owner = null, .contracts = true });
            switch (node) {
                .target => |target| switch (evidence.target(target).nested) {
                    .resolved => |nested| if (nested.count != 0) try open.append(self.allocator, .{ .remaining = nested.count, .owner = index, .contracts = false }),
                    .from_callable => {},
                },
                .structural, .from_callable, .from_scheme, .unreachable_value, .checked_error => {},
            }
        }
        // Parents precede their children: bind each target's scheme from
        // its parameter's callable type at its parent's instantiation.
        const callables = try self.allocator.alloc(?TypeId, parsed.items.len);
        defer self.allocator.free(callables);
        const dispatchers = try self.allocator.alloc(?TypeId, parsed.items.len);
        defer self.allocator.free(dispatchers);
        for (parsed.items, callables, dispatchers) |*entry, *callable, *dispatcher| {
            callable.* = null;
            dispatcher.* = null;
            if (entry.contract) continue;
            const params, const env: types.Env = if (entry.parent) |p| blk: {
                const parent = parsed.items[p];
                const owner_record = self.program.worker(parent.target.?).*;
                break :blk .{ self.templateEvidenceParams(parent.target.?), .{ .module = owner_record.module, .vars = owner_record.scheme_vars, .bound = parent.bound } };
            } else .{ root_params, root_env };
            if (entry.position >= params.len) invariant("stored evidence has more entries than its owner has evidence parameters");
            const param = params[entry.position];
            callable.* = try self.program.converter().convert(env.module, param.callable_ty, env);
            dispatcher.* = try self.program.converter().convert(env.module, param.dispatcher_ty, env);
            switch (evidence.nodes[entry.node]) {
                .target => |target_index| {
                    const stored = evidence.target(target_index).*;
                    const target = try self.methodTargetWorker(stored.method);
                    const target_worker = self.program.worker(target).*;
                    const formal = try self.program.store.intern(.{ .function = .{ .args = target_worker.params, .ret = target_worker.ret, .effectful = false } });
                    entry.target = target;
                    // A scheme variable only the target's own requirements
                    // mention is fixed by relating those requirements'
                    // methods (`Program.instantiateTarget`).
                    entry.bound = try self.allocator.dupe(TypeId, (try self.program.instantiateTarget(target, formal, callable.*.?)).bound);
                },
                .structural, .from_callable, .from_scheme, .unreachable_value, .checked_error => {},
            }
        }
        // Children follow their parents, so the reverse order builds every
        // nested dictionary before the dictionary that captures it.
        const dicts = try self.allocator.alloc(?Word, parsed.items.len);
        defer self.allocator.free(dicts);
        @memset(dicts, null);
        var index = parsed.items.len;
        while (index > 0) {
            index -= 1;
            const entry = parsed.items[index];
            if (callables[index] == null) continue;
            dicts[index] = switch (evidence.nodes[entry.node]) {
                .target => blk: {
                    const target_worker = self.program.worker(entry.target.?).*;
                    const hidden = try self.allocator.alloc(Word, entry.bound.len + target_worker.dictionary_params.len + target_worker.literal_params);
                    defer self.allocator.free(hidden);
                    for (entry.bound, hidden[0..entry.bound.len]) |ty, *desc| desc.* = try self.descWord(ty);
                    try self.derivedLiteralArgs(entry.target.?, entry.bound, hidden[entry.bound.len + target_worker.dictionary_params.len ..]);
                    for (target_worker.dictionary_params, hidden[entry.bound.len..][0..target_worker.dictionary_params.len]) |param_index, *dict| {
                        if (param_index >= entry.children.items.len) invariant("stored method evidence does not cover its target's dictionaries");
                        dict.* = dicts[entry.children.items[param_index]] orelse invariant("a stored dictionary's evidence denotes no dictionary");
                    }
                    break :blk try self.composeDictionary(try self.program.methodEntryWorker(entry.target.?), hidden);
                },
                .structural => |structural_index| try self.storedStructuralDictionary(source, evidence.structural(structural_index).*, dispatchers[index].?, callables[index].?),
                // Evidence no dictionary reads restores to none.
                .from_callable, .from_scheme, .unreachable_value, .checked_error => null,
            };
        }
        const result = try self.allocator.alloc(?Word, roots.items.len);
        for (roots.items, result) |root, *dict| dict.* = dicts[root];
        return result;
    }

    pub fn storedStructuralDictionary(self: *Builder, source: lower.ConstSource, stored: check.ConstStore.ConstFnStructuralEvidence, dispatcher: TypeId, callable: TypeId) Allocator.Error!?Word {
        const method: lower.StructuralMethod = switch (stored.derivation) {
            .equality => .is_eq,
            .hash => .to_hash,
            .parser, .encoder => {
                const edge = try Codec.storedEdge(self, source, stored.checked orelse invariant("stored structural codec evidence records no checked edge"));
                return try Codec.dictionaryAt(self, edge.module, edge.id orelse invariant("stored structural codec evidence names no generated derivation"), callable);
            },
            .map, .map_effectful => |plan| return try self.structuralMapDictionary(plan, stored.derivation == .map_effectful, callable),
        };
        return try self.composeDictionary(try self.program.structuralWorker(self.module, method), &.{try self.descWord(dispatcher)});
    }

    /// A read of immutable target data. No runtime constructor exists.
    pub fn staticRead(self: *Builder, slot: LIR.StaticDataId) Allocator.Error!LocalId {
        const local = try self.closedLocal(self.program.result.static_data_values.items[@backingInt(slot)].layout_idx);
        try self.emitLinear(.{ .assign_literal = .{ .target = local, .value = .{ .static_data = .{ .id = slot } }, .next = undefined } });
        try self.constant_slots.put(self.allocator, local, slot);
        return local;
    }

    fn storedDataLocal(self: *Builder, source: lower.ConstSource, node: checked.ConstNodeId, ty: TypeId) Allocator.Error!LocalId {
        return self.staticRead(try self.storedDataSlot(source, node, ty));
    }

    fn storedDataSlot(self: *Builder, source: lower.ConstSource, node: checked.ConstNodeId, ty: TypeId) Allocator.Error!LIR.StaticDataId {
        if (self.program.stored_value_slots.get(.{ .module = source.module, .node = node, .ty = ty })) |slot| return slot;
        const value_layout = try self.program.layouts.layoutOf(ty);
        const plan = try @import("const_schema.zig").constPlanOf(self.program, ty);
        const slot: LIR.StaticDataId = @fromBackingInt(@intCast(self.program.result.static_data_values.items.len));
        try self.program.result.static_data_values.append(self.allocator, .{
            .initializer = null,
            .layout_idx = value_layout,
            .stored = .{ .module = self.program.module(source.module).key, .node = node, .plan = plan },
        });
        return slot;
    }

    pub fn constantSlot(self: *Builder, value: LocalId) Allocator.Error!LIR.StaticDataId {
        if (self.constant_slots.get(value)) |slot| return slot;
        invariant("a completed compile-time value has no immutable data slot");
    }

    pub fn staticPartsLocal(self: *Builder, value_layout: layout.Idx, kind: @FieldType(lir_core.Program.StaticDataParts, "kind"), values: []const LocalId) Allocator.Error!LocalId {
        const children = try self.allocator.alloc(LIR.StaticDataId, values.len);
        errdefer self.allocator.free(children);
        for (values, children) |value, *child| child.* = try self.constantSlot(value);
        const slot: LIR.StaticDataId = @fromBackingInt(@intCast(self.program.result.static_data_values.items.len));
        try self.program.result.static_data_values.append(self.allocator, .{
            .initializer = null,
            .layout_idx = value_layout,
            .parts = .{ .kind = kind, .children = children },
        });
        return self.staticRead(slot);
    }

    fn finishStaticParts(self: *Builder, ty: TypeId, count: u32, kind: @FieldType(lir_core.Program.StaticDataParts, "kind")) Allocator.Error!void {
        const values = try self.popValues(count);
        defer self.allocator.free(values);
        const value_layout = try self.program.layouts.layoutOf(ty);
        // Box(function) has the callable's own representation, with no
        // second allocation or pointer around it.
        if (kind == .box and self.program.result.layouts.getLayout(value_layout).tag == .erased_callable) {
            try self.pushValue(values[0]);
            return;
        }
        try self.pushValue(try self.staticPartsLocal(value_layout, kind, values));
    }

    /// What a procedure use calls: a template's worker, the worker an exact
    /// procedure alias forwards to, or a function value compile-time
    /// evaluation stored.
    pub const Callee = union(enum) {
        worker: WorkerId,
        alias: AliasCallee,
        callable_eval: struct { module: ModuleIndex, template: checked.CallableEvalTemplateId },

        /// The worker a procedure use reaches.
        fn target(self: Callee) WorkerId {
            return switch (self) {
                .worker => |worker_id| worker_id,
                .alias => |alias| alias.target,
                .callable_eval => invariant("a computed callable has no worker"),
            };
        }
    };

    /// The exact procedure alias whose callable-eval template is `template`
    /// of `module`, and the worker its chain reaches. A use instantiates the
    /// alias's scheme, not the worker's (design.md "Procedure Aliases").
    pub const AliasCallee = struct {
        target: WorkerId,
        module: ModuleIndex,
        template: checked.CallableEvalTemplateId,
    };

    pub fn procedureUseCallee(self: *Builder, use: checked.ProcedureUseTemplate) Allocator.Error!Callee {
        return try self.bindingCallee(use.binding);
    }

    /// What a procedure binding calls: for an exact procedure alias, the
    /// alias and the worker its chain reaches.
    pub fn bindingCallee(self: *Builder, binding: @FieldType(checked.ProcedureUseTemplate, "binding")) Allocator.Error!Callee {
        const resolved = self.program.procedureBinding(binding);
        switch (resolved.body) {
            .direct_template => |direct| return .{ .worker = try self.program.templateRefWorker(Program.directTemplate(direct.template)) },
            .checked_error => invariant("a call reached a rejected procedure binding"),
            .callable_eval_template => |template_id| {
                const owner = self.program.module(resolved.module);
                const template = owner.view.callable_eval_templates.templates[@backingInt(template_id)];
                if (template.forwarded_lookup == null) return .{ .callable_eval = .{ .module = resolved.module, .template = template_id } };
                return .{ .alias = try self.aliasCallee(resolved.module, template_id) };
            },
        }
    }

    /// The exact procedure alias `template_id` of `module`, with the worker
    /// its chain reaches. Checking forwards only to a procedure or another
    /// forwarding alias, so a chain ends in no computed callable.
    pub fn aliasCallee(self: *Builder, module: ModuleIndex, template_id: checked.CallableEvalTemplateId) Allocator.Error!AliasCallee {
        var binding = self.forwardedBinding(module, template_id);
        while (true) {
            const resolved = self.program.procedureBinding(binding);
            switch (resolved.body) {
                .direct_template => |direct| return .{ .target = try self.program.templateRefWorker(Program.directTemplate(direct.template)), .module = module, .template = template_id },
                .checked_error => invariant("a procedure alias forwards to a rejected binding"),
                .callable_eval_template => |next| binding = self.forwardedBinding(resolved.module, next),
            }
        }
    }

    /// The procedure binding that the exact procedure alias whose
    /// callable-eval template is `template_id` of `module` forwards to.
    fn forwardedBinding(self: *Builder, module: types.ModuleIndex, template_id: checked.CallableEvalTemplateId) @FieldType(checked.ProcedureUseTemplate, "binding") {
        const owner = self.program.module(module);
        const template = owner.view.callable_eval_templates.templates[@backingInt(template_id)];
        const forwarded = template.forwarded_lookup orelse invariant("a callable-eval template that is no alias forwards nothing");
        const ref_id = owner.view.resolved_value_refs.lookupIdByExpr(forwarded) orelse
            invariant("a procedure alias's lookup has no resolved reference");
        var ref = owner.view.resolved_value_refs.records[@backingInt(ref_id)].ref;
        while (true) switch (ref) {
            .local_proc => |local| {
                if (!local.is_alias) invariant("a top-level procedure alias names a top-level procedure, as checking forwards only those");
                const final = local.alias_target orelse invariant("a callable alias has no recorded target");
                ref = owner.view.resolved_value_refs.records[@backingInt(final)].ref;
            },
            .local_param, .local_value, .local_mutable_version, .pattern_binder, .selected_hoisted_const, .top_level_const, .imported_const, .top_level_proc, .imported_proc, .hosted_proc, .platform_required_declaration, .platform_required_checked_error, .platform_required_const, .platform_required_proc, .promoted_top_level_proc => break,
        };
        return switch (ref) {
            .top_level_proc, .imported_proc, .hosted_proc, .promoted_top_level_proc => |next| next.binding,
            .platform_required_proc => |required| required.procedure.binding,
            .local_param, .local_value, .local_mutable_version, .pattern_binder, .local_proc, .selected_hoisted_const, .top_level_const, .imported_const, .platform_required_declaration, .platform_required_checked_error, .platform_required_const => invariant("a procedure alias forwards only to a procedure, as checking requires"),
        };
    }

    pub fn lowerCall(self: *Builder, expr_id: ExprId, call: @FieldType(bodies_mod.ExprData, "call")) Allocator.Error!void {
        const func = self.bodies().expr(call.func);
        const resolved: ?checked.ResolvedValueRefId = switch (func.data) {
            .lookup_external => self.bodies().resolvedRef(call.func) orelse invariant("a call's external callee was unresolved"),
            .lookup_required => self.bodies().resolvedRef(call.func) orelse invariant("a call's platform requirement callee was unresolved"),
            .lookup_local => self.bodies().resolvedRef(call.func),
            .numeral, .str_segment, .str, .bytes_literal, .list, .empty_list, .tuple, .match_, .if_, .call, .record, .empty_record, .block, .tag, .nominal, .zero_argument_tag, .closure, .lambda, .binop, .unary_minus, .field_access, .dispatch_call, .interpolation, .structural_eq, .structural_hash, .method_eq, .type_dispatch_call, .tuple_access, .runtime_error, .crash, .dbg, .expect_err, .expect, .ellipsis, .anno_only, .break_, .return_, .for_, .hosted_lambda, .run_low_level => null,
        };
        const callee: Callee = if (resolved) |id| switch (self.resolvedRef(id).ref) {
            inline .top_level_proc, .imported_proc, .hosted_proc, .promoted_top_level_proc, .platform_required_proc => |ref, tag| try self.procedureUseCallee(if (tag == .platform_required_proc) ref.procedure else ref),
            .local_proc => |local| {
                if (local.is_alias) return try self.lowerValueCall(expr_id, call);
                if (local.dispatch_scope) |scope_id| {
                    const scheme = try self.localScheme(scope_id);
                    if (!scheme.isEmpty()) return try self.lowerGeneralizedLocalCall(expr_id, call, local, scheme);
                }
                return try self.lowerValueCall(expr_id, call);
            },
            .local_param, .local_value, .pattern_binder, .local_mutable_version, .platform_required_const, .top_level_const, .imported_const, .selected_hoisted_const, .platform_required_checked_error => return try self.lowerValueCall(expr_id, call),
            .platform_required_declaration => invariant("a platform requirement reached lowering unpaired with its app binding"),
        } else return try self.lowerValueCall(expr_id, call);
        const call_fn = try self.valueType(call.func);
        if (callee == .callable_eval) {
            const eval = callee.callable_eval;
            try self.push(.{ .finish_erased_call = .{ .fn_ty = call_fn, .arg_count = @intCast(call.args.len), .result = try self.exprType(expr_id) } });
            try self.pushExprsReversed(call.args);
            try self.push(.{ .restore_callable_eval = .{ .module = eval.module, .template = eval.template, .ty = call_fn } });
            return;
        }
        // A platform requirement's call of the app's binding is checked
        // before the two are paired, so checking records no edge at it.
        const required: ?checked.PlatformRequiredProcedureResolvedRef = if (resolved) |id| switch (self.resolvedRef(id).ref) {
            .platform_required_proc => |requirement| requirement,
            .top_level_proc, .imported_proc, .hosted_proc, .promoted_top_level_proc => null,
            .local_proc, .local_param, .local_value, .pattern_binder, .local_mutable_version, .platform_required_const, .top_level_const, .imported_const, .selected_hoisted_const, .platform_required_checked_error, .platform_required_declaration => invariant("a direct call's target is no procedure"),
        } else null;
        const instance = try self.procedureUseInstance(callee, call.func, required, call_fn);
        const arg_types = switch (self.program.store.get(call_fn)) {
            .function => |function| self.program.store.span(function.args),
            .primitive, .empty_record, .record, .tuple, .tag_union, .list, .box, .nominal, .variable => invariant("a called value's type is not a function type"),
        };
        if (arg_types.len != call.args.len) invariant("a call's argument count differs from its callable type");
        try self.push(.{ .finish_call = .{ .callee = callee.target(), .hidden = instance.hidden, .bound = instance.bound, .arg_types = arg_types, .ret = instance.ret, .result = try self.exprType(expr_id) } });
        try self.pushExprsReversed(call.args);
    }

    /// A call of a function value: evaluate the function, then its
    /// arguments, then call through the erased-callable ABI.
    pub fn lowerValueCall(self: *Builder, expr_id: ExprId, call: @FieldType(bodies_mod.ExprData, "call")) Allocator.Error!void {
        try self.push(.{ .finish_erased_call = .{ .fn_ty = try self.valueType(call.func), .arg_count = @intCast(call.args.len), .result = try self.exprType(expr_id) } });
        try self.pushExprsReversed(call.args);
        try self.push(.{ .expr = call.func });
    }

    /// The hidden arguments of a call of `callee` whose function type at the
    /// call is `call_fn`. A substitution entry the checker recorded as
    /// erroneous is a scheme variable the call's own type does not fix: it
    /// takes what the call's function type relates it to, or, related to
    /// nothing, what a variable no use relates seals to.
    pub fn hiddenArgsAt(self: *Builder, callee: WorkerId, subst: []const checked.CheckedTypeId, evidence: []const static_dispatch.CheckedEvidence, call_fn: ?TypeId, edge: checked.LiteralEdge) Allocator.Error![]Word {
        return (try self.callInstanceAt(callee, subst, evidence, call_fn, edge)).hidden;
    }

    /// One instantiation of a direct call of a template worker: its hidden
    /// arguments (owned by the caller), the types its scheme variables take
    /// (arena-owned), and the callee's result type at that instantiation.
    /// A literal chain frame's instance sets only the literal arguments the
    /// frame reads eagerly in `hidden`: `deferred_dicts` holds its dictionaries.
    pub const CallInstance = struct { hidden: []Word, bound: []const TypeId, ret: TypeId, deferred_dicts: ?DeferredDictionaries = null };

    /// `hiddenArgsAt` with the instantiation it computed.
    pub fn callInstanceAt(self: *Builder, callee: WorkerId, subst: []const checked.CheckedTypeId, evidence: []const static_dispatch.CheckedEvidence, call_fn: ?TypeId, edge: checked.LiteralEdge) Allocator.Error!CallInstance {
        const owned = try self.boundTypesAt(callee, subst, call_fn);
        defer self.allocator.free(owned);
        const bound = try self.program.arena.allocator().dupe(TypeId, owned);
        return try self.callInstanceBound(callee, bound, evidence, edge);
    }

    /// The call instance of `callee` whose scheme variables take `bound`
    /// (arena-owned), its dictionaries from `evidence` and its literal
    /// arguments from the checked edge `edge` of this body's module
    /// (design.md "Literal Arguments").
    pub fn callInstanceBound(self: *Builder, callee: WorkerId, bound: []const TypeId, evidence: []const static_dispatch.CheckedEvidence, edge: checked.LiteralEdge) Allocator.Error!CallInstance {
        return try self.callInstanceFrom(callee, bound, evidence, null, edge);
    }

    /// The call instance of a use of platform requirement `required`, whose
    /// target is the app's binding `callee`. Checking pairs the two after the
    /// platform's use, so the use has no evidence of its own: the requirement
    /// carries the root evidence the app's table holds for the binding's
    /// complete scheme, captured codec requirements included (design.md
    /// "Platform/App Relation").
    pub fn requiredInstanceAt(self: *Builder, callee: WorkerId, required: checked.PlatformRequiredProcedureResolvedRef, subst: []const checked.CheckedTypeId, call_fn: ?TypeId, edge: checked.LiteralEdge) Allocator.Error!CallInstance {
        const owned = try self.boundTypesAt(callee, subst, call_fn);
        defer self.allocator.free(owned);
        const bound = try self.program.arena.allocator().dupe(TypeId, owned);
        const span = required.root_evidence orelse return try self.callInstanceFrom(callee, bound, &.{}, null, edge);
        const owner = self.program.moduleByKey(span.checked_module);
        const evidence = owner.view.static_dispatch_plans.evidence_refs[span.span.start..][0..span.span.len];
        return try self.callInstanceFrom(callee, bound, evidence, owner, edge);
    }

    /// `callInstanceBound` with evidence read from `evidence_owner`'s table
    /// when it is not this body's module's, resolved in that module at the
    /// instance's bindings of the callee's scheme.
    fn callInstanceFrom(self: *Builder, callee: WorkerId, bound: []const TypeId, evidence: []const static_dispatch.CheckedEvidence, evidence_owner: ?*const types.ModuleView, edge: checked.LiteralEdge) Allocator.Error!CallInstance {
        // Only this instance's frame reads as it goes, not the instances its
        // dictionaries are built from.
        const chain_frame = self.chain_frame;
        self.chain_frame = false;
        const target = self.program.worker(callee).*;
        const hidden = try self.allocator.alloc(Word, bound.len + target.dictionary_params.len + target.literal_params);
        errdefer self.allocator.free(hidden);
        if (chain_frame) {
            const ret = try self.program.substitute(self.allocator, target.ret, bound);
            if (target.dictionary_params.len == 0) return .{ .hidden = hidden, .bound = bound, .ret = ret };
            const context = self.templateParamContext(callee, bound);
            var deferred = try self.deferredDictionaries(evidence, context, target.dictionary_params.len);
            deferred.declaring = callee;
            if (evidence_owner) |owner| {
                deferred.module = owner.index;
                deferred.view = owner;
                deferred.env = context.env;
                deferred.foreign_scopes = self.alias_scopes.items.len;
            }
            return .{ .hidden = hidden, .bound = bound, .ret = ret, .deferred_dicts = deferred };
        }
        for (bound, hidden[0..bound.len]) |ty, *desc| desc.* = try self.descWord(ty);
        const literals = hidden[bound.len + target.dictionary_params.len ..];
        if (edge == .literal) {
            // A call checking recorded no edge for (a platform requirement's
            // call of the app's binding) derives its literal arguments from
            // its instantiation, as it does its dictionaries.
            try self.derivedLiteralArgs(callee, bound, literals);
        } else {
            try self.fillLiteralArgs(callee, edge, literals);
        }
        if (target.dictionary_params.len != 0) {
            const count = target.dictionary_params.len;
            const dicts = try self.allocator.alloc(?Word, count);
            defer self.allocator.free(dicts);
            @memset(dicts, null);
            const building = try self.allocator.alloc(bool, count);
            defer self.allocator.free(building);
            @memset(building, false);
            var declaration: Declaration = .{
                .worker = callee,
                .bound = bound,
                .words = .{ .descs = hidden[0..bound.len], .literals = literals },
                .dicts = dicts,
                .building = building,
                .source = .{ .call = .{ .evidence = evidence, .context = self.templateParamContext(callee, bound), .owner = evidence_owner } },
                .outer = self.declaration,
            };
            for (hidden[bound.len..][0..count], 0..) |*dict, slot| {
                dict.* = try self.declarationDictionary(&declaration, slot);
            }
        }
        return .{ .hidden = hidden, .bound = bound, .ret = try self.program.substitute(self.allocator, target.ret, bound) };
    }

    /// Dictionary slot `slot`, with a reserved recipe for a recursive read.
    fn declarationDictionary(self: *Builder, declaration: *Declaration, slot: usize) Allocator.Error!Word {
        if (declaration.dicts[slot]) |dictionary| return dictionary;
        if (declaration.building[slot]) {
            const pending: Word = .{ .recipe = try self.recipes.reserve() };
            declaration.dicts[slot] = pending;
            return pending;
        }
        declaration.building[slot] = true;
        // The slot is built in the scope its instance's evidence is read
        // in, whose innermost declaration context is the instance's.
        const saved_declaration = self.declaration;
        self.declaration = declaration;
        defer self.declaration = saved_declaration;
        const param_index = self.program.worker(declaration.worker).dictionary_params[slot];
        const dictionary = switch (declaration.source) {
            .call => |call| blk: {
                if (param_index >= call.evidence.len) invariant("a call's evidence does not cover its target's dictionaries");
                const saved_module = self.module;
                const saved_view = self.view;
                const saved_env = self.env;
                const saved_foreign = self.foreign_scopes;
                defer {
                    self.module = saved_module;
                    self.view = saved_view;
                    self.env = saved_env;
                    self.foreign_scopes = saved_foreign;
                }
                if (call.owner) |owner| {
                    self.module = owner.index;
                    self.view = owner;
                    self.env = call.context.env;
                    self.foreign_scopes = self.alias_scopes.items.len;
                }
                break :blk try self.paramDictionary(call.evidence, param_index, call.context);
            },
            .frame => |frame| try self.frameDictionary(frame.position, param_index),
            .restored => invariant("a restored declaration context's dictionary is neither restored nor being composed"),
        };
        self.completeDeclarationSlot(declaration, slot, dictionary);
        return dictionary;
    }

    /// Complete the reserved node before returning the dictionary's recipe.
    fn completeDeclarationSlot(self: *Builder, declaration: *Declaration, slot: usize, dictionary: Word) void {
        declaration.building[slot] = false;
        if (declaration.dicts[slot]) |pending| switch (dictionary) {
            .recipe => |recipe| self.recipes.setSame(pending.recipe, recipe),
            .static => invariant("a recursive dictionary resolves to no callable recipe"),
        };
        declaration.dicts[slot] = dictionary;
    }

    /// The descriptors and literal arguments of `declaration`, composed at
    /// the first read of a literal chain frame's.
    fn declarationWords(self: *Builder, declaration: *Declaration) Allocator.Error!Declaration.Words {
        if (declaration.words) |words| return words;
        const frame = switch (declaration.source) {
            .frame => |frame| frame,
            .call, .restored => invariant("a call's declaration context has no descriptors or literal arguments"),
        };
        const arena = self.program.arena.allocator();
        const descs = try arena.alloc(Word, declaration.bound.len);
        for (declaration.bound, descs) |ty, *desc| desc.* = try self.descWord(ty);
        const literals = try arena.alloc(Word, self.program.worker(declaration.worker).literal_params);
        for (literals, 0..) |*literal, index| literal.* = try self.scopeFrameLiteral(frame.scope, frame.position, @intCast(index));
        declaration.words = .{ .descs = descs, .literals = literals };
        return declaration.words.?;
    }

    /// The innermost declaration context of the call instances being built
    /// whose callee is `worker`.
    fn declarationOf(self: *const Builder, worker: WorkerId) ?*Declaration {
        var next = self.declaration;
        while (next) |declaration| : (next = declaration.outer) {
            if (declaration.worker == worker) return declaration;
        }
        return null;
    }

    /// The declaration context of the literal chain frame whose
    /// dictionaries are `composed`, when one is being built.
    fn frameDeclaration(self: *const Builder, composed: []?Word) ?*Declaration {
        var next = self.declaration;
        while (next) |declaration| : (next = declaration.outer) {
            switch (declaration.source) {
                .frame => |frame| if (frame.composed == composed.ptr) return declaration,
                .call, .restored => {},
            }
        }
        return null;
    }

    /// The dictionaries of a literal chain frame with `count` parameters,
    /// whose edge is in the current scope: none is composed until it is read
    /// (`frameDictionary`).
    fn deferredDictionaries(self: *Builder, evidence: []const static_dispatch.CheckedEvidence, context: ?ParamContext, count: usize) Allocator.Error!DeferredDictionaries {
        const composed = try self.program.arena.allocator().alloc(?Word, count);
        @memset(composed, null);
        return .{
            .evidence = evidence,
            .context = context,
            .module = self.module,
            .view = self.view,
            .env = self.env,
            .foreign_scopes = self.foreign_scopes,
            .frames = self.alias_scopes.items.len,
            .composed = composed,
        };
    }

    /// The function type an evidence edge instantiated its target at, when
    /// it recorded one.
    pub fn instantiationCallable(self: *Builder, instantiation: static_dispatch.EvidenceTargetInstantiation) Allocator.Error!?TypeId {
        return switch (instantiation) {
            .callable, .alias, .derived_from_callable => |callable| try self.convert(callable),
            .monomorphic => null,
        };
    }

    /// The function type a dispatch calls at, in this body's scope. A
    /// direct call's plan callable is specialized against its target, which
    /// renames every variable it leaves unsubstituted, so it cannot name this
    /// body's scheme variables; the edge's own callable relation does
    /// (design.md "Call Sites"). An edge with no
    /// callable relation has a monomorphic target, whose plan callable holds
    /// no variable of this body.
    pub fn dispatchCallable(self: *Builder, resolution: static_dispatch.CheckedCallResolution, callable_ty: checked.CheckedTypeId) Allocator.Error!TypeId {
        const direct = switch (resolution) {
            .direct_closed, .direct_parametric => |direct| direct,
            .direct_pending, .evidence_dependent, .structural, .checked_error, .@"unreachable" => return try self.convert(callable_ty),
        };
        const node = self.view.view.static_dispatch_plans.evidenceNode(direct.evidence);
        return (try self.instantiationCallable(node.instantiation)) orelse try self.convert(callable_ty);
    }

    /// The types the scheme variables of `callee` take at a call: the
    /// checked substitution's, where an erroneous entry takes what the call's
    /// function type `call_fn` relates it to, or seals.
    pub fn boundTypesAt(self: *Builder, callee: WorkerId, checked_subst: []const checked.CheckedTypeId, call_fn: ?TypeId) Allocator.Error![]TypeId {
        const target = self.program.worker(callee).*;
        const subst = try self.program.keptSubstitution(callee, checked_subst);
        // A use the checker recorded no instantiation for (a platform's use
        // of a procedure the app provides) takes every variable from the
        // call's function type.
        const recorded = subst.len != 0;
        if (recorded and subst.len != target.scheme_vars.len) invariant("a call's checked substitution differs from its target's scheme");
        const bound = try self.allocator.alloc(TypeId, target.scheme_vars.len);
        errdefer self.allocator.free(bound);
        // A row variable takes its open row's remainder of the call's row
        // representative, whose layout the caller's values have (design.md
        // "Row Joins").
        const row_vars = try self.allocator.alloc(bool, target.scheme_vars.len);
        defer self.allocator.free(row_vars);
        @memset(row_vars, false);
        const related: []?TypeId = if (call_fn) |actual| blk: {
            const formal = try self.program.store.intern(.{ .function = .{ .args = target.params, .ret = target.ret, .effectful = false } });
            break :blk try self.program.relateVariablesRows(self.allocator, &.{.{ formal, actual }}, target.scheme_vars.len, row_vars);
        } else try self.allocator.alloc(?TypeId, 0);
        defer self.allocator.free(related);
        // Without a recorded instantiation, a variable the function type
        // doesn't mention is one the target's own requirements fix.
        if (!recorded and related.len != 0) try self.program.relateThroughTargetRequirements(callee, related);
        for (bound, 0..) |*slot, index| {
            const ty = if (index < related.len) related[index] else null;
            if (row_vars[index] and ty != null) {
                slot.* = ty.?;
                continue;
            }
            if (recorded and self.view.types().payload(subst[index]) != .err) {
                slot.* = try self.convert(subst[index]);
                continue;
            }
            slot.* = ty orelse try self.program.converter().sealedScheme(target.module, target.scheme_vars[index]);
        }
        try self.program.resolveKindSlots(target.kinds, bound);
        return bound;
    }

    /// `own_types`, the types a generalized local scheme's variables take at
    /// a use, with its kind variables at their fields' storage. `checked_ty`
    /// is the scheme's root type.
    fn resolveOwnKinds(self: *Builder, checked_ty: checked.CheckedTypeId, scheme: lower.LocalScheme, own_types: []TypeId) Allocator.Error!void {
        const outer = self.env.vars.len;
        const vars = try std.mem.concat(self.allocator, checked.CheckedTypeId, &.{ self.env.vars, scheme.scheme_vars });
        defer self.allocator.free(vars);
        const generic = try self.allocator.alloc(TypeId, vars.len);
        defer self.allocator.free(generic);
        for (generic, 0..) |*slot, index| slot.* = try self.program.store.variable(@intCast(index));
        const env: types.Env = .{ .module = self.module, .vars = vars, .bound = generic };
        const roots = try self.allocator.alloc(TypeId, 1 + 2 * scheme.params.len);
        defer self.allocator.free(roots);
        roots[0] = try self.program.converter().convert(self.module, checked_ty, env);
        for (scheme.params, 0..) |param, index| {
            roots[1 + 2 * index] = try self.program.converter().convert(self.module, param.callable_ty, env);
            roots[2 + 2 * index] = try self.program.converter().convert(self.module, param.dispatcher_ty, env);
        }
        const actual = try std.mem.concat(self.allocator, TypeId, &.{ self.env.bound, own_types });
        defer self.allocator.free(actual);
        try self.program.resolveKindSlots(try self.program.schemeKinds(roots, vars.len), actual);
        @memcpy(own_types, actual[outer..]);
    }

    /// The worker of a method target: the procedure template the target
    /// names.
    pub fn methodTargetWorker(self: *Builder, target: static_dispatch.MethodTarget) Allocator.Error!WorkerId {
        return switch (target.kind) {
            .procedure => |procedure| try self.program.templateRefWorker(procedure.template),
            .local_proc => invariant("a method declared in a function body is a local procedure, called through its function value, never as a worker"),
            .callable_binding => invariant("a method bound to a computed callable is called through its function value, never as a worker"),
            .structural => invariant("a structural method reached a procedure dispatch"),
        };
    }

    /// The function value of the callable-eval binding `binding`, at `fn_ty`:
    /// the value its compile-time root evaluated, or, where this program
    /// evaluated none, the value its entry wrapper computes.
    pub fn callableEvalValueAt(self: *Builder, binding: lower.CallableEvalBinding, fn_ty: TypeId) Allocator.Error!LocalId {
        return try self.lowerNow(.{ .restore_callable_eval = .{ .module = binding.module, .template = binding.template, .ty = fn_ty } });
    }

    /// The function value of a method bound to a computed callable, at the
    /// callable type `fn_ty` a dispatch calls it at.
    pub fn callableBindingMethodValue(self: *Builder, binding: static_dispatch.CallableBindingTarget, fn_ty: TypeId) Allocator.Error!LocalId {
        return try self.callableEvalValueAt(self.program.callableEvalBinding(binding), fn_ty);
    }

    /// The dictionary `evidence` denotes (design.md "Dictionaries").
    pub fn dictionaryOf(self: *Builder, evidence: static_dispatch.CheckedEvidence) Allocator.Error!Word {
        return try self.dictionaryOfAt(evidence, null);
    }

    /// `dictionaryOf`, its method adapted to the callable type `interface`
    /// its consumer calls it at when one is known (the parameter it fills).
    pub fn dictionaryOfAt(self: *Builder, evidence: static_dispatch.CheckedEvidence, interface: ?TypeId) Allocator.Error!Word {
        const table = self.view.view.static_dispatch_plans;
        switch (evidence.resolution) {
            .constraint => |constraint| return try self.scopeDictionary(constraint.index),
            .direct => |node_id| {
                const node = table.evidenceNode(node_id);
                if (node.target.kind == .local_proc) {
                    const fn_ty = (try self.instantiationCallable(node.instantiation)) orelse try self.convert(node.target.callable_ty);
                    return try self.localMethodWord(node, fn_ty);
                }
                if (node.target.kind == .callable_binding) {
                    // The entry restores the value when called, so the
                    // dictionary holds the entry, adapted to the callable
                    // type it is called at.
                    const entry = try self.program.callableEvalEntryWorker(self.module, self.program.callableEvalBinding(node.target.kind.callable_binding));
                    const dictionary = try self.composeDictionary(entry, &.{});
                    const expected = interface orelse (try self.instantiationCallable(node.instantiation)) orelse return dictionary;
                    const entry_worker = self.program.worker(entry).*;
                    const own_fn = try self.program.store.intern(.{ .function = .{ .args = entry_worker.params, .ret = entry_worker.ret, .effectful = false } });
                    return try self.adaptDictionary(dictionary, own_fn, expected);
                }
                const target = try self.methodTargetWorker(node.target);
                const subst = table.site_substitutions[node.subst.start..][0..node.subst.len];
                switch (node.instantiation) {
                    .derived_from_callable, .alias => |callable| {
                        const hidden = if (node.instantiation == .alias) try self.aliasTargetHidden(node, target, .{ .node = node_id }) else try self.relatedHiddenArgs(target, callable);
                        defer self.allocator.free(hidden);
                        return try self.composeDictionary(try self.program.methodEntryWorker(target), hidden);
                    },
                    .monomorphic, .callable => {},
                }
                const callable = try self.instantiationCallable(node.instantiation);
                const hidden = switch (node.nested) {
                    .resolved => try self.hiddenArgsAt(target, subst, table.nestedEvidence(node), callable, .{ .node = node_id }),
                    .from_callable => try self.callableDerivedHidden(target, subst, callable),
                    .through_alias => invariant("alias-scope evidence accompanies only an alias instantiation"),
                };
                defer self.allocator.free(hidden);
                const entry = try self.program.methodEntryWorker(target);
                const dictionary = try self.composeDictionary(entry, hidden);
                // A method whose signature stores record fields in presence
                // slots, or holds rows its consumers' type widens, is adapted
                // to the representation they call it with (design.md "Field
                // Kinds", "Row Coercion Primitive").
                const expected = interface orelse callable orelse return dictionary;
                const target_worker = self.program.worker(target).*;
                const bound = try self.boundTypesAt(target, subst, callable);
                defer self.allocator.free(bound);
                const own_fn = try self.program.store.intern(.{ .function = .{ .args = target_worker.params, .ret = target_worker.ret, .effectful = false } });
                return try self.adaptDictionary(dictionary, try self.program.substitute(self.allocator, own_fn, bound), expected);
            },
            .structural => |structural| {
                const method: lower.StructuralMethod = switch (structural.derivation) {
                    .equality => .is_eq,
                    .hash => .to_hash,
                    .parser, .encoder => return try Codec.dictionary(self, structural),
                    .map, .map_effectful => |plan| return try self.structuralMapDictionary(plan, structural.derivation == .map_effectful, try self.convert(structural.callable_ty)),
                };
                const entry = try self.program.structuralWorker(self.module, method);
                return try self.composeDictionary(entry, &.{try self.descWord(try self.convert(structural.dispatcher_ty))});
            },
            .checked_error => return try self.composeDictionary(try self.program.crashDictionaryWorker(self.module, .checked_error), &.{}),
            // A lambda stored with its own dictionaries takes them where it is
            // constructed. `from_scheme` there means no use instantiated it, so
            // nothing ever reads the dictionary.
            .unreachable_value, .from_scheme => return try self.composeDictionary(try self.program.crashDictionaryWorker(self.module, .unreachable_value), &.{}),
            .from_callable => invariant("callable-derived evidence is resolved with the parameter it stands for (`paramDictionary`)"),
        }
    }

    /// The evidence parameters a dictionary vector stands for, and the types
    /// their scheme takes where the vector is built.
    pub const ParamContext = struct {
        module: ModuleIndex,
        params: []const static_dispatch.EvidenceParamRecord,
        env: types.Env,
    };

    /// The parameter context of template worker `callee` whose scheme takes
    /// `bound`.
    fn templateParamContext(self: *Builder, callee: WorkerId, bound: []const TypeId) ParamContext {
        const target = self.program.worker(callee).*;
        const key = switch (target.key) {
            .template => |key| key,
            .adapter, .default, .lambda, .method_entry, .structural, .structural_map, .root, .codec, .static_data, .crash_dictionary, .convert, .literal_thunk, .root_slot, .callable_eval_entry => invariant("a worker with dictionary parameters is not a template's"),
        };
        const templates = self.program.module(key.module).view.checked_procedure_templates;
        return .{
            .module = key.module,
            .params = lower.poolSlice(static_dispatch.EvidenceParamRecord, templates.evidence_params_pool, templates.templates.items[@backingInt(key.template)].evidence_params),
            .env = .{ .module = key.module, .vars = target.scheme_vars, .bound = bound },
        };
    }

    fn scopeParams(self: *Builder, scope_id: checked.DispatchScopeId) []const static_dispatch.EvidenceParamRecord {
        const templates = self.view.view.checked_procedure_templates;
        return lower.poolSlice(static_dispatch.EvidenceParamRecord, templates.evidence_params_pool, templates.dispatch_scopes[@backingInt(scope_id)].evidence_params);
    }

    /// The parameter context of generalized-local scope `scope_id` whose own
    /// variables `vars` take `own_types`, inside this body's scope.
    fn scopeParamContext(self: *Builder, scope_id: checked.DispatchScopeId, vars: []const checked.CheckedTypeId, own_types: []const TypeId) Allocator.Error!ParamContext {
        const arena = self.program.arena.allocator();
        return .{
            .module = self.module,
            .params = self.scopeParams(scope_id),
            .env = .{
                .module = self.module,
                .vars = try std.mem.concat(arena, checked.CheckedTypeId, &.{ vars, self.env.vars }),
                .bound = try std.mem.concat(arena, TypeId, &.{ own_types, self.env.bound }),
            },
        };
    }

    /// The dictionary for evidence parameter `index` of `context`. An entry
    /// the checker left to the concrete callable (`from_callable`) is the
    /// dictionary of the parameter's dispatcher at the types the context
    /// binds.
    pub fn paramDictionary(self: *Builder, evidence: []const static_dispatch.CheckedEvidence, index: u32, context: ParamContext) Allocator.Error!Word {
        switch (evidence[index].resolution) {
            // A literal chain reads only the requirements of the literal's
            // closed type, and evidence for a closed type names no scope
            // (design.md "Literal Arguments").
            .constraint => |constraint| if (!self.chainHoldsScope(constraint.index.depth)) invariant("a literal chain read evidence of a scope it never entered"),
            .from_callable => {
                if (index >= context.params.len) invariant("callable-derived evidence names a parameter its scheme does not have");
                return try self.derivedParamDictionary(index, context);
            },
            .direct, .structural, .from_scheme, .checked_error, .unreachable_value => {},
        }
        const interface = if (index < context.params.len) try self.program.converter().convert(context.module, context.params[index].callable_ty, context.env) else null;
        return try self.dictionaryOfAt(evidence[index], interface);
    }

    /// Whether the scope `depth` frames out from the innermost entered one
    /// is a frame of the current literal chain. Outside a chain every scope
    /// is the worker's own.
    fn chainHoldsScope(self: *const Builder, depth: u32) bool {
        const first = self.foreign_scopes orelse return true;
        return depth < self.alias_scopes.items.len - first;
    }

    /// The dictionary of scheme parameter `index`, resolved from the types
    /// its instantiation `context` binds its requirement at.
    fn derivedParamDictionary(self: *Builder, index: u32, context: ParamContext) Allocator.Error!Word {
        if (index >= context.params.len) invariant("a dictionary parameter names a requirement its scheme does not have");
        const param = context.params[index];
        const converter = self.program.converter();
        return try self.dictionaryFromType(.{
            .module = context.module,
            .method = param.method,
            .structural = param.structural,
            .dispatcher = try converter.convert(context.module, param.dispatcher_ty, context.env),
            .callable = try converter.convert(context.module, param.callable_ty, context.env),
        });
    }

    /// The dictionary of `root`, resolved from its dispatcher's type
    /// (`Program.resolveRequirement`), with each method target's own
    /// requirements resolved the same way (`Program.dictionaryTree`).
    pub fn dictionaryFromType(self: *Builder, root: lower.DictRequirement) Allocator.Error!Word {
        return try self.dictionaryOfTree(.{ .requirement = root });
    }

    /// The dictionary of the requirement tree `root`: a target's dictionary
    /// is its entry capturing the descriptors of the types its scheme takes,
    /// its children's dictionaries, and literal thunks derived from those
    /// types, each adapted to the callable its requirement names. A target a
    /// back edge returns to is reserved before its children are built, so
    /// the dictionaries of a requirement that reaches itself capture one
    /// another (`materializeCallable`). Children follow their parents in the
    /// tree, so building it from the last node builds every child first.
    fn dictionaryOfTree(self: *Builder, root: lower.DictionaryRoot) Allocator.Error!Word {
        const tree = try self.program.dictionaryTree(root);
        const words = try self.allocator.alloc(?Word, tree.len);
        defer self.allocator.free(words);
        @memset(words, null);
        // The reserved recipe of each target a back edge returns to.
        const heads = try self.allocator.alloc(?Tables.RecipeId, tree.len);
        defer self.allocator.free(heads);
        for (tree, heads) |node, *head| head.* = switch (node.kind) {
            .target => |target| if (target.recursive) try self.recipes.reserve() else null,
            .resolved, .back_edge => null,
        };
        var index = tree.len;
        while (index > 0) {
            index -= 1;
            const node = tree[index];
            words[index] = switch (node.kind) {
                .resolved => |resolution| switch (resolution) {
                    .structural => |structural| try self.composeDictionary(try self.program.structuralWorker(self.module, structural.method), &.{try self.descWord(structural.over)}),
                    .crash => |kind| try self.composeDictionary(try self.program.crashDictionaryWorker(self.module, kind), &.{}),
                    .scope_variable => |variable| try self.variableDictionary(variable, self.program.module(node.requirement.?.module).nameStore().methodNameText(node.requirement.?.method)),
                    .callable_eval => |entry| try self.composeDictionary(entry, &.{}),
                    .derived => invariant("a derived codec or map dictionary is resolved from a type alone, but checking records each with its plan at the dispatch it serves"),
                    .target => invariant("a dictionary's target requirement is not a resolved node"),
                },
                .back_edge => |head| try self.adaptTargetDictionary(.{ .recipe = heads[head].? }, tree[head].kind.target, node.requirement.?.callable),
                .target => |target| blk: {
                    const worker = self.program.worker(target.target).*;
                    const bound = target.instance.bound;
                    const hidden = try self.program.arena.allocator().alloc(Word, bound.len + target.children.len + worker.literal_params);
                    for (bound, hidden[0..bound.len]) |ty, *desc| desc.* = try self.descWord(ty);
                    for (target.children, hidden[bound.len..][0..target.children.len]) |child, *dictionary| dictionary.* = words[child].?;
                    try self.derivedLiteralArgs(target.target, bound, hidden[bound.len + target.children.len ..]);
                    const entry = try self.program.methodEntryWorker(target.target);
                    const dictionary: Word = if (heads[index]) |head| head: {
                        try self.recipes.set(head, try self.dictionaryRecipe(entry, hidden));
                        break :head .{ .recipe = head };
                    } else try self.composeDictionary(entry, hidden);
                    break :blk if (node.requirement) |requirement| try self.adaptTargetDictionary(dictionary, target, requirement.callable) else dictionary;
                },
            };
        }
        return words[0].?;
    }

    /// `dictionary`, of `target`'s method entry, at the method's `callable`.
    fn adaptTargetDictionary(self: *Builder, dictionary: Word, target: lower.DictionaryNode.Target, callable: TypeId) Allocator.Error!Word {
        const worker = self.program.worker(target.target).*;
        const own_fn = try self.program.store.intern(.{ .function = .{ .args = worker.params, .ret = worker.ret, .effectful = false } });
        return try self.adaptDictionary(dictionary, try self.program.substitute(self.allocator, own_fn, target.instance.bound), callable);
    }

    /// The dictionary this body's scopes hold for method `method_text` on
    /// scope variable `index`.
    fn variableDictionary(self: *Builder, index: u32, method_text: []const u8) Allocator.Error!Word {
        const worker = self.currentWorker().*;
        if (index >= worker.scheme_vars.len) invariant("a dispatcher variable is outside its worker's scope");
        const variable = worker.scheme_vars[index];
        const names = self.view.nameStore();
        var offset: usize = 0;
        for (worker.dict_scopes, worker.dict_scope_params) |scope, params| {
            for (scope, 0..) |param_index, position| {
                const param = params[param_index];
                if (param.dispatcher_ty == variable and std.mem.eql(u8, names.methodNameText(param.method), method_text)) return try self.dictInputWord(offset + position);
            }
            offset += scope.len;
        }
        invariant("a scope variable's method requirement has no dictionary in its scope, which checking requires");
    }

    /// The dictionary of a derived map with the checked selection `plan`,
    /// at the function type `fn_ty` (`input, transform -> output`): its
    /// worker captures the descriptors of the input and output unions, of
    /// the selected variant's payloads, and of the transformed payload.
    pub fn structuralMapDictionary(self: *Builder, plan: static_dispatch.DerivedMapPlan, effectful: bool, fn_ty: TypeId) Allocator.Error!Word {
        const store = &self.program.store;
        const function = switch (store.get(fn_ty)) {
            .function => |value| value,
            .primitive, .empty_record, .record, .tuple, .tag_union, .list, .box, .nominal, .variable => invariant("a derived map's callable type is not a function type"),
        };
        const args = try self.allocator.dupe(TypeId, store.span(function.args));
        defer self.allocator.free(args);
        if (args.len != 2) invariant("a derived map does not take a value and a transform");
        const transform = switch (store.get(args[1])) {
            .function => |value| value,
            .primitive, .empty_record, .record, .tuple, .tag_union, .list, .box, .nominal, .variable => invariant("a derived map's transform is not a function"),
        };
        const mapped = transform.ret;
        const name = try store.internName(self.view.nameStore().tagLabelText(plan.tag));
        const input_union = switch (store.get(try self.structuralType(args[0]))) {
            .tag_union => |value| value,
            .primitive, .empty_record, .record, .tuple, .list, .box, .nominal, .function, .variable => invariant("a derived map's input is not a tag union"),
        };
        const payload_types: []const TypeId = for (store.tagSpan(input_union.tags)) |tag| {
            if (tag.name == name) break try self.allocator.dupe(TypeId, store.span(tag.payloads));
        } else invariant("a derived map's selected tag is not in its input row");
        defer self.allocator.free(payload_types);
        if (plan.payload_index >= payload_types.len) invariant("a derived map's selected payload is past its tag's payloads");
        const map: lower.StructuralMap = .{ .name = name, .payload_index = plan.payload_index, .arity = @intCast(payload_types.len), .effectful = effectful };
        const descs = try self.allocator.alloc(Word, 3 + payload_types.len);
        defer self.allocator.free(descs);
        descs[0] = try self.descWord(args[0]);
        descs[1] = try self.descWord(function.ret);
        for (payload_types, descs[2..][0..payload_types.len]) |ty, *desc| desc.* = try self.descWord(ty);
        descs[2 + payload_types.len] = try self.descWord(mapped);
        return try self.composeDictionary(try self.program.structuralMapWorker(self.module, map), descs);
    }

    /// The hidden arguments of `callee`, the procedure a dispatch edge
    /// reached through the exact procedure alias the method is bound to
    /// (`EvidenceTargetInstantiation.alias`). The edge instantiated the
    /// alias's scope (`node.subst`) and resolved its requirements
    /// (`through_alias`); each alias's forwarded lookup recorded its own
    /// substitution and evidence over that scope, so lowering the lookup's
    /// in the scope so instantiated gives the next alias's, alias by alias,
    /// up to the procedure the chain reaches.
    pub fn aliasTargetHidden(self: *Builder, node: static_dispatch.EvidenceNode, callee: WorkerId, edge: checked.LiteralEdge) Allocator.Error![]Word {
        return (try self.aliasTargetInstance(node, callee, edge)).hidden;
    }

    /// `aliasTargetHidden` with the instantiation the chain's last lookup
    /// gives the target.
    pub fn aliasTargetInstance(self: *Builder, node: static_dispatch.EvidenceNode, callee: WorkerId, edge: checked.LiteralEdge) Allocator.Error!CallInstance {
        const table = self.view.view.static_dispatch_plans;
        const alias = node.target.alias orelse invariant("an alias-instantiated target has no alias binding");
        const alias_module = self.program.moduleByKey(alias.module).index;
        const chain_frame = self.chain_frame;
        self.chain_frame = false;
        const first: AliasStep = switch (alias.binding) {
            .top_level => |pattern| self.callableEvalAliasStep(alias_module, self.program.callable_eval_by_pattern.get(.{ .module = alias_module, .pattern = pattern }) orelse
                invariant("a method's alias binding has no callable-eval template")),
            .local => |local| self.localAliasStep(alias_module, local.lookup, local.dispatch_scope),
        };
        // The edge's instantiation of the first alias's scope, in this body.
        const node_subst = table.site_substitutions[node.subst.start..][0..node.subst.len];
        if (node_subst.len != first.vars.len) invariant("an alias edge's substitution differs from its alias's scope");
        const bound = try self.program.arena.allocator().alloc(TypeId, node_subst.len);
        for (node_subst, bound) |checked_ty, *ty| ty.* = try self.convert(checked_ty);
        const evidence = table.aliasScopeEvidence(node);
        // The module holding the alias chain records the target's
        // instantiation composed through it (design.md "Literal Arguments").
        // A dispatch through another module's alias derives the literal
        // arguments from that instantiation.
        const literal_source: AliasLiterals = if (alias_module == self.module) blk: {
            const literals = try self.program.arena.allocator().alloc(Word, self.program.worker(callee).literal_params);
            try self.fillLiteralArgs(null, edge, literals);
            break :blk .{ .edge = literals };
        } else .derived;
        return try self.aliasChainInstance(.{
            .callee = callee,
            .first = first,
            .bound = bound,
            .dicts = if (chain_frame) &.{} else try self.scopeDictionaries(first, bound, evidence),
            .literals = &.{},
            .evidence = evidence,
            .literal_source = literal_source,
            .chain_frame = chain_frame,
            .call_fn = null,
        });
    }

    /// The instantiation of the procedure a use reaches through exact
    /// procedure alias `alias`. The use instantiates the alias's scope, with
    /// `subst` over its variables (empty where checking recorded none, as
    /// `call_fn` then relates them) and `evidence` for its requirements, and
    /// `edge` supplies its literal arguments (`.literal` where checking
    /// recorded none).
    pub fn aliasUseInstance(self: *Builder, alias: AliasCallee, subst: []const checked.CheckedTypeId, evidence: []const static_dispatch.CheckedEvidence, call_fn: TypeId, edge: checked.LiteralEdge) Allocator.Error!CallInstance {
        const first = self.callableEvalAliasStep(alias.module, alias.template);
        const bound = try self.aliasScopeBound(first, subst, call_fn);
        return try self.aliasChainInstance(.{
            .callee = alias.target,
            .first = first,
            .bound = bound,
            .dicts = try self.scopeDictionaries(first, bound, evidence),
            .literals = try self.aliasLiteralArgs(first, edge),
            .evidence = evidence,
            .literal_source = if (edge == .literal) .derived else .scopes,
            .chain_frame = false,
            .call_fn = call_fn,
        });
    }

    /// `aliasUseInstance` for a platform requirement `required` the app binds
    /// to `alias`. Checking pairs the two after the platform's use, so the
    /// use records no instantiation of the alias's scope: `call_fn` relates
    /// its variables and the requirement carries the app's root evidence for
    /// its scheme.
    fn requiredAliasInstance(self: *Builder, alias: AliasCallee, required: checked.PlatformRequiredProcedureResolvedRef, call_fn: TypeId) Allocator.Error!CallInstance {
        const first = self.callableEvalAliasStep(alias.module, alias.template);
        const scope = try self.requiredAliasScope(first, required, call_fn);
        return try self.aliasChainInstance(.{
            .callee = alias.target,
            .first = first,
            .bound = scope.bound,
            .dicts = scope.dicts,
            .literals = &.{},
            .evidence = &.{},
            .literal_source = .derived,
            .chain_frame = false,
            .call_fn = call_fn,
        });
    }

    /// The instantiation and dictionaries of the scope of alias `alias_step`
    /// that platform requirement `required` enters. The evidence is the app's,
    /// resolved in its module at the scope's types.
    fn requiredAliasScope(self: *Builder, alias_step: AliasStep, required: checked.PlatformRequiredProcedureResolvedRef, call_fn: TypeId) Allocator.Error!struct { bound: []const TypeId, dicts: []const Word } {
        const bound = try self.aliasScopeBound(alias_step, &.{}, call_fn);
        const span = required.root_evidence orelse return .{ .bound = bound, .dicts = try self.scopeDictionaries(alias_step, bound, &.{}) };
        const owner = self.program.moduleByKey(span.checked_module);
        const saved_module = self.module;
        const saved_view = self.view;
        const saved_env = self.env;
        const saved_foreign = self.foreign_scopes;
        defer {
            self.module = saved_module;
            self.view = saved_view;
            self.env = saved_env;
            self.foreign_scopes = saved_foreign;
        }
        self.module = owner.index;
        self.view = owner;
        self.env = .{ .module = alias_step.module, .vars = alias_step.vars, .bound = bound };
        self.foreign_scopes = self.alias_scopes.items.len;
        return .{ .bound = bound, .dicts = try self.scopeDictionaries(alias_step, bound, owner.view.static_dispatch_plans.evidence_refs[span.span.start..][0..span.span.len]) };
    }

    /// The types the variables of alias `alias_step`'s scope take at a use: those
    /// of the use's substitution `subst`, where an entry it left erroneous or
    /// did not record takes what the use's function type relates it to, or
    /// seals.
    fn aliasScopeBound(self: *Builder, alias_step: AliasStep, subst: []const checked.CheckedTypeId, call_fn: TypeId) Allocator.Error![]const TypeId {
        if (subst.len != 0 and subst.len != alias_step.vars.len) invariant("a use's substitution differs from its alias's scope");
        const bound = try self.program.arena.allocator().alloc(TypeId, alias_step.vars.len);
        var unrecorded = subst.len != alias_step.vars.len;
        for (subst) |checked_ty| unrecorded = unrecorded or self.view.types().payload(checked_ty) == .err;
        if (!unrecorded) {
            for (subst, bound) |checked_ty, *ty| ty.* = try self.convert(checked_ty);
            return bound;
        }
        const generic = try self.allocator.alloc(TypeId, alias_step.vars.len);
        defer self.allocator.free(generic);
        for (generic, 0..) |*slot, index| slot.* = try self.program.store.variable(@intCast(index));
        const alias_fn = alias_step.fn_ty orelse invariant("an alias with an unrecorded use has no function type");
        const generic_fn = try self.program.converter().convert(alias_step.module, alias_fn, .{ .module = alias_step.module, .vars = alias_step.vars, .bound = generic });
        const related = try self.relateVariablesPartial(&.{.{ generic_fn, call_fn }}, alias_step.vars.len);
        defer self.allocator.free(related);
        for (bound, related, alias_step.vars, 0..) |*slot, ty, variable, index| {
            slot.* = if (index < subst.len and self.view.types().payload(subst[index]) != .err)
                try self.convert(subst[index])
            else
                ty orelse try self.program.converter().sealedScheme(alias_step.module, variable);
        }
        return bound;
    }

    /// The literal arguments the edge `edge` of this body's module gives the
    /// scope of alias `alias_step`.
    fn aliasLiteralArgs(self: *Builder, alias_step: AliasStep, edge: checked.LiteralEdge) Allocator.Error![]const Word {
        if (edge == .literal) return &.{};
        const literals = try self.program.arena.allocator().alloc(Word, alias_step.literal_requirements.len);
        try self.fillLiteralArgs(null, edge, literals);
        return literals;
    }

    /// Walk an alias chain from the scope `chain` enters up to the procedure
    /// it reaches: each alias's forwarded lookup, read in that alias's scope,
    /// gives the next alias's instantiation, and the last gives the target's.
    fn aliasChainInstance(self: *Builder, chain: AliasChain) Allocator.Error!CallInstance {
        const arena = self.program.arena.allocator();
        const callee = chain.callee;
        const chain_frame = chain.chain_frame;
        var alias_step = chain.first;
        var bound = chain.bound;
        var dicts = chain.dicts;
        var literals = chain.literals;
        var literal_source = chain.literal_source;
        var step_evidence = chain.evidence;

        const saved_module = self.module;
        const saved_view = self.view;
        const saved_env = self.env;
        const saved_foreign = self.foreign_scopes;
        const saved_scopes = self.alias_scopes.items.len;
        defer {
            self.module = saved_module;
            self.view = saved_view;
            self.env = saved_env;
            self.foreign_scopes = saved_foreign;
            if (!chain_frame) self.alias_scopes.shrinkRetainingCapacity(saved_scopes);
        }
        while (true) {
            // A chain's alias scopes stay entered under the frame of the
            // procedure they reach, each read in the scope before it.
            const deferred_dicts: ?DeferredDictionaries = if (chain_frame) blk: {
                if (step_evidence.len != alias_step.params.len) invariant("an alias scope's evidence differs from its requirements");
                break :blk try self.deferredDictionaries(step_evidence, null, alias_step.params.len);
            } else null;
            self.module = alias_step.module;
            self.view = self.program.module(alias_step.module);
            self.env = .{ .module = alias_step.module, .vars = alias_step.vars, .bound = bound };
            if (chain_frame) {
                try self.alias_scopes.append(self.allocator, .{ .params = try self.runtimeParams(alias_step.params), .dicts = &.{}, .literals = &.{}, .literal_requirements = &.{}, .deferred_dicts = deferred_dicts, .saved_env = self.env });
            } else {
                self.alias_scopes.shrinkRetainingCapacity(saved_scopes);
                try self.alias_scopes.append(self.allocator, .{ .params = self.runtimeParams(alias_step.params) catch return error.OutOfMemory, .dicts = dicts, .literals = literals, .literal_requirements = &.{}, .saved_env = self.env });
                self.foreign_scopes = saved_scopes;
            }
            const lookup_subst = self.siteSubstitution(alias_step.lookup);
            const evidence = self.view.view.static_dispatch_plans.siteEvidence(alias_step.lookup) orelse &.{};
            const ref_id = self.view.view.resolved_value_refs.lookupIdByExpr(alias_step.lookup) orelse
                invariant("a forwarded procedure alias lookup has no resolved reference");
            const lookup_ref = self.view.view.resolved_value_refs.records[@backingInt(ref_id)].ref;
            const next: AliasStep = switch (lookup_ref) {
                .local_proc => |local| if (local.is_alias)
                    self.localAliasStep(alias_step.module, local.expr, local.dispatch_scope)
                else
                    invariant("an alias chain reaches a local procedure, which a forwarding alias never does"),
                inline .top_level_proc, .imported_proc, .hosted_proc, .promoted_top_level_proc, .platform_required_proc => |ref, tag| blk: {
                    const use = if (tag == .platform_required_proc) ref.procedure else ref;
                    const resolved = self.program.procedureBinding(use.binding);
                    switch (resolved.body) {
                        .callable_eval_template => |template_id| {
                            const template = self.program.module(resolved.module).view.callable_eval_templates.templates[@backingInt(template_id)];
                            if (template.forwarded_lookup != null) break :blk self.callableEvalAliasStep(resolved.module, template_id);
                            invariant("an alias chain reaches a computed callable, which a forwarding alias never does");
                        },
                        .direct_template => {},
                        .checked_error => invariant("an alias chain reaches a rejected binding"),
                    }
                    // A platform requirement's lookup records no
                    // instantiation of the app's procedure.
                    if (tag == .platform_required_proc) {
                        if (chain.call_fn) |call_fn| return try self.requiredInstanceAt(callee, ref, lookup_subst, call_fn, .literal);
                    }
                    // The procedure the chain reaches: its substitution and
                    // evidence are the lookup's.
                    const target = self.program.worker(callee).*;
                    if (chain.call_fn == null and lookup_subst.len == 0 and target.scheme_vars.len != 0) invariant("an alias's forwarded lookup's substitution differs from its target's scheme");
                    const owned_bound = try self.boundTypesAt(callee, lookup_subst, chain.call_fn);
                    defer self.allocator.free(owned_bound);
                    const target_bound = try arena.dupe(TypeId, owned_bound);
                    const hidden = try self.allocator.alloc(Word, target_bound.len + target.dictionary_params.len + target.literal_params);
                    errdefer self.allocator.free(hidden);
                    if (!chain_frame) {
                        for (target_bound, hidden[0..target_bound.len]) |ty, *desc| desc.* = try self.descWord(ty);
                    }
                    const target_literals = hidden[target_bound.len + target.dictionary_params.len ..];
                    switch (literal_source) {
                        .edge => |literal_args| @memcpy(target_literals, literal_args),
                        .scopes => try self.fillLiteralArgs(null, .{ .use = alias_step.lookup }, target_literals),
                        .derived => try self.derivedLiteralArgs(callee, target_bound, target_literals),
                    }
                    const ret = try self.program.substitute(self.allocator, target.ret, target_bound);
                    if (chain_frame) {
                        if (target.dictionary_params.len == 0) return .{ .hidden = hidden, .bound = target_bound, .ret = ret };
                        return .{ .hidden = hidden, .bound = target_bound, .ret = ret, .deferred_dicts = try self.deferredDictionaries(evidence, null, target.dictionary_params.len) };
                    }
                    const context = self.templateParamContext(callee, target_bound);
                    for (target.dictionary_params, hidden[target_bound.len..][0..target.dictionary_params.len]) |param_index, *dict| {
                        if (param_index >= evidence.len) invariant("an alias's forwarded lookup's evidence does not cover its target's dictionaries");
                        dict.* = try self.paramDictionary(evidence, param_index, context);
                    }
                    return .{ .hidden = hidden, .bound = target_bound, .ret = ret };
                },
                .local_param, .local_value, .local_mutable_version, .pattern_binder, .selected_hoisted_const, .top_level_const, .imported_const, .platform_required_declaration, .platform_required_checked_error, .platform_required_const => invariant("a forwarded procedure alias lookup names no procedure"),
            };
            // A platform requirement bound to the app's alias enters that
            // alias's scope as a use with no recorded instantiation.
            if (lookup_ref == .platform_required_proc) if (chain.call_fn) |call_fn| {
                const scope = try self.requiredAliasScope(next, lookup_ref.platform_required_proc, call_fn);
                bound = scope.bound;
                dicts = scope.dicts;
                literals = &.{};
                literal_source = .derived;
                alias_step = next;
                continue;
            };
            if (lookup_subst.len != next.vars.len) invariant("an alias's forwarded lookup's substitution differs from the next alias's scope");
            const next_bound = try arena.alloc(TypeId, lookup_subst.len);
            for (lookup_subst, next_bound) |checked_ty, *ty| ty.* = try self.convert(checked_ty);
            if (chain_frame) {
                step_evidence = evidence;
            } else {
                dicts = try self.scopeDictionaries(next, next_bound, evidence);
            }
            literals = switch (literal_source) {
                .scopes => try self.aliasLiteralArgs(next, .{ .use = alias_step.lookup }),
                .edge, .derived => &.{},
            };
            bound = next_bound;
            alias_step = next;
        }
    }

    /// The step of an alias chain through a local exact procedure alias of
    /// `module`, which owns generalized scope `dispatch_scope` when its
    /// scheme has one.
    fn localAliasStep(self: *Builder, module: ModuleIndex, lookup: ExprId, dispatch_scope: ?checked.DispatchScopeId) AliasStep {
        const scope_id = dispatch_scope orelse return .{ .module = module, .lookup = lookup, .vars = &.{}, .params = &.{} };
        const templates = self.program.module(module).view.checked_procedure_templates;
        const scope = templates.dispatch_scopes[@backingInt(scope_id)];
        return .{
            .module = module,
            .lookup = lookup,
            .vars = lower.poolSlice(checked.CheckedTypeId, templates.scheme_vars_pool, scope.scheme_vars),
            .params = lower.poolSlice(static_dispatch.EvidenceParamRecord, templates.evidence_params_pool, scope.evidence_params),
            .literal_requirements = templates.scopeLiteralRequirements(&scope),
        };
    }

    /// The step of an alias chain through the exact procedure alias
    /// `template_id` of `module_index`: its forwarded lookup in its entry
    /// wrapper's scope.
    fn callableEvalAliasStep(self: *Builder, module_index: ModuleIndex, template_id: checked.CallableEvalTemplateId) AliasStep {
        const view = self.program.module(module_index);
        const template = view.view.callable_eval_templates.templates[@backingInt(template_id)];
        const wrapper = self.program.wrapper_by_root.get(.{ .module = module_index, .root = template.root }) orelse
            invariant("a callable-eval template's root has no entry wrapper");
        const owner = self.program.moduleByKey(.{ .bytes = wrapper.artifact.bytes });
        const templates = owner.view.checked_procedure_templates;
        const wrapper_template = templates.templates.items[@backingInt(wrapper.template)];
        return .{
            .module = module_index,
            .lookup = template.forwarded_lookup orelse invariant("a method's alias binding is not an exact procedure alias"),
            .vars = lower.poolSlice(checked.CheckedTypeId, templates.scheme_vars_pool, wrapper_template.scheme_vars),
            .params = lower.poolSlice(static_dispatch.EvidenceParamRecord, templates.evidence_params_pool, wrapper_template.evidence_params),
            .literal_requirements = templates.templateLiteralRequirements(&wrapper_template),
            .fn_ty = template.checked_fn_root,
        };
    }

    /// The dictionaries of the scope of alias `alias_step` whose variables
    /// take `bound`, from `evidence` (one entry per parameter).
    fn scopeDictionaries(self: *Builder, alias_step: AliasStep, bound: []const TypeId, evidence: []const static_dispatch.CheckedEvidence) Allocator.Error![]const Word {
        if (evidence.len != alias_step.params.len) invariant("an alias scope's evidence differs from its requirements");
        const dicts = try self.program.arena.allocator().alloc(Word, alias_step.params.len);
        const context: ParamContext = .{ .module = alias_step.module, .params = alias_step.params, .env = .{ .module = alias_step.module, .vars = alias_step.vars, .bound = bound } };
        for (dicts, 0..) |*dict, index| dict.* = try self.paramDictionary(evidence, @intCast(index), context);
        return dicts;
    }

    fn runtimeParams(self: *const Builder, params: []const static_dispatch.EvidenceParamRecord) Allocator.Error![]const u32 {
        var indices: std.ArrayList(u32) = .empty;
        for (params, 0..) |_, index| {
            try indices.append(self.program.arena.allocator(), @intCast(index));
        }
        return indices.items;
    }

    pub fn relatedHiddenArgs(self: *Builder, callee: WorkerId, callable: checked.CheckedTypeId) Allocator.Error![]Word {
        return try self.callableDerivedHidden(callee, &.{}, try self.convert(callable));
    }

    /// The hidden arguments of `callee` instantiated at `subst` (or, with
    /// none recorded, at what `call_fn` relates its variables to), its
    /// dictionaries derived from those types: each requirement dispatches on
    /// its dispatcher's type at the instantiation, as checking selects
    /// targets.
    pub fn callableDerivedHidden(self: *Builder, callee: WorkerId, subst: []const checked.CheckedTypeId, call_fn: ?TypeId) Allocator.Error![]Word {
        return (try self.callableDerivedInstance(callee, subst, call_fn)).hidden;
    }

    /// `callableDerivedHidden` with the instantiation it computed.
    pub fn callableDerivedInstance(self: *Builder, callee: WorkerId, subst: []const checked.CheckedTypeId, call_fn: ?TypeId) Allocator.Error!CallInstance {
        const target = self.program.worker(callee).*;
        const owned = try self.boundTypesAt(callee, subst, call_fn);
        defer self.allocator.free(owned);
        const bound = try self.program.arena.allocator().dupe(TypeId, owned);
        const hidden = try self.allocator.alloc(Word, bound.len + target.dictionary_params.len + target.literal_params);
        errdefer self.allocator.free(hidden);
        for (bound, hidden[0..bound.len]) |ty, *desc| desc.* = try self.descWord(ty);
        const ret = try self.program.substitute(self.allocator, target.ret, bound);
        try self.derivedLiteralArgs(callee, bound, hidden[bound.len + target.dictionary_params.len ..]);
        if (target.dictionary_params.len == 0) return .{ .hidden = hidden, .bound = bound, .ret = ret };
        const instance = try self.program.targetRequirements(callee, bound);
        for (instance.requirements, hidden[bound.len..][0..target.dictionary_params.len]) |requirement, *dict| dict.* = try self.dictionaryFromType(requirement);
        return .{ .hidden = hidden, .bound = bound, .ret = ret };
    }

    /// Whether stored type evidence `root` of `source` holds an erased
    /// function type, which names no signature.
    fn constTypeHasErased(self: *Builder, source: lower.ConstSource, root: check.ConstStore.ConstTypeId) Allocator.Error!bool {
        const const_types = &self.program.constStore(source).type_store;
        var stack: std.ArrayList(check.ConstStore.ConstTypeId) = .empty;
        defer stack.deinit(self.allocator);
        var seen: collections.DenseMapUnmanaged(check.ConstStore.ConstTypeId, void) = .empty;
        defer seen.deinit(self.allocator);
        try stack.append(self.allocator, root);
        while (stack.pop()) |id| {
            if ((try seen.getOrPut(self.allocator, id)).found_existing) continue;
            switch (const_types.get(id)) {
                .erased => return true,
                .primitive, .zst => {},
                .list, .box => |child| try stack.append(self.allocator, child),
                .tuple => |range| try stack.appendSlice(self.allocator, const_types.typeSpan(range)),
                .record => |range| for (const_types.fieldSpan(range)) |field| {
                    try stack.append(self.allocator, field.ty);
                    if (field.value_ty) |value_ty| try stack.append(self.allocator, value_ty);
                },
                .tag_union => |range| for (const_types.tagSpan(range)) |tag| try stack.appendSlice(self.allocator, const_types.typeSpan(tag.payloads)),
                .func => |func| {
                    try stack.appendSlice(self.allocator, const_types.typeSpan(func.args));
                    try stack.append(self.allocator, func.ret);
                },
                .named => |named| try stack.appendSlice(self.allocator, const_types.typeSpan(named.args)),
            }
        }
        return false;
    }

    /// The Boxy type of a stored constant's type evidence `root` in the
    /// constant store of `source`.
    pub fn constType(self: *Builder, source: lower.ConstSource, root: check.ConstStore.ConstTypeId) Allocator.Error!TypeId {
        const const_types = &self.program.constStore(source).type_store;
        const names = self.program.constNames(source);
        const store = &self.program.store;
        var done: collections.DenseMapUnmanaged(check.ConstStore.ConstTypeId, TypeId) = .empty;
        defer done.deinit(self.allocator);
        const Frame = struct { id: check.ConstStore.ConstTypeId, expanded: bool };
        var stack: std.ArrayList(Frame) = .empty;
        defer stack.deinit(self.allocator);
        try stack.append(self.allocator, .{ .id = root, .expanded = false });
        while (stack.pop()) |frame| {
            if (done.contains(frame.id)) continue;
            const ty = const_types.get(frame.id);
            if (!frame.expanded) {
                try stack.append(self.allocator, .{ .id = frame.id, .expanded = true });
                switch (ty) {
                    .list, .box => |child| try stack.append(self.allocator, .{ .id = child, .expanded = false }),
                    .tuple => |range| for (const_types.typeSpan(range)) |child| try stack.append(self.allocator, .{ .id = child, .expanded = false }),
                    .record => |range| for (const_types.fieldSpan(range)) |field| {
                        try stack.append(self.allocator, .{ .id = field.ty, .expanded = false });
                        if (field.value_ty) |value_ty| try stack.append(self.allocator, .{ .id = value_ty, .expanded = false });
                    },
                    .tag_union => |range| for (const_types.tagSpan(range)) |tag| {
                        for (const_types.typeSpan(tag.payloads)) |child| try stack.append(self.allocator, .{ .id = child, .expanded = false });
                    },
                    .func => |func| {
                        for (const_types.typeSpan(func.args)) |child| try stack.append(self.allocator, .{ .id = child, .expanded = false });
                        try stack.append(self.allocator, .{ .id = func.ret, .expanded = false });
                    },
                    .named => |named| {
                        for (const_types.typeSpan(named.args)) |child| try stack.append(self.allocator, .{ .id = child, .expanded = false });
                        // A stored alias is its backing.
                        if (named.kind == .alias) {
                            const backing = named.backing orelse invariant("a stored alias has no backing type");
                            try stack.append(self.allocator, .{ .id = backing.ty, .expanded = false });
                        }
                    },
                    .primitive, .erased, .zst => {},
                }
                continue;
            }
            const converted: TypeId = switch (ty) {
                .primitive => |primitive| try store.primitive(primitive),
                .zst => try store.emptyRecord(),
                .list => |child| try store.intern(.{ .list = done.get(child).? }),
                .box => |child| try store.intern(.{ .box = done.get(child).? }),
                .tuple => |range| blk: {
                    const items = try self.allocator.alloc(TypeId, range.len);
                    defer self.allocator.free(items);
                    for (const_types.typeSpan(range), items) |child, *item| item.* = done.get(child).?;
                    break :blk try store.intern(.{ .tuple = items });
                },
                .record => |range| blk: {
                    const fields = try self.allocator.alloc(types.Field, range.len);
                    defer self.allocator.free(fields);
                    for (const_types.fieldSpan(range), fields) |field, *out| {
                        // An optional field is stored as its presence slot
                        // over the value type; a defaulted field is stored
                        // inline like a required one.
                        out.* = if (field.value_ty) |value_ty|
                            .{ .name = try store.internName(names.recordFieldLabelText(field.name)), .ty = done.get(value_ty).?, .kind = .presence_slot }
                        else
                            .{ .name = try store.internName(names.recordFieldLabelText(field.name)), .ty = done.get(field.ty).?, .kind = .inline_slot };
                    }
                    std.mem.sort(types.Field, fields, store, struct {
                        fn lessThan(s: *types.Store, a: types.Field, b: types.Field) bool {
                            return s.nameLessThan(a.name, b.name);
                        }
                    }.lessThan);
                    break :blk if (fields.len == 0) try store.emptyRecord() else try store.intern(.{ .record = .{ .fields = fields, .ext = null } });
                },
                .tag_union => |range| blk: {
                    const tags = const_types.tagSpan(range);
                    const inputs = try self.allocator.alloc(types.TagInput, tags.len);
                    defer self.allocator.free(inputs);
                    var payload_store: std.ArrayList(TypeId) = .empty;
                    defer payload_store.deinit(self.allocator);
                    for (tags) |tag| {
                        for (const_types.typeSpan(tag.payloads)) |child| try payload_store.append(self.allocator, done.get(child).?);
                    }
                    var at: usize = 0;
                    for (tags, inputs) |tag, *input| {
                        input.* = .{ .name = try store.internName(names.tagLabelText(tag.checked_name)), .payloads = payload_store.items[at..][0..tag.payloads.len] };
                        at += tag.payloads.len;
                    }
                    std.mem.sort(types.TagInput, inputs, store, struct {
                        fn lessThan(s: *types.Store, a: types.TagInput, b: types.TagInput) bool {
                            return s.nameLessThan(a.name, b.name);
                        }
                    }.lessThan);
                    break :blk try store.intern(.{ .tag_union = .{ .tags = inputs, .ext = null } });
                },
                .func => |func| blk: {
                    const args = try self.allocator.alloc(TypeId, func.args.len);
                    defer self.allocator.free(args);
                    for (const_types.typeSpan(func.args), args) |child, *arg| arg.* = done.get(child).?;
                    break :blk try store.intern(.{ .function = .{ .args = args, .ret = done.get(func.ret).?, .effectful = false } });
                },
                // The stored definition names the nominal's declaration by
                // its declaring module's content identity and its source
                // statement, as Monotype reads it; the stored arguments are
                // what it is applied to. A stored type never names a type
                // store of another module: the program a value was evaluated
                // in need not be the one restoring it.
                .named => |named| blk: {
                    if (named.kind == .alias) break :blk done.get(named.backing.?.ty).?;
                    if (named.def.generated != null) invariant("a stored type names a generated nominal, which only specialization mints");
                    const identity = names.moduleIdentityBytes(named.def.module).*;
                    const declaring = self.program.modules.by_identity.get(identity) orelse
                        invariant("a stored type's declaring module is not in this lowering");
                    const source_decl = named.def.source_decl orelse invariant("a stored nominal type names no declaring statement");
                    const view = self.program.module(declaring);
                    const declaration_id = try self.program.store.declarationBySourceDecl(view, source_decl) orelse
                        invariant("a stored type names a declaration its module does not make");
                    const declaration = view.types().nominalDeclarationById(declaration_id);
                    // A declaration in a function body takes its context
                    // arguments, which a stored type leaves out, from the
                    // restoring scope.
                    const context = declaration.contextFormalArgs(view.types());
                    const args = try self.allocator.alloc(TypeId, named.args.len + context.len);
                    defer self.allocator.free(args);
                    for (const_types.typeSpan(named.args), args[0..named.args.len]) |child, *arg| arg.* = done.get(child).?;
                    for (context, args[named.args.len..]) |formal, *arg| arg.* = try self.program.converter().convert(declaring, formal, self.env);
                    break :blk try self.program.converter().nominalAt(declaring, declaration.declaration_root, args);
                },
                // Only a closure's capture holds an erased function type, and
                // its restore relates no such capture (`constTypeHasErased`).
                .erased => invariant("a stored type converted for Boxy holds an erased function type, which only captures, which relate none, hold"),
            };
            try done.put(self.allocator, frame.id, converted);
        }
        return done.get(root).?;
    }

    /// The types that variables `0..count` of `formal` take in `actual`: each
    /// variable takes the component of `actual` at the position the variable
    /// has in `formal`. An open row's remainder variable takes the actual
    /// row's entries the formal row does not name.
    pub fn relateVariables(self: *Builder, formal_root: TypeId, actual_root: TypeId, count: usize) Allocator.Error![]TypeId {
        return try self.relateVariablesAll(&.{.{ formal_root, actual_root }}, count);
    }

    /// `relateVariables` over several formal and actual type pairs at once.
    pub fn relateVariablesAll(self: *Builder, roots: []const [2]TypeId, count: usize) Allocator.Error![]TypeId {
        const bound = try self.relateVariablesPartial(roots, count);
        defer self.allocator.free(bound);
        const result = try self.allocator.alloc(TypeId, count);
        for (bound, result, 0..) |ty, *slot, index| slot.* = ty orelse {
            var formal_text: std.Io.Writer.Allocating = .init(self.allocator);
            defer formal_text.deinit();
            var actual_text: std.Io.Writer.Allocating = .init(self.allocator);
            defer actual_text.deinit();
            for (roots) |pair| {
                self.debugTypeText(pair[0], &formal_text.writer, 0) catch {};
                formal_text.writer.writeAll("; ") catch {};
                self.debugTypeText(pair[1], &actual_text.writer, 0) catch {};
                actual_text.writer.writeAll("; ") catch {};
            }
            invariantFmt("a related type leaves variable {d} of {d} unrelated: formal {s} actual {s}", .{ index, count, formal_text.written(), actual_text.written() });
        };
        return result;
    }

    /// The relation of `relateVariablesAll`, leaving variables the types do
    /// not relate null.
    pub fn relateVariablesPartial(self: *Builder, roots: []const [2]TypeId, count: usize) Allocator.Error![]?TypeId {
        return try self.program.relateVariablesPartial(self.allocator, roots, count);
    }

    /// Reserve the constructor before following its local-method capture edges.
    /// Re-entry names that same node only in the same declaration instance.
    pub fn beginCodecComposition(self: *Builder, family: u32, bound: []const TypeId, actual: TypeId) Allocator.Error!union(enum) { composing: u32, back_edge: Word } {
        for (self.codec_compositions.items) |composition| {
            if (composition.family != family or composition.declaration != self.declaration or !std.mem.eql(TypeId, composition.bound, bound)) continue;
            const formal = try self.program.substitute(self.allocator, self.program.codec.families.items[family].constructor_fn, bound);
            return .{ .back_edge = try self.adaptDictionary(.{ .recipe = composition.recipe }, formal, actual) };
        }
        const index: u32 = @intCast(self.codec_compositions.items.len);
        const recipe = try self.recipes.reserve();
        try self.codec_compositions.append(self.allocator, .{ .family = family, .bound = bound, .recipe = recipe, .declaration = self.declaration });
        return .{ .composing = index };
    }

    pub fn finishCodecComposition(self: *Builder, index: u32, composed: Word) void {
        if (self.codec_compositions.items.len != index + 1) invariant("generated codec constructor compositions did not nest");
        const composition = self.codec_compositions.pop().?;
        switch (composed) {
            .recipe => |recipe| self.recipes.setSame(composition.recipe, recipe),
            .static => invariant("a generated codec constructor has no callable recipe"),
        }
    }

    /// The callable type the requirement of evidence parameter `index` of
    /// this worker's scopes names, at this body's types: what its
    /// dictionary's method is called at. Null for an alias scope's
    /// parameter.
    pub fn scopeInterface(self: *Builder, index: static_dispatch.EvidenceChainIndex) Allocator.Error!?TypeId {
        if (self.foreign_scopes != null) return null;
        const aliases = self.alias_scopes.items.len;
        if (index.depth < aliases) return null;
        const depth = index.depth - aliases;
        const worker = self.currentWorker().*;
        if (depth >= worker.dict_scope_params.len) invariant("evidence named a generalized scope outside its worker's scopes");
        const params = worker.dict_scope_params[depth];
        if (index.index >= params.len) invariant("evidence named a parameter its scope does not have");
        return try self.convert(params[index.index].callable_ty);
    }

    /// The dictionary in scope for checked evidence parameter `index`, or
    /// null when the parameter is no dictionary of the worker's scopes: a
    /// requirement on a sealed scheme variable.
    pub fn scopeDictionaryIfAny(self: *Builder, index: static_dispatch.EvidenceChainIndex) Allocator.Error!?Word {
        if (self.foreign_scopes != null) return try self.scopeDictionary(index);
        const aliases = self.alias_scopes.items;
        if (index.depth < aliases.len) return try self.scopeDictionary(index);
        const depth = index.depth - aliases.len;
        const scopes = self.currentWorker().dict_scopes;
        if (depth >= scopes.len) invariant("evidence named a generalized scope outside its worker's scopes");
        var offset: usize = 0;
        for (scopes[0..depth]) |scope| offset += scope.len;
        for (scopes[depth], 0..) |param_index, position| {
            if (param_index == index.index) return try self.dictInputWord(offset + position);
        }
        return null;
    }

    /// The dictionary of the requirement that evidence parameter `index`
    /// of the worker's scopes names, resolved from the sealed type of the
    /// variable it dispatches on.
    fn sealedScopeDictionary(self: *Builder, index: static_dispatch.EvidenceChainIndex) Allocator.Error!Word {
        const aliases = self.alias_scopes.items.len;
        if (index.depth < aliases) invariant("a scheme alias scope's requirement has no dictionary");
        const depth = index.depth - aliases;
        const worker = self.currentWorker().*;
        if (depth >= worker.dict_scope_params.len) invariant("evidence named a generalized scope outside its worker's scopes");
        const params = worker.dict_scope_params[depth];
        if (index.index >= params.len) invariant("evidence named a parameter its scope does not have");
        const param = params[index.index];
        const converter = self.program.converter();
        return try self.dictionaryFromType(.{
            .module = self.module,
            .method = param.method,
            .structural = param.structural,
            .dispatcher = try converter.convert(self.module, param.dispatcher_ty, self.env),
            .callable = try converter.convert(self.module, param.callable_ty, self.env),
        });
    }

    /// Dictionary parameter `param` of the frame at `position` of
    /// `alias_scopes`. A literal chain frame's is composed at its first read,
    /// in the scope that holds the frame's edge.
    fn frameDictionary(self: *Builder, position: usize, param: u32) Allocator.Error!Word {
        const scope = self.alias_scopes.items[position];
        const slot = std.mem.findScalar(u32, scope.params, param) orelse invariant("evidence named a dictionary parameter its alias scope does not have");
        if (scope.declaration) |declaration| if (declaration.dicts[slot] == null) {
            // Only a requirement naming a type the callee's body declares
            // (a captured codec requirement whose format is a local type)
            // composes a dictionary that reaches the callee's local
            // procedures, so only such a slot is ever being built while one
            // is. A local procedure's own scheme names a declaring slot only
            // through a requirement on one of the callee's quantified
            // variables, which the callee's annotation states outside its
            // body and so cannot name a type its body declares; a captured
            // requirement on a structural receiver resolves structurally.
            invariant("a local procedure's own scheme names a declaration context's dictionary while it is built");
        };
        const deferred = scope.deferred_dicts orelse return scope.dicts[slot];
        if (deferred.composed[slot]) |dictionary| return dictionary;
        if (deferred.declaring) |declaring| if (self.frameDeclaration(deferred.composed) == null) {
            return try self.frameDeclarationDictionary(position, declaring, deferred, slot);
        };
        const saved_module = self.module;
        const saved_view = self.view;
        const saved_env = self.env;
        const saved_foreign = self.foreign_scopes;
        // Reading can enter alias scopes over the frames below the edge.
        const saved_frames = try self.program.arena.allocator().dupe(AliasScope, self.alias_scopes.items[deferred.frames..]);
        defer {
            self.module = saved_module;
            self.view = saved_view;
            self.env = saved_env;
            self.foreign_scopes = saved_foreign;
            self.alias_scopes.shrinkRetainingCapacity(deferred.frames);
            self.alias_scopes.appendSliceAssumeCapacity(saved_frames);
        }
        self.module = deferred.module;
        self.view = deferred.view;
        self.env = deferred.env;
        self.foreign_scopes = deferred.foreign_scopes;
        self.alias_scopes.shrinkRetainingCapacity(deferred.frames);
        const dictionary = if (deferred.context) |context|
            (if (param < deferred.evidence.len) try self.paramDictionary(deferred.evidence, param, context) else try self.derivedParamDictionary(param, context))
        else if (param < deferred.evidence.len)
            try self.dictionaryOf(deferred.evidence[param])
        else
            invariant("an alias's evidence does not cover the parameter a chain read");
        deferred.composed[slot] = dictionary;
        return dictionary;
    }

    /// Dictionary `slot` of the literal chain frame at `position`, a call
    /// instance of template worker `declaring`, built in the frame's
    /// declaration context.
    fn frameDeclarationDictionary(self: *Builder, position: usize, declaring: WorkerId, deferred: DeferredDictionaries, slot: usize) Allocator.Error!Word {
        const context = deferred.context orelse invariant("a template frame's dictionaries have no parameter context");
        const bound = context.env.bound;
        const dicts = try self.allocator.dupe(?Word, deferred.composed);
        defer self.allocator.free(dicts);
        const building = try self.allocator.alloc(bool, dicts.len);
        defer self.allocator.free(building);
        @memset(building, false);
        var declaration: Declaration = .{
            .worker = declaring,
            .bound = bound,
            .words = null,
            .dicts = dicts,
            .building = building,
            .source = .{ .frame = .{ .position = position, .composed = deferred.composed.ptr, .scope = self.alias_scopes.items[position] } },
            .outer = self.declaration,
        };
        return try self.declarationDictionary(&declaration, slot);
    }

    /// The dictionary in scope for checked evidence parameter `index`.
    /// Depth 0 is the innermost scope: an entered scheme alias's, then the
    /// worker's own.
    pub fn scopeDictionary(self: *Builder, index: static_dispatch.EvidenceChainIndex) Allocator.Error!Word {
        if (self.foreign_scopes) |first| {
            const scopes = self.alias_scopes.items[first..];
            if (index.depth >= scopes.len) invariant("an exact procedure alias's lookup names evidence outside the alias's scope");
            return try self.frameDictionary(first + scopes.len - 1 - index.depth, index.index);
        }
        const aliases = self.alias_scopes.items;
        if (index.depth < aliases.len) return try self.frameDictionary(aliases.len - 1 - index.depth, index.index);
        const depth = index.depth - aliases.len;
        const scopes = self.currentWorker().dict_scopes;
        if (depth >= scopes.len) invariant("evidence named a generalized scope outside its worker's scopes");
        var offset: usize = 0;
        for (scopes[0..depth]) |scope| offset += scope.len;
        for (scopes[depth], 0..) |param_index, position| {
            if (param_index == index.index) return try self.dictInputWord(offset + position);
        }
        const worker = self.currentWorker().*;
        const named = self.workerName(self.worker_id);
        const evidence_params = if (worker.key == .template) self.templateEvidenceParams(self.worker_id) else &.{};
        var listing: [2048]u8 = undefined;
        var written: usize = 0;
        for (evidence_params, 0..) |param, position| {
            const text = std.fmt.bufPrint(listing[written..], " [{d}] {s}", .{ position, self.view.nameStore().methodNameText(param.method) }) catch break;
            written += text.len;
        }
        invariantFmt("evidence named dictionary parameter {d} at depth {d}, which worker {s} (scope depth {d} of {d}, {d} dictionaries, {d} sealed; evidence params:{s}) does not have", .{
            index.index,
            index.depth,
            named,
            depth,
            scopes.len,
            scopes[depth].len,
            worker.checked_scheme_count - worker.checked_slots.len,
            listing[0..written],
        });
    }

    pub fn siteSubstitution(self: *Builder, site: ExprId) []const checked.CheckedTypeId {
        return self.view.view.static_dispatch_plans.siteSubstitution(site) orelse &.{};
    }

    /// Redirect an immediately returned call or constructor from its fresh
    /// dynamic frame local into the caller's result storage. Nothing has
    /// read the fresh local yet. Boxy calls supply separate result storage;
    /// forwarding that destination cannot overlap a constructor's operands.
    /// ARC sees the rewritten producer and assigns ownership normally.
    pub fn retargetLastProducer(self: *Builder, value: LocalId, result: LocalId) Allocator.Error!bool {
        const last = self.last_stmt orelse return false;
        const dyn = self.store.localDyn(value) orelse return false;
        if (dyn.storage != .frame) return false;
        var stmt = self.store.getCFStmt(last);
        switch (stmt) {
            .assign_call => |*call| {
                if (call.target != value or call.next != self.hole.?) return false;
                call.target = result;
            },
            .dyn => |*op| {
                if (op.target != value or op.next != self.hole.?) return false;
                switch (self.store.stmtRow(op.data)) {
                    .call_erased => {},
                    .record, .tag => {
                        // Adaptation to a different result representation
                        // must remain an explicit operation.
                        if (dyn.desc != self.store.localDyn(result).?.desc) return false;
                    },
                    .frame_alloc, .table_entry, .copy, .field_addr, .list_item_addr, .box_payload_addr, .tag_discriminant, .tag_payload_addr, .record_update, .list, .box, .slot_alloc, .slot_store, .eq, .hash, .inspect, .closure, .view, .adapt => return false,
                }
                op.target = result;
            },
            .init_uninitialized, .assign_ref, .assign_literal, .assign_call_erased, .assign_packed_erased_fn, .assign_low_level, .assign_list, .assign_struct, .assign_tag, .store_struct, .store_tag, .set_local, .debug, .expect, .expect_err, .runtime_error, .comptime_exhaustiveness_failed, .incref, .decref, .decref_if_initialized, .free, .switch_stmt, .switch_initialized_payload, .str_match, .str_match_set, .join, .jump, .ret, .crash => return false,
        }
        try self.store.replaceCFStmt(last, stmt, self.store.stmtOrigin(last));
        const index = std.mem.findScalarLast(LocalId, self.frame_locals.items, value) orelse unreachable; // addLocal records every local a statement targets
        _ = self.frame_locals.orderedRemove(index);
        return true;
    }

    /// A direct call of `callee` with the hidden arguments `hidden`, whose
    /// result the callee writes at its own result type at this
    /// instantiation, `ret`; the call's own type `result_ty` may widen a
    /// sealed row of it, which the result is converted to (design.md
    /// "Sealed Result Rows").
    pub fn finishCall(self: *Builder, callee: WorkerId, hidden_words: []const Word, bound: []const TypeId, arg_types: []const TypeId, ret: TypeId, result_ty: TypeId) Allocator.Error!void {
        const explicit = try self.popValues(@intCast(arg_types.len));
        defer self.allocator.free(explicit);
        // Checking identifies compiler-provided operations explicitly. Emit
        // that operation at its checked instantiation, using the same path
        // as the wrapper's own body, rather than calling the wrapper.
        const worker = self.program.worker(callee).*;
        if (worker.key == .template) {
            const key = worker.key.template;
            const owner = self.program.module(key.module);
            const template = owner.view.checked_procedure_templates.templates.items[@backingInt(key.template)];
            if (template.provided_low_level) |op| {
                const adapted_types = try self.allocator.dupe(TypeId, arg_types);
                defer self.allocator.free(adapted_types);
                try self.argsAtParams(explicit, arg_types, callee, bound, adapted_types);
                try self.emitLowLevelCall(op, explicit, ret, adapted_types);
                const value = self.popValue();
                return try self.pushValue(try self.adaptValue(value, ret, result_ty));
            }
        }
        // A `List` template called at a fixed-size integer item type calls
        // its integer list worker for that type, whose inputs are the
        // generic worker's.
        const direct = try self.program.directCallTarget(callee, bound);
        try self.argsAtParams(explicit, arg_types, direct, bound, null);
        // The callee's one hidden argument: its type table at the hidden
        // words.
        const args = try self.callArgs(explicit, direct, hidden_words);
        defer self.allocator.free(args);
        const target = try self.localOf(ret, .frame);
        const proc = self.program.worker(direct).proc;
        try self.emitLinear(.{ .assign_call = .{ .target = target, .proc = proc, .args = try self.store.addLocalSpan(args), .next = undefined } });
        try self.pushValue(try self.adaptValue(target, ret, result_ty));
    }

    /// `args` of types `arg_types`, where one parameter of `callee` under
    /// `bound` stores a record in another order: a padded nominal passed to
    /// a parameter of its record row (design.md "Nominal Record Field
    /// Order"), which the callee reads by the structural rule.
    fn argsAtParams(self: *Builder, args: []LocalId, arg_types: []const TypeId, callee: WorkerId, bound: []const TypeId, adapted_types: ?[]TypeId) Allocator.Error!void {
        const store = &self.program.store;
        for (args, arg_types, self.program.worker(callee).params[0..args.len], 0..) |*arg, arg_ty, param, index| {
            // A variable has the one representation of whatever it binds.
            if (store.get(param) == .variable) continue;
            const param_ty = if (store.isClosed(param)) param else try self.program.substitute(self.allocator, param, bound);
            if (try self.recordOrderDiffers(arg_ty, param_ty)) {
                arg.* = try self.adaptValue(arg.*, arg_ty, param_ty);
                // A direct primitive consumes these adapted bytes, so its
                // descriptor operands must describe the adapted type too.
                if (adapted_types) |types_after_adaptation| types_after_adaptation[index] = param_ty;
            }
        }
    }

    /// Whether two types of one value store a record in different orders,
    /// outside the function values each holds.
    fn recordOrderDiffers(self: *Builder, from_root: TypeId, to_root: TypeId) Allocator.Error!bool {
        const store = &self.program.store;
        var pending: std.ArrayList([2]TypeId) = .empty;
        defer pending.deinit(self.allocator);
        try pending.append(self.allocator, .{ from_root, to_root });
        while (pending.pop()) |pair| {
            if (pair[0] == pair[1]) continue;
            const from = store.get(pair[0]);
            const to = store.get(pair[1]);
            if (std.meta.activeTag(from) != std.meta.activeTag(to)) {
                if (from == .nominal and from.nominal.has_backing) {
                    try pending.append(self.allocator, .{ try self.program.converter().backing(pair[0]), pair[1] });
                } else if (to == .nominal and to.nominal.has_backing) {
                    try pending.append(self.allocator, .{ pair[0], try self.program.converter().backing(pair[1]) });
                } else if (Program.paddedAgainstEmpty(from, to)) {
                    return true;
                }
                continue;
            }
            switch (from) {
                .record => |record| {
                    if (!store.sameRecordOrder(record, to.record)) return true;
                    for (store.fieldSpan(to.record.fields)) |to_field| {
                        for (store.fieldSpan(record.fields)) |from_field| {
                            if (from_field.name == to_field.name) try pending.append(self.allocator, .{ from_field.ty, to_field.ty });
                        }
                    }
                },
                .tuple => |items| for (store.span(items), store.span(to.tuple)) |a, b| try pending.append(self.allocator, .{ a, b }),
                .tag_union => |tag_union| for (store.tagSpan(to.tag_union.tags)) |to_tag| {
                    for (store.tagSpan(tag_union.tags)) |from_tag| {
                        if (from_tag.name != to_tag.name or from_tag.payloads.len != to_tag.payloads.len) continue;
                        for (store.span(from_tag.payloads), store.span(to_tag.payloads)) |a, b| try pending.append(self.allocator, .{ a, b });
                    }
                },
                .list => |elem| try pending.append(self.allocator, .{ elem, to.list }),
                .box => |payload| try pending.append(self.allocator, .{ payload, to.box }),
                .nominal => |nominal| for (store.nominalArgs(nominal), store.nominalArgs(to.nominal)) |a, b| try pending.append(self.allocator, .{ a, b }),
                .primitive, .empty_record, .function, .variable => {},
            }
        }
        return false;
    }

    /// A debug rendering of `ty`, for invariant messages.
    fn debugTypeText(self: *Builder, ty: TypeId, writer: *std.Io.Writer, depth: u32) std.Io.Writer.Error!void {
        const store = &self.program.store;
        if (depth > 6) return try writer.writeAll("...");
        switch (store.get(ty)) {
            .primitive => |primitive| try writer.print("{s}", .{@tagName(primitive)}),
            .empty_record => try writer.writeAll("{}"),
            .variable => |index| try writer.print("var{d}", .{index}),
            .list => |elem| {
                try writer.writeAll("List(");
                try self.debugTypeText(elem, writer, depth + 1);
                try writer.writeAll(")");
            },
            .box => |payload| {
                try writer.writeAll("box(");
                try self.debugTypeText(payload, writer, depth + 1);
                try writer.writeAll(")");
            },
            .tuple => |items| {
                try writer.writeAll("(");
                for (store.span(items), 0..) |item, index| {
                    if (index != 0) try writer.writeAll(", ");
                    try self.debugTypeText(item, writer, depth + 1);
                }
                try writer.writeAll(")");
            },
            .record => |record| {
                try writer.writeAll("{");
                for (store.fieldSpan(record.fields), 0..) |field, index| {
                    if (index != 0) try writer.writeAll(", ");
                    try writer.print("{s}#{s}: ", .{ store.nameText(field.name), @tagName(field.kind) });
                    try self.debugTypeText(field.ty, writer, depth + 1);
                }
                if (record.ext) |ext| {
                    try writer.writeAll(" | ");
                    try self.debugTypeText(ext, writer, depth + 1);
                }
                try writer.writeAll("}");
            },
            .tag_union => |tag_union| {
                try writer.writeAll("[");
                for (store.tagSpan(tag_union.tags), 0..) |tag, index| {
                    if (index != 0) try writer.writeAll(", ");
                    try writer.print("{s}(", .{store.nameText(tag.name)});
                    for (store.span(tag.payloads), 0..) |payload, payload_index| {
                        if (payload_index != 0) try writer.writeAll(", ");
                        try self.debugTypeText(payload, writer, depth + 1);
                    }
                    try writer.writeAll(")");
                }
                if (tag_union.ext) |ext| {
                    try writer.writeAll(" | ");
                    try self.debugTypeText(ext, writer, depth + 1);
                }
                try writer.writeAll("]");
            },
            .nominal => |nominal| {
                try writer.print("{s}#{d}.{d}{s}{s}(", .{ store.nameText(nominal.name), @backingInt(nominal.decl.module), @backingInt(nominal.decl.decl), if (nominal.is_opaque) "!" else "", if (nominal.has_backing) "" else "?" });
                for (store.nominalArgs(nominal), 0..) |arg, index| {
                    if (index != 0) try writer.writeAll(", ");
                    try self.debugTypeText(arg, writer, depth + 1);
                }
                try writer.writeAll(")");
            },
            .function => |function| {
                try writer.writeAll("(");
                for (store.span(function.args), 0..) |arg, index| {
                    if (index != 0) try writer.writeAll(", ");
                    try self.debugTypeText(arg, writer, depth + 1);
                }
                try writer.writeAll(" -> ");
                try self.debugTypeText(function.ret, writer, depth + 1);
                try writer.writeAll(")");
            },
        }
    }

    pub fn dispatchPlan(self: *const Builder, id: static_dispatch.StaticDispatchPlanId) static_dispatch.StaticDispatchCallPlan {
        return self.view.view.static_dispatch_plans.plans[@backingInt(id)];
    }

    pub fn lowerDispatch(self: *Builder, expr_id: ExprId, plan_id: static_dispatch.StaticDispatchPlanId) Allocator.Error!void {
        try self.lowerDispatchAt(plan_id, try self.exprType(expr_id));
    }

    /// The equality test of a custom literal pattern: its `is_eq` dispatch
    /// on the value the pattern matches and the pattern's literal converted
    /// at its type (`checked_bodies.LiteralPatternGuard`).
    fn lowerLiteralPatternGuard(self: *Builder, pattern: PatternId) Allocator.Error!void {
        const guard = self.bodies().literalPatternGuard(pattern) orelse invariant("a literal guard was asked of a pattern compared directly");
        self.region = self.nodeRegion(@backingInt(pattern));
        try self.lowerDispatchAt(guard.equality_plan orelse invariant("a custom literal pattern had no checked equality plan"), try self.convert(guard.equality_ty));
    }

    /// The value of a custom literal pattern's guard operand where no
    /// execution reaches it: a value of the pattern's type.
    fn literalPatternDeadValue(self: *Builder, pattern: PatternId) Allocator.Error!LocalId {
        if (try self.program.hasError(self.module, self.bodies().patternType(pattern))) return try self.closedLocal(.zst);
        return try self.localOf(try self.patternType(pattern), .frame);
    }

    /// Push a dispatch operand: a checked expression, a value the checker
    /// generated for a literal conversion, or an operand of a custom
    /// literal pattern's equality.
    pub fn pushOperand(self: *Builder, operand: static_dispatch.StaticDispatchOperand, ty: TypeId) Allocator.Error!void {
        switch (operand) {
            .checked_expr => |expr| try self.push(.{ .own_expr = expr }),
            .generated_numeral => |literal| try self.push(.{ .numeral_arg = .{ .literal = literal, .ty = ty } }),
            .generated_quote => |node| try self.push(.{ .push_value_quote = node }),
            .generated_interpolation_segments => |expr| try self.push(.{ .segments_arg = .{ .expr = expr, .ty = ty } }),
            .literal_pattern_value => |pattern| try self.push(.{ .literal_pattern_value = pattern }),
            .literal_pattern_conversion => |pattern| try self.push(.{ .literal_pattern_conversion = pattern }),
        }
    }

    /// The type of a structural equality's operand.
    fn operandType(self: *Builder, operand: static_dispatch.StaticDispatchOperand) Allocator.Error!TypeId {
        return switch (operand) {
            .checked_expr => |expr| try self.exprType(expr),
            .literal_pattern_value, .literal_pattern_conversion => |pattern| try self.patternType(pattern),
            .generated_interpolation_segments, .generated_numeral, .generated_quote => invariant("structural equality operand is not an expression"),
        };
    }

    pub fn operandTypes(self: *Builder, call_plan: static_dispatch.StaticDispatchCallPlan) Allocator.Error![]const TypeId {
        return switch (self.program.store.get(try self.dispatchCallable(call_plan.resolution, call_plan.callable_ty))) {
            .function => |function| self.program.store.span(function.args),
            .primitive, .empty_record, .record, .tuple, .tag_union, .list, .box, .nominal, .variable => invariant("a dispatch's callable type is not a function type"),
        };
    }

    pub fn lowerDispatchAt(self: *Builder, plan_id: static_dispatch.StaticDispatchPlanId, result_ty: TypeId) Allocator.Error!void {
        const table = self.view.view.static_dispatch_plans;
        const call_plan = self.dispatchPlan(plan_id);
        const operands = call_plan.argsSlice(table);
        const operand_types = try self.allocator.dupe(TypeId, try self.operandTypes(call_plan));
        defer self.allocator.free(operand_types);
        if (operand_types.len != operands.len) invariant("a dispatch's operand count differs from its callable type");
        switch (call_plan.resolution) {
            .direct_closed, .direct_parametric => |direct| {
                const node = table.evidenceNode(direct.evidence);
                if (node.target.kind == .local_proc) {
                    // A method declared in a function body is a call of
                    // its local procedure's function value.
                    const fn_ty = try self.dispatchCallable(call_plan.resolution, call_plan.callable_ty);
                    const value = try self.localMethodValue(node, fn_ty);
                    switch (call_plan.result_mode) {
                        .value, .parser_for, .encoder_for, .hash, .map, .map_effectful => {},
                        .equality => |equality| if (equality.negated) {
                            try self.push(.{ .finish_low_level = .{ .op = .bool_not, .arg_count = 1, .result = result_ty } });
                        },
                    }
                    try self.push(.{ .finish_erased_call = .{ .fn_ty = fn_ty, .arg_count = @intCast(operands.len), .result = if (call_plan.result_mode == .equality) try self.program.store.primitive(.bool) else result_ty } });
                    var index = operands.len;
                    while (index > 0) {
                        index -= 1;
                        try self.pushOperand(operands[index], operand_types[index]);
                    }
                    try self.pushValue(value);
                    return;
                }
                if (node.target.kind == .callable_binding) {
                    // A method bound to a computed callable is a call of
                    // the function value its root evaluated.
                    const fn_ty = try self.dispatchCallable(call_plan.resolution, call_plan.callable_ty);
                    switch (call_plan.result_mode) {
                        .value, .parser_for, .encoder_for, .hash, .map, .map_effectful => {},
                        .equality => |equality| if (equality.negated) {
                            try self.push(.{ .finish_low_level = .{ .op = .bool_not, .arg_count = 1, .result = result_ty } });
                        },
                    }
                    try self.push(.{ .finish_erased_call = .{ .fn_ty = fn_ty, .arg_count = @intCast(operands.len), .result = if (call_plan.result_mode == .equality) try self.program.store.primitive(.bool) else result_ty } });
                    var index = operands.len;
                    while (index > 0) {
                        index -= 1;
                        try self.pushOperand(operands[index], operand_types[index]);
                    }
                    const binding = self.program.callableEvalBinding(node.target.kind.callable_binding);
                    try self.push(.{ .restore_callable_eval = .{ .module = binding.module, .template = binding.template, .ty = fn_ty } });
                    return;
                }
                const callee = try self.methodTargetWorker(node.target);
                const call_fn = try self.dispatchCallable(call_plan.resolution, call_plan.callable_ty);
                const instance = switch (node.instantiation) {
                    // Reached through a procedure alias, the target takes the
                    // instantiation the call's callable type gives it.
                    .alias => try self.aliasTargetInstance(node, callee, .{ .node = direct.evidence }),
                    .monomorphic, .callable, .derived_from_callable => blk: {
                        const subst = table.site_substitutions[node.subst.start..][0..node.subst.len];
                        break :blk switch (node.nested) {
                            .resolved => try self.callInstanceAt(callee, subst, table.nestedEvidence(node), call_fn, .{ .node = direct.evidence }),
                            .from_callable => try self.callableDerivedInstance(callee, subst, call_fn),
                            .through_alias => invariant("alias-scope evidence accompanies only an alias instantiation"),
                        };
                    },
                };
                switch (call_plan.result_mode) {
                    // A codec method is an ordinary call of its target.
                    .value, .parser_for, .encoder_for, .hash, .map, .map_effectful => {},
                    .equality => |equality| if (equality.negated) {
                        try self.push(.{ .finish_low_level = .{ .op = .bool_not, .arg_count = 1, .result = result_ty } });
                    },
                }
                try self.push(.{ .finish_call = .{ .callee = callee, .hidden = instance.hidden, .bound = instance.bound, .arg_types = try self.program.arena.allocator().dupe(TypeId, operand_types), .ret = instance.ret, .result = if (call_plan.result_mode == .equality) try self.program.store.primitive(.bool) else result_ty } });
                var index = operands.len;
                while (index > 0) {
                    index -= 1;
                    try self.pushOperand(operands[index], operand_types[index]);
                }
            },
            .structural => |derivation| switch (derivation) {
                .hash => {
                    if (operands.len != 2) invariant("structural hashing has a value and a hasher");
                    try self.push(.{ .finish_hash = .{ .value = operand_types[0], .result = result_ty } });
                    try self.pushOperand(operands[1], operand_types[1]);
                    try self.pushOperand(operands[0], operand_types[0]);
                },
                .equality => {
                    const negated = switch (call_plan.result_mode) {
                        .equality => |equality| equality.negated,
                        .value, .hash, .parser_for, .encoder_for, .map, .map_effectful => false,
                    };
                    if (operands.len != 2) invariant("structural equality has two operands");
                    switch (operands[1]) {
                        .checked_expr, .literal_pattern_value, .literal_pattern_conversion => {},
                        .generated_interpolation_segments, .generated_numeral, .generated_quote => invariant("structural equality operand is not an expression"),
                    }
                    try self.push(.{ .finish_eq = .{ .negated = negated, .operand = try self.operandType(operands[0]) } });
                    try self.pushOperand(operands[1], operand_types[1]);
                    try self.pushOperand(operands[0], operand_types[0]);
                },
                .parser, .encoder => try Codec.dispatch(self, call_plan, operands, operand_types, result_ty),
                .map, .map_effectful => |plan| {
                    // A derived map is a call of its dictionary.
                    const fn_ty = try self.convert(call_plan.callable_ty);
                    const dict = try self.structuralMapDictionary(plan, derivation == .map_effectful, fn_ty);
                    try self.push(.{ .finish_erased_call = .{ .fn_ty = fn_ty, .arg_count = @intCast(operands.len), .result = result_ty } });
                    var index = operands.len;
                    while (index > 0) {
                        index -= 1;
                        try self.pushOperand(operands[index], operand_types[index]);
                    }
                    try self.pushValue(try self.materialize(dict));
                },
            },
            .evidence_dependent => |dependent| {
                // A requirement on a sealed scheme variable is no
                // dictionary parameter (design.md "Sealed Result Rows"): its
                // dictionary is resolved from the variable's sealed type,
                // as a callable-derived requirement's is.
                const dict = try self.materialize((try self.scopeDictionaryIfAny(dependent.index)) orelse try self.sealedScopeDictionary(dependent.index));
                // The dictionary's method is called at the interface its
                // scope's requirement names; its result is read at the
                // dispatch's type.
                const fn_ty = (try self.scopeInterface(dependent.index)) orelse try self.convert(call_plan.callable_ty);
                switch (call_plan.result_mode) {
                    // A codec method is an ordinary call of its target.
                    .value, .parser_for, .encoder_for, .hash, .map, .map_effectful => {},
                    .equality => |equality| if (equality.negated) {
                        try self.push(.{ .finish_low_level = .{ .op = .bool_not, .arg_count = 1, .result = result_ty } });
                    },
                }
                const call_result = switch (self.program.store.get(try self.convert(call_plan.callable_ty))) {
                    .function => |function| function.ret,
                    .primitive, .empty_record, .record, .tuple, .tag_union, .list, .box, .nominal, .variable => invariant("a dispatch's callable type is not a function type"),
                };
                try self.push(.{ .finish_erased_call = .{ .fn_ty = fn_ty, .arg_count = @intCast(operands.len), .result = call_result } });
                var index = operands.len;
                while (index > 0) {
                    index -= 1;
                    try self.pushOperand(operands[index], operand_types[index]);
                }
                try self.pushValue(dict);
            },
            // A dispatch that cannot run evaluates its operands, each for its
            // effects, then crashes.
            .checked_error, .@"unreachable" => {
                try self.push(.{ .dispatch_crash = .{
                    .kind = if (call_plan.resolution == .checked_error) .checked_error else .unreachable_value,
                    .result = result_ty,
                } });
                var index = operands.len;
                while (index > 0) {
                    index -= 1;
                    try self.push(.discard);
                    try self.pushOperand(operands[index], operand_types[index]);
                }
            },
            .direct_pending => invariant("a checked module holds a pending dispatch resolution"),
        }
    }

    /// Emit a low-level operation. An operation over values of open types
    /// is a list operation, which takes the element descriptor of its list
    /// as a trailing argument (`LowLevelExtra.dyn_elem_desc`), or a box
    /// operation, which lowers to the dynamic box statements.
    pub fn finishLowLevel(self: *Builder, op: LIR.LowLevel, arg_count: u32, result_ty: TypeId, arg_types: []const TypeId) Allocator.Error!void {
        const args = try self.popValues(arg_count);
        defer self.allocator.free(args);
        try self.emitLowLevelCall(op, args, result_ty, arg_types);
    }

    fn emitLowLevelCall(self: *Builder, op: LIR.LowLevel, args: []const LocalId, result_ty: TypeId, arg_types: []const TypeId) Allocator.Error!void {
        const store = &self.program.store;
        var open = !store.isClosed(result_ty);
        for (arg_types) |arg_ty| open = open or !store.isClosed(arg_ty);
        if (op == .list_map_can_reuse) return try self.pushValue(try self.listMapCanReuse(args, arg_types, result_ty));
        if (op == .list_prefetched) {
            // The result is the list itself, so it is the argument; the
            // hint is a separate statement that borrows it.
            if (args.len != 2) invariant("list_prefetched reached Boxy lowering with the wrong arity");
            const hint = try self.closedLocal(.zst);
            if (open) {
                const elem = switch (store.get(arg_types[0])) {
                    .list => |elem| elem,
                    .primitive, .empty_record, .record, .tuple, .tag_union, .box, .nominal, .function, .variable => invariant("list_prefetched's first operand is not a List"),
                };
                try self.emitLinear(.{ .assign_low_level = .{
                    .target = hint,
                    .op = .list_prefetch,
                    .rc_effect = try self.store.addRcEffect(LIR.LowLevel.list_prefetch.rcEffect()),
                    .extra = try self.store.addStmtRow(LIR.LowLevelExtra, .{ .dyn_elem_desc = true }),
                    .args = try self.store.addLocalSpan(&.{ args[0], args[1], try self.descOf(elem) }),
                    .next = undefined,
                } });
            } else {
                try self.emitLowLevel(hint, .list_prefetch, args);
            }
            return try self.pushValue(args[0]);
        }
        if (op == .crash) {
            // A crash ends its block; the result stands for the value no
            // execution reaches.
            try self.emitTerminal(.{ .crash = .{ .msg = .{ .local = args[0] } } });
            return try self.pushValue(try self.localOf(result_ty, .frame));
        }
        // `Box(function)` is the function's erased callable: boxing or
        // unboxing one is the callable itself.
        switch (op) {
            .boxed_sequence_pack, .boxed_sequence_unpack => invariant("LIR-only sequence operation reached checked-body lowering"),
            .box_box => if (store.get(result_ty) == .box and store.get(store.get(result_ty).box) == .function) return try self.pushValue(args[0]),
            .box_unbox, .box_unbox_borrowed => if (store.get(result_ty) == .function) return try self.pushValue(args[0]),
            .str_is_eq, .str_is_eq_static_small, .str_static_small_word_eq, .str_static_small_word_caseless_eq, .str_concat, .str_append, .str_contains, .str_trim, .str_trim_start, .str_trim_end, .str_caseless_ascii_equals, .str_with_ascii_lowercased, .str_with_ascii_uppercased, .str_starts_with, .str_ends_with, .str_repeat, .str_drop_prefix, .str_drop_prefix_caseless_ascii, .str_drop_suffix, .str_split_first, .str_split_last, .str_count_utf8_bytes, .str_get_utf8_byte_unsafe, .str_substring_unsafe, .str_with_capacity, .str_reserve, .str_release_excess_capacity, .str_to_utf8, .str_from_utf8_lossy, .str_from_utf8_validated, .str_from_utf16_le_short, .str_from_utf16_be_short, .str_from_utf32_le_short, .str_from_utf32_be_short, .str_from_utf8, .str_split_on, .str_join_with, .str_from_parts, .str_inspect, .u8_to_str, .i8_to_str, .u16_to_str, .i16_to_str, .u32_to_str, .i32_to_str, .u64_to_str, .i64_to_str, .u128_to_str, .i128_to_str, .dec_to_str, .f32_to_str, .f64_to_str, .list_len, .list_capacity, .list_get_unsafe, .list_prefetched, .list_prefetch, .list_append_unsafe, .list_concat, .list_append_range_within, .list_copy_range_within, .list_append_range_within_unsafe, .list_append_sublist, .list_append_le_bytes, .list_append_le_bytes_unsafe, .list_slack_unique, .list_owned_unique, .list_set_in_place_unsafe, .list_with_capacity, .list_drop_at, .list_sublist, .list_sublist_borrowed, .list_set, .list_replace_unsafe, .list_swap, .list_prepend, .list_take_first, .list_take_last, .list_reverse, .list_reserve, .list_reserve_for_append, .list_release_excess_capacity, .list_clear, .list_map_prepare_reuse, .list_map_can_reuse, .list_map_cast_unsafe, .list_map_extract_unsafe, .list_move_out_unsafe, .list_map_write_unsafe, .list_item_size_at_most, .bool_not, .bool_likely, .dict_pseudo_seed, .hasher_finish, .hasher_write_bool, .hasher_write_u8, .hasher_write_u16, .hasher_write_u32, .hasher_write_u64, .hasher_write_u128, .hasher_write_i8, .hasher_write_i16, .hasher_write_i32, .hasher_write_i64, .hasher_write_i128, .hasher_write_f32, .hasher_write_f64, .hasher_write_dec, .hasher_write_bytes, .hasher_write_str, .crypto_sha256_hash_bytes, .crypto_sha256_hasher_empty, .crypto_sha256_hasher_write, .crypto_sha256_hasher_finish, .crypto_blake3_hash_bytes, .crypto_blake3_hasher_empty, .crypto_blake3_hasher_write, .crypto_blake3_hasher_finish, .num_is_eq, .num_is_gt, .num_is_gte, .num_is_lt, .num_is_lte, .num_negate, .num_abs, .num_abs_diff, .num_plus, .num_minus, .num_times, .num_int_add_wrap, .num_int_add_crash_on_overflow, .num_int_add_overflows, .num_int_add_proven_cannot_overflow, .num_int_sub_wrap, .num_int_sub_crash_on_overflow, .num_int_sub_overflows, .num_int_sub_proven_cannot_overflow, .num_int_mul_wrap, .num_int_mul_crash_on_overflow, .num_int_mul_overflows, .num_int_mul_proven_cannot_overflow, .num_float_add, .num_float_sub, .num_float_mul, .dec_mul, .num_div_by, .num_div_by_checked, .num_div_trunc_by, .num_div_trunc_by_checked, .num_rem_by, .num_rem_by_checked, .num_mod_by, .num_mod_by_checked, .num_negate_checked, .num_abs_checked, .num_pow, .num_atan2, .num_sqrt, .num_sin, .num_cos, .num_tan, .num_asin, .num_acos, .num_atan, .num_floor, .num_ceiling, .f32_to_bits, .f32_from_bits, .f64_to_bits, .f64_from_bits, .num_shift_left_by, .num_shift_right_by, .num_shift_right_zf_by, .num_bitwise_and, .num_bitwise_or, .num_bitwise_xor, .num_bitwise_not, .num_count_one_bits, .num_count_leading_zero_bits, .num_count_trailing_zero_bits, .num_from_le_bytes_unchecked, .simd_load_16_unchecked, .simd_store_16_unchecked, .simd_append_16, .simd_splat, .simd_get_lane_unchecked, .simd_with_lane_unchecked, .simd_to_u128_bits, .simd_from_u128_bits, .simd_add_wrap, .simd_sub_wrap, .simd_add_sat, .simd_sub_sat, .simd_neg_wrap, .simd_abs_wrap, .simd_min, .simd_max, .simd_abs_diff, .simd_avg_rounded, .simd_mul_wrap, .simd_mul_high, .simd_mul_q15_sat, .simd_mul_wide_lo, .simd_mul_wide_hi, .simd_dot_pairs, .simd_dot_pairs_sat, .simd_sad, .simd_and, .simd_or, .simd_xor, .simd_not, .simd_bit_select, .simd_eq_lanes, .simd_gt_lanes, .simd_gte_lanes, .simd_bitmask, .simd_shl_wrap, .simd_shr_wrap, .simd_shr_zf_wrap, .simd_shr_rounded, .simd_interleave_lo, .simd_interleave_hi, .simd_even_lanes, .simd_odd_lanes, .simd_reverse_lanes, .simd_table_lookup, .simd_concat_shift_bytes, .simd_widen_lo, .simd_widen_hi, .simd_pairwise_add_widen, .simd_narrow_wrap, .simd_narrow_sat, .simd_sum_lanes, .simd_sum_lanes_wrap, .simd_clmul_lo, .simd_clmul_hi, .u8_from_str, .i8_from_str, .u16_from_str, .i16_from_str, .u32_from_str, .i32_from_str, .u64_from_str, .i64_from_str, .u128_from_str, .i128_from_str, .dec_from_str, .dec_to_attos, .dec_from_attos, .f32_from_str, .f64_from_str, .u8_from_str_prefix, .u8_from_utf8_prefix, .i8_from_str_prefix, .i8_from_utf8_prefix, .u16_from_str_prefix, .u16_from_utf8_prefix, .i16_from_str_prefix, .i16_from_utf8_prefix, .u32_from_str_prefix, .u32_from_utf8_prefix, .i32_from_str_prefix, .i32_from_utf8_prefix, .u64_from_str_prefix, .u64_from_utf8_prefix, .i64_from_str_prefix, .i64_from_utf8_prefix, .u128_from_str_prefix, .u128_from_utf8_prefix, .i128_from_str_prefix, .i128_from_utf8_prefix, .dec_from_str_prefix, .dec_from_utf8_prefix, .f32_from_str_prefix, .f32_from_utf8_prefix, .f64_from_str_prefix, .f64_from_utf8_prefix, .u8_to_i8_wrap, .u8_to_i8_try, .u8_to_i16, .u8_to_i32, .u8_to_i64, .u8_to_i128, .u8_to_u16, .u8_to_u32, .u8_to_u64, .u8_to_u128, .u8_to_f32, .u8_to_f64, .u8_to_dec, .i8_to_i16, .i8_to_i32, .i8_to_i64, .i8_to_i128, .i8_to_u8_wrap, .i8_to_u8_try, .i8_to_u16_wrap, .i8_to_u16_try, .i8_to_u32_wrap, .i8_to_u32_try, .i8_to_u64_wrap, .i8_to_u64_try, .i8_to_u128_wrap, .i8_to_u128_try, .i8_to_f32, .i8_to_f64, .i8_to_dec, .u16_to_i8_wrap, .u16_to_i8_try, .u16_to_i16_wrap, .u16_to_i16_try, .u16_to_i32, .u16_to_i64, .u16_to_i128, .u16_to_u8_wrap, .u16_to_u8_try, .u16_to_u32, .u16_to_u64, .u16_to_u128, .u16_to_f32, .u16_to_f64, .u16_to_dec, .i16_to_i8_wrap, .i16_to_i8_try, .i16_to_i32, .i16_to_i64, .i16_to_i128, .i16_to_u8_wrap, .i16_to_u8_try, .i16_to_u16_wrap, .i16_to_u16_try, .i16_to_u32_wrap, .i16_to_u32_try, .i16_to_u64_wrap, .i16_to_u64_try, .i16_to_u128_wrap, .i16_to_u128_try, .i16_to_f32, .i16_to_f64, .i16_to_dec, .u32_to_i8_wrap, .u32_to_i8_try, .u32_to_i16_wrap, .u32_to_i16_try, .u32_to_i32_wrap, .u32_to_i32_try, .u32_to_i64, .u32_to_i128, .u32_to_u8_wrap, .u32_to_u8_try, .u32_to_u16_wrap, .u32_to_u16_try, .u32_to_u64, .u32_to_u128, .u32_to_f32, .u32_to_f64, .u32_to_dec, .i32_to_i8_wrap, .i32_to_i8_try, .i32_to_i16_wrap, .i32_to_i16_try, .i32_to_i64, .i32_to_i128, .i32_to_u8_wrap, .i32_to_u8_try, .i32_to_u16_wrap, .i32_to_u16_try, .i32_to_u32_wrap, .i32_to_u32_try, .i32_to_u64_wrap, .i32_to_u64_try, .i32_to_u128_wrap, .i32_to_u128_try, .i32_to_f32, .i32_to_f64, .i32_to_dec, .u64_to_i8_wrap, .u64_to_i8_try, .u64_to_i16_wrap, .u64_to_i16_try, .u64_to_i32_wrap, .u64_to_i32_try, .u64_to_i64_wrap, .u64_to_i64_try, .u64_to_i128, .u64_to_u8_wrap, .u64_to_u8_try, .u64_to_u16_wrap, .u64_to_u16_try, .u64_to_u32_wrap, .u64_to_u32_try, .u64_to_u128, .u64_to_f32, .u64_to_f64, .u64_to_dec, .i64_to_i8_wrap, .i64_to_i8_try, .i64_to_i16_wrap, .i64_to_i16_try, .i64_to_i32_wrap, .i64_to_i32_try, .i64_to_i128, .i64_to_u8_wrap, .i64_to_u8_try, .i64_to_u16_wrap, .i64_to_u16_try, .i64_to_u32_wrap, .i64_to_u32_try, .i64_to_u64_wrap, .i64_to_u64_try, .i64_to_u128_wrap, .i64_to_u128_try, .i64_to_f32, .i64_to_f64, .i64_to_dec, .u128_to_i8_wrap, .u128_to_i8_try, .u128_to_i16_wrap, .u128_to_i16_try, .u128_to_i32_wrap, .u128_to_i32_try, .u128_to_i64_wrap, .u128_to_i64_try, .u128_to_i128_wrap, .u128_to_i128_try, .u128_to_u8_wrap, .u128_to_u8_try, .u128_to_u16_wrap, .u128_to_u16_try, .u128_to_u32_wrap, .u128_to_u32_try, .u128_to_u64_wrap, .u128_to_u64_try, .u128_to_f32, .u128_to_f64, .u128_to_dec_try_unsafe, .i128_to_i8_wrap, .i128_to_i8_try, .i128_to_i16_wrap, .i128_to_i16_try, .i128_to_i32_wrap, .i128_to_i32_try, .i128_to_i64_wrap, .i128_to_i64_try, .i128_to_u8_wrap, .i128_to_u8_try, .i128_to_u16_wrap, .i128_to_u16_try, .i128_to_u32_wrap, .i128_to_u32_try, .i128_to_u64_wrap, .i128_to_u64_try, .i128_to_u128_wrap, .i128_to_u128_try, .i128_to_f32, .i128_to_f64, .i128_to_dec_try_unsafe, .f32_to_i8_trunc, .f32_to_i8_try_unsafe, .f32_to_i16_trunc, .f32_to_i16_try_unsafe, .f32_to_i32_trunc, .f32_to_i32_try_unsafe, .f32_to_i64_trunc, .f32_to_i64_try_unsafe, .f32_to_i128_trunc, .f32_to_i128_try_unsafe, .f32_to_u8_trunc, .f32_to_u8_try_unsafe, .f32_to_u16_trunc, .f32_to_u16_try_unsafe, .f32_to_u32_trunc, .f32_to_u32_try_unsafe, .f32_to_u64_trunc, .f32_to_u64_try_unsafe, .f32_to_u128_trunc, .f32_to_u128_try_unsafe, .f32_to_f64, .f64_to_i8_trunc, .f64_to_i8_try_unsafe, .f64_to_i16_trunc, .f64_to_i16_try_unsafe, .f64_to_i32_trunc, .f64_to_i32_try_unsafe, .f64_to_i64_trunc, .f64_to_i64_try_unsafe, .f64_to_i128_trunc, .f64_to_i128_try_unsafe, .f64_to_u8_trunc, .f64_to_u8_try_unsafe, .f64_to_u16_trunc, .f64_to_u16_try_unsafe, .f64_to_u32_trunc, .f64_to_u32_try_unsafe, .f64_to_u64_trunc, .f64_to_u64_try_unsafe, .f64_to_u128_trunc, .f64_to_u128_try_unsafe, .f64_to_f32_wrap, .f64_to_f32_try_unsafe, .dec_to_i8_trunc, .dec_to_i8_try_unsafe, .dec_to_i16_trunc, .dec_to_i16_try_unsafe, .dec_to_i32_trunc, .dec_to_i32_try_unsafe, .dec_to_i64_trunc, .dec_to_i64_try_unsafe, .dec_to_i128_trunc, .dec_to_u8_trunc, .dec_to_u8_try_unsafe, .dec_to_u16_trunc, .dec_to_u16_try_unsafe, .dec_to_u32_trunc, .dec_to_u32_try_unsafe, .dec_to_u64_trunc, .dec_to_u64_try_unsafe, .dec_to_u128_trunc, .dec_to_u128_try_unsafe, .dec_to_f32_wrap, .dec_to_f32_try_unsafe, .dec_to_f64, .box_prepare_update, .erased_capture_load, .ptr_alloca, .box_alloc_zeroed, .ptr_store, .ptr_load, .ptr_cast, .compare, .crash => {},
        }
        const sequence_box = (op == .box_box and store.get(result_ty) == .box and store.isSequence(store.get(result_ty).box)) or
            ((op == .box_unbox or op == .box_unbox_borrowed) and store.isSequence(result_ty));
        if (!open and !sequence_box) {
            const target = try self.localOf(result_ty, .frame);
            // Source arithmetic commits to its operand type's behavior.
            const committed = if (args.len != 0) CheckedArithmetic.lowerOp(op, self.store.getLocal(args[0]).layout_idx) else op;
            try self.emitLowLevel(target, committed, args);
            return try self.pushValue(target);
        }
        switch (op) {
            .boxed_sequence_pack, .boxed_sequence_unpack => invariant("LIR-only sequence operation reached a generic source operation"),
            .box_box => {
                const target = try self.localOf(result_ty, .frame);
                try self.emitDyn(target, .{ .box = .{ .desc = try self.descOf(arg_types[0]), .payload = args[0] } });
                return try self.pushValue(target);
            },
            .box_unbox, .box_unbox_borrowed => {
                const view = try self.localOf(result_ty, .view);
                try self.emitDyn(view, .{ .box_payload_addr = .{ .box = args[0], .desc = try self.descOf(result_ty) } });
                const target = try self.localOf(result_ty, .frame);
                try self.emitDyn(target, .{ .copy = .{ .src = view, .desc = try self.descOf(result_ty) } });
                return try self.pushValue(target);
            },
            .str_is_eq, .str_is_eq_static_small, .str_static_small_word_eq, .str_static_small_word_caseless_eq, .str_concat, .str_append, .str_contains, .str_trim, .str_trim_start, .str_trim_end, .str_caseless_ascii_equals, .str_with_ascii_lowercased, .str_with_ascii_uppercased, .str_starts_with, .str_ends_with, .str_repeat, .str_drop_prefix, .str_drop_prefix_caseless_ascii, .str_drop_suffix, .str_split_first, .str_split_last, .str_count_utf8_bytes, .str_get_utf8_byte_unsafe, .str_substring_unsafe, .str_with_capacity, .str_reserve, .str_release_excess_capacity, .str_to_utf8, .str_from_utf8_lossy, .str_from_utf8_validated, .str_from_utf16_le_short, .str_from_utf16_be_short, .str_from_utf32_le_short, .str_from_utf32_be_short, .str_from_utf8, .str_split_on, .str_join_with, .str_from_parts, .str_inspect, .u8_to_str, .i8_to_str, .u16_to_str, .i16_to_str, .u32_to_str, .i32_to_str, .u64_to_str, .i64_to_str, .u128_to_str, .i128_to_str, .dec_to_str, .f32_to_str, .f64_to_str, .list_len, .list_capacity, .list_get_unsafe, .list_prefetched, .list_prefetch, .list_append_unsafe, .list_concat, .list_append_range_within, .list_copy_range_within, .list_append_range_within_unsafe, .list_append_sublist, .list_append_le_bytes, .list_append_le_bytes_unsafe, .list_slack_unique, .list_owned_unique, .list_set_in_place_unsafe, .list_with_capacity, .list_drop_at, .list_sublist, .list_sublist_borrowed, .list_set, .list_replace_unsafe, .list_swap, .list_prepend, .list_take_first, .list_take_last, .list_reverse, .list_reserve, .list_reserve_for_append, .list_release_excess_capacity, .list_clear, .list_map_prepare_reuse, .list_map_can_reuse, .list_map_cast_unsafe, .list_map_extract_unsafe, .list_move_out_unsafe, .list_map_write_unsafe, .list_item_size_at_most, .bool_not, .bool_likely, .dict_pseudo_seed, .hasher_finish, .hasher_write_bool, .hasher_write_u8, .hasher_write_u16, .hasher_write_u32, .hasher_write_u64, .hasher_write_u128, .hasher_write_i8, .hasher_write_i16, .hasher_write_i32, .hasher_write_i64, .hasher_write_i128, .hasher_write_f32, .hasher_write_f64, .hasher_write_dec, .hasher_write_bytes, .hasher_write_str, .crypto_sha256_hash_bytes, .crypto_sha256_hasher_empty, .crypto_sha256_hasher_write, .crypto_sha256_hasher_finish, .crypto_blake3_hash_bytes, .crypto_blake3_hasher_empty, .crypto_blake3_hasher_write, .crypto_blake3_hasher_finish, .num_is_eq, .num_is_gt, .num_is_gte, .num_is_lt, .num_is_lte, .num_negate, .num_abs, .num_abs_diff, .num_plus, .num_minus, .num_times, .num_int_add_wrap, .num_int_add_crash_on_overflow, .num_int_add_overflows, .num_int_add_proven_cannot_overflow, .num_int_sub_wrap, .num_int_sub_crash_on_overflow, .num_int_sub_overflows, .num_int_sub_proven_cannot_overflow, .num_int_mul_wrap, .num_int_mul_crash_on_overflow, .num_int_mul_overflows, .num_int_mul_proven_cannot_overflow, .num_float_add, .num_float_sub, .num_float_mul, .dec_mul, .num_div_by, .num_div_by_checked, .num_div_trunc_by, .num_div_trunc_by_checked, .num_rem_by, .num_rem_by_checked, .num_mod_by, .num_mod_by_checked, .num_negate_checked, .num_abs_checked, .num_pow, .num_atan2, .num_sqrt, .num_sin, .num_cos, .num_tan, .num_asin, .num_acos, .num_atan, .num_floor, .num_ceiling, .f32_to_bits, .f32_from_bits, .f64_to_bits, .f64_from_bits, .num_shift_left_by, .num_shift_right_by, .num_shift_right_zf_by, .num_bitwise_and, .num_bitwise_or, .num_bitwise_xor, .num_bitwise_not, .num_count_one_bits, .num_count_leading_zero_bits, .num_count_trailing_zero_bits, .num_from_le_bytes_unchecked, .simd_load_16_unchecked, .simd_store_16_unchecked, .simd_append_16, .simd_splat, .simd_get_lane_unchecked, .simd_with_lane_unchecked, .simd_to_u128_bits, .simd_from_u128_bits, .simd_add_wrap, .simd_sub_wrap, .simd_add_sat, .simd_sub_sat, .simd_neg_wrap, .simd_abs_wrap, .simd_min, .simd_max, .simd_abs_diff, .simd_avg_rounded, .simd_mul_wrap, .simd_mul_high, .simd_mul_q15_sat, .simd_mul_wide_lo, .simd_mul_wide_hi, .simd_dot_pairs, .simd_dot_pairs_sat, .simd_sad, .simd_and, .simd_or, .simd_xor, .simd_not, .simd_bit_select, .simd_eq_lanes, .simd_gt_lanes, .simd_gte_lanes, .simd_bitmask, .simd_shl_wrap, .simd_shr_wrap, .simd_shr_zf_wrap, .simd_shr_rounded, .simd_interleave_lo, .simd_interleave_hi, .simd_even_lanes, .simd_odd_lanes, .simd_reverse_lanes, .simd_table_lookup, .simd_concat_shift_bytes, .simd_widen_lo, .simd_widen_hi, .simd_pairwise_add_widen, .simd_narrow_wrap, .simd_narrow_sat, .simd_sum_lanes, .simd_sum_lanes_wrap, .simd_clmul_lo, .simd_clmul_hi, .u8_from_str, .i8_from_str, .u16_from_str, .i16_from_str, .u32_from_str, .i32_from_str, .u64_from_str, .i64_from_str, .u128_from_str, .i128_from_str, .dec_from_str, .dec_to_attos, .dec_from_attos, .f32_from_str, .f64_from_str, .u8_from_str_prefix, .u8_from_utf8_prefix, .i8_from_str_prefix, .i8_from_utf8_prefix, .u16_from_str_prefix, .u16_from_utf8_prefix, .i16_from_str_prefix, .i16_from_utf8_prefix, .u32_from_str_prefix, .u32_from_utf8_prefix, .i32_from_str_prefix, .i32_from_utf8_prefix, .u64_from_str_prefix, .u64_from_utf8_prefix, .i64_from_str_prefix, .i64_from_utf8_prefix, .u128_from_str_prefix, .u128_from_utf8_prefix, .i128_from_str_prefix, .i128_from_utf8_prefix, .dec_from_str_prefix, .dec_from_utf8_prefix, .f32_from_str_prefix, .f32_from_utf8_prefix, .f64_from_str_prefix, .f64_from_utf8_prefix, .u8_to_i8_wrap, .u8_to_i8_try, .u8_to_i16, .u8_to_i32, .u8_to_i64, .u8_to_i128, .u8_to_u16, .u8_to_u32, .u8_to_u64, .u8_to_u128, .u8_to_f32, .u8_to_f64, .u8_to_dec, .i8_to_i16, .i8_to_i32, .i8_to_i64, .i8_to_i128, .i8_to_u8_wrap, .i8_to_u8_try, .i8_to_u16_wrap, .i8_to_u16_try, .i8_to_u32_wrap, .i8_to_u32_try, .i8_to_u64_wrap, .i8_to_u64_try, .i8_to_u128_wrap, .i8_to_u128_try, .i8_to_f32, .i8_to_f64, .i8_to_dec, .u16_to_i8_wrap, .u16_to_i8_try, .u16_to_i16_wrap, .u16_to_i16_try, .u16_to_i32, .u16_to_i64, .u16_to_i128, .u16_to_u8_wrap, .u16_to_u8_try, .u16_to_u32, .u16_to_u64, .u16_to_u128, .u16_to_f32, .u16_to_f64, .u16_to_dec, .i16_to_i8_wrap, .i16_to_i8_try, .i16_to_i32, .i16_to_i64, .i16_to_i128, .i16_to_u8_wrap, .i16_to_u8_try, .i16_to_u16_wrap, .i16_to_u16_try, .i16_to_u32_wrap, .i16_to_u32_try, .i16_to_u64_wrap, .i16_to_u64_try, .i16_to_u128_wrap, .i16_to_u128_try, .i16_to_f32, .i16_to_f64, .i16_to_dec, .u32_to_i8_wrap, .u32_to_i8_try, .u32_to_i16_wrap, .u32_to_i16_try, .u32_to_i32_wrap, .u32_to_i32_try, .u32_to_i64, .u32_to_i128, .u32_to_u8_wrap, .u32_to_u8_try, .u32_to_u16_wrap, .u32_to_u16_try, .u32_to_u64, .u32_to_u128, .u32_to_f32, .u32_to_f64, .u32_to_dec, .i32_to_i8_wrap, .i32_to_i8_try, .i32_to_i16_wrap, .i32_to_i16_try, .i32_to_i64, .i32_to_i128, .i32_to_u8_wrap, .i32_to_u8_try, .i32_to_u16_wrap, .i32_to_u16_try, .i32_to_u32_wrap, .i32_to_u32_try, .i32_to_u64_wrap, .i32_to_u64_try, .i32_to_u128_wrap, .i32_to_u128_try, .i32_to_f32, .i32_to_f64, .i32_to_dec, .u64_to_i8_wrap, .u64_to_i8_try, .u64_to_i16_wrap, .u64_to_i16_try, .u64_to_i32_wrap, .u64_to_i32_try, .u64_to_i64_wrap, .u64_to_i64_try, .u64_to_i128, .u64_to_u8_wrap, .u64_to_u8_try, .u64_to_u16_wrap, .u64_to_u16_try, .u64_to_u32_wrap, .u64_to_u32_try, .u64_to_u128, .u64_to_f32, .u64_to_f64, .u64_to_dec, .i64_to_i8_wrap, .i64_to_i8_try, .i64_to_i16_wrap, .i64_to_i16_try, .i64_to_i32_wrap, .i64_to_i32_try, .i64_to_i128, .i64_to_u8_wrap, .i64_to_u8_try, .i64_to_u16_wrap, .i64_to_u16_try, .i64_to_u32_wrap, .i64_to_u32_try, .i64_to_u64_wrap, .i64_to_u64_try, .i64_to_u128_wrap, .i64_to_u128_try, .i64_to_f32, .i64_to_f64, .i64_to_dec, .u128_to_i8_wrap, .u128_to_i8_try, .u128_to_i16_wrap, .u128_to_i16_try, .u128_to_i32_wrap, .u128_to_i32_try, .u128_to_i64_wrap, .u128_to_i64_try, .u128_to_i128_wrap, .u128_to_i128_try, .u128_to_u8_wrap, .u128_to_u8_try, .u128_to_u16_wrap, .u128_to_u16_try, .u128_to_u32_wrap, .u128_to_u32_try, .u128_to_u64_wrap, .u128_to_u64_try, .u128_to_f32, .u128_to_f64, .u128_to_dec_try_unsafe, .i128_to_i8_wrap, .i128_to_i8_try, .i128_to_i16_wrap, .i128_to_i16_try, .i128_to_i32_wrap, .i128_to_i32_try, .i128_to_i64_wrap, .i128_to_i64_try, .i128_to_u8_wrap, .i128_to_u8_try, .i128_to_u16_wrap, .i128_to_u16_try, .i128_to_u32_wrap, .i128_to_u32_try, .i128_to_u64_wrap, .i128_to_u64_try, .i128_to_u128_wrap, .i128_to_u128_try, .i128_to_f32, .i128_to_f64, .i128_to_dec_try_unsafe, .f32_to_i8_trunc, .f32_to_i8_try_unsafe, .f32_to_i16_trunc, .f32_to_i16_try_unsafe, .f32_to_i32_trunc, .f32_to_i32_try_unsafe, .f32_to_i64_trunc, .f32_to_i64_try_unsafe, .f32_to_i128_trunc, .f32_to_i128_try_unsafe, .f32_to_u8_trunc, .f32_to_u8_try_unsafe, .f32_to_u16_trunc, .f32_to_u16_try_unsafe, .f32_to_u32_trunc, .f32_to_u32_try_unsafe, .f32_to_u64_trunc, .f32_to_u64_try_unsafe, .f32_to_u128_trunc, .f32_to_u128_try_unsafe, .f32_to_f64, .f64_to_i8_trunc, .f64_to_i8_try_unsafe, .f64_to_i16_trunc, .f64_to_i16_try_unsafe, .f64_to_i32_trunc, .f64_to_i32_try_unsafe, .f64_to_i64_trunc, .f64_to_i64_try_unsafe, .f64_to_i128_trunc, .f64_to_i128_try_unsafe, .f64_to_u8_trunc, .f64_to_u8_try_unsafe, .f64_to_u16_trunc, .f64_to_u16_try_unsafe, .f64_to_u32_trunc, .f64_to_u32_try_unsafe, .f64_to_u64_trunc, .f64_to_u64_try_unsafe, .f64_to_u128_trunc, .f64_to_u128_try_unsafe, .f64_to_f32_wrap, .f64_to_f32_try_unsafe, .dec_to_i8_trunc, .dec_to_i8_try_unsafe, .dec_to_i16_trunc, .dec_to_i16_try_unsafe, .dec_to_i32_trunc, .dec_to_i32_try_unsafe, .dec_to_i64_trunc, .dec_to_i64_try_unsafe, .dec_to_i128_trunc, .dec_to_u8_trunc, .dec_to_u8_try_unsafe, .dec_to_u16_trunc, .dec_to_u16_try_unsafe, .dec_to_u32_trunc, .dec_to_u32_try_unsafe, .dec_to_u64_trunc, .dec_to_u64_try_unsafe, .dec_to_u128_trunc, .dec_to_u128_try_unsafe, .dec_to_f32_wrap, .dec_to_f32_try_unsafe, .dec_to_f64, .box_prepare_update, .erased_capture_load, .ptr_alloca, .box_alloc_zeroed, .ptr_store, .ptr_load, .ptr_cast, .compare, .crash => {},
        }
        // Every Builtin low-level whose operands or result can mention a type
        // variable is a list or box operation; every other one is declared
        // at closed types (`BuiltinLowLevel`).
        if (!std.mem.startsWith(u8, @tagName(op), "list_")) {
            invariantFmt("low-level {s} over values of open types, which Builtin declares only at closed types", .{@tagName(op)});
        }
        // The element type of the operation's list: its first List operand's,
        // else its List result's.
        const elem = for (arg_types) |arg_ty| {
            if (store.get(arg_ty) == .list) break store.get(arg_ty).list;
        } else switch (store.get(result_ty)) {
            .list => |elem| elem,
            .primitive, .empty_record, .record, .tuple, .tag_union, .box, .nominal, .function, .variable => invariantFmt("list low-level {s} has no List operand or result", .{@tagName(op)}),
        };
        const with_desc = try std.mem.concat(self.allocator, LocalId, &.{ args, &.{try self.descOf(elem)} });
        defer self.allocator.free(with_desc);
        const target = try self.localOf(result_ty, .frame);
        try self.emitLinear(.{ .assign_low_level = .{
            .target = target,
            .op = op,
            .rc_effect = try self.store.addRcEffect(op.rcEffect()),
            .extra = try self.store.addStmtRow(LIR.LowLevelExtra, .{ .dyn_elem_desc = true }),
            .args = try self.store.addLocalSpan(with_desc),
            .next = undefined,
        } });
        try self.pushValue(target);
    }

    /// The branch of a match on `cond` that a layout-resolved low-level's
    /// statically known answer selects (as `.lss` folds it): `cond` calls a
    /// Builtin wrapper of `list_map_can_reuse` with local arguments, every
    /// branch tests an integer literal or nothing, and the answer is the
    /// same on every pointer width.
    fn layoutResolvedBranch(self: *Builder, cond: ExprId, branches: []const CIR.Expr.Match.Branch.Idx) Allocator.Error!?ExprId {
        const call = switch (self.bodies().exprData(cond)) {
            .call => |call| call,
            .numeral, .str_segment, .str, .bytes_literal, .lookup_local, .lookup_external, .lookup_required, .list, .empty_list, .tuple, .match_, .if_, .record, .empty_record, .block, .tag, .nominal, .zero_argument_tag, .closure, .lambda, .binop, .unary_minus, .field_access, .dispatch_call, .interpolation, .structural_eq, .structural_hash, .method_eq, .type_dispatch_call, .tuple_access, .runtime_error, .crash, .dbg, .expect_err, .expect, .ellipsis, .anno_only, .break_, .return_, .for_, .hosted_lambda, .run_low_level => return null,
        };
        const resolved = switch (self.bodies().exprData(call.func)) {
            .lookup_external, .lookup_local => self.bodies().resolvedRef(call.func) orelse return null,
            .numeral, .str_segment, .str, .bytes_literal, .lookup_required, .list, .empty_list, .tuple, .match_, .if_, .call, .record, .empty_record, .block, .tag, .nominal, .zero_argument_tag, .closure, .lambda, .binop, .unary_minus, .field_access, .dispatch_call, .interpolation, .structural_eq, .structural_hash, .method_eq, .type_dispatch_call, .tuple_access, .runtime_error, .crash, .dbg, .expect_err, .expect, .ellipsis, .anno_only, .break_, .return_, .for_, .hosted_lambda, .run_low_level => return null,
        };
        const use = switch (self.resolvedRef(resolved).ref) {
            .top_level_proc, .imported_proc => |use| use,
            .local_param, .local_value, .local_mutable_version, .pattern_binder, .local_proc, .selected_hoisted_const, .top_level_const, .imported_const, .hosted_proc, .platform_required_declaration, .platform_required_checked_error, .platform_required_const, .platform_required_proc, .promoted_top_level_proc => return null,
        };
        const binding = self.program.procedureBinding(use.binding);
        const template_ref = switch (binding.body) {
            .direct_template => |direct| Program.directTemplate(direct.template),
            .callable_eval_template, .checked_error => return null,
        };
        const owner = self.program.moduleByKey(.{ .bytes = template_ref.artifact.bytes });
        const op = (owner.view.checked_procedure_templates.templates.items[@backingInt(template_ref.template)].provided_low_level orelse return null).layoutResolved() orelse return null;
        for (call.args) |arg| switch (self.bodies().exprData(arg)) {
            .lookup_local => {},
            .numeral, .str_segment, .str, .bytes_literal, .lookup_external, .lookup_required, .list, .empty_list, .tuple, .match_, .if_, .call, .record, .empty_record, .block, .tag, .nominal, .zero_argument_tag, .closure, .lambda, .binop, .unary_minus, .field_access, .dispatch_call, .interpolation, .structural_eq, .structural_hash, .method_eq, .type_dispatch_call, .tuple_access, .runtime_error, .crash, .dbg, .expect_err, .expect, .ellipsis, .anno_only, .break_, .return_, .for_, .hosted_lambda, .run_low_level => return null,
        };
        for (branches) |branch_idx| {
            const branch = self.bodies().matchBranch(branch_idx);
            if (branch.guard != null) return null;
            for (branch.patterns) |pattern| switch (self.bodies().patternData(self.bodies().matchBranchPattern(pattern).pattern)) {
                .underscore => {},
                .numeral_literal => if (self.bodies().literalPatternIsCustom(self.bodies().matchBranchPattern(pattern).pattern)) return null,
                .assign, .as, .applied_tag, .nominal, .record_destructure, .list, .tuple, .str_literal, .str_interpolation, .runtime_error => return null,
            };
        }
        const arg_types = try self.allocator.alloc(TypeId, call.args.len);
        defer self.allocator.free(arg_types);
        for (call.args, arg_types) |arg, *ty| ty.* = try self.valueType(arg);
        const answer: u8 = switch (op) {
            .list_map_can_reuse => blk: {
                const decision = try self.listMapInterchangeable(arg_types);
                if (decision.open) return null;
                break :blk if (!decision.widths.get(.u32) and !decision.widths.get(.u64)) 0 else return null;
            },
            // Its answer is computed where it runs.
            .list_item_size_at_most => return null,
        };
        for (branches) |branch_idx| {
            const branch = self.bodies().matchBranch(branch_idx);
            for (branch.patterns) |pattern| switch (self.bodies().patternData(self.bodies().matchBranchPattern(pattern).pattern)) {
                .underscore => return branch.value,
                .numeral_literal => {
                    const literal = self.bodies().numeralLiteral(@backingInt(self.bodies().matchBranchPattern(pattern).pattern));
                    const bits = checked.exact_numeral.intBits(self.view.view.module_env.exactNumeral(literal), .u8) orelse continue;
                    const value: i128 = switch (bits) {
                        .i128 => |value| value,
                        .u128 => |value| @bitCast(value),
                    };
                    if (value == answer) return branch.value;
                },
                .assign, .as, .applied_tag, .nominal, .record_destructure, .list, .tuple, .str_literal, .str_interpolation, .runtime_error => unreachable, // only literal and wildcard branches reach here
            };
        }
        return null;
    }

    /// Per pointer width, whether a `List.map` over a list of `arg_types[0]`
    /// with the transform `arg_types[1]` may reuse its input's allocation:
    /// never when the optimization is disabled or an element type is open
    /// (`open`: no committed layout to compare where this body is lowered),
    /// otherwise when the element layouts are interchangeable on that width.
    fn listMapInterchangeable(self: *Builder, arg_types: []const TypeId) Allocator.Error!struct { widths: layout.WidthValues(bool), output_elem: ?layout.Idx, open: bool = false } {
        const none = layout.WidthValues(bool).both(false, false);
        if (!self.program.list_in_place_map) return .{ .widths = none, .output_elem = null };
        const store = &self.program.store;
        const in_elem = switch (store.get(try self.structuralType(arg_types[0]))) {
            .list => |elem| elem,
            .primitive, .empty_record, .record, .tuple, .tag_union, .box, .nominal, .function, .variable => invariant("list_map_can_reuse's first operand is not a List"),
        };
        const out_elem = switch (store.get(arg_types[1])) {
            .function => |function| function.ret,
            .primitive, .empty_record, .record, .tuple, .tag_union, .list, .box, .nominal, .variable => invariant("list_map_can_reuse's transform is not a function"),
        };
        if (!try self.closed(in_elem) or !try self.closed(out_elem)) return .{ .widths = none, .output_elem = null, .open = true };
        const layouts = &self.program.result.layouts;
        const in_idx = layouts.runtimeRepresentationLayoutIdx(try self.program.layouts.layoutOf(in_elem));
        const out_idx = layouts.runtimeRepresentationLayoutIdx(try self.program.layouts.layoutOf(out_elem));
        return .{ .widths = layout.WidthValues(bool).both(
            layouts.listItemsInterchangeableAt(in_idx, out_idx, .u32),
            layouts.listItemsInterchangeableAt(in_idx, out_idx, .u64),
        ), .output_elem = out_idx };
    }

    /// Whether `List.map` may reuse its input's allocation: the input and
    /// output element layouts must be interchangeable in one allocation (the
    /// same rule `.lss` applies), per pointer width, before the runtime
    /// uniqueness check decides. Elements of open types have no committed
    /// layout to compare, so their lists are never reused.
    pub fn listMapCanReuse(self: *Builder, args: []const LocalId, arg_types: []const TypeId, result_ty: TypeId) Allocator.Error!LocalId {
        if (args.len != 2) invariant("list_map_can_reuse takes a list and a transform");
        const target = try self.localOf(result_ty, .frame);
        const decision = try self.listMapInterchangeable(arg_types);
        if (!decision.widths.get(.u32) and !decision.widths.get(.u64)) {
            try self.emitLinear(.{ .assign_literal = .{ .target = target, .value = .{ .i64_literal = .{ .value = 0, .layout_idx = .u8 } }, .next = undefined } });
            return target;
        }
        try self.emitLinear(.{ .assign_low_level = .{
            .target = target,
            .op = .list_map_can_reuse,
            .rc_effect = try self.store.addRcEffect(LIR.LowLevel.list_map_can_reuse.rcEffect()),
            .extra = try self.store.addStmtRow(LIR.LowLevelExtra, .{
                .interchangeable = decision.widths,
                .map_output_elem = decision.output_elem.?,
            }),
            .args = try self.store.addLocalSpan(args),
            .next = undefined,
        } });
        return target;
    }

    /// A function value: an erased callable of the lambda's worker, whose
    /// captures hold the captured values and this worker's descriptors.
    pub fn lowerClosure(self: *Builder, expr_id: ExprId, lambda_expr: ExprId, captures: []const checked.CheckedCapture) Allocator.Error!void {
        // rejectCapturingMethods removes methods that transitively capture
        // invocation-specific values before the registry records a target.
        // Only that checked proof authorizes immutable capture edges here.
        if (try self.program.localMethodBinding(self.module, lambda_expr)) |method| {
            const closure = try self.localProcClosureWithCaptures(method, true);
            return try self.pushValue(try self.materialize(.{ .recipe = try self.immortalRecipe(closure) }));
        }
        // A generalized local procedure's type is converted with its own
        // scheme's variables in scope.
        const own: ?lower.LocalScheme = if (try self.program.localScope(self.module, lambda_expr)) |scope_id|
            try self.localScheme(scope_id)
        else
            null;
        const fn_ty = if (own) |scheme| blk: {
            if (scheme.isEmpty()) break :blk try self.exprType(expr_id);
            const vars = try std.mem.concat(self.allocator, checked.CheckedTypeId, &.{ self.env.vars, scheme.scheme_vars });
            defer self.allocator.free(vars);
            const bound = try self.allocator.alloc(TypeId, vars.len);
            defer self.allocator.free(bound);
            for (bound, 0..) |*slot, index| slot.* = try self.program.store.variable(@intCast(index));
            break :blk try self.program.converter().convert(self.module, self.bodies().exprType(expr_id), .{ .module = self.module, .vars = vars, .bound = bound });
        } else try self.exprType(expr_id);
        const generalized: ?lower.LocalScheme = if (own) |scheme| (if (scheme.isEmpty()) null else scheme) else null;
        const capture_types = try self.allocator.alloc(TypeId, captures.len);
        defer self.allocator.free(capture_types);
        const values = try self.allocator.alloc(LocalId, captures.len + self.var_descs.len + self.dict_locals.len);
        defer self.allocator.free(values);
        for (captures, capture_types, 0..) |capture, *capture_ty, index| {
            capture_ty.* = try self.capturedType(capture.pattern, try self.patternType(capture.pattern));
            values[index] = self.capturedValue(capture.pattern) orelse invariant("a closure captured a binder this body never bound");
        }
        try self.declared_captures.put(self.allocator, lambda_expr, try self.program.arena.allocator().dupe(LocalId, values[0..captures.len]));
        const callee = try self.program.lambdaWorker(self.worker_id, lambda_expr, captures, capture_types, fn_ty, generalized);
        // A generalized lambda that is not a local procedure is instantiated
        // where it is constructed: its function value captures its own
        // scheme's descriptors and dictionaries after the enclosing scope's.
        const enclosing_dicts = try self.dictLiteralInputWords(0, 0);
        var descs: []const Word = try self.descInputWords(0, self.var_descs.len);
        var dicts: []const Word = enclosing_dicts;
        var own_descs: std.ArrayList(Word) = .empty;
        defer own_descs.deinit(self.allocator);
        var own_dicts: std.ArrayList(Word) = .empty;
        defer own_dicts.deinit(self.allocator);
        if (generalized) |scheme| if (!scheme.per_call) {
            // A variable the construction's checked instantiation fixes takes
            // that type; one it fixes none for (no use relates the value's
            // variables, or the requirement it carried was retired, leaving
            // an erroneous type) seals as a variable no use relates.
            const subst = self.siteSubstitution(expr_id);
            if (subst.len != 0 and subst.len != scheme.scheme_vars.len) invariant("a generalized lambda's construction substitution differs from its scheme");
            const evidence = self.view.view.static_dispatch_plans.siteEvidence(expr_id) orelse &.{};
            try own_descs.appendSlice(self.allocator, descs);
            const own_types = try self.allocator.alloc(TypeId, scheme.scheme_vars.len);
            defer self.allocator.free(own_types);
            for (own_types, scheme.scheme_vars, 0..) |*own_ty, variable, index| {
                own_ty.* = if (index < subst.len and self.view.types().payload(subst[index]) != .err)
                    try self.convert(subst[index])
                else
                    try self.program.converter().sealedScheme(self.module, variable);
            }
            try self.resolveOwnKinds(self.bodies().exprType(expr_id), scheme, own_types);
            for (own_types) |own_ty| try own_descs.append(self.allocator, try self.descWord(own_ty));
            const scope_id = (try self.program.localScope(self.module, lambda_expr)).?;
            const context = try self.scopeParamContext(scope_id, scheme.scheme_vars, own_types);
            for (scheme.dictionaries) |param_index| {
                if (param_index >= evidence.len) invariant("a generalized lambda's construction evidence does not cover its dictionaries");
                try own_dicts.append(self.allocator, try self.paramDictionary(evidence, param_index, context));
            }
            try own_dicts.appendSlice(self.allocator, enclosing_dicts[0..self.dict_locals.len]);
            // Its own scheme's literal arguments, from the construction's
            // edge, then the enclosing scope's. A construction with no checked
            // instantiation has no edge: no use instantiated the lambda, so
            // nothing calls it, and like a `from_scheme` dictionary each
            // argument is a crash nothing reads.
            const literal_args = self.view.view.checked_procedure_templates.literalEdgeArgs(.{ .use = expr_id });
            if (literal_args.len == scheme.literal_requirements.len) {
                for (literal_args) |source| try own_dicts.append(self.allocator, try self.literalArg(source));
            } else {
                if (subst.len != 0) invariant("a generalized lambda's instantiated construction has no literal arguments for its scheme's literal_requirements");
                for (scheme.literal_requirements) |_| try own_dicts.append(self.allocator, try self.unreadLiteralArg());
            }
            try own_dicts.appendSlice(self.allocator, enclosing_dicts[self.dict_locals.len..]);
            descs = own_descs.items;
            dicts = own_dicts.items;
        };
        // A procedure whose calls pass its table captures none.
        const inputs: []const Word = if (self.program.perCall(callee)) &.{} else try std.mem.concat(self.program.arena.allocator(), Word, &.{ descs, dicts });
        if (try self.allClosed(capture_types)) {
            try self.pushValue(try self.packClosure(callee, values[0..captures.len], capture_types, inputs));
        } else {
            try self.pushValue(try self.packDynamicClosure(callee, values[0..captures.len], capture_types, inputs));
        }
    }

    /// The descriptor of a dynamic capture record (design.md "Function
    /// Values"): a declared-order record of its own descriptor word, its
    /// allocation's capture capacity word, the descriptor and dictionary
    /// words, then the captured values.
    pub fn dynamicCaptureDesc(self: *Builder, capture_types: []const TypeId, word_count: usize) Allocator.Error!LocalId {
        return try self.captureRecordDesc(@intCast(capture_record_leading_words + word_count), try self.descWords(capture_types));
    }

    /// A capture record's purpose is explicit, including when its fields
    /// happen to match an ordinary argument buffer's fields.
    fn captureRecordDesc(self: *Builder, leading: u32, elems: []const Word) Allocator.Error!LocalId {
        return try self.materialize(.{ .recipe = try self.recipes.intern(.{ .args_desc = .{ .leading = leading, .elems = elems, .capture_record = true } }) });
    }

    /// The words before a dynamic capture record's descriptor and
    /// dictionary words: its descriptor and its capture capacity.
    pub const capture_record_leading_words = 2;

    /// A function value whose captures include values of open types: the
    /// capture record is built in the dynamic frame region and moved into
    /// the callable. `capture_types` are the captured values' types in this
    /// body's scope.
    pub fn packDynamicClosure(self: *Builder, callee: WorkerId, captured: []const LocalId, capture_types: []const TypeId, inputs: []const Word) Allocator.Error!LocalId {
        var table_word: [1]LocalId = undefined;
        const words: []const LocalId = if (try self.capturedTable(callee, inputs)) |table| blk: {
            table_word[0] = table;
            break :blk &table_word;
        } else &.{};
        const record_desc = try self.dynamicCaptureDesc(capture_types, words.len);
        const record = try self.addLocal(.{ .layout_idx = try self.program.result.layouts.dynamicLayout(), .dyn = .{ .desc = record_desc, .storage = .frame } });
        // The capacity word is the runtime's to write when it packs the
        // callable; the record carries its descriptor there until then.
        const fields = try std.mem.concat(self.allocator, LocalId, &.{ &.{ record_desc, record_desc }, words, captured });
        defer self.allocator.free(fields);
        try self.emitDyn(record, .{ .record = .{ .desc = record_desc, .fields = try self.store.addLocalSpan(fields) } });
        const target = try self.closedLocal(try self.erasedLayout());
        try self.emitDyn(target, .{ .closure = .{ .proc = self.program.worker(callee).proc, .capture = record, .reuse = self.takeReuse(.dynamic) } });
        return target;
    }

    /// The type table an erased callable of `callee` captures at `inputs`:
    /// none for a worker without inputs, or one whose calls pass its table
    /// (`Program.perCall`), which takes no inputs here.
    pub fn capturedTable(self: *Builder, callee: WorkerId, inputs: []const Word) Allocator.Error!?LocalId {
        if (self.program.perCall(callee)) {
            if (inputs.len != 0) invariant("a procedure whose calls pass its table was packed with inputs");
            return null;
        }
        return try self.tableLocal(callee, inputs);
    }

    /// The erased callable of worker `callee` capturing `values` (of
    /// `capture_types`), then its type table at `inputs`.
    pub fn packClosure(self: *Builder, callee: WorkerId, captured: []const LocalId, capture_types: []const TypeId, inputs: []const Word) Allocator.Error!LocalId {
        var table_word: [1]LocalId = undefined;
        const words: []const LocalId = if (try self.capturedTable(callee, inputs)) |table| blk: {
            table_word[0] = table;
            break :blk &table_word;
        } else &.{};
        const values = try std.mem.concat(self.allocator, LocalId, &.{ captured, words });
        defer self.allocator.free(values);
        // Producer-authored constant slots prove that this environment has
        // no invocation-specific data, including the capture-free case.
        const all_static = for (values) |value| {
            if (!self.constant_slots.contains(value)) break false;
        } else true;
        if (all_static) return try self.staticClosure(callee, captured, capture_types, inputs, false, null);
        const target = try self.closedLocal(try self.erasedLayout());
        var capture: ?LocalId = null;
        var capture_layout: ?layout.Idx = null;
        var on_drop: LIR.ErasedCallableOnDrop = .none;
        if (values.len != 0) {
            const record_layout = try self.captureLayout(capture_types, words.len);
            const record = try self.closedLocal(record_layout);
            try self.emitLinear(.{ .assign_struct = .{ .target = record, .fields = try self.store.addLocalSpan(values), .next = undefined } });
            capture = record;
            capture_layout = record_layout;
            const layouts = &self.program.result.layouts;
            if (layouts.layoutContainsRefcounted(layouts.getLayout(record_layout))) {
                on_drop = .{ .rc_helper = .{ .op = .host_drop, .layout_idx = record_layout } };
            }
        }
        // A transferred allocation of this worker's own closed capture
        // layout is repacked in place when unique.
        const reuse: ?LocalId = if (capture_layout != null and self.reuse_kind == .static and self.reuse_kind.static == capture_layout.?) self.takeReuse(.static) else null;
        try self.emitLinear(.{ .assign_packed_erased_fn = .{
            .target = target,
            .data = try self.store.addStmtRow(LIR.PackedErasedFnData, .{
                .proc = self.program.worker(callee).proc,
                .capture = capture,
                .capture_layout = capture_layout,
                .on_drop = on_drop,
                .on_copy = on_drop.copyCallback(),
                .frozen_capture_layout = switch (on_drop) {
                    .none => null,
                    .rc_helper => |helper| try self.program.result.layouts.putPrefixedCaptureLayout(helper.layout_idx, self.program.result.erased_capture_prefix),
                },
                .reuse = reuse,
            }),
            .next = undefined,
        } });
        return target;
    }

    /// Encode a completed callable and its captures as immutable target data.
    /// The layout follows the worker's declared ABI; no closure packing executes
    /// when the constant is read.
    fn staticClosure(self: *Builder, worker: WorkerId, captured: []const LocalId, capture_types: []const TypeId, inputs: []const Word, dynamic: bool, capture_descs: ?[]const Word) Allocator.Error!LocalId {
        const per_call = self.program.perCall(worker);
        if (per_call and inputs.len != 0) invariant("a completed per-call worker captures unexpected inputs");
        const table = if (per_call or (inputs.len == 0 and self.program.inputCount(worker) == 0)) null else table: {
            const word = try self.tableWord(worker, inputs);
            const static = switch (word) {
                .static => |value| value,
                .recipe => |id| value: {
                    if (try self.readsInputs(id)) invariant("a completed function's type table depends on runtime inputs");
                    break :value try self.foldWord(id);
                },
            };
            break :table try self.staticRead(try self.program.static_descs.wordSlot(static));
        };
        const table_count: usize = @intFromBool(table != null);
        var fields: std.ArrayList(LocalId) = .empty;
        defer fields.deinit(self.allocator);
        var record_layout: layout.Idx = .zst;
        if (dynamic) {
            const count = capture_record_leading_words + table_count + captured.len;
            const layouts = try self.allocator.alloc(layout.StructField, count);
            defer self.allocator.free(layouts);
            const words = capture_record_leading_words + table_count;
            for (layouts, 0..) |*field, i| {
                field.* = .{ .index = @intCast(i), .layout = if (i < words) .opaque_ptr else try self.program.layouts.layoutOf(capture_types[i - words]) };
            }
            record_layout = try self.program.result.layouts.putNominalStructFields(layouts);
            const record_desc = try self.captureRecordDesc(@intCast(words), capture_descs orelse try self.descWords(capture_types));
            const capacity = try self.staticPartsLocal(.opaque_ptr, .{ .word = self.program.result.layouts.layoutSize(self.program.result.layouts.getLayout(record_layout)) }, &.{});
            try fields.appendSlice(self.allocator, &.{ record_desc, capacity });
            if (table) |local| try fields.append(self.allocator, local);
            try fields.appendSlice(self.allocator, captured);
        } else {
            record_layout = try self.captureLayout(capture_types, table_count);
            try fields.appendSlice(self.allocator, captured);
            if (table) |local| try fields.append(self.allocator, local);
        }
        const proc = self.program.worker(worker).proc;
        if (fields.items.len == 0) return self.staticPartsLocal(try self.erasedLayout(), .{ .callable = .{ .proc = proc } }, &.{});
        const record = try self.staticPartsLocal(record_layout, .product, fields.items);
        const layouts = &self.program.result.layouts;
        const on_drop: LIR.ErasedCallableOnDrop = if (layouts.layoutContainsRefcounted(layouts.getLayout(record_layout)))
            .{ .rc_helper = .{ .op = .host_drop, .layout_idx = try layouts.putPrefixedCaptureLayout(record_layout, self.program.result.erased_capture_prefix) } }
        else
            .none;
        return self.staticPartsLocal(try self.erasedLayout(), .{ .callable = .{ .proc = proc, .on_drop = on_drop } }, &.{record});
    }

    /// Pop a function value's arguments and the function value, and call it
    /// through the uniform erased-callable ABI.
    pub fn finishErasedCall(self: *Builder, fn_ty: TypeId, arg_count: u32, result_ty: TypeId) Allocator.Error!void {
        const args = try self.popValues(arg_count);
        defer self.allocator.free(args);
        const closure = self.popValue();
        try self.pushValue(try self.emitErasedCall(closure, fn_ty, args, result_ty));
    }

    /// Call the erased callable `closure` of type `fn_ty` with `args`.
    pub fn emitErasedCall(self: *Builder, closure: LocalId, fn_ty: TypeId, args: []const LocalId, result_ty: TypeId) Allocator.Error!LocalId {
        const params = switch (self.program.store.get(fn_ty)) {
            .function => |function| self.program.store.span(function.args),
            .primitive, .empty_record, .record, .tuple, .tag_union, .list, .box, .nominal, .variable => invariant("a called value's type is not a function type"),
        };
        var record: ?LocalId = null;
        if (args.len == 1 and self.canPassUnaryArgument(closure, args[0])) {
            record = args[0];
        } else if (args.len != 0) {
            const params_copy = try self.allocator.dupe(TypeId, params);
            defer self.allocator.free(params_copy);
            const local = try self.argsRecordLocal(params_copy, .frame);
            if (self.store.localDyn(local)) |dyn| {
                try self.emitDyn(local, .{ .record = .{ .desc = dyn.desc, .fields = try self.store.addLocalSpan(args) } });
            } else {
                try self.emitLinear(.{ .assign_struct = .{ .target = local, .fields = try self.store.addLocalSpan(args), .next = undefined } });
            }
            record = local;
        }
        // The callee writes its result as the called type's result; a use
        // whose type rows it differently reads it re-tagged (design.md
        // "Row Joins").
        const ret = switch (self.program.store.get(fn_ty)) {
            .function => |function| function.ret,
            .primitive, .empty_record, .record, .tuple, .tag_union, .list, .box, .nominal, .variable => unreachable, // checked above
        };
        if (ret == result_ty) return try self.callErased(closure, record, result_ty);
        return try self.adaptValue(try self.callErased(closure, record, ret), ret, result_ty);
    }

    /// One complete argument has exactly the argument block's size and
    /// alignment, at offset zero. Its existing ownership unit is consumed by
    /// the call. A view must still be copied: deferred calls release the
    /// caller's owners before copying their argument bytes, potentially
    /// freeing the allocation containing a view. Frame and caller-provided
    /// storage remain valid until the invocation returns.
    fn canPassUnaryArgument(self: *Builder, closure: LocalId, arg: LocalId) bool {
        if (closure == arg) return false;
        return if (self.store.localDyn(arg)) |dyn| dyn.storage != .view else true;
    }

    /// Call the erased callable `closure` with the complete argument block
    /// `record` (possibly a single value), for a result of `result_ty`.
    pub fn callErased(self: *Builder, closure: LocalId, record: ?LocalId, result_ty: TypeId) Allocator.Error!LocalId {
        const target = try self.localOf(result_ty, .frame);
        // A call whose result holds one erased-callable slot transfers the
        // callable's allocation, which the callee repacks when it packs a
        // callable that fits it (design.md "Destination-Passing Results
        // and Allocation Reuse").
        const owned = try self.program.converter().erasedResultSlots(result_ty) == .single;
        try self.emitDyn(target, .{ .call_erased = .{ .closure = closure, .args = record, .reuse_closure = owned, .reuse_source = if (owned) closure else null } });
        return target;
    }

    pub fn finishEq(self: *Builder, negated: bool, operand_ty: TypeId) Allocator.Error!void {
        const rhs = self.popValue();
        const lhs = self.popValue();
        const result = try self.closedLocal(.bool);
        try self.emitDyn(result, .{ .eq = .{ .lhs = lhs, .rhs = rhs, .desc = try self.descOf(operand_ty) } });
        if (!negated) return try self.pushValue(result);
        const flipped = try self.closedLocal(.bool);
        try self.emitLowLevel(flipped, .bool_not, &.{result});
        try self.pushValue(flipped);
    }

    /// `a and b` evaluates `b` only when `a` is true; `a or b` only when
    /// `a` is false.
    pub fn lowerShortCircuit(self: *Builder, is_and: bool, lhs: ExprId, rhs: ExprId) Allocator.Error!void {
        const result = try self.closedLocal(.bool);
        const join = self.freshJoin();
        const after = try self.declareMergeJoin(join, result, null);
        try self.push(.{ .push_value = result });
        try self.push(.{ .resume_at = after });
        try self.push(.{ .short_circuit = .{ .is_and = is_and, .rhs = rhs, .join = join } });
        try self.push(.{ .expr = lhs });
    }

    pub fn shortCircuit(self: *Builder, is_and: bool, rhs: ExprId, join: LIR.JoinPointId) Allocator.Error!void {
        const holes = try self.emitBoolSwitch(self.popValue());
        // The side that decides the result alone jumps with the constant.
        self.hole = if (is_and) holes.otherwise else holes.then;
        try self.emitJump(join, try self.boolValue(!is_and));
        try self.push(.{ .jump = join });
        try self.push(.{ .expr = rhs });
        self.hole = if (is_and) holes.then else holes.otherwise;
    }

    // -- Aggregates --

    pub fn finishRecord(self: *Builder, expr_id: ExprId) Allocator.Error!void {
        const record: @FieldType(bodies_mod.ExprData, "record") = switch (self.bodies().exprData(expr_id)) {
            .record => |record| record,
            .empty_record => .{ .fields = &.{}, .unsets = &.{}, .ext = null },
            .numeral, .str_segment, .str, .bytes_literal, .lookup_local, .lookup_external, .lookup_required, .list, .empty_list, .tuple, .match_, .if_, .call, .block, .tag, .nominal, .zero_argument_tag, .closure, .lambda, .binop, .unary_minus, .field_access, .dispatch_call, .interpolation, .structural_eq, .structural_hash, .method_eq, .type_dispatch_call, .tuple_access, .runtime_error, .crash, .dbg, .expect_err, .expect, .ellipsis, .anno_only, .break_, .return_, .for_, .hosted_lambda, .run_low_level => invariant("a record construction is not a record expression"),
        };
        const ty = try self.exprType(expr_id);
        const values = try self.popValues(@intCast(record.fields.len));
        defer self.allocator.free(values);
        const update_base: ?LocalId = if (record.ext != null) self.popValue() else null;
        const record_ty = switch (self.program.store.get(try self.structuralType(ty))) {
            .record => |value| value,
            .empty_record => return try self.pushValue(try self.unitValue()),
            .primitive, .tuple, .tag_union, .list, .box, .nominal, .function, .variable => invariant("a record expression's type is not a record"),
        };
        const fields = try self.allocator.dupe(types.Field, self.program.store.fieldSpan(record_ty.fields));
        defer self.allocator.free(fields);
        // Values arrive in source order; store them by checked field index. An
        // optional field's slot holds `#Present(value)`, or `#Missing` when
        // the literal omits it (design.md "Field Kinds").
        const ordered = try self.allocator.alloc(?LocalId, fields.len);
        defer self.allocator.free(ordered);
        @memset(ordered, null);
        const name_store = self.view.nameStore();
        for (record.fields, values) |field_id, value| {
            const field = self.bodies().recordField(field_id);
            const name = try self.program.store.internName(name_store.recordFieldLabelText(field.label));
            const position = for (fields, 0..) |candidate, index| {
                if (candidate.name == name) break index;
            } else invariant("a record expression's field is absent from its type");
            ordered[position] = switch (fields[position].kind) {
                .inline_slot => value,
                .presence_slot => try self.buildTag(try self.program.layouts.presenceSlot(fields[position].ty), try self.program.store.internName(@import("layouts.zig").present_label), &.{value}),
                .variable_slot => blk: {
                    const storage = try self.program.layouts.fieldStorage(fields[position]);
                    const slot_local = try self.localOf(storage, .frame);
                    try self.emitAdapt(slot_local, value, fields[position].ty, storage);
                    break :blk slot_local;
                },
            };
        }
        // Unset fields (`name: _`) are missing.
        for (record.unsets) |unset| {
            const label = self.bodies().unsetField(unset);
            const name = try self.program.store.internName(name_store.recordFieldLabelText(label));
            for (fields, ordered) |field, *slot| {
                if (field.name != name) continue;
                if (field.kind != .presence_slot) invariant("an unset field is not optional");
                slot.* = try self.buildTag(try self.program.layouts.presenceSlot(field.ty), try self.program.store.internName(@import("layouts.zig").missing_label), &.{});
            }
        }
        // An update copies the fields it does not mention from its base.
        if (update_base) |base_value| {
            // A record of an open row takes the fields its update names and
            // the rest of its base's, which only its descriptor lists.
            if (record_ty.ext != null) {
                var names: std.ArrayList(types.NameId) = .empty;
                defer names.deinit(self.allocator);
                var updated: std.ArrayList(LocalId) = .empty;
                defer updated.deinit(self.allocator);
                for (fields, ordered) |field, value| {
                    const local = value orelse continue;
                    try names.append(self.allocator, field.name);
                    try updated.append(self.allocator, local);
                }
                const base_ty = try self.valueType(record.ext.?);
                const plan = try self.materialize(.{ .recipe = try self.recipes.intern(.{ .update_plan = .{
                    .base = try self.descWord(base_ty),
                    .result = try self.descWord(ty),
                    .names = names.items,
                } }) });
                const result = try self.localOf(ty, .frame);
                try self.emitDyn(result, .{ .record_update = .{
                    .base = base_value,
                    .base_desc = try self.descOf(base_ty),
                    .desc = try self.descOf(ty),
                    .plan = plan,
                    .values = try self.store.addLocalSpan(updated.items),
                } });
                return try self.pushValue(result);
            }
            const base_ty = try self.structuralType(try self.valueType(record.ext.?));
            const base_record = switch (self.program.store.get(base_ty)) {
                .record => |value| value,
                .primitive, .empty_record, .tuple, .tag_union, .list, .box, .nominal, .function, .variable => invariant("a record update's base is not a record"),
            };
            const base_fields = try self.allocator.dupe(types.Field, self.program.store.fieldSpan(base_record.fields));
            defer self.allocator.free(base_fields);
            for (fields, ordered) |field, *slot| {
                if (slot.* != null) continue;
                const storage = try self.program.layouts.fieldStorage(field);
                const base_index = for (base_fields, 0..) |candidate, index| {
                    if (candidate.name == field.name) break index;
                } else null;
                if (base_index) |index| {
                    slot.* = try self.readField(base_ty, base_value, @intCast(index), storage);
                } else {
                    if (base_record.ext == null) invariant("a record update's field is absent from its base");
                    const local = try self.localOf(storage, .view);
                    try self.emitDyn(local, .{ .field_addr = .{ .base = base_value, .desc = try self.descOf(base_ty), .field_index = try self.fieldIndex(base_ty, field.name) } });
                    slot.* = local;
                }
            }
        }
        const slots = try self.allocator.alloc(LocalId, fields.len);
        defer self.allocator.free(slots);
        for (fields, ordered, slots) |field, value, *slot| {
            slot.* = value orelse switch (field.kind) {
                .presence_slot => try self.buildTag(try self.program.layouts.presenceSlot(field.ty), try self.program.store.internName(@import("layouts.zig").missing_label), &.{}),
                .inline_slot => try self.defaultValue((try self.program.omittedDefault(.{ .module = self.module, .expr = expr_id, .name = field.name })) orelse
                    invariant("a record construction omits a field it has no default for"), field.ty),
                .variable_slot => invariant("a record construction omits a field of undetermined kind"),
            };
        }
        try self.pushValue(try self.buildStruct(ty, slots));
    }

    /// A record or tuple of `ty` from its fields in checked field order.
    pub fn buildStruct(self: *Builder, ty: TypeId, fields: []const LocalId) Allocator.Error!LocalId {
        const target = try self.localOf(ty, .frame);
        if (try self.closed(ty)) {
            const struct_layout = self.store.getLocal(target).layout_idx;
            const component_types = try self.structComponentTypes(ty);
            defer self.allocator.free(component_types);
            const slots = try self.allocator.alloc(LocalId, fields.len);
            defer self.allocator.free(slots);
            for (fields, slots, 0..) |field, *slot, index| {
                slot.* = if (component_types.len == fields.len and fields.len != 0)
                    try self.intoSlot(field, component_types[index], self.structFieldLayout(struct_layout, @intCast(index)))
                else
                    field;
            }
            try self.emitLinear(.{ .assign_struct = .{ .target = target, .fields = try self.store.addLocalSpan(slots), .next = undefined } });
        } else {
            try self.emitDyn(target, .{ .record = .{ .desc = try self.descOf(ty), .fields = try self.store.addLocalSpan(fields) } });
        }
        return target;
    }

    pub fn finishTuple(self: *Builder, expr_id: ExprId) Allocator.Error!void {
        const items = self.bodies().exprData(expr_id).tuple;
        const ty = try self.exprType(expr_id);
        const values = try self.popValues(@intCast(items.len));
        defer self.allocator.free(values);
        try self.pushValue(try self.buildStruct(ty, values));
    }

    pub fn finishTag(self: *Builder, expr_id: ExprId, at_ty: ?TypeId) Allocator.Error!void {
        const expr = self.bodies().expr(expr_id);
        const label, const arg_count: usize = switch (expr.data) {
            .tag => |tag| .{ tag.name, tag.args.len },
            .zero_argument_tag => |tag| .{ tag.name, 0 },
            .numeral, .str_segment, .str, .bytes_literal, .lookup_local, .lookup_external, .lookup_required, .list, .empty_list, .tuple, .match_, .if_, .call, .record, .empty_record, .block, .nominal, .closure, .lambda, .binop, .unary_minus, .field_access, .dispatch_call, .interpolation, .structural_eq, .structural_hash, .method_eq, .type_dispatch_call, .tuple_access, .runtime_error, .crash, .dbg, .expect_err, .expect, .ellipsis, .anno_only, .break_, .return_, .for_, .hosted_lambda, .run_low_level => unreachable, // only tag expressions finish as tags
        };
        const ty = at_ty orelse try self.exprType(expr_id);
        const values = try self.popValues(@intCast(arg_count));
        defer self.allocator.free(values);
        const name = try self.program.store.internName(self.view.nameStore().tagLabelText(self.bodies().tagLabel(label)));
        try self.pushValue(try self.buildTag(ty, name, values));
    }

    /// The tag `name` of the tag union `ty` (through nominal wrappers) with
    /// `values` as its payloads.
    pub fn buildTag(self: *Builder, ty: TypeId, name: types.NameId, values: []const LocalId) Allocator.Error!LocalId {
        if (!try self.closed(ty)) {
            const target = try self.localOf(ty, .frame);
            const union_ty = try self.structuralType(ty);
            try self.emitDyn(target, .{ .tag = .{ .desc = try self.descOf(union_ty), .discriminant = try self.tagIndex(union_ty, name), .payloads = try self.store.addLocalSpan(values) } });
            return target;
        }
        switch (self.program.store.get(try self.structuralType(ty))) {
            .tag_union => {},
            .primitive => |primitive| {
                if (primitive != .bool) invariant("a tag expression's type is a non-Bool primitive");
                return try self.boolValue(std.mem.eql(u8, self.program.store.nameText(name), "True"));
            },
            .empty_record, .record, .tuple, .list, .box, .nominal, .function, .variable => invariant("a tag expression's type is not a tag union"),
        }
        const selected = try self.selectVariant(try self.structuralType(ty), self.ownTag(try self.structuralType(ty), name, values.len));
        const variant = selected.index;
        const target = try self.localOf(ty, .frame);
        const committed = self.program.result.layouts.getLayout(try self.program.layouts.layoutOf(ty));
        const payload: ?LocalId = if (values.len == 0 or committed.tag != .tag_union) null else blk: {
            const payload_layout = try self.variantPayloadLayout(try self.structuralType(ty), variant);
            if (values.len == 1) break :blk try self.intoSlot(values[0], selected.payloads[0], payload_layout);
            // A payload of several values is a struct at the variant's
            // committed payload layout.
            const slots = try self.allocator.alloc(LocalId, values.len);
            defer self.allocator.free(slots);
            for (values, selected.payloads, slots, 0..) |value, payload_ty, *slot, index| {
                slot.* = try self.intoSlot(value, payload_ty, self.structFieldLayout(payload_layout, @intCast(index)));
            }
            const payload_struct = try self.closedLocal(payload_layout);
            try self.emitLinear(.{ .assign_struct = .{ .target = payload_struct, .fields = try self.store.addLocalSpan(slots), .next = undefined } });
            break :blk payload_struct;
        };
        if (committed.tag != .tag_union) {
            // A single-variant union of zero-sized payloads has no tag storage.
            try self.emitLinear(.{ .assign_struct = .{ .target = target, .fields = LIR.LocalSpan.empty(), .next = undefined } });
        } else {
            try self.emitLinear(.{ .assign_tag = .{ .target = target, .variant_index = @intCast(variant), .discriminant = @intCast(variant), .payload = payload, .next = undefined } });
        }
        return target;
    }

    pub fn finishList(self: *Builder, expr_id: ExprId) Allocator.Error!void {
        const expr = self.bodies().expr(expr_id);
        const count: usize = switch (expr.data) {
            .list => |items| items.len,
            .empty_list => 0,
            .numeral, .str_segment, .str, .bytes_literal, .lookup_local, .lookup_external, .lookup_required, .tuple, .match_, .if_, .call, .record, .empty_record, .block, .tag, .nominal, .zero_argument_tag, .closure, .lambda, .binop, .unary_minus, .field_access, .dispatch_call, .interpolation, .structural_eq, .structural_hash, .method_eq, .type_dispatch_call, .tuple_access, .runtime_error, .crash, .dbg, .expect_err, .expect, .ellipsis, .anno_only, .break_, .return_, .for_, .hosted_lambda, .run_low_level => unreachable, // only list expressions finish as lists
        };
        const ty = try self.exprType(expr_id);
        const values = try self.popValues(@intCast(count));
        defer self.allocator.free(values);
        try self.pushValue(try self.buildList(ty, values));
    }

    /// A list of `ty` holding `values`.
    pub fn buildList(self: *Builder, ty: TypeId, values: []const LocalId) Allocator.Error!LocalId {
        const target = try self.localOf(ty, .frame);
        if (try self.closed(ty)) {
            try self.emitLinear(.{ .assign_list = .{ .target = target, .elems = try self.store.addLocalSpan(values), .next = undefined } });
        } else {
            const elem = switch (self.program.store.get(try self.structuralType(ty))) {
                .list => |elem| elem,
                .primitive, .empty_record, .record, .tuple, .tag_union, .box, .nominal, .function, .variable => invariant("a list's type is not a List"),
            };
            try self.emitDyn(target, .{ .list = .{ .elem_desc = try self.descOf(elem), .elems = try self.store.addLocalSpan(values) } });
        }
        return target;
    }

    // -- Control flow --

    pub fn ifTest(self: *Builder, expr_id: ExprId, index: u32, join: ?LIR.JoinPointId, merge: ?MergeId) Allocator.Error!void {
        const if_ = self.bodies().exprData(expr_id).if_;
        const cond = self.popValue();
        const holes = try self.emitBoolSwitch(cond);
        // Pushed in reverse execution order: the then-branch runs first, then
        // the else side continues at its own hole. Each side starts from the
        // binder values the construct started with.
        if (index + 1 < if_.branches.len) {
            try self.push(.{ .if_test = .{ .expr = expr_id, .index = index + 1, .join = join, .merge = merge } });
            try self.push(.{ .expr = self.bodies().ifBranch(if_.branches[index + 1]).cond });
        } else {
            try self.pushBranchEnd(join);
            try self.push(.{ .expr = if_.final_else });
        }
        if (merge) |m| try self.push(.{ .restore_merge = m });
        try self.push(.{ .resume_at = holes.otherwise });
        try self.pushBranchEnd(join);
        try self.push(.{ .expr = self.bodies().ifBranch(if_.branches[index]).body });
        if (merge) |m| try self.restoreMerge(m);
        self.hole = holes.then;
    }

    /// Where a branch of a construct ends: a jump to its join, or, in tail
    /// position, a return.
    pub fn pushBranchEnd(self: *Builder, join: ?LIR.JoinPointId) Allocator.Error!void {
        try self.push(if (join) |id| .{ .jump = id } else .ret);
    }

    /// What a match reports when no branch matches (`comptimeFailure`).
    fn comptimeMatchFailure(self: *Builder, expr_id: ExprId, match: @FieldType(bodies_mod.ExprData, "match_")) Allocator.Error!?Match.ComptimeFailure {
        if (match.skip_exhaustiveness) return null;
        return try self.comptimeFailure(self.view.view.exhaustiveness_sites.lookupByMatchExpr(expr_id));
    }

    pub fn matchStart(self: *Builder, expr_id: ExprId) Allocator.Error!void {
        const scrutinee = self.popValue();
        const match = self.bodies().exprData(expr_id).match_;
        const scrutinee_ty = try self.valueType(match.cond);
        const tail = self.isTail(expr_id);
        const state = try self.program.arena.allocator().create(Match.State);
        state.* = .{
            .arena = std.heap.ArenaAllocator.init(self.allocator),
            .done = self.freshJoin(),
            .tail = tail,
            .merge = null,
            .remainder = undefined,
            .comptime_failure = try self.comptimeMatchFailure(expr_id, match),
        };
        const merge_binders = try self.bodies().stateMergeBinders(expr_id);
        if (tail) {
            // Each branch returns; nothing reads the construct's own value,
            // and each branch starts from the binder values the construct
            // started with.
            for (match.branches) |branch| try self.tail_exprs.put(self.allocator, self.bodies().matchBranch(branch).value, {});
            state.merge = try self.beginMerge(merge_binders, .tail);
            try self.push(.unreachable_or_unit);
        } else {
            const result = try self.localOf(try self.exprType(expr_id), .frame);
            state.merge = try self.beginMerge(merge_binders, .joined);
            const after = try self.declareMergeJoin(state.done, result, state.merge);
            try self.push(.{ .push_value = result });
            if (state.merge) |m| try self.push(.{ .enter_merge = m });
            try self.push(.{ .resume_at = after });
        }
        state.remainder = try self.currentHole();
        self.hole = null;

        // Every pattern of a branch is a row of the decision tree; rows of
        // one branch share its one lowered body.
        const arena = state.arena.allocator();
        const ctx: Match.Ctx = .{ .b = self, .state = state };
        var rows: std.ArrayList(Match.Compiler.Branch) = .empty;
        var remaps: std.ArrayList(checked.AlternativeBinderRemap) = .empty;
        for (match.branches, 0..) |branch_idx, index| {
            const branch = self.bodies().matchBranch(branch_idx);
            try state.alternatives.put(arena, branch.value, @intCast(branch.patterns.len));
            // A later alternative's binders read the locals of the
            // branch's first pattern's binders of the same names.
            if (branch.patterns.len > 1) {
                const representative = self.bodies().matchBranchPattern(branch.patterns[0]).pattern;
                for (branch.patterns[1..]) |alternative_idx| {
                    const alternative = self.bodies().matchBranchPattern(alternative_idx);
                    if (alternative.degenerate) continue;
                    remaps.clearRetainingCapacity();
                    try self.bodies().appendAlternativeBinderRemaps(arena, representative, alternative.pattern, &remaps);
                    for (remaps.items) |remap| {
                        try state.representatives.put(arena, remap.candidate_binder, remap.representative_binder);
                    }
                }
            }
            for (branch.patterns) |alternative_idx| try rows.append(arena, .{
                .pat = self.bodies().matchBranchPattern(alternative_idx).pattern,
                .guard = branch.guard,
                .body = branch.value,
                .branch_index = @intCast(index),
            });
        }
        try ctx.noteScrutinee(scrutinee, scrutinee_ty);
        const built = try Match.Compiler.build(arena, ctx, scrutinee_ty, rows.items);
        state.emitter = try Match.Compiler.Emitter.begin(arena, ctx, built, scrutinee, state.done);
        try self.push(.{ .match_advance = .{ .state = state, .input = null } });
    }

    /// Continue a match's decision-tree emission: lower the body, guard, or
    /// literal guard it asks for into a fresh hole, or link the finished
    /// tree into the match's place.
    pub fn matchAdvance(self: *Builder, state: *Match.State, input: ?CFStmtId) Allocator.Error!void {
        switch (try state.emitter.advance(input)) {
            .request => |request| switch (request) {
                .body => |requested| {
                    const shared = (state.alternatives.get(requested.body) orelse 1) > 1;
                    if (shared) {
                        if (state.shared_bodies.get(requested.body)) |join| {
                            const jump = try self.store.addCFStmt(.{ .jump = .{ .target = join } }, self.origin(.lowering_glue));
                            return try self.push(.{ .match_advance = .{ .state = state, .input = jump } });
                        }
                    }
                    const body_hole = try self.newHole();
                    var answer = body_hole;
                    if (shared) {
                        // Several patterns' rows share the one lowering:
                        // each jumps to it, its binders bound on every path.
                        const join = self.freshJoin();
                        try state.shared_bodies.put(state.arena.allocator(), requested.body, join);
                        try state.shared_joins.append(state.arena.allocator(), .{ .id = join, .body = body_hole });
                        answer = try self.store.addCFStmt(.{ .jump = .{ .target = join } }, self.origin(.lowering_glue));
                    }
                    try self.push(.{ .match_advance = .{ .state = state, .input = answer } });
                    try self.pushBranchEnd(if (state.tail) null else state.done);
                    try self.push(.{ .expr = requested.body });
                    if (state.merge) |m| try self.push(.{ .restore_merge = m });
                    try self.push(.{ .resume_at = body_hole });
                },
                .guard => |guard| {
                    const guard_hole = try self.newHole();
                    try self.push(.{ .match_advance = .{ .state = state, .input = guard_hole } });
                    try self.push(.{ .match_guard_done = .{ .cond = guard.cond, .next = guard.next } });
                    try self.push(.{ .expr = guard.guard });
                    if (state.merge) |m| try self.push(.{ .restore_merge = m });
                    try self.push(.{ .resume_at = guard_hole });
                },
                .literal_guard => |guard| {
                    // The literal's checked equality reads the matched value
                    // through the pattern itself, its binder.
                    try self.binders.put(self.allocator, guard.pat, guard.value);
                    const guard_hole = try self.newHole();
                    try self.push(.{ .match_advance = .{ .state = state, .input = guard_hole } });
                    try self.push(.{ .match_guard_done = .{ .cond = guard.cond, .next = guard.next } });
                    try self.push(.{ .literal_pattern_guard = guard.pat });
                    try self.push(.{ .resume_at = guard_hole });
                },
                // Checked branches bind nothing beyond their patterns.
                .bindings => |bindings| try self.push(.{ .match_advance = .{ .state = state, .input = bindings.next } }),
            },
            .done => |tree| {
                var head = tree;
                for (state.shared_joins.items) |shared| head = try self.addJoinStmt(shared.id, &.{}, shared.body, head);
                try self.fill(state.remainder, self.store.getCFStmt(head), .lowering_glue);
                state.arena.deinit();
            },
        }
    }

    /// A join statement of this body, registered among its join points.
    pub fn addJoinStmt(self: *Builder, id: LIR.JoinPointId, params: []const LocalId, body: CFStmtId, remainder: CFStmtId) Allocator.Error!CFStmtId {
        const span = try self.store.addLocalSpan(params);
        try self.join_points.append(self.allocator, .{ .id = id, .params = span, .body = body });
        return try self.store.addCFStmt(.{ .join = .{ .id = id, .params = span, .body = body, .remainder = remainder } }, self.origin(.lowering_glue));
    }

    /// Start emitting statements front to back for a caller that gives
    /// their successor afterward (`endBackward`).
    pub fn beginBackward(self: *Builder) Allocator.Error!Backward {
        const saved: Backward = .{ .start = try self.newHole(), .hole = self.hole, .last_stmt = self.last_stmt, .branches = self.branch_count };
        self.hole = saved.start;
        self.last_stmt = null;
        return saved;
    }

    /// End statements begun by `beginBackward` at `next`: the first of them,
    /// or `next` itself when none was emitted. A straight-line run links its
    /// last statement to `next`; one that branched reaches `next` through a
    /// join.
    pub fn endBackward(self: *Builder, saved: Backward, next: CFStmtId) Allocator.Error!CFStmtId {
        defer {
            self.hole = saved.hole;
            self.last_stmt = saved.last_stmt;
        }
        const end_hole = self.hole orelse return saved.start;
        if (end_hole == saved.start) return next;
        if (self.branch_count == saved.branches) {
            const last = self.last_stmt orelse invariant("a straight-line statement run recorded no last statement");
            var stmt = self.store.getCFStmt(last);
            setNext(&stmt, next);
            try self.store.replaceCFStmt(last, stmt, self.store.stmtOrigin(last));
            return saved.start;
        }
        const join = self.freshJoin();
        try self.fill(end_hole, .{ .jump = .{ .target = join } }, .lowering_glue);
        return try self.addJoinStmt(join, &.{}, next, saved.start);
    }

    /// End statements begun by `beginBackward` that ended in a terminal:
    /// the first of them.
    pub fn endBackwardTerminal(self: *Builder, saved: Backward) CFStmtId {
        if (self.hole != null) invariant("a statement run expected to end in a terminal falls through");
        self.hole = saved.hole;
        self.last_stmt = saved.last_stmt;
        return saved.start;
    }

    pub const Backward = struct { start: CFStmtId, hole: ?CFStmtId, last_stmt: ?CFStmtId, branches: u32 };

    /// Emit tests that `value` matches `pattern`, branching to `miss` when it
    /// does not, and bind the pattern's binders: a destructuring pattern
    /// outside a match (a parameter, a declaration, a loop's pattern).
    /// Continues in the matched path.
    pub fn testPattern(self: *Builder, root_pattern: PatternId, root_value: LocalId, root_ty: TypeId, miss: CFStmtId) Allocator.Error!void {
        var pending: std.ArrayList(PatternItem) = .empty;
        defer pending.deinit(self.allocator);
        try pending.append(self.allocator, .{ .pattern = root_pattern, .value = root_value, .ty = root_ty });
        while (pending.pop()) |item| {
            switch (self.bodies().patternData(item.pattern)) {
                .assign => try self.binders.put(self.allocator, item.pattern, item.value),
                .underscore => {},
                .as => |as| {
                    try self.binders.put(self.allocator, item.pattern, item.value);
                    try pending.append(self.allocator, .{ .pattern = as.pattern, .value = item.value, .ty = item.ty });
                },
                .nominal => |nominal| try pending.append(self.allocator, .{ .pattern = nominal.backing_pattern, .value = item.value, .ty = try self.structuralType(item.ty) }),
                .applied_tag => |tag| {
                    const ty = item.ty;
                    const name = try self.program.store.internName(self.view.nameStore().tagLabelText(self.bodies().tagLabel(tag.name)));
                    if (!try self.closed(ty)) {
                        try self.testDynamicTag(ty, name, tag.args, item.value, miss, &pending);
                        continue;
                    }
                    switch (self.program.store.get(try self.structuralType(ty))) {
                        .primitive => |primitive| {
                            if (primitive != .bool) invariant("a tag pattern's type is a non-Bool primitive");
                            const want_true = std.mem.eql(u8, self.program.store.nameText(name), "True");
                            const cond = if (want_true) item.value else blk: {
                                const negated = try self.closedLocal(.bool);
                                try self.emitLowLevel(negated, .bool_not, &.{item.value});
                                break :blk negated;
                            };
                            try self.branchOrMiss(cond, miss);
                        },
                        .tag_union => |union_ty| {
                            const tags = self.program.store.tagSpan(union_ty.tags);
                            const structural = try self.structuralType(ty);
                            const variant = (try self.selectVariant(structural, self.ownTag(structural, name, tag.args.len))).index;
                            const committed = self.program.result.layouts.getLayout(try self.program.layouts.layoutOf(ty));
                            if (committed.tag == .tag_union and tags.len > 1) {
                                // A switch on the discriminant, which proves
                                // the variant its arm reads.
                                const discriminant = try self.closedLocal(.u32);
                                try self.emitLinear(.{ .assign_ref = .{ .target = discriminant, .op = LIR.RefOp.discriminant(item.value), .next = undefined } });
                                const then = try self.newHole();
                                const branches = try self.store.addCFSwitchBranches(&.{.{ .value = @intCast(variant), .body = then }});
                                try self.emitTerminal(.{ .switch_stmt = .{ .cond = discriminant, .branches = branches, .default_branch = miss } });
                                self.hole = then;
                            }
                            const payloads = self.program.store.span(tags[variant].payloads);
                            for (tag.args, 0..) |arg_pattern, arg_index| {
                                if (committed.tag != .tag_union) {
                                    // A union without storage has only
                                    // zero-sized payloads.
                                    const payload_local = try self.localOf(payloads[arg_index], .frame);
                                    if (self.store.getLocal(payload_local).layout_idx != .zst) invariant("a storage-free union has a payload with storage");
                                    try self.emitLinear(.{ .assign_struct = .{ .target = payload_local, .fields = LIR.LocalSpan.empty(), .next = undefined } });
                                    try pending.append(self.allocator, .{ .pattern = arg_pattern, .value = payload_local, .ty = payloads[arg_index] });
                                    continue;
                                }
                                const payload_local = try self.readTagPayload(item.value, structural, variant, payloads, arg_index);
                                try pending.append(self.allocator, .{ .pattern = arg_pattern, .value = payload_local, .ty = payloads[arg_index] });
                            }
                        },
                        .empty_record, .record, .tuple, .list, .box, .nominal, .function, .variable => invariant("a tag pattern's type is not a tag union"),
                    }
                },
                .tuple => |items| {
                    const ty = try self.structuralType(item.ty);
                    const elems = switch (self.program.store.get(ty)) {
                        .tuple => |span| self.program.store.span(span),
                        .primitive, .empty_record, .record, .tag_union, .list, .box, .nominal, .function, .variable => invariant("a tuple pattern's type is not a tuple"),
                    };
                    for (items, 0..) |sub, index| {
                        const local = try self.readField(ty, item.value, @intCast(index), elems[index]);
                        try pending.append(self.allocator, .{ .pattern = sub, .value = local, .ty = elems[index] });
                    }
                },
                .record_destructure => |destruct_ids| {
                    const ty = try self.structuralType(item.ty);
                    const record = switch (self.program.store.get(ty)) {
                        .record => |record| record,
                        .empty_record => if (destruct_ids.len == 0) continue else invariant("a record pattern names a field of the empty record"),
                        .primitive, .tuple, .tag_union, .list, .box, .nominal, .function, .variable => invariant("a record pattern's type is not a record"),
                    };
                    const fields = self.program.store.fieldSpan(record.fields);
                    for (destruct_ids) |destruct_id| {
                        const destruct = self.bodies().recordDestruct(destruct_id);
                        const name = try self.program.store.internName(self.view.nameStore().recordFieldLabelText(destruct.label));
                        const sub = switch (destruct.kind) {
                            .required, .sub_pattern => |p| p,
                            .rest => |rest| {
                                // `..` with nothing to bind reads nothing.
                                if (self.bodies().patternData(rest) == .underscore) continue;
                                // The rest is the fields the pattern does
                                // not name, read by name.
                                const rest_ty = try self.patternType(rest);
                                const rest_value = try self.localOf(rest_ty, .frame);
                                try self.emitAdapt(rest_value, item.value, item.ty, rest_ty);
                                try pending.append(self.allocator, .{ .pattern = rest, .value = rest_value, .ty = rest_ty });
                                continue;
                            },
                        };
                        const position = for (fields, 0..) |field, index| {
                            if (field.name == name) break index;
                        } else null;
                        if (position) |index| {
                            if (fields[index].kind == .presence_slot) {
                                // An optional field destructures to its
                                // presence as `Try(value, [MissingField])`.
                                const slot_ty = try self.program.layouts.presenceSlot(fields[index].ty);
                                const slot = try self.readField(ty, item.value, @intCast(index), slot_ty);
                                const try_ty = try self.patternType(sub);
                                const presence = try self.presenceTry(slot, slot_ty, try_ty);
                                try pending.append(self.allocator, .{ .pattern = sub, .value = presence, .ty = try_ty });
                                continue;
                            }
                            const local = try self.readField(ty, item.value, @intCast(index), fields[index].ty);
                            try pending.append(self.allocator, .{ .pattern = sub, .value = local, .ty = fields[index].ty });
                        } else {
                            // A field of an open row's remainder: only the
                            // record's descriptor knows where it is.
                            if (record.ext == null) invariant("a record pattern's field is absent from its type");
                            const field_ty = try self.patternType(sub);
                            const local = try self.localOf(field_ty, .view);
                            try self.emitDyn(local, .{ .field_addr = .{ .base = item.value, .desc = try self.descOf(ty), .field_index = try self.fieldIndex(ty, name) } });
                            try pending.append(self.allocator, .{ .pattern = sub, .value = local, .ty = field_ty });
                        }
                    }
                },
                .list => |list| try self.testListPattern(item, list.patterns, list.rest, miss, &pending),
                .numeral_literal => {
                    if (self.bodies().literalPatternIsCustom(item.pattern)) {
                        try self.literalPatternGuard(item, miss);
                    } else {
                        const expected = try self.builtinNumeral(self.bodies().numeralLiteral(@backingInt(item.pattern)), item.ty);
                        const matches = try self.closedLocal(.bool);
                        try self.emitLowLevel(matches, .num_is_eq, &.{ item.value, expected });
                        try self.branchOrMiss(matches, miss);
                    }
                },
                .str_literal => |literal| {
                    if (self.bodies().literalPatternIsCustom(item.pattern)) {
                        try self.literalPatternGuard(item, miss);
                    } else {
                        const expected = try self.strLiteral(self.bodies().text(literal));
                        const matches = try self.closedLocal(.bool);
                        try self.emitLowLevel(matches, .str_is_eq, &.{ item.value, expected });
                        try self.branchOrMiss(matches, miss);
                    }
                },
                .str_interpolation => |str| {
                    // The match binds each captured span as a `Str` view of
                    // the matched string.
                    const str_ty = try self.program.store.primitive(.str);
                    const steps = try self.allocator.alloc(LIR.StrMatchStep, str.steps.span.len);
                    defer self.allocator.free(steps);
                    for (steps, 0..) |*out, step_index| {
                        const str_step = self.bodies().strPatternStep(str.steps, @intCast(step_index));
                        const delimiter = self.bodies().text(str_step.delimiter);
                        const capture: LIR.StrMatchCapture = if (str_step.capture) |capture_pattern| blk: {
                            const local = try self.closedLocal(.str);
                            try pending.append(self.allocator, .{ .pattern = capture_pattern, .value = local, .ty = str_ty });
                            break :blk .{ .view = local };
                        } else .discard;
                        out.* = .{ .capture = capture, .delimiter = .{ .backing = try self.store.insertString(delimiter), .offset = 0, .len = @intCast(delimiter.len) } };
                    }
                    const prefix = self.bodies().text(str.prefix);
                    const then = try self.newHole();
                    try self.emitTerminal(.{ .str_match = .{
                        .source = item.value,
                        .data = try self.store.addStmtRow(LIR.StrMatchData, .{
                            .prefix = .{ .backing = try self.store.insertString(prefix), .offset = 0, .len = @intCast(prefix.len) },
                            .steps = try self.store.addStrMatchSteps(steps),
                            .end = switch (str.end) {
                                .exact => .exact,
                                .tail => .tail,
                            },
                        }),
                        .on_match = then,
                        .on_miss = miss,
                    } });
                    self.hole = then;
                },
                .runtime_error => invariant("an erroneous pattern reached Boxy pattern lowering, which code holding one never reaches"),
            }
        }
    }

    /// A list pattern: the length test (exact, or at least the items' count
    /// with a rest), then each item and the rest's sublist.
    pub fn testListPattern(self: *Builder, item: PatternItem, items: []const PatternId, rest: ?checked.CheckedListRestPattern, miss: CFStmtId, pending: *std.ArrayList(PatternItem)) Allocator.Error!void {
        const store = &self.program.store;
        const list_ty = try self.structuralType(item.ty);
        const elem_ty = switch (store.get(list_ty)) {
            .list => |elem| elem,
            .primitive, .empty_record, .record, .tuple, .tag_union, .box, .nominal, .function, .variable => invariant("a list pattern's value is not a List"),
        };
        const u64_ty = try store.primitive(.u64);
        const len = try self.closedLocal(.u64);
        try self.emitListLowLevel(len, .list_len, &.{item.value}, elem_ty, list_ty);
        const count = try self.intValue(items.len, u64_ty);
        const fits = try self.closedLocal(.bool);
        try self.emitLowLevel(fits, if (rest == null) .num_is_eq else .num_is_gte, &.{ len, count });
        try self.branchOrMiss(fits, miss);
        for (items, 0..) |sub, index| {
            const at = if (rest != null and index >= rest.?.index) blk: {
                // An item after the rest counts from the end.
                const from_end = try self.intValue(items.len - index, u64_ty);
                const position = try self.closedLocal(.u64);
                try self.emitLowLevel(position, .num_int_sub_wrap, &.{ len, from_end });
                break :blk position;
            } else try self.intValue(index, u64_ty);
            const elem = try self.localOf(elem_ty, .frame);
            try self.emitListLowLevel(elem, .list_get_unsafe, &.{ item.value, at }, elem_ty, list_ty);
            try pending.append(self.allocator, .{ .pattern = sub, .value = elem, .ty = elem_ty });
        }
        if (rest) |rest_info| if (rest_info.pattern) |rest_pattern| {
            if (self.bodies().patternData(rest_pattern) == .underscore) return;
            const rest_len = try self.closedLocal(.u64);
            try self.emitLowLevel(rest_len, .num_int_sub_wrap, &.{ len, count });
            // `{ len, start }` in field-name order.
            const range_ty = try store.intern(.{ .record = .{ .fields = &.{
                .{ .name = try store.internName("len"), .ty = u64_ty, .kind = .inline_slot },
                .{ .name = try store.internName("start"), .ty = u64_ty, .kind = .inline_slot },
            }, .ext = null } });
            const range = try self.buildStruct(range_ty, &.{ rest_len, try self.intValue(rest_info.index, u64_ty) });
            const sublist = try self.localOf(list_ty, .frame);
            try self.emitListLowLevel(sublist, .list_sublist, &.{ item.value, range }, elem_ty, list_ty);
            try pending.append(self.allocator, .{ .pattern = rest_pattern, .value = sublist, .ty = item.ty });
        };
    }

    /// A list low-level over a list of `elem_ty`: an ordinary low-level for
    /// closed items, the descriptor-carrying form otherwise.
    pub fn emitListLowLevel(self: *Builder, target: LocalId, op: LIR.LowLevel, args: []const LocalId, elem_ty: TypeId, list_ty: TypeId) Allocator.Error!void {
        if (try self.closed(list_ty)) return try self.emitLowLevel(target, op, args);
        const with_desc = try std.mem.concat(self.allocator, LocalId, &.{ args, &.{try self.descOf(elem_ty)} });
        defer self.allocator.free(with_desc);
        try self.emitLinear(.{ .assign_low_level = .{
            .target = target,
            .op = op,
            .rc_effect = try self.store.addRcEffect(op.rcEffect()),
            .extra = try self.store.addStmtRow(LIR.LowLevelExtra, .{ .dyn_elem_desc = true }),
            .args = try self.store.addLocalSpan(with_desc),
            .next = undefined,
        } });
    }

    /// A literal pattern at a type whose literals convert through a method:
    /// the pattern, its own binder, holds the tested value, and the checked
    /// equality against the converted literal decides the match.
    pub fn literalPatternGuard(self: *Builder, item: PatternItem, miss: CFStmtId) Allocator.Error!void {
        try self.binders.put(self.allocator, item.pattern, item.value);
        const matches = try self.lowerNow(.{ .literal_pattern_guard = item.pattern });
        try self.branchOrMiss(matches, miss);
    }

    /// The `Try(value, [MissingField])` of type `try_ty` a presence slot
    /// holds: `Ok` of its payload, or `Err(MissingField)`.
    pub fn presenceTry(self: *Builder, slot: LocalId, slot_ty: TypeId, try_ty: TypeId) Allocator.Error!LocalId {
        const store = &self.program.store;
        const present = try store.internName(@import("layouts.zig").present_label);
        const try_union = try self.structuralType(try_ty);
        const result = try self.localOf(try_ty, .frame);
        const join = self.freshJoin();
        const after = try self.declareMergeJoin(join, result, null);
        const missing_hole = try self.newHole();
        try self.branchOnTag(slot, slot_ty, present, missing_hole);
        const payload = try self.tagPayloadValue(slot, slot_ty, present, 0);
        try self.emitJump(join, try self.buildTag(try_union, try store.internName("Ok"), &.{payload.value}));
        self.hole = missing_hole;
        const err_payload_ty = (try self.closedVariant(try_union, try store.internName("Err"))).payloads[0];
        const missing = try self.buildTag(try self.structuralType(err_payload_ty), try store.internName("MissingField"), &.{});
        try self.emitJump(join, try self.buildTag(try_union, try store.internName("Err"), &.{missing}));
        self.hole = after;
        return result;
    }

    /// Whether a tag union value holds the payload-free tag `label`: the
    /// checker's discriminant-only equality.
    pub fn finishDiscriminantEq(self: *Builder, value_ty: TypeId, label: check.CheckedNames.TagLabelId, negated: bool) Allocator.Error!void {
        const value = self.popValue();
        const name = try self.program.store.internName(self.view.nameStore().tagLabelText(label));
        const ty = try self.structuralType(value_ty);
        const actual = try self.closedLocal(.u32);
        const expected = try self.closedLocal(.u32);
        if (try self.closed(ty)) {
            const tags = switch (self.program.store.get(ty)) {
                .tag_union => |union_ty| self.program.store.tagSpan(union_ty.tags),
                .primitive, .empty_record, .record, .tuple, .list, .box, .nominal, .function, .variable => invariant("a discriminant equality's operand is not a tag union"),
            };
            const variant = for (tags, 0..) |tag, index| {
                if (tag.name == name and tag.payloads.len == 0) break index;
            } else invariant("a discriminant equality's tag is absent from its operand's type");
            const committed = self.program.result.layouts.getLayout(try self.program.layouts.layoutOf(ty));
            if (committed.tag == .tag_union) {
                try self.emitLinear(.{ .assign_ref = .{ .target = actual, .op = LIR.RefOp.discriminant(value), .next = undefined } });
            } else {
                try self.emitLinear(.{ .assign_literal = .{ .target = actual, .value = .{ .i64_literal = .{ .value = 0, .layout_idx = .u32 } }, .next = undefined } });
            }
            try self.emitLinear(.{ .assign_literal = .{ .target = expected, .value = .{ .i64_literal = .{ .value = @intCast(variant), .layout_idx = .u32 } }, .next = undefined } });
        } else {
            try self.emitDyn(actual, .{ .tag_discriminant = .{ .base = value, .desc = try self.descOf(ty) } });
            try self.assignIndex(expected, try self.tagIndex(ty, name));
        }
        const result = try self.closedLocal(.bool);
        try self.emitLowLevel(result, .num_is_eq, &.{ actual, expected });
        if (!negated) return try self.pushValue(result);
        const flipped = try self.closedLocal(.bool);
        try self.emitLowLevel(flipped, .bool_not, &.{result});
        try self.pushValue(flipped);
    }

    /// A record field access: each segment reads one field of the record
    /// the previous segment produced.
    pub fn finishFieldAccess(self: *Builder, expr_id: ExprId) Allocator.Error!void {
        const access = self.bodies().exprData(expr_id).field_access;
        var value = self.popValue();
        var record_ty = try self.valueType(access.receiver);
        var misses: std.ArrayList(CFStmtId) = .empty;
        defer misses.deinit(self.allocator);
        const present = try self.program.store.internName(@import("layouts.zig").present_label);
        // A path with a `.?` segment is one flat `Try(value, [MissingField])`,
        // which every exit of the path delivers to one join.
        const segment_count = access.segments.len;
        const optional = for (0..segment_count) |position| {
            if (self.bodies().fieldAccessSegment(access.segments, @intCast(position)).mode == .optional) break true;
        } else false;
        var result_join: ?struct { join: LIR.JoinPointId, after: CFStmtId, result: LocalId } = null;
        if (optional) {
            const result = try self.localOf(try self.exprType(expr_id), .frame);
            const join = self.freshJoin();
            const after = try self.declareMergeJoin(join, result, null);
            result_join = .{ .join = join, .after = after, .result = result };
        }
        for (0..segment_count) |segment_position| {
            const segment = self.bodies().fieldAccessSegment(access.segments, @intCast(segment_position));
            const name = try self.program.store.internName(self.view.nameStore().recordFieldLabelText(segment.field_name));
            const structural = try self.structuralType(record_ty);
            const record = switch (self.program.store.get(structural)) {
                .record => |record| record,
                .primitive, .empty_record, .tuple, .tag_union, .list, .box, .nominal, .function, .variable => invariant("a field access's receiver is not a record"),
            };
            const fields = self.program.store.fieldSpan(record.fields);
            const position = for (fields, 0..) |field, index| {
                if (field.name == name) break index;
            } else null;
            const field_ty = try self.convert(segment.success_ty);
            // The stored type of the field: its value, or for an optional
            // segment its presence slot.
            const slot_ty = switch (segment.mode) {
                .required => field_ty,
                .optional => try self.program.layouts.presenceSlot(field_ty),
            };
            var stored: LocalId = undefined;
            if (record.ext != null and position == null) {
                // A field of an open row's remainder: only the record's
                // descriptor knows where it is.
                stored = try self.localOf(slot_ty, .view);
                try self.emitDyn(stored, .{ .field_addr = .{ .base = value, .desc = try self.descOf(structural), .field_index = try self.fieldIndex(structural, name) } });
            } else {
                const index = position orelse invariant("a field access's field is absent from its record type");
                const expected_kind: types.FieldKind = switch (segment.mode) {
                    .required => .inline_slot,
                    .optional => .presence_slot,
                };
                if (fields[index].kind != expected_kind) invariant("a field access's mode differs from its field's kind");
                stored = try self.readField(structural, value, @intCast(index), slot_ty);
            }
            switch (segment.mode) {
                .required => value = stored,
                .optional => {
                    // `.?`: a missing field ends the path with an error.
                    const miss = try self.newHole();
                    try self.branchOnTag(stored, slot_ty, present, miss);
                    try misses.append(self.allocator, miss);
                    value = (try self.tagPayloadValue(stored, slot_ty, present, 0)).value;
                },
            }
            record_ty = field_ty;
        }
        const exit = result_join orelse return try self.pushValue(value);
        const join = exit.join;
        const try_union = try self.structuralType(try self.exprType(expr_id));
        const store = &self.program.store;
        const ok = try self.buildTag(try_union, try store.internName("Ok"), &.{value});
        try self.emitJump(join, ok);
        const err_payload_ty = (try self.closedVariant(try_union, try store.internName("Err"))).payloads[0];
        for (misses.items) |miss| {
            self.hole = miss;
            const missing = try self.buildTag(try self.structuralType(err_payload_ty), try store.internName("MissingField"), &.{});
            const err = try self.buildTag(try_union, try store.internName("Err"), &.{missing});
            try self.emitJump(join, err);
        }
        self.hole = exit.after;
        try self.pushValue(exit.result);
    }

    /// The stored types of a record's or tuple's components, by checked field
    /// index.
    pub fn structComponentTypes(self: *Builder, raw_ty: TypeId) Allocator.Error![]TypeId {
        const ty = try self.structuralType(raw_ty);
        return switch (self.program.store.get(ty)) {
            .tuple => |items| try self.allocator.dupe(TypeId, self.program.store.span(items)),
            .record => |record| blk: {
                const fields = self.program.store.fieldSpan(record.fields);
                const out = try self.allocator.alloc(TypeId, fields.len);
                for (fields, out) |field, *slot| slot.* = try self.program.layouts.fieldStorage(field);
                break :blk out;
            },
            .primitive, .empty_record, .tag_union, .list, .box, .nominal, .function, .variable => try self.allocator.alloc(TypeId, 0),
        };
    }

    /// `ty` with its nominal wrappers peeled: a nominal type shares its
    /// backing's representation, so a tag or record of a nominal type is
    /// built and matched at its backing.
    pub fn structuralType(self: *Builder, ty: TypeId) Allocator.Error!TypeId {
        var current = ty;
        while (self.program.store.get(current) == .nominal) current = try self.program.converter().backing(current);
        return current;
    }

    /// Field `field_index` (of type `field_ty`) of the record or tuple
    /// `base` of type `ty`, as a borrowed read.
    pub fn readField(self: *Builder, ty: TypeId, base_local: LocalId, field_index: u32, field_ty: TypeId) Allocator.Error!LocalId {
        const local = try self.localOf(field_ty, if (try self.closed(ty) or self.store.getLocal(base_local).product != .none) .frame else .view);
        try self.readFieldInto(local, ty, base_local, field_index, field_ty);
        return local;
    }

    /// `readField` into `target`.
    pub fn readFieldInto(self: *Builder, target: LocalId, ty: TypeId, base_local: LocalId, field_index: u32, field_ty: TypeId) Allocator.Error!void {
        if (try self.closed(ty)) {
            const base_layout = self.store.getLocal(base_local).layout_idx;
            if (base_layout == .zst) {
                // A zero-sized product has no stored fields. Its field is
                // the same zero-bit value, independent of how the base was
                // produced (including an immutable constant read).
                if (self.store.getLocal(target).layout_idx != .zst) invariant("a zero-sized record has a field with storage");
                return self.emitLinear(.{ .assign_ref = .{ .target = target, .op = LIR.RefOp.local(base_local), .next = undefined } });
            }
            if (self.program.result.layouts.getLayout(base_layout).tag != .struct_) {
                try self.emitLinear(.{ .assign_ref = .{ .target = target, .op = LIR.RefOp.field(base_local, field_index), .next = undefined } });
                return;
            }
            return try self.readSlotInto(target, LIR.RefOp.field(base_local, field_index), field_ty, self.structFieldLayout(base_layout, field_index));
        }
        if (self.store.getLocal(base_local).product != .none) {
            // Independent representation storage permits an ownership take;
            // retaining contents would not keep an interior address alive.
            if (self.store.localDyn(target)) |dyn| {
                if (dyn.storage == .view) self.store.getLocalPtr(target).dyn_storage = .frame;
            }
            return self.emitLinear(.{ .assign_ref = .{ .target = target, .op = LIR.RefOp.field(base_local, field_index), .next = undefined } });
        }
        // A field of an open row ranks among the remainder's fields too, so
        // its position is the instantiation's.
        switch (self.program.store.get(try self.structuralType(ty))) {
            .record => |record| if (record.ext != null) {
                const name = self.program.store.fieldSpan(record.fields)[field_index].name;
                return try self.emitDyn(target, .{ .field_addr = .{ .base = base_local, .desc = try self.descOf(ty), .field_index = try self.fieldIndex(ty, name) } });
            },
            .primitive, .empty_record, .tuple, .tag_union, .list, .box, .nominal, .function, .variable => {},
        }
        try self.emitDyn(target, .{ .field_addr = .{ .base = base_local, .desc = try self.descOf(ty), .field_index = .{ .immediate = field_index } } });
    }

    /// Test that the dynamic tag union `value` holds tag `name`, and queue
    /// its payloads against `args`.
    pub fn testDynamicTag(self: *Builder, nominal_ty: TypeId, name: types.NameId, args: []const PatternId, value: LocalId, miss: CFStmtId, pending: anytype) Allocator.Error!void {
        const store = &self.program.store;
        const ty = try self.structuralType(nominal_ty);
        const union_ty = switch (store.get(ty)) {
            .tag_union => |union_ty| union_ty,
            .primitive, .empty_record, .record, .tuple, .list, .box, .nominal, .function, .variable => invariant("a tag pattern's type is not a tag union"),
        };
        const desc = try self.descOf(ty);
        const actual = try self.closedLocal(.u32);
        try self.emitDyn(actual, .{ .tag_discriminant = .{ .base = value, .desc = desc } });
        const expected = try self.indexLocal(try self.tagIndex(ty, name));
        const matches = try self.closedLocal(.bool);
        try self.emitLowLevel(matches, .num_is_eq, &.{ actual, expected });
        try self.branchOrMiss(matches, miss);
        const payload_types = for (store.tagSpan(union_ty.tags)) |tag| {
            if (tag.name == name and tag.payloads.len == args.len) break store.span(tag.payloads);
        } else invariant("a tag pattern's tag is absent from its type");
        for (args, payload_types, 0..) |arg, payload_ty, index| {
            const local = try self.localOf(payload_ty, .view);
            try self.emitDyn(local, .{ .tag_payload_addr = .{ .base = value, .desc = desc, .payload_index = @intCast(index) } });
            try pending.append(self.allocator, .{ .pattern = arg, .value = local, .ty = payload_ty });
        }
    }

    /// Continue when `cond` is true; otherwise continue at the enclosing
    /// match branch's shared `miss` continuation.
    pub fn branchOrMiss(self: *Builder, cond: LocalId, miss: CFStmtId) Allocator.Error!void {
        const then = try self.newHole();
        const branches = try self.store.addCFSwitchBranches(&.{.{ .value = 1, .body = then }});
        try self.emitTerminal(.{ .switch_stmt = .{ .cond = cond, .branches = branches, .default_branch = miss } });
        self.hole = then;
    }

    // -- Statements --

    pub fn stepStmt(self: *Builder, stmt_id: StatementId) Allocator.Error!void {
        const stmt = self.bodies().statement(stmt_id);
        self.region = stmt.source_region;
        switch (stmt.data) {
            .decl => |decl| {
                // An annotation-only declaration (a method requesting its
                // derivation) has no value, and every use of it resolves to
                // what it requests.
                if (self.bodies().exprData(decl.expr) == .anno_only) return;
                // A recursive value's binders get slots before its value is
                // built, which the callables in the value capture, and the
                // slots are filled once the value is bound.
                if (self.bodies().declIsRecursive(stmt_id)) {
                    try self.allocRecursiveSlots(decl.pattern);
                    try self.push(.{ .fill_recursive_slots = decl.pattern });
                }
                // A scheme alias with a generalized scheme binds nothing:
                // each use lowers its right-hand side at the use's
                // instantiation (`lowerAliasUse`).
                if (self.bodies().binder(decl.pattern).is_scheme_alias) {
                    if (try self.program.localScope(self.module, decl.expr)) |scope_id| {
                        const scheme = try self.localScheme(scope_id);
                        if (!scheme.isEmpty()) return;
                    }
                }
                // A value that diverges binds nothing.
                if (try self.bodies().exprDiverges(decl.expr, self.inlineExpectMode())) {
                    try self.push(.discard);
                } else {
                    try self.push(.{ .bind = .{ .pattern = decl.pattern, .ty = try self.valueType(decl.expr) } });
                }
                try self.push(.{ .expr = decl.expr });
            },
            .var_ => |var_| {
                if (try self.bodies().exprDiverges(var_.expr, self.inlineExpectMode())) {
                    try self.push(.discard);
                } else {
                    try self.push(.{ .bind = .{ .pattern = var_.pattern, .ty = try self.valueType(var_.expr) } });
                }
                try self.push(.{ .expr = var_.expr });
            },
            .reassign => |reassign| {
                try self.push(.{ .bind = .{ .pattern = reassign.pattern, .ty = try self.valueType(reassign.expr) } });
                try self.push(.{ .expr = reassign.expr });
            },
            .var_uninitialized => |var_| {
                const local = try self.localOf(try self.patternType(var_.pattern), .frame);
                try self.emitLinear(.{ .init_uninitialized = .{ .target = local, .next = undefined } });
                try self.bindIrrefutable(var_.pattern, local, try self.patternType(var_.pattern));
            },
            .while_ => |loop| try self.lowerLoop(@backingInt(stmt_id), loop, true),
            // `while True`: the body runs unconditionally, and only a break
            // (in a breakable loop) leaves it.
            .infinite_loop, .breakable_loop => |loop| try self.lowerLoop(@backingInt(stmt_id), loop, false),
            .for_ => |for_| {
                try self.push(.discard);
                try self.pushForLoop(@backingInt(stmt_id), for_.pattern, for_.expr, for_.body, self.bodies().iteratorForPlan(@backingInt(stmt_id)));
            },
            .break_ => {
                if (self.loops.items.len == 0) invariant("a break outside every loop reached Boxy lowering");
                const loop = &self.loops.items[self.loops.items.len - 1];
                loop.reached = true;
                try self.emitJump(loop.join, null);
            },
            .expr => |expr| {
                try self.push(.discard);
                try self.push(.{ .expr = expr });
            },
            .expect => |cond| {
                try self.push(.discard);
                try self.push(.{ .finish_expect = cond });
                try self.push(.{ .expr = cond });
            },
            .crash => |literal| {
                const message = try self.store.insertString(self.bodies().text(literal));
                try self.emitTerminal(.{ .crash = .{ .msg = .{ .literal = message } } });
            },
            .runtime_error => {
                const literal = try self.store.insertString("runtime error");
                try self.emitTerminal(.{ .crash = .{ .msg = .{ .literal = literal }, .checked_error = true } });
            },
            .dbg => |expr| {
                try self.push(.discard);
                try self.push(.{ .finish_dbg = try self.valueType(expr) });
                try self.push(.{ .expr = expr });
            },
            .import_, .alias_decl, .where_alias_decl, .nominal_decl, .type_anno, .type_var_alias => {},
            .promoted_proc => {},
            // A method declaration checking rejected evaluates nothing.
            .rejected_method_decl => {},
            .return_ => |ret| {
                // The enclosing function returns the value; nothing after the
                // statement is reached.
                try self.push(.ret);
                try self.destination_types.put(self.allocator, ret.expr, self.currentWorker().ret);
                try self.push(.{ .expr = ret.expr });
            },
        }
    }

    /// A condition-driven loop: a head join carrying the loop's mutated
    /// binders, entered first with their current values, and an exit join
    /// carrying them out.
    /// A condition loop, the statement at CIR node `loop_node`.
    pub fn lowerLoop(self: *Builder, loop_node: u32, loop: bodies_mod.ConditionLoop, tests_condition: bool) Allocator.Error!void {
        const binders = try self.bodies().loopMutationBinders(loop_node);
        const exit_merge = try self.beginMerge(binders, .joined);
        const head_merge = try self.beginMerge(binders, .joined);
        const exit = self.freshJoin();
        const head = self.freshJoin();
        const after = try self.declareMergeJoin(exit, null, exit_merge);
        const head_body = try self.declareMergeJoin(head, null, head_merge);
        try self.emitJump(head, null);
        self.store.shapes.loop = true;
        // In the head: binders are the head's parameters.
        self.hole = head_body;
        if (head_merge) |m| try self.enterMerge(m);
        try self.loops.append(self.allocator, .{ .join = exit, .reached = tests_condition });
        try self.push(.end_loop);
        if (exit_merge) |m| try self.push(.{ .enter_merge = m });
        try self.push(.{ .resume_at = after });
        if (tests_condition) {
            try self.push(.{ .loop_test = .{ .body = loop.body, .head = head, .exit = exit } });
            try self.push(.{ .expr = loop.cond });
        } else {
            try self.push(.{ .jump_without_value = head });
            try self.push(.discard);
            try self.push(.{ .expr = loop.body });
        }
    }

    /// A `for` loop, the expression or statement at CIR node `loop_node`.
    pub fn pushForLoop(self: *Builder, loop_node: u32, pattern: PatternId, iterable: ExprId, body: ExprId, plan: ?static_dispatch.IteratorForPlanId) Allocator.Error!void {
        const plan_id = plan orelse invariant("a for loop has no checked iterator plan");
        // A loop whose pattern checking rejected evaluates its iterable,
        // then crashes where it would begin: its pattern binds nothing.
        const item_ty = self.view.view.static_dispatch_plans.iterator_for_plans[@backingInt(plan_id)].item_ty;
        if (try self.program.hasError(self.module, item_ty) or try self.program.hasError(self.module, self.bodies().patternType(pattern))) {
            try self.push(.{ .rejected_for = plan_id });
            try self.push(.{ .expr = iterable });
            return;
        }
        try self.push(.{ .unreachable_or_unit = {} });
        if (try self.iteratorChain(plan_id)) |chain| {
            try self.forChainStart(loop_node, pattern, body, plan_id, chain);
            return;
        }
        try self.push(.{ .for_start = .{ .loop = loop_node, .pattern = pattern, .body = body, .plan = plan_id } });
        try self.push(.{ .expr = iterable });
    }

    /// One producer of a `for` loop's iterable, which the loop runs
    /// structurally when the checker stamped every producer of the chain
    /// (design.md "Iterator `for`"): a source yielding each item of a
    /// list, a string's bytes, or the steps of an iterator value, and the
    /// adapters over it, innermost first. Operands are checked expressions
    /// the loop evaluates in chain order before it starts.
    pub const ChainLayer = union(enum) {
        list: ChainListSource,
        list_rev: ChainListSource,
        str: struct { str: ExprId },
        /// An iterator value whose steps the loop takes through the `Iter`
        /// protocol, as a loop over the value alone does.
        value: struct { iterator: ExprId },
        map: struct { transform: ExprId, item_ty: TypeId },
        keep_if: struct { predicate: ExprId },
        drop_if: struct { predicate: ExprId },
        /// The stamped `iter_with_index` worker: its counter starts at the
        /// second operand.
        with_index: struct { start: ExprId, item_ty: TypeId },
        take_first: struct { count: ExprId },
        drop_first: struct { count: ExprId },
    };

    pub const ChainListSource = struct { list: ExprId, item_ty: TypeId };

    /// The structural chain of a `for` loop's iterable, innermost producer
    /// first, or null when the loop takes its steps through the `Iter`
    /// protocol alone: its `iter` dispatch and every producer of the
    /// iterable expression are read from the checker's
    /// `IteratorProcedureId` stamps on their resolved targets, never from
    /// names or type shapes, and a producer without a stamp this lowering
    /// runs structurally ends the chain as an iterator value.
    fn iteratorChain(self: *Builder, plan_id: static_dispatch.IteratorForPlanId) Allocator.Error!?[]const ChainLayer {
        const plan = self.view.view.static_dispatch_plans.iterator_for_plans[@backingInt(plan_id)];
        const for_site = plan.forLoopSite();
        const iterator_ty = try self.convert(plan.iterator_ty);
        // The loop steps the chain's result and every iterator value in it
        // through the builtin protocol, whose step field the loop calls.
        if (try self.builtinStepField(plan.next, iterator_ty) == null) return null;
        var layers: std.ArrayList(ChainLayer) = .empty;
        defer layers.deinit(self.allocator);
        const iter_stamp = self.dispatchStamp(plan.iter.resolution);
        switch (iter_stamp orelse return null) {
            .list_iter => try layers.append(self.allocator, .{ .list = .{ .list = for_site.iterable, .item_ty = try self.iteratorItemType(iterator_ty) } }),
            .list_iter_rev => try layers.append(self.allocator, .{ .list_rev = .{ .list = for_site.iterable, .item_ty = try self.iteratorItemType(iterator_ty) } }),
            .str_iter_utf8 => try layers.append(self.allocator, .{ .str = .{ .str = for_site.iterable } }),
            .identity => {
                var current = for_site.iterable;
                walk: while (true) {
                    const call = try self.chainCall(current) orelse {
                        try layers.append(self.allocator, .{ .value = .{ .iterator = current } });
                        break :walk;
                    };
                    const item_ty = try self.iteratorItemType(try self.exprType(current));
                    switch (call.stamp) {
                        .identity => current = call.operands[0],
                        .list_iter => {
                            try layers.append(self.allocator, .{ .list = .{ .list = call.operands[0], .item_ty = item_ty } });
                            break :walk;
                        },
                        .list_iter_rev => {
                            try layers.append(self.allocator, .{ .list_rev = .{ .list = call.operands[0], .item_ty = item_ty } });
                            break :walk;
                        },
                        .str_iter_utf8 => {
                            try layers.append(self.allocator, .{ .str = .{ .str = call.operands[0] } });
                            break :walk;
                        },
                        .map => {
                            try layers.append(self.allocator, .{ .map = .{ .transform = call.operands[1], .item_ty = item_ty } });
                            current = call.operands[0];
                        },
                        .keep_if => {
                            try layers.append(self.allocator, .{ .keep_if = .{ .predicate = call.operands[1] } });
                            current = call.operands[0];
                        },
                        .drop_if => {
                            try layers.append(self.allocator, .{ .drop_if = .{ .predicate = call.operands[1] } });
                            current = call.operands[0];
                        },
                        .with_index => {
                            try layers.append(self.allocator, .{ .with_index = .{ .start = call.operands[1], .item_ty = item_ty } });
                            current = call.operands[0];
                        },
                        .take_first => {
                            try layers.append(self.allocator, .{ .take_first = .{ .count = call.operands[1] } });
                            current = call.operands[0];
                        },
                        .drop_first => {
                            try layers.append(self.allocator, .{ .drop_first = .{ .count = call.operands[1] } });
                            current = call.operands[0];
                        },
                        .next, .custom, .single, .concat, .append, .step_by, .from_iter, .range_iter, .numeric_range_iter, .numeric_to, .numeric_until, .from_step, .range_done => {
                            try layers.append(self.allocator, .{ .value = .{ .iterator = current } });
                            break :walk;
                        },
                    }
                }
            },
            .next, .custom, .single, .map, .keep_if, .drop_if, .take_first, .drop_first, .concat, .append, .with_index, .step_by, .from_iter, .range_iter, .numeric_range_iter, .numeric_to, .numeric_until, .from_step, .range_done => return null,
        }
        // A chain of one iterator value and nothing over it is the loop the
        // protocol lowering already emits.
        if (layers.items.len == 1 and layers.items[0] == .value) return null;
        std.mem.reverse(ChainLayer, layers.items);
        return try self.program.arena.allocator().dupe(ChainLayer, layers.items);
    }

    /// The `IteratorProcedureId` the checker stamped on the procedure a
    /// dispatch resolves to, or null when it resolves to anything else.
    fn dispatchStamp(self: *Builder, resolution: static_dispatch.CheckedCallResolution) ?static_dispatch.IteratorProcedureId {
        const table = self.view.view.static_dispatch_plans;
        const node = switch (resolution) {
            .direct_closed, .direct_parametric => |direct| table.evidenceNode(direct.evidence),
            .evidence_dependent, .checked_error, .@"unreachable", .structural, .direct_pending => return null,
        };
        return switch (node.target.kind) {
            .procedure => |procedure| procedure.runtime_target.iteratorProcedure(),
            .local_proc, .structural, .callable_binding => null,
        };
    }

    pub const ChainCall = struct { stamp: static_dispatch.IteratorProcedureId, operands: []const ExprId };

    /// The stamped producer call `expr` is: a dispatch of a value result on
    /// the pure `Iter` protocol whose target the checker stamped, with its
    /// checked operand expressions (the receiver first). Null for any other
    /// expression.
    fn chainCall(self: *Builder, expr: ExprId) Allocator.Error!?ChainCall {
        const plan_id = switch (self.bodies().exprData(expr)) {
            .dispatch_call, .type_dispatch_call => self.bodies().dispatchPlan(@backingInt(expr)) orelse return null,
            .numeral, .str_segment, .str, .bytes_literal, .lookup_local, .lookup_external, .lookup_required, .list, .empty_list, .tuple, .match_, .if_, .call, .record, .empty_record, .block, .tag, .nominal, .zero_argument_tag, .closure, .lambda, .binop, .unary_minus, .field_access, .interpolation, .structural_eq, .structural_hash, .method_eq, .tuple_access, .runtime_error, .crash, .dbg, .expect_err, .expect, .ellipsis, .anno_only, .break_, .return_, .for_, .hosted_lambda, .run_low_level => return null,
        };
        const call_plan = self.dispatchPlan(plan_id);
        // A `map` dispatch on a type with a map method carries the map
        // result mode, which a direct resolution calls as a value does.
        switch (call_plan.result_mode) {
            .value, .map => {},
            .equality, .hash, .parser_for, .encoder_for, .map_effectful => return null,
        }
        const stamp = self.dispatchStamp(call_plan.resolution) orelse return null;
        switch (self.program.store.get(try self.exprType(expr))) {
            .nominal => |nominal| if ((nominal.builtin orelse return null) != .iter) return null,
            .primitive, .empty_record, .record, .tuple, .tag_union, .list, .box, .function, .variable => return null,
        }
        const operands = call_plan.argsSlice(self.view.view.static_dispatch_plans);
        const exprs = try self.program.arena.allocator().alloc(ExprId, operands.len);
        for (operands, exprs) |operand, *slot| {
            slot.* = switch (operand) {
                .checked_expr => |operand_expr| operand_expr,
                .generated_interpolation_segments, .generated_numeral, .generated_quote, .literal_pattern_value, .literal_pattern_conversion => return null,
            };
        }
        const expected_operands: usize = switch (stamp) {
            .identity, .list_iter, .list_iter_rev, .str_iter_utf8 => 1,
            .map, .keep_if, .drop_if, .take_first, .drop_first, .with_index => 2,
            .next, .custom, .single, .concat, .append, .step_by, .from_iter, .range_iter, .numeric_range_iter, .numeric_to, .numeric_until, .from_step, .range_done => return .{ .stamp = stamp, .operands = exprs },
        };
        if (exprs.len != expected_operands) invariant("a stamped iterator producer's checked operand count differs from its definition's");
        return .{ .stamp = stamp, .operands = exprs };
    }

    /// The item type of the iterator nominal `iterator_ty`: its one
    /// argument.
    fn iteratorItemType(self: *Builder, iterator_ty: TypeId) Allocator.Error!TypeId {
        const args = switch (self.program.store.get(iterator_ty)) {
            .nominal => |nominal| self.program.store.nominalArgs(nominal),
            .primitive, .empty_record, .record, .tuple, .tag_union, .list, .box, .function, .variable => invariant("an iterator's type is not the iterator nominal"),
        };
        if (args.len != 1) invariant("the iterator nominal has one argument");
        return args[0];
    }

    /// A `u64` local holding `value`.
    fn u64Literal(self: *Builder, value: u64) Allocator.Error!LocalId {
        const local = try self.closedLocal(.u64);
        try self.emitLinear(.{ .assign_literal = .{ .target = local, .value = .{ .i64_literal = .{ .value = @bitCast(value), .layout_idx = .u64 } }, .next = undefined } });
        return local;
    }

    /// `lhs op rhs` for `u64` operands.
    fn u64Op(self: *Builder, op: LIR.LowLevel, lhs: LocalId, rhs: LocalId) Allocator.Error!LocalId {
        const local = try self.closedLocal(if (op == .num_is_eq) .bool else .u64);
        try self.emitLowLevel(local, op, &.{ lhs, rhs });
        return local;
    }

    /// `for pattern in <chain> { body }` over a structural chain: the loop
    /// carries the chain's state (a list source's index, a reversed list
    /// source's remaining count, an iterator source's value, an adapter's
    /// counter) in one tuple, and each iteration steps the source and runs
    /// the adapters over its item, innermost first, exactly as each
    /// producer's step does: a `take_first` at its count is done before the
    /// source steps, a `keep_if` or `drop_if` or `drop_first` skip
    /// continues with the state the inner producers advanced to, and a
    /// `with_index` counter advances only when its item is yielded.
    fn forChainStart(self: *Builder, loop_node: u32, pattern: PatternId, body: ExprId, plan_id: static_dispatch.IteratorForPlanId, chain: []const ChainLayer) Allocator.Error!void {
        const table = self.view.view.static_dispatch_plans;
        const plan = table.iterator_for_plans[@backingInt(plan_id)];
        const for_site = plan.forLoopSite();
        const store = &self.program.store;
        const names = self.view.nameStore();

        // The chain's operands, in chain order.
        const Evaluated = union(enum) {
            list: struct { list: LocalId, list_ty: TypeId, item_ty: TypeId, len: LocalId },
            str: struct { str: LocalId, len: LocalId },
            value: struct { iterator_ty: TypeId, step_name: types.NameId },
            map: struct { transform: LocalId, fn_ty: TypeId, item_ty: TypeId },
            keep_if: struct { predicate: LocalId, fn_ty: TypeId },
            drop_if: struct { predicate: LocalId, fn_ty: TypeId },
            with_index: struct { item_ty: TypeId },
            take_first,
            drop_first,
        };
        const evaluated = try self.allocator.alloc(Evaluated, chain.len);
        defer self.allocator.free(evaluated);
        // The loop's state tuple: each layer's carried slot, and its initial
        // value.
        var state_types: std.ArrayList(TypeId) = .empty;
        defer state_types.deinit(self.allocator);
        var state_init: std.ArrayList(LocalId) = .empty;
        defer state_init.deinit(self.allocator);
        const slot_of = try self.allocator.alloc(?usize, chain.len);
        defer self.allocator.free(slot_of);
        const u64_ty = try store.primitive(.u64);
        for (chain, evaluated, slot_of) |layer, *done, *slot| {
            slot.* = null;
            switch (layer) {
                inline .list, .list_rev => |source, tag| {
                    const list = try self.lowerNow(.{ .expr = source.list });
                    const list_ty = try self.exprType(source.list);
                    const len = try self.closedLocal(.u64);
                    try self.emitListLowLevel(len, .list_len, &.{list}, source.item_ty, list_ty);
                    done.* = .{ .list = .{ .list = list, .list_ty = list_ty, .item_ty = source.item_ty, .len = len } };
                    slot.* = state_types.items.len;
                    try state_types.append(self.allocator, u64_ty);
                    try state_init.append(self.allocator, if (tag == .list) try self.u64Literal(0) else len);
                },
                .str => |source| {
                    const str = try self.lowerNow(.{ .expr = source.str });
                    const len = try self.closedLocal(.u64);
                    try self.emitLowLevel(len, .str_count_utf8_bytes, &.{str});
                    done.* = .{ .str = .{ .str = str, .len = len } };
                    slot.* = state_types.items.len;
                    try state_types.append(self.allocator, u64_ty);
                    try state_init.append(self.allocator, try self.u64Literal(0));
                },
                .value => |source| {
                    const iterator = try self.lowerNow(.{ .expr = source.iterator });
                    const iterator_ty = try self.exprType(source.iterator);
                    const step_name = (try self.builtinStepField(plan.next, iterator_ty)) orelse invariant("a chain's iterator value is not stepped by the builtin protocol");
                    done.* = .{ .value = .{ .iterator_ty = iterator_ty, .step_name = step_name } };
                    slot.* = state_types.items.len;
                    try state_types.append(self.allocator, iterator_ty);
                    try state_init.append(self.allocator, iterator);
                },
                .map => |adapter| {
                    const transform = try self.lowerNow(.{ .expr = adapter.transform });
                    done.* = .{ .map = .{ .transform = transform, .fn_ty = try self.exprType(adapter.transform), .item_ty = adapter.item_ty } };
                },
                .keep_if => |adapter| {
                    const predicate = try self.lowerNow(.{ .expr = adapter.predicate });
                    done.* = .{ .keep_if = .{ .predicate = predicate, .fn_ty = try self.exprType(adapter.predicate) } };
                },
                .drop_if => |adapter| {
                    const predicate = try self.lowerNow(.{ .expr = adapter.predicate });
                    done.* = .{ .drop_if = .{ .predicate = predicate, .fn_ty = try self.exprType(adapter.predicate) } };
                },
                .with_index => |adapter| {
                    const start = try self.lowerNow(.{ .expr = adapter.start });
                    done.* = .{ .with_index = .{ .item_ty = adapter.item_ty } };
                    slot.* = state_types.items.len;
                    try state_types.append(self.allocator, u64_ty);
                    try state_init.append(self.allocator, start);
                },
                inline .take_first, .drop_first => |adapter, tag| {
                    const count = try self.lowerNow(.{ .expr = adapter.count });
                    done.* = if (tag == .take_first) .take_first else .drop_first;
                    slot.* = state_types.items.len;
                    try state_types.append(self.allocator, u64_ty);
                    try state_init.append(self.allocator, count);
                },
            }
        }
        const state_ty = try store.intern(.{ .tuple = state_types.items });
        const initial = try self.buildStruct(state_ty, state_init.items);

        const binders = try self.bodies().loopMutationBinders(loop_node);
        const exit_merge = try self.beginMerge(binders, .joined);
        const head_merge = try self.beginMerge(binders, .joined);
        const exit = self.freshJoin();
        const head = self.freshJoin();
        const state = try self.localOf(state_ty, .frame);
        const after = try self.declareMergeJoin(exit, null, exit_merge);
        const head_body = try self.declareMergeJoin(head, state, head_merge);
        try self.emitJump(head, initial);
        self.store.shapes.loop = true;

        self.hole = head_body;
        if (head_merge) |m| try self.enterMerge(m);
        // The state this iteration advances to, slot by slot.
        const current = try self.allocator.alloc(LocalId, state_types.items.len);
        defer self.allocator.free(current);
        for (current, state_types.items, 0..) |*slot, slot_ty, index| slot.* = try self.readField(state_ty, state, @intCast(index), slot_ty);

        // A `take_first` at its count is done before anything steps.
        for (chain, slot_of) |layer, slot| {
            if (layer != .take_first) continue;
            const remaining = current[slot.?];
            const is_done = try self.u64Op(.num_is_eq, remaining, try self.u64Literal(0));
            const holes = try self.emitBoolSwitch(is_done);
            self.hole = holes.then;
            try self.emitJump(exit, null);
            self.hole = holes.otherwise;
        }

        // The source's item.
        var item: TypedValue = undefined;
        switch (evaluated[0]) {
            .list => |source| {
                const index = current[slot_of[0].?];
                const reversed = chain[0] == .list_rev;
                const is_done = try self.u64Op(.num_is_eq, index, if (reversed) try self.u64Literal(0) else source.len);
                const holes = try self.emitBoolSwitch(is_done);
                self.hole = holes.then;
                try self.emitJump(exit, null);
                self.hole = holes.otherwise;
                const one = try self.u64Literal(1);
                const at = if (reversed) try self.u64Op(.num_int_sub_wrap, index, one) else index;
                const value = try self.localOf(source.item_ty, .frame);
                try self.emitListLowLevel(value, .list_get_unsafe, &.{ source.list, at }, source.item_ty, source.list_ty);
                item = .{ .value = value, .ty = source.item_ty };
                current[slot_of[0].?] = if (reversed) at else try self.u64Op(.num_int_add_wrap, index, one);
            },
            .str => |source| {
                const index = current[slot_of[0].?];
                const is_done = try self.u64Op(.num_is_eq, index, source.len);
                const holes = try self.emitBoolSwitch(is_done);
                self.hole = holes.then;
                try self.emitJump(exit, null);
                self.hole = holes.otherwise;
                const value = try self.closedLocal(.u8);
                try self.emitLowLevel(value, .str_get_utf8_byte_unsafe, &.{ source.str, index });
                item = .{ .value = value, .ty = try store.primitive(.u8) };
                current[slot_of[0].?] = try self.u64Op(.num_int_add_wrap, index, try self.u64Literal(1));
            },
            .value => |source| {
                const iterator = current[slot_of[0].?];
                const thunk = try self.recordFieldValue(iterator, source.iterator_ty, source.step_name);
                const step_ty = switch (store.get(thunk.ty)) {
                    .function => |function| function.ret,
                    .primitive, .empty_record, .record, .tuple, .tag_union, .list, .box, .nominal, .variable => invariant("an iterator's step field is not a function"),
                };
                const step_value = try self.emitErasedCall(thunk.value, thunk.ty, &.{}, step_ty);
                const done_name = try store.internName(names.tagLabelText(for_site.step_topology.done_tag));
                const skip_name = try store.internName(names.tagLabelText(for_site.step_topology.skip_tag));
                const one_name = try store.internName(names.tagLabelText(for_site.step_topology.one_tag));
                const item_name = try store.internName(names.recordFieldLabelText(for_site.step_topology.item_field));
                const rest_name = try store.internName(names.recordFieldLabelText(for_site.step_topology.rest_field));
                const not_done_hole = try self.newHole();
                try self.branchOnTag(step_value, step_ty, done_name, not_done_hole);
                try self.emitJump(exit, null);
                self.hole = not_done_hole;
                const one_hole = try self.newHole();
                try self.branchOnTag(step_value, step_ty, skip_name, one_hole);
                {
                    const payload = try self.tagPayloadValue(step_value, step_ty, skip_name, 0);
                    const rest = try self.recordFieldValue(payload.value, payload.ty, rest_name);
                    current[slot_of[0].?] = rest.value;
                    try self.emitJump(head, try self.buildStruct(state_ty, current));
                }
                self.hole = one_hole;
                const payload = try self.tagPayloadValue(step_value, step_ty, one_name, 0);
                item = try self.recordFieldValue(payload.value, payload.ty, item_name);
                current[slot_of[0].?] = (try self.recordFieldValue(payload.value, payload.ty, rest_name)).value;
            },
            .map, .keep_if, .drop_if, .with_index, .take_first, .drop_first => invariant("a structural chain does not start with a source"),
        }

        // The adapters, innermost first.
        for (evaluated[1..], slot_of[1..]) |layer, slot| {
            switch (layer) {
                .map => |adapter| item = .{ .value = try self.emitErasedCall(adapter.transform, adapter.fn_ty, &.{item.value}, adapter.item_ty), .ty = adapter.item_ty },
                inline .keep_if, .drop_if => |adapter, tag| {
                    const verdict = try self.emitErasedCall(adapter.predicate, adapter.fn_ty, &.{item.value}, try store.primitive(.bool));
                    const holes = try self.emitBoolSwitch(verdict);
                    // A rejected item is skipped: the loop continues with the
                    // state the inner producers advanced to.
                    self.hole = if (tag == .keep_if) holes.otherwise else holes.then;
                    try self.emitJump(head, try self.buildStruct(state_ty, current));
                    self.hole = if (tag == .keep_if) holes.then else holes.otherwise;
                },
                .with_index => |adapter| {
                    const index = current[slot.?];
                    item = .{ .value = try self.buildStruct(adapter.item_ty, &.{ index, item.value }), .ty = adapter.item_ty };
                    current[slot.?] = try self.u64Op(.num_int_add_wrap, index, try self.u64Literal(1));
                },
                .take_first => current[slot.?] = try self.u64Op(.num_int_sub_wrap, current[slot.?], try self.u64Literal(1)),
                .drop_first => {
                    const pending = current[slot.?];
                    const yields = try self.u64Op(.num_is_eq, pending, try self.u64Literal(0));
                    const holes = try self.emitBoolSwitch(yields);
                    self.hole = holes.otherwise;
                    current[slot.?] = try self.u64Op(.num_int_sub_wrap, pending, try self.u64Literal(1));
                    try self.emitJump(head, try self.buildStruct(state_ty, current));
                    current[slot.?] = pending;
                    self.hole = holes.then;
                },
                .list, .str, .value => invariant("a structural chain has one source, first"),
            }
        }

        const next_state = try self.buildStruct(state_ty, current);
        try self.bindIrrefutable(pattern, item.value, item.ty);
        try self.loops.append(self.allocator, .{ .join = exit, .reached = true });
        try self.push(.end_loop);
        if (exit_merge) |m| try self.push(.{ .enter_merge = m });
        try self.push(.{ .resume_at = after });
        try self.push(.{ .jump = head });
        try self.push(.{ .push_value = next_state });
        try self.push(.discard);
        try self.push(.{ .expr = body });
    }

    /// `for pattern in iterable { body }`: the iterable's iterator is the
    /// loop's state; each step calls `next` and matches its Done, Skip, or
    /// One tag (design.md "Iterator `for`").
    pub fn forStart(self: *Builder, loop_node: u32, pattern: PatternId, body: ExprId, plan_id: static_dispatch.IteratorForPlanId) Allocator.Error!void {
        const table = self.view.view.static_dispatch_plans;
        const plan = table.iterator_for_plans[@backingInt(plan_id)];
        const iterable = self.popValue();
        const iterator_ty = try self.convert(plan.iterator_ty);
        const for_site = plan.forLoopSite();
        const step_ty = try self.convert(for_site.step_ty);
        const iterator = try self.emitDispatch(plan.iter.resolution, plan.iter.callable_ty, &.{iterable}, iterator_ty);

        const binders = try self.bodies().loopMutationBinders(loop_node);
        const exit_merge = try self.beginMerge(binders, .joined);
        const head_merge = try self.beginMerge(binders, .joined);
        const exit = self.freshJoin();
        const head = self.freshJoin();
        const state = try self.localOf(iterator_ty, .frame);
        const after = try self.declareMergeJoin(exit, null, exit_merge);
        const head_body = try self.declareMergeJoin(head, state, head_merge);
        try self.emitJump(head, iterator);
        self.store.shapes.loop = true;

        self.hole = head_body;
        if (head_merge) |m| try self.enterMerge(m);
        const step_value = if (try self.builtinStepField(plan.next, iterator_ty)) |step_name| blk: {
            const thunk = try self.recordFieldValue(state, iterator_ty, step_name);
            break :blk try self.emitErasedCall(thunk.value, thunk.ty, &.{}, step_ty);
        } else try self.emitDispatch(plan.next.resolution, plan.next.callable_ty, &.{state}, step_ty);
        const names = self.view.nameStore();
        const done = try self.program.store.internName(names.tagLabelText(for_site.step_topology.done_tag));
        const skip = try self.program.store.internName(names.tagLabelText(for_site.step_topology.skip_tag));
        const one = try self.program.store.internName(names.tagLabelText(for_site.step_topology.one_tag));
        const item_name = try self.program.store.internName(names.recordFieldLabelText(for_site.step_topology.item_field));
        const rest_name = try self.program.store.internName(names.recordFieldLabelText(for_site.step_topology.rest_field));

        // Done: leave the loop.
        const not_done_hole = try self.newHole();
        try self.branchOnTag(step_value, step_ty, done, not_done_hole);
        try self.emitJump(exit, null);
        // Skip: continue with its rest.
        self.hole = not_done_hole;
        const one_hole = try self.newHole();
        try self.branchOnTag(step_value, step_ty, skip, one_hole);
        {
            const payload = try self.tagPayloadValue(step_value, step_ty, skip, 0);
            const rest = try self.recordFieldValue(payload.value, payload.ty, rest_name);
            try self.emitJump(head, rest.value);
        }
        // One: bind the item, run the body, continue with its rest.
        self.hole = one_hole;
        const payload = try self.tagPayloadValue(step_value, step_ty, one, 0);
        const item = try self.recordFieldValue(payload.value, payload.ty, item_name);
        const rest = try self.recordFieldValue(payload.value, payload.ty, rest_name);
        try self.bindIrrefutable(pattern, item.value, item.ty);
        try self.loops.append(self.allocator, .{ .join = exit, .reached = true });
        try self.push(.end_loop);
        if (exit_merge) |m| try self.push(.{ .enter_merge = m });
        try self.push(.{ .resume_at = after });
        try self.push(.{ .jump = head });
        try self.push(.{ .push_value = rest.value });
        try self.push(.discard);
        try self.push(.{ .expr = body });
    }

    /// The step field of a `for` loop's iterator when the loop's `next`
    /// dispatch resolves to the procedure the checker stamped
    /// `IteratorProcedureId.next` of the pure `Iter` protocol, which is
    /// `|iterator| (iterator.step)()`: the loop then calls the step thunk
    /// itself, owning the iterator it dismantles, so the call transfers the
    /// thunk's allocation to it (design.md "Iterator `for`"). Null for
    /// every other `next`, which the loop dispatches to.
    fn builtinStepField(self: *Builder, next: static_dispatch.IteratorDispatchCall, iterator_ty: TypeId) Allocator.Error!?types.NameId {
        const table = self.view.view.static_dispatch_plans;
        const node = switch (next.resolution) {
            .direct_closed, .direct_parametric => |direct| table.evidenceNode(direct.evidence),
            .evidence_dependent, .checked_error, .@"unreachable", .structural, .direct_pending => return null,
        };
        const stamp = switch (node.target.kind) {
            .procedure => |procedure| procedure.runtime_target.iteratorProcedure(),
            .local_proc, .structural, .callable_binding => return null,
        };
        if (stamp != .next) return null;
        const owner: static_dispatch.IteratorOwner = switch (self.program.store.get(iterator_ty)) {
            .nominal => |nominal| switch (nominal.builtin orelse return null) {
                .iter => .iter,
                .stream, .bool, .try_, .str, .u8, .i8, .u16, .i16, .u32, .i32, .u64, .i64, .u128, .i128, .f32, .f64, .dec, .u8x16, .i8x16, .u16x8, .i16x8, .u32x4, .i32x4, .u64x2, .i64x2, .list, .box, .dict, .set, .parse_tag_union_spec, .fields, .field, .crypto_sha256_digest, .crypto_sha256_hasher, .crypto_blake3_digest, .crypto_blake3_hasher => return null,
            },
            .primitive, .empty_record, .record, .tuple, .tag_union, .list, .box, .function, .variable => return null,
        };
        const topologies = table.iterator_topologies;
        if (topologies.len != std.enums.values(static_dispatch.IteratorOwner).len) invariant("a checked module omitted an iterator representation topology");
        const topology = topologies[@backingInt(owner)];
        return try self.program.store.internName(self.view.nameStore().recordFieldLabelText(topology.step_field));
    }

    /// Continue at the next statement when `value` (of `ty`) holds tag
    /// `name`; otherwise continue at `miss`.
    pub fn branchOnTag(self: *Builder, value: LocalId, raw_ty: TypeId, name: types.NameId, miss: CFStmtId) Allocator.Error!void {
        const ty = try self.structuralType(raw_ty);
        const actual = try self.closedLocal(.u32);
        const expected = try self.closedLocal(.u32);
        if (try self.closed(ty)) {
            const variant = try self.closedVariant(ty, name);
            const committed = self.program.result.layouts.getLayout(try self.program.layouts.layoutOf(ty));
            if (committed.tag == .tag_union) {
                try self.emitLinear(.{ .assign_ref = .{ .target = actual, .op = LIR.RefOp.discriminant(value), .next = undefined } });
            } else {
                try self.emitLinear(.{ .assign_literal = .{ .target = actual, .value = .{ .i64_literal = .{ .value = 0, .layout_idx = .u32 } }, .next = undefined } });
            }
            try self.emitLinear(.{ .assign_literal = .{ .target = expected, .value = .{ .i64_literal = .{ .value = @intCast(variant.index), .layout_idx = .u32 } }, .next = undefined } });
        } else {
            try self.emitDyn(actual, .{ .tag_discriminant = .{ .base = value, .desc = try self.descOf(ty) } });
            try self.assignIndex(expected, try self.tagIndex(ty, name));
        }
        const matches = try self.closedLocal(.bool);
        try self.emitLowLevel(matches, .num_is_eq, &.{ actual, expected });
        try self.branchOrMiss(matches, miss);
    }

    pub const ClosedVariant = struct { index: usize, payloads: []const TypeId };

    pub fn closedVariant(self: *Builder, ty: TypeId, name: types.NameId) Allocator.Error!ClosedVariant {
        const own = switch (self.program.store.get(ty)) {
            .tag_union => |union_ty| for (self.program.store.tagSpan(union_ty.tags)) |tag| {
                if (tag.name == name) break tag;
            } else invariant("a tag test's tag is absent from its type"),
            .primitive, .empty_record, .record, .tuple, .list, .box, .nominal, .function, .variable => invariant("a tag test's value is not a tag union"),
        };
        return try self.selectVariant(ty, own);
    }

    /// The variant of the closed tag union `ty` that its tag `own` is: the
    /// one with its label and arity, which a checked row lists once.
    pub fn selectVariant(self: *Builder, ty: TypeId, own: types.Tag) Allocator.Error!ClosedVariant {
        const tags = switch (self.program.store.get(ty)) {
            .tag_union => |union_ty| self.program.store.tagSpan(union_ty.tags),
            .primitive, .empty_record, .record, .tuple, .list, .box, .nominal, .function, .variable => invariant("a tag's type is not a tag union"),
        };
        for (tags, 0..) |tag, index| {
            if (tag.name != own.name or tag.payloads.len != own.payloads.len) continue;
            return .{ .index = index, .payloads = self.program.store.span(tag.payloads) };
        }
        invariant("a tag is absent from its row");
    }

    /// The tag of `ty` with label `name` and `arity` payloads.
    pub fn ownTag(self: *Builder, ty: TypeId, name: types.NameId, arity: usize) types.Tag {
        const union_ty = switch (self.program.store.get(ty)) {
            .tag_union => |union_ty| union_ty,
            .primitive, .empty_record, .record, .tuple, .list, .box, .nominal, .function, .variable => invariant("a tag's type is not a tag union"),
        };
        for (self.program.store.tagSpan(union_ty.tags)) |tag| {
            if (tag.name == name and tag.payloads.len == arity) return tag;
        }
        invariant("a tag is absent from its type");
    }

    pub const TypedValue = struct { value: LocalId, ty: TypeId };

    /// Payload `index` of tag `name` in `value`, which holds that tag.
    pub fn tagPayloadValue(self: *Builder, value: LocalId, raw_ty: TypeId, name: types.NameId, index: u32) Allocator.Error!TypedValue {
        const ty = try self.structuralType(raw_ty);
        if (!try self.closed(ty)) {
            const union_ty = self.program.store.get(ty).tag_union;
            const payload_ty = for (self.program.store.tagSpan(union_ty.tags)) |tag| {
                if (tag.name == name) break self.program.store.span(tag.payloads)[index];
            } else invariant("a tag payload's tag is absent from its type");
            const local = try self.localOf(payload_ty, .view);
            try self.emitDyn(local, .{ .tag_payload_addr = .{ .base = value, .desc = try self.descOf(ty), .payload_index = index } });
            return .{ .value = local, .ty = payload_ty };
        }
        const variant = try self.closedVariant(ty, name);
        const payload_ty = variant.payloads[index];
        return .{ .value = try self.readTagPayload(value, ty, variant.index, variant.payloads, index), .ty = payload_ty };
    }

    /// The committed layout a closed tag union stores variant `variant`'s
    /// payload in.
    pub fn variantPayloadLayout(self: *Builder, union_ty: TypeId, variant: usize) Allocator.Error!layout.Idx {
        const layouts = &self.program.result.layouts;
        const committed = layouts.getLayout(try self.program.layouts.layoutOf(union_ty));
        if (committed.tag != .tag_union) invariant("a stored tag payload's union has no storage");
        return layouts.getTagUnionInfo(committed).variants.get(variant).payload_layout;
    }

    /// Whether a component of type `ty` stored at `slot` is held in a box
    /// the committed layout inserted to break a recursive type.
    pub fn isRecursionBox(self: *Builder, slot: layout.Idx, ty: TypeId) Allocator.Error!bool {
        return try self.program.layouts.isRecursionBox(slot, ty);
    }

    /// `value` (of `ty`) as it is stored at `slot`: boxed when the slot is a
    /// recursion box.
    pub fn intoSlot(self: *Builder, value: LocalId, ty: TypeId, slot: layout.Idx) Allocator.Error!LocalId {
        if (!try self.isRecursionBox(slot, ty)) return value;
        const boxed = try self.closedLocal(slot);
        try self.emitLowLevel(boxed, .box_box, &.{value});
        return boxed;
    }

    /// Read the component at `op`, stored at `slot`, as a value of `ty`:
    /// unboxed when the slot is a recursion box.
    pub fn readSlot(self: *Builder, op: LIR.RefOp, ty: TypeId, slot: layout.Idx) Allocator.Error!LocalId {
        const local = try self.localOf(ty, .frame);
        try self.readSlotInto(local, op, ty, slot);
        return local;
    }

    /// `readSlot` into `target`.
    pub fn readSlotInto(self: *Builder, target: LocalId, op: LIR.RefOp, ty: TypeId, slot: layout.Idx) Allocator.Error!void {
        if (!try self.isRecursionBox(slot, ty)) {
            try self.emitLinear(.{ .assign_ref = .{ .target = target, .op = op, .next = undefined } });
            return;
        }
        const boxed = try self.closedLocal(slot);
        try self.emitLinear(.{ .assign_ref = .{ .target = boxed, .op = op, .next = undefined } });
        try self.emitLowLevel(target, .box_unbox, &.{boxed});
    }

    /// Payload `index` of variant `variant` (with payload types `payloads`)
    /// of the closed tag union `value` of type `union_ty`.
    pub fn readTagPayload(self: *Builder, value: LocalId, union_ty: TypeId, variant: usize, payloads: []const TypeId, index: usize) Allocator.Error!LocalId {
        const local = try self.localOf(payloads[index], .frame);
        try self.readTagPayloadInto(local, value, union_ty, variant, payloads, index);
        return local;
    }

    /// `readTagPayload` into `target`.
    pub fn readTagPayloadInto(self: *Builder, target: LocalId, value: LocalId, union_ty: TypeId, variant: usize, payloads: []const TypeId, index: usize) Allocator.Error!void {
        const payload_layout = try self.variantPayloadLayout(union_ty, variant);
        const whole = LIR.RefOp.tagPayloadStruct(value, @intCast(variant), @intCast(variant));
        if (payloads.len == 1) return try self.readSlotInto(target, whole, payloads[0], payload_layout);
        const payload_struct = try self.closedLocal(payload_layout);
        try self.emitLinear(.{ .assign_ref = .{ .target = payload_struct, .op = whole, .next = undefined } });
        const field_layout = self.structFieldLayout(payload_layout, @intCast(index));
        try self.readSlotInto(target, LIR.RefOp.field(payload_struct, @intCast(index)), payloads[index], field_layout);
    }

    /// The committed layout of field `index` (by checked field index) of the
    /// struct layout `struct_layout`. Every field of a zero-sized struct is
    /// zero-sized.
    pub fn structFieldLayout(self: *Builder, struct_layout: layout.Idx, index: u32) layout.Idx {
        const layouts = &self.program.result.layouts;
        const committed = layouts.getLayout(struct_layout);
        return switch (committed.tag) {
            .struct_ => layouts.getStructFieldLayoutByOriginalIndex(committed.getStruct().idx, index),
            .zst => .zst,
            .scalar, .box, .box_of_zst, .list, .list_of_zst, .closure, .erased_callable, .tag_union, .ptr, .hole, .dynamic => invariant("a component read names a field of a layout that is neither a struct nor zero-sized"),
        };
    }

    /// Field `name` of the record `value`.
    pub fn recordFieldValue(self: *Builder, value: LocalId, raw_ty: TypeId, name: types.NameId) Allocator.Error!TypedValue {
        const ty = try self.structuralType(raw_ty);
        const record = switch (self.program.store.get(ty)) {
            .record => |record| record,
            .primitive, .empty_record, .tuple, .tag_union, .list, .box, .nominal, .function, .variable => invariant("a record field read's value is not a record"),
        };
        const fields = self.program.store.fieldSpan(record.fields);
        const index = for (fields, 0..) |field, i| {
            if (field.name == name) break i;
        } else invariant("a record field read's field is absent from its type");
        const field_ty = fields[index].ty;
        return .{ .value = try self.readField(ty, value, @intCast(index), field_ty), .ty = field_ty };
    }

    /// Call the target a dispatch resolution names with already-lowered
    /// arguments.
    pub fn emitDispatch(self: *Builder, resolution: static_dispatch.CheckedCallResolution, callable_ty: checked.CheckedTypeId, args: []const LocalId, result_ty: TypeId) Allocator.Error!LocalId {
        const table = self.view.view.static_dispatch_plans;
        switch (resolution) {
            .direct_closed, .direct_parametric => |direct| {
                const node = table.evidenceNode(direct.evidence);
                if (node.target.kind == .local_proc) {
                    const fn_ty = try self.dispatchCallable(resolution, callable_ty);
                    return try self.emitErasedCall(try self.localMethodValue(node, fn_ty), fn_ty, args, result_ty);
                }
                if (node.target.kind == .callable_binding) {
                    const fn_ty = try self.dispatchCallable(resolution, callable_ty);
                    return try self.emitErasedCall(try self.callableBindingMethodValue(node.target.kind.callable_binding, fn_ty), fn_ty, args, result_ty);
                }
                const callee = try self.methodTargetWorker(node.target);
                const subst = table.site_substitutions[node.subst.start..][0..node.subst.len];
                const call_fn = try self.dispatchCallable(resolution, callable_ty);
                // Reached through a procedure alias, the target takes the
                // instantiation the call's callable type gives it.
                const instance = if (node.instantiation == .alias) try self.aliasTargetInstance(node, callee, .{ .node = direct.evidence }) else switch (node.nested) {
                    .resolved => try self.callInstanceAt(callee, subst, table.nestedEvidence(node), call_fn, .{ .node = direct.evidence }),
                    .from_callable => try self.callableDerivedInstance(callee, subst, call_fn),
                    .through_alias => invariant("alias-scope evidence accompanies only an alias instantiation"),
                };
                defer self.allocator.free(instance.hidden);
                const worker_id = try self.program.directCallTarget(callee, instance.bound);
                const all = try self.callArgs(args, worker_id, instance.hidden);
                defer self.allocator.free(all);
                const target = try self.localOf(instance.ret, .frame);
                try self.emitLinear(.{ .assign_call = .{ .target = target, .proc = self.program.worker(worker_id).proc, .args = try self.store.addLocalSpan(all), .next = undefined } });
                return try self.adaptValue(target, instance.ret, result_ty);
            },
            .evidence_dependent => |dependent| {
                const fn_ty = (try self.scopeInterface(dependent.index)) orelse try self.convert(callable_ty);
                return try self.emitErasedCall(try self.materialize(try self.scopeDictionary(dependent.index)), fn_ty, args, result_ty);
            },
            .checked_error, .@"unreachable" => {
                try self.emitDispatchCrash(if (resolution == .checked_error) .checked_error else .unreachable_value);
                return try self.localOf(result_ty, .frame);
            },
            .structural => invariant("an iterator dispatch resolved to a structural derivation, which no iterator method has"),
            .direct_pending => invariant("a checked module holds a pending dispatch resolution"),
        }
    }

    pub fn loopTest(self: *Builder, body: ExprId, head: LIR.JoinPointId, exit: LIR.JoinPointId) Allocator.Error!void {
        const holes = try self.emitBoolSwitch(self.popValue());
        self.hole = holes.otherwise;
        try self.emitJump(exit, null);
        self.hole = holes.then;
        try self.push(.{ .jump_without_value = head });
        try self.push(.discard);
        try self.push(.{ .expr = body });
    }

    /// Bind an irrefutable pattern (a parameter or a `decl`) to `value`.
    /// Destructuring binds through the same tests a match uses; an
    /// irrefutable pattern never takes their miss path.
    pub fn bindIrrefutable(self: *Builder, pattern_id: PatternId, value: LocalId, ty: TypeId) Allocator.Error!void {
        const miss = try self.newHole();
        try self.testPattern(pattern_id, value, ty, miss);
    }
};
