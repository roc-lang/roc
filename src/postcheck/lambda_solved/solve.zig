//! Lambda solving over lifted Monotype IR.

const std = @import("std");
const TypeDigestHasher = @import("base").TypeDigestHasher;
const collections = @import("collections");
const can = @import("can");
const check = @import("check");

const Common = @import("../common.zig");
const AnyAll = collections.AnyAll;
const MonoType = @import("../monotype/type.zig");
const Lifted = @import("../monotype_lifted/ast.zig");
const Ast = @import("ast.zig");
const Type = @import("type.zig");

const Allocator = std.mem.Allocator;
const static_dispatch = check.StaticDispatchRegistry;
const names = check.CheckedNames;

const UninhabitedAnswer = struct {
    epoch: u64,
    uninhabited: bool,
};

const UnifyPair = struct {
    first: Type.TypeVarId,
    second: Type.TypeVarId,

    fn init(lhs: Type.TypeVarId, rhs: Type.TypeVarId) UnifyPair {
        return if (lhs.is_gt(rhs))
            .{ .first = rhs, .second = lhs }
        else
            .{ .first = lhs, .second = rhs };
    }
};

/// Unification checks one pair set entry per structural step, so the pair
/// sets hash the two variable ids directly.
const UnifyPairContext = struct {
    pub fn hash(_: UnifyPairContext, pair: UnifyPair) u64 {
        return std.hash.int((@as(u64, @intFromEnum(pair.first)) << 32) | @intFromEnum(pair.second));
    }

    pub fn eql(_: UnifyPairContext, a: UnifyPair, b: UnifyPair) bool {
        return a.first == b.first and a.second == b.second;
    }
};

const UnifyPairSet = std.HashMap(UnifyPair, void, UnifyPairContext, std.hash_map.default_max_load_percentage);

/// The store writes a unification defers until every type it pushed onto the
/// unify stack has been processed.
const UnifyFinishAction = union(enum) {
    none,
    link_rhs_to_lhs: struct {
        lhs: Type.TypeVarId,
        rhs: Type.TypeVarId,
    },
    link_var_to_root: struct {
        var_: Type.TypeVarId,
        target: Type.TypeVarId,
    },
    link_structural_to_inspectable_named: struct {
        structural: Type.TypeVarId,
        named: Type.TypeVarId,
    },
    set_left_erased_link_right: struct {
        lhs: Type.TypeVarId,
        rhs: Type.TypeVarId,
        source_fn_ty: Type.names.TypeDigest,
        members: Type.Span,
        abi_fn: ?Type.TypeVarId,
    },
    set_left_lambda_set_link_right: struct {
        lhs: Type.TypeVarId,
        rhs: Type.TypeVarId,
        members: Type.Span,
    },
    set_left_tag_union_link_right: struct {
        lhs: Type.TypeVarId,
        rhs: Type.TypeVarId,
        tags: Type.Span,
    },
};

const UnifyFrame = union(enum) {
    process: struct {
        lhs: Type.TypeVarId,
        rhs: Type.TypeVarId,
        structural_isolated: bool,
    },
    finish: struct {
        pair: UnifyPair,
        action: UnifyFinishAction,
        /// `Solver.lift_count` when this pair began; a different count at
        /// its finish means a nominal lift lies at or below this pair.
        lifts_before: u32,
    },
    /// Relate generated-private evidence for one public/private pair.
    relate: struct {
        public: Type.TypeVarId,
        private: Type.TypeVarId,
    },
    /// Retire a public/private pair once everything it relates is related.
    relate_exit: UnifyPair,
};

/// A pair of spans whose element-wise unification is deferred until after the
/// enclosing merge has computed the span it writes into the store.
const DeferredSpanPair = struct {
    lhs: Type.Span,
    rhs: Type.Span,
};

/// Solve lambda-set relationships in a lifted Monotype program.
pub fn run(
    allocator: Allocator,
    lifted: Lifted.Program,
) Common.LowerError!Ast.Program {
    // `Ast.Program.init` cannot fail and takes ownership of `lifted`, so
    // `program.deinit()` is the only cleanup a later error needs.
    var program = Ast.Program.init(allocator, lifted);
    errdefer program.deinit();

    var solver = try Solver.init(allocator, &program);
    defer solver.deinit();
    try solver.solve();

    return program;
}

const Solver = struct {
    allocator: Allocator,
    program: *Ast.Program,
    lifted: Lifted.ProgramView,
    local_tys: []?Type.TypeVarId,
    expr_tys: []?Type.TypeVarId,
    pat_tys: []?Type.TypeVarId,
    expr_done: []bool,
    expr_stack: std.ArrayList(ExprFrame),
    generated_backing_pats: []bool,
    loop_results: std.ArrayList(Type.TypeVarId),
    loop_params: std.ArrayList(Type.Span),
    join_points: std.ArrayList(ActiveJoinPoint),
    return_contexts: std.ArrayList(ReturnContext),
    active_unifications: UnifyPairSet,
    unify_stack: std.ArrayList(UnifyFrame),
    active_private_evidence_relations: UnifyPairSet,
    /// Per lifted Monotype: whether any `func` or `erased` node is reachable
    /// from it. Clones of callable-free types carry no unbound slots and no
    /// mutable lambda-set state, so one shared clone serves every use, and
    /// lazy-leaf walks skip callable-free leaves without materializing them.
    contains_callable: []bool,
    /// Per lifted Monotype: whether a forced-dynamic iterator named type is
    /// reachable from it. The forced-dynamic scan materializes exactly these
    /// leaves so the named nodes it must mark exist in the solved store.
    contains_forced_dynamic: []bool,
    shared_clones: collections.DenseMap(MonoType.TypeId, Type.TypeVarId),
    /// One memo map per lazily materialized tree, tying recursive
    /// back-references to their existing vars exactly as an eager clone's
    /// per-call memo did. Allocated on a leaf's first expansion.
    leaf_contexts: std.ArrayList(collections.DenseMap(MonoType.TypeId, Type.TypeVarId)),
    /// Pools for the short-lived maps the solver creates per work item (clone
    /// memos, visited sets). Their sparse chunks span the large type ID
    /// domains, so per-item fresh maps would spend most of their time
    /// allocating and zeroing chunks; pooled maps keep chunks across uses.
    solved_set_pool: collections.DenseMapPool(Type.TypeVarId, void),
    solved_position_pool: collections.DenseMapPool(Type.TypeVarId, u32),
    /// Tag positions by name per stored tag row, keyed by the row's start.
    /// Rows are immutable once stored, so an index stays valid for the
    /// solver's lifetime and a row unified against many small rows is
    /// indexed once.
    tag_row_indexes: std.AutoHashMapUnmanaged(u32, TagRowIndex),
    /// Member positions by lambda per stored lambda set (`lambdaSetIndex`).
    lambda_set_indexes: std.AutoHashMapUnmanaged(u32, LambdaSetIndex),
    /// Field positions by name per stored record row, keyed by the row's
    /// start, so a record's fields each resolve in constant time.
    record_row_indexes: std.AutoHashMapUnmanaged(u32, RecordRowIndex),
    /// The clone context every callable-free lazy leaf expands in. A
    /// callable-free Monotype has no lambda-set state to solve, so all of its
    /// uses can read one clone; giving each use its own would make every
    /// meeting of two uses unify the whole type again.
    shared_leaf_context: ?u32,
    /// The one type every read of a declared compile-time root shares at a
    /// Monotype type other than the root's own, so each such type unifies
    /// with the root's return once however many reads name it.
    comptime_read_tys: std.AutoHashMapUnmanaged(ComptimeReadKey, Type.TypeVarId) = .empty,
    /// Nominal lifts unification has met: a structural type unified with a
    /// nominal whose inspectable backing it matches.
    lift_count: u32 = 0,
    /// Set while a root-slot read unifies with its root's return. A lifted
    /// pair, and every pair containing one, then unifies its components
    /// without joining the two types, so the root keeps the type it owns.
    preserving_lifted_roots: bool = false,
    mono_set_pool: collections.DenseMapPool(MonoType.TypeId, void),
    /// Uninhabitedness of lifted Monotypes whose proof never stopped at a
    /// type already on the walk's path. Such a result is a pure function of
    /// the immutable Monotype, independent of where the walk began.
    mono_uninhabited: collections.DenseMap(MonoType.TypeId, bool),
    /// Path stops seen by the Monotype uninhabitedness walk currently
    /// running; a result is recorded only when its subtree added none.
    mono_uninhabited_path_stops: u32 = 0,
    /// Proven-uninhabited answers for solved variables, valid while the type
    /// store's mutation epoch is unchanged. Answers that depended on a cycle
    /// being cut are provisional and never stored.
    uninhabited_memo: collections.DenseMap(Type.TypeVarId, UninhabitedAnswer),
    /// Types on the current uninhabitedness walk's path, indexed by id.
    uninhabited_path: std.DynamicBitSetUnmanaged = .{},
    mono_uninhabited_path: std.DynamicBitSetUnmanaged = .{},
    clone_map_pool: collections.DenseMapPool(MonoType.TypeId, Type.TypeVarId),
    /// Frame stacks no clone is running on and the lists a finished clone
    /// build leaves behind, kept for their capacity across clones. Each
    /// clone takes its own stack, since a step can begin another clone.
    spare_clone_stacks: std.ArrayList(std.ArrayList(TypeCloner.CloneFrame)) = .empty,
    spare_clone_lists: std.ArrayList(TypeCloner.CloneLists) = .empty,
    /// Stacks the uninhabitedness scans run on, kept between scans.
    solved_uninhabited_scratch: SolvedUninhabitedScan.Eval.Scratch = .{},
    solved_entry_marks: std.ArrayList(SolvedUninhabitedScan.EntryMark) = .empty,
    mono_uninhabited_scratch: MonoUninhabitedScan.Eval.Scratch = .{},
    mono_entry_stops: std.ArrayList(u32) = .empty,

    const FunctionShape = struct {
        args: Type.Span,
        callable: Type.TypeVarId,
        ret: Type.TypeVarId,
    };

    const BoundLowLevel = enum {
        box_box,
        box_unbox,
        list_get_unsafe,
        list_append_unsafe,
        list_concat,
        list_reserve,
        list_reserve_for_append,
        list_drop_at,
        list_sublist,
        list_take_first,
        list_take_last,
        list_drop_first,
        list_drop_last,
        list_release_excess_capacity,
        list_clear,
        list_reverse,
        list_set,
        list_replace_unsafe,
        list_swap,
        list_prepend,
        list_map_prepare_reuse,
        list_prefetched,
        list_map_can_reuse,
        list_map_write_unsafe,
        dict_pseudo_seed,
        hasher_finish,
        crypto_sha256_hash_bytes,
        crypto_sha256_hasher_finish,
        crypto_blake3_hash_bytes,
        crypto_blake3_hasher_finish,
        crypto_sha256_hasher_empty,
        crypto_blake3_hasher_empty,
        crypto_sha256_hasher_write,
        crypto_blake3_hasher_write,
        hasher_write_bool,
        hasher_write_u8,
        hasher_write_u16,
        hasher_write_u32,
        hasher_write_u64,
        hasher_write_u128,
        hasher_write_i8,
        hasher_write_i16,
        hasher_write_i32,
        hasher_write_i64,
        hasher_write_i128,
        hasher_write_f32,
        hasher_write_f64,
        hasher_write_dec,
        hasher_write_bytes,
        hasher_write_str,
    };

    const ReturnContext = struct {
        mono_ret: MonoType.TypeId,
        solved_ret: Type.TypeVarId,
    };

    const ActiveJoinPoint = struct {
        id: Lifted.JoinPointId,
        params: Type.Span,
    };

    fn init(allocator: Allocator, program: *Ast.Program) Allocator.Error!Solver {
        const lifted = program.lifted.view();

        const local_tys = try allocator.alloc(?Type.TypeVarId, lifted.locals.len);
        errdefer allocator.free(local_tys);
        @memset(local_tys, null);

        const expr_tys = try allocator.alloc(?Type.TypeVarId, lifted.exprs.len);
        errdefer allocator.free(expr_tys);
        @memset(expr_tys, null);

        const expr_done = try allocator.alloc(bool, lifted.exprs.len);
        errdefer allocator.free(expr_done);
        @memset(expr_done, false);

        const pat_tys = try allocator.alloc(?Type.TypeVarId, lifted.pats.len);
        errdefer allocator.free(pat_tys);
        @memset(pat_tys, null);

        const generated_backing_pats = try allocator.alloc(bool, lifted.pats.len);
        errdefer allocator.free(generated_backing_pats);
        @memset(generated_backing_pats, false);

        const masks = try computeReachabilityMasks(allocator, lifted.types);
        errdefer allocator.free(masks.contains_callable);
        errdefer allocator.free(masks.contains_forced_dynamic);

        return .{
            .allocator = allocator,
            .program = program,
            .lifted = lifted,
            .local_tys = local_tys,
            .expr_tys = expr_tys,
            .pat_tys = pat_tys,
            .expr_done = expr_done,
            .expr_stack = .empty,
            .generated_backing_pats = generated_backing_pats,
            .loop_results = .empty,
            .loop_params = .empty,
            .join_points = .empty,
            .return_contexts = .empty,
            .active_unifications = UnifyPairSet.init(allocator),
            .unify_stack = .empty,
            .active_private_evidence_relations = UnifyPairSet.init(allocator),
            .contains_callable = masks.contains_callable,
            .contains_forced_dynamic = masks.contains_forced_dynamic,
            .shared_clones = collections.DenseMap(MonoType.TypeId, Type.TypeVarId).init(allocator),
            .leaf_contexts = .empty,
            .solved_set_pool = collections.DenseMapPool(Type.TypeVarId, void).init(allocator),
            .solved_position_pool = collections.DenseMapPool(Type.TypeVarId, u32).init(allocator),
            .tag_row_indexes = .empty,
            .lambda_set_indexes = .empty,
            .record_row_indexes = .empty,
            .shared_leaf_context = null,
            .mono_set_pool = collections.DenseMapPool(MonoType.TypeId, void).init(allocator),
            .mono_uninhabited = collections.DenseMap(MonoType.TypeId, bool).init(allocator),
            .uninhabited_memo = collections.DenseMap(Type.TypeVarId, UninhabitedAnswer).init(allocator),
            .clone_map_pool = collections.DenseMapPool(MonoType.TypeId, Type.TypeVarId).init(allocator),
        };
    }

    fn deinit(self: *Solver) void {
        var row_indexes = self.tag_row_indexes.valueIterator();
        while (row_indexes.next()) |row_index| row_index.by_name.deinit(self.allocator);
        self.tag_row_indexes.deinit(self.allocator);
        var set_indexes = self.lambda_set_indexes.valueIterator();
        while (set_indexes.next()) |set_index| set_index.by_lambda.deinit(self.allocator);
        self.lambda_set_indexes.deinit(self.allocator);
        var record_indexes = self.record_row_indexes.valueIterator();
        while (record_indexes.next()) |row_index| row_index.by_name.deinit(self.allocator);
        self.record_row_indexes.deinit(self.allocator);
        self.clone_map_pool.deinit();
        for (self.spare_clone_stacks.items) |*stack| stack.deinit(self.allocator);
        self.spare_clone_stacks.deinit(self.allocator);
        for (self.spare_clone_lists.items) |*lists| lists.deinit(self.allocator);
        self.spare_clone_lists.deinit(self.allocator);
        self.solved_uninhabited_scratch.deinit(self.allocator);
        self.solved_entry_marks.deinit(self.allocator);
        self.mono_uninhabited_scratch.deinit(self.allocator);
        self.mono_entry_stops.deinit(self.allocator);
        self.mono_set_pool.deinit();
        self.mono_uninhabited.deinit();
        self.uninhabited_memo.deinit();
        self.uninhabited_path.deinit(self.allocator);
        self.mono_uninhabited_path.deinit(self.allocator);
        self.solved_position_pool.deinit();
        self.solved_set_pool.deinit();
        for (self.leaf_contexts.items) |*ctx| ctx.deinit();
        self.leaf_contexts.deinit(self.allocator);
        self.shared_clones.deinit();
        self.allocator.free(self.contains_forced_dynamic);
        self.allocator.free(self.contains_callable);
        self.comptime_read_tys.deinit(self.allocator);
        self.active_private_evidence_relations.deinit();
        self.unify_stack.deinit(self.allocator);
        self.active_unifications.deinit();
        self.return_contexts.deinit(self.allocator);
        self.join_points.deinit(self.allocator);
        self.loop_params.deinit(self.allocator);
        self.loop_results.deinit(self.allocator);
        self.allocator.free(self.generated_backing_pats);
        self.expr_stack.deinit(self.allocator);
        self.allocator.free(self.expr_done);
        self.allocator.free(self.pat_tys);
        self.allocator.free(self.expr_tys);
        self.allocator.free(self.local_tys);
    }

    fn solve(self: *Solver) Allocator.Error!void {
        for (self.lifted.locals, 0..) |local, index| {
            self.local_tys[index] = try self.lowerTypeFresh(local.ty);
        }

        try self.program.fn_tys.ensureTotalCapacity(self.allocator, self.lifted.fns.len);
        try self.program.defs.ensureTotalCapacity(self.allocator, self.lifted.fns.len);

        for (self.lifted.fns) |fn_| {
            const fn_ty = try self.functionType(fn_);
            try self.program.fn_tys.append(self.allocator, fn_ty);
            try self.program.defs.append(self.allocator, .{
                .symbol = fn_.symbol,
                .ty = fn_ty,
                .body = switch (fn_.body) {
                    .roc => |body| .{ .roc = body },
                    .hosted => .hosted,
                },
            });
        }

        for (self.lifted.fns, 0..) |fn_, index| {
            const fn_id: Lifted.FnId = @enumFromInt(@as(u32, @intCast(index)));
            try self.solveFn(fn_id, fn_);
        }

        try self.markAbiBoundaryCallables();

        try self.program.layout_requests.ensureTotalCapacity(self.allocator, self.lifted.layout_requests.len);
        for (self.lifted.layout_requests) |request| {
            const ty = if (request.fn_id) |fn_id|
                self.fnRetType(fn_id)
            else
                try self.monoLeaf(request.ty);
            try self.markErasedCallablesReachedByType(ty);
            try self.program.layout_requests.append(self.allocator, .{
                .checked_type = request.checked_type,
                .ty = ty,
                .fn_id = request.fn_id,
                .const_locator = request.const_locator,
                .comptime_root = request.comptime_root,
            });
        }

        try self.program.runtime_schema_requests.ensureTotalCapacity(self.allocator, self.lifted.runtime_schema_requests.len);
        for (self.lifted.runtime_schema_requests) |request| {
            const ty = try self.monoLeaf(request.ty);
            try self.markErasedCallablesReachedByType(ty);
            try self.program.runtime_schema_requests.append(self.allocator, .{
                .def = request.def,
                .ty = ty,
            });
        }

        try self.markForcedDynamicIteratorCallables();

        try self.program.expr_tys.ensureTotalCapacity(self.allocator, self.expr_tys.len);
        for (self.expr_tys, 0..) |maybe_ty, index| {
            const ty = maybe_ty orelse try self.monoLeaf(self.lifted.exprs[index].ty);
            try self.program.expr_tys.append(self.allocator, self.program.types.rootCompressed(ty));
        }

        try self.program.pat_tys.ensureTotalCapacity(self.allocator, self.pat_tys.len);
        for (self.pat_tys, 0..) |maybe_ty, index| {
            const ty = maybe_ty orelse try self.monoLeaf(self.lifted.pats[index].ty);
            try self.program.pat_tys.append(self.allocator, self.program.types.rootCompressed(ty));
        }

        try self.program.local_tys.ensureTotalCapacity(self.allocator, self.local_tys.len);
        for (self.local_tys) |maybe_ty| {
            const ty = maybe_ty orelse Common.invariant("Lambda Solved local type slot was not initialized");
            try self.program.local_tys.append(self.allocator, self.program.types.rootCompressed(ty));
        }

        for (self.program.layout_requests.items) |*request| {
            request.ty = self.program.types.rootCompressed(request.ty);
        }
        for (self.program.runtime_schema_requests.items) |*request| {
            request.ty = self.program.types.rootCompressed(request.ty);
        }

        try self.finalizeMonoLeaves();
        // After finalization so the materialized clones' callable slots close
        // exactly as their eager counterparts always did.
        try self.closeUnfilledCallableSlots();
    }

    fn functionType(self: *Solver, fn_: Lifted.Fn) Allocator.Error!Type.TypeVarId {
        const arg_locals = self.lifted.typedLocalSpan(fn_.args);
        const capture_locals = self.lifted.typedLocalSpan(fn_.captures);
        const captures = try self.allocator.alloc(Type.Capture, capture_locals.len);
        defer self.allocator.free(captures);
        for (capture_locals, 0..) |capture, i| {
            const local = self.lifted.locals[@intFromEnum(capture.local)];
            captures[i] = .{
                .local = capture.local,
                .symbol = local.symbol,
                .binder = local.binder,
                .capture_id = local.capture_id,
                .checked_capture_id = local.checked_capture_id,
                .ty = self.localTy(capture.local),
            };
        }

        const capture_span = try self.program.types.addCaptures(captures);
        const members = [_]Type.FnMember{.{
            .lambda = fn_.symbol,
            .captures = capture_span,
        }};
        const callable = try self.program.types.add(.{ .lambda_set = try self.program.types.addMembers(&members) });

        if (fn_.signature) |signature| {
            const fn_ty = try self.monoLeaf(signature);
            const content = try self.resolvedContent(fn_ty);
            if (std.meta.activeTag(content) != .func) Common.invariant("producer-authored lifted function signature was not a function");
            const func = content.func;
            if (func.args.count() != arg_locals.len) {
                Common.invariant("producer-authored lifted function signature arity changed before Lambda Solved");
            }
            for (arg_locals, 0..) |arg, i| {
                const local = self.lifted.locals[@intFromEnum(arg.local)];
                if (@import("builtin").mode == .Debug and
                    !try self.sameMonoType(local.ty, arg.ty))
                {
                    Common.invariant("Lambda Solved function argument type differed from its local type");
                }
                try self.unify(self.localTy(arg.local), self.program.types.spanItem(func.args, i));
            }
            try self.unify(func.callable, callable);
            return self.program.types.rootCompressed(fn_ty);
        }

        const args = try self.allocator.alloc(Type.TypeVarId, arg_locals.len);
        defer self.allocator.free(args);
        for (arg_locals, 0..) |arg, i| {
            const local = self.lifted.locals[@intFromEnum(arg.local)];
            if (@import("builtin").mode == .Debug and
                !try self.sameMonoType(local.ty, arg.ty))
            {
                Common.invariant("Lambda Solved function argument type differed from its local type");
            }
            args[i] = self.localTy(arg.local);
        }

        return try self.program.types.add(.{ .func = .{
            .args = try self.program.types.addSpan(args),
            .callable = callable,
            .ret = try self.lowerTypeFresh(fn_.ret),
        } });
    }

    fn fnRetType(self: *Solver, fn_id: Lifted.FnId) Type.TypeVarId {
        const raw = @intFromEnum(fn_id);
        if (raw >= self.program.fn_tys.items.len) Common.invariant("Lambda Solved layout request referenced a missing function");
        const fn_ty = self.program.types.rootContentCompressed(self.program.fn_tys.items[raw]);
        if (std.meta.activeTag(fn_ty) != .func) Common.invariant("Lambda Solved layout request referenced a non-function");
        return fn_ty.func.ret;
    }

    fn solveFn(self: *Solver, fn_id: Lifted.FnId, fn_: Lifted.Fn) Allocator.Error!void {
        const fn_ty = self.program.fn_tys.items[@intFromEnum(fn_id)];
        const fn_content = self.program.types.rootContentCompressed(fn_ty);
        if (std.meta.activeTag(fn_content) != .func) Common.invariant("Lambda Solved function table contains a non-function type");
        const func = fn_content.func;

        const arg_locals = self.lifted.typedLocalSpan(fn_.args);
        if (func.args.count() != arg_locals.len) Common.invariant("Lambda Solved function arity changed after registration");
        for (arg_locals, 0..) |arg, i| {
            try self.unify(self.program.types.spanItem(func.args, i), self.localTy(arg.local));
        }

        try self.return_contexts.append(self.allocator, .{
            .mono_ret = fn_.ret,
            .solved_ret = func.ret,
        });
        defer _ = self.return_contexts.pop();

        switch (fn_.body) {
            .roc => |body| {
                _ = try self.expectExpr(body, func.ret);
            },
            .hosted => {},
        }
    }

    /// Host-facing function schemas use the erased callable representation for
    /// every callable value reachable from an argument or result. Seed that
    /// explicit boundary requirement after ordinary body constraints have
    /// unified, but before unresolved callable slots are closed as finite.
    fn markAbiBoundaryCallables(self: *Solver) Allocator.Error!void {
        for (self.lifted.fns, 0..) |fn_, index| {
            if (fn_.body != .hosted) continue;
            const fn_id: Lifted.FnId = @enumFromInt(@as(u32, @intCast(index)));
            try self.markErasedCallablesAtFunctionBoundary(self.program.fn_tys.items[@intFromEnum(fn_id)]);
        }

        for (self.lifted.roots) |root| {
            switch (root.request.abi) {
                .platform, .hosted => {
                    const index = @intFromEnum(root.fn_id);
                    if (index >= self.program.fn_tys.items.len) {
                        Common.invariant("Lambda Solved ABI root referenced a missing function");
                    }
                    try self.markErasedCallablesAtFunctionBoundary(self.program.fn_tys.items[index]);
                },
                .roc, .test_expect, .compile_time => {},
            }
        }
    }

    fn markErasedCallablesAtFunctionBoundary(self: *Solver, fn_ty: Type.TypeVarId) Allocator.Error!void {
        const content = try self.resolvedContent(fn_ty);
        if (std.meta.activeTag(content) != .func) Common.invariant("Lambda Solved ABI boundary referenced a non-function");
        const func = content.func;
        for (0..func.args.count()) |index| {
            const arg = self.program.types.spanItem(func.args, index);
            try self.markErasedCallablesReachedByType(arg);
        }
        try self.markErasedCallablesReachedByType(func.ret);
    }

    fn closeUnfilledCallableSlots(self: *Solver) Allocator.Error!void {
        self.program.types.compressAllRoots();

        const count = self.program.types.vars.items.len;
        const done = try self.allocator.alloc(bool, count);
        defer self.allocator.free(done);
        @memset(done, false);

        var pending: std.ArrayList(Type.TypeVarId) = .empty;
        defer pending.deinit(self.allocator);

        for (0..count) |index| {
            const ty: Type.TypeVarId = @enumFromInt(@as(u32, @intCast(index)));
            if (std.meta.activeTag(self.program.types.get(ty)) != .link) try self.closeCallableSlotsInType(ty, done, &pending);
        }
    }

    /// Close every unbound callable slot reachable from `ty` to an empty
    /// lambda set, visiting each type not yet `done` once on an explicit
    /// stack.
    fn closeCallableSlotsInType(
        self: *Solver,
        ty: Type.TypeVarId,
        done: []bool,
        pending: *std.ArrayList(Type.TypeVarId),
    ) Allocator.Error!void {
        const types = &self.program.types;
        try pending.append(self.allocator, ty);
        while (pending.pop()) |next| {
            const root = types.rootCompressed(next);
            const root_index = @intFromEnum(root);
            if (done[root_index]) continue;
            done[root_index] = true;

            switch (types.get(root)) {
                // A leaf never materialized its callable slots; finalization's
                // clones create them after this pass, unbound, exactly as the
                // post-solve eager clones always did.
                .mono => {},
                .link => Common.invariant("Lambda Solved root returned a link"),
                .unbound,
                .forall,
                .primitive,
                .zst,
                => {},
                .func => |func| {
                    try self.closeCallableSlot(func.callable, pending);
                    for (0..func.args.count()) |arg_index| try pending.append(self.allocator, types.spanItem(func.args, arg_index));
                    try pending.append(self.allocator, func.ret);
                },
                .list, .box => |elem| try pending.append(self.allocator, elem),
                .tuple => |items| for (0..items.count()) |index| try pending.append(self.allocator, types.spanItem(items, index)),
                .record => |fields| for (0..fields.count()) |index| {
                    const field = types.fieldItem(fields, index);
                    try pending.append(self.allocator, field.ty);
                    if (field.value_ty) |value_ty| try pending.append(self.allocator, value_ty);
                },
                .tag_union => |tags| for (0..tags.count()) |tag_index| {
                    const tag = types.tagItem(tags, tag_index);
                    for (0..tag.payloads.count()) |payload_index| try pending.append(self.allocator, types.spanItem(tag.payloads, payload_index));
                },
                .named => |named| {
                    for (0..named.args.count()) |index| try pending.append(self.allocator, types.spanItem(named.args, index));
                    if (named.backing) |backing| try pending.append(self.allocator, backing.ty);
                },
                .lambda_set => |members| try self.appendCallableCaptureTypes(members, pending),
                .erased => |erased| try self.appendCallableCaptureTypes(erased.members, pending),
            }
        }
    }

    fn closeCallableSlot(
        self: *Solver,
        callable: Type.TypeVarId,
        pending: *std.ArrayList(Type.TypeVarId),
    ) Allocator.Error!void {
        const root = self.program.types.rootCompressed(callable);
        switch (self.program.types.get(root)) {
            .unbound => self.program.types.set(root, .{ .lambda_set = .empty() }),
            .lambda_set,
            .erased,
            => try pending.append(self.allocator, root),
            .link,
            .forall,
            .primitive,
            .named,
            .record,
            .tuple,
            .tag_union,
            .list,
            .box,
            .func,
            .zst,
            .mono,
            => Common.invariant("function callable slot resolved to a non-callable type"),
        }
    }

    fn appendCallableCaptureTypes(
        self: *Solver,
        members: Type.Span,
        pending: *std.ArrayList(Type.TypeVarId),
    ) Allocator.Error!void {
        for (0..members.count()) |member_index| {
            const member = self.program.types.memberItem(members, member_index);
            for (0..member.captures.count()) |capture_index| {
                try pending.append(self.allocator, self.program.types.captureItem(member.captures, capture_index).ty);
            }
        }
    }

    /// A suspended expression or statement. Sequential spans retain one cursor,
    /// so their length does not increase either native or continuation depth.
    const ExprFrame = struct {
        node: union(enum) { expr: Lifted.ExprId, stmt: Lifted.StmtId },
        ty: ?Type.TypeVarId = null,
        finish_slot: ?Type.TypeVarId = null,
        cursor: usize = 0,
        branch: usize = 0,
        binding: usize = 0,
        match_phase: enum { bind_pattern, bindings, guard, body, next_branch } = .bind_pattern,
        generated_backing: bool = false,
    };

    const ExprRequest = union(enum) {
        expr: struct {
            id: Lifted.ExprId,
            expected: ?Type.TypeVarId = null,
            generated_backing: bool = false,
        },
        stmt: Lifted.StmtId,
    };

    fn beginExpr(self: *Solver, request: ExprRequest) Allocator.Error!?ExprFrame {
        switch (request) {
            .stmt => |stmt| return .{ .node = .{ .stmt = stmt } },
            .expr => |expr| {
                const slot = if (expr.expected) |expected| try self.expectExprSlot(expr.id, expected) else null;
                const ty = try self.exprSlot(expr.id);
                const index = @intFromEnum(expr.id);
                if (self.expr_done[index]) {
                    if (slot) |expected| try self.unify(expected, ty);
                    return null;
                }
                self.expr_done[index] = true;
                return .{ .node = .{ .expr = expr.id }, .ty = ty, .finish_slot = slot, .generated_backing = expr.generated_backing };
            },
        }
    }

    fn inferredExpr(self: *Solver, expr: Lifted.ExprId) Type.TypeVarId {
        std.debug.assert(self.expr_done[@intFromEnum(expr)]);
        return self.program.types.rootCompressed(self.expr_tys[@intFromEnum(expr)].?);
    }

    fn inferExpr(self: *Solver, expr_id: Lifted.ExprId) Allocator.Error!Type.TypeVarId {
        std.debug.assert(self.expr_stack.items.len == 0);
        defer self.expr_stack.clearRetainingCapacity();
        var frame = (try self.beginExpr(.{ .expr = .{ .id = expr_id } })) orelse return self.inferredExpr(expr_id);
        while (true) {
            if (try self.stepExpr(&frame)) |request| {
                if (try self.beginExpr(request)) |child| {
                    try self.expr_stack.append(self.allocator, frame);
                    frame = child;
                }
            } else {
                if (frame.finish_slot) |slot| try self.unify(slot, self.program.types.rootCompressed(frame.ty.?));
                frame = self.expr_stack.pop() orelse break;
            }
        }
        return self.inferredExpr(expr_id);
    }

    /// Resume at the next source-ordered child, after all constraints from the
    /// previous child have been applied. Scope stacks stay live until the
    /// owning expression's last child returns.
    fn stepExpr(self: *Solver, frame: *ExprFrame) Allocator.Error!?ExprRequest {
        const expr_id = switch (frame.node) {
            .expr => |expr| expr,
            .stmt => |stmt_id| {
                const stmt = self.lifted.stmts[@intFromEnum(stmt_id)];
                if (frame.cursor != 0) {
                    if (stmt == .let_) try self.bindPattern(stmt.let_.pat, self.inferredExpr(stmt.let_.value));
                    if (stmt == .return_) try self.relateReturnedExpr(stmt.return_.value, try self.returnTargetTy(stmt.return_.target));
                    return null;
                }
                frame.cursor = 1;
                switch (stmt) {
                    .uninitialized => |pat| {
                        const pat_ty = try self.lowerTypeFresh(self.lifted.pats[@intFromEnum(pat)].ty);
                        try self.bindPattern(pat, pat_ty);
                        return null;
                    },
                    .let_ => |let_| return .{ .expr = .{ .id = let_.value } },
                    .expr, .expect, .dbg => |expr| return .{ .expr = .{ .id = expr } },
                    .return_ => |ret| return .{ .expr = .{ .id = ret.value } },
                    .crash, .checked_error => return null,
                }
            },
        };
        const expr = self.lifted.exprs[@intFromEnum(expr_id)];
        const expected = frame.ty.?;
        const cursor = frame.cursor;
        frame.cursor += 1;

        if (frame.generated_backing) switch (expr.data) {
            .record => |fields| {
                const children = self.lifted.fieldExprSpan(fields);
                return if (cursor < children.len) .{ .expr = .{ .id = children[cursor].value } } else null;
            },
            .record_update => |update| {
                if (cursor == 0) return .{ .expr = .{ .id = update.base } };
                const children = self.lifted.fieldExprSpan(update.fields);
                return if (cursor - 1 < children.len) .{ .expr = .{ .id = children[cursor - 1].value } } else null;
            },
            .tuple, .tag => {
                const children = self.lifted.exprSpan(if (expr.data == .tuple) expr.data.tuple else expr.data.tag.payloads);
                return if (cursor < children.len) .{ .expr = .{ .id = children[cursor] } } else null;
            },
            .comptime_value => |value| {
                return if (cursor == 0) .{ .expr = .{ .id = value.initializer, .generated_backing = true } } else null;
            },
            .static_data_candidate, .nominal => {
                const child = if (expr.data == .nominal) expr.data.nominal else expr.data.static_data_candidate.runtime_expr;
                return if (cursor == 0) .{ .expr = .{ .id = child, .generated_backing = true } } else null;
            },
            .let_ => |let_| {
                if (cursor == 0) return .{ .expr = .{ .id = let_.value } };
                if (cursor == 1) {
                    try self.bindPattern(let_.bind, self.inferredExpr(let_.value));
                    return .{ .expr = .{ .id = let_.rest, .generated_backing = true } };
                }
                return null;
            },
            .local, .unit, .@"unreachable", .int_lit, .frac_f32_lit, .frac_f64_lit, .dec_lit, .str_lit, .bytes_lit, .typed_boundary, .list, .lambda, .def_ref, .fn_def, .fn_ref, .call_value, .call_proc, .low_level, .field_access, .tuple_access, .structural_eq, .structural_hash, .match_, .if_, .uninitialized, .uninitialized_payload, .if_initialized_payload, .try_sequence, .try_record_sequence, .block, .loop_, .break_, .continue_, .join_point, .jump, .return_, .crash, .checked_error, .comptime_exhaustiveness_failed, .dbg, .expect, .expect_err, .literal_rejected, .comptime_branch_taken => {},
        };

        switch (expr.data) {
            .local => |local| try self.unify(expected, self.localTy(local)),
            .unit, .int_lit, .frac_f32_lit, .frac_f64_lit, .dec_lit, .str_lit, .bytes_lit, .uninitialized, .uninitialized_payload, .crash, .checked_error, .comptime_exhaustiveness_failed, .@"unreachable" => {},
            .comptime_value => |value| {
                if (cursor == 0) return .{ .expr = .{ .id = value.initializer } };
                try self.unifyComptimeValueRead(expr.ty, expected, value.initializer);
            },
            .static_data_candidate => |candidate| {
                if (cursor == 0) return .{ .expr = .{ .id = candidate.runtime_expr, .expected = expected } };
            },
            .typed_boundary => |boundary| {
                if (cursor == 0) return .{ .expr = .{ .id = boundary.value } };
                try self.unify(expected, self.inferredExpr(boundary.value));
            },
            .list => |items| {
                const children = self.lifted.exprSpan(items);
                if (cursor < children.len) return .{ .expr = .{ .id = children[cursor], .expected = try self.listElem(expected) } };
            },
            .tuple => |items| {
                const tys = try self.tupleItemsSpan(expected);
                const children = self.lifted.exprSpan(items);
                if (tys.count() != children.len) Common.invariant("tuple expression arity differs from its checked type");
                if (cursor < children.len) return .{ .expr = .{ .id = children[cursor], .expected = self.program.types.spanItem(tys, cursor) } };
            },
            .record => |fields| {
                const children = self.lifted.fieldExprSpan(fields);
                if (cursor < children.len) {
                    const field = children[cursor];
                    return .{ .expr = .{ .id = field.value, .expected = try self.recordField(expected, field.name) } };
                }
            },
            .record_update => |update| {
                if (cursor == 0) return .{ .expr = .{ .id = update.base } };
                if (cursor == 1) try self.relateRecordUpdate(expected, self.inferredExpr(update.base), update.fields);
                const children = self.lifted.fieldExprSpan(update.fields);
                if (cursor - 1 < children.len) {
                    const field = children[cursor - 1];
                    return .{ .expr = .{ .id = field.value, .expected = try self.recordField(expected, field.name) } };
                }
            },
            .tag => |tag| {
                const tys = try self.tagPayloadsSpan(expected, tag.name);
                const children = self.lifted.exprSpan(tag.payloads);
                if (tys.count() != children.len) Common.invariant("tag expression payload arity differs from its checked type");
                if (cursor < children.len) return .{ .expr = .{ .id = children[cursor], .expected = self.program.types.spanItem(tys, cursor) } };
            },
            .nominal => |backing| {
                if (cursor == 0) {
                    if (try self.namedBacking(expected)) |ty| {
                        if (try self.hasBuiltinOwner(expected, .fields) or try self.hasBuiltinOwner(expected, .field)) {
                            return .{ .expr = .{ .id = backing, .generated_backing = true } };
                        }
                        return .{ .expr = .{ .id = backing, .expected = ty } };
                    }
                    return .{ .expr = .{ .id = backing } };
                }
            },
            .let_ => |let_| {
                if (cursor == 0) return .{ .expr = .{ .id = let_.value } };
                if (cursor == 1) {
                    try self.bindPattern(let_.bind, self.inferredExpr(let_.value));
                    return .{ .expr = .{ .id = let_.rest, .expected = expected } };
                }
            },
            .lambda, .def_ref, .fn_def => Common.invariant("pre-lift function expression reached Lambda Solved"),
            .fn_ref => |ref| {
                if (cursor == 0) try self.unify(expected, self.program.fn_tys.items[@intFromEnum(ref.fn_id)]);
                return try self.captureRequest(ref.fn_id, ref.captures, cursor);
            },
            .call_value => |call| {
                if (cursor == 0) return .{ .expr = .{ .id = call.callee } };
                const func = try self.functionShape(self.inferredExpr(call.callee));
                const args = self.lifted.exprSpan(call.args);
                if (func.args.count() != args.len) Common.invariant("value call arity differs from its checked type");
                if (cursor == 1) try self.unify(expected, func.ret);
                if (cursor - 1 < args.len) return .{ .expr = .{ .id = args[cursor - 1], .expected = self.program.types.spanItem(func.args, cursor - 1) } };
            },
            .call_proc => |call| {
                const args = self.lifted.exprSpan(call.args);
                switch (Lifted.directCallee(call)) {
                    .local => |callee| {
                        const func = try self.functionShape(self.program.fn_tys.items[@intFromEnum(callee)]);
                        if (func.args.count() != args.len) Common.invariant("procedure call arity differs from its checked type");
                        if (cursor == 0) try self.unify(expected, func.ret);
                        if (cursor < args.len) return .{ .expr = .{ .id = args[cursor], .expected = self.program.types.spanItem(func.args, cursor) } };
                        return try self.captureRequest(callee, call.captures, cursor - args.len);
                    },
                }
            },
            .low_level => |call| {
                const args = self.lifted.exprSpan(call.args);
                if (cursor < args.len) return .{ .expr = .{ .id = args[cursor] } };
                const tys = try self.allocator.alloc(Type.TypeVarId, args.len);
                defer self.allocator.free(tys);
                for (args, tys) |arg, *ty| ty.* = self.inferredExpr(arg);
                try self.bindLowLevelTypes(call.op, expected, tys);
            },
            .field_access => |field| {
                if (cursor == 0) return .{ .expr = .{ .id = field.receiver } };
                var ty = self.inferredExpr(field.receiver);
                const segments = self.lifted.fieldAccessSegmentSpan(field.segments);
                if (segments.len == 0) Common.invariant("field access path had no segments");
                for (segments) |segment| ty = try self.recordField(ty, segment.field);
                try self.unify(expected, ty);
            },
            .tuple_access => |access| {
                if (cursor == 0) return .{ .expr = .{ .id = access.tuple } };
                const items = try self.tupleItemsSpan(self.inferredExpr(access.tuple));
                if (access.elem_index >= items.count()) Common.invariant("tuple access index exceeds tuple arity");
                try self.unify(expected, self.program.types.spanItem(items, access.elem_index));
            },
            .structural_eq => |eq| {
                if (cursor == 0) return .{ .expr = .{ .id = eq.lhs } };
                if (cursor == 1) return .{ .expr = .{ .id = eq.rhs } };
                try self.unify(self.inferredExpr(eq.lhs), self.inferredExpr(eq.rhs));
            },
            .structural_hash => |h| {
                if (cursor == 0) return .{ .expr = .{ .id = h.value } };
                if (cursor == 1) return .{ .expr = .{ .id = h.hasher } };
                try self.unify(expected, self.inferredExpr(h.hasher));
            },
            .match_ => |match| {
                if (cursor == 0) return .{ .expr = .{ .id = match.scrutinee } };
                const branches = self.lifted.branchSpan(match.branches);
                while (frame.branch < branches.len) {
                    const branch = branches[frame.branch];
                    switch (frame.match_phase) {
                        .bind_pattern => {
                            try self.bindPattern(branch.pat, self.inferredExpr(match.scrutinee));
                            frame.binding = 0;
                            frame.match_phase = .bindings;
                        },
                        .bindings => {
                            const bindings = self.lifted.stmtSpan(branch.bindings);
                            if (frame.binding < bindings.len) {
                                const stmt = bindings[frame.binding];
                                frame.binding += 1;
                                return .{ .stmt = stmt };
                            }
                            frame.match_phase = .guard;
                        },
                        .guard => {
                            frame.match_phase = .body;
                            if (branch.guard) |guard| return .{ .expr = .{ .id = guard } };
                        },
                        .body => {
                            frame.match_phase = .next_branch;
                            return .{ .expr = .{ .id = branch.body, .expected = expected } };
                        },
                        .next_branch => {
                            frame.branch += 1;
                            frame.match_phase = .bind_pattern;
                        },
                    }
                }
            },
            .if_ => |if_| {
                const branches = self.lifted.ifBranchSpan(if_.branches);
                if (cursor / 2 < branches.len) {
                    const branch = branches[cursor / 2];
                    return .{ .expr = if (cursor % 2 == 0) .{ .id = branch.cond } else .{ .id = branch.body, .expected = expected } };
                }
                if (cursor == 2 * branches.len) return .{ .expr = .{ .id = if_.final_else, .expected = expected } };
            },
            .if_initialized_payload => |payload| {
                if (cursor == 0) return .{ .expr = .{ .id = payload.cond } };
                if (cursor == 1) {
                    _ = self.localTy(payload.payload);
                    return .{ .expr = .{ .id = payload.initialized, .expected = expected } };
                }
                if (cursor == 2) return .{ .expr = .{ .id = payload.uninitialized, .expected = expected } };
            },
            .try_sequence, .try_record_sequence => {
                const input = if (expr.data == .try_sequence) expr.data.try_sequence.try_expr else expr.data.try_record_sequence.try_expr;
                if (cursor == 0) return .{ .expr = .{ .id = input } };
                if (cursor == 1) {
                    const content = try self.shapeContent(self.inferredExpr(input));
                    if (content != .tag_union) Common.invariant("try sequence input was not a Try tag union");
                    var ok_ty: ?Type.TypeVarId = null;
                    var err_ty: ?Type.TypeVarId = null;
                    for (0..content.tag_union.count()) |index| {
                        const tag = self.program.types.tagItem(content.tag_union, index);
                        const name = self.lifted.names.tagLabelText(tag.name);
                        if (std.mem.eql(u8, name, "Ok") or std.mem.eql(u8, name, "Err")) {
                            if (tag.payloads.count() != 1) Common.invariant("try sequence tag had unexpected payload arity");
                            if (std.mem.eql(u8, name, "Ok")) ok_ty = self.program.types.spanItem(tag.payloads, 0) else err_ty = self.program.types.spanItem(tag.payloads, 0);
                        }
                    }
                    const ok = ok_ty orelse Common.invariant("try sequence input had no Ok tag");
                    const target = if (expr.data == .try_sequence) expr.data.try_sequence.err_target else expr.data.try_record_sequence.err_target;
                    if (expr.data == .try_sequence) {
                        try self.unify(self.localTy(expr.data.try_sequence.ok_local), ok);
                    } else {
                        const seq = expr.data.try_record_sequence;
                        try self.unify(self.localTy(seq.value_local), try self.recordField(ok, seq.value_field));
                        try self.unify(self.localTy(seq.rest_local), try self.recordField(ok, seq.rest_field));
                    }
                    if (target) |id| {
                        const params = self.activeJoinPoint(id).params;
                        if (params.count() != 1) Common.invariant("try sequence error target did not have one parameter");
                        try self.unify(self.program.types.spanItem(params, 0), err_ty orelse Common.invariant("try sequence input had no Err tag"));
                    }
                    const body = if (expr.data == .try_sequence) expr.data.try_sequence.ok_body else expr.data.try_record_sequence.ok_body;
                    return .{ .expr = .{ .id = body, .expected = expected } };
                }
            },
            .block => |block| {
                const statements = self.lifted.stmtSpan(block.statements);
                if (cursor < statements.len) return .{ .stmt = statements[cursor] };
                if (cursor == statements.len) return .{ .expr = .{ .id = block.final_expr, .expected = expected } };
            },
            .loop_ => |loop| {
                const params = self.lifted.typedLocalSpan(loop.params);
                const initials = self.lifted.exprSpan(loop.initial_values);
                if (params.len != initials.len) Common.invariant("loop parameter count differs from initial value count");
                if (cursor < params.len) return .{ .expr = .{ .id = initials[cursor], .expected = self.localTy(params[cursor].local) } };
                if (cursor == params.len) {
                    const tys = try self.allocator.alloc(Type.TypeVarId, params.len);
                    defer self.allocator.free(tys);
                    for (params, tys) |param, *ty| ty.* = self.localTy(param.local);
                    try self.loop_results.append(self.allocator, expected);
                    try self.loop_params.append(self.allocator, try self.program.types.addSpan(tys));
                    return .{ .expr = .{ .id = loop.body, .expected = expected } };
                }
                _ = self.loop_params.pop();
                _ = self.loop_results.pop();
            },
            .break_ => |value| {
                if (cursor == 0) if (value) |child| return .{ .expr = .{ .id = child, .expected = self.currentLoopResult() } };
            },
            .continue_ => |cont| {
                const params = self.currentLoopParams();
                const values = self.lifted.exprSpan(cont.values);
                if (params.count() != values.len) Common.invariant("continue value count differs from loop parameter count");
                if (cursor < values.len) return .{ .expr = .{ .id = values[cursor], .expected = self.program.types.spanItem(params, cursor) } };
            },
            .join_point => |join| {
                if (cursor == 0) {
                    const params = self.lifted.typedLocalSpan(join.params);
                    const tys = try self.allocator.alloc(Type.TypeVarId, params.len);
                    defer self.allocator.free(tys);
                    for (params, tys) |param, *ty| ty.* = self.localTy(param.local);
                    try self.join_points.append(self.allocator, .{ .id = join.id, .params = try self.program.types.addSpan(tys) });
                    return .{ .expr = .{ .id = join.body, .expected = expected } };
                }
                if (cursor == 1) return .{ .expr = .{ .id = join.remainder, .expected = expected } };
                _ = self.join_points.pop();
            },
            .jump => |jump| {
                const params = self.activeJoinPoint(jump.target).params;
                const args = self.lifted.exprSpan(jump.args);
                if (params.count() != args.len) Common.invariant("jump argument count differs from join-point parameter count");
                if (cursor < args.len) return .{ .expr = .{ .id = args[cursor], .expected = self.program.types.spanItem(params, cursor) } };
                const loop_params = self.lifted.typedLocalSpan(jump.loop_params);
                const values = self.lifted.exprSpan(jump.loop_values);
                if (loop_params.len != values.len) Common.invariant("jump loop-update parameter count differs from value count");
                if (cursor - args.len < values.len) return .{ .expr = .{ .id = values[cursor - args.len], .expected = self.localTy(loop_params[cursor - args.len].local) } };
            },
            .return_ => |ret| {
                if (cursor == 0) return .{ .expr = .{ .id = ret.value } };
                try self.relateReturnedExpr(ret.value, try self.returnTargetTy(ret.target));
            },
            .dbg, .expect => |child| {
                if (cursor == 0) return .{ .expr = .{ .id = child } };
            },
            .expect_err => |err| {
                if (cursor == 0) return .{ .expr = .{ .id = err.msg } };
            },
            .literal_rejected => |rejected| {
                if (cursor == 0) return .{ .expr = .{ .id = rejected.msg } };
            },
            .comptime_branch_taken => |taken| {
                if (cursor == 0) return .{ .expr = .{ .id = taken.body, .expected = expected } };
            },
        }
        return null;
    }

    /// Terminal expressions retain their checked type for structural consumers,
    /// but produce no value that can flow into a return destination. That
    /// includes a block SpecConstr terminated: its final `unreachable` follows
    /// a statement that never completes.
    fn relateReturnedExpr(self: *Solver, value: Lifted.ExprId, target: Type.TypeVarId) Allocator.Error!void {
        const data = self.lifted.exprs[@intFromEnum(value)].data;
        const tag = std.meta.activeTag(data);
        if (tag == .crash or tag == .checked_error or tag == .comptime_exhaustiveness_failed or tag == .@"unreachable") return;
        if (tag == .block and self.lifted.exprs[@intFromEnum(data.block.final_expr)].data == .@"unreachable") return;
        try self.relateReturn(self.inferredExpr(value), target);
    }

    /// A checked return boundary carries a value from its source row into
    /// the enclosing result row. Relate payload flow without identifying the
    /// rows: a shared callee must retain its own result on every return path.
    fn relateReturn(self: *Solver, source: Type.TypeVarId, target: Type.TypeVarId) Allocator.Error!void {
        var work = std.ArrayList(UnifyPair).empty;
        defer work.deinit(self.allocator);
        var visited = UnifyPairSet.init(self.allocator);
        defer visited.deinit();
        // These pairs are directed, unlike ordinary unification pairs.
        try work.append(self.allocator, .{ .first = source, .second = target });
        while (work.pop()) |pair| {
            const src = self.program.types.rootCompressed(pair.first);
            const dst = self.program.types.rootCompressed(pair.second);
            if (src == dst) continue;
            const entry = try visited.getOrPut(.{ .first = src, .second = dst });
            if (entry.found_existing) continue;
            const source_content = try self.shapeContent(src);
            if (source_content != .tag_union) {
                try self.unify(src, dst);
                continue;
            }
            // The empty row is uninhabited, including when the destination
            // payload is a primitive rather than another row.
            if (source_content.tag_union.count() == 0) continue;
            const target_content = try self.shapeContent(dst);
            if (target_content != .tag_union) Common.invariant("return tag row had a non-row destination");
            const target_index = try self.tagRowIndex(target_content.tag_union);
            for (0..source_content.tag_union.count()) |source_index| {
                const source_tag = self.program.types.tagItem(source_content.tag_union, source_index);
                const target_position = target_index.by_name.get(source_tag.name) orelse
                    Common.invariant("return source tag was absent from its destination row");
                const target_payloads = self.program.types.tagItem(target_content.tag_union, target_position).payloads;
                if (source_tag.payloads.count() != target_payloads.count()) Common.invariant("return tag payload arity differed");
                for (0..source_tag.payloads.count()) |payload_index| {
                    try work.append(self.allocator, .{
                        .first = self.program.types.spanItem(source_tag.payloads, payload_index),
                        .second = self.program.types.spanItem(target_payloads, payload_index),
                    });
                }
            }
        }
    }

    /// Only unchanged fields flow from the base. Replacement fields may have
    /// distinct specialized representations and callable sets.
    fn relateRecordUpdate(self: *Solver, result: Type.TypeVarId, base: Type.TypeVarId, updates: Lifted.Span(Lifted.FieldExpr)) Allocator.Error!void {
        const result_content = try self.shapeContent(result);
        const base_content = try self.shapeContent(base);
        if (result_content != .record or base_content != .record) Common.invariant("record update had a non-record type");
        const result_fields = result_content.record;
        const base_fields = base_content.record;
        if (result_fields.count() != base_fields.count()) Common.invariant("record update changed its field names");
        for (0..result_fields.count()) |index| {
            const result_field = self.program.types.fieldItem(result_fields, index);
            const base_field = self.program.types.fieldItem(base_fields, index);
            if (result_field.name != base_field.name) Common.invariant("record update changed its ordered field names");
            const replaced = for (self.lifted.fieldExprSpan(updates)) |field| {
                if (field.name == result_field.name) break true;
            } else false;
            if (!replaced) try self.unify(result_field.ty, base_field.ty);
        }
    }

    fn captureRequest(self: *Solver, fn_id: Lifted.FnId, span: Lifted.Span(Lifted.CaptureOperand), index: usize) Allocator.Error!?ExprRequest {
        const captures = self.liftedCapturesForFn(fn_id);
        const operands = self.lifted.captureOperandSpan(span);
        if (captures.len != operands.len) Common.invariant("capture operand count differs from its target");
        if (index == operands.len) return null;
        if (operands[index].id != self.lifted.captureIdOfLocal(captures[index].local)) Common.invariant("capture operand CaptureId did not match its slot");
        return .{ .expr = .{ .id = operands[index].value, .expected = self.localTy(captures[index].local) } };
    }

    fn bindPattern(self: *Solver, pat_id: Lifted.PatId, value_ty: Type.TypeVarId) Allocator.Error!void {
        try self.runPatternBinds(.{ .pattern = .{ .pat = pat_id, .value_ty = value_ty } });
    }

    /// One pending step of binding a pattern.
    const PatternBind = union(enum) {
        /// Bind a pattern against the value type it matches.
        pattern: struct { pat: Lifted.PatId, value_ty: Type.TypeVarId },
        /// Bind a pattern at its own type.
        at_type: struct { pat: Lifted.PatId, ty: Type.TypeVarId },
        /// Bind the next subpattern of a destructuring pattern.
        child: PatternChildren,
    };

    const PatternChildren = struct {
        pat: Lifted.PatId,
        ty: Type.TypeVarId,
        index: usize = 0,
        /// A tuple's item types or a tag's payload types.
        component_tys: Type.Span = undefined,
        /// A list's element type.
        elem_ty: Type.TypeVarId = undefined,
    };

    /// Bind patterns on an explicit stack, so pattern nesting never becomes
    /// native call depth. Each subpattern is bound, and each subpattern's
    /// type is read, in the order a direct recursive binding did.
    fn runPatternBinds(self: *Solver, root: PatternBind) Allocator.Error!void {
        var pending: std.ArrayList(PatternBind) = .empty;
        defer pending.deinit(self.allocator);
        try pending.append(self.allocator, root);
        while (pending.pop()) |step| switch (step) {
            .pattern => |bind| {
                const index = @intFromEnum(bind.pat);
                if (self.generated_backing_pats[index]) {
                    const pat_ty = self.pat_tys[index] orelse Common.invariant("generated backing pattern was marked before its type was assigned");
                    try self.unifyGeneratedOpaqueBacking(pat_ty, bind.value_ty);
                    continue;
                }
                const pat_ty = try self.expectPat(bind.pat, bind.value_ty);
                try pending.append(self.allocator, .{ .at_type = .{ .pat = bind.pat, .ty = pat_ty } });
            },
            .at_type => |bind| try self.bindPatternNode(bind.pat, bind.ty, &pending),
            .child => |children| {
                var next = children;
                const child = try self.nextPatternChild(&next) orelse continue;
                try pending.append(self.allocator, .{ .child = next });
                try pending.append(self.allocator, .{ .pattern = child });
            },
        };
    }

    /// Bind a pattern's own variables at `pat_ty` and queue its subpatterns.
    fn bindPatternNode(
        self: *Solver,
        pat_id: Lifted.PatId,
        pat_ty: Type.TypeVarId,
        pending: *std.ArrayList(PatternBind),
    ) Allocator.Error!void {
        const pat = self.lifted.pats[@intFromEnum(pat_id)];
        var children = PatternChildren{ .pat = pat_id, .ty = pat_ty };
        switch (pat.data) {
            .bind => |local| return try self.unify(self.localTy(local), pat_ty),
            .wildcard,
            .int_lit,
            .dec_lit,
            .frac_f32_lit,
            .frac_f64_lit,
            .str_lit,
            => return,
            .as => |as| {
                try self.unify(self.localTy(as.local), pat_ty);
                return try pending.append(self.allocator, .{ .pattern = .{ .pat = as.pattern, .value_ty = pat_ty } });
            },
            .str_pattern, .record => {},
            .tuple => |items| {
                children.component_tys = try self.tupleItemsSpan(pat_ty);
                if (children.component_tys.count() != self.lifted.patSpan(items).len) Common.invariant("tuple pattern arity differs from its checked type");
            },
            .list => children.elem_ty = try self.listElem(pat_ty),
            .tag => |tag| {
                children.component_tys = try self.tagPayloadsSpan(pat_ty, tag.name);
                if (children.component_tys.count() != self.lifted.patSpan(tag.payloads).len) Common.invariant("tag pattern payload arity differs from its checked type");
            },
            .nominal => |backing| {
                if (try self.hasBuiltinOwner(pat_ty, .fields) or try self.hasBuiltinOwner(pat_ty, .field)) {
                    const backing_index = @intFromEnum(backing);
                    if (self.generated_backing_pats[backing_index]) return;
                    self.generated_backing_pats[backing_index] = true;
                    const backing_ty = try self.lowerTypeFresh(self.lifted.pats[backing_index].ty);
                    self.pat_tys[backing_index] = backing_ty;
                    return try pending.append(self.allocator, .{ .at_type = .{ .pat = backing, .ty = backing_ty } });
                }
                const backing_ty = try self.namedBacking(pat_ty) orelse pat_ty;
                return try pending.append(self.allocator, .{ .pattern = .{ .pat = backing, .value_ty = backing_ty } });
            },
        }
        try pending.append(self.allocator, .{ .child = children });
    }

    /// The next subpattern of a destructuring pattern with the type it is
    /// bound against, advancing `children`; null after the last.
    fn nextPatternChild(self: *Solver, children: *PatternChildren) Allocator.Error!?@FieldType(PatternBind, "pattern") {
        const pat = self.lifted.pats[@intFromEnum(children.pat)];
        const index = children.index;
        children.index += 1;
        switch (pat.data) {
            .str_pattern => |str| {
                const steps = self.lifted.strPatternStepSpan(str.steps);
                var step_index = index;
                while (step_index < steps.len) : (step_index += 1) {
                    if (steps[step_index].capture) |capture| {
                        children.index = step_index + 1;
                        return .{ .pat = capture, .value_ty = children.ty };
                    }
                }
                return null;
            },
            .record => |fields| {
                const destructs = self.lifted.recordDestructSpan(fields);
                if (index == destructs.len) return null;
                const field = destructs[index];
                return .{ .pat = field.pattern, .value_ty = try self.recordField(children.ty, field.name) };
            },
            .tuple => |items| {
                const pats = self.lifted.patSpan(items);
                if (index == pats.len) return null;
                return .{ .pat = pats[index], .value_ty = self.program.types.spanItem(children.component_tys, index) };
            },
            .list => |list| {
                const pats = self.lifted.patSpan(list.patterns);
                if (index < pats.len) return .{ .pat = pats[index], .value_ty = children.elem_ty };
                // A captured rest is itself a list with the same element type.
                if (index == pats.len) if (list.rest) |rest| if (rest.pattern) |rest_pattern| return .{ .pat = rest_pattern, .value_ty = children.ty };
                return null;
            },
            .tag => |tag| {
                const payloads = self.lifted.patSpan(tag.payloads);
                if (index == payloads.len) return null;
                return .{ .pat = payloads[index], .value_ty = self.program.types.spanItem(children.component_tys, index) };
            },
            .bind, .wildcard, .int_lit, .dec_lit, .frac_f32_lit, .frac_f64_lit, .str_lit, .as, .nominal => Common.invariant("pattern without subpattern children resumed its children"),
        }
    }

    fn unifyGeneratedOpaqueBacking(self: *Solver, generated_ty: Type.TypeVarId, expected_ty: Type.TypeVarId) Allocator.Error!void {
        const generated = self.program.types.rootCompressed(generated_ty);
        const expected = self.program.types.rootCompressed(expected_ty);
        if (generated == expected) return;
        // The caller reached this path only through the backing pattern of a
        // `Fields` or `Field` nominal, whose backing is compiler-generated.
        // Preserve that explicit generated backing deterministically;
        // structural size is not an authority signal.
        self.program.types.set(expected, .{ .link = generated });
    }

    fn expectExpr(self: *Solver, expr_id: Lifted.ExprId, expected: Type.TypeVarId) Allocator.Error!Type.TypeVarId {
        const slot = try self.expectExprSlot(expr_id, expected);
        const inferred = try self.inferExpr(expr_id);
        try self.unify(slot, inferred);
        return self.program.types.rootCompressed(slot);
    }

    fn exprSlot(self: *Solver, expr_id: Lifted.ExprId) Allocator.Error!Type.TypeVarId {
        const index = @intFromEnum(expr_id);
        if (self.expr_tys[index]) |ty| return ty;

        const expr = self.lifted.exprs[index];
        const tag = std.meta.activeTag(expr.data);
        const ty = if (tag == .local)
            self.localTy(expr.data.local)
        else if (tag == .fn_ref)
            self.program.fn_tys.items[@intFromEnum(expr.data.fn_ref.fn_id)]
        else if (tag == .call_proc)
            switch (Lifted.directCallee(expr.data.call_proc)) {
                .local => |callee| (try self.functionShape(self.program.fn_tys.items[@intFromEnum(callee)])).ret,
            }
        else
            try self.lowerTypeFresh(expr.ty);
        self.expr_tys[index] = ty;
        return ty;
    }

    fn expectExprSlot(self: *Solver, expr_id: Lifted.ExprId, expected: Type.TypeVarId) Allocator.Error!Type.TypeVarId {
        const index = @intFromEnum(expr_id);
        if (self.expr_tys[index]) |ty| {
            try self.unify(ty, expected);
            return self.program.types.rootCompressed(ty);
        }

        const expr = self.lifted.exprs[index];
        const tag = std.meta.activeTag(expr.data);
        const ty = if (tag == .local)
            self.localTy(expr.data.local)
        else if (tag == .fn_ref)
            self.program.fn_tys.items[@intFromEnum(expr.data.fn_ref.fn_id)]
        else
            expected;
        try self.unify(ty, expected);
        self.expr_tys[index] = ty;
        return self.program.types.rootCompressed(ty);
    }

    fn expectPat(self: *Solver, pat_id: Lifted.PatId, expected: Type.TypeVarId) Allocator.Error!Type.TypeVarId {
        const index = @intFromEnum(pat_id);
        if (self.pat_tys[index]) |ty| {
            try self.unify(ty, expected);
            return self.program.types.rootCompressed(ty);
        }

        const pat = self.lifted.pats[index];
        const tag = std.meta.activeTag(pat.data);
        const ty = if (tag == .bind)
            self.localTy(pat.data.bind)
        else if (tag == .as)
            self.localTy(pat.data.as.local)
        else
            expected;
        try self.unify(ty, expected);
        self.pat_tys[index] = ty;
        return self.program.types.rootCompressed(ty);
    }

    fn functionShape(self: *Solver, ty: Type.TypeVarId) Allocator.Error!FunctionShape {
        const content = try self.shapeContent(ty);
        if (std.meta.activeTag(content) != .func) Common.invariant("call expression had a non-function checked type");
        return .{ .args = content.func.args, .callable = content.func.callable, .ret = content.func.ret };
    }

    fn liftedCapturesForFn(self: *Solver, fn_id: Lifted.FnId) []const Lifted.TypedLocal {
        return self.lifted.typedLocalSpan(self.lifted.fns[@intFromEnum(fn_id)].captures);
    }

    fn localTy(self: *Solver, local: Lifted.LocalId) Type.TypeVarId {
        return self.local_tys[@intFromEnum(local)] orelse Common.invariant("Lambda Solved local reached solver without a type slot");
    }

    fn returnTargetTy(self: *Solver, target: MonoType.TypeId) Allocator.Error!Type.TypeVarId {
        if (self.return_contexts.items.len == 0) Common.invariant("return expression reached Lambda Solved outside a function");
        const context = self.return_contexts.items[self.return_contexts.items.len - 1];
        if (!try self.sameMonoType(target, context.mono_ret)) {
            Common.invariant("return target type differed from enclosing function return type");
        }
        return context.solved_ret;
    }

    fn markForcedDynamicIteratorCallables(self: *Solver) Allocator.Error!void {
        self.program.types.compressAllRoots();
        // Expanding a leaf appends fresh child vars, so the bound is re-read
        // every iteration and an expanded var is revisited in place.
        var index: usize = 0;
        while (index < self.program.types.vars.items.len) : (index += 1) {
            const ty: Type.TypeVarId = @enumFromInt(@as(u32, @intCast(index)));
            if (self.program.types.rootCompressed(ty) != ty) continue;
            const content = self.program.types.get(ty);
            const tag = std.meta.activeTag(content);
            if (tag == .named) {
                if (content.named.def.iterator_representation == .forced_dynamic) {
                    try self.markErasedCallablesReachedByType(ty);
                }
            } else if (tag == .mono) {
                if (self.contains_forced_dynamic[@intFromEnum(content.mono.id)]) {
                    _ = try self.expandMonoRoot(ty, content.mono);
                    index -= 1;
                }
            }
        }
    }

    fn sameMonoType(self: *Solver, a: MonoType.TypeId, b: MonoType.TypeId) Allocator.Error!bool {
        if (a == b) return true;
        return try self.lifted.types.typeEql(self.allocator, self.lifted.names, a, b);
    }

    fn currentLoopResult(self: *Solver) Type.TypeVarId {
        if (self.loop_results.items.len == 0) Common.invariant("break expression reached Lambda Solved outside a loop");
        return self.loop_results.items[self.loop_results.items.len - 1];
    }

    fn currentLoopParams(self: *Solver) Type.Span {
        if (self.loop_params.items.len == 0) Common.invariant("continue expression reached Lambda Solved outside a loop");
        return self.loop_params.items[self.loop_params.items.len - 1];
    }

    fn activeJoinPoint(self: *Solver, id: Lifted.JoinPointId) ActiveJoinPoint {
        var index = self.join_points.items.len;
        while (index > 0) {
            index -= 1;
            const join_point = self.join_points.items[index];
            if (join_point.id == id) return join_point;
        }
        Common.invariant("jump expression referenced a join point outside its lexical scope");
    }

    fn markErasedCallablesReachedByType(self: *Solver, ty: Type.TypeVarId) Allocator.Error!void {
        var visited = self.solved_set_pool.acquire();
        defer self.solved_set_pool.release(&visited);
        try self.markErasedCallablesReachedByTypeInner(ty, &visited);
    }

    /// Marking a root erases every callable it reaches, and marking it again
    /// in the same traversal changes nothing, so each root is visited once.
    /// Types are visited in preorder on an explicit stack.
    fn markErasedCallablesReachedByTypeInner(
        self: *Solver,
        ty: Type.TypeVarId,
        visited: *collections.DenseMap(Type.TypeVarId, void),
    ) Allocator.Error!void {
        const types = &self.program.types;
        var pending: std.ArrayList(Type.TypeVarId) = .empty;
        defer pending.deinit(self.allocator);
        try pending.append(self.allocator, ty);
        while (pending.pop()) |next| {
            const root = types.rootCompressed(next);
            if ((try visited.getOrPut(root)).found_existing) continue;

            const content = types.get(root);
            const resolved = if (std.meta.activeTag(content) == .mono)
                // Callable-free leaves contain nothing this walk could mark.
                if (self.contains_callable[@intFromEnum(content.mono.id)])
                    try self.expandMonoRoot(root, content.mono)
                else
                    continue
            else
                content;
            // Children are pushed last-first so the first child is visited
            // next.
            const children_start = pending.items.len;
            switch (resolved) {
                .mono => Common.invariant("lazy Monotype leaf reached erased-callable marking unexpanded"),
                .link => Common.invariant("Lambda Solved root returned a link"),
                .unbound, .forall, .primitive, .zst => {},
                .erased => |erased| try self.appendCallableCaptureTypes(erased.members, &pending),
                .func => |func| {
                    // An erased callable already has this marking's effect.
                    if (std.meta.activeTag(types.get(types.rootCompressed(func.callable))) != .erased) {
                        const erased = try types.add(.{ .erased = .{
                            .source_fn_ty = try self.solvedTypeDigest(root),
                            .members = .empty(),
                            .abi_fn = root,
                        } });
                        try self.unify(func.callable, erased);
                    }
                    for (0..func.args.count()) |index| try pending.append(self.allocator, types.spanItem(func.args, index));
                    try pending.append(self.allocator, func.ret);
                },
                .list, .box => |elem| try pending.append(self.allocator, elem),
                .tuple => |items| for (0..items.count()) |index| try pending.append(self.allocator, types.spanItem(items, index)),
                .record => |fields| for (0..fields.count()) |index| {
                    const field = types.fieldItem(fields, index);
                    try pending.append(self.allocator, field.ty);
                    if (field.value_ty) |value_ty| try pending.append(self.allocator, value_ty);
                },
                .tag_union => |tags| for (0..tags.count()) |tag_index| {
                    const tag = types.tagItem(tags, tag_index);
                    for (0..tag.payloads.count()) |payload_index| try pending.append(self.allocator, types.spanItem(tag.payloads, payload_index));
                },
                .named => |named| {
                    for (0..named.args.count()) |index| try pending.append(self.allocator, types.spanItem(named.args, index));
                    if (named.backing) |backing| try pending.append(self.allocator, backing.ty);
                },
                .lambda_set => |members| try self.appendCallableCaptureTypes(members, &pending),
            }
            std.mem.reverse(Type.TypeVarId, pending.items[children_start..]);
        }
    }

    /// New lazy leaf for a lifted Monotype. Each use owns its var; the leaf
    /// materializes one level at a time as unification or shape reads touch
    /// it, and `finalizeMonoLeaves` replaces whatever survives solving.
    fn monoLeaf(self: *Solver, ty: MonoType.TypeId) Allocator.Error!Type.TypeVarId {
        return try self.program.types.add(.{ .mono = .{ .id = ty } });
    }

    fn lowerTypeFresh(self: *Solver, ty: MonoType.TypeId) Allocator.Error!Type.TypeVarId {
        return try self.monoLeaf(ty);
    }

    const MonoLeaf = std.meta.fieldInfo(Type.Content, .mono).type;

    /// Materialize a lazy leaf's root one level in place: children become new
    /// leaves and function callable slots start unbound, exactly as an eager
    /// clone's would. The leaf's clone context ties recursive back-references
    /// to their existing vars, so a recursive Monotype materializes as the
    /// same cyclic graph an eager clone produced.
    fn expandMonoRoot(self: *Solver, root: Type.TypeVarId, leaf: MonoLeaf) Allocator.Error!Type.Content {
        const ctx: u32 = if (leaf.ctx != Type.no_leaf_context) leaf.ctx else blk: {
            if (self.isCallableFree(leaf.id)) break :blk try self.sharedLeafContext();
            const index: u32 = @intCast(self.leaf_contexts.items.len);
            try self.leaf_contexts.append(self.allocator, collections.DenseMap(MonoType.TypeId, Type.TypeVarId).init(self.allocator));
            break :blk index;
        };
        if (self.leaf_contexts.items[ctx].get(leaf.id)) |existing| {
            const existing_root = self.program.types.rootCompressed(existing);
            if (existing_root != root) {
                self.program.types.set(root, .{ .link = existing_root });
                return try self.resolvedContentAt(existing_root);
            }
        } else {
            try self.leaf_contexts.items[ctx].put(leaf.id, root);
        }
        var cloner = TypeCloner.init(self);
        cloner.lazy_ctx = ctx;
        defer cloner.deinit();
        const content = try cloner.lowerContent(self.lifted.types.get(leaf.id));
        self.program.types.set(root, content);
        self.registerNamedBacking(content);
        return content;
    }

    /// Whether clones of this Monotype carry no callable slot and no
    /// forced-dynamic iterator, so every clone of it solves identically.
    fn isCallableFree(self: *const Solver, ty: MonoType.TypeId) bool {
        const raw_id = @intFromEnum(ty);
        return !self.contains_callable[raw_id] and !self.contains_forced_dynamic[raw_id];
    }

    /// Give a build lists from the pool, or empty ones.
    fn acquireCloneLists(self: *Solver, build: *TypeCloner.CloneBuild) void {
        const lists = self.spare_clone_lists.pop() orelse TypeCloner.CloneLists{};
        build.parts = lists.parts;
        build.results = lists.results;
        build.spans = lists.spans;
    }

    /// Keep a build's lists for the next build; when the pool cannot grow,
    /// their capacity is released instead.
    fn releaseCloneLists(self: *Solver, build: *TypeCloner.CloneBuild) void {
        var lists = TypeCloner.CloneLists{ .parts = build.parts, .results = build.results, .spans = build.spans };
        build.parts = .empty;
        build.results = .empty;
        build.spans = .empty;
        lists.parts.clearRetainingCapacity();
        lists.results.clearRetainingCapacity();
        lists.spans.clearRetainingCapacity();
        self.spare_clone_lists.append(self.allocator, lists) catch lists.deinit(self.allocator);
    }

    /// The one clone context every callable-free leaf materializes in.
    fn sharedLeafContext(self: *Solver) Allocator.Error!u32 {
        if (self.shared_leaf_context) |shared| return shared;
        const index: u32 = @intCast(self.leaf_contexts.items.len);
        try self.leaf_contexts.append(self.allocator, collections.DenseMap(MonoType.TypeId, Type.TypeVarId).init(self.allocator));
        self.shared_leaf_context = index;
        return index;
    }

    fn registerNamedBacking(self: *Solver, content: Type.Content) void {
        if (content == .named) {
            if (content.named.backing) |backing| self.program.types.markNamedBacking(backing.ty);
        }
    }

    fn resolvedContentAt(self: *Solver, root: Type.TypeVarId) Allocator.Error!Type.Content {
        const content = self.program.types.get(root);
        if (std.meta.activeTag(content) == .mono) return try self.expandMonoRoot(root, content.mono);
        return content;
    }

    fn resolvedContent(self: *Solver, ty: Type.TypeVarId) Allocator.Error!Type.Content {
        return try self.resolvedContentAt(self.program.types.rootCompressed(ty));
    }

    /// Replace every output-reachable lazy leaf with a link to a materialized
    /// clone so program views never observe one. Untouched leaves of one
    /// callable-free Monotype share one clone; callable-bearing leaves get a
    /// private clone whose callable-free subgraphs still share.
    fn finalizeMonoLeaves(self: *Solver) Allocator.Error!void {
        var visited = collections.DenseMap(Type.TypeVarId, void).init(self.allocator);
        defer visited.deinit();
        var work = std.ArrayList(Type.TypeVarId).empty;
        defer work.deinit(self.allocator);

        for (self.program.defs.items) |def| try work.append(self.allocator, def.ty);
        try work.appendSlice(self.allocator, self.program.fn_tys.items);
        try work.appendSlice(self.allocator, self.program.local_tys.items);
        try work.appendSlice(self.allocator, self.program.expr_tys.items);
        try work.appendSlice(self.allocator, self.program.pat_tys.items);
        for (self.program.layout_requests.items) |request| try work.append(self.allocator, request.ty);
        for (self.program.runtime_schema_requests.items) |request| try work.append(self.allocator, request.ty);

        while (work.pop()) |ty| {
            const root = self.program.types.rootCompressed(ty);
            const gop = try visited.getOrPut(root);
            if (gop.found_existing) continue;
            switch (self.program.types.get(root)) {
                .link => Common.invariant("Lambda Solved root returned a link"),
                .mono => |leaf| {
                    const clone = try self.finalMonoClone(leaf.id);
                    self.program.types.set(root, .{ .link = self.program.types.rootCompressed(clone) });
                },
                .unbound, .forall, .primitive, .zst => {},
                .list, .box => |elem| try work.append(self.allocator, elem),
                .tuple => |items| for (0..items.count()) |index| {
                    try work.append(self.allocator, self.program.types.spanItem(items, index));
                },
                .record => |fields| for (0..fields.count()) |index| {
                    const field = self.program.types.fieldItem(fields, index);
                    try work.append(self.allocator, field.ty);
                    if (field.value_ty) |value_ty| try work.append(self.allocator, value_ty);
                },
                .tag_union => |tags| for (0..tags.count()) |tag_index| {
                    const tag = self.program.types.tagItem(tags, tag_index);
                    for (0..tag.payloads.count()) |payload_index| {
                        try work.append(self.allocator, self.program.types.spanItem(tag.payloads, payload_index));
                    }
                },
                .func => |func| {
                    for (0..func.args.count()) |index| {
                        try work.append(self.allocator, self.program.types.spanItem(func.args, index));
                    }
                    try work.append(self.allocator, func.callable);
                    try work.append(self.allocator, func.ret);
                },
                .named => |named| {
                    for (0..named.args.count()) |index| {
                        try work.append(self.allocator, self.program.types.spanItem(named.args, index));
                    }
                    if (named.backing) |backing| try work.append(self.allocator, backing.ty);
                    for (0..named.declared_order.count()) |index| switch (self.program.types.declaredFieldItem(named.declared_order, index)) {
                        .named => {},
                        .padding => |padding_ty| try work.append(self.allocator, padding_ty),
                    };
                },
                .lambda_set => |members| for (0..members.count()) |member_index| {
                    const member = self.program.types.memberItem(members, member_index);
                    for (0..member.captures.count()) |capture_index| {
                        try work.append(self.allocator, self.program.types.captureItem(member.captures, capture_index).ty);
                    }
                },
                .erased => |erased| for (0..erased.members.count()) |member_index| {
                    const member = self.program.types.memberItem(erased.members, member_index);
                    for (0..member.captures.count()) |capture_index| {
                        try work.append(self.allocator, self.program.types.captureItem(member.captures, capture_index).ty);
                    }
                },
            }
        }
    }

    /// Materialized clone for a leaf that survived solving, matching what the
    /// post-solve eager clone produced: shared for callable-free Monotypes,
    /// self-marking for forced-dynamic iterator content.
    fn finalMonoClone(self: *Solver, id: MonoType.TypeId) Allocator.Error!Type.TypeVarId {
        var cloner = TypeCloner.init(self);
        cloner.share = true;
        defer cloner.deinit();
        const lowered = try cloner.lower(id);
        try cloner.markForcedDynamicCallables();
        return lowered;
    }

    fn listElem(self: *Solver, ty: Type.TypeVarId) Allocator.Error!Type.TypeVarId {
        const content = try self.shapeContent(ty);
        if (std.meta.activeTag(content) != .list) Common.invariant("list expression had a non-list checked type");
        return content.list;
    }

    fn tupleItemsSpan(self: *Solver, ty: Type.TypeVarId) Allocator.Error!Type.Span {
        const content = try self.shapeContent(ty);
        if (std.meta.activeTag(content) != .tuple) Common.invariant("tuple expression had a non-tuple checked type");
        return content.tuple;
    }

    fn recordField(self: *Solver, ty: Type.TypeVarId, name: Type.names.RecordFieldNameId) Allocator.Error!Type.TypeVarId {
        const content = try self.shapeContent(ty);
        if (std.meta.activeTag(content) != .record) Common.invariant("record field operation had a non-record checked type");
        const index = (try self.recordRowIndex(content.record)).by_name.get(name) orelse
            Common.invariant("record field was absent from checked record type");
        return self.program.types.fieldItem(content.record, index).ty;
    }

    fn recordFieldByLabel(self: *Solver, ty: Type.TypeVarId, label: []const u8) Allocator.Error!Type.TypeVarId {
        const content = try self.shapeContent(ty);
        if (std.meta.activeTag(content) != .record) Common.invariant("low-level record result had a non-record checked type");
        for (0..content.record.count()) |index| {
            const field = self.program.types.fieldItem(content.record, index);
            if (std.mem.eql(u8, self.lifted.names.recordFieldLabelText(field.name), label)) return field.ty;
        }
        Common.invariant("low-level record result was missing a required field");
    }

    fn tagPayloadsSpan(self: *Solver, ty: Type.TypeVarId, name: Type.names.TagNameId) Allocator.Error!Type.Span {
        const content = try self.shapeContent(ty);
        if (std.meta.activeTag(content) != .tag_union) Common.invariant("tag operation had a non-tag-union checked type");
        const index = (try self.tagRowIndex(content.tag_union)).by_name.get(name) orelse
            Common.invariant("tag was absent from checked tag-union type");
        return self.program.types.tagItem(content.tag_union, index).payloads;
    }

    fn namedBacking(self: *Solver, ty: Type.TypeVarId) Allocator.Error!?Type.TypeVarId {
        const content = try self.resolvedContent(ty);
        if (std.meta.activeTag(content) != .named) return null;
        return if (content.named.backing) |backing| backing.ty else null;
    }

    fn hasBuiltinOwner(self: *Solver, ty: Type.TypeVarId, owner: static_dispatch.BuiltinOwner) Allocator.Error!bool {
        const content = try self.resolvedContent(ty);
        if (std.meta.activeTag(content) != .named) return false;
        return if (content.named.builtin_owner) |builtin_owner| builtin_owner == owner else false;
    }

    fn bindLowLevelTypes(
        self: *Solver,
        op: can.CIR.Expr.LowLevel,
        expected: Type.TypeVarId,
        args: []const Type.TypeVarId,
    ) Allocator.Error!void {
        const bound_op = std.meta.stringToEnum(BoundLowLevel, @tagName(op)) orelse return;
        switch (bound_op) {
            .box_box => {
                expectLowLevelArity(op, args, 1);
                try self.unify(args[0], try self.boxElem(expected));
                try self.markErasedCallablesReachedByType(args[0]);
            },
            .box_unbox => {
                expectLowLevelArity(op, args, 1);
                try self.unify(expected, try self.boxElem(args[0]));
                try self.markErasedCallablesReachedByType(expected);
            },
            .list_get_unsafe => {
                expectLowLevelArity(op, args, 2);
                try self.unify(expected, try self.listElem(args[0]));
            },
            .list_append_unsafe => {
                expectLowLevelArity(op, args, 2);
                try self.unify(expected, args[0]);
                try self.unify(args[1], try self.listElem(expected));
            },
            .list_concat => {
                expectLowLevelArity(op, args, 2);
                try self.unify(expected, args[0]);
                try self.unify(expected, args[1]);
            },
            .list_reserve,
            .list_reserve_for_append,
            .list_drop_at,
            .list_sublist,
            .list_take_first,
            .list_take_last,
            .list_drop_first,
            .list_drop_last,
            => {
                expectLowLevelArity(op, args, 2);
                try self.unify(expected, args[0]);
            },
            .list_release_excess_capacity,
            .list_clear,
            .list_reverse,
            => {
                expectLowLevelArity(op, args, 1);
                try self.unify(expected, args[0]);
            },
            .list_set, .list_map_write_unsafe => {
                expectLowLevelArity(op, args, 3);
                try self.unify(expected, args[0]);
                try self.unify(args[2], try self.listElem(expected));
            },
            .list_replace_unsafe => {
                expectLowLevelArity(op, args, 3);
                const elem = try self.listElem(args[0]);
                try self.unify(args[2], elem);
                try self.unify(try self.recordFieldByLabel(expected, "list"), args[0]);
                try self.unify(try self.recordFieldByLabel(expected, "prev"), elem);
            },
            .list_swap => {
                expectLowLevelArity(op, args, 3);
                try self.unify(expected, args[0]);
            },
            .list_prepend => {
                expectLowLevelArity(op, args, 2);
                try self.unify(expected, args[0]);
                try self.unify(args[1], try self.listElem(expected));
            },
            .list_map_prepare_reuse => {
                expectLowLevelArity(op, args, 1);
                try self.unify(expected, args[0]);
            },
            .list_prefetched => {
                expectLowLevelArity(op, args, 2);
                try self.unify(expected, args[0]);
            },
            .list_map_can_reuse => {
                expectLowLevelArity(op, args, 2);
                const transform = try self.functionShape(args[1]);
                if (transform.args.count() != 1) Common.invariant("list map transform must have one argument");
                try self.unify(try self.listElem(args[0]), self.program.types.spanItem(transform.args, 0));
            },
            .dict_pseudo_seed => expectLowLevelArity(op, args, 0),
            .hasher_finish => expectLowLevelArity(op, args, 1),
            .crypto_sha256_hash_bytes,
            .crypto_sha256_hasher_finish,
            .crypto_blake3_hash_bytes,
            .crypto_blake3_hasher_finish,
            => expectLowLevelArity(op, args, 1),
            .crypto_sha256_hasher_empty,
            .crypto_blake3_hasher_empty,
            => expectLowLevelArity(op, args, 0),
            .crypto_sha256_hasher_write,
            .crypto_blake3_hasher_write,
            => expectLowLevelArity(op, args, 2),
            .hasher_write_bool,
            .hasher_write_u8,
            .hasher_write_u16,
            .hasher_write_u32,
            .hasher_write_u64,
            .hasher_write_u128,
            .hasher_write_i8,
            .hasher_write_i16,
            .hasher_write_i32,
            .hasher_write_i64,
            .hasher_write_i128,
            .hasher_write_f32,
            .hasher_write_f64,
            .hasher_write_dec,
            .hasher_write_bytes,
            .hasher_write_str,
            => expectLowLevelArity(op, args, 2),
        }
    }

    fn expectLowLevelArity(
        op: can.CIR.Expr.LowLevel,
        args: []const Type.TypeVarId,
        expected: usize,
    ) void {
        if (args.len == expected) return;

        if (@import("builtin").mode == .Debug) {
            std.debug.panic(
                "postcheck invariant violated: low-level op {s} had {d} args, expected {d}",
                .{ @tagName(op), args.len, expected },
            );
        }
        unreachable;
    }

    fn boxElem(self: *Solver, ty: Type.TypeVarId) Allocator.Error!Type.TypeVarId {
        const content = try self.shapeContent(ty);
        if (std.meta.activeTag(content) != .box) Common.invariant("box low-level operation had a non-box checked type");
        return content.box;
    }

    fn shapeContent(self: *Solver, ty: Type.TypeVarId) Allocator.Error!Type.Content {
        var current = self.program.types.rootCompressed(ty);
        while (true) {
            const content = try self.resolvedContentAt(current);
            if (std.meta.activeTag(content) != .named) return content;
            const backing = content.named.backing orelse return content;
            current = self.program.types.rootCompressed(backing.ty);
        }
    }

    /// Drive unification from an explicit stack so structural nesting costs
    /// heap frames instead of call frames. The loop only owns the frames it
    /// pushed above `base`, so the helpers below may call back into `unify`
    /// while an outer unification still has pending frames underneath.
    fn unify(self: *Solver, lhs: Type.TypeVarId, rhs: Type.TypeVarId) Allocator.Error!void {
        const base = self.unify_stack.items.len;
        try self.pushUnifyPair(&self.unify_stack, lhs, rhs);
        try self.drainUnifyStack(base);
    }

    /// Unify a root-slot read with the declared root's return, which owns
    /// the root's type. Each use of an imported constant is checked at its
    /// own copy of the constant's type, and a copy may lift a record or tag
    /// union into a nominal the root never names. Such a lifted pair, and
    /// every pair containing one, unifies its components without joining the
    /// two types: callable slots are shared, and no read changes the type
    /// the root is evaluated, stored, and cached at.
    fn unifyComptimeValueRead(
        self: *Solver,
        read_mono: MonoType.TypeId,
        read_ty: Type.TypeVarId,
        initializer: Lifted.ExprId,
    ) Allocator.Error!void {
        if (self.program.types.get(self.program.types.rootCompressed(read_ty)) == .unbound) {
            try self.unify(read_ty, try self.lowerTypeFresh(read_mono));
        }
        // Monotype sealed both types, so a read with no callable slot has no
        // Lambda Solved evidence to take from its root.
        if (self.isCallableFree(read_mono)) return;
        const root_ty = self.inferredExpr(initializer);
        const initializer_expr = self.lifted.exprs[@intFromEnum(initializer)];
        if (initializer_expr.data != .call_proc) return self.unify(read_ty, root_ty);
        const root_fn = switch (Lifted.directCallee(initializer_expr.data.call_proc)) {
            .local => |callee| callee,
        };
        // A read at the root's own Monotype type cannot contain a lift.
        if (self.lifted.fns[@intFromEnum(root_fn)].ret == read_mono) return self.unify(read_ty, root_ty);

        const entry = try self.comptime_read_tys.getOrPut(self.allocator, .{ .root_fn = root_fn, .read_mono = read_mono });
        if (entry.found_existing) {
            const read_root = self.program.types.rootCompressed(read_ty);
            const shared = self.program.types.rootCompressed(entry.value_ptr.*);
            if (read_root == shared) return;
            const raw = self.program.types.get(read_root);
            if (raw == .mono and raw.mono.id == read_mono and raw.mono.ctx == Type.no_leaf_context) {
                self.program.types.set(read_root, .{ .link = shared });
                return;
            }
            return self.unify(read_ty, shared);
        }
        entry.value_ptr.* = read_ty;

        const was_preserving = self.preserving_lifted_roots;
        self.preserving_lifted_roots = true;
        defer self.preserving_lifted_roots = was_preserving;
        try self.unify(read_ty, root_ty);
    }

    const ComptimeReadKey = struct { root_fn: Lifted.FnId, read_mono: MonoType.TypeId };

    /// Relate generated-private evidence from a checked-public shape into
    /// its private representation, on the unification stack.
    fn relateGeneratedPrivateEvidence(self: *Solver, public_ty: Type.TypeVarId, private_ty: Type.TypeVarId) Allocator.Error!void {
        const base = self.unify_stack.items.len;
        try self.pushRelate(&self.unify_stack, public_ty, private_ty);
        try self.drainUnifyStack(base);
    }

    fn drainUnifyStack(self: *Solver, base: usize) Allocator.Error!void {
        while (self.unify_stack.items.len > base) {
            const frame = self.unify_stack.pop().?;
            switch (frame) {
                .process => |process| try self.processUnifyPair(
                    &self.unify_stack,
                    process.lhs,
                    process.rhs,
                    process.structural_isolated,
                ),
                .finish => |finish| {
                    if (!(self.preserving_lifted_roots and finish.lifts_before != self.lift_count and joinsTypeRoots(finish.action))) {
                        self.applyUnifyFinish(finish.action);
                    }
                    _ = self.active_unifications.remove(finish.pair);
                },
                .relate => |relate| try self.processRelate(&self.unify_stack, relate.public, relate.private),
                .relate_exit => |pair| _ = self.active_private_evidence_relations.remove(pair),
            }
        }
        // Every pair retires as its frame finishes, so an empty stack leaves
        // both active sets empty; clearing them drops the removal markers
        // that would otherwise lengthen every later probe.
        if (self.unify_stack.items.len == 0) {
            if (self.active_unifications.count() == 0) self.active_unifications.clearRetainingCapacity();
            if (self.active_private_evidence_relations.count() == 0) self.active_private_evidence_relations.clearRetainingCapacity();
        }
    }

    fn pushUnifyPair(
        self: *Solver,
        stack: *std.ArrayList(UnifyFrame),
        lhs: Type.TypeVarId,
        rhs: Type.TypeVarId,
    ) Allocator.Error!void {
        try stack.append(self.allocator, .{ .process = .{
            .lhs = lhs,
            .rhs = rhs,
            .structural_isolated = false,
        } });
    }

    fn pushIsolatedStructuralPair(
        self: *Solver,
        stack: *std.ArrayList(UnifyFrame),
        structural_ty: Type.TypeVarId,
        backing_ty: Type.TypeVarId,
    ) Allocator.Error!void {
        try stack.append(self.allocator, .{
            .process = .{
                // Keep the owned backing as the representative when the
                // structural shapes are merged. The isolated clone may later be
                // related to a nominal root, but a backing must never resolve to
                // the nominal type that owns it.
                .lhs = backing_ty,
                .rhs = structural_ty,
                .structural_isolated = true,
            },
        });
    }

    fn processUnifyPair(
        self: *Solver,
        stack: *std.ArrayList(UnifyFrame),
        lhs: Type.TypeVarId,
        rhs: Type.TypeVarId,
        structural_isolated: bool,
    ) Allocator.Error!void {
        var a = self.program.types.rootCompressed(lhs);
        var b = self.program.types.rootCompressed(rhs);
        if (a == b) return;

        const raw_left = self.program.types.get(a);
        const raw_right = self.program.types.get(b);
        if (raw_left == .mono and raw_right == .mono and raw_left.mono.id == raw_right.mono.id) {
            self.program.types.set(b, .{ .link = a });
            return;
        }
        const left = if (std.meta.activeTag(raw_left) == .mono) try self.expandMonoRoot(a, raw_left.mono) else raw_left;
        const right = if (std.meta.activeTag(raw_right) == .mono) try self.expandMonoRoot(b, raw_right.mono) else raw_right;
        // Expanding a leaf already materialized in its context links the
        // root to that clone, so the pair may have become one class.
        a = self.program.types.rootCompressed(a);
        b = self.program.types.rootCompressed(b);
        if (a == b) return;

        const left_tag = std.meta.activeTag(left);
        if (left_tag == .link) Common.invariant("Lambda Solved root returned a link");
        if (left_tag == .unbound) {
            self.program.types.set(a, .{ .link = b });
            return;
        }
        if (left_tag == .forall) Common.invariant("generalized Lambda Solved type reached local unification without instantiation");

        const right_tag = std.meta.activeTag(right);
        if (right_tag == .link) Common.invariant("Lambda Solved root returned a link");
        if (right_tag == .unbound) {
            self.program.types.set(b, .{ .link = a });
            return;
        }
        if (right_tag == .forall) Common.invariant("generalized Lambda Solved type reached local unification without instantiation");

        const pair = UnifyPair.init(a, b);
        const active_entry = try self.active_unifications.getOrPut(pair);
        if (active_entry.found_existing) return;
        errdefer _ = self.active_unifications.remove(pair);

        // Reserve the finish frame before pushing any children so it pops last
        // and retires `pair` once every type it scheduled has been unified.
        const finish_index = stack.items.len;
        try stack.append(self.allocator, .{ .finish = .{ .pair = pair, .action = .none, .lifts_before = self.lift_count } });
        try self.unifyRoots(stack, finish_index, a, b, left, right, structural_isolated);
    }

    fn unifyRoots(
        self: *Solver,
        stack: *std.ArrayList(UnifyFrame),
        finish_index: usize,
        a: Type.TypeVarId,
        b: Type.TypeVarId,
        left: Type.Content,
        right: Type.Content,
        structural_isolated: bool,
    ) Allocator.Error!void {
        if (transparentAliasBacking(left)) |backing| {
            stack.items[finish_index].finish.action = .{ .link_var_to_root = .{ .var_ = a, .target = backing } };
            try self.pushUnifyPair(stack, backing, b);
            return;
        }
        if (transparentAliasBacking(right)) |backing| {
            stack.items[finish_index].finish.action = .{ .link_var_to_root = .{ .var_ = b, .target = backing } };
            try self.pushUnifyPair(stack, a, backing);
            return;
        }
        if (try self.typeIsProvenUninhabited(a)) {
            self.program.types.set(a, .{ .link = b });
            return;
        }
        if (try self.typeIsProvenUninhabited(b)) {
            self.program.types.set(b, .{ .link = a });
            return;
        }
        if (try self.unifyInspectableNamedBacking(stack, finish_index, a, b, left, right, structural_isolated)) return;
        if (try self.unifyInspectableNamedBacking(stack, finish_index, b, a, right, left, structural_isolated)) return;
        if (try self.unifyPublicNamedBacking(a, b, right)) return;
        if (try self.unifyPublicNamedBacking(b, a, left)) return;

        switch (left) {
            .mono => Common.invariant("lazy Monotype leaf reached unification unexpanded"),
            .primitive => |left_primitive| {
                if (right != .primitive) Common.invariant("primitive type failed Lambda Solved unification");
                if (left_primitive != right.primitive) {
                    Common.invariant("primitive types failed Lambda Solved unification");
                }
                self.program.types.set(b, .{ .link = a });
            },
            .zst => {
                if (right != .zst) Common.invariant("zero-sized type failed Lambda Solved unification");
                self.program.types.set(b, .{ .link = a });
            },
            .erased => |left_erased| {
                if (right == .erased) {
                    const right_erased = right.erased;
                    if (!std.mem.eql(u8, left_erased.source_fn_ty.bytes[0..], right_erased.source_fn_ty.bytes[0..])) {
                        Common.invariant("erased callable source function types failed Lambda Solved unification");
                    }
                    var capture_pairs = std.ArrayList(DeferredSpanPair).empty;
                    defer capture_pairs.deinit(self.allocator);
                    const merged = try self.mergeLambdaSets(left_erased.members, right_erased.members, &capture_pairs);
                    stack.items[finish_index].finish.action = .{ .set_left_erased_link_right = .{
                        .lhs = a,
                        .rhs = b,
                        .source_fn_ty = left_erased.source_fn_ty,
                        .members = merged,
                        .abi_fn = left_erased.abi_fn orelse right_erased.abi_fn,
                    } };
                    try self.pushCaptureSpanPairs(stack, capture_pairs.items);
                } else if (right == .lambda_set) {
                    const right_members = right.lambda_set;
                    var capture_pairs = std.ArrayList(DeferredSpanPair).empty;
                    defer capture_pairs.deinit(self.allocator);
                    const merged = try self.mergeLambdaSets(left_erased.members, right_members, &capture_pairs);
                    stack.items[finish_index].finish.action = .{ .set_left_erased_link_right = .{
                        .lhs = a,
                        .rhs = b,
                        .source_fn_ty = left_erased.source_fn_ty,
                        .members = merged,
                        .abi_fn = left_erased.abi_fn,
                    } };
                    try self.pushCaptureSpanPairs(stack, capture_pairs.items);
                } else {
                    Common.invariant("erased callable type failed Lambda Solved unification");
                }
            },
            .lambda_set => |left_members| {
                if (right == .erased) {
                    const right_erased = right.erased;
                    var capture_pairs = std.ArrayList(DeferredSpanPair).empty;
                    defer capture_pairs.deinit(self.allocator);
                    const merged = try self.mergeLambdaSets(left_members, right_erased.members, &capture_pairs);
                    stack.items[finish_index].finish.action = .{ .set_left_erased_link_right = .{
                        .lhs = a,
                        .rhs = b,
                        .source_fn_ty = right_erased.source_fn_ty,
                        .members = merged,
                        .abi_fn = right_erased.abi_fn,
                    } };
                    try self.pushCaptureSpanPairs(stack, capture_pairs.items);
                } else if (right == .lambda_set) {
                    const right_members = right.lambda_set;
                    var capture_pairs = std.ArrayList(DeferredSpanPair).empty;
                    defer capture_pairs.deinit(self.allocator);
                    const merged = try self.mergeLambdaSets(left_members, right_members, &capture_pairs);
                    stack.items[finish_index].finish.action = .{ .set_left_lambda_set_link_right = .{
                        .lhs = a,
                        .rhs = b,
                        .members = merged,
                    } };
                    try self.pushCaptureSpanPairs(stack, capture_pairs.items);
                } else {
                    Common.invariant("lambda set failed Lambda Solved unification");
                }
            },
            .func => |left_fn| {
                if (right != .func) Common.invariant("function type failed Lambda Solved unification");
                const right_fn = right.func;
                stack.items[finish_index].finish.action = .{ .link_rhs_to_lhs = .{ .lhs = a, .rhs = b } };
                try self.pushUnifyPair(stack, left_fn.ret, right_fn.ret);
                try self.pushUnifyPair(stack, left_fn.callable, right_fn.callable);
                try self.pushSpanPairs(stack, left_fn.args, right_fn.args, "function argument lists failed Lambda Solved unification");
            },
            .list => |left_elem| {
                if (right != .list) Common.invariant("list type failed Lambda Solved unification");
                stack.items[finish_index].finish.action = .{ .link_rhs_to_lhs = .{ .lhs = a, .rhs = b } };
                try self.pushUnifyPair(stack, left_elem, right.list);
            },
            .box => |left_elem| {
                if (right != .box) Common.invariant("box type failed Lambda Solved unification");
                stack.items[finish_index].finish.action = .{ .link_rhs_to_lhs = .{ .lhs = a, .rhs = b } };
                try self.pushUnifyPair(stack, left_elem, right.box);
            },
            .tuple => |left_items| {
                if (right != .tuple) Common.invariant("tuple type failed Lambda Solved unification");
                stack.items[finish_index].finish.action = .{ .link_rhs_to_lhs = .{ .lhs = a, .rhs = b } };
                try self.pushSpanPairs(stack, left_items, right.tuple, "tuple item lists failed Lambda Solved unification");
            },
            .record => |left_fields| {
                if (right != .record) Common.invariant("record type failed Lambda Solved unification");
                stack.items[finish_index].finish.action = .{ .link_rhs_to_lhs = .{ .lhs = a, .rhs = b } };
                try self.pushFieldPairs(stack, left_fields, right.record);
            },
            .tag_union => |left_tags| {
                if (right != .tag_union) Common.invariant("tag-union type failed Lambda Solved unification");
                const right_tags = right.tag_union;
                if (left_tags.count() == 0) {
                    self.program.types.set(a, .{ .link = b });
                    return;
                }
                if (right_tags.count() == 0) {
                    self.program.types.set(b, .{ .link = a });
                    return;
                }
                var payload_pairs = std.ArrayList(DeferredSpanPair).empty;
                defer payload_pairs.deinit(self.allocator);
                const merged = try self.mergeTags(left_tags, right_tags, &payload_pairs);
                stack.items[finish_index].finish.action = .{ .set_left_tag_union_link_right = .{
                    .lhs = a,
                    .rhs = b,
                    .tags = merged,
                } };
                try self.pushPayloadSpanPairs(stack, payload_pairs.items);
            },
            .named => |left_named| {
                if (right != .named) Common.invariant("named type failed Lambda Solved unification");
                const right_named = right.named;
                if (!std.meta.eql(left_named.def, right_named.def) or
                    left_named.kind != right_named.kind or
                    left_named.builtin_owner != right_named.builtin_owner)
                {
                    if (try self.unifyForcedDynamicIterator(stack, finish_index, a, b, left_named, right_named)) return;
                    if (try self.unifyIteratorOwnerStampedPublic(stack, finish_index, a, b, left_named, right_named)) return;
                    if (try self.unifyGeneratedIteratorJoin(stack, finish_index, a, b, left_named, right_named)) return;
                    if (try self.unifyPublicGeneratedIterator(stack, finish_index, a, b, left_named, right_named)) return;
                    if (try self.unifyNominalOpaqueViews(stack, finish_index, a, b, left_named, right_named)) return;
                    Common.invariant("named type identity failed Lambda Solved unification");
                }
                if (left_named.backing) |left_backing| {
                    const right_backing = right_named.backing orelse Common.invariant("named type backing differed during Lambda Solved unification");
                    if (left_backing.use != right_backing.use) Common.invariant("named type backing use differed during Lambda Solved unification");
                    if (left_backing.authority == right_backing.authority) {
                        stack.items[finish_index].finish.action = .{ .link_rhs_to_lhs = .{ .lhs = a, .rhs = b } };
                        try self.pushUnifyPair(stack, left_backing.ty, right_backing.ty);
                    } else if (left_backing.authority == .generated_private) {
                        stack.items[finish_index].finish.action = .{ .link_rhs_to_lhs = .{ .lhs = a, .rhs = b } };
                        try self.pushRelate(stack, right_backing.ty, left_backing.ty);
                    } else if (right_backing.authority == .generated_private) {
                        stack.items[finish_index].finish.action = .{ .link_rhs_to_lhs = .{ .lhs = b, .rhs = a } };
                        try self.pushRelate(stack, left_backing.ty, right_backing.ty);
                    } else {
                        Common.invariant("named type backing authorities were incompatible during Lambda Solved unification");
                    }
                } else if (right_named.backing != null) {
                    Common.invariant("named type backing differed during Lambda Solved unification");
                } else {
                    stack.items[finish_index].finish.action = .{ .link_rhs_to_lhs = .{ .lhs = a, .rhs = b } };
                    try self.pushSpanPairs(stack, left_named.args, right_named.args, "named type arguments failed Lambda Solved unification");
                }
            },
            .link, .unbound, .forall => unreachable,
        }
    }

    /// Whether a finish action joins the two unified types into one type,
    /// as opposed to settling a callable slot or an alias's own backing.
    fn joinsTypeRoots(action: UnifyFinishAction) bool {
        return switch (action) {
            .link_rhs_to_lhs, .link_structural_to_inspectable_named, .set_left_tag_union_link_right => true,
            .none, .link_var_to_root, .set_left_erased_link_right, .set_left_lambda_set_link_right => false,
        };
    }

    fn applyUnifyFinish(self: *Solver, action: UnifyFinishAction) void {
        switch (action) {
            .none => {},
            .link_rhs_to_lhs => |link| self.program.types.set(link.rhs, .{ .link = link.lhs }),
            .link_var_to_root => |link| self.program.types.set(link.var_, .{ .link = self.program.types.rootCompressed(link.target) }),
            .link_structural_to_inspectable_named => |link| {
                const structural_root = self.program.types.rootCompressed(link.structural);
                const named_root = self.program.types.rootCompressed(link.named);
                if (structural_root == named_root or self.program.types.isOwnedNamedBacking(structural_root)) return;
                self.program.types.set(structural_root, .{ .link = named_root });
            },
            .set_left_erased_link_right => |set| {
                self.program.types.set(set.lhs, .{ .erased = .{
                    .source_fn_ty = set.source_fn_ty,
                    .members = set.members,
                    .abi_fn = set.abi_fn,
                } });
                self.program.types.set(set.rhs, .{ .link = set.lhs });
            },
            .set_left_lambda_set_link_right => |set| {
                self.program.types.set(set.lhs, .{ .lambda_set = set.members });
                self.program.types.set(set.rhs, .{ .link = set.lhs });
            },
            .set_left_tag_union_link_right => |set| {
                self.program.types.set(set.lhs, .{ .tag_union = set.tags });
                self.program.types.set(set.rhs, .{ .link = set.lhs });
            },
        }
    }

    /// Relate the definition-private nominal and opaque interface views of one
    /// checked definition without widening the opaque side's inspectability.
    /// Checking and Monotype have already established the exact TypeDef
    /// identity; Lambda Solved consumes that relation solely to propagate
    /// callable flow through the shared runtime representation.
    fn unifyNominalOpaqueViews(
        self: *Solver,
        stack: *std.ArrayList(UnifyFrame),
        finish_index: usize,
        left_ty: Type.TypeVarId,
        right_ty: Type.TypeVarId,
        left: anytype,
        right: anytype,
    ) Allocator.Error!bool {
        if (!sameMonoTypeDef(left.def, right.def) or
            left.builtin_owner != right.builtin_owner)
        {
            return false;
        }
        const left_is_nominal = left.kind == .nominal;
        const right_is_nominal = right.kind == .nominal;
        const left_is_opaque = left.kind == .@"opaque";
        const right_is_opaque = right.kind == .@"opaque";
        if (!((left_is_nominal and right_is_opaque) or
            (left_is_opaque and right_is_nominal)))
        {
            return false;
        }

        const left_backing = left.backing orelse
            Common.invariant("nominal/opaque visibility relation lacked a checked runtime backing");
        const right_backing = right.backing orelse
            Common.invariant("nominal/opaque visibility relation lacked a checked runtime backing");
        if (left_backing.authority != .checked_public or
            right_backing.authority != .checked_public)
        {
            Common.invariant("nominal/opaque visibility relation lacked checked-public backing authority");
        }
        if (left_is_nominal and left_backing.use != .inspectable) {
            Common.invariant("definition-private nominal view lacked inspectable backing authority");
        }
        if (right_is_nominal and right_backing.use != .inspectable) {
            Common.invariant("definition-private nominal view lacked inspectable backing authority");
        }
        if (left_is_opaque and left_backing.use != .runtime_layout_only) {
            Common.invariant("opaque interface view carried inspectable backing authority");
        }
        if (right_is_opaque and right_backing.use != .runtime_layout_only) {
            Common.invariant("opaque interface view carried inspectable backing authority");
        }

        linkAtFinish(stack, finish_index, if (left_is_opaque) left_ty else right_ty, if (left_is_opaque) right_ty else left_ty);
        try self.pushUnifyPair(stack, left_backing.ty, right_backing.ty);
        try self.pushSpanPairs(
            stack,
            left.args,
            right.args,
            "nominal/opaque type arguments failed Lambda Solved unification",
        );
        return true;
    }

    fn unifyPublicNamedBacking(
        self: *Solver,
        backing_ty: Type.TypeVarId,
        named_ty: Type.TypeVarId,
        named_content: Type.Content,
    ) Allocator.Error!bool {
        if (std.meta.activeTag(named_content) != .named) return false;
        const named = named_content.named;
        switch (named.kind) {
            .nominal, .@"opaque" => {},
            .alias => return false,
        }
        const backing = named.backing orelse return false;
        if (backing.authority != .checked_public or backing.use != .inspectable) return false;
        if (!try self.typeIsProvenUninhabited(backing_ty)) return false;
        const backing_root = self.program.types.rootCompressed(backing_ty);
        const named_root = self.program.types.rootCompressed(named_ty);
        if (backing_root != named_root) {
            self.program.types.set(backing_root, .{ .link = named_root });
        }
        return true;
    }

    fn unifyInspectableNamedBacking(
        self: *Solver,
        stack: *std.ArrayList(UnifyFrame),
        finish_index: usize,
        structural_ty: Type.TypeVarId,
        named_ty: Type.TypeVarId,
        structural_content: Type.Content,
        named_content: Type.Content,
        structural_isolated: bool,
    ) Allocator.Error!bool {
        const structural_tag = std.meta.activeTag(structural_content);
        if (structural_tag == .named or structural_tag == .link or structural_tag == .unbound or structural_tag == .forall) return false;
        if (std.meta.activeTag(named_content) != .named) return false;
        const named = named_content.named;
        if (named.kind == .alias) return false;
        const backing = named.backing orelse return false;
        if (backing.use != .inspectable) return false;

        const working_structural = if (structural_isolated)
            structural_ty
        else
            try self.program.types.add(structural_content);
        if (!structural_isolated) {
            self.lift_count += 1;
            stack.items[finish_index].finish.action = .{ .link_structural_to_inspectable_named = .{
                .structural = structural_ty,
                .named = named_ty,
            } };
        }
        try self.pushIsolatedStructuralPair(stack, working_structural, backing.ty);
        return true;
    }

    fn typeIsProvenUninhabited(self: *Solver, ty: Type.TypeVarId) Allocator.Error!bool {
        // A remembered answer needs no scan.
        if (self.uninhabited_memo.get(self.program.types.rootCompressed(ty))) |answer| {
            if (answer.epoch == self.program.types.mutation_epoch) return answer.uninhabited;
        }
        const var_count = self.program.types.vars.items.len;
        if (self.uninhabited_path.bit_length < var_count) {
            try self.uninhabited_path.resize(self.allocator, var_count, false);
        }
        var scan = SolvedUninhabitedScan{ .solver = self, .entry_marks = self.solved_entry_marks };
        self.solved_entry_marks = .empty;
        defer {
            // A scan that ran inside this one has already returned its
            // list; this one's is then released.
            scan.entry_marks.clearRetainingCapacity();
            if (self.solved_entry_marks.capacity == 0) {
                self.solved_entry_marks = scan.entry_marks;
            } else scan.entry_marks.deinit(self.allocator);
        }
        return try SolvedUninhabitedScan.Eval.runWith(self.allocator, &self.solved_uninhabited_scratch, &scan, ty);
    }

    /// `typeIsProvenUninhabited` over the lifted Monotype store, for lazy
    /// leaves that have not materialized.
    fn monoProvenUninhabited(self: *Solver, id: MonoType.TypeId) Allocator.Error!bool {
        if (self.mono_uninhabited.get(id)) |result| return result;
        const type_count = self.lifted.types.types.len;
        if (self.mono_uninhabited_path.bit_length < type_count) {
            try self.mono_uninhabited_path.resize(self.allocator, type_count, false);
        }
        var scan = MonoUninhabitedScan{ .solver = self, .entry_stops = self.mono_entry_stops };
        self.mono_entry_stops = .empty;
        defer {
            // A scan that ran inside this one has already returned its
            // list; this one's is then released.
            scan.entry_stops.clearRetainingCapacity();
            if (self.mono_entry_stops.capacity == 0) {
                self.mono_entry_stops = scan.entry_stops;
            } else scan.entry_stops.deinit(self.allocator);
        }
        return try MonoUninhabitedScan.Eval.runWith(self.allocator, &self.mono_uninhabited_scratch, &scan, id);
    }

    /// Transfer Lambda Solved callable evidence from a checked-public value
    /// shape into its producer-authored generated-private representation.
    /// Monotype has already sealed both representations, so this relation
    /// deliberately preserves every composite and named root. Only callable
    /// slots (and still-open Lambda Solved slots) are unified. A checked-public
    /// inspectable named type may correspond to its structural backing in the
    /// private witness; walk through that backing without linking either root.
    fn pushRelate(
        self: *Solver,
        stack: *std.ArrayList(UnifyFrame),
        public_ty: Type.TypeVarId,
        private_ty: Type.TypeVarId,
    ) Allocator.Error!void {
        try stack.append(self.allocator, .{ .relate = .{ .public = public_ty, .private = private_ty } });
    }

    /// Relates one public/private pair, pushing the pairs it relates next.
    /// The pair stays active until its `relate_exit` frame pops, after
    /// everything it pushed. Components are pushed in order and then
    /// reversed so they are related in order.
    fn processRelate(
        self: *Solver,
        stack: *std.ArrayList(UnifyFrame),
        public_ty: Type.TypeVarId,
        private_ty: Type.TypeVarId,
    ) Allocator.Error!void {
        const public_root = self.program.types.rootCompressed(public_ty);
        const private_root = self.program.types.rootCompressed(private_ty);
        if (public_root == private_root) return;

        const pair = UnifyPair.init(public_root, private_root);
        const active = try self.active_private_evidence_relations.getOrPut(pair);
        if (active.found_existing) return;
        stack.append(self.allocator, .{ .relate_exit = pair }) catch |err| {
            _ = self.active_private_evidence_relations.remove(pair);
            return err;
        };
        const mark = stack.items.len;
        defer std.mem.reverse(UnifyFrame, stack.items[mark..]);

        const public = try self.resolvedContentAt(public_root);
        const private = try self.resolvedContentAt(private_root);
        const private_content_tag = std.meta.activeTag(private);
        if (public == .unbound or private == .unbound or
            public == .lambda_set or private == .lambda_set or
            public == .erased or private == .erased)
        {
            try self.pushUnifyPair(stack, public_root, private_root);
            return;
        }

        switch (public) {
            .link, .unbound, .lambda_set, .erased => unreachable,
            .mono => Common.invariant("lazy Monotype leaf reached the generated-private evidence relation unexpanded"),
            .forall => Common.invariant("generated-private evidence relation received a generalized public type"),
            .primitive => |public_primitive| {
                if (private_content_tag != .primitive) Common.invariant("generated-private evidence relation received different type structure");
                if (public_primitive != private.primitive) Common.invariant("generated-private evidence relation received different primitive types");
            },
            .zst => if (private != .zst) Common.invariant("generated-private evidence relation received different type structure"),
            .list => |public_elem| {
                if (private_content_tag != .list) Common.invariant("generated-private evidence relation received different type structure");
                try self.pushRelate(stack, public_elem, private.list);
            },
            .box => |public_elem| {
                if (private_content_tag != .box) Common.invariant("generated-private evidence relation received different type structure");
                try self.pushRelate(stack, public_elem, private.box);
            },
            .tuple => |public_items| {
                if (private_content_tag != .tuple) Common.invariant("generated-private evidence relation received different type structure");
                const private_items = private.tuple;
                if (public_items.count() != private_items.count()) {
                    Common.invariant("generated-private evidence relation received tuples of different arity");
                }
                for (0..public_items.count()) |index| {
                    try self.pushRelate(
                        stack,
                        self.program.types.spanItem(public_items, index),
                        self.program.types.spanItem(private_items, index),
                    );
                }
            },
            .record => |public_fields| {
                if (private_content_tag != .record) Common.invariant("generated-private evidence relation received different type structure");
                const private_fields = private.record;
                if (public_fields.count() != private_fields.count()) {
                    Common.invariant("generated-private evidence relation received records with different fields");
                }
                for (0..public_fields.count()) |index| {
                    const public_field = self.program.types.fieldItem(public_fields, index);
                    const private_field = self.program.types.fieldItem(private_fields, index);
                    if (public_field.name != private_field.name) {
                        Common.invariant("generated-private evidence relation received records with different fields");
                    }
                    try self.pushRelate(stack, public_field.ty, private_field.ty);
                    if ((public_field.value_ty == null) != (private_field.value_ty == null)) {
                        Common.invariant("generated-private evidence relation received different record field kinds");
                    }
                    if (public_field.value_ty) |public_value_ty| {
                        try self.pushRelate(stack, public_value_ty, private_field.value_ty.?);
                    }
                }
            },
            .tag_union => |public_tags| {
                if (private_content_tag != .tag_union) Common.invariant("generated-private evidence relation received different type structure");
                const private_tags = private.tag_union;
                if (public_tags.count() != private_tags.count()) {
                    Common.invariant("generated-private evidence relation received tag unions with different tags");
                }
                for (0..public_tags.count()) |tag_index| {
                    const public_tag = self.program.types.tagItem(public_tags, tag_index);
                    const private_tag = self.program.types.tagItem(private_tags, tag_index);
                    if (public_tag.name != private_tag.name or public_tag.checked_name != private_tag.checked_name or
                        public_tag.payloads.count() != private_tag.payloads.count())
                    {
                        Common.invariant("generated-private evidence relation received tag unions with different tags");
                    }
                    for (0..public_tag.payloads.count()) |payload_index| {
                        try self.pushRelate(
                            stack,
                            self.program.types.spanItem(public_tag.payloads, payload_index),
                            self.program.types.spanItem(private_tag.payloads, payload_index),
                        );
                    }
                }
            },
            .func => |public_fn| {
                if (private_content_tag != .func) Common.invariant("generated-private evidence relation received different type structure");
                const private_fn = private.func;
                if (public_fn.args.count() != private_fn.args.count()) {
                    Common.invariant("generated-private evidence relation received functions of different arity");
                }
                for (0..public_fn.args.count()) |index| {
                    try self.pushRelate(
                        stack,
                        self.program.types.spanItem(public_fn.args, index),
                        self.program.types.spanItem(private_fn.args, index),
                    );
                }
                try self.pushUnifyPair(stack, public_fn.callable, private_fn.callable);
                try self.pushRelate(stack, public_fn.ret, private_fn.ret);
            },
            .named => |public_named| {
                if (private_content_tag != .named) {
                    const public_backing = public_named.backing orelse
                        Common.invariant("generated-private evidence relation could not traverse a public named type without backing");
                    if (public_backing.authority != .checked_public or public_backing.use != .inspectable) {
                        Common.invariant("generated-private evidence relation could not traverse an opaque public named type");
                    }
                    try self.pushRelate(stack, public_backing.ty, private_root);
                    return;
                }
                const private_named = private.named;
                const same_identity = public_named.kind == private_named.kind and
                    std.meta.eql(public_named.def, private_named.def) and
                    public_named.builtin_owner == private_named.builtin_owner;
                if (!same_identity and MonoType.iteratorRelation(public_named, private_named) == .ordinary) {
                    Common.invariant("generated-private evidence relation received different named types");
                }
                if (same_identity) {
                    if (public_named.args.count() != private_named.args.count()) {
                        Common.invariant("generated-private evidence relation received named types with different arity");
                    }
                    for (0..public_named.args.count()) |index| {
                        try self.pushRelate(
                            stack,
                            self.program.types.spanItem(public_named.args, index),
                            self.program.types.spanItem(private_named.args, index),
                        );
                    }
                    if (public_named.backing) |public_backing| {
                        const private_backing = private_named.backing orelse
                            Common.invariant("generated-private evidence relation received different named backing presence");
                        if (public_backing.use != private_backing.use) {
                            Common.invariant("generated-private evidence relation received different named backing uses");
                        }
                        try self.pushRelate(stack, public_backing.ty, private_backing.ty);
                    } else if (private_named.backing != null) {
                        Common.invariant("generated-private evidence relation received different named backing presence");
                    }
                } else {
                    if (public_named.args.count() == 0 or private_named.args.count() == 0) {
                        Common.invariant("generated-private iterator evidence lacked a public item argument");
                    }
                    try self.pushRelate(
                        stack,
                        self.program.types.spanItem(public_named.args, 0),
                        self.program.types.spanItem(private_named.args, 0),
                    );
                    // A public iterator viewing a generated representation
                    // receives its callable evidence through the backings,
                    // exactly as the unifying relation transfers it.
                    if (public_named.backing) |public_backing| if (private_named.backing) |private_backing| {
                        if (public_backing.authority == .checked_public and private_backing.authority == .generated_private) {
                            if (public_backing.use != private_backing.use) {
                                Common.invariant("generated-private iterator evidence relation received different backing uses");
                            }
                            try self.pushRelate(stack, public_backing.ty, private_backing.ty);
                        }
                    };
                }
            },
        }
    }

    fn unifyIteratorOwnerStampedPublic(
        self: *Solver,
        stack: *std.ArrayList(UnifyFrame),
        finish_index: usize,
        left_ty: Type.TypeVarId,
        right_ty: Type.TypeVarId,
        left: anytype,
        right: anytype,
    ) Allocator.Error!bool {
        if (left.kind != right.kind) return false;
        if (!sameMonoTypeDef(left.def, right.def)) return false;
        _ = iteratorLikeOwnerFromPair(left.builtin_owner, right.builtin_owner) orelse return false;
        if (left.builtin_owner == right.builtin_owner) return false;

        const left_owns = isIteratorLikeOwner(left.builtin_owner);
        linkAtFinish(stack, finish_index, if (left_owns) left_ty else right_ty, if (left_owns) right_ty else left_ty);
        try self.pushSpanPairs(stack, left.args, right.args, "iterator owner-stamp argument lists failed Lambda Solved unification");
        return true;
    }

    fn unifyForcedDynamicIterator(
        self: *Solver,
        stack: *std.ArrayList(UnifyFrame),
        finish_index: usize,
        left_ty: Type.TypeVarId,
        right_ty: Type.TypeVarId,
        left: anytype,
        right: anytype,
    ) Allocator.Error!bool {
        if (MonoType.iteratorRelation(left, right) != .forced_dynamic) return false;

        const left_dynamic = left.def.iterator_representation == .forced_dynamic;
        if (left.args.count() == 0 or right.args.count() == 0) {
            Common.invariant("forced-dynamic iterator reached Lambda Solved without a public item argument");
        }

        linkAtFinish(stack, finish_index, if (left_dynamic) left_ty else right_ty, if (left_dynamic) right_ty else left_ty);
        const dynamic = if (left_dynamic) left else right;
        const other = if (left_dynamic) right else left;
        switch (other.def.iterator_representation) {
            .none => try self.relateForcedDynamicPublicEvidence(stack, dynamic, other),
            .minted => try self.unifyGeneratedIteratorBackings(stack, left, right),
            .forced_dynamic => Common.invariant("forced-dynamic iterator relation received two dynamic representations"),
        }
        try self.pushUnifyPair(stack, self.program.types.spanItem(left.args, 0), self.program.types.spanItem(right.args, 0));
        return true;
    }

    fn relateForcedDynamicPublicEvidence(self: *Solver, stack: *std.ArrayList(UnifyFrame), dynamic: anytype, public: anytype) Allocator.Error!void {
        const public_backing = public.backing orelse return;
        const dynamic_backing = dynamic.backing orelse
            Common.invariant("forced-dynamic iterator relation found dynamic backing on only one side");
        if (public_backing.use != dynamic_backing.use) {
            Common.invariant("forced-dynamic iterator relation found different backing uses");
        }
        if (public_backing.authority != .checked_public or dynamic_backing.authority != .generated_private) {
            Common.invariant("forced-dynamic iterator evidence relation received incorrect backing authority");
        }
        try self.pushRelate(stack, public_backing.ty, dynamic_backing.ty);
    }

    fn unifyGeneratedIteratorJoin(
        self: *Solver,
        stack: *std.ArrayList(UnifyFrame),
        finish_index: usize,
        left_ty: Type.TypeVarId,
        right_ty: Type.TypeVarId,
        left: anytype,
        right: anytype,
    ) Allocator.Error!bool {
        if (MonoType.iteratorRelation(left, right) != .minted_join) return false;

        if (left.args.count() == 0 or right.args.count() == 0) {
            Common.invariant("generated iterator join reached Lambda Solved without a public item argument");
        }
        const left_owns = isIteratorLikeOwner(left.builtin_owner);
        linkAtFinish(stack, finish_index, if (left_owns) left_ty else right_ty, if (left_owns) right_ty else left_ty);

        if (left.backing) |left_backing| {
            const right_backing = right.backing orelse
                Common.invariant("generated iterator join found backing on only one side");
            if (left_backing.use != right_backing.use) {
                Common.invariant("generated iterator join found different backing uses");
            }
            if (left_backing.authority != right_backing.authority) {
                Common.invariant("generated iterator join found different backing authorities");
            }
            try self.pushUnifyPair(stack, left_backing.ty, right_backing.ty);
        } else if (right.backing != null) {
            Common.invariant("generated iterator join found backing on only one side");
        }
        try self.pushUnifyPair(stack, self.program.types.spanItem(left.args, 0), self.program.types.spanItem(right.args, 0));
        return true;
    }

    fn unifyPublicGeneratedIterator(
        self: *Solver,
        stack: *std.ArrayList(UnifyFrame),
        finish_index: usize,
        left_ty: Type.TypeVarId,
        right_ty: Type.TypeVarId,
        left: anytype,
        right: anytype,
    ) Allocator.Error!bool {
        if (MonoType.iteratorRelation(left, right) != .public_minted) return false;

        if (left.args.count() == 0 or right.args.count() == 0) {
            Common.invariant("generated iterator evidence reached Lambda Solved without a public item argument");
        }

        const left_minted = left.def.iterator_representation == .minted;
        linkAtFinish(stack, finish_index, if (left_minted) left_ty else right_ty, if (left_minted) right_ty else left_ty);
        const generated = if (left_minted) left else right;
        const public = if (left_minted) right else left;
        if (public.backing) |public_backing| {
            const generated_backing = generated.backing orelse
                Common.invariant("generated iterator evidence had no private backing");
            if (generated_backing.authority != .generated_private) {
                Common.invariant("generated iterator evidence backing lacked private authority");
            }
            try self.pushRelate(stack, public_backing.ty, generated_backing.ty);
        }
        try self.pushUnifyPair(stack, self.program.types.spanItem(left.args, 0), self.program.types.spanItem(right.args, 0));
        return true;
    }

    fn unifyGeneratedIteratorBackings(self: *Solver, stack: *std.ArrayList(UnifyFrame), left: anytype, right: anytype) Allocator.Error!void {
        const left_backing = left.backing orelse
            Common.invariant("generated iterator relation found backing on only one side");
        const right_backing = right.backing orelse
            Common.invariant("generated iterator relation found backing on only one side");
        if (left_backing.use != right_backing.use) {
            Common.invariant("generated iterator relation found different backing uses");
        }
        if (left_backing.authority != .generated_private or right_backing.authority != .generated_private) {
            Common.invariant("private iterator relation received a checked-public backing");
        }
        try self.pushUnifyPair(stack, left_backing.ty, right_backing.ty);
    }

    fn transparentAliasBacking(content: Type.Content) ?Type.TypeVarId {
        if (std.meta.activeTag(content) != .named or content.named.kind != .alias) return null;
        return (content.named.backing orelse Common.invariant("transparent alias reached Lambda Solved without a backing type")).ty;
    }

    /// Links `rhs` to `lhs` when the pair's finish frame pops, after every
    /// relation the pair pushed has completed.
    fn linkAtFinish(stack: *std.ArrayList(UnifyFrame), finish_index: usize, lhs: Type.TypeVarId, rhs: Type.TypeVarId) void {
        stack.items[finish_index].finish.action = .{ .link_rhs_to_lhs = .{ .lhs = lhs, .rhs = rhs } };
    }

    /// Push one `process` frame per span element, in reverse so the stack
    /// pops them in span order.
    fn pushSpanPairs(
        self: *Solver,
        stack: *std.ArrayList(UnifyFrame),
        lhs: Type.Span,
        rhs: Type.Span,
        comptime message: []const u8,
    ) Allocator.Error!void {
        if (lhs.count() != rhs.count()) Common.invariant(message);
        var i = lhs.count();
        while (i > 0) {
            i -= 1;
            const left_ty = self.program.types.spanItem(lhs, i);
            const right_ty = self.program.types.spanItem(rhs, i);
            try self.pushUnifyPair(stack, left_ty, right_ty);
        }
    }

    fn pushFieldPairs(self: *Solver, stack: *std.ArrayList(UnifyFrame), lhs: Type.Span, rhs: Type.Span) Allocator.Error!void {
        if (lhs.count() != rhs.count()) Common.invariant("record field count failed Lambda Solved unification");
        var i = lhs.count();
        while (i > 0) {
            i -= 1;
            const left_field = self.program.types.fieldItem(lhs, i);
            const right_field = self.program.types.fieldItem(rhs, i);
            if (left_field.name != right_field.name) Common.invariant("record field order failed Lambda Solved unification");
            try self.pushUnifyPair(stack, left_field.ty, right_field.ty);
            if ((left_field.value_ty == null) != (right_field.value_ty == null)) {
                Common.invariant("record field kind failed Lambda Solved unification");
            }
            if (left_field.value_ty) |left_value_ty| {
                try self.pushUnifyPair(stack, left_value_ty, right_field.value_ty.?);
            }
        }
    }

    fn pushPayloadSpanPairs(self: *Solver, stack: *std.ArrayList(UnifyFrame), pairs: []const DeferredSpanPair) Allocator.Error!void {
        var i = pairs.len;
        while (i > 0) {
            i -= 1;
            try self.pushSpanPairs(stack, pairs[i].lhs, pairs[i].rhs, "tag payload count failed Lambda Solved unification");
        }
    }

    fn pushCaptureSpanPairs(self: *Solver, stack: *std.ArrayList(UnifyFrame), pairs: []const DeferredSpanPair) Allocator.Error!void {
        var i = pairs.len;
        while (i > 0) {
            i -= 1;
            try self.pushCapturePairs(stack, pairs[i].lhs, pairs[i].rhs);
        }
    }

    fn pushCapturePairs(self: *Solver, stack: *std.ArrayList(UnifyFrame), lhs: Type.Span, rhs: Type.Span) Allocator.Error!void {
        if (lhs.count() != rhs.count()) Common.invariant("capture count failed Lambda Solved unification");
        var i = lhs.count();
        while (i > 0) {
            i -= 1;
            const left_capture = self.program.types.captureItem(lhs, i);
            const right_capture = self.program.types.captureItem(rhs, i);
            if (left_capture.capture_id != right_capture.capture_id) {
                Common.invariant("capture identity failed Lambda Solved unification");
            }
            try self.pushUnifyPair(stack, left_capture.ty, right_capture.ty);
        }
    }

    /// Merge two tag unions, collecting the shared tags' payload spans for the
    /// caller to unify once the merged span has been recorded.
    /// Positions of a stored tag row's tags by name; the first position wins
    /// for a repeated name.
    const TagRowIndex = struct {
        len: u32,
        by_name: std.AutoHashMapUnmanaged(Type.names.TagNameId, usize),
    };

    fn tagRowIndex(self: *Solver, span: Type.Span) Allocator.Error!*const TagRowIndex {
        const gop = try self.tag_row_indexes.getOrPut(self.allocator, span.start);
        if (gop.found_existing and gop.value_ptr.len == span.len) return gop.value_ptr;
        if (!gop.found_existing) gop.value_ptr.* = .{ .len = span.len, .by_name = .empty };
        gop.value_ptr.len = span.len;
        gop.value_ptr.by_name.clearRetainingCapacity();
        try gop.value_ptr.by_name.ensureTotalCapacity(self.allocator, span.len);
        for (0..span.count()) |index| {
            const entry = gop.value_ptr.by_name.getOrPutAssumeCapacity(self.program.types.tagItem(span, index).name);
            if (!entry.found_existing) entry.value_ptr.* = index;
        }
        return gop.value_ptr;
    }

    const RecordRowIndex = struct {
        len: u32,
        by_name: std.AutoHashMapUnmanaged(Type.names.RecordFieldNameId, usize),
    };

    fn recordRowIndex(self: *Solver, span: Type.Span) Allocator.Error!*const RecordRowIndex {
        const gop = try self.record_row_indexes.getOrPut(self.allocator, span.start);
        if (gop.found_existing and gop.value_ptr.len == span.len) return gop.value_ptr;
        if (!gop.found_existing) gop.value_ptr.* = .{ .len = span.len, .by_name = .empty };
        gop.value_ptr.len = span.len;
        gop.value_ptr.by_name.clearRetainingCapacity();
        try gop.value_ptr.by_name.ensureTotalCapacity(self.allocator, span.len);
        for (0..span.count()) |index| {
            const entry = gop.value_ptr.by_name.getOrPutAssumeCapacity(self.program.types.fieldItem(span, index).name);
            if (!entry.found_existing) entry.value_ptr.* = index;
        }
        return gop.value_ptr;
    }

    fn mergeTags(
        self: *Solver,
        lhs: Type.Span,
        rhs: Type.Span,
        payload_pairs: *std.ArrayList(DeferredSpanPair),
    ) Allocator.Error!Type.Span {
        const right_index = try self.tagRowIndex(rhs);
        var shared_count: usize = 0;
        var every_left_in_right = true;
        for (0..lhs.count()) |left_index| {
            const left_tag = self.program.types.tagItem(lhs, left_index);
            if (right_index.by_name.get(left_tag.name)) |right_position| {
                try payload_pairs.append(self.allocator, .{
                    .lhs = left_tag.payloads,
                    .rhs = self.program.types.tagItem(rhs, right_position).payloads,
                });
                shared_count += 1;
            } else {
                every_left_in_right = false;
            }
        }

        if (shared_count == 0) Common.invariant("disjoint tag unions failed Lambda Solved unification");

        // The right row already holds every left tag: the merge is the right
        // row itself.
        if (every_left_in_right) return rhs;

        var merged = std.ArrayList(Type.Tag).empty;
        defer merged.deinit(self.allocator);
        var left_names = std.AutoHashMapUnmanaged(Type.names.TagNameId, void).empty;
        defer left_names.deinit(self.allocator);
        try left_names.ensureTotalCapacity(self.allocator, @intCast(lhs.count()));
        for (0..lhs.count()) |left_index| {
            const left_tag = self.program.types.tagItem(lhs, left_index);
            try merged.append(self.allocator, left_tag);
            left_names.putAssumeCapacity(left_tag.name, {});
        }
        for (0..rhs.count()) |right_position| {
            const right_tag = self.program.types.tagItem(rhs, right_position);
            if (left_names.contains(right_tag.name)) continue;
            try merged.append(self.allocator, right_tag);
        }
        // Every right tag already sits in the left row: the merge is the
        // left row itself.
        if (merged.items.len == lhs.count()) return lhs;
        return try self.program.types.addTags(merged.items);
    }

    /// Merge two lambda sets, collecting the shared members' capture spans for
    /// the caller to unify once the merged span has been recorded.
    fn mergeLambdaSets(
        self: *Solver,
        lhs: Type.Span,
        rhs: Type.Span,
        capture_pairs: *std.ArrayList(DeferredSpanPair),
    ) Allocator.Error!Type.Span {
        // The merge keeps one set's members in order and appends the other's
        // missing members. The kept set is one that can grow in place at the
        // end of the member pool, so growing a long set by a few members never
        // copies it.
        const keep_rhs = !self.lambdaSetGrowsInPlace(lhs) and self.lambdaSetGrowsInPlace(rhs);
        const kept = if (keep_rhs) rhs else lhs;
        const other = if (keep_rhs) lhs else rhs;
        const kept_index = try self.lambdaSetIndex(kept);
        var added = std.ArrayList(Type.FnMember).empty;
        defer added.deinit(self.allocator);
        var added_positions = std.AutoHashMapUnmanaged(Common.Symbol, usize).empty;
        defer added_positions.deinit(self.allocator);

        for (0..other.count()) |i| {
            const other_member = self.program.types.memberItem(other, i);
            const kept_captures = if (kept_index.position(other_member.lambda, kept)) |position|
                self.program.types.memberItem(kept, position).captures
            else if (added_positions.get(other_member.lambda)) |position|
                added.items[position].captures
            else {
                try added_positions.put(self.allocator, other_member.lambda, added.items.len);
                try added.append(self.allocator, other_member);
                continue;
            };
            try capture_pairs.append(self.allocator, if (keep_rhs) .{
                .lhs = other_member.captures,
                .rhs = kept_captures,
            } else .{
                .lhs = kept_captures,
                .rhs = other_member.captures,
            });
        }

        // Every other member already sits in the kept set: the merge is the
        // kept set itself.
        if (added.items.len == 0) return kept;
        // A kept set at the end of the member pool grows in place. Every set
        // sharing its start stays a prefix of it, so they all read the same
        // stored members.
        if (self.lambdaSetGrowsInPlace(kept)) {
            _ = try self.program.types.addMembers(added.items);
            return .{ .start = kept.start, .len = @intCast(kept.count() + added.items.len) };
        }
        var members = try std.ArrayList(Type.FnMember).initCapacity(self.allocator, kept.count() + added.items.len);
        defer members.deinit(self.allocator);
        for (0..kept.count()) |i| members.appendAssumeCapacity(self.program.types.memberItem(kept, i));
        members.appendSliceAssumeCapacity(added.items);
        return try self.program.types.addMembers(members.items);
    }

    /// Whether `span` is a nonempty set ending at the end of the member pool,
    /// so members appended to the pool extend it.
    fn lambdaSetGrowsInPlace(self: *const Solver, span: Type.Span) bool {
        return span.count() != 0 and @as(usize, span.start) + span.count() == self.program.types.fn_members.items.len;
    }

    const LambdaSetIndex = struct {
        len: u32,
        by_lambda: std.AutoHashMapUnmanaged(Common.Symbol, usize),

        /// The first position of `lambda` within `span`, a set sharing this
        /// index's start.
        fn position(index: *const LambdaSetIndex, lambda: Common.Symbol, span: Type.Span) ?usize {
            const found = index.by_lambda.get(lambda) orelse return null;
            return if (found < span.count()) found else null;
        }
    };

    /// Member positions by lambda per stored lambda set, keyed by the set's
    /// start. Stored members never change and a set grows only in place, so
    /// every set sharing a start is a prefix of the longest, and one index
    /// serves them all, extended as the longest grows.
    fn lambdaSetIndex(self: *Solver, span: Type.Span) Allocator.Error!*const LambdaSetIndex {
        const gop = try self.lambda_set_indexes.getOrPut(self.allocator, span.start);
        if (!gop.found_existing) gop.value_ptr.* = .{ .len = 0, .by_lambda = .empty };
        if (gop.value_ptr.len >= span.len) return gop.value_ptr;
        try gop.value_ptr.by_lambda.ensureUnusedCapacity(self.allocator, span.len - gop.value_ptr.len);
        for (gop.value_ptr.len..span.count()) |index| {
            const entry = gop.value_ptr.by_lambda.getOrPutAssumeCapacity(self.program.types.memberItem(span, index).lambda);
            if (!entry.found_existing) entry.value_ptr.* = index;
        }
        gop.value_ptr.len = span.len;
        return gop.value_ptr;
    }

    fn solvedTypeDigest(self: *Solver, ty: Type.TypeVarId) Allocator.Error!Type.names.TypeDigest {
        var hasher = TypeDigestHasher.init();
        var active = self.solved_position_pool.acquire();
        defer self.solved_position_pool.release(&active);
        try self.writeSolvedTypeDigest(&hasher, ty, &active);
        return .{ .bytes = hasher.finalResult() };
    }

    /// One step of a solved type digest: a write, a type to digest, or the
    /// end of a type's digest.
    const DigestAction = union(enum) {
        visit: Type.TypeVarId,
        /// `writeBytes`
        bytes: []const u8,
        /// A raw 32-byte identity.
        raw: [32]u8,
        word: u32,
        optional_word: ?u32,
        field_default: ?MonoType.FieldDefault,
        /// The type left the active path.
        leave: Type.TypeVarId,
    };

    /// Write the digest of `ty`. Each type's digest is its writes and its
    /// components' digests in order; the pending steps wait on an explicit
    /// stack so type nesting never becomes native call depth.
    fn writeSolvedTypeDigest(
        self: *Solver,
        hasher: *TypeDigestHasher,
        ty: Type.TypeVarId,
        active: *collections.DenseMap(Type.TypeVarId, u32),
    ) Allocator.Error!void {
        var actions: std.ArrayList(DigestAction) = .empty;
        defer actions.deinit(self.allocator);
        errdefer for (actions.items) |action| switch (action) {
            .leave => |root| _ = active.remove(root),
            .visit, .bytes, .raw, .word, .optional_word, .field_default => {},
        };
        try actions.append(self.allocator, .{ .visit = ty });
        while (actions.pop()) |action| switch (action) {
            .visit => |child| try self.expandSolvedTypeDigest(hasher, child, active, &actions),
            .bytes => |bytes| writeBytes(hasher, bytes),
            .raw => |raw| hasher.update(&raw),
            .word => |word| writeU32(hasher, word),
            .optional_word => |word| writeOptionalU32(hasher, word),
            .field_default => |default| MonoType.writeFieldDefaultDigest(self.lifted.names, hasher, default),
            .leave => |root| _ = active.remove(root),
        };
    }

    /// Write a back reference for a type already on the active path, or
    /// push the steps of its digest.
    fn expandSolvedTypeDigest(
        self: *Solver,
        hasher: *TypeDigestHasher,
        ty: Type.TypeVarId,
        active: *collections.DenseMap(Type.TypeVarId, u32),
        actions: *std.ArrayList(DigestAction),
    ) Allocator.Error!void {
        var root = ty;
        var content: Type.Content = undefined;
        while (true) {
            root = self.program.types.rootCompressed(root);
            content = try self.resolvedContentAt(root);

            // Materializing a lazy leaf can link it to an existing clone.
            // Re-root before recording this node in the active traversal.
            const materialized_root = self.program.types.rootCompressed(root);
            if (materialized_root != root) {
                root = materialized_root;
                continue;
            }

            // Transparent aliases have no runtime or source-function identity;
            // their backing supplies the erased-callable digest identity.
            root = transparentAliasBacking(content) orelse break;
        }

        if (active.get(root)) |position| {
            // Stack positions, unlike TypeVarIds, are stable across separately
            // cloned but isomorphic recursive type graphs.
            writeBytes(hasher, "cycle");
            writeU32(hasher, position);
            return;
        }

        // This type's steps are listed in order, then reversed so the first
        // is popped next.
        const start = actions.items.len;
        try actions.append(self.allocator, .{ .leave = root });
        try active.putNoClobber(root, @intCast(active.count()));
        const types = &self.program.types;
        const name_store = self.lifted.names;
        switch (content) {
            .mono => Common.invariant("lazy Monotype leaf reached digest hashing unexpanded"),
            .link => Common.invariant("Lambda Solved root returned a link"),
            .unbound, .forall => Common.invariant("unresolved Lambda Solved type reached erased callable digest"),
            .primitive => |primitive| {
                try self.addDigestActions(actions, &.{ .{ .bytes = "primitive" }, .{ .bytes = @tagName(primitive) } });
            },
            .zst => try self.addDigestActions(actions, &.{.{ .bytes = "zst" }}),
            .erased => |erased| try self.addDigestActions(actions, &.{ .{ .bytes = "erased" }, .{ .raw = erased.source_fn_ty.bytes } }),
            .func => |func| {
                try self.addDigestActions(actions, &.{.{ .bytes = "func" }});
                try self.addSpanDigestActions(actions, func.args);
                try self.addDigestActions(actions, &.{.{ .visit = func.ret }});
            },
            .list => |elem| try self.addDigestActions(actions, &.{ .{ .bytes = "list" }, .{ .visit = elem } }),
            .box => |elem| try self.addDigestActions(actions, &.{ .{ .bytes = "box" }, .{ .visit = elem } }),
            .tuple => |items| {
                try self.addDigestActions(actions, &.{.{ .bytes = "tuple" }});
                try self.addSpanDigestActions(actions, items);
            },
            .record => |fields| {
                try self.addDigestActions(actions, &.{ .{ .bytes = "record" }, .{ .word = @intCast(fields.count()) } });
                for (0..fields.count()) |index| {
                    const field = types.fieldItem(fields, index);
                    try self.addDigestActions(actions, &.{
                        .{ .bytes = name_store.recordFieldLabelText(field.name) },
                        .{ .field_default = field.default },
                    });
                    if (field.value_ty) |value_ty| {
                        try self.addDigestActions(actions, &.{ .{ .bytes = "field-optional-value" }, .{ .visit = value_ty } });
                    } else {
                        try self.addDigestActions(actions, &.{.{ .bytes = "field-inline-value" }});
                    }
                    try self.addDigestActions(actions, &.{.{ .visit = field.ty }});
                }
            },
            .tag_union => |tags| {
                try self.addDigestActions(actions, &.{ .{ .bytes = "tag_union" }, .{ .word = @intCast(tags.count()) } });
                for (0..tags.count()) |index| {
                    const tag = types.tagItem(tags, index);
                    try self.addDigestActions(actions, &.{.{ .bytes = name_store.tagLabelText(tag.name) }});
                    try self.addSpanDigestActions(actions, tag.payloads);
                }
            },
            .named => |named| {
                try self.addDigestActions(actions, &.{
                    .{ .bytes = "named" },
                    .{ .raw = named.named_type.module.bytes },
                    .{ .bytes = name_store.moduleIdentityBytes(named.def.module) },
                    .{ .optional_word = named.def.source_decl },
                    .{ .bytes = name_store.typeNameText(named.def.type_name) },
                    .{ .bytes = @tagName(named.kind) },
                });
                if (named.builtin_owner) |owner| {
                    try self.addDigestActions(actions, &.{ .{ .bytes = "builtin" }, .{ .bytes = @tagName(owner) } });
                } else {
                    try self.addDigestActions(actions, &.{.{ .bytes = "not-builtin" }});
                }
                try self.addSpanDigestActions(actions, named.args);
            },
            .lambda_set => |members| {
                try self.addDigestActions(actions, &.{ .{ .bytes = "lambda_set" }, .{ .word = @intCast(members.count()) } });
                for (0..members.count()) |member_index| {
                    const member = types.memberItem(members, member_index);
                    try self.addDigestActions(actions, &.{
                        .{ .word = @intFromEnum(member.lambda) },
                        .{ .word = @intCast(member.captures.count()) },
                    });
                    for (0..member.captures.count()) |capture_index| {
                        const capture = types.captureItem(member.captures, capture_index);
                        try self.addDigestActions(actions, &.{
                            .{ .word = @intFromEnum(capture.symbol) },
                            .{ .visit = capture.ty },
                        });
                    }
                }
            },
        }
        // The `leave` step stays last, below this type's other steps.
        std.mem.reverse(DigestAction, actions.items[start + 1 ..]);
    }

    fn addDigestActions(self: *Solver, actions: *std.ArrayList(DigestAction), steps: []const DigestAction) Allocator.Error!void {
        try actions.appendSlice(self.allocator, steps);
    }

    fn addSpanDigestActions(self: *Solver, actions: *std.ArrayList(DigestAction), span: Type.Span) Allocator.Error!void {
        try actions.append(self.allocator, .{ .word = @intCast(span.count()) });
        for (0..span.count()) |index| {
            try actions.append(self.allocator, .{ .visit = self.program.types.spanItem(span, index) });
        }
    }
};

fn writeBytes(hasher: *TypeDigestHasher, bytes: []const u8) void {
    writeU32(hasher, @intCast(bytes.len));
    hasher.update(bytes);
}

fn writeOptionalU32(hasher: *TypeDigestHasher, value: ?u32) void {
    if (value) |v| {
        hasher.update(&[_]u8{1});
        writeU32(hasher, v);
    } else {
        hasher.update(&[_]u8{0});
    }
}

fn writeU32(hasher: *TypeDigestHasher, value: u32) void {
    const little = std.mem.nativeToLittle(u32, value);
    hasher.update(std.mem.asBytes(&little));
}

const ReachabilityMasks = struct {
    contains_callable: []bool,
    contains_forced_dynamic: []bool,
};

/// Reverse-reachability over the lifted Monotype store: from `func` and
/// `erased` nodes (types whose clones need fresh callable slots) and from
/// forced-dynamic iterator named nodes (leaves the forced-dynamic scan must
/// materialize).
fn computeReachabilityMasks(allocator: Allocator, types: anytype) Allocator.Error!ReachabilityMasks {
    const count = types.types.len;
    const flags = try allocator.alloc(bool, count);
    errdefer allocator.free(flags);
    @memset(flags, false);
    const forced = try allocator.alloc(bool, count);
    errdefer allocator.free(forced);
    @memset(forced, false);

    const edge_counts = try allocator.alloc(u32, count);
    defer allocator.free(edge_counts);
    @memset(edge_counts, 0);

    const Walk = struct {
        fn children(store: @TypeOf(types), content: MonoType.Content, callback: anytype) void {
            switch (content) {
                .primitive, .zst, .erased => {},
                .list, .box => |elem| callback.child(elem),
                .tuple => |items| for (store.span(items)) |item| callback.child(item),
                .record => |fields| for (store.fieldSpan(fields)) |field| {
                    callback.child(field.ty);
                    if (field.value_ty) |value_ty| callback.child(value_ty);
                },
                .tag_union => |tags| for (store.tagSpan(tags)) |tag| {
                    for (store.span(tag.payloads)) |payload| callback.child(payload);
                },
                .named => |named| {
                    for (store.span(named.args)) |arg| callback.child(arg);
                    if (named.backing) |backing| callback.child(backing.ty);
                    for (store.declaredFieldSpan(named.declared_order)) |declared| switch (declared) {
                        .named => {},
                        .padding => |padding_ty| callback.child(padding_ty),
                    };
                },
                .func => |func| {
                    for (store.span(func.args)) |arg| callback.child(arg);
                    callback.child(func.ret);
                },
            }
        }
    };

    for (types.types) |content| {
        const Counter = struct {
            counts: []u32,
            fn child(self: @This(), ty: MonoType.TypeId) void {
                self.counts[@intFromEnum(ty)] += 1;
            }
        };
        Walk.children(types, content, Counter{ .counts = edge_counts });
    }

    var parent_starts = try allocator.alloc(u32, count + 1);
    defer allocator.free(parent_starts);
    parent_starts[0] = 0;
    for (edge_counts, 0..) |edge_count, index| {
        parent_starts[index + 1] = parent_starts[index] + edge_count;
    }
    const parents = try allocator.alloc(u32, parent_starts[count]);
    defer allocator.free(parents);
    const parent_writes = try allocator.dupe(u32, parent_starts[0..count]);
    defer allocator.free(parent_writes);
    for (types.types, 0..) |content, parent_index| {
        const Filler = struct {
            parents: []u32,
            writes: []u32,
            parent: u32,
            fn child(self: @This(), ty: MonoType.TypeId) void {
                const child_index = @intFromEnum(ty);
                self.parents[self.writes[child_index]] = self.parent;
                self.writes[child_index] += 1;
            }
        };
        Walk.children(types, content, Filler{ .parents = parents, .writes = parent_writes, .parent = @intCast(parent_index) });
    }

    var work = std.ArrayList(u32).empty;
    defer work.deinit(allocator);
    for (types.types, 0..) |content, index| {
        const tag = std.meta.activeTag(content);
        if (tag == .func or tag == .erased) {
            flags[index] = true;
            try work.append(allocator, @intCast(index));
        }
    }
    while (work.pop()) |index| {
        for (parents[parent_starts[index]..parent_starts[index + 1]]) |parent| {
            if (flags[parent]) continue;
            flags[parent] = true;
            try work.append(allocator, parent);
        }
    }
    for (types.types, 0..) |content, index| {
        if (std.meta.activeTag(content) == .named) {
            if (content.named.def.iterator_representation == .forced_dynamic) {
                forced[index] = true;
                try work.append(allocator, @intCast(index));
            }
        }
    }
    while (work.pop()) |index| {
        for (parents[parent_starts[index]..parent_starts[index + 1]]) |parent| {
            if (forced[parent]) continue;
            forced[parent] = true;
            try work.append(allocator, parent);
        }
    }
    return .{ .contains_callable = flags, .contains_forced_dynamic = forced };
}

/// Decides whether a solved type is proven uninhabited. A type on the active
/// path is not.
const SolvedUninhabitedScan = struct {
    solver: *Solver,
    /// Path stops seen by this walk: a variable already on the path answers
    /// provisionally, so no answer above it is remembered.
    path_stops: u32 = 0,
    /// For each variable on the active path, the walk's state when it
    /// entered: its answer is remembered only when nothing below it stopped
    /// at the path and the type store was not mutated meanwhile.
    entry_marks: std.ArrayList(EntryMark) = .empty,

    const EntryMark = struct { path_stops: u32, mono_stops: u32, epoch: u64 };

    const Eval = AnyAll.Evaluation(Type.TypeVarId, SolvedUninhabitedScan);

    pub fn enter(self: *SolvedUninhabitedScan, items: Eval.Items, ty: Type.TypeVarId) Allocator.Error!Eval.Expansion {
        const types = &self.solver.program.types;
        const root = types.rootCompressed(ty);
        if (self.solver.uninhabited_memo.get(root)) |answer| {
            if (answer.epoch == types.mutation_epoch) return .{ .value = answer.uninhabited };
        }
        if (self.solver.uninhabited_path.isSet(@intFromEnum(root))) {
            self.path_stops += 1;
            return .{ .value = false };
        }
        const expansion: Eval.Expansion = switch (types.get(root)) {
            // Probe leaves against the lifted store instead of materializing:
            // uninhabitedness is a pure function of the Monotype.
            .mono => |leaf| .{ .value = try self.solver.monoProvenUninhabited(leaf.id) },
            .named => |named| blk: {
                const backing = named.backing orelse break :blk .{ .value = false };
                if (backing.use != .inspectable) break :blk .{ .value = false };
                try items.add(backing.ty);
                break :blk .{ .group = .any };
            },
            // Uninhabited when every tag has an uninhabited payload.
            .tag_union => |tags| blk: {
                if (tags.count() == 0) break :blk .{ .value = true };
                for (0..tags.count()) |tag_index| {
                    const tag = types.tagItem(tags, tag_index);
                    try items.group(.any, tag.payloads.count());
                    for (0..tag.payloads.count()) |payload_index| try items.add(types.spanItem(tag.payloads, payload_index));
                }
                break :blk .{ .group = .all };
            },
            .tuple => |elems| blk: {
                for (0..elems.count()) |index| try items.add(types.spanItem(elems, index));
                break :blk .{ .group = .any };
            },
            .record => |fields| blk: {
                for (0..fields.count()) |index| try items.add(types.fieldItem(fields, index).ty);
                break :blk .{ .group = .any };
            },
            .box => |payload| blk: {
                try items.add(payload);
                break :blk .{ .group = .any };
            },
            .list, .func, .primitive, .lambda_set, .erased, .zst, .link, .unbound, .forall => .{ .value = false },
        };
        if (expansion == .group) {
            try self.entry_marks.append(self.solver.allocator, .{
                .path_stops = self.path_stops,
                .mono_stops = self.solver.mono_uninhabited_path_stops,
                .epoch = types.mutation_epoch,
            });
            self.solver.uninhabited_path.set(@intFromEnum(root));
        }
        return expansion;
    }

    pub fn exit(self: *SolvedUninhabitedScan, ty: Type.TypeVarId, result: ?bool) std.mem.Allocator.Error!void {
        const types = &self.solver.program.types;
        const root = types.rootCompressed(ty);
        self.solver.uninhabited_path.unset(@intFromEnum(root));
        const mark = self.entry_marks.pop().?;
        const value = result orelse return;
        if (mark.path_stops != self.path_stops or
            mark.mono_stops != self.solver.mono_uninhabited_path_stops or
            mark.epoch != types.mutation_epoch) return;
        try self.solver.uninhabited_memo.put(root, .{ .epoch = mark.epoch, .uninhabited = value });
    }
};

/// Decides whether a lifted Monotype is proven uninhabited. A type on the
/// active path is not. A type's answer is remembered only when no type below
/// it stopped at the active path, since such an answer depends on the path.
const MonoUninhabitedScan = struct {
    solver: *Solver,
    /// The solver's path-stop count when each type on the active path entered.
    entry_stops: std.ArrayList(u32) = .empty,

    const Eval = AnyAll.Evaluation(MonoType.TypeId, MonoUninhabitedScan);

    pub fn enter(self: *MonoUninhabitedScan, items: Eval.Items, id: MonoType.TypeId) Allocator.Error!Eval.Expansion {
        if (self.solver.mono_uninhabited.get(id)) |result| return .{ .value = result };
        if (self.solver.mono_uninhabited_path.isSet(@intFromEnum(id))) {
            self.solver.mono_uninhabited_path_stops += 1;
            return .{ .value = false };
        }
        const types = self.solver.lifted.types;
        const expansion: Eval.Expansion = switch (types.get(id)) {
            .named => |named| blk: {
                const backing = named.backing orelse break :blk .{ .value = false };
                if (backing.use != .inspectable) break :blk .{ .value = false };
                try items.add(backing.ty);
                break :blk .{ .group = .any };
            },
            // Uninhabited when every tag has an uninhabited payload.
            .tag_union => |tags| blk: {
                const tag_span = types.tagSpan(tags);
                if (tag_span.len == 0) break :blk .{ .value = true };
                for (tag_span) |tag| {
                    const payloads = types.span(tag.payloads);
                    try items.group(.any, payloads.len);
                    for (payloads) |payload| try items.add(payload);
                }
                break :blk .{ .group = .all };
            },
            .tuple => |elems| blk: {
                for (types.span(elems)) |item| try items.add(item);
                break :blk .{ .group = .any };
            },
            .record => |fields| blk: {
                for (types.fieldSpan(fields)) |field| try items.add(field.ty);
                break :blk .{ .group = .any };
            },
            .box => |payload| blk: {
                try items.add(payload);
                break :blk .{ .group = .any };
            },
            .list, .func, .primitive, .erased, .zst => .{ .value = false },
        };
        if (expansion == .group) {
            try self.entry_stops.append(self.solver.allocator, self.solver.mono_uninhabited_path_stops);
            self.solver.mono_uninhabited_path.set(@intFromEnum(id));
        }
        return expansion;
    }

    pub fn exit(self: *MonoUninhabitedScan, id: MonoType.TypeId, result: ?bool) std.mem.Allocator.Error!void {
        self.solver.mono_uninhabited_path.unset(@intFromEnum(id));
        const stops = self.entry_stops.pop().?;
        const value = result orelse return;
        if (stops == self.solver.mono_uninhabited_path_stops) try self.solver.mono_uninhabited.put(id, value);
    }
};

const TypeCloner = struct {
    solver: *Solver,
    map: collections.DenseMap(MonoType.TypeId, Type.TypeVarId),
    /// Unification rewrites var contents in place (alias backings, named
    /// absorption, uninhabited links), so clones that can still reach `unify`
    /// must stay per-use. After solving no var is unified again, and clones of
    /// callable-free types carry no unbound slots, so those may share one var
    /// per Monotype during finalization.
    share: bool = false,
    /// One-level mode: children lower to lazy leaves instead of eager clones,
    /// callable-free ones in the shared context and the rest in this one,
    /// reusing a context's existing var when the Monotype already occurs in
    /// it. Used by `expandMonoRoot`.
    lazy_ctx: ?u32 = null,

    fn init(solver: *Solver) TypeCloner {
        return .{
            .solver = solver,
            .map = solver.clone_map_pool.acquire(),
        };
    }

    fn deinit(self: *TypeCloner) void {
        self.solver.clone_map_pool.release(&self.map);
    }

    /// Clone a Monotype into the solver's store. Cloning a type clones its
    /// component types from inside its own clone; each unfinished clone waits
    /// in a `CloneFrame` on one heap-backed stack, so type nesting never
    /// becomes native call depth. Components clone, and spans are added, in
    /// the order a direct recursive clone visited them.
    fn lower(self: *TypeCloner, ty: MonoType.TypeId) Allocator.Error!Type.TypeVarId {
        if (try self.existingClone(ty)) |existing| return existing;
        const allocator = self.solver.allocator;
        var stack: std.ArrayList(CloneFrame) = self.solver.spare_clone_stacks.pop() orelse .empty;
        const frames = &stack;
        defer {
            while (frames.items.len > 0) {
                self.solver.releaseCloneLists(&frames.items[frames.items.len - 1].build);
                frames.items.len -= 1;
            }
            self.solver.spare_clone_stacks.append(allocator, stack) catch stack.deinit(allocator);
        }
        // A frame is built in place on the stack, so a begun frame's build
        // is always owned by the stack.
        try self.beginClone(try frames.addOne(allocator), ty);
        var input: ?Type.TypeVarId = null;
        while (true) {
            const frame = &frames.items[frames.items.len - 1];
            if (input) |lowered| try frame.build.results.append(allocator, lowered);
            input = null;
            const child = while (frame.build.next < frame.build.parts.items.len) {
                const part = frame.build.parts.items[frame.build.next];
                frame.build.next += 1;
                const child_ty = try self.issueClonePart(&frame.build, part) orelse continue;
                if (try self.existingClone(child_ty)) |existing| {
                    try frame.build.results.append(allocator, existing);
                    continue;
                }
                break child_ty;
            } else null;
            if (child) |child_ty| {
                try self.beginClone(try frames.addOne(allocator), child_ty);
                continue;
            }
            const finished = &frames.items[frames.items.len - 1];
            const lowered = try self.finishClone(&finished.build);
            const finished_ty = finished.ty;
            const reserved = finished.reserved;
            const shareable = finished.shareable;
            self.solver.releaseCloneLists(&finished.build);
            frames.items.len -= 1;
            self.solver.program.types.set(reserved, lowered);
            self.solver.registerNamedBacking(lowered);
            if (shareable) try self.solver.shared_clones.put(finished_ty, reserved);
            if (frames.items.len == 0) return reserved;
            input = reserved;
        }
    }

    /// The clone `ty` already has, or that lazy mode creates without
    /// descending; null when it must be cloned.
    fn existingClone(self: *TypeCloner, ty: MonoType.TypeId) Allocator.Error!?Type.TypeVarId {
        if (self.lazy_ctx) |parent_ctx| {
            // A callable-free child is the same shared clone wherever it
            // occurs, exactly as a callable-free root is: it has no callable
            // slot to solve, so unifying two of its occurrences reaches one
            // var instead of walking two copies of its structure.
            const ctx = if (self.solver.isCallableFree(ty)) try self.solver.sharedLeafContext() else parent_ctx;
            const map = &self.solver.leaf_contexts.items[ctx];
            if (map.get(ty)) |existing| return existing;
            const created = try self.solver.program.types.add(.{ .mono = .{ .id = ty, .ctx = ctx } });
            try map.put(ty, created);
            return created;
        }
        if (self.map.get(ty)) |cached| return cached;
        if (self.share and !self.solver.contains_callable[@intFromEnum(ty)]) {
            if (self.solver.shared_clones.get(ty)) |shared| {
                try self.map.put(ty, shared);
                return shared;
            }
        }
        return null;
    }

    const CloneFrame = struct {
        ty: MonoType.TypeId,
        reserved: Type.TypeVarId,
        shareable: bool,
        build: CloneBuild,
    };

    /// One component a clone lowers, or a store entry it adds, in order.
    const ClonePart = union(enum) {
        ty: MonoType.TypeId,
        /// Add the components lowered since result `start` as one span.
        span: usize,
        /// A function's fresh callable slot.
        callable,
        /// A named type's backing, found structurally when it is reached.
        backing: struct { def: MonoType.TypeDef, ty: MonoType.TypeId },
    };

    const CloneBuild = struct {
        content: MonoType.Content,
        parts: std.ArrayList(ClonePart) = .empty,
        next: usize = 0,
        results: std.ArrayList(Type.TypeVarId) = .empty,
        spans: std.ArrayList(Type.Span) = .empty,
    };

    /// A build's lists, pooled on the solver between builds.
    const CloneLists = struct {
        parts: std.ArrayList(ClonePart) = .empty,
        results: std.ArrayList(Type.TypeVarId) = .empty,
        spans: std.ArrayList(Type.Span) = .empty,

        fn deinit(self: *CloneLists, allocator: Allocator) void {
            self.parts.deinit(allocator);
            self.results.deinit(allocator);
            self.spans.deinit(allocator);
        }
    };

    /// Begin cloning `ty` in `frame`, which is written in place: a frame
    /// holds the type's content and three lists, so it is never copied.
    /// The frame's build owns its lists before anything can fail.
    fn beginClone(self: *TypeCloner, frame: *CloneFrame, ty: MonoType.TypeId) Allocator.Error!void {
        frame.* = .{
            .ty = ty,
            .reserved = undefined,
            .shareable = self.share and !self.solver.contains_callable[@intFromEnum(ty)],
            .build = .{ .content = self.solver.lifted.types.get(ty) },
        };
        self.solver.acquireCloneLists(&frame.build);
        const reserved = try self.solver.program.types.add(.unbound);
        try self.map.put(ty, reserved);
        frame.reserved = reserved;
        try self.planClone(&frame.build);
    }

    /// Lower one Monotype content whose components are cloned already or,
    /// in lazy mode, become lazy leaves.
    fn lowerContent(self: *TypeCloner, content: MonoType.Content) Allocator.Error!Type.Content {
        std.debug.assert(self.lazy_ctx != null);
        const allocator = self.solver.allocator;
        var build = CloneBuild{ .content = content };
        self.solver.acquireCloneLists(&build);
        defer self.solver.releaseCloneLists(&build);
        try self.planClone(&build);
        for (build.parts.items) |part| {
            const child = try self.issueClonePart(&build, part) orelse continue;
            try build.results.append(allocator, (try self.existingClone(child)).?);
        }
        return try self.finishClone(&build);
    }

    inline fn addClonePart(self: *TypeCloner, build: *CloneBuild, part: ClonePart) Allocator.Error!void {
        if (build.parts.items.len == build.parts.capacity) try build.parts.ensureUnusedCapacity(self.solver.allocator, 1);
        build.parts.appendAssumeCapacity(part);
    }

    fn addCloneSpanParts(self: *TypeCloner, build: *CloneBuild, start: *usize, span: MonoType.Span) Allocator.Error!void {
        const items = self.solver.lifted.types.span(span);
        for (items) |item| try self.addClonePart(build, .{ .ty = item });
        try self.addClonePart(build, .{ .span = start.* });
        start.* += items.len;
    }

    fn planClone(self: *TypeCloner, build: *CloneBuild) Allocator.Error!void {
        const types = self.solver.lifted.types;
        var start: usize = 0;
        switch (build.content) {
            .primitive, .zst, .erased => {},
            .list, .box => |elem| try self.addClonePart(build, .{ .ty = elem }),
            .tuple => |items| try self.addCloneSpanParts(build, &start, items),
            .record => |fields| for (types.fieldSpan(fields)) |field| {
                try self.addClonePart(build, .{ .ty = field.ty });
                if (field.value_ty) |value_ty| try self.addClonePart(build, .{ .ty = value_ty });
            },
            .tag_union => |tags| for (types.tagSpan(tags)) |tag| try self.addCloneSpanParts(build, &start, tag.payloads),
            .named => |named| {
                try self.addCloneSpanParts(build, &start, named.args);
                if (named.backing) |raw_backing| try self.addClonePart(build, if (raw_backing.authority == .generated_private)
                    .{ .ty = raw_backing.ty }
                else
                    .{ .backing = .{ .def = named.def, .ty = raw_backing.ty } });
                for (types.declaredFieldSpan(named.declared_order)) |entry| switch (entry) {
                    .named => {},
                    .padding => |ty| try self.addClonePart(build, .{ .ty = ty }),
                };
            },
            .func => |fn_ty| {
                try self.addCloneSpanParts(build, &start, fn_ty.args);
                try self.addClonePart(build, .callable);
                try self.addClonePart(build, .{ .ty = fn_ty.ret });
            },
        }
    }

    /// Carry out a part that needs no clone, or return the type it clones.
    fn issueClonePart(self: *TypeCloner, build: *CloneBuild, part: ClonePart) Allocator.Error!?MonoType.TypeId {
        const allocator = self.solver.allocator;
        switch (part) {
            .ty => |ty| return ty,
            .backing => |backing| return try self.structuralBackingForNamed(backing.def, backing.ty),
            .span => |start| {
                try build.spans.append(allocator, try self.solver.program.types.addSpan(build.results.items[start..]));
                return null;
            },
            .callable => {
                try build.results.append(allocator, try self.solver.program.types.add(.unbound));
                return null;
            },
        }
    }

    /// Reads a clone's lowered components and spans in the order it listed
    /// them.
    const CloneReader = struct {
        build: *const CloneBuild,
        result: usize = 0,
        span: usize = 0,

        fn next(self: *CloneReader) Type.TypeVarId {
            const lowered = self.build.results.items[self.result];
            self.result += 1;
            return lowered;
        }

        fn nextSpan(self: *CloneReader) Type.Span {
            const span = self.build.spans.items[self.span];
            self.span += 1;
            self.result += span.count();
            return span;
        }
    };

    fn finishClone(self: *TypeCloner, build: *CloneBuild) Allocator.Error!Type.Content {
        const allocator = self.solver.allocator;
        const types = self.solver.lifted.types;
        const store = &self.solver.program.types;
        var reader = CloneReader{ .build = build };
        return switch (build.content) {
            .primitive => |primitive| .{ .primitive = primitive },
            .zst => .zst,
            .erased => |source_fn_ty| .{ .erased = .{ .source_fn_ty = source_fn_ty, .members = .empty() } },
            .list => .{ .list = reader.next() },
            .box => .{ .box = reader.next() },
            .tuple => .{ .tuple = reader.nextSpan() },
            .record => |fields| blk: {
                const lowered = try allocator.alloc(Type.Field, fields.len);
                defer allocator.free(lowered);
                for (types.fieldSpan(fields), lowered) |field, *out| {
                    const field_ty = reader.next();
                    out.* = .{
                        .name = field.name,
                        .ty = field_ty,
                        .value_ty = if (field.value_ty != null) reader.next() else null,
                        .default = field.default,
                    };
                }
                break :blk .{ .record = try store.addFields(lowered) };
            },
            .tag_union => |tags| blk: {
                const lowered = try allocator.alloc(Type.Tag, tags.len);
                defer allocator.free(lowered);
                for (types.tagSpan(tags), lowered) |tag, *out| {
                    out.* = .{
                        .name = tag.name,
                        .checked_name = tag.checked_name,
                        .payloads = reader.nextSpan(),
                    };
                }
                break :blk .{ .tag_union = try store.addTags(lowered) };
            },
            .named => |named| blk: {
                const args = reader.nextSpan();
                const backing: @FieldType(@FieldType(Type.Content, "named"), "backing") = if (named.backing) |raw_backing| .{
                    .ty = reader.next(),
                    .use = raw_backing.use,
                    .authority = raw_backing.authority,
                } else null;
                const declared = types.declaredFieldSpan(named.declared_order);
                const declared_order = if (declared.len == 0) Type.Span.empty() else declared: {
                    const lowered = try allocator.alloc(Type.DeclaredField, declared.len);
                    defer allocator.free(lowered);
                    for (declared, lowered) |entry, *out| out.* = switch (entry) {
                        .named => |name| .{ .named = name },
                        .padding => .{ .padding = reader.next() },
                    };
                    break :declared try store.addDeclaredFields(lowered);
                };
                break :blk .{ .named = .{
                    .named_type = named.named_type,
                    .def = named.def,
                    .kind = named.kind,
                    .builtin_owner = named.builtin_owner,
                    .args = args,
                    .backing = backing,
                    .declared_order = declared_order,
                } };
            },
            .func => blk: {
                const args = reader.nextSpan();
                const callable = reader.next();
                break :blk .{ .func = .{
                    .args = args,
                    .callable = callable,
                    .ret = reader.next(),
                } };
            },
        };
    }

    /// Apply the explicit dynamic boundary only after the entire requested
    /// Monotype clone is complete. A forced iterator can be reached while an
    /// enclosing function or payload clone still holds reservations, so doing
    /// this per-node would let callable identity observe an unfinished graph.
    fn markForcedDynamicCallables(self: *TypeCloner) Allocator.Error!void {
        var entries = self.map.iterator();
        while (entries.next()) |entry| {
            const content = self.solver.lifted.types.get(entry.key_ptr.*);
            if (std.meta.activeTag(content) == .named) {
                if (content.named.def.iterator_representation == .forced_dynamic) {
                    try self.solver.markErasedCallablesReachedByType(entry.value_ptr.*);
                }
            }
        }
    }

    fn structuralBackingForNamed(
        self: *TypeCloner,
        owner_def: MonoType.TypeDef,
        backing: MonoType.TypeId,
    ) Allocator.Error!MonoType.TypeId {
        var seen = self.solver.mono_set_pool.acquire();
        defer self.solver.mono_set_pool.release(&seen);
        var current = backing;
        while (true) {
            if (seen.contains(current)) return current;
            try seen.put(current, {});
            const content = self.solver.lifted.types.get(current);
            if (std.meta.activeTag(content) != .named) return current;
            if (content.named.kind != .alias and !sameMonoTypeDef(content.named.def, owner_def)) return current;
            const next = content.named.backing orelse return current;
            current = next.ty;
        }
    }
};

fn sameMonoTypeDef(left: MonoType.TypeDef, right: MonoType.TypeDef) bool {
    return left.module == right.module and
        left.type_name == right.type_name and
        left.source_decl == right.source_decl and
        optionalDigestEql(left.generated, right.generated) and
        left.iterator_representation == right.iterator_representation and
        left.iterator_kind == right.iterator_kind and
        left.iterator_depth == right.iterator_depth and
        std.meta.eql(left.iterator_topology, right.iterator_topology);
}

fn iteratorLikeOwnerFromPair(
    left: ?static_dispatch.BuiltinOwner,
    right: ?static_dispatch.BuiltinOwner,
) ?static_dispatch.BuiltinOwner {
    if (left) |left_owner| {
        if (!isIteratorLikeOwner(left_owner)) return null;
        if (right) |right_owner| {
            if (left_owner != right_owner) return null;
        }
        return left_owner;
    }
    if (right) |right_owner| {
        if (!isIteratorLikeOwner(right_owner)) return null;
        return right_owner;
    }
    return null;
}

fn isIteratorLikeOwner(owner: ?static_dispatch.BuiltinOwner) bool {
    return static_dispatch.isIteratorOwner(owner orelse return false);
}

fn optionalDigestEql(left: ?names.TypeDigest, right: ?names.TypeDigest) bool {
    if (left == null and right == null) return true;
    if (left == null or right == null) return false;
    return std.mem.eql(u8, left.?.bytes[0..], right.?.bytes[0..]);
}

/// The digest tests build already-materialized solved types directly, so only
/// the Solver fields read by `solvedTypeDigest` need test values. Callers own
/// the returned solver's `solved_position_pool` and must deinit it.
fn solvedTypeDigestTestSolver(
    allocator: Allocator,
    program: *Ast.Program,
    name_store: *const names.NameStore,
) Solver {
    var solver: Solver = undefined;
    solver.allocator = allocator;
    solver.mono_uninhabited = collections.DenseMap(MonoType.TypeId, bool).init(allocator);
    defer solver.mono_uninhabited.deinit();
    solver.uninhabited_memo = collections.DenseMap(Type.TypeVarId, UninhabitedAnswer).init(allocator);
    defer solver.uninhabited_memo.deinit();
    solver.mono_uninhabited_path_stops = 0;
    solver.spare_clone_stacks = .empty;
    solver.spare_clone_lists = .empty;
    solver.solved_uninhabited_scratch = .{};
    solver.solved_entry_marks = .empty;
    solver.mono_uninhabited_scratch = .{};
    solver.mono_entry_stops = .empty;
    defer {
        for (solver.spare_clone_stacks.items) |*stack| stack.deinit(allocator);
        solver.spare_clone_stacks.deinit(allocator);
        for (solver.spare_clone_lists.items) |*lists| lists.deinit(allocator);
        solver.spare_clone_lists.deinit(allocator);
        solver.solved_uninhabited_scratch.deinit(allocator);
        solver.solved_entry_marks.deinit(allocator);
        solver.mono_uninhabited_scratch.deinit(allocator);
        solver.mono_entry_stops.deinit(allocator);
    }
    solver.uninhabited_path = .{};
    defer solver.uninhabited_path.deinit(allocator);
    solver.mono_uninhabited_path = .{};
    defer solver.mono_uninhabited_path.deinit(allocator);
    solver.program = program;
    solver.lifted = undefined;
    solver.lifted.names = name_store;
    solver.solved_position_pool = collections.DenseMapPool(Type.TypeVarId, u32).init(allocator);
    solver.tag_row_indexes = .empty;
    solver.lambda_set_indexes = .empty;
    solver.record_row_indexes = .empty;
    solver.shared_leaf_context = null;
    return solver;
}

test "solved type digest treats a transparent alias as its backing" {
    const allocator = std.testing.allocator;

    var name_store = names.NameStore.init(allocator);
    defer name_store.deinit();

    var program: Ast.Program = undefined;
    program.types = Type.Store.init(allocator);
    defer program.types.deinit();

    var solver = solvedTypeDigestTestSolver(allocator, &program, &name_store);
    defer solver.solved_position_pool.deinit();
    const backing = try program.types.add(.{ .primitive = .u64 });
    const module = try name_store.internModuleIdentity(&([_]u8{0xA5} ** 32));
    const type_name = try name_store.internTypeName("Count");
    const alias = try program.types.add(.{ .named = .{
        .named_type = .{ .module = .{}, .ty = undefined },
        .def = .{ .module = module, .type_name = type_name },
        .kind = .alias,
        .args = .empty(),
        .backing = .{ .ty = backing, .use = .inspectable },
    } });

    const backing_digest = try solver.solvedTypeDigest(backing);
    const alias_digest = try solver.solvedTypeDigest(alias);
    try std.testing.expect(std.mem.eql(u8, backing_digest.bytes[0..], alias_digest.bytes[0..]));
}

test "solved type digest is stable across clone-isomorphic cycles" {
    const allocator = std.testing.allocator;

    var name_store = names.NameStore.init(allocator);
    defer name_store.deinit();

    var program: Ast.Program = undefined;
    program.types = Type.Store.init(allocator);
    defer program.types.deinit();

    var solver = solvedTypeDigestTestSolver(allocator, &program, &name_store);
    defer solver.solved_position_pool.deinit();
    const field_name = try name_store.internRecordFieldLabel("step");
    const callable = try program.types.add(.{ .lambda_set = .empty() });

    const record_a = try program.types.add(.unbound);
    const function_a = try program.types.add(.{ .func = .{
        .args = .empty(),
        .callable = callable,
        .ret = record_a,
    } });
    const fields_a = try program.types.addFields(&.{.{ .name = field_name, .ty = function_a, .default = null }});
    program.types.set(record_a, .{ .record = fields_a });

    const record_b = try program.types.add(.unbound);
    const function_b = try program.types.add(.{ .func = .{
        .args = .empty(),
        .callable = callable,
        .ret = record_b,
    } });
    const fields_b = try program.types.addFields(&.{.{ .name = field_name, .ty = function_b, .default = null }});
    program.types.set(record_b, .{ .record = fields_b });

    const digest_a = try solver.solvedTypeDigest(record_a);
    const digest_b = try solver.solvedTypeDigest(record_b);
    try std.testing.expect(std.mem.eql(u8, digest_a.bytes[0..], digest_b.bytes[0..]));
}

test "lambda solved erased callable digest includes record field default identity" {
    const gpa = std.testing.allocator;

    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();
    const field_name = try name_store.internRecordFieldLabel("retries");
    const module = try name_store.internModuleIdentity(&([_]u8{0xD5} ** 32));

    var program: Ast.Program = undefined;
    program.types = Type.Store.init(gpa);
    defer program.types.deinit();

    const value_ty = try program.types.add(.{ .primitive = .u8 });
    const plain_ty = try program.types.add(.{ .record = try program.types.addFields(&.{.{
        .name = field_name,
        .ty = value_ty,
        .default = null,
    }}) });
    const first_default_ty = try program.types.add(.{ .record = try program.types.addFields(&.{.{
        .name = field_name,
        .ty = value_ty,
        .default = .{ .module = module, .expr_node = 3 },
    }}) });
    const second_default_ty = try program.types.add(.{ .record = try program.types.addFields(&.{.{
        .name = field_name,
        .ty = value_ty,
        .default = .{ .module = module, .expr_node = 4 },
    }}) });

    var lifted: Lifted.ProgramView = undefined;
    lifted.names = &name_store;
    var solver: Solver = undefined;
    solver.allocator = gpa;
    solver.mono_uninhabited = collections.DenseMap(MonoType.TypeId, bool).init(gpa);
    defer solver.mono_uninhabited.deinit();
    solver.uninhabited_memo = collections.DenseMap(Type.TypeVarId, UninhabitedAnswer).init(gpa);
    defer solver.uninhabited_memo.deinit();
    solver.mono_uninhabited_path_stops = 0;
    solver.spare_clone_stacks = .empty;
    solver.spare_clone_lists = .empty;
    solver.solved_uninhabited_scratch = .{};
    solver.solved_entry_marks = .empty;
    solver.mono_uninhabited_scratch = .{};
    solver.mono_entry_stops = .empty;
    defer {
        for (solver.spare_clone_stacks.items) |*stack| stack.deinit(gpa);
        solver.spare_clone_stacks.deinit(gpa);
        for (solver.spare_clone_lists.items) |*lists| lists.deinit(gpa);
        solver.spare_clone_lists.deinit(gpa);
        solver.solved_uninhabited_scratch.deinit(gpa);
        solver.solved_entry_marks.deinit(gpa);
        solver.mono_uninhabited_scratch.deinit(gpa);
        solver.mono_entry_stops.deinit(gpa);
    }
    solver.uninhabited_path = .{};
    defer solver.uninhabited_path.deinit(gpa);
    solver.mono_uninhabited_path = .{};
    defer solver.mono_uninhabited_path.deinit(gpa);
    solver.program = &program;
    solver.lifted = lifted;
    solver.solved_position_pool = collections.DenseMapPool(Type.TypeVarId, u32).init(gpa);
    defer solver.solved_position_pool.deinit();

    const plain_digest = try solver.solvedTypeDigest(plain_ty);
    const first_default_digest = try solver.solvedTypeDigest(first_default_ty);
    const second_default_digest = try solver.solvedTypeDigest(second_default_ty);
    try std.testing.expect(!std.mem.eql(u8, plain_digest.bytes[0..], first_default_digest.bytes[0..]));
    try std.testing.expect(!std.mem.eql(u8, first_default_digest.bytes[0..], second_default_digest.bytes[0..]));
}

test "inspectable backing unification isolates the structural type variable once" {
    const allocator = std.testing.allocator;

    var program: Ast.Program = undefined;
    program.types = Type.Store.init(allocator);
    defer program.types.deinit();

    const structural = try program.types.add(.{ .primitive = .u64 });
    var backing = try program.types.add(.{ .primitive = .u64 });
    for (0..4) |_| {
        backing = try program.types.add(.{ .named = .{
            .named_type = undefined,
            .def = undefined,
            .kind = .nominal,
            .args = .empty(),
            .backing = .{ .ty = backing, .use = .inspectable },
        } });
    }
    const outer_named = backing;
    const vars_before_unify = program.types.vars.items.len;

    var solver: Solver = undefined;
    solver.allocator = allocator;
    solver.mono_uninhabited = collections.DenseMap(MonoType.TypeId, bool).init(allocator);
    defer solver.mono_uninhabited.deinit();
    solver.uninhabited_memo = collections.DenseMap(Type.TypeVarId, UninhabitedAnswer).init(allocator);
    defer solver.uninhabited_memo.deinit();
    solver.mono_uninhabited_path_stops = 0;
    solver.spare_clone_stacks = .empty;
    solver.spare_clone_lists = .empty;
    solver.solved_uninhabited_scratch = .{};
    solver.solved_entry_marks = .empty;
    solver.mono_uninhabited_scratch = .{};
    solver.mono_entry_stops = .empty;
    defer {
        for (solver.spare_clone_stacks.items) |*stack| stack.deinit(allocator);
        solver.spare_clone_stacks.deinit(allocator);
        for (solver.spare_clone_lists.items) |*lists| lists.deinit(allocator);
        solver.spare_clone_lists.deinit(allocator);
        solver.solved_uninhabited_scratch.deinit(allocator);
        solver.solved_entry_marks.deinit(allocator);
        solver.mono_uninhabited_scratch.deinit(allocator);
        solver.mono_entry_stops.deinit(allocator);
    }
    solver.uninhabited_path = .{};
    defer solver.uninhabited_path.deinit(allocator);
    solver.mono_uninhabited_path = .{};
    defer solver.mono_uninhabited_path.deinit(allocator);
    solver.program = &program;
    solver.active_unifications = UnifyPairSet.init(allocator);
    defer solver.active_unifications.deinit();
    solver.lift_count = 0;
    solver.preserving_lifted_roots = false;
    solver.unify_stack = .empty;
    defer solver.unify_stack.deinit(allocator);
    solver.solved_set_pool = collections.DenseMapPool(Type.TypeVarId, void).init(allocator);
    defer solver.solved_set_pool.deinit();
    solver.mono_set_pool = collections.DenseMapPool(MonoType.TypeId, void).init(allocator);
    defer solver.mono_set_pool.deinit();

    try solver.unify(structural, outer_named);

    try std.testing.expectEqual(vars_before_unify + 1, program.types.vars.items.len);
    try std.testing.expectEqual(program.types.root(outer_named), program.types.root(structural));
    try std.testing.expectEqual(Type.Content{ .primitive = .u64 }, try solver.shapeContent(structural));
}

test "inspectable backing unification never redirects an owned backing to its nominal type" {
    const allocator = std.testing.allocator;

    var program: Ast.Program = undefined;
    program.types = Type.Store.init(allocator);
    defer program.types.deinit();

    const backing = try program.types.add(.{ .primitive = .u64 });
    const named = try program.types.add(.{ .named = .{
        .named_type = undefined,
        .def = undefined,
        .kind = .nominal,
        .args = .empty(),
        .backing = .{ .ty = backing, .use = .inspectable },
    } });
    const backing_representative = try program.types.add(.{ .primitive = .u64 });

    var solver: Solver = undefined;
    solver.allocator = allocator;
    solver.mono_uninhabited = collections.DenseMap(MonoType.TypeId, bool).init(allocator);
    defer solver.mono_uninhabited.deinit();
    solver.uninhabited_memo = collections.DenseMap(Type.TypeVarId, UninhabitedAnswer).init(allocator);
    defer solver.uninhabited_memo.deinit();
    solver.mono_uninhabited_path_stops = 0;
    solver.spare_clone_stacks = .empty;
    solver.spare_clone_lists = .empty;
    solver.solved_uninhabited_scratch = .{};
    solver.solved_entry_marks = .empty;
    solver.mono_uninhabited_scratch = .{};
    solver.mono_entry_stops = .empty;
    defer {
        for (solver.spare_clone_stacks.items) |*stack| stack.deinit(allocator);
        solver.spare_clone_stacks.deinit(allocator);
        for (solver.spare_clone_lists.items) |*lists| lists.deinit(allocator);
        solver.spare_clone_lists.deinit(allocator);
        solver.solved_uninhabited_scratch.deinit(allocator);
        solver.solved_entry_marks.deinit(allocator);
        solver.mono_uninhabited_scratch.deinit(allocator);
        solver.mono_entry_stops.deinit(allocator);
    }
    solver.uninhabited_path = .{};
    defer solver.uninhabited_path.deinit(allocator);
    solver.mono_uninhabited_path = .{};
    defer solver.mono_uninhabited_path.deinit(allocator);
    solver.program = &program;
    solver.lifted = undefined;
    solver.active_unifications = UnifyPairSet.init(allocator);
    defer solver.active_unifications.deinit();
    solver.lift_count = 0;
    solver.preserving_lifted_roots = false;
    solver.unify_stack = .empty;
    defer solver.unify_stack.deinit(allocator);
    program.types.markNamedBacking(backing);
    program.types.set(backing, .{ .link = backing_representative });
    solver.solved_set_pool = collections.DenseMapPool(Type.TypeVarId, void).init(allocator);
    defer solver.solved_set_pool.deinit();
    solver.mono_set_pool = collections.DenseMapPool(MonoType.TypeId, void).init(allocator);
    defer solver.mono_set_pool.deinit();

    try solver.unify(backing, named);

    try std.testing.expectEqual(backing_representative, program.types.root(backing));
    try std.testing.expect(program.types.root(backing) != program.types.root(named));
    try std.testing.expect(program.types.isOwnedNamedBacking(backing));
    try std.testing.expectEqual(Type.Content{ .primitive = .u64 }, program.types.rootContent(backing));
}

test "generated-private evidence traverses a public inspectable named backing" {
    const allocator = std.testing.allocator;
    var lifted = emptyLiftedProgramForTest(allocator);
    var program = Ast.Program.init(allocator, lifted);
    lifted = undefined;
    defer program.deinit();

    const field_name = try program.lifted.names.internRecordFieldLabel("step");
    const ret_ty = try program.types.add(.zst);
    const public_callable = try program.types.add(.unbound);
    const private_callable = try program.types.add(.{ .lambda_set = try program.types.addMembers(&.{.{
        .lambda = @enumFromInt(1),
        .captures = .empty(),
    }}) });
    const public_fn = try program.types.add(.{ .func = .{
        .args = .empty(),
        .callable = public_callable,
        .ret = ret_ty,
    } });
    const private_fn = try program.types.add(.{ .func = .{
        .args = .empty(),
        .callable = private_callable,
        .ret = ret_ty,
    } });
    const public_backing = try program.types.add(.{ .record = try program.types.addFields(&.{.{
        .name = field_name,
        .ty = public_fn,
        .default = null,
    }}) });
    const private_record = try program.types.add(.{ .record = try program.types.addFields(&.{.{
        .name = field_name,
        .ty = private_fn,
        .default = null,
    }}) });
    const public_named = try program.types.add(.{ .named = .{
        .named_type = undefined,
        .def = undefined,
        .kind = .nominal,
        .args = .empty(),
        .backing = .{ .ty = public_backing, .use = .inspectable },
    } });

    var solver = try Solver.init(allocator, &program);
    defer solver.deinit();
    try solver.relateGeneratedPrivateEvidence(public_named, private_record);

    try std.testing.expectEqual(
        program.types.rootCompressed(public_callable),
        program.types.rootCompressed(private_callable),
    );
    try std.testing.expect(program.types.rootCompressed(public_named) != program.types.rootCompressed(private_record));
    try std.testing.expect(program.types.rootCompressed(public_backing) != program.types.rootCompressed(private_record));
}

test "root-slot read unification shares callables without joining a lifted pair" {
    const allocator = std.testing.allocator;
    var lifted = emptyLiftedProgramForTest(allocator);
    var program = Ast.Program.init(allocator, lifted);
    lifted = undefined;
    defer program.deinit();

    const field_name = try program.lifted.names.internRecordFieldLabel("step");
    const ret_ty = try program.types.add(.zst);
    const read_callable = try program.types.add(.unbound);
    const root_callable = try program.types.add(.{ .lambda_set = try program.types.addMembers(&.{.{
        .lambda = @enumFromInt(1),
        .captures = .empty(),
    }}) });
    const read_fn = try program.types.add(.{ .func = .{
        .args = .empty(),
        .callable = read_callable,
        .ret = ret_ty,
    } });
    const root_fn = try program.types.add(.{ .func = .{
        .args = .empty(),
        .callable = root_callable,
        .ret = ret_ty,
    } });
    const read_backing = try program.types.add(.{ .record = try program.types.addFields(&.{.{
        .name = field_name,
        .ty = read_fn,
        .default = null,
    }}) });
    const root_record = try program.types.add(.{ .record = try program.types.addFields(&.{.{
        .name = field_name,
        .ty = root_fn,
        .default = null,
    }}) });
    const read_named = try program.types.add(.{ .named = .{
        .named_type = undefined,
        .def = undefined,
        .kind = .nominal,
        .args = .empty(),
        .backing = .{ .ty = read_backing, .use = .inspectable },
    } });
    const read_list = try program.types.add(.{ .list = read_named });
    const root_list = try program.types.add(.{ .list = root_record });

    var solver = try Solver.init(allocator, &program);
    defer solver.deinit();
    solver.preserving_lifted_roots = true;
    try solver.unify(read_list, root_list);
    solver.preserving_lifted_roots = false;

    // The function beneath the lift is one type with one callable slot.
    try std.testing.expectEqual(program.types.rootCompressed(read_fn), program.types.rootCompressed(root_fn));
    try std.testing.expectEqual(program.types.rootCompressed(read_callable), program.types.rootCompressed(root_callable));
    // The lifted pair and the list containing it keep their own types.
    try std.testing.expect(program.types.rootCompressed(read_named) != program.types.rootCompressed(root_record));
    try std.testing.expect(program.types.rootCompressed(read_backing) != program.types.rootCompressed(root_record));
    try std.testing.expect(program.types.rootCompressed(read_list) != program.types.rootCompressed(root_list));
    try std.testing.expect(program.types.rootContent(root_record) == .record);
    try std.testing.expectEqual(program.types.rootCompressed(root_record), program.types.rootContent(root_list).list);

    // Ordinary unification of the same pair joins the record into the nominal.
    try solver.unify(read_named, root_record);
    try std.testing.expectEqual(program.types.rootCompressed(read_named), program.types.rootCompressed(root_record));
}

test "lambda solved solve declarations are referenced" {
    std.testing.refAllDecls(@This());
}

test "lambda solved list map primitives preserve callable element flow" {
    const allocator = std.testing.allocator;
    var program = Ast.Program.init(allocator, emptyLiftedProgramForTest(allocator));
    defer program.deinit();
    var solver = try Solver.init(allocator, &program);
    defer solver.deinit();

    const scalar = try program.types.add(.{ .primitive = .u64 });
    var callables: [5]Type.TypeVarId = undefined;
    var elements: [5]Type.TypeVarId = undefined;
    var lists: [5]Type.TypeVarId = undefined;
    for (&callables, &elements, &lists) |*callable, *element, *list| {
        callable.* = try program.types.add(.unbound);
        element.* = try program.types.add(.{ .func = .{
            .args = .empty(),
            .callable = callable.*,
            .ret = scalar,
        } });
        list.* = try program.types.add(.{ .list = element.* });
    }
    const transform = try program.types.add(.{ .func = .{
        .args = try program.types.addSpan(&.{elements[1]}),
        .callable = try program.types.add(.unbound),
        .ret = elements[3],
    } });
    const reuse_result = try program.types.add(.{ .primitive = .u8 });
    try solver.bindLowLevelTypes(.list_map_can_reuse, reuse_result, &.{ lists[0], transform });
    try std.testing.expectEqual(program.types.rootCompressed(callables[0]), program.types.rootCompressed(callables[1]));

    // A write preserves the buffer's element representation, including the
    // lambda set, both in the stored value and in the returned list handle.
    try solver.bindLowLevelTypes(.list_map_write_unsafe, lists[4], &.{ lists[2], scalar, elements[3] });
    try std.testing.expectEqual(program.types.rootCompressed(callables[2]), program.types.rootCompressed(callables[3]));
    try std.testing.expectEqual(program.types.rootCompressed(callables[2]), program.types.rootCompressed(callables[4]));
    // Reuse does not make the input and output callable sets interchangeable.
    try std.testing.expect(program.types.rootCompressed(callables[0]) != program.types.rootCompressed(callables[2]));
}

fn emptyLiftedProgramForTest(allocator: Allocator) Lifted.Program {
    return Lifted.Program.init(
        allocator,
        names.NameStore.init(allocator),
        MonoType.Store.init(allocator),
        .empty, // const_fn_evidence
        .empty, // const_fn_evidence_frames
        .empty, // exprs
        .empty, // pats
        .empty, // stmts
        .empty, // locals
        .empty, // expr_ids
        .empty, // pat_ids
        .empty, // typed_locals
        .empty, // stmt_ids
        .empty, // field_exprs
        .empty, // field_access_segments
        .empty, // fn_def_captures
        .empty, // capture_operands
        .empty, // record_destructs
        .empty, // str_pattern_steps
        .empty, // branches
        .empty, // if_branches
        .empty, // string_literals
        Lifted.ProcDebugNameMap.init(allocator),
        .empty, // source_files
        .empty, // expr_locs
        .empty, // expr_regions
        .empty, // stmt_locs
        .empty, // stmt_regions
        .empty, // inline_scopes
        .empty, // expr_inline_scopes
        .empty, // stmt_inline_scopes
        .empty, // local_names
        .empty, // static_data_values
        .empty, // comptime_sites
        0, // next_symbol
    );
}

test "lambda solved traverses deep sequential and nested expressions without native recursion" {
    const allocator = std.testing.allocator;
    var lifted = emptyLiftedProgramForTest(allocator);
    var lifted_owned = true;
    errdefer if (lifted_owned) lifted.deinit();
    const unit_ty = try lifted.types.add(.zst);
    const unit = try lifted.addExpr(.{ .ty = unit_ty, .data = .unit });
    var body = unit;
    for (0..50_000) |_| {
        const local = try lifted.addLocal(@enumFromInt(@as(u32, @intCast(lifted.localsView().len))), unit_ty);
        const bind = try lifted.addPat(.{ .ty = unit_ty, .data = .{ .bind = local } });
        body = try lifted.addExpr(.{ .ty = unit_ty, .data = .{ .typed_boundary = .{ .value = body } } });
        body = try lifted.addExpr(.{ .ty = unit_ty, .data = .{ .let_ = .{ .bind = bind, .value = unit, .rest = body } } });
    }
    _ = try lifted.addFn(.{ .symbol = @enumFromInt(50_001), .args = .empty(), .captures = .empty(), .body = .{ .roc = body }, .ret = unit_ty });
    var program = Ast.Program.init(allocator, lifted);
    lifted_owned = false;
    lifted = undefined;
    defer program.deinit();
    var solver = try Solver.init(allocator, &program);
    defer solver.deinit();
    try solver.solve();
    for (solver.expr_done) |done| try std.testing.expect(done);
    try std.testing.expectEqual(solver.inferredExpr(unit), solver.inferredExpr(body));
}

test "lambda solved compact record updates relate only unchanged field representations" {
    const allocator = std.testing.allocator;
    var lifted = emptyLiftedProgramForTest(allocator);
    var lifted_owned = true;
    errdefer if (lifted_owned) lifted.deinit();
    const u8_ty = try lifted.types.add(.{ .primitive = .u8 });
    const u16_ty = try lifted.types.add(.{ .primitive = .u16 });
    const a = try lifted.names.internRecordFieldLabel("a");
    const b = try lifted.names.internRecordFieldLabel("b");
    const base_ty = try lifted.types.add(.{ .record = try lifted.types.addRecordFields(&lifted.names, &.{
        .{ .name = a, .ty = u8_ty, .default = null },
        .{ .name = b, .ty = u8_ty, .default = null },
    }) });
    const result_ty = try lifted.types.add(.{ .record = try lifted.types.addRecordFields(&lifted.names, &.{
        .{ .name = a, .ty = u8_ty, .default = null },
        .{ .name = b, .ty = u16_ty, .default = null },
    }) });
    const local = try lifted.addLocal(@enumFromInt(@as(u32, @intCast(lifted.localsView().len))), base_ty);
    const base = try lifted.addExpr(.{ .ty = base_ty, .data = .{ .local = local } });
    const value = try lifted.addExpr(.{ .ty = u16_ty, .data = .{ .int_lit = .{ .bytes = @bitCast(@as(i128, 256)), .kind = .i128 } } });
    const update = try lifted.addExpr(.{ .ty = result_ty, .data = .{ .record_update = .{
        .base = base,
        .fields = try lifted.addFieldExprSpan(&.{.{ .name = b, .value = value }}),
    } } });
    _ = try lifted.addFn(.{ .symbol = @enumFromInt(1), .args = try lifted.addTypedLocalSpan(&.{.{ .local = local, .ty = base_ty }}), .captures = .empty(), .body = .{ .roc = update }, .ret = result_ty });
    var program = Ast.Program.init(allocator, lifted);
    lifted_owned = false;
    lifted = undefined;
    defer program.deinit();
    var solver = try Solver.init(allocator, &program);
    defer solver.deinit();
    try solver.solve();
    const solved_base = solver.inferredExpr(base);
    const solved_update = solver.inferredExpr(update);
    try std.testing.expectEqual(try solver.recordField(solved_base, a), try solver.recordField(solved_update, a));
    try std.testing.expect((try solver.recordField(solved_base, b)) != (try solver.recordField(solved_update, b)));
}

test "lambda solved return boundaries preserve rows and propagate callable payloads" {
    const allocator = std.testing.allocator;
    var lifted = emptyLiftedProgramForTest(allocator);
    const ok_name = try lifted.names.internTagLabel("Ok");
    const err_name = try lifted.names.internTagLabel("Err");
    var program = Ast.Program.init(allocator, lifted);
    defer program.deinit();
    var solver = try Solver.init(allocator, &program);
    defer solver.deinit();

    const empty = try program.types.add(.{ .tag_union = .empty() });
    const str = try program.types.add(.{ .primitive = .str });
    const u64_ty = try program.types.add(.{ .primitive = .u64 });
    const source_callable = try program.types.add(.{ .lambda_set = .empty() });
    const source_fn = try program.types.add(.{ .func = .{
        .args = .empty(),
        .callable = source_callable,
        .ret = str,
    } });
    const source = try program.types.add(.{ .tag_union = try program.types.addTags(&.{
        .{ .name = ok_name, .checked_name = ok_name, .payloads = try program.types.addSpan(&.{source_fn}) },
        .{ .name = err_name, .checked_name = err_name, .payloads = try program.types.addSpan(&.{empty}) },
    }) });
    // Two destinations disagree even on the base type of Err's payload.
    // Neither destination may pollute the shared empty source error row.
    for ([_]Type.TypeVarId{ str, u64_ty }) |payload| {
        const err_row = try program.types.add(.{ .tag_union = try program.types.addTags(&.{
            .{ .name = err_name, .checked_name = err_name, .payloads = try program.types.addSpan(&.{payload}) },
        }) });
        const target_callable = try program.types.add(.unbound);
        const target_fn = try program.types.add(.{ .func = .{
            .args = .empty(),
            .callable = target_callable,
            .ret = str,
        } });
        const target = try program.types.add(.{ .tag_union = try program.types.addTags(&.{
            .{ .name = ok_name, .checked_name = ok_name, .payloads = try program.types.addSpan(&.{target_fn}) },
            .{ .name = err_name, .checked_name = err_name, .payloads = try program.types.addSpan(&.{err_row}) },
        }) });
        try solver.relateReturn(source, target);
        try std.testing.expectEqual(program.types.rootCompressed(source_callable), program.types.rootCompressed(target_callable));
        try std.testing.expect(program.types.rootCompressed(source) != program.types.rootCompressed(target));
        try std.testing.expectEqual(@as(usize, 0), (try solver.shapeContent(empty)).tag_union.count());
        try std.testing.expectEqual(@as(usize, 1), (try solver.shapeContent(err_row)).tag_union.count());
    }

    try solver.relateReturn(empty, str);
    try solver.relateReturn(empty, u64_ty);
    try std.testing.expectEqual(@as(usize, 0), (try solver.shapeContent(empty)).tag_union.count());

    // A recursive payload follows the same directed pair only once.
    const source_cycle = try program.types.add(.unbound);
    const target_cycle = try program.types.add(.unbound);
    program.types.set(source_cycle, .{ .tag_union = try program.types.addTags(&.{
        .{ .name = ok_name, .checked_name = ok_name, .payloads = try program.types.addSpan(&.{source_cycle}) },
    }) });
    program.types.set(target_cycle, .{ .tag_union = try program.types.addTags(&.{
        .{ .name = ok_name, .checked_name = ok_name, .payloads = try program.types.addSpan(&.{target_cycle}) },
        .{ .name = err_name, .checked_name = err_name, .payloads = .empty() },
    }) });
    try solver.relateReturn(source_cycle, target_cycle);
    try std.testing.expectEqual(@as(usize, 1), (try solver.shapeContent(source_cycle)).tag_union.count());
    try std.testing.expectEqual(@as(usize, 2), (try solver.shapeContent(target_cycle)).tag_union.count());
}
