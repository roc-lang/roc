//! Splits struct-typed join parameters into one parameter per field.
//!
//! Match-arm and loop lowering shuttle multi-value state between joins inside
//! by-value wrapper structs: each jump builds a fresh struct, the join body
//! immediately reads the fields back out, and nothing else ever touches the
//! whole value. The wrapper costs a build, per-field reads, and—once ARC
//! runs—a retain on each refcounted field read paired with a release of the
//! wrapper, because the field read's lender dies at the jump.
//!
//! This pass runs after direct LIR lowering and before ARC insertion. For a
//! join parameter that is only ever read field-by-field and whose entries can
//! explicitly supply every field, it replaces the parameter with one parameter
//! per field. Each entry snapshots all replacement fields before writing any
//! parameter, so an old parameter that lends one replacement is not released
//! before the remaining replacements have been materialized. Single-use
//! literal builds are deleted or replaced, and field reads become local
//! aliases. Refcounted state then flows through pure alias chains that borrow
//! inference turns into moves, and the wrapper disappears entirely.
//! Descriptor-bearing wrappers remain aggregate because their field values are
//! coupled to projections of the root descriptor; scalarizing them requires
//! explicit per-field descriptor parameters as well as value parameters.
//!
//! A join's remainder (its run-once entry path) may enter the join without an
//! `initialize_join_param` write for a parameter—a plain tail-call
//! elimination loop header does this, entering with a bare jump because the
//! parameters are the proc's own argument locals. Such a parameter is still
//! scalarizable: its per-field parameters are seeded once on the remainder by
//! reading the incoming struct's fields. That read is sound only because the
//! sole write-less entry shape supplies the value through a proc argument, an
//! invariant the pass enforces.
//!
//! The pass iterates to a fixpoint so nested wrappers dissolve layer by
//! layer. Parameters with any whole-value use or a shared initializer keep
//! their shape.

const std = @import("std");
const collections = @import("collections");
const core = @import("lir_core");
const layout_mod = @import("layout");

const LIR = core.LIR;
const LirStore = core.LirStore;
const GuardedList = collections.GuardedList;
const Allocator = std.mem.Allocator;

pub const ScalarizeError = std.mem.Allocator.Error;

/// Maximum scalarized field count per parameter; wider wrappers keep their
/// shape. This must comfortably exceed the loop-carried state of large
/// hand-written loops: a loop whose state struct stays unscalarized pays a
/// refcount round-trip per iteration on every refcounted field, and the
/// materialized struct holds a second reference to each of them across the
/// body's calls, turning every in-place list update into a copy.
const max_fields = 64;

/// Backstop on scalarizations per proc; each round makes real progress (one
/// parameter rewritten), so this only exists to bound a pass bug. It must
/// comfortably exceed the number of loops a large hand-written proc can
/// have, since every loop contributes one struct-typed join parameter.
const max_rounds = 4096;

/// Scalarizes eligible struct-typed join parameters across every proc in the
/// store, repeating until no parameter qualifies.
pub fn run(store: *LirStore, layouts: *const layout_mod.Store) ScalarizeError!void {
    var pass = Pass{
        .store = store,
        .layouts = layouts,
        .allocator = store.allocator,
        .use_other = collections.DenseMap(LIR.LocalId, void).init(store.allocator),
        .field_reads = collections.DenseMap(LIR.LocalId, std.ArrayList(LIR.CFStmtId)).init(store.allocator),
        .init_writes = collections.DenseMap(LIR.LocalId, std.ArrayList(LIR.CFStmtId)).init(store.allocator),
        .write_other = collections.DenseMap(LIR.LocalId, void).init(store.allocator),
        .struct_builds = collections.DenseMap(LIR.LocalId, StructBuild).init(store.allocator),
        .alias_init_writes = collections.DenseMap(LIR.LocalId, std.ArrayList(LIR.CFStmtId)).init(store.allocator),
        .alias_defs = collections.DenseMap(LIR.LocalId, AliasDef).init(store.allocator),
        .join_params = collections.DenseMap(LIR.LocalId, std.ArrayList(LIR.CFStmtId)).init(store.allocator),
        .transparent = collections.DenseMap(LIR.LocalId, void).init(store.allocator),
        .removed = collections.DenseMap(LIR.CFStmtId, LIR.CFStmtId).init(store.allocator),
        .visited = collections.DenseMap(LIR.CFStmtId, void).init(store.allocator),
        .stack = .empty,
    };
    defer pass.deinit();

    // Procs are independent (join parameters are proc-local), so each proc
    // converges on its own before moving on; a proc that changes nothing is
    // scanned exactly once instead of once per global round.
    for (0..store.procSpecCount()) |proc_index| {
        var rounds: usize = 0;
        while (rounds < max_rounds) : (rounds += 1) {
            const proc = store.getProcSpec(@enumFromInt(@as(u32, @intCast(proc_index))));
            const body = proc.body orelse break;
            if (!try pass.scalarizeProc(@enumFromInt(@as(u32, @intCast(proc_index))), body)) break;
        }
    }
}

const StructBuild = struct {
    /// Every struct-literal statement defining the local. A wrapper
    /// temporary qualifies with exactly one; a parameter built directly may
    /// have one per jump.
    builds: std.ArrayList(BuildSite),
    /// Uses other than being an `initialize_join_param` value.
    uses: u32,
    /// Uses as an `initialize_join_param` value; a wrapper temporary
    /// qualifies with exactly one.
    init_uses: u32,
};

const BuildSite = struct {
    stmt: LIR.CFStmtId,
    fields: LIR.LocalSpan,
};

const AliasDef = struct {
    stmt: LIR.CFStmtId,
    source: LIR.LocalId,
    /// A local defined by more than one alias statement is never transparent.
    def_count: u32,
};

const Pass = struct {
    store: *LirStore,
    layouts: *const layout_mod.Store,
    allocator: Allocator,
    /// Locals with any use other than a field read.
    use_other: collections.DenseMap(LIR.LocalId, void),
    /// Field-read statements per source local.
    field_reads: collections.DenseMap(LIR.LocalId, std.ArrayList(LIR.CFStmtId)),
    /// `initialize_join_param` writes per target local.
    init_writes: collections.DenseMap(LIR.LocalId, std.ArrayList(LIR.CFStmtId)),
    /// Locals with any write other than an `initialize_join_param`.
    write_other: collections.DenseMap(LIR.LocalId, void),
    /// Struct-literal defs per target local.
    struct_builds: collections.DenseMap(LIR.LocalId, StructBuild),
    /// Direct `ref.local` assignments into a join parameter, per parameter.
    /// Lowering initializes a join parameter on some edges this way instead
    /// of with `set_local`; such a statement is an initializer that can seed
    /// per-field writes, exactly like a non-literal `set_local` initializer.
    alias_init_writes: collections.DenseMap(LIR.LocalId, std.ArrayList(LIR.CFStmtId)),
    /// Pure `ref.local` defs per target local. Lowered user code reads
    /// aggregates through such aliases, so the pass looks through them:
    /// an alias whose uses are all field reads (or further such aliases) is
    /// transparent, and its reads count as reads of the alias's source.
    alias_defs: collections.DenseMap(LIR.LocalId, AliasDef),
    /// How many join-parameter slots list each local. A join parameter can be
    /// written by a plain assignment on a jump edge, so an alias-shaped
    /// definition of one is an edge initializer, never a transparent alias.
    /// Lowering also shares one local across the parameter spans of nested
    /// joins; scalarizing such a local for one join would steal the shared
    /// initializers from the others, so only a local listed exactly once may
    /// scalarize.
    join_params: collections.DenseMap(LIR.LocalId, std.ArrayList(LIR.CFStmtId)),
    /// Aliases proved transparent this round.
    transparent: collections.DenseMap(LIR.LocalId, void),
    /// Deleted build statements mapped to their continuations, for edge
    /// patching.
    removed: collections.DenseMap(LIR.CFStmtId, LIR.CFStmtId),
    visited: collections.DenseMap(LIR.CFStmtId, void),
    stack: std.ArrayList(LIR.CFStmtId),
    /// Field-parameter locals created this round; they join the proc's
    /// frame locals so frame plans cover them.
    new_locals: std.ArrayList(LIR.LocalId) = .empty,

    fn deinit(self: *Pass) void {
        self.use_other.deinit();
        self.clearLists();
        self.field_reads.deinit();
        self.init_writes.deinit();
        self.write_other.deinit();
        self.struct_builds.deinit();
        self.alias_init_writes.deinit();
        self.alias_defs.deinit();
        self.join_params.deinit();
        self.transparent.deinit();
        self.removed.deinit();
        self.visited.deinit();
        self.stack.deinit(self.allocator);
        self.new_locals.deinit(self.allocator);
    }

    fn clearLists(self: *Pass) void {
        var reads = self.field_reads.valueIterator();
        while (reads.next()) |list| list.deinit(self.allocator);
        var writes = self.init_writes.valueIterator();
        while (writes.next()) |list| list.deinit(self.allocator);
        var alias_writes = self.alias_init_writes.valueIterator();
        while (alias_writes.next()) |list| list.deinit(self.allocator);
        var listing_joins = self.join_params.valueIterator();
        while (listing_joins.next()) |list| list.deinit(self.allocator);
        var builds = self.struct_builds.valueIterator();
        while (builds.next()) |build| build.builds.deinit(self.allocator);
    }

    fn resetProc(self: *Pass) void {
        self.use_other.clearRetainingCapacity();
        self.clearLists();
        self.field_reads.clearRetainingCapacity();
        self.init_writes.clearRetainingCapacity();
        self.write_other.clearRetainingCapacity();
        self.struct_builds.clearRetainingCapacity();
        self.alias_init_writes.clearRetainingCapacity();
        self.alias_defs.clearRetainingCapacity();
        self.join_params.clearRetainingCapacity();
        self.transparent.clearRetainingCapacity();
        self.removed.clearRetainingCapacity();
        self.visited.clearRetainingCapacity();
        self.stack.clearRetainingCapacity();
        self.new_locals.clearRetainingCapacity();
    }

    fn noteUse(self: *Pass, local: LIR.LocalId) ScalarizeError!void {
        try self.use_other.put(local, {});
        if (self.struct_builds.getPtr(local)) |build| build.uses += 1;
    }

    fn noteDescUse(self: *Pass, desc: LIR.BoxyDescRef) ScalarizeError!void {
        if (desc.localOrNull()) |local| try self.noteUse(local);
    }

    fn noteDictUse(self: *Pass, dict: LIR.BoxyDictRef) ScalarizeError!void {
        if (dict.localOrNull()) |local| try self.noteUse(local);
    }

    fn noteFieldRead(self: *Pass, source: LIR.LocalId, stmt: LIR.CFStmtId) ScalarizeError!void {
        const entry = try self.field_reads.getOrPut(source);
        if (!entry.found_existing) entry.value_ptr.* = .empty;
        try entry.value_ptr.append(self.allocator, stmt);
        if (self.struct_builds.getPtr(source)) |build| build.uses += 1;
    }

    fn noteWrite(self: *Pass, target: LIR.LocalId) ScalarizeError!void {
        try self.write_other.put(target, {});
    }

    fn noteStructBuild(self: *Pass, target: LIR.LocalId, stmt: LIR.CFStmtId, fields: LIR.LocalSpan) ScalarizeError!void {
        const entry = try self.struct_builds.getOrPut(target);
        if (!entry.found_existing) {
            entry.value_ptr.* = .{ .builds = .empty, .uses = 0, .init_uses = 0 };
        }
        try entry.value_ptr.builds.append(self.allocator, .{ .stmt = stmt, .fields = fields });
    }

    /// Decide which recorded aliases are transparent: a single-definition
    /// alias, not a join parameter, with no other writes, whose every use is a
    /// field read or another transparent alias. Everything else is an ordinary
    /// whole-value use of its source and is counted as one here, exactly as if
    /// collection had noted it directly. Demotion cascades: an opaque alias is
    /// a whole-value use of its source, which can demote the alias it was read
    /// through in turn.
    fn resolveAliases(self: *Pass) ScalarizeError!void {
        var it = self.alias_defs.iterator();
        while (it.next()) |entry| {
            const target = entry.key_ptr.*;
            const def = entry.value_ptr.*;
            if (def.def_count == 1 and
                !self.join_params.contains(target) and
                !self.write_other.contains(target) and
                self.init_writes.get(target) == null)
            {
                try self.transparent.put(target, {});
            }
        }

        var changed = true;
        while (changed) {
            changed = false;
            var candidates = self.alias_defs.iterator();
            while (candidates.next()) |entry| {
                const target = entry.key_ptr.*;
                if (!self.transparent.contains(target)) continue;
                if (self.use_other.contains(target)) {
                    _ = self.transparent.remove(target);
                    try self.noteUse(entry.value_ptr.source);
                    changed = true;
                }
            }
        }

        // A multiply-defined alias already counted its sources at collection.
        var settled = self.alias_defs.iterator();
        while (settled.next()) |entry| {
            const target = entry.key_ptr.*;
            const def = entry.value_ptr.*;
            if (self.transparent.contains(target)) {
                // Transparent reads still count toward initializer-build
                // qualification: a build read through an alias must not be
                // splatted away, or the alias's reads would dangle.
                if (self.struct_builds.getPtr(self.transparentRoot(def.source))) |build| {
                    build.uses += 1;
                }
            } else if (def.def_count == 1) {
                try self.noteUse(def.source);
            }
        }
    }

    /// Follow a transparent-alias chain to the local whose value it reads.
    fn transparentRoot(self: *const Pass, source: LIR.LocalId) LIR.LocalId {
        var root = source;
        var steps: usize = 0;
        while (self.transparent.contains(root)) {
            const def = self.alias_defs.get(root) orelse break;
            root = def.source;
            steps += 1;
            if (steps > self.alias_defs.count()) break;
        }
        return root;
    }

    /// The transparent aliases rooted at `param`, in dependency order, plus
    /// their field-read statements. Both are rewritten when the parameter
    /// scalarizes: the reads become aliases of the field parameters, and the
    /// alias definitions are deleted because their source disappears.
    const AliasClosure = struct {
        stmts: std.ArrayList(LIR.CFStmtId),
        reads: std.ArrayList(LIR.CFStmtId),

        fn deinit(closure: *AliasClosure, allocator: Allocator) void {
            closure.stmts.deinit(allocator);
            closure.reads.deinit(allocator);
        }
    };

    fn aliasClosureOf(self: *Pass, param: LIR.LocalId) ScalarizeError!AliasClosure {
        var closure = AliasClosure{ .stmts = .empty, .reads = .empty };
        errdefer closure.deinit(self.allocator);
        var it = self.alias_defs.iterator();
        while (it.next()) |entry| {
            const target = entry.key_ptr.*;
            if (!self.transparent.contains(target)) continue;
            if (self.transparentRoot(entry.value_ptr.source) != param) continue;
            try closure.stmts.append(self.allocator, entry.value_ptr.stmt);
            if (self.field_reads.getPtr(target)) |reads| {
                try closure.reads.appendSlice(self.allocator, reads.items);
            }
        }
        return closure;
    }

    fn scalarizeProc(self: *Pass, proc_id: LIR.LirProcSpecId, body: LIR.CFStmtId) ScalarizeError!bool {
        self.resetProc();
        try self.collect(body);
        try self.resolveAliases();

        // Resolve proc-argument membership here, where the argument span is
        // freshly valid, and pass a plain bool into `tryScalarize`. The span is
        // a view into the store's local-id buffer, which `tryScalarize`
        // reallocates when it scalarizes, so it must not be read across that
        // call.
        const proc_args = try GuardedList.dupe(self.allocator, LIR.LocalId, self.store.getLocalSpan(self.store.getProcSpec(proc_id).args));
        defer self.allocator.free(proc_args);

        // Find one scalarizable parameter; the fixpoint loop picks up the
        // rest on later rounds.
        var changed = false;

        self.visited.clearRetainingCapacity();
        self.stack.clearRetainingCapacity();
        try self.stack.append(self.allocator, body);
        outer: while (self.stack.pop()) |current| {
            if (self.visited.contains(current)) continue;
            try self.visited.put(current, {});
            switch (self.store.getCFStmt(current)) {
                .join => |join_stmt| {
                    const params = self.store.getLocalSpan(join_stmt.params);
                    for (0..params.len) |position| {
                        const param = GuardedList.at(params, position);
                        const param_is_proc_arg = std.mem.findScalar(LIR.LocalId, proc_args, param) != null;
                        if (try self.tryScalarize(param_is_proc_arg, current, param)) {
                            changed = true;
                            break :outer;
                        }
                    }
                    try self.stack.append(self.allocator, join_stmt.body);
                    try self.stack.append(self.allocator, join_stmt.remainder);
                },
                .switch_stmt => |s| {
                    const branches = self.store.getCFSwitchBranches(s.branches);
                    for (0..branches.len) |index| try self.stack.append(self.allocator, GuardedList.at(branches, index).body);
                    try self.stack.append(self.allocator, s.default_branch);
                    if (s.continuation) |continuation| {
                        try self.stack.append(self.allocator, continuation);
                    }
                },
                .switch_initialized_payload => |s| {
                    try self.stack.append(self.allocator, s.initialized_branch);
                    try self.stack.append(self.allocator, s.uninitialized_branch);
                },
                .str_match => |s| {
                    try self.stack.append(self.allocator, s.on_match);
                    try self.stack.append(self.allocator, s.on_miss);
                },
                .str_match_set => |s| {
                    const arms = self.store.getStrMatchArms(s.arms);
                    for (0..arms.len) |index| try self.stack.append(self.allocator, GuardedList.at(arms, index).on_match);
                    try self.stack.append(self.allocator, s.on_miss);
                },
                .boxy_tag_match => |s| {
                    try self.stack.append(self.allocator, s.on_match);
                    try self.stack.append(self.allocator, s.on_miss);
                },
                inline .assign_ref, .assign_literal, .init_uninitialized, .assign_call, .assign_call_erased, .assign_packed_erased_fn, .assign_boxy_desc_ref, .assign_boxy_dict_ref, .assign_boxy_box, .assign_boxy_reuse_box, .assign_boxy_unbox, .assign_boxy_adapt, .assign_boxy_inspect, .assign_boxy_eq, .assign_boxy_tag, .assign_boxy_tag_payload, .assign_call_dict, .assign_low_level, .assign_list, .assign_struct, .assign_tag, .store_struct, .store_tag, .set_local, .debug, .expect, .comptime_branch_taken, .incref, .decref, .decref_if_initialized, .free => |s| {
                    try self.stack.append(self.allocator, s.next);
                },
                .jump, .ret, .crash, .expect_err, .runtime_error, .comptime_exhaustiveness_failed, .loop_continue, .loop_break => {},
            }
        }

        if (changed) {
            try self.patchRemovedEdges(proc_id);
            try self.extendFrameLocals(proc_id);
        }
        return changed;
    }

    /// Adds the new field-parameter locals to the proc's frame locals so
    /// frame plans cover them.
    fn extendFrameLocals(self: *Pass, proc_id: LIR.LirProcSpecId) ScalarizeError!void {
        if (self.new_locals.items.len == 0) return;
        const proc = self.store.getProcSpecPtr(proc_id);
        var combined = std.ArrayList(LIR.LocalId).empty;
        defer combined.deinit(self.allocator);
        const frame_locals = self.store.getLocalSpan(proc.frame_locals);
        for (0..frame_locals.len) |index| try combined.append(self.allocator, GuardedList.at(frame_locals, index));
        try combined.appendSlice(self.allocator, self.new_locals.items);
        proc.frame_locals = try self.store.addLocalSpan(combined.items);
        if (self.store.procNeedsStackProbe(self.layouts, proc.*)) {
            proc.stack_probe = .required;
        }
    }

    /// `param_is_proc_arg` records whether this join parameter is also one of
    /// the proc's argument locals. Every jump that targets the join carries
    /// `initialize_join_param` writes for its parameters, which the rewrite
    /// below turns into per-field writes. The one entry that carries no such
    /// write is a plain-TCE loop header: it reuses the proc's own argument
    /// locals as the join parameters and enters with a bare jump, so the
    /// parameter's initial value arrives through the argument. That is the only
    /// shape in which a join parameter is also a proc argument, and it is
    /// exactly the shape whose field parameters must be seeded from the
    /// argument struct on entry. (Any future shape that entered a parameter
    /// without a write and without a seed would surface as an unbound local in
    /// the ARC borrow certifier, never as a silent miscompile.)
    fn tryScalarize(
        self: *Pass,
        param_is_proc_arg: bool,
        join_id: LIR.CFStmtId,
        param: LIR.LocalId,
    ) ScalarizeError!bool {
        const param_local = self.store.getLocal(param);
        const param_layout = self.layouts.getLayout(param_local.layout_idx);
        if (param_layout.tag != .struct_) return false;
        if (param_local.boxy_desc != null) return false;

        const info = self.layouts.getStructInfo(param_layout);
        var field_count: usize = 0;
        for (0..info.fields.len) |i| {
            const field = info.fields.get(@intCast(i));
            field_count = @max(field_count, @as(usize, field.index) + 1);
        }
        if (field_count == 0 or field_count > max_fields) return false;

        // The parameter must be touched only by field reads (directly or
        // through transparent aliases), direct struct-literal builds, and
        // `initialize_join_param` writes.
        if (self.use_other.contains(param)) return false;
        if (self.write_other.contains(param)) return false;
        // Every join listing this parameter is rewritten in the same round,
        // sharing one set of field parameters exactly as they shared the
        // struct parameter. The write-less proc-entry seed reads the argument
        // struct in a specific join's remainder, so that shape keeps the
        // single-join requirement.
        const listing_joins: []const LIR.CFStmtId = if (self.join_params.getPtr(param)) |list| list.items else &.{};
        if (listing_joins.len == 0) return false;
        if (param_is_proc_arg and listing_joins.len != 1) return false;
        // A conditionally initialized parameter's metadata names the whole
        // local; splitting it is not modeled.
        for (listing_joins) |listing_id| {
            const listing = self.store.getCFStmt(listing_id).join;
            const maybe_uninitialized = self.store.getLocalSpan(listing.maybe_uninitialized_params);
            for (0..GuardedList.borrowLen(maybe_uninitialized)) |i| {
                if (GuardedList.at(maybe_uninitialized, i) == param) return false;
            }
        }
        var closure = try self.aliasClosureOf(param);
        defer closure.deinit(self.allocator);
        const empty_reads: []const LIR.CFStmtId = &.{};
        const direct_reads: []const LIR.CFStmtId = if (self.field_reads.getPtr(param)) |list| list.items else empty_reads;
        if (direct_reads.len == 0 and closure.reads.items.len == 0) return false;
        const empty_writes: []const LIR.CFStmtId = &.{};
        const writes: []const LIR.CFStmtId = if (self.init_writes.getPtr(param)) |list| list.items else empty_writes;
        const alias_writes: []const LIR.CFStmtId = if (self.alias_init_writes.getPtr(param)) |list| list.items else empty_writes;
        const empty_builds: []const BuildSite = &.{};
        const direct_builds: []const BuildSite = if (self.struct_builds.getPtr(param)) |entry| entry.builds.items else empty_builds;
        if (writes.len == 0 and alias_writes.len == 0 and direct_builds.len == 0) return false;

        // A directly-built parameter's literals each become per-field
        // writes in place.
        for (direct_builds) |site| {
            if (self.store.getLocalSpan(site.fields).len != field_count) return false;
        }

        // A copied-in initializer that is a single-def, single-use struct
        // literal splats its operands onto the field parameters; any other
        // initializer value is seeded by reading its fields at the write.
        for (writes) |write_stmt| {
            const write = self.store.getCFStmt(write_stmt).set_local;
            if (write.value == param) return false;
            const build = self.struct_builds.get(write.value) orelse continue;
            if (build.builds.items.len != 1 or build.uses != 0 or build.init_uses != 1) continue;
            if (self.write_other.contains(write.value)) continue;
            if (self.join_params.contains(write.value)) continue;
            if (self.store.getLocalSpan(build.builds.items[0].fields).len != field_count) return false;
        }
        for (direct_reads) |read_stmt| {
            const read = self.store.getCFStmt(read_stmt).assign_ref;
            if (read.op.field.field_idx >= field_count) return false;
        }
        for (closure.reads.items) |read_stmt| {
            const read = self.store.getCFStmt(read_stmt).assign_ref;
            if (read.op.field.field_idx >= field_count) return false;
        }

        // Create the per-field parameter locals.
        var field_locals_buffer: [max_fields]LIR.LocalId = undefined;
        for (0..field_count) |k| {
            const field_layout = self.layouts.getStructFieldLayoutByOriginalIndex(
                param_layout.getStruct().idx,
                @intCast(k),
            );
            field_locals_buffer[k] = try self.store.addLocal(.{ .layout_idx = field_layout });
        }
        const field_locals = field_locals_buffer[0..field_count];
        try self.new_locals.appendSlice(self.allocator, field_locals);

        // Every listing join's parameter span gets the fields in the
        // parameter's place.
        for (listing_joins) |listing_id| {
            const old_params = self.store.getLocalSpan(self.store.getCFStmt(listing_id).join.params);
            var new_params = std.ArrayList(LIR.LocalId).empty;
            defer new_params.deinit(self.allocator);
            for (0..GuardedList.borrowLen(old_params)) |old_position| {
                const old_param = GuardedList.at(old_params, old_position);
                if (old_param == param) {
                    try new_params.appendSlice(self.allocator, field_locals);
                } else {
                    try new_params.append(self.allocator, old_param);
                }
            }
            const new_span = try self.store.addLocalSpan(new_params.items);
            self.store.getCFStmtPtr(listing_id).join.params = new_span;
        }

        // Field reads become aliases of the field parameters, whether they
        // read the parameter directly or through a transparent alias.
        for (direct_reads) |read_stmt| {
            const read_ptr = self.store.getCFStmtPtr(read_stmt);
            const field_idx = read_ptr.assign_ref.op.field.field_idx;
            read_ptr.assign_ref.op = .{ .local = field_locals[field_idx] };
        }
        for (closure.reads.items) |read_stmt| {
            const read_ptr = self.store.getCFStmtPtr(read_stmt);
            const field_idx = read_ptr.assign_ref.op.field.field_idx;
            read_ptr.assign_ref.op = .{ .local = field_locals[field_idx] };
        }

        // The transparent aliases' definitions read a value that no longer
        // exists; every use of them was rewritten above, so they are deleted.
        for (closure.stmts.items) |alias_stmt| {
            const alias = self.store.getCFStmt(alias_stmt);
            std.debug.assert(alias == .assign_ref);
            const alias_next = alias.assign_ref.next;
            try self.removed.put(alias_stmt, alias_next);
        }

        // Each jump-site write becomes one snapshotted write per field: a
        // qualifying struct literal supplies its operands and its build is
        // deleted; any other initializer is read field-by-field at the write.
        for (writes) |write_stmt| {
            const write = self.store.getCFStmt(write_stmt).set_local;
            const qualifying: ?StructBuild = qualifying: {
                const build = self.struct_builds.get(write.value) orelse break :qualifying null;
                if (build.builds.items.len != 1 or build.uses != 0 or build.init_uses != 1) break :qualifying null;
                if (self.write_other.contains(write.value)) break :qualifying null;
                // A join parameter's literal build is that join's edge
                // initialization, not a site-local wrapper temporary:
                // deleting it would leave the parameter uninitialized, and
                // its operands live at the other join's jump site, not
                // here. Seeding by field reads keeps the parameter intact
                // (and lets a later round scalarize it on its own).
                if (self.join_params.contains(write.value)) break :qualifying null;
                break :qualifying build;
            };
            if (qualifying) |build| {
                const site = build.builds.items[0];
                const operands = self.store.getLocalSpan(site.fields);
                try self.writeFields(write_stmt, write.next, field_locals, operands);

                const build_stmt = self.store.getCFStmt(site.stmt);
                std.debug.assert(build_stmt == .assign_struct);
                const build_next = build_stmt.assign_struct.next;
                try self.removed.put(site.stmt, build_next);
            } else {
                try self.seedWrite(write_stmt, write.value, write.next, field_locals);
            }
        }

        // Edge initializers seed the same way: the whole-value assignment
        // becomes per-field reads of its source.
        for (alias_writes) |write_stmt| {
            const write = self.store.getCFStmt(write_stmt).assign_ref;
            try self.seedWrite(write_stmt, write.op.local, write.next, field_locals);
        }

        // Each direct build becomes per-field writes in its place.
        for (direct_builds) |site| {
            const operands = self.store.getLocalSpan(site.fields);
            const build_stmt = self.store.getCFStmt(site.stmt);
            std.debug.assert(build_stmt == .assign_struct);
            const build_next = build_stmt.assign_struct.next;
            try self.writeFields(site.stmt, build_next, field_locals, operands);
        }

        // Seed the field parameters on the write-less proc-entry path (see the
        // comment at param_is_proc_arg above): reading the argument struct's
        // fields in the join's remainder, which runs once before the loop body.
        if (param_is_proc_arg) try self.seedFieldsFromArgStruct(join_id, param, field_locals);

        return true;
    }

    /// Prepend, to the join's remainder, one `ref.field arg[k]` read plus an
    /// `initialize_join_param` write into the corresponding field parameter,
    /// so the field parameters carry the argument struct's fields on the
    /// proc-entry path. The read temporaries join the proc's frame locals.
    fn seedFieldsFromArgStruct(
        self: *Pass,
        join_id: LIR.CFStmtId,
        arg_struct: LIR.LocalId,
        field_locals: []const LIR.LocalId,
    ) ScalarizeError!void {
        var next = self.store.getCFStmtPtr(join_id).join.remainder;
        var k: usize = field_locals.len;
        while (k > 0) {
            k -= 1;
            const tmp = try self.store.addLocal(.{ .layout_idx = self.store.getLocal(field_locals[k]).layout_idx });
            try self.new_locals.append(self.allocator, tmp);
            const set_stmt = try self.store.addCFStmt(.{ .set_local = .{
                .target = field_locals[k],
                .value = tmp,
                .mode = .initialize_join_param,
                .next = next,
            } });
            next = try self.store.addCFStmt(.{ .assign_ref = .{
                .target = tmp,
                .op = .{ .field = .{ .source = arg_struct, .field_idx = @intCast(k) } },
                .next = set_stmt,
            } });
        }
        self.store.getCFStmtPtr(join_id).join.remainder = next;
    }

    /// Replaces an `initialize_join_param` write whose value is not a
    /// splattable struct literal with per-field reads of that value: each
    /// field is read into a temporary and written to its field parameter.
    /// The value stays defined by its own statement and simply dies here,
    /// which is what lets comptime-static and call-produced initializers
    /// scalarize.
    fn seedWrite(
        self: *Pass,
        write_stmt: LIR.CFStmtId,
        value: LIR.LocalId,
        next_after: LIR.CFStmtId,
        field_locals: []const LIR.LocalId,
    ) ScalarizeError!void {
        var temps_buffer: [max_fields]LIR.LocalId = undefined;
        for (field_locals, 0..) |field_local, k| {
            const tmp = try self.store.addLocal(.{ .layout_idx = self.store.getLocal(field_local).layout_idx });
            try self.new_locals.append(self.allocator, tmp);
            temps_buffer[k] = tmp;
        }
        const temps = temps_buffer[0..field_locals.len];

        // Snapshot every field before replacing any parameter. A source can
        // borrow through one of the old parameter values, so interleaving a
        // read with its set could release that lender before a later field
        // read. The original whole-struct assignment materialized all fields
        // before the jump changed any parameter; preserve that ordering.
        var next = next_after;
        var k: usize = field_locals.len;
        while (k > 0) {
            k -= 1;
            next = try self.store.addCFStmt(.{ .set_local = .{
                .target = field_locals[k],
                .value = temps[k],
                .mode = .initialize_join_param,
                .next = next,
            } });
        }
        k = field_locals.len;
        while (k > 0) {
            k -= 1;
            if (k == 0) {
                self.store.getCFStmtPtr(write_stmt).* = .{ .assign_ref = .{
                    .target = temps[0],
                    .op = .{ .field = .{ .source = value, .field_idx = 0 } },
                    .next = next,
                } };
            } else {
                next = try self.store.addCFStmt(.{ .assign_ref = .{
                    .target = temps[k],
                    .op = .{ .field = .{ .source = value, .field_idx = @intCast(k) } },
                    .next = next,
                } });
            }
        }
    }

    /// Replaces `stmt` with a parallel parameter transfer: first snapshot all
    /// operands into fresh locals, then initialize the field parameters.
    /// Snapshotting preserves the whole-struct assignment's ordering when an
    /// operand borrows through an old parameter value that a set will replace.
    fn writeFields(
        self: *Pass,
        stmt: LIR.CFStmtId,
        next_after: LIR.CFStmtId,
        field_locals: []const LIR.LocalId,
        operands: anytype,
    ) ScalarizeError!void {
        var temps_buffer: [max_fields]LIR.LocalId = undefined;
        for (field_locals, 0..) |field_local, k| {
            const tmp = try self.store.addLocal(.{ .layout_idx = self.store.getLocal(field_local).layout_idx });
            try self.new_locals.append(self.allocator, tmp);
            temps_buffer[k] = tmp;
        }
        const temps = temps_buffer[0..field_locals.len];

        var next = next_after;
        var k: usize = field_locals.len;
        while (k > 0) {
            k -= 1;
            next = try self.store.addCFStmt(.{ .set_local = .{
                .target = field_locals[k],
                .value = temps[k],
                .mode = .initialize_join_param,
                .next = next,
            } });
        }
        k = field_locals.len;
        while (k > 0) {
            k -= 1;
            if (k == 0) {
                self.store.getCFStmtPtr(stmt).* = .{ .assign_ref = .{
                    .target = temps[0],
                    .op = .{ .local = GuardedList.at(operands, 0) },
                    .next = next,
                } };
            } else {
                next = try self.store.addCFStmt(.{ .assign_ref = .{
                    .target = temps[k],
                    .op = .{ .local = GuardedList.at(operands, k) },
                    .next = next,
                } });
            }
        }
    }

    /// Redirects every edge that targets a deleted build statement to that
    /// statement's continuation.
    fn patchRemovedEdges(self: *Pass, proc_id: LIR.LirProcSpecId) ScalarizeError!void {
        if (self.removed.count() == 0) return;
        const proc = self.store.getProcSpecPtr(proc_id);
        if (proc.body) |body| {
            proc.body = self.resolveRemoved(body);
        }

        self.visited.clearRetainingCapacity();
        self.stack.clearRetainingCapacity();
        try self.stack.append(self.allocator, proc.body.?);
        while (self.stack.pop()) |current| {
            if (self.visited.contains(current)) continue;
            try self.visited.put(current, {});
            const stmt = self.store.getCFStmtPtr(current);
            switch (stmt.*) {
                .switch_stmt => |*s| {
                    const branches = self.store.getCFSwitchBranchesMut(s.branches);
                    for (0..branches.len) |index| {
                        const branch = GuardedList.atPtr(branches, index);
                        branch.body = self.resolveRemoved(branch.body);
                        try self.stack.append(self.allocator, branch.body);
                    }
                    s.default_branch = self.resolveRemoved(s.default_branch);
                    try self.stack.append(self.allocator, s.default_branch);
                    if (s.continuation) |continuation| {
                        s.continuation = self.resolveRemoved(continuation);
                        try self.stack.append(self.allocator, s.continuation.?);
                    }
                },
                .switch_initialized_payload => |*s| {
                    s.initialized_branch = self.resolveRemoved(s.initialized_branch);
                    s.uninitialized_branch = self.resolveRemoved(s.uninitialized_branch);
                    try self.stack.append(self.allocator, s.initialized_branch);
                    try self.stack.append(self.allocator, s.uninitialized_branch);
                },
                .str_match => |*s| {
                    s.on_match = self.resolveRemoved(s.on_match);
                    s.on_miss = self.resolveRemoved(s.on_miss);
                    try self.stack.append(self.allocator, s.on_match);
                    try self.stack.append(self.allocator, s.on_miss);
                },
                .str_match_set => |*s| {
                    const arms = self.store.getStrMatchArms(s.arms);
                    const rewritten_arms = try self.allocator.alloc(LIR.StrMatchArm, arms.len);
                    defer self.allocator.free(rewritten_arms);
                    for (0..arms.len) |index| {
                        const arm = GuardedList.at(arms, index);
                        const rewritten = &rewritten_arms[index];
                        rewritten.* = arm;
                        rewritten.on_match = self.resolveRemoved(arm.on_match);
                        try self.stack.append(self.allocator, rewritten.on_match);
                    }
                    s.arms = try self.store.addStrMatchArms(rewritten_arms);
                    s.on_miss = self.resolveRemoved(s.on_miss);
                    try self.stack.append(self.allocator, s.on_miss);
                },
                .boxy_tag_match => |*s| {
                    s.on_match = self.resolveRemoved(s.on_match);
                    s.on_miss = self.resolveRemoved(s.on_miss);
                    try self.stack.append(self.allocator, s.on_match);
                    try self.stack.append(self.allocator, s.on_miss);
                },
                .join => |*j| {
                    j.body = self.resolveRemoved(j.body);
                    j.remainder = self.resolveRemoved(j.remainder);
                    try self.stack.append(self.allocator, j.body);
                    try self.stack.append(self.allocator, j.remainder);
                },
                inline .assign_ref, .assign_literal, .init_uninitialized, .assign_call, .assign_call_erased, .assign_packed_erased_fn, .assign_boxy_desc_ref, .assign_boxy_dict_ref, .assign_boxy_box, .assign_boxy_reuse_box, .assign_boxy_unbox, .assign_boxy_adapt, .assign_boxy_inspect, .assign_boxy_eq, .assign_boxy_tag, .assign_boxy_tag_payload, .assign_call_dict, .assign_low_level, .assign_list, .assign_struct, .assign_tag, .store_struct, .store_tag, .set_local, .debug, .expect, .comptime_branch_taken, .incref, .decref, .decref_if_initialized, .free => |*s| {
                    s.next = self.resolveRemoved(s.next);
                    try self.stack.append(self.allocator, s.next);
                },
                .jump, .ret, .crash, .expect_err, .runtime_error, .comptime_exhaustiveness_failed, .loop_continue, .loop_break => {},
            }
        }
    }

    fn resolveRemoved(self: *const Pass, stmt: LIR.CFStmtId) LIR.CFStmtId {
        var cursor = stmt;
        var steps: usize = 0;
        while (self.removed.get(cursor)) |next| {
            cursor = next;
            steps += 1;
            if (steps > self.removed.count()) break;
        }
        return cursor;
    }

    fn collect(self: *Pass, body: LIR.CFStmtId) ScalarizeError!void {
        // First pass: struct-literal defs, so the use pass can attribute
        // uses to builds regardless of traversal order.
        self.visited.clearRetainingCapacity();
        self.stack.clearRetainingCapacity();
        try self.stack.append(self.allocator, body);
        while (self.stack.pop()) |current| {
            if (self.visited.contains(current)) continue;
            try self.visited.put(current, {});
            switch (self.store.getCFStmt(current)) {
                .assign_struct => |assign| {
                    try self.noteStructBuild(assign.target, current, assign.fields);
                    try self.stack.append(self.allocator, assign.next);
                },
                .switch_stmt => |s| {
                    const branches = self.store.getCFSwitchBranches(s.branches);
                    for (0..branches.len) |index| try self.stack.append(self.allocator, GuardedList.at(branches, index).body);
                    try self.stack.append(self.allocator, s.default_branch);
                    if (s.continuation) |continuation| {
                        try self.stack.append(self.allocator, continuation);
                    }
                },
                .join => |join_stmt| {
                    const params = self.store.getLocalSpan(join_stmt.params);
                    for (0..GuardedList.borrowLen(params)) |index| {
                        const entry = try self.join_params.getOrPut(GuardedList.at(params, index));
                        if (!entry.found_existing) entry.value_ptr.* = .empty;
                        try entry.value_ptr.append(self.allocator, current);
                    }
                    try self.stack.append(self.allocator, join_stmt.body);
                    try self.stack.append(self.allocator, join_stmt.remainder);
                },
                .switch_initialized_payload => |s| {
                    try self.stack.append(self.allocator, s.initialized_branch);
                    try self.stack.append(self.allocator, s.uninitialized_branch);
                },
                .str_match => |s| {
                    try self.stack.append(self.allocator, s.on_match);
                    try self.stack.append(self.allocator, s.on_miss);
                },
                .str_match_set => |s| {
                    const arms = self.store.getStrMatchArms(s.arms);
                    for (0..arms.len) |index| try self.stack.append(self.allocator, GuardedList.at(arms, index).on_match);
                    try self.stack.append(self.allocator, s.on_miss);
                },
                .boxy_tag_match => |s| {
                    try self.stack.append(self.allocator, s.on_match);
                    try self.stack.append(self.allocator, s.on_miss);
                },
                inline .assign_ref, .assign_literal, .init_uninitialized, .assign_call, .assign_call_erased, .assign_packed_erased_fn, .assign_boxy_desc_ref, .assign_boxy_dict_ref, .assign_boxy_box, .assign_boxy_reuse_box, .assign_boxy_unbox, .assign_boxy_adapt, .assign_boxy_inspect, .assign_boxy_eq, .assign_boxy_tag, .assign_boxy_tag_payload, .assign_call_dict, .assign_low_level, .assign_list, .assign_tag, .store_struct, .store_tag, .set_local, .debug, .expect, .comptime_branch_taken, .incref, .decref, .decref_if_initialized, .free => |a| {
                    try self.stack.append(self.allocator, a.next);
                },
                .jump, .ret, .crash, .expect_err, .runtime_error, .comptime_exhaustiveness_failed, .loop_continue, .loop_break => {},
            }
        }

        self.visited.clearRetainingCapacity();
        self.stack.clearRetainingCapacity();
        try self.stack.append(self.allocator, body);
        while (self.stack.pop()) |current| {
            if (self.visited.contains(current)) continue;
            try self.visited.put(current, {});
            switch (self.store.getCFStmt(current)) {
                .assign_ref => |assign| {
                    switch (assign.op) {
                        .field => |op| try self.noteFieldRead(op.source, current),
                        .local => |source| {
                            if (source == assign.target) {
                                try self.noteUse(source);
                            } else if (self.join_params.contains(assign.target)) {
                                // An edge initializer. The source is treated
                                // as used whole for this round: if the target
                                // scalarizes, this statement becomes per-field
                                // reads of the source, and the source can
                                // qualify on a later round.
                                const entry = try self.alias_init_writes.getOrPut(assign.target);
                                if (!entry.found_existing) entry.value_ptr.* = .empty;
                                try entry.value_ptr.append(self.allocator, current);
                                try self.noteUse(source);
                                try self.stack.append(self.allocator, assign.next);
                                continue;
                            } else {
                                // Recorded, not yet counted as a use: whether
                                // the source is used whole through this alias
                                // is decided by resolveAliases once every use
                                // of the alias target is known.
                                const entry = try self.alias_defs.getOrPut(assign.target);
                                if (entry.found_existing) {
                                    // A multiply-defined target is never
                                    // transparent, and its sources may differ,
                                    // so both count as whole-value uses here.
                                    entry.value_ptr.def_count += 1;
                                    try self.noteUse(entry.value_ptr.source);
                                    try self.noteUse(source);
                                } else {
                                    entry.value_ptr.* = .{ .stmt = current, .source = source, .def_count = 1 };
                                }
                                try self.stack.append(self.allocator, assign.next);
                                continue;
                            }
                        },
                        .discriminant => |op| try self.noteUse(op.source),
                        .tag_payload => |op| try self.noteUse(op.source),
                        .tag_payload_struct => |op| try self.noteUse(op.source),
                        .list_reinterpret => |op| try self.noteUse(op.backing_ref),
                        .nominal => |op| try self.noteUse(op.backing_ref),
                    }
                    try self.noteWrite(assign.target);
                    try self.stack.append(self.allocator, assign.next);
                },
                .assign_literal => |assign| {
                    try self.noteWrite(assign.target);
                    try self.stack.append(self.allocator, assign.next);
                },
                .init_uninitialized => |init| {
                    try self.noteWrite(init.target);
                    try self.stack.append(self.allocator, init.next);
                },
                .assign_call => |assign| {
                    if (assign.result_desc) |result_desc| {
                        if (result_desc.localOrNull()) |local| try self.noteUse(local);
                    }
                    const args = self.store.getLocalSpan(assign.args);
                    for (0..args.len) |index| try self.noteUse(GuardedList.at(args, index));
                    try self.noteWrite(assign.target);
                    try self.stack.append(self.allocator, assign.next);
                },
                .assign_call_erased => |assign| {
                    try self.noteUse(assign.closure);
                    if (assign.result_desc) |result_desc| {
                        if (result_desc.localOrNull()) |local| try self.noteUse(local);
                    }
                    if (assign.reuse_source) |reuse_source| try self.noteUse(reuse_source);
                    const args = self.store.getLocalSpan(assign.args);
                    for (0..args.len) |index| try self.noteUse(GuardedList.at(args, index));
                    try self.noteWrite(assign.target);
                    if (assign.out_desc) |out_desc| try self.noteWrite(out_desc);
                    try self.stack.append(self.allocator, assign.next);
                },
                .assign_packed_erased_fn => |assign| {
                    if (assign.capture) |capture| try self.noteUse(capture);
                    if (assign.result_desc) |result_desc| {
                        if (result_desc.localOrNull()) |local| try self.noteUse(local);
                    }
                    if (assign.reuse) |reuse| try self.noteUse(reuse);
                    try self.noteWrite(assign.target);
                    try self.stack.append(self.allocator, assign.next);
                },
                .assign_boxy_desc_ref => |assign| {
                    try self.noteDescUse(assign.desc);
                    if (assign.tag_residual_for) |desc| try self.noteDescUse(desc);
                    const captures = self.store.getLocalSpan(assign.captures);
                    for (0..GuardedList.borrowLen(captures)) |index| try self.noteUse(GuardedList.at(captures, index));
                    try self.noteWrite(assign.target);
                    try self.stack.append(self.allocator, assign.next);
                },
                .assign_boxy_dict_ref => |assign| {
                    try self.noteDictUse(assign.dict);
                    try self.noteWrite(assign.target);
                    try self.stack.append(self.allocator, assign.next);
                },
                .assign_boxy_box => |assign| {
                    try self.noteUse(assign.payload);
                    if (assign.payload_desc) |desc| try self.noteDescUse(desc);
                    try self.noteWrite(assign.target);
                    try self.stack.append(self.allocator, assign.next);
                },
                .assign_boxy_reuse_box => |assign| {
                    try self.noteUse(assign.source);
                    try self.noteDescUse(assign.desc);
                    try self.noteWrite(assign.target);
                    try self.stack.append(self.allocator, assign.next);
                },
                .assign_boxy_unbox => |assign| {
                    try self.noteUse(assign.source);
                    try self.noteDescUse(assign.source_desc);
                    if (assign.target_desc) |desc| try self.noteDescUse(desc);
                    try self.noteWrite(assign.target);
                    try self.stack.append(self.allocator, assign.next);
                },
                .assign_boxy_adapt => |assign| {
                    try self.noteUse(assign.source);
                    if (assign.source_desc) |desc| try self.noteDescUse(desc);
                    if (assign.target_desc) |desc| try self.noteDescUse(desc);
                    try self.noteWrite(assign.target);
                    try self.stack.append(self.allocator, assign.next);
                },
                .assign_boxy_inspect => |assign| {
                    try self.noteUse(assign.source);
                    try self.noteDescUse(assign.source_desc);
                    try self.noteWrite(assign.target);
                    try self.stack.append(self.allocator, assign.next);
                },
                .assign_boxy_eq => |assign| {
                    try self.noteUse(assign.lhs);
                    try self.noteUse(assign.rhs);
                    try self.noteDescUse(assign.source_desc);
                    try self.noteWrite(assign.target);
                    try self.stack.append(self.allocator, assign.next);
                },
                .assign_boxy_tag => |assign| {
                    try self.noteDescUse(assign.target_desc);
                    if (assign.payload) |payload| try self.noteUse(payload);
                    if (assign.payload_desc) |desc| try self.noteDescUse(desc);
                    try self.noteWrite(assign.target);
                    try self.stack.append(self.allocator, assign.next);
                },
                .assign_boxy_tag_payload => |assign| {
                    try self.noteUse(assign.source);
                    try self.noteDescUse(assign.source_desc);
                    try self.noteWrite(assign.target);
                    if (assign.target_desc) |target_desc| try self.noteWrite(target_desc);
                    try self.stack.append(self.allocator, assign.next);
                },
                .boxy_tag_match => |s| {
                    try self.noteUse(s.source);
                    try self.noteDescUse(s.source_desc);
                    try self.stack.append(self.allocator, s.on_match);
                    try self.stack.append(self.allocator, s.on_miss);
                },
                .assign_call_dict => |assign| {
                    try self.noteDictUse(assign.dict);
                    const args = self.store.getLocalSpan(assign.args);
                    for (0..GuardedList.borrowLen(args)) |index| try self.noteUse(GuardedList.at(args, index));
                    const arg_descs = self.store.getLocalSpan(assign.arg_descs);
                    for (0..GuardedList.borrowLen(arg_descs)) |index| try self.noteUse(GuardedList.at(arg_descs, index));
                    const hidden_args = self.store.getLocalSpan(assign.hidden_args);
                    for (0..GuardedList.borrowLen(hidden_args)) |index| try self.noteUse(GuardedList.at(hidden_args, index));
                    try self.noteWrite(assign.target);
                    try self.stack.append(self.allocator, assign.next);
                },
                .assign_low_level => |assign| {
                    const args = self.store.getLocalSpan(assign.args);
                    for (0..args.len) |index| try self.noteUse(GuardedList.at(args, index));
                    try self.noteWrite(assign.target);
                    try self.stack.append(self.allocator, assign.next);
                },
                .assign_list => |assign| {
                    const elems = self.store.getLocalSpan(assign.elems);
                    for (0..elems.len) |index| try self.noteUse(GuardedList.at(elems, index));
                    try self.noteWrite(assign.target);
                    try self.stack.append(self.allocator, assign.next);
                },
                .assign_struct => |assign| {
                    const fields = self.store.getLocalSpan(assign.fields);
                    for (0..fields.len) |index| try self.noteUse(GuardedList.at(fields, index));
                    try self.stack.append(self.allocator, assign.next);
                },
                .assign_tag => |assign| {
                    if (assign.target_desc) |target_desc| try self.noteDescUse(target_desc);
                    if (assign.payload) |payload| try self.noteUse(payload);
                    try self.noteWrite(assign.target);
                    try self.stack.append(self.allocator, assign.next);
                },
                .store_struct => |assign| {
                    try self.noteUse(assign.dest);
                    const fields = self.store.getLocalSpan(assign.fields);
                    for (0..fields.len) |index| try self.noteUse(GuardedList.at(fields, index));
                    try self.stack.append(self.allocator, assign.next);
                },
                .store_tag => |assign| {
                    try self.noteUse(assign.dest);
                    if (assign.payload) |payload| try self.noteUse(payload);
                    try self.stack.append(self.allocator, assign.next);
                },
                .set_local => |assign| {
                    if (assign.mode == .initialize_join_param) {
                        const entry = try self.init_writes.getOrPut(assign.target);
                        if (!entry.found_existing) entry.value_ptr.* = .empty;
                        try entry.value_ptr.append(self.allocator, current);
                        if (self.struct_builds.getPtr(assign.value)) |build| {
                            // Counted separately: a qualifying build's only
                            // use must be this write.
                            build.init_uses += 1;
                        } else {
                            try self.noteUse(assign.value);
                        }
                    } else {
                        try self.noteUse(assign.value);
                        try self.noteWrite(assign.target);
                    }
                    try self.stack.append(self.allocator, assign.next);
                },
                .debug => |s| {
                    try self.noteUse(s.message);
                    try self.stack.append(self.allocator, s.next);
                },
                .expect_err => |s| try self.noteUse(s.message),
                .expect => |s| {
                    try self.noteUse(s.condition);
                    try self.stack.append(self.allocator, s.next);
                },
                .comptime_branch_taken => |s| try self.stack.append(self.allocator, s.next),
                .switch_stmt => |s| {
                    try self.noteUse(s.cond);
                    const branches = self.store.getCFSwitchBranches(s.branches);
                    for (0..branches.len) |index| try self.stack.append(self.allocator, GuardedList.at(branches, index).body);
                    try self.stack.append(self.allocator, s.default_branch);
                    if (s.continuation) |continuation| {
                        try self.stack.append(self.allocator, continuation);
                    }
                },
                .switch_initialized_payload => |s| {
                    try self.noteUse(s.cond);
                    try self.stack.append(self.allocator, s.initialized_branch);
                    try self.stack.append(self.allocator, s.uninitialized_branch);
                },
                .str_match => |s| {
                    try self.noteUse(s.source);
                    const steps = self.store.getStrMatchSteps(s.steps);
                    for (0..steps.len) |index| {
                        const step = GuardedList.at(steps, index);
                        switch (step.capture) {
                            .discard => {},
                            .view => |local| try self.noteWrite(local),
                        }
                    }
                    try self.stack.append(self.allocator, s.on_match);
                    try self.stack.append(self.allocator, s.on_miss);
                },
                .str_match_set => |s| {
                    try self.noteUse(s.source);
                    const arms = self.store.getStrMatchArms(s.arms);
                    for (0..arms.len) |arm_index| {
                        const arm = GuardedList.at(arms, arm_index);
                        const steps = self.store.getStrMatchSteps(arm.steps);
                        for (0..steps.len) |step_index| {
                            const step = GuardedList.at(steps, step_index);
                            switch (step.capture) {
                                .discard => {},
                                .view => |local| try self.noteWrite(local),
                            }
                        }
                        try self.stack.append(self.allocator, arm.on_match);
                    }
                    try self.stack.append(self.allocator, s.on_miss);
                },
                .join => |join_stmt| {
                    try self.stack.append(self.allocator, join_stmt.body);
                    try self.stack.append(self.allocator, join_stmt.remainder);
                },
                .ret => |ret_stmt| try self.noteUse(ret_stmt.value),
                .crash => |crash_stmt| if (crash_stmt.msg.localId()) |message| try self.noteUse(message),
                .incref => |rc| {
                    try self.noteUse(rc.value);
                    try self.stack.append(self.allocator, rc.next);
                },
                .decref => |rc| {
                    try self.noteUse(rc.value);
                    try self.stack.append(self.allocator, rc.next);
                },
                .decref_if_initialized => |rc| {
                    try self.noteUse(rc.cond);
                    try self.noteUse(rc.value);
                    try self.stack.append(self.allocator, rc.next);
                },
                .free => |rc| {
                    try self.noteUse(rc.value);
                    try self.stack.append(self.allocator, rc.next);
                },
                .jump, .runtime_error, .comptime_exhaustiveness_failed, .loop_continue, .loop_break => {},
            }
        }
    }
};

test "scalarize declarations are referenced" {
    std.testing.refAllDecls(@This());
}

const testing = std.testing;

const ScalarizeTest = struct {
    store: LirStore,
    layouts: layout_mod.Store,
    pair: layout_mod.Idx,
    next_join_point: u32 = 0,

    fn init(allocator: Allocator) Allocator.Error!ScalarizeTest {
        var layouts = try layout_mod.Store.init(allocator, .u64);
        errdefer layouts.deinit();
        const pair = try layouts.putStructFields(&[_]layout_mod.StructField{
            .{ .index = 0, .layout = .i64 },
            .{ .index = 1, .layout = .str },
        });
        return .{
            .store = LirStore.init(allocator),
            .layouts = layouts,
            .pair = pair,
        };
    }

    fn deinit(self: *ScalarizeTest) void {
        self.store.deinit();
        self.layouts.deinit();
    }

    fn freshJoinPointId(self: *ScalarizeTest) LIR.JoinPointId {
        const id: LIR.JoinPointId = @enumFromInt(self.next_join_point);
        self.next_join_point += 1;
        return id;
    }
};

test "scalarize splits a literal-initialized struct join parameter" {
    var f = try ScalarizeTest.init(testing.allocator);
    defer f.deinit();
    const store = &f.store;

    // join J(state: {i64, str}):
    //   body: n = state.0; s = state.1; ret n
    //   remainder: num = 1; text = "x"; wrapper = {num, text};
    //              state := wrapper; jump J
    const state = try store.addLocal(.{ .layout_idx = f.pair });
    const num = try store.addLocal(.{ .layout_idx = .i64 });
    const text = try store.addLocal(.{ .layout_idx = .str });
    const wrapper = try store.addLocal(.{ .layout_idx = f.pair });
    const n = try store.addLocal(.{ .layout_idx = .i64 });
    const s = try store.addLocal(.{ .layout_idx = .str });
    const join_id = f.freshJoinPointId();

    const ret = try store.addCFStmt(.{ .ret = .{ .value = n } });
    const read_s = try store.addCFStmt(.{ .assign_ref = .{
        .target = s,
        .op = .{ .field = .{ .source = state, .field_idx = 1 } },
        .next = ret,
    } });
    const read_n = try store.addCFStmt(.{ .assign_ref = .{
        .target = n,
        .op = .{ .field = .{ .source = state, .field_idx = 0 } },
        .next = read_s,
    } });

    const jump = try store.addCFStmt(.{ .jump = .{ .target = join_id } });
    const set_state = try store.addCFStmt(.{ .set_local = .{
        .target = state,
        .value = wrapper,
        .mode = .initialize_join_param,
        .next = jump,
    } });
    const build = try store.addCFStmt(.{ .assign_struct = .{
        .target = wrapper,
        .fields = try store.addLocalSpan(&.{ num, text }),
        .next = set_state,
    } });
    const text_assign = try store.addCFStmt(.{ .assign_literal = .{
        .target = text,
        .value = .{ .str_literal = try store.insertStringView("x", 0, 1) },
        .next = build,
    } });
    const num_assign = try store.addCFStmt(.{ .assign_literal = .{
        .target = num,
        .value = .{ .i64_literal = .{ .value = 1, .layout_idx = .i64 } },
        .next = text_assign,
    } });
    const join = try store.addCFStmt(.{ .join = .{
        .id = join_id,
        .params = try store.addLocalSpan(&.{state}),
        .body = read_n,
        .remainder = num_assign,
    } });
    _ = try store.addProcSpec(.{
        .name = store.freshSyntheticSymbol(),
        .args = LIR.LocalSpan.empty(),
        .body = join,
        .ret_layout = .i64,
    });

    try run(store, &f.layouts);

    // The join now carries two parameters, the field reads are aliases of
    // them, the jump site snapshots both operands before writing either
    // parameter, and the wrapper's build is unreachable.
    const new_join = store.getCFStmt(join).join;
    const params = store.getLocalSpan(new_join.params);
    try testing.expectEqual(@as(usize, 2), params.len);

    const new_read_n = store.getCFStmt(read_n).assign_ref;
    try testing.expectEqual(GuardedList.at(params, 0), new_read_n.op.local);
    const new_read_s = store.getCFStmt(read_s).assign_ref;
    try testing.expectEqual(GuardedList.at(params, 1), new_read_s.op.local);

    const first_snapshot = store.getCFStmt(set_state).assign_ref;
    try testing.expectEqual(num, first_snapshot.op.local);
    const second_snapshot = store.getCFStmt(first_snapshot.next).assign_ref;
    try testing.expectEqual(text, second_snapshot.op.local);
    const first_set = store.getCFStmt(second_snapshot.next).set_local;
    try testing.expectEqual(GuardedList.at(params, 0), first_set.target);
    try testing.expectEqual(first_snapshot.target, first_set.value);
    const second_set = store.getCFStmt(first_set.next).set_local;
    try testing.expectEqual(GuardedList.at(params, 1), second_set.target);
    try testing.expectEqual(second_snapshot.target, second_set.value);
    try testing.expectEqual(jump, second_set.next);

    // The text literal now flows straight to the first snapshot.
    const new_text_assign = store.getCFStmt(text_assign).assign_literal;
    try testing.expectEqual(set_state, new_text_assign.next);
}

test "scalarize keeps descriptor-bearing struct join parameters" {
    var f = try ScalarizeTest.init(testing.allocator);
    defer f.deinit();
    const store = &f.store;

    const state = try store.addLocal(.{ .layout_idx = f.pair });
    const state_desc = try store.addLocal(.{ .layout_idx = .opaque_ptr });
    store.setLocalBoxyDesc(state, .{ .local = state_desc });
    const num = try store.addLocal(.{ .layout_idx = .i64 });
    const text = try store.addLocal(.{ .layout_idx = .str });
    const wrapper = try store.addLocal(.{ .layout_idx = f.pair });
    const n = try store.addLocal(.{ .layout_idx = .i64 });
    const s = try store.addLocal(.{ .layout_idx = .str });
    const join_id = f.freshJoinPointId();

    const ret = try store.addCFStmt(.{ .ret = .{ .value = n } });
    const read_s = try store.addCFStmt(.{ .assign_ref = .{
        .target = s,
        .op = .{ .field = .{ .source = state, .field_idx = 1 } },
        .next = ret,
    } });
    const read_n = try store.addCFStmt(.{ .assign_ref = .{
        .target = n,
        .op = .{ .field = .{ .source = state, .field_idx = 0 } },
        .next = read_s,
    } });
    const jump = try store.addCFStmt(.{ .jump = .{ .target = join_id } });
    const set_state = try store.addCFStmt(.{ .set_local = .{
        .target = state,
        .value = wrapper,
        .mode = .initialize_join_param,
        .next = jump,
    } });
    const build = try store.addCFStmt(.{ .assign_struct = .{
        .target = wrapper,
        .fields = try store.addLocalSpan(&.{ num, text }),
        .next = set_state,
    } });
    const text_assign = try store.addCFStmt(.{ .assign_literal = .{
        .target = text,
        .value = .{ .str_literal = try store.insertStringView("x", 0, 1) },
        .next = build,
    } });
    const num_assign = try store.addCFStmt(.{ .assign_literal = .{
        .target = num,
        .value = .{ .i64_literal = .{ .value = 1, .layout_idx = .i64 } },
        .next = text_assign,
    } });
    const join = try store.addCFStmt(.{ .join = .{
        .id = join_id,
        .params = try store.addLocalSpan(&.{state}),
        .body = read_n,
        .remainder = num_assign,
    } });
    _ = try store.addProcSpec(.{
        .name = store.freshSyntheticSymbol(),
        .args = try store.addLocalSpan(&.{state_desc}),
        .body = join,
        .ret_layout = .i64,
    });

    try run(store, &f.layouts);

    const params = store.getLocalSpan(store.getCFStmt(join).join.params);
    try testing.expectEqual(@as(usize, 1), params.len);
    try testing.expectEqual(state, GuardedList.at(params, 0));
    try testing.expectEqual(state, store.getCFStmt(read_n).assign_ref.op.field.source);
    try testing.expectEqual(state, store.getCFStmt(read_s).assign_ref.op.field.source);
}

test "scalarize keeps parameters with whole-value uses" {
    var f = try ScalarizeTest.init(testing.allocator);
    defer f.deinit();
    const store = &f.store;

    const state = try store.addLocal(.{ .layout_idx = f.pair });
    const whole = try store.addLocal(.{ .layout_idx = f.pair });
    const num = try store.addLocal(.{ .layout_idx = .i64 });
    const text = try store.addLocal(.{ .layout_idx = .str });
    const wrapper = try store.addLocal(.{ .layout_idx = f.pair });
    const join_id = f.freshJoinPointId();

    // The body copies the whole parameter, which must block scalarization.
    const ret_local = try store.addLocal(.{ .layout_idx = .i64 });
    const ret = try store.addCFStmt(.{ .ret = .{ .value = ret_local } });
    const ret_assign = try store.addCFStmt(.{ .assign_literal = .{
        .target = ret_local,
        .value = .{ .i64_literal = .{ .value = 0, .layout_idx = .i64 } },
        .next = ret,
    } });
    const copy_whole = try store.addCFStmt(.{ .assign_ref = .{
        .target = whole,
        .op = .{ .local = state },
        .next = ret_assign,
    } });

    const jump = try store.addCFStmt(.{ .jump = .{ .target = join_id } });
    const set_state = try store.addCFStmt(.{ .set_local = .{
        .target = state,
        .value = wrapper,
        .mode = .initialize_join_param,
        .next = jump,
    } });
    const build = try store.addCFStmt(.{ .assign_struct = .{
        .target = wrapper,
        .fields = try store.addLocalSpan(&.{ num, text }),
        .next = set_state,
    } });
    const text_assign = try store.addCFStmt(.{ .assign_literal = .{
        .target = text,
        .value = .{ .str_literal = try store.insertStringView("x", 0, 1) },
        .next = build,
    } });
    const num_assign = try store.addCFStmt(.{ .assign_literal = .{
        .target = num,
        .value = .{ .i64_literal = .{ .value = 1, .layout_idx = .i64 } },
        .next = text_assign,
    } });
    const join = try store.addCFStmt(.{ .join = .{
        .id = join_id,
        .params = try store.addLocalSpan(&.{state}),
        .body = copy_whole,
        .remainder = num_assign,
    } });
    _ = try store.addProcSpec(.{
        .name = store.freshSyntheticSymbol(),
        .args = LIR.LocalSpan.empty(),
        .body = join,
        .ret_layout = .i64,
    });

    try run(store, &f.layouts);

    const unchanged_join = store.getCFStmt(join).join;
    try testing.expectEqual(@as(usize, 1), store.getLocalSpan(unchanged_join.params).len);
    const unchanged_set = store.getCFStmt(set_state).set_local;
    try testing.expectEqual(state, unchanged_set.target);
    try testing.expectEqual(wrapper, unchanged_set.value);
}

test "scalarize splits a parameter built directly by a struct literal" {
    var f = try ScalarizeTest.init(testing.allocator);
    defer f.deinit();
    const store = &f.store;

    // join J(state: {i64, str}):
    //   body: n = state.0; s = state.1; ret n
    //   remainder: num = 1; text = "x"; state = {num, text}; jump J
    const state = try store.addLocal(.{ .layout_idx = f.pair });
    const num = try store.addLocal(.{ .layout_idx = .i64 });
    const text = try store.addLocal(.{ .layout_idx = .str });
    const n = try store.addLocal(.{ .layout_idx = .i64 });
    const s = try store.addLocal(.{ .layout_idx = .str });
    const join_id = f.freshJoinPointId();

    const ret = try store.addCFStmt(.{ .ret = .{ .value = n } });
    const read_s = try store.addCFStmt(.{ .assign_ref = .{
        .target = s,
        .op = .{ .field = .{ .source = state, .field_idx = 1 } },
        .next = ret,
    } });
    const read_n = try store.addCFStmt(.{ .assign_ref = .{
        .target = n,
        .op = .{ .field = .{ .source = state, .field_idx = 0 } },
        .next = read_s,
    } });

    const jump = try store.addCFStmt(.{ .jump = .{ .target = join_id } });
    const build = try store.addCFStmt(.{ .assign_struct = .{
        .target = state,
        .fields = try store.addLocalSpan(&.{ num, text }),
        .next = jump,
    } });
    const text_assign = try store.addCFStmt(.{ .assign_literal = .{
        .target = text,
        .value = .{ .str_literal = try store.insertStringView("x", 0, 1) },
        .next = build,
    } });
    const num_assign = try store.addCFStmt(.{ .assign_literal = .{
        .target = num,
        .value = .{ .i64_literal = .{ .value = 1, .layout_idx = .i64 } },
        .next = text_assign,
    } });
    const join = try store.addCFStmt(.{ .join = .{
        .id = join_id,
        .params = try store.addLocalSpan(&.{state}),
        .body = read_n,
        .remainder = num_assign,
    } });
    _ = try store.addProcSpec(.{
        .name = store.freshSyntheticSymbol(),
        .args = LIR.LocalSpan.empty(),
        .body = join,
        .ret_layout = .i64,
    });

    try run(store, &f.layouts);

    // The join now carries two parameters, the field reads are aliases of
    // them, and the build became snapshots followed by per-field writes.
    const new_join = store.getCFStmt(join).join;
    const params = store.getLocalSpan(new_join.params);
    try testing.expectEqual(@as(usize, 2), params.len);

    const new_read_n = store.getCFStmt(read_n).assign_ref;
    try testing.expectEqual(GuardedList.at(params, 0), new_read_n.op.local);
    const new_read_s = store.getCFStmt(read_s).assign_ref;
    try testing.expectEqual(GuardedList.at(params, 1), new_read_s.op.local);

    const first_snapshot = store.getCFStmt(build).assign_ref;
    try testing.expectEqual(num, first_snapshot.op.local);
    const second_snapshot = store.getCFStmt(first_snapshot.next).assign_ref;
    try testing.expectEqual(text, second_snapshot.op.local);
    const first_set = store.getCFStmt(second_snapshot.next).set_local;
    try testing.expectEqual(GuardedList.at(params, 0), first_set.target);
    try testing.expectEqual(first_snapshot.target, first_set.value);
    try testing.expectEqual(LIR.SetLocalWriteMode.initialize_join_param, first_set.mode);
    const second_set = store.getCFStmt(first_set.next).set_local;
    try testing.expectEqual(GuardedList.at(params, 1), second_set.target);
    try testing.expectEqual(second_snapshot.target, second_set.value);
    try testing.expectEqual(jump, second_set.next);
}

test "scalarize sees through pure aliases to field reads" {
    var f = try ScalarizeTest.init(testing.allocator);
    defer f.deinit();
    const store = &f.store;

    // join J(state: {i64, str}):
    //   body: view = state; n = view.0; s = view.1; ret n
    //   remainder: num = 1; text = "x"; wrapper = {num, text};
    //              state := wrapper; jump J
    //
    // Lowered user code reads aggregates through `ref.local` aliases like
    // `view`; the parameter must still scalarize, with the alias deleted.
    const state = try store.addLocal(.{ .layout_idx = f.pair });
    const view = try store.addLocal(.{ .layout_idx = f.pair });
    const num = try store.addLocal(.{ .layout_idx = .i64 });
    const text = try store.addLocal(.{ .layout_idx = .str });
    const wrapper = try store.addLocal(.{ .layout_idx = f.pair });
    const n = try store.addLocal(.{ .layout_idx = .i64 });
    const s = try store.addLocal(.{ .layout_idx = .str });
    const join_id = f.freshJoinPointId();

    const ret = try store.addCFStmt(.{ .ret = .{ .value = n } });
    const read_s = try store.addCFStmt(.{ .assign_ref = .{
        .target = s,
        .op = .{ .field = .{ .source = view, .field_idx = 1 } },
        .next = ret,
    } });
    const read_n = try store.addCFStmt(.{ .assign_ref = .{
        .target = n,
        .op = .{ .field = .{ .source = view, .field_idx = 0 } },
        .next = read_s,
    } });
    const make_view = try store.addCFStmt(.{ .assign_ref = .{
        .target = view,
        .op = .{ .local = state },
        .next = read_n,
    } });

    const jump = try store.addCFStmt(.{ .jump = .{ .target = join_id } });
    const set_state = try store.addCFStmt(.{ .set_local = .{
        .target = state,
        .value = wrapper,
        .mode = .initialize_join_param,
        .next = jump,
    } });
    const build = try store.addCFStmt(.{ .assign_struct = .{
        .target = wrapper,
        .fields = try store.addLocalSpan(&.{ num, text }),
        .next = set_state,
    } });
    const text_assign = try store.addCFStmt(.{ .assign_literal = .{
        .target = text,
        .value = .{ .str_literal = try store.insertStringView("x", 0, 1) },
        .next = build,
    } });
    const num_assign = try store.addCFStmt(.{ .assign_literal = .{
        .target = num,
        .value = .{ .i64_literal = .{ .value = 1, .layout_idx = .i64 } },
        .next = text_assign,
    } });
    const join = try store.addCFStmt(.{ .join = .{
        .id = join_id,
        .params = try store.addLocalSpan(&.{state}),
        .body = make_view,
        .remainder = num_assign,
    } });
    const proc = try store.addProcSpec(.{
        .name = store.freshSyntheticSymbol(),
        .args = LIR.LocalSpan.empty(),
        .body = join,
        .ret_layout = .i64,
    });

    try run(store, &f.layouts);

    const new_join = store.getCFStmt(join).join;
    const params = store.getLocalSpan(new_join.params);
    try testing.expectEqual(@as(usize, 2), params.len);

    // Both reads now alias the field parameters, and the alias's definition
    // is gone: the join body starts at the first read.
    const new_read_n = store.getCFStmt(read_n).assign_ref;
    try testing.expectEqual(GuardedList.at(params, 0), new_read_n.op.local);
    const new_read_s = store.getCFStmt(read_s).assign_ref;
    try testing.expectEqual(GuardedList.at(params, 1), new_read_s.op.local);
    try testing.expectEqual(read_n, store.getCFStmt(join).join.body);
    try testing.expectEqual(join, store.getProcSpec(proc).body);
}

test "scalarize keeps parameters whose alias escapes whole" {
    var f = try ScalarizeTest.init(testing.allocator);
    defer f.deinit();
    const store = &f.store;

    // Like the alias test, but the alias is also returned whole, so the
    // parameter must keep its shape.
    const state = try store.addLocal(.{ .layout_idx = f.pair });
    const view = try store.addLocal(.{ .layout_idx = f.pair });
    const num = try store.addLocal(.{ .layout_idx = .i64 });
    const text = try store.addLocal(.{ .layout_idx = .str });
    const wrapper = try store.addLocal(.{ .layout_idx = f.pair });
    const n = try store.addLocal(.{ .layout_idx = .i64 });
    const join_id = f.freshJoinPointId();

    const ret = try store.addCFStmt(.{ .ret = .{ .value = view } });
    const read_n = try store.addCFStmt(.{ .assign_ref = .{
        .target = n,
        .op = .{ .field = .{ .source = view, .field_idx = 0 } },
        .next = ret,
    } });
    const make_view = try store.addCFStmt(.{ .assign_ref = .{
        .target = view,
        .op = .{ .local = state },
        .next = read_n,
    } });

    const jump = try store.addCFStmt(.{ .jump = .{ .target = join_id } });
    const set_state = try store.addCFStmt(.{ .set_local = .{
        .target = state,
        .value = wrapper,
        .mode = .initialize_join_param,
        .next = jump,
    } });
    const build = try store.addCFStmt(.{ .assign_struct = .{
        .target = wrapper,
        .fields = try store.addLocalSpan(&.{ num, text }),
        .next = set_state,
    } });
    const text_assign = try store.addCFStmt(.{ .assign_literal = .{
        .target = text,
        .value = .{ .str_literal = try store.insertStringView("x", 0, 1) },
        .next = build,
    } });
    const num_assign = try store.addCFStmt(.{ .assign_literal = .{
        .target = num,
        .value = .{ .i64_literal = .{ .value = 1, .layout_idx = .i64 } },
        .next = text_assign,
    } });
    const join = try store.addCFStmt(.{ .join = .{
        .id = join_id,
        .params = try store.addLocalSpan(&.{state}),
        .body = make_view,
        .remainder = num_assign,
    } });
    _ = try store.addProcSpec(.{
        .name = store.freshSyntheticSymbol(),
        .args = LIR.LocalSpan.empty(),
        .body = join,
        .ret_layout = f.pair,
    });

    try run(store, &f.layouts);

    const new_join = store.getCFStmt(join).join;
    const params = store.getLocalSpan(new_join.params);
    try testing.expectEqual(@as(usize, 1), params.len);
    try testing.expectEqual(state, GuardedList.at(params, 0));
}

test "scalarize seeds field parameters from a non-literal initializer" {
    var f = try ScalarizeTest.init(testing.allocator);
    defer f.deinit();
    const store = &f.store;

    // join J(state: {i64, str}):
    //   body: n = state.0; s = state.1; ret n
    //   remainder: init = <static literal>; state := init; jump J
    //
    // The initializer is not a struct build, so the write is replaced by
    // per-field reads of it. This is the shape comptime-evaluated initial
    // state lowers to.
    const state = try store.addLocal(.{ .layout_idx = f.pair });
    const init_value = try store.addLocal(.{ .layout_idx = f.pair });
    const n = try store.addLocal(.{ .layout_idx = .i64 });
    const s = try store.addLocal(.{ .layout_idx = .str });
    const join_id = f.freshJoinPointId();

    const ret = try store.addCFStmt(.{ .ret = .{ .value = n } });
    const read_s = try store.addCFStmt(.{ .assign_ref = .{
        .target = s,
        .op = .{ .field = .{ .source = state, .field_idx = 1 } },
        .next = ret,
    } });
    const read_n = try store.addCFStmt(.{ .assign_ref = .{
        .target = n,
        .op = .{ .field = .{ .source = state, .field_idx = 0 } },
        .next = read_s,
    } });

    const jump = try store.addCFStmt(.{ .jump = .{ .target = join_id } });
    const set_state = try store.addCFStmt(.{ .set_local = .{
        .target = state,
        .value = init_value,
        .mode = .initialize_join_param,
        .next = jump,
    } });
    const init_assign = try store.addCFStmt(.{ .assign_literal = .{
        .target = init_value,
        .value = .{ .i64_literal = .{ .value = 0, .layout_idx = .i64 } },
        .next = set_state,
    } });
    const join = try store.addCFStmt(.{ .join = .{
        .id = join_id,
        .params = try store.addLocalSpan(&.{state}),
        .body = read_n,
        .remainder = init_assign,
    } });
    _ = try store.addProcSpec(.{
        .name = store.freshSyntheticSymbol(),
        .args = LIR.LocalSpan.empty(),
        .body = join,
        .ret_layout = .i64,
    });

    try run(store, &f.layouts);

    const new_join = store.getCFStmt(join).join;
    const params = store.getLocalSpan(new_join.params);
    try testing.expectEqual(@as(usize, 2), params.len);

    // The write became: read init.0; read init.1; set P0; set P1; jump.
    const first_read = store.getCFStmt(set_state).assign_ref;
    try testing.expectEqual(init_value, first_read.op.field.source);
    try testing.expectEqual(@as(u32, 0), first_read.op.field.field_idx);
    const second_read = store.getCFStmt(first_read.next).assign_ref;
    try testing.expectEqual(init_value, second_read.op.field.source);
    try testing.expectEqual(@as(u32, 1), second_read.op.field.field_idx);
    const first_set = store.getCFStmt(second_read.next).set_local;
    try testing.expectEqual(GuardedList.at(params, 0), first_set.target);
    try testing.expectEqual(first_read.target, first_set.value);
    const second_set = store.getCFStmt(first_set.next).set_local;
    try testing.expectEqual(GuardedList.at(params, 1), second_set.target);
    try testing.expectEqual(jump, second_set.next);

    // The field reads alias the new parameters.
    const new_read_n = store.getCFStmt(read_n).assign_ref;
    try testing.expectEqual(GuardedList.at(params, 0), new_read_n.op.local);
    const new_read_s = store.getCFStmt(read_s).assign_ref;
    try testing.expectEqual(GuardedList.at(params, 1), new_read_s.op.local);
}

test "scalarize splits a parameter shared by two joins" {
    var f = try ScalarizeTest.init(testing.allocator);
    defer f.deinit();
    const store = &f.store;

    // Lowering shares one local as the parameter of nested joins:
    //   join OUTER(state):
    //     body: n = state.0; ret n
    //     remainder:
    //       join INNER(state):
    //         body: s = state.1; jump OUTER
    //         remainder: num = 1; text = "x"; state = {num, text}; jump INNER
    //
    // Both joins scalarize in one round onto one shared set of field
    // parameters, exactly as they shared the struct parameter: the build's
    // splat initializes the fields for both, and values carried from INNER
    // to OUTER stay in the shared field locals across the jump.
    const state = try store.addLocal(.{ .layout_idx = f.pair });
    const num = try store.addLocal(.{ .layout_idx = .i64 });
    const text = try store.addLocal(.{ .layout_idx = .str });
    const n = try store.addLocal(.{ .layout_idx = .i64 });
    const s = try store.addLocal(.{ .layout_idx = .str });
    const outer_id = f.freshJoinPointId();
    const inner_id = f.freshJoinPointId();

    const ret = try store.addCFStmt(.{ .ret = .{ .value = n } });
    const read_n = try store.addCFStmt(.{ .assign_ref = .{
        .target = n,
        .op = .{ .field = .{ .source = state, .field_idx = 0 } },
        .next = ret,
    } });

    const jump_outer = try store.addCFStmt(.{ .jump = .{ .target = outer_id } });
    const read_s = try store.addCFStmt(.{ .assign_ref = .{
        .target = s,
        .op = .{ .field = .{ .source = state, .field_idx = 1 } },
        .next = jump_outer,
    } });

    const jump_inner = try store.addCFStmt(.{ .jump = .{ .target = inner_id } });
    const build = try store.addCFStmt(.{ .assign_struct = .{
        .target = state,
        .fields = try store.addLocalSpan(&.{ num, text }),
        .next = jump_inner,
    } });
    const text_assign = try store.addCFStmt(.{ .assign_literal = .{
        .target = text,
        .value = .{ .str_literal = try store.insertStringView("x", 0, 1) },
        .next = build,
    } });
    const num_assign = try store.addCFStmt(.{ .assign_literal = .{
        .target = num,
        .value = .{ .i64_literal = .{ .value = 1, .layout_idx = .i64 } },
        .next = text_assign,
    } });
    const inner = try store.addCFStmt(.{ .join = .{
        .id = inner_id,
        .params = try store.addLocalSpan(&.{state}),
        .body = read_s,
        .remainder = num_assign,
    } });
    const outer = try store.addCFStmt(.{ .join = .{
        .id = outer_id,
        .params = try store.addLocalSpan(&.{state}),
        .body = read_n,
        .remainder = inner,
    } });
    _ = try store.addProcSpec(.{
        .name = store.freshSyntheticSymbol(),
        .args = LIR.LocalSpan.empty(),
        .body = outer,
        .ret_layout = .i64,
    });

    try run(store, &f.layouts);

    // Both spans carry the same field parameters in the struct's place.
    const outer_params = store.getLocalSpan(store.getCFStmt(outer).join.params);
    try testing.expectEqual(@as(usize, 2), outer_params.len);
    const inner_params = store.getLocalSpan(store.getCFStmt(inner).join.params);
    try testing.expectEqual(@as(usize, 2), inner_params.len);
    try testing.expectEqual(GuardedList.at(outer_params, 0), GuardedList.at(inner_params, 0));
    try testing.expectEqual(GuardedList.at(outer_params, 1), GuardedList.at(inner_params, 1));

    // The build became snapshot reads of its operands feeding per-field
    // writes.
    const first_snap = store.getCFStmt(build).assign_ref;
    try testing.expectEqual(num, first_snap.op.local);
    const second_snap = store.getCFStmt(first_snap.next).assign_ref;
    try testing.expectEqual(text, second_snap.op.local);
    const first_write = store.getCFStmt(second_snap.next).set_local;
    try testing.expectEqual(GuardedList.at(inner_params, 0), first_write.target);
    try testing.expectEqual(first_snap.target, first_write.value);
    const second_write = store.getCFStmt(first_write.next).set_local;
    try testing.expectEqual(GuardedList.at(inner_params, 1), second_write.target);
    try testing.expectEqual(second_snap.target, second_write.value);
    try testing.expectEqual(jump_inner, second_write.next);

    // Each join's field reads alias the shared field parameters.
    const new_read_n = store.getCFStmt(read_n).assign_ref;
    try testing.expectEqual(GuardedList.at(outer_params, 0), new_read_n.op.local);
    const new_read_s = store.getCFStmt(read_s).assign_ref;
    try testing.expectEqual(GuardedList.at(inner_params, 1), new_read_s.op.local);
}
