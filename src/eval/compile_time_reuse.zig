//! Reuse of compile-time values across edits of a module (design.md
//! "Compile-Time Value Reuse Across Edits").
//!
//! A freshly checked module copies, before evaluation, every root of its
//! previous cache entry whose value identity is unchanged and whose stored
//! data is portable. Compilers with runtime safety evaluate the copied roots
//! anyway, compare, and then install the copy.

const std = @import("std");
const base = @import("base");
const check = @import("check");

const CompileTimeFinalization = @import("compile_time_finalization.zig");

const Allocator = std.mem.Allocator;
const checked = check.CheckedArtifact;
const canonical = check.CanonicalNames;
const ConstStore = check.ConstStore.ConstStore;
const ConstTypeId = check.ConstStore.ConstTypeId;
const ComptimeRootId = checked.ComptimeRootId;

const trace_available = @import("builtin").link_libc and @import("build_options").dev_diagnostics;

/// `ROC_CTFE_REUSE_TRACE` is set: report each module's reuse. Inline, so a
/// compiler without developer diagnostics compiles no trace.
pub inline fn traceEnabled() bool {
    return trace_available and std.c.getenv("ROC_CTFE_REUSE_TRACE") != null;
}

/// The evaluation columns of a module's previous checked cache entry,
/// borrowed from its relocated artifact body.
pub const Previous = struct {
    key: checked.CheckedModuleArtifactKey,
    names: canonical.CanonicalNameStore,
    roots: checked.CompileTimeRootTable,
    identities: checked.CompileTimeRootIdentityTable,
    portability: checked.CompileTimeRootPortability,
    const_store: ConstStore,
    const_templates: checked.ConstTemplateTable,
    debug: checked.CompileTimeDebugStore,
    reads: checked.CompileTimeRootReadTable,

    /// Borrow the columns of a validated artifact body. The body must
    /// outlive the result.
    pub fn fromSerialized(serialized: *const checked.CheckedModuleArtifact.Serialized, gpa: Allocator) Previous {
        const base_addr = @intFromPtr(serialized);
        return .{
            .key = serialized.key,
            .names = deserializeField(serialized, "canonical_names", base_addr, gpa),
            .roots = deserializeField(serialized, "compile_time_roots", base_addr, gpa),
            .identities = deserializeField(serialized, "compile_time_root_identities", base_addr, gpa),
            .portability = deserializeField(serialized, "compile_time_root_portability", base_addr, gpa),
            .const_store = deserializeField(serialized, "const_store", base_addr, gpa),
            .const_templates = deserializeField(serialized, "const_templates", base_addr, gpa),
            .debug = deserializeField(serialized, "compile_time_debug", base_addr, gpa),
            .reads = deserializeField(serialized, "compile_time_root_reads", base_addr, gpa),
        };
    }

    pub fn deinit(self: *Previous) void {
        self.names.deinit();
        self.const_store.deinit();
    }
};

fn deserializeField(
    serialized: *const checked.CheckedModuleArtifact.Serialized,
    comptime name: []const u8,
    base_addr: usize,
    gpa: Allocator,
) @FieldType(checked.CheckedModuleArtifact, name) {
    const params = @typeInfo(@TypeOf(@FieldType(checked.CheckedModuleArtifact.Serialized, name).deserialize)).@"fn".param_types;
    return if (params.len == 3)
        @field(serialized, name).deserialize(base_addr, gpa)
    else
        @field(serialized, name).deserialize(base_addr);
}

/// One root of the new version and the previous root it copies.
const Match = struct {
    root: ComptimeRootId,
    previous: ComptimeRootId,
    /// `previous`'s reads, with the module's own reads renamed to the new
    /// version's roots.
    reads_start: u32,
    reads_len: u32,
};

/// The roots of one freshly checked module that its previous entry
/// supplies, with everything needed to install their copies.
pub const Transplant = struct {
    allocator: Allocator,
    matches: std.ArrayList(Match) = .empty,
    reads: std.ArrayList(checked.CompileTimeRootReadTable.Ref) = .empty,
    /// How many roots were waiting for evaluation.
    pending_roots: u32 = 0,

    pub fn deinit(self: *Transplant) void {
        self.matches.deinit(self.allocator);
        self.reads.deinit(self.allocator);
    }

    pub fn count(self: *const Transplant) usize {
        return self.matches.items.len;
    }

    pub fn rootAt(self: *const Transplant, index: usize) ComptimeRootId {
        return self.matches.items[index].root;
    }

    fn readsOf(self: *const Transplant, match: Match) []const checked.CompileTimeRootReadTable.Ref {
        return self.reads.items[match.reads_start..][0..match.reads_len];
    }
};

const IdentityKey = [64]u8;

fn identityKey(identity: checked.CompileTimeRootIdentity) IdentityKey {
    var key: IdentityKey = undefined;
    @memcpy(key[0..32], &identity.value);
    @memcpy(key[32..], &identity.site);
    return key;
}

/// Whether a previous root's stored data can be copied: it completed, its
/// stored data is portable, and it is a kind whose value can be reused.
fn reusable(previous: *const Previous, root: checked.CompileTimeRoot) bool {
    if (!previous.portability.isPortable(root.id)) return false;
    return switch (root.payload) {
        .const_node, .discarded => true,
        .pending, .unevaluated, .fn_value, .expect, .runtime => false,
    };
}

/// Find the previous root each pending root of `module` copies.
pub fn plan(allocator: Allocator, module: *const checked.CheckedModuleArtifact, previous: *const Previous) Allocator.Error!Transplant {
    var transplant = Transplant{ .allocator = allocator };
    errdefer transplant.deinit();
    const identities = module.compile_time_root_identities.rows;
    const previous_identities = previous.identities.rows;
    if (identities.len == 0 or previous_identities.len == 0) return transplant;

    var by_identity_and_site = std.AutoHashMapUnmanaged(IdentityKey, ComptimeRootId).empty;
    defer by_identity_and_site.deinit(allocator);
    var by_identity = std.AutoHashMapUnmanaged([32]u8, ComptimeRootId).empty;
    defer by_identity.deinit(allocator);
    for (previous.roots.roots) |root| {
        if (!reusable(previous, root)) continue;
        const identity = previous_identities[@backingInt(root.id)];
        const exact = try by_identity_and_site.getOrPut(allocator, identityKey(identity));
        if (!exact.found_existing) exact.value_ptr.* = root.id;
        const any = try by_identity.getOrPut(allocator, identity.value);
        if (!any.found_existing) any.value_ptr.* = root.id;
    }
    if (by_identity.count() == 0) return transplant;

    var by_site = std.AutoHashMapUnmanaged([32]u8, ComptimeRootId).empty;
    defer by_site.deinit(allocator);
    for (module.compile_time_roots.roots) |root| {
        if (root.kind == .expect or root.kind == .repl_expr) continue;
        const entry = try by_site.getOrPut(allocator, identities[@backingInt(root.id)].site);
        if (!entry.found_existing) entry.value_ptr.* = root.id;
    }

    for (module.compile_time_roots.roots) |root| {
        if (root.payload != .pending or root.kind == .repl_expr) continue;
        transplant.pending_roots += 1;
        const identity = identities[@backingInt(root.id)];
        const source = by_identity_and_site.get(identityKey(identity)) orelse by_identity.get(identity.value) orelse continue;
        const reads_start = transplant.reads.items.len;
        for (previous.reads.readsOf(source)) |read| {
            if (!std.mem.eql(u8, &read.module, &previous.key.bytes)) {
                try transplant.reads.append(allocator, read);
                continue;
            }
            const read_identity = previous_identities[@backingInt(read.root)];
            const target = by_site.get(read_identity.site) orelse
                base.invariant("a reused compile-time root read a root whose site the new module version lacks", .{});
            if (!std.mem.eql(u8, &identities[@backingInt(target)].value, &read_identity.value)) {
                base.invariant("a reused compile-time root read a root whose value identity changed", .{});
            }
            try transplant.reads.append(allocator, .{ .module = module.key.bytes, .root = target });
        }
        try transplant.matches.append(allocator, .{
            .root = root.id,
            .previous = source,
            .reads_start = @intCast(reads_start),
            .reads_len = @intCast(transplant.reads.items.len - reads_start),
        });
    }
    return transplant;
}

fn storedRootType(previous: *const Previous, root: ComptimeRootId) ConstTypeId {
    for (previous.const_templates.templates.items) |template| {
        switch (template.state) {
            .stored_const => |stored| if (stored.root == root) return stored.root_type,
            .reserved, .eval_template, .unimplemented => {},
        }
    }
    base.invariant("a stored compile-time constant had no stored template", .{});
}

/// A previous root's payload and stored type, copied into `module`.
const Copy = struct {
    payload: checked.CompileTimeRootPayload,
    root_type: ?ConstTypeId,
};

fn copyRoot(module: *checked.CheckedModuleArtifact, previous: *const Previous, match: Match) Allocator.Error!Copy {
    const root = module.compile_time_roots.root(match.root);
    const source = previous.roots.root(match.previous);
    return switch (source.payload) {
        .const_node => |node| .{
            .payload = .{ .const_node = try module.const_store.cloneValueFrom(&previous.const_store, node) },
            .root_type = switch (root.kind) {
                .constant, .hoisted_constant => try module.const_store.type_store.cloneTypeFromTranslated(
                    &previous.const_store.type_store,
                    &previous.names,
                    &module.canonical_names,
                    storedRootType(previous, match.previous),
                ),
                .hoisted_validation, .callable_binding, .expect, .numeral_conversion, .quote_conversion, .repl_expr => null,
            },
        },
        .discarded => .{ .payload = .discarded, .root_type = null },
        .pending, .unevaluated, .fn_value, .expect, .runtime => base.invariant("a compile-time root without reusable stored data was transplanted", .{}),
    };
}

/// Install every match's copy before evaluation: payload, stored templates,
/// translated reads, and `dbg` observations. Finalization then reads these
/// roots from the store like a reopened entry's evaluated roots.
pub fn install(module: *checked.CheckedModuleArtifact, previous: *const Previous, transplant: *const Transplant) Allocator.Error!void {
    for (transplant.matches.items) |match| {
        const copy = try copyRoot(module, previous, match);
        fillRoot(module, match.root, copy);
    }
    try installEvaluationRows(module, previous, transplant, transplant.matches.items);
}

fn fillRoot(module: *checked.CheckedModuleArtifact, root_id: ComptimeRootId, copy: Copy) void {
    module.compile_time_roots.fillPayload(root_id, copy.payload);
    CompileTimeFinalization.finishConstRoot(module, module.compile_time_roots.root(root_id), copy.payload, copy.root_type);
}

/// Merge `matches`' translated reads and copied observations into the
/// module's evaluation columns.
fn installEvaluationRows(module: *checked.CheckedModuleArtifact, previous: *const Previous, transplant: *const Transplant, matches: []const Match) Allocator.Error!void {
    if (matches.len == 0) return;
    const allocator = transplant.allocator;
    const artifact_allocator = module.evaluationAllocator();

    var inputs = std.ArrayList(checked.CompileTimeRootReadTable.Input).empty;
    defer inputs.deinit(allocator);
    for (matches) |match| try inputs.append(allocator, .{ .root = match.root, .reads = transplant.readsOf(match) });
    const merged = try checked.CompileTimeRootReadTable.merge(artifact_allocator, module.compile_time_root_reads, module.compile_time_roots.roots.len, inputs.items);
    if (module.freesReplacedEvaluationColumns()) module.compile_time_root_reads.deinit(artifact_allocator);
    module.compile_time_root_reads = merged;

    var debug_inputs = std.ArrayList(checked.CompileTimeDebugStore.Input).empty;
    defer debug_inputs.deinit(allocator);
    for (module.compile_time_debug.entries) |entry| try debug_inputs.append(allocator, .{ .root = entry.root, .message = module.compile_time_debug.message(entry) });
    var copied: usize = 0;
    for (matches) |match| {
        for (previous.debug.entries) |entry| {
            if (entry.root != match.previous) continue;
            try debug_inputs.append(allocator, .{ .root = match.root, .message = previous.debug.message(entry) });
            copied += 1;
        }
    }
    if (copied == 0) return;
    // Stable, so each root's observations keep their order.
    std.mem.sort(checked.CompileTimeDebugStore.Input, debug_inputs.items, {}, struct {
        fn lessThan(_: void, a: checked.CompileTimeDebugStore.Input, b: checked.CompileTimeDebugStore.Input) bool {
            return @backingInt(a.root) < @backingInt(b.root);
        }
    }.lessThan);
    const stored = try checked.CompileTimeDebugStore.init(artifact_allocator, debug_inputs.items);
    if (module.freesReplacedEvaluationColumns()) module.compile_time_debug.deinit(artifact_allocator);
    module.compile_time_debug = stored;
}

/// After finalization of a module whose matches stayed pending: compare each
/// evaluated root with its copy, then install the copy. A root the
/// compilation did not demand is installed from its copy. A difference means
/// the value identity missed an input, which is a compiler invariant.
pub fn verifyAndInstall(module: *checked.CheckedModuleArtifact, previous: *const Previous, transplant: *const Transplant) Allocator.Error!void {
    var unevaluated = std.ArrayList(Match).empty;
    defer unevaluated.deinit(transplant.allocator);
    for (transplant.matches.items) |match| {
        const root = module.compile_time_roots.root(match.root);
        const fresh = root.payload;
        const copy = try copyRoot(module, previous, match);
        switch (fresh) {
            .unevaluated => {
                fillRoot(module, match.root, copy);
                try unevaluated.append(transplant.allocator, match);
                continue;
            },
            .pending => base.invariant("a transplanted compile-time root was still pending after finalization", .{}),
            .const_node, .discarded, .fn_value, .expect, .runtime => {},
        }
        if (!try sameEvaluation(module, previous, transplant, match, fresh, copy)) {
            base.invariant("a transplanted compile-time root differs from its evaluation (module {s}, root {d}); its value identity missed an input", .{
                module.moduleEnvConst().module_name,
                @backingInt(match.root),
            });
        }
        module.compile_time_roots.fillPayload(match.root, copy.payload);
        if (copy.root_type) |root_type| replaceStoredConst(module, root, copy.payload, root_type);
    }
    try installEvaluationRows(module, previous, transplant, unevaluated.items);
}

fn sameEvaluation(
    module: *checked.CheckedModuleArtifact,
    previous: *const Previous,
    transplant: *const Transplant,
    match: Match,
    fresh: checked.CompileTimeRootPayload,
    copy: Copy,
) Allocator.Error!bool {
    if (std.meta.activeTag(fresh) != std.meta.activeTag(copy.payload)) return false;
    switch (fresh) {
        .const_node => |node| if (!try module.const_store.graphEql(node, &module.const_store, copy.payload.const_node)) return false,
        .discarded => {},
        .pending, .unevaluated, .fn_value, .expect, .runtime => return false,
    }
    if (copy.root_type) |root_type| {
        const fresh_type = storedTemplateType(module, module.compile_time_roots.root(match.root));
        if (!try module.const_store.type_store.graphEql(&module.canonical_names, fresh_type, &module.const_store.type_store, &module.canonical_names, root_type)) return false;
    }
    if (!try sameReadSets(transplant.allocator, module.compile_time_root_reads.readsOf(match.root), transplant.readsOf(match))) return false;
    var fresh_messages = std.ArrayList([]const u8).empty;
    defer fresh_messages.deinit(transplant.allocator);
    for (module.compile_time_debug.entries) |entry| {
        if (entry.root == match.root) try fresh_messages.append(transplant.allocator, module.compile_time_debug.message(entry));
    }
    var index: usize = 0;
    for (previous.debug.entries) |entry| {
        if (entry.root != match.previous) continue;
        if (index >= fresh_messages.items.len or !std.mem.eql(u8, fresh_messages.items[index], previous.debug.message(entry))) return false;
        index += 1;
    }
    return index == fresh_messages.items.len;
}

fn sameReadSets(allocator: Allocator, left: []const checked.CompileTimeRootReadTable.Ref, right: []const checked.CompileTimeRootReadTable.Ref) Allocator.Error!bool {
    const Ref = checked.CompileTimeRootReadTable.Ref;
    const lessThan = struct {
        fn lessThan(_: void, a: Ref, b: Ref) bool {
            const order = std.mem.order(u8, &a.module, &b.module);
            if (order != .eq) return order == .lt;
            return @backingInt(a.root) < @backingInt(b.root);
        }
    }.lessThan;
    const sorted_left = try allocator.dupe(Ref, left);
    defer allocator.free(sorted_left);
    const sorted_right = try allocator.dupe(Ref, right);
    defer allocator.free(sorted_right);
    std.mem.sort(Ref, sorted_left, {}, lessThan);
    std.mem.sort(Ref, sorted_right, {}, lessThan);
    const unique_left = uniqueRefs(sorted_left);
    const unique_right = uniqueRefs(sorted_right);
    if (unique_left.len != unique_right.len) return false;
    for (unique_left, unique_right) |a, b| {
        if (!std.mem.eql(u8, &a.module, &b.module) or a.root != b.root) return false;
    }
    return true;
}

fn uniqueRefs(refs: []checked.CompileTimeRootReadTable.Ref) []checked.CompileTimeRootReadTable.Ref {
    var len: usize = 0;
    for (refs) |ref| {
        if (len != 0 and std.mem.eql(u8, &refs[len - 1].module, &ref.module) and refs[len - 1].root == ref.root) continue;
        refs[len] = ref;
        len += 1;
    }
    return refs[0..len];
}

fn constRefForRoot(module: *const checked.CheckedModuleArtifact, root: checked.CompileTimeRoot) checked.ConstRef {
    return switch (root.kind) {
        .constant => blk: {
            const def_idx = switch (root.source) {
                .def => |def| def,
                .expr, .statement, .required_binding, .hoisted => base.invariant("constant root source was not a top-level definition", .{}),
            };
            const top_level = module.top_level_values.lookupByDef(def_idx) orelse
                base.invariant("constant root had no top-level value", .{});
            break :blk switch (top_level.value) {
                .const_ref => |ref| ref,
                .procedure_binding => base.invariant("constant root top-level value was not a constant", .{}),
            };
        },
        .hoisted_constant => (module.hoisted_constants.lookupByRoot(root.id) orelse
            base.invariant("hoisted constant root had no hoisted const entry", .{})).const_ref,
        .hoisted_validation, .callable_binding, .expect, .numeral_conversion, .quote_conversion, .repl_expr => base.invariant("compile-time root kind has no constant template", .{}),
    };
}

fn storedTemplateType(module: *const checked.CheckedModuleArtifact, root: checked.CompileTimeRoot) ConstTypeId {
    return switch (module.const_templates.get(constRefForRoot(module, root)).state) {
        .stored_const => |stored| stored.root_type,
        .reserved, .eval_template, .unimplemented => base.invariant("an evaluated compile-time constant had no stored template", .{}),
    };
}

fn replaceStoredConst(module: *checked.CheckedModuleArtifact, root: checked.CompileTimeRoot, payload: checked.CompileTimeRootPayload, root_type: ConstTypeId) void {
    const stored = checked.StoredConstTemplate{ .node = payload.const_node, .root = root.id, .root_type = root_type };
    const const_ref = constRefForRoot(module, root);
    module.const_templates.replaceStoredConst(const_ref, stored);
    if (root.kind == .constant) module.exported_const_templates.fillStoredConst(const_ref, stored);
}

/// Classify every root's stored data after finalization (design.md
/// "Compile-Time Value Reuse Across Edits"), replacing the column.
pub fn publishPortability(module: *checked.CheckedModuleArtifact) Allocator.Error!void {
    const artifact_allocator = module.evaluationAllocator();
    const roots = module.compile_time_roots.roots;
    const flags = try artifact_allocator.alloc(u8, roots.len);
    errdefer artifact_allocator.free(flags);
    const own: canonical.CheckedModuleDigest = .{ .bytes = module.key.bytes };
    const own_identity = &module.module_identity.stable_hash;
    for (roots, flags) |root, *flag| {
        const portable = switch (root.payload) {
            .discarded => true,
            .pending, .unevaluated, .fn_value, .expect, .runtime => false,
            .const_node => |node| switch (root.kind) {
                .repl_expr, .callable_binding, .expect => false,
                .numeral_conversion, .quote_conversion, .hoisted_validation => try module.const_store.valuePortable(node, own),
                .constant, .hoisted_constant => try module.const_store.valuePortable(node, own) and
                    try module.const_store.type_store.typePortable(&module.canonical_names, storedTemplateType(module, root), own, own_identity),
            },
        };
        flag.* = @intFromBool(portable);
    }
    if (module.freesReplacedEvaluationColumns()) module.compile_time_root_portability.deinit(artifact_allocator);
    module.compile_time_root_portability = .{ .portable = flags };
}
