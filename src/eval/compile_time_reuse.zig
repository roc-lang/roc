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
const collections = @import("collections");

const CompileTimeFinalization = @import("compile_time_finalization.zig");

const Allocator = std.mem.Allocator;
const checked = check.CheckedArtifact;
const canonical = check.CanonicalNames;
const ConstStore = check.ConstStore.ConstStore;
const ConstTypeId = check.ConstStore.ConstTypeId;
const ComptimeRootId = checked.ComptimeRootId;

/// `ROC_CTFE_REUSE_TRACE` is set: report each module's reuse. Inline, so a
/// compiler without developer diagnostics compiles no trace.
pub inline fn traceEnabled() bool {
    return base.dev_diagnostics.isSet(.ctfe_reuse_trace);
}

/// The evaluation columns of a module's previous checked cache entry,
/// borrowed from its relocated artifact body.
pub const Previous = struct {
    key: checked.CheckedModuleArtifactKey,
    checked_types: checked.CheckedTypeStore,
    own_identity: [32]u8,
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
            .own_identity = serialized.module_identity.stable_hash,
            .checked_types = deserializeField(serialized, "checked_types", base_addr, gpa),
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
        self.checked_types.deinit(self.const_store.allocator);
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
    misses: Misses = .{},
    /// The previous entry's value context and semantic context, for the
    /// reuse trace.
    previous_context: ?[32]u8 = null,
    previous_semantic_context: ?[32]u8 = null,
    type_occurrences: check.ConstStore.TypeOccurrenceMap,

    /// Why roots were not reused, for the reuse trace.
    pub const Misses = struct {
        /// Previous roots holding a function value, or a value or type that
        /// names the version of the module that stored them with no current
        /// occurrence to take its place.
        previous_bound: u32 = 0,
        /// Previous roots holding a function value, which no identity reuses
        /// across imports.
        previous_functions: u32 = 0,
        /// Previous roots whose value names a module version the compilation
        /// has no current version of, so they cannot be reused under
        /// semantic identities.
        previous_versioned: u32 = 0,
        /// Pending roots with no previous root of the same value identity.
        no_identity: u32 = 0,
        /// Pending roots matched by semantic identity whose reads could not
        /// be resolved in the current program.
        unresolved_reads: u32 = 0,
    };

    pub fn deinit(self: *Transplant) void {
        self.type_occurrences.deinit();
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

/// Whether a previous root can be reused under semantic dependency
/// identities: its value holds no function value, and every module version
/// it names has a current version to bind to (`Relocations`). Its recorded
/// demand is separately validated and rebound through producer-authored
/// read identities.
const AcrossImports = enum { reusable, holds_function, names_version };

fn reusableAcrossImports(previous: *const Previous, root: checked.CompileTimeRoot, relocations: *const Relocations) Allocator.Error!AcrossImports {
    switch (root.payload) {
        // A `.boxy` function value stays beside an unevaluated payload.
        .unevaluated => {
            const stored = root.boxy_value orelse return .holds_function;
            switch (try previous.const_store.valueRelocatability(stored.node, relocations)) {
                .relocatable => {},
                .holds_function => return .holds_function,
                .names_version => return .names_version,
            }
            return if (try previous.const_store.type_store.typeRelocatable(stored.root_type, relocations)) .reusable else .names_version;
        },
        .const_node => |node| {
            switch (try previous.const_store.valueRelocatability(node, relocations)) {
                .relocatable => {},
                .holds_function => return .holds_function,
                .names_version => return .names_version,
            }
            return switch (root.kind) {
                .constant, .hoisted_constant => if (try previous.const_store.type_store.typeRelocatable(storedRootType(previous, root.id), relocations)) .reusable else .names_version,
                .hoisted_validation, .callable_binding, .numeral_conversion, .quote_conversion, .literal_argument => .reusable,
                .expect, .repl_expr => .names_version,
            };
        },
        .discarded => return .reusable,
        .pending, .fn_value, .expect, .runtime => return .holds_function,
    }
}

/// Resolve a committed read to the current producer at its explicit source
/// placement, admitting it only when its checked value identity still matches.
pub const ReadInputs = struct {
    allocator: Allocator,
    roots: std.AutoHashMapUnmanaged([64]u8, Entry) = .empty,

    const Entry = struct { value: [32]u8, ref: checked.CompileTimeRootReadTable.Ref };

    pub fn deinit(self: *ReadInputs) void {
        self.roots.deinit(self.allocator);
    }

    pub fn addModule(self: *ReadInputs, placement: [32]u8, module: *const checked.CheckedModuleArtifact) Allocator.Error!void {
        if (module.compile_time_root_identities.test_rows.len == 0) return;
        for (module.compile_time_root_identities.test_rows, module.compile_time_roots.roots) |identity, root| {
            try self.roots.put(self.allocator, key(placement, identity.site), .{
                .value = identity.value,
                .ref = .{ .module = module.key.bytes, .root = root.id },
            });
        }
    }

    fn key(placement: [32]u8, site: [32]u8) [64]u8 {
        var bytes: [64]u8 = undefined;
        @memcpy(bytes[0..32], &placement);
        @memcpy(bytes[32..], &site);
        return bytes;
    }

    fn resolve(self: *const ReadInputs, read: checked.CompileTimeRootReadTable.Identity) ?checked.CompileTimeRootReadTable.Ref {
        const entry = self.roots.get(key(read.placement, read.root.site)) orelse return null;
        if (!std.meta.eql(entry.value, read.root.value)) return null;
        return entry.ref;
    }
};

/// Find the previous root each pending root of `module` copies.
pub fn plan(allocator: Allocator, module: *const checked.CheckedModuleArtifact, previous: *const Previous, inputs: *const ReadInputs, relocations: *const Relocations) Allocator.Error!Transplant {
    var transplant = Transplant{ .allocator = allocator, .type_occurrences = .init(allocator) };
    errdefer transplant.deinit();
    if (previous.identities.context.len != 0) transplant.previous_context = previous.identities.context[0];
    if (previous.identities.test_context.len != 0) transplant.previous_semantic_context = previous.identities.test_context[0];
    defer if (traceEnabled()) traceUnmatchedRoots(module, previous, &transplant);
    const identities = module.compile_time_root_identities.rows;
    const previous_identities = previous.identities.rows;
    if (identities.len == 0 or previous_identities.len == 0) return transplant;

    var by_identity_and_site = std.AutoHashMapUnmanaged(IdentityKey, ComptimeRootId).empty;
    defer by_identity_and_site.deinit(allocator);
    var by_identity = std.AutoHashMapUnmanaged([32]u8, ComptimeRootId).empty;
    defer by_identity.deinit(allocator);
    var by_semantics = std.AutoHashMapUnmanaged([32]u8, ComptimeRootId).empty;
    defer by_semantics.deinit(allocator);
    const semantic_rows = module.compile_time_root_identities.test_rows;
    const previous_semantics = previous.identities.test_rows;
    const has_semantics = semantic_rows.len == identities.len and previous_semantics.len == previous_identities.len;
    var occurrences_prepared = false;
    for (previous.roots.roots) |root| {
        // Semantic admission is independent of exact admission: a value
        // naming module versions moves to their current versions, which the
        // exact path leaves to its own version.
        if (has_semantics) switch (try reusableAcrossImports(previous, root, relocations)) {
            .reusable => {
                const entry = try by_semantics.getOrPut(allocator, previous_semantics[@backingInt(root.id)].value);
                if (!entry.found_existing) entry.value_ptr.* = root.id;
            },
            .holds_function => transplant.misses.previous_functions += 1,
            .names_version => transplant.misses.previous_versioned += 1,
        };
        if (!reusable(previous, root)) {
            if (root.payload != .const_node) continue;
            if (!occurrences_prepared) {
                for (previous.checked_types.view().rootRows()) |old| {
                    if (module.checked_types.view().rootForKey(old.key)) |current| {
                        try transplant.type_occurrences.put(old.id, current);
                    }
                }
                occurrences_prepared = true;
            }
            const own: canonical.CheckedModuleDigest = .{ .bytes = previous.key.bytes };
            if (!try previous.const_store.valuePortableWithOccurrences(root.payload.const_node, own, &transplant.type_occurrences)) {
                transplant.misses.previous_bound += 1;
                continue;
            }
            switch (root.kind) {
                .constant, .hoisted_constant => if (!try previous.const_store.type_store.typePortableWithOccurrences(
                    &previous.names,
                    storedRootType(previous, root.id),
                    own,
                    &previous.own_identity,
                    &transplant.type_occurrences,
                )) {
                    transplant.misses.previous_bound += 1;
                    continue;
                },
                .numeral_conversion, .quote_conversion, .hoisted_validation, .literal_argument => {},
                .callable_binding, .expect, .repl_expr => continue,
            }
        }
        const identity = previous_identities[@backingInt(root.id)];
        const exact = try by_identity_and_site.getOrPut(allocator, identityKey(identity));
        if (!exact.found_existing) exact.value_ptr.* = root.id;
        const any = try by_identity.getOrPut(allocator, identity.value);
        if (!any.found_existing) any.value_ptr.* = root.id;
    }
    if (by_identity.count() == 0 and by_semantics.count() == 0) return transplant;

    var by_site = std.AutoHashMapUnmanaged([32]u8, ComptimeRootId).empty;
    defer by_site.deinit(allocator);
    for (module.compile_time_roots.roots) |root| {
        if (root.kind == .expect or root.kind == .repl_expr) continue;
        const entry = try by_site.getOrPut(allocator, identities[@backingInt(root.id)].site);
        if (!entry.found_existing) entry.value_ptr.* = root.id;
    }

    roots: for (module.compile_time_roots.roots) |root| {
        if (root.payload != .pending or root.kind == .repl_expr) continue;
        transplant.pending_roots += 1;
        const identity = identities[@backingInt(root.id)];
        const semantic_source = if (has_semantics) by_semantics.get(semantic_rows[@backingInt(root.id)].value) else null;
        const exact_source = by_identity_and_site.get(identityKey(identity)) orelse by_identity.get(identity.value);
        const source = exact_source orelse semantic_source orelse {
            transplant.misses.no_identity += 1;
            continue;
        };
        const reads_start = transplant.reads.items.len;
        const reads = previous.reads.readsOf(source);
        if (exact_source == null and reads.len != 0) {
            // Admission uses only the producer's committed read identities.
            // Missing optional cache metadata is a miss, never an old root id.
            if (previous.reads.identities.len == 0) continue;
            const row = previous.reads.rows[@backingInt(source)];
            for (previous.reads.identities[row.start..][0..row.len]) |read| {
                const translated = inputs.resolve(read) orelse {
                    transplant.reads.shrinkRetainingCapacity(reads_start);
                    transplant.misses.unresolved_reads += 1;
                    continue :roots;
                };
                try transplant.reads.append(allocator, translated);
            }
        } else for (reads) |read| {
            if (!std.meta.eql(read.module, previous.key.bytes)) {
                try transplant.reads.append(allocator, read);
                continue;
            }
            const read_identity = previous_identities[@backingInt(read.root)];
            const target = by_site.get(read_identity.site) orelse
                base.invariant("a reused compile-time root read a root whose site the new module version lacks", .{});
            if (!std.meta.eql(identities[@backingInt(target)].value, read_identity.value)) {
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

/// Diagnostic only: explain whether a missed root still has the same source
/// site and value identities. No admission or rebinding decision uses this scan.
fn traceUnmatchedRoots(module: *const checked.CheckedModuleArtifact, previous: *const Previous, transplant: *const Transplant) void {
    const current = module.compile_time_root_identities;
    if (current.rows.len == 0 or previous.identities.rows.len == 0) return;
    roots: for (module.compile_time_roots.roots) |root| {
        if (root.payload != .pending) continue;
        for (transplant.matches.items) |match| if (match.root == root.id) continue :roots;
        const index = @backingInt(root.id);
        for (previous.roots.roots) |old| {
            const old_index = @backingInt(old.id);
            const has_semantics = current.test_rows.len != 0 and previous.identities.test_rows.len != 0;
            const same_site = std.meta.eql(current.rows[index].site, previous.identities.rows[old_index].site);
            if (!same_site and !(has_semantics and std.meta.eql(current.test_rows[index].site, previous.identities.test_rows[old_index].site))) continue;
            const same_semantics = has_semantics and
                std.meta.eql(current.test_rows[index].value, previous.identities.test_rows[old_index].value);
            std.debug.print("ctfe-reuse miss {s} root {d} {s}: previous {s}, portable {}, exact identity {}, semantic identity {}\n", .{
                module.moduleEnvConst().module_name,
                index,
                @tagName(root.kind),
                @tagName(old.payload),
                previous.portability.isPortable(old.id),
                std.meta.eql(current.rows[index].value, previous.identities.rows[old_index].value),
                same_semantics,
            });
            continue :roots;
        }
        std.debug.print("ctfe-reuse miss {s} root {d} {s}: no previous site\n", .{ module.moduleEnvConst().module_name, index, @tagName(root.kind) });
    }
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
    /// The `.boxy` function value copied beside an unevaluated payload.
    boxy_value: ?checked.StoredConstTemplate = null,

    /// The copied value's node, if it holds one.
    fn node(self: Copy) ?check.ConstStore.ConstNodeId {
        return switch (self.payload) {
            .const_node => |id| id,
            .unevaluated => if (self.boxy_value) |stored| stored.node else null,
            .discarded, .pending, .fn_value, .expect, .runtime => null,
        };
    }

    /// The copied value's stored type, if it holds one.
    fn storedType(self: Copy) ?ConstTypeId {
        if (self.root_type) |root_type| return root_type;
        return if (self.boxy_value) |stored| stored.root_type else null;
    }
};

fn copyRoot(module: *checked.CheckedModuleArtifact, previous: *const Previous, match: Match, occurrences: *const check.ConstStore.TypeOccurrenceMap, relocations: *const Relocations) Allocator.Error!Copy {
    const root = module.compile_time_roots.root(match.root);
    const source = previous.roots.root(match.previous);
    const values_start = module.const_store.values.items.len;
    const types_start = module.const_store.type_store.named_pool.items.len;
    const copy: Copy = switch (source.payload) {
        .const_node => |node| .{
            .payload = .{ .const_node = try module.const_store.cloneValueFrom(&previous.const_store, node) },
            .root_type = switch (root.kind) {
                .constant, .hoisted_constant => try module.const_store.type_store.cloneTypeFromTranslated(
                    &previous.const_store.type_store,
                    &previous.names,
                    &module.canonical_names,
                    storedRootType(previous, match.previous),
                ),
                .hoisted_validation, .callable_binding, .expect, .numeral_conversion, .quote_conversion, .repl_expr, .literal_argument => null,
            },
        },
        .discarded => .{ .payload = .discarded, .root_type = null },
        .unevaluated => blk: {
            const stored = source.boxy_value orelse base.invariant("a compile-time root without reusable stored data was transplanted", .{});
            break :blk .{
                .payload = .unevaluated,
                .root_type = null,
                .boxy_value = .{
                    .node = try module.const_store.cloneValueFrom(&previous.const_store, stored.node),
                    .root = match.root,
                    .root_type = try module.const_store.type_store.cloneTypeFromTranslated(
                        &previous.const_store.type_store,
                        &previous.names,
                        &module.canonical_names,
                        stored.root_type,
                    ),
                },
            };
        },
        .pending, .fn_value, .expect, .runtime => base.invariant("a compile-time root without reusable stored data was transplanted", .{}),
    };
    for (module.const_store.values.items[values_start..], values_start..) |*value, index| {
        switch (value.*) {
            .nominal => |*nominal| rebindOccurrence(&nominal.named_type, previous.key, module.key, occurrences, relocations),
            // Only semantic admission copies a stored function, after
            // `valueRelocatability` found every module it names current.
            .boxy_fn => try module.const_store.relocateRecipe(@fromBackingInt(@intCast(index)), relocations),
            else => {},
        }
    }
    for (module.const_store.type_store.named_pool.items[types_start..]) |*named| {
        rebindOccurrence(&named.named_type, previous.key, module.key, occurrences, relocations);
    }
    return copy;
}

/// Bind `named` to the current modules: an occurrence in the version of the
/// module that stored the value moves to the occurrence admission
/// established for it; one in another module's version moves to that
/// module's current version.
fn rebindOccurrence(named: *check.ConstStore.NamedType, previous: checked.CheckedModuleArtifactKey, current: checked.CheckedModuleArtifactKey, occurrences: *const check.ConstStore.TypeOccurrenceMap, relocations: *const Relocations) void {
    if (std.meta.eql(named.ty_module.bytes, previous.bytes)) {
        named.ty = occurrences.get(named.ty) orelse base.invariant("stored value admission established every local type occurrence", .{});
        named.ty_module = current.digest();
        return;
    }
    relocations.relocate(named);
}

/// Install every match's copy before evaluation: payload, stored templates,
/// translated reads, and `dbg` observations. Finalization then reads these
/// roots from the store like a reopened entry's evaluated roots.
pub fn install(module: *checked.CheckedModuleArtifact, previous: *const Previous, transplant: *const Transplant, relocations: *const Relocations) Allocator.Error!void {
    for (transplant.matches.items) |match| {
        const copy = try copyRoot(module, previous, match, &transplant.type_occurrences, relocations);
        fillRoot(module, match.root, copy);
    }
    try installEvaluationRows(module, previous, transplant, transplant.matches.items);
}

fn fillRoot(module: *checked.CheckedModuleArtifact, root_id: ComptimeRootId, copy: Copy) void {
    module.compile_time_roots.fillPayload(root_id, copy.payload);
    if (copy.boxy_value) |stored| {
        module.compile_time_roots.fillBoxyValue(root_id, stored);
        return;
    }
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

/// The checked modules of a compilation, by checked key: the type stores a
/// stored named type's checked occurrence (`NamedType.ty_module`) names.
pub const CheckedModules = struct {
    context: *anyopaque,
    find: *const fn (context: *anyopaque, key: [32]u8) ?*const checked.CheckedModuleArtifact,
};

/// How a stored value names the current versions of the modules of a
/// compilation: a module the compilation re-checked from an unchanged source
/// succeeds its previous entry, and each type root of that entry has its
/// occurrence in the new version; every other module of the compilation is
/// named as it is. A module whose source changed has no successor: a value
/// naming a type it declares stays with its version.
pub const Relocations = struct {
    allocator: Allocator,
    modules: CheckedModules,
    /// By previous checked key.
    successors: std.AutoHashMapUnmanaged([32]u8, Successor) = .empty,

    const Successor = struct {
        key: [32]u8,
        occurrences: check.ConstStore.TypeOccurrenceMap,
    };

    const Occurrence = struct { module: [32]u8, ty: checked.CheckedTypeId };

    pub fn init(allocator: Allocator, modules: CheckedModules) Relocations {
        return .{ .allocator = allocator, .modules = modules };
    }

    pub fn deinit(self: *Relocations) void {
        var successors = self.successors.valueIterator();
        while (successors.next()) |successor| successor.occurrences.deinit();
        self.successors.deinit(self.allocator);
    }

    /// Record `module` as the successor of its previous entry `previous`,
    /// when the module's own source is unchanged.
    pub fn add(self: *Relocations, module: *const checked.CheckedModuleArtifact, previous: *const Previous) Allocator.Error!void {
        if (!std.meta.eql(previous.own_identity, module.module_identity.stable_hash)) return;
        var occurrences = check.ConstStore.TypeOccurrenceMap.init(self.allocator);
        errdefer occurrences.deinit();
        const current = module.checked_types.view();
        for (previous.checked_types.view().rootRows()) |old| {
            if (current.rootForKey(old.key)) |id| try occurrences.put(old.id, id);
        }
        try self.successors.put(self.allocator, previous.key.bytes, .{ .key = module.key.bytes, .occurrences = occurrences });
    }

    /// The current key of the module `key` names, if it is in the
    /// compilation as it is or through a successor.
    pub fn currentKey(self: *const Relocations, key: [32]u8) ?[32]u8 {
        if (self.successors.get(key)) |successor| return successor.key;
        if (self.modules.find(self.modules.context, key) != null) return key;
        return null;
    }

    /// The current occurrence of type `ty` of the type store of `key`.
    fn currentOccurrence(self: *const Relocations, key: [32]u8, ty: checked.CheckedTypeId) ?Occurrence {
        if (self.successors.get(key)) |successor| return .{ .module = successor.key, .ty = successor.occurrences.get(ty) orelse return null };
        if (self.modules.find(self.modules.context, key) != null) return .{ .module = key, .ty = ty };
        return null;
    }

    /// Whether `named` can be bound to the current modules.
    pub fn nominalRelocatable(self: *const Relocations, named: check.ConstStore.NamedType) bool {
        return self.currentKey(named.module.bytes) != null and self.currentOccurrence(named.ty_module.bytes, named.ty) != null;
    }

    /// Bind `named`, which `nominalRelocatable` admitted, to the current
    /// modules.
    fn relocate(self: *const Relocations, named: *check.ConstStore.NamedType) void {
        named.module = .{ .bytes = self.currentKey(named.module.bytes) orelse base.invariant("a relocated named type names a module outside the compilation", .{}) };
        const occurrence = self.currentOccurrence(named.ty_module.bytes, named.ty) orelse base.invariant("a relocated named type's occurrence has no current occurrence", .{});
        named.ty_module = .{ .bytes = occurrence.module };
        named.ty = occurrence.ty;
    }
};

/// After finalization of a module whose matches stayed pending: compare each
/// evaluated root with its copy, then install the copy. A root the
/// compilation did not demand is installed from its copy. A difference means
/// the value identity missed an input, which is a compiler invariant. Every
/// named type the copy holds must name, through its checked occurrence, the
/// nominal type it stands for in the compilation's own checked modules.
pub fn verifyAndInstall(module: *checked.CheckedModuleArtifact, previous: *const Previous, transplant: *const Transplant, modules: CheckedModules, relocations: *const Relocations) Allocator.Error!void {
    var unevaluated = std.ArrayList(Match).empty;
    defer unevaluated.deinit(transplant.allocator);
    for (transplant.matches.items) |match| {
        const root = module.compile_time_roots.root(match.root);
        const fresh = root.payload;
        const copy = try copyRoot(module, previous, match, &transplant.type_occurrences, relocations);
        try verifyNamedTypeOccurrences(module, previous, modules, copy);
        switch (fresh) {
            .unevaluated => if (root.boxy_value == null) {
                fillRoot(module, match.root, copy);
                try unevaluated.append(transplant.allocator, match);
                continue;
            },
            .pending => base.invariant("a transplanted compile-time root was still pending after finalization", .{}),
            .const_node, .discarded, .fn_value, .expect, .runtime => {},
        }
        if (!try sameEvaluation(module, previous, transplant, match, root, copy)) {
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

/// Assert that every named type of a copied value graph and stored type
/// re-enters a current checked type store at the nominal type it names. A
/// copy may name this edited module only after explicit occurrence rebinding;
/// it never carries an occurrence of the previous version.
fn verifyNamedTypeOccurrences(module: *const checked.CheckedModuleArtifact, previous: *const Previous, modules: CheckedModules, copy: Copy) Allocator.Error!void {
    const allocator = module.canonical_names.allocator;
    const store = &module.const_store;
    if (copy.node()) |root| {
        var seen = collections.DenseMap(check.ConstStore.ConstNodeId, void).init(allocator);
        defer seen.deinit();
        var pending = std.ArrayList(check.ConstStore.ConstNodeId).empty;
        defer pending.deinit(allocator);
        try pending.append(allocator, root);
        while (pending.pop()) |node| {
            const entry = try seen.getOrPut(node);
            if (entry.found_existing) continue;
            switch (store.get(node)) {
                .pending, .fn_value => base.invariant("a transplanted value held a pending node or a Monotype function value", .{}),
                .boxy_fn => |function| try pending.appendSlice(allocator, function.captures),
                .zst, .scalar, .str, .crash, .checked_error => {},
                .box => |payload| try pending.append(allocator, payload),
                .nominal => |nominal| {
                    verifyNamedTypeOccurrence(module, previous, modules, nominal.named_type, null);
                    try pending.append(allocator, nominal.backing);
                },
                .list => |list| switch (list) {
                    .nodes => |items| try pending.appendSlice(allocator, items),
                    .packed_bytes, .empty => {},
                },
                .tuple, .record => |items| try pending.appendSlice(allocator, items),
                .tag => |tag| try pending.appendSlice(allocator, tag.payloads),
            }
        }
    }
    const root_type = copy.storedType() orelse return;
    const types = &store.type_store;
    var seen = collections.DenseMap(ConstTypeId, void).init(allocator);
    defer seen.deinit();
    var pending = std.ArrayList(ConstTypeId).empty;
    defer pending.deinit(allocator);
    try pending.append(allocator, root_type);
    while (pending.pop()) |ty| {
        const entry = try seen.getOrPut(ty);
        if (entry.found_existing) continue;
        switch (types.get(ty)) {
            .primitive, .zst, .erased => {},
            .list, .box => |elem| try pending.append(allocator, elem),
            .tuple => |span| try pending.appendSlice(allocator, types.typeSpan(span)),
            .func => |func| {
                try pending.appendSlice(allocator, types.typeSpan(func.args));
                try pending.append(allocator, func.ret);
            },
            .record => |span| for (types.fieldSpan(span)) |field| {
                try pending.append(allocator, field.ty);
                if (field.value_ty) |value_ty| try pending.append(allocator, value_ty);
            },
            .tag_union => |span| for (types.tagSpan(span)) |tag| try pending.appendSlice(allocator, types.typeSpan(tag.payloads)),
            .named => |named| {
                verifyNamedTypeOccurrence(module, previous, modules, named.named_type, named.kind);
                try pending.appendSlice(allocator, types.typeSpan(named.args));
                for (types.declaredFieldSpan(named.declared_order)) |declared| switch (declared) {
                    .named => {},
                    .padding => |padding| try pending.append(allocator, padding),
                };
                if (named.backing) |backing| try pending.append(allocator, backing.ty);
            },
        }
    }
}

fn verifyNamedTypeOccurrence(
    module: *const checked.CheckedModuleArtifact,
    previous: *const Previous,
    modules: CheckedModules,
    named_type: check.ConstStore.NamedType,
    kind: ?check.ConstStore.TypeNamedKind,
) void {
    if (std.meta.eql(named_type.ty_module.bytes, previous.key.bytes)) {
        base.invariant("a transplanted named type's checked occurrence was in the version of its module that stored it", .{});
    }
    const owner = if (std.meta.eql(named_type.ty_module.bytes, module.key.bytes)) module else modules.find(modules.context, named_type.ty_module.bytes) orelse
        base.invariant("a transplanted named type's checked occurrence named a module outside the compilation", .{});
    const types = owner.checked_types.view();
    if (@backingInt(named_type.ty) >= types.payloadCount()) {
        base.invariant("a transplanted named type's checked occurrence was out of range", .{});
    }
    switch (types.payload(named_type.ty)) {
        .nominal => |nominal| if (!std.meta.eql(nominal.owner_module.bytes, named_type.module.bytes)) {
            base.invariant("a transplanted named type's checked occurrence named another module's nominal type", .{});
        },
        .alias => if (kind != .alias) base.invariant("a transplanted nominal type's checked occurrence was an alias", .{}),
        .pending, .err, .flex, .rigid, .record, .tuple, .function, .empty_record, .tag_union, .empty_tag_union => base.invariant("a transplanted named type's checked occurrence was not a named type", .{}),
    }
}

fn sameEvaluation(
    module: *checked.CheckedModuleArtifact,
    previous: *const Previous,
    transplant: *const Transplant,
    match: Match,
    fresh_root: checked.CompileTimeRoot,
    copy: Copy,
) Allocator.Error!bool {
    const fresh = fresh_root.payload;
    if (std.meta.activeTag(fresh) != std.meta.activeTag(copy.payload)) return false;
    switch (fresh) {
        .const_node => |node| if (!try module.const_store.graphEql(node, &module.const_store, copy.payload.const_node)) return false,
        .unevaluated => {
            const fresh_stored = fresh_root.boxy_value orelse return false;
            const copied = copy.boxy_value orelse return false;
            if (!try module.const_store.graphEql(fresh_stored.node, &module.const_store, copied.node)) return false;
            if (!try module.const_store.type_store.graphEql(&module.canonical_names, fresh_stored.root_type, &module.const_store.type_store, &module.canonical_names, copied.root_type)) return false;
        },
        .discarded => {},
        .pending, .fn_value, .expect, .runtime => return false,
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
        if (index >= fresh_messages.items.len or !sameBytes(fresh_messages.items[index], previous.debug.message(entry))) return false;
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
        if (!std.meta.eql(a.module, b.module) or a.root != b.root) return false;
    }
    return true;
}

fn uniqueRefs(refs: []checked.CompileTimeRootReadTable.Ref) []checked.CompileTimeRootReadTable.Ref {
    var len: usize = 0;
    for (refs) |ref| {
        if (len != 0 and std.meta.eql(refs[len - 1].module, ref.module) and refs[len - 1].root == ref.root) continue;
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
        .hoisted_validation, .callable_binding, .expect, .numeral_conversion, .quote_conversion, .repl_expr, .literal_argument => base.invariant("compile-time root kind has no constant template", .{}),
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
                .numeral_conversion, .quote_conversion, .hoisted_validation, .literal_argument => try module.const_store.valuePortable(node, own),
                .constant, .hoisted_constant => try module.const_store.valuePortable(node, own) and
                    try module.const_store.type_store.typePortable(&module.canonical_names, storedTemplateType(module, root), own, own_identity),
            },
        };
        flag.* = @intFromBool(portable);
    }
    if (module.freesReplacedEvaluationColumns()) module.compile_time_root_portability.deinit(artifact_allocator);
    module.compile_time_root_portability = .{ .portable = flags };
}

/// Whether two byte strings stored in different stores hold the same bytes.
fn sameBytes(a: []const u8, b: []const u8) bool {
    if (a.len != b.len) return false;
    for (a, b) |left, right| {
        if (left != right) return false;
    }
    return true;
}
