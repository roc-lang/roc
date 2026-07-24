//! Monotype specialization reservation and reuse.
//!
//! `SpecBuilder` is the single index that answers every specialization reuse
//! question in the Monotype pass. A specialization's identity — its callable,
//! its checked source function type digest, and the closed monomorphic
//! function type REQUESTED by the call site that reserved it, and the checked
//! method lookup scope used by generated dispatch — is written once at
//! `reserve` and never rewritten. Refinements produced later (a deferred
//! request type sealed by its requester's graph, or the solved type produced
//! by lowering the body) are data on the `SpecRecord` and become *alias*
//! lookup entries pointing at the same record, never a rekey. A request whose
//! type is less specific than a record's solved type therefore reuses the
//! record through the request-shaped alias; it never widens the record
//! (`design.md`'s one-way snapshot rule).
//!
//! Lookup is a digest-keyed hash map with exact structural type equality as
//! the collision authority, so reuse checks stay O(1) expected with zero
//! SHA-256 recomputation on the lookup path.
//!
//! Boundary notes: the generated structural-derivation helper defs in
//! `lower.zig` (`inspect_defs`/`equality_defs`/`hash_defs`) are def memos keyed
//! by process-local type ids — they have no `FnId` and are not specialization
//! records, so they deliberately live outside this index. Likewise
//! `monotype_lifted/spec_constr.zig` specializes on call-pattern *value*
//! shape, not type, and owns its own separate identity space.

const std = @import("std");
const builtin = @import("builtin");
const check = @import("check");
const Ast = @import("ast.zig");
const Type = @import("type.zig");

const names = check.CheckedNames;

/// Deterministic counters used by specialization-shape tests.
pub const Counters = struct {
    template_requests: u64 = 0,
    template_hits: u64 = 0,
    template_misses: u64 = 0,
    nested_requests: u64 = 0,
    nested_hits: u64 = 0,
    nested_misses: u64 = 0,
    template_lookup_candidates: u64 = 0,
    nested_lookup_candidates: u64 = 0,
    specialization_type_digest_requests: u64 = 0,
    specialization_type_digest_cache_hits: u64 = 0,
    specialization_type_digest_cache_misses: u64 = 0,
    specialization_type_digest_nodes_visited: u64 = 0,
    exact_type_checks: u64 = 0,
    /// Declaration-backed nominal backings served from the per-graph
    /// instantiation cache. Reuse compares argument cells by union-find root,
    /// so instantiation counts must stay flat as unification merges argument
    /// cells (issue #10128).
    nominal_backing_reuses: u64 = 0,
    nominal_backing_instantiations: u64 = 0,
    /// Generated-evidence discovery walks, and how many were answered by the
    /// generation-validated memo instead of re-walking the type.
    evidence_walks: u64 = 0,
    evidence_walk_memo_hits: u64 = 0,
};

/// Result of reserving or reusing a specialization record.
pub const ReserveResult = struct {
    spec: ?Ast.SpecId,
    target: Ast.FnSlot,
    created: bool,
};

/// Existing local specialization found by a lookup.
pub const LocalHit = struct {
    spec: Ast.SpecId,
    fn_id: Ast.FnId,
    status: Ast.SpecStatus,
    /// The record view (current request or solved type) that structurally
    /// matched the requested type. Callers unify their request with this view.
    match_ty: Type.TypeId,
    /// The record's solved view; equals the request view until `.ready`.
    solved_fn_ty: Type.TypeId,
};

/// Existing specialization found by a lookup: either a record lowered in this
/// shard or a ready record loaded from another shard's specialization cache.
pub const LookupResult = union(enum(u8)) {
    local: LocalHit,
    loaded: Ast.ImportedFnId,

    pub fn target(self: LookupResult) Ast.FnSlot {
        return switch (self) {
            .local => |hit| .{ .local = hit.fn_id },
            .loaded => |imported| .{ .imported = imported },
        };
    }
};

const LoadedSpecId = enum(u32) { _ };

const LoadedSpec = struct {
    record: Ast.SpecRecord,
    types: Type.DurableView,
    imported: Ast.ImportedFnId,
};

const SpecEntryId = union(enum(u8)) {
    local: Ast.SpecId,
    loaded: LoadedSpecId,
};

/// Flat, hash-map-friendly image of one lookup key: the callable identity,
/// the checked method lookup scope, the source function type digest, and one
/// closed monomorphic function type digest (a record is reachable under its
/// requested digest and, once ready with a different solved type, under its
/// solved digest as an alias).
const SpecLookupAddress = struct {
    callable_kind: u8,
    module_bytes: [32]u8,
    index_a: u32,
    index_b: u32,
    index_c: u32,
    owner_fn_digest: [32]u8,
    method_scope_bytes: [32]u8,
    source_digest: [32]u8,
    type_digest: [32]u8,

    fn from(
        callable: Ast.CallableIdentity,
        method_scope: names.CheckedModuleDigest,
        source_digest: names.TypeDigest,
        type_digest: names.TypeDigest,
    ) SpecLookupAddress {
        var key: SpecLookupAddress = .{
            .callable_kind = @intFromEnum(callable),
            .module_bytes = @splat(0),
            .index_a = 0,
            .index_b = 0,
            .index_c = 0,
            .owner_fn_digest = @splat(0),
            .method_scope_bytes = method_scope.bytes,
            .source_digest = source_digest.bytes,
            .type_digest = type_digest.bytes,
        };
        switch (callable) {
            .proc_template => |template| {
                key.module_bytes = template.module.bytes;
                key.index_a = template.proc_base;
                key.index_b = template.template;
            },
            .nested_site => |site| {
                key.module_bytes = site.module.bytes;
                key.index_a = site.owner_proc_base;
                key.index_b = site.owner_template;
                key.index_c = site.site;
                key.owner_fn_digest = site.owner_fn_digest.bytes;
            },
            .hosted => |hosted| {
                key.index_a = @intFromEnum(hosted);
            },
            .generated => |generated| {
                key.index_a = @intFromEnum(generated);
            },
        }
        return key;
    }
};

/// Which lookup entries a search may return.
const FindScope = enum {
    local_only,
    local_and_loaded,
};

/// Track creation-time identities and refinement history in debug builds so
/// the validator can prove no record's identity was rewritten after `reserve`
/// and that every lookup entry belongs to some record's identity history.
const identity_shadow_enabled = builtin.mode == .Debug;

/// One request-view refinement remembered by the debug shadow. A record can
/// be refined more than once while `.reserved` (each deferring graph seals its
/// own view of the request), and each refinement's alias entry stays in the
/// lookup map, so the validator needs the full digest history.
const RefinedDigestShadow = struct {
    spec: Ast.SpecId,
    digest: names.TypeDigest,
};

/// Direct specialization reservation table keyed by callable identity, source
/// type digest, and closed requested (or solved-alias) type digest.
pub const SpecBuilder = struct {
    const RecordList = Ast.ProgramList(Ast.SpecRecord, "specs");
    allocator: std.mem.Allocator,
    names: *const names.NameStore,
    types: *const Type.Store,
    records: *RecordList,
    loaded_records: std.ArrayList(LoadedSpec),
    lookup: std.AutoHashMap(SpecLookupAddress, std.ArrayList(SpecEntryId)),
    counters: ?*Counters,
    reserved_identities: if (identity_shadow_enabled) std.ArrayList(Ast.SpecIdentity) else void,
    refined_digest_shadow: if (identity_shadow_enabled) std.ArrayList(RefinedDigestShadow) else void,

    pub fn init(
        allocator: std.mem.Allocator,
        name_store: *const names.NameStore,
        type_store: *const Type.Store,
        records: *RecordList,
    ) SpecBuilder {
        return .{
            .allocator = allocator,
            .names = name_store,
            .types = type_store,
            .records = records,
            .loaded_records = .empty,
            .lookup = std.AutoHashMap(SpecLookupAddress, std.ArrayList(SpecEntryId)).init(allocator),
            .counters = null,
            .reserved_identities = if (identity_shadow_enabled) .empty else {},
            .refined_digest_shadow = if (identity_shadow_enabled) .empty else {},
        };
    }

    pub fn deinit(self: *SpecBuilder) void {
        if (identity_shadow_enabled) {
            self.refined_digest_shadow.deinit(self.allocator);
            self.reserved_identities.deinit(self.allocator);
        }
        var lists = self.lookup.valueIterator();
        while (lists.next()) |list| list.deinit(self.allocator);
        self.lookup.deinit();
        self.loaded_records.deinit(self.allocator);
    }

    /// Index a ready record loaded from another shard's specialization cache.
    /// Loaded records are reachable only at their SOLVED shape: a loaded
    /// record is a finished snapshot, and a requester that matches it embeds
    /// the matched type directly. A request at the record's (less specific)
    /// requested shape has no way to adopt the snapshot's solved evidence
    /// into its graph, so it lowers a local specialization instead — the same
    /// join the pre-immutability cache implemented by serializing identities
    /// already rewritten to the solved type.
    pub fn insertLoadedReady(
        self: *SpecBuilder,
        record: Ast.SpecRecord,
        types: Type.DurableView,
        imported: Ast.ImportedFnId,
    ) std.mem.Allocator.Error!LoadedSpecId {
        if (record.status != .ready) invariant("loaded Monotype specialization record was not ready");

        const loaded_id: LoadedSpecId = @enumFromInt(@as(u32, @intCast(self.loaded_records.items.len)));
        try self.loaded_records.append(self.allocator, .{
            .record = record,
            .types = types,
            .imported = imported,
        });
        errdefer _ = self.loaded_records.pop();

        const address = SpecLookupAddress.from(record.identity.callable, record.identity.method_scope, record.identity.source_fn_ty_digest, record.solved_fn_ty_digest);
        const gop = try self.lookup.getOrPut(address);
        if (!gop.found_existing) gop.value_ptr.* = .empty;
        try gop.value_ptr.append(self.allocator, .{ .loaded = loaded_id });
        return loaded_id;
    }

    /// Reserve a fresh record for `identity`, or return the existing local or
    /// loaded specialization it matches. A fresh record starts `.reserved`
    /// with both type views mirroring the requested type. The whole
    /// find-or-create runs against one lookup bucket, probed once.
    pub fn reserve(
        self: *SpecBuilder,
        identity: Ast.SpecIdentity,
        fn_id: Ast.FnId,
    ) std.mem.Allocator.Error!ReserveResult {
        const address = SpecLookupAddress.from(identity.callable, identity.method_scope, identity.source_fn_ty_digest, identity.request_fn_ty_digest);
        const gop = try self.lookup.getOrPut(address);
        if (!gop.found_existing) gop.value_ptr.* = .empty;

        if (try self.matchInBucket(gop.value_ptr.items, identity, .local_and_loaded)) |hit| {
            return .{
                .spec = switch (hit) {
                    .local => |local| local.spec,
                    .loaded => null,
                },
                .target = hit.target(),
                .created = false,
            };
        }

        const spec_id: Ast.SpecId = @enumFromInt(@as(u32, @intCast(self.records.len())));
        try self.records.append(self.allocator, .{
            .identity = identity,
            .request_fn_ty = identity.request_fn_ty,
            .request_fn_ty_digest = identity.request_fn_ty_digest,
            .solved_fn_ty = identity.request_fn_ty,
            .solved_fn_ty_digest = identity.request_fn_ty_digest,
            .fn_id = fn_id,
            .status = .reserved,
        });
        errdefer _ = self.records.pop();
        if (identity_shadow_enabled) {
            try self.reserved_identities.append(self.allocator, identity);
        }
        errdefer if (identity_shadow_enabled) {
            _ = self.reserved_identities.pop();
        };
        try gop.value_ptr.append(self.allocator, .{ .local = spec_id });
        return .{
            .spec = spec_id,
            .target = .{ .local = fn_id },
            .created = true,
        };
    }

    /// Find the specialization a request at `identity` should reuse. Local
    /// records lowered in this shard win over records loaded from other
    /// shards' caches; request-view matches win over solved-view aliases.
    pub fn find(self: *SpecBuilder, identity: Ast.SpecIdentity) std.mem.Allocator.Error!?LookupResult {
        return try self.findInScope(identity, .local_and_loaded);
    }

    /// Find like `find`, but only consider records lowered in this shard.
    /// Body-lowering paths that must produce a local definition use this.
    pub fn findLocal(self: *SpecBuilder, identity: Ast.SpecIdentity) std.mem.Allocator.Error!?LocalHit {
        const hit = (try self.findInScope(identity, .local_only)) orelse return null;
        return switch (hit) {
            .local => |local| local,
            .loaded => invariant("Monotype local specialization lookup returned a loaded record"),
        };
    }

    fn findInScope(
        self: *SpecBuilder,
        identity: Ast.SpecIdentity,
        scope: FindScope,
    ) std.mem.Allocator.Error!?LookupResult {
        const address = SpecLookupAddress.from(identity.callable, identity.method_scope, identity.source_fn_ty_digest, identity.request_fn_ty_digest);
        const entries = self.lookup.get(address) orelse return null;
        self.countCandidatesBy(identity.callable, entries.items.len);
        return try self.matchInBucket(entries.items, identity, scope);
    }

    /// Match `identity` against one lookup bucket. Request-view matches win
    /// over solved-view aliases, and local records win over loaded ones, so
    /// the bucket is scanned in that priority order (which also defers the
    /// more expensive cross-store comparisons until no local record matched).
    /// Every entry in the bucket already agrees on callable, source digest,
    /// and type digest by address equality; only the view check and the exact
    /// structural equality collision authority remain.
    fn matchInBucket(
        self: *SpecBuilder,
        entries: []const SpecEntryId,
        identity: Ast.SpecIdentity,
        scope: FindScope,
    ) std.mem.Allocator.Error!?LookupResult {
        for (entries) |entry_id| {
            const local_spec = switch (entry_id) {
                .local => |spec_id| spec_id,
                .loaded => continue,
            };
            const record = self.recordPtr(local_spec);
            if (!try self.localViewMatches(record.request_fn_ty, record.request_fn_ty_digest, identity)) continue;
            return localResult(local_spec, record, record.request_fn_ty);
        }
        for (entries) |entry_id| {
            const local_spec = switch (entry_id) {
                .local => |spec_id| spec_id,
                .loaded => continue,
            };
            const record = self.recordPtr(local_spec);
            if (record.status != .ready) continue;
            if (!try self.localViewMatches(record.solved_fn_ty, record.solved_fn_ty_digest, identity)) continue;
            return localResult(local_spec, record, record.solved_fn_ty);
        }
        if (scope == .local_only) return null;
        for (entries) |entry_id| {
            const loaded_id = switch (entry_id) {
                .local => continue,
                .loaded => |loaded_id| loaded_id,
            };
            const loaded = &self.loaded_records.items[@intFromEnum(loaded_id)];
            if (!digestEql(loaded.record.solved_fn_ty_digest, identity.request_fn_ty_digest)) continue;
            self.countExactTypeCheck();
            if (!try Type.typeEqlAcrossStores(
                self.allocator,
                self.names,
                self.types.view(),
                identity.request_fn_ty,
                loaded.types,
                loaded.record.solved_fn_ty,
            )) continue;
            return .{ .loaded = loaded.imported };
        }
        return null;
    }

    fn localViewMatches(
        self: *SpecBuilder,
        view_ty: Type.TypeId,
        view_digest: names.TypeDigest,
        identity: Ast.SpecIdentity,
    ) std.mem.Allocator.Error!bool {
        if (!digestEql(view_digest, identity.request_fn_ty_digest)) return false;
        if (view_ty == identity.request_fn_ty) return true;
        self.countExactTypeCheck();
        return try self.types.typeEql(self.names, view_ty, identity.request_fn_ty);
    }

    /// Refine a still-`.reserved` record's request view after a requester's
    /// graph sealed the deferred request type. The identity keeps the
    /// creation-time request; the sealed shape becomes the record's current
    /// request view plus an alias lookup entry. This can happen more than
    /// once while the record stays `.reserved` — each graph that deferred a
    /// request against the record seals its own view — and each superseded
    /// shape's entry stays inert in the map: it no longer matches (only the
    /// owning graphs could produce those shapes), and a later independent
    /// request at a superseded shape reserves its own record.
    pub fn refineRequest(
        self: *SpecBuilder,
        spec: Ast.SpecId,
        request_fn_ty: Type.TypeId,
        request_fn_ty_digest: names.TypeDigest,
    ) std.mem.Allocator.Error!void {
        const record = self.recordPtr(spec);
        if (record.status != .reserved) {
            invariant("Monotype specialization request was refined after lowering began");
        }
        if (digestEql(record.request_fn_ty_digest, request_fn_ty_digest) and
            record.request_fn_ty == request_fn_ty)
        {
            return;
        }
        const digest_changed = !digestEql(record.request_fn_ty_digest, request_fn_ty_digest);
        record.request_fn_ty = request_fn_ty;
        record.request_fn_ty_digest = request_fn_ty_digest;
        record.solved_fn_ty = request_fn_ty;
        record.solved_fn_ty_digest = request_fn_ty_digest;
        if (digest_changed) {
            if (identity_shadow_enabled) {
                try self.refined_digest_shadow.append(self.allocator, .{ .spec = spec, .digest = request_fn_ty_digest });
            }
            try self.appendAliasEntry(record.identity.callable, record.identity.method_scope, record.identity.source_fn_ty_digest, request_fn_ty_digest, spec);
        }
    }

    pub fn markLowering(self: *SpecBuilder, spec: Ast.SpecId) void {
        const record = self.recordPtr(spec);
        if (record.status != .reserved) {
            invariant("Monotype specialization began lowering from a non-reserved status");
        }
        record.status = .lowering;
    }

    /// Complete a record with the solved type its body evidence produced. If
    /// the solved digest differs from the request digest, the solved shape
    /// becomes an alias lookup entry; the record is never rekeyed. The
    /// function id was fixed at `reserve` and never changes.
    pub fn markReady(
        self: *SpecBuilder,
        spec: Ast.SpecId,
        solved_fn_ty: Type.TypeId,
        solved_fn_ty_digest: names.TypeDigest,
    ) std.mem.Allocator.Error!void {
        const record = self.recordPtr(spec);
        if (record.status != .lowering) {
            invariant("Monotype specialization was marked ready without lowering");
        }
        record.solved_fn_ty = solved_fn_ty;
        record.solved_fn_ty_digest = solved_fn_ty_digest;
        record.status = .ready;
        if (!digestEql(solved_fn_ty_digest, record.request_fn_ty_digest)) {
            try self.appendAliasEntry(record.identity.callable, record.identity.method_scope, record.identity.source_fn_ty_digest, solved_fn_ty_digest, spec);
        }
    }

    /// Register an additional shape for an existing record. The shape may
    /// coincide with one the record already occupies (a body can solve back
    /// to a superseded request shape), so insertion deduplicates.
    fn appendAliasEntry(
        self: *SpecBuilder,
        callable: Ast.CallableIdentity,
        method_scope: names.CheckedModuleDigest,
        source_digest: names.TypeDigest,
        type_digest: names.TypeDigest,
        spec: Ast.SpecId,
    ) std.mem.Allocator.Error!void {
        const address = SpecLookupAddress.from(callable, method_scope, source_digest, type_digest);
        const gop = try self.lookup.getOrPut(address);
        if (!gop.found_existing) gop.value_ptr.* = .empty;
        for (gop.value_ptr.items) |existing| {
            switch (existing) {
                .local => |existing_spec| if (existing_spec == spec) return,
                .loaded => {},
            }
        }
        try gop.value_ptr.append(self.allocator, .{ .local = spec });
    }

    fn recordPtr(self: *SpecBuilder, spec: Ast.SpecId) *Ast.SpecRecord {
        const index = @intFromEnum(spec);
        if (index >= self.records.len()) invariant("Monotype spec builder referenced a missing record");
        return self.records.getPtrImmediate(index);
    }

    fn countCandidatesBy(self: *SpecBuilder, callable: Ast.CallableIdentity, amount: usize) void {
        const counters = self.counters orelse return;
        switch (callable) {
            .nested_site => counters.nested_lookup_candidates += @intCast(amount),
            .proc_template,
            .hosted,
            .generated,
            => counters.template_lookup_candidates += @intCast(amount),
        }
    }

    fn countExactTypeCheck(self: *SpecBuilder) void {
        const counters = self.counters orelse return;
        counters.exact_type_checks += 1;
    }

    /// Integrity defects `lookupIntegrityError` can detect.
    pub const IntegrityError = enum {
        identity_shadow_diverged,
        identity_rewritten_after_reserve,
        record_reachable_from_foreign_key,
        record_missing_expected_key,
    };

    /// Debug-only integrity validator: every local record must be reachable
    /// from exactly the keys its immutable identity and refinement history
    /// produce, from no others, and its identity must equal the identity
    /// captured when it was reserved. Compiled out of release builds.
    pub fn validateLookupIntegrity(self: *const SpecBuilder) void {
        if (comptime identity_shadow_enabled) {
            if (self.lookupIntegrityError()) |err| switch (err) {
                .identity_shadow_diverged => invariant("Monotype spec builder identity shadow diverged from the record table"),
                .identity_rewritten_after_reserve => invariant("Monotype specialization identity was rewritten after reserve"),
                .record_reachable_from_foreign_key => invariant("Monotype specialization record was reachable from a key outside its identity history"),
                .record_missing_expected_key => invariant("Monotype specialization record was not reachable from every key in its identity history"),
            };
        }
    }

    /// Find the first lookup-integrity defect, if any. Only meaningful in
    /// debug builds, where the creation-time identity shadow exists.
    pub fn lookupIntegrityError(self: *const SpecBuilder) ?IntegrityError {
        if (comptime !identity_shadow_enabled) return null;
        return self.lookupIntegrityErrorDebug();
    }

    fn lookupIntegrityErrorDebug(self: *const SpecBuilder) ?IntegrityError {
        if (self.reserved_identities.items.len != self.records.len()) {
            return .identity_shadow_diverged;
        }
        for (0..self.records.len(), self.reserved_identities.items) |index, reserved_identity| {
            const record = self.records.get(index);
            if (!identityEql(record.identity, reserved_identity)) {
                return .identity_rewritten_after_reserve;
            }
        }

        // Forward: every record must be reachable from each address its
        // history requires (creation identity, every refinement, the current
        // request view, and the solved view once ready).
        for (0..self.records.len()) |index| {
            const record = self.records.get(index);
            const spec: Ast.SpecId = @enumFromInt(@as(u32, @intCast(index)));
            if (!self.recordReachableAt(spec, record, record.identity.request_fn_ty_digest)) {
                return .record_missing_expected_key;
            }
            if (!self.recordReachableAt(spec, record, record.request_fn_ty_digest)) {
                return .record_missing_expected_key;
            }
            if (record.status == .ready and !self.recordReachableAt(spec, record, record.solved_fn_ty_digest)) {
                return .record_missing_expected_key;
            }
            for (self.refined_digest_shadow.items) |refined| {
                if (refined.spec != spec) continue;
                if (!self.recordReachableAt(spec, record, refined.digest)) {
                    return .record_missing_expected_key;
                }
            }
        }

        // Reverse: every lookup entry must be an address the record's history
        // produced, and no address may list the same record twice.
        var iterator = self.lookup.iterator();
        while (iterator.next()) |entry| {
            for (entry.value_ptr.items, 0..) |entry_id, entry_index| {
                const spec_id = switch (entry_id) {
                    .local => |spec_id| spec_id,
                    .loaded => continue,
                };
                const record = self.records.get(@intFromEnum(spec_id));
                if (!self.addressInRecordHistory(spec_id, record, entry.key_ptr.*)) {
                    return .record_reachable_from_foreign_key;
                }
                for (entry.value_ptr.items[entry_index + 1 ..]) |later| {
                    switch (later) {
                        .local => |later_spec| if (later_spec == spec_id) return .record_reachable_from_foreign_key,
                        .loaded => {},
                    }
                }
            }
        }
        return null;
    }

    fn recordReachableAt(self: *const SpecBuilder, spec: Ast.SpecId, record: Ast.SpecRecord, digest: names.TypeDigest) bool {
        const address = SpecLookupAddress.from(record.identity.callable, record.identity.method_scope, record.identity.source_fn_ty_digest, digest);
        const entries = self.lookup.get(address) orelse return false;
        for (entries.items) |entry_id| {
            switch (entry_id) {
                .local => |entry_spec| if (entry_spec == spec) return true,
                .loaded => {},
            }
        }
        return false;
    }

    fn addressInRecordHistory(self: *const SpecBuilder, spec: Ast.SpecId, record: Ast.SpecRecord, address: SpecLookupAddress) bool {
        const callable = record.identity.callable;
        const method_scope = record.identity.method_scope;
        const source_digest = record.identity.source_fn_ty_digest;
        if (std.meta.eql(address, SpecLookupAddress.from(callable, method_scope, source_digest, record.identity.request_fn_ty_digest))) return true;
        if (std.meta.eql(address, SpecLookupAddress.from(callable, method_scope, source_digest, record.request_fn_ty_digest))) return true;
        if (record.status == .ready and std.meta.eql(address, SpecLookupAddress.from(callable, method_scope, source_digest, record.solved_fn_ty_digest))) return true;
        for (self.refined_digest_shadow.items) |refined| {
            if (refined.spec != spec) continue;
            if (std.meta.eql(address, SpecLookupAddress.from(callable, method_scope, source_digest, refined.digest))) return true;
        }
        return false;
    }
};

fn localResult(spec: Ast.SpecId, record: *const Ast.SpecRecord, match_ty: Type.TypeId) LookupResult {
    return .{ .local = .{
        .spec = spec,
        .fn_id = record.fn_id,
        .status = record.status,
        .match_ty = match_ty,
        .solved_fn_ty = record.solved_fn_ty,
    } };
}

fn identityEql(left: Ast.SpecIdentity, right: Ast.SpecIdentity) bool {
    return std.meta.eql(left.callable, right.callable) and
        std.mem.eql(u8, left.method_scope.bytes[0..], right.method_scope.bytes[0..]) and
        digestEql(left.source_fn_ty_digest, right.source_fn_ty_digest) and
        digestEql(left.request_fn_ty_digest, right.request_fn_ty_digest) and
        left.request_fn_ty == right.request_fn_ty;
}

fn digestEql(left: names.TypeDigest, right: names.TypeDigest) bool {
    return std.mem.eql(u8, left.bytes[0..], right.bytes[0..]);
}

fn invariant(comptime message: []const u8) noreturn {
    @import("../common.zig").invariant(message);
}

test "monotype specialize declarations are referenced" {
    std.testing.refAllDecls(@This());
}

test "monotype spec builder reuses exact specialization identities" {
    var name_store = names.NameStore.init(std.testing.allocator);
    defer name_store.deinit();

    var type_store = Type.Store.init(std.testing.allocator);
    defer type_store.deinit();

    const unit_ty = try type_store.add(.zst);
    const identity = testSpecIdentity(unit_ty, digestWithFirstByte(1), digestWithFirstByte(2));

    var records = Ast.ProgramList(Ast.SpecRecord, "specs").empty;
    defer records.deinit(std.testing.allocator);

    var builder = SpecBuilder.init(std.testing.allocator, &name_store, &type_store, &records);
    defer builder.deinit();

    const requested_fn: Ast.FnId = @enumFromInt(1);
    const duplicate_request_fn: Ast.FnId = @enumFromInt(2);
    try std.testing.expectEqual(@as(?LookupResult, null), try builder.find(identity));

    const first = try builder.reserve(identity, requested_fn);
    const second = try builder.reserve(identity, duplicate_request_fn);

    try std.testing.expect(first.created);
    try std.testing.expect(!second.created);
    try std.testing.expectEqual(first.spec, second.spec);
    try std.testing.expectEqual(Ast.FnSlot{ .local = requested_fn }, first.target);
    try std.testing.expectEqual(Ast.FnSlot{ .local = requested_fn }, second.target);
    try std.testing.expectEqual(@as(usize, 1), builder.records.len());
    const found = (try builder.find(identity)) orelse return error.TestUnexpectedResult;
    try std.testing.expectEqual(first.spec, @as(?Ast.SpecId, found.local.spec));
    try std.testing.expectEqual(requested_fn, found.local.fn_id);

    const first_spec = first.spec orelse return error.TestUnexpectedResult;
    builder.markLowering(first_spec);
    try std.testing.expectEqual(Ast.SpecStatus.lowering, builder.records.get(@intFromEnum(first_spec)).status);
    try builder.markReady(first_spec, unit_ty, identity.request_fn_ty_digest);
    try std.testing.expectEqual(Ast.SpecStatus.ready, builder.records.get(@intFromEnum(first_spec)).status);
    try std.testing.expectEqual(requested_fn, builder.records.get(@intFromEnum(first_spec)).fn_id);
    builder.validateLookupIntegrity();
}

test "monotype spec builder keeps identity immutable and aliases the solved type" {
    var name_store = names.NameStore.init(std.testing.allocator);
    defer name_store.deinit();

    var type_store = Type.Store.init(std.testing.allocator);
    defer type_store.deinit();

    const request_ty = try type_store.add(.zst);
    const solved_ty = try type_store.add(.{ .primitive = .str });
    const source_digest = digestWithFirstByte(1);
    const request_identity = testSpecIdentity(request_ty, source_digest, digestWithFirstByte(2));
    const solved_digest = digestWithFirstByte(3);
    const solved_shaped_identity = testSpecIdentity(solved_ty, source_digest, solved_digest);

    var records = Ast.ProgramList(Ast.SpecRecord, "specs").empty;
    defer records.deinit(std.testing.allocator);

    var builder = SpecBuilder.init(std.testing.allocator, &name_store, &type_store, &records);
    defer builder.deinit();

    const reserved = try builder.reserve(request_identity, @enumFromInt(1));
    const spec = reserved.spec orelse return error.TestUnexpectedResult;
    builder.markLowering(spec);

    // Before the record is ready, a solved-shaped request must not match.
    try std.testing.expectEqual(@as(?LookupResult, null), try builder.find(solved_shaped_identity));

    try builder.markReady(spec, solved_ty, solved_digest);

    // The identity still records the requested type; only the record data
    // carries the solved view.
    const record = builder.records.get(@intFromEnum(spec));
    try std.testing.expectEqual(request_ty, record.identity.request_fn_ty);
    try std.testing.expectEqual(solved_ty, record.solved_fn_ty);

    // A solved-shaped request reuses the record through the alias entry, and
    // the original request shape (less specific than the solved type) still
    // reuses it through the request entry — the record is never rekeyed.
    const solved_found = (try builder.find(solved_shaped_identity)) orelse return error.TestUnexpectedResult;
    try std.testing.expectEqual(spec, solved_found.local.spec);
    try std.testing.expectEqual(solved_ty, solved_found.local.match_ty);
    const request_found = (try builder.find(request_identity)) orelse return error.TestUnexpectedResult;
    try std.testing.expectEqual(spec, request_found.local.spec);
    try std.testing.expectEqual(request_ty, request_found.local.match_ty);
    try std.testing.expectEqual(solved_ty, request_found.local.solved_fn_ty);

    const repeated = try builder.reserve(solved_shaped_identity, @enumFromInt(2));
    try std.testing.expect(!repeated.created);
    try std.testing.expectEqual(@as(?Ast.SpecId, spec), repeated.spec);
    try std.testing.expectEqual(@as(usize, 1), records.len());
    builder.validateLookupIntegrity();
}

test "monotype spec builder refines a reserved request through an alias entry" {
    var name_store = names.NameStore.init(std.testing.allocator);
    defer name_store.deinit();

    var type_store = Type.Store.init(std.testing.allocator);
    defer type_store.deinit();

    const request_ty = try type_store.add(.zst);
    const sealed_ty = try type_store.add(.{ .primitive = .str });
    const source_digest = digestWithFirstByte(1);
    const request_identity = testSpecIdentity(request_ty, source_digest, digestWithFirstByte(2));
    const sealed_digest = digestWithFirstByte(3);
    const sealed_identity = testSpecIdentity(sealed_ty, source_digest, sealed_digest);

    var records = Ast.ProgramList(Ast.SpecRecord, "specs").empty;
    defer records.deinit(std.testing.allocator);

    var builder = SpecBuilder.init(std.testing.allocator, &name_store, &type_store, &records);
    defer builder.deinit();

    const reserved = try builder.reserve(request_identity, @enumFromInt(1));
    const spec = reserved.spec orelse return error.TestUnexpectedResult;

    try builder.refineRequest(spec, sealed_ty, sealed_digest);

    // The identity still records the creation-time request; the record's
    // request view carries the sealed shape.
    const record = builder.records.get(@intFromEnum(spec));
    try std.testing.expectEqual(request_ty, record.identity.request_fn_ty);
    try std.testing.expectEqual(sealed_ty, record.request_fn_ty);
    try std.testing.expectEqual(Ast.SpecStatus.reserved, record.status);

    // The sealed shape finds the record; the pre-seal shape's entry is inert.
    const sealed_found = (try builder.find(sealed_identity)) orelse return error.TestUnexpectedResult;
    try std.testing.expectEqual(spec, sealed_found.local.spec);
    try std.testing.expectEqual(@as(?LookupResult, null), try builder.find(request_identity));

    const repeated = try builder.reserve(sealed_identity, @enumFromInt(2));
    try std.testing.expect(!repeated.created);
    try std.testing.expectEqual(@as(?Ast.SpecId, spec), repeated.spec);
    try std.testing.expectEqual(@as(usize, 1), records.len());
    builder.validateLookupIntegrity();
}

test "monotype spec builder keeps checked module boundary in callable identity" {
    var name_store = names.NameStore.init(std.testing.allocator);
    defer name_store.deinit();

    var type_store = Type.Store.init(std.testing.allocator);
    defer type_store.deinit();

    const unit_ty = try type_store.add(.zst);
    const source_digest = digestWithFirstByte(1);
    const request_digest = digestWithFirstByte(2);

    var records = Ast.ProgramList(Ast.SpecRecord, "specs").empty;
    defer records.deinit(std.testing.allocator);

    var builder = SpecBuilder.init(std.testing.allocator, &name_store, &type_store, &records);
    defer builder.deinit();

    const first_module = testSpecIdentityWithModule(unit_ty, moduleDigestWithFirstByte(1), source_digest, request_digest);
    const second_module = testSpecIdentityWithModule(unit_ty, moduleDigestWithFirstByte(2), source_digest, request_digest);

    const first = try builder.reserve(first_module, @enumFromInt(1));
    const second = try builder.reserve(second_module, @enumFromInt(2));
    const repeated_first = try builder.reserve(first_module, @enumFromInt(3));

    try std.testing.expect(first.created);
    try std.testing.expect(second.created);
    try std.testing.expect(!repeated_first.created);
    try std.testing.expect(first.spec != second.spec);
    try std.testing.expectEqual(first.spec, repeated_first.spec);
    try std.testing.expectEqual(Ast.FnSlot{ .local = @enumFromInt(1) }, repeated_first.target);
    try std.testing.expectEqual(@as(usize, 2), builder.records.len());
    builder.validateLookupIntegrity();
}

test "monotype spec builder keeps method lookup scopes distinct" {
    var name_store = names.NameStore.init(std.testing.allocator);
    defer name_store.deinit();

    var type_store = Type.Store.init(std.testing.allocator);
    defer type_store.deinit();

    const unit_ty = try type_store.add(.zst);
    var first_identity = testSpecIdentity(unit_ty, digestWithFirstByte(1), digestWithFirstByte(2));
    first_identity.method_scope = moduleDigestWithFirstByte(3);
    var second_identity = first_identity;
    second_identity.method_scope = moduleDigestWithFirstByte(4);

    var records = Ast.ProgramList(Ast.SpecRecord, "specs").empty;
    defer records.deinit(std.testing.allocator);

    var builder = SpecBuilder.init(std.testing.allocator, &name_store, &type_store, &records);
    defer builder.deinit();

    const first = try builder.reserve(first_identity, @enumFromInt(1));
    const second = try builder.reserve(second_identity, @enumFromInt(2));
    const repeated_first = try builder.reserve(first_identity, @enumFromInt(3));

    try std.testing.expect(first.created);
    try std.testing.expect(second.created);
    try std.testing.expect(!repeated_first.created);
    try std.testing.expect(first.spec != second.spec);
    try std.testing.expectEqual(first.spec, repeated_first.spec);
    try std.testing.expectEqual(@as(usize, 2), builder.records.len());
    builder.validateLookupIntegrity();
}

test "monotype spec builder uses exact type equality after digest match" {
    var name_store = names.NameStore.init(std.testing.allocator);
    defer name_store.deinit();

    var type_store = Type.Store.init(std.testing.allocator);
    defer type_store.deinit();

    const module_identity = try name_store.internModuleIdentity(&([_]u8{0xCD} ** 32));
    const first_name = try name_store.internTypeName("First");
    const second_name = try name_store.internTypeName("Second");

    const first_ty = try type_store.add(.{ .named = .{
        .named_type = .{ .module = .{}, .ty = @enumFromInt(1) },
        .def = .{ .module = module_identity, .type_name = first_name },
        .kind = .alias,
        .args = Type.Span.empty(),
        .backing = null,
    } });
    const second_ty = try type_store.add(.{ .named = .{
        .named_type = .{ .module = .{}, .ty = @enumFromInt(2) },
        .def = .{ .module = module_identity, .type_name = second_name },
        .kind = .alias,
        .args = Type.Span.empty(),
        .backing = null,
    } });

    const forced_digest = digestWithFirstByte(9);
    var records = Ast.ProgramList(Ast.SpecRecord, "specs").empty;
    defer records.deinit(std.testing.allocator);

    var builder = SpecBuilder.init(std.testing.allocator, &name_store, &type_store, &records);
    defer builder.deinit();

    const first = try builder.reserve(testSpecIdentity(first_ty, digestWithFirstByte(1), forced_digest), @enumFromInt(1));
    const second = try builder.reserve(testSpecIdentity(second_ty, digestWithFirstByte(1), forced_digest), @enumFromInt(2));

    try std.testing.expect(first.created);
    try std.testing.expect(second.created);
    try std.testing.expect(first.spec != second.spec);
    try std.testing.expectEqual(@as(usize, 2), builder.records.len());
    builder.validateLookupIntegrity();
}

test "monotype spec builder reuses loaded records through exact cross-store type equality" {
    const allocator = std.testing.allocator;

    var name_store = names.NameStore.init(allocator);
    defer name_store.deinit();

    var current_types = Type.Store.init(allocator);
    defer current_types.deinit();
    var loaded_types = Type.Store.init(allocator);
    defer loaded_types.deinit();

    const current_unit = try current_types.add(.zst);
    const current_str = try current_types.add(.{ .primitive = .str });
    const loaded_str = try loaded_types.add(.{ .primitive = .str });
    const loaded_unit = try loaded_types.add(.zst);

    const loaded_view = loaded_types.view();
    const loaded_digests = try allocator.alloc(names.TypeDigest, loaded_view.types.len);
    defer allocator.free(loaded_digests);
    for (loaded_digests, 0..) |*digest, index| {
        digest.* = loaded_types.typeDigest(&name_store, @enumFromInt(@as(u32, @intCast(index))));
    }
    const loaded_durable = Type.DurableView{
        .types = loaded_view.types,
        .type_digests = loaded_digests,
        .spans = loaded_view.spans,
        .fields = loaded_view.fields,
        .tags = loaded_view.tags,
        .declared_fields = loaded_view.declared_fields,
    };

    const source_digest = digestWithFirstByte(1);
    const request_digest = digestWithFirstByte(2);
    const solved_digest = digestWithFirstByte(3);
    // The loaded record was requested at the unit shape and solved to str.
    var loaded_record = testSpecRecordReady(testSpecIdentity(loaded_unit, source_digest, request_digest), @enumFromInt(9));
    loaded_record.solved_fn_ty = loaded_str;
    loaded_record.solved_fn_ty_digest = solved_digest;

    var records = Ast.ProgramList(Ast.SpecRecord, "specs").empty;
    defer records.deinit(allocator);

    var builder = SpecBuilder.init(allocator, &name_store, &current_types, &records);
    defer builder.deinit();

    const imported: Ast.ImportedFnId = @enumFromInt(1);
    _ = try builder.insertLoadedReady(loaded_record, loaded_durable, imported);

    // A request shaped like the loaded record's SOLVED type reuses it: the
    // requester's type already equals the imported body's type.
    const solved_shaped = testSpecIdentity(current_str, source_digest, solved_digest);
    const solved_hit = (try builder.find(solved_shaped)) orelse return error.TestUnexpectedResult;
    try std.testing.expectEqual(Ast.FnSlot{ .imported = imported }, solved_hit.target());

    // A request shaped like the loaded record's (less specific) REQUEST type
    // must not reuse the snapshot — it has no way to adopt the solved
    // evidence — and lowers a fresh local specialization instead.
    const request_shaped = testSpecIdentity(current_unit, source_digest, request_digest);
    try std.testing.expectEqual(@as(?LookupResult, null), try builder.find(request_shaped));
    const local_miss = try builder.reserve(request_shaped, @enumFromInt(1));
    try std.testing.expect(local_miss.created);
    try std.testing.expectEqual(@as(usize, 1), builder.records.len());

    const solved_hit_again = try builder.reserve(solved_shaped, @enumFromInt(2));
    try std.testing.expect(!solved_hit_again.created);
    try std.testing.expectEqual(@as(?Ast.SpecId, null), solved_hit_again.spec);
    try std.testing.expectEqual(Ast.FnSlot{ .imported = imported }, solved_hit_again.target);
    builder.validateLookupIntegrity();
}

test "monotype spec builder rejects loaded records when exact cross-store type equality fails" {
    const allocator = std.testing.allocator;

    var name_store = names.NameStore.init(allocator);
    defer name_store.deinit();

    var current_types = Type.Store.init(allocator);
    defer current_types.deinit();
    var loaded_types = Type.Store.init(allocator);
    defer loaded_types.deinit();

    const current_unit = try current_types.add(.zst);
    const loaded_str = try loaded_types.add(.{ .primitive = .str });

    const loaded_view = loaded_types.view();
    const loaded_digests = try allocator.alloc(names.TypeDigest, loaded_view.types.len);
    defer allocator.free(loaded_digests);
    for (loaded_digests, 0..) |*digest, index| {
        digest.* = loaded_types.typeDigest(&name_store, @enumFromInt(@as(u32, @intCast(index))));
    }
    const loaded_durable = Type.DurableView{
        .types = loaded_view.types,
        .type_digests = loaded_digests,
        .spans = loaded_view.spans,
        .fields = loaded_view.fields,
        .tags = loaded_view.tags,
        .declared_fields = loaded_view.declared_fields,
    };

    const source_digest = digestWithFirstByte(1);
    const forced_request_digest = digestWithFirstByte(2);
    const current_identity = testSpecIdentity(current_unit, source_digest, forced_request_digest);
    const loaded_identity = testSpecIdentity(loaded_str, source_digest, forced_request_digest);

    var records = Ast.ProgramList(Ast.SpecRecord, "specs").empty;
    defer records.deinit(allocator);

    var builder = SpecBuilder.init(allocator, &name_store, &current_types, &records);
    defer builder.deinit();

    _ = try builder.insertLoadedReady(testSpecRecordReady(loaded_identity, @enumFromInt(9)), loaded_durable, @enumFromInt(1));

    try std.testing.expectEqual(@as(?LookupResult, null), try builder.find(current_identity));

    const miss = try builder.reserve(current_identity, @enumFromInt(1));
    try std.testing.expect(miss.created);
    try std.testing.expect(miss.spec != null);
    try std.testing.expectEqual(Ast.FnSlot{ .local = @as(Ast.FnId, @enumFromInt(1)) }, miss.target);
    try std.testing.expectEqual(@as(usize, 1), builder.records.len());
    builder.validateLookupIntegrity();
}

test "monotype spec builder prefers local records over loaded records" {
    const allocator = std.testing.allocator;

    var name_store = names.NameStore.init(allocator);
    defer name_store.deinit();

    var current_types = Type.Store.init(allocator);
    defer current_types.deinit();
    var loaded_types = Type.Store.init(allocator);
    defer loaded_types.deinit();

    const loaded_unit = try loaded_types.add(.zst);
    const current_str = try current_types.add(.{ .primitive = .str });
    const loaded_str = try loaded_types.add(.{ .primitive = .str });

    const loaded_view = loaded_types.view();
    const loaded_digests = try allocator.alloc(names.TypeDigest, loaded_view.types.len);
    defer allocator.free(loaded_digests);
    for (loaded_digests, 0..) |*digest, index| {
        digest.* = loaded_types.typeDigest(&name_store, @enumFromInt(@as(u32, @intCast(index))));
    }
    const loaded_durable = Type.DurableView{
        .types = loaded_view.types,
        .type_digests = loaded_digests,
        .spans = loaded_view.spans,
        .fields = loaded_view.fields,
        .tags = loaded_view.tags,
        .declared_fields = loaded_view.declared_fields,
    };

    const source_digest = digestWithFirstByte(1);
    const shared_solved_digest = digestWithFirstByte(9);

    var records = Ast.ProgramList(Ast.SpecRecord, "specs").empty;
    defer records.deinit(allocator);

    var builder = SpecBuilder.init(allocator, &name_store, &current_types, &records);
    defer builder.deinit();

    // A loaded ready record whose SOLVED shape occupies the shared key.
    var loaded_record = testSpecRecordReady(testSpecIdentity(loaded_unit, source_digest, digestWithFirstByte(2)), @enumFromInt(9));
    loaded_record.solved_fn_ty = loaded_str;
    loaded_record.solved_fn_ty_digest = shared_solved_digest;
    _ = try builder.insertLoadedReady(loaded_record, loaded_durable, @enumFromInt(1));

    // A local record reserved at a different request shape that SOLVES to the
    // same shared key, so both a local and a loaded entry answer that key.
    const local_fn: Ast.FnId = @enumFromInt(2);
    const local_reserved = try builder.reserve(testSpecIdentity(current_str, source_digest, digestWithFirstByte(3)), local_fn);
    try std.testing.expect(local_reserved.created);
    const local_spec = local_reserved.spec orelse return error.TestUnexpectedResult;
    builder.markLowering(local_spec);
    try builder.markReady(local_spec, current_str, shared_solved_digest);

    // A request at the shared solved shape must reuse the local record, not
    // the loaded one.
    const shared_shaped = testSpecIdentity(current_str, source_digest, shared_solved_digest);
    const hit = (try builder.find(shared_shaped)) orelse return error.TestUnexpectedResult;
    try std.testing.expectEqual(local_spec, hit.local.spec);
    try std.testing.expectEqual(local_fn, hit.local.fn_id);
    builder.validateLookupIntegrity();
}

test "monotype spec builder validator catches a hand-corrupted identity" {
    if (comptime !identity_shadow_enabled) return error.SkipZigTest;

    var name_store = names.NameStore.init(std.testing.allocator);
    defer name_store.deinit();

    var type_store = Type.Store.init(std.testing.allocator);
    defer type_store.deinit();

    const unit_ty = try type_store.add(.zst);
    const str_ty = try type_store.add(.{ .primitive = .str });
    const identity = testSpecIdentity(unit_ty, digestWithFirstByte(1), digestWithFirstByte(2));

    var records = Ast.ProgramList(Ast.SpecRecord, "specs").empty;
    defer records.deinit(std.testing.allocator);

    var builder = SpecBuilder.init(std.testing.allocator, &name_store, &type_store, &records);
    defer builder.deinit();

    const reserved = try builder.reserve(identity, @enumFromInt(1));
    const spec = reserved.spec orelse return error.TestUnexpectedResult;
    try std.testing.expectEqual(@as(?SpecBuilder.IntegrityError, null), builder.lookupIntegrityError());

    // Rewrite the identity behind the builder's back — the exact mutation the
    // immutable-identity design forbids.
    records.getPtrImmediate(@intFromEnum(spec)).identity.request_fn_ty = str_ty;
    records.getPtrImmediate(@intFromEnum(spec)).identity.request_fn_ty_digest = digestWithFirstByte(3);
    try std.testing.expectEqual(
        @as(?SpecBuilder.IntegrityError, .identity_rewritten_after_reserve),
        builder.lookupIntegrityError(),
    );

    // Restore the identity but corrupt the request view instead: the record
    // is no longer reachable from the key its history requires.
    records.getPtrImmediate(@intFromEnum(spec)).identity.request_fn_ty = identity.request_fn_ty;
    records.getPtrImmediate(@intFromEnum(spec)).identity.request_fn_ty_digest = identity.request_fn_ty_digest;
    records.getPtrImmediate(@intFromEnum(spec)).request_fn_ty_digest = digestWithFirstByte(4);
    try std.testing.expectEqual(
        @as(?SpecBuilder.IntegrityError, .record_missing_expected_key),
        builder.lookupIntegrityError(),
    );
}

fn digestWithFirstByte(comptime byte: u8) names.TypeDigest {
    var digest: names.TypeDigest = .{};
    digest.bytes[0] = byte;
    return digest;
}

fn moduleDigestWithFirstByte(comptime byte: u8) names.CheckedModuleDigest {
    var digest: names.CheckedModuleDigest = .{};
    digest.bytes[0] = byte;
    return digest;
}

fn testSpecIdentity(
    request_fn_ty: Type.TypeId,
    source_digest: names.TypeDigest,
    request_digest: names.TypeDigest,
) Ast.SpecIdentity {
    return testSpecIdentityWithModule(request_fn_ty, .{}, source_digest, request_digest);
}

fn testSpecIdentityWithModule(
    request_fn_ty: Type.TypeId,
    module_digest: names.CheckedModuleDigest,
    source_digest: names.TypeDigest,
    request_digest: names.TypeDigest,
) Ast.SpecIdentity {
    return .{
        .callable = .{ .proc_template = .{
            .module = module_digest,
            .proc_base = 0,
            .template = 1,
        } },
        .method_scope = .{},
        .source_fn_ty_digest = source_digest,
        .request_fn_ty_digest = request_digest,
        .request_fn_ty = request_fn_ty,
    };
}

fn testSpecRecordReady(identity: Ast.SpecIdentity, fn_id: Ast.FnId) Ast.SpecRecord {
    return .{
        .identity = identity,
        .request_fn_ty = identity.request_fn_ty,
        .request_fn_ty_digest = identity.request_fn_ty_digest,
        .solved_fn_ty = identity.request_fn_ty,
        .solved_fn_ty_digest = identity.request_fn_ty_digest,
        .fn_id = fn_id,
        .status = .ready,
    };
}
