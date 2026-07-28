//! Production FinalSpecId computation (reunify.md 11.1/11.5, Slice 7 Stage C).
//!
//! A specialization's FinalSpecId is its logical-identity digest — callable
//! identity, representation-erased logical binding, and method scope — combined
//! with the sorted digests of its sealed representation inputs. Body-produced
//! outputs never enter this key. This module computes it from a `SpecRecord`
//! and the program's immutable Monotype store, so a record can carry the
//! identity as inert, parallel data: it is never a reuse or cache key.
//!
//! The representation-erasing mono walk and the representation-input sealing are
//! faithful ports of the Debug shadow's `reunify_shadow/logical_identity.zig`
//! (`walkRequestSealing` and its `MonoWalk`) and `reunify_shadow/shadow.zig`
//! (`sealRepresentationInputs`, `logicalIdentityDigest`, `finalSpecIdDigest`,
//! `sealedShapeDigest`, `hashCallable`). The shadow keeps its own copy for its
//! other censuses; the Stage C parity test asserts the two agree on a hand-built
//! record, so this production duplicate stays faithful. Production must not
//! depend on the shadow, which deletes at the flip.

const std = @import("std");
const check = @import("check");
const collections = @import("collections");

const Ast = @import("ast.zig");
const MonoType = @import("type.zig");
const closure = @import("../representation_closure.zig");
const policy = @import("../representation_policy.zig");

const names = check.CheckedNames;
const checked = check.CheckedModule;
const GuardedList = collections.GuardedList;
const Allocator = std.mem.Allocator;

/// The interned id of a representation-erased logical skeleton in the erasure
/// store. Structurally-equal skeletons intern to one id, so within one
/// `Computer` exact id equality is exact logical equality — the structural
/// collision witness two records sharing one FinalSpecId must agree on.
pub const LogicalId = MonoType.TypeId;

/// A named type's erased occurrence id is meaningless (occurrence identity is
/// dropped), so it is set to this fixed index rather than any real checked id.
const erased_occurrence_index: u32 = 0;
const erased_occurrence: checked.CheckedTypeId = @enumFromInt(erased_occurrence_index);

/// A fixed atom for an iterator's backing leaf: the backing is representation the
/// tier rules relate as a paired component, so same-identity iterators must
/// present equal backing atoms for `relate` to close them.
const backing_leaf_atom: u64 = 0;

/// Why a request type fell outside the representation-reducible subset. Recorded
/// so the caller can census the skip; never a control-flow signal.
pub const SkipReason = enum {
    recursive_cycle,
    representation_bearing,
    zero_sized_or_erased,
    alias_without_backing,
    open_row,
};

const WalkError = error{Skip} || Allocator.Error;

/// One representation-input position discovered while erasing a request type for
/// sealing (reunify.md 11.1). It pairs the position's representation-erased
/// logical identity with the declared representation descriptor a FinalSpecId
/// must additionally digest. `item_logical` is the public item's erased identity
/// for an iterator (declared argument index 0), or the position's own identity.
const RepresentationInput = struct {
    logical: LogicalId,
    is_iterator: bool,
    descriptor: policy.NamedDescriptor,
    item_logical: LogicalId,
};

/// The interning store the erasure walk builds skeletons in. Its own name
/// interner resolves every label so two logically-equal skeletons — however
/// their source stores named them — meet on one id.
const ErasureStore = struct {
    allocator: Allocator,
    erasure_names: names.NameStore,
    store: MonoType.Store,

    fn init(allocator: Allocator) ErasureStore {
        var store = MonoType.Store.init(allocator);
        store.enableInterning();
        return .{
            .allocator = allocator,
            .erasure_names = names.NameStore.init(allocator),
            .store = store,
        };
    }

    fn deinit(self: *ErasureStore) void {
        self.store.deinit();
        self.erasure_names.deinit();
    }

    fn digestBytes(self: *ErasureStore, id: LogicalId) names.TypeDigest {
        return self.store.typeDigest(&self.erasure_names, id);
    }
};

/// One request-type erasure walk. Representation-bearing named nodes erase to
/// their plain skeleton and record their descriptor, so a request that carries
/// iterator or generated representation still yields the logical binding half a
/// FinalSpecId digests together with the sealed representation inputs. Other skip
/// reasons (recursive cycle, open row, zero sized) leave the reducible subset.
const Walk = struct {
    owner: *ErasureStore,
    store: *const MonoType.Store,
    source_names: *const names.NameStore,
    active: std.AutoHashMap(MonoType.TypeId, void),
    skip_reason: *SkipReason,
    rep_inputs: *std.ArrayList(RepresentationInput),

    fn skip(self: *Walk, reason: SkipReason) WalkError {
        self.skip_reason.* = reason;
        return error.Skip;
    }

    fn node(self: *Walk, mono_ty: MonoType.TypeId) WalkError!LogicalId {
        if (self.active.contains(mono_ty)) return self.skip(.recursive_cycle);
        try self.active.put(mono_ty, {});
        defer _ = self.active.remove(mono_ty);

        return switch (self.store.get(mono_ty)) {
            .primitive => |value| try self.owner.store.internPrimitive(&self.owner.erasure_names, value),
            .zst => self.skip(.zero_sized_or_erased),
            .erased => self.skip(.zero_sized_or_erased),
            .list => |elem| try self.owner.store.internList(&self.owner.erasure_names, try self.node(elem)),
            .box => |elem| try self.owner.store.internBox(&self.owner.erasure_names, try self.node(elem)),
            .tuple => |span| try self.tupleFrom(span),
            .record => |span| try self.recordFrom(span),
            .tag_union => |span| try self.tagUnionFrom(span),
            .func => |fn_ty| try self.function(fn_ty),
            .named => |n| try self.named(n),
        };
    }

    fn function(self: *Walk, fn_ty: std.meta.fieldInfo(MonoType.Content, .func).type) WalkError!LogicalId {
        var args = std.ArrayList(LogicalId).empty;
        defer args.deinit(self.owner.allocator);
        const arg_span = self.store.span(fn_ty.args);
        for (0..GuardedList.borrowLen(arg_span)) |index| {
            try args.append(self.owner.allocator, try self.node(GuardedList.at(arg_span, index)));
        }
        const ret = try self.node(fn_ty.ret);
        return try self.owner.store.internFunc(&self.owner.erasure_names, args.items, ret);
    }

    fn tupleFrom(self: *Walk, span: MonoType.Span) WalkError!LogicalId {
        var lowered = std.ArrayList(LogicalId).empty;
        defer lowered.deinit(self.owner.allocator);
        const item_span = self.store.span(span);
        for (0..GuardedList.borrowLen(item_span)) |index| {
            try lowered.append(self.owner.allocator, try self.node(GuardedList.at(item_span, index)));
        }
        return try self.owner.store.internTuple(&self.owner.erasure_names, lowered.items);
    }

    fn recordFrom(self: *Walk, span: MonoType.Span) WalkError!LogicalId {
        var fields = std.ArrayList(MonoType.Field).empty;
        defer fields.deinit(self.owner.allocator);
        const field_span = self.store.fieldSpan(span);
        for (0..GuardedList.borrowLen(field_span)) |index| {
            const field = GuardedList.at(field_span, index);
            const label = try self.owner.erasure_names.internRecordFieldLabel(self.source_names.recordFieldLabelText(field.name));
            const ty = try self.node(field.ty);
            try fields.append(self.owner.allocator, .{ .name = label, .ty = ty });
        }
        return try self.owner.store.internRecord(&self.owner.erasure_names, fields.items);
    }

    fn tagUnionFrom(self: *Walk, span: MonoType.Span) WalkError!LogicalId {
        var tags = std.ArrayList(MonoType.Store.TagInput).empty;
        defer {
            for (tags.items) |tag| self.owner.allocator.free(tag.payloads);
            tags.deinit(self.owner.allocator);
        }
        const tag_span = self.store.tagSpan(span);
        for (0..GuardedList.borrowLen(tag_span)) |tag_index| {
            const tag = GuardedList.at(tag_span, tag_index);
            const label = try self.owner.erasure_names.internTagLabel(self.source_names.tagLabelText(tag.name));
            var payloads = std.ArrayList(LogicalId).empty;
            errdefer payloads.deinit(self.owner.allocator);
            const payload_span = self.store.span(tag.payloads);
            for (0..GuardedList.borrowLen(payload_span)) |payload_index| {
                try payloads.append(self.owner.allocator, try self.node(GuardedList.at(payload_span, payload_index)));
            }
            try tags.append(self.owner.allocator, .{
                .name = label,
                .checked_name = label,
                .payloads = try payloads.toOwnedSlice(self.owner.allocator),
            });
        }
        return try self.owner.store.internTagUnion(&self.owner.erasure_names, tags.items);
    }

    /// A named node. An alias erases to its backing (source-level identity drops
    /// every backed alias, builtin-owned included — reunify.md section 8.2). A
    /// representation-bearing nominal or opaque erases to its plain skeleton and
    /// records its descriptor as a representation input.
    fn named(self: *Walk, n: MonoType.NamedContent) WalkError!LogicalId {
        if (n.kind == .alias) {
            const backing = n.backing orelse return self.skip(.alias_without_backing);
            return try self.node(backing.ty);
        }

        var args = std.ArrayList(LogicalId).empty;
        defer args.deinit(self.owner.allocator);
        const arg_span = self.store.span(n.args);
        for (0..GuardedList.borrowLen(arg_span)) |index| {
            try args.append(self.owner.allocator, try self.node(GuardedList.at(arg_span, index)));
        }

        const def_module_hash = self.source_names.moduleIdentityBytes(n.def.module).*;
        // The erased skeleton drops iterator tier/kind/depth and the generated
        // owner (section 8.2), so every representation variant of one nominal
        // shares this identity. The backing is representation, never part of it.
        const module_id = try self.owner.erasure_names.internModuleIdentity(&def_module_hash);
        const type_name = try self.owner.erasure_names.internTypeName(self.source_names.typeNameText(n.def.type_name));
        const skeleton = try self.owner.store.internNamed(&self.owner.erasure_names, .{
            .named_type = .{ .module = .{ .bytes = n.named_type.module.bytes }, .ty = erased_occurrence },
            .def = .{ .module = module_id, .type_name = type_name, .source_decl = n.def.source_decl },
            .kind = n.kind,
            .builtin_owner = null,
            .args = args.items,
            .backing = null,
            .declared_order = &.{},
        });

        const representation_bearing = n.def.iterator_representation != .none or n.def.generated != null;
        if (representation_bearing) {
            try self.rep_inputs.append(self.owner.allocator, .{
                .logical = skeleton,
                .is_iterator = n.def.iterator_representation != .none,
                .descriptor = .{
                    .kind = n.kind,
                    .def = n.def,
                    .builtin_owner = n.builtin_owner,
                },
                .item_logical = if (args.items.len > 0) args.items[0] else skeleton,
            });
        }

        return skeleton;
    }
};

/// The FinalSpecId computed for one record, plus the collision witness and the
/// output-summary digest lists the Stage D cache serializes. The caller owns
/// `input_digests` and `output_rep_digests` and must `deinit`.
pub const Computed = struct {
    /// The combined FinalSpecId digest.
    final_spec_id: names.TypeDigest,
    /// Callable identity, erased logical binding, and method scope.
    logical_identity_digest: names.TypeDigest,
    /// The erased solved logical skeleton's digest, or zero when the solved type
    /// left the reducible subset.
    output_solved_digest: names.TypeDigest,
    /// The erased solved logical skeleton's interned id, present only when the
    /// solved type reduced. Two records with equal ids have structurally equal
    /// solved skeletons.
    solved_logical: ?LogicalId,
    /// The sorted sealed representation-input digests of the request — the
    /// representation inputs the FinalSpecId digests.
    input_digests: []names.TypeDigest,
    /// The sorted sealed representation digests the body produced (the solved
    /// type's representation inputs) — the output summary a cache hit replays.
    output_rep_digests: []names.TypeDigest,

    pub fn deinit(self: *Computed, allocator: Allocator) void {
        allocator.free(self.input_digests);
        allocator.free(self.output_rep_digests);
    }
};

/// Computes and accumulates FinalSpecId identities across one lowering. Its
/// erasure store persists across records, so solved skeletons interned for two
/// records compare by exact id (the structural collision witness).
pub const Computer = struct {
    allocator: Allocator,
    erasure: ErasureStore,

    pub fn init(allocator: Allocator) Computer {
        return .{ .allocator = allocator, .erasure = ErasureStore.init(allocator) };
    }

    pub fn deinit(self: *Computer) void {
        self.erasure.deinit();
    }

    /// Compute the FinalSpecId of `record`, reading its types from `store`
    /// resolved through `store_names`. Returns null when the request type is
    /// outside the reducible subset (recursive cycle, open row, zero sized), in
    /// which case the record carries no FinalSpecId.
    pub fn compute(
        self: *Computer,
        record: Ast.SpecRecord,
        store: *const MonoType.Store,
        store_names: *const names.NameStore,
    ) Allocator.Error!?Computed {
        var rep_inputs = std.ArrayList(RepresentationInput).empty;
        defer rep_inputs.deinit(self.allocator);

        var request_reason: SkipReason = undefined;
        const erased_request = self.walk(store, store_names, record.request_fn_ty, &rep_inputs, &request_reason) catch |err| switch (err) {
            error.Skip => return null,
            else => |other| return other,
        };

        var sealed = try self.sealRepresentationInputs(store_names, rep_inputs.items);
        errdefer sealed.deinit(self.allocator);

        const logical_identity_digest = self.logicalIdentityDigest(record, erased_request);
        const final_spec_id = finalSpecIdDigest(logical_identity_digest, sealed.items);

        var solved_inputs = std.ArrayList(RepresentationInput).empty;
        defer solved_inputs.deinit(self.allocator);
        var solved_reason: SkipReason = undefined;
        const solved_logical: ?LogicalId = self.walk(store, store_names, record.solved_fn_ty, &solved_inputs, &solved_reason) catch |err| switch (err) {
            error.Skip => null,
            else => |other| return other,
        };

        var output_reps = try self.sealRepresentationInputs(store_names, solved_inputs.items);
        errdefer output_reps.deinit(self.allocator);

        return .{
            .final_spec_id = final_spec_id,
            .logical_identity_digest = logical_identity_digest,
            .output_solved_digest = if (solved_logical) |id| self.erasure.digestBytes(id) else .{},
            .solved_logical = solved_logical,
            .input_digests = try sealed.toOwnedSlice(self.allocator),
            .output_rep_digests = try output_reps.toOwnedSlice(self.allocator),
        };
    }

    fn walk(
        self: *Computer,
        store: *const MonoType.Store,
        store_names: *const names.NameStore,
        mono_ty: MonoType.TypeId,
        rep_inputs: *std.ArrayList(RepresentationInput),
        skip_reason: *SkipReason,
    ) WalkError!LogicalId {
        var walker = Walk{
            .owner = &self.erasure,
            .store = store,
            .source_names = store_names,
            .active = std.AutoHashMap(MonoType.TypeId, void).init(self.allocator),
            .skip_reason = skip_reason,
            .rep_inputs = rep_inputs,
        };
        defer walker.active.deinit();
        return try walker.node(mono_ty);
    }

    /// The LogicalSpecIdentity digest (reunify.md 11.1): callable identity, the
    /// erased logical binding of the request, and the method scope.
    fn logicalIdentityDigest(self: *Computer, record: Ast.SpecRecord, erased_request: LogicalId) names.TypeDigest {
        var hasher = std.crypto.hash.sha2.Sha256.init(.{});
        hashCallable(&hasher, record.identity.callable);
        const erased_digest = self.erasure.digestBytes(erased_request);
        hasher.update(&erased_digest.bytes);
        hasher.update(&record.identity.method_scope.bytes);
        return .{ .bytes = hasher.finalResult() };
    }

    /// Seal a record's representation-input positions through the section 10.3
    /// closure engine and return the sorted distinct digests of the sealed
    /// representatives. Two positions carrying one logical identity are related,
    /// driving the tier rules to a fixpoint.
    fn sealRepresentationInputs(
        self: *Computer,
        store_names: *const names.NameStore,
        rep_inputs: []const RepresentationInput,
    ) Allocator.Error!std.ArrayList(names.TypeDigest) {
        var engine = closure.Engine.init(self.allocator);
        defer engine.deinit();

        var top_slots = std.ArrayList(closure.RepresentationSlotId).empty;
        defer top_slots.deinit(self.allocator);
        var by_token = std.AutoHashMap(u64, closure.RepresentationSlotId).init(self.allocator);
        defer by_token.deinit();

        var atom_counter: u32 = 0;
        for (rep_inputs) |input| {
            const token: closure.LogicalToken = @enumFromInt(@as(u64, @intFromEnum(input.logical)));
            const slot: closure.RepresentationSlotId = if (input.is_iterator) iter_blk: {
                const item_token: closure.LogicalToken = @enumFromInt(@as(u64, @intFromEnum(input.item_logical)));
                const item = try engine.createSlot(item_token, @enumFromInt(atom_counter), .{ .leaf = @intFromEnum(input.item_logical) });
                atom_counter += 1;
                const backing = try engine.createSlot(token, @enumFromInt(atom_counter), .{ .leaf = backing_leaf_atom });
                atom_counter += 1;
                const iterator = try engine.createSlot(token, @enumFromInt(atom_counter), .{ .iterator = .{
                    .descriptor = input.descriptor,
                    .item = item,
                    .backing = backing,
                } });
                atom_counter += 1;
                break :iter_blk iterator;
            } else leaf_blk: {
                const leaf_atom: u64 = if (input.descriptor.def.generated) |generated|
                    firstBytesToU64(&generated.bytes)
                else
                    0;
                const leaf = try engine.createSlot(token, @enumFromInt(atom_counter), .{ .leaf = leaf_atom });
                atom_counter += 1;
                break :leaf_blk leaf;
            };

            if (by_token.get(@intFromEnum(token))) |prior| {
                engine.relate(prior, slot, .component_equality) catch |err| switch (err) {
                    // Two same-logical positions whose sub-components are not
                    // logically equal are left in separate classes; the census
                    // only measures.
                    error.LogicallyUnequal => {},
                    else => |other| return other,
                };
            } else {
                try by_token.put(@intFromEnum(token), slot);
            }
            try top_slots.append(self.allocator, slot);
        }

        var seen_reps = std.AutoHashMap(u32, void).init(self.allocator);
        defer seen_reps.deinit();
        var digests = std.ArrayList(names.TypeDigest).empty;
        errdefer digests.deinit(self.allocator);
        for (top_slots.items) |slot| {
            const rep = engine.find(slot);
            const rep_entry = try seen_reps.getOrPut(@intFromEnum(rep));
            if (rep_entry.found_existing) continue;
            try digests.append(self.allocator, sealedShapeDigest(store_names, engine.shapeOf(rep)));
        }
        std.sort.pdq(names.TypeDigest, digests.items, {}, lessThanDigest);
        return digests;
    }
};

fn lessThanDigest(_: void, a: names.TypeDigest, b: names.TypeDigest) bool {
    return std.mem.order(u8, &a.bytes, &b.bytes) == .lt;
}

/// The leading eight bytes of a digest as a u64 atom, so two positions carrying
/// the same generated owner present equal leaf atoms and relate as equal
/// representations.
fn firstBytesToU64(bytes: *const [32]u8) u64 {
    var atom: u64 = 0;
    for (bytes[0..8]) |byte| atom = (atom << 8) | byte;
    return atom;
}

/// The deterministic digest of one sealed representation representative
/// (reunify.md 11.5): an iterator digests its declared identity and recorded
/// tier/kind/depth and generated owner; a leaf digests its atom.
fn sealedShapeDigest(store_names: *const names.NameStore, shape: closure.SlotShape) names.TypeDigest {
    var hasher = std.crypto.hash.sha2.Sha256.init(.{});
    switch (shape) {
        .iterator => |iter| {
            hasher.update("iterator");
            const def = iter.descriptor.def;
            hasher.update(store_names.moduleIdentityBytes(def.module));
            const type_name: u32 = @intFromEnum(def.type_name);
            hasher.update(std.mem.asBytes(&type_name));
            const source_decl: u32 = def.source_decl orelse std.math.maxInt(u32);
            hasher.update(std.mem.asBytes(&source_decl));
            hasher.update(&.{@intFromEnum(def.iterator_representation)});
            hasher.update(&.{@intFromEnum(def.iterator_kind)});
            hasher.update(&.{def.iterator_depth});
            if (def.generated) |generated| {
                hasher.update("gen");
                hasher.update(&generated.bytes);
            } else {
                hasher.update("nogen");
            }
        },
        .evidence => |ev| {
            hasher.update("evidence");
            hasher.update(&.{ev.score});
        },
        .wrapper => hasher.update("wrapper"),
        .leaf => |atom| {
            hasher.update("leaf");
            hasher.update(std.mem.asBytes(&atom));
        },
    }
    return .{ .bytes = hasher.finalResult() };
}

/// FinalSpecId (reunify.md 11.1): the logical-identity digest plus the sorted
/// sealed representation-input digests. Body-produced outputs never enter it.
fn finalSpecIdDigest(logical_id_digest: names.TypeDigest, sealed: []const names.TypeDigest) names.TypeDigest {
    var hasher = std.crypto.hash.sha2.Sha256.init(.{});
    hasher.update(&logical_id_digest.bytes);
    const count: u32 = @intCast(sealed.len);
    hasher.update(std.mem.asBytes(&count));
    for (sealed) |digest| hasher.update(&digest.bytes);
    return .{ .bytes = hasher.finalResult() };
}

/// Hash a callable identity field-by-field: its in-memory bytes carry union
/// padding, so hashing the declared fields keeps the digest deterministic.
fn hashCallable(hasher: *std.crypto.hash.sha2.Sha256, callable: Ast.CallableIdentity) void {
    hasher.update(&.{@intFromEnum(std.meta.activeTag(callable))});
    switch (callable) {
        .proc_template => |proc| {
            hasher.update(&proc.module.bytes);
            hasher.update(std.mem.asBytes(&proc.proc_base));
            hasher.update(std.mem.asBytes(&proc.template));
        },
        .nested_site => |nested| {
            hasher.update(&nested.module.bytes);
            hasher.update(std.mem.asBytes(&nested.owner_proc_base));
            hasher.update(std.mem.asBytes(&nested.owner_template));
            hasher.update(&nested.owner_fn_digest.bytes);
            hasher.update(std.mem.asBytes(&nested.site));
        },
        .hosted => |hosted| {
            const raw: u32 = @intFromEnum(hosted);
            hasher.update(std.mem.asBytes(&raw));
        },
        .generated => |generated| {
            const raw: u32 = @intFromEnum(generated);
            hasher.update(std.mem.asBytes(&raw));
        },
    }
}

test "declarations are referenced" {
    std.testing.refAllDecls(@This());
}
