//! Completed compile-time values that lower as literals or as the runtime
//! construction they came from, instead of as static-data slots.
//!
//! A runtime continuation forked from a completed host program is lowered
//! after every compile-time root has been evaluated, so a scalar root's
//! value is known when its read is lowered. Reading it through a
//! static-data slot instead would hide the constant from the LIR passes that
//! reason about constants—range proving, loop versioning, overflow
//! elision—so a table built by `List.repeat` with a compile-time length
//! would keep every index check the prover otherwise discharges. This table
//! holds each completed successful scalar root's literal, decoded from the
//! host's frozen image and keyed by checked root identity exactly as the
//! later transcoding matches slots, so the lowerer emits the literal
//! directly and creates no slot, failure record, or guard for it.
//!
//! Two list shapes get the same treatment, because static data is the wrong
//! home for them: a value's constructor is cheaper than its bytes. An empty
//! list lowers to the `with_capacity` it was evaluated with, so the request
//! survives the freeze (a frozen descriptor cannot carry capacity) and the
//! first append goes in place. A list of copies of one scalar, which is
//! what `List.repeat` and every constant fill loop produce, lowers to that
//! repeat loop again: a table of zeros is a few instructions at runtime and
//! would otherwise be that many bytes of zeros in the binary, and a static
//! list can never be born unique, which loses the in-place writes of every
//! loop the table is carried through. Other aggregate roots keep their
//! slots and fold in the backend; failed roots keep the guard that crashes
//! with the original failure.
//!
//! A build that restores its compile-time values from a checked module's
//! const store, rather than from a completed host program, reaches the
//! same constructions through the restored expressions; the lowerer's
//! static-data candidate path decides those, and shares this decoder for
//! the elements of a packed list.
const std = @import("std");
const check = @import("check");
const core = @import("lir_core");
const layout = @import("layout");
const checked = check.CheckedModule;
const LIR = core.LIR;
const Program = core.Program;
const Allocator = std.mem.Allocator;

/// How a completed value, or a part of one, lowers in place of its slot:
/// a scalar's literal, or the constructor of an empty or uniform value.
///
/// A construction is the value's shape and leaves alone. It names no list,
/// field or payload layout: the program that decoded it and the program that
/// emits it intern layouts in their own order, so an index from one is not a
/// name in the other. The reading site's own layout supplies every layout
/// the emitted code needs, and a scalar literal's layout is a fixed index
/// that every store shares.
pub const Construction = union(enum) {
    literal: LIR.LiteralValue,
    zst,
    /// The empty string.
    empty_str,
    /// An empty list, rebuilt with the capacity it was evaluated with.
    empty_list: u64,
    /// `count` copies of one element, rebuilt by the repeat loop.
    uniform_list: struct {
        element: *const Construction,
        count: u64,
    },
    /// A record, one construction per field in original order.
    record: []const Construction,
    /// A tag with its payload, when it has one.
    tag: struct {
        variant_index: u16,
        discriminant: u16,
        payload: ?*const Construction,
    },
};

/// Constructions of the completed successful roots of one host program that
/// lower without a slot, keyed by checked root identity.
pub const CompletedScalarValues = struct {
    entries: Map,
    /// Owns the nested constructions the entries point into.
    arena: std.heap.ArenaAllocator,

    const Key = struct {
        module: checked.ModuleId,
        root: checked.ComptimeRootId,
    };

    const Context = struct {
        pub fn hash(_: Context, key: Key) u64 {
            var hasher = std.hash.Wyhash.init(0);
            hasher.update(&key.module.bytes);
            hasher.update(std.mem.asBytes(&key.root));
            return hasher.final();
        }

        pub fn eql(_: Context, a: Key, b: Key) bool {
            return std.meta.eql(a.module, b.module) and a.root == b.root;
        }
    };

    const Map = std.HashMapUnmanaged(Key, Construction, Context, std.hash_map.default_max_load_percentage);

    /// Collects every completed successful root of `program` whose frozen
    /// image decodes to a construction.
    pub fn init(allocator: Allocator, program: *const Program.Result, frozen: *const Program.FrozenStaticData) Allocator.Error!CompletedScalarValues {
        var values = CompletedScalarValues{ .entries = .empty, .arena = std.heap.ArenaAllocator.init(allocator) };
        errdefer values.deinit(allocator);
        var decoder = Decoder{ .program = program, .frozen = frozen, .arena = values.arena.allocator() };
        for (program.static_data_values.items, 0..) |entry, index| {
            const root = entry.compile_time_root orelse continue;
            if (root.role != .value) continue;
            const slot: LIR.StaticDataId = @enumFromInt(index);
            if (!slotSucceeded(program, frozen, slot)) continue;
            const data_export = exportOf(frozen, slot) orelse continue;
            const construction = try decoder.decode(data_export, data_export.bytes[data_export.symbol_offset..], data_export.symbol_offset, root.role.value.plan, entry.layout_idx) orelse continue;
            try values.entries.put(allocator, .{ .module = root.module, .root = root.root }, construction);
        }
        return values;
    }

    pub fn deinit(self: *CompletedScalarValues, allocator: Allocator) void {
        self.entries.deinit(allocator);
        self.arena.deinit();
    }

    /// The construction for a root read at `layout_idx`, when the root
    /// completed successfully in a shape that lowers without a slot. A
    /// scalar is checked against the read's layout here; an aggregate is
    /// checked against it shape by shape as `emit` builds it.
    pub fn constructionFor(self: *const CompletedScalarValues, module: checked.ModuleId, root: checked.ComptimeRootId, layout_idx: layout.Idx) ?Construction {
        const construction = self.entries.get(.{ .module = module, .root = root }) orelse return null;
        switch (construction) {
            .literal => |literal| if (!literalFitsLayout(literal, layout_idx)) return null,
            .zst, .empty_str, .empty_list, .uniform_list, .record, .tag => {},
        }
        return construction;
    }

    /// The literal for a root read at `layout_idx`, when the root completed
    /// successfully with a scalar of that layout.
    pub fn literalFor(self: *const CompletedScalarValues, module: checked.ModuleId, root: checked.ComptimeRootId, layout_idx: layout.Idx) ?LIR.LiteralValue {
        const construction = self.constructionFor(module, root, layout_idx) orelse return null;
        return switch (construction) {
            .literal => |literal| literal,
            .zst, .empty_str, .empty_list, .uniform_list, .record, .tag => null,
        };
    }
};

/// Whether a decoded scalar literal is a value of `layout_idx`: the literal
/// carries the scalar layout it was decoded at, and scalar layouts are fixed
/// indices shared by every store.
fn literalFitsLayout(literal: LIR.LiteralValue, layout_idx: layout.Idx) bool {
    return switch (literal) {
        .i64_literal => |int| int.layout_idx == layout_idx,
        .i128_literal => |int| int.layout_idx == layout_idx,
        .f32_literal => layout_idx == .f32,
        .f64_literal => layout_idx == .f64,
        .dec_literal => layout_idx == .dec,
        .str_literal, .boxy_dynamic_num_literal, .boxy_dynamic_frac_literal, .static_data, .bytes_literal, .null_ptr, .proc_ref => false,
    };
}

/// Emits `target = construction` into `store`, continuing at `next`, and
/// returns the entry statement; null when the construction does not fit
/// the target's layout. `ctx` supplies locals and join-point ids:
/// `addLocal(layout.Idx) Allocator.Error!LIR.LocalId` and
/// `freshJoinPointId() LIR.JoinPointId`, and
/// `addJoin(LIR.JoinPoint, LIR.CFStmtId) Allocator.Error!LIR.CFStmtId`.
/// The context owns final join metadata when emitting after ARC. Every value the emitted code
/// builds is fresh and consumed exactly once, so the code is complete
/// without a reference-counting pass: a repeat loop builds its element
/// anew on each iteration rather than sharing one across appends.
pub fn emit(ctx: anytype, store: *core.LirStore, layouts: *const layout.Store, target: LIR.LocalId, construction: Construction, next: LIR.CFStmtId) Allocator.Error!?LIR.CFStmtId {
    switch (construction) {
        .literal => |literal| return try store.addCFStmt(.{ .assign_literal = .{ .target = target, .value = literal, .next = next } }),
        .zst => return try store.addCFStmt(.{ .assign_struct = .{ .target = target, .fields = LIR.LocalSpan.empty(), .next = next } }),
        .empty_str => return try store.addCFStmt(.{ .assign_literal = .{
            .target = target,
            .value = .{ .str_literal = try store.insertStringView("", 0, 0) },
            .next = next,
        } }),
        .empty_list => |capacity| {
            if (capacity > std.math.maxInt(i64)) return null;
            return try emitWithCapacity(ctx, store, target, @intCast(capacity), next);
        },
        .uniform_list => |uniform| {
            if (uniform.count > std.math.maxInt(i64)) return null;
            return try emitRepeat(ctx, store, layouts, target, uniform.element.*, @intCast(uniform.count), next);
        },
        .record => |fields| {
            const layout_idx = store.getLocal(target).layout_idx;
            const value_layout = layouts.getLayout(layout_idx);
            if (value_layout.tag != .struct_) return null;
            const struct_idx = value_layout.getStruct().idx;
            const field_locals = try store.allocator.alloc(LIR.LocalId, fields.len);
            defer store.allocator.free(field_locals);
            for (field_locals, 0..) |*local, original_index| {
                local.* = try ctx.addLocal(layouts.getStructFieldLayoutByOriginalIndex(struct_idx, @intCast(original_index)));
            }
            var current = try store.addCFStmt(.{ .assign_struct = .{
                .target = target,
                .fields = try store.addLocalSpan(field_locals),
                .next = next,
            } });
            var index = fields.len;
            while (index > 0) {
                index -= 1;
                current = try emit(ctx, store, layouts, field_locals[index], fields[index], current) orelse return null;
            }
            return current;
        },
        .tag => |tag| {
            const layout_idx = store.getLocal(target).layout_idx;
            const value_layout = layouts.getLayout(layout_idx);
            if (value_layout.tag != .tag_union) return null;
            const info = layouts.getTagUnionInfo(value_layout);
            if (tag.variant_index >= info.variants.len) return null;
            var payload_local: ?LIR.LocalId = null;
            if (tag.payload != null) {
                payload_local = try ctx.addLocal(info.variants.get(tag.variant_index).payload_layout);
            }
            const build = try store.addCFStmt(.{ .assign_tag = .{
                .target = target,
                .variant_index = tag.variant_index,
                .discriminant = tag.discriminant,
                .payload = payload_local,
                .next = next,
            } });
            if (tag.payload) |payload| {
                return try emit(ctx, store, layouts, payload_local.?, payload.*, build);
            }
            return build;
        },
    }
}

/// `target = list_with_capacity(capacity)`, the runtime form of a completed
/// empty list.
fn emitWithCapacity(ctx: anytype, store: *core.LirStore, target: LIR.LocalId, capacity: i64, next: LIR.CFStmtId) Allocator.Error!LIR.CFStmtId {
    const capacity_local = try ctx.addLocal(.u64);
    const build = try store.addCFStmt(.{ .assign_low_level = .{
        .target = target,
        .op = .list_with_capacity,
        .rc_effect = LIR.LowLevel.list_with_capacity.rcEffect(),
        .args = try store.addLocalSpan(&[_]LIR.LocalId{capacity_local}),
        .next = next,
    } });
    return try store.addCFStmt(.{ .assign_literal = .{
        .target = capacity_local,
        .value = .{ .i64_literal = .{ .value = capacity, .layout_idx = .u64 } },
        .next = build,
    } });
}

/// The repeat loop a completed uniform list came from: reserve `count`
/// elements, then build the element and append it `count` times unchecked.
/// The later passes treat it as they do any source loop.
fn emitRepeat(ctx: anytype, store: *core.LirStore, layouts: *const layout.Store, target: LIR.LocalId, element: Construction, count: i64, next: LIR.CFStmtId) Allocator.Error!?LIR.CFStmtId {
    const list_layout = store.getLocal(target).layout_idx;
    const list_value_layout = layouts.getLayout(list_layout);
    if (list_value_layout.tag != .list) return null;
    const element_layout = list_value_layout.getIdx();
    const count_local = try ctx.addLocal(.u64);
    const reserved = try ctx.addLocal(list_layout);
    const zero = try ctx.addLocal(.u64);
    const list_param = try ctx.addLocal(list_layout);
    const index_param = try ctx.addLocal(.u64);
    const more = try ctx.addLocal(.bool);
    const element_local = try ctx.addLocal(element_layout);
    const appended = try ctx.addLocal(list_layout);
    const one = try ctx.addLocal(.u64);
    const next_index = try ctx.addLocal(.u64);
    const join_id = ctx.freshJoinPointId();

    // Exit: the carried list is the result.
    const exit = try store.addCFStmt(.{ .assign_ref = .{ .target = target, .op = .{ .local = list_param }, .next = next } });
    // Step: build the element, append it, and go round again.
    const back_jump = try store.addCFStmt(.{ .jump = .{ .target = join_id } });
    const set_index = try store.addCFStmt(.{ .set_local = .{ .target = index_param, .value = next_index, .mode = .initialize_join_param, .next = back_jump } });
    const set_list = try store.addCFStmt(.{ .set_local = .{ .target = list_param, .value = appended, .mode = .initialize_join_param, .next = set_index } });
    const bump = try store.addCFStmt(.{ .assign_low_level = .{
        .target = next_index,
        .op = .num_int_add_wrap,
        .rc_effect = LIR.LowLevel.num_int_add_wrap.rcEffect(),
        .args = try store.addLocalSpan(&[_]LIR.LocalId{ index_param, one }),
        .next = set_list,
    } });
    const one_literal = try store.addCFStmt(.{ .assign_literal = .{ .target = one, .value = .{ .i64_literal = .{ .value = 1, .layout_idx = .u64 } }, .next = bump } });
    const append = try store.addCFStmt(.{ .assign_low_level = .{
        .target = appended,
        .op = .list_append_unsafe,
        .rc_effect = LIR.LowLevel.list_append_unsafe.rcEffect(),
        .args = try store.addLocalSpan(&[_]LIR.LocalId{ list_param, element_local }),
        .next = one_literal,
    } });
    const build_element = try emit(ctx, store, layouts, element_local, element, append) orelse return null;
    const dispatch = try store.addCFStmt(.{ .switch_stmt = .{
        .cond = more,
        .branches = try store.addCFSwitchBranches(&.{.{ .value = 1, .body = build_element }}),
        .default_branch = exit,
        .default_is_cold = false,
        .continuation = null,
    } });
    const body = try store.addCFStmt(.{ .assign_low_level = .{
        .target = more,
        .op = .num_is_lt,
        .rc_effect = LIR.LowLevel.num_is_lt.rcEffect(),
        .args = try store.addLocalSpan(&[_]LIR.LocalId{ index_param, count_local }),
        .next = dispatch,
    } });
    // Entry: the count, the reserved list, and index zero.
    const entry_jump = try store.addCFStmt(.{ .jump = .{ .target = join_id } });
    const init_index = try store.addCFStmt(.{ .set_local = .{ .target = index_param, .value = zero, .mode = .initialize_join_param, .next = entry_jump } });
    const init_list = try store.addCFStmt(.{ .set_local = .{ .target = list_param, .value = reserved, .mode = .initialize_join_param, .next = init_index } });
    const zero_literal = try store.addCFStmt(.{ .assign_literal = .{ .target = zero, .value = .{ .i64_literal = .{ .value = 0, .layout_idx = .u64 } }, .next = init_list } });
    const reserve = try store.addCFStmt(.{ .assign_low_level = .{
        .target = reserved,
        .op = .list_with_capacity,
        .rc_effect = LIR.LowLevel.list_with_capacity.rcEffect(),
        .args = try store.addLocalSpan(&[_]LIR.LocalId{count_local}),
        .next = zero_literal,
    } });
    const count_literal = try store.addCFStmt(.{ .assign_literal = .{ .target = count_local, .value = .{ .i64_literal = .{ .value = count, .layout_idx = .u64 } }, .next = reserve } });
    return try ctx.addJoin(.{
        .id = join_id,
        .params = try store.addLocalSpan(&[_]LIR.LocalId{ list_param, index_param }),
        .body = body,
    }, count_literal);
}

/// Decodes a completed value into its construction by walking the same
/// const plan the freezer walked, at the same byte offsets. Any part that
/// is not a scalar, the empty string, an empty or uniform list, a record,
/// or a tag leaves the whole value undecoded. A decoder over plain memory
/// bytes has no frozen image; lists in such bytes stay undecoded, since
/// their elements live behind a relocation the bytes cannot follow.
pub const Decoder = struct {
    program: *const Program.Result,
    frozen: ?*const Program.FrozenStaticData,
    arena: Allocator,

    /// `bytes` is the value's image, starting at `offset` within
    /// `data_export`; both are null for plain memory bytes.
    pub fn decode(self: *Decoder, data_export: ?*const Program.StaticDataExport, bytes: []const u8, offset: usize, plan: Program.ConstPlanId, layout_idx: layout.Idx) Allocator.Error!?Construction {
        const value_layout = self.program.layouts.getLayout(layout_idx);
        if (value_layout.tag == .zst) return .zst;
        return switch (self.program.const_plans.items[@intFromEnum(plan)]) {
            .zst => .zst,
            .scalar => if (decodeScalar(layout_idx, bytes)) |literal| .{ .literal = literal } else null,
            .str => if (self.stringIsEmpty(bytes)) .empty_str else null,
            .list => |element_plan| try self.decodeList(data_export, bytes, offset, element_plan, value_layout),
            .named => |named| try self.decode(data_export, bytes, offset, named.backing, layout_idx),
            .tuple, .record => |child_plans| try self.decodeRecord(data_export, bytes, offset, child_plans, value_layout),
            .tag_union => |variants| try self.decodeTag(data_export, bytes, offset, variants, layout_idx),
            .pending, .layout_only, .box, .fn_value, .erased_fn => null,
        };
    }

    fn word(self: *const Decoder) usize {
        return self.program.layouts.targetUsize().size();
    }

    fn readWord(self: *const Decoder, bytes: []const u8, index: usize) ?u64 {
        const size = self.word();
        if (bytes.len < (index + 1) * size) return null;
        const start = index * size;
        return switch (size) {
            4 => std.mem.readInt(u32, bytes[start..][0..4], .little),
            8 => std.mem.readInt(u64, bytes[start..][0..8], .little),
            else => null,
        };
    }

    /// A string descriptor is three words, the last being its length; a
    /// small string sets that word's top bit and keeps its length in the
    /// low seven bits of the descriptor's final byte.
    fn stringIsEmpty(self: *const Decoder, bytes: []const u8) bool {
        const size = self.word();
        if (bytes.len < 3 * size) return false;
        const length_word = self.readWord(bytes, 2) orelse return false;
        const small = switch (size) {
            4 => @as(i32, @bitCast(@as(u32, @intCast(length_word)))) < 0,
            8 => @as(i64, @bitCast(length_word)) < 0,
            else => return false,
        };
        const len: u64 = if (small) bytes[3 * size - 1] & 0x7f else length_word;
        return len == 0;
    }

    fn decodeList(self: *Decoder, data_export: ?*const Program.StaticDataExport, bytes: []const u8, offset: usize, element_plan: Program.ConstPlanId, value_layout: layout.Layout) Allocator.Error!?Construction {
        if (value_layout.tag != .list) return null;
        const len = self.readWord(bytes, 1) orelse return null;
        if (len == 0) {
            var capacity: u64 = 0;
            if (data_export) |exported| {
                for (exported.empty_list_capacities) |item| {
                    if (item.offset == offset) capacity = item.capacity;
                }
            }
            return .{ .empty_list = capacity };
        }
        const exported = data_export orelse return null;
        const frozen = self.frozen orelse return null;
        const relocation = relocationAt(exported, offset) orelse return null;
        const backing = exportNamed(frozen, relocation.target_symbol_name) orelse return null;
        const element_layout = value_layout.getIdx();
        const element_size = self.program.layouts.layoutSize(self.program.layouts.getLayout(element_layout));
        if (element_size == 0) return null;
        // The elements follow the backing's allocation header; the
        // relocation's addend is that header's size.
        if (relocation.addend < 0) return null;
        const start = backing.symbol_offset + @as(usize, @intCast(relocation.addend));
        if (start > backing.bytes.len) return null;
        const elements = backing.bytes[start..];
        if (elements.len < len * element_size) return null;
        const first = elements[0..element_size];
        var index: usize = 1;
        while (index < len) : (index += 1) {
            if (!std.mem.eql(u8, first, elements[index * element_size ..][0..element_size])) return null;
        }
        const element = try self.decode(backing, elements, start, element_plan, element_layout) orelse return null;
        const stored = try self.arena.create(Construction);
        stored.* = element;
        return .{ .uniform_list = .{ .element = stored, .count = len } };
    }

    fn decodeRecord(self: *Decoder, data_export: ?*const Program.StaticDataExport, bytes: []const u8, offset: usize, child_plans: []const Program.ConstPlanId, value_layout: layout.Layout) Allocator.Error!?Construction {
        if (value_layout.tag != .struct_) return null;
        const struct_idx = value_layout.getStruct().idx;
        const fields = try self.arena.alloc(Construction, child_plans.len);
        for (child_plans, 0..) |child_plan, original_index| {
            const field_layout = self.program.layouts.getStructFieldLayoutByOriginalIndex(struct_idx, @intCast(original_index));
            const field_offset = self.program.layouts.getStructFieldOffsetByOriginalIndex(struct_idx, @intCast(original_index));
            if (bytes.len < field_offset) return null;
            fields[original_index] = try self.decode(data_export, bytes[field_offset..], offset + field_offset, child_plan, field_layout) orelse return null;
        }
        return .{ .record = fields };
    }

    fn decodeTag(self: *Decoder, data_export: ?*const Program.StaticDataExport, bytes: []const u8, offset: usize, variants: []const Program.ConstTagVariant, layout_idx: layout.Idx) Allocator.Error!?Construction {
        // A payload-free two-variant union is a byte: its discriminant is
        // the value.
        if (layout_idx == .bool) {
            if (bytes.len < 1) return null;
            return .{ .literal = .{ .i128_literal = .{ .value = bytes[0], .layout_idx = .bool } } };
        }
        const value_layout = self.program.layouts.getLayout(layout_idx);
        if (value_layout.tag != .tag_union) return null;
        const data = self.program.layouts.getTagUnionData(value_layout.getTagUnion().idx);
        if (bytes.len < data.size.get(self.program.layouts.targetUsize())) return null;
        const discriminant = data.readDiscriminant(bytes.ptr, self.program.layouts.targetUsize());
        const layout_variants = self.program.layouts.getTagUnionVariants(data);
        if (discriminant >= layout_variants.len) return null;
        const payload_layout = layout_variants.get(discriminant).payload_layout;
        for (variants) |variant| {
            if (variant.discriminant != discriminant) continue;
            var payload: ?*const Construction = null;
            if (variant.payloads.len == 1) {
                const stored = try self.arena.create(Construction);
                stored.* = try self.decode(data_export, bytes, offset, variant.payloads[0], payload_layout) orelse return null;
                payload = stored;
            } else if (variant.payloads.len > 1) {
                const stored = try self.arena.create(Construction);
                stored.* = try self.decodeRecord(data_export, bytes, offset, variant.payloads, self.program.layouts.getLayout(payload_layout)) orelse return null;
                payload = stored;
            }
            return .{ .tag = .{ .variant_index = @intCast(discriminant), .discriminant = @intCast(discriminant), .payload = payload } };
        }
        return null;
    }
};

fn relocationAt(data_export: *const Program.StaticDataExport, offset: usize) ?Program.StaticDataRelocation {
    for (data_export.relocations) |relocation| {
        if (relocation.offset == offset) return relocation;
    }
    return null;
}

fn exportNamed(frozen: *const Program.FrozenStaticData, name: []const u8) ?*const Program.StaticDataExport {
    for (frozen.exports) |*item| {
        if (std.mem.eql(u8, item.symbol_name, name)) return item;
    }
    return null;
}

/// Whether the completed value in `slot` is a successful root: its failure
/// record's `failed` byte is zero in the frozen image.
fn slotSucceeded(program: *const Program.Result, frozen: *const Program.FrozenStaticData, slot: LIR.StaticDataId) bool {
    const root = program.static_data_values.items[@intFromEnum(slot)].compile_time_root orelse return false;
    if (root.role != .value) return false;
    const failure_slot = root.role.value.failure_slot;
    const failure_root = program.static_data_values.items[@intFromEnum(failure_slot)].compile_time_root orelse return false;
    if (failure_root.role != .failure_message) return false;
    const failure_export = exportOf(frozen, failure_slot) orelse return false;
    const offset = failure_export.symbol_offset + failure_root.role.failure_message.failed_offset;
    if (offset >= failure_export.bytes.len) return false;
    return failure_export.bytes[offset] == 0;
}

fn exportOf(frozen: *const Program.FrozenStaticData, slot: LIR.StaticDataId) ?*const Program.StaticDataExport {
    for (frozen.exports) |*item| {
        if (item.value_id == slot) return item;
    }
    return null;
}

/// The literal form of a scalar's target bytes, or null for a layout the
/// LIR has no literal for.
fn decodeScalar(layout_idx: layout.Idx, bytes: []const u8) ?LIR.LiteralValue {
    return switch (layout_idx) {
        .u8 => intLiteral(u8, layout_idx, bytes),
        .i8 => intLiteral(i8, layout_idx, bytes),
        .u16 => intLiteral(u16, layout_idx, bytes),
        .i16 => intLiteral(i16, layout_idx, bytes),
        .u32 => intLiteral(u32, layout_idx, bytes),
        .i32 => intLiteral(i32, layout_idx, bytes),
        .u64 => intLiteral(u64, layout_idx, bytes),
        .i64 => intLiteral(i64, layout_idx, bytes),
        .u128 => intLiteral(u128, layout_idx, bytes),
        .i128 => intLiteral(i128, layout_idx, bytes),
        .f32 => if (bytes.len >= 4) .{ .f32_literal = @bitCast(std.mem.readInt(u32, bytes[0..4], .little)) } else null,
        .f64 => if (bytes.len >= 8) .{ .f64_literal = @bitCast(std.mem.readInt(u64, bytes[0..8], .little)) } else null,
        .dec => if (bytes.len >= 16) .{ .dec_literal = std.mem.readInt(i128, bytes[0..16], .little) } else null,
        .bool => intLiteral(u8, layout_idx, bytes),
        .str, .opaque_ptr, .zst, .u8x16, .i8x16, .u16x8, .i16x8, .u32x4, .i32x4, .u64x2, .i64x2 => null,
        _ => null,
    };
}

fn intLiteral(comptime Int: type, layout_idx: layout.Idx, bytes: []const u8) ?LIR.LiteralValue {
    const size = @sizeOf(Int);
    if (bytes.len < size) return null;
    const value = std.mem.readInt(Int, bytes[0..size], .little);
    // An integer literal carries its value's two's-complement bits in i128,
    // as source literals do: a u128 above the i128 range keeps its bit
    // pattern, and every narrower integer extends losslessly.
    return .{ .i128_literal = .{
        .value = if (Int == u128) @bitCast(value) else value,
        .layout_idx = layout_idx,
    } };
}

test "a u128 scalar above the i128 range decodes to its bit pattern" {
    const max_bytes = [_]u8{0xff} ** 16;
    const max = decodeScalar(.u128, &max_bytes) orelse return error.TestUnexpectedResult;
    try std.testing.expectEqual(@as(u128, std.math.maxInt(u128)), @as(u128, @bitCast(max.i128_literal.value)));
    try std.testing.expectEqual(layout.Idx.u128, max.i128_literal.layout_idx);

    const high_bit_bytes = [_]u8{0} ** 15 ++ [_]u8{0x80};
    const high_bit = decodeScalar(.u128, &high_bit_bytes) orelse return error.TestUnexpectedResult;
    try std.testing.expectEqual(@as(u128, 1) << 127, @as(u128, @bitCast(high_bit.i128_literal.value)));

    const min_i128 = decodeScalar(.i128, &high_bit_bytes) orelse return error.TestUnexpectedResult;
    try std.testing.expectEqual(@as(i128, std.math.minInt(i128)), min_i128.i128_literal.value);
}

test "completed successful scalar roots decode to literals; failed and aggregate roots do not" {
    const allocator = std.testing.allocator;
    var program = try Program.Result.init(allocator, .u64);
    defer program.deinit();
    const record_layout = try program.layouts.putStructFields(&.{ .{ .index = 0, .layout = .u8 }, .{ .index = 1, .layout = .str } });
    const struct_idx = program.layouts.getLayout(record_layout).getStruct().idx;
    const failed_offset = program.layouts.getStructFieldOffsetByOriginalIndex(struct_idx, 0);
    const message_offset = program.layouts.getStructFieldOffsetByOriginalIndex(struct_idx, 1);
    const plan: Program.ConstPlanId = @enumFromInt(program.const_plans.items.len);
    try program.const_plans.append(allocator, .scalar);

    // Slots: 0 = failure record of 1, 1 = successful u32 root, 2 = failure
    // record of 3, 3 = failed u32 root, 4 = failure record of 5 and 6, 5 =
    // successful i16 root, 6 = successful string root.
    const roles = [_]enum { failure, value, string }{ .failure, .value, .failure, .value, .failure, .value, .string };
    for (roles, 0..) |role, index| {
        try program.static_data_values.append(allocator, .{
            .initializer = null,
            .layout_idx = switch (role) {
                .failure => record_layout,
                .value => if (index == 5) .i16 else .u32,
                .string => .str,
            },
            .compile_time_root = .{
                .module = .{},
                .root = @enumFromInt(index),
                .const_locator = null,
                .role = switch (role) {
                    .failure => .{ .failure_message = .{ .failed_field = 0, .message_field = 1, .failed_offset = failed_offset, .message_offset = message_offset } },
                    .value => .{ .value = .{ .failure_slot = @enumFromInt(index - 1), .plan = plan } },
                    .string => .{ .value = .{ .failure_slot = @enumFromInt(4), .plan = plan } },
                },
            },
        });
    }
    var ok_record = [_]u8{0} ** 32;
    var failed_record = [_]u8{0} ** 32;
    failed_record[failed_offset] = 1;
    const relocation = [_]Program.StaticDataRelocation{.{ .offset = 0, .target_symbol_name = "backing" }};
    var exports = [_]Program.StaticDataExport{
        .{ .symbol_name = "s0", .bytes = &ok_record, .alignment = 8 },
        .{ .symbol_name = "s1", .bytes = &.{ 0x39, 0x30, 0, 0 }, .alignment = 4 },
        .{ .symbol_name = "s2", .bytes = &failed_record, .alignment = 8 },
        .{ .symbol_name = "s3", .bytes = &.{ 7, 0, 0, 0 }, .alignment = 4 },
        .{ .symbol_name = "s4", .bytes = &ok_record, .alignment = 8 },
        .{ .symbol_name = "s5", .bytes = &.{ 0xfe, 0xff }, .alignment = 2 },
        .{ .symbol_name = "s6", .bytes = &([_]u8{0} ** 24), .alignment = 8, .relocations = &relocation },
    };
    // This program's static roots are exported densely in root order, so each
    // export carries the id of its own position.
    for (&exports, 0..) |*item, index| item.value_id = @enumFromInt(@as(u32, @intCast(index)));
    const frozen = Program.FrozenStaticData{ .allocator = allocator, .exports = &exports };

    var values = try CompletedScalarValues.init(allocator, &program, &frozen);
    defer values.deinit(allocator);
    const first = values.literalFor(.{}, @enumFromInt(1), .u32) orelse return error.TestUnexpectedResult;
    try std.testing.expectEqual(@as(i128, 12345), first.i128_literal.value);
    try std.testing.expectEqual(layout.Idx.u32, first.i128_literal.layout_idx);
    try std.testing.expect(values.literalFor(.{}, @enumFromInt(1), .u64) == null);
    try std.testing.expect(values.literalFor(.{}, @enumFromInt(3), .u32) == null);
    const third = values.literalFor(.{}, @enumFromInt(5), .i16) orelse return error.TestUnexpectedResult;
    try std.testing.expectEqual(@as(i128, -2), third.i128_literal.value);
    try std.testing.expect(values.literalFor(.{}, @enumFromInt(6), .str) == null);
}

test "completed empty and uniform list roots decode to their constructions" {
    const allocator = std.testing.allocator;
    var program = try Program.Result.init(allocator, .u64);
    defer program.deinit();
    const record_layout = try program.layouts.putStructFields(&.{ .{ .index = 0, .layout = .u8 }, .{ .index = 1, .layout = .str } });
    const struct_idx = program.layouts.getLayout(record_layout).getStruct().idx;
    const failed_offset = program.layouts.getStructFieldOffsetByOriginalIndex(struct_idx, 0);
    const message_offset = program.layouts.getStructFieldOffsetByOriginalIndex(struct_idx, 1);
    const list_layout = try program.layouts.insertList(.u32);
    const scalar_plan: Program.ConstPlanId = @enumFromInt(program.const_plans.items.len);
    try program.const_plans.append(allocator, .scalar);
    const plan: Program.ConstPlanId = @enumFromInt(program.const_plans.items.len);
    try program.const_plans.append(allocator, .{ .list = scalar_plan });
    // Slots: 0 = failure record of 1, 2 and 3; 1 = empty list evaluated with
    // capacity 16; 2 = three copies of 7; 3 = the list [1, 2, 3].
    const failure_slot: LIR.StaticDataId = @enumFromInt(program.static_data_values.items.len);
    for (0..4) |index| {
        try program.static_data_values.append(allocator, .{
            .initializer = null,
            .layout_idx = if (index == 0) record_layout else list_layout,
            .compile_time_root = .{
                .module = .{},
                .root = @enumFromInt(index),
                .const_locator = null,
                .role = if (index == 0)
                    .{ .failure_message = .{ .failed_field = 0, .message_field = 1, .failed_offset = failed_offset, .message_offset = message_offset } }
                else
                    .{ .value = .{ .failure_slot = failure_slot, .plan = plan } },
            },
        });
    }
    var ok_record = [_]u8{0} ** 32;
    var empty_descriptor = [_]u8{0} ** 24;
    var uniform_descriptor = [_]u8{0} ** 24;
    std.mem.writeInt(u64, uniform_descriptor[8..16], 3, .little);
    var varied_descriptor = [_]u8{0} ** 24;
    std.mem.writeInt(u64, varied_descriptor[8..16], 3, .little);
    // A backing starts with a word-sized allocation header that the
    // relocation's addend skips; the varied list's header and first
    // elements are zeros, so reading from the node start would mistake it
    // for a uniform list of zeros.
    const uniform_relocation = [_]Program.StaticDataRelocation{.{ .offset = 0, .target_symbol_name = "uniform_backing", .addend = 8 }};
    const varied_relocation = [_]Program.StaticDataRelocation{.{ .offset = 0, .target_symbol_name = "varied_backing", .addend = 8 }};
    var exports = [_]Program.StaticDataExport{
        .{ .symbol_name = "s0", .bytes = &ok_record, .alignment = 8 },
        .{ .symbol_name = "s1", .bytes = &empty_descriptor, .alignment = 8, .empty_list_capacities = &.{.{ .offset = 0, .capacity = 16 }} },
        .{ .symbol_name = "s2", .bytes = &uniform_descriptor, .alignment = 8, .relocations = &uniform_relocation },
        .{ .symbol_name = "s3", .bytes = &varied_descriptor, .alignment = 8, .relocations = &varied_relocation },
        .{ .symbol_name = "uniform_backing", .bytes = &.{ 0, 0, 0, 0, 0, 0, 0, 0, 7, 0, 0, 0, 7, 0, 0, 0, 7, 0, 0, 0 }, .alignment = 8 },
        .{ .symbol_name = "varied_backing", .bytes = &.{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 0 }, .alignment = 8 },
    };
    for (&exports, 0..) |*item, index| {
        if (index < 4) item.value_id = @enumFromInt(index);
    }
    const frozen = Program.FrozenStaticData{ .allocator = allocator, .exports = &exports };
    var values = try CompletedScalarValues.init(allocator, &program, &frozen);
    defer values.deinit(allocator);

    const empty = values.constructionFor(.{}, @enumFromInt(1), list_layout) orelse return error.TestUnexpectedResult;
    try std.testing.expectEqual(@as(u64, 16), empty.empty_list);
    const uniform = values.constructionFor(.{}, @enumFromInt(2), list_layout) orelse return error.TestUnexpectedResult;
    try std.testing.expectEqual(@as(u64, 3), uniform.uniform_list.count);
    try std.testing.expectEqual(@as(i128, 7), uniform.uniform_list.element.literal.i128_literal.value);
    try std.testing.expectEqual(@as(?LIR.LiteralValue, null), values.literalFor(.{}, @enumFromInt(2), list_layout));
    try std.testing.expectEqual(@as(?Construction, null), values.constructionFor(.{}, @enumFromInt(3), list_layout));

    // A consumer that lowers its own roots interns layouts in its own order,
    // so the list layout it reads the root at is a different index from the
    // decoding program's. The construction still names the root's value and
    // emits under the reader's layouts.
    var reader = try Program.Result.init(allocator, .u64);
    defer reader.deinit();
    _ = try reader.layouts.insertList(.u8);
    _ = try reader.layouts.insertList(.u16);
    const reader_list_layout = try reader.layouts.insertList(.u32);
    try std.testing.expect(reader_list_layout != list_layout);
    const TestEmitContext = struct {
        store: *core.LirStore,
        locals: *std.ArrayList(LIR.LocalId),
        next_join_point: *u32,

        pub fn addLocal(self: @This(), layout_idx: layout.Idx) Allocator.Error!LIR.LocalId {
            const local = try self.store.addLocal(.{ .layout_idx = layout_idx });
            try self.locals.append(self.store.allocator, local);
            return local;
        }

        pub fn freshJoinPointId(self: @This()) LIR.JoinPointId {
            const id: LIR.JoinPointId = @enumFromInt(self.next_join_point.*);
            self.next_join_point.* += 1;
            return id;
        }

        pub fn addJoin(self: @This(), point: LIR.JoinPoint, remainder: LIR.CFStmtId) Allocator.Error!LIR.CFStmtId {
            return try self.store.addCFStmt(.{ .join = .{ .id = point.id, .params = point.params, .body = point.body, .remainder = remainder } });
        }
    };
    var locals: std.ArrayList(LIR.LocalId) = .empty;
    defer locals.deinit(allocator);
    var next_join_point: u32 = 0;
    const ctx = TestEmitContext{ .store = &reader.store, .locals = &locals, .next_join_point = &next_join_point };
    const uniform_target = try ctx.addLocal(reader_list_layout);
    const uniform_ret = try reader.store.addCFStmt(.{ .ret = .{ .value = uniform_target } });
    const built = try emit(ctx, &reader.store, &reader.layouts, uniform_target, values.constructionFor(.{}, @enumFromInt(2), reader_list_layout).?, uniform_ret) orelse return error.TestUnexpectedResult;
    var element_locals: usize = 0;
    var list_locals: usize = 0;
    for (locals.items) |local| {
        const local_layout = reader.store.getLocal(local).layout_idx;
        if (local_layout == .u32) element_locals += 1;
        if (local_layout == reader_list_layout) list_locals += 1;
    }
    try std.testing.expectEqual(@as(usize, 1), element_locals);
    try std.testing.expectEqual(@as(usize, 4), list_locals);
    // The repeat loop enters through its join, whose remainder starts by
    // loading the count.
    const loop_entry = reader.store.getCFStmt(built).join;
    try std.testing.expectEqual(@as(i64, 3), reader.store.getCFStmt(loop_entry.remainder).assign_literal.value.i64_literal.value);
    const empty_target = try ctx.addLocal(reader_list_layout);
    const empty_ret = try reader.store.addCFStmt(.{ .ret = .{ .value = empty_target } });
    const reserved = try emit(ctx, &reader.store, &reader.layouts, empty_target, values.constructionFor(.{}, @enumFromInt(1), reader_list_layout).?, empty_ret) orelse return error.TestUnexpectedResult;
    try std.testing.expectEqual(@as(i64, 16), reader.store.getCFStmt(reserved).assign_literal.value.i64_literal.value);
    // A read at a layout that is not a list has no construction to emit.
    const scalar_target = try ctx.addLocal(.u64);
    const scalar_ret = try reader.store.addCFStmt(.{ .ret = .{ .value = scalar_target } });
    try std.testing.expectEqual(@as(?LIR.CFStmtId, null), try emit(ctx, &reader.store, &reader.layouts, scalar_target, values.constructionFor(.{}, @enumFromInt(2), .u64).?, scalar_ret));
}

test "an empty string root and a record of an empty list and a scalar decode to constructions" {
    const allocator = std.testing.allocator;
    var program = try Program.Result.init(allocator, .u64);
    defer program.deinit();
    const failure_layout = try program.layouts.putStructFields(&.{ .{ .index = 0, .layout = .u8 }, .{ .index = 1, .layout = .str } });
    const failure_idx = program.layouts.getLayout(failure_layout).getStruct().idx;
    const failed_offset = program.layouts.getStructFieldOffsetByOriginalIndex(failure_idx, 0);
    const message_offset = program.layouts.getStructFieldOffsetByOriginalIndex(failure_idx, 1);
    const list_layout = try program.layouts.insertList(.u32);
    const record_layout = try program.layouts.putStructFields(&.{ .{ .index = 0, .layout = list_layout }, .{ .index = 1, .layout = .u64 } });
    const record_idx = program.layouts.getLayout(record_layout).getStruct().idx;
    const list_field_offset = program.layouts.getStructFieldOffsetByOriginalIndex(record_idx, 0);
    const count_field_offset = program.layouts.getStructFieldOffsetByOriginalIndex(record_idx, 1);
    const scalar_plan: Program.ConstPlanId = @enumFromInt(program.const_plans.items.len);
    try program.const_plans.append(allocator, .scalar);
    const str_plan: Program.ConstPlanId = @enumFromInt(program.const_plans.items.len);
    try program.const_plans.append(allocator, .str);
    const list_plan: Program.ConstPlanId = @enumFromInt(program.const_plans.items.len);
    try program.const_plans.append(allocator, .{ .list = scalar_plan });
    const record_plan: Program.ConstPlanId = @enumFromInt(program.const_plans.items.len);
    // The program frees a record plan's children on deinit.
    try program.const_plans.append(allocator, .{ .record = try allocator.dupe(Program.ConstPlanId, &.{ list_plan, scalar_plan }) });
    // Slots: 0 = failure record of the rest; 1 = the empty string; 2 = the
    // record { empty list with capacity 4, 9 }; 3 = the small string "ab".
    const layouts_by_slot = [_]layout.Idx{ failure_layout, .str, record_layout, .str };
    const plans_by_slot = [_]Program.ConstPlanId{ scalar_plan, str_plan, record_plan, str_plan };
    const failure_slot: LIR.StaticDataId = @enumFromInt(program.static_data_values.items.len);
    for (layouts_by_slot, plans_by_slot, 0..) |slot_layout, slot_plan, index| {
        try program.static_data_values.append(allocator, .{
            .initializer = null,
            .layout_idx = slot_layout,
            .compile_time_root = .{
                .module = .{},
                .root = @enumFromInt(index),
                .const_locator = null,
                .role = if (index == 0)
                    .{ .failure_message = .{ .failed_field = 0, .message_field = 1, .failed_offset = failed_offset, .message_offset = message_offset } }
                else
                    .{ .value = .{ .failure_slot = failure_slot, .plan = slot_plan } },
            },
        });
    }
    var ok_record = [_]u8{0} ** 32;
    // Small strings set the top bit of the final byte and keep their
    // length in its low seven bits.
    var empty_string = [_]u8{0} ** 24;
    empty_string[23] = 0x80;
    var short_string = [_]u8{0} ** 24;
    short_string[0] = 'a';
    short_string[1] = 'b';
    short_string[23] = 0x82;
    var record_bytes = [_]u8{0} ** 32;
    std.mem.writeInt(u64, record_bytes[count_field_offset..][0..8], 9, .little);
    var exports = [_]Program.StaticDataExport{
        .{ .symbol_name = "s0", .bytes = &ok_record, .alignment = 8 },
        .{ .symbol_name = "s1", .bytes = &empty_string, .alignment = 8 },
        .{ .symbol_name = "s2", .bytes = &record_bytes, .alignment = 8, .empty_list_capacities = &.{.{ .offset = list_field_offset, .capacity = 4 }} },
        .{ .symbol_name = "s3", .bytes = &short_string, .alignment = 8 },
    };
    for (&exports, 0..) |*item, index| item.value_id = @enumFromInt(index);
    const frozen = Program.FrozenStaticData{ .allocator = allocator, .exports = &exports };
    var values = try CompletedScalarValues.init(allocator, &program, &frozen);
    defer values.deinit(allocator);

    const string = values.constructionFor(.{}, @enumFromInt(1), .str) orelse return error.TestUnexpectedResult;
    try std.testing.expect(string == .empty_str);
    const record = values.constructionFor(.{}, @enumFromInt(2), record_layout) orelse return error.TestUnexpectedResult;
    try std.testing.expectEqual(@as(usize, 2), record.record.len);
    try std.testing.expectEqual(@as(u64, 4), record.record[0].empty_list);
    try std.testing.expectEqual(@as(i128, 9), record.record[1].literal.i128_literal.value);
    try std.testing.expect(values.constructionFor(.{}, @enumFromInt(3), .str) == null);
}
