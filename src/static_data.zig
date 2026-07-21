//! Target-layout readonly data symbols for internal constants and provided data.
//!
//! Static data exports are frozen from closed, construction-only LIR
//! initializer procedures using target-width symbolic memory.

const std = @import("std");

const builtins = @import("builtins");
const check = @import("check");
const layout = @import("layout");
const lir = @import("lir");
const roc_target = @import("roc_target");

const Allocator = std.mem.Allocator;
const Checked = check.CheckedArtifact;
const CheckedModule = check.CheckedModule;
const GuardedList = @import("collections").GuardedList;

/// Immutable data symbol materialized in the target's readonly representation.
pub const StaticDataExport = struct {
    /// Linker-visible symbol name, for example `roc__answer`.
    symbol_name: []const u8,
    /// Fully materialized Roc ABI bytes for the constant.
    bytes: []const u8,
    /// Offset inside `bytes` where `symbol_name` points.
    symbol_offset: u32 = 0,
    /// Required target alignment of the symbol.
    alignment: u32,
    /// Whether an object-file symbol has global linker binding.
    is_global: bool = true,
    /// Whether this symbol is part of the host-visible ABI.
    is_exported: bool = true,
    /// Pointer relocations from this symbol's bytes to other symbols.
    relocations: []const StaticDataRelocation = &.{},
};

/// One explicit pointer relocation inside a readonly static-data symbol.
pub const StaticDataRelocation = struct {
    /// Runtime meaning of a relocation target.
    pub const Kind = enum {
        address,
        function_pointer,
    };

    /// Byte offset inside `StaticDataExport.bytes` where the pointer is stored.
    offset: u64,
    /// Symbol whose address should be written at `offset`.
    target_symbol_name: []const u8,
    /// Addend applied to the target symbol address.
    addend: i64 = 0,
    /// Runtime meaning of the stored pointer.
    kind: Kind = .address,
    /// For an erased-callable function pointer, the byte distance from this
    /// pointer field to the callable's capture bytes.
    callable_capture_offset: ?u32 = null,
    /// Exact LIR procedure named by an erased-callable function relocation.
    ///
    /// In-process consumers use this identity directly; object backends use
    /// `target_symbol_name` as its linker representation.
    procedure: ?lir.LIR.LirProcSpecId = null,
    /// Exact generated RC helper required by this function-pointer relocation.
    ///
    /// Static erased-callable `on_drop` slots are always atomic: their
    /// construction site makes no thread-confinement claim. Backends consume
    /// this identity directly instead of recovering it from a symbol or layout.
    rc_helper: ?layout.RcHelperKey = null,
    /// Whether `target_symbol_name` is owned by this relocation.
    owns_target_symbol_name: bool = false,
};

/// Deterministic cross-object symbol for an atomic generated RC helper.
pub fn atomicRcHelperSymbolName(allocator: Allocator, helper: layout.RcHelperKey) Allocator.Error![]u8 {
    return try std.fmt.allocPrint(allocator, "roc__rc_helper_{x}", .{helper.encode()});
}

/// Collect the distinct explicit RC-helper requirements in a static-data graph.
pub fn collectRequiredRcHelpers(
    allocator: Allocator,
    exports: []const StaticDataExport,
) Allocator.Error![]layout.RcHelperKey {
    var seen = std.AutoHashMap(u64, void).init(allocator);
    defer seen.deinit();
    var result = std.ArrayList(layout.RcHelperKey).empty;
    errdefer result.deinit(allocator);

    for (exports) |data_export| {
        for (data_export.relocations) |relocation| {
            const helper = relocation.rc_helper orelse continue;
            const gop = try seen.getOrPut(helper.encode());
            if (gop.found_existing) continue;
            try result.append(allocator, helper);
        }
    }

    return try result.toOwnedSlice(allocator);
}

/// Deterministic object-file symbol name for an internal LIR procedure.
pub fn procSymbolName(allocator: Allocator, proc_symbol: lir.Symbol) Allocator.Error![]u8 {
    return try std.fmt.allocPrint(allocator, "roc__proc_{x}", .{proc_symbol.raw()});
}

/// Checked modules whose constants can become target static data.
pub const ModuleViews = struct {
    root: ?Checked.LoweringModuleView = null,
    imports: []const Checked.ImportedModuleView = &.{},
};

const MaterializationError = Allocator.Error || error{
    UnsupportedTarget,
};

const PointerTarget = struct {
    symbol_name: []const u8,
    addend: i64,
};

const MaterializedValue = struct {
    bytes: []u8,
    alignment: u32,
    relocations: []StaticDataRelocation,
};

const SymbolicAllocationId = enum(u32) { _ };

const SymbolicRelocation = struct {
    const Target = union(enum) {
        allocation: SymbolicAllocationId,
        procedure: lir.LIR.LirProcSpecId,
        rc_helper: layout.RcHelperKey,
    };

    offset: u32,
    target: Target,
    addend: i64 = 0,
    kind: StaticDataRelocation.Kind = .address,
    callable_capture_offset: ?u32 = null,
};

/// Exact target-width bytes produced by a closed static initializer.
///
/// Relocations keep pointers symbolic until the graph is frozen into backend
/// data exports; no host pointer is ever written into these bytes.
const SymbolicValue = struct {
    layout_idx: layout.Idx,
    bytes: []u8,
    relocations: std.ArrayList(SymbolicRelocation),
};

const SymbolicAllocation = struct {
    payload: SymbolicValue,
    alignment: u32,
    contains_refcounted: bool,
    list_element_count: ?usize,
};

const StaticInitializerMachine = struct {
    arena: std.heap.ArenaAllocator,
    lowered: *const lir.CheckedPipeline.LoweredProgram,
    target_usize: @import("base").target.TargetUsize,
    word_size: u32,
    allocations: std.ArrayList(*SymbolicAllocation),
    static_roots: []?*SymbolicValue,
    static_active: []bool,
    string_backings: std.AutoHashMapUnmanaged(@import("base").StringLiteral.Idx, SymbolicAllocationId),

    fn init(
        child_allocator: Allocator,
        lowered: *const lir.CheckedPipeline.LoweredProgram,
        target_usize: @import("base").target.TargetUsize,
    ) Allocator.Error!StaticInitializerMachine {
        var arena = std.heap.ArenaAllocator.init(child_allocator);
        errdefer arena.deinit();
        const arena_allocator = arena.allocator();
        const static_count = lowered.lir_result.static_data_values.items.len;
        const static_roots = try arena_allocator.alloc(?*SymbolicValue, static_count);
        @memset(static_roots, null);
        const static_active = try arena_allocator.alloc(bool, static_count);
        @memset(static_active, false);
        return .{
            .arena = arena,
            .lowered = lowered,
            .target_usize = target_usize,
            .word_size = target_usize.size(),
            .allocations = .empty,
            .static_roots = static_roots,
            .static_active = static_active,
            .string_backings = .empty,
        };
    }

    fn deinit(self: *StaticInitializerMachine) void {
        self.arena.deinit();
    }

    fn allocator(self: *StaticInitializerMachine) Allocator {
        return self.arena.allocator();
    }

    fn store(self: *const StaticInitializerMachine) *const lir.LirStore {
        return &self.lowered.lir_result.store;
    }

    fn layouts(self: *const StaticInitializerMachine) *const layout.Store {
        return &self.lowered.lir_result.layouts;
    }

    fn layoutValue(self: *const StaticInitializerMachine, layout_idx: layout.Idx) layout.Layout {
        return self.layouts().getLayout(layout_idx);
    }

    fn newValue(self: *StaticInitializerMachine, layout_idx: layout.Idx) Allocator.Error!*SymbolicValue {
        const value = try self.allocator().create(SymbolicValue);
        const bytes = try self.allocator().alloc(u8, self.layouts().layoutSize(self.layoutValue(layout_idx)));
        @memset(bytes, 0);
        value.* = .{
            .layout_idx = layout_idx,
            .bytes = bytes,
            .relocations = .empty,
        };
        return value;
    }

    fn cloneValueAs(
        self: *StaticInitializerMachine,
        source: *const SymbolicValue,
        layout_idx: layout.Idx,
    ) Allocator.Error!*SymbolicValue {
        const result = try self.newValue(layout_idx);
        if (result.bytes.len != source.bytes.len) {
            const target_layout = self.layoutValue(layout_idx);
            if (target_layout.tag == .box_of_zst) return result;
            staticDataInvariant("static initializer explicit reinterpret changed target byte size");
        }
        @memcpy(result.bytes, source.bytes);
        try result.relocations.appendSlice(self.allocator(), source.relocations.items);
        return result;
    }

    fn local(locals: []const ?*SymbolicValue, id: lir.LIR.LocalId) *SymbolicValue {
        return locals[@intFromEnum(id)] orelse
            staticDataInvariant("static initializer read an uninitialized local");
    }

    fn setLocal(
        locals: []?*SymbolicValue,
        id: lir.LIR.LocalId,
        value: *SymbolicValue,
    ) void {
        locals[@intFromEnum(id)] = value;
    }

    fn evaluateStatic(self: *StaticInitializerMachine, id: lir.LIR.StaticDataId) MaterializationError!*SymbolicValue {
        const raw = @intFromEnum(id);
        if (raw >= self.static_roots.len) staticDataInvariant("static initializer referenced an unknown static data value");
        if (self.static_roots[raw]) |root| return root;
        if (self.static_active[raw]) staticDataInvariant("static initializer data dependency graph contained a cycle");
        self.static_active[raw] = true;
        defer self.static_active[raw] = false;
        const root = try self.evaluateProc(self.lowered.lir_result.static_data_values.items[raw].initializer);
        self.static_roots[raw] = root;
        return root;
    }

    fn evaluateProc(
        self: *StaticInitializerMachine,
        proc_id: lir.LIR.LirProcSpecId,
    ) MaterializationError!*SymbolicValue {
        const proc = self.store().getProcSpec(proc_id);
        if (!proc.is_static_initializer) {
            staticDataInvariant("static materializer was asked to execute a runtime procedure");
        }
        if (!proc.args.isEmpty()) staticDataInvariant("static initializer procedure had runtime arguments");
        if (proc.hosted != null) staticDataInvariant("static initializer procedure was hosted");
        const body = proc.body orelse staticDataInvariant("static initializer procedure had no body");
        const locals = try self.allocator().alloc(?*SymbolicValue, self.store().localCount());
        @memset(locals, null);

        var current = body;
        while (true) {
            switch (self.store().getCFStmt(current)) {
                .assign_ref => |assign| {
                    const target_layout = self.store().getLocal(assign.target).layout_idx;
                    const value = try self.evalRef(locals, assign.op, target_layout);
                    setLocal(locals, assign.target, value);
                    current = assign.next;
                },
                .assign_literal => |assign| {
                    const target_layout = self.store().getLocal(assign.target).layout_idx;
                    setLocal(locals, assign.target, try self.evalLiteral(assign.value, target_layout));
                    current = assign.next;
                },
                .assign_packed_erased_fn => |assign| {
                    const target_layout = self.store().getLocal(assign.target).layout_idx;
                    setLocal(locals, assign.target, try self.evalPackedErasedFn(locals, assign, target_layout));
                    current = assign.next;
                },
                .assign_low_level => |assign| {
                    const target_layout = self.store().getLocal(assign.target).layout_idx;
                    setLocal(locals, assign.target, try self.evalLowLevel(locals, assign.op, assign.args, target_layout));
                    current = assign.next;
                },
                .assign_list => |assign| {
                    const target_layout = self.store().getLocal(assign.target).layout_idx;
                    setLocal(locals, assign.target, try self.evalList(locals, assign.elems, target_layout));
                    current = assign.next;
                },
                .assign_struct => |assign| {
                    const target_layout = self.store().getLocal(assign.target).layout_idx;
                    setLocal(locals, assign.target, try self.evalStruct(locals, assign.fields, target_layout));
                    current = assign.next;
                },
                .assign_tag => |assign| {
                    const target_layout = self.store().getLocal(assign.target).layout_idx;
                    setLocal(locals, assign.target, try self.evalTag(
                        locals,
                        assign.variant_index,
                        assign.discriminant,
                        assign.payload,
                        target_layout,
                    ));
                    current = assign.next;
                },
                .set_local => |assign| {
                    const source = local(locals, assign.value);
                    const target_layout = self.store().getLocal(assign.target).layout_idx;
                    setLocal(locals, assign.target, try self.cloneValueAs(source, target_layout));
                    current = assign.next;
                },
                .incref => |arc| current = arc.next,
                .decref => |arc| current = arc.next,
                .decref_if_initialized => |arc| current = arc.next,
                .free => |arc| current = arc.next,
                .ret => |ret| {
                    const value = local(locals, ret.value);
                    if (value.layout_idx != proc.ret_layout) {
                        return try self.cloneValueAs(value, proc.ret_layout);
                    }
                    return value;
                },
                else => staticDataInvariant("non-construction LIR reached static initializer materialization"),
            }
        }
    }

    fn evalRef(
        self: *StaticInitializerMachine,
        locals: []const ?*SymbolicValue,
        op: lir.LIR.RefOp,
        target_layout: layout.Idx,
    ) MaterializationError!*SymbolicValue {
        return switch (op) {
            .local => |source| try self.cloneValueAs(local(locals, source), target_layout),
            .list_reinterpret => |source| try self.cloneValueAs(local(locals, source.backing_ref), target_layout),
            .nominal => |source| try self.cloneValueAs(local(locals, source.backing_ref), target_layout),
            .discriminant,
            .field,
            .tag_payload,
            .tag_payload_struct,
            => staticDataInvariant("projection reached construction-only static initializer"),
        };
    }

    fn evalLiteral(
        self: *StaticInitializerMachine,
        literal: lir.LIR.LiteralValue,
        target_layout: layout.Idx,
    ) MaterializationError!*SymbolicValue {
        const value = try self.newValue(target_layout);
        switch (literal) {
            .i64_literal => |int| self.writeSigned(value.bytes, int.value),
            .i128_literal => |int| self.writeSigned128(value.bytes, int.value),
            .f64_literal => |float| std.mem.writeInt(u64, value.bytes[0..8], @bitCast(float), .little),
            .f32_literal => |float| std.mem.writeInt(u32, value.bytes[0..4], @bitCast(float), .little),
            .dec_literal => |dec| std.mem.writeInt(i128, value.bytes[0..16], dec, .little),
            .str_literal => |str| return try self.evalStr(str, target_layout),
            .static_data => |id| return try self.cloneValueAs(try self.evaluateStatic(id), target_layout),
            .bytes_literal => |str| return try self.evalBytes(str, target_layout),
            .null_ptr => {},
            .proc_ref => |proc| try value.relocations.append(self.allocator(), .{
                .offset = 0,
                .target = .{ .procedure = proc },
                .kind = .function_pointer,
            }),
        }
        return value;
    }

    fn writeSigned(_: *StaticInitializerMachine, bytes: []u8, value: i64) void {
        const bits: u64 = @bitCast(value);
        switch (bytes.len) {
            0 => {},
            1 => std.mem.writeInt(u8, bytes[0..1], @truncate(bits), .little),
            2 => std.mem.writeInt(u16, bytes[0..2], @truncate(bits), .little),
            4 => std.mem.writeInt(u32, bytes[0..4], @truncate(bits), .little),
            8 => std.mem.writeInt(u64, bytes[0..8], bits, .little),
            else => staticDataInvariant("i64 static initializer literal had unsupported layout size"),
        }
    }

    fn writeSigned128(_: *StaticInitializerMachine, bytes: []u8, value: i128) void {
        const bits: u128 = @bitCast(value);
        switch (bytes.len) {
            0 => {},
            1 => std.mem.writeInt(u8, bytes[0..1], @truncate(bits), .little),
            2 => std.mem.writeInt(u16, bytes[0..2], @truncate(bits), .little),
            4 => std.mem.writeInt(u32, bytes[0..4], @truncate(bits), .little),
            8 => std.mem.writeInt(u64, bytes[0..8], @truncate(bits), .little),
            16 => std.mem.writeInt(u128, bytes[0..16], bits, .little),
            else => staticDataInvariant("i128 static initializer literal had unsupported layout size"),
        }
    }

    fn writeTargetWord(self: *const StaticInitializerMachine, bytes: []u8, offset: u32, word: u64) void {
        const unused_bits: std.math.Log2Int(u64) = @intCast((8 - self.word_size) * 8);
        const max_word = @as(u64, std.math.maxInt(u64)) >> unused_bits;
        if (word > max_word) staticDataInvariant("static initializer word exceeded target usize");
        switch (self.word_size) {
            4 => std.mem.writeInt(u32, bytes[offset..][0..4], @intCast(word), .little),
            8 => std.mem.writeInt(u64, bytes[offset..][0..8], word, .little),
            else => unreachable,
        }
    }

    fn copyInto(
        self: *StaticInitializerMachine,
        dest: *SymbolicValue,
        dest_offset: u32,
        source: *const SymbolicValue,
        size: u32,
    ) Allocator.Error!void {
        if (size == 0) return;
        if (size > source.bytes.len or dest_offset > dest.bytes.len or size > dest.bytes.len - dest_offset) {
            staticDataInvariant("static initializer aggregate copy exceeded target bytes");
        }
        @memcpy(dest.bytes[dest_offset..][0..size], source.bytes[0..size]);
        for (source.relocations.items) |relocation| {
            if (relocation.offset >= size) {
                staticDataInvariant("static initializer copied a partial value containing an out-of-range relocation");
            }
            var shifted = relocation;
            shifted.offset += dest_offset;
            try dest.relocations.append(self.allocator(), shifted);
        }
    }

    fn addAllocation(
        self: *StaticInitializerMachine,
        payload_layout: layout.Idx,
        payload_size: usize,
        alignment: u32,
        contains_refcounted: bool,
        list_element_count: ?usize,
    ) Allocator.Error!struct { id: SymbolicAllocationId, payload: *SymbolicValue } {
        const payload = try self.allocator().create(SymbolicValue);
        const bytes = try self.allocator().alloc(u8, payload_size);
        @memset(bytes, 0);
        payload.* = .{
            .layout_idx = payload_layout,
            .bytes = bytes,
            .relocations = .empty,
        };
        const allocation = try self.allocator().create(SymbolicAllocation);
        allocation.* = .{
            .payload = payload.*,
            .alignment = alignment,
            .contains_refcounted = contains_refcounted,
            .list_element_count = list_element_count,
        };
        const id: SymbolicAllocationId = @enumFromInt(@as(u32, @intCast(self.allocations.items.len)));
        try self.allocations.append(self.allocator(), allocation);
        return .{ .id = id, .payload = &allocation.payload };
    }

    fn pointerValue(
        self: *StaticInitializerMachine,
        layout_idx: layout.Idx,
        allocation: SymbolicAllocationId,
        addend: i64,
    ) Allocator.Error!*SymbolicValue {
        const result = try self.newValue(layout_idx);
        if (result.bytes.len != self.word_size) {
            staticDataInvariant("static initializer allocation pointer had non-pointer-sized layout");
        }
        try result.relocations.append(self.allocator(), .{
            .offset = 0,
            .target = .{ .allocation = allocation },
            .addend = addend,
        });
        return result;
    }

    fn stringBacking(
        self: *StaticInitializerMachine,
        backing_id: @import("base").StringLiteral.Idx,
    ) Allocator.Error!SymbolicAllocationId {
        if (self.string_backings.get(backing_id)) |id| return id;
        const backing = self.store().getString(backing_id);
        const allocation = try self.addAllocation(.u8, backing.len, self.word_size, false, null);
        @memcpy(allocation.payload.bytes, backing);
        try self.string_backings.put(self.allocator(), backing_id, allocation.id);
        return allocation.id;
    }

    fn validateLiteralView(self: *const StaticInitializerMachine, literal: lir.LIR.StrLiteral) []const u8 {
        const backing = self.store().getStringLiteralBacking(literal);
        const offset: usize = literal.offset;
        const len: usize = literal.len;
        if (offset > backing.len or len > backing.len - offset) {
            staticDataInvariant("static initializer string literal view exceeded its backing");
        }
        return backing[offset..][0..len];
    }

    fn evalStr(
        self: *StaticInitializerMachine,
        literal: lir.LIR.StrLiteral,
        target_layout: layout.Idx,
    ) MaterializationError!*SymbolicValue {
        const result = try self.newValue(target_layout);
        const backing = self.store().getStringLiteralBacking(literal);
        const bytes = self.validateLiteralView(literal);
        const roc_str_size = self.word_size * builtins.str.RocStr.word_count;
        if (backing.len < roc_str_size and bytes.len < roc_str_size) {
            @memcpy(result.bytes[0..bytes.len], bytes);
            result.bytes[roc_str_size - 1] = builtins.str.RocStr.smallStrFlagByte(bytes.len);
            return result;
        }

        const allocation = try self.stringBacking(literal.backing);
        try result.relocations.append(self.allocator(), .{
            .offset = 0,
            .target = .{ .allocation = allocation },
            .addend = literal.offset,
        });
        const whole_backing = literal.offset == 0 and @as(usize, literal.len) == backing.len;
        if (whole_backing) {
            self.writeTargetWord(
                result.bytes,
                self.word_size,
                builtins.str.RocStr.encodeCapacityForWidth(bytes.len),
            );
        } else {
            try result.relocations.append(self.allocator(), .{
                .offset = self.word_size,
                .target = .{ .allocation = allocation },
                .addend = 1,
            });
        }
        self.writeTargetWord(result.bytes, self.word_size * 2, bytes.len);
        return result;
    }

    fn evalBytes(
        self: *StaticInitializerMachine,
        literal: lir.LIR.StrLiteral,
        target_layout: layout.Idx,
    ) MaterializationError!*SymbolicValue {
        const result = try self.newValue(target_layout);
        const backing = self.store().getStringLiteralBacking(literal);
        const bytes = self.validateLiteralView(literal);
        if (bytes.len == 0) return result;

        const allocation = try self.stringBacking(literal.backing);
        try result.relocations.append(self.allocator(), .{
            .offset = 0,
            .target = .{ .allocation = allocation },
            .addend = literal.offset,
        });
        self.writeTargetWord(result.bytes, self.word_size, bytes.len);
        const whole_backing = literal.offset == 0 and @as(usize, literal.len) == backing.len;
        if (whole_backing) {
            self.writeTargetWord(
                result.bytes,
                self.word_size * 2,
                builtins.list.RocList.encodeCapacityForWidth(bytes.len),
            );
        } else {
            try result.relocations.append(self.allocator(), .{
                .offset = self.word_size * 2,
                .target = .{ .allocation = allocation },
                .addend = 1,
            });
        }
        return result;
    }

    fn evalList(
        self: *StaticInitializerMachine,
        locals: []const ?*SymbolicValue,
        elems_span: lir.LIR.LocalSpan,
        target_layout: layout.Idx,
    ) MaterializationError!*SymbolicValue {
        const result = try self.newValue(target_layout);
        const elems = self.store().getLocalSpan(elems_span);
        self.writeTargetWord(result.bytes, self.word_size, elems.len);
        self.writeTargetWord(
            result.bytes,
            self.word_size * 2,
            builtins.list.RocList.encodeCapacityForWidth(elems.len),
        );
        if (elems.len == 0) return result;

        const abi = self.layouts().builtinListAbi(target_layout);
        if (abi.elem_size == 0) return result;
        const elem_layout = abi.elem_layout_idx orelse
            staticDataInvariant("non-ZST static initializer list had no element layout");
        const allocation = try self.addAllocation(
            elem_layout,
            @as(usize, abi.elem_size) * elems.len,
            abi.elem_alignment,
            abi.contains_refcounted,
            if (abi.contains_refcounted) elems.len else null,
        );
        for (0..elems.len) |index| {
            const elem_local = GuardedList.at(elems, index);
            const elem = try self.cloneValueAs(local(locals, elem_local), elem_layout);
            try self.copyInto(allocation.payload, @intCast(index * abi.elem_size), elem, abi.elem_size);
        }
        try result.relocations.append(self.allocator(), .{
            .offset = 0,
            .target = .{ .allocation = allocation.id },
        });
        return result;
    }

    const AggregateTarget = struct {
        outer: *SymbolicValue,
        base: *SymbolicValue,
        base_layout: layout.Idx,
    };

    fn allocAggregate(
        self: *StaticInitializerMachine,
        target_layout: layout.Idx,
    ) MaterializationError!AggregateTarget {
        const target = self.layoutValue(target_layout);
        return switch (target.tag) {
            .zst => blk: {
                const value = try self.newValue(target_layout);
                break :blk .{ .outer = value, .base = value, .base_layout = target_layout };
            },
            .box_of_zst => blk: {
                const outer = try self.newValue(target_layout);
                const base = try self.newValue(.zst);
                break :blk .{ .outer = outer, .base = base, .base_layout = .zst };
            },
            .box => blk: {
                const abi = self.layouts().builtinBoxAbi(target_layout);
                const elem_layout = abi.elem_layout_idx orelse .zst;
                const allocation = try self.addAllocation(
                    elem_layout,
                    abi.elem_size,
                    abi.elem_alignment,
                    abi.contains_refcounted,
                    null,
                );
                const outer = try self.pointerValue(target_layout, allocation.id, 0);
                break :blk .{ .outer = outer, .base = allocation.payload, .base_layout = elem_layout };
            },
            .struct_, .tag_union => blk: {
                const value = try self.newValue(target_layout);
                break :blk .{ .outer = value, .base = value, .base_layout = target_layout };
            },
            else => staticDataInvariant("static initializer aggregate target had non-aggregate layout"),
        };
    }

    fn evalStruct(
        self: *StaticInitializerMachine,
        locals: []const ?*SymbolicValue,
        fields_span: lir.LIR.LocalSpan,
        target_layout: layout.Idx,
    ) MaterializationError!*SymbolicValue {
        const fields = self.store().getLocalSpan(fields_span);
        const target = try self.allocAggregate(target_layout);
        const base_layout = self.layoutValue(target.base_layout);
        if (base_layout.tag != .struct_) {
            if (fields.len != 0) staticDataInvariant("static initializer ZST struct had runtime fields");
            return target.outer;
        }
        for (0..fields.len) |index| {
            const field_layout = self.layouts().getStructFieldLayoutByOriginalIndex(
                base_layout.getStruct().idx,
                @intCast(index),
            );
            const field_size = self.layouts().layoutSize(self.layoutValue(field_layout));
            if (field_size == 0) continue;
            const field_offset = self.layouts().getStructFieldOffsetByOriginalIndex(
                base_layout.getStruct().idx,
                @intCast(index),
            );
            const field_local = GuardedList.at(fields, index);
            const field = try self.cloneValueAs(local(locals, field_local), field_layout);
            try self.copyInto(target.base, field_offset, field, field_size);
        }
        return target.outer;
    }

    fn tagPayloadLayout(
        self: *const StaticInitializerMachine,
        union_layout: layout.Idx,
        variant_index: u16,
    ) layout.Idx {
        const outer = self.layoutValue(union_layout);
        const tag_layout = switch (outer.tag) {
            .tag_union => outer,
            .box => self.layoutValue(self.layouts().builtinBoxAbi(union_layout).elem_layout_idx orelse .zst),
            else => return .zst,
        };
        if (tag_layout.tag != .tag_union) return .zst;
        const info = self.layouts().getTagUnionInfo(tag_layout);
        if (variant_index >= info.variants.len) return .zst;
        return info.variants.get(variant_index).payload_layout;
    }

    fn evalTag(
        self: *StaticInitializerMachine,
        locals: []const ?*SymbolicValue,
        variant_index: u16,
        discriminant: u16,
        payload_local: ?lir.LIR.LocalId,
        target_layout: layout.Idx,
    ) MaterializationError!*SymbolicValue {
        const target = try self.allocAggregate(target_layout);
        const base_layout = self.layoutValue(target.base_layout);
        if (base_layout.tag == .tag_union) {
            const tag_data = self.layouts().getTagUnionData(base_layout.getTagUnion().idx);
            tag_data.writeDiscriminant(target.base.bytes.ptr, discriminant, self.target_usize);
        } else if (discriminant != 0 and target.base.bytes.len == 0) {
            staticDataInvariant("nonzero static initializer tag discriminant had a ZST layout");
        }

        if (payload_local) |payload_id| {
            const payload_layout = self.tagPayloadLayout(target_layout, variant_index);
            const payload_size = self.layouts().layoutSize(self.layoutValue(payload_layout));
            if (payload_size != 0) {
                const payload = try self.cloneValueAs(local(locals, payload_id), payload_layout);
                try self.copyInto(target.base, 0, payload, payload_size);
            }
        }
        return target.outer;
    }

    fn evalLowLevel(
        self: *StaticInitializerMachine,
        locals: []const ?*SymbolicValue,
        op: lir.LIR.LowLevel,
        args_span: lir.LIR.LocalSpan,
        target_layout: layout.Idx,
    ) MaterializationError!*SymbolicValue {
        const args = self.store().getLocalSpan(args_span);
        return switch (op) {
            .box_box => blk: {
                if (args.len != 1) staticDataInvariant("static initializer box_box arity differed from one");
                const target = self.layoutValue(target_layout);
                if (target.tag == .box_of_zst) break :blk try self.newValue(target_layout);
                if (target.tag != .box) staticDataInvariant("static initializer box_box had non-box target layout");
                const abi = self.layouts().builtinBoxAbi(target_layout);
                const elem_layout = abi.elem_layout_idx orelse .zst;
                const allocation = try self.addAllocation(
                    elem_layout,
                    abi.elem_size,
                    abi.elem_alignment,
                    abi.contains_refcounted,
                    null,
                );
                const arg = try self.cloneValueAs(local(locals, GuardedList.at(args, 0)), elem_layout);
                try self.copyInto(allocation.payload, 0, arg, abi.elem_size);
                break :blk try self.pointerValue(target_layout, allocation.id, 0);
            },
            else => staticDataInvariant("non-construction low-level operation reached static initializer materialization"),
        };
    }

    fn evalPackedErasedFn(
        self: *StaticInitializerMachine,
        locals: []const ?*SymbolicValue,
        assign: anytype,
        target_layout: layout.Idx,
    ) MaterializationError!*SymbolicValue {
        if (assign.reuse != null) {
            staticDataInvariant("erased callable allocation reuse reached closed static initializer");
        }
        const capture_size: usize = if (assign.capture_layout) |capture_layout|
            self.layouts().layoutSize(self.layoutValue(capture_layout))
        else
            0;
        const allocation = try self.addAllocation(
            target_layout,
            builtins.erased_callable.payloadSize(capture_size),
            builtins.erased_callable.payload_alignment,
            builtins.erased_callable.allocation_has_refcounted_children,
            null,
        );
        try allocation.payload.relocations.append(self.allocator(), .{
            .offset = 0,
            .target = .{ .procedure = assign.proc },
            .kind = .function_pointer,
            .callable_capture_offset = builtins.erased_callable.capture_offset,
        });
        switch (assign.on_drop) {
            .none => {},
            .rc_helper => |helper| {
                if (self.layouts().rcHelperPlan(helper) == .noop) {
                    staticDataInvariant("static erased callable named a no-op RC helper");
                }
                try allocation.payload.relocations.append(self.allocator(), .{
                    .offset = self.word_size,
                    .target = .{ .rc_helper = helper },
                    .kind = .function_pointer,
                });
            },
            .interpreter_context_drop => staticDataInvariant("interpreter erased callable reached target static initializer"),
        }
        if (assign.capture) |capture_local| {
            const capture_layout = assign.capture_layout orelse
                staticDataInvariant("static erased callable capture lacked a layout");
            const capture = try self.cloneValueAs(local(locals, capture_local), capture_layout);
            try self.copyInto(
                allocation.payload,
                builtins.erased_callable.capture_offset,
                capture,
                @intCast(capture_size),
            );
        } else if (assign.capture_layout != null) {
            staticDataInvariant("static erased callable capture layout lacked a capture");
        }
        return try self.pointerValue(target_layout, allocation.id, 0);
    }
};

/// Selects which closed LIR initializers are frozen into static-data exports.
pub const BuildOptions = struct {
    /// Include host-visible provided constants as well as internal LIR values.
    include_provided_exports: bool = false,
};

/// Build readonly data symbols for internal LIR values and optional provided constants.
pub fn buildStaticData(
    allocator: Allocator,
    modules: ModuleViews,
    lowered: ?*const lir.CheckedPipeline.LoweredProgram,
    target: roc_target.RocTarget,
    options: BuildOptions,
) MaterializationError![]StaticDataExport {
    const root = modules.root orelse {
        if (hasProvidedData(modules)) staticDataInvariant("provided data exports require a root checked module");
        if (lowered) |lowered_program| {
            if (lowered_program.lir_result.static_data_values.items.len != 0) {
                staticDataInvariant("internal static data values require a root checked module");
            }
        }
        return try allocator.alloc(StaticDataExport, 0);
    };
    const lowered_program = lowered orelse {
        if (moduleHasProvidedData(root.module)) staticDataInvariant("static data exports require LIR layout output");
        return try allocator.alloc(StaticDataExport, 0);
    };

    var builder = (try StaticDataBuilder.init(allocator, root, lowered_program, target, options)) orelse
        return error.UnsupportedTarget;
    defer builder.deinitScratch();
    return try builder.build();
}

pub fn deinitStaticData(allocator: Allocator, exports: []StaticDataExport) void {
    for (exports) |static_export| {
        allocator.free(static_export.symbol_name);
        allocator.free(static_export.bytes);
        deinitRelocationSlice(allocator, static_export.relocations);
        allocator.free(static_export.relocations);
    }
    allocator.free(exports);
}

fn deinitRelocationSlice(allocator: Allocator, relocations: []const StaticDataRelocation) void {
    for (relocations) |relocation| {
        if (relocation.owns_target_symbol_name) allocator.free(relocation.target_symbol_name);
    }
}

const StaticDataBuilder = struct {
    allocator: Allocator,
    root: Checked.LoweringModuleView,
    lowered: *const lir.CheckedPipeline.LoweredProgram,
    target_usize: @import("base").target.TargetUsize,
    word_size: u32,
    nodes: std.ArrayList(StaticDataExport),
    initializer_machine: StaticInitializerMachine,
    frozen_allocations: std.AutoHashMap(SymbolicAllocationId, PointerTarget),
    local_symbol_ordinal: u32,
    include_provided_exports: bool,

    fn init(
        allocator: Allocator,
        root: Checked.LoweringModuleView,
        lowered: *const lir.CheckedPipeline.LoweredProgram,
        target: roc_target.RocTarget,
        options: BuildOptions,
    ) MaterializationError!?StaticDataBuilder {
        const target_usize = @import("base").target.TargetUsize.fromPtrBitWidth(target.ptrBitWidth());
        return .{
            .allocator = allocator,
            .root = root,
            .lowered = lowered,
            .target_usize = target_usize,
            .word_size = target_usize.size(),
            .nodes = .empty,
            .initializer_machine = try StaticInitializerMachine.init(allocator, lowered, target_usize),
            .frozen_allocations = std.AutoHashMap(SymbolicAllocationId, PointerTarget).init(allocator),
            .local_symbol_ordinal = 0,
            .include_provided_exports = options.include_provided_exports,
        };
    }

    fn deinitScratch(self: *StaticDataBuilder) void {
        self.initializer_machine.deinit();
        self.frozen_allocations.deinit();
    }

    fn build(self: *StaticDataBuilder) MaterializationError![]StaticDataExport {
        errdefer self.deinitNodes();

        if (self.include_provided_exports) try self.buildProvidedExports();
        try self.buildInternalStaticValues();

        return try self.nodes.toOwnedSlice(self.allocator);
    }

    fn buildProvidedExports(self: *StaticDataBuilder) MaterializationError!void {
        for (self.root.module.provided_exports.exports) |provided| {
            const data = switch (provided) {
                .data => |data| data,
                .procedure => continue,
            };

            const request = self.requestedLayout(data.const_ref);
            const initializer = request.initializer orelse
                staticDataInvariant("provided static data export had no closed LIR initializer");
            const entrypoint_name = self.root.module.canonical_names.externalSymbolNameText(data.ffi_symbol);
            const symbol_name = try self.allocator.dupe(u8, entrypoint_name);
            errdefer self.allocator.free(symbol_name);

            const value = try self.initializer_machine.evaluateProc(initializer);
            if (value.layout_idx != request.layout_idx) {
                staticDataInvariant("provided static initializer return layout differed from requested layout");
            }
            const materialized = try self.freezeValue(value);
            errdefer self.deinitMaterialized(materialized);

            try self.nodes.append(self.allocator, .{
                .symbol_name = symbol_name,
                .bytes = materialized.bytes,
                .alignment = materialized.alignment,
                .is_global = true,
                .is_exported = true,
                .relocations = materialized.relocations,
            });
        }
    }

    fn buildInternalStaticValues(self: *StaticDataBuilder) MaterializationError!void {
        for (0..self.lowered.lir_result.static_data_values.items.len) |index| {
            const static_data_id: lir.LIR.StaticDataId = @enumFromInt(@as(u32, @intCast(index)));
            const symbol_name = try lir.Program.staticDataSymbolName(self.allocator, static_data_id);
            errdefer self.allocator.free(symbol_name);

            const initialized = try self.initializer_machine.evaluateStatic(static_data_id);
            const materialized = try self.freezeValue(initialized);
            errdefer self.deinitMaterialized(materialized);

            try self.nodes.append(self.allocator, .{
                .symbol_name = symbol_name,
                .bytes = materialized.bytes,
                .alignment = materialized.alignment,
                .is_global = true,
                .is_exported = false,
                .relocations = materialized.relocations,
            });
        }
    }

    fn requestedLayout(self: *StaticDataBuilder, const_locator: CheckedModule.ConstLocator) lir.Program.RequestedLayout {
        for (self.lowered.lir_result.requested_layouts.items) |request| {
            const request_locator = request.const_locator orelse continue;
            if (std.meta.eql(request_locator, const_locator) and request.initializer != null) return request;
        }
        staticDataInvariant("provided data export had no initialized LIR layout request");
    }

    fn freezeValue(
        self: *StaticDataBuilder,
        value: *const SymbolicValue,
    ) MaterializationError!MaterializedValue {
        const bytes = try self.allocator.dupe(u8, value.bytes);
        errdefer self.allocator.free(bytes);
        const relocations = try self.freezeRelocations(value.relocations.items);
        errdefer {
            deinitRelocationSlice(self.allocator, relocations);
            self.allocator.free(relocations);
        }
        const alignment: u32 = @intCast(self.layoutValue(value.layout_idx).alignment(self.target_usize).toByteUnits());
        return .{
            .bytes = bytes,
            .alignment = alignment,
            .relocations = relocations,
        };
    }

    fn freezeRelocations(
        self: *StaticDataBuilder,
        symbolic: []const SymbolicRelocation,
    ) MaterializationError![]StaticDataRelocation {
        const result = try self.allocator.alloc(StaticDataRelocation, symbolic.len);
        var written: usize = 0;
        errdefer {
            deinitRelocationSlice(self.allocator, result[0..written]);
            self.allocator.free(result);
        }
        for (symbolic, result) |source, *dest| {
            switch (source.target) {
                .allocation => |allocation| {
                    const target = try self.freezeAllocation(allocation);
                    dest.* = .{
                        .offset = source.offset,
                        .target_symbol_name = target.symbol_name,
                        .addend = target.addend + source.addend,
                        .kind = source.kind,
                        .callable_capture_offset = source.callable_capture_offset,
                    };
                },
                .procedure => |proc_id| {
                    const proc = self.lowered.lir_result.store.getProcSpec(proc_id);
                    dest.* = .{
                        .offset = source.offset,
                        .target_symbol_name = try procSymbolName(self.allocator, proc.name),
                        .addend = source.addend,
                        .kind = .function_pointer,
                        .callable_capture_offset = source.callable_capture_offset,
                        .procedure = proc_id,
                        .owns_target_symbol_name = true,
                    };
                },
                .rc_helper => |helper| {
                    dest.* = .{
                        .offset = source.offset,
                        .target_symbol_name = try atomicRcHelperSymbolName(self.allocator, helper),
                        .addend = source.addend,
                        .kind = .function_pointer,
                        .callable_capture_offset = source.callable_capture_offset,
                        .rc_helper = helper,
                        .owns_target_symbol_name = true,
                    };
                },
            }
            written += 1;
        }
        return result;
    }

    fn freezeAllocation(
        self: *StaticDataBuilder,
        id: SymbolicAllocationId,
    ) MaterializationError!PointerTarget {
        if (self.frozen_allocations.get(id)) |target| return target;
        const raw = @intFromEnum(id);
        if (raw >= self.initializer_machine.allocations.items.len) {
            staticDataInvariant("static initializer relocation referenced an unknown allocation");
        }
        const allocation = self.initializer_machine.allocations.items[raw];

        // Reserve the symbol before following its relocations. This makes the
        // target-memory graph capable of representing recursive allocation
        // cycles without reconstructing or breaking them.
        const symbol_name = try std.fmt.allocPrint(self.allocator, "roc__static_const_{d}", .{self.local_symbol_ordinal});
        var node_appended = false;
        errdefer if (!node_appended) self.allocator.free(symbol_name);
        self.local_symbol_ordinal += 1;
        const data_offset = staticDataPtrOffset(self.word_size, allocation.alignment, allocation.contains_refcounted);
        const bytes = try self.allocator.alloc(u8, data_offset + allocation.payload.bytes.len);
        errdefer if (!node_appended) self.allocator.free(bytes);
        @memset(bytes, 0);
        @memcpy(bytes[data_offset..][0..allocation.payload.bytes.len], allocation.payload.bytes);
        if (allocation.contains_refcounted) {
            self.writeTargetWord(
                bytes,
                data_offset - self.word_size * 2,
                if (allocation.list_element_count) |count| @intCast(count) else 0,
            );
        }
        self.writeTargetSignedWord(bytes, data_offset - self.word_size, 0);
        const empty_relocations = try self.allocator.alloc(StaticDataRelocation, 0);
        errdefer if (!node_appended) self.allocator.free(empty_relocations);
        const node_index = self.nodes.items.len;
        try self.nodes.append(self.allocator, .{
            .symbol_name = symbol_name,
            .bytes = bytes,
            .alignment = @max(allocation.alignment, self.word_size),
            .is_global = false,
            .is_exported = false,
            .relocations = empty_relocations,
        });
        node_appended = true;
        const target = PointerTarget{
            .symbol_name = symbol_name,
            .addend = @intCast(data_offset),
        };
        try self.frozen_allocations.put(id, target);

        const relocations = try self.freezeRelocations(allocation.payload.relocations.items);
        for (relocations) |*relocation| relocation.offset += data_offset;
        self.allocator.free(self.nodes.items[node_index].relocations);
        self.nodes.items[node_index].relocations = relocations;
        return target;
    }

    fn deinitNodes(self: *StaticDataBuilder) void {
        for (self.nodes.items) |node| {
            self.allocator.free(node.symbol_name);
            self.allocator.free(node.bytes);
            deinitRelocationSlice(self.allocator, node.relocations);
            self.allocator.free(node.relocations);
        }
        self.nodes.deinit(self.allocator);
    }

    fn deinitMaterialized(self: *StaticDataBuilder, value: MaterializedValue) void {
        self.allocator.free(value.bytes);
        deinitRelocationSlice(self.allocator, value.relocations);
        self.allocator.free(value.relocations);
    }

    fn layouts(self: *StaticDataBuilder) *const layout.Store {
        return &self.lowered.lir_result.layouts;
    }

    fn layoutValue(self: *StaticDataBuilder, layout_idx: layout.Idx) layout.Layout {
        return self.layouts().getLayout(layout_idx);
    }

    fn writeTargetWord(self: *StaticDataBuilder, bytes: []u8, offset: u32, value: u64) void {
        if (value > self.targetWordMax()) staticDataInvariant("static data word exceeds target usize");
        switch (self.word_size) {
            4 => std.mem.writeInt(u32, bytes[offset..][0..4], @intCast(value), .little),
            8 => std.mem.writeInt(u64, bytes[offset..][0..8], value, .little),
            else => unreachable,
        }
    }

    fn writeTargetSignedWord(self: *StaticDataBuilder, bytes: []u8, offset: u32, value: i64) void {
        switch (self.word_size) {
            4 => std.mem.writeInt(i32, bytes[offset..][0..4], @intCast(value), .little),
            8 => std.mem.writeInt(i64, bytes[offset..][0..8], value, .little),
            else => unreachable,
        }
    }

    fn targetWordMax(self: *StaticDataBuilder) u64 {
        const unused_bits: std.math.Log2Int(u64) = @intCast((8 - self.word_size) * 8);
        return @as(u64, std.math.maxInt(u64)) >> unused_bits;
    }
};

fn moduleHasProvidedData(module: *const Checked.CheckedModuleArtifact) bool {
    for (module.provided_exports.exports) |provided| {
        switch (provided) {
            .data => return true,
            .procedure => {},
        }
    }
    return false;
}

fn hasProvidedData(modules: ModuleViews) bool {
    if (modules.root) |root| {
        if (moduleHasProvidedData(root.module)) return true;
    }
    return false;
}

fn staticDataPtrOffset(word_size: u32, element_alignment: u32, contains_refcounted: bool) u32 {
    const required_space = if (contains_refcounted) word_size * 2 else word_size;
    return alignForwardU32(required_space, element_alignment);
}

fn alignForwardU32(value: u32, alignment: u32) u32 {
    std.debug.assert(alignment != 0);
    return @intCast(std.mem.alignForward(usize, value, alignment));
}

fn staticDataInvariant(comptime message: []const u8) noreturn {
    if (@import("builtin").mode == .Debug) {
        std.debug.panic("static data invariant violated: {s}", .{message});
    }
    unreachable;
}

test "static data declarations are referenced" {
    std.testing.refAllDecls(@This());
}
