//! Freeze one evaluated native LIR root into readonly data and relocations.
//!
//! This is a forward consumer of the root's producer-owned const/layout plans.
//! It never restores ConstStore values or evaluates a static initializer. The
//! evaluator keeps the source value and its allocations alive until freezeRoot
//! returns; the returned exports then own every reachable byte independently.

const std = @import("std");
const builtins = @import("builtins");
const layout = @import("layout");
const lir = @import("lir");
const static_data = @import("static_data");
const Value = @import("value.zig").Value;
const Allocator = std.mem.Allocator;
const Program = lir.Program;
const SymbolId = static_data.StaticDataSymbolId;
const word_size = @sizeOf(usize);

/// The evaluator supplies identities recorded when it emitted native code.
/// Native code addresses are never recovered by searching procedure names or
/// by reconstructing capture/drop choices from a runtime pointer.
pub const CallableResolution = struct {
    proc: lir.LIR.LirProcSpecId,
    capture_ptr: [*]u8,
};

pub const CallableResolver = struct {
    context: ?*anyopaque = null,
    resolve: *const fn (?*anyopaque, [*]u8) CallableResolution = missingCallableResolver,
};

/// The first export is the requested slot; all data-symbol relocation indices
/// belong to this returned slice. Concatenating export slices must rebase those
/// indices. Procedure relocations retain identities in `program`.
/// Release the result with static_data.deinitStaticData.
pub fn freezeRoot(
    allocator: Allocator,
    program: *const Program.Result,
    slot: lir.LIR.StaticDataId,
    root: Program.ConstRootPlan,
    value: Value,
    callables: CallableResolver,
) Allocator.Error![]static_data.StaticDataExport {
    if (program.layouts.targetUsize().size() != word_size) {
        invariant("native root export requires host-width LIR");
    }
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    var builder = Builder{
        .allocator = arena.allocator(),
        .program = program,
        .slot = slot,
        .callables = callables,
    };
    const root_symbol = try builder.addNode(
        try Program.staticDataSymbolName(builder.allocator, slot),
        builder.size(root.ret_layout),
        builder.alignment(root.ret_layout),
    );
    try builder.enqueue(root.plan, root.ret_layout, value, .{ .symbol = root_symbol }, .value);
    var next: usize = 0;
    while (next < builder.jobs.items.len) : (next += 1) {
        const job = builder.jobs.items[next];
        try builder.visit(job);
    }
    return builder.finish(allocator);
}

const Destination = struct {
    symbol: SymbolId,
    offset: usize = 0,

    fn offsetBy(self: Destination, amount: usize) Destination {
        return .{ .symbol = self.symbol, .offset = self.offset + amount };
    }
};

const Job = struct {
    plan: Program.ConstPlanId,
    layout_idx: layout.Idx,
    source: Value,
    dest: Destination,
    storage: Program.CaptureSlotStorage,
};

const Node = struct {
    name: []const u8,
    bytes: []u8,
    alignment: u32,
    relocations: std.ArrayList(static_data.StaticDataRelocation) = .empty,
};

/// A typed view is explicit in the producer's plan. Reserving its destination
/// before visiting children preserves sharing and closes recursive graphs.
const AllocationKey = struct {
    address: usize,
    plan: Program.ConstPlanId,
    layout_idx: layout.Idx,
    count: usize,
    kind: enum { value, list, string, erased },
};

const Builder = struct {
    allocator: Allocator,
    program: *const Program.Result,
    slot: lir.LIR.StaticDataId,
    callables: CallableResolver,
    nodes: std.ArrayList(Node) = .empty,
    jobs: std.ArrayList(Job) = .empty,
    allocations: std.AutoHashMapUnmanaged(AllocationKey, Destination) = .empty,

    fn size(self: *const Builder, idx: layout.Idx) usize {
        return self.program.layouts.layoutSize(self.program.layouts.getLayout(idx));
    }

    fn alignment(self: *const Builder, idx: layout.Idx) u32 {
        return @intCast(self.program.layouts.getLayout(idx).alignment(self.program.layouts.targetUsize()).toByteUnits());
    }

    fn addNode(self: *Builder, name: []const u8, byte_count: usize, alignment_: u32) Allocator.Error!SymbolId {
        const node_bytes = try self.allocator.alloc(u8, byte_count);
        @memset(node_bytes, 0);
        const symbol: SymbolId = @enumFromInt(self.nodes.items.len);
        try self.nodes.append(self.allocator, .{ .name = name, .bytes = node_bytes, .alignment = alignment_ });
        return symbol;
    }

    fn node(self: *Builder, dest: Destination) *Node {
        return &self.nodes.items[@intFromEnum(dest.symbol)];
    }

    fn bytes(self: *Builder, dest: Destination, count: usize) []u8 {
        return self.node(dest).bytes[dest.offset..][0..count];
    }

    fn writeWord(self: *Builder, dest: Destination, value: usize) void {
        std.mem.writeInt(usize, self.bytes(dest, word_size)[0..word_size], value, .little);
    }

    fn enqueue(self: *Builder, plan: Program.ConstPlanId, idx: layout.Idx, source: Value, dest: Destination, storage: Program.CaptureSlotStorage) Allocator.Error!void {
        try self.jobs.append(self.allocator, .{ .plan = plan, .layout_idx = idx, .source = source, .dest = dest, .storage = storage });
    }

    fn relocate(self: *Builder, dest: Destination, target: Destination) Allocator.Error!void {
        // Zero pointer bytes explicitly; only the relocation gives them meaning.
        self.writeWord(dest, 0);
        try self.node(dest).relocations.append(self.allocator, .{
            .offset = dest.offset,
            .target_symbol_name = self.node(target).name,
            .target = .{ .data_symbol = target.symbol },
            .addend = @intCast(target.offset),
        });
    }

    fn reserveAllocation(self: *Builder, key: AllocationKey, byte_count: usize, alignment_: u32, contains_refcounted: bool, element_count: ?usize) Allocator.Error!struct { dest: Destination, fresh: bool } {
        if (self.allocations.get(key)) |existing| return .{ .dest = existing, .fresh = false };
        const header_size: usize = if (contains_refcounted) 2 * word_size else word_size;
        const payload_offset = std.mem.alignForward(usize, header_size, alignment_);
        const name = try std.fmt.allocPrint(self.allocator, "roc__ctfe_{d}_{d}", .{ @intFromEnum(self.slot), self.nodes.items.len });
        const symbol = try self.addNode(name, payload_offset + byte_count, @max(alignment_, word_size));
        const dest = Destination{ .symbol = symbol, .offset = payload_offset };
        // A zero RC header is the runtime's immutable/static allocation marker.
        if (contains_refcounted) self.writeWord(.{ .symbol = symbol, .offset = payload_offset - 2 * word_size }, element_count orelse 0);
        try self.allocations.put(self.allocator, key, dest);
        return .{ .dest = dest, .fresh = true };
    }

    fn boxed(self: *Builder, job: Job, child_plan: Program.ConstPlanId, child_layout: layout.Idx) Allocator.Error!void {
        const address = job.source.read(usize);
        if (address == 0) invariant("nonzero-sized boxed root value had a null payload");
        const child_value = Value{ .ptr = @ptrFromInt(address) };
        const result = try self.reserveAllocation(.{
            .address = address,
            .plan = child_plan,
            .layout_idx = child_layout,
            .count = 1,
            .kind = .value,
        }, self.size(child_layout), self.alignment(child_layout), self.program.layouts.layoutContainsRefcounted(self.program.layouts.getLayout(child_layout)), null);
        try self.relocate(job.dest, result.dest);
        if (result.fresh) try self.enqueue(child_plan, child_layout, child_value, result.dest, .value);
    }

    fn visit(self: *Builder, job: Job) Allocator.Error!void {
        const physical = self.program.layouts.getLayout(job.layout_idx);
        if (job.storage == .recursive_box) {
            if (physical.tag != .box) invariant("recursive capture plan lacked box storage");
            return self.boxed(job, job.plan, physical.getIdx());
        }
        const plan = self.program.const_plans.items[@intFromEnum(job.plan)];
        switch (plan) {
            .pending, .layout_only => invariant("incomplete const plan reached native root export"),
            .zst => {},
            .scalar => {
                if (physical.tag != .scalar or physical.getScalar().tag == .opaque_ptr or physical.getScalar().tag == .str) invariant("scalar export plan did not name pointer-free scalar bytes");
                @memcpy(self.bytes(job.dest, self.size(job.layout_idx)), job.source.readBytes(self.size(job.layout_idx)));
            },
            .str => try self.string(job),
            .list => |element| try self.list(job, element),
            .box => |element| switch (physical.tag) {
                .box_of_zst => self.writeWord(job.dest, 0),
                .box => try self.boxed(job, element, physical.getIdx()),
                .erased_callable => try self.enqueue(element, job.layout_idx, job.source, job.dest, .value),
                else => invariant("box export plan had incompatible layout"),
            },
            .tuple, .record => |child_plans| {
                if (physical.tag == .box) return self.boxed(job, job.plan, physical.getIdx());
                if (physical.tag == .box_of_zst) {
                    self.writeWord(job.dest, 0);
                    return;
                }
                try self.children(child_plans, job.layout_idx, job.source, job.dest, false);
            },
            .named => |named| try self.enqueue(named.backing, job.layout_idx, job.source, job.dest, .value),
            .tag_union => |variants| {
                if (physical.tag == .box) return self.boxed(job, job.plan, physical.getIdx());
                const discriminant = self.tagDiscriminant(job);
                for (variants) |variant| {
                    if (variant.discriminant != discriminant) continue;
                    try self.children(variant.payloads, self.tagPayload(job.layout_idx, discriminant), job.source, job.dest, true);
                    return;
                }
                invariant("native tag did not match an explicit const variant");
            },
            .fn_value => |set_id| {
                if (physical.tag == .box) return self.boxed(job, job.plan, physical.getIdx());
                const set = self.program.fn_sets.items[@intFromEnum(set_id)];
                const discriminant = self.tagDiscriminant(job);
                for (set.variants) |variant| {
                    if (variant.discriminant != discriminant) continue;
                    try self.captures(variant.captures, variant.payload_layout, job.source, job.dest);
                    return;
                }
                invariant("native callable did not match an explicit finite variant");
            },
            .erased_fn => |set_id| try self.erased(job, set_id),
        }
    }

    fn string(self: *Builder, job: Job) Allocator.Error!void {
        const str = job.source.read(builtins.str.RocStr);
        const slice = str.asSlice();
        if (builtins.str.RocStr.fitsInSmallStr(slice.len)) {
            var small = builtins.str.RocStr.fromSliceSmall(slice);
            @memcpy(self.bytes(job.dest, @sizeOf(builtins.str.RocStr)), std.mem.asBytes(&small));
            return;
        }
        const result = try self.reserveAllocation(.{
            .address = @intFromPtr(slice.ptr),
            .plan = job.plan,
            .layout_idx = job.layout_idx,
            .count = slice.len,
            .kind = .string,
        }, slice.len, 1, false, null);
        if (result.fresh) @memcpy(self.bytes(result.dest, slice.len), slice);
        try self.relocate(job.dest, result.dest);
        self.writeWord(job.dest.offsetBy(@offsetOf(builtins.str.RocStr, "capacity_or_alloc_ptr")), builtins.str.RocStr.encodeCapacity(slice.len));
        self.writeWord(job.dest.offsetBy(@offsetOf(builtins.str.RocStr, "length")), slice.len);
    }

    fn list(self: *Builder, job: Job, element: Program.ConstPlanId) Allocator.Error!void {
        const physical = self.program.layouts.getLayout(job.layout_idx);
        if (physical.tag != .list and physical.tag != .list_of_zst) invariant("list export plan had non-list layout");
        const list_value = job.source.read(builtins.list.RocList);
        self.writeWord(job.dest.offsetBy(word_size), list_value.len());
        self.writeWord(job.dest.offsetBy(2 * word_size), builtins.list.RocList.encodeCapacity(list_value.len()));
        if (physical.tag == .list_of_zst or list_value.len() == 0) return;
        const ptr = list_value.bytes orelse invariant("nonempty native list had a null pointer");
        const element_layout = physical.getIdx();
        const element_size = self.size(element_layout);
        const result = try self.reserveAllocation(.{
            .address = @intFromPtr(ptr),
            .plan = element,
            .layout_idx = element_layout,
            .count = list_value.len(),
            .kind = .list,
        }, element_size * list_value.len(), self.alignment(element_layout), self.program.layouts.layoutContainsRefcounted(self.program.layouts.getLayout(element_layout)), list_value.len());
        try self.relocate(job.dest, result.dest);
        if (result.fresh) {
            for (0..list_value.len()) |index| {
                try self.enqueue(element, element_layout, .{ .ptr = ptr + index * element_size }, result.dest.offsetBy(index * element_size), .value);
            }
        }
    }

    fn children(self: *Builder, plans: []const Program.ConstPlanId, idx: layout.Idx, source: Value, dest: Destination, tag_payload: bool) Allocator.Error!void {
        if (plans.len == 0 or self.size(idx) == 0) return;
        if (tag_payload and plans.len == 1) return self.enqueue(plans[0], idx, source, dest, .value);
        const physical = self.program.layouts.getLayout(idx);
        if (physical.tag != .struct_) invariant("aggregate export plan had non-struct layout");
        for (plans, 0..) |plan, original_index| {
            const child_layout = self.program.layouts.getStructFieldLayoutByOriginalIndex(physical.getStruct().idx, @intCast(original_index));
            const offset = self.program.layouts.getStructFieldOffsetByOriginalIndex(physical.getStruct().idx, @intCast(original_index));
            try self.enqueue(plan, child_layout, source.offset(offset), dest.offsetBy(offset), .value);
        }
    }

    fn tagDiscriminant(self: *Builder, job: Job) u16 {
        const physical = self.program.layouts.getLayout(job.layout_idx);
        if (physical.tag == .zst) return 0;
        if (physical.tag != .tag_union) invariant("native tag export had non-tag layout");
        const data = self.program.layouts.getTagUnionData(physical.getTagUnion().idx);
        const discriminant = data.readDiscriminant(job.source.ptr, self.program.layouts.targetUsize());
        data.writeDiscriminant(self.bytes(job.dest, self.size(job.layout_idx)).ptr, discriminant, self.program.layouts.targetUsize());
        return @intCast(discriminant);
    }

    fn tagPayload(self: *Builder, idx: layout.Idx, discriminant: u16) layout.Idx {
        const physical = self.program.layouts.getLayout(idx);
        if (physical.tag == .zst) return .zst;
        const data = self.program.layouts.getTagUnionData(physical.getTagUnion().idx);
        const variants = self.program.layouts.getTagUnionVariants(data);
        if (discriminant >= variants.len) invariant("native tag discriminant exceeded layout variants");
        return variants.get(discriminant).payload_layout;
    }

    fn captures(self: *Builder, slots: []const Program.CaptureSlot, idx: layout.Idx, source: Value, dest: Destination) Allocator.Error!void {
        if (slots.len == 0 or self.size(idx) == 0) return;
        const physical = self.program.layouts.getLayout(idx);
        if (physical.tag == .struct_) {
            for (slots) |slot| {
                const child_layout = self.program.layouts.getStructFieldLayoutByOriginalIndex(physical.getStruct().idx, slot.slot);
                const offset = self.program.layouts.getStructFieldOffsetByOriginalIndex(physical.getStruct().idx, slot.slot);
                try self.enqueue(slot.plan, child_layout, source.offset(offset), dest.offsetBy(offset), slot.storage);
            }
        } else if (slots.len == 1) {
            try self.enqueue(slots[0].plan, idx, source, dest, slots[0].storage);
        } else invariant("native callable capture plan had non-struct multiple captures");
    }

    fn erased(self: *Builder, job: Job, set_id: Program.ErasedFnsId) Allocator.Error!void {
        const address = job.source.read(usize);
        if (address == 0) invariant("native erased callable had a null payload");
        const resolved = self.callables.resolve(self.callables.context, @ptrFromInt(address));
        const set = self.program.erased_fns.items[@intFromEnum(set_id)];
        for (set.entries) |entry| {
            if (entry.entry != resolved.proc) continue;
            const result = try self.reserveAllocation(.{
                .address = address,
                .plan = job.plan,
                .layout_idx = job.layout_idx,
                .count = 1,
                .kind = .erased,
            }, builtins.erased_callable.payloadSize(self.size(entry.capture_layout)), builtins.erased_callable.payload_alignment, builtins.erased_callable.allocation_has_refcounted_children, null);
            try self.relocate(job.dest, result.dest);
            if (!result.fresh) return;
            const proc_name = try static_data.procSymbolName(self.allocator, self.program.store.getProcSpec(resolved.proc).name);
            try self.node(result.dest).relocations.append(self.allocator, .{
                .offset = result.dest.offset,
                .target_symbol_name = proc_name,
                .kind = .function_pointer,
                .callable_capture_offset = builtins.erased_callable.capture_offset,
                .procedure = resolved.proc,
            });
            const on_drop: ?layout.RcHelperKey = switch (entry.on_drop) {
                .none => null,
                .rc_helper => |helper| helper,
                .boxy_capture, .interpreter_context_drop => invariant("frozen callable lacks durable producer drop authority"),
            };
            if (on_drop) |helper| {
                try self.node(result.dest).relocations.append(self.allocator, .{
                    .offset = result.dest.offset + word_size,
                    .target_symbol_name = try static_data.atomicRcHelperSymbolName(self.allocator, helper),
                    .kind = .function_pointer,
                    .rc_helper = helper,
                });
            }
            try self.captures(entry.captures, entry.capture_layout, .{ .ptr = resolved.capture_ptr }, result.dest.offsetBy(builtins.erased_callable.capture_offset));
            return;
        }
        invariant("native erased callable did not match an explicit entry");
    }

    fn finish(self: *Builder, allocator: Allocator) Allocator.Error![]static_data.StaticDataExport {
        const exports = try allocator.alloc(static_data.StaticDataExport, self.nodes.items.len);
        var done: usize = 0;
        errdefer {
            for (exports[0..done]) |export_| {
                allocator.free(export_.symbol_name);
                allocator.free(export_.bytes);
                for (export_.relocations) |relocation| {
                    if (relocation.owns_target_symbol_name) allocator.free(relocation.target_symbol_name);
                }
                allocator.free(export_.relocations);
            }
            allocator.free(exports);
        }
        for (self.nodes.items, exports, 0..) |source, *dest, index| {
            const name = try allocator.dupe(u8, source.name);
            errdefer allocator.free(name);
            const owned_bytes = try allocator.dupe(u8, source.bytes);
            errdefer allocator.free(owned_bytes);
            const relocations = try allocator.dupe(static_data.StaticDataRelocation, source.relocations.items);
            // All names are assigned in a separate pass once every symbol exists.
            for (relocations) |*relocation| relocation.owns_target_symbol_name = false;
            dest.* = .{
                .symbol_name = name,
                .value_id = if (index == 0) self.slot else null,
                .bytes = owned_bytes,
                .alignment = source.alignment,
                .is_global = false,
                .is_exported = false,
                .relocations = relocations,
            };
            done += 1;
        }
        for (exports) |*export_| {
            const relocations: []static_data.StaticDataRelocation = @constCast(export_.relocations);
            for (relocations) |*relocation| {
                switch (relocation.target) {
                    .data_symbol => |symbol| relocation.target_symbol_name = exports[@intFromEnum(symbol)].symbol_name,
                    .named => {
                        relocation.target_symbol_name = try allocator.dupe(u8, relocation.target_symbol_name);
                        relocation.owns_target_symbol_name = true;
                    },
                }
            }
        }
        return exports;
    }
};

fn missingCallableResolver(_: ?*anyopaque, _: [*]u8) CallableResolution {
    invariant("native root exporter requires explicit erased callable identities");
}

fn invariant(comptime message: []const u8) noreturn {
    if (@import("builtin").mode == .Debug) std.debug.panic("native root export invariant violated: {s}", .{message});
    unreachable;
}

fn testRoot(plan: Program.ConstPlanId, ret_layout: layout.Idx) Program.ConstRootPlan {
    return .{
        .root_order = 0,
        .request = .{
            .order = 0,
            .module_idx = 0,
            .kind = .compile_time_constant,
            .source = undefined,
            .checked_type = undefined,
            .abi = .compile_time,
            .exposure = .private,
        },
        .proc = undefined,
        .ret_layout = ret_layout,
        .ret_type = undefined,
        .plan = plan,
    };
}

test "native root export owns list strings and preserves shared typed pointers" {
    const allocator = std.testing.allocator;
    var program = try Program.Result.init(allocator, @import("base").target.TargetUsize.native);
    defer program.deinit();
    const str_plan: Program.ConstPlanId = @enumFromInt(0);
    const list_plan: Program.ConstPlanId = @enumFromInt(1);
    try program.const_plans.append(allocator, .str);
    try program.const_plans.append(allocator, .{ .list = str_plan });
    const list_layout = try program.layouts.insertList(.str);
    const text = "a native compile-time string exceeding inline capacity";
    const backing = try allocator.dupe(u8, text);
    defer allocator.free(backing);
    const str = builtins.str.RocStr{
        .bytes = backing.ptr,
        .length = backing.len,
        .capacity_or_alloc_ptr = builtins.str.RocStr.encodeCapacity(backing.len),
    };
    var items = [_]builtins.str.RocStr{ str, str };
    var list_value = builtins.list.RocList{
        .bytes = @ptrCast(&items),
        .length = items.len,
        .capacity_or_alloc_ptr = builtins.list.RocList.encodeCapacity(items.len),
    };
    const slot: lir.LIR.StaticDataId = @enumFromInt(7);
    const exports = try freezeRoot(allocator, &program, slot, testRoot(list_plan, list_layout), .{ .ptr = @ptrCast(&list_value) }, .{});
    defer static_data.deinitStaticData(allocator, exports);
    @memset(backing, 'x');
    try std.testing.expectEqual(@as(usize, 3), exports.len);
    try std.testing.expectEqual(slot, exports[0].value_id.?);
    try std.testing.expectEqual(@as(usize, 1), exports[0].relocations.len);
    const elements_relocation = exports[0].relocations[0];
    const elements = exports[@intFromEnum(elements_relocation.target.data_symbol)];
    try std.testing.expectEqual(@as(usize, 2), elements.relocations.len);
    const count_header = elements.bytes[@intCast(elements_relocation.addend - 2 * word_size)..][0..word_size];
    try std.testing.expectEqual(@as(usize, 2), std.mem.readInt(usize, count_header[0..word_size], .little));
    try std.testing.expectEqual(elements.relocations[0].target.data_symbol, elements.relocations[1].target.data_symbol);
    const first_string_offset: usize = @intCast(elements_relocation.addend);
    const frozen_string = std.mem.bytesAsValue(builtins.str.RocStr, elements.bytes[first_string_offset..][0..@sizeOf(builtins.str.RocStr)]).*;
    try std.testing.expectEqual(text.len, frozen_string.length);
    try std.testing.expectEqual(builtins.str.RocStr.encodeCapacity(text.len), frozen_string.capacity_or_alloc_ptr);
    const string_relocation = elements.relocations[0];
    const string_bytes = exports[@intFromEnum(string_relocation.target.data_symbol)];
    try std.testing.expectEqualStrings(text, string_bytes.bytes[@intCast(string_relocation.addend)..]);
    const header = string_bytes.bytes[@intCast(string_relocation.addend - word_size)..][0..word_size];
    try std.testing.expectEqual(@as(usize, 0), std.mem.readInt(usize, header[0..word_size], .little));
}

test "native root export removes seamless-slice native pointers" {
    const allocator = std.testing.allocator;
    var program = try Program.Result.init(allocator, @import("base").target.TargetUsize.native);
    defer program.deinit();
    const plan: Program.ConstPlanId = @enumFromInt(0);
    try program.const_plans.append(allocator, .str);
    var backing = "prefix:the native slice is longer than the inline string representation:suffix".*;
    const expected = backing[7 .. backing.len - 7];
    var str = builtins.str.RocStr{
        .bytes = expected.ptr,
        .length = expected.len,
        .capacity_or_alloc_ptr = builtins.str.RocStr.encodeSliceAllocationPtr(&backing),
    };
    const exports = try freezeRoot(allocator, &program, @enumFromInt(0), testRoot(plan, .str), .{ .ptr = @ptrCast(&str) }, .{});
    defer static_data.deinitStaticData(allocator, exports);
    const root = exports[0];
    try std.testing.expectEqual(@as(usize, 1), root.relocations.len);
    try std.testing.expectEqual(@as(usize, 0), std.mem.readInt(usize, root.bytes[0..word_size], .little));
    const frozen_string = std.mem.bytesAsValue(builtins.str.RocStr, root.bytes[0..@sizeOf(builtins.str.RocStr)]).*;
    try std.testing.expectEqual(expected.len, frozen_string.length);
    try std.testing.expectEqual(builtins.str.RocStr.encodeCapacity(expected.len), frozen_string.capacity_or_alloc_ptr);
    const relocation = root.relocations[0];
    const bytes_ = exports[@intFromEnum(relocation.target.data_symbol)].bytes;
    try std.testing.expectEqualStrings(expected, bytes_[@intCast(relocation.addend)..]);
}

fn testTemplate() Program.FnTemplate {
    return .{ .fn_def = undefined, .source_fn_ty = undefined, .source_fn_key = undefined };
}

fn testCapture(plan: Program.ConstPlanId, storage: Program.CaptureSlotStorage) Program.CaptureSlot {
    return .{ .id = undefined, .slot = 0, .ty = undefined, .plan = plan, .storage = storage };
}

test "native root export closes recursive finite callable capture graphs" {
    const allocator = std.testing.allocator;
    var program = try Program.Result.init(allocator, @import("base").target.TargetUsize.native);
    defer program.deinit();
    const plan: Program.ConstPlanId = @enumFromInt(0);
    const set: Program.FnSetId = @enumFromInt(0);
    try program.const_plans.append(allocator, .{ .fn_value = set });
    // A recursive function captures itself through the producer's recursive_box
    // capture slot. The tag payload is that box, with no explicit discriminant.
    const box_layout = try program.layouts.reserveLayout(layout.Layout.box(.zst));
    const fn_layout = try program.layouts.putTagUnion(&.{box_layout});
    program.layouts.updateLayout(box_layout, layout.Layout.box(fn_layout));
    const captures = try allocator.dupe(Program.CaptureSlot, &.{testCapture(plan, .recursive_box)});
    const variants = try allocator.dupe(Program.FnVariant, &.{.{
        .id = @enumFromInt(0),
        .discriminant = 0,
        .variant_index = 0,
        .payload_layout = box_layout,
        .template = testTemplate(),
        .captures = captures,
    }});
    try program.fn_sets.append(allocator, .{ .layout = fn_layout, .variants = variants });
    var recursive_capture: usize = undefined;
    recursive_capture = @intFromPtr(&recursive_capture);
    const exports = try freezeRoot(allocator, &program, @enumFromInt(0), testRoot(plan, fn_layout), .{ .ptr = @ptrCast(&recursive_capture) }, .{});
    defer static_data.deinitStaticData(allocator, exports);
    try std.testing.expectEqual(@as(usize, 2), exports.len);
    const root_pointer = exports[0].relocations[0];
    const capture_export = exports[@intFromEnum(root_pointer.target.data_symbol)];
    try std.testing.expectEqual(@as(usize, 1), capture_export.relocations.len);
    const self_pointer = capture_export.relocations[0];
    try std.testing.expectEqual(root_pointer.target.data_symbol, self_pointer.target.data_symbol);
    try std.testing.expectEqual(root_pointer.addend, self_pointer.addend);
    try std.testing.expectEqual(@as(usize, 0), std.mem.readInt(usize, capture_export.bytes[@intCast(root_pointer.addend - word_size)..][0..word_size], .little));
}

test "native root export selects explicit finite callable tag and captured string" {
    const allocator = std.testing.allocator;
    var program = try Program.Result.init(allocator, @import("base").target.TargetUsize.native);
    defer program.deinit();
    const str_plan: Program.ConstPlanId = @enumFromInt(0);
    const fn_plan: Program.ConstPlanId = @enumFromInt(1);
    const fn_layout = try program.layouts.putTagUnion(&.{ .zst, .str });
    try program.const_plans.append(allocator, .str);
    try program.const_plans.append(allocator, .{ .fn_value = @enumFromInt(0) });
    const captures = try allocator.dupe(Program.CaptureSlot, &.{testCapture(str_plan, .value)});
    const variants = try allocator.dupe(Program.FnVariant, &.{
        .{ .id = @enumFromInt(0), .discriminant = 0, .variant_index = 0, .payload_layout = .zst, .template = testTemplate() },
        .{ .id = @enumFromInt(1), .discriminant = 1, .variant_index = 1, .payload_layout = .str, .template = testTemplate(), .captures = captures },
    });
    try program.fn_sets.append(allocator, .{ .layout = fn_layout, .variants = variants });
    const text = "the selected callable captures a large native string";
    var str = builtins.str.RocStr{ .bytes = @constCast(text.ptr), .length = text.len, .capacity_or_alloc_ptr = builtins.str.RocStr.encodeCapacity(text.len) };
    const data = program.layouts.getTagUnionData(program.layouts.getLayout(fn_layout).getTagUnion().idx);
    const native = try allocator.alloc(u8, program.layouts.layoutSize(program.layouts.getLayout(fn_layout)));
    defer allocator.free(native);
    @memset(native, 0xaa);
    @memcpy(native[0..@sizeOf(builtins.str.RocStr)], std.mem.asBytes(&str));
    data.writeDiscriminant(native.ptr, 1, program.layouts.targetUsize());
    const exports = try freezeRoot(allocator, &program, @enumFromInt(0), testRoot(fn_plan, fn_layout), .{ .ptr = native.ptr }, .{});
    defer static_data.deinitStaticData(allocator, exports);
    try std.testing.expectEqual(@as(u32, 1), data.readDiscriminant(exports[0].bytes.ptr, program.layouts.targetUsize()));
    try std.testing.expectEqual(@as(usize, 1), exports[0].relocations.len);
    const relocation = exports[0].relocations[0];
    try std.testing.expectEqualStrings(text, exports[@intFromEnum(relocation.target.data_symbol)].bytes[@intCast(relocation.addend)..]);
}

test "native root export preserves erased callable procedure and drop helper identities" {
    const allocator = std.testing.allocator;
    var program = try Program.Result.init(allocator, @import("base").target.TargetUsize.native);
    defer program.deinit();
    const proc = try program.store.addProcSpec(.{ .name = lir.Symbol.fromRaw(42), .args = .empty(), .ret_layout = .zst });
    const str_plan: Program.ConstPlanId = @enumFromInt(0);
    const fn_plan: Program.ConstPlanId = @enumFromInt(1);
    const fn_layout = try program.layouts.insertErasedCallable();
    try program.const_plans.append(allocator, .str);
    try program.const_plans.append(allocator, .{ .erased_fn = @enumFromInt(0) });
    const captures = try allocator.dupe(Program.CaptureSlot, &.{testCapture(str_plan, .value)});
    const entries = try allocator.dupe(Program.ErasedFn, &.{.{ .entry = proc, .capture_layout = .str, .template = testTemplate(), .captures = captures, .on_drop = .{ .rc_helper = .{ .op = .decref, .layout_idx = .str } } }});
    try program.erased_fns.append(allocator, .{ .layout = fn_layout, .entries = entries });
    const text = "an erased callable retains this exact native capture";
    var str = builtins.str.RocStr{ .bytes = @constCast(text.ptr), .length = text.len, .capacity_or_alloc_ptr = builtins.str.RocStr.encodeCapacity(text.len) };
    var payload: [builtins.erased_callable.capture_offset + @sizeOf(builtins.str.RocStr)]u8 align(16) = @splat(0);
    std.mem.writeInt(usize, payload[0..word_size], 0x1111, .little);
    std.mem.writeInt(usize, payload[word_size..][0..word_size], 0x2222, .little);
    @memcpy(payload[builtins.erased_callable.capture_offset..], std.mem.asBytes(&str));
    var pointer = @intFromPtr(&payload);
    const Resolver = struct {
        proc: lir.LIR.LirProcSpecId,
        payload: [*]u8,
        fn resolve(context: ?*anyopaque, data_ptr: [*]u8) CallableResolution {
            const self: *@This() = @ptrCast(@alignCast(context.?));
            std.debug.assert(data_ptr == self.payload);
            return .{ .proc = self.proc, .capture_ptr = data_ptr + builtins.erased_callable.capture_offset };
        }
    };
    var resolver = Resolver{ .proc = proc, .payload = &payload };
    const exports = try freezeRoot(allocator, &program, @enumFromInt(0), testRoot(fn_plan, fn_layout), .{ .ptr = @ptrCast(&pointer) }, .{ .context = &resolver, .resolve = Resolver.resolve });
    defer static_data.deinitStaticData(allocator, exports);
    const payload_pointer = exports[0].relocations[0];
    const payload_export = exports[@intFromEnum(payload_pointer.target.data_symbol)];
    try std.testing.expectEqual(@as(usize, 3), payload_export.relocations.len);
    try std.testing.expectEqual(proc, payload_export.relocations[0].procedure.?);
    try std.testing.expectEqual(builtins.erased_callable.capture_offset, payload_export.relocations[0].callable_capture_offset.?);
    try std.testing.expectEqual(layout.RcHelperKey{ .op = .decref, .layout_idx = .str }, payload_export.relocations[1].rc_helper.?);
    const copied_header = payload_export.bytes[@intCast(payload_pointer.addend)..][0 .. 2 * word_size];
    try std.testing.expectEqualSlices(u8, &(@as([2 * word_size]u8, @splat(0))), copied_header);
    const capture_pointer = payload_export.relocations[2];
    try std.testing.expectEqualStrings(text, exports[@intFromEnum(capture_pointer.target.data_symbol)].bytes[@intCast(capture_pointer.addend)..]);
}

test "native root export follows only selected tag payload and clears inactive bytes" {
    const allocator = std.testing.allocator;
    var program = try Program.Result.init(allocator, @import("base").target.TargetUsize.native);
    defer program.deinit();
    const str_plan: Program.ConstPlanId = @enumFromInt(0);
    const tag_plan: Program.ConstPlanId = @enumFromInt(1);
    const tag_layout = try program.layouts.putTagUnion(&.{ .zst, .str });
    try program.const_plans.append(allocator, .str);
    const variants = try allocator.alloc(Program.ConstTagVariant, 2);
    variants[0] = .{ .name = try allocator.dupe(u8, "Absent"), .checked_name = undefined, .discriminant = 0, .payloads = try allocator.alloc(Program.ConstPlanId, 0) };
    variants[1] = .{ .name = try allocator.dupe(u8, "Present"), .checked_name = undefined, .discriminant = 1, .payloads = try allocator.dupe(Program.ConstPlanId, &.{str_plan}) };
    try program.const_plans.append(allocator, .{ .tag_union = variants });
    const data = program.layouts.getTagUnionData(program.layouts.getLayout(tag_layout).getTagUnion().idx);
    const native = try allocator.alloc(u8, program.layouts.layoutSize(program.layouts.getLayout(tag_layout)));
    defer allocator.free(native);
    for ([_]u32{ 0, 1 }) |discriminant| {
        @memset(native, 0xaa);
        const text = "a selected tag payload larger than an inline string";
        var str = builtins.str.RocStr{ .bytes = @constCast(text.ptr), .length = text.len, .capacity_or_alloc_ptr = builtins.str.RocStr.encodeCapacity(text.len) };
        if (discriminant == 1) @memcpy(native[0..@sizeOf(builtins.str.RocStr)], std.mem.asBytes(&str));
        data.writeDiscriminant(native.ptr, discriminant, program.layouts.targetUsize());
        const exports = try freezeRoot(allocator, &program, @enumFromInt(0), testRoot(tag_plan, tag_layout), .{ .ptr = native.ptr }, .{});
        defer static_data.deinitStaticData(allocator, exports);
        try std.testing.expectEqual(discriminant, data.readDiscriminant(exports[0].bytes.ptr, program.layouts.targetUsize()));
        if (discriminant == 0) {
            try std.testing.expectEqual(@as(usize, 1), exports.len);
            try std.testing.expectEqual(@as(usize, 0), exports[0].relocations.len);
            for (exports[0].bytes) |byte| try std.testing.expectEqual(@as(u8, 0), byte);
        } else {
            try std.testing.expectEqual(@as(usize, 2), exports.len);
            const pointer = exports[0].relocations[0];
            try std.testing.expectEqualStrings(text, exports[@intFromEnum(pointer.target.data_symbol)].bytes[@intCast(pointer.addend)..]);
        }
    }
}
