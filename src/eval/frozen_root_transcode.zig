//! Re-encode immutable root graphs using paired producer-owned plans.
//! Symbol relocations are the only pointer authority; source bytes are never
//! interpreted as addresses and no initializer is evaluated.
const std = @import("std");
const builtins = @import("builtins");
const layout = @import("layout");
const lir = @import("lir");
const static_data = @import("static_data");
const Allocator = std.mem.Allocator;
const Program = lir.Program;
const SymbolId = static_data.StaticDataSymbolId;

const PlanView = struct { plan: Program.ConstPlanId, layout_idx: layout.Idx };

pub fn transcodeRoot(allocator: Allocator, source_program: *const Program.Result, source_root: Program.ConstRootPlan, source_exports: []const static_data.StaticDataExport, source_symbol: SymbolId, target_program: *const Program.Result, target_root: Program.ConstRootPlan, target_slot: lir.LIR.StaticDataId) Allocator.Error![]static_data.StaticDataExport {
    return transcodePlans(allocator, source_program, .{ .plan = source_root.plan, .layout_idx = source_root.ret_layout }, source_exports, source_symbol, target_program, .{ .plan = target_root.plan, .layout_idx = target_root.ret_layout }, target_slot);
}

pub fn transcodeValueSlot(allocator: Allocator, source_program: *const Program.Result, source_slot: Program.StaticDataValue, source_exports: []const static_data.StaticDataExport, source_symbol: SymbolId, target_program: *const Program.Result, target_slot: lir.LIR.StaticDataId) Allocator.Error![]static_data.StaticDataExport {
    const target = target_program.static_data_values.items[@intFromEnum(target_slot)];
    const source_identity = source_slot.compile_time_root orelse invariant("source slot lacks checked root authority");
    const target_identity = target.compile_time_root orelse invariant("target slot lacks checked root authority");
    if (!std.meta.eql(source_identity.module, target_identity.module) or source_identity.root != target_identity.root or std.meta.activeTag(source_identity.role) != std.meta.activeTag(target_identity.role)) invariant("paired slots do not name the same checked root role");
    return transcodePlans(allocator, source_program, .{ .plan = source_slot.compile_time_root.?.role.value.plan, .layout_idx = source_slot.layout_idx }, source_exports, source_symbol, target_program, .{ .plan = target.compile_time_root.?.role.value.plan, .layout_idx = target.layout_idx }, target_slot);
}

fn transcodePlans(allocator: Allocator, source_program: *const Program.Result, source_root: PlanView, source_exports: []const static_data.StaticDataExport, source_symbol: SymbolId, target_program: *const Program.Result, target_root: PlanView, target_slot: lir.LIR.StaticDataId) Allocator.Error![]static_data.StaticDataExport {
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    var builder = Builder{ .allocator = arena.allocator(), .source_program = source_program, .program = target_program, .source_exports = source_exports, .slot = target_slot };
    const symbol = try builder.addNode(try Program.staticDataSymbolName(builder.allocator, target_slot), builder.size(target_root.layout_idx), builder.alignment(target_root.layout_idx));
    try builder.enqueue(source_root.plan, source_root.layout_idx, target_root.plan, target_root.layout_idx, .{ .symbol = source_symbol, .offset = source_exports[@intFromEnum(source_symbol)].symbol_offset }, .{ .symbol = symbol }, .value, .value);
    var index: usize = 0;
    while (index < builder.jobs.items.len) : (index += 1) try builder.visit(builder.jobs.items[index]);
    return builder.finish(allocator);
}

pub fn transcodeFailure(allocator: Allocator, source_program: *const Program.Result, source_slot: Program.StaticDataValue, source_exports: []const static_data.StaticDataExport, source_symbol: SymbolId, target_program: *const Program.Result, target_slot: lir.LIR.StaticDataId) Allocator.Error![]static_data.StaticDataExport {
    const target = target_program.static_data_values.items[@intFromEnum(target_slot)];
    const source_identity = source_slot.compile_time_root orelse invariant("source slot lacks checked root authority");
    const target_identity = target.compile_time_root orelse invariant("target slot lacks checked root authority");
    if (!std.meta.eql(source_identity.module, target_identity.module) or source_identity.root != target_identity.root or std.meta.activeTag(source_identity.role) != std.meta.activeTag(target_identity.role)) invariant("paired slots do not name the same checked root role");
    const source_fields = source_slot.compile_time_root.?.role.failure_message;
    const target_fields = target.compile_time_root.?.role.failure_message;
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    var builder = Builder{ .allocator = arena.allocator(), .source_program = source_program, .program = target_program, .source_exports = source_exports, .slot = target_slot };
    const symbol = try builder.addNode(try Program.staticDataSymbolName(builder.allocator, target_slot), builder.size(target.layout_idx), builder.alignment(target.layout_idx));
    builder.bytes(.{ .symbol = symbol, .offset = target_fields.failed_offset }, 1)[0] = builder.sourceBytes(.{ .symbol = source_symbol, .offset = source_exports[@intFromEnum(source_symbol)].symbol_offset + source_fields.failed_offset }, 1)[0];
    try builder.stringValue(.{ .symbol = source_symbol, .offset = source_exports[@intFromEnum(source_symbol)].symbol_offset + source_fields.message_offset }, .{ .symbol = symbol, .offset = target_fields.message_offset });
    return builder.finish(allocator);
}

const Destination = struct {
    symbol: SymbolId,
    offset: usize = 0,
    fn offsetBy(self: Destination, amount: usize) Destination {
        return .{ .symbol = self.symbol, .offset = self.offset + amount };
    }
};
const Job = struct { source_plan: Program.ConstPlanId, source_layout: layout.Idx, plan: Program.ConstPlanId, layout_idx: layout.Idx, source: Destination, dest: Destination, source_storage: Program.CaptureSlotStorage, storage: Program.CaptureSlotStorage };
const Node = struct { name: []const u8, bytes: []u8, alignment: u32, relocations: std.ArrayList(static_data.StaticDataRelocation) = .empty };
const AllocationKey = struct { source: Destination, plan: ?Program.ConstPlanId, layout_idx: layout.Idx, count: usize, kind: enum { value, list, string, erased } };
const Builder = struct {
    allocator: Allocator,
    source_program: *const Program.Result,
    program: *const Program.Result,
    source_exports: []const static_data.StaticDataExport,
    slot: lir.LIR.StaticDataId,
    nodes: std.ArrayList(Node) = .empty,
    jobs: std.ArrayList(Job) = .empty,
    allocations: std.AutoHashMapUnmanaged(AllocationKey, Destination) = .empty,
    fn word(self: *const Builder) usize {
        return self.program.layouts.targetUsize().size();
    }
    fn sourceWord(self: *const Builder) usize {
        return self.source_program.layouts.targetUsize().size();
    }
    fn size(self: *const Builder, idx: layout.Idx) usize {
        return self.program.layouts.layoutSize(self.program.layouts.getLayout(idx));
    }
    fn sourceSize(self: *const Builder, idx: layout.Idx) usize {
        return self.source_program.layouts.layoutSize(self.source_program.layouts.getLayout(idx));
    }
    fn alignment(self: *const Builder, idx: layout.Idx) u32 {
        return @intCast(self.program.layouts.getLayout(idx).alignment(self.program.layouts.targetUsize()).toByteUnits());
    }
    fn addNode(self: *Builder, name: []const u8, byte_count: usize, alignment_: u32) Allocator.Error!SymbolId {
        const data = try self.allocator.alloc(u8, byte_count);
        @memset(data, 0);
        const symbol: SymbolId = @enumFromInt(self.nodes.items.len);
        try self.nodes.append(self.allocator, .{ .name = name, .bytes = data, .alignment = alignment_ });
        return symbol;
    }
    fn node(self: *Builder, dest: Destination) *Node {
        return &self.nodes.items[@intFromEnum(dest.symbol)];
    }
    fn bytes(self: *Builder, dest: Destination, count: usize) []u8 {
        return self.node(dest).bytes[dest.offset..][0..count];
    }
    fn sourceBytes(self: *Builder, src: Destination, count: usize) []const u8 {
        return self.source_exports[@intFromEnum(src.symbol)].bytes[src.offset..][0..count];
    }
    fn writeWord(self: *Builder, dest: Destination, value: u64) void {
        switch (self.word()) {
            4 => std.mem.writeInt(u32, self.bytes(dest, 4)[0..4], @intCast(value), .little),
            8 => std.mem.writeInt(u64, self.bytes(dest, 8)[0..8], value, .little),
            else => invariant("unsupported target pointer width"),
        }
    }
    fn readWord(self: *Builder, src: Destination) usize {
        return switch (self.sourceWord()) {
            4 => std.mem.readInt(u32, self.sourceBytes(src, 4)[0..4], .little),
            8 => @intCast(std.mem.readInt(u64, self.sourceBytes(src, 8)[0..8], .little)),
            else => invariant("unsupported source pointer width"),
        };
    }
    fn sourceRelocation(self: *Builder, src: Destination) ?static_data.StaticDataRelocation {
        for (self.source_exports[@intFromEnum(src.symbol)].relocations) |rel| if (rel.offset == src.offset) return rel;
        return null;
    }
    fn pointer(self: *Builder, src: Destination) Destination {
        const rel = self.sourceRelocation(src) orelse invariant("frozen pointer lacked a relocation");
        return switch (rel.target) {
            .data_symbol => |symbol| .{ .symbol = symbol, .offset = @intCast(@as(i64, self.source_exports[@intFromEnum(symbol)].symbol_offset) + rel.addend) },
            .named => invariant("frozen data pointer lacked symbolic graph identity"),
        };
    }
    fn enqueue(self: *Builder, sp: Program.ConstPlanId, sl: layout.Idx, tp: Program.ConstPlanId, tl: layout.Idx, src: Destination, dest: Destination, ss: Program.CaptureSlotStorage, ts: Program.CaptureSlotStorage) Allocator.Error!void {
        try self.jobs.append(self.allocator, .{ .source_plan = sp, .source_layout = sl, .plan = tp, .layout_idx = tl, .source = src, .dest = dest, .source_storage = ss, .storage = ts });
    }
    fn relocate(self: *Builder, dest: Destination, target: Destination) Allocator.Error!void {
        self.writeWord(dest, 0);
        try self.node(dest).relocations.append(self.allocator, .{ .offset = dest.offset, .target_symbol_name = self.node(target).name, .target = .{ .data_symbol = target.symbol }, .addend = @intCast(target.offset) });
    }
    fn reserveAllocation(self: *Builder, key: AllocationKey, byte_count: usize, alignment_: u32, rc: bool, count: ?usize) Allocator.Error!struct { dest: Destination, fresh: bool } {
        if (self.allocations.get(key)) |dest| return .{ .dest = dest, .fresh = false };
        const offset = std.mem.alignForward(usize, (if (rc) @as(usize, 2) else 1) * self.word(), alignment_);
        const name = try std.fmt.allocPrint(self.allocator, "roc__ctfe_{d}_{d}", .{ @intFromEnum(self.slot), self.nodes.items.len });
        const symbol = try self.addNode(name, offset + byte_count, @intCast(@max(alignment_, self.word())));
        const dest = Destination{ .symbol = symbol, .offset = offset };
        if (rc) self.writeWord(.{ .symbol = symbol, .offset = offset - 2 * self.word() }, count orelse 0);
        try self.allocations.put(self.allocator, key, dest);
        return .{ .dest = dest, .fresh = true };
    }
    fn boxed(self: *Builder, job: Job, sp: Program.ConstPlanId, sl: layout.Idx, tp: Program.ConstPlanId, tl: layout.Idx) Allocator.Error!void {
        const src = self.pointer(job.source);
        const allocation = try self.reserveAllocation(.{ .source = src, .plan = tp, .layout_idx = tl, .count = 1, .kind = .value }, self.size(tl), self.alignment(tl), self.program.layouts.layoutContainsRefcounted(self.program.layouts.getLayout(tl)), null);
        try self.relocate(job.dest, allocation.dest);
        if (allocation.fresh) try self.enqueue(sp, sl, tp, tl, src, allocation.dest, .value, .value);
    }
    fn visit(self: *Builder, job: Job) Allocator.Error!void {
        const source_physical = self.source_program.layouts.getLayout(job.source_layout);
        const physical = self.program.layouts.getLayout(job.layout_idx);
        if (job.source_storage != job.storage) invariant("paired capture storage differs");
        if (job.storage == .recursive_box) return self.boxed(job, job.source_plan, source_physical.getIdx(), job.plan, physical.getIdx());
        const source_plan = self.source_program.const_plans.items[@intFromEnum(job.source_plan)];
        const plan = self.program.const_plans.items[@intFromEnum(job.plan)];
        if (std.meta.activeTag(source_plan) != std.meta.activeTag(plan)) invariant("paired canonical const plan shapes differ");
        switch (plan) {
            .pending, .layout_only => invariant("incomplete paired const plan"),
            .zst => {},
            .scalar => {
                // CheckedPrimitive has only fixed-width numeric/vector types.
                // Target pointer width never changes a language scalar.
                if (source_physical.tag != .scalar or physical.tag != .scalar or !std.meta.eql(source_physical.getScalar(), physical.getScalar())) invariant("paired fixed-width scalar representations differ");
                if (self.size(job.layout_idx) != self.sourceSize(job.source_layout)) invariant("paired fixed-width scalar sizes differ");
                @memcpy(self.bytes(job.dest, self.size(job.layout_idx)), self.sourceBytes(job.source, self.sourceSize(job.source_layout)));
            },
            .str => try self.string(job),
            .list => |element| try self.list(job, source_plan.list, element),
            .named => |named| try self.enqueue(source_plan.named.backing, job.source_layout, named.backing, job.layout_idx, job.source, job.dest, .value, .value),
            .box => |element| switch (physical.tag) {
                .box_of_zst => self.writeWord(job.dest, 0),
                .box => try self.boxed(job, source_plan.box, source_physical.getIdx(), element, physical.getIdx()),
                .erased_callable => try self.enqueue(source_plan.box, job.source_layout, element, job.layout_idx, job.source, job.dest, .value, .value),
                else => invariant("invalid box layout"),
            },
            .tuple, .record => |children_| {
                if (physical.tag == .box) return self.boxed(job, job.source_plan, source_physical.getIdx(), job.plan, physical.getIdx());
                if (physical.tag == .box_of_zst) return;
                const source_children = if (source_plan == .tuple) source_plan.tuple else source_plan.record;
                try self.children(source_children, job.source_layout, children_, job.layout_idx, job.source, job.dest, false);
            },
            .tag_union => |variants| {
                if (physical.tag == .box) return self.boxed(job, job.source_plan, source_physical.getIdx(), job.plan, physical.getIdx());
                const disc = self.discriminant(job.source_layout, job.source);
                const source_variant = for (source_plan.tag_union) |variant| {
                    if (variant.discriminant == disc) break variant;
                } else invariant("source tag absent from plan");
                for (variants) |variant| {
                    if (variant.discriminant != disc) continue;
                    self.writeDiscriminant(job.layout_idx, job.dest, disc);
                    try self.children(source_variant.payloads, self.payload(self.source_program, job.source_layout, disc), variant.payloads, self.payload(self.program, job.layout_idx, disc), job.source, job.dest, true);
                    return;
                }
                invariant("target tag absent from plan");
            },
            .fn_value => |set_id| {
                if (physical.tag == .box) return self.boxed(job, job.source_plan, source_physical.getIdx(), job.plan, physical.getIdx());
                const disc = self.discriminant(job.source_layout, job.source);
                const source_set = self.source_program.fn_sets.items[@intFromEnum(source_plan.fn_value)];
                const source_variant = for (source_set.variants) |variant| {
                    if (variant.discriminant == disc) break variant;
                } else invariant("source callable variant absent");
                const target_set = self.program.fn_sets.items[@intFromEnum(set_id)];
                requireUniqueFrozenFunction(Program.FnVariant, source_variant.template, target_set.variants);
                for (target_set.variants) |variant| {
                    if (!sameFrozenFunction(source_variant.template, variant.template)) continue;
                    self.writeDiscriminant(job.layout_idx, job.dest, variant.discriminant);
                    try self.captures(source_variant.captures, source_variant.payload_layout, variant.captures, variant.payload_layout, job.source, job.dest);
                    return;
                }
                invariant("producer callable correspondence absent");
            },
            .erased_fn => |set_id| try self.erased(job, source_plan.erased_fn, set_id),
        }
    }
    fn string(self: *Builder, job: Job) Allocator.Error!void {
        return self.stringValue(job.source, job.dest);
    }
    fn stringValue(self: *Builder, source: Destination, dest: Destination) Allocator.Error!void {
        const raw = self.sourceBytes(source, 3 * self.sourceWord());
        const small = raw[raw.len - 1] & builtins.str.RocStr.small_str_flag != 0;
        const count = if (small) raw[raw.len - 1] & ~builtins.str.RocStr.small_str_flag else self.readWord(source.offsetBy(2 * self.sourceWord()));
        const src = if (small) source else self.pointer(source);
        const text = self.sourceBytes(src, count);
        if (count < 3 * self.word()) {
            @memcpy(self.bytes(dest, count), text);
            self.bytes(dest, 3 * self.word())[3 * self.word() - 1] = @as(u8, @intCast(count)) | builtins.str.RocStr.small_str_flag;
            return;
        }
        const result = try self.reserveAllocation(.{ .source = src, .plan = null, .layout_idx = .str, .count = count, .kind = .string }, count, 1, false, null);
        if (result.fresh) @memcpy(self.bytes(result.dest, count), text);
        try self.relocate(dest, result.dest);
        self.writeWord(dest.offsetBy(self.word()), builtins.str.RocStr.encodeCapacityForWidth(count));
        self.writeWord(dest.offsetBy(2 * self.word()), count);
    }
    fn list(self: *Builder, job: Job, sp: Program.ConstPlanId, tp: Program.ConstPlanId) Allocator.Error!void {
        const count = self.readWord(job.source.offsetBy(self.sourceWord()));
        self.writeWord(job.dest.offsetBy(self.word()), count);
        self.writeWord(job.dest.offsetBy(2 * self.word()), builtins.list.RocList.encodeCapacityForWidth(count));
        const physical = self.program.layouts.getLayout(job.layout_idx);
        if (count == 0 or physical.tag == .list_of_zst) return;
        const sl = self.source_program.layouts.getLayout(job.source_layout).getIdx();
        const tl = physical.getIdx();
        const src = self.pointer(job.source);
        const result = try self.reserveAllocation(.{ .source = src, .plan = tp, .layout_idx = tl, .count = count, .kind = .list }, count * self.size(tl), self.alignment(tl), self.program.layouts.layoutContainsRefcounted(self.program.layouts.getLayout(tl)), count);
        try self.relocate(job.dest, result.dest);
        if (result.fresh) for (0..count) |i| try self.enqueue(sp, sl, tp, tl, src.offsetBy(i * self.sourceSize(sl)), result.dest.offsetBy(i * self.size(tl)), .value, .value);
    }
    fn children(self: *Builder, sp: []const Program.ConstPlanId, sl: layout.Idx, tp: []const Program.ConstPlanId, tl: layout.Idx, src: Destination, dest: Destination, tag_payload: bool) Allocator.Error!void {
        if (sp.len != tp.len) invariant("paired aggregate field count differs");
        if (tp.len == 0 or self.size(tl) == 0) return;
        if (tag_payload and tp.len == 1) return self.enqueue(sp[0], sl, tp[0], tl, src, dest, .value, .value);
        const ss = self.source_program.layouts.getLayout(sl).getStruct().idx;
        const ts = self.program.layouts.getLayout(tl).getStruct().idx;
        for (sp, tp, 0..) |s, t, i| {
            const index: u32 = @intCast(i);
            try self.enqueue(s, self.source_program.layouts.getStructFieldLayoutByOriginalIndex(ss, index), t, self.program.layouts.getStructFieldLayoutByOriginalIndex(ts, index), src.offsetBy(self.source_program.layouts.getStructFieldOffsetByOriginalIndex(ss, index)), dest.offsetBy(self.program.layouts.getStructFieldOffsetByOriginalIndex(ts, index)), .value, .value);
        }
    }
    fn discriminant(self: *Builder, idx: layout.Idx, src: Destination) u16 {
        const physical = self.source_program.layouts.getLayout(idx);
        if (physical.tag == .zst) return 0;
        const data = self.source_program.layouts.getTagUnionData(physical.getTagUnion().idx);
        return @intCast(data.readDiscriminant(self.sourceBytes(src, self.sourceSize(idx)).ptr, self.source_program.layouts.targetUsize()));
    }
    fn writeDiscriminant(self: *Builder, idx: layout.Idx, dest: Destination, disc: u16) void {
        const physical = self.program.layouts.getLayout(idx);
        if (physical.tag == .zst) return;
        const data = self.program.layouts.getTagUnionData(physical.getTagUnion().idx);
        data.writeDiscriminant(self.bytes(dest, self.size(idx)).ptr, disc, self.program.layouts.targetUsize());
    }
    fn payload(_: *Builder, program: *const Program.Result, idx: layout.Idx, disc: u16) layout.Idx {
        const physical = program.layouts.getLayout(idx);
        if (physical.tag == .zst) return .zst;
        const data = program.layouts.getTagUnionData(physical.getTagUnion().idx);
        return program.layouts.getTagUnionVariants(data).get(disc).payload_layout;
    }
    fn captureLocation(program: *const Program.Result, idx: layout.Idx, slot: Program.CaptureSlot, location: Destination, count: usize) struct { idx: layout.Idx, location: Destination } {
        const physical = program.layouts.getLayout(idx);
        if (physical.tag == .struct_) return .{ .idx = program.layouts.getStructFieldLayoutByOriginalIndex(physical.getStruct().idx, slot.slot), .location = location.offsetBy(program.layouts.getStructFieldOffsetByOriginalIndex(physical.getStruct().idx, slot.slot)) };
        if (count != 1) invariant("multiple captures lack struct layout");
        return .{ .idx = idx, .location = location };
    }
    fn captures(self: *Builder, sp: []const Program.CaptureSlot, sl: layout.Idx, tp: []const Program.CaptureSlot, tl: layout.Idx, src: Destination, dest: Destination) Allocator.Error!void {
        if (self.size(tl) == 0) return;
        for (tp) |t| {
            const s = for (sp) |candidate| {
                if (std.meta.eql(candidate.id, t.id)) break candidate;
            } else invariant("paired callable capture identity absent");
            const source = captureLocation(self.source_program, sl, s, src, sp.len);
            const target = captureLocation(self.program, tl, t, dest, tp.len);
            try self.enqueue(s.plan, source.idx, t.plan, target.idx, source.location, target.location, s.storage, t.storage);
        }
    }
    fn erased(self: *Builder, job: Job, source_set: Program.ErasedFnsId, target_set: Program.ErasedFnsId) Allocator.Error!void {
        const src = self.pointer(job.source);
        const code = self.sourceRelocation(src) orelse invariant("erased code lacked relocation");
        const source_proc = code.procedure orelse invariant("erased code lacked procedure identity");
        const source_entry = for (self.source_program.erased_fns.items[@intFromEnum(source_set)].entries) |entry| {
            if (entry.entry == source_proc) break entry;
        } else invariant("erased procedure absent from source plan");
        requireUniqueFrozenFunction(Program.ErasedFn, source_entry.template, self.program.erased_fns.items[@intFromEnum(target_set)].entries);
        for (self.program.erased_fns.items[@intFromEnum(target_set)].entries) |entry| {
            if (!sameFrozenFunction(source_entry.template, entry.template)) continue;
            const capture_offset = std.mem.alignForward(usize, 2 * self.word(), builtins.erased_callable.payload_alignment);
            const result = try self.reserveAllocation(.{ .source = src, .plan = job.plan, .layout_idx = job.layout_idx, .count = 1, .kind = .erased }, capture_offset + self.size(entry.capture_layout), builtins.erased_callable.payload_alignment, builtins.erased_callable.allocation_has_refcounted_children, null);
            try self.relocate(job.dest, result.dest);
            if (!result.fresh) return;
            try self.node(result.dest).relocations.append(self.allocator, .{ .offset = result.dest.offset, .target_symbol_name = try static_data.procSymbolName(self.allocator, self.program.store.getProcSpec(entry.entry).name), .kind = .function_pointer, .callable_capture_offset = @intCast(capture_offset), .procedure = entry.entry });
            switch (entry.on_drop) {
                .none => {},
                .rc_helper => |helper| try self.node(result.dest).relocations.append(self.allocator, .{ .offset = result.dest.offset + self.word(), .target_symbol_name = try static_data.atomicRcHelperSymbolName(self.allocator, helper), .kind = .function_pointer, .rc_helper = helper }),
                .boxy_capture, .interpreter_context_drop => invariant("frozen callable target lacks durable drop authority"),
            }

            try self.captures(source_entry.captures, source_entry.capture_layout, entry.captures, entry.capture_layout, src.offsetBy(code.callable_capture_offset orelse invariant("erased source lacked capture offset")), result.dest.offsetBy(capture_offset));
            return;
        }
        invariant("target erased callable correspondence absent");
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
fn invariant(comptime message: []const u8) noreturn {
    if (@import("builtin").mode == .Debug) std.debug.panic("frozen root transcode invariant violated: {s}", .{message});
    unreachable;
}

fn sameFrozenFunction(source: Program.FnTemplate, target: Program.FnTemplate) bool {
    return (source.frozen_fn orelse invariant("source callable lacks frozen owner identity")) == (target.frozen_fn orelse invariant("target callable lacks frozen owner identity")) and std.meta.eql(source.frozen_worker, target.frozen_worker);
}

fn testRoot(plan: Program.ConstPlanId, idx: layout.Idx) Program.ConstRootPlan {
    return .{ .root_order = 0, .request = .{ .order = 0, .module_idx = 0, .kind = .compile_time_constant, .source = undefined, .checked_type = undefined, .abi = .compile_time, .exposure = .private }, .proc = undefined, .ret_layout = idx, .ret_type = undefined, .plan = plan };
}

test "frozen root transcode preserves shared list strings across pointer widths" {
    const allocator = std.testing.allocator;
    const TargetUsize = @import("base").target.TargetUsize;
    var source = try Program.Result.init(allocator, TargetUsize.native);
    defer source.deinit();
    var target = try Program.Result.init(allocator, .u32);
    defer target.deinit();
    const str_plan: Program.ConstPlanId = @enumFromInt(0);
    const list_plan: Program.ConstPlanId = @enumFromInt(1);
    try source.const_plans.append(allocator, .str);
    try source.const_plans.append(allocator, .{ .list = str_plan });
    try target.const_plans.append(allocator, .str);
    try target.const_plans.append(allocator, .{ .list = str_plan });
    const source_list = try source.layouts.insertList(.str);
    const target_list = try target.layouts.insertList(.str);
    const text = "shared source backing wider than either small string";
    const string = builtins.str.RocStr{ .bytes = @constCast(text.ptr), .length = text.len, .capacity_or_alloc_ptr = builtins.str.RocStr.encodeCapacity(text.len) };
    var items = [_]builtins.str.RocStr{ string, string };
    var list_value = builtins.list.RocList{ .bytes = @ptrCast(&items), .length = 2, .capacity_or_alloc_ptr = builtins.list.RocList.encodeCapacity(2) };
    const native = try @import("native_root_export.zig").freezeRoot(allocator, &source, @enumFromInt(0), testRoot(list_plan, source_list), .{ .ptr = @ptrCast(&list_value) }, .{});
    defer static_data.deinitStaticData(allocator, native);
    // The linker symbol may point inside its owned byte image. Relocation
    // addends are relative to that symbol, not the image's first byte.
    native[2].symbol_offset = 4;
    for (@constCast(native[1].relocations)) |*rel| rel.addend -= 4;
    const converted = try transcodeRoot(allocator, &source, testRoot(list_plan, source_list), native, @enumFromInt(0), &target, testRoot(list_plan, target_list), @enumFromInt(4));
    defer static_data.deinitStaticData(allocator, converted);
    try std.testing.expectEqual(@as(usize, 3), converted.len);
    try std.testing.expectEqual(@as(usize, 12), converted[0].bytes.len);
    try std.testing.expectEqual(@as(u32, 2), std.mem.readInt(u32, converted[0].bytes[4..8], .little));
    const list_rel = converted[0].relocations[0];
    const list_export = converted[@intFromEnum(list_rel.target.data_symbol)];
    const list_offset: usize = @intCast(list_rel.addend);
    try std.testing.expectEqual(@as(u32, 2), std.mem.readInt(u32, list_export.bytes[list_offset - 8 ..][0..4], .little));
    try std.testing.expectEqual(@as(u32, 0), std.mem.readInt(u32, list_export.bytes[list_offset - 4 ..][0..4], .little));
    try std.testing.expectEqual(@as(u32, text.len), std.mem.readInt(u32, list_export.bytes[list_offset + 8 ..][0..4], .little));
    try std.testing.expectEqual(list_export.relocations[0].target.data_symbol, list_export.relocations[1].target.data_symbol);
    const str_rel = list_export.relocations[0];
    try std.testing.expectEqualStrings(text, converted[@intFromEnum(str_rel.target.data_symbol)].bytes[@intCast(str_rel.addend)..]);
}

test "frozen root transcode promotes inline strings when target width shrinks" {
    const allocator = std.testing.allocator;
    var source = try Program.Result.init(allocator, @import("base").target.TargetUsize.native);
    defer source.deinit();
    var target = try Program.Result.init(allocator, .u32);
    defer target.deinit();
    const plan: Program.ConstPlanId = @enumFromInt(0);
    try source.const_plans.append(allocator, .str);
    try target.const_plans.append(allocator, .str);
    const text = "sixteen-byte-str";
    var value = builtins.str.RocStr.fromSliceSmall(text);
    const native = try @import("native_root_export.zig").freezeRoot(allocator, &source, @enumFromInt(0), testRoot(plan, .str), .{ .ptr = @ptrCast(&value) }, .{});
    defer static_data.deinitStaticData(allocator, native);
    const converted = try transcodeRoot(allocator, &source, testRoot(plan, .str), native, @enumFromInt(0), &target, testRoot(plan, .str), @enumFromInt(0));
    defer static_data.deinitStaticData(allocator, converted);
    try std.testing.expectEqual(@as(usize, 2), converted.len);
    try std.testing.expectEqual(@as(u32, text.len), std.mem.readInt(u32, converted[0].bytes[8..12], .little));
    const rel = converted[0].relocations[0];
    try std.testing.expectEqualStrings(text, converted[@intFromEnum(rel.target.data_symbol)].bytes[@intCast(rel.addend)..]);
}

fn requireUniqueFrozenFunction(comptime Entry: type, source: Program.FnTemplate, targets: []const Entry) void {
    var found = false;
    for (targets) |entry| {
        if (!sameFrozenFunction(source, entry.template)) continue;
        if (found) invariant("frozen callable owner has ambiguous target representation");
        found = true;
    }
    if (!found) invariant("frozen callable owner has no target representation");
}

test "frozen root transcode preserves fixed U64 values on 32-bit targets" {
    const allocator = std.testing.allocator;
    var source = try Program.Result.init(allocator, @import("base").target.TargetUsize.native);
    defer source.deinit();
    var target = try Program.Result.init(allocator, .u32);
    defer target.deinit();
    const plan: Program.ConstPlanId = @enumFromInt(0);
    try source.const_plans.append(allocator, .scalar);
    try target.const_plans.append(allocator, .scalar);
    var value: u64 = 0x123456789abcdef0;
    const native = try @import("native_root_export.zig").freezeRoot(allocator, &source, @enumFromInt(0), testRoot(plan, .u64), .{ .ptr = @ptrCast(&value) }, .{});
    defer static_data.deinitStaticData(allocator, native);
    const converted = try transcodeRoot(allocator, &source, testRoot(plan, .u64), native, @enumFromInt(0), &target, testRoot(plan, .u64), @enumFromInt(0));
    defer static_data.deinitStaticData(allocator, converted);
    try std.testing.expectEqual(value, std.mem.readInt(u64, converted[0].bytes[0..8], .little));
}

fn recursiveCallable(allocator: Allocator, program: *Program.Result) Allocator.Error!layout.Idx {
    const plan: Program.ConstPlanId = @enumFromInt(0);
    try program.const_plans.append(allocator, .{ .fn_value = @enumFromInt(0) });
    const box_layout = try program.layouts.reserveLayout(layout.Layout.box(.zst));
    const fn_layout = try program.layouts.putTagUnion(&.{box_layout});
    program.layouts.updateLayout(box_layout, layout.Layout.box(fn_layout));
    const captures = try allocator.dupe(Program.CaptureSlot, &.{.{ .id = @enumFromInt(3), .slot = 0, .ty = undefined, .plan = plan, .storage = .recursive_box }});
    const variants = try allocator.dupe(Program.FnVariant, &.{.{ .id = @enumFromInt(0), .discriminant = 0, .variant_index = 0, .payload_layout = box_layout, .template = .{ .frozen_fn = 17, .fn_def = undefined, .source_fn_ty = undefined, .source_fn_key = undefined }, .captures = captures }});
    try program.fn_sets.append(allocator, .{ .layout = fn_layout, .variants = variants });
    return fn_layout;
}

test "frozen root transcode closes recursive callable graphs at target pointer width" {
    const allocator = std.testing.allocator;
    var source = try Program.Result.init(allocator, @import("base").target.TargetUsize.native);
    defer source.deinit();
    var target = try Program.Result.init(allocator, .u32);
    defer target.deinit();
    const source_layout = try recursiveCallable(allocator, &source);
    const target_layout = try recursiveCallable(allocator, &target);
    const plan: Program.ConstPlanId = @enumFromInt(0);
    var recursive_capture: usize = undefined;
    recursive_capture = @intFromPtr(&recursive_capture);
    const native = try @import("native_root_export.zig").freezeRoot(allocator, &source, @enumFromInt(0), testRoot(plan, source_layout), .{ .ptr = @ptrCast(&recursive_capture) }, .{});
    defer static_data.deinitStaticData(allocator, native);
    const converted = try transcodeRoot(allocator, &source, testRoot(plan, source_layout), native, @enumFromInt(0), &target, testRoot(plan, target_layout), @enumFromInt(0));
    defer static_data.deinitStaticData(allocator, converted);
    try std.testing.expectEqual(@as(usize, 2), converted.len);
    try std.testing.expectEqual(@as(usize, 4), converted[0].bytes.len);
    const pointer = converted[0].relocations[0];
    const backing = converted[@intFromEnum(pointer.target.data_symbol)];
    try std.testing.expectEqual(pointer.target.data_symbol, backing.relocations[0].target.data_symbol);
    try std.testing.expectEqual(pointer.addend, backing.relocations[0].addend);
    try std.testing.expectEqual(@as(u32, 0), std.mem.readInt(u32, backing.bytes[@intCast(pointer.addend - 4)..][0..4], .little));
}

test "frozen root transcode maps erased worker and drop identities across targets" {
    const allocator = std.testing.allocator;
    var source = try Program.Result.init(allocator, @import("base").target.TargetUsize.native);
    defer source.deinit();
    var target = try Program.Result.init(allocator, .u32);
    defer target.deinit();
    const source_proc = try source.store.addProcSpec(.{ .name = lir.Symbol.fromRaw(42), .args = .empty(), .ret_layout = .zst });
    const other_proc = try target.store.addProcSpec(.{ .name = lir.Symbol.fromRaw(71), .args = .empty(), .ret_layout = .zst });
    const target_proc = try target.store.addProcSpec(.{ .name = lir.Symbol.fromRaw(99), .args = .empty(), .ret_layout = .zst });
    const str_plan: Program.ConstPlanId = @enumFromInt(0);
    const fn_plan: Program.ConstPlanId = @enumFromInt(1);
    const source_layout = try source.layouts.insertErasedCallable();
    const target_layout = try target.layouts.insertErasedCallable();
    for ([_]*Program.Result{ &source, &target }) |program| {
        try program.const_plans.append(allocator, .str);
        try program.const_plans.append(allocator, .{ .erased_fn = @enumFromInt(0) });
    }
    const template = Program.FnTemplate{ .frozen_fn = 12, .frozen_worker = @as([96]u8, @splat(1)), .fn_def = undefined, .source_fn_ty = undefined, .source_fn_key = undefined };
    var other_template = template;
    other_template.frozen_worker = @as([96]u8, @splat(2));
    const capture = Program.CaptureSlot{ .id = @enumFromInt(5), .slot = 0, .ty = undefined, .plan = str_plan, .storage = .value };
    const source_captures = try allocator.dupe(Program.CaptureSlot, &.{capture});
    const target_captures = try allocator.dupe(Program.CaptureSlot, &.{capture});
    const drop = lir.LIR.ErasedCallableOnDrop{ .rc_helper = .{ .op = .decref, .layout_idx = .str } };
    const source_entries = try allocator.dupe(Program.ErasedFn, &.{.{ .entry = source_proc, .capture_layout = .str, .template = template, .captures = source_captures, .on_drop = drop }});
    const target_entries = try allocator.dupe(Program.ErasedFn, &.{
        .{ .entry = other_proc, .template = other_template },
        .{ .entry = target_proc, .capture_layout = .str, .template = template, .captures = target_captures, .on_drop = drop },
    });
    try source.erased_fns.append(allocator, .{ .layout = source_layout, .entries = source_entries });
    try target.erased_fns.append(allocator, .{ .layout = target_layout, .entries = target_entries });
    var string = builtins.str.RocStr.fromSliceSmall("capture");
    var pointer = @intFromPtr(&string);
    const Resolver = struct {
        fn resolve(_: ?*anyopaque, data: [*]u8) @import("native_root_export.zig").CallableResolution {
            return .{ .proc = @enumFromInt(0), .capture_ptr = data };
        }
    };
    const native = try @import("native_root_export.zig").freezeRoot(allocator, &source, @enumFromInt(0), testRoot(fn_plan, source_layout), .{ .ptr = @ptrCast(&pointer) }, .{ .resolve = Resolver.resolve });
    defer static_data.deinitStaticData(allocator, native);
    const converted = try transcodeRoot(allocator, &source, testRoot(fn_plan, source_layout), native, @enumFromInt(0), &target, testRoot(fn_plan, target_layout), @enumFromInt(0));
    defer static_data.deinitStaticData(allocator, converted);
    const root_pointer = converted[0].relocations[0];
    const payload = converted[@intFromEnum(root_pointer.target.data_symbol)];
    try std.testing.expectEqual(target_proc, payload.relocations[0].procedure.?);
    try std.testing.expectEqual(@as(u32, 16), payload.relocations[0].callable_capture_offset.?);
    try std.testing.expectEqualDeep(drop.rc_helper, payload.relocations[1].rc_helper.?);
    const capture_offset: usize = @intCast(root_pointer.addend + 16);
    try std.testing.expectEqualStrings("capture", payload.bytes[capture_offset..][0..7]);
    try std.testing.expectEqual(@as(u8, 0x87), payload.bytes[capture_offset + 11]);
}

fn failureSlot(allocator: Allocator, program: *Program.Result) Allocator.Error!lir.LIR.StaticDataId {
    const idx = try program.layouts.putStructFields(&.{ .{ .index = 0, .layout = .u8 }, .{ .index = 1, .layout = .str } });
    const data = program.layouts.getLayout(idx).getStruct().idx;
    const id: lir.LIR.StaticDataId = @enumFromInt(program.static_data_values.items.len);
    try program.static_data_values.append(allocator, .{ .initializer = null, .layout_idx = idx, .compile_time_root = .{ .module = .{ .bytes = @splat(0) }, .root = @enumFromInt(0), .const_locator = null, .role = .{ .failure_message = .{ .failed_field = 0, .message_field = 1, .failed_offset = program.layouts.getStructFieldOffsetByOriginalIndex(data, 0), .message_offset = program.layouts.getStructFieldOffsetByOriginalIndex(data, 1) } } } });
    return id;
}

test "frozen root transcode preserves failure flag and message using explicit field offsets" {
    const allocator = std.testing.allocator;
    var source = try Program.Result.init(allocator, @import("base").target.TargetUsize.native);
    defer source.deinit();
    var target = try Program.Result.init(allocator, .u32);
    defer target.deinit();
    _ = try failureSlot(allocator, &source);
    const target_id = try failureSlot(allocator, &target);
    const slot = source.static_data_values.items[0];
    const fields = slot.compile_time_root.?.role.failure_message;
    const bytes = try allocator.alloc(u8, source.layouts.layoutSize(source.layouts.getLayout(slot.layout_idx)));
    defer allocator.free(bytes);
    @memset(bytes, 0);
    bytes[fields.failed_offset] = 1;
    var message = builtins.str.RocStr.fromSliceSmall("failed value");
    @memcpy(bytes[fields.message_offset..][0..@sizeOf(builtins.str.RocStr)], std.mem.asBytes(&message));
    const source_exports = [_]static_data.StaticDataExport{.{ .symbol_name = "failure", .bytes = bytes, .alignment = 8, .is_global = false, .is_exported = false }};
    const converted = try transcodeFailure(allocator, &source, slot, &source_exports, @enumFromInt(0), &target, target_id);
    defer static_data.deinitStaticData(allocator, converted);
    const target_fields = target.static_data_values.items[0].compile_time_root.?.role.failure_message;
    try std.testing.expectEqual(@as(u8, 1), converted[0].bytes[target_fields.failed_offset]);
    try std.testing.expectEqual(@as(u32, 12), std.mem.readInt(u32, converted[0].bytes[target_fields.message_offset + 8 ..][0..4], .little));
    const rel = converted[0].relocations[0];
    try std.testing.expectEqualStrings("failed value", converted[@intFromEnum(rel.target.data_symbol)].bytes[@intCast(rel.addend)..]);
}

fn reorderedCallable(allocator: Allocator, program: *Program.Result, reverse: bool) Allocator.Error!layout.Idx {
    const str_plan: Program.ConstPlanId = @enumFromInt(0);
    try program.const_plans.append(allocator, .str);
    try program.const_plans.append(allocator, .{ .fn_value = @enumFromInt(0) });
    const idx = try program.layouts.putTagUnion(if (reverse) &.{ .str, .zst } else &.{ .zst, .str });
    const captures = try allocator.dupe(Program.CaptureSlot, &.{.{ .id = @enumFromInt(3), .slot = 0, .ty = undefined, .plan = str_plan, .storage = .value }});
    const selected: u16 = if (reverse) 0 else 1;
    const variants = try allocator.alloc(Program.FnVariant, 2);
    for (variants, 0..) |*variant, i| {
        const has_capture = i == selected;
        variant.* = .{ .id = @enumFromInt(i), .discriminant = @intCast(i), .variant_index = @intCast(i), .payload_layout = if (has_capture) .str else .zst, .template = .{ .frozen_fn = if (has_capture) 17 else 18, .fn_def = undefined, .source_fn_ty = undefined, .source_fn_key = undefined }, .captures = if (has_capture) captures else &.{} };
    }
    try program.fn_sets.append(allocator, .{ .layout = idx, .variants = variants });
    return idx;
}

test "frozen root transcode selects target callable discriminant by frozen origin" {
    const allocator = std.testing.allocator;
    var source = try Program.Result.init(allocator, @import("base").target.TargetUsize.native);
    defer source.deinit();
    var target = try Program.Result.init(allocator, .u32);
    defer target.deinit();
    const source_layout = try reorderedCallable(allocator, &source, false);
    const target_layout = try reorderedCallable(allocator, &target, true);
    const plan: Program.ConstPlanId = @enumFromInt(1);
    const bytes = try allocator.alloc(u8, source.layouts.layoutSize(source.layouts.getLayout(source_layout)));
    defer allocator.free(bytes);
    @memset(bytes, 0);
    var string = builtins.str.RocStr.fromSliceSmall("capture");
    @memcpy(bytes[0..@sizeOf(builtins.str.RocStr)], std.mem.asBytes(&string));
    source.layouts.getTagUnionData(source.layouts.getLayout(source_layout).getTagUnion().idx).writeDiscriminant(bytes.ptr, 1, source.layouts.targetUsize());
    const native = try @import("native_root_export.zig").freezeRoot(allocator, &source, @enumFromInt(0), testRoot(plan, source_layout), .{ .ptr = bytes.ptr }, .{});
    defer static_data.deinitStaticData(allocator, native);
    const converted = try transcodeRoot(allocator, &source, testRoot(plan, source_layout), native, @enumFromInt(0), &target, testRoot(plan, target_layout), @enumFromInt(0));
    defer static_data.deinitStaticData(allocator, converted);
    const data = target.layouts.getTagUnionData(target.layouts.getLayout(target_layout).getTagUnion().idx);
    try std.testing.expectEqual(@as(u32, 0), data.readDiscriminant(converted[0].bytes.ptr, target.layouts.targetUsize()));
    try std.testing.expectEqualStrings("capture", converted[0].bytes[0..7]);
    try std.testing.expectEqual(@as(u8, 0x87), converted[0].bytes[11]);
}
