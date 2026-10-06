//! Match a materialized CTFE environment to its producer's closed recipe.
//! Traversal follows explicit descriptor and dictionary references only.
const std = @import("std");
const lir = @import("lir");
const Program = lir.Program;
const Runtime = @import("boxy_runtime.zig").BoxyRuntime;

/// Exact comparison of producer schemas and materialized environment evidence.
pub const Matcher = struct {
    const Pair = struct { left: usize, right: usize, dictionary: bool };

    /// A pair of descriptors or dictionaries still to compare.
    const Pending = union(enum) {
        descriptor: [2]*const Program.BoxyTypeDesc,
        dictionary: [2]*const Program.BoxyDict,
    };

    allocator: std.mem.Allocator,
    runtime: *const Runtime,
    program: *const Program.Result,
    seen: std.AutoHashMapUnmanaged(Pair, void) = .empty,
    pending: std.ArrayListUnmanaged(Pending) = .empty,

    pub fn deinit(self: *Matcher) void {
        self.seen.deinit(self.allocator);
        self.pending.deinit(self.allocator);
    }

    pub fn descriptor(self: *Matcher, a: *const Program.BoxyTypeDesc, b: *const Program.BoxyTypeDesc) std.mem.Allocator.Error!bool {
        return try self.run(.{ .descriptor = .{ a, b } });
    }

    pub fn dictionary(self: *Matcher, a: *const Program.BoxyDict, b: *const Program.BoxyDict) std.mem.Allocator.Error!bool {
        return try self.run(.{ .dictionary = .{ a, b } });
    }

    /// Compare `root` and every pair it reaches from an explicit work list.
    fn run(self: *Matcher, root: Pending) std.mem.Allocator.Error!bool {
        self.pending.clearRetainingCapacity();
        try self.pending.append(self.allocator, root);
        while (self.pending.pop()) |item| {
            const matches = switch (item) {
                .descriptor => |pair| try self.descriptorFields(pair[0], pair[1]),
                .dictionary => |pair| try self.dictionaryFields(pair[0], pair[1]),
            };
            if (!matches) {
                self.pending.clearRetainingCapacity();
                return false;
            }
        }
        return true;
    }

    fn pushDescriptor(self: *Matcher, a: Program.BoxyDescRef, b: Program.BoxyDescRef) std.mem.Allocator.Error!void {
        try self.pending.append(self.allocator, .{ .descriptor = .{ self.desc(a), self.desc(b) } });
    }

    fn desc(self: *Matcher, ref: Program.BoxyDescRef) *const Program.BoxyTypeDesc {
        return switch (ref) {
            .static => |id| self.runtime.requireBoxyTypeDesc(id),
            .runtime => |id| self.runtime.runtime_boxy_type_descs.items[id],
            .local, .dict_method_arg, .dict_method_hidden => @panic("materialized freeze witness retained frame evidence"),
        };
    }

    fn dict(self: *Matcher, ref: Program.BoxyDictRef) *const Program.BoxyDict {
        return switch (ref) {
            .static => |id| self.runtime.requireBoxyDict(id),
            .runtime => |id| self.runtime.runtime_boxy_dicts.dicts.items[id],
            .local => @panic("materialized freeze witness retained a dictionary local"),
        };
    }

    /// Queue each descriptor pair of two spans; false when their lengths
    /// differ.
    fn descriptors(self: *Matcher, a: Program.BoxySpan, b: Program.BoxySpan) std.mem.Allocator.Error!bool {
        if (a.len != b.len) return false;
        for (self.runtime.requireBoxyDescRefs(a), self.runtime.requireBoxyDescRefs(b)) |left, right| {
            try self.pushDescriptor(left, right);
        }
        return true;
    }

    fn dictionaries(self: *Matcher, a: Program.BoxySpan, b: Program.BoxySpan) std.mem.Allocator.Error!bool {
        if (a.len != b.len) return false;
        for (self.runtime.requireBoxyDictRefs(a), self.runtime.requireBoxyDictRefs(b)) |left, right| {
            try self.pending.append(self.allocator, .{ .dictionary = .{ self.dict(left), self.dict(right) } });
        }
        return true;
    }

    fn optionalDescriptor(self: *Matcher, a: ?Program.BoxyDescRef, b: ?Program.BoxyDescRef) std.mem.Allocator.Error!bool {
        if (a) |left| {
            const right = b orelse return false;
            try self.pushDescriptor(left, right);
            return true;
        }
        return b == null;
    }

    fn payload(self: *Matcher, a: Program.BoxySpan, b: Program.BoxySpan) std.mem.Allocator.Error!bool {
        if (a.len != b.len) return false;
        for (self.runtime.requireBoxyPayloadSteps(a), self.runtime.requireBoxyPayloadSteps(b)) |left, right| {
            if (std.meta.activeTag(left) != std.meta.activeTag(right)) return false;
            switch (left) {
                .concrete => |step| if (!std.meta.eql(step, right.concrete)) {
                    return false;
                },
                .dynamic => |step| {
                    if (step.op != right.dynamic.op) return false;
                    try self.pushDescriptor(step.desc, right.dynamic.desc);
                },
            }
        }
        return true;
    }

    /// Compare two descriptors' own fields, queueing the descriptors they
    /// reference.
    fn descriptorFields(self: *Matcher, a: *const Program.BoxyTypeDesc, b: *const Program.BoxyTypeDesc) std.mem.Allocator.Error!bool {
        if (a == b) return true;
        const key: Pair = .{ .left = @intFromPtr(a), .right = @intFromPtr(b), .dictionary = false };
        if ((try self.seen.getOrPut(self.allocator, key)).found_existing) return true;
        inline for (.{ "payload_layout", "contains_refcounted", "shape", "presence_slot_present_discriminant", "inspect_opaque", "inspect_method" }) |field| {
            if (!std.meta.eql(@field(a, field), @field(b, field))) return false;
        }
        inline for (.{ "nested_descs", "inspect_hidden_descs", "inspect_arg_descs" }) |field| {
            if (!try self.descriptors(@field(a, field), @field(b, field))) return false;
        }
        if (!try self.optionalDescriptor(a.tag_ext_desc, b.tag_ext_desc)) return false;
        if (!try self.optionalDescriptor(a.inspect_from, b.inspect_from)) return false;
        const left_names = self.runtime.requireBoxyFieldNames(a.field_names);
        const right_names = self.runtime.requireBoxyFieldNames(b.field_names);
        if (left_names.len != right_names.len) return false;
        for (left_names, right_names) |left, right| if (left != right) return false;
        if (!try self.payload(a.copy_plan, b.copy_plan) or !try self.payload(a.drop_plan, b.drop_plan)) return false;
        if (a.tag_variants.len != b.tag_variants.len) return false;
        for (self.runtime.requireBoxyTagVariants(a.tag_variants), self.runtime.requireBoxyTagVariants(b.tag_variants)) |left, right| {
            inline for (.{ "name", "discriminant", "payload_count", "payload_layout" }) |field| {
                if (!std.meta.eql(@field(left, field), @field(right, field))) return false;
            }
            if (left.payload_descs.len != right.payload_descs.len) return false;
            for (self.runtime.requireBoxyTagPayloadDescs(left.payload_descs), self.runtime.requireBoxyTagPayloadDescs(right.payload_descs)) |lp, rp| {
                if (lp.payload_index != rp.payload_index) return false;
                try self.pushDescriptor(lp.desc, rp.desc);
            }
        }
        return true;
    }

    /// Compare two dictionaries' own fields, queueing the descriptors and
    /// dictionaries they reference.
    fn dictionaryFields(self: *Matcher, a: *const Program.BoxyDict, b: *const Program.BoxyDict) std.mem.Allocator.Error!bool {
        if (a == b) return true;
        const key: Pair = .{ .left = @intFromPtr(a), .right = @intFromPtr(b), .dictionary = true };
        if ((try self.seen.getOrPut(self.allocator, key)).found_existing) return true;
        if (a.template or b.template) @panic("frozen dictionary witness retained frame captures");
        if (a.method_slots.len != b.method_slots.len) return false;
        for (self.runtime.requireBoxyMethodSlots(a.method_slots), self.runtime.requireBoxyMethodSlots(b.method_slots)) |left, right| {
            if (left.present != right.present) return false;
            if (!left.present) continue;
            if (left.method != right.method) return false;
            if (left.proc != right.proc) {
                const left_worker = self.program.boxy_frozen_method_origins.get(left.proc) orelse return false;
                const right_worker = self.program.boxy_frozen_method_origins.get(right.proc) orelse return false;
                if (!std.meta.eql(left_worker, right_worker)) return false;
            }
            if (!try self.descriptors(left.hidden_descs, right.hidden_descs) or !try self.dictionaries(left.nested_dicts, right.nested_dicts)) return false;
            const la = left.adapter;
            const ra = right.adapter;
            if (la.ret_layout != ra.ret_layout) return false;
            const left_layouts = self.runtime.requireBoxyMethodArgLayouts(la.arg_layouts);
            const right_layouts = self.runtime.requireBoxyMethodArgLayouts(ra.arg_layouts);
            if (left_layouts.len != right_layouts.len) return false;
            for (left_layouts, right_layouts) |left_layout, right_layout| if (left_layout != right_layout) return false;
            inline for (.{ "arg_descs", "call_descs" }) |field| if (!try self.descriptors(@field(la, field), @field(ra, field))) {
                return false;
            };
            inline for (.{ "call_desc_sources", "hidden_desc_sources" }) |field| {
                const ls = self.runtime.requireBoxyMethodHiddenDescSources(@field(la, field));
                const rs = self.runtime.requireBoxyMethodHiddenDescSources(@field(ra, field));
                if (ls.len != rs.len) return false;
                for (ls, rs) |l, r| if (!std.meta.eql(l, r)) {
                    return false;
                };
            }
            if (!try self.optionalDescriptor(la.ret_desc, ra.ret_desc) or !try self.dictionaries(la.nested_dicts, ra.nested_dicts)) return false;
        }
        return true;
    }
};

test "Boxy frozen evidence compares runtime descriptor graphs and declared method origins" {
    const gpa = std.testing.allocator;
    var program = try Program.Result.init(gpa, @import("base").target.TargetUsize.native);
    defer program.deinit();
    const str_desc: Program.BoxyTypeDescId = @enumFromInt(program.boxy_type_descs.items.len);
    try program.boxy_type_descs.append(gpa, .{ .payload_layout = .str, .contains_refcounted = true, .shape = .primitive, .closure = .closed });
    const int_desc: Program.BoxyTypeDescId = @enumFromInt(program.boxy_type_descs.items.len);
    try program.boxy_type_descs.append(gpa, .{ .payload_layout = .u64, .contains_refcounted = false, .shape = .primitive, .closure = .closed });
    try program.boxy_desc_refs.append(gpa, .{ .static = str_desc });
    const list_desc: Program.BoxyTypeDescId = @enumFromInt(program.boxy_type_descs.items.len);
    try program.boxy_type_descs.append(gpa, .{ .payload_layout = try program.layouts.insertList(.str), .contains_refcounted = true, .shape = .list, .nested_descs = .{ .start = 0, .len = 1 }, .closure = .closed });
    const first = try program.store.addProcSpec(.{ .name = lir.Symbol.fromRaw(1), .identity = lir.LIR.ProcIdentity.forTest(1), .args = .empty(), .ret_layout = .zst }, .none);
    const second = try program.store.addProcSpec(.{ .name = lir.Symbol.fromRaw(2), .identity = lir.LIR.ProcIdentity.forTest(2), .args = .empty(), .ret_layout = .zst }, .none);
    const method: @import("check").CheckedNames.MethodNameId = @enumFromInt(1);
    try program.boxy_method_slots.appendSlice(gpa, &.{ .{ .method = method, .proc = first }, .{ .method = method, .proc = second } });
    var host = @import("runtime_host.zig").init(gpa);
    defer host.deinit();
    const runtime = try @import("boxy_abi.zig").createRuntimeFromStores(gpa, &program.store, &program.layouts, @import("boxy_runtime.zig").BoxyTables.fromResult(&program), host.get_ops());
    defer @import("boxy_abi.zig").deinitRuntime(runtime);
    try runtime.runtime_boxy_desc_refs.appendSlice(gpa, &.{ .{ .static = str_desc }, .{ .static = int_desc } });
    var actual = program.boxy_type_descs.items[@intFromEnum(list_desc)];
    actual.nested_descs = @import("boxy_runtime.zig").makeRuntimeBoxySpan(0, 1);
    actual.closure = .context;
    var matcher = Matcher{ .allocator = gpa, .runtime = &runtime.runtime, .program = &program };
    defer matcher.deinit();
    try std.testing.expect(try matcher.descriptor(&actual, &program.boxy_type_descs.items[@intFromEnum(list_desc)]));
    matcher.seen.clearRetainingCapacity();
    actual.nested_descs = @import("boxy_runtime.zig").makeRuntimeBoxySpan(1, 1);
    try std.testing.expect(!try matcher.descriptor(&actual, &program.boxy_type_descs.items[@intFromEnum(list_desc)]));
    matcher.seen.clearRetainingCapacity();
    const left = Program.BoxyDict{ .method_slots = .{ .start = 0, .len = 1 } };
    const right = Program.BoxyDict{ .method_slots = .{ .start = 1, .len = 1 } };
    try std.testing.expect(!try matcher.dictionary(&left, &right));
    matcher.seen.clearRetainingCapacity();
    // Equality requires producer evidence, never similarity of procedure bodies.
    const origin = Program.BoxyFrozenMethodOrigin{ .worker = program.store.getProcSpec(first).identity, .requirement_module = .{ .bytes = @splat(0) }, .requirement_type = @enumFromInt(1), .callable_module = .{ .bytes = @splat(0) }, .callable_type = @enumFromInt(2) };
    try program.boxy_frozen_method_origins.put(gpa, first, origin);
    try program.boxy_frozen_method_origins.put(gpa, second, origin);
    try std.testing.expect(try matcher.dictionary(&left, &right));
    matcher.seen.clearRetainingCapacity();
    var other = origin;
    other.requirement_type = @enumFromInt(3);
    try program.boxy_frozen_method_origins.put(gpa, second, other);
    try std.testing.expect(!try matcher.dictionary(&left, &right));
}
