//! Discovery summaries (design.md "Discovery Summaries"), computed from the
//! lifted program once a compilation has evaluated every literal root it
//! registered without an error or a message.
//!
//! A summary names a specialization by the key its body was reserved under
//! and records the checked keys of the modules its closure's code comes from.
//! A later compilation whose lowering has every one of those modules at the
//! same key would lower the same closure, registering the same literal roots
//! with the same outcomes, so it parks the request instead.
//!
//! The module set covers everything a parked body could reach. A function
//! names code only in its module's import closure, which that module's
//! checked key covers, and a function value a body receives comes from code
//! already in the closure. A body the compilation did not lower (a parked
//! stub or an object-cache entry) outside the app can still reach the app
//! through a platform requirement, so a closure holding one records the app
//! module's key too.

const std = @import("std");
const Lifted = @import("monotype_lifted/ast.zig");
const Allocator = std.mem.Allocator;

/// One specialization's summary: its key and the sorted checked keys of the
/// modules its closure's code comes from.
pub const Summary = struct {
    key: [32]u8,
    modules: []const [32]u8,
};

/// Every summary of one compilation, owned by its arena.
pub const Summaries = struct {
    arena: std.heap.ArenaAllocator,
    items: []const Summary,

    pub fn deinit(self: *Summaries) void {
        self.arena.deinit();
    }
};

/// The summary of every keyed specialization body `program` holds.
pub fn collect(allocator: Allocator, program: *const Lifted.Program) Allocator.Error!Summaries {
    var arena = std.heap.ArenaAllocator.init(allocator);
    errdefer arena.deinit();
    const fn_count = program.fnCount();

    // The module each function's code comes from, as an index into `modules`.
    var modules = std.ArrayList([32]u8).empty;
    defer modules.deinit(allocator);
    var module_index = std.AutoHashMap([32]u8, u32).init(allocator);
    defer module_index.deinit();
    const own_module = try allocator.alloc(?u32, fn_count);
    defer allocator.free(own_module);
    @memset(own_module, null);
    // Whether the function's code is not in the program: a parked stub or an
    // object-cache entry.
    const unlowered = try allocator.alloc(bool, fn_count);
    defer allocator.free(unlowered);
    @memset(unlowered, false);

    const Intern = struct {
        fn module(list: *std.ArrayList([32]u8), index: *std.AutoHashMap([32]u8, u32), gpa: Allocator, key: [32]u8) Allocator.Error!u32 {
            const entry = try index.getOrPut(key);
            if (!entry.found_existing) {
                entry.value_ptr.* = @intCast(list.items.len);
                try list.append(gpa, key);
            }
            return entry.value_ptr.*;
        }
    };
    for (0..fn_count) |raw| {
        const fn_ = program.getFn(@fromBackingInt(@intCast(raw)));
        const template = fn_.source orelse continue;
        const proc_template = switch (template.fn_def) {
            .local_template, .imported_template, .checked_generated => |proc_template| proc_template,
            .nested => |nested| nested.owner,
            .local_hosted, .imported_hosted => |hosted_fn| hosted_fn.template,
            inline .parser_runtime, .encoder_for_runtime => |runtime| runtime.owner,
        };
        own_module[raw] = try Intern.module(&modules, &module_index, allocator, proc_template.artifact.bytes);
        const stub = template.discovery and template.discovery_key == null;
        const cached = fn_.body == .hosted and switch (template.fn_def) {
            .local_hosted, .imported_hosted => false,
            .local_template, .imported_template, .checked_generated, .nested, .parser_runtime, .encoder_for_runtime => true,
        };
        unlowered[raw] = stub or cached;
    }
    for (program.literalRootsView()) |root| {
        own_module[@backingInt(root.fn_id)] = try Intern.module(&modules, &module_index, allocator, root.module.bytes);
    }
    const lowering_modules = program.loweringModules();
    for (program.rootsView()) |root| {
        own_module[@backingInt(root.fn_id)] = try Intern.module(&modules, &module_index, allocator, lowering_modules[@backingInt(root.owner)].bytes);
    }
    const app_module: ?u32 = if (program.platform_requirement_filling) |filling|
        try Intern.module(&modules, &module_index, allocator, filling.app_module)
    else
        null;

    // Every function each function references, in one flat list.
    const edge_starts = try allocator.alloc(u32, fn_count + 1);
    defer allocator.free(edge_starts);
    var edges = std.ArrayList(u32).empty;
    defer edges.deinit(allocator);
    const Collector = struct {
        program: *const Lifted.Program,
        allocator: Allocator,
        edges: *std.ArrayList(u32),

        pub fn enterExpr(collector: @This(), expr_id: Lifted.ExprId) Allocator.Error!Lifted.ExprWalk {
            switch (collector.program.getExpr(expr_id).data) {
                .fn_ref => |value| try collector.edges.append(collector.allocator, @backingInt(value.fn_id)),
                .call_proc => |call| try collector.edges.append(collector.allocator, @backingInt(Lifted.localDirectCallee(call).?)),
                // A value read's initializer names the producer whose
                // conversion a literal root runs, so the walk descends into it.
                .call_value, .comptime_value, .local, .unit, .@"unreachable", .int_lit, .frac_f32_lit, .frac_f64_lit, .dec_lit, .str_lit, .bytes_lit, .static_data_candidate, .typed_boundary, .row_widen, .list, .tuple, .record, .record_update, .tag, .nominal, .let_, .lambda, .def_ref, .fn_def, .low_level, .field_access, .tuple_access, .structural_eq, .structural_hash, .match_, .if_, .uninitialized, .uninitialized_payload, .if_initialized_payload, .try_sequence, .try_record_sequence, .block, .loop_, .break_, .continue_, .join_point, .jump, .return_, .crash, .checked_error, .comptime_exhaustiveness_failed, .dbg, .expect_err, .expect, .literal_rejected => {},
            }
            return .descend;
        }

        pub fn exitExpr(_: @This(), _: Lifted.ExprId) Allocator.Error!void {}
    };
    for (0..fn_count) |raw| {
        edge_starts[raw] = @intCast(edges.items.len);
        switch (program.getFn(@fromBackingInt(@intCast(raw))).body) {
            .roc => |body| try Lifted.walkExprs(allocator, program, .{ .expr = body }, Collector{ .program = program, .allocator = allocator, .edges = &edges }),
            .hosted => {},
        }
    }
    edge_starts[fn_count] = @intCast(edges.items.len);

    // Close each function's modules over its references: a strongly
    // connected group has its members' modules and those of everything it
    // references. Tarjan's algorithm finishes a group after every group it
    // references.
    const module_count = modules.items.len;
    var group_modules = std.ArrayList(std.DynamicBitSetUnmanaged).empty;
    defer {
        for (group_modules.items) |*set| set.deinit(allocator);
        group_modules.deinit(allocator);
    }
    var group_unlowered = std.ArrayList(bool).empty;
    defer group_unlowered.deinit(allocator);
    const group_of = try allocator.alloc(u32, fn_count);
    defer allocator.free(group_of);
    {
        const unvisited = std.math.maxInt(u32);
        const index = try allocator.alloc(u32, fn_count);
        defer allocator.free(index);
        @memset(index, unvisited);
        const low = try allocator.alloc(u32, fn_count);
        defer allocator.free(low);
        const on_stack = try allocator.alloc(bool, fn_count);
        defer allocator.free(on_stack);
        @memset(on_stack, false);
        var stack = std.ArrayList(u32).empty;
        defer stack.deinit(allocator);
        const Frame = struct { node: u32, next: u32 };
        var frames = std.ArrayList(Frame).empty;
        defer frames.deinit(allocator);
        var next_index: u32 = 0;
        for (0..fn_count) |start_raw| {
            if (index[start_raw] != unvisited) continue;
            const start: u32 = @intCast(start_raw);
            index[start] = next_index;
            low[start] = next_index;
            next_index += 1;
            try stack.append(allocator, start);
            on_stack[start] = true;
            try frames.append(allocator, .{ .node = start, .next = edge_starts[start] });
            while (frames.items.len != 0) {
                const frame = &frames.items[frames.items.len - 1];
                const node = frame.node;
                if (frame.next < edge_starts[node + 1]) {
                    const callee = edges.items[frame.next];
                    frame.next += 1;
                    if (index[callee] == unvisited) {
                        index[callee] = next_index;
                        low[callee] = next_index;
                        next_index += 1;
                        try stack.append(allocator, callee);
                        on_stack[callee] = true;
                        try frames.append(allocator, .{ .node = callee, .next = edge_starts[callee] });
                    } else if (on_stack[callee]) {
                        low[node] = @min(low[node], index[callee]);
                    }
                    continue;
                }
                _ = frames.pop();
                if (frames.items.len != 0) {
                    const parent = frames.items[frames.items.len - 1].node;
                    low[parent] = @min(low[parent], low[node]);
                }
                if (low[node] != index[node]) continue;
                const group: u32 = @intCast(group_modules.items.len);
                var set = try std.DynamicBitSetUnmanaged.initEmpty(allocator, module_count);
                errdefer set.deinit(allocator);
                var any_unlowered = false;
                var member_start = stack.items.len;
                while (true) {
                    member_start -= 1;
                    if (stack.items[member_start] == node) break;
                }
                for (stack.items[member_start..]) |member| {
                    on_stack[member] = false;
                    group_of[member] = group;
                }
                for (stack.items[member_start..]) |member| {
                    if (own_module[member]) |module| set.set(module);
                    if (unlowered[member]) any_unlowered = true;
                    for (edges.items[edge_starts[member]..edge_starts[member + 1]]) |callee| {
                        const callee_group = group_of[callee];
                        if (callee_group == group) continue;
                        set.setUnion(group_modules.items[callee_group]);
                        if (group_unlowered.items[callee_group]) any_unlowered = true;
                    }
                }
                stack.shrinkRetainingCapacity(member_start);
                try group_modules.append(allocator, set);
                try group_unlowered.append(allocator, any_unlowered);
            }
        }
    }

    const arena_allocator = arena.allocator();
    var summaries = std.ArrayList(Summary).empty;
    for (0..fn_count) |raw| {
        const fn_ = program.getFn(@fromBackingInt(@intCast(raw)));
        if (fn_.body != .roc or fn_.spec_constr_pattern != null) continue;
        const template = fn_.source orelse continue;
        const key = (if (template.discovery) template.discovery_key else template.spec_key) orelse continue;
        const group = group_of[raw];
        var set = &group_modules.items[group];
        if (group_unlowered.items[group]) if (app_module) |app| set.set(app);
        const recorded = try arena_allocator.alloc([32]u8, set.count());
        var iterator = set.iterator(.{});
        var written: usize = 0;
        while (iterator.next()) |module| : (written += 1) recorded[written] = modules.items[module];
        std.mem.sort([32]u8, recorded, {}, keyLessThan);
        try summaries.append(arena_allocator, .{ .key = key.bytes, .modules = recorded });
    }
    std.mem.sort(Summary, summaries.items, {}, summaryLessThan);
    return .{ .arena = arena, .items = summaries.items };
}

fn keyLessThan(_: void, a: [32]u8, b: [32]u8) bool {
    return std.mem.order(u8, &a, &b) == .lt;
}

fn summaryLessThan(_: void, a: Summary, b: Summary) bool {
    switch (std.mem.order(u8, &a.key, &b.key)) {
        .lt => return true,
        .gt => return false,
        .eq => {},
    }
    if (a.modules.len != b.modules.len) return a.modules.len < b.modules.len;
    for (a.modules, b.modules) |left, right| switch (std.mem.order(u8, &left, &right)) {
        .lt => return true,
        .gt => return false,
        .eq => {},
    };
    return false;
}
