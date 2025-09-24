//! Experimental next-gen interpreter implementing the type-carrying architecture.
//! This file will evolve behind its own tests until it replaces interpreter.zig.

const std = @import("std");
const types = @import("types");
const layout = @import("layout");
const can = @import("can");
const TypeScope = types.TypeScope;
const Content = types.Content;

pub const Interpreter2 = struct {
    allocator: std.mem.Allocator,
    runtime_types: *types.store.Store,
    runtime_layout_store: layout.Store,
    // O(1) Var -> Layout slot cache (0 = unset, else layout_idx + 1)
    var_to_layout_slot: std.ArrayList(u32),
    // Empty scope used when converting runtime vars to layouts
    empty_scope: TypeScope,
    // Translation cache: (env_ptr, compile_var) -> runtime_var
    translate_cache: std.AutoHashMap(u64, types.Var),

    pub fn init(allocator: std.mem.Allocator, env: *can.ModuleEnv) !Interpreter2 {
        const rt_types_ptr = try allocator.create(types.store.Store);
        rt_types_ptr.* = try types.store.Store.initCapacity(allocator, 1024, 512);
        var slots = try std.ArrayList(u32).initCapacity(allocator, 1024);
        slots.appendNTimesAssumeCapacity(0, 1024);
        const scope = TypeScope.init(allocator);
        var result = Interpreter2{
            .allocator = allocator,
            .runtime_types = rt_types_ptr,
            .runtime_layout_store = undefined, // set below to point at result.runtime_types
            .var_to_layout_slot = slots,
            .empty_scope = scope,
            .translate_cache = std.AutoHashMap(u64, types.Var).init(allocator),
        };
        result.runtime_layout_store = try layout.Store.init(env, result.runtime_types);
        return result;
    }

    pub fn deinit(self: *Interpreter2) void {
        self.empty_scope.deinit();
        self.translate_cache.deinit();
        self.var_to_layout_slot.deinit();
        self.runtime_layout_store.deinit();
        self.runtime_types.deinit();
        self.allocator.destroy(self.runtime_types);
    }

    /// Ensure the slot array can index at least `min_len` entries; zero-fill new entries.
    pub fn ensureVarLayoutCapacity(self: *Interpreter2, min_len: usize) !void {
        if (self.var_to_layout_slot.items.len >= min_len) return;
        const old_len = self.var_to_layout_slot.items.len;
        try self.var_to_layout_slot.ensureTotalCapacityPrecise(min_len);
        // Set new length and zero-fill
        self.var_to_layout_slot.items.len = min_len;
        @memset(self.var_to_layout_slot.items[old_len..], 0);
    }

    /// Get the layout for a runtime type var using the O(1) biased slot array.
    pub fn getRuntimeLayout(self: *Interpreter2, type_var: types.Var) !layout.Layout {
        const resolved = self.runtime_types.resolveVar(type_var);
        const idx: usize = @intFromEnum(resolved.var_);
        try self.ensureVarLayoutCapacity(idx + 1);
        const slot_ptr = &self.var_to_layout_slot.items[idx];
        if (slot_ptr.* != 0) {
            const layout_idx_plus_one = slot_ptr.*;
            const layout_idx: layout.Idx = @enumFromInt(layout_idx_plus_one - 1);
            return self.runtime_layout_store.getLayout(layout_idx);
        }

        const layout_idx = try self.runtime_layout_store.addTypeVar(resolved.var_, &self.empty_scope);
        slot_ptr.* = @intFromEnum(layout_idx) + 1;
        return self.runtime_layout_store.getLayout(layout_idx);
    }
    
    /// Minimal translate implementation (scaffolding): handles .str only for now
    pub fn translateTypeVar(self: *Interpreter2, module: *can.ModuleEnv, compile_var: types.Var) !types.Var {
        const key: u64 = (@as(u64, @intFromPtr(module)) << 32) | @as(u64, @intFromEnum(compile_var));
        if (self.translate_cache.get(key)) |found| return found;

        const resolved = module.types.resolveVar(compile_var);
        const out_var = switch (resolved.desc.content) {
            .structure => |flat| switch (flat) {
                .str => try self.runtime_types.freshFromContent(.{ .structure = .str }),
                .num => |n| switch (n) {
                    .num_compact => |c| switch (c) {
                        .int => |p| try self.runtime_types.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = .{ .int = p } } } }),
                        .frac => |p| try self.runtime_types.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = .{ .frac = p } } } }),
                    },
                    else => return error.NotImplemented,
                },
                .tuple => |t| {
                    const ct_elems = module.types.sliceVars(t.elems);
                    var buf = try self.allocator.alloc(types.Var, ct_elems.len);
                    defer self.allocator.free(buf);
                    for (ct_elems, 0..) |ct_elem, i| {
                        buf[i] = try self.translateTypeVar(module, ct_elem);
                    }
                    const range = try self.runtime_types.appendVars(buf);
                    return try self.runtime_types.freshFromContent(.{ .structure = .{ .tuple = .{ .elems = range } } });
                },
                else => return error.NotImplemented,
            },
            else => return error.NotImplemented,
        };

        try self.translate_cache.put(key, out_var);
        return out_var;
    }
};

pub fn add(a: i32, b: i32) i32 {
    return a + b;
}

// GREEN step: basic test to confirm the module’s tests run
test "interpreter2: wiring works" {
    try std.testing.expectEqual(@as(i32, 3), add(1, 2));
}

// RED: expect Var->Layout slot to work (will fail until implemented)
test "interpreter2: Var->Layout slot caches computed layout" {
    const gpa = std.testing.allocator;
    var env = try can.ModuleEnv.init(gpa, "");
    defer env.deinit();

    var interp = try Interpreter2.init(gpa, &env);
    defer interp.deinit();

    // Create a concrete runtime type: Str
    const str_var = try interp.runtime_types.freshFromContent(.{ .structure = .str });

    // Initially, slot is either absent or zero; ensure capacity then check
    const root_idx: usize = @intFromEnum(interp.runtime_types.resolveVar(str_var).var_);
    try interp.ensureVarLayoutCapacity(root_idx + 1);
    try std.testing.expectEqual(@as(u32, 0), interp.var_to_layout_slot.items[root_idx]);

    // Retrieve layout and expect scalar.str; slot becomes non-zero
    const layout_value = try interp.getRuntimeLayout(str_var);
    try std.testing.expect(layout_value.tag == .scalar);
    try std.testing.expect(layout_value.data.scalar.tag == .str);
    try std.testing.expect(interp.var_to_layout_slot.items[root_idx] != 0);
}

// RED: translating a compile-time str var should produce a runtime str var
test "interpreter2: translateTypeVar for str" {
    const gpa = std.testing.allocator;
    var env = try can.ModuleEnv.init(gpa, "");
    defer env.deinit();

    var interp = try Interpreter2.init(gpa, &env);
    defer interp.deinit();

    const ct_str = try env.types.freshFromContent(.{ .structure = .str });
    const rt_var = try interp.translateTypeVar(&env, ct_str);

    const resolved = interp.runtime_types.resolveVar(rt_var);
    try std.testing.expect(resolved.desc.content == .structure);
    try std.testing.expect(resolved.desc.content.structure == .str);
}

// RED: translating a compile-time concrete int64 should produce a runtime int64
test "interpreter2: translateTypeVar for int64" {
    const gpa = std.testing.allocator;
    var env = try can.ModuleEnv.init(gpa, "");
    defer env.deinit();

    var interp = try Interpreter2.init(gpa, &env);
    defer interp.deinit();

    const ct_int = try env.types.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = .{ .int = .i64 } } } });
    const rt_var = try interp.translateTypeVar(&env, ct_int);
    const resolved = interp.runtime_types.resolveVar(rt_var);
    try std.testing.expect(resolved.desc.content == .structure);
    switch (resolved.desc.content.structure) {
        .num => |n| switch (n) {
            .num_compact => |c| switch (c) {
                .int => |p| try std.testing.expectEqual(types.Num.Int.Precision.i64, p),
                else => return error.TestUnexpectedResult,
            },
            else => return error.TestUnexpectedResult,
        },
        else => return error.TestUnexpectedResult,
    }
}

// RED: translating a compile-time concrete f64 should produce a runtime f64
test "interpreter2: translateTypeVar for f64" {
    const gpa = std.testing.allocator;
    var env = try can.ModuleEnv.init(gpa, "");
    defer env.deinit();

    var interp = try Interpreter2.init(gpa, &env);
    defer interp.deinit();

    const ct_frac = try env.types.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = .{ .frac = .f64 } } } });
    const rt_var = try interp.translateTypeVar(&env, ct_frac);
    const resolved = interp.runtime_types.resolveVar(rt_var);
    try std.testing.expect(resolved.desc.content == .structure);
    switch (resolved.desc.content.structure) {
        .num => |n| switch (n) {
            .num_compact => |c| switch (c) {
                .frac => |p| try std.testing.expectEqual(types.Num.Frac.Precision.f64, p),
                else => return error.TestUnexpectedResult,
            },
            else => return error.TestUnexpectedResult,
        },
        else => return error.TestUnexpectedResult,
    }
}

// RED: translating a compile-time tuple (Str, I64) should produce a runtime tuple with same element shapes
test "interpreter2: translateTypeVar for tuple(Str, I64)" {
    const gpa = std.testing.allocator;
    var env = try can.ModuleEnv.init(gpa, "");
    defer env.deinit();

    var interp = try Interpreter2.init(gpa, &env);
    defer interp.deinit();

    const ct_str = try env.types.freshFromContent(.{ .structure = .str });
    const ct_i64 = try env.types.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = .{ .int = .i64 } } } });
    const elems = [_]types.Var{ ct_str, ct_i64 };
    const ct_tuple = try env.types.freshFromContent(.{ .structure = .{ .tuple = .{ .elems = try env.types.appendVars(&elems) } } });

    const rt_var = try interp.translateTypeVar(&env, ct_tuple);
    const resolved = interp.runtime_types.resolveVar(rt_var);
    try std.testing.expect(resolved.desc.content == .structure);
    switch (resolved.desc.content.structure) {
        .tuple => |t| {
            const rt_elems = interp.runtime_types.sliceVars(t.elems);
            try std.testing.expectEqual(@as(usize, 2), rt_elems.len);
            // elem 0: str
            const e0 = interp.runtime_types.resolveVar(rt_elems[0]);
            try std.testing.expect(e0.desc.content == .structure);
            try std.testing.expect(e0.desc.content.structure == .str);
            // elem 1: i64
            const e1 = interp.runtime_types.resolveVar(rt_elems[1]);
            try std.testing.expect(e1.desc.content == .structure);
            switch (e1.desc.content.structure) {
                .num => |n| switch (n) {
                    .num_compact => |c| switch (c) {
                        .int => |p| try std.testing.expectEqual(types.Num.Int.Precision.i64, p),
                        else => return error.TestUnexpectedResult,
                    },
                    else => return error.TestUnexpectedResult,
                },
                else => return error.TestUnexpectedResult,
            }
        },
        else => return error.TestUnexpectedResult,
    }
}
