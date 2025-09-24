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
                .record => |rec| {
                    // Translate fields
                    const ct_fields = module.types.getRecordFieldsSlice(rec.fields);
                    var tmp = try self.allocator.alloc(types.RecordField, ct_fields.len);
                    defer self.allocator.free(tmp);
                    var i: usize = 0;
                    while (i < ct_fields.len) : (i += 1) {
                        const f = ct_fields.get(i);
                        const rt_field_var = try self.translateTypeVar(module, f.var_);
                        tmp[i] = .{ .name = f.name, .var_ = rt_field_var };
                    }
                    const rt_fields = try self.runtime_types.appendRecordFields(tmp);
                    // Translate ext var too
                    const rt_ext = try self.translateTypeVar(module, rec.ext);
                    return try self.runtime_types.freshFromContent(.{ .structure = .{ .record = .{ .fields = rt_fields, .ext = rt_ext } } });
                },
                .empty_record => try self.runtime_types.freshFromContent(.{ .structure = .empty_record }),
                .nominal_type => |nom| {
                    const ct_backing = module.types.getNominalBackingVar(nom);
                    const rt_backing = try self.translateTypeVar(module, ct_backing);
                    const ct_args = module.types.sliceNominalArgs(nom);
                    var buf = try self.allocator.alloc(types.Var, ct_args.len);
                    defer self.allocator.free(buf);
                    for (ct_args, 0..) |ct_arg, i| {
                        buf[i] = try self.translateTypeVar(module, ct_arg);
                    }
                    const content = try self.runtime_types.mkNominal(nom.ident, rt_backing, buf, nom.origin_module);
                    return try self.runtime_types.register(.{ .content = content, .rank = types.Rank.top_level, .mark = types.Mark.none });
                },
                else => return error.NotImplemented,
            },
            .alias => |alias| {
                const ct_backing = module.types.getAliasBackingVar(alias);
                const rt_backing = try self.translateTypeVar(module, ct_backing);
                const ct_args = module.types.sliceAliasArgs(alias);
                var buf = try self.allocator.alloc(types.Var, ct_args.len);
                defer self.allocator.free(buf);
                for (ct_args, 0..) |ct_arg, i| {
                    buf[i] = try self.translateTypeVar(module, ct_arg);
                }
                const content = try self.runtime_types.mkAlias(alias.ident, rt_backing, buf);
                return try self.runtime_types.register(.{ .content = content, .rank = types.Rank.top_level, .mark = types.Mark.none });
            },
            .flex_var => |id_opt| {
                const content: types.Content = .{ .flex_var = id_opt };
                return try self.runtime_types.freshFromContent(content);
            },
            .rigid_var => |ident| {
                const content: types.Content = .{ .rigid_var = ident };
                return try self.runtime_types.freshFromContent(content);
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

// RED: translating a compile-time record { first: Str, second: I64 } should produce equivalent runtime record
test "interpreter2: translateTypeVar for record {first: Str, second: I64}" {
    const gpa = std.testing.allocator;
    var env = try can.ModuleEnv.init(gpa, "");
    defer env.deinit();

    var interp = try Interpreter2.init(gpa, &env);
    defer interp.deinit();

    // Build compile-time record content
    const name_first = try env.common.idents.insert(gpa, @import("base").Ident.for_text("first"));
    const name_second = try env.common.idents.insert(gpa, @import("base").Ident.for_text("second"));
    const ct_str = try env.types.freshFromContent(.{ .structure = .str });
    const ct_i64 = try env.types.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = .{ .int = .i64 } } } });
    var ct_fields = [_]types.RecordField{
        .{ .name = name_first, .var_ = ct_str },
        .{ .name = name_second, .var_ = ct_i64 },
    };
    const ct_fields_range = try env.types.appendRecordFields(&ct_fields);
    const ct_ext_empty = try env.types.freshFromContent(.{ .structure = .empty_record });
    const ct_record = try env.types.freshFromContent(.{ .structure = .{ .record = .{ .fields = ct_fields_range, .ext = ct_ext_empty } } });

    // Translate
    const rt_var = try interp.translateTypeVar(&env, ct_record);
    const resolved = interp.runtime_types.resolveVar(rt_var);
    try std.testing.expect(resolved.desc.content == .structure);
    switch (resolved.desc.content.structure) {
        .record => |rec| {
            const rt_fields = interp.runtime_types.getRecordFieldsSlice(rec.fields);
            try std.testing.expectEqual(@as(u32, 2), rt_fields.len);
            const f0 = rt_fields.get(0);
            const f1 = rt_fields.get(1);
            // Field names are preserved
            try std.testing.expectEqual(name_first, f0.name);
            try std.testing.expectEqual(name_second, f1.name);
            // Field 0 type is Str
            const e0 = interp.runtime_types.resolveVar(f0.var_);
            try std.testing.expect(e0.desc.content == .structure);
            try std.testing.expect(e0.desc.content.structure == .str);
            // Field 1 type is I64
            const e1 = interp.runtime_types.resolveVar(f1.var_);
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

// RED: translating a compile-time alias should produce equivalent runtime alias
test "interpreter2: translateTypeVar for alias of Str" {
    const gpa = std.testing.allocator;
    var env = try can.ModuleEnv.init(gpa, "");
    defer env.deinit();

    var interp = try Interpreter2.init(gpa, &env);
    defer interp.deinit();

    const alias_name = try env.common.idents.insert(gpa, @import("base").Ident.for_text("MyAlias"));
    const type_ident = types.TypeIdent{ .ident_idx = alias_name };
    const ct_str = try env.types.freshFromContent(.{ .structure = .str });
    const ct_alias_content = try env.types.mkAlias(type_ident, ct_str, &.{});
    const ct_alias_var = try env.types.register(.{ .content = ct_alias_content, .rank = types.Rank.top_level, .mark = types.Mark.none });

    const rt_var = try interp.translateTypeVar(&env, ct_alias_var);
    const resolved = interp.runtime_types.resolveVar(rt_var);
    try std.testing.expect(resolved.desc.content == .alias);
    const rt_alias = resolved.desc.content.alias;
    try std.testing.expectEqual(alias_name, rt_alias.ident.ident_idx);
    const rt_backing = interp.runtime_types.getAliasBackingVar(rt_alias);
    const backing_resolved = interp.runtime_types.resolveVar(rt_backing);
    try std.testing.expect(backing_resolved.desc.content == .structure);
    try std.testing.expect(backing_resolved.desc.content.structure == .str);
}

// RED: translating a compile-time nominal type should produce equivalent runtime nominal
test "interpreter2: translateTypeVar for nominal Point(Str)" {
    const gpa = std.testing.allocator;
    var env = try can.ModuleEnv.init(gpa, "");
    defer env.deinit();

    var interp = try Interpreter2.init(gpa, &env);
    defer interp.deinit();

    const name_nominal = try env.common.idents.insert(gpa, @import("base").Ident.for_text("Point"));
    const type_ident = types.TypeIdent{ .ident_idx = name_nominal };
    const ct_str = try env.types.freshFromContent(.{ .structure = .str });
    // backing type is Str for simplicity
    const ct_nominal_content = try env.types.mkNominal(type_ident, ct_str, &.{}, name_nominal);
    const ct_nominal_var = try env.types.register(.{ .content = ct_nominal_content, .rank = types.Rank.top_level, .mark = types.Mark.none });

    const rt_var = try interp.translateTypeVar(&env, ct_nominal_var);
    const resolved = interp.runtime_types.resolveVar(rt_var);
    try std.testing.expect(resolved.desc.content == .structure);
    switch (resolved.desc.content.structure) {
        .nominal_type => |nom| {
            try std.testing.expectEqual(name_nominal, nom.ident.ident_idx);
            const backing = interp.runtime_types.getNominalBackingVar(nom);
            const b_resolved = interp.runtime_types.resolveVar(backing);
            try std.testing.expect(b_resolved.desc.content == .structure);
            try std.testing.expect(b_resolved.desc.content.structure == .str);
        },
        else => return error.TestUnexpectedResult,
    }
}

// RED: translating a compile-time flex var should produce a runtime flex var
test "interpreter2: translateTypeVar for flex var" {
    const gpa = std.testing.allocator;
    var env = try can.ModuleEnv.init(gpa, "");
    defer env.deinit();

    var interp = try Interpreter2.init(gpa, &env);
    defer interp.deinit();

    const ct_flex = try env.types.freshFromContent(.{ .flex_var = null });
    const rt_var = try interp.translateTypeVar(&env, ct_flex);
    const resolved = interp.runtime_types.resolveVar(rt_var);
    try std.testing.expect(resolved.desc.content == .flex_var);
}

// RED: translating a compile-time rigid var should produce a runtime rigid var with same ident
test "interpreter2: translateTypeVar for rigid var" {
    const gpa = std.testing.allocator;
    var env = try can.ModuleEnv.init(gpa, "");
    defer env.deinit();

    var interp = try Interpreter2.init(gpa, &env);
    defer interp.deinit();

    const name_a = try env.common.idents.insert(gpa, @import("base").Ident.for_text("A"));
    const ct_rigid = try env.types.freshFromContent(.{ .rigid_var = name_a });
    const rt_var = try interp.translateTypeVar(&env, ct_rigid);
    const resolved = interp.runtime_types.resolveVar(rt_var);
    try std.testing.expect(resolved.desc.content == .rigid_var);
    try std.testing.expectEqual(name_a, resolved.desc.content.rigid_var);
}
