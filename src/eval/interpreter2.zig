//! Experimental next-gen interpreter implementing the type-carrying architecture.
//! This file will evolve behind its own tests until it replaces interpreter.zig.

const std = @import("std");
const types = @import("types");
const layout = @import("layout");
const can = @import("can");
const TypeScope = types.TypeScope;
const Content = types.Content;
const HashMap = std.hash_map.HashMap;
const unify = @import("check").unifier;
const problem_mod = @import("check").problem;
const snapshot_mod = @import("check").snapshot;
const stack = @import("stack.zig");
const StackValue = @import("StackValue.zig");
const builtins = @import("builtins");
const RocOps = builtins.host_abi.RocOps;
const RocStr = builtins.str.RocStr;
const Layout = layout.Layout;

pub const Interpreter2 = struct {
    const MAX_ARITY: usize = 8;
    const PolyKey = struct {
        func_id: u32,
        arity: u8,
        args: [MAX_ARITY]types.Var, // roots of runtime vars
    };

    const PolyEntry = struct {
        return_var: types.Var,
        return_layout_slot: u32, // biased: layout_idx + 1, or 0 if unset
    };

    const PolyKeyCtx = struct {
        pub fn hash(_: PolyKeyCtx, k: PolyKey) u64 {
            var h = std.hash.Wyhash.init(0);
            h.update(std.mem.asBytes(&k.func_id));
            h.update(std.mem.asBytes(&k.arity));
            var i: usize = 0;
            while (i < k.arity) : (i += 1) {
                const v_int: u32 = @intFromEnum(k.args[i]);
                h.update(std.mem.asBytes(&v_int));
            }
            return h.final();
        }
        pub fn eql(_: PolyKeyCtx, a: PolyKey, b: PolyKey) bool {
            if (a.func_id != b.func_id or a.arity != b.arity) return false;
            var i: usize = 0;
            while (i < a.arity) : (i += 1) {
                if (a.args[i] != b.args[i]) return false;
            }
            return true;
        }
    };
    const Binding = struct { pattern_idx: can.CIR.Pattern.Idx, value: StackValue };
    allocator: std.mem.Allocator,
    runtime_types: *types.store.Store,
    runtime_layout_store: layout.Store,
    // O(1) Var -> Layout slot cache (0 = unset, else layout_idx + 1)
    var_to_layout_slot: std.ArrayList(u32),
    // Empty scope used when converting runtime vars to layouts
    empty_scope: TypeScope,
    // Translation cache: (env_ptr, compile_var) -> runtime_var
    translate_cache: std.AutoHashMap(u64, types.Var),
    
    // Polymorphic instantiation cache

    poly_cache: HashMap(PolyKey, PolyEntry, PolyKeyCtx, 80),

    // Runtime unification context
    env: *can.ModuleEnv,
    problems: problem_mod.Store,
    snapshots: snapshot_mod.Store,
    unify_scratch: unify.Scratch,

    // Minimal eval support
    stack_memory: stack.Stack,
    bindings: std.ArrayList(Binding),

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
            .poly_cache = HashMap(PolyKey, PolyEntry, PolyKeyCtx, 80).init(allocator),
            .env = env,
            .problems = try problem_mod.Store.initCapacity(allocator, 64),
            .snapshots = try snapshot_mod.Store.initCapacity(allocator, 256),
            .unify_scratch = try unify.Scratch.init(allocator),
            .stack_memory = try stack.Stack.initCapacity(allocator, 4096),
            .bindings = try std.ArrayList(Binding).initCapacity(allocator, 8),
        };
        result.runtime_layout_store = try layout.Store.init(env, result.runtime_types);
        return result;
    }

    // Minimal evaluator for subset: string literals, lambdas without captures, and lambda calls
    pub fn evalMinimal(self: *Interpreter2, expr_idx: can.CIR.Expr.Idx, roc_ops: *RocOps) !StackValue {
        return try self.evalExprMinimal(expr_idx, roc_ops);
    }

    fn evalExprMinimal(self: *Interpreter2, expr_idx: can.CIR.Expr.Idx, roc_ops: *RocOps) !StackValue {
        const expr = self.env.store.getExpr(expr_idx);
        switch (expr) {
            .e_int => |int_lit| {
                // Use runtime type to choose layout
                const ct_var = can.ModuleEnv.varFrom(expr_idx);
                const rt_var = try self.translateTypeVar(self.env, ct_var);
                const layout_val = try self.getRuntimeLayout(rt_var);
                var value = try self.pushRaw(layout_val, 0);
                // Write integer as i128 respecting precision via StackValue
                value.is_initialized = false;
                value.setInt(int_lit.value.toI128());
                value.is_initialized = true;
                return value;
            },
            .e_binop => |binop| {
                if (binop.op == .add) {
                    const lhs = try self.evalExprMinimal(binop.lhs, roc_ops);
                    const rhs = try self.evalExprMinimal(binop.rhs, roc_ops);
                    if (!(lhs.layout.tag == .scalar and lhs.layout.data.scalar.tag == .int)) return error.TypeMismatch;
                    if (!(rhs.layout.tag == .scalar and rhs.layout.data.scalar.tag == .int)) return error.TypeMismatch;
                    const ct_var = can.ModuleEnv.varFrom(expr_idx);
                    const rt_var = try self.translateTypeVar(self.env, ct_var);
                    const result_layout = try self.getRuntimeLayout(rt_var);
                    var out = try self.pushRaw(result_layout, 0);
                    out.is_initialized = false;
                    const sum = lhs.asI128() + rhs.asI128();
                    out.setInt(sum);
                    out.is_initialized = true;
                    return out;
                } else if (binop.op == .@"and" or binop.op == .@"or") {
                    const lhs = try self.evalExprMinimal(binop.lhs, roc_ops);
                    const rhs = try self.evalExprMinimal(binop.rhs, roc_ops);
                    if (!(lhs.layout.tag == .scalar and lhs.layout.data.scalar.tag == .bool)) return error.TypeMismatch;
                    if (!(rhs.layout.tag == .scalar and rhs.layout.data.scalar.tag == .bool)) return error.TypeMismatch;
                    const lptr: *const u8 = @ptrCast(@alignCast(lhs.ptr.?));
                    const rptr: *const u8 = @ptrCast(@alignCast(rhs.ptr.?));
                    const res: u8 = if (binop.op == .@"and") (if (lptr.* != 0 and rptr.* != 0) 1 else 0) else (if (lptr.* != 0 or rptr.* != 0) 1 else 0);
                    const layout_val = Layout.boolType();
                    const out = try self.pushRaw(layout_val, 0);
                    const optr: *u8 = @ptrCast(@alignCast(out.ptr.?));
                    optr.* = res;
                    return out;
                }
                return error.NotImplemented;
            },
            .e_str => |str_expr| {
                const segments = self.env.store.sliceExpr(str_expr.span);
                if (segments.len == 0) {
                    // empty string
                    const value = try self.pushStr("");
                    return value;
                } else if (segments.len == 1) {
                    const seg_expr = self.env.store.getExpr(segments[0]);
                    if (seg_expr == .e_str_segment) {
                        const content = self.env.getString(seg_expr.e_str_segment.literal);
                        const value = try self.pushStr(content);
                        // Initialize RocStr in place
                        const roc_str: *RocStr = @ptrCast(@alignCast(value.ptr.?));
                        roc_str.* = RocStr.fromSlice(content, roc_ops);
                        return value;
                    }
                }
                return error.NotImplemented;
            },
            .e_str_segment => |seg| {
                const content = self.env.getString(seg.literal);
                const value = try self.pushStr(content);
                const roc_str: *RocStr = @ptrCast(@alignCast(value.ptr.?));
                roc_str.* = RocStr.fromSlice(content, roc_ops);
                return value;
            },
            .e_zero_argument_tag => |tag| {
                const name = self.env.getIdent(tag.name);
                // Handle Bool.True/Bool.False to scalar bool
                if (std.mem.eql(u8, name, "True") or std.mem.eql(u8, name, "False")) {
                    const layout_val = Layout.boolType();
                    const out = try self.pushRaw(layout_val, 0);
                    // write 1 for True, 0 for False
                    const bptr: *u8 = @ptrCast(@alignCast(out.ptr.?));
                    bptr.* = if (std.mem.eql(u8, name, "True")) 1 else 0;
                    return out;
                }
                return error.NotImplemented;
            },
            .e_nominal => |nom| {
                // Evaluate backing expression using minimal evaluator
                return try self.evalExprMinimal(nom.backing_expr, roc_ops);
            },
            .e_nominal_external => |nom| {
                return try self.evalExprMinimal(nom.backing_expr, roc_ops);
            },
            .e_tag => |tag| {
                // Treat True/False with zero args as booleans
                const name = self.env.getIdent(tag.name);
                const args = self.env.store.sliceExpr(tag.args);
                if (args.len == 0 and (std.mem.eql(u8, name, "True") or std.mem.eql(u8, name, "False"))) {
                    const layout_val = Layout.boolType();
                    const out = try self.pushRaw(layout_val, 0);
                    const b: *u8 = @ptrCast(@alignCast(out.ptr.?));
                    b.* = if (std.mem.eql(u8, name, "True")) 1 else 0;
                    return out;
                }
                return error.NotImplemented;
            },
            .e_lambda => |_| {
                // minimal: return a placeholder value that indicates lambda; actual call handled in e_call special-case
                // We don't construct a full closure; just return a zero-sized placeholder (empty record) for now
                // Instead, signal by returning a bool true as marker (not used). Better: return a zero-sized layout
                const layout_val = Layout.opaquePtr();
                return try self.pushRaw(layout_val, 0);
            },
            .e_call => |call| {
                const all = self.env.store.sliceExpr(call.args);
                if (all.len < 2) return error.TypeMismatch;
                const func_idx = all[0];
                const arg_idx = all[1];
                // Special-case: function is a lambda with one arg, body returns the arg; handle as identity
                const func_expr = self.env.store.getExpr(func_idx);
                if (func_expr != .e_lambda) return error.NotImplemented;
                const lambda = func_expr.e_lambda;
                // Evaluate argument
                const arg_val = try self.evalExprMinimal(arg_idx, roc_ops);
                // Bind the first param to the evaluated argument
                const params = self.env.store.slicePatterns(lambda.args);
                if (params.len != 1) return error.NotImplemented;
                try self.bindings.append(.{ .pattern_idx = params[0], .value = arg_val });
                defer _ = self.bindings.pop();
                // Evaluate body with binding
                return try self.evalExprMinimal(lambda.body, roc_ops);
            },
            .e_lookup_local => |lookup| {
                // Search bindings in reverse
                var i: usize = self.bindings.items.len;
                while (i > 0) {
                    i -= 1;
                    const b = self.bindings.items[i];
                    if (b.pattern_idx == lookup.pattern_idx) {
                        return try self.pushCopy(b.value);
                    }
                }
                return error.NotImplemented;
            },
            .e_unary_not => |un| {
                const v = try self.evalExprMinimal(un.expr, roc_ops);
                if (!(v.layout.tag == .scalar and v.layout.data.scalar.tag == .bool)) return error.TypeMismatch;
                const bptr: *u8 = @ptrCast(@alignCast(v.ptr.?));
                const val: u8 = if (bptr.* == 0) 1 else 0;
                const layout_val = Layout.boolType();
                const out = try self.pushRaw(layout_val, 0);
                const outptr: *u8 = @ptrCast(@alignCast(out.ptr.?));
                outptr.* = val;
                return out;
            },
            .e_if => |ifi| {
                // minimal: handle single branch if-then-else
                const branches = self.env.store.sliceIfBranches(ifi.branches);
                if (branches.len == 0) return try self.evalExprMinimal(ifi.final_else, roc_ops);
                const branch = self.env.store.getIfBranch(branches[0]);
                const cond_val = try self.evalExprMinimal(branch.cond, roc_ops);
                if (!(cond_val.layout.tag == .scalar and cond_val.layout.data.scalar.tag == .bool)) return error.TypeMismatch;
                const cptr: *const u8 = @ptrCast(@alignCast(cond_val.ptr.?));
                if (cptr.* != 0) {
                    return try self.evalExprMinimal(branch.body, roc_ops);
                } else {
                    return try self.evalExprMinimal(ifi.final_else, roc_ops);
                }
            },
            // no second e_binop case; handled above
            else => return error.NotImplemented,
        }
    }

    fn pushStr(self: *Interpreter2, content: []const u8) !StackValue {
        _ = content; // size computed below but content copied via RocStr
        const layout_val = Layout.str();
        const size: u32 = self.runtime_layout_store.layoutSize(layout_val);
        if (size == 0) {
            return StackValue{ .layout = layout_val, .ptr = null, .is_initialized = false };
        }
        const alignment = layout_val.alignment(self.runtime_layout_store.targetUsize());
        const ptr = try self.stack_memory.alloca(size, alignment);
        return StackValue{ .layout = layout_val, .ptr = ptr, .is_initialized = true };
    }

    fn pushRaw(self: *Interpreter2, layout_val: Layout, initial_size: usize) !StackValue {
        const size: u32 = if (initial_size == 0) self.runtime_layout_store.layoutSize(layout_val) else @intCast(initial_size);
        if (size == 0) {
            return StackValue{ .layout = layout_val, .ptr = null, .is_initialized = true };
        }
        const alignment = layout_val.alignment(self.runtime_layout_store.targetUsize());
        const ptr = try self.stack_memory.alloca(size, alignment);
        return StackValue{ .layout = layout_val, .ptr = ptr, .is_initialized = true };
    }

    fn pushCopy(self: *Interpreter2, src: StackValue) !StackValue {
        const size: u32 = if (src.layout.tag == .closure) src.getTotalSize(&self.runtime_layout_store) else self.runtime_layout_store.layoutSize(src.layout);
        const alignment = src.layout.alignment(self.runtime_layout_store.targetUsize());
        const ptr = if (size > 0) try self.stack_memory.alloca(size, alignment) else null;
        const dest = StackValue{ .layout = src.layout, .ptr = ptr, .is_initialized = true };
        if (size > 0 and src.ptr != null and ptr != null) {
            @memcpy(@as([*]u8, @ptrCast(ptr))[0..size], @as([*]const u8, @ptrCast(src.ptr.?))[0..size]);
        }
        return dest;
    }

    pub fn renderValueRoc(self: *Interpreter2, value: StackValue) ![]u8 {
        const gpa = self.allocator;
        if (value.layout.tag == .scalar) {
            switch (value.layout.data.scalar.tag) {
                .bool => {
                    const bptr: *const u8 = @ptrCast(@alignCast(value.ptr.?));
                    return if (bptr.* != 0) try std.fmt.allocPrint(gpa, "True", .{}) else try std.fmt.allocPrint(gpa, "False", .{});
                },
                .str => {
                    const rs: *const RocStr = @ptrCast(@alignCast(value.ptr.?));
                    const s = rs.asSlice();
                    var buf = std.ArrayList(u8).init(gpa);
                    errdefer buf.deinit();
                    try buf.append('"');
                    for (s) |ch| {
                        switch (ch) {
                            '\\' => { try buf.appendSlice("\\\\"); },
                            '"' => { try buf.appendSlice("\\\""); },
                            else => try buf.append(ch),
                        }
                    }
                    try buf.append('"');
                    return buf.toOwnedSlice();
                },
                .int => {
                    const i = value.asI128();
                    return try std.fmt.allocPrint(gpa, "{d}", .{i});
                },
                else => {},
            }
        }
        // Fallback
        return try std.fmt.allocPrint(gpa, "<unsupported>", .{});
    }
    pub fn deinit(self: *Interpreter2) void {
        self.empty_scope.deinit();
        self.translate_cache.deinit();
        self.poly_cache.deinit();
        self.var_to_layout_slot.deinit();
        self.runtime_layout_store.deinit();
        self.runtime_types.deinit();
        self.allocator.destroy(self.runtime_types);
        self.snapshots.deinit();
        self.problems.deinit(self.allocator);
        self.unify_scratch.deinit();
        self.stack_memory.deinit();
        self.bindings.deinit();
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
                    .int_precision => |p| try self.runtime_types.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = .{ .int = p } } } }),
                    .frac_precision => |p| try self.runtime_types.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = .{ .frac = p } } } }),
                    .num_unbound, .int_unbound => try self.runtime_types.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = .{ .int = types.Num.Int.Precision.default } } } }),
                    .frac_unbound => try self.runtime_types.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = .{ .frac = types.Num.Frac.Precision.default } } } }),
                    .num_poly, .int_poly => try self.runtime_types.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = .{ .int = types.Num.Int.Precision.default } } } }),
                    .frac_poly => try self.runtime_types.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = .{ .frac = types.Num.Frac.Precision.default } } } }),
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
                .fn_pure => |f| {
                    const ct_args = module.types.sliceVars(f.args);
                    var buf = try self.allocator.alloc(types.Var, ct_args.len);
                    defer self.allocator.free(buf);
                    for (ct_args, 0..) |ct_arg, i| {
                        buf[i] = try self.translateTypeVar(module, ct_arg);
                    }
                    const rt_ret = try self.translateTypeVar(module, f.ret);
                    const content = try self.runtime_types.mkFuncPure(buf, rt_ret);
                    return try self.runtime_types.register(.{ .content = content, .rank = types.Rank.top_level, .mark = types.Mark.none });
                },
                .fn_effectful => |f| {
                    const ct_args = module.types.sliceVars(f.args);
                    var buf = try self.allocator.alloc(types.Var, ct_args.len);
                    defer self.allocator.free(buf);
                    for (ct_args, 0..) |ct_arg, i| {
                        buf[i] = try self.translateTypeVar(module, ct_arg);
                    }
                    const rt_ret = try self.translateTypeVar(module, f.ret);
                    const content = try self.runtime_types.mkFuncEffectful(buf, rt_ret);
                    return try self.runtime_types.register(.{ .content = content, .rank = types.Rank.top_level, .mark = types.Mark.none });
                },
                .fn_unbound => |f| {
                    const ct_args = module.types.sliceVars(f.args);
                    var buf = try self.allocator.alloc(types.Var, ct_args.len);
                    defer self.allocator.free(buf);
                    for (ct_args, 0..) |ct_arg, i| {
                        buf[i] = try self.translateTypeVar(module, ct_arg);
                    }
                    const rt_ret = try self.translateTypeVar(module, f.ret);
                    const content = try self.runtime_types.mkFuncUnbound(buf, rt_ret);
                    return try self.runtime_types.register(.{ .content = content, .rank = types.Rank.top_level, .mark = types.Mark.none });
                },
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
    
    pub fn makePolyKey(self: *Interpreter2, func_id: u32, args: []const types.Var) PolyKey {
        var key = PolyKey{
            .func_id = func_id,
            .arity = @intCast(@min(args.len, MAX_ARITY)),
            .args = std.mem.zeroes([MAX_ARITY]types.Var),
        };
        var i: usize = 0;
        while (i < key.arity) : (i += 1) {
            const root = self.runtime_types.resolveVar(args[i]).var_;
            key.args[i] = root;
        }
        return key;
    }

    pub fn polyLookup(self: *Interpreter2, key: PolyKey) ?PolyEntry {
        return self.poly_cache.get(key);
    }

    pub fn polyInsert(self: *Interpreter2, key: PolyKey, entry: PolyEntry) !void {
        try self.poly_cache.put(key, entry);
    }

    /// Prepare a call: return cached instantiation entry if present; on miss, insert using return_var_hint if provided.
    pub fn prepareCall(self: *Interpreter2, func_id: u32, args: []const types.Var, return_var_hint: ?types.Var) !?PolyEntry {
        const key = self.makePolyKey(func_id, args);
        if (self.polyLookup(key)) |found| return found;

        if (return_var_hint) |ret| {
            // Ensure layout slot for return var
            _ = try self.getRuntimeLayout(ret);
            const root_idx: usize = @intFromEnum(self.runtime_types.resolveVar(ret).var_);
            try self.ensureVarLayoutCapacity(root_idx + 1);
            const slot = self.var_to_layout_slot.items[root_idx];
            const entry = PolyEntry{ .return_var = ret, .return_layout_slot = slot };
            try self.polyInsert(key, entry);
            return entry;
        }

        return null;
    }

    /// Prepare a call using a known runtime function type var.
    /// Builds and inserts a cache entry on miss using the function's declared return var.
    pub fn prepareCallWithFuncVar(self: *Interpreter2, func_id: u32, func_type_var: types.Var, args: []const types.Var) !PolyEntry {
        const key = self.makePolyKey(func_id, args);
        if (self.polyLookup(key)) |found| return found;

        const func_resolved = self.runtime_types.resolveVar(func_type_var);
        const ret_var: types.Var = switch (func_resolved.desc.content) {
            .structure => |flat| switch (flat) {
                .fn_pure => |f| f.ret,
                .fn_effectful => |f| f.ret,
                .fn_unbound => |f| f.ret,
                else => return error.TypeMismatch,
            },
            else => return error.TypeMismatch,
        };

        // Attempt simple runtime unification of parameters with arguments.
        const params: []types.Var = switch (func_resolved.desc.content) {
            .structure => |flat| switch (flat) {
                .fn_pure => |f| self.runtime_types.sliceVars(f.args),
                .fn_effectful => |f| self.runtime_types.sliceVars(f.args),
                .fn_unbound => |f| self.runtime_types.sliceVars(f.args),
                else => &[_]types.Var{},
            },
            else => &[_]types.Var{},
        };
        if (params.len != args.len) return error.TypeMismatch;

        var i: usize = 0;
        while (i < params.len) : (i += 1) {
            _ = try unify.unifyWithContext(
                self.env,
                self.runtime_types,
                &self.problems,
                &self.snapshots,
                &self.unify_scratch,
                &self.unify_scratch.occurs_scratch,
                params[i],
                args[i],
                false,
            );
        }
        // ret_var may now be constrained

        // Ensure layout slot for return var
        _ = try self.getRuntimeLayout(ret_var);
        const root_idx: usize = @intFromEnum(self.runtime_types.resolveVar(ret_var).var_);
        try self.ensureVarLayoutCapacity(root_idx + 1);
        const slot = self.var_to_layout_slot.items[root_idx];
        const entry = PolyEntry{ .return_var = ret_var, .return_layout_slot = slot };
        try self.polyInsert(key, entry);
        return entry;
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

// RED: poly cache miss then hit
test "interpreter2: poly cache insert and lookup" {
    const gpa = std.testing.allocator;
    var env = try can.ModuleEnv.init(gpa, "");
    defer env.deinit();

    var interp = try Interpreter2.init(gpa, &env);
    defer interp.deinit();

    const f_id: u32 = 12345;
    // Create runtime args: (Str, I64)
    const rt_str = try interp.runtime_types.freshFromContent(.{ .structure = .str });
    const rt_i64 = try interp.runtime_types.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = .{ .int = .i64 } } } });
    const args = [_]types.Var{ rt_str, rt_i64 };

    const key = interp.makePolyKey(f_id, &args);
    try std.testing.expect(interp.polyLookup(key) == null);

    // For testing, say return type is Str
    const ret_var = rt_str;
    // Precompute layout slot for return type
    _ = try interp.getRuntimeLayout(ret_var);
    const root_idx: usize = @intFromEnum(interp.runtime_types.resolveVar(ret_var).var_);
    try interp.ensureVarLayoutCapacity(root_idx + 1);
    const slot = interp.var_to_layout_slot.items[root_idx];
    try std.testing.expect(slot != 0);

    try interp.polyInsert(key, .{ .return_var = ret_var, .return_layout_slot = slot });
    const found = interp.polyLookup(key) orelse return error.TestUnexpectedResult;
    try std.testing.expectEqual(ret_var, found.return_var);
    try std.testing.expectEqual(slot, found.return_layout_slot);
}

// RED: prepareCall should miss without hint, then hit after inserting with hint
test "interpreter2: prepareCall miss then hit" {
    const gpa = std.testing.allocator;
    var env = try can.ModuleEnv.init(gpa, "");
    defer env.deinit();

    var interp = try Interpreter2.init(gpa, &env);
    defer interp.deinit();

    const func_id: u32 = 7777;
    const rt_str = try interp.runtime_types.freshFromContent(.{ .structure = .str });
    const rt_i64 = try interp.runtime_types.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = .{ .int = .i64 } } } });
    const args = [_]types.Var{ rt_str, rt_i64 };

    // miss without hint
    const miss = try interp.prepareCall(func_id, &args, null);
    try std.testing.expect(miss == null);

    // insert with hint
    const entry = (try interp.prepareCall(func_id, &args, rt_str)) orelse return error.TestUnexpectedResult;
    try std.testing.expectEqual(rt_str, entry.return_var);
    try std.testing.expect(entry.return_layout_slot != 0);

    // subsequent call should hit without hint
    const hit = (try interp.prepareCall(func_id, &args, null)) orelse return error.TestUnexpectedResult;
    try std.testing.expectEqual(rt_str, hit.return_var);
    try std.testing.expectEqual(entry.return_layout_slot, hit.return_layout_slot);
}

// RED: prepareCallWithFuncVar populates cache based on function type
test "interpreter2: prepareCallWithFuncVar populates cache" {
    const gpa = std.testing.allocator;
    var env = try can.ModuleEnv.init(gpa, "");
    defer env.deinit();

    var interp = try Interpreter2.init(gpa, &env);
    defer interp.deinit();

    const func_id: u32 = 9999;
    const rt_str = try interp.runtime_types.freshFromContent(.{ .structure = .str });
    const rt_i64 = try interp.runtime_types.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = .{ .int = .i64 } } } });
    const args = [_]types.Var{ rt_str, rt_i64 };

    // Build a runtime function type: (Str, I64) -> Str
    const func_content = try interp.runtime_types.mkFuncPure(&args, rt_str);
    const func_var = try interp.runtime_types.register(.{ .content = func_content, .rank = types.Rank.top_level, .mark = types.Mark.none });

    // Should populate cache
    const entry = try interp.prepareCallWithFuncVar(func_id, func_var, &args);
    try std.testing.expectEqual(rt_str, entry.return_var);
    try std.testing.expect(entry.return_layout_slot != 0);

    // Now a plain prepareCall without hint should hit
    const hit = (try interp.prepareCall(func_id, &args, null)) orelse return error.TestUnexpectedResult;
    try std.testing.expectEqual(rt_str, hit.return_var);
    try std.testing.expectEqual(entry.return_layout_slot, hit.return_layout_slot);
}

// RED: unification constrains return type for polymorphic (a -> a), when called with Str
test "interpreter2: unification constrains (a->a) with Str" {
    const gpa = std.testing.allocator;
    var env = try can.ModuleEnv.init(gpa, "");
    defer env.deinit();

    var interp = try Interpreter2.init(gpa, &env);
    defer interp.deinit();

    const func_id: u32 = 42;
    // runtime flex var 'a'
    const a = try interp.runtime_types.freshFromContent(.{ .flex_var = null });
    const func_content = try interp.runtime_types.mkFuncPure(&.{a}, a);
    const func_var = try interp.runtime_types.register(.{ .content = func_content, .rank = types.Rank.top_level, .mark = types.Mark.none });

    // Call with Str
    const rt_str = try interp.runtime_types.freshFromContent(.{ .structure = .str });
    const entry = try interp.prepareCallWithFuncVar(func_id, func_var, &.{rt_str});

    // After unification, return var should resolve to str
    const resolved_ret = interp.runtime_types.resolveVar(entry.return_var);
    try std.testing.expect(resolved_ret.desc.content == .structure);
    try std.testing.expect(resolved_ret.desc.content.structure == .str);
    try std.testing.expect(entry.return_layout_slot != 0);
}
