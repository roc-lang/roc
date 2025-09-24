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
const render_helpers = @import("render_helpers.zig");
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
    // Track active closures during calls (for capture lookup)
    active_closures: std.ArrayList(StackValue),

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
            .active_closures = try std.ArrayList(StackValue).initCapacity(allocator, 4),
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
            .e_block => |blk| {
                // New scope for bindings
                const original_len = self.bindings.items.len;
                defer self.bindings.items.len = original_len;

                const stmts = self.env.store.sliceStatements(blk.stmts);

                // First pass: add placeholders for all decl/var lambdas/closures (mutual recursion support)
                for (stmts) |stmt_idx| {
                    const stmt = self.env.store.getStatement(stmt_idx);
                    const Placeholder = struct {
                        fn exists(self_interp: *Interpreter2, start: usize, pattern_idx: can.CIR.Pattern.Idx) bool {
                            var i: usize = self_interp.bindings.items.len;
                            while (i > start) {
                                i -= 1;
                                if (self_interp.bindings.items[i].pattern_idx == pattern_idx) return true;
                            }
                            return false;
                        }
                        fn add(self_interp: *Interpreter2, patt_idx: can.CIR.Pattern.Idx, rhs_expr: can.CIR.Expr.Idx) !void {
                            const patt_ct_var = can.ModuleEnv.varFrom(patt_idx);
                            const patt_rt_var = try self_interp.translateTypeVar(self_interp.env, patt_ct_var);
                            const closure_layout = try self_interp.getRuntimeLayout(patt_rt_var);
                            if (closure_layout.tag != .closure) return; // only closures get placeholders
                            const lam_or = self_interp.env.store.getExpr(rhs_expr);
                            var body_idx: can.CIR.Expr.Idx = rhs_expr;
                            var params: can.CIR.Pattern.Span = .{ .span = .{ .start = 0, .len = 0 } };
                            if (lam_or == .e_lambda) {
                                body_idx = lam_or.e_lambda.body;
                                params = lam_or.e_lambda.args;
                            } else if (lam_or == .e_closure) {
                                const lam_expr = self_interp.env.store.getExpr(lam_or.e_closure.lambda_idx);
                                if (lam_expr == .e_lambda) {
                                    body_idx = lam_expr.e_lambda.body;
                                    params = lam_expr.e_lambda.args;
                                }
                            } else return;
                            const ph = try self_interp.pushRaw(closure_layout, 0);
                            if (ph.ptr) |ptr| {
                                const header: *layout.Closure = @ptrCast(@alignCast(ptr));
                                header.* = .{
                                    .body_idx = body_idx,
                                    .params = params,
                                    .captures_pattern_idx = @enumFromInt(@as(u32, 0)),
                                    .captures_layout_idx = closure_layout.data.closure.captures_layout_idx,
                                    .lambda_expr_idx = rhs_expr,
                                };
                            }
                            try self_interp.bindings.append(.{ .pattern_idx = patt_idx, .value = ph });
                        }
                    };
                    switch (stmt) {
                        .s_decl => |d| {
                            const patt = self.env.store.getPattern(d.pattern);
                            if (patt != .assign) continue;
                            const rhs = self.env.store.getExpr(d.expr);
                            if ((rhs == .e_lambda or rhs == .e_closure) and !Placeholder.exists(self, original_len, d.pattern)) {
                                try Placeholder.add(self, d.pattern, d.expr);
                            }
                        },
                        .s_var => |v| {
                            const patt = self.env.store.getPattern(v.pattern_idx);
                            if (patt != .assign) continue;
                            const rhs = self.env.store.getExpr(v.expr);
                            if ((rhs == .e_lambda or rhs == .e_closure) and !Placeholder.exists(self, original_len, v.pattern_idx)) {
                                try Placeholder.add(self, v.pattern_idx, v.expr);
                            }
                        },
                        else => {},
                    }
                }

                // Second pass: evaluate statements, updating placeholders
                for (stmts) |stmt_idx| {
                    const stmt = self.env.store.getStatement(stmt_idx);
                    switch (stmt) {
                        .s_decl => |d| {
                            const patt = self.env.store.getPattern(d.pattern);
                            if (patt != .assign) return error.NotImplemented;
                            const val = try self.evalExprMinimal(d.expr, roc_ops);
                            // Update existing binding if present
                            var i: usize = self.bindings.items.len;
                            var updated = false;
                            while (i > original_len) {
                                i -= 1;
                                if (self.bindings.items[i].pattern_idx == d.pattern) {
                                    self.bindings.items[i].value = val;
                                    updated = true;
                                    break;
                                }
                            }
                            if (!updated) try self.bindings.append(.{ .pattern_idx = d.pattern, .value = val });
                        },
                        .s_var => |v| {
                            const patt = self.env.store.getPattern(v.pattern_idx);
                            if (patt != .assign) return error.NotImplemented;
                            const val = try self.evalExprMinimal(v.expr, roc_ops);
                            var i: usize = self.bindings.items.len;
                            var updated = false;
                            while (i > original_len) {
                                i -= 1;
                                if (self.bindings.items[i].pattern_idx == v.pattern_idx) {
                                    self.bindings.items[i].value = val;
                                    updated = true;
                                    break;
                                }
                            }
                            if (!updated) try self.bindings.append(.{ .pattern_idx = v.pattern_idx, .value = val });
                        },
                        .s_reassign => |r| {
                            const patt = self.env.store.getPattern(r.pattern_idx);
                            if (patt != .assign) return error.NotImplemented;
                            const new_val = try self.evalExprMinimal(r.expr, roc_ops);
                            var j: usize = self.bindings.items.len;
                            while (j > original_len) {
                                j -= 1;
                                if (self.bindings.items[j].pattern_idx == r.pattern_idx) {
                                    self.bindings.items[j].value = new_val;
                                    break;
                                }
                            }
                        },
                        .s_expr => |sx| {
                            _ = try self.evalExprMinimal(sx.expr, roc_ops);
                        },
                        else => return error.NotImplemented,
                    }
                }

                return try self.evalExprMinimal(blk.final_expr, roc_ops);
            },
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
                } else if (binop.op == .sub) {
                    const lhs = try self.evalExprMinimal(binop.lhs, roc_ops);
                    const rhs = try self.evalExprMinimal(binop.rhs, roc_ops);
                    if (!(lhs.layout.tag == .scalar and lhs.layout.data.scalar.tag == .int)) return error.TypeMismatch;
                    if (!(rhs.layout.tag == .scalar and rhs.layout.data.scalar.tag == .int)) return error.TypeMismatch;
                    const ct_var = can.ModuleEnv.varFrom(expr_idx);
                    const rt_var = try self.translateTypeVar(self.env, ct_var);
                    const result_layout = try self.getRuntimeLayout(rt_var);
                    var out = try self.pushRaw(result_layout, 0);
                    out.is_initialized = false;
                    const diff = lhs.asI128() - rhs.asI128();
                    out.setInt(diff);
                    out.is_initialized = true;
                    return out;
                } else if (binop.op == .mul) {
                    const lhs = try self.evalExprMinimal(binop.lhs, roc_ops);
                    const rhs = try self.evalExprMinimal(binop.rhs, roc_ops);
                    if (!(lhs.layout.tag == .scalar and lhs.layout.data.scalar.tag == .int)) return error.TypeMismatch;
                    if (!(rhs.layout.tag == .scalar and rhs.layout.data.scalar.tag == .int)) return error.TypeMismatch;
                    const ct_var = can.ModuleEnv.varFrom(expr_idx);
                    const rt_var = try self.translateTypeVar(self.env, ct_var);
                    const result_layout = try self.getRuntimeLayout(rt_var);
                    var out = try self.pushRaw(result_layout, 0);
                    out.is_initialized = false;
                    const prod = lhs.asI128() * rhs.asI128();
                    out.setInt(prod);
                    out.is_initialized = true;
                    return out;
                } else if (binop.op == .eq) {
                    const lhs = try self.evalExprMinimal(binop.lhs, roc_ops);
                    const rhs = try self.evalExprMinimal(binop.rhs, roc_ops);
                    // Only int==int supported in minimal path
                    if (!(lhs.layout.tag == .scalar and lhs.layout.data.scalar.tag == .int)) return error.TypeMismatch;
                    if (!(rhs.layout.tag == .scalar and rhs.layout.data.scalar.tag == .int)) return error.TypeMismatch;
                    const result_layout = Layout.boolType();
                    var out = try self.pushRaw(result_layout, 0);
                    out.is_initialized = false;
                    const is_eq: u8 = if (lhs.asI128() == rhs.asI128()) 1 else 0;
                    // write byte bool
                    const p: *u8 = @ptrCast(@alignCast(out.ptr.?));
                    p.* = is_eq;
                    out.is_initialized = true;
                    return out;
                } else if (binop.op == .@"or") {
                    // Short-circuit OR: if lhs is true, return lhs; else return rhs
                    const lhs = try self.evalExprMinimal(binop.lhs, roc_ops);
                    if (!(lhs.layout.tag == .scalar and lhs.layout.data.scalar.tag == .bool)) return error.TypeMismatch;
                    const lhs_b: *const u8 = @ptrCast(@alignCast(lhs.ptr.?));
                    if (lhs_b.* != 0) {
                        // Return lhs (already true) without evaluating rhs
                        return lhs;
                    }
                    // Evaluate rhs and return it
                    const rhs = try self.evalExprMinimal(binop.rhs, roc_ops);
                    if (!(rhs.layout.tag == .scalar and rhs.layout.data.scalar.tag == .bool)) return error.TypeMismatch;
                    return rhs;
                } else if (binop.op == .@"and") {
                    // Short-circuit AND: if lhs is false, return lhs; else return rhs
                    const lhs = try self.evalExprMinimal(binop.lhs, roc_ops);
                    if (!(lhs.layout.tag == .scalar and lhs.layout.data.scalar.tag == .bool)) return error.TypeMismatch;
                    const lhs_b: *const u8 = @ptrCast(@alignCast(lhs.ptr.?));
                    if (lhs_b.* == 0) {
                        // Return lhs (already false) without evaluating rhs
                        return lhs;
                    }
                    // Evaluate rhs and return it
                    const rhs = try self.evalExprMinimal(binop.rhs, roc_ops);
                    if (!(rhs.layout.tag == .scalar and rhs.layout.data.scalar.tag == .bool)) return error.TypeMismatch;
                    return rhs;
                }
                return error.NotImplemented;
            },
            .e_if => |if_expr| {
                const branches = self.env.store.sliceIfBranches(if_expr.branches);
                // Evaluate branches in order; pick first true condition
                var i: usize = 0;
                while (i < branches.len) : (i += 1) {
                    const br = self.env.store.getIfBranch(branches[i]);
                    const cond_val = try self.evalExprMinimal(br.cond, roc_ops);
                    if (!(cond_val.layout.tag == .scalar and cond_val.layout.data.scalar.tag == .bool)) return error.TypeMismatch;
                    const cond_byte: *const u8 = @ptrCast(@alignCast(cond_val.ptr.?));
                    if (cond_byte.* != 0) {
                        return try self.evalExprMinimal(br.body, roc_ops);
                    }
                }
                // No condition matched; evaluate final else
                return try self.evalExprMinimal(if_expr.final_else, roc_ops);
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
            .e_tuple => |tup| {
                // Evaluate all elements first to drive runtime unification
                const elems = self.env.store.sliceExpr(tup.elems);
                var values = try std.ArrayList(StackValue).initCapacity(self.allocator, elems.len);
                defer values.deinit();
                for (elems) |e_idx| {
                    const v = try self.evalExprMinimal(e_idx, roc_ops);
                    try values.append(v);
                }

                // Compute tuple layout from concrete element value layouts
                var elem_layouts = try self.allocator.alloc(Layout, values.items.len);
                defer self.allocator.free(elem_layouts);
                for (values.items, 0..) |v, ii| elem_layouts[ii] = v.layout;
                const tuple_layout_idx = try self.runtime_layout_store.putTuple(elem_layouts);
                const tuple_layout = self.runtime_layout_store.getLayout(tuple_layout_idx);
                var dest = try self.pushRaw(tuple_layout, 0);
                var accessor = try dest.asTuple(&self.runtime_layout_store);

                if (values.items.len != accessor.getElementCount()) return error.TypeMismatch;
                var i: usize = 0;
                while (i < values.items.len) : (i += 1) {
                    const sorted_idx = accessor.findElementIndexByOriginal(i) orelse return error.TypeMismatch;
                    try accessor.setElement(sorted_idx, values.items[i], roc_ops);
                }
                return dest;
            },
            .e_record => |rec| {
                // Allocate record and fill fields
                const ct_var = can.ModuleEnv.varFrom(expr_idx);
                const rt_var = try self.translateTypeVar(self.env, ct_var);
                const rec_layout = try self.getRuntimeLayout(rt_var);
                var dest = try self.pushRaw(rec_layout, 0);
                var accessor = try dest.asRecord(&self.runtime_layout_store);
                const fields = self.env.store.sliceRecordFields(rec.fields);
                for (fields) |f_idx| {
                    const f = self.env.store.getRecordField(f_idx);
                    const name_text = self.env.getIdent(f.name);
                    const idx_opt = accessor.findFieldIndex(self.env, name_text);
                    if (idx_opt) |findex| {
                        const val = try self.evalExprMinimal(f.value, roc_ops);
                        try accessor.setFieldByIndex(findex, val, roc_ops);
                    } else return error.TypeMismatch;
                }
                return dest;
            },
            // no zero-argument tag handling in minimal evaluator
            .e_nominal => |nom| {
                // Evaluate backing expression using minimal evaluator
                return try self.evalExprMinimal(nom.backing_expr, roc_ops);
            },
            .e_nominal_external => |nom| {
                return try self.evalExprMinimal(nom.backing_expr, roc_ops);
            },
            .e_zero_argument_tag => |zero| {
                // Construct a tag union value with no payload
                // Determine discriminant index by consulting the runtime tag union type
                const ct_var = can.ModuleEnv.varFrom(expr_idx);
                const rt_var = try self.translateTypeVar(self.env, ct_var);
                const resolved = self.runtime_types.resolveVar(rt_var);
                if (resolved.desc.content != .structure or resolved.desc.content.structure != .tag_union) return error.NotImplemented;
                const tu = resolved.desc.content.structure.tag_union;
                const tags = self.runtime_types.getTagsSlice(tu.tags);
                // Find index by name
                var tag_index: usize = 0;
                var found = false;
                const name_text = self.env.getIdent(zero.name);
                var i: usize = 0;
                while (i < tags.len) : (i += 1) {
                    if (std.mem.eql(u8, self.env.getIdent(tags.items(.name)[i]), name_text)) {
                        tag_index = i;
                        found = true;
                        break;
                    }
                }
                if (!found) return error.NotImplemented;
                const layout_val = try self.getRuntimeLayout(rt_var);
                // If layout is scalar (bool/uint), write discriminant directly
                if (layout_val.tag == .scalar) {
                    var out = try self.pushRaw(layout_val, 0);
                    if (layout_val.data.scalar.tag == .bool) {
                        const p: *u8 = @ptrCast(@alignCast(out.ptr.?));
                        p.* = if (tag_index != 0) 1 else 0;
                        return out;
                    } else if (layout_val.data.scalar.tag == .int) {
                        out.is_initialized = false;
                        out.setInt(@intCast(tag_index));
                        out.is_initialized = true;
                        return out;
                    }
                    return error.NotImplemented;
                } else if (layout_val.tag == .record) {
                    // Record { tag: Discriminant, payload: ZST }
                    var dest = try self.pushRaw(layout_val, 0);
                    var acc = try dest.asRecord(&self.runtime_layout_store);
                    const tag_idx = acc.findFieldIndex(self.env, "tag") orelse return error.NotImplemented;
                    const tag_field = try acc.getFieldByIndex(tag_idx);
                    // write tag as int/byte
                    if (tag_field.layout.tag == .scalar and tag_field.layout.data.scalar.tag == .int) {
                        var tmp = tag_field;
                        tmp.is_initialized = false;
                        tmp.setInt(@intCast(tag_index));
                    } else if (tag_field.layout.tag == .scalar and tag_field.layout.data.scalar.tag == .bool) {
                        const p: *u8 = @ptrCast(@alignCast(tag_field.ptr.?));
                        p.* = if (tag_index != 0) 1 else 0;
                    } else return error.NotImplemented;
                    return dest;
                }
                return error.NotImplemented;
            },
            .e_tag => |tag| {
                // Construct a tag union value with payloads
                const ct_var = can.ModuleEnv.varFrom(expr_idx);
                const rt_var = try self.translateTypeVar(self.env, ct_var);
                const resolved = self.runtime_types.resolveVar(rt_var);
                if (resolved.desc.content != .structure or resolved.desc.content.structure != .tag_union) return error.NotImplemented;
                const tu = resolved.desc.content.structure.tag_union;
                const tags = self.runtime_types.getTagsSlice(tu.tags);
                // Find index by name
                const name_text = self.env.getIdent(tag.name);
                var tag_index: usize = 0;
                var found = false;
                var i: usize = 0;
                while (i < tags.len) : (i += 1) {
                    if (std.mem.eql(u8, self.env.getIdent(tags.items(.name)[i]), name_text)) {
                        tag_index = i;
                        found = true;
                        break;
                    }
                }
                if (!found) return error.NotImplemented;

                const layout_val = try self.getRuntimeLayout(rt_var);
                if (layout_val.tag == .scalar) {
                    // No payload union
                    var out = try self.pushRaw(layout_val, 0);
                    if (layout_val.data.scalar.tag == .bool) {
                        const p: *u8 = @ptrCast(@alignCast(out.ptr.?));
                        p.* = if (tag_index != 0) 1 else 0;
                        return out;
                    } else if (layout_val.data.scalar.tag == .int) {
                        out.is_initialized = false;
                        out.setInt(@intCast(tag_index));
                        out.is_initialized = true;
                        return out;
                    }
                    return error.NotImplemented;
                } else if (layout_val.tag == .record) {
                    // Has payload: record { tag, payload }
                    var dest = try self.pushRaw(layout_val, 0);
                    var acc = try dest.asRecord(&self.runtime_layout_store);
                    const tag_field_idx = acc.findFieldIndex(self.env, "tag") orelse return error.NotImplemented;
                    const payload_field_idx = acc.findFieldIndex(self.env, "payload") orelse return error.NotImplemented;
                    // write tag discriminant
                    const tag_field = try acc.getFieldByIndex(tag_field_idx);
                    if (tag_field.layout.tag == .scalar and tag_field.layout.data.scalar.tag == .int) {
                        var tmp = tag_field;
                        tmp.is_initialized = false;
                        tmp.setInt(@intCast(tag_index));
                    } else if (tag_field.layout.tag == .scalar and tag_field.layout.data.scalar.tag == .bool) {
                        const p: *u8 = @ptrCast(@alignCast(tag_field.ptr.?));
                        p.* = if (tag_index != 0) 1 else 0;
                    } else return error.NotImplemented;

                    // payload
                    const args = self.env.store.sliceExpr(tag.args);
                    if (args.len == 0) {
                        // nothing to write
                        return dest;
                    } else if (args.len == 1) {
                        const arg_val = try self.evalExprMinimal(args[0], roc_ops);
                        try acc.setFieldByIndex(payload_field_idx, arg_val, roc_ops);
                        return dest;
                    } else {
                        // Multiple args -> tuple payload
                        var payload_field = try acc.getFieldByIndex(payload_field_idx);
                        if (payload_field.layout.tag != .tuple) return error.NotImplemented;
                        var tup_acc = try payload_field.asTuple(&self.runtime_layout_store);
                        var j: usize = 0;
                        while (j < args.len) : (j += 1) {
                            const ev = try self.evalExprMinimal(args[j], roc_ops);
                            const sorted_idx = tup_acc.findElementIndexByOriginal(j) orelse return error.TypeMismatch;
                            try tup_acc.setElement(sorted_idx, ev, roc_ops);
                        }
                        return dest;
                    }
                }
                return error.NotImplemented;
            },
            .e_match => |m| {
                // Evaluate scrutinee once
                const scrutinee = try self.evalExprMinimal(m.cond, roc_ops);
                // Iterate branches and find first matching pattern set
                const branches = self.env.store.matchBranchSlice(m.branches);
                for (branches) |br_idx| {
                    const br = self.env.store.getMatchBranch(br_idx);
                    // Guard not supported in minimal eval
                    if (br.guard != null) return error.NotImplemented;
                    const patterns = self.env.store.sliceMatchBranchPatterns(br.patterns);
                    var temp_binds = try std.ArrayList(Binding).initCapacity(self.allocator, 4);
                    defer temp_binds.deinit();

                    var any_matched = false;
                    // OR patterns in a branch; succeed if any match
                    for (patterns) |bp_idx| {
                        temp_binds.items.len = 0; // reset temporary binds for each OR alternative
                        if (try self.patternMatchesBind(self.env.store.getMatchBranchPattern(bp_idx).pattern, scrutinee, &temp_binds)) {
                            any_matched = true;
                            break;
                        }
                    }

                    if (any_matched) {
                        // Apply temp binds
                        const start_len = self.bindings.items.len;
                        try self.bindings.appendSlice(temp_binds.items);
                        defer self.bindings.items.len = start_len;
                        return try self.evalExprMinimal(br.value, roc_ops);
                    }
                }
                // Non-exhaustive or unsupported
                return error.NotImplemented;
            },
            // no tag handling in minimal evaluator
            .e_lambda => |lam| {
                // Build a closure value with empty captures using the runtime layout for the lambda's type
                const ct_var = can.ModuleEnv.varFrom(expr_idx);
                const rt_var = try self.translateTypeVar(self.env, ct_var);
                const closure_layout = try self.getRuntimeLayout(rt_var);
                // Expect a closure layout from type-to-layout translation
                if (closure_layout.tag != .closure) return error.NotImplemented;
                const value = try self.pushRaw(closure_layout, 0);
                // Initialize the closure header
                if (value.ptr) |ptr| {
                    const header: *layout.Closure = @ptrCast(@alignCast(ptr));
                    header.* = .{
                        .body_idx = lam.body,
                        .params = lam.args,
                        .captures_pattern_idx = @enumFromInt(@as(u32, 0)), // no captures in minimal path
                        .captures_layout_idx = closure_layout.data.closure.captures_layout_idx,
                        .lambda_expr_idx = expr_idx,
                    };
                }
                return value;
            },
            .e_closure => |clos| {
                // Build a closure value with concrete captures. The closure references a lambda.
                const lam_expr = self.env.store.getExpr(clos.lambda_idx);
                if (lam_expr != .e_lambda) return error.NotImplemented;
                const lam = lam_expr.e_lambda;

                // Collect capture layouts and names from current bindings
                const caps = self.env.store.sliceCaptures(clos.captures);
                var field_layouts = try self.allocator.alloc(Layout, caps.len);
                defer self.allocator.free(field_layouts);
                var field_names = try self.allocator.alloc(@import("base").Ident.Idx, caps.len);
                defer self.allocator.free(field_names);

                // Helper: resolve a capture value (from local bindings or active closure captures)
                const resolveCapture = struct {
                    fn go(self_interp: *Interpreter2, cap: can.CIR.Expr.Capture) ?StackValue {
                        // First try local bindings by pattern idx
                        var i: usize = self_interp.bindings.items.len;
                        while (i > 0) {
                            i -= 1;
                            const b = self_interp.bindings.items[i];
                            if (b.pattern_idx == cap.pattern_idx) return b.value;
                        }
                        // Next try active closure captures (top-most only) by name
                        if (self_interp.active_closures.items.len > 0) {
                            const top = self_interp.active_closures.items[self_interp.active_closures.items.len - 1];
                            if (top.layout.tag == .closure and top.ptr != null) {
                                const captures_layout = self_interp.runtime_layout_store.getLayout(top.layout.data.closure.captures_layout_idx);
                                const header_sz = @sizeOf(layout.Closure);
                                const cap_align = captures_layout.alignment(self_interp.runtime_layout_store.targetUsize());
                                const aligned_off = std.mem.alignForward(usize, header_sz, @intCast(cap_align.toByteUnits()));
                                const base: [*]u8 = @ptrCast(@alignCast(top.ptr.?));
                                const rec_ptr: *anyopaque = @ptrCast(base + aligned_off);
                                const rec_val = StackValue{ .layout = captures_layout, .ptr = rec_ptr, .is_initialized = true };
                                var accessor = self_interp.runtime_layout_store; // just for type
                                _ = &accessor;
                                var rec_acc = (try rec_val.asRecord(&self_interp.runtime_layout_store));
                                const name_text = self_interp.env.getIdent(cap.name);
                                if (rec_acc.findFieldIndex(self_interp.env, name_text)) |fidx| {
                                    return rec_acc.getFieldByIndex(fidx) catch null;
                                }
                            }
                        }
                        return null;
                    }
                }.go;

                for (caps, 0..) |cap_idx, i| {
                    const cap = self.env.store.getCapture(cap_idx);
                    field_names[i] = cap.name;
                    const captured_val = resolveCapture(self, cap) orelse return error.NotImplemented;
                    field_layouts[i] = captured_val.layout;
                }

                const captures_layout_idx = try self.runtime_layout_store.putRecord(field_layouts, field_names);
                const captures_layout = self.runtime_layout_store.getLayout(captures_layout_idx);
                const closure_layout = Layout.closure(captures_layout_idx);
                const value = try self.pushRaw(closure_layout, 0);

                // Initialize header
                if (value.ptr) |ptr| {
                    const header: *layout.Closure = @ptrCast(@alignCast(ptr));
                    header.* = .{
                        .body_idx = lam.body,
                        .params = lam.args,
                        .captures_pattern_idx = @enumFromInt(@as(u32, 0)), // not used in minimal path
                        .captures_layout_idx = captures_layout_idx,
                        .lambda_expr_idx = clos.lambda_idx,
                    };
                    // Copy captures into record area following header (aligned)
                    const header_size = @sizeOf(layout.Closure);
                    const cap_align = captures_layout.alignment(self.runtime_layout_store.targetUsize());
                    const aligned_off = std.mem.alignForward(usize, header_size, @intCast(cap_align.toByteUnits()));
                    const base: [*]u8 = @ptrCast(@alignCast(ptr));
                    const rec_ptr: *anyopaque = @ptrCast(base + aligned_off);
                    const rec_val = StackValue{ .layout = captures_layout, .ptr = rec_ptr, .is_initialized = true };
                    var accessor = try rec_val.asRecord(&self.runtime_layout_store);
                    for (caps) |cap_idx2| {
                        const cap2 = self.env.store.getCapture(cap_idx2);
                        const cap_val2 = resolveCapture(self, cap2) orelse return error.NotImplemented;
                        const idx_opt = accessor.findFieldIndex(self.env, self.env.getIdent(cap2.name)) orelse return error.NotImplemented;
                        try accessor.setFieldByIndex(idx_opt, cap_val2, roc_ops);
                    }
                }
                return value;
            },
            .e_call => |call| {
                const all = self.env.store.sliceExpr(call.args);
                if (all.len < 2) return error.TypeMismatch;
                const func_idx = all[0];
                const arg_idx = all[1];
                // Runtime unification for call: constrain return type from arg types
                const func_ct_var = can.ModuleEnv.varFrom(func_idx);
                const func_rt_var = try self.translateTypeVar(self.env, func_ct_var);
                const arg_ct_var = can.ModuleEnv.varFrom(arg_idx);
                const arg_rt_var = try self.translateTypeVar(self.env, arg_ct_var);
                const poly_entry = try self.prepareCallWithFuncVar(@intCast(@intFromEnum(func_idx)), func_rt_var, &.{ arg_rt_var });
                // Unify this call expression's return var with the function's constrained return var
                const call_ret_ct_var = can.ModuleEnv.varFrom(expr_idx);
                const call_ret_rt_var = try self.translateTypeVar(self.env, call_ret_ct_var);
                _ = try unify.unifyWithContext(
                    self.env,
                    self.runtime_types,
                    &self.problems,
                    &self.snapshots,
                    &self.unify_scratch,
                    &self.unify_scratch.occurs_scratch,
                    call_ret_rt_var,
                    poly_entry.return_var,
                    false,
                );

                const func_val = try self.evalExprMinimal(func_idx, roc_ops);
                // Support calling closures produced by evaluating expressions (including nested calls)
                if (func_val.layout.tag == .closure) {
                    const header: *const layout.Closure = @ptrCast(@alignCast(func_val.ptr.?));
                    // Evaluate argument and bind to first parameter
                    const arg_val = try self.evalExprMinimal(arg_idx, roc_ops);
                    const params = self.env.store.slicePatterns(header.params);
                    if (params.len != 1) return error.NotImplemented;
                    // Provide closure context for capture lookup during body eval
                    try self.active_closures.append(func_val);
                    defer _ = self.active_closures.pop();
                    try self.bindings.append(.{ .pattern_idx = params[0], .value = arg_val });
                    defer _ = self.bindings.pop();
                    return try self.evalExprMinimal(header.body_idx, roc_ops);
                }

                // Fallback: direct lambda expression (legacy minimal path)
                const func_expr = self.env.store.getExpr(func_idx);
                if (func_expr == .e_lambda) {
                    const lambda = func_expr.e_lambda;
                    const arg_val = try self.evalExprMinimal(arg_idx, roc_ops);
                    const params = self.env.store.slicePatterns(lambda.args);
                    if (params.len != 1) return error.NotImplemented;
                    try self.bindings.append(.{ .pattern_idx = params[0], .value = arg_val });
                    defer _ = self.bindings.pop();
                    return try self.evalExprMinimal(lambda.body, roc_ops);
                }

                return error.NotImplemented;
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
                // If not found, try active closure captures by variable name
                if (self.active_closures.items.len > 0) {
                    const pat = self.env.store.getPattern(lookup.pattern_idx);
                    if (pat == .assign) {
                        const var_name = self.env.getIdent(pat.assign.ident);
                        const clos_val = self.active_closures.items[self.active_closures.items.len - 1];
                        if (clos_val.layout.tag == .closure and clos_val.ptr != null) {
                            const captures_layout = self.runtime_layout_store.getLayout(clos_val.layout.data.closure.captures_layout_idx);
                            const header_sz = @sizeOf(layout.Closure);
                            const cap_align = captures_layout.alignment(self.runtime_layout_store.targetUsize());
                            const aligned_off = std.mem.alignForward(usize, header_sz, @intCast(cap_align.toByteUnits()));
                            const base: [*]u8 = @ptrCast(@alignCast(clos_val.ptr.?));
                            const rec_ptr: *anyopaque = @ptrCast(base + aligned_off);
                            const rec_val = StackValue{ .layout = captures_layout, .ptr = rec_ptr, .is_initialized = true };
                            var accessor = try rec_val.asRecord(&self.runtime_layout_store);
                            if (accessor.findFieldIndex(self.env, var_name)) |fidx| {
                                const field_val = try accessor.getFieldByIndex(fidx);
                                return try self.pushCopy(field_val);
                            }
                        }
                    }
                }
                return error.NotImplemented;
            },
            // no boolean unary not in minimal evaluator
            // no if handling in minimal evaluator
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
        // Tag unions pretty rendering is available via renderValueRocWithType
        if (value.layout.tag == .scalar) {
            switch (value.layout.data.scalar.tag) {
                // no boolean rendering in minimal evaluator yet
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
        if (value.layout.tag == .tuple) {
            var out = std.ArrayList(u8).init(gpa);
            errdefer out.deinit();
            try out.append('(');
            var acc = try value.asTuple(&self.runtime_layout_store);
            const count = acc.getElementCount();
            var i: usize = 0;
            while (i < count) : (i += 1) {
                const elem = try acc.getElement(i);
                const rendered = try self.renderValueRoc(elem);
                defer gpa.free(rendered);
                try out.appendSlice(rendered);
                if (i + 1 < count) try out.appendSlice(", ");
            }
            try out.append(')');
            return out.toOwnedSlice();
        }
        if (value.layout.tag == .record) {
            var out = std.ArrayList(u8).init(gpa);
            errdefer out.deinit();
            try out.appendSlice("{ ");
            const rec_data = self.runtime_layout_store.getRecordData(value.layout.data.record.idx);
            const fields = self.runtime_layout_store.record_fields.sliceRange(rec_data.getFields());
            var i: usize = 0;
            while (i < fields.len) : (i += 1) {
                const fld = fields.get(i);
                const name_text = self.env.getIdent(fld.name);
                try out.appendSlice(name_text);
                try out.appendSlice(": ");
                // compute field offset
                const offset = self.runtime_layout_store.getRecordFieldOffset(value.layout.data.record.idx, @intCast(i));
                const field_layout = self.runtime_layout_store.getLayout(fld.layout);
                const base_ptr: [*]u8 = @ptrCast(@alignCast(value.ptr.?));
                const field_ptr: *anyopaque = @ptrCast(base_ptr + offset);
                const field_val = StackValue{ .layout = field_layout, .ptr = field_ptr, .is_initialized = true };
                const rendered = try self.renderValueRoc(field_val);
                defer gpa.free(rendered);
                try out.appendSlice(rendered);
                if (i + 1 < fields.len) try out.appendSlice(", ");
            }
            try out.appendSlice(" }");
            return out.toOwnedSlice();
        }
        // Fallback
        return try std.fmt.allocPrint(gpa, "<unsupported>", .{});
    }

    // removed duplicate

    // Helper for REPL and tests: render a value given its runtime type var
    pub fn renderValueRocWithType(self: *Interpreter2, value: StackValue, rt_var: types.Var) ![]u8 {
        const gpa = self.allocator;
        var resolved = self.runtime_types.resolveVar(rt_var);
        // Unwrap aliases and nominals to get to underlying structure
        unwrap: while (true) {
            switch (resolved.desc.content) {
                .alias => |al| {
                    const backing = self.runtime_types.getAliasBackingVar(al);
                    resolved = self.runtime_types.resolveVar(backing);
                },
                .structure => |st| switch (st) {
                    .nominal_type => |nt| {
                        const backing = self.runtime_types.getNominalBackingVar(nt);
                        resolved = self.runtime_types.resolveVar(backing);
                    },
                    else => break :unwrap,
                },
                else => break :unwrap,
            }
        }
        if (resolved.desc.content == .structure) {
            switch (resolved.desc.content.structure) {
                .tag_union => |tu| {
                    const tags = self.runtime_types.getTagsSlice(tu.tags);
                    var tag_index: usize = 0;
                    var have_tag = false;
                    if (value.layout.tag == .scalar) {
                        if (value.layout.data.scalar.tag == .bool) {
                            const b: *const u8 = @ptrCast(@alignCast(value.ptr.?));
                            tag_index = if (b.* != 0) 1 else 0;
                            have_tag = true;
                        } else if (value.layout.data.scalar.tag == .int) {
                            tag_index = @intCast(value.asI128());
                            have_tag = true;
                        }
                    } else if (value.layout.tag == .record) {
                        var acc = try value.asRecord(&self.runtime_layout_store);
                        if (acc.findFieldIndex(self.env, "tag")) |idx| {
                            const tag_field = try acc.getFieldByIndex(idx);
                            if (tag_field.layout.tag == .scalar and tag_field.layout.data.scalar.tag == .int) {
                                const tmp_sv = StackValue{ .layout = tag_field.layout, .ptr = tag_field.ptr, .is_initialized = true };
                                tag_index = @intCast(tmp_sv.asI128());
                                have_tag = true;
                            } else if (tag_field.layout.tag == .scalar and tag_field.layout.data.scalar.tag == .bool) {
                                const b: *const u8 = @ptrCast(@alignCast(tag_field.ptr.?));
                                tag_index = if (b.* != 0) 1 else 0;
                                have_tag = true;
                            }
                        }
                        if (have_tag and tag_index < tags.len) {
                            const tag_name = self.env.getIdent(tags.items(.name)[tag_index]);
                            var out = std.ArrayList(u8).init(gpa);
                            errdefer out.deinit();
                            try out.appendSlice(tag_name);
                            if (acc.findFieldIndex(self.env, "payload")) |pidx| {
                                const payload = try acc.getFieldByIndex(pidx);
                                const psize = self.runtime_layout_store.layoutSize(payload.layout);
                                if (psize > 0) {
                                    try out.append('(');
                                    if (payload.layout.tag == .tuple) {
                                        var tup = try payload.asTuple(&self.runtime_layout_store);
                                        const count = tup.getElementCount();
                                        var k: usize = 0;
                                        while (k < count) : (k += 1) {
                                            const elem = try tup.getElement(k);
                                            const r = try self.renderValueRoc(elem);
                                            defer gpa.free(r);
                                            try out.appendSlice(r);
                                            if (k + 1 < count) try out.appendSlice(", ");
                                        }
                                    } else {
                                        const rendered = try self.renderValueRoc(payload);
                                        defer gpa.free(rendered);
                                        try out.appendSlice(rendered);
                                    }
                                    try out.append(')');
                                }
                            }
                            return out.toOwnedSlice();
                        }
                    }
                },
                .record => |rec| {
                    var out = std.ArrayList(u8).init(gpa);
                    errdefer out.deinit();
                    try out.appendSlice("{ ");
                    var acc = try value.asRecord(&self.runtime_layout_store);
                    const fields = self.runtime_types.getRecordFieldsSlice(rec.fields);
                    var i: usize = 0;
                    while (i < fields.len) : (i += 1) {
                        const f = fields.get(i);
                        const name_text = self.env.getIdent(f.name);
                        try out.appendSlice(name_text);
                        try out.appendSlice(": ");
                        if (acc.findFieldIndex(self.env, name_text)) |idx| {
                            const field_val = try acc.getFieldByIndex(idx);
                            const rendered = try self.renderValueRoc(field_val);
                            defer gpa.free(rendered);
                            try out.appendSlice(rendered);
                        } else {
                            try out.appendSlice("<missing>");
                        }
                        if (i + 1 < fields.len) try out.appendSlice(", ");
                    }
                    try out.appendSlice(" }");
                    return out.toOwnedSlice();
                },
                else => {},
            }
        }
        return try self.renderValueRoc(value);
    }

    fn patternMatchesBind(self: *Interpreter2, pattern_idx: can.CIR.Pattern.Idx, value: StackValue, out_binds: *std.ArrayList(Binding)) !bool {
        const pat = self.env.store.getPattern(pattern_idx);
        switch (pat) {
            .assign => |_| {
                // Bind entire value to this pattern
                const copied = try self.pushCopy(value);
                try out_binds.append(.{ .pattern_idx = pattern_idx, .value = copied });
                return true;
            },
            .underscore => return true,
            .int_literal => |il| {
                if (!(value.layout.tag == .scalar and value.layout.data.scalar.tag == .int)) return false;
                const lit = il.value.toI128();
                return value.asI128() == lit;
            },
            .str_literal => |sl| {
                if (!(value.layout.tag == .scalar and value.layout.data.scalar.tag == .str)) return false;
                const lit = self.env.getString(sl.literal);
                const rs: *const RocStr = @ptrCast(@alignCast(value.ptr.?));
                return std.mem.eql(u8, rs.asSlice(), lit);
            },
            .nominal => |n| {
                return try self.patternMatchesBind(n.backing_pattern, value, out_binds);
            },
            .nominal_external => |n| {
                return try self.patternMatchesBind(n.backing_pattern, value, out_binds);
            },
            else => return false,
        }
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
        self.active_closures.deinit();
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
                .tag_union => |tu| {
                    const ct_tags = module.types.getTagsSlice(tu.tags);
                    var rt_tags = try self.allocator.alloc(@import("types").Tag, ct_tags.len);
                    defer self.allocator.free(rt_tags);
                    for (ct_tags.items(.name), ct_tags.items(.args), 0..) |name, args_range, i| {
                        const ct_args = module.types.sliceVars(args_range);
                        var arg_buf = try self.allocator.alloc(types.Var, ct_args.len);
                        defer self.allocator.free(arg_buf);
                        for (ct_args, 0..) |ct_arg, j| {
                            arg_buf[j] = try self.translateTypeVar(module, ct_arg);
                        }
                        const rt_args_range = try self.runtime_types.appendVars(arg_buf);
                        rt_tags[i] = .{ .name = name, .args = rt_args_range };
                    }
                    const rt_ext = try self.translateTypeVar(module, tu.ext);
                    const content = try self.runtime_types.mkTagUnion(rt_tags, rt_ext);
                    return try self.runtime_types.register(.{ .content = content, .rank = types.Rank.top_level, .mark = types.Mark.none });
                },
                .empty_tag_union => {
                    return try self.runtime_types.freshFromContent(.{ .structure = .empty_tag_union });
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
                .record_unbound => |fields_range| {
                    const ct_fields = module.types.getRecordFieldsSlice(fields_range);
                    var tmp = try self.allocator.alloc(types.RecordField, ct_fields.len);
                    defer self.allocator.free(tmp);
                    var i: usize = 0;
                    while (i < ct_fields.len) : (i += 1) {
                        const f = ct_fields.get(i);
                        const rt_field_var = try self.translateTypeVar(module, f.var_);
                        tmp[i] = .{ .name = f.name, .var_ = rt_field_var };
                    }
                    const rt_fields = try self.runtime_types.appendRecordFields(tmp);
                    const ext_empty = try self.runtime_types.freshFromContent(.{ .structure = .empty_record });
                    return try self.runtime_types.freshFromContent(.{ .structure = .{ .record = .{ .fields = rt_fields, .ext = ext_empty } } });
                },
                .record_poly => |poly| {
                    // Translate inner record and var_, then collapse to concrete record for runtime
                    const ct_fields = module.types.getRecordFieldsSlice(poly.record.fields);
                    var tmp = try self.allocator.alloc(types.RecordField, ct_fields.len);
                    defer self.allocator.free(tmp);
                    var i: usize = 0;
                    while (i < ct_fields.len) : (i += 1) {
                        const f = ct_fields.get(i);
                        const rt_field_var = try self.translateTypeVar(module, f.var_);
                        tmp[i] = .{ .name = f.name, .var_ = rt_field_var };
                    }
                    const rt_fields = try self.runtime_types.appendRecordFields(tmp);
                    const rt_ext = try self.translateTypeVar(module, poly.var_);
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
