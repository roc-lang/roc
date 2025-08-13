//! Deep cloning system for CIR nodes and their types.
//!
//! This module provides functionality to create deep copies of CIR nodes (expressions, patterns, etc.)
//! while also cloning their associated types. Each cloned node gets fresh type variables that
//! are mapped from the original types, preserving the structure but creating independent copies.
//! This is useful for specialization, where you need to create copies of generic code that can
//! be unified with different concrete types.
//!
//! Important: Type aliases are resolved during cloning. The cloned types will not contain any
//! aliases - they are expanded to their underlying types. This simplifies the cloned type
//! structure. Nominal types, however, are preserved as they represent distinct types.
const std = @import("std");
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;
const testing = std.testing;

const base = @import("base");
const types = @import("types");

const ModuleEnv = @import("ModuleEnv.zig");
const CIR = @import("CIR.zig");
const NodeStore = CIR.NodeStore;
const Expr = CIR.Expr;
const Pattern = CIR.Pattern;
const TypeAnno = CIR.TypeAnno;
const Statement = CIR.Statement;
const Store = types.Store;
const TypeStore = types.Store;
const TypeVar = types.Var;
const Content = types.Content;
const FlatType = types.FlatType;
const Ident = base.Ident;
const Region = base.Region;

/// Context for deep cloning CIR nodes, maintaining mappings between source and target environments.
pub const CloneContext = struct {
    allocator: Allocator,
    type_store: *Store,
    source_env: *const ModuleEnv,
    target_env: *ModuleEnv,
    type_var_cache: std.AutoHashMap(TypeVar, TypeVar),
    copied_expr_map: std.AutoHashMap(Expr.Idx, Expr.Idx),
    copied_pattern_map: std.AutoHashMap(Pattern.Idx, Pattern.Idx),
    copied_type_anno_map: std.AutoHashMap(TypeAnno.Idx, TypeAnno.Idx),

    pub fn init(
        allocator: Allocator,
        type_store: *Store,
        source_env: *const ModuleEnv,
        target_env: *ModuleEnv,
    ) CloneContext {
        return CloneContext{
            .allocator = allocator,
            .type_store = type_store,
            .source_env = source_env,
            .target_env = target_env,
            .type_var_cache = std.AutoHashMap(TypeVar, TypeVar).init(allocator),
            .copied_expr_map = std.AutoHashMap(Expr.Idx, Expr.Idx).init(allocator),
            .copied_pattern_map = std.AutoHashMap(Pattern.Idx, Pattern.Idx).init(allocator),
            .copied_type_anno_map = std.AutoHashMap(TypeAnno.Idx, TypeAnno.Idx).init(allocator),
        };
    }

    pub fn deinit(self: *CloneContext) void {
        self.type_var_cache.deinit();
        self.copied_expr_map.deinit();
        self.copied_pattern_map.deinit();
        self.copied_type_anno_map.deinit();
    }

    pub fn cloneTypeVar(self: *CloneContext, old_var: TypeVar) !TypeVar {
        if (self.type_var_cache.get(old_var)) |new_var| {
            return new_var;
        }

        const new_var = try self.type_store.fresh();
        try self.type_var_cache.put(old_var, new_var);

        const old_resolved = self.type_store.resolveVar(old_var);

        // Copy the content from the old variable to the new variable
        const new_content = try self.copyContent(old_resolved.desc.content);
        self.type_store.setVarContent(new_var, new_content) catch unreachable;

        return new_var;
    }

    fn copyContent(self: *CloneContext, content: Content) Allocator.Error!Content {
        return switch (content) {
            .flex_var => |name| Content{ .flex_var = name },
            .rigid_var => |name| Content{ .rigid_var = name },
            .alias => |alias| blk: {
                // Aliases are resolved during cloning - we clone what the alias points to,
                // not the alias itself. This simplifies the cloned type structure.
                const backing_var = self.type_store.getAliasBackingVar(alias);
                const resolved = self.type_store.resolveVar(backing_var);
                break :blk try self.copyContent(resolved.desc.content);
            },
            .structure => |flat_type| Content{ .structure = try self.copyFlatType(flat_type) },
            .err => Content{ .err = {} },
        };
    }

    fn copyFlatType(self: *CloneContext, flat_type: FlatType) Allocator.Error!FlatType {
        return switch (flat_type) {
            .box => |v| FlatType{ .box = try self.cloneTypeVar(v) },
            .list => |v| FlatType{ .list = try self.cloneTypeVar(v) },
            .str => FlatType{ .str = {} },
            .num => |num| blk: {
                const new_num = switch (num) {
                    .num_poly => |poly| types.Num{ .num_poly = .{
                        .var_ = try self.cloneTypeVar(poly.var_),
                        .requirements = poly.requirements,
                    } },
                    .int_poly => |poly| types.Num{ .int_poly = .{
                        .var_ = try self.cloneTypeVar(poly.var_),
                        .requirements = poly.requirements,
                    } },
                    .frac_poly => |poly| types.Num{ .frac_poly = .{
                        .var_ = try self.cloneTypeVar(poly.var_),
                        .requirements = poly.requirements,
                    } },
                    else => num,
                };
                break :blk FlatType{ .num = new_num };
            },
            .record => |record| blk: {
                const mapped_ext = try self.cloneTypeVar(record.ext);
                break :blk FlatType{
                    .record = .{
                        .fields = record.fields, // Fields contain TypeVars that are mapped at unification time
                        .ext = mapped_ext,
                    },
                };
            },
            .tuple => |tuple| blk: {
                // Tuple elements are TypeVars that get mapped during unification
                break :blk FlatType{ .tuple = tuple };
            },
            .fn_pure => |func| blk: {
                const mapped_ret = try self.cloneTypeVar(func.ret);
                break :blk FlatType{
                    .fn_pure = .{
                        .args = func.args, // Args contain TypeVars that are mapped at unification time
                        .ret = mapped_ret,
                        .needs_instantiation = func.needs_instantiation,
                    },
                };
            },
            .fn_effectful => |func| blk: {
                const mapped_ret = try self.cloneTypeVar(func.ret);
                break :blk FlatType{
                    .fn_effectful = .{
                        .args = func.args, // Args contain TypeVars that are mapped at unification time
                        .ret = mapped_ret,
                        .needs_instantiation = func.needs_instantiation,
                    },
                };
            },
            .fn_unbound => |func| blk: {
                const mapped_ret = try self.cloneTypeVar(func.ret);
                break :blk FlatType{
                    .fn_unbound = .{
                        .args = func.args, // Args contain TypeVars that are mapped at unification time
                        .ret = mapped_ret,
                        .needs_instantiation = func.needs_instantiation,
                    },
                };
            },
            .tag_union => |tag_union| blk: {
                const mapped_ext = try self.cloneTypeVar(tag_union.ext);
                break :blk FlatType{
                    .tag_union = .{
                        .tags = tag_union.tags, // Tags contain TypeVars that are mapped at unification time
                        .ext = mapped_ext,
                    },
                };
            },
            .list_unbound => FlatType{ .list_unbound = {} },
            .record_unbound => |range| FlatType{ .record_unbound = range },
            .record_poly => |rec| FlatType{ .record_poly = .{
                .record = .{
                    .fields = rec.record.fields,
                    .ext = try self.cloneTypeVar(rec.record.ext),
                },
                .var_ = try self.cloneTypeVar(rec.var_),
            } },
            .nominal_type => |nom| FlatType{ .nominal_type = nom },
            .empty_record => FlatType{ .empty_record = {} },
            .empty_tag_union => FlatType{ .empty_tag_union = {} },
        };
    }
};

/// Deep clones any CIR node (expression, pattern, type annotation, or statement) along with its types.
///
/// This creates an independent copy of the node and all its children, with fresh type variables
/// that are mapped from the original types. The cloned node can then be unified with different
/// concrete types without affecting the original.
///
/// Only fails if allocation fails.
pub fn deepClone(
    allocator: Allocator,
    type_store: *Store,
    source_env: *const ModuleEnv,
    target_env: *ModuleEnv,
    node: anytype, // Can be Expr.Idx, Pattern.Idx, TypeAnno.Idx, or Statement.Idx
) Allocator.Error!@TypeOf(node) {
    var context = CloneContext.init(allocator, type_store, source_env, target_env);
    defer context.deinit();

    // Clone based on the type of node
    return switch (@TypeOf(node)) {
        Expr.Idx => try copyExpr(&context, node),
        Pattern.Idx => try copyPattern(&context, node),
        TypeAnno.Idx => try copyTypeAnno(&context, node),
        Statement.Idx => try copyStatement(&context, node),
        else => @compileError("deepClone only supports Expr.Idx, Pattern.Idx, TypeAnno.Idx, and Statement.Idx"),
    };
}
// Helper functions for proper span creation

fn createPatternSpan(context: *CloneContext, patterns: []const Pattern.Idx) !Pattern.Span {
    const start = context.target_env.store.scratch_patterns.items.items.len;
    for (patterns) |pattern| {
        try context.target_env.store.addScratchPattern(pattern);
    }
    return context.target_env.store.patternSpanFrom(@intCast(start));
}

fn createExprSpan(context: *CloneContext, exprs: []const Expr.Idx) !Expr.Span {
    const start = context.target_env.store.scratch_exprs.items.items.len;
    for (exprs) |expr| {
        try context.target_env.store.addScratchExpr(expr);
    }
    return context.target_env.store.exprSpanFrom(@intCast(start));
}

fn createRecordDestructSpan(context: *CloneContext, destructs: []const Pattern.RecordDestruct.Idx) !Pattern.RecordDestruct.Span {
    const start = context.target_env.store.scratch_record_destructs.items.items.len;
    for (destructs) |destruct| {
        try context.target_env.store.addScratchRecordDestruct(destruct);
    }
    return context.target_env.store.recordDestructSpanFrom(@intCast(start));
}

fn createRecordFieldSpan(context: *CloneContext, fields: []const CIR.RecordField.Idx) !CIR.RecordField.Span {
    const start = context.target_env.store.scratch_record_fields.items.items.len;
    for (fields) |field| {
        try context.target_env.store.addScratch("scratch_record_fields", field);
    }
    return context.target_env.store.recordFieldSpanFrom(@intCast(start));
}

fn createIfBranchSpan(context: *CloneContext, branches: []const Expr.IfBranch.Idx) !Expr.IfBranch.Span {
    const start = context.target_env.store.scratch_if_branches.items.items.len;
    for (branches) |branch| {
        try context.target_env.store.addScratchIfBranch(branch);
    }
    return context.target_env.store.ifBranchSpanFrom(@intCast(start));
}

fn createMatchBranchSpan(context: *CloneContext, branches: []const Expr.Match.Branch.Idx) !Expr.Match.Branch.Span {
    const start = context.target_env.store.scratch_match_branches.items.items.len;
    for (branches) |branch| {
        try context.target_env.store.addScratchMatchBranch(branch);
    }
    return context.target_env.store.matchBranchSpanFrom(@intCast(start));
}

fn copyPattern(context: *CloneContext, pattern_idx: Pattern.Idx) Allocator.Error!Pattern.Idx {
    if (context.copied_pattern_map.get(pattern_idx)) |copied_idx| {
        return copied_idx;
    }

    const old_pattern = context.source_env.store.getPattern(pattern_idx);
    const region = context.source_env.store.getPatternRegion(pattern_idx);

    const new_pattern_idx = switch (old_pattern) {
        .assign => |a| try context.target_env.addPatternAndTypeVar(.{ .assign = a }, .{ .flex_var = null }, region),
        .underscore => try context.target_env.addPatternAndTypeVar(.underscore, .{ .flex_var = null }, region),
        .int_literal => |int| try context.target_env.addPatternAndTypeVar(.{ .int_literal = int }, .{ .flex_var = null }, region),
        .frac_f32_literal => |float| try context.target_env.addPatternAndTypeVar(.{ .frac_f32_literal = float }, .{ .flex_var = null }, region),
        .frac_f64_literal => |float| try context.target_env.addPatternAndTypeVar(.{ .frac_f64_literal = float }, .{ .flex_var = null }, region),
        .small_dec_literal => |dec| try context.target_env.addPatternAndTypeVar(.{ .small_dec_literal = dec }, .{ .flex_var = null }, region),
        .dec_literal => |dec| try context.target_env.addPatternAndTypeVar(.{ .dec_literal = dec }, .{ .flex_var = null }, region),
        .str_literal => |str| try context.target_env.addPatternAndTypeVar(.{ .str_literal = str }, .{ .flex_var = null }, region),
        .list => |list| blk: {
            const patterns_slice = context.source_env.store.slicePatterns(list.patterns);
            var new_patterns = try context.allocator.alloc(Pattern.Idx, patterns_slice.len);
            defer context.allocator.free(new_patterns);

            for (patterns_slice, 0..) |pat, i| {
                new_patterns[i] = try copyPattern(context, pat);
            }

            const new_rest_info = if (list.rest_info) |rest_info| @as(?@TypeOf(rest_info), .{
                .index = rest_info.index,
                .pattern = if (rest_info.pattern) |p| try copyPattern(context, p) else null,
            }) else null;

            const new_pattern_span = try createPatternSpan(context, new_patterns);

            break :blk try context.target_env.addPatternAndTypeVar(.{ .list = .{
                .list_var = try context.cloneTypeVar(list.list_var),
                .elem_var = try context.cloneTypeVar(list.elem_var),
                .patterns = new_pattern_span,
                .rest_info = new_rest_info,
            } }, .{ .flex_var = null }, region);
        },
        .record_destructure => |record| blk: {
            const destructs_slice = context.source_env.store.sliceRecordDestructs(record.destructs);
            var new_destructs = try context.allocator.alloc(Pattern.RecordDestruct.Idx, destructs_slice.len);
            defer context.allocator.free(new_destructs);

            for (destructs_slice, 0..) |destruct_idx, i| {
                const old_destruct = context.source_env.store.getRecordDestruct(destruct_idx);

                const new_kind = switch (old_destruct.kind) {
                    .Required => |p| Pattern.RecordDestruct.Kind{ .Required = try copyPattern(context, p) },
                    .SubPattern => |p| Pattern.RecordDestruct.Kind{ .SubPattern = try copyPattern(context, p) },
                };

                const destruct_region = context.source_env.store.getPatternRegion(@enumFromInt(@intFromEnum(destruct_idx)));
                new_destructs[i] = try context.target_env.store.addRecordDestruct(.{
                    .label = old_destruct.label,
                    .ident = old_destruct.ident,
                    .kind = new_kind,
                }, destruct_region);
            }

            const new_destructs_span = try createRecordDestructSpan(context, new_destructs);

            break :blk try context.target_env.addPatternAndTypeVar(.{ .record_destructure = .{
                .whole_var = try context.cloneTypeVar(record.whole_var),
                .ext_var = try context.cloneTypeVar(record.ext_var),
                .destructs = new_destructs_span,
            } }, .{ .flex_var = null }, region);
        },
        .tuple => |tuple| blk: {
            const patterns_slice = context.source_env.store.slicePatterns(tuple.patterns);
            var new_patterns = try context.allocator.alloc(Pattern.Idx, patterns_slice.len);
            defer context.allocator.free(new_patterns);

            for (patterns_slice, 0..) |pat, i| {
                new_patterns[i] = try copyPattern(context, pat);
            }

            const new_pattern_span = try createPatternSpan(context, new_patterns);

            break :blk try context.target_env.addPatternAndTypeVar(.{ .tuple = .{
                .patterns = new_pattern_span,
            } }, .{ .flex_var = null }, region);
        },
        .applied_tag => |tag| blk: {
            const args_slice = context.source_env.store.slicePatterns(tag.args);
            var new_args = try context.allocator.alloc(Pattern.Idx, args_slice.len);
            defer context.allocator.free(new_args);

            for (args_slice, 0..) |arg, i| {
                new_args[i] = try copyPattern(context, arg);
            }

            const new_args_span = try createPatternSpan(context, new_args);

            break :blk try context.target_env.addPatternAndTypeVar(.{ .applied_tag = .{
                .name = tag.name,
                .args = new_args_span,
            } }, .{ .flex_var = null }, region);
        },
        .as => |as| try context.target_env.addPatternAndTypeVar(.{ .as = .{
            .pattern = try copyPattern(context, as.pattern),
            .ident = as.ident,
        } }, .{ .flex_var = null }, region),
        .nominal => |nom| try context.target_env.addPatternAndTypeVar(.{ .nominal = .{
            .nominal_type_decl = nom.nominal_type_decl,
            .backing_pattern = try copyPattern(context, nom.backing_pattern),
            .backing_type = nom.backing_type,
        } }, .{ .flex_var = null }, region),
        .nominal_external => |nom| try context.target_env.addPatternAndTypeVar(.{ .nominal_external = .{
            .module_idx = nom.module_idx,
            .target_node_idx = nom.target_node_idx,
            .backing_pattern = try copyPattern(context, nom.backing_pattern),
            .backing_type = nom.backing_type,
        } }, .{ .flex_var = null }, region),
        .runtime_error => |err| try context.target_env.addPatternAndTypeVar(.{ .runtime_error = err }, .{ .flex_var = null }, region),
    };

    const old_type_var = @intFromEnum(pattern_idx);
    const new_type_var = @intFromEnum(new_pattern_idx);
    const cloned_type_var = try context.cloneTypeVar(@enumFromInt(old_type_var));

    // Unify by setting a redirect from the new type var to the mapped one
    context.type_store.setVarRedirect(@enumFromInt(new_type_var), cloned_type_var) catch unreachable;

    try context.copied_pattern_map.put(pattern_idx, new_pattern_idx);
    return new_pattern_idx;
}

fn copyExpr(context: *CloneContext, expr_idx: Expr.Idx) Allocator.Error!Expr.Idx {
    if (context.copied_expr_map.get(expr_idx)) |copied_idx| {
        return copied_idx;
    }

    const old_expr = context.source_env.store.getExpr(expr_idx);
    const region = context.source_env.store.getExprRegion(expr_idx);

    const new_expr_idx = switch (old_expr) {
        .e_lookup_local => |lookup| {
            // For local lookups, we need to find the corresponding pattern in the new environment
            // For now, just copy it directly - this might need more sophisticated handling
            return try context.target_env.addExprAndTypeVar(.{ .e_lookup_local = lookup }, .{ .flex_var = null }, region);
        },
        .e_lookup_external => |lookup| try context.target_env.addExprAndTypeVar(.{ .e_lookup_external = lookup }, .{ .flex_var = null }, region),
        .e_int => |int| try context.target_env.addExprAndTypeVar(.{ .e_int = int }, .{ .flex_var = null }, region),
        .e_frac_f32 => |float| try context.target_env.addExprAndTypeVar(.{ .e_frac_f32 = float }, .{ .flex_var = null }, region),
        .e_frac_f64 => |float| try context.target_env.addExprAndTypeVar(.{ .e_frac_f64 = float }, .{ .flex_var = null }, region),
        .e_frac_dec => |dec| try context.target_env.addExprAndTypeVar(.{ .e_frac_dec = dec }, .{ .flex_var = null }, region),
        .e_dec_small => |dec| try context.target_env.addExprAndTypeVar(.{ .e_dec_small = dec }, .{ .flex_var = null }, region),
        .e_str_segment => |str| try context.target_env.addExprAndTypeVar(.{ .e_str_segment = str }, .{ .flex_var = null }, region),
        .e_str => |str| blk: {
            const segments = context.source_env.store.sliceExpr(str.span);
            var new_segments = try context.allocator.alloc(Expr.Idx, segments.len);
            defer context.allocator.free(new_segments);

            for (segments, 0..) |seg, i| {
                new_segments[i] = try copyExpr(context, seg);
            }

            const new_span = try createExprSpan(context, new_segments);

            break :blk try context.target_env.addExprAndTypeVar(.{ .e_str = .{
                .span = new_span,
            } }, .{ .flex_var = null }, region);
        },
        .e_list => |list| blk: {
            const elems = context.source_env.store.sliceExpr(list.elems);
            var new_elems = try context.allocator.alloc(Expr.Idx, elems.len);
            defer context.allocator.free(new_elems);

            for (elems, 0..) |elem, i| {
                new_elems[i] = try copyExpr(context, elem);
            }

            const new_span = try createExprSpan(context, new_elems);

            break :blk try context.target_env.addExprAndTypeVar(.{ .e_list = .{
                .elem_var = try context.cloneTypeVar(list.elem_var),
                .elems = new_span,
            } }, .{ .flex_var = null }, region);
        },
        .e_empty_list => try context.target_env.addExprAndTypeVar(.{ .e_empty_list = .{} }, .{ .flex_var = null }, region),
        .e_record => |record| blk: {
            const fields = context.source_env.store.sliceRecordFields(record.fields);
            var new_fields = try context.allocator.alloc(CIR.RecordField.Idx, fields.len);
            defer context.allocator.free(new_fields);

            for (fields, 0..) |field_idx, i| {
                const old_field = context.source_env.store.getRecordField(field_idx);
                const value_region = context.source_env.store.getExprRegion(old_field.value);
                new_fields[i] = try context.target_env.store.addRecordField(.{
                    .name = old_field.name,
                    .value = try copyExpr(context, old_field.value),
                }, value_region);
            }

            const new_span = try createRecordFieldSpan(context, new_fields);
            const new_ext = if (record.ext) |ext| try copyExpr(context, ext) else null;

            break :blk try context.target_env.addExprAndTypeVar(.{ .e_record = .{
                .fields = new_span,
                .ext = new_ext,
            } }, .{ .flex_var = null }, region);
        },
        .e_empty_record => try context.target_env.addExprAndTypeVar(.{ .e_empty_record = .{} }, .{ .flex_var = null }, region),
        .e_tuple => |tuple| blk: {
            const elems = context.source_env.store.sliceExpr(tuple.elems);
            var new_elems = try context.allocator.alloc(Expr.Idx, elems.len);
            defer context.allocator.free(new_elems);

            for (elems, 0..) |elem, i| {
                new_elems[i] = try copyExpr(context, elem);
            }

            const new_span = try createExprSpan(context, new_elems);

            break :blk try context.target_env.addExprAndTypeVar(.{ .e_tuple = .{
                .elems = new_span,
            } }, .{ .flex_var = null }, region);
        },
        .e_dot_access => |access| blk: {
            const new_receiver = try copyExpr(context, access.receiver);

            // Copy args if present
            const new_args = if (access.args) |args_span| blk2: {
                const args = context.source_env.store.sliceExpr(args_span);
                var copied_args = try context.allocator.alloc(Expr.Idx, args.len);
                defer context.allocator.free(copied_args);

                for (args, 0..) |arg, i| {
                    copied_args[i] = try copyExpr(context, arg);
                }

                break :blk2 try createExprSpan(context, copied_args);
            } else null;

            break :blk try context.target_env.addExprAndTypeVar(.{ .e_dot_access = .{
                .receiver = new_receiver,
                .field_name = access.field_name,
                .args = new_args,
            } }, .{ .flex_var = null }, region);
        },
        .e_tag => |tag| blk: {
            const args = context.source_env.store.sliceExpr(tag.args);
            var new_args = try context.allocator.alloc(Expr.Idx, args.len);
            defer context.allocator.free(new_args);

            for (args, 0..) |arg, i| {
                new_args[i] = try copyExpr(context, arg);
            }

            const new_span = try createExprSpan(context, new_args);

            break :blk try context.target_env.addExprAndTypeVar(.{ .e_tag = .{
                .name = tag.name,
                .args = new_span,
            } }, .{ .flex_var = null }, region);
        },
        .e_zero_argument_tag => |tag| try context.target_env.addExprAndTypeVar(.{ .e_zero_argument_tag = .{
            .closure_name = tag.closure_name,
            .variant_var = try context.cloneTypeVar(tag.variant_var),
            .ext_var = try context.cloneTypeVar(tag.ext_var),
            .name = tag.name,
        } }, .{ .flex_var = null }, region),
        .e_nominal => |nom| try context.target_env.addExprAndTypeVar(.{ .e_nominal = .{
            .nominal_type_decl = nom.nominal_type_decl,
            .backing_expr = try copyExpr(context, nom.backing_expr),
            .backing_type = nom.backing_type,
        } }, .{ .flex_var = null }, region),
        .e_nominal_external => |nom| try context.target_env.addExprAndTypeVar(.{ .e_nominal_external = .{
            .module_idx = nom.module_idx,
            .target_node_idx = nom.target_node_idx,
            .backing_expr = try copyExpr(context, nom.backing_expr),
            .backing_type = nom.backing_type,
        } }, .{ .flex_var = null }, region),
        .e_call => |call| blk: {
            const args = context.source_env.store.sliceExpr(call.args);
            var new_args = try context.allocator.alloc(Expr.Idx, args.len);
            defer context.allocator.free(new_args);

            for (args, 0..) |arg, i| {
                new_args[i] = try copyExpr(context, arg);
            }

            const new_span = try createExprSpan(context, new_args);

            break :blk try context.target_env.addExprAndTypeVar(.{ .e_call = .{
                .args = new_span,
                .called_via = call.called_via,
            } }, .{ .flex_var = null }, region);
        },
        .e_lambda => |lambda| blk: {
            const args = context.source_env.store.slicePatterns(lambda.args);
            var new_args = try context.allocator.alloc(Pattern.Idx, args.len);
            defer context.allocator.free(new_args);

            for (args, 0..) |arg, i| {
                new_args[i] = try copyPattern(context, arg);
            }

            const new_args_span = try createPatternSpan(context, new_args);
            const new_body = try copyExpr(context, lambda.body);

            break :blk try context.target_env.addExprAndTypeVar(.{ .e_lambda = .{
                .args = new_args_span,
                .body = new_body,
            } }, .{ .flex_var = null }, region);
        },
        .e_closure => |closure| blk: {
            // Copy the lambda expression that this closure references
            const new_lambda_idx = try copyExpr(context, closure.lambda_idx);

            // TODO: Properly handle captures
            // For now, we'll just use an empty span for captures
            const new_captures = Expr.Capture.Span{ .span = .{ .start = 0, .len = 0 } };

            break :blk try context.target_env.addExprAndTypeVar(.{ .e_closure = .{
                .lambda_idx = new_lambda_idx,
                .captures = new_captures,
            } }, .{ .flex_var = null }, region);
        },
        .e_match => |match| blk: {
            const new_cond = try copyExpr(context, match.cond);

            const branches = context.source_env.store.sliceMatchBranches(match.branches);
            var new_branches = try context.allocator.alloc(Expr.Match.Branch.Idx, branches.len);
            defer context.allocator.free(new_branches);

            for (branches, 0..) |branch_idx, i| {
                const old_branch = context.source_env.store.getMatchBranch(branch_idx);
                const branch_region = context.source_env.store.getExprRegion(old_branch.value);

                // TODO: Properly handle branch patterns
                // For now, we'll just use an empty span
                const new_patterns = Expr.Match.BranchPattern.Span{ .span = .{ .start = 0, .len = 0 } };

                new_branches[i] = try context.target_env.store.addMatchBranch(.{
                    .patterns = new_patterns,
                    .guard = if (old_branch.guard) |g| try copyExpr(context, g) else null,
                    .value = try copyExpr(context, old_branch.value),
                    .redundant = try context.cloneTypeVar(old_branch.redundant),
                }, branch_region);
            }

            const new_span = try createMatchBranchSpan(context, new_branches);

            break :blk try context.target_env.addExprAndTypeVar(.{ .e_match = .{
                .cond = new_cond,
                .branches = new_span,
                .exhaustive = try context.cloneTypeVar(match.exhaustive),
            } }, .{ .flex_var = null }, region);
        },
        .e_if => |if_expr| blk: {
            const branches = context.source_env.store.sliceIfBranches(if_expr.branches);
            var new_branches = try context.allocator.alloc(Expr.IfBranch.Idx, branches.len);
            defer context.allocator.free(new_branches);

            for (branches, 0..) |branch_idx, i| {
                const old_branch = context.source_env.store.getIfBranch(branch_idx);
                const branch_region = context.source_env.store.getExprRegion(old_branch.body);
                new_branches[i] = try context.target_env.store.addIfBranch(.{
                    .cond = try copyExpr(context, old_branch.cond),
                    .body = try copyExpr(context, old_branch.body),
                }, branch_region);
            }

            const new_span = try createIfBranchSpan(context, new_branches);
            const new_final_else = try copyExpr(context, if_expr.final_else);

            break :blk try context.target_env.addExprAndTypeVar(.{ .e_if = .{
                .branches = new_span,
                .final_else = new_final_else,
            } }, .{ .flex_var = null }, region);
        },
        .e_block => |block| blk: {
            const stmts = context.source_env.store.sliceStatements(block.stmts);
            var new_stmts = try context.allocator.alloc(Statement.Idx, stmts.len);
            defer context.allocator.free(new_stmts);

            for (stmts, 0..) |stmt, i| {
                new_stmts[i] = try copyStatement(context, stmt);
            }

            // Create a new statement span
            const start = context.target_env.store.scratch_statements.items.items.len;
            for (new_stmts) |stmt| {
                try context.target_env.store.addScratchStatement(stmt);
            }
            const new_span = try context.target_env.store.statementSpanFrom(@intCast(start));
            const new_final_expr = try copyExpr(context, block.final_expr);

            break :blk try context.target_env.addExprAndTypeVar(.{ .e_block = .{
                .stmts = new_span,
                .final_expr = new_final_expr,
            } }, .{ .flex_var = null }, region);
        },
        .e_binop => |binop| blk: {
            const new_lhs = try copyExpr(context, binop.lhs);
            const new_rhs = try copyExpr(context, binop.rhs);

            break :blk try context.target_env.addExprAndTypeVar(.{ .e_binop = .{
                .lhs = new_lhs,
                .op = binop.op,
                .rhs = new_rhs,
            } }, .{ .flex_var = null }, region);
        },
        .e_unary_minus => |unary| blk: {
            const new_operand = try copyExpr(context, unary.expr);

            break :blk try context.target_env.addExprAndTypeVar(.{ .e_unary_minus = .{
                .expr = new_operand,
            } }, .{ .flex_var = null }, region);
        },
        .e_unary_not => |unary| blk: {
            const new_operand = try copyExpr(context, unary.expr);

            break :blk try context.target_env.addExprAndTypeVar(.{ .e_unary_not = .{
                .expr = new_operand,
            } }, .{ .flex_var = null }, region);
        },
        .e_crash => |crash| try context.target_env.addExprAndTypeVar(.{ .e_crash = crash }, .{ .flex_var = null }, region),
        .e_runtime_error => |err| try context.target_env.addExprAndTypeVar(.{ .e_runtime_error = err }, .{ .flex_var = null }, region),
        .e_expect => |expect| blk: {
            const new_body = try copyExpr(context, expect.body);

            break :blk try context.target_env.addExprAndTypeVar(.{ .e_expect = .{
                .body = new_body,
            } }, .{ .flex_var = null }, region);
        },
        .e_dbg => |dbg| blk: {
            const new_expr = try copyExpr(context, dbg.expr);

            break :blk try context.target_env.addExprAndTypeVar(.{ .e_dbg = .{
                .expr = new_expr,
            } }, .{ .flex_var = null }, region);
        },
        .e_ellipsis => try context.target_env.addExprAndTypeVar(.{ .e_ellipsis = .{} }, .{ .flex_var = null }, region),
    };

    const old_type_var = @intFromEnum(expr_idx);
    const new_type_var = @intFromEnum(new_expr_idx);
    const cloned_type_var = try context.cloneTypeVar(@enumFromInt(old_type_var));

    // Unify by setting a redirect from the new type var to the mapped one
    context.type_store.setVarRedirect(@enumFromInt(new_type_var), cloned_type_var) catch unreachable;

    try context.copied_expr_map.put(expr_idx, new_expr_idx);
    return new_expr_idx;
}

fn copyStatement(context: *CloneContext, stmt_idx: Statement.Idx) !Statement.Idx {
    const old_stmt = context.source_env.store.getStatement(stmt_idx);
    const region = context.source_env.store.getStatementRegion(stmt_idx);

    const new_stmt_idx = switch (old_stmt) {
        .s_decl => |decl| blk: {
            const new_pattern = try copyPattern(context, decl.pattern);
            const new_expr = try copyExpr(context, decl.expr);

            break :blk try context.target_env.addStatementAndTypeVar(.{ .s_decl = .{
                .pattern = new_pattern,
                .expr = new_expr,
            } }, .{ .flex_var = null }, region);
        },
        .s_var => |v| blk: {
            const new_pattern = try copyPattern(context, v.pattern_idx);
            const new_expr = try copyExpr(context, v.expr);

            break :blk try context.target_env.addStatementAndTypeVar(.{ .s_var = .{
                .pattern_idx = new_pattern,
                .expr = new_expr,
            } }, .{ .flex_var = null }, region);
        },
        .s_reassign => |reassign| blk: {
            const new_pattern = try copyPattern(context, reassign.pattern_idx);
            const new_expr = try copyExpr(context, reassign.expr);

            break :blk try context.target_env.addStatementAndTypeVar(.{ .s_reassign = .{
                .pattern_idx = new_pattern,
                .expr = new_expr,
            } }, .{ .flex_var = null }, region);
        },
        .s_crash => |crash| try context.target_env.addStatementAndTypeVar(.{ .s_crash = crash }, .{ .flex_var = null }, region),
        .s_dbg => |dbg| blk: {
            const new_expr = try copyExpr(context, dbg.expr);

            break :blk try context.target_env.addStatementAndTypeVar(.{ .s_dbg = .{
                .expr = new_expr,
            } }, .{ .flex_var = null }, region);
        },
        .s_expr => |expr| blk: {
            const new_expr = try copyExpr(context, expr.expr);

            break :blk try context.target_env.addStatementAndTypeVar(.{ .s_expr = .{
                .expr = new_expr,
            } }, .{ .flex_var = null }, region);
        },
        .s_expect => |expect| blk: {
            const new_body = try copyExpr(context, expect.body);

            break :blk try context.target_env.addStatementAndTypeVar(.{ .s_expect = .{
                .body = new_body,
            } }, .{ .flex_var = null }, region);
        },
        .s_for => |f| blk: {
            const new_pattern = try copyPattern(context, f.patt);
            const new_expr = try copyExpr(context, f.expr);
            const new_body = try copyExpr(context, f.body);

            break :blk try context.target_env.addStatementAndTypeVar(.{ .s_for = .{
                .patt = new_pattern,
                .expr = new_expr,
                .body = new_body,
            } }, .{ .flex_var = null }, region);
        },
        .s_return => |ret| blk: {
            const new_expr = try copyExpr(context, ret.expr);

            break :blk try context.target_env.addStatementAndTypeVar(.{ .s_return = .{
                .expr = new_expr,
            } }, .{ .flex_var = null }, region);
        },
        .s_import => |import| try context.target_env.addStatementAndTypeVar(.{ .s_import = import }, .{ .flex_var = null }, region),
        .s_alias_decl => |alias| blk: {
            // For now, just copy as-is. In a real implementation, we'd need to copy TypeHeader and TypeAnno
            break :blk try context.target_env.addStatementAndTypeVar(.{ .s_alias_decl = alias }, .{ .flex_var = null }, region);
        },
        .s_nominal_decl => |nominal| blk: {
            // For now, just copy as-is. In a real implementation, we'd need to copy TypeHeader and TypeAnno
            break :blk try context.target_env.addStatementAndTypeVar(.{ .s_nominal_decl = nominal }, .{ .flex_var = null }, region);
        },
        .s_type_anno => |anno| blk: {
            const new_anno = try copyTypeAnno(context, anno.anno);

            break :blk try context.target_env.addStatementAndTypeVar(.{ .s_type_anno = .{
                .name = anno.name,
                .anno = new_anno,
                .where = anno.where,
            } }, .{ .flex_var = null }, region);
        },
    };

    const old_type_var = @intFromEnum(stmt_idx);
    const new_type_var = @intFromEnum(new_stmt_idx);
    const cloned_type_var = try context.cloneTypeVar(@enumFromInt(old_type_var));

    // Unify by setting a redirect from the new type var to the mapped one
    context.type_store.setVarRedirect(@enumFromInt(new_type_var), cloned_type_var) catch unreachable;

    return new_stmt_idx;
}

fn copyTypeAnno(context: *CloneContext, type_anno_idx: TypeAnno.Idx) Allocator.Error!TypeAnno.Idx {
    if (context.copied_type_anno_map.get(type_anno_idx)) |copied_idx| {
        return copied_idx;
    }

    const old_type_anno = context.source_env.store.getTypeAnno(type_anno_idx);
    const region = context.source_env.store.getTypeAnnoRegion(type_anno_idx);

    const new_type_anno_idx = switch (old_type_anno) {
        .underscore => try context.target_env.addTypeAnnoAndTypeVar(.underscore, .{ .flex_var = null }, region),
        .ty_var => |var_name| try context.target_env.addTypeAnnoAndTypeVar(.{ .ty_var = var_name }, .{ .flex_var = null }, region),
        .ty => |ty| try context.target_env.addTypeAnnoAndTypeVar(.{ .ty = ty }, .{ .flex_var = null }, region),
        .apply => |apply| blk: {
            // Create a span for the new arguments
            const start = context.target_env.store.scratch_type_annos.items.items.len;
            const arg_slice = context.source_env.store.sliceTypeAnnos(apply.args);
            for (arg_slice) |arg_idx| {
                const new_arg = try copyTypeAnno(context, arg_idx);
                try context.target_env.store.addScratchTypeAnno(new_arg);
            }
            const new_args_span = try context.target_env.store.typeAnnoSpanFrom(@intCast(start));

            break :blk try context.target_env.addTypeAnnoAndTypeVar(.{ .apply = .{
                .symbol = apply.symbol,
                .args = new_args_span,
            } }, .{ .flex_var = null }, region);
        },
        .apply_external => |apply| blk: {
            // Create a span for the new arguments
            const start = context.target_env.store.scratch_type_annos.items.items.len;
            const arg_slice = context.source_env.store.sliceTypeAnnos(apply.args);
            for (arg_slice) |arg_idx| {
                const new_arg = try copyTypeAnno(context, arg_idx);
                try context.target_env.store.addScratchTypeAnno(new_arg);
            }
            const new_args_span = try context.target_env.store.typeAnnoSpanFrom(@intCast(start));

            break :blk try context.target_env.addTypeAnnoAndTypeVar(.{ .apply_external = .{
                .module_idx = apply.module_idx,
                .target_node_idx = apply.target_node_idx,
                .args = new_args_span,
            } }, .{ .flex_var = null }, region);
        },
        .@"fn" => |func| blk: {
            // Create a span for the new arguments
            const start = context.target_env.store.scratch_type_annos.items.items.len;
            const arg_slice = context.source_env.store.sliceTypeAnnos(func.args);
            for (arg_slice) |arg_idx| {
                const new_arg = try copyTypeAnno(context, arg_idx);
                try context.target_env.store.addScratchTypeAnno(new_arg);
            }
            const new_args_span = try context.target_env.store.typeAnnoSpanFrom(@intCast(start));

            const new_ret = try copyTypeAnno(context, func.ret);

            break :blk try context.target_env.addTypeAnnoAndTypeVar(.{ .@"fn" = .{
                .args = new_args_span,
                .ret = new_ret,
                .effectful = func.effectful,
            } }, .{ .flex_var = null }, region);
        },
        .record => |record| blk: {
            // Create a span for the new fields
            const start = context.target_env.store.scratch_anno_record_fields.items.items.len;
            const field_slice = context.source_env.store.sliceAnnoRecordFields(record.fields);
            for (field_slice) |field_idx| {
                const field = context.source_env.store.getAnnoRecordField(field_idx);
                const new_ty = try copyTypeAnno(context, field.ty);
                const new_field_idx = try context.target_env.store.addAnnoRecordField(.{
                    .name = field.name,
                    .ty = new_ty,
                }, region);
                try context.target_env.store.addScratchAnnoRecordField(new_field_idx);
            }
            const new_fields_span = try context.target_env.store.annoRecordFieldSpanFrom(@intCast(start));

            break :blk try context.target_env.addTypeAnnoAndTypeVar(.{ .record = .{
                .fields = new_fields_span,
            } }, .{ .flex_var = null }, region);
        },
        .tuple => |tuple| blk: {
            // Create a span for the new tuple elements
            const start = context.target_env.store.scratch_type_annos.items.items.len;
            const elem_slice = context.source_env.store.sliceTypeAnnos(tuple.elems);
            for (elem_slice) |elem_idx| {
                const new_elem = try copyTypeAnno(context, elem_idx);
                try context.target_env.store.addScratchTypeAnno(new_elem);
            }
            const new_elems_span = try context.target_env.store.typeAnnoSpanFrom(@intCast(start));

            break :blk try context.target_env.addTypeAnnoAndTypeVar(.{ .tuple = .{
                .elems = new_elems_span,
            } }, .{ .flex_var = null }, region);
        },
        .tag_union => |tag_union| blk: {
            // Create a span for the new tags
            const start = context.target_env.store.scratch_type_annos.items.items.len;
            const tag_slice = context.source_env.store.sliceTypeAnnos(tag_union.tags);
            for (tag_slice) |tag_idx| {
                const new_tag = try copyTypeAnno(context, tag_idx);
                try context.target_env.store.addScratchTypeAnno(new_tag);
            }
            const new_tags_span = try context.target_env.store.typeAnnoSpanFrom(@intCast(start));

            const new_ext = if (tag_union.ext) |ext| try copyTypeAnno(context, ext) else null;

            break :blk try context.target_env.addTypeAnnoAndTypeVar(.{ .tag_union = .{
                .tags = new_tags_span,
                .ext = new_ext,
            } }, .{ .flex_var = null }, region);
        },
        .parens => |parens| try context.target_env.addTypeAnnoAndTypeVar(.{ .parens = .{
            .anno = try copyTypeAnno(context, parens.anno),
        } }, .{ .flex_var = null }, region),
        .ty_lookup_external => |lookup| try context.target_env.addTypeAnnoAndTypeVar(.{ .ty_lookup_external = lookup }, .{ .flex_var = null }, region),
        .malformed => |malformed| try context.target_env.addTypeAnnoAndTypeVar(.{ .malformed = malformed }, .{ .flex_var = null }, region),
    };

    const old_type_var = @intFromEnum(type_anno_idx);
    const new_type_var = @intFromEnum(new_type_anno_idx);
    const cloned_type_var = try context.cloneTypeVar(@enumFromInt(old_type_var));

    // Unify by setting a redirect from the new type var to the mapped one
    context.type_store.setVarRedirect(@enumFromInt(new_type_var), cloned_type_var) catch unreachable;

    try context.copied_type_anno_map.put(type_anno_idx, new_type_anno_idx);
    return new_type_anno_idx;
}

test "deep clone expression" {
    _ = testing.allocator; // Will be used for comprehensive tests later

    // TODO: Add comprehensive tests for deep cloning functionality
    // This would require setting up test environments, type stores, etc.
    // For now, just ensure the module compiles correctly.
    try testing.expect(true);
}
