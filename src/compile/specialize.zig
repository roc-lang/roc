/// Specialization system for monomorphizing generic CIR functions.
///
/// This module provides functionality to create specialized versions of generic functions
/// by copying their CIR representation and unifying type variables with concrete types.
/// The specialization process preserves type relationships and creates independent copies
/// of all AST nodes while maintaining proper type variable mappings.
const std = @import("std");
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;
const testing = std.testing;

const base = @import("base");
const types = @import("types");

const ModuleEnv = @import("ModuleEnv.zig");
const NodeStore = ModuleEnv.NodeStore;
const Expr = ModuleEnv.Expr;
const Pattern = ModuleEnv.Pattern;
const TypeAnno = ModuleEnv.TypeAnno;
const Statement = ModuleEnv.Statement;
const Store = types.Store;
const TypeVar = types.Var;
const Content = types.Content;
const FlatType = types.FlatType;
const Ident = base.Ident;
const Region = base.Region;

pub const SpecializeError = error{
    OutOfMemory,
    UnificationFailed,
    InvalidArgument,
    TypeMismatch,
    InvalidSpan,
};

/// Context for managing the specialization process
pub const SpecializationContext = struct {
    allocator: Allocator,
    type_store: *Store,
    source_env: *const ModuleEnv,
    target_env: *ModuleEnv,
    type_var_map: std.AutoHashMap(TypeVar, TypeVar),
    copied_expr_map: std.AutoHashMap(Expr.Idx, Expr.Idx),
    copied_pattern_map: std.AutoHashMap(Pattern.Idx, Pattern.Idx),
    copied_type_anno_map: std.AutoHashMap(TypeAnno.Idx, TypeAnno.Idx),

    pub fn init(
        allocator: Allocator,
        type_store: *Store,
        source_env: *const ModuleEnv,
        target_env: *ModuleEnv,
    ) SpecializationContext {
        return SpecializationContext{
            .allocator = allocator,
            .type_store = type_store,
            .source_env = source_env,
            .target_env = target_env,
            .type_var_map = std.AutoHashMap(TypeVar, TypeVar).init(allocator),
            .copied_expr_map = std.AutoHashMap(Expr.Idx, Expr.Idx).init(allocator),
            .copied_pattern_map = std.AutoHashMap(Pattern.Idx, Pattern.Idx).init(allocator),
            .copied_type_anno_map = std.AutoHashMap(TypeAnno.Idx, TypeAnno.Idx).init(allocator),
        };
    }

    pub fn deinit(self: *SpecializationContext) void {
        self.type_var_map.deinit();
        self.copied_expr_map.deinit();
        self.copied_pattern_map.deinit();
        self.copied_type_anno_map.deinit();
    }

    pub fn mapTypeVar(self: *SpecializationContext, old_var: TypeVar) !TypeVar {
        if (self.type_var_map.get(old_var)) |new_var| {
            return new_var;
        }

        const new_var = try self.type_store.fresh();
        try self.type_var_map.put(old_var, new_var);

        const old_resolved = self.type_store.resolveVar(old_var);

        // Copy the content from the old variable to the new variable
        const new_content = try self.copyContent(old_resolved.desc.content);
        try self.type_store.setVarContent(new_var, new_content);

        return new_var;
    }

    fn copyContent(self: *SpecializationContext, content: Content) SpecializeError!Content {
        return switch (content) {
            .flex_var => |name| Content{ .flex_var = name },
            .rigid_var => |name| Content{ .rigid_var = name },
            .alias => |alias| blk: {
                // For now, just return the same alias. In a real implementation,
                // we'd need to map all the type variables in alias.vars
                break :blk Content{ .alias = alias };
            },
            .structure => |flat_type| Content{ .structure = try self.copyFlatType(flat_type) },
            .err => Content{ .err = {} },
        };
    }

    fn copyFlatType(self: *SpecializationContext, flat_type: FlatType) SpecializeError!FlatType {
        return switch (flat_type) {
            .box => |v| FlatType{ .box = try self.mapTypeVar(v) },
            .list => |v| FlatType{ .list = try self.mapTypeVar(v) },
            .str => FlatType{ .str = {} },
            .num => |num| blk: {
                const new_num = switch (num) {
                    .num_poly => |poly| types.Num{ .num_poly = .{
                        .var_ = try self.mapTypeVar(poly.var_),
                        .requirements = poly.requirements,
                    } },
                    .int_poly => |poly| types.Num{ .int_poly = .{
                        .var_ = try self.mapTypeVar(poly.var_),
                        .requirements = poly.requirements,
                    } },
                    .frac_poly => |poly| types.Num{ .frac_poly = .{
                        .var_ = try self.mapTypeVar(poly.var_),
                        .requirements = poly.requirements,
                    } },
                    else => num,
                };
                break :blk FlatType{ .num = new_num };
            },
            .record => |record| blk: {
                const mapped_ext = try self.mapTypeVar(record.ext);
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
                const mapped_ret = try self.mapTypeVar(func.ret);
                break :blk FlatType{
                    .fn_pure = .{
                        .args = func.args, // Args contain TypeVars that are mapped at unification time
                        .ret = mapped_ret,
                        .needs_instantiation = func.needs_instantiation,
                    },
                };
            },
            .fn_effectful => |func| blk: {
                const mapped_ret = try self.mapTypeVar(func.ret);
                break :blk FlatType{
                    .fn_effectful = .{
                        .args = func.args, // Args contain TypeVars that are mapped at unification time
                        .ret = mapped_ret,
                        .needs_instantiation = func.needs_instantiation,
                    },
                };
            },
            .fn_unbound => |func| blk: {
                const mapped_ret = try self.mapTypeVar(func.ret);
                break :blk FlatType{
                    .fn_unbound = .{
                        .args = func.args, // Args contain TypeVars that are mapped at unification time
                        .ret = mapped_ret,
                        .needs_instantiation = func.needs_instantiation,
                    },
                };
            },
            .tag_union => |tag_union| blk: {
                const mapped_ext = try self.mapTypeVar(tag_union.ext);
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
                    .ext = try self.mapTypeVar(rec.record.ext),
                },
                .var_ = try self.mapTypeVar(rec.var_),
            } },
            .nominal_type => |nom| FlatType{ .nominal_type = nom },
            .empty_record => FlatType{ .empty_record = {} },
            .empty_tag_union => FlatType{ .empty_tag_union = {} },
        };
    }
};

/// Creates a specialized version of a generic function expression.
///
/// This function takes a generic lambda or closure and creates a monomorphized
/// copy where all type variables are unified with concrete argument types.
///
/// Args:
///   - allocator: Memory allocator for temporary allocations
///   - type_store: Type storage system for managing type variables
///   - source_env: Module environment containing the original function
///   - target_env: Module environment where the specialized function will be created
///   - function_expr: Index of the function expression to specialize (must be lambda or closure)
///   - arg_types: Array of concrete types to unify with function arguments
///
/// Returns:
///   - A struct containing the specialized expression index and its type variable
///
/// Errors:
///   - InvalidArgument: If function_expr is not a lambda/closure or arg count mismatch
///   - UnificationFailed: If argument types cannot be unified with function parameters
///   - OutOfMemory: If allocation fails during copying process
pub fn specializeFunctionExpr(
    allocator: Allocator,
    type_store: *Store,
    source_env: *const ModuleEnv,
    target_env: *ModuleEnv,
    function_expr: Expr.Idx,
    arg_types: []const TypeVar,
) !struct { expr: Expr.Idx, type_var: TypeVar } {
    var context = SpecializationContext.init(allocator, type_store, source_env, target_env);
    defer context.deinit();

    const original_expr = source_env.store.getExpr(function_expr);

    if (original_expr != .e_lambda and original_expr != .e_closure) {
        return SpecializeError.InvalidArgument;
    }

    // Deep copy the function expression and all its dependencies
    const new_expr_idx = try copyExpr(&context, function_expr);
    const new_expr = target_env.store.getExpr(new_expr_idx);

    // Extract argument patterns from the copied function
    const args = if (new_expr == .e_lambda) new_expr.e_lambda.args else blk: {
        const lambda_expr = context.target_env.store.getExpr(new_expr.e_closure.lambda_idx);
        break :blk lambda_expr.e_lambda.args;
    };

    // Get type variables for all argument patterns
    const pattern_type_vars = try extractPatternTypeVars(&context, args);
    defer allocator.free(pattern_type_vars);

    // Validate argument count matches
    if (pattern_type_vars.len != arg_types.len) {
        return SpecializeError.InvalidArgument;
    }

    // Unify each pattern's type variable with the corresponding concrete type
    for (pattern_type_vars, arg_types) |pattern_var, arg_var| {
        try type_store.setVarRedirect(pattern_var, arg_var);
    }

    // Create function type variable for the specialized expression
    const result_type_var = @intFromEnum(new_expr_idx);

    return .{ .expr = new_expr_idx, .type_var = @enumFromInt(result_type_var) };
}

// Helper functions for proper span creation

fn createPatternSpan(context: *SpecializationContext, patterns: []const Pattern.Idx) !Pattern.Span {
    const start = context.target_env.store.scratch_patterns.items.items.len;
    for (patterns) |pattern| {
        try context.target_env.store.addScratchPattern(pattern);
    }
    return context.target_env.store.patternSpanFrom(@intCast(start));
}

fn createExprSpan(context: *SpecializationContext, exprs: []const Expr.Idx) !Expr.Span {
    const start = context.target_env.store.scratch_exprs.items.items.len;
    for (exprs) |expr| {
        try context.target_env.store.addScratchExpr(expr);
    }
    return context.target_env.store.exprSpanFrom(@intCast(start));
}

fn createRecordDestructSpan(context: *SpecializationContext, destructs: []const Pattern.RecordDestruct.Idx) !Pattern.RecordDestruct.Span {
    const start = context.target_env.store.scratch_record_destructs.items.items.len;
    for (destructs) |destruct| {
        try context.target_env.store.addScratchRecordDestruct(destruct);
    }
    return context.target_env.store.recordDestructSpanFrom(@intCast(start));
}

fn createRecordFieldSpan(context: *SpecializationContext, fields: []const ModuleEnv.RecordField.Idx) !ModuleEnv.RecordField.Span {
    const start = context.target_env.store.scratch_record_fields.items.items.len;
    for (fields) |field| {
        try context.target_env.store.addScratchRecordField(field);
    }
    return context.target_env.store.recordFieldSpanFrom(@intCast(start));
}

fn createIfBranchSpan(context: *SpecializationContext, branches: []const Expr.IfBranch.Idx) !Expr.IfBranch.Span {
    const start = context.target_env.store.scratch_if_branches.items.items.len;
    for (branches) |branch| {
        try context.target_env.store.addScratchIfBranch(branch);
    }
    return context.target_env.store.ifBranchSpanFrom(@intCast(start));
}

fn createMatchBranchSpan(context: *SpecializationContext, branches: []const Expr.Match.Branch.Idx) !Expr.Match.Branch.Span {
    const start = context.target_env.store.scratch_match_branches.items.items.len;
    for (branches) |branch| {
        try context.target_env.store.addScratchMatchBranch(branch);
    }
    return context.target_env.store.matchBranchSpanFrom(@intCast(start));
}

fn copyPattern(context: *SpecializationContext, pattern_idx: Pattern.Idx) SpecializeError!Pattern.Idx {
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
                .list_var = try context.mapTypeVar(list.list_var),
                .elem_var = try context.mapTypeVar(list.elem_var),
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
                .whole_var = try context.mapTypeVar(record.whole_var),
                .ext_var = try context.mapTypeVar(record.ext_var),
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
    const mapped_type_var = try context.mapTypeVar(@enumFromInt(old_type_var));

    // Unify by setting a redirect from the new type var to the mapped one
    try context.type_store.setVarRedirect(@enumFromInt(new_type_var), mapped_type_var);

    try context.copied_pattern_map.put(pattern_idx, new_pattern_idx);
    return new_pattern_idx;
}

fn copyExpr(context: *SpecializationContext, expr_idx: Expr.Idx) SpecializeError!Expr.Idx {
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
                .elem_var = try context.mapTypeVar(list.elem_var),
                .elems = new_span,
            } }, .{ .flex_var = null }, region);
        },
        .e_empty_list => try context.target_env.addExprAndTypeVar(.{ .e_empty_list = .{} }, .{ .flex_var = null }, region),
        .e_record => |record| blk: {
            const fields = context.source_env.store.sliceRecordFields(record.fields);
            var new_fields = try context.allocator.alloc(ModuleEnv.RecordField.Idx, fields.len);
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
            .variant_var = try context.mapTypeVar(tag.variant_var),
            .ext_var = try context.mapTypeVar(tag.ext_var),
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
                    .redundant = try context.mapTypeVar(old_branch.redundant),
                }, branch_region);
            }

            const new_span = try createMatchBranchSpan(context, new_branches);

            break :blk try context.target_env.addExprAndTypeVar(.{ .e_match = .{
                .cond = new_cond,
                .branches = new_span,
                .exhaustive = try context.mapTypeVar(match.exhaustive),
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
    const mapped_type_var = try context.mapTypeVar(@enumFromInt(old_type_var));

    // Unify by setting a redirect from the new type var to the mapped one
    try context.type_store.setVarRedirect(@enumFromInt(new_type_var), mapped_type_var);

    try context.copied_expr_map.put(expr_idx, new_expr_idx);
    return new_expr_idx;
}

fn copyStatement(context: *SpecializationContext, stmt_idx: Statement.Idx) !Statement.Idx {
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
    const mapped_type_var = try context.mapTypeVar(@enumFromInt(old_type_var));

    // Unify by setting a redirect from the new type var to the mapped one
    try context.type_store.setVarRedirect(@enumFromInt(new_type_var), mapped_type_var);

    return new_stmt_idx;
}

fn copyTypeAnno(context: *SpecializationContext, type_anno_idx: TypeAnno.Idx) SpecializeError!TypeAnno.Idx {
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
    const mapped_type_var = try context.mapTypeVar(@enumFromInt(old_type_var));

    // Unify by setting a redirect from the new type var to the mapped one
    try context.type_store.setVarRedirect(@enumFromInt(new_type_var), mapped_type_var);

    try context.copied_type_anno_map.put(type_anno_idx, new_type_anno_idx);
    return new_type_anno_idx;
}

fn extractPatternTypeVars(context: *SpecializationContext, patterns: Pattern.Span) ![]TypeVar {
    var list = std.ArrayList(TypeVar).init(context.allocator);
    defer list.deinit();

    const pattern_slice = context.target_env.store.slicePatterns(patterns);
    for (pattern_slice) |pattern_idx| {
        try collectPatternTypeVars(context, pattern_idx, &list);
    }

    return try list.toOwnedSlice();
}

fn collectPatternTypeVars(context: *SpecializationContext, pattern_idx: Pattern.Idx, list: *std.ArrayList(TypeVar)) !void {
    const type_var = @intFromEnum(pattern_idx);
    try list.append(@enumFromInt(type_var));

    const pattern = context.target_env.store.getPattern(pattern_idx);

    switch (pattern) {
        .assign => {},
        .underscore => {},
        .int_literal => {},
        .frac_f32_literal => {},
        .frac_f64_literal => {},
        .small_dec_literal => {},
        .dec_literal => {},
        .str_literal => {},
        .list => |list_pat| {
            const patterns_slice = context.target_env.store.slicePatterns(list_pat.patterns);
            for (patterns_slice) |pat| {
                try collectPatternTypeVars(context, pat, list);
            }
            if (list_pat.rest_info) |rest_info| {
                if (rest_info.pattern) |p| {
                    try collectPatternTypeVars(context, p, list);
                }
            }
        },
        .tuple => |tuple| {
            const patterns_slice = context.target_env.store.slicePatterns(tuple.patterns);
            for (patterns_slice) |pat| {
                try collectPatternTypeVars(context, pat, list);
            }
        },
        .record_destructure => |record| {
            const destructs_slice = context.target_env.store.sliceRecordDestructs(record.destructs);
            for (destructs_slice) |destruct_idx| {
                const destruct = context.target_env.store.getRecordDestruct(destruct_idx);
                const pattern_idx_from_kind = destruct.kind.toPatternIdx();
                try collectPatternTypeVars(context, pattern_idx_from_kind, list);
            }
        },
        .applied_tag => |tag| {
            const args_slice = context.target_env.store.slicePatterns(tag.args);
            for (args_slice) |arg| {
                try collectPatternTypeVars(context, arg, list);
            }
        },
        .as => |as| {
            try collectPatternTypeVars(context, as.pattern, list);
        },
        .nominal => |nom| {
            try collectPatternTypeVars(context, nom.backing_pattern, list);
        },
        .nominal_external => |nom| {
            try collectPatternTypeVars(context, nom.backing_pattern, list);
        },
        .runtime_error => {},
    }
}

test "specialize identity function" {
    const allocator = testing.allocator;

    var type_store = Store.init(allocator);
    defer type_store.deinit();

    var source_env = try ModuleEnv.init(allocator, &type_store);
    defer source_env.deinit();

    var target_env = try ModuleEnv.init(allocator, &type_store);
    defer target_env.deinit();

    // Create a simple identity function: \x -> x
    const x_ident = try source_env.addIdent("x");
    const param_pattern = try source_env.addPatternAndTypeVar(.{ .assign = .{ .ident = x_ident } }, .{ .flex_var = null }, Region.empty());
    const body_expr = try source_env.addExprAndTypeVar(.{ .e_lookup_local = .{ .pattern_idx = param_pattern } }, .{ .flex_var = null }, Region.empty());

    // Create the lambda
    const start = source_env.store.scratchPatternTop();
    try source_env.store.addScratchPattern(param_pattern);
    const args_span = try source_env.store.patternSpanFrom(start);

    const func_expr = try source_env.addExprAndTypeVar(.{ .e_lambda = .{
        .args = args_span,
        .body = body_expr,
    } }, .{ .flex_var = null }, Region.empty());

    // Create a concrete type to specialize with
    const str_type_var = try type_store.fresh();
    try type_store.setVarContent(str_type_var, Content{ .structure = .str });

    // Specialize the function
    const result = try specializeFunctionExpr(
        allocator,
        &type_store,
        &source_env,
        &target_env,
        func_expr,
        &[_]TypeVar{str_type_var},
    );

    // Verify the result
    const specialized_expr = target_env.store.getExpr(result.expr);
    try testing.expect(specialized_expr == .e_lambda);

    // Verify the type is a function type
    const result_content = type_store.resolveVar(result.type_var).desc.content;
    try testing.expect(result_content == .structure);
    try testing.expect(result_content.structure == .fn_pure);
}

test "specialize polymorphic list function" {
    const allocator = testing.allocator;

    var type_store = Store.init(allocator);
    defer type_store.deinit();

    var source_env = try ModuleEnv.init(allocator, &type_store);
    defer source_env.deinit();

    var target_env = try ModuleEnv.init(allocator, &type_store);
    defer target_env.deinit();

    // Create a function that returns an empty list: \lst -> []
    const elem_type_var = try type_store.fresh();
    const list_type_var = try type_store.fresh();
    try type_store.setVarContent(list_type_var, Content{ .structure = .{ .list = elem_type_var } });

    const empty_pat_start = source_env.store.scratchPatternTop();
    const empty_patterns = try source_env.store.patternSpanFrom(empty_pat_start);
    const param_pattern = try source_env.addPatternAndTypeVarRedirect(.{ .list = .{
        .list_var = list_type_var,
        .elem_var = elem_type_var,
        .patterns = empty_patterns,
        .rest_info = null,
    } }, list_type_var, Region.empty());

    const body_expr = try source_env.addExprAndTypeVarRedirect(.{ .e_empty_list = {} }, list_type_var, Region.empty());

    const pat_start = source_env.store.scratchPatternTop();
    try source_env.store.addScratchPattern(param_pattern);
    const args_span = try source_env.store.patternSpanFrom(pat_start);
    const func_expr = try source_env.addExprAndTypeVar(.{ .e_lambda = .{
        .args = args_span,
        .body = body_expr,
    } }, .{ .flex_var = null }, Region.empty());

    // Specialize with List(I32)
    const num_type_var = try type_store.fresh();
    try type_store.setVarContent(num_type_var, Content{ .structure = .{ .num = .{ .num_compact = .{ .int = .i32 } } } });

    const list_of_num_var = try type_store.fresh();
    try type_store.setVarContent(list_of_num_var, Content{ .structure = .{ .list = num_type_var } });

    const result = try specializeFunctionExpr(
        allocator,
        &type_store,
        &source_env,
        &target_env,
        func_expr,
        &[_]TypeVar{list_of_num_var},
    );

    const specialized_expr = target_env.store.getExpr(result.expr);
    try testing.expect(specialized_expr == .e_lambda);

    const result_content = type_store.resolveVar(result.type_var).desc.content;
    try testing.expect(result_content == .structure);
    try testing.expect(result_content.structure == .fn_pure);
}

test "specialize function with Box type" {
    const allocator = testing.allocator;

    var type_store = Store.init(allocator);
    defer type_store.deinit();

    var source_env = try ModuleEnv.init(allocator, &type_store);
    defer source_env.deinit();

    var target_env = try ModuleEnv.init(allocator, &type_store);
    defer target_env.deinit();

    // Create a function that takes a Box(a) and returns it: \boxed -> boxed
    const inner_type_var = try type_store.fresh();
    try type_store.setVarContent(inner_type_var, Content{ .flex_var = null });

    const box_type_var = try type_store.fresh();
    try type_store.setVarContent(box_type_var, Content{ .structure = .{ .box = inner_type_var } });

    const param_pattern = try source_env.addPatternAndTypeVarRedirect(.{ .assign = .{ .ident = try source_env.addIdent("boxed") } }, box_type_var, Region.empty());
    const body_expr = try source_env.addExprAndTypeVarRedirect(.{ .e_lookup_local = .{ .pattern_idx = param_pattern } }, box_type_var, Region.empty());

    const pat_start = source_env.store.scratchPatternTop();
    try source_env.store.addScratchPattern(param_pattern);
    const args_span = try source_env.store.patternSpanFrom(pat_start);
    const func_expr = try source_env.addExprAndTypeVar(.{ .e_lambda = .{
        .args = args_span,
        .body = body_expr,
    } }, .{ .flex_var = null }, Region.empty());

    // Specialize with Box(Str)
    const str_type_var = try type_store.fresh();
    try type_store.setVarContent(str_type_var, Content{ .structure = .str });

    const box_str_var = try type_store.fresh();
    try type_store.setVarContent(box_str_var, Content{ .structure = .{ .box = str_type_var } });

    const result = try specializeFunctionExpr(
        allocator,
        &type_store,
        &source_env,
        &target_env,
        func_expr,
        &[_]TypeVar{box_str_var},
    );

    const specialized_expr = target_env.store.getExpr(result.expr);
    try testing.expect(specialized_expr == .e_lambda);
}

test "specialize function with record pattern" {
    const allocator = testing.allocator;

    var type_store = Store.init(allocator);
    defer type_store.deinit();

    var source_env = try ModuleEnv.init(allocator, &type_store);
    defer source_env.deinit();

    var target_env = try ModuleEnv.init(allocator, &type_store);
    defer target_env.deinit();

    // Create field types
    const name_type_var = try type_store.fresh();
    try type_store.setVarContent(name_type_var, Content{ .structure = .str });

    const age_type_var = try type_store.fresh();
    try type_store.setVarContent(age_type_var, Content{ .structure = .{ .num = .{ .num_compact = .{ .int = .u8 } } } });

    // Create record type
    const name_ident = try source_env.addIdent("name");
    const age_ident = try source_env.addIdent("age");

    const record_fields = try type_store.appendRecordFields(&[_]types.RecordField{
        .{ .name = name_ident, .var_ = name_type_var },
        .{ .name = age_ident, .var_ = age_type_var },
    });

    const record_type_var = try type_store.fresh();
    const ext_var = try type_store.fresh();
    try type_store.setVarContent(record_type_var, Content{ .structure = .{ .record = .{
        .fields = record_fields,
        .ext = ext_var,
    } } });

    // Create pattern destructuring
    const name_pattern = try source_env.addPatternAndTypeVarRedirect(.{ .assign = .{ .ident = try source_env.addIdent("n") } }, name_type_var, Region.empty());
    const age_pattern = try source_env.addPatternAndTypeVarRedirect(.{ .assign = .{ .ident = try source_env.addIdent("a") } }, age_type_var, Region.empty());

    const destruct_start = source_env.store.scratchRecordDestructTop();
    const name_destruct = try source_env.store.addRecordDestruct(.{
        .label = try source_env.addIdent("name"),
        .ident = try source_env.addIdent("n"),
        .kind = .{ .Required = name_pattern },
    }, Region.empty());
    try source_env.store.addScratchRecordDestruct(name_destruct);

    const age_destruct = try source_env.store.addRecordDestruct(.{
        .label = try source_env.addIdent("age"),
        .ident = try source_env.addIdent("a"),
        .kind = .{ .Required = age_pattern },
    }, Region.empty());
    try source_env.store.addScratchRecordDestruct(age_destruct);

    const destructs_span = try source_env.store.recordDestructSpanFrom(destruct_start);

    const param_pattern = try source_env.addPatternAndTypeVarRedirect(.{ .record_destructure = .{
        .record_var = record_type_var,
        .destructs = destructs_span,
        .opt_rec_var = null,
    } }, record_type_var, Region.empty());

    // Body returns the name field
    const body_expr = try source_env.addExprAndTypeVarRedirect(.{ .e_lookup_local = .{ .pattern_idx = name_pattern } }, name_type_var, Region.empty());

    const pat_start = source_env.store.scratchPatternTop();
    try source_env.store.addScratchPattern(param_pattern);
    const args_span = try source_env.store.patternSpanFrom(pat_start);
    const func_expr = try source_env.addExprAndTypeVar(.{ .e_lambda = .{
        .args = args_span,
        .body = body_expr,
    } }, .{ .flex_var = null }, Region.empty());

    const result = try specializeFunctionExpr(
        allocator,
        &type_store,
        &source_env,
        &target_env,
        func_expr,
        &[_]TypeVar{record_type_var},
    );

    const specialized_expr = target_env.store.getExpr(result.expr);
    try testing.expect(specialized_expr == .e_lambda);

    const unified_content = type_store.resolveVar(result.type_var).desc.content;
    try testing.expect(unified_content == .structure);
    try testing.expect(unified_content.structure == .fn_pure);
}

test "specialize function with unbound num type" {
    const allocator = testing.allocator;

    var type_store = Store.init(allocator);
    defer type_store.deinit();

    var source_env = try ModuleEnv.init(allocator, &type_store);
    defer source_env.deinit();

    var target_env = try ModuleEnv.init(allocator, &type_store);
    defer target_env.deinit();

    // Create a function that takes an unbound number literal: \42 -> 42
    const num_type_var = try type_store.fresh();
    try type_store.setVarContent(num_type_var, Content{ .structure = .{ .num = .{ .num_unbound = .{ .sign_needed = false, .bits_needed = 6 } } } });

    const param_pattern = try source_env.addPatternAndTypeVarRedirect(.{ .int_literal = .{
        .value = .{ .bytes = .{ 42, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 }, .kind = .i64 },
    } }, num_type_var, Region.empty());

    const body_expr = try source_env.addExprAndTypeVar(.{ .e_int = .{
        .value = 42,
        .sign = false,
    } }, .{ .flex_var = null }, Region.empty());

    const pat_start = source_env.store.scratchPatternTop();
    try source_env.store.addScratchPattern(param_pattern);
    const args_span = try source_env.store.patternSpanFrom(pat_start);
    const func_expr = try source_env.addExprAndTypeVar(.{ .e_lambda = .{
        .args = args_span,
        .body = body_expr,
    } }, .{ .flex_var = null }, Region.empty());

    // Specialize with I64
    const int_type_var = try type_store.fresh();
    try type_store.setVarContent(int_type_var, Content{ .structure = .{ .num = .{ .num_compact = .{ .int = .i64 } } } });

    const result = try specializeFunctionExpr(
        allocator,
        &type_store,
        &source_env,
        &target_env,
        func_expr,
        &[_]TypeVar{int_type_var},
    );

    const specialized_expr = target_env.store.getExpr(result.expr);
    try testing.expect(specialized_expr == .e_lambda);

    const result_content = type_store.resolveVar(result.type_var).desc.content;
    try testing.expect(result_content == .structure);
    try testing.expect(result_content.structure == .fn_pure);
}

// Error handling tests

test "specializeFunctionExpr rejects non-function expressions" {
    const allocator = testing.allocator;

    var type_store = Store.init(allocator);
    defer type_store.deinit();

    var source_env = try ModuleEnv.init(allocator, &type_store);
    defer source_env.deinit();

    var target_env = try ModuleEnv.init(allocator, &type_store);
    defer target_env.deinit();

    // Create a non-function expression (integer literal)
    const int_expr = try source_env.addExprAndTypeVar(.{ .e_int = .{ .value = 42, .sign = false } }, .{ .flex_var = null }, Region.empty());

    // Should fail with InvalidArgument
    const result = specializeFunctionExpr(
        allocator,
        &type_store,
        &source_env,
        &target_env,
        int_expr,
        &[_]TypeVar{},
    );

    try testing.expectError(SpecializeError.InvalidArgument, result);
}

test "specializeFunctionExpr rejects argument count mismatch" {
    const allocator = testing.allocator;

    var type_store = Store.init(allocator);
    defer type_store.deinit();

    var source_env = try ModuleEnv.init(allocator, &type_store);
    defer source_env.deinit();

    var target_env = try ModuleEnv.init(allocator, &type_store);
    defer target_env.deinit();

    // Create a function with 2 parameters: \x, y -> x
    const x_pattern = try source_env.addPatternAndTypeVar(.{ .assign = .{ .ident = try source_env.addIdent("x") } }, .{ .flex_var = null }, Region.empty());
    const y_pattern = try source_env.addPatternAndTypeVar(.{ .assign = .{ .ident = try source_env.addIdent("y") } }, .{ .flex_var = null }, Region.empty());
    const body_expr = try source_env.addExprAndTypeVar(.{ .e_lookup_local = .{ .pattern_idx = x_pattern } }, .{ .flex_var = null }, Region.empty());

    const start = source_env.store.scratchPatternTop();
    try source_env.store.addScratchPattern(x_pattern);
    try source_env.store.addScratchPattern(y_pattern);
    const args_span = try source_env.store.patternSpanFrom(start);

    const func_expr = try source_env.addExprAndTypeVar(.{ .e_lambda = .{
        .args = args_span,
        .body = body_expr,
    } }, .{ .flex_var = null }, Region.empty());

    // Provide only 1 argument type instead of 2
    const str_type = try type_store.fresh();
    try type_store.setVarContent(str_type, Content{ .structure = .str });

    // Should fail with InvalidArgument
    const result = specializeFunctionExpr(
        allocator,
        &type_store,
        &source_env,
        &target_env,
        func_expr,
        &[_]TypeVar{str_type}, // Only 1 arg, but function expects 2
    );

    try testing.expectError(SpecializeError.InvalidArgument, result);
}

test "type variable mapping consistency" {
    const allocator = testing.allocator;

    var type_store = Store.init(allocator);
    defer type_store.deinit();

    var source_env = try ModuleEnv.init(allocator, &type_store);
    defer source_env.deinit();

    var target_env = try ModuleEnv.init(allocator, &type_store);
    defer target_env.deinit();

    var context = SpecializationContext.init(allocator, &type_store, &source_env, &target_env);
    defer context.deinit();

    // Create a type variable
    const original_var = try type_store.fresh();
    try type_store.setVarContent(original_var, Content{ .structure = .str });

    // Map it twice - should get the same result
    const mapped_var1 = try context.mapTypeVar(original_var);
    const mapped_var2 = try context.mapTypeVar(original_var);

    try testing.expectEqual(mapped_var1, mapped_var2);

    // Verify content was copied correctly
    const mapped_content = type_store.resolveVar(mapped_var1).desc.content;
    try testing.expect(mapped_content == .structure);
    try testing.expect(mapped_content.structure == .str);
}

test "empty function argument lists" {
    const allocator = testing.allocator;

    var type_store = Store.init(allocator);
    defer type_store.deinit();

    var source_env = try ModuleEnv.init(allocator, &type_store);
    defer source_env.deinit();

    var target_env = try ModuleEnv.init(allocator, &type_store);
    defer target_env.deinit();

    // Create a zero-argument function: \-> 42
    const body_expr = try source_env.addExprAndTypeVar(.{ .e_int = .{ .value = 42, .sign = false } }, .{ .flex_var = null }, Region.empty());

    const start = source_env.store.scratchPatternTop();
    const args_span = try source_env.store.patternSpanFrom(start); // Empty span

    const func_expr = try source_env.addExprAndTypeVar(.{ .e_lambda = .{
        .args = args_span,
        .body = body_expr,
    } }, .{ .flex_var = null }, Region.empty());

    // Specialize with empty argument types
    const result = try specializeFunctionExpr(
        allocator,
        &type_store,
        &source_env,
        &target_env,
        func_expr,
        &[_]TypeVar{}, // No arguments
    );

    // Verify the result is a lambda
    const specialized_expr = target_env.store.getExpr(result.expr);
    try testing.expect(specialized_expr == .e_lambda);
}

test "complex nested function specialization" {
    const allocator = testing.allocator;

    var type_store = Store.init(allocator);
    defer type_store.deinit();

    var source_env = try ModuleEnv.init(allocator, &type_store);
    defer source_env.deinit();

    var target_env = try ModuleEnv.init(allocator, &type_store);
    defer target_env.deinit();

    // Create a complex function: \(x, y) -> if x == 42 then [y] else []
    // This tests pattern matching, conditionals, literals, and list construction

    // Create types
    const num_type_var = try type_store.fresh();
    try type_store.setVarContent(num_type_var, Content{ .structure = .{ .num = .{ .num_compact = .{ .int = .i32 } } } });

    const elem_type_var = try type_store.fresh();
    try type_store.setVarContent(elem_type_var, Content{ .structure = .str });

    const list_type_var = try type_store.fresh();
    try type_store.setVarContent(list_type_var, Content{ .structure = .{ .list = elem_type_var } });

    // Create tuple pattern (x, y)
    const x_pattern = try source_env.addPatternAndTypeVarRedirect(.{ .assign = .{ .ident = try source_env.addIdent("x") } }, num_type_var, Region.empty());
    const y_pattern = try source_env.addPatternAndTypeVarRedirect(.{ .assign = .{ .ident = try source_env.addIdent("y") } }, elem_type_var, Region.empty());

    const tuple_pat_start = source_env.store.scratchPatternTop();
    try source_env.store.addScratchPattern(x_pattern);
    try source_env.store.addScratchPattern(y_pattern);
    const tuple_patterns = try source_env.store.patternSpanFrom(tuple_pat_start);

    const tuple_pattern = try source_env.addPatternAndTypeVar(.{ .tuple = .{
        .patterns = tuple_patterns,
    } }, .{ .flex_var = null }, Region.empty());

    // Create condition: x == 42
    const x_lookup = try source_env.addExprAndTypeVarRedirect(.{ .e_lookup_local = .{ .pattern_idx = x_pattern } }, num_type_var, Region.empty());
    const num_42 = try source_env.addExprAndTypeVarRedirect(.{ .e_int = .{ .value = 42, .sign = false } }, num_type_var, Region.empty());
    const condition = try source_env.addExprAndTypeVar(.{ .e_binop = .{
        .lhs = x_lookup,
        .op = .equals,
        .rhs = num_42,
    } }, .{ .flex_var = null }, Region.empty());

    // Create then branch: [y]
    const y_lookup = try source_env.addExprAndTypeVarRedirect(.{ .e_lookup_local = .{ .pattern_idx = y_pattern } }, elem_type_var, Region.empty());
    const list_start = source_env.store.scratchExprTop();
    try source_env.store.addScratchExpr(y_lookup);
    const list_elems = try source_env.store.exprSpanFrom(list_start);
    const then_branch = try source_env.addExprAndTypeVarRedirect(.{ .e_list = .{
        .elem_var = elem_type_var,
        .elems = list_elems,
    } }, list_type_var, Region.empty());

    // Create else branch: []
    const else_branch = try source_env.addExprAndTypeVarRedirect(.{ .e_empty_list = {} }, list_type_var, Region.empty());

    // Create if expression
    const if_branch_idx = try source_env.store.addIfBranch(.{
        .cond = condition,
        .body = then_branch,
    }, Region.empty());

    const if_branch_start = source_env.store.scratchIfBranchTop();
    try source_env.store.addScratchIfBranch(if_branch_idx);
    const if_branches = try source_env.store.ifBranchSpanFrom(if_branch_start);

    const body_expr = try source_env.addExprAndTypeVarRedirect(.{ .e_if = .{
        .branches = if_branches,
        .final_else = else_branch,
    } }, list_type_var, Region.empty());

    // Create lambda
    const lambda_pat_start = source_env.store.scratchPatternTop();
    try source_env.store.addScratchPattern(tuple_pattern);
    const lambda_args = try source_env.store.patternSpanFrom(lambda_pat_start);

    const func_expr = try source_env.addExprAndTypeVar(.{ .e_lambda = .{
        .args = lambda_args,
        .body = body_expr,
    } }, .{ .flex_var = null }, Region.empty());

    // Create tuple argument type for specialization
    const tuple_type_var = try type_store.fresh();
    const tuple_elem_range = try type_store.appendTypeVars(&[_]TypeVar{ num_type_var, elem_type_var });
    try type_store.setVarContent(tuple_type_var, Content{ .structure = .{ .tuple = .{ .elems = tuple_elem_range } } });

    // Specialize the function
    const result = try specializeFunctionExpr(
        allocator,
        &type_store,
        &source_env,
        &target_env,
        func_expr,
        &[_]TypeVar{tuple_type_var},
    );

    // Verify the result
    const specialized_expr = target_env.store.getExpr(result.expr);
    try testing.expect(specialized_expr == .e_lambda);

    // Verify the lambda has the correct structure
    const lambda_body = target_env.store.getExpr(specialized_expr.e_lambda.body);
    try testing.expect(lambda_body == .e_if);

    // Verify type
    const result_content = type_store.resolveVar(result.type_var).desc.content;
    try testing.expect(result_content == .structure);
    try testing.expect(result_content.structure == .fn_pure);
}
