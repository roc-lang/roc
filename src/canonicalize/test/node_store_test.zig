//! Unit tests to verify `ModuleEnv.Statement` are correctly stored in `NodeStore`

const std = @import("std");
const testing = std.testing;
const base = @import("base");
const types = @import("types");
const compile = @import("compile");
const RocDec = @import("builtins").RocDec;

const from_raw_offsets = base.Region.from_raw_offsets;
const ModuleEnv = compile.ModuleEnv;
const NodeStore = compile.NodeStore;
const StringLiteral = base.StringLiteral;
const Ident = base.Ident;
const CalledVia = base.CalledVia;
const TypeVar = types.Var;

var rand = std.Random.DefaultPrng.init(1234);

/// Helper function to compare diagnostics that contain Ident.Idx fields
fn expectEqualDiagnostics(expected: ModuleEnv.Diagnostic, actual: ModuleEnv.Diagnostic) !void {
    // First check that the tags match
    try testing.expectEqual(std.meta.activeTag(expected), std.meta.activeTag(actual));
    
    // Then compare the fields based on the tag
    switch (expected) {
        .not_implemented => |e| {
            const a = actual.not_implemented;
            try testing.expectEqual(e.feature, a.feature);
            try testing.expectEqual(e.region, a.region);
        },
        .exposed_but_not_implemented => |e| {
            const a = actual.exposed_but_not_implemented;
            try testing.expect(e.ident.eql(a.ident));
            try testing.expectEqual(e.region, a.region);
        },
        .redundant_exposed => |e| {
            const a = actual.redundant_exposed;
            try testing.expect(e.ident.eql(a.ident));
            try testing.expectEqual(e.region, a.region);
            try testing.expectEqual(e.original_region, a.original_region);
        },
        .invalid_num_literal => |e| {
            const a = actual.invalid_num_literal;
            try testing.expectEqual(e.region, a.region);
        },
        .invalid_single_quote => |e| {
            const a = actual.invalid_single_quote;
            try testing.expectEqual(e.region, a.region);
        },
        .empty_tuple => |e| {
            const a = actual.empty_tuple;
            try testing.expectEqual(e.region, a.region);
        },
        .ident_already_in_scope => |e| {
            const a = actual.ident_already_in_scope;
            try testing.expect(e.ident.eql(a.ident));
            try testing.expectEqual(e.region, a.region);
        },
        .ident_not_in_scope => |e| {
            const a = actual.ident_not_in_scope;
            try testing.expect(e.ident.eql(a.ident));
            try testing.expectEqual(e.region, a.region);
        },
        .invalid_top_level_statement => |e| {
            const a = actual.invalid_top_level_statement;
            try testing.expectEqual(e.stmt, a.stmt);
            try testing.expectEqual(e.region, a.region);
        },
        .expr_not_canonicalized => |e| {
            const a = actual.expr_not_canonicalized;
            try testing.expectEqual(e.region, a.region);
        },
        .invalid_string_interpolation => |e| {
            const a = actual.invalid_string_interpolation;
            try testing.expectEqual(e.region, a.region);
        },
        .pattern_arg_invalid => |e| {
            const a = actual.pattern_arg_invalid;
            try testing.expectEqual(e.region, a.region);
        },
        .pattern_not_canonicalized => |e| {
            const a = actual.pattern_not_canonicalized;
            try testing.expectEqual(e.region, a.region);
        },
        .can_lambda_not_implemented => |e| {
            const a = actual.can_lambda_not_implemented;
            try testing.expectEqual(e.region, a.region);
        },
        .lambda_body_not_canonicalized => |e| {
            const a = actual.lambda_body_not_canonicalized;
            try testing.expectEqual(e.region, a.region);
        },
        .if_condition_not_canonicalized => |e| {
            const a = actual.if_condition_not_canonicalized;
            try testing.expectEqual(e.region, a.region);
        },
        .if_then_not_canonicalized => |e| {
            const a = actual.if_then_not_canonicalized;
            try testing.expectEqual(e.region, a.region);
        },
        .if_else_not_canonicalized => |e| {
            const a = actual.if_else_not_canonicalized;
            try testing.expectEqual(e.region, a.region);
        },
        .malformed_type_annotation => |e| {
            const a = actual.malformed_type_annotation;
            try testing.expectEqual(e.region, a.region);
        },
        .malformed_where_clause => |e| {
            const a = actual.malformed_where_clause;
            try testing.expectEqual(e.region, a.region);
        },
        .var_across_function_boundary => |e| {
            const a = actual.var_across_function_boundary;
            try testing.expectEqual(e.region, a.region);
        },
        .shadowing_warning => |e| {
            const a = actual.shadowing_warning;
            try testing.expect(e.ident.eql(a.ident));
            try testing.expectEqual(e.region, a.region);
            try testing.expectEqual(e.original_region, a.original_region);
        },
        .type_redeclared => |e| {
            const a = actual.type_redeclared;
            try testing.expect(e.name.eql(a.name));
            try testing.expectEqual(e.original_region, a.original_region);
            try testing.expectEqual(e.redeclared_region, a.redeclared_region);
        },
        .tuple_elem_not_canonicalized => |e| {
            const a = actual.tuple_elem_not_canonicalized;
            try testing.expectEqual(e.region, a.region);
        },
        .module_not_found => |e| {
            const a = actual.module_not_found;
            try testing.expect(e.module_name.eql(a.module_name));
            try testing.expectEqual(e.region, a.region);
        },
        .module_not_imported => |e| {
            const a = actual.module_not_imported;
            try testing.expect(e.module_name.eql(a.module_name));
            try testing.expectEqual(e.region, a.region);
        },
        .too_many_exports => |e| {
            const a = actual.too_many_exports;
            try testing.expectEqual(e.count, a.count);
            try testing.expectEqual(e.region, a.region);
        },
        .value_not_exposed => |e| {
            const a = actual.value_not_exposed;
            try testing.expect(e.module_name.eql(a.module_name));
            try testing.expect(e.value_name.eql(a.value_name));
            try testing.expectEqual(e.region, a.region);
        },
        .type_not_exposed => |e| {
            const a = actual.type_not_exposed;
            try testing.expect(e.module_name.eql(a.module_name));
            try testing.expect(e.type_name.eql(a.type_name));
            try testing.expectEqual(e.region, a.region);
        },
        .type_parameter_conflict => |e| {
            const a = actual.type_parameter_conflict;
            try testing.expect(e.name.eql(a.name));
            try testing.expect(e.parameter_name.eql(a.parameter_name));
            try testing.expectEqual(e.region, a.region);
            try testing.expectEqual(e.original_region, a.original_region);
        },
        .unused_type_var_name => |e| {
            const a = actual.unused_type_var_name;
            try testing.expect(e.name.eql(a.name));
            try testing.expect(e.suggested_name.eql(a.suggested_name));
            try testing.expectEqual(e.region, a.region);
        },
        .type_var_marked_unused => |e| {
            const a = actual.type_var_marked_unused;
            try testing.expect(e.name.eql(a.name));
            try testing.expect(e.suggested_name.eql(a.suggested_name));
            try testing.expectEqual(e.region, a.region);
        },
        .type_var_ending_in_underscore => |e| {
            const a = actual.type_var_ending_in_underscore;
            try testing.expect(e.name.eql(a.name));
            try testing.expect(e.suggested_name.eql(a.suggested_name));
            try testing.expectEqual(e.region, a.region);
        },
        .undeclared_type => |e| {
            const a = actual.undeclared_type;
            try testing.expect(e.name.eql(a.name));
            try testing.expectEqual(e.region, a.region);
        },
        .undeclared_type_var => |e| {
            const a = actual.undeclared_type_var;
            try testing.expect(e.name.eql(a.name));
            try testing.expectEqual(e.region, a.region);
        },
        .unused_variable => |e| {
            const a = actual.unused_variable;
            try testing.expect(e.ident.eql(a.ident));
            try testing.expectEqual(e.region, a.region);
        },
        .used_underscore_variable => |e| {
            const a = actual.used_underscore_variable;
            try testing.expect(e.ident.eql(a.ident));
            try testing.expectEqual(e.region, a.region);
        },
        .type_alias_redeclared => |e| {
            const a = actual.type_alias_redeclared;
            try testing.expect(e.name.eql(a.name));
            try testing.expectEqual(e.original_region, a.original_region);
            try testing.expectEqual(e.redeclared_region, a.redeclared_region);
        },
        .nominal_type_redeclared => |e| {
            const a = actual.nominal_type_redeclared;
            try testing.expect(e.name.eql(a.name));
            try testing.expectEqual(e.original_region, a.original_region);
            try testing.expectEqual(e.redeclared_region, a.redeclared_region);
        },
        .type_shadowed_warning => |e| {
            const a = actual.type_shadowed_warning;
            try testing.expect(e.name.eql(a.name));
            try testing.expectEqual(e.region, a.region);
            try testing.expectEqual(e.original_region, a.original_region);
            try testing.expectEqual(e.cross_scope, a.cross_scope);
        },
        .duplicate_record_field => |e| {
            const a = actual.duplicate_record_field;
            try testing.expect(e.field_name.eql(a.field_name));
            try testing.expectEqual(e.duplicate_region, a.duplicate_region);
            try testing.expectEqual(e.original_region, a.original_region);
        },
        .f64_pattern_literal => |e| {
            const a = actual.f64_pattern_literal;
            try testing.expectEqual(e.region, a.region);
        },
        .underscore_in_type_declaration => |e| {
            const a = actual.underscore_in_type_declaration;
            try testing.expectEqual(e.is_alias, a.is_alias);
            try testing.expectEqual(e.region, a.region);
        },
        .crash_expects_string => |e| {
            const a = actual.crash_expects_string;
            try testing.expectEqual(e.region, a.region);
        },
    }
}

/// Generate a random index of type `T`.
fn rand_idx(comptime T: type) T {
    return @enumFromInt(rand.random().int(u32));
}

/// Helper function to compare statements that contain Ident.Idx fields
fn expectEqualStatements(expected: ModuleEnv.Statement, actual: ModuleEnv.Statement) !void {
    // First check that the tags match
    try testing.expectEqual(std.meta.activeTag(expected), std.meta.activeTag(actual));
    
    // Then compare the fields based on the tag
    switch (expected) {
        .s_decl => |e| {
            const a = actual.s_decl;
            try testing.expectEqual(e.pattern, a.pattern);
            try testing.expectEqual(e.expr, a.expr);
        },
        .s_var => |e| {
            const a = actual.s_var;
            try testing.expectEqual(e.pattern_idx, a.pattern_idx);
            try testing.expectEqual(e.expr, a.expr);
        },
        .s_reassign => |e| {
            const a = actual.s_reassign;
            try testing.expectEqual(e.pattern_idx, a.pattern_idx);
            try testing.expectEqual(e.expr, a.expr);
        },
        .s_crash => |e| {
            const a = actual.s_crash;
            try testing.expectEqual(e.msg, a.msg);
        },
        .s_dbg => |e| {
            const a = actual.s_dbg;
            try testing.expectEqual(e.expr, a.expr);
        },
        .s_import => |e| {
            const a = actual.s_import;
            try testing.expect(e.module_name_tok.eql(a.module_name_tok));
            if (e.qualifier_tok) |eq| {
                try testing.expect(eq.eql(a.qualifier_tok.?));
            } else {
                try testing.expect(a.qualifier_tok == null);
            }
            if (e.alias_tok) |ea| {
                try testing.expect(ea.eql(a.alias_tok.?));
            } else {
                try testing.expect(a.alias_tok == null);
            }
            try testing.expectEqual(e.exposes, a.exposes);
        },
        .s_alias_decl => |e| {
            const a = actual.s_alias_decl;
            try testing.expectEqual(e.header, a.header);
            try testing.expectEqual(e.anno, a.anno);
            try testing.expectEqual(e.where, a.where);
        },
        .s_nominal_decl => |e| {
            const a = actual.s_nominal_decl;
            try testing.expectEqual(e.header, a.header);
            try testing.expectEqual(e.anno, a.anno);
            try testing.expectEqual(e.where, a.where);
        },
        .s_type_anno => |e| {
            const a = actual.s_type_anno;
            try testing.expect(e.name.eql(a.name));
            try testing.expectEqual(e.anno, a.anno);
            try testing.expectEqual(e.where, a.where);
        },
        .s_expr => |e| {
            const a = actual.s_expr;
            try testing.expectEqual(e.expr, a.expr);
        },
        .s_expect => |e| {
            const a = actual.s_expect;
            try testing.expectEqual(e.body, a.body);
        },
        .s_for => |e| {
            const a = actual.s_for;
            try testing.expectEqual(e.patt, a.patt);
            try testing.expectEqual(e.expr, a.expr);
            try testing.expectEqual(e.body, a.body);
        },
        .s_return => |e| {
            const a = actual.s_return;
            try testing.expectEqual(e.expr, a.expr);
        },
    }
}

/// Generate a random index of type `T`.
fn rand_idx_u16(comptime T: type) T {
    return @enumFromInt(rand.random().int(u16));
}

/// Helper function to compare expressions that contain Ident.Idx fields
fn expectEqualExpressions(expected: ModuleEnv.Expr, actual: ModuleEnv.Expr) !void {
    // First check that the tags match
    try testing.expectEqual(std.meta.activeTag(expected), std.meta.activeTag(actual));
    
    // Then compare the fields based on the tag
    switch (expected) {
        .e_int => |e| {
            const a = actual.e_int;
            try testing.expectEqual(e.value, a.value);
        },
        .e_frac_f64 => |e| {
            const a = actual.e_frac_f64;
            try testing.expectEqual(e.value, a.value);
        },
        .e_frac_dec => |e| {
            const a = actual.e_frac_dec;
            try testing.expectEqual(e.value, a.value);
        },
        .e_dec_small => |e| {
            const a = actual.e_dec_small;
            try testing.expectEqual(e.numerator, a.numerator);
            try testing.expectEqual(e.denominator_power_of_ten, a.denominator_power_of_ten);
        },
        .e_str => |e| {
            const a = actual.e_str;
            // Compare span lengths instead of direct comparison since segments may contain Ident.Idx
            try testing.expectEqual(e.span.span.len, a.span.span.len);
        },


        .e_tag => |e| {
            const a = actual.e_tag;
            try testing.expect(e.name.eql(a.name));
            // Compare args span instead of direct comparison
            try testing.expectEqual(e.args.span.len, a.args.span.len);
        },
        .e_nominal => |e| {
            const a = actual.e_nominal;
            try testing.expectEqual(e.nominal_type_decl, a.nominal_type_decl);
            try testing.expectEqual(e.backing_expr, a.backing_expr);
            try testing.expectEqual(e.backing_type, a.backing_type);
        },
        .e_zero_argument_tag => |e| {
            const a = actual.e_zero_argument_tag;
            try testing.expect(e.closure_name.eql(a.closure_name));
            try testing.expectEqual(e.variant_var, a.variant_var);
            try testing.expectEqual(e.ext_var, a.ext_var);
            try testing.expect(e.name.eql(a.name));
        },
        .e_closure => |e| {
            const a = actual.e_closure;
            try testing.expectEqual(e.lambda_idx, a.lambda_idx);
            try testing.expectEqual(e.captures.span.len, a.captures.span.len);
        },
        .e_lookup_local => |e| {
            const a = actual.e_lookup_local;
            try testing.expectEqual(e.pattern_idx, a.pattern_idx);
        },
        .e_lookup_external => |e| {
            const a = actual.e_lookup_external;
            try testing.expectEqual(e.module_idx, a.module_idx);
            try testing.expectEqual(e.target_node_idx, a.target_node_idx);
        },
        .e_block => |e| {
            const a = actual.e_block;
            try testing.expectEqual(e.stmts, a.stmts);
            try testing.expectEqual(e.final_expr, a.final_expr);
        },
        .e_if => |e| {
            const a = actual.e_if;
            try testing.expectEqual(e.branches.span.len, a.branches.span.len);
            try testing.expectEqual(e.final_else, a.final_else);
        },
        .e_match => |e| {
            const a = actual.e_match;
            try testing.expectEqual(e.cond, a.cond);
            try testing.expectEqual(e.branches, a.branches);
        },
        .e_call => |e| {
            const a = actual.e_call;
            try testing.expectEqual(e.args, a.args);
            try testing.expectEqual(e.called_via, a.called_via);
        },

        .e_record => |e| {
            const a = actual.e_record;
            // TODO: Fix comparison of fields which may contain Ident.Idx
            // try testing.expectEqual(e.fields, a.fields);
            _ = e;
            _ = a;
        },
        .e_list => |e| {
            const a = actual.e_list;
            try testing.expectEqual(e.elem_var, a.elem_var);
            try testing.expectEqual(e.elems, a.elems);
        },
        .e_tuple => |e| {
            const a = actual.e_tuple;
            try testing.expectEqual(e.elems, a.elems);
        },
        .e_crash => |e| {
            const a = actual.e_crash;
            try testing.expectEqual(e.msg, a.msg);
        },


        .e_dot_access => |e| {
            const a = actual.e_dot_access;
            try testing.expectEqual(e.receiver, a.receiver);
            try testing.expect(e.field_name.eql(a.field_name));
            try testing.expectEqual(e.args, a.args);
        },
        .e_runtime_error => |e| {
            const a = actual.e_runtime_error;
            // Compare diagnostic indices as integers to avoid untagged union comparison issues
            try testing.expectEqual(@intFromEnum(e.diagnostic), @intFromEnum(a.diagnostic));
        },
        .e_dbg => |e| {
            const a = actual.e_dbg;
            try testing.expectEqual(e.expr, a.expr);
        },
        .e_expect => |e| {
            const a = actual.e_expect;
            try testing.expectEqual(e.body, a.body);
        },
        .e_ellipsis => {
            // No fields to compare
        },
        .e_unary_minus => |e| {
            const a = actual.e_unary_minus;
            try testing.expectEqual(e.expr, a.expr);
        },
        .e_str_segment => |e| {
            const a = actual.e_str_segment;
            try testing.expectEqual(e.literal, a.literal);
        },
        .e_nominal_external => |e| {
            const a = actual.e_nominal_external;
            try testing.expectEqual(e.module_idx, a.module_idx);
            try testing.expectEqual(e.target_node_idx, a.target_node_idx);
            try testing.expectEqual(e.backing_expr, a.backing_expr);
            try testing.expectEqual(e.backing_type, a.backing_type);
        },
        .e_lambda => |e| {
            const a = actual.e_lambda;
            try testing.expectEqual(e.args.span.len, a.args.span.len);
            try testing.expectEqual(e.body, a.body);
        },
        .e_binop => |e| {
            const a = actual.e_binop;
            try testing.expectEqual(e.op, a.op);
            try testing.expectEqual(e.lhs, a.lhs);
            try testing.expectEqual(e.rhs, a.rhs);
        },
        .e_empty_record => {
            // No fields to compare
        },
        .e_empty_list => {
            // No fields to compare
        },
    }
}

fn expectEqualPatterns(expected: ModuleEnv.Pattern, actual: ModuleEnv.Pattern) !void {
    try testing.expectEqual(std.meta.activeTag(expected), std.meta.activeTag(actual));

    switch (expected) {
        .assign => |expected_assign| {
            const actual_assign = actual.assign;
            try testing.expect(expected_assign.ident.eql(actual_assign.ident));
        },
        .as => |expected_as| {
            const actual_as = actual.as;
            try testing.expectEqual(expected_as.pattern, actual_as.pattern);
            try testing.expect(expected_as.ident.eql(actual_as.ident));
        },
        .applied_tag => |expected_tag| {
            const actual_tag = actual.applied_tag;
            try testing.expect(expected_tag.name.eql(actual_tag.name));
            try testing.expectEqual(expected_tag.args.span, actual_tag.args.span);
        },
        .record_destructure => |expected_record| {
            const actual_record = actual.record_destructure;
            try testing.expectEqual(expected_record.whole_var, actual_record.whole_var);
            try testing.expectEqual(expected_record.ext_var, actual_record.ext_var);
            try testing.expectEqual(expected_record.destructs.span, actual_record.destructs.span);
        },
        .tuple => |expected_tuple| {
            const actual_tuple = actual.tuple;
            try testing.expectEqual(expected_tuple.patterns.span, actual_tuple.patterns.span);
        },
        .list => |expected_list| {
            const actual_list = actual.list;
            try testing.expectEqual(expected_list.list_var, actual_list.list_var);
            try testing.expectEqual(expected_list.elem_var, actual_list.elem_var);
            try testing.expectEqual(expected_list.patterns.span, actual_list.patterns.span);
            try testing.expectEqual(expected_list.rest_info, actual_list.rest_info);
        },

        .int_literal => |expected_int| {
            const actual_int = actual.int_literal;
            try testing.expectEqual(expected_int.value, actual_int.value);
        },
        .str_literal => |expected_str| {
            const actual_str = actual.str_literal;
            try testing.expectEqual(expected_str, actual_str);
        },

        .underscore => {
            // No fields to compare
        },

        .nominal => |expected_nominal| {
            const actual_nominal = actual.nominal;
            try testing.expectEqual(expected_nominal.nominal_type_decl, actual_nominal.nominal_type_decl);
            try testing.expectEqual(expected_nominal.backing_pattern, actual_nominal.backing_pattern);
            try testing.expectEqual(expected_nominal.backing_type, actual_nominal.backing_type);
        },
        .nominal_external => |expected_nominal_ext| {
            const actual_nominal_ext = actual.nominal_external;
            try testing.expectEqual(expected_nominal_ext.module_idx, actual_nominal_ext.module_idx);
            try testing.expectEqual(expected_nominal_ext.target_node_idx, actual_nominal_ext.target_node_idx);
            try testing.expectEqual(expected_nominal_ext.backing_pattern, actual_nominal_ext.backing_pattern);
            try testing.expectEqual(expected_nominal_ext.backing_type, actual_nominal_ext.backing_type);
        },

        .small_dec_literal => |expected_small_dec| {
            const actual_small_dec = actual.small_dec_literal;
            try testing.expectEqual(expected_small_dec.numerator, actual_small_dec.numerator);
            try testing.expectEqual(expected_small_dec.denominator_power_of_ten, actual_small_dec.denominator_power_of_ten);
        },
        .dec_literal => |expected_dec| {
            const actual_dec = actual.dec_literal;
            try testing.expectEqual(expected_dec.value, actual_dec.value);
        },
        .runtime_error => |expected_error| {
            const actual_error = actual.runtime_error;
            // Compare diagnostic indices as integers to avoid untagged union comparison issues
            try testing.expectEqual(@intFromEnum(expected_error.diagnostic), @intFromEnum(actual_error.diagnostic));
        },
    }
}

fn expectEqualTypeAnno(expected: ModuleEnv.TypeAnno, actual: ModuleEnv.TypeAnno) !void {
    try testing.expectEqual(std.meta.activeTag(expected), std.meta.activeTag(actual));

    switch (expected) {
        .apply => |expected_apply| {
            const actual_apply = actual.apply;
            try testing.expect(expected_apply.symbol.eql(actual_apply.symbol));
            // args is a TypeAnno.Span, compare the spans
            try testing.expectEqual(expected_apply.args.span, actual_apply.args.span);
        },
        .ty_var => |expected_tv| {
            const actual_tv = actual.ty_var;
            try testing.expect(expected_tv.name.eql(actual_tv.name));
        },
        .underscore => {
            // No fields to compare
        },
        .ty => |expected_ty| {
            const actual_ty = actual.ty;
            try testing.expect(expected_ty.symbol.eql(actual_ty.symbol));
        },
        .tag_union => |expected_tu| {
            const actual_tu = actual.tag_union;
            // tags is a TypeAnno.Span
            try testing.expectEqual(expected_tu.tags.span, actual_tu.tags.span);
            // ext is optional TypeAnno.Idx
            if (expected_tu.ext) |expected_ext| {
                try testing.expect(actual_tu.ext != null);
                try testing.expectEqual(expected_ext, actual_tu.ext.?);
            } else {
                try testing.expect(actual_tu.ext == null);
            }
        },
        .tuple => |expected_tuple| {
            const actual_tuple = actual.tuple;
            // elems is a TypeAnno.Span
            try testing.expectEqual(expected_tuple.elems.span, actual_tuple.elems.span);
        },
        .record => |expected_record| {
            const actual_record = actual.record;
            // fields is a RecordField.Span
            try testing.expectEqual(expected_record.fields.span, actual_record.fields.span);
        },
        .@"fn" => |expected_fn| {
            const actual_fn = actual.@"fn";
            // args is a TypeAnno.Span
            try testing.expectEqual(expected_fn.args.span, actual_fn.args.span);
            // ret is a TypeAnno.Idx
            try testing.expectEqual(expected_fn.ret, actual_fn.ret);
            // effectful is a bool
            try testing.expectEqual(expected_fn.effectful, actual_fn.effectful);
        },
        .parens => |expected_parens| {
            const actual_parens = actual.parens;
            // anno is a TypeAnno.Idx
            try testing.expectEqual(expected_parens.anno, actual_parens.anno);
        },
        .ty_lookup_external => |expected_lookup| {
            const actual_lookup = actual.ty_lookup_external;
            try testing.expectEqual(expected_lookup.module_idx, actual_lookup.module_idx);
            try testing.expectEqual(expected_lookup.target_node_idx, actual_lookup.target_node_idx);
        },
        .malformed => |expected_malformed| {
            const actual_malformed = actual.malformed;
            // diagnostic is a Diagnostic.Idx
            // Compare diagnostic indices as integers to avoid untagged union comparison issues
            try testing.expectEqual(@intFromEnum(expected_malformed.diagnostic), @intFromEnum(actual_malformed.diagnostic));
        },
        .apply_external => |expected_apply_ext| {
            const actual_apply_ext = actual.apply_external;
            try testing.expectEqual(expected_apply_ext.module_idx, actual_apply_ext.module_idx);
            try testing.expectEqual(expected_apply_ext.target_node_idx, actual_apply_ext.target_node_idx);
            try testing.expectEqual(expected_apply_ext.args.span, actual_apply_ext.args.span);
        },
    }
}

fn expectEqualTypeAnnos(expected: []const ModuleEnv.TypeAnno, actual: []const ModuleEnv.TypeAnno) !void {
    try testing.expectEqual(expected.len, actual.len);
    for (expected, actual) |expected_item, actual_item| {
        try expectEqualTypeAnno(expected_item, actual_item);
    }
}

/// Helper to create a `DataSpan` from raw start and length positions.
fn rand_span() base.DataSpan {
    const start = rand.random().int(u32);
    const len = rand.random().int(u30); // Constrain len to fit within u30 (used by ImportRhs.num_exposes)
    return base.DataSpan{
        .start = start,
        .len = len,
    };
}

/// Generate a random identifier index.
fn rand_ident_idx() Ident.Idx {
    return @bitCast(rand.random().int(u32));
}

/// Helper to create a random region.
fn rand_region() base.Region {
    const start = rand.random().int(u32);
    var end = rand.random().int(u32);
    while (start > end) {
        end = rand.random().int(u32);
    }
    return from_raw_offsets(start, end);
}

test "NodeStore round trip - Statements" {
    const gpa = testing.allocator;
    var store = try NodeStore.init(gpa);
    defer store.deinit();

    var statements = std.ArrayList(ModuleEnv.Statement).init(gpa);
    defer statements.deinit();

    try statements.append(ModuleEnv.Statement{
        .s_decl = .{
            .pattern = rand_idx(ModuleEnv.Pattern.Idx),
            .expr = rand_idx(ModuleEnv.Expr.Idx),
        },
    });

    try statements.append(ModuleEnv.Statement{
        .s_var = .{
            .pattern_idx = rand_idx(ModuleEnv.Pattern.Idx),
            .expr = rand_idx(ModuleEnv.Expr.Idx),
        },
    });

    try statements.append(ModuleEnv.Statement{
        .s_reassign = .{
            .pattern_idx = rand_idx(ModuleEnv.Pattern.Idx),
            .expr = rand_idx(ModuleEnv.Expr.Idx),
        },
    });

    try statements.append(ModuleEnv.Statement{
        .s_expr = .{
            .expr = rand_idx(ModuleEnv.Expr.Idx),
        },
    });

    try statements.append(ModuleEnv.Statement{
        .s_crash = .{
            .msg = rand_idx(StringLiteral.Idx),
        },
    });

    try statements.append(ModuleEnv.Statement{
        .s_dbg = .{
            .expr = rand_idx(ModuleEnv.Expr.Idx),
        },
    });

    try statements.append(ModuleEnv.Statement{
        .s_expect = .{
            .body = rand_idx(ModuleEnv.Expr.Idx),
        },
    });

    try statements.append(ModuleEnv.Statement{
        .s_for = .{
            .patt = rand_idx(ModuleEnv.Pattern.Idx),
            .expr = rand_idx(ModuleEnv.Expr.Idx),
            .body = rand_idx(ModuleEnv.Expr.Idx),
        },
    });

    try statements.append(ModuleEnv.Statement{
        .s_return = .{
            .expr = rand_idx(ModuleEnv.Expr.Idx),
        },
    });

    try statements.append(ModuleEnv.Statement{
        .s_import = .{
            .module_name_tok = rand_ident_idx(),
            .qualifier_tok = rand_ident_idx(),
            .alias_tok = rand_ident_idx(),
            .exposes = ModuleEnv.ExposedItem.Span{ .span = rand_span() },
        },
    });

    try statements.append(ModuleEnv.Statement{
        .s_alias_decl = .{
            .header = rand_idx(ModuleEnv.TypeHeader.Idx),
            .anno = rand_idx(ModuleEnv.TypeAnno.Idx),
            .where = null,
        },
    });

    try statements.append(ModuleEnv.Statement{
        .s_nominal_decl = .{
            .header = rand_idx(ModuleEnv.TypeHeader.Idx),
            .anno = rand_idx(ModuleEnv.TypeAnno.Idx),
            .where = null,
        },
    });

    try statements.append(ModuleEnv.Statement{ .s_type_anno = .{
        .name = rand_ident_idx(),
        .anno = rand_idx(ModuleEnv.TypeAnno.Idx),
        .where = null,
    } });

    for (statements.items, 0..) |stmt, i| {
        const region = from_raw_offsets(@intCast(i * 100), @intCast(i * 100 + 50));
        const idx = try store.addStatement(stmt, region);
        const retrieved = store.getStatement(idx);

        try expectEqualStatements(stmt, retrieved);
    }

    const actual_test_count = statements.items.len;
    if (actual_test_count < NodeStore.MODULEENV_STATEMENT_NODE_COUNT) {
        std.debug.print("Statement test coverage insufficient! Need at least {d} test cases but found {d}.\n", .{ NodeStore.MODULEENV_STATEMENT_NODE_COUNT, actual_test_count });
        std.debug.print("Please add test cases for missing statement variants.\n", .{});
        return error.IncompleteStatementTestCoverage;
    }
}

test "NodeStore round trip - Expressions" {
    const gpa = testing.allocator;
    var store = try NodeStore.init(gpa);
    defer store.deinit();

    var expressions = std.ArrayList(ModuleEnv.Expr).init(gpa);
    defer expressions.deinit();

    try expressions.append(ModuleEnv.Expr{
        .e_int = .{
            .value = .{ .bytes = @bitCast(@as(i128, 42)), .kind = .i128 },
        },
    });
    try expressions.append(ModuleEnv.Expr{
        .e_frac_f64 = .{
            .value = 3.14,
        },
    });
    try expressions.append(ModuleEnv.Expr{
        .e_frac_dec = .{
            .value = ModuleEnv.RocDec{ .num = 314 },
        },
    });
    try expressions.append(ModuleEnv.Expr{
        .e_dec_small = .{
            .numerator = rand.random().int(i16),
            .denominator_power_of_ten = rand.random().int(u8),
        },
    });
    try expressions.append(ModuleEnv.Expr{
        .e_str_segment = .{
            .literal = rand_idx(StringLiteral.Idx),
        },
    });
    try expressions.append(ModuleEnv.Expr{
        .e_str = .{
            .span = ModuleEnv.Expr.Span{ .span = rand_span() },
        },
    });
    try expressions.append(ModuleEnv.Expr{
        .e_lookup_local = .{
            .pattern_idx = rand_idx(ModuleEnv.Pattern.Idx),
        },
    });
    try expressions.append(ModuleEnv.Expr{
        .e_lookup_external = .{
            .module_idx = rand_idx_u16(ModuleEnv.Import.Idx),
            .target_node_idx = rand.random().int(u16),
            .region = rand_region(),
        },
    });
    try expressions.append(ModuleEnv.Expr{
        .e_list = .{
            .elem_var = rand_idx(TypeVar),
            .elems = ModuleEnv.Expr.Span{ .span = rand_span() },
        },
    });
    try expressions.append(ModuleEnv.Expr{
        .e_tuple = .{
            .elems = ModuleEnv.Expr.Span{ .span = rand_span() },
        },
    });
    try expressions.append(ModuleEnv.Expr{
        .e_match = ModuleEnv.Expr.Match{
            .cond = rand_idx(ModuleEnv.Expr.Idx),
            .branches = ModuleEnv.Expr.Match.Branch.Span{ .span = rand_span() },
            .exhaustive = rand_idx(TypeVar),
        },
    });
    try expressions.append(ModuleEnv.Expr{
        .e_if = .{
            .branches = ModuleEnv.Expr.IfBranch.Span{ .span = rand_span() },
            .final_else = rand_idx(ModuleEnv.Expr.Idx),
        },
    });
    try expressions.append(ModuleEnv.Expr{
        .e_call = .{
            .args = ModuleEnv.Expr.Span{ .span = rand_span() },
            .called_via = CalledVia.apply,
        },
    });
    try expressions.append(ModuleEnv.Expr{
        .e_record = .{
            .fields = ModuleEnv.RecordField.Span{ .span = rand_span() },
            .ext = null,
        },
    });
    try expressions.append(ModuleEnv.Expr{
        .e_empty_list = .{},
    });
    try expressions.append(ModuleEnv.Expr{
        .e_block = .{
            .stmts = ModuleEnv.Statement.Span{ .span = rand_span() },
            .final_expr = rand_idx(ModuleEnv.Expr.Idx),
        },
    });
    try expressions.append(ModuleEnv.Expr{
        .e_tag = .{
            .name = rand_ident_idx(),
            .args = ModuleEnv.Expr.Span{ .span = rand_span() },
        },
    });
    try expressions.append(ModuleEnv.Expr{
        .e_nominal = .{
            .nominal_type_decl = rand_idx(ModuleEnv.Statement.Idx),
            .backing_expr = rand_idx(ModuleEnv.Expr.Idx),
            .backing_type = .tag,
        },
    });
    try expressions.append(ModuleEnv.Expr{
        .e_zero_argument_tag = .{
            .closure_name = rand_ident_idx(),
            .variant_var = rand_idx(TypeVar),
            .ext_var = rand_idx(TypeVar),
            .name = rand_ident_idx(),
        },
    });
    try expressions.append(ModuleEnv.Expr{
        .e_closure = .{
            .lambda_idx = rand_idx(ModuleEnv.Expr.Idx),
            .captures = ModuleEnv.Expr.Capture.Span{ .span = rand_span() },
        },
    });
    try expressions.append(ModuleEnv.Expr{
        .e_lambda = .{
            .args = ModuleEnv.Pattern.Span{ .span = rand_span() },
            .body = rand_idx(ModuleEnv.Expr.Idx),
        },
    });
    try expressions.append(ModuleEnv.Expr{
        .e_binop = ModuleEnv.Expr.Binop.init(
            .add,
            rand_idx(ModuleEnv.Expr.Idx),
            rand_idx(ModuleEnv.Expr.Idx),
        ),
    });
    try expressions.append(ModuleEnv.Expr{
        .e_unary_minus = ModuleEnv.Expr.UnaryMinus.init(rand_idx(ModuleEnv.Expr.Idx)),
    });
    try expressions.append(ModuleEnv.Expr{
        .e_dot_access = .{
            .receiver = rand_idx(ModuleEnv.Expr.Idx),
            .field_name = rand_ident_idx(),
            .args = null,
        },
    });
    try expressions.append(ModuleEnv.Expr{
        .e_runtime_error = .{
            .diagnostic = rand_idx(ModuleEnv.Diagnostic.Idx),
        },
    });
    try expressions.append(ModuleEnv.Expr{
        .e_crash = .{
            .msg = rand_idx(StringLiteral.Idx),
        },
    });
    try expressions.append(ModuleEnv.Expr{
        .e_dbg = .{
            .expr = rand_idx(ModuleEnv.Expr.Idx),
        },
    });
    try expressions.append(ModuleEnv.Expr{
        .e_empty_record = .{},
    });
    try expressions.append(ModuleEnv.Expr{
        .e_expect = .{
            .body = rand_idx(ModuleEnv.Expr.Idx),
        },
    });
    try expressions.append(ModuleEnv.Expr{
        .e_frac_dec = .{
            .value = ModuleEnv.RocDec{ .num = 123456789 },
        },
    });
    try expressions.append(ModuleEnv.Expr{
        .e_nominal_external = .{
            .module_idx = rand_idx_u16(ModuleEnv.Import.Idx),
            .target_node_idx = rand.random().int(u16),
            .backing_expr = rand_idx(ModuleEnv.Expr.Idx),
            .backing_type = .tag,
        },
    });
    try expressions.append(ModuleEnv.Expr{
        .e_ellipsis = .{},
    });

    for (expressions.items, 0..) |expr, i| {
        const region = from_raw_offsets(@intCast(i * 100), @intCast(i * 100 + 50));
        const idx = try store.addExpr(expr, region);
        const retrieved = store.getExpr(idx);

        try expectEqualExpressions(expr, retrieved);
    }

    const actual_test_count = expressions.items.len;
    if (actual_test_count < NodeStore.MODULEENV_EXPR_NODE_COUNT) {
        std.debug.print("Expression test coverage insufficient! Need at least {d} test cases but found {d}.\n", .{ NodeStore.MODULEENV_EXPR_NODE_COUNT, actual_test_count });
        std.debug.print("Please add test cases for missing expression variants.\n", .{});
        return error.IncompleteExpressionTestCoverage;
    }
}

test "NodeStore round trip - Diagnostics" {
    const gpa = testing.allocator;
    var store = try NodeStore.init(gpa);
    defer store.deinit();

    var diagnostics = std.ArrayList(ModuleEnv.Diagnostic).init(gpa);
    defer diagnostics.deinit();

    // Test all diagnostic types to ensure complete coverage
    try diagnostics.append(ModuleEnv.Diagnostic{
        .not_implemented = .{
            .feature = rand_idx(StringLiteral.Idx),
            .region = rand_region(),
        },
    });

    try diagnostics.append(ModuleEnv.Diagnostic{
        .invalid_num_literal = .{
            .region = rand_region(),
        },
    });

    try diagnostics.append(ModuleEnv.Diagnostic{
        .ident_already_in_scope = .{
            .ident = rand_ident_idx(),
            .region = rand_region(),
        },
    });

    try diagnostics.append(ModuleEnv.Diagnostic{
        .crash_expects_string = .{
            .region = rand_region(),
        },
    });

    try diagnostics.append(ModuleEnv.Diagnostic{
        .ident_not_in_scope = .{
            .ident = rand_ident_idx(),
            .region = rand_region(),
        },
    });

    try diagnostics.append(ModuleEnv.Diagnostic{
        .invalid_top_level_statement = .{
            .stmt = rand_idx(StringLiteral.Idx),
            .region = rand_region(),
        },
    });

    try diagnostics.append(ModuleEnv.Diagnostic{
        .expr_not_canonicalized = .{
            .region = rand_region(),
        },
    });

    try diagnostics.append(ModuleEnv.Diagnostic{
        .invalid_string_interpolation = .{
            .region = rand_region(),
        },
    });

    try diagnostics.append(ModuleEnv.Diagnostic{
        .pattern_arg_invalid = .{
            .region = rand_region(),
        },
    });

    try diagnostics.append(ModuleEnv.Diagnostic{
        .pattern_not_canonicalized = .{
            .region = rand_region(),
        },
    });

    try diagnostics.append(ModuleEnv.Diagnostic{
        .can_lambda_not_implemented = .{
            .region = rand_region(),
        },
    });

    try diagnostics.append(ModuleEnv.Diagnostic{
        .lambda_body_not_canonicalized = .{
            .region = rand_region(),
        },
    });

    try diagnostics.append(ModuleEnv.Diagnostic{
        .if_condition_not_canonicalized = .{
            .region = rand_region(),
        },
    });

    try diagnostics.append(ModuleEnv.Diagnostic{
        .if_then_not_canonicalized = .{
            .region = rand_region(),
        },
    });

    try diagnostics.append(ModuleEnv.Diagnostic{
        .if_else_not_canonicalized = .{
            .region = rand_region(),
        },
    });

    try diagnostics.append(ModuleEnv.Diagnostic{
        .var_across_function_boundary = .{
            .region = rand_region(),
        },
    });

    try diagnostics.append(ModuleEnv.Diagnostic{
        .shadowing_warning = .{
            .ident = rand_ident_idx(),
            .region = rand_region(),
            .original_region = rand_region(),
        },
    });

    try diagnostics.append(ModuleEnv.Diagnostic{
        .type_redeclared = .{
            .name = rand_ident_idx(),
            .redeclared_region = rand_region(),
            .original_region = rand_region(),
        },
    });

    try diagnostics.append(ModuleEnv.Diagnostic{
        .undeclared_type = .{
            .name = rand_ident_idx(),
            .region = rand_region(),
        },
    });

    try diagnostics.append(ModuleEnv.Diagnostic{
        .undeclared_type_var = .{
            .name = rand_ident_idx(),
            .region = rand_region(),
        },
    });

    try diagnostics.append(ModuleEnv.Diagnostic{
        .malformed_type_annotation = .{
            .region = rand_region(),
        },
    });

    try diagnostics.append(ModuleEnv.Diagnostic{
        .malformed_where_clause = .{
            .region = rand_region(),
        },
    });

    try diagnostics.append(ModuleEnv.Diagnostic{
        .unused_variable = .{
            .ident = rand_ident_idx(),
            .region = rand_region(),
        },
    });

    try diagnostics.append(ModuleEnv.Diagnostic{
        .used_underscore_variable = .{
            .ident = rand_ident_idx(),
            .region = rand_region(),
        },
    });

    try diagnostics.append(ModuleEnv.Diagnostic{
        .type_alias_redeclared = .{
            .name = rand_ident_idx(),
            .original_region = rand_region(),
            .redeclared_region = rand_region(),
        },
    });

    try diagnostics.append(ModuleEnv.Diagnostic{
        .nominal_type_redeclared = .{
            .name = rand_ident_idx(),
            .original_region = rand_region(),
            .redeclared_region = rand_region(),
        },
    });

    try diagnostics.append(ModuleEnv.Diagnostic{
        .type_shadowed_warning = .{
            .name = rand_ident_idx(),
            .region = rand_region(),
            .original_region = rand_region(),
            .cross_scope = rand.random().boolean(),
        },
    });

    try diagnostics.append(ModuleEnv.Diagnostic{
        .type_parameter_conflict = .{
            .name = rand_ident_idx(),
            .parameter_name = rand_ident_idx(),
            .region = rand_region(),
            .original_region = rand_region(),
        },
    });

    try diagnostics.append(ModuleEnv.Diagnostic{
        .duplicate_record_field = .{
            .field_name = rand_ident_idx(),
            .duplicate_region = rand_region(),
            .original_region = rand_region(),
        },
    });

    try diagnostics.append(ModuleEnv.Diagnostic{
        .invalid_single_quote = .{
            .region = rand_region(),
        },
    });

    try diagnostics.append(ModuleEnv.Diagnostic{
        .f64_pattern_literal = .{
            .region = rand_region(),
        },
    });

    try diagnostics.append(ModuleEnv.Diagnostic{
        .unused_type_var_name = .{
            .name = rand_ident_idx(),
            .suggested_name = rand_ident_idx(),
            .region = rand_region(),
        },
    });

    try diagnostics.append(ModuleEnv.Diagnostic{
        .type_var_marked_unused = .{
            .name = rand_ident_idx(),
            .suggested_name = rand_ident_idx(),
            .region = rand_region(),
        },
    });

    try diagnostics.append(ModuleEnv.Diagnostic{
        .type_var_ending_in_underscore = .{
            .name = rand_ident_idx(),
            .suggested_name = rand_ident_idx(),
            .region = rand_region(),
        },
    });

    try diagnostics.append(ModuleEnv.Diagnostic{
        .underscore_in_type_declaration = .{
            .is_alias = rand.random().boolean(),
            .region = rand_region(),
        },
    });

    try diagnostics.append(ModuleEnv.Diagnostic{
        .tuple_elem_not_canonicalized = .{
            .region = rand_region(),
        },
    });

    try diagnostics.append(ModuleEnv.Diagnostic{
        .empty_tuple = .{
            .region = rand_region(),
        },
    });

    try diagnostics.append(ModuleEnv.Diagnostic{
        .exposed_but_not_implemented = .{
            .ident = rand_ident_idx(),
            .region = rand_region(),
        },
    });

    try diagnostics.append(ModuleEnv.Diagnostic{
        .redundant_exposed = .{
            .ident = rand_ident_idx(),
            .region = rand_region(),
            .original_region = rand_region(),
        },
    });

    try diagnostics.append(ModuleEnv.Diagnostic{
        .module_not_found = .{
            .module_name = rand_ident_idx(),
            .region = rand_region(),
        },
    });

    try diagnostics.append(ModuleEnv.Diagnostic{
        .value_not_exposed = .{
            .module_name = rand_ident_idx(),
            .value_name = rand_ident_idx(),
            .region = rand_region(),
        },
    });

    try diagnostics.append(ModuleEnv.Diagnostic{
        .type_not_exposed = .{
            .module_name = rand_ident_idx(),
            .type_name = rand_ident_idx(),
            .region = rand_region(),
        },
    });

    try diagnostics.append(ModuleEnv.Diagnostic{
        .module_not_imported = .{
            .module_name = rand_ident_idx(),
            .region = rand_region(),
        },
    });

    try diagnostics.append(ModuleEnv.Diagnostic{
        .too_many_exports = .{
            .count = rand.random().int(u32),
            .region = rand_region(),
        },
    });

    // Test the round-trip for all diagnostics
    for (diagnostics.items) |diagnostic| {
        const idx = try store.addDiagnostic(diagnostic);
        const retrieved = store.getDiagnostic(idx);

        try expectEqualDiagnostics(diagnostic, retrieved);
    }

    const actual_test_count = diagnostics.items.len;
    if (actual_test_count < NodeStore.MODULEENV_DIAGNOSTIC_NODE_COUNT) {
        std.debug.print("Diagnostic test coverage insufficient! Need at least {d} test cases but found {d}.\n", .{ NodeStore.MODULEENV_DIAGNOSTIC_NODE_COUNT, actual_test_count });
        std.debug.print("Please add test cases for missing diagnostic variants.\n", .{});
        return error.IncompleteDiagnosticTestCoverage;
    }
}

test "NodeStore round trip - TypeAnno" {
    const gpa = testing.allocator;
    var store = try NodeStore.init(gpa);
    defer store.deinit();

    var type_annos = std.ArrayList(ModuleEnv.TypeAnno).init(gpa);
    defer type_annos.deinit();

    // Test all TypeAnno variants to ensure complete coverage
    try type_annos.append(ModuleEnv.TypeAnno{
        .apply = .{
            .symbol = rand_ident_idx(),
            .args = ModuleEnv.TypeAnno.Span{ .span = rand_span() },
        },
    });

    try type_annos.append(ModuleEnv.TypeAnno{
        .ty_var = .{
            .name = rand_ident_idx(),
        },
    });

    try type_annos.append(ModuleEnv.TypeAnno{
        .underscore = {},
    });

    try type_annos.append(ModuleEnv.TypeAnno{
        .ty = .{
            .symbol = rand_ident_idx(),
        },
    });

    try type_annos.append(ModuleEnv.TypeAnno{
        .ty = .{
            .symbol = rand_ident_idx(),
        },
    });

    try type_annos.append(ModuleEnv.TypeAnno{
        .tag_union = .{
            .tags = ModuleEnv.TypeAnno.Span{ .span = rand_span() },
            .ext = rand_idx(ModuleEnv.TypeAnno.Idx),
        },
    });

    try type_annos.append(ModuleEnv.TypeAnno{
        .tuple = .{
            .elems = ModuleEnv.TypeAnno.Span{ .span = rand_span() },
        },
    });

    try type_annos.append(ModuleEnv.TypeAnno{
        .record = .{
            .fields = ModuleEnv.TypeAnno.RecordField.Span{ .span = rand_span() },
        },
    });

    try type_annos.append(ModuleEnv.TypeAnno{
        .@"fn" = .{
            .args = ModuleEnv.TypeAnno.Span{ .span = rand_span() },
            .ret = rand_idx(ModuleEnv.TypeAnno.Idx),
            .effectful = rand.random().boolean(),
        },
    });

    try type_annos.append(ModuleEnv.TypeAnno{
        .parens = .{
            .anno = rand_idx(ModuleEnv.TypeAnno.Idx),
        },
    });

    try type_annos.append(ModuleEnv.TypeAnno{
        .ty = .{
            .symbol = rand_ident_idx(),
        },
    });

    try type_annos.append(ModuleEnv.TypeAnno{
        .ty_lookup_external = .{
            .module_idx = rand_idx(ModuleEnv.Import.Idx),
            .target_node_idx = rand.random().int(u16),
        },
    });

    try type_annos.append(ModuleEnv.TypeAnno{
        .malformed = .{
            .diagnostic = rand_idx(ModuleEnv.Diagnostic.Idx),
        },
    });

    // Test the round-trip for all type annotations
    for (type_annos.items, 0..) |type_anno, i| {
        const region = from_raw_offsets(@intCast(i * 100), @intCast(i * 100 + 50));
        const idx = try store.addTypeAnno(type_anno, region);
        const retrieved = store.getTypeAnno(idx);

        expectEqualTypeAnno(type_anno, retrieved) catch |err| {
            std.debug.print("\n\nOriginal:  {any}\n\n", .{type_anno});
            std.debug.print("Retrieved: {any}\n\n", .{retrieved});
            return err;
        };
    }

    const actual_test_count = type_annos.items.len;
    if (actual_test_count < NodeStore.MODULEENV_TYPE_ANNO_NODE_COUNT) {
        std.debug.print("ModuleEnv.TypeAnno test coverage insufficient! Need at least {d} test cases but found {d}.\n", .{ NodeStore.MODULEENV_TYPE_ANNO_NODE_COUNT, actual_test_count });
        std.debug.print("Please add test cases for missing type annotation variants.\n", .{});
        return error.IncompleteTypeAnnoTestCoverage;
    }
}

test "NodeStore round trip - Pattern" {
    const gpa = testing.allocator;
    var store = try NodeStore.init(gpa);
    defer store.deinit();

    var patterns = std.ArrayList(ModuleEnv.Pattern).init(gpa);
    defer patterns.deinit();

    // Test all Pattern variants to ensure complete coverage
    try patterns.append(ModuleEnv.Pattern{
        .assign = .{
            .ident = rand_ident_idx(),
        },
    });
    try patterns.append(ModuleEnv.Pattern{
        .as = .{
            .pattern = rand_idx(ModuleEnv.Pattern.Idx),
            .ident = rand_ident_idx(),
        },
    });
    try patterns.append(ModuleEnv.Pattern{
        .applied_tag = .{
            .name = rand_ident_idx(),
            .args = ModuleEnv.Pattern.Span{ .span = rand_span() },
        },
    });
    try patterns.append(ModuleEnv.Pattern{
        .nominal = .{
            .nominal_type_decl = rand_idx(ModuleEnv.Statement.Idx),
            .backing_pattern = rand_idx(ModuleEnv.Pattern.Idx),
            .backing_type = .tag,
        },
    });
    try patterns.append(ModuleEnv.Pattern{
        .nominal_external = .{
            .module_idx = rand_idx_u16(ModuleEnv.Import.Idx),
            .target_node_idx = rand.random().int(u16),
            .backing_pattern = rand_idx(ModuleEnv.Pattern.Idx),
            .backing_type = .tag,
        },
    });
    try patterns.append(ModuleEnv.Pattern{
        .record_destructure = .{
            .whole_var = rand_idx(TypeVar),
            .ext_var = rand_idx(TypeVar),
            .destructs = ModuleEnv.Pattern.RecordDestruct.Span{ .span = rand_span() },
        },
    });
    try patterns.append(ModuleEnv.Pattern{
        .list = .{
            .list_var = rand_idx(TypeVar),
            .elem_var = rand_idx(TypeVar),
            .patterns = ModuleEnv.Pattern.Span{ .span = rand_span() },
            .rest_info = .{ .index = rand.random().int(u32), .pattern = rand_idx(ModuleEnv.Pattern.Idx) },
        },
    });
    try patterns.append(ModuleEnv.Pattern{
        .tuple = .{
            .patterns = ModuleEnv.Pattern.Span{ .span = rand_span() },
        },
    });
    try patterns.append(ModuleEnv.Pattern{
        .int_literal = .{
            .value = ModuleEnv.IntValue{
                .bytes = @bitCast(rand.random().int(i128)),
                .kind = .i128,
            },
        },
    });
    try patterns.append(ModuleEnv.Pattern{
        .small_dec_literal = .{
            .numerator = rand.random().int(i16),
            .denominator_power_of_ten = rand.random().int(u8),
        },
    });
    try patterns.append(ModuleEnv.Pattern{
        .dec_literal = .{
            .value = RocDec.fromU64(rand.random().int(u64)),
        },
    });
    try patterns.append(ModuleEnv.Pattern{
        .str_literal = .{
            .literal = rand_idx(StringLiteral.Idx),
        },
    });
    try patterns.append(ModuleEnv.Pattern{ .underscore = {} });
    try patterns.append(ModuleEnv.Pattern{
        .runtime_error = .{
            .diagnostic = rand_idx(ModuleEnv.Diagnostic.Idx),
        },
    });

    // Test the round-trip for all patterns with their original regions
    var regions = [_]base.Region{undefined} ** NodeStore.MODULEENV_PATTERN_NODE_COUNT;
    for (&regions) |*region| {
        region.* = rand_region();
    }

    for (patterns.items, regions) |pattern, region| {
        const idx = try store.addPattern(pattern, region);
        const retrieved = store.getPattern(idx);

        try expectEqualPatterns(pattern, retrieved);

        // Also verify the region was stored correctly
        const stored_region = store.getRegionAt(@enumFromInt(@intFromEnum(idx)));
        testing.expectEqualDeep(region, stored_region) catch |err| {
            std.debug.print("\n\nExpected region: {any}\n\n", .{region});
            std.debug.print("Stored region: {any}\n\n", .{stored_region});
            return err;
        };
    }

    const actual_test_count = patterns.items.len;
    if (actual_test_count < NodeStore.MODULEENV_PATTERN_NODE_COUNT) {
        std.debug.print("ModuleEnv.Pattern test coverage insufficient! Need at least {d} test cases but found {d}.\n", .{ NodeStore.MODULEENV_PATTERN_NODE_COUNT, actual_test_count });
        std.debug.print("Please add test cases for missing pattern variants.\n", .{});
        return error.IncompletePatternTestCoverage;
    }
}
