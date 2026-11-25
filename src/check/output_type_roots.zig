//! Output type roots that inference produced (design.md "Row Union
//! Normalization"). Checked-module publication and the checker's settled row
//! walk both enumerate them through these functions, so every row the checked
//! module publishes is one the checker normalized. A visitor provides
//! `visit(Var) Allocator.Error!void` and
//! `visitRequired(?Var, comptime []const u8) Allocator.Error!void`; the latter
//! receives a var checking must have recorded, with the invariant it states.

const std = @import("std");
const invariant = @import("base").invariant;
const can = @import("can");
const types = @import("types");

const Allocator = std.mem.Allocator;
const ModuleEnv = can.ModuleEnv;
const CIR = can.CIR;
const Var = types.Var;

/// The dispatcher type var of a checked type-dispatch call.
pub fn typeDispatchCallDispatcherVar(module: anytype, owner: CIR.TypeDispatchOwner) Var {
    return switch (owner) {
        .statement => |stmt_idx| typeDispatchOwnerVar(module, stmt_idx),
        .dispatcher => |dispatcher| dispatcher,
    };
}

/// The type var that owns a type-dispatch call's statement.
pub fn typeDispatchOwnerVar(module: anytype, stmt_idx: CIR.Statement.Idx) Var {
    return switch (module.getStatement(stmt_idx)) {
        .s_type_var_alias => |alias| ModuleEnv.varFrom(alias.type_var_anno),
        .s_alias_decl => ModuleEnv.varFrom(stmt_idx),
        .s_decl,
        .s_var,
        .s_var_uninitialized,
        .s_reassign,
        .s_crash,
        .s_dbg,
        .s_expr,
        .s_expect,
        .s_for,
        .s_while,
        .s_infinite_loop,
        .s_breakable_loop,
        .s_break,
        .s_return,
        .s_import,
        .s_nominal_decl,
        .s_where_alias_decl,
        .s_type_anno,
        .s_runtime_error,
        => invariant("{s}", .{"type dispatch owner statement was not a type-var alias or type alias"}),
    };
}

/// Which of this file's per-expression enumerations a node of `tag` has roots
/// for.
pub const NodeTypeRoots = enum { none, call, static_dispatch };

/// The per-expression roots a node of `tag` publishes beyond its own type:
/// `forEachCallTypeRoot` for `.call` and `forEachStaticDispatchTypeRoot` for
/// `.static_dispatch`.
pub fn nodeTypeRoots(tag: CIR.Node.Tag) NodeTypeRoots {
    return switch (tag) {
        .expr_call, .expr_field_access => .call,
        .expr_dispatch_call,
        .expr_interpolation,
        .expr_type_dispatch_call,
        .expr_type_dispatch_call_dispatcher,
        .expr_method_eq,
        => .static_dispatch,
        .reserved,
        .statement_decl,
        .statement_var,
        .statement_var_uninitialized,
        .statement_reassign,
        .statement_crash,
        .statement_dbg,
        .statement_expr,
        .statement_expect,
        .statement_for,
        .statement_while,
        .statement_infinite_loop,
        .statement_breakable_loop,
        .statement_break,
        .statement_return,
        .statement_import,
        .statement_alias_decl,
        .statement_nominal_decl,
        .statement_where_alias_decl,
        .statement_type_anno,
        .statement_type_var_alias,
        .expr_var,
        .expr_tuple,
        .expr_tuple_access,
        .expr_list,
        .expr_empty_list,
        .expr_record,
        .expr_empty_record,
        .record_field,
        .record_unset_field,
        .record_destruct,
        .field_access_segment,
        .expr_method_call,
        .expr_structural_eq,
        .expr_structural_hash,
        .expr_type_method_call,
        .expr_static_dispatch,
        .expr_external_lookup,
        .expr_deferred_import_ref,
        .expr_deferred_nominal_external,
        .expr_associated_lookup_local,
        .expr_associated_lookup,
        .expr_associated_lookup_resolved,
        .expr_required_lookup,
        .expr_apply,
        .expr_string,
        .expr_string_segment,
        .expr_bytes_literal,
        .expr_num,
        .expr_frac_f32,
        .expr_frac_f64,
        .expr_dec,
        .expr_dec_small,
        .expr_num_from_numeral,
        .expr_typed_int,
        .expr_typed_frac,
        .expr_typed_num_from_numeral,
        .expr_tag,
        .expr_nominal,
        .expr_nominal_external,
        .expr_zero_argument_tag,
        .expr_closure,
        .expr_lambda,
        .expr_record_update,
        .expr_bin_op,
        .expr_unary_minus,
        .expr_suffix_single_question,
        .expr_if_then_else,
        .expr_match,
        .expr_dbg,
        .expr_crash,
        .expr_expect_err,
        .expr_block,
        .expr_ellipsis,
        .expr_anno_only,
        .expr_derived_method,
        .expr_hosted_lambda,
        .expr_low_level,
        .expr_run_low_level,
        .expr_expect,
        .expr_for,
        .expr_record_builder,
        .expr_return,
        .expr_break,
        .match_branch,
        .match_branch_pattern,
        .type_header,
        .annotation,
        .ty_apply,
        .ty_apply_external,
        .ty_rigid_var,
        .ty_rigid_var_lookup,
        .ty_lookup,
        .ty_underscore,
        .ty_tag_union,
        .ty_tag,
        .ty_tuple,
        .ty_record,
        .ty_record_field,
        .ty_record_field_defaulted,
        .ty_fn,
        .ty_parens,
        .ty_lookup_external,
        .ty_malformed,
        .where_method,
        .where_alias,
        .where_malformed,
        .pattern_identifier,
        .pattern_var_identifier,
        .pattern_as,
        .pattern_applied_tag,
        .pattern_nominal,
        .pattern_nominal_external,
        .pattern_deferred_import_ref,
        .pattern_record_destructure,
        .pattern_list,
        .pattern_tuple,
        .pattern_num_literal,
        .pattern_dec_literal,
        .pattern_num_from_numeral_literal,
        .pattern_f32_literal,
        .pattern_f64_literal,
        .pattern_small_dec_literal,
        .pattern_str_literal,
        .pattern_str_interpolation,
        .pattern_underscore,
        .lambda_capture,
        .def,
        .exposed_item,
        .if_branch,
        .type_var_slot,
        .malformed,
        .diag_not_implemented,
        .diag_invalid_num_literal,
        .diag_empty_single_quote,
        .diag_empty_tuple,
        .diag_ident_already_in_scope,
        .diag_ident_not_in_scope,
        .diag_read_uninitialized_var,
        .diag_self_referential_definition,
        .diag_circular_value_definition,
        .diag_local_reference_before_definition,
        .diag_mutually_recursive_local_definitions,
        .diag_erroneous_value_use,
        .diag_erroneous_value_expr,
        .diag_qualified_ident_does_not_exist,
        .diag_invalid_top_level_statement,
        .diag_invalid_associated_statement,
        .diag_expr_not_canonicalized,
        .diag_expr_syntax_error,
        .diag_unreachable_string_pattern_capture,
        .diag_pattern_arg_invalid,
        .diag_pattern_not_canonicalized,
        .diag_malformed_type_annotation,
        .diag_malformed_where_clause,
        .diag_where_clause_not_allowed_in_type_decl,
        .diag_where_alias_constraint_not_on_receiver,
        .diag_open_ext_not_allowed_in_type_decl,
        .diag_unnamed_field_not_allowed_in_structural_record,
        .diag_optional_field_cannot_have_default,
        .diag_record_default_reference_cycle,
        .diag_type_module_missing_matching_type,
        .diag_type_module_has_alias_not_nominal,
        .diag_default_app_missing_main,
        .diag_module_name_reserved,
        .diag_default_app_wrong_arity,
        .diag_cannot_import_default_app,
        .diag_type_name_case_mismatch,
        .diag_module_header_deprecated,
        .diag_roc_version_mismatch,
        .diag_redundant_expose_main_type,
        .diag_invalid_main_type_rename_in_exposing,
        .diag_var_across_function_boundary,
        .diag_shadowing_warning,
        .diag_binding_name_does_not_match_mutability,
        .diag_var_in_binder_pattern,
        .diag_type_redeclared,
        .diag_undeclared_type,
        .diag_undeclared_type_var,
        .diag_type_alias_but_needed_nominal,
        .diag_type_alias_redeclared,
        .diag_file_import_not_found,
        .diag_file_import_io_error,
        .diag_file_import_absolute_path,
        .diag_file_import_not_utf8,
        .diag_module_not_found,
        .diag_value_not_exposed,
        .diag_type_not_exposed,
        .diag_private_type_in_exposed_type,
        .diag_private_type_in_exposed_field,
        .diag_type_from_missing_module,
        .diag_module_not_imported,
        .diag_public_type_not_defined,
        .diag_nested_type_not_found,
        .diag_internal_builtin_type,
        .diag_nested_value_not_found,
        .diag_record_builder_map2_not_found,
        .diag_too_many_exports,
        .diag_nominal_type_redeclared,
        .diag_type_shadowed_warning,
        .diag_builtin_type_shadowed_warning,
        .diag_builtin_import_conflict,
        .diag_type_parameter_conflict,
        .diag_unused_variable,
        .diag_used_underscore_variable,
        .diag_duplicate_record_field,
        .diag_duplicate_pattern_binder,
        .diag_duplicate_tag,
        .diag_crash_expects_string,
        .diag_f64_pattern_literal,
        .diag_type_var_starting_with_dollar,
        .diag_underscore_in_type_declaration,
        .diagnostic_exposed_but_not_implemented,
        .diag_provided_value_is_required,
        .diag_redundant_exposed,
        .diag_if_expr_without_else,
        .diag_break_outside_loop,
        .diag_infinite_loop_never_exits,
        .diag_trailing_try_suffix,
        .diag_return_outside_fn,
        .diag_redundant_return,
        .diag_unreachable_code,
        .diag_control_flow_in_expect,
        .diag_var_reassigned_in_expect,
        .diag_mutually_recursive_type_aliases,
        .diag_deprecated_number_suffix,
        .diag_range_op_chained,
        .diag_unnamed_field_cannot_have_default,
        .diag_default_not_allowed_in_structural_record,
        .diag_default_not_allowed_on_local_type_decl,
        => .none,
    };
}

/// The roots a call or field-access expression publishes beyond its own type.
pub fn forEachCallTypeRoot(module_env: *const ModuleEnv, expr_idx: CIR.Expr.Idx, visitor: anytype) Allocator.Error!void {
    const kind = module_env.store.exprKind(expr_idx);
    if (kind == .e_call) {
        if (module_env.store.exprView(expr_idx).e_call.get(.constraint_fn_var)) |constraint_fn_var| try visitor.visit(constraint_fn_var);
    } else if (kind == .e_field_access) {
        const field_access = module_env.store.exprView(expr_idx).e_field_access.decode();
        var position: u32 = 0;
        while (position < field_access.segments.len) : (position += 1) {
            try visitor.visit(ModuleEnv.varFrom(module_env.store.fieldAccessSegmentAt(field_access.segments, position)));
        }
    }
}

/// The roots a statically dispatched expression publishes.
pub fn forEachStaticDispatchTypeRoot(module_env: *const ModuleEnv, expr_idx: CIR.Expr.Idx, visitor: anytype) Allocator.Error!void {
    const kind = module_env.store.exprKind(expr_idx);
    if (kind == .e_dispatch_call) {
        const dispatch_call = module_env.store.exprView(expr_idx).e_dispatch_call.decode();
        try visitor.visit(ModuleEnv.varFrom(dispatch_call.receiver));
        try visitor.visit(dispatch_call.constraint_fn_var);
    } else if (kind == .e_interpolation) {
        const interpolation = module_env.store.exprView(expr_idx).e_interpolation.decode();
        try visitor.visit(ModuleEnv.varFrom(expr_idx));
        // An interpolation introduced as the builtin `Str` has no dispatch
        // (design.md "Concrete Builtin Interpolation Introduction").
        if (interpolation.constraint_fn_var != null) {
            try visitor.visitRequired(interpolation.dispatcher_var, "checked interpolation expression had no static dispatch dispatcher type");
            try visitor.visitRequired(interpolation.constraint_fn_var, "checked interpolation expression had no static dispatch constraint type");
            try visitor.visitRequired(interpolation.assembler_fn_var, "checked interpolation expression had no assembler function type");
        }
    } else if (kind == .e_type_dispatch_call) {
        const type_dispatch_call = module_env.store.exprView(expr_idx).e_type_dispatch_call.decode();
        try visitor.visit(typeDispatchCallDispatcherVar(&module_env.store, type_dispatch_call.owner));
        try visitor.visit(type_dispatch_call.constraint_fn_var);
    } else if (kind == .e_method_eq) {
        const method_eq = module_env.store.exprView(expr_idx).e_method_eq.decode();
        try visitor.visit(ModuleEnv.varFrom(method_eq.lhs));
        try visitor.visit(method_eq.constraint_fn_var);
    }
}

/// The roots of recorded codec requirements, scheme uses, platform invocations,
/// and generated codec derivations.
pub fn forEachRecordedTypeRoot(module_env: *const ModuleEnv, visitor: anytype) Allocator.Error!void {
    for (module_env.platform_requirement_solutions.items()) |solution| {
        try forEachPlatformRequirementTypeRoot(solution.solved_var, module_env.platformRequirementIdentityVars(solution), visitor);
    }
    for (module_env.binding_scheme_codec_requirements.items()) |requirement| {
        const constraint = module_env.types.getStaticDispatchConstraintAt(requirement.constraint_index);
        try visitor.visit(@fromBackingInt(@intCast(requirement.receiver_var)));
        try visitor.visit(constraint.fn_var);
    }
    for (module_env.scheme_use_pairs.items()) |pair| {
        try visitor.visit(@fromBackingInt(@intCast(pair.fresh_var)));
    }
    for (module_env.generated_codec_derivations.items()) |derivation| {
        inline for (.{
            derivation.source_constraint_fn_var,
            derivation.source_runtime_fn_var,
            derivation.source_shape_var,
            derivation.source_body_shape_var,
            derivation.source_encoding_var,
            derivation.source_state_var,
            derivation.source_error_var,
            derivation.constraint_fn_var,
            derivation.runtime_fn_var,
            derivation.shape_var,
            derivation.body_shape_var,
            derivation.encoding_var,
            derivation.state_var,
            derivation.error_var,
        }) |raw_var| {
            try visitor.visit(@fromBackingInt(@intCast(raw_var)));
        }
        const calls = module_env.generated_codec_calls.items()[derivation.calls_start..][0..derivation.calls_len];
        for (calls) |call| {
            inline for (.{ call.dispatcher_var, call.callable_var, call.evidence_var }) |raw_var| {
                try visitor.visit(@fromBackingInt(@intCast(raw_var)));
            }
            if (call.subject_var.get()) |subject_var| {
                try visitor.visit(@fromBackingInt(@intCast(subject_var)));
            }
        }
        const renamings = module_env.generated_codec_renamings.items()[derivation.renamings_start..][0..derivation.renamings_len];
        for (renamings) |renaming| {
            try visitor.visit(@fromBackingInt(@intCast(renaming.copy_var)));
        }
    }
}

/// A platform invocation owns its solved type even when an empty substitution
/// and empty evidence vector need no scheme-use record.
pub fn forEachPlatformRequirementTypeRoot(solved: Var, identities: []const Var, visitor: anytype) Allocator.Error!void {
    try visitor.visit(solved);
    for (identities) |identity| try visitor.visit(identity);
}

/// The roots of every recorded scheme use: the instance a dispatch target,
/// where-method use, or stored nested function names, and each fresh var of
/// the use's substitution.
pub fn forEachSchemeUseTypeRoot(module_env: *const ModuleEnv, visitor: anytype) Allocator.Error!void {
    for (module_env.scheme_uses.items()) |record| {
        if (record.slot_kind == @backingInt(ModuleEnv.SchemeUseRecord.Slot.dispatch_target) or
            record.slot_kind == @backingInt(ModuleEnv.SchemeUseRecord.Slot.recursive_dispatch_target) or
            record.slot_kind == @backingInt(ModuleEnv.SchemeUseRecord.Slot.where_method_use) or
            record.slot_kind == @backingInt(ModuleEnv.SchemeUseRecord.Slot.platform_requirement) or
            record.slot_kind == @backingInt(ModuleEnv.SchemeUseRecord.Slot.nested_function_use))
        {
            try visitor.visit(@fromBackingInt(@intCast(record.slot_data)));
        }
        const pairs = module_env.scheme_use_pairs.items()[record.pairs_start .. record.pairs_start + record.pairs_len];
        for (pairs) |pair| {
            try visitor.visit(@fromBackingInt(@intCast(pair.fresh_var)));
        }
    }
}

/// The roots of a `for` loop's dispatch plan.
pub fn forEachForLoopDispatchTypeRoot(plan: ModuleEnv.ForLoopDispatchPlan, visitor: anytype) Allocator.Error!void {
    try visitor.visit(@fromBackingInt(@intCast(plan.iterator_var)));
    try visitor.visit(@fromBackingInt(@intCast(plan.step_var)));
    try visitor.visit(@fromBackingInt(@intCast(plan.iter_fn_var)));
    try visitor.visit(@fromBackingInt(@intCast(plan.next_fn_var)));
    try visitor.visit(@fromBackingInt(@intCast(plan.step_topology.one_payload_var)));
    try visitor.visit(@fromBackingInt(@intCast(plan.step_topology.skip_payload_var)));
}

/// The roots of every literal conversion that dispatches to a custom or
/// specialization-time callable: its target and callable, and a literal
/// pattern's equality callable. A direct builtin conversion needs neither.
pub fn forEachLiteralDispatchTypeRoot(module_env: *const ModuleEnv, visitor: anytype) Allocator.Error!void {
    for (module_env.store.literalDispatchPlans()) |plan| {
        switch (plan.dispatchResolution()) {
            .builtin_direct, .checked_error => continue,
            .custom_dispatch, .specialization_dispatch => {},
            .unresolved => {
                try visitor.visitRequired(null, "unresolved literal dispatch plan reached checked type publication");
                continue;
            },
        }
        try visitor.visit(@fromBackingInt(@intCast(plan.target_var)));
        try visitor.visit(plan.fnVar().?);
        if (plan.patternContext(&module_env.store)) |context| {
            try visitor.visit(context.equality_fn_var.get().?);
        }
    }
}
