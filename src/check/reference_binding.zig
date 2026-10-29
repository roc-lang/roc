//! Relocate foreign source locators using a producer-owned correspondence.
//! Type variables, local nodes, solver relationships and dispatch decisions do
//! not change. The mapper rejects any reference outside its declared domain.
const base = @import("base");
const can = @import("can");
const CIR = can.CIR;
const ModuleEnv = can.ModuleEnv;

/// Relocate all CIR and ModuleEnv source-reference storage, including nodes
/// retained as source provenance after checker replacement.
pub fn source(env: *ModuleEnv, mapper: anytype) @TypeOf(mapper.*).Error!void {
    inline for (.{ &env.store.nodes, &env.store.replaced_source_nodes }) |nodes| {
        for (0..nodes.len()) |i| {
            const id: CIR.Node.Idx = @fromBackingInt(@as(u32, @intCast(i)));
            var node = nodes.get(id);
            switch (node.tag) {
                inline .expr_external_lookup, .expr_nominal_external, .pattern_nominal_external => |tag| {
                    var payload = @field(node.getPayload(), @tagName(tag));
                    payload.target_node_idx = try mapper.importNode(env, payload.module_idx, payload.target_node_idx);
                    node.setPayload(@unionInit(CIR.Node.Payload, @tagName(tag), payload));
                },
                .expr_associated_lookup => {
                    var payload = node.getPayload().expr_associated_lookup;
                    payload.type_node_idx = try mapper.importNode(env, payload.module_idx, payload.type_node_idx);
                    node.setPayload(.{ .expr_associated_lookup = payload });
                },
                .expr_associated_lookup_resolved => {
                    var payload = node.getPayload().expr_associated_lookup_resolved;
                    const owner: base.ModuleIdentity.Idx = @fromBackingInt(payload.module_identity);
                    payload.target_node_idx = try mapper.node(owner, payload.target_node_idx);
                    payload.target_def_idx = try mapper.node(owner, payload.target_def_idx);
                    node.setPayload(.{ .expr_associated_lookup_resolved = payload });
                },
                // Annotation payloads share side records. Those are handled
                // below once, independently of live/replaced node aliases.
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
                .expr_call,
                .expr_record,
                .expr_empty_record,
                .record_field,
                .record_unset_field,
                .record_destruct,
                .expr_field_access,
                .field_access_segment,
                .expr_method_call,
                .expr_dispatch_call,
                .expr_interpolation,
                .expr_structural_eq,
                .expr_structural_hash,
                .expr_method_eq,
                .expr_type_method_call,
                .expr_type_dispatch_call,
                .expr_type_dispatch_call_dispatcher,
                .expr_static_dispatch,
                .expr_deferred_import_ref,
                .expr_deferred_nominal_external,
                .expr_associated_lookup_local,
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
                => {},
            }
            nodes.set(id, node);
        }
    }
    for (env.store.type_apply_data.items.items) |*data| {
        switch (@as(CIR.TypeAnno.LocalOrExternal.Tag, @fromBackingInt(@intCast(data.base_tag)))) {
            .external => data.value2 = try mapper.importNode(env, data.value1, data.value2),
            .external_identity => data.value2 = try mapper.node(@fromBackingInt(data.value1), data.value2),
            .builtin, .local, .pending => {},
        }
    }
    // A lookup's span2 record is shared by its retained source node, if any.
    var visited = @import("collections").DenseMap(enum(u32) { _ }, void).init(env.gpa);
    defer visited.deinit();
    inline for (.{ &env.store.nodes, &env.store.replaced_source_nodes }) |nodes| {
        for (0..nodes.len()) |i| {
            const node = nodes.get(@fromBackingInt(@as(u32, @intCast(i))));
            if (node.tag != .ty_lookup) continue;
            const payload = node.getPayload().ty_lookup;
            if ((try visited.getOrPut(@fromBackingInt(payload.base_span2_idx))).found_existing) continue;
            const data = &env.store.span2_data.items.items[payload.base_span2_idx];
            switch (@as(CIR.TypeAnno.LocalOrExternal.Tag, @fromBackingInt(@intCast(payload.base)))) {
                .external => data.len = try mapper.importNode(env, data.start, data.len),
                .external_identity => data.len = try mapper.node(@fromBackingInt(data.start), data.len),
                .builtin, .local, .pending => {},
            }
        }
    }
    for (env.numeric_suffix_targets.items.items) |*suffix| {
        switch (suffix.target()) {
            .external => |target| suffix.data2 = try mapper.importNode(env, @backingInt(target.import_idx), target.target_node_idx),
            .external_identity => |target| suffix.data2 = try mapper.node(target.module_identity, target.target_node_idx),
            .builtin, .local, .pending, .invalid => {},
        }
    }
    for (env.record_omitted_defaults.items.items) |*record| record.default_expr_node = try mapper.node(record.origin_module, record.default_expr_node);
    for (env.hosted_entries.items.items) |*entry| {
        if (entry.target_import) |import_idx| {
            if (entry.target_def) |def| entry.target_def = @fromBackingInt(try mapper.importNode(env, @backingInt(import_idx), @backingInt(def)));
        }
    }
    try env.types.rebindSourceReferences(mapper);
}
