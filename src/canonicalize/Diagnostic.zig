//! Diagnostics related to canonicalization

const std = @import("std");
const base = @import("base");
const reporting = @import("reporting");

const Region = base.Region;
const Ident = base.Ident;
const StringLiteral = base.StringLiteral;
const Report = reporting.Report;

const Allocator = std.mem.Allocator;

/// The kind of declaration a diagnostic is reported against, so its message can
/// name what the user actually wrote.
pub const DeclaredTypeKind = enum(u8) {
    alias,
    @"opaque",
    where_alias,
    nominal,

    /// The headline of an underscore diagnostic for this declaration.
    pub fn underscoreHeadline(self: DeclaredTypeKind) []const u8 {
        return switch (self) {
            .alias => "Underscores are not allowed in type alias declarations.",
            .where_alias => "Underscores are not allowed in where alias declarations.",
            .@"opaque" => "A bare underscore is not allowed in opaque type declarations.",
            .nominal => "A bare underscore is not allowed in nominal type declarations.",
        };
    }

    /// The title of an underscore diagnostic for this declaration.
    pub fn underscoreReportTitle(self: DeclaredTypeKind) []const u8 {
        return switch (self) {
            .alias, .where_alias => "Underscore In Type Alias",
            .@"opaque" => "Underscore In Opaque Type",
            .nominal => "Underscore In Nominal Type",
        };
    }
};

/// Public codec family that owns an otherwise unnameable builtin state type.
pub const InternalBuiltinTypeKind = enum(u8) {
    json,
    http_header,
};

/// Different types of diagnostic errors
pub const Diagnostic = union(enum) {
    pub const BindingMutability = enum(u1) {
        immutable,
        mutable,
    };

    not_implemented: struct {
        feature: StringLiteral.Idx,
        region: Region,
    },
    exposed_but_not_implemented: struct {
        ident: Ident.Idx,
        region: Region,
    },
    provided_value_is_required: struct {
        ident: Ident.Idx,
        region: Region,
    },
    redundant_exposed: struct {
        ident: Ident.Idx,
        region: Region,
        original_region: Region,
    },
    invalid_num_literal: struct {
        region: Region,
    },
    empty_tuple: struct {
        region: Region,
    },
    ident_already_in_scope: struct {
        ident: Ident.Idx,
        region: Region,
    },
    ident_not_in_scope: struct {
        ident: Ident.Idx,
        region: Region,
    },
    read_uninitialized_var: struct {
        ident: Ident.Idx,
        region: Region,
    },
    /// A non-function value is defined in terms of itself, which would cause an infinite loop.
    /// For example: `a = a` or `a = [a, b]`. Only functions can reference themselves (for recursion).
    self_referential_definition: struct {
        ident: Ident.Idx,
        region: Region,
    },
    /// A top-level non-function value participates in a recursive SCC.
    /// Only function values may be recursive.
    circular_value_definition: struct {
        ident: Ident.Idx,
        region: Region,
    },
    /// A local (block) definition references a name that is defined LATER in the
    /// same block. Local definitions are sequential: they may reference
    /// themselves or earlier definitions, but not later ones.
    local_reference_before_definition: struct {
        ident: Ident.Idx,
        region: Region,
    },
    /// Two local (block) definitions are mutually recursive, which is not
    /// supported for local definitions (only top-level definitions may be
    /// mutually recursive).
    mutually_recursive_local_definitions: struct {
        ident1: Ident.Idx,
        ident2: Ident.Idx,
        region: Region,
    },
    /// This use-site was rewritten to crash because the referenced top-level
    /// non-function value failed type checking earlier in the pipeline.
    erroneous_value_use: struct {
        ident: Ident.Idx,
        region: Region,
    },
    /// This expression was rewritten to crash because it failed type checking.
    erroneous_value_expr: struct {
        region: Region,
    },
    qualified_ident_does_not_exist: struct {
        ident: Ident.Idx, // The full qualified identifier (e.g., "Stdout.line!")
        region: Region,
    },
    invalid_top_level_statement: struct {
        stmt: StringLiteral.Idx,
        region: Region,
    },
    invalid_associated_statement: struct {
        stmt: StringLiteral.Idx,
        region: Region,
    },
    expr_not_canonicalized: struct {
        region: Region,
    },
    /// Range operators are non-associative: `a..<b..<c` is not allowed.
    range_op_chained: struct {
        region: Region,
    },
    /// This expression was replaced by a runtime error because it did not
    /// parse; the parser reports the syntax error itself.
    expr_syntax_error: struct {
        region: Region,
    },
    unreachable_string_pattern_capture: struct {
        region: Region,
    },
    pattern_arg_invalid: struct {
        region: Region,
    },
    pattern_not_canonicalized: struct {
        region: Region,
    },
    if_expr_without_else: struct {
        region: Region,
    },
    malformed_type_annotation: struct {
        region: Region,
    },
    malformed_where_clause: struct {
        region: Region,
    },
    where_clause_not_allowed_in_type_decl: struct {
        region: Region,
    },
    /// A where alias declaration constrains exactly its receiver, so a clause
    /// written against another type variable has no home.
    where_alias_constraint_not_on_receiver: struct {
        receiver_name: Ident.Idx,
        region: Region,
    },
    open_ext_not_allowed_in_type_decl: struct {
        region: Region,
    },
    unnamed_field_not_allowed_in_structural_record: struct {
        region: Region,
    },
    optional_field_cannot_have_default: struct {
        region: Region,
    },
    unnamed_field_cannot_have_default: struct {
        region: Region,
    },
    /// A `??` default declared outside a nominal type declaration's backing
    /// record: defaults are only legal on the direct fields of a nominal
    /// (`:=`) backing record, never in structural record types (type
    /// aliases, inline annotations, or nested records).
    default_not_allowed_in_structural_record: struct {
        region: Region,
    },
    /// A `??` default on a nominal (or opaque) type declaration inside a
    /// block: a local declaration's default canonicalizes in function scope,
    /// so it could capture locals that no other construction site can
    /// supply, and the end-of-module default-cycle pass only sees top-level
    /// declarations. Defaults are only legal on module top-level nominal
    /// declarations; the default is dropped and the field degrades to
    /// required.
    default_not_allowed_on_local_type_decl: struct {
        region: Region,
    },
    /// A `??` default whose materialization cycles back to itself through
    /// name-resolvable edges: references to same-module top-level defs
    /// and/or local nominal constructions that omit defaulted fields.
    /// Detected by the end-of-module default-cycle pass; dispatch-mediated
    /// cycles are the checker's residue (design.md "Defaulted Fields").
    record_default_reference_cycle: struct {
        field_name: Ident.Idx,
        region: Region,
    },
    var_across_function_boundary: struct {
        region: Region,
    },
    shadowing_warning: struct {
        ident: Ident.Idx,
        region: Region,
        original_region: Region,
    },
    binding_name_does_not_match_mutability: struct {
        ident: Ident.Idx,
        mutability: BindingMutability,
        region: Region,
    },
    type_redeclared: struct {
        name: Ident.Idx,
        original_region: Region,
        redeclared_region: Region,
    },
    file_import_not_found: struct {
        path: StringLiteral.Idx,
        region: Region,
    },
    file_import_io_error: struct {
        path: StringLiteral.Idx,
        region: Region,
    },
    file_import_absolute_path: struct {
        path: StringLiteral.Idx,
        region: Region,
    },
    file_import_not_utf8: struct {
        path: StringLiteral.Idx,
        region: Region,
    },
    module_not_found: struct {
        module_name: Ident.Idx,
        region: Region,
    },
    value_not_exposed: struct {
        module_name: Ident.Idx,
        value_name: Ident.Idx,
        region: Region,
    },
    type_not_exposed: struct {
        module_name: Ident.Idx,
        type_name: Ident.Idx,
        region: Region,
    },
    private_type_in_exposed_type: struct {
        exposed_type: Ident.Idx,
        private_type: Ident.Idx,
        region: Region,
    },
    private_type_in_exposed_field: struct {
        exposed_type: Ident.Idx,
        field_name: Ident.Idx,
        private_type: Ident.Idx,
        region: Region,
    },
    type_from_missing_module: struct {
        module_name: Ident.Idx,
        type_name: Ident.Idx,
        region: Region,
    },
    module_not_imported: struct {
        module_name: Ident.Idx,
        region: Region,
    },
    nested_type_not_found: struct {
        parent_name: Ident.Idx,
        nested_name: Ident.Idx,
        region: Region,
    },
    /// A nested builtin type that exists but is internal to the format module
    /// that owns it, so Roc code has no way to name it.
    internal_builtin_type: struct {
        parent_name: Ident.Idx,
        nested_name: Ident.Idx,
        kind: InternalBuiltinTypeKind,
        region: Region,
    },
    nested_value_not_found: struct {
        parent_name: Ident.Idx,
        nested_name: Ident.Idx,
        region: Region,
    },
    record_builder_map2_not_found: struct {
        type_name: Ident.Idx,
        region: Region,
    },
    too_many_exports: struct {
        count: u32,
        region: Region,
    },
    undeclared_type: struct {
        name: Ident.Idx,
        region: Region,
    },
    undeclared_type_var: struct {
        name: Ident.Idx,
        region: Region,
    },
    type_alias_but_needed_nominal: struct {
        name: Ident.Idx,
        region: Region,
    },
    crash_expects_string: struct {
        region: Region,
    },
    type_module_missing_matching_type: struct {
        module_name: Ident.Idx,
        region: Region,
    },
    type_module_has_alias_not_nominal: struct {
        module_name: Ident.Idx,
        region: Region,
    },
    default_app_missing_main: struct {
        module_name: Ident.Idx,
        region: Region,
    },
    default_app_wrong_arity: struct {
        arity: u32,
        region: Region,
    },
    cannot_import_default_app: struct {
        module_name: Ident.Idx,
        region: Region,
    },
    execution_requires_app_or_default_app: struct {
        region: Region,
    },
    type_name_case_mismatch: struct {
        module_name: Ident.Idx,
        type_name: Ident.Idx,
        region: Region,
    },
    module_header_deprecated: struct {
        region: Region,
    },
    /// The header pins a compiler version that is not the one running.
    roc_version_mismatch: struct {
        pinned: Ident.Idx,
        running: Ident.Idx,
        region: Region,
    },
    redundant_expose_main_type: struct {
        type_name: Ident.Idx,
        module_name: Ident.Idx,
        region: Region,
    },
    invalid_main_type_rename_in_exposing: struct {
        type_name: Ident.Idx,
        alias: Ident.Idx,
        region: Region,
    },
    type_alias_redeclared: struct {
        name: Ident.Idx,
        original_region: Region,
        redeclared_region: Region,
    },
    nominal_type_redeclared: struct {
        name: Ident.Idx,
        original_region: Region,
        redeclared_region: Region,
    },
    type_shadowed_warning: struct {
        name: Ident.Idx,
        region: Region,
        original_region: Region,
    },
    builtin_type_shadowed_warning: struct {
        name: Ident.Idx,
        region: Region,
    },
    type_parameter_conflict: struct {
        name: Ident.Idx,
        parameter_name: Ident.Idx,
        region: Region,
        original_region: Region,
    },
    unused_variable: struct {
        ident: Ident.Idx,
        region: Region,
    },
    used_underscore_variable: struct {
        ident: Ident.Idx,
        region: Region,
    },
    duplicate_record_field: struct {
        field_name: Ident.Idx,
        duplicate_region: Region,
        original_region: Region,
    },
    duplicate_pattern_binder: struct {
        ident: Ident.Idx,
        duplicate_region: Region,
        original_region: Region,
    },
    duplicate_tag: struct {
        tag_name: Ident.Idx,
        duplicate_region: Region,
        original_region: Region,
    },
    f64_pattern_literal: struct {
        region: Region,
    },
    underscore_in_type_declaration: struct {
        declared: DeclaredTypeKind,
        region: Region,
    },
    type_var_starting_with_dollar: struct {
        name: Ident.Idx,
        suggested_name: Ident.Idx,
        region: Region,
    },
    break_outside_loop: struct {
        region: Region,
    },
    infinite_loop_never_exits: struct {
        region: Region,
    },
    /// A `?` applied to the value a function returns: the final expression of
    /// the function body (looking through blocks, `if` branches, and `match`
    /// branches) or the operand of a `return`. Such a `?` unwraps the `Try` the
    /// function was about to return, which only type-checks for a nested `Try`
    /// and is almost always a mistake.
    trailing_try_suffix: struct {
        region: Region,
    },
    return_outside_fn: struct {
        region: Region,
        context: ReturnContext,

        pub const ReturnContext = enum(u8) {
            /// Explicit `return` statement
            return_statement,
            /// Return as final expression in a block
            return_expr,
            /// `?` suffix operator (try operator)
            try_suffix,
        };
    },
    /// A `return`, `break`, or `?` that would move control flow out of an
    /// `expect` body (outside any lambda nested within it). `?` is only reported
    /// in inline `expect`s; in top-level `expect`s it fails the test instead.
    control_flow_in_expect: struct {
        region: Region,
        kind: Kind,

        pub const Kind = enum(u8) {
            return_keyword,
            break_keyword,
            try_suffix,
        };
    },
    /// Reassigning a var declared outside the `expect` whose body (outside any
    /// lambda nested within it) contains the reassignment.
    var_reassigned_in_expect: struct {
        ident: Ident.Idx,
        region: Region,
        declaration_region: Region,
    },
    /// Two or more type aliases form a cycle where each references another.
    /// This is not allowed because type aliases are transparent synonyms.
    /// Use nominal types (:=) for recursive types.
    mutually_recursive_type_aliases: struct {
        name: Ident.Idx,
        other_name: Ident.Idx,
        region: Region,
        other_region: Region,
    },
    /// A number literal uses the deprecated suffix syntax (e.g., 123u64 instead of 123.U64)
    deprecated_number_suffix: struct {
        suffix: StringLiteral.Idx,
        suggested: StringLiteral.Idx,
        region: Region,
    },

    pub const Idx = enum(u32) { _ };
    pub const Span = extern struct { span: base.DataSpan };

    /// Helper to extract the region from any diagnostic variant
    pub fn toRegion(self: Diagnostic) Region {
        return switch (self) {
            .not_implemented => |d| d.region,
            .exposed_but_not_implemented => |d| d.region,
            .provided_value_is_required => |d| d.region,
            .redundant_exposed => |d| d.region,
            .invalid_num_literal => |d| d.region,
            .ident_already_in_scope => |d| d.region,
            .ident_not_in_scope => |d| d.region,
            .read_uninitialized_var => |d| d.region,
            .self_referential_definition => |d| d.region,
            .circular_value_definition => |d| d.region,
            .local_reference_before_definition => |d| d.region,
            .mutually_recursive_local_definitions => |d| d.region,
            .erroneous_value_use => |d| d.region,
            .erroneous_value_expr => |d| d.region,
            .qualified_ident_does_not_exist => |d| d.region,
            .invalid_top_level_statement => |d| d.region,
            .invalid_associated_statement => |d| d.region,
            .expr_not_canonicalized => |d| d.region,
            .expr_syntax_error => |d| d.region,
            .unreachable_string_pattern_capture => |d| d.region,
            .pattern_arg_invalid => |d| d.region,
            .pattern_not_canonicalized => |d| d.region,
            .if_expr_without_else => |d| d.region,
            .malformed_type_annotation => |d| d.region,
            .malformed_where_clause => |d| d.region,
            .where_clause_not_allowed_in_type_decl => |d| d.region,
            .where_alias_constraint_not_on_receiver => |d| d.region,
            .open_ext_not_allowed_in_type_decl => |d| d.region,
            .unnamed_field_not_allowed_in_structural_record => |d| d.region,
            .optional_field_cannot_have_default => |d| d.region,
            .unnamed_field_cannot_have_default => |d| d.region,
            .default_not_allowed_in_structural_record => |d| d.region,
            .default_not_allowed_on_local_type_decl => |d| d.region,
            .record_default_reference_cycle => |d| d.region,
            .var_across_function_boundary => |d| d.region,
            .shadowing_warning => |d| d.region,
            .binding_name_does_not_match_mutability => |d| d.region,
            .type_redeclared => |d| d.redeclared_region,
            .file_import_not_found => |d| d.region,
            .file_import_io_error => |d| d.region,
            .file_import_absolute_path => |d| d.region,
            .file_import_not_utf8 => |d| d.region,
            .module_not_found => |d| d.region,
            .value_not_exposed => |d| d.region,
            .type_not_exposed => |d| d.region,
            .private_type_in_exposed_type => |d| d.region,
            .private_type_in_exposed_field => |d| d.region,
            .type_from_missing_module => |d| d.region,
            .module_not_imported => |d| d.region,
            .nested_type_not_found => |d| d.region,
            .internal_builtin_type => |d| d.region,
            .nested_value_not_found => |d| d.region,
            .record_builder_map2_not_found => |d| d.region,
            .too_many_exports => |d| d.region,
            .undeclared_type => |d| d.region,
            .undeclared_type_var => |d| d.region,
            .type_alias_but_needed_nominal => |d| d.region,
            .crash_expects_string => |d| d.region,
            .type_module_missing_matching_type => |d| d.region,
            .type_module_has_alias_not_nominal => |d| d.region,
            .default_app_missing_main => |d| d.region,
            .default_app_wrong_arity => |d| d.region,
            .cannot_import_default_app => |d| d.region,
            .execution_requires_app_or_default_app => |d| d.region,
            .type_name_case_mismatch => |d| d.region,
            .module_header_deprecated => |d| d.region,
            .roc_version_mismatch => |d| d.region,
            .redundant_expose_main_type => |d| d.region,
            .invalid_main_type_rename_in_exposing => |d| d.region,
            .type_alias_redeclared => |d| d.redeclared_region,
            .nominal_type_redeclared => |d| d.redeclared_region,
            .type_shadowed_warning => |d| d.region,
            .builtin_type_shadowed_warning => |d| d.region,
            .type_parameter_conflict => |d| d.region,
            .unused_variable => |d| d.region,
            .used_underscore_variable => |d| d.region,
            .duplicate_record_field => |d| d.duplicate_region,
            .duplicate_pattern_binder => |d| d.duplicate_region,
            .duplicate_tag => |d| d.duplicate_region,
            .empty_tuple => |d| d.region,
            .f64_pattern_literal => |d| d.region,
            .type_var_starting_with_dollar => |d| d.region,
            .underscore_in_type_declaration => |d| d.region,
            .break_outside_loop => |d| d.region,
            .infinite_loop_never_exits => |d| d.region,
            .trailing_try_suffix => |d| d.region,
            .return_outside_fn => |d| d.region,
            .control_flow_in_expect => |d| d.region,
            .var_reassigned_in_expect => |d| d.region,
            .mutually_recursive_type_aliases => |d| d.region,
            .deprecated_number_suffix => |d| d.region,
            .range_op_chained => |d| d.region,
        };
    }

    /// Explain why a parsed root cannot supply an executable entrypoint.
    /// Shared by root preparation and canonicalization diagnostic rendering.
    pub fn buildExecutionRequiresAppOrDefaultAppReport(
        allocator: Allocator,
        region_info: base.RegionInfo,
        filename: []const u8,
        source: []const u8,
        line_starts: []const u32,
    ) Allocator.Error!Report {
        var report = try Report.init(allocator, "Execution Requires App Or Default App", "This file cannot be executed because it is not an app or default-app module.", .runtime_error);
        errdefer report.deinit();

        try report.document.addReflowingText("Add either:");
        try report.document.addLineBreak();
        try report.document.addInlineCode("app");
        try report.document.addReflowingText(" header at the top of the file");
        try report.document.addLineBreak();
        try report.document.addReflowingText("or:");
        try report.document.addLineBreak();
        try report.document.addReflowingText("a ");
        try report.document.addInlineCode("main!");
        try report.document.addReflowingText(" function with 1 argument (for default-app)");
        try report.document.addLineBreak();
        try report.document.addSourceRegion(region_info, .error_highlight, filename, source, line_starts);
        return report;
    }

    /// Build a report for shadowing a type from an outer lexical scope.
    pub fn buildTypeShadowedWarningReport(
        allocator: Allocator,
        type_name: []const u8,
        new_region_info: base.RegionInfo,
        original_region_info: base.RegionInfo,
        filename: []const u8,
        source: []const u8,
        line_starts: []const u32,
    ) Allocator.Error!Report {
        var report = try Report.init(allocator, "Type Shadowed", "", .warning);
        const owned_type_name = try report.addOwnedString(type_name);

        try report.headline.addText("The type ");
        try report.headline.addUnqualifiedSymbol(owned_type_name);
        try report.headline.addText(" shadows a type from an outer scope.");

        try report.document.addReflowingText("This may make the outer type inaccessible in this scope.");
        try report.document.addLineBreak();

        const owned_filename = try report.addOwnedString(filename);
        try report.document.addSourceRegion(
            new_region_info,
            .warning_highlight,
            owned_filename,
            source,
            line_starts,
        );
        try report.document.addLineBreak();
        try report.document.addText("The outer type was declared in ");
        try report.document.addSourceLocation(original_region_info, owned_filename);
        try report.document.addText(":");
        try report.document.addLineBreak();
        try report.document.addSourceRegion(
            original_region_info,
            .dimmed,
            owned_filename,
            source,
            line_starts,
        );

        return report;
    }

    /// Build a report for shadowing a compiler-owned builtin type.
    pub fn buildBuiltinTypeShadowedWarningReport(
        allocator: Allocator,
        type_name: []const u8,
        new_region_info: base.RegionInfo,
        filename: []const u8,
        source: []const u8,
        line_starts: []const u32,
    ) Allocator.Error!Report {
        var report = try Report.init(allocator, "Builtin Type Shadowed", "", .warning);
        const owned_type_name = try report.addOwnedString(type_name);

        try report.headline.addText("The type ");
        try report.headline.addUnqualifiedSymbol(owned_type_name);
        try report.headline.addText(" shadows a builtin type.");

        try report.document.addReflowingText("This may make the builtin type inaccessible in this scope.");
        try report.document.addLineBreak();

        const owned_filename = try report.addOwnedString(filename);
        try report.document.addSourceRegion(
            new_region_info,
            .warning_highlight,
            owned_filename,
            source,
            line_starts,
        );

        return report;
    }

    /// Build a report for "type parameter conflict" diagnostic
    pub fn buildTypeParameterConflictReport(
        allocator: Allocator,
        type_name: []const u8,
        parameter_name: []const u8,
        region_info: base.RegionInfo,
        original_region_info: base.RegionInfo,
        filename: []const u8,
        source: []const u8,
        line_starts: []const u32,
    ) Allocator.Error!Report {
        var report = try Report.init(allocator, "Type Parameter Conflict", "", .runtime_error);
        const owned_type_name = try report.addOwnedString(type_name);
        const owned_parameter_name = try report.addOwnedString(parameter_name);

        try report.headline.addText("The type parameter ");
        try report.headline.addUnqualifiedSymbol(owned_parameter_name);
        try report.headline.addText(" in type ");
        try report.headline.addUnqualifiedSymbol(owned_type_name);
        try report.headline.addText(" conflicts with another declaration.");
        try report.document.addReflowingText("Type parameters must have unique names within their scope.");
        try report.document.addLineBreak();
        try report.document.addLineBreak();

        const owned_filename = try report.addOwnedString(filename);
        try report.document.addSourceRegion(
            region_info,
            .error_highlight,
            owned_filename,
            source,
            line_starts,
        );

        try report.document.addLineBreak();
        try report.document.addText("But ");
        try report.document.addUnqualifiedSymbol(owned_parameter_name);
        try report.document.addText(" was already declared in ");
        try report.document.addSourceLocation(original_region_info, owned_filename);
        try report.document.addText(":");
        try report.document.addLineBreak();
        try report.document.addSourceRegion(
            original_region_info,
            .dimmed,
            owned_filename,
            source,
            line_starts,
        );

        return report;
    }

    pub fn buildDuplicateTagReport(
        allocator: Allocator,
        tag_name: []const u8,
        duplicate_region_info: base.RegionInfo,
        original_region_info: base.RegionInfo,
        filename: []const u8,
        source: []const u8,
        line_starts: []const u32,
    ) Allocator.Error!Report {
        var report = try Report.init(allocator, "Duplicate Tag", "", .runtime_error);
        const owned_tag_name = try report.addOwnedString(tag_name);

        try report.headline.addReflowingText("The tag ");
        try report.headline.addUnqualifiedSymbol(owned_tag_name);
        try report.headline.addReflowingText(" appears more than once in this tag union.");

        const owned_filename = try report.addOwnedString(filename);
        try report.document.addSourceRegion(
            duplicate_region_info,
            .error_highlight,
            owned_filename,
            source,
            line_starts,
        );

        try report.document.addLineBreak();
        try report.document.addReflowingText("The tag ");
        try report.document.addUnqualifiedSymbol(owned_tag_name);
        try report.document.addReflowingText(" was first defined in ");
        try report.document.addSourceLocation(original_region_info, owned_filename);
        try report.document.addReflowingText(":");
        try report.document.addLineBreak();
        try report.document.addSourceRegion(
            original_region_info,
            .dimmed,
            owned_filename,
            source,
            line_starts,
        );

        try report.document.addLineBreak();
        try report.document.addReflowingText("Tag union tags must have unique names. Rename one of these tags or remove the duplicate.");

        return report;
    }

    /// Build a report for "file import not found" diagnostic
    pub fn buildFileImportNotFoundReport(
        allocator: Allocator,
        path: []const u8,
        region_info: base.RegionInfo,
        filename: []const u8,
        source: []const u8,
        line_starts: []const u32,
    ) Allocator.Error!Report {
        var report = try Report.init(allocator, "File Not Found", "", .runtime_error);

        const owned_path = try report.addOwnedString(path);
        try report.headline.addReflowingText("The file ");
        try report.headline.addModuleName(owned_path);
        try report.headline.addReflowingText(" was not found.");

        try report.document.addReflowingText("Make sure the file exists relative to your source file.");
        try report.document.addLineBreak();

        const owned_filename = try report.addOwnedString(filename);
        try report.document.addSourceRegion(
            region_info,
            .error_highlight,
            owned_filename,
            source,
            line_starts,
        );

        return report;
    }

    /// Build a report for "file import IO error" diagnostic
    pub fn buildFileImportIOErrorReport(
        allocator: Allocator,
        path: []const u8,
        region_info: base.RegionInfo,
        filename: []const u8,
        source: []const u8,
        line_starts: []const u32,
    ) Allocator.Error!Report {
        var report = try Report.init(allocator, "File Import Error", "", .runtime_error);

        const owned_path = try report.addOwnedString(path);
        try report.headline.addReflowingText("Could not read the file ");
        try report.headline.addModuleName(owned_path);
        try report.headline.addReflowingText(".");

        try report.document.addReflowingText("An IO error occurred while trying to read this file:");
        try report.document.addLineBreak();

        const owned_filename = try report.addOwnedString(filename);
        try report.document.addSourceRegion(
            region_info,
            .error_highlight,
            owned_filename,
            source,
            line_starts,
        );

        return report;
    }

    /// Build a report for absolute file import paths.
    pub fn buildFileImportAbsolutePathReport(
        allocator: Allocator,
        path: []const u8,
        region_info: base.RegionInfo,
        filename: []const u8,
        source: []const u8,
        line_starts: []const u32,
    ) Allocator.Error!Report {
        var report = try Report.init(allocator, "Absolute File Import", "", .runtime_error);

        const owned_path = try report.addOwnedString(path);
        try report.document.addReflowingText("File imports must use a relative path, but this import uses ");
        try report.document.addModuleName(owned_path);
        try report.document.addReflowingText(".");
        try report.document.addLineBreak();
        try report.document.addLineBreak();
        try report.document.addReflowingText("Use a path relative to the source file instead.");
        try report.document.addLineBreak();

        const owned_filename = try report.addOwnedString(filename);
        try report.document.addSourceRegion(
            region_info,
            .error_highlight,
            owned_filename,
            source,
            line_starts,
        );

        return report;
    }

    /// Build a report for "file import not UTF-8" diagnostic
    pub fn buildFileImportNotUtf8Report(
        allocator: Allocator,
        path: []const u8,
        region_info: base.RegionInfo,
        filename: []const u8,
        source: []const u8,
        line_starts: []const u32,
    ) Allocator.Error!Report {
        var report = try Report.init(allocator, "File Not UTF-8", "", .runtime_error);

        const owned_path = try report.addOwnedString(path);
        try report.headline.addReflowingText("The file ");
        try report.headline.addModuleName(owned_path);
        try report.headline.addReflowingText(" is not valid UTF-8.");

        try report.document.addReflowingText("To import binary files, use `List(U8)` instead of `Str`.");
        try report.document.addLineBreak();

        const owned_filename = try report.addOwnedString(filename);
        try report.document.addSourceRegion(
            region_info,
            .error_highlight,
            owned_filename,
            source,
            line_starts,
        );

        return report;
    }

    /// Build a report for "type variable starting with dollar" diagnostic
    pub fn buildTypeVarStartingWithDollarReport(
        allocator: Allocator,
        type_var_name: []const u8,
        suggested_name: []const u8,
        region_info: base.RegionInfo,
        filename: []const u8,
        source: []const u8,
        line_starts: []const u32,
    ) Allocator.Error!Report {
        var report = try Report.init(allocator, "Type Variable Starting With Dollar", "", .warning);
        const owned_type_var_name = try report.addOwnedString(type_var_name);
        const owned_suggested_name = try report.addOwnedString(suggested_name);

        try report.headline.addReflowingText("The type variable ");
        try report.headline.addInlineCode(owned_type_var_name);
        try report.headline.addReflowingText(" starts with ");
        try report.headline.addInlineCode("$");
        try report.headline.addReflowingText(".");

        const owned_filename = try report.addOwnedString(filename);
        try report.document.addSourceRegion(
            region_info,
            .error_highlight,
            owned_filename,
            source,
            line_starts,
        );

        try report.document.addLineBreak();
        try report.document.addReflowingText("The ");
        try report.document.addInlineCode("$");
        try report.document.addReflowingText(" prefix is only for variables declared with ");
        try report.document.addKeyword("var");
        try report.document.addReflowingText(", and type variables can never be reassigned. Rename it to ");
        try report.document.addInlineCode(owned_suggested_name);
        try report.document.addReflowingText(" instead.");

        return report;
    }
};
