//! Diagnostics related to canonicalization

const std = @import("std");
const base = @import("../../base.zig");
const reporting = @import("../../reporting.zig");

const Region = base.Region;
const Ident = base.Ident;
const StringLiteral = base.StringLiteral;
const Document = reporting.Document;
const Report = reporting.Report;
const Allocator = std.mem.Allocator;

/// Different types of diagnostic errors
pub const Diagnostic = union(enum) {
    not_implemented: struct {
        feature: StringLiteral.Idx,
        region: Region,
    },
    invalid_num_literal: struct {
        literal: StringLiteral.Idx,
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
    invalid_top_level_statement: struct {
        stmt: StringLiteral.Idx,
    },
    expr_not_canonicalized: struct {
        region: Region,
    },
    invalid_string_interpolation: struct {
        region: Region,
    },
    pattern_arg_invalid: struct {
        region: Region,
    },
    pattern_not_canonicalized: struct {
        region: Region,
    },
    can_lambda_not_implemented: struct {
        region: Region,
    },
    lambda_body_not_canonicalized: struct {
        region: Region,
    },
    malformed_type_annotation: struct {
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
    type_redeclared: struct {
        name: Ident.Idx,
        original_region: Region,
        redeclared_region: Region,
    },
    undeclared_type: struct {
        name: Ident.Idx,
        region: Region,
    },
    undeclared_type_var: struct {
        name: Ident.Idx,
        region: Region,
    },
    type_alias_redeclared: struct {
        name: Ident.Idx,
        original_region: Region,
        redeclared_region: Region,
    },
    custom_type_redeclared: struct {
        name: Ident.Idx,
        original_region: Region,
        redeclared_region: Region,
    },
    type_shadowed_warning: struct {
        name: Ident.Idx,
        region: Region,
        original_region: Region,
        cross_scope: bool,
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

    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: base.DataSpan };

    /// Helper to extract the region from any diagnostic variant
    pub fn toRegion(self: Diagnostic) Region {
        return switch (self) {
            .not_implemented => |d| d.region,
            .invalid_num_literal => |d| d.region,
            .ident_already_in_scope => |d| d.region,
            .ident_not_in_scope => |d| d.region,
            .invalid_top_level_statement => base.Region.zero(),
            .expr_not_canonicalized => |d| d.region,
            .invalid_string_interpolation => |d| d.region,
            .pattern_arg_invalid => |d| d.region,
            .pattern_not_canonicalized => |d| d.region,
            .can_lambda_not_implemented => |d| d.region,
            .lambda_body_not_canonicalized => |d| d.region,
            .malformed_type_annotation => |d| d.region,
            .var_across_function_boundary => |d| d.region,
            .shadowing_warning => |d| d.region,
            .type_redeclared => |d| d.redeclared_region,
            .undeclared_type => |d| d.region,
            .undeclared_type_var => |d| d.region,
            .type_alias_redeclared => |d| d.redeclared_region,
            .custom_type_redeclared => |d| d.redeclared_region,
            .type_shadowed_warning => |d| d.region,
            .type_parameter_conflict => |d| d.region,
            .unused_variable => |d| d.region,
            .used_underscore_variable => |d| d.region,
        };
    }

    /// Build a report for "not implemented" diagnostic
    pub fn buildNotImplementedReport(allocator: Allocator, feature: []const u8) !Report {
        var report = Report.init(allocator, "NOT IMPLEMENTED", .fatal);
        const owned_feature = try report.addOwnedString(feature);
        try report.document.addText("This feature is not yet implemented: ");
        try report.document.addText(owned_feature);
        return report;
    }

    /// Build a report for "invalid number literal" diagnostic
    pub fn buildInvalidNumLiteralReport(allocator: Allocator, literal: []const u8) !Report {
        var report = Report.init(allocator, "INVALID NUMBER", .runtime_error);
        const owned_literal = try report.addOwnedString(literal);
        try report.document.addText("This number literal is not valid: ");
        try report.document.addText(owned_literal);
        return report;
    }

    /// Build a report for "identifier already in scope" diagnostic
    pub fn buildIdentAlreadyInScopeReport(allocator: Allocator, ident_name: []const u8) !Report {
        var report = Report.init(allocator, "DUPLICATE DEFINITION", .warning);
        const owned_ident = try report.addOwnedString(ident_name);
        try report.document.addText("The name ");
        try report.document.addUnqualifiedSymbol(owned_ident);
        try report.document.addText(" is already defined in this scope.");
        try report.document.addLineBreak();
        try report.document.addReflowingText("Choose a different name for this identifier, or remove the duplicate definition.");
        return report;
    }

    /// Build a report for "identifier not in scope" diagnostic
    pub fn buildIdentNotInScopeReport(allocator: Allocator, ident_name: []const u8) !Report {
        var report = Report.init(allocator, "UNDEFINED VARIABLE", .runtime_error);
        const owned_ident = try report.addOwnedString(ident_name);
        try report.document.addText("Nothing is named ");
        try report.document.addUnqualifiedSymbol(owned_ident);
        try report.document.addText(" in this scope.");
        try report.document.addLineBreak();
        try report.document.addText("Is there an ");
        try report.document.addKeyword("import");
        try report.document.addText(" or ");
        try report.document.addKeyword("exposing");
        try report.document.addReflowingText(" missing up-top?");
        return report;
    }

    /// Build a report for "invalid top level statement" diagnostic
    pub fn buildInvalidTopLevelStatementReport(allocator: Allocator, stmt_name: []const u8) !Report {
        var report = Report.init(allocator, "INVALID STATEMENT", .runtime_error);
        const owned_stmt = try report.addOwnedString(stmt_name);
        try report.document.addText("The statement ");
        try report.document.addAnnotated(owned_stmt, .emphasized);
        try report.document.addText(" is not allowed at the top level.");
        try report.document.addLineBreak();
        try report.document.addReflowingText("Only definitions, type annotations, and imports are allowed at the top level.");
        return report;
    }

    /// Build a report for "expression not canonicalized" diagnostic
    pub fn buildExprNotCanonicalizedReport(allocator: Allocator) !Report {
        var report = Report.init(allocator, "UNKNOWN OPERATOR", .runtime_error);
        try report.document.addReflowingText("This looks like an operator, but it's not one I recognize!");
        try report.document.addLineBreak();
        try report.document.addReflowingText("Check the spelling and make sure you're using a valid Roc operator.");
        return report;
    }

    /// Build a report for "invalid string interpolation" diagnostic
    pub fn buildInvalidStringInterpolationReport(allocator: Allocator) !Report {
        var report = Report.init(allocator, "INVALID INTERPOLATION", .runtime_error);
        try report.document.addReflowingText("This string interpolation is not valid.");
        try report.document.addLineBreak();
        try report.document.addReflowingText("String interpolation should use the format: \"text $(expression) more text\"");
        return report;
    }

    /// Build a report for "pattern argument invalid" diagnostic
    pub fn buildPatternArgInvalidReport(allocator: Allocator) !Report {
        var report = Report.init(allocator, "INVALID PATTERN", .runtime_error);
        try report.document.addReflowingText("Pattern arguments must be valid patterns like identifiers, literals, or destructuring patterns.");
        return report;
    }

    /// Build a report for "pattern not canonicalized" diagnostic
    pub fn buildPatternNotCanonicalizedReport(allocator: Allocator) !Report {
        var report = Report.init(allocator, "INVALID PATTERN", .runtime_error);
        try report.document.addReflowingText("This pattern contains invalid syntax or uses unsupported features.");
        return report;
    }

    /// Build a report for "lambda not implemented" diagnostic
    pub fn buildCanLambdaNotImplementedReport(allocator: Allocator) !Report {
        var report = Report.init(allocator, "NOT IMPLEMENTED", .runtime_error);
        try report.document.addReflowingText("Lambda expressions are not yet fully implemented.");
        return report;
    }

    /// Build a report for "lambda body not canonicalized" diagnostic
    pub fn buildLambdaBodyNotCanonicalizedReport(allocator: Allocator) !Report {
        var report = Report.init(allocator, "INVALID LAMBDA", .runtime_error);
        try report.document.addReflowingText("The body of this lambda expression is not valid.");
        return report;
    }

    /// Build a report for "var across function boundary" diagnostic
    pub fn buildVarAcrossFunctionBoundaryReport(allocator: Allocator) !Report {
        var report = Report.init(allocator, "VAR REASSIGNMENT ERROR", .runtime_error);
        try report.document.addReflowingText("Cannot reassign a ");
        try report.document.addKeyword("var");
        try report.document.addReflowingText(" from outside the function where it was declared.");
        try report.document.addLineBreak();
        try report.document.addReflowingText("Variables declared with ");
        try report.document.addKeyword("var");
        try report.document.addReflowingText(" can only be reassigned within the same function scope.");
        return report;
    }

    /// Build a report for "malformed type annotation" diagnostic
    pub fn buildMalformedTypeAnnotationReport(allocator: Allocator) !Report {
        var report = Report.init(allocator, "MALFORMED TYPE", .runtime_error);
        try report.document.addReflowingText("This type annotation is malformed or contains invalid syntax.");
        return report;
    }

    /// Build a report for "shadowing warning" diagnostic
    pub fn buildShadowingWarningReport(
        allocator: Allocator,
        ident_name: []const u8,
        new_region_info: base.RegionInfo,
        original_region_info: base.RegionInfo,
        source: []const u8,
        filename: []const u8,
    ) !Report {
        var report = Report.init(allocator, "DUPLICATE DEFINITION", .warning);
        const owned_ident = try report.addOwnedString(ident_name);
        try report.document.addText("The name ");
        try report.document.addUnqualifiedSymbol(owned_ident);
        try report.document.addText(" is being redeclared in this scope.");
        try report.document.addLineBreak();
        try report.document.addLineBreak();

        // Show where the new declaration is
        try report.document.addText("The redeclaration is here:");
        try report.document.addLineBreak();
        try report.document.addSourceRegion(
            source,
            new_region_info.start_line_idx,
            new_region_info.start_col_idx,
            new_region_info.end_line_idx,
            new_region_info.end_col_idx,
            .error_highlight,
            filename,
        );

        try report.document.addLineBreak();
        try report.document.addText("But ");
        try report.document.addUnqualifiedSymbol(owned_ident);
        try report.document.addText(" was already defined here:");
        try report.document.addLineBreak();
        try report.document.addSourceRegion(
            source,
            original_region_info.start_line_idx,
            original_region_info.start_col_idx,
            original_region_info.end_line_idx,
            original_region_info.end_col_idx,
            .dimmed,
            filename,
        );

        return report;
    }

    /// Build a report for "type redeclared" diagnostic
    pub fn buildTypeRedeclaredReport(
        allocator: Allocator,
        type_name: []const u8,
        original_region_info: base.RegionInfo,
        redeclared_region_info: base.RegionInfo,
        source: []const u8,
        filename: []const u8,
    ) !Report {
        var report = Report.init(allocator, "TYPE REDECLARED", .runtime_error);
        const owned_type_name = try report.addOwnedString(type_name);
        try report.document.addText("The type `");
        try report.document.addUnqualifiedSymbol(owned_type_name);
        try report.document.addText("` is being redeclared.");
        try report.document.addLineBreak();
        try report.document.addLineBreak();

        // Show where the redeclaration is
        try report.document.addText("The redeclaration is here:");
        try report.document.addLineBreak();
        try report.document.addSourceRegion(
            source,
            redeclared_region_info.start_line_idx,
            redeclared_region_info.start_col_idx,
            redeclared_region_info.end_line_idx,
            redeclared_region_info.end_col_idx,
            .error_highlight,
            filename,
        );

        try report.document.addLineBreak();
        try report.document.addText("But `");
        try report.document.addUnqualifiedSymbol(owned_type_name);
        try report.document.addText("` was already declared here:");
        try report.document.addLineBreak();
        try report.document.addSourceRegion(
            source,
            original_region_info.start_line_idx,
            original_region_info.start_col_idx,
            original_region_info.end_line_idx,
            original_region_info.end_col_idx,
            .dimmed,
            filename,
        );

        return report;
    }

    /// Build a report for "undeclared type" diagnostic
    pub fn buildUndeclaredTypeReport(
        allocator: Allocator,
        type_name: []const u8,
        region_info: base.RegionInfo,
        source: []const u8,
        filename: []const u8,
    ) !Report {
        var report = Report.init(allocator, "UNDECLARED TYPE", .runtime_error);
        const owned_type_name = try report.addOwnedString(type_name);
        try report.document.addText("The type `");
        try report.document.addUnqualifiedSymbol(owned_type_name);
        try report.document.addText("` is not declared in this scope.");
        try report.document.addLineBreak();
        try report.document.addLineBreak();

        try report.document.addText("This type is referenced here:");
        try report.document.addLineBreak();
        try report.document.addSourceRegion(
            source,
            region_info.start_line_idx,
            region_info.start_col_idx,
            region_info.end_line_idx,
            region_info.end_col_idx,
            .error_highlight,
            filename,
        );

        return report;
    }

    /// Build a report for "undeclared type variable" diagnostic
    pub fn buildUndeclaredTypeVarReport(
        allocator: Allocator,
        type_var_name: []const u8,
        region_info: base.RegionInfo,
        source: []const u8,
        filename: []const u8,
    ) !Report {
        var report = Report.init(allocator, "UNDECLARED TYPE VARIABLE", .runtime_error);
        const owned_type_var_name = try report.addOwnedString(type_var_name);
        try report.document.addText("The type variable `");
        try report.document.addUnqualifiedSymbol(owned_type_var_name);
        try report.document.addText("` is not declared in this scope.");
        try report.document.addLineBreak();
        try report.document.addLineBreak();

        try report.document.addText("Type variables must be introduced in a type annotation before they can be used.");
        try report.document.addLineBreak();
        try report.document.addLineBreak();

        try report.document.addText("This type variable is referenced here:");
        try report.document.addLineBreak();
        try report.document.addSourceRegion(
            source,
            region_info.start_line_idx,
            region_info.start_col_idx,
            region_info.end_line_idx,
            region_info.end_col_idx,
            .error_highlight,
            filename,
        );

        return report;
    }

    /// Build a report for "type alias redeclared" diagnostic
    pub fn buildTypeAliasRedeclaredReport(
        allocator: Allocator,
        type_name: []const u8,
        original_region_info: base.RegionInfo,
        redeclared_region_info: base.RegionInfo,
        source: []const u8,
        filename: []const u8,
    ) !Report {
        var report = Report.init(allocator, "TYPE ALIAS REDECLARED", .runtime_error);
        const owned_type_name = try report.addOwnedString(type_name);
        try report.document.addText("The type alias `");
        try report.document.addUnqualifiedSymbol(owned_type_name);
        try report.document.addText("` is being redeclared.");
        try report.document.addLineBreak();
        try report.document.addReflowingText("Type aliases can only be declared once in the same scope.");
        try report.document.addLineBreak();
        try report.document.addLineBreak();

        // Show where the redeclaration is
        try report.document.addText("The redeclaration is here:");
        try report.document.addLineBreak();
        try report.document.addSourceRegion(
            source,
            redeclared_region_info.start_line_idx,
            redeclared_region_info.start_col_idx,
            redeclared_region_info.end_line_idx,
            redeclared_region_info.end_col_idx,
            .error_highlight,
            filename,
        );

        try report.document.addLineBreak();
        try report.document.addText("But `");
        try report.document.addUnqualifiedSymbol(owned_type_name);
        try report.document.addText("` was already declared here:");
        try report.document.addLineBreak();
        try report.document.addSourceRegion(
            source,
            original_region_info.start_line_idx,
            original_region_info.start_col_idx,
            original_region_info.end_line_idx,
            original_region_info.end_col_idx,
            .dimmed,
            filename,
        );

        return report;
    }

    /// Build a report for "custom type redeclared" diagnostic
    pub fn buildCustomTypeRedeclaredReport(
        allocator: Allocator,
        type_name: []const u8,
        original_region_info: base.RegionInfo,
        redeclared_region_info: base.RegionInfo,
        source: []const u8,
        filename: []const u8,
    ) !Report {
        var report = Report.init(allocator, "CUSTOM TYPE REDECLARED", .runtime_error);
        const owned_type_name = try report.addOwnedString(type_name);
        try report.document.addText("The custom type `");
        try report.document.addUnqualifiedSymbol(owned_type_name);
        try report.document.addText("` is being redeclared.");
        try report.document.addLineBreak();
        try report.document.addReflowingText("Custom types can only be declared once in the same scope.");
        try report.document.addLineBreak();
        try report.document.addLineBreak();

        // Show where the redeclaration is
        try report.document.addText("The redeclaration is here:");
        try report.document.addLineBreak();
        try report.document.addSourceRegion(
            source,
            redeclared_region_info.start_line_idx,
            redeclared_region_info.start_col_idx,
            redeclared_region_info.end_line_idx,
            redeclared_region_info.end_col_idx,
            .error_highlight,
            filename,
        );

        try report.document.addLineBreak();
        try report.document.addText("But `");
        try report.document.addUnqualifiedSymbol(owned_type_name);
        try report.document.addText("` was already declared here:");
        try report.document.addLineBreak();
        try report.document.addSourceRegion(
            source,
            original_region_info.start_line_idx,
            original_region_info.start_col_idx,
            original_region_info.end_line_idx,
            original_region_info.end_col_idx,
            .dimmed,
            filename,
        );

        return report;
    }

    /// Build a report for "type shadowed warning" diagnostic
    pub fn buildTypeShadowedWarningReport(
        allocator: Allocator,
        type_name: []const u8,
        new_region_info: base.RegionInfo,
        original_region_info: base.RegionInfo,
        cross_scope: bool,
        source: []const u8,
        filename: []const u8,
    ) !Report {
        const severity = if (cross_scope) reporting.Severity.warning else reporting.Severity.runtime_error;
        const title = if (cross_scope) "TYPE SHADOWED" else "TYPE DUPLICATE";

        var report = Report.init(allocator, title, severity);
        const owned_type_name = try report.addOwnedString(type_name);

        if (cross_scope) {
            try report.document.addText("The type `");
            try report.document.addUnqualifiedSymbol(owned_type_name);
            try report.document.addText("` shadows a type from an outer scope.");
            try report.document.addLineBreak();
            try report.document.addReflowingText("This may make the outer type inaccessible in this scope.");
        } else {
            try report.document.addText("The type `");
            try report.document.addUnqualifiedSymbol(owned_type_name);
            try report.document.addText("` is being redeclared in the same scope.");
        }

        try report.document.addLineBreak();
        try report.document.addLineBreak();

        // Show where the new declaration is
        try report.document.addText("The new declaration is here:");
        try report.document.addLineBreak();
        try report.document.addSourceRegion(
            source,
            new_region_info.start_line_idx,
            new_region_info.start_col_idx,
            new_region_info.end_line_idx,
            new_region_info.end_col_idx,
            .error_highlight,
            filename,
        );

        try report.document.addLineBreak();
        const scope_text = if (cross_scope) "outer scope" else "same scope";
        try report.document.addText("But `");
        try report.document.addUnqualifiedSymbol(owned_type_name);
        try report.document.addText("` was already declared in the ");
        try report.document.addText(scope_text);
        try report.document.addText(" here:");
        try report.document.addLineBreak();
        try report.document.addSourceRegion(
            source,
            original_region_info.start_line_idx,
            original_region_info.start_col_idx,
            original_region_info.end_line_idx,
            original_region_info.end_col_idx,
            .dimmed,
            filename,
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
        source: []const u8,
        filename: []const u8,
    ) !Report {
        var report = Report.init(allocator, "TYPE PARAMETER CONFLICT", .runtime_error);
        const owned_type_name = try report.addOwnedString(type_name);
        const owned_parameter_name = try report.addOwnedString(parameter_name);

        try report.document.addText("The type parameter `");
        try report.document.addUnqualifiedSymbol(owned_parameter_name);
        try report.document.addText("` in type `");
        try report.document.addUnqualifiedSymbol(owned_type_name);
        try report.document.addText("` conflicts with another declaration.");
        try report.document.addLineBreak();
        try report.document.addReflowingText("Type parameters must have unique names within their scope.");
        try report.document.addLineBreak();
        try report.document.addLineBreak();

        // Show where the conflict is
        try report.document.addText("The conflicting parameter is here:");
        try report.document.addLineBreak();
        try report.document.addSourceRegion(
            source,
            region_info.start_line_idx,
            region_info.start_col_idx,
            region_info.end_line_idx,
            region_info.end_col_idx,
            .error_highlight,
            filename,
        );

        try report.document.addLineBreak();
        try report.document.addText("But `");
        try report.document.addUnqualifiedSymbol(owned_parameter_name);
        try report.document.addText("` was already declared here:");
        try report.document.addLineBreak();
        try report.document.addSourceRegion(
            source,
            original_region_info.start_line_idx,
            original_region_info.start_col_idx,
            original_region_info.end_line_idx,
            original_region_info.end_col_idx,
            .dimmed,
            filename,
        );

        return report;
    }

    pub fn buildUnusedVariableReport(
        gpa: Allocator,
        ident_store: *const base.Ident.Store,
        region_info: base.RegionInfo,
        diagnostic: @TypeOf(@as(Diagnostic, undefined).unused_variable),
        source: []const u8,
        filename: []const u8,
    ) !Report {
        const ident_name = ident_store.getText(diagnostic.ident);

        var report = Report.init(gpa, "UNUSED VARIABLE", .warning);
        const owned_ident = try report.addOwnedString(ident_name);

        try report.document.addText("Variable `");
        try report.document.addUnqualifiedSymbol(owned_ident);
        try report.document.addText("` is not used anywhere in your code.");
        try report.document.addLineBreak();
        try report.document.addLineBreak();

        try report.document.addText("If you don't need this variable, prefix it with an underscore like `_");
        try report.document.addText(owned_ident);
        try report.document.addText("` to suppress this warning.");

        try report.document.addLineBreak();
        try report.document.addText("The unused variable is declared here:");
        try report.document.addLineBreak();

        try report.document.addSourceRegion(
            source,
            region_info.start_line_idx,
            region_info.start_col_idx,
            region_info.end_line_idx,
            region_info.end_col_idx,
            .error_highlight,
            filename,
        );

        return report;
    }

    pub fn buildUsedUnderscoreVariableReport(
        gpa: Allocator,
        ident_store: *const base.Ident.Store,
        region_info: base.RegionInfo,
        diagnostic: @TypeOf(@as(Diagnostic, undefined).used_underscore_variable),
        source: []const u8,
        filename: []const u8,
    ) !Report {
        const ident_name = ident_store.getText(diagnostic.ident);

        var report = Report.init(gpa, "UNDERSCORE VARIABLE USED", .warning);
        const owned_ident = try report.addOwnedString(ident_name);

        try report.document.addText("Variable `");
        try report.document.addUnqualifiedSymbol(owned_ident);
        try report.document.addText("` is prefixed with an underscore but is actually used.");
        try report.document.addLineBreak();
        try report.document.addLineBreak();

        try report.document.addText("Variables prefixed with `_` are intended to be unused. Remove the underscore prefix: `");
        const name_without_underscore = if (std.mem.startsWith(u8, ident_name, "_")) ident_name[1..] else ident_name;
        const owned_name_without_underscore = try report.addOwnedString(name_without_underscore);
        try report.document.addText(owned_name_without_underscore);
        try report.document.addText("`.");

        try report.document.addLineBreak();
        try report.document.addText("The underscore variable is declared here:");
        try report.document.addLineBreak();

        try report.document.addSourceRegion(
            source,
            region_info.start_line_idx,
            region_info.start_col_idx,
            region_info.end_line_idx,
            region_info.end_col_idx,
            .error_highlight,
            filename,
        );

        return report;
    }
};
