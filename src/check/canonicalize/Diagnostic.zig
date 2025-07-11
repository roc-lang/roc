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
    exposed_but_not_implemented: struct {
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
    invalid_single_quote: struct {
        region: Region,
    },
    too_long_single_quote: struct {
        region: Region,
    },
    empty_single_quote: struct {
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
    invalid_top_level_statement: struct {
        stmt: StringLiteral.Idx,
        region: Region,
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
    if_condition_not_canonicalized: struct {
        region: Region,
    },
    if_then_not_canonicalized: struct {
        region: Region,
    },
    if_else_not_canonicalized: struct {
        region: Region,
    },
    malformed_type_annotation: struct {
        region: Region,
    },
    malformed_where_clause: struct {
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
    tuple_elem_not_canonicalized: struct {
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
    module_not_imported: struct {
        module_name: Ident.Idx,
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
    crash_expects_string: struct {
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
    duplicate_record_field: struct {
        field_name: Ident.Idx,
        duplicate_region: Region,
        original_region: Region,
    },
    f64_pattern_literal: struct {
        region: Region,
    },

    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: base.DataSpan };

    /// Helper to extract the region from any diagnostic variant
    pub fn toRegion(self: Diagnostic) Region {
        return switch (self) {
            .not_implemented => |d| d.region,
            .exposed_but_not_implemented => |d| d.region,
            .redundant_exposed => |d| d.region,
            .invalid_num_literal => |d| d.region,
            .ident_already_in_scope => |d| d.region,
            .ident_not_in_scope => |d| d.region,
            .invalid_top_level_statement => |d| d.region,
            .expr_not_canonicalized => |d| d.region,
            .invalid_string_interpolation => |d| d.region,
            .pattern_arg_invalid => |d| d.region,
            .pattern_not_canonicalized => |d| d.region,
            .can_lambda_not_implemented => |d| d.region,
            .lambda_body_not_canonicalized => |d| d.region,
            .if_condition_not_canonicalized => |d| d.region,
            .if_then_not_canonicalized => |d| d.region,
            .if_else_not_canonicalized => |d| d.region,
            .malformed_type_annotation => |d| d.region,
            .malformed_where_clause => |d| d.region,
            .var_across_function_boundary => |d| d.region,
            .shadowing_warning => |d| d.region,
            .type_redeclared => |d| d.redeclared_region,
            .tuple_elem_not_canonicalized => |d| d.region,
            .module_not_found => |d| d.region,
            .value_not_exposed => |d| d.region,
            .type_not_exposed => |d| d.region,
            .module_not_imported => |d| d.region,
            .too_many_exports => |d| d.region,
            .undeclared_type => |d| d.region,
            .undeclared_type_var => |d| d.region,
            .crash_expects_string => |d| d.region,
            .type_alias_redeclared => |d| d.redeclared_region,
            .nominal_type_redeclared => |d| d.redeclared_region,
            .type_shadowed_warning => |d| d.region,
            .type_parameter_conflict => |d| d.region,
            .unused_variable => |d| d.region,
            .used_underscore_variable => |d| d.region,
            .duplicate_record_field => |d| d.duplicate_region,
            .invalid_single_quote => |d| d.region,
            .too_long_single_quote => |d| d.region,
            .empty_single_quote => |d| d.region,
            .empty_tuple => |d| d.region,
            .f64_pattern_literal => |d| d.region,
        };
    }

    /// Build a report for "not implemented" diagnostic
    pub fn buildNotImplementedReport(allocator: Allocator, feature: []const u8) !Report {
        var report = Report.init(allocator, "NOT IMPLEMENTED", .fatal);
        const owned_feature = try report.addOwnedString(feature);
        try report.document.addReflowingText("This feature is not yet implemented or doesn't have a proper error report yet: ");
        try report.document.addText(owned_feature);
        try report.document.addLineBreak();
        try report.document.addReflowingText("Let us know if you want to help!");
        return report;
    }

    /// Build a report for "malformed where clause" diagnostic
    pub fn buildMalformedWhereClauseReport(allocator: Allocator) !Report {
        var report = Report.init(allocator, "MALFORMED WHERE CLAUSE", .runtime_error);
        try report.document.addReflowingText("This where clause could not be parsed correctly.");
        try report.document.addLineBreak();
        try report.document.addReflowingText("Please check the syntax of your where clause constraint.");
        return report;
    }

    /// Build a report for "invalid number literal" diagnostic
    pub fn buildInvalidNumLiteralReport(allocator: Allocator, region: Region, source: []const u8) !Report {
        var report = Report.init(allocator, "INVALID NUMBER", .runtime_error);

        // Extract the literal's text from the source using its region
        // const literal_text = source[region.start.offset..region.end.offset];

        try report.document.addText("This number literal is not valid: ");
        try report.document.addText(source[region.start.offset..region.end.offset]);
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

    /// Build a report for "exposed but not implemented" diagnostic
    pub fn buildExposedButNotImplementedReport(
        allocator: Allocator,
        ident_name: []const u8,
        region_info: base.RegionInfo,
        filename: []const u8,
    ) !Report {
        var report = Report.init(allocator, "EXPOSED BUT NOT DEFINED", .runtime_error);
        const owned_ident = try report.addOwnedString(ident_name);

        try report.document.addReflowingText("The module header says that `");
        try report.document.addInlineCode(owned_ident);
        try report.document.addReflowingText("` is exposed, but it is not defined anywhere in this module.");
        try report.document.addLineBreak();
        try report.document.addLineBreak();
        try report.document.addSourceRegion(
            region_info,
            .error_highlight,
            filename,
        );

        try report.document.addReflowingText("You can fix this by either defining `");
        try report.document.addInlineCode(owned_ident);
        try report.document.addReflowingText("` in this module, or by removing it from the list of exposed values.");

        return report;
    }

    /// Build a report for "redundant exposed" diagnostic
    pub fn buildRedundantExposedReport(
        allocator: Allocator,
        ident_name: []const u8,
        region_info: base.RegionInfo,
        original_region_info: base.RegionInfo,
        filename: []const u8,
    ) !Report {
        var report = Report.init(allocator, "REDUNDANT EXPOSED", .warning);
        const owned_ident = try report.addOwnedString(ident_name);

        try report.document.addReflowingText("The identifier `");
        try report.document.addInlineCode(owned_ident);
        try report.document.addReflowingText("` is exposed multiple times in the module header.");
        try report.document.addLineBreak();
        try report.document.addLineBreak();
        try report.document.addSourceRegion(
            region_info,
            .error_highlight,
            filename,
        );

        try report.document.addReflowingText("It was already exposed here:");

        try report.document.addSourceRegion(
            original_region_info,
            .dimmed,
            filename,
        );

        try report.document.addReflowingText("You can remove the duplicate entry to fix this warning.");

        return report;
    }

    /// Build a report for "identifier not in scope" diagnostic
    pub fn buildIdentNotInScopeReport(
        allocator: Allocator,
        ident_name: []const u8,
        region_info: base.RegionInfo,
        filename: []const u8,
    ) !Report {
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
        try report.document.addLineBreak();
        try report.document.addLineBreak();
        try report.document.addSourceRegion(
            region_info,
            .error_highlight,
            filename,
        );
        return report;
    }

    /// Build a report for "invalid top level statement" diagnostic
    pub fn buildInvalidTopLevelStatementReport(
        allocator: Allocator,
        stmt_name: []const u8,
        region_info: base.RegionInfo,
        filename: []const u8,
    ) !Report {
        var report = Report.init(allocator, "INVALID STATEMENT", .runtime_error);
        const owned_stmt = try report.addOwnedString(stmt_name);
        try report.document.addText("The statement ");
        try report.document.addAnnotated(owned_stmt, .emphasized);
        try report.document.addText(" is not allowed at the top level.");
        try report.document.addLineBreak();
        try report.document.addReflowingText("Only definitions, type annotations, and imports are allowed at the top level.");
        try report.document.addLineBreak();
        try report.document.addLineBreak();
        try report.document.addSourceRegion(
            region_info,
            .error_highlight,
            filename,
        );
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

    /// Build a report for "if condition not canonicalized" diagnostic
    pub fn buildIfConditionNotCanonicalizedReport(allocator: Allocator) !Report {
        var report = Report.init(allocator, "INVALID IF CONDITION", .runtime_error);
        try report.document.addReflowingText("The condition in this ");
        try report.document.addKeyword("if");
        try report.document.addReflowingText(" expression could not be processed.");
        try report.document.addLineBreak();
        try report.document.addLineBreak();
        try report.document.addReflowingText("The condition must be a valid expression that evaluates to a ");
        try report.document.addKeyword("Bool");
        try report.document.addReflowingText(" value (");
        try report.document.addKeyword("Bool.true");
        try report.document.addReflowingText(" or ");
        try report.document.addKeyword("Bool.false");
        try report.document.addReflowingText(").");
        return report;
    }

    /// Build a report for "if then not canonicalized" diagnostic
    pub fn buildIfThenNotCanonicalizedReport(allocator: Allocator) !Report {
        var report = Report.init(allocator, "INVALID IF BRANCH", .runtime_error);
        try report.document.addReflowingText("The ");
        try report.document.addKeyword("then");
        try report.document.addReflowingText(" branch of this ");
        try report.document.addKeyword("if");
        try report.document.addReflowingText(" expression could not be processed.");
        try report.document.addLineBreak();
        try report.document.addLineBreak();
        try report.document.addReflowingText("The ");
        try report.document.addKeyword("then");
        try report.document.addReflowingText(" branch must contain a valid expression. Check for syntax errors or missing values.");
        return report;
    }

    /// Build a report for "if else not canonicalized" diagnostic
    pub fn buildIfElseNotCanonicalizedReport(allocator: Allocator) !Report {
        var report = Report.init(allocator, "INVALID IF BRANCH", .runtime_error);
        try report.document.addReflowingText("The ");
        try report.document.addKeyword("else");
        try report.document.addReflowingText(" branch of this ");
        try report.document.addKeyword("if");
        try report.document.addReflowingText(" expression could not be processed.");
        try report.document.addLineBreak();
        try report.document.addLineBreak();
        try report.document.addReflowingText("The ");
        try report.document.addKeyword("else");
        try report.document.addReflowingText(" branch must contain a valid expression. Check for syntax errors or missing values.");
        try report.document.addLineBreak();
        try report.document.addLineBreak();
        try report.document.addReflowingText("Note: Every ");
        try report.document.addKeyword("if");
        try report.document.addReflowingText(" expression in Roc must have an ");
        try report.document.addKeyword("else");
        try report.document.addReflowingText(" branch, and both branches must have the same type.");
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

    /// Build a report for "invalid single quote" diagnostic
    pub fn buildInvalidSingleQuoteReport(
        allocator: Allocator,
    ) !Report {
        var report = Report.init(allocator, "INVALID SCALAR", .runtime_error);

        // Extract the literal's text from the source
        try report.document.addReflowingText("I am part way through parsing this scalar literal (character literal), but it appears to be invalid.");

        return report;
    }

    /// Build a report for "too long single quote" diagnostic
    pub fn buildTooLongSingleQuoteReport(
        allocator: Allocator,
    ) !Report {
        var report = Report.init(allocator, "INVALID SCALAR", .runtime_error);
        try report.document.addReflowingText("I am part way through parsing this scalar literal (character literal), but it contains more than one character.");
        try report.document.addLineBreak();
        try report.document.addReflowingText("A single-quoted literal must contain exactly one character, e.g. 'a'.");
        return report;
    }

    /// Build a report for "empty single quote" diagnostic
    pub fn buildEmptySingleQuoteReport(
        allocator: Allocator,
    ) !Report {
        var report = Report.init(allocator, "INVALID SCALAR", .runtime_error);
        try report.document.addReflowingText("I am part way through parsing this scalar literal (character literal), but it is empty.");
        try report.document.addLineBreak();
        try report.document.addReflowingText("A single-quoted literal must contain exactly one character, e.g. 'a'.");
        return report;
    }

    /// Build a report for "crash expects string" diagnostic
    pub fn buildCrashExpectsStringReport(
        allocator: Allocator,
        region_info: base.RegionInfo,
        filename: []const u8,
    ) !Report {
        var report = Report.init(allocator, "CRASH EXPECTS STRING", .runtime_error);
        try report.document.addReflowingText("The ");
        try report.document.addAnnotated("crash", .inline_code);
        try report.document.addReflowingText(" keyword expects a string literal as its argument.");
        try report.document.addLineBreak();
        try report.document.addReflowingText("For example: ");
        try report.document.addAnnotated("crash \"Something went wrong\"", .inline_code);
        try report.document.addLineBreak();
        try report.document.addSourceRegion(
            region_info,
            .error_highlight,
            filename,
        );
        return report;
    }

    /// Build a report for "empty single quote" diagnostic
    pub fn buildEmptyTupleReport(
        allocator: Allocator,
        region_info: base.RegionInfo,
        filename: []const u8,
    ) !Report {
        var report = Report.init(allocator, "EMPTY TUPLE NOT ALLOWED", .runtime_error);
        try report.document.addReflowingText("I am part way through parsing this tuple, but it is empty:");
        try report.document.addLineBreak();
        try report.document.addSourceRegion(
            region_info,
            .error_highlight,
            filename,
        );
        try report.document.addLineBreak();
        try report.document.addReflowingText("If you want to represent nothing, try using an empty record: ");
        try report.document.addAnnotated("{}", .inline_code);
        try report.document.addReflowingText(".");
        return report;
    }

    /// Build a report for "shadowing warning" diagnostic
    pub fn buildShadowingWarningReport(
        allocator: Allocator,
        ident_name: []const u8,
        new_region_info: base.RegionInfo,
        original_region_info: base.RegionInfo,
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
            new_region_info,
            .error_highlight,
            filename,
        );

        try report.document.addLineBreak();
        try report.document.addText("But ");
        try report.document.addUnqualifiedSymbol(owned_ident);
        try report.document.addText(" was already defined here:");
        try report.document.addLineBreak();
        try report.document.addSourceRegion(
            original_region_info,
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
            redeclared_region_info,
            .error_highlight,
            filename,
        );

        try report.document.addLineBreak();
        try report.document.addText("But `");
        try report.document.addUnqualifiedSymbol(owned_type_name);
        try report.document.addText("` was already declared here:");
        try report.document.addLineBreak();
        try report.document.addSourceRegion(
            original_region_info,
            .dimmed,
            filename,
        );

        return report;
    }

    /// Build a report for "malformed type annotation" diagnostic
    pub fn buildTupleElemNotCanonicalizedReport(allocator: Allocator) !Report {
        var report = Report.init(allocator, "INVALID TUPLE ELEMENT", .runtime_error);
        try report.document.addReflowingText("This tuple element is malformed or contains invalid syntax.");
        return report;
    }

    /// Build a report for "undeclared type" diagnostic
    pub fn buildUndeclaredTypeReport(
        allocator: Allocator,
        type_name: []const u8,
        region_info: base.RegionInfo,
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
            region_info,
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
            region_info,
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
            redeclared_region_info,
            .error_highlight,
            filename,
        );

        try report.document.addLineBreak();
        try report.document.addText("But `");
        try report.document.addUnqualifiedSymbol(owned_type_name);
        try report.document.addText("` was already declared here:");
        try report.document.addLineBreak();
        try report.document.addSourceRegion(
            original_region_info,
            .dimmed,
            filename,
        );

        return report;
    }

    /// Build a report for "nominal type redeclared" diagnostic
    pub fn buildNominalTypeRedeclaredReport(
        allocator: Allocator,
        type_name: []const u8,
        original_region_info: base.RegionInfo,
        redeclared_region_info: base.RegionInfo,
        filename: []const u8,
    ) !Report {
        var report = Report.init(allocator, "CUSTOM TYPE REDECLARED", .runtime_error);
        const owned_type_name = try report.addOwnedString(type_name);
        try report.document.addText("The nominal type `");
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
            redeclared_region_info,
            .error_highlight,
            filename,
        );

        try report.document.addLineBreak();
        try report.document.addText("But `");
        try report.document.addUnqualifiedSymbol(owned_type_name);
        try report.document.addText("` was already declared here:");
        try report.document.addLineBreak();
        try report.document.addSourceRegion(
            original_region_info,
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
            new_region_info,
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
            original_region_info,
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
            region_info,
            .error_highlight,
            filename,
        );

        try report.document.addLineBreak();
        try report.document.addText("But `");
        try report.document.addUnqualifiedSymbol(owned_parameter_name);
        try report.document.addText("` was already declared here:");
        try report.document.addLineBreak();
        try report.document.addSourceRegion(
            original_region_info,
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
            region_info,
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
            region_info,
            .error_highlight,
            filename,
        );

        return report;
    }

    pub fn buildDuplicateRecordFieldReport(
        allocator: Allocator,
        field_name: []const u8,
        duplicate_region_info: base.RegionInfo,
        original_region_info: base.RegionInfo,
        filename: []const u8,
    ) !Report {
        var report = Report.init(allocator, "DUPLICATE RECORD FIELD", .runtime_error);
        const owned_field_name = try report.addOwnedString(field_name);

        try report.document.addText("The record field `");
        try report.document.addUnqualifiedSymbol(owned_field_name);
        try report.document.addText("` appears more than once in this record.");
        try report.document.addLineBreak();
        try report.document.addLineBreak();

        // Show where the duplicate field is
        try report.document.addText("This field is duplicated here:");
        try report.document.addLineBreak();
        try report.document.addSourceRegion(
            duplicate_region_info,
            .error_highlight,
            filename,
        );

        try report.document.addLineBreak();
        try report.document.addText("The field `");
        try report.document.addUnqualifiedSymbol(owned_field_name);
        try report.document.addText("` was first defined here:");
        try report.document.addLineBreak();
        try report.document.addSourceRegion(
            original_region_info,
            .dimmed,
            filename,
        );

        try report.document.addLineBreak();
        try report.document.addText("Record fields must have unique names. Consider renaming one of these fields or removing the duplicate.");

        return report;
    }

    /// Build a report for "f64 pattern literal" diagnostic
    pub fn buildF64PatternLiteralReport(allocator: Allocator, region: Region, source: []const u8) !Report {
        var report = Report.init(allocator, "F64 NOT ALLOWED IN PATTERN", .runtime_error);

        // Extract the literal's text from the source using its region
        const literal_text = source[region.start.offset..region.end.offset];

        try report.document.addText("This floating-point literal cannot be used in a pattern match: ");
        try report.document.addInlineCode(literal_text);
        try report.document.addLineBreak();
        try report.document.addLineBreak();
        try report.document.addReflowingText("This number exceeds the precision range of Roc's ");
        try report.document.addInlineCode("Dec");
        try report.document.addReflowingText(" type and would require F64 representation. ");
        try report.document.addReflowingText("Floating-point numbers (F64) cannot be used in patterns because they don't have reliable equality comparison.");
        try report.document.addLineBreak();
        try report.document.addLineBreak();
        try report.document.addText("Consider one of these alternatives:");
        try report.document.addLineBreak();
        try report.document.addText("• Use a guard condition with a range check");
        try report.document.addLineBreak();
        try report.document.addText("• Use a smaller number that fits in Dec's precision");
        try report.document.addLineBreak();
        try report.document.addText("• Restructure your code to avoid pattern matching on this value");
        try report.document.addLineBreak();
        try report.document.addLineBreak();
        try report.document.addText("For example, instead of:");
        try report.document.addLineBreak();
        try report.document.addInlineCode("1e100 => ...");
        try report.document.addLineBreak();
        try report.document.addText("Use a guard:");
        try report.document.addLineBreak();
        try report.document.addInlineCode("n if n > 1e99 => ...");

        return report;
    }
    /// Build a report for "module not found" diagnostic
    pub fn buildModuleNotFoundReport(
        allocator: Allocator,
        module_name: []const u8,
        region_info: base.RegionInfo,
        filename: []const u8,
    ) !Report {
        var report = Report.init(allocator, "MODULE NOT FOUND", .runtime_error);

        const owned_module = try report.addOwnedString(module_name);
        try report.document.addText("The module ");
        try report.document.addAnnotated(owned_module, .module_name);
        try report.document.addText(" was not found.");
        try report.document.addLineBreak();
        try report.document.addReflowingText("Make sure this module is imported and available in your project.");

        try report.document.addSourceRegion(
            region_info,
            .error_highlight,
            filename,
        );

        return report;
    }

    /// Build a report for "value not exposed" diagnostic
    pub fn buildValueNotExposedReport(
        allocator: Allocator,
        module_name: []const u8,
        value_name: []const u8,
        region_info: base.RegionInfo,
        filename: []const u8,
    ) !Report {
        var report = Report.init(allocator, "VALUE NOT EXPOSED", .runtime_error);

        const owned_module = try report.addOwnedString(module_name);
        const owned_value = try report.addOwnedString(value_name);
        try report.document.addText("The ");
        try report.document.addAnnotated(owned_module, .module_name);
        try report.document.addText(" module does not expose anything named ");
        try report.document.addUnqualifiedSymbol(owned_value);
        try report.document.addText(".");
        try report.document.addLineBreak();
        try report.document.addReflowingText("Make sure the module exports this value, or use a value that is exposed.");

        try report.document.addSourceRegion(
            region_info,
            .error_highlight,
            filename,
        );

        return report;
    }

    /// Build a report for "type not exposed" diagnostic
    pub fn buildTypeNotExposedReport(
        allocator: Allocator,
        module_name: []const u8,
        type_name: []const u8,
        region_info: base.RegionInfo,
        filename: []const u8,
    ) !Report {
        var report = Report.init(allocator, "TYPE NOT EXPOSED", .runtime_error);

        const owned_module = try report.addOwnedString(module_name);
        const owned_type = try report.addOwnedString(type_name);
        try report.document.addText("The ");
        try report.document.addAnnotated(owned_module, .module_name);
        try report.document.addText(" module does not expose anything named ");
        try report.document.addAnnotated(owned_type, .emphasized);
        try report.document.addText(".");
        try report.document.addLineBreak();
        try report.document.addReflowingText("Make sure the module exports this type, or use a type that is exposed.");

        try report.document.addSourceRegion(
            region_info,
            .error_highlight,
            filename,
        );

        return report;
    }

    /// Build a report for "module not imported" diagnostic
    pub fn buildModuleNotImportedReport(
        allocator: Allocator,
        module_name: []const u8,
        region_info: base.RegionInfo,
        filename: []const u8,
    ) !Report {
        var report = Report.init(allocator, "MODULE NOT IMPORTED", .runtime_error);

        const owned_module = try report.addOwnedString(module_name);
        try report.document.addText("The module ");
        try report.document.addAnnotated(owned_module, .module_name);
        try report.document.addText(" is not imported in the current scope.");
        try report.document.addLineBreak();
        try report.document.addReflowingText("Try adding an import statement like: ");
        try report.document.addKeyword("import");
        try report.document.addText(" ");
        try report.document.addAnnotated(owned_module, .module_name);

        try report.document.addSourceRegion(
            region_info,
            .error_highlight,
            filename,
        );

        return report;
    }

    /// Build a report for "too many exports" diagnostic
    pub fn buildTooManyExportsReport(
        allocator: Allocator,
        count: u32,
        region_info: base.RegionInfo,
        filename: []const u8,
    ) !Report {
        var report = Report.init(allocator, "TOO MANY EXPORTS", .runtime_error);

        const max_exports = std.math.maxInt(u16);

        try report.document.addText("This module has ");
        const count_str = try std.fmt.allocPrint(allocator, "{}", .{count});
        defer allocator.free(count_str);
        try report.document.addAnnotated(count_str, .emphasized);
        try report.document.addText(" exports, but the maximum allowed is ");
        const max_str = try std.fmt.allocPrint(allocator, "{}", .{max_exports});
        defer allocator.free(max_str);
        try report.document.addAnnotated(max_str, .emphasized);
        try report.document.addText(".");
        try report.document.addLineBreak();
        try report.document.addReflowingText("Consider splitting this module into smaller modules with fewer exports.");

        try report.document.addSourceRegion(
            region_info,
            .error_highlight,
            filename,
        );

        return report;
    }
};
