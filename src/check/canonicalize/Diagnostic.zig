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

    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: base.DataSpan };

    /// Build a report for "not implemented" diagnostic
    pub fn buildNotImplementedReport(allocator: Allocator, feature: []const u8, source: []const u8, region: Region) !Report {
        var report = Report.init(allocator, "NOT IMPLEMENTED", .runtime_error, reporting.ReportingConfig.initPlainText());
        try report.document.addText("This feature is not yet implemented: ");
        try report.document.addText(feature);
        try report.document.addLineBreak();
        try report.document.addLineBreak();

        try report.document.addSourceRegion(source, region.start.offset, region.start.offset, region.end.offset, region.end.offset, .error_highlight, null);

        try report.document.addLineBreak();
        try report.document.addReflowingText("This will be supported in a future version of Roc.");
        try report.document.addLineBreak();
        return report;
    }

    /// Build a report for "invalid number literal" diagnostic
    pub fn buildInvalidNumLiteralReport(allocator: Allocator, literal: []const u8, source: []const u8, region: Region) !Report {
        var report = Report.init(allocator, "INVALID NUMBER", .runtime_error, reporting.ReportingConfig.initPlainText());
        try report.document.addText("This number literal is not valid: ");
        try report.document.addText(literal);
        try report.document.addLineBreak();
        try report.document.addLineBreak();

        try report.document.addSourceRegion(source, region.start.offset, region.start.offset, region.end.offset, region.end.offset, .error_highlight, null);

        try report.document.addLineBreak();
        try report.document.addReflowingText("Roc supports integers, floats, and scientific notation. Check that the number format is correct.");
        try report.document.addLineBreak();
        return report;
    }

    /// Build a report for "identifier already in scope" diagnostic
    pub fn buildIdentAlreadyInScopeReport(allocator: Allocator, ident_name: []const u8, source: []const u8, region: Region) !Report {
        var report = Report.init(allocator, "DUPLICATE DEFINITION", .runtime_error, reporting.ReportingConfig.initPlainText());
        try report.document.addText("The name `");
        try report.document.addUnqualifiedSymbol(ident_name);
        try report.document.addText("` is already defined in this scope.");
        try report.document.addLineBreak();
        try report.document.addLineBreak();

        try report.document.addSourceRegion(source, region.start.offset, region.start.offset, region.end.offset, region.end.offset, .error_highlight, null);

        try report.document.addLineBreak();
        try report.document.addReflowingText("Choose a different name for this identifier, or remove the duplicate definition.");
        try report.document.addLineBreak();
        return report;
    }

    /// Build a report for "identifier not in scope" diagnostic
    pub fn buildIdentNotInScopeReport(allocator: Allocator, ident_name: []const u8, source: []const u8, region: Region) !Report {
        var report = Report.init(allocator, "UNDEFINED VARIABLE", .runtime_error, reporting.ReportingConfig.initPlainText());
        try report.document.addText("Nothing is named `");
        try report.document.addUnqualifiedSymbol(ident_name);
        try report.document.addText("` in this scope.");
        try report.document.addLineBreak();
        try report.document.addLineBreak();

        try report.document.addSourceRegion(source, region.start.offset, region.start.offset, region.end.offset, region.end.offset, .error_highlight, null);

        try report.document.addLineBreak();
        try report.document.addText("Is there an ");
        try report.document.addKeyword("import");
        try report.document.addText(" or ");
        try report.document.addKeyword("exposing");
        try report.document.addReflowingText(" missing up-top?");
        try report.document.addLineBreak();
        return report;
    }

    /// Build a report for "invalid top level statement" diagnostic
    pub fn buildInvalidTopLevelStatementReport(allocator: Allocator, source: []const u8, region: Region) !Report {
        var report = Report.init(allocator, "INVALID STATEMENT", .runtime_error, reporting.ReportingConfig.initPlainText());
        try report.document.addReflowingText("This statement is not allowed at the top level.");
        try report.document.addLineBreak();
        try report.document.addLineBreak();

        try report.document.addSourceRegion(source, region.start.offset, region.start.offset, region.end.offset, region.end.offset, .error_highlight, null);

        try report.document.addLineBreak();
        try report.document.addReflowingText("Only definitions, type annotations, and imports are allowed at the top level.");
        try report.document.addLineBreak();
        return report;
    }

    /// Build a report for "expression not canonicalized" diagnostic
    pub fn buildExprNotCanonicalizedReport(allocator: Allocator, source: []const u8, region: Region) !Report {
        var report = Report.init(allocator, "UNKNOWN OPERATOR", .runtime_error, reporting.ReportingConfig.initPlainText());
        try report.document.addReflowingText("This looks like an operator, but it's not one I recognize!");
        try report.document.addLineBreak();
        try report.document.addLineBreak();

        try report.document.addSourceRegion(source, region.start.offset, region.start.offset, region.end.offset, region.end.offset, .error_highlight, null);

        try report.document.addLineBreak();
        try report.document.addReflowingText("Check the spelling and make sure you're using a valid Roc operator.");
        try report.document.addLineBreak();
        return report;
    }

    /// Build a report for "invalid string interpolation" diagnostic
    pub fn buildInvalidStringInterpolationReport(allocator: Allocator, source: []const u8, region: Region) !Report {
        var report = Report.init(allocator, "INVALID INTERPOLATION", .runtime_error, reporting.ReportingConfig.initPlainText());
        try report.document.addReflowingText("This string interpolation is not valid.");
        try report.document.addLineBreak();
        try report.document.addLineBreak();

        try report.document.addSourceRegion(source, region.start.offset, region.start.offset, region.end.offset, region.end.offset, .error_highlight, null);

        try report.document.addLineBreak();
        try report.document.addReflowingText("String interpolation should use the format: \"text $(expression) more text\"");
        try report.document.addLineBreak();
        return report;
    }

    /// Build a report for "pattern argument invalid" diagnostic
    pub fn buildPatternArgInvalidReport(allocator: Allocator, source: []const u8, region: Region) !Report {
        var report = Report.init(allocator, "INVALID PATTERN", .runtime_error, reporting.ReportingConfig.initPlainText());
        try report.document.addReflowingText("This pattern argument is not valid.");
        try report.document.addLineBreak();
        try report.document.addLineBreak();

        try report.document.addSourceRegion(source, region.start.offset, region.start.offset, region.end.offset, region.end.offset, .error_highlight, null);

        try report.document.addLineBreak();
        try report.document.addReflowingText("Pattern arguments must be valid patterns like identifiers, literals, or destructuring patterns.");
        try report.document.addLineBreak();
        return report;
    }

    /// Build a report for "pattern not canonicalized" diagnostic
    pub fn buildPatternNotCanonicalizedReport(allocator: Allocator, source: []const u8, region: Region) !Report {
        var report = Report.init(allocator, "INVALID PATTERN", .runtime_error, reporting.ReportingConfig.initPlainText());
        try report.document.addReflowingText("This pattern could not be processed.");
        try report.document.addLineBreak();
        try report.document.addLineBreak();

        try report.document.addSourceRegion(source, region.start.offset, region.start.offset, region.end.offset, region.end.offset, .error_highlight, null);

        try report.document.addLineBreak();
        try report.document.addReflowingText("This pattern contains invalid syntax or uses unsupported features.");
        try report.document.addLineBreak();
        return report;
    }

    /// Build a report for "lambda not implemented" diagnostic
    pub fn buildCanLambdaNotImplementedReport(allocator: Allocator, source: []const u8, region: Region) !Report {
        var report = Report.init(allocator, "NOT IMPLEMENTED", .runtime_error, reporting.ReportingConfig.initPlainText());
        try report.document.addReflowingText("Lambda expressions are not yet fully implemented.");
        try report.document.addLineBreak();
        try report.document.addLineBreak();

        try report.document.addSourceRegion(source, region.start.offset, region.start.offset, region.end.offset, region.end.offset, .error_highlight, null);

        try report.document.addLineBreak();
        try report.document.addReflowingText("Lambda expressions will be supported in a future version of Roc.");
        try report.document.addLineBreak();
        return report;
    }

    /// Build a report for "lambda body not canonicalized" diagnostic
    pub fn buildLambdaBodyNotCanonicalizedReport(allocator: Allocator, source: []const u8, region: Region) !Report {
        var report = Report.init(allocator, "INVALID LAMBDA", .runtime_error, reporting.ReportingConfig.initPlainText());
        try report.document.addReflowingText("The body of this lambda expression is not valid.");
        try report.document.addLineBreak();
        try report.document.addLineBreak();

        try report.document.addSourceRegion(source, region.start.offset, region.start.offset, region.end.offset, region.end.offset, .error_highlight, null);

        try report.document.addLineBreak();
        try report.document.addReflowingText("The lambda body must be a valid expression.");
        try report.document.addLineBreak();
        return report;
    }
};
