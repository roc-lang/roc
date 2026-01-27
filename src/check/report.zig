//! Generate Reports for type checking errors
//!
//! This module provides ReportBuilder for rendering type errors to user-friendly
//! messages. Problem data types are defined in problem/types.zig and re-exported here.

const std = @import("std");
const base = @import("base");
const tracy = @import("tracy");
const types_mod = @import("types");
const can = @import("can");
const reporting = @import("reporting");

const snapshot = @import("snapshot.zig");
const diff = @import("diff.zig");

// Import problem types from the problem/ submodule
const problem_mod = @import("problem.zig");

const Region = base.Region;

const Allocator = std.mem.Allocator;

const CIR = can.CIR;
const ModuleEnv = can.ModuleEnv;

const Report = reporting.Report;
const UnderlineRegion = reporting.UnderlineRegion;
const SourceCodeDisplayRegion = reporting.SourceCodeDisplayRegion;

const Ident = base.Ident;

const SnapshotContentIdx = snapshot.SnapshotContentIdx;
const SnapshotRecordFieldSafeList = snapshot.SnapshotRecordFieldSafeList;
const SnapshotRecordField = snapshot.SnapshotRecordField;
const SnapshotTagSafeList = snapshot.SnapshotTagSafeList;

const ByteList = std.array_list.Managed(u8);
const ByteListRange = struct { start: usize, count: usize };

const Var = types_mod.Var;

const Problem = problem_mod.Problem;
const Store = problem_mod.Store;
const ExtraStringIdx = problem_mod.ExtraStringIdx;
const MissingPatternsRange = problem_mod.MissingPatternsRange;

// Type mismatch types
const TypeMismatch = problem_mod.TypeMismatch;
const TypePair = problem_mod.TypePair;
const IncompatiblePlatformRequirement = problem_mod.IncompatiblePlatformRequirement;

// Function errors
const FnCallArityMismatch = problem_mod.FnCallArityMismatch;

// Static dispatch errors
const StaticDispatch = problem_mod.StaticDispatch;
const DispatcherNotNominal = problem_mod.DispatcherNotNominal;
const DispatcherDoesNotImplMethod = problem_mod.DispatcherDoesNotImplMethod;
const TypeDoesNotSupportEquality = problem_mod.TypeDoesNotSupportEquality;

// Number errors
const NumberDoesNotFit = problem_mod.NumberDoesNotFit;
const NegativeUnsignedInt = problem_mod.NegativeUnsignedInt;
const InvalidNumericLiteral = problem_mod.InvalidNumericLiteral;
const UnusedValue = problem_mod.UnusedValue;

// Match/exhaustiveness errors
const NonExhaustiveMatch = problem_mod.NonExhaustiveMatch;
const RedundantPattern = problem_mod.RedundantPattern;
const UnmatchablePattern = problem_mod.UnmatchablePattern;

// Type declaration errors
const TypeApplyArityMismatch = problem_mod.TypeApplyArityMismatch;
const RecursiveAlias = problem_mod.RecursiveAlias;
const UnsupportedAliasWhereClause = problem_mod.UnsupportedAliasWhereClause;

// Nominal type errors
const CannotAccessOpaqueNominal = problem_mod.CannotAccessOpaqueNominal;
const NominalTypeResolutionFailed = problem_mod.NominalTypeResolutionFailed;

// Platform errors
const PlatformAliasNotFound = problem_mod.PlatformAliasNotFound;
const PlatformDefNotFound = problem_mod.PlatformDefNotFound;

// Comptime errors
const ComptimeCrash = problem_mod.ComptimeCrash;
const ComptimeExpectFailed = problem_mod.ComptimeExpectFailed;
const ComptimeEvalError = problem_mod.ComptimeEvalError;

// Generic errors
const VarWithSnapshot = problem_mod.VarWithSnapshot;

// Context types for precise error reporting
const Context = problem_mod.Context;
const Category = problem_mod.Category;

/// Returns singular form if count is 1, plural form otherwise.
/// Usage: pluralize(count, "argument", "arguments")
fn pluralize(count: anytype, singular: []const u8, plural: []const u8) []const u8 {
    return if (count == 1) singular else plural;
}

// reporting //

/// Build reports for problems
pub const ReportBuilder = struct {
    const Self = @This();

    gpa: Allocator,
    bytes_buf: std.array_list.Managed(u8),
    module_env: *ModuleEnv,
    can_ir: *const ModuleEnv,
    snapshots: *const snapshot.Store,
    problems: *const Store,
    source: []const u8,
    filename: []const u8,
    other_modules: []const *const ModuleEnv,
    import_mapping: *const @import("types").import_mapping.ImportMapping,
    diff_fields: SnapshotRecordFieldSafeList,
    diff_tags: SnapshotTagSafeList,
    typo_suggestions: diff.TypoSuggestion.ArrayList,

    /// Init report builder
    /// Only owned field is `buf`
    pub fn init(
        gpa: Allocator,
        module_env: *ModuleEnv,
        can_ir: *const ModuleEnv,
        snapshots: *const snapshot.Store,
        problems: *const Store,
        filename: []const u8,
        other_modules: []const *const ModuleEnv,
        import_mapping: *const @import("types").import_mapping.ImportMapping,
    ) !Self {
        return .{
            .gpa = gpa,
            .bytes_buf = try std.array_list.Managed(u8).initCapacity(gpa, 16),
            .module_env = module_env,
            .can_ir = can_ir,
            .snapshots = snapshots,
            .problems = problems,
            .import_mapping = import_mapping,
            .source = module_env.common.source,
            .filename = filename,
            .other_modules = other_modules,
            .diff_fields = try SnapshotRecordFieldSafeList.initCapacity(gpa, 8),
            .diff_tags = try SnapshotTagSafeList.initCapacity(gpa, 8),
            .typo_suggestions = try diff.TypoSuggestion.ArrayList.initCapacity(gpa, 16),
        };
    }

    /// Deinit report builder, only fields it owns
    pub fn deinit(self: *Self) void {
        self.bytes_buf.deinit();
        self.diff_fields.deinit(self.gpa);
        self.diff_tags.deinit(self.gpa);
        self.typo_suggestions.deinit();
    }

    /// Reset report builder, only fields it owns
    pub fn reset(self: *Self) void {
        self.bytes_buf.clearRetainingCapacity();
        self.diff_fields.items.clearRetainingCapacity();
        self.diff_tags.items.clearRetainingCapacity();
        self.typo_suggestions.clearRetainingCapacity();
    }

    // Helpers

    /// Add source code highlighting for a variable's region.
    /// Consolidates the common pattern of: get region -> calc info -> add to document.
    fn addSourceHighlight(self: *Self, report: *Report, region_idx: Region.Idx) !void {
        const region = self.can_ir.store.regions.get(region_idx);
        return self.addSourceHighlightRegion(report, region.*);
    }

    /// Add source code highlighting for a region.
    fn addSourceHighlightRegion(self: *Self, report: *Report, region: Region) !void {
        const region_info = self.module_env.calcRegionInfo(region);
        try report.document.addSourceRegion(
            region_info,
            .error_highlight,
            self.filename,
            self.source,
            self.module_env.getLineStarts(),
        );
    }

    /// Add source code highlighting for a variable's region.
    /// Consolidates the common pattern of: get region -> calc info -> add to document.
    fn addFocusedSourceHighlight(self: *Self, report: *Report, outer_region_idx: Region.Idx, inner_region_idx: Region.Idx) !void {
        const outer_region = self.can_ir.store.regions.get(outer_region_idx);
        const outer_region_info = self.module_env.calcRegionInfo(outer_region.*);

        const inner_region = self.can_ir.store.regions.get(inner_region_idx);
        const inner_region_info = self.module_env.calcRegionInfo(inner_region.*);

        const display_region = SourceCodeDisplayRegion{
            .line_text = try self.gpa.dupe(u8, outer_region_info.calculateLineText(self.source, self.module_env.getLineStarts())),
            .start_line = outer_region_info.start_line_idx + 1,
            .start_column = outer_region_info.start_col_idx + 1,
            .end_line = outer_region_info.end_line_idx + 1,
            .end_column = outer_region_info.end_col_idx + 1,
            .region_annotation = .dimmed,
            .filename = self.filename,
        };
        const underline_regions = [_]UnderlineRegion{
            .{
                .start_line = inner_region_info.start_line_idx + 1,
                .start_column = inner_region_info.start_col_idx + 1,
                .end_line = inner_region_info.end_line_idx + 1,
                .end_column = inner_region_info.end_col_idx + 1,
                .annotation = .error_highlight,
            },
        };

        try report.document.addSourceCodeWithUnderlines(display_region, &underline_regions);
    }

    // Generic Reports

    /// Convert a type into a type var
    pub fn regionIdxFrom(idx: anytype) Region.Idx {
        std.debug.assert(@TypeOf(idx) == Var or @TypeOf(idx) == CIR.Expr.Idx or @TypeOf(idx) == CIR.Pattern.Idx or @TypeOf(idx) == CIR.Node.Idx);
        return @enumFromInt(@intFromEnum(idx));
    }

    const ProblemRegion = union(enum) {
        simple: Region.Idx,
        focused: struct { outer: Region.Idx, highlight: Region.Idx },
    };

    /// Region for source highlighting - can be either a Region.Idx or a direct Region
    const SourceHighlightRegion = union(enum) {
        idx: Region.Idx,
        region: Region,
    };

    /// A lightweight document fragment for building error report text.
    ///
    /// Doc provides a declarative way to construct report messages by composing
    /// small fragments (text, identifiers, numbers) with optional annotations.
    /// Fragments are rendered immediately via `renderSlice` - they are NOT stored.
    ///
    /// ## Usage Pattern
    ///
    /// Create Doc slices inline using anonymous array literals and render immediately:
    ///
    /// ```zig
    /// try D.renderSlice(&.{
    ///     D.bytes("Expected"),
    ///     D.num(3),
    ///     D.bytes("arguments"),
    /// }, self, &report);
    /// ```
    ///
    /// ## IMPORTANT: Slice Lifetime Constraints
    ///
    /// Doc slices created with `&.{...}` are stack-allocated and must be rendered
    /// immediately. NEVER store Doc slices for later use - the underlying memory
    /// becomes invalid after the enclosing scope ends.
    ///
    /// BAD (dangling pointer):
    /// ```zig
    /// var stored_slices: ArrayList([]const Doc) = ...;
    /// for (items) |item| {
    ///     stored_slices.append(&.{ D.ident(item.name) }); // BUG: stack memory!
    /// }
    /// // Later iteration sees garbage data
    /// ```
    ///
    /// GOOD (render immediately):
    /// ```zig
    /// for (items) |item| {
    ///     try D.renderSlice(&.{ D.ident(item.name) }, self, &report);
    /// }
    /// ```
    ///
    /// For dynamic content that varies per iteration, render directly using
    /// `report.document` methods instead of the Doc abstraction.
    const Doc = struct {
        type_: union(enum) {
            bytes: []const u8,
            link: []const u8,
            ident: Ident.Idx,
            num_ord: u32,
            num: u32,
        },
        preceding_space: bool,
        annotation: ?reporting.Annotation,

        /// Create a Doc containing literal text bytes.
        /// The bytes must have static lifetime (string literals) or be owned elsewhere.
        fn bytes(b: []const u8) Doc {
            return Doc{
                .type_ = .{ .bytes = b },
                .preceding_space = true,
                .annotation = null,
            };
        }

        /// Create a Doc containing a hyperlink.
        fn link(b: []const u8) Doc {
            return Doc{
                .type_ = .{ .link = b },
                .preceding_space = true,
                .annotation = null,
            };
        }

        /// Create a Doc that will render an identifier's text.
        /// The ident is resolved at render time via the ReportBuilder's can_ir.
        fn ident(b: Ident.Idx) Doc {
            return Doc{
                .type_ = .{ .ident = b },
                .preceding_space = true,
                .annotation = null,
            };
        }

        /// Create a Doc containing an ordinal number (1st, 2nd, 3rd, etc.).
        fn num_ord(b: u32) Doc {
            return Doc{
                .type_ = .{ .num_ord = b },
                .preceding_space = true,
                .annotation = null,
            };
        }

        /// Create a Doc containing a cardinal number.
        fn num(b: u32) Doc {
            return Doc{
                .type_ = .{ .num = b },
                .preceding_space = true,
                .annotation = null,
            };
        }

        /// Add a visual annotation (inline_code, emphasized, etc.) to this Doc.
        fn withAnnotation(doc: Doc, anno: reporting.Annotation) Doc {
            var result = doc;
            result.annotation = anno;
            return result;
        }

        /// Remove the default space that precedes this Doc when rendered in a slice.
        fn withNoPrecedingSpace(doc: Doc) Doc {
            var result = doc;
            result.preceding_space = false;
            return result;
        }

        /// Render a single Doc fragment to the report.
        fn render(doc: Doc, builder: *ReportBuilder, report: *Report) !void {
            switch (doc.type_) {
                .bytes => |b| {
                    if (doc.annotation) |annotation| {
                        try report.document.addAnnotated(b, annotation);
                    } else {
                        try report.document.addReflowingText(b);
                    }
                },
                .link => |b| {
                    try report.document.addLink(b);
                },
                .ident => |i| {
                    const ident_bytes = try report.addOwnedString(builder.can_ir.getIdent(i));
                    if (doc.annotation) |annotation| {
                        try report.document.addAnnotated(ident_bytes, annotation);
                    } else {
                        try report.document.addReflowingText(ident_bytes);
                    }
                },
                .num_ord => |ord| {
                    const ord_bytes = try builder.getOrdinalOwned(report, ord);
                    if (doc.annotation) |annotation| {
                        try report.document.addAnnotated(ord_bytes, annotation);
                    } else {
                        try report.document.addReflowingText(ord_bytes);
                    }
                },
                .num => |n| {
                    const num_bytes = try builder.getNumOwned(report, n);
                    if (doc.annotation) |annotation| {
                        try report.document.addAnnotated(num_bytes, annotation);
                    } else {
                        try report.document.addReflowingText(num_bytes);
                    }
                },
            }
        }

        /// Render a slice of Doc fragments to the report, joining with spaces.
        ///
        /// This is the primary way to use Docs - create an inline slice and render
        /// immediately. The slice is processed in a single pass with no allocations
        /// beyond what individual Doc variants require (e.g., ident lookup).
        ///
        /// Performance: O(n) where n is the number of docs. Each doc involves:
        /// - A switch on the doc type
        /// - For .bytes/.link: direct string append (very fast)
        /// - For .ident: one hash lookup + string copy
        /// - For .num/.num_ord: integer formatting to scratch buffer + copy
        fn renderSlice(docs: []const Doc, builder: *ReportBuilder, report: *Report) !void {
            for (docs, 0..) |doc, i| {
                if (i != 0 and doc.preceding_space) try report.document.addReflowingText(" ");
                try doc.render(builder, report);
            }
        }

        pub const ArrayList = std.array_list.Managed(Doc);
    };
    const D = Doc;

    /// Helper to create a "type mismatch" report, where both the actual type and
    /// the expected type need to be displayed
    fn makeMismatchReport(
        self: *Self,
        region: ProblemRegion,
        title: []const Doc,
        actual_label: []const Doc,
        actual_snapshot: SnapshotContentIdx,
        expected_label: []const Doc,
        expected_snapshot: SnapshotContentIdx,
        hints: []const []const Doc,
    ) !Report {
        var report = Report.init(self.gpa, "TYPE MISMATCH", .runtime_error);
        errdefer report.deinit();

        // Add title
        try D.renderSlice(title, self, &report);
        try report.document.addLineBreak();

        // Add the region to highlight
        switch (region) {
            .simple => |region_idx| {
                try self.addSourceHighlight(&report, region_idx);
            },
            .focused => |ctx| {
                try self.addFocusedSourceHighlight(&report, ctx.outer, ctx.highlight);
            },
        }
        try report.document.addLineBreak();

        // Print the actual
        try D.renderSlice(actual_label, self, &report);
        try report.document.addLineBreak();
        try report.document.addLineBreak();
        const actual_type_str = try report.addOwnedString(self.getFormattedString(actual_snapshot));
        try report.document.addCodeBlock(actual_type_str);

        try report.document.addLineBreak();
        try report.document.addLineBreak();

        // Print the expected
        try D.renderSlice(expected_label, self, &report);
        try report.document.addLineBreak();
        try report.document.addLineBreak();
        const expected_type_str = try report.addOwnedString(self.getFormattedString(expected_snapshot));
        try report.document.addCodeBlock(expected_type_str);

        // Print static hints
        for (hints, 0..) |hint, i| {
            if (i == 0) try report.document.addLineBreak();
            try report.document.addLineBreak();
            try D.renderSlice(hint, self, &report);
        }

        // Generate and print type comparison hints
        const diff_hints = diff.compareTypes(
            self.snapshots,
            self.module_env.getIdentStoreConst(),
            expected_snapshot,
            actual_snapshot,
            self.gpa,
            &self.diff_fields,
            &self.diff_tags,
        );
        try self.renderDiffHints(&report, diff_hints, hints.len > 0);

        return report;
    }

    /// Helper to create a "bad type" report, where only the actual type needs
    /// to be displayed
    fn makeBadTypeReport(
        self: *Self,
        region: ProblemRegion,
        title: []const Doc,
        actual_label: []const Doc,
        actual_snapshot: SnapshotContentIdx,
        hints: []const []const Doc,
    ) !Report {
        var report = Report.init(self.gpa, "TYPE MISMATCH", .runtime_error);
        errdefer report.deinit();

        // Add title
        try D.renderSlice(title, self, &report);
        try report.document.addLineBreak();

        // Add the region to highlight
        switch (region) {
            .simple => |region_idx| try self.addSourceHighlight(&report, region_idx),
            .focused => |ctx| try self.addFocusedSourceHighlight(&report, ctx.outer, ctx.highlight),
        }
        try report.document.addLineBreak();

        // Print the actual
        try D.renderSlice(actual_label, self, &report);
        try report.document.addLineBreak();
        try report.document.addLineBreak();
        const actual_type_str = try report.addOwnedString(self.getFormattedString(actual_snapshot));
        try report.document.addCodeBlock(actual_type_str);

        // Print hints
        for (hints, 0..) |hint, i| {
            if (i == 0) try report.document.addLineBreak();
            try report.document.addLineBreak();
            try D.renderSlice(hint, self, &report);
        }

        return report;
    }

    /// Helper to create a "type mismatch" report, where both the actual type and
    /// the expected type need to be displayed
    fn makeCustomReport(
        self: *Self,
        region: ProblemRegion,
        title: []const Doc,
        hints: []const []const Doc,
    ) !Report {
        var report = Report.init(self.gpa, "TYPE MISMATCH", .runtime_error);
        errdefer report.deinit();

        // Add title
        try D.renderSlice(title, self, &report);
        try report.document.addLineBreak();

        // Add the region to highlight
        switch (region) {
            .simple => |region_idx| {
                try self.addSourceHighlight(&report, region_idx);
            },
            .focused => |ctx| {
                try self.addFocusedSourceHighlight(&report, ctx.outer, ctx.highlight);
            },
        }

        // Print static hints
        for (hints) |hint| {
            try report.document.addLineBreak();
            try D.renderSlice(hint, self, &report);
        }

        return report;
    }

    /// Render type comparison hints (arity mismatch, missing fields, typos, effect mismatch)
    fn renderDiffHints(self: *Self, report: *Report, hints: diff.HintList, had_static_hints: bool) !void {
        for (hints.slice(), 0..) |hint, i| {
            // Add spacing before first hint
            if (i == 0 and !had_static_hints) {
                try report.document.addLineBreak();
            }
            try report.document.addLineBreak();

            switch (hint) {
                .arity_mismatch => |am| {
                    try D.renderSlice(&.{
                        D.bytes("Hint:").withAnnotation(.emphasized),
                        D.bytes("This function expects"),
                        D.num(am.expected),
                        D.bytes(pluralize(am.expected, "argument", "arguments")),
                        D.bytes("but got"),
                        D.num(am.actual),
                        D.bytes(".").withNoPrecedingSpace(),
                    }, self, report);
                },
                .fields_missing => |fm| {
                    // Reconstruct slice from range
                    const fields = self.diff_fields.sliceRange(fm.fields).items(.name);
                    if (fields.len == 1) {
                        try D.renderSlice(&.{
                            D.bytes("Hint:").withAnnotation(.emphasized),
                            D.bytes("This record is missing the field:"),
                            D.ident(fields[0]).withAnnotation(.inline_code),
                        }, self, report);
                    } else {
                        try D.renderSlice(&.{
                            D.bytes("Hint:").withAnnotation(.emphasized),
                            D.bytes("This record is missing these fields:"),
                        }, self, report);
                        for (fields) |field| {
                            try report.document.addLineBreak();
                            try D.renderSlice(&.{
                                D.bytes(" -"),
                                D.ident(field).withAnnotation(.inline_code),
                            }, self, report);
                        }
                    }
                },
                .field_typo => |ft| {
                    try D.renderSlice(&.{
                        D.bytes("Hint:").withAnnotation(.emphasized),
                        D.bytes("Maybe"),
                        D.ident(ft.typo).withAnnotation(.inline_code),
                        D.bytes("should be"),
                        D.ident(ft.suggestion).withAnnotation(.inline_code),
                        D.bytes("?").withNoPrecedingSpace(),
                    }, self, report);
                },
                .tag_typo => |tt| {
                    try D.renderSlice(&.{
                        D.bytes("Hint:").withAnnotation(.emphasized),
                        D.bytes("Maybe"),
                        D.ident(tt.typo).withAnnotation(.inline_code),
                        D.bytes("should be"),
                        D.ident(tt.suggestion).withAnnotation(.inline_code),
                        D.bytes("?").withNoPrecedingSpace(),
                    }, self, report);
                },
                .effect_mismatch => |em| {
                    switch (em.expected) {
                        .pure => {
                            try D.renderSlice(&.{
                                D.bytes("Hint:").withAnnotation(.emphasized),
                                D.bytes("This function is effectful, but a pure function is expected."),
                            }, self, report);
                        },
                        .effectful => {
                            try D.renderSlice(&.{
                                D.bytes("Hint:").withAnnotation(.emphasized),
                                D.bytes("This function is pure, but an effectful function is expected."),
                            }, self, report);
                        },
                    }
                },
            }
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    // Build reports

    /// Build a report for a problem
    pub fn build(
        self: *Self,
        problem: Problem,
    ) !Report {
        const trace = tracy.trace(@src());
        defer trace.end();

        self.reset();

        switch (problem) {
            .type_mismatch => |mismatch| {
                // All error contexts are now handled via mismatch.context
                return switch (mismatch.context) {
                    .if_condition => self.buildIfConditionReport(mismatch.types),
                    .if_branch => |ctx| self.buildIfBranchReport(mismatch.types, ctx),
                    .match_pattern => |ctx| self.buildMatchPatternReport(mismatch.types, ctx),
                    .match_branch => |ctx| self.buildMatchBranchReport(mismatch.types, ctx),
                    .list_entry => |ctx| self.buildListEntryReport(mismatch.types, ctx),
                    .fn_call_arity => |ctx| self.buildIncompatibleFnCallArity(mismatch.types, ctx),
                    .fn_call_arg => |ctx| self.buildIncompatibleFnCallArg(mismatch.types, ctx),
                    .binop_lhs => |ctx| self.buildBinopReport(mismatch.types, ctx, .lhs),
                    .binop_rhs => |ctx| self.buildBinopReport(mismatch.types, ctx, .rhs),
                    .try_operator_expr => |ctx| self.buildTryOperatorExprReport(mismatch.types, ctx),
                    .statement_value => self.buildStatementValueReport(mismatch.types),
                    .early_return => self.buildEarlyReturnReport(mismatch.types),
                    .try_operator => self.buildTryOperatorReport(mismatch.types),
                    .nominal_constructor => |ctx| switch (ctx.backing_type) {
                        .tag => self.buildInvalidNominalTag(mismatch.types),
                        .record => self.buildInvalidNominalRecord(mismatch.types),
                        .tuple => self.buildInvalidNominalTuple(mismatch.types),
                        .value => self.buildInvalidNominalValue(mismatch.types),
                    },
                    .fn_args_bound_var => |ctx| self.buildIncompatibleFnArgsBoundVar(mismatch.types, ctx),
                    .method_type => |ctx| self.buildIncompatibleMethodType(mismatch.types, ctx),
                    .expect => self.buildExpect(mismatch.types),
                    .record_access => |ctx| self.buildRecordAccess(mismatch.types, ctx),
                    .record_update => |ctx| self.buildRecordUpdate(mismatch.types, ctx),
                    .platform_requirement => return try self.makeMismatchReport(
                        ProblemRegion{ .simple = regionIdxFrom(mismatch.types.actual_var) },
                        &.{D.bytes("This expression is used in an unexpected way:")},
                        &.{D.bytes("It has the type:")},
                        mismatch.types.actual_snapshot,
                        &.{D.bytes("But the platform says it should be:")},
                        mismatch.types.expected_snapshot,
                        &.{},
                    ),
                    .type_annotation => return try self.makeMismatchReport(
                        ProblemRegion{ .simple = regionIdxFrom(mismatch.types.actual_var) },
                        &.{D.bytes("This expression is used in an unexpected way:")},
                        &.{D.bytes("It has the type:")},
                        mismatch.types.actual_snapshot,
                        &.{D.bytes("But the annotation say it should be:")},
                        mismatch.types.expected_snapshot,
                        &.{},
                    ),
                    .none => return try self.buildGenericMismatch(mismatch.types),
                };
            },
            .type_apply_mismatch_arities => |data| {
                return self.buildTypeApplyArityMismatchReport(data);
            },
            .cannot_access_opaque_nominal => |data| {
                return self.buildCannotAccessOpaqueNominal(data);
            },
            .nominal_type_resolution_failed => |data| {
                return self.buildNominalTypeResolutionFailed(data);
            },
            .static_dispatch => |detail| {
                switch (detail) {
                    .dispatcher_not_nominal => |data| return self.buildStaticDispatchDispatcherNotNominal(data),
                    .dispatcher_does_not_impl_method => |data| return self.buildStaticDispatchDispatcherDoesNotImplMethod(data),
                    .type_does_not_support_equality => |data| return self.buildTypeDoesNotSupportEquality(data),
                }
            },
            .recursive_alias => |data| {
                return self.buildRecursiveAliasReport(data);
            },
            .unsupported_alias_where_clause => |data| {
                return self.buildUnsupportedAliasWhereClauseReport(data);
            },
            .infinite_recursion => |data| {
                return self.buildUnimplementedReport("Infinite recursion", data.var_, data.snapshot);
            },
            .anonymous_recursion => |data| {
                return self.buildUnimplementedReport("Anonymous recursion", data.var_, data.snapshot);
            },
            .platform_alias_not_found => |data| {
                return self.buildPlatformAliasNotFound(data);
            },
            .platform_def_not_found => |data| {
                return self.buildPlatformDefNotFound(data);
            },
            .comptime_crash => |data| return self.buildComptimeCrashReport(data),
            .comptime_expect_failed => |data| return self.buildComptimeExpectFailedReport(data),
            .comptime_eval_error => |data| return self.buildComptimeEvalErrorReport(data),
            .non_exhaustive_match => |data| return self.buildNonExhaustiveMatchReport(data),
            .redundant_pattern => |data| return self.buildRedundantPatternReport(data),
            .unmatchable_pattern => |data| return self.buildUnmatchablePatternReport(data),
        }
    }

    // type mismatch //

    fn buildGenericMismatch(self: *Self, types: TypePair) !Report {
        return try self.makeMismatchReport(
            ProblemRegion{ .simple = regionIdxFrom(types.actual_var) },
            &.{D.bytes("This expression is used in an unexpected way:")},
            &.{D.bytes("It has the type:")},
            types.actual_snapshot,
            &.{D.bytes("But you are trying to use it as:")},
            types.expected_snapshot,
            &.{},
        );
    }

    /// Build a report for if condition type error
    fn buildIfConditionReport(self: *Self, types: TypePair) !Report {
        return try self.makeBadTypeReport(
            .{ .simple = regionIdxFrom(types.actual_var) },
            &.{
                D.bytes("This"),
                D.bytes("if").withAnnotation(.inline_code),
                D.bytes("condition must evaluate to a"),
                D.bytes("Bool").withAnnotation(.inline_code),
                D.bytes("–either").withNoPrecedingSpace(),
                D.bytes("True").withAnnotation(.inline_code),
                D.bytes("or"),
                D.bytes("False").withAnnotation(.inline_code),
                D.bytes(":").withNoPrecedingSpace(),
            },
            &.{D.bytes("It is:")},
            types.actual_snapshot,
            &.{
                &.{
                    D.bytes("But I need this to be a"),
                    D.bytes("Bool").withAnnotation(.inline_code),
                    D.bytes("value."),
                },
            },
        );
    }

    /// Build a report for if branch type mismatch
    fn buildIfBranchReport(self: *Self, types: TypePair, ctx: Context.IfBranchContext) !Report {
        const branch_index = ctx.branch_index + 1;
        return try self.makeMismatchReport(
            .{ .simple = regionIdxFrom(types.actual_var) },
            &.{
                D.bytes("The"),
                D.num_ord(branch_index),
                D.bytes("branch of this"),
                D.bytes("if").withAnnotation(.inline_code),
                D.bytes("does not match the previous"),
                if (ctx.num_branches > 2)
                    D.bytes("branches")
                else
                    D.bytes("branch"),
                D.bytes(":"),
            },
            &.{
                D.bytes("The"),
                D.num_ord(branch_index),
                D.bytes("branch is:"),
            },
            types.actual_snapshot,
            &.{
                D.bytes("But the previous"),
                if (ctx.num_branches > 2)
                    D.bytes("branches result")
                else
                    D.bytes("branch results"),
                D.bytes("in:"),
            },
            types.expected_snapshot,
            &.{},
        );
    }

    /// Build a report for match pattern type mismatch
    fn buildMatchPatternReport(self: *Self, types: TypePair, ctx: Context.MatchPatternContext) !Report {
        const branch_index = ctx.branch_index + 1;
        if (branch_index == 1) {
            const title_parts: []const Doc =
                if (ctx.num_patterns == 1)
                    &.{
                        D.bytes("The first pattern in this"),
                        D.bytes("match").withAnnotation(.inline_code),
                        D.bytes("is incompatible:"),
                    }
                else
                    &.{
                        D.bytes("This pattern in this"),
                        D.bytes("match").withAnnotation(.inline_code),
                        D.bytes("is incompatible:"),
                    };
            const actual_parts: []const Doc =
                if (ctx.num_patterns == 1)
                    &.{D.bytes("The first pattern is trying to match:")}
                else
                    &.{D.bytes("This pattern is trying to match:")};
            return try self.makeMismatchReport(
                ProblemRegion{ .focused = .{
                    .outer = regionIdxFrom(ctx.match_expr),
                    .highlight = regionIdxFrom(types.actual_var),
                } },
                title_parts,
                actual_parts,
                types.actual_snapshot,
                &.{
                    D.bytes("But the expression between the"),
                    D.bytes("match").withAnnotation(.inline_code),
                    D.bytes("parenthesis has the type:"),
                },
                types.expected_snapshot,
                &.{
                    &.{
                        D.bytes("These can never match! Either the pattern or expression has a problem."),
                    },
                },
            );
        } else {
            const title_parts: []const Doc =
                if (ctx.num_patterns == 1)
                    &.{
                        D.bytes("The"),
                        D.num_ord(branch_index),
                        D.bytes("branch of this"),
                        D.bytes("match").withAnnotation(.inline_code),
                        D.bytes("does not match the previous ones:"),
                    }
                else
                    &.{
                        D.bytes("This pattern in the "),
                        D.num_ord(branch_index),
                        D.bytes("branch of this"),
                        D.bytes("match").withAnnotation(.inline_code),
                        D.bytes("does not match the previous ones:"),
                    };
            const actual_parts: []const Doc =
                if (ctx.num_patterns == 1)
                    &.{
                        D.bytes("This"),
                        D.num_ord(branch_index),
                        D.bytes("branch is trying to match:"),
                    }
                else
                    &.{
                        D.bytes("This pattern is trying to match:"),
                    };
            return try self.makeMismatchReport(
                ProblemRegion{ .focused = .{
                    .outer = regionIdxFrom(ctx.match_expr),
                    .highlight = regionIdxFrom(types.actual_var),
                } },
                title_parts,
                actual_parts,
                types.actual_snapshot,
                &.{
                    D.bytes("But the expression between the"),
                    D.bytes("match").withAnnotation(.inline_code),
                    D.bytes("parenthesis has the type:"),
                },
                types.expected_snapshot,
                &.{
                    &.{
                        D.bytes("These can never match! Either the pattern or expression has a problem."),
                    },
                },
            );
        }
    }

    /// Build a report for match branch type mismatch
    fn buildMatchBranchReport(self: *Self, types: TypePair, ctx: Context.MatchBranchContext) !Report {
        const branch_index = ctx.branch_index + 1;
        return try self.makeMismatchReport(
            .{ .simple = regionIdxFrom(types.actual_var) },
            &.{
                D.bytes("The"),
                D.num_ord(branch_index),
                D.bytes("branch of this"),
                D.bytes("match").withAnnotation(.inline_code),
                D.bytes("does not match the previous"),
                if (ctx.num_branches > 2)
                    D.bytes("branches")
                else
                    D.bytes("branch"),
                D.bytes(":"),
            },
            &.{
                D.bytes("The"),
                D.num_ord(branch_index),
                D.bytes("branch is:"),
            },
            types.actual_snapshot,
            &.{
                D.bytes("But the previous"),
                if (ctx.num_branches > 2)
                    D.bytes("branches result")
                else
                    D.bytes("branch results"),
                D.bytes("in:"),
            },
            types.expected_snapshot,
            &.{
                &.{
                    D.bytes("All branches in a"),
                    D.bytes("match").withAnnotation(.inline_code),
                    D.bytes("must have compatible types."),
                },
                &.{
                    D.bytes("Note:").withAnnotation(.underline),
                    D.bytes("You can wrap branches values in a tag to make them compatible."),
                },
                &.{
                    D.bytes("To learn about tags, see"),
                    D.link("https://www.roc-lang.org/tutorial#tags"),
                },
            },
        );
    }

    /// Build a report for list entry type mismatch
    fn buildListEntryReport(self: *Self, types: TypePair, ctx: Context.ListEntryContext) !Report {
        const elem_idx = ctx.elem_index;
        const title: []const Doc = if (ctx.list_length == 2)
            &.{D.bytes("The two elements in this list have incompatible types:")}
        else if (elem_idx == 1)
            &.{D.bytes("The first two elements in this list have incompatible types:")}
        else
            &.{
                D.bytes("The"),
                D.num_ord(elem_idx),
                D.bytes("and"),
                D.num_ord(elem_idx + 1),
                D.bytes("elements in this list have incompatible types:"),
            };

        return try self.makeMismatchReport(
            .{ .simple = regionIdxFrom(types.actual_var) },
            title,
            &.{
                D.bytes("The"),
                D.num_ord(elem_idx),
                D.bytes("element has this type:"),
            },
            types.expected_snapshot,
            &.{
                D.bytes("However, the"),
                D.num_ord(elem_idx + 1),
                D.bytes("element has this type:"),
            },
            types.actual_snapshot,
            &.{
                &.{D.bytes("All elements in a list must have compatible types.")},
                &.{
                    D.bytes("Note:").withAnnotation(.underline),
                    D.bytes("You can wrap each element in a tag to make them compatible."),
                },
                &.{
                    D.bytes("To learn about tags, see"),
                    D.link("https://www.roc-lang.org/tutorial#tags"),
                },
            },
        );
    }

    /// Build a report for bool binop type error (and/or)
    fn buildBinopReport(self: *Self, types: TypePair, ctx: Context.BinopContext, side: enum { lhs, rhs }) !Report {
        const op_str = if (ctx.operator == .@"or") "or" else "and";
        const side_str = if (side == .lhs) "left" else "right";
        return try self.makeBadTypeReport(
            .{ .simple = regionIdxFrom(types.actual_var) },
            &.{D.bytes("I'm having trouble with this bool operation:")},
            &.{
                D.bytes("Both sides of"),
                D.bytes(op_str).withAnnotation(.inline_code),
                D.bytes("must be"),
                D.bytes("Bool").withAnnotation(.inline_code),
                D.bytes("values, but the"),
                D.bytes(side_str),
                D.bytes("side is:"),
            },
            types.actual_snapshot,
            &.{
                &.{
                    D.bytes("Note:").withAnnotation(.underline),
                    D.bytes("Roc does not have \"truthiness\". You must convert values to bools yourself."),
                },
            },
        );
    }

    /// Build a report for invalid try operator expression
    fn buildTryOperatorExprReport(self: *Self, types: TypePair, ctx: Context.TryOperatorContext) !Report {
        return try self.makeBadTypeReport(
            .{ .simple = regionIdxFrom(ctx.expr) },
            &.{
                D.bytes("The"),
                D.bytes("?").withAnnotation(.inline_code),
                D.bytes("operator expects a"),
                D.bytes("Try").withAnnotation(.inline_code),
                D.bytes("type (a tag union containing ONLY"),
                D.bytes("Ok").withAnnotation(.inline_code),
                D.bytes("and"),
                D.bytes("Err").withAnnotation(.inline_code),
                D.bytes("tags), but I found:"),
            },
            &.{D.bytes("This expression has type:")},
            types.actual_snapshot,
            &.{
                &.{
                    D.bytes("Tip:").withAnnotation(.underline),
                    D.bytes("Maybe wrap a value using"),
                    D.bytes("Ok(value)").withAnnotation(.inline_code),
                    D.bytes("or"),
                    D.bytes("Err(value)").withAnnotation(.inline_code),
                    D.bytes(".").withNoPrecedingSpace(),
                },
            },
        );
    }

    /// Build a report for statement value type error
    fn buildStatementValueReport(self: *Self, types: TypePair) !Report {
        return try self.makeBadTypeReport(
            .{ .simple = regionIdxFrom(types.actual_var) },
            &.{D.bytes("This expression produces a value, but it's not being used:")},
            &.{D.bytes("It has the type:")},
            types.actual_snapshot,
            &.{
                &.{
                    D.bytes("Since this expression is used as a statement, it must evaluate to"),
                    D.bytes("{}").withAnnotation(.inline_code),
                    D.bytes(".").withNoPrecedingSpace(),
                },
                &.{
                    D.bytes("If you don't need the value, you can ignore it with"),
                    D.bytes("_ =").withAnnotation(.inline_code),
                    D.bytes(".").withNoPrecedingSpace(),
                },
            },
        );
    }

    /// Build a report for early return type mismatch
    fn buildEarlyReturnReport(self: *Self, types: TypePair) !Report {
        return try self.makeMismatchReport(
            .{ .simple = regionIdxFrom(types.actual_var) },
            &.{
                D.bytes("This"),
                D.bytes("return").withAnnotation(.inline_code),
                D.bytes("does not match the function's return type:"),
            },
            &.{D.bytes("It has the type:")},
            types.actual_snapshot,
            &.{D.bytes("But the function's return type is:")},
            types.expected_snapshot,
            &.{
                &.{
                    D.bytes("Hint:").withAnnotation(.emphasized),
                    D.bytes("All"),
                    D.bytes("return").withAnnotation(.inline_code),
                    D.bytes("statements and the final expression in a function must have the same type."),
                },
            },
        );
    }

    /// Build a report for try operator return type mismatch
    fn buildTryOperatorReport(self: *Self, types: TypePair) !Report {
        return try self.makeMismatchReport(
            .{ .simple = regionIdxFrom(types.actual_var) },
            &.{
                D.bytes("This"),
                D.bytes("?").withAnnotation(.inline_code),
                D.bytes("may return early with a type that doesn't match the function body:"),
            },
            &.{D.bytes("On error, this would return:")},
            types.actual_snapshot,
            &.{D.bytes("But the function body evaluates to:")},
            types.expected_snapshot,
            &.{
                &.{
                    D.bytes("Hint:").withAnnotation(.emphasized),
                    D.bytes("The error types from all"),
                    D.bytes("?").withAnnotation(.inline_code),
                    D.bytes("operators and the function body must be compatible since any of them could be the actual return value."),
                },
            },
        );
    }

    /// Build a report for function argument type mismatch
    fn buildIncompatibleFnCallArity(
        self: *Self,
        types: TypePair,
        ctx: Context.FnCallArityContext,
    ) !Report {
        const title = blk: {
            if (ctx.expected_args > ctx.actual_args) {
                break :blk "TOO FEW ARGS";
            } else if (ctx.expected_args < ctx.actual_args) {
                break :blk "TOO MANY ARGS";
            } else {
                std.debug.assert(false);
                break :blk "WRONG NUMBER OF ARGS";
            }
        };

        var report = Report.init(self.gpa, title, .runtime_error);
        errdefer report.deinit();

        // Add body
        if (ctx.fn_name) |fn_name| {
            try D.renderSlice(&.{
                D.bytes("The"),
                D.ident(fn_name).withAnnotation(.inline_code),
            }, self, &report);
        } else {
            try D.renderSlice(&.{
                D.bytes("This"),
            }, self, &report);
        }
        try D.renderSlice(&.{
            D.bytes(" function expects"),
            D.num(ctx.expected_args),
            D.bytes(pluralize(ctx.expected_args, "argument", "arguments")),
            D.bytes(",").withNoPrecedingSpace(),
            D.bytes("but it got"),
            D.num(ctx.actual_args),
            D.bytes("instead:"),
        }, self, &report);
        try report.document.addLineBreak();

        // Add region
        try self.addSourceHighlight(&report, regionIdxFrom(types.actual_var));
        try report.document.addLineBreak();

        if (ctx.fn_name) |fn_name| {
            try D.renderSlice(&.{
                D.bytes("The"),
                D.ident(fn_name).withAnnotation(.inline_code),
            }, self, &report);
        } else {
            try D.renderSlice(&.{
                D.bytes("This"),
            }, self, &report);
        }
        try D.renderSlice(&.{
            D.bytes(" function has the type:"),
        }, self, &report);
        try report.document.addLineBreak();
        try report.document.addLineBreak();
        const expected_type_str = try report.addOwnedString(self.getFormattedString(types.expected_snapshot));
        try report.document.addCodeBlock(expected_type_str);

        if (ctx.actual_args < ctx.expected_args) {
            try report.document.addLineBreak();
            try report.document.addLineBreak();

            try D.renderSlice(&.{
                D.bytes("Are there any missing commas?"),
            }, self, &report);
        }

        return report;
    }

    /// Build a report for function argument type mismatch
    fn buildIncompatibleFnCallArg(
        self: *Self,
        types: TypePair,
        ctx: Context.FnCallArgContext,
    ) !Report {
        // Extract only the argument types from the function snapshots
        const actual_arg_type = types.actual_snapshot;
        const expected_arg_type = types.expected_snapshot;

        if (ctx.fn_name) |fn_name_ident| {
            return try self.makeMismatchReport(
                .{ .focused = .{
                    .outer = regionIdxFrom(ctx.call_expr),
                    .highlight = regionIdxFrom(ctx.arg_var),
                } },
                &.{
                    D.bytes("The"),
                    D.num_ord(ctx.arg_index + 1),
                    D.bytes("argument being passed to this function has the wrong type:"),
                },
                &.{D.bytes("This argument has the type:")},
                actual_arg_type,
                &.{
                    D.bytes("But"),
                    D.ident(fn_name_ident).withAnnotation(.inline_code),
                    D.bytes("needs the"),
                    D.num_ord(ctx.arg_index + 1),
                    D.bytes("argument to be:"),
                },
                expected_arg_type,
                &.{},
            );
        } else {
            return try self.makeMismatchReport(
                .{ .simple = regionIdxFrom(ctx.arg_var) },
                &.{
                    D.bytes("The"),
                    D.num_ord(ctx.arg_index + 1),
                    D.bytes("argument being passed to this function has the wrong type:"),
                },
                &.{D.bytes("This argument has the type:")},
                actual_arg_type,
                &.{
                    D.bytes("But the function needs the"),
                    D.num_ord(ctx.arg_index + 1),
                    D.bytes("argument to be:"),
                },
                expected_arg_type,
                &.{},
            );
        }
    }

    /// Build a report for incompatible match branches
    fn buildInvalidNominalTag(
        self: *Self,
        types: TypePair,
    ) !Report {
        var report = Report.init(self.gpa, "INVALID NOMINAL TAG", .runtime_error);
        errdefer report.deinit();

        // Create actual tag str
        const actual_content = self.snapshots.getContentUnwrapAlias(types.actual_snapshot);
        std.debug.assert(actual_content == .structure);
        std.debug.assert(actual_content.structure == .tag_union);
        std.debug.assert(actual_content.structure.tag_union.tags.len() == 1);
        const actual_tag = self.snapshots.tags.get(actual_content.structure.tag_union.tags.start);
        const actual_tag_str = try report.addOwnedString(snapshot.Store.getFormattedTagString(actual_tag));

        // Create expected tag str
        const expected_content = self.snapshots.getContentUnwrapAlias(types.expected_snapshot);
        std.debug.assert(expected_content == .structure);
        std.debug.assert(expected_content.structure == .tag_union);
        const expected_num_tags_str = expected_content.structure.tag_union.tags.len();

        // Add description
        try report.document.addText("I'm having trouble with this nominal tag:");
        try report.document.addLineBreak();

        const region = self.can_ir.store.regions.get(@enumFromInt(@intFromEnum(types.actual_var)));
        const region_info = self.module_env.calcRegionInfo(region.*);
        try report.document.addSourceRegion(
            region_info,
            .error_highlight,
            self.filename,
            self.source,
            self.module_env.getLineStarts(),
        );
        try report.document.addLineBreak();

        // Show the invalid tag
        try report.document.addText("The tag is:");
        try report.document.addLineBreak();
        try report.document.addLineBreak();
        try report.document.addCodeBlock(actual_tag_str);
        try report.document.addLineBreak();
        try report.document.addLineBreak();

        // Show the expected tags
        if (expected_num_tags_str == 1) {
            const expected_tag = self.snapshots.tags.get(expected_content.structure.tag_union.tags.start);
            const expected_tag_str = try report.addOwnedString(snapshot.Store.getFormattedTagString(expected_tag));

            try report.document.addText("But the nominal type needs it to be:");
            try report.document.addLineBreak();
            try report.document.addLineBreak();
            try report.document.addCodeBlock(expected_tag_str);
        } else {
            const expected_type = try report.addOwnedString(self.getFormattedString(types.expected_snapshot));

            try report.document.addText("But the nominal type needs it to one of:");
            try report.document.addLineBreak();
            try report.document.addLineBreak();
            try report.document.addCodeBlock(expected_type);

            // Check if there's a tag with the same name in the list of possible tags

            var iter = expected_content.structure.tag_union.tags.iterIndices();
            while (iter.next()) |tag_index| {
                const cur_expected_tag = self.snapshots.tags.get(tag_index);

                if (actual_tag.name == cur_expected_tag.name) {
                    const cur_expected_tag_str = try report.addOwnedString(snapshot.Store.getFormattedTagString(cur_expected_tag));

                    try report.document.addLineBreak();
                    try report.document.addLineBreak();
                    try report.document.addAnnotated("Hint:", .emphasized);
                    try report.document.addReflowingText(" The nominal type has a tag with the same name, but different args:");
                    try report.document.addLineBreak();
                    try report.document.addCodeBlock(cur_expected_tag_str);

                    break;
                }
            }
        }

        return report;
    }

    /// Build a report for invalid nominal record (record fields don't match)
    fn buildInvalidNominalRecord(
        self: *Self,
        types: TypePair,
    ) !Report {
        var report = Report.init(self.gpa, "INVALID NOMINAL RECORD", .runtime_error);
        errdefer report.deinit();

        try report.document.addText("I'm having trouble with this nominal type that wraps a record:");
        try report.document.addLineBreak();

        const region = self.can_ir.store.regions.get(@enumFromInt(@intFromEnum(types.actual_var)));
        const region_info = self.module_env.calcRegionInfo(region.*);
        try report.document.addSourceRegion(
            region_info,
            .error_highlight,
            self.filename,
            self.source,
            self.module_env.getLineStarts(),
        );
        try report.document.addLineBreak();

        const actual_type = try report.addOwnedString(self.getFormattedString(types.actual_snapshot));
        const expected_type = try report.addOwnedString(self.getFormattedString(types.expected_snapshot));

        try report.document.addText("The record I found is:");
        try report.document.addLineBreak();
        try report.document.addLineBreak();
        try report.document.addCodeBlock(actual_type);
        try report.document.addLineBreak();
        try report.document.addLineBreak();

        try report.document.addText("But the nominal type expects:");
        try report.document.addLineBreak();
        try report.document.addLineBreak();
        try report.document.addCodeBlock(expected_type);

        return report;
    }

    /// Build a report for invalid nominal tuple (tuple elements don't match)
    fn buildInvalidNominalTuple(
        self: *Self,
        types: TypePair,
    ) !Report {
        var report = Report.init(self.gpa, "INVALID NOMINAL TUPLE", .runtime_error);
        errdefer report.deinit();

        try report.document.addText("I'm having trouble with this nominal type that wraps a tuple:");
        try report.document.addLineBreak();

        const region = self.can_ir.store.regions.get(@enumFromInt(@intFromEnum(types.actual_var)));
        const region_info = self.module_env.calcRegionInfo(region.*);
        try report.document.addSourceRegion(
            region_info,
            .error_highlight,
            self.filename,
            self.source,
            self.module_env.getLineStarts(),
        );
        try report.document.addLineBreak();

        const actual_type = try report.addOwnedString(self.getFormattedString(types.actual_snapshot));
        const expected_type = try report.addOwnedString(self.getFormattedString(types.expected_snapshot));

        try report.document.addText("The tuple I found is:");
        try report.document.addLineBreak();
        try report.document.addLineBreak();
        try report.document.addCodeBlock(actual_type);
        try report.document.addLineBreak();
        try report.document.addLineBreak();

        try report.document.addText("But the nominal type expects:");
        try report.document.addLineBreak();
        try report.document.addLineBreak();
        try report.document.addCodeBlock(expected_type);

        return report;
    }

    /// Build a report for invalid nominal value (value type doesn't match)
    fn buildInvalidNominalValue(
        self: *Self,
        types: TypePair,
    ) !Report {
        var report = Report.init(self.gpa, "INVALID NOMINAL TYPE", .runtime_error);
        errdefer report.deinit();

        try report.document.addText("I'm having trouble with this nominal type:");
        try report.document.addLineBreak();

        const region = self.can_ir.store.regions.get(@enumFromInt(@intFromEnum(types.actual_var)));
        const region_info = self.module_env.calcRegionInfo(region.*);
        try report.document.addSourceRegion(
            region_info,
            .error_highlight,
            self.filename,
            self.source,
            self.module_env.getLineStarts(),
        );
        try report.document.addLineBreak();

        const actual_type = try report.addOwnedString(self.getFormattedString(types.actual_snapshot));
        const expected_type = try report.addOwnedString(self.getFormattedString(types.expected_snapshot));

        try report.document.addText("The value I found has type:");
        try report.document.addLineBreak();
        try report.document.addLineBreak();
        try report.document.addCodeBlock(actual_type);
        try report.document.addLineBreak();
        try report.document.addLineBreak();

        try report.document.addText("But the nominal type expects:");
        try report.document.addLineBreak();
        try report.document.addLineBreak();
        try report.document.addCodeBlock(expected_type);

        return report;
    }

    fn buildIncompatibleFnArgsBoundVar(
        self: *Self,
        types: TypePair,
        ctx: Context.FnArgsBoundVarContext,
    ) !Report {
        // expected = first argument, actual = second argument
        if (ctx.fn_name) |fn_name_ident| {
            return try self.makeMismatchReport(
                .{ .simple = regionIdxFrom(ctx.second_arg_var) },
                &.{
                    D.bytes("The"),
                    D.num_ord(ctx.first_arg_index + 1),
                    D.bytes("and"),
                    D.num_ord(ctx.second_arg_index + 1),
                    D.bytes("arguments to"),
                    D.ident(fn_name_ident).withAnnotation(.inline_code),
                    D.bytes("must have compatible types, but they are incompatible in this call:"),
                },
                &.{
                    D.bytes("The"),
                    D.num_ord(ctx.first_arg_index + 1),
                    D.bytes("argument has the type:"),
                },
                types.expected_snapshot,
                &.{
                    D.bytes("But the"),
                    D.num_ord(ctx.second_arg_index + 1),
                    D.bytes("argument has the type:"),
                },
                types.actual_snapshot,
                &.{
                    &.{
                        D.ident(fn_name_ident).withAnnotation(.inline_code),
                        D.bytes("needs these arguments to have compatible types."),
                    },
                },
            );
        } else {
            return try self.makeMismatchReport(
                .{ .simple = regionIdxFrom(ctx.second_arg_var) },
                &.{
                    D.bytes("The"),
                    D.num_ord(ctx.first_arg_index + 1),
                    D.bytes("and"),
                    D.num_ord(ctx.second_arg_index + 1),
                    D.bytes("arguments to this function must have compatible types, but they are incompatible in this call:"),
                },
                &.{
                    D.bytes("The"),
                    D.num_ord(ctx.first_arg_index + 1),
                    D.bytes("argument has the type:"),
                },
                types.expected_snapshot,
                &.{
                    D.bytes("But the"),
                    D.num_ord(ctx.second_arg_index + 1),
                    D.bytes("argument has the type:"),
                },
                types.actual_snapshot,
                &.{
                    &.{D.bytes("This function needs these arguments to have compatible types.")},
                },
            );
        }
    }

    // annotation problems //

    /// Build a report for "number does not fit in type" diagnostic
    fn buildTypeApplyArityMismatchReport(
        self: *Self,
        data: TypeApplyArityMismatch,
    ) !Report {
        const title = blk: {
            if (data.num_expected_args > data.num_actual_args) {
                break :blk "TOO FEW ARGS";
            } else if (data.num_expected_args < data.num_actual_args) {
                break :blk "TOO MANY ARGS";
            } else {
                break :blk "WRONG NUMBER OF ARGS";
            }
        };
        var report = Report.init(self.gpa, title, .runtime_error);
        errdefer report.deinit();

        // Look up display name in import mapping (handles auto-imported builtin types)
        // If the type_name is in the mapping (e.g., "Builtin.Bool"), use the mapped display name ("Bool")
        // Otherwise, use the identifier as-is
        const type_name_ident = if (self.import_mapping.get(data.type_name)) |display_ident|
            display_ident
        else
            data.type_name;

        try D.renderSlice(&.{
            D.bytes("The type"),
            D.ident(type_name_ident).withAnnotation(.type_variable),
            D.bytes("expects"),
            D.num(data.num_expected_args),
            D.bytes(pluralize(data.num_expected_args, "argument,", "arguments,")),
            D.bytes("but got"),
            D.num(data.num_actual_args),
            D.bytes("instead."),
        }, self, &report);
        try report.document.addLineBreak();

        // Add source region highlighting
        const region_info = self.module_env.calcRegionInfo(data.region);
        try report.document.addSourceRegion(
            region_info,
            .error_highlight,
            self.filename,
            self.source,
            self.module_env.getLineStarts(),
        );

        return report;
    }

    /// Build a report for when a type alias references itself recursively
    fn buildRecursiveAliasReport(
        self: *Self,
        data: RecursiveAlias,
    ) !Report {
        var report = Report.init(self.gpa, "RECURSIVE ALIAS", .runtime_error);
        errdefer report.deinit();

        // Look up display name in import mapping (handles auto-imported builtin types)
        const type_name_ident = if (self.import_mapping.get(data.type_name)) |display_ident|
            display_ident
        else
            data.type_name;

        try D.renderSlice(&.{
            D.bytes("The type alias"),
            D.ident(type_name_ident).withAnnotation(.type_variable),
            D.bytes("references itself, which is not allowed:"),
        }, self, &report);
        try report.document.addLineBreak();

        // Add source region highlighting
        const region_info = self.module_env.calcRegionInfo(data.region);
        try report.document.addSourceRegion(
            region_info,
            .error_highlight,
            self.filename,
            self.source,
            self.module_env.getLineStarts(),
        );
        try report.document.addLineBreak();

        try D.renderSlice(&.{
            D.bytes("Type aliases cannot be recursive. If you need a recursive type, use a nominal type"),
            D.bytes(":=").withAnnotation(.inline_code),
            D.bytes("instead of an alias"),
            D.bytes(":").withAnnotation(.inline_code).withNoPrecedingSpace(),
            D.bytes(".").withNoPrecedingSpace(),
        }, self, &report);

        return report;
    }

    /// Build a report for when alias syntax is used in a where clause
    /// This syntax was used for abilities which have been removed
    fn buildUnsupportedAliasWhereClauseReport(
        self: *Self,
        data: UnsupportedAliasWhereClause,
    ) !Report {
        var report = Report.init(self.gpa, "UNSUPPORTED WHERE CLAUSE", .runtime_error);
        errdefer report.deinit();

        try D.renderSlice(&.{
            D.bytes("The where clause syntax"),
            D.ident(data.alias_name).withAnnotation(.type_variable),
            D.bytes("is not supported:"),
        }, self, &report);
        try report.document.addLineBreak();

        // Add source region highlighting
        const region_info = self.module_env.calcRegionInfo(data.region);
        try report.document.addSourceRegion(
            region_info,
            .error_highlight,
            self.filename,
            self.source,
            self.module_env.getLineStarts(),
        );
        try report.document.addLineBreak();

        try D.renderSlice(&.{
            D.bytes("This syntax was used for abilities, which have been removed from Roc. Use method constraints like"),
            D.bytes("where [a.methodName(args) -> ret]").withAnnotation(.inline_code),
            D.bytes("instead."),
        }, self, &report);

        return report;
    }

    // static dispatch //

    /// Build a report for when a type is not nominal, but you're trying to
    /// static dispatch on it
    fn buildStaticDispatchDispatcherNotNominal(
        self: *Self,
        data: DispatcherNotNominal,
    ) !Report {
        var report = Report.init(self.gpa, "MISSING METHOD", .runtime_error);
        errdefer report.deinit();

        const snapshot_str = try report.addOwnedString(self.getFormattedString(data.dispatcher_snapshot));

        try D.renderSlice(&.{
            D.bytes("This"),
            D.ident(data.method_name).withAnnotation(.emphasized),
            D.bytes("method is being called on a value whose type doesn't have that method:"),
        }, self, &report);
        try report.document.addLineBreak();

        // Add source region highlighting
        const region = self.can_ir.store.regions.get(@enumFromInt(@intFromEnum(data.fn_var)));
        const region_info = self.module_env.calcRegionInfo(region.*);
        try report.document.addSourceRegion(
            region_info,
            .error_highlight,
            self.filename,
            self.source,
            self.module_env.getLineStarts(),
        );
        try report.document.addLineBreak();

        try D.renderSlice(&.{
            D.bytes("The value's type, which does not have a method named"),
            D.ident(data.method_name).withAnnotation(.emphasized).withNoPrecedingSpace(),
            D.bytes(",").withNoPrecedingSpace(),
            D.bytes("is:"),
        }, self, &report);
        try report.document.addLineBreak();
        try report.document.addLineBreak();
        try report.document.addCodeBlock(snapshot_str);

        return report;
    }

    /// Build a report for when a type doesn't have the expected static dispatch
    /// method
    fn buildStaticDispatchDispatcherDoesNotImplMethod(
        self: *Self,
        data: DispatcherDoesNotImplMethod,
    ) !Report {
        // Special case: number literal being used where a non-number type is expected
        if (data.origin == .from_numeral) {
            return self.buildNumberUsedAsNonNumber(data);
        }

        var report = Report.init(self.gpa, "MISSING METHOD", .runtime_error);
        errdefer report.deinit();

        const snapshot_str = try report.addOwnedString(self.getFormattedString(data.dispatcher_snapshot));

        // Check if this method corresponds to an operator (using ident index comparison, not strings)
        const is_from_binop = data.origin == .desugared_binop;
        const mb_operator = self.getOperatorForMethod(data.method_name);

        if (is_from_binop) {
            if (mb_operator) |operator| {
                try D.renderSlice(&.{
                    D.bytes("The value before this"),
                    D.bytes(operator).withAnnotation(.emphasized),
                    D.bytes("operator has a type that doesn't have a"),
                    D.ident(data.method_name).withAnnotation(.emphasized),
                    D.bytes("method:"),
                }, self, &report);
            } else {
                try D.renderSlice(&.{
                    D.bytes("This"),
                    D.ident(data.method_name).withAnnotation(.emphasized),
                    D.bytes("method is being called on a value whose type doesn't have that method:"),
                }, self, &report);
            }
        } else {
            try D.renderSlice(&.{
                D.bytes("This"),
                D.ident(data.method_name).withAnnotation(.emphasized),
                D.bytes("method is being called on a value whose type doesn't have that method:"),
            }, self, &report);
        }
        try report.document.addLineBreak();

        // Add source region highlighting
        const region = self.can_ir.store.regions.get(@enumFromInt(@intFromEnum(data.fn_var)));
        const region_info = self.module_env.calcRegionInfo(region.*);
        try report.document.addSourceRegion(
            region_info,
            .error_highlight,
            self.filename,
            self.source,
            self.module_env.getLineStarts(),
        );
        try report.document.addLineBreak();

        try D.renderSlice(&.{
            D.bytes("The value's type, which does not have a method named"),
            D.ident(data.method_name).withAnnotation(.emphasized).withNoPrecedingSpace(),
            D.bytes(",").withNoPrecedingSpace(),
            D.bytes("is:"),
        }, self, &report);
        try report.document.addLineBreak();
        try report.document.addLineBreak();
        try report.document.addCodeBlock(snapshot_str);

        try report.document.addLineBreak();
        try report.document.addLineBreak();
        switch (data.dispatcher_type) {
            .nominal => {
                if (is_from_binop) {
                    if (mb_operator) |operator| {
                        try D.renderSlice(&.{
                            D.bytes("Hint:").withAnnotation(.emphasized),
                            D.bytes("The"),
                            D.bytes(operator).withAnnotation(.emphasized),
                            D.bytes("operator calls a method named"),
                            D.ident(data.method_name).withAnnotation(.emphasized),
                            D.bytes("on the value preceding it, passing the value after the operator as the one argument."),
                        }, self, &report);
                    } else {
                        try D.renderSlice(&.{
                            D.bytes("Hint:").withAnnotation(.emphasized),
                            D.bytes("For this to work, the type would need to have a method named"),
                            D.ident(data.method_name).withAnnotation(.emphasized),
                            D.bytes("associated with it in the type's declaration."),
                        }, self, &report);
                    }
                } else {
                    try D.renderSlice(&.{
                        D.bytes("Hint:").withAnnotation(.emphasized),
                        D.bytes("For this to work, the type would need to have a method named"),
                        D.ident(data.method_name).withAnnotation(.emphasized),
                        D.bytes("associated with it in the type's declaration."),
                    }, self, &report);
                }
            },
            .rigid => {
                if (is_from_binop) {
                    if (mb_operator) |operator| {
                        try D.renderSlice(&.{
                            D.bytes("Hint:").withAnnotation(.emphasized),
                            D.bytes("The"),
                            D.bytes(operator).withAnnotation(.emphasized),
                            D.bytes("operator requires the type to have a"),
                            D.ident(data.method_name).withAnnotation(.emphasized),
                            D.bytes("method. Did you forget to specify it in the type annotation?"),
                        }, self, &report);
                    } else {
                        try D.renderSlice(&.{
                            D.bytes("Hint:").withAnnotation(.emphasized),
                            D.bytes("Did you forget to specify"),
                            D.ident(data.method_name).withAnnotation(.emphasized),
                            D.bytes("in the type annotation?"),
                        }, self, &report);
                    }
                } else {
                    try D.renderSlice(&.{
                        D.bytes("Hint:").withAnnotation(.emphasized),
                        D.bytes("Did you forget to specify"),
                        D.ident(data.method_name).withAnnotation(.emphasized),
                        D.bytes("in the type annotation?"),
                    }, self, &report);
                }
            },
        }

        return report;
    }

    /// Build a report for when a number literal is used where a non-number type is expected
    fn buildNumberUsedAsNonNumber(
        self: *Self,
        data: DispatcherDoesNotImplMethod,
    ) !Report {
        var report = Report.init(self.gpa, "TYPE MISMATCH", .runtime_error);
        errdefer report.deinit();

        const snapshot_str = try report.addOwnedString(self.getFormattedString(data.dispatcher_snapshot));

        // Get the region of the number literal from the num_literal info
        const num_literal = data.num_literal.?;
        const num_region = num_literal.region;
        const num_region_info = self.module_env.calcRegionInfo(num_region);

        // Get the region of the dispatcher (the type that was expected)
        // This might be different if the type came from somewhere else (e.g., a type annotation)
        const dispatcher_region = self.can_ir.store.regions.get(@enumFromInt(@intFromEnum(data.dispatcher_var))).*;

        try D.renderSlice(&.{
            D.bytes("This number is being used where a non-number type is needed:"),
        }, self, &report);
        try report.document.addLineBreak();

        try report.document.addSourceRegion(
            num_region_info,
            .error_highlight,
            self.filename,
            self.source,
            self.module_env.getLineStarts(),
        );
        try report.document.addLineBreak();

        // Check if we have a different origin region we can show
        if (dispatcher_region.start.offset != num_region.start.offset or
            dispatcher_region.end.offset != num_region.end.offset)
        {
            const dispatcher_region_info = self.module_env.calcRegionInfo(dispatcher_region);
            try D.renderSlice(&.{
                D.bytes("The type was determined to be non-numeric here:"),
            }, self, &report);
            try report.document.addLineBreak();

            try report.document.addSourceRegion(
                dispatcher_region_info,
                .error_highlight,
                self.filename,
                self.source,
                self.module_env.getLineStarts(),
            );
            try report.document.addLineBreak();
        }

        try D.renderSlice(&.{
            D.bytes("Other code expects this to have the type:"),
        }, self, &report);
        try report.document.addLineBreak();
        try report.document.addLineBreak();
        try report.document.addCodeBlock(snapshot_str);

        return report;
    }

    /// Build a report for when an anonymous type doesn't support equality
    fn buildTypeDoesNotSupportEquality(
        self: *Self,
        data: TypeDoesNotSupportEquality,
    ) !Report {
        var report = Report.init(self.gpa, "TYPE DOES NOT SUPPORT EQUALITY", .runtime_error);
        errdefer report.deinit();

        const snapshot_str = try report.addOwnedString(self.getFormattedString(data.dispatcher_snapshot));

        const region = self.can_ir.store.regions.get(@enumFromInt(@intFromEnum(data.fn_var)));
        const region_info = self.module_env.calcRegionInfo(region.*);

        try D.renderSlice(&.{
            D.bytes("This expression is doing an equality check on a type that doesn't support equality:"),
        }, self, &report);
        try report.document.addLineBreak();

        try report.document.addSourceRegion(
            region_info,
            .error_highlight,
            self.filename,
            self.source,
            self.module_env.getLineStarts(),
        );
        try report.document.addLineBreak();

        try D.renderSlice(&.{
            D.bytes("The type is:"),
        }, self, &report);
        try report.document.addLineBreak();
        try report.document.addLineBreak();
        try report.document.addCodeBlock(snapshot_str);
        try report.document.addLineBreak();
        try report.document.addLineBreak();

        // Get the content and explain which parts don't support equality
        const content = self.snapshots.getContentUnwrapAlias(data.dispatcher_snapshot);
        if (content == .structure) {
            switch (content.structure) {
                .record => |record| {
                    try self.explainRecordEqualityFailure(&report, record);
                },
                .tuple => |tuple| {
                    try self.explainTupleEqualityFailure(&report, tuple);
                },
                .tag_union => |tag_union| {
                    try self.explainTagUnionEqualityFailure(&report, tag_union);
                },
                .fn_pure, .fn_effectful, .fn_unbound => {
                    try D.renderSlice(&.{
                        D.bytes("Functions cannot be compared for equality."),
                    }, self, &report);
                    try report.document.addLineBreak();
                },
                else => {},
            }
        }

        return report;
    }

    /// Build a report for when a method exists but its type doesn't match the where clause requirement
    fn buildIncompatibleMethodType(
        self: *Self,
        types: TypePair,
        ctx: Context.MethodTypeContext,
    ) !Report {
        // Note: The unifier's actual/expected are opposite to display order.
        // We want to show "type has X" (from expected_snapshot) then "expected Y" (from actual_snapshot)
        return try self.makeMismatchReport(
            .{ .simple = regionIdxFrom(ctx.constraint_var) },
            &.{
                D.bytes("The"),
                D.ident(ctx.method_name).withAnnotation(.inline_code),
                D.bytes("method on"),
                D.ident(ctx.dispatcher_name).withAnnotation(.inline_code),
                D.bytes("has an incompatible type:"),
            },
            &.{
                D.bytes("The method"),
                D.ident(ctx.method_name).withAnnotation(.inline_code),
                D.bytes("has the type:"),
            },
            types.expected_snapshot,
            &.{D.bytes("But is needs to have the type:")},
            types.actual_snapshot,
            &.{
                // TODO: Once we have nicer type diff hints, use them here
            },
        );
    }

    /// Build a report for when a method exists but its type doesn't match the where clause requirement
    fn buildExpect(
        self: *Self,
        types: TypePair,
    ) !Report {
        // Note: The unifier's actual/expected are opposite to display order.
        // We want to show "type has X" (from expected_snapshot) then "expected Y" (from actual_snapshot)
        return try self.makeBadTypeReport(
            .{ .simple = regionIdxFrom(types.actual_var) },
            &.{
                D.bytes("This"),
                D.bytes("expect").withAnnotation(.inline_code),
                D.bytes("statement must evaluate to a"),
                D.bytes("Bool").withAnnotation(.inline_code),
                D.bytes("–either").withNoPrecedingSpace(),
                D.bytes("True").withAnnotation(.inline_code),
                D.bytes("or"),
                D.bytes("False").withAnnotation(.inline_code),
                D.bytes(":").withNoPrecedingSpace(),
            },
            &.{D.bytes("It is:")},
            types.actual_snapshot,
            &.{
                &.{
                    D.bytes("But I need this to be a"),
                    D.bytes("Bool").withAnnotation(.inline_code),
                    D.bytes("value."),
                },
            },
        );
    }

    /// Build a typo suggestions report for when a record field is not found.
    /// This is used by both buildRecordAccess and buildRecordUpdate.
    fn buildTypoSuggestionsReport(
        self: *Self,
        field_name: Ident.Idx,
        available_fields: []const Ident.Idx,
        source_region: SourceHighlightRegion,
        is_record_update: bool,
    ) !Report {
        // Get a sorted list of the most similar field names
        try diff.findBestTypoSuggestions(
            field_name,
            available_fields,
            self.can_ir.getIdentStoreConst(),
            &self.typo_suggestions,
        );
        std.debug.assert(self.typo_suggestions.items.len > 0);
        const best_suggestion = self.typo_suggestions.items[0];

        // Create report directly and render dynamic suggestions inline
        var report = Report.init(self.gpa, "TYPE MISMATCH", .runtime_error);
        errdefer report.deinit();

        // Add title
        try D.renderSlice(&.{
            D.bytes("This record does not have a"),
            D.ident(field_name).withAnnotation(.inline_code),
            D.bytes("field:"),
        }, self, &report);
        try report.document.addLineBreak();

        // Add source highlight
        switch (source_region) {
            .idx => |idx| try self.addSourceHighlight(&report, idx),
            .region => |region| try self.addSourceHighlightRegion(&report, region),
        }

        // Render typo suggestions directly
        try report.document.addLineBreak();
        try report.document.addReflowingText("This is often due to a typo. The most similar fields are:");
        try report.document.addLineBreak();
        const count = @min(self.typo_suggestions.items.len, 3);
        for (self.typo_suggestions.items[0..count]) |suggestion| {
            try report.document.addLineBreak();
            try report.document.addText("    - ");
            try report.document.addAnnotated(self.can_ir.getIdentText(suggestion.ident), .inline_code);
        }
        try report.document.addLineBreak();
        try report.document.addLineBreak();
        try report.document.addReflowingText("So maybe ");
        try report.document.addAnnotated(self.can_ir.getIdentText(field_name), .inline_code);
        try report.document.addReflowingText(" should be ");
        try report.document.addAnnotated(self.can_ir.getIdentText(best_suggestion.ident), .inline_code);
        try report.document.addText("?");

        // Add note about record update syntax limitations
        if (is_record_update) {
            try report.document.addLineBreak();
            try report.document.addLineBreak();
            try D.renderSlice(&.{
                D.bytes("Note:").withAnnotation(.underline),
                D.bytes("You cannot add new fields to a record with the record update syntax."),
            }, self, &report);
        }

        return report;
    }

    /// Build a report for when a record field is accessed but doesn't exist
    fn buildRecordAccess(
        self: *Self,
        types: TypePair,
        ctx: Context.RecordAccessContext,
    ) !Report {
        self.diff_fields.items.clearRetainingCapacity();

        const record = try self.snapshots.gatherRecordFields(types.actual_snapshot, self.gpa, &self.diff_fields);

        const region = ProblemRegion{ .simple = regionIdxFrom(types.actual_var) };
        switch (record) {
            .not_a_record => {
                return try self.makeBadTypeReport(
                    region,
                    &.{D.bytes("This is not a record, so it does not have any fields to access:")},
                    &.{D.bytes("It is:")},
                    types.actual_snapshot,
                    &.{
                        &.{
                            D.bytes("But I need a record with a"),
                            D.ident(ctx.field_name).withAnnotation(.inline_code),
                            D.bytes("field."),
                        },
                    },
                );
            },
            .empty_record => {
                return try self.makeCustomReport(
                    region,
                    &.{
                        D.bytes("This record does not have a"),
                        D.ident(ctx.field_name).withAnnotation(.inline_code),
                        D.bytes("field:"),
                    },
                    &.{
                        &.{D.bytes("It is actually a record with no fields.")},
                    },
                );
            },
            .record => |actual_fields_range| {
                const actual_fields = self.diff_fields.sliceRange(actual_fields_range).items(.name);
                return try self.buildTypoSuggestionsReport(
                    ctx.field_name,
                    actual_fields,
                    SourceHighlightRegion{ .region = ctx.field_region },
                    false,
                );
            },
        }
    }

    /// Build a report for when a method exists but its type doesn't match the where clause requirement
    fn buildRecordUpdate(
        self: *Self,
        types: TypePair,
        ctx: Context.RecordUpdateContext,
    ) !Report {
        self.diff_fields.items.clearRetainingCapacity();

        // Get the record data of the type we tried to  update
        const expected_record = try self.snapshots.gatherRecordFields(types.expected_snapshot, self.gpa, &self.diff_fields);
        switch (expected_record) {
            .not_a_record => {
                return try self.makeBadTypeReport(
                    ProblemRegion{ .simple = ctx.record_region_idx },
                    &.{D.bytes("This is not a record, so it does not have any fields to update:")},
                    &.{D.bytes("It is:")},
                    types.expected_snapshot,
                    &.{
                        &.{D.bytes("But I need a record with a record!")},
                    },
                );
            },
            .empty_record => {
                return try self.makeCustomReport(
                    ProblemRegion{ .simple = ctx.record_region_idx },
                    if (ctx.record_name) |record_name| &.{
                        D.bytes("The"),
                        D.ident(record_name).withAnnotation(.inline_code),
                        D.bytes("record does not have a"),
                        D.ident(ctx.field_name).withAnnotation(.inline_code),
                        D.bytes("field:"),
                    } else &.{
                        D.bytes("This record does not have a"),
                        D.ident(ctx.field_name).withAnnotation(.inline_code),
                        D.bytes("field:"),
                    },
                    &.{
                        &.{D.bytes("It is actually a record with no fields.")},
                    },
                );
            },
            .record => |expected_fields| {
                // In this variant, we have to dynamically calculate and
                // print similar record fields. This gets hairy with the
                // makeCustomReport  abstraction, so we fall back to the more
                // robust full record builder

                // Get the record data of the type we tried to  update
                const actual_record = try self.snapshots.gatherRecordFields(types.actual_snapshot, self.gpa, &self.diff_fields);
                const actual_field = switch (actual_record) {
                    .record => |fields| blk: {
                        const slice = self.diff_fields.sliceRange(fields);
                        for (slice.items(.name), slice.items(.content)) |name, content| {
                            if (name == ctx.field_name) break :blk SnapshotRecordField{
                                .name = name,
                                .content = content,
                            };
                        }

                        // Should be impossible for the thing we're updating to
                        // not have the field, but if so show a generic message.
                        std.debug.assert(false);
                        return try self.buildGenericMismatch(types);
                    },
                    else => {
                        // Should be impossible for the thing we're updating to
                        // not be a record, but if so show a generic message.
                        std.debug.assert(false);
                        return try self.buildGenericMismatch(types);
                    },
                };

                // Get the possible field we're trying to update
                const mb_expected_field = blk: {
                    const slice = self.diff_fields.sliceRange(expected_fields);
                    for (slice.items(.name), slice.items(.content)) |name, content| {
                        if (name == ctx.field_name) break :blk SnapshotRecordField{
                            .name = name,
                            .content = content,
                        };
                    }
                    break :blk null;
                };

                if (mb_expected_field) |expected_field| {
                    // If the expected  field exist, but we're here in a
                    // type mismatch, then it must mean that the fields are
                    // incompatible

                    return try self.makeMismatchReport(
                        ProblemRegion{ .simple = ctx.field_region_idx },
                        &.{
                            D.bytes("The type of the field"),
                            D.ident(ctx.field_name).withAnnotation(.inline_code),
                            D.bytes("is incompatible:"),
                        },
                        &.{
                            D.bytes("You are trying to update the"),
                            D.ident(ctx.field_name).withAnnotation(.inline_code),
                            D.bytes("field to be the type:"),
                        },
                        actual_field.content,
                        if (ctx.record_name) |record_name| &.{
                            D.bytes("But the"),
                            D.ident(record_name).withAnnotation(.inline_code),
                            D.bytes("record needs it to be"),
                        } else &.{
                            D.bytes("But it should be:"),
                        },
                        expected_field.content,
                        &.{
                            &.{
                                D.bytes("Note:").withAnnotation(.underline),
                                D.bytes("You cannot change the type of a record field with the record update syntax."),
                                D.bytes("You can do that by create a new record, copying over the unchanged fields, then transforming"),
                                D.ident(ctx.field_name).withAnnotation(.inline_code),
                                D.bytes("to be the new type."),
                            },
                        },
                    );
                } else {
                    // If the expected field does NOT exist, then it likely means
                    // there was a typo.
                    const expected_field_names = self.diff_fields.sliceRange(expected_fields).items(.name);
                    return try self.buildTypoSuggestionsReport(
                        ctx.field_name,
                        expected_field_names,
                        SourceHighlightRegion{ .idx = ctx.record_region_idx },
                        true,
                    );
                }
            },
        }
    }

    /// Build a report for when an anonymous type doesn't support equality
    fn buildCannotAccessOpaqueNominal(
        self: *Self,
        data: CannotAccessOpaqueNominal,
    ) !Report {
        var report = Report.init(self.gpa, "CANNOT USE OPAQUE NOMINAL TYPE", .runtime_error);
        errdefer report.deinit();

        const region = self.can_ir.store.regions.get(@enumFromInt(@intFromEnum(data.var_)));
        const region_info = self.module_env.calcRegionInfo(region.*);

        try D.renderSlice(&.{
            D.bytes("You're attempting to create an instance of"),
            D.ident(data.nominal_type_name).withAnnotation(.inline_code).withNoPrecedingSpace(),
            D.bytes(",").withNoPrecedingSpace(),
            D.bytes("but it's an"),
            D.bytes("opaque").withAnnotation(.emphasized),
            D.bytes("type:"),
        }, self, &report);
        try report.document.addLineBreak();

        try report.document.addSourceRegion(
            region_info,
            .error_highlight,
            self.filename,
            self.source,
            self.module_env.getLineStarts(),
        );

        try report.document.addLineBreak();
        try D.renderSlice(&.{
            D.bytes("Hint:").withAnnotation(.emphasized),
            D.bytes("To create an instance of this type outside the module it's defined in, you have to define it with"),
            D.bytes(":=").withAnnotation(.emphasized),
            D.bytes("instead of"),
            D.bytes("::").withAnnotation(.emphasized).withNoPrecedingSpace(),
            D.bytes(".").withNoPrecedingSpace(),
        }, self, &report);

        return report;
    }

    /// Build a report for when a nominal type variable doesn't resolve properly.
    /// This is a compiler bug - it should never happen in correctly compiled code.
    fn buildNominalTypeResolutionFailed(
        self: *Self,
        data: NominalTypeResolutionFailed,
    ) !Report {
        var report = Report.init(self.gpa, "COMPILER BUG", .runtime_error);
        errdefer report.deinit();

        const region = self.can_ir.store.regions.get(@enumFromInt(@intFromEnum(data.var_)));
        const region_info = self.module_env.calcRegionInfo(region.*);

        try D.renderSlice(&.{
            D.bytes("An internal compiler error occurred while checking this nominal type usage:"),
        }, self, &report);
        try report.document.addLineBreak();

        try report.document.addSourceRegion(
            region_info,
            .error_highlight,
            self.filename,
            self.source,
            self.module_env.getLineStarts(),
        );

        try report.document.addLineBreak();
        try D.renderSlice(&.{
            D.bytes("The nominal type declaration variable did not resolve to a nominal type structure. This indicates a bug in the Roc compiler. Please report this issue at"),
            D.link("https://github.com/roc-lang/roc/issues"),
        }, self, &report);

        return report;
    }

    /// Explain which record fields don't support equality
    fn explainRecordEqualityFailure(
        self: *Self,
        report: *Report,
        record: snapshot.SnapshotRecord,
    ) !void {
        const fields = self.snapshots.sliceRecordFields(record.fields);
        var has_problem_fields = false;

        // First pass: check if any fields don't support equality
        for (fields.items(.content)) |field_content_idx| {
            if (!self.snapshotSupportsEquality(field_content_idx)) {
                has_problem_fields = true;
                break;
            }
        }

        if (has_problem_fields) {
            try report.document.addReflowingText("This record does not support equality because these fields have types that don't support ");
            try report.document.addAnnotated("is_eq", .emphasized);
            try report.document.addReflowingText(":");
            try report.document.addLineBreak();
            try report.document.addLineBreak();

            const field_names = fields.items(.name);
            const field_contents = fields.items(.content);
            for (field_names, field_contents) |name, field_content_idx| {
                if (!self.snapshotSupportsEquality(field_content_idx)) {
                    const field_name = self.can_ir.getIdentText(name);

                    const field_type_str = try report.addOwnedString(self.getFormattedString(field_content_idx));

                    try report.document.addText("    ");
                    try report.document.addAnnotated(field_name, .emphasized);
                    try report.document.addText(": ");
                    try report.document.addAnnotated(field_type_str, .type_variable);
                    try report.document.addLineBreak();

                    // Explain WHY this field doesn't support equality
                    _ = try self.explainWhyNoEquality(report, field_content_idx, "        ");
                }
            }
            try report.document.addAnnotated("Hint:", .emphasized);
            try report.document.addReflowingText(" Anonymous records only have an ");
            try report.document.addAnnotated("is_eq", .emphasized);
            try report.document.addReflowingText(" method if all of their fields have ");
            try report.document.addAnnotated("is_eq", .emphasized);
            try report.document.addReflowingText(" methods.");
            try report.document.addLineBreak();
        }
    }

    /// Explain which tuple elements don't support equality
    fn explainTupleEqualityFailure(
        self: *Self,
        report: *Report,
        tuple: snapshot.SnapshotTuple,
    ) !void {
        const elems = self.snapshots.sliceVars(tuple.elems);
        var has_problem_elems = false;

        // First pass: check if any elements don't support equality
        for (elems) |elem_content_idx| {
            if (!self.snapshotSupportsEquality(elem_content_idx)) {
                has_problem_elems = true;
                break;
            }
        }

        if (has_problem_elems) {
            try report.document.addReflowingText("This tuple does not support equality because these elements have types that don't support ");
            try report.document.addAnnotated("is_eq", .emphasized);
            try report.document.addReflowingText(":");
            try report.document.addLineBreak();
            try report.document.addLineBreak();

            for (elems, 0..) |elem_content_idx, i| {
                if (!self.snapshotSupportsEquality(elem_content_idx)) {
                    const elem_type_str = try report.addOwnedString(self.getFormattedString(elem_content_idx));

                    try report.document.addText("    element ");
                    var buf: [20]u8 = undefined;
                    const index_str = std.fmt.bufPrint(&buf, "{}", .{i}) catch "?";
                    try report.document.addAnnotated(index_str, .emphasized);
                    try report.document.addText(": ");
                    try report.document.addAnnotated(elem_type_str, .type_variable);
                    try report.document.addLineBreak();

                    // Explain WHY this element doesn't support equality
                    _ = try self.explainWhyNoEquality(report, elem_content_idx, "        ");
                }
            }
            try report.document.addAnnotated("Hint:", .emphasized);
            try report.document.addReflowingText(" Tuples only have an ");
            try report.document.addAnnotated("is_eq", .emphasized);
            try report.document.addReflowingText(" method if all of their elements have ");
            try report.document.addAnnotated("is_eq", .emphasized);
            try report.document.addReflowingText(" methods.");
            try report.document.addLineBreak();
        }
    }

    /// Explain which tag union payloads don't support equality
    fn explainTagUnionEqualityFailure(
        self: *Self,
        report: *Report,
        tag_union: snapshot.SnapshotTagUnion,
    ) !void {
        const tags = self.snapshots.sliceTags(tag_union.tags);
        var has_problem_tags = false;

        // First pass: check if any tag payloads don't support equality
        for (tags.items(.args)) |tag_args| {
            const args = self.snapshots.sliceVars(tag_args);
            for (args) |arg_content_idx| {
                if (!self.snapshotSupportsEquality(arg_content_idx)) {
                    has_problem_tags = true;
                    break;
                }
            }
            if (has_problem_tags) break;
        }

        if (has_problem_tags) {
            try report.document.addReflowingText("This tag union does not support equality because these tags have payload types that don't support ");
            try report.document.addAnnotated("is_eq", .emphasized);
            try report.document.addReflowingText(":");
            try report.document.addLineBreak();
            try report.document.addLineBreak();

            const tag_names = tags.items(.name);
            const tag_args_list = tags.items(.args);
            for (tag_names, tag_args_list) |name, tag_args| {
                const args = self.snapshots.sliceVars(tag_args);
                var tag_has_problem = false;
                for (args) |arg_content_idx| {
                    if (!self.snapshotSupportsEquality(arg_content_idx)) {
                        tag_has_problem = true;
                        break;
                    }
                }
                if (tag_has_problem) {
                    const tag_name = self.can_ir.getIdentText(name);
                    try report.document.addText("    ");
                    try report.document.addAnnotated(tag_name, .emphasized);

                    // Show the problematic payload types
                    if (args.len > 0) {
                        try report.document.addText(" (");
                        var first = true;
                        for (args) |arg_content_idx| {
                            if (!first) try report.document.addText(", ");
                            first = false;

                            const arg_type_str = try report.addOwnedString(self.getFormattedString(arg_content_idx));
                            try report.document.addAnnotated(arg_type_str, .type_variable);
                        }
                        try report.document.addText(")");
                    }
                    try report.document.addLineBreak();

                    // Explain WHY each problematic payload doesn't support equality
                    for (args) |arg_content_idx| {
                        if (!self.snapshotSupportsEquality(arg_content_idx)) {
                            _ = try self.explainWhyNoEquality(report, arg_content_idx, "        ");
                        }
                    }
                }
            }
            try report.document.addAnnotated("Hint:", .emphasized);
            try report.document.addReflowingText(" Tag unions only have an ");
            try report.document.addAnnotated("is_eq", .emphasized);
            try report.document.addReflowingText(" method if all of their payload types have ");
            try report.document.addAnnotated("is_eq", .emphasized);
            try report.document.addReflowingText(" methods.");
            try report.document.addLineBreak();
        }
    }

    /// Check if a snapshotted type supports equality
    fn snapshotSupportsEquality(self: *Self, content_idx: snapshot.SnapshotContentIdx) bool {
        const content = self.snapshots.getContentUnwrapAlias(content_idx);
        return switch (content) {
            .structure => |s| switch (s) {
                // Functions never support equality
                .fn_pure, .fn_effectful, .fn_unbound => false,
                // Empty types trivially support equality
                .empty_record, .empty_tag_union => true,
                // Records: all fields must support equality
                .record => |record| {
                    const fields = self.snapshots.sliceRecordFields(record.fields);
                    for (fields.items(.content)) |field_content| {
                        if (!self.snapshotSupportsEquality(field_content)) return false;
                    }
                    return true;
                },
                // Tuples: all elements must support equality
                .tuple => |tuple| {
                    const elems = self.snapshots.sliceVars(tuple.elems);
                    for (elems) |elem_content| {
                        if (!self.snapshotSupportsEquality(elem_content)) return false;
                    }
                    return true;
                },
                // Tag unions: all payloads must support equality
                .tag_union => |tag_union| {
                    const tags_slice = self.snapshots.sliceTags(tag_union.tags);
                    for (tags_slice.items(.args)) |tag_args| {
                        const args = self.snapshots.sliceVars(tag_args);
                        for (args) |arg_content| {
                            if (!self.snapshotSupportsEquality(arg_content)) return false;
                        }
                    }
                    return true;
                },
                // Nominal types: check the backing type (first element in vars)
                .nominal_type => |nominal| {
                    const vars = self.snapshots.sliceVars(nominal.vars);
                    if (vars.len > 0) {
                        // First var is the backing type
                        return self.snapshotSupportsEquality(vars[0]);
                    }
                    return true;
                },
                // Other types (box, etc.) assumed to support equality
                else => true,
            },
            // Aliases: check the underlying type
            .alias => |alias| self.snapshotSupportsEquality(alias.backing),
            // Other types (flex, rigid, recursive, err) assumed to support equality
            else => true,
        };
    }

    /// Explain why a type doesn't support equality, adding the explanation to the report.
    /// Returns true if an explanation was added.
    fn explainWhyNoEquality(self: *Self, report: *Report, content_idx: snapshot.SnapshotContentIdx, indent: []const u8) !bool {
        const content = self.snapshots.getContentUnwrapAlias(content_idx);
        switch (content) {
            .structure => |s| switch (s) {
                .fn_pure, .fn_effectful, .fn_unbound => {
                    try report.document.addText(indent);
                    try report.document.addReflowingText("Function equality is not supported.");
                    try report.document.addLineBreak();
                    return true;
                },
                .record => |record| {
                    const fields = self.snapshots.sliceRecordFields(record.fields);
                    const field_names = fields.items(.name);
                    const field_contents = fields.items(.content);
                    for (field_names, field_contents) |name, field_content| {
                        if (!self.snapshotSupportsEquality(field_content)) {
                            const field_name = self.can_ir.getIdentText(name);
                            try report.document.addText(indent);
                            try report.document.addReflowingText("The ");
                            try report.document.addAnnotated(field_name, .emphasized);
                            try report.document.addReflowingText(" field doesn't support equality:");
                            try report.document.addLineBreak();
                            // Recurse with more indent
                            var deeper_indent_buf: [64]u8 = undefined;
                            const deeper_indent = std.fmt.bufPrint(&deeper_indent_buf, "{s}    ", .{indent}) catch indent;
                            _ = try self.explainWhyNoEquality(report, field_content, deeper_indent);
                            return true;
                        }
                    }
                    return false;
                },
                .tuple => |tuple| {
                    const elems = self.snapshots.sliceVars(tuple.elems);
                    for (elems, 0..) |elem_content, i| {
                        if (!self.snapshotSupportsEquality(elem_content)) {
                            var buf: [20]u8 = undefined;
                            const index_str = std.fmt.bufPrint(&buf, "{}", .{i}) catch "?";
                            try report.document.addText(indent);
                            try report.document.addReflowingText("Element ");
                            try report.document.addAnnotated(index_str, .emphasized);
                            try report.document.addReflowingText(" doesn't support equality:");
                            try report.document.addLineBreak();
                            // Recurse with more indent
                            var deeper_indent_buf: [64]u8 = undefined;
                            const deeper_indent = std.fmt.bufPrint(&deeper_indent_buf, "{s}    ", .{indent}) catch indent;
                            _ = try self.explainWhyNoEquality(report, elem_content, deeper_indent);
                            return true;
                        }
                    }
                    return false;
                },
                .tag_union => |tag_union| {
                    const tags_slice = self.snapshots.sliceTags(tag_union.tags);
                    const tag_names = tags_slice.items(.name);
                    const tag_args_list = tags_slice.items(.args);
                    for (tag_names, tag_args_list) |name, tag_args| {
                        const args = self.snapshots.sliceVars(tag_args);
                        for (args, 0..) |arg_content, i| {
                            if (!self.snapshotSupportsEquality(arg_content)) {
                                const tag_name = self.can_ir.getIdentText(name);
                                try report.document.addText(indent);
                                try report.document.addReflowingText("The ");
                                try report.document.addAnnotated(tag_name, .emphasized);
                                if (args.len > 1) {
                                    var buf: [32]u8 = undefined;
                                    const payload_str = std.fmt.bufPrint(&buf, " tag's payload {}", .{i}) catch " tag's payload";
                                    try report.document.addReflowingText(payload_str);
                                } else {
                                    try report.document.addReflowingText(" tag's payload");
                                }
                                try report.document.addReflowingText(" doesn't support equality:");
                                try report.document.addLineBreak();
                                // Recurse with more indent
                                var deeper_indent_buf: [64]u8 = undefined;
                                const deeper_indent = std.fmt.bufPrint(&deeper_indent_buf, "{s}    ", .{indent}) catch indent;
                                _ = try self.explainWhyNoEquality(report, arg_content, deeper_indent);
                                return true;
                            }
                        }
                    }
                    return false;
                },
                .nominal_type => |nominal| {
                    const vars = self.snapshots.sliceVars(nominal.vars);
                    if (vars.len > 0) {
                        const backing = vars[0];
                        if (!self.snapshotSupportsEquality(backing)) {
                            const nominal_name = self.can_ir.getIdentText(nominal.ident.ident_idx);
                            try report.document.addText(indent);
                            try report.document.addReflowingText("The ");
                            try report.document.addAnnotated(nominal_name, .emphasized);
                            try report.document.addReflowingText(" type's backing structure doesn't support equality:");
                            try report.document.addLineBreak();
                            // Recurse with more indent
                            var deeper_indent_buf: [64]u8 = undefined;
                            const deeper_indent = std.fmt.bufPrint(&deeper_indent_buf, "{s}    ", .{indent}) catch indent;
                            _ = try self.explainWhyNoEquality(report, backing, deeper_indent);
                            return true;
                        }
                    }
                    return false;
                },
                else => return false,
            },
            .alias => |alias| {
                if (!self.snapshotSupportsEquality(alias.backing)) {
                    return try self.explainWhyNoEquality(report, alias.backing, indent);
                }
                return false;
            },
            else => return false,
        }
    }

    /// Build a report for "invalid number literal" diagnostic
    fn buildUnimplementedReport(self: *Self, bytes: []const u8, var_: Var, snapshot_idx: SnapshotContentIdx) !Report {
        var report = Report.init(self.gpa, "UNIMPLEMENTED REPORT", .runtime_error);
        errdefer report.deinit();

        try D.renderSlice(&.{
            D.bytes(bytes),
            D.bytes("error reporting has not been implemented yet!"),
        }, self, &report);
        try report.document.addLineBreak();

        const region = self.can_ir.store.regions.get(@enumFromInt(@intFromEnum(var_)));
        const region_info = self.module_env.calcRegionInfo(region.*);
        try report.document.addSourceRegion(
            region_info,
            .error_highlight,
            self.filename,
            self.source,
            self.module_env.getLineStarts(),
        );
        try report.document.addLineBreak();

        try D.renderSlice(&.{
            D.bytes("The problematic type was:"),
        }, self, &report);
        try report.document.addLineBreak();
        try report.document.addLineBreak();
        const actual_type_str = try report.addOwnedString(self.getFormattedString(snapshot_idx));
        try report.document.addCodeBlock(actual_type_str);
        try report.document.addLineBreak();

        return report;
    }

    fn buildPlatformAliasNotFound(self: *Self, data: PlatformAliasNotFound) !Report {
        var report = Report.init(self.gpa, "MISSING PLATFORM REQUIRED TYPE", .runtime_error);
        errdefer report.deinit();

        try D.renderSlice(&.{
            D.bytes("The platform expects your"),
            D.bytes("app").withAnnotation(.inline_code),
            D.bytes("module to define a type alias named"),
            D.ident(data.expected_alias_ident).withAnnotation(.type_variable).withNoPrecedingSpace(),
            D.bytes(",").withNoPrecedingSpace(),
            D.bytes("but I couldn't find one."),
        }, self, &report);

        switch (data.ctx) {
            .not_found => {
                try report.document.addLineBreak();
                try report.document.addLineBreak();
                try D.renderSlice(&.{
                    D.bytes("Hint:").withAnnotation(.emphasized),
                    D.bytes("Add a type alias definition for"),
                    D.ident(data.expected_alias_ident).withAnnotation(.type_variable),
                    D.bytes("to your app module. Check your platform's documentation for the expected type."),
                }, self, &report);
            },
            .found_but_not_alias => {
                try report.document.addLineBreak();
                try report.document.addLineBreak();
                try D.renderSlice(&.{
                    D.bytes("Hint:").withAnnotation(.emphasized),
                    D.bytes("You have a definition named"),
                    D.ident(data.expected_alias_ident).withAnnotation(.type_variable).withNoPrecedingSpace(),
                    D.bytes(",").withNoPrecedingSpace(),
                    D.bytes("but it's not a type alias. The platform requires a type alias (defined with"),
                    D.bytes(":").withAnnotation(.inline_code).withNoPrecedingSpace(),
                    D.bytes("),").withNoPrecedingSpace(),
                    D.bytes("not a value definition."),
                }, self, &report);
            },
        }

        return report;
    }

    fn buildPlatformDefNotFound(self: *Self, data: PlatformDefNotFound) !Report {
        var report = Report.init(self.gpa, "MISSING PLATFORM REQUIRED DEFINITION", .runtime_error);
        errdefer report.deinit();

        try D.renderSlice(&.{
            D.bytes("The platform expects your"),
            D.bytes("app").withAnnotation(.inline_code),
            D.bytes("module to export a definition named"),
            D.ident(data.expected_def_ident).withAnnotation(.inline_code).withNoPrecedingSpace(),
            D.bytes(",").withNoPrecedingSpace(),
            D.bytes("but I couldn't find one."),
        }, self, &report);

        switch (data.ctx) {
            .not_found => {
                try report.document.addLineBreak();
                try report.document.addLineBreak();
                try D.renderSlice(&.{
                    D.bytes("Hint:").withAnnotation(.emphasized),
                    D.bytes("Define and export"),
                    D.ident(data.expected_def_ident).withAnnotation(.inline_code),
                    D.bytes("in your app module. Check your platform's documentation for the expected type signature."),
                }, self, &report);
            },
            .found_but_not_exported => {
                try report.document.addLineBreak();
                try report.document.addLineBreak();
                try D.renderSlice(&.{
                    D.bytes("Hint:").withAnnotation(.emphasized),
                    D.bytes("You have a definition named"),
                    D.ident(data.expected_def_ident).withAnnotation(.inline_code).withNoPrecedingSpace(),
                    D.bytes(",").withNoPrecedingSpace(),
                    D.bytes("but it's not exported. Add it to your module's"),
                    D.bytes("exposes").withAnnotation(.inline_code),
                    D.bytes("list in the module header."),
                }, self, &report);
            },
        }

        return report;
    }

    /// Build a report for compile-time crash
    fn buildComptimeCrashReport(self: *Self, data: ComptimeCrash) !Report {
        var report = Report.init(self.gpa, "COMPTIME CRASH", .runtime_error);
        errdefer report.deinit();

        const owned_message = try report.addOwnedString(
            self.problems.getExtraString(data.message),
        );

        try D.renderSlice(&.{
            D.bytes("This definition crashed during compile-time evaluation:"),
        }, self, &report);
        try report.document.addLineBreak();

        // Add source region highlighting
        const region_info = self.module_env.calcRegionInfo(data.region);
        try report.document.addSourceRegion(
            region_info,
            .error_highlight,
            self.filename,
            self.source,
            self.module_env.getLineStarts(),
        );
        try report.document.addLineBreak();

        try D.renderSlice(&.{
            D.bytes("The"),
            D.bytes("crash").withAnnotation(.keyword),
            D.bytes("happened with this message:"),
        }, self, &report);
        try report.document.addLineBreak();
        try report.document.addLineBreak();
        try report.document.addCodeBlock(owned_message);

        return report;
    }

    /// Build a report for compile-time expect failure
    fn buildComptimeExpectFailedReport(self: *Self, data: ComptimeExpectFailed) !Report {
        // Note: data.message contains raw source bytes which we don't display separately
        // since the source region highlighting already shows the expect expression
        var report = Report.init(self.gpa, "COMPTIME EXPECT FAILED", .runtime_error);
        errdefer report.deinit();

        try D.renderSlice(&.{
            D.bytes("This"),
            D.bytes("expect").withAnnotation(.keyword),
            D.bytes("failed during compile-time evaluation:"),
        }, self, &report);
        try report.document.addLineBreak();

        // Add source region highlighting - shows the expect expression with syntax highlighting
        const region_info = self.module_env.calcRegionInfo(data.region);
        try report.document.addSourceRegion(
            region_info,
            .error_highlight,
            self.filename,
            self.source,
            self.module_env.getLineStarts(),
        );

        return report;
    }

    /// Build a report for compile-time evaluation error
    fn buildComptimeEvalErrorReport(self: *Self, data: ComptimeEvalError) !Report {
        var report = Report.init(self.gpa, "COMPTIME EVAL ERROR", .runtime_error);
        errdefer report.deinit();

        const owned_error_name = try report.addOwnedString(
            self.problems.getExtraString(data.error_name),
        );

        try D.renderSlice(&.{
            D.bytes("This definition could not be evaluated at compile time:"),
        }, self, &report);
        try report.document.addLineBreak();

        // Add source region highlighting
        const region_info = self.module_env.calcRegionInfo(data.region);
        try report.document.addSourceRegion(
            region_info,
            .error_highlight,
            self.filename,
            self.source,
            self.module_env.getLineStarts(),
        );
        try report.document.addLineBreak();

        try D.renderSlice(&.{
            D.bytes("The evaluation failed with error:"),
        }, self, &report);
        try report.document.addLineBreak();
        try report.document.addLineBreak();
        try report.document.addCodeBlock(owned_error_name);

        return report;
    }

    fn buildNonExhaustiveMatchReport(self: *Self, data: NonExhaustiveMatch) !Report {
        var report = Report.init(self.gpa, "NON-EXHAUSTIVE MATCH", .runtime_error);
        errdefer report.deinit();

        try D.renderSlice(&.{
            D.bytes("This"),
            D.bytes("match").withAnnotation(.keyword),
            D.bytes("expression doesn't cover all possible cases:"),
        }, self, &report);
        try report.document.addLineBreak();

        // Add source region highlighting
        const match_region = self.can_ir.store.regions.get(@enumFromInt(@intFromEnum(data.match_expr)));
        const region_info = self.module_env.calcRegionInfo(match_region.*);
        try report.document.addSourceRegion(
            region_info,
            .error_highlight,
            self.filename,
            self.source,
            self.module_env.getLineStarts(),
        );
        try report.document.addLineBreak();

        const condition_type = self.getFormattedString(data.condition_snapshot);
        try D.renderSlice(&.{
            D.bytes("The value being matched on has type:"),
        }, self, &report);
        try report.document.addLineBreak();
        try report.document.addText("        ");
        try report.document.addAnnotated(condition_type, .type_variable);
        try report.document.addLineBreak();
        try report.document.addLineBreak();

        try D.renderSlice(&.{
            D.bytes("Missing patterns:"),
        }, self, &report);
        try report.document.addLineBreak();

        const missing_patterns = self.problems.getMissingPatterns(data.missing_patterns);
        for (missing_patterns) |pattern_idx| {
            const pattern = self.problems.getExtraString(pattern_idx);
            const owned_pattern = try report.addOwnedString(pattern);
            try report.document.addText("    ");
            try report.document.addCodeBlock(owned_pattern);
            try report.document.addLineBreak();
        }

        try report.document.addLineBreak();
        try D.renderSlice(&.{
            D.bytes("Hint: Add branches to handle these cases, or use"),
            D.bytes("_").withAnnotation(.keyword),
            D.bytes("to match anything."),
        }, self, &report);

        return report;
    }

    fn buildRedundantPatternReport(self: *Self, data: RedundantPattern) !Report {
        var report = Report.init(self.gpa, "REDUNDANT PATTERN", .warning);
        errdefer report.deinit();

        try D.renderSlice(&.{
            D.bytes("The"),
            D.num_ord(data.problem_branch_index + 1),
            D.bytes("branch of this"),
            D.bytes("match").withAnnotation(.keyword),
            D.bytes("is redundant:"),
        }, self, &report);
        try report.document.addLineBreak();

        // Cast Expr.Idx to Var (they're parallel arrays)
        try self.addSourceHighlight(&report, regionIdxFrom(data.match_expr));
        try report.document.addLineBreak();

        try D.renderSlice(&.{
            D.bytes("This pattern can never match because earlier patterns already cover all the values it would match."),
        }, self, &report);

        return report;
    }

    fn buildUnmatchablePatternReport(self: *Self, data: UnmatchablePattern) !Report {
        var report = Report.init(self.gpa, "UNMATCHABLE PATTERN", .warning);
        errdefer report.deinit();

        try D.renderSlice(&.{
            D.bytes("The"),
            D.num_ord(data.problem_branch_index + 1),
            D.bytes("branch of this"),
            D.bytes("match").withAnnotation(.keyword),
            D.bytes("can never match:"),
        }, self, &report);
        try report.document.addLineBreak();

        // Cast Expr.Idx to Var (they're parallel arrays)
        try self.addSourceHighlight(&report, regionIdxFrom(data.match_expr));
        try report.document.addLineBreak();

        try D.renderSlice(&.{
            D.bytes("This pattern matches a type that has no possible values (an uninhabited type), so no value can ever match it."),
        }, self, &report);

        return report;
    }

    // helpers //

    /// Get a number string ("1", "2", ...)
    fn getNumOwned(self: *Self, report: *Report, n: u32) ![]const u8 {
        self.bytes_buf.clearRetainingCapacity();
        try self.bytes_buf.writer().print("{d}", .{n});
        return try report.addOwnedString(self.bytes_buf.items);
    }

    /// Get an ordinal string (1st, 2nd, 3rd, etc.) as an owned string in the report.
    /// Consolidates the common pattern of: clear buffer -> append ordinal -> add owned string.
    fn getOrdinalOwned(self: *Self, report: *Report, index: u32) ![]const u8 {
        self.bytes_buf.clearRetainingCapacity();
        try appendOrdinal(&self.bytes_buf, index);
        return try report.addOwnedString(self.bytes_buf.items);
    }

    // Given a buffer and a number, write a the human-readably ordinal number
    // Note that the caller likely needs to clear the buffer before calling this function
    fn appendOrdinal(buf: *std.array_list.Managed(u8), n: u32) !void {
        switch (n) {
            1 => try buf.appendSlice("first"),
            2 => try buf.appendSlice("second"),
            3 => try buf.appendSlice("third"),
            4 => try buf.appendSlice("fourth"),
            5 => try buf.appendSlice("fifth"),
            6 => try buf.appendSlice("sixth"),
            7 => try buf.appendSlice("seventh"),
            8 => try buf.appendSlice("eighth"),
            9 => try buf.appendSlice("ninth"),
            10 => try buf.appendSlice("tenth"),
            else => {
                // Using character arrays to avoid typo checker flagging these strings as typos
                // (e.g. it thinks ['n', 'd'] is a typo of "and") - and that's a useful typo
                // to catch, so we're sacrificing readability of this particular code snippet
                // for the sake of catching actual typos of "and" elsewhere in the code base.
                const suffix = if (n % 100 >= 11 and n % 100 <= 13) &[_]u8{ 't', 'h' } else switch (n % 10) {
                    1 => &[_]u8{ 's', 't' },
                    2 => &[_]u8{ 'n', 'd' },
                    3 => &[_]u8{ 'r', 'd' },
                    else => &[_]u8{ 't', 'h' },
                };
                try buf.writer().print("{d}{s}", .{ n, suffix });
            },
        }
    }

    /// Get the formatted string for a snapshot.
    /// Returns a placeholder if the formatted string is missing, allowing error reporting
    /// to continue gracefully even if snapshots are incomplete.
    fn getFormattedString(self: *const Self, idx: SnapshotContentIdx) []const u8 {
        return self.snapshots.getFormattedString(idx) orelse "<unknown type>";
    }

    /// Returns the operator symbol for a given method ident, or null if not an operator method.
    /// Maps method idents like plus, minus, times, div_by to their corresponding operator symbols.
    fn getOperatorForMethod(self: *const Self, method_ident: Ident.Idx) ?[]const u8 {
        const idents = self.can_ir.idents;
        if (method_ident == idents.plus) return "+";
        if (method_ident == idents.minus) return "-";
        if (method_ident == idents.times) return "*";
        if (method_ident == idents.div_by) return "/";
        if (method_ident == idents.div_trunc_by) return "//";
        if (method_ident == idents.rem_by) return "%";
        if (method_ident == idents.negate) return "-";
        if (method_ident == idents.is_eq) return "==";
        if (method_ident == idents.is_lt) return "<";
        if (method_ident == idents.is_lte) return "<=";
        if (method_ident == idents.is_gt) return ">";
        if (method_ident == idents.is_gte) return ">=";
        if (method_ident == idents.not) return "not";
        return null;
    }
};
