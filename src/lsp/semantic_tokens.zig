//! Semantic token extraction for the Roc LSP.
//!
//! This module provides functionality to extract semantic tokens from Roc source code
//! and encode them in the LSP delta-encoded format for syntax highlighting.
//!
//! For a checked document, tokenization supplies source spans and lexical classes;
//! CIR supplies resolved identity and role, and checked types distinguish functions
//! from other values. A parse-tree-only path preserves highlighting for documents
//! which do not have checked CIR because parsing or error reporting rejected them.

const std = @import("std");
const Allocator = std.mem.Allocator;
const tokenize = @import("parse").tokenize;
const parse = @import("parse");
const can = @import("can");
const base = @import("base");
const collections = @import("collections");
const CirVisitor = @import("cir_visitor.zig").CirVisitor;
const VisitAction = @import("cir_visitor.zig").VisitAction;
const position = @import("position.zig");

const Token = tokenize.Token;
const LineOffsets = position.LineOffsets;
const ModuleEnv = can.ModuleEnv;
const AST = parse.AST;
const CIR = can.CIR;
const Region = base.Region;

/// The semantic token types, named as the LSP specification spells them. A
/// token is encoded as its type's value, and `capabilities.TOKEN_TYPES` is the
/// legend generated from these names in value order.
pub const SemanticType = enum(u32) {
    namespace = 0, // module names
    type = 1, // UpperIdent, type keywords
    parameter = 2, // function parameters (requires AST context)
    variable = 3, // LowerIdent
    property = 4, // record fields
    enumMember = 5, // tags
    function = 6, // function names (requires AST context)
    keyword = 7, // keywords
    string = 8, // string literals
    number = 9, // numeric literals
    operator = 10, // operators
    comment = 11, // comments (stripped by tokenizer)
    typeParameter = 12, // type variables
};

/// A semantic token with absolute position information.
pub const SemanticToken = struct {
    line: u32,
    start_char: u32,
    length: u32,
    token_type: u32,
    modifiers: u32 = 0,

    /// Comparison function for sorting tokens by position.
    pub fn lessThan(_: void, a: SemanticToken, b: SemanticToken) bool {
        if (a.line != b.line) return a.line < b.line;
        return a.start_char < b.start_char;
    }
};

/// Maps a Roc Token.Tag to an LSP semantic type index.
/// Returns null for tokens that should not be highlighted (punctuation, etc.).
///
/// Classification is driven by `Token.Tag.highlightCategory`, the single source
/// of truth shared with the playground's HTML token view; this function only
/// projects each category onto the LSP `SemanticType` index it corresponds to.
pub fn tokenTagToSemanticType(tag: Token.Tag) ?u32 {
    return switch (tag.highlightCategory()) {
        .keyword => @backingInt(SemanticType.keyword),
        .type => @backingInt(SemanticType.type),
        .variable => @backingInt(SemanticType.variable),
        .field => @backingInt(SemanticType.property),
        .tag => @backingInt(SemanticType.enumMember),
        .number => @backingInt(SemanticType.number),
        .string => @backingInt(SemanticType.string),
        .operator => @backingInt(SemanticType.operator),

        // Brackets, structural punctuation, and non-highlighted tokens carry no
        // semantic-token type.
        .bracket, .punctuation, .default => null,
    };
}

/// Classifies a token using the syntax encoded by its neighboring token tags.
/// A dotted lowercase name followed immediately by `(` cannot be record field
/// access. The parser separately distinguishes an attached method call from an
/// uppercase-qualified lookup; both use the LSP `function` token category.
fn tokenSemanticTypeAt(tags: []const Token.Tag, token_index: usize) ?u32 {
    const tag = tags[token_index];

    if (tag == .NoSpaceDotLowerIdent and
        token_index + 1 < tags.len and
        tags[token_index + 1] == .NoSpaceOpenRound)
    {
        return @backingInt(SemanticType.function);
    }

    return tokenTagToSemanticType(tag);
}

/// Extracts semantic tokens from Roc source code.
/// Returns a list of SemanticToken structs with absolute positions.
pub fn extractSemanticTokens(
    allocator: std.mem.Allocator,
    source: []const u8,
    info: *const LineOffsets,
) Allocator.Error![]SemanticToken {
    var module_env = ModuleEnv.init(allocator, source) catch return error.OutOfMemory;
    defer module_env.deinit();

    // Parse the source. Syntax errors are reported through the AST diagnostics.
    const parse_ast = try parse.file(allocator, &module_env.common);
    defer parse_ast.deinit();

    const token_count = parse_ast.tokens.tokens.len;
    const classes = try allocator.alloc(Class, token_count);
    defer allocator.free(classes);
    @memset(classes, .default);

    var classifier = Classifier.init(allocator, parse_ast, classes);
    defer classifier.deinit();
    try classifier.walkFile();

    var tokens: std.ArrayListUnmanaged(SemanticToken) = .empty;
    errdefer tokens.deinit(allocator);
    try emitTokens(allocator, parse_ast, classes, source, info, &tokens);
    return tokens.toOwnedSlice(allocator);
}

/// Extract semantic tokens from the checked module retained by the LSP build.
///
/// The token stream owns spelling and exact source extents. CIR owns identifier
/// identity and role, and the checked type store owns value-vs-function
/// classification. The parse tree is used only to retain useful highlighting
/// for source which did not reach CIR because parsing or canonicalization
/// reported an error.
pub fn extractSemanticTokensFromChecked(
    allocator: std.mem.Allocator,
    source: []const u8,
    info: *const LineOffsets,
    module_env: *ModuleEnv,
) Allocator.Error![]SemanticToken {
    var parse_env = ModuleEnv.init(allocator, source) catch return error.OutOfMemory;
    defer parse_env.deinit();

    const parse_ast = try parse.file(allocator, &parse_env.common);
    defer parse_ast.deinit();

    const token_count = parse_ast.tokens.tokens.len;
    const classes = try allocator.alloc(Class, token_count);
    defer allocator.free(classes);
    @memset(classes, .default);

    var classifier = CheckedClassifier{
        .allocator = allocator,
        .module_env = module_env,
        .source = source,
        .tags = parse_ast.tokens.tokens.items(.tag),
        .regions = parse_ast.tokens.tokens.items(.region),
        .classes = classes,
    };
    defer classifier.parameter_patterns.deinit(allocator);
    try classifier.classify();

    var tokens: std.ArrayListUnmanaged(SemanticToken) = .empty;
    errdefer tokens.deinit(allocator);
    try emitTokens(allocator, parse_ast, classes, source, info, &tokens);
    return tokens.toOwnedSlice(allocator);
}

/// What the parse tree says about a token. `.default` leaves the token to the
/// category of its tag; `.skip` means it is punctuation here, whatever its tag.
const Class = enum(u8) {
    default,
    skip,
    namespace,
    type,
    /// The name a type declaration introduces.
    type_declaration,
    type_parameter,
    parameter,
    variable,
    property,
    enum_member,
    function,

    fn semanticType(self: Class) ?SemanticType {
        return switch (self) {
            .default, .skip => null,
            .namespace => .namespace,
            .type, .type_declaration => .type,
            .type_parameter => .typeParameter,
            .parameter => .parameter,
            .variable => .variable,
            .property => .property,
            .enum_member => .enumMember,
            .function => .function,
        };
    }

    fn modifiers(self: Class) u32 {
        return if (self == .type_declaration) modifier_declaration else 0;
    }
};

/// Classifies source-name tokens from one traversal of checked CIR. Auxiliary
/// CIR nodes retain the exact source regions for names which are not expressions
/// or patterns themselves (record fields, method names, imports, and headers).
const CheckedClassifier = struct {
    allocator: Allocator,
    module_env: *ModuleEnv,
    source: []const u8,
    tags: []const Token.Tag,
    regions: []const Region,
    classes: []Class,
    parameter_patterns: std.AutoHashMapUnmanaged(CIR.Pattern.Idx, void) = .empty,
    oom: ?Allocator.Error = null,

    fn classify(self: *CheckedClassifier) Allocator.Error!void {
        try self.collectParameterPatterns();

        var visitor = CirVisitor(CheckedClassifier).init(self, .{
            .visit_expr_pre = visitExpr,
            .visit_stmt_pre = visitStatement,
            .visit_pattern_pre = visitPattern,
            .visit_type_anno_pre = visitTypeAnno,
        });

        for (self.module_env.store.sliceDefs(self.module_env.all_defs)) |def_idx| {
            const def = self.module_env.store.getDef(def_idx);
            visitor.walkPattern(&self.module_env.store, def.pattern);
            visitor.walkExpr(&self.module_env.store, def.expr);
            if (def.annotation) |annotation_idx| self.classifyAnnotation(&visitor, annotation_idx, def.pattern);
        }
        visitor.walkModule(&self.module_env.store, self.module_env.all_statements);
    }

    fn collectParameterPatterns(self: *CheckedClassifier) Allocator.Error!void {
        const Context = struct {
            classifier: *CheckedClassifier,

            fn addPattern(ctx: *@This(), pattern_idx: CIR.Pattern.Idx, pattern: CIR.Pattern) VisitAction {
                switch (pattern) {
                    .assign, .var_assign, .as => ctx.classifier.parameter_patterns.put(ctx.classifier.allocator, pattern_idx, {}) catch |err| {
                        ctx.classifier.oom = err;
                        return .stop;
                    },
                    .applied_tag,
                    .nominal,
                    .nominal_external,
                    .record_destructure,
                    .list,
                    .tuple,
                    .num_literal,
                    .frac_f32_literal,
                    .frac_f64_literal,
                    .small_dec_literal,
                    .dec_literal,
                    .num_from_numeral_literal,
                    .str_literal,
                    .str_interpolation,
                    .underscore,
                    .runtime_error,
                    => {},
                    .deferred_import_ref => base.invariant("compiler invariant violated: deferred import reference pattern reached a stage that runs after import resolution", .{}),
                }
                return .continue_traversal;
            }

            fn visitExpr(ctx: *@This(), _: CIR.Expr.Idx, expr: CIR.Expr) VisitAction {
                const args = if (expr == .e_lambda)
                    expr.e_lambda.args
                else if (expr == .e_hosted_lambda)
                    expr.e_hosted_lambda.args
                else
                    return .continue_traversal;

                for (ctx.classifier.module_env.store.slicePatterns(args)) |arg_idx| {
                    var pattern_visitor = CirVisitor(@This()).init(ctx, .{ .visit_pattern_pre = addPattern });
                    pattern_visitor.walkPattern(&ctx.classifier.module_env.store, arg_idx);
                    if (ctx.classifier.oom != null) return .stop;
                }
                return .continue_traversal;
            }
        };

        var ctx = Context{ .classifier = self };
        var visitor = CirVisitor(Context).init(&ctx, .{ .visit_expr_pre = Context.visitExpr });
        for (self.module_env.store.sliceDefs(self.module_env.all_defs)) |def_idx| {
            visitor.walkExpr(&self.module_env.store, self.module_env.store.getDef(def_idx).expr);
            if (visitor.stopped) break;
        }
        if (!visitor.stopped) visitor.walkModule(&self.module_env.store, self.module_env.all_statements);
        if (self.oom) |err| return err;
    }

    fn isParameter(self: *const CheckedClassifier, pattern_idx: CIR.Pattern.Idx) bool {
        return self.parameter_patterns.contains(pattern_idx);
    }

    fn patternClass(self: *const CheckedClassifier, pattern_idx: CIR.Pattern.Idx) Class {
        if (self.isParameter(pattern_idx)) return .parameter;
        return if (self.module_env.types.varResolvesToFunction(ModuleEnv.varFrom(pattern_idx))) .function else .variable;
    }

    fn exprClass(self: *const CheckedClassifier, expr_idx: CIR.Expr.Idx) Class {
        return if (self.module_env.types.varResolvesToFunction(ModuleEnv.varFrom(expr_idx))) .function else .variable;
    }

    fn classPriority(class: Class) u8 {
        return switch (class) {
            .default => 0,
            .namespace, .type, .type_parameter, .variable => 1,
            .parameter, .function, .enum_member => 2,
            .property, .type_declaration => 3,
            .skip => 4,
        };
    }

    fn set(self: *CheckedClassifier, token_index: usize, class: Class) void {
        if (classPriority(class) >= classPriority(self.classes[token_index])) self.classes[token_index] = class;
    }

    fn firstTokenAtOrAfter(self: *const CheckedClassifier, offset: u32) usize {
        var low: usize = 0;
        var high = self.regions.len;
        while (low < high) {
            const mid = low + (high - low) / 2;
            if (self.regions[mid].end.offset <= offset) low = mid + 1 else high = mid;
        }
        return low;
    }

    fn bareTokenText(self: *const CheckedClassifier, token_index: usize) []const u8 {
        const region = self.regions[token_index];
        const start = region.start.offset + namePrefixLen(self.tags[token_index]);
        if (start >= region.end.offset or region.end.offset > self.source.len) return "";
        return self.source[start..region.end.offset];
    }

    fn identTail(name: []const u8) []const u8 {
        const dot = std.mem.findScalarLast(u8, name, '.') orelse return name;
        return name[dot + 1 ..];
    }

    fn setIdent(self: *CheckedClassifier, region: Region, ident: base.Ident.Idx, class: Class) void {
        const name = identTail(self.module_env.common.idents.getText(ident));
        var i = self.firstTokenAtOrAfter(region.start.offset);
        while (i < self.regions.len and self.regions[i].start.offset < region.end.offset) : (i += 1) {
            if (std.mem.eql(u8, self.bareTokenText(i), name)) {
                self.set(i, class);
                return;
            }
        }
    }

    fn setLastIdent(self: *CheckedClassifier, region: Region, ident: base.Ident.Idx, class: Class) void {
        const name = identTail(self.module_env.common.idents.getText(ident));
        var matched: ?usize = null;
        var i = self.firstTokenAtOrAfter(region.start.offset);
        while (i < self.regions.len and self.regions[i].start.offset < region.end.offset) : (i += 1) {
            if (std.mem.eql(u8, self.bareTokenText(i), name)) matched = i;
        }
        if (matched) |token_index| self.set(token_index, class);
    }

    fn setFirstIdentifier(self: *CheckedClassifier, region: Region, class: Class) void {
        var i = self.firstTokenAtOrAfter(region.start.offset);
        while (i < self.regions.len and self.regions[i].start.offset < region.end.offset) : (i += 1) {
            if (isIdentifierTag(self.tags[i])) {
                self.set(i, class);
                return;
            }
        }
    }

    fn setTagsInRegion(self: *CheckedClassifier, region: Region, tag: Token.Tag, class: Class) void {
        var i = self.firstTokenAtOrAfter(region.start.offset);
        while (i < self.regions.len and self.regions[i].start.offset < region.end.offset) : (i += 1) {
            if (self.tags[i] == tag) self.set(i, class);
        }
    }

    fn nodeRegion(self: *const CheckedClassifier, idx: anytype) Region {
        return self.module_env.store.getRegionAt(ModuleEnv.nodeIdxFrom(idx));
    }

    fn classifyAnnotation(
        self: *CheckedClassifier,
        visitor: anytype,
        annotation_idx: CIR.Annotation.Idx,
        pattern_idx: CIR.Pattern.Idx,
    ) void {
        const annotation = self.module_env.store.getAnnotation(annotation_idx);
        if (annotation.name_region) |region| self.setFirstIdentifier(region, self.patternClass(pattern_idx));
        visitor.walkTypeAnno(&self.module_env.store, annotation.anno);
        if (annotation.where) |where| self.classifyWhere(where);
    }

    fn classifyWhere(self: *CheckedClassifier, where: CIR.WhereClause.Span) void {
        for (self.module_env.store.sliceWhereClauses(where)) |where_idx| {
            const clause = self.module_env.store.getWhereClause(where_idx);
            if (clause == .w_method) self.setIdent(self.nodeRegion(where_idx), clause.w_method.method_name, .function);
        }
    }

    fn visitStatement(self: *CheckedClassifier, stmt_idx: CIR.Statement.Idx, statement: CIR.Statement) VisitAction {
        switch (statement) {
            .s_import => |import| {
                const region = self.nodeRegion(stmt_idx);
                self.setIdent(region, import.module_name_tok, .type);
                if (import.qualifier_tok) |qualifier| self.setIdent(region, qualifier, .namespace);
                if (import.alias_tok) |alias| self.setIdent(region, alias, .type);
                self.setTagsInRegion(region, .OpSlash, .skip);
                for (self.module_env.store.sliceExposedItems(import.exposes)) |item_idx| {
                    const item = self.module_env.store.getExposedItem(item_idx);
                    const item_region = self.nodeRegion(item_idx);
                    const class: Class = if (item.kind == .type) .type else .variable;
                    self.setIdent(item_region, item.name, class);
                    if (item.alias) |alias| self.setIdent(item_region, alias, class);
                }
            },
            .s_alias_decl => |decl| self.classifyTypeHeader(decl.header),
            .s_nominal_decl => |decl| self.classifyTypeHeader(decl.header),
            .s_where_alias_decl => |decl| {
                self.classifyTypeHeader(decl.header);
                self.classifyWhere(decl.where);
            },
            .s_decl => |decl| if (decl.anno) |anno| self.classifyAnnotationMetadata(anno, decl.pattern),
            .s_var => |decl| if (decl.anno) |anno| self.classifyAnnotationMetadata(anno, decl.pattern_idx),
            .s_var_uninitialized => |decl| if (decl.anno) |anno| self.classifyAnnotationMetadata(anno, decl.pattern_idx),
            .s_type_anno => |anno| {
                const region = self.nodeRegion(stmt_idx);
                self.setIdent(region, anno.name, .variable);
                if (anno.where) |where| self.classifyWhere(where);
            },
            .s_type_var_alias => |alias| {
                const region = self.nodeRegion(stmt_idx);
                self.setIdent(region, alias.alias_name, .type);
                self.setIdent(region, alias.type_var_name, .type_parameter);
            },
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
            .s_runtime_error,
            => {},
        }
        return .continue_traversal;
    }

    fn classifyTypeHeader(self: *CheckedClassifier, header_idx: CIR.TypeHeader.Idx) void {
        const header = self.module_env.store.getTypeHeader(header_idx);
        self.setIdent(self.nodeRegion(header_idx), header.relative_name, .type_declaration);
        var visitor = CirVisitor(CheckedClassifier).init(self, .{ .visit_type_anno_pre = visitTypeAnno });
        for (self.module_env.store.sliceTypeAnnos(header.args)) |arg| visitor.walkTypeAnno(&self.module_env.store, arg);
    }

    fn classifyAnnotationMetadata(
        self: *CheckedClassifier,
        annotation_idx: CIR.Annotation.Idx,
        pattern_idx: CIR.Pattern.Idx,
    ) void {
        const annotation = self.module_env.store.getAnnotation(annotation_idx);
        if (annotation.name_region) |region| self.setFirstIdentifier(region, self.patternClass(pattern_idx));
        if (annotation.where) |where| self.classifyWhere(where);
    }

    fn visitExpr(self: *CheckedClassifier, expr_idx: CIR.Expr.Idx, expr: CIR.Expr) VisitAction {
        const region = self.module_env.store.getExprRegion(expr_idx);
        switch (expr) {
            .e_lookup_local => |lookup| self.setFirstIdentifier(region, self.patternClass(lookup.pattern_idx)),
            .e_lookup_external => |lookup| self.setIdent(lookup.region, lookup.ident_idx, self.exprClass(expr_idx)),
            .e_lookup_associated_local => |lookup| self.setLastIdent(region, lookup.item_ident, self.exprClass(expr_idx)),
            .e_lookup_associated => |lookup| self.setLastIdent(region, lookup.item_ident, self.exprClass(expr_idx)),
            .e_lookup_associated_resolved => |lookup| self.setLastIdent(region, lookup.source_ident, self.exprClass(expr_idx)),
            .e_lookup_required => |lookup| {
                const required = self.module_env.requires_types.items.items[@backingInt(lookup.requires_idx)];
                self.setIdent(region, required.ident, self.exprClass(expr_idx));
            },
            .e_tag => |tag| self.setIdent(region, tag.name, .enum_member),
            .e_zero_argument_tag => |tag| self.setIdent(region, tag.name, .enum_member),
            .e_record => |record| {
                for (self.module_env.store.sliceRecordFields(record.fields)) |field_idx| {
                    const field = self.module_env.store.getRecordField(field_idx);
                    self.setIdent(self.nodeRegion(field_idx), field.name, .property);
                }
                for (self.module_env.store.sliceUnsetFields(record.unsets)) |field_idx| {
                    const field = self.module_env.store.getUnsetField(field_idx);
                    self.setIdent(self.nodeRegion(field_idx), field.name, .property);
                }
            },
            .e_field_access => |access| {
                var segment_idx = access.segments.start;
                var remaining = access.segments.len;
                while (remaining > 0) : ({
                    segment_idx = @fromBackingInt(@intCast(@backingInt(segment_idx) + 1));
                    remaining -= 1;
                }) {
                    const segment = self.module_env.store.getFieldAccessSegment(segment_idx);
                    self.setIdent(self.module_env.store.getFieldAccessSegmentRegion(segment_idx), segment.name, .property);
                }
            },
            .e_method_call => |call| self.setIdent(call.method_name_region, call.method_name, .function),
            .e_dispatch_call => |call| self.setIdent(call.method_name_region, call.method_name, .function),
            .e_type_method_call => |call| self.setIdent(call.method_name_region, call.method_name, .function),
            .e_type_dispatch_call => |call| self.setIdent(call.method_name_region, call.method_name, .function),
            .e_num,
            .e_frac_f32,
            .e_frac_f64,
            .e_dec,
            .e_dec_small,
            .e_num_from_numeral,
            .e_typed_int,
            .e_typed_frac,
            .e_typed_num_from_numeral,
            .e_str_segment,
            .e_str,
            .e_bytes_literal,
            .e_list,
            .e_empty_list,
            .e_tuple,
            .e_match,
            .e_if,
            .e_call,
            .e_empty_record,
            .e_block,
            .e_nominal,
            .e_nominal_external,
            .e_closure,
            .e_lambda,
            .e_binop,
            .e_unary_minus,
            .e_interpolation,
            .e_structural_eq,
            .e_structural_hash,
            .e_method_eq,
            .e_tuple_access,
            .e_runtime_error,
            .e_crash,
            .e_dbg,
            .e_expect_err,
            .e_expect,
            .e_ellipsis,
            .e_anno_only,
            .e_derived_method,
            .e_return,
            .e_break,
            .e_for,
            .e_hosted_lambda,
            .e_run_low_level,
            => {},
            .e_deferred_import_ref => base.invariant("compiler invariant violated: deferred import reference reached a stage that runs after import resolution", .{}),
        }
        return .continue_traversal;
    }

    fn visitPattern(self: *CheckedClassifier, pattern_idx: CIR.Pattern.Idx, pattern: CIR.Pattern) VisitAction {
        const region = self.module_env.store.getPatternRegion(pattern_idx);
        switch (pattern) {
            inline .assign, .var_assign, .as => |binding| self.setIdent(region, binding.ident, self.patternClass(pattern_idx)),
            .applied_tag => |tag| self.setIdent(region, tag.name, .enum_member),
            .record_destructure => |record| {
                for (self.module_env.store.sliceRecordDestructs(record.destructs)) |field_idx| {
                    const field = self.module_env.store.getRecordDestruct(field_idx);
                    self.setIdent(self.nodeRegion(field_idx), field.label, .property);
                }
            },
            .nominal,
            .nominal_external,
            .list,
            .tuple,
            .num_literal,
            .frac_f32_literal,
            .frac_f64_literal,
            .small_dec_literal,
            .dec_literal,
            .num_from_numeral_literal,
            .str_literal,
            .str_interpolation,
            .underscore,
            .runtime_error,
            => {},
            .deferred_import_ref => base.invariant("compiler invariant violated: deferred import reference pattern reached a stage that runs after import resolution", .{}),
        }
        return .continue_traversal;
    }

    fn visitTypeAnno(self: *CheckedClassifier, anno_idx: CIR.TypeAnno.Idx, anno: CIR.TypeAnno) VisitAction {
        const region = self.module_env.store.getTypeAnnoRegion(anno_idx);
        switch (anno) {
            .apply => |apply| self.setIdent(region, apply.name, .type),
            .lookup => |lookup| self.setIdent(region, lookup.name, .type),
            .rigid_var => |rigid| self.setIdent(region, rigid.name, .type_parameter),
            .rigid_var_lookup => self.setFirstIdentifier(region, .type_parameter),
            .tag => |tag| self.setIdent(region, tag.name, .enum_member),
            .record => |record| {
                for (self.module_env.store.sliceAnnoRecordFields(record.fields)) |field_idx| {
                    const field = self.module_env.store.getAnnoRecordField(field_idx);
                    self.setIdent(self.nodeRegion(field_idx), field.name, .property);
                }
            },
            .underscore, .tag_union, .tuple, .@"fn", .parens, .malformed => {},
        }
        return .continue_traversal;
    }
};

fn isIdentifierTag(tag: Token.Tag) bool {
    return tag == .LowerIdent or
        tag == .UpperIdent or
        tag == .DotLowerIdent or
        tag == .NoSpaceDotLowerIdent or
        tag == .DotQuestionLowerIdent or
        tag == .NoSpaceDotQuestionLowerIdent or
        tag == .DotUpperIdent or
        tag == .NoSpaceDotUpperIdent;
}

/// Bit of the `declaration` modifier, matching TOKEN_MODIFIERS in capabilities.zig.
pub const modifier_declaration: u32 = 1 << 0;

/// The bytes of a dotted token that are punctuation rather than part of the name.
fn namePrefixLen(tag: Token.Tag) u32 {
    if (isDotUpperIdent(tag) or tag == .DotLowerIdent or tag == .NoSpaceDotLowerIdent or
        tag == .DotInt or tag == .NoSpaceDotInt) return 1;
    if (tag == .DotQuestionLowerIdent or tag == .NoSpaceDotQuestionLowerIdent) return 2;
    return 0;
}

fn isDotUpperIdent(tag: Token.Tag) bool {
    return tag == .DotUpperIdent or tag == .NoSpaceDotUpperIdent;
}

fn isStringText(tag: Token.Tag) bool {
    return tag == .StringPart or tag == .MalformedStringPart;
}

fn emitTokens(
    allocator: Allocator,
    ast: *const AST,
    classes: []const Class,
    source: []const u8,
    info: *const LineOffsets,
    out: *std.ArrayListUnmanaged(SemanticToken),
) Allocator.Error!void {
    const tags = ast.tokens.tokens.items(.tag);
    const regions = ast.tokens.tokens.items(.region);

    for (tags, regions, 0..) |tag, region, token_index| {
        const class = classes[token_index];
        if (class == .skip) continue;

        const follows_number = token_index > 0 and
            (tags[token_index - 1] == .Int or tags[token_index - 1] == .Float);
        // After a number this is the literal's type suffix, part of the literal.
        const is_suffix = class == .default and isDotUpperIdent(tag) and follows_number;

        const semantic_type: u32 = if (class.semanticType()) |semantic|
            @backingInt(semantic)
        else if (tag == .OpColon or tag == .OpBar)
            // A colon or a lambda bar delimits; it does not operate on anything.
            continue
        else if (is_suffix)
            @backingInt(SemanticType.number)
        else
            tokenSemanticTypeAt(tags, token_index) orelse continue;

        // A number's type suffix keeps its dot, as the literal is one unit.
        const start = region.start.offset + if (is_suffix) 0 else namePrefixLen(tag);
        const end = region.end.offset;
        if (start >= end or end > source.len) continue;

        if (isStringText(tag)) {
            // Leave escape sequences out, so that a client's own highlighting of
            // them shows through the string colour.
            var segment_start = start;
            var i = start;
            while (i < end) {
                if (source[i] != '\\' or i + 1 >= end) {
                    i += 1;
                    continue;
                }
                try appendSpan(allocator, out, info, segment_start, i, semantic_type, 0);
                i += 2;
                if (source[i - 1] == 'u' and i < end and source[i] == '(') {
                    while (i < end and source[i] != ')') i += 1;
                    if (i < end) i += 1;
                }
                segment_start = i;
            }
            try appendSpan(allocator, out, info, segment_start, end, semantic_type, 0);
        } else {
            try appendSpan(allocator, out, info, start, end, semantic_type, class.modifiers());
        }
    }
}

fn appendSpan(
    allocator: Allocator,
    out: *std.ArrayListUnmanaged(SemanticToken),
    info: *const LineOffsets,
    start: u32,
    end: u32,
    semantic_type: u32,
    modifiers: u32,
) Allocator.Error!void {
    if (start >= end) return;
    const start_pos = position.offsetToPosition(start, info);
    const end_pos = position.offsetToPosition(end, info);
    // LSP lengths are UTF-16 code units. A token that spans lines keeps its byte
    // length, which is what clients without multiline support clip anyway.
    const length = if (end_pos.line == start_pos.line and end_pos.character > start_pos.character)
        end_pos.character - start_pos.character
    else
        end - start;

    try out.append(allocator, .{
        .line = start_pos.line,
        .start_char = start_pos.character,
        .length = length,
        .token_type = semantic_type,
        .modifiers = modifiers,
    });
}

/// Walks the parse tree, recording what each name token is.
///
/// Names are resolved lexically: a lambda's parameters, the bindings of a match
/// branch or loop, and the declarations of a block are in scope for what they
/// enclose. Declarations are hoisted within their block, as Roc's are at the top
/// level, so a function can be recognised before its definition is reached.
///
/// The walk keeps its pending work on an explicit stack (design.md "Stack
/// Safety"). Each item does its own work and pushes what follows it in reverse,
/// so items run in the order a recursive walk would reach them. Bindings are
/// keyed by interned name: each one records the binding of the same name it
/// shadows, and `innermost` names the latest binding of each name in scope.
const Classifier = struct {
    allocator: Allocator,
    ast: *const AST,
    classes: []Class,
    bindings: std.ArrayListUnmanaged(Binding) = .empty,
    innermost: collections.DenseMap(base.Ident.Idx, u32),
    work: std.ArrayListUnmanaged(Work) = .empty,

    const no_binding = std.math.maxInt(u32);

    const Binding = struct {
        name: base.Ident.Idx,
        class: Class,
        /// The binding of the same name this one shadows, or `no_binding`.
        shadowed: u32,
    };

    const Work = union(enum) {
        expr: AST.Expr.Idx,
        /// After walking a call's callee: a called name is a function unless
        /// it is a parameter.
        callee: AST.Expr.Idx,
        field_segments: AST.FieldAccessSegment.Span,
        record_field: AST.RecordField.Idx,
        pattern: struct { idx: AST.Pattern.Idx, class: Class },
        pattern_record_field: struct { idx: AST.PatternRecordField.Idx, class: Class },
        bind: struct { token: Token.Idx, class: Class },
        set: struct { token: Token.Idx, class: Class },
        type_anno: AST.TypeAnno.Idx,
        /// After walking a tag union member: its head names a tag.
        tag_union_member: AST.TypeAnno.Idx,
        anno_record_field: AST.AnnoRecordField.Idx,
        where: AST.Collection.Idx,
        where_clause: AST.WhereClause.Idx,
        statement: AST.Statement.Idx,
        bind_statement: AST.Statement.Idx,
        statements: struct { span: AST.Statement.Span, hoist: bool },
        /// Statements whose bindings are in scope only for one another.
        scoped_statements: AST.Statement.Span,
        match_branch: AST.MatchBranch.Idx,
        /// A loop's pattern and body, after its iterable.
        for_body: struct { patt: AST.Pattern.Idx, body: AST.Expr.Idx },
        /// Leave a scope: drop every binding made since `bindings` had this
        /// length.
        end_scope: u32,
    };

    fn init(allocator: Allocator, ast: *const AST, classes: []Class) Classifier {
        return .{
            .allocator = allocator,
            .ast = ast,
            .classes = classes,
            .innermost = .init(allocator),
        };
    }

    fn deinit(self: *Classifier) void {
        self.bindings.deinit(self.allocator);
        self.innermost.deinit();
        self.work.deinit(self.allocator);
    }

    fn set(self: *Classifier, token: Token.Idx, class: Class) void {
        if (token < self.classes.len) self.classes[token] = class;
    }

    fn setAll(self: *Classifier, span: Token.Span, class: Class) void {
        for (self.ast.store.tokenSlice(span)) |token| self.set(token, class);
    }

    /// The interned name `text` spells, where `text` is all of `token`'s text
    /// or the part after its punctuation. The tokenizer interns an identifier
    /// token's name, which ends where the token does, so it is `text` exactly
    /// when the lengths agree; any other text is interned here.
    fn nameOf(self: *const Classifier, token: Token.Idx, text: []const u8) Allocator.Error!base.Ident.Idx {
        if (self.ast.tokens.resolveIdentifier(token)) |ident| {
            if (self.ast.env.getIdent(ident).len == text.len) return ident;
        }
        return self.ast.env.insertIdent(self.allocator, base.Ident.for_text(text));
    }

    fn bind(self: *Classifier, token: Token.Idx, class: Class) Allocator.Error!void {
        self.set(token, class);
        const name = try self.nameOf(token, self.ast.resolve(token));
        const slot = std.math.cast(u32, self.bindings.items.len) orelse return error.OutOfMemory;
        if (slot == no_binding) return error.OutOfMemory;
        try self.bindings.ensureUnusedCapacity(self.allocator, 1);
        const entry = try self.innermost.getOrPut(name);
        const shadowed = if (entry.found_existing) entry.value_ptr.* else no_binding;
        entry.value_ptr.* = slot;
        self.bindings.appendAssumeCapacity(.{ .name = name, .class = class, .shadowed = shadowed });
    }

    fn endScope(self: *Classifier, mark: u32) void {
        while (self.bindings.items.len > mark) {
            const binding = self.bindings.pop().?;
            if (binding.shadowed == no_binding) {
                _ = self.innermost.remove(binding.name);
            } else {
                self.innermost.getPtr(binding.name).?.* = binding.shadowed;
            }
        }
    }

    fn scopeMark(self: *const Classifier) u32 {
        // `bind` keeps the binding count within `u32`.
        return @intCast(self.bindings.items.len);
    }

    fn lookup(self: *const Classifier, name: base.Ident.Idx) ?Class {
        const slot = self.innermost.get(name) orelse return null;
        return self.bindings.items[slot].class;
    }

    fn isLambda(self: *const Classifier, expr_idx: AST.Expr.Idx) bool {
        return self.ast.store.getExpr(expr_idx) == .lambda;
    }

    fn isFunctionType(self: *const Classifier, anno_idx: AST.TypeAnno.Idx) bool {
        var anno = self.ast.store.getTypeAnno(anno_idx);
        while (anno == .parens) anno = self.ast.store.getTypeAnno(anno.parens.anno);
        return anno == .@"fn";
    }

    fn push(self: *Classifier, item: Work) Allocator.Error!void {
        try self.work.append(self.allocator, item);
    }

    fn pushExprs(self: *Classifier, exprs: []const AST.Expr.Idx) Allocator.Error!void {
        var i = exprs.len;
        while (i > 0) {
            i -= 1;
            try self.push(.{ .expr = exprs[i] });
        }
    }

    fn pushPatterns(self: *Classifier, patterns: []const AST.Pattern.Idx, class: Class) Allocator.Error!void {
        var i = patterns.len;
        while (i > 0) {
            i -= 1;
            try self.push(.{ .pattern = .{ .idx = patterns[i], .class = class } });
        }
    }

    fn pushTypeAnnos(self: *Classifier, annos: []const AST.TypeAnno.Idx) Allocator.Error!void {
        var i = annos.len;
        while (i > 0) {
            i -= 1;
            try self.push(.{ .type_anno = annos[i] });
        }
    }

    fn pushRecordFields(self: *Classifier, fields: AST.RecordField.Span) Allocator.Error!void {
        const items = self.ast.store.recordFieldSlice(fields);
        var i = items.len;
        while (i > 0) {
            i -= 1;
            try self.push(.{ .record_field = items[i] });
        }
    }

    fn walkFile(self: *Classifier) Allocator.Error!void {
        const file = self.ast.store.getFile();
        try self.push(.{ .statements = .{ .span = file.statements, .hoist = true } });
        while (self.work.pop()) |item| try self.step(item);
    }

    fn step(self: *Classifier, item: Work) Allocator.Error!void {
        switch (item) {
            .expr => |expr_idx| try self.stepExpr(expr_idx),
            .callee => |expr_idx| {
                const callee = self.ast.store.getExpr(expr_idx);
                if (callee != .ident) return;
                const token = callee.ident.token;
                if (token < self.classes.len and self.classes[token] != .parameter) self.set(token, .function);
            },
            .field_segments => |segments| {
                for (self.ast.store.fieldAccessSegmentSlice(segments)) |segment| {
                    self.set(segment.field_token, .property);
                }
            },
            .record_field => |field_idx| {
                const field = self.ast.store.getRecordField(field_idx);
                self.set(field.name, .property);
                if (field.value.asSupplied()) |value| try self.push(.{ .expr = value });
            },
            .pattern => |p| try self.stepPattern(p.idx, p.class),
            .pattern_record_field => |f| {
                const field = self.ast.store.getPatternRecordField(f.idx);
                if (field.value) |value| {
                    if (field.name) |name| self.set(name, .property);
                    try self.push(.{ .pattern = .{ .idx = value, .class = f.class } });
                } else if (field.name) |name| {
                    try self.bind(name, f.class);
                }
            },
            .bind => |b| try self.bind(b.token, b.class),
            .set => |s| self.set(s.token, s.class),
            .type_anno => |anno_idx| try self.stepTypeAnno(anno_idx),
            .tag_union_member => |tag_idx| {
                var head = self.ast.store.getTypeAnno(tag_idx);
                if (head == .apply) {
                    const args = self.ast.store.typeAnnoSlice(head.apply.args);
                    if (args.len == 0) return;
                    head = self.ast.store.getTypeAnno(args[0]);
                }
                if (head == .ty) self.set(head.ty.token, .enum_member);
            },
            .anno_record_field => |field_idx| {
                const field = self.ast.store.getAnnoRecordField(field_idx) catch return;
                self.set(field.name, .property);
                if (field.default_value) |default_value| try self.push(.{ .expr = default_value });
                try self.push(.{ .type_anno = field.ty });
            },
            .where => |where| {
                const collection = self.ast.store.getCollection(where);
                const clauses = self.ast.store.whereClauseSlice(.{ .span = collection.span });
                var i = clauses.len;
                while (i > 0) {
                    i -= 1;
                    try self.push(.{ .where_clause = clauses[i] });
                }
            },
            .where_clause => |clause_idx| switch (self.ast.store.getWhereClause(clause_idx)) {
                .mod_method => |m| {
                    self.set(m.var_tok, .type_parameter);
                    self.set(m.name_tok, .function);
                    try self.push(.{ .type_anno = m.anno });
                },
                .mod_alias => |m| {
                    self.set(m.var_tok, .type_parameter);
                    try self.push(.{ .type_anno = m.alias });
                },
                .malformed => {},
            },
            .statement => |stmt_idx| try self.stepStatement(stmt_idx),
            .bind_statement => |stmt_idx| {
                const statement = self.ast.store.getStatement(stmt_idx);
                if (statement == .decl) {
                    const d = statement.decl;
                    try self.push(.{ .pattern = .{
                        .idx = d.pattern,
                        .class = if (self.isLambda(d.body)) .function else .variable,
                    } });
                } else if (statement == .type_anno and self.isFunctionType(statement.type_anno.anno)) {
                    try self.bind(statement.type_anno.name, .function);
                }
            },
            .statements => |s| {
                const statements = self.ast.store.statementSlice(s.span);
                var i = statements.len;
                while (i > 0) {
                    i -= 1;
                    try self.push(.{ .statement = statements[i] });
                    if (!s.hoist) try self.push(.{ .bind_statement = statements[i] });
                }
                if (s.hoist) {
                    i = statements.len;
                    while (i > 0) {
                        i -= 1;
                        try self.push(.{ .bind_statement = statements[i] });
                    }
                }
            },
            .scoped_statements => |span| {
                try self.push(.{ .end_scope = self.scopeMark() });
                try self.push(.{ .statements = .{ .span = span, .hoist = false } });
            },
            .match_branch => |branch_idx| {
                const branch = self.ast.store.getBranch(branch_idx);
                try self.push(.{ .end_scope = self.scopeMark() });
                try self.push(.{ .expr = branch.body });
                if (branch.guard) |guard| try self.push(.{ .expr = guard });
                try self.push(.{ .pattern = .{ .idx = branch.pattern, .class = .variable } });
            },
            .for_body => |f| {
                try self.push(.{ .end_scope = self.scopeMark() });
                try self.push(.{ .expr = f.body });
                try self.push(.{ .pattern = .{ .idx = f.patt, .class = .variable } });
            },
            .end_scope => |mark| self.endScope(mark),
        }
    }

    fn stepStatement(self: *Classifier, stmt_idx: AST.Statement.Idx) Allocator.Error!void {
        switch (self.ast.store.getStatement(stmt_idx)) {
            .decl => |d| try self.push(.{ .expr = d.body }),
            .@"var" => |v| {
                try self.push(.{ .bind = .{ .token = v.name, .class = .variable } });
                if (v.body) |body| try self.push(.{ .expr = body });
            },
            inline .expr, .crash, .dbg => |e| try self.push(.{ .expr = e.expr }),
            .expect => |e| try self.push(.{ .expr = e.body }),
            .@"return" => |e| try self.push(.{ .expr = e.expr }),
            .@"for" => |f| {
                try self.push(.{ .for_body = .{ .patt = f.patt, .body = f.body } });
                try self.push(.{ .expr = f.expr });
            },
            .@"while" => |w| {
                try self.push(.{ .expr = w.body });
                try self.push(.{ .expr = w.cond });
            },
            .import => |i| {
                const target = i.target;
                var token = target.start_tok;
                while (token <= target.lastToken()) : (token += 1) {
                    const tag = self.ast.tokens.tokens.items(.tag)[token];
                    self.set(token, if (tag == .UpperIdent or isDotUpperIdent(tag)) .type else .skip);
                }
                if (target.qualifier_tok) |qualifier| self.set(qualifier, .namespace);
                if (i.alias_tok) |alias| self.set(alias, .type);
                for (self.ast.store.exposedItemSlice(i.exposes)) |item_idx| {
                    switch (self.ast.store.getExposedItem(item_idx)) {
                        .lower_ident => {},
                        .upper_ident => |u| {
                            self.set(u.ident, .type);
                            if (u.as) |alias| self.set(alias, .type);
                        },
                        .upper_ident_star => |u| self.set(u.ident, .type),
                        .malformed => {},
                    }
                }
            },
            .file_import => |f| {
                try self.bind(f.name_tok, .variable);
                self.set(f.type_tok, .type);
            },
            .type_decl => |t| {
                if (t.associated) |associated| try self.push(.{ .scoped_statements = associated.statements });
                if (t.where) |where| try self.push(.{ .where = where });
                try self.push(.{ .type_anno = t.anno });
                if (self.ast.store.getTypeHeader(t.header)) |header| {
                    self.set(header.name, .type_declaration);
                    try self.pushTypeAnnos(self.ast.store.typeAnnoSlice(header.args));
                } else |_| {}
            },
            .type_anno => |a| {
                self.set(a.name, if (self.isFunctionType(a.anno)) .function else .variable);
                if (a.where) |where| try self.push(.{ .where = where });
                try self.push(.{ .type_anno = a.anno });
            },
            .@"break", .malformed => {},
        }
    }

    /// Classify a pattern; the names it binds get `class`.
    fn stepPattern(self: *Classifier, pattern_idx: AST.Pattern.Idx, class: Class) Allocator.Error!void {
        switch (self.ast.store.getPattern(pattern_idx)) {
            .ident => |p| try self.bind(p.ident_tok, class),
            .var_ident => |p| try self.bind(p.ident_tok, class),
            .tag => |p| {
                self.setAll(p.qualifiers, .type);
                self.set(p.tag_tok, if (p.backing_value) .type else .enum_member);
                // A destructured payload holds values, never the function itself.
                const inner: Class = if (class == .function) .variable else class;
                try self.pushPatterns(self.ast.store.patternSlice(p.args), inner);
            },
            .record => |p| {
                const inner: Class = if (class == .function) .variable else class;
                const fields = self.ast.store.patternRecordFieldSlice(p.fields);
                var i = fields.len;
                while (i > 0) {
                    i -= 1;
                    try self.push(.{ .pattern_record_field = .{ .idx = fields[i], .class = inner } });
                }
            },
            .list => |p| try self.pushPatterns(self.ast.store.patternSlice(p.patterns), class),
            .tuple => |p| try self.pushPatterns(self.ast.store.patternSlice(p.patterns), class),
            .alternatives => |p| try self.pushPatterns(self.ast.store.patternSlice(p.patterns), class),
            .list_rest => |p| if (p.name) |name| try self.bind(name, class),
            .as => |p| {
                try self.push(.{ .bind = .{ .token = p.name, .class = class } });
                try self.push(.{ .pattern = .{ .idx = p.pattern, .class = class } });
            },
            .int, .frac, .typed_int, .typed_frac, .string, .single_quote, .underscore, .malformed => {},
        }
    }

    fn stepTypeAnno(self: *Classifier, anno_idx: AST.TypeAnno.Idx) Allocator.Error!void {
        switch (self.ast.store.getTypeAnno(anno_idx)) {
            .ty => |t| {
                self.setAll(t.qualifiers, .type);
                self.set(t.token, .type);
            },
            .ty_var => |t| self.set(t.tok, .type_parameter),
            .underscore_type_var => |t| self.set(t.tok, .type_parameter),
            .apply => |a| try self.pushTypeAnnos(self.ast.store.typeAnnoSlice(a.args)),
            .tag_union => |u| {
                switch (u.ext) {
                    .named => |n| try self.push(.{ .type_anno = n.anno }),
                    .closed, .open => {},
                }
                const tags = self.ast.store.typeAnnoSlice(u.tags);
                var i = tags.len;
                while (i > 0) {
                    i -= 1;
                    try self.push(.{ .tag_union_member = tags[i] });
                    try self.push(.{ .type_anno = tags[i] });
                }
            },
            .tuple => |t| try self.pushTypeAnnos(self.ast.store.typeAnnoSlice(t.annos)),
            .record => |r| {
                switch (r.ext) {
                    .named => |n| try self.push(.{ .type_anno = n.anno }),
                    .closed, .open => {},
                }
                const fields = self.ast.store.annoRecordFieldSlice(r.fields);
                var i = fields.len;
                while (i > 0) {
                    i -= 1;
                    try self.push(.{ .anno_record_field = fields[i] });
                }
            },
            .@"fn" => |f| {
                try self.push(.{ .type_anno = f.ret });
                try self.pushTypeAnnos(self.ast.store.typeAnnoSlice(f.args));
            },
            .parens => |p| try self.push(.{ .type_anno = p.anno }),
            .underscore, .malformed => {},
        }
    }

    /// Push the walk of a call's callee, followed by its classification.
    fn pushCallee(self: *Classifier, expr_idx: AST.Expr.Idx) Allocator.Error!void {
        try self.push(.{ .callee = expr_idx });
        try self.push(.{ .expr = expr_idx });
    }

    fn stepExpr(self: *Classifier, expr_idx: AST.Expr.Idx) Allocator.Error!void {
        switch (self.ast.store.getExpr(expr_idx)) {
            .ident => |i| {
                const qualifiers = self.ast.store.tokenSlice(i.qualifiers);
                for (qualifiers) |qualifier| self.set(qualifier, .type);
                if (qualifiers.len == 0) {
                    const text = self.ast.resolve(i.token);
                    // A qualified name arrives as a dotted token; resolve the bare name.
                    const name = text[@min(text.len, namePrefixLen(self.ast.tokens.tokens.items(.tag)[i.token]))..];
                    if (self.lookup(try self.nameOf(i.token, name))) |class| self.set(i.token, class);
                } else {
                    self.set(i.token, .variable);
                }
            },
            .tag => |t| {
                self.setAll(t.qualifiers, .type);
                self.set(t.token, .enum_member);
            },
            .lambda => |l| {
                try self.push(.{ .end_scope = self.scopeMark() });
                try self.push(.{ .expr = l.body });
                try self.pushPatterns(self.ast.store.patternSlice(l.args), .parameter);
            },
            .apply => |a| {
                try self.pushExprs(self.ast.store.exprSlice(a.args));
                try self.pushCallee(a.@"fn");
            },
            .record => |r| {
                try self.pushRecordFields(r.fields);
                if (r.ext) |ext| try self.push(.{ .expr = ext });
            },
            .record_builder => |r| {
                try self.pushRecordFields(r.fields);
                try self.push(.{ .expr = r.mapper });
            },
            .nominal_record => |n| {
                try self.push(.{ .expr = n.backing });
                try self.push(.{ .expr = n.mapper });
            },
            .nominal_apply => |n| {
                try self.pushExprs(self.ast.store.exprSlice(n.args));
                try self.push(.{ .expr = n.mapper });
            },
            .field_access => |f| {
                try self.push(.{ .field_segments = f.segments });
                try self.push(.{ .expr = f.receiver });
            },
            .method_call => |m| {
                try self.pushExprs(self.ast.store.exprSlice(m.args));
                try self.push(.{ .set = .{ .token = m.method_token, .class = .function } });
                try self.push(.{ .expr = m.receiver });
            },
            .tuple_access => |t| try self.push(.{ .expr = t.expr }),
            .arrow_call => |a| {
                try self.pushCallee(a.right);
                try self.push(.{ .expr = a.left });
            },
            .bin_op => |b| {
                try self.push(.{ .expr = b.right });
                try self.push(.{ .expr = b.left });
            },
            .suffix_single_question => |u| try self.push(.{ .expr = u.expr }),
            .unary_op => |u| try self.push(.{ .expr = u.expr }),
            .if_then_else => |i| {
                try self.push(.{ .expr = i.@"else" });
                try self.push(.{ .expr = i.then });
                try self.push(.{ .expr = i.condition });
            },
            .if_without_else => |i| {
                try self.push(.{ .expr = i.then });
                try self.push(.{ .expr = i.condition });
            },
            .match => |m| {
                const branches = self.ast.store.matchBranchSlice(m.branches);
                var i = branches.len;
                while (i > 0) {
                    i -= 1;
                    try self.push(.{ .match_branch = branches[i] });
                }
                try self.push(.{ .expr = m.expr });
            },
            .block => |b| try self.push(.{ .scoped_statements = b.statements }),
            .for_expr => |f| {
                try self.push(.{ .for_body = .{ .patt = f.patt, .body = f.body } });
                try self.push(.{ .expr = f.expr });
            },
            .dbg => |e| try self.push(.{ .expr = e.expr }),
            .crash => |e| try self.push(.{ .expr = e.expr }),
            .@"return" => |e| try self.push(.{ .expr = e.expr }),
            .list => |l| try self.pushExprs(self.ast.store.exprSlice(l.items)),
            .tuple => |t| try self.pushExprs(self.ast.store.exprSlice(t.items)),
            .string => |s| try self.pushExprs(self.ast.store.exprSlice(s.parts)),
            .multiline_string => |s| try self.pushExprs(self.ast.store.exprSlice(s.parts)),
            .typed_string => |s| try self.pushExprs(self.ast.store.exprSlice(s.parts)),
            .typed_multiline_string => |s| try self.pushExprs(self.ast.store.exprSlice(s.parts)),
            .int,
            .frac,
            .typed_int,
            .typed_frac,
            .single_quote,
            .string_part,
            .record_updater,
            .ellipsis,
            .@"break",
            .malformed,
            => {},
        }
    }
};

/// Delta-encodes a list of semantic tokens into the LSP format.
/// The LSP format uses 5 integers per token: [deltaLine, deltaStartChar, length, tokenType, tokenModifiers]
/// where deltaLine and deltaStartChar are relative to the previous token.
pub fn deltaEncode(allocator: std.mem.Allocator, tokens: []const SemanticToken) Allocator.Error![]u32 {
    if (tokens.len == 0) {
        return &[_]u32{};
    }

    var result = try allocator.alloc(u32, tokens.len * 5);
    errdefer allocator.free(result);

    var prev_line: u32 = 0;
    var prev_char: u32 = 0;

    for (tokens, 0..) |token, i| {
        const delta_line = token.line - prev_line;
        const delta_char = if (delta_line == 0) token.start_char - prev_char else token.start_char;

        result[i * 5 + 0] = delta_line;
        result[i * 5 + 1] = delta_char;
        result[i * 5 + 2] = token.length;
        result[i * 5 + 3] = token.token_type;
        result[i * 5 + 4] = token.modifiers;

        prev_line = token.line;
        prev_char = token.start_char;
    }

    return result;
}
