//! Semantic token extraction for the Roc LSP.
//!
//! This module provides functionality to extract semantic tokens from Roc source code
//! and encode them in the LSP delta-encoded format for syntax highlighting.
//!
//! Two extraction modes are available:
//! - Token-based: Fast, uses only tokenizer output (fallback)
//! - CIR-based: Richer semantics from canonicalized IR (function/parameter detection)

const std = @import("std");
const tokenize = @import("parse").tokenize;
const parse = @import("parse");
const can = @import("can");
const base = @import("base");
const line_info = @import("line_info.zig");

const Allocators = base.Allocators;
const Token = tokenize.Token;
const Tokenizer = tokenize.Tokenizer;
const CommonEnv = base.CommonEnv;
const LineInfo = line_info.LineInfo;
const CIR = can.CIR;
const ModuleEnv = can.ModuleEnv;
const Region = base.Region;

/// Semantic token indices matching TOKEN_TYPES in capabilities.zig.
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
pub fn tokenTagToSemanticType(tag: Token.Tag) ?u32 {
    return switch (tag) {
        // Keywords
        .KwApp,
        .KwAs,
        .KwCrash,
        .KwDbg,
        .KwElse,
        .KwExpect,
        .KwExposes,
        .KwExposing,
        .KwFor,
        .KwGenerates,
        .KwHas,
        .KwHosted,
        .KwIf,
        .KwImplements,
        .KwImport,
        .KwImports,
        .KwIn,
        .KwInterface,
        .KwMatch,
        .KwModule,
        .KwPackage,
        .KwPackages,
        .KwPlatform,
        .KwProvides,
        .KwRequires,
        .KwReturn,
        .KwTargets,
        .KwVar,
        .KwWhere,
        .KwWhile,
        .KwWith,
        .KwBreak,
        => @intFromEnum(SemanticType.keyword),

        // Type identifiers
        .UpperIdent => @intFromEnum(SemanticType.type),

        // Variable identifiers
        .LowerIdent => @intFromEnum(SemanticType.variable),

        // Property access (record fields)
        .DotLowerIdent,
        .NoSpaceDotLowerIdent,
        => @intFromEnum(SemanticType.property),

        // Tag access (enum members)
        .DotUpperIdent,
        .NoSpaceDotUpperIdent,
        => @intFromEnum(SemanticType.enumMember),

        // Numeric literals
        .Int,
        .Float,
        .DotInt,
        .NoSpaceDotInt,
        .MalformedNumberBadSuffix,
        .MalformedNumberUnicodeSuffix,
        .MalformedNumberNoDigits,
        .MalformedNumberNoExponentDigits,
        => @intFromEnum(SemanticType.number),

        // String literals
        .StringStart,
        .StringEnd,
        .StringPart,
        .MultilineStringStart,
        .SingleQuote,
        .MalformedSingleQuote,
        .MalformedStringPart,
        .MalformedInvalidUnicodeEscapeSequence,
        .MalformedInvalidEscapeSequence,
        => @intFromEnum(SemanticType.string),

        // Operators
        .OpPlus,
        .OpStar,
        .OpPizza,
        .OpAssign,
        .OpBinaryMinus,
        .OpUnaryMinus,
        .OpNotEquals,
        .OpBang,
        .OpAnd,
        .OpAmpersand,
        .OpQuestion,
        .OpDoubleQuestion,
        .OpOr,
        .OpBar,
        .OpDoubleSlash,
        .OpSlash,
        .OpPercent,
        .OpCaret,
        .OpGreaterThanOrEq,
        .OpGreaterThan,
        .OpLessThanOrEq,
        .OpBackArrow,
        .OpLessThan,
        .OpEquals,
        .OpColonEqual,
        .OpDoubleColon,
        .NoSpaceOpQuestion,
        .OpColon,
        .OpArrow,
        .OpFatArrow,
        .OpBackslash,
        .DoubleDot,
        .TripleDot,
        .DotStar,
        => @intFromEnum(SemanticType.operator),

        // Named underscore and opaque names
        .NamedUnderscore,
        .MalformedNamedUnderscoreUnicode,
        => @intFromEnum(SemanticType.variable),

        .OpaqueName,
        .MalformedOpaqueNameUnicode,
        .MalformedOpaqueNameWithoutName,
        => @intFromEnum(SemanticType.type),

        // Unicode identifier variants
        .MalformedUnicodeIdent,
        .MalformedDotUnicodeIdent,
        .MalformedNoSpaceDotUnicodeIdent,
        => @intFromEnum(SemanticType.variable),

        // Punctuation and structural tokens (not highlighted)
        .EndOfFile,
        .OpenRound,
        .CloseRound,
        .OpenSquare,
        .CloseSquare,
        .OpenCurly,
        .CloseCurly,
        .OpenStringInterpolation,
        .CloseStringInterpolation,
        .NoSpaceOpenRound,
        .Comma,
        .Dot,
        .Underscore,
        .MalformedUnknownToken,
        => null,
    };
}

/// Extracts semantic tokens from Roc source code.
/// Returns a list of SemanticToken structs with absolute positions.
pub fn extractSemanticTokens(
    allocator: std.mem.Allocator,
    source: []const u8,
    info: *const LineInfo,
) ![]SemanticToken {
    // Create a CommonEnv for tokenization
    const source_copy = try allocator.dupe(u8, source);
    defer allocator.free(source_copy);

    var env = try CommonEnv.init(allocator, source_copy);
    defer env.deinit(allocator);

    // Create diagnostics buffer (we ignore diagnostics for semantic tokens)
    var diagnostics: [64]tokenize.Diagnostic = undefined;

    // Tokenize the source
    var tokenizer = try Tokenizer.init(&env, allocator, source_copy, &diagnostics);
    defer tokenizer.deinit(allocator);
    try tokenizer.tokenize(allocator);

    // Extract token data
    const tags = tokenizer.output.tokens.items(.tag);
    const regions = tokenizer.output.tokens.items(.region);

    // Build semantic tokens list
    var tokens: std.ArrayListUnmanaged(SemanticToken) = .{};
    errdefer tokens.deinit(allocator);

    for (tags, regions) |tag, region| {
        const semantic_type = tokenTagToSemanticType(tag) orelse continue;

        const start_offset = region.start.offset;
        const end_offset = region.end.offset;
        const length = end_offset - start_offset;

        // Skip zero-length tokens
        if (length == 0) continue;

        // Convert byte offset to line/character position
        const pos = info.positionFromOffset(start_offset) orelse continue;

        try tokens.append(allocator, .{
            .line = pos.line,
            .start_char = pos.character,
            .length = length,
            .token_type = semantic_type,
            .modifiers = 0,
        });
    }

    return tokens.toOwnedSlice(allocator);
}

/// Extracts semantic tokens using the Canonicalized IR for richer semantic information.
/// Falls back to token-only extraction on canonicalization errors.
pub fn extractSemanticTokensWithCIR(
    allocator: std.mem.Allocator,
    source: []const u8,
    info: *const LineInfo,
) ![]SemanticToken {
    return extractSemanticTokensWithImports(allocator, source, info, null);
}

/// Extracts semantic tokens with cross-module import context.
/// When imported_envs is provided, can distinguish Module.function from record.field.
pub fn extractSemanticTokensWithImports(
    allocator: std.mem.Allocator,
    source: []const u8,
    info: *const LineInfo,
    imported_envs: ?[]*ModuleEnv,
) ![]SemanticToken {
    // Create ModuleEnv with source
    var allocators: Allocators = undefined;
    allocators.initInPlace(allocator);
    defer allocators.deinit();

    var module_env = ModuleEnv.init(allocator, source) catch {
        // Fall back to token-only extraction on error
        return extractSemanticTokens(allocator, source, info);
    };
    defer module_env.deinit();

    // Parse the source
    const parse_ast = parse.parse(&allocators, &module_env.common) catch {
        // Fall back to token-only extraction on parse error
        return extractSemanticTokens(allocator, source, info);
    };
    defer parse_ast.deinit();

    // Initialize CIR fields
    module_env.initCIRFields("semantic-tokens") catch {
        return extractSemanticTokens(allocator, source, info);
    };

    // Create canonicalizer and run
    var canonicalizer = can.Can.init(&module_env, parse_ast, null) catch {
        return extractSemanticTokens(allocator, source, info);
    };
    defer canonicalizer.deinit();

    canonicalizer.canonicalizeFile() catch {
        // Fall back to token-only on canonicalization error
        return extractSemanticTokens(allocator, source, info);
    };

    // Build import context for cross-module lookups
    var import_context = ImportContext.init(allocator);
    defer import_context.deinit();

    if (imported_envs) |envs| {
        for (envs) |imp_env| {
            import_context.addModuleExports(imp_env) catch {};
        }
    }

    // Create a semantic collector to walk the CIR
    var collector = SemanticCollector{
        .allocator = allocator,
        .tokens = std.ArrayListUnmanaged(SemanticToken){},
        .module_env = &module_env,
        .info = info,
        .source = source,
        .import_context = &import_context,
    };
    errdefer collector.tokens.deinit(allocator);

    // Walk CIR statements for semantic information
    collector.walkStatements() catch {
        // Fall back on error
        collector.tokens.deinit(allocator);
        return extractSemanticTokens(allocator, source, info);
    };

    // Also extract tokens from the tokenizer that weren't covered by CIR
    // (keywords, operators, and identifiers as fallback)
    try collector.addTokensFromTokenizer(parse_ast);

    // Sort tokens by position (line, then character)
    std.mem.sort(SemanticToken, collector.tokens.items, {}, SemanticToken.lessThan);

    return collector.tokens.toOwnedSlice(allocator);
}

/// Context for cross-module import lookups.
/// Maps module names to their exported symbols.
const ImportContext = struct {
    allocator: std.mem.Allocator,
    /// Maps module name to a set of exported function names
    module_functions: std.StringHashMap(std.StringHashMap(void)),

    fn init(allocator: std.mem.Allocator) ImportContext {
        return .{
            .allocator = allocator,
            .module_functions = std.StringHashMap(std.StringHashMap(void)).init(allocator),
        };
    }

    fn deinit(self: *ImportContext) void {
        var it = self.module_functions.iterator();
        while (it.next()) |entry| {
            entry.value_ptr.deinit();
        }
        self.module_functions.deinit();
    }

    /// Add exports from a module to the context.
    fn addModuleExports(self: *ImportContext, module_env: *ModuleEnv) !void {
        const module_name = module_env.module_name;
        if (module_name.len == 0) return;

        // Get or create the function set for this module
        const gop = try self.module_functions.getOrPut(module_name);
        if (!gop.found_existing) {
            gop.value_ptr.* = std.StringHashMap(void).init(self.allocator);
        }

        // Add all exported definitions that are functions
        const exports = module_env.store.sliceDefs(module_env.exports);
        for (exports) |def_idx| {
            const def = module_env.store.getDef(def_idx);
            // Check if this definition is a function by looking at its expression
            const expr = module_env.store.getExpr(def.expr);
            const is_function = switch (expr) {
                .e_lambda, .e_closure => true,
                else => false,
            };
            if (is_function) {
                // Get the name from the pattern
                const pattern = module_env.store.getPattern(def.pattern);
                if (pattern == .assign) {
                    const ident_idx = pattern.assign.ident;
                    const name = module_env.common.idents.getText(ident_idx);
                    try gop.value_ptr.put(name, {});
                }
            }
        }
    }

    /// Check if a symbol is an exported function from a given module.
    fn isModuleFunction(self: *const ImportContext, module_name: []const u8, symbol_name: []const u8) bool {
        const functions = self.module_functions.get(module_name) orelse return false;
        return functions.contains(symbol_name);
    }
};

/// Collector for walking CIR and extracting semantic tokens.
const SemanticCollector = struct {
    allocator: std.mem.Allocator,
    tokens: std.ArrayListUnmanaged(SemanticToken),
    module_env: *const ModuleEnv,
    info: *const LineInfo,
    source: []const u8,
    import_context: *const ImportContext,

    /// Walk all top-level statements in the module.
    fn walkStatements(self: *SemanticCollector) !void {
        const statements_slice = self.module_env.store.sliceStatements(self.module_env.all_statements);
        for (statements_slice) |stmt_idx| {
            try self.visitStatement(stmt_idx);
        }
    }

    /// Visit a single statement and extract semantic tokens.
    fn visitStatement(self: *SemanticCollector, stmt_idx: CIR.Statement.Idx) !void {
        const stmt = self.module_env.store.getStatement(stmt_idx);
        switch (stmt) {
            .s_decl => |d| try self.visitDecl(d.pattern, d.expr),
            .s_var => |v| try self.visitDecl(v.pattern_idx, v.expr),
            .s_expr => |e| try self.visitExpr(e.expr),
            // Type declarations and imports don't need special handling
            // since they're covered by the tokenizer
            else => {},
        }
    }

    /// Visit a declaration (s_decl, s_var).
    fn visitDecl(self: *SemanticCollector, pattern_idx: CIR.Pattern.Idx, expr_idx: CIR.Expr.Idx) !void {
        // Check if RHS is a lambda/closure (then LHS is a function name)
        const expr = self.module_env.store.getExpr(expr_idx);
        const is_function = switch (expr) {
            .e_closure, .e_lambda, .e_hosted_lambda => true,
            else => false,
        };

        // Add token for pattern with appropriate type
        const pattern_region = self.module_env.store.getPatternRegion(pattern_idx);
        const pattern_type: SemanticType = if (is_function) .function else .variable;
        try self.addToken(pattern_region, pattern_type);

        // If it's a function, visit lambda parameters
        if (is_function) {
            try self.visitLambdaParams(expr_idx);
        }

        // Visit the expression
        try self.visitExpr(expr_idx);
    }

    /// Visit lambda parameters and mark them as parameters.
    fn visitLambdaParams(self: *SemanticCollector, expr_idx: CIR.Expr.Idx) !void {
        const expr = self.module_env.store.getExpr(expr_idx);
        switch (expr) {
            .e_closure => |c| {
                // Closure wraps a lambda - get the inner lambda's args
                const lambda = self.module_env.store.getExpr(c.lambda_idx);
                switch (lambda) {
                    .e_lambda => |l| {
                        var i: u32 = 0;
                        while (i < l.args.span.len) : (i += 1) {
                            const param_idx: CIR.Pattern.Idx = @enumFromInt(l.args.span.start + i);
                            try self.visitPatternAsParameter(param_idx);
                        }
                    },
                    else => {},
                }
            },
            .e_lambda => |l| {
                // Pure lambda - visit each parameter pattern
                var i: u32 = 0;
                while (i < l.args.span.len) : (i += 1) {
                    const param_idx: CIR.Pattern.Idx = @enumFromInt(l.args.span.start + i);
                    try self.visitPatternAsParameter(param_idx);
                }
            },
            .e_hosted_lambda => |h| {
                // Hosted lambda has args directly
                var i: u32 = 0;
                while (i < h.args.span.len) : (i += 1) {
                    const param_idx: CIR.Pattern.Idx = @enumFromInt(h.args.span.start + i);
                    try self.visitPatternAsParameter(param_idx);
                }
            },
            else => {},
        }
    }

    /// Visit a pattern and mark it as a parameter.
    fn visitPatternAsParameter(self: *SemanticCollector, pattern_idx: CIR.Pattern.Idx) !void {
        const pattern = self.module_env.store.getPattern(pattern_idx);
        switch (pattern) {
            .assign => {
                // Simple identifier pattern
                const region = self.module_env.store.getPatternRegion(pattern_idx);
                try self.addToken(region, .parameter);
            },
            .as => |a| {
                // The "as" pattern: pattern as name
                // Visit the inner pattern
                try self.visitPatternAsParameter(a.pattern);
            },
            .record_destructure => |r| {
                // Visit each destructured field's pattern
                var i: u32 = 0;
                while (i < r.destructs.span.len) : (i += 1) {
                    const destruct_idx: CIR.Pattern.RecordDestruct.Idx = @enumFromInt(r.destructs.span.start + i);
                    const destruct = self.module_env.store.getRecordDestruct(destruct_idx);
                    // Get the pattern from the kind union
                    const field_pattern = destruct.kind.toPatternIdx();
                    try self.visitPatternAsParameter(field_pattern);
                }
            },
            .tuple => |t| {
                var i: u32 = 0;
                while (i < t.patterns.span.len) : (i += 1) {
                    const elem_idx: CIR.Pattern.Idx = @enumFromInt(t.patterns.span.start + i);
                    try self.visitPatternAsParameter(elem_idx);
                }
            },
            .list => |l| {
                var i: u32 = 0;
                while (i < l.patterns.span.len) : (i += 1) {
                    const elem_idx: CIR.Pattern.Idx = @enumFromInt(l.patterns.span.start + i);
                    try self.visitPatternAsParameter(elem_idx);
                }
            },
            .underscore => {
                // Underscores are still highlighted as parameters
                const region = self.module_env.store.getPatternRegion(pattern_idx);
                try self.addToken(region, .parameter);
            },
            else => {},
        }
    }

    /// Visit an expression and extract any relevant tokens.
    fn visitExpr(self: *SemanticCollector, expr_idx: CIR.Expr.Idx) !void {
        const expr = self.module_env.store.getExpr(expr_idx);
        switch (expr) {
            .e_tag => {
                // Tags are enum members
                const region = self.module_env.store.getExprRegion(expr_idx);
                try self.addToken(region, .enumMember);
            },
            .e_closure => |c| {
                // Closure wraps a lambda - visit the inner lambda
                try self.visitExpr(c.lambda_idx);
            },
            .e_lambda => |l| {
                // Visit lambda body
                try self.visitExpr(l.body);
            },
            else => {},
        }
    }

    /// Add tokens from the tokenizer that weren't covered by CIR.
    /// This includes keywords, operators, literals, and identifiers as fallback.
    /// Uses import context to distinguish Module.function from record.field.
    fn addTokensFromTokenizer(self: *SemanticCollector, ast: *const parse.AST) !void {
        const tags = ast.tokens.tokens.items(.tag);
        const regions = ast.tokens.tokens.items(.region);

        // Track previous token for Module.function detection
        var prev_tag: ?Token.Tag = null;
        var prev_region: ?base.Region = null;

        for (tags, regions) |tag, region| {
            defer {
                prev_tag = tag;
                prev_region = region;
            }

            var semantic_type = tokenTagToSemanticType(tag) orelse continue;

            const start_offset = region.start.offset;
            const end_offset = region.end.offset;
            const length = end_offset - start_offset;

            if (length == 0) continue;

            // Check for Module.function pattern:
            // Previous token was UpperIdent (module name) and current is DotLowerIdent
            if ((tag == .DotLowerIdent or tag == .NoSpaceDotLowerIdent) and
                prev_tag != null and prev_tag.? == .UpperIdent)
            {
                if (prev_region) |prev_reg| {
                    // Get the module name from the previous UpperIdent token
                    const module_start = prev_reg.start.offset;
                    const module_end = prev_reg.end.offset;
                    if (module_start < self.source.len and module_end <= self.source.len) {
                        const module_name = self.source[module_start..module_end];

                        // Get the function name from the current token (skip the leading dot)
                        const func_start = start_offset + 1; // Skip the '.'
                        if (func_start < self.source.len and end_offset <= self.source.len) {
                            const func_name = self.source[func_start..end_offset];

                            // Check if this is an exported function from the module
                            if (self.import_context.isModuleFunction(module_name, func_name)) {
                                semantic_type = @intFromEnum(SemanticType.function);
                            }
                        }
                    }
                }
            }

            const pos = self.info.positionFromOffset(start_offset) orelse continue;

            // Check if we already have a token at this position (from CIR)
            const already_exists = for (self.tokens.items) |existing| {
                if (existing.line == pos.line and existing.start_char == pos.character) {
                    break true;
                }
            } else false;

            if (already_exists) continue;

            try self.tokens.append(self.allocator, .{
                .line = pos.line,
                .start_char = pos.character,
                .length = length,
                .token_type = semantic_type,
                .modifiers = 0,
            });
        }
    }

    /// Add a token at the given region with the given semantic type.
    fn addToken(self: *SemanticCollector, region: Region, semantic_type: SemanticType) !void {
        const start_offset = region.start.offset;
        const end_offset = region.end.offset;
        const length = end_offset - start_offset;

        if (length == 0) return;

        const pos = self.info.positionFromOffset(start_offset) orelse return;

        try self.tokens.append(self.allocator, .{
            .line = pos.line,
            .start_char = pos.character,
            .length = length,
            .token_type = @intFromEnum(semantic_type),
            .modifiers = 0,
        });
    }
};

/// Delta-encodes a list of semantic tokens into the LSP format.
/// The LSP format uses 5 integers per token: [deltaLine, deltaStartChar, length, tokenType, tokenModifiers]
/// where deltaLine and deltaStartChar are relative to the previous token.
pub fn deltaEncode(allocator: std.mem.Allocator, tokens: []const SemanticToken) ![]u32 {
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
