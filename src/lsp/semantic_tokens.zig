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
const cir_queries = @import("cir_queries.zig");
const line_info = @import("line_info.zig");

const Token = tokenize.Token;
const LineInfo = line_info.LineInfo;
const ModuleEnv = can.ModuleEnv;
const AST = parse.AST;
const CIR = can.CIR;

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
        .keyword => @intFromEnum(SemanticType.keyword),
        .type => @intFromEnum(SemanticType.type),
        .variable => @intFromEnum(SemanticType.variable),
        .field => @intFromEnum(SemanticType.property),
        .tag => @intFromEnum(SemanticType.enumMember),
        .number => @intFromEnum(SemanticType.number),
        .string => @intFromEnum(SemanticType.string),
        .operator => @intFromEnum(SemanticType.operator),

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
        return @intFromEnum(SemanticType.function);
    }

    return tokenTagToSemanticType(tag);
}

/// Extracts semantic tokens from Roc source code.
/// Returns a list of SemanticToken structs with absolute positions.
pub fn extractSemanticTokens(
    allocator: std.mem.Allocator,
    source: []const u8,
    info: *const LineInfo,
) Allocator.Error![]SemanticToken {
    return extractSemanticTokensWithImports(allocator, source, info, null);
}

/// Extracts semantic tokens with cross-module import context.
/// When imported_envs is provided, a qualified name that an imported module
/// exports as a function is a function even where it is not being called.
pub fn extractSemanticTokensWithImports(
    allocator: std.mem.Allocator,
    source: []const u8,
    info: *const LineInfo,
    imported_envs: ?[]*ModuleEnv,
) Allocator.Error![]SemanticToken {
    var module_env = ModuleEnv.init(allocator, source) catch return error.OutOfMemory;
    defer module_env.deinit();

    // Parse the source. Syntax errors are reported through the AST diagnostics.
    const parse_ast = try parse.file(allocator, &module_env.common);
    defer parse_ast.deinit();

    var import_context = ImportContext.init(allocator);
    defer import_context.deinit();
    if (imported_envs) |envs| {
        for (envs) |imp_env| try import_context.addModuleExports(imp_env);
    }

    const token_count = parse_ast.tokens.tokens.len;
    const classes = try allocator.alloc(Class, token_count);
    defer allocator.free(classes);
    @memset(classes, .default);

    var classifier = Classifier{
        .allocator = allocator,
        .ast = parse_ast,
        .classes = classes,
        .import_context = &import_context,
    };
    defer classifier.bindings.deinit(allocator);
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
    info: *const LineInfo,
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

    const tags = parse_ast.tokens.tokens.items(.tag);
    const regions = parse_ast.tokens.tokens.items(.region);
    for (tags, regions, 0..) |tag, region, token_index| {
        if (!isIdentifierTag(tag)) continue;
        const name_offset = region.start.offset + namePrefixLen(tag);
        if (name_offset >= region.end.offset) continue;

        if (cir_queries.findTagAtOffset(module_env, name_offset) != null) {
            classes[token_index] = .enum_member;
            continue;
        }

        if (cir_queries.findTypeAnnotationRoleAtOffset(module_env, name_offset)) |role| {
            classes[token_index] = switch (role) {
                .type_name => .type,
                .type_parameter => .type_parameter,
                .tag => .enum_member,
            };
            continue;
        }

        if (cir_queries.findLookupAtOffset(module_env, name_offset)) |lookup| {
            classes[token_index] = switch (lookup) {
                .field_access => .property,
                .expr => |expr_idx| classifyCheckedExpr(module_env, expr_idx),
            };
            continue;
        }

        if (cir_queries.findPatternAtOffset(module_env, name_offset)) |pattern_idx| {
            classes[token_index] = classifyCheckedPattern(module_env, pattern_idx);
        }
    }

    var tokens: std.ArrayListUnmanaged(SemanticToken) = .empty;
    errdefer tokens.deinit(allocator);
    try emitTokens(allocator, parse_ast, classes, source, info, &tokens);
    return tokens.toOwnedSlice(allocator);
}

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

fn classifyCheckedPattern(
    module_env: *ModuleEnv,
    pattern_idx: CIR.Pattern.Idx,
) Class {
    if (cir_queries.isParameterPattern(module_env, pattern_idx)) return .parameter;
    if (module_env.types.varResolvesToFunction(ModuleEnv.varFrom(pattern_idx))) return .function;
    return .variable;
}

fn classifyCheckedExpr(
    module_env: *ModuleEnv,
    expr_idx: CIR.Expr.Idx,
) Class {
    const expr = module_env.store.getExpr(expr_idx);
    if (expr == .e_lookup_local) return classifyCheckedPattern(module_env, expr.e_lookup_local.pattern_idx);
    const tag = std.meta.activeTag(expr);
    if (tag == .e_method_call or
        tag == .e_dispatch_call or
        tag == .e_type_method_call or
        tag == .e_type_dispatch_call or
        tag == .e_lookup_associated_local or
        tag == .e_lookup_associated or
        tag == .e_lookup_associated_resolved) return .function;
    return if (module_env.types.varResolvesToFunction(ModuleEnv.varFrom(expr_idx))) .function else .variable;
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
    info: *const LineInfo,
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
            @intFromEnum(semantic)
        else if (tag == .OpColon or tag == .OpBar)
            // A colon or a lambda bar delimits; it does not operate on anything.
            continue
        else if (is_suffix)
            @intFromEnum(SemanticType.number)
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
    info: *const LineInfo,
    start: u32,
    end: u32,
    semantic_type: u32,
    modifiers: u32,
) Allocator.Error!void {
    if (start >= end) return;
    const start_pos = info.positionFromOffset(start) orelse return;
    const end_pos = info.positionFromOffset(end) orelse return;
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
    fn addModuleExports(self: *ImportContext, module_env: *ModuleEnv) std.mem.Allocator.Error!void {
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
            const expr_tag = std.meta.activeTag(expr);
            const is_function = expr_tag == .e_lambda or expr_tag == .e_closure;
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

/// Walks the parse tree, recording what each name token is.
///
/// Names are resolved lexically: a lambda's parameters, the bindings of a match
/// branch or loop, and the declarations of a block are in scope for what they
/// enclose. Declarations are hoisted within their block, as Roc's are at the top
/// level, so a function can be recognised before its definition is reached.
const Classifier = struct {
    allocator: Allocator,
    ast: *const AST,
    classes: []Class,
    import_context: *const ImportContext,
    bindings: std.ArrayListUnmanaged(Binding) = .empty,

    const Binding = struct { name: []const u8, class: Class };

    fn set(self: *Classifier, token: Token.Idx, class: Class) void {
        if (token < self.classes.len) self.classes[token] = class;
    }

    fn setAll(self: *Classifier, span: Token.Span, class: Class) void {
        for (self.ast.store.tokenSlice(span)) |token| self.set(token, class);
    }

    fn bind(self: *Classifier, token: Token.Idx, class: Class) Allocator.Error!void {
        self.set(token, class);
        try self.bindings.append(self.allocator, .{ .name = self.ast.resolve(token), .class = class });
    }

    fn lookup(self: *const Classifier, name: []const u8) ?Class {
        var i = self.bindings.items.len;
        while (i > 0) {
            i -= 1;
            if (std.mem.eql(u8, self.bindings.items[i].name, name)) return self.bindings.items[i].class;
        }
        return null;
    }

    fn isLambda(self: *const Classifier, expr_idx: AST.Expr.Idx) bool {
        return self.ast.store.getExpr(expr_idx) == .lambda;
    }

    fn isFunctionType(self: *const Classifier, anno_idx: AST.TypeAnno.Idx) bool {
        const anno = self.ast.store.getTypeAnno(anno_idx);
        if (anno == .parens) return self.isFunctionType(anno.parens.anno);
        return anno == .@"fn";
    }

    fn walkFile(self: *Classifier) Allocator.Error!void {
        const file = self.ast.store.getFile();
        try self.walkStatements(file.statements);
    }

    fn walkStatements(self: *Classifier, span: AST.Statement.Span) Allocator.Error!void {
        const statements = self.ast.store.statementSlice(span);

        for (statements) |stmt_idx| {
            const statement = self.ast.store.getStatement(stmt_idx);
            if (statement == .decl) {
                const d = statement.decl;
                try self.walkPattern(d.pattern, if (self.isLambda(d.body)) .function else .variable);
            } else if (statement == .type_anno and self.isFunctionType(statement.type_anno.anno)) {
                try self.bind(statement.type_anno.name, .function);
            }
        }

        for (statements) |stmt_idx| try self.walkStatement(stmt_idx);
    }

    fn walkStatement(self: *Classifier, stmt_idx: AST.Statement.Idx) Allocator.Error!void {
        switch (self.ast.store.getStatement(stmt_idx)) {
            .decl => |d| try self.walkExpr(d.body),
            .@"var" => |v| {
                if (v.body) |body| try self.walkExpr(body);
                try self.bind(v.name, .variable);
            },
            .expr => |e| try self.walkExpr(e.expr),
            .crash => |e| try self.walkExpr(e.expr),
            .dbg => |e| try self.walkExpr(e.expr),
            .expect => |e| try self.walkExpr(e.body),
            .@"return" => |e| try self.walkExpr(e.expr),
            .@"for" => |f| try self.walkFor(f.patt, f.expr, f.body),
            .@"while" => |w| {
                try self.walkExpr(w.cond);
                try self.walkExpr(w.body);
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
                        .lower_ident => |l| {
                            const module = self.ast.resolve(target.lastToken());
                            const bare = module[@min(module.len, namePrefixLen(self.ast.tokens.tokens.items(.tag)[target.lastToken()]))..];
                            if (self.import_context.isModuleFunction(bare, self.ast.resolve(l.ident))) {
                                try self.bind(l.ident, .function);
                                if (l.as) |alias| try self.bind(alias, .function);
                            }
                        },
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
                if (self.ast.store.getTypeHeader(t.header)) |header| {
                    self.set(header.name, .type_declaration);
                    for (self.ast.store.typeAnnoSlice(header.args)) |arg| try self.walkTypeAnno(arg);
                } else |_| {}
                try self.walkTypeAnno(t.anno);
                if (t.where) |where| try self.walkWhere(where);
                if (t.associated) |associated| {
                    const mark = self.bindings.items.len;
                    defer self.bindings.shrinkRetainingCapacity(mark);
                    try self.walkStatements(associated.statements);
                }
            },
            .type_anno => |a| {
                self.set(a.name, if (self.isFunctionType(a.anno)) .function else .variable);
                try self.walkTypeAnno(a.anno);
                if (a.where) |where| try self.walkWhere(where);
            },
            .@"break", .malformed => {},
        }
    }

    fn walkWhere(self: *Classifier, where: AST.Collection.Idx) Allocator.Error!void {
        const collection = self.ast.store.getCollection(where);
        const clauses = self.ast.store.whereClauseSlice(.{ .span = collection.span });
        for (clauses) |clause_idx| {
            switch (self.ast.store.getWhereClause(clause_idx)) {
                .mod_method => |m| {
                    self.set(m.var_tok, .type_parameter);
                    self.set(m.name_tok, .function);
                    try self.walkTypeAnno(m.anno);
                },
                .mod_alias => |m| {
                    self.set(m.var_tok, .type_parameter);
                    try self.walkTypeAnno(m.alias);
                },
                .malformed => {},
            }
        }
    }

    fn walkFor(self: *Classifier, patt: AST.Pattern.Idx, expr: AST.Expr.Idx, body: AST.Expr.Idx) Allocator.Error!void {
        try self.walkExpr(expr);
        const mark = self.bindings.items.len;
        defer self.bindings.shrinkRetainingCapacity(mark);
        try self.walkPattern(patt, .variable);
        try self.walkExpr(body);
    }

    /// Classify a pattern; the names it binds get `class`.
    fn walkPattern(self: *Classifier, pattern_idx: AST.Pattern.Idx, class: Class) Allocator.Error!void {
        switch (self.ast.store.getPattern(pattern_idx)) {
            .ident => |p| try self.bind(p.ident_tok, class),
            .var_ident => |p| try self.bind(p.ident_tok, class),
            .tag => |p| {
                self.setAll(p.qualifiers, .type);
                self.set(p.tag_tok, if (p.backing_value) .type else .enum_member);
                // A destructured payload holds values, never the function itself.
                const inner: Class = if (class == .function) .variable else class;
                for (self.ast.store.patternSlice(p.args)) |arg| try self.walkPattern(arg, inner);
            },
            .record => |p| {
                for (self.ast.store.patternRecordFieldSlice(p.fields)) |field_idx| {
                    const field = self.ast.store.getPatternRecordField(field_idx);
                    const inner: Class = if (class == .function) .variable else class;
                    if (field.value) |value| {
                        if (field.name) |name| self.set(name, .property);
                        try self.walkPattern(value, inner);
                    } else if (field.name) |name| {
                        try self.bind(name, inner);
                    }
                }
            },
            .list => |p| for (self.ast.store.patternSlice(p.patterns)) |item| try self.walkPattern(item, class),
            .tuple => |p| for (self.ast.store.patternSlice(p.patterns)) |item| try self.walkPattern(item, class),
            .alternatives => |p| for (self.ast.store.patternSlice(p.patterns)) |item| try self.walkPattern(item, class),
            .list_rest => |p| if (p.name) |name| try self.bind(name, class),
            .as => |p| {
                try self.walkPattern(p.pattern, class);
                try self.bind(p.name, class);
            },
            .int, .frac, .typed_int, .typed_frac, .string, .single_quote, .underscore, .malformed => {},
        }
    }

    fn walkTypeAnno(self: *Classifier, anno_idx: AST.TypeAnno.Idx) Allocator.Error!void {
        switch (self.ast.store.getTypeAnno(anno_idx)) {
            .ty => |t| {
                self.setAll(t.qualifiers, .type);
                self.set(t.token, .type);
            },
            .ty_var => |t| self.set(t.tok, .type_parameter),
            .underscore_type_var => |t| self.set(t.tok, .type_parameter),
            .apply => |a| for (self.ast.store.typeAnnoSlice(a.args)) |arg| try self.walkTypeAnno(arg),
            .tag_union => |u| {
                for (self.ast.store.typeAnnoSlice(u.tags)) |tag_idx| {
                    try self.walkTypeAnno(tag_idx);
                    // The head of each member names a tag, not a type.
                    var head = self.ast.store.getTypeAnno(tag_idx);
                    if (head == .apply) {
                        const args = self.ast.store.typeAnnoSlice(head.apply.args);
                        if (args.len == 0) continue;
                        head = self.ast.store.getTypeAnno(args[0]);
                    }
                    if (head == .ty) self.set(head.ty.token, .enum_member);
                }
                switch (u.ext) {
                    .named => |n| try self.walkTypeAnno(n.anno),
                    .closed, .open => {},
                }
            },
            .tuple => |t| for (self.ast.store.typeAnnoSlice(t.annos)) |item| try self.walkTypeAnno(item),
            .record => |r| {
                for (self.ast.store.annoRecordFieldSlice(r.fields)) |field_idx| {
                    const field = self.ast.store.getAnnoRecordField(field_idx) catch continue;
                    self.set(field.name, .property);
                    try self.walkTypeAnno(field.ty);
                    if (field.default_value) |default_value| try self.walkExpr(default_value);
                }
                switch (r.ext) {
                    .named => |n| try self.walkTypeAnno(n.anno),
                    .closed, .open => {},
                }
            },
            .@"fn" => |f| {
                for (self.ast.store.typeAnnoSlice(f.args)) |arg| try self.walkTypeAnno(arg);
                try self.walkTypeAnno(f.ret);
            },
            .parens => |p| try self.walkTypeAnno(p.anno),
            .underscore, .malformed => {},
        }
    }

    fn walkRecordFields(self: *Classifier, fields: AST.RecordField.Span) Allocator.Error!void {
        for (self.ast.store.recordFieldSlice(fields)) |field_idx| {
            const field = self.ast.store.getRecordField(field_idx);
            self.set(field.name, .property);
            if (field.value.asSupplied()) |value| try self.walkExpr(value);
        }
    }

    /// The callee of a call is a function unless it is a parameter being called.
    fn walkCallee(self: *Classifier, expr_idx: AST.Expr.Idx) Allocator.Error!void {
        try self.walkExpr(expr_idx);
        const callee = self.ast.store.getExpr(expr_idx);
        if (callee != .ident) return;
        const token = callee.ident.token;
        if (token < self.classes.len and self.classes[token] != .parameter) self.set(token, .function);
    }

    fn walkExpr(self: *Classifier, expr_idx: AST.Expr.Idx) Allocator.Error!void {
        switch (self.ast.store.getExpr(expr_idx)) {
            .ident => |i| {
                const qualifiers = self.ast.store.tokenSlice(i.qualifiers);
                for (qualifiers) |qualifier| self.set(qualifier, .type);
                const text = self.ast.resolve(i.token);
                // A qualified name arrives as a dotted token; resolve the bare name.
                const name = text[@min(text.len, namePrefixLen(self.ast.tokens.tokens.items(.tag)[i.token]))..];
                if (qualifiers.len == 0) {
                    if (self.lookup(name)) |class| self.set(i.token, class);
                } else {
                    const module = self.ast.resolve(qualifiers[qualifiers.len - 1]);
                    const bare_module = module[@min(module.len, namePrefixLen(self.ast.tokens.tokens.items(.tag)[qualifiers[qualifiers.len - 1]]))..];
                    const is_function = self.import_context.isModuleFunction(bare_module, name);
                    self.set(i.token, if (is_function) .function else .variable);
                }
            },
            .tag => |t| {
                self.setAll(t.qualifiers, .type);
                self.set(t.token, .enum_member);
            },
            .lambda => |l| {
                const mark = self.bindings.items.len;
                defer self.bindings.shrinkRetainingCapacity(mark);
                for (self.ast.store.patternSlice(l.args)) |arg| try self.walkPattern(arg, .parameter);
                try self.walkExpr(l.body);
            },
            .apply => |a| {
                try self.walkCallee(a.@"fn");
                for (self.ast.store.exprSlice(a.args)) |arg| try self.walkExpr(arg);
            },
            .record => |r| {
                if (r.ext) |ext| try self.walkExpr(ext);
                try self.walkRecordFields(r.fields);
            },
            .record_builder => |r| {
                try self.walkExpr(r.mapper);
                try self.walkRecordFields(r.fields);
            },
            .nominal_record => |n| {
                try self.walkExpr(n.mapper);
                try self.walkExpr(n.backing);
            },
            .nominal_apply => |n| {
                try self.walkExpr(n.mapper);
                for (self.ast.store.exprSlice(n.args)) |arg| try self.walkExpr(arg);
            },
            .field_access => |f| {
                try self.walkExpr(f.receiver);
                for (self.ast.store.fieldAccessSegmentSlice(f.segments)) |segment| {
                    self.set(segment.field_token, .property);
                }
            },
            .method_call => |m| {
                try self.walkExpr(m.receiver);
                self.set(m.method_token, .function);
                for (self.ast.store.exprSlice(m.args)) |arg| try self.walkExpr(arg);
            },
            .tuple_access => |t| try self.walkExpr(t.expr),
            .arrow_call => |a| {
                try self.walkExpr(a.left);
                try self.walkCallee(a.right);
            },
            .bin_op => |b| {
                try self.walkExpr(b.left);
                try self.walkExpr(b.right);
            },
            .suffix_single_question => |u| try self.walkExpr(u.expr),
            .unary_op => |u| try self.walkExpr(u.expr),
            .if_then_else => |i| {
                try self.walkExpr(i.condition);
                try self.walkExpr(i.then);
                try self.walkExpr(i.@"else");
            },
            .if_without_else => |i| {
                try self.walkExpr(i.condition);
                try self.walkExpr(i.then);
            },
            .match => |m| {
                try self.walkExpr(m.expr);
                for (self.ast.store.matchBranchSlice(m.branches)) |branch_idx| {
                    const branch = self.ast.store.getBranch(branch_idx);
                    const mark = self.bindings.items.len;
                    defer self.bindings.shrinkRetainingCapacity(mark);
                    try self.walkPattern(branch.pattern, .variable);
                    if (branch.guard) |guard| try self.walkExpr(guard);
                    try self.walkExpr(branch.body);
                }
            },
            .block => |b| {
                const mark = self.bindings.items.len;
                defer self.bindings.shrinkRetainingCapacity(mark);
                try self.walkStatements(b.statements);
            },
            .for_expr => |f| try self.walkFor(f.patt, f.expr, f.body),
            .dbg => |e| try self.walkExpr(e.expr),
            .crash => |e| try self.walkExpr(e.expr),
            .@"return" => |e| try self.walkExpr(e.expr),
            .list => |l| for (self.ast.store.exprSlice(l.items)) |item| try self.walkExpr(item),
            .tuple => |t| for (self.ast.store.exprSlice(t.items)) |item| try self.walkExpr(item),
            .string => |s| for (self.ast.store.exprSlice(s.parts)) |part| try self.walkExpr(part),
            .multiline_string => |s| for (self.ast.store.exprSlice(s.parts)) |part| try self.walkExpr(part),
            .typed_string => |s| for (self.ast.store.exprSlice(s.parts)) |part| try self.walkExpr(part),
            .typed_multiline_string => |s| for (self.ast.store.exprSlice(s.parts)) |part| try self.walkExpr(part),
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
