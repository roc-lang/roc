//! Finds the anonymous `..` tag-union extensions that the formatter drops
//! because they mean exactly what their absence means.
//!
//! The type checker generates an extensionless tag union in an OUTPUT position
//! of an annotation as implicitly open, and generates an anonymous `..` written
//! in that same position the same way (design.md "Polarity"). Such a `..` adds
//! nothing, and the checker warns about it (`redundant_open_tag_union`). This
//! module finds those `..` from the parse AST alone, so `roc fmt` can delete
//! them without changing what any annotation means.
//!
//! The walk mirrors `Check.generateAnnoTypeInPlace`: the root of an annotation
//! is an output, each function establishes input arguments and an output
//! return, and other positions inherit their surroundings; a type application's argument is generated at
//! the application's polarity composed with the positions of the declaration
//! formal it is substituted for; a where-method signature opens only the rows
//! the result-row widening adapter can re-tag. Which annotations qualify at all
//! mirrors `Check.checkDef`'s `generalizes_regardless` together with
//! `Check.collectHostBoundaryAnnotations`.
//!
//! The checker reads resolved names; the parse AST has only spellings. So every
//! application root is checked against every declaration its spelling could
//! resolve to: same-named declarations in the file and compiler builtins.
//! Roots with imports or disagreeing candidates retain `..`; ambiguous nested
//! references also retain it because their declaration binding is unknown.
//! A question with no proven answer keeps
//! the `..`, so a row is never opened that the checker would generate closed or
//! rigid: this module may keep a `..` the checker calls redundant, and never
//! drops one it does not.

const std = @import("std");
const parse = @import("parse");
const base = @import("base");
const CIR = @import("can").CIR;
const Positions = base.annotation_positions.Positions;

const AST = parse.AST;
const Token = parse.tokenize.Token;
const Allocator = std.mem.Allocator;

/// Which statement list a type annotation statement sits in.
pub const StatementScope = enum {
    /// The file's own top-level statements.
    file,
    /// The associated block of a type declaration (`Foo := [...].{ ... }`).
    associated,
    /// A block expression's statements.
    block,
};

const try_type_name = "Try";

const Polarity = enum {
    pos,
    neg,
};

/// `Check.GenTypeAnnoCtx.AnnotationGenCtx.OpeningBehavior`.
const Opening = enum { implicit_open, per_use, as_written };

/// `Check.GenTypeAnnoCtx.AnnotationGenCtx.AdapterReach`.
const Reach = enum { signature, result, try_row, nested };

const Ctx = struct {
    opening: Opening,
    reach: Reach,

    fn withReach(self: Ctx, reach: Reach) Ctx {
        return .{ .opening = self.opening, .reach = reach };
    }

    fn withOpening(self: Ctx, opening: Opening) Ctx {
        return .{ .opening = opening, .reach = self.reach };
    }
};

/// A question the walk could not answer the way the checker does.
/// The declarations one written type name could resolve to.
const Candidates = struct {
    /// Every type declaration in the file with this name, at any depth.
    locals: []const AST.Statement.Idx,
    /// The `Builtin` type of this name.
    builtin: bool,
    /// A declaration in another module (an import, a header-introduced name),
    /// or nothing this file can see.
    external: bool,
};

/// One type annotation's type variables: whether each was seen, and whether any
/// occurrence was generated as written. A where clause's constraints are
/// generated with the opening of the receiver's owning occurrence.
const VarOccurrences = std.StringHashMapUnmanaged(bool);

/// Redundant-`..` analysis for one parsed file.
pub const OpenRows = struct {
    gpa: Allocator,
    builtin_owner: bool = false,
    builtin_syntax: ?*BuiltinSyntax = null,
    /// Borrowed invocation-owned builtin analysis, shared across files.
    shared_builtins: ?*BuiltinFacts = null,
    position_cache: std.AutoHashMapUnmanaged(AST.Statement.Idx, ?[]Positions) = .empty,
    ast: *const AST,
    /// One bit per AST node, set on a tag union whose anonymous `..` is
    /// redundant.
    redundant: std.DynamicBitSetUnmanaged,
    /// Type declarations by name, at any depth of the file.
    type_decls: std.StringHashMapUnmanaged(std.ArrayList(AST.Statement.Idx)),
    /// Type names something other than a type declaration in this file may
    /// introduce: every uppercase name written in an import, every platform
    /// `requires` alias, and every where-alias name.
    external_names: std.StringHashMapUnmanaged(void),
    /// Whether some import exposes a module's items wholesale (`Foo.*`), which
    /// can introduce any name.
    wildcard_import: bool,
    /// Whether an annotation-only definition is certainly not a hosted lambda.
    /// Only a platform package's annotation-only definitions become hosted,
    /// and only an app's own header proves the module is not in one.
    anno_only_is_not_hosted: bool,
    /// Names a platform header `provides` to the host; their annotations keep
    /// their rows as written.
    provided_names: std.StringHashMapUnmanaged(void),
    /// Reused worklist for walking destructuring patterns.
    pattern_worklist: std.ArrayList(AST.Pattern.Idx) = .empty,

    /// Index the file's declarations, imports and header.
    pub fn init(gpa: Allocator, ast: *const AST) Allocator.Error!OpenRows {
        var self = OpenRows{
            .gpa = gpa,
            .ast = ast,
            .redundant = try std.DynamicBitSetUnmanaged.initEmpty(gpa, ast.store.nodeCount()),
            .type_decls = .{},
            .external_names = .{},
            .wildcard_import = false,
            .anno_only_is_not_hosted = false,
            .provided_names = .{},
        };
        errdefer self.deinit();

        const tags = ast.store.nodes.items.items(.tag);
        for (tags, 0..) |tag, node_index| {
            const is_type_decl = tag == .type_decl or tag == .type_decl_nominal or
                tag == .type_decl_opaque or tag == .type_decl_where_alias;
            if (is_type_decl) {
                const stmt_idx: AST.Statement.Idx = @fromBackingInt(@intCast(node_index));
                const decl = ast.store.getStatement(stmt_idx).type_decl;
                const header = ast.store.getTypeHeader(decl.header) catch continue;
                const name = self.tokenName(header.name);
                if (decl.kind == .where_alias) {
                    try self.external_names.put(gpa, name, {});
                    continue;
                }
                const entry = try self.type_decls.getOrPut(gpa, name);
                if (!entry.found_existing) entry.value_ptr.* = .empty;
                try entry.value_ptr.append(gpa, stmt_idx);
            } else if (tag == .import) {
                const stmt_idx: AST.Statement.Idx = @fromBackingInt(@intCast(node_index));
                const import = ast.store.getStatement(stmt_idx).import;
                try self.addUpperNamesIn(import.region);
                for (ast.store.exposedItemSlice(import.exposes)) |item_idx| {
                    switch (ast.store.getExposedItem(item_idx)) {
                        .upper_ident_star => self.wildcard_import = true,
                        .lower_ident, .upper_ident, .malformed => {},
                    }
                }
            }
        }

        const file = ast.store.getFile();
        switch (ast.store.getHeader(file.header)) {
            .app, .default_app => self.anno_only_is_not_hosted = true,
            .platform => |platform| {
                for (ast.store.symbolMapEntrySlice(platform.provides)) |entry_idx| {
                    const entry = ast.store.getSymbolMapEntry(entry_idx);
                    try self.provided_names.put(gpa, self.tokenName(entry.func), {});
                }
                // `requires { [Model : model] for main : ... }` introduces
                // `Model` as a type name.
                for (ast.store.requiresEntrySlice(platform.requires_entries)) |requires_idx| {
                    const requires = ast.store.getRequiresEntry(requires_idx);
                    for (ast.store.forClauseTypeAliasSlice(requires.type_aliases)) |alias_idx| {
                        const alias = ast.store.getForClauseTypeAlias(alias_idx);
                        try self.external_names.put(gpa, self.tokenName(alias.alias_name), {});
                    }
                }
            },
            .module, .package, .hosted, .type_module, .malformed => {},
        }

        return self;
    }

    /// Free everything `init` allocated.
    pub fn deinit(self: *OpenRows) void {
        var positions = self.position_cache.valueIterator();
        while (positions.next()) |value| if (value.*) |items| self.gpa.free(items);
        self.position_cache.deinit(self.gpa);
        if (self.builtin_syntax) |syntax| syntax.destroy(self.gpa);
        var decls_it = self.type_decls.valueIterator();
        while (decls_it.next()) |list| list.deinit(self.gpa);
        self.type_decls.deinit(self.gpa);
        self.external_names.deinit(self.gpa);
        self.provided_names.deinit(self.gpa);
        self.pattern_worklist.deinit(self.gpa);
        self.redundant.deinit(self.gpa);
    }

    /// Whether the anonymous `..` of this tag union is redundant.
    pub fn isRedundant(self: *const OpenRows, anno_idx: AST.TypeAnno.Idx) bool {
        return self.redundant.isSet(@backingInt(anno_idx));
    }

    /// Mark the redundant `..` in every type annotation statement of one
    /// statement list.
    pub fn markStatements(self: *OpenRows, statements: []const AST.Statement.Idx, scope: StatementScope) Allocator.Error!void {
        for (statements, 0..) |stmt_idx, index| {
            const stmt = self.ast.store.getStatement(stmt_idx);
            if (stmt != .type_anno) continue;
            const anno = stmt.type_anno;
            if (anno.is_var) continue;
            const next: ?AST.Statement.Idx = if (index + 1 < statements.len) statements[index + 1] else null;
            if (!try self.annotationGeneralizesRegardless(anno.name, next, scope)) continue;
            try self.markAnnotation(anno.anno, anno.where);
        }
    }

    /// Whether the definition this annotation belongs to generalizes whatever
    /// its annotation writes (`Check.checkDef`'s `generalizes_regardless`) and
    /// is not a host boundary (`Check.collectHostBoundaryAnnotations`). A
    /// value binding does not: on a value, `..` is the opt-in to a quantified
    /// row.
    fn annotationGeneralizesRegardless(self: *OpenRows, name_tok: Token.Idx, next: ?AST.Statement.Idx, scope: StatementScope) Allocator.Error!bool {
        const name = self.tokenName(name_tok);
        // A platform's provided definitions are host-boundary annotations.
        if (self.provided_names.contains(name)) return false;

        if (next) |next_idx| {
            const next_stmt = self.ast.store.getStatement(next_idx);
            if (next_stmt == .decl) {
                const decl = next_stmt.decl;
                const pattern = self.ast.store.getPattern(decl.pattern);
                if (pattern == .ident and std.mem.eql(u8, self.tokenName(pattern.ident.ident_tok), name)) {
                    // A function definition. Every other body, including a
                    // lookup whose canonical form this AST cannot tell, keeps
                    // its `..`.
                    return self.ast.store.getExpr(decl.body) == .lambda;
                }
                // At the top level, Can attaches the annotation to the def a
                // destructured literal splits off for that name, which may be
                // a value, so the `..` stays. Associated and block scopes
                // attach only to a same-named ident; any other declaration
                // leaves the annotation annotation-only.
                if (scope == .file and self.destructuredLiteralShapesMatch(decl.pattern, decl.body) and
                    try self.destructuredLiteralPatternBindsName(decl.pattern, name))
                {
                    return false;
                }
            }
        }

        // An annotation-only definition generalizes like a function, but in a
        // platform package it is a hosted lambda, whose rows are a fixed ABI.
        return switch (scope) {
            .file, .associated => self.anno_only_is_not_hosted,
            // A block's annotation with no definition is not a definition.
            .block => false,
        };
    }

    /// Mirrors `Can.destructuredLiteralShapesMatch`: whether `pattern` is a
    /// record or tuple pattern and `expr` a literal of the same kind with
    /// exactly the pattern's fields, every field pattern a name or a nested
    /// record or tuple pattern.
    fn destructuredLiteralShapesMatch(self: *const OpenRows, pattern_idx: AST.Pattern.Idx, expr_idx: AST.Expr.Idx) bool {
        const store = &self.ast.store;
        switch (store.getPattern(pattern_idx)) {
            .record => |pattern_record| {
                const expr = store.getExpr(expr_idx);
                if (expr != .record) return false;
                if (expr.record.ext != null) return false;
                const pattern_fields = store.patternRecordFieldSlice(pattern_record.fields);
                const expr_fields = store.recordFieldSlice(expr.record.fields);
                if (pattern_fields.len == 0 or pattern_fields.len != expr_fields.len) return false;
                for (pattern_fields, 0..) |pattern_field_idx, pattern_index| {
                    const pattern_field = store.getPatternRecordField(pattern_field_idx);
                    if (pattern_field.rest) return false;
                    const name_tok = pattern_field.name orelse return false;
                    const name = self.tokenName(name_tok);
                    if (pattern_field.value) |sub_pattern| {
                        if (!self.destructuredLiteralFieldPatternIsBinding(sub_pattern)) return false;
                    }
                    for (pattern_fields[0..pattern_index]) |earlier_idx| {
                        const earlier_tok = store.getPatternRecordField(earlier_idx).name orelse return false;
                        if (std.mem.eql(u8, self.tokenName(earlier_tok), name)) return false;
                    }
                    if (!self.literalSuppliesField(expr_fields, name)) return false;
                }
                return true;
            },
            .tuple => |pattern_tuple| {
                const expr = store.getExpr(expr_idx);
                if (expr != .tuple) return false;
                const item_patterns = store.patternSlice(pattern_tuple.patterns);
                if (item_patterns.len == 0 or item_patterns.len != store.exprSlice(expr.tuple.items).len) return false;
                for (item_patterns) |item_pattern| {
                    if (!self.destructuredLiteralFieldPatternIsBinding(item_pattern)) return false;
                }
                return true;
            },
            .ident,
            .var_ident,
            .tag,
            .int,
            .frac,
            .typed_int,
            .typed_frac,
            .string,
            .single_quote,
            .list,
            .list_rest,
            .underscore,
            .alternatives,
            .as,
            .malformed,
            => return false,
        }
    }

    fn destructuredLiteralFieldPatternIsBinding(self: *const OpenRows, pattern_idx: AST.Pattern.Idx) bool {
        return switch (self.ast.store.getPattern(pattern_idx)) {
            .ident, .record, .tuple => true,
            .var_ident,
            .tag,
            .int,
            .frac,
            .typed_int,
            .typed_frac,
            .string,
            .single_quote,
            .list,
            .list_rest,
            .underscore,
            .alternatives,
            .as,
            .malformed,
            => false,
        };
    }

    /// Mirrors `Can.literalFieldSupplyingName`: exactly one field named
    /// `name`, with a supplied value.
    fn literalSuppliesField(self: *const OpenRows, expr_fields: []const AST.RecordField.Idx, name: []const u8) bool {
        var found = false;
        for (expr_fields) |field_idx| {
            const field = self.ast.store.getRecordField(field_idx);
            if (!std.mem.eql(u8, self.tokenName(field.name), name)) continue;
            if (found) return false;
            switch (field.value) {
                .supplied => found = true,
                .punned, .unset => return false,
            }
        }
        return found;
    }

    /// Mirrors `Can.destructuredLiteralPatternBindsName`: whether the pattern
    /// binds `name` as a field name or an identifier sub-pattern, at any
    /// nesting of record and tuple patterns.
    fn destructuredLiteralPatternBindsName(self: *OpenRows, root: AST.Pattern.Idx, name: []const u8) Allocator.Error!bool {
        const store = &self.ast.store;
        const pending = &self.pattern_worklist;
        pending.clearRetainingCapacity();
        try pending.append(self.gpa, root);
        while (pending.pop()) |pattern_idx| {
            switch (store.getPattern(pattern_idx)) {
                .ident => |ident| if (std.mem.eql(u8, self.tokenName(ident.ident_tok), name)) return true,
                .record => |record| for (store.patternRecordFieldSlice(record.fields)) |field_idx| {
                    const field = store.getPatternRecordField(field_idx);
                    if (field.value) |sub_pattern| {
                        try pending.append(self.gpa, sub_pattern);
                    } else if (field.name) |name_tok| {
                        if (std.mem.eql(u8, self.tokenName(name_tok), name)) return true;
                    }
                },
                .tuple => |tuple| for (store.patternSlice(tuple.patterns)) |item| try pending.append(self.gpa, item),
                .var_ident,
                .tag,
                .int,
                .frac,
                .typed_int,
                .typed_frac,
                .string,
                .single_quote,
                .list,
                .list_rest,
                .underscore,
                .alternatives,
                .as,
                .malformed,
                => {},
            }
        }
        return false;
    }

    fn markAnnotation(self: *OpenRows, anno_idx: AST.TypeAnno.Idx, where: ?AST.Collection.Idx) Allocator.Error!void {
        var occurrences: VarOccurrences = .{};
        defer occurrences.deinit(self.gpa);

        const ctx = Ctx{ .opening = .implicit_open, .reach = .nested };
        try self.walk(anno_idx, ctx, .pos, &occurrences);

        const where_coll = where orelse return;
        const coll = self.ast.store.getCollection(where_coll);
        for (self.ast.store.whereClauseSlice(.{ .span = coll.span })) |clause_idx| {
            switch (self.ast.store.getWhereClause(clause_idx)) {
                .mod_method => |method| {
                    if (!receiverOpens(&occurrences, self.tokenName(method.var_tok))) continue;
                    // `Check.completeOwnedStaticDispatchConstraint`.
                    try self.walk(method.anno, .{ .opening = .per_use, .reach = .signature }, .pos, null);
                },
                .mod_alias => |alias| {
                    if (!receiverOpens(&occurrences, self.tokenName(alias.var_tok))) continue;
                    // `Check.generateWhereAliasReferenceArgs`.
                    switch (self.ast.store.getTypeAnno(alias.alias)) {
                        .apply => |apply| for (self.ast.store.typeAnnoSlice(apply.args)[1..]) |arg| {
                            try self.walk(arg, ctx, .neg, null);
                        },
                        .ty_var, .underscore_type_var, .underscore, .ty, .tag_union, .tuple, .record, .@"fn", .parens, .malformed => {},
                    }
                },
                .malformed => {},
            }
        }
    }

    /// Whether a where clause on this receiver is generated with the
    /// annotation's own opening: the receiver is introduced by the annotation,
    /// and no occurrence of it that could own the clause is generated as
    /// written.
    fn receiverOpens(occurrences: *const VarOccurrences, receiver: []const u8) bool {
        const as_written = occurrences.get(receiver) orelse return false;
        return !as_written;
    }

    /// `Check.generateAnnoTypeInPlace`, deciding only where an anonymous `..`
    /// is generated exactly as its absence would be.
    /// One annotation position still to visit.
    const WalkItem = struct {
        anno: AST.TypeAnno.Idx,
        ctx: Ctx,
        polarity: Polarity,
    };

    /// Visit an annotation and every position nested in it. Pending positions
    /// wait on a heap-backed stack, so annotation nesting never becomes native
    /// call depth; visiting only sets marks and joins occurrence flags, so the
    /// order positions are visited in does not matter.
    fn walk(self: *OpenRows, anno_idx: AST.TypeAnno.Idx, ctx: Ctx, polarity: Polarity, occurrences: ?*VarOccurrences) Allocator.Error!void {
        var pending: std.ArrayList(WalkItem) = .empty;
        defer pending.deinit(self.gpa);
        try pending.append(self.gpa, .{ .anno = anno_idx, .ctx = ctx, .polarity = polarity });
        while (pending.pop()) |item| try self.visit(item, occurrences, &pending);
    }

    fn visit(self: *OpenRows, item: WalkItem, occurrences: ?*VarOccurrences, pending: *std.ArrayList(WalkItem)) Allocator.Error!void {
        const anno_idx = item.anno;
        const ctx = item.ctx;
        const polarity = item.polarity;
        switch (self.ast.store.getTypeAnno(anno_idx)) {
            .ty_var => |v| try noteOccurrence(self.gpa, occurrences, self.tokenName(v.tok), ctx),
            .underscore_type_var => |v| try noteOccurrence(self.gpa, occurrences, self.tokenName(v.tok), ctx),
            .underscore, .ty, .malformed => {},
            .parens => |parens| try pending.append(self.gpa, .{ .anno = parens.anno, .ctx = ctx, .polarity = polarity }),
            .@"fn" => |func| {
                for (self.ast.store.typeAnnoSlice(func.args)) |arg| {
                    try pending.append(self.gpa, .{ .anno = arg, .ctx = ctx.withReach(.nested), .polarity = .neg });
                }
                const ret_reach = base.annotation_positions.functionReturnReach(ctx.reach);
                try pending.append(self.gpa, .{ .anno = func.ret, .ctx = ctx.withReach(ret_reach), .polarity = .pos });
            },
            .tag_union => |tag_union| {
                const tags = self.ast.store.typeAnnoSlice(tag_union.tags);
                for (tags) |tag_idx| {
                    switch (self.ast.store.getTypeAnno(tag_idx)) {
                        .apply => |tag| for (self.ast.store.typeAnnoSlice(tag.args)[1..]) |payload| {
                            try pending.append(self.gpa, .{ .anno = payload, .ctx = ctx.withReach(.nested), .polarity = polarity });
                        },
                        .ty_var, .underscore_type_var, .underscore, .ty, .tag_union, .tuple, .record, .@"fn", .parens, .malformed => {},
                    }
                }
                switch (tag_union.ext) {
                    .open => if (tags.len > 0 and polarity == .pos and outputOpens(ctx)) {
                        self.redundant.set(@backingInt(anno_idx));
                    },
                    .named => |named| try pending.append(self.gpa, .{ .anno = named.anno, .ctx = ctx.withReach(.nested), .polarity = polarity }),
                    .closed => {},
                }
            },
            .tuple => |tuple| for (self.ast.store.typeAnnoSlice(tuple.annos)) |elem| {
                try pending.append(self.gpa, .{ .anno = elem, .ctx = ctx.withReach(.nested), .polarity = polarity });
            },
            .record => |record| {
                for (self.ast.store.annoRecordFieldSlice(record.fields)) |field_idx| {
                    const field = self.ast.store.getAnnoRecordField(field_idx) catch continue;
                    try pending.append(self.gpa, .{ .anno = field.ty, .ctx = ctx.withReach(.nested), .polarity = polarity });
                }
                switch (record.ext) {
                    .named => |named| try pending.append(self.gpa, .{ .anno = named.anno, .ctx = ctx.withReach(.nested), .polarity = polarity }),
                    .open, .closed => {},
                }
            },
            .apply => |apply| {
                const all_args = self.ast.store.typeAnnoSlice(apply.args);
                const head = all_args[0];
                const args = all_args[1..];

                const positions = try self.applyPositions(head, args.len);
                defer if (positions) |items| self.gpa.free(items);

                const reaches = if (ctx.opening == .per_use and ctx.reach != .nested) try self.applyReaches(head, args.len, ctx.reach) else null;
                defer if (reaches) |items| self.gpa.free(items);

                for (args, 0..) |arg, arg_index| {
                    const reach: Reach = if (reaches) |known| known[arg_index] else .nested;
                    const arg_ctx = if (positions != null) ctx.withReach(reach) else ctx.withReach(reach).withOpening(.as_written);
                    const arg_polarity = if (positions) |known| known[arg_index].polarity(polarity) else polarity;
                    try pending.append(self.gpa, .{ .anno = arg, .ctx = arg_ctx, .polarity = arg_polarity });
                }
            },
        }
    }

    /// Whether an output-position union with tags is opened in this context,
    /// with an anonymous `..` generated as its absence is.
    fn outputOpens(ctx: Ctx) bool {
        return switch (ctx.opening) {
            .implicit_open => true,
            .per_use => switch (ctx.reach) {
                .signature, .result, .try_row => true,
                .nested => false,
            },
            .as_written => false,
        };
    }

    fn noteOccurrence(gpa: Allocator, occurrences: ?*VarOccurrences, name: []const u8, ctx: Ctx) Allocator.Error!void {
        const map = occurrences orelse return;
        const entry = try map.getOrPut(gpa, name);
        const as_written = ctx.opening == .as_written;
        if (entry.found_existing) {
            entry.value_ptr.* = entry.value_ptr.* or as_written;
        } else {
            entry.value_ptr.* = as_written;
        }
    }

    /// The declarations a written type name could resolve to.
    fn candidates(self: *const OpenRows, head: AST.TypeAnno.Idx) Candidates {
        const ty = switch (self.ast.store.getTypeAnno(head)) {
            .ty => |ty| ty,
            .apply, .ty_var, .underscore_type_var, .underscore, .tag_union, .tuple, .record, .@"fn", .parens, .malformed => return .{ .locals = &.{}, .builtin = false, .external = true },
        };
        // A qualified name may name another module's type.
        if (ty.qualifiers.span.len != 0) return .{ .locals = &.{}, .builtin = false, .external = true };

        const name = self.tokenName(ty.token);
        const locals: []const AST.Statement.Idx = if (self.type_decls.get(name)) |list| list.items else &.{};
        var builtin = false;
        // Use the same exposure inventory as canonicalization. Arity and
        // positions still come from the compiler-owned declaration source.
        for (CIR.builtin_type_specs) |spec| {
            if (!self.builtin_owner and spec.auto_import and std.mem.eql(u8, spec.display_name, name)) builtin = true;
        }
        const imported = self.wildcard_import or self.external_names.contains(name);
        return .{
            .locals = locals,
            .builtin = builtin,
            // A name nothing visible declares resolves somewhere this file
            // cannot read, or nowhere.
            .external = imported or (locals.len == 0 and !builtin),
        };
    }

    fn builtinRows(self: *OpenRows) Allocator.Error!*OpenRows {
        if (self.builtin_owner) return self;
        if (self.builtin_syntax) |syntax| return &syntax.rows;
        if (self.shared_builtins) |facts| return facts.rows();
        const syntax = try BuiltinSyntax.create(self.gpa);
        self.builtin_syntax = syntax;
        return &syntax.rows;
    }

    fn builtinDeclaration(self: *const OpenRows, name: []const u8) AST.Statement.Idx {
        std.debug.assert(self.builtin_owner);
        const qualified = for (CIR.builtin_type_specs) |spec| {
            if (spec.auto_import and std.mem.eql(u8, spec.display_name, name)) break spec.qualified_name;
        } else unreachable;
        var path = std.mem.splitScalar(u8, qualified, '.');
        var statements = self.ast.store.statementSlice(self.ast.store.getFile().statements);
        while (path.next()) |part| {
            const statement = for (statements) |index| {
                const node = self.ast.store.getStatement(index);
                if (node != .type_decl) continue;
                const header = self.ast.store.getTypeHeader(node.type_decl.header) catch unreachable;
                if (std.mem.eql(u8, self.tokenName(header.name), part)) break index;
            } else unreachable;
            if (path.peek() == null) return statement;
            const associated = self.ast.store.getStatement(statement).type_decl.associated orelse unreachable;
            statements = self.ast.store.statementSlice(associated.statements);
        }
        unreachable;
    }

    fn declarationPositions(self: *OpenRows, statement: AST.Statement.Idx) Allocator.Error!?[]const Positions {
        if (self.position_cache.get(statement)) |cached| return cached;
        var analysis = PositionAnalysis.init(self.gpa, .{ .allocator = self.gpa });
        defer analysis.adapter.children.deinit(self.gpa);
        defer analysis.deinit();
        const result = try analysis.analyze(.{ .owner = self, .statement = statement });
        errdefer if (result) |items| self.gpa.free(items);
        try self.position_cache.put(self.gpa, statement, result);
        return result;
    }

    /// Drop syntax only when every possible declaration has the same transfer.
    fn applyPositions(self: *OpenRows, head: AST.TypeAnno.Idx, arity: usize) Allocator.Error!?[]Positions {
        const found = self.candidates(head);
        if (found.external) return null;
        var answer: ?[]Positions = null;
        errdefer if (answer) |items| self.gpa.free(items);
        for (found.locals) |statement| {
            const positions = (try self.declarationPositions(statement)) orelse {
                if (answer) |items| self.gpa.free(items);
                return null;
            };
            if (positions.len != arity) {
                if (answer) |items| self.gpa.free(items);
                return null;
            }
            if (answer) |items| {
                for (items, positions) |left, right| if (@as(u8, @bitCast(left)) != @as(u8, @bitCast(right))) {
                    self.gpa.free(items);
                    return null;
                };
            } else answer = try self.gpa.dupe(Positions, positions);
        }
        if (found.builtin) {
            const builtin_rows = try self.builtinRows();
            const name = self.tokenName(self.ast.store.getTypeAnno(head).ty.token);
            const statement = builtin_rows.builtinDeclaration(name);
            const positions = (try builtin_rows.declarationPositions(statement)) orelse {
                if (answer) |items| self.gpa.free(items);
                return null;
            };
            if (positions.len != arity) {
                if (answer) |items| self.gpa.free(items);
                return null;
            }
            if (answer) |items| {
                for (items, positions) |left, right| if (@as(u8, @bitCast(left)) != @as(u8, @bitCast(right))) {
                    self.gpa.free(items);
                    return null;
                };
            } else answer = try self.gpa.dupe(Positions, positions);
        }
        return answer;
    }

    /// The index of the declaration formal named `name`.
    fn formalIndex(self: *const OpenRows, name: []const u8, formals: []const AST.TypeAnno.Idx) ?usize {
        for (formals, 0..) |formal_idx, index| {
            const formal_name = self.varName(formal_idx) orelse continue;
            if (std.mem.eql(u8, formal_name, name)) return index;
        }
        return null;
    }

    /// The type-variable name `anno_idx` is, looking through parentheses.
    fn varName(self: *const OpenRows, anno_idx: AST.TypeAnno.Idx) ?[]const u8 {
        return switch (self.ast.store.getTypeAnno(self.skipParens(anno_idx))) {
            .ty_var => |v| self.tokenName(v.tok),
            .underscore_type_var => |v| self.tokenName(v.tok),
            .apply, .underscore, .ty, .tag_union, .tuple, .record, .@"fn", .parens, .malformed => null,
        };
    }

    fn skipParens(self: *const OpenRows, anno_idx: AST.TypeAnno.Idx) AST.TypeAnno.Idx {
        var current = anno_idx;
        while (true) {
            switch (self.ast.store.getTypeAnno(current)) {
                .parens => |parens| current = parens.anno,
                .apply, .ty_var, .underscore_type_var, .underscore, .ty, .tag_union, .tuple, .record, .@"fn", .malformed => return current,
            }
        }
    }

    fn applyReaches(self: *OpenRows, head: AST.TypeAnno.Idx, arity: usize, reach: Reach) Allocator.Error!?[]Reach {
        const found = self.candidates(head);
        if (found.external) return null;
        var answer: ?[]Reach = null;
        errdefer if (answer) |items| self.gpa.free(items);
        for (found.locals) |statement| {
            const candidate = (try ReachAnalysis.analyze(self.gpa, .{ .owner = self, .statement = statement }, reach)) orelse {
                if (answer) |items| self.gpa.free(items);
                return null;
            };
            defer self.gpa.free(candidate);
            if (candidate.len != arity or (answer != null and !std.mem.eql(Reach, answer.?, candidate))) {
                if (answer) |items| self.gpa.free(items);
                return null;
            }
            if (answer == null) answer = try self.gpa.dupe(Reach, candidate);
        }
        if (found.builtin) {
            const owner = try self.builtinRows();
            const name = self.tokenName(self.ast.store.getTypeAnno(head).ty.token);
            const declaration = owner.builtinDeclaration(name);
            const candidate = (try ReachAnalysis.analyze(self.gpa, .{ .owner = owner, .statement = declaration }, reach)) orelse {
                if (answer) |items| self.gpa.free(items);
                return null;
            };
            defer self.gpa.free(candidate);
            if (candidate.len != arity or (answer != null and !std.mem.eql(Reach, answer.?, candidate))) {
                if (answer) |items| self.gpa.free(items);
                return null;
            }
            if (answer == null) answer = try self.gpa.dupe(Reach, candidate);
        }
        return answer;
    }

    /// Every uppercase name written in an import's token range.
    fn addUpperNamesIn(self: *OpenRows, region: AST.TokenizedRegion) Allocator.Error!void {
        const tags = self.ast.tokens.tokens.items(.tag);
        var tok: Token.Idx = region.start;
        while (tok < region.end and tok < tags.len) : (tok += 1) {
            const tag = tags[tok];
            if (tag == .UpperIdent or tag == .DotUpperIdent or tag == .NoSpaceDotUpperIdent) {
                try self.external_names.put(self.gpa, self.tokenName(tok), {});
            }
        }
    }

    /// A name token's text, without the leading `.` a qualified segment's
    /// token carries.
    fn tokenName(self: *const OpenRows, tok: Token.Idx) []const u8 {
        const text = self.ast.resolve(tok);
        return if (text.len > 0 and text[0] == '.') text[1..] else text;
    }
};

/// Invocation-owned lazy builtin syntax and derived declaration facts. A caller
/// may share this across sequential file formatting operations, then deinit it.
/// Each concurrent formatting worker must own its own instance.
pub const BuiltinFacts = struct {
    allocator: Allocator,
    syntax: ?*BuiltinSyntax = null,

    /// Release the builtin syntax and cached declaration positions.
    pub fn deinit(self: *BuiltinFacts) void {
        if (self.syntax) |syntax| syntax.destroy(self.allocator);
        self.syntax = null;
    }

    fn rows(self: *BuiltinFacts) Allocator.Error!*OpenRows {
        if (self.syntax == null) self.syntax = try BuiltinSyntax.create(self.allocator);
        return &self.syntax.?.rows;
    }
};

const BuiltinSyntax = struct {
    env: base.CommonEnv,
    ast: *AST,
    rows: OpenRows,

    fn create(allocator: Allocator) Allocator.Error!*BuiltinSyntax {
        const syntax = try allocator.create(BuiltinSyntax);
        errdefer allocator.destroy(syntax);
        syntax.env = try base.CommonEnv.init(allocator, @import("builtin_source").source);
        errdefer syntax.env.deinit(allocator);
        syntax.ast = try parse.file(allocator, &syntax.env);
        errdefer syntax.ast.deinit();
        syntax.rows = try OpenRows.init(allocator, syntax.ast);
        syntax.rows.builtin_owner = true;
        return syntax;
    }

    fn destroy(self: *BuiltinSyntax, allocator: Allocator) void {
        self.rows.deinit();
        self.ast.deinit();
        self.env.deinit(allocator);
        allocator.destroy(self);
    }
};
const PositionAnalysis = base.annotation_positions.Solver(PositionAdapter);
const PositionAdapter = struct {
    /// AST ownership is explicit so builtin and user node indices never mix.
    pub const Key = struct { owner: *OpenRows, statement: AST.Statement.Idx };
    /// Parse annotation identity within its owner.
    pub const Annotation = AST.TypeAnno.Idx;
    allocator: Allocator,
    children: std.ArrayList(Annotation) = .empty,

    pub fn declaration(_: *PositionAdapter, key: Key) Allocator.Error!?struct { body: Annotation, formal_count: usize, nominal: bool } {
        const decl = key.owner.ast.store.getStatement(key.statement).type_decl;
        const header = key.owner.ast.store.getTypeHeader(decl.header) catch return null;
        return .{ .body = decl.anno, .formal_count = key.owner.ast.store.typeAnnoSlice(header.args).len, .nominal = decl.kind != .alias };
    }

    fn reference(owner: *OpenRows, head: Annotation) Allocator.Error!PositionAnalysis.Reference {
        const found = owner.candidates(head);
        if (found.external) return .invalid;
        if (found.locals.len == 1 and !found.builtin) return .{ .declaration = .{ .owner = owner, .statement = found.locals[0] } };
        if (found.locals.len == 0 and found.builtin) {
            const builtin_rows = try owner.builtinRows();
            const name = owner.tokenName(owner.ast.store.getTypeAnno(head).ty.token);
            return .{ .declaration = .{ .owner = builtin_rows, .statement = builtin_rows.builtinDeclaration(name) } };
        }
        // The parser cannot resolve shadowing within a declaration body.
        return .invalid;
    }

    pub fn node(self: *PositionAdapter, key: Key, annotation: Annotation) Allocator.Error!PositionAnalysis.Node {
        const owner = key.owner;
        const ast = owner.ast;
        const decl = ast.store.getStatement(key.statement).type_decl;
        const header = ast.store.getTypeHeader(decl.header) catch return .invalid;
        const formals = ast.store.typeAnnoSlice(header.args);
        if (owner.varName(annotation)) |name| if (owner.formalIndex(name, formals)) |index| return .{ .formal = index };
        self.children.clearRetainingCapacity();
        switch (ast.store.getTypeAnno(annotation)) {
            .parens => |parens| try self.children.append(self.allocator, parens.anno),
            .@"fn" => |func| return .{ .function = .{ .args = ast.store.typeAnnoSlice(func.args), .ret = func.ret } },
            .tag_union => |union_| {
                for (ast.store.typeAnnoSlice(union_.tags)) |tag| switch (ast.store.getTypeAnno(tag)) {
                    .apply => |application| try self.children.appendSlice(self.allocator, ast.store.typeAnnoSlice(application.args)[1..]),
                    .ty => {},
                    .ty_var, .underscore_type_var, .underscore, .tag_union, .tuple, .record, .@"fn", .parens, .malformed => return .invalid,
                };
                if (union_.ext == .named) try self.children.append(self.allocator, union_.ext.named.anno);
            },
            .tuple => |tuple| return .{ .children = ast.store.typeAnnoSlice(tuple.annos) },
            .record => |record| {
                for (ast.store.annoRecordFieldSlice(record.fields)) |field_index| {
                    const field = ast.store.getAnnoRecordField(field_index) catch return .invalid;
                    try self.children.append(self.allocator, field.ty);
                }
                if (record.ext == .named) try self.children.append(self.allocator, record.ext.named.anno);
            },
            .apply => |application| {
                const args = ast.store.typeAnnoSlice(application.args);
                return .{ .apply = .{ .reference = try reference(owner, args[0]), .args = args[1..] } };
            },
            .ty_var, .underscore_type_var, .underscore, .ty => return .leaf,
            .malformed => return .invalid,
        }
        return .{ .children = self.children.items };
    }
};

/// Exact alias source-formal adapter reach over a finite declaration/context graph.
/// Nominal bodies are not transparent; unresolved parser names supply no proof.
const ReachAnalysis = struct {
    const Key = struct { declaration: PositionAdapter.Key, reach: Reach };
    const State = struct { key: Key, results: []u4 };
    const Item = struct { annotation: AST.TypeAnno.Idx, reaches: u4 };
    allocator: Allocator,
    states: std.ArrayList(State) = .empty,
    indices: std.AutoHashMapUnmanaged(Key, usize) = .empty,
    pending: std.ArrayList(Item) = .empty,
    invalid: bool = false,

    fn bit(reach: Reach) u4 {
        return @as(u4, 1) << @as(u2, @intCast(@backingInt(reach)));
    }

    fn register(self: *ReachAnalysis, key: Key) Allocator.Error!?usize {
        if (self.indices.get(key)) |index| return index;
        const owner = key.declaration.owner;
        const decl = owner.ast.store.getStatement(key.declaration.statement).type_decl;
        const header = owner.ast.store.getTypeHeader(decl.header) catch return null;
        const args = owner.ast.store.typeAnnoSlice(header.args);
        const results = try self.allocator.alloc(u4, args.len);
        errdefer self.allocator.free(results);
        @memset(results, 0);
        if (decl.kind != .alias) {
            const builtin_try = owner.builtin_owner and std.mem.eql(u8, owner.tokenName(header.name), try_type_name);
            for (results, 0..) |*result, index| result.* = bit(base.annotation_positions.nominalArgumentReach(key.reach, builtin_try, index));
        }
        const index = self.states.items.len;
        try self.indices.put(self.allocator, key, index);
        try self.states.append(self.allocator, .{ .key = key, .results = results });
        return index;
    }

    fn push(self: *ReachAnalysis, annotation: AST.TypeAnno.Idx, reaches: u4) Allocator.Error!void {
        if (reaches != 0) try self.pending.append(self.allocator, .{ .annotation = annotation, .reaches = reaches });
    }

    fn pushSlice(self: *ReachAnalysis, annotations: []const AST.TypeAnno.Idx, reaches: u4) Allocator.Error!void {
        for (annotations) |annotation| try self.push(annotation, reaches);
    }

    fn evaluate(self: *ReachAnalysis, index: usize) Allocator.Error!bool {
        const state = self.states.items[index];
        const owner = state.key.declaration.owner;
        const ast = owner.ast;
        const decl = ast.store.getStatement(state.key.declaration.statement).type_decl;
        if (decl.kind != .alias) return false;
        const header = ast.store.getTypeHeader(decl.header) catch {
            self.invalid = true;
            return false;
        };
        const formals = ast.store.typeAnnoSlice(header.args);
        var changed = false;
        self.pending.clearRetainingCapacity();
        try self.push(decl.anno, bit(state.key.reach));
        while (self.pending.pop()) |item| {
            if (owner.varName(item.annotation)) |name| {
                if (owner.formalIndex(name, formals)) |formal| {
                    const previous = state.results[formal];
                    state.results[formal] |= item.reaches;
                    changed = changed or previous != state.results[formal];
                    continue;
                }
            }
            switch (ast.store.getTypeAnno(item.annotation)) {
                .parens => |parens| try self.push(parens.anno, item.reaches),
                .@"fn" => |func| {
                    try self.pushSlice(ast.store.typeAnnoSlice(func.args), bit(.nested));
                    var returns: u4 = 0;
                    inline for (@typeInfo(Reach).@"enum".field_names) |field_name| {
                        const reach: Reach = @fromBackingInt(@intCast(@backingInt(@field(Reach, field_name))));
                        if (item.reaches & bit(reach) != 0) returns |= bit(base.annotation_positions.functionReturnReach(reach));
                    }
                    try self.push(func.ret, returns);
                },
                .tuple => |tuple| try self.pushSlice(ast.store.typeAnnoSlice(tuple.annos), bit(.nested)),
                .record => |record| {
                    for (ast.store.annoRecordFieldSlice(record.fields)) |field_index| {
                        const field = ast.store.getAnnoRecordField(field_index) catch {
                            self.invalid = true;
                            continue;
                        };
                        try self.push(field.ty, bit(.nested));
                    }
                    if (record.ext == .named) try self.push(record.ext.named.anno, bit(.nested));
                },
                .tag_union => |union_| {
                    for (ast.store.typeAnnoSlice(union_.tags)) |tag| switch (ast.store.getTypeAnno(tag)) {
                        .apply => |application| try self.pushSlice(ast.store.typeAnnoSlice(application.args)[1..], bit(.nested)),
                        .ty => {},
                        .ty_var, .underscore_type_var, .underscore, .tag_union, .tuple, .record, .@"fn", .parens, .malformed => self.invalid = true,
                    };
                    if (union_.ext == .named) try self.push(union_.ext.named.anno, item.reaches);
                },
                .apply => |application| {
                    const all_args = ast.store.typeAnnoSlice(application.args);
                    const args = all_args[1..];
                    switch (try PositionAdapter.reference(owner, all_args[0])) {
                        .invalid => self.invalid = true,
                        .builtin => try self.pushSlice(args, bit(.nested)),
                        .declaration => |target| {
                            inline for (@typeInfo(Reach).@"enum".field_names) |field_name| {
                                const reach: Reach = @fromBackingInt(@intCast(@backingInt(@field(Reach, field_name))));
                                if (item.reaches & bit(reach) != 0) {
                                    const target_index = (try self.register(.{ .declaration = target, .reach = reach })) orelse {
                                        self.invalid = true;
                                        return false;
                                    };
                                    const results = self.states.items[target_index].results;
                                    if (results.len != args.len) {
                                        self.invalid = true;
                                        return false;
                                    }
                                    for (args, results) |arg, reaches| try self.push(arg, reaches);
                                }
                            }
                        },
                    }
                },
                .ty_var, .underscore_type_var, .underscore, .ty => {},
                .malformed => self.invalid = true,
            }
        }
        return changed;
    }

    fn analyze(allocator: Allocator, declaration: PositionAdapter.Key, reach: Reach) Allocator.Error!?[]Reach {
        var self = ReachAnalysis{ .allocator = allocator };
        defer {
            for (self.states.items) |state| allocator.free(state.results);
            self.states.deinit(allocator);
            self.indices.deinit(allocator);
            self.pending.deinit(allocator);
        }
        const root = (try self.register(.{ .declaration = declaration, .reach = reach })) orelse return null;
        var changed = true;
        while (changed and !self.invalid) {
            changed = false;
            const count = self.states.items.len;
            for (0..count) |index| changed = (try self.evaluate(index)) or changed;
            changed = changed or count != self.states.items.len;
        }
        if (self.invalid) return null;
        const result = try allocator.alloc(Reach, self.states.items[root].results.len);
        for (result, self.states.items[root].results) |*value, mask| {
            value.* = if (mask == 0 or mask & bit(.nested) != 0) .nested else if (@popCount(mask) > 1) .try_row else @fromBackingInt(@intCast(@ctz(mask)));
        }
        return result;
    }
};
