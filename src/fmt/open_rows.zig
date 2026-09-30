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
//! is an output, function arguments negate the surrounding polarity, and every
//! other position preserves it; a type application's argument is generated at
//! the application's polarity composed with the variance of the declaration
//! formal it is substituted for; a where-method signature opens only the rows
//! the result-row widening adapter can re-tag. Which annotations qualify at all
//! mirrors `Check.checkDef`'s `generalizes_regardless` together with
//! `Check.collectHostBoundaryAnnotations`.
//!
//! The checker reads resolved names; the parse AST has only spellings. So every
//! question the checker answers from a resolved declaration is answered here
//! over EVERY declaration the spelling could resolve to—each same-named type
//! declaration anywhere in the file, the `Builtin` type of that name, and any
//! import that could introduce it—and a `..` is dropped only when every
//! candidate gives the same answer. A question with no unanimous answer keeps
//! the `..`, so a row is never opened that the checker would generate closed or
//! rigid: this module may keep a `..` the checker calls redundant, and never
//! drops one it does not.

const std = @import("std");
const parse = @import("parse");

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

/// The parameterized types every module can name unqualified from `Builtin`.
/// Every parameterized `Builtin` declaration is covariant in each of its
/// formals or leaves the formal unused (design.md "Polarity"), which is what
/// `Check.applyDeclKnowledge` answers for a reference into `Builtin`.
const builtin_parameterized_types = [_][]const u8{ "List", "Box", "Try", "Dict", "Set", "Iter", "Stream" };

/// `Try`'s arity and the index of its error row, as `Check.applyTryErrorArgIndex`
/// counts them.
const try_type_name = "Try";
const try_arity: usize = 2;
const try_error_arg_index: usize = 1;

const Polarity = enum {
    pos,
    neg,

    fn flip(self: Polarity) Polarity {
        return switch (self) {
            .pos => .neg,
            .neg => .pos,
        };
    }
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

/// `Check.FormalVariance`.
const Variance = enum {
    unused,
    covariant,
    contravariant,
    invariant,

    fn join(self: Variance, other: Variance) Variance {
        if (self == .unused) return other;
        if (other == .unused) return self;
        if (self == other) return self;
        return .invariant;
    }
};

/// How one argument of a type application is generated relative to the
/// application itself.
const ArgRule = enum {
    /// At the application's own polarity (a covariant or unused formal).
    keep,
    /// At the flipped polarity (a contravariant formal).
    flip,
    /// At the negative polarity whatever the application's is (an invariant
    /// formal).
    neg,
    /// The declaration's variance is unknown: the argument is generated as
    /// written at every depth, and a formal found beneath it is invariant.
    opaque_variance,

    fn ofVariance(variance: Variance) ArgRule {
        return switch (variance) {
            .unused, .covariant => .keep,
            .contravariant => .flip,
            .invariant => .neg,
        };
    }

    fn apply(self: ArgRule, polarity: Polarity) Polarity {
        return switch (self) {
            .keep, .opaque_variance => polarity,
            .flip => polarity.flip(),
            .neg => .neg,
        };
    }
};

/// `Check.VariancePosition`: where a position of a declaration body sits
/// relative to the declaration's root.
const Position = enum {
    covariant,
    contravariant,
    invariant,

    fn flip(self: Position) Position {
        return switch (self) {
            .covariant => .contravariant,
            .contravariant => .covariant,
            .invariant => .invariant,
        };
    }

    fn through(self: Position, variance: Variance) Position {
        return switch (variance) {
            .unused, .covariant => self,
            .contravariant => self.flip(),
            .invariant => .invariant,
        };
    }

    fn occurrence(self: Position) Variance {
        return switch (self) {
            .covariant => .covariant,
            .contravariant => .contravariant,
            .invariant => .invariant,
        };
    }
};

/// A question the walk could not answer the way the checker does.
const Unanswered = error{Unanswered};

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
    /// Solved variances of local type declarations' formals, or null for a
    /// declaration this file cannot answer for.
    decl_variances: std.AutoHashMapUnmanaged(AST.Statement.Idx, ?[]Variance),

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
            .decl_variances = .{},
        };
        errdefer self.deinit();

        const tags = ast.store.nodes.items.items(.tag);
        for (tags, 0..) |tag, node_index| {
            const is_type_decl = tag == .type_decl or tag == .type_decl_nominal or
                tag == .type_decl_opaque or tag == .type_decl_where_alias;
            if (is_type_decl) {
                const stmt_idx: AST.Statement.Idx = @enumFromInt(node_index);
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
                const stmt_idx: AST.Statement.Idx = @enumFromInt(node_index);
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
        var decls_it = self.type_decls.valueIterator();
        while (decls_it.next()) |list| list.deinit(self.gpa);
        self.type_decls.deinit(self.gpa);
        self.external_names.deinit(self.gpa);
        self.provided_names.deinit(self.gpa);
        var variances = self.decl_variances.valueIterator();
        while (variances.next()) |solved| if (solved.*) |owned| self.gpa.free(owned);
        self.decl_variances.deinit(self.gpa);
        self.redundant.deinit(self.gpa);
    }

    /// Whether the anonymous `..` of this tag union is redundant.
    pub fn isRedundant(self: *const OpenRows, anno_idx: AST.TypeAnno.Idx) bool {
        return self.redundant.isSet(@intFromEnum(anno_idx));
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
            if (!self.annotationGeneralizesRegardless(anno.name, next, scope)) continue;
            try self.markAnnotation(anno.anno, anno.where);
        }
    }

    /// Whether the definition this annotation belongs to generalizes whatever
    /// its annotation writes (`Check.checkDef`'s `generalizes_regardless`) and
    /// is not a host boundary (`Check.collectHostBoundaryAnnotations`). A
    /// value binding does not: on a value, `..` is the opt-in to a quantified
    /// row.
    fn annotationGeneralizesRegardless(self: *const OpenRows, name_tok: Token.Idx, next: ?AST.Statement.Idx, scope: StatementScope) bool {
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

    /// One annotation the walk still has to visit, with its context.
    const WalkItem = struct {
        anno: AST.TypeAnno.Idx,
        ctx: Ctx,
        polarity: Polarity,
    };

    /// `Check.generateAnnoTypeInPlace`, deciding only where an anonymous `..`
    /// is generated exactly as its absence would be. Annotations nest as
    /// deeply as source does, so the walk keeps its pending annotations on an
    /// explicit stack, visiting them in source order.
    fn walk(self: *OpenRows, root: AST.TypeAnno.Idx, root_ctx: Ctx, root_polarity: Polarity, occurrences: ?*VarOccurrences) Allocator.Error!void {
        var pending = std.ArrayList(WalkItem).empty;
        defer pending.deinit(self.gpa);
        try pending.append(self.gpa, .{ .anno = root, .ctx = root_ctx, .polarity = root_polarity });
        while (pending.pop()) |item| {
            const children_start = pending.items.len;
            try self.walkOne(&pending, item.anno, item.ctx, item.polarity, occurrences);
            std.mem.reverse(WalkItem, pending.items[children_start..]);
        }
    }

    /// Visit one annotation, pushing its children in source order.
    fn walkOne(self: *OpenRows, pending: *std.ArrayList(WalkItem), anno_idx: AST.TypeAnno.Idx, ctx: Ctx, polarity: Polarity, occurrences: ?*VarOccurrences) Allocator.Error!void {
        switch (self.ast.store.getTypeAnno(anno_idx)) {
            .ty_var => |v| try noteOccurrence(self.gpa, occurrences, self.tokenName(v.tok), ctx),
            .underscore_type_var => |v| try noteOccurrence(self.gpa, occurrences, self.tokenName(v.tok), ctx),
            .underscore, .ty, .malformed => {},
            .parens => |parens| try pending.append(self.gpa, .{ .anno = parens.anno, .ctx = ctx, .polarity = polarity }),
            .@"fn" => |func| {
                for (self.ast.store.typeAnnoSlice(func.args)) |arg| {
                    try pending.append(self.gpa, .{ .anno = arg, .ctx = ctx.withReach(.nested), .polarity = polarity.flip() });
                }
                const ret_reach: Reach = switch (ctx.reach) {
                    .signature => .result,
                    .result, .try_row, .nested => .nested,
                };
                try pending.append(self.gpa, .{ .anno = func.ret, .ctx = ctx.withReach(ret_reach), .polarity = polarity });
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
                        self.redundant.set(@intFromEnum(anno_idx));
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

                // An application this walk cannot answer for is generated as
                // written beneath it, the most conservative answer.
                const rules = try self.gpa.alloc(ArgRule, args.len);
                defer self.gpa.free(rules);
                const candidate_rules = try self.gpa.alloc(ArgRule, args.len);
                defer self.gpa.free(candidate_rules);
                const rules_known = if (self.applyArgRules(head, rules, candidate_rules)) |_| true else |err| switch (err) {
                    error.Unanswered => false,
                    error.OutOfMemory => return error.OutOfMemory,
                };

                const try_error_index: ?usize = switch (ctx.reach) {
                    .result => try self.tryErrorArgIndex(head, args.len),
                    .signature, .try_row, .nested => null,
                };

                for (args, 0..) |arg, arg_index| {
                    const rule: ArgRule = if (rules_known) rules[arg_index] else .opaque_variance;
                    const reach: Reach = if (try_error_index == arg_index) .try_row else .nested;
                    const arg_ctx = switch (rule) {
                        .opaque_variance => ctx.withReach(reach).withOpening(.as_written),
                        .keep, .flip, .neg => ctx.withReach(reach),
                    };
                    try pending.append(self.gpa, .{ .anno = arg, .ctx = arg_ctx, .polarity = rule.apply(polarity) });
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
        for (builtin_parameterized_types) |builtin_name| {
            if (std.mem.eql(u8, builtin_name, name)) builtin = true;
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

    /// How each argument of an application of `head` is generated, answered
    /// unanimously over every declaration `head` could name
    /// (`Check.applyFormalVariances` with `Check.applyDeclKnowledge`).
    fn applyArgRules(
        self: *OpenRows,
        head: AST.TypeAnno.Idx,
        out: []ArgRule,
        candidate_rules: []ArgRule,
    ) (Allocator.Error || Unanswered)!void {
        const found = self.candidates(head);
        var answered = false;

        if (found.builtin) {
            @memset(candidate_rules, .keep);
            try agree(out, &answered, candidate_rules);
        }
        if (found.external) {
            @memset(candidate_rules, .opaque_variance);
            try agree(out, &answered, candidate_rules);
        }
        for (found.locals) |decl_idx| {
            const variances = try self.localDeclVariances(decl_idx) orelse return Unanswered.Unanswered;
            if (variances.len != out.len) return Unanswered.Unanswered;
            for (variances, candidate_rules) |variance, *rule| rule.* = ArgRule.ofVariance(variance);
            try agree(out, &answered, candidate_rules);
        }
        if (!answered) return Unanswered.Unanswered;
    }

    /// Record one candidate's answer, requiring it to match every earlier one.
    fn agree(out: []ArgRule, answered: *bool, rules: []const ArgRule) Unanswered!void {
        if (answered.*) {
            for (out, rules) |existing, rule| {
                if (existing != rule) return Unanswered.Unanswered;
            }
            return;
        }
        @memcpy(out, rules);
        answered.* = true;
    }

    /// What a written type name inside a declaration body resolves to, when
    /// exactly one declaration could be the one it names.
    const Resolution = union(enum) {
        local: AST.Statement.Idx,
        /// A `Builtin` type: covariant in every formal.
        covariant,
        /// Another module's type, whose variance is unknown.
        unknown,
        /// More than one declaration could be the one it names.
        ambiguous,
    };

    fn resolveHead(self: *const OpenRows, head: AST.TypeAnno.Idx) Resolution {
        const found = self.candidates(head);
        const count = found.locals.len + @intFromBool(found.builtin) + @intFromBool(found.external);
        if (count != 1) return .ambiguous;
        if (found.locals.len == 1) return .{ .local = found.locals[0] };
        if (found.builtin) return .covariant;
        return .unknown;
    }

    /// A local type declaration's formals and body.
    const DeclBody = struct {
        formals: []const AST.TypeAnno.Idx,
        body: AST.TypeAnno.Idx,
    };

    fn declBody(self: *const OpenRows, decl_idx: AST.Statement.Idx) ?DeclBody {
        const decl = self.ast.store.getStatement(decl_idx).type_decl;
        const header = self.ast.store.getTypeHeader(decl.header) catch return null;
        return .{ .formals = self.ast.store.typeAnnoSlice(header.args), .body = decl.anno };
    }

    /// `Check.localDeclFormalVariances`: the variance of each of a local
    /// declaration's formals, solved together with every declaration it
    /// reaches the same way. Null when this file cannot answer
    /// the way the checker does: some reference in the group could name
    /// more than one declaration, or a declaration does not parse.
    fn localDeclVariances(self: *OpenRows, decl_idx: AST.Statement.Idx) Allocator.Error!?[]const Variance {
        if (self.decl_variances.get(decl_idx)) |solved| return solved;

        var group = std.ArrayListUnmanaged(AST.Statement.Idx).empty;
        defer group.deinit(self.gpa);
        var solving = std.AutoHashMapUnmanaged(AST.Statement.Idx, []Variance).empty;
        defer {
            var owned = solving.valueIterator();
            while (owned.next()) |variances| self.gpa.free(variances.*);
            solving.deinit(self.gpa);
        }
        var pending = std.ArrayListUnmanaged(AST.TypeAnno.Idx).empty;
        defer pending.deinit(self.gpa);

        const answerable = discover: {
            if (!try self.addGroupMember(decl_idx, &group, &solving)) break :discover false;
            var member_index: usize = 0;
            while (member_index < group.items.len) : (member_index += 1) {
                pending.clearRetainingCapacity();
                try pending.append(self.gpa, self.declBody(group.items[member_index]).?.body);
                while (pending.pop()) |anno_idx| {
                    const children_answerable = try self.appendBodyChildren(anno_idx, &pending);
                    if (!children_answerable) break :discover false;
                    const apply = switch (self.ast.store.getTypeAnno(anno_idx)) {
                        .apply => |apply| apply,
                        .ty_var, .underscore_type_var, .underscore, .ty, .tag_union, .tuple, .record, .@"fn", .parens, .malformed => continue,
                    };
                    switch (self.resolveHead(self.ast.store.typeAnnoSlice(apply.args)[0])) {
                        .ambiguous => break :discover false,
                        .covariant, .unknown => {},
                        .local => |referenced| {
                            if (self.decl_variances.get(referenced)) |solved| {
                                if (solved == null) break :discover false;
                            } else if (!solving.contains(referenced)) {
                                if (!try self.addGroupMember(referenced, &group, &solving)) break :discover false;
                            }
                        },
                    }
                }
            }
            break :discover true;
        };

        try self.decl_variances.ensureUnusedCapacity(self.gpa, @intCast(group.items.len));
        if (!answerable) {
            // Every member reaches the reference this file cannot answer.
            for (group.items) |member| self.decl_variances.putAssumeCapacity(member, null);
            return null;
        }

        var round = std.ArrayListUnmanaged(Variance).empty;
        defer round.deinit(self.gpa);
        var changed = true;
        while (changed) {
            changed = false;
            for (group.items) |member| {
                const current = solving.get(member).?;
                round.clearRetainingCapacity();
                try round.appendNTimes(self.gpa, .unused, current.len);
                try self.accumulateFormalVariances(member, round.items, &solving, &pending);
                // Each round's occurrences join the previous estimate, as
                // in `Check.localDeclFormalVariances`.
                for (current, round.items) |*estimate, next| {
                    const joined = estimate.join(next);
                    if (joined == estimate.*) continue;
                    estimate.* = joined;
                    changed = true;
                }
            }
        }

        for (group.items) |member| {
            const solved = solving.fetchRemove(member).?;
            self.decl_variances.putAssumeCapacity(member, solved.value);
        }
        return self.decl_variances.get(decl_idx).?;
    }

    /// Add a declaration to the group being solved; false when it does not
    /// parse.
    fn addGroupMember(
        self: *OpenRows,
        decl_idx: AST.Statement.Idx,
        group: *std.ArrayListUnmanaged(AST.Statement.Idx),
        solving: *std.AutoHashMapUnmanaged(AST.Statement.Idx, []Variance),
    ) Allocator.Error!bool {
        const decl = self.declBody(decl_idx) orelse return false;
        try group.ensureUnusedCapacity(self.gpa, 1);
        try solving.ensureUnusedCapacity(self.gpa, 1);
        const variances = try self.gpa.alloc(Variance, decl.formals.len);
        @memset(variances, .unused);
        group.appendAssumeCapacity(decl_idx);
        solving.putAssumeCapacity(decl_idx, variances);
        return true;
    }

    /// The type positions directly beneath `anno_idx` in a declaration body;
    /// false when one does not parse.
    fn appendBodyChildren(self: *const OpenRows, anno_idx: AST.TypeAnno.Idx, out: *std.ArrayListUnmanaged(AST.TypeAnno.Idx)) Allocator.Error!bool {
        switch (self.ast.store.getTypeAnno(anno_idx)) {
            .ty_var, .underscore_type_var, .underscore, .ty, .malformed => {},
            .parens => |parens| try out.append(self.gpa, parens.anno),
            .@"fn" => |func| {
                try out.appendSlice(self.gpa, self.ast.store.typeAnnoSlice(func.args));
                try out.append(self.gpa, func.ret);
            },
            .tag_union => |tag_union| {
                for (self.ast.store.typeAnnoSlice(tag_union.tags)) |tag_idx| {
                    switch (self.ast.store.getTypeAnno(tag_idx)) {
                        .apply => |tag| try out.appendSlice(self.gpa, self.ast.store.typeAnnoSlice(tag.args)[1..]),
                        .ty_var, .underscore_type_var, .underscore, .ty, .tag_union, .tuple, .record, .@"fn", .parens, .malformed => {},
                    }
                }
                switch (tag_union.ext) {
                    .named => |named| try out.append(self.gpa, named.anno),
                    .open, .closed => {},
                }
            },
            .tuple => |tuple| try out.appendSlice(self.gpa, self.ast.store.typeAnnoSlice(tuple.annos)),
            .record => |record| {
                for (self.ast.store.annoRecordFieldSlice(record.fields)) |field_idx| {
                    const field = self.ast.store.getAnnoRecordField(field_idx) catch return false;
                    try out.append(self.gpa, field.ty);
                }
                switch (record.ext) {
                    .named => |named| try out.append(self.gpa, named.anno),
                    .open, .closed => {},
                }
            },
            .apply => |apply| try out.appendSlice(self.gpa, self.ast.store.typeAnnoSlice(apply.args)[1..]),
        }
        return true;
    }

    /// One position of a declaration body still to be visited.
    const VarianceWalkItem = struct {
        anno: AST.TypeAnno.Idx,
        position: Position,
    };

    /// `Check.accumulateFormalVariances`: join into `out[i]` the variance of
    /// every occurrence of `decl_idx`'s formal `i` within its body.
    fn accumulateFormalVariances(
        self: *OpenRows,
        decl_idx: AST.Statement.Idx,
        out: []Variance,
        solving: *const std.AutoHashMapUnmanaged(AST.Statement.Idx, []Variance),
        scratch: *std.ArrayListUnmanaged(AST.TypeAnno.Idx),
    ) Allocator.Error!void {
        const decl = self.declBody(decl_idx).?;
        var walk_items = std.ArrayListUnmanaged(VarianceWalkItem).empty;
        defer walk_items.deinit(self.gpa);
        try walk_items.append(self.gpa, .{ .anno = decl.body, .position = .covariant });
        while (walk_items.pop()) |here| {
            switch (self.ast.store.getTypeAnno(here.anno)) {
                .ty_var => |v| self.joinFormal(v.tok, decl.formals, here.position, out),
                .underscore_type_var => |v| self.joinFormal(v.tok, decl.formals, here.position, out),
                .@"fn" => |func| {
                    for (self.ast.store.typeAnnoSlice(func.args)) |arg| {
                        try walk_items.append(self.gpa, .{ .anno = arg, .position = here.position.flip() });
                    }
                    try walk_items.append(self.gpa, .{ .anno = func.ret, .position = here.position });
                },
                .apply => |apply| {
                    const all_args = self.ast.store.typeAnnoSlice(apply.args);
                    const args = all_args[1..];
                    const variances: ?[]const Variance = switch (self.resolveHead(all_args[0])) {
                        .local => |referenced| if (self.decl_variances.get(referenced)) |solved| solved.? else solving.get(referenced).?,
                        .covariant, .unknown, .ambiguous => null,
                    };
                    const unknown = self.resolveHead(all_args[0]) == .unknown;
                    for (args, 0..) |arg, index| {
                        const position: Position = if (unknown)
                            .invariant
                        else if (variances) |formals|
                            if (formals.len == args.len) here.position.through(formals[index]) else here.position
                        else
                            here.position;
                        try walk_items.append(self.gpa, .{ .anno = arg, .position = position });
                    }
                },
                .parens, .tag_union, .tuple, .record, .underscore, .ty, .malformed => {
                    scratch.clearRetainingCapacity();
                    _ = try self.appendBodyChildren(here.anno, scratch);
                    for (scratch.items) |child| try walk_items.append(self.gpa, .{ .anno = child, .position = here.position });
                },
            }
        }
    }

    fn joinFormal(
        self: *const OpenRows,
        var_tok: Token.Idx,
        formals: []const AST.TypeAnno.Idx,
        position: Position,
        out: []Variance,
    ) void {
        const formal_index = self.formalIndex(self.tokenName(var_tok), formals) orelse return;
        out[formal_index] = out[formal_index].join(position.occurrence());
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

    /// A head whose candidate declarations are being asked for their `Try`
    /// error row, and the argument count it is applied to.
    const TryRowHead = struct {
        head: AST.TypeAnno.Idx,
        arity: usize,
        locals: []const AST.Statement.Idx,
        next_local: usize = 0,
        answer: ?usize,
    };

    /// A local alias whose body's head is being asked for its `Try` error
    /// row.
    const TryRowAlias = struct {
        decl_idx: AST.Statement.Idx,
        formals: []const AST.TypeAnno.Idx,
        body_args: []const AST.TypeAnno.Idx,
        body_is_try: bool,
    };

    const TryRowFrame = union(enum) {
        head: TryRowHead,
        alias: TryRowAlias,
    };

    /// `Check.applyTryErrorArgIndex`: which argument of an application of
    /// `head` lands in the builtin `Try`'s error row, answered unanimously
    /// over every declaration `head` could name. Every step fails closed: a
    /// shape not recognized exactly, disagreeing candidates, or an alias chain
    /// that returns to a declaration it is still expanding leaves the question
    /// unanswered.
    fn tryErrorArgIndex(self: *OpenRows, head: AST.TypeAnno.Idx, arity: usize) Allocator.Error!?usize {
        var frames = std.ArrayList(TryRowFrame).empty;
        defer frames.deinit(self.gpa);
        // Aliases being expanded, to detect a chain returning to itself.
        var expanding = std.AutoHashMapUnmanaged(AST.Statement.Idx, void).empty;
        defer expanding.deinit(self.gpa);

        try frames.append(self.gpa, .{ .head = self.tryRowHead(head, arity) orelse return null });
        var input: ?usize = null;
        while (frames.items.len != 0) {
            const top = &frames.items[frames.items.len - 1];
            switch (top.*) {
                .head => |*head_frame| {
                    if (input) |index| {
                        // The previous local declaration answered `index`.
                        input = null;
                        if (head_frame.answer) |existing| {
                            if (existing != index) return null;
                        }
                        head_frame.answer = index;
                    }
                    if (head_frame.next_local < head_frame.locals.len) {
                        const decl_idx = head_frame.locals[head_frame.next_local];
                        head_frame.next_local += 1;
                        const alias = self.tryRowAlias(decl_idx, head_frame.arity) orelse return null;
                        if ((try expanding.getOrPut(self.gpa, decl_idx)).found_existing) return null;
                        const body_head = alias.body_args[0];
                        const body_arity = alias.body_args.len - 1;
                        try frames.append(self.gpa, .{ .alias = alias });
                        try frames.append(self.gpa, .{ .head = self.tryRowHead(body_head, body_arity) orelse return null });
                        continue;
                    }
                    const answer = head_frame.answer orelse return null;
                    _ = frames.pop();
                    input = answer;
                },
                .alias => |alias| {
                    const inner_index = input orelse return null;
                    _ = frames.pop();
                    _ = expanding.remove(alias.decl_idx);
                    input = self.tryRowAliasIndex(alias, inner_index) orelse return null;
                },
            }
        }
        return input;
    }

    /// The candidates of `head` to ask, or null when the question has no
    /// answer at this head.
    fn tryRowHead(self: *const OpenRows, head: AST.TypeAnno.Idx, arity: usize) ?TryRowHead {
        const found = self.candidates(head);
        if (found.external) return null;
        var answer: ?usize = null;
        if (found.builtin) {
            const name = self.tokenName(self.ast.store.getTypeAnno(head).ty.token);
            if (!std.mem.eql(u8, name, try_type_name) or arity != try_arity) return null;
            answer = try_error_arg_index;
        }
        return .{ .head = head, .arity = arity, .locals = found.locals, .answer = answer };
    }

    /// A local declaration's shape for the question: a transparent alias of
    /// `arity` formals whose body is an application.
    fn tryRowAlias(self: *const OpenRows, decl_idx: AST.Statement.Idx, arity: usize) ?TryRowAlias {
        const decl = self.ast.store.getStatement(decl_idx).type_decl;
        if (decl.kind != .alias) return null;
        const header = self.ast.store.getTypeHeader(decl.header) catch return null;
        const formals = self.ast.store.typeAnnoSlice(header.args);
        if (formals.len != arity) return null;

        const body = switch (self.ast.store.getTypeAnno(self.skipParens(decl.anno))) {
            .apply => |apply| apply,
            .ty_var, .underscore_type_var, .underscore, .ty, .tag_union, .tuple, .record, .@"fn", .parens, .malformed => return null,
        };
        const body_all_args = self.ast.store.typeAnnoSlice(body.args);
        return .{
            .decl_idx = decl_idx,
            .formals = formals,
            .body_args = body_all_args,
            .body_is_try = self.candidatesAreOnlyBuiltin(body_all_args[0]),
        };
    }

    /// Map the body head's answer back to one of the alias's formals. Past a
    /// `Try` itself only its error row must be a formal; through an alias
    /// over an alias every argument must be passed straight through.
    fn tryRowAliasIndex(self: *const OpenRows, alias: TryRowAlias, inner_index: usize) ?usize {
        const body_args = alias.body_args[1..];
        if (!alias.body_is_try) {
            for (body_args) |body_arg| {
                const name = self.varName(body_arg) orelse return null;
                _ = self.formalIndex(name, alias.formals) orelse return null;
            }
        }
        const name = self.varName(body_args[inner_index]) orelse return null;
        return self.formalIndex(name, alias.formals);
    }

    fn candidatesAreOnlyBuiltin(self: *const OpenRows, head: AST.TypeAnno.Idx) bool {
        const found = self.candidates(head);
        return found.builtin and !found.external and found.locals.len == 0;
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
