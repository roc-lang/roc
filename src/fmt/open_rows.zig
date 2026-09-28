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

/// The same bounds `Check` walks declarations with. A walk that reaches one
/// gives no answer, and an unanswered question keeps the `..`.
const max_tracked_alias_formals: usize = 8;
const max_formal_variance_decl_depth: usize = 8;
const max_formal_variance_pending: usize = 256;
const max_formal_variance_nodes: usize = 2048;
const max_try_alias_depth: usize = 64;

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

/// `Check.FormalVariance`.
const Variance = enum {
    unused,
    covariant,
    output,
    contravariant,
    invariant,

    fn join(self: Variance, other: Variance) Variance {
        if (self == .unused) return other;
        if (other == .unused) return self;
        if (self == other) return self;
        if ((self == .covariant and other == .output) or
            (self == .output and other == .covariant)) return .covariant;
        return .invariant;
    }

    fn ofOccurrence(polarity: ?Polarity) Variance {
        return if (polarity) |position| switch (position) {
            .pos => .output,
            .neg => .contravariant,
        } else .covariant;
    }
};

/// How one argument of a type application is generated relative to the
/// application itself.
const ArgRule = enum {
    /// At the application's own polarity (a covariant or unused formal).
    keep,
    /// In a function result, independently of the reference position.
    pos,
    /// At the negative polarity whatever the application's is (an invariant
    /// formal).
    neg,
    /// The declaration's variance is unknown: the argument is generated as
    /// written at every depth, and a formal found beneath it is invariant.
    opaque_variance,

    fn ofVariance(variance: Variance) ArgRule {
        return switch (variance) {
            .unused, .covariant => .keep,
            .output => .pos,
            .contravariant, .invariant => .neg,
        };
    }

    fn apply(self: ArgRule, polarity: anytype) @TypeOf(polarity) {
        return switch (self) {
            .keep, .opaque_variance => polarity,
            .pos => .pos,
            .neg => .neg,
        };
    }
};

/// State of one `applyArgRules` question, with the bounds of
/// `Check.FormalVarianceWalk`.
const VarianceWalk = struct {
    /// Declarations whose bodies are being walked, outermost first.
    open_decls: [max_formal_variance_decl_depth]AST.Statement.Idx = undefined,
    open_decls_len: usize = 0,
    /// Positions the walk may still visit, counted as the checker counts them.
    fuel: usize = max_formal_variance_nodes,

    fn isOpen(self: *const VarianceWalk, decl_idx: AST.Statement.Idx) bool {
        for (self.open_decls[0..self.open_decls_len]) |open_decl| {
            if (open_decl == decl_idx) return true;
        }
        return false;
    }

    fn visit(self: *VarianceWalk) Unanswered!void {
        if (self.fuel == 0) return Unanswered.Unanswered;
        self.fuel -= 1;
    }
};

/// One position of a declaration body, relative to the declaration's root.
const VariancePosition = struct {
    polarity: ?Polarity,
    /// Below a reference whose variance is unknown.
    unknown: bool = false,
    /// How many positions the checker's explicit stack could be holding
    /// alongside this one: at most every child of every ancestor within the
    /// declaration.
    pending: usize = 0,

    fn child(self: VariancePosition, sibling_count: usize) VariancePosition {
        return .{ .polarity = self.polarity, .unknown = self.unknown, .pending = self.pending + sibling_count };
    }

    fn withPolarity(self: VariancePosition, polarity: ?Polarity) VariancePosition {
        return .{ .polarity = polarity, .unknown = self.unknown, .pending = self.pending };
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

    /// `Check.generateAnnoTypeInPlace`, deciding only where an anonymous `..`
    /// is generated exactly as its absence would be.
    fn walk(self: *OpenRows, anno_idx: AST.TypeAnno.Idx, ctx: Ctx, polarity: Polarity, occurrences: ?*VarOccurrences) Allocator.Error!void {
        switch (self.ast.store.getTypeAnno(anno_idx)) {
            .ty_var => |v| try noteOccurrence(self.gpa, occurrences, self.tokenName(v.tok), ctx),
            .underscore_type_var => |v| try noteOccurrence(self.gpa, occurrences, self.tokenName(v.tok), ctx),
            .underscore, .ty, .malformed => {},
            .parens => |parens| try self.walk(parens.anno, ctx, polarity, occurrences),
            .@"fn" => |func| {
                for (self.ast.store.typeAnnoSlice(func.args)) |arg| {
                    try self.walk(arg, ctx.withReach(.nested), .neg, occurrences);
                }
                const ret_reach: Reach = switch (ctx.reach) {
                    .signature => .result,
                    .result, .try_row, .nested => .nested,
                };
                try self.walk(func.ret, ctx.withReach(ret_reach), .pos, occurrences);
            },
            .tag_union => |tag_union| {
                const tags = self.ast.store.typeAnnoSlice(tag_union.tags);
                for (tags) |tag_idx| {
                    switch (self.ast.store.getTypeAnno(tag_idx)) {
                        .apply => |tag| for (self.ast.store.typeAnnoSlice(tag.args)[1..]) |payload| {
                            try self.walk(payload, ctx.withReach(.nested), polarity, occurrences);
                        },
                        .ty_var, .underscore_type_var, .underscore, .ty, .tag_union, .tuple, .record, .@"fn", .parens, .malformed => {},
                    }
                }
                switch (tag_union.ext) {
                    .open => if (tags.len > 0 and polarity == .pos and outputOpens(ctx)) {
                        self.redundant.set(@intFromEnum(anno_idx));
                    },
                    .named => |named| try self.walk(named.anno, ctx.withReach(.nested), polarity, occurrences),
                    .closed => {},
                }
            },
            .tuple => |tuple| for (self.ast.store.typeAnnoSlice(tuple.annos)) |elem| {
                try self.walk(elem, ctx.withReach(.nested), polarity, occurrences);
            },
            .record => |record| {
                for (self.ast.store.annoRecordFieldSlice(record.fields)) |field_idx| {
                    const field = self.ast.store.getAnnoRecordField(field_idx) catch continue;
                    try self.walk(field.ty, ctx.withReach(.nested), polarity, occurrences);
                }
                switch (record.ext) {
                    .named => |named| try self.walk(named.anno, ctx.withReach(.nested), polarity, occurrences),
                    .open, .closed => {},
                }
            },
            .apply => |apply| {
                const all_args = self.ast.store.typeAnnoSlice(apply.args);
                const head = all_args[0];
                const args = all_args[1..];

                // An application this walk cannot answer for is generated as
                // written beneath it, the most conservative answer.
                var rules: [max_tracked_alias_formals]ArgRule = undefined;
                var variance_walk = VarianceWalk{};
                const rules_known = args.len <= max_tracked_alias_formals and
                    if (self.applyArgRules(head, args.len, &rules, &variance_walk)) |_| true else |_| false;

                const try_error_index: ?usize = switch (ctx.reach) {
                    .result => self.tryErrorArgIndex(head, args.len, 0),
                    .signature, .try_row, .nested => null,
                };

                for (args, 0..) |arg, arg_index| {
                    const rule: ArgRule = if (rules_known) rules[arg_index] else .opaque_variance;
                    const reach: Reach = if (try_error_index == arg_index) .try_row else .nested;
                    const arg_ctx = switch (rule) {
                        .opaque_variance => ctx.withReach(reach).withOpening(.as_written),
                        .keep, .pos, .neg => ctx.withReach(reach),
                    };
                    try self.walk(arg, arg_ctx, rule.apply(polarity), occurrences);
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
        self: *const OpenRows,
        head: AST.TypeAnno.Idx,
        arity: usize,
        out: *[max_tracked_alias_formals]ArgRule,
        variance_walk: *VarianceWalk,
    ) Unanswered!void {
        std.debug.assert(arity <= max_tracked_alias_formals);
        const found = self.candidates(head);
        var answered = false;
        var candidate_rules: [max_tracked_alias_formals]ArgRule = undefined;

        if (found.builtin) {
            try agree(out, &answered, uniformRules(&candidate_rules, arity, .keep));
        }
        if (found.external) {
            try agree(out, &answered, uniformRules(&candidate_rules, arity, .opaque_variance));
        }
        for (found.locals) |decl_idx| {
            if (variance_walk.isOpen(decl_idx)) {
                // A cycle cannot prove an explicit row extension redundant.
                try agree(out, &answered, uniformRules(&candidate_rules, arity, .opaque_variance));
                continue;
            }
            var variances: [max_tracked_alias_formals]Variance = undefined;
            const formals_len = try self.declFormalVariances(decl_idx, &variances, variance_walk);
            if (formals_len != arity) return Unanswered.Unanswered;
            for (variances[0..arity], candidate_rules[0..arity]) |variance, *rule| rule.* = ArgRule.ofVariance(variance);
            try agree(out, &answered, candidate_rules[0..arity]);
        }
        if (!answered) return Unanswered.Unanswered;
    }

    fn uniformRules(rules: *[max_tracked_alias_formals]ArgRule, arity: usize, rule: ArgRule) []const ArgRule {
        for (rules[0..arity]) |*slot| slot.* = rule;
        return rules[0..arity];
    }

    /// Record one candidate's answer, requiring it to match every earlier one.
    fn agree(out: *[max_tracked_alias_formals]ArgRule, answered: *bool, rules: []const ArgRule) Unanswered!void {
        if (answered.*) {
            for (out[0..rules.len], rules) |existing, rule| {
                if (existing != rule) return Unanswered.Unanswered;
            }
            return;
        }
        @memcpy(out[0..rules.len], rules);
        answered.* = true;
    }

    /// `Check.declFormalVariances`: the variance of each of a type
    /// declaration's formals within its body.
    fn declFormalVariances(
        self: *const OpenRows,
        decl_idx: AST.Statement.Idx,
        out: *[max_tracked_alias_formals]Variance,
        variance_walk: *VarianceWalk,
    ) Unanswered!usize {
        if (variance_walk.open_decls_len == max_formal_variance_decl_depth) return Unanswered.Unanswered;
        const decl = self.ast.store.getStatement(decl_idx).type_decl;
        const header = self.ast.store.getTypeHeader(decl.header) catch return Unanswered.Unanswered;
        const formals = self.ast.store.typeAnnoSlice(header.args);
        if (formals.len > max_tracked_alias_formals) return Unanswered.Unanswered;
        for (out[0..formals.len]) |*variance| variance.* = .unused;

        variance_walk.open_decls[variance_walk.open_decls_len] = decl_idx;
        variance_walk.open_decls_len += 1;
        defer variance_walk.open_decls_len -= 1;

        try self.accumulateFormalVariances(decl.anno, formals, .{ .polarity = null }, out, variance_walk);
        return formals.len;
    }

    /// `Check.accumulateFormalVariances`, recursive where the checker keeps an
    /// explicit stack. `here.pending` bounds that stack's height from above,
    /// so this walk gives up whenever the checker's could.
    fn accumulateFormalVariances(
        self: *const OpenRows,
        anno_idx: AST.TypeAnno.Idx,
        formals: []const AST.TypeAnno.Idx,
        here: VariancePosition,
        out: *[max_tracked_alias_formals]Variance,
        variance_walk: *VarianceWalk,
    ) Unanswered!void {
        try variance_walk.visit();
        const anno = self.ast.store.getTypeAnno(anno_idx);
        const child_count: usize = switch (anno) {
            .ty_var, .underscore_type_var, .underscore, .ty, .malformed => 0,
            .parens => 1,
            .@"fn" => |func| self.ast.store.typeAnnoSlice(func.args).len + 1,
            .tag_union => |tag_union| self.ast.store.typeAnnoSlice(tag_union.tags).len +
                @intFromBool(tag_union.ext != .closed),
            .tuple => |tuple| self.ast.store.typeAnnoSlice(tuple.annos).len,
            .record => |record| self.ast.store.annoRecordFieldSlice(record.fields).len +
                @intFromBool(record.ext != .closed),
            .apply => |apply| self.ast.store.typeAnnoSlice(apply.args).len - 1,
        };
        if (here.pending + child_count > max_formal_variance_pending) return Unanswered.Unanswered;
        const child = here.child(child_count);

        switch (anno) {
            .ty_var => |v| self.joinFormal(v.tok, formals, here, out),
            .underscore_type_var => |v| self.joinFormal(v.tok, formals, here, out),
            .underscore, .ty, .malformed => {},
            .parens => |parens| try self.accumulateFormalVariances(parens.anno, formals, child, out, variance_walk),
            .@"fn" => |func| {
                for (self.ast.store.typeAnnoSlice(func.args)) |arg| {
                    try self.accumulateFormalVariances(arg, formals, child.withPolarity(.neg), out, variance_walk);
                }
                try self.accumulateFormalVariances(func.ret, formals, child.withPolarity(.pos), out, variance_walk);
            },
            .tag_union => |tag_union| {
                for (self.ast.store.typeAnnoSlice(tag_union.tags)) |tag_idx| {
                    // The checker visits the tag itself before its payloads.
                    try variance_walk.visit();
                    switch (self.ast.store.getTypeAnno(tag_idx)) {
                        .apply => |tag| {
                            const payloads = self.ast.store.typeAnnoSlice(tag.args)[1..];
                            if (child.pending + payloads.len > max_formal_variance_pending) return Unanswered.Unanswered;
                            const payload_position = child.child(payloads.len);
                            for (payloads) |payload| {
                                try self.accumulateFormalVariances(payload, formals, payload_position, out, variance_walk);
                            }
                        },
                        .ty_var, .underscore_type_var, .underscore, .ty, .tag_union, .tuple, .record, .@"fn", .parens, .malformed => {},
                    }
                }
                switch (tag_union.ext) {
                    .named => |named| try self.accumulateFormalVariances(named.anno, formals, child, out, variance_walk),
                    .open => try variance_walk.visit(),
                    .closed => {},
                }
            },
            .tuple => |tuple| for (self.ast.store.typeAnnoSlice(tuple.annos)) |elem| {
                try self.accumulateFormalVariances(elem, formals, child, out, variance_walk);
            },
            .record => |record| {
                for (self.ast.store.annoRecordFieldSlice(record.fields)) |field_idx| {
                    const field = self.ast.store.getAnnoRecordField(field_idx) catch return Unanswered.Unanswered;
                    try self.accumulateFormalVariances(field.ty, formals, child, out, variance_walk);
                }
                switch (record.ext) {
                    .named => |named| try self.accumulateFormalVariances(named.anno, formals, child, out, variance_walk),
                    .open => try variance_walk.visit(),
                    .closed => {},
                }
            },
            .apply => |apply| {
                const all_args = self.ast.store.typeAnnoSlice(apply.args);
                const args = all_args[1..];
                if (args.len > max_tracked_alias_formals) return Unanswered.Unanswered;
                var rules: [max_tracked_alias_formals]ArgRule = undefined;
                try self.applyArgRules(all_args[0], args.len, &rules, variance_walk);
                for (args, rules[0..args.len]) |arg, rule| {
                    var arg_position = child.withPolarity(rule.apply(here.polarity));
                    arg_position.unknown = here.unknown or rule == .opaque_variance;
                    try self.accumulateFormalVariances(arg, formals, arg_position, out, variance_walk);
                }
            },
        }
    }

    fn joinFormal(
        self: *const OpenRows,
        var_tok: Token.Idx,
        formals: []const AST.TypeAnno.Idx,
        here: VariancePosition,
        out: *[max_tracked_alias_formals]Variance,
    ) void {
        const formal_index = self.formalIndex(self.tokenName(var_tok), formals) orelse return;
        const occurrence: Variance = if (here.unknown) .invariant else Variance.ofOccurrence(here.polarity);
        out[formal_index] = out[formal_index].join(occurrence);
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

    /// `Check.applyTryErrorArgIndex`: which argument of an application of
    /// `head` lands in the builtin `Try`'s error row, answered unanimously
    /// over every declaration `head` could name.
    fn tryErrorArgIndex(self: *const OpenRows, head: AST.TypeAnno.Idx, arity: usize, depth: usize) ?usize {
        if (depth == max_try_alias_depth) return null;
        if (arity > max_tracked_alias_formals) return null;
        const found = self.candidates(head);
        if (found.external) return null;

        var answer: ?usize = null;
        if (found.builtin) {
            const name = self.tokenName(self.ast.store.getTypeAnno(head).ty.token);
            if (!std.mem.eql(u8, name, try_type_name) or arity != try_arity) return null;
            answer = try_error_arg_index;
        }
        for (found.locals) |decl_idx| {
            const index = self.aliasTryErrorArgIndex(decl_idx, arity, depth) orelse return null;
            if (answer) |existing| {
                if (existing != index) return null;
            }
            answer = index;
        }
        return answer;
    }

    /// One local declaration's answer to `tryErrorArgIndex`: a transparent
    /// alias passing a formal straight through to a `Try`'s error row, or a
    /// chain of such aliases.
    fn aliasTryErrorArgIndex(self: *const OpenRows, decl_idx: AST.Statement.Idx, arity: usize, depth: usize) ?usize {
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
        const body_args = body_all_args[1..];
        const inner_index = self.tryErrorArgIndex(body_all_args[0], body_args.len, depth + 1) orelse return null;

        // Past a `Try` itself only its error row must be a formal; through an
        // alias over an alias every argument must be passed straight through.
        const body_is_try = self.candidatesAreOnlyBuiltin(body_all_args[0]);
        if (!body_is_try) {
            for (body_args) |body_arg| {
                const name = self.varName(body_arg) orelse return null;
                _ = self.formalIndex(name, formals) orelse return null;
            }
        }
        const name = self.varName(body_args[inner_index]) orelse return null;
        return self.formalIndex(name, formals);
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
