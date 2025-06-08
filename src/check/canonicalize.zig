const std = @import("std");
const base = @import("../base.zig");
const parse = @import("parse.zig");
const problem = @import("../problem.zig");
const collections = @import("../collections.zig");

const Scope = @import("./canonicalize/Scope.zig");
const Alias = @import("./canonicalize/Alias.zig");

can_ir: *IR,
parse_ir: *parse.IR,
scope: *Scope,

const Ident = base.Ident;
const Region = base.Region;
const TagName = base.TagName;
const ModuleEnv = base.ModuleEnv;
const Problem = problem.Problem;
const exitOnOom = collections.utils.exitOnOom;

pub fn init(can_ir: *IR, parse_ir: *parse.IR, scope: *Scope) Self {
    return .{
        .can_ir = can_ir,
        .parse_ir = parse_ir,
        .scope = scope,
    };
}

const Self = @This();

/// The intermediate representation of a canonicalized Roc program.
pub const IR = @import("canonicalize/IR.zig");

/// After parsing a Roc program, the [ParseIR](src/check/parse/ir.zig) is transformed into a [canonical
/// form](src/check/canonicalize/ir.zig) called CanIR.
///
/// Canonicalization performs analysis to catch user errors, and sets up the state necessary to solve the types in a
/// program. Among other things, canonicalization;
/// - Uniquely identifies names (think variable and function names). Along the way,
///     canonicalization builds a graph of all variables' references, and catches
///     unused definitions, undefined definitions, and shadowed definitions.
/// - Resolves type signatures, including aliases, into a form suitable for type
///     solving.
/// - Determines the order definitions are used in, if they are defined
///     out-of-order.
/// - Eliminates syntax sugar (for example, renaming `+` to the function call `add`).
///
/// The canonicalization occurs on a single module (file) in isolation. This allows for this work to be easily parallelized and also cached. So where the source code for a module has not changed, the CanIR can simply be loaded from disk and used immediately.
pub fn canonicalize_file(
    self: *Self,
) void {
    const file = self.parse_ir.store.getFile();

    // canonicalize_header_packages();

    for (self.parse_ir.store.statementSlice(file.statements)) |stmt_id| {
        const stmt = self.parse_ir.store.getStatement(stmt_id);
        switch (stmt) {
            .import => |import| {
                self.bringImportIntoScope(&import);
            },
            .decl => |decl| {
                self.canonicalize_decl(decl);
            },
            .@"var" => |v| {
                // Not valid at top-level
                _ = self.can_ir.env.problems.append(self.can_ir.env.gpa, Problem.Canonicalize.make(.{ .InvalidTopLevelStatement = .{
                    .region = v.region.toBase(),
                    .ty = .@"var",
                } }));
            },
            .expr => |expr| {
                // Not valid at top-level
                _ = self.can_ir.env.problems.append(self.can_ir.env.gpa, Problem.Canonicalize.make(.{ .InvalidTopLevelStatement = .{
                    .region = expr.region.toBase(),
                    .ty = .expr,
                } }));
            },
            .crash => |crash| {
                // Not valid at top-level
                _ = self.can_ir.env.problems.append(self.can_ir.env.gpa, Problem.Canonicalize.make(.{ .InvalidTopLevelStatement = .{
                    .region = crash.region.toBase(),
                    .ty = .crash,
                } }));
            },
            .expect => |expect| {
                _ = expect;
            },
            .@"for" => |f| {
                // Not valid at top-level
                _ = self.can_ir.env.problems.append(self.can_ir.env.gpa, Problem.Canonicalize.make(.{ .InvalidTopLevelStatement = .{
                    .region = f.region.toBase(),
                    .ty = .@"for",
                } }));
            },
            .@"return" => |return_stmt| {
                // Not valid at top-level
                _ = self.can_ir.env.problems.append(self.can_ir.env.gpa, Problem.Canonicalize.make(.{ .InvalidTopLevelStatement = .{
                    .region = return_stmt.region.toBase(),
                    .ty = .@"return",
                } }));
            },
            .type_decl => |type_decl| {
                _ = type_decl;
            },
            .type_anno => |type_anno| {
                _ = type_anno;
            },
            .malformed => |malformed| {
                // We won't touch this since it's already a parse error.
                _ = malformed;
            },
        }
    }

    // TODO: implement

    // canonicalize_header_exposes();
}

fn bringImportIntoScope(
    self: *Self,
    import: *const parse.IR.NodeStore.Statement.Import,
) void {
    // const import_name: []u8 = &.{}; // import.module_name_tok;
    // const shorthand: []u8 = &.{}; // import.qualifier_tok;
    // const region = Region{
    //     .start = Region.Position.zero(),
    //     .end = Region.Position.zero(),
    // };

    // const res = self.can_ir.imports.getOrInsert(self.can_ir.env.gpa, import_name, shorthand);

    // if (res.was_present) {
    //     _ = self.can_ir.env.problems.append(self.can_ir.env.gpa, Problem.Canonicalize.make(.{ .DuplicateImport = .{
    //         .duplicate_import_region = region,
    //     } }));
    // }

    const exposesSlice = self.parse_ir.store.exposedItemSlice(import.exposes);
    for (exposesSlice) |exposed_idx| {
        const exposed = self.parse_ir.store.getExposedItem(exposed_idx);
        switch (exposed) {
            .lower_ident => |ident| {
                if (self.parse_ir.tokens.resolveIdentifier(ident.ident)) |ident_idx| {
                    if (ident.as) |as_| {
                        if (self.parse_ir.tokens.resolveIdentifier(as_)) |alias_idx| {
                            _ = self.scope.levels.introduce(.alias, .{ .scope_name = ident_idx, .alias = alias_idx });
                        }
                    } else {
                        _ = self.scope.levels.introduce(.ident, .{ .scope_name = ident_idx, .ident = ident_idx });
                    }
                }
            },
            .upper_ident => |imported_type| {
                _ = imported_type;
                // const alias = Alias{
                //     .name = imported_type.name,
                //     .region = ir.env.tag_names.getRegion(imported_type.name),
                //     .is_builtin = false,
                //     .kind = .ImportedUnknown,
                // };
                // const alias_idx = ir.aliases.append(alias);
                //
                // _ = scope.levels.introduce(.alias, .{
                //     .scope_name = imported_type.name,
                //     .alias = alias_idx,
                // });
            },
            .upper_ident_star => |ident| {
                _ = ident;
            },
            // .CustomTagUnion => |custom| {
            //     const alias = Alias{
            //         .name = custom.name,
            //         .region = ir.env.tag_names.getRegion(custom.name),
            //         .is_builtin = false,
            //         .kind = .ImportedCustomUnion,
            //     };
            //     const alias_idx = ir.aliases.append(alias);
            //
            //     _ = scope.levels.introduce(.alias, .{
            //         .scope_name = custom.name,
            //         .alias = alias_idx,
            //     });
            //     _ = scope.custom_tags.fetchPutAssumeCapacity(custom.name, alias_idx);
            //     // TODO: add to scope.custom_tags
            // },
        }
    }
}

fn bringIngestedFileIntoScope(
    self: *Self,
    import: *const parse.IR.Stmt.Import,
) void {
    const res = self.can_ir.env.modules.getOrInsert(
        import.name,
        import.package_shorthand,
    );

    if (res.was_present) {
        _ = self.can_ir.env.problems.append(Problem.Canonicalize.make(.DuplicateImport{
            .duplicate_import_region = import.name_region,
        }));
    }

    // scope.introduce(self: *Scope, comptime item_kind: Level.ItemKind, ident: Ident.Idx)

    for (import.exposing.items.items) |exposed| {
        const exposed_ident = switch (exposed) {
            .Value => |ident| ident,
            .Type => |ident| ident,
            .CustomTagUnion => |custom| custom.name,
        };
        self.can_ir.env.addExposedIdentForModule(exposed_ident, res.module_idx);
        self.scope.introduce(exposed);
    }
}

const PendingValueDef = union(enum) {
    /// A standalone annotation with no body
    AnnotationOnly: struct {
        pattern: IR.PatternAtRegion.Idx,
        type: IR.Annotation.Idx,
    },
    /// A body with no type annotation
    Body: struct {
        pattern: IR.PatternAtRegion.Idx,
        expr: IR.ExprAtRegion.Idx,
    },
    /// A body with a type annotation
    TypedBody: struct {
        // &'a Loc<ast::Pattern<'a>>,
        // Loc<Pattern>,
        // &'a Loc<ast::TypeAnnotation<'a>>,
        // &'a Loc<ast::Expr<'a>>,
    },
    /// A standalone statement
    Stmt: IR.ExprAtRegion.Idx,

    pub const List = collections.SafeList(@This());
};

fn canonicalize_decl(
    self: *Self,
    decl: parse.IR.NodeStore.Statement.Decl,
) void {
    _ = self.canonicalize_expr(decl.body);
    self.canonicalize_pattern(decl.pattern);
}

fn canonicalize_expr(
    self: *Self,
    expr_idx: parse.IR.NodeStore.ExprIdx,
) ?IR.Expr.Idx {
    const expr = self.parse_ir.store.getExpr(expr_idx);
    switch (expr) {
        .apply => |e| {
            _ = self.canonicalize_expr(e.@"fn");
            const args_slice = self.parse_ir.store.exprSlice(e.args);
            for (args_slice) |arg| {
                _ = self.canonicalize_expr(arg);
            }
        },
        .ident => |e| {
            if (self.parse_ir.tokens.resolveIdentifier(e.token)) |ident| {
                switch (self.scope.levels.lookup(.ident, ident)) {
                    .InScope => {
                        return self.can_ir.store.addExpr(.{
                            .region = e.region.toBase(),
                            .expr = .{ .lookup = .{
                                .ident = ident,
                            } },
                        });
                    },
                    .NotInScope => {},
                    .NotPresent => {},
                }
            }
        },
        else => {},
    }
    return null;
}

fn canonicalize_pattern(
    self: *Self,
    pattern_idx: parse.IR.NodeStore.PatternIdx,
) void {
    _ = self;
    _ = pattern_idx;
}

test {
    try test_can_expr("foo", &.{"foo"}, &.{});
    try test_can_expr("bar", &.{"foo"}, &.{"Ident not in scope"});
}

fn test_can_expr(source: []const u8, idents: []const []const u8, error_messages: []const []const u8) !void {
    const gpa = std.testing.allocator;

    // Setup environment
    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    // Parse source
    var parse_ir = parse.parseExpr(&module_env, source);
    defer parse_ir.deinit();

    // Expect NIL parser errors
    try std.testing.expectEqualSlices(parse.IR.Diagnostic, &.{}, parse_ir.errors);

    // Initialize Can IR
    var can_ir = IR.initCapacity(module_env, parse_ir.store.nodes.items.len);
    defer can_ir.deinit();

    var scope = Scope.init(&can_ir.env, &.{}, &.{});
    defer scope.deinit();
    var can = Self.init(&can_ir, &parse_ir, &scope);

    for (idents) |ident| {
        const id = can.can_ir.env.idents.insert(std.testing.allocator, Ident.for_text(ident), .{ .start = .{ .offset = 0 }, .end = .{ .offset = 0 } });
        _ = can.scope.levels.introduce(.ident, .{ .ident = id, .scope_name = id });
    }

    _ = can.canonicalize_expr(.{ .id = parse_ir.root_node_idx });

    try std.testing.expectEqual(error_messages.len, can.can_ir.env.problems.items.items.len);

    for (0..error_messages.len) |i| {
        var buf = std.ArrayListUnmanaged(u8){};
        var writer = buf.writer(std.testing.allocator);
        defer buf.deinit(std.testing.allocator);
        try can_ir.env.problems.items.items[i].toStr(std.testing.allocator, source, &writer);
        try std.testing.expectEqualStrings(error_messages[i], buf.items);
    }
}
