const std = @import("std");
const base = @import("../base.zig");
const parse = @import("parse.zig");
const collections = @import("../collections.zig");
const types = @import("../types/types.zig");

const NodeStore = @import("./canonicalize/NodeStore.zig");
pub const Scope = @import("./canonicalize/Scope.zig");

const AST = parse.AST;

can_ir: *CIR,
parse_ir: *AST,
scope: *Scope,

const Ident = base.Ident;
const Region = base.Region;
const TagName = base.TagName;
const ModuleEnv = base.ModuleEnv;
const CalledVia = base.CalledVia;
const exitOnOom = collections.utils.exitOnOom;

const BUILTIN_NUM_ADD: CIR.Pattern.Idx = @enumFromInt(0);
const BUILTIN_NUM_SUB: CIR.Pattern.Idx = @enumFromInt(1);

pub fn init(self: *CIR, parse_ir: *AST, scope: *Scope) Self {
    const gpa = self.env.gpa;
    const ident_store = &self.env.idents;

    // Simulate the builtins by adding to both the NodeStore and Scope
    // Not sure if this is how we want to do it long term, but want something to
    // make a start on canonicalization.

    const region_zero = Region.zero();

    // BUILTIN_NUM_ADD
    const ident_add = self.env.idents.insert(gpa, base.Ident.for_text("add"), region_zero);
    const pattern_idx_add = self.store.addPattern(CIR.Pattern{ .assign = .{ .ident = ident_add, .region = region_zero } });
    _ = scope.levels.introduce(gpa, ident_store, .ident, ident_add, pattern_idx_add) catch {};
    std.debug.assert(BUILTIN_NUM_ADD == pattern_idx_add);

    // BUILTIN_NUM_SUB
    const ident_sub = self.env.idents.insert(gpa, base.Ident.for_text("sub"), region_zero);
    const pattern_idx_sub = self.store.addPattern(CIR.Pattern{ .assign = .{ .ident = ident_sub, .region = region_zero } });
    _ = scope.levels.introduce(gpa, ident_store, .ident, ident_sub, pattern_idx_sub) catch {};
    std.debug.assert(BUILTIN_NUM_SUB == pattern_idx_sub);

    return .{
        .can_ir = self,
        .parse_ir = parse_ir,
        .scope = scope,
    };
}

const Self = @This();

/// The intermediate representation of a canonicalized Roc program.
pub const CIR = @import("canonicalize/CIR.zig");

/// helper to get the allocator from ModuleEnv
pub fn get_gpa(self: *Self) std.mem.Allocator {
    comptime return self.can_ir.env.gpa;
}

/// After parsing a Roc program, the [ParseIR](src/check/parse/AST.zig) is transformed into a [canonical
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

    // Track the start of scratch defs
    const scratch_defs_start = self.can_ir.store.scratchDefTop();

    for (self.parse_ir.store.statementSlice(file.statements)) |stmt_id| {
        const stmt = self.parse_ir.store.getStatement(stmt_id);
        switch (stmt) {
            .import => |_| {
                const feature = self.can_ir.env.strings.insert(self.can_ir.env.gpa, "top-level import");
                self.can_ir.pushDiagnostic(CIR.Diagnostic{ .not_implemented = .{
                    .feature = feature,
                    .region = Region.zero(),
                } });
            },
            .decl => |decl| {
                const def_idx = self.canonicalize_decl(decl);
                self.can_ir.store.addScratchDef(def_idx);
            },
            .@"var" => {
                // Not valid at top-level
                const string_idx = self.can_ir.env.strings.insert(self.can_ir.env.gpa, "var");
                self.can_ir.pushDiagnostic(CIR.Diagnostic{ .invalid_top_level_statement = .{
                    .stmt = string_idx,
                } });
            },
            .expr => {
                // Not valid at top-level
                const string_idx = self.can_ir.env.strings.insert(self.can_ir.env.gpa, "expr");
                self.can_ir.pushDiagnostic(CIR.Diagnostic{ .invalid_top_level_statement = .{
                    .stmt = string_idx,
                } });
            },
            .crash => {
                // Not valid at top-level
                const string_idx = self.can_ir.env.strings.insert(self.can_ir.env.gpa, "crash");
                self.can_ir.pushDiagnostic(CIR.Diagnostic{ .invalid_top_level_statement = .{
                    .stmt = string_idx,
                } });
            },
            .expect => {
                const feature = self.can_ir.env.strings.insert(self.can_ir.env.gpa, "top-level expect");
                self.can_ir.pushDiagnostic(CIR.Diagnostic{ .not_implemented = .{
                    .feature = feature,
                    .region = Region.zero(),
                } });
            },
            .@"for" => {
                // Not valid at top-level
                const string_idx = self.can_ir.env.strings.insert(self.can_ir.env.gpa, "for");
                self.can_ir.pushDiagnostic(CIR.Diagnostic{ .invalid_top_level_statement = .{
                    .stmt = string_idx,
                } });
            },
            .@"return" => {
                // Not valid at top-level
                const string_idx = self.can_ir.env.strings.insert(self.can_ir.env.gpa, "return");
                self.can_ir.pushDiagnostic(CIR.Diagnostic{ .invalid_top_level_statement = .{
                    .stmt = string_idx,
                } });
            },
            .type_decl => |_| {
                const feature = self.can_ir.env.strings.insert(self.can_ir.env.gpa, "top-level type_decl");
                self.can_ir.pushDiagnostic(CIR.Diagnostic{ .not_implemented = .{
                    .feature = feature,
                    .region = Region.zero(),
                } });
            },
            .type_anno => |_| {
                const feature = self.can_ir.env.strings.insert(self.can_ir.env.gpa, "top-level type_anno");
                self.can_ir.pushDiagnostic(CIR.Diagnostic{ .not_implemented = .{
                    .feature = feature,
                    .region = Region.zero(),
                } });
            },
            .malformed => |malformed| {
                // We won't touch this since it's already a parse error.
                _ = malformed;
            },
        }
    }

    // TODO: implement

    // Get the header and canonicalize exposes based on header type
    const header = self.parse_ir.store.getHeader(file.header);
    switch (header) {
        .module => |h| self.canonicalize_header_exposes(h.exposes),
        .package => |h| self.canonicalize_header_exposes(h.exposes),
        .platform => |h| self.canonicalize_header_exposes(h.exposes),
        .hosted => |h| self.canonicalize_header_exposes(h.exposes),
        .app => {
            // App headers have 'provides' instead of 'exposes'
            // TODO: Handle app provides differently
        },
        .malformed => {
            // Skip malformed headers
        },
    }

    // Create the span of all top-level defs
    self.can_ir.top_level_defs = self.can_ir.store.defSpanFrom(scratch_defs_start);
}

fn canonicalize_header_exposes(
    self: *Self,
    exposes: AST.Collection.Idx,
) void {
    const collection = self.parse_ir.store.getCollection(exposes);
    const exposed_items = self.parse_ir.store.exposedItemSlice(.{ .span = collection.span });

    for (exposed_items) |exposed_idx| {
        const exposed = self.parse_ir.store.getExposedItem(exposed_idx);
        switch (exposed) {
            .lower_ident => |ident| {
                // For now, just mark that we've seen this exposed identifier
                // In a full implementation, we'd check if it's actually defined
                _ = ident;
            },
            .upper_ident => |type_name| {
                // For now, just mark that we've seen this exposed type
                // In a full implementation, we'd check if it's actually defined
                _ = type_name;
            },
            .upper_ident_star => |type_with_constructors| {
                // For now, just mark that we've seen this exposed type with constructors
                // In a full implementation, we'd check if it's actually defined and has constructors
                _ = type_with_constructors;
            },
        }
    }
}

fn bringImportIntoScope(
    self: *Self,
    import: *const AST.Statement,
) void {
    // const gpa = self.can_ir.env.gpa;
    // const import_name: []u8 = &.{}; // import.module_name_tok;
    // const shorthand: []u8 = &.{}; // import.qualifier_tok;
    // const region = Region{
    //     .start = Region.Position.zero(),
    //     .end = Region.Position.zero(),
    // };

    // const res = self.can_ir.imports.getOrInsert(gpa, import_name, shorthand);

    // if (res.was_present) {
    //     _ = self.can_ir.env.problems.append(gpa, Problem.Canonicalize.make(.{ .DuplicateImport = .{
    //         .duplicate_import_region = region,
    //     } }));
    // }

    const exposesSlice = self.parse_ir.store.exposedItemSlice(import.exposes);
    for (exposesSlice) |exposed_idx| {
        const exposed = self.parse_ir.store.getExposedItem(exposed_idx);
        switch (exposed) {
            .lower_ident => |ident| {

                // TODO handle `as` here using an Alias

                if (self.parse_ir.tokens.resolveIdentifier(ident.ident)) |ident_idx| {
                    _ = ident_idx;

                    // TODO Introduce our import

                    // _ = self.scope.levels.introduce(gpa, &self.can_ir.env.idents, .ident, .{ .scope_name = ident_idx, .ident = ident_idx });
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
        }
    }
}

fn bringIngestedFileIntoScope(
    self: *Self,
    import: *const parse.AST.Stmt.Import,
) void {
    const res = self.can_ir.env.modules.getOrInsert(
        import.name,
        import.package_shorthand,
    );

    if (res.was_present) {
        // _ = self.can_ir.env.problems.append(Problem.Canonicalize.make(.DuplicateImport{
        //     .duplicate_import_region = import.name_region,
        // }));
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

fn tokenizedRegionToRegion(self: *Self, ast_region: AST.TokenizedRegion) base.Region {
    const start_region = self.parse_ir.tokens.resolve(ast_region.start);
    const end_region = self.parse_ir.tokens.resolve(ast_region.end);
    return .{
        .start = start_region.start,
        .end = end_region.end,
    };
}

fn canonicalize_decl(
    self: *Self,
    decl: AST.Statement.Decl,
) CIR.Def.Idx {
    const pattern_idx = blk: {
        if (self.canonicalize_pattern(decl.pattern)) |idx| {
            break :blk idx;
        } else {
            const region = self.tokenizedRegionToRegion(decl.region);
            const malformed_idx = self.can_ir.pushMalformed(CIR.Pattern.Idx, CIR.Diagnostic{ .pattern_not_canonicalized = .{
                .region = region,
            } });
            break :blk malformed_idx;
        }
    };

    const expr_idx = blk: {
        if (self.canonicalize_expr(decl.body)) |idx| {
            break :blk idx;
        } else {
            const region = self.tokenizedRegionToRegion(decl.region);
            const malformed_idx = self.can_ir.pushMalformed(CIR.Expr.Idx, CIR.Diagnostic{ .expr_not_canonicalized = .{
                .region = region,
            } });
            break :blk malformed_idx;
        }
    };

    const expr_var = self.can_ir.env.types_store.freshAt(@intFromEnum(expr_idx)) catch |err| exitOnOom(err);

    // Create the def entry
    return self.can_ir.store.addDef(.{
        .pattern = pattern_idx,
        .pattern_region = self.tokenizedRegionToRegion(self.parse_ir.store.getPattern(decl.pattern).to_tokenized_region()),
        .expr = expr_idx,
        .expr_region = self.tokenizedRegionToRegion(self.parse_ir.store.getExpr(decl.body).to_tokenized_region()),
        .expr_var = expr_var,
        .annotation = null,
        .kind = .let,
    });
}

/// Canonicalize an expression.
pub fn canonicalize_expr(
    self: *Self,
    expr_idx: AST.Expr.Idx,
) ?CIR.Expr.Idx {
    const expr = self.parse_ir.store.getExpr(expr_idx);

    // use the next can node idx to create the expr type var
    switch (expr) {
        .apply => |e| {
            // Mark the start of scratch expressions
            const scratch_top = self.can_ir.store.scratchExprTop();

            // Canonicalize the function being called and add as first element
            const fn_expr = self.canonicalize_expr(e.@"fn") orelse {
                self.can_ir.store.clearScratchExprsFrom(scratch_top);
                return null;
            };
            self.can_ir.store.addScratchExpr(fn_expr);

            // Canonicalize and add all arguments
            const args_slice = self.parse_ir.store.exprSlice(e.args);
            for (args_slice) |arg| {
                if (self.canonicalize_expr(arg)) |canonicalized_arg| {
                    self.can_ir.store.addScratchExpr(canonicalized_arg);
                }
            }

            // Create span from scratch expressions
            const args_span = self.can_ir.store.exprSpanFrom(scratch_top);

            return self.can_ir.store.addExpr(CIR.Expr{
                .call = .{
                    .args = args_span,
                    .called_via = CalledVia.apply,
                    .region = self.tokenizedRegionToRegion(e.region),
                },
            });
        },
        .ident => |e| {
            if (self.parse_ir.tokens.resolveIdentifier(e.token)) |ident| {
                if (self.scope.levels.lookup(&self.can_ir.env.idents, .ident, ident)) |pattern_idx| {
                    // We found the ident in scope, lookup to reference the pattern
                    return self.can_ir.store.addExpr(CIR.Expr{ .lookup = .{
                        .pattern_idx = pattern_idx,
                        .region = self.tokenizedRegionToRegion(e.region),
                    } });
                } else {
                    // We did not find the ident in scope
                    const region = self.tokenizedRegionToRegion(e.region);
                    return self.can_ir.pushMalformed(CIR.Expr.Idx, CIR.Diagnostic{ .ident_not_in_scope = .{
                        .ident = ident,
                        .region = region,
                    } });
                }
            } else {
                const feature = self.can_ir.env.strings.insert(self.can_ir.env.gpa, "report an error when unable to resolve identifier");
                return self.can_ir.pushMalformed(CIR.Expr.Idx, CIR.Diagnostic{ .not_implemented = .{
                    .feature = feature,
                    .region = Region.zero(),
                } });
            }
        },
        .int => |e| {
            const region = self.tokenizedRegionToRegion(e.region);

            // resolve to a string slice from the source
            const token_text = self.parse_ir.resolve(e.token);

            // intern the string slice
            const literal = self.can_ir.env.strings.insert(self.can_ir.env.gpa, token_text);

            // parse the integer value
            const value = std.fmt.parseInt(i128, token_text, 10) catch {
                // Invalid number literal
                return self.can_ir.pushMalformed(CIR.Expr.Idx, CIR.Diagnostic{ .invalid_num_literal = .{
                    .literal = literal,
                    .region = region,
                } });
            };

            // create type vars, first "reserve" node slots
            const ret_expr_idx = self.can_ir.store.predictNodeIndex(3);

            // then insert the type vars, setting the parent to be the final slot
            const precision_type_var = self.can_ir.pushFreshTypeVar(ret_expr_idx, region);
            const num_type_var = self.can_ir.pushFreshTypeVar(ret_expr_idx, region);

            // then in the final slot the actual expr is inserted
            return self.can_ir.store.addExpr(CIR.Expr{
                .int = .{
                    .num_var = num_type_var,
                    .precision_var = precision_type_var,
                    .literal = literal,
                    .value = CIR.IntValue{
                        .bytes = @bitCast(value),
                        .kind = .i128,
                    },
                    .bound = types.Num.Int.Precision.fromValue(value),
                    .region = self.tokenizedRegionToRegion(e.region),
                },
            });
        },
        .float => |e| {
            const region = self.tokenizedRegionToRegion(e.region);

            // resolve to a string slice from the source
            const token_text = self.parse_ir.resolve(e.token);

            // intern the string slice
            const literal = self.can_ir.env.strings.insert(self.can_ir.env.gpa, token_text);

            // parse the float value
            const value = std.fmt.parseFloat(f64, token_text) catch {
                // Invalid number literal
                return self.can_ir.pushMalformed(CIR.Expr.Idx, CIR.Diagnostic{ .invalid_num_literal = .{
                    .literal = literal,
                    .region = region,
                } });
            };

            // create type vars, first "reserve" 3 can node slots
            const ret_expr_idx = self.can_ir.store.predictNodeIndex(3);

            // then insert the type vars, setting the parent to be the final slot
            const precision_type_var = self.can_ir.pushFreshTypeVar(ret_expr_idx, region);
            const num_type_var = self.can_ir.pushFreshTypeVar(ret_expr_idx, region);

            // then in the final slot the actual expr is inserted
            return self.can_ir.store.addExpr(CIR.Expr{
                .float = .{
                    .num_var = num_type_var,
                    .precision_var = precision_type_var,
                    .literal = literal,
                    .value = value,
                    .bound = types.Num.Frac.Precision.fromValue(value),
                    .region = self.tokenizedRegionToRegion(e.region),
                },
            });
        },
        .string => |e| {
            // Get all the string parts
            const parts = self.parse_ir.store.exprSlice(e.parts);

            // Extract segments from the string, inserting them into the string interner
            // For non-string interpolation segments, canonicalize them
            //
            // Returns a Expr.Span containing the canonicalized string segments
            // a string may consist of multiple string literal or expression segments
            const str_segments_span = self.extractStringSegments(parts);

            return self.can_ir.store.addExpr(CIR.Expr{ .str = .{
                .span = str_segments_span,
                .region = self.tokenizedRegionToRegion(e.region),
            } });
        },
        .list => |e| {
            // Mark the start of scratch expressions for the list
            const scratch_top = self.can_ir.store.scratchExprTop();

            // Iterate over the list item, canonicalizing each one
            // Then append the result to the scratch list
            const items_slice = self.parse_ir.store.exprSlice(e.items);
            for (items_slice) |item| {
                if (self.canonicalize_expr(item)) |canonicalized| {
                    self.can_ir.store.addScratchExpr(canonicalized);
                }
            }

            // Create span of the new scratch expressions
            const elems_span = self.can_ir.store.exprSpanFrom(scratch_top);

            // create type vars, first "reserve" node slots
            const list_expr_idx = self.can_ir.store.predictNodeIndex(2);

            // then insert the type vars, setting the parent to be the final slot
            const elem_type_var = self.can_ir.pushFreshTypeVar(
                list_expr_idx,
                self.tokenizedRegionToRegion(e.region),
            );

            // then in the final slot the actual expr is inserted
            return self.can_ir.store.addExpr(CIR.Expr{
                .list = .{
                    .elems = elems_span,
                    .elem_var = elem_type_var,
                    .region = self.tokenizedRegionToRegion(e.region),
                },
            });
        },
        .tag => |e| {
            if (self.parse_ir.tokens.resolveIdentifier(e.token)) |tag_name| {
                const region = self.tokenizedRegionToRegion(e.region);

                // create type vars, first "reserve" node slots
                const ret_expr_idx = self.can_ir.store.predictNodeIndex(3);

                // then insert the type vars, setting the parent to be the final slot
                const tag_union_type_var = self.can_ir.pushFreshTypeVar(ret_expr_idx, region);
                const ext_type_var = self.can_ir.pushFreshTypeVar(ret_expr_idx, region);

                // then in the final slot the actual expr is inserted
                return self.can_ir.store.addExpr(CIR.Expr{
                    .tag = .{
                        .tag_union_var = tag_union_type_var,
                        .ext_var = ext_type_var,
                        .name = tag_name,
                        .args = .{ .span = .{ .start = 0, .len = 0 } }, // empty arguments
                        .region = region,
                    },
                });
            } else {
                return null;
            }
        },
        .string_part => |_| {
            const feature = self.can_ir.env.strings.insert(self.can_ir.env.gpa, "canonicalize string_part expression");
            return self.can_ir.pushMalformed(CIR.Expr.Idx, CIR.Diagnostic{ .not_implemented = .{
                .feature = feature,
                .region = Region.zero(),
            } });
        },
        .tuple => |_| {
            const feature = self.can_ir.env.strings.insert(self.can_ir.env.gpa, "canonicalize tuple expression");
            return self.can_ir.pushMalformed(CIR.Expr.Idx, CIR.Diagnostic{ .not_implemented = .{
                .feature = feature,
                .region = Region.zero(),
            } });
        },
        .record => |_| {
            const feature = self.can_ir.env.strings.insert(self.can_ir.env.gpa, "canonicalize record expression");
            return self.can_ir.pushMalformed(CIR.Expr.Idx, CIR.Diagnostic{ .not_implemented = .{
                .feature = feature,
                .region = Region.zero(),
            } });
        },
        .lambda => |e| {

            // args
            // TODO implement canonicalization of args here

            // body
            const body_idx = blk: {
                if (self.canonicalize_expr(e.body)) |idx| {
                    break :blk idx;
                } else {
                    const ast_body = self.parse_ir.store.getExpr(e.body);
                    const region = self.tokenizedRegionToRegion(ast_body.to_tokenized_region());
                    const malformed_idx = self.can_ir.pushMalformed(CIR.Expr.Idx, CIR.Diagnostic{ .lambda_body_not_canonicalized = .{
                        .region = region,
                    } });
                    break :blk malformed_idx;
                }
            };

            // We don't have a Lambda in CIR.Expr ... TODO should we add one?
            _ = body_idx;
            return self.can_ir.pushMalformed(CIR.Expr.Idx, CIR.Diagnostic{ .can_lambda_not_implemented = .{
                .region = self.tokenizedRegionToRegion(e.region),
            } });
        },
        .record_updater => |_| {
            const feature = self.can_ir.env.strings.insert(self.can_ir.env.gpa, "canonicalize record_updater expression");
            return self.can_ir.pushMalformed(CIR.Expr.Idx, CIR.Diagnostic{ .not_implemented = .{
                .feature = feature,
                .region = Region.zero(),
            } });
        },
        .field_access => |_| {
            const feature = self.can_ir.env.strings.insert(self.can_ir.env.gpa, "canonicalize record field_access expression");
            return self.can_ir.pushMalformed(CIR.Expr.Idx, CIR.Diagnostic{ .not_implemented = .{
                .feature = feature,
                .region = Region.zero(),
            } });
        },
        .local_dispatch => |_| {
            const feature = self.can_ir.env.strings.insert(self.can_ir.env.gpa, "canonicalize local_dispatch expression");
            return self.can_ir.pushMalformed(CIR.Expr.Idx, CIR.Diagnostic{ .not_implemented = .{
                .feature = feature,
                .region = Region.zero(),
            } });
        },
        .bin_op => |e| {

            // Canonicalize left and right operands
            const lhs = if (self.canonicalize_expr(e.left)) |left_expr_idx|
                left_expr_idx
            else
                // TODO should probably use LHS region here
                self.can_ir.pushMalformed(CIR.Expr.Idx, CIR.Diagnostic{ .expr_not_canonicalized = .{
                    .region = self.tokenizedRegionToRegion(e.region),
                } });

            const rhs = if (self.canonicalize_expr(e.right)) |right_expr_idx|
                right_expr_idx
            else
                // TODO should probably use RHS region here
                self.can_ir.pushMalformed(CIR.Expr.Idx, CIR.Diagnostic{ .expr_not_canonicalized = .{
                    .region = self.tokenizedRegionToRegion(e.region),
                } });

            // Get the operator token
            const op_token = self.parse_ir.tokens.tokens.get(e.operator);

            const op: CIR.Expr.Binop.Op = switch (op_token.tag) {
                .OpPlus => .add,
                .OpBinaryMinus => .sub,
                .OpStar => .mul,
                else => {
                    // Unknown operator
                    const feature = self.can_ir.env.strings.insert(self.can_ir.env.gpa, "binop");
                    return self.can_ir.pushMalformed(CIR.Expr.Idx, CIR.Diagnostic{ .not_implemented = .{
                        .feature = feature,
                        .region = Region.zero(),
                    } });
                },
            };

            return self.can_ir.store.addExpr(CIR.Expr{
                .binop = CIR.Expr.Binop.init(
                    op,
                    lhs,
                    rhs,
                    self.tokenizedRegionToRegion(e.region),
                ),
            });
        },
        .suffix_single_question => |_| {
            const feature = self.can_ir.env.strings.insert(self.can_ir.env.gpa, "canonicalize suffix_single_question expression");
            return self.can_ir.pushMalformed(CIR.Expr.Idx, CIR.Diagnostic{ .not_implemented = .{
                .feature = feature,
                .region = Region.zero(),
            } });
        },
        .unary_op => |_| {
            const feature = self.can_ir.env.strings.insert(self.can_ir.env.gpa, "canonicalize unary_op expression");
            return self.can_ir.pushMalformed(CIR.Expr.Idx, CIR.Diagnostic{ .not_implemented = .{
                .feature = feature,
                .region = Region.zero(),
            } });
        },
        .if_then_else => |_| {
            const feature = self.can_ir.env.strings.insert(self.can_ir.env.gpa, "canonicalize if_then_else expression");
            return self.can_ir.pushMalformed(CIR.Expr.Idx, CIR.Diagnostic{ .not_implemented = .{
                .feature = feature,
                .region = Region.zero(),
            } });
        },
        .match => |_| {
            const feature = self.can_ir.env.strings.insert(self.can_ir.env.gpa, "canonicalize match expression");
            return self.can_ir.pushMalformed(CIR.Expr.Idx, CIR.Diagnostic{ .not_implemented = .{
                .feature = feature,
                .region = Region.zero(),
            } });
        },
        .dbg => |_| {
            const feature = self.can_ir.env.strings.insert(self.can_ir.env.gpa, "canonicalize dbg expression");
            return self.can_ir.pushMalformed(CIR.Expr.Idx, CIR.Diagnostic{ .not_implemented = .{
                .feature = feature,
                .region = Region.zero(),
            } });
        },
        .record_builder => |_| {
            const feature = self.can_ir.env.strings.insert(self.can_ir.env.gpa, "canonicalize record_builder expression");
            return self.can_ir.pushMalformed(CIR.Expr.Idx, CIR.Diagnostic{ .not_implemented = .{
                .feature = feature,
                .region = Region.zero(),
            } });
        },
        .ellipsis => |_| {
            const feature = self.can_ir.env.strings.insert(self.can_ir.env.gpa, "canonicalize ellipsis expression");
            return self.can_ir.pushMalformed(CIR.Expr.Idx, CIR.Diagnostic{ .not_implemented = .{
                .feature = feature,
                .region = Region.zero(),
            } });
        },
        .block => |_| {
            const feature = self.can_ir.env.strings.insert(self.can_ir.env.gpa, "canonicalize block expression");
            return self.can_ir.pushMalformed(CIR.Expr.Idx, CIR.Diagnostic{ .not_implemented = .{
                .feature = feature,
                .region = Region.zero(),
            } });
        },
        .malformed => |malformed| {
            // We won't touch this since it's already a parse error.
            _ = malformed;
            return null;
        },
    }
}

/// Extract string segments from parsed string parts
fn extractStringSegments(self: *Self, parts: []const AST.Expr.Idx) CIR.Expr.Span {
    const gpa = self.can_ir.env.gpa;
    const start = self.can_ir.store.scratchExprTop();

    for (parts) |part| {
        const part_node = self.parse_ir.store.getExpr(part);
        switch (part_node) {
            .string_part => |sp| {
                // get the raw text of the string part
                const part_text = self.parse_ir.resolve(sp.token);

                // intern the string in the ModuleEnv
                const string_idx = self.can_ir.env.strings.insert(gpa, part_text);

                // create a node for the string literal
                const str_expr_idx = self.can_ir.store.addExpr(CIR.Expr{ .str_segment = .{
                    .literal = string_idx,
                    .region = self.tokenizedRegionToRegion(part_node.to_tokenized_region()),
                } });

                // add the node idx to our scratch expr stack
                self.can_ir.store.addScratchExpr(str_expr_idx);
            },
            else => {

                // Any non-string-part is an interpolation
                if (self.canonicalize_expr(part)) |expr_idx| {
                    // append our interpolated expression
                    self.can_ir.store.addScratchExpr(expr_idx);
                } else {
                    // unable to canonicalize the interpolation, push a malformed node
                    const region = self.tokenizedRegionToRegion(part_node.to_tokenized_region());
                    const malformed_idx = self.can_ir.pushMalformed(CIR.Expr.Idx, CIR.Diagnostic{ .invalid_string_interpolation = .{
                        .region = region,
                    } });
                    self.can_ir.store.addScratchExpr(malformed_idx);
                }
            },
        }
    }

    return self.can_ir.store.exprSpanFrom(start);
}

fn canonicalize_pattern(
    self: *Self,
    pattern_idx: AST.Pattern.Idx,
) ?CIR.Pattern.Idx {
    const gpa = self.can_ir.env.gpa;
    switch (self.parse_ir.store.getPattern(pattern_idx)) {
        .ident => |e| {
            const region = self.tokenizedRegionToRegion(e.region);

            if (self.parse_ir.tokens.resolveIdentifier(e.ident_tok)) |ident_idx| {

                // Push a Pattern node for our identifier
                const assign_idx = self.can_ir.store.addPattern(CIR.Pattern{ .assign = .{
                    .ident = ident_idx,
                    .region = region,
                } });

                // Introduce the identifier into scope mapping to this pattern node
                self.scope.levels.introduce(
                    gpa,
                    &self.can_ir.env.idents,
                    .ident,
                    ident_idx,
                    assign_idx,
                ) catch {
                    return self.can_ir.pushMalformed(CIR.Pattern.Idx, CIR.Diagnostic{ .ident_already_in_scope = .{
                        .ident = ident_idx,
                        .region = region,
                    } });
                };

                return assign_idx;
            } else {
                const feature = self.can_ir.env.strings.insert(self.can_ir.env.gpa, "report an error when unable to resolve identifier");
                self.can_ir.pushDiagnostic(CIR.Diagnostic{ .not_implemented = .{
                    .feature = feature,
                    .region = Region.zero(),
                } });
            }
        },
        .underscore => |p| {
            const underscore_pattern = CIR.Pattern{
                .underscore = .{
                    .region = self.tokenizedRegionToRegion(p.region),
                },
            };

            return self.can_ir.store.addPattern(underscore_pattern);
        },
        .number => |e| {
            const region = self.tokenizedRegionToRegion(e.region);

            // resolve to a string slice from the source
            const token_text = self.parse_ir.resolve(e.number_tok);

            // intern the string slice
            const literal = self.can_ir.env.strings.insert(gpa, token_text);

            // parse the integer value
            const value = std.fmt.parseInt(i128, token_text, 10) catch {
                // Invalid num literal
                return self.can_ir.pushMalformed(CIR.Pattern.Idx, CIR.Diagnostic{ .invalid_num_literal = .{
                    .literal = literal,
                    .region = region,
                } });
            };

            // create type vars, first "reserve" node slots
            const ret_expr_idx = self.can_ir.store.predictNodeIndex(2);

            // then insert the type vars, setting the parent to be the final slot
            const num_type_var = self.can_ir.pushFreshTypeVar(ret_expr_idx, region);

            // then in the final slot the actual expr is inserted
            const num_pattern = CIR.Pattern{
                .num_literal = .{
                    .num_var = num_type_var,
                    .literal = literal,
                    .value = CIR.IntValue{
                        .bytes = @bitCast(value),
                        .kind = .i128,
                    },
                    .bound = types.Num.Int.Precision.fromValue(value),
                    .region = region,
                },
            };

            return self.can_ir.store.addPattern(num_pattern);
        },
        .string => |e| {
            // resolve to a string slice from the source
            const token_text = self.parse_ir.resolve(e.string_tok);

            // TODO: Handle escape sequences
            // For now, just intern the raw string
            const literal = self.can_ir.env.strings.insert(gpa, token_text);

            const str_pattern = CIR.Pattern{
                .str_literal = .{
                    .literal = literal,
                    .region = self.tokenizedRegionToRegion(e.region),
                },
            };

            return self.can_ir.store.addPattern(str_pattern);
        },
        .tag => |e| {
            if (self.parse_ir.tokens.resolveIdentifier(e.tag_tok)) |tag_name| {
                const start = self.can_ir.store.scratch_patterns.top();

                for (self.parse_ir.store.patternSlice(e.args)) |ast_pattern_idx| {
                    if (self.canonicalize_pattern(ast_pattern_idx)) |idx| {
                        self.can_ir.store.scratch_patterns.append(gpa, idx);
                    } else {
                        const arg = self.parse_ir.store.getPattern(ast_pattern_idx);
                        const region = self.tokenizedRegionToRegion(arg.to_tokenized_region());
                        const malformed_idx = self.can_ir.pushMalformed(CIR.Pattern.Idx, CIR.Diagnostic{ .pattern_arg_invalid = .{
                            .region = region,
                        } });

                        self.can_ir.store.scratch_patterns.append(gpa, malformed_idx);
                    }
                }

                const region = self.tokenizedRegionToRegion(e.region);

                const args = self.can_ir.store.patternSpanFrom(start);

                // create type vars, first "reserve" node slots
                const ret_expr_idx = self.can_ir.store.predictNodeIndex(2);

                // then insert the type vars, setting the parent to be the final slot
                const tag_union_type_var = self.can_ir.pushFreshTypeVar(ret_expr_idx, region);
                const ext_type_var = self.can_ir.pushFreshTypeVar(ret_expr_idx, region);

                // then in the final slot the actual expr is inserted
                const tag_pattern = CIR.Pattern{
                    .applied_tag = .{
                        .whole_var = tag_union_type_var,
                        .ext_var = ext_type_var,
                        .tag_name = tag_name,
                        .arguments = args,
                        .region = region,
                    },
                };

                return self.can_ir.store.addPattern(tag_pattern);
            }
            return null;
        },
        .record => |_| {
            const feature = self.can_ir.env.strings.insert(self.can_ir.env.gpa, "canonicalize record pattern");
            self.can_ir.pushDiagnostic(CIR.Diagnostic{ .not_implemented = .{
                .feature = feature,
                .region = Region.zero(),
            } });
        },
        .tuple => |_| {
            const feature = self.can_ir.env.strings.insert(self.can_ir.env.gpa, "canonicalize tuple pattern");
            self.can_ir.pushDiagnostic(CIR.Diagnostic{ .not_implemented = .{
                .feature = feature,
                .region = Region.zero(),
            } });
        },
        .list => |_| {
            const feature = self.can_ir.env.strings.insert(self.can_ir.env.gpa, "canonicalize list pattern");
            self.can_ir.pushDiagnostic(CIR.Diagnostic{ .not_implemented = .{
                .feature = feature,
                .region = Region.zero(),
            } });
        },
        .list_rest => |_| {
            const feature = self.can_ir.env.strings.insert(self.can_ir.env.gpa, "canonicalize list rest pattern");
            self.can_ir.pushDiagnostic(CIR.Diagnostic{ .not_implemented = .{
                .feature = feature,
                .region = Region.zero(),
            } });
        },
        .alternatives => |_| {
            const feature = self.can_ir.env.strings.insert(self.can_ir.env.gpa, "canonicalize alternatives pattern");
            self.can_ir.pushDiagnostic(CIR.Diagnostic{ .not_implemented = .{
                .feature = feature,
                .region = Region.zero(),
            } });
        },
        .malformed => |malformed| {
            // We won't touch this since it's already a parse error.
            _ = malformed;
        },
    }
    return null;
}

/// Introduce a new identifier to the current scope, return an
/// index if
pub fn scope_introduce_ident(
    self: Self,
    ident_idx: Ident.Idx,
    pattern_idx: CIR.Pattern.Idx,
    region: Region,
    comptime T: type,
) T {
    const gpa = self.get_gpa();
    self.scope.levels.introduce(
        gpa,
        &self.can_ir.env.idents,
        .ident,
        ident_idx,
        pattern_idx,
    ) catch {
        return self.can_ir.pushMalformed(CIR.Pattern.Idx, CIR.Diagnostic{ .ident_already_in_scope = .{
            .ident = ident_idx,
            .region = region,
        } });
    };
}
