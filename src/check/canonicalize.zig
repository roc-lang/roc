const std = @import("std");
const base = @import("../base.zig");
const parse = @import("parse.zig");
const collections = @import("../collections.zig");
const types = @import("../types/types.zig");

const NodeStore = @import("./canonicalize/NodeStore.zig");
const Scope = @import("./canonicalize/Scope.zig");

const AST = parse.AST;

can_ir: *CIR,
parse_ir: *AST,
scopes: std.ArrayListUnmanaged(Scope) = .{},
/// Stack of function regions for tracking var reassignment across function boundaries
function_regions: std.ArrayListUnmanaged(Region),
/// Maps var patterns to the function region they were declared in
var_function_regions: std.AutoHashMapUnmanaged(CIR.Pattern.Idx, Region),
/// Set of pattern indices that are vars
var_patterns: std.AutoHashMapUnmanaged(CIR.Pattern.Idx, void),

const Ident = base.Ident;
const Region = base.Region;
const TagName = base.TagName;
const ModuleEnv = base.ModuleEnv;
const CalledVia = base.CalledVia;
const exitOnOom = collections.utils.exitOnOom;

const Content = types.Content;
const FlatType = types.FlatType;
const Num = types.Num;
const TagUnion = types.TagUnion;
const Tag = types.Tag;

const BUILTIN_BOOL: CIR.Pattern.Idx = @enumFromInt(0);
const BUILTIN_BOX: CIR.Pattern.Idx = @enumFromInt(1);
const BUILTIN_DECODE: CIR.Pattern.Idx = @enumFromInt(2);
const BUILTIN_DICT: CIR.Pattern.Idx = @enumFromInt(3);
const BUILTIN_ENCODE: CIR.Pattern.Idx = @enumFromInt(4);
const BUILTIN_HASH: CIR.Pattern.Idx = @enumFromInt(5);
const BUILTIN_INSPECT: CIR.Pattern.Idx = @enumFromInt(6);
const BUILTIN_LIST: CIR.Pattern.Idx = @enumFromInt(7);
const BUILTIN_NUM: CIR.Pattern.Idx = @enumFromInt(8);
const BUILTIN_RESULT: CIR.Pattern.Idx = @enumFromInt(9);
const BUILTIN_SET: CIR.Pattern.Idx = @enumFromInt(10);
const BUILTIN_STR: CIR.Pattern.Idx = @enumFromInt(11);

/// Deinitialize canonicalizer resources
pub fn deinit(
    self: *Self,
) void {
    const gpa = self.can_ir.env.gpa;

    // First deinit individual scopes
    for (0..self.scopes.items.len) |i| {
        var scope = &self.scopes.items[i];
        scope.deinit(gpa);
    }

    // Then deinit the collections
    self.scopes.deinit(gpa);
    self.function_regions.deinit(gpa);
    self.var_function_regions.deinit(gpa);
    self.var_patterns.deinit(gpa);
}

pub fn init(self: *CIR, parse_ir: *AST) Self {
    const gpa = self.env.gpa;

    // Create the canonicalizer with scopes
    var result = Self{
        .can_ir = self,
        .parse_ir = parse_ir,
        .scopes = .{},
        .function_regions = std.ArrayListUnmanaged(Region){},
        .var_function_regions = std.AutoHashMapUnmanaged(CIR.Pattern.Idx, Region){},
        .var_patterns = std.AutoHashMapUnmanaged(CIR.Pattern.Idx, void){},
    };

    // Top-level scope is not a function boundary
    result.scopeEnter(gpa, false);

    // Simulate the builtins by adding to both the NodeStore and Scopes
    // Not sure if this is how we want to do it long term, but want something to
    // make a start on canonicalization.

    result.addBuiltin(self, "Bool", BUILTIN_BOOL);
    result.addBuiltin(self, "Box", BUILTIN_BOX);
    result.addBuiltin(self, "Decode", BUILTIN_DECODE);
    result.addBuiltin(self, "Dict", BUILTIN_DICT);
    result.addBuiltin(self, "Encode", BUILTIN_ENCODE);
    result.addBuiltin(self, "Hash", BUILTIN_HASH);
    result.addBuiltin(self, "Inspect", BUILTIN_INSPECT);
    result.addBuiltin(self, "List", BUILTIN_LIST);
    result.addBuiltin(self, "Num", BUILTIN_NUM);
    result.addBuiltin(self, "Result", BUILTIN_RESULT);
    result.addBuiltin(self, "Set", BUILTIN_SET);
    result.addBuiltin(self, "Str", BUILTIN_STR);

    return result;
}

fn addBuiltin(self: *Self, ir: *CIR, ident_text: []const u8, idx: CIR.Pattern.Idx) void {
    const gpa = ir.env.gpa;
    const ident_store = &ir.env.idents;
    const ident_add = ir.env.idents.insert(gpa, base.Ident.for_text(ident_text), Region.zero());
    const pattern_idx_add = ir.store.addPattern(CIR.Pattern{ .assign = .{ .ident = ident_add, .region = Region.zero() } });
    _ = self.scopeIntroduceInternal(gpa, ident_store, .ident, ident_add, pattern_idx_add, false, true);
    std.debug.assert(idx == pattern_idx_add);
}

const Self = @This();

/// The intermediate representation of a canonicalized Roc program.
pub const CIR = @import("canonicalize/CIR.zig");

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
    self.can_ir.all_defs = self.can_ir.store.defSpanFrom(scratch_defs_start);
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
                // TODO -- do we need a Pattern for "exposed_lower" identifiers?
                _ = ident;
            },
            .upper_ident => |type_name| {
                // TODO -- do we need a Pattern for "exposed_upper" identifiers?
                _ = type_name;
            },
            .upper_ident_star => |type_with_constructors| {
                // TODO -- do we need a Pattern for "exposed_upper_star" identifiers?
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
        // TODO: Implement scope introduction for exposed identifiers
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
    const pattern_region = self.tokenizedRegionToRegion(self.parse_ir.store.getPattern(decl.pattern).to_tokenized_region());
    const expr_region = self.tokenizedRegionToRegion(self.parse_ir.store.getExpr(decl.body).to_tokenized_region());

    const pattern_idx = blk: {
        if (self.canonicalize_pattern(decl.pattern)) |idx| {
            break :blk idx;
        } else {
            const malformed_idx = self.can_ir.pushMalformed(CIR.Pattern.Idx, CIR.Diagnostic{ .pattern_not_canonicalized = .{
                .region = pattern_region,
            } });
            break :blk malformed_idx;
        }
    };

    const expr_idx = blk: {
        if (self.canonicalize_expr(decl.body)) |idx| {
            break :blk idx;
        } else {
            const malformed_idx = self.can_ir.pushMalformed(CIR.Expr.Idx, CIR.Diagnostic{ .expr_not_canonicalized = .{
                .region = expr_region,
            } });
            break :blk malformed_idx;
        }
    };

    // Create the def entry
    return self.can_ir.store.addDef(.{
        .pattern = pattern_idx,
        .pattern_region = pattern_region,
        .expr = expr_idx,
        .expr_region = expr_region,
        .annotation = null,
        .kind = .let,
    });
}

/// Canonicalize an expression.
pub fn canonicalize_expr(
    self: *Self,
    ast_expr_idx: AST.Expr.Idx,
) ?CIR.Expr.Idx {
    const expr = self.parse_ir.store.getExpr(ast_expr_idx);

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
                if (self.canonicalize_expr(arg)) |canonicalized_arg_expr_idx| {
                    self.can_ir.store.addScratchExpr(canonicalized_arg_expr_idx);
                }
            }

            // Create span from scratch expressions
            const args_span = self.can_ir.store.exprSpanFrom(scratch_top);

            const expr_idx = self.can_ir.store.addExpr(CIR.Expr{
                .call = .{
                    .args = args_span,
                    .called_via = CalledVia.apply,
                    .region = self.tokenizedRegionToRegion(e.region),
                },
            });

            // Insert flex type variable
            _ = self.can_ir.setTypeVarAtExpr(expr_idx, Content{ .flex_var = null });

            return expr_idx;
        },
        .ident => |e| {
            const region = self.tokenizedRegionToRegion(e.region);
            if (self.parse_ir.tokens.resolveIdentifier(e.token)) |ident| {
                switch (self.scopeLookup(&self.can_ir.env.idents, .ident, ident)) {
                    .found => |pattern_idx| {
                        // We found the ident in scope, lookup to reference the pattern
                        const expr_idx =
                            self.can_ir.store.addExpr(CIR.Expr{ .lookup = .{
                                .pattern_idx = pattern_idx,
                                .region = region,
                            } });
                        _ = self.can_ir.setTypeVarAtExpr(expr_idx, Content{ .flex_var = null });
                        return expr_idx;
                    },
                    .not_found => {
                        // We did not find the ident in scope
                        return self.can_ir.pushMalformed(CIR.Expr.Idx, CIR.Diagnostic{ .ident_not_in_scope = .{
                            .ident = ident,
                            .region = region,
                        } });
                    },
                }
            } else {
                const feature = self.can_ir.env.strings.insert(self.can_ir.env.gpa, "report an error when unable to resolve identifier");
                return self.can_ir.pushMalformed(CIR.Expr.Idx, CIR.Diagnostic{ .not_implemented = .{
                    .feature = feature,
                    .region = region,
                } });
            }
        },
        .int => |e| {
            const region = self.tokenizedRegionToRegion(e.region);

            // Resolve to a string slice from the source
            const token_text = self.parse_ir.resolve(e.token);

            // Parse the integer value
            const is_negated = token_text[0] == '-'; // Drop the negation for now, so all valid literals fit in u128
            const after_minus_sign = @as(usize, @intFromBool(is_negated));

            // The index the first *actual* digit (after minus sign, "0x" prefix, etc.) in the token
            var first_digit: usize = undefined;

            // Default to base-10
            var int_base: u8 = 10;

            // If this begins with "0x" or "0b" or "Oo" then it's not base-10.
            // We don't bother storing this info anywhere else besides token text,
            // because we already have to look at the whole token to parse the digits
            // into a number, so it will be in cache. It's also trivial to parse.
            if (token_text[after_minus_sign] == '0' and token_text.len > after_minus_sign + 2) {
                switch (token_text[after_minus_sign + 1]) {
                    'x', 'X' => {
                        int_base = 16;
                        first_digit = after_minus_sign + 2;
                    },
                    'o', 'O' => {
                        int_base = 8;
                        first_digit = after_minus_sign + 2;
                    },
                    'b', 'B' => {
                        int_base = 2;
                        first_digit = after_minus_sign + 2;
                    },
                    else => {
                        first_digit = after_minus_sign;
                    },
                }
            } else {
                first_digit = after_minus_sign;
            }

            const u128_val: u128 = std.fmt.parseInt(u128, token_text[first_digit..], int_base) catch {
                // Any number literal that is too large for u128 is invalid, regardless of whether it had a minus sign!
                const expr_idx = self.can_ir.pushMalformed(CIR.Expr.Idx, CIR.Diagnostic{ .invalid_num_literal = .{
                    .region = region,
                } });
                return expr_idx;
            };

            // If this had a minus sign, but negating it would result in a negative number
            // that would be too low to fit in i128, then this int literal is also invalid.
            if (is_negated and u128_val > min_i128_negated) {
                const expr_idx = self.can_ir.pushMalformed(CIR.Expr.Idx, CIR.Diagnostic{ .invalid_num_literal = .{
                    .region = region,
                } });
                return expr_idx;
            }

            // Now we've confirmed that our int literal is one of these:
            // * A signed integer that fits in i128
            // * An unsigned integer that fits in u128
            //
            // We'll happily bitcast a u128 to i128 for storage (and bitcast it back later
            // using its type information), but for negative numbers, we do need to actually
            // negate them (branchlessly) if we skipped its minus sign earlier.
            //
            // This operation should never overflow i128, because we already would have errored out
            // if the u128 portion was bigger than the lowest i128 without a minus sign.
            // Special case: exactly i128 min already has the correct bit pattern when bitcast from u128,
            // so if we try to negate it we'll get an overflow. We specifically *don't* negate that one.
            const sign: i128 = (@as(i128, @intFromBool(!is_negated or u128_val == min_i128_negated)) << 1) - 1;
            const i128_val: i128 = sign * @as(i128, @bitCast(u128_val));

            // create type vars, first "reserve" node slots
            const final_expr_idx = self.can_ir.store.predictNodeIndex(2);

            // Calculate requirements based on the value
            const requirements = blk: {
                ///////////////////////////////////////////////////////////////////////////////////////
                // TODO revise all this, or at least move it into a method on types.Num.Int.BitsNeeded,
                // such that it just takes the input and returns a thing. also use constants, not magic numbers.
                // oh and use counting zeros if possible? leading zeros I think?
                ///////////////////////////////////////////////////////////////////////////////////////
                const sign_needed = is_negated;
                const bits_needed: types.Num.Int.BitsNeeded = if (u128_val <= 127) .@"7" else if (u128_val <= 255) .@"8" else if (u128_val <= 32767) .@"9_to_15" else if (u128_val <= 65535) .@"16" else if (u128_val <= 2147483647) .@"17_to_31" else if (u128_val <= 4294967295) .@"32" else if (u128_val <= 9223372036854775807) .@"33_to_63" else if (u128_val <= 18446744073709551615) .@"64" else if (u128_val <= 170141183460469231731687303715884105727) .@"65_to_127" else .@"128";

                break :blk types.Num.Int.Requirements{
                    .sign_needed = sign_needed,
                    .bits_needed = bits_needed,
                };
            };

            // Create a polymorphic int type variable
            const int_type_var = self.can_ir.pushTypeVar(
                //////////////////////////////////////////////////////////////
                // TODO why is this enumFromInt(0)?
                //////////////////////////////////////////////////////////////
                Content{ .structure = .{ .num = .{ .int_poly = @enumFromInt(0) } } },
                final_expr_idx,
                region,
            );

            // then in the final slot the actual expr is inserted
            const expr_idx = self.can_ir.store.addExpr(CIR.Expr{
                .int = .{
                    .int_var = int_type_var,
                    .requirements = requirements,
                    .value = CIR.IntLiteralValue{ .value = i128_val },
                    .region = region,
                },
            });

            std.debug.assert(@intFromEnum(expr_idx) == @intFromEnum(final_expr_idx));

            // Insert concrete type variable
            _ = self.can_ir.setTypeVarAtExpr(
                expr_idx,
                Content{ .structure = .{ .num = .{ .num_poly = int_type_var } } },
            );

            return expr_idx;
        },
        .frac => |e| {
            const region = self.tokenizedRegionToRegion(e.region);

            // resolve to a string slice from the source
            const token_text = self.parse_ir.resolve(e.token);

            // First, try to parse as RocDec
            const RocDec = @import("../builtins/dec.zig").RocDec;

            var fits_in_f32 = false;
            var fits_in_dec = false;
            var frac_value: base.FracLiteral = undefined;

            if (RocDec.fromNonemptySlice(token_text)) |dec| {
                // Successfully parsed as Dec
                fits_in_dec = true;
                frac_value = .{ .Dec = @as(u128, @bitCast(dec.num)) };

                // Also check if it fits in f32
                const f64_val = dec.toF64();

                // Check if it fits in f32 without precision loss
                if (f64_val >= -std.math.floatMax(f32) and f64_val <= std.math.floatMax(f32)) {
                    const as_f32 = @as(f32, @floatCast(f64_val));
                    const back_to_f64 = @as(f64, @floatCast(as_f32));
                    fits_in_f32 = f64_val == back_to_f64;
                }
            } else {
                // Parsing it as a Dec failed, so try F64 next.
                const f64_value = std.fmt.parseFloat(f64, token_text) catch {
                    // It doesn't parse as Dec or as F64, so it's too big to fit in any of Roc's numeric types. Error out!
                    const expr_idx = self.can_ir.pushMalformed(CIR.Expr.Idx, CIR.Diagnostic{ .invalid_num_literal = .{
                        .region = region,
                    } });
                    return expr_idx;
                };

                frac_value = .{ .F64 = f64_value };

                // Check if it fits in F32 without precision loss
                if (f64_value >= -std.math.floatMax(f32) and f64_value <= std.math.floatMax(f32)) {
                    const as_f32 = @as(f32, @floatCast(f64_value));
                    const back_to_f64 = @as(f64, @floatCast(as_f32));
                    fits_in_f32 = f64_value == back_to_f64;
                }
            }

            // create type vars, first "reserve" node slots
            const final_expr_idx = self.can_ir.store.predictNodeIndex(2);

            const requirements = types.Num.Frac.Requirements{
                .fits_in_f32 = fits_in_f32,
                .fits_in_dec = fits_in_dec,
            };

            // Create a polymorphic frac type variable
            const frac_type_var = self.can_ir.pushTypeVar(
                Content{ .structure = .{ .num = .{ .frac_poly = @enumFromInt(0) } } },
                final_expr_idx,
                region,
            );

            // Store the f64 value for now (TODO: update to store the full frac_value)
            const value: f64 = switch (frac_value) {
                .F64 => |f| f,
                .F32 => |f| @floatCast(f),
                .Dec => |d| blk: {
                    const dec = RocDec{ .num = @as(i128, @bitCast(d)) };
                    break :blk dec.toF64();
                },
            };

            // then in the final slot the actual expr is inserted
            const expr_idx = self.can_ir.store.addExpr(CIR.Expr{
                .frac = .{
                    .frac_var = frac_type_var,
                    .requirements = requirements,
                    .value = value,
                    .region = region,
                },
            });

            std.debug.assert(@intFromEnum(expr_idx) == @intFromEnum(final_expr_idx));

            // Insert concrete type variable
            _ = self.can_ir.setTypeVarAtExpr(
                expr_idx,
                Content{ .structure = .{ .num = .{ .num_poly = frac_type_var } } },
            );

            return expr_idx;
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

            const expr_idx = self.can_ir.store.addExpr(CIR.Expr{ .str = .{
                .span = str_segments_span,
                .region = self.tokenizedRegionToRegion(e.region),
            } });

            // Insert concrete type variable
            _ = self.can_ir.setTypeVarAtExpr(expr_idx, Content{ .structure = .str });

            return expr_idx;
        },
        .list => |e| {
            const region = self.tokenizedRegionToRegion(e.region);

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
                region,
            );

            // then in the final slot the actual expr is inserted
            const expr_idx = self.can_ir.store.addExpr(CIR.Expr{
                .list = .{
                    .elems = elems_span,
                    .elem_var = elem_type_var,
                    .region = region,
                },
            });

            // Insert concrete type variable
            _ = self.can_ir.setTypeVarAtExpr(
                expr_idx,
                Content{ .structure = .{ .list = elem_type_var } },
            );

            return expr_idx;
        },
        .tag => |e| {
            if (self.parse_ir.tokens.resolveIdentifier(e.token)) |tag_name| {
                const region = self.tokenizedRegionToRegion(e.region);

                // create type vars, first "reserve" node slots
                const final_expr_idx = self.can_ir.store.predictNodeIndex(2);

                // then insert the type vars, setting the parent to be the final slot
                const ext_type_var = self.can_ir.pushFreshTypeVar(final_expr_idx, region);

                // then in the final slot the actual expr is inserted
                const expr_idx = self.can_ir.store.addExpr(CIR.Expr{
                    .tag = .{
                        .ext_var = ext_type_var,
                        .name = tag_name,
                        .args = .{ .span = .{ .start = 0, .len = 0 } }, // empty arguments
                        .region = region,
                    },
                });

                std.debug.assert(@intFromEnum(expr_idx) == @intFromEnum(final_expr_idx));

                // Insert concrete type variable
                const tag_union = self.can_ir.env.types_store.mkTagUnion(
                    &[_]Tag{Tag{ .name = tag_name, .args = types.Var.SafeList.Range.empty }},
                    ext_type_var,
                );
                _ = self.can_ir.setTypeVarAtExpr(expr_idx, tag_union);

                return expr_idx;
            } else {
                return null;
            }
        },
        .string_part => |_| {
            const feature = self.can_ir.env.strings.insert(self.can_ir.env.gpa, "canonicalize string_part expression");
            const expr_idx = self.can_ir.pushMalformed(CIR.Expr.Idx, CIR.Diagnostic{ .not_implemented = .{
                .feature = feature,
                .region = Region.zero(),
            } });
            return expr_idx;
        },
        .tuple => |_| {
            const feature = self.can_ir.env.strings.insert(self.can_ir.env.gpa, "canonicalize tuple expression");
            const expr_idx = self.can_ir.pushMalformed(CIR.Expr.Idx, CIR.Diagnostic{ .not_implemented = .{
                .feature = feature,
                .region = Region.zero(),
            } });
            return expr_idx;
        },
        .record => |_| {
            const feature = self.can_ir.env.strings.insert(self.can_ir.env.gpa, "canonicalize record expression");
            const expr_idx = self.can_ir.pushMalformed(CIR.Expr.Idx, CIR.Diagnostic{ .not_implemented = .{
                .feature = feature,
                .region = Region.zero(),
            } });
            return expr_idx;
        },
        .lambda => |e| {
            const region = self.tokenizedRegionToRegion(e.region);

            // Enter function boundary
            self.enterFunction(region);
            defer self.exitFunction();

            // Enter new scope for function parameters and body
            self.scopeEnter(self.can_ir.env.gpa, true); // true = is_function_boundary
            defer self.scopeExit(self.can_ir.env.gpa) catch {};

            // args
            const gpa = self.can_ir.env.gpa;
            const args_start = self.can_ir.store.scratch_patterns.top();
            for (self.parse_ir.store.patternSlice(e.args)) |arg_pattern_idx| {
                if (self.canonicalize_pattern(arg_pattern_idx)) |pattern_idx| {
                    self.can_ir.store.scratch_patterns.append(gpa, pattern_idx);
                } else {
                    const arg = self.parse_ir.store.getPattern(arg_pattern_idx);
                    const arg_region = self.tokenizedRegionToRegion(arg.to_tokenized_region());
                    const malformed_idx = self.can_ir.pushMalformed(CIR.Pattern.Idx, CIR.Diagnostic{ .pattern_arg_invalid = .{
                        .region = arg_region,
                    } });
                    self.can_ir.store.scratch_patterns.append(gpa, malformed_idx);
                }
            }
            const args_span = self.can_ir.store.patternSpanFrom(args_start);

            // body
            const body_idx = blk: {
                if (self.canonicalize_expr(e.body)) |idx| {
                    break :blk idx;
                } else {
                    const ast_body = self.parse_ir.store.getExpr(e.body);
                    const body_region = self.tokenizedRegionToRegion(ast_body.to_tokenized_region());
                    break :blk self.can_ir.pushMalformed(CIR.Expr.Idx, CIR.Diagnostic{
                        .lambda_body_not_canonicalized = .{ .region = body_region },
                    });
                }
            };

            // Create lambda expression
            const lambda_expr = CIR.Expr{
                .lambda = .{
                    .args = args_span,
                    .body = body_idx,
                    .region = region,
                },
            };
            const expr_idx = self.can_ir.store.addExpr(lambda_expr);
            return expr_idx;
        },
        .record_updater => |_| {
            const feature = self.can_ir.env.strings.insert(self.can_ir.env.gpa, "canonicalize record_updater expression");
            const expr_idx = self.can_ir.pushMalformed(CIR.Expr.Idx, CIR.Diagnostic{ .not_implemented = .{
                .feature = feature,
                .region = Region.zero(),
            } });
            return expr_idx;
        },
        .field_access => |_| {
            const feature = self.can_ir.env.strings.insert(self.can_ir.env.gpa, "canonicalize record field_access expression");
            const expr_idx = self.can_ir.pushMalformed(CIR.Expr.Idx, CIR.Diagnostic{ .not_implemented = .{
                .feature = feature,
                .region = Region.zero(),
            } });
            return expr_idx;
        },
        .local_dispatch => |_| {
            const feature = self.can_ir.env.strings.insert(self.can_ir.env.gpa, "canonicalize local_dispatch expression");
            const expr_idx = self.can_ir.pushMalformed(CIR.Expr.Idx, CIR.Diagnostic{ .not_implemented = .{
                .feature = feature,
                .region = Region.zero(),
            } });
            return expr_idx;
        },
        .bin_op => |e| {
            const region = self.tokenizedRegionToRegion(e.region);

            // Canonicalize left and right operands
            const lhs = blk: {
                if (self.canonicalize_expr(e.left)) |left_expr_idx| {
                    break :blk left_expr_idx;
                } else {
                    // TODO should probably use LHS region here
                    const left_expr_idx = self.can_ir.pushMalformed(CIR.Expr.Idx, CIR.Diagnostic{ .expr_not_canonicalized = .{
                        .region = region,
                    } });
                    break :blk left_expr_idx;
                }
            };

            const rhs = blk: {
                if (self.canonicalize_expr(e.right)) |right_expr_idx| {
                    break :blk right_expr_idx;
                } else {
                    // TODO should probably use RHS region here
                    const right_expr_idx = self.can_ir.pushMalformed(CIR.Expr.Idx, CIR.Diagnostic{ .expr_not_canonicalized = .{
                        .region = region,
                    } });
                    break :blk right_expr_idx;
                }
            };

            // Get the operator token
            const op_token = self.parse_ir.tokens.tokens.get(e.operator);

            const op: CIR.Expr.Binop.Op = switch (op_token.tag) {
                .OpPlus => .add,
                .OpBinaryMinus => .sub,
                .OpStar => .mul,
                else => {
                    // Unknown operator
                    const feature = self.can_ir.env.strings.insert(self.can_ir.env.gpa, "binop");
                    const expr_idx = self.can_ir.pushMalformed(CIR.Expr.Idx, CIR.Diagnostic{ .not_implemented = .{
                        .feature = feature,
                        .region = region,
                    } });
                    return expr_idx;
                },
            };

            const expr_idx = self.can_ir.store.addExpr(CIR.Expr{
                .binop = CIR.Expr.Binop.init(op, lhs, rhs, region),
            });

            _ = self.can_ir.setTypeVarAtExpr(expr_idx, Content{ .flex_var = null });

            return expr_idx;
        },
        .suffix_single_question => |_| {
            const feature = self.can_ir.env.strings.insert(self.can_ir.env.gpa, "canonicalize suffix_single_question expression");
            const expr_idx = self.can_ir.pushMalformed(CIR.Expr.Idx, CIR.Diagnostic{ .not_implemented = .{
                .feature = feature,
                .region = Region.zero(),
            } });
            return expr_idx;
        },
        .unary_op => |_| {
            const feature = self.can_ir.env.strings.insert(self.can_ir.env.gpa, "canonicalize unary_op expression");
            const expr_idx = self.can_ir.pushMalformed(CIR.Expr.Idx, CIR.Diagnostic{ .not_implemented = .{
                .feature = feature,
                .region = Region.zero(),
            } });
            return expr_idx;
        },
        .if_then_else => |_| {
            const feature = self.can_ir.env.strings.insert(self.can_ir.env.gpa, "canonicalize if_then_else expression");
            const expr_idx = self.can_ir.pushMalformed(CIR.Expr.Idx, CIR.Diagnostic{ .not_implemented = .{
                .feature = feature,
                .region = Region.zero(),
            } });
            return expr_idx;
        },
        .match => |_| {
            const feature = self.can_ir.env.strings.insert(self.can_ir.env.gpa, "canonicalize match expression");
            const expr_idx = self.can_ir.pushMalformed(CIR.Expr.Idx, CIR.Diagnostic{ .not_implemented = .{
                .feature = feature,
                .region = Region.zero(),
            } });
            return expr_idx;
        },
        .dbg => |_| {
            const feature = self.can_ir.env.strings.insert(self.can_ir.env.gpa, "canonicalize dbg expression");
            const expr_idx = self.can_ir.pushMalformed(CIR.Expr.Idx, CIR.Diagnostic{ .not_implemented = .{
                .feature = feature,
                .region = Region.zero(),
            } });
            return expr_idx;
        },
        .record_builder => |_| {
            const feature = self.can_ir.env.strings.insert(self.can_ir.env.gpa, "canonicalize record_builder expression");
            const expr_idx = self.can_ir.pushMalformed(CIR.Expr.Idx, CIR.Diagnostic{ .not_implemented = .{
                .feature = feature,
                .region = Region.zero(),
            } });
            return expr_idx;
        },
        .ellipsis => |_| {
            const feature = self.can_ir.env.strings.insert(self.can_ir.env.gpa, "canonicalize ellipsis expression");
            const expr_idx = self.can_ir.pushMalformed(CIR.Expr.Idx, CIR.Diagnostic{ .not_implemented = .{
                .feature = feature,
                .region = Region.zero(),
            } });
            return expr_idx;
        },
        .block => |e| {
            const region = self.tokenizedRegionToRegion(e.region);

            // Blocks don't introduce function boundaries, but may contain var statements
            self.scopeEnter(self.can_ir.env.gpa, false); // false = not a function boundary
            defer self.scopeExit(self.can_ir.env.gpa) catch {};

            // Keep track of the start position for statements
            const stmt_start = self.can_ir.store.scratch_statements.top();

            // Canonicalize all statements in the block
            const statements = self.parse_ir.store.statementSlice(e.statements);
            var last_expr: ?CIR.Expr.Idx = null;

            for (statements, 0..) |stmt_idx, i| {
                // Check if this is the last statement and if it's an expression
                const is_last = (i == statements.len - 1);
                const stmt = self.parse_ir.store.getStatement(stmt_idx);

                if (is_last and stmt == .expr) {
                    // For the last expression statement, canonicalize it directly as the final expression
                    // without adding it as a statement
                    last_expr = self.canonicalize_expr(stmt.expr.expr);
                } else {
                    // Regular statement processing
                    const result = self.canonicalize_statement(stmt_idx);
                    if (result) |expr_idx| {
                        last_expr = expr_idx;
                    }
                }
            }

            // Determine the final expression
            const final_expr = if (last_expr) |expr_idx|
                expr_idx
            else blk: {
                // Empty block - create empty record
                const expr_idx = self.can_ir.store.addExpr(CIR.Expr{
                    .empty_record = .{ .region = region },
                });
                _ = self.can_ir.setTypeVarAtExpr(expr_idx, Content{ .structure = .empty_record });
                break :blk expr_idx;
            };

            // Create statement span
            const stmt_span = self.can_ir.store.statementSpanFrom(stmt_start);

            // Create and return block expression
            const block_expr = CIR.Expr{
                .block = .{
                    .stmts = stmt_span,
                    .final_expr = final_expr,
                    .region = region,
                },
            };
            const block_idx = self.can_ir.store.addExpr(block_expr);

            // TODO: Propagate type from final expression during type checking
            // For now, create a fresh type var for the block
            _ = self.can_ir.pushFreshTypeVar(@enumFromInt(@intFromEnum(block_idx)), region);

            return block_idx;
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
    ast_pattern_idx: AST.Pattern.Idx,
) ?CIR.Pattern.Idx {
    const gpa = self.can_ir.env.gpa;
    switch (self.parse_ir.store.getPattern(ast_pattern_idx)) {
        .ident => |e| {
            const region = self.tokenizedRegionToRegion(e.region);
            if (self.parse_ir.tokens.resolveIdentifier(e.ident_tok)) |ident_idx| {
                // Push a Pattern node for our identifier
                const assign_idx = self.can_ir.store.addPattern(CIR.Pattern{ .assign = .{
                    .ident = ident_idx,
                    .region = region,
                } });
                _ = self.can_ir.setTypeVarAtPat(assign_idx, .{ .flex_var = null });

                // Introduce the identifier into scope mapping to this pattern node
                switch (self.scopeIntroduceInternal(self.can_ir.env.gpa, &self.can_ir.env.idents, .ident, ident_idx, assign_idx, false, true)) {
                    .success => {},
                    .shadowing_warning => |shadowed_pattern_idx| {
                        const shadowed_pattern = self.can_ir.store.getPattern(shadowed_pattern_idx);
                        const original_region = shadowed_pattern.toRegion();
                        self.can_ir.pushDiagnostic(CIR.Diagnostic{ .shadowing_warning = .{
                            .ident = ident_idx,
                            .region = region,
                            .original_region = original_region,
                        } });
                    },
                    .top_level_var_error => {
                        return self.can_ir.pushMalformed(CIR.Pattern.Idx, CIR.Diagnostic{ .invalid_top_level_statement = .{
                            .stmt = self.can_ir.env.strings.insert(self.can_ir.env.gpa, "var"),
                        } });
                    },
                    .var_across_function_boundary => {
                        return self.can_ir.pushMalformed(CIR.Pattern.Idx, CIR.Diagnostic{ .ident_already_in_scope = .{
                            .ident = ident_idx,
                            .region = region,
                        } });
                    },
                }

                return assign_idx;
            } else {
                const feature = self.can_ir.env.strings.insert(self.can_ir.env.gpa, "report an error when unable to resolve identifier");
                const malformed_idx = self.can_ir.pushMalformed(CIR.Pattern.Idx, CIR.Diagnostic{ .not_implemented = .{
                    .feature = feature,
                    .region = Region.zero(),
                } });
                return malformed_idx;
            }
        },
        .underscore => |p| {
            const underscore_pattern = CIR.Pattern{
                .underscore = .{
                    .region = self.tokenizedRegionToRegion(p.region),
                },
            };

            const pattern_idx = self.can_ir.store.addPattern(underscore_pattern);

            _ = self.can_ir.setTypeVarAtPat(pattern_idx, Content{ .flex_var = null });

            return pattern_idx;
        },
        .number => |e| {
            const region = self.tokenizedRegionToRegion(e.region);

            // Resolve to a string slice from the source
            const token_text = self.parse_ir.resolve(e.number_tok);

            // Check if this is a float literal (contains '.' or 'e'/'E')
            const is_float = std.mem.indexOfAny(u8, token_text, ".eE") != null;

            if (is_float) {
                // Handle float literal pattern
                const RocDec = @import("../builtins/dec.zig").RocDec;

                var fits_in_f32 = false;
                var fits_in_dec = false;

                // create type vars, first "reserve" node slots
                const final_pattern_idx = self.can_ir.store.predictNodeIndex(2);

                // then insert the type vars, setting the parent to be the final slot
                const num_type_var = self.can_ir.pushFreshTypeVar(final_pattern_idx, region);

                // Try to parse as RocDec first
                if (RocDec.fromNonemptySlice(token_text)) |dec| {
                    // Successfully parsed as Dec
                    fits_in_dec = true;

                    // Also check if it fits in f32
                    const f64_val = dec.toF64();

                    // Check if it fits in f32 without precision loss
                    if (f64_val >= -std.math.floatMax(f32) and f64_val <= std.math.floatMax(f32)) {
                        const as_f32 = @as(f32, @floatCast(f64_val));
                        const back_to_f64 = @as(f64, @floatCast(as_f32));
                        if (f64_val == back_to_f64) {
                            fits_in_f32 = true;
                        }
                    }

                    const requirements = types.Num.Frac.Requirements{
                        .fits_in_f32 = fits_in_f32,
                        .fits_in_dec = fits_in_dec,
                    };

                    // Create dec_literal pattern
                    const dec_pattern = CIR.Pattern{
                        .dec_literal = .{
                            .num_var = num_type_var,
                            .requirements = requirements,
                            .value = dec,
                            .region = region,
                        },
                    };
                    const pattern_idx = self.can_ir.store.addPattern(dec_pattern);

                    std.debug.assert(@intFromEnum(pattern_idx) == @intFromEnum(final_pattern_idx));

                    // Set the concrete type variable
                    _ = self.can_ir.setTypeVarAtPat(pattern_idx, Content{
                        .structure = .{ .num = .{ .num_poly = num_type_var } },
                    });

                    return pattern_idx;
                } else {
                    // Failed to parse as Dec - parse as f64
                    const f64_value = std.fmt.parseFloat(f64, token_text) catch {
                        // Invalid float literal
                        const malformed_idx = self.can_ir.pushMalformed(CIR.Pattern.Idx, CIR.Diagnostic{ .invalid_num_literal = .{
                            .region = region,
                        } });
                        return malformed_idx;
                    };

                    // Check for non-finite values (NaN, Infinity, -Infinity)
                    if (std.math.isNan(f64_value) or std.math.isInf(f64_value)) {
                        // Non-finite values can be represented in f32 and f64, but not dec
                        fits_in_f32 = true;
                        fits_in_dec = false;
                    } else {
                        // Check if it fits in f32 without precision loss
                        if (f64_value >= -std.math.floatMax(f32) and f64_value <= std.math.floatMax(f32)) {
                            const as_f32 = @as(f32, @floatCast(f64_value));
                            const back_to_f64 = @as(f64, @floatCast(as_f32));
                            if (f64_value == back_to_f64) {
                                fits_in_f32 = true;
                            }
                        }
                    }

                    const requirements = types.Num.Frac.Requirements{
                        .fits_in_f32 = fits_in_f32,
                        .fits_in_dec = fits_in_dec,
                    };

                    // Create f64_literal pattern
                    const f64_pattern = CIR.Pattern{
                        .f64_literal = .{
                            .num_var = num_type_var,
                            .requirements = requirements,
                            .value = f64_value,
                            .region = region,
                        },
                    };
                    const pattern_idx = self.can_ir.store.addPattern(f64_pattern);

                    std.debug.assert(@intFromEnum(pattern_idx) == @intFromEnum(final_pattern_idx));

                    // Set the concrete type variable
                    _ = self.can_ir.setTypeVarAtPat(pattern_idx, Content{
                        .structure = .{ .num = .{ .num_poly = num_type_var } },
                    });

                    return pattern_idx;
                }
            } else {
                // Parse as integer
                const value = std.fmt.parseInt(i128, token_text, 10) catch {
                    // Invalid num literal
                    const malformed_idx = self.can_ir.pushMalformed(CIR.Pattern.Idx, CIR.Diagnostic{ .invalid_num_literal = .{
                        .region = region,
                    } });
                    return malformed_idx;
                };

                // Calculate requirements based on the value
                const sign_needed = value < 0;
                const u128_val: u128 = if (value < 0) @as(u128, @intCast(-(value + 1))) + 1 else @as(u128, @intCast(value));
                const bits_needed: types.Num.Int.BitsNeeded = if (u128_val <= 127) .@"7" else if (u128_val <= 255) .@"8" else if (u128_val <= 32767) .@"9_to_15" else if (u128_val <= 65535) .@"16" else if (u128_val <= 2147483647) .@"17_to_31" else if (u128_val <= 4294967295) .@"32" else if (u128_val <= 9223372036854775807) .@"33_to_63" else if (u128_val <= 18446744073709551615) .@"64" else if (u128_val <= 170141183460469231731687303715884105727) .@"65_to_127" else .@"128";
                const requirements = types.Num.Int.Requirements{
                    .sign_needed = sign_needed,
                    .bits_needed = bits_needed,
                };

                // Create type vars, first "reserve" node slots
                const final_pattern_idx = self.can_ir.store.predictNodeIndex(2);
                const num_type_var = self.can_ir.pushFreshTypeVar(final_pattern_idx, region);

                // then in the final slot the actual pattern is inserted
                const int_pattern = CIR.Pattern{
                    .int_literal = .{
                        .num_var = num_type_var,
                        .requirements = requirements,
                        .value = CIR.IntLiteralValue{ .value = value },
                        .region = region,
                    },
                };
                const pattern_idx = self.can_ir.store.addPattern(int_pattern);

                std.debug.assert(@intFromEnum(pattern_idx) == @intFromEnum(final_pattern_idx));

                // Set the concrete type variable
                _ = self.can_ir.setTypeVarAtPat(pattern_idx, Content{
                    .structure = .{ .num = .{ .num_poly = num_type_var } },
                });

                return pattern_idx;
            }
        },
        .string => |e| {
            const region = self.tokenizedRegionToRegion(e.region);

            // resolve to a string slice from the source
            const token_text = self.parse_ir.resolve(e.string_tok);

            // TODO: Handle escape sequences
            // For now, just intern the raw string
            const literal = self.can_ir.env.strings.insert(gpa, token_text);

            const str_pattern = CIR.Pattern{
                .str_literal = .{
                    .literal = literal,
                    .region = region,
                },
            };
            const pattern_idx = self.can_ir.store.addPattern(str_pattern);

            // Set the concrete type variable
            _ = self.can_ir.setTypeVarAtPat(pattern_idx, Content{ .structure = .str });

            return pattern_idx;
        },
        .tag => |e| {
            if (self.parse_ir.tokens.resolveIdentifier(e.tag_tok)) |tag_name| {
                const start = self.can_ir.store.scratch_patterns.top();

                for (self.parse_ir.store.patternSlice(e.args)) |sub_ast_pattern_idx| {
                    if (self.canonicalize_pattern(sub_ast_pattern_idx)) |idx| {
                        self.can_ir.store.scratch_patterns.append(gpa, idx);
                    } else {
                        const arg = self.parse_ir.store.getPattern(sub_ast_pattern_idx);
                        const arg_region = self.tokenizedRegionToRegion(arg.to_tokenized_region());
                        const malformed_idx = self.can_ir.pushMalformed(CIR.Pattern.Idx, CIR.Diagnostic{ .pattern_arg_invalid = .{
                            .region = arg_region,
                        } });
                        self.can_ir.store.scratch_patterns.append(gpa, malformed_idx);
                    }
                }

                const region = self.tokenizedRegionToRegion(e.region);

                const args = self.can_ir.store.patternSpanFrom(start);

                // create type vars, first "reserve" node slots
                const final_pattern_idx = self.can_ir.store.predictNodeIndex(2);

                // then insert the type vars, setting the parent to be the final slot
                const ext_type_var = self.can_ir.pushFreshTypeVar(final_pattern_idx, region);

                // then in the final slot the actual pattern is inserted
                const tag_pattern = CIR.Pattern{
                    .applied_tag = .{
                        .ext_var = ext_type_var,
                        .tag_name = tag_name,
                        .arguments = args,
                        .region = region,
                    },
                };
                const pattern_idx = self.can_ir.store.addPattern(tag_pattern);

                std.debug.assert(@intFromEnum(pattern_idx) == @intFromEnum(final_pattern_idx));

                // Set the concrete type variable
                const tag_union_type = self.can_ir.env.types_store.mkTagUnion(
                    &[_]Tag{Tag{ .name = tag_name, .args = types.Var.SafeList.Range.empty }},
                    ext_type_var,
                );
                _ = self.can_ir.setTypeVarAtPat(pattern_idx, tag_union_type);

                return pattern_idx;
            }
            return null;
        },
        .record => |_| {
            const feature = self.can_ir.env.strings.insert(self.can_ir.env.gpa, "canonicalize record pattern");
            const pattern_idx = self.can_ir.pushMalformed(CIR.Pattern.Idx, CIR.Diagnostic{ .not_implemented = .{
                .feature = feature,
                .region = Region.zero(),
            } });
            return pattern_idx;
        },
        .tuple => |_| {
            const feature = self.can_ir.env.strings.insert(self.can_ir.env.gpa, "canonicalize tuple pattern");
            const pattern_idx = self.can_ir.pushMalformed(CIR.Pattern.Idx, CIR.Diagnostic{ .not_implemented = .{
                .feature = feature,
                .region = Region.zero(),
            } });
            return pattern_idx;
        },
        .list => |_| {
            const feature = self.can_ir.env.strings.insert(self.can_ir.env.gpa, "canonicalize list pattern");
            const pattern_idx = self.can_ir.pushMalformed(CIR.Pattern.Idx, CIR.Diagnostic{ .not_implemented = .{
                .feature = feature,
                .region = Region.zero(),
            } });
            return pattern_idx;
        },
        .list_rest => |_| {
            const feature = self.can_ir.env.strings.insert(self.can_ir.env.gpa, "canonicalize list rest pattern");
            const pattern_idx = self.can_ir.pushMalformed(CIR.Pattern.Idx, CIR.Diagnostic{ .not_implemented = .{
                .feature = feature,
                .region = Region.zero(),
            } });
            return pattern_idx;
        },
        .alternatives => |_| {
            const feature = self.can_ir.env.strings.insert(self.can_ir.env.gpa, "canonicalize alternatives pattern");
            const pattern_idx = self.can_ir.pushMalformed(CIR.Pattern.Idx, CIR.Diagnostic{ .not_implemented = .{
                .feature = feature,
                .region = Region.zero(),
            } });
            return pattern_idx;
        },
        .malformed => |malformed| {
            // We won't touch this since it's already a parse error.
            _ = malformed;
            return null;
        },
    }
}

/// Enter a function boundary by pushing its region onto the stack
fn enterFunction(self: *Self, region: Region) void {
    self.function_regions.append(self.can_ir.env.gpa, region) catch |err| exitOnOom(err);
}

/// Exit a function boundary by popping from the stack
fn exitFunction(self: *Self) void {
    _ = self.function_regions.pop();
}

/// Get the current function region (the function we're currently in)
fn getCurrentFunctionRegion(self: *const Self) ?Region {
    if (self.function_regions.items.len > 0) {
        return self.function_regions.items[self.function_regions.items.len - 1];
    }
    return null;
}

/// Record which function a var pattern was declared in
fn recordVarFunction(self: *Self, pattern_idx: CIR.Pattern.Idx) void {
    // Mark this pattern as a var
    self.var_patterns.put(self.can_ir.env.gpa, pattern_idx, {}) catch |err| exitOnOom(err);

    if (self.getCurrentFunctionRegion()) |function_region| {
        self.var_function_regions.put(self.can_ir.env.gpa, pattern_idx, function_region) catch |err| exitOnOom(err);
    }
}

/// Check if a pattern is a var
fn isVarPattern(self: *const Self, pattern_idx: CIR.Pattern.Idx) bool {
    return self.var_patterns.contains(pattern_idx);
}

/// Check if a var reassignment crosses function boundaries
fn isVarReassignmentAcrossFunctionBoundary(self: *const Self, pattern_idx: CIR.Pattern.Idx) bool {
    if (self.var_function_regions.get(pattern_idx)) |var_function_region| {
        if (self.getCurrentFunctionRegion()) |current_function_region| {
            return !var_function_region.eq(current_function_region);
        }
    }
    return false;
}

test {
    _ = @import("canonicalize/test/int_test.zig");
}

/// Introduce a new identifier to the current scope, return an
/// index if
fn scopeIntroduceIdent(
    self: Self,
    ident_idx: Ident.Idx,
    pattern_idx: CIR.Pattern.Idx,
    region: Region,
    comptime T: type,
) T {
    const result = self.scopeIntroduceInternal(self.can_ir.env.gpa, &self.can_ir.env.idents, .ident, ident_idx, pattern_idx, false, true);

    switch (result) {
        .success => {
            return pattern_idx;
        },
        .shadowing_warning => |shadowed_pattern_idx| {
            const shadowed_pattern = self.can_ir.store.getPattern(shadowed_pattern_idx);
            const original_region = shadowed_pattern.toRegion();
            self.can_ir.pushDiagnostic(CIR.Diagnostic{ .shadowing_warning = .{
                .ident = ident_idx,
                .region = region,
                .original_region = original_region,
            } });
            return pattern_idx;
        },
        .top_level_var_error => {
            return self.can_ir.pushMalformed(T, CIR.Diagnostic{ .invalid_top_level_statement = .{
                .stmt = self.can_ir.env.strings.insert(self.can_ir.env.gpa, "var"),
            } });
        },
        .var_across_function_boundary => |_| {
            // This shouldn't happen for regular identifiers
            return self.can_ir.pushMalformed(T, CIR.Diagnostic{ .not_implemented = .{
                .feature = self.can_ir.env.strings.insert(self.can_ir.env.gpa, "var across function boundary for non-var identifier"),
                .region = region,
            } });
        },
    }
}

/// Introduce a var identifier to the current scope with function boundary tracking
fn scopeIntroduceVar(
    self: *Self,
    ident_idx: Ident.Idx,
    pattern_idx: CIR.Pattern.Idx,
    region: Region,
    is_declaration: bool,
    comptime T: type,
) T {
    const result = self.scopeIntroduceInternal(self.can_ir.env.gpa, &self.can_ir.env.idents, .ident, ident_idx, pattern_idx, true, is_declaration);

    switch (result) {
        .success => {
            // If this is a var declaration, record which function it belongs to
            if (is_declaration) {
                self.recordVarFunction(pattern_idx);
            }
            return pattern_idx;
        },
        .shadowing_warning => |shadowed_pattern_idx| {
            const shadowed_pattern = self.can_ir.store.getPattern(shadowed_pattern_idx);
            const original_region = shadowed_pattern.toRegion();
            self.can_ir.pushDiagnostic(CIR.Diagnostic{ .shadowing_warning = .{
                .ident = ident_idx,
                .region = region,
                .original_region = original_region,
            } });
            if (is_declaration) {
                self.recordVarFunction(pattern_idx);
            }
            return pattern_idx;
        },
        .top_level_var_error => {
            return self.can_ir.pushMalformed(T, CIR.Diagnostic{ .invalid_top_level_statement = .{
                .stmt = self.can_ir.env.strings.insert(self.can_ir.env.gpa, "var"),
            } });
        },
        .var_across_function_boundary => |_| {
            // Generate crash expression for var reassignment across function boundary
            return self.can_ir.pushMalformed(T, CIR.Diagnostic{ .var_across_function_boundary = .{
                .region = region,
            } });
        },
    }
}

/// Canonicalize a statement within a block
fn canonicalize_statement(self: *Self, stmt_idx: AST.Statement.Idx) ?CIR.Expr.Idx {
    const stmt = self.parse_ir.store.getStatement(stmt_idx);

    switch (stmt) {
        .decl => |d| {
            // Check if this is a var reassignment
            const pattern = self.parse_ir.store.getPattern(d.pattern);
            if (pattern == .ident) {
                const ident_tok = pattern.ident.ident_tok;
                if (self.parse_ir.tokens.resolveIdentifier(ident_tok)) |ident_idx| {
                    const region = self.tokenizedRegionToRegion(self.parse_ir.store.getPattern(d.pattern).to_tokenized_region());

                    // Check if this identifier exists and is a var
                    switch (self.scopeLookup(&self.can_ir.env.idents, .ident, ident_idx)) {
                        .found => |existing_pattern_idx| {
                            // Check if this is a var reassignment across function boundaries
                            if (self.isVarReassignmentAcrossFunctionBoundary(existing_pattern_idx)) {
                                // Generate error for var reassignment across function boundary
                                const error_expr = self.can_ir.pushMalformed(CIR.Expr.Idx, CIR.Diagnostic{ .var_across_function_boundary = .{
                                    .region = region,
                                } });

                                // Create a reassign statement with the error expression
                                const reassign_stmt = CIR.Statement{ .reassign = .{
                                    .pattern_idx = existing_pattern_idx,
                                    .expr = error_expr,
                                    .region = region,
                                } };
                                const reassign_idx = self.can_ir.store.addStatement(reassign_stmt);
                                self.can_ir.store.addScratchStatement(reassign_idx);

                                return error_expr;
                            }

                            // Check if this was declared as a var
                            if (self.isVarPattern(existing_pattern_idx)) {
                                // This is a var reassignment - canonicalize the expression and create reassign statement
                                const expr_idx = self.canonicalize_expr(d.body) orelse return null;

                                // Create reassign statement
                                const reassign_stmt = CIR.Statement{ .reassign = .{
                                    .pattern_idx = existing_pattern_idx,
                                    .expr = expr_idx,
                                    .region = region,
                                } };
                                const reassign_idx = self.can_ir.store.addStatement(reassign_stmt);
                                self.can_ir.store.addScratchStatement(reassign_idx);

                                return expr_idx;
                            }
                        },
                        .not_found => {
                            // Not found in scope, fall through to regular declaration
                        },
                    }
                }
            }

            // Regular declaration - canonicalize as usual
            const pattern_idx = self.canonicalize_pattern(d.pattern) orelse return null;
            const expr_idx = self.canonicalize_expr(d.body) orelse return null;

            // Create a declaration statement
            const decl_stmt = CIR.Statement{ .decl = .{
                .pattern = pattern_idx,
                .expr = expr_idx,
                .region = self.tokenizedRegionToRegion(d.region),
            } };
            const decl_idx = self.can_ir.store.addStatement(decl_stmt);
            self.can_ir.store.addScratchStatement(decl_idx);

            return expr_idx;
        },
        .@"var" => |v| {
            // Var declaration - handle specially with function boundary tracking
            const var_name = self.parse_ir.tokens.resolveIdentifier(v.name) orelse return null;
            const region = self.tokenizedRegionToRegion(v.region);

            // Canonicalize the initial value
            const init_expr_idx = self.canonicalize_expr(v.body) orelse return null;

            // Create pattern for the var
            const pattern_idx = self.can_ir.store.addPattern(CIR.Pattern{ .assign = .{ .ident = var_name, .region = region } });

            // Introduce the var with function boundary tracking
            _ = self.scopeIntroduceVar(var_name, pattern_idx, region, true, CIR.Pattern.Idx);

            // Create var statement
            const var_stmt = CIR.Statement{ .@"var" = .{
                .pattern_idx = pattern_idx,
                .expr = init_expr_idx,
                .region = region,
            } };
            const var_idx = self.can_ir.store.addStatement(var_stmt);
            self.can_ir.store.addScratchStatement(var_idx);

            return init_expr_idx;
        },
        .expr => |e| {
            // Expression statement
            const expr_idx = self.canonicalize_expr(e.expr) orelse return null;

            // Create expression statement
            const expr_stmt = CIR.Statement{ .expr = .{
                .expr = expr_idx,
                .region = self.tokenizedRegionToRegion(e.region),
            } };
            const expr_stmt_idx = self.can_ir.store.addStatement(expr_stmt);
            self.can_ir.store.addScratchStatement(expr_stmt_idx);

            return expr_idx;
        },
        .crash => |c| {
            // Crash statement
            const region = self.tokenizedRegionToRegion(c.region);
            const msg_expr = self.canonicalize_expr(c.expr) orelse {
                const feature = self.can_ir.env.strings.insert(self.can_ir.env.gpa, "crash message not canonicalized");
                return self.can_ir.pushMalformed(CIR.Expr.Idx, CIR.Diagnostic{ .not_implemented = .{
                    .feature = feature,
                    .region = region,
                } });
            };

            // Create a crash expression using pushMalformed
            _ = msg_expr; // TODO: incorporate crash message
            const feature = self.can_ir.env.strings.insert(self.can_ir.env.gpa, "explicit crash");
            return self.can_ir.pushMalformed(CIR.Expr.Idx, CIR.Diagnostic{ .not_implemented = .{
                .feature = feature,
                .region = region,
            } });
        },
        else => {
            // Other statement types not yet implemented
            const feature = self.can_ir.env.strings.insert(self.can_ir.env.gpa, "statement type in block");
            return self.can_ir.pushMalformed(CIR.Expr.Idx, CIR.Diagnostic{ .not_implemented = .{
                .feature = feature,
                .region = Region.zero(),
            } });
        },
    }
}

/// Enter a new scope level
fn scopeEnter(self: *Self, gpa: std.mem.Allocator, is_function_boundary: bool) void {
    const scope = Scope.init(is_function_boundary);
    self.scopes.append(gpa, scope) catch |err| collections.utils.exitOnOom(err);
}

/// Exit the current scope level
fn scopeExit(self: *Self, gpa: std.mem.Allocator) Scope.Error!void {
    if (self.scopes.items.len <= 1) {
        return Scope.Error.ExitedTopScopeLevel;
    }
    var scope: Scope = self.scopes.pop().?;
    scope.deinit(gpa);
}

/// This will be used later for builtins like Num.nan, Num.infinity, etc.
pub fn addNonFiniteFloat(self: *Self, value: f64, region: base.Region) CIR.Expr.Idx {
    // Dec doesn't have infinity, -infinity, or NaN
    const requirements = types.Num.Frac.Requirements{
        .fits_in_f32 = true,
        .fits_in_dec = false,
    };

    // Create type vars, first "reserve" node slots
    const final_expr_idx = self.can_ir.store.predictNodeIndex(2);

    // Create a polymorphic frac type variable
    const frac_type_var = self.can_ir.pushTypeVar(
        Content{ .structure = .{ .num = .{ .frac_poly = @enumFromInt(0) } } },
        final_expr_idx,
        region,
    );

    // then in the final slot the actual expr is inserted
    const expr_idx = self.can_ir.store.addExpr(CIR.Expr{
        .frac = .{
            .frac_var = frac_type_var,
            .requirements = requirements,
            .value = value,
            .region = region,
        },
    });

    std.debug.assert(@intFromEnum(expr_idx) == @intFromEnum(final_expr_idx));

    // Insert concrete type variable
    _ = self.can_ir.setTypeVarAtExpr(
        expr_idx,
        Content{ .structure = .{ .num = .{ .num_poly = frac_type_var } } },
    );

    return expr_idx;
}

/// Check if an identifier is in scope
fn scopeContains(
    self: *Self,
    ident_store: *const base.Ident.Store,
    comptime item_kind: Scope.ItemKind,
    name: base.Ident.Idx,
) ?CIR.Pattern.Idx {
    var scope_idx = self.scopes.items.len;
    while (scope_idx > 0) {
        scope_idx -= 1;
        const scope = &self.scopes.items[scope_idx];
        const map = scope.itemsConst(item_kind);

        var iter = map.iterator();
        while (iter.next()) |entry| {
            if (ident_store.identsHaveSameText(name, entry.key_ptr.*)) {
                return entry.value_ptr.*;
            }
        }
    }
    return null;
}

/// Look up an identifier in the scope
fn scopeLookup(
    self: *Self,
    ident_store: *const base.Ident.Store,
    comptime item_kind: Scope.ItemKind,
    name: base.Ident.Idx,
) Scope.LookupResult {
    if (self.scopeContains(ident_store, item_kind, name)) |pattern| {
        return Scope.LookupResult{ .found = pattern };
    }
    return Scope.LookupResult{ .not_found = {} };
}

/// Introduce a new identifier to the current scope level
fn scopeIntroduceInternal(
    self: *Self,
    gpa: std.mem.Allocator,
    ident_store: *const base.Ident.Store,
    comptime item_kind: Scope.ItemKind,
    ident_idx: base.Ident.Idx,
    pattern_idx: CIR.Pattern.Idx,
    is_var: bool,
    is_declaration: bool,
) Scope.IntroduceResult {
    // Check if var is being used at top-level
    if (is_var and self.scopes.items.len == 1) {
        return Scope.IntroduceResult{ .top_level_var_error = {} };
    }

    // Check for existing identifier in any scope level for shadowing detection
    if (self.scopeContains(ident_store, item_kind, ident_idx)) |existing_pattern| {
        // If it's a var reassignment (not declaration), check function boundaries
        if (is_var and !is_declaration) {
            // Find the scope where the var was declared and check for function boundaries
            var declaration_scope_idx: ?usize = null;
            var scope_idx = self.scopes.items.len;

            // First, find where the identifier was declared
            while (scope_idx > 0) {
                scope_idx -= 1;
                const scope = &self.scopes.items[scope_idx];
                const map = scope.itemsConst(item_kind);

                var iter = map.iterator();
                while (iter.next()) |entry| {
                    if (ident_store.identsHaveSameText(ident_idx, entry.key_ptr.*)) {
                        declaration_scope_idx = scope_idx;
                        break;
                    }
                }

                if (declaration_scope_idx != null) break;
            }

            // Now check if there are function boundaries between declaration and current scope
            if (declaration_scope_idx) |decl_idx| {
                var current_idx = decl_idx + 1;
                var found_function_boundary = false;

                while (current_idx < self.scopes.items.len) {
                    const scope = &self.scopes.items[current_idx];
                    if (scope.is_function_boundary) {
                        found_function_boundary = true;
                        break;
                    }
                    current_idx += 1;
                }

                if (found_function_boundary) {
                    // Different function, return error
                    return Scope.IntroduceResult{ .var_across_function_boundary = existing_pattern };
                } else {
                    // Same function, allow reassignment without warning
                    self.scopes.items[self.scopes.items.len - 1].put(gpa, item_kind, ident_idx, pattern_idx);
                    return Scope.IntroduceResult{ .success = {} };
                }
            }
        }

        // Regular shadowing case - produce warning but still introduce
        self.scopes.items[self.scopes.items.len - 1].put(gpa, item_kind, ident_idx, pattern_idx);
        return Scope.IntroduceResult{ .shadowing_warning = existing_pattern };
    }

    // Check the current level for duplicates
    const current_scope = &self.scopes.items[self.scopes.items.len - 1];
    const map = current_scope.itemsConst(item_kind);

    var iter = map.iterator();
    while (iter.next()) |entry| {
        if (ident_store.identsHaveSameText(ident_idx, entry.key_ptr.*)) {
            // Duplicate in same scope - still introduce but return shadowing warning
            self.scopes.items[self.scopes.items.len - 1].put(gpa, item_kind, ident_idx, pattern_idx);
            return Scope.IntroduceResult{ .shadowing_warning = entry.value_ptr.* };
        }
    }

    // No conflicts, introduce successfully
    self.scopes.items[self.scopes.items.len - 1].put(gpa, item_kind, ident_idx, pattern_idx);
    return Scope.IntroduceResult{ .success = {} };
}

/// Get all identifiers in scope
fn scopeAllIdents(self: *const Self, gpa: std.mem.Allocator, comptime item_kind: Scope.ItemKind) []base.Ident.Idx {
    var result = std.ArrayList(base.Ident.Idx).init(gpa);

    for (self.scopes.items) |scope| {
        const map = scope.itemsConst(item_kind);
        var iter = map.iterator();
        while (iter.next()) |entry| {
            result.append(entry.key_ptr.*) catch |err| collections.utils.exitOnOom(err);
        }
    }

    return result.toOwnedSlice() catch |err| collections.utils.exitOnOom(err);
}

/// Context helper for Scope tests
const ScopeTestContext = struct {
    self: Self,
    cir: *CIR,
    env: *base.ModuleEnv,
    gpa: std.mem.Allocator,

    fn init(gpa: std.mem.Allocator) !ScopeTestContext {
        // heap allocate env for testing
        const env = try gpa.create(base.ModuleEnv);
        env.* = base.ModuleEnv.init(gpa);

        // heap allocate CIR for testing
        const cir = try gpa.create(CIR);
        cir.* = CIR.init(env);

        return ScopeTestContext{
            .self = Self.init(cir, undefined),
            .cir = cir,
            .env = env,
            .gpa = gpa,
        };
    }

    fn deinit(ctx: *ScopeTestContext) void {
        ctx.self.deinit();
        ctx.cir.deinit();
        ctx.gpa.destroy(ctx.cir);
        ctx.env.deinit();
        ctx.gpa.destroy(ctx.env);
    }
};

// We write out this giant literal because it's actually annoying to try to
// take std.math.minInt(i128), drop the minus sign, and convert it to u128
// all at comptime. Instead we just have a test that verifies its correctness.
const min_i128_negated: u128 = 170141183460469231731687303715884105728;

test "min_i128_negated is actually the minimum i128, negated" {
    var min_i128_buf: [64]u8 = undefined;
    const min_i128_str = std.fmt.bufPrint(&min_i128_buf, "{}", .{std.math.minInt(i128)}) catch unreachable;

    var negated_buf: [64]u8 = undefined;
    const negated_str = std.fmt.bufPrint(&negated_buf, "-{}", .{min_i128_negated}) catch unreachable;

    try std.testing.expectEqualStrings(min_i128_str, negated_str);
}

test "basic scope initialization" {
    const gpa = std.testing.allocator;

    var ctx = try ScopeTestContext.init(gpa);
    defer ctx.deinit();

    // Test that we start with one scope (top-level)
    try std.testing.expect(ctx.self.scopes.items.len == 1);
}

test "empty scope has no items" {
    const gpa = std.testing.allocator;

    var ctx = try ScopeTestContext.init(gpa);
    defer ctx.deinit();

    const foo_ident = ctx.env.idents.insert(gpa, base.Ident.for_text("foo"), base.Region.zero());
    const result = ctx.self.scopeLookup(&ctx.env.idents, .ident, foo_ident);

    try std.testing.expectEqual(Scope.LookupResult{ .not_found = {} }, result);
}

test "can add and lookup idents at top level" {
    const gpa = std.testing.allocator;

    var ctx = try ScopeTestContext.init(gpa);
    defer ctx.deinit();

    const foo_ident = ctx.env.idents.insert(gpa, base.Ident.for_text("foo"), base.Region.zero());
    const bar_ident = ctx.env.idents.insert(gpa, base.Ident.for_text("bar"), base.Region.zero());
    const foo_pattern: CIR.Pattern.Idx = @enumFromInt(1);
    const bar_pattern: CIR.Pattern.Idx = @enumFromInt(2);

    // Add identifiers
    const foo_result = ctx.self.scopeIntroduceInternal(gpa, &ctx.env.idents, .ident, foo_ident, foo_pattern, false, true);
    const bar_result = ctx.self.scopeIntroduceInternal(gpa, &ctx.env.idents, .ident, bar_ident, bar_pattern, false, true);

    try std.testing.expectEqual(Scope.IntroduceResult{ .success = {} }, foo_result);
    try std.testing.expectEqual(Scope.IntroduceResult{ .success = {} }, bar_result);

    // Lookup should find them
    const foo_lookup = ctx.self.scopeLookup(&ctx.env.idents, .ident, foo_ident);
    const bar_lookup = ctx.self.scopeLookup(&ctx.env.idents, .ident, bar_ident);

    try std.testing.expectEqual(Scope.LookupResult{ .found = foo_pattern }, foo_lookup);
    try std.testing.expectEqual(Scope.LookupResult{ .found = bar_pattern }, bar_lookup);
}

test "nested scopes shadow outer scopes" {
    const gpa = std.testing.allocator;

    var ctx = try ScopeTestContext.init(gpa);
    defer ctx.deinit();

    const x_ident = ctx.env.idents.insert(gpa, base.Ident.for_text("x"), base.Region.zero());
    const outer_pattern: CIR.Pattern.Idx = @enumFromInt(1);
    const inner_pattern: CIR.Pattern.Idx = @enumFromInt(2);

    // Add x to outer scope
    const outer_result = ctx.self.scopeIntroduceInternal(gpa, &ctx.env.idents, .ident, x_ident, outer_pattern, false, true);
    try std.testing.expectEqual(Scope.IntroduceResult{ .success = {} }, outer_result);

    // Enter new scope
    ctx.self.scopeEnter(gpa, false);

    // x from outer scope should still be visible
    const outer_lookup = ctx.self.scopeLookup(&ctx.env.idents, .ident, x_ident);
    try std.testing.expectEqual(Scope.LookupResult{ .found = outer_pattern }, outer_lookup);

    // Add x to inner scope (shadows outer)
    const inner_result = ctx.self.scopeIntroduceInternal(gpa, &ctx.env.idents, .ident, x_ident, inner_pattern, false, true);
    try std.testing.expectEqual(Scope.IntroduceResult{ .shadowing_warning = outer_pattern }, inner_result);

    // Now x should resolve to inner scope
    const inner_lookup = ctx.self.scopeLookup(&ctx.env.idents, .ident, x_ident);
    try std.testing.expectEqual(Scope.LookupResult{ .found = inner_pattern }, inner_lookup);

    // Exit inner scope
    try ctx.self.scopeExit(gpa);

    // x should resolve to outer scope again
    const after_exit_lookup = ctx.self.scopeLookup(&ctx.env.idents, .ident, x_ident);
    try std.testing.expectEqual(Scope.LookupResult{ .found = outer_pattern }, after_exit_lookup);
}

test "top level var error" {
    const gpa = std.testing.allocator;

    var ctx = try ScopeTestContext.init(gpa);
    defer ctx.deinit();

    const var_ident = ctx.env.idents.insert(gpa, base.Ident.for_text("count_"), base.Region.zero());
    const pattern: CIR.Pattern.Idx = @enumFromInt(1);

    // Should fail to introduce var at top level
    const result = ctx.self.scopeIntroduceInternal(gpa, &ctx.env.idents, .ident, var_ident, pattern, true, true);
    try std.testing.expectEqual(Scope.IntroduceResult{ .top_level_var_error = {} }, result);
}

test "var reassignment within same function" {
    const gpa = std.testing.allocator;

    var ctx = try ScopeTestContext.init(gpa);
    defer ctx.deinit();

    // Enter function scope
    ctx.self.scopeEnter(gpa, true);

    const count_ident = ctx.env.idents.insert(gpa, base.Ident.for_text("count_"), base.Region.zero());
    const pattern1: CIR.Pattern.Idx = @enumFromInt(1);
    const pattern2: CIR.Pattern.Idx = @enumFromInt(2);

    // Declare var
    const declare_result = ctx.self.scopeIntroduceInternal(gpa, &ctx.env.idents, .ident, count_ident, pattern1, true, true);
    try std.testing.expectEqual(Scope.IntroduceResult{ .success = {} }, declare_result);

    // Reassign var (not a declaration)
    const reassign_result = ctx.self.scopeIntroduceInternal(gpa, &ctx.env.idents, .ident, count_ident, pattern2, true, false);
    try std.testing.expectEqual(Scope.IntroduceResult{ .success = {} }, reassign_result);

    // Should resolve to the reassigned value
    const lookup_result = ctx.self.scopeLookup(&ctx.env.idents, .ident, count_ident);
    try std.testing.expectEqual(Scope.LookupResult{ .found = pattern2 }, lookup_result);
}

test "var reassignment across function boundary fails" {
    const gpa = std.testing.allocator;

    var ctx = try ScopeTestContext.init(gpa);
    defer ctx.deinit();

    // Enter first function scope
    ctx.self.scopeEnter(gpa, true);

    const count_ident = ctx.env.idents.insert(gpa, base.Ident.for_text("count_"), base.Region.zero());
    const pattern1: CIR.Pattern.Idx = @enumFromInt(1);
    const pattern2: CIR.Pattern.Idx = @enumFromInt(2);

    // Declare var in first function
    const declare_result = ctx.self.scopeIntroduceInternal(gpa, &ctx.env.idents, .ident, count_ident, pattern1, true, true);
    try std.testing.expectEqual(Scope.IntroduceResult{ .success = {} }, declare_result);

    // Enter second function scope (function boundary)
    ctx.self.scopeEnter(gpa, true);

    // Try to reassign var from different function - should fail
    const reassign_result = ctx.self.scopeIntroduceInternal(gpa, &ctx.env.idents, .ident, count_ident, pattern2, true, false);
    try std.testing.expectEqual(Scope.IntroduceResult{ .var_across_function_boundary = pattern1 }, reassign_result);
}

test "identifiers with and without underscores are different" {
    const gpa = std.testing.allocator;

    var ctx = try ScopeTestContext.init(gpa);
    defer ctx.deinit();

    const sum_ident = ctx.env.idents.insert(gpa, base.Ident.for_text("sum"), base.Region.zero());
    const sum_underscore_ident = ctx.env.idents.insert(gpa, base.Ident.for_text("sum_"), base.Region.zero());
    const pattern1: CIR.Pattern.Idx = @enumFromInt(1);
    const pattern2: CIR.Pattern.Idx = @enumFromInt(2);

    // Enter function scope so we can use var
    ctx.self.scopeEnter(gpa, true);

    // Introduce regular identifier
    const regular_result = ctx.self.scopeIntroduceInternal(gpa, &ctx.env.idents, .ident, sum_ident, pattern1, false, true);
    try std.testing.expectEqual(Scope.IntroduceResult{ .success = {} }, regular_result);

    // Introduce var with underscore - should not conflict
    const var_result = ctx.self.scopeIntroduceInternal(gpa, &ctx.env.idents, .ident, sum_underscore_ident, pattern2, true, true);
    try std.testing.expectEqual(Scope.IntroduceResult{ .success = {} }, var_result);

    // Both should be found independently
    const regular_lookup = ctx.self.scopeLookup(&ctx.env.idents, .ident, sum_ident);
    const var_lookup = ctx.self.scopeLookup(&ctx.env.idents, .ident, sum_underscore_ident);

    try std.testing.expectEqual(Scope.LookupResult{ .found = pattern1 }, regular_lookup);
    try std.testing.expectEqual(Scope.LookupResult{ .found = pattern2 }, var_lookup);
}

test "aliases work separately from idents" {
    const gpa = std.testing.allocator;

    var ctx = try ScopeTestContext.init(gpa);
    defer ctx.deinit();

    const foo_ident = ctx.env.idents.insert(gpa, base.Ident.for_text("Foo"), base.Region.zero());
    const ident_pattern: CIR.Pattern.Idx = @enumFromInt(1);
    const alias_pattern: CIR.Pattern.Idx = @enumFromInt(2);

    // Add as both ident and alias (they're in separate namespaces)
    const ident_result = ctx.self.scopeIntroduceInternal(gpa, &ctx.env.idents, .ident, foo_ident, ident_pattern, false, true);
    const alias_result = ctx.self.scopeIntroduceInternal(gpa, &ctx.env.idents, .alias, foo_ident, alias_pattern, false, true);

    try std.testing.expectEqual(Scope.IntroduceResult{ .success = {} }, ident_result);
    try std.testing.expectEqual(Scope.IntroduceResult{ .success = {} }, alias_result);

    // Both should be found in their respective namespaces
    const ident_lookup = ctx.self.scopeLookup(&ctx.env.idents, .ident, foo_ident);
    const alias_lookup = ctx.self.scopeLookup(&ctx.env.idents, .alias, foo_ident);

    try std.testing.expectEqual(Scope.LookupResult{ .found = ident_pattern }, ident_lookup);
    try std.testing.expectEqual(Scope.LookupResult{ .found = alias_pattern }, alias_lookup);
}

test "hexadecimal integer literals" {
    const test_cases = [_]struct {
        literal: []const u8,
        expected_value: i128,
        expected_sign_needed: bool,
        expected_bits_needed: types.Num.Int.BitsNeeded,
    }{
        // Basic hex literals
        .{ .literal = "0x0", .expected_value = 0, .expected_sign_needed = false, .expected_bits_needed = .@"7" },
        .{ .literal = "0x1", .expected_value = 1, .expected_sign_needed = false, .expected_bits_needed = .@"7" },
        .{ .literal = "0xFF", .expected_value = 255, .expected_sign_needed = false, .expected_bits_needed = .@"8" },
        .{ .literal = "0x100", .expected_value = 256, .expected_sign_needed = false, .expected_bits_needed = .@"9_to_15" },
        .{ .literal = "0xFFFF", .expected_value = 65535, .expected_sign_needed = false, .expected_bits_needed = .@"16" },
        .{ .literal = "0x10000", .expected_value = 65536, .expected_sign_needed = false, .expected_bits_needed = .@"17_to_31" },
        .{ .literal = "0xFFFFFFFF", .expected_value = 4294967295, .expected_sign_needed = false, .expected_bits_needed = .@"32" },
        .{ .literal = "0x100000000", .expected_value = 4294967296, .expected_sign_needed = false, .expected_bits_needed = .@"33_to_63" },
        .{ .literal = "0xFFFFFFFFFFFFFFFF", .expected_value = @as(i128, @bitCast(@as(u128, 18446744073709551615))), .expected_sign_needed = false, .expected_bits_needed = .@"64" },

        // Hex with underscores
        .{ .literal = "0x1_000", .expected_value = 4096, .expected_sign_needed = false, .expected_bits_needed = .@"9_to_15" },
        .{ .literal = "0xFF_FF", .expected_value = 65535, .expected_sign_needed = false, .expected_bits_needed = .@"16" },
        .{ .literal = "0x1234_5678_9ABC_DEF0", .expected_value = @as(i128, @bitCast(@as(u128, 0x123456789ABCDEF0))), .expected_sign_needed = false, .expected_bits_needed = .@"33_to_63" },

        // Negative hex literals
        .{ .literal = "-0x1", .expected_value = -1, .expected_sign_needed = true, .expected_bits_needed = .@"7" },
        .{ .literal = "-0x80", .expected_value = -128, .expected_sign_needed = true, .expected_bits_needed = .@"8" },
        .{ .literal = "-0x81", .expected_value = -129, .expected_sign_needed = true, .expected_bits_needed = .@"8" },
        .{ .literal = "-0x8000", .expected_value = -32768, .expected_sign_needed = true, .expected_bits_needed = .@"16" },
        .{ .literal = "-0x8001", .expected_value = -32769, .expected_sign_needed = true, .expected_bits_needed = .@"16" },
        .{ .literal = "-0x80000000", .expected_value = -2147483648, .expected_sign_needed = true, .expected_bits_needed = .@"32" },
        .{ .literal = "-0x80000001", .expected_value = -2147483649, .expected_sign_needed = true, .expected_bits_needed = .@"32" },
        .{ .literal = "-0x8000000000000000", .expected_value = -9223372036854775808, .expected_sign_needed = true, .expected_bits_needed = .@"64" },
        .{ .literal = "-0x8000000000000001", .expected_value = @as(i128, -9223372036854775809), .expected_sign_needed = true, .expected_bits_needed = .@"64" },
    };

    var gpa_state = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
    defer std.debug.assert(gpa_state.deinit() == .ok);
    const gpa = gpa_state.allocator();

    for (test_cases) |tc| {
        var env = base.ModuleEnv.init(gpa);
        defer env.deinit();

        var ast = parse.parseExpr(&env, tc.literal);
        defer ast.deinit(gpa);

        var cir = CIR.init(&env);
        defer cir.deinit();

        var can = init(&cir, &ast);
        defer can.deinit();

        const expr_idx: parse.AST.Expr.Idx = @enumFromInt(ast.root_node_idx);
        const canonical_expr_idx = can.canonicalize_expr(expr_idx) orelse {
            std.debug.print("Failed to canonicalize: {s}\n", .{tc.literal});
            try std.testing.expect(false);
            continue;
        };

        const expr = cir.store.getExpr(canonical_expr_idx);
        try std.testing.expect(expr == .int);

        // Check the value
        try std.testing.expectEqual(tc.expected_value, expr.int.value.value);

        // Check the requirements
        try std.testing.expectEqual(tc.expected_sign_needed, expr.int.requirements.sign_needed);
        try std.testing.expectEqual(tc.expected_bits_needed, expr.int.requirements.bits_needed);
    }
}

test "binary integer literals" {
    const test_cases = [_]struct {
        literal: []const u8,
        expected_value: i128,
        expected_sign_needed: bool,
        expected_bits_needed: types.Num.Int.BitsNeeded,
    }{
        // Basic binary literals
        .{ .literal = "0b0", .expected_value = 0, .expected_sign_needed = false, .expected_bits_needed = .@"7" },
        .{ .literal = "0b1", .expected_value = 1, .expected_sign_needed = false, .expected_bits_needed = .@"7" },
        .{ .literal = "0b10", .expected_value = 2, .expected_sign_needed = false, .expected_bits_needed = .@"7" },
        .{ .literal = "0b11111111", .expected_value = 255, .expected_sign_needed = false, .expected_bits_needed = .@"8" },
        .{ .literal = "0b100000000", .expected_value = 256, .expected_sign_needed = false, .expected_bits_needed = .@"9_to_15" },
        .{ .literal = "0b1111111111111111", .expected_value = 65535, .expected_sign_needed = false, .expected_bits_needed = .@"16" },
        .{ .literal = "0b10000000000000000", .expected_value = 65536, .expected_sign_needed = false, .expected_bits_needed = .@"17_to_31" },

        // Binary with underscores
        .{ .literal = "0b11_11", .expected_value = 15, .expected_sign_needed = false, .expected_bits_needed = .@"7" },
        .{ .literal = "0b1111_1111", .expected_value = 255, .expected_sign_needed = false, .expected_bits_needed = .@"8" },
        .{ .literal = "0b1010_1010_1010_1010", .expected_value = 43690, .expected_sign_needed = false, .expected_bits_needed = .@"16" },

        // Negative binary literals
        .{ .literal = "-0b1", .expected_value = -1, .expected_sign_needed = true, .expected_bits_needed = .@"7" },
        .{ .literal = "-0b1000000", .expected_value = -64, .expected_sign_needed = true, .expected_bits_needed = .@"7" },
        .{ .literal = "-0b10000000", .expected_value = -128, .expected_sign_needed = true, .expected_bits_needed = .@"8" },
        .{ .literal = "-0b10000001", .expected_value = -129, .expected_sign_needed = true, .expected_bits_needed = .@"8" },
        .{ .literal = "-0b1000000000000000", .expected_value = -32768, .expected_sign_needed = true, .expected_bits_needed = .@"16" },
        .{ .literal = "-0b1000000000000001", .expected_value = -32769, .expected_sign_needed = true, .expected_bits_needed = .@"16" },
    };

    var gpa_state = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
    defer std.debug.assert(gpa_state.deinit() == .ok);
    const gpa = gpa_state.allocator();

    for (test_cases) |tc| {
        var env = base.ModuleEnv.init(gpa);
        defer env.deinit();

        var ast = parse.parseExpr(&env, tc.literal);
        defer ast.deinit(gpa);

        var cir = CIR.init(&env);
        defer cir.deinit();

        var can = init(&cir, &ast);
        defer can.deinit();

        const expr_idx: parse.AST.Expr.Idx = @enumFromInt(ast.root_node_idx);
        const canonical_expr_idx = can.canonicalize_expr(expr_idx) orelse {
            std.debug.print("Failed to canonicalize: {s}\n", .{tc.literal});
            try std.testing.expect(false);
            continue;
        };

        const expr = cir.store.getExpr(canonical_expr_idx);
        try std.testing.expect(expr == .int);

        // Check the value
        try std.testing.expectEqual(tc.expected_value, expr.int.value.value);

        // Check the requirements
        try std.testing.expectEqual(tc.expected_sign_needed, expr.int.requirements.sign_needed);
        try std.testing.expectEqual(tc.expected_bits_needed, expr.int.requirements.bits_needed);
    }
}

test "octal integer literals" {
    const test_cases = [_]struct {
        literal: []const u8,
        expected_value: i128,
        expected_sign_needed: bool,
        expected_bits_needed: types.Num.Int.BitsNeeded,
    }{
        // Basic octal literals
        .{ .literal = "0o0", .expected_value = 0, .expected_sign_needed = false, .expected_bits_needed = .@"7" },
        .{ .literal = "0o1", .expected_value = 1, .expected_sign_needed = false, .expected_bits_needed = .@"7" },
        .{ .literal = "0o7", .expected_value = 7, .expected_sign_needed = false, .expected_bits_needed = .@"7" },
        .{ .literal = "0o10", .expected_value = 8, .expected_sign_needed = false, .expected_bits_needed = .@"7" },
        .{ .literal = "0o377", .expected_value = 255, .expected_sign_needed = false, .expected_bits_needed = .@"8" },
        .{ .literal = "0o400", .expected_value = 256, .expected_sign_needed = false, .expected_bits_needed = .@"9_to_15" },
        .{ .literal = "0o177777", .expected_value = 65535, .expected_sign_needed = false, .expected_bits_needed = .@"16" },
        .{ .literal = "0o200000", .expected_value = 65536, .expected_sign_needed = false, .expected_bits_needed = .@"17_to_31" },

        // Octal with underscores
        .{ .literal = "0o377_377", .expected_value = 130815, .expected_sign_needed = false, .expected_bits_needed = .@"17_to_31" },
        .{ .literal = "0o1_234_567", .expected_value = 342391, .expected_sign_needed = false, .expected_bits_needed = .@"17_to_31" },

        // Negative octal literals
        .{ .literal = "-0o1", .expected_value = -1, .expected_sign_needed = true, .expected_bits_needed = .@"7" },
        .{ .literal = "-0o100", .expected_value = -64, .expected_sign_needed = true, .expected_bits_needed = .@"7" },
        .{ .literal = "-0o200", .expected_value = -128, .expected_sign_needed = true, .expected_bits_needed = .@"8" },
        .{ .literal = "-0o201", .expected_value = -129, .expected_sign_needed = true, .expected_bits_needed = .@"8" },
        .{ .literal = "-0o100000", .expected_value = -32768, .expected_sign_needed = true, .expected_bits_needed = .@"16" },
        .{ .literal = "-0o100001", .expected_value = -32769, .expected_sign_needed = true, .expected_bits_needed = .@"16" },
    };

    var gpa_state = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
    defer std.debug.assert(gpa_state.deinit() == .ok);
    const gpa = gpa_state.allocator();

    for (test_cases) |tc| {
        var env = base.ModuleEnv.init(gpa);
        defer env.deinit();

        var ast = parse.parseExpr(&env, tc.literal);
        defer ast.deinit(gpa);

        var cir = CIR.init(&env);
        defer cir.deinit();

        var can = init(&cir, &ast);
        defer can.deinit();

        const expr_idx: parse.AST.Expr.Idx = @enumFromInt(ast.root_node_idx);
        const canonical_expr_idx = can.canonicalize_expr(expr_idx) orelse {
            std.debug.print("Failed to canonicalize: {s}\n", .{tc.literal});
            try std.testing.expect(false);
            continue;
        };

        const expr = cir.store.getExpr(canonical_expr_idx);
        try std.testing.expect(expr == .int);

        // Check the value
        try std.testing.expectEqual(tc.expected_value, expr.int.value.value);

        // Check the requirements
        try std.testing.expectEqual(tc.expected_sign_needed, expr.int.requirements.sign_needed);
        try std.testing.expectEqual(tc.expected_bits_needed, expr.int.requirements.bits_needed);
    }
}

test "integer literals with uppercase base prefixes" {
    const test_cases = [_]struct {
        literal: []const u8,
        expected_value: i128,
        expected_sign_needed: bool,
        expected_bits_needed: types.Num.Int.BitsNeeded,
    }{
        // Uppercase hex prefix
        .{ .literal = "0X0", .expected_value = 0, .expected_sign_needed = false, .expected_bits_needed = .@"7" },
        .{ .literal = "0X1", .expected_value = 1, .expected_sign_needed = false, .expected_bits_needed = .@"7" },
        .{ .literal = "0XFF", .expected_value = 255, .expected_sign_needed = false, .expected_bits_needed = .@"8" },
        .{ .literal = "0XABCD", .expected_value = 43981, .expected_sign_needed = false, .expected_bits_needed = .@"16" },

        // Uppercase binary prefix
        .{ .literal = "0B0", .expected_value = 0, .expected_sign_needed = false, .expected_bits_needed = .@"7" },
        .{ .literal = "0B1", .expected_value = 1, .expected_sign_needed = false, .expected_bits_needed = .@"7" },
        .{ .literal = "0B1111", .expected_value = 15, .expected_sign_needed = false, .expected_bits_needed = .@"7" },
        .{ .literal = "0B11111111", .expected_value = 255, .expected_sign_needed = false, .expected_bits_needed = .@"8" },

        // Uppercase octal prefix
        .{ .literal = "0O0", .expected_value = 0, .expected_sign_needed = false, .expected_bits_needed = .@"7" },
        .{ .literal = "0O7", .expected_value = 7, .expected_sign_needed = false, .expected_bits_needed = .@"7" },
        .{ .literal = "0O377", .expected_value = 255, .expected_sign_needed = false, .expected_bits_needed = .@"8" },
        .{ .literal = "0O777", .expected_value = 511, .expected_sign_needed = false, .expected_bits_needed = .@"9_to_15" },

        // Mixed case in value (should still work)
        .{ .literal = "0xAbCd", .expected_value = 43981, .expected_sign_needed = false, .expected_bits_needed = .@"16" },
        .{ .literal = "0XaBcD", .expected_value = 43981, .expected_sign_needed = false, .expected_bits_needed = .@"16" },
    };

    var gpa_state = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
    defer std.debug.assert(gpa_state.deinit() == .ok);
    const gpa = gpa_state.allocator();

    for (test_cases) |tc| {
        var env = base.ModuleEnv.init(gpa);
        defer env.deinit();

        var ast = parse.parseExpr(&env, tc.literal);
        defer ast.deinit(gpa);

        var cir = CIR.init(&env);
        defer cir.deinit();

        var can = init(&cir, &ast);
        defer can.deinit();

        const expr_idx: parse.AST.Expr.Idx = @enumFromInt(ast.root_node_idx);
        const canonical_expr_idx = can.canonicalize_expr(expr_idx) orelse {
            std.debug.print("Failed to canonicalize: {s}\n", .{tc.literal});
            try std.testing.expect(false);
            continue;
        };

        const expr = cir.store.getExpr(canonical_expr_idx);
        try std.testing.expect(expr == .int);

        // Check the value
        try std.testing.expectEqual(tc.expected_value, expr.int.value.value);

        // Check the requirements
        try std.testing.expectEqual(tc.expected_sign_needed, expr.int.requirements.sign_needed);
        try std.testing.expectEqual(tc.expected_bits_needed, expr.int.requirements.bits_needed);
    }
}
