const std = @import("std");
const base = @import("../base.zig");
const parse = @import("parse.zig");
const tokenize = @import("parse/tokenize.zig");
const collections = @import("../collections.zig");
const types = @import("../types.zig");
const RocDec = @import("../builtins/dec.zig").RocDec;

const NodeStore = @import("./canonicalize/NodeStore.zig");
const Scope = @import("./canonicalize/Scope.zig");
const Node = @import("./canonicalize/Node.zig");

const AST = parse.AST;
const Token = tokenize.Token;

can_ir: *CIR,
parse_ir: *AST,
scopes: std.ArrayListUnmanaged(Scope) = .{},
/// Stack of function regions for tracking var reassignment across function boundaries
function_regions: std.ArrayListUnmanaged(Region),
/// Maps var patterns to the function region they were declared in
var_function_regions: std.AutoHashMapUnmanaged(CIR.Pattern.Idx, Region),
/// Set of pattern indices that are vars
var_patterns: std.AutoHashMapUnmanaged(CIR.Pattern.Idx, void),
/// Tracks which pattern indices have been used/referenced
used_patterns: std.AutoHashMapUnmanaged(CIR.Pattern.Idx, void),
/// Maps identifier names to pending type annotations awaiting connection to declarations
pending_type_annos: std.ArrayListUnmanaged(PendingTypeAnno),

const PendingTypeAnno = struct {
    ident: base.Ident.Idx,
    anno: CIR.TypeAnno.Idx,
};

const Ident = base.Ident;
const Region = base.Region;
const TagName = base.TagName;
const ModuleEnv = base.ModuleEnv;
const CalledVia = base.CalledVia;
const exitOnOom = collections.utils.exitOnOom;

const TypeVar = types.Var;
const Content = types.Content;
const FlatType = types.FlatType;
const Num = types.Num;
const TagUnion = types.TagUnion;
const Tag = types.Tag;

/// The idx of the builtin Bool
pub const BUILTIN_BOOL: CIR.Pattern.Idx = @enumFromInt(0);
/// The idx of the builtin Box
pub const BUILTIN_BOX: CIR.Pattern.Idx = @enumFromInt(1);
/// The idx of the builtin Decode
pub const BUILTIN_DECODE: CIR.Pattern.Idx = @enumFromInt(2);
/// The idx of the builtin Dict
pub const BUILTIN_DICT: CIR.Pattern.Idx = @enumFromInt(3);
/// The idx of the builtin Encode
pub const BUILTIN_ENCODE: CIR.Pattern.Idx = @enumFromInt(4);
/// The idx of the builtin Hash
pub const BUILTIN_HASH: CIR.Pattern.Idx = @enumFromInt(5);
/// The idx of the builtin Inspect
pub const BUILTIN_INSPECT: CIR.Pattern.Idx = @enumFromInt(6);
/// The idx of the builtin List
pub const BUILTIN_LIST: CIR.Pattern.Idx = @enumFromInt(7);
/// The idx of the builtin Num
pub const BUILTIN_NUM: CIR.Pattern.Idx = @enumFromInt(8);
/// The idx of the builtin Result
pub const BUILTIN_RESULT: CIR.Pattern.Idx = @enumFromInt(9);
/// The idx of the builtin Set
pub const BUILTIN_SET: CIR.Pattern.Idx = @enumFromInt(10);
/// The idx of the builtin Str
pub const BUILTIN_STR: CIR.Pattern.Idx = @enumFromInt(11);

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
    self.used_patterns.deinit(gpa);
    self.pending_type_annos.deinit(gpa);
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
        .used_patterns = std.AutoHashMapUnmanaged(CIR.Pattern.Idx, void){},
        .pending_type_annos = std.ArrayListUnmanaged(PendingTypeAnno){},
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

    // Add built-in types to the type scope
    // TODO: These should ultimately come from the platform/builtin files rather than being hardcoded
    result.addBuiltinType(self, "Bool");
    result.addBuiltinType(self, "Str");
    result.addBuiltinType(self, "U8");
    result.addBuiltinType(self, "U16");
    result.addBuiltinType(self, "U32");
    result.addBuiltinType(self, "U64");
    result.addBuiltinType(self, "U128");
    result.addBuiltinType(self, "I8");
    result.addBuiltinType(self, "I16");
    result.addBuiltinType(self, "I32");
    result.addBuiltinType(self, "I64");
    result.addBuiltinType(self, "I128");
    result.addBuiltinType(self, "F32");
    result.addBuiltinType(self, "F64");
    result.addBuiltinType(self, "Dec");
    result.addBuiltinType(self, "List");
    result.addBuiltinType(self, "Dict");
    result.addBuiltinType(self, "Set");
    result.addBuiltinType(self, "Result");
    result.addBuiltinType(self, "Box");

    // Set type for builtin Bool. In the future this will come from the builtin
    // module. Also, it will be a nominal type, not a simple tag union
    const bool_ext = self.pushTypeVar(
        Content{ .structure = .empty_tag_union },
        @enumFromInt(@intFromEnum(BUILTIN_BOOL)),
        Region.zero(),
    );
    _ = self.setTypeVarAtPat(
        BUILTIN_BOOL,
        self.env.types.mkBool(self.env.gpa, &self.env.idents, bool_ext),
    );

    return result;
}

fn addBuiltin(self: *Self, ir: *CIR, ident_text: []const u8, idx: CIR.Pattern.Idx) void {
    const gpa = ir.env.gpa;
    const ident_store = &ir.env.idents;
    const ident_add = ir.env.idents.insert(gpa, base.Ident.for_text(ident_text), Region.zero());
    const pattern_idx_add = ir.store.addPattern(CIR.Pattern{ .assign = .{ .ident = ident_add, .region = Region.zero() } });
    _ = self.scopeIntroduceInternal(gpa, ident_store, .ident, ident_add, pattern_idx_add, false, true);
    std.debug.assert(idx == pattern_idx_add);

    // TODO: Set correct type for builtins? But these types should ultimately
    // come from the builtins roc files, so maybe the resolve stage will handle?
    _ = ir.setTypeVarAtPat(pattern_idx_add, Content{ .flex_var = null });
}

fn addBuiltinType(self: *Self, ir: *CIR, type_name: []const u8) void {
    const gpa = ir.env.gpa;
    const type_ident = ir.env.idents.insert(gpa, base.Ident.for_text(type_name), Region.zero());

    // Create a type header for the built-in type
    const header_idx = ir.store.addTypeHeader(.{
        .name = type_ident,
        .args = .{ .span = .{ .start = 0, .len = 0 } }, // No type parameters for built-ins
        .region = Region.zero(),
    });

    // Create a type annotation that refers to itself (built-in types are primitive)
    const anno_idx = ir.store.addTypeAnno(.{ .ty = .{
        .symbol = type_ident,
        .region = Region.zero(),
    } });

    // Create the type declaration statement
    const type_decl_stmt = CIR.Statement{
        .s_type_decl = .{
            .header = header_idx,
            .anno = anno_idx,
            .where = null,
            .region = Region.zero(),
        },
    };

    const type_decl_idx = ir.store.addStatement(type_decl_stmt);

    // Add to scope without any error checking (built-ins are always valid)
    const current_scope = &self.scopes.items[self.scopes.items.len - 1];
    current_scope.put(gpa, .type_decl, type_ident, type_decl_idx);
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
) std.mem.Allocator.Error!void {
    const file = self.parse_ir.store.getFile();

    // canonicalize_header_packages();

    // Track the start of scratch defs and statements
    const scratch_defs_start = self.can_ir.store.scratchDefTop();
    const scratch_statements_start = self.can_ir.store.scratch_statements.top();

    // First pass: Process all type declarations to introduce them into scope
    for (self.parse_ir.store.statementSlice(file.statements)) |stmt_id| {
        const stmt = self.parse_ir.store.getStatement(stmt_id);
        switch (stmt) {
            .type_decl => |type_decl| {
                // Canonicalize the type declaration header first
                const header_idx = self.canonicalize_type_header(type_decl.header);

                // Process type parameters and annotation in a separate scope
                const anno_idx = blk: {
                    // Enter a new scope for type parameters
                    self.scopeEnter(self.can_ir.env.gpa, false);
                    defer self.scopeExit(self.can_ir.env.gpa) catch {};

                    // Introduce type parameters from the header into the scope
                    self.introduceTypeParametersFromHeader(header_idx);

                    // Now canonicalize the type annotation with type parameters in scope
                    break :blk self.canonicalize_type_anno(type_decl.anno);
                };

                // Create the CIR type declaration statement
                const region = self.parse_ir.tokenizedRegionToRegion(type_decl.region);
                const cir_type_decl = CIR.Statement{
                    .s_type_decl = .{
                        .header = header_idx,
                        .anno = anno_idx,
                        .where = null, // TODO: implement where clauses
                        .region = region,
                    },
                };

                const type_decl_idx = self.can_ir.store.addStatement(cir_type_decl);
                self.can_ir.store.addScratchStatement(type_decl_idx);

                // Introduce the type name into scope (now truly outside the parameter scope)
                const header = self.can_ir.store.getTypeHeader(header_idx);
                self.scopeIntroduceTypeDecl(header.name, type_decl_idx, region);
            },
            else => {
                // Skip non-type-declaration statements in first pass
            },
        }
    }

    // Second pass: Process all other statements
    for (self.parse_ir.store.statementSlice(file.statements)) |stmt_id| {
        const stmt = self.parse_ir.store.getStatement(stmt_id);
        switch (stmt) {
            .import => |import_stmt| {
                _ = self.canonicalizeImportStatement(import_stmt);
            },
            .decl => |decl| {
                const def_idx = try self.canonicalize_decl(decl);
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
            .type_decl => {
                // Already processed in first pass, skip
            },
            .type_anno => |ta| {
                const gpa = self.can_ir.env.gpa;
                const region = self.parse_ir.tokenizedRegionToRegion(ta.region);

                // Top-level type annotation - store for connection to next declaration
                const name = self.parse_ir.tokens.resolveIdentifier(ta.name) orelse {
                    // Malformed identifier - skip this annotation
                    const feature = self.can_ir.env.strings.insert(gpa, "handle malformed identifier for a type annotation");
                    self.can_ir.pushDiagnostic(CIR.Diagnostic{
                        .not_implemented = .{
                            .feature = feature,
                            .region = region,
                        },
                    });
                    continue;
                };

                // First, extract all type variables from the AST annotation
                var type_vars = std.ArrayList(Ident.Idx).init(self.can_ir.env.gpa);
                defer type_vars.deinit();

                // Extract type variables from the AST annotation
                self.extractTypeVarsFromASTAnno(ta.anno, &type_vars);

                // Enter a new scope for type variables
                self.scopeEnter(self.can_ir.env.gpa, false);
                defer self.scopeExit(self.can_ir.env.gpa) catch {};

                // Introduce type variables into scope
                for (type_vars.items) |type_var| {
                    // Create a dummy type annotation for the type variable
                    const dummy_anno = self.can_ir.store.addTypeAnno(.{
                        .ty_var = .{
                            .name = type_var,
                            .region = region, // TODO we may want to use the region for the type_var instead of the whole annotation
                        },
                    });
                    self.scopeIntroduceTypeVar(type_var, dummy_anno);
                }

                // Now canonicalize the annotation with type variables in scope
                const anno_idx = self.canonicalize_type_anno(ta.anno);

                // Store annotation for connection to next declaration
                self.pending_type_annos.append(self.can_ir.env.gpa, .{ .ident = name, .anno = anno_idx }) catch |err| exitOnOom(err);
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

    // Create the span of all top-level defs and statements
    self.can_ir.all_defs = self.can_ir.store.defSpanFrom(scratch_defs_start);
    self.can_ir.all_statements = self.can_ir.store.statementSpanFrom(scratch_statements_start);
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
            .malformed => |malformed| {
                // Malformed exposed items are already captured as diagnostics during parsing
                _ = malformed;
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

/// Canonicalize an import statement, handling both top-level file imports and statement imports
fn canonicalizeImportStatement(
    self: *Self,
    import_stmt: @TypeOf(@as(AST.Statement, undefined).import),
) ?CIR.Statement.Idx {
    // 1. Reconstruct the full module name (e.g., "json.Json")
    const module_name = blk: {
        if (self.parse_ir.tokens.resolveIdentifier(import_stmt.module_name_tok) == null) {
            const region = self.parse_ir.tokenizedRegionToRegion(import_stmt.region);
            const feature = self.can_ir.env.strings.insert(self.can_ir.env.gpa, "resolve import module name token");
            self.can_ir.pushDiagnostic(CIR.Diagnostic{ .not_implemented = .{
                .feature = feature,
                .region = region,
            } });
            return null;
        }

        if (import_stmt.qualifier_tok) |qualifier_tok| {
            if (self.parse_ir.tokens.resolveIdentifier(qualifier_tok) == null) {
                const region = self.parse_ir.tokenizedRegionToRegion(import_stmt.region);
                const feature = self.can_ir.env.strings.insert(self.can_ir.env.gpa, "resolve import qualifier token");
                self.can_ir.pushDiagnostic(CIR.Diagnostic{ .not_implemented = .{
                    .feature = feature,
                    .region = region,
                } });
                return null;
            }

            // Slice from original source to get "qualifier.ModuleName"
            const qualifier_region = self.parse_ir.tokens.resolve(qualifier_tok);
            const module_region = self.parse_ir.tokens.resolve(import_stmt.module_name_tok);
            const full_name = self.parse_ir.source[qualifier_region.start.offset..module_region.end.offset];
            break :blk self.can_ir.env.idents.insert(self.can_ir.env.gpa, base.Ident.for_text(full_name), Region.zero());
        } else {
            // No qualifier, just use the module name directly
            break :blk self.parse_ir.tokens.resolveIdentifier(import_stmt.module_name_tok).?;
        }
    };

    // 2. Determine the alias (either explicit or default to last part)
    const alias = self.resolveModuleAlias(import_stmt.alias_tok, module_name) orelse return null;

    // 3. Add to scope: alias -> module_name mapping
    self.scopeIntroduceModuleAlias(alias, module_name);

    // 4. Convert exposed items and introduce them into scope
    const cir_exposes = self.convertASTExposesToCIR(import_stmt.exposes);
    self.introduceExposedItemsIntoScope(cir_exposes, module_name);

    // 5. Create CIR import statement
    const cir_import = CIR.Statement{
        .s_import = .{
            .module_name_tok = module_name,
            .qualifier_tok = if (import_stmt.qualifier_tok) |q_tok| self.parse_ir.tokens.resolveIdentifier(q_tok) else null,
            .alias_tok = if (import_stmt.alias_tok) |a_tok| self.parse_ir.tokens.resolveIdentifier(a_tok) else null,
            .exposes = cir_exposes,
            .region = self.parse_ir.tokenizedRegionToRegion(import_stmt.region),
        },
    };

    const import_idx = self.can_ir.store.addStatement(cir_import);
    self.can_ir.store.addScratchStatement(import_idx);
    return import_idx;
}

/// Resolve the module alias name from either explicit alias or module name
fn resolveModuleAlias(
    self: *Self,
    alias_tok: ?Token.Idx,
    module_name: Ident.Idx,
) ?Ident.Idx {
    if (alias_tok) |alias_token| {
        return self.parse_ir.tokens.resolveIdentifier(alias_token);
    } else {
        // Extract last part from module name - e.g., "Json" from "json.Json"
        return self.extractModuleName(module_name);
    }
}

/// Create a qualified name by combining module and field names (e.g., "json.Json.utf8")
fn createQualifiedName(
    self: *Self,
    module_name: Ident.Idx,
    field_name: Ident.Idx,
) Ident.Idx {
    const module_text = self.can_ir.env.idents.getText(module_name);
    const field_text = self.can_ir.env.idents.getText(field_name);

    // Allocate space for "module.field" - this case still needs allocation since we're combining
    // module name from import with field name from usage site
    const qualified_text = std.fmt.allocPrint(self.can_ir.env.gpa, "{s}.{s}", .{ module_text, field_text }) catch |err| exitOnOom(err);
    defer self.can_ir.env.gpa.free(qualified_text);

    return self.can_ir.env.idents.insert(self.can_ir.env.gpa, base.Ident.for_text(qualified_text), Region.zero());
}

/// Create an external declaration for a qualified name
fn createExternalDeclaration(
    self: *Self,
    qualified_name: Ident.Idx,
    module_name: Ident.Idx,
    local_name: Ident.Idx,
    kind: CIR.ExternalDecl.kind,
    region: Region,
) CIR.ExternalDecl.Idx {
    const external_decl = CIR.ExternalDecl{
        .qualified_name = qualified_name,
        .module_name = module_name,
        .local_name = local_name,
        .type_var = self.can_ir.pushFreshTypeVar(@enumFromInt(0), region),
        .kind = kind,
        .region = region,
    };

    return self.can_ir.pushExternalDecl(external_decl);
}

/// Convert AST exposed items to CIR exposed items
fn convertASTExposesToCIR(
    self: *Self,
    ast_exposes: AST.ExposedItem.Span,
) CIR.ExposedItem.Span {
    const scratch_start = self.can_ir.store.scratchExposedItemTop();

    const ast_exposed_slice = self.parse_ir.store.exposedItemSlice(ast_exposes);
    for (ast_exposed_slice) |ast_exposed_idx| {
        const ast_exposed = self.parse_ir.store.getExposedItem(ast_exposed_idx);

        // Convert AST exposed item to CIR exposed item
        const cir_exposed = convert_item: {
            // Extract identifier token and alias token
            const ident_token, const alias_token, const is_wildcard = switch (ast_exposed) {
                .lower_ident => |ident| .{ ident.ident, ident.as, false },
                .upper_ident => |ident| .{ ident.ident, ident.as, false },
                .upper_ident_star => |star_ident| .{ star_ident.ident, null, true },
                .malformed => |_| continue, // Skip malformed exposed items
            };

            // Resolve the main identifier name
            const name = resolve_ident: {
                if (self.parse_ir.tokens.resolveIdentifier(ident_token)) |resolved| {
                    break :resolve_ident resolved;
                } else {
                    break :resolve_ident self.can_ir.env.idents.insert(self.can_ir.env.gpa, base.Ident.for_text("unknown"), base.Region.zero());
                }
            };

            // Resolve the alias if present
            const alias = resolve_alias: {
                if (alias_token) |as_token| {
                    if (self.parse_ir.tokens.resolveIdentifier(as_token)) |resolved| {
                        break :resolve_alias resolved;
                    } else {
                        break :resolve_alias self.can_ir.env.idents.insert(self.can_ir.env.gpa, base.Ident.for_text("unknown"), base.Region.zero());
                    }
                } else {
                    break :resolve_alias null;
                }
            };

            break :convert_item CIR.ExposedItem{
                .name = name,
                .alias = alias,
                .is_wildcard = is_wildcard,
            };
        };

        const cir_exposed_idx = self.can_ir.store.addExposedItem(cir_exposed);
        self.can_ir.store.addScratchExposedItem(cir_exposed_idx);
    }

    return self.can_ir.store.exposedItemSpanFrom(scratch_start);
}

/// Introduce converted exposed items into scope for identifier resolution
fn introduceExposedItemsIntoScope(
    self: *Self,
    exposed_items_span: CIR.ExposedItem.Span,
    module_name: Ident.Idx,
) void {
    const exposed_items_slice = self.can_ir.store.sliceExposedItems(exposed_items_span);

    for (exposed_items_slice) |exposed_item_idx| {
        const exposed_item = self.can_ir.store.getExposedItem(exposed_item_idx);

        // Use the alias if provided, otherwise use the original name for the local lookup
        const item_name = exposed_item.alias orelse exposed_item.name;

        // Create the exposed item info with module name and original name
        const item_info = Scope.ExposedItemInfo{
            .module_name = module_name,
            .original_name = exposed_item.name, // Always use the original name for module lookup
        };

        // Introduce the exposed item into scope
        // This allows `decode` to resolve to `json.Json.decode`
        self.scopeIntroduceExposedItem(item_name, item_info);
    }
}

fn canonicalize_decl(
    self: *Self,
    decl: AST.Statement.Decl,
) std.mem.Allocator.Error!CIR.Def.Idx {
    const pattern_region = self.parse_ir.tokenizedRegionToRegion(self.parse_ir.store.getPattern(decl.pattern).to_tokenized_region());
    const expr_region = self.parse_ir.tokenizedRegionToRegion(self.parse_ir.store.getExpr(decl.body).to_tokenized_region());

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
        if (try self.canonicalize_expr(decl.body)) |idx| {
            break :blk idx;
        } else {
            const malformed_idx = self.can_ir.pushMalformed(CIR.Expr.Idx, CIR.Diagnostic{ .expr_not_canonicalized = .{
                .region = expr_region,
            } });
            break :blk malformed_idx;
        }
    };

    // Check for pending type annotation to connect to this declaration
    var annotation: ?CIR.Annotation.Idx = null;

    // Extract identifier from pattern if it's an identifier pattern
    const pattern = self.can_ir.store.getPattern(pattern_idx);
    if (pattern == .assign) {
        const ident = pattern.assign.ident;

        // Check if there's a pending type annotation for this identifier
        var found_anno_idx: ?CIR.TypeAnno.Idx = null;
        var found_index: ?usize = null;
        for (self.pending_type_annos.items, 0..) |pending, i| {
            if (self.can_ir.env.idents.identsHaveSameText(ident, pending.ident)) {
                found_anno_idx = pending.anno;
                found_index = i;
                break;
            }
        }

        if (found_anno_idx) |type_anno_idx| {
            // Create a basic annotation from the type annotation
            const type_var = self.can_ir.pushFreshTypeVar(@enumFromInt(0), pattern_region);

            // For now, create a simple annotation with just the signature and region
            // TODO: Convert TypeAnno to proper type constraints and populate signature
            annotation = self.createAnnotationFromTypeAnno(type_anno_idx, type_var, pattern_region);

            // Remove the used annotation
            _ = self.pending_type_annos.swapRemove(found_index.?);
        }
    }

    // Create the def entry
    const def_idx = self.can_ir.store.addDef(.{
        .pattern = pattern_idx,
        .pattern_region = pattern_region,
        .expr = expr_idx,
        .expr_region = expr_region,
        .annotation = annotation,
        .kind = .let,
    });
    // Set def type variable: use annotation signature if available, otherwise flex
    //
    // This is for proper type checking:
    // 1. When there's an annotation, it's the programmer's explicit type declaration and should be ground truth
    // 2. We copy the annotation's signature content to the def's type variable, ensuring the annotation type is authoritative
    // 3. During type inference, the lambda implementation will be unified against this annotation type
    // 4. If the implementation doesn't match the annotation, it will be caught as a type error
    // 5. The final resolved type will be the annotation type (assuming type checking passes)
    // 6. When there's no annotation, we use a flex variable for normal type inference
    if (annotation) |anno_idx| {
        const anno = self.can_ir.store.getAnnotation(anno_idx);
        const signature_resolved = self.can_ir.env.types.resolveVar(anno.signature);

        _ = self.can_ir.setTypeVarAtDef(def_idx, signature_resolved.desc.content);
    } else {
        _ = self.can_ir.setTypeVarAtDef(def_idx, Content{ .flex_var = null });
    }

    return def_idx;
}

fn canonicalize_record_field(
    self: *Self,
    ast_field_idx: AST.RecordField.Idx,
) std.mem.Allocator.Error!?CIR.RecordField.Idx {
    const field = self.parse_ir.store.getRecordField(ast_field_idx);

    // Canonicalize the field name
    const name = self.parse_ir.tokens.resolveIdentifier(field.name) orelse {
        return null;
    };

    // Canonicalize the field value
    const value = if (field.value) |v|
        try self.canonicalize_expr(v) orelse return null
    else blk: {
        // Shorthand syntax: create implicit identifier expression
        // For { name, age }, this creates an implicit identifier lookup for "name" etc.
        const ident_expr = AST.Expr{
            .ident = .{
                .token = field.name,
                .qualifier = null,
                .region = field.region,
            },
        };
        const ident_expr_idx = self.parse_ir.store.addExpr(ident_expr);
        break :blk try self.canonicalize_expr(ident_expr_idx) orelse return null;
    };

    // Create the CIR record field
    const cir_field = CIR.RecordField{
        .name = name,
        .value = value,
    };

    return self.can_ir.store.addRecordField(cir_field);
}

/// Canonicalize an expression.
pub fn canonicalize_expr(
    self: *Self,
    ast_expr_idx: AST.Expr.Idx,
) std.mem.Allocator.Error!?CIR.Expr.Idx {
    const expr = self.parse_ir.store.getExpr(ast_expr_idx);

    switch (expr) {
        .apply => |e| {
            // Mark the start of scratch expressions
            const scratch_top = self.can_ir.store.scratchExprTop();

            // Canonicalize the function being called and add as first element
            const fn_expr = try self.canonicalize_expr(e.@"fn") orelse {
                self.can_ir.store.clearScratchExprsFrom(scratch_top);
                return null;
            };
            self.can_ir.store.addScratchExpr(fn_expr);

            // Canonicalize and add all arguments
            const args_slice = self.parse_ir.store.exprSlice(e.args);
            for (args_slice) |arg| {
                if (try self.canonicalize_expr(arg)) |canonicalized_arg_expr_idx| {
                    self.can_ir.store.addScratchExpr(canonicalized_arg_expr_idx);
                }
            }

            // Create span from scratch expressions
            const args_span = self.can_ir.store.exprSpanFrom(scratch_top);

            const region = self.parse_ir.tokenizedRegionToRegion(e.region);

            // Reserve slot for call expression and create effect variable with proper node correspondence
            const final_expr_idx = self.can_ir.store.predictNodeIndex(2);
            const effect_var = self.can_ir.pushTypeVar(Content{ .pure = {} }, final_expr_idx, region);

            const expr_idx = self.can_ir.store.addExpr(CIR.Expr{
                .e_call = .{
                    .args = args_span,
                    .called_via = CalledVia.apply,
                    .effect_var = effect_var,
                    .region = region,
                },
            });
            // Insert flex type variable
            _ = self.can_ir.setTypeVarAtExpr(expr_idx, Content{ .flex_var = null });

            return expr_idx;
        },
        .ident => |e| {
            const region = self.parse_ir.tokenizedRegionToRegion(e.region);
            if (self.parse_ir.tokens.resolveIdentifier(e.token)) |ident| {
                // Check if this is a module-qualified identifier
                if (e.qualifier) |qualifier_tok| {
                    if (self.parse_ir.tokens.resolveIdentifier(qualifier_tok)) |module_alias| {
                        // Check if this is a module alias
                        if (self.scopeLookupModule(module_alias)) |module_name| {
                            // This is a module-qualified lookup
                            // Create qualified name for external declaration
                            const module_text = self.can_ir.env.idents.getText(module_name);
                            const field_text = self.can_ir.env.idents.getText(ident);

                            // Allocate space for qualified name
                            const qualified_text = std.fmt.allocPrint(self.can_ir.env.gpa, "{s}.{s}", .{ module_text, field_text }) catch |err| collections.utils.exitOnOom(err);
                            defer self.can_ir.env.gpa.free(qualified_text);

                            const qualified_name = self.can_ir.env.idents.insert(self.can_ir.env.gpa, base.Ident.for_text(qualified_text), Region.zero());

                            // Create external declaration
                            const external_decl = CIR.ExternalDecl{
                                .qualified_name = qualified_name,
                                .module_name = module_name,
                                .local_name = ident,
                                .type_var = self.can_ir.pushFreshTypeVar(@enumFromInt(0), region),
                                .kind = .value,
                                .region = region,
                            };

                            const external_idx = self.can_ir.pushExternalDecl(external_decl);

                            // Create lookup expression for external declaration
                            const expr_idx = self.can_ir.store.addExpr(CIR.Expr{ .e_lookup = .{ .external = external_idx } });
                            _ = self.can_ir.setTypeVarAtExpr(expr_idx, Content{ .flex_var = null });
                            return expr_idx;
                        }
                    }
                }

                // Not a module-qualified lookup, or qualifier not found, proceed with normal lookup
                switch (self.scopeLookup(&self.can_ir.env.idents, .ident, ident)) {
                    .found => |pattern_idx| {
                        // Mark this pattern as used for unused variable checking
                        self.used_patterns.put(self.can_ir.env.gpa, pattern_idx, {}) catch |err| exitOnOom(err);

                        // Check if this is a used underscore variable
                        self.checkUsedUnderscoreVariable(ident, region);

                        // We found the ident in scope, lookup to reference the pattern
                        const expr_idx =
                            self.can_ir.store.addExpr(CIR.Expr{ .e_lookup = .{ .local = .{
                                .pattern_idx = pattern_idx,
                                .region = region,
                            } } });
                        _ = self.can_ir.setTypeVarAtExpr(expr_idx, Content{ .flex_var = null });
                        return expr_idx;
                    },
                    .not_found => {
                        // Check if this identifier is an exposed item from an import
                        if (self.scopeLookupExposedItem(ident)) |exposed_info| {
                            // Create qualified name using the original name, not the alias
                            const qualified_name = self.createQualifiedName(exposed_info.module_name, exposed_info.original_name);

                            // Create external declaration for the exposed item
                            const external_decl = CIR.ExternalDecl{
                                .qualified_name = qualified_name,
                                .module_name = exposed_info.module_name,
                                .local_name = ident,
                                .type_var = self.can_ir.pushFreshTypeVar(@enumFromInt(0), region),
                                .kind = .value,
                                .region = region,
                            };

                            const external_idx = self.can_ir.pushExternalDecl(external_decl);

                            // Create lookup expression for external declaration
                            const expr_idx = self.can_ir.store.addExpr(CIR.Expr{ .e_lookup = .{ .external = external_idx } });
                            _ = self.can_ir.setTypeVarAtExpr(expr_idx, Content{ .flex_var = null });
                            return expr_idx;
                        }

                        // We did not find the ident in scope or as an exposed item
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
            const region = self.parse_ir.tokenizedRegionToRegion(e.region);

            // Resolve to a string slice from the source
            const token_text = self.parse_ir.resolve(e.token);

            // Parse the integer value
            const is_negated = token_text[0] == '-'; // Drop the negation for now, so all valid literals fit in u128
            const after_minus_sign = @as(usize, @intFromBool(is_negated));

            // The index the first *actual* digit (after minus sign, "0x" prefix, etc.) in the token
            var first_digit: usize = undefined;

            const DEFAULT_BASE: u8 = 10; // default to base-10, naturally
            var int_base: u8 = undefined;

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
                        int_base = DEFAULT_BASE;
                        first_digit = after_minus_sign;
                    },
                }
            } else {
                int_base = DEFAULT_BASE;
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

            // Calculate requirements based on the value
            // Special handling for minimum signed values (-128, -32768, etc.)
            // These are special because they have a power-of-2 magnitude that fits exactly
            // in their signed type. We report them as needing one less bit to make the
            // standard "signed types have n-1 usable bits" logic work correctly.
            const is_negative_u1 = @as(u1, @intFromBool(is_negated));
            const is_power_of_2 = @as(u1, @intFromBool(u128_val != 0 and (u128_val & (u128_val - 1)) == 0));
            const is_minimum_signed = is_negative_u1 & is_power_of_2;
            const adjusted_val = u128_val - is_minimum_signed;

            const requirements = types.Num.Int.Requirements{
                .sign_needed = is_negated,
                .bits_needed = types.Num.Int.BitsNeeded.fromValue(adjusted_val),
            };

            const int_requirements = types.Num.IntRequirements{
                .sign_needed = requirements.sign_needed,
                .bits_needed = @intCast(@intFromEnum(requirements.bits_needed)),
            };

            // For non-decimal integers (hex, binary, octal), use int_poly directly
            // For decimal integers, use num_poly so they can be either Int or Frac
            const is_non_decimal = int_base != DEFAULT_BASE;

            // Add the expression
            const expr_idx = self.can_ir.store.addExpr(CIR.Expr{
                .e_int = .{
                    .value = .{ .bytes = @bitCast(i128_val), .kind = .i128 },
                    .region = region,
                },
            });

            // Insert concrete type variable
            const type_content = if (is_non_decimal)
                Content{ .structure = .{ .num = .{ .int_unbound = int_requirements } } }
            else
                Content{ .structure = .{ .num = .{ .num_unbound = int_requirements } } };
            _ = self.can_ir.setTypeVarAtExpr(expr_idx, type_content);

            return expr_idx;
        },
        .frac => |e| {
            const region = self.parse_ir.tokenizedRegionToRegion(e.region);

            // Resolve to a string slice from the source
            const token_text = self.parse_ir.resolve(e.token);

            const parsed = parseFracLiteral(token_text) catch |err| switch (err) {
                error.InvalidNumLiteral => {
                    const expr_idx = self.can_ir.pushMalformed(CIR.Expr.Idx, CIR.Diagnostic{ .invalid_num_literal = .{
                        .region = region,
                    } });
                    return expr_idx;
                },
            };

            // Parse the literal first to get requirements
            const requirements = switch (parsed) {
                .small => |small_info| small_info.requirements,
                .dec => |dec_info| dec_info.requirements,
                .f64 => |f64_info| f64_info.requirements,
            };

            const frac_requirements = types.Num.FracRequirements{
                .fits_in_f32 = requirements.fits_in_f32,
                .fits_in_dec = requirements.fits_in_dec,
            };

            const cir_expr = switch (parsed) {
                .small => |small_info| CIR.Expr{
                    .e_dec_small = .{
                        .numerator = small_info.numerator,
                        .denominator_power_of_ten = small_info.denominator_power_of_ten,
                        .region = region,
                    },
                },
                .dec => |dec_info| CIR.Expr{
                    .e_frac_dec = .{
                        .value = dec_info.value,
                        .region = region,
                    },
                },
                .f64 => |f64_info| CIR.Expr{
                    .e_frac_f64 = .{
                        .value = f64_info.value,
                        .region = region,
                    },
                },
            };

            const expr_idx = self.can_ir.store.addExpr(cir_expr);

            // Insert concrete type variable
            _ = self.can_ir.setTypeVarAtExpr(expr_idx, Content{ .structure = .{ .num = .{ .frac_unbound = frac_requirements } } });

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
            const str_segments_span = try self.extractStringSegments(parts);

            const expr_idx = self.can_ir.store.addExpr(CIR.Expr{ .e_str = .{
                .span = str_segments_span,
                .region = self.parse_ir.tokenizedRegionToRegion(e.region),
            } });

            // Insert concrete type variable
            _ = self.can_ir.setTypeVarAtExpr(expr_idx, Content{ .structure = .str });

            return expr_idx;
        },
        .list => |e| {
            const region = self.parse_ir.tokenizedRegionToRegion(e.region);

            // Empty lists get the .list_unbound type
            const items_slice = self.parse_ir.store.exprSlice(e.items);
            if (items_slice.len == 0) {
                // Empty list - use e_empty_list
                const expr_idx = self.can_ir.store.addExpr(CIR.Expr{
                    .e_empty_list = .{
                        .region = region,
                    },
                });

                // Insert concrete type variable as list_unbound
                _ = self.can_ir.setTypeVarAtExpr(
                    expr_idx,
                    Content{ .structure = .list_unbound },
                );

                return expr_idx;
            }

            // Mark the start of scratch expressions for the list
            const scratch_top = self.can_ir.store.scratchExprTop();

            // Iterate over the list item, canonicalizing each one
            // Then append the result to the scratch list
            for (items_slice) |item| {
                if (try self.canonicalize_expr(item)) |canonicalized| {
                    self.can_ir.store.addScratchExpr(canonicalized);
                }
            }

            // Create span of the new scratch expressions
            const elems_span = self.can_ir.store.exprSpanFrom(scratch_top);

            // If all elements failed to canonicalize, treat as empty list
            if (elems_span.span.len == 0) {
                // All elements failed to canonicalize - create empty list
                const expr_idx = self.can_ir.store.addExpr(CIR.Expr{
                    .e_empty_list = .{
                        .region = region,
                    },
                });

                // Insert concrete type variable as list_unbound
                _ = self.can_ir.setTypeVarAtExpr(
                    expr_idx,
                    Content{ .structure = .list_unbound },
                );

                return expr_idx;
            }

            // Initialize the list's type variable to its first element's CIR Index
            // (later steps will unify that type with the other elems' types)
            const first_elem_idx = self.can_ir.store.sliceExpr(elems_span)[0];
            const elem_type_var = @as(TypeVar, @enumFromInt(@intFromEnum(first_elem_idx)));
            const expr_idx = self.can_ir.store.addExpr(CIR.Expr{
                .e_list = .{
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
                const region = self.parse_ir.tokenizedRegionToRegion(e.region);

                // create type vars, first "reserve" node slots
                const final_expr_idx = self.can_ir.store.predictNodeIndex(2);

                // then insert the type vars, setting the parent to be the final slot
                const poly_var = self.can_ir.pushFreshTypeVar(final_expr_idx, region);

                // then in the final slot the actual expr is inserted
                const expr_idx = self.can_ir.store.addExpr(CIR.Expr{
                    .e_tag = .{
                        .ext_var = poly_var,
                        .name = tag_name,
                        .args = .{ .span = .{ .start = 0, .len = 0 } }, // empty arguments
                        .region = region,
                    },
                });

                std.debug.assert(@intFromEnum(expr_idx) == @intFromEnum(final_expr_idx));

                // Insert concrete type variable
                const tag_union = self.can_ir.env.types.mkTagUnion(
                    &[_]Tag{Tag{ .name = tag_name, .args = types.Var.SafeList.Range.empty }},
                    poly_var,
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
        .tuple => |e| {
            const region = self.parse_ir.tokenizedRegionToRegion(e.region);

            // Mark the start of scratch expressions for the tuple
            const scratch_top = self.can_ir.store.scratchExprTop();

            // Iterate over the tuple items, canonicalizing each one
            // Then append the result to the scratch list
            const items_slice = self.parse_ir.store.exprSlice(e.items);
            const elems_var_top = self.can_ir.env.types.tuple_elems.len();
            for (items_slice) |item| {
                if (try self.canonicalize_expr(item)) |canonicalized| {
                    self.can_ir.store.addScratchExpr(canonicalized);
                    _ = self.can_ir.env.types.appendTupleElem(@enumFromInt(@intFromEnum(canonicalized)));
                }
            }
            const elems_var_range = types.Var.SafeList.Range{
                .start = @enumFromInt(elems_var_top),
                .end = @enumFromInt(self.can_ir.env.types.tuple_elems.len()),
            };

            // Create span of the new scratch expressions
            const elems_span = self.can_ir.store.exprSpanFrom(scratch_top);

            // then in the final slot the actual expr is inserted
            const expr_idx = self.can_ir.store.addExpr(CIR.Expr{
                .e_tuple = .{
                    .elems = elems_span,
                    .region = region,
                },
            });

            // Insert tuple_unbound type for tuple literals
            _ = self.can_ir.setTypeVarAtExpr(
                expr_idx,
                Content{ .structure = FlatType{
                    .tuple_unbound = types.Tuple{ .elems = elems_var_range },
                } },
            );

            return expr_idx;
        },
        .record => |e| {
            const region = self.parse_ir.tokenizedRegionToRegion(e.region);

            const fields_slice = self.parse_ir.store.recordFieldSlice(e.fields);
            if (fields_slice.len == 0) {
                const expr_idx = self.can_ir.store.addExpr(CIR.Expr{
                    .e_empty_record = .{
                        .region = region,
                    },
                });

                _ = self.can_ir.setTypeVarAtExpr(expr_idx, Content{ .structure = .empty_record });

                return expr_idx;
            }

            // Mark the start of scratch record fields for the record
            const scratch_top = self.can_ir.store.scratch_record_fields.top();

            // Track field names to detect duplicates
            const FieldInfo = struct { ident: base.Ident.Idx, region: base.Region };
            var seen_fields = std.ArrayListUnmanaged(FieldInfo){};
            defer seen_fields.deinit(self.can_ir.env.gpa);

            // Iterate over the record fields, canonicalizing each one
            // Then append the result to the scratch list
            for (fields_slice) |field| {
                const ast_field = self.parse_ir.store.getRecordField(field);

                // Get the field name identifier
                if (self.parse_ir.tokens.resolveIdentifier(ast_field.name)) |field_name_ident| {
                    const field_name_region = self.parse_ir.tokens.resolve(ast_field.name);

                    // Check for duplicate field names
                    var found_duplicate = false;
                    for (seen_fields.items) |seen_field| {
                        if (self.can_ir.env.idents.identsHaveSameText(field_name_ident, seen_field.ident)) {
                            // Found a duplicate - add diagnostic
                            const diagnostic = CIR.Diagnostic{
                                .duplicate_record_field = .{
                                    .field_name = field_name_ident,
                                    .duplicate_region = field_name_region,
                                    .original_region = seen_field.region,
                                },
                            };
                            self.can_ir.pushDiagnostic(diagnostic);
                            found_duplicate = true;
                            break;
                        }
                    }

                    if (!found_duplicate) {
                        // First occurrence of this field name
                        seen_fields.append(self.can_ir.env.gpa, FieldInfo{
                            .ident = field_name_ident,
                            .region = field_name_region,
                        }) catch |err| exitOnOom(err);

                        // Only canonicalize and include non-duplicate fields
                        if (try self.canonicalize_record_field(field)) |canonicalized| {
                            self.can_ir.store.scratch_record_fields.append(self.can_ir.env.gpa, canonicalized);
                        }
                    }
                } else {
                    // Field name couldn't be resolved, still try to canonicalize
                    if (try self.canonicalize_record_field(field)) |canonicalized| {
                        self.can_ir.store.scratch_record_fields.append(self.can_ir.env.gpa, canonicalized);
                    }
                }
            }

            // Create span of the new scratch record fields
            const fields_span = self.can_ir.store.recordFieldSpanFrom(scratch_top);

            // create type vars, first "reserve" node slots
            const record_expr_idx = self.can_ir.store.predictNodeIndex(2);

            // then in the final slot the actual expr is inserted
            const expr_idx = self.can_ir.store.addExpr(CIR.Expr{
                .e_record = .{
                    .fields = fields_span,
                    .ext_var = self.can_ir.pushFreshTypeVar(record_expr_idx, region),
                    .region = region,
                },
            });

            // Create fresh type variables for each record field
            // The type checker will unify these with the field expression types
            const cir_fields = self.can_ir.store.sliceRecordFields(fields_span);

            // Reserve additional type variable slots for field types
            const field_count = cir_fields.len;
            const total_vars_needed = field_count; // field_vars (ext_var already created above)
            const record_with_fields_expr_idx = self.can_ir.store.predictNodeIndex(@intCast(total_vars_needed));

            // Create fresh type variables for each field
            var type_record_fields = std.ArrayList(types.RecordField).init(self.can_ir.env.gpa);
            defer type_record_fields.deinit();

            for (cir_fields) |cir_field_idx| {
                const cir_field = self.can_ir.store.getRecordField(cir_field_idx);

                // Create a fresh type variable for this field
                const field_type_var = self.can_ir.pushFreshTypeVar(record_with_fields_expr_idx, region);

                type_record_fields.append(types.RecordField{
                    .name = cir_field.name,
                    .var_ = field_type_var,
                }) catch |err| exitOnOom(err);
            }

            // Create the record type structure
            const type_fields_range = self.can_ir.env.types.appendRecordFields(type_record_fields.items);
            const ext_var = self.can_ir.env.types.freshFromContent(.{ .structure = .empty_record });

            // Set the record structure on the expression variable
            // This provides the concrete type information for type checking and final output
            _ = self.can_ir.setTypeVarAtExpr(
                expr_idx,
                Content{ .structure = .{ .record = .{ .fields = type_fields_range, .ext = ext_var } } },
            );

            return expr_idx;
        },
        .lambda => |e| {
            const region = self.parse_ir.tokenizedRegionToRegion(e.region);

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
                    const arg_region = self.parse_ir.tokenizedRegionToRegion(arg.to_tokenized_region());
                    const malformed_idx = self.can_ir.pushMalformed(CIR.Pattern.Idx, CIR.Diagnostic{ .pattern_arg_invalid = .{
                        .region = arg_region,
                    } });
                    self.can_ir.store.scratch_patterns.append(gpa, malformed_idx);
                }
            }
            const args_span = self.can_ir.store.patternSpanFrom(args_start);

            // body
            const body_idx = blk: {
                if (try self.canonicalize_expr(e.body)) |idx| {
                    break :blk idx;
                } else {
                    const ast_body = self.parse_ir.store.getExpr(e.body);
                    const body_region = self.parse_ir.tokenizedRegionToRegion(ast_body.to_tokenized_region());
                    break :blk self.can_ir.pushMalformed(CIR.Expr.Idx, CIR.Diagnostic{
                        .lambda_body_not_canonicalized = .{ .region = body_region },
                    });
                }
            };

            // Create effect variable using dummy index (like other expressions do)
            const effect_var = self.can_ir.pushFreshTypeVar(@enumFromInt(0), region);

            // Create lambda expression
            const lambda_expr = CIR.Expr{
                .e_lambda = .{
                    .args = args_span,
                    .body = body_idx,
                    .effect_var = effect_var,
                    .region = region,
                },
            };
            const expr_idx = self.can_ir.store.addExpr(lambda_expr);

            _ = self.can_ir.setTypeVarAtExpr(expr_idx, Content{ .flex_var = null });
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
        .field_access => |field_access| {
            // Try module-qualified lookup first (e.g., Json.utf8)
            if (self.tryModuleQualifiedLookup(field_access)) |expr_idx| {
                return expr_idx;
            }

            // Regular field access canonicalization
            return try self.canonicalizeRegularFieldAccess(field_access);
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
            const region = self.parse_ir.tokenizedRegionToRegion(e.region);

            // Canonicalize left and right operands
            const lhs = blk: {
                if (try self.canonicalize_expr(e.left)) |left_expr_idx| {
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
                if (try self.canonicalize_expr(e.right)) |right_expr_idx| {
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
                .OpSlash => .div,
                .OpPercent => .rem,
                .OpLessThan => .lt,
                .OpGreaterThan => .gt,
                .OpLessThanOrEq => .le,
                .OpGreaterThanOrEq => .ge,
                .OpEquals => .eq,
                .OpNotEquals => .ne,
                .OpCaret => .pow,
                .OpDoubleSlash => .div_trunc,
                .OpAnd => .@"and",
                .OpOr => .@"or",
                .OpPizza => .pipe_forward,
                .OpDoubleQuestion => .null_coalesce,
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
                .e_binop = CIR.Expr.Binop.init(op, lhs, rhs, region),
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
        .if_then_else => |e| {
            const region = self.parse_ir.tokenizedRegionToRegion(e.region);

            // Start collecting if-branches
            const scratch_top = self.can_ir.store.scratchIfBranchTop();

            // Flatten the if-then-else chain
            const final_else = try self.flattenIfThenElseChainRecursive(e);
            const branches_span = self.can_ir.store.ifBranchSpanFrom(scratch_top);

            // Get the first branch's body to redirect to it
            const branches = self.can_ir.store.sliceIfBranches(branches_span);
            std.debug.assert(branches.len > 0);
            const first_branch = self.can_ir.store.getIfBranch(branches[0]);
            const first_branch_type_var = @as(TypeVar, @enumFromInt(@intFromEnum(first_branch.body)));

            // Create the if expression
            const expr_idx = self.can_ir.store.addExpr(CIR.Expr{
                .e_if = .{
                    .branches = branches_span,
                    .final_else = final_else,
                    .region = region,
                },
            });

            // Immediately redirect the if expression's type variable to the first branch's body
            // This is similar to how lists unify with their first element
            const expr_var = @as(TypeVar, @enumFromInt(@intFromEnum(expr_idx)));

            // Ensure the type store has slots up to the expression variable
            try self.can_ir.env.types.fillInSlotsThru(expr_var);

            const slot_idx = types.Store.varToSlotIdx(expr_var);
            self.can_ir.env.types.slots.set(slot_idx, .{ .redirect = first_branch_type_var });

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
        .ellipsis => |e| {
            const region = self.parse_ir.tokenizedRegionToRegion(e.region);
            const feature = self.can_ir.env.strings.insert(self.can_ir.env.gpa, "...");
            const diagnostic = self.can_ir.store.addDiagnostic(CIR.Diagnostic{ .not_implemented = .{
                .feature = feature,
                .region = region,
            } });
            const ellipsis_expr = self.can_ir.store.addExpr(CIR.Expr{ .e_runtime_error = .{
                .diagnostic = diagnostic,
                .region = region,
            } });
            return ellipsis_expr;
        },
        .block => |e| {
            const region = self.parse_ir.tokenizedRegionToRegion(e.region);

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
                    last_expr = try self.canonicalize_expr(stmt.expr.expr);
                } else {
                    // Regular statement processing
                    const result = try self.canonicalize_statement(stmt_idx);
                    if (result) |expr_idx| {
                        last_expr = expr_idx;
                    }
                }
            }

            // Determine the final expression
            const final_expr = if (last_expr) |expr_idx| blk: {
                break :blk expr_idx;
            } else blk: {
                // Empty block - create empty record
                const expr_idx = self.can_ir.store.addExpr(CIR.Expr{
                    .e_empty_record = .{ .region = region },
                });
                _ = self.can_ir.setTypeVarAtExpr(expr_idx, Content{ .structure = .empty_record });
                break :blk expr_idx;
            };

            // Create statement span
            const stmt_span = self.can_ir.store.statementSpanFrom(stmt_start);

            // Create and return block expression
            const block_expr = CIR.Expr{
                .e_block = .{
                    .stmts = stmt_span,
                    .final_expr = final_expr,
                    .region = region,
                },
            };
            const block_idx = self.can_ir.store.addExpr(block_expr);

            // The root expr will be unified with the last expr in type solving
            _ = self.can_ir.setTypeVarAtExpr(block_idx, Content{ .flex_var = null });

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
fn extractStringSegments(self: *Self, parts: []const AST.Expr.Idx) std.mem.Allocator.Error!CIR.Expr.Span {
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
                const str_expr_idx = self.can_ir.store.addExpr(CIR.Expr{ .e_str_segment = .{
                    .literal = string_idx,
                    .region = self.parse_ir.tokenizedRegionToRegion(part_node.to_tokenized_region()),
                } });

                // add the node idx to our scratch expr stack
                self.can_ir.store.addScratchExpr(str_expr_idx);
            },
            else => {

                // Any non-string-part is an interpolation
                if (try self.canonicalize_expr(part)) |expr_idx| {
                    // append our interpolated expression
                    self.can_ir.store.addScratchExpr(expr_idx);
                } else {
                    // unable to canonicalize the interpolation, push a malformed node
                    const region = self.parse_ir.tokenizedRegionToRegion(part_node.to_tokenized_region());
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
            const region = self.parse_ir.tokenizedRegionToRegion(e.region);
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
                    .region = self.parse_ir.tokenizedRegionToRegion(p.region),
                },
            };

            const pattern_idx = self.can_ir.store.addPattern(underscore_pattern);

            _ = self.can_ir.setTypeVarAtPat(pattern_idx, Content{ .flex_var = null });

            return pattern_idx;
        },
        .int => |e| {
            const region = self.parse_ir.tokenizedRegionToRegion(e.region);

            // Resolve to a string slice from the source
            const token_text = self.parse_ir.resolve(e.number_tok);

            // Parse as integer
            const value = std.fmt.parseInt(i128, token_text, 10) catch {
                // Invalid integer literal
                const malformed_idx = self.can_ir.pushMalformed(CIR.Pattern.Idx, CIR.Diagnostic{ .invalid_num_literal = .{
                    .region = region,
                } });
                return malformed_idx;
            };

            // Calculate requirements based on the value
            const u128_val: u128 = if (value < 0) @as(u128, @intCast(-(value + 1))) + 1 else @as(u128, @intCast(value));

            // Special handling for minimum signed values (-128, -32768, etc.)
            // These are special because they have a power-of-2 magnitude that fits exactly
            // in their signed type. We report them as needing one less bit to make the
            // standard "signed types have n-1 usable bits" logic work correctly.
            // This is done branchlessly by checking if the value is negative and its
            // magnitude is a power of 2.
            const is_negative = @as(u1, @intFromBool(value < 0));
            const is_power_of_2 = @as(u1, @intFromBool(u128_val != 0 and (u128_val & (u128_val - 1)) == 0));
            const is_minimum_signed = is_negative & is_power_of_2;

            // If it's a minimum signed value, we subtract 1 from the magnitude before
            // calculating bits needed. This makes -128 report as needing 7 bits instead of 8.
            const adjusted_val = u128_val - is_minimum_signed;

            const requirements = types.Num.Int.Requirements{
                .sign_needed = value < 0,
                .bits_needed = types.Num.Int.BitsNeeded.fromValue(adjusted_val),
            };

            const int_requirements = types.Num.IntRequirements{
                .sign_needed = requirements.sign_needed,
                .bits_needed = @intCast(@intFromEnum(requirements.bits_needed)),
            };
            const int_pattern = CIR.Pattern{
                .int_literal = .{
                    .value = .{ .bytes = @bitCast(value), .kind = .i128 },
                    .region = region,
                },
            };
            const pattern_idx = self.can_ir.store.addPattern(int_pattern);

            _ = self.can_ir.setTypeVarAtPat(pattern_idx, Content{
                .structure = .{ .num = .{ .num_unbound = int_requirements } },
            });

            return pattern_idx;
        },
        .frac => |e| {
            const region = self.parse_ir.tokenizedRegionToRegion(e.region);

            // Resolve to a string slice from the source
            const token_text = self.parse_ir.resolve(e.number_tok);

            const parsed = parseFracLiteral(token_text) catch |err| switch (err) {
                error.InvalidNumLiteral => {
                    const malformed_idx = self.can_ir.pushMalformed(CIR.Pattern.Idx, CIR.Diagnostic{ .invalid_num_literal = .{
                        .region = region,
                    } });
                    return malformed_idx;
                },
            };

            // Parse the literal first to get requirements
            const requirements = switch (parsed) {
                .small => |small_info| small_info.requirements,
                .dec => |dec_info| dec_info.requirements,
                .f64 => |f64_info| f64_info.requirements,
            };

            const frac_requirements = types.Num.FracRequirements{
                .fits_in_f32 = requirements.fits_in_f32,
                .fits_in_dec = requirements.fits_in_dec,
            };

            const cir_pattern = switch (parsed) {
                .small => |small_info| CIR.Pattern{
                    .small_dec_literal = .{
                        .numerator = small_info.numerator,
                        .denominator_power_of_ten = small_info.denominator_power_of_ten,
                        .region = region,
                    },
                },
                .dec => |dec_info| CIR.Pattern{
                    .dec_literal = .{
                        .value = dec_info.value,
                        .region = region,
                    },
                },
                .f64 => |f64_info| CIR.Pattern{
                    .f64_literal = .{
                        .value = f64_info.value,
                        .region = region,
                    },
                },
            };

            const pattern_idx = self.can_ir.store.addPattern(cir_pattern);

            _ = self.can_ir.setTypeVarAtPat(pattern_idx, Content{
                .structure = .{ .num = .{ .frac_unbound = frac_requirements } },
            });

            return pattern_idx;
        },
        .string => |e| {
            const region = self.parse_ir.tokenizedRegionToRegion(e.region);

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
                        const arg_region = self.parse_ir.tokenizedRegionToRegion(arg.to_tokenized_region());
                        const malformed_idx = self.can_ir.pushMalformed(CIR.Pattern.Idx, CIR.Diagnostic{ .pattern_arg_invalid = .{
                            .region = arg_region,
                        } });
                        self.can_ir.store.scratch_patterns.append(gpa, malformed_idx);
                    }
                }

                const region = self.parse_ir.tokenizedRegionToRegion(e.region);

                const args = self.can_ir.store.patternSpanFrom(start);

                // Reserve node slots for type vars, then insert into them.
                const final_pattern_idx = self.can_ir.store.predictNodeIndex(2);
                const ext_type_var = self.can_ir.pushFreshTypeVar(final_pattern_idx, region);
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

                const tag_union_type = self.can_ir.env.types.mkTagUnion(
                    &[_]Tag{Tag{ .name = tag_name, .args = types.Var.SafeList.Range.empty }},
                    ext_type_var,
                );
                _ = self.can_ir.setTypeVarAtPat(pattern_idx, tag_union_type);

                return pattern_idx;
            }
            return null;
        },
        .record => |e| {
            const region = self.parse_ir.tokenizedRegionToRegion(e.region);

            // Mark the start of scratch record destructs
            const scratch_top = self.can_ir.store.scratchRecordDestructTop();

            // Process each field in the record pattern
            for (self.parse_ir.store.patternRecordFieldSlice(e.fields)) |field_idx| {
                const field = self.parse_ir.store.getPatternRecordField(field_idx);
                const field_region = self.parse_ir.tokenizedRegionToRegion(field.region);

                // Resolve the field name
                if (self.parse_ir.tokens.resolveIdentifier(field.name)) |field_name_ident| {
                    // For simple destructuring like `{ name, age }`, both label and ident are the same
                    if (field.value) |_| {
                        // TODO: For patterns like `{ name: x }`, we'd need the value pattern, but that's not implemented yet
                        // TODO: Handle patterns like `{ name: x }` where there's a sub-pattern
                        const feature = self.can_ir.env.strings.insert(self.can_ir.env.gpa, "record pattern with sub-patterns");
                        const pattern_idx = self.can_ir.pushMalformed(CIR.Pattern.Idx, CIR.Diagnostic{ .not_implemented = .{
                            .feature = feature,
                            .region = field_region,
                        } });
                        return pattern_idx;
                    }

                    // Create the RecordDestruct for this field
                    const record_destruct = CIR.RecordDestruct{
                        .region = field_region,
                        .label = field_name_ident,
                        .ident = field_name_ident,
                        .kind = .Required,
                    };

                    const destruct_idx = self.can_ir.store.addRecordDestruct(record_destruct);
                    self.can_ir.store.addScratchRecordDestruct(destruct_idx);

                    // Create an assign pattern for this identifier and introduce it into scope
                    const assign_pattern_idx = self.can_ir.store.addPattern(CIR.Pattern{ .assign = .{
                        .ident = field_name_ident,
                        .region = field_region,
                    } });
                    _ = self.can_ir.setTypeVarAtPat(assign_pattern_idx, .{ .flex_var = null });

                    // Introduce the identifier into scope
                    switch (self.scopeIntroduceInternal(self.can_ir.env.gpa, &self.can_ir.env.idents, .ident, field_name_ident, assign_pattern_idx, false, true)) {
                        .success => {},
                        .shadowing_warning => |shadowed_pattern_idx| {
                            const shadowed_pattern = self.can_ir.store.getPattern(shadowed_pattern_idx);
                            const original_region = shadowed_pattern.toRegion();
                            self.can_ir.pushDiagnostic(CIR.Diagnostic{ .shadowing_warning = .{
                                .ident = field_name_ident,
                                .region = field_region,
                                .original_region = original_region,
                            } });
                        },
                        .top_level_var_error => {
                            const pattern_idx = self.can_ir.pushMalformed(CIR.Pattern.Idx, CIR.Diagnostic{ .invalid_top_level_statement = .{
                                .stmt = self.can_ir.env.strings.insert(self.can_ir.env.gpa, "var"),
                            } });
                            return pattern_idx;
                        },
                        .var_across_function_boundary => {
                            const pattern_idx = self.can_ir.pushMalformed(CIR.Pattern.Idx, CIR.Diagnostic{ .ident_already_in_scope = .{
                                .ident = field_name_ident,
                                .region = field_region,
                            } });
                            return pattern_idx;
                        },
                    }
                } else {
                    const feature = self.can_ir.env.strings.insert(self.can_ir.env.gpa, "report an error when unable to resolve field identifier");
                    const pattern_idx = self.can_ir.pushMalformed(CIR.Pattern.Idx, CIR.Diagnostic{ .not_implemented = .{
                        .feature = feature,
                        .region = field_region,
                    } });
                    return pattern_idx;
                }
            }

            // Create span of the new scratch record destructs
            const destructs_span = self.can_ir.store.recordDestructSpanFrom(scratch_top);

            // Create type variables for the record
            const whole_var = self.can_ir.env.types.fresh();
            const ext_var = self.can_ir.env.types.fresh();

            // Create the record destructure pattern
            const pattern_idx = self.can_ir.store.addPattern(CIR.Pattern{
                .record_destructure = .{
                    .whole_var = whole_var,
                    .ext_var = ext_var,
                    .destructs = destructs_span,
                    .region = region,
                },
            });

            // Set type variable for the pattern
            _ = self.can_ir.setTypeVarAtPat(pattern_idx, .{ .flex_var = null });

            return pattern_idx;
        },
        .tuple => |e| {
            const region = self.parse_ir.tokenizedRegionToRegion(e.region);

            // Mark the start of scratch patterns for the tuple
            const scratch_top = self.can_ir.store.scratchPatternTop();

            // Iterate over the tuple patterns, canonicalizing each one
            // Then append the result to the scratch list
            const patterns_slice = self.parse_ir.store.patternSlice(e.patterns);
            const elems_var_top = self.can_ir.env.types.tuple_elems.len();
            for (patterns_slice) |pattern| {
                if (self.canonicalize_pattern(pattern)) |canonicalized| {
                    self.can_ir.store.addScratchPattern(canonicalized);
                    _ = self.can_ir.env.types.appendTupleElem(@enumFromInt(@intFromEnum(canonicalized)));
                }
            }
            const elems_var_range = types.Var.SafeList.Range{
                .start = @enumFromInt(elems_var_top),
                .end = @enumFromInt(self.can_ir.env.types.tuple_elems.len()),
            };

            // Create span of the new scratch patterns
            const patterns_span = self.can_ir.store.patternSpanFrom(scratch_top);

            const pattern_idx = self.can_ir.store.addPattern(CIR.Pattern{
                .tuple = .{
                    .patterns = patterns_span,
                    .region = region,
                },
            });

            // Insert concrete type variable for tuple pattern
            _ = self.can_ir.setTypeVarAtPat(
                pattern_idx,
                Content{ .structure = FlatType{
                    .tuple = types.Tuple{ .elems = elems_var_range },
                } },
            );

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
        .as => |_| {
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

// Check if the given f64 fits in f32 range (ignoring precision loss)
fn fitsInF32(f64_val: f64) bool {
    // Check if it's within the range that f32 can represent.
    // This includes normal, subnormal, and zero values.
    // (This is a magnitude check, so take the abs value to check
    // positive and negative at the same time.)
    const abs_val = @abs(f64_val);
    return abs_val == 0.0 or (abs_val >= std.math.floatTrueMin(f32) and abs_val <= std.math.floatMax(f32));
}

// Check if a float value can be represented accurately in RocDec
fn fitsInDec(value: f64) bool {
    // RocDec uses i128 with 18 decimal places
    const max_dec_value = 170141183460469231731.0;
    const min_dec_value = -170141183460469231731.0;

    return value >= min_dec_value and value <= max_dec_value;
}

// Result type for parsing fractional literals into small, Dec, or f64
const FracLiteralResult = union(enum) {
    small: struct {
        numerator: i16,
        denominator_power_of_ten: u8,
        requirements: types.Num.Frac.Requirements,
    },
    dec: struct {
        value: RocDec,
        requirements: types.Num.Frac.Requirements,
    },
    f64: struct {
        value: f64,
        requirements: types.Num.Frac.Requirements,
    },
};

// Try to parse a fractional literal as a small dec (numerator/10^power)
fn parseSmallDec(token_text: []const u8) ?struct { numerator: i16, denominator_power_of_ten: u8 } {
    // Return null if input is too long to fit in our 32-byte buffer
    if (token_text.len > 32) return null;

    // For negative zero, we'll return null to force f64 path
    if (token_text.len > 0 and token_text[0] == '-') {
        const rest = token_text[1..];
        // Check if it's -0, -0.0, -0.00, etc.
        var all_zeros = true;
        for (rest) |c| {
            if (c != '0' and c != '.') {
                all_zeros = false;
                break;
            }
        }
        if (all_zeros) return null;
    }

    // Parse as a whole number by removing the decimal point
    const dot_pos = std.mem.indexOf(u8, token_text, ".") orelse {
        // No decimal point, parse as integer
        const val = std.fmt.parseInt(i32, token_text, 10) catch return null;
        if (val < -32768 or val > 32767) return null;
        return .{ .numerator = @as(i16, @intCast(val)), .denominator_power_of_ten = 0 };
    };

    // Count digits after decimal point
    const after_decimal_len = token_text.len - dot_pos - 1;
    if (after_decimal_len > 255) return null; // Too many decimal places

    // Build the string without the decimal point
    var buf: [32]u8 = undefined;
    var len: usize = 0;

    // Copy part before decimal
    @memcpy(buf[0..dot_pos], token_text[0..dot_pos]);
    len = dot_pos;

    // Copy part after decimal
    if (after_decimal_len > 0) {
        @memcpy(buf[len..][0..after_decimal_len], token_text[dot_pos + 1 ..]);
        len += after_decimal_len;
    }

    // Parse the combined number
    const val = std.fmt.parseInt(i32, buf[0..len], 10) catch return null;
    if (val < -32768 or val > 32767) return null;

    return .{ .numerator = @as(i16, @intCast(val)), .denominator_power_of_ten = @as(u8, @intCast(after_decimal_len)) };
}

// Parse a fractional literal from text and return small, Dec, or F64 value
fn parseFracLiteral(token_text: []const u8) !FracLiteralResult {
    // First, always parse as f64 to get the numeric value
    const f64_val = std.fmt.parseFloat(f64, token_text) catch {
        // If it can't be parsed as F64, it's too big to fit in any of Roc's Frac types.
        return error.InvalidNumLiteral;
    };

    // Check if it has scientific notation
    const has_scientific_notation = blk: {
        for (token_text) |char| {
            if (char == 'e' or char == 'E') {
                break :blk true;
            }
        }
        break :blk false;
    };

    // For non-scientific notation, try the original parseSmallDec first to preserve behavior
    if (!has_scientific_notation) {
        if (parseSmallDec(token_text)) |small| {
            // Convert to f64 to check requirements
            const numerator_f64 = @as(f64, @floatFromInt(small.numerator));
            var divisor: f64 = 1.0;
            var i: u8 = 0;
            while (i < small.denominator_power_of_ten) : (i += 1) {
                divisor *= 10.0;
            }
            const small_f64_val = numerator_f64 / divisor;

            return FracLiteralResult{
                .small = .{
                    .numerator = small.numerator,
                    .denominator_power_of_ten = small.denominator_power_of_ten,
                    .requirements = types.Num.Frac.Requirements{
                        .fits_in_f32 = fitsInF32(small_f64_val),
                        .fits_in_dec = true,
                    },
                },
            };
        }
    }

    // For scientific notation or when parseSmallDec fails, check if it's a whole number
    const rounded = @round(f64_val);
    if (f64_val == rounded and rounded >= -32768 and rounded <= 32767) {
        // It's a whole number in i16 range, can use small dec with denominator_power_of_ten = 0
        return FracLiteralResult{
            .small = .{
                .numerator = @as(i16, @intFromFloat(rounded)),
                .denominator_power_of_ten = 0,
                .requirements = types.Num.Frac.Requirements{
                    .fits_in_f32 = fitsInF32(f64_val),
                    .fits_in_dec = true,
                },
            },
        };
    }

    // Check if the value can fit in RocDec (whether or not it uses scientific notation)
    // RocDec uses i128 with 18 decimal places
    // We need to check if the value is within RocDec's range
    if (fitsInDec(f64_val)) {
        // Convert f64 to RocDec by multiplying by 10^18
        const dec_scale = std.math.pow(f64, 10, 18);
        const scaled_val = f64_val * dec_scale;

        // i128 max is 170141183460469231731687303715884105727
        // i128 min is -170141183460469231731687303715884105728
        // We need to be more conservative to avoid overflow during conversion
        const i128_max_f64 = 170141183460469231731687303715884105727.0;
        const i128_min_f64 = -170141183460469231731687303715884105728.0;

        if (scaled_val >= i128_min_f64 and scaled_val <= i128_max_f64) {
            // Safe to convert - but check for special cases
            const rounded_val = @round(scaled_val);

            // Extra safety check for boundary values
            if (rounded_val < i128_min_f64 or rounded_val > i128_max_f64) {
                // Would overflow, use f64 instead
                return FracLiteralResult{
                    .f64 = .{
                        .value = f64_val,
                        .requirements = types.Num.Frac.Requirements{
                            .fits_in_f32 = fitsInF32(f64_val),
                            .fits_in_dec = false,
                        },
                    },
                };
            }

            const dec_num = @as(i128, @intFromFloat(rounded_val));

            // Check if the value is too small (would round to 0 or near 0)
            // This prevents loss of precision for very small numbers like 1e-40
            const min_representable = 1e-18; // Smallest non-zero value Dec can represent
            if (@abs(f64_val) > 0 and @abs(f64_val) < min_representable) {
                // Too small for Dec precision, use f64
                return FracLiteralResult{
                    .f64 = .{
                        .value = f64_val,
                        .requirements = types.Num.Frac.Requirements{
                            .fits_in_f32 = fitsInF32(f64_val),
                            .fits_in_dec = false,
                        },
                    },
                };
            }

            return FracLiteralResult{
                .dec = .{
                    .value = RocDec{ .num = dec_num },
                    .requirements = types.Num.Frac.Requirements{
                        .fits_in_f32 = fitsInF32(f64_val),
                        .fits_in_dec = true,
                    },
                },
            };
        }
    }

    // If it doesn't fit in small dec or RocDec, use f64
    return FracLiteralResult{
        .f64 = .{
            .value = f64_val,
            .requirements = types.Num.Frac.Requirements{
                .fits_in_f32 = fitsInF32(f64_val),
                .fits_in_dec = false,
            },
        },
    };
}

test {
    _ = @import("canonicalize/test/int_test.zig");
    _ = @import("canonicalize/test/frac_test.zig");
    _ = @import("canonicalize/test/node_store_test.zig");
}

/// Flatten a chain of if-then-else expressions into multiple if-branches
/// Returns the final else expression that is not an if-then-else
fn flattenIfThenElseChainRecursive(self: *Self, if_expr: anytype) std.mem.Allocator.Error!CIR.Expr.Idx {
    // Canonicalize and add the current condition/then pair
    const cond_idx = blk: {
        if (try self.canonicalize_expr(if_expr.condition)) |idx| {
            break :blk idx;
        } else {
            const ast_cond = self.parse_ir.store.getExpr(if_expr.condition);
            const cond_region = self.parse_ir.tokenizedRegionToRegion(ast_cond.to_tokenized_region());
            break :blk self.can_ir.pushMalformed(CIR.Expr.Idx, CIR.Diagnostic{
                .if_condition_not_canonicalized = .{ .region = cond_region },
            });
        }
    };

    const then_idx = blk: {
        if (try self.canonicalize_expr(if_expr.then)) |idx| {
            break :blk idx;
        } else {
            const ast_then = self.parse_ir.store.getExpr(if_expr.then);
            const then_region = self.parse_ir.tokenizedRegionToRegion(ast_then.to_tokenized_region());
            break :blk self.can_ir.pushMalformed(CIR.Expr.Idx, CIR.Diagnostic{
                .if_then_not_canonicalized = .{ .region = then_region },
            });
        }
    };

    // Add this condition/then pair as an if-branch
    const if_branch = CIR.IfBranch{
        .cond = cond_idx,
        .body = then_idx,
    };
    self.can_ir.store.addScratchIfBranch(if_branch);

    // Check if the else clause is another if-then-else that we should flatten
    const else_expr = self.parse_ir.store.getExpr(if_expr.@"else");
    switch (else_expr) {
        .if_then_else => |nested_if| {
            // Recursively process the nested if-then-else
            return try self.flattenIfThenElseChainRecursive(nested_if);
        },
        else => {
            // This is the final else - canonicalize and return it
            if (try self.canonicalize_expr(if_expr.@"else")) |else_idx| {
                return else_idx;
            } else {
                const else_region = self.parse_ir.tokenizedRegionToRegion(else_expr.to_tokenized_region());
                return self.can_ir.pushMalformed(CIR.Expr.Idx, CIR.Diagnostic{
                    .if_else_not_canonicalized = .{ .region = else_region },
                });
            }
        },
    }
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

/// Canonicalize a tag variant within a tag union type annotation
/// Unlike general type canonicalization, this doesn't validate tag names against scope
/// since tags in tag unions are anonymous and defined by the union itself
fn canonicalize_tag_variant(self: *Self, anno_idx: AST.TypeAnno.Idx) CIR.TypeAnno.Idx {
    const ast_anno = self.parse_ir.store.getTypeAnno(anno_idx);
    switch (ast_anno) {
        .ty => |ty| {
            // For simple tags like `None`, just create the type annotation without scope validation
            const region = self.parse_ir.tokenizedRegionToRegion(ty.region);
            return self.can_ir.store.addTypeAnno(.{ .ty = .{
                .symbol = ty.ident,
                .region = region,
            } });
        },
        .apply => |apply| {
            // For tags with arguments like `Some(Str)`, validate the arguments but not the tag name
            const region = self.parse_ir.tokenizedRegionToRegion(apply.region);
            const args_slice = self.parse_ir.store.typeAnnoSlice(apply.args);

            if (args_slice.len == 0) {
                return self.can_ir.pushMalformed(CIR.TypeAnno.Idx, CIR.Diagnostic{ .malformed_type_annotation = .{ .region = region } });
            }

            // First argument is the tag name - don't validate it against scope
            const base_type = self.parse_ir.store.getTypeAnno(args_slice[0]);
            const type_name = switch (base_type) {
                .ty => |ty| ty.ident,
                else => return self.can_ir.pushMalformed(CIR.TypeAnno.Idx, CIR.Diagnostic{ .malformed_type_annotation = .{ .region = region } }),
            };

            // Canonicalize type arguments (skip first which is the tag name)
            // These should be validated against scope since they're real types like `Str`, `Int`, etc.
            const scratch_top = self.can_ir.store.scratchTypeAnnoTop();
            defer self.can_ir.store.clearScratchTypeAnnosFrom(scratch_top);

            for (args_slice[1..]) |arg_idx| {
                const canonicalized = self.canonicalize_type_anno(arg_idx);
                self.can_ir.store.addScratchTypeAnno(canonicalized);
            }

            const args = self.can_ir.store.typeAnnoSpanFrom(scratch_top);
            return self.can_ir.store.addTypeAnno(.{ .apply = .{
                .symbol = type_name,
                .args = args,
                .region = region,
            } });
        },
        else => {
            // For other cases, fall back to regular canonicalization
            return self.canonicalize_type_anno(anno_idx);
        },
    }
}

/// Canonicalize a statement within a block
fn canonicalize_type_anno(self: *Self, anno_idx: AST.TypeAnno.Idx) CIR.TypeAnno.Idx {
    const ast_anno = self.parse_ir.store.getTypeAnno(anno_idx);
    switch (ast_anno) {
        .apply => |apply| {
            const region = self.parse_ir.tokenizedRegionToRegion(apply.region);
            const args_slice = self.parse_ir.store.typeAnnoSlice(apply.args);

            // Validate we have arguments
            if (args_slice.len == 0) {
                return self.can_ir.pushMalformed(CIR.TypeAnno.Idx, CIR.Diagnostic{ .malformed_type_annotation = .{ .region = region } });
            }

            // First argument is the type constructor
            const base_type = self.parse_ir.store.getTypeAnno(args_slice[0]);
            const type_name = switch (base_type) {
                .ty => |ty| ty.ident,
                else => return self.can_ir.pushMalformed(CIR.TypeAnno.Idx, CIR.Diagnostic{ .malformed_type_annotation = .{ .region = region } }),
            };

            // Canonicalize type arguments (skip first which is the type name)
            const scratch_top = self.can_ir.store.scratchTypeAnnoTop();
            defer self.can_ir.store.clearScratchTypeAnnosFrom(scratch_top);

            for (args_slice[1..]) |arg_idx| {
                const canonicalized = self.canonicalize_type_anno(arg_idx);
                self.can_ir.store.addScratchTypeAnno(canonicalized);
            }

            const args = self.can_ir.store.typeAnnoSpanFrom(scratch_top);
            return self.can_ir.store.addTypeAnno(.{ .apply = .{
                .symbol = type_name,
                .args = args,
                .region = region,
            } });
        },
        .ty_var => |ty_var| {
            const region = self.parse_ir.tokenizedRegionToRegion(ty_var.region);
            const name_ident = self.parse_ir.tokens.resolveIdentifier(ty_var.tok) orelse {
                return self.can_ir.pushMalformed(CIR.TypeAnno.Idx, CIR.Diagnostic{ .malformed_type_annotation = .{
                    .region = region,
                } });
            };

            // Check if this type variable is in scope
            const type_var_in_scope = self.scopeLookupTypeVar(name_ident);
            if (type_var_in_scope == null) {
                // Type variable not found in scope - issue diagnostic
                self.can_ir.pushDiagnostic(CIR.Diagnostic{ .undeclared_type_var = .{
                    .name = name_ident,
                    .region = region,
                } });
            }

            return self.can_ir.store.addTypeAnno(.{ .ty_var = .{
                .name = name_ident,
                .region = region,
            } });
        },
        .ty => |ty| {
            const region = self.parse_ir.tokenizedRegionToRegion(ty.region);

            // Check if this type is declared in scope
            if (self.scopeLookupTypeDecl(ty.ident)) |_| {
                // Type found in scope - good!
                // TODO: We could store a reference to the type declaration for better error messages
            } else {
                // Type not found in scope - issue diagnostic
                self.can_ir.pushDiagnostic(CIR.Diagnostic{ .undeclared_type = .{
                    .name = ty.ident,
                    .region = region,
                } });
            }

            return self.can_ir.store.addTypeAnno(.{ .ty = .{
                .symbol = ty.ident,
                .region = region,
            } });
        },
        .mod_ty => |mod_ty| {
            const region = self.parse_ir.tokenizedRegionToRegion(mod_ty.region);
            return self.can_ir.store.addTypeAnno(.{ .mod_ty = .{
                .mod_symbol = mod_ty.mod_ident,
                .ty_symbol = mod_ty.ty_ident,
                .region = region,
            } });
        },
        .underscore => |underscore| {
            const region = self.parse_ir.tokenizedRegionToRegion(underscore.region);
            return self.can_ir.store.addTypeAnno(.{ .underscore = .{
                .region = region,
            } });
        },
        .tuple => |tuple| {
            const region = self.parse_ir.tokenizedRegionToRegion(tuple.region);
            // Canonicalize all tuple elements
            const scratch_top = self.can_ir.store.scratchTypeAnnoTop();
            defer self.can_ir.store.clearScratchTypeAnnosFrom(scratch_top);

            for (self.parse_ir.store.typeAnnoSlice(tuple.annos)) |elem_idx| {
                const canonicalized = self.canonicalize_type_anno(elem_idx);
                self.can_ir.store.addScratchTypeAnno(canonicalized);
            }

            const annos = self.can_ir.store.typeAnnoSpanFrom(scratch_top);
            return self.can_ir.store.addTypeAnno(.{ .tuple = .{
                .annos = annos,
                .region = region,
            } });
        },
        .record => |record| {
            const region = self.parse_ir.tokenizedRegionToRegion(record.region);

            // Canonicalize all record fields
            const scratch_top = self.can_ir.store.scratchAnnoRecordFieldTop();
            defer self.can_ir.store.clearScratchAnnoRecordFieldsFrom(scratch_top);

            for (self.parse_ir.store.annoRecordFieldSlice(record.fields)) |field_idx| {
                const ast_field = self.parse_ir.store.getAnnoRecordField(field_idx) catch |err| switch (err) {
                    error.MalformedNode => {
                        // Skip malformed field entirely - it was already handled during parsing
                        continue;
                    },
                };

                // Resolve field name
                const field_name = self.parse_ir.tokens.resolveIdentifier(ast_field.name) orelse {
                    // Malformed field name - continue with placeholder
                    const malformed_field_ident = Ident.for_text("malformed_field");
                    const malformed_ident = self.can_ir.env.idents.insert(self.can_ir.env.gpa, malformed_field_ident, Region.zero());
                    const canonicalized_ty = self.canonicalize_type_anno(ast_field.ty);
                    const field_region = self.parse_ir.tokenizedRegionToRegion(ast_field.region);

                    const cir_field = CIR.AnnoRecordField{
                        .name = malformed_ident,
                        .ty = canonicalized_ty,
                        .region = field_region,
                    };
                    const field_cir_idx = self.can_ir.store.addAnnoRecordField(cir_field);
                    self.can_ir.store.addScratchAnnoRecordField(field_cir_idx);
                    continue;
                };

                // Canonicalize field type
                const canonicalized_ty = self.canonicalize_type_anno(ast_field.ty);
                const field_region = self.parse_ir.tokenizedRegionToRegion(ast_field.region);

                const cir_field = CIR.AnnoRecordField{
                    .name = field_name,
                    .ty = canonicalized_ty,
                    .region = field_region,
                };
                const field_cir_idx = self.can_ir.store.addAnnoRecordField(cir_field);
                self.can_ir.store.addScratchAnnoRecordField(field_cir_idx);
            }

            const fields = self.can_ir.store.annoRecordFieldSpanFrom(scratch_top);
            return self.can_ir.store.addTypeAnno(.{ .record = .{
                .fields = fields,
                .region = region,
            } });
        },
        .tag_union => |tag_union| {
            const region = self.parse_ir.tokenizedRegionToRegion(tag_union.region);

            // Canonicalize all tags in the union using tag-specific canonicalization
            const scratch_top = self.can_ir.store.scratchTypeAnnoTop();
            defer self.can_ir.store.clearScratchTypeAnnosFrom(scratch_top);

            for (self.parse_ir.store.typeAnnoSlice(tag_union.tags)) |tag_idx| {
                const canonicalized = self.canonicalize_tag_variant(tag_idx);
                self.can_ir.store.addScratchTypeAnno(canonicalized);
            }

            const tags = self.can_ir.store.typeAnnoSpanFrom(scratch_top);

            // Handle optional open annotation (for extensible tag unions)
            const open_anno = if (tag_union.open_anno) |open_idx|
                self.canonicalize_type_anno(open_idx)
            else
                null;

            return self.can_ir.store.addTypeAnno(.{ .tag_union = .{
                .tags = tags,
                .open_anno = open_anno,
                .region = region,
            } });
        },
        .@"fn" => |fn_anno| {
            const region = self.parse_ir.tokenizedRegionToRegion(fn_anno.region);

            // Canonicalize argument types
            const scratch_top = self.can_ir.store.scratchTypeAnnoTop();
            defer self.can_ir.store.clearScratchTypeAnnosFrom(scratch_top);

            for (self.parse_ir.store.typeAnnoSlice(fn_anno.args)) |arg_idx| {
                const canonicalized = self.canonicalize_type_anno(arg_idx);
                self.can_ir.store.addScratchTypeAnno(canonicalized);
            }

            const args = self.can_ir.store.typeAnnoSpanFrom(scratch_top);

            // Canonicalize return type
            const ret = self.canonicalize_type_anno(fn_anno.ret);

            return self.can_ir.store.addTypeAnno(.{ .@"fn" = .{
                .args = args,
                .ret = ret,
                .effectful = fn_anno.effectful,
                .region = region,
            } });
        },
        .parens => |parens| {
            const region = self.parse_ir.tokenizedRegionToRegion(parens.region);
            const inner_anno = self.canonicalize_type_anno(parens.anno);
            return self.can_ir.store.addTypeAnno(.{ .parens = .{
                .anno = inner_anno,
                .region = region,
            } });
        },
        .malformed => |malformed| {
            const region = self.parse_ir.tokenizedRegionToRegion(malformed.region);
            return self.can_ir.pushMalformed(CIR.TypeAnno.Idx, CIR.Diagnostic{ .malformed_type_annotation = .{
                .region = region,
            } });
        },
    }
}

fn canonicalize_type_header(self: *Self, header_idx: AST.TypeHeader.Idx) CIR.TypeHeader.Idx {
    // Check if the node is malformed before calling getTypeHeader
    const node = self.parse_ir.store.nodes.get(@enumFromInt(@intFromEnum(header_idx)));
    if (node.tag == .malformed) {
        // Create a malformed type header with an invalid identifier
        const region = self.parse_ir.tokenizedRegionToRegion(node.region);
        return self.can_ir.store.addTypeHeader(.{
            .name = base.Ident.Idx{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = 0 }, // Invalid identifier
            .args = .{ .span = .{ .start = 0, .len = 0 } },
            .region = region,
        });
    }

    const ast_header = self.parse_ir.store.getTypeHeader(header_idx);
    const region = self.parse_ir.tokenizedRegionToRegion(ast_header.region);

    // Get the type name identifier
    const name_ident = self.parse_ir.tokens.resolveIdentifier(ast_header.name) orelse {
        // If we can't resolve the identifier, create a malformed header with invalid identifier
        return self.can_ir.store.addTypeHeader(.{
            .name = base.Ident.Idx{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = 0 }, // Invalid identifier
            .args = .{ .span = .{ .start = 0, .len = 0 } },
            .region = region,
        });
    };

    // Canonicalize type arguments - these are parameter declarations, not references
    const scratch_top = self.can_ir.store.scratchTypeAnnoTop();
    defer self.can_ir.store.clearScratchTypeAnnosFrom(scratch_top);

    for (self.parse_ir.store.typeAnnoSlice(ast_header.args)) |arg_idx| {
        const ast_arg = self.parse_ir.store.getTypeAnno(arg_idx);
        // Type parameters should be treated as declarations, not lookups
        switch (ast_arg) {
            .ty_var => |ty_var| {
                const param_region = self.parse_ir.tokenizedRegionToRegion(ty_var.region);
                const param_ident = self.parse_ir.tokens.resolveIdentifier(ty_var.tok) orelse {
                    const malformed = self.can_ir.pushMalformed(CIR.TypeAnno.Idx, CIR.Diagnostic{ .malformed_type_annotation = .{
                        .region = param_region,
                    } });
                    self.can_ir.store.addScratchTypeAnno(malformed);
                    continue;
                };

                // Create type variable annotation for this parameter
                const param_anno = self.can_ir.store.addTypeAnno(.{ .ty_var = .{
                    .name = param_ident,
                    .region = param_region,
                } });
                self.can_ir.store.addScratchTypeAnno(param_anno);
            },
            else => {
                // Other types in parameter position - canonicalize normally but warn
                const canonicalized = self.canonicalize_type_anno(arg_idx);
                self.can_ir.store.addScratchTypeAnno(canonicalized);
            },
        }
    }

    const args = self.can_ir.store.typeAnnoSpanFrom(scratch_top);

    return self.can_ir.store.addTypeHeader(.{
        .name = name_ident,
        .args = args,
        .region = region,
    });
}

/// Canonicalize a statement in the canonical IR.
pub fn canonicalize_statement(self: *Self, stmt_idx: AST.Statement.Idx) std.mem.Allocator.Error!?CIR.Expr.Idx {
    const stmt = self.parse_ir.store.getStatement(stmt_idx);

    switch (stmt) {
        .decl => |d| {
            // Check if this is a var reassignment
            const pattern = self.parse_ir.store.getPattern(d.pattern);
            if (pattern == .ident) {
                const ident_tok = pattern.ident.ident_tok;
                if (self.parse_ir.tokens.resolveIdentifier(ident_tok)) |ident_idx| {
                    const region = self.parse_ir.tokenizedRegionToRegion(self.parse_ir.store.getPattern(d.pattern).to_tokenized_region());

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
                                const reassign_stmt = CIR.Statement{ .s_reassign = .{
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
                                const expr_idx = try self.canonicalize_expr(d.body) orelse return null;

                                // Create reassign statement
                                const reassign_stmt = CIR.Statement{ .s_reassign = .{
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
            const expr_idx = try self.canonicalize_expr(d.body) orelse return null;

            // Create a declaration statement
            const decl_stmt = CIR.Statement{ .s_decl = .{
                .pattern = pattern_idx,
                .expr = expr_idx,
                .region = self.parse_ir.tokenizedRegionToRegion(d.region),
            } };
            const decl_idx = self.can_ir.store.addStatement(decl_stmt);
            self.can_ir.store.addScratchStatement(decl_idx);

            return expr_idx;
        },
        .@"var" => |v| {
            // Var declaration - handle specially with function boundary tracking
            const var_name = self.parse_ir.tokens.resolveIdentifier(v.name) orelse return null;
            const region = self.parse_ir.tokenizedRegionToRegion(v.region);

            // Canonicalize the initial value
            const init_expr_idx = try self.canonicalize_expr(v.body) orelse return null;

            // Create pattern for the var
            const pattern_idx = self.can_ir.store.addPattern(CIR.Pattern{ .assign = .{ .ident = var_name, .region = region } });

            // Introduce the var with function boundary tracking
            _ = self.scopeIntroduceVar(var_name, pattern_idx, region, true, CIR.Pattern.Idx);

            // Create var statement
            const var_stmt = CIR.Statement{ .s_var = .{
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
            const expr_idx = try self.canonicalize_expr(e.expr) orelse return null;

            // Create expression statement
            const expr_stmt = CIR.Statement{ .s_expr = .{
                .expr = expr_idx,
                .region = self.parse_ir.tokenizedRegionToRegion(e.region),
            } };
            const expr_stmt_idx = self.can_ir.store.addStatement(expr_stmt);
            self.can_ir.store.addScratchStatement(expr_stmt_idx);

            return expr_idx;
        },
        .crash => |c| {
            // Crash statement
            const region = self.parse_ir.tokenizedRegionToRegion(c.region);

            // Create a crash diagnostic and runtime error expression
            const feature = self.can_ir.env.strings.insert(self.can_ir.env.gpa, "crash statement");
            const diagnostic = self.can_ir.store.addDiagnostic(CIR.Diagnostic{ .not_implemented = .{
                .feature = feature,
                .region = region,
            } });
            const crash_expr = self.can_ir.store.addExpr(CIR.Expr{ .e_runtime_error = .{
                .diagnostic = diagnostic,
                .region = region,
            } });
            return crash_expr;
        },
        .type_decl => |s| {
            // TODO type declarations in statement context
            const feature = self.can_ir.env.strings.insert(self.can_ir.env.gpa, "type_decl in statement context");
            return self.can_ir.pushMalformed(CIR.Expr.Idx, CIR.Diagnostic{ .not_implemented = .{
                .feature = feature,
                .region = self.parse_ir.tokenizedRegionToRegion(s.region),
            } });
        },
        .type_anno => |ta| {
            // Type annotation statement
            const region = self.parse_ir.tokenizedRegionToRegion(ta.region);

            // Resolve the identifier name
            const name_ident = self.parse_ir.tokens.resolveIdentifier(ta.name) orelse {
                const feature = self.can_ir.env.strings.insert(self.can_ir.env.gpa, "type annotation identifier resolution");
                return self.can_ir.pushMalformed(CIR.Expr.Idx, CIR.Diagnostic{ .not_implemented = .{
                    .feature = feature,
                    .region = region,
                } });
            };

            // First, extract all type variables from the AST annotation
            var type_vars = std.ArrayList(Ident.Idx).init(self.can_ir.env.gpa);
            defer type_vars.deinit();

            // Extract type variables from the AST annotation
            self.extractTypeVarsFromASTAnno(ta.anno, &type_vars);

            // Enter a new scope for type variables
            self.scopeEnter(self.can_ir.env.gpa, false);
            defer self.scopeExit(self.can_ir.env.gpa) catch {};

            // Introduce type variables into scope
            for (type_vars.items) |type_var| {
                // Get the proper region for this type variable from the AST
                const type_var_region = self.getTypeVarRegionFromAST(ta.anno, type_var) orelse region;
                const type_var_anno = self.can_ir.store.addTypeAnno(.{ .ty_var = .{
                    .name = type_var,
                    .region = type_var_region,
                } });
                self.scopeIntroduceTypeVar(type_var, type_var_anno);
            }

            // Now canonicalize the annotation with type variables in scope
            const type_anno_idx = self.canonicalize_type_anno(ta.anno);

            // Store the type annotation for connection with the next declaration
            self.pending_type_annos.append(self.can_ir.env.gpa, .{ .ident = name_ident, .anno = type_anno_idx }) catch |err| exitOnOom(err);

            // Create a type annotation statement
            const type_anno_stmt = CIR.Statement{
                .s_type_anno = .{
                    .name = name_ident,
                    .anno = type_anno_idx,
                    .where = null, // Where clauses are not yet implemented in the parser
                    .region = region,
                },
            };
            const type_anno_stmt_idx = self.can_ir.store.addStatement(type_anno_stmt);
            self.can_ir.store.addScratchStatement(type_anno_stmt_idx);

            // Type annotations don't produce runtime values, so return a unit expression
            // Create an empty tuple as a unit value
            const empty_span = CIR.Expr.Span{ .span = base.DataSpan{ .start = 0, .len = 0 } };
            const unit_expr = self.can_ir.store.addExpr(CIR.Expr{ .e_tuple = .{
                .elems = empty_span,
                .region = region,
            } });
            _ = self.can_ir.setTypeVarAtExpr(unit_expr, Content{ .flex_var = null });
            return unit_expr;
        },
        .import => |import_stmt| {
            _ = self.canonicalizeImportStatement(import_stmt);

            // Import statements don't produce runtime values, so return a unit expression
            const region = self.parse_ir.tokenizedRegionToRegion(import_stmt.region);
            const empty_span = CIR.Expr.Span{ .span = base.DataSpan{ .start = 0, .len = 0 } };
            const unit_expr = self.can_ir.store.addExpr(CIR.Expr{ .e_tuple = .{
                .elems = empty_span,
                .region = region,
            } });
            _ = self.can_ir.setTypeVarAtExpr(unit_expr, Content{ .flex_var = null });
            return unit_expr;
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

    // Check for unused variables in the scope we're about to exit
    const scope = &self.scopes.items[self.scopes.items.len - 1];
    self.checkScopeForUnusedVariables(scope);

    var popped_scope: Scope = self.scopes.pop().?;
    popped_scope.deinit(gpa);
}

/// Get the current scope
fn currentScope(self: *Self) *Scope {
    std.debug.assert(self.scopes.items.len > 0);
    return &self.scopes.items[self.scopes.items.len - 1];
}

/// This will be used later for builtins like Num.nan, Num.infinity, etc.
pub fn addNonFiniteFloat(self: *Self, value: f64, region: base.Region) CIR.Expr.Idx {
    // Dec doesn't have infinity, -infinity, or NaN
    const requirements = types.Num.Frac.Requirements{
        .fits_in_f32 = true,
        .fits_in_dec = false,
    };

    const frac_requirements = types.Num.FracRequirements{
        .fits_in_f32 = requirements.fits_in_f32,
        .fits_in_dec = requirements.fits_in_dec,
    };

    // then in the final slot the actual expr is inserted
    const expr_idx = self.can_ir.store.addExpr(CIR.Expr{
        .e_frac_f64 = .{
            .value = value,
            .region = region,
        },
    });

    // Insert concrete type variable
    _ = self.can_ir.setTypeVarAtExpr(
        expr_idx,
        Content{ .structure = .{ .num = .{ .frac_unbound = frac_requirements } } },
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

/// Lookup a type variable in the scope hierarchy
fn scopeLookupTypeVar(self: *const Self, name_ident: Ident.Idx) ?CIR.TypeAnno.Idx {
    // Search from innermost to outermost scope
    var i = self.scopes.items.len;
    while (i > 0) {
        i -= 1;
        const scope = &self.scopes.items[i];

        switch (scope.lookupTypeVar(&self.can_ir.env.idents, name_ident)) {
            .found => |type_var_idx| return type_var_idx,
            .not_found => continue,
        }
    }
    return null;
}

/// Introduce a type variable into the current scope
fn scopeIntroduceTypeVar(self: *Self, name: Ident.Idx, type_var_anno: CIR.TypeAnno.Idx) void {
    const gpa = self.can_ir.env.gpa;
    const current_scope = &self.scopes.items[self.scopes.items.len - 1];

    // Don't use parent lookup function for now - just introduce directly
    // Type variable shadowing is allowed in Roc
    const result = current_scope.introduceTypeVar(gpa, &self.can_ir.env.idents, name, type_var_anno, null);

    switch (result) {
        .success => {},
        .shadowing_warning => |shadowed_type_var_idx| {
            // Type variable shadowing is allowed but should produce warning
            const shadowed_type_var = self.can_ir.store.getTypeAnno(shadowed_type_var_idx);
            const original_region = shadowed_type_var.toRegion();
            self.can_ir.pushDiagnostic(CIR.Diagnostic{ .shadowing_warning = .{
                .ident = name,
                .region = self.can_ir.store.getTypeAnno(type_var_anno).toRegion(),
                .original_region = original_region,
            } });
        },
        .already_in_scope => |_| {
            // Type variable already exists in this scope - this is fine for repeated references
        },
    }
}

/// Extract type variables from a type annotation and introduce them into scope
fn extractTypeVarsFromAnno(self: *Self, type_anno_idx: CIR.TypeAnno.Idx) void {
    const type_anno = self.can_ir.store.getTypeAnno(type_anno_idx);

    switch (type_anno) {
        .ty_var => |ty_var| {
            // This is a type variable - introduce it into scope
            self.scopeIntroduceTypeVar(ty_var.name, type_anno_idx);
        },
        .apply => |apply| {
            // Recursively extract from arguments
            for (self.can_ir.store.sliceTypeAnnos(apply.args)) |arg_idx| {
                self.extractTypeVarsFromAnno(arg_idx);
            }
        },
        .@"fn" => |fn_anno| {
            // Extract type variables from function parameter types
            for (self.can_ir.store.sliceTypeAnnos(fn_anno.args)) |param_idx| {
                self.extractTypeVarsFromAnno(param_idx);
            }
            // Extract type variables from return type
            self.extractTypeVarsFromAnno(fn_anno.ret);
        },
        .tuple => |tuple| {
            // Extract from tuple elements
            for (self.can_ir.store.sliceTypeAnnos(tuple.annos)) |elem_idx| {
                self.extractTypeVarsFromAnno(elem_idx);
            }
        },
        .parens => |parens| {
            // Extract from inner annotation
            self.extractTypeVarsFromAnno(parens.anno);
        },
        .record => |record| {
            // Extract type variables from record field types
            for (self.can_ir.store.sliceAnnoRecordFields(record.fields)) |field_idx| {
                const field = self.can_ir.store.getAnnoRecordField(field_idx);
                self.extractTypeVarsFromAnno(field.ty);
            }
        },
        .ty, .underscore, .mod_ty, .tag_union, .malformed => {
            // These don't contain type variables to extract
        },
    }
}

fn introduceTypeParametersFromHeader(self: *Self, header_idx: CIR.TypeHeader.Idx) void {
    const header = self.can_ir.store.getTypeHeader(header_idx);

    // Introduce each type parameter into the current scope
    for (self.can_ir.store.sliceTypeAnnos(header.args)) |param_idx| {
        const param = self.can_ir.store.getTypeAnno(param_idx);
        if (param == .ty_var) {
            self.scopeIntroduceTypeVar(param.ty_var.name, param_idx);
        }
    }
}

fn extractTypeVarsFromASTAnno(self: *Self, anno_idx: AST.TypeAnno.Idx, vars: *std.ArrayList(Ident.Idx)) void {
    switch (self.parse_ir.store.getTypeAnno(anno_idx)) {
        .ty_var => |ty_var| {
            if (self.parse_ir.tokens.resolveIdentifier(ty_var.tok)) |ident| {
                // Check if we already have this type variable
                for (vars.items) |existing| {
                    if (existing.idx == ident.idx) return; // Already added
                }
                vars.append(ident) catch |err| exitOnOom(err);
            }
        },
        .apply => |apply| {
            for (self.parse_ir.store.typeAnnoSlice(apply.args)) |arg_idx| {
                self.extractTypeVarsFromASTAnno(arg_idx, vars);
            }
        },
        .@"fn" => |fn_anno| {
            for (self.parse_ir.store.typeAnnoSlice(fn_anno.args)) |arg_idx| {
                self.extractTypeVarsFromASTAnno(arg_idx, vars);
            }
            self.extractTypeVarsFromASTAnno(fn_anno.ret, vars);
        },
        .tuple => |tuple| {
            for (self.parse_ir.store.typeAnnoSlice(tuple.annos)) |elem_idx| {
                self.extractTypeVarsFromASTAnno(elem_idx, vars);
            }
        },
        .parens => |parens| {
            self.extractTypeVarsFromASTAnno(parens.anno, vars);
        },
        .record => |record| {
            // Extract type variables from record field types
            for (self.parse_ir.store.annoRecordFieldSlice(record.fields)) |field_idx| {
                const field = self.parse_ir.store.getAnnoRecordField(field_idx) catch |err| switch (err) {
                    error.MalformedNode => continue,
                };
                self.extractTypeVarsFromASTAnno(field.ty, vars);
            }
        },
        .ty, .underscore, .mod_ty, .tag_union, .malformed => {
            // These don't contain type variables to extract
        },
    }
}

/// Get the region of a specific type variable from an AST type annotation
fn getTypeVarRegionFromAST(self: *Self, anno_idx: AST.TypeAnno.Idx, target_ident: Ident.Idx) ?Region {
    const ast_anno = self.parse_ir.store.getTypeAnno(anno_idx);

    switch (ast_anno) {
        .ty_var => |ty_var| {
            if (self.parse_ir.tokens.resolveIdentifier(ty_var.tok)) |ident| {
                if (ident.idx == target_ident.idx) {
                    return self.parse_ir.tokenizedRegionToRegion(ty_var.region);
                }
            }
            return null;
        },
        .apply => |apply| {
            for (self.parse_ir.store.typeAnnoSlice(apply.args)) |arg_idx| {
                if (self.getTypeVarRegionFromAST(arg_idx, target_ident)) |region| {
                    return region;
                }
            }
            return null;
        },
        .@"fn" => |fn_anno| {
            for (self.parse_ir.store.typeAnnoSlice(fn_anno.args)) |arg_idx| {
                if (self.getTypeVarRegionFromAST(arg_idx, target_ident)) |region| {
                    return region;
                }
            }
            return self.getTypeVarRegionFromAST(fn_anno.ret, target_ident);
        },
        .tuple => |tuple| {
            for (self.parse_ir.store.typeAnnoSlice(tuple.annos)) |elem_idx| {
                if (self.getTypeVarRegionFromAST(elem_idx, target_ident)) |region| {
                    return region;
                }
            }
            return null;
        },
        .parens => |parens| {
            return self.getTypeVarRegionFromAST(parens.anno, target_ident);
        },
        .record => |record| {
            for (self.parse_ir.store.annoRecordFieldSlice(record.fields)) |field_idx| {
                const field = self.parse_ir.store.getAnnoRecordField(field_idx) catch |err| switch (err) {
                    error.MalformedNode => continue,
                };
                if (self.getTypeVarRegionFromAST(field.ty, target_ident)) |region| {
                    return region;
                }
            }
            return null;
        },
        .tag_union => |tag_union| {
            for (self.parse_ir.store.typeAnnoSlice(tag_union.tags)) |tag_idx| {
                if (self.getTypeVarRegionFromAST(tag_idx, target_ident)) |region| {
                    return region;
                }
            }
            if (tag_union.open_anno) |open_idx| {
                return self.getTypeVarRegionFromAST(open_idx, target_ident);
            }
            return null;
        },
        .ty, .underscore, .mod_ty, .malformed => {
            // These don't contain type variables
            return null;
        },
    }
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

/// Check if an identifier is marked as ignored (underscore prefix)
fn identIsIgnored(ident_idx: base.Ident.Idx) bool {
    return ident_idx.attributes.ignored;
}

/// Handle unused variable checking and diagnostics
fn checkUsedUnderscoreVariable(
    self: *Self,
    ident_idx: base.Ident.Idx,
    region: Region,
) void {
    const is_ignored = identIsIgnored(ident_idx);

    if (is_ignored) {
        // Variable prefixed with _ but is actually used - warning
        self.can_ir.pushDiagnostic(CIR.Diagnostic{ .used_underscore_variable = .{
            .ident = ident_idx,
            .region = region,
        } });
    }
}

fn checkScopeForUnusedVariables(self: *Self, scope: *const Scope) void {
    // Iterate through all identifiers in this scope
    var iterator = scope.idents.iterator();
    while (iterator.next()) |entry| {
        const ident_idx = entry.key_ptr.*;
        const pattern_idx = entry.value_ptr.*;

        // Skip if this variable was used
        if (self.used_patterns.contains(pattern_idx)) {
            continue;
        }

        // Skip if this is an ignored variable (starts with _)
        if (identIsIgnored(ident_idx)) {
            continue;
        }

        // Get the region for this pattern to provide good error location
        const pattern = self.can_ir.store.getPattern(pattern_idx);
        const region = pattern.toRegion();

        // Report unused variable
        self.can_ir.pushDiagnostic(CIR.Diagnostic{ .unused_variable = .{
            .ident = ident_idx,
            .region = region,
        } });
    }
}

/// Introduce a type declaration into the current scope
fn scopeIntroduceTypeDecl(
    self: *Self,
    name_ident: Ident.Idx,
    type_decl_stmt: CIR.Statement.Idx,
    region: Region,
) void {
    const gpa = self.can_ir.env.gpa;
    const current_scope = &self.scopes.items[self.scopes.items.len - 1];

    // Check for shadowing in parent scopes
    var shadowed_in_parent: ?CIR.Statement.Idx = null;
    if (self.scopes.items.len > 1) {
        var i = self.scopes.items.len - 1;
        while (i > 0) {
            i -= 1;
            const scope = &self.scopes.items[i];
            switch (scope.lookupTypeDecl(&self.can_ir.env.idents, name_ident)) {
                .found => |type_decl_idx| {
                    shadowed_in_parent = type_decl_idx;
                    break;
                },
                .not_found => continue,
            }
        }
    }

    const result = current_scope.introduceTypeDecl(gpa, &self.can_ir.env.idents, name_ident, type_decl_stmt, null);

    switch (result) {
        .success => {
            // Check if we're shadowing a type in a parent scope
            if (shadowed_in_parent) |shadowed_stmt| {
                const shadowed_statement = self.can_ir.store.getStatement(shadowed_stmt);
                const original_region = shadowed_statement.toRegion();
                self.can_ir.pushDiagnostic(CIR.Diagnostic{
                    .shadowing_warning = .{
                        .ident = name_ident,
                        .region = region,
                        .original_region = original_region,
                    },
                });
            }
        },
        .shadowing_warning => |shadowed_stmt| {
            // This shouldn't happen since we're not passing a parent lookup function
            // but handle it just in case the Scope implementation changes
            const shadowed_statement = self.can_ir.store.getStatement(shadowed_stmt);
            const original_region = shadowed_statement.toRegion();
            self.can_ir.pushDiagnostic(CIR.Diagnostic{
                .shadowing_warning = .{
                    .ident = name_ident,
                    .region = region,
                    .original_region = original_region,
                },
            });
        },
        .redeclared_error => |original_stmt| {
            // Extract region information from the original statement
            const original_statement = self.can_ir.store.getStatement(original_stmt);
            const original_region = original_statement.toRegion();
            self.can_ir.pushDiagnostic(CIR.Diagnostic{
                .type_redeclared = .{
                    .original_region = original_region,
                    .redeclared_region = region,
                    .name = name_ident,
                },
            });
        },
        .type_alias_redeclared => |original_stmt| {
            const original_statement = self.can_ir.store.getStatement(original_stmt);
            const original_region = original_statement.toRegion();
            self.can_ir.pushDiagnostic(CIR.Diagnostic{
                .type_alias_redeclared = .{
                    .name = name_ident,
                    .original_region = original_region,
                    .redeclared_region = region,
                },
            });
        },
        .nominal_type_redeclared => |original_stmt| {
            const original_statement = self.can_ir.store.getStatement(original_stmt);
            const original_region = original_statement.toRegion();
            self.can_ir.pushDiagnostic(CIR.Diagnostic{
                .nominal_type_redeclared = .{
                    .name = name_ident,
                    .original_region = original_region,
                    .redeclared_region = region,
                },
            });
        },
        .cross_scope_shadowing => |shadowed_stmt| {
            const shadowed_statement = self.can_ir.store.getStatement(shadowed_stmt);
            const original_region = shadowed_statement.toRegion();
            self.can_ir.pushDiagnostic(CIR.Diagnostic{
                .type_shadowed_warning = .{
                    .name = name_ident,
                    .region = region,
                    .original_region = original_region,
                    .cross_scope = true,
                },
            });
        },
        .parameter_conflict => |conflict| {
            const original_statement = self.can_ir.store.getStatement(conflict.original_stmt);
            const original_region = original_statement.toRegion();
            self.can_ir.pushDiagnostic(CIR.Diagnostic{
                .type_parameter_conflict = .{
                    .name = name_ident,
                    .parameter_name = conflict.conflicting_parameter,
                    .region = region,
                    .original_region = original_region,
                },
            });
        },
    }
}

/// Lookup a type declaration in the scope hierarchy
fn scopeLookupTypeDecl(self: *const Self, name_ident: Ident.Idx) ?CIR.Statement.Idx {
    // Search from innermost to outermost scope
    var i = self.scopes.items.len;
    while (i > 0) {
        i -= 1;
        const scope = &self.scopes.items[i];

        switch (scope.lookupTypeDecl(&self.can_ir.env.idents, name_ident)) {
            .found => |type_decl_idx| return type_decl_idx,
            .not_found => continue,
        }
    }

    return null;
}

/// Look up a module alias in the scope hierarchy
fn scopeLookupModule(self: *const Self, alias_name: Ident.Idx) ?Ident.Idx {
    // Search from innermost to outermost scope
    var i = self.scopes.items.len;
    while (i > 0) {
        i -= 1;
        const scope = &self.scopes.items[i];

        switch (scope.lookupModuleAlias(&self.can_ir.env.idents, alias_name)) {
            .found => |module_name| return module_name,
            .not_found => continue,
        }
    }

    return null;
}

/// Introduce a module alias into scope
fn scopeIntroduceModuleAlias(self: *Self, alias_name: Ident.Idx, module_name: Ident.Idx) void {
    const gpa = self.can_ir.env.gpa;
    const current_scope = &self.scopes.items[self.scopes.items.len - 1];

    // Simplified introduction without parent lookup for now
    const result = current_scope.introduceModuleAlias(gpa, &self.can_ir.env.idents, alias_name, module_name, null);

    switch (result) {
        .success => {},
        .shadowing_warning => |shadowed_module| {
            // Create diagnostic for module alias shadowing
            self.can_ir.pushDiagnostic(CIR.Diagnostic{
                .shadowing_warning = .{
                    .ident = alias_name,
                    .region = Region.zero(), // TODO: get proper region
                    .original_region = Region.zero(), // TODO: get proper region
                },
            });
            _ = shadowed_module; // Suppress unused variable warning
        },
        .already_in_scope => |existing_module| {
            // Module alias already exists in current scope
            // For now, just issue a diagnostic
            self.can_ir.pushDiagnostic(CIR.Diagnostic{
                .shadowing_warning = .{
                    .ident = alias_name,
                    .region = Region.zero(), // TODO: get proper region
                    .original_region = Region.zero(), // TODO: get proper region
                },
            });
            _ = existing_module; // Suppress unused variable warning
        },
    }
}

/// Helper function to look up module aliases in parent scopes only
fn scopeLookupModuleInParentScopes(self: *const Self, alias_name: Ident.Idx) ?Ident.Idx {
    // Search from second-innermost to outermost scope (excluding current scope)
    if (self.scopes.items.len <= 1) return null;

    var i = self.scopes.items.len - 1;
    while (i > 0) {
        i -= 1;
        const scope = &self.scopes.items[i];

        switch (scope.lookupModuleAlias(&self.can_ir.env.idents, alias_name)) {
            .found => |module_name| return module_name,
            .not_found => continue,
        }
    }

    return null;
}

/// Look up an exposed item across all scopes
fn scopeLookupExposedItem(self: *const Self, item_name: Ident.Idx) ?Scope.ExposedItemInfo {
    // Search from innermost to outermost scope
    var i = self.scopes.items.len;
    while (i > 0) {
        i -= 1;
        const scope = &self.scopes.items[i];

        switch (scope.lookupExposedItem(&self.can_ir.env.idents, item_name)) {
            .found => |item_info| return item_info,
            .not_found => continue,
        }
    }

    return null;
}

/// Introduce an exposed item into the current scope
fn scopeIntroduceExposedItem(self: *Self, item_name: Ident.Idx, item_info: Scope.ExposedItemInfo) void {
    const gpa = self.can_ir.env.gpa;
    const current_scope = &self.scopes.items[self.scopes.items.len - 1];

    // Simplified introduction without parent lookup for now
    const result = current_scope.introduceExposedItem(gpa, &self.can_ir.env.idents, item_name, item_info, null);

    switch (result) {
        .success => {},
        .shadowing_warning => |shadowed_info| {
            // Create diagnostic for exposed item shadowing
            const item_text = self.can_ir.env.idents.getText(item_name);
            const shadowed_module_text = self.can_ir.env.idents.getText(shadowed_info.module_name);
            const current_module_text = self.can_ir.env.idents.getText(item_info.module_name);

            // For now, just add a simple diagnostic message
            const message = std.fmt.allocPrint(gpa, "Exposed item '{s}' from module '{s}' shadows item from module '{s}'", .{ item_text, current_module_text, shadowed_module_text }) catch |err| collections.utils.exitOnOom(err);
            const message_str = self.can_ir.env.strings.insert(gpa, message);
            gpa.free(message);

            self.can_ir.pushDiagnostic(CIR.Diagnostic{
                .not_implemented = .{
                    .feature = message_str,
                    .region = Region.zero(), // TODO: Get proper region from import statement
                },
            });
        },
        .already_in_scope => |existing_info| {
            // Create diagnostic for duplicate exposed item
            const item_text = self.can_ir.env.idents.getText(item_name);
            const existing_module_text = self.can_ir.env.idents.getText(existing_info.module_name);
            const new_module_text = self.can_ir.env.idents.getText(item_info.module_name);

            const message = std.fmt.allocPrint(gpa, "Exposed item '{s}' already imported from module '{s}', cannot import again from module '{s}'", .{ item_text, existing_module_text, new_module_text }) catch |err| collections.utils.exitOnOom(err);
            const message_str = self.can_ir.env.strings.insert(gpa, message);
            gpa.free(message);

            self.can_ir.pushDiagnostic(CIR.Diagnostic{
                .not_implemented = .{
                    .feature = message_str,
                    .region = Region.zero(), // TODO: Get proper region from import statement
                },
            });
        },
    }
}

/// Look up an exposed item in parent scopes (for shadowing detection)
fn scopeLookupExposedItemInParentScopes(self: *const Self, item_name: Ident.Idx) ?Scope.ExposedItemInfo {
    // Search from second-innermost to outermost scope (excluding current scope)
    if (self.scopes.items.len <= 1) return null;

    var i = self.scopes.items.len - 1;
    while (i > 0) {
        i -= 1;
        const scope = &self.scopes.items[i];

        switch (scope.lookupExposedItem(&self.can_ir.env.idents, item_name)) {
            .found => |item_info| return item_info,
            .not_found => continue,
        }
    }

    return null;
}

/// Extract the module name from a full qualified name (e.g., "Json" from "json.Json")
fn extractModuleName(self: *Self, module_name_ident: Ident.Idx) Ident.Idx {
    const module_text = self.can_ir.env.idents.getText(module_name_ident);

    // Find the last dot and extract the part after it
    if (std.mem.lastIndexOf(u8, module_text, ".")) |last_dot_idx| {
        const extracted_name = module_text[last_dot_idx + 1 ..];
        return self.can_ir.env.idents.insert(self.can_ir.env.gpa, base.Ident.for_text(extracted_name), Region.zero());
    } else {
        // No dot found, return the original name
        return module_name_ident;
    }
}

/// Convert a parsed TypeAnno into a canonical TypeVar with appropriate Content
fn canonicalizeTypeAnnoToTypeVar(self: *Self, type_anno_idx: CIR.TypeAnno.Idx, parent_node_idx: Node.Idx, region: Region) TypeVar {
    const type_anno = self.can_ir.store.getTypeAnno(type_anno_idx);

    switch (type_anno) {
        .ty_var => |tv| {
            // Check if this type variable is already in scope
            const scope = self.currentScope();
            const ident_store = &self.can_ir.env.idents;

            switch (scope.lookupTypeVar(ident_store, tv.name)) {
                .found => |_| {
                    // Type variable already exists, create fresh var with same name
                    return self.can_ir.pushTypeVar(.{ .flex_var = tv.name }, parent_node_idx, region);
                },
                .not_found => {
                    // Create fresh flex var and add to scope
                    const fresh_var = self.can_ir.pushTypeVar(.{ .flex_var = tv.name }, parent_node_idx, region);

                    // Create a basic type annotation for the scope
                    const ty_var_anno = self.can_ir.store.addTypeAnno(.{ .ty_var = .{ .name = tv.name, .region = region } });

                    // Add to scope (simplified - ignoring result for now)
                    _ = scope.introduceTypeVar(self.can_ir.env.gpa, ident_store, tv.name, ty_var_anno, null);

                    return fresh_var;
                },
            }
        },
        .underscore => {
            // Create anonymous flex var
            return self.can_ir.pushFreshTypeVar(parent_node_idx, region);
        },
        .ty => |t| {
            // Look up built-in or user-defined type
            return self.canonicalizeBasicType(t.symbol, parent_node_idx, region);
        },
        .apply => |apply| {
            // Handle type application like List(String), Dict(a, b)
            return self.canonicalizeTypeApplication(apply, parent_node_idx, region);
        },
        .@"fn" => |func| {
            // Create function type
            return self.canonicalizeFunctionType(func, parent_node_idx, region);
        },
        .tuple => |tuple| {
            // Create tuple type
            return self.canonicalizeTupleType(tuple, parent_node_idx, region);
        },
        .record => |record| {
            // Create record type
            return self.canonicalizeRecordType(record, parent_node_idx, region);
        },
        .tag_union => |tag_union| {
            // Create tag union type
            return self.canonicalizeTagUnionType(tag_union, parent_node_idx, region);
        },
        .parens => |parens| {
            // Recursively canonicalize the inner type
            return self.canonicalizeTypeAnnoToTypeVar(parens.anno, parent_node_idx, region);
        },
        .mod_ty => |mod_ty| {
            // Handle module-qualified types
            return self.canonicalizeModuleType(mod_ty, parent_node_idx, region);
        },
        .malformed => {
            // Return error type for malformed annotations
            return self.can_ir.pushTypeVar(.err, parent_node_idx, region);
        },
    }
}

/// Handle basic type lookup (Bool, Str, Num, etc.)
fn canonicalizeBasicType(self: *Self, symbol: Ident.Idx, parent_node_idx: Node.Idx, region: Region) TypeVar {
    const name = self.can_ir.env.idents.getText(symbol);

    // Built-in types mapping
    if (std.mem.eql(u8, name, "Bool")) {
        return self.can_ir.pushTypeVar(.{ .flex_var = symbol }, parent_node_idx, region);
    } else if (std.mem.eql(u8, name, "Str")) {
        return self.can_ir.pushTypeVar(.{ .structure = .str }, parent_node_idx, region);
    } else if (std.mem.eql(u8, name, "Num")) {
        // Create a fresh TypeVar for the polymorphic number type
        const num_var = self.can_ir.pushFreshTypeVar(parent_node_idx, region);
        const num_requirements = types.Num.IntRequirements{
            .sign_needed = false,
            .bits_needed = 0, // 7 bits - most permissive for generic Num type
        };
        return self.can_ir.pushTypeVar(.{ .structure = .{ .num = .{ .num_poly = .{ .var_ = num_var, .requirements = num_requirements } } } }, parent_node_idx, region);
    } else if (std.mem.eql(u8, name, "U8")) {
        return self.can_ir.pushTypeVar(.{ .structure = .{ .num = .{ .num_compact = .{ .int = .u8 } } } }, parent_node_idx, region);
    } else if (std.mem.eql(u8, name, "U16")) {
        return self.can_ir.pushTypeVar(.{ .structure = .{ .num = .{ .num_compact = .{ .int = .u16 } } } }, parent_node_idx, region);
    } else if (std.mem.eql(u8, name, "U32")) {
        return self.can_ir.pushTypeVar(.{ .structure = .{ .num = .{ .num_compact = .{ .int = .u32 } } } }, parent_node_idx, region);
    } else if (std.mem.eql(u8, name, "U64")) {
        return self.can_ir.pushTypeVar(.{ .structure = .{ .num = .{ .num_compact = .{ .int = .u64 } } } }, parent_node_idx, region);
    } else if (std.mem.eql(u8, name, "U128")) {
        return self.can_ir.pushTypeVar(.{ .structure = .{ .num = .{ .num_compact = .{ .int = .u128 } } } }, parent_node_idx, region);
    } else if (std.mem.eql(u8, name, "I8")) {
        return self.can_ir.pushTypeVar(.{ .structure = .{ .num = .{ .num_compact = .{ .int = .i8 } } } }, parent_node_idx, region);
    } else if (std.mem.eql(u8, name, "I16")) {
        return self.can_ir.pushTypeVar(.{ .structure = .{ .num = .{ .num_compact = .{ .int = .i16 } } } }, parent_node_idx, region);
    } else if (std.mem.eql(u8, name, "I32")) {
        return self.can_ir.pushTypeVar(.{ .structure = .{ .num = .{ .num_compact = .{ .int = .i32 } } } }, parent_node_idx, region);
    } else if (std.mem.eql(u8, name, "I64")) {
        return self.can_ir.pushTypeVar(.{ .structure = .{ .num = .{ .num_compact = .{ .int = .i64 } } } }, parent_node_idx, region);
    } else if (std.mem.eql(u8, name, "I128")) {
        return self.can_ir.pushTypeVar(.{ .structure = .{ .num = .{ .num_compact = .{ .int = .i128 } } } }, parent_node_idx, region);
    } else if (std.mem.eql(u8, name, "F32")) {
        return self.can_ir.pushTypeVar(.{ .structure = .{ .num = .{ .num_compact = .{ .frac = .f32 } } } }, parent_node_idx, region);
    } else if (std.mem.eql(u8, name, "F64")) {
        return self.can_ir.pushTypeVar(.{ .structure = .{ .num = .{ .num_compact = .{ .frac = .f64 } } } }, parent_node_idx, region);
    } else if (std.mem.eql(u8, name, "Dec")) {
        return self.can_ir.pushTypeVar(.{ .structure = .{ .num = .{ .num_compact = .{ .frac = .dec } } } }, parent_node_idx, region);
    } else {
        // Look up user-defined type in scope
        const scope = self.currentScope();
        const ident_store = &self.can_ir.env.idents;

        switch (scope.lookupTypeDecl(ident_store, symbol)) {
            .found => |_| {
                return self.can_ir.pushTypeVar(.{ .flex_var = symbol }, parent_node_idx, region);
            },
            .not_found => {
                // Unknown type - create error type
                return self.can_ir.pushTypeVar(.err, parent_node_idx, region);
            },
        }
    }
}

/// Handle type applications like List(a), Dict(k, v)
fn canonicalizeTypeApplication(self: *Self, apply: anytype, parent_node_idx: Node.Idx, region: Region) TypeVar {
    // Simplified implementation - create a flex var for the applied type
    return self.can_ir.pushTypeVar(.{ .flex_var = apply.symbol }, parent_node_idx, region);
}

/// Handle function types like a -> b
fn canonicalizeFunctionType(self: *Self, func: anytype, parent_node_idx: Node.Idx, region: Region) TypeVar {
    const gpa = self.can_ir.env.gpa;

    // Canonicalize argument types and return type
    const args_slice = self.can_ir.store.sliceTypeAnnos(func.args);

    // Collect canonicalized argument type variables
    var arg_vars = std.ArrayList(types.Var).init(gpa);
    defer arg_vars.deinit();

    // For each argument, canonicalize its type and collect the type var
    for (args_slice) |arg_anno_idx| {
        const arg_type_var = self.canonicalizeTypeAnnoToTypeVar(arg_anno_idx, parent_node_idx, region);
        arg_vars.append(arg_type_var) catch |err| exitOnOom(err);
    }

    // Canonicalize return type
    const ret_type_var = self.canonicalizeTypeAnnoToTypeVar(func.ret, parent_node_idx, region);

    // Create the function args range
    const args_range = self.can_ir.env.types.appendFuncArgs(arg_vars.items);

    // Create effect variable based on effectfulness
    const eff_var = if (func.effectful)
        self.can_ir.pushTypeVar(.effectful, parent_node_idx, region)
    else
        self.can_ir.pushTypeVar(.pure, parent_node_idx, region);

    // Create the complete function structure
    return self.can_ir.pushTypeVar(
        .{ .structure = .{ .func = .{ .args = args_range, .ret = ret_type_var, .eff = eff_var } } },
        parent_node_idx,
        region,
    );
}

/// Handle tuple types like (a, b, c)
fn canonicalizeTupleType(self: *Self, tuple: anytype, parent_node_idx: Node.Idx, region: Region) TypeVar {
    _ = tuple;
    // Simplified implementation - create flex var for tuples
    return self.can_ir.pushFreshTypeVar(parent_node_idx, region);
}

/// Handle record types like { name: Str, age: Num }
fn canonicalizeRecordType(self: *Self, record: anytype, parent_node_idx: Node.Idx, region: Region) TypeVar {
    // Create fresh type variables for each field
    var type_record_fields = std.ArrayList(types.RecordField).init(self.can_ir.env.gpa);
    defer type_record_fields.deinit();

    // Process each field in the record type annotation
    for (self.can_ir.store.sliceAnnoRecordFields(record.fields)) |field_idx| {
        const field = self.can_ir.store.getAnnoRecordField(field_idx);

        // Canonicalize the field's type annotation
        const field_type_var = self.canonicalizeTypeAnnoToTypeVar(field.ty, parent_node_idx, region);

        type_record_fields.append(types.RecordField{
            .name = field.name,
            .var_ = field_type_var,
        }) catch |err| exitOnOom(err);
    }

    // Create the record type structure
    const type_fields_range = self.can_ir.env.types.appendRecordFields(type_record_fields.items);
    const ext_var = self.can_ir.pushTypeVar(.{ .structure = .empty_record }, parent_node_idx, region);

    return self.can_ir.pushTypeVar(
        .{ .structure = .{ .record = .{ .fields = type_fields_range, .ext = ext_var } } },
        parent_node_idx,
        region,
    );
}

/// Handle tag union types like [Some(a), None]
fn canonicalizeTagUnionType(self: *Self, tag_union: anytype, parent_node_idx: Node.Idx, region: Region) TypeVar {
    _ = tag_union;
    // Simplified implementation - create flex var for tag unions
    return self.can_ir.pushFreshTypeVar(parent_node_idx, region);
}

/// Handle module-qualified types like Json.Decoder
fn canonicalizeModuleType(self: *Self, mod_ty: anytype, parent_node_idx: Node.Idx, region: Region) TypeVar {
    // Simplified implementation - create flex var for module types
    return self.can_ir.pushTypeVar(.{ .flex_var = mod_ty.ty_symbol }, parent_node_idx, region);
}

/// Create an annotation from a type annotation
fn createAnnotationFromTypeAnno(self: *Self, type_anno_idx: CIR.TypeAnno.Idx, _: TypeVar, region: Region) ?CIR.Annotation.Idx {
    // Convert the type annotation to a type variable
    const signature = self.canonicalizeTypeAnnoToTypeVar(type_anno_idx, @enumFromInt(0), region);

    // Create the annotation structure
    const annotation = CIR.Annotation{
        .type_anno = type_anno_idx,
        .signature = signature,
        .region = region,
    };

    // Add to NodeStore and return the index
    const annotation_idx = self.can_ir.store.addAnnotation(annotation);

    return annotation_idx;
}

/// Try to handle field access as a module-qualified lookup.
///
/// Examples:
/// - `Json.utf8` where `Json` is a module alias and `utf8` is an exposed function
/// - `Http.get` where `Http` is imported and `get` is available in that module
///
/// Returns `null` if this is not a module-qualified lookup (e.g., regular field access like `user.name`)
fn tryModuleQualifiedLookup(self: *Self, field_access: AST.BinOp) ?CIR.Expr.Idx {
    const left_expr = self.parse_ir.store.getExpr(field_access.left);
    if (left_expr != .ident) return null;

    const left_ident = left_expr.ident;
    const module_alias = self.parse_ir.tokens.resolveIdentifier(left_ident.token) orelse return null;

    // Check if this is a module alias
    const module_name = self.scopeLookupModule(module_alias) orelse return null;

    // This is a module-qualified lookup
    const right_expr = self.parse_ir.store.getExpr(field_access.right);
    if (right_expr != .ident) return null;

    const right_ident = right_expr.ident;
    const field_name = self.parse_ir.tokens.resolveIdentifier(right_ident.token) orelse return null;

    // Create qualified name by slicing from original source text
    // The field_access region covers the entire "Module.field" span
    const region = self.parse_ir.tokenizedRegionToRegion(field_access.region);
    const source_text = self.parse_ir.source[region.start.offset..region.end.offset];

    const qualified_name = self.can_ir.env.idents.insert(self.can_ir.env.gpa, base.Ident.for_text(source_text), region);

    // Create external declaration
    const external_decl = CIR.ExternalDecl{
        .qualified_name = qualified_name,
        .module_name = module_name,
        .local_name = field_name,
        .type_var = self.can_ir.pushFreshTypeVar(@enumFromInt(0), region),
        .kind = .value,
        .region = region,
    };

    const external_idx = self.can_ir.pushExternalDecl(external_decl);

    // Create lookup expression for external declaration
    const expr_idx = self.can_ir.store.addExpr(CIR.Expr{ .e_lookup = .{ .external = external_idx } });
    _ = self.can_ir.setTypeVarAtExpr(expr_idx, Content{ .flex_var = null });
    return expr_idx;
}

/// Canonicalize regular field access (not module-qualified).
///
/// Examples:
/// - `user.name` - accessing a field on a record
/// - `list.map(transform)` - calling a method with arguments
/// - `result.isOk` - accessing a field that might be a function
fn canonicalizeRegularFieldAccess(self: *Self, field_access: AST.BinOp) std.mem.Allocator.Error!?CIR.Expr.Idx {
    // Canonicalize the receiver (left side of the dot)
    const receiver_idx = try self.canonicalizeFieldAccessReceiver(field_access) orelse return null;

    // Parse the right side - this could be just a field name or a method call
    const field_name, const args = try self.parseFieldAccessRight(field_access);

    const dot_access_expr = CIR.Expr{
        .e_dot_access = .{
            .receiver = receiver_idx,
            .field_name = field_name,
            .args = args,
            .region = self.parse_ir.tokenizedRegionToRegion(field_access.region),
        },
    };

    const expr_idx = self.can_ir.store.addExpr(dot_access_expr);
    _ = self.can_ir.setTypeVarAtExpr(expr_idx, Content{ .flex_var = null });
    return expr_idx;
}

/// Canonicalize the receiver (left side) of field access.
///
/// Examples:
/// - In `user.name`, canonicalizes `user`
/// - In `getUser().email`, canonicalizes `getUser()`
/// - In `[1,2,3].map(fn)`, canonicalizes `[1,2,3]`
fn canonicalizeFieldAccessReceiver(self: *Self, field_access: AST.BinOp) std.mem.Allocator.Error!?CIR.Expr.Idx {
    if (try self.canonicalize_expr(field_access.left)) |idx| {
        return idx;
    } else {
        // Failed to canonicalize receiver, return malformed
        const region = self.parse_ir.tokenizedRegionToRegion(field_access.region);
        return self.can_ir.pushMalformed(CIR.Expr.Idx, CIR.Diagnostic{ .expr_not_canonicalized = .{
            .region = region,
        } });
    }
}

/// Parse the right side of field access, handling both plain fields and method calls.
///
/// Examples:
/// - `user.name` - returns `("name", null)` for plain field access
/// - `list.map(fn)` - returns `("map", args)` where args contains the canonicalized function
/// - `obj.method(a, b)` - returns `("method", args)` where args contains canonicalized a and b
fn parseFieldAccessRight(self: *Self, field_access: AST.BinOp) std.mem.Allocator.Error!struct { Ident.Idx, ?CIR.Expr.Span } {
    const right_expr = self.parse_ir.store.getExpr(field_access.right);

    return switch (right_expr) {
        .apply => |apply| self.parseMethodCall(apply),
        .ident => |ident| .{ self.resolveIdentOrFallback(ident.token), null },
        else => .{ self.createUnknownIdent(), null },
    };
}

/// Parse a method call on the right side of field access.
///
/// Examples:
/// - `.map(transform)` - extracts "map" as method name and canonicalizes `transform` argument
/// - `.filter(predicate)` - extracts "filter" and canonicalizes `predicate`
/// - `.fold(0, combine)` - extracts "fold" and canonicalizes both `0` and `combine` arguments
fn parseMethodCall(self: *Self, apply: @TypeOf(@as(AST.Expr, undefined).apply)) std.mem.Allocator.Error!struct { Ident.Idx, ?CIR.Expr.Span } {
    const method_expr = self.parse_ir.store.getExpr(apply.@"fn");
    const field_name = switch (method_expr) {
        .ident => |ident| self.resolveIdentOrFallback(ident.token),
        else => self.createUnknownIdent(),
    };

    // Canonicalize the arguments using scratch system
    const scratch_top = self.can_ir.store.scratchExprTop();
    for (self.parse_ir.store.exprSlice(apply.args)) |arg_idx| {
        if (try self.canonicalize_expr(arg_idx)) |canonicalized| {
            self.can_ir.store.addScratchExpr(canonicalized);
        } else {
            self.can_ir.store.clearScratchExprsFrom(scratch_top);
            return .{ field_name, null };
        }
    }
    const args = self.can_ir.store.exprSpanFrom(scratch_top);

    return .{ field_name, args };
}

/// Resolve an identifier token or return a fallback "unknown" identifier.
///
/// This helps maintain the "inform don't block" philosophy - even if we can't
/// resolve an identifier (due to malformed input), we continue compilation.
///
/// Examples:
/// - Valid token for "name" -> returns the interned identifier for "name"
/// - Malformed/missing token -> returns identifier for "unknown"
fn resolveIdentOrFallback(self: *Self, token: Token.Idx) Ident.Idx {
    if (self.parse_ir.tokens.resolveIdentifier(token)) |ident_idx| {
        return ident_idx;
    } else {
        return self.createUnknownIdent();
    }
}

/// Create an "unknown" identifier for fallback cases.
///
/// Used when we encounter malformed or unexpected syntax but want to continue
/// compilation instead of stopping. This supports the compiler's "inform don't block" approach.
fn createUnknownIdent(self: *Self) Ident.Idx {
    return self.can_ir.env.idents.insert(self.can_ir.env.gpa, base.Ident.for_text("unknown"), Region.zero());
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

test "type variables are tracked separately from value identifiers" {
    const gpa = std.testing.allocator;

    var ctx = try ScopeTestContext.init(gpa);
    defer ctx.deinit();

    // Create identifiers for 'a' - one for value, one for type
    const a_ident = ctx.env.idents.insert(gpa, base.Ident.for_text("a"), base.Region.zero());
    const pattern: CIR.Pattern.Idx = @enumFromInt(1);
    const type_anno: CIR.TypeAnno.Idx = @enumFromInt(1);

    // Introduce 'a' as a value identifier
    const value_result = ctx.self.scopeIntroduceInternal(gpa, &ctx.env.idents, .ident, a_ident, pattern, false, true);
    try std.testing.expectEqual(Scope.IntroduceResult{ .success = {} }, value_result);

    // Introduce 'a' as a type variable - should succeed because they're in separate namespaces
    const current_scope = &ctx.self.scopes.items[ctx.self.scopes.items.len - 1];
    const type_result = current_scope.introduceTypeVar(gpa, &ctx.env.idents, a_ident, type_anno, null);
    try std.testing.expectEqual(Scope.TypeVarIntroduceResult{ .success = {} }, type_result);

    // Lookup 'a' as value should find the pattern
    const value_lookup = ctx.self.scopeLookup(&ctx.env.idents, .ident, a_ident);
    try std.testing.expectEqual(Scope.LookupResult{ .found = pattern }, value_lookup);

    // Lookup 'a' as type variable should find the type annotation
    const type_lookup = current_scope.lookupTypeVar(&ctx.env.idents, a_ident);
    try std.testing.expectEqual(Scope.TypeVarLookupResult{ .found = type_anno }, type_lookup);
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
        expected_bits_needed: u8,
    }{
        // Basic hex literals
        .{ .literal = "0x0", .expected_value = 0, .expected_sign_needed = false, .expected_bits_needed = 0 },
        .{ .literal = "0x1", .expected_value = 1, .expected_sign_needed = false, .expected_bits_needed = 0 },
        .{ .literal = "0xFF", .expected_value = 255, .expected_sign_needed = false, .expected_bits_needed = 1 },
        .{ .literal = "0x100", .expected_value = 256, .expected_sign_needed = false, .expected_bits_needed = 2 },
        .{ .literal = "0xFFFF", .expected_value = 65535, .expected_sign_needed = false, .expected_bits_needed = 3 },
        .{ .literal = "0x10000", .expected_value = 65536, .expected_sign_needed = false, .expected_bits_needed = 4 },
        .{ .literal = "0xFFFFFFFF", .expected_value = 4294967295, .expected_sign_needed = false, .expected_bits_needed = 5 },
        .{ .literal = "0x100000000", .expected_value = 4294967296, .expected_sign_needed = false, .expected_bits_needed = 6 },
        .{ .literal = "0xFFFFFFFFFFFFFFFF", .expected_value = @as(i128, @bitCast(@as(u128, 18446744073709551615))), .expected_sign_needed = false, .expected_bits_needed = 7 },

        // Hex with underscores
        .{ .literal = "0x1_000", .expected_value = 4096, .expected_sign_needed = false, .expected_bits_needed = 2 },
        .{ .literal = "0xFF_FF", .expected_value = 65535, .expected_sign_needed = false, .expected_bits_needed = 3 },
        .{ .literal = "0x1234_5678_9ABC_DEF0", .expected_value = @as(i128, @bitCast(@as(u128, 0x123456789ABCDEF0))), .expected_sign_needed = false, .expected_bits_needed = 6 },

        // Negative hex literals
        .{ .literal = "-0x1", .expected_value = -1, .expected_sign_needed = true, .expected_bits_needed = 0 },
        .{ .literal = "-0x80", .expected_value = -128, .expected_sign_needed = true, .expected_bits_needed = 0 },
        .{ .literal = "-0x81", .expected_value = -129, .expected_sign_needed = true, .expected_bits_needed = 1 },
        .{ .literal = "-0x8000", .expected_value = -32768, .expected_sign_needed = true, .expected_bits_needed = 2 },
        .{ .literal = "-0x8001", .expected_value = -32769, .expected_sign_needed = true, .expected_bits_needed = 3 },
        .{ .literal = "-0x80000000", .expected_value = -2147483648, .expected_sign_needed = true, .expected_bits_needed = 4 },
        .{ .literal = "-0x80000001", .expected_value = -2147483649, .expected_sign_needed = true, .expected_bits_needed = 5 },
        .{ .literal = "-0x8000000000000000", .expected_value = -9223372036854775808, .expected_sign_needed = true, .expected_bits_needed = 6 },
        .{ .literal = "-0x8000000000000001", .expected_value = @as(i128, -9223372036854775809), .expected_sign_needed = true, .expected_bits_needed = 7 },
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
        const canonical_expr_idx = try can.canonicalize_expr(expr_idx) orelse {
            std.debug.print("Failed to canonicalize: {s}\n", .{tc.literal});
            try std.testing.expect(false);
            continue;
        };

        const expr = cir.store.getExpr(canonical_expr_idx);
        try std.testing.expect(expr == .e_int);

        // Check the value
        try std.testing.expectEqual(tc.expected_value, @as(i128, @bitCast(expr.e_int.value.bytes)));

        const expr_as_type_var: types.Var = @enumFromInt(@intFromEnum(canonical_expr_idx));
        const resolved = env.types.resolveVar(expr_as_type_var);
        switch (resolved.desc.content) {
            .structure => |structure| switch (structure) {
                .num => |num| switch (num) {
                    .num_poly => |poly| {
                        try std.testing.expectEqual(tc.expected_sign_needed, poly.requirements.sign_needed);
                        try std.testing.expectEqual(tc.expected_bits_needed, poly.requirements.bits_needed);
                    },
                    .int_poly => |poly| {
                        try std.testing.expectEqual(tc.expected_sign_needed, poly.requirements.sign_needed);
                        try std.testing.expectEqual(tc.expected_bits_needed, poly.requirements.bits_needed);
                    },
                    .num_unbound => |requirements| {
                        try std.testing.expectEqual(tc.expected_sign_needed, requirements.sign_needed);
                        try std.testing.expectEqual(tc.expected_bits_needed, requirements.bits_needed);
                    },
                    .int_unbound => |requirements| {
                        try std.testing.expectEqual(tc.expected_sign_needed, requirements.sign_needed);
                        try std.testing.expectEqual(tc.expected_bits_needed, requirements.bits_needed);
                    },
                    else => return error.UnexpectedNumType,
                },
                else => return error.UnexpectedStructureType,
            },
            else => return error.UnexpectedContentType,
        }
    }
}

test "binary integer literals" {
    const test_cases = [_]struct {
        literal: []const u8,
        expected_value: i128,
        expected_sign_needed: bool,
        expected_bits_needed: u8,
    }{
        // Basic binary literals
        .{ .literal = "0b0", .expected_value = 0, .expected_sign_needed = false, .expected_bits_needed = 0 },
        .{ .literal = "0b1", .expected_value = 1, .expected_sign_needed = false, .expected_bits_needed = 0 },
        .{ .literal = "0b10", .expected_value = 2, .expected_sign_needed = false, .expected_bits_needed = 0 },
        .{ .literal = "0b11111111", .expected_value = 255, .expected_sign_needed = false, .expected_bits_needed = 1 },
        .{ .literal = "0b100000000", .expected_value = 256, .expected_sign_needed = false, .expected_bits_needed = 2 },
        .{ .literal = "0b1111111111111111", .expected_value = 65535, .expected_sign_needed = false, .expected_bits_needed = 3 },
        .{ .literal = "0b10000000000000000", .expected_value = 65536, .expected_sign_needed = false, .expected_bits_needed = 4 },

        // Binary with underscores
        .{ .literal = "0b11_11", .expected_value = 15, .expected_sign_needed = false, .expected_bits_needed = 0 },
        .{ .literal = "0b1111_1111", .expected_value = 255, .expected_sign_needed = false, .expected_bits_needed = 1 },
        .{ .literal = "0b1_0000_0000", .expected_value = 256, .expected_sign_needed = false, .expected_bits_needed = 2 },
        .{ .literal = "0b1010_1010_1010_1010", .expected_value = 43690, .expected_sign_needed = false, .expected_bits_needed = 3 },

        // Negative binary
        .{ .literal = "-0b1", .expected_value = -1, .expected_sign_needed = true, .expected_bits_needed = 0 },
        .{ .literal = "-0b10000000", .expected_value = -128, .expected_sign_needed = true, .expected_bits_needed = 0 },
        .{ .literal = "-0b10000001", .expected_value = -129, .expected_sign_needed = true, .expected_bits_needed = 1 },
        .{ .literal = "-0b1000000000000000", .expected_value = -32768, .expected_sign_needed = true, .expected_bits_needed = 2 },
        .{ .literal = "-0b1000000000000001", .expected_value = -32769, .expected_sign_needed = true, .expected_bits_needed = 3 },
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
        const canonical_expr_idx = try can.canonicalize_expr(expr_idx) orelse {
            std.debug.print("Failed to canonicalize: {s}\n", .{tc.literal});
            try std.testing.expect(false);
            continue;
        };

        const expr = cir.store.getExpr(canonical_expr_idx);
        try std.testing.expect(expr == .e_int);

        // Check the value
        try std.testing.expectEqual(tc.expected_value, @as(i128, @bitCast(expr.e_int.value.bytes)));

        const expr_as_type_var: types.Var = @enumFromInt(@intFromEnum(canonical_expr_idx));
        const resolved = env.types.resolveVar(expr_as_type_var);
        switch (resolved.desc.content) {
            .structure => |structure| switch (structure) {
                .num => |num| switch (num) {
                    .num_poly => |poly| {
                        try std.testing.expectEqual(tc.expected_sign_needed, poly.requirements.sign_needed);
                        try std.testing.expectEqual(tc.expected_bits_needed, poly.requirements.bits_needed);
                    },
                    .int_poly => |poly| {
                        try std.testing.expectEqual(tc.expected_sign_needed, poly.requirements.sign_needed);
                        try std.testing.expectEqual(tc.expected_bits_needed, poly.requirements.bits_needed);
                    },
                    .num_unbound => |requirements| {
                        try std.testing.expectEqual(tc.expected_sign_needed, requirements.sign_needed);
                        try std.testing.expectEqual(tc.expected_bits_needed, requirements.bits_needed);
                    },
                    .int_unbound => |requirements| {
                        try std.testing.expectEqual(tc.expected_sign_needed, requirements.sign_needed);
                        try std.testing.expectEqual(tc.expected_bits_needed, requirements.bits_needed);
                    },
                    else => return error.UnexpectedNumType,
                },
                else => return error.UnexpectedStructureType,
            },
            else => return error.UnexpectedContentType,
        }
    }
}

test "octal integer literals" {
    const test_cases = [_]struct {
        literal: []const u8,
        expected_value: i128,
        expected_sign_needed: bool,
        expected_bits_needed: u8,
    }{
        // Basic octal literals
        .{ .literal = "0o0", .expected_value = 0, .expected_sign_needed = false, .expected_bits_needed = 0 },
        .{ .literal = "0o1", .expected_value = 1, .expected_sign_needed = false, .expected_bits_needed = 0 },
        .{ .literal = "0o7", .expected_value = 7, .expected_sign_needed = false, .expected_bits_needed = 0 },
        .{ .literal = "0o10", .expected_value = 8, .expected_sign_needed = false, .expected_bits_needed = 0 },
        .{ .literal = "0o377", .expected_value = 255, .expected_sign_needed = false, .expected_bits_needed = 1 },
        .{ .literal = "0o400", .expected_value = 256, .expected_sign_needed = false, .expected_bits_needed = 2 },
        .{ .literal = "0o177777", .expected_value = 65535, .expected_sign_needed = false, .expected_bits_needed = 3 },
        .{ .literal = "0o200000", .expected_value = 65536, .expected_sign_needed = false, .expected_bits_needed = 4 },

        // Octal with underscores
        .{ .literal = "0o377_377", .expected_value = 130815, .expected_sign_needed = false, .expected_bits_needed = 4 },
        .{ .literal = "0o1_234_567", .expected_value = 342391, .expected_sign_needed = false, .expected_bits_needed = 4 },

        // Negative octal literals
        .{ .literal = "-0o1", .expected_value = -1, .expected_sign_needed = true, .expected_bits_needed = 0 },
        .{ .literal = "-0o100", .expected_value = -64, .expected_sign_needed = true, .expected_bits_needed = 0 },
        .{ .literal = "-0o200", .expected_value = -128, .expected_sign_needed = true, .expected_bits_needed = 0 },
        .{ .literal = "-0o201", .expected_value = -129, .expected_sign_needed = true, .expected_bits_needed = 1 },
        .{ .literal = "-0o100000", .expected_value = -32768, .expected_sign_needed = true, .expected_bits_needed = 2 },
        .{ .literal = "-0o100001", .expected_value = -32769, .expected_sign_needed = true, .expected_bits_needed = 3 },
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
        const canonical_expr_idx = try can.canonicalize_expr(expr_idx) orelse {
            std.debug.print("Failed to canonicalize: {s}\n", .{tc.literal});
            try std.testing.expect(false);
            continue;
        };

        const expr = cir.store.getExpr(canonical_expr_idx);
        try std.testing.expect(expr == .e_int);

        // Check the value
        try std.testing.expectEqual(tc.expected_value, @as(i128, @bitCast(expr.e_int.value.bytes)));

        const expr_as_type_var: types.Var = @enumFromInt(@intFromEnum(canonical_expr_idx));
        const resolved = env.types.resolveVar(expr_as_type_var);
        switch (resolved.desc.content) {
            .structure => |structure| switch (structure) {
                .num => |num| switch (num) {
                    .num_poly => |poly| {
                        try std.testing.expectEqual(tc.expected_sign_needed, poly.requirements.sign_needed);
                        try std.testing.expectEqual(tc.expected_bits_needed, poly.requirements.bits_needed);
                    },
                    .int_poly => |poly| {
                        try std.testing.expectEqual(tc.expected_sign_needed, poly.requirements.sign_needed);
                        try std.testing.expectEqual(tc.expected_bits_needed, poly.requirements.bits_needed);
                    },
                    .num_unbound => |requirements| {
                        try std.testing.expectEqual(tc.expected_sign_needed, requirements.sign_needed);
                        try std.testing.expectEqual(tc.expected_bits_needed, requirements.bits_needed);
                    },
                    .int_unbound => |requirements| {
                        try std.testing.expectEqual(tc.expected_sign_needed, requirements.sign_needed);
                        try std.testing.expectEqual(tc.expected_bits_needed, requirements.bits_needed);
                    },
                    else => return error.UnexpectedNumType,
                },
                else => return error.UnexpectedStructureType,
            },
            else => return error.UnexpectedContentType,
        }
    }
}

test "integer literals with uppercase base prefixes" {
    const test_cases = [_]struct {
        literal: []const u8,
        expected_value: i128,
        expected_sign_needed: bool,
        expected_bits_needed: u8,
    }{
        // Uppercase hex prefix
        .{ .literal = "0X0", .expected_value = 0, .expected_sign_needed = false, .expected_bits_needed = 0 },
        .{ .literal = "0X1", .expected_value = 1, .expected_sign_needed = false, .expected_bits_needed = 0 },
        .{ .literal = "0XFF", .expected_value = 255, .expected_sign_needed = false, .expected_bits_needed = 1 },
        .{ .literal = "0XABCD", .expected_value = 43981, .expected_sign_needed = false, .expected_bits_needed = 3 },

        // Uppercase binary prefix
        .{ .literal = "0B0", .expected_value = 0, .expected_sign_needed = false, .expected_bits_needed = 0 },
        .{ .literal = "0B1", .expected_value = 1, .expected_sign_needed = false, .expected_bits_needed = 0 },
        .{ .literal = "0B1111", .expected_value = 15, .expected_sign_needed = false, .expected_bits_needed = 0 },
        .{ .literal = "0B11111111", .expected_value = 255, .expected_sign_needed = false, .expected_bits_needed = 1 },

        // Uppercase octal prefix
        .{ .literal = "0O0", .expected_value = 0, .expected_sign_needed = false, .expected_bits_needed = 0 },
        .{ .literal = "0O7", .expected_value = 7, .expected_sign_needed = false, .expected_bits_needed = 0 },
        .{ .literal = "0O377", .expected_value = 255, .expected_sign_needed = false, .expected_bits_needed = 1 },
        .{ .literal = "0O777", .expected_value = 511, .expected_sign_needed = false, .expected_bits_needed = 2 },

        // Mixed case in value (should still work)
        .{ .literal = "0xAbCd", .expected_value = 43981, .expected_sign_needed = false, .expected_bits_needed = 3 },
        .{ .literal = "0XaBcD", .expected_value = 43981, .expected_sign_needed = false, .expected_bits_needed = 3 },
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
        const canonical_expr_idx = try can.canonicalize_expr(expr_idx) orelse {
            std.debug.print("Failed to canonicalize: {s}\n", .{tc.literal});
            try std.testing.expect(false);
            continue;
        };

        const expr = cir.store.getExpr(canonical_expr_idx);
        try std.testing.expect(expr == .e_int);

        // Check the value
        try std.testing.expectEqual(tc.expected_value, @as(i128, @bitCast(expr.e_int.value.bytes)));

        const expr_as_type_var: types.Var = @enumFromInt(@intFromEnum(canonical_expr_idx));
        const resolved = env.types.resolveVar(expr_as_type_var);
        switch (resolved.desc.content) {
            .structure => |structure| switch (structure) {
                .num => |num| switch (num) {
                    .num_poly => |poly| {
                        try std.testing.expectEqual(tc.expected_sign_needed, poly.requirements.sign_needed);
                        try std.testing.expectEqual(tc.expected_bits_needed, poly.requirements.bits_needed);
                    },
                    .int_poly => |poly| {
                        try std.testing.expectEqual(tc.expected_sign_needed, poly.requirements.sign_needed);
                        try std.testing.expectEqual(tc.expected_bits_needed, poly.requirements.bits_needed);
                    },
                    .num_unbound => |requirements| {
                        try std.testing.expectEqual(tc.expected_sign_needed, requirements.sign_needed);
                        try std.testing.expectEqual(tc.expected_bits_needed, requirements.bits_needed);
                    },
                    .int_unbound => |requirements| {
                        try std.testing.expectEqual(tc.expected_sign_needed, requirements.sign_needed);
                        try std.testing.expectEqual(tc.expected_bits_needed, requirements.bits_needed);
                    },
                    else => return error.UnexpectedNumType,
                },
                else => return error.UnexpectedStructureType,
            },
            else => return error.UnexpectedContentType,
        }
    }
}

test "numeric literal patterns use pattern idx as type var" {
    var gpa_state = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
    defer std.debug.assert(gpa_state.deinit() == .ok);
    const gpa = gpa_state.allocator();

    // Test that int literal patterns work and use the pattern index as the type variable
    {
        var env = base.ModuleEnv.init(gpa);
        defer env.deinit();

        var cir = CIR.init(&env);
        defer cir.deinit();

        // Create an int literal pattern directly
        const int_pattern = CIR.Pattern{
            .int_literal = .{
                .value = .{ .bytes = @bitCast(@as(i128, 42)), .kind = .i128 },
                .region = Region.zero(),
            },
        };

        const pattern_idx = cir.store.addPattern(int_pattern);

        // Set the type variable for the pattern
        _ = cir.setTypeVarAtPat(pattern_idx, Content{
            .structure = .{ .num = .{ .num_unbound = .{
                .sign_needed = false,
                .bits_needed = 0,
            } } },
        });

        // Verify the stored pattern
        const stored_pattern = cir.store.getPattern(pattern_idx);
        try std.testing.expect(stored_pattern == .int_literal);
        try std.testing.expectEqual(@as(i128, 42), @as(i128, @bitCast(stored_pattern.int_literal.value.bytes)));

        // Verify the pattern index can be used as a type variable
        const pattern_as_type_var: types.Var = @enumFromInt(@intFromEnum(pattern_idx));
        const resolved = env.types.resolveVar(pattern_as_type_var);
        switch (resolved.desc.content) {
            .structure => |structure| switch (structure) {
                .num => |num| switch (num) {
                    .num_unbound => |requirements| {
                        try std.testing.expectEqual(false, requirements.sign_needed);
                        try std.testing.expectEqual(@as(u8, 0), requirements.bits_needed);
                    },
                    else => return error.UnexpectedNumType,
                },
                else => return error.UnexpectedStructureType,
            },
            else => return error.UnexpectedContentType,
        }
    }

    // Test that f64 literal patterns work
    {
        var env = base.ModuleEnv.init(gpa);
        defer env.deinit();

        var cir = CIR.init(&env);
        defer cir.deinit();

        // Create an f64 literal pattern directly
        const f64_pattern = CIR.Pattern{
            .f64_literal = .{
                .value = 3.14,
                .region = Region.zero(),
            },
        };

        const pattern_idx = cir.store.addPattern(f64_pattern);

        // Set the type variable for the pattern
        _ = cir.setTypeVarAtPat(pattern_idx, Content{
            .structure = .{ .num = .{ .frac_unbound = .{
                .fits_in_f32 = true,
                .fits_in_dec = true,
            } } },
        });

        // Verify the stored pattern
        const stored_pattern = cir.store.getPattern(pattern_idx);
        try std.testing.expect(stored_pattern == .f64_literal);
        try std.testing.expectApproxEqAbs(@as(f64, 3.14), stored_pattern.f64_literal.value, 0.001);

        // Verify the pattern index can be used as a type variable
        const pattern_as_type_var: types.Var = @enumFromInt(@intFromEnum(pattern_idx));
        const resolved = env.types.resolveVar(pattern_as_type_var);
        switch (resolved.desc.content) {
            .structure => |structure| switch (structure) {
                .num => |num| switch (num) {
                    .frac_unbound => |requirements| {
                        try std.testing.expectEqual(true, requirements.fits_in_f32);
                        try std.testing.expectEqual(true, requirements.fits_in_dec);
                    },
                    else => return error.UnexpectedNumType,
                },
                else => return error.UnexpectedStructureType,
            },
            else => return error.UnexpectedContentType,
        }
    }
}

test "numeric pattern types: unbound vs polymorphic" {
    var gpa_state = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
    defer std.debug.assert(gpa_state.deinit() == .ok);
    const gpa = gpa_state.allocator();

    // Test int_unbound pattern
    {
        var env = base.ModuleEnv.init(gpa);
        defer env.deinit();

        var cir = CIR.init(&env);
        defer cir.deinit();

        const pattern = CIR.Pattern{
            .int_literal = .{
                .value = .{ .bytes = @bitCast(@as(i128, -17)), .kind = .i128 },
                .region = Region.zero(),
            },
        };

        const pattern_idx = cir.store.addPattern(pattern);

        // Set as int_unbound (used when pattern matching where type is not yet known)
        _ = cir.setTypeVarAtPat(pattern_idx, Content{
            .structure = .{ .num = .{ .int_unbound = .{
                .sign_needed = true,
                .bits_needed = 0,
            } } },
        });

        const pattern_var: types.Var = @enumFromInt(@intFromEnum(pattern_idx));
        const resolved = env.types.resolveVar(pattern_var);
        switch (resolved.desc.content) {
            .structure => |s| switch (s) {
                .num => |n| switch (n) {
                    .int_unbound => |req| {
                        try std.testing.expectEqual(true, req.sign_needed);
                        try std.testing.expectEqual(@as(u8, 0), req.bits_needed);
                    },
                    else => return error.ExpectedIntUnbound,
                },
                else => {},
            },
            else => {},
        }
    }

    // Test int_poly pattern (polymorphic integer that can be different int types)
    {
        var env = base.ModuleEnv.init(gpa);
        defer env.deinit();

        var cir = CIR.init(&env);
        defer cir.deinit();

        const pattern = CIR.Pattern{
            .int_literal = .{
                .value = .{ .bytes = @bitCast(@as(i128, 255)), .kind = .i128 },
                .region = Region.zero(),
            },
        };

        const pattern_idx = cir.store.addPattern(pattern);

        // Create a fresh type variable for polymorphic int
        const poly_var = env.types.fresh();

        // Set as int_poly (can be any integer type that satisfies requirements)
        _ = cir.setTypeVarAtPat(pattern_idx, Content{
            .structure = .{
                .num = .{
                    .int_poly = .{
                        .var_ = poly_var,
                        .requirements = .{
                            .sign_needed = false,
                            .bits_needed = 1, // Needs at least 8 bits for 255
                        },
                    },
                },
            },
        });

        const pattern_var: types.Var = @enumFromInt(@intFromEnum(pattern_idx));
        const resolved = env.types.resolveVar(pattern_var);
        switch (resolved.desc.content) {
            .structure => |s| switch (s) {
                .num => |n| switch (n) {
                    .int_poly => |poly| {
                        try std.testing.expectEqual(false, poly.requirements.sign_needed);
                        try std.testing.expectEqual(@as(u8, 1), poly.requirements.bits_needed);
                        try std.testing.expectEqual(poly_var, poly.var_);
                    },
                    else => return error.ExpectedIntPoly,
                },
                else => {},
            },
            else => {},
        }
    }

    // Test num_unbound pattern (can be int or frac)
    {
        var env = base.ModuleEnv.init(gpa);
        defer env.deinit();

        var cir = CIR.init(&env);
        defer cir.deinit();

        const pattern = CIR.Pattern{
            .int_literal = .{
                .value = .{ .bytes = @bitCast(@as(i128, 10)), .kind = .i128 },
                .region = Region.zero(),
            },
        };

        const pattern_idx = cir.store.addPattern(pattern);

        // Set as num_unbound (decimal literal that could be int or frac)
        _ = cir.setTypeVarAtPat(pattern_idx, Content{
            .structure = .{ .num = .{ .num_unbound = .{
                .sign_needed = false,
                .bits_needed = 0,
            } } },
        });

        const pattern_var: types.Var = @enumFromInt(@intFromEnum(pattern_idx));
        const resolved = env.types.resolveVar(pattern_var);
        switch (resolved.desc.content) {
            .structure => |s| switch (s) {
                .num => |n| switch (n) {
                    .num_unbound => |req| {
                        try std.testing.expectEqual(false, req.sign_needed);
                        try std.testing.expectEqual(@as(u8, 0), req.bits_needed);
                    },
                    else => return error.ExpectedNumUnbound,
                },
                else => {},
            },
            else => {},
        }
    }

    // Test num_poly pattern (polymorphic num that can be int or frac)
    {
        var env = base.ModuleEnv.init(gpa);
        defer env.deinit();

        var cir = CIR.init(&env);
        defer cir.deinit();

        const pattern = CIR.Pattern{
            .int_literal = .{
                .value = .{ .bytes = @bitCast(@as(i128, 5)), .kind = .i128 },
                .region = Region.zero(),
            },
        };

        const pattern_idx = cir.store.addPattern(pattern);

        // Create a fresh type variable for polymorphic num
        const poly_var = env.types.fresh();

        // Set as num_poly (can be any numeric type)
        _ = cir.setTypeVarAtPat(pattern_idx, Content{
            .structure = .{ .num = .{ .num_poly = .{
                .var_ = poly_var,
                .requirements = .{
                    .sign_needed = false,
                    .bits_needed = 0,
                },
            } } },
        });

        const pattern_var: types.Var = @enumFromInt(@intFromEnum(pattern_idx));
        const resolved = env.types.resolveVar(pattern_var);
        switch (resolved.desc.content) {
            .structure => |s| switch (s) {
                .num => |n| switch (n) {
                    .num_poly => |poly| {
                        try std.testing.expectEqual(false, poly.requirements.sign_needed);
                        try std.testing.expectEqual(@as(u8, 0), poly.requirements.bits_needed);
                        try std.testing.expectEqual(poly_var, poly.var_);
                    },
                    else => return error.ExpectedNumPoly,
                },
                else => {},
            },
            else => {},
        }
    }

    // Test frac_unbound pattern
    {
        var env = base.ModuleEnv.init(gpa);
        defer env.deinit();

        var cir = CIR.init(&env);
        defer cir.deinit();

        const pattern = CIR.Pattern{
            .f64_literal = .{
                .value = 2.5,
                .region = Region.zero(),
            },
        };

        const pattern_idx = cir.store.addPattern(pattern);

        // Set as frac_unbound
        _ = cir.setTypeVarAtPat(pattern_idx, Content{
            .structure = .{ .num = .{ .frac_unbound = .{
                .fits_in_f32 = true,
                .fits_in_dec = true,
            } } },
        });

        const pattern_var: types.Var = @enumFromInt(@intFromEnum(pattern_idx));
        const resolved = env.types.resolveVar(pattern_var);
        switch (resolved.desc.content) {
            .structure => |s| switch (s) {
                .num => |n| switch (n) {
                    .frac_unbound => |req| {
                        try std.testing.expectEqual(true, req.fits_in_f32);
                        try std.testing.expectEqual(true, req.fits_in_dec);
                    },
                    else => return error.ExpectedFracUnbound,
                },
                else => {},
            },
            else => {},
        }
    }

    // Test frac_poly pattern
    {
        var env = base.ModuleEnv.init(gpa);
        defer env.deinit();

        var cir = CIR.init(&env);
        defer cir.deinit();

        const pattern = CIR.Pattern{
            .f64_literal = .{
                .value = std.math.inf(f64),
                .region = Region.zero(),
            },
        };

        const pattern_idx = cir.store.addPattern(pattern);

        // Create a fresh type variable for polymorphic frac
        const poly_var = env.types.fresh();

        // Set as frac_poly (infinity doesn't fit in Dec)
        _ = cir.setTypeVarAtPat(pattern_idx, Content{
            .structure = .{
                .num = .{
                    .frac_poly = .{
                        .var_ = poly_var,
                        .requirements = .{
                            .fits_in_f32 = true,
                            .fits_in_dec = false, // Infinity doesn't fit in Dec
                        },
                    },
                },
            },
        });

        const pattern_var: types.Var = @enumFromInt(@intFromEnum(pattern_idx));
        const resolved = env.types.resolveVar(pattern_var);
        switch (resolved.desc.content) {
            .structure => |s| switch (s) {
                .num => |n| switch (n) {
                    .frac_poly => |poly| {
                        try std.testing.expectEqual(true, poly.requirements.fits_in_f32);
                        try std.testing.expectEqual(false, poly.requirements.fits_in_dec);
                        try std.testing.expectEqual(poly_var, poly.var_);
                    },
                    else => return error.ExpectedFracPoly,
                },
                else => {},
            },
            else => {},
        }
    }
}

test "pattern numeric literal value edge cases" {
    var gpa_state = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
    defer std.debug.assert(gpa_state.deinit() == .ok);
    const gpa = gpa_state.allocator();

    // Test max/min integer values
    {
        var env = base.ModuleEnv.init(gpa);
        defer env.deinit();

        var cir = CIR.init(&env);
        defer cir.deinit();

        // Test i128 max
        const max_pattern = CIR.Pattern{
            .int_literal = .{
                .value = .{ .bytes = @bitCast(@as(i128, std.math.maxInt(i128))), .kind = .i128 },
                .region = Region.zero(),
            },
        };
        const max_idx = cir.store.addPattern(max_pattern);
        const stored_max = cir.store.getPattern(max_idx);
        try std.testing.expectEqual(std.math.maxInt(i128), @as(i128, @bitCast(stored_max.int_literal.value.bytes)));

        // Test i128 min
        const min_pattern = CIR.Pattern{
            .int_literal = .{
                .value = .{ .bytes = @bitCast(@as(i128, std.math.minInt(i128))), .kind = .i128 },
                .region = Region.zero(),
            },
        };
        const min_idx = cir.store.addPattern(min_pattern);
        const stored_min = cir.store.getPattern(min_idx);
        try std.testing.expectEqual(std.math.minInt(i128), @as(i128, @bitCast(stored_min.int_literal.value.bytes)));
    }

    // Test small decimal pattern
    {
        var env = base.ModuleEnv.init(gpa);
        defer env.deinit();

        var cir = CIR.init(&env);
        defer cir.deinit();

        const small_dec_pattern = CIR.Pattern{
            .small_dec_literal = .{
                .numerator = 1234,
                .denominator_power_of_ten = 2, // 12.34
                .region = Region.zero(),
            },
        };

        const pattern_idx = cir.store.addPattern(small_dec_pattern);
        const stored = cir.store.getPattern(pattern_idx);

        try std.testing.expect(stored == .small_dec_literal);
        try std.testing.expectEqual(@as(i16, 1234), stored.small_dec_literal.numerator);
        try std.testing.expectEqual(@as(u8, 2), stored.small_dec_literal.denominator_power_of_ten);
    }

    // Test dec literal pattern
    {
        var env = base.ModuleEnv.init(gpa);
        defer env.deinit();

        var cir = CIR.init(&env);
        defer cir.deinit();

        const dec_pattern = CIR.Pattern{
            .dec_literal = .{
                .value = RocDec{ .num = 314159265358979323 }, // π * 10^17
                .region = Region.zero(),
            },
        };

        const pattern_idx = cir.store.addPattern(dec_pattern);
        const stored = cir.store.getPattern(pattern_idx);

        try std.testing.expect(stored == .dec_literal);
        try std.testing.expectEqual(@as(i128, 314159265358979323), stored.dec_literal.value.num);
    }

    // Test special float values
    {
        var env = base.ModuleEnv.init(gpa);
        defer env.deinit();

        var cir = CIR.init(&env);
        defer cir.deinit();

        // Test NaN
        const nan_pattern = CIR.Pattern{
            .f64_literal = .{
                .value = std.math.nan(f64),
                .region = Region.zero(),
            },
        };
        const nan_idx = cir.store.addPattern(nan_pattern);
        const stored_nan = cir.store.getPattern(nan_idx);
        try std.testing.expect(std.math.isNan(stored_nan.f64_literal.value));

        // Test negative zero
        const neg_zero_pattern = CIR.Pattern{
            .f64_literal = .{
                .value = -0.0,
                .region = Region.zero(),
            },
        };
        const neg_zero_idx = cir.store.addPattern(neg_zero_pattern);
        const stored_neg_zero = cir.store.getPattern(neg_zero_idx);
        try std.testing.expect(std.math.signbit(stored_neg_zero.f64_literal.value));
        try std.testing.expectEqual(@as(f64, -0.0), stored_neg_zero.f64_literal.value);
    }
}

test "pattern literal type transitions" {
    var gpa_state = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
    defer std.debug.assert(gpa_state.deinit() == .ok);
    const gpa = gpa_state.allocator();

    // Test transitioning from unbound to concrete type
    {
        var env = base.ModuleEnv.init(gpa);
        defer env.deinit();

        var cir = CIR.init(&env);
        defer cir.deinit();

        const pattern = CIR.Pattern{
            .int_literal = .{
                .value = .{ .bytes = @bitCast(@as(i128, 100)), .kind = .i128 },
                .region = Region.zero(),
            },
        };

        const pattern_idx = cir.store.addPattern(pattern);

        // Start as num_unbound
        _ = cir.setTypeVarAtPat(pattern_idx, Content{
            .structure = .{ .num = .{ .num_unbound = .{
                .sign_needed = false,
                .bits_needed = 1,
            } } },
        });

        // Simulate type inference determining it's a U8
        const pattern_var: types.Var = @enumFromInt(@intFromEnum(pattern_idx));
        _ = env.types.setVarContent(pattern_var, Content{
            .structure = .{ .num = types.Num.int_u8 },
        });

        // Verify it resolved to U8
        const resolved = env.types.resolveVar(pattern_var);
        switch (resolved.desc.content) {
            .structure => |s| switch (s) {
                .num => |n| switch (n) {
                    .num_compact => |compact| switch (compact) {
                        .int => |int| {
                            try std.testing.expect(int == .u8);
                        },
                        else => return error.ExpectedInt,
                    },
                    else => return error.ExpectedNumCompact,
                },
                else => {},
            },
            else => {},
        }
    }

    // Test hex/binary/octal patterns must be integers
    {
        var env = base.ModuleEnv.init(gpa);
        defer env.deinit();

        var cir = CIR.init(&env);
        defer cir.deinit();

        // Hex pattern (0xFF)
        const hex_pattern = CIR.Pattern{
            .int_literal = .{
                .value = .{ .bytes = @bitCast(@as(i128, 0xFF)), .kind = .i128 },
                .region = Region.zero(),
            },
        };

        const hex_idx = cir.store.addPattern(hex_pattern);

        // Non-decimal literals use int_unbound (not num_unbound)
        _ = cir.setTypeVarAtPat(hex_idx, Content{
            .structure = .{ .num = .{ .int_unbound = .{
                .sign_needed = false,
                .bits_needed = 1,
            } } },
        });

        const hex_var: types.Var = @enumFromInt(@intFromEnum(hex_idx));
        const resolved = env.types.resolveVar(hex_var);
        switch (resolved.desc.content) {
            .structure => |s| switch (s) {
                .num => |n| switch (n) {
                    .int_unbound => |req| {
                        // Verify it's constrained to integers only
                        try std.testing.expectEqual(false, req.sign_needed);
                        try std.testing.expectEqual(@as(u8, 1), req.bits_needed);
                    },
                    else => return error.ExpectedIntUnbound,
                },
                else => {},
            },
            else => {},
        }
    }
}

test "pattern type inference with numeric literals" {
    var gpa_state = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
    defer std.debug.assert(gpa_state.deinit() == .ok);
    const gpa = gpa_state.allocator();

    // Test that pattern indices work correctly as type variables with type inference
    {
        var env = base.ModuleEnv.init(gpa);
        defer env.deinit();

        var cir = CIR.init(&env);
        defer cir.deinit();

        // Create patterns representing different numeric literals
        const patterns = [_]struct {
            pattern: CIR.Pattern,
            expected_type: types.Content,
        }{
            // Small positive int - could be any unsigned type
            .{
                .pattern = CIR.Pattern{
                    .int_literal = .{
                        .value = .{ .bytes = @bitCast(@as(i128, 42)), .kind = .i128 },
                        .region = Region.zero(),
                    },
                },
                .expected_type = Content{ .structure = .{ .num = .{ .num_unbound = .{
                    .sign_needed = false,
                    .bits_needed = 0,
                } } } },
            },
            // Negative int - needs signed type
            .{
                .pattern = CIR.Pattern{
                    .int_literal = .{
                        .value = .{ .bytes = @bitCast(@as(i128, -42)), .kind = .i128 },
                        .region = Region.zero(),
                    },
                },
                .expected_type = Content{ .structure = .{ .num = .{ .num_unbound = .{
                    .sign_needed = true,
                    .bits_needed = 0,
                } } } },
            },
            // Large int requiring more bits
            .{
                .pattern = CIR.Pattern{
                    .int_literal = .{
                        .value = .{ .bytes = @bitCast(@as(i128, 65536)), .kind = .i128 },
                        .region = Region.zero(),
                    },
                },
                .expected_type = Content{
                    .structure = .{
                        .num = .{
                            .num_unbound = .{
                                .sign_needed = false,
                                .bits_needed = 4, // Needs at least 17 bits
                            },
                        },
                    },
                },
            },
        };

        for (patterns) |test_case| {
            const pattern_idx = cir.store.addPattern(test_case.pattern);
            _ = cir.setTypeVarAtPat(pattern_idx, test_case.expected_type);

            // Verify the pattern index works as a type variable
            const pattern_var: types.Var = @enumFromInt(@intFromEnum(pattern_idx));
            const resolved = env.types.resolveVar(pattern_var);

            // Compare the resolved type with expected
            try std.testing.expectEqual(test_case.expected_type, resolved.desc.content);
        }
    }

    // Test patterns with type constraints from context
    {
        var env = base.ModuleEnv.init(gpa);
        defer env.deinit();

        var cir = CIR.init(&env);
        defer cir.deinit();

        // Create a pattern that will be constrained by context
        const pattern = CIR.Pattern{
            .int_literal = .{
                .value = .{ .bytes = @bitCast(@as(i128, 100)), .kind = .i128 },
                .region = Region.zero(),
            },
        };

        const pattern_idx = cir.store.addPattern(pattern);

        // Start as num_unbound
        _ = cir.setTypeVarAtPat(pattern_idx, Content{
            .structure = .{ .num = .{ .num_unbound = .{
                .sign_needed = false,
                .bits_needed = 1,
            } } },
        });

        // Simulate type inference constraining it to U8
        const pattern_var: types.Var = @enumFromInt(@intFromEnum(pattern_idx));
        _ = env.types.setVarContent(pattern_var, Content{
            .structure = .{ .num = types.Num.int_u8 },
        });

        // Verify it was constrained correctly
        const resolved = env.types.resolveVar(pattern_var);
        switch (resolved.desc.content) {
            .structure => |s| switch (s) {
                .num => |n| switch (n) {
                    .num_compact => |compact| {
                        try std.testing.expect(compact.int == .u8);
                    },
                    else => return error.ExpectedConcreteType,
                },
                else => {},
            },
            else => {},
        }
    }
}
