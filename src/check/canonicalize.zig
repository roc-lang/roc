const std = @import("std");
const base = @import("../base.zig");
const parse = @import("parse.zig");
const problem = @import("../problem.zig");
const collections = @import("../collections.zig");

const Scope = @import("./canonicalize/Scope.zig");
const Alias = @import("./canonicalize/Alias.zig");

const Ident = base.Ident;
const Region = base.Region;
const TagName = base.TagName;
const Problem = problem.Problem;
const exitOnOom = collections.utils.exitOnOom;

const Self = @This();
pub const IR = @import("./canonicalize/IR.zig");

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
pub fn canonicalize(
    can_ir: *IR,
    parse_ir: *parse.IR,
    allocator: std.mem.Allocator,
) void {
    var env = can_ir.env;
    const builtin_aliases = &.{};
    const imported_idents = &.{};
    var scope = Scope.init(&env, builtin_aliases, imported_idents, allocator);

    const file = parse_ir.store.getFile(parse.IR.NodeStore.FileIdx{ .id = 0 });

    for (file.statements) |stmt_id| {
        const stmt = parse_ir.store.getStatement(stmt_id);
        switch (stmt) {
            .import => |import| {
                bringImportIntoScope(&import, parse_ir, can_ir, &scope);
            },
            else => std.debug.panic("Unhandled statement type: {}", .{stmt}),
        }
    }

    @panic("not implemented");
}

fn bringImportIntoScope(
    import: *const parse.IR.NodeStore.Statement.Import,
    parse_ir: *parse.IR,
    ir: *IR,
    scope: *Scope,
) void {
    _ = import;
    _ = parse_ir;
    _ = scope;

    const import_name: []u8 = &.{}; // import.module_name_tok;
    const shorthand: []u8 = &.{}; // import.qualifier_tok;
    const region = Region{
        .start = Region.Position.zero(),
        .end = Region.Position.zero(),
    };

    const res = ir.env.modules.getOrInsert(import_name, shorthand);

    if (res.was_present) {
        ir.env.problems.append(Problem.Canonicalize.make(.{ .DuplicateImport = .{
            .duplicate_import_region = region,
        } })) catch exitOnOom();
    }

    // for (import.exposing.items.items) |exposed| {
    //     switch (exposed) {
    //         .Value => |ident| {
    //             ir.env.addExposedIdentForModule(ident, res.module_idx);
    //             _ = scope.levels.introduce(.ident, .{ .scope_name = ident, .ident = ident });
    //         },
    //         .Type => |imported_type| {
    //             const alias = Alias{
    //                 .name = imported_type.name,
    //                 .region = ir.env.tag_names.getRegion(imported_type.name),
    //                 .is_builtin = false,
    //                 .kind = .ImportedUnknown,
    //             };
    //             const alias_idx = ir.aliases.append(alias);

    //             _ = scope.levels.introduce(.alias, .{
    //                 .scope_name = imported_type.name,
    //                 .alias = alias_idx,
    //             });
    //         },
    //         .CustomTagUnion => |custom| {
    //             const alias = Alias{
    //                 .name = custom.name,
    //                 .region = ir.env.tag_names.getRegion(custom.name),
    //                 .is_builtin = false,
    //                 .kind = .ImportedCustomUnion,
    //             };
    //             const alias_idx = ir.aliases.append(alias);

    //             _ = scope.levels.introduce(.alias, .{
    //                 .scope_name = custom.name,
    //                 .alias = alias_idx,
    //             });
    //             // TODO: add to scope.custom_tags
    //         },
    //     }
    // }
}

fn bringIngestedFileIntoScope(
    import: *const parse.IR.Stmt.Import,
    env: *base.ModuleEnv,
    scope: *Scope,
) void {
    const res = env.modules.getOrInsert(
        import.name,
        import.package_shorthand,
    );

    if (res.was_present) {
        _ = env.problems.append(Problem.Canonicalize.make(.DuplicateImport{
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
        env.addExposedIdentForModule(exposed_ident, res.module_idx);
        scope.introduce(exposed);
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

// pub(crate) fn canonicalize_defs<'a>(
//     env: &mut Env<'a>,
//     mut output: Output,
//     var_store: &mut VarStore,
//     scope: &mut Scope,
//     loc_defs: &'a mut roc_parse::ast::Defs<'a>,
//     pattern_type: PatternType,
// ) -> (
//     CanDefs,
//     Output,
//     MutMap<Symbol, Region>,
//     Vec<IntroducedImport>,
// ) {
//     // Canonicalizing defs while detecting shadowing involves a multi-step process:
//     //
//     // 1. Go through each of the patterns.
//     // 2. For each identifier pattern, get the scope.symbol() for the ident. (That symbol will use the home module for its module.)
//     // 3. If that symbol is already in scope, then we're about to shadow it. Error!
//     // 4. Otherwise, add it to the scope immediately, so we can detect shadowing within the same
//     //    pattern (e.g. (Foo a a) = ...)
//     // 5. Add this canonicalized pattern and its corresponding ast::Expr to pending_exprs.
//     // 5. Once every pattern has been processed and added to scope, go back and canonicalize the exprs from
//     //    pending_exprs, this time building up a canonical def for each one.
//     //
//     // This way, whenever any expr is doing lookups, it knows everything that's in scope -
//     // even defs that appear after it in the source.
//     //
//     // This naturally handles recursion too, because a given expr which refers
//     // to itself won't be processed until after its def has been added to scope.

//     let mut pending_type_defs = Vec::with_capacity(loc_defs.type_defs.len());
//     let mut pending_value_defs = Vec::with_capacity(loc_defs.value_defs.len());
//     let mut pending_abilities_in_scope = PendingAbilitiesInScope::default();

//     // Convert the type defs into pending defs first, then all the value defs.
//     // Follow this order because we need all value symbols to fully canonicalize type defs (in case
//     // there are opaques that implement an ability using a value symbol). But, value symbols might
//     // shadow symbols defined in a local ability def.

//     for either_index in loc_defs.tags.iter() {
//         if let Ok(type_index) = either_index.split() {
//             let type_def = &loc_defs.type_defs[type_index.index()];
//             let pending_type_def = to_pending_type_def(env, type_def, scope, pattern_type);
//             if let PendingTypeDef::Ability { name, members } = &pending_type_def {
//                 pending_abilities_in_scope.insert(
//                     name.value,
//                     members.iter().map(|mem| mem.name.value).collect(),
//                 );
//             }
//             pending_type_defs.push(pending_type_def);
//         }
//     }

//     for (index, either_index) in loc_defs.tags.iter().enumerate() {
//         if let Err(value_index) = either_index.split() {
//             let value_def = &loc_defs.value_defs[value_index.index()];
//             let region = loc_defs.regions[index];

//             let pending = to_pending_value_def(
//                 env,
//                 var_store,
//                 value_def,
//                 region,
//                 scope,
//                 &pending_abilities_in_scope,
//                 &mut output,
//                 pattern_type,
//             );

//             pending_value_defs.push(Loc::at(region, pending));
//         }
//     }

//     let CanonicalizedTypeDefs {
//         aliases,
//         symbols_introduced,
//         derived_defs,
//     } = canonicalize_type_defs(
//         env,
//         &mut output,
//         var_store,
//         scope,
//         &pending_abilities_in_scope,
//         pending_type_defs,
//     );

//     // Add the derived ASTs, so that we create proper canonicalized defs for them.
//     // They can go at the end, and derived defs should never reference anything other than builtin
//     // ability members.
//     pending_value_defs.extend(derived_defs);

//     // Now that we have the scope completely assembled, and shadowing resolved,
//     // we're ready to canonicalize any body exprs.
//     canonicalize_value_defs(
//         env,
//         output,
//         var_store,
//         scope,
//         pending_value_defs,
//         pattern_type,
//         aliases,
//         symbols_introduced,
//     )
// }

// #[allow(clippy::too_many_arguments)]
// fn canonicalize_value_defs<'a>(
//     env: &mut Env<'a>,
//     mut output: Output,
//     var_store: &mut VarStore,
//     scope: &mut Scope,
//     value_defs: Vec<Loc<PendingValue<'a>>>,
//     pattern_type: PatternType,
//     mut aliases: VecMap<Symbol, Alias>,
//     mut symbols_introduced: MutMap<Symbol, Region>,
// ) -> (
//     CanDefs,
//     Output,
//     MutMap<Symbol, Region>,
//     Vec<IntroducedImport>,
// ) {
//     // Canonicalize all the patterns, record shadowing problems, and store
//     // the ast::Expr values in pending_exprs for further canonicalization
//     // once we've finished assembling the entire scope.
//     let mut pending_value_defs = Vec::with_capacity(value_defs.len());
//     let mut pending_dbgs = Vec::with_capacity(value_defs.len());
//     let mut pending_expects = Vec::with_capacity(value_defs.len());

//     let mut imports_introduced = Vec::with_capacity(value_defs.len());

//     for loc_pending_def in value_defs {
//         match loc_pending_def.value {
//             PendingValue::Def(pending_def) => {
//                 // Record the ast::Expr for later. We'll do another pass through these
//                 // once we have the entire scope assembled. If we were to canonicalize
//                 // the exprs right now, they wouldn't have symbols in scope from defs
//                 // that get would have gotten added later in the defs list!
//                 pending_value_defs.push(pending_def);
//             }
//             PendingValue::SignatureDefMismatch => { /* skip */ }
//             PendingValue::Dbg(pending_dbg) => {
//                 pending_dbgs.push(pending_dbg);
//             }
//             PendingValue::Expect(pending_expect) => {
//                 pending_expects.push(pending_expect);
//             }
//             PendingValue::ModuleImport(PendingModuleImport {
//                 module_id,
//                 region,
//                 exposed_symbols,
//                 params,
//             }) => {
//                 imports_introduced.push(IntroducedImport {
//                     module_id,
//                     region,
//                     exposed_symbols,
//                 });

//                 pending_value_defs.push(PendingValueDef::ImportParams {
//                     symbol: params.symbol,
//                     variable: params.variable,
//                     loc_pattern: params.loc_pattern,
//                     opt_provided: params.opt_provided,
//                     module_id,
//                 });
//             }
//             PendingValue::InvalidIngestedFile => { /* skip */ }
//             PendingValue::ImportNameConflict => { /* skip */ }
//             PendingValue::StmtAfterExpr => { /* skip */ }
//         }
//     }

//     let mut symbol_to_index: Vec<(IdentId, u32)> = Vec::with_capacity(pending_value_defs.len());

//     for (def_index, pending_def) in pending_value_defs.iter().enumerate() {
//         let Some(loc_pattern) = pending_def.loc_pattern() else {
//             continue;
//         };

//         let new_bindings = BindingsFromPattern::new(loc_pattern).peekable();

//         for (s, r) in new_bindings {
//             // store the top-level defs, used to ensure that closures won't capture them
//             if let PatternType::TopLevelDef = pattern_type {
//                 env.top_level_symbols.insert(s);
//             }

//             symbols_introduced.insert(s, r);

//             debug_assert_eq!(env.home, s.module_id());
//             debug_assert!(
//                 !symbol_to_index.iter().any(|(id, _)| *id == s.ident_id()),
//                 "{s:?}"
//             );

//             symbol_to_index.push((s.ident_id(), def_index as u32));
//         }
//     }

//     let capacity = pending_value_defs.len();
//     let mut defs = Vec::with_capacity(capacity);
//     let mut def_ordering = DefOrdering::from_symbol_to_id(env.home, symbol_to_index, capacity);

//     for (def_id, pending_def) in pending_value_defs.into_iter().enumerate() {
//         let temp_output = canonicalize_pending_value_def(
//             env,
//             pending_def,
//             output,
//             scope,
//             var_store,
//             pattern_type,
//             &mut aliases,
//         );

//         output = temp_output.output;

//         if let (PatternType::TopLevelDef, DefKind::Ignored(_)) =
//             (pattern_type, temp_output.def.kind)
//         {
//             env.problems.push(Problem::NoIdentifiersIntroduced(
//                 temp_output.def.loc_pattern.region,
//             ))
//         }

//         defs.push(Some(temp_output.def));

//         def_ordering.insert_symbol_references(def_id as u32, &temp_output.references)
//     }

//     let mut dbgs = ExpectsOrDbgs::with_capacity(pending_dbgs.len());
//     let mut expects = ExpectsOrDbgs::with_capacity(pending_expects.len());

//     for pending in pending_dbgs {
//         let (loc_can_condition, can_output) = canonicalize_expr(
//             env,
//             var_store,
//             scope,
//             pending.condition.region,
//             &pending.condition.value,
//         );

//         dbgs.push(loc_can_condition, pending.preceding_comment);

//         output.union(can_output);
//     }

//     for pending in pending_expects {
//         let (loc_can_condition, can_output) = canonicalize_expr(
//             env,
//             var_store,
//             scope,
//             pending.condition.region,
//             &pending.condition.value,
//         );

//         expects.push(loc_can_condition, pending.preceding_comment);

//         output.union(can_output);
//     }

//     let can_defs = CanDefs {
//         defs,
//         dbgs,
//         expects,
//         def_ordering,
//         aliases,
//     };

//     (can_defs, output, symbols_introduced, imports_introduced)
// }
