use crate::can::def::{canonicalize_defs, sort_can_defs, Declaration};
use crate::can::env::Env;
use crate::can::expr::Output;
use crate::can::ident::Ident;
use crate::can::operator::desugar_def;
use crate::can::pattern::PatternType;
use crate::can::problem::RuntimeError;
use crate::can::scope::Scope;
use crate::collections::MutMap;
use crate::module::symbol::{IdentIds, ModuleId, ModuleIds, Symbol};
use crate::parse::ast;
use crate::region::{Located, Region};
use crate::subs::{VarStore, Variable};
use bumpalo::Bump;
use std::sync::Arc;

#[derive(Debug)]
pub struct ModuleOutput {
    pub declarations: Vec<Declaration>,
    pub exposed_imports: MutMap<Symbol, Variable>,
    pub lookups: Vec<(Symbol, Variable, Region)>,
    pub ident_ids: IdentIds,
}

pub fn canonicalize_module_defs<'a>(
    arena: &Bump,
    loc_defs: bumpalo::collections::Vec<'a, Located<ast::Def<'a>>>,
    home: ModuleId,
    module_ids: ModuleIds,
    dep_idents: MutMap<ModuleId, Arc<IdentIds>>,
    exposed_imports: MutMap<Ident, (Symbol, Region)>,
    var_store: &VarStore,
) -> Result<ModuleOutput, RuntimeError> {
    let mut can_exposed_imports = MutMap::default();
    let mut scope = Scope::new(home);
    let num_deps = dep_idents.len();

    // Desugar operators (convert them to Apply calls, taking into account
    // operator precedence and associativity rules), before doing other canonicalization.
    //
    // If we did this *during* canonicalization, then each time we
    // visited a BinOp node we'd recursively try to apply this to each of its nested
    // operators, and then again on *their* nested operators, ultimately applying the
    // rules multiple times unnecessarily.
    let mut desugared =
        bumpalo::collections::Vec::with_capacity_in(loc_defs.len() + num_deps, arena);

    for loc_def in loc_defs {
        desugared.push(&*arena.alloc(Located {
            value: desugar_def(arena, arena.alloc(loc_def.value)),
            region: loc_def.region,
        }));
    }

    let mut env = Env::new(home, dep_idents, module_ids, IdentIds::default());
    let mut lookups = Vec::with_capacity(num_deps);

    // Exposed values are treated like defs that appear before any others, e.g.
    //
    // imports [ Foo.{ bar, baz } ]
    //
    // ...is basically the same as if we'd added these extra defs at the start of the module:
    //
    // bar = Foo.bar
    // baz = Foo.baz
    //
    // Here we essentially add those "defs" to "the beginning of the module"
    // by canonicalizing them right before we canonicalize the actual ast::Def nodes.
    for (ident, (symbol, region)) in exposed_imports {
        let first_char = ident.as_inline_str().chars().next().unwrap();

        if first_char.is_lowercase() {
            let expr_var = var_store.fresh();

            match scope.import(ident, symbol, region) {
                Ok(symbol) => {
                    // Add an entry to exposed_imports using the current module's name
                    // as the key; e.g. if this is the Foo module and we have
                    // exposes [ Bar.{ baz } ] then insert Foo.baz as the key, so when
                    // anything references `baz` in this Foo module, it will resolve to Bar.baz.
                    can_exposed_imports.insert(symbol, expr_var);

                    // This will be used during constraint generation,
                    // to add the usual Lookup constraint as if this were a normal def.
                    lookups.push((symbol, expr_var, region));
                }
                Err((shadowed_symbol, region)) => {
                    panic!("TODO gracefully handle shadowing in imports.")
                }
            }
        } else {
            // TODO add type aliases to type alias dictionary, based on exposed types
        }
    }

    let mut output = Output::default();
    let defs = canonicalize_defs(
        &mut env,
        &mut output.rigids,
        var_store,
        &mut scope,
        &desugared,
        PatternType::TopLevelDef,
    );

    match sort_can_defs(&mut env, defs, Output::default()) {
        (Ok(declarations), _) => {
            // TODO examine the patterns, extract toplevel identifiers from them,
            // and verify that everything in the `exposes` list is actually present in
            // that set of identifiers. You can't expose it if it wasn't defined!

            // TODO incorporate rigids into here (possibly by making this be a Let instead
            // of an And)

            Ok(ModuleOutput {
                declarations,
                exposed_imports: can_exposed_imports,
                lookups,
                ident_ids: env.ident_ids,
            })
        }
        (Err(runtime_error), _) => Err(runtime_error),
    }
}
