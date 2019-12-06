use crate::can::def::{canonicalize_defs, sort_can_defs, Def, Info};
use crate::can::env::Env;
use crate::can::expr::Output;
use crate::can::operator::desugar_def;
use crate::can::scope::Scope;
use crate::collections::ImMap;
use crate::parse::ast::{self, ExposesEntry};
use crate::region::Located;
use crate::subs::VarStore;
use crate::types::Constraint::{self, *};
use bumpalo::Bump;

#[derive(Clone, Debug, PartialEq)]
pub struct Module {
    pub name: Option<Box<str>>,
    pub defs: Vec<Def>,
    pub constraint: Constraint,
}

pub fn canonicalize_module_defs<'a, I>(
    arena: &Bump,
    loc_defs: bumpalo::collections::Vec<'a, Located<ast::Def<'a>>>,
    home: Box<str>,
    _exposes: I,
    scope: &mut Scope,
    var_store: &VarStore,
) -> (Vec<Def>, Constraint)
where
    I: Iterator<Item = Located<ExposesEntry<'a>>>,
{
    // Desugar operators (convert them to Apply calls, taking into account
    // operator precedence and associativity rules), before doing other canonicalization.
    //
    // If we did this *during* canonicalization, then each time we
    // visited a BinOp node we'd recursively try to apply this to each of its nested
    // operators, and then again on *their* nested operators, ultimately applying the
    // rules multiple times unnecessarily.
    let mut desugared =
        bumpalo::collections::Vec::with_capacity_in(loc_defs.len() + scope.idents.len(), arena);

    // Exposed values are desugared as defs that appear before any others, e.g.
    //
    // imports [ Foo.{ bar, baz } ]
    //
    // ...desugars to these extra defs at the start of the module:
    //
    // bar = Foo.bar
    // baz = Foo.baz
    //
    // This is the part where we add those defs to the beginning of the module.
    for (ident, (symbol, region)) in scope.idents.iter() {
        if ident.first_char().is_lowercase() {
            let pattern = ast::Pattern::Identifier(arena.alloc(ident.clone().name()));
            let expr = ast::Expr::RawVar(arena.alloc(symbol.clone().into_boxed_str()));
            let loc_pattern = Located {
                value: pattern,
                region: region.clone(),
            };
            let loc_expr = Located {
                value: expr,
                region: region.clone(),
            };
            let value = ast::Def::Body(arena.alloc(loc_pattern), arena.alloc(loc_expr));

            desugared.push(&*arena.alloc(Located {
                value,
                region: region.clone(),
            }));
        } else {
            // TODO add type aliases to type alias dictionary, based on exposed types
        }
    }

    for loc_def in loc_defs {
        desugared.push(&*arena.alloc(Located {
            value: desugar_def(arena, arena.alloc(loc_def.value)),
            region: loc_def.region,
        }));
    }

    let mut env = Env::new(home);
    let rigids = ImMap::default();
    let mut flex_info = Info::default();
    let defs = canonicalize_defs(
        &rigids,
        &mut env,
        var_store,
        scope,
        &desugared,
        &mut flex_info,
    );

    let (defs, _) = sort_can_defs(&mut env, defs, Output::default());

    let defs = defs.expect("TODO error canonicalizing module defs");

    // TODO examine the patterns, extract toplevel identifiers from them,
    // and verify that everything in the `exposes` list is actually present in
    // that set of identifiers. You can't expose it if it wasn't defined!

    // TODO incorporate rigids into here (possibly by making this be a Let instead
    // of an And)

    (defs, And(flex_info.constraints))
}
