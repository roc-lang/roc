use crate::can::def::{canonicalize_defs, sort_can_defs, Def, Info};
use crate::can::env::Env;
use crate::can::expr::Output;
use crate::can::operator::desugar_def;
use crate::can::scope::Scope;
use crate::collections::ImMap;
use crate::parse::ast::{self, ExposesEntry};
use crate::region::Located;
use crate::subs::VarStore;
use crate::types::Constraint;
use bumpalo::Bump;

#[derive(Clone, Debug, PartialEq)]
pub struct Module {
    pub name: Option<Box<str>>,
    pub defs: Vec<Def>,
    pub constraint: Constraint,
}

pub fn canonicalize_module_defs<'a, Exposes>(
    arena: &Bump,
    loc_defs: bumpalo::collections::Vec<'a, Located<ast::Def<'a>>>,
    home: Box<str>,
    _exposes: Exposes,
    mut scope: Scope,
    var_store: &VarStore,
) -> (Vec<Def>, Constraint)
where
    Exposes: Iterator<Item = Located<ExposesEntry<'a>>>,
{
    // Desugar operators (convert them to Apply calls, taking into account
    // operator precedence and associativity rules), before doing other canonicalization.
    //
    // If we did this *during* canonicalization, then each time we
    // visited a BinOp node we'd recursively try to apply this to each of its nested
    // operators, and then again on *their* nested operators, ultimately applying the
    // rules multiple times unnecessarily.
    let mut desugared = bumpalo::collections::Vec::with_capacity_in(loc_defs.len(), arena);

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
        &mut scope,
        &desugared,
        &mut flex_info,
    );

    let (defs, _) = sort_can_defs(&mut env, defs, Output::default());

    let defs = defs.expect("TODO error canonicalizing module defs");
    let constraint = Constraint::True; // TODO generate combined constraint from defs

    (defs, constraint)
}
