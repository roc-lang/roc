use crate::can::def::{canonicalize_defs, sort_can_defs, Def, Info};
use crate::can::env::Env;
use crate::can::expr::Output;
use crate::can::operator::desugar_def;
use crate::can::scope::Scope;
use crate::can::symbol::Symbol;
use crate::collections::{ImMap, SendMap};
use crate::parse::ast::{self, ExposesEntry};
use crate::region::Located;
use crate::subs::{VarStore, Variable};
use crate::types::Constraint::{self, *};
use crate::types::Expected::*;
use crate::types::Type;
use bumpalo::Bump;

#[derive(Clone, Debug, PartialEq)]
pub struct Module {
    pub name: Option<Box<str>>,
    pub defs: Vec<Def>,
    pub exposed_imports: SendMap<Symbol, Variable>,
    pub constraint: Constraint,
}

pub fn canonicalize_module_defs<'a, I>(
    arena: &Bump,
    loc_defs: bumpalo::collections::Vec<'a, Located<ast::Def<'a>>>,
    home: Box<str>,
    _exposes: I,
    scope: &mut Scope,
    var_store: &VarStore,
) -> (Vec<Def>, SendMap<Symbol, Variable>, Constraint)
where
    I: Iterator<Item = Located<ExposesEntry<'a>>>,
{
    let mut exposed_imports = SendMap::default();

    // Desugar operators (convert them to Apply calls, taking into account
    // operator precedence and associativity rules), before doing other canonicalization.
    //
    // If we did this *during* canonicalization, then each time we
    // visited a BinOp node we'd recursively try to apply this to each of its nested
    // operators, and then again on *their* nested operators, ultimately applying the
    // rules multiple times unnecessarily.
    let mut desugared =
        bumpalo::collections::Vec::with_capacity_in(loc_defs.len() + scope.idents.len(), arena);

    for loc_def in loc_defs {
        desugared.push(&*arena.alloc(Located {
            value: desugar_def(arena, arena.alloc(loc_def.value)),
            region: loc_def.region,
        }));
    }

    let mut env = Env::new(home);
    let rigids = ImMap::default();
    let mut flex_info = Info::default();

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
    for (ident, (symbol, region)) in scope.idents.iter() {
        if ident.first_char().is_lowercase() {
            let expr_var = var_store.fresh();

            // Add an entry to exposed_imports using the current module's name
            // as the key; e.g. if this is the Foo module and we have
            // exposes [ Bar.{ baz } ] then insert Foo.baz as the key, so when
            // anything references `baz` in this Foo module, it will resolve to Bar.baz.
            exposed_imports.insert(scope.symbol(&*ident.clone().name()), expr_var);

            // Add the usual Lookup constraint as if this were a normal def.
            let expr_type = Type::Variable(expr_var);
            let expected = NoExpectation(expr_type.clone());

            flex_info
                .constraints
                .push(Lookup(symbol.clone(), expected, *region));
        } else {
            // TODO add type aliases to type alias dictionary, based on exposed types
        }
    }

    let defs = canonicalize_defs(
        &rigids,
        &mut env,
        var_store,
        scope,
        &desugared,
        &mut flex_info,
    );

    let defs = match sort_can_defs(&mut env, defs, Output::default()) {
        (Ok(defs), _) => {
            // TODO examine the patterns, extract toplevel identifiers from them,
            // and verify that everything in the `exposes` list is actually present in
            // that set of identifiers. You can't expose it if it wasn't defined!

            defs
        }
        (Err(problem), _) => {
            panic!("TODO problem canonicalizing module defs: {:?}", problem);
        }
    };

    // TODO incorporate rigids into here (possibly by making this be a Let instead
    // of an And)

    (defs, exposed_imports, And(flex_info.constraints))
}
