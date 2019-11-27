extern crate bumpalo;

use self::bumpalo::Bump;
use roc::can;
use roc::can::expr::Expr;
use roc::can::problem::Problem;
use roc::can::symbol::Symbol;
use roc::can::Output;
use roc::collections::ImMap;
use roc::ident::Ident;
use roc::parse;
use roc::parse::ast::{self, Attempting};
use roc::parse::blankspace::space0_before;
use roc::parse::parser::{loc, Fail, Parser, State};
use roc::region::{Located, Region};
use roc::subs::{Subs, Variable};
use roc::types::{Expected, Type};

#[allow(dead_code)]
pub fn parse_with<'a>(arena: &'a Bump, input: &'a str) -> Result<ast::Expr<'a>, Fail> {
    parse_loc_with(arena, input).map(|loc_expr| loc_expr.value)
}

#[allow(dead_code)]
pub fn parse_loc_with<'a>(arena: &'a Bump, input: &'a str) -> Result<Located<ast::Expr<'a>>, Fail> {
    let state = State::new(&input, Attempting::Module);
    let parser = space0_before(loc(parse::expr(0)), 0);
    let answer = parser.parse(&arena, state);

    answer
        .map(|(loc_expr, _)| loc_expr)
        .map_err(|(fail, _)| fail)
}

#[allow(dead_code)]
pub fn can_expr(expr_str: &str) -> (Expr, Output, Vec<Problem>, Subs, Variable) {
    can_expr_with(
        &Bump::new(),
        "blah",
        expr_str,
        &ImMap::default(),
        &ImMap::default(),
    )
}

#[allow(dead_code)]
pub fn can_expr_with(
    arena: &Bump,
    name: &str,
    expr_str: &str,
    declared_idents: &ImMap<Ident, (Symbol, Region)>,
    declared_variants: &ImMap<Symbol, Located<Box<str>>>,
) -> (Expr, Output, Vec<Problem>, Subs, Variable) {
    let loc_expr = parse_loc_with(&arena, expr_str).unwrap_or_else(|_| {
        panic!(
            "can_expr_with() got a parse error when attempting to canonicalize:\n\n{:?}",
            expr_str
        )
    });

    let mut subs = Subs::new();
    let variable = subs.mk_flex_var();
    let expected = Expected::NoExpectation(Type::Variable(variable));
    let home = "Test";
    let (loc_expr, output, problems) = can::canonicalize_declaration(
        arena,
        &mut subs,
        home.into(),
        name.into(),
        Region::zero(),
        loc_expr,
        declared_idents,
        declared_variants,
        expected,
    );

    (loc_expr.value, output, problems, subs, variable)
}

// pub fn mut_map_from_pairs<K, V, I>(pairs: I) -> MutMap<K, V>
// where
//     I: IntoIterator<Item = (K, V)>,
//     K: Hash + Eq,
// {
//     let mut answer = MutMap::default();

//     for (key, value) in pairs {
//         answer.insert(key, value);
//     }

//     answer
// }
