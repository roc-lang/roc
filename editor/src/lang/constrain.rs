use crate::lang::pool::{Pool, PoolVec};
use crate::lang::{ast::Expr2, expr::Env, types::Type2};

use roc_can::expected::Expected;
use roc_module::symbol::Symbol;
use roc_region::all::Region;
use roc_types::types::Category;

#[derive(Debug)]
pub enum Constraint {
    Eq(Type2, Expected<Type2>, Category, Region),
    // Store(Type, Variable, &'static str, u32),
    // Lookup(Symbol, Expected<Type>, Region),
    // Pattern(Region, PatternCategory, Type, PExpected<Type>),
    True, // Used for things that always unify, e.g. blanks and runtime errors
          // SaveTheEnvironment,
          // Let(Box<LetConstraint>),
          // And(Vec<Constraint>),
}

pub fn constrain_expr(env: &mut Env, expr: &Expr2, expected: Expected<Type2>) -> Constraint {
    use Constraint::*;

    match expr {
        Expr2::Str(_) => Eq(str_type(env.pool), expected, Category::Str, Region::zero()),
        _ => todo!("implement constaints for {:?}", expr),
    }
}

fn str_type(pool: &mut Pool) -> Type2 {
    Type2::Apply(Symbol::STR_STR, PoolVec::empty(pool))
}
