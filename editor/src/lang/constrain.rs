use bumpalo::{collections::Vec as BumpVec, Bump};

use crate::lang::pool::{Pool, PoolStr, PoolVec};
use crate::lang::{
    ast::Expr2,
    expr::Env,
    types::{Type2, TypeId},
};

use roc_can::expected::Expected;
use roc_collections::all::SendMap;
use roc_module::ident::TagName;
use roc_module::symbol::Symbol;
use roc_region::all::{Located, Region};
use roc_types::{
    subs::Variable,
    types::{Category, Reason},
};

#[derive(Debug)]
pub enum Constraint<'a> {
    Eq(Type2, Expected<Type2>, Category, Region),
    // Store(Type, Variable, &'static str, u32),
    // Lookup(Symbol, Expected<Type>, Region),
    // Pattern(Region, PatternCategory, Type, PExpected<Type>),
    And(BumpVec<'a, Constraint<'a>>),
    Let(&'a LetConstraint<'a>),
    // SaveTheEnvironment,
    True, // Used for things that always unify, e.g. blanks and runtime errors
}

#[derive(Debug)]
pub struct LetConstraint<'a> {
    pub rigid_vars: BumpVec<'a, Variable>,
    pub flex_vars: BumpVec<'a, Variable>,
    pub def_types: SendMap<Symbol, Located<Type2>>,
    pub defs_constraint: Constraint<'a>,
    pub ret_constraint: Constraint<'a>,
}

pub fn constrain_expr<'a>(
    arena: &'a Bump,
    env: &mut Env,
    expr: &Expr2,
    expected: Expected<Type2>,
    region: Region,
) -> Constraint<'a> {
    use Constraint::*;

    match expr {
        Expr2::Str(_) => Eq(str_type(env.pool), expected, Category::Str, region),
        Expr2::EmptyRecord => Eq(Type2::EmptyRec, expected, Category::Record, region),
        Expr2::SmallInt { var, .. } => {
            let mut flex_vars = BumpVec::with_capacity_in(1, arena);
            let rigid_vars = BumpVec::new_in(arena);

            let mut and_constraints = BumpVec::with_capacity_in(2, arena);

            flex_vars.push(*var);

            let range_type_id = env.pool.add(Type2::Variable(*var));

            and_constraints.push(Eq(
                Type2::Variable(*var),
                Expected::ForReason(Reason::IntLiteral, num_int(env.pool, range_type_id), region),
                Category::Int,
                region,
            ));

            and_constraints.push(Eq(Type2::Variable(*var), expected, Category::Int, region));

            let defs_constraint = And(and_constraints);

            let let_constraint = arena.alloc(LetConstraint {
                rigid_vars,
                flex_vars,
                def_types: SendMap::default(),
                defs_constraint,
                ret_constraint: Constraint::True,
            });

            Let(let_constraint)
        }
        _ => todo!("implement constaints for {:?}", expr),
    }
}

fn str_type(pool: &mut Pool) -> Type2 {
    Type2::Apply(Symbol::STR_STR, PoolVec::empty(pool))
}

fn num_int(pool: &mut Pool, range: TypeId) -> Type2 {
    let num_integer_type = num_integer(pool, range);
    let num_integer_id = pool.add(num_integer_type);

    let num_num_type = num_num(pool, num_integer_id);
    let num_num_id = pool.add(num_num_type);

    Type2::Alias(
        Symbol::NUM_INT,
        PoolVec::new(vec![(PoolStr::new("range", pool), range)].into_iter(), pool),
        num_num_id,
    )
}

fn num_signed64(pool: &mut Pool) -> Type2 {
    let alias_content = Type2::TagUnion(
        PoolVec::new(
            // TagName::Private(Symbol::NUM_AT_SIGNED64)
            vec![(PoolStr::new("Num.@Signed64", pool), PoolVec::empty(pool))].into_iter(),
            pool,
        ),
        pool.add(Type2::EmptyTagUnion),
    );

    Type2::Alias(
        Symbol::NUM_SIGNED64,
        PoolVec::empty(pool),
        pool.add(alias_content),
    )
}

fn num_integer(pool: &mut Pool, range: TypeId) -> Type2 {
    let range_type = pool.get(range);

    let alias_content = Type2::TagUnion(
        PoolVec::new(
            vec![(
                // TagName::Private(Symbol::NUM_AT_INTEGER)
                PoolStr::new("Num.@Integer", pool),
                PoolVec::new(vec![*range_type].into_iter(), pool),
            )]
            .into_iter(),
            pool,
        ),
        pool.add(Type2::EmptyTagUnion),
    );

    Type2::Alias(
        Symbol::NUM_INTEGER,
        PoolVec::new(vec![(PoolStr::new("range", pool), range)].into_iter(), pool),
        pool.add(alias_content),
    )
}

fn num_num(pool: &mut Pool, type_id: TypeId) -> Type2 {
    let range_type = pool.get(type_id);

    let alias_content = Type2::TagUnion(
        PoolVec::new(
            vec![(
                // TagName::Private(Symbol::NUM_AT_NUM)
                PoolStr::new("Num.@Num", pool),
                PoolVec::new(vec![*range_type].into_iter(), pool),
            )]
            .into_iter(),
            pool,
        ),
        pool.add(Type2::EmptyTagUnion),
    );

    Type2::Alias(
        Symbol::NUM_NUM,
        PoolVec::new(
            vec![(PoolStr::new("range", pool), type_id)].into_iter(),
            pool,
        ),
        pool.add(alias_content),
    )
}
