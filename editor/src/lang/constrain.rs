use crate::lang::pool::{NodeId, Pool, PoolVec};
use crate::lang::{ast::Expr2, expr::Env, types::Type2};

use roc_can::expected::Expected;
use roc_collections::all::SendMap;
use roc_module::symbol::Symbol;
use roc_region::all::{Located, Region};
use roc_types::{
    subs::Variable,
    types::{Category, Reason},
};

#[derive(Debug)]
pub enum Constraint {
    Eq(Type2, Expected<Type2>, Category, Region),
    // Store(Type, Variable, &'static str, u32),
    // Lookup(Symbol, Expected<Type>, Region),
    // Pattern(Region, PatternCategory, Type, PExpected<Type>),
    And(PoolVec<Constraint>),
    Let(NodeId<LetConstraint>),
    // SaveTheEnvironment,
    True, // Used for things that always unify, e.g. blanks and runtime errors
}

#[derive(Debug)]
pub struct LetConstraint {
    pub rigid_vars: PoolVec<Variable>,
    pub flex_vars: PoolVec<Variable>,
    pub def_types: SendMap<Symbol, Located<Type2>>,
    pub defs_constraint: Constraint,
    pub ret_constraint: Constraint,
}

pub fn constrain_expr(
    env: &mut Env,
    expr: &Expr2,
    expected: Expected<Type2>,
    region: Region,
) -> Constraint {
    use Constraint::*;

    match expr {
        Expr2::Str(_) => Eq(str_type(env.pool), expected, Category::Str, region),
        Expr2::EmptyRecord => Eq(Type2::EmptyRec, expected, Category::Record, region),
        Expr2::SmallInt { number, var, .. } => {
            let flex_vars = PoolVec::with_capacity(1, env.pool);
            let rigid_vars = PoolVec::empty(env.pool);

            for flex_var_node_id in flex_vars.iter_node_ids() {
                env.pool[flex_var_node_id] = *var;
            }

            let num_type = Type2::Variable(*var);

            let and_nodes = vec![
                Eq(
                    num_type,
                    Expected::ForReason(
                        Reason::IntLiteral,
                        Type2::Alias(
                            Symbol::NUM_INT,
                            PoolVec::empty(env.pool),
                            env.pool.add(Type2::EmptyRec),
                        ),
                        region,
                    ),
                    Category::Int,
                    region,
                ),
                Eq(Type2::Variable(*var), expected, Category::Int, region),
            ];

            let and_constraints = PoolVec::new(and_nodes.into_iter(), env.pool);

            let defs_constraint = And(and_constraints);

            let node_id = env.pool.add(LetConstraint {
                rigid_vars,
                flex_vars,
                def_types: SendMap::default(),
                defs_constraint,
                ret_constraint: Constraint::True,
            });

            Let(node_id)
        }
        _ => todo!("implement constaints for {:?}", expr),
    }
}

fn str_type(pool: &mut Pool) -> Type2 {
    Type2::Apply(Symbol::STR_STR, PoolVec::empty(pool))
}
