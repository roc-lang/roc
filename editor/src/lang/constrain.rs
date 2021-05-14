use bumpalo::{collections::Vec as BumpVec, Bump};

use crate::lang::{
    ast::{Expr2, RecordField},
    expr::Env,
    pool::{Pool, PoolStr, PoolVec, ShallowClone},
    types::{Type2, TypeId},
};

use roc_can::expected::Expected;
use roc_collections::all::{BumpMap, BumpMapDefault, Index};
use roc_module::{ident::TagName, symbol::Symbol};
use roc_region::all::{Located, Region};
use roc_types::{
    subs::Variable,
    types,
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
    pub def_types: BumpMap<Symbol, Located<Type2>>,
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
        Expr2::SmallStr(_) => Eq(str_type(env.pool), expected, Category::Str, region),
        Expr2::Blank => True,
        Expr2::EmptyRecord => constrain_empty_record(expected, region),
        Expr2::SmallInt { var, .. } => {
            let mut flex_vars = BumpVec::with_capacity_in(1, arena);

            flex_vars.push(*var);

            let precision_var = env.var_store.fresh();

            let range_type = Type2::Variable(precision_var);

            let range_type_id = env.pool.add(range_type);

            exists(
                arena,
                flex_vars,
                Eq(
                    num_num(env.pool, range_type_id),
                    expected,
                    Category::Num,
                    region,
                ),
            )
        }
        Expr2::Float { var, .. } => {
            let mut flex_vars = BumpVec::with_capacity_in(1, arena);

            let mut and_constraints = BumpVec::with_capacity_in(2, arena);

            let num_type = Type2::Variable(*var);

            flex_vars.push(*var);

            let precision_var = env.var_store.fresh();

            let range_type = Type2::Variable(precision_var);

            let range_type_id = env.pool.add(range_type);

            and_constraints.push(Eq(
                num_type.shallow_clone(),
                Expected::ForReason(
                    Reason::FloatLiteral,
                    num_float(env.pool, range_type_id),
                    region,
                ),
                Category::Int,
                region,
            ));

            and_constraints.push(Eq(num_type, expected, Category::Float, region));

            let defs_constraint = And(and_constraints);

            exists(arena, flex_vars, defs_constraint)
        }
        Expr2::List {
            elem_var, elems, ..
        } => {
            let mut flex_vars = BumpVec::with_capacity_in(1, arena);

            flex_vars.push(*elem_var);

            if elems.is_empty() {
                exists(
                    arena,
                    flex_vars,
                    Eq(
                        empty_list_type(env.pool, *elem_var),
                        expected,
                        Category::List,
                        region,
                    ),
                )
            } else {
                let mut constraints = BumpVec::with_capacity_in(1 + elems.len(), arena);

                let list_elem_type = Type2::Variable(*elem_var);

                for (index, elem_node_id) in elems.iter_node_ids().enumerate() {
                    let elem_expr = env.pool.get(elem_node_id);

                    let elem_expected = Expected::ForReason(
                        Reason::ElemInList {
                            index: Index::zero_based(index),
                        },
                        list_elem_type.shallow_clone(),
                        region,
                    );

                    let constraint = constrain_expr(arena, env, elem_expr, elem_expected, region);

                    constraints.push(constraint);
                }

                constraints.push(Eq(
                    list_type(env.pool, list_elem_type),
                    expected,
                    Category::List,
                    region,
                ));

                exists(arena, flex_vars, And(constraints))
            }
        }
        Expr2::Record { fields, record_var } => {
            if fields.is_empty() {
                constrain_empty_record(expected, region)
            } else {
                let field_types = PoolVec::with_capacity(fields.len() as u32, env.pool);

                let mut field_vars = BumpVec::with_capacity_in(fields.len(), arena);

                // Constraints need capacity for each field
                // + 1 for the record itself + 1 for record var
                let mut constraints = BumpVec::with_capacity_in(2 + fields.len(), arena);

                for (record_field_node_id, field_type_node_id) in
                    fields.iter_node_ids().zip(field_types.iter_node_ids())
                {
                    let record_field = env.pool.get(record_field_node_id);

                    match record_field {
                        RecordField::LabeledValue(pool_str, var, node_id) => {
                            let expr = env.pool.get(*node_id);

                            let (field_type, field_con) = constrain_field(arena, env, *var, expr);

                            field_vars.push(*var);

                            let field_type_id = env.pool.add(field_type);

                            env.pool[field_type_node_id] =
                                (*pool_str, types::RecordField::Required(field_type_id));

                            constraints.push(field_con);
                        }
                        e => todo!("{:?}", e),
                    }
                }

                let record_type = Type2::Record(field_types, env.pool.add(Type2::EmptyRec));

                let record_con = Eq(
                    record_type,
                    expected.shallow_clone(),
                    Category::Record,
                    region,
                );

                constraints.push(record_con);

                // variable to store in the AST
                let stored_con = Eq(
                    Type2::Variable(*record_var),
                    expected,
                    Category::Storage(std::file!(), std::line!()),
                    region,
                );

                field_vars.push(*record_var);
                constraints.push(stored_con);

                exists(arena, field_vars, And(constraints))
            }
        }
        Expr2::GlobalTag {
            variant_var,
            ext_var,
            name,
            arguments,
        } => {
            let mut flex_vars = BumpVec::with_capacity_in(arguments.len(), arena);
            let types = PoolVec::with_capacity(arguments.len() as u32, env.pool);
            let mut arg_cons = BumpVec::with_capacity_in(arguments.len(), arena);

            for (argument_node_id, type_node_id) in
                arguments.iter_node_ids().zip(types.iter_node_ids())
            {
                let (var, expr_node_id) = env.pool.get(argument_node_id);

                let argument_expr = env.pool.get(*expr_node_id);

                let arg_con = constrain_expr(
                    arena,
                    env,
                    argument_expr,
                    Expected::NoExpectation(Type2::Variable(*var)),
                    region,
                );

                arg_cons.push(arg_con);
                flex_vars.push(*var);

                env.pool[type_node_id] = Type2::Variable(*var);
            }

            let union_con = Eq(
                Type2::TagUnion(
                    PoolVec::new(std::iter::once((*name, types)), env.pool),
                    env.pool.add(Type2::Variable(*ext_var)),
                ),
                expected.shallow_clone(),
                Category::TagApply {
                    tag_name: TagName::Global(name.as_str(env.pool).into()),
                    args_count: arguments.len(),
                },
                region,
            );

            let ast_con = Eq(
                Type2::Variable(*variant_var),
                expected,
                Category::Storage(std::file!(), std::line!()),
                region,
            );

            flex_vars.push(*variant_var);
            flex_vars.push(*ext_var);

            arg_cons.push(union_con);
            arg_cons.push(ast_con);

            exists(arena, flex_vars, And(arg_cons))
        }
        _ => todo!("implement constaints for {:?}", expr),
    }
}

fn exists<'a>(
    arena: &'a Bump,
    flex_vars: BumpVec<'a, Variable>,
    defs_constraint: Constraint<'a>,
) -> Constraint<'a> {
    Constraint::Let(arena.alloc(LetConstraint {
        rigid_vars: BumpVec::new_in(arena),
        flex_vars,
        def_types: BumpMap::new_in(arena),
        defs_constraint,
        ret_constraint: Constraint::True,
    }))
}

fn constrain_field<'a>(
    arena: &'a Bump,
    env: &mut Env,
    field_var: Variable,
    expr: &Expr2,
) -> (Type2, Constraint<'a>) {
    let field_type = Type2::Variable(field_var);
    let field_expected = Expected::NoExpectation(field_type.shallow_clone());
    let constraint = constrain_expr(arena, env, expr, field_expected, Region::zero());

    (field_type, constraint)
}

fn constrain_empty_record<'a>(expected: Expected<Type2>, region: Region) -> Constraint<'a> {
    Constraint::Eq(Type2::EmptyRec, expected, Category::Record, region)
}

#[inline(always)]
fn builtin_type(symbol: Symbol, args: PoolVec<Type2>) -> Type2 {
    Type2::Apply(symbol, args)
}

#[inline(always)]
fn str_type(pool: &mut Pool) -> Type2 {
    builtin_type(Symbol::STR_STR, PoolVec::empty(pool))
}

#[inline(always)]
fn empty_list_type(pool: &mut Pool, var: Variable) -> Type2 {
    list_type(pool, Type2::Variable(var))
}

#[inline(always)]
fn list_type(pool: &mut Pool, typ: Type2) -> Type2 {
    builtin_type(Symbol::LIST_LIST, PoolVec::new(vec![typ].into_iter(), pool))
}

#[inline(always)]
fn num_float(pool: &mut Pool, range: TypeId) -> Type2 {
    let num_floatingpoint_type = num_floatingpoint(pool, range);
    let num_floatingpoint_id = pool.add(num_floatingpoint_type);

    let num_num_type = num_num(pool, num_floatingpoint_id);
    let num_num_id = pool.add(num_num_type);

    Type2::Alias(
        Symbol::NUM_FLOAT,
        PoolVec::new(vec![(PoolStr::new("range", pool), range)].into_iter(), pool),
        num_num_id,
    )
}

#[inline(always)]
fn num_floatingpoint(pool: &mut Pool, range: TypeId) -> Type2 {
    let range_type = pool.get(range);

    let alias_content = Type2::TagUnion(
        PoolVec::new(
            vec![(
                // TagName::Private(Symbol::NUM_AT_FLOATINGPOINT)
                PoolStr::new("Num.@FloatingPoint", pool),
                PoolVec::new(vec![range_type.shallow_clone()].into_iter(), pool),
            )]
            .into_iter(),
            pool,
        ),
        pool.add(Type2::EmptyTagUnion),
    );

    Type2::Alias(
        Symbol::NUM_FLOATINGPOINT,
        PoolVec::new(vec![(PoolStr::new("range", pool), range)].into_iter(), pool),
        pool.add(alias_content),
    )
}

#[inline(always)]
fn _num_int(pool: &mut Pool, range: TypeId) -> Type2 {
    let num_integer_type = _num_integer(pool, range);
    let num_integer_id = pool.add(num_integer_type);

    let num_num_type = num_num(pool, num_integer_id);
    let num_num_id = pool.add(num_num_type);

    Type2::Alias(
        Symbol::NUM_INT,
        PoolVec::new(vec![(PoolStr::new("range", pool), range)].into_iter(), pool),
        num_num_id,
    )
}

#[inline(always)]
fn _num_signed64(pool: &mut Pool) -> Type2 {
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

#[inline(always)]
fn _num_integer(pool: &mut Pool, range: TypeId) -> Type2 {
    let range_type = pool.get(range);

    let alias_content = Type2::TagUnion(
        PoolVec::new(
            vec![(
                // TagName::Private(Symbol::NUM_AT_INTEGER)
                PoolStr::new("Num.@Integer", pool),
                PoolVec::new(vec![range_type.shallow_clone()].into_iter(), pool),
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

#[inline(always)]
fn num_num(pool: &mut Pool, type_id: TypeId) -> Type2 {
    let range_type = pool.get(type_id);

    let alias_content = Type2::TagUnion(
        PoolVec::new(
            vec![(
                // TagName::Private(Symbol::NUM_AT_NUM)
                PoolStr::new("Num.@Num", pool),
                PoolVec::new(vec![range_type.shallow_clone()].into_iter(), pool),
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
