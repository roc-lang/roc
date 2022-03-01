use roc_can::constraint::Constraint::{self, *};
use roc_can::constraint::LetConstraint;
use roc_can::expected::Expected::{self, *};
use roc_can::num::{FloatBound, FloatWidth, IntBound, IntWidth, NumericBound, SignDemand};
use roc_collections::all::SendMap;
use roc_module::ident::{Lowercase, TagName};
use roc_module::symbol::Symbol;
use roc_region::all::Region;
use roc_types::subs::Variable;
use roc_types::types::Reason;
use roc_types::types::Type::{self, *};
use roc_types::types::{AliasKind, Category};

#[must_use]
pub fn add_numeric_bound_constr(
    constrs: &mut Vec<Constraint>,
    num_type: Type,
    bound: impl TypedNumericBound,
    region: Region,
    category: Category,
) -> Type {
    let range = bound.bounded_range();

    let total_num_type = num_type;

    match range.len() {
        0 => total_num_type,
        1 => {
            let actual_type = Variable(range[0]);
            constrs.push(Eq(
                total_num_type.clone(),
                Expected::ForReason(Reason::NumericLiteralSuffix, actual_type, region),
                category,
                region,
            ));
            total_num_type
        }
        _ => RangedNumber(Box::new(total_num_type), range),
    }
}

#[inline(always)]
pub fn int_literal(
    num_var: Variable,
    precision_var: Variable,
    expected: Expected<Type>,
    region: Region,
    bound: IntBound,
) -> Constraint {
    let reason = Reason::IntLiteral;

    let mut constrs = Vec::with_capacity(3);
    // Always add the bound first; this improves the resolved type quality in case it's an alias
    // like "U8".
    let num_type = add_numeric_bound_constr(
        &mut constrs,
        Variable(num_var),
        bound,
        region,
        Category::Num,
    );
    constrs.extend(vec![
        Eq(
            num_type.clone(),
            ForReason(reason, num_int(Type::Variable(precision_var)), region),
            Category::Int,
            region,
        ),
        Eq(num_type, expected, Category::Int, region),
    ]);

    exists(vec![num_var], And(constrs))
}

#[inline(always)]
pub fn float_literal(
    num_var: Variable,
    precision_var: Variable,
    expected: Expected<Type>,
    region: Region,
    bound: FloatBound,
) -> Constraint {
    let reason = Reason::FloatLiteral;

    let mut constrs = Vec::with_capacity(3);
    let num_type = add_numeric_bound_constr(
        &mut constrs,
        Variable(num_var),
        bound,
        region,
        Category::Float,
    );
    constrs.extend(vec![
        Eq(
            num_type.clone(),
            ForReason(reason, num_float(Type::Variable(precision_var)), region),
            Category::Float,
            region,
        ),
        Eq(num_type, expected, Category::Float, region),
    ]);

    exists(vec![num_var, precision_var], And(constrs))
}

#[inline(always)]
pub fn num_literal(
    num_var: Variable,
    expected: Expected<Type>,
    region: Region,
    bound: NumericBound,
) -> Constraint {
    let open_number_type = crate::builtins::num_num(Type::Variable(num_var));

    let mut constrs = Vec::with_capacity(3);
    let num_type =
        add_numeric_bound_constr(&mut constrs, open_number_type, bound, region, Category::Num);
    constrs.extend(vec![Eq(num_type, expected, Category::Num, region)]);

    exists(vec![num_var], And(constrs))
}

#[inline(always)]
pub fn exists(flex_vars: Vec<Variable>, constraint: Constraint) -> Constraint {
    Let(Box::new(LetConstraint {
        rigid_vars: Vec::new(),
        flex_vars,
        def_types: SendMap::default(),
        defs_constraint: constraint,
        ret_constraint: Constraint::True,
    }))
}

#[inline(always)]
pub fn builtin_type(symbol: Symbol, args: Vec<Type>) -> Type {
    Type::Apply(symbol, args, Region::zero())
}

#[inline(always)]
pub fn empty_list_type(var: Variable) -> Type {
    list_type(Type::Variable(var))
}

#[inline(always)]
pub fn list_type(typ: Type) -> Type {
    builtin_type(Symbol::LIST_LIST, vec![typ])
}

#[inline(always)]
pub fn str_type() -> Type {
    builtin_type(Symbol::STR_STR, Vec::new())
}

#[inline(always)]
fn builtin_alias(
    symbol: Symbol,
    type_arguments: Vec<(Lowercase, Type)>,
    actual: Box<Type>,
) -> Type {
    Type::Alias {
        symbol,
        type_arguments,
        actual,
        lambda_set_variables: vec![],
        // TODO(opaques): revisit later
        kind: AliasKind::Structural,
    }
}

#[inline(always)]
pub fn num_float(range: Type) -> Type {
    builtin_alias(
        Symbol::NUM_FLOAT,
        vec![("range".into(), range.clone())],
        Box::new(num_num(num_floatingpoint(range))),
    )
}

#[inline(always)]
pub fn num_floatingpoint(range: Type) -> Type {
    let alias_content = Type::TagUnion(
        vec![(
            TagName::Private(Symbol::NUM_AT_FLOATINGPOINT),
            vec![range.clone()],
        )],
        Box::new(Type::EmptyTagUnion),
    );

    builtin_alias(
        Symbol::NUM_FLOATINGPOINT,
        vec![("range".into(), range)],
        Box::new(alias_content),
    )
}

#[inline(always)]
pub fn num_u32() -> Type {
    builtin_alias(Symbol::NUM_U32, vec![], Box::new(num_int(num_unsigned32())))
}

#[inline(always)]
fn num_unsigned32() -> Type {
    let alias_content = Type::TagUnion(
        vec![(TagName::Private(Symbol::NUM_AT_UNSIGNED32), vec![])],
        Box::new(Type::EmptyTagUnion),
    );

    builtin_alias(Symbol::NUM_UNSIGNED32, vec![], Box::new(alias_content))
}

#[inline(always)]
pub fn num_binary64() -> Type {
    let alias_content = Type::TagUnion(
        vec![(TagName::Private(Symbol::NUM_AT_BINARY64), vec![])],
        Box::new(Type::EmptyTagUnion),
    );

    builtin_alias(Symbol::NUM_BINARY64, vec![], Box::new(alias_content))
}

#[inline(always)]
pub fn num_int(range: Type) -> Type {
    builtin_alias(
        Symbol::NUM_INT,
        vec![("range".into(), range.clone())],
        Box::new(num_num(num_integer(range))),
    )
}

#[inline(always)]
pub fn num_signed64() -> Type {
    let alias_content = Type::TagUnion(
        vec![(TagName::Private(Symbol::NUM_AT_SIGNED64), vec![])],
        Box::new(Type::EmptyTagUnion),
    );

    builtin_alias(Symbol::NUM_SIGNED64, vec![], Box::new(alias_content))
}

#[inline(always)]
pub fn num_integer(range: Type) -> Type {
    let alias_content = Type::TagUnion(
        vec![(
            TagName::Private(Symbol::NUM_AT_INTEGER),
            vec![range.clone()],
        )],
        Box::new(Type::EmptyTagUnion),
    );

    builtin_alias(
        Symbol::NUM_INTEGER,
        vec![("range".into(), range)],
        Box::new(alias_content),
    )
}

#[inline(always)]
pub fn num_num(typ: Type) -> Type {
    let alias_content = Type::TagUnion(
        vec![(TagName::Private(Symbol::NUM_AT_NUM), vec![typ.clone()])],
        Box::new(Type::EmptyTagUnion),
    );

    builtin_alias(
        Symbol::NUM_NUM,
        vec![("range".into(), typ)],
        Box::new(alias_content),
    )
}

pub trait TypedNumericBound {
    fn bounded_range(&self) -> Vec<Variable>;
}

impl TypedNumericBound for IntBound {
    fn bounded_range(&self) -> Vec<Variable> {
        match self {
            IntBound::None => vec![],
            IntBound::Exact(w) => vec![match w {
                IntWidth::U8 => Variable::U8,
                IntWidth::U16 => Variable::U16,
                IntWidth::U32 => Variable::U32,
                IntWidth::U64 => Variable::U64,
                IntWidth::U128 => Variable::U128,
                IntWidth::I8 => Variable::I8,
                IntWidth::I16 => Variable::I16,
                IntWidth::I32 => Variable::I32,
                IntWidth::I64 => Variable::I64,
                IntWidth::I128 => Variable::I128,
                IntWidth::Nat => Variable::NAT,
            }],
            IntBound::AtLeast { sign, width } => {
                let whole_range: &[(IntWidth, Variable)] = match sign {
                    SignDemand::NoDemand => {
                        &[
                            (IntWidth::I8, Variable::I8),
                            (IntWidth::U8, Variable::U8),
                            (IntWidth::I16, Variable::I16),
                            (IntWidth::U16, Variable::U16),
                            (IntWidth::I32, Variable::I32),
                            (IntWidth::U32, Variable::U32),
                            (IntWidth::I64, Variable::I64),
                            (IntWidth::Nat, Variable::NAT), // FIXME: Nat's order here depends on the platform!
                            (IntWidth::U64, Variable::U64),
                            (IntWidth::I128, Variable::I128),
                            (IntWidth::U128, Variable::U128),
                        ]
                    }
                    SignDemand::Signed => &[
                        (IntWidth::I8, Variable::I8),
                        (IntWidth::I16, Variable::I16),
                        (IntWidth::I32, Variable::I32),
                        (IntWidth::I64, Variable::I64),
                        (IntWidth::I128, Variable::I128),
                    ],
                };
                whole_range
                    .iter()
                    .skip_while(|(lower_bound, _)| *lower_bound != *width)
                    .map(|(_, var)| *var)
                    .collect()
            }
        }
    }
}

impl TypedNumericBound for FloatBound {
    fn bounded_range(&self) -> Vec<Variable> {
        match self {
            FloatBound::None => vec![],
            FloatBound::Exact(w) => vec![match w {
                FloatWidth::Dec => Variable::DEC,
                FloatWidth::F32 => Variable::F32,
                FloatWidth::F64 => Variable::F64,
            }],
        }
    }
}

impl TypedNumericBound for NumericBound {
    fn bounded_range(&self) -> Vec<Variable> {
        match self {
            NumericBound::None => vec![],
            &NumericBound::AtLeastIntOrFloat { sign, width } => {
                let mut range = IntBound::AtLeast { sign, width }.bounded_range();
                range.extend_from_slice(&[Variable::F32, Variable::F64, Variable::DEC]);
                range
            }
        }
    }
}
