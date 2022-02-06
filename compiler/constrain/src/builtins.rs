use roc_can::constraint::Constraint::{self, *};
use roc_can::constraint::LetConstraint;
use roc_can::expected::Expected::{self, *};
use roc_can::num::{FloatBound, FloatWidth, IntBound, IntWidth, NumericBound, SignDemand};
use roc_collections::all::SendMap;
use roc_module::ident::{Lowercase, TagName};
use roc_module::symbol::Symbol;
use roc_region::all::Region;
use roc_types::subs::Variable;
use roc_types::types::Category;
use roc_types::types::Reason;
use roc_types::types::Type::{self, *};

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

// macro_rules! num_types {
//     // Represent
//     //   num_u8 ~ U8 : Num Integer Unsigned8 = @Num (@Integer (@Unsigned8))
//     //   int_u8 ~ Integer Unsigned8 = @Integer (@Unsigned8)
//     //
//     //   num_f32 ~ F32 : Num FloaingPoint Binary32 = @Num (@FloaingPoint (@Binary32))
//     //   float_f32 ~ FloatingPoint Binary32 = @FloatingPoint (@Binary32)
//     // and so on, for all numeric types.
//     ($($num_fn:ident, $sub_fn:ident, $num_type:ident, $alias:path, $inner_alias:path, $inner_private_tag:path)*) => {
//         $(
//             #[inline(always)]
//             fn $sub_fn() -> Type {
//                 builtin_alias(
//                     $inner_alias,
//                     vec![],
//                     Box::new(Type::TagUnion(
//                         vec![(TagName::Private($inner_private_tag), vec![])],
//                         Box::new(Type::EmptyTagUnion)
//                     )),
//                 )
//             }
//
//             #[inline(always)]
//             fn $num_fn() -> Type {
//                 builtin_alias(
//                     $alias,
//                     vec![],
//                     Box::new($num_type($sub_fn()))
//                 )
//             }
//         )*
//     }
// }
//
// num_types! {
//     num_u8,   int_u8,    num_int,   Symbol::NUM_U8,   Symbol::NUM_UNSIGNED8,   Symbol::NUM_AT_UNSIGNED8
//     num_u16,  int_u16,   num_int,   Symbol::NUM_U16,  Symbol::NUM_UNSIGNED16,  Symbol::NUM_AT_UNSIGNED16
//     num_u32,  int_u32,   num_int,   Symbol::NUM_U32,  Symbol::NUM_UNSIGNED32,  Symbol::NUM_AT_UNSIGNED32
//     num_u64,  int_u64,   num_int,   Symbol::NUM_U64,  Symbol::NUM_UNSIGNED64,  Symbol::NUM_AT_UNSIGNED64
//     num_u128, int_u128,  num_int,   Symbol::NUM_U128, Symbol::NUM_UNSIGNED128, Symbol::NUM_AT_UNSIGNED128
//     num_i8,   int_i8,    num_int,   Symbol::NUM_I8,   Symbol::NUM_SIGNED8,     Symbol::NUM_AT_SIGNED8
//     num_i16,  int_i16,   num_int,   Symbol::NUM_I16,  Symbol::NUM_SIGNED16,    Symbol::NUM_AT_SIGNED16
//     num_i32,  int_i32,   num_int,   Symbol::NUM_I32,  Symbol::NUM_SIGNED32,    Symbol::NUM_AT_SIGNED32
//     num_i64,  int_i64,   num_int,   Symbol::NUM_I64,  Symbol::NUM_SIGNED64,    Symbol::NUM_AT_SIGNED64
//     num_i128, int_i128,  num_int,   Symbol::NUM_I128, Symbol::NUM_SIGNED128,   Symbol::NUM_AT_SIGNED128
//     num_nat,  int_nat,   num_int,   Symbol::NUM_NAT,  Symbol::NUM_NATURAL,     Symbol::NUM_AT_NATURAL
//     num_dec,  float_dec, num_float, Symbol::NUM_DEC,  Symbol::NUM_DECIMAL,     Symbol::NUM_AT_DECIMAL
//     num_f32,  float_f32, num_float, Symbol::NUM_F32,  Symbol::NUM_BINARY32,    Symbol::NUM_AT_BINARY32
//     num_f64,  float_f64, num_float, Symbol::NUM_F64,  Symbol::NUM_BINARY64,    Symbol::NUM_AT_BINARY64
// }

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
            NumericBound::Int(ib) => ib.bounded_range(),
            NumericBound::Float(fb) => fb.bounded_range(),
        }
    }
}
