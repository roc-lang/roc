#![allow(clippy::too_many_arguments)]

use arrayvec::ArrayVec;
use roc_can::constraint::{Constraint, Constraints, ExpectedTypeIndex};
use roc_can::expected::Expected::{self, *};
use roc_can::num::{FloatBound, FloatWidth, IntBound, IntLitWidth, NumBound, SignDemand};
use roc_module::symbol::Symbol;
use roc_region::all::{Loc, Region};
use roc_types::num::{NumericRange, SingleQuoteBound};
use roc_types::subs::Variable;
use roc_types::types::Type::{self, *};
use roc_types::types::{AliasKind, Category, Types};
use roc_types::types::{OptAbleType, Reason};

#[inline(always)]
pub(crate) fn add_numeric_bound_constr(
    types: &mut Types,
    constraints: &mut Constraints,
    num_constraints: &mut impl Extend<Constraint>,
    num_var: Variable,
    precision_var: Variable,
    bound: impl TypedNumericBound,
    region: Region,
    category: Category,
) -> Type {
    let range = bound.numeric_bound();

    use roc_types::num::{float_width_to_variable, int_lit_width_to_variable};

    match range {
        NumericBound::None => {
            // no additional constraints, just a Num *
            num_num(Variable(num_var))
        }
        NumericBound::FloatExact(width) => {
            let actual_type = constraints.push_variable(float_width_to_variable(width));
            let expected = Expected::ForReason(Reason::NumericLiteralSuffix, actual_type, region);
            let type_index = constraints.push_variable(num_var);
            let expected_index = constraints.push_expected_type(expected);
            let because_suffix =
                constraints.equal_types(type_index, expected_index, category, region);

            num_constraints.extend([because_suffix]);

            Variable(num_var)
        }
        NumericBound::IntExact(width) => {
            let actual_type = constraints.push_variable(int_lit_width_to_variable(width));
            let expected = Expected::ForReason(Reason::NumericLiteralSuffix, actual_type, region);
            let type_index = constraints.push_variable(num_var);
            let expected_index = constraints.push_expected_type(expected);
            let because_suffix =
                constraints.equal_types(type_index, expected_index, category, region);

            num_constraints.extend([because_suffix]);

            Variable(num_var)
        }
        NumericBound::Range(range) => {
            let precision_type = constraints.push_variable(precision_var);
            let expected = {
                let typ = types.from_old_type(&RangedNumber(range));
                Expected::NoExpectation(constraints.push_type(types, typ))
            };
            let expected_index = constraints.push_expected_type(expected);
            let constr = constraints.equal_types(precision_type, expected_index, category, region);

            num_constraints.extend([constr]);

            num_num(Variable(num_var))
        }
    }
}

#[inline(always)]
pub(crate) fn int_literal(
    types: &mut Types,
    constraints: &mut Constraints,
    num_var: Variable,
    precision_var: Variable,
    expected: ExpectedTypeIndex,
    region: Region,
    bound: IntBound,
) -> Constraint {
    let reason = Reason::IntLiteral;

    // Always add the bound first; this improves the resolved type quality in case it's an alias like "U8".
    let mut constrs = ArrayVec::<_, 3>::new();
    let num_type = add_numeric_bound_constr(
        types,
        constraints,
        &mut constrs,
        num_var,
        precision_var,
        bound,
        region,
        Category::Num,
    );

    let num_type_index = {
        let typ = types.from_old_type(&num_type);
        constraints.push_type(types, typ)
    };
    let int_precision_type = {
        let typ = types.from_old_type(&num_int(Type::Variable(precision_var)));
        constraints.push_type(types, typ)
    };

    let expect_precision_var =
        constraints.push_expected_type(ForReason(reason, int_precision_type, region));

    constrs.extend([
        constraints.equal_types(num_type_index, expect_precision_var, Category::Int, region),
        constraints.equal_types(num_type_index, expected, Category::Int, region),
    ]);

    // TODO the precision_var is not part of the exists here; for float it is. Which is correct?
    let and_constraint = constraints.and_constraint(constrs);
    constraints.exists([num_var], and_constraint)
}

pub(crate) fn single_quote_literal(
    types: &mut Types,
    constraints: &mut Constraints,
    num_var: Variable,
    precision_var: Variable,
    expected: ExpectedTypeIndex,
    region: Region,
    bound: SingleQuoteBound,
) -> Constraint {
    let reason = Reason::IntLiteral;

    // Always add the bound first; this improves the resolved type quality in case it's an alias like "U8".
    let mut constrs = ArrayVec::<_, 3>::new();
    let num_type = add_numeric_bound_constr(
        types,
        constraints,
        &mut constrs,
        num_var,
        precision_var,
        bound,
        region,
        Category::Character,
    );

    let num_type_index = {
        let typ = types.from_old_type(&num_type);
        constraints.push_type(types, typ)
    };
    let int_precision_type = {
        let typ = types.from_old_type(&num_int(Type::Variable(precision_var)));
        constraints.push_type(types, typ)
    };

    let expect_precision_var =
        constraints.push_expected_type(ForReason(reason, int_precision_type, region));

    constrs.extend([
        constraints.equal_types(
            num_type_index,
            expect_precision_var,
            Category::Character,
            region,
        ),
        constraints.equal_types(num_type_index, expected, Category::Character, region),
    ]);

    let and_constraint = constraints.and_constraint(constrs);
    constraints.exists([num_var], and_constraint)
}

#[inline(always)]
pub(crate) fn float_literal(
    types: &mut Types,
    constraints: &mut Constraints,
    num_var: Variable,
    precision_var: Variable,
    expected: ExpectedTypeIndex,
    region: Region,
    bound: FloatBound,
) -> Constraint {
    let reason = Reason::FloatLiteral;

    let mut constrs = ArrayVec::<_, 3>::new();
    let num_type = add_numeric_bound_constr(
        types,
        constraints,
        &mut constrs,
        num_var,
        precision_var,
        bound,
        region,
        Category::Frac,
    );

    let num_type_index = {
        let typ = types.from_old_type(&num_type);
        constraints.push_type(types, typ)
    };
    let float_precision_type = {
        let typ = types.from_old_type(&num_float(Type::Variable(precision_var)));
        constraints.push_type(types, typ)
    };

    let expect_precision_var =
        constraints.push_expected_type(ForReason(reason, float_precision_type, region));

    constrs.extend([
        constraints.equal_types(num_type_index, expect_precision_var, Category::Frac, region),
        constraints.equal_types(num_type_index, expected, Category::Frac, region),
    ]);

    let and_constraint = constraints.and_constraint(constrs);
    constraints.exists([num_var, precision_var], and_constraint)
}

#[inline(always)]
pub(crate) fn num_literal(
    types: &mut Types,
    constraints: &mut Constraints,
    num_var: Variable,
    expected: ExpectedTypeIndex,
    region: Region,
    bound: NumBound,
) -> Constraint {
    let mut constrs = ArrayVec::<_, 2>::new();
    let num_type = add_numeric_bound_constr(
        types,
        constraints,
        &mut constrs,
        num_var,
        num_var,
        bound,
        region,
        Category::Num,
    );

    let type_index = {
        let typ = types.from_old_type(&num_type);
        constraints.push_type(types, typ)
    };
    constrs.extend([constraints.equal_types(type_index, expected, Category::Num, region)]);

    let and_constraint = constraints.and_constraint(constrs);
    constraints.exists([num_var], and_constraint)
}

// Try not to be too clever about inlining, at least in debug builds.
// Inlining these tiny leaf functions can lead to death by a thousand cuts,
// where we end up with huge stack frames in non-tail-recursive functions.
#[cfg_attr(not(debug_assertions), inline(always))]
pub(crate) fn builtin_type(symbol: Symbol, args: Vec<Type>) -> Type {
    Type::Apply(
        symbol,
        args.into_iter().map(Loc::at_zero).collect(),
        Region::zero(),
    )
}

#[cfg_attr(not(debug_assertions), inline(always))]
pub(crate) fn empty_list_type(var: Variable) -> Type {
    list_type(Type::Variable(var))
}

#[cfg_attr(not(debug_assertions), inline(always))]
pub(crate) fn list_type(typ: Type) -> Type {
    builtin_type(Symbol::LIST_LIST, vec![typ])
}

#[cfg_attr(not(debug_assertions), inline(always))]
fn builtin_num_alias(
    symbol: Symbol,
    type_arguments: Vec<OptAbleType>,
    actual: Box<Type>,
    kind: AliasKind,
) -> Type {
    Type::Alias {
        symbol,
        type_arguments,
        actual,
        lambda_set_variables: vec![],
        infer_ext_in_output_types: vec![],
        kind,
    }
}

#[cfg_attr(not(debug_assertions), inline(always))]
pub(crate) fn num_float(range: Type) -> Type {
    builtin_num_alias(
        Symbol::NUM_FRAC,
        vec![OptAbleType::unbound(range.clone())],
        Box::new(num_num(num_floatingpoint(range))),
        AliasKind::Structural,
    )
}

#[cfg_attr(not(debug_assertions), inline(always))]
pub(crate) fn num_floatingpoint(range: Type) -> Type {
    builtin_num_alias(
        Symbol::NUM_FLOATINGPOINT,
        vec![OptAbleType::unbound(range.clone())],
        Box::new(range),
        AliasKind::Opaque,
    )
}

#[cfg_attr(not(debug_assertions), inline(always))]
pub(crate) fn num_int(range: Type) -> Type {
    builtin_num_alias(
        Symbol::NUM_INT,
        vec![OptAbleType::unbound(range.clone())],
        Box::new(num_num(num_integer(range))),
        AliasKind::Structural,
    )
}

#[cfg_attr(not(debug_assertions), inline(always))]
pub(crate) fn num_integer(range: Type) -> Type {
    builtin_num_alias(
        Symbol::NUM_INTEGER,
        vec![OptAbleType::unbound(range.clone())],
        Box::new(range),
        AliasKind::Opaque,
    )
}

#[cfg_attr(not(debug_assertions), inline(always))]
pub(crate) fn num_num(typ: Type) -> Type {
    builtin_num_alias(
        Symbol::NUM_NUM,
        vec![OptAbleType::unbound(typ.clone())],
        Box::new(typ),
        AliasKind::Opaque,
    )
}

pub trait TypedNumericBound {
    fn numeric_bound(&self) -> NumericBound;
}

impl TypedNumericBound for IntBound {
    fn numeric_bound(&self) -> NumericBound {
        match self {
            IntBound::None => NumericBound::None,
            IntBound::Exact(w) => NumericBound::IntExact(*w),
            IntBound::AtLeast {
                sign: SignDemand::NoDemand,
                width,
            } => NumericBound::Range(NumericRange::IntAtLeastEitherSign(*width)),
            IntBound::AtLeast {
                sign: SignDemand::Signed,
                width,
            } => NumericBound::Range(NumericRange::IntAtLeastSigned(*width)),
        }
    }
}

impl TypedNumericBound for FloatBound {
    fn numeric_bound(&self) -> NumericBound {
        match self {
            FloatBound::None => NumericBound::None,
            FloatBound::Exact(w) => NumericBound::FloatExact(*w),
        }
    }
}

impl TypedNumericBound for NumBound {
    fn numeric_bound(&self) -> NumericBound {
        match self {
            NumBound::None => NumericBound::None,
            &NumBound::AtLeastIntOrFloat {
                sign: SignDemand::NoDemand,
                width,
            } => NumericBound::Range(NumericRange::NumAtLeastEitherSign(width)),
            &NumBound::AtLeastIntOrFloat {
                sign: SignDemand::Signed,
                width,
            } => NumericBound::Range(NumericRange::NumAtLeastSigned(width)),
        }
    }
}

impl TypedNumericBound for SingleQuoteBound {
    fn numeric_bound(&self) -> NumericBound {
        match self {
            &SingleQuoteBound::AtLeast { width } => {
                NumericBound::Range(NumericRange::IntAtLeastEitherSign(width))
            }
        }
    }
}

/// A bound placed on a number because of its literal value.
/// e.g. `-5` cannot be unsigned, and 300 does not fit in a U8
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NumericBound {
    None,
    FloatExact(FloatWidth),
    IntExact(IntLitWidth),
    Range(NumericRange),
}
