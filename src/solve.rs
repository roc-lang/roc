use std::collections::BTreeSet;
use self::Variable::*;
use ena::unify::{UnificationTable, UnifyKey, InPlace};

pub type Name = String;

pub type ModuleName = String;

type UTable = UnificationTable<InPlace<VarId>>;

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Type {
    // Symbol(String),
    // Int,
    // Float,
    // Number,
    // TypeUnion(BTreeSet<Type>),
    // Function(Box<Type>, Box<Type>),
    CallOperator(Operator, Box<Type>, Box<Type>),
}


#[derive(Debug, PartialEq)]
pub enum Expr {
    HexOctalBinary(i64),    // : Int
    FractionalNumber(f64),  // : Float
    WholeNumber(i64),       // : Int | Float

    // Functions
    CallOperator(Operator, Box<Expr>, Box<Expr>),
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Operator {
    Plus, Minus, FloatDivision, IntDivision,
}

#[derive(Debug, PartialEq)]
pub enum Problem {
    Mismatch
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Variable {
    Wildcard,
    RigidVar(Name),
    FlexUnion(BTreeSet<VarId>),
    RigidUnion(BTreeSet<VarId>),
    Structure(FlatType),
    Mismatch
}

type CanonicalModuleName = String;

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum FlatType {
    Function(VarId, VarId),

    // Apply a higher-kinded type constructor by name. For example:
    // "Apply the higher-kinded type constructor `Array` to the variable `Int`
    // to form `Array Int`."
    // ApplyTypeConstructor(CanonicalModuleName, Name, VarId)
    Tuple2(VarId, VarId),
    Tuple3(VarId, VarId, VarId),
    // TupleN(Vec<VarId>), // Last resort - allocates
    // Record1 (Map.Map N.Name VarId) VarId,
}

#[inline]
fn unify_rigid(named: &Variable, other: &Variable) -> Variable {
    match other {
        Wildcard => named.clone(),
        RigidVar(_) => Mismatch,
        FlexUnion(_) => Mismatch,
        RigidUnion(_) => Mismatch,
        Structure(_) => { panic!("TODO"); Mismatch }
        Mismatch => other.clone()
    }
}

#[inline]
fn unify_rigid_union(utable: &mut UTable, rigid_union: &BTreeSet<VarId>, var: &Variable, other: &Variable) -> Variable {
    match other {
        Wildcard => var.clone(),
        RigidVar(_) => Mismatch,
        FlexUnion(flex_union) => {
            if rigid_union_fits_flex_union(utable, &rigid_union, &flex_union) {
                var.clone()
            } else {
                Mismatch
            }
        },
        Structure(_) => { panic!("TODO"); Mismatch }
        RigidUnion(_) => Mismatch,
        Mismatch => other.clone()
    }
}

#[inline]
fn rigid_union_fits_flex_union(utable: &mut UTable, rigid_union: &BTreeSet<VarId>, flex_union: &BTreeSet<VarId>) -> bool {
    if rigid_union.is_subset(&flex_union) {
        // If the keys of the rigid one are a subset of the flex keys, we're done.
        return true;
    }

    let potentially_missing_flex_ids = flex_union.difference(rigid_union);

    // a flex union can conform to a rigid one, as long 
    // as the rigid union contains all the flex union's alternative types
    let rigid_union_values: BTreeSet<Variable> =
        rigid_union.iter().map(|var_id| utable.probe_value(*var_id)).collect();

    for flex_var_id in potentially_missing_flex_ids {
        let flex_val = utable.probe_value(*flex_var_id);

        if !rigid_union_values.contains(&flex_val) {
            return false;
        }
    }

    true
}

#[inline]
fn unify_flex_union(utable: &mut UTable, flex_union: &BTreeSet<VarId>, var: &Variable, other: &Variable) -> Variable {
    match other {
        Wildcard => var.clone(),
        RigidVar(_) => Mismatch,
        RigidUnion(rigid_union) => {
            if rigid_union_fits_flex_union(utable, &rigid_union, &flex_union) {
                other.clone()
            } else {
                Mismatch
            }
        },
        FlexUnion(other_union) => unify_flex_unions(&flex_union, &other_union),
        Structure(_) => unify_flex_union_with_structure(&flex_union, other),
        Mismatch => other.clone()
    }
}

#[inline]
fn unify_flex_unions(my_union: &BTreeSet<VarId>, other_union: &BTreeSet<VarId>) -> Variable {
    let ids_in_common = my_union.intersection(other_union);
    let unified_union: BTreeSet<VarId> = ids_in_common.into_iter().map(|var_id| *var_id).collect();

    // If they have no types in common, that's a mismatch.
    if unified_union.len() == 0 {
        Mismatch
    } else {
        FlexUnion(unified_union)
    }
}

fn unify_vars(utable: &mut UTable, first: &Variable, second: &Variable) -> Variable {
    match first {
        // wildcard types defer to whatever the other type happens to be.
        Wildcard => second.clone(),
        FlexUnion(union) => unify_flex_union(utable, &union, first, second),
        RigidVar(Name) => unify_rigid(first, second),
        RigidUnion(union) => unify_rigid_union(utable, &union, first, second),
        Structure(flat_type) => unify_structure(utable, flat_type, first, second),
        // Mismatches propagate.
        Mismatch => first.clone()
    }
}

#[inline]
pub fn unify_structure(utable: &mut UTable, flat_type: &FlatType, var: &Variable, other: &Variable) -> Variable {
    match other {
        Wildcard => var.clone(),
        RigidVar(_) => Mismatch,
        FlexUnion(flex_union) => unify_flex_union_with_structure(&flex_union, var),
        RigidUnion(_) => Mismatch,
        Structure(other_flat_type) => unify_flat_types(utable, flat_type, other_flat_type),
        Mismatch => other.clone()
    }
}

#[inline]
pub fn unify_flat_types(utable: &mut UTable, flat_type: &FlatType, other_flat_type: &FlatType) -> Variable {
    match (flat_type, other_flat_type) {
        (FlatType::Function(my_arg, my_return),
         FlatType::Function(other_arg, other_return)) => {
            let new_arg = unify_var_ids(utable, *my_arg, *other_arg);
            let new_return = unify_var_ids(utable, *my_return, *other_return);

            // Propagate any mismatches.
            if new_arg == Mismatch {
                new_arg
            } else if new_return == Mismatch {
                new_return
            } else {
                let new_arg_id = utable.new_key(new_arg);
                let new_return_id = utable.new_key(new_return);

                Structure(FlatType::Function(new_arg_id, new_return_id))
            }
        },
        (FlatType::Function(_, __return), _) => Mismatch,
        (_, FlatType::Function(_, __return)) => Mismatch,
        (FlatType::Tuple2(my_first, my_second),
         FlatType::Tuple2(other_first, other_second)) => {
            let new_first = unify_var_ids(utable, *my_first, *other_first);
            let new_second = unify_var_ids(utable, *my_second, *other_second);

            // Propagate any mismatches.
            if new_first == Mismatch {
                new_first
            } else if new_second == Mismatch {
                new_second
            } else {
                let new_first_id = utable.new_key(new_first);
                let new_second_id = utable.new_key(new_second);

                Structure(FlatType::Tuple2(new_first_id, new_second_id))
            }
        },
        (FlatType::Tuple2(_, _), _) => Mismatch,
        (_, FlatType::Tuple2(_, _)) => Mismatch,
        (FlatType::Tuple3(my_first, my_second, my_third),
         FlatType::Tuple3(other_first, other_second, other_third)) => {
            let new_first = unify_var_ids(utable, *my_first, *other_first);
            let new_second = unify_var_ids(utable, *my_second, *other_second);
            let new_third = unify_var_ids(utable, *my_third, *other_third);

            // Propagate any mismatches.
            if new_first == Mismatch {
                new_first
            } else if new_second == Mismatch {
                new_second
            } else if new_third == Mismatch {
                new_third
            } else {
                let new_first_id = utable.new_key(new_first);
                let new_second_id = utable.new_key(new_second);
                let new_third_id = utable.new_key(new_third);

                Structure(FlatType::Tuple3(new_first_id, new_second_id, new_third_id))
            }
        },
        // (FlatType::Tuple3(_, _, _), _) => Mismatch,
        // (_, FlatType::Tuple3(_, _, _)) => Mismatch,
    }
}

#[inline]
fn unify_flex_union_with_structure(flex_union: &BTreeSet<VarId>, var: &Variable) -> Variable {
    // TODO I guess iterate through the set, looking up Variables
    
    panic!("TODO");
    // if flex_union.contains(var) {
        // Narrow the union to the one member type
        var.clone()
    // } else {
    //     Mismatch
    // }
}

type ExpectedType = Type;

pub enum Constraint {
    True,
    Equal(Type, ExpectedType),
    Batch(Vec<Constraint>),
}

pub fn infer_type(expr: Expr) -> Result<Type, Problem> {
    Err(Problem::Mismatch)
}

struct State {
    errors: Vec<String>
}

// Given a type, create a constraint variable for it and add it to the table.
// Return the VarId corresponding to the variable in the table.
fn type_to_var_id(utable: &mut UTable, typ: Type) -> VarId {
    match typ {
        Type::CallOperator(op, box left_type, box right_type) => {
            let left_var_id = type_to_var_id(utable, left_type);
            let right_var_id = type_to_var_id(utable, right_type);

            // TODO should we match on op to hardcode the types we expect?
            let flat_type = FlatType::Function(left_var_id, right_var_id);

            utable.new_key(Structure(flat_type))
        }
    }
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct VarId(u32);

impl UnifyKey for VarId {
    type Value = Variable;

    fn index(&self) -> u32 { self.0 }
    fn from_index(u: u32) -> VarId { VarId(u) }

    // tag is a static string that's only used in debugging
    fn tag() -> &'static str { "VarId" }
}

fn unify_var_ids(utable: &mut UTable, left_id: VarId, right_id: VarId) -> Variable {
    let left_content = utable.probe_value(left_id);
    let right_content = utable.probe_value(right_id);

    if left_content == right_content {
        left_content
    } else {
       unify_vars(utable, &left_content, &right_content)
    }
}

type TypeError = String;

pub fn solve(utable: &mut UTable, errors: &mut Vec<TypeError>, constraint: Constraint) {
    match constraint {
        Constraint::True => {},

        Constraint::Equal(actual_type, expectation) => {
            let actual_var_id = type_to_var_id(utable, actual_type);
            let expected_var_id = type_to_var_id(utable, expectation);
            let answer = unify_var_ids(utable, actual_var_id, expected_var_id);

            panic!("Oh no! TYPE MISMATCH! (TODO: record errors as appropriate)");
            ()
            // match answer {
            //     Mismatch => {
            //         panic!("Oh no! TYPE MISMATCH! (TODO: record errors as appropriate)");
            //     }
                    // do  introduce rank pools vars
                    //     return state

                    // UF.modify var $ \(Descriptor content _ mark copy) ->
                    // Descriptor content rank mark copy

                // Unify.Err vars actualType expectedType ->

                //     panic!("TODO xyz");
                // do  introduce rank pools vars
                //     return $ addError state $
                //         Error.BadExpr region category actualType $
                //         Error.typeReplace expectation expectedType
            // }
        },

        Constraint::Batch(_) => {
            panic!("TODO");
            ()
        }
    }
}

