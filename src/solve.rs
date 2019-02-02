use std::collections::BTreeSet;
use self::VarContent::*;
use self::Operator::*;
use ena::unify::UnificationTable;
use ena::unify::UnifyValue;
use ena::unify::InPlace;

pub type Name<'a> = &'a str;

pub type ModuleName<'a> = &'a str;

type UTable<'a> = UnificationTable<InPlace<Variable<'a>>>;

type TypeUnion<'a> = BTreeSet<Type<'a>>;
type VarUnion<'a> = BTreeSet<VarContent<'a>>;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Type<'a> {
    Symbol(&'a str),
    Int,
    Float,
    Number,
    Function(Box<Type<'a>>, Box<Type<'a>>),
    CallOperator(Operator, Box<&'a Type<'a>>, Box<&'a Type<'a>>),
}


#[derive(Debug, PartialEq)]
pub enum Expr<'a> {
    HexOctalBinary(i64),    // : Int
    FractionalNumber(f64),  // : Float
    WholeNumber(i64),       // : Int | Float

    // Functions
    CallOperator(Operator, Box<&'a Expr<'a>>, Box<&'a Expr<'a>>),
}

#[derive(Debug, PartialEq)]
pub enum Operator {
    Plus, Minus, FloatDivision, IntDivision,
}

#[derive(Debug, PartialEq)]
pub enum Problem {
    Mismatch
}

#[derive(Debug, PartialEq, Clone)]
pub struct Variable<'a> {
    content: VarContent<'a>,
    rank: u8
}

#[derive(Debug, PartialEq)]
enum VarContent<'a> {
    Wildcard,
    RigidVar(&'a Name<'a>),
    FlexUnion(TypeUnion<'a>),
    RigidUnion(TypeUnion<'a>),
    Structure(FlatType<'a>),
    Mismatch
}

fn unify_rigid<'a>(named: &'a VarContent<'a>, other: &'a VarContent<'a>) -> &'a VarContent<'a> {
    match other {
        Wildcard => named,
        RigidVar(_) => Mismatch,
        FlexUnion(_) => Mismatch,
        RigidUnion(_) => Mismatch,
        Mismatch => other
    }
}

fn unify_rigid_union<'a>(rigid_union: &'a VarUnion<'a>, var: &'a VarContent<'a>, other: &'a VarContent<'a>) -> &'a VarContent<'a> {
    match other {
        Wildcard => var,
        RigidVar(_) => Mismatch,
        FlexUnion(flex_union) => {
            // a flex union can conform to a rigid one, as long as
            // as the rigid union contains all the flex union's options
            if rigid_union.is_subset(flex_union) {
                var
            } else {
                Mismatch
            }
        },
        RigidUnion(_) => Mismatch,
        Mismatch => other
    }
}

fn unify_flex_union<'a>(flex_union: &'a VarUnion<'a>, var: &'a VarContent<'a>, other: &'a VarContent<'a>) -> &'a VarContent<'a> {
    match other {
        Wildcard => var,
        RigidVar(_) => Mismatch,
        RigidUnion(rigid_union) => {
            // a flex union can conform to a rigid one, as long as
            // as the rigid union contains all the flex union's options
            if rigid_union.is_subset(flex_union) {
                other
            } else {
                Mismatch
            }
        },
        FlexUnion(other_union) => unify_flex_unions(flex_union, var, other_union, other),
        Structure(flat_type) => unify_flex_union_with_flat_type(flex_union, flat_type),
        Mismatch => other
    }
}

fn unify_flex_unions<'a>(my_union: &'a VarUnion<'a>, my_var: &'a VarContent<'a>, other_union: &'a VarUnion<'a>, other_var: &'a VarContent<'a>) -> &'a VarContent<'a> {
    // Prioritize not allocating a new BTreeSet if possible.
    if my_union == other_union {
        return my_var;
    }

    let types_in_common = my_union.intersection(other_union);

    if types_in_common.is_empty() {
        Mismatch
    } else {
        let unified_union: VarUnion<'a> = types_in_common.into_iter().collect();

        FlexUnion(unified_union)
    }
}

fn actually_unify<'a>(first: &'a VarContent<'a>, second: &'a VarContent<'a>) -> &'a VarContent<'a> {
    match first {
        // wildcard types defer to whatever the other type happens to be.
        Wildcard => second,
        FlexUnion(union) => unify_flex_union(union, first, second),
        RigidVar(Name) => unify_rigid(first, second),
        RigidUnion(union) => unify_rigid_union(union, first, second),
        Structure(flat_type) => unify_structure(flat_type, first, second),
        // Mismatches propagate.
        Mismatch => first
    }
}

type CanonicalModuleName = String;

enum FlatType<'a> {
    Function(Variable<'a>, Variable<'a>),
    // Apply a higher-kinded type constructor by name
    // e.g. apply `Array` to the variable `Int` to form `Array Int`
    // ApplyTypeConstructor(CanonicalModuleName, Name, &'a Variable<'a>)
    Tuple2(Variable<'a>, Variable<'a>),
    // Tuple3(Variable<'a>, Variable<'a>, Variable<'a>),
    // TupleN(Vec<Variable<'a>>), // Last resort - allocates
    // Record1 (Map.Map N.Name Variable) Variable,
}

fn unify_args<'a>(arg1: &'a Variable<'a>, arg2: Variable) -> Result<Vec<Variable<'a>>, Vec<Variable<'a>>> {
    guarded_unify(arg1, arg2)
    // case subUnify arg1 arg2 of
    // Unify k ->
    //     k vars
    //     (\vs () -> unifyArgs vs context others1 others2 ok err)
    //     (\vs () -> unifyArgs vs context others1 others2 err err)
}

fn guarded_unify<'a>(utable: UTable<'a>, left: Variable<'a>, right: Variable<'a>) -> Result<(), ()> {
    if utable.unioned(left, right) {
        Ok(())
    } else {
        let left_descriptor = utable.probe_key(left);
        let right_descriptor = utable.probe_key(right);

        actually_unify(left, left_descriptor, right, right_descriptor)
    }
}

pub fn unify_structure<'a>(utable: &'a mut UTable<'a>, flat_type: &'a FlatType<'a>, var: &'a VarContent<'a>, other: &'a VarContent<'a>) -> &'a VarContent<'a> {
    match other {
        Wildcard => var,
        RigidVar(_) => Mismatch,
        FlexUnion(union) => unify_flex_union_with_flat_type(flex_union, flat_type),
        RigidUnion(_) => Mismatch,
        Structure(other_flat_type) =>
            match (flat_type, other) {
                (FlatType::Function(my_arg, my_return),
                 FlatType::Function(other_arg, other_return)) => {
                    guarded_unify(utable, my_arg, other_arg);
                    guarded_unify(utable, my_returned, other_returned);
                },
                (FlatType::Tuple2(my_first, my_second),
                 FlatType::Tuple2(other_first, other_second)) => {
                    guarded_unify(utable, my_first, other_first);
                    guarded_unify(utable, my_second, other_second);
                }
            }

        Mismatch =>
            other
    }
}

fn unify_flex_union_with_flat_type<'a>(utable: &'a mut UTable<'a>, flex_union: &'a VarUnion<'a>, flat_type: &'a FlatType<'a>) -> &'a VarContent<'a> {
    if var_union_contains(flex_union, flat_type) {
        // This will use the UnifyValue trait to unify the values.
        utable.union(var1, var2);
    } else {
        Mismatch
    }
}


type ExpectedType<'a> = Type<'a>;

pub enum Constraint<'a> {
    True,
    Equal(Type<'a>, ExpectedType<'a>),
    Batch(Vec<Constraint<'a>>),
}

pub fn infer_type<'a>(expr: Expr<'a>) -> Result<Type<'a>, Problem> {
    Err(Problem::Mismatch)
}

struct State {
    errors: Vec<String>
}


impl<'a> UnifyValue for Variable<'a> {
    // We return our own Mismatch variant to track errors.
    type Error = ena::unify::NoError;

    fn unify_values(value1: &'a Variable<'a>, value2: &'a Variable<'a>) -> Result<Variable<'a>, ena::unify::NoError> {
        // TODO unify 'em
        
        // TODO problem: Elm's unification mutates and looks things up as it goes.
        // I can see these possible ways to proceed:
        // (1) Try to have the table's values contain a mutable reference to the table itself.
        //     This sounds like a mistake.
        // (2) Implement unification without mutating as we go.
        //     Might be too slow, and might not even work.
        //     Like, what if I need to look something up in the middle?
        // (3) Make a custom fork of ena that supports Elm's way.
        //      (3a) Change the unify_values function to accept the table itself, so it can be
        //      passed in and used during unification
        //      (3b) Change the unify_values function to accept the table itself, so it can be
        //      passed in and used during unification. I'm not super confident this would work.
        //
        // Possibly before doing any of this, I should look at ena's examples/tests
        
        // TODO also I'm pretty sure in this implementation,
        // I'm supposed to let them take care of the rank.
        Ok(Variable {content, rank: min(rank1, rank2)})
    }
}

fn type_to_var(rank: u8, typ: Type) -> Variable {
    match typ {
        Type::CallOperator(op, left_type, right_type) => {
            let left_var = type_to_var(left_type);
            let right_var = type_to_var(right_type);

            // TODO should we match on op to hardcode the types we expect?
            let flat_type = FlatType::Function(left_var, right_var);
            let content = Structure(flat_type);

            utable.new_key(Variable {rank, content})
        }
    }
}


pub fn unify(utable: Table, left_var: Variable, right_var: Variable) -> Result<(), ()>{
    let left_content = utable.probe_value(left_var);
    let right_content = utable.probe_value(right_var);

    if left_content == right_content {
        Ok(())
    } else {
        Ok(actually_unify(left, left_desc, right, right_desc))
    }
}

pub fn solve(rank: u8, state: State, constraint: Constraint) {
    match constraint {
        True =>
            state

        Equal(actual_type, expectation) => {
            let actual_var = type_to_var(rank, actual_type)
            let expected_var = type_to_var(rank, expectation)
            let answer = unify(actual_var, expected_var)

            match answer {
                Ok vars ->
                    panic!("TODO abc");
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
            }
        }
    }
}

