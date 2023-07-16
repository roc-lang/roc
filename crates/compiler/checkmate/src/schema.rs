use std::collections::HashMap;

use serde::Serialize;

#[derive(Serialize)]
pub enum Constraint {}

#[derive(Serialize, Debug, PartialEq)]
pub struct Variable(pub u32);

macro_rules! impl_content {
    ($($name:ident { $($arg:ident: $ty:ty,)* },)*) => {
        #[derive(Serialize)]
        pub enum Content {
            $(
                $name {
                    $($arg: $ty),*
                },
            )*
        }

        impl Content {
            $(
                #[allow(non_snake_case)]
                pub(crate) fn $name($($arg: $ty),*) -> Self {
                    Self::$name { $($arg),* }
                }
            )*
        }
    };
}

impl_content! {
    Flex {
        name: Option<String>,
    },
    Rigid {
        name: String,
    },
    FlexAble {
        name: Option<String>,
        abilities: Vec<Symbol>,
    },
    RigidAble {
        name: String,
        abilities: Vec<Symbol>,
    },
    Recursive {
        name: Option<String>,
        structure: Variable,
    },
    LambdaSet {
        solved: Vec<ClosureType>,
        unspecialized: Vec<UnspecializedClosureType>,
        recursion_var: Option<Variable>,
        ambient_function: Variable,
    },
    Alias {
        name: Symbol,
        variables: AliasTypeVariables,
        real_variable: Variable,
        kind: AliasKind,
    },
    Apply {
        symbol: Symbol,
        variables: Vec<Variable>,
    },
    Function {
        arguments: Vec<Variable>,
        lambda_type: Variable,
        ret: Variable,
    },
    Record {
        fields: HashMap<String, RecordField>,
        extension: Variable,
    },
    Tuple {
        elements: HashMap<u32, Variable>,
        extension: Variable,
    },
    TagUnion {
        tags: HashMap<String, Vec<Variable>>,
        extension: TagUnionExtension,
    },
    FunctionOrTagUnion {
        functions: Vec<Symbol>,
        tags: Vec<String>,
        extension: TagUnionExtension,
    },
    RecursiveTagUnion {
        recursion_var: Variable,
        tags: HashMap<String, Vec<Variable>>,
        extension: TagUnionExtension,
    },
    EmptyRecord {},
    EmptyTuple {},
    EmptyTagUnion {},
    RangedNumber {
        range: NumericRange,
    },
    Error {},
}

#[derive(Serialize)]
pub struct ClosureType {
    pub function: Symbol,
    pub environment: Vec<Variable>,
}

#[derive(Serialize)]
pub struct UnspecializedClosureType {
    pub specialization: Variable,
    pub ability_member: Symbol,
    pub lambda_set_region: u8,
}

#[derive(Serialize)]
pub enum AliasKind {
    Structural,
    Opaque,
}

#[derive(Serialize)]
pub struct AliasTypeVariables {
    pub type_variables: Vec<Variable>,
    pub lambda_set_variables: Vec<Variable>,
    pub infer_ext_in_output_position_variables: Vec<Variable>,
}

#[derive(Serialize)]
pub struct RecordField {
    pub kind: RecordFieldKind,
    pub field_type: Variable,
}

#[derive(Serialize)]
#[serde(tag = "kind")]
pub enum RecordFieldKind {
    Demanded,
    Required { rigid: bool },
    Optional { rigid: bool },
}

#[derive(Serialize)]
#[serde(tag = "kind")]
pub enum TagUnionExtension {
    Openness(Variable),
    Any(Variable),
}

#[derive(Serialize)]
pub struct NumericRange {
    pub kind: NumericRangeKind,
    pub signed: bool,
    pub min_width: u32,
}

#[derive(Serialize)]
pub enum NumericRangeKind {
    Int,
    AnyNum,
}

#[derive(Serialize)]
pub struct Rank(pub u32);

#[derive(Serialize)]
pub struct Descriptor {
    pub content: Content,
    pub rank: Rank,
}

#[derive(Serialize)]
pub struct Symbol(
    // TODO: should this be module ID + symbol?
    pub String,
);

#[derive(Serialize)]
pub enum UnificationMode {
    Eq,
    Present,
    LambdaSetSpecialization,
}

#[derive(Serialize)]
pub enum Event {
    Top(Vec<Event>),
    VariableEvent(VariableEvent),
    Unification {
        left: Variable,
        right: Variable,
        mode: UnificationMode,
        success: Option<bool>,
        subevents: Vec<Event>,
    },
}

#[derive(Serialize)]
pub enum VariableEvent {
    Unify {
        from: Variable,
        to: Variable,
    },
    SetDescriptor {
        variable: Variable,
        rank: Option<Rank>,
        content: Option<Content>,
    },
}
