use std::collections::HashMap;

use schemars::{schema::RootSchema, schema_for, JsonSchema};
use serde::Serialize;

#[derive(Serialize, JsonSchema, Debug, PartialEq)]
pub struct Variable(pub u32);

macro_rules! impl_content {
    ($($name:ident { $($arg:ident: $ty:ty,)* },)*) => {
        #[derive(Serialize, JsonSchema, Debug)]
        #[serde(tag = "type")]
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
                pub fn $name($($arg: $ty),*) -> Self {
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
    ErasedLambda {},
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
        fx: Variable,
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
    EffectfulFunc {},
    RangedNumber {
        range: NumericRange,
    },
    Pure {},
    Effectful {},
    Error {},
}

#[derive(Serialize, JsonSchema, Debug)]
pub struct ClosureType {
    pub function: Symbol,
    pub environment: Vec<Variable>,
}

#[derive(Serialize, JsonSchema, Debug)]
pub struct UnspecializedClosureType {
    pub specialization: Variable,
    pub ability_member: Symbol,
    pub lambda_set_region: u8,
}

#[derive(Serialize, JsonSchema, Debug)]
#[serde(tag = "type")]
pub enum AliasKind {
    Structural,
    Opaque,
}

#[derive(Serialize, JsonSchema, Debug)]
pub struct AliasTypeVariables {
    pub type_variables: Vec<Variable>,
    pub lambda_set_variables: Vec<Variable>,
    pub infer_ext_in_output_position_variables: Vec<Variable>,
}

#[derive(Serialize, JsonSchema, Debug)]
pub struct RecordField {
    pub kind: RecordFieldKind,
    pub field_type: Variable,
}

#[derive(Serialize, JsonSchema, Debug)]
#[serde(tag = "type")]
pub enum RecordFieldKind {
    Demanded,
    Required { rigid: bool },
    Optional { rigid: bool },
}

#[derive(Serialize, JsonSchema, Debug)]
#[serde(tag = "type", content = "variable")]
pub enum TagUnionExtension {
    Openness(Variable),
    Any(Variable),
}

#[derive(Serialize, JsonSchema, Debug)]
pub struct NumericRange {
    pub kind: NumericRangeKind,
    pub signed: bool,
    pub min_width: u32,
}

#[derive(Serialize, JsonSchema, Debug)]
#[serde(tag = "type")]
pub enum NumericRangeKind {
    Int,
    AnyNum,
}

#[derive(Serialize, JsonSchema, Debug)]
pub struct Rank(pub u32);

#[derive(Serialize, JsonSchema, Debug)]
pub struct Symbol(
    // TODO: should this be module ID + symbol?
    pub String,
);

#[derive(Serialize, JsonSchema, Debug)]
#[serde(tag = "type")]
pub enum UnificationMode {
    Eq,
    Present,
    LambdaSetSpecialization,
}

#[derive(Serialize, JsonSchema, Debug)]
#[serde(tag = "type")]
pub enum Event {
    Unification {
        left: Variable,
        right: Variable,
        mode: UnificationMode,
        success: Option<bool>,
        subevents: Vec<Event>,
    },
    VariableUnified {
        from: Variable,
        to: Variable,
    },
    VariableSetDescriptor {
        variable: Variable,
        rank: Option<Rank>,
        content: Option<Content>,
    },
}

#[derive(Serialize, JsonSchema, Debug)]
pub struct AllEvents(pub Vec<Event>);

impl AllEvents {
    pub fn schema() -> RootSchema {
        schema_for!(AllEvents)
    }

    pub fn write(&self, writer: impl std::io::Write) -> Result<(), serde_json::Error> {
        serde_json::to_writer(writer, self)
    }
}
