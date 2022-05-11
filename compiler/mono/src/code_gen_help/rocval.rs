use roc_builtins::bitcode::{FloatWidth, IntWidth};
use roc_module::ident::TagName;
use roc_module::symbol::IdentIds;
use roc_types::subs::{Content, Subs};

use crate::ir::Expr;
use crate::layout::{Builtin, Layout, UnionLayout};

use super::CodeGenHelp;

//      a = "foo"
//      expect $(a != a)
//
//
//      roc_report(a != a, NotEq Str a a
//
//  E   expect "foo" != "foo"
//
// a: "foo"

/// THESE MUST REMAIN SORTED ALPHABETICALLY!!!
#[repr(u8)]
pub enum RocValueTag {
    Dec,
    F32,
    F64,
    Function,
    I8,
    I16,
    I32,
    I64,
    I128,
    List,
    Nat,
    Opaque,
    Record,
    Str,
    Tag,
    U8,
    U16,
    U32,
    U64,
    U128,
}

/// In Roc:
///
/// Type : [
///     Variable Str
///
///     Named { name : Str, variables : List Type }
///
///     Function (List Type) Type
///
///     Record (List { field : Str, type : Type })
///
///     TagUnion (List { tag : Str, payload : List Type }) Type
///
///     Opaque Type
/// ]
///
/// THESE MUST REMAIN SORTED ALPHABETICALLY!!!
#[repr(u8)]
pub enum RocTypeTag {
    Function,
    Named,
    Opaque,
    Record,
    TagUnion,
    Variable,
}

/// In Roc:
///
/// Value : [
///     Str Str
///     I8 I8
///     U8 U8
///     I16 I16
///     U16 U16
///     I32 I32
///     U32 U32
///     I64 I64
///     U64 U64
///     I128 I128
///     U128 U128
///     F32 F32
///     F64 F64
///     Nat Nat
///     Dec Dec
///
///     List (List Value) Type
///     Function (List Type) Type
///     Record (List { field : Str, value : Value }) Type
///     Tag Str (List Value) Type
///     Opaque Value Type
/// ]
pub const ROC_VALUE_LAYOUT: UnionLayout<'static> = UnionLayout::Recursive(&[
    &[Layout::Builtin(Builtin::Decimal)],
    &[Layout::Builtin(Builtin::Float(FloatWidth::F32))],
    &[Layout::Builtin(Builtin::Float(FloatWidth::F64))],
    // Function,
    &[
        Layout::Builtin(Builtin::List(&ROC_TYPE_LAYOUT)),
        ROC_TYPE_LAYOUT,
    ],
    &[Layout::Builtin(Builtin::Int(IntWidth::I8))],
    &[Layout::Builtin(Builtin::Int(IntWidth::I16))],
    &[Layout::Builtin(Builtin::Int(IntWidth::I32))],
    &[Layout::Builtin(Builtin::Int(IntWidth::I64))],
    &[Layout::Builtin(Builtin::Int(IntWidth::I128))],
    // List,
    &[
        Layout::Builtin(Builtin::List(&Layout::RecursivePointer)),
        ROC_TYPE_LAYOUT,
    ],
    &[Layout::Builtin(Builtin::Int(IntWidth::U64))], // Nat
    // Opaque,
    &[Layout::RecursivePointer, ROC_TYPE_LAYOUT],
    // Record
    &[
        Layout::Builtin(Builtin::List(&ROC_VALUE_RECORD_LAYOUT)),
        ROC_TYPE_LAYOUT,
    ],
    &[Layout::Builtin(Builtin::Str)],
    // Tag,
    &[
        Layout::Builtin(Builtin::Str),
        Layout::Builtin(Builtin::List(&ROC_TYPE_LAYOUT)),
        ROC_TYPE_LAYOUT,
    ],
    &[Layout::Builtin(Builtin::Int(IntWidth::U8))],
    &[Layout::Builtin(Builtin::Int(IntWidth::U16))],
    &[Layout::Builtin(Builtin::Int(IntWidth::U32))],
    &[Layout::Builtin(Builtin::Int(IntWidth::U64))],
    &[Layout::Builtin(Builtin::Int(IntWidth::U128))],
]);

pub const ROC_TYPE_LAYOUT: Layout<'static> = Layout::Builtin(Builtin::Int(IntWidth::U64));
pub const ROC_VALUE_RECORD_LAYOUT: Layout<'static> = Layout::UNIT;

pub fn generate_roc_value<'a>(ident_ids: &mut IdentIds, content: Content, subs: &Subs) -> Expr<'a> {
    match content {
        Content::FlexVar(_) => todo!(),
        Content::RigidVar(_) => todo!(),
        Content::RecursionVar {
            structure,
            opt_name,
        } => todo!(),
        Content::Structure(flat_type) => {
            use roc_types::subs::FlatType::*;

            match flat_type {
                Apply(_, _) => todo!(),
                Func(_, _, _) => todo!(),
                Record(_, _) => todo!(),
                TagUnion(_, _) => todo!(),
                FunctionOrTagUnion(_, _, _) => todo!(),
                RecursiveTagUnion(_, _, _) => todo!(),
                EmptyRecord => {
                    // Record []
                    Expr::Tag {
                        tag_layout: ROC_VALUE_LAYOUT,
                        tag_name: TagName::Global("Record".into()),
                        tag_id: RocValueTag::Record as u8 as _,
                        arguments: &[],
                    }
                }
                EmptyTagUnion => todo!("code gen a panic Stmt about how you got an empty tag union value, which should be impossible"),
                Erroneous(_) => todo!("code gen a panic Stmt"),
            }
        }
        Content::Alias(_, _, _, _) => todo!(),
        Content::RangedNumber(_, _) => todo!(),
        Content::Error => todo!(),
    }
}
