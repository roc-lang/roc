use roc_parse::ast::{self, ValueDef, TypeDef};

use crate::ir::IR;
use roc_collections::all::Either;

pub fn can_defs<'a>(ir: &mut IR<'a>, defs: impl Iterator<Item=Either<ValueDef<'a>, TypeDef<'a>>>) {
    for def in defs {
        match def {
            Either::First(value_def) => {
                match value_def {
                    ValueDef::Annotation(_, _) => todo!(),
                    ValueDef::Body(_, _) => todo!(),
                    ValueDef::AnnotatedBody { ann_pattern, ann_type, comment, body_pattern, body_expr } => todo!(),
                    ValueDef::Dbg { condition, preceding_comment } => todo!(),
                    ValueDef::Expect { condition, preceding_comment } => todo!(),
                    ValueDef::ExpectFx { condition, preceding_comment } => todo!(),
                }
            }
            Either::Second(type_def) => {
                match type_def {
                    TypeDef::Alias { header, ann } => todo!(),
                    TypeDef::Opaque { header, typ, derived } => todo!(),
                    TypeDef::Ability { header, loc_implements, members } => todo!(),
                }
            }
        }
    }
}
