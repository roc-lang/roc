use crate::{
    lang::{core::expr::expr2_to_string::expr2_to_string, rigids::Rigids},
    mem_pool::{
        pool::{NodeId, Pool},
        shallow_clone::ShallowClone,
    },
};
use roc_types::subs::Variable;

use super::{
    expr::expr2::ExprId,
    pattern::{Pattern2, PatternId},
    types::TypeId,
};

#[derive(Debug)]
pub enum ValueDef {
    WithAnnotation {
        pattern_id: PatternId, // 4B
        expr_id: ExprId,       // 4B
        type_id: TypeId,
        rigids: Rigids,
        expr_var: Variable, // 4B
    },
    NoAnnotation {
        pattern_id: PatternId, // 4B
        expr_id: ExprId,       // 4B
        expr_var: Variable,    // 4B
    },
}

impl ShallowClone for ValueDef {
    fn shallow_clone(&self) -> Self {
        match self {
            Self::WithAnnotation {
                pattern_id,
                expr_id,
                type_id,
                rigids,
                expr_var,
            } => Self::WithAnnotation {
                pattern_id: *pattern_id,
                expr_id: *expr_id,
                type_id: *type_id,
                rigids: rigids.shallow_clone(),
                expr_var: *expr_var,
            },
            Self::NoAnnotation {
                pattern_id,
                expr_id,
                expr_var,
            } => Self::NoAnnotation {
                pattern_id: *pattern_id,
                expr_id: *expr_id,
                expr_var: *expr_var,
            },
        }
    }
}

impl ValueDef {
    pub fn get_expr_id(&self) -> ExprId {
        match self {
            ValueDef::WithAnnotation { expr_id, .. } => *expr_id,
            ValueDef::NoAnnotation { expr_id, .. } => *expr_id,
        }
    }

    pub fn get_pattern_id(&self) -> NodeId<Pattern2> {
        match self {
            ValueDef::WithAnnotation { pattern_id, .. } => *pattern_id,
            ValueDef::NoAnnotation { pattern_id, .. } => *pattern_id,
        }
    }
}

pub fn value_def_to_string(val_def: &ValueDef, pool: &Pool) -> String {
    match val_def {
        ValueDef::WithAnnotation {
            pattern_id,
            expr_id,
            type_id,
            rigids,
            expr_var,
        } => {
            format!("WithAnnotation {{ pattern_id: {:?}, expr_id: {:?}, type_id: {:?}, rigids: {:?}, expr_var: {:?}}}", pool.get(*pattern_id), expr2_to_string(*expr_id, pool), pool.get(*type_id), rigids, expr_var)
        }
        ValueDef::NoAnnotation {
            pattern_id,
            expr_id,
            expr_var,
        } => {
            format!(
                "NoAnnotation {{ pattern_id: {:?}, expr_id: {:?}, expr_var: {:?}}}",
                pool.get(*pattern_id),
                expr2_to_string(*expr_id, pool),
                expr_var
            )
        }
    }
}
