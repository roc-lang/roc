use crate::{
    lang::rigids::Rigids,
    mem_pool::{pool::NodeId, pool_vec::PoolVec, shallow_clone::ShallowClone},
};
use roc_module::symbol::Symbol;
use roc_types::subs::Variable;

use super::{
    expr::expr2::ExprId,
    pattern::PatternId,
    types::{Type2, TypeId},
};

#[derive(Debug)]
pub enum FunctionDef {
    WithAnnotation {
        name: Symbol,                                   // 8B
        arguments: PoolVec<(NodeId<Type2>, PatternId)>, // 8B
        rigids: NodeId<Rigids>,                         // 4B
        return_type: TypeId,                            // 4B
        body_id: ExprId,                                // 4B
    },
    NoAnnotation {
        name: Symbol,                              // 8B
        arguments: PoolVec<(Variable, PatternId)>, // 8B
        return_var: Variable,                      // 4B
        body_id: ExprId,                           // 4B
    },
}

impl ShallowClone for FunctionDef {
    fn shallow_clone(&self) -> Self {
        match self {
            Self::WithAnnotation {
                name,
                arguments,
                rigids,
                return_type,
                body_id,
            } => Self::WithAnnotation {
                name: *name,
                arguments: arguments.shallow_clone(),
                rigids: *rigids,
                return_type: *return_type,
                body_id: *body_id,
            },

            Self::NoAnnotation {
                name,
                arguments,
                return_var,
                body_id,
            } => Self::NoAnnotation {
                name: *name,
                arguments: arguments.shallow_clone(),
                return_var: *return_var,
                body_id: *body_id,
            },
        }
    }
}
