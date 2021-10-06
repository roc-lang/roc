use crate::{
    ast_error::{ASTNodeIdWithoutExprId, ASTResult},
    mem_pool::pool::Pool,
};

use super::{
    def::def2::{def2_to_string, DefId},
    expr::{expr2::ExprId, expr2_to_string::expr2_to_string},
    header::AppHeader,
};

#[derive(Debug)]
pub struct AST {
    pub header: AppHeader,
    pub def_ids: Vec<DefId>,
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum ASTNodeId {
    ADefId(DefId),
    AExprId(ExprId),
}

impl ASTNodeId {
    pub fn to_expr_id(&self) -> ASTResult<ExprId> {
        match self {
            ASTNodeId::AExprId(expr_id) => Ok(*expr_id),
            _ => ASTNodeIdWithoutExprId { ast_node_id: *self }.fail()?,
        }
    }

    pub fn to_def_id(&self) -> ASTResult<DefId> {
        match self {
            ASTNodeId::ADefId(def_id) => Ok(*def_id),
            _ => ASTNodeIdWithoutExprId { ast_node_id: *self }.fail()?,
        }
    }
}

pub fn ast_node_to_string(node_id: ASTNodeId, pool: &Pool) -> String {
    match node_id {
        ASTNodeId::ADefId(def_id) => def2_to_string(def_id, pool),
        ASTNodeId::AExprId(expr_id) => expr2_to_string(expr_id, pool),
    }
}
