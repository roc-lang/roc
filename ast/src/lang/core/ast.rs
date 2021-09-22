use crate::ast_error::{ASTResult, ASTNodeIdWithoutExprId};

use super::{def::def2::DefId, expr::expr2::ExprId, header::AppHeader};

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