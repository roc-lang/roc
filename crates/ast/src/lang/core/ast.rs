use crate::{
    ast_error::{ASTNodeIdWithoutExprIdSnafu, ASTResult},
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

impl AST {
    pub fn insert_def_at_index(&mut self, new_def_id: DefId, index: usize) {
        self.def_ids.insert(index, new_def_id);
    }

    // TODO print in tree shape, similar to linux tree command
    pub fn ast_to_string(&self, pool: &Pool) -> String {
        let mut full_ast_string = String::new();

        for def_id in self.def_ids.iter() {
            full_ast_string.push_str(&def2_to_string(*def_id, pool));
            full_ast_string.push_str("\n\n");
        }

        full_ast_string
    }
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
            _ => ASTNodeIdWithoutExprIdSnafu { ast_node_id: *self }.fail()?,
        }
    }

    pub fn to_def_id(&self) -> ASTResult<DefId> {
        match self {
            ASTNodeId::ADefId(def_id) => Ok(*def_id),
            _ => ASTNodeIdWithoutExprIdSnafu { ast_node_id: *self }.fail()?,
        }
    }
}

pub fn ast_node_to_string(node_id: ASTNodeId, pool: &Pool) -> String {
    match node_id {
        ASTNodeId::ADefId(def_id) => def2_to_string(def_id, pool),
        ASTNodeId::AExprId(expr_id) => expr2_to_string(expr_id, pool),
    }
}
