use std::collections::HashMap;

use roc_ast::lang::core::ast::ASTNodeId;

use crate::markup_error::MarkNodeIdWithoutCorrespondingASTNodeIdSnafu;
use crate::{markup_error::MarkResult, slow_pool::MarkNodeId};

/// A hashmap is wrapped to allow for an easy swap out with more performant alternatives
#[derive(Debug, Default)]
pub struct MarkIdAstIdMap {
    map: HashMap<MarkNodeId, ASTNodeId>,
}

impl MarkIdAstIdMap {
    pub fn insert(&mut self, mn_id: MarkNodeId, ast_id: ASTNodeId) {
        self.map.insert(mn_id, ast_id);
    }

    pub fn get(&self, mn_id: MarkNodeId) -> MarkResult<ASTNodeId> {
        match self.map.get(&mn_id) {
            Some(ast_node_id) => Ok(*ast_node_id),
            None => MarkNodeIdWithoutCorrespondingASTNodeIdSnafu {
                node_id: mn_id,
                keys_str: format!("{:?}", self.map.keys()),
            }
            .fail(),
        }
    }
}
