use crate::{editor::{slow_pool::{MarkNodeId, SlowPool}, syntax_highlight::HighlightStyle}, lang::ast::ExprId};

use super::{attribute::Attributes, nodes::MarkupNode, nodes};


pub fn new_equals_mn(ast_node_id: ExprId, parent_id_opt: Option<MarkNodeId>, mn_pool: &mut SlowPool) -> MarkNodeId {
    let equals_mark_node = MarkupNode::Text {
        content: nodes::EQUALS.to_owned(),
        ast_node_id,
        syn_high_style: HighlightStyle::Operator,
        attributes: Attributes::new(),
        parent_id_opt,
    };

    mn_pool.add(equals_mark_node)
}

pub fn new_comma_mn(ast_node_id: ExprId, parent_id_opt: Option<MarkNodeId>, mn_pool: &mut SlowPool) -> MarkNodeId {
    let comma_mark_node = MarkupNode::Text {
        content: nodes::COMMA.to_owned(),
        ast_node_id,
        syn_high_style: HighlightStyle::Blank,
        attributes: Attributes::new(),
        parent_id_opt,
    };

    mn_pool.add(comma_mark_node)
}

pub fn new_blank_mn(ast_node_id: ExprId, parent_id_opt: Option<MarkNodeId>, mn_pool: &mut SlowPool) -> MarkNodeId {
    let blank_mark_node = MarkupNode::Blank {
        ast_node_id,
        syn_high_style: HighlightStyle::Blank,
        attributes: Attributes::new(),
        parent_id_opt,
    };

    mn_pool.add(blank_mark_node)
}
