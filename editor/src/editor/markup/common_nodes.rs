use crate::{
    editor::{slow_pool::MarkNodeId, syntax_highlight::HighlightStyle},
    lang::ast::ExprId,
};

use super::{attribute::Attributes, nodes, nodes::MarkupNode};

pub fn new_equals_mn(ast_node_id: ExprId, parent_id_opt: Option<MarkNodeId>) -> MarkupNode {
    MarkupNode::Text {
        content: nodes::EQUALS.to_owned(),
        ast_node_id,
        syn_high_style: HighlightStyle::Operator,
        attributes: Attributes::new(),
        parent_id_opt,
    }
}

pub fn new_comma_mn(ast_node_id: ExprId, parent_id_opt: Option<MarkNodeId>) -> MarkupNode {
    MarkupNode::Text {
        content: nodes::COMMA.to_owned(),
        ast_node_id,
        syn_high_style: HighlightStyle::Blank,
        attributes: Attributes::new(),
        parent_id_opt,
    }
}

pub fn new_blank_mn(ast_node_id: ExprId, parent_id_opt: Option<MarkNodeId>) -> MarkupNode {
    MarkupNode::Blank {
        ast_node_id,
        syn_high_style: HighlightStyle::Blank,
        attributes: Attributes::new(),
        parent_id_opt,
    }
}

pub fn new_colon_mn(ast_node_id: ExprId, parent_id_opt: Option<MarkNodeId>) -> MarkupNode {
    MarkupNode::Text {
        content: nodes::COLON.to_owned(),
        ast_node_id,
        syn_high_style: HighlightStyle::Operator,
        attributes: Attributes::new(),
        parent_id_opt,
    }
}

pub fn new_left_accolade_mn(ast_node_id: ExprId, parent_id_opt: Option<MarkNodeId>) -> MarkupNode {
    MarkupNode::Text {
        content: nodes::LEFT_ACCOLADE.to_owned(),
        ast_node_id,
        syn_high_style: HighlightStyle::Bracket,
        attributes: Attributes::new(),
        parent_id_opt,
    }
}

pub fn new_right_accolade_mn(ast_node_id: ExprId, parent_id_opt: Option<MarkNodeId>) -> MarkupNode {
    MarkupNode::Text {
        content: nodes::RIGHT_ACCOLADE.to_owned(),
        ast_node_id,
        syn_high_style: HighlightStyle::Bracket,
        attributes: Attributes::new(),
        parent_id_opt,
    }
}

pub fn new_left_square_mn(ast_node_id: ExprId, parent_id_opt: Option<MarkNodeId>) -> MarkupNode {
    MarkupNode::Text {
        content: nodes::LEFT_SQUARE_BR.to_owned(),
        ast_node_id,
        syn_high_style: HighlightStyle::Bracket,
        attributes: Attributes::new(),
        parent_id_opt,
    }
}

pub fn new_right_square_mn(ast_node_id: ExprId, parent_id_opt: Option<MarkNodeId>) -> MarkupNode {
    MarkupNode::Text {
        content: nodes::RIGHT_SQUARE_BR.to_owned(),
        ast_node_id,
        syn_high_style: HighlightStyle::Bracket,
        attributes: Attributes::new(),
        parent_id_opt,
    }
}

pub fn new_line_mn(ast_node_id: ExprId, parent_id_opt: Option<MarkNodeId>) -> MarkupNode {
    MarkupNode::Text {
        content: "\n".to_owned(),
        ast_node_id,
        syn_high_style: HighlightStyle::Blank,
        attributes: Attributes::new(),
        parent_id_opt,
    }
}
