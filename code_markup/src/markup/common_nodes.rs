use roc_ast::lang::core::{ast::ASTNodeId, expr::expr2::ExprId};

use crate::{slow_pool::MarkNodeId, syntax_highlight::HighlightStyle};

use super::{attribute::Attributes, nodes, nodes::MarkupNode};

pub fn new_equals_mn(ast_node_id: ASTNodeId, parent_id_opt: Option<MarkNodeId>) -> MarkupNode {
    MarkupNode::Text {
        content: nodes::EQUALS.to_owned(),
        ast_node_id,
        syn_high_style: HighlightStyle::Operator,
        attributes: Attributes::default(),
        parent_id_opt,
        newlines_at_end: 0,
    }
}

pub fn new_comma_mn(expr_id: ExprId, parent_id_opt: Option<MarkNodeId>) -> MarkupNode {
    new_comma_mn_ast(ASTNodeId::AExprId(expr_id), parent_id_opt)
}

pub fn new_comma_mn_ast(ast_node_id: ASTNodeId, parent_id_opt: Option<MarkNodeId>) -> MarkupNode {
    MarkupNode::Text {
        content: nodes::COMMA.to_owned(),
        ast_node_id,
        syn_high_style: HighlightStyle::Comma,
        attributes: Attributes::default(),
        parent_id_opt,
        newlines_at_end: 0,
    }
}

pub fn new_blank_mn(ast_node_id: ASTNodeId, parent_id_opt: Option<MarkNodeId>) -> MarkupNode {
    MarkupNode::Blank {
        ast_node_id,
        attributes: Attributes::default(),
        parent_id_opt,
        newlines_at_end: 0,
    }
}

pub fn new_blank_mn_w_nls(
    ast_node_id: ASTNodeId,
    parent_id_opt: Option<MarkNodeId>,
    nr_of_newlines: usize,
) -> MarkupNode {
    MarkupNode::Blank {
        ast_node_id,
        attributes: Attributes::default(),
        parent_id_opt,
        newlines_at_end: nr_of_newlines,
    }
}

pub fn new_colon_mn(expr_id: ExprId, parent_id_opt: Option<MarkNodeId>) -> MarkupNode {
    new_operator_mn(nodes::COLON.to_owned(), expr_id, parent_id_opt)
}

pub fn new_operator_mn(
    content: String,
    expr_id: ExprId,
    parent_id_opt: Option<MarkNodeId>,
) -> MarkupNode {
    MarkupNode::Text {
        content,
        ast_node_id: ASTNodeId::AExprId(expr_id),
        syn_high_style: HighlightStyle::Operator,
        attributes: Attributes::default(),
        parent_id_opt,
        newlines_at_end: 0,
    }
}

pub fn new_left_accolade_mn(expr_id: ExprId, parent_id_opt: Option<MarkNodeId>) -> MarkupNode {
    MarkupNode::Text {
        content: nodes::LEFT_ACCOLADE.to_owned(),
        ast_node_id: ASTNodeId::AExprId(expr_id),
        syn_high_style: HighlightStyle::Bracket,
        attributes: Attributes::default(),
        parent_id_opt,
        newlines_at_end: 0,
    }
}

pub fn new_right_accolade_mn(expr_id: ExprId, parent_id_opt: Option<MarkNodeId>) -> MarkupNode {
    MarkupNode::Text {
        content: nodes::RIGHT_ACCOLADE.to_owned(),
        ast_node_id: ASTNodeId::AExprId(expr_id),
        syn_high_style: HighlightStyle::Bracket,
        attributes: Attributes::default(),
        parent_id_opt,
        newlines_at_end: 0,
    }
}

pub fn new_left_square_mn(expr_id: ExprId, parent_id_opt: Option<MarkNodeId>) -> MarkupNode {
    MarkupNode::Text {
        content: nodes::LEFT_SQUARE_BR.to_owned(),
        ast_node_id: ASTNodeId::AExprId(expr_id),
        syn_high_style: HighlightStyle::Bracket,
        attributes: Attributes::default(),
        parent_id_opt,
        newlines_at_end: 0,
    }
}

pub fn new_right_square_mn(expr_id: ExprId, parent_id_opt: Option<MarkNodeId>) -> MarkupNode {
    MarkupNode::Text {
        content: nodes::RIGHT_SQUARE_BR.to_owned(),
        ast_node_id: ASTNodeId::AExprId(expr_id),
        syn_high_style: HighlightStyle::Bracket,
        attributes: Attributes::default(),
        parent_id_opt,
        newlines_at_end: 0,
    }
}

pub fn new_func_name_mn(content: String, expr_id: ExprId) -> MarkupNode {
    MarkupNode::Text {
        content,
        ast_node_id: ASTNodeId::AExprId(expr_id),
        syn_high_style: HighlightStyle::FunctionName,
        attributes: Attributes::default(),
        parent_id_opt: None,
        newlines_at_end: 0,
    }
}

pub fn new_arg_name_mn(content: String, expr_id: ExprId) -> MarkupNode {
    MarkupNode::Text {
        content,
        ast_node_id: ASTNodeId::AExprId(expr_id),
        syn_high_style: HighlightStyle::FunctionArgName,
        attributes: Attributes::default(),
        parent_id_opt: None,
        newlines_at_end: 0,
    }
}

pub fn new_arrow_mn(ast_node_id: ASTNodeId, newlines_at_end: usize) -> MarkupNode {
    MarkupNode::Text {
        content: nodes::ARROW.to_owned(),
        ast_node_id,
        syn_high_style: HighlightStyle::Operator,
        attributes: Attributes::default(),
        parent_id_opt: None,
        newlines_at_end,
    }
}
