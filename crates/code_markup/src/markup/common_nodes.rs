use crate::{
    slow_pool::{MarkNodeId, SlowPool},
    syntax_highlight::HighlightStyle,
};

use super::{
    attribute::Attributes,
    nodes::MarkupNode,
    nodes::{self, make_nested_mn},
};

pub fn new_equals_mn() -> MarkupNode {
    common_text_node(nodes::EQUALS.to_owned(), HighlightStyle::Operator, 0)
}

pub fn new_comma_mn() -> MarkupNode {
    common_text_node(nodes::COMMA.to_owned(), HighlightStyle::Operator, 0)
}

pub fn new_dot_mn() -> MarkupNode {
    common_text_node(nodes::DOT.to_owned(), HighlightStyle::Operator, 0)
}

pub fn new_blank_mn() -> MarkupNode {
    MarkupNode::Blank {
        attributes: Attributes::default(),
        parent_id_opt: None,
        newlines_at_end: 0,
    }
}

pub fn new_blank_mn_w_nls(nr_of_newlines: usize) -> MarkupNode {
    MarkupNode::Blank {
        attributes: Attributes::default(),
        parent_id_opt: None,
        newlines_at_end: nr_of_newlines,
    }
}

pub fn new_colon_mn() -> MarkupNode {
    new_operator_mn(nodes::COLON.to_owned())
}

pub fn new_operator_mn(content: String) -> MarkupNode {
    common_text_node(content, HighlightStyle::Operator, 0)
}

pub fn new_left_accolade_mn() -> MarkupNode {
    common_text_node(nodes::LEFT_ACCOLADE.to_owned(), HighlightStyle::Bracket, 0)
}

pub fn new_right_accolade_mn() -> MarkupNode {
    common_text_node(nodes::RIGHT_ACCOLADE.to_owned(), HighlightStyle::Bracket, 0)
}

pub fn new_left_square_mn() -> MarkupNode {
    common_text_node(nodes::LEFT_SQUARE_BR.to_owned(), HighlightStyle::Bracket, 0)
}

pub fn new_right_square_mn() -> MarkupNode {
    common_text_node(
        nodes::RIGHT_SQUARE_BR.to_owned(),
        HighlightStyle::Bracket,
        0,
    )
}

pub fn new_func_name_mn(content: String) -> MarkupNode {
    common_text_node(content, HighlightStyle::FunctionName, 0)
}

pub fn new_arg_name_mn(content: String) -> MarkupNode {
    common_text_node(content, HighlightStyle::FunctionArgName, 0)
}

pub fn new_arrow_mn(newlines_at_end: usize) -> MarkupNode {
    common_text_node(
        nodes::ARROW.to_owned(),
        HighlightStyle::Operator,
        newlines_at_end,
    )
}

pub fn new_comments_mn(comment: String, newlines_at_end: usize) -> MarkupNode {
    common_text_node(comment, HighlightStyle::Comment, newlines_at_end)
}

fn common_text_node(
    content: String,
    highlight_style: HighlightStyle,
    newlines_at_end: usize,
) -> MarkupNode {
    MarkupNode::Text {
        content,
        syn_high_style: highlight_style,
        attributes: Attributes::default(),
        parent_id_opt: None,
        newlines_at_end,
    }
}

pub const NEW_LINES_AFTER_DEF: usize = 2;

pub fn new_assign_mn(
    val_name_mn_id: MarkNodeId,
    equals_mn_id: MarkNodeId,
    expr_mark_node_id: MarkNodeId,
) -> MarkupNode {
    make_nested_mn(
        vec![val_name_mn_id, equals_mn_id, expr_mark_node_id],
        NEW_LINES_AFTER_DEF,
    )
}

pub fn new_module_name_mn_id(mn_ids: Vec<MarkNodeId>, mark_node_pool: &mut SlowPool) -> MarkNodeId {
    if mn_ids.len() == 1 {
        *mn_ids.get(0).unwrap() // safe because we checked the length before
    } else {
        let nested_node = make_nested_mn(mn_ids, 0);
        mark_node_pool.add(nested_node)
    }
}

pub fn new_module_var_mn(
    module_name_id: MarkNodeId,
    dot_id: MarkNodeId,
    ident_id: MarkNodeId,
) -> MarkupNode {
    make_nested_mn(vec![module_name_id, dot_id, ident_id], 0)
}

pub fn if_mn() -> MarkupNode {
    keyword_mn("if ")
}

pub fn then_mn() -> MarkupNode {
    keyword_mn(" then ")
}

pub fn else_mn() -> MarkupNode {
    keyword_mn(" else ")
}

fn keyword_mn(keyword: &str) -> MarkupNode {
    common_text_node(keyword.to_owned(), HighlightStyle::Keyword, 0)
}

pub fn new_if_expr_mn(
    if_mn_id: MarkNodeId,
    cond_expr_mn_id: MarkNodeId,
    then_mn_id: MarkNodeId,
    then_expr_mn_id: MarkNodeId,
    else_mn_id: MarkNodeId,
    else_expr_mn_id: MarkNodeId,
) -> MarkupNode {
    make_nested_mn(
        vec![
            if_mn_id,
            cond_expr_mn_id,
            then_mn_id,
            then_expr_mn_id,
            else_mn_id,
            else_expr_mn_id,
        ],
        1,
    )
}
