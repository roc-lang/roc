use crate::{syntax_highlight::HighlightStyle};

use super::{attribute::Attributes, nodes, nodes::MarkupNode};

pub fn new_equals_mn() -> MarkupNode {
    MarkupNode::Text {
        content: nodes::EQUALS.to_owned(),
        syn_high_style: HighlightStyle::Operator,
        attributes: Attributes::default(),
        parent_id_opt: None,
        newlines_at_end: 0,
    }
}

pub fn new_comma_mn() -> MarkupNode {
    MarkupNode::Text {
        content: nodes::COMMA.to_owned(),
        syn_high_style: HighlightStyle::Comma,
        attributes: Attributes::default(),
        parent_id_opt: None,
        newlines_at_end: 0,
    }
}

pub fn new_blank_mn() -> MarkupNode {
    MarkupNode::Blank {
        attributes: Attributes::default(),
        parent_id_opt: None,
        newlines_at_end: 0,
    }
}

pub fn new_blank_mn_w_nls(
    nr_of_newlines: usize,
) -> MarkupNode {
    MarkupNode::Blank {
        attributes: Attributes::default(),
        parent_id_opt: None,
        newlines_at_end: nr_of_newlines,
    }
}

pub fn new_colon_mn() -> MarkupNode {
    new_operator_mn(nodes::COLON.to_owned())
}

pub fn new_operator_mn(
    content: String,
) -> MarkupNode {
    MarkupNode::Text {
        content,
        syn_high_style: HighlightStyle::Operator,
        attributes: Attributes::default(),
        parent_id_opt: None,
        newlines_at_end: 0,
    }
}

pub fn new_left_accolade_mn() -> MarkupNode {
    MarkupNode::Text {
        content: nodes::LEFT_ACCOLADE.to_owned(),
        syn_high_style: HighlightStyle::Bracket,
        attributes: Attributes::default(),
        parent_id_opt: None,
        newlines_at_end: 0,
    }
}

pub fn new_right_accolade_mn() -> MarkupNode {
    MarkupNode::Text {
        content: nodes::RIGHT_ACCOLADE.to_owned(),
        syn_high_style: HighlightStyle::Bracket,
        attributes: Attributes::default(),
        parent_id_opt: None,
        newlines_at_end: 0,
    }
}

pub fn new_left_square_mn() -> MarkupNode {
    MarkupNode::Text {
        content: nodes::LEFT_SQUARE_BR.to_owned(),
        syn_high_style: HighlightStyle::Bracket,
        attributes: Attributes::default(),
        parent_id_opt: None,
        newlines_at_end: 0,
    }
}

pub fn new_right_square_mn() -> MarkupNode {
    MarkupNode::Text {
        content: nodes::RIGHT_SQUARE_BR.to_owned(),
        syn_high_style: HighlightStyle::Bracket,
        attributes: Attributes::default(),
        parent_id_opt: None,
        newlines_at_end: 0,
    }
}

pub fn new_func_name_mn(content: String) -> MarkupNode {
    MarkupNode::Text {
        content,
        syn_high_style: HighlightStyle::FunctionName,
        attributes: Attributes::default(),
        parent_id_opt: None,
        newlines_at_end: 0,
    }
}

pub fn new_arg_name_mn(content: String) -> MarkupNode {
    MarkupNode::Text {
        content,
        syn_high_style: HighlightStyle::FunctionArgName,
        attributes: Attributes::default(),
        parent_id_opt: None,
        newlines_at_end: 0,
    }
}

pub fn new_arrow_mn(newlines_at_end: usize) -> MarkupNode {
    MarkupNode::Text {
        content: nodes::ARROW.to_owned(),
        syn_high_style: HighlightStyle::Operator,
        attributes: Attributes::default(),
        parent_id_opt: None,
        newlines_at_end,
    }
}

pub fn new_comments_mn(
    comments: String,
    newlines_at_end: usize,
) -> MarkupNode {
    MarkupNode::Text {
        content: comments,
        syn_high_style: HighlightStyle::Comment,
        attributes: Attributes::default(),
        parent_id_opt: None,
        newlines_at_end,
    }
}
