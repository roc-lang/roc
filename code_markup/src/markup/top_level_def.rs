use roc_ast::{
    ast_error::ASTResult,
    lang::{core::ast::ASTNodeId, env::Env},
};
use roc_module::symbol::IdentId;

use crate::{
    markup::{
        attribute::Attributes,
        common_nodes::{new_comments_mn, new_equals_mn},
        nodes::MarkupNode,
    },
    slow_pool::{MarkNodeId, SlowPool},
    syntax_highlight::HighlightStyle,
};

// Top Level Defined Value. example: `main = "Hello, World!"`
pub fn tld_mark_node<'a>(
    identifier_id: IdentId,
    expr_mark_node_id: MarkNodeId,
    ast_node_id: ASTNodeId,
    mark_node_pool: &mut SlowPool,
    env: &Env<'a>,
) -> ASTResult<MarkupNode> {
    let val_name = env.ident_ids.get_name_str_res(identifier_id)?;

    let val_name_mn = MarkupNode::Text {
        content: val_name.to_owned(),
        ast_node_id,
        syn_high_style: HighlightStyle::Value,
        attributes: Attributes::default(),
        parent_id_opt: None,
        newlines_at_end: 0,
    };

    let val_name_mn_id = mark_node_pool.add(val_name_mn);

    let equals_mn_id = mark_node_pool.add(new_equals_mn(ast_node_id, None));

    let full_let_node = MarkupNode::Nested {
        ast_node_id,
        children_ids: vec![val_name_mn_id, equals_mn_id, expr_mark_node_id],
        parent_id_opt: None,
        newlines_at_end: 3,
    };

    Ok(full_let_node)
}

pub fn tld_w_comments_mark_node(
    comments: String,
    def_mark_node_id: MarkNodeId,
    ast_node_id: ASTNodeId,
    mark_node_pool: &mut SlowPool,
    comments_before: bool,
) -> ASTResult<MarkupNode> {
    let comment_mn_id = mark_node_pool.add(new_comments_mn(comments, ast_node_id, 1));

    let children_ids = if comments_before {
        vec![comment_mn_id, def_mark_node_id]
    } else {
        vec![def_mark_node_id, comment_mn_id]
    };

    let tld_w_comment_node = MarkupNode::Nested {
        ast_node_id,
        children_ids,
        parent_id_opt: None,
        newlines_at_end: 2,
    };

    Ok(tld_w_comment_node)
}
