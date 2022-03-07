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

use super::{
    common_nodes::new_assign_mn, convert::from_def2::add_node, mark_id_ast_id_map::MarkIdAstIdMap,
};

// represents for example: `main = "Hello, World!"`
pub fn assignment_mark_node<'a>(
    identifier_id: IdentId,
    expr_mark_node_id: MarkNodeId,
    ast_node_id: ASTNodeId,
    mark_node_pool: &mut SlowPool,
    mark_id_ast_id_map: &mut MarkIdAstIdMap,
    env: &Env<'a>,
) -> ASTResult<MarkupNode> {
    let val_name = env.ident_ids.get_name_str_res(identifier_id)?;

    let val_name_mn = MarkupNode::Text {
        content: val_name.to_owned(),
        syn_high_style: HighlightStyle::Value,
        attributes: Attributes::default(),
        parent_id_opt: None,
        newlines_at_end: 0,
    };

    let val_name_mn_id = add_node(val_name_mn, ast_node_id, mark_node_pool, mark_id_ast_id_map);

    let equals_mn_id = add_node(
        new_equals_mn(),
        ast_node_id,
        mark_node_pool,
        mark_id_ast_id_map,
    );

    Ok(new_assign_mn(
        val_name_mn_id,
        equals_mn_id,
        expr_mark_node_id,
    ))
}

pub fn tld_w_comments_mark_node(
    comments: String,
    def_mark_node_id: MarkNodeId,
    ast_node_id: ASTNodeId,
    mark_node_pool: &mut SlowPool,
    mark_id_ast_id_map: &mut MarkIdAstIdMap,
    comments_before: bool,
) -> ASTResult<MarkupNode> {
    let comment_mn_id = add_node(
        new_comments_mn(comments, 1),
        ast_node_id,
        mark_node_pool,
        mark_id_ast_id_map,
    );

    let children_ids = if comments_before {
        vec![comment_mn_id, def_mark_node_id]
    } else {
        vec![def_mark_node_id, comment_mn_id]
    };

    let tld_w_comment_node = MarkupNode::Nested {
        children_ids,
        parent_id_opt: None,
        newlines_at_end: 2,
    };

    Ok(tld_w_comment_node)
}
