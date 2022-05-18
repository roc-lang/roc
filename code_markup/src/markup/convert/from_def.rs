use crate::{
    markup::{
        common_nodes::new_blank_mn_w_nls,
        mark_id_ast_id_map::MarkIdAstIdMap,
        nodes::MarkupNode,
        top_level_def::{assignment_mark_node, tld_w_comments_mark_node},
    },
    slow_pool::{MarkNodeId, SlowPool},
};

use super::from_expr::expr2_to_markup;

use roc_ast::{
    ast_error::ASTResult,
    lang::{
        core::{
            ast::ASTNodeId,
            def::{def2::{Def2, DefId}},
        },
        env::Env,
    },
};
use roc_module::symbol::Interns;
use roc_parse::ast::Def;

pub fn add_node(
    mark_node: MarkupNode,
    ast_node_id: ASTNodeId,
    mark_node_pool: &mut SlowPool,
    mark_id_ast_id_map: &mut MarkIdAstIdMap,
) -> MarkNodeId {
    let mark_node_id = mark_node_pool.add(mark_node);

    mark_id_ast_id_map.insert(mark_node_id, ast_node_id);

    mark_node_id
}

pub fn def_to_markup<'a>(
    env: &mut Env<'a>,
    def: &Def<'a>,
    mark_id_ast_id_map: &mut MarkIdAstIdMap,
    interns: &Interns,
) -> ASTResult<MarkNodeId> {

    let mark_node_id = match def {
        Def::Value {
            value_def
        } => {
            let expr_mn_id = expr2_to_markup(
                env,
                env.pool.get(*expr_id),
                *expr_id,
                mark_node_pool,
                mark_id_ast_id_map,
                interns,
                0,
            )?;

            let tld_mn = assignment_mark_node(
                *identifier_id,
                expr_mn_id,
                ast_node_id,
                mark_node_pool,
                mark_id_ast_id_map,
                env,
            )?;

            add_node(tld_mn, ast_node_id, mark_node_pool, mark_id_ast_id_map)
        }
        Def2::Blank => add_node(
            new_blank_mn_w_nls(2),
            ast_node_id,
            mark_node_pool,
            mark_id_ast_id_map,
        ),
        Def2::CommentsBefore { comments, def_id } => {
            let inner_def = env.pool.get(*def_id);
            let inner_def_mark_node_id = def2_to_markup(
                env,
                inner_def,
                *def_id,
                mark_node_pool,
                mark_id_ast_id_map,
                interns,
            )?;

            let full_mark_node = tld_w_comments_mark_node(
                comments.clone(),
                inner_def_mark_node_id,
                ast_node_id,
                mark_node_pool,
                mark_id_ast_id_map,
                true,
            )?;

            add_node(
                full_mark_node,
                ast_node_id,
                mark_node_pool,
                mark_id_ast_id_map,
            )
        }
        Def2::CommentsAfter { def_id, comments } => {
            let inner_def = env.pool.get(*def_id);
            let inner_def_mark_node_id = def2_to_markup(
                env,
                inner_def,
                *def_id,
                mark_node_pool,
                mark_id_ast_id_map,
                interns,
            )?;

            let full_mark_node = tld_w_comments_mark_node(
                comments.clone(),
                inner_def_mark_node_id,
                ast_node_id,
                mark_node_pool,
                mark_id_ast_id_map,
                false,
            )?;

            add_node(
                full_mark_node,
                ast_node_id,
                mark_node_pool,
                mark_id_ast_id_map,
            )
        }
    };

    Ok(mark_node_id)
}
