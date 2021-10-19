use crate::{
    markup::{common_nodes::new_blank_mn_w_nls, top_level_def::tld_mark_node},
    slow_pool::{MarkNodeId, SlowPool},
};

use super::from_expr2::expr2_to_markup;

use roc_ast::{
    ast_error::ASTResult,
    lang::{
        core::{
            ast::ASTNodeId,
            def::def2::{Def2, DefId},
        },
        env::Env,
    },
};
use roc_module::symbol::Interns;

pub fn def2_to_markup<'a>(
    env: &mut Env<'a>,
    def2: &Def2,
    def2_node_id: DefId,
    mark_node_pool: &mut SlowPool,
    interns: &Interns,
) -> ASTResult<MarkNodeId> {
    let ast_node_id = ASTNodeId::ADefId(def2_node_id);

    let mark_node_id = match def2 {
        Def2::ValueDef {
            identifier_id,
            expr_id,
        } => {
            let expr_mn_id = expr2_to_markup(
                env,
                env.pool.get(*expr_id),
                *expr_id,
                mark_node_pool,
                interns,
                0,
            )?;

            let tld_mn =
                tld_mark_node(*identifier_id, expr_mn_id, ast_node_id, mark_node_pool, env)?;

            mark_node_pool.add(tld_mn)
        }
        Def2::Blank => mark_node_pool.add(new_blank_mn_w_nls(ast_node_id, None, 2)),
    };

    Ok(mark_node_id)
}
