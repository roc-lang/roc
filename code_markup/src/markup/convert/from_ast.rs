use roc_ast::{
    ast_error::ASTResult,
    lang::{core::ast::AST, env::Env},
};
use roc_module::symbol::Interns;

use crate::{
    markup::{
        convert::{from_def2::def2_to_markup, from_header::header_to_markup},
        nodes::set_parent_for_all,
    },
    slow_pool::{MarkNodeId, SlowPool},
};

pub fn ast_to_mark_nodes<'a>(
    env: &mut Env<'a>,
    ast: &AST,
    mark_node_pool: &mut SlowPool,
    interns: &Interns,
) -> ASTResult<Vec<MarkNodeId>> {
    let mut all_mark_node_ids = vec![header_to_markup(&ast.header, mark_node_pool)];

    for &def_id in ast.def_ids.iter() {
        let def2 = env.pool.get(def_id);

        let expr2_markup_id = def2_to_markup(env, def2, def_id, mark_node_pool, interns)?;

        set_parent_for_all(expr2_markup_id, mark_node_pool);

        all_mark_node_ids.push(expr2_markup_id);
    }

    Ok(all_mark_node_ids)
}
