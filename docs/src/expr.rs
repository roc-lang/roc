use crate::html::mark_node_to_html;
use bumpalo::{collections::String as BumpString, Bump};
use roc_ast::{
    ast_error::ASTResult,
    lang::{self, core::expr::expr_to_expr2::expr_to_expr2},
    mem_pool::pool::Pool,
};
use roc_code_markup::{markup::convert::from_expr2::expr2_to_markup, slow_pool::SlowPool};
use roc_module::symbol::{IdentIds, Interns, ModuleId, ModuleIds};
use roc_parse::ast::Expr;
use roc_region::all::Region;
use roc_types::subs::VarStore;

// html is written to buf
pub fn expr_to_html<'a>(
    buf: &mut BumpString<'a>,
    expr: Expr<'a>,
    env_module_id: ModuleId,
    env_module_ids: &'a ModuleIds,
    interns: &Interns,
) {
    let mut env_pool = Pool::with_capacity(1024);
    let env_arena = Bump::new();

    let mut var_store = VarStore::default();
    let dep_idents = IdentIds::exposed_builtins(8);
    let exposed_ident_ids = IdentIds::default();

    let mut env = lang::env::Env::new(
        env_module_id,
        &env_arena,
        &mut env_pool,
        &mut var_store,
        dep_idents,
        env_module_ids,
        exposed_ident_ids,
    );

    let mut scope = lang::scope::Scope::new(env.home, env.pool, env.var_store);
    let region = Region::new(0, 0, 0, 0);

    // TODO remove unwrap
    write_expr_to_bump_str_html(&mut env, &mut scope, region, &expr, interns, buf).unwrap();
}

fn write_expr_to_bump_str_html<'a, 'b>(
    env: &mut lang::env::Env<'a>,
    scope: &mut lang::scope::Scope,
    region: Region,
    expr: &'a Expr,
    interns: &Interns,
    buf: &mut BumpString<'b>,
) -> ASTResult<()> {
    let (expr2, _) = expr_to_expr2(env, scope, expr, region);

    let expr2_id = env.pool.add(expr2);

    let mut mark_node_pool = SlowPool::default();

    let expr2_markup_id = expr2_to_markup(
        env,
        env.pool.get(expr2_id),
        expr2_id,
        &mut mark_node_pool,
        interns,
        0,
    )?;

    let expr2_markup_node = mark_node_pool.get(expr2_markup_id);

    mark_node_to_html(expr2_markup_node, &mark_node_pool, buf);

    Ok(())
}
