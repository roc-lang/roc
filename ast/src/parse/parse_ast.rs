use bumpalo::Bump;
use roc_module::symbol::Interns;
use roc_region::all::Region;

use crate::{
    ast_error::ASTResult,
    lang::{
        core::{
            ast::AST,
            def::{def2::DefId, def_to_def2::str_to_def2},
            expr::expr2::Expr2,
        },
        env::Env,
        scope::Scope,
    },
};

use super::parse_header;

pub fn parse_from_string<'a>(
    code_str: &'a str,
    env: &mut Env<'a>,
    ast_arena: &'a Bump,
    interns: &mut Interns,
) -> ASTResult<AST> {
    let blank_line_indx = code_str
        .find("\n\n")
        .expect("I was expecting a double newline to split header and rest of code.");

    let header_str = &code_str[0..blank_line_indx];
    let tail_str = &code_str[blank_line_indx..];

    let mut scope = Scope::new(env.home, env.pool, env.var_store);
    scope.fill_scope(env, &mut interns.all_ident_ids)?;

    let region = Region::new(0, 0, 0, 0);

    let mut def_ids = Vec::<DefId>::new();

    let def2_vec = str_to_def2(ast_arena, tail_str, env, &mut scope, region)?;

    for def2 in def2_vec {
        let def_id = env.pool.add(def2);

        def_ids.push(def_id);
    }

    let ast_node_id = env.pool.add(Expr2::Blank);

    Ok(AST {
        header: parse_header::parse_from_string(header_str, ast_node_id),
        def_ids,
    })
}
