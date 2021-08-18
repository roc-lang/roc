use crate::lang::scope::Scope;
use bumpalo::Bump;
use roc_parse::parser::SyntaxError;
use roc_region::all::Region;

use super::{
    ast::{Expr2, ExprId},
    expr::{str_to_expr2_w_defs, Env},
};

// WORK IN PROGRESS FILE

#[derive(Debug)]
pub struct AST {
    pub header: AppHeader,
    pub expression_ids: Vec<ExprId>,
}

#[derive(Debug)]
pub struct AppHeader {
    pub app_name: String,
    pub packages_base: String,
    pub imports: Vec<String>,
    pub provides: Vec<String>,
    pub ast_node_id: ExprId,
}

impl AST {
    pub fn parse_from_string<'a>(
        code_str: &'a str,
        env: &mut Env<'a>,
        ast_arena: &'a Bump,
    ) -> Result<AST, SyntaxError<'a>> {
        let blank_line_indx = code_str
            .find("\n\n")
            .expect("I was expecting a double newline to split header and rest of code.");

        let header_str = &code_str[0..blank_line_indx];
        let tail_str = &code_str[blank_line_indx..];

        let mut scope = Scope::new(env.home, env.pool, env.var_store);
        let region = Region::new(0, 0, 0, 0);

        let mut expression_ids = Vec::<ExprId>::new();

        let expr2_vec = str_to_expr2_w_defs(&ast_arena, tail_str, env, &mut scope, region)?;

        for expr2 in expr2_vec {
            let expr_id = env.pool.add(expr2);

            expression_ids.push(expr_id);
        }

        let ast_node_id = env.pool.add(Expr2::Blank);

        Ok(AST {
            header: AppHeader::parse_from_string(header_str, ast_node_id),
            expression_ids,
        })
    }
}

impl AppHeader {
    // TODO don't use mock struct and actually parse string
    pub fn parse_from_string(_header_str: &str, ast_node_id: ExprId) -> Self {
        AppHeader {
            app_name: "\"untitled_app\"".to_owned(),
            packages_base: "\"platform\"".to_owned(),
            imports: vec![],
            provides: vec!["main".to_owned()],
            ast_node_id,
        }
    }
}
