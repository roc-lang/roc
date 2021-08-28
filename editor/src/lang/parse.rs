use std::fmt::Debug;

use crate::{editor::ed_error::EdResult, editor::ed_error::ASTNodeIdWithoutExprId,lang::scope::Scope};
use bumpalo::Bump;
use roc_parse::parser::SyntaxError;
use roc_region::all::Region;

use super::{ast::{DefId, Expr2, ExprId}, expr::{str_to_def2, Env}, pool::Pool};

// WORK IN PROGRESS FILE

#[derive(Debug)]
pub struct AST {
    pub header: AppHeader,
    pub def_ids: Vec<DefId>,
}

#[derive(Debug)]
pub struct ASTNodeId {
    pub def_id_opt: Option<DefId>,
    pub expr_id_opt: Option<ExprId>,
}

impl ASTNodeId {
    pub fn to_expr_id(&self) -> EdResult<ExprId>{
        if let Some(expr_id) = self.expr_id_opt {
            Ok(expr_id)
        } else {
            ASTNodeIdWithoutExprId {
                ast_node_id: self
            }.fail()
        }
    }

    pub fn equals(&self, other: &ASTNodeId) -> bool {
        if let Some(def_id_self) = self.def_id_opt {
            if let Some(def_id_other) = other.def_id_opt {
                def_id_self == def_id_other
            } else {
                false
            }
        } else if let Some(expr_id_self) = self.expr_id_opt {
            if let Some(expr_id_other) = other.expr_id_opt {
                expr_id_self == expr_id_other
            } else {
                false
            }
        } else {
            unreachable!()
        }
    }
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

        let mut def_ids = Vec::<DefId>::new();

        let def2_vec = str_to_def2(ast_arena, tail_str, env, &mut scope, region)?;

        for def2 in def2_vec {
            let def_id = env.pool.add(def2);

            def_ids.push(def_id);
        }

        let ast_node_id = env.pool.add(Expr2::Blank);

        Ok(AST {
            header: AppHeader::parse_from_string(header_str, ast_node_id),
            def_ids,
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
