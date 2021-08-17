use bumpalo::Bump;
use roc_parse::parser::SyntaxError;
use crate::lang::scope::Scope;
use roc_region::all::Region;

use super::{ast::ExprId, expr::{Env, str_to_expr2_w_defs}};



// <temporary WIP zone>
#[derive(Debug)]
pub struct AST {
    pub header: String,
    pub expression_ids: Vec<ExprId>,
}

impl AST {
    pub fn parse_from_string<'a>(code_str: &'a str, env: &mut Env<'a>, ast_arena: &'a Bump) -> Result<AST, SyntaxError<'a>> {

        let blank_line_indx = code_str.find("\n\n").expect("I was expecting a double newline to split header and rest of code.");
        
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
        

        Ok(
            AST { 
                header: header_str.to_string(),
                expression_ids,
            }
        )
         
    }
}
// </temporary WIP zone>