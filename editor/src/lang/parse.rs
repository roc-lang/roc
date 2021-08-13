use bumpalo::Bump;
use roc_parse::parser::SyntaxError;
use crate::lang::scope::Scope;
use roc_region::all::Region;

use super::{ast::ExprId, expr::{Env, str_to_expr2}};



// <temporary WIP zone>
#[derive(Debug)]
pub struct AST {
    pub header: String,
    pub expressions: Vec<ExprId>,
}

impl AST {
    pub fn parse_from_string<'a>(code_str: &str, mut env: Env<'a>, ast_arena: &Bump) -> Result<AST, SyntaxError<'a>> {

        let mut split_string = code_str.split("\n\n");

        let split_code_vec: Vec<&str> = split_string.collect();

        if let Some((head, tail)) = split_code_vec.split_first() {

            let mut scope = Scope::new(env.home, env.pool, env.var_store);

            let region = Region::new(0, 0, 0, 0);

            let expressions =
                tail
                    .iter()
                    .map(|&expr_str|
                        {
                            let (expr2, _output) = str_to_expr2(&ast_arena, code_str, &mut env, &mut scope, region)?;

                            let expr_id = env.pool.add(expr2);

                            expr_id
                        }
                    ).collect::<Vec<_>>();
            

            Ok(
                AST { 
                    header: head.to_string(),
                    expressions,
                }
            )
        } else {
            panic!("I was expecting a double newline to split header and rest of code.")
        }
    }
}
// </temporary WIP zone>