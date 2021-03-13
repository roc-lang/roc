use crate::{graphics::primitives::rect::Rect};
use crate::editor::ed_error::{EdResult};
use crate::editor::ed_error::EdError::ParseError;
use std::path::Path;
use crate::lang::expr::{Env, str_to_expr2};
use crate::lang::{
    ast::Expr2,
};
use bumpalo::Bump;
use crate::editor::markup::MarkupNode;


#[derive(Debug)]
pub struct EdModel<'a> {
    pub module: EdModule<'a>,
    pub markup_root: MarkupNode,
    pub glyph_dim_rect_opt: Option<Rect>,
    pub has_focus: bool,
    carets: Vec<&'a MarkupNode>,
}

pub fn init_model<'a>(_file_path: &Path, env: Env<'a>, ast_arena: &'a Bump) -> EdResult<EdModel<'a>> {
    let module = EdModule::new(None, env, ast_arena)?;
    // TODO fix moving issue and insert module.ast_root into pool
    let ast_root_id = module.env.pool.add(Expr2::Hole);
    let markup_root = 
        MarkupNode::Hole{
            ast_node_id: ast_root_id,
            attributes: Vec::new(),
        };

    Ok(EdModel {
        module,
        markup_root,
        glyph_dim_rect_opt: None,
        has_focus: true,
        carets: Vec::new()
    })
}

#[derive(Debug)]
pub struct EdModule<'a> {
    pub env: Env<'a>,
    pub ast_root: Expr2,
}

impl<'a> EdModule<'a> {
    pub fn new(code_str_opt: Option<&'a str>, mut env: Env<'a>, ast_arena: &'a Bump) -> EdResult<EdModule<'a>> {

        if let Some(code_str) = code_str_opt {
            let expr2_result = str_to_expr2(
                &ast_arena,
                code_str,
                &mut env,
            );

            match expr2_result {
                Ok((expr2, _output)) => 
                    Ok(
                        EdModule {
                            env,
                            ast_root: expr2,
                        }
                    ),
                Err(err) => 
                    Err(
                        ParseError { syntax_err: format!("{:?}", err)}
                    )
            }
        } else {
            Ok(
                EdModule {
                    env,
                    ast_root: Expr2::Hole,
                }
            )
        }
    }
}
