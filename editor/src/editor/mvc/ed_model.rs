use crate::{graphics::primitives::rect::Rect};
use crate::editor::ed_error::{EdResult};
use crate::editor::ed_error::EdError::ParseError;
use std::path::Path;
use crate::lang::expr::{Env, str_to_expr2};
use crate::lang::{
    pool::{Pool, NodeId},
    scope::Scope,
    ast::Expr2,
    expr::Output,
};
use bumpalo::Bump;
use roc_module::symbol::{IdentIds, ModuleIds};
use roc_region::all::Region;
use roc_types::subs::VarStore;
use std::collections::HashSet;


#[derive(Debug)]
pub struct EdModel<'a> {
    pub module: EdModule<'a>,
    pub glyph_dim_rect_opt: Option<Rect>,
    pub has_focus: bool,
}

pub fn init_model<'a>(_file_path: &Path, env: Env<'a>, ast_arena: &'a Bump) -> EdResult<EdModel<'a>> {
    Ok(EdModel {
        module: EdModule::new(None, env, ast_arena)?,
        glyph_dim_rect_opt: None,
        has_focus: true,
    })
}

#[derive(Debug)]
pub struct EdModule<'a> {
    pub env: Env<'a>,
    pub ast_root: Expr2,
    carets: HashSet<NodeId<Expr2>>,
}

impl<'a> EdModule<'a> {
    pub fn new(code_str_opt: Option<&'a str>, mut env: Env<'a>, ast_arena: &'a Bump) -> EdResult<EdModule<'a>> {

        let expr2_result = 
            if let Some(code_str) = code_str_opt {
                str_to_expr2(
                    &ast_arena,
                    code_str,
                    &mut env,
                )
            } else {
                let node_id = env.add(
                    Expr2::Hole,
                    Region::new(0, 0, 0, 0)
                );
                Ok((
                    Expr2::Caret{
                        offset_row: 0,
                        offset_col: 0,
                        inner: node_id,
                    },
                    Output::default()
                ))
            };

        match expr2_result {
            Ok((expr2, _output)) => 
                Ok(
                    EdModule {
                        env,
                        ast_root: expr2,
                        carets: HashSet::new(),
                    }
                ),
            Err(err) => 
                Err(
                    ParseError { syntax_err: format!("{:?}", err)}
                )
        }
    }
}
