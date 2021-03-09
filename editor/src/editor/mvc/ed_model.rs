use crate::{graphics::primitives::rect::Rect};
use crate::editor::ed_error::{EdResult};
use crate::editor::ed_error::EdError::ParseError;
use std::path::Path;
use crate::lang::expr::{Env, str_to_expr2};
use crate::lang::{
    pool::{Pool, NodeId},
    scope::Scope,
    ast::Expr2,
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

pub fn init_model<'a>(_file_path: &Path, env: &'a mut Env<'a>, ast_arena: &'a Bump) -> EdResult<EdModel<'a>> {
    Ok(EdModel {
        module: EdModule::new(env, ast_arena)?,
        glyph_dim_rect_opt: None,
        has_focus: true,
    })
}

#[derive(Debug)]
pub struct EdModule<'a> {
    env: &'a mut Env<'a>,
    ast_root: Expr2,
    carets: HashSet<NodeId<Expr2>>,
}

impl<'a> EdModule<'a> {
    pub fn new(env: &'a mut Env<'a>, ast_arena: &'a Bump) -> EdResult<EdModule<'a>> {

        let expr2_result = str_to_expr2(
            &ast_arena,
            "{ population: 5437, coords: {x: 3.637, y: 4}, style: \"Functional\" }",
            &mut env,
        );

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
