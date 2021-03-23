use crate::editor::code_lines::CodeLines;
use crate::editor::slow_pool::{MarkNodeId, SlowPool};
use crate::editor::syntax_highlight::HighlightStyle;
use crate::editor::{
    ed_error::EdError::ParseError,
    ed_error::EdResult,
    markup::attribute::{Attributes, Caret},
    markup::nodes::{expr2_to_markup, set_parent_for_all, MarkupNode},
};
use crate::graphics::primitives::rect::Rect;
use crate::lang::ast::Expr2;
use crate::lang::expr::{str_to_expr2, Env};
use crate::lang::scope::Scope;
use crate::ui::text::caret_w_select::CaretWSelect;
use bumpalo::collections::String as BumpString;
use bumpalo::Bump;
use nonempty::NonEmpty;
use roc_region::all::Region;
use std::path::Path;

#[derive(Debug)]
pub struct EdModel<'a> {
    pub module: EdModule<'a>,
    pub file_path: &'a Path,
    pub code_lines: CodeLines,
    pub markup_root_id: MarkNodeId,
    pub markup_node_pool: SlowPool,
    pub glyph_dim_rect_opt: Option<Rect>,
    pub has_focus: bool,
    // Option<MarkNodeId>: MarkupNode that corresponds to caret position, Option because this MarkNodeId is only calculated when it needs to be used.
    pub caret_w_select_vec: NonEmpty<(CaretWSelect, Option<MarkNodeId>)>,
}

pub fn init_model<'a>(
    code_str: &'a BumpString,
    file_path: &'a Path,
    env: Env<'a>,
    code_arena: &'a Bump,
) -> EdResult<EdModel<'a>> {
    let mut module = EdModule::new(&code_str, env, code_arena)?;
    // TODO fix moving issue and insert module.ast_root into pool
    let ast_root_id = module.env.pool.add(Expr2::Blank);
    let mut markup_node_pool = SlowPool::new();

    let markup_root_id = if code_str.is_empty() {
        let blank_root = MarkupNode::Blank {
            ast_node_id: ast_root_id,
            attributes: Attributes {
                all: vec![Caret::new_attr(0)],
            },
            syn_high_style: HighlightStyle::Blank,
            parent_id_opt: None,
        };

        markup_node_pool.add(blank_root)
    } else {
        let temp_markup_root_id = expr2_to_markup(
            code_arena,
            &mut module.env,
            &module.ast_root,
            &mut markup_node_pool,
        );
        set_parent_for_all(temp_markup_root_id, &mut markup_node_pool);

        temp_markup_root_id
    };

    Ok(EdModel {
        module,
        file_path,
        code_lines: CodeLines::from_str(code_str),
        markup_root_id,
        markup_node_pool,
        glyph_dim_rect_opt: None,
        has_focus: true,
        caret_w_select_vec: NonEmpty::new((CaretWSelect::default(), None)),
    })
}

#[derive(Debug)]
pub struct EdModule<'a> {
    pub env: Env<'a>,
    pub ast_root: Expr2,
}

impl<'a> EdModule<'a> {
    pub fn new(code_str: &'a str, mut env: Env<'a>, ast_arena: &'a Bump) -> EdResult<EdModule<'a>> {
        if !code_str.is_empty() {
            let mut scope = Scope::new(env.home, env.pool, env.var_store);

            let region = Region::new(0, 0, 0, 0);

            let expr2_result = str_to_expr2(&ast_arena, &code_str, &mut env, &mut scope, region);

            match expr2_result {
                Ok((expr2, _output)) => Ok(EdModule {
                    env,
                    ast_root: expr2,
                }),
                Err(err) => Err(ParseError {
                    syntax_err: format!("{:?}", err),
                }),
            }
        } else {
            Ok(EdModule {
                env,
                ast_root: Expr2::Blank,
            })
        }
    }
}
