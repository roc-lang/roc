use crate::editor::ed_error::EdError::ParseError;
use crate::editor::ed_error::EdResult;
use crate::editor::markup::{expr2_to_markup, set_caret_at_start, Attribute, MarkupNode};
use crate::editor::syntax_highlight::HighlightStyle;
use crate::graphics::primitives::rect::Rect;
use crate::lang::ast::Expr2;
use crate::lang::expr::{str_to_expr2, Env};
use crate::lang::scope::Scope;
use bumpalo::collections::String as BumpString;
use bumpalo::Bump;
use roc_region::all::Region;

#[derive(Debug)]
pub struct EdModel<'a> {
    pub module: EdModule<'a>,
    pub code_as_str: &'a str,
    pub markup_root: MarkupNode,
    pub glyph_dim_rect_opt: Option<Rect>,
    pub has_focus: bool,
    carets: Vec<&'a MarkupNode>,
}

pub fn init_model<'a>(
    code_str: &'a BumpString,
    env: Env<'a>,
    code_arena: &'a Bump,
) -> EdResult<EdModel<'a>> {
    let mut module = EdModule::new(&code_str, env, code_arena)?;
    // TODO fix moving issue and insert module.ast_root into pool
    let ast_root_id = module.env.pool.add(Expr2::Hole);

    let markup_root = if code_str.is_empty() {
        MarkupNode::Hole {
            ast_node_id: ast_root_id,
            attributes: vec![Attribute::Caret { offset_col: 0 }],
            syn_high_style: HighlightStyle::Hole,
        }
    } else {
        let mut temp_markup_root = expr2_to_markup(code_arena, &mut module.env, &module.ast_root);
        set_caret_at_start(&mut temp_markup_root);
        temp_markup_root
    };

    Ok(EdModel {
        module,
        code_as_str: code_str,
        markup_root,
        glyph_dim_rect_opt: None,
        has_focus: true,
        carets: Vec::new(),
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
                ast_root: Expr2::Hole,
            })
        }
    }
}
