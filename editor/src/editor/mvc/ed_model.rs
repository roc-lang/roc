use crate::editor::slow_pool::{SlowNodeId, SlowPool};
use crate::editor::syntax_highlight::HighlightStyle;
use crate::editor::{
    ed_error::EdError::ParseError,
    ed_error::EdResult,
    markup::attribute::{Attributes, Caret},
    markup::caret::{move_carets_right_for_node, set_caret_at_start},
    markup::nodes::{expr2_to_markup, set_parent_for_all, MarkupNode},
};
use crate::graphics::primitives::rect::Rect;
use crate::lang::ast::Expr2;
use crate::lang::expr::{str_to_expr2, Env};
use crate::lang::scope::Scope;
use crate::window::keyboard_input::Modifiers;
use bumpalo::collections::String as BumpString;
use bumpalo::Bump;
use roc_region::all::Region;
use std::collections::HashSet;
use winit::event::VirtualKeyCode;

pub type LeafIndex = usize;

#[derive(Debug)]
pub struct EdModel<'a> {
    pub module: EdModule<'a>,
    pub code_as_str: &'a str,
    pub markup_root_id: SlowNodeId,
    pub glyph_dim_rect_opt: Option<Rect>,
    pub has_focus: bool,
    // This HashSet may have less elements than there are carets. There can be multiple carets for a single node.
    caret_nodes: HashSet<(SlowNodeId, LeafIndex)>,
    dfs_ordered_leaves: Vec<SlowNodeId>,
}

pub fn init_model<'a>(
    code_str: &'a BumpString,
    env: Env<'a>,
    code_arena: &'a Bump,
    markup_node_pool: &mut SlowPool,
) -> EdResult<EdModel<'a>> {
    let mut module = EdModule::new(&code_str, env, code_arena)?;
    // TODO fix moving issue and insert module.ast_root into pool
    let ast_root_id = module.env.pool.add(Expr2::Blank);

    let (markup_root_id, node_with_caret_id) = if code_str.is_empty() {
        let blank_root = MarkupNode::Blank {
            ast_node_id: ast_root_id,
            attributes: Attributes {
                all: vec![Caret::new_attr(0)],
            },
            syn_high_style: HighlightStyle::Blank,
            parent_id_opt: None,
        };

        let root_id = markup_node_pool.add(blank_root);

        (root_id, root_id)
    } else {
        let temp_markup_root_id = expr2_to_markup(
            code_arena,
            &mut module.env,
            &module.ast_root,
            markup_node_pool,
        );
        set_parent_for_all(temp_markup_root_id, markup_node_pool);
        let node_w_caret_id = set_caret_at_start(temp_markup_root_id, markup_node_pool);

        (temp_markup_root_id, node_w_caret_id)
    };

    let mut dfs_ordered_leaves = Vec::new();
    markup_node_pool.get(markup_root_id).get_dfs_leaves(
        markup_root_id,
        markup_node_pool,
        &mut dfs_ordered_leaves,
    );

    Ok(EdModel {
        module,
        code_as_str: code_str,
        markup_root_id,
        glyph_dim_rect_opt: None,
        has_focus: true,
        caret_nodes: vec![(node_with_caret_id, 0)].into_iter().collect(),
        dfs_ordered_leaves,
    })
}

impl<'a> EdModel<'a> {
    pub fn handle_key_down(
        &mut self,
        _modifiers: &Modifiers,
        virtual_keycode: VirtualKeyCode,
        markup_node_pool: &mut SlowPool,
    ) -> EdResult<()> {
        match virtual_keycode {
            VirtualKeyCode::Right => {
                let mut new_caret_nodes: Vec<(SlowNodeId, LeafIndex)> = Vec::new();

                for (caret_node_id_ref, leaf_index) in self.caret_nodes.iter() {
                    let caret_node_id = *caret_node_id_ref;
                    let next_leaf_id_opt = self.get_next_leaf(*leaf_index);

                    new_caret_nodes.extend(move_carets_right_for_node(
                        caret_node_id,
                        *leaf_index,
                        next_leaf_id_opt,
                        markup_node_pool,
                    )?);
                }

                self.caret_nodes = new_caret_nodes.into_iter().collect();
            }
            VirtualKeyCode::Left => unimplemented!("TODO"),
            _ => (),
        };

        Ok(())
    }

    pub fn get_next_leaf(&self, index: usize) -> Option<SlowNodeId> {
        self.dfs_ordered_leaves.get(index + 1).copied()
    }
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
