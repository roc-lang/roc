use crate::editor::code_lines::CodeLines;
use crate::editor::grid_node_map::GridNodeMap;
use crate::editor::slow_pool::{MarkNodeId, SlowPool};
use crate::editor::syntax_highlight::HighlightStyle;
use crate::editor::{
    ed_error::EdError::ParseError,
    ed_error::{EdResult, MissingParent, NoNodeAtCaretPosition},
    markup::attribute::Attributes,
    markup::nodes::{expr2_to_markup, set_parent_for_all, MarkupNode},
};
use crate::graphics::primitives::rect::Rect;
use crate::lang::ast::Expr2;
use crate::lang::expr::{str_to_expr2, Env};
use crate::lang::pool::NodeId;
use crate::lang::pool::PoolStr;
use crate::lang::scope::Scope;
use crate::ui::text::caret_w_select::CaretWSelect;
use crate::ui::text::lines::SelectableLines;
use crate::ui::text::text_pos::TextPos;
use crate::ui::ui_error::UIResult;
use bumpalo::collections::String as BumpString;
use bumpalo::Bump;
use nonempty::NonEmpty;
use roc_module::symbol::Interns;
use roc_region::all::Region;
use std::path::Path;

#[derive(Debug)]
pub struct EdModel<'a> {
    pub module: EdModule<'a>,
    pub file_path: &'a Path,
    pub code_lines: CodeLines,
    // allows us to map window coordinates to MarkNodeId's
    pub grid_node_map: GridNodeMap,
    pub markup_root_id: MarkNodeId,
    pub markup_node_pool: SlowPool,
    // contains single char dimensions, used to calculate line height, column width...
    pub glyph_dim_rect_opt: Option<Rect>,
    pub has_focus: bool,
    pub caret_w_select_vec: NonEmpty<(CaretWSelect, Option<MarkNodeId>)>,
    pub selected_expr_opt: Option<SelectedExpression>,
    pub interns: &'a Interns, // this should eventually come from LoadedModule, see #1442
    pub show_debug_view: bool,
    // EdModel is dirty if it has changed since the previous render.
    pub dirty: bool,
}

#[derive(Debug, Copy, Clone)]
pub struct SelectedExpression {
    pub ast_node_id: NodeId<Expr2>,
    pub mark_node_id: MarkNodeId,
    pub type_str: PoolStr,
}

pub fn init_model<'a>(
    code_str: &'a BumpString,
    file_path: &'a Path,
    env: Env<'a>,
    interns: &'a Interns,
    code_arena: &'a Bump,
) -> EdResult<EdModel<'a>> {
    let mut module = EdModule::new(code_str, env, code_arena)?;

    let ast_root_id = module.ast_root_id;
    let mut markup_node_pool = SlowPool::new();

    let markup_root_id = if code_str.is_empty() {
        let blank_root = MarkupNode::Blank {
            ast_node_id: ast_root_id,
            attributes: Attributes::new(),
            syn_high_style: HighlightStyle::Blank,
            parent_id_opt: None,
        };

        markup_node_pool.add(blank_root)
    } else {
        let ast_root = &module.env.pool.get(ast_root_id);

        let temp_markup_root_id = expr2_to_markup(
            code_arena,
            &mut module.env,
            ast_root,
            ast_root_id,
            &mut markup_node_pool,
        );
        set_parent_for_all(temp_markup_root_id, &mut markup_node_pool);

        temp_markup_root_id
    };

    let code_lines = EdModel::build_code_lines_from_markup(markup_root_id, &markup_node_pool)?;
    let grid_node_map = EdModel::build_node_map_from_markup(markup_root_id, &markup_node_pool)?;

    Ok(EdModel {
        module,
        file_path,
        code_lines,
        grid_node_map,
        markup_root_id,
        markup_node_pool,
        glyph_dim_rect_opt: None,
        has_focus: true,
        caret_w_select_vec: NonEmpty::new((CaretWSelect::default(), None)),
        selected_expr_opt: None,
        interns,
        show_debug_view: false,
        dirty: true,
    })
}

impl<'a> EdModel<'a> {
    pub fn get_curr_mark_node_id(&self) -> UIResult<MarkNodeId> {
        let caret_pos = self.get_caret();
        self.grid_node_map.get_id_at_row_col(caret_pos)
    }

    pub fn get_prev_mark_node_id(&self) -> UIResult<Option<MarkNodeId>> {
        let caret_pos = self.get_caret();

        let prev_id_opt = if caret_pos.column > 0 {
            let prev_mark_node_id = self.grid_node_map.get_id_at_row_col(TextPos {
                line: caret_pos.line,
                column: caret_pos.column - 1,
            })?;

            Some(prev_mark_node_id)
        } else {
            None
        };

        Ok(prev_id_opt)
    }

    pub fn node_exists_at_caret(&self) -> bool {
        self.grid_node_map.node_exists_at_pos(self.get_caret())
    }

    // return (index of child in list of children, closest ast index of child corresponding to ast node) of MarkupNode at current caret position
    pub fn get_curr_child_indices(&self) -> EdResult<(usize, usize)> {
        if self.node_exists_at_caret() {
            let curr_mark_node_id = self.get_curr_mark_node_id()?;
            let curr_mark_node = self.markup_node_pool.get(curr_mark_node_id);

            if let Some(parent_id) = curr_mark_node.get_parent_id_opt() {
                let parent = self.markup_node_pool.get(parent_id);
                parent.get_child_indices(curr_mark_node_id, &self.markup_node_pool)
            } else {
                MissingParent {
                    node_id: curr_mark_node_id,
                }
                .fail()
            }
        } else {
            NoNodeAtCaretPosition {
                caret_pos: self.get_caret(),
            }
            .fail()
        }
    }
}

#[derive(Debug)]
pub struct EdModule<'a> {
    pub env: Env<'a>,
    pub ast_root_id: NodeId<Expr2>,
}

// for debugging
// use crate::lang::ast::expr2_to_string;

impl<'a> EdModule<'a> {
    pub fn new(code_str: &'a str, mut env: Env<'a>, ast_arena: &'a Bump) -> EdResult<EdModule<'a>> {
        if !code_str.is_empty() {
            let mut scope = Scope::new(env.home, env.pool, env.var_store);

            let region = Region::new(0, 0, 0, 0);

            let expr2_result = str_to_expr2(ast_arena, code_str, &mut env, &mut scope, region);

            match expr2_result {
                Ok((expr2, _output)) => {
                    let ast_root_id = env.pool.add(expr2);

                    // for debugging
                    // dbg!(expr2_to_string(ast_root_id, env.pool));

                    Ok(EdModule { env, ast_root_id })
                }
                Err(err) => Err(ParseError {
                    syntax_err: format!("{:?}", err),
                }),
            }
        } else {
            let ast_root_id = env.pool.add(Expr2::Blank);

            Ok(EdModule { env, ast_root_id })
        }
    }
}

#[cfg(test)]
pub mod test_ed_model {
    use crate::editor::ed_error::EdResult;
    use crate::editor::mvc::ed_model;
    use crate::lang::expr::Env;
    use crate::lang::pool::Pool;
    use crate::ui::text::caret_w_select::test_caret_w_select::convert_dsl_to_selection;
    use crate::ui::text::caret_w_select::test_caret_w_select::convert_selection_to_dsl;
    use crate::ui::text::lines::SelectableLines;
    use crate::ui::ui_error::UIResult;
    use bumpalo::collections::String as BumpString;
    use bumpalo::Bump;
    use ed_model::EdModel;
    use roc_module::symbol::{IdentIds, Interns, ModuleIds};
    use roc_types::subs::VarStore;
    use std::path::Path;

    pub fn init_dummy_model<'a>(
        code_str: &'a BumpString,
        ed_model_refs: &'a mut EdModelRefs,
    ) -> EdResult<EdModel<'a>> {
        let file_path = Path::new("");

        let dep_idents = IdentIds::exposed_builtins(8);
        let exposed_ident_ids = IdentIds::default();
        let mod_id = ed_model_refs
            .interns
            .module_ids
            .get_or_insert(&"ModId123".into());

        let env = Env::new(
            mod_id,
            &ed_model_refs.env_arena,
            &mut ed_model_refs.env_pool,
            &mut ed_model_refs.var_store,
            dep_idents,
            &ed_model_refs.interns.module_ids,
            exposed_ident_ids,
        );

        ed_model::init_model(
            code_str,
            file_path,
            env,
            &ed_model_refs.interns,
            &ed_model_refs.code_arena,
        )
    }

    pub struct EdModelRefs {
        code_arena: Bump,
        env_arena: Bump,
        env_pool: Pool,
        var_store: VarStore,
        interns: Interns,
    }

    pub fn init_model_refs() -> EdModelRefs {
        EdModelRefs {
            code_arena: Bump::new(),
            env_arena: Bump::new(),
            env_pool: Pool::with_capacity(1024),
            var_store: VarStore::default(),
            interns: Interns {
                module_ids: ModuleIds::default(),
                all_ident_ids: IdentIds::exposed_builtins(8),
            },
        }
    }

    pub fn ed_model_from_dsl<'a>(
        clean_code_str: &'a BumpString,
        code_lines: &[&str],
        ed_model_refs: &'a mut EdModelRefs,
    ) -> Result<EdModel<'a>, String> {
        let code_lines_vec: Vec<String> = (*code_lines).iter().map(|s| s.to_string()).collect();
        let caret_w_select = convert_dsl_to_selection(&code_lines_vec)?;

        let mut ed_model = init_dummy_model(clean_code_str, ed_model_refs)?;

        ed_model.set_caret(caret_w_select.caret_pos);

        Ok(ed_model)
    }

    pub fn ed_model_to_dsl(ed_model: &EdModel) -> UIResult<Vec<String>> {
        let caret_w_select = ed_model.caret_w_select_vec.first().0;
        let code_lines = ed_model.code_lines.lines.clone();

        convert_selection_to_dsl(caret_w_select, code_lines)
    }
}
