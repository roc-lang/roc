use crate::editor::code_lines::CodeLines;
use crate::editor::grid_node_map::GridNodeMap;
use crate::editor::markup::nodes::ast_to_mark_nodes;
use crate::editor::slow_pool::{MarkNodeId, SlowPool};
use crate::editor::{
    ed_error::SrcParseError,
    ed_error::{EdResult, MissingParent, NoNodeAtCaretPosition, EmptyCodeString},
};
use crate::graphics::primitives::rect::Rect;
use crate::lang::ast::{Expr2};
use crate::lang::expr::{Env};
use crate::lang::parse::AST;
use crate::lang::pool::{NodeId};
use crate::lang::pool::PoolStr;
use crate::ui::text::caret_w_select::CaretWSelect;
use crate::ui::text::lines::SelectableLines;
use crate::ui::text::text_pos::TextPos;
use crate::ui::ui_error::UIResult;
use bumpalo::Bump;
use nonempty::NonEmpty;
use roc_load::file::LoadedModule;
use std::path::Path;

#[derive(Debug)]
pub struct EdModel<'a> {
    pub module: EdModule<'a>,
    pub file_path: &'a Path,
    pub code_lines: CodeLines,
    // allows us to map window coordinates to MarkNodeId's
    pub grid_node_map: GridNodeMap,
    pub markup_ids: Vec<MarkNodeId>, // one root node for every expression
    pub markup_node_pool: SlowPool,
    // contains single char dimensions, used to calculate line height, column width...
    pub glyph_dim_rect_opt: Option<Rect>,
    pub has_focus: bool,
    pub caret_w_select_vec: NonEmpty<(CaretWSelect, Option<MarkNodeId>)>,
    pub selected_expr_opt: Option<SelectedExpression>,
    pub loaded_module: LoadedModule,
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
    code_str: &'a str,
    file_path: &'a Path,
    env: Env<'a>,
    loaded_module: LoadedModule,
    code_arena: &'a Bump,
) -> EdResult<EdModel<'a>> {

    let mut module = EdModule::new(&code_str, env, &code_arena)?;

    let mut markup_node_pool = SlowPool::new();

    let markup_ids = 
        if code_str.is_empty() {

            EmptyCodeString{}.fail()

        } else {

            ast_to_mark_nodes(
                &code_arena,
                &mut module.env,
                &module.ast,
                &mut markup_node_pool,
                &loaded_module.interns
            )

        }?;

    let code_lines = EdModel::build_code_lines_from_markup(&markup_ids, &markup_node_pool)?;
    let grid_node_map = EdModel::build_node_map_from_markup(&markup_ids, &markup_node_pool)?;

    Ok(EdModel {
        module,
        file_path,
        code_lines,
        grid_node_map,
        markup_ids,
        markup_node_pool,
        glyph_dim_rect_opt: None,
        has_focus: true,
        caret_w_select_vec: NonEmpty::new((CaretWSelect::default(), None)),
        selected_expr_opt: None,
        loaded_module,
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
    pub ast: AST,
}

// for debugging
//use crate::lang::ast::expr2_to_string;

impl<'a> EdModule<'a> {
    pub fn new(code_str: &'a str, mut env: Env<'a>, ast_arena: &'a Bump) -> EdResult<EdModule<'a>> {
        if !code_str.is_empty() {
            
            let parse_res = AST::parse_from_string(code_str, &mut env, ast_arena);

            match parse_res {
                Ok(ast) => {
                    Ok(
                        EdModule { 
                            env,
                            ast,
                        }
                    )
                },
                Err(err) => SrcParseError {
                    syntax_err: format!("{:?}", err),
                }.fail()
            }

        } else {
            EmptyCodeString{}.fail()
        }
    }
}

#[cfg(test)]
pub mod test_ed_model {
    use crate::editor::ed_error::EdResult;
    use crate::editor::main::load_module;
    use crate::editor::mvc::ed_model;
    use crate::lang::expr::Env;
    use crate::lang::pool::Pool;
    use crate::ui::text::caret_w_select::test_caret_w_select::convert_dsl_to_selection;
    use crate::ui::text::caret_w_select::test_caret_w_select::convert_selection_to_dsl;
    use crate::ui::text::lines::SelectableLines;
    use crate::ui::ui_error::UIResult;
    use bumpalo::Bump;
    use ed_model::EdModel;
    use roc_load::file::LoadedModule;
    use roc_module::symbol::IdentIds;
    use roc_module::symbol::ModuleIds;
    use roc_types::subs::VarStore;
    use tempfile::tempdir;
    use uuid::Uuid;
    use std::fs::File;
    use std::path::Path;
    use std::path::PathBuf;
    use std::io::Write;

    pub fn init_dummy_model<'a>(
        code_str: &'a str,
        loaded_module: LoadedModule,
        module_ids: &'a ModuleIds,
        ed_model_refs: &'a mut EdModelRefs,
        code_arena: &'a Bump,
    ) -> EdResult<EdModel<'a>> {
        let file_path = Path::new("");

        let dep_idents = IdentIds::exposed_builtins(8);
        let exposed_ident_ids = IdentIds::default();

        let env = Env::new(
            loaded_module.module_id,
            &ed_model_refs.env_arena,
            &mut ed_model_refs.env_pool,
            &mut ed_model_refs.var_store,
            dep_idents,
            module_ids,
            exposed_ident_ids,
        );

        ed_model::init_model(
            code_str,
            file_path,
            env,
            loaded_module,
            code_arena,
        )
    }

    pub struct EdModelRefs {
        env_arena: Bump,
        env_pool: Pool,
        var_store: VarStore,
    }

    pub fn init_model_refs() -> EdModelRefs {
        EdModelRefs {
            env_arena: Bump::new(),
            env_pool: Pool::with_capacity(1024),
            var_store: VarStore::default(),
        }
    }

    pub fn ed_model_from_dsl<'a>(
        clean_code_str: &'a mut String,
        code_lines: &[&str],
        ed_model_refs: &'a mut EdModelRefs,
        module_ids: &'a ModuleIds,
        code_arena: &'a Bump,
    ) -> Result<EdModel<'a>, String> {
        let code_lines_vec: Vec<String> = (*code_lines).iter().map(|s| s.to_string()).collect();
        let caret_w_select = convert_dsl_to_selection(&code_lines_vec)?;

        let header_str = r#"
app "test-app"
packages { base: "platform" }
imports []
provides [ main ] to base

main = "Hello, world!"
"#;

        *clean_code_str = [header_str, clean_code_str.as_str()].join("");

        let temp_dir = tempdir().expect("Failed to create temporary directory for test.");
        let temp_file_path_buf = PathBuf::from(
                                    [Uuid::new_v4().to_string(), ".roc".to_string()].join("")
                                );
        let temp_file_full_path = temp_dir.path().join(temp_file_path_buf);
        
        let mut file = File::create(temp_file_full_path.clone())
                                .expect(
                                    &format!("Failed to create temporary file for path {:?}", temp_file_full_path)
                                );
        writeln!(file, "{}", clean_code_str)
            .expect(
                &format!("Failed to write {:?} to file: {:?}", clean_code_str, file)
            );

        let loaded_module = load_module(&temp_file_full_path);

        let mut ed_model =
            init_dummy_model(
                clean_code_str,
                loaded_module,
                module_ids,
                ed_model_refs,
                code_arena,
            )?;

        ed_model.set_caret(caret_w_select.caret_pos);

        Ok(ed_model)
    }

    pub fn ed_model_to_dsl(ed_model: &EdModel) -> UIResult<Vec<String>> {
        let caret_w_select = ed_model.caret_w_select_vec.first().0;
        let code_lines = ed_model.code_lines.lines.clone();

        convert_selection_to_dsl(caret_w_select, code_lines)
    }
}
