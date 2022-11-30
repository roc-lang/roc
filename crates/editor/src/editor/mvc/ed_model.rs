use crate::editor::code_lines::CodeLines;
use crate::editor::grid_node_map::GridNodeMap;
use crate::editor::{
    ed_error::SrcParseSnafu,
    ed_error::{EdResult, EmptyCodeStringSnafu, MissingParentSnafu, NoNodeAtCaretPositionSnafu},
};
use crate::graphics::primitives::rect::Rect;
use crate::ui::text::caret_w_select::{CaretPos, CaretWSelect};
use crate::ui::text::lines::SelectableLines;
use crate::ui::text::text_pos::TextPos;
use crate::ui::ui_error::UIResult;
use bumpalo::Bump;
use nonempty::NonEmpty;
use roc_ast::lang::core::ast::{ASTNodeId, AST};
use roc_ast::lang::env::Env;
use roc_ast::mem_pool::pool_str::PoolStr;
use roc_ast::parse::parse_ast;
use roc_code_markup::markup::convert::from_ast::ast_to_mark_nodes;
use roc_code_markup::markup::mark_id_ast_id_map::MarkIdAstIdMap;
use roc_code_markup::markup::nodes;
use roc_code_markup::slow_pool::{MarkNodeId, SlowPool};
use roc_load::LoadedModule;
use roc_module::symbol::Interns;
use std::path::Path;

/// Contains nearly all state related to a single roc file in the editor.
#[derive(Debug)]
pub struct EdModel<'a> {
    pub module: EdModule<'a>, // contains Abstract Syntax Tree of code
    pub file_path: &'a Path,
    pub code_lines: CodeLines, // Vec<String> of all code, this Vec is written to disk when saving a file.
    pub grid_node_map: GridNodeMap, // allows us to map window coordinates to MarkNodeId's
    pub markup_ids: Vec<MarkNodeId>, // one root node for every top level definition
    pub mark_node_pool: SlowPool, // all MarkupNodes for this file are saved into this pool and can be retrieved using their MarkNodeId
    pub mark_id_ast_id_map: MarkIdAstIdMap, // To find the ASTNode that is represented by a MarkNode
    pub glyph_dim_rect_opt: Option<Rect>, // represents the width and height of single monospace glyph(char)
    pub has_focus: bool,
    pub caret_w_select_vec: NonEmpty<(CaretWSelect, Option<MarkNodeId>)>, // the editor supports multiple carets/cursors and multiple selections
    pub selected_block_opt: Option<SelectedBlock>, // a selected AST node, the roc type of this node is shown in the editor on ctrl+shift+"up arrow"
    pub loaded_module: LoadedModule, // contains all roc symbols, exposed values, exposed aliases, solved types... in the file(=module)
    pub show_debug_view: bool,       // see render_debug.rs for the debug view
    pub dirty: bool, // EdModel is dirty if it has changed since the previous render.
}

// a selected AST node, the roc type of this node is shown in the editor on ctrl+shift+"up arrow"
#[derive(Debug, Copy, Clone)]
pub struct SelectedBlock {
    pub ast_node_id: ASTNodeId,
    pub mark_node_id: MarkNodeId,
    pub type_str: PoolStr,
}

pub fn init_model<'a>(
    code_str: &'a str, // entire roc file as one str
    file_path: &'a Path,
    env: Env<'a>, // contains all variables, identifiers, closures, top level symbols...
    loaded_module: LoadedModule, // contains all roc symbols, exposed values, exposed aliases, solved types... in the file(=module)
    code_arena: &'a Bump,        // bump allocation arena, used for fast memory allocation
    caret_pos: CaretPos,         // to set caret position when the file is displayed
) -> EdResult<EdModel<'a>> {
    // for debugging
    //println!("{}", code_str);
    let mut owned_loaded_module = loaded_module;

    let mut module = EdModule::new(code_str, env, &mut owned_loaded_module.interns, code_arena)?;

    let mut mark_node_pool = SlowPool::default();

    let (markup_ids, mark_id_ast_id_map) = if code_str.is_empty() {
        EmptyCodeStringSnafu {}.fail()
    } else {
        Ok(ast_to_mark_nodes(
            &mut module.env,
            &module.ast,
            &mut mark_node_pool,
            &owned_loaded_module.interns,
        )?)
    }?;

    let code_lines =
        CodeLines::from_str(&nodes::mark_nodes_to_string(&markup_ids, &mark_node_pool));
    let mut grid_node_map = GridNodeMap::default();

    let mut line_nr = 0;
    let mut col_nr = 0;

    for mark_node_id in &markup_ids {
        // for debugging:
        //println!("{}", tree_as_string(*mark_node_id, &mark_node_pool));
        EdModel::insert_mark_node_between_line(
            &mut line_nr,
            &mut col_nr,
            *mark_node_id,
            &mut grid_node_map,
            &mark_node_pool,
        )?
    }

    let caret = match caret_pos {
        CaretPos::Start => CaretWSelect::default(),
        CaretPos::Exact(txt_pos) => CaretWSelect::new(txt_pos, None),
        CaretPos::End => CaretWSelect::new(code_lines.end_txt_pos(), None),
    };

    Ok(EdModel {
        module,
        file_path,
        code_lines,
        grid_node_map,
        markup_ids,
        mark_node_pool,
        mark_id_ast_id_map,
        glyph_dim_rect_opt: None,
        has_focus: true,
        caret_w_select_vec: NonEmpty::new((caret, None)),
        selected_block_opt: None,
        loaded_module: owned_loaded_module,
        show_debug_view: false,
        dirty: true,
    })
}

impl<'a> EdModel<'a> {
    pub fn get_carets(&self) -> Vec<TextPos> {
        self.caret_w_select_vec
            .iter()
            .map(|tup| tup.0.caret_pos)
            .collect()
    }

    pub fn get_curr_mark_node_id(&self) -> UIResult<MarkNodeId> {
        let caret_pos = self.get_caret();
        self.grid_node_map.get_id_at_row_col(caret_pos)
    }

    // get id of MarkNode that is located before the caret
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
            let curr_mark_node = self.mark_node_pool.get(curr_mark_node_id);

            if let Some(parent_id) = curr_mark_node.get_parent_id_opt() {
                let parent = self.mark_node_pool.get(parent_id);
                let ast_node_id = self.mark_id_ast_id_map.get(curr_mark_node_id)?;
                Ok(parent.get_child_indices(
                    curr_mark_node_id,
                    ast_node_id,
                    &self.mark_id_ast_id_map,
                )?)
            } else {
                MissingParentSnafu {
                    node_id: curr_mark_node_id,
                }
                .fail()
            }
        } else {
            NoNodeAtCaretPositionSnafu {
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
    pub fn new(
        code_str: &'a str,
        mut env: Env<'a>,
        interns: &mut Interns, // contains ids of all identifiers in this roc file
        ast_arena: &'a Bump,
    ) -> EdResult<EdModule<'a>> {
        if !code_str.is_empty() {
            let parse_res = parse_ast::parse_from_string(code_str, &mut env, ast_arena, interns);

            match parse_res {
                Ok(ast) => Ok(EdModule { env, ast }),
                Err(err) => SrcParseSnafu {
                    syntax_err: format!("{:?}", err),
                }
                .fail(),
            }
        } else {
            EmptyCodeStringSnafu {}.fail()
        }
    }
}

#[cfg(test)]
pub mod test_ed_model {
    use crate::editor::ed_error::EdResult;
    use crate::editor::mvc::ed_model;
    use crate::editor::resources::strings::{
        nr_hello_world_lines, HELLO_WORLD, PLATFORM_DIR_NAME, PLATFORM_STR,
    };
    use crate::ui::text::caret_w_select::test_caret_w_select::convert_dsl_to_selection;
    use crate::ui::text::caret_w_select::test_caret_w_select::convert_selection_to_dsl;
    use crate::ui::text::caret_w_select::CaretPos;
    use crate::ui::text::lines::SelectableLines;
    use crate::ui::text::text_pos::TextPos;
    use crate::ui::ui_error::UIResult;
    use bumpalo::Bump;
    use ed_model::EdModel;
    use roc_ast::lang::env::Env;
    use roc_ast::mem_pool::pool::Pool;
    use roc_ast::module::load_module;
    use roc_load::{LoadedModule, Threading};
    use roc_module::symbol::IdentIds;
    use roc_module::symbol::ModuleIds;
    use roc_packaging::cache::RocCacheDir;
    use roc_types::subs::VarStore;
    use std::fs;
    use std::fs::File;
    use std::io::Write;
    use std::path::Path;
    use std::path::PathBuf;
    use tempfile::tempdir;
    use uuid::Uuid;

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
            CaretPos::End,
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

    // We use a Domain Specific Language to clearly represent some of the editor's state.
    // Here we convert that DSL to an EdModel.
    // Example of dsl: "val = ┃❮5❯", 5 is selected and the caret is located before the 5.
    pub fn ed_model_from_dsl<'a>(
        clean_code_str: &'a mut String,
        code_lines: Vec<String>,
        ed_model_refs: &'a mut EdModelRefs,
        module_ids: &'a ModuleIds,
        code_arena: &'a Bump,
    ) -> Result<EdModel<'a>, String> {
        // to be able to load the code as a LoadedModule we add a roc app header and a main function
        *clean_code_str = vec![HELLO_WORLD, clean_code_str.as_str()].join("");
        // for debugging
        //println!("{}", clean_code_str);

        let temp_dir = tempdir().expect("Failed to create temporary directory for test.");

        let platform_dir = temp_dir.path().join(PLATFORM_DIR_NAME);
        fs::create_dir(platform_dir.clone()).expect("Failed to create platform directory");
        let platform_module_path = platform_dir.join("main.roc");
        let mut platform_module_file =
            File::create(platform_module_path).expect("Failed to create main.roc");
        writeln!(platform_module_file, "{}", PLATFORM_STR).expect("Failed to write to main.roc");

        let temp_file_path_buf =
            PathBuf::from([Uuid::new_v4().to_string(), ".roc".to_string()].join(""));
        let temp_file_full_path = temp_dir.path().join(temp_file_path_buf);

        let mut file = File::create(temp_file_full_path.clone()).unwrap_or_else(|_| {
            panic!(
                "Failed to create temporary file for path {:?}",
                temp_file_full_path
            )
        });
        writeln!(file, "{}", clean_code_str)
            .unwrap_or_else(|_| panic!("Failed to write {:?} to file: {:?}", clean_code_str, file));

        let loaded_module = load_module(
            &temp_file_full_path,
            RocCacheDir::Disallowed,
            Threading::AllAvailable,
        );

        let mut ed_model = init_dummy_model(
            clean_code_str,
            loaded_module,
            module_ids,
            ed_model_refs,
            code_arena,
        )?;

        // adjust caret for header and main function
        let caret_w_select = convert_dsl_to_selection(&code_lines)?;
        let adjusted_caret_pos = TextPos {
            line: caret_w_select.caret_pos.line + nr_hello_world_lines(),
            column: caret_w_select.caret_pos.column,
        };

        ed_model.set_caret(adjusted_caret_pos);

        Ok(ed_model)
    }

    pub fn ed_model_to_dsl(ed_model: &EdModel) -> UIResult<Vec<String>> {
        let caret_w_select = ed_model.caret_w_select_vec.first().0;
        let code_lines = ed_model.code_lines.lines.clone();

        convert_selection_to_dsl(caret_w_select, code_lines)
    }
}
