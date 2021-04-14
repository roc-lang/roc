use crate::lang::ast::Expr2;
use crate::lang::pool::NodeId;
use crate::editor::slow_pool::SlowPool;
use crate::editor::util::first_last_index_of;
use crate::editor::ed_error::EdResult;
use crate::editor::slow_pool::MarkNodeId;
use crate::editor::util::index_of;
use crate::ui::text::text_pos::TextPos;
use crate::ui::ui_error::UIResult;
use crate::ui::util::{slice_get, slice_get_mut};
use std::fmt;

#[derive(Debug)]
pub struct GridNodeMap {
    pub lines: Vec<Vec<MarkNodeId>>,
}

impl GridNodeMap {
    pub fn new() -> GridNodeMap {
        GridNodeMap {
            lines: vec![vec![]],
        }
    }

    pub fn add_to_line(&mut self, line_nr: usize, len: usize, node_id: MarkNodeId) -> UIResult<()> {
        let line_ref = slice_get_mut(line_nr, &mut self.lines)?;
        let mut new_cols_vec: Vec<MarkNodeId> = std::iter::repeat(node_id).take(len).collect();

        line_ref.append(&mut new_cols_vec);

        Ok(())
    }

    pub fn insert_between_line(
        &mut self,
        line_nr: usize,
        index: usize,
        len: usize,
        node_id: MarkNodeId,
    ) -> UIResult<()> {
        let line_ref = slice_get_mut(line_nr, &mut self.lines)?;
        let new_cols_vec: Vec<MarkNodeId> = std::iter::repeat(node_id).take(len).collect();

        line_ref.splice(index..index, new_cols_vec);

        Ok(())
    }

    pub fn del_at_line(&mut self, line_nr: usize, index: usize) -> UIResult<()> {
        let line_ref = slice_get_mut(line_nr, &mut self.lines)?;

        line_ref.remove(index);

        Ok(())
    }

    /*pub fn new_line(&mut self) {
        self.lines.push(vec![])
    }*/

    pub fn get_id_at_row_col(&self, caret_pos: TextPos) -> UIResult<MarkNodeId> {
        let line = slice_get(caret_pos.line, &self.lines)?;
        let node_id = slice_get(caret_pos.column, line)?;

        Ok(*node_id)
    }

    pub fn get_offset_to_node_id(
        &self,
        caret_pos: TextPos,
        node_id: MarkNodeId,
    ) -> EdResult<usize> {
        let line = slice_get(caret_pos.line, &self.lines)?;

        let first_node_index = index_of(node_id, line)?;

        Ok(caret_pos.column - first_node_index)
    }

    pub fn get_node_start_end_pos(
        &self,
        caret_pos: TextPos,
    ) -> EdResult<(TextPos, TextPos)> {
        let line = slice_get(caret_pos.line, &self.lines)?;
        let node_id = slice_get(caret_pos.column, line)?;

        let (first_node_index, last_node_index) = first_last_index_of(*node_id, line)?;

        Ok((
            TextPos {
                line: caret_pos.line,
                column: first_node_index,
            },
            TextPos {
                line: caret_pos.line,
                column: last_node_index + 1,
            }
        ))
    }

    pub fn get_expr_start_end_pos(
        &self,
        caret_pos: TextPos,
        mark_node_pool: &SlowPool,
    ) -> EdResult<(TextPos, TextPos, NodeId<Expr2>)> {
        let line = slice_get(caret_pos.line, &self.lines)?;
        let node_id = slice_get(caret_pos.column, line)?;

        let (first_node_index, last_node_index) = first_last_index_of(*node_id, line)?;

        let curr_node_id = slice_get(first_node_index, line)?;
        let curr_ast_node_id = mark_node_pool.get(*curr_node_id).get_ast_node_id();

        let mut expr_start_index = first_node_index;
        let mut expr_end_index = last_node_index;

        for i in (0..first_node_index).rev() {
            let prev_pos_node_id = slice_get(i, line)?;
            let prev_ast_node_id = mark_node_pool.get(*prev_pos_node_id).get_ast_node_id();

            if prev_ast_node_id == curr_ast_node_id {
                expr_start_index -= 1;
            } else {
                break
            }
        }

        for i in last_node_index..line.len() {
            let next_pos_node_id = slice_get(i, line)?;
            let next_ast_node_id = mark_node_pool.get(*next_pos_node_id).get_ast_node_id();

            if next_ast_node_id == curr_ast_node_id {
                expr_end_index += 1;
            } else {
                break
            }
        }


        Ok((
            TextPos {
                line: caret_pos.line,
                column: expr_start_index,
            },
            TextPos {
                line: caret_pos.line,
                column: expr_end_index,
            },
            curr_ast_node_id,
        ))
    }
}

impl fmt::Display for GridNodeMap {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for row in &self.lines {
            let row_str = row
                .iter()
                .map(|mark_node_id| format!(" {} ", mark_node_id))
                .collect::<Vec<String>>()
                .join(", ");

            write!(f, "{}", row_str)?;
        }

        write!(f, "      (grid_node_map)")?;

        Ok(())
    }
}
