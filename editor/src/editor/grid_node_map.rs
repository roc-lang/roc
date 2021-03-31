use crate::editor::ed_error::EdResult;
use crate::editor::slow_pool::MarkNodeId;
use crate::editor::util::index_of;
use crate::ui::text::text_pos::TextPos;
use crate::ui::ui_error::UIResult;
use crate::ui::util::{slice_get, slice_get_mut};

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
}
