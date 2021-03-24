
use crate::ui::text::text_pos::TextPos;
use crate::ui::util::{slice_get, slice_get_mut};
use crate::ui::ui_error::UIResult;
use crate::editor::slow_pool::MarkNodeId;


#[derive(Debug)]
pub struct GridNodeMap {
    pub lines: Vec<Vec<MarkNodeId>>
}

impl GridNodeMap {
    pub fn new() -> GridNodeMap {
        vec![vec![]]
    }

    pub fn add_to_line(&mut self, line: usize, len: usize, node_id: MarkNodeId) -> UIResult<()> {
        let mut line = slice_get_mut(line, &mut self.lines)?;
        let mut new_cols_vec: Vec<MarkNodeId> = std::iter::repeat(node_id).take(len).collect();

        line.append(&mut new_cols_vec);

        Ok(())
    }

    pub fn new_line(&mut self) {
        self.lines.push(vec![])
    }

    pub fn get_id_at_row_col(&self, caret_pos: TextPos) -> UIResult<MarkNodeId> {
        let line = slice_get(caret_pos.line, &self.lines)?;
        let node_id = slice_get(caret_pos.column, line)?;
    
        Ok(*node_id)
    }
}