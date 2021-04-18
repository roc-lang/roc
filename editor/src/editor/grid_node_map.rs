use crate::editor::ed_error::EdResult;
use crate::editor::ed_error::NestedNodeWithoutChildren;
use crate::editor::ed_error::NodeIdNotInGridNodeMap;
use crate::editor::mvc::ed_model::EdModel;
use crate::editor::slow_pool::MarkNodeId;
use crate::editor::util::first_last_index_of;
use crate::editor::util::index_of;
use crate::lang::ast::Expr2;
use crate::lang::pool::NodeId;
use crate::ui::text::selection::Selection;
use crate::ui::text::text_pos::TextPos;
use crate::ui::ui_error::UIResult;
use crate::ui::util::{slice_get, slice_get_mut};
use snafu::OptionExt;
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

    pub fn del_selection(&mut self, selection: Selection) -> UIResult<()> {
        if selection.is_on_same_line() {
            let line_ref = slice_get_mut(selection.start_pos.line, &mut self.lines)?;

            line_ref.drain(selection.start_pos.column..selection.end_pos.column);
        } else {
            // TODO support multiline
        }

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

    pub fn node_exists_at_pos(&self, pos: TextPos) -> bool {
        if pos.line < self.lines.len() {
            // safe unwrap because we checked the length
            let line = self.lines.get(pos.line).unwrap();

            pos.column < line.len()
        } else {
            false
        }
    }

    // get position of first occurence of node_id if get_first_pos, else get the last occurence
    pub fn get_node_position(&self, node_id: MarkNodeId, get_first_pos: bool) -> EdResult<TextPos> {
        let mut last_pos_opt = None;

        for (line_index, line) in self.lines.iter().enumerate() {
            for (col_index, iter_node_id) in line.iter().enumerate() {
                if node_id == *iter_node_id && get_first_pos {
                    return Ok(TextPos {
                        line: line_index,
                        column: col_index,
                    });
                } else if node_id == *iter_node_id {
                    last_pos_opt = Some(TextPos {
                        line: line_index,
                        column: col_index,
                    })
                } else if let Some(last_pos) = last_pos_opt {
                    return Ok(last_pos);
                }
            }
        }

        if let Some(last_pos) = last_pos_opt {
            Ok(last_pos)
        } else {
            NodeIdNotInGridNodeMap { node_id }.fail()
        }
    }

    // retruns start and end pos of Expr2, relevant AST node and MarkNodeId of the corresponding MarkupNode
    pub fn get_expr_start_end_pos(
        &self,
        caret_pos: TextPos,
        ed_model: &EdModel,
    ) -> EdResult<(TextPos, TextPos, NodeId<Expr2>, MarkNodeId)> {
        let line = slice_get(caret_pos.line, &self.lines)?;
        let node_id = slice_get(caret_pos.column, line)?;
        let node = ed_model.markup_node_pool.get(*node_id);

        if node.is_nested() {
            let (start_pos, end_pos) = self.get_nested_start_end_pos(*node_id, ed_model)?;

            Ok((start_pos, end_pos, node.get_ast_node_id(), *node_id))
        } else {
            let (first_node_index, last_node_index) = first_last_index_of(*node_id, line)?;

            let curr_node_id = slice_get(first_node_index, line)?;
            let curr_ast_node_id = ed_model
                .markup_node_pool
                .get(*curr_node_id)
                .get_ast_node_id();

            let mut expr_start_index = first_node_index;
            let mut expr_end_index = last_node_index;

            // we may encounter ast id's of children of the current node
            let mut pos_extra_subtract = 0;

            for i in (0..first_node_index).rev() {
                let prev_pos_node_id = slice_get(i, line)?;
                let prev_ast_node_id = ed_model
                    .markup_node_pool
                    .get(*prev_pos_node_id)
                    .get_ast_node_id();

                if prev_ast_node_id == curr_ast_node_id {
                    if pos_extra_subtract > 0 {
                        expr_start_index -= pos_extra_subtract + 1;
                        pos_extra_subtract = 0;
                    } else {
                        expr_start_index -= 1;
                    }
                } else {
                    pos_extra_subtract += 1;
                }
            }

            // we may encounter ast id's of children of the current node
            let mut pos_extra_add = 0;

            for i in last_node_index..line.len() {
                let next_pos_node_id = slice_get(i, line)?;
                let next_ast_node_id = ed_model
                    .markup_node_pool
                    .get(*next_pos_node_id)
                    .get_ast_node_id();

                if next_ast_node_id == curr_ast_node_id {
                    if pos_extra_add > 0 {
                        expr_end_index += pos_extra_add + 1;
                        pos_extra_add = 0;
                    } else {
                        expr_end_index += 1;
                    }
                } else {
                    pos_extra_add += 1;
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
                *curr_node_id,
            ))
        }
    }

    pub fn get_nested_start_end_pos(
        &self,
        nested_node_id: MarkNodeId,
        ed_model: &EdModel,
    ) -> EdResult<(TextPos, TextPos)> {
        let parent_mark_node = ed_model.markup_node_pool.get(nested_node_id);

        let all_child_ids = parent_mark_node.get_children_ids();
        let first_child_id = all_child_ids
            .first()
            .with_context(|| NestedNodeWithoutChildren {
                node_id: nested_node_id,
            })?;
        let last_child_id = all_child_ids
            .last()
            .with_context(|| NestedNodeWithoutChildren {
                node_id: nested_node_id,
            })?;

        let expr_start_pos = ed_model
            .grid_node_map
            .get_node_position(*first_child_id, true)?;
        let expr_end_pos = ed_model
            .grid_node_map
            .get_node_position(*last_child_id, false)?
            .increment_col();

        Ok((expr_start_pos, expr_end_pos))
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
