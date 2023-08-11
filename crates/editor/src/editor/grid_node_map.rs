use crate::editor::ed_error::EdResult;
use crate::editor::ed_error::NestedNodeWithoutChildrenSnafu;
use crate::editor::ed_error::{NoDefMarkNodeBeforeLineNrSnafu, NodeIdNotInGridNodeMapSnafu};
use crate::editor::mvc::ed_model::EdModel;
use crate::editor::util::first_last_index_of;
use crate::editor::util::index_of;
use crate::ui::text::selection::Selection;
use crate::ui::text::text_pos::TextPos;
use crate::ui::ui_error::{LineInsertionFailedSnafu, OutOfBoundsSnafu, UIResult};
use crate::ui::util::{slice_get, slice_get_mut};
use roc_ast::lang::core::ast::ASTNodeId;
use roc_code_markup::markup::mark_id_ast_id_map::MarkIdAstIdMap;
use roc_code_markup::markup::nodes::get_root_mark_node_id;
use roc_code_markup::slow_pool::MarkNodeId;
use roc_code_markup::slow_pool::SlowPool;
use snafu::OptionExt;
use std::cmp::Ordering;
use std::fmt;

#[derive(Debug)]
pub struct GridNodeMap {
    pub lines: Vec<Vec<MarkNodeId>>,
}

impl GridNodeMap {
    pub fn insert_between_line(
        &mut self,
        line_nr: usize,
        index: usize,
        len: usize,
        node_id: MarkNodeId,
    ) -> UIResult<()> {
        let nr_of_lines = self.lines.len();

        if line_nr < nr_of_lines {
            let line_ref = slice_get_mut(line_nr, &mut self.lines)?;
            let new_cols_vec: Vec<MarkNodeId> = std::iter::repeat(node_id).take(len).collect();

            line_ref.splice(index..index, new_cols_vec);
        } else if line_nr >= nr_of_lines {
            for _ in 0..((line_nr - nr_of_lines) + 1) {
                self.push_empty_line();
            }

            self.insert_between_line(line_nr, index, len, node_id)?;
        } else {
            LineInsertionFailedSnafu {
                line_nr,
                nr_of_lines,
            }
            .fail()?;
        }

        Ok(())
    }

    pub fn insert_empty_line(&mut self, line_nr: usize) -> UIResult<()> {
        if line_nr <= self.lines.len() {
            self.lines.insert(line_nr, Vec::new());

            Ok(())
        } else {
            OutOfBoundsSnafu {
                index: line_nr,
                collection_name: "code_lines.lines".to_owned(),
                len: self.lines.len(),
            }
            .fail()
        }
    }

    pub fn push_empty_line(&mut self) {
        self.lines.push(vec![]);
    }

    pub fn break_line(&mut self, line_nr: usize, col_nr: usize) -> UIResult<()> {
        // clippy prefers this over if-else
        match line_nr.cmp(&self.lines.len()) {
            Ordering::Less => {
                self.insert_empty_line(line_nr + 1)?;

                let line_ref = self.lines.get_mut(line_nr).unwrap(); // safe because we checked line_nr

                if col_nr < line_ref.len() {
                    let next_line_str: Vec<MarkNodeId> = line_ref.drain(col_nr..).collect();

                    let next_line_ref = self.lines.get_mut(line_nr + 1).unwrap(); // safe because we just added the line

                    *next_line_ref = next_line_str;
                }

                Ok(())
            }
            Ordering::Equal => self.insert_empty_line(line_nr + 1),
            Ordering::Greater => OutOfBoundsSnafu {
                index: line_nr,
                collection_name: "grid_node_map.lines".to_owned(),
                len: self.lines.len(),
            }
            .fail(),
        }
    }

    pub fn clear_line(&mut self, line_nr: usize) -> UIResult<()> {
        let line_ref = slice_get_mut(line_nr, &mut self.lines)?;

        *line_ref = vec![];

        Ok(())
    }

    pub fn del_line(&mut self, line_nr: usize) {
        self.lines.remove(line_nr);
    }

    pub fn del_at_line(&mut self, line_nr: usize, column: usize) -> UIResult<()> {
        let line_ref = slice_get_mut(line_nr, &mut self.lines)?;

        line_ref.remove(column);

        Ok(())
    }

    pub fn del_range_at_line(
        &mut self,
        line_nr: usize,
        col_range: std::ops::Range<usize>,
    ) -> UIResult<()> {
        let line_ref = slice_get_mut(line_nr, &mut self.lines)?;

        line_ref.drain(col_range);

        Ok(())
    }

    pub fn del_selection(&mut self, selection: Selection) -> UIResult<()> {
        if selection.is_on_same_line() {
            let line_ref = slice_get_mut(selection.start_pos.line, &mut self.lines)?;

            line_ref.drain(selection.start_pos.column..selection.end_pos.column);
        } else {
            unimplemented!("TODO support deleting multiline selection")
        }

        Ok(())
    }

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

    // get position of first occurrence of node_id if get_first_pos, else get the last occurrence
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
            NodeIdNotInGridNodeMapSnafu { node_id }.fail()
        }
    }

    // returns start and end pos of Expr2/Def2, relevant AST node and MarkNodeId of the corresponding MarkupNode
    pub fn get_block_start_end_pos(
        &self,
        caret_pos: TextPos,
        ed_model: &EdModel,
    ) -> EdResult<(TextPos, TextPos, ASTNodeId, MarkNodeId)> {
        let line = slice_get(caret_pos.line, &self.lines)?;
        let node_id = *slice_get(caret_pos.column, line)?;
        let node = ed_model.mark_node_pool.get(node_id);

        if node.is_nested() {
            let (start_pos, end_pos) = self.get_nested_start_end_pos(node_id, ed_model)?;

            Ok((
                start_pos,
                end_pos,
                ed_model.mark_id_ast_id_map.get(node_id)?,
                node_id,
            ))
        } else {
            let (first_node_index, last_node_index) = first_last_index_of(node_id, line)?;

            let curr_node_id = *slice_get(first_node_index, line)?;
            let curr_ast_node_id = ed_model.mark_id_ast_id_map.get(curr_node_id)?;

            let mut expr_start_index = first_node_index;
            let mut expr_end_index = last_node_index;

            // we may encounter ast id's of children of the current node
            let mut pos_extra_subtract = 0;

            for i in (0..first_node_index).rev() {
                let prev_pos_node_id = *slice_get(i, line)?;
                let prev_ast_node_id = ed_model.mark_id_ast_id_map.get(prev_pos_node_id)?;

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
                let next_ast_node_id = ed_model.mark_id_ast_id_map.get(*next_pos_node_id)?;

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

            let correct_mark_node_id = GridNodeMap::get_top_node_with_expr_id(
                curr_node_id,
                &ed_model.mark_node_pool,
                &ed_model.mark_id_ast_id_map,
            )?;

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
                correct_mark_node_id,
            ))
        }
    }

    // A markup node may refer to a bracket `{`, in that case we want the parent, a Nested MarkNode.
    // `{` is not the entire Expr2
    fn get_top_node_with_expr_id(
        curr_node_id: MarkNodeId,
        mark_node_pool: &SlowPool,
        mark_id_ast_id_map: &MarkIdAstIdMap,
    ) -> EdResult<MarkNodeId> {
        let curr_node = mark_node_pool.get(curr_node_id);

        if let Some(parent_id) = curr_node.get_parent_id_opt() {
            if mark_id_ast_id_map.get(parent_id)? == mark_id_ast_id_map.get(curr_node_id)? {
                Ok(parent_id)
            } else {
                Ok(curr_node_id)
            }
        } else {
            Ok(curr_node_id)
        }
    }

    pub fn get_nested_start_end_pos(
        &self,
        nested_node_id: MarkNodeId,
        ed_model: &EdModel,
    ) -> EdResult<(TextPos, TextPos)> {
        let left_most_leaf = self.get_leftmost_leaf(nested_node_id, ed_model)?;

        let right_most_leaf = self.get_rightmost_leaf(nested_node_id, ed_model)?;

        let expr_start_pos = ed_model
            .grid_node_map
            .get_node_position(left_most_leaf, true)?;
        let expr_end_pos = ed_model
            .grid_node_map
            .get_node_position(right_most_leaf, false)?
            .increment_col();

        Ok((expr_start_pos, expr_end_pos))
    }

    fn get_leftmost_leaf(
        &self,
        nested_node_id: MarkNodeId,
        ed_model: &EdModel,
    ) -> EdResult<MarkNodeId> {
        let mut children_ids = ed_model
            .mark_node_pool
            .get(nested_node_id)
            .get_children_ids();
        let mut first_child_id = 0;

        while !children_ids.is_empty() {
            first_child_id =
                *children_ids
                    .first()
                    .with_context(|| NestedNodeWithoutChildrenSnafu {
                        node_id: nested_node_id,
                    })?;

            children_ids = ed_model
                .mark_node_pool
                .get(first_child_id)
                .get_children_ids();
        }

        Ok(first_child_id)
    }

    fn get_rightmost_leaf(
        &self,
        nested_node_id: MarkNodeId,
        ed_model: &EdModel,
    ) -> EdResult<MarkNodeId> {
        let mut children_ids = ed_model
            .mark_node_pool
            .get(nested_node_id)
            .get_children_ids();
        let mut last_child_id = 0;

        while !children_ids.is_empty() {
            last_child_id =
                *children_ids
                    .last()
                    .with_context(|| NestedNodeWithoutChildrenSnafu {
                        node_id: nested_node_id,
                    })?;

            children_ids = ed_model
                .mark_node_pool
                .get(last_child_id)
                .get_children_ids();
        }

        Ok(last_child_id)
    }

    // get id of root mark_node whose ast_node_id points to a DefId
    pub fn get_def_mark_node_id_before_line(
        &self,
        line_nr: usize,
        mark_node_pool: &SlowPool,
        mark_id_ast_id_map: &MarkIdAstIdMap,
    ) -> EdResult<MarkNodeId> {
        for curr_line_nr in (0..line_nr).rev() {
            let first_col_pos = TextPos {
                line: curr_line_nr,
                column: 0,
            };

            if self.node_exists_at_pos(first_col_pos) {
                let mark_node_id = self.get_id_at_row_col(first_col_pos)?;
                let root_mark_node_id = get_root_mark_node_id(mark_node_id, mark_node_pool);

                let ast_node_id = mark_id_ast_id_map.get(root_mark_node_id)?;

                if let ASTNodeId::ADefId(_) = ast_node_id {
                    return Ok(root_mark_node_id);
                }
            }
        }

        NoDefMarkNodeBeforeLineNrSnafu { line_nr }.fail()
    }
}

impl Default for GridNodeMap {
    fn default() -> Self {
        GridNodeMap {
            lines: vec![Vec::new()],
        }
    }
}

impl fmt::Display for GridNodeMap {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for row in &self.lines {
            let row_str = row
                .iter()
                .map(|mark_node_id| format!(" {mark_node_id} "))
                .collect::<Vec<String>>()
                .join(", ");

            writeln!(f, "{row_str}")?;
        }

        writeln!(f, "(grid_node_map, {:?} lines)", self.lines.len())?;

        Ok(())
    }
}
