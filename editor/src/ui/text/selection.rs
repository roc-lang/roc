#![allow(dead_code)]

use super::lines::Lines;
use super::text_pos::TextPos;
use crate::ui::theme::UITheme;
use crate::ui::ui_error::{InvalidSelectionSnafu, UIResult};
use bumpalo::collections::Vec as BumpVec;
use snafu::ensure;
use std::fmt;

#[derive(Debug, Copy, Clone)]
pub struct RawSelection {
    pub start_pos: TextPos,
    pub end_pos: TextPos,
}

impl std::fmt::Display for RawSelection {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "RawSelection: start_pos: line:{} col:{}, end_pos: line:{} col:{}",
            self.start_pos.line, self.start_pos.column, self.end_pos.line, self.end_pos.column
        )
    }
}

#[derive(Debug, Copy, Clone)]
pub struct Selection {
    pub start_pos: TextPos,
    pub end_pos: TextPos,
}

impl Selection {
    pub fn is_on_same_line(&self) -> bool {
        self.start_pos.line == self.end_pos.line
    }
}

pub fn validate_raw_sel(raw_sel: RawSelection) -> UIResult<Selection> {
    validate_selection(raw_sel.start_pos, raw_sel.end_pos)
}

pub fn validate_sel_opt(start_pos: TextPos, end_pos: TextPos) -> UIResult<Option<Selection>> {
    Ok(Some(validate_selection(start_pos, end_pos)?))
}

pub fn validate_selection(start_pos: TextPos, end_pos: TextPos) -> UIResult<Selection> {
    ensure!(
        start_pos.line <= end_pos.line,
        InvalidSelectionSnafu {
            err_msg: format!(
                "start_pos.line ({}) should be smaller than or equal to end_pos.line ({})",
                start_pos.line, end_pos.line
            )
        }
    );

    ensure!(
        !(start_pos.line == end_pos.line && start_pos.column >= end_pos.column),
        InvalidSelectionSnafu {
            err_msg: format!(
                "start_pos.column ({}) should be smaller than end_pos.column ({}) when start_pos.line equals end_pos.line",
                start_pos.column,
                end_pos.column
            )
        }
    );

    Ok(Selection { start_pos, end_pos })
}

use crate::graphics::primitives::rect::Rect;
use bumpalo::Bump;

pub fn create_selection_rects<'a>(
    valid_sel: Selection,
    lines: &dyn Lines,
    glyph_dim_rect: &Rect,
    theme: &UITheme,
    arena: &'a Bump,
) -> UIResult<BumpVec<'a, Rect>> {
    let Selection { start_pos, end_pos } = valid_sel;

    let mut all_rects: BumpVec<Rect> = BumpVec::new_in(arena);

    let height = glyph_dim_rect.height;
    let start_y = glyph_dim_rect.top_left_coords.y + height * (start_pos.line as f32);
    let line_start_x = glyph_dim_rect.top_left_coords.x;

    if start_pos.line == end_pos.line {
        let width = ((end_pos.column as f32) * glyph_dim_rect.width)
            - ((start_pos.column as f32) * glyph_dim_rect.width);
        let sel_rect_x = line_start_x + ((start_pos.column as f32) * glyph_dim_rect.width);

        all_rects.push(Rect {
            top_left_coords: (sel_rect_x, start_y).into(),
            width,
            height,
            color: theme.select_highlight,
        });
    } else {
        // first line
        let end_col = lines.line_len(start_pos.line)?;
        let width = ((end_col as f32) * glyph_dim_rect.width)
            - ((start_pos.column as f32) * glyph_dim_rect.width);

        let sel_rect_x = line_start_x + ((start_pos.column as f32) * glyph_dim_rect.width);

        all_rects.push(Rect {
            top_left_coords: (sel_rect_x, start_y).into(),
            width,
            height,
            color: theme.select_highlight,
        });

        //middle lines
        let nr_mid_lines = (end_pos.line - start_pos.line) - 1;
        let first_mid_line = start_pos.line + 1;

        for i in first_mid_line..(first_mid_line + nr_mid_lines) {
            let mid_line_len = lines.line_len(i)?;

            let width = (mid_line_len as f32) * glyph_dim_rect.width;

            let sel_rect_y = start_y + ((i - start_pos.line) as f32) * glyph_dim_rect.height;

            all_rects.push(Rect {
                top_left_coords: (line_start_x, sel_rect_y).into(),
                width,
                height,
                color: theme.select_highlight,
            });
        }

        //last line
        if end_pos.column > 0 {
            let sel_rect_y =
                start_y + ((end_pos.line - start_pos.line) as f32) * glyph_dim_rect.height;

            let width = (end_pos.column as f32) * glyph_dim_rect.width;

            all_rects.push(Rect {
                top_left_coords: (line_start_x, sel_rect_y).into(),
                width,
                height,
                color: theme.select_highlight,
            });
        }
    }

    Ok(all_rects)
}
