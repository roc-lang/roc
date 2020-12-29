use crate::colors;
use crate::error::{EdResult, InvalidSelection};
use crate::rect::Rect;
use crate::tea::model::RawSelection;
use crate::vec_result::get_res;
use snafu::ensure;

//using the "parse don't validate" pattern
struct ValidSelection {
    selection: RawSelection,
}

fn validate_selection(selection: RawSelection) -> EdResult<ValidSelection> {
    let RawSelection { start_pos, end_pos } = selection;

    ensure!(
        start_pos.line <= end_pos.line,
        InvalidSelection {
            err_msg: format!(
                "start_pos.line ({}) should be smaller than or equal to end_pos.line ({})",
                start_pos.line, end_pos.line
            )
        }
    );

    ensure!(
        !(start_pos.line == end_pos.line && start_pos.column > end_pos.column),
        InvalidSelection {
            err_msg: format!(
                "start_pos.column ({}) should be smaller than or equal to end_pos.column ({}) when start_pos.line equals end_pos.line",
                start_pos.column,
                end_pos.column
            )
        }
    );

    Ok(ValidSelection {
        selection: RawSelection { start_pos, end_pos },
    })
}

pub fn create_selection_rects(
    raw_sel: RawSelection,
    glyph_bound_rects: &Vec<Vec<Rect>>,
) -> EdResult<Vec<Rect>> {
    let valid_sel = validate_selection(raw_sel)?;
    let RawSelection { start_pos, end_pos } = valid_sel.selection;

    let mut all_rects = Vec::new();

    if start_pos.line == end_pos.line {
        let start_glyph_rect = get_res(
            start_pos.column,
            get_res(start_pos.line, glyph_bound_rects)?,
        )?;

        let stop_glyph_rect = get_res(
            end_pos.column - 1,
            get_res(end_pos.line, glyph_bound_rects)?,
        )?;

        let top_left_coords = start_glyph_rect.top_left_coords;

        let height = start_glyph_rect.height;
        let width = (stop_glyph_rect.top_left_coords.x - start_glyph_rect.top_left_coords.x)
            + stop_glyph_rect.width;

        all_rects.push(Rect {
            top_left_coords,
            width,
            height,
            color: colors::WHITE,
        });

        Ok(all_rects)
    } else {
        // first line
        let start_line = get_res(start_pos.line, glyph_bound_rects)?;

        let start_glyph_rect = get_res(start_pos.column, start_line)?;

        let start_line_last_glyph_rect = get_res(start_line.len() - 1, start_line)?;

        let top_left_coords = start_glyph_rect.top_left_coords;

        let height = start_glyph_rect.height;
        let width = (start_line_last_glyph_rect.top_left_coords.x
            - start_glyph_rect.top_left_coords.x)
            + start_line_last_glyph_rect.width;

        all_rects.push(Rect {
            top_left_coords,
            width,
            height,
            color: colors::WHITE,
        });

        //middle lines
        let nr_mid_lines = (end_pos.line - start_pos.line) - 1;
        let first_mid_line = start_pos.line + 1;

        for i in first_mid_line..(first_mid_line + nr_mid_lines) {
            let mid_line = get_res(i, glyph_bound_rects)?;

            let mid_line_first_glyph_rect = get_res(0, mid_line)?;

            let mid_line_last_glyph_rect = get_res(mid_line.len() - 1, mid_line)?;

            let top_left_coords = mid_line_first_glyph_rect.top_left_coords;

            let height = mid_line_first_glyph_rect.height;
            let width = (mid_line_last_glyph_rect.top_left_coords.x
                - mid_line_first_glyph_rect.top_left_coords.x)
                + mid_line_last_glyph_rect.width;

            all_rects.push(Rect {
                top_left_coords,
                width,
                height,
                color: colors::WHITE,
            });
        }

        //last line
        if end_pos.column > 0 {
            let stop_line = get_res(end_pos.line, glyph_bound_rects)?;

            let stop_line_first_glyph_rect = get_res(0, stop_line)?;

            let stop_glyph_rect = get_res(end_pos.column - 1, stop_line)?;

            let top_left_coords = stop_line_first_glyph_rect.top_left_coords;

            let height = stop_glyph_rect.height;
            let width = (stop_glyph_rect.top_left_coords.x
                - stop_line_first_glyph_rect.top_left_coords.x)
                + stop_glyph_rect.width;

            all_rects.push(Rect {
                top_left_coords,
                width,
                height,
                color: colors::WHITE,
            });
        }

        Ok(all_rects)
    }
}
