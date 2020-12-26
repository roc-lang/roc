
use crate::rect::{Rect};
use crate::vec_result::{get_res};
use crate::error::{EdResult, InvalidSelection};
use crate::colors;
use snafu::ensure;

pub struct RawSelection {
    pub start_line_indx: usize,
    pub pos_in_start_line: usize,
    pub stop_line_indx: usize,
    pub pos_in_stop_line: usize,
}

//using the "parse don't validate" pattern
struct ValidSelection {
    selection: RawSelection
}

fn validate_selection( selection: RawSelection) -> EdResult<ValidSelection> {
    let RawSelection {start_line_indx, pos_in_start_line, stop_line_indx, pos_in_stop_line} = selection;

    ensure!(
        start_line_indx <= stop_line_indx,
        InvalidSelection { err_msg: 
            format!(
                "start_line_indx ({}) should be smaller than or equal to stop_line_indx ({})",
                start_line_indx,
                stop_line_indx 
            )
        }
    );

    ensure!(
        !(start_line_indx == stop_line_indx && pos_in_start_line > pos_in_stop_line),
        InvalidSelection { err_msg: 
            format!(
                "pos_in_start_line ({}) should be smaller than or equal to pos_in_stop_line ({}) when start_line_indx equals stop_line_indx",
                pos_in_start_line,
                pos_in_stop_line
            )
        }
    );

    Ok(
        ValidSelection {
            selection
        }
    )
}


pub fn create_selection_rects(
    raw_sel: RawSelection,
    glyph_bound_rects: &Vec<Vec<Rect>>
)  -> EdResult<Vec<Rect>> {
    let valid_sel = validate_selection(raw_sel)?;
    let RawSelection {start_line_indx, pos_in_start_line, stop_line_indx, pos_in_stop_line} = valid_sel.selection;

    let mut all_rects = Vec::new();

    if start_line_indx == stop_line_indx {
        let start_glyph_rect = 
            get_res(
                pos_in_start_line,
                get_res(start_line_indx, glyph_bound_rects)?,
            )?;

        let stop_glyph_rect = 
            get_res(
                pos_in_stop_line,
                get_res(stop_line_indx, glyph_bound_rects)?
            )?;

        let top_left_coords =
            start_glyph_rect.top_left_coords;

        let height = start_glyph_rect.height;
        let width = (stop_glyph_rect.top_left_coords.x - start_glyph_rect.top_left_coords.x) + stop_glyph_rect.width;

        all_rects.push(
            Rect {
                top_left_coords,
                width,
                height,
                color: colors::WHITE
            }
        );

        Ok(all_rects)
    } else {
        // first line
        let start_line = get_res(start_line_indx, glyph_bound_rects)?;

        let start_glyph_rect = 
            get_res(
                pos_in_start_line,
                start_line
            )?;

        let start_line_last_glyph_rect = 
            get_res(
                start_line.len() - 1,
                start_line
            )?;

        let top_left_coords =
            start_glyph_rect.top_left_coords;

        let height = start_glyph_rect.height;
        let width = (start_line_last_glyph_rect.top_left_coords.x - start_glyph_rect.top_left_coords.x) + start_line_last_glyph_rect.width;

        all_rects.push(
            Rect {
                top_left_coords,
                width,
                height,
                color: colors::WHITE
            }
        );

        //middle lines
        let nr_mid_lines = (stop_line_indx - start_line_indx) - 1;
        let first_mid_line = start_line_indx + 1;

        for i in first_mid_line..(first_mid_line + nr_mid_lines)  {
            let mid_line = get_res(i, glyph_bound_rects)?;

            let mid_line_first_glyph_rect = 
                get_res(
                    0,
                    mid_line
                )?;

            let mid_line_last_glyph_rect = 
                get_res(
                    mid_line.len() - 1,
                    mid_line
                )?;

            let top_left_coords =
                mid_line_first_glyph_rect.top_left_coords;

            let height = mid_line_first_glyph_rect.height;
            let width = (mid_line_last_glyph_rect.top_left_coords.x - mid_line_first_glyph_rect.top_left_coords.x) + mid_line_last_glyph_rect.width;

            all_rects.push(
                Rect {
                    top_left_coords,
                    width,
                    height,
                    color: colors::WHITE
                }
            );
        }

        //last line
        let stop_line = get_res(stop_line_indx, glyph_bound_rects)?;

        let stop_line_first_glyph_rect = 
        get_res(
            0,
            stop_line
        )?;

        let stop_glyph_rect = 
            get_res(
                pos_in_stop_line,
                stop_line
            )?;

        let top_left_coords =
            stop_line_first_glyph_rect.top_left_coords;

        let height = stop_glyph_rect.height;
        let width = (stop_glyph_rect.top_left_coords.x - stop_line_first_glyph_rect.top_left_coords.x) + stop_glyph_rect.width;

        all_rects.push(
            Rect {
                top_left_coords,
                width,
                height,
                color: colors::WHITE
            }
        );
        
        Ok(all_rects)
    }
}