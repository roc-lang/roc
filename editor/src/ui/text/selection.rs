use crate::ui::ui_error::{UIResult, InvalidSelection};
use crate::ui::colors;
use super::text_pos::TextPos;
use super::lines::Lines;
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

pub fn validate_raw_sel(raw_sel: RawSelection) -> UIResult<Selection> {
    validate_selection(raw_sel.start_pos, raw_sel.end_pos)
}

pub fn validate_selection(start_pos: TextPos, end_pos: TextPos) -> UIResult<Selection> {

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

    Ok(Selection {
        start_pos, end_pos,
    })
}


use bumpalo::Bump;
use crate::graphics::primitives::rect::Rect;

pub fn create_selection_rects<'a>(
    valid_sel: Selection,
    lines: &dyn Lines,
    glyph_dim_rect: &Rect,
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
            color: colors::SELECT_COLOR,
        });

        Ok(all_rects)
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
            color: colors::SELECT_COLOR,
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
                color: colors::SELECT_COLOR,
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
                color: colors::SELECT_COLOR,
            });
        }

        Ok(all_rects)
    }
}

#[cfg(test)]
pub mod test_selection {
    use crate::ui::ui_error::{UIResult, OutOfBounds};
    use crate::ui::text::{
        text_pos::TextPos,
        selection::{validate_selection},
        big_selectable_text,
        big_selectable_text::BigSelectableText,
        caret_w_select::CaretWSelect,
        lines::Lines,
    };
    use crate::editor::mvc::ed_update::{
        move_caret_down, move_caret_left, move_caret_right, move_caret_up, MoveCaretFun,
    };
    use crate::ui::util::slice_get;
    use core::cmp::Ordering;
    use pest::Parser;
    use snafu::OptionExt;
    use bumpalo::Bump;
    use std::collections::HashMap;
    use std::{
        slice::SliceIndex,
        fs,
        io::prelude::*,
        path::Path,
        time::{SystemTime}
    };

    #[derive(Parser)]
    #[grammar = "../tests/selection.pest"]
    pub struct LineParser;

    // show selection and caret position as symbols in lines for easy testing
    pub fn convert_selection_to_dsl(
        caret_w_select: CaretWSelect,
        lines: &mut [String],
    ) -> UIResult<&[String]> {
        let selection_opt = caret_w_select.selection_opt;
        let caret_pos = caret_w_select.caret_pos;

        if let Some(sel) = selection_opt {
            let mut to_insert = vec![
                (sel.start_pos, '['),
                (sel.end_pos, ']'),
                (caret_pos, '|'),
            ];
            let symbol_map: HashMap<char, usize> =
                [('[', 2), (']', 0), ('|', 1)].iter().cloned().collect();

            // sort for nice printing
            to_insert.sort_by(|a, b| {
                let pos_cmp = a.0.cmp(&b.0);
                if pos_cmp == Ordering::Equal {
                    symbol_map.get(&a.1).cmp(&symbol_map.get(&b.1))
                } else {
                    pos_cmp
                }
            });

            // insert symbols into text lines
            for i in 0..to_insert.len() {
                let (pos, insert_char) = *slice_get(i, &to_insert)?;

                insert_at_pos(lines, pos, insert_char)?;

                // shift position of following symbols now that symbol is inserted
                for j in i..to_insert.len() {
                    let (old_pos, _) = get_mut_res(j, &mut to_insert)?;

                    if old_pos.line == pos.line {
                        old_pos.column += 1;
                    }
                }
            }
        } else {
            insert_at_pos(lines, caret_pos, '|')?;
        }

        Ok(lines)
    }

    fn insert_at_pos(lines: &mut [String], pos: TextPos, insert_char: char) -> UIResult<()> {
        let line = get_mut_res(pos.line, lines)?;
        line.insert(pos.column, insert_char);

        Ok(())
    }

    // It's much nicer to have get_mut return a Result with clear error than an Option
    fn get_mut_res<T>(
        index: usize,
        vec: &mut [T],
    ) -> UIResult<&mut <usize as SliceIndex<[T]>>::Output> {
        let vec_len = vec.len();

        let elt_ref = vec.get_mut(index).context(OutOfBounds {
            index,
            collection_name: "Slice",
            len: vec_len,
        })?;

        Ok(elt_ref)
    }

    fn big_text_from_str(lines_str: &str) -> BigSelectableText {
        let epoch_time = SystemTime::now().duration_since(SystemTime::UNIX_EPOCH).unwrap().as_nanos();
        let file_name = format!("temp_{:?}.txt", epoch_time);
        let path = Path::new(&file_name);

        let mut temp_file = fs::File::create(path).unwrap();
        temp_file.write_all(lines_str.as_bytes()).unwrap();

        let big_text = big_selectable_text::from_path(path).unwrap();
        fs::remove_file(path).unwrap();

        big_text
    }

    pub fn big_text_from_dsl_str(lines: &[String]) -> BigSelectableText {
        big_text_from_str(
            &lines
                .iter()
                .map(|line| line.replace(&['[', ']', '|'][..], ""))
                .collect::<Vec<String>>()
                .join(""),
        )
    }

    pub fn all_lines_vec(big_sel_text: &BigSelectableText) -> Vec<String> {
        let mut lines: Vec<String> = Vec::new();

        for i in 0..big_sel_text.nr_of_lines() {
            lines.push(
                big_sel_text.get_line(i).unwrap().to_string()
            );
        }

        lines
    }

    // Retrieve selection and position from formatted string
    pub fn convert_dsl_to_selection(
        lines: &[String],
    ) -> Result<CaretWSelect, String> {
        let lines_str: String = lines.join("");

        let parsed = LineParser::parse(Rule::linesWithSelect, &lines_str)
            .expect("Selection test DSL parsing failed");

        let mut caret_opt: Option<(usize, usize)> = None;
        let mut sel_start_opt: Option<(usize, usize)> = None;
        let mut sel_end_opt: Option<(usize, usize)> = None;
        let mut line_nr = 0;
        let mut col_nr = 0;

        for line in parsed {
            for elt in line.into_inner() {
                match elt.as_rule() {
                    Rule::optCaret => {
                        if elt.as_span().as_str() == "|" {
                            if caret_opt.is_some() {
                                return Err(
                                    "Multiple carets found, there should be only one".to_owned()
                                );
                            } else {
                                caret_opt = Some((line_nr, col_nr));
                            }
                        }
                    }
                    Rule::optSelStart => {
                        if sel_start_opt.is_some() {
                            if elt.as_span().as_str() == "[" {
                                return Err("Found start of selection more than once, there should be only one".to_owned());
                            }
                        } else if elt.as_span().as_str() == "[" {
                            sel_start_opt = Some((line_nr, col_nr));
                        }
                    }
                    Rule::optSelEnd => {
                        if sel_end_opt.is_some() {
                            if elt.as_span().as_str() == "]" {
                                return Err("Found end of selection more than once, there should be only one".to_owned());
                            }
                        } else if elt.as_span().as_str() == "]" {
                            sel_end_opt = Some((line_nr, col_nr));
                        }
                    }
                    Rule::text => {
                        let split_str = elt
                            .as_span()
                            .as_str()
                            .split('\n')
                            .into_iter()
                            .collect::<Vec<&str>>();

                        if split_str.len() > 1 {
                            line_nr += split_str.len() - 1;
                            col_nr = 0
                        }
                        if let Some(last_str) = split_str.last() {
                            col_nr += last_str.len()
                        }
                    }
                    _ => {}
                }
            }
        }

        // Make sure return makes sense
        if let Some((line, column)) = caret_opt {
            let caret_pos = TextPos { line, column };
            if sel_start_opt.is_none() && sel_end_opt.is_none() {
                Ok(CaretWSelect::new(caret_pos, None))
            } else if let Some((start_line, start_column)) = sel_start_opt {
                if let Some((end_line, end_column)) = sel_end_opt {
                    Ok(
                        CaretWSelect::new(
                            caret_pos,
                            Some(
                                validate_selection(
                                    TextPos {
                                            line: start_line,
                                            column: start_column,
                                        },
                                    TextPos {
                                            line: end_line,
                                            column: end_column,
                                        },
                                ).unwrap()
                            ),
                        )
                    )
                } else {
                    Err("Selection end ']' was not found, but selection start '[' was. Bad input string.".to_owned())
                }
            } else {
                Err("Selection start '[' was not found, but selection end ']' was. Bad input string.".to_owned())
            }
        } else {
            Err("No caret was found in lines.".to_owned())
        }
    }

    // Convert nice string representations and compare results
    fn assert_move(
        pre_lines_str: &[&str],
        expected_post_lines_str: &[&str],
        shift_pressed: bool,
        move_fun: MoveCaretFun,
        arena: &Bump,
    ) -> Result<(), String> {
        let pre_lines: Vec<String> = pre_lines_str.iter().map(|l| l.to_string()).collect();
        let expected_post_lines: Vec<String> = expected_post_lines_str
            .iter()
            .map(|l| l.to_string())
            .collect();

        let clean_text_buf = big_text_from_dsl_str(&pre_lines);

        let caret_w_select =
            move_fun(shift_pressed, &clean_text_buf)?;

        let mut lines_vec = all_lines_vec(&clean_text_buf);
        let post_lines_res = convert_selection_to_dsl(caret_w_select, &mut lines_vec);

        match post_lines_res {
            Ok(post_lines) => {
                assert_eq!(expected_post_lines, post_lines);
                Ok(())
            }
            Err(e) => Err(format!("{:?}", e)),
        }
    }

    #[test]
    fn move_right() -> Result<(), String> {
        let arena = &Bump::new();

        assert_move(&["|"], &["|"], false, move_caret_right, arena)?;
        assert_move(&["a|"], &["a|"], false, move_caret_right, arena)?;
        assert_move(&["|A"], &["A|"], false, move_caret_right, arena)?;
        assert_move(&["|abc"], &["a|bc"], false, move_caret_right, arena)?;
        assert_move(&["a|bc"], &["ab|c"], false, move_caret_right, arena)?;
        assert_move(&["abc|"], &["abc|"], false, move_caret_right, arena)?;
        assert_move(&["| abc"], &[" |abc"], false, move_caret_right, arena)?;
        assert_move(&["abc| "], &["abc |"], false, move_caret_right, arena)?;
        assert_move(&["abc|\n", "d"], &["abc\n", "|d"], false, move_caret_right, arena)?;
        assert_move(&["abc|\n", ""], &["abc\n", "|"], false, move_caret_right, arena)?;
        assert_move(
            &["abc\n", "|def"],
            &["abc\n", "d|ef"],
            false,
            move_caret_right,
            arena,
        )?;
        assert_move(
            &["abc\n", "def| "],
            &["abc\n", "def |"],
            false,
            move_caret_right,
            arena,
        )?;
        assert_move(
            &["abc\n", "def |\n", "ghi"],
            &["abc\n", "def \n", "|ghi"],
            false,
            move_caret_right,
            arena,
        )?;
        assert_move(
            &["abc\n", "def|\n", ""],
            &["abc\n", "def\n", "|"],
            false,
            move_caret_right,
            arena,
        )?;
        assert_move(
            &["abc\n", "def\n", "ghi|\n", "jkl"],
            &["abc\n", "def\n", "ghi\n", "|jkl"],
            false,
            move_caret_right,
            arena,
        )?;
        assert_move(
            &["abc\n", "def\n", "|ghi\n", "jkl"],
            &["abc\n", "def\n", "g|hi\n", "jkl"],
            false,
            move_caret_right,
            arena,
        )?;
        assert_move(
            &["abc\n", "def\n", "g|hi\n", "jkl"],
            &["abc\n", "def\n", "gh|i\n", "jkl"],
            false,
            move_caret_right,
            arena,
        )?;

        Ok(())
    }

    #[test]
    fn move_left() -> Result<(), String> {
        let arena = &Bump::new();

        assert_move(&["|"], &["|"], false, move_caret_left, arena)?;
        assert_move(&["|a"], &["|a"], false, move_caret_left, arena)?;
        assert_move(&["|A"], &["|A"], false, move_caret_left, arena)?;
        assert_move(&["a|bc"], &["|abc"], false, move_caret_left, arena)?;
        assert_move(&["ab|c"], &["a|bc"], false, move_caret_left, arena)?;
        assert_move(&["abc|"], &["ab|c"], false, move_caret_left, arena)?;
        assert_move(&[" |abc"], &["| abc"], false, move_caret_left, arena)?;
        assert_move(&["abc |"], &["abc| "], false, move_caret_left, arena)?;
        assert_move(&["abc\n", "|d"], &["abc|\n", "d"], false, move_caret_left, arena)?;
        assert_move(&["abc\n", "|"], &["abc|\n", ""], false, move_caret_left, arena)?;
        assert_move(
            &["abc\n", "d|ef"],
            &["abc\n", "|def"],
            false,
            move_caret_left,
            arena,
        )?;
        assert_move(
            &["abc\n", "def |"],
            &["abc\n", "def| "],
            false,
            move_caret_left,
            arena,
        )?;
        assert_move(
            &["abc\n", "def \n", "|ghi"],
            &["abc\n", "def |\n", "ghi"],
            false,
            move_caret_left,
            arena,
        )?;
        assert_move(
            &["abc\n", "def\n", "|"],
            &["abc\n", "def|\n", ""],
            false,
            move_caret_left,
            arena,
        )?;
        assert_move(
            &["abc\n", "def\n", "ghi\n", "|jkl"],
            &["abc\n", "def\n", "ghi|\n", "jkl"],
            false,
            move_caret_left,
            arena,
        )?;
        assert_move(
            &["abc\n", "def\n", "g|hi\n", "jkl"],
            &["abc\n", "def\n", "|ghi\n", "jkl"],
            false,
            move_caret_left,
            arena,
        )?;
        assert_move(
            &["abc\n", "def\n", "gh|i\n", "jkl"],
            &["abc\n", "def\n", "g|hi\n", "jkl"],
            false,
            move_caret_left,
            arena,
        )?;

        Ok(())
    }

    #[test]
    fn move_up() -> Result<(), String> {
        let arena = &Bump::new();

        assert_move(&["|"], &["|"], false, move_caret_up, arena)?;
        assert_move(&["|a"], &["|a"], false, move_caret_up, arena)?;
        assert_move(&["A|"], &["|A"], false, move_caret_up, arena)?;
        assert_move(&["a|bc"], &["|abc"], false, move_caret_up, arena)?;
        assert_move(&["ab|c"], &["|abc"], false, move_caret_up, arena)?;
        assert_move(&["abc|"], &["|abc"], false, move_caret_up, arena)?;
        assert_move(&["|abc\n", "def"], &["|abc\n", "def"], false, move_caret_up, arena)?;
        assert_move(&["abc\n", "|def"], &["|abc\n", "def"], false, move_caret_up, arena)?;
        assert_move(&["abc\n", "d|ef"], &["a|bc\n", "def"], false, move_caret_up, arena)?;
        assert_move(&["abc\n", "de|f"], &["ab|c\n", "def"], false, move_caret_up, arena)?;
        assert_move(&["abc\n", "def|"], &["abc|\n", "def"], false, move_caret_up, arena)?;
        assert_move(
            &["abc\n", "def \n", "|ghi"],
            &["abc\n", "|def \n", "ghi"],
            false,
            move_caret_up,
            arena,
        )?;
        assert_move(
            &["abc\n", "def \n", "g|hi"],
            &["abc\n", "d|ef \n", "ghi"],
            false,
            move_caret_up,
            arena,
        )?;
        assert_move(
            &["abc\n", "def \n", "gh|i"],
            &["abc\n", "de|f \n", "ghi"],
            false,
            move_caret_up,
            arena,
        )?;
        assert_move(
            &["abc\n", "def \n", "ghi|"],
            &["abc\n", "def| \n", "ghi"],
            false,
            move_caret_up,
            arena,
        )?;
        assert_move(
            &["abc\n", "de\n", "ghi|"],
            &["abc\n", "de|\n", "ghi"],
            false,
            move_caret_up,
            arena,
        )?;
        assert_move(&["abc\n", "de|"], &["ab|c\n", "de"], false, move_caret_up, arena)?;
        assert_move(&["abc\n", "d|e"], &["a|bc\n", "de"], false, move_caret_up, arena)?;
        assert_move(&["abc\n", "|de"], &["|abc\n", "de"], false, move_caret_up, arena)?;
        assert_move(
            &["ab\n", "cdef\n", "ghijkl\n", "mnopqrst|"],
            &["ab\n", "cdef\n", "ghijkl|\n", "mnopqrst"],
            false,
            move_caret_up,
            arena,
        )?;
        assert_move(
            &["ab\n", "cdef\n", "ghijkl|\n", "mnopqrst"],
            &["ab\n", "cdef|\n", "ghijkl\n", "mnopqrst"],
            false,
            move_caret_up,
            arena,
        )?;
        assert_move(
            &["ab\n", "cdef\n", "ghijkl\n", "|mnopqrst"],
            &["ab\n", "cdef\n", "|ghijkl\n", "mnopqrst"],
            false,
            move_caret_up,
            arena,
        )?;
        assert_move(
            &[" ab\n", " |cdef\n", "ghijkl\n", "mnopqrst"],
            &[" |ab\n", " cdef\n", "ghijkl\n", "mnopqrst"],
            false,
            move_caret_up,
            arena,
        )?;
        assert_move(
            &["ab\n", "cdef\n", "ghijkl\n", "mnopqr|st"],
            &["ab\n", "cdef\n", "ghijkl|\n", "mnopqrst"],
            false,
            move_caret_up,
            arena,
        )?;
        assert_move(
            &["ab\n", "cde|f\n", "ghijkl\n", "mnopqrst"],
            &["ab|\n", "cdef\n", "ghijkl\n", "mnopqrst"],
            false,
            move_caret_up,
            arena,
        )?;
        assert_move(
            &["abcdefgh\n", "ijklmn\n", "opqr\n", "st|"],
            &["abcdefgh\n", "ijklmn\n", "op|qr\n", "st"],
            false,
            move_caret_up,
            arena,
        )?;
        assert_move(
            &["abcdefgh\n", "ijklmn\n", "opqr|\n", "st"],
            &["abcdefgh\n", "ijkl|mn\n", "opqr\n", "st"],
            false,
            move_caret_up,
            arena,
        )?;
        assert_move(
            &["abcdefgh\n", "ijklmn|\n", "opqr\n", "st"],
            &["abcdef|gh\n", "ijklmn\n", "opqr\n", "st"],
            false,
            move_caret_up,
            arena,
        )?;
        assert_move(
            &["abcdefgh|\n", "ijklmn\n", "opqr\n", "st"],
            &["|abcdefgh\n", "ijklmn\n", "opqr\n", "st"],
            false,
            move_caret_up,
            arena,
        )?;
        assert_move(
            &["abcdefg|h\n", "ijklmn\n", "opqr\n", "st"],
            &["|abcdefgh\n", "ijklmn\n", "opqr\n", "st"],
            false,
            move_caret_up,
            arena,
        )?;
        assert_move(
            &["a|bcdefgh\n", "ijklmn\n", "opqr\n", "st"],
            &["|abcdefgh\n", "ijklmn\n", "opqr\n", "st"],
            false,
            move_caret_up,
            arena,
        )?;
        assert_move(
            &["|abcdefgh\n", "ijklmn\n", "opqr\n", "st"],
            &["|abcdefgh\n", "ijklmn\n", "opqr\n", "st"],
            false,
            move_caret_up,
            arena,
        )?;
        assert_move(&["abc def gh |"], &["|abc def gh "], false, move_caret_up, arena)?;
        assert_move(&["abc de|f gh "], &["|abc def gh "], false, move_caret_up, arena)?;
        assert_move(&["ab|c def gh "], &["|abc def gh "], false, move_caret_up, arena)?;
        assert_move(&["a|bc def gh "], &["|abc def gh "], false, move_caret_up, arena)?;

        Ok(())
    }

    #[test]
    fn move_down() -> Result<(), String> {
        let arena = &Bump::new();
        
        assert_move(&["|"], &["|"], false, move_caret_down, arena)?;
        assert_move(&["|a"], &["a|"], false, move_caret_down, arena)?;
        assert_move(&["A|"], &["A|"], false, move_caret_down, arena)?;
        assert_move(&["a|bc"], &["abc|"], false, move_caret_down, arena)?;
        assert_move(&["ab|c"], &["abc|"], false, move_caret_down, arena)?;
        assert_move(&["abc|"], &["abc|"], false, move_caret_down, arena)?;
        assert_move(&["abc| "], &["abc |"], false, move_caret_down, arena)?;
        assert_move(
            &["abc\n", "|def"],
            &["abc\n", "def|"],
            false,
            move_caret_down,
            arena,
        )?;
        assert_move(
            &["abc\n", "d|ef"],
            &["abc\n", "def|"],
            false,
            move_caret_down,
            arena,
        )?;
        assert_move(
            &["abc\n", "de|f"],
            &["abc\n", "def|"],
            false,
            move_caret_down,
            arena,
        )?;
        assert_move(
            &["abc\n", "def|"],
            &["abc\n", "def|"],
            false,
            move_caret_down,
            arena,
        )?;
        assert_move(
            &["|abc\n", "def"],
            &["abc\n", "|def"],
            false,
            move_caret_down,
            arena,
        )?;
        assert_move(
            &["a|bc\n", "def"],
            &["abc\n", "d|ef"],
            false,
            move_caret_down,
            arena,
        )?;
        assert_move(
            &["ab|c\n", "def"],
            &["abc\n", "de|f"],
            false,
            move_caret_down,
            arena,
        )?;
        assert_move(
            &["abc|\n", "def"],
            &["abc\n", "def|"],
            false,
            move_caret_down,
            arena,
        )?;
        assert_move(
            &["abc\n", "|def \n", "ghi"],
            &["abc\n", "def \n", "|ghi"],
            false,
            move_caret_down,
            arena,
        )?;
        assert_move(
            &["abc\n", "d|ef \n", "ghi"],
            &["abc\n", "def \n", "g|hi"],
            false,
            move_caret_down,
            arena,
        )?;
        assert_move(
            &["abc\n", "de|f \n", "ghi"],
            &["abc\n", "def \n", "gh|i"],
            false,
            move_caret_down,
            arena,
        )?;
        assert_move(
            &["abc\n", "def| \n", "ghi"],
            &["abc\n", "def \n", "ghi|"],
            false,
            move_caret_down,
            arena,
        )?;
        assert_move(
            &["abc\n", "def |\n", "ghi"],
            &["abc\n", "def \n", "ghi|"],
            false,
            move_caret_down,
            arena,
        )?;
        assert_move(
            &["abc\n", "de|\n", "ghi"],
            &["abc\n", "de\n", "gh|i"],
            false,
            move_caret_down,
            arena,
        )?;
        assert_move(&["abc|\n", "de"], &["abc\n", "de|"], false, move_caret_down, arena)?;
        assert_move(&["ab|c\n", "de"], &["abc\n", "de|"], false, move_caret_down, arena)?;
        assert_move(&["a|bc\n", "de"], &["abc\n", "d|e"], false, move_caret_down, arena)?;
        assert_move(&["|abc\n", "de"], &["abc\n", "|de"], false, move_caret_down, arena)?;
        assert_move(
            &["ab|\n", "cdef\n", "ghijkl\n", "mnopqrst"],
            &["ab\n", "cd|ef\n", "ghijkl\n", "mnopqrst"],
            false,
            move_caret_down,
            arena,
        )?;
        assert_move(
            &["ab\n", "cdef|\n", "ghijkl\n", "mnopqrst"],
            &["ab\n", "cdef\n", "ghij|kl\n", "mnopqrst"],
            false,
            move_caret_down,
            arena,
        )?;
        assert_move(
            &["ab\n", "cdef\n", "ghijkl|\n", "mnopqrst"],
            &["ab\n", "cdef\n", "ghijkl\n", "mnopqr|st"],
            false,
            move_caret_down,
            arena,
        )?;
        assert_move(
            &[" |ab\n", " cdef\n", "ghijkl\n", "mnopqrst"],
            &[" ab\n", " |cdef\n", "ghijkl\n", "mnopqrst"],
            false,
            move_caret_down,
            arena,
        )?;
        assert_move(
            &["ab\n", "|cdef\n", "ghijkl\n", "mnopqrst"],
            &["ab\n", "cdef\n", "|ghijkl\n", "mnopqrst"],
            false,
            move_caret_down,
            arena,
        )?;
        assert_move(
            &["ab\n", "cdef\n", "|ghijkl\n", "mnopqrst"],
            &["ab\n", "cdef\n", "ghijkl\n", "|mnopqrst"],
            false,
            move_caret_down,
            arena,
        )?;
        assert_move(
            &["abcdefgh|\n", "ijklmn\n", "opqr\n", "st"],
            &["abcdefgh\n", "ijklmn|\n", "opqr\n", "st"],
            false,
            move_caret_down,
            arena,
        )?;
        assert_move(
            &["abcdefgh\n", "ijklmn|\n", "opqr\n", "st"],
            &["abcdefgh\n", "ijklmn\n", "opqr|\n", "st"],
            false,
            move_caret_down,
            arena,
        )?;
        assert_move(
            &["abcdefgh\n", "ijklmn\n", "opqr|\n", "st"],
            &["abcdefgh\n", "ijklmn\n", "opqr\n", "st|"],
            false,
            move_caret_down,
            arena,
        )?;
        assert_move(
            &["abcdefgh\n", "ijklmn\n", "opqr\n", "|st"],
            &["abcdefgh\n", "ijklmn\n", "opqr\n", "st|"],
            false,
            move_caret_down,
            arena,
        )?;
        assert_move(&["abc def gh |"], &["abc def gh |"], false, move_caret_down, arena)?;
        assert_move(&["abc de|f gh "], &["abc def gh |"], false, move_caret_down, arena)?;
        assert_move(&["ab|c def gh "], &["abc def gh |"], false, move_caret_down, arena)?;
        assert_move(&["a|bc def gh "], &["abc def gh |"], false, move_caret_down, arena)?;
        assert_move(&["|abc def gh "], &["abc def gh |"], false, move_caret_down, arena)?;

        Ok(())
    }

    #[test]
    fn start_selection_right() -> Result<(), String> {
        let arena = &Bump::new();

        assert_move(&["|"], &["|"], true, move_caret_right, arena)?;
        assert_move(&["a|"], &["a|"], true, move_caret_right, arena)?;
        assert_move(&["|A"], &["[A]|"], true, move_caret_right, arena)?;
        assert_move(&["|abc"], &["[a]|bc"], true, move_caret_right, arena)?;
        assert_move(&["a|bc"], &["a[b]|c"], true, move_caret_right, arena)?;
        assert_move(&["abc|"], &["abc|"], true, move_caret_right, arena)?;
        assert_move(&["| abc"], &["[ ]|abc"], true, move_caret_right, arena)?;
        assert_move(&["abc| "], &["abc[ ]|"], true, move_caret_right, arena)?;
        assert_move(&["abc|\n", "d"], &["abc[\n", "]|d"], true, move_caret_right, arena)?;
        assert_move(&["abc|\n", ""], &["abc[\n", "]|"], true, move_caret_right, arena)?;
        assert_move(
            &["abc\n", "|def"],
            &["abc\n", "[d]|ef"],
            true,
            move_caret_right,
            arena
        )?;
        assert_move(
            &["abc\n", "def| "],
            &["abc\n", "def[ ]|"],
            true,
            move_caret_right,
            arena
        )?;
        assert_move(
            &["abc\n", "def |\n", "ghi"],
            &["abc\n", "def [\n", "]|ghi"],
            true,
            move_caret_right,
            arena
        )?;
        assert_move(
            &["abc\n", "def|\n", ""],
            &["abc\n", "def[\n", "]|"],
            true,
            move_caret_right,
            arena
        )?;
        assert_move(
            &["abc\n", "def\n", "ghi|\n", "jkl"],
            &["abc\n", "def\n", "ghi[\n", "]|jkl"],
            true,
            move_caret_right,
            arena
        )?;
        assert_move(
            &["abc\n", "def\n", "|ghi\n", "jkl"],
            &["abc\n", "def\n", "[g]|hi\n", "jkl"],
            true,
            move_caret_right,
            arena
        )?;
        assert_move(
            &["abc\n", "def\n", "g|hi\n", "jkl"],
            &["abc\n", "def\n", "g[h]|i\n", "jkl"],
            true,
            move_caret_right,
            arena
        )?;

        Ok(())
    }

    #[test]
    fn start_selection_left() -> Result<(), String> {
        let arena = &Bump::new();

        assert_move(&["|"], &["|"], true, move_caret_left, arena)?;
        assert_move(&["a|"], &["|[a]"], true, move_caret_left, arena)?;
        assert_move(&["|A"], &["|A"], true, move_caret_left, arena)?;
        assert_move(&["|abc"], &["|abc"], true, move_caret_left, arena)?;
        assert_move(&["a|bc"], &["|[a]bc"], true, move_caret_left, arena)?;
        assert_move(&["abc|"], &["ab|[c]"], true, move_caret_left, arena)?;
        assert_move(&[" |abc"], &["|[ ]abc"], true, move_caret_left, arena)?;
        assert_move(&["abc |"], &["abc|[ ]"], true, move_caret_left, arena)?;
        assert_move(&["abc|\n", "d"], &["ab|[c]\n", "d"], true, move_caret_left, arena)?;
        assert_move(&["abc\n", "|d"], &["abc|[\n", "]d"], true, move_caret_left, arena)?;
        assert_move(&["abc\n", "|"], &["abc|[\n", "]"], true, move_caret_left, arena)?;
        assert_move(
            &["abc\n", " |def"],
            &["abc\n", "|[ ]def"],
            true,
            move_caret_left,
            arena
        )?;
        assert_move(
            &["abc\n", "d|ef"],
            &["abc\n", "|[d]ef"],
            true,
            move_caret_left,
            arena
        )?;
        assert_move(
            &["abc\n", "de|f "],
            &["abc\n", "d|[e]f "],
            true,
            move_caret_left,
            arena
        )?;
        assert_move(
            &["abc\n", "def\n", "|"],
            &["abc\n", "def|[\n", "]"],
            true,
            move_caret_left,
            arena
        )?;
        assert_move(
            &["abc\n", "def\n", "|ghi\n", "jkl"],
            &["abc\n", "def|[\n", "]ghi\n", "jkl"],
            true,
            move_caret_left,
            arena
        )?;
        assert_move(
            &["abc\n", "def\n", "g|hi\n", "jkl"],
            &["abc\n", "def\n", "|[g]hi\n", "jkl"],
            true,
            move_caret_left,
            arena
        )?;
        assert_move(
            &["abc\n", "def\n", "gh|i\n", "jkl"],
            &["abc\n", "def\n", "g|[h]i\n", "jkl"],
            true,
            move_caret_left,
            arena
        )?;
        assert_move(
            &["abc\n", "def\n", "ghi|\n", "jkl"],
            &["abc\n", "def\n", "gh|[i]\n", "jkl"],
            true,
            move_caret_left,
            arena
        )?;

        Ok(())
    }

    #[test]
    fn start_selection_down() -> Result<(), String> {
        let arena = &Bump::new();

        assert_move(&["|"], &["|"], true, move_caret_down, arena)?;
        assert_move(&["|a"], &["[a]|"], true, move_caret_down, arena)?;
        assert_move(&["A|"], &["A|"], true, move_caret_down, arena)?;
        assert_move(&["a|bc"], &["a[bc]|"], true, move_caret_down, arena)?;
        assert_move(&["ab|c"], &["ab[c]|"], true, move_caret_down, arena)?;
        assert_move(&["abc|"], &["abc|"], true, move_caret_down, arena)?;
        assert_move(&["abc| "], &["abc[ ]|"], true, move_caret_down, arena)?;
        assert_move(
            &["abc\n", "|def"],
            &["abc\n", "[def]|"],
            true,
            move_caret_down,
            arena,
        )?;
        assert_move(
            &["abc\n", "d|ef"],
            &["abc\n", "d[ef]|"],
            true,
            move_caret_down,
            arena,
        )?;
        assert_move(
            &["abc\n", "de|f"],
            &["abc\n", "de[f]|"],
            true,
            move_caret_down,
            arena,
        )?;
        assert_move(
            &["abc\n", "def|"],
            &["abc\n", "def|"],
            true,
            move_caret_down,
            arena,
        )?;
        assert_move(
            &["|abc\n", "def"],
            &["[abc\n", "]|def"],
            true,
            move_caret_down,
            arena,
        )?;
        assert_move(
            &["a|bc\n", "def"],
            &["a[bc\n", "d]|ef"],
            true,
            move_caret_down,
            arena,
        )?;
        assert_move(
            &["ab|c\n", "def"],
            &["ab[c\n", "de]|f"],
            true,
            move_caret_down,
            arena,
        )?;
        assert_move(
            &["abc|\n", "def"],
            &["abc[\n", "def]|"],
            true,
            move_caret_down,
            arena,
        )?;
        assert_move(
            &["abc\n", "|def \n", "ghi"],
            &["abc\n", "[def \n", "]|ghi"],
            true,
            move_caret_down,
            arena,
        )?;
        assert_move(
            &["abc\n", "d|ef \n", "ghi"],
            &["abc\n", "d[ef \n", "g]|hi"],
            true,
            move_caret_down,
            arena,
        )?;
        assert_move(
            &["abc\n", "de|f \n", "ghi"],
            &["abc\n", "de[f \n", "gh]|i"],
            true,
            move_caret_down,
            arena,
        )?;
        assert_move(
            &["abc\n", "def| \n", "ghi"],
            &["abc\n", "def[ \n", "ghi]|"],
            true,
            move_caret_down,
            arena,
        )?;
        assert_move(
            &["abc\n", "def |\n", "ghi"],
            &["abc\n", "def [\n", "ghi]|"],
            true,
            move_caret_down,
            arena,
        )?;
        assert_move(
            &["abc\n", "de|\n", "ghi"],
            &["abc\n", "de[\n", "gh]|i"],
            true,
            move_caret_down,
            arena,
        )?;
        assert_move(
            &["abc|\n", "de"],
            &["abc[\n", "de]|"],
            true,
            move_caret_down,
            arena,
        )?;
        assert_move(
            &["ab|c\n", "de"],
            &["ab[c\n", "de]|"],
            true,
            move_caret_down,
            arena,
        )?;
        assert_move(
            &["a|bc\n", "de"],
            &["a[bc\n", "d]|e"],
            true,
            move_caret_down,
            arena,
        )?;
        assert_move(
            &["|abc\n", "de"],
            &["[abc\n", "]|de"],
            true,
            move_caret_down,
            arena,
        )?;
        assert_move(
            &["ab|\n", "cdef\n", "ghijkl\n", "mnopqrst"],
            &["ab[\n", "cd]|ef\n", "ghijkl\n", "mnopqrst"],
            true,
            move_caret_down,
            arena,
        )?;
        assert_move(
            &["ab\n", "cdef|\n", "ghijkl\n", "mnopqrst"],
            &["ab\n", "cdef[\n", "ghij]|kl\n", "mnopqrst"],
            true,
            move_caret_down,
            arena,
        )?;
        assert_move(
            &["ab\n", "cdef\n", "ghijkl|\n", "mnopqrst"],
            &["ab\n", "cdef\n", "ghijkl[\n", "mnopqr]|st"],
            true,
            move_caret_down,
            arena,
        )?;
        assert_move(
            &[" |ab\n", " cdef\n", "ghijkl\n", "mnopqrst"],
            &[" [ab\n", " ]|cdef\n", "ghijkl\n", "mnopqrst"],
            true,
            move_caret_down,
            arena,
        )?;
        assert_move(
            &["ab\n", "|cdef\n", "ghijkl\n", "mnopqrst"],
            &["ab\n", "[cdef\n", "]|ghijkl\n", "mnopqrst"],
            true,
            move_caret_down,
            arena,
        )?;
        assert_move(
            &["ab\n", "cdef\n", "|ghijkl\n", "mnopqrst"],
            &["ab\n", "cdef\n", "[ghijkl\n", "]|mnopqrst"],
            true,
            move_caret_down,
            arena,
        )?;
        assert_move(
            &["abcdefgh|\n", "ijklmn\n", "opqr\n", "st"],
            &["abcdefgh[\n", "ijklmn]|\n", "opqr\n", "st"],
            true,
            move_caret_down,
            arena,
        )?;
        assert_move(
            &["abcdefgh\n", "ijklmn|\n", "opqr\n", "st"],
            &["abcdefgh\n", "ijklmn[\n", "opqr]|\n", "st"],
            true,
            move_caret_down,
            arena,
        )?;
        assert_move(
            &["abcdefgh\n", "ijklmn\n", "opqr|\n", "st"],
            &["abcdefgh\n", "ijklmn\n", "opqr[\n", "st]|"],
            true,
            move_caret_down,
            arena,
        )?;
        assert_move(
            &["abcdefgh\n", "ijklmn\n", "opqr\n", "|st"],
            &["abcdefgh\n", "ijklmn\n", "opqr\n", "[st]|"],
            true,
            move_caret_down,
            arena,
        )?;
        assert_move(&["abc def gh |"], &["abc def gh |"], true, move_caret_down, arena)?;
        assert_move(
            &["abc de|f gh "],
            &["abc de[f gh ]|"],
            true,
            move_caret_down,
            arena,
        )?;
        assert_move(
            &["ab|c def gh "],
            &["ab[c def gh ]|"],
            true,
            move_caret_down,
            arena,
        )?;
        assert_move(
            &["a|bc def gh "],
            &["a[bc def gh ]|"],
            true,
            move_caret_down,
            arena,
        )?;
        assert_move(
            &["|abc def gh "],
            &["[abc def gh ]|"],
            true,
            move_caret_down,
            arena,
        )?;

        Ok(())
    }

    #[test]
    fn start_selection_up() -> Result<(), String> {
        let arena = &Bump::new();

        assert_move(&["|"], &["|"], true, move_caret_up, arena)?;
        assert_move(&["|a"], &["|a"], true, move_caret_up, arena)?;
        assert_move(&["A|"], &["|[A]"], true, move_caret_up, arena)?;
        assert_move(&["a|bc"], &["|[a]bc"], true, move_caret_up, arena)?;
        assert_move(&["ab|c"], &["|[ab]c"], true, move_caret_up, arena)?;
        assert_move(&["abc|"], &["|[abc]"], true, move_caret_up, arena)?;
        assert_move(&["|abc\n", "def"], &["|abc\n", "def"], true, move_caret_up, arena)?;
        assert_move(
            &["abc\n", "|def"],
            &["|[abc\n", "]def"],
            true,
            move_caret_up,
            arena,
        )?;
        assert_move(
            &["abc\n", "d|ef"],
            &["a|[bc\n", "d]ef"],
            true,
            move_caret_up,
            arena,
        )?;
        assert_move(
            &["abc\n", "de|f"],
            &["ab|[c\n", "de]f"],
            true,
            move_caret_up,
            arena,
        )?;
        assert_move(
            &["abc\n", "def|"],
            &["abc|[\n", "def]"],
            true,
            move_caret_up,
            arena,
        )?;
        assert_move(
            &["abc\n", "def \n", "|ghi"],
            &["abc\n", "|[def \n", "]ghi"],
            true,
            move_caret_up,
            arena,
        )?;
        assert_move(
            &["abc\n", "def \n", "g|hi"],
            &["abc\n", "d|[ef \n", "g]hi"],
            true,
            move_caret_up,
            arena,
        )?;
        assert_move(
            &["abc\n", "def \n", "gh|i"],
            &["abc\n", "de|[f \n", "gh]i"],
            true,
            move_caret_up,
            arena,
        )?;
        assert_move(
            &["abc\n", "def \n", "ghi|"],
            &["abc\n", "def|[ \n", "ghi]"],
            true,
            move_caret_up,
            arena,
        )?;
        assert_move(
            &["abc\n", "de\n", "ghi|"],
            &["abc\n", "de|[\n", "ghi]"],
            true,
            move_caret_up,
            arena,
        )?;
        assert_move(&["abc\n", "de|"], &["ab|[c\n", "de]"], true, move_caret_up, arena)?;
        assert_move(&["abc\n", "d|e"], &["a|[bc\n", "d]e"], true, move_caret_up, arena)?;
        assert_move(&["abc\n", "|de"], &["|[abc\n", "]de"], true, move_caret_up, arena)?;
        assert_move(
            &["ab\n", "cdef\n", "ghijkl\n", "mnopqrst|"],
            &["ab\n", "cdef\n", "ghijkl|[\n", "mnopqrst]"],
            true,
            move_caret_up,
            arena,
        )?;
        assert_move(
            &["ab\n", "cdef\n", "ghijkl|\n", "mnopqrst"],
            &["ab\n", "cdef|[\n", "ghijkl]\n", "mnopqrst"],
            true,
            move_caret_up,
            arena,
        )?;
        assert_move(
            &["ab\n", "cdef\n", "ghijkl\n", "|mnopqrst"],
            &["ab\n", "cdef\n", "|[ghijkl\n", "]mnopqrst"],
            true,
            move_caret_up,
            arena,
        )?;
        assert_move(
            &[" ab\n", " |cdef\n", "ghijkl\n", "mnopqrst"],
            &[" |[ab\n", " ]cdef\n", "ghijkl\n", "mnopqrst"],
            true,
            move_caret_up,
            arena,
        )?;
        assert_move(
            &["ab\n", "cdef\n", "ghijkl\n", "mnopqr|st"],
            &["ab\n", "cdef\n", "ghijkl|[\n", "mnopqr]st"],
            true,
            move_caret_up,
            arena,
        )?;
        assert_move(
            &["ab\n", "cde|f\n", "ghijkl\n", "mnopqrst"],
            &["ab|[\n", "cde]f\n", "ghijkl\n", "mnopqrst"],
            true,
            move_caret_up,
            arena,
        )?;
        assert_move(
            &["abcdefgh\n", "ijklmn\n", "opqr\n", "st|"],
            &["abcdefgh\n", "ijklmn\n", "op|[qr\n", "st]"],
            true,
            move_caret_up,
            arena,
        )?;
        assert_move(
            &["abcdefgh\n", "ijklmn\n", "opqr|\n", "st"],
            &["abcdefgh\n", "ijkl|[mn\n", "opqr]\n", "st"],
            true,
            move_caret_up,
            arena,
        )?;
        assert_move(
            &["abcdefgh\n", "ijklmn|\n", "opqr\n", "st"],
            &["abcdef|[gh\n", "ijklmn]\n", "opqr\n", "st"],
            true,
            move_caret_up,
            arena,
        )?;
        assert_move(
            &["abcdefgh|\n", "ijklmn\n", "opqr\n", "st"],
            &["|[abcdefgh]\n", "ijklmn\n", "opqr\n", "st"],
            true,
            move_caret_up,
            arena,
        )?;
        assert_move(
            &["abcdefg|h\n", "ijklmn\n", "opqr\n", "st"],
            &["|[abcdefg]h\n", "ijklmn\n", "opqr\n", "st"],
            true,
            move_caret_up,
            arena,
        )?;
        assert_move(
            &["a|bcdefgh\n", "ijklmn\n", "opqr\n", "st"],
            &["|[a]bcdefgh\n", "ijklmn\n", "opqr\n", "st"],
            true,
            move_caret_up,
            arena,
        )?;
        assert_move(
            &["|abcdefgh\n", "ijklmn\n", "opqr\n", "st"],
            &["|abcdefgh\n", "ijklmn\n", "opqr\n", "st"],
            true,
            move_caret_up,
            arena,
        )?;
        assert_move(&["abc def gh |"], &["|[abc def gh ]"], true, move_caret_up, arena)?;
        assert_move(&["abc de|f gh "], &["|[abc de]f gh "], true, move_caret_up, arena)?;
        assert_move(&["ab|c def gh "], &["|[ab]c def gh "], true, move_caret_up, arena)?;
        assert_move(&["a|bc def gh "], &["|[a]bc def gh "], true, move_caret_up, arena)?;

        Ok(())
    }

    #[test]
    fn end_selection_right() -> Result<(), String> {
        let arena = &Bump::new();

        assert_move(&["[A]|"], &["A|"], false, move_caret_right, arena)?;
        assert_move(&["[a]|bc"], &["a|bc"], false, move_caret_right, arena)?;
        assert_move(&["a[b]|c"], &["ab|c"], false, move_caret_right, arena)?;
        assert_move(&["ab[c]|"], &["abc|"], false, move_caret_right, arena)?;
        assert_move(&["[ ]|abc"], &[" |abc"], false, move_caret_right, arena)?;
        assert_move(&["|[ ]abc"], &[" |abc"], false, move_caret_right, arena)?;
        assert_move(&["a|[b]c"], &["ab|c"], false, move_caret_right, arena)?;
        assert_move(
            &["abc[\n", "]|d"],
            &["abc\n", "|d"],
            false,
            move_caret_right,
            arena,
        )?;
        assert_move(
            &["abc|[\n", "]d"],
            &["abc\n", "|d"],
            false,
            move_caret_right,
            arena,
        )?;
        assert_move(&["abc|[\n", "]"], &["abc\n", "|"], false, move_caret_right, arena)?;
        assert_move(
            &["abc\n", "[d]|ef"],
            &["abc\n", "d|ef"],
            false,
            move_caret_right,
            arena,
        )?;
        assert_move(
            &["abc\n", "def\n", "ghi[\n", "]|jkl"],
            &["abc\n", "def\n", "ghi\n", "|jkl"],
            false,
            move_caret_right,
            arena,
        )?;
        assert_move(&["[ab]|c"], &["ab|c"], false, move_caret_right, arena)?;
        assert_move(&["[abc]|"], &["abc|"], false, move_caret_right, arena)?;
        assert_move(
            &["ab|[c\n", "]def\n", "ghi"],
            &["abc\n", "|def\n", "ghi"],
            false,
            move_caret_right,
            arena,
        )?;
        assert_move(
            &["ab[c\n", "]|def\n", "ghi"],
            &["abc\n", "|def\n", "ghi"],
            false,
            move_caret_right,
            arena,
        )?;
        assert_move(
            &["a|[bc\n", "]def\n", "ghi"],
            &["abc\n", "|def\n", "ghi"],
            false,
            move_caret_right,
            arena,
        )?;
        assert_move(
            &["|[abc\n", "]def\n", "ghi"],
            &["abc\n", "|def\n", "ghi"],
            false,
            move_caret_right,
            arena,
        )?;
        assert_move(
            &["a|[bc\n", "d]ef\n", "ghi"],
            &["abc\n", "d|ef\n", "ghi"],
            false,
            move_caret_right,
            arena,
        )?;
        assert_move(
            &["|[abc\n", "def]\n", "ghi"],
            &["abc\n", "def|\n", "ghi"],
            false,
            move_caret_right,
            arena,
        )?;
        assert_move(
            &["[ab\n", "cdef\n", "ghijkl\n", "mnopqrst]|"],
            &["ab\n", "cdef\n", "ghijkl\n", "mnopqrst|"],
            false,
            move_caret_right,
            arena,
        )?;
        assert_move(
            &["|[ab\n", "cdef\n", "ghijkl\n", "mnopqrst]"],
            &["ab\n", "cdef\n", "ghijkl\n", "mnopqrst|"],
            false,
            move_caret_right,
            arena,
        )?;
        assert_move(
            &["ab\n", "c[def\n", "ghijkl\n", "mno]|pqrst"],
            &["ab\n", "cdef\n", "ghijkl\n", "mno|pqrst"],
            false,
            move_caret_right,
            arena,
        )?;
        assert_move(
            &["ab\n", "c|[def\n", "ghijkl\n", "mno]pqrst"],
            &["ab\n", "cdef\n", "ghijkl\n", "mno|pqrst"],
            false,
            move_caret_right,
            arena,
        )?;

        Ok(())
    }

    #[test]
    fn end_selection_left() -> Result<(), String> {
        let arena = &Bump::new();

        assert_move(&["[A]|"], &["|A"], false, move_caret_left, arena)?;
        assert_move(&["[a]|bc"], &["|abc"], false, move_caret_left, arena)?;
        assert_move(&["a[b]|c"], &["a|bc"], false, move_caret_left, arena)?;
        assert_move(&["ab[c]|"], &["ab|c"], false, move_caret_left, arena)?;
        assert_move(&["[ ]|abc"], &["| abc"], false, move_caret_left, arena)?;
        assert_move(&["|[ ]abc"], &["| abc"], false, move_caret_left, arena)?;
        assert_move(&["a|[b]c"], &["a|bc"], false, move_caret_left, arena)?;
        assert_move(&["abc[\n", "]|d"], &["abc|\n", "d"], false, move_caret_left, arena)?;
        assert_move(&["abc|[\n", "]d"], &["abc|\n", "d"], false, move_caret_left, arena)?;
        assert_move(&["abc|[\n", "]"], &["abc|\n", ""], false, move_caret_left, arena)?;
        assert_move(
            &["abc\n", "[d]|ef"],
            &["abc\n", "|def"],
            false,
            move_caret_left,
            arena
        )?;
        assert_move(
            &["abc\n", "def\n", "ghi[\n", "]|jkl"],
            &["abc\n", "def\n", "ghi|\n", "jkl"],
            false,
            move_caret_left,
            arena
        )?;
        assert_move(&["[ab]|c"], &["|abc"], false, move_caret_left, arena)?;
        assert_move(&["[abc]|"], &["|abc"], false, move_caret_left, arena)?;
        assert_move(
            &["ab|[c\n", "]def\n", "ghi"],
            &["ab|c\n", "def\n", "ghi"],
            false,
            move_caret_left,
            arena
        )?;
        assert_move(
            &["ab[c\n", "]|def\n", "ghi"],
            &["ab|c\n", "def\n", "ghi"],
            false,
            move_caret_left,
            arena
        )?;
        assert_move(
            &["a|[bc\n", "]def\n", "ghi"],
            &["a|bc\n", "def\n", "ghi"],
            false,
            move_caret_left,
            arena
        )?;
        assert_move(
            &["|[abc\n", "]def\n", "ghi"],
            &["|abc\n", "def\n", "ghi"],
            false,
            move_caret_left,
            arena
        )?;
        assert_move(
            &["a|[bc\n", "d]ef\n", "ghi"],
            &["a|bc\n", "def\n", "ghi"],
            false,
            move_caret_left,
            arena
        )?;
        assert_move(
            &["|[abc\n", "def]\n", "ghi"],
            &["|abc\n", "def\n", "ghi"],
            false,
            move_caret_left,
            arena
        )?;
        assert_move(
            &["[ab\n", "cdef\n", "ghijkl\n", "mnopqrst]|"],
            &["|ab\n", "cdef\n", "ghijkl\n", "mnopqrst"],
            false,
            move_caret_left,
            arena
        )?;
        assert_move(
            &["|[ab\n", "cdef\n", "ghijkl\n", "mnopqrst]"],
            &["|ab\n", "cdef\n", "ghijkl\n", "mnopqrst"],
            false,
            move_caret_left,
            arena
        )?;
        assert_move(
            &["ab\n", "c[def\n", "ghijkl\n", "mno]|pqrst"],
            &["ab\n", "c|def\n", "ghijkl\n", "mnopqrst"],
            false,
            move_caret_left,
            arena
        )?;
        assert_move(
            &["ab\n", "c|[def\n", "ghijkl\n", "mno]pqrst"],
            &["ab\n", "c|def\n", "ghijkl\n", "mnopqrst"],
            false,
            move_caret_left,
            arena
        )?;

        Ok(())
    }

    #[test]
    fn end_selection_down() -> Result<(), String> {
        let arena = &Bump::new();

        assert_move(&["[a]|"], &["a|"], false, move_caret_down, arena)?;
        assert_move(&["|[a]"], &["a|"], false, move_caret_down, arena)?;
        assert_move(&["a|[bc]"], &["abc|"], false, move_caret_down, arena)?;
        assert_move(&["ab[c]|"], &["abc|"], false, move_caret_down, arena)?;
        assert_move(&["abc|[ ]"], &["abc |"], false, move_caret_down, arena)?;
        assert_move(
            &["abc\n", "|[def]"],
            &["abc\n", "def|"],
            false,
            move_caret_down,
            arena
        )?;
        assert_move(
            &["abc\n", "d|[ef]"],
            &["abc\n", "def|"],
            false,
            move_caret_down,
            arena
        )?;
        assert_move(
            &["abc\n", "de|[f]"],
            &["abc\n", "def|"],
            false,
            move_caret_down,
            arena
        )?;
        assert_move(
            &["[abc\n", "]|def"],
            &["abc\n", "|def"],
            false,
            move_caret_down,
            arena
        )?;
        assert_move(
            &["a[bc\n", "d]|ef"],
            &["abc\n", "d|ef"],
            false,
            move_caret_down,
            arena
        )?;
        assert_move(
            &["ab|[c\n", "de]f"],
            &["abc\n", "de|f"],
            false,
            move_caret_down,
            arena
        )?;
        assert_move(
            &["abc[\n", "def]|"],
            &["abc\n", "def|"],
            false,
            move_caret_down,
            arena
        )?;
        assert_move(
            &["abc\n", "|[def \n", "]ghi"],
            &["abc\n", "def \n", "|ghi"],
            false,
            move_caret_down,
            arena
        )?;
        assert_move(
            &["abc\n", "d[ef \n", "g]|hi"],
            &["abc\n", "def \n", "g|hi"],
            false,
            move_caret_down,
            arena
        )?;
        assert_move(
            &["abc\n", "de[f \n", "gh]|i"],
            &["abc\n", "def \n", "gh|i"],
            false,
            move_caret_down,
            arena
        )?;
        assert_move(
            &["abc\n", "def[ \n", "ghi]|"],
            &["abc\n", "def \n", "ghi|"],
            false,
            move_caret_down,
            arena
        )?;
        assert_move(
            &["abc\n", "def [\n", "ghi]|"],
            &["abc\n", "def \n", "ghi|"],
            false,
            move_caret_down,
            arena
        )?;
        assert_move(
            &["abc\n", "de[\n", "gh]|i"],
            &["abc\n", "de\n", "gh|i"],
            false,
            move_caret_down,
            arena
        )?;
        assert_move(
            &["abc|[\n", "de]"],
            &["abc\n", "de|"],
            false,
            move_caret_down,
            arena
        )?;
        assert_move(
            &["ab[c\n", "de]|"],
            &["abc\n", "de|"],
            false,
            move_caret_down,
            arena
        )?;
        assert_move(
            &["a|[bc\n", "d]e"],
            &["abc\n", "d|e"],
            false,
            move_caret_down,
            arena
        )?;
        assert_move(
            &["[abc\n", "]|de"],
            &["abc\n", "|de"],
            false,
            move_caret_down,
            arena
        )?;
        assert_move(
            &["ab[\n", "cd]|ef\n", "ghijkl\n", "mnopqrst"],
            &["ab\n", "cd|ef\n", "ghijkl\n", "mnopqrst"],
            false,
            move_caret_down,
            arena
        )?;
        assert_move(
            &["ab\n", "cdef|[\n", "ghij]kl\n", "mnopqrst"],
            &["ab\n", "cdef\n", "ghij|kl\n", "mnopqrst"],
            false,
            move_caret_down,
            arena
        )?;
        assert_move(
            &["ab\n", "cdef\n", "ghijkl[\n", "mnopqr]|st"],
            &["ab\n", "cdef\n", "ghijkl\n", "mnopqr|st"],
            false,
            move_caret_down,
            arena
        )?;
        assert_move(
            &[" [ab\n", " ]|cdef\n", "ghijkl\n", "mnopqrst"],
            &[" ab\n", " |cdef\n", "ghijkl\n", "mnopqrst"],
            false,
            move_caret_down,
            arena
        )?;
        assert_move(
            &["ab\n", "|[cdef\n", "]ghijkl\n", "mnopqrst"],
            &["ab\n", "cdef\n", "|ghijkl\n", "mnopqrst"],
            false,
            move_caret_down,
            arena
        )?;
        assert_move(
            &["ab\n", "cdef\n", "[ghijkl\n", "]|mnopqrst"],
            &["ab\n", "cdef\n", "ghijkl\n", "|mnopqrst"],
            false,
            move_caret_down,
            arena
        )?;
        assert_move(
            &["abcdefgh[\n", "ijklmn]|\n", "opqr\n", "st"],
            &["abcdefgh\n", "ijklmn|\n", "opqr\n", "st"],
            false,
            move_caret_down,
            arena
        )?;
        assert_move(
            &["abcdefgh\n", "ijklmn[\n", "opqr]|\n", "st"],
            &["abcdefgh\n", "ijklmn\n", "opqr|\n", "st"],
            false,
            move_caret_down,
            arena
        )?;
        assert_move(
            &["abcdefgh\n", "ijklmn\n", "opqr[\n", "st]|"],
            &["abcdefgh\n", "ijklmn\n", "opqr\n", "st|"],
            false,
            move_caret_down,
            arena
        )?;
        assert_move(
            &["abcdefgh\n", "ijklmn\n", "opqr\n", "[st]|"],
            &["abcdefgh\n", "ijklmn\n", "opqr\n", "st|"],
            false,
            move_caret_down,
            arena
        )?;
        assert_move(
            &["abc de[f gh ]|"],
            &["abc def gh |"],
            false,
            move_caret_down,
            arena
        )?;
        assert_move(
            &["ab|[c def gh ]"],
            &["abc def gh |"],
            false,
            move_caret_down,
            arena
        )?;
        assert_move(
            &["a[bc def gh ]|"],
            &["abc def gh |"],
            false,
            move_caret_down,
            arena
        )?;
        assert_move(
            &["[abc def gh ]|"],
            &["abc def gh |"],
            false,
            move_caret_down,
            arena
        )?;

        Ok(())
    }

    #[test]
    fn end_selection_up() -> Result<(), String> {
        let arena = &Bump::new();

        assert_move(&["[a]|"], &["|a"], false, move_caret_up, arena)?;
        assert_move(&["|[a]"], &["|a"], false, move_caret_up, arena)?;
        assert_move(&["a|[bc]"], &["a|bc"], false, move_caret_up, arena)?;
        assert_move(&["ab[c]|"], &["ab|c"], false, move_caret_up, arena)?;
        assert_move(&["abc|[ ]"], &["abc| "], false, move_caret_up, arena)?;
        assert_move(
            &["abc\n", "|[def]"],
            &["abc\n", "|def"],
            false,
            move_caret_up,
            arena
        )?;
        assert_move(
            &["abc\n", "d|[ef]"],
            &["abc\n", "d|ef"],
            false,
            move_caret_up,
            arena
        )?;
        assert_move(
            &["abc\n", "de|[f]"],
            &["abc\n", "de|f"],
            false,
            move_caret_up,
            arena
        )?;
        assert_move(
            &["[abc\n", "]|def"],
            &["|abc\n", "def"],
            false,
            move_caret_up,
            arena
        )?;
        assert_move(
            &["a[bc\n", "d]|ef"],
            &["a|bc\n", "def"],
            false,
            move_caret_up,
            arena
        )?;
        assert_move(
            &["ab|[c\n", "de]f"],
            &["ab|c\n", "def"],
            false,
            move_caret_up,
            arena
        )?;
        assert_move(
            &["abc[\n", "def]|"],
            &["abc|\n", "def"],
            false,
            move_caret_up,
            arena
        )?;
        assert_move(
            &["abc\n", "|[def \n", "]ghi"],
            &["abc\n", "|def \n", "ghi"],
            false,
            move_caret_up,
            arena
        )?;
        assert_move(
            &["abc\n", "d[ef \n", "g]|hi"],
            &["abc\n", "d|ef \n", "ghi"],
            false,
            move_caret_up,
            arena
        )?;
        assert_move(
            &["abc\n", "de|[f \n", "gh]i"],
            &["abc\n", "de|f \n", "ghi"],
            false,
            move_caret_up,
            arena
        )?;
        assert_move(
            &["abc\n", "def[ \n", "ghi]|"],
            &["abc\n", "def| \n", "ghi"],
            false,
            move_caret_up,
            arena
        )?;
        assert_move(
            &["abc\n", "def [\n", "ghi]|"],
            &["abc\n", "def |\n", "ghi"],
            false,
            move_caret_up,
            arena
        )?;
        assert_move(
            &["abc\n", "de[\n", "gh]|i"],
            &["abc\n", "de|\n", "ghi"],
            false,
            move_caret_up,
            arena
        )?;
        assert_move(&["abc|[\n", "de]"], &["abc|\n", "de"], false, move_caret_up, arena)?;
        assert_move(&["ab[c\n", "de]|"], &["ab|c\n", "de"], false, move_caret_up, arena)?;
        assert_move(&["a|[bc\n", "d]e"], &["a|bc\n", "de"], false, move_caret_up, arena)?;
        assert_move(&["[abc\n", "]|de"], &["|abc\n", "de"], false, move_caret_up, arena)?;
        assert_move(
            &["ab[\n", "cd]|ef\n", "ghijkl\n", "mnopqrst"],
            &["ab|\n", "cdef\n", "ghijkl\n", "mnopqrst"],
            false,
            move_caret_up,
            arena
        )?;
        assert_move(
            &["ab\n", "cdef|[\n", "ghij]kl\n", "mnopqrst"],
            &["ab\n", "cdef|\n", "ghijkl\n", "mnopqrst"],
            false,
            move_caret_up,
            arena
        )?;
        assert_move(
            &["ab\n", "cdef\n", "ghijkl[\n", "mnopqr]|st"],
            &["ab\n", "cdef\n", "ghijkl|\n", "mnopqrst"],
            false,
            move_caret_up,
            arena
        )?;
        assert_move(
            &[" [ab\n", " ]|cdef\n", "ghijkl\n", "mnopqrst"],
            &[" |ab\n", " cdef\n", "ghijkl\n", "mnopqrst"],
            false,
            move_caret_up,
            arena
        )?;
        assert_move(
            &["ab\n", "|[cdef\n", "]ghijkl\n", "mnopqrst"],
            &["ab\n", "|cdef\n", "ghijkl\n", "mnopqrst"],
            false,
            move_caret_up,
            arena
        )?;
        assert_move(
            &["ab\n", "cdef\n", "[ghijkl\n", "]|mnopqrst"],
            &["ab\n", "cdef\n", "|ghijkl\n", "mnopqrst"],
            false,
            move_caret_up,
            arena
        )?;
        assert_move(
            &["abcdefgh[\n", "ijklmn]|\n", "opqr\n", "st"],
            &["abcdefgh|\n", "ijklmn\n", "opqr\n", "st"],
            false,
            move_caret_up,
            arena
        )?;
        assert_move(
            &["abcdefgh\n", "ijklmn[\n", "opqr]|\n", "st"],
            &["abcdefgh\n", "ijklmn|\n", "opqr\n", "st"],
            false,
            move_caret_up,
            arena
        )?;
        assert_move(
            &["abcdefgh\n", "ijklmn\n", "opqr[\n", "st]|"],
            &["abcdefgh\n", "ijklmn\n", "opqr|\n", "st"],
            false,
            move_caret_up,
            arena
        )?;
        assert_move(
            &["abcdefgh\n", "ijklmn\n", "opqr\n", "[st]|"],
            &["abcdefgh\n", "ijklmn\n", "opqr\n", "|st"],
            false,
            move_caret_up,
            arena
        )?;
        assert_move(&["abc de[f gh ]|"], &["abc de|f gh "], false, move_caret_up, arena)?;
        assert_move(&["ab|[c def gh ]"], &["ab|c def gh "], false, move_caret_up, arena)?;
        assert_move(&["a[bc def gh ]|"], &["a|bc def gh "], false, move_caret_up, arena)?;
        assert_move(&["[abc def gh ]|"], &["|abc def gh "], false, move_caret_up, arena)?;

        Ok(())
    }

    #[test]
    fn extend_selection_right() -> Result<(), String> {
        let arena = &Bump::new();

        assert_move(&["[a]|bc"], &["[ab]|c"], true, move_caret_right, arena)?;
        assert_move(&["a[b]|c"], &["a[bc]|"], true, move_caret_right, arena)?;
        assert_move(&["[ab]|c"], &["[abc]|"], true, move_caret_right, arena)?;
        assert_move(&["[ ]|abc"], &["[ a]|bc"], true, move_caret_right, arena)?;
        assert_move(&["[abc]|"], &["[abc]|"], true, move_caret_right, arena)?;
        assert_move(&["a[bc]|"], &["a[bc]|"], true, move_caret_right, arena)?;
        assert_move(&["ab[c]|"], &["ab[c]|"], true, move_caret_right, arena)?;
        assert_move(
            &["abc[\n", "]|d"],
            &["abc[\n", "d]|"],
            true,
            move_caret_right,
            arena
        )?;
        assert_move(&["ab[c]|\n", ""], &["ab[c\n", "]|"], true, move_caret_right, arena)?;
        assert_move(
            &["ab[c]|\n", "d"],
            &["ab[c\n", "]|d"],
            true,
            move_caret_right,
            arena
        )?;
        assert_move(
            &["abc\n", "def\n", "ghi[\n", "]|jkl"],
            &["abc\n", "def\n", "ghi[\n", "j]|kl"],
            true,
            move_caret_right,
            arena
        )?;
        assert_move(
            &["ab[c\n", "def\n", "ghi\n", "]|jkl"],
            &["ab[c\n", "def\n", "ghi\n", "j]|kl"],
            true,
            move_caret_right,
            arena
        )?;
        assert_move(
            &["ab[c\n", "def\n", "]|ghi\n", "jkl"],
            &["ab[c\n", "def\n", "g]|hi\n", "jkl"],
            true,
            move_caret_right,
            arena
        )?;
        assert_move(
            &["[abc\n", "def\n", "ghi\n", "jk]|l"],
            &["[abc\n", "def\n", "ghi\n", "jkl]|"],
            true,
            move_caret_right,
            arena
        )?;
        assert_move(
            &["[abc\n", "def\n", "ghi\n", "jkl]|"],
            &["[abc\n", "def\n", "ghi\n", "jkl]|"],
            true,
            move_caret_right,
            arena
        )?;

        Ok(())
    }

    #[test]
    fn extend_selection_left() -> Result<(), String> {
        let arena = &Bump::new();

        assert_move(&["ab|[c]"], &["a|[bc]"], true, move_caret_left, arena)?;
        assert_move(&["a|[bc]"], &["|[abc]"], true, move_caret_left, arena)?;
        assert_move(&["|[abc]"], &["|[abc]"], true, move_caret_left, arena)?;
        assert_move(&["|[ab]c"], &["|[ab]c"], true, move_caret_left, arena)?;
        assert_move(&["|[a]bc"], &["|[a]bc"], true, move_caret_left, arena)?;
        assert_move(&[" |[a]bc"], &["|[ a]bc"], true, move_caret_left, arena)?;
        assert_move(
            &["abc|[\n", "]d"],
            &["ab|[c\n", "]d"],
            true,
            move_caret_left,
            arena
        )?;
        assert_move(
            &["abc\n", "|[d]"],
            &["abc|[\n", "d]"],
            true,
            move_caret_left,
            arena
        )?;
        assert_move(&["ab|[c\n", "]"], &["a|[bc\n", "]"], true, move_caret_left, arena)?;
        assert_move(
            &["abc\n", "def|[\n", "ghi\n", "j]kl"],
            &["abc\n", "de|[f\n", "ghi\n", "j]kl"],
            true,
            move_caret_left,
            arena
        )?;
        assert_move(
            &["a|[bc\n", "def\n", "ghi\n", "jkl]"],
            &["|[abc\n", "def\n", "ghi\n", "jkl]"],
            true,
            move_caret_left,
            arena
        )?;
        assert_move(
            &["abc\n", "def\n", "ghi\n", "|[jkl]"],
            &["abc\n", "def\n", "ghi|[\n", "jkl]"],
            true,
            move_caret_left,
            arena
        )?;

        Ok(())
    }

    #[test]
    fn extend_selection_up() -> Result<(), String> {
        let arena = &Bump::new();

        assert_move(&["ab|[c]"], &["|[abc]"], true, move_caret_up, arena)?;
        assert_move(&["a|[bc]"], &["|[abc]"], true, move_caret_up, arena)?;
        assert_move(&["|[abc]"], &["|[abc]"], true, move_caret_up, arena)?;
        assert_move(&["|[ab]c"], &["|[ab]c"], true, move_caret_up, arena)?;
        assert_move(&["|[a]bc"], &["|[a]bc"], true, move_caret_up, arena)?;
        assert_move(&[" |[a]bc"], &["|[ a]bc"], true, move_caret_up, arena)?;
        assert_move(&["ab[c]|"], &["|[ab]c"], true, move_caret_up, arena)?;
        assert_move(&["[a]|"], &["|a"], true, move_caret_up, arena)?;
        assert_move(&["[a]|bc"], &["|abc"], true, move_caret_up, arena)?;
        assert_move(&["[a]|bc\n", "d"], &["|abc\n", "d"], true, move_caret_up, arena)?;
        assert_move(
            &["abc\n", "de[f]|"],
            &["abc|[\n", "de]f"],
            true,
            move_caret_up,
            arena
        )?;
        assert_move(
            &["abc\n", "de|[f]"],
            &["ab|[c\n", "def]"],
            true,
            move_caret_up,
            arena
        )?;
        assert_move(
            &["ab|[c\n", "def]"],
            &["|[abc\n", "def]"],
            true,
            move_caret_up,
            arena
        )?;
        assert_move(
            &["ab\n", "cdef\n", "ghijkl\n", "[mnopqr]|st"],
            &["ab\n", "cdef\n", "ghijkl|[\n", "]mnopqrst"],
            true,
            move_caret_up,
            arena
        )?;
        assert_move(
            &["ab\n", "cdef\n", "ghijkl\n", "[mnopqrs]|t"],
            &["ab\n", "cdef\n", "ghijkl|[\n", "]mnopqrst"],
            true,
            move_caret_up,
            arena
        )?;
        assert_move(
            &["abcdefgh\n", "ijklmn\n", "|[o]pqr\n", "st"],
            &["abcdefgh\n", "|[ijklmn\n", "o]pqr\n", "st"],
            true,
            move_caret_up,
            arena
        )?;

        Ok(())
    }

    #[test]
    fn extend_selection_down() -> Result<(), String> {
        let arena = &Bump::new();

        assert_move(&["[ab]|c"], &["[abc]|"], true, move_caret_down, arena)?;
        assert_move(&["[a]|bc"], &["[abc]|"], true, move_caret_down, arena)?;
        assert_move(&["[abc]|"], &["[abc]|"], true, move_caret_down, arena)?;
        assert_move(&["|[ab]c"], &["ab[c]|"], true, move_caret_down, arena)?;
        assert_move(&["|[a]bc"], &["a[bc]|"], true, move_caret_down, arena)?;
        assert_move(
            &["[a]|bc\n", "d"],
            &["[abc\n", "d]|"],
            true,
            move_caret_down,
            arena
        )?;
        assert_move(
            &["[a]|bc\n", "de"],
            &["[abc\n", "d]|e"],
            true,
            move_caret_down,
            arena
        )?;
        assert_move(
            &["[abc\n", "d]|e"],
            &["[abc\n", "de]|"],
            true,
            move_caret_down,
            arena
        )?;
        assert_move(&["[a]|bc\n", ""], &["[abc\n", "]|"], true, move_caret_down, arena)?;
        assert_move(
            &["ab\n", "cdef\n", "ghijkl\n", "[mnopqr]|st"],
            &["ab\n", "cdef\n", "ghijkl\n", "[mnopqrst]|"],
            true,
            move_caret_down,
            arena
        )?;
        assert_move(
            &["a[b\n", "cdef\n", "ghijkl\n", "mnopqr]|st"],
            &["a[b\n", "cdef\n", "ghijkl\n", "mnopqrst]|"],
            true,
            move_caret_down,
            arena
        )?;
        assert_move(
            &["[ab\n", "cdef\n", "ghijkl\n", "mnopqrst]|"],
            &["[ab\n", "cdef\n", "ghijkl\n", "mnopqrst]|"],
            true,
            move_caret_down,
            arena
        )?;
        assert_move(
            &["abcd[efgh]|\n", "ijklmn\n", "opqr\n", "st"],
            &["abcd[efgh\n", "ijklmn]|\n", "opqr\n", "st"],
            true,
            move_caret_down,
            arena
        )?;
        assert_move(
            &["abcd[e]|fgh\n", "ijklmn\n", "opqr\n", "st"],
            &["abcd[efgh\n", "ijklm]|n\n", "opqr\n", "st"],
            true,
            move_caret_down,
            arena
        )?;

        Ok(())
    }

    #[test]
    fn shrink_selection_right() -> Result<(), String> {
        let arena = &Bump::new();

        assert_move(&["ab|[c]"], &["abc|"], true, move_caret_right, arena)?;
        assert_move(&["a|[bc]"], &["ab|[c]"], true, move_caret_right, arena)?;
        assert_move(&["|[abc]"], &["a|[bc]"], true, move_caret_right, arena)?;
        assert_move(
            &["|[abc\n", "def\n", "ghi\n", "jkl]"],
            &["a|[bc\n", "def\n", "ghi\n", "jkl]"],
            true,
            move_caret_right,
            arena
        )?;
        assert_move(
            &["abc\n", "d|[ef\n", "]ghi\n", "jkl"],
            &["abc\n", "de|[f\n", "]ghi\n", "jkl"],
            true,
            move_caret_right,
            arena
        )?;
        assert_move(
            &["abc\n", "de|[f]\n", "ghi\n", "jkl"],
            &["abc\n", "def|\n", "ghi\n", "jkl"],
            true,
            move_caret_right,
            arena
        )?;

        Ok(())
    }

    #[test]
    fn shrink_selection_left() -> Result<(), String> {
        let arena = &Bump::new();

        assert_move(&["ab[c]|"], &["ab|c"], true, move_caret_left, arena)?;
        assert_move(&["a[bc]|"], &["a[b]|c"], true, move_caret_left, arena)?;
        assert_move(&["[abc]|"], &["[ab]|c"], true, move_caret_left, arena)?;
        assert_move(
            &["[abc\n", "def\n", "ghi\n", "jkl]|"],
            &["[abc\n", "def\n", "ghi\n", "jk]|l"],
            true,
            move_caret_left,
            arena
        )?;
        assert_move(
            &["|[abc\n", "def\n", "ghi\n", "jkl]"],
            &["|[abc\n", "def\n", "ghi\n", "jkl]"],
            true,
            move_caret_left,
            arena
        )?;
        assert_move(
            &["abc\n", "def[\n", "]|ghi\n", "jkl"],
            &["abc\n", "def|\n", "ghi\n", "jkl"],
            true,
            move_caret_left,
            arena
        )?;
        assert_move(
            &["abc\n", "d[ef\n", "gh]|i\n", "jkl"],
            &["abc\n", "d[ef\n", "g]|hi\n", "jkl"],
            true,
            move_caret_left,
            arena
        )?;

        Ok(())
    }

    #[test]
    fn shrink_selection_up() -> Result<(), String> {
        let arena = &Bump::new();

        assert_move(&["[abc]|"], &["|abc"], true, move_caret_up, arena)?;
        assert_move(&["[ab]|c"], &["|abc"], true, move_caret_up, arena)?;
        assert_move(&["[a]|bc"], &["|abc"], true, move_caret_up, arena)?;
        assert_move(&["|abc"], &["|abc"], true, move_caret_up, arena)?;
        assert_move(
            &["[abc\n", "def]|"],
            &["[abc]|\n", "def"],
            true,
            move_caret_up,
            arena
        )?;
        assert_move(
            &["[abc\n", "de]|f"],
            &["[ab]|c\n", "def"],
            true,
            move_caret_up,
            arena
        )?;
        assert_move(
            &["[abc\n", "def\n", "ghi\n", "jkl]|"],
            &["[abc\n", "def\n", "ghi]|\n", "jkl"],
            true,
            move_caret_up,
            arena
        )?;
        assert_move(
            &["abc\n", "def\n", "ghi[\n", "jkl]|"],
            &["abc\n", "def\n", "ghi|\n", "jkl"],
            true,
            move_caret_up,
            arena
        )?;
        assert_move(
            &["abc\n", "d[ef\n", "ghi\n", "jk]|l"],
            &["abc\n", "d[ef\n", "gh]|i\n", "jkl"],
            true,
            move_caret_up,
            arena
        )?;
        assert_move(
            &["[abc\n", "d]|ef\n", "ghi\n", "jkl"],
            &["[a]|bc\n", "def\n", "ghi\n", "jkl"],
            true,
            move_caret_up,
            arena
        )?;

        Ok(())
    }

    #[test]
    fn shrink_selection_down() -> Result<(), String> {
        let arena = &Bump::new();

        assert_move(&["|[abc]"], &["abc|"], true, move_caret_down, arena)?;
        assert_move(
            &["|[abc\n", "def]"],
            &["abc\n", "|[def]"],
            true,
            move_caret_down,
            arena
        )?;
        assert_move(
            &["a|[bc\n", "def]"],
            &["abc\n", "d|[ef]"],
            true,
            move_caret_down,
            arena
        )?;
        assert_move(
            &["|[abc\n", "def\n", "ghi]"],
            &["abc\n", "|[def\n", "ghi]"],
            true,
            move_caret_down,
            arena
        )?;
        assert_move(
            &["ab|[c\n", "def\n", "ghi]"],
            &["abc\n", "de|[f\n", "ghi]"],
            true,
            move_caret_down,
            arena
        )?;
        assert_move(
            &["abc\n", "de|[f\n", "ghi]"],
            &["abc\n", "def\n", "gh|[i]"],
            true,
            move_caret_down,
            arena
        )?;
        assert_move(
            &["abcdef|[\n", "ghij\n", "kl]"],
            &["abcdef\n", "ghij|[\n", "kl]"],
            true,
            move_caret_down,
            arena
        )?;
        assert_move(
            &["abcde|[f\n", "ghij\n", "kl]"],
            &["abcdef\n", "ghij|[\n", "kl]"],
            true,
            move_caret_down,
            arena
        )?;
        assert_move(
            &["ab|[cdef\n", "ghij\n", "kl]"],
            &["abcdef\n", "gh|[ij\n", "kl]"],
            true,
            move_caret_down,
            arena
        )?;

        Ok(())
    }
}
