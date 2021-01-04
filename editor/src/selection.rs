use crate::error::{EdResult, InvalidSelection};
use crate::graphics::colors;
use crate::graphics::primitives::rect::Rect;
use crate::tea::model::RawSelection;
use crate::vec_result::{get_res};
use bumpalo::collections::Vec as BumpVec;
use bumpalo::Bump;
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

pub fn create_selection_rects<'a>(
    raw_sel: RawSelection,
    glyph_bound_rects: &[Vec<Rect>],
    arena: &'a Bump,
) -> EdResult<BumpVec<'a, Rect>> {
    let valid_sel = validate_selection(raw_sel)?;
    let RawSelection { start_pos, end_pos } = valid_sel.selection;

    let mut all_rects: BumpVec<Rect> = BumpVec::new_in(arena);

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

#[cfg(test)]
mod test_parse {
    use crate::tea::update::{move_caret_left, move_caret_right, move_caret_down, move_caret_up};
    use crate::tea::model::{RawSelection, Position};
    use crate::error::{EdResult, OutOfBounds};
    use crate::vec_result::{get_res};
    use snafu::OptionExt;
    use std::slice::SliceIndex;
    use pest::Parser;

    #[derive(Parser)]
    #[grammar = "../tests/selection.pest"]
    pub struct LineParser;

    fn convert_selection_to_dsl(raw_sel_opt: Option<RawSelection>, caret_pos: Position, lines: &mut [String])  -> EdResult<&[String]> {
        if let Some(raw_sel) = raw_sel_opt {
            let mut to_insert = vec![(raw_sel.start_pos, '['), (raw_sel.end_pos, ']'), (caret_pos, '|')];
            to_insert.sort();

            for i in 0..to_insert.len() {
                let (pos, insert_char) = *get_res(i, &to_insert)?;

                insert_at_pos(lines, pos, insert_char)?;

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

    fn insert_at_pos(lines: &mut [String], pos: Position, insert_char: char) -> EdResult<()> {
        let line = get_mut_res(pos.line, lines)?;
        line.insert(pos.column, insert_char);

        Ok(())
    }

    fn get_mut_res<T>(index: usize, vec: & mut [T]) -> EdResult<& mut <usize as SliceIndex<[T]>>::Output> {
        let vec_len = vec.len();
    
        let elt_ref = vec.get_mut(index).context(OutOfBounds {
            index,
            vec_len,
        })?;
    
        Ok(elt_ref)
    }

    fn convert_dsl_to_selection(lines: &[String]) -> Result<(Option<RawSelection>, Position), String> {
        let lines_str: String = lines.join("");

        let parsed = LineParser::parse(Rule::linesWithSelect, &lines_str).expect("Selection test DSL parsing failed");

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
                                return Err("Multiple carets found, there should be only one".to_owned())
                            } else {
                                caret_opt = Some((line_nr, col_nr));
                                col_nr += elt.as_span().as_str().len();
                            }
                        }
                    },
                    Rule::optSelStart => {
                        if sel_start_opt.is_some() {
                            if elt.as_span().as_str() == "[" {
                                return Err("Found start of selection more than once, there should be only one".to_owned())
                            }
                        } else  if elt.as_span().as_str() == "[" {
                            sel_start_opt = Some((line_nr, col_nr));
                            col_nr += elt.as_span().as_str().len();
                        }
                    },
                    Rule::optSelEnd => {
                        if sel_end_opt.is_some() {
                            if elt.as_span().as_str() == "]" {
                                return Err("Found end of selection more than once, there should be only one".to_owned())
                            } 
                        } else if elt.as_span().as_str() == "]" {
                                sel_end_opt = Some((line_nr, col_nr));
                                col_nr += elt.as_span().as_str().len();
                        }
                    },
                    Rule::text => {
                        let split_str = elt.as_span().as_str().split('\n').into_iter().collect::<Vec<&str>>();

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
        
        if let Some((line, column)) = caret_opt {
            let caret_pos =
                Position {
                    line,
                    column
                };
            if sel_start_opt.is_none() && sel_end_opt.is_none() {
                Ok ((
                    None,
                    caret_pos
                ))
            } else if let Some((start_line, start_column)) = sel_start_opt {
                    if let Some((end_line, end_column)) = sel_end_opt  {
                        Ok ((
                            Some (
                                RawSelection {
                                    start_pos :
                                        Position {
                                            line: start_line,
                                            column: start_column
                                        },
                                    end_pos :
                                        Position {
                                            line: end_line,
                                            column: end_column
                                        }
                                }
                            ),
                            caret_pos
                        ))
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

    fn assert_move(
        pre_lines_str: &[&str],
        expected_post_lines_str: &[&str],
        shift_pressed: bool,
        move_fun: 
            fn(Position, Option<RawSelection>, bool, &[String]) -> (Position, Option<RawSelection>)
    ) -> Result<(), String> {
        let pre_lines: Vec<String> = pre_lines_str.iter().map(|l| l.to_string()).collect();
        let expected_post_lines: Vec<String> = expected_post_lines_str.iter().map(|l| l.to_string()).collect();

        let (sel_opt, caret_pos) = convert_dsl_to_selection(&pre_lines)?;

        let mut clean_lines = 
            pre_lines.into_iter()
            .map(|line| {
                line.replace(&['[', ']', '|'][..], "")
            })
            .collect::<Vec<String>>();

        let (new_caret_pos, new_sel_opt) = move_fun(caret_pos, sel_opt, shift_pressed, &clean_lines);

        let post_lines_res =
            convert_selection_to_dsl(new_sel_opt, new_caret_pos, &mut clean_lines);

        match post_lines_res {
            Ok(post_lines) => {
                assert_eq!(expected_post_lines, post_lines);
                Ok(())
            },
            Err(e) => Err(format!("{:?}", e))
        }
    }

    #[test]
    fn move_right() -> Result<(), String> {
        assert_move(&["|"], &["|"], false, move_caret_right)?;
        assert_move(&["a|"], &["a|"], false, move_caret_right)?;
        assert_move(&["|A"], &["A|"], false, move_caret_right)?;
        assert_move(&["|abc"], &["a|bc"], false, move_caret_right)?;
        assert_move(&["a|bc"], &["ab|c"], false, move_caret_right)?;
        assert_move(&["abc|"], &["abc|"], false, move_caret_right)?;
        assert_move(&["| abc"], &[" |abc"], false, move_caret_right)?;
        assert_move(&["abc| "], &["abc |"], false, move_caret_right)?;
        assert_move(&["abc|\n","d"], &["abc\n","|d"], false, move_caret_right)?;
        assert_move(&["abc|\n",""], &["abc\n","|"], false, move_caret_right)?;
        assert_move(&["abc\n","|def"], &["abc\n","d|ef"], false, move_caret_right)?;
        assert_move(&["abc\n","def| "], &["abc\n","def |"], false, move_caret_right)?;
        assert_move(&["abc\n","def |\n", "ghi"], &["abc\n","def \n", "|ghi"], false, move_caret_right)?;
        assert_move(&["abc\n","def|\n",""], &["abc\n","def\n", "|"], false, move_caret_right)?;
        assert_move(&["abc\n","def\n", "ghi|\n","jkl"], &["abc\n","def\n", "ghi\n", "|jkl"], false, move_caret_right)?;
        assert_move(&["abc\n","def\n", "|ghi\n","jkl"], &["abc\n","def\n", "g|hi\n", "jkl"], false, move_caret_right)?;
        assert_move(&["abc\n","def\n", "g|hi\n","jkl"], &["abc\n","def\n", "gh|i\n", "jkl"], false, move_caret_right)?;
        
        Ok(())
    }

    #[test]
    fn move_left() -> Result<(), String> {
        assert_move(&["|"], &["|"], false, move_caret_left)?;
        assert_move(&["|a"], &["|a"], false, move_caret_left)?;
        assert_move(&["|A"], &["|A"], false, move_caret_left)?;
        assert_move(&["a|bc"], &["|abc"], false, move_caret_left)?;
        assert_move(&["ab|c"], &["a|bc"], false, move_caret_left)?;
        assert_move(&["abc|"], &["ab|c"], false, move_caret_left)?;
        assert_move(&[" |abc"], &["| abc"], false, move_caret_left)?;
        assert_move(&["abc |"], &["abc| "], false, move_caret_left)?;
        assert_move(&["abc\n","|d"], &["abc|\n","d"], false, move_caret_left)?;
        assert_move(&["abc\n","|"], &["abc|\n",""], false, move_caret_left)?;
        assert_move(&["abc\n","d|ef"], &["abc\n","|def"], false, move_caret_left)?;
        assert_move(&["abc\n","def |"], &["abc\n","def| "], false, move_caret_left)?;
        assert_move(&["abc\n","def \n", "|ghi"], &["abc\n","def |\n", "ghi"], false, move_caret_left)?;
        assert_move(&["abc\n","def\n","|"], &["abc\n","def|\n", ""], false, move_caret_left)?;
        assert_move(&["abc\n","def\n", "ghi\n","|jkl"], &["abc\n","def\n", "ghi|\n", "jkl"], false, move_caret_left)?;
        assert_move(&["abc\n","def\n", "g|hi\n","jkl"], &["abc\n","def\n", "|ghi\n", "jkl"], false, move_caret_left)?;
        assert_move(&["abc\n","def\n", "gh|i\n","jkl"], &["abc\n","def\n", "g|hi\n", "jkl"], false, move_caret_left)?;
        
        Ok(())
    }

    #[test]
    fn move_up() -> Result<(), String> {
        assert_move(&["|"], &["|"], false, move_caret_up)?;
        assert_move(&["|a"], &["|a"], false, move_caret_up)?;
        assert_move(&["A|"], &["|A"], false, move_caret_up)?;
        assert_move(&["a|bc"], &["|abc"], false, move_caret_up)?;
        assert_move(&["ab|c"], &["|abc"], false, move_caret_up)?;
        assert_move(&["abc|"], &["|abc"], false, move_caret_up)?;
        assert_move(&["|abc\n","def"], &["|abc\n","def"], false, move_caret_up)?;
        assert_move(&["abc\n","|def"], &["|abc\n","def"], false, move_caret_up)?;
        assert_move(&["abc\n","d|ef"], &["a|bc\n","def"], false, move_caret_up)?;
        assert_move(&["abc\n","de|f"], &["ab|c\n","def"], false, move_caret_up)?;
        assert_move(&["abc\n","def|"], &["abc|\n","def"], false, move_caret_up)?;
        assert_move(&["abc\n","def \n", "|ghi"], &["abc\n","|def \n", "ghi"], false, move_caret_up)?;
        assert_move(&["abc\n","def \n", "g|hi"], &["abc\n","d|ef \n", "ghi"], false, move_caret_up)?;
        assert_move(&["abc\n","def \n", "gh|i"], &["abc\n","de|f \n", "ghi"], false, move_caret_up)?;
        assert_move(&["abc\n","def \n", "ghi|"], &["abc\n","def| \n", "ghi"], false, move_caret_up)?;
        assert_move(&["abc\n","de\n", "ghi|"], &["abc\n","de|\n", "ghi"], false, move_caret_up)?;
        assert_move(&["abc\n","de|"], &["ab|c\n","de"], false, move_caret_up)?;
        assert_move(&["abc\n","d|e"], &["a|bc\n","de"], false, move_caret_up)?;
        assert_move(&["abc\n","|de"], &["|abc\n","de"], false, move_caret_up)?;
        assert_move(&["ab\n","cdef\n", "ghijkl\n","mnopqrst|"], &["ab\n","cdef\n", "ghijkl|\n","mnopqrst"], false, move_caret_up)?;
        assert_move(&["ab\n","cdef\n", "ghijkl|\n","mnopqrst"], &["ab\n","cdef|\n", "ghijkl\n","mnopqrst"], false, move_caret_up)?;
        assert_move(&["ab\n","cdef\n", "ghijkl\n","|mnopqrst"], &["ab\n","cdef\n", "|ghijkl\n","mnopqrst"], false, move_caret_up)?;
        assert_move(&[" ab\n"," |cdef\n", "ghijkl\n","mnopqrst"], &[" |ab\n"," cdef\n", "ghijkl\n","mnopqrst"], false, move_caret_up)?;
        assert_move(&["ab\n","cdef\n", "ghijkl\n","mnopqr|st"], &["ab\n","cdef\n", "ghijkl|\n","mnopqrst"], false, move_caret_up)?;
        assert_move(&["ab\n","cde|f\n", "ghijkl\n","mnopqrst"], &["ab|\n","cdef\n", "ghijkl\n","mnopqrst"], false, move_caret_up)?;
        assert_move(&["abcdefgh\n","ijklmn\n", "opqr\n","st|"], &["abcdefgh\n","ijklmn\n", "op|qr\n","st"], false, move_caret_up)?;
        assert_move(&["abcdefgh\n","ijklmn\n", "opqr|\n","st"], &["abcdefgh\n","ijkl|mn\n", "opqr\n","st"], false, move_caret_up)?;
        assert_move(&["abcdefgh\n","ijklmn|\n", "opqr\n","st"], &["abcdef|gh\n","ijklmn\n", "opqr\n","st"], false, move_caret_up)?;
        assert_move(&["abcdefgh|\n","ijklmn\n", "opqr\n","st"], &["|abcdefgh\n","ijklmn\n", "opqr\n","st"], false, move_caret_up)?;
        assert_move(&["abcdefg|h\n","ijklmn\n", "opqr\n","st"], &["|abcdefgh\n","ijklmn\n", "opqr\n","st"], false, move_caret_up)?;
        assert_move(&["a|bcdefgh\n","ijklmn\n", "opqr\n","st"], &["|abcdefgh\n","ijklmn\n", "opqr\n","st"], false, move_caret_up)?;
        assert_move(&["|abcdefgh\n","ijklmn\n", "opqr\n","st"], &["|abcdefgh\n","ijklmn\n", "opqr\n","st"], false, move_caret_up)?;
        assert_move(&["abc def gh |"], &["|abc def gh "], false, move_caret_up)?;
        assert_move(&["abc de|f gh "], &["|abc def gh "], false, move_caret_up)?;
        assert_move(&["ab|c def gh "], &["|abc def gh "], false, move_caret_up)?;
        assert_move(&["a|bc def gh "], &["|abc def gh "], false, move_caret_up)?;

        Ok(())
    }

    #[test]
    fn move_down() -> Result<(), String> {
        assert_move(&["|"], &["|"], false, move_caret_down)?;
        assert_move(&["|a"], &["a|"], false, move_caret_down)?;
        assert_move(&["A|"], &["A|"], false, move_caret_down)?;
        assert_move(&["a|bc"], &["abc|"], false, move_caret_down)?;
        assert_move(&["ab|c"], &["abc|"], false, move_caret_down)?;
        assert_move(&["abc|"], &["abc|"], false, move_caret_down)?;
        assert_move(&["abc| "], &["abc |"], false, move_caret_down)?;
        assert_move(&["abc\n","|def"], &["abc\n","def|"], false, move_caret_down)?;
        assert_move(&["abc\n","d|ef"], &["abc\n","def|"], false, move_caret_down)?;
        assert_move(&["abc\n","de|f"], &["abc\n","def|"], false, move_caret_down)?;
        assert_move(&["abc\n","def|"], &["abc\n","def|"], false, move_caret_down)?;
        assert_move(&["|abc\n","def"], &["abc\n","|def"], false, move_caret_down)?;
        assert_move(&["a|bc\n","def"], &["abc\n","d|ef"], false, move_caret_down)?;
        assert_move(&["ab|c\n","def"], &["abc\n","de|f"], false, move_caret_down)?;
        assert_move(&["abc|\n","def"], &["abc\n","def|"], false, move_caret_down)?;
        assert_move(&["abc\n","|def \n", "ghi"], &["abc\n","def \n", "|ghi"], false, move_caret_down)?;
        assert_move(&["abc\n","d|ef \n", "ghi"], &["abc\n","def \n", "g|hi"], false, move_caret_down)?;
        assert_move(&["abc\n","de|f \n", "ghi"], &["abc\n","def \n", "gh|i"], false, move_caret_down)?;
        assert_move(&["abc\n","def| \n", "ghi"], &["abc\n","def \n", "ghi|"], false, move_caret_down)?;
        assert_move(&["abc\n","def |\n", "ghi"], &["abc\n","def \n", "ghi|"], false, move_caret_down)?;
        assert_move(&["abc\n","de|\n", "ghi"], &["abc\n","de\n", "gh|i"], false, move_caret_down)?;
        assert_move(&["abc|\n","de"], &["abc\n","de|"], false, move_caret_down)?;
        assert_move(&["ab|c\n","de"], &["abc\n","de|"], false, move_caret_down)?;
        assert_move(&["a|bc\n","de"], &["abc\n","d|e"], false, move_caret_down)?;
        assert_move(&["|abc\n","de"], &["abc\n","|de"], false, move_caret_down)?;
        assert_move(&["ab|\n","cdef\n", "ghijkl\n","mnopqrst"], &["ab\n","cd|ef\n", "ghijkl\n","mnopqrst"], false, move_caret_down)?;
        assert_move(&["ab\n","cdef|\n", "ghijkl\n","mnopqrst"], &["ab\n","cdef\n", "ghij|kl\n","mnopqrst"], false, move_caret_down)?;
        assert_move(&["ab\n","cdef\n", "ghijkl|\n","mnopqrst"], &["ab\n","cdef\n", "ghijkl\n","mnopqr|st"], false, move_caret_down)?;
        assert_move(&[" |ab\n"," cdef\n", "ghijkl\n","mnopqrst"], &[" ab\n"," |cdef\n", "ghijkl\n","mnopqrst"], false, move_caret_down)?;
        assert_move(&["ab\n","|cdef\n", "ghijkl\n","mnopqrst"], &["ab\n","cdef\n", "|ghijkl\n","mnopqrst"], false, move_caret_down)?;
        assert_move(&["ab\n","cdef\n", "|ghijkl\n","mnopqrst"], &["ab\n","cdef\n", "ghijkl\n","|mnopqrst"], false, move_caret_down)?;
        assert_move(&["abcdefgh|\n","ijklmn\n", "opqr\n","st"], &["abcdefgh\n","ijklmn|\n", "opqr\n","st"], false, move_caret_down)?;
        assert_move(&["abcdefgh\n","ijklmn|\n", "opqr\n","st"], &["abcdefgh\n","ijklmn\n", "opqr|\n","st"], false, move_caret_down)?;
        assert_move(&["abcdefgh\n","ijklmn\n", "opqr|\n","st"], &["abcdefgh\n","ijklmn\n", "opqr\n","st|"], false, move_caret_down)?;
        assert_move(&["abcdefgh\n","ijklmn\n", "opqr\n","|st"], &["abcdefgh\n","ijklmn\n", "opqr\n","st|"], false, move_caret_down)?;
        assert_move(&["abc def gh |"], &["abc def gh |"], false, move_caret_down)?;
        assert_move(&["abc de|f gh "], &["abc def gh |"], false, move_caret_down)?;
        assert_move(&["ab|c def gh "], &["abc def gh |"], false, move_caret_down)?;
        assert_move(&["a|bc def gh "], &["abc def gh |"], false, move_caret_down)?;
        assert_move(&["|abc def gh "], &["abc def gh |"], false, move_caret_down)?;

        Ok(())
    }
}