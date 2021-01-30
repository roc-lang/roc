use super::ed_model::EdModel;
use super::ed_model::{Position, RawSelection};
use crate::error::EdResult;
use crate::text_buffer::TextBuffer;
use crate::util::is_newline;
use std::cmp::{max, min};
use winit::event::VirtualKeyCode::*;
use winit::event::{ModifiersState, VirtualKeyCode};

pub type MoveCaretFun =
    fn(Position, Option<RawSelection>, bool, &TextBuffer) -> (Position, Option<RawSelection>);

pub fn move_caret_left(
    old_caret_pos: Position,
    old_selection_opt: Option<RawSelection>,
    shift_pressed: bool,
    text_buf: &TextBuffer,
) -> (Position, Option<RawSelection>) {
    let old_line_nr = old_caret_pos.line;
    let old_col_nr = old_caret_pos.column;

    let (line_nr, col_nr) = if old_selection_opt.is_some() && !shift_pressed {
        match old_selection_opt {
            Some(old_selection) => (old_selection.start_pos.line, old_selection.start_pos.column),
            None => unreachable!(),
        }
    } else if old_col_nr == 0 {
        if old_line_nr == 0 {
            (0, 0)
        } else if let Some(curr_line_len) = text_buf.line_len(old_line_nr - 1) {
            (old_line_nr - 1, curr_line_len - 1)
        } else {
            unreachable!()
        }
    } else {
        (old_line_nr, old_col_nr - 1)
    };

    let new_caret_pos = Position {
        line: line_nr,
        column: col_nr,
    };

    let new_selection_opt = if shift_pressed {
        if let Some(old_selection) = old_selection_opt {
            if old_caret_pos >= old_selection.end_pos {
                if new_caret_pos == old_selection.start_pos {
                    None
                } else {
                    Some(RawSelection {
                        start_pos: old_selection.start_pos,
                        end_pos: new_caret_pos,
                    })
                }
            } else {
                Some(RawSelection {
                    start_pos: Position {
                        line: line_nr,
                        column: col_nr,
                    },
                    end_pos: old_selection.end_pos,
                })
            }
        } else if !(old_line_nr == line_nr && old_col_nr == col_nr) {
            Some(RawSelection {
                start_pos: Position {
                    line: line_nr,
                    column: col_nr,
                },
                end_pos: Position {
                    line: old_line_nr,
                    column: old_col_nr,
                },
            })
        } else {
            None
        }
    } else {
        None
    };

    (new_caret_pos, new_selection_opt)
}

pub fn move_caret_right(
    old_caret_pos: Position,
    old_selection_opt: Option<RawSelection>,
    shift_pressed: bool,
    text_buf: &TextBuffer,
) -> (Position, Option<RawSelection>) {
    let old_line_nr = old_caret_pos.line;
    let old_col_nr = old_caret_pos.column;

    let (line_nr, col_nr) = if old_selection_opt.is_some() && !shift_pressed {
        match old_selection_opt {
            Some(old_selection) => (old_selection.end_pos.line, old_selection.end_pos.column),
            None => unreachable!(),
        }
    } else if let Some(curr_line) = text_buf.line(old_line_nr) {
        if let Some(last_char) = curr_line.chars().last() {
            if is_newline(&last_char) {
                if old_col_nr + 1 > curr_line.len() - 1 {
                    (old_line_nr + 1, 0)
                } else {
                    (old_line_nr, old_col_nr + 1)
                }
            } else if old_col_nr < curr_line.len() {
                (old_line_nr, old_col_nr + 1)
            } else {
                (old_line_nr, old_col_nr)
            }
        } else {
            (old_line_nr, old_col_nr)
        }
    } else {
        unreachable!()
    };

    let new_caret_pos = Position {
        line: line_nr,
        column: col_nr,
    };

    let new_selection_opt = if shift_pressed {
        if let Some(old_selection) = old_selection_opt {
            if old_caret_pos <= old_selection.start_pos {
                if new_caret_pos == old_selection.end_pos {
                    None
                } else {
                    Some(RawSelection {
                        start_pos: new_caret_pos,
                        end_pos: old_selection.end_pos,
                    })
                }
            } else {
                Some(RawSelection {
                    start_pos: old_selection.start_pos,
                    end_pos: Position {
                        line: line_nr,
                        column: col_nr,
                    },
                })
            }
        } else if !(old_line_nr == line_nr && old_col_nr == col_nr) {
            Some(RawSelection {
                start_pos: Position {
                    line: old_line_nr,
                    column: old_col_nr,
                },
                end_pos: Position {
                    line: line_nr,
                    column: col_nr,
                },
            })
        } else {
            None
        }
    } else {
        None
    };

    (new_caret_pos, new_selection_opt)
}

pub fn move_caret_up(
    old_caret_pos: Position,
    old_selection_opt: Option<RawSelection>,
    shift_pressed: bool,
    text_buf: &TextBuffer,
) -> (Position, Option<RawSelection>) {
    let old_line_nr = old_caret_pos.line;
    let old_col_nr = old_caret_pos.column;

    let (line_nr, col_nr) = if old_selection_opt.is_some() && !shift_pressed {
        match old_selection_opt {
            Some(old_selection) => (old_selection.start_pos.line, old_selection.start_pos.column),
            None => unreachable!(),
        }
    } else if old_line_nr == 0 {
        (old_line_nr, 0)
    } else if let Some(prev_line_len) = text_buf.line_len(old_line_nr - 1) {
        if prev_line_len <= old_col_nr {
            (old_line_nr - 1, prev_line_len - 1)
        } else {
            (old_line_nr - 1, old_col_nr)
        }
    } else {
        unreachable!()
    };

    let new_caret_pos = Position {
        line: line_nr,
        column: col_nr,
    };

    let new_selection_opt = if shift_pressed {
        if let Some(old_selection) = old_selection_opt {
            if old_selection.end_pos <= old_caret_pos {
                if new_caret_pos == old_selection.start_pos {
                    None
                } else {
                    Some(RawSelection {
                        start_pos: min(old_selection.start_pos, new_caret_pos),
                        end_pos: max(old_selection.start_pos, new_caret_pos),
                    })
                }
            } else {
                Some(RawSelection {
                    start_pos: new_caret_pos,
                    end_pos: old_selection.end_pos,
                })
            }
        } else if !(old_line_nr == line_nr && old_col_nr == col_nr) {
            Some(RawSelection {
                start_pos: min(old_caret_pos, new_caret_pos),
                end_pos: max(old_caret_pos, new_caret_pos),
            })
        } else {
            None
        }
    } else {
        None
    };

    (new_caret_pos, new_selection_opt)
}

pub fn move_caret_down(
    old_caret_pos: Position,
    old_selection_opt: Option<RawSelection>,
    shift_pressed: bool,
    text_buf: &TextBuffer,
) -> (Position, Option<RawSelection>) {
    let old_line_nr = old_caret_pos.line;
    let old_col_nr = old_caret_pos.column;

    let (line_nr, col_nr) = if old_selection_opt.is_some() && !shift_pressed {
        match old_selection_opt {
            Some(old_selection) => (old_selection.end_pos.line, old_selection.end_pos.column),
            None => unreachable!(),
        }
    } else if old_line_nr + 1 >= text_buf.nr_of_lines() {
        if let Some(curr_line_len) = text_buf.line_len(old_line_nr) {
            (old_line_nr, curr_line_len)
        } else {
            unreachable!()
        }
    } else if let Some(next_line) = text_buf.line(old_line_nr + 1) {
        if next_line.len() <= old_col_nr {
            if let Some(last_char) = next_line.chars().last() {
                if is_newline(&last_char) {
                    (old_line_nr + 1, next_line.len() - 1)
                } else {
                    (old_line_nr + 1, next_line.len())
                }
            } else {
                (old_line_nr + 1, 0)
            }
        } else {
            (old_line_nr + 1, old_col_nr)
        }
    } else {
        unreachable!()
    };

    let new_caret_pos = Position {
        line: line_nr,
        column: col_nr,
    };

    let new_selection_opt = if shift_pressed {
        if let Some(old_selection) = old_selection_opt {
            if old_caret_pos <= old_selection.start_pos {
                if new_caret_pos == old_selection.end_pos {
                    None
                } else {
                    Some(RawSelection {
                        start_pos: min(old_selection.end_pos, new_caret_pos),
                        end_pos: max(old_selection.end_pos, new_caret_pos),
                    })
                }
            } else {
                Some(RawSelection {
                    start_pos: old_selection.start_pos,
                    end_pos: new_caret_pos,
                })
            }
        } else if !(old_line_nr == line_nr && old_col_nr == col_nr) {
            Some(RawSelection {
                start_pos: min(old_caret_pos, new_caret_pos),
                end_pos: max(old_caret_pos, new_caret_pos),
            })
        } else {
            None
        }
    } else {
        None
    };

    (new_caret_pos, new_selection_opt)
}

fn handle_arrow(move_caret_fun: MoveCaretFun, modifiers: &ModifiersState, ed_model: &mut EdModel) {
    let (new_caret_pos, new_selection_opt) = move_caret_fun(
        ed_model.caret_pos,
        ed_model.selection_opt,
        modifiers.shift(),
        &ed_model.text_buf,
    );
    ed_model.caret_pos = new_caret_pos;
    ed_model.selection_opt = new_selection_opt;
}

fn del_selection(selection: RawSelection, ed_model: &mut EdModel) -> EdResult<()> {
    ed_model.text_buf.del_selection(selection)?;
    ed_model.caret_pos = selection.start_pos;

    Ok(())
}

pub fn handle_new_char(received_char: &char, ed_model: &mut EdModel) -> EdResult<()> {
    let old_caret_pos = ed_model.caret_pos;

    match received_char {
        '\u{8}' | '\u{7f}' => {
            // On Linux, '\u{8}' is backspace,
            // on macOS '\u{7f}'.
            if let Some(selection) = ed_model.selection_opt {
                del_selection(selection, ed_model)?;
            } else {
                ed_model.caret_pos =
                    move_caret_left(old_caret_pos, None, false, &ed_model.text_buf).0;

                ed_model.text_buf.pop_char(old_caret_pos);
            }

            ed_model.selection_opt = None;
        }
        ch if is_newline(ch) => {
            if let Some(selection) = ed_model.selection_opt {
                del_selection(selection, ed_model)?;
                ed_model.text_buf.insert_char(ed_model.caret_pos, &'\n')?;
            } else {
                ed_model.text_buf.insert_char(old_caret_pos, &'\n')?;

                ed_model.caret_pos = Position {
                    line: old_caret_pos.line + 1,
                    column: 0,
                };
            }

            ed_model.selection_opt = None;
        }
        '\u{3}'
        | '\u{16}'
        | '\u{30}'
        | '\u{e000}'..='\u{f8ff}'
        | '\u{f0000}'..='\u{ffffd}'
        | '\u{100000}'..='\u{10fffd}' => {
            // chars that can be ignored
        }
        _ => {
            if let Some(selection) = ed_model.selection_opt {
                del_selection(selection, ed_model)?;
                ed_model
                    .text_buf
                    .insert_char(ed_model.caret_pos, received_char)?;

                ed_model.caret_pos =
                    move_caret_right(ed_model.caret_pos, None, false, &ed_model.text_buf).0;
            } else {
                ed_model
                    .text_buf
                    .insert_char(old_caret_pos, received_char)?;

                ed_model.caret_pos = Position {
                    line: old_caret_pos.line,
                    column: old_caret_pos.column + 1,
                };
            }

            ed_model.selection_opt = None;
        }
    }

    Ok(())
}

pub fn handle_key_down(
    modifiers: &ModifiersState,
    virtual_keycode: VirtualKeyCode,
    ed_model: &mut EdModel,
) {
    match virtual_keycode {
        Left => handle_arrow(move_caret_left, modifiers, ed_model),
        Up => handle_arrow(move_caret_up, modifiers, ed_model),
        Right => handle_arrow(move_caret_right, modifiers, ed_model),
        Down => handle_arrow(move_caret_down, modifiers, ed_model),
        _ => {}
    }
}

#[cfg(test)]
pub mod test_ed_update {
    use crate::mvc::app_update::test_app_update::mock_app_model;
    use crate::mvc::ed_model::{Position, RawSelection};
    use crate::mvc::ed_update::handle_new_char;
    use crate::selection::test_selection::{
        all_lines_vec, convert_dsl_to_selection, convert_selection_to_dsl, text_buffer_from_dsl_str,
    };
    use crate::text_buffer::TextBuffer;

    pub fn gen_caret_text_buf(
        lines: &[&str],
    ) -> Result<(Position, Option<RawSelection>, TextBuffer), String> {
        let lines_string_slice: Vec<String> = lines.iter().map(|l| l.to_string()).collect();
        let (selection_opt, caret_pos) = convert_dsl_to_selection(&lines_string_slice)?;
        let text_buf = text_buffer_from_dsl_str(&lines_string_slice);

        Ok((caret_pos, selection_opt, text_buf))
    }

    fn assert_insert(
        pre_lines_str: &[&str],
        expected_post_lines_str: &[&str],
        new_char: char,
    ) -> Result<(), String> {
        let (caret_pos, selection_opt, pre_text_buf) = gen_caret_text_buf(pre_lines_str)?;

        let app_model = mock_app_model(pre_text_buf, caret_pos, selection_opt, None);
        let mut ed_model = app_model.ed_model_opt.unwrap();

        if let Err(e) = handle_new_char(&new_char, &mut ed_model) {
            return Err(e.to_string());
        }

        let mut actual_lines = all_lines_vec(&ed_model.text_buf);
        let dsl_slice = convert_selection_to_dsl(
            ed_model.selection_opt,
            ed_model.caret_pos,
            &mut actual_lines,
        )
        .unwrap();
        assert_eq!(dsl_slice, expected_post_lines_str);

        Ok(())
    }

    #[test]
    fn insert_new_char_simple() -> Result<(), String> {
        assert_insert(&["|"], &["a|"], 'a')?;
        assert_insert(&["|"], &[" |"], ' ')?;
        assert_insert(&["a|"], &["aa|"], 'a')?;
        assert_insert(&["a|"], &["a |"], ' ')?;
        assert_insert(&["a|\n", ""], &["ab|\n", ""], 'b')?;
        assert_insert(&["a|\n", ""], &["ab|\n", ""], 'b')?;
        assert_insert(&["a\n", "|"], &["a\n", "b|"], 'b')?;
        assert_insert(&["a\n", "b\n", "c|"], &["a\n", "b\n", "cd|"], 'd')?;

        Ok(())
    }

    #[test]
    fn insert_new_char_mid() -> Result<(), String> {
        assert_insert(&["ab|d"], &["abc|d"], 'c')?;
        assert_insert(&["a|cd"], &["ab|cd"], 'b')?;
        assert_insert(&["abc\n", "|e"], &["abc\n", "d|e"], 'd')?;
        assert_insert(&["abc\n", "def\n", "| "], &["abc\n", "def\n", "g| "], 'g')?;
        assert_insert(&["abc\n", "def\n", "| "], &["abc\n", "def\n", " | "], ' ')?;

        Ok(())
    }

    #[test]
    fn simple_backspace() -> Result<(), String> {
        assert_insert(&["|"], &["|"], '\u{8}')?;
        assert_insert(&[" |"], &["|"], '\u{8}')?;
        assert_insert(&["a|"], &["|"], '\u{8}')?;
        assert_insert(&["ab|"], &["a|"], '\u{8}')?;
        assert_insert(&["a|\n", ""], &["|\n", ""], '\u{8}')?;
        assert_insert(&["ab|\n", ""], &["a|\n", ""], '\u{8}')?;
        assert_insert(&["a\n", "|"], &["a|"], '\u{8}')?;
        assert_insert(&["a\n", "b\n", "c|"], &["a\n", "b\n", "|"], '\u{8}')?;
        assert_insert(&["a\n", "b\n", "|"], &["a\n", "b|"], '\u{8}')?;

        Ok(())
    }

    #[test]
    fn selection_backspace() -> Result<(), String> {
        assert_insert(&["[a]|"], &["|"], '\u{8}')?;
        assert_insert(&["a[a]|"], &["a|"], '\u{8}')?;
        assert_insert(&["[aa]|"], &["|"], '\u{8}')?;
        assert_insert(&["a[b c]|"], &["a|"], '\u{8}')?;
        assert_insert(&["[abc]|\n", ""], &["|\n", ""], '\u{8}')?;
        assert_insert(&["a\n", "[abc]|"], &["a\n", "|"], '\u{8}')?;
        assert_insert(&["[a\n", "abc]|"], &["|"], '\u{8}')?;
        assert_insert(&["a[b\n", "cdef ghij]|"], &["a|"], '\u{8}')?;
        assert_insert(&["[a\n", "b\n", "c]|"], &["|"], '\u{8}')?;
        assert_insert(&["a\n", "[b\n", "]|"], &["a\n", "|"], '\u{8}')?;
        assert_insert(
            &["abc\n", "d[ef\n", "ghi]|\n", "jkl"],
            &["abc\n", "d|\n", "jkl"],
            '\u{8}',
        )?;
        assert_insert(
            &["abc\n", "[def\n", "ghi]|\n", "jkl"],
            &["abc\n", "|\n", "jkl"],
            '\u{8}',
        )?;
        assert_insert(
            &["abc\n", "\n", "[def\n", "ghi]|\n", "jkl"],
            &["abc\n", "\n", "|\n", "jkl"],
            '\u{8}',
        )?;
        assert_insert(
            &["[abc\n", "\n", "def\n", "ghi\n", "jkl]|"],
            &["|"],
            '\u{8}',
        )?;

        Ok(())
    }

    #[test]
    fn insert_with_selection() -> Result<(), String> {
        assert_insert(&["[a]|"], &["z|"], 'z')?;
        assert_insert(&["a[a]|"], &["az|"], 'z')?;
        assert_insert(&["[aa]|"], &["z|"], 'z')?;
        assert_insert(&["a[b c]|"], &["az|"], 'z')?;
        assert_insert(&["[abc]|\n", ""], &["z|\n", ""], 'z')?;
        assert_insert(&["a\n", "[abc]|"], &["a\n", "z|"], 'z')?;
        assert_insert(&["[a\n", "abc]|"], &["z|"], 'z')?;
        assert_insert(&["a[b\n", "cdef ghij]|"], &["az|"], 'z')?;
        assert_insert(&["[a\n", "b\n", "c]|"], &["z|"], 'z')?;
        assert_insert(&["a\n", "[b\n", "]|"], &["a\n", "z|"], 'z')?;
        assert_insert(
            &["abc\n", "d[ef\n", "ghi]|\n", "jkl"],
            &["abc\n", "dz|\n", "jkl"],
            'z',
        )?;
        assert_insert(
            &["abc\n", "[def\n", "ghi]|\n", "jkl"],
            &["abc\n", "z|\n", "jkl"],
            'z',
        )?;
        assert_insert(
            &["abc\n", "\n", "[def\n", "ghi]|\n", "jkl"],
            &["abc\n", "\n", "z|\n", "jkl"],
            'z',
        )?;
        assert_insert(&["[abc\n", "\n", "def\n", "ghi\n", "jkl]|"], &["z|"], 'z')?;

        Ok(())
    }
}
