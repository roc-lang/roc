use super::ed_model::{Position, RawSelection};
use crate::text_buffer::TextBuffer;
use crate::util::is_newline;
use super::app_model::AppModel;
use crate::error::EdResult;
use std::cmp::{max, min};

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

pub fn handle_new_char(app_model: &mut AppModel, received_char: &char) -> EdResult<()> {
    if let Some(ref mut ed_model) = app_model.ed_model_opt {
        ed_model.selection_opt = None;
        let old_caret_pos = ed_model.caret_pos;

        match received_char {
            '\u{8}' | '\u{7f}' => {
                // On Linux, '\u{8}' is backspace,
                // on macOS '\u{7f}'.
                if let Some(selection) = ed_model.selection_opt {
                    ed_model.text_buf.del_selection(selection)?;
                    ed_model.caret_pos = selection.start_pos;
                } else {
                    ed_model.text_buf.pop_char(old_caret_pos);

                    ed_model.caret_pos =
                        move_caret_left(old_caret_pos, None, false, &ed_model.text_buf).0;
                }
            }
            ch if is_newline(ch) => {
                ed_model.text_buf.insert_char(old_caret_pos, ch)?;
                ed_model.caret_pos = Position {
                    line: old_caret_pos.line + 1,
                    column: 0,
                };
            }
            '\u{e000}'..='\u{f8ff}' | '\u{f0000}'..='\u{ffffd}' | '\u{100000}'..='\u{10fffd}' => {
                // These are private use characters; ignore them.
                // See http://www.unicode.org/faq/private_use.html
            }
            _ => {
                ed_model.text_buf.insert_char(old_caret_pos, received_char)?;

                ed_model.caret_pos = Position {
                    line: old_caret_pos.line,
                    column: old_caret_pos.column + 1,
                };

            }
        }

        ed_model.selection_opt = None;
    }

    Ok(())
}
