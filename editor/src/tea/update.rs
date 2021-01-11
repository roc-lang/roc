use super::ed_model::EdModel;
use super::ed_model::{Position, RawSelection};
use crate::util::is_newline;
use std::cmp::{max, min};

pub fn move_caret_left(
    old_caret_pos: Position,
    old_selection_opt: Option<RawSelection>,
    shift_pressed: bool,
    lines: &[String],
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
        } else if let Some(curr_line) = lines.get(old_line_nr - 1) {
            (old_line_nr - 1, curr_line.len() - 1)
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
    lines: &[String],
) -> (Position, Option<RawSelection>) {
    let old_line_nr = old_caret_pos.line;
    let old_col_nr = old_caret_pos.column;

    let (line_nr, col_nr) = if old_selection_opt.is_some() && !shift_pressed {
        match old_selection_opt {
            Some(old_selection) => (old_selection.end_pos.line, old_selection.end_pos.column),
            None => unreachable!(),
        }
    } else if let Some(curr_line) = lines.get(old_line_nr) {
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
    lines: &[String],
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
    } else if let Some(prev_line) = lines.get(old_line_nr - 1) {
        if prev_line.len() <= old_col_nr {
            (old_line_nr - 1, prev_line.len() - 1)
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
    lines: &[String],
) -> (Position, Option<RawSelection>) {
    let old_line_nr = old_caret_pos.line;
    let old_col_nr = old_caret_pos.column;

    let (line_nr, col_nr) = if old_selection_opt.is_some() && !shift_pressed {
        match old_selection_opt {
            Some(old_selection) => (old_selection.end_pos.line, old_selection.end_pos.column),
            None => unreachable!(),
        }
    } else if old_line_nr + 1 >= lines.len() {
        if let Some(curr_line) = lines.get(old_line_nr) {
            (old_line_nr, curr_line.len())
        } else {
            unreachable!()
        }
    } else if let Some(next_line) = lines.get(old_line_nr + 1) {
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

pub fn update_text_state(ed_model: &mut EdModel, received_char: &char) {
    ed_model.selection_opt = None;

    match received_char {
        '\u{8}' | '\u{7f}' => {
            // In Linux, we get a '\u{8}' when you press backspace,
            // but in macOS we get '\u{7f}'.
            if let Some(last_line) = ed_model.lines.last_mut() {
                if !last_line.is_empty() {
                    last_line.pop();
                } else if ed_model.lines.len() > 1 {
                    ed_model.lines.pop();
                }
                ed_model.caret_pos =
                    move_caret_left(ed_model.caret_pos, None, false, &ed_model.lines).0;
            }
        }
        '\u{e000}'..='\u{f8ff}' | '\u{f0000}'..='\u{ffffd}' | '\u{100000}'..='\u{10fffd}' => {
            // These are private use characters; ignore them.
            // See http://www.unicode.org/faq/private_use.html
        }
        ch if is_newline(ch) => {
            if let Some(last_line) = ed_model.lines.last_mut() {
                last_line.push(*received_char)
            }
            ed_model.lines.push(String::new());
            ed_model.caret_pos = Position {
                line: ed_model.caret_pos.line + 1,
                column: 0,
            };

            ed_model.selection_opt = None;
        }
        _ => {
            let nr_lines = ed_model.lines.len();

            if let Some(last_line) = ed_model.lines.last_mut() {
                last_line.push(*received_char);

                ed_model.caret_pos = Position {
                    line: nr_lines - 1,
                    column: last_line.len(),
                };

                ed_model.selection_opt = None;
            }
        }
    }
}
