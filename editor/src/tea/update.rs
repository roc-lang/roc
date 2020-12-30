use crate::tea::model::{Position, RawSelection};
use crate::text::is_newline;
use std::cmp::{max, min};

pub fn move_caret_left(
    old_caret_pos: Position,
    old_selection_opt: Option<RawSelection>,
    shift_pressed: bool,
    lines: &[String],
) -> (Position, Option<RawSelection>) {
    let old_line_nr = old_caret_pos.line;
    let old_col_nr = old_caret_pos.column;

    let (line_nr, col_nr) = if old_col_nr == 0 {
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
            Some(RawSelection {
                start_pos: Position {
                    line: line_nr,
                    column: col_nr,
                },
                end_pos: old_selection.end_pos,
            })
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

    let (line_nr, col_nr) = if let Some(curr_line) = lines.get(old_line_nr) {
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
            Some(RawSelection {
                start_pos: old_selection.start_pos,
                end_pos: Position {
                    line: line_nr,
                    column: col_nr,
                },
            })
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

    let (line_nr, col_nr) = if old_line_nr == 0 {
        (old_line_nr, old_col_nr)
    } else if let Some(prev_line) = lines.get(old_line_nr - 1) {
        if prev_line.len() < old_col_nr {
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
            Some(RawSelection {
                start_pos: new_caret_pos,
                end_pos: old_selection.end_pos,
            })
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

    let (line_nr, col_nr) = if old_line_nr + 1 >= lines.len() {
        (old_line_nr, old_col_nr)
    } else if let Some(next_line) = lines.get(old_line_nr + 1) {
        if next_line.len() < old_col_nr {
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
            Some(RawSelection {
                start_pos: old_selection.start_pos,
                end_pos: new_caret_pos,
            })
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
