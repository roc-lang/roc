// Adapted from https://github.com/cessen/ropey
// by Nathan Vegdahl - license information can be found in the LEGAL_DETAILS
// file in the root directory of this distribution.
//
// Thank you, Nathan!

use crate::ui::text::caret_w_select::CaretWSelect;
use crate::ui::text::selection::validate_sel_opt;
use crate::ui::text::{
    selection::{RawSelection, Selection},
    text_pos::TextPos,
};
use crate::ui::ui_error::UIResult;
use crate::ui::util::is_newline;
use crate::window::keyboard_input::Modifiers;
use std::cmp::max;
use std::cmp::min;
use winit::event::VirtualKeyCode;

pub trait Lines {
    fn get_line_ref(&self, line_nr: usize) -> UIResult<&str>;

    fn line_len(&self, line_nr: usize) -> UIResult<usize>;

    fn nr_of_lines(&self) -> usize;

    fn nr_of_chars(&self) -> usize;

    fn all_lines_as_string(&self) -> String;

    fn is_last_line(&self, line_nr: usize) -> bool;

    fn last_char(&self, line_nr: usize) -> UIResult<Option<char>>;
}

pub trait SelectableLines {
    fn get_caret(&self) -> TextPos;

    fn set_caret(&mut self, caret_pos: TextPos);

    fn move_caret_left(&mut self, modifiers: &Modifiers) -> UIResult<()>;

    fn move_caret_right(&mut self, modifiers: &Modifiers) -> UIResult<()>;

    fn move_caret_up(&mut self, modifiers: &Modifiers) -> UIResult<()>;

    fn move_caret_down(&mut self, modifiers: &Modifiers) -> UIResult<()>;

    fn move_caret_home(&mut self, modifiers: &Modifiers) -> UIResult<()>;

    fn move_caret_end(&mut self, modifiers: &Modifiers) -> UIResult<()>;

    fn get_selection(&self) -> Option<Selection>;

    fn is_selection_active(&self) -> bool;

    fn get_selected_str(&self) -> UIResult<Option<String>>;

    fn set_raw_sel(&mut self, raw_sel: RawSelection) -> UIResult<()>;

    fn set_sel_none(&mut self);

    fn set_caret_w_sel(&mut self, caret_w_sel: CaretWSelect);

    fn select_all(&mut self) -> UIResult<()>;

    fn last_text_pos(&self) -> UIResult<TextPos>;

    fn handle_key_down(
        &mut self,
        modifiers: &Modifiers,
        virtual_keycode: VirtualKeyCode,
    ) -> UIResult<()>;
}

pub trait MutSelectableLines {
    fn insert_char(&mut self, new_char: &char) -> UIResult<()>;

    // could be for insertion, backspace, del...
    fn handle_new_char(&mut self, received_char: &char) -> UIResult<()>;

    fn insert_str(&mut self, new_str: &str) -> UIResult<()>;

    fn backspace(&mut self) -> UIResult<()>;

    fn del_selection(&mut self) -> UIResult<()>;
}

// T: Lines
pub type MoveCaretFun<T> = fn(&T, CaretWSelect, &Modifiers) -> UIResult<CaretWSelect>;

pub fn move_caret_left<T: Lines>(
    lines: &T,
    caret_w_select: CaretWSelect,
    modifiers: &Modifiers,
) -> UIResult<CaretWSelect> {
    let old_selection_opt = caret_w_select.selection_opt;
    let old_caret_pos = caret_w_select.caret_pos;
    let old_line_nr = old_caret_pos.line;
    let old_col_nr = old_caret_pos.column;

    let shift_pressed = modifiers.shift;

    let (line_nr, col_nr) = if old_selection_opt.is_some() && !shift_pressed {
        match old_selection_opt {
            Some(old_selection) => (old_selection.start_pos.line, old_selection.start_pos.column),
            None => unreachable!(),
        }
    } else if old_col_nr == 0 {
        if old_line_nr == 0 {
            (0, 0)
        } else {
            let curr_line_len = lines.line_len(old_line_nr - 1)?;

            (old_line_nr - 1, curr_line_len)
        }
    } else {
        (old_line_nr, old_col_nr - 1)
    };

    let new_caret_pos = TextPos {
        line: line_nr,
        column: col_nr,
    };

    let new_selection_opt = if shift_pressed {
        if let Some(old_selection) = old_selection_opt {
            if old_caret_pos >= old_selection.end_pos {
                if new_caret_pos == old_selection.start_pos {
                    None
                } else {
                    validate_sel_opt(old_selection.start_pos, new_caret_pos)?
                }
            } else {
                validate_sel_opt(
                    TextPos {
                        line: line_nr,
                        column: col_nr,
                    },
                    old_selection.end_pos,
                )?
            }
        } else if !(old_line_nr == line_nr && old_col_nr == col_nr) {
            validate_sel_opt(
                TextPos {
                    line: line_nr,
                    column: col_nr,
                },
                TextPos {
                    line: old_line_nr,
                    column: old_col_nr,
                },
            )?
        } else {
            None
        }
    } else {
        None
    };

    Ok(CaretWSelect::new(new_caret_pos, new_selection_opt))
}

pub fn move_caret_right<T: Lines>(
    lines: &T,
    caret_w_select: CaretWSelect,
    modifiers: &Modifiers,
) -> UIResult<CaretWSelect> {
    let old_selection_opt = caret_w_select.selection_opt;
    let old_caret_pos = caret_w_select.caret_pos;
    let old_line_nr = old_caret_pos.line;
    let old_col_nr = old_caret_pos.column;

    let shift_pressed = modifiers.shift;

    let (line_nr, col_nr) = if old_selection_opt.is_some() && !shift_pressed {
        match old_selection_opt {
            Some(old_selection) => (old_selection.end_pos.line, old_selection.end_pos.column),
            None => unreachable!(),
        }
    } else {
        let curr_line_len = lines.line_len(old_line_nr)?;
        let is_last_line = lines.is_last_line(old_line_nr);

        if !is_last_line {
            if old_col_nr + 1 > curr_line_len {
                (old_line_nr + 1, 0)
            } else {
                (old_line_nr, old_col_nr + 1)
            }
        } else if old_col_nr < curr_line_len {
            (old_line_nr, old_col_nr + 1)
        } else {
            (old_line_nr, old_col_nr)
        }
    };

    let new_caret_pos = TextPos {
        line: line_nr,
        column: col_nr,
    };

    let new_selection_opt = if shift_pressed {
        if let Some(old_selection) = old_selection_opt {
            if old_caret_pos <= old_selection.start_pos {
                if new_caret_pos == old_selection.end_pos {
                    None
                } else {
                    validate_sel_opt(new_caret_pos, old_selection.end_pos)?
                }
            } else {
                validate_sel_opt(
                    old_selection.start_pos,
                    TextPos {
                        line: line_nr,
                        column: col_nr,
                    },
                )?
            }
        } else if !(old_line_nr == line_nr && old_col_nr == col_nr) {
            validate_sel_opt(
                TextPos {
                    line: old_line_nr,
                    column: old_col_nr,
                },
                TextPos {
                    line: line_nr,
                    column: col_nr,
                },
            )?
        } else {
            None
        }
    } else {
        None
    };

    Ok(CaretWSelect::new(new_caret_pos, new_selection_opt))
}

pub fn move_caret_up<T: Lines>(
    lines: &T,
    caret_w_select: CaretWSelect,
    modifiers: &Modifiers,
) -> UIResult<CaretWSelect> {
    let old_selection_opt = caret_w_select.selection_opt;
    let old_caret_pos = caret_w_select.caret_pos;
    let old_line_nr = old_caret_pos.line;
    let old_col_nr = old_caret_pos.column;

    let shift_pressed = modifiers.shift;

    let (line_nr, col_nr) = if old_selection_opt.is_some() && !shift_pressed {
        match old_selection_opt {
            Some(old_selection) => (old_selection.start_pos.line, old_selection.start_pos.column),
            None => unreachable!(),
        }
    } else if old_line_nr == 0 {
        (old_line_nr, 0)
    } else {
        let prev_line_len = lines.line_len(old_line_nr - 1)?;

        if prev_line_len <= old_col_nr {
            let new_column = if prev_line_len > 0 { prev_line_len } else { 0 };

            (old_line_nr - 1, new_column)
        } else {
            (old_line_nr - 1, old_col_nr)
        }
    };

    let new_caret_pos = TextPos {
        line: line_nr,
        column: col_nr,
    };

    let new_selection_opt = if shift_pressed {
        if let Some(old_selection) = old_selection_opt {
            if old_selection.end_pos <= old_caret_pos {
                if new_caret_pos == old_selection.start_pos {
                    None
                } else {
                    validate_sel_opt(
                        min(old_selection.start_pos, new_caret_pos),
                        max(old_selection.start_pos, new_caret_pos),
                    )?
                }
            } else {
                validate_sel_opt(new_caret_pos, old_selection.end_pos)?
            }
        } else if !(old_line_nr == line_nr && old_col_nr == col_nr) {
            validate_sel_opt(
                min(old_caret_pos, new_caret_pos),
                max(old_caret_pos, new_caret_pos),
            )?
        } else {
            None
        }
    } else {
        None
    };

    Ok(CaretWSelect::new(new_caret_pos, new_selection_opt))
}

pub fn move_caret_down<T: Lines>(
    lines: &T,
    caret_w_select: CaretWSelect,
    modifiers: &Modifiers,
) -> UIResult<CaretWSelect> {
    let old_selection_opt = caret_w_select.selection_opt;
    let old_caret_pos = caret_w_select.caret_pos;
    let old_line_nr = old_caret_pos.line;
    let old_col_nr = old_caret_pos.column;

    let shift_pressed = modifiers.shift;

    let (line_nr, col_nr) = if old_selection_opt.is_some() && !shift_pressed {
        match old_selection_opt {
            Some(old_selection) => (old_selection.end_pos.line, old_selection.end_pos.column),
            None => unreachable!(),
        }
    } else if old_line_nr + 1 >= lines.nr_of_lines() {
        let curr_line_len = lines.line_len(old_line_nr)?;

        (old_line_nr, curr_line_len)
    } else {
        let next_line_index = old_line_nr + 1;
        let next_line_len = lines.line_len(next_line_index)?;
        let is_last_line = lines.is_last_line(next_line_index);

        if next_line_len <= old_col_nr {
            if !is_last_line {
                let new_column = if next_line_len > 0 { next_line_len } else { 0 };

                (old_line_nr + 1, new_column)
            } else {
                (old_line_nr + 1, next_line_len)
            }
        } else {
            (old_line_nr + 1, old_col_nr)
        }
    };

    let new_caret_pos = TextPos {
        line: line_nr,
        column: col_nr,
    };

    let new_selection_opt = if shift_pressed {
        if let Some(old_selection) = old_selection_opt {
            if old_caret_pos <= old_selection.start_pos {
                if new_caret_pos == old_selection.end_pos {
                    None
                } else {
                    validate_sel_opt(
                        min(old_selection.end_pos, new_caret_pos),
                        max(old_selection.end_pos, new_caret_pos),
                    )?
                }
            } else {
                validate_sel_opt(old_selection.start_pos, new_caret_pos)?
            }
        } else if !(old_line_nr == line_nr && old_col_nr == col_nr) {
            validate_sel_opt(
                min(old_caret_pos, new_caret_pos),
                max(old_caret_pos, new_caret_pos),
            )?
        } else {
            None
        }
    } else {
        None
    };

    Ok(CaretWSelect::new(new_caret_pos, new_selection_opt))
}

pub fn move_caret_home<T: Lines>(
    lines: &T,
    caret_w_select: CaretWSelect,
    modifiers: &Modifiers,
) -> UIResult<CaretWSelect> {
    let curr_line_nr = caret_w_select.caret_pos.line;
    let old_col_nr = caret_w_select.caret_pos.column;

    let curr_line_str = lines.get_line_ref(curr_line_nr)?;
    let line_char_iter = curr_line_str.chars();

    let mut first_no_space_char_col = 0;
    let mut non_space_found = false;

    for c in line_char_iter {
        if !c.is_whitespace() {
            non_space_found = true;
            break;
        } else {
            first_no_space_char_col += 1;
        }
    }

    if !non_space_found {
        first_no_space_char_col = 0;
    }

    let new_col_nr = if first_no_space_char_col == old_col_nr {
        0
    } else {
        first_no_space_char_col
    };

    caret_w_select.move_caret_w_mods(
        TextPos {
            line: curr_line_nr,
            column: new_col_nr,
        },
        modifiers,
    )
}

pub fn move_caret_end<T: Lines>(
    lines: &T,
    caret_w_select: CaretWSelect,
    modifiers: &Modifiers,
) -> UIResult<CaretWSelect> {
    let curr_line_nr = caret_w_select.caret_pos.line;
    let curr_line_len = lines.line_len(curr_line_nr)?;

    let new_col = if let Some(last_char) = lines.last_char(curr_line_nr)? {
        if is_newline(&last_char) {
            curr_line_len - 1
        } else {
            curr_line_len
        }
    } else {
        0
    };

    let new_pos = TextPos {
        line: curr_line_nr,
        column: new_col,
    };

    caret_w_select.move_caret_w_mods(new_pos, modifiers)
}
