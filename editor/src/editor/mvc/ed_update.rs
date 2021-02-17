use super::ed_model::EdModel;
use crate::ui::text::{
    text_pos::TextPos,
    selection::{RawSelection, Selection, validate_selection},
    big_selectable_text::BigSelectableText,
    lines::{Lines, SelectableLines, MutSelectableLines},
    caret_w_select::CaretWSelect,
};
use crate::ui::ui_error::UIResult;
use crate::editor::ed_error::EdResult;
use crate::editor::util::is_newline;
use std::cmp::{max, min};
use winit::event::VirtualKeyCode::*;
use winit::event::{ModifiersState, VirtualKeyCode};

pub type MoveCaretFun =
    fn(bool, &BigSelectableText) -> UIResult<CaretWSelect>;

fn validate_sel_opt(start_pos: TextPos, end_pos: TextPos) -> UIResult<Option<Selection>> {
    Ok(
        Some(
            validate_selection(start_pos, end_pos)?
        )
    )
}

fn handle_arrow(move_caret_fun: MoveCaretFun, modifiers: &ModifiersState, ed_model: &mut EdModel) -> UIResult<()> {
    let new_caret_w_select = move_caret_fun(
        modifiers.shift(),
        &ed_model.text,
    )?;

    ed_model.text.caret_w_select = new_caret_w_select;

    Ok(())
}

// TODO move this to impl EdModel
pub fn handle_select_all(ed_model: &mut EdModel) -> UIResult<()> {
    if ed_model.text.nr_of_chars() > 0 {
        let last_pos = ed_model.text.last_text_pos();

        ed_model.text.set_raw_sel(
            RawSelection {
                start_pos: TextPos { line: 0, column: 0 },
                end_pos: last_pos,
            }
        )?;

        ed_model.text.set_caret(last_pos);
    }

    Ok(())
}

pub fn handle_new_char(received_char: &char, ed_model: &mut EdModel) -> EdResult<()> {
    let old_caret_pos = ed_model.text.caret_w_select.caret_pos;

    // TODO move this to ui folder

    match received_char {
        '\u{8}' | '\u{7f}' => {
            // On Linux, '\u{8}' is backspace,
            // on macOS '\u{7f}'.

            if ed_model.text.is_selection_active() {
                ed_model.text.del_selection()?;
            } else {
                ed_model.text.pop_char();
            }

            ed_model.text.set_sel_none();
        }
        ch if is_newline(ch) => {
            if ed_model.text.is_selection_active() {
                ed_model.text.del_selection()?;
                ed_model.text.insert_char(&'\n')?;
            } else {
                ed_model.text.insert_char(&'\n')?;

                ed_model.text.set_caret(TextPos {
                    line: old_caret_pos.line + 1,
                    column: 0,
                });
            }

            ed_model.text.set_sel_none();
        }
        '\u{1}' // Ctrl + A
        | '\u{3}' // Ctrl + C
        | '\u{16}' // Ctrl + V
        | '\u{18}' // Ctrl + X
        | '\u{e000}'..='\u{f8ff}' // http://www.unicode.org/faq/private_use.html
        | '\u{f0000}'..='\u{ffffd}' // ^
        | '\u{100000}'..='\u{10fffd}' // ^
        => {
            // chars that can be ignored
        }
        _ => {
            if ed_model.text.is_selection_active() {
                ed_model.text.del_selection()?;
                ed_model
                    .text
                    .insert_char(received_char)?;

                ed_model.text.set_caret(
                    move_caret_right(false, &ed_model.text)?.caret_pos
                );
            } else {
                ed_model
                    .text
                    .insert_char(received_char)?;

                ed_model.text.set_caret(
                    TextPos {
                        line: old_caret_pos.line,
                        column: old_caret_pos.column + 1,
                    }
                );
            }

            ed_model.text.set_sel_none();
        }
    }

    Ok(())
}

pub fn handle_key_down(
    modifiers: &ModifiersState,
    virtual_keycode: VirtualKeyCode,
    ed_model: &mut EdModel,
) -> UIResult<()> {
    match virtual_keycode {
        Left => handle_arrow(move_caret_left, modifiers, ed_model),
        Up => handle_arrow(move_caret_up, modifiers, ed_model),
        Right => handle_arrow(move_caret_right, modifiers, ed_model),
        Down => handle_arrow(move_caret_down, modifiers, ed_model),

        A => {
            if modifiers.ctrl() {
                handle_select_all(ed_model)
            } else {
                Ok(())
            }
        }
        Home => {
            let curr_line_nr = ed_model.text.caret_w_select.caret_pos.line;
            // TODO no unwrap
            let curr_line_str = ed_model.text.get_line(curr_line_nr).unwrap();
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

            ed_model.text.caret_w_select.move_caret_w_mods(
                TextPos {
                    line: ed_model.text.caret_w_select.caret_pos.line,
                    column: first_no_space_char_col
                },
                modifiers
            )
        }
        End => {
            let curr_line = ed_model.text.caret_w_select.caret_pos.line;
            // TODO no unwrap
            let new_col = 
                max(
                        0,
                        ed_model.text.line_len(curr_line).unwrap() - 1
                    );

            let new_pos =
                TextPos {
                    line: curr_line,
                    column: new_col
                };

            ed_model.text.caret_w_select.move_caret_w_mods(new_pos, modifiers)
        }
        _ => Ok(())
    }
}

#[cfg(test)]
pub mod test_ed_update {
    use crate::editor::mvc::app_update::test_app_update::mock_app_model;
    use crate::editor::mvc::ed_update::{handle_new_char, handle_select_all};
    use crate::ui::text::{
        big_selectable_text::BigSelectableText,
    };
    use crate::ui::text::selection::test_selection::{
        all_lines_vec, convert_dsl_to_selection, convert_selection_to_dsl, big_text_from_dsl_str,
    };
    use bumpalo::Bump;

    pub fn gen_big_text(
        lines: &[&str],
    ) -> Result<BigSelectableText, String> {
        let lines_string_slice: Vec<String> = lines.iter().map(|l| l.to_string()).collect();
        let mut big_text = big_text_from_dsl_str(&lines_string_slice);
        let caret_w_select = convert_dsl_to_selection(&lines_string_slice).unwrap();

        big_text.caret_w_select = caret_w_select;

        Ok(big_text)
    }

    fn assert_insert(
        pre_lines_str: &[&str],
        expected_post_lines_str: &[&str],
        new_char: char,
        arena: &Bump,
    ) -> Result<(), String> {
        let pre_big_text = gen_big_text(pre_lines_str)?;

        let app_model = mock_app_model(pre_big_text, None);
        let mut ed_model = app_model.ed_model_opt.unwrap();

        if let Err(e) = handle_new_char(&new_char, &mut ed_model) {
            return Err(e.to_string());
        }

        let mut actual_lines = all_lines_vec(&ed_model.text);
        let dsl_slice = convert_selection_to_dsl(
            ed_model.text.caret_w_select,
            &mut actual_lines,
        )
        .unwrap();
        assert_eq!(dsl_slice, expected_post_lines_str);

        Ok(())
    }

    #[test]
    fn insert_new_char_simple() -> Result<(), String> {
        let arena = &Bump::new();

        assert_insert(&["|"], &["a|"], 'a', arena)?;
        assert_insert(&["|"], &[" |"], ' ', arena)?;
        assert_insert(&["a|"], &["aa|"], 'a', arena)?;
        assert_insert(&["a|"], &["a |"], ' ', arena)?;
        assert_insert(&["a|\n", ""], &["ab|\n", ""], 'b', arena)?;
        assert_insert(&["a|\n", ""], &["ab|\n", ""], 'b', arena)?;
        assert_insert(&["a\n", "|"], &["a\n", "b|"], 'b', arena)?;
        assert_insert(&["a\n", "b\n", "c|"], &["a\n", "b\n", "cd|"], 'd', arena)?;

        Ok(())
    }

    #[test]
    fn insert_new_char_mid() -> Result<(), String> {
        let arena = &Bump::new();

        assert_insert(&["ab|d"], &["abc|d"], 'c', arena)?;
        assert_insert(&["a|cd"], &["ab|cd"], 'b', arena)?;
        assert_insert(&["abc\n", "|e"], &["abc\n", "d|e"], 'd', arena)?;
        assert_insert(&["abc\n", "def\n", "| "], &["abc\n", "def\n", "g| "], 'g', arena)?;
        assert_insert(&["abc\n", "def\n", "| "], &["abc\n", "def\n", " | "], ' ', arena)?;

        Ok(())
    }

    #[test]
    fn simple_backspace() -> Result<(), String> {
        let arena = &Bump::new();

        assert_insert(&["|"], &["|"], '\u{8}', arena)?;
        assert_insert(&[" |"], &["|"], '\u{8}', arena)?;
        assert_insert(&["a|"], &["|"], '\u{8}', arena)?;
        assert_insert(&["ab|"], &["a|"], '\u{8}', arena)?;
        assert_insert(&["a|\n", ""], &["|\n", ""], '\u{8}', arena)?;
        assert_insert(&["ab|\n", ""], &["a|\n", ""], '\u{8}', arena)?;
        assert_insert(&["a\n", "|"], &["a|"], '\u{8}', arena)?;
        assert_insert(&["a\n", "b\n", "c|"], &["a\n", "b\n", "|"], '\u{8}', arena)?;
        assert_insert(&["a\n", "b\n", "|"], &["a\n", "b|"], '\u{8}', arena)?;

        Ok(())
    }

    #[test]
    fn selection_backspace() -> Result<(), String> {
        let arena = &Bump::new();

        assert_insert(&["[a]|"], &["|"], '\u{8}', arena)?;
        assert_insert(&["a[a]|"], &["a|"], '\u{8}', arena)?;
        assert_insert(&["[aa]|"], &["|"], '\u{8}', arena)?;
        assert_insert(&["a[b c]|"], &["a|"], '\u{8}', arena)?;
        assert_insert(&["[abc]|\n", ""], &["|\n", ""], '\u{8}', arena)?;
        assert_insert(&["a\n", "[abc]|"], &["a\n", "|"], '\u{8}', arena)?;
        assert_insert(&["[a\n", "abc]|"], &["|"], '\u{8}', arena)?;
        assert_insert(&["a[b\n", "cdef ghij]|"], &["a|"], '\u{8}', arena)?;
        assert_insert(&["[a\n", "b\n", "c]|"], &["|"], '\u{8}', arena)?;
        assert_insert(&["a\n", "[b\n", "]|"], &["a\n", "|"], '\u{8}', arena)?;
        assert_insert(
            &["abc\n", "d[ef\n", "ghi]|\n", "jkl"],
            &["abc\n", "d|\n", "jkl"],
            '\u{8}',
            arena,
        )?;
        assert_insert(
            &["abc\n", "[def\n", "ghi]|\n", "jkl"],
            &["abc\n", "|\n", "jkl"],
            '\u{8}',
            arena,
        )?;
        assert_insert(
            &["abc\n", "\n", "[def\n", "ghi]|\n", "jkl"],
            &["abc\n", "\n", "|\n", "jkl"],
            '\u{8}',
            arena,
        )?;
        assert_insert(
            &["[abc\n", "\n", "def\n", "ghi\n", "jkl]|"],
            &["|"],
            '\u{8}',
            arena,
        )?;

        Ok(())
    }

    #[test]
    fn insert_with_selection() -> Result<(), String> {
        let arena = &Bump::new();

        assert_insert(&["[a]|"], &["z|"], 'z', arena)?;
        assert_insert(&["a[a]|"], &["az|"], 'z', arena)?;
        assert_insert(&["[aa]|"], &["z|"], 'z', arena)?;
        assert_insert(&["a[b c]|"], &["az|"], 'z', arena)?;
        assert_insert(&["[abc]|\n", ""], &["z|\n", ""], 'z', arena)?;
        assert_insert(&["a\n", "[abc]|"], &["a\n", "z|"], 'z', arena)?;
        assert_insert(&["[a\n", "abc]|"], &["z|"], 'z', arena)?;
        assert_insert(&["a[b\n", "cdef ghij]|"], &["az|"], 'z', arena)?;
        assert_insert(&["[a\n", "b\n", "c]|"], &["z|"], 'z', arena)?;
        assert_insert(&["a\n", "[b\n", "]|"], &["a\n", "z|"], 'z', arena)?;
        assert_insert(
            &["abc\n", "d[ef\n", "ghi]|\n", "jkl"],
            &["abc\n", "dz|\n", "jkl"],
            'z',
            arena,
        )?;
        assert_insert(
            &["abc\n", "[def\n", "ghi]|\n", "jkl"],
            &["abc\n", "z|\n", "jkl"],
            'z',
            arena,
        )?;
        assert_insert(
            &["abc\n", "\n", "[def\n", "ghi]|\n", "jkl"],
            &["abc\n", "\n", "z|\n", "jkl"],
            'z',
            arena,
        )?;
        assert_insert(&["[abc\n", "\n", "def\n", "ghi\n", "jkl]|"], &["z|"], 'z', arena)?;

        Ok(())
    }

    fn assert_select_all(
        pre_lines_str: &[&str],
        expected_post_lines_str: &[&str],
        arena: &Bump,
    ) -> Result<(), String> {
        let pre_big_text = gen_big_text(pre_lines_str)?;

        let app_model = mock_app_model(pre_big_text, None);
        let mut ed_model = app_model.ed_model_opt.unwrap();

        handle_select_all(&mut ed_model).unwrap();

        let mut big_text_lines = all_lines_vec(&ed_model.text);
        let post_lines_str = convert_selection_to_dsl(
            ed_model.text.caret_w_select,
            &mut big_text_lines,
        )?;

        assert_eq!(post_lines_str, expected_post_lines_str);

        Ok(())
    }

    #[test]
    fn select_all() -> Result<(), String> {
        let arena = &Bump::new();

        assert_select_all(&["|"], &["|"], arena)?;
        assert_select_all(&["|a"], &["[a]|"], arena)?;
        assert_select_all(&["a|"], &["[a]|"], arena)?;
        assert_select_all(&["abc d|ef ghi"], &["[abc def ghi]|"], arena)?;
        assert_select_all(&["[a]|"], &["[a]|"], arena)?;
        assert_select_all(&["|[a]"], &["[a]|"], arena)?;
        assert_select_all(&["|[abc def ghi]"], &["[abc def ghi]|"], arena)?;
        assert_select_all(&["a\n", "[b\n", "]|"], &["[a\n", "b\n", "]|"], arena)?;
        assert_select_all(&["a\n", "[b]|\n", ""], &["[a\n", "b\n", "]|"], arena)?;
        assert_select_all(&["a\n", "|[b\n", "]"], &["[a\n", "b\n", "]|"], arena)?;
        assert_select_all(
            &["abc\n", "def\n", "gh|i\n", "jkl"],
            &["[abc\n", "def\n", "ghi\n", "jkl]|"],
            arena,
        )?;
        assert_select_all(
            &["|[abc\n", "def\n", "ghi\n", "jkl]"],
            &["[abc\n", "def\n", "ghi\n", "jkl]|"],
            arena,
        )?;

        Ok(())
    }

    // TODO hometest

    // TODO endtest
}
