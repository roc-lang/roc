use super::app_model;
use super::app_model::AppModel;
use super::ed_model::Position;
use super::ed_update;
use crate::error::EdResult;
use winit::event::{ModifiersState, VirtualKeyCode};

pub fn handle_copy(app_model: &mut AppModel) -> EdResult<()> {
    if let Some(ref mut ed_model) = app_model.ed_model_opt {
        if ed_model.has_focus {
            let selected_str_opt = super::ed_model::get_selected_str(ed_model)?;

            if let Some(selected_str) = selected_str_opt {
                app_model::set_clipboard_txt(&mut app_model.clipboard_opt, selected_str)?;
            }
        }
    }

    Ok(())
}

pub fn handle_paste(app_model: &mut AppModel) -> EdResult<()> {
    if let Some(ref mut ed_model) = app_model.ed_model_opt {
        if ed_model.has_focus {
            let clipboard_content = app_model::get_clipboard_txt(&mut app_model.clipboard_opt)?;

            if !clipboard_content.is_empty() {
                let mut rsplit_iter = clipboard_content.rsplit('\n');
                // safe unwrap because we checked if empty
                let last_line_nr_chars = rsplit_iter.next().unwrap().len();
                let clipboard_nr_lines = rsplit_iter.count();

                let old_caret_pos = ed_model.caret_pos;

                if let Some(selection) = ed_model.selection_opt {
                    let start_caret_pos = selection.start_pos;
                    ed_model.text_buf.del_selection(selection)?;
                    ed_model.selection_opt = None;

                    ed_model
                        .text_buf
                        .insert_str(start_caret_pos, &clipboard_content)?;

                    if clipboard_nr_lines > 0 {
                        ed_model.caret_pos = Position {
                            line: start_caret_pos.line + clipboard_nr_lines,
                            column: last_line_nr_chars,
                        }
                    } else {
                        ed_model.caret_pos = Position {
                            line: start_caret_pos.line,
                            column: start_caret_pos.column + last_line_nr_chars,
                        }
                    }
                } else {
                    ed_model
                        .text_buf
                        .insert_str(old_caret_pos, &clipboard_content)?;

                    if clipboard_nr_lines > 0 {
                        ed_model.caret_pos = Position {
                            line: old_caret_pos.line + clipboard_nr_lines,
                            column: last_line_nr_chars,
                        }
                    } else {
                        ed_model.caret_pos = Position {
                            line: old_caret_pos.line,
                            column: old_caret_pos.column + last_line_nr_chars,
                        }
                    }
                }
            }
        }
    }

    Ok(())
}

pub fn pass_keydown_to_focused(
    modifiers: &ModifiersState,
    virtual_keycode: VirtualKeyCode,
    app_model: &mut AppModel,
) {
    if let Some(ref mut ed_model) = app_model.ed_model_opt {
        if ed_model.has_focus {
            ed_update::handle_key_down(modifiers, virtual_keycode, ed_model);
        }
    }
}

pub fn handle_new_char(received_char: &char, app_model: &mut AppModel) -> EdResult<()> {
    if let Some(ref mut ed_model) = app_model.ed_model_opt {
        if ed_model.has_focus {
            ed_update::handle_new_char(received_char, ed_model)?;
        }
    }

    Ok(())
}

#[cfg(test)]
pub mod test_app_update {
    use crate::mvc::app_model;
    use crate::mvc::app_model::{AppModel, Clipboard};
    use crate::mvc::app_update::{handle_copy, handle_paste};
    use crate::mvc::ed_model::{EdModel, Position, RawSelection};
    use crate::mvc::ed_update::test_ed_update::gen_caret_text_buf;
    use crate::selection::test_selection::{all_lines_vec, convert_selection_to_dsl};
    use crate::text_buffer::TextBuffer;

    pub fn mock_app_model(
        text_buf: TextBuffer,
        caret_pos: Position,
        selection_opt: Option<RawSelection>,
        clipboard_opt: Option<Clipboard>,
    ) -> AppModel {
        AppModel {
            ed_model_opt: Some(EdModel {
                text_buf,
                caret_pos,
                selection_opt,
                glyph_dim_rect_opt: None,
                has_focus: true,
            }),
            clipboard_opt,
        }
    }

    fn assert_copy(
        pre_lines_str: &[&str],
        expected_clipboard_content: &str,
        clipboard_opt: Option<Clipboard>,
    ) -> Result<Option<Clipboard>, String> {
        let (caret_pos, selection_opt, pre_text_buf) = gen_caret_text_buf(pre_lines_str)?;

        let mut app_model = mock_app_model(pre_text_buf, caret_pos, selection_opt, clipboard_opt);

        handle_copy(&mut app_model)?;

        let clipboard_content = app_model::get_clipboard_txt(&mut app_model.clipboard_opt)?;

        assert_eq!(clipboard_content, expected_clipboard_content);

        Ok(app_model.clipboard_opt)
    }

    fn assert_paste(
        pre_lines_str: &[&str],
        clipboard_content: &str,
        expected_post_lines_str: &[&str],
        clipboard_opt: Option<Clipboard>,
    ) -> Result<Option<Clipboard>, String> {
        let (caret_pos, selection_opt, pre_text_buf) = gen_caret_text_buf(pre_lines_str)?;

        let mut app_model = mock_app_model(pre_text_buf, caret_pos, selection_opt, clipboard_opt);

        app_model::set_clipboard_txt(&mut app_model.clipboard_opt, clipboard_content)?;

        handle_paste(&mut app_model)?;

        let ed_model = app_model.ed_model_opt.unwrap();
        let mut text_buf_lines = all_lines_vec(&ed_model.text_buf);
        let post_lines_str = convert_selection_to_dsl(
            ed_model.selection_opt,
            ed_model.caret_pos,
            &mut text_buf_lines,
        )?;

        assert_eq!(post_lines_str, expected_post_lines_str);

        Ok(app_model.clipboard_opt)
    }

    #[test]
    #[ignore] // ignored because of clipboard problems on ci
    fn copy_paste() -> Result<(), String> {
        // can only init clipboard once
        let mut clipboard_opt = AppModel::init_clipboard_opt();

        clipboard_opt = assert_copy(&["[a]|"], "a", clipboard_opt)?;
        clipboard_opt = assert_copy(&["|[b]"], "b", clipboard_opt)?;
        clipboard_opt = assert_copy(&["a[ ]|"], " ", clipboard_opt)?;
        clipboard_opt = assert_copy(&["[ ]|b"], " ", clipboard_opt)?;
        clipboard_opt = assert_copy(&["a\n", "[b\n", "]|"], "b\n", clipboard_opt)?;
        clipboard_opt = assert_copy(&["[a\n", " b\n", "]|"], "a\n b\n", clipboard_opt)?;
        clipboard_opt = assert_copy(
            &["abc\n", "d[ef\n", "ghi]|\n", "jkl"],
            "ef\nghi",
            clipboard_opt,
        )?;
        clipboard_opt = assert_paste(&["|"], "", &["|"], clipboard_opt)?;
        clipboard_opt = assert_paste(&["|"], "a", &["a|"], clipboard_opt)?;
        clipboard_opt = assert_paste(&["a|"], "b", &["ab|"], clipboard_opt)?;
        clipboard_opt = assert_paste(&["|a"], "b", &["b|a"], clipboard_opt)?;
        clipboard_opt = assert_paste(&["[a]|"], "c", &["c|"], clipboard_opt)?;
        clipboard_opt = assert_paste(&["[ab]|"], "d", &["d|"], clipboard_opt)?;
        clipboard_opt = assert_paste(&["a[b]|c"], "e", &["ae|c"], clipboard_opt)?;
        clipboard_opt = assert_paste(&["a\n", "[b\n", "]|"], "f", &["a\n", "f|"], clipboard_opt)?;
        assert_paste(
            &["abc\n", "d[ef\n", "ghi]|\n", "jkl"],
            "ef\nghi",
            &["abc\n", "def\n", "ghi|\n", "jkl"],
            clipboard_opt,
        )?;

        Ok(())
    }
}
