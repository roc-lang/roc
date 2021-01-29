use super::app_model::AppModel;
use super::ed_update;
use super::ed_model::{Position};
use crate::error::EdResult;
use crate::error::EdError::{ClipboardWriteFailed, ClipboardReadFailed};
use winit::event::{ModifiersState, VirtualKeyCode};


pub fn handle_copy(app_model: &mut AppModel) -> EdResult<()> {
    if let Some(ref mut ed_model) = app_model.ed_model_opt {
        if ed_model.has_focus {
            let selected_str_opt = super::ed_model::get_selected_str(ed_model)?;

            if let Some(selected_str) = selected_str_opt {
                if let Some(ref mut clipboard) = app_model.clipboard_opt {
                    clipboard.set_content(selected_str.to_owned())?;
                } else {
                    return Err(ClipboardWriteFailed {
                        err_msg: "Clipboard was never initialized succesfully.".to_owned()
                    })
                }                
            }
        }
    }

    Ok(())
}

pub fn handle_paste(app_model: &mut AppModel) -> EdResult<()> {

    if let Some(ref mut ed_model) = app_model.ed_model_opt {
        if ed_model.has_focus {
            if let Some(ref mut clipboard) = app_model.clipboard_opt {
                let clipboard_content = clipboard.get_content()?;

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

                        ed_model.text_buf.insert_str(
                            start_caret_pos,
                            &clipboard_content
                        )?;

                        ed_model.caret_pos = Position {
                            line: start_caret_pos.line + clipboard_nr_lines,
                            column: start_caret_pos.column + last_line_nr_chars
                        }
                    } else {
                        ed_model.text_buf.insert_str(
                            old_caret_pos,
                            &clipboard_content
                        )?;

                        ed_model.caret_pos = Position {
                            line: old_caret_pos.line + clipboard_nr_lines,
                            column: old_caret_pos.column + last_line_nr_chars
                        }
                    }
                }

            } else {
                return Err(ClipboardReadFailed {
                    err_msg: "Clipboard was never initialized succesfully.".to_owned()
                })
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

pub fn handle_new_char(received_char: &char, app_model: &mut AppModel) -> EdResult<()>  {
    if let Some(ref mut ed_model) = app_model.ed_model_opt {
        if ed_model.has_focus {
            ed_update::handle_new_char(received_char, ed_model)?;
        }
    }

    Ok(())
}

#[cfg(test)]
pub mod test_app_update {
    use crate::mvc::app_model::{AppModel, Clipboard};
    use crate::mvc::app_update::{handle_copy, handle_paste};
    use crate::mvc::ed_update::test_ed_update::{gen_caret_text_buf};
    use crate::mvc::ed_model::{EdModel, Position, RawSelection};
    use crate::selection::test_selection::{convert_selection_to_dsl, all_lines_vec};
    use crate::text_buffer::TextBuffer;
    use crate::error::EdResult;
    use crate::error::EdError::ClipboardInitFailed;

    pub fn mock_app_model(
        text_buf: TextBuffer,
        caret_pos: Position,
        selection_opt: Option<RawSelection>,
    ) -> AppModel {
        AppModel::init(
            Some(
                EdModel {
                    text_buf,
                    caret_pos,
                    selection_opt,
                    glyph_dim_rect_opt: None,
                    has_focus: true,
                }
            )
        )
    }

    fn get_clipboard(app_model: &mut AppModel) -> EdResult<&mut Clipboard> {
        if let Some(ref mut clipboard) = app_model.clipboard_opt {
            Ok(clipboard)
        } else {
            Err(ClipboardInitFailed {
                err_msg: "Clipboard was never initialized succesfully.".to_owned()
            })
        }   
    }

    fn assert_copy(
        pre_lines_str: &[&str],
        expected_clipboard_content: &str,
    ) -> Result<(), String> {
        let (caret_pos, selection_opt, pre_text_buf) = gen_caret_text_buf(pre_lines_str)?;

        let mut app_model = mock_app_model(pre_text_buf, caret_pos, selection_opt);

        handle_copy(&mut app_model)?;

        let clipboard = get_clipboard(&mut app_model)?;

        assert_eq!(
            clipboard.get_content()?,
            expected_clipboard_content
        );

        Ok(())
    }

    fn assert_paste(
        pre_lines_str: &[&str],
        clipboard_content: &str,
        expected_post_lines_str: &[&str],
    ) -> Result<(), String> {
        let (caret_pos, selection_opt, pre_text_buf) = gen_caret_text_buf(pre_lines_str)?;

        let mut app_model = mock_app_model(pre_text_buf, caret_pos, selection_opt);
        let clipboard = get_clipboard(&mut app_model)?;
        clipboard.set_content(clipboard_content.to_owned())?;

        handle_paste(&mut app_model)?;

        let ed_model = app_model.get_ed_model()?;
        let mut text_buf_lines = all_lines_vec(&ed_model.text_buf);
        let post_lines_str = convert_selection_to_dsl(
            ed_model.selection_opt,
            ed_model.caret_pos,
            &mut text_buf_lines
        )?;

        assert_eq!(
            post_lines_str,
            expected_post_lines_str
        );

        Ok(())
    }

    #[test]
    fn copy() -> Result<(), String> {
        assert_copy(&["[a]|"], "a")?;
        assert_copy(&["|[b]"], "b")?;
        assert_copy(&["a[ ]|"], " ")?;
        assert_copy(&["[ ]|b"], " ")?;
        assert_copy(&["a\n", "[b\n", "]|"], "b\n")?;
        assert_copy(&["[a\n", " b\n", "]|"], "a\n b\n")?;
        assert_copy(&["abc\n", "d[ef\n", "ghi]|\n", "jkl"], "ef\nghi")?;

        Ok(())
    }

    #[test]
    fn paste() -> Result<(), String> {
        assert_paste(&["|"], "", &["|"])?;

        Ok(())
    }
}