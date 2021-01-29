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