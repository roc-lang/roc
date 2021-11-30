use super::app_model::AppModel;
use super::ed_update;
use crate::window::keyboard_input::Modifiers;
use crate::{editor::ed_error::EdResult, window::keyboard_input::from_winit};
use winit::event::{ModifiersState, VirtualKeyCode};

pub fn handle_copy(app_model: &mut AppModel) -> EdResult<()> {
    if let Some(ref mut ed_model) = app_model.ed_model_opt {
        if ed_model.has_focus {
            unimplemented!("TODO");
        }
    }

    Ok(())
}

pub fn handle_paste(app_model: &mut AppModel) -> EdResult<()> {
    if let Some(ref mut ed_model) = app_model.ed_model_opt {
        if ed_model.has_focus {
            unimplemented!("TODO");
        }
    }

    Ok(())
}

pub fn handle_cut(app_model: &mut AppModel) -> EdResult<()> {
    if let Some(ref mut ed_model) = app_model.ed_model_opt {
        if ed_model.has_focus {
            unimplemented!("TODO");
        }
    }

    Ok(())
}

pub fn pass_keydown_to_focused(
    modifiers: &Modifiers,
    virtual_keycode: VirtualKeyCode,
    app_model: &mut AppModel,
) -> EdResult<()> {
    if let Some(ref mut ed_model) = app_model.ed_model_opt {
        if ed_model.has_focus {
            ed_model.ed_handle_key_down(
                modifiers,
                virtual_keycode,
                &mut app_model.sound_thread_pool,
            )?;
        }
    }

    Ok(())
}

pub enum InputOutcome {
    Accepted,
    Ignored,
    SilentIgnored,
}

pub fn handle_new_char(
    received_char: &char,
    app_model: &mut AppModel,
    modifiers_winit: ModifiersState,
) -> EdResult<InputOutcome> {
    if let Some(ref mut ed_model) = app_model.ed_model_opt {
        if ed_model.has_focus {
            let modifiers = from_winit(&modifiers_winit);

            if modifiers.new_char_modifiers() {
                // shortcuts with modifiers are handled by ed_handle_key_down
                return ed_update::handle_new_char(received_char, ed_model);
            }
        }
    }

    Ok(InputOutcome::SilentIgnored)
}

/*
#[cfg(test)]
pub mod test_app_update {
    use crate::editor::mvc::app_model;
    use crate::editor::mvc::app_model::{AppModel, Clipboard};
    use crate::editor::mvc::app_update::{handle_copy, handle_cut, handle_paste};
    use crate::editor::mvc::ed_model::EdModel;
    use crate::ui::text::{
        big_selectable_text::test_big_sel_text::{
            all_lines_vec, convert_selection_to_dsl, gen_big_text,
        },
        big_selectable_text::BigSelectableText,
    };

    pub fn mock_app_model(
        big_sel_text: BigSelectableText,
        clipboard_opt: Option<Clipboard>,
    ) -> AppModel {
        AppModel {
            ed_model_opt: Some(EdModel {
                text: big_sel_text,
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
        let pre_text_buf = gen_big_text(pre_lines_str)?;

        let mut app_model = mock_app_model(pre_text_buf, clipboard_opt);

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
        let pre_big_text = gen_big_text(pre_lines_str)?;

        let mut app_model = mock_app_model(pre_big_text, clipboard_opt);

        app_model::set_clipboard_txt(&mut app_model.clipboard_opt, clipboard_content)?;

        handle_paste(&mut app_model)?;

        let ed_model = app_model.ed_model_opt.unwrap();
        let mut text_lines = all_lines_vec(&ed_model.text);
        let post_lines_str =
            convert_selection_to_dsl(ed_model.text.caret_w_select, &mut text_lines)?;

        assert_eq!(post_lines_str, expected_post_lines_str);

        Ok(app_model.clipboard_opt)
    }

    fn assert_cut(
        pre_lines_str: &[&str],
        expected_clipboard_content: &str,
        expected_post_lines_str: &[&str],
        clipboard_opt: Option<Clipboard>,
    ) -> Result<Option<Clipboard>, String> {
        let pre_big_text = gen_big_text(pre_lines_str)?;

        let mut app_model = mock_app_model(pre_big_text, clipboard_opt);

        handle_cut(&mut app_model)?;

        let clipboard_content = app_model::get_clipboard_txt(&mut app_model.clipboard_opt)?;

        assert_eq!(clipboard_content, expected_clipboard_content);

        let ed_model = app_model.ed_model_opt.unwrap();
        let mut text_lines = all_lines_vec(&ed_model.text);
        let post_lines_str =
            convert_selection_to_dsl(ed_model.text.caret_w_select, &mut text_lines)?;

        assert_eq!(post_lines_str, expected_post_lines_str);

        Ok(app_model.clipboard_opt)
    }

    #[test]
    #[ignore] // ignored because of clipboard problems on ci
    fn copy_paste_cut() -> Result<(), String> {
        // can only init clipboard once
        let mut clipboard_opt = AppModel::init_clipboard_opt();

        // copy
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

        // paste

        clipboard_opt = assert_paste(&["|"], "", &["|"], clipboard_opt)?;
        clipboard_opt = assert_paste(&["|"], "a", &["a|"], clipboard_opt)?;
        clipboard_opt = assert_paste(&["a|"], "b", &["ab|"], clipboard_opt)?;
        clipboard_opt = assert_paste(&["|a"], "b", &["b|a"], clipboard_opt)?;
        clipboard_opt = assert_paste(&["[a]|"], "c", &["c|"], clipboard_opt)?;
        clipboard_opt = assert_paste(&["[ab]|"], "d", &["d|"], clipboard_opt)?;
        clipboard_opt = assert_paste(&["a[b]|c"], "e", &["ae|c"], clipboard_opt)?;
        clipboard_opt = assert_paste(&["a\n", "[b\n", "]|"], "f", &["a\n", "f|"], clipboard_opt)?;
        clipboard_opt = assert_paste(
            &["abc\n", "d[ef\n", "ghi]|\n", "jkl"],
            "ef\nghi",
            &["abc\n", "def\n", "ghi|\n", "jkl"],
            clipboard_opt,
        )?;

        // cut
        clipboard_opt = assert_cut(&["[a]|"], "a", &["|"], clipboard_opt)?;
        clipboard_opt = assert_cut(&["|[b]"], "b", &["|"], clipboard_opt)?;
        clipboard_opt = assert_cut(&["a[ ]|"], " ", &["a|"], clipboard_opt)?;
        clipboard_opt = assert_cut(&["[ ]|b"], " ", &["|b"], clipboard_opt)?;
        clipboard_opt = assert_cut(&["a\n", "[b\n", "]|"], "b\n", &["a\n", "|"], clipboard_opt)?;
        clipboard_opt = assert_cut(&["[a\n", " b\n", "]|"], "a\n b\n", &["|"], clipboard_opt)?;
        assert_cut(
            &["abc\n", "d[ef\n", "ghi]|\n", "jkl"],
            "ef\nghi",
            &["abc\n", "d|\n", "jkl"],
            clipboard_opt,
        )?;

        Ok(())
    }
}*/
