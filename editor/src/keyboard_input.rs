use crate::mvc;
use crate::mvc::ed_model::{EdModel, Position};
use crate::error::EdResult;
use crate::mvc::app_model::AppModel;
use crate::mvc::update::{
    move_caret_down, move_caret_left, move_caret_right, move_caret_up, MoveCaretFun,
};
use winit::event::{ElementState, ModifiersState, VirtualKeyCode};
use winit::event::VirtualKeyCode::*;

pub fn handle_keydown(
    elem_state: ElementState,
    virtual_keycode: VirtualKeyCode,
    modifiers: ModifiersState,
    app_model: &mut AppModel,
) -> EdResult<()> {

    if let ElementState::Released = elem_state {
        return Ok(())
    }

    match virtual_keycode {
        Left => pass_to_focused(app_model, &modifiers, virtual_keycode),
        Up => pass_to_focused(app_model, &modifiers, virtual_keycode),
        Right => pass_to_focused(app_model, &modifiers, virtual_keycode),
        Down => pass_to_focused(app_model, &modifiers, virtual_keycode),

        Copy => handle_copy(app_model),
        Paste => handle_paste(app_model),
        Cut => {
            todo!("cut");
        }

        C => if modifiers.ctrl() {
            handle_copy(app_model)
        } else { Ok(()) },
        V => if modifiers.ctrl() {
            handle_paste(app_model)
        } else { Ok(()) },
        _ => Ok(())
    }
}

fn pass_to_focused(
    app_model: &mut AppModel,
    modifiers: &ModifiersState,
    virtual_keycode: VirtualKeyCode,
) -> EdResult<()> {
    if let Some(ref mut ed_model) = app_model.ed_model_opt {
        if ed_model.has_focus {
            match virtual_keycode {
                Left => handle_arrow(move_caret_left, modifiers, ed_model),
                Up => handle_arrow(move_caret_up, modifiers, ed_model),
                Right => handle_arrow(move_caret_right, modifiers, ed_model),
                Down => handle_arrow(move_caret_down, modifiers, ed_model),
                _ => {}
            }
        }
    }

    Ok(())
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

fn handle_copy(app_model: &mut AppModel) -> EdResult<()> {
    if let Some(ref mut ed_model) = app_model.ed_model_opt {
        if ed_model.has_focus {
            let selected_str_opt = mvc::ed_model::get_selected_str(ed_model)?;

            if let Some(selected_str) = selected_str_opt {
                if let Ok(ref mut clipboard) = app_model.clipboard_res {
                    clipboard.set_content(selected_str.to_owned())?;
                    return Ok(())
                } else if let Err(ref mut e) = app_model.clipboard_res {
                    return Err(e)
                }
            }
        }
    }

    Ok(())
}

fn handle_paste(app_model: &mut AppModel) -> EdResult<()> {

    if let Some(ref mut ed_model) = app_model.ed_model_opt {
        if ed_model.has_focus {
            let clipboard_content = app_model.clipboard_res?.get_content()?;
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
        }
    }

    Ok(())
}

// pub fn handle_text_input(
//     text_state: &mut String,
//     elem_state: ElementState,
//     virtual_keycode: VirtualKeyCode,
//     _modifiers: ModifiersState,
// ) {
//     use winit::event::VirtualKeyCode::*;

//     if let ElementState::Released = elem_state {
//         return;
//     }

//     match virtual_keycode {
//         Key1 | Numpad1 => text_state.push('1'),
//         Key2 | Numpad2 => text_state.push('2'),
//         Key3 | Numpad3 => text_state.push('3'),
//         Key4 | Numpad4 => text_state.push('4'),
//         Key5 | Numpad5 => text_state.push('5'),
//         Key6 | Numpad6 => text_state.push('6'),
//         Key7 | Numpad7 => text_state.push('7'),
//         Key8 | Numpad8 => text_state.push('8'),
//         Key9 | Numpad9 => text_state.push('9'),
//         Key0 | Numpad0 => text_state.push('0'),
//         A => text_state.push('a'),
//         B => text_state.push('b'),
//         C => text_state.push('c'),
//         D => text_state.push('d'),
//         E => text_state.push('e'),
//         F => text_state.push('f'),
//         G => text_state.push('g'),
//         H => text_state.push('h'),
//         I => text_state.push('i'),
//         J => text_state.push('j'),
//         K => text_state.push('k'),
//         L => text_state.push('l'),
//         M => text_state.push('m'),
//         N => text_state.push('n'),
//         O => text_state.push('o'),
//         P => text_state.push('p'),
//         Q => text_state.push('q'),
//         R => text_state.push('r'),
//         S => text_state.push('s'),
//         T => text_state.push('t'),
//         U => text_state.push('u'),
//         V => text_state.push('v'),
//         W => text_state.push('w'),
//         X => text_state.push('x'),
//         Y => text_state.push('y'),
//         Z => text_state.push('z'),
//         Escape | F1 | F2 | F3 | F4 | F5 | F6 | F7 | F8 | F9 | F10 | F11 | F12 | F13 | F14 | F15
//         | F16 | F17 | F18 | F19 | F20 | F21 | F22 | F23 | F24 | Snapshot | Scroll | Pause
//         | Insert | Home | Delete | End | PageDown | PageUp | Left | Up | Right | Down | Compose
//         | caret | Numlock | AbntC1 | AbntC2 | Ax | Calculator | Capital | Convert | Kana
//         | Kanji | LAlt | LBracket | LControl | LShift | LWin | Mail | MediaSelect | PlayPause
//         | Power | PrevTrack | MediaStop | Mute | MyComputer | NavigateForward
//         | NavigateBackward | NextTrack | NoConvert | OEM102 | RAlt | Sysrq | RBracket
//         | RControl | RShift | RWin | Sleep | Stop | Unlabeled | VolumeDown | VolumeUp | Wake
//         | WebBack | WebFavorites | WebForward | WebHome | WebRefresh | WebSearch | Apps | Tab
//         | WebStop => {
//             // TODO handle
//         }
//         Back => {
//             text_state.pop();
//         }
//         Return | NumpadEnter => {
//             text_state.push('\n');
//         }
//         Space => {
//             text_state.push(' ');
//         }
//         Comma | NumpadComma => {
//             text_state.push(',');
//         }
//         Add => {
//             text_state.push('+');
//         }
//         Apostrophe => {
//             text_state.push('\'');
//         }
//         At => {
//             text_state.push('@');
//         }
//         Backslash => {
//             text_state.push('\\');
//         }
//         Colon => {
//             text_state.push(':');
//         }
//         Period | Decimal => {
//             text_state.push('.');
//         }
//         Equals | NumpadEquals => {
//             text_state.push('=');
//         }
//         Grave => {
//             text_state.push('`');
//         }
//         Minus | Subtract => {
//             text_state.push('-');
//         }
//         Multiply => {
//             text_state.push('*');
//         }
//         Semicolon => {
//             text_state.push(';');
//         }
//         Slash | Divide => {
//             text_state.push('/');
//         }
//         Underline => {
//             text_state.push('_');
//         }
//         Yen => {
//             text_state.push('¥');
//         }
//         Copy => {
//             todo!("copy");
//         }
//         Paste => {
//             todo!("paste");
//         }
//         Cut => {
//             todo!("cut");
//         }
//     }
// }
