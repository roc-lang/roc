use crate::editor::ed_error::EdResult;
use crate::editor::mvc::app_model::AppModel;
use crate::editor::mvc::app_update::{
    handle_copy, handle_cut, handle_paste, pass_keydown_to_focused,
};
use crate::window::keyboard_input::from_winit;
use winit::event::VirtualKeyCode::*;
use winit::event::{ElementState, ModifiersState, VirtualKeyCode};

pub fn handle_keydown(
    elem_state: ElementState,
    virtual_keycode: VirtualKeyCode,
    modifiers_winit: ModifiersState,
    app_model: &mut AppModel,
) -> EdResult<()> {
    if let ElementState::Released = elem_state {
        return Ok(());
    }

    let modifiers = from_winit(&modifiers_winit);

    match virtual_keycode {
        Left | Up | Right | Down => {
            pass_keydown_to_focused(&modifiers, virtual_keycode, app_model)?
        }

        Copy => handle_copy(app_model)?,
        Paste => handle_paste(app_model)?,
        Cut => handle_cut(app_model)?,
        C => {
            if modifiers.cmd_or_ctrl() {
                handle_copy(app_model)?
            }
        }
        V => {
            if modifiers.cmd_or_ctrl() {
                handle_paste(app_model)?
            }
        }
        X => {
            if modifiers.cmd_or_ctrl() {
                handle_cut(app_model)?
            }
        }

        _ => pass_keydown_to_focused(&modifiers, virtual_keycode, app_model)?,
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
