use winit::event::{ElementState, ModifiersState, VirtualKeyCode};

pub fn handle_text_input(
    text_state: &mut String,
    elem_state: ElementState,
    virtual_keycode: VirtualKeyCode,
    _modifiers: ModifiersState,
) {
    use winit::event::VirtualKeyCode::*;

    if let ElementState::Released = elem_state {
        return;
    }

    match virtual_keycode {
        Key1 | Numpad1 => text_state.push_str("1"),
        Key2 | Numpad2 => text_state.push_str("2"),
        Key3 | Numpad3 => text_state.push_str("3"),
        Key4 | Numpad4 => text_state.push_str("4"),
        Key5 | Numpad5 => text_state.push_str("5"),
        Key6 | Numpad6 => text_state.push_str("6"),
        Key7 | Numpad7 => text_state.push_str("7"),
        Key8 | Numpad8 => text_state.push_str("8"),
        Key9 | Numpad9 => text_state.push_str("9"),
        Key0 | Numpad0 => text_state.push_str("0"),
        A => text_state.push_str("a"),
        B => text_state.push_str("b"),
        C => text_state.push_str("c"),
        D => text_state.push_str("d"),
        E => text_state.push_str("e"),
        F => text_state.push_str("f"),
        G => text_state.push_str("g"),
        H => text_state.push_str("h"),
        I => text_state.push_str("i"),
        J => text_state.push_str("j"),
        K => text_state.push_str("k"),
        L => text_state.push_str("l"),
        M => text_state.push_str("m"),
        N => text_state.push_str("n"),
        O => text_state.push_str("o"),
        P => text_state.push_str("p"),
        Q => text_state.push_str("q"),
        R => text_state.push_str("r"),
        S => text_state.push_str("s"),
        T => text_state.push_str("t"),
        U => text_state.push_str("u"),
        V => text_state.push_str("v"),
        W => text_state.push_str("w"),
        X => text_state.push_str("x"),
        Y => text_state.push_str("y"),
        Z => text_state.push_str("z"),
        Escape | F1 | F2 | F3 | F4 | F5 | F6 | F7 | F8 | F9 | F10 | F11 | F12 | F13 | F14 | F15
        | F16 | F17 | F18 | F19 | F20 | F21 | F22 | F23 | F24 | Snapshot | Scroll | Pause
        | Insert | Home | Delete | End | PageDown | PageUp | Left | Up | Right | Down | Compose
        | Caret | Numlock | AbntC1 | AbntC2 | Ax | Calculator | Capital | Convert | Kana
        | Kanji | LAlt | LBracket | LControl | LShift | LWin | Mail | MediaSelect | PlayPause
        | Power | PrevTrack | MediaStop | Mute | MyComputer | NavigateForward
        | NavigateBackward | NextTrack | NoConvert | OEM102 | RAlt | Sysrq | RBracket
        | RControl | RShift | RWin | Sleep | Stop | Unlabeled | VolumeDown | VolumeUp | Wake
        | WebBack | WebFavorites | WebForward | WebHome | WebRefresh | WebSearch | Apps | Tab
        | WebStop => {
            // TODO handle
            dbg!(virtual_keycode);
        }
        Back => {
            text_state.pop();
        }
        Return | NumpadEnter => {
            text_state.push_str("\n");
        }
        Space => {
            text_state.push_str(" ");
        }
        Comma | NumpadComma => {
            text_state.push_str(",");
        }
        Add => {
            text_state.push_str("+");
        }
        Apostrophe => {
            text_state.push_str("'");
        }
        At => {
            text_state.push_str("@");
        }
        Backslash => {
            text_state.push_str("\\");
        }
        Colon => {
            text_state.push_str(":");
        }
        Period | Decimal => {
            text_state.push_str(".");
        }
        Equals | NumpadEquals => {
            text_state.push_str("=");
        }
        Grave => {
            text_state.push_str("`");
        }
        Minus | Subtract => {
            text_state.push_str("-");
        }
        Multiply => {
            text_state.push_str("*");
        }
        Semicolon => {
            text_state.push_str(";");
        }
        Slash | Divide => {
            text_state.push_str("/");
        }
        Underline => {
            text_state.push_str("_");
        }
        Yen => {
            text_state.push_str("¥");
        }
        Copy => {
            todo!("copy");
        }
        Paste => {
            todo!("paste");
        }
        Cut => {
            todo!("cut");
        }
    }
}
