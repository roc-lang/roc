mod graphics;
mod gui;
mod rects_and_texts;
mod roc;

use crate::roc::RocElem;
use roc_std::RocStr;

extern "C" {
    #[link_name = "roc__renderForHost_1_exposed"]
    fn roc_render() -> RocElem;
}

enum Action<State> {
    Update(State),
    DoNothing,
}

enum Elem<State> {
    Button(Key, Box<dyn Fn() -> Action<State>>, Box<Elem<State>>),
    Text(Key, RocStr),
    TextInput {
        key: Key,
        /// current text that's been entered
        text: RocStr,
        /// event handler to run when the user changes the text
        on_change: Box<dyn Fn(RocStr) -> Action<State>>,
    },
    Col(Key, Vec<Elem<State>>),
    Row(Key, Vec<Elem<State>>),
}

/// Either a number between 0 and `isize::MAX`,
/// or a "null" value (meaning no number was specified).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct Key(usize);

impl Key {
    const NULL: Self = Self(usize::MAX);

    fn new(val: usize) -> Self {
        debug_assert!(val <= isize::MAX as usize);

        Key(val)
    }

    fn null() -> Self {
        Self::NULL
    }

    fn is_null(self) -> bool {
        self == Self::NULL
    }
}

struct AppState {
    /// glyph index of caret position
    caret: usize,
    /// number of glyphs selected (0 means it's a normal caret)
    selected: usize,
}

#[no_mangle]
pub extern "C" fn rust_main() -> i32 {
    let root_elem = unsafe { roc_render() };

    gui::render("test title".into(), root_elem);

    // Exit code
    0
}
