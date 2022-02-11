#![allow(non_snake_case)]

use core::ffi::c_void;
use roc_std::RocStr;
use std::ffi::CStr;
use std::os::raw::c_char;

mod graphics;
mod gui;
mod rects_and_texts;

extern "C" {
    #[link_name = "roc__renderForHost_1_exposed"]
    fn roc_render() -> RocStr;
}

#[repr(C)]
#[derive(Debug)]
struct RocElem {
    tag: u8,
    string: RocStr,
}

#[no_mangle]
pub unsafe extern "C" fn roc_alloc(size: usize, _alignment: u32) -> *mut c_void {
    return libc::malloc(size);
}

#[no_mangle]
pub unsafe extern "C" fn roc_realloc(
    c_ptr: *mut c_void,
    new_size: usize,
    _old_size: usize,
    _alignment: u32,
) -> *mut c_void {
    return libc::realloc(c_ptr, new_size);
}

#[no_mangle]
pub unsafe extern "C" fn roc_dealloc(c_ptr: *mut c_void, _alignment: u32) {
    return libc::free(c_ptr);
}

#[no_mangle]
pub unsafe extern "C" fn roc_panic(c_ptr: *mut c_void, tag_id: u32) {
    match tag_id {
        0 => {
            let slice = CStr::from_ptr(c_ptr as *const c_char);
            let string = slice.to_str().unwrap();
            eprintln!("Roc hit a panic: {}", string);
            std::process::exit(1);
        }
        _ => todo!(),
    }
}

#[no_mangle]
pub unsafe extern "C" fn roc_memcpy(dst: *mut c_void, src: *mut c_void, n: usize) -> *mut c_void {
    libc::memcpy(dst, src, n)
}

#[no_mangle]
pub unsafe extern "C" fn roc_memset(dst: *mut c_void, c: i32, n: usize) -> *mut c_void {
    libc::memset(dst, c, n)
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
    let roc_str = unsafe { roc_render() };

    fn render(clicks: i64) -> Elem<i64> {
        let txt = Elem::Text(Key::null(), format!("Clicks: {}", clicks).as_str().into());

        Elem::Button(
            Key::null(),
            Box::new(move || Action::Update(clicks + 1)),
            Box::new(txt),
        )
    }

    fn draw_elem<T>(elem: Elem<T>) {
        use Elem::*;

        match elem {
            Button(_key, _on_click, label) => {
                print!("Drawing button label:\n\t");

                draw_elem(*label);
            }
            Text(_key, roc_str) => {
                println!("Drawing string \"{}\"", roc_str);
            }
            Col(_key, elems) => {
                println!("Drawing col contents...");

                for elem in elems {
                    draw_elem(elem);
                }
            }
            Row(_key, elems) => {
                println!("Drawing row contents...");

                for elem in elems {
                    draw_elem(elem);
                }
            }
            TextInput {
                key: _,
                text,
                on_change: _,
            } => {
                println!("Drawing text input with current text \"{}\"", text);
            }
        }
    }

    draw_elem(render(0));

    gui::render(roc_str);

    // Exit code
    0
}
