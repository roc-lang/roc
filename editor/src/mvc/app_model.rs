use super::ed_model::EdModel;
use crate::error::{EdResult, print_err};
use crate::error::EdError::{ClipboardReadFailed, ClipboardWriteFailed, ClipboardInitFailed};
use clipboard::{ClipboardContext, ClipboardProvider};
use std::fmt;


#[derive(Debug)]
pub struct AppModel {
    pub ed_model_opt: Option<EdModel>,
    pub clipboard_opt: Option<Clipboard>,
}

impl AppModel {
    pub fn init(ed_model_opt: Option<EdModel>) -> AppModel {
        let clipboard_res = Clipboard::init();

        let clipboard_opt =
            match clipboard_res {
                Ok(clipboard) => Some(clipboard),
                Err(e) => {
                    print_err(&e);
                    None
                }

            };

        AppModel {
            ed_model_opt,
            clipboard_opt
        }
    }
}

pub struct Clipboard {
    context: ClipboardContext
}

impl Clipboard {

    pub fn init() -> EdResult<Clipboard> {
        let context_res = ClipboardProvider::new();

        match context_res {
            Ok(context) => Ok(
                Clipboard {
                    context
                }
            ),
            Err(e) => Err(ClipboardInitFailed {
                err_msg: e.to_string()
            })
        }
    }

    // clipboard crate needs this to be mutable
    pub fn get_content(&mut self) -> EdResult<String> {
        let content_res = self.context.get_contents();

        match content_res {
            Ok(content_str) => Ok(content_str),
            Err(e) => Err(ClipboardReadFailed {
                err_msg: e.to_string()
            })
        }
    }

    pub fn set_content(&mut self, copy_str: String) -> EdResult<()> {
        let content_set_res = self.context.set_contents(copy_str);

        match content_set_res {
            Ok(_) => Ok(()),
            Err(e) => Err(ClipboardWriteFailed {
                err_msg: e.to_string()
            })
        }
    }
}

impl fmt::Debug for Clipboard {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // showing the clipboard would require a mut ref which is not possible
        f.debug_struct("Clipboard (can't show)")
         .finish()
    }
}
