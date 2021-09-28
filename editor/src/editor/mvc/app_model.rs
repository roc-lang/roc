#![allow(dead_code)]

use super::ed_model::EdModel;
use crate::editor::ed_error::{
    print_err,
    EdError::{ClipboardInitFailed, ClipboardReadFailed, ClipboardWriteFailed},
    EdResult,
};
use copypasta::{ClipboardContext, ClipboardProvider};
use rodio::{Decoder, OutputStream, OutputStreamHandle};
use std::{fmt, fs::File, io::BufReader};

pub struct AppModel<'a> {
    pub ed_model_opt: Option<EdModel<'a>>,
    pub clipboard_opt: Option<Clipboard>,
    pub sound_output_stream_opt: Option<OutputStreamHandle>,
    pub sound_file_name_opt: Option<&'static str>
}

impl<'a> AppModel<'a> {
    pub fn init(ed_model_opt: Option<EdModel<'a>>, sound_output_stream_opt: Option<OutputStreamHandle>) -> AppModel {

        /*let (sound_output_stream_opt, sound_file_name_opt) =
            if let Some((sound_output_stream, sound_file_name)) = setup_sound() {
                (Some(sound_output_stream), Some(sound_file_name))
            } else {
                (None, None)
            };*/

        AppModel {
            ed_model_opt,
            clipboard_opt: AppModel::init_clipboard_opt(),
            sound_output_stream_opt,
            sound_file_name_opt: None
        }
    }

    pub fn init_clipboard_opt() -> Option<Clipboard> {
        let clipboard_res = Clipboard::init();

        match clipboard_res {
            Ok(clipboard) => Some(clipboard),
            Err(e) => {
                print_err(&e);
                None
            }
        }
    }
}

pub struct Clipboard {
    context: ClipboardContext,
}

impl Clipboard {
    pub fn init() -> EdResult<Clipboard> {
        let context_res = ClipboardContext::new();

        match context_res {
            Ok(context) => Ok(Clipboard { context }),
            Err(e) => Err(ClipboardInitFailed {
                err_msg: e.to_string(),
            }),
        }
    }

    // clipboard crate needs this to be mutable
    pub fn get_content(&mut self) -> EdResult<String> {
        let content_res = self.context.get_contents();

        match content_res {
            Ok(content_str) => Ok(content_str),
            Err(e) => Err(ClipboardReadFailed {
                err_msg: e.to_string(),
            }),
        }
    }

    pub fn set_content(&mut self, copy_str: String) -> EdResult<()> {
        let content_set_res = self.context.set_contents(copy_str);

        match content_set_res {
            Ok(_) => Ok(()),
            Err(e) => Err(ClipboardWriteFailed {
                err_msg: e.to_string(),
            }),
        }
    }
}

pub fn set_clipboard_txt(clipboard_opt: &mut Option<Clipboard>, txt: &str) -> EdResult<()> {
    if let Some(ref mut clipboard) = clipboard_opt {
        clipboard.set_content(txt.to_owned())?;
    } else {
        return Err(ClipboardWriteFailed {
            err_msg: "Clipboard was never initialized successfully.".to_owned(),
        });
    }

    Ok(())
}

pub fn get_clipboard_txt(clipboard_opt: &mut Option<Clipboard>) -> EdResult<String> {
    if let Some(ref mut clipboard) = clipboard_opt {
        clipboard.get_content()
    } else {
        Err(ClipboardReadFailed {
            err_msg: "Clipboard was never initialized successfully.".to_owned(),
        })
    }
}

fn setup_sound() -> Option<(OutputStreamHandle, &'static str)> {
    // TODO no unwrap
    //let (_, stream_handle) = OutputStream::try_default().unwrap();
    // Load a sound from a file, using a path relative to Cargo.toml
    let file_path_str = "./editor/src/editor/resources/sounds/bell_sound.mp3";
    
    //Some((stream_handle, file_path_str))
    None
}

impl fmt::Debug for Clipboard {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // showing the clipboard would require a mut ref which is not possible
        f.debug_struct("Clipboard (can't show)").finish()
    }
}
