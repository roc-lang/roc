use std::path::PathBuf;

use bumpalo::collections::String;
use bumpalo::Bump;
use roc_fmt::def::fmt_def;
use roc_fmt::module::fmt_module;
use roc_parse::{
    module::{self, module_defs},
    parser::{Parser, State},
};
use roc_reporting::user_error;

pub fn format(files: Vec<PathBuf>) {
    for file in files {
        let arena = Bump::new();

        let src = std::fs::read_to_string(&file).unwrap();

        match module::parse_header(&arena, State::new(src.as_bytes())) {
            Ok((result, state)) => {
                let mut buf = String::new_in(&arena);

                fmt_module(&mut buf, &result);

                match module_defs().parse(&arena, state) {
                    Ok((_, loc_defs, _)) => {
                        for loc_def in loc_defs {
                            fmt_def(&mut buf, arena.alloc(loc_def.value), 0);
                        }
                    }
                    Err(error) => user_error!("Unexpected parse failure when parsing this for defs formatting:\n\n{:?}\n\nParse error was:\n\n{:?}\n\n", src, error)
                }

                std::fs::write(&file, buf).unwrap();
            }
            Err(error) => user_error!("Unexpected parse failure when parsing this for module header formatting:\n\n{:?}\n\nParse error was:\n\n{:?}\n\n", src, error)
        };
    }
}
