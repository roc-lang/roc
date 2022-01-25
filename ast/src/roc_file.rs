use bumpalo::collections::Vec;
use bumpalo::Bump;
use roc_fmt::def::fmt_def;
use roc_fmt::module::fmt_module;
use roc_parse::ast::{Def, Module};
use roc_parse::module::module_defs;
use roc_parse::parser;
use roc_parse::parser::{Parser, SyntaxError};
use roc_region::all::Located;
use std::ffi::OsStr;
use std::path::Path;
use std::{fs, io};

#[derive(Debug)]
pub struct File<'a> {
    path: &'a Path,
    module_header: Module<'a>,
    content: Vec<'a, Located<Def<'a>>>,
}

#[derive(Debug)]
pub enum ReadError<'a> {
    Read(std::io::Error),
    ParseDefs(SyntaxError<'a>),
    ParseHeader(SyntaxError<'a>),
    DoesntHaveRocExtension,
}

impl<'a> File<'a> {
    pub fn read(path: &'a Path, arena: &'a Bump) -> Result<File<'a>, ReadError<'a>> {
        if path.extension() != Some(OsStr::new("roc")) {
            return Err(ReadError::DoesntHaveRocExtension);
        }

        let bytes = fs::read(path).map_err(ReadError::Read)?;

        let allocation = arena.alloc(bytes);

        let module_parse_state = parser::State::new(allocation);
        let parsed_module = roc_parse::module::parse_header(arena, module_parse_state);

        match parsed_module {
            Ok((module, state)) => {
                let parsed_defs = module_defs().parse(arena, state);

                match parsed_defs {
                    Ok((_, defs, _)) => Ok(File {
                        path,
                        module_header: module,
                        content: defs,
                    }),
                    Err((_, error, _)) => Err(ReadError::ParseDefs(error)),
                }
            }
            Err(error) => Err(ReadError::ParseHeader(SyntaxError::Header(error))),
        }
    }

    pub fn fmt(&self) -> String {
        let arena = Bump::new();
        let mut formatted_file = String::new();

        let mut module_header_buf = bumpalo::collections::String::new_in(&arena);
        fmt_module(&mut module_header_buf, &self.module_header);

        formatted_file.push_str(module_header_buf.as_str());

        for def in &self.content {
            let mut def_buf = bumpalo::collections::String::new_in(&arena);

            fmt_def(&mut def_buf, &def.value, 0);

            formatted_file.push_str(def_buf.as_str());
        }

        formatted_file
    }

    pub fn fmt_then_write_to(&self, write_path: &'a Path) -> io::Result<()> {
        let formatted_file = self.fmt();

        fs::write(write_path, formatted_file)
    }

    pub fn fmt_then_write_with_name(&self, new_name: &str) -> io::Result<()> {
        self.fmt_then_write_to(
            self.path
                .with_file_name(new_name)
                .with_extension("roc")
                .as_path(),
        )
    }

    pub fn fmt_then_write(&self) -> io::Result<()> {
        self.fmt_then_write_to(self.path)
    }
}

#[cfg(test)]
mod test_file {
    use crate::lang::roc_file;
    use bumpalo::Bump;
    use std::path::Path;

    #[test]
    fn read_and_fmt_simple_roc_module() {
        let simple_module_path = Path::new("./tests/modules/SimpleUnformatted.roc");

        let arena = Bump::new();

        let file = roc_file::File::read(simple_module_path, &arena)
            .expect("Could not read SimpleUnformatted.roc in test_file test");

        assert_eq!(
            file.fmt(),
            indoc!(
                r#"
                    interface Simple
                        exposes [
                        v, x
                         ]
                        imports []

                    v : Str

                    v = "Value!"

                    x : Int
                    x = 4"#
            )
        );
    }
}
