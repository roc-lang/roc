use std::{path::{PathBuf, Path}, fs};

use bumpalo::Bump;
use roc_can::def::{CanDefs, Def, Declaration};
use roc_load::{LoadedModule, LoadStart, Phase, Threading};
use roc_load_internal::file::ModuleHeader;
use roc_constrain::module::ExposedByModule;
use roc_reporting::report::RenderTarget;
use roc_target::TargetInfo;

#[derive(Debug)]
pub struct AST<'a> {
    pub module_header: ModuleHeader<'a>,
    pub defs: Vec<Def>
}

impl<'a> AST<'a> {
    pub fn insert_def_at_index(&mut self, _new_def: Def, _index: usize) {
        unimplemented!("TODO")
    }

    // TODO print in tree shape, similar to linux tree command
    pub fn ast_to_string(&self) -> String {
        let mut full_ast_string = String::new();

        for def in self.defs.iter() {
            full_ast_string.push_str(&format!("{:?}", def));
            full_ast_string.push_str("\n\n");
        }

        full_ast_string
    }
}

pub fn build_ast<'a>(
    loaded_module: LoadedModule
) -> AST<'a> {
    // TODO remove unwrap
    let declarations =
        loaded_module.declarations_by_id.get(
            &loaded_module.module_id
        ).unwrap();

    let defs =
        declarations.iter().filter_map(|dec| {
            match dec {
                Declaration::Declare(def) => Some(*def),
                _ => unimplemented!("TODO handle other varianst of Declaration"),
            }
        }).collect();

    AST {
        module_header: (), // TODO find this somewhere
        defs
    }
}
