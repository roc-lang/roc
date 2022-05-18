use roc_can::def::{CanDefs, Def};
use roc_load::LoadedModule;
use roc_load_internal::file::ModuleHeader;

#[derive(Debug)]
pub struct AST<'a> {
    pub module_header: ModuleHeader<'a>,
    pub can_defs: CanDefs
}

impl<'a> AST<'a> {
    pub fn insert_def_at_index(&mut self, _new_def: Def, _index: usize) {
        unimplemented!("TODO")
    }

    // TODO print in tree shape, similar to linux tree command
    pub fn ast_to_string(&self) -> String {
        let mut full_ast_string = String::new();

        for def in self.can_defs.defs.iter() {
            full_ast_string.push_str(&format!("{:?}", def));
            full_ast_string.push_str("\n\n");
        }

        full_ast_string
    }
}

pub fn build_ast<'a>(loaded_module: LoadedModule) -> AST<'a> {
    //TODO file.rs load with goal phase CanAndConstrain, defs are in declarations
}
