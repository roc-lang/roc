use roc_can::def::{Def, Declaration};
use roc_load::LoadedModule;
use roc_load_internal::file::ModuleHeader;

#[derive(Debug)]
pub struct AST<'a> {
    pub module_header: ModuleHeader<'a>,
    pub defs_view: Vec<&'a Def>
}

impl<'a> AST<'a> {
    pub fn new(loaded_module: &'a LoadedModule, module_header: ModuleHeader<'a>) -> Self {
        let declarations = loaded_module.declarations_by_id.get(
            &loaded_module.module_id
        ).unwrap(); // TODO remove unwrap
    
        let defs_view: Vec<&Def> = declarations.iter().map(|decl| {
            match decl {
                Declaration::Declare(def) => def,
                _ => unimplemented!("TODO other declaration variants")
            }
        }).collect();

        Self {
            module_header,
            defs_view
        }
    }
 
    pub fn insert_def_at_index(&mut self, _new_def: Def, _index: usize) {
        unimplemented!("TODO")
    }

    // TODO print in tree shape, similar to linux tree command
    pub fn ast_to_string(&self) -> String {
        let mut full_ast_string = String::new();

        for def in self.defs_view.iter() {
            full_ast_string.push_str(&format!("{:?}", def));
            full_ast_string.push_str("\n\n");
        }

        full_ast_string
    }
}
