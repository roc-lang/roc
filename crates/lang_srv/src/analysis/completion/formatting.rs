use std::{collections::HashMap, sync::Arc};

use parking_lot::Mutex;
use roc_module::symbol::{Interns, ModuleId, Symbol};
use roc_types::subs::{Subs, Variable};
use tower_lsp::lsp_types::{Documentation, MarkupContent, MarkupKind};

use crate::analysis::utils::format_var_type;

fn module_exposed_list(
    module_id: &ModuleId,
    interns: &Interns,
    imported_modules: &HashMap<ModuleId, Arc<Vec<(Symbol, Variable)>>>,
    all_subs: &Mutex<HashMap<ModuleId, Subs>>,
    modules_exposed: &Mutex<HashMap<ModuleId, Arc<Vec<(Symbol, Variable)>>>>,
) -> Option<std::string::String> {
    modules_exposed.lock().get(module_id).and_then(|exposed| {
        all_subs.lock().get_mut(module_id).map(|subs| {
            let items = exposed
                .iter()
                .map(|(symb, var)| {
                    let var_str = format_var_type(*var, subs, module_id, interns);
                    format!("    {0}: {1}", symb.as_str(interns), var_str)
                })
                .collect::<Vec<_>>();

            format!("{{\n{0}\n}}", items.join(",\n"))
        })
    })
}
pub enum DescripitonType {
    Name,
    Exposes,
    NameAndExposes,
}
fn md_doc(val: String) -> Documentation {
    Documentation::MarkupContent(MarkupContent {
        kind: MarkupKind::Markdown,
        value: val,
    })
}

pub fn module_documentation(
    description_type: DescripitonType,
    module_id: &ModuleId,
    mod_name: &String,
    interns: &Interns,
    imported_modules: &HashMap<ModuleId, Arc<Vec<(Symbol, Variable)>>>,
    all_subs: &Mutex<HashMap<ModuleId, Subs>>,
    modules_exposed: &Mutex<HashMap<ModuleId, Arc<Vec<(Symbol, Variable)>>>>,
) -> Documentation {
    let exposed = || {
        module_exposed_list(
            module_id,
            interns,
            imported_modules,
            all_subs,
            modules_exposed,
        )
        .unwrap_or_default()
    };

    match description_type {
        DescripitonType::Name => md_doc(format!("{0} module", mod_name)),
        DescripitonType::Exposes => md_doc(format!("```roc\n{0}\n```", exposed())),
        DescripitonType::NameAndExposes => {
            md_doc(format!("{0}\n```roc\n{1}\n```", mod_name, exposed()))
        }
    }
}
