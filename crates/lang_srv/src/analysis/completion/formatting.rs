use roc_module::symbol::{Interns, ModuleId};

use tower_lsp::lsp_types::{Documentation, MarkupContent, MarkupKind};

use crate::analysis::{utils::format_var_type, ModulesInfo};

fn module_exposed_list(
    module_id: &ModuleId,
    interns: &Interns,
    ModulesInfo { subs, exposed }: &ModulesInfo,
) -> Option<std::string::String> {
    exposed.get(module_id).and_then(|exposed| {
        subs.lock().get_mut(module_id).map(|subs| {
            let items = exposed
                .iter()
                .map(|(symb, var)| {
                    let var_str = format_var_type(*var, subs, module_id, interns);
                    format!("{0}: {1}", symb.as_str(interns), var_str)
                })
                .collect::<Vec<_>>();

            items.join("\n").to_string()
        })
    })
}
pub(super) enum DescriptionsType {
    Exposes,
}
fn md_doc(val: String) -> Documentation {
    Documentation::MarkupContent(MarkupContent {
        kind: MarkupKind::Markdown,
        value: val,
    })
}
///Generates a nicely formatted block of text for the completionitem documentation field

pub(super) fn module_documentation(
    description_type: DescriptionsType,
    module_id: &ModuleId,
    interns: &Interns,
    modules_info: &ModulesInfo,
) -> Documentation {
    let exposed = || module_exposed_list(module_id, interns, modules_info).unwrap_or_default();

    match description_type {
        DescriptionsType::Exposes => md_doc(format!("```roc\n{0}\n```", exposed())),
    }
}
