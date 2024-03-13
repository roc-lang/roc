use roc_module::symbol::{Interns, ModuleId, Symbol};

use roc_types::subs::Variable;
use tower_lsp::lsp_types::{Documentation, MarkupContent, MarkupKind};

use crate::analysis::{utils::format_var_type, ModulesInfo};

fn get_module_exposed_list(
    module_id: &ModuleId,
    interns: &Interns,
    modules_info: &ModulesInfo,
    exposed: &[(Symbol, Variable)],
) -> Option<std::string::String> {
    modules_info.with_subs(module_id, |subs| {
        let items = exposed
            .iter()
            .map(|(symbol, var)| {
                let var_str = format_var_type(*var, subs, module_id, interns);
                format!("{0}: {1}", symbol.as_str(interns), var_str)
            })
            .collect::<Vec<_>>();

        items.join("\n").to_string()
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

/// Generates a nicely formatted block of text for the completionitem documentation field.
pub(super) fn module_documentation(
    description_type: DescriptionsType,
    module_id: &ModuleId,
    interns: &Interns,
    exposed: &[(Symbol, Variable)],
    modules_info: &ModulesInfo,
) -> Documentation {
    let exposed_string =
        get_module_exposed_list(module_id, interns, modules_info, exposed).unwrap_or_default();

    match description_type {
        DescriptionsType::Exposes => md_doc(format!("```roc\n{0}\n```", exposed_string)),
    }
}
