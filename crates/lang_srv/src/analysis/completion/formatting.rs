use roc_load::docs::ModuleDocumentation;
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
    module_docs: Option<&ModuleDocumentation>,
    modules_info: &ModulesInfo,
) -> Documentation {
    let exposed_string =
        get_module_exposed_list(module_id, interns, modules_info, exposed).unwrap_or_default();

    let module_doc = module_docs
        .and_then(|docs| {
            docs.entries.first().and_then(|first_doc| match first_doc {
                roc_load::docs::DocEntry::ModuleDoc(str) => Some(str.clone().trim().to_string()),
                _ => None,
            })
        })
        .unwrap_or_default();

    match description_type {
        DescriptionsType::Exposes => md_doc(format!(
            "{0}```roc\n{1}\n```",
            module_doc + "\n",
            exposed_string
        )),
    }
}
