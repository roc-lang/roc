use std::{collections::HashMap, sync::Arc};

use log::{debug, warn};

use roc_can::{expr::Declarations, traverse::Visitor};
use roc_collections::MutMap;
use roc_load::docs::{DocDef, ModuleDocumentation};
use roc_module::symbol::{Interns, ModuleId, Symbol};
use roc_region::all::Position;
use roc_types::{
    subs::{Subs, Variable},
    types::Alias,
};
use tower_lsp::lsp_types::{self, CompletionItem, CompletionItemKind};

use self::visitor::CompletionVisitor;

use super::{utils::format_var_type, ModulesInfo};
mod formatting;
mod visitor;

fn get_completions(
    position: Position,
    decls: &Declarations,
    prefix: String,
    interns: &Interns,
) -> Vec<(Symbol, Variable)> {
    let mut visitor = CompletionVisitor {
        position,
        found_declarations: Vec::new(),
        interns,
        prefix,
    };
    visitor.visit_decls(decls);
    visitor.found_declarations
}

#[allow(clippy::too_many_arguments)]
/// Walks through declarations that would be accessible from the provided
/// position adding them to a list of completion items until all accessible
/// declarations have been fully explored.
pub fn get_completion_items(
    position: Position,
    prefix: String,
    decls: &Declarations,
    subs: &mut Subs,
    module_id: &ModuleId,
    interns: &Interns,
    docs: Option<&ModuleDocumentation>,
    exposed_imports: &[(Symbol, Variable)],
) -> Vec<CompletionItem> {
    let mut completions = get_completions(position, decls, prefix, interns);
    completions.extend(exposed_imports);
    debug!("extended with:{:#?}", exposed_imports);
    make_completion_items(subs, module_id, interns, docs, completions)
}

/// Super basic tag completion. Doesn't include any type documentation for the tags being completed
pub(super) fn get_tag_completion_items(
    prefix: &String,
    module_id: &ModuleId,
    modules_info: &ModulesInfo,
) -> Vec<CompletionItem> {
    modules_info
        .subs_by_module
        .get(module_id)
        .iter()
        .flat_map(|a| {
            let mut lock = a.lock();
            lock.tag_names.dedup();
            lock.tag_names
                .iter()
                .filter_map(|a| {
                    if a.as_ident_str().starts_with(prefix) {
                        Some(CompletionItem {
                            label: a.as_ident_str().to_string(),
                            kind: Some(CompletionItemKind::ENUM),
                            documentation: Some(lsp_types::Documentation::String(
                                a.as_ident_str().to_string(),
                            )),
                            ..Default::default()
                        })
                    } else {
                        None
                    }
                })
                .collect::<Vec<_>>() //we have to collect so that we can release the lock
        })
        .collect()
}

pub(super) fn get_module_completion_items(
    prefix: &String,
    interns: &Interns,
    imported_modules: &HashMap<ModuleId, Arc<Vec<(Symbol, Variable)>>>,
    modules_info: &ModulesInfo,
    just_modules: bool,
) -> Vec<CompletionItem> {
    let module_completions = imported_modules
        .iter()
        .flat_map(|(mod_id, exposed_symbols)| {
            let mod_name = mod_id.to_ident_str(interns).to_string();

            // Completion for modules themselves
            if mod_name.starts_with(prefix) {
                let item = CompletionItem {
                    label: mod_name.clone(),
                    kind: Some(CompletionItemKind::MODULE),
                    documentation: Some(formatting::module_documentation(
                        formatting::DescriptionsType::Exposes,
                        mod_id,
                        interns,
                        exposed_symbols,
                        modules_info.get_docs(mod_id),
                        modules_info,
                    )),
                    ..Default::default()
                };

                vec![item]
            // Complete dot completions for module exports
            } else if prefix.starts_with(&(mod_name + ".")) {
                get_module_exposed_completion(
                    exposed_symbols,
                    modules_info,
                    mod_id,
                    modules_info.get_docs(mod_id),
                    interns,
                )
            } else {
                vec![]
            }
        });

    if just_modules {
        return module_completions.collect();
    }

    module_completions.collect()
}

fn get_module_exposed_completion(
    exposed_symbols: &[(Symbol, Variable)],
    modules_info: &ModulesInfo,
    mod_id: &ModuleId,
    docs: Option<&ModuleDocumentation>,
    interns: &Interns,
) -> Vec<CompletionItem> {
    let mut completion_docs = docs.map_or(Default::default(), |docs| {
        get_completion_docs(exposed_symbols, docs)
    });

    exposed_symbols
        .iter()
        .map(|(symbol, var)| {
            // We need to fetch the subs for the module that is exposing what we
            // are trying to complete because that will have the type info we need.
            modules_info
                .with_subs(mod_id, |subs| {
                    make_completion_item(
                        subs,
                        mod_id,
                        interns,
                        completion_docs.remove(symbol),
                        symbol.as_str(interns).to_string(),
                        *var,
                    )
                })
                .expect("Couldn't find subs for module during completion.")
        })
        .collect::<Vec<_>>()
}

/// Efficiently walks the list of docs collecting the docs for completions as we go.
/// Should be faster than re-walking for every completion.
fn get_completion_docs(
    completions: &[(Symbol, Variable)],
    docs: &ModuleDocumentation,
) -> HashMap<Symbol, String> {
    let mut symbols = completions
        .iter()
        .map(|(symbol, _)| symbol)
        .collect::<Vec<_>>();

    docs.entries
        .iter()
        .filter_map(|doc| match doc {
            roc_load::docs::DocEntry::DocDef(DocDef { docs, symbol, .. }) => {
                let docs_str = docs.as_ref().map(|str| str.trim().to_string())?;
                let (index, _symbol) = symbols
                    .iter()
                    .enumerate()
                    .find(|(_index, symb)| symb == &&symbol)?;

                symbols.swap_remove(index);

                Some((*symbol, docs_str))
            }
            _ => None,
        })
        .collect()
}

/// Provides a list of completions for Type aliases within the scope.
///TODO: Use this when we know we are within a type definition
fn _alias_completions(
    aliases: &MutMap<Symbol, (bool, Alias)>,
    module_id: &ModuleId,
    interns: &Interns,
) -> Vec<CompletionItem> {
    aliases
        .iter()
        .filter(|(symbol, (_exposed, _alias))| &symbol.module_id() == module_id)
        .map(|(symbol, (_exposed, _alias))| {
            let name = symbol.as_str(interns).to_string();
            CompletionItem {
                label: name.clone(),
                detail: Some(name + "we don't know how to print types "),
                kind: Some(CompletionItemKind::CLASS),
                ..Default::default()
            }
        })
        .collect()
}

fn make_completion_items(
    subs: &mut Subs,
    module_id: &ModuleId,
    interns: &Interns,
    docs: Option<&ModuleDocumentation>,
    completions: Vec<(Symbol, Variable)>,
) -> Vec<CompletionItem> {
    let mut completion_docs = docs.map_or(Default::default(), |mod_docs| {
        get_completion_docs(&completions, mod_docs)
    });

    completions
        .into_iter()
        .map(|(symbol, var)| {
            make_completion_item(
                subs,
                module_id,
                interns,
                completion_docs.remove(&symbol),
                symbol.as_str(interns).to_string(),
                var,
            )
        })
        .collect()
}

fn make_completion_items_string(
    subs: &mut Subs,
    module_id: &ModuleId,
    interns: &Interns,
    completions: Vec<(String, Variable)>,
) -> Vec<CompletionItem> {
    completions
        .into_iter()
        .map(|(symbol, var)| make_completion_item(subs, module_id, interns, None, symbol, var))
        .collect()
}

fn make_completion_item(
    subs: &mut Subs,
    module_id: &ModuleId,
    interns: &Interns,
    docs_opt: Option<String>,
    symbol_str: String,
    var: Variable,
) -> CompletionItem {
    let type_str = format_var_type(var, subs, module_id, interns);
    let typ = match subs.get(var).content {
        roc_types::subs::Content::Structure(var) => match var {
            roc_types::subs::FlatType::Apply(_, _) => CompletionItemKind::FUNCTION,
            roc_types::subs::FlatType::Func(_, _, _, _) => CompletionItemKind::FUNCTION,
            roc_types::subs::FlatType::EmptyTagUnion
            | roc_types::subs::FlatType::TagUnion(_, _) => CompletionItemKind::ENUM,
            _ => CompletionItemKind::VARIABLE,
        },
        other => {
            debug!(
                "No specific completionKind for variable type: {:?} defaulting to 'Variable'",
                other
            );
            CompletionItemKind::VARIABLE
        }
    };

    CompletionItem {
        label: symbol_str,
        detail: Some(type_str),
        kind: Some(typ),
        documentation: docs_opt.map(|docs| {
            lsp_types::Documentation::MarkupContent(lsp_types::MarkupContent {
                kind: lsp_types::MarkupKind::Markdown,
                value: docs,
            })
        }),
        ..Default::default()
    }
}

/// E.g. a.b.c.d->{variable_name:"a", field:"d", middle_fields:["b","c"]}
struct RecFieldCompletion {
    /// name of variable that is a record
    variable_name: String,
    field: String,
    middle_fields: Vec<String>,
}

/// Finds the types of and names of all the fields of a record.
/// `var` should be a `Variable` that you know is of type record or else it will return an empty list.
fn find_record_fields(var: Variable, subs: &mut Subs) -> Vec<(String, Variable)> {
    let content = subs.get(var);
    match content.content {
        roc_types::subs::Content::Structure(typ) => match typ {
            roc_types::subs::FlatType::Record(fields, ext) => {
                let field_types = fields.unsorted_iterator(subs, ext);

                match field_types {
                    Ok(field) => field
                        .map(|a| (a.0.clone().into(), a.1.into_inner()))
                        .collect::<Vec<_>>(),
                    Err(err) => {
                        warn!("Error getting record field types for completion: {:?}", err);
                        vec![]
                    }
                }
            }
            roc_types::subs::FlatType::Tuple(elems, ext) => {
                let elems = elems.unsorted_iterator(subs, ext);

                match elems {
                    Ok(elem) => elem.map(|(num, var)| (num.to_string(), var)).collect(),
                    Err(err) => {
                        warn!("Error getting tuple elems for completion: {:?}", err);
                        vec![]
                    }
                }
            }

            _ => {
                warn!(
                    "Trying to get field completion for a type that is not a record: {:?}",
                    typ
                );
                vec![]
            }
        },
        roc_types::subs::Content::Error => {
            //This is caused by typechecking our partially typed variable name causing the typechecking to be confused as the type of the parent variable
            //TODO! ideally i could recover using some previous typecheck result that isn't broken
            warn!("Variable type of record was of type 'error', cannot access field",);
            vec![]
        }
        _ => {
            warn!(
                "Variable before field was unsupported type: {:?}",
                subs.dbg(var)
            );
            vec![]
        }
    }
}

/// Splits a completion prefix for a field into its components.
/// E.g. a.b.c.d->{variable_name:"a",middle_fields:["b","c"],field:"d"}
fn get_field_completion_parts(symbol_prefix: &str) -> Option<RecFieldCompletion> {
    let mut parts = symbol_prefix.split('.').collect::<Vec<_>>();
    let field = parts.pop().unwrap_or("").to_string();
    let variable_name = parts.remove(0).to_string();
    // Now that we have the head and tail removed this is all the intermediate fields.
    let middle_fields = parts.into_iter().map(ToString::to_string).collect();

    Some(RecFieldCompletion {
        variable_name,
        field,
        middle_fields,
    })
}
pub fn field_completion(
    position: Position,
    symbol_prefix: String,
    declarations: &Declarations,
    interns: &Interns,
    subs: &mut Subs,
    module_id: &ModuleId,
) -> Option<Vec<CompletionItem>> {
    let RecFieldCompletion {
        variable_name,
        field,
        middle_fields,
    } = get_field_completion_parts(&symbol_prefix)?;

    debug!(
        "Getting record field completions: variable: {:?} field: {:?} middle: {:?} ",
        variable_name, field, middle_fields
    );

    // We get completions here, but all we really want is the info about the variable that
    // is the first part of our record completion.
    // We are completing the full name of the variable so we should only have one match.
    let completion = get_completions(position, declarations, variable_name, interns)
        .into_iter()
        .map(|(symbol, var)| (symbol.as_str(interns).to_string(), var))
        .next()?;

    // If we have a type that has nested records we could have a completion prefix like: "var.field1.field2.fi".
    // If the document isn't fully typechecked we won't know what the type of field2 is for us to offer
    // completions based on it's fields. Instead we get the type of "var" and then the type of "field1" within
    // var's type and then "field2" within field1's type etc etc, until we have the type of the record we are
    // actually looking for field completions for.
    let completion_record = middle_fields.iter().fold(completion, |state, chain_field| {
        let fields_vars = find_record_fields(state.1, subs);
        fields_vars
            .into_iter()
            .find(|type_field| chain_field == &type_field.0)
            .unwrap_or(state)
    });

    let field_completions: Vec<_> = find_record_fields(completion_record.1, subs)
        .into_iter()
        .filter(|(str, _)| str.starts_with(&field.to_string()))
        .collect();

    let field_completions =
        make_completion_items_string(subs, module_id, interns, field_completions);

    Some(field_completions)
}
