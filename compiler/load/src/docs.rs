use inlinable_string::InlinableString;
use roc_module::ident::ModuleName;
use roc_module::symbol::IdentIds;
use roc_parse::ast::Def;
use roc_region::all::Located;

// Documentation generation requirements

#[derive(Debug, Clone)]
pub struct Documentation {
    pub name: String,
    pub version: String,
    pub docs: String,
    pub modules: Vec<ModuleDocumentation>,
}

#[derive(Debug, Clone)]
pub struct ModuleDocumentation {
    pub name: String,
    pub docs: String,
    pub entries: Vec<DocEntry>,
}

#[derive(Debug, Clone)]
pub struct DocEntry {
    pub name: String,
    pub docs: Option<String>,
}

pub fn generate_module_docs<'a>(
    module_name: ModuleName,
    exposed_ident_ids: &'a IdentIds,
    parsed_defs: &'a [Located<Def<'a>>],
) -> ModuleDocumentation {
    let (entries, _) =
        parsed_defs
            .iter()
            .fold((vec![], None), |(acc, maybe_comments_after), def| {
                generate_module_doc(exposed_ident_ids, acc, maybe_comments_after, &def.value)
            });

    ModuleDocumentation {
        name: module_name.as_str().to_string(),
        docs: "".to_string(),
        entries,
    }
}

fn generate_module_doc<'a>(
    exposed_ident_ids: &'a IdentIds,
    mut acc: Vec<DocEntry>,
    before_comments_or_new_lines: Option<&'a [roc_parse::ast::CommentOrNewline<'a>]>,
    def: &'a Def<'a>,
) -> (
    Vec<DocEntry>,
    Option<&'a [roc_parse::ast::CommentOrNewline<'a>]>,
) {
    use roc_parse::ast::Def::*;
    use roc_parse::ast::Pattern;

    match def {
        SpaceBefore(sub_def, comments_or_new_lines) => {
            // Comments before a definition are attached to the current defition
            generate_module_doc(exposed_ident_ids, acc, Some(comments_or_new_lines), sub_def)
        }

        SpaceAfter(sub_def, comments_or_new_lines) => {
            let (new_acc, _) =
                // If there are comments before, attach to this definition
                generate_module_doc(exposed_ident_ids, acc, before_comments_or_new_lines, sub_def);

            // Comments after a definition are attached to the next definition
            (new_acc, Some(comments_or_new_lines))
        }

        Annotation(loc_pattern, _loc_ann) => match loc_pattern.value {
            Pattern::Identifier(identifier) => {
                // Check if the definition is exposed
                if exposed_ident_ids
                    .get_id(&InlinableString::from(identifier))
                    .is_some()
                {
                    let entry = DocEntry {
                        name: identifier.to_string(),
                        docs: before_comments_or_new_lines.and_then(comments_or_new_lines_to_docs),
                    };
                    acc.push(entry);
                }
                (acc, None)
            }

            _ => (acc, None),
        },

        Alias {
            name: _,
            vars: _,
            ann: _,
        } =>
        // TODO
        {
            (acc, None)
        }

        Body(_, _) | Nested(_) => (acc, None),

        NotYetImplemented(s) => todo!("{}", s),
    }
}

fn comments_or_new_lines_to_docs<'a>(
    comments_or_new_lines: &'a [roc_parse::ast::CommentOrNewline<'a>],
) -> Option<String> {
    use roc_parse::ast::CommentOrNewline::*;

    let mut docs = String::new();

    for comment_or_new_line in comments_or_new_lines.iter() {
        match comment_or_new_line {
            DocComment(doc_str) => {
                docs.push_str(doc_str);
                docs.push_str("\n");
            }
            // TODO: Lines with only `##` are not being parsed as a
            // DocComment, but as a LineComment("#\r"). This pattern should cover this.
            // The problem is that this is only valid if it is at the start
            // of a line. False positive example: `x = 2 ##`.
            LineComment("#\r") => docs.push_str("\n"),
            Newline | LineComment(_) => {}
        }
    }
    if docs.is_empty() {
        None
    } else {
        Some(docs)
    }
}
