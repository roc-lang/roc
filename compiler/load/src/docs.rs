use crate::docs::TypeAnnotation::{Apply, BoundVariable, Record, TagUnion};
use inlinable_string::InlinableString;
use roc_module::ident::ModuleName;
use roc_module::symbol::IdentIds;
use roc_parse::ast;
use roc_parse::ast::AssignedField;
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
    pub type_vars: Vec<String>,
    pub type_annotation: Option<TypeAnnotation>,
    pub docs: Option<String>,
}

#[derive(Debug, Clone)]
pub enum TypeAnnotation {
    TagUnion {
        tags: Vec<Tag>,
        extension: Option<Box<TypeAnnotation>>,
    },
    BoundVariable(String),
    Apply {
        name: String,
        parts: Vec<TypeAnnotation>,
    },
    Record {
        fields: Vec<RecordField>,
    },
}

#[derive(Debug, Clone)]
pub struct RecordField {
    pub name: String,
    pub optional: bool,
    pub type_annotation: TypeAnnotation,
}

#[derive(Debug, Clone)]
pub struct Tag {
    pub name: String,
    pub values: Vec<TypeAnnotation>,
}

pub fn generate_module_docs<'a>(
    module_name: ModuleName,
    exposed_ident_ids: &'a IdentIds,
    parsed_defs: &'a [Located<ast::Def<'a>>],
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
    def: &'a ast::Def<'a>,
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
                        type_annotation: None,
                        type_vars: Vec::new(),
                        docs: before_comments_or_new_lines.and_then(comments_or_new_lines_to_docs),
                    };
                    acc.push(entry);
                }
                (acc, None)
            }

            _ => (acc, None),
        },
        AnnotatedBody { ann_pattern, .. } => match ann_pattern.value {
            Pattern::Identifier(identifier) => {
                // Check if the definition is exposed
                if exposed_ident_ids
                    .get_id(&InlinableString::from(identifier))
                    .is_some()
                {
                    let entry = DocEntry {
                        name: identifier.to_string(),
                        type_annotation: None,
                        type_vars: Vec::new(),
                        docs: before_comments_or_new_lines.and_then(comments_or_new_lines_to_docs),
                    };
                    acc.push(entry);
                }
                (acc, None)
            }

            _ => (acc, None),
        },

        Alias { name, vars, ann } => {
            let mut type_vars = Vec::new();

            for var in vars.iter() {
                if let Pattern::Identifier(ident_name) = var.value {
                    type_vars.push(ident_name.to_string());
                }
            }

            let entry = DocEntry {
                name: name.value.to_string(),
                type_annotation: type_to_docs(ann.value),
                type_vars,
                docs: before_comments_or_new_lines.and_then(comments_or_new_lines_to_docs),
            };
            acc.push(entry);

            (acc, None)
        }

        Body(_, _) => (acc, None),

        Expect(c) => todo!("documentation for tests {:?}", c),

        NotYetImplemented(s) => todo!("{}", s),
    }
}

fn type_to_docs(type_annotation: ast::TypeAnnotation) -> Option<TypeAnnotation> {
    match type_annotation {
        ast::TypeAnnotation::TagUnion {
            tags,
            ext,
            final_comments: _,
        } => {
            let mut tags_to_render: Vec<Tag> = Vec::new();

            let mut any_tags_are_private = false;

            let mut index = 0;

            while index < tags.len() && !any_tags_are_private {
                let tag = tags[index];

                match tag_to_doc(tag.value) {
                    None => {
                        any_tags_are_private = true;
                    }
                    Some(tag_ann) => {
                        tags_to_render.push(tag_ann);
                    }
                }

                index += 1;
            }

            if any_tags_are_private {
                None
            } else {
                let extension = match ext {
                    None => None,
                    Some(ext_type_ann) => type_to_docs(ext_type_ann.value).map(Box::new),
                };

                Some(TagUnion {
                    tags: tags_to_render,
                    extension,
                })
            }
        }
        ast::TypeAnnotation::BoundVariable(var_name) => Some(BoundVariable(var_name.to_string())),
        ast::TypeAnnotation::Apply(module_name, type_name, type_ann_parts) => {
            let mut name = String::new();

            if !module_name.is_empty() {
                name.push_str(module_name);
                name.push('.');
            }

            name.push_str(type_name);

            let mut parts: Vec<TypeAnnotation> = Vec::new();

            for type_ann_part in type_ann_parts {
                if let Some(part) = type_to_docs(type_ann_part.value) {
                    parts.push(part);
                }
            }

            Some(Apply { name, parts })
        }
        ast::TypeAnnotation::Record {
            fields,
            ext: _,
            final_comments: _,
        } => {
            let mut doc_fields = Vec::new();

            let mut any_fields_include_private_tags = false;

            let mut index = 0;

            while index < fields.len() && !any_fields_include_private_tags {
                let field = fields[index];

                match record_field_to_doc(field.value) {
                    None => {
                        any_fields_include_private_tags = true;
                    }
                    Some(doc_field) => {
                        doc_fields.push(doc_field);

                        index = index + 1;
                    }
                }
            }
            Some(Record { fields: doc_fields })
        }
        ast::TypeAnnotation::SpaceBefore(&sub_type_ann, _) => type_to_docs(sub_type_ann),
        ast::TypeAnnotation::SpaceAfter(&sub_type_ann, _) => type_to_docs(sub_type_ann),
        _ => {
            // TODO "Implement type to docs")

            None
        }
    }
}

fn record_field_to_doc<'a>(
    field: ast::AssignedField<'a, ast::TypeAnnotation>,
) -> Option<RecordField> {
    match field {
        AssignedField::RequiredValue(name, _, type_ann) => {
            type_to_docs(type_ann.value).map(|type_ann_docs| RecordField {
                name: name.value.to_string(),
                type_annotation: type_ann_docs,
                optional: false,
            })
        }
        AssignedField::SpaceBefore(&sub_field, _) => record_field_to_doc(sub_field),
        AssignedField::SpaceAfter(&sub_field, _) => record_field_to_doc(sub_field),
        AssignedField::OptionalValue(name, _, type_ann) => {
            type_to_docs(type_ann.value).map(|type_ann_docs| RecordField {
                name: name.value.to_string(),
                type_annotation: type_ann_docs,
                optional: true,
            })
        }
        AssignedField::LabelOnly(label) => Some(RecordField {
            name: label.value.to_string(),
            type_annotation: BoundVariable(label.value.to_string()),
            optional: false,
        }),
        AssignedField::Malformed(_) => None,
    }
}

// The Option here represents if it is private. Private tags
// evaluate to `None`.
fn tag_to_doc(tag: ast::Tag) -> Option<Tag> {
    match tag {
        ast::Tag::Global { name, args } => Some(Tag {
            name: name.value.to_string(),
            values: {
                let mut type_vars = Vec::new();

                let mut index = 0;

                while index < args.len() {
                    let arg = args[index];

                    if let Some(type_var) = type_to_docs(arg.value) {
                        type_vars.push(type_var);
                    }

                    index += 1;
                }

                type_vars
            },
        }),
        ast::Tag::Private { .. } => None,
        ast::Tag::SpaceBefore(&sub_tag, _) => tag_to_doc(sub_tag),
        ast::Tag::SpaceAfter(&sub_tag, _) => tag_to_doc(sub_tag),
        ast::Tag::Malformed(_) => None,
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
                docs.push('\n');
            }
            Newline | LineComment(_) => {
                docs = String::new();
            }
        }
    }
    if docs.is_empty() {
        None
    } else {
        Some(docs)
    }
}
