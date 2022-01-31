use crate::docs::DocEntry::DetachedDoc;
use crate::docs::TypeAnnotation::{
    Apply, BoundVariable, Function, NoTypeAnn, ObscuredRecord, ObscuredTagUnion, Record, TagUnion,
};
use crate::file::LoadedModule;
use roc_can::scope::Scope;
use roc_module::ident::ModuleName;
use roc_module::symbol::IdentIds;
use roc_parse::ast::CommentOrNewline;
use roc_parse::ast::{self, AliasHeader};
use roc_parse::ast::{AssignedField, Def};
use roc_region::all::Loc;

// Documentation generation requirements

#[derive(Debug)]
pub struct Documentation {
    pub name: String,
    pub version: String,
    pub docs: String,
    pub modules: Vec<LoadedModule>,
}

#[derive(Debug)]
pub struct ModuleDocumentation {
    pub name: String,
    pub entries: Vec<DocEntry>,
    pub scope: Scope,
}

#[derive(Debug, Clone)]
pub enum DocEntry {
    DocDef(DocDef),
    DetachedDoc(String),
}

#[derive(Debug, Clone)]
pub struct DocDef {
    pub name: String,
    pub type_vars: Vec<String>,
    pub type_annotation: TypeAnnotation,
    pub docs: Option<String>,
}

#[derive(Debug, Clone)]
pub enum TypeAnnotation {
    TagUnion {
        tags: Vec<Tag>,
        extension: Box<TypeAnnotation>,
    },
    Function {
        args: Vec<TypeAnnotation>,
        output: Box<TypeAnnotation>,
    },
    ObscuredTagUnion,
    ObscuredRecord,
    BoundVariable(String),
    Apply {
        name: String,
        parts: Vec<TypeAnnotation>,
    },
    Record {
        fields: Vec<RecordField>,
        extension: Box<TypeAnnotation>,
    },
    Wildcard,
    NoTypeAnn,
}
#[derive(Debug, Clone)]
pub enum RecordField {
    RecordField {
        name: String,
        type_annotation: TypeAnnotation,
    },
    OptionalField {
        name: String,
        type_annotation: TypeAnnotation,
    },
    LabelOnly {
        name: String,
    },
}

#[derive(Debug, Clone)]
pub struct Tag {
    pub name: String,
    pub values: Vec<TypeAnnotation>,
}

pub fn generate_module_docs<'a>(
    scope: Scope,
    module_name: ModuleName,
    ident_ids: &'a IdentIds,
    parsed_defs: &'a [Loc<ast::Def<'a>>],
) -> ModuleDocumentation {
    let (entries, _) =
        parsed_defs
            .iter()
            .fold((vec![], None), |(acc, maybe_comments_after), def| {
                generate_entry_doc(ident_ids, acc, maybe_comments_after, &def.value)
            });

    ModuleDocumentation {
        name: module_name.as_str().to_string(),
        scope,
        entries,
    }
}

fn detached_docs_from_comments_and_new_lines<'a>(
    comments_or_new_lines: &'a [roc_parse::ast::CommentOrNewline<'a>],
) -> Vec<String> {
    let mut detached_docs: Vec<String> = Vec::new();

    let mut docs = String::new();

    for comment_or_new_line in comments_or_new_lines.iter() {
        match comment_or_new_line {
            CommentOrNewline::DocComment(doc_str) => {
                docs.push_str(doc_str);
                docs.push('\n');
            }

            CommentOrNewline::LineComment(_) | CommentOrNewline::Newline => {
                if !docs.is_empty() {
                    detached_docs.push(docs.clone());
                }

                docs = String::new();
            }
        }
    }

    detached_docs
}

fn generate_entry_doc<'a>(
    ident_ids: &'a IdentIds,
    mut acc: Vec<DocEntry>,
    before_comments_or_new_lines: Option<&'a [roc_parse::ast::CommentOrNewline<'a>]>,
    def: &'a ast::Def<'a>,
) -> (
    Vec<DocEntry>,
    Option<&'a [roc_parse::ast::CommentOrNewline<'a>]>,
) {
    use roc_parse::ast::Pattern;

    match def {
        Def::SpaceBefore(sub_def, comments_or_new_lines) => {
            // Comments before a definition are attached to the current definition

            for detached_doc in detached_docs_from_comments_and_new_lines(comments_or_new_lines) {
                acc.push(DetachedDoc(detached_doc));
            }

            generate_entry_doc(ident_ids, acc, Some(comments_or_new_lines), sub_def)
        }

        Def::SpaceAfter(sub_def, comments_or_new_lines) => {
            let (new_acc, _) =
                // If there are comments before, attach to this definition
                generate_entry_doc(ident_ids, acc, before_comments_or_new_lines, sub_def);

            // Comments after a definition are attached to the next definition
            (new_acc, Some(comments_or_new_lines))
        }

        Def::Annotation(loc_pattern, loc_ann) => match loc_pattern.value {
            Pattern::Identifier(identifier) => {
                // Check if the definition is exposed
                if ident_ids.get_id(&identifier.into()).is_some() {
                    let name = identifier.to_string();
                    let doc_def = DocDef {
                        name,
                        type_annotation: type_to_docs(false, loc_ann.value),
                        type_vars: Vec::new(),
                        docs: before_comments_or_new_lines.and_then(comments_or_new_lines_to_docs),
                    };
                    acc.push(DocEntry::DocDef(doc_def));
                }
                (acc, None)
            }

            _ => (acc, None),
        },
        Def::AnnotatedBody {
            ann_pattern,
            ann_type,
            ..
        } => match ann_pattern.value {
            Pattern::Identifier(identifier) => {
                // Check if the definition is exposed
                if ident_ids.get_id(&identifier.into()).is_some() {
                    let doc_def = DocDef {
                        name: identifier.to_string(),
                        type_annotation: type_to_docs(false, ann_type.value),
                        type_vars: Vec::new(),
                        docs: before_comments_or_new_lines.and_then(comments_or_new_lines_to_docs),
                    };
                    acc.push(DocEntry::DocDef(doc_def));
                }
                (acc, None)
            }

            _ => (acc, None),
        },

        Def::Alias {
            header: AliasHeader { name, vars },
            ann,
        } => {
            let mut type_vars = Vec::new();

            for var in vars.iter() {
                if let Pattern::Identifier(ident_name) = var.value {
                    type_vars.push(ident_name.to_string());
                }
            }

            let doc_def = DocDef {
                name: name.value.to_string(),
                type_annotation: type_to_docs(false, ann.value),
                type_vars,
                docs: before_comments_or_new_lines.and_then(comments_or_new_lines_to_docs),
            };
            acc.push(DocEntry::DocDef(doc_def));

            (acc, None)
        }

        Def::Body(_, _) => (acc, None),

        Def::Expect(c) => todo!("documentation for tests {:?}", c),

        Def::NotYetImplemented(s) => todo!("{}", s),
    }
}

fn type_to_docs(in_func_type_ann: bool, type_annotation: ast::TypeAnnotation) -> TypeAnnotation {
    match type_annotation {
        ast::TypeAnnotation::TagUnion { tags, ext } => {
            let mut tags_to_render: Vec<Tag> = Vec::new();

            let mut any_tags_are_private = false;

            for tag in tags.iter() {
                match tag_to_doc(in_func_type_ann, tag.value) {
                    None => {
                        any_tags_are_private = true;
                        break;
                    }
                    Some(tag_ann) => {
                        tags_to_render.push(tag_ann);
                    }
                }
            }

            if any_tags_are_private {
                if in_func_type_ann {
                    ObscuredTagUnion
                } else {
                    NoTypeAnn
                }
            } else {
                let extension = match ext {
                    None => NoTypeAnn,
                    Some(ext_type_ann) => type_to_docs(in_func_type_ann, ext_type_ann.value),
                };

                TagUnion {
                    tags: tags_to_render,
                    extension: Box::new(extension),
                }
            }
        }
        ast::TypeAnnotation::BoundVariable(var_name) => BoundVariable(var_name.to_string()),
        ast::TypeAnnotation::Apply(module_name, type_name, type_ann_parts) => {
            let mut name = String::new();

            if !module_name.is_empty() {
                name.push_str(module_name);
                name.push('.');
            }

            name.push_str(type_name);

            let mut parts: Vec<TypeAnnotation> = Vec::new();

            for type_ann_part in type_ann_parts {
                parts.push(type_to_docs(in_func_type_ann, type_ann_part.value));
            }

            Apply { name, parts }
        }
        ast::TypeAnnotation::Record { fields, ext } => {
            let mut doc_fields = Vec::new();

            let mut any_fields_include_private_tags = false;

            for field in fields.items {
                match record_field_to_doc(in_func_type_ann, field.value) {
                    None => {
                        any_fields_include_private_tags = true;
                        break;
                    }
                    Some(doc_field) => {
                        doc_fields.push(doc_field);
                    }
                }
            }
            if any_fields_include_private_tags {
                if in_func_type_ann {
                    ObscuredRecord
                } else {
                    NoTypeAnn
                }
            } else {
                let extension = match ext {
                    None => NoTypeAnn,
                    Some(ext_type_ann) => type_to_docs(in_func_type_ann, ext_type_ann.value),
                };

                Record {
                    fields: doc_fields,
                    extension: Box::new(extension),
                }
            }
        }
        ast::TypeAnnotation::SpaceBefore(&sub_type_ann, _) => {
            type_to_docs(in_func_type_ann, sub_type_ann)
        }
        ast::TypeAnnotation::SpaceAfter(&sub_type_ann, _) => {
            type_to_docs(in_func_type_ann, sub_type_ann)
        }
        ast::TypeAnnotation::Function(ast_arg_anns, output_ann) => {
            let mut doc_arg_anns = Vec::new();

            for ast_arg_ann in ast_arg_anns {
                doc_arg_anns.push(type_to_docs(true, ast_arg_ann.value));
            }

            Function {
                args: doc_arg_anns,
                output: Box::new(type_to_docs(true, output_ann.value)),
            }
        }
        ast::TypeAnnotation::Wildcard => TypeAnnotation::Wildcard,
        _ => NoTypeAnn,
    }
}

fn record_field_to_doc(
    in_func_ann: bool,
    field: ast::AssignedField<'_, ast::TypeAnnotation>,
) -> Option<RecordField> {
    match field {
        AssignedField::RequiredValue(name, _, type_ann) => Some(RecordField::RecordField {
            name: name.value.to_string(),
            type_annotation: type_to_docs(in_func_ann, type_ann.value),
        }),
        AssignedField::SpaceBefore(&sub_field, _) => record_field_to_doc(in_func_ann, sub_field),
        AssignedField::SpaceAfter(&sub_field, _) => record_field_to_doc(in_func_ann, sub_field),
        AssignedField::OptionalValue(name, _, type_ann) => Some(RecordField::OptionalField {
            name: name.value.to_string(),
            type_annotation: type_to_docs(in_func_ann, type_ann.value),
        }),
        AssignedField::LabelOnly(label) => Some(RecordField::LabelOnly {
            name: label.value.to_string(),
        }),
        AssignedField::Malformed(_) => None,
    }
}

// The Option here represents if it is private. Private tags
// evaluate to `None`.
fn tag_to_doc(in_func_ann: bool, tag: ast::Tag) -> Option<Tag> {
    match tag {
        ast::Tag::Global { name, args } => Some(Tag {
            name: name.value.to_string(),
            values: {
                let mut type_vars = Vec::new();

                for arg in args {
                    type_vars.push(type_to_docs(in_func_ann, arg.value));
                }

                type_vars
            },
        }),
        ast::Tag::Private { .. } => None,
        ast::Tag::SpaceBefore(&sub_tag, _) => tag_to_doc(in_func_ann, sub_tag),
        ast::Tag::SpaceAfter(&sub_tag, _) => tag_to_doc(in_func_ann, sub_tag),
        ast::Tag::Malformed(_) => None,
    }
}

fn comments_or_new_lines_to_docs<'a>(
    comments_or_new_lines: &'a [roc_parse::ast::CommentOrNewline<'a>],
) -> Option<String> {
    let mut docs = String::new();

    for comment_or_new_line in comments_or_new_lines.iter() {
        match comment_or_new_line {
            CommentOrNewline::DocComment(doc_str) => {
                docs.push_str(doc_str);
                docs.push('\n');
            }
            CommentOrNewline::Newline | CommentOrNewline::LineComment(_) => {
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
