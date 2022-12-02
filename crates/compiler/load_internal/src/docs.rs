use crate::docs::DocEntry::DetachedDoc;
use crate::docs::TypeAnnotation::{Apply, BoundVariable, Function, NoTypeAnn, Record, TagUnion};
use roc_can::scope::Scope;
use roc_collections::VecSet;
use roc_module::ident::ModuleName;
use roc_module::symbol::{IdentIds, ModuleId, ModuleIds, Symbol};
use roc_parse::ast::AssignedField;
use roc_parse::ast::{self, ExtractSpaces, TypeHeader};
use roc_parse::ast::{CommentOrNewline, TypeDef, ValueDef};

// Documentation generation requirements

#[derive(Debug)]
pub struct ModuleDocumentation {
    pub name: String,
    pub entries: Vec<DocEntry>,
    pub scope: Scope,
    pub exposed_symbols: VecSet<Symbol>,
}

#[derive(Debug, Clone)]
pub enum DocEntry {
    DocDef(DocDef),
    DetachedDoc(String),
}

#[derive(Debug, Clone)]
pub struct DocDef {
    pub name: String,
    pub symbol: Symbol,
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
    Ability {
        members: Vec<AbilityMember>,
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
pub struct AbilityMember {
    pub name: String,
    pub type_annotation: TypeAnnotation,
    pub able_variables: Vec<(String, Vec<TypeAnnotation>)>,
    pub docs: Option<String>,
}

#[derive(Debug, Clone)]
pub struct Tag {
    pub name: String,
    pub values: Vec<TypeAnnotation>,
}

pub fn generate_module_docs(
    scope: Scope,
    home: ModuleId,
    module_ids: &ModuleIds,
    module_name: ModuleName,
    parsed_defs: &roc_parse::ast::Defs,
    exposed_module_ids: &[ModuleId],
    exposed_symbols: VecSet<Symbol>,
) -> ModuleDocumentation {
    let entries = generate_entry_docs(
        home,
        &scope.locals.ident_ids,
        module_ids,
        parsed_defs,
        exposed_module_ids,
    );

    ModuleDocumentation {
        name: module_name.as_str().to_string(),
        scope,
        entries,
        exposed_symbols,
    }
}

fn detached_docs_from_comments_and_new_lines<'a>(
    comments_or_new_lines: impl Iterator<Item = &'a roc_parse::ast::CommentOrNewline<'a>>,
) -> Vec<String> {
    let mut detached_docs: Vec<String> = Vec::new();

    let mut docs = String::new();

    for comment_or_new_line in comments_or_new_lines {
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

fn generate_entry_docs(
    home: ModuleId,
    ident_ids: &IdentIds,
    module_ids: &ModuleIds,
    defs: &roc_parse::ast::Defs<'_>,
    exposed_module_ids: &[ModuleId],
) -> Vec<DocEntry> {
    use roc_parse::ast::Pattern;

    let mut acc = Vec::with_capacity(defs.tags.len());
    let mut before_comments_or_new_lines: Option<&[CommentOrNewline]> = None;
    let mut scratchpad = Vec::new();

    for (index, either_index) in defs.tags.iter().enumerate() {
        let spaces_before = &defs.spaces[defs.space_before[index].indices()];

        scratchpad.clear();
        scratchpad.extend(
            before_comments_or_new_lines
                .take()
                .iter()
                .flat_map(|e| e.iter()),
        );
        scratchpad.extend(spaces_before);

        let docs = comments_or_new_lines_to_docs(&scratchpad);

        match either_index.split() {
            Err(value_index) => match &defs.value_defs[value_index.index()] {
                ValueDef::Annotation(loc_pattern, loc_ann) => {
                    if let Pattern::Identifier(identifier) = loc_pattern.value {
                        // Check if this module exposes the def
                        if let Some(ident_id) = ident_ids.get_id(identifier) {
                            let name = identifier.to_string();
                            let doc_def = DocDef {
                                name,
                                symbol: Symbol::new(home, ident_id),
                                type_annotation: type_to_docs(false, loc_ann.value),
                                type_vars: Vec::new(),
                                docs,
                            };
                            acc.push(DocEntry::DocDef(doc_def));
                        }
                    }
                }

                ValueDef::AnnotatedBody {
                    ann_pattern,
                    ann_type,
                    ..
                } => {
                    if let Pattern::Identifier(identifier) = ann_pattern.value {
                        // Check if the definition is exposed
                        if let Some(ident_id) = ident_ids.get_id(identifier) {
                            let doc_def = DocDef {
                                name: identifier.to_string(),
                                type_annotation: type_to_docs(false, ann_type.value),
                                type_vars: Vec::new(),
                                symbol: Symbol::new(home, ident_id),
                                docs,
                            };
                            acc.push(DocEntry::DocDef(doc_def));
                        }
                    }
                }

                ValueDef::Body(_, _) => (),

                ValueDef::Dbg { .. } => {

                    // Don't generate docs for `dbg`s
                }

                ValueDef::Expect { .. } => {
                    // Don't generate docs for `expect`s
                }

                ValueDef::ExpectFx { .. } => {
                    // Don't generate docs for `expect-fx`s
                }
            },
            Ok(type_index) => match &defs.type_defs[type_index.index()] {
                TypeDef::Alias {
                    header: TypeHeader { name, vars },
                    ann,
                } => {
                    let mut type_vars = Vec::new();

                    for var in vars.iter() {
                        if let Pattern::Identifier(ident_name) = var.value {
                            type_vars.push(ident_name.to_string());
                        }
                    }

                    let type_annotation =
                        if contains_unexposed_type(&ann.value, exposed_module_ids, module_ids) {
                            TypeAnnotation::NoTypeAnn
                        } else {
                            type_to_docs(false, ann.value)
                        };

                    let ident_id = ident_ids.get_id(name.value).unwrap();
                    let doc_def = DocDef {
                        name: name.value.to_string(),
                        type_annotation,
                        type_vars,
                        docs,
                        symbol: Symbol::new(home, ident_id),
                    };
                    acc.push(DocEntry::DocDef(doc_def));
                }

                TypeDef::Opaque {
                    header: TypeHeader { name, vars },
                    ..
                } => {
                    let mut type_vars = Vec::new();

                    for var in vars.iter() {
                        if let Pattern::Identifier(ident_name) = var.value {
                            type_vars.push(ident_name.to_string());
                        }
                    }

                    let ident_id = ident_ids.get_id(name.value).unwrap();
                    let doc_def = DocDef {
                        name: name.value.to_string(),
                        type_annotation: TypeAnnotation::NoTypeAnn,
                        type_vars,
                        docs,
                        symbol: Symbol::new(home, ident_id),
                    };
                    acc.push(DocEntry::DocDef(doc_def));
                }

                TypeDef::Ability {
                    header: TypeHeader { name, vars },
                    members,
                    ..
                } => {
                    let mut type_vars = Vec::new();

                    for var in vars.iter() {
                        if let Pattern::Identifier(ident_name) = var.value {
                            type_vars.push(ident_name.to_string());
                        }
                    }

                    let members = members
                        .iter()
                        .map(|mem| {
                            let extracted = mem.name.value.extract_spaces();
                            let (type_annotation, able_variables) =
                                ability_member_type_to_docs(mem.typ.value);

                            AbilityMember {
                                name: extracted.item.to_string(),
                                type_annotation,
                                able_variables,
                                docs: comments_or_new_lines_to_docs(extracted.before),
                            }
                        })
                        .collect();

                    let ident_id = ident_ids.get_id(name.value).unwrap();
                    let doc_def = DocDef {
                        name: name.value.to_string(),
                        type_annotation: TypeAnnotation::Ability { members },
                        symbol: Symbol::new(home, ident_id),
                        type_vars,
                        docs,
                    };
                    acc.push(DocEntry::DocDef(doc_def));
                }
            },
        }

        let spaces_after = &defs.spaces[defs.space_after[index].indices()];
        before_comments_or_new_lines = Some(spaces_after);
    }

    let it = before_comments_or_new_lines.iter().flat_map(|e| e.iter());

    for detached_doc in detached_docs_from_comments_and_new_lines(it) {
        acc.push(DetachedDoc(detached_doc));
    }

    acc
}

fn contains_unexposed_type(
    ann: &ast::TypeAnnotation,
    exposed_module_ids: &[ModuleId],
    module_ids: &ModuleIds,
) -> bool {
    use ast::TypeAnnotation::*;

    match ann {
        // Apply is the one case that can directly return true.
        Apply(module_name, _ident, loc_args) => {
            // If the *ident* was unexposed, we would have gotten a naming error
            // during canonicalization, so all we need to check is the module.
            let module_id = module_ids.get_id(&(*module_name).into()).unwrap();

            !exposed_module_ids.contains(&module_id)
                || loc_args.iter().any(|loc_arg| {
                    contains_unexposed_type(&loc_arg.value, exposed_module_ids, module_ids)
                })
        }
        Malformed(_) | Inferred | Wildcard | BoundVariable(_) => false,
        Function(loc_args, loc_ret) => {
            contains_unexposed_type(&loc_ret.value, exposed_module_ids, module_ids)
                || loc_args.iter().any(|loc_arg| {
                    contains_unexposed_type(&loc_arg.value, exposed_module_ids, module_ids)
                })
        }
        Record { fields, ext } => {
            if let Some(loc_ext) = ext {
                if contains_unexposed_type(&loc_ext.value, exposed_module_ids, module_ids) {
                    return true;
                }
            }

            let mut fields_to_process =
                Vec::from_iter(fields.iter().map(|loc_field| loc_field.value));

            while let Some(field) = fields_to_process.pop() {
                match field {
                    AssignedField::RequiredValue(_field, _spaces, loc_val)
                    | AssignedField::OptionalValue(_field, _spaces, loc_val) => {
                        if contains_unexposed_type(&loc_val.value, exposed_module_ids, module_ids) {
                            return true;
                        }
                    }
                    AssignedField::Malformed(_) | AssignedField::LabelOnly(_) => {
                        // contains no unexposed types, so continue
                    }
                    AssignedField::SpaceBefore(field, _) | AssignedField::SpaceAfter(field, _) => {
                        fields_to_process.push(*field);
                    }
                }
            }

            false
        }
        Tuple { fields, ext } => {
            if let Some(loc_ext) = ext {
                if contains_unexposed_type(&loc_ext.value, exposed_module_ids, module_ids) {
                    return true;
                }
            }

            fields.iter().any(|loc_field| {
                contains_unexposed_type(&loc_field.value, exposed_module_ids, module_ids)
            })
        }
        TagUnion { ext, tags } => {
            use ast::Tag;

            if let Some(loc_ext) = ext {
                if contains_unexposed_type(&loc_ext.value, exposed_module_ids, module_ids) {
                    return true;
                }
            }

            let mut tags_to_process = Vec::from_iter(tags.iter().map(|loc_tag| loc_tag.value));

            while let Some(tag) = tags_to_process.pop() {
                match tag {
                    Tag::Apply { name: _, args } => {
                        for loc_ann in args.iter() {
                            if contains_unexposed_type(
                                &loc_ann.value,
                                exposed_module_ids,
                                module_ids,
                            ) {
                                return true;
                            }
                        }
                    }
                    Tag::Malformed(_) => {
                        // contains no unexposed types, so continue
                    }
                    Tag::SpaceBefore(tag, _) | Tag::SpaceAfter(tag, _) => {
                        tags_to_process.push(*tag);
                    }
                }
            }

            false
        }
        Where(loc_ann, _loc_has_clauses) => {
            // We assume all the abilities in the `has` clause are from exported modules.
            // TODO don't assume this! Instead, look them up and verify.
            contains_unexposed_type(&loc_ann.value, exposed_module_ids, module_ids)
        }
        As(loc_ann, _spaces, _type_header) => {
            contains_unexposed_type(&loc_ann.value, exposed_module_ids, module_ids)
        }
        SpaceBefore(ann, _) | ast::TypeAnnotation::SpaceAfter(ann, _) => {
            contains_unexposed_type(ann, exposed_module_ids, module_ids)
        }
    }
}

fn type_to_docs(in_func_type_ann: bool, type_annotation: ast::TypeAnnotation) -> TypeAnnotation {
    match type_annotation {
        ast::TypeAnnotation::TagUnion { tags, ext } => {
            let mut tags_to_render: Vec<Tag> = Vec::new();

            for tag in tags.iter() {
                if let Some(tag_ann) = tag_to_doc(in_func_type_ann, tag.value) {
                    tags_to_render.push(tag_ann);
                }
            }

            let extension = match ext {
                None => NoTypeAnn,
                Some(ext_type_ann) => type_to_docs(in_func_type_ann, ext_type_ann.value),
            };

            TagUnion {
                tags: tags_to_render,
                extension: Box::new(extension),
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

            for field in fields.items {
                if let Some(doc_field) = record_field_to_doc(in_func_type_ann, field.value) {
                    doc_fields.push(doc_field);
                }
            }
            let extension = match ext {
                None => NoTypeAnn,
                Some(ext_type_ann) => type_to_docs(in_func_type_ann, ext_type_ann.value),
            };

            Record {
                fields: doc_fields,
                extension: Box::new(extension),
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

fn ability_member_type_to_docs(
    type_annotation: ast::TypeAnnotation,
) -> (TypeAnnotation, Vec<(String, Vec<TypeAnnotation>)>) {
    match type_annotation {
        ast::TypeAnnotation::Where(ta, has_clauses) => {
            let ta = type_to_docs(false, ta.value);
            let has_clauses = has_clauses
                .iter()
                .map(|hc| {
                    let ast::HasClause { var, abilities } = hc.value;
                    (
                        var.value.extract_spaces().item.to_string(),
                        abilities
                            .iter()
                            .map(|ability| type_to_docs(false, ability.value))
                            .collect(),
                    )
                })
                .collect();

            (ta, has_clauses)
        }
        _ => (type_to_docs(false, type_annotation), vec![]),
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

// The Option here represents if it is malformed.
fn tag_to_doc(in_func_ann: bool, tag: ast::Tag) -> Option<Tag> {
    match tag {
        ast::Tag::Apply { name, args } => Some(Tag {
            name: name.value.to_string(),
            values: {
                let mut type_vars = Vec::new();

                for arg in args {
                    type_vars.push(type_to_docs(in_func_ann, arg.value));
                }

                type_vars
            },
        }),
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
