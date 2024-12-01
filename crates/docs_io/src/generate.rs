use crate::file::{self, Assets};
use crate::problem::Problem;
use crate::render_markdown::{markdown_to_html, LookupOrTag};
use crate::render_package::AbilityMember;
use bumpalo::collections::Vec;
use bumpalo::{collections::string::String, Bump};
use core::fmt::{Debug, Write};
use roc_can::scope::Scope;
use roc_collections::{MutMap, VecMap};
use roc_load::docs::{DocEntry, RecordField, Tag, TypeAnnotation};
use roc_load::{ExecutionMode, LoadConfig, LoadedModule, LoadingProblem, Threading};
use roc_module::symbol::{IdentId, Interns, ModuleId, Symbol};
use roc_packaging::cache::{self, RocCacheDir};
use roc_parse::ast::FunctionArrow;
use roc_parse::ident::Accessor;
use roc_parse::keyword;
use roc_parse::state::State;
use roc_region::all::Region;
use roc_target::Target;
use roc_types::subs::Variable;
use roc_types::types::{Alias, AliasCommon, Type, TypeExtension};
use std::fs;
use std::path::{Path, PathBuf};

pub fn generate_docs_html<'a>(
    arena: &'a Bump,
    pkg_name: &str,
    root_file: impl AsRef<Path>,
    out_dir: impl AsRef<Path>,
    opt_user_specified_base_url: Option<&'a str>,
) -> Result<(), Problem> {
    let loaded_module = load_module_for_docs(root_file.as_ref().to_path_buf());

    // Copy over the assets
    // For debug builds, read assets from fs to speed up build
    // Otherwise, include as string literal

    #[cfg(not(debug_assertions))]
    let assets = {
        let search_js = include_str!("./static/search.js");
        let styles_css = include_str!("./static/styles.css");
        let favicon_svg = include_str!("../../../www/public/favicon.svg");
        let raw_template_html = include_str!("./static/index.html");

        Assets {
            search_js,
            styles_css,
            favicon_svg,
            raw_template_html,
        }
    };

    #[cfg(debug_assertions)]
    let assets = {
        // Construct the absolute path to the static assets
        let workspace_dir = std::env!("ROC_WORKSPACE_DIR");
        let static_dir = Path::new(workspace_dir).join("crates/docs_io/src/static");

        // Read the assets from the filesystem
        let search_js = fs::read_to_string(static_dir.join("search.js")).unwrap();
        let styles_css = fs::read_to_string(static_dir.join("styles.css")).unwrap();
        let favicon_svg =
            fs::read_to_string(static_dir.join("../../../../www/public/favicon.svg")).unwrap();
        let raw_template_html = fs::read_to_string(static_dir.join("index.html")).unwrap();

        Assets {
            search_js,
            styles_css,
            favicon_svg,
            raw_template_html,
        }
    };

    // Clear out generated-docs dir and populate it with static assets
    // (.js, .css, etc.) - note that this does not actually delete the
    // directory if it already existed, but rather deletes all the files
    // inside it. That way, if you have a shell open to that directory,
    // it doesn't get messed up from the dir having been deleted out from
    // under the shell.
    file::populate_build_dir(arena, out_dir.as_ref(), &assets)?;

    Docs::new(
        arena,
        loaded_module,
        assets.raw_template_html.as_ref(),
        pkg_name,
        opt_user_specified_base_url,
    )
    .generate(out_dir)
}

struct Ability<'a> {
    name: &'a str,
    docs_url: &'a str,
}

struct BodyEntry<'a> {
    pub entry_name: &'a str,
    pub type_vars_names: &'a [&'a str],
    pub type_annotation: TypeAnnotation,
    pub docs: Option<&'a str>,
}

struct Docs<'a> {
    arena: &'a Bump,
    header_doc_comment: &'a str,
    raw_template_html: &'a str,
    pkg_name: &'a str,
    opt_user_specified_base_url: Option<&'a str>,
    sb_entries: Vec<'a, SBEntry<'a>>,
    body_entries_by_module: Vec<'a, (ModuleId, &'a [BodyEntry<'a>])>,
    scopes_by_module: Vec<'a, (ModuleId, Scope)>,
    interns: Interns,
    module_names: Vec<'a, (ModuleId, &'a str)>,
}

impl<'a> Docs<'a> {
    pub fn new(
        arena: &'a Bump,
        loaded_module: LoadedModule,
        raw_template_html: &'a str,
        pkg_name: &'a str,
        opt_user_specified_base_url: Option<&'a str>,
    ) -> Docs<'a> {
        let mut module_names =
            Vec::with_capacity_in(loaded_module.exposed_module_docs.len(), arena);
        let mut sb_entries = Vec::with_capacity_in(loaded_module.exposed_modules.len(), arena);
        let mut body_entries_by_module =
            Vec::with_capacity_in(loaded_module.exposed_modules.len(), arena);
        let mut scopes_by_module =
            Vec::with_capacity_in(loaded_module.exposed_modules.len(), arena);
        let header_doc_comment = arena.alloc_str(&loaded_module.header_doc_comment);

        for (module_id, docs) in loaded_module.exposed_module_docs.into_iter() {
            let opt_decls = loaded_module
                .typechecked
                .get(&module_id)
                .map(|checked| &checked.decls);

            module_names.push((module_id, &*arena.alloc_str(&docs.name)));

            let mut exposed = Vec::with_capacity_in(docs.exposed_symbols.len(), arena);
            for symbol in docs.exposed_symbols.iter() {
                if let Some(ident_ids) =
                    loaded_module.interns.all_ident_ids.get(&symbol.module_id())
                {
                    if let Some(name) = ident_ids.get_name(symbol.ident_id()) {
                        exposed.push(&*arena.alloc_str(name));
                    }
                }
            }

            sb_entries.push(SBEntry {
                link_text: arena.alloc_str(&docs.name),
                exposed,
                doc_comment: {
                    let todo = (); // TODO: thread through whatever doc comment might be above this.
                                   // Currently, we only know about docs.header_doc_comment, which is
                                   // the doc comment at the top of the module, NOT the doc comment above
                                   // the entry in the package, which is what we want here.
                    ""
                },
            });

            let mut body_entries = Vec::with_capacity_in(docs.entries.len(), arena);

            for entry in docs.entries.into_iter() {
                if let DocEntry::DocDef(def) = entry {
                    body_entries.push(BodyEntry {
                        entry_name: &*arena.alloc_str(&def.name),
                        type_vars_names: Vec::from_iter_in(
                            def.type_vars.iter().map(|s| &*arena.alloc_str(s)),
                            arena,
                        )
                        .into_bump_slice(),
                        type_annotation: {
                            if let TypeAnnotation::NoTypeAnn = def.type_annotation {
                                // If the user didn't specify a type annotation, infer it!
                                opt_decls
                                    .and_then(|decls| {
                                        decls
                                            .ann_from_symbol(def.symbol)
                                            .map(|result| {
                                                result.ok().map(|ann| {
                                                    clean_annotation(type_to_ann(
                                                        &ann.signature,
                                                        &loaded_module.interns,
                                                        &loaded_module.aliases,
                                                        &mut MutMap::default(),
                                                    ))
                                                })
                                            })
                                            .flatten()
                                    })
                                    .unwrap_or_else(|| {
                                        // We couldn't find it in decls, but it might have been a type alias.
                                        loaded_module
                                            .aliases
                                            .get(&def.symbol.module_id())
                                            .and_then(|aliases| {
                                                aliases.get(&def.symbol).map(
                                                    |(_imported, alias)| {
                                                        clean_annotation(type_to_ann(
                                                            &alias.typ,
                                                            &loaded_module.interns,
                                                            &loaded_module.aliases,
                                                            &mut MutMap::default(),
                                                        ))
                                                    },
                                                )
                                            })
                                            .unwrap_or(TypeAnnotation::NoTypeAnn)
                                    })
                            } else {
                                def.type_annotation
                            }
                        },
                        docs: def.docs.map(|str| &*arena.alloc_str(&str)),
                    });
                }
            }

            body_entries_by_module.push((module_id, body_entries.into_bump_slice()));
            scopes_by_module.push((module_id, docs.scope));
        }

        Self {
            interns: loaded_module.interns,
            header_doc_comment,
            sb_entries,
            body_entries_by_module,
            scopes_by_module,
            raw_template_html,
            pkg_name,
            opt_user_specified_base_url,
            module_names,
            arena,
        }
    }

    fn generate(self, build_dir: impl AsRef<Path>) -> Result<(), Problem> {
        let arena = &self.arena;
        let build_dir = build_dir.as_ref();

        self.render_to_disk(
            self.arena,
            // Takes the module name to be used as the directory name
            // (or None if this is the root index.html),
            // as well as the contents of the file.
            |opt_module_name, contents| {
                let dir = match opt_module_name {
                    Some(module_name) => PathBuf::from(build_dir)
                        // Replace dots in the module name with slashes
                        .join(module_name.replace('.', std::path::MAIN_SEPARATOR_STR)),
                    None => PathBuf::from(build_dir),
                };

                file::create_dir_all(arena, &dir)?;

                let path_buf = dir.join("index.html");

                file::write(arena, path_buf, contents)
            },
        )
    }

    fn module_name(&'a self, module_id: ModuleId) -> &'a str {
        self.module_names
            .iter()
            .find_map(|(id, name)| if *id == module_id { Some(*name) } else { None })
            .unwrap_or("")
    }
}

/// After generating a type annotation, clean up its generated variables -
/// e.g. if we have a tag union with a variable in it, drop that from the annotation.
fn clean_annotation(ann: TypeAnnotation) -> TypeAnnotation {
    let mut var_counts = MutMap::default();

    count_var_names(&ann, &mut var_counts);

    match ann {
        TypeAnnotation::TagUnion { tags, extension } => todo!(),
        TypeAnnotation::Function {
            args,
            arrow,
            output,
        } => todo!(),
        TypeAnnotation::ObscuredTagUnion => todo!(),
        TypeAnnotation::ObscuredRecord => todo!(),
        TypeAnnotation::BoundVariable(_) => todo!(),
        TypeAnnotation::Apply { name, parts } => todo!(),
        TypeAnnotation::Record { fields, extension } => todo!(),
        TypeAnnotation::Tuple { elems, extension } => todo!(),
        TypeAnnotation::Ability { members } => todo!(),
        TypeAnnotation::Wildcard => todo!(),
        TypeAnnotation::NoTypeAnn => todo!(),
        TypeAnnotation::Where { ann, implements } => todo!(),
        TypeAnnotation::As { ann, name, vars } => todo!(),
    }
}

fn clean_annotation_help(
    ann: TypeAnnotation,
    var_counts: &MutMap<std::string::String, u32>,
) -> TypeAnnotation {
    match ann {
        TypeAnnotation::TagUnion { tags, extension } => TypeAnnotation::TagUnion {
            tags: tags
                .into_iter()
                .map(|tag| Tag {
                    name: tag.name,
                    values: tag
                        .values
                        .into_iter()
                        .map(|v| clean_annotation_help(v, var_counts))
                        .collect(),
                })
                .collect(),
            extension: Box::new(clean_annotation_help(*extension, var_counts)),
        },
        TypeAnnotation::Function {
            args,
            arrow,
            output,
        } => TypeAnnotation::Function {
            args: args
                .into_iter()
                .map(|arg| clean_annotation_help(arg, var_counts))
                .collect(),
            arrow,
            output: Box::new(clean_annotation_help(*output, var_counts)),
        },
        TypeAnnotation::ObscuredTagUnion => TypeAnnotation::ObscuredTagUnion,
        TypeAnnotation::ObscuredRecord => TypeAnnotation::ObscuredRecord,
        TypeAnnotation::BoundVariable(var_name) => {
            if var_counts.get(&var_name) == Some(&1) {
                TypeAnnotation::BoundVariable("*".to_string())
            } else {
                TypeAnnotation::BoundVariable(var_name)
            }
        }
        TypeAnnotation::Apply { name, parts } => TypeAnnotation::Apply {
            name,
            parts: parts
                .into_iter()
                .map(|part| clean_annotation_help(part, var_counts))
                .collect(),
        },
        TypeAnnotation::Record { fields, extension } => TypeAnnotation::Record {
            fields: fields
                .into_iter()
                .map(|field| match field {
                    RecordField::RecordField {
                        name,
                        type_annotation,
                    } => RecordField::RecordField {
                        name,
                        type_annotation: clean_annotation_help(type_annotation, var_counts),
                    },
                    RecordField::OptionalField {
                        name,
                        type_annotation,
                    } => RecordField::OptionalField {
                        name,
                        type_annotation: clean_annotation_help(type_annotation, var_counts),
                    },
                    RecordField::LabelOnly { name } => RecordField::LabelOnly { name },
                })
                .collect(),
            extension: Box::new(clean_annotation_help(*extension, var_counts)),
        },
        TypeAnnotation::Tuple { elems, extension } => TypeAnnotation::Tuple {
            elems: elems
                .into_iter()
                .map(|elem| clean_annotation_help(elem, var_counts))
                .collect(),
            extension: Box::new(clean_annotation_help(*extension, var_counts)),
        },
        TypeAnnotation::Ability { members } => TypeAnnotation::Ability {
            members: members
                .into_iter()
                .map(|member| AbilityMember {
                    name: member.name,
                    type_annotation: clean_annotation_help(member.type_annotation, var_counts),
                    able_variables: member
                        .able_variables
                        .into_iter()
                        .map(|(name, anns)| {
                            (
                                name,
                                anns.into_iter()
                                    .map(|ann| clean_annotation_help(ann, var_counts))
                                    .collect(),
                            )
                        })
                        .collect(),
                    docs: member.docs,
                })
                .collect(),
        },
        TypeAnnotation::Wildcard => TypeAnnotation::Wildcard,
        TypeAnnotation::NoTypeAnn => TypeAnnotation::NoTypeAnn,
        TypeAnnotation::Where { ann, implements } => TypeAnnotation::Where {
            ann: Box::new(clean_do!(),
        TypeAnnotation::As { ann, name, vars } => todo!(),
    }
}

fn count_var_names(ann: &TypeAnnotation, var_counts: &mut MutMap<std::string::String, u32>) {
    match ann {
        TypeAnnotation::TagUnion { tags, extension } => {
            for tag in tags {
                for value in &tag.values {
                    count_var_names(value, var_counts);
                }
            }
            count_var_names(extension, var_counts);
        }
        TypeAnnotation::Function {
            args,
            arrow: _,
            output,
        } => {
            for arg in args {
                count_var_names(arg, var_counts);
            }
            count_var_names(output, var_counts);
        }
        TypeAnnotation::BoundVariable(var) => {
            var_counts
                .entry(var.to_string())
                .and_modify(|count| *count += 1)
                .or_insert(1);
        }
        TypeAnnotation::Apply { name: _, parts } => {
            for part in parts {
                count_var_names(part, var_counts);
            }
        }
        TypeAnnotation::Record { fields, extension } => {
            for field in fields {
                match field {
                    RecordField::RecordField {
                        name: _,
                        type_annotation,
                    }
                    | RecordField::OptionalField {
                        name: _,
                        type_annotation,
                    } => {
                        count_var_names(type_annotation, var_counts);
                    }
                    RecordField::LabelOnly { name: _ } => {}
                }
            }
            count_var_names(extension, var_counts);
        }
        TypeAnnotation::Tuple { elems, extension } => {
            for elem in elems {
                count_var_names(elem, var_counts);
            }
            count_var_names(extension, var_counts);
        }
        TypeAnnotation::Ability { members } => {
            for member in members {
                count_var_names(&member.type_annotation, var_counts);
                for (_, ability_anns) in &member.able_variables {
                    for ability_ann in ability_anns {
                        count_var_names(ability_ann, var_counts);
                    }
                }
            }
        }
        TypeAnnotation::Wildcard => {}
        TypeAnnotation::Where { ann, implements } => {
            count_var_names(ann, var_counts);
            for implement in implements {
                for ability in &implement.abilities {
                    count_var_names(ability, var_counts);
                }
            }
        }
        TypeAnnotation::As {
            ann,
            name: _,
            vars: _,
        } => {
            count_var_names(ann, var_counts);
        }
        TypeAnnotation::ObscuredTagUnion
        | TypeAnnotation::ObscuredRecord
        | TypeAnnotation::NoTypeAnn => {
            // No-op
        }
    }
}

fn type_to_ann(
    typ: &Type,
    interns: &Interns,
    aliases: &MutMap<ModuleId, MutMap<Symbol, (bool, Alias)>>,
    var_names: &mut MutMap<Variable, std::string::String>,
) -> TypeAnnotation {
    use roc_types::types::RecordField;
    use std::vec::Vec;

    match typ {
        Type::EmptyRec => TypeAnnotation::Record {
            fields: Vec::new(),
            extension: Box::new(TypeAnnotation::NoTypeAnn),
        },
        Type::EmptyTagUnion => TypeAnnotation::TagUnion {
            tags: Vec::new(),
            extension: Box::new(TypeAnnotation::NoTypeAnn),
        },
        Type::Function(args, _lambda_set, ret, fx) => TypeAnnotation::Function {
            args: args
                .iter()
                .map(|arg| type_to_ann(arg, interns, aliases, var_names))
                .collect(),
            arrow: if **fx == Type::Effectful {
                FunctionArrow::Effectful
            } else {
                FunctionArrow::Pure
            },
            output: Box::new(type_to_ann(ret, interns, aliases, var_names)),
        },
        Type::Record(fields, ext) => TypeAnnotation::Record {
            fields: fields
                .iter()
                .map(|(name, field)| match field {
                    RecordField::Required(t)
                    | RecordField::Demanded(t)
                    | RecordField::RigidRequired(t) => roc_load::docs::RecordField::RecordField {
                        name: name.to_string(),
                        type_annotation: type_to_ann(t, interns, aliases, var_names),
                    },
                    RecordField::Optional(t) | RecordField::RigidOptional(t) => {
                        roc_load::docs::RecordField::OptionalField {
                            name: name.to_string(),
                            type_annotation: type_to_ann(t, interns, aliases, var_names),
                        }
                    }
                })
                .collect(),
            extension: Box::new(match ext {
                TypeExtension::Open(t, _) => type_to_ann(t, interns, aliases, var_names),
                TypeExtension::Closed => TypeAnnotation::NoTypeAnn,
            }),
        },
        Type::Tuple(elems, ext) => TypeAnnotation::Tuple {
            elems: elems
                .iter()
                .map(|(_, t)| type_to_ann(t, interns, aliases, var_names))
                .collect(),
            extension: Box::new(match ext {
                TypeExtension::Open(t, _) => type_to_ann(t, interns, aliases, var_names),
                TypeExtension::Closed => TypeAnnotation::NoTypeAnn,
            }),
        },
        Type::TagUnion(tags, ext) => TypeAnnotation::TagUnion {
            tags: tags
                .iter()
                .map(|(name, args)| Tag {
                    name: name.0.to_string(),
                    values: args
                        .iter()
                        .map(|arg| type_to_ann(arg, interns, aliases, var_names))
                        .collect(),
                })
                .collect(),
            extension: Box::new(match ext {
                TypeExtension::Open(t, _) => type_to_ann(t, interns, aliases, var_names),
                TypeExtension::Closed => TypeAnnotation::NoTypeAnn,
            }),
        },
        Type::FunctionOrTagUnion(tag_name, _, ext) => TypeAnnotation::TagUnion {
            tags: vec![Tag {
                name: tag_name.0.to_string(),
                values: Vec::new(),
            }],
            extension: Box::new(match ext {
                TypeExtension::Open(t, _) => type_to_ann(t, interns, aliases, var_names),
                TypeExtension::Closed => TypeAnnotation::NoTypeAnn,
            }),
        },
        Type::ClosureTag { name, captures, .. } => TypeAnnotation::Apply {
            name: name.as_str(interns).to_string(),
            parts: captures
                .iter()
                .map(|t| type_to_ann(t, interns, aliases, var_names))
                .collect(),
        },
        Type::UnspecializedLambdaSet { .. } => TypeAnnotation::NoTypeAnn,
        Type::DelayedAlias(AliasCommon {
            symbol,
            type_arguments: _, // TODO should we make use of these in the unwrapped type?
            lambda_set_variables: _,
            infer_ext_in_output_types: _,
        }) => {
            // Unwrap delayed aliases
            aliases
                .get(&symbol.module_id())
                .and_then(|module_aliases| {
                    module_aliases.get(&symbol).map(|(_imported, alias)| {
                        type_to_ann(&alias.typ, interns, aliases, var_names)
                    })
                })
                .unwrap_or(TypeAnnotation::NoTypeAnn)
        }
        Type::Alias { actual, .. } => type_to_ann(actual, interns, aliases, var_names),
        Type::RecursiveTagUnion(_, tags, ext) => TypeAnnotation::TagUnion {
            tags: tags
                .iter()
                .map(|(name, args)| Tag {
                    name: name.0.to_string(),
                    values: args
                        .iter()
                        .map(|arg| type_to_ann(arg, interns, aliases, var_names))
                        .collect(),
                })
                .collect(),
            extension: Box::new(match ext {
                TypeExtension::Open(t, _) => type_to_ann(t, interns, aliases, var_names),
                TypeExtension::Closed => TypeAnnotation::NoTypeAnn,
            }),
        },
        Type::Apply(symbol, args, _) => TypeAnnotation::Apply {
            name: symbol.as_str(interns).to_string(),
            parts: args
                .iter()
                .map(|arg| type_to_ann(&arg.value, interns, aliases, var_names))
                .collect(),
        },
        Type::Variable(var) => TypeAnnotation::BoundVariable({
            let vars_named = var_names.len();

            var_names
                .entry(*var)
                .or_insert_with(|| {
                    // 'a', 'b', 'c', ...etc.
                    // If we get to 'z' and still need more, continue with 'a2', 'a3', etc.
                    let letter = (b'a' + (vars_named % 26) as u8) as char;
                    let number = (vars_named / 26) + 1;

                    if number > 1 {
                        format!("{}{}", letter, number)
                    } else {
                        letter.to_string()
                    }
                })
                .clone()
        }),
        Type::RangedNumber(_) => TypeAnnotation::Apply {
            name: "Num".to_string(),
            parts: Vec::new(),
        },
        Type::Error => TypeAnnotation::NoTypeAnn,
        Type::Pure => TypeAnnotation::NoTypeAnn,
        Type::Effectful => TypeAnnotation::NoTypeAnn,
    }
}

#[derive(Debug)]
struct SBEntry<'a> {
    /// In the source code, this will appear in a module's `exposes` list like:
    ///
    /// [
    ///     Foo,
    ///     Bar,
    ///     ## Heading
    ///     Baz,
    ///     Blah,
    /// ]
    pub link_text: &'a str,

    /// The entries this module exposes (types, values, abilities)
    pub exposed: Vec<'a, &'a str>,

    /// These doc comments get interpreted as flat strings; Markdown is not allowed
    /// in them, because they will be rendered in the sidebar as plain text.
    pub doc_comment: &'a str,
}

impl<'a> Docs<'a> {
    fn render_to_disk<Problem>(
        &'a self,
        arena: &'a Bump,
        // Takes the module name to be used as the directory name (or None if this is the root index.html),
        // as well as the contents of the file.
        write_to_disk: impl Fn(Option<&str>, &str) -> Result<(), Problem>,
    ) -> Result<(), Problem> {
        let package_doc_comment_html = self.header_doc_comment;
        let raw_template_html = self.raw_template_html;
        let package_name = self.pkg_name;
        let mut buf = String::with_capacity_in(raw_template_html.len() + 2048, arena);
        let mut module_template_html =
            String::with_capacity_in(raw_template_html.len() + 2048, arena);
        let mut sidebar_links = String::with_capacity_in(4096, arena);

        let sidebar_links = &mut sidebar_links;

        self.render_sidebar(sidebar_links);

        // Write index.html for package (/index.html)
        {
            let mut src = raw_template_html;

            {
                src = advance_past("<!-- base -->", src, &mut buf);
                write_base_url(self.opt_user_specified_base_url, &mut buf);
            }

            {
                src = advance_past("<!-- Prefetch links -->", src, &mut buf);

                for (index, (_, module_name)) in self.module_names.iter().enumerate() {
                    if index > 0 {
                        buf.push_str("\n    ");
                    }

                    let _ = write!(buf, "<link rel='prefetch' href='{module_name}'/>",);
                }
            }

            // Set module_template_html to be all the replacements we've made so far,
            // plus the rest of the source template. We'll use this partially-completed
            // template later on for the individual modules.
            {
                module_template_html.push_str(&buf);
                module_template_html.push_str(&src);
            }

            {
                src = advance_past("<!-- Page title -->", src, &mut buf);
                let _ = write!(buf, "<title>{package_name}</title>");
            }

            {
                src = advance_past("<!-- Module links -->", src, &mut buf);
                buf.push_str(&sidebar_links);
            }

            {
                src = advance_past("<!-- Package Name -->", src, &mut buf);
                render_package_name_link(package_name, &mut buf);
            }

            {
                src = advance_past("<!-- Module Docs -->", src, &mut buf);

                if package_doc_comment_html.is_empty() {
                    buf.push_str("Choose a module from the list to see its documentation.");
                } else {
                    buf.push_str(package_doc_comment_html);
                }
            }

            {
                // Write the rest of the template into the buffer.
                buf.push_str(&src);

                // Finally, write the accumulated buffer to disk.
                write_to_disk(None, &buf)?;

                buf.clear(); // We're done with this now. It's ready to be reused!
            }
        }

        // Write each package module's index.html file
        for (module_id, module_name) in self.module_names.iter() {
            let mut src = module_template_html.as_str();

            {
                {
                    src = advance_past("<!-- Page title -->", src, &mut buf);
                    let _ = write!(buf, "<title>{module_name} - {package_name}</title>",);
                }

                {
                    src = advance_past("<!-- Module links -->", src, &mut buf);
                    buf.push_str(sidebar_links);
                }

                {
                    src = advance_past("<!-- Package Name -->", src, &mut buf);
                    render_package_name_link(package_name, &mut buf);
                }
            }

            {
                src = advance_past("<!-- Module Docs -->", src, &mut buf);
                self.render_module(arena, *module_id, &mut buf);
            }

            {
                // Write the rest of the template into the buffer.
                buf.push_str(&src);
            }

            {
                // Finally, write the accumulated buffer to disk.
                write_to_disk(Some(module_name), &buf)?;
            }

            buf.clear(); // We're done with this now. It's ready to be reused in the next iteration of the loop!
        }

        Ok(())
    }

    fn render_sidebar(&'a self, buf: &mut String<'_>) {
        for entry in self.sb_entries.iter() {
            let heading = entry.doc_comment;

            if !heading.is_empty() {
                let _ = write!(buf, "\t<h3 class=\"sidebar-heading\">{heading}</a>\n");
            }

            let module_name = entry.link_text;

            // Sidebar entries should all be relative URLs and unqualified names
            let _ = write!(
                buf,
                "<div class='sidebar-entry'><a class='sidebar-module-link' href='{module_name}'>{module_name}</a><div class='sidebar-sub-entries'>",
            );

            for name in entry.exposed.iter() {
                let _ = write!(buf, "<a href='{module_name}#{name}'>{name}</a>",);
            }

            buf.push_str("</div></div>");
        }
    }

    fn render_type(&'a self, arena: &'a Bump, typ: &TypeAnnotation, buf: &mut String<'a>) {
        todo!("implement render_type");
    }

    fn render_absolute_url(&'a self, ident_id: IdentId, module_id: ModuleId, buf: &mut String<'_>) {
        todo!();
        // let base_url = self.base_url(module_id);

        // let _ = write!(
        //     buf,
        //     // e.g. "https://example.com/Str#isEmpty"
        //     "{base_url}{}#{}",
        //     self.module_name(module_id),
        //     self.ident_name(module_id, ident_id)
        // );
    }

    fn render_module(&'a self, arena: &'a Bump, module_id: ModuleId, buf: &mut String<'a>) {
        let module_name = self.module_name(module_id);
        let _ = write!(
            buf,
            "<h2 class='module-name'><a href='/{module_name}'>{module_name}</a></h2>"
        );

        let entries = self
            .body_entries_by_module
            .iter()
            .find_map(|(id, entries)| {
                if *id == module_id {
                    Some(*entries)
                } else {
                    None
                }
            })
            .unwrap_or(&[]);

        for entry in entries {
            let name = entry.entry_name;

            let _ = write!(
                buf,
                "<section><h3 id='{name}' class='entry-name'><a href='{module_name}#{name}'>{name}</a>"
            );

            const TYPE_START: &str = " : ";

            buf.push_str(TYPE_START);

            let old_buf_len = buf.len();

            type_annotation_to_html(0, buf, &entry.type_annotation, false);

            if buf.len() == old_buf_len {
                // If we didn't add any actual anntoation, trim off the `:` from the end
                buf.truncate(buf.len() - TYPE_START.len());
            }

            buf.push_str("</h3>");

            if let Some(docs) = entry.docs {
                if let Some((_id, scope)) = self
                    .scopes_by_module
                    .iter()
                    .find(|(id, _scope)| *id == module_id)
                {
                    markdown_to_html(arena, buf, arena, scope, &self.interns, docs);
                }
            }

            buf.push_str("</section>");
        }
    }

    fn render_ability_decl(
        &'a self,
        arena: &'a Bump,
        members: impl Iterator<Item = &'a AbilityMember<'a, TypeAnnotation>>,
        buf: &mut String<'a>,
    ) {
        buf.push_str(" <span class='kw'>implements {</span>");

        let mut any_rendered = false;

        for (index, member) in members.enumerate() {
            if index == 0 {
                buf.push_str(" <h4 class='kw'>implements</h4><ul class='opaque-abilities'>");
                any_rendered = true;
            }

            let _ = write!(buf, "<li>{} : ", member.entry_name);
            // TODO should we render docs for each member individually?

            self.render_type(arena, &member.type_annotation, buf);

            buf.push_str("</li>");
        }

        if any_rendered {
            buf.push_str("</ul>");
        }

        buf.push_str("<span class='kw'>}</span>");
    }

    fn render_type_alias_decl(
        &'a self,
        arena: &'a Bump,
        type_var_names: impl Iterator<Item = &'a str>,
        alias: &'a TypeAnnotation,
        buf: &mut String<'a>,
    ) {
        // Render the type variables
        // e.g. if the alias is `Foo a b c :`, render the `a b c` part
        for type_var_name in type_var_names {
            buf.push(' ');
            buf.push_str(type_var_name);
        }

        buf.push_str(" <span class='kw'>:</span>");

        self.render_type(arena, &alias, buf);
    }

    fn render_opaque_type_decl(
        &'a self,
        _arena: &'a Bump, // TODO this will be needed in the future arena API
        type_var_names: impl Iterator<Item = impl AsRef<str>>,
        abilities: impl Iterator<Item = &'a Ability<'a>>,
        buf: &mut String<'_>,
    ) {
        // Render the type variables
        // e.g. if the opaque type is `Foo a b c :=`, render the `a b c` part
        for type_var_name in type_var_names {
            buf.push(' ');
            buf.push_str(type_var_name.as_ref());
        }

        buf.push_str(" <span class='kw'>:=</span> <a class='opaque-note-link' href='#opaque-note'>(opaque - TODO: put this in italics, have a section at the end of the page with a note, have JS hide it on load unless that's the anchor of the page e.g. you opened it in a new tab - and then have JS display it inline on click here)</a>");
        let mut any_rendered = false;

        for (index, ability) in abilities.enumerate() {
            let name = ability.name;
            let href = ability.docs_url;

            if index == 0 {
                buf.push_str(" <h4 class='kw'>implements</h4><ul class='opaque-abilities'>");
                any_rendered = true;
            } else {
                buf.push_str(",</li> ")
            }

            let _ = write!(buf, "<li><a href='{href}'>{name}</a>");
        }

        if any_rendered {
            buf.push_str("</li></ul>");
        }
    }

    fn render_val_decl(&'a self, arena: &'a Bump, typ: &'a TypeAnnotation, buf: &mut String<'a>) {
        buf.push_str(" <span class='kw'>:</span>");

        self.render_type(arena, &typ, buf)
    }
}
#[derive(Debug)]
pub struct AbilityAnn<'a> {
    name: &'a str,
}

// fn render_package_index(root_module: &LoadedModule) -> String {
//     // The list items containing module links
//     let mut module_list_buf = String::new();

//     for (_, module) in root_module.docs_by_module.iter() {
//         // The anchor tag containing the module link
//         let mut link_buf = String::new();

//         push_html(
//             &mut link_buf,
//             "a",
//             vec![("href", module.name.as_str())],
//             module.name.as_str(),
//         );

//         push_html(&mut module_list_buf, "li", vec![], link_buf.as_str());
//     }

//     // The HTML for the index page
//     let mut index_buf = String::new();

//     push_html(
//         &mut index_buf,
//         "h2",
//         vec![("class", "module-name")],
//         "Exposed Modules",
//     );
//     push_html(
//         &mut index_buf,
//         "ul",
//         vec![("class", "index-module-links")],
//         module_list_buf.as_str(),
//     );

//     index_buf
// }

// fn render_module_documentation(
//     module: &ModuleDocumentation,
//     root_module: &LoadedModule,
//     all_exposed_symbols: &VecSet<Symbol>,
// ) -> String {
//     let mut buf = String::new();
//     let module_name = module.name.as_str();

//     push_html(&mut buf, "h2", vec![("class", "module-name")], {
//         let mut link_buf = String::new();

//         push_html(&mut link_buf, "a", vec![("href", "/")], module_name);

//         link_buf
//     });

//     for entry in &module.entries {
//         match entry {
//             DocEntry::DocDef(doc_def) => {
//                 // Only render entries that are exposed
//                 if all_exposed_symbols.contains(&doc_def.symbol) {
//                     buf.push_str("<section>");

//                     let def_name = doc_def.name.as_str();
//                     let href = format!("{module_name}#{def_name}");
//                     let mut content = String::new();

//                     push_html(&mut content, "a", vec![("href", href.as_str())], LINK_SVG);
//                     push_html(&mut content, "strong", vec![], def_name);

//                     for type_var in &doc_def.type_vars {
//                         content.push(' ');
//                         content.push_str(type_var.as_str());
//                     }

//                     let type_ann = &doc_def.type_annotation;

//                     if !matches!(type_ann, TypeAnnotation::NoTypeAnn) {
//                         // Ability declarations don't have ":" after the name, just `implements`
//                         if !matches!(type_ann, TypeAnnotation::Ability { .. }) {
//                             content.push_str(" :");
//                         }

//                         content.push(' ');

//                         type_annotation_to_html(0, &mut content, type_ann, false);
//                     }

//                     push_html(
//                         &mut buf,
//                         "h3",
//                         vec![("id", def_name), ("class", "entry-name")],
//                         content.as_str(),
//                     );

//                     if let Some(docs) = &doc_def.docs {
//                         markdown_to_html(
//                             &mut buf,
//                             all_exposed_symbols,
//                             &module.scope,
//                             docs,
//                             root_module,
//                         );
//                     }

//                     buf.push_str("</section>");
//                 }
//             }
//             DocEntry::DetachedDoc(docs) => {
//                 markdown_to_html(
//                     &mut buf,
//                     all_exposed_symbols,
//                     &module.scope,
//                     docs,
//                     root_module,
//                 );
//             }
//         };
//     }

//     buf
// }

// fn push_html(buf: &mut String, tag_name: &str, attrs: Vec<(&str, &str)>, content: impl AsRef<str>) {
//     buf.push('<');
//     buf.push_str(tag_name);

//     for (key, value) in &attrs {
//         buf.push(' ');
//         buf.push_str(key);
//         buf.push_str("=\"");
//         buf.push_str(value);
//         buf.push('"');
//     }

//     if !&attrs.is_empty() {
//         buf.push(' ');
//     }

//     buf.push('>');

//     buf.push_str(content.as_ref());

//     buf.push_str("</");
//     buf.push_str(tag_name);
//     buf.push('>');
// }

// fn base_url() -> String {
//     // e.g. "builtins/" in "https://roc-lang.org/builtins/Str"
//     //
//     // TODO make this a CLI flag to the `docs` subcommand instead of an env var
//     match std::env::var("ROC_DOCS_URL_ROOT") {
//         Ok(root_builtins_path) => {
//             let mut url_str = String::with_capacity(root_builtins_path.len() + 64);

//             if !root_builtins_path.starts_with('/') {
//                 url_str.push('/');
//             }

//             url_str.push_str(&root_builtins_path);

//             if !root_builtins_path.ends_with('/') {
//                 url_str.push('/');
//             }

//             url_str
//         }
//         _ => {
//             let mut url_str = String::with_capacity(64);

//             url_str.push('/');

//             url_str
//         }
//     }
// }

// // TODO render version as well
// fn render_name_link(name: &str) -> String {
//     let mut buf = String::new();

//     push_html(&mut buf, "h1", vec![("class", "pkg-full-name")], {
//         let mut link_buf = String::new();

//         // link to root (= docs overview page)
//         push_html(
//             &mut link_buf,
//             "a",
//             vec![("href", base_url().as_str())],
//             name,
//         );

//         link_buf
//     });

//     buf
// }

// fn render_sidebar<'a, I: Iterator<Item = &'a ModuleDocumentation>>(modules: I) -> String {
//     let mut buf = String::new();

//     for module in modules {
//         let href = module.name.as_str();
//         let mut sidebar_entry_content = String::new();

//         push_html(
//             &mut sidebar_entry_content,
//             "a",
//             vec![("class", "sidebar-module-link"), ("href", href)],
//             module.name.as_str(),
//         );

//         let entries = {
//             let mut entries_buf = String::new();

//             for entry in &module.entries {
//                 if let DocEntry::DocDef(doc_def) = entry {
//                     if module.exposed_symbols.contains(&doc_def.symbol) {
//                         let mut entry_href = String::new();

//                         entry_href.push_str(href);
//                         entry_href.push('#');
//                         entry_href.push_str(doc_def.name.as_str());

//                         push_html(
//                             &mut entries_buf,
//                             "a",
//                             vec![("href", entry_href.as_str())],
//                             doc_def.name.as_str(),
//                         );
//                     }
//                 }
//             }

//             entries_buf
//         };

//         push_html(
//             &mut sidebar_entry_content,
//             "div",
//             vec![("class", "sidebar-sub-entries")],
//             entries.as_str(),
//         );

//         push_html(
//             &mut buf,
//             "div",
//             vec![("class", "sidebar-entry")],
//             sidebar_entry_content.as_str(),
//         );
//     }

//     buf
// }

pub fn load_module_for_docs(filename: PathBuf) -> LoadedModule {
    let arena = Bump::new();
    let load_config = LoadConfig {
        target: Target::LinuxX64, // This is just type-checking for docs, so "target" doesn't matter
        function_kind: roc_solve::FunctionKind::LambdaSet,
        render: roc_reporting::report::RenderTarget::ColorTerminal,
        palette: roc_reporting::report::DEFAULT_PALETTE,
        threading: Threading::AllAvailable,
        exec_mode: ExecutionMode::Check,
    };
    match roc_load::load_and_typecheck(
        &arena,
        filename.clone(),
        Some(filename),
        RocCacheDir::Persistent(cache::roc_cache_dir().as_path()),
        load_config,
    ) {
        Ok(loaded) => loaded,
        Err(LoadingProblem::FormattedReport(report)) => {
            eprintln!("{report}");
            std::process::exit(1);
        }
        Err(e) => panic!("{e:?}"),
    }
}

const INDENT: &str = "    ";

fn indent(buf: &mut String, times: usize) {
    for _ in 0..times {
        buf.push_str(INDENT);
    }
}

fn new_line(buf: &mut String) {
    buf.push('\n');
}

fn type_annotation_to_html(
    indent_level: usize,
    buf: &mut String,
    type_ann: &TypeAnnotation,
    needs_parens: bool,
) {
    let is_multiline = should_be_multiline(type_ann);
    match type_ann {
        TypeAnnotation::TagUnion { tags, extension } => {
            if tags.is_empty() {
                buf.push_str("[]");
            } else {
                let tags_len = tags.len();

                let tag_union_indent = indent_level + 1;

                if is_multiline {
                    new_line(buf);

                    indent(buf, tag_union_indent);
                }

                buf.push('[');

                if is_multiline {
                    new_line(buf);
                }

                let next_indent_level = tag_union_indent + 1;

                for (index, tag) in tags.iter().enumerate() {
                    if is_multiline {
                        indent(buf, next_indent_level);
                    }

                    buf.push_str(tag.name.as_str());

                    for type_value in &tag.values {
                        buf.push(' ');
                        type_annotation_to_html(next_indent_level, buf, type_value, true);
                    }

                    if is_multiline {
                        if index < (tags_len - 1) {
                            buf.push(',');
                        }

                        new_line(buf);
                    }
                }

                if is_multiline {
                    indent(buf, tag_union_indent);
                }

                buf.push(']');
            }

            type_annotation_to_html(indent_level, buf, extension, true);
        }
        TypeAnnotation::BoundVariable(var_name) => {
            buf.push_str(var_name);
        }
        TypeAnnotation::Apply { name, parts } => {
            if parts.is_empty() {
                buf.push_str(name);
            } else {
                if needs_parens {
                    buf.push('(');
                }

                buf.push_str(name);
                for part in parts {
                    buf.push(' ');
                    type_annotation_to_html(indent_level, buf, part, true);
                }

                if needs_parens {
                    buf.push(')');
                }
            }
        }
        TypeAnnotation::Record { fields, extension } => {
            if fields.is_empty() {
                buf.push_str("{}");
            } else {
                let fields_len = fields.len();
                let record_indent = indent_level + 1;

                if is_multiline {
                    new_line(buf);
                    indent(buf, record_indent);
                }

                buf.push('{');

                if is_multiline {
                    new_line(buf);
                }

                let next_indent_level = record_indent + 1;

                for (index, field) in fields.iter().enumerate() {
                    if is_multiline {
                        indent(buf, next_indent_level);
                    } else {
                        buf.push(' ');
                    }

                    let fields_name = match field {
                        RecordField::RecordField { name, .. } => name,
                        RecordField::OptionalField { name, .. } => name,
                        RecordField::LabelOnly { name } => name,
                    };

                    buf.push_str(fields_name.as_str());

                    match field {
                        RecordField::RecordField {
                            type_annotation, ..
                        } => {
                            buf.push_str(" : ");
                            type_annotation_to_html(next_indent_level, buf, type_annotation, false);
                        }
                        RecordField::OptionalField {
                            type_annotation, ..
                        } => {
                            buf.push_str(" ? ");
                            type_annotation_to_html(next_indent_level, buf, type_annotation, false);
                        }
                        RecordField::LabelOnly { .. } => {}
                    }

                    if is_multiline {
                        if index < (fields_len - 1) {
                            buf.push(',');
                        }

                        new_line(buf);
                    }
                }

                if is_multiline {
                    indent(buf, record_indent);
                } else {
                    buf.push(' ');
                }

                buf.push('}');
            }

            type_annotation_to_html(indent_level, buf, extension, true);
        }
        TypeAnnotation::Function {
            args,
            output,
            arrow,
        } => {
            let mut paren_is_open = false;
            let mut peekable_args = args.iter().peekable();

            while let Some(arg) = peekable_args.next() {
                if is_multiline {
                    if !should_be_multiline(arg) {
                        new_line(buf);
                    }
                    indent(buf, indent_level + 1);
                }
                if needs_parens && !paren_is_open {
                    buf.push('(');
                    paren_is_open = true;
                }

                let child_needs_parens = matches!(arg, TypeAnnotation::Function { .. });
                type_annotation_to_html(indent_level, buf, arg, child_needs_parens);

                if peekable_args.peek().is_some() {
                    buf.push_str(", ");
                }
            }

            if is_multiline {
                new_line(buf);
                indent(buf, indent_level + 1);
            } else {
                buf.push(' ');
            }

            // Push either `->` or `=>` depending on whether it's a pure or effectful function.
            let arrow_body = match arrow {
                FunctionArrow::Pure => '-',
                FunctionArrow::Effectful => '=',
            };

            buf.push(arrow_body);
            buf.push_str("> ");

            let mut next_indent_level = indent_level;

            if should_be_multiline(output) {
                next_indent_level += 1;
            }

            type_annotation_to_html(next_indent_level, buf, output, false);
            if needs_parens && paren_is_open {
                buf.push(')');
            }
        }
        TypeAnnotation::Ability { members } => {
            buf.push_str(keyword::IMPLEMENTS);

            for member in members {
                new_line(buf);
                indent(buf, indent_level + 1);

                // TODO use member.docs somehow. This doesn't look good though:
                // if let Some(docs) = &member.docs {
                //     buf.push_str("## ");
                //     buf.push_str(docs);

                //     new_line(buf);
                //     indent(buf, indent_level + 1);
                // }

                buf.push_str(&member.name);
                buf.push_str(" : ");

                type_annotation_to_html(indent_level + 1, buf, &member.type_annotation, false);

                if !member.able_variables.is_empty() {
                    new_line(buf);
                    indent(buf, indent_level + 2);
                    buf.push_str(keyword::WHERE);

                    for (index, (name, type_anns)) in member.able_variables.iter().enumerate() {
                        if index != 0 {
                            buf.push(',');
                        }

                        buf.push(' ');
                        buf.push_str(name);
                        buf.push(' ');
                        buf.push_str(keyword::IMPLEMENTS);

                        for (index, ann) in type_anns.iter().enumerate() {
                            if index != 0 {
                                buf.push_str(" &");
                            }

                            buf.push(' ');

                            type_annotation_to_html(indent_level + 2, buf, ann, false);
                        }
                    }
                }
            }
        }
        TypeAnnotation::ObscuredTagUnion => {
            buf.push_str("[@..]");
        }
        TypeAnnotation::ObscuredRecord => {
            buf.push_str("{ @.. }");
        }
        TypeAnnotation::NoTypeAnn => {}
        TypeAnnotation::Wildcard => buf.push('*'),
        TypeAnnotation::Tuple { elems, extension } => {
            let elems_len = elems.len();
            let tuple_indent = indent_level + 1;

            if is_multiline {
                new_line(buf);
                indent(buf, tuple_indent);
            }

            buf.push('(');

            if is_multiline {
                new_line(buf);
            }

            let next_indent_level = tuple_indent + 1;

            for (index, elem) in elems.iter().enumerate() {
                if is_multiline {
                    indent(buf, next_indent_level);
                }

                type_annotation_to_html(next_indent_level, buf, elem, false);

                if is_multiline {
                    if index < (elems_len - 1) {
                        buf.push(',');
                    }

                    new_line(buf);
                }
            }

            if is_multiline {
                indent(buf, tuple_indent);
            }

            buf.push(')');

            type_annotation_to_html(indent_level, buf, extension, true);
        }
        TypeAnnotation::Where { ann, implements } => {
            type_annotation_to_html(indent_level, buf, ann, false);

            new_line(buf);
            indent(buf, indent_level + 1);

            buf.push_str(keyword::WHERE);

            let multiline_implements = implements
                .iter()
                .any(|imp| imp.abilities.iter().any(should_be_multiline));

            for (index, imp) in implements.iter().enumerate() {
                if index != 0 {
                    buf.push(',');
                }

                if multiline_implements {
                    new_line(buf);
                    indent(buf, indent_level + 2);
                } else {
                    buf.push(' ')
                }

                buf.push_str(&imp.name);
                buf.push(' ');
                buf.push_str(keyword::IMPLEMENTS);
                buf.push(' ');

                for (index, ability) in imp.abilities.iter().enumerate() {
                    if index != 0 {
                        buf.push_str(" & ");
                    }

                    type_annotation_to_html(indent_level, buf, ability, false);
                }
            }
        }
        TypeAnnotation::As { ann, name, vars } => {
            type_annotation_to_html(indent_level, buf, ann, true);
            buf.push(' ');
            buf.push_str(name);

            for var in vars {
                buf.push(' ');
                buf.push_str(var);
            }
        }
    }
}

fn should_be_multiline(type_ann: &TypeAnnotation) -> bool {
    match type_ann {
        TypeAnnotation::TagUnion { tags, extension } => {
            tags.len() > 1
                || should_be_multiline(extension)
                || tags
                    .iter()
                    .any(|tag| tag.values.iter().any(should_be_multiline))
        }
        TypeAnnotation::Function {
            args,
            output,
            arrow: _,
        } => args.len() > 2 || should_be_multiline(output) || args.iter().any(should_be_multiline),
        TypeAnnotation::ObscuredTagUnion => false,
        TypeAnnotation::ObscuredRecord => false,
        TypeAnnotation::BoundVariable(_) => false,
        TypeAnnotation::Apply { parts, .. } => parts.iter().any(should_be_multiline),
        TypeAnnotation::Record { fields, extension } => {
            fields.len() > 1
                || should_be_multiline(extension)
                || fields.iter().any(|field| match field {
                    RecordField::RecordField {
                        type_annotation, ..
                    } => should_be_multiline(type_annotation),
                    RecordField::OptionalField {
                        type_annotation, ..
                    } => should_be_multiline(type_annotation),
                    RecordField::LabelOnly { .. } => false,
                })
        }
        TypeAnnotation::Ability { .. } => true,
        TypeAnnotation::Wildcard => false,
        TypeAnnotation::NoTypeAnn => false,
        TypeAnnotation::Tuple { elems, extension } => {
            elems.len() > 1
                || should_be_multiline(extension)
                || elems.iter().any(should_be_multiline)
        }
        TypeAnnotation::Where { ann, implements } => {
            should_be_multiline(ann)
                || implements
                    .iter()
                    .any(|imp| imp.abilities.iter().any(should_be_multiline))
        }
        TypeAnnotation::As {
            ann,
            name: _,
            vars: _,
        } => should_be_multiline(ann),
    }
}

// fn name_from_ident_id(&self, ident_id: IdentId, ident_ids: &'a IdentIds) -> &'a str {
//     ident_ids.get_name(ident_id).unwrap_or_else(|| {
//             if cfg!(debug_assertions) {
//                 unreachable!("docs generation tried to render a relative URL for IdentId {:?} but it was not found in home_identids, which should never happen!", ident_id);
//             }

//             // In release builds, don't panic, just gracefully continue
//             // by not writing the url. It'll be a bug, but at least
//             // it won't block the user from seeing *some* docs rendered.
//             ""
//         })
// }

fn render_package_name_link(name: &str, buf: &mut String<'_>) {
    let _ = write!(buf, "<h1 class='pkg-full-name'><a href='/'>{name}</a></h1>");
}

fn advance_past<'a>(needle: &'static str, src: &'a str, buf: &mut String<'_>) -> &'a str {
    if let Some(start_index) = src.find(needle) {
        // Copy over everything up to this point.
        buf.push_str(&src[..start_index]);

        // Advance past the end of this string.
        &src[(start_index + needle.len())..]
    } else {
        unreachable!( // TODO replace this with a panic in debug builds and a much more concise crash in release
            "Compiler bug in docs generation code: could not find doc template section {:?} in the template - this should never happen!\n\nNOTE: advance_past must be called on each template section in the order they appear in the template! This improves performance, but means that working on sections out of order can lead to this error.\n\nAt this point, the remaining template was:\n\n{src}",
            needle
        );
    }
}

fn write_base_url(user_specified_base_url: Option<impl AsRef<str>>, buf: &mut String) {
    // e.g. "builtins/" in "https://roc-lang.org/builtins/Str"
    match user_specified_base_url {
        Some(root_builtins_path) => {
            let root_builtins_path = root_builtins_path.as_ref();

            if !root_builtins_path.starts_with('/') {
                buf.push('/');
            }

            buf.push_str(&root_builtins_path);

            if !root_builtins_path.ends_with('/') {
                buf.push('/');
            }
        }
        None => {
            buf.push('/');
        }
    }
}
