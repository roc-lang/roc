use crate::file::{self, Assets};
use crate::problem::Problem;
use crate::render_markdown::markdown_to_html;
use bumpalo::collections::Vec;
use bumpalo::{collections::string::String, Bump};
use core::fmt::{Debug, Write};
use roc_can::scope::Scope;
use roc_collections::MutMap;
use roc_load::docs::{DocEntry, RecordField, Tag, TypeAnnotation};
use roc_load::{ExecutionMode, LoadConfig, LoadedModule, LoadingProblem, Threading};
use roc_module::symbol::{IdentId, Interns, ModuleId, Symbol};
use roc_packaging::cache::{self, RocCacheDir};
use roc_parse::ast::FunctionArrow;
use roc_parse::keyword;
use roc_target::Target;
use roc_types::subs::{Content, FlatType, GetSubsSlice, Subs, Variable};
use roc_types::types::{Alias, AliasCommon, AliasKind, Type, TypeExtension};
use std::borrow::{Borrow, Cow};
use std::path::{Path, PathBuf};
use std::{fs, iter};

const ABILITIES_DOCS: &str = "https://www.roc-lang.org/abilities";

pub fn generate_docs_html<'a>(
    arena: &'a Bump,
    pkg_name: &str,
    root_file: impl AsRef<Path>,
    out_dir: impl AsRef<Path>,
    opt_user_specified_base_url: Option<&'a str>,
) -> Result<(), Problem> {
    // Copy over the assets
    // For debug builds, read assets from fs to speed up build
    // Otherwise, include as string literal

    #[cfg(not(debug_assertions))]
    let assets = {
        let search_js = include_str!("./static/search.js");
        let llms_txt = include_str!("./static/llms.txt");
        let styles_css = include_str!("./static/styles.css");
        let favicon_svg = include_str!("../../../www/public/favicon.svg");
        let raw_template_html = include_str!("./static/index.html");

        Assets {
            search_js,
            llms_txt,
            styles_css,
            favicon_svg,
            raw_template_html,
        }
    };

    #[cfg(debug_assertions)]
    let assets = {
        // Construct the absolute path to the static assets
        let workspace_dir = std::env!("ROC_WORKSPACE_DIR");
        let static_dir = Path::new(workspace_dir).join("crates/docs/src/static");

        // Read the assets from the filesystem
        let search_js = fs::read_to_string(static_dir.join("search.js")).unwrap();
        let llms_txt = fs::read_to_string(static_dir.join("llms.txt")).unwrap();
        let styles_css = fs::read_to_string(static_dir.join("styles.css")).unwrap();
        let favicon_svg =
            fs::read_to_string(static_dir.join("../../../../www/public/favicon.svg")).unwrap();
        let raw_template_html = fs::read_to_string(static_dir.join("index.html")).unwrap();

        Assets {
            search_js,
            llms_txt,
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

<<<<<<< HEAD
    let loaded_module = load_module_for_docs(root_file.as_ref().to_path_buf());
    let mut paths_by_shorthand = Vec::from_iter_in(iter::once((PathBuf::new(), root_file)), arena);

    // TODO add the other shorthands to paths_by_shorthand

    while let Some((path_prefix, path)) = paths_by_shorthand.pop() {
        Docs::new(
            arena,
            loaded_module,
            assets.raw_template_html.as_ref(),
            pkg_name,
            &path_prefix,
            opt_user_specified_base_url,
        )
        .generate(&out_dir)?;
    }

    Ok(())
||||||| parent of 7d06960495 (Sketch out doing package shorthands in docs)
    Docs::new(
        arena,
        loaded_module,
        assets.raw_template_html.as_ref(),
        pkg_name,
        opt_user_specified_base_url,
    )
    .generate(out_dir)
=======
    let loaded_module = load_module_for_docs(root_file.as_ref().to_path_buf());
    let mut paths_by_shorthand = Vec::from_iter_in(iter::once((PathBuf::new(), root_file)), arena);

    // TODO add the other shorthands to paths_by_shorthand

    while let Some((path_prefix, path)) = paths_by_shorthand.pop() {
        Docs::new(
            arena,
            &loaded_module,
            assets.raw_template_html.as_ref(),
            pkg_name,
            &path_prefix,
            opt_user_specified_base_url,
        )
        .generate(&out_dir)?;
    }

    Ok(())
>>>>>>> 7d06960495 (Sketch out doing package shorthands in docs)
}

struct Ability<'a> {
    name: &'a str,
    docs_url: &'a str,
}

struct BodyEntry<'a> {
    pub name: &'a str,
    pub type_vars_names: &'a [&'a str],
    pub type_annotation: Cow<'a, TypeAnnotation>,
    pub symbol: Symbol,
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
    scopes_by_module: Vec<'a, (ModuleId, &'a Scope)>,
    interns: &'a Interns,
    module_names: Vec<'a, (ModuleId, &'a str)>,
}

impl<'a> Docs<'a> {
    pub fn new(
        arena: &'a Bump,
        loaded_module: &'a LoadedModule,
        raw_template_html: &'a str,
        pkg_name: &'a str,
        path_prefix: &'a Path,
        opt_user_specified_base_url: Option<&'a str>,
    ) -> Self {
        let mut module_names =
            Vec::with_capacity_in(loaded_module.exposed_module_docs.len(), arena);
        let mut sb_entries = Vec::with_capacity_in(loaded_module.exposed_modules.len(), arena);
        let mut body_entries_by_module =
            Vec::with_capacity_in(loaded_module.exposed_modules.len(), arena);
        let mut scopes_by_module =
            Vec::with_capacity_in(loaded_module.exposed_modules.len(), arena);
        let header_doc_comment = arena.alloc_str(&loaded_module.header_doc_comment);

        for (module_id, docs) in loaded_module.exposed_module_docs.iter() {
            let opt_decls = loaded_module
                .typechecked
                .get(&module_id)
                .map(|checked| &checked.decls);

            module_names.push((*module_id, &*arena.alloc_str(&docs.name)));

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

            for entry in docs.entries.iter() {
                if let DocEntry::DocDef(def) = entry {
                    if docs.exposed_symbols.contains(&def.symbol) {
                        let type_annotation = if let TypeAnnotation::NoTypeAnn = def.type_annotation
                        {
                            // If the user didn't specify a type annotation, infer it!
                            let ann = opt_decls
                                .and_then(|decls| {
                                    decls
                                        .ann_from_symbol(def.symbol)
                                        .map(|result| match result {
                                            Ok(ann) => clean_annotation(type_to_ann(
                                                &ann.signature,
                                                &loaded_module.interns,
                                                &loaded_module.aliases,
                                                &mut MutMap::default(),
                                            )),
                                            Err(var) => {
                                                // It wasn't annotated at all; look up its inferred type in Subs!
                                                let subs = loaded_module
                                                    .typechecked
                                                    .get(&def.symbol.module_id()).unwrap_or_else(|| {
                                                        unreachable!("Got a loaded module with a symbol from a module that was not typechecked! module_id.is_builtin() = {:?}", def.symbol.module_id().is_builtin());
                                                    }).solved_subs.inner();

                                                clean_annotation(content_to_ann(
                                                    subs.get_content_without_compacting(var),
                                                    subs,
                                                    &loaded_module.interns,
                                                    &loaded_module.aliases,
                                                    &mut MutMap::default(),
                                                ))
                                            }
                                        })
                                })
                                .unwrap_or_else(|| {
                                    // We couldn't find it in decls, but it might have been a type alias.
                                    loaded_module
                                        .aliases
                                        .get(&def.symbol.module_id())
                                        .and_then(|aliases| {
                                            aliases.get(&def.symbol).map(|(_imported, alias)| {
                                                clean_annotation(type_to_ann(
                                                    &alias.typ,
                                                    &loaded_module.interns,
                                                    &loaded_module.aliases,
                                                    &mut MutMap::default(),
                                                ))
                                            })
                                        })
                                        .unwrap_or(TypeAnnotation::NoTypeAnn)
                                });

                            Cow::Owned(ann)
                        } else {
                            Cow::Borrowed(&def.type_annotation)
                        };

                        let type_vars_names = Vec::from_iter_in(
                            def.type_vars.iter().map(|s| &*arena.alloc_str(s)),
                            arena,
                        )
                        .into_bump_slice();

                        body_entries.push(BodyEntry {
                            name: &*arena.alloc_str(&def.name),
                            docs: def.docs.as_ref().map(|str| &*arena.alloc_str(&str)),
                            symbol: def.symbol,
                            type_annotation,
                            type_vars_names,
                        });
                    }
                }
            }

            body_entries_by_module.push((*module_id, body_entries.into_bump_slice()));
            scopes_by_module.push((*module_id, &docs.scope));
        }

        Self {
            interns: &loaded_module.interns,
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

        // Generate llms.txt
        file::write(arena, "llms.txt", self.llm_prompt())?;

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

    fn write_search_type_ahead(&self, buf: &mut String) {
        let arena = self.arena;

        for (module_id, module_name) in self.module_names.iter() {
            let entries = self
                .body_entries_by_module
                .iter()
                .find_map(|(id, entries)| {
                    if id == module_id {
                        Some(*entries)
                    } else {
                        None
                    }
                })
                .unwrap_or(&[]);

            for entry in entries.iter() {
                let mut entry_contents_buf = String::new_in(arena);

                push_html(
                    &mut entry_contents_buf,
                    "span",
                    [("class", "type-ahead-module-name")],
                    module_name,
                );

                push_html(
                    &mut entry_contents_buf,
                    "span",
                    [("class", "type-ahead-module-dot")],
                    ".",
                );

                push_html(
                    &mut entry_contents_buf,
                    "span",
                    [("class", "type-ahead-def-name")],
                    &entry.name,
                );

                let mut type_ann_buf = String::new_in(arena);
                type_annotation_to_html(0, &mut type_ann_buf, &entry.type_annotation, false);

                if !type_ann_buf.is_empty() {
                    push_html(
                        &mut entry_contents_buf,
                        "span",
                        [("class", "type-ahead-signature")],
                        format!(" : {type_ann_buf}"),
                    );
                }

                let mut entry_href = String::new_in(arena);

                entry_href.push_str(module_name);
                entry_href.push('#');
                entry_href.push_str(&entry.name);

                let mut anchor_buf = String::new_in(arena);

                push_html(
                    &mut anchor_buf,
                    "a",
                    [("href", entry_href.as_str()), ("class", "type-ahead-link")],
                    &entry_contents_buf,
                );

                push_html(buf, "li", [("role", "option")], &anchor_buf);
            }
        }
    }

    fn llm_prompt(&self) -> &'a str {
        let arena = self.arena;
        let mut example_type_question_buf = String::new_in(arena);
        let mut example_description_question_buf = String::new_in(arena);
        let mut buf = String::new_in(arena);
        buf.push_str(format!("# LLM Prompt for {}\n\n", self.pkg_name).as_str());
        buf.push_str("## Documentation\n\n");
        for (module_id, module_name) in self.module_names.iter() {
            buf.push_str(format!("### {}\n\n", module_name).as_str());

            let entries = self
                .body_entries_by_module
                .iter()
                .find_map(|(id, entries)| {
                    if id == module_id {
                        Some(*entries)
                    } else {
                        None
                    }
                })
                .unwrap_or(&[]);

            for entry in entries.iter() {
                let mut doc_def_buf = String::new_in(arena);
                doc_def_buf.push_str(format!("#### {}\n\n", entry.name).as_str());

                doc_def_buf.push_str("**Type Annotation**\n\n");
                let mut annotation_buf = String::new_in(arena);
                type_annotation_to_html(0, &mut annotation_buf, &entry.type_annotation, false);

                if !annotation_buf.is_empty() {
                    doc_def_buf.push_str("```roc\n");
                    doc_def_buf.push_str(format!("{}\n", annotation_buf).as_str());
                    doc_def_buf.push_str("```\n\n");
                }

                let mut description_buf = String::new_in(arena);
                if let Some(docs) = &entry.docs {
                    doc_def_buf.push_str("**Description**\n\n");
                    doc_def_buf.push_str(format!("{}\n", docs).as_str());
                    description_buf.push_str(docs);
                }

                buf.push_str(doc_def_buf.as_str());

                if example_type_question_buf.is_empty() && !annotation_buf.is_empty() {
                    example_type_question_buf.push_str("**Annotation Question Example**\n\n");
                    example_type_question_buf.push_str("**Question:**\n");
                    example_type_question_buf.push_str(
                        format!("What is the type definition for `{}`?\n\n", entry.name).as_str(),
                    );
                    example_type_question_buf.push_str("**Response:**\n");
                    example_type_question_buf.push_str(format!("{}\n\n", annotation_buf).as_str());
                    example_type_question_buf.push_str("**Source:**\n");
                    example_description_question_buf.push_str("```md\n");
                    example_type_question_buf.push_str(format!("{}\n", annotation_buf).as_str());
                    example_description_question_buf.push_str("```\n\n");
                }

                if example_description_question_buf.is_empty() && !description_buf.is_empty() {
                    example_description_question_buf
                        .push_str("**Description Question Example**\n\n");
                    example_description_question_buf.push_str("**Question:**\n");
                    example_description_question_buf
                        .push_str(format!("What does `{}` do?\n\n", entry.name).as_str());
                    example_description_question_buf.push_str("**Response:**\n");
                    example_description_question_buf
                        .push_str(format!("{}\n\n", description_buf).as_str());
                    example_description_question_buf.push_str("**Source:**\n");
                    example_description_question_buf.push_str("```md\n");
                    example_description_question_buf
                        .push_str(format!("{}\n", doc_def_buf).as_str());
                    example_description_question_buf.push_str("```\n\n");
                }
            }
        }

        buf.into_bump_str()
    }

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
                src = advance_past("<!-- Search Type Ahead -->", src, &mut buf);
                self.write_search_type_ahead(&mut buf);
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

                {
                    src = advance_past("<!-- Search Type Ahead -->", src, &mut buf);
                    self.write_search_type_ahead(&mut buf);
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
            let name = entry.name;

            let _ = write!(
                buf,
                "<section><h3 id='{name}' class='entry-name'><a href='{module_name}#{name}'>{name}</a>"
            );

            const TYPE_START: &str = " : ";

            push_punct(buf, TYPE_START);

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
}

/// After generating a type annotation, clean up its generated variables -
/// e.g. if we have a tag union with a visible unbound variable for its ext var,
/// drop that from the annotation so we render it as [Foo] instead of [Foo]*
fn clean_annotation(mut ann: TypeAnnotation) -> TypeAnnotation {
    let mut var_counts = MutMap::default();

    count_var_names(&ann, &mut var_counts);
    clean_annotation_help(&mut ann, &var_counts);

    ann
}

fn clean_annotation_help(ann: &mut TypeAnnotation, var_counts: &MutMap<std::string::String, u32>) {
    match ann {
        TypeAnnotation::TagUnion { tags, extension } => {
            for tag in tags.iter_mut() {
                for v in tag.values.iter_mut() {
                    clean_annotation_help(v, var_counts);
                }
            }

            match &mut **extension {
                TypeAnnotation::BoundVariable(ext_var) if var_counts.get(ext_var) == Some(&1) => {
                    // If the extension is an unbound var, don't render it.
                    *ext_var = std::string::String::new();
                }
                _ => {
                    clean_annotation_help(extension, var_counts);
                }
            }
        }
        TypeAnnotation::Function {
            args,
            arrow: _,
            output,
        } => {
            for arg in args.iter_mut() {
                clean_annotation_help(arg, var_counts);
            }

            clean_annotation_help(output, var_counts);
        }
        TypeAnnotation::ObscuredTagUnion => {}
        TypeAnnotation::ObscuredRecord => {}
        TypeAnnotation::BoundVariable(var_name) => {
            if var_counts.get(var_name) == Some(&1) {
                *var_name = "*".to_string();
            }
        }
        TypeAnnotation::Apply {
            name: _,
            parts,
            symbol: _,
        } => {
            for part in parts.iter_mut() {
                clean_annotation_help(part, var_counts);
            }
        }
        TypeAnnotation::Record { fields, extension } => {
            for field in fields.iter_mut() {
                match field {
                    RecordField::RecordField {
                        name: _,
                        type_annotation,
                    } => {
                        clean_annotation_help(type_annotation, var_counts);
                    }
                    RecordField::OptionalField {
                        name: _,
                        type_annotation,
                    } => {
                        clean_annotation_help(type_annotation, var_counts);
                    }
                    RecordField::LabelOnly { name: _ } => {}
                }
            }

            match &mut **extension {
                TypeAnnotation::BoundVariable(ext_var) if var_counts.get(ext_var) == Some(&1) => {
                    // If the extension is an unbound var, don't render it.
                    *ext_var = std::string::String::new();
                }
                _ => {
                    clean_annotation_help(extension, var_counts);
                }
            }
        }
        TypeAnnotation::Tuple { elems, extension } => {
            for elem in elems.iter_mut() {
                clean_annotation_help(elem, var_counts);
            }

            match &mut **extension {
                TypeAnnotation::BoundVariable(ext_var) if var_counts.get(ext_var) == Some(&1) => {
                    // If the extension is an unbound var, don't render it.
                    *ext_var = std::string::String::new();
                }
                _ => {
                    clean_annotation_help(extension, var_counts);
                }
            }
        }
        TypeAnnotation::Ability { members } => {
            for member in members.iter_mut() {
                clean_annotation_help(&mut member.type_annotation, var_counts);
                for (_, abilities) in member.able_variables.iter_mut() {
                    for ability in abilities.iter_mut() {
                        clean_annotation_help(ability, var_counts);
                    }
                }
            }
        }
        TypeAnnotation::Wildcard => {}
        TypeAnnotation::NoTypeAnn => {}
        TypeAnnotation::Where { ann, implements } => {
            clean_annotation_help(ann, var_counts);
            for implement in implements.iter_mut() {
                for ability in implement.abilities.iter_mut() {
                    clean_annotation_help(ability, var_counts);
                }
            }
        }
        TypeAnnotation::As {
            ann,
            symbol: _,
            vars: _,
        } => {
            clean_annotation_help(ann, var_counts);
        }
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
        TypeAnnotation::Apply { parts, symbol: _ } => {
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
            symbol: _,
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
            symbol: name,
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
            symbol,
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
            symbol: Symbol::NUM_NUM,
            parts: vec![TypeAnnotation::Variable("*")],
        },
        Type::Error => TypeAnnotation::NoTypeAnn,
        Type::Pure => TypeAnnotation::NoTypeAnn,
        Type::Effectful => TypeAnnotation::NoTypeAnn,
    }
}

fn content_to_ann(
    content: &Content,
    subs: &Subs,
    interns: &Interns,
    aliases: &MutMap<ModuleId, MutMap<Symbol, (bool, Alias)>>,
    var_names: &mut MutMap<Variable, std::string::String>,
) -> TypeAnnotation {
    match content {
        Content::FlexVar(opt_name) | Content::FlexAbleVar(opt_name, _) => {
            TypeAnnotation::BoundVariable(opt_name.map_or_else(
                || get_var_name(var_names),
                |index| subs.field_names[index.index()].to_string(),
            ))
        }
        Content::RigidVar(name) | Content::RigidAbleVar(name, _) => {
            TypeAnnotation::BoundVariable(subs.field_names[name.index()].to_string())
        }
        Content::RecursionVar { structure, .. } => content_to_ann(
            subs.get_content_without_compacting(*structure),
            subs,
            interns,
            aliases,
            var_names,
        ),
        Content::LambdaSet(_) | Content::ErasedLambda => TypeAnnotation::NoTypeAnn,
        Content::Structure(flat_type) => match flat_type {
            FlatType::Apply(symbol, args) => TypeAnnotation::Apply {
                name: symbol.as_str(interns).to_string(),
                parts: subs
                    .get_subs_slice(*args)
                    .iter()
                    .map(|&arg| {
                        content_to_ann(
                            subs.get_content_without_compacting(arg),
                            subs,
                            interns,
                            aliases,
                            var_names,
                        )
                    })
                    .collect(),
            },
            FlatType::Func(args, closure_var, ret, fx) => TypeAnnotation::Function {
                args: subs
                    .get_subs_slice(*args)
                    .iter()
                    .map(|&arg| {
                        content_to_ann(
                            subs.get_content_without_compacting(arg),
                            subs,
                            interns,
                            aliases,
                            var_names,
                        )
                    })
                    .collect(),
                arrow: if matches!(subs.get_content_without_compacting(*fx), Content::Effectful) {
                    FunctionArrow::Effectful
                } else {
                    FunctionArrow::Pure
                },
                output: Box::new(content_to_ann(
                    subs.get_content_without_compacting(*ret),
                    subs,
                    interns,
                    aliases,
                    var_names,
                )),
            },
            FlatType::Record(fields, ext) => TypeAnnotation::Record {
                fields: fields
                    .sorted_iterator(subs, *ext)
                    .map(|(name, field)| {
                        use roc_types::types::RecordField;

                        match field {
                            RecordField::Required(v)
                            | RecordField::Demanded(v)
                            | RecordField::RigidRequired(v) => {
                                roc_load::docs::RecordField::RecordField {
                                    name: name.to_string(),
                                    type_annotation: content_to_ann(
                                        subs.get_content_without_compacting(v),
                                        subs,
                                        interns,
                                        aliases,
                                        var_names,
                                    ),
                                }
                            }
                            RecordField::Optional(v) | RecordField::RigidOptional(v) => {
                                roc_load::docs::RecordField::OptionalField {
                                    name: name.to_string(),
                                    type_annotation: content_to_ann(
                                        subs.get_content_without_compacting(v),
                                        subs,
                                        interns,
                                        aliases,
                                        var_names,
                                    ),
                                }
                            }
                        }
                    })
                    .collect(),
                extension: Box::new(content_to_ann(
                    subs.get_content_without_compacting(*ext),
                    subs,
                    interns,
                    aliases,
                    var_names,
                )),
            },
            FlatType::Tuple(elems, ext) => TypeAnnotation::Tuple {
                elems: elems
                    .sorted_iterator(subs, *ext)
                    .map(|(_, elem)| {
                        content_to_ann(
                            subs.get_content_without_compacting(elem),
                            subs,
                            interns,
                            aliases,
                            var_names,
                        )
                    })
                    .collect(),
                extension: Box::new(content_to_ann(
                    subs.get_content_without_compacting(*ext),
                    subs,
                    interns,
                    aliases,
                    var_names,
                )),
            },
            FlatType::TagUnion(tags, ext) => TypeAnnotation::TagUnion {
                tags: tags
                    .unsorted_iterator(subs, *ext)
                    .map(|(name, vars)| Tag {
                        name: name.0.to_string(),
                        values: vars
                            .iter()
                            .map(|&var| {
                                content_to_ann(
                                    subs.get_content_without_compacting(var),
                                    subs,
                                    interns,
                                    aliases,
                                    var_names,
                                )
                            })
                            .collect(),
                    })
                    .collect(),
                extension: Box::new(content_to_ann(
                    subs.get_content_without_compacting(ext.var()),
                    subs,
                    interns,
                    aliases,
                    var_names,
                )),
            },
            FlatType::FunctionOrTagUnion(_, _, ext) => content_to_ann(
                subs.get_content_without_compacting(ext.var()),
                subs,
                interns,
                aliases,
                var_names,
            ),
            FlatType::RecursiveTagUnion(rec_var, tags, ext) => TypeAnnotation::TagUnion {
                tags: tags
                    .unsorted_iterator(subs, *ext)
                    .map(|(name, vars)| Tag {
                        name: name.0.to_string(),
                        values: vars
                            .iter()
                            .map(|&var| {
                                content_to_ann(
                                    subs.get_content_without_compacting(var),
                                    subs,
                                    interns,
                                    aliases,
                                    var_names,
                                )
                            })
                            .collect(),
                    })
                    .collect(),
                extension: Box::new(content_to_ann(
                    subs.get_content_without_compacting(ext.var()),
                    subs,
                    interns,
                    aliases,
                    var_names,
                )),
            },
            FlatType::EmptyRecord => TypeAnnotation::Record {
                fields: std::vec::Vec::new(),
                extension: Box::new(TypeAnnotation::NoTypeAnn),
            },
            FlatType::EmptyTagUnion => TypeAnnotation::TagUnion {
                tags: std::vec::Vec::new(),
                extension: Box::new(TypeAnnotation::NoTypeAnn),
            },
            FlatType::EffectfulFunc => TypeAnnotation::NoTypeAnn,
        },
        Content::Alias(_symbol, args, actual_var, kind) => {
            // Unwrap all type aliases. Docs should never show type aliases; they should always be followed!
            let todo_propagate_arg_names_to_annotation = args;
            let mut actual_content = subs.get_content_without_compacting(*actual_var);

            while let AliasKind::Structural = kind {
                match actual_content {
                    Content::Alias(_symbol, _args, actual_var, AliasKind::Structural) => {
                        actual_content = subs.get_content_without_compacting(*actual_var);
                    }
                    non_alias_content => {
                        actual_content = non_alias_content;
                        break;
                    }
                }
            }

            content_to_ann(actual_content, subs, interns, aliases, var_names)
        }
        Content::RangedNumber(_) => TypeAnnotation::Apply {
            name: "Num".to_string(),
            parts: std::vec::Vec::new(),
        },
        Content::Error => TypeAnnotation::NoTypeAnn,
        Content::Pure | Content::Effectful => TypeAnnotation::NoTypeAnn,
    }
}

fn get_var_name(var_names: &mut MutMap<Variable, std::string::String>) -> std::string::String {
    let vars_named = var_names.len();
    let letter = (b'a' + (vars_named % 26) as u8) as char;
    let number = (vars_named / 26) + 1;

    if number > 1 {
        format!("{}{}", letter, number)
    } else {
        letter.to_string()
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

// fn render_package_index(root_module: &LoadedModule) -> String {
//     // The list items containing module links
//     let mut module_list_buf = String::new_in(arena);

//     for (_, module) in root_module.docs_by_module.iter() {
//         // The anchor tag containing the module link
//         let mut link_buf = String::new_in(arena);

//         push_html(
//             &mut link_buf,
//             "a",
//             vec![("href", module.name.as_str())],
//             module.name.as_str(),
//         );

//         push_html(&mut module_list_buf, "li", vec![], link_buf.as_str());
//     }

//     // The HTML for the index page
//     let mut index_buf = String::new_in(arena);

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
//     let mut buf = String::new_in(arena);
//     let module_name = module.name.as_str();

//     push_html(&mut buf, "h2", vec![("class", "module-name")], {
//         let mut link_buf = String::new_in(arena);

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
//                     let mut content = String::new_in(arena);

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
//     let mut buf = String::new_in(arena);

//     push_html(&mut buf, "h1", vec![("class", "pkg-full-name")], {
//         let mut link_buf = String::new_in(arena);

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
//     let mut buf = String::new_in(arena);

//     for module in modules {
//         let href = module.name.as_str();
//         let mut sidebar_entry_content = String::new_in(arena);

//         push_html(
//             &mut sidebar_entry_content,
//             "a",
//             vec![("class", "sidebar-module-link"), ("href", href)],
//             module.name.as_str(),
//         );

//         let entries = {
//             let mut entries_buf = String::new_in(arena);

//             for entry in &module.entries {
//                 if let DocEntry::DocDef(doc_def) = entry {
//                     if module.exposed_symbols.contains(&doc_def.symbol) {
//                         let mut entry_href = String::new_in(arena);

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
            let mut extension = Cow::Borrowed(&**extension);
            let mut var = std::string::String::new();
            let mut extra_tags = std::vec::Vec::new();

            loop {
                match extension.borrow() {
                    TypeAnnotation::TagUnion {
                        tags: ext_tags,
                        extension: ext_extension,
                    } => {
                        extra_tags.extend_from_slice(&ext_tags);
                        extension = Cow::Owned((**ext_extension).clone());
                    }
                    TypeAnnotation::BoundVariable(var_name) => {
                        var = (*var_name).to_string();
                        break;
                    }
                    _ => break,
                }
            }

            if tags.is_empty() && extra_tags.is_empty() {
                push_punct(buf, "[]");
            } else {
                push_punct(buf, "[");

                if is_multiline {
                    new_line(buf);
                }

                let next_indent_level = indent_level + 1;

                for tag in tags.iter().chain(extra_tags.iter()) {
                    if is_multiline {
                        indent(buf, next_indent_level);
                    }

                    buf.push_str(tag.name.as_str());

                    for type_value in &tag.values {
                        buf.push(' ');
                        type_annotation_to_html(next_indent_level, buf, type_value, true);
                    }

                    if is_multiline {
                        push_punct(buf, ",");
                        new_line(buf);
                    }
                }

                if is_multiline {
                    indent(buf, indent_level);
                }

                push_punct(buf, "]");
            }

            buf.push_str(&var);
        }
        TypeAnnotation::BoundVariable(var_name) => {
            buf.push_str(var_name);
        }
        TypeAnnotation::Apply { name, parts } => {
            let todo = (); // TODO hack! Get this from a Symbol or something, and use the appropriate URL
            let is_builtin =
                name.as_str() == "Str" || name.as_str() == "List" || name.as_str() == "Num";

            if parts.is_empty() {
                // Don't print parens (regardless of needs_parens) if it's a standalone name.
                if is_builtin {
                    buf.push_str("<a href='https://example.com'>");
                }
                buf.push_str(name);
                if is_builtin {
                    buf.push_str("</a>");
                }
            } else {
                if needs_parens {
                    push_punct(buf, "(");
                }

                if is_builtin {
                    buf.push_str("<a href='https://example.com'>");
                }
                buf.push_str(name);
                if is_builtin {
                    buf.push_str("</a>");
                }
                for part in parts {
                    buf.push(' ');
                    type_annotation_to_html(indent_level, buf, part, true);
                }

                if needs_parens {
                    push_punct(buf, ")");
                }
            }
        }
        TypeAnnotation::Record { fields, extension } => {
            let mut extension = Cow::Borrowed(&**extension);
            let mut var = std::string::String::new();
            let mut extra_fields = std::vec::Vec::new();

            loop {
                match extension.borrow() {
                    TypeAnnotation::Record {
                        fields: ext_fields,
                        extension: ext_extension,
                    } => {
                        extra_fields.extend_from_slice(&ext_fields);
                        extension = Cow::Owned((**ext_extension).clone());
                    }
                    TypeAnnotation::BoundVariable(var_name) => {
                        var = (*var_name).to_string();
                        break;
                    }
                    _ => break,
                }
            }

            if fields.is_empty() && extra_fields.is_empty() {
                push_punct(buf, "{}");
            } else {
                push_punct(buf, "{");

                if is_multiline {
                    new_line(buf);
                }

                let next_indent_level = indent_level + 1;

                for field in fields.iter().chain(extra_fields.iter()) {
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
                            push_punct(buf, " : ");
                            type_annotation_to_html(next_indent_level, buf, type_annotation, false);
                        }
                        RecordField::OptionalField {
                            type_annotation, ..
                        } => {
                            push_punct(buf, " ? ");
                            type_annotation_to_html(next_indent_level, buf, type_annotation, false);
                        }
                        RecordField::LabelOnly { .. } => {}
                    }

                    if is_multiline {
                        push_punct(buf, ",");
                        new_line(buf);
                    }
                }

                if is_multiline {
                    indent(buf, indent_level);
                } else {
                    buf.push(' ');
                }

                push_punct(buf, "}");
            }

            buf.push_str(&var);
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
                    push_punct(buf, "(");
                    paren_is_open = true;
                }

                let child_needs_parens = matches!(arg, TypeAnnotation::Function { .. });
                type_annotation_to_html(indent_level, buf, arg, child_needs_parens);

                if peekable_args.peek().is_some() {
                    push_punct(buf, ", ");
                }
            }

            if is_multiline {
                new_line(buf);
                indent(buf, indent_level + 1);
            } else {
                buf.push(' ');
            }

            // Push either `->` or `=>` depending on whether it's a pure or effectful function.
            let arrow = match arrow {
                FunctionArrow::Pure => "->",
                FunctionArrow::Effectful => "=>",
            };

            push_punct(buf, arrow);
            buf.push(' ');

            let mut next_indent_level = indent_level;

            if should_be_multiline(output) {
                next_indent_level += 1;
            }

            type_annotation_to_html(next_indent_level, buf, output, false);
            if needs_parens && paren_is_open {
                push_punct(buf, ")");
            }
        }
        TypeAnnotation::Ability { members } => {
            push_docs_link(buf, keyword::IMPLEMENTS, ABILITIES_DOCS);

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
                push_punct(buf, " : ");

                type_annotation_to_html(indent_level + 1, buf, &member.type_annotation, false);

                if !member.able_variables.is_empty() {
                    new_line(buf);
                    indent(buf, indent_level + 2);

                    push_docs_link(buf, keyword::WHERE, ABILITIES_DOCS);

                    for (index, (name, type_anns)) in member.able_variables.iter().enumerate() {
                        if index != 0 {
                            push_punct(buf, ",");
                        }

                        buf.push(' ');
                        buf.push_str(name);
                        buf.push(' ');
                        push_docs_link(buf, keyword::IMPLEMENTS, ABILITIES_DOCS);

                        for (index, ann) in type_anns.iter().enumerate() {
                            if index != 0 {
                                push_punct(buf, " &");
                            }

                            buf.push(' ');

                            type_annotation_to_html(indent_level + 2, buf, ann, false);
                        }
                    }
                }
            }
        }
        TypeAnnotation::ObscuredTagUnion => {
            push_punct(buf, "[@..]");
        }
        TypeAnnotation::ObscuredRecord => {
            push_punct(buf, "{ @.. }");
        }
        TypeAnnotation::NoTypeAnn => {}
        TypeAnnotation::Wildcard => {
            push_punct(buf, "*");
        }
        TypeAnnotation::Tuple { elems, extension } => {
            let mut extension = Cow::Borrowed(&**extension);
            let mut var = std::string::String::new();
            let mut extra_elems = std::vec::Vec::new();

            loop {
                match extension.borrow() {
                    TypeAnnotation::Tuple {
                        elems: ext_elems,
                        extension: ext_extension,
                    } => {
                        extra_elems.extend_from_slice(&ext_elems);
                        extension = Cow::Owned((**ext_extension).clone());
                    }
                    TypeAnnotation::BoundVariable(var_name) => {
                        var = (*var_name).to_string();
                        break;
                    }
                    _ => break,
                }
            }

            if elems.is_empty() && extra_elems.is_empty() {
                push_punct(buf, "()");
            } else {
                let elems_len = elems.len() + extra_elems.len();

                push_punct(buf, "(");

                if is_multiline {
                    new_line(buf);
                }

                let next_indent_level = indent_level + 1;

                for (index, elem) in elems.iter().chain(extra_elems.iter()).enumerate() {
                    if is_multiline {
                        indent(buf, next_indent_level);
                    }

                    type_annotation_to_html(next_indent_level, buf, elem, false);

                    if is_multiline {
                        push_punct(buf, ",");
                        new_line(buf);
                    } else if index < (elems_len - 1) {
                        push_punct(buf, ", ");
                    }
                }

                if is_multiline {
                    indent(buf, indent_level);
                }

                push_punct(buf, ")");
            }

            buf.push_str(&var);
        }
        TypeAnnotation::Where { ann, implements } => {
            type_annotation_to_html(indent_level, buf, ann, false);

            new_line(buf);
            indent(buf, indent_level + 1);

            push_docs_link(buf, keyword::WHERE, ABILITIES_DOCS);

            let multiline_implements = implements
                .iter()
                .any(|imp| imp.abilities.iter().any(should_be_multiline));

            for (index, imp) in implements.iter().enumerate() {
                if index != 0 {
                    push_punct(buf, ",");
                }

                if multiline_implements {
                    new_line(buf);
                    indent(buf, indent_level + 2);
                } else {
                    buf.push(' ')
                }

                buf.push_str(&imp.name);
                buf.push(' ');
                push_docs_link(buf, keyword::IMPLEMENTS, ABILITIES_DOCS);
                buf.push(' ');

                for (index, ability) in imp.abilities.iter().enumerate() {
                    if index != 0 {
                        push_punct(buf, " & ");
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

fn push_html<'a, 'b, I>(buf: &mut String, tag_name: &str, attrs: I, content: impl AsRef<str>)
where
    I: IntoIterator<Item = (&'a str, &'b str)>,
{
    buf.push('<');
    buf.push_str(tag_name);
    buf.push(' ');

    for (key, value) in attrs.into_iter() {
        buf.push_str(key);
        buf.push_str("=\"");
        buf.push_str(value);
        buf.push('"');
        buf.push(' ');
    }

    buf.push('>');

    buf.push_str(content.as_ref());

    buf.push_str("</");
    buf.push_str(tag_name);
    buf.push('>');
}

fn push_punct(buf: &mut String, punctuation: &str) {
    buf.push_str("<span class='punct'>");
    buf.push_str(punctuation);
    buf.push_str("</span>");
}

fn push_docs_link(buf: &mut String, link_text: &str, docs_url: &str) {
    buf.push_str("<a href='");
    buf.push_str(docs_url);
    buf.push_str("'>");
    buf.push_str(link_text);
    buf.push_str("</a>");
}
