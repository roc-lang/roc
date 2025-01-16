//! Generates html documentation from Roc files. Used for
//! [roc-lang.org/builtins/Num](https://www.roc-lang.org/builtins/Num).
extern crate pulldown_cmark;
extern crate roc_load;
use bumpalo::Bump;
use roc_can::scope::Scope;
use roc_collections::VecSet;
use roc_highlight::highlight_roc_code_inline;
use roc_load::docs::{DocEntry, TypeAnnotation};
use roc_load::docs::{ModuleDocumentation, RecordField};
use roc_load::{ExecutionMode, LoadConfig, LoadedModule, LoadingProblem, Threading};
use roc_module::symbol::{Interns, ModuleId, Symbol};
use roc_packaging::cache::{self, RocCacheDir};
use roc_parse::ast::FunctionArrow;
use roc_parse::ident::{parse_ident, Accessor, Ident};
use roc_parse::keyword;
use roc_parse::state::State;
use roc_problem::Severity;
use roc_region::all::Region;
use std::fs;
use std::path::{Path, PathBuf};

const LINK_SVG: &str = include_str!("./static/link.svg");

pub fn generate_docs_html(root_file: PathBuf, build_dir: &Path, maybe_root_dir: Option<String>) {
    let mut loaded_module = load_module_for_docs(root_file);
    let exposed_module_docs = get_exposed_module_docs(&mut loaded_module);

    // TODO get these from the platform's source file rather than hardcoding them!
    // github.com/roc-lang/roc/issues/5712
    let package_name = "Documentation".to_string();

    // Clear out the generated-docs dir (we'll create a fresh one at the end)
    if build_dir.exists() {
        fs::remove_dir_all(build_dir)
            .expect("TODO gracefully handle being unable to delete build dir");
    }
    fs::create_dir_all(build_dir).expect("TODO gracefully handle being unable to create build dir");

    // Copy over the assets
    // For debug builds, read assets from fs to speed up build
    // Otherwise, include as string literal

    struct Assets<S: AsRef<str>> {
        search_js: S,
        llms_txt: S,
        styles_css: S,
        raw_template_html: S,
    }

    #[cfg(not(debug_assertions))]
    let assets = {
        let search_js = include_str!("./static/search.js");
        let llms_txt = include_str!("./static/llms.txt");
        let styles_css = include_str!("./static/styles.css");
        let raw_template_html = include_str!("./static/index.html");

        Assets {
            search_js,
            llms_txt,
            styles_css,
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
        let raw_template_html = fs::read_to_string(static_dir.join("index.html")).unwrap();

        Assets {
            search_js,
            llms_txt,
            styles_css,
            raw_template_html,
        }
    };

    // Write CSS, JS, and favicon
    // (The HTML requires more work!)
    for (file, contents) in [
        ("search.js", assets.search_js),
        ("llms.txt", assets.llms_txt),
        ("styles.css", assets.styles_css),
    ] {
        let dir = build_dir.join(file);
        fs::write(&dir, contents).unwrap_or_else(|error| {
            panic!(
                "Attempted to write {} but failed with this error: {}",
                dir.display(),
                error
            )
        })
    }

    // Insert asset urls & sidebar links
    let template_html = assets
        .raw_template_html
        .replace(
            "<!-- Prefetch links -->",
            exposed_module_docs
                .iter()
                .map(|(_, module)| {
                    let module_href = module.name.replace('.', "/");
                    let href = module_href.as_str();

                    format!(r#"<link rel="prefetch" href="{href}"/>"#)
                })
                .collect::<Vec<String>>()
                .join("\n    ")
                .as_str(),
        )
        .replace("<!-- base -->", &base_url(maybe_root_dir.as_deref()))
        .replace(
            "<!-- Module links -->",
            render_sidebar(exposed_module_docs.iter().map(|(_, docs)| docs)).as_str(),
        )
        .replace(
            "<!-- Search Type Ahead -->",
            render_search_type_ahead(exposed_module_docs.iter().map(|(_, docs)| docs)).as_str(),
        );

    {
        let llms_txt = llm_prompt(
            package_name.as_str(),
            exposed_module_docs.iter().map(|(_, docs)| docs),
        );
        fs::write(build_dir.join("llms.txt"), llms_txt)
            .expect("TODO gracefully handle failing to write llms.txt");
    }

    let all_exposed_symbols = {
        let mut set = VecSet::default();

        for (_, docs) in exposed_module_docs.iter() {
            set.insert_all(docs.exposed_symbols.iter().copied());
        }

        set
    };

    // TODO fix: as is, this overrides an existing index.html
    // Write index.html for package (/index.html)
    {
        let rendered_package = template_html
            .replace(
                "<!-- Page title -->",
                page_title(package_name.as_str(), "").as_str(),
            )
            .replace(
                "<!-- Package Name -->",
                render_name_link(package_name.as_str(), maybe_root_dir.as_deref()).as_str(),
            )
            .replace("<!-- Package Name String -->", package_name.as_str())
            .replace(
                "<!-- Module Docs -->",
                render_package_index(&exposed_module_docs).as_str(),
            );

        fs::write(build_dir.join("index.html"), rendered_package).unwrap_or_else(|error| {
            panic!("Attempted to write index.html but failed with this error: {error}")
        });
    }

    // Write each package module's index.html file
    for (module_id, module_docs) in exposed_module_docs.iter() {
        let module_name = module_docs.name.as_str();
        let module_dir = build_dir.join(module_name.replace('.', "/").as_str());

        fs::create_dir_all(&module_dir)
            .expect("TODO gracefully handle not being able to create the module dir");

        let rendered_module = template_html
            .replace(
                "<!-- Page title -->",
                page_title(package_name.as_str(), module_name).as_str(),
            )
            .replace(
                "<!-- Package Name -->",
                render_name_link(package_name.as_str(), maybe_root_dir.as_deref()).as_str(),
            )
            .replace("<!-- Package Name String -->", package_name.as_str())
            .replace(
                "<!-- Module Docs -->",
                render_module_documentation(
                    *module_id,
                    module_docs,
                    &loaded_module,
                    &all_exposed_symbols,
                    maybe_root_dir.as_deref(),
                )
                .as_str(),
            );

        fs::write(module_dir.join("index.html"), rendered_module)
            .expect("TODO gracefully handle failing to write index.html inside module's dir");
    }

    println!("🎉 Docs generated in {}", build_dir.display());
}

/// Gives only the module docs for modules that are exposed by the platform or package.
fn get_exposed_module_docs(
    loaded_module: &mut LoadedModule,
) -> Vec<(ModuleId, ModuleDocumentation)> {
    let mut exposed_docs = Vec::with_capacity(loaded_module.exposed_modules.len());
    // let mut docs_by_module = Vec::with_capacity(state.exposed_modules.len());

    for module_id in loaded_module.exposed_modules.iter() {
        let docs =
            loaded_module.docs_by_module.remove(module_id).unwrap_or_else(|| {
                panic!("A module was exposed but didn't have an entry in `documentation` somehow: {module_id:?}");
            });

        exposed_docs.push(docs);
    }
    exposed_docs
}

fn page_title(package_name: &str, module_name: &str) -> String {
    format!("<title>{module_name} - {package_name}</title>")
}

fn render_package_index(docs_by_module: &[(ModuleId, ModuleDocumentation)]) -> String {
    // The list items containing module links
    let mut module_list_buf = String::new();

    for (_, module) in docs_by_module.iter() {
        // The anchor tag containing the module link
        let mut link_buf = String::new();

        push_html(
            &mut link_buf,
            "a",
            [("href", module.name.replace('.', "/").as_str())],
            module.name.as_str(),
        );

        push_html(&mut module_list_buf, "li", [], link_buf.as_str());
    }

    let header = {
        let mut header_buf = String::new();

        push_html(
            &mut header_buf,
            "h2",
            [("class", "module-name")],
            "Exposed Modules",
        );

        push_html(
            &mut header_buf,
            "a",
            [
                ("class", "llm-prompt-link"),
                ("title", "Documentation in a LLM-friendly format"),
                ("href", "llms.txt"),
            ],
            "LLM docs",
        );

        header_buf
    };

    // The HTML for the index page
    let mut index_buf = String::new();

    push_html(
        &mut index_buf,
        "div",
        [("class", "module-header-container")],
        &header,
    );

    push_html(
        &mut index_buf,
        "ul",
        [("class", "index-module-links")],
        &module_list_buf,
    );

    index_buf
}

fn render_module_documentation(
    module_id: ModuleId,
    module: &ModuleDocumentation,
    root_module: &LoadedModule,
    all_exposed_symbols: &VecSet<Symbol>,
    maybe_root_dir: Option<&str>,
) -> String {
    let mut buf = String::new();
    let module_name = module.name.as_str();

    push_html(&mut buf, "h2", [("class", "module-name")], {
        let mut link_buf = String::new();

        push_html(&mut link_buf, "a", [("href", "/")], module_name);

        link_buf
    });

    for entry in &module.entries {
        match entry {
            DocEntry::DocDef(doc_def) => {
                // Only render entries that are exposed
                if all_exposed_symbols.contains(&doc_def.symbol) {
                    buf.push_str("<section>");

                    let def_name = doc_def.name.as_str();
                    let href = format!("{module_name}#{def_name}");
                    let mut content = String::new();
                    let mut anno_buf = String::new();

                    push_html(&mut content, "a", [("href", href.as_str())], LINK_SVG);
                    // push_html(&mut content, "strong", [], def_name);
                    anno_buf.push_str(def_name);

                    for type_var in &doc_def.type_vars {
                        anno_buf.push(' ');
                        anno_buf.push_str(type_var.as_str());
                    }

                    let type_ann = &doc_def.type_annotation;

                    if !matches!(type_ann, TypeAnnotation::NoTypeAnn) {
                        // Ability declarations don't have ":" after the name, just `implements`
                        if !matches!(type_ann, TypeAnnotation::Ability { .. }) {
                            anno_buf.push_str(" :");
                        }

                        anno_buf.push(' ');

                        type_annotation_to_html(0, &mut anno_buf, type_ann, false);
                    }
                    content.push_str(highlight_roc_code_inline(anno_buf.as_str()).as_str());

                    push_html(
                        &mut buf,
                        "h3",
                        [("id", def_name), ("class", "entry-name")],
                        content.as_str(),
                    );

                    if let Some(docs) = &doc_def.docs {
                        markdown_to_html(
                            &mut buf,
                            &root_module.filename(module_id),
                            all_exposed_symbols,
                            &module.scope,
                            docs,
                            root_module,
                            maybe_root_dir,
                        );
                    }

                    buf.push_str("</section>");
                }
            }
            DocEntry::ModuleDoc(docs) => {
                markdown_to_html(
                    &mut buf,
                    &root_module.filename(module_id),
                    all_exposed_symbols,
                    &module.scope,
                    docs,
                    root_module,
                    maybe_root_dir,
                );
            }
            DocEntry::DetachedDoc(docs) => {
                markdown_to_html(
                    &mut buf,
                    &root_module.filename,
                    all_exposed_symbols,
                    &module.scope,
                    docs,
                    root_module,
                    maybe_root_dir,
                );
            }
        };
    }

    buf
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

fn base_url(maybe_root_dir: Option<&str>) -> String {
    match maybe_root_dir {
        Some(root_builtins_path) => {
            let mut url_str = String::with_capacity(root_builtins_path.len() + 64);

            if !root_builtins_path.starts_with('/') {
                url_str.push('/');
            }

            url_str.push_str(root_builtins_path);

            if !root_builtins_path.ends_with('/') {
                url_str.push('/');
            }

            url_str
        }
        _ => {
            let mut url_str = String::with_capacity(64);

            url_str.push('/');

            url_str
        }
    }
}

// TODO render version as well
fn render_name_link(name: &str, maybe_root_dir: Option<&str>) -> String {
    let mut buf = String::new();

    push_html(&mut buf, "h1", [("class", "pkg-full-name")], {
        let mut link_buf = String::new();

        // link to root (= docs overview page)
        push_html(
            &mut link_buf,
            "a",
            [("href", base_url(maybe_root_dir).as_str())],
            name,
        );

        link_buf
    });

    buf
}

fn render_sidebar<'a, I: Iterator<Item = &'a ModuleDocumentation>>(modules: I) -> String {
    let mut buf = String::new();

    for module in modules {
        let module_href = module.name.replace('.', "/");
        let href = module_href.as_str();
        let mut sidebar_entry_content = String::new();
        let mut module_link_content = String::new();

        push_html(&mut module_link_content, "span", [], module.name.as_str());

        push_html(
            &mut module_link_content,
            "button",
            [("class", "entry-toggle")],
            "▶",
        );

        push_html(
            &mut sidebar_entry_content,
            "a",
            [
                ("class", "sidebar-module-link"),
                ("href", href),
                ("data-module-name", module.name.as_str()),
            ],
            module_link_content.as_str(),
        );

        let entries = {
            let mut entries_buf = String::new();

            for entry in &module.entries {
                if let DocEntry::DocDef(doc_def) = entry {
                    if module.exposed_symbols.contains(&doc_def.symbol) {
                        let mut entry_href = String::new();

                        entry_href.push_str(href);
                        entry_href.push('#');
                        entry_href.push_str(doc_def.name.as_str());

                        push_html(
                            &mut entries_buf,
                            "a",
                            [("href", entry_href.as_str())],
                            doc_def.name.as_str(),
                        );
                    }
                }
            }

            entries_buf
        };

        push_html(
            &mut sidebar_entry_content,
            "div",
            [("class", "sidebar-sub-entries")],
            entries.as_str(),
        );

        push_html(
            &mut buf,
            "div",
            [("class", "sidebar-entry")],
            sidebar_entry_content.as_str(),
        );
    }

    buf
}

fn render_search_type_ahead<'a, I: Iterator<Item = &'a ModuleDocumentation>>(modules: I) -> String {
    let mut buf = String::new();
    for module in modules {
        let module_name = module.name.as_str();
        for entry in &module.entries {
            if let DocEntry::DocDef(doc_def) = entry {
                if module.exposed_symbols.contains(&doc_def.symbol) {
                    let mut entry_contents_buf = String::new();

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
                        &doc_def.name,
                    );

                    let mut type_ann_buf = String::new();
                    type_annotation_to_html(0, &mut type_ann_buf, &doc_def.type_annotation, false);

                    if !type_ann_buf.is_empty() {
                        push_html(
                            &mut entry_contents_buf,
                            "span",
                            [("class", "type-ahead-signature")],
                            format!(" : {type_ann_buf}"),
                        );
                    }

                    let mut entry_href = String::new();

                    entry_href.push_str(&module_name.replace('.', "/"));
                    entry_href.push('#');
                    entry_href.push_str(&doc_def.name);

                    let mut anchor_buf = String::new();

                    push_html(
                        &mut anchor_buf,
                        "a",
                        [("href", entry_href.as_str()), ("class", "type-ahead-link")],
                        &entry_contents_buf,
                    );

                    push_html(&mut buf, "li", [("role", "option")], &anchor_buf);
                }
            }
        }
    }

    buf
}

fn llm_prompt<'a, I: Iterator<Item = &'a ModuleDocumentation>>(
    package_name: &str,
    modules: I,
) -> String {
    let mut example_type_question_buf = String::new();
    let mut example_description_question_buf = String::new();
    let mut buf = String::new();
    buf.push_str(format!("# LLM Prompt for {}\n\n", package_name).as_str());
    buf.push_str("## Documentation\n\n");
    for module in modules {
        let module_name = module.name.as_str();
        buf.push_str(format!("### {}\n\n", module_name).as_str());
        for entry in &module.entries {
            if let DocEntry::DocDef(doc_def) = entry {
                if module.exposed_symbols.contains(&doc_def.symbol) {
                    let mut doc_def_buf = String::new();
                    doc_def_buf.push_str(format!("#### {}\n\n", doc_def.name).as_str());

                    doc_def_buf.push_str("**Type Annotation**\n\n");
                    let mut annotation_buf = String::new();
                    type_annotation_to_html(
                        0,
                        &mut annotation_buf,
                        &doc_def.type_annotation,
                        false,
                    );

                    if !annotation_buf.is_empty() {
                        doc_def_buf.push_str("```roc\n");
                        doc_def_buf.push_str(format!("{}\n", annotation_buf).as_str());
                        doc_def_buf.push_str("```\n\n");
                    }

                    let mut description_buf = String::new();
                    if let Some(docs) = &doc_def.docs {
                        doc_def_buf.push_str("**Description**\n\n");
                        doc_def_buf.push_str(format!("{}\n", docs).as_str());
                        description_buf.push_str(docs.as_str());
                    }

                    buf.push_str(doc_def_buf.as_str());

                    if example_type_question_buf.is_empty() && !annotation_buf.is_empty() {
                        example_type_question_buf.push_str("**Annotation Question Example**\n\n");
                        example_type_question_buf.push_str("**Question:**\n");
                        example_type_question_buf.push_str(
                            format!("What is the type definition for `{}`?\n\n", doc_def.name)
                                .as_str(),
                        );
                        example_type_question_buf.push_str("**Response:**\n");
                        example_type_question_buf
                            .push_str(format!("{}\n\n", annotation_buf).as_str());
                        example_type_question_buf.push_str("**Source:**\n");
                        example_description_question_buf.push_str("```md\n");
                        example_type_question_buf
                            .push_str(format!("{}\n", annotation_buf).as_str());
                        example_description_question_buf.push_str("```\n\n");
                    }

                    if example_description_question_buf.is_empty() && !description_buf.is_empty() {
                        example_description_question_buf
                            .push_str("**Description Question Example**\n\n");
                        example_description_question_buf.push_str("**Question:**\n");
                        example_description_question_buf
                            .push_str(format!("What does `{}` do?\n\n", doc_def.name).as_str());
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
        }
    }
    buf
}

pub fn load_module_for_docs(filename: PathBuf) -> LoadedModule {
    let arena = Bump::new();
    let load_config = LoadConfig {
        target: roc_target::Target::LinuxX64, // This is just type-checking for docs, so "target" doesn't matter
        function_kind: roc_solve::FunctionKind::LambdaSet,
        render: roc_reporting::report::RenderTarget::ColorTerminal,
        palette: roc_reporting::report::DEFAULT_PALETTE,
        threading: Threading::AllAvailable,
        exec_mode: ExecutionMode::Check,
    };
    match roc_load::load_and_typecheck(
        &arena,
        filename,
        None,
        RocCacheDir::Persistent(cache::roc_cache_packages_dir().as_path()),
        load_config,
    ) {
        Ok(loaded) => loaded,
        Err(LoadingProblem::FormattedReport(report, _)) => {
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

// html is written to buf
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
            arrow,
            output,
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

            match arrow {
                FunctionArrow::Effectful => buf.push_str("=> "),
                FunctionArrow::Pure => buf.push_str("-> "),
            }

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
            arrow: _,
            output,
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

struct DocUrl {
    url: String,
    title: String,
}

enum LinkProblem {
    MalformedAutoLink,
    AutoLinkIdentNotInScope,
    AutoLinkNotExposed,
    AutoLinkModuleNotImported,
}

fn doc_url<'a>(
    all_exposed_symbols: &VecSet<Symbol>,
    scope: &Scope,
    interns: &'a Interns,
    mut module_name: &'a str,
    ident: &str,
    maybe_root_dir: Option<&str>,
) -> Result<DocUrl, (String, LinkProblem)> {
    if module_name.is_empty() {
        // This is an unqualified lookup, so look for the ident
        // in scope!
        match scope.lookup_str(ident, Region::zero()) {
            Ok(symbol) => {
                // Get the exact module_name from scope. It could be the
                // current module's name, but it also could be a different
                // module - for example, if this is in scope from an
                // unqualified import.
                module_name = symbol.symbol.module_string(interns);
            }
            Err(_) => {
                return Err((format!("[{ident}]"), LinkProblem::AutoLinkIdentNotInScope));
            }
        }
    } else {
        match interns.module_ids.get_id(&module_name.into()) {
            Some(module_id) => {
                let symbol = interns.symbol(module_id, ident.into());

                if symbol.is_builtin() {
                    // We can always generate links for builtin modules.
                    // TODO add a `--include-builtins` CLI flag for generating offline docs locally
                    // which include builtins; if that flag is omitted, have this code path generate
                    // a link directly to the builtin docs on roc-lang.org instead of to a localhost
                    // URL that will 404.
                    module_name = symbol.module_string(interns);
                }
                // Note: You can do qualified lookups on your own module, e.g.
                // if I'm in the Foo module, I can do a `Foo.bar` lookup.
                else if !all_exposed_symbols.contains(&symbol) {
                    return Err((
                        format!("[{module_name}.{ident}]"),
                        LinkProblem::AutoLinkNotExposed,
                    ));
                }

                // This is a valid symbol for this dependency,
                // so proceed using the current module's name.
                //
                // TODO: In the future, this is where we'll
                // incorporate the package name into the link.
            }
            None => {
                return Err((
                    format!("[{module_name}.{ident}]"),
                    LinkProblem::AutoLinkModuleNotImported,
                ));
            }
        }
    }

    let mut url = base_url(maybe_root_dir);

    // Example:
    //
    // module_name: "Str", ident: "join" => "/Str#join"
    url.push_str(module_name);
    url.push('#');
    url.push_str(ident);

    Ok(DocUrl {
        url,
        title: format!("Docs for {module_name}.{ident}"),
    })
}

fn markdown_to_html(
    buf: &mut String,
    filename: &Path,
    all_exposed_symbols: &VecSet<Symbol>,
    scope: &Scope,
    markdown: &str,
    loaded_module: &LoadedModule,
    maybe_root_dir: Option<&str>,
) {
    use pulldown_cmark::{BrokenLink, CodeBlockKind, CowStr, Event, LinkType, Tag::*};

    let mut arena = Bump::new();
    let mut broken_link_callback = |link: BrokenLink| {
        // A shortcut link - see https://spec.commonmark.org/0.30/#shortcut-reference-link -
        // is something like `[foo]` in markdown. If you have a shortcut link
        // without a corresponding `[foo]: https://foo.com` entry
        // at the end of the document, we resolve it as an identifier based on
        // what's currently in scope, so you write things like [Str.join] or
        // [myFunction] and have them resolve to the docs for what you wrote.
        match link.link_type {
            LinkType::Shortcut => {
                let state = State::new(link.reference.as_bytes());

                // Reset the bump arena so we aren't constantly reallocating
                // more memory as we iterate through these.
                arena.reset();

                match parse_ident(&arena, state, 0) {
                    Ok((
                        _,
                        Ident::Access {
                            module_name, parts, ..
                        },
                        _,
                    )) => {
                        let mut iter = parts.iter();

                        match iter.next() {
                            Some(Accessor::RecordField(symbol_name)) if iter.next().is_none() => {
                                match doc_url(
                                    all_exposed_symbols,
                                    scope,
                                    &loaded_module.interns,
                                    module_name,
                                    symbol_name,
                                    maybe_root_dir,
                                ) {
                                    Ok(DocUrl { url, title }) => Some((url.into(), title.into())),
                                    Err((link_markdown, problem)) => {
                                        report_markdown_link_problem(
                                            loaded_module.module_id,
                                            filename.to_path_buf(),
                                            &link_markdown,
                                            problem,
                                        );

                                        None
                                    }
                                }
                            }
                            _ => {
                                report_markdown_link_problem(
                                    loaded_module.module_id,
                                    filename.to_path_buf(),
                                    &format!("[{}]", link.reference),
                                    LinkProblem::MalformedAutoLink,
                                );
                                None
                            }
                        }
                    }
                    Ok((_, Ident::Tag(type_name), _)) => {
                        // This looks like a tag name, but it could
                        // be a type alias that's in scope, e.g. [I64]
                        match doc_url(
                            all_exposed_symbols,
                            scope,
                            &loaded_module.interns,
                            "",
                            type_name,
                            maybe_root_dir,
                        ) {
                            Ok(DocUrl { url, title }) => Some((url.into(), title.into())),
                            Err((link_markdown, problem)) => {
                                report_markdown_link_problem(
                                    loaded_module.module_id,
                                    filename.to_path_buf(),
                                    &link_markdown,
                                    problem,
                                );

                                None
                            }
                        }
                    }
                    _ => {
                        report_markdown_link_problem(
                            loaded_module.module_id,
                            filename.to_path_buf(),
                            &format!("[{}]", link.reference),
                            LinkProblem::MalformedAutoLink,
                        );

                        None
                    }
                }
            }
            _ => None,
        }
    };

    let markdown_options =
        pulldown_cmark::Options::ENABLE_TABLES | pulldown_cmark::Options::ENABLE_HEADING_ATTRIBUTES;

    let mut in_code_block: Option<CowStr> = None;
    let mut to_highlight = String::new();

    let mut docs_parser = Vec::new();
    let parser = pulldown_cmark::Parser::new_with_broken_link_callback(
        markdown,
        markdown_options,
        Some(&mut broken_link_callback),
    );

    for event in parser {
        match event {
            Event::Code(code_str) => {
                let inline_code = pulldown_cmark::CowStr::from(format!("<code>{code_str}</code>"));
                docs_parser.push(pulldown_cmark::Event::Html(inline_code));
            }
            Event::End(Link(LinkType::ShortcutUnknown, ref _url, ref _title)) => {
                // Replace the preceding Text node with a Code node, so it
                // renders as the equivalent of [`List.len`] instead of [List.len]
                match docs_parser.pop() {
                    Some(Event::Text(string)) => {
                        docs_parser.push(Event::Code(string));
                    }
                    Some(first) => {
                        docs_parser.push(first);
                    }
                    None => {}
                }

                docs_parser.push(event);
            }
            Event::Start(CodeBlock(CodeBlockKind::Fenced(code_str))) => {
                in_code_block = Some(code_str);
            }
            Event::End(CodeBlock(_)) => {
                match in_code_block {
                    Some(code_str) => {
                        if code_str.contains("unchecked") {
                            // TODO HANDLE UNCHECKED
                        }

                        if code_str.contains("repl") {
                            // TODO HANDLE REPL
                        }

                        // TODO HANDLE CHECKING BY DEFAULT
                        let highlighted_html = roc_highlight::highlight_roc_code(&to_highlight);
                        docs_parser.push(Event::Html(CowStr::from(highlighted_html)));
                    }
                    None => {
                        // Indented code block

                        let highlighted_html = roc_highlight::highlight_roc_code(&to_highlight);
                        docs_parser.push(Event::Html(CowStr::from(highlighted_html)));
                    }
                }

                // Reset codeblock buffer
                to_highlight = String::new();
                in_code_block = None;

                // Push Event::End(CodeBlock)
                docs_parser.push(event);
            }
            Event::Text(t) => {
                match in_code_block {
                    Some(_) => {
                        // If we're in a code block, build up the string of text
                        to_highlight.push_str(&t);
                    }
                    None => {
                        docs_parser.push(Event::Text(t));
                    }
                }
            }
            Event::Html(html) => {
                docs_parser.push(Event::Text(html));
            }
            e => {
                docs_parser.push(e);
            }
        }
    }

    pulldown_cmark::html::push_html(buf, docs_parser.into_iter());
}

/// TODO: this should be moved into Reporting, and the markdown checking
/// for docs should be part of `roc check`. Problems like these should
/// be reported as `roc check` warnings and included in the total count
/// of warnings at the end.
fn report_markdown_link_problem(
    module_id: ModuleId,
    filename: PathBuf,
    link_markdown: &str,
    problem: LinkProblem,
) {
    use roc_reporting::report::{Report, RocDocAllocator, DEFAULT_PALETTE};
    use ven_pretty::DocAllocator;

    // Report parsing and canonicalization problems
    let interns = Interns::default();
    let alloc = RocDocAllocator::new(&[], module_id, &interns);

    let report = {
        const AUTO_LINK_TIP: &str = "Tip: When a link in square brackets doesn't have a URL immediately after it in parentheses, the part in square brackets needs to be the name of either an uppercase type in scope, or a lowercase value in scope. Then Roc will generate a link to its docs, if available.";

        let link_problem = match problem {
            LinkProblem::MalformedAutoLink => alloc.stack([
                alloc.reflow("The part in square brackets is not a Roc type or value name that can be automatically linked to."),
                alloc.reflow(AUTO_LINK_TIP),
            ]),
            LinkProblem::AutoLinkIdentNotInScope => alloc.stack([
                alloc.reflow("The name in square brackets was not found in scope."),
                alloc.reflow(AUTO_LINK_TIP),
            ]),
            LinkProblem::AutoLinkNotExposed => alloc.stack([
                alloc.reflow("The name in square brackets is not exposed by the module where it's defined."),
                alloc.reflow(AUTO_LINK_TIP),
            ]),
            LinkProblem::AutoLinkModuleNotImported => alloc.stack([
                alloc.reflow("The name in square brackets is not in scope because its module is not imported."),
                alloc.reflow(AUTO_LINK_TIP),
            ])
        };

        let doc = alloc.stack([
            alloc.reflow("This link in a doc comment is invalid:"),
            alloc.reflow(link_markdown).indent(4),
            link_problem,
        ]);

        Report {
            filename,
            doc,
            title: "INVALID DOCS LINK".to_string(),
            severity: Severity::Warning,
        }
    };

    let palette = DEFAULT_PALETTE;
    let mut buf = String::new();

    report.render_color_terminal(&mut buf, &alloc, &palette);
}
