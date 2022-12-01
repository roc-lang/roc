//! Generates html documentation from Roc files. Used for
//! [roc-lang.org/builtins/Num](https://www.roc-lang.org/builtins/Num).
extern crate pulldown_cmark;
extern crate roc_load;
use bumpalo::Bump;
use docs_error::{DocsError, DocsResult};
use html::mark_node_to_html;
use roc_can::scope::Scope;
use roc_code_markup::markup::nodes::MarkupNode;
use roc_code_markup::slow_pool::SlowPool;
use roc_highlight::highlight_parser::{highlight_defs, highlight_expr};
use roc_load::docs::{DocEntry, TypeAnnotation};
use roc_load::docs::{ModuleDocumentation, RecordField};
use roc_load::{ExecutionMode, LoadConfig, LoadedModule, LoadingProblem, Threading};
use roc_module::symbol::{IdentIdsByModule, Interns, ModuleId};
use roc_packaging::cache::{self, RocCacheDir};
use roc_parse::ident::{parse_ident, Ident};
use roc_parse::state::State;
use roc_region::all::Region;
use std::fs;
use std::path::{Path, PathBuf};

mod docs_error;
mod html;

const BUILD_DIR: &str = "./generated-docs";

pub fn generate_docs_html(root_file: PathBuf) {
    let build_dir = Path::new(BUILD_DIR);
    let loaded_module = load_module_for_docs(root_file);

    // TODO get these from the platform's source file rather than hardcoding them!
    let package_name = "Documentation".to_string();
    let version = String::new();

    // Clear out the generated-docs dir (we'll create a fresh one at the end)
    if build_dir.exists() {
        fs::remove_dir_all(build_dir)
            .expect("TODO gracefully handle being unable to delete build dir");
    }
    fs::create_dir_all(build_dir).expect("TODO gracefully handle being unable to create build dir");

    // Copy over the assets
    fs::write(
        build_dir.join("search.js"),
        include_str!("./static/search.js"),
    )
    .expect("TODO gracefully handle failing to make the search javascript");

    fs::write(
        build_dir.join("styles.css"),
        include_str!("./static/styles.css"),
    )
    .expect("TODO gracefully handle failing to make the stylesheet");

    fs::write(
        build_dir.join("favicon.svg"),
        include_str!("./static/favicon.svg"),
    )
    .expect("TODO gracefully handle failing to make the favicon");

    let module_pairs = loaded_module
        .documentation
        .iter()
        .flat_map(|(module_id, module)| {
            let exposed_values = loaded_module
                .exposed_values
                .iter()
                .map(|symbol| symbol.as_str(&loaded_module.interns).to_string())
                .collect::<Vec<String>>();

            Some((module, exposed_values))
        });

    let template_html = include_str!("./static/index.html")
        .replace("<!-- search.js -->", "/search.js")
        .replace("<!-- styles.css -->", "/styles.css")
        .replace("<!-- favicon.svg -->", "/favicon.svg")
        .replace(
            "<!-- Prefetch links -->",
            &module_pairs
                .clone()
                .map(|(module, _)| {
                    let href = sidebar_link_url(module.name.as_str());

                    format!(r#"<link rel="prefetch" href="{href}"/>"#)
                })
                .collect::<Vec<String>>()
                .join("\n    "),
        )
        .replace(
            "<!-- Module links -->",
            render_sidebar(module_pairs).as_str(),
        );

    // Write each package's module docs html file
    for (module_id, module_docs) in loaded_module.documentation.iter() {
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
                "<!-- Package Name and Version -->",
                render_name_and_version(package_name.as_str(), version.as_str()).as_str(),
            )
            .replace(
                "<!-- Module Docs -->",
                render_module_documentation(module_docs, &loaded_module).as_str(),
            );

        fs::write(module_dir.join("index.html"), rendered_module)
            .expect("TODO gracefully handle failing to write index.html inside module's dir");
    }

    println!("🎉 Docs generated in {}", build_dir.display());
}

fn sidebar_link_url(module_name: &str) -> String {
    format!("{}{}", base_url(), module_name)
}

fn page_title(package_name: &str, module_name: &str) -> String {
    format!("<title>{module_name} - {package_name}</title>")
}

// converts plain-text code to highlighted html
pub fn syntax_highlight_expr(code_str: &str) -> DocsResult<String> {
    let trimmed_code_str = code_str.trim_end().trim();
    let mut mark_node_pool = SlowPool::default();

    let mut highlighted_html_str = String::new();

    match highlight_expr(trimmed_code_str, &mut mark_node_pool) {
        Ok(root_mark_node_id) => {
            let root_mark_node = mark_node_pool.get(root_mark_node_id);
            mark_node_to_html(root_mark_node, &mark_node_pool, &mut highlighted_html_str);

            Ok(highlighted_html_str)
        }
        Err(err) => Err(DocsError::from(err)),
    }
}

// converts plain-text code to highlighted html
pub fn syntax_highlight_top_level_defs(code_str: &str) -> DocsResult<String> {
    let trimmed_code_str = code_str.trim_end().trim();

    let mut mark_node_pool = SlowPool::default();

    let mut highlighted_html_str = String::new();

    match highlight_defs(trimmed_code_str, &mut mark_node_pool) {
        Ok(mark_node_id_vec) => {
            let def_mark_nodes: Vec<&MarkupNode> = mark_node_id_vec
                .iter()
                .map(|mn_id| mark_node_pool.get(*mn_id))
                .collect();

            for mn in def_mark_nodes {
                mark_node_to_html(mn, &mark_node_pool, &mut highlighted_html_str)
            }

            Ok(highlighted_html_str)
        }
        Err(err) => Err(DocsError::from(err)),
    }
}

fn render_module_documentation(
    module: &ModuleDocumentation,
    loaded_module: &LoadedModule,
) -> String {
    let mut buf = String::new();

    buf.push_str(
        html_to_string(
            "h2",
            vec![("class", "module-name")],
            html_to_string("a", vec![("href", "/#")], module.name.as_str()).as_str(),
        )
        .as_str(),
    );

    let exposed_values = loaded_module.exposed_values_str();
    let exposed_aliases = loaded_module.exposed_aliases_str();

    for entry in &module.entries {
        match entry {
            DocEntry::DocDef(doc_def) => {
                let name_str = doc_def.name.as_str();
                // We dont want to render entries that arent exposed
                let should_render_entry =
                    exposed_values.contains(&name_str) || exposed_aliases.contains(&name_str);

                if should_render_entry {
                    buf.push_str("<section>");

                    let name = doc_def.name.as_str();
                    let href = format!("#{name}");
                    let mut content = String::new();

                    content.push_str(
                        html_to_string("a", vec![("href", href.as_str())], name).as_str(),
                    );

                    for type_var in &doc_def.type_vars {
                        content.push(' ');
                        content.push_str(type_var.as_str());
                    }

                    let type_ann = &doc_def.type_annotation;

                    if !matches!(type_ann, TypeAnnotation::NoTypeAnn) {
                        content.push_str(" : ");
                        type_annotation_to_html(0, &mut content, type_ann, false);
                    }

                    buf.push_str(
                        html_to_string(
                            "h3",
                            vec![("id", name), ("class", "entry-name")],
                            content.as_str(),
                        )
                        .as_str(),
                    );

                    if let Some(docs) = &doc_def.docs {
                        buf.push_str(
                            markdown_to_html(
                                &exposed_values,
                                &module.scope,
                                docs.to_string(),
                                loaded_module,
                            )
                            .as_str(),
                        );
                    }

                    buf.push_str("</section>");
                }
            }
            DocEntry::DetachedDoc(docs) => {
                let markdown = markdown_to_html(
                    &exposed_values,
                    &module.scope,
                    docs.to_string(),
                    loaded_module,
                );
                buf.push_str(markdown.as_str());
            }
        };
    }

    buf
}

fn html_to_string(tag_name: &str, attrs: Vec<(&str, &str)>, content: &str) -> String {
    let mut buf = String::new();

    buf.push('<');
    buf.push_str(tag_name);

    for (key, value) in &attrs {
        buf.push(' ');
        buf.push_str(key);
        buf.push_str("=\"");
        buf.push_str(value);
        buf.push('"');
    }

    if !&attrs.is_empty() {
        buf.push(' ');
    }

    buf.push('>');

    buf.push_str(content);

    buf.push_str("</");
    buf.push_str(tag_name);
    buf.push('>');

    buf
}

fn base_url() -> String {
    // e.g. "builtins/" in "https://roc-lang.org/builtins/Str"
    //
    // TODO make this a CLI flag to the `docs` subcommand instead of an env var
    match std::env::var("ROC_DOCS_URL_ROOT") {
        Ok(root_builtins_path) => {
            let mut url_str = String::with_capacity(root_builtins_path.len() + 64);

            if !root_builtins_path.starts_with('/') {
                url_str.push('/');
            }

            url_str.push_str(&root_builtins_path);

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

fn render_name_and_version(name: &str, version: &str) -> String {
    let mut buf = String::new();
    let mut url_str = base_url();

    url_str.push_str(name);

    buf.push_str(
        html_to_string(
            "h1",
            vec![("class", "pkg-full-name")],
            html_to_string("a", vec![("href", url_str.as_str())], name).as_str(),
        )
        .as_str(),
    );

    let mut versions_url_str = base_url();

    versions_url_str.push('/');
    versions_url_str.push_str(name);
    versions_url_str.push('/');
    versions_url_str.push_str(version);

    buf.push_str(
        html_to_string(
            "a",
            vec![("class", "version"), ("href", versions_url_str.as_str())],
            version,
        )
        .as_str(),
    );

    buf
}

fn render_sidebar<'a, I: Iterator<Item = (&'a ModuleDocumentation, Vec<String>)>>(
    modules: I,
) -> String {
    let mut buf = String::new();

    for (module, exposed_values) in modules {
        let href = sidebar_link_url(module.name.as_str());
        let mut sidebar_entry_content = String::new();

        sidebar_entry_content.push_str(
            html_to_string(
                "a",
                vec![("class", "sidebar-module-link"), ("href", &href)],
                module.name.as_str(),
            )
            .as_str(),
        );

        let entries = {
            let mut entries_buf = String::new();

            for entry in &module.entries {
                if let DocEntry::DocDef(doc_def) = entry {
                    if exposed_values.contains(&doc_def.name) {
                        let mut entry_href = String::new();

                        entry_href.push_str(href.as_str());
                        entry_href.push('#');
                        entry_href.push_str(doc_def.name.as_str());

                        entries_buf.push_str(
                            html_to_string(
                                "a",
                                vec![("href", entry_href.as_str())],
                                doc_def.name.as_str(),
                            )
                            .as_str(),
                        );
                    }
                }
            }

            entries_buf
        };

        sidebar_entry_content.push_str(
            html_to_string(
                "div",
                vec![("class", "sidebar-sub-entries")],
                entries.as_str(),
            )
            .as_str(),
        );

        buf.push_str(
            html_to_string(
                "div",
                vec![("class", "sidebar-entry")],
                sidebar_entry_content.as_str(),
            )
            .as_str(),
        );
    }

    buf
}

pub fn load_module_for_docs(filename: PathBuf) -> LoadedModule {
    let arena = Bump::new();
    let load_config = LoadConfig {
        target_info: roc_target::TargetInfo::default_x86_64(), // This is just type-checking for docs, so "target" doesn't matter
        render: roc_reporting::report::RenderTarget::ColorTerminal,
        palette: roc_reporting::report::DEFAULT_PALETTE,
        threading: Threading::AllAvailable,
        exec_mode: ExecutionMode::Check,
    };
    match roc_load::load_and_typecheck(
        &arena,
        filename,
        Default::default(),
        RocCacheDir::Persistent(cache::roc_cache_dir().as_path()),
        load_config,
    ) {
        Ok(loaded) => loaded,
        Err(LoadingProblem::FormattedReport(report)) => {
            eprintln!("{}", report);
            std::process::exit(1);
        }
        Err(e) => panic!("{:?}", e),
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
        TypeAnnotation::Function { args, output } => {
            let mut peekable_args = args.iter().peekable();
            while let Some(arg) = peekable_args.next() {
                if is_multiline {
                    if !should_be_multiline(arg) {
                        new_line(buf);
                    }
                    indent(buf, indent_level + 1);
                }

                type_annotation_to_html(indent_level, buf, arg, false);

                if peekable_args.peek().is_some() {
                    buf.push_str(", ");
                }
            }

            if is_multiline {
                new_line(buf);
                indent(buf, indent_level + 1);
            }

            buf.push_str(" -> ");

            let mut next_indent_level = indent_level;

            if should_be_multiline(output) {
                next_indent_level += 1;
            }

            type_annotation_to_html(next_indent_level, buf, output, false);
        }
        TypeAnnotation::Ability { members: _ } => {
            // TODO(abilities): fill me in
        }
        TypeAnnotation::ObscuredTagUnion => {
            buf.push_str("[@..]");
        }
        TypeAnnotation::ObscuredRecord => {
            buf.push_str("{ @.. }");
        }
        TypeAnnotation::NoTypeAnn => {}
        TypeAnnotation::Wildcard => buf.push('*'),
    }
}

fn should_be_multiline(type_ann: &TypeAnnotation) -> bool {
    match type_ann {
        TypeAnnotation::TagUnion { tags, extension } => {
            let mut is_multiline = should_be_multiline(extension) || tags.len() > 1;

            for tag in tags {
                for value in &tag.values {
                    if is_multiline {
                        break;
                    }
                    is_multiline = should_be_multiline(value);
                }
            }

            is_multiline
        }
        TypeAnnotation::Function { args, output } => {
            let mut is_multiline = should_be_multiline(output) || args.len() > 2;

            for arg in args {
                if is_multiline {
                    break;
                }

                is_multiline = should_be_multiline(arg);
            }

            is_multiline
        }
        TypeAnnotation::ObscuredTagUnion => false,
        TypeAnnotation::ObscuredRecord => false,
        TypeAnnotation::BoundVariable(_) => false,
        TypeAnnotation::Apply { parts, .. } => {
            let mut is_multiline = false;

            for part in parts {
                is_multiline = should_be_multiline(part);

                if is_multiline {
                    break;
                }
            }

            is_multiline
        }
        TypeAnnotation::Record { fields, extension } => {
            let mut is_multiline = should_be_multiline(extension) || fields.len() > 1;

            for field in fields {
                if is_multiline {
                    break;
                }
                match field {
                    RecordField::RecordField {
                        type_annotation, ..
                    } => is_multiline = should_be_multiline(type_annotation),
                    RecordField::OptionalField {
                        type_annotation, ..
                    } => is_multiline = should_be_multiline(type_annotation),
                    RecordField::LabelOnly { .. } => {}
                }
            }

            is_multiline
        }
        TypeAnnotation::Ability { .. } => true,
        TypeAnnotation::Wildcard => false,
        TypeAnnotation::NoTypeAnn => false,
    }
}

struct DocUrl {
    url: String,
    title: String,
}

fn doc_url<'a>(
    home: ModuleId,
    exposed_values: &[&str],
    dep_idents: &IdentIdsByModule,
    scope: &Scope,
    interns: &'a Interns,
    mut module_name: &'a str,
    ident: &str,
) -> DocUrl {
    if module_name.is_empty() {
        // This is an unqualified lookup, so look for the ident
        // in scope!
        match scope.lookup_str(ident, Region::zero()) {
            Ok(symbol) => {
                // Get the exact module_name from scope. It could be the
                // current module's name, but it also could be a different
                // module - for example, if this is in scope from an
                // unqualified import.
                module_name = symbol.module_string(interns);
            }
            Err(_) => {
                // TODO return Err here
                panic!(
                "Tried to generate an automatic link in docs for symbol `{}`, but that symbol was not in scope in this module.",
                ident
            );
            }
        }
    } else {
        match interns.module_ids.get_id(&module_name.into()) {
            Some(module_id) => {
                // You can do qualified lookups on your own module, e.g.
                // if I'm in the Foo module, I can do a `Foo.bar` lookup.
                if module_id == home {
                    // Check to see if the value is exposed in this module.
                    // If it's not exposed, then we can't link to it!
                    if !exposed_values.contains(&ident) {
                        // TODO return Err here
                        panic!(
                            "Tried to generate an automatic link in docs for `{}.{}`, but `{}` does not expose `{}`.",
                            module_name, ident, module_name, ident);
                    }
                } else {
                    // This is not the home module
                    match dep_idents
                        .get(&module_id)
                        .and_then(|exposed_ids| exposed_ids.get_id(ident))
                    {
                        Some(_) => {
                            // This is a valid symbol for this dependency,
                            // so proceed using the current module's name.
                            //
                            // TODO: In the future, this is where we'll
                            // incorporate the package name into the link.
                        }
                        _ => {
                            // TODO return Err here
                            panic!(
                                "Tried to generate an automatic link in docs for `{}.{}`, but `{}` is not exposed in `{}`.",
                                module_name, ident, ident, module_name);
                        }
                    }
                }
            }
            None => {
                // TODO return Err here
                panic!("Tried to generate a doc link for `{}.{}` but the `{}` module was not imported!", module_name, ident, module_name);
            }
        }
    }

    let mut url = base_url();

    // Example:
    //
    // module_name: "Str", ident: "join" => "/Str#join"
    url.push_str(module_name);
    url.push('#');
    url.push_str(ident);

    DocUrl {
        url,
        title: format!("Docs for {}.{}", module_name, ident),
    }
}

fn markdown_to_html(
    exposed_values: &[&str],
    scope: &Scope,
    markdown: String,
    loaded_module: &LoadedModule,
) -> String {
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
                    Ok((_, Ident::Access { module_name, parts }, _)) => {
                        let mut iter = parts.iter();

                        match iter.next() {
                            Some(symbol_name) if iter.next().is_none() => {
                                let DocUrl { url, title } = doc_url(
                                    loaded_module.module_id,
                                    exposed_values,
                                    &loaded_module.dep_idents,
                                    scope,
                                    &loaded_module.interns,
                                    module_name,
                                    symbol_name,
                                );

                                Some((url.into(), title.into()))
                            }
                            _ => {
                                // This had record field access,
                                // e.g. [foo.bar] - which we
                                // can't create a doc link to!
                                None
                            }
                        }
                    }
                    Ok((_, Ident::Tag(type_name), _)) => {
                        // This looks like a tag name, but it could
                        // be a type alias that's in scope, e.g. [I64]
                        let DocUrl { url, title } = doc_url(
                            loaded_module.module_id,
                            exposed_values,
                            &loaded_module.dep_idents,
                            scope,
                            &loaded_module.interns,
                            "",
                            type_name,
                        );

                        Some((url.into(), title.into()))
                    }
                    _ => None,
                }
            }
            _ => None,
        }
    };

    let markdown_options = pulldown_cmark::Options::ENABLE_TABLES;

    let mut expecting_code_block = false;

    let mut docs_parser = vec![];
    let (_, _) = pulldown_cmark::Parser::new_with_broken_link_callback(
        &markdown,
        markdown_options,
        Some(&mut broken_link_callback),
    )
    .fold((0, 0), |(start_quote_count, end_quote_count), event| {

        match &event {
            // Replace this sequence (`>>>` syntax):
            //     Start(BlockQuote)
            //     Start(BlockQuote)
            //     Start(BlockQuote)
            //     Start(Paragraph)
            // For `Start(CodeBlock(Fenced(Borrowed("roc"))))`
            Event::Start(BlockQuote) => {
                docs_parser.push(event);
                (start_quote_count + 1, 0)
            }
            Event::Start(Paragraph) => {
                if start_quote_count == 3 {
                    docs_parser.pop();
                    docs_parser.pop();
                    docs_parser.pop();
                    docs_parser.push(Event::Start(CodeBlock(CodeBlockKind::Fenced(
                        CowStr::Borrowed("roc"),
                    ))));
                } else {
                    docs_parser.push(event);
                }
                (0, 0)
            }
            // Replace this sequence (`>>>` syntax):
            //     End(Paragraph)
            //     End(BlockQuote)
            //     End(BlockQuote)
            //     End(BlockQuote)
            // For `End(CodeBlock(Fenced(Borrowed("roc"))))`
            Event::End(Paragraph) => {
                docs_parser.push(event);
                (0, 1)
            }
            Event::End(BlockQuote) => {
                if end_quote_count == 3 {
                    docs_parser.pop();
                    docs_parser.pop();
                    docs_parser.pop();
                    docs_parser.push(Event::End(CodeBlock(CodeBlockKind::Fenced(
                        CowStr::Borrowed("roc"),
                    ))));
                    (0, 0)
                } else {
                    docs_parser.push(event);
                    (0, end_quote_count + 1)
                }
            }
            Event::End(Link(LinkType::ShortcutUnknown, _url, _title)) => {
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

                (start_quote_count, end_quote_count)
            }
            Event::Start(CodeBlock(CodeBlockKind::Fenced(_))) => {
                expecting_code_block = true;
                docs_parser.push(event);
                (0, 0)
            }
            Event::End(CodeBlock(_)) => {
                expecting_code_block = false;
                docs_parser.push(event);
                (0, 0)
            }
            Event::Text(CowStr::Borrowed(code_str)) if expecting_code_block => {

                match syntax_highlight_expr(
                    code_str
                )
                {
                    Ok(highlighted_code_str) => {
                        docs_parser.push(Event::Html(CowStr::from(highlighted_code_str)));
                    }
                    Err(syntax_error) => {
                        panic!("Unexpected parse failure when parsing this for rendering in docs:\n\n{}\n\nParse error was:\n\n{:?}\n\n", code_str, syntax_error)
                    }
                };

                (0, 0)
            }
            _ => {
                docs_parser.push(event);
                (0, 0)
            }
        }
    });

    let mut docs_html = String::new();

    pulldown_cmark::html::push_html(&mut docs_html, docs_parser.into_iter());

    docs_html
}
