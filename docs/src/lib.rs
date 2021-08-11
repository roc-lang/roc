extern crate pulldown_cmark;
extern crate roc_load;
use bumpalo::Bump;
use roc_builtins::std::StdLib;
use roc_can::builtins::builtin_defs_map;
use roc_can::scope::Scope;
use roc_collections::all::MutMap;
use roc_load::docs::DocEntry::DocDef;
use roc_load::docs::{DocEntry, TypeAnnotation};
use roc_load::docs::{ModuleDocumentation, RecordField};
use roc_load::file::{LoadedModule, LoadingProblem};
use roc_module::symbol::{IdentIds, Interns, ModuleId};
use roc_parse::ident::{parse_ident, Ident};
use roc_parse::parser::State;
use roc_region::all::Region;
use std::fs;
use std::path::{Path, PathBuf};

pub fn generate(filenames: Vec<PathBuf>, std_lib: StdLib, build_dir: &Path) {
    let files_docs = files_to_documentations(filenames, std_lib);
    let mut arena = Bump::new();

    //
    // TODO: get info from a file like "elm.json"
    let mut package = roc_load::docs::Documentation {
        name: "roc/builtins".to_string(),
        version: "1.0.0".to_string(),
        docs: "Package introduction or README.".to_string(),
        modules: files_docs,
    };

    if !build_dir.exists() {
        fs::create_dir_all(build_dir).expect("TODO gracefully handle unable to create build dir");
    }

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

    let template_html = include_str!("./static/index.html")
        .replace("<!-- search.js -->", &format!("{}search.js", base_href()))
        .replace("<!-- styles.css -->", &format!("{}styles.css", base_href()))
        .replace(
            "<!-- favicon.svg -->",
            &format!("{}favicon.svg", base_href()),
        )
        .replace(
            "<!-- Module links -->",
            render_sidebar(package.modules.iter().flat_map(|loaded_module| {
                loaded_module.documentation.values().map(move |d| {
                    let exposed_values = loaded_module
                        .exposed_values
                        .iter()
                        .map(|symbol| symbol.ident_str(&loaded_module.interns).to_string())
                        .collect::<Vec<String>>();

                    (exposed_values, d)
                })
            }))
            .as_str(),
        );

    // Write each package's module docs html file
    for loaded_module in package.modules.iter_mut() {
        arena.reset();

        let mut exports: bumpalo::collections::Vec<&str> =
            bumpalo::collections::Vec::with_capacity_in(loaded_module.exposed_values.len(), &arena);

        // TODO should this also include exposed_aliases?
        for symbol in loaded_module.exposed_values.iter() {
            exports.push(symbol.ident_str(&loaded_module.interns));
        }

        let exports = exports.into_bump_slice();

        for module in loaded_module.documentation.values_mut() {
            let module_dir = build_dir.join(module.name.replace(".", "/").as_str());

            fs::create_dir_all(&module_dir)
                .expect("TODO gracefully handle not being able to create the module dir");

            let rendered_module = template_html
                .replace(
                    "<!-- Package Name and Version -->",
                    render_name_and_version(package.name.as_str(), package.version.as_str())
                        .as_str(),
                )
                .replace(
                    "<!-- Module Docs -->",
                    render_main_content(
                        loaded_module.module_id,
                        exports,
                        &loaded_module.dep_idents,
                        &loaded_module.interns,
                        module,
                    )
                    .as_str(),
                );

            fs::write(module_dir.join("index.html"), rendered_module)
                .expect("TODO gracefully handle failing to write html");
        }
    }

    println!("🎉 Docs generated in {}", build_dir.display());
}

fn render_main_content(
    home: ModuleId,
    exposed_values: &[&str],
    dep_idents: &MutMap<ModuleId, IdentIds>,
    interns: &Interns,
    module: &mut ModuleDocumentation,
) -> String {
    let mut buf = String::new();

    buf.push_str(
        html_node(
            "h2",
            vec![("class", "module-name")],
            html_node("a", vec![("href", "/#")], module.name.as_str()).as_str(),
        )
        .as_str(),
    );

    for entry in &module.entries {
        let mut should_render_entry = true;

        if let DocDef(def) = entry {
            // We dont want to render entries that arent exposed
            should_render_entry = exposed_values.contains(&def.name.as_str());
        }

        if should_render_entry {
            match entry {
                DocEntry::DocDef(doc_def) => {
                    let mut href = String::new();
                    href.push('#');
                    href.push_str(doc_def.name.as_str());

                    let name = doc_def.name.as_str();

                    let mut content = String::new();

                    content.push_str(html_node("a", vec![("href", href.as_str())], name).as_str());

                    for type_var in &doc_def.type_vars {
                        content.push(' ');
                        content.push_str(type_var.as_str());
                    }

                    let type_ann = &doc_def.type_annotation;

                    match type_ann {
                        TypeAnnotation::NoTypeAnn => {}
                        _ => {
                            content.push_str(" : ");
                        }
                    }

                    type_annotation_to_html(0, &mut content, type_ann);

                    buf.push_str(
                        html_node(
                            "h3",
                            vec![("id", name), ("class", "entry-name")],
                            content.as_str(),
                        )
                        .as_str(),
                    );

                    if let Some(docs) = &doc_def.docs {
                        buf.push_str(
                            markdown_to_html(
                                home,
                                exposed_values,
                                dep_idents,
                                &mut module.scope,
                                interns,
                                docs.to_string(),
                            )
                            .as_str(),
                        );
                    }
                }
                DocEntry::DetachedDoc(docs) => {
                    let markdown = markdown_to_html(
                        home,
                        exposed_values,
                        dep_idents,
                        &mut module.scope,
                        interns,
                        docs.to_string(),
                    );
                    buf.push_str(markdown.as_str());
                }
            };
        }
    }

    buf
}

fn html_node(tag_name: &str, attrs: Vec<(&str, &str)>, content: &str) -> String {
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

fn base_href() -> String {
    // e.g. "builtins/" in "https://roc-lang.org/builtins/Str"
    //
    // TODO make this a CLI flag to the `docs` subcommand instead of an env var
    match std::env::var("ROC_DOCS_URL_ROOT") {
        Ok(root_builtins_path) => {
            let mut href = String::with_capacity(root_builtins_path.len() + 64);

            if !root_builtins_path.starts_with('/') {
                href.push('/');
            }

            href.push_str(&root_builtins_path);

            if !root_builtins_path.ends_with('/') {
                href.push('/');
            }

            href
        }
        _ => {
            let mut href = String::with_capacity(64);

            href.push('/');

            href
        }
    }
}

fn render_name_and_version(name: &str, version: &str) -> String {
    let mut buf = String::new();
    let mut href = base_href();

    href.push_str(name);

    buf.push_str(
        html_node(
            "h1",
            vec![("class", "pkg-full-name")],
            html_node("a", vec![("href", href.as_str())], name).as_str(),
        )
        .as_str(),
    );

    let mut versions_href = base_href();

    versions_href.push('/');
    versions_href.push_str(name);
    versions_href.push('/');
    versions_href.push_str(version);

    buf.push_str(
        html_node(
            "a",
            vec![("class", "version"), ("href", versions_href.as_str())],
            version,
        )
        .as_str(),
    );

    buf
}

fn render_sidebar<'a, I: Iterator<Item = (Vec<String>, &'a ModuleDocumentation)>>(
    modules: I,
) -> String {
    let mut buf = String::new();

    for (exposed_values, module) in modules {
        let mut sidebar_entry_content = String::new();

        let name = module.name.as_str();

        let href = {
            let mut href_buf = base_href();
            href_buf.push_str(name);
            href_buf
        };

        sidebar_entry_content.push_str(
            html_node(
                "a",
                vec![("class", "sidebar-module-link"), ("href", href.as_str())],
                name,
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
                            html_node(
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
            html_node(
                "div",
                vec![("class", "sidebar-sub-entries")],
                entries.as_str(),
            )
            .as_str(),
        );

        buf.push_str(
            html_node(
                "div",
                vec![("class", "sidebar-entry")],
                sidebar_entry_content.as_str(),
            )
            .as_str(),
        );
    }

    buf
}

pub fn files_to_documentations(filenames: Vec<PathBuf>, std_lib: StdLib) -> Vec<LoadedModule> {
    let arena = Bump::new();
    let mut files_docs = vec![];

    for filename in filenames {
        let mut src_dir = filename.clone();
        src_dir.pop();

        match roc_load::file::load_and_typecheck(
            &arena,
            filename,
            &std_lib,
            src_dir.as_path(),
            MutMap::default(),
            std::mem::size_of::<usize>() as u32, // This is just type-checking for docs, so "target" doesn't matter
            builtin_defs_map,
        ) {
            Ok(loaded) => files_docs.push(loaded),
            Err(LoadingProblem::FormattedReport(report)) => {
                println!("{}", report);
                panic!();
            }
            Err(e) => panic!("{:?}", e),
        }
    }

    files_docs
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

fn type_annotation_to_html(indent_level: usize, buf: &mut String, type_ann: &TypeAnnotation) {
    let is_multiline = should_be_multiline(type_ann);
    match type_ann {
        TypeAnnotation::TagUnion { tags, extension } => {
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
                } else {
                    buf.push(' ');
                }

                buf.push_str(tag.name.as_str());

                for type_value in &tag.values {
                    buf.push(' ');
                    type_annotation_to_html(next_indent_level, buf, type_value);
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
            } else {
                buf.push(' ');
            }

            buf.push(']');

            type_annotation_to_html(indent_level, buf, extension);
        }
        TypeAnnotation::BoundVariable(var_name) => {
            buf.push_str(var_name);
        }
        TypeAnnotation::Apply { name, parts } => {
            if parts.is_empty() {
                buf.push_str(name);
            } else {
                buf.push('(');
                buf.push_str(name);
                for part in parts {
                    buf.push(' ');
                    type_annotation_to_html(indent_level, buf, part);
                }
                buf.push(')');
            }
        }
        TypeAnnotation::Record { fields, extension } => {
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
                        type_annotation_to_html(next_indent_level, buf, type_annotation);
                    }
                    RecordField::OptionalField {
                        type_annotation, ..
                    } => {
                        buf.push_str(" ? ");
                        type_annotation_to_html(next_indent_level, buf, type_annotation);
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

            type_annotation_to_html(indent_level, buf, extension);
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

                type_annotation_to_html(indent_level, buf, arg);

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

            type_annotation_to_html(next_indent_level, buf, output);
        }
        TypeAnnotation::ObscuredTagUnion => {
            buf.push_str("[ @.. ]");
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
    dep_idents: &MutMap<ModuleId, IdentIds>,
    scope: &mut Scope,
    interns: &'a Interns,
    mut module_name: &'a str,
    ident: &str,
) -> DocUrl {
    if module_name.is_empty() {
        // This is an unqualified lookup, so look for the ident
        // in scope!
        match scope.lookup(&ident.into(), Region::zero()) {
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
                "Tried to generate an automatic link in docs for symbol `{}`, but that symbol was not in scope in this module. Scope was: {:?}",
                ident, scope
            );
            }
        }
    } else {
        match interns.module_ids.get_id(&module_name.into()) {
            Some(&module_id) => {
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
                        .and_then(|exposed_ids| exposed_ids.get_id(&ident.into()))
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

    let mut url = base_href();

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
    home: ModuleId,
    exposed_values: &[&str],
    dep_idents: &MutMap<ModuleId, IdentIds>,
    scope: &mut Scope,
    interns: &Interns,
    markdown: String,
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

                match parse_ident(&arena, state) {
                    Ok((_, Ident::Access { module_name, parts }, _)) => {
                        let mut iter = parts.iter();

                        match iter.next() {
                            Some(symbol_name) if iter.next().is_none() => {
                                let DocUrl { url, title } = doc_url(
                                    home,
                                    exposed_values,
                                    dep_idents,
                                    scope,
                                    interns,
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
                    Ok((_, Ident::GlobalTag(type_name), _)) => {
                        // This looks like a global tag name, but it could
                        // be a type alias that's in scope, e.g. [I64]
                        let DocUrl { url, title } = doc_url(
                            home,
                            exposed_values,
                            dep_idents,
                            scope,
                            interns,
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

    let markdown_options = pulldown_cmark::Options::empty();
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
