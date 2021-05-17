extern crate pulldown_cmark;
use roc_builtins::std::StdLib;
use roc_can::builtins::builtin_defs_map;
use roc_load::docs::{DocEntry, TypeAnnotation};
use roc_load::docs::{ModuleDocumentation, RecordField};
use roc_load::file::LoadingProblem;

use std::fs;
extern crate roc_load;
use bumpalo::Bump;
use roc_collections::all::MutMap;
use std::path::{Path, PathBuf};

pub fn generate(filenames: Vec<PathBuf>, std_lib: StdLib, build_dir: &Path) {
    let files_docs = files_to_documentations(filenames, std_lib);
    //
    // TODO: get info from a file like "elm.json"
    let package = roc_load::docs::Documentation {
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

    let template_html = include_str!("./static/index.html");

    // Write each package's module docs html file
    for module in &package.modules {
        let mut filename = String::new();
        filename.push_str(module.name.as_str());
        filename.push_str(".html");

        let rendered_module = template_html
            .replace(
                "<!-- Module links -->",
                render_module_links(&package.modules).as_str(),
            )
            .replace(
                "<!-- Package Name and Version -->",
                render_name_and_version(package.name.as_str(), package.version.as_str()).as_str(),
            )
            .replace(
                "<!-- Module Docs -->",
                render_main_content(&module).as_str(),
            );

        fs::write(build_dir.join(filename), rendered_module)
            .expect("TODO gracefully handle failing to write html");
    }

    println!("🎉 Docs generated in {}", build_dir.display());
}

fn render_main_content(module: &ModuleDocumentation) -> String {
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

                if let Some(type_ann) = &doc_def.type_annotation {
                    content.push_str(" : ");
                    type_annotation_to_html(0, &mut content, &type_ann);
                }

                buf.push_str(
                    html_node(
                        "h3",
                        vec![("id", name), ("class", "entry-name")],
                        content.as_str(),
                    )
                    .as_str(),
                );

                if let Some(docs) = &doc_def.docs {
                    buf.push_str(markdown_to_html(docs.to_string()).as_str());
                }
            }
            DocEntry::DetatchedDoc(docs) => {
                buf.push_str(markdown_to_html(docs.to_string()).as_str());
            }
        };
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

fn render_name_and_version(name: &str, version: &str) -> String {
    let mut buf = String::new();

    let mut href = String::new();
    href.push('/');
    href.push_str(name);

    buf.push_str(
        html_node(
            "h1",
            vec![("class", "pkg-full-name")],
            html_node("a", vec![("href", href.as_str())], name).as_str(),
        )
        .as_str(),
    );

    let mut verions_href = String::new();

    verions_href.push('/');
    verions_href.push_str(name);
    verions_href.push('/');
    verions_href.push_str(version);

    buf.push_str(
        html_node(
            "a",
            vec![("class", "version"), ("href", verions_href.as_str())],
            version,
        )
        .as_str(),
    );

    buf
}

fn render_module_links(modules: &[ModuleDocumentation]) -> String {
    let mut buf = String::new();

    for module in modules {
        let mut sidebar_entry_content = String::new();

        let name = module.name.as_str();

        let href = {
            let mut href_buf = String::new();
            href_buf.push_str(name);
            href_buf.push_str(".html");
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

pub fn files_to_documentations(
    filenames: Vec<PathBuf>,
    std_lib: StdLib,
) -> Vec<ModuleDocumentation> {
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
            8, // TODO: Is it okay to hardcode ptr_bytes here? I think it should be fine since we'er only type checking (also, 8 => 32bit system)
            builtin_defs_map,
        ) {
            Ok(mut loaded) => files_docs.extend(loaded.documentation.drain().map(|x| x.1)),
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
    match type_ann {
        TypeAnnotation::TagUnion { tags, extension } => {
            new_line(buf);

            let tag_union_indent = indent_level + 1;
            indent(buf, tag_union_indent);
            buf.push('[');
            new_line(buf);

            let next_indent_level = tag_union_indent + 1;

            for (index, tag) in tags.iter().enumerate() {
                indent(buf, next_indent_level);
                buf.push_str(tag.name.as_str());

                for type_value in &tag.values {
                    buf.push(' ');
                    type_annotation_to_html(next_indent_level, buf, type_value);
                }

                if index < (tags.len() - 1) {
                    buf.push(',');
                }

                new_line(buf);
            }

            indent(buf, tag_union_indent);
            buf.push(']');

            if let Some(ext) = extension {
                type_annotation_to_html(indent_level, buf, ext);
            }
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
        TypeAnnotation::Record { fields } => {
            new_line(buf);

            let record_indent = indent_level + 1;
            indent(buf, record_indent);
            buf.push('{');

            new_line(buf);

            let next_indent_level = record_indent + 1;
            for (index, field) in fields.iter().enumerate() {
                indent(buf, next_indent_level);

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

                if index < (fields.len() - 1) {
                    buf.push(',');
                }

                new_line(buf);
            }

            indent(buf, record_indent);
            buf.push('}');
        }
    }
}

fn insert_doc_links(markdown: String) -> String {
    insert_doc_links_help(markdown)
}

fn insert_doc_links_help(markdown: String) -> String {
    let buf = &markdown;
    let mut result = String::new();

    let mut chomping_from: Option<usize> = None;

    let mut chars = buf.chars().enumerate().peekable();

    while let Some((index, char)) = chars.next() {
        match chomping_from {
            None => {
                let next_is_alphabetic = match chars.peek() {
                    None => false,
                    Some((_, next_char)) => next_char.is_alphabetic(),
                };

                if char == '#' && next_is_alphabetic {
                    chomping_from = Some(index);
                }
            }
            Some(from) => {
                if !(char.is_alphabetic() || char == '.') {
                    let after_link = buf.chars().skip(from + buf.len());

                    result = buf.chars().take(from).collect();

                    let doc_link =
                        make_doc_link(buf.chars().skip(from + 1).take(index - from).collect());

                    result.insert_str(from, doc_link.as_str());

                    let remainder = insert_doc_links_help(after_link.collect());

                    result.push_str(remainder.as_str());
                    break;
                }
            }
        }
    }

    result
}

fn make_doc_link(doc_item: String) -> String {
    let mut label = String::new();
    let mut link = String::new();

    let mut parts = doc_item.split('.').into_iter().peekable();

    while let Some(part) = parts.next() {
        label.push_str(part);

        match parts.peek() {
            None => {
                link.push_str(".html#");
                link.push_str(part);
            }
            Some(_) => {
                link.push_str(part);
                link.push('.');

                label.push('.');
            }
        }
    }

    let mut buf = String::new();

    buf.push('[');
    buf.push_str(label.as_str());
    buf.push_str("](");

    buf.push_str(link.as_str());
    buf.push(')');

    buf
}

fn markdown_to_html(markdown: String) -> String {
    use pulldown_cmark::CodeBlockKind;
    use pulldown_cmark::CowStr;
    use pulldown_cmark::Event;
    use pulldown_cmark::Tag::*;

    let markdown_with_links = insert_doc_links(markdown);

    let markdown_options = pulldown_cmark::Options::empty();
    let mut docs_parser = vec![];
    let (_, _) = pulldown_cmark::Parser::new_ext(&markdown_with_links, markdown_options).fold(
        (0, 0),
        |(start_quote_count, end_quote_count), event| match event {
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
            _ => {
                docs_parser.push(event);
                (0, 0)
            }
        },
    );

    let mut docs_html = String::new();
    pulldown_cmark::html::push_html(&mut docs_html, docs_parser.into_iter());

    docs_html
}
