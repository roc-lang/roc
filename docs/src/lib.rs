extern crate pulldown_cmark;
use roc_builtins::std::StdLib;
use roc_can::builtins::builtin_defs_map;
use roc_load::docs::DocTypeAnnotation;
use roc_load::docs::ModuleDocumentation;
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

    buf.push_str(markdown_to_html(module.docs.clone()).as_str());

    for entry in &module.entries {
        let mut href = String::new();
        href.push('#');
        href.push_str(entry.name.as_str());

        let name = entry.name.as_str();

        let mut content = String::new();

        content.push_str(html_node("a", vec![("href", href.as_str())], name).as_str());

        for type_var in &entry.type_vars {
            content.push(' ');
            content.push_str(type_var.as_str());
        }

        if let Some(type_ann) = &entry.type_annotation {
            content.push_str(" : ");
            type_annotation_to_html(0, &mut content, &type_ann);
        }

        buf.push_str(html_node("h3", vec![("id", name)], content.as_str()).as_str());

        if let Some(docs) = &entry.docs {
            buf.push_str(docs.as_str());
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
                let mut entry_href = String::new();

                entry_href.push_str(href.as_str());
                entry_href.push('#');
                entry_href.push_str(entry.name.as_str());

                entries_buf.push_str(
                    html_node(
                        "a",
                        vec![("href", entry_href.as_str())],
                        entry.name.as_str(),
                    )
                    .as_str(),
                );
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

const INDENT: &str = "&nbsp;&nbsp;&nbsp;&nbsp;";

fn indent(buf: &mut String, times: usize) {
    for _ in 0..times {
        buf.push_str(INDENT);
    }
}

fn type_annotation_to_html(indent_level: usize, buf: &mut String, type_ann: &DocTypeAnnotation) {
    match type_ann {
        DocTypeAnnotation::TagUnion { tags, extension } => {
            buf.push_str("<br>");

            let tag_union_indent = indent_level + 1;
            indent(buf, tag_union_indent);
            buf.push('[');
            buf.push_str("<br>");

            let mut index = 0;
            let next_indent_level = tag_union_indent + 1;
            let tags_len = tags.len();
            while index < tags_len {
                let tag = &tags[index];

                indent(buf, next_indent_level);
                buf.push_str(tag.name.as_str());

                let mut tag_value_index = 0;
                while tag_value_index < tag.values.len() {
                    let type_value = &tag.values[tag_value_index];

                    buf.push(' ');
                    type_annotation_to_html(next_indent_level, buf, type_value);

                    tag_value_index += 1;
                }

                if index < (tags_len - 1) {
                    buf.push(',');
                }

                buf.push_str("<br>");

                index += 1;
            }

            indent(buf, tag_union_indent);
            buf.push(']');

            if let Some(ext) = extension {
                type_annotation_to_html(indent_level, buf, ext);
            }
        }
        DocTypeAnnotation::BoundVariable(var_name) => {
            buf.push_str(var_name);
        }
        DocTypeAnnotation::Apply { name, parts } => {
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
    }
}

fn markdown_to_html(markdown: String) -> String {
    use pulldown_cmark::CodeBlockKind::*;
    use pulldown_cmark::CowStr::*;
    use pulldown_cmark::Event::*;
    use pulldown_cmark::Tag::*;

    let markdown_options = pulldown_cmark::Options::all();
    let mut docs_parser = vec![];
    let (_, _) = pulldown_cmark::Parser::new_ext(&markdown, markdown_options).fold(
        (0, 0),
        |(start_quote_count, end_quote_count), event| match event {
            // Replace this sequence (`>>>` syntax):
            //     Start(BlockQuote)
            //     Start(BlockQuote)
            //     Start(BlockQuote)
            //     Start(Paragraph)
            // For `Start(CodeBlock(Fenced(Borrowed("roc"))))`
            Start(BlockQuote) => {
                docs_parser.push(event);
                (start_quote_count + 1, 0)
            }
            Start(Paragraph) => {
                if start_quote_count == 3 {
                    docs_parser.pop();
                    docs_parser.pop();
                    docs_parser.pop();
                    docs_parser.push(Start(CodeBlock(Fenced(Borrowed("roc")))));
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
            End(Paragraph) => {
                docs_parser.push(event);
                (0, 1)
            }
            End(BlockQuote) => {
                if end_quote_count == 3 {
                    docs_parser.pop();
                    docs_parser.pop();
                    docs_parser.pop();
                    docs_parser.push(End(CodeBlock(Fenced(Borrowed("roc")))));
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
