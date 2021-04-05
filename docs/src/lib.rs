extern crate fs_extra;
extern crate serde;
#[macro_use]
extern crate serde_derive;
extern crate pulldown_cmark;
extern crate serde_json;
use roc_builtins::std::StdLib;
use roc_can::builtins::builtin_defs_map;
use roc_load::docs::ModuleDocumentation;
use roc_load::docs::{DocTypeAnnotation, Documentation};
use roc_load::file::LoadingProblem;

use std::fs;
extern crate roc_load;
use bumpalo::Bump;
use roc_collections::all::MutMap;
use std::path::{Path, PathBuf};

#[derive(Serialize)]
pub struct Template {
    pub package_name: String,
    pub package_version: String,
    pub module_name: String,
    pub module_docs: String,
    pub module_entries: Vec<ModuleEntry>,
    pub module_links: Vec<TemplateLink>,
}

#[derive(Serialize, Clone, Debug, PartialEq)]
pub struct ModuleEntry {
    pub name: String,
    pub type_vars: Vec<String>,
    pub type_annotation: String,
    pub docs: String,
}

#[derive(Serialize)]
pub struct TemplateLink {
    pub name: String,
    pub href: String,
    pub classes: String,
    pub entries: Vec<TemplateLinkEntry>,
}

#[derive(Serialize)]
pub struct TemplateLinkEntry {
    name: String,
}

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

    // Register handlebars template
    let mut handlebars = handlebars::Handlebars::new();
    handlebars
        .register_template_file("page", "./docs/src/templates/page.hbs")
        .expect("TODO gracefully handle registering template failing");

    // Write each package's module docs html file
    for module in &package.modules {
        let template = documentation_to_template_data(&package, module);

        let handlebars_data = handlebars::to_json(&template);
        let filepath = build_dir.join(format!("{}.html", module.name));
        let mut output_file =
            fs::File::create(filepath).expect("TODO gracefully handle creating file failing");
        handlebars
            .render_to_write("page", &handlebars_data, &mut output_file)
            .expect("TODO gracefully handle writing file failing");
    }

    println!("🎉 Docs generated in {}", build_dir.display());
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

pub fn documentation_to_template_data(
    doc: &Documentation,
    module: &ModuleDocumentation,
) -> Template {
    Template {
        package_name: doc.name.clone(),
        package_version: doc.version.clone(),
        module_name: module.name.clone(),
        module_docs: markdown_to_html(module.docs.clone()),
        module_entries: module
            .entries
            .clone()
            .into_iter()
            .map(|entry| ModuleEntry {
                name: entry.name.clone(),
                type_vars: entry.type_vars,
                type_annotation: match entry.type_annotation {
                    None => String::new(),
                    Some(type_ann) => {
                        let type_ann_html = &mut String::new();

                        type_ann_html.push_str(" : ");

                        type_annotation_to_html(0, type_ann_html, &type_ann);

                        type_ann_html.to_string()
                    }
                },
                docs: match entry.docs {
                    Some(docs) => markdown_to_html(docs),
                    None => String::new(),
                },
            })
            .collect(),
        module_links: doc
            .modules
            .clone()
            .into_iter()
            .map(|module_link| TemplateLink {
                name: module_link.name.clone(),
                href: format!("./{}.html", module_link.name),
                classes: "".to_string(),
                entries: module_link
                    .entries
                    .into_iter()
                    .map(|entry| TemplateLinkEntry { name: entry.name })
                    .collect(),
            })
            .collect(),
    }
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
