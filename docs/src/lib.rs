extern crate fs_extra;
extern crate serde;
#[macro_use]
extern crate serde_derive;
extern crate pulldown_cmark;
extern crate serde_json;
use roc_builtins::std::StdLib;
use roc_can::builtins::builtin_defs_map;
use roc_load::docs::Documentation;
use roc_load::docs::ModuleDocumentation;
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

    println!("Docs generated at {}", build_dir.display());
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
