extern crate fs_extra;
extern crate handlebars;
extern crate serde;
#[macro_use]
extern crate serde_derive;
extern crate pulldown_cmark;
extern crate serde_json;
use roc_builtins::std::StdLib;
use roc_load::docs::Documentation;
use roc_load::docs::ModuleDocumentation;

use std::fs;
extern crate roc_load;
use bumpalo::Bump;
use roc_collections::all::MutMap;
use std::path::{Path, PathBuf};

#[derive(Serialize)]
pub struct Template {
    package_name: String,
    package_version: String,
    module_name: String,
    module_docs: String,
    module_entries: Vec<ModuleEntry>,
    module_links: Vec<TemplateLink>,
}

#[derive(Serialize, Clone, Debug, PartialEq)]
pub struct ModuleEntry {
    name: String,
    docs: String,
}

#[derive(Serialize)]
pub struct TemplateLink {
    name: String,
    href: String,
    classes: String,
    entries: Vec<TemplateLinkEntry>,
}

#[derive(Serialize)]
pub struct TemplateLinkEntry {
    name: String,
}

fn main() {
    generate(
        vec![
            PathBuf::from(r"../compiler/builtins/docs/Bool.roc"),
            PathBuf::from(r"../compiler/builtins/docs/Map.roc"),
            // Not working
            // PathBuf::from(r"../compiler/builtins/docs/List.roc"),
            // Not working
            // PathBuf::from(r"../compiler/builtins/docs/Num.roc"),
            PathBuf::from(r"../compiler/builtins/docs/Set.roc"),
            PathBuf::from(r"../compiler/builtins/docs/Str.roc"),
        ],
        roc_builtins::std::standard_stdlib(),
        Path::new("../compiler/builtins/docs"),
        Path::new("./build"),
    )
}

pub fn generate(filenames: Vec<PathBuf>, std_lib: StdLib, src_dir: &Path, build_dir: &Path) {
    let files_docs = files_to_documentations(filenames, std_lib, src_dir);

    // TODO: get info from a file like "elm.json"
    let package = roc_load::docs::Documentation {
        name: "roc/builtins".to_string(),
        version: "1.0.0".to_string(),
        docs: "Package introduction or README.".to_string(),
        modules: files_docs,
    };

    // Remove old build folder, if exists
    let _ = fs::remove_dir_all(build_dir);

    let version_folder = build_dir
        .join(package.name.clone())
        .join(package.version.clone());

    // Make sure the output directories exists
    fs::create_dir_all(&version_folder)
        .expect("TODO gracefully handle creating directories failing");

    // Register handlebars template
    let mut handlebars = handlebars::Handlebars::new();
    handlebars
        .register_template_file("page", "./src/templates/page.hbs")
        .expect("TODO gracefully handle registering template failing");

    // Write each package's module docs html file
    for module in &package.modules {
        let template = documentation_to_template_data(&package, module);

        let handlebars_data = handlebars::to_json(&template);
        let filepath = version_folder.join(format!("{}.html", module.name));
        let mut output_file =
            fs::File::create(filepath).expect("TODO gracefully handle creating file failing");
        handlebars
            .render_to_write("page", &handlebars_data, &mut output_file)
            .expect("TODO gracefully handle writing file failing");
    }

    // Copy /static folder content to /build
    let copy_options = fs_extra::dir::CopyOptions {
        overwrite: true,
        skip_exist: false,
        buffer_size: 64000, //64kb
        copy_inside: false,
        content_only: true,
        depth: 0,
    };
    fs_extra::dir::copy("./src/static/", &build_dir, &copy_options)
        .expect("TODO gracefully handle copying static content failing");
    println!("Docs generated at {}", build_dir.display());
}

fn files_to_documentations(
    filenames: Vec<PathBuf>,
    std_lib: StdLib,
    src_dir: &Path,
) -> Vec<ModuleDocumentation> {
    let arena = Bump::new();
    let mut files_docs = vec![];

    for filename in filenames {
        let mut loaded = roc_load::file::load_and_typecheck(
            &arena,
            filename,
            std_lib.clone(),
            src_dir,
            MutMap::default(),
        )
        .expect("TODO gracefully handle load failing");
        files_docs.extend(loaded.documentation.drain().map(|x| x.1));
    }
    files_docs
}

fn documentation_to_template_data(doc: &Documentation, module: &ModuleDocumentation) -> Template {
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

#[cfg(test)]
mod test_docs {
    use super::*;

    #[test]
    fn internal() {
        let files_docs = files_to_documentations(
            vec![PathBuf::from(r"tests/fixtures/Interface.roc")],
            roc_builtins::std::standard_stdlib(),
            Path::new("tests/fixtures"),
        );

        let package = roc_load::docs::Documentation {
            name: "roc/builtins".to_string(),
            version: "1.0.0".to_string(),
            docs: "Package introduction or README.".to_string(),
            modules: files_docs,
        };

        let expected_entries = vec![
            ModuleEntry {
                name: "singleline".to_string(),
                docs: "<p>Single line documentation.</p>\n".to_string(),
            },
            ModuleEntry {
                name: "multiline".to_string(),
                docs: "<p>Multiline documentation.\nWithout any complex syntax yet!</p>\n".to_string(),
            }, ModuleEntry {
                name: "multiparagraph".to_string(),
                docs: "<p>Multiparagraph documentation.</p>\n<p>Without any complex syntax yet!</p>\n".to_string(),
            }, ModuleEntry {
                name: "codeblock".to_string(),
                docs: "<p>Turns &gt;&gt;&gt; into code block for now.</p>\n<pre><code class=\"language-roc\">codeblock</code></pre>\n".to_string(),
            },
        ];

        for module in &package.modules {
            let template = documentation_to_template_data(&package, module);
            assert_eq!(template.module_name, "Test");
            template
                .module_entries
                .iter()
                .zip(expected_entries.iter())
                .for_each(|(x, y)| assert_eq!(x, y));
        }
    }
}
