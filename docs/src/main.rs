extern crate fs_extra;
extern crate handlebars;
extern crate serde;
#[macro_use]
extern crate serde_derive;
extern crate pulldown_cmark;
extern crate serde_json;
use std::error::Error;
use std::fs;

#[derive(Serialize)]
pub struct Package {
    name: String,
    version: String,
    docs: String,
    modules: Vec<Module>,
}

#[derive(Serialize, Clone)]
pub struct Module {
    name: String,
    docs: String,
    entries: Vec<ModuleEntry>,
}

#[derive(Serialize, Clone)]
pub struct ModuleEntry {
    name: String,
    docs: String,
}

#[derive(Serialize)]
pub struct Template {
    package_name: String,
    package_version: String,
    module_name: String,
    module_docs: String,
    module_entries: Vec<ModuleEntry>,
    module_links: Vec<TemplateLink>,
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

fn main() -> Result<(), Box<dyn Error>> {
    let package = Package {
        name: "roc/builtins".to_string(),
        version: "1.0.0".to_string(),
        docs: "Package introduction or README.".to_string(),
        modules: vec![
            Module {
                name: "Str".to_string(),
                docs: "Module introduction".to_string(),
                entries: vec![
                    ModuleEntry {
                        name: "Str".to_string(),
                        docs: "Hello world, this is a **complicated** *very simple* example."
                            .to_string(),
                    },
                    ModuleEntry {
                        name: "isEmpty".to_string(),
                        docs: "Hello world, this is a **complicated** *very simple* example."
                            .to_string(),
                    },
                    ModuleEntry {
                        name: "append".to_string(),
                        docs: "Hello world, this is a **complicated** *very simple* example."
                            .to_string(),
                    },
                    ModuleEntry {
                        name: "prepend".to_string(),
                        docs: "Hello world, this is a **complicated** *very simple* example."
                            .to_string(),
                    },
                    ModuleEntry {
                        name: "concat".to_string(),
                        docs: "Hello world, this is a **complicated** *very simple* example."
                            .to_string(),
                    },
                    ModuleEntry {
                        name: "join".to_string(),
                        docs: "Hello world, this is a **complicated** *very simple* example."
                            .to_string(),
                    },
                    ModuleEntry {
                        name: "split".to_string(),
                        docs: "Hello world, this is a **complicated** *very simple* example."
                            .to_string(),
                    },
                    ModuleEntry {
                        name: "countGraphemes".to_string(),
                        docs: "Hello world, this is a **complicated** *very simple* example."
                            .to_string(),
                    },
                    ModuleEntry {
                        name: "foldGraphemes".to_string(),
                        docs: "Hello world, this is a **complicated** *very simple* example."
                            .to_string(),
                    },
                ],
            },
            Module {
                name: "Bool".to_string(),
                docs: "Hello world, this is a **complicated** *very simple* example.".to_string(),
                entries: vec![
                    ModuleEntry {
                        name: "isEq".to_string(),
                        docs: "Hello world, this is a **complicated** *very simple* example."
                            .to_string(),
                    },
                    ModuleEntry {
                        name: "isNeq".to_string(),
                        docs: "Hello world, this is a **complicated** *very simple* example."
                            .to_string(),
                    },
                ],
            },
            Module {
                name: "Num".to_string(),
                docs: "Hello world, this is a **complicated** *very simple* example.".to_string(),
                entries: vec![
                    ModuleEntry {
                        name: "add".to_string(),
                        docs: "Hello world, this is a **complicated** *very simple* example."
                            .to_string(),
                    },
                    ModuleEntry {
                        name: "sub".to_string(),
                        docs: "Hello world, this is a **complicated** *very simple* example."
                            .to_string(),
                    },
                    ModuleEntry {
                        name: "mul".to_string(),
                        docs: "Hello world, this is a **complicated** *very simple* example."
                            .to_string(),
                    },
                ],
            },
            Module {
                name: "List".to_string(),
                docs: "Hello world, this is a **complicated** *very simple* example.".to_string(),
                entries: vec![],
            },
            Module {
                name: "Set".to_string(),
                docs: "Hello world, this is a **complicated** *very simple* example.".to_string(),
                entries: vec![],
            },
            Module {
                name: "Map".to_string(),
                docs: "Hello world, this is a **complicated** *very simple* example.".to_string(),
                entries: vec![],
            },
            Module {
                name: "Result".to_string(),
                docs: "Hello world, this is a **complicated** *very simple* example.".to_string(),
                entries: vec![],
            },
        ],
    };

    // Make sure the directories exists
    fs::create_dir_all(format!("./build/{}/{}", package.name, package.version))?;

    // Register handlebar template
    let mut handlebars = handlebars::Handlebars::new();
    assert!(handlebars
        .register_template_file("page", "./src/templates/page.hbs")
        .is_ok());

    let markdown_options = pulldown_cmark::Options::empty();

    // Write each package's module docs
    for module in &package.modules {
        // Convert module docs from markdown to html
        let docs_parser = pulldown_cmark::Parser::new_ext(&module.docs, markdown_options);
        let mut docs_html: String = String::with_capacity(module.docs.len() * 3 / 2);
        pulldown_cmark::html::push_html(&mut docs_html, docs_parser);

        let template = Template {
            package_name: package.name.clone(),
            package_version: package.version.clone(),
            module_name: module.name.clone(),
            module_docs: docs_html,
            module_entries: module
                .entries
                .clone()
                .into_iter()
                .map(|entry| {
                    // Convert entry docs from markdown to html
                    let entry_docs_parser =
                        pulldown_cmark::Parser::new_ext(&entry.docs, markdown_options);
                    let mut entry_docs_html: String =
                        String::with_capacity(entry.docs.len() * 3 / 2);
                    pulldown_cmark::html::push_html(&mut entry_docs_html, entry_docs_parser);

                    ModuleEntry {
                        name: entry.name.clone(),
                        docs: entry_docs_html,
                    }
                })
                .collect(),
            module_links: package
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
        };

        let handlebars_data = handlebars::to_json(&template);
        let mut output_file = fs::File::create(format!(
            "./build/{}/{}/{}.html",
            package.name, package.version, module.name
        ))?;
        handlebars.render_to_write("page", &handlebars_data, &mut output_file)?;
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
    fs_extra::dir::copy("./src/static/", "./build", &copy_options)?;
    println!("Docs generated at /build");
    Ok(())
}
