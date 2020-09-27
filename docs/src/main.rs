extern crate fs_extra;
extern crate handlebars;
extern crate serde;
#[macro_use]
extern crate serde_derive;
extern crate pulldown_cmark;
extern crate serde_json;
use std::error::Error;
use std::fs;
extern crate roc_load;
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

#[derive(Serialize, Clone)]
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

fn main() -> Result<(), Box<dyn Error>> {
    let std_lib = roc_builtins::std::standard_stdlib();
    let subs_by_module = MutMap::default();
    let src_dir = Path::new("../compiler/builtins/docs");
    let files = vec![
        PathBuf::from(r"../compiler/builtins/docs/Bool.roc"),
        PathBuf::from(r"../compiler/builtins/docs/Map.roc"),
        // Not working
        // PathBuf::from(r"../compiler/builtins/docs/List.roc"),
        // Not working
        // PathBuf::from(r"../compiler/builtins/docs/Num.roc"),
        PathBuf::from(r"../compiler/builtins/docs/Set.roc"),
        PathBuf::from(r"../compiler/builtins/docs/Str.roc"),
    ];

    let mut modules_docs = vec![];

    // Load each file is files vector
    for filename in files {
        let loaded = roc_load::docs::load(filename, &std_lib, src_dir, subs_by_module.clone())
            .expect("TODO gracefully handle load failing");
        modules_docs.push(loaded.module_docs);
    }

    let package = roc_load::docs::Documentation {
        name: "roc/builtins".to_string(),
        version: "1.0.0".to_string(),
        docs: "Package introduction or README.".to_string(),
        modules: modules_docs,
    };

    // Remove old build folder
    fs::remove_dir_all("./build")?;

    // Make sure the output directories exists
    fs::create_dir_all(format!("./build/{}/{}", package.name, package.version))?;

    // Register handlebars template
    let mut handlebars = handlebars::Handlebars::new();
    assert!(handlebars
        .register_template_file("page", "./src/templates/page.hbs")
        .is_ok());

    let markdown_options = pulldown_cmark::Options::all();

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
                    let mut entry_docs_html: String = String::new();
                    if let Some(docs) = entry.docs {
                        let entry_docs_parser =
                            pulldown_cmark::Parser::new_ext(&docs, markdown_options);
                            pulldown_cmark::html::push_html(&mut entry_docs_html, entry_docs_parser);
                    }

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
