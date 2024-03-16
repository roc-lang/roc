use crate::{
    file::{self, Assets},
    html::{ModuleDocs, SidebarEntry},
    problem::Problem,
};
use bumpalo::Bump;
use core::fmt::Write;
use roc_collections::{VecMap, VecSet};
use roc_load::{
    docs::ModuleDocumentation, ExecutionMode, LoadConfig, LoadedModule, LoadingProblem, Threading,
};
use roc_module::symbol::ModuleId;
use roc_packaging::cache::{self, RocCacheDir};
use std::path::Path;

pub fn generate<'a>(
    arena: &'a Bump,
    root_file: &Path,
    build_dir: &'a Path,
    user_specified_base_url: Option<impl AsRef<str>>,
) -> Result<(), Problem> {
    let loaded_module = load_module_for_docs(arena, root_file)
        .map_err(|_loading_problem| Problem::FailedToLoadModule)?;

    // For debug builds, read assets from fs to speed up build
    // Otherwise, include as string literal

    #[cfg(not(debug_assertions))]
    let assets = {
        let search_js = include_str!("./static/search.js");
        let styles_css = include_str!("./static/styles.css");
        let raw_template_html = include_str!("./static/index.html");

        Assets {
            search_js,
            styles_css,
            raw_template_html,
        }
    };

    #[cfg(debug_assertions)]
    let assets = {
        use std::fs::read_to_string;

        // Construct the absolute path to the static assets
        let workspace_dir = std::env!("ROC_WORKSPACE_DIR");
        let static_dir = Path::new(workspace_dir).join("crates/docs/src/static");

        // Read the assets from the filesystem
        let search_js = read_to_string(static_dir.join("search.js")).unwrap();
        let styles_css = read_to_string(static_dir.join("styles.css")).unwrap();
        let raw_template_html = read_to_string(static_dir.join("index.html")).unwrap();

        Assets {
            search_js,
            styles_css,
            raw_template_html,
        }
    };

    // Copy over the assets
    file::populate_build_dir(arena, build_dir, &assets)?;

    // TODO get these from the platform's source file rather than hardcoding them!
    // github.com/roc-lang/roc/issues/5712
    let package_name = "Documentation";

    let exposed_symbols_by_module = {
        // Heurstic (a guess, really): assume an average of 8 exposed symbols per module.
        let mut map = VecMap::with_capacity(loaded_module.docs_by_module.len() * 8);

        for (module_id, docs) in loaded_module.docs_by_module.iter() {
            map.insert(module_id, &docs.exposed_symbols);
        }

        map
    };
    let home = loaded_module.module_id;
    let interns = loaded_module.interns;
    let todo = (); // TODO do we actually use/need home_ident_ids?
    let home_ident_ids = interns.all_ident_ids.get(&home).unwrap_or_else(|| {
        #[cfg(debug)] {
            panic!("all_ident_ids did not contain the home module ID ({home}). This should never happen!")
        }

        #[cfg(not(debug))] {
            // Gracefully attempt to continue.
            arena.alloc(Default::default())
        }
    });
    let module_name = interns.module_name(home);
    let docs_by_id = loaded_module.docs_by_module.iter();
    // let module_sidebar_entries = home_exposed_symbols.exposed.map(|symbol| {
    //     let todo = (); // TODO need to do this not on home_exposed_symbols
    //     let doc_comment = None; // TODO keep these after parsing somehow.

    //     SidebarEntry {
    //         module_name: module_name.as_str(),
    //         doc_comment,
    //     }
    // });
    let todo = (); // TODO don't have base_urls be empty here:
    let base_urls = Default::default();
    let package_sidebar_entries = docs_by_id.map(|(_, module_docs)| {
        let doc_comment = None; // TODO keep these after parsing somehow.

        SidebarEntry {
            link_text: module_docs.name.as_str(),
            doc_comment,
        }
    });
    let module_docs = ModuleDocs {
        module_name,
        home,
        home_ident_ids,
        interns: &interns,
        package_sidebar_entries,
        base_urls,
    };

    render_to_disk(RenderArgs {
        module_docs,
        arena,
        base_url: &base_url(user_specified_base_url),
        build_dir,
        package_name,
        docs_by_id: loaded_module.docs_by_module.iter(),
        raw_template_html: assets.raw_template_html.as_ref(),
    })
}

fn load_module_for_docs<'a>(
    arena: &'a Bump,
    filename: &Path,
) -> Result<LoadedModule, LoadingProblem<'a>> {
    let load_config = LoadConfig {
        target_info: roc_target::TargetInfo::default_x86_64(), // This is just type-checking for docs, so "target" doesn't matter
        function_kind: roc_solve::FunctionKind::LambdaSet,
        render: roc_reporting::report::RenderTarget::ColorTerminal,
        palette: roc_reporting::report::DEFAULT_PALETTE,
        threading: Threading::AllAvailable,
        exec_mode: ExecutionMode::Check,
    };

    roc_load::load_and_typecheck(
        &arena,
        filename.to_path_buf(),
        RocCacheDir::Persistent(cache::roc_cache_dir().as_path()),
        load_config,
    )
}

struct RenderArgs<
    'a,
    DocsById: Iterator<Item = &'a (ModuleId, ModuleDocumentation)> + Clone,
    SidebarEntries: Iterator<Item = SidebarEntry<'a>> + Clone,
> {
    arena: &'a Bump,
    base_url: &'a str,
    build_dir: &'a Path,
    package_name: &'a str,
    docs_by_id: DocsById,
    raw_template_html: &'a str,
    module_docs: ModuleDocs<'a, SidebarEntries>,
}

fn render_to_disk<
    'a,
    DocsById: Iterator<Item = &'a (ModuleId, ModuleDocumentation)> + Clone,
    SidebarEntries: Iterator<Item = SidebarEntry<'a>> + Clone,
>(
    RenderArgs {
        arena,
        base_url,
        build_dir,
        package_name,
        docs_by_id,
        raw_template_html,
        module_docs,
    }: RenderArgs<'a, DocsById, SidebarEntries>,
) -> Result<(), Problem> {
    let mut buf = bumpalo::collections::string::String::with_capacity_in(1024, arena);

    // Insert asset urls & sidebar links
    let mut template_html = raw_template_html
        .replace("<!-- Prefetch links -->", {
            for (index, (_, module)) in docs_by_id.clone().enumerate() {
                if index > 0 {
                    buf.write_str("\n    ");
                }

                write!(
                    &mut buf,
                    "<link rel='prefetch' href='{}'/>",
                    module.name.as_str()
                );
            }

            buf.as_str()
        })
        .replace("<!-- base -->", base_url)
        .replace("<!-- Module links -->", {
            buf.truncate(0);
            module_docs.render_sidebar(&mut buf);
            buf.as_str()
        });

    let all_exposed_symbols = {
        let mut set = VecSet::default();

        for (_, docs) in docs_by_id.clone() {
            set.insert_all(docs.exposed_symbols.iter().copied());
        }

        set
    };

    let mut output = template_html.as_str();

    // TODO fix: as is, this overrides an existing index.html
    // Write index.html for package (/index.html)
    {
        {
            buf.truncate(0);

            if write!(&mut buf, "<title>{package_name}</title>").is_ok() {
                output = arena.alloc(output.replace("<!-- Page title -->", buf.as_str()));
            }
        }

        {
            buf.truncate(0);

            if module_docs
                .render_package_name_link(package_name, base_url, &mut buf)
                .is_ok()
            {
                output = arena.alloc(output.replace("<!-- Package Name -->", buf.as_str()));
            }
        }

        {
            buf.truncate(0);

            if module_docs
                .render_package_index(
                    docs_by_id.clone().map(|(_, module_docs)| {
                        let doc_comment = None; // TODO keep these after parsing somehow.

                        SidebarEntry {
                            link_text: &module_docs.name,
                            doc_comment,
                        }
                    }),
                    &mut buf,
                )
                .is_ok()
            {
                output = arena.alloc(output.replace("<!-- Module Docs -->", buf.as_str()));
            }
        }

        file::write(arena, &build_dir.join("index.html"), &output)?;
    }

    // Write each package module's index.html file
    for module_name in docs_by_id.map(|(_, docs)| docs.name.as_str()) {
        let mut output = template_html.as_str();
        let module_dir = build_dir.join(module_name.replace('.', "/").as_str());

        file::create_dir_all(arena, &module_dir)?;

        {
            buf.truncate(0);

            if write!(&mut buf, "<title>{module_name} - {package_name}</title>",).is_ok() {
                output = arena.alloc(output.replace("<!-- Page title -->", buf.as_str()));
            }
        }

        {
            buf.truncate(0);

            if module_docs
                .render_package_name_link(package_name, base_url, &mut buf)
                .is_ok()
            {
                output.replace("<!-- Package Name -->", &buf);
            }
        }

        {
            buf.truncate(0);

            if module_docs
                .render_module(&all_exposed_symbols, &mut buf)
                .is_ok()
            {
                output.replace("<!-- Module Docs -->", &buf);
            }
        }

        file::write(arena, &module_dir.join("index.html"), output)?;
    }

    Ok(())
}

fn base_url(user_specified_base_url: Option<impl AsRef<str>>) -> String {
    // e.g. "builtins/" in "https://roc-lang.org/builtins/Str"
    match user_specified_base_url {
        Some(root_builtins_path) => {
            let root_builtins_path = root_builtins_path.as_ref();
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
        None => {
            let mut url_str = String::with_capacity(64);

            url_str.push('/');

            url_str
        }
    }
}
