use crate::{
    file::{self, Assets},
    html::{self, ModuleDocs, SidebarEntry},
    problem::Problem,
};
use bumpalo::{collections::string::String, Bump};
use core::fmt::{self, Write};
use roc_can::expr::Declarations;
use roc_collections::{VecMap, VecSet};
use roc_load::{
    docs::{DocEntry, ModuleDocumentation},
    ExecutionMode, LoadConfig, LoadedModule, LoadingProblem, Threading,
};
use roc_module::symbol::{ModuleId, Symbol};
use roc_packaging::cache::{self, RocCacheDir};
use roc_types::{subs::Subs, types::Alias};
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
        let favicon_svg = include_str!("../../../www/public/favicon.svg");
        let raw_template_html = include_str!("./static/index.html");

        Assets {
            search_js,
            styles_css,
            raw_template_html,
            favicon_svg,
        }
    };

    #[cfg(debug_assertions)]
    let assets = {
        use std::fs::read_to_string;

        // Construct the absolute path to the static assets
        let workspace_dir = std::env!("ROC_WORKSPACE_DIR");
        let public_dir = Path::new(workspace_dir).join("www/public");
        let static_dir = Path::new(workspace_dir).join("crates/docs/src/static");

        // Read the assets from the filesystem
        let search_js = read_to_string(static_dir.join("search.js")).unwrap();
        let styles_css = read_to_string(static_dir.join("styles.css")).unwrap();
        let favicon_svg = read_to_string(public_dir.join("favicon.svg")).unwrap();
        let raw_template_html = read_to_string(static_dir.join("index.html")).unwrap();

        Assets {
            search_js,
            styles_css,
            favicon_svg,
            raw_template_html,
        }
    };

    // Copy over the assets
    file::populate_build_dir(arena, build_dir, &assets)?;

    // TODO get this name from somewhere else (e.g. the <h1> in the platform's header docs)
    // and if that's unavailable, default to "Package" or "Platform" based on which it is.
    // Also, include a version number (from the source code).
    //
    // github.com/roc-lang/roc/issues/5712
    let package_name = "Documentation";
    let home = loaded_module.module_id;
    let interns = &loaded_module.interns;

    let todo = (); // TODO use the abilities store to somehow obtain which abilities a given opaque type implements
    let abilities = &loaded_module.abilities_store;

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
    let subs = loaded_module.solved.inner();
    let todo = (); // TODO don't have base_urls be empty here:
    let base_urls = Default::default();
    let todo = (); // TODO populate the package doc comment as markdown from the AST
    let package_doc_comment_html = "";
    let package_sidebar_entries = loaded_module.docs_by_module.iter().map(|(_, module_docs)| {
        let doc_comment = None; // TODO keep these after parsing somehow.

        SidebarEntry {
            link_text: module_docs.name.as_str(),
            exposed: module_docs.entries.iter().filter_map(|entry| match entry {
                DocEntry::DocDef(doc_def)
                    if module_docs.exposed_symbols.contains(&doc_def.symbol) =>
                {
                    Some(&doc_def.name)
                }
                _ => None,
            }),
            doc_comment,
        }
    });
    let module_docs = ModuleDocs {
        arena,
        module_name,
        package_doc_comment_html,
        home,
        home_ident_ids,
        interns,
        package_sidebar_entries,
        base_urls,
    };

    render_to_disk(RenderArgs {
        module_docs,
        subs,
        arena,
        user_specified_base_url,
        build_dir,
        package_name,
        docs_by_id: loaded_module.docs_by_module.iter(),
        raw_template_html: assets.raw_template_html.as_ref(),
        declarations: |module_id| loaded_module.declarations(module_id),
        exposed_aliases: |module_id| {
            loaded_module
                .aliases(module_id)
                .into_iter()
                .flatten()
                .filter_map(|(symbol, (is_exposed, alias))| {
                    if *is_exposed {
                        Some((*symbol, alias))
                    } else {
                        None
                    }
                })
        },
        alias_by_symbol: |symbol| {
            loaded_module
                .aliases
                .get(&symbol.module_id())
                .and_then(|aliases_by_symbol| {
                    aliases_by_symbol
                        .get(&symbol)
                        .map(|(_exposed, alias)| alias)
                })
        },
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
    ExposedSidebarEntries: Iterator<Item = S> + Clone,
    SidebarEntries: Iterator<Item = SidebarEntry<'a, ExposedSidebarEntries, S>> + Clone,
    S: AsRef<str> + fmt::Display,
    Decls: Fn(ModuleId) -> Option<&'a Declarations>,
    ExposedAliases: Fn(ModuleId) -> AliasIter,
    AliasIter: Iterator<Item = (Symbol, &'a Alias)> + Clone,
    AliasBySymbol: Fn(Symbol) -> Option<&'a Alias>,
    BaseUrl: AsRef<str>,
> {
    arena: &'a Bump,
    subs: &'a Subs,
    user_specified_base_url: Option<BaseUrl>,
    build_dir: &'a Path,
    package_name: &'a str,
    docs_by_id: DocsById,
    raw_template_html: &'a str,
    declarations: Decls,
    exposed_aliases: ExposedAliases,
    module_docs: ModuleDocs<'a, SidebarEntries, ExposedSidebarEntries, S>,
    alias_by_symbol: AliasBySymbol,
}

fn render_to_disk<
    'a,
    DocsById: Iterator<Item = &'a (ModuleId, ModuleDocumentation)> + Clone,
    ExposedSidebarEntries: Iterator<Item = S> + Clone,
    SidebarEntries: Iterator<Item = SidebarEntry<'a, ExposedSidebarEntries, S>> + Clone,
    S: AsRef<str> + fmt::Display,
    Decls: Fn(ModuleId) -> Option<&'a Declarations>,
    ExposedAliases: Fn(ModuleId) -> AliasIter,
    AliasIter: Iterator<Item = (Symbol, &'a Alias)> + Clone,
    BaseUrl: AsRef<str>,
    AliasBySymbol: Fn(Symbol) -> Option<&'a Alias>,
>(
    RenderArgs {
        arena,
        subs,
        user_specified_base_url,
        build_dir,
        package_name,
        docs_by_id,
        raw_template_html,
        declarations,
        exposed_aliases,
        module_docs,
        alias_by_symbol,
    }: RenderArgs<
        'a,
        DocsById,
        ExposedSidebarEntries,
        SidebarEntries,
        S,
        Decls,
        ExposedAliases,
        AliasIter,
        AliasBySymbol,
        BaseUrl,
    >,
) -> Result<(), Problem> {
    let all_exposed_symbols = {
        let mut set = VecSet::default();

        for (_, docs) in docs_by_id.clone() {
            set.insert_all(docs.exposed_symbols.iter().copied());
        }

        set
    };

    let mut buf = String::with_capacity_in(raw_template_html.len() + 2048, arena);
    let mut module_template_html = String::with_capacity_in(raw_template_html.len() + 2048, arena);
    let mut sidebar_links = String::with_capacity_in(4096, arena);

    let buf = &mut buf;
    let sidebar_links = &mut sidebar_links;

    module_docs.render_sidebar(sidebar_links);

    // Write index.html for package (/index.html)
    {
        let mut src = raw_template_html;

        {
            src = advance_past("<!-- base -->", src, buf);
            write_base_url(user_specified_base_url, buf);
        }

        {
            src = advance_past("<!-- Prefetch links -->", src, buf);

            for (index, (_, module)) in docs_by_id.clone().enumerate() {
                if index > 0 {
                    buf.push_str("\n    ");
                }

                let _ = write!(
                    buf,
                    "<link rel='prefetch' href='{}'/>",
                    module.name.as_str()
                );
            }
        }

        // Set module_template_html to be all the replacements we've made so far,
        // plus the rest of the source template. We'll use this partially-completed
        // template later on for the individual modules.
        {
            module_template_html.push_str(&buf);
            module_template_html.push_str(&src);
        }

        {
            src = advance_past("<!-- Page title -->", src, buf);
            let _ = write!(buf, "<title>{package_name}</title>");
        }

        {
            src = advance_past("<!-- Module links -->", src, buf);
            buf.push_str(&sidebar_links);
        }

        {
            src = advance_past("<!-- Package Name -->", src, buf);
            html::render_package_name_link(package_name, buf);
        }

        {
            src = advance_past("<!-- Module Docs -->", src, buf);

            if module_docs.package_doc_comment_html.is_empty() {
                buf.push_str("Choose a module from the list to see its documentation.");
            } else {
                buf.push_str(module_docs.package_doc_comment_html);
            }
        }

        {
            // Write the rest of the template into the buffer.
            buf.push_str(&src);

            // Finally, write the accumulated buffer to disk.
            file::write(arena, &build_dir.join("index.html"), &buf)?;

            buf.clear(); // We're done with this now. It's ready to be reused!
        }
    }

    // Write each package module's index.html file
    for (module_id, docs) in docs_by_id {
        let mut src = module_template_html.as_str();

        {
            let name = docs.name.as_str();

            {
                src = advance_past("<!-- Page title -->", src, buf);
                let _ = write!(buf, "<title>{name} - {package_name}</title>",);
            }

            {
                src = advance_past("<!-- Module links -->", src, buf);
                buf.push_str(sidebar_links);
            }

            {
                src = advance_past("<!-- Package Name -->", src, buf);
                html::render_package_name_link(package_name, buf);
            }
        }

        {
            src = advance_past("<!-- Module Docs -->", src, buf);
            html::render_module(
                arena,
                *module_id,
                &docs,
                declarations(*module_id),
                exposed_aliases(*module_id),
                &alias_by_symbol,
                &subs,
                buf,
            );
        }

        {
            // Write the rest of the template into the buffer.
            buf.push_str(&src);
        }

        {
            let name = docs.name.as_str();
            let module_dir = build_dir.join(name.replace('.', "/").as_str());

            file::create_dir_all(arena, &module_dir)?;

            // Finally, write the accumulated buffer to disk.
            file::write(arena, &module_dir.join("index.html"), &buf)?;
        }

        buf.clear(); // We're done with this now. It's ready to be reused in the next iteration of the loop!
    }

    Ok(())
}

fn advance_past<'a>(needle: &'static str, src: &'a str, buf: &mut String<'_>) -> &'a str {
    if let Some(start_index) = src.find(needle) {
        // Copy over everything up to this point.
        buf.push_str(&src[..start_index]);

        // Advance past the end of this string.
        &src[(start_index + needle.len())..]
    } else {
        unreachable!( // TODO replace this with a panic in debug builds and a much more concise crash in release
            "Compiler bug in docs generation code: could not find doc template section {:?} in the template - this should never happen!\n\nNOTE: advance_past must be called on each template section in the order they appear in the template! This improves performance, but means that working on sections out of order can lead to this error.\n\nAt this point, the remaining template was:\n\n{src}",
            needle
        );
    }
}

fn write_base_url(user_specified_base_url: Option<impl AsRef<str>>, buf: &mut String) {
    // e.g. "builtins/" in "https://roc-lang.org/builtins/Str"
    match user_specified_base_url {
        Some(root_builtins_path) => {
            let root_builtins_path = root_builtins_path.as_ref();

            if !root_builtins_path.starts_with('/') {
                buf.push('/');
            }

            buf.push_str(&root_builtins_path);

            if !root_builtins_path.ends_with('/') {
                buf.push('/');
            }
        }
        None => {
            buf.push('/');
        }
    }
}
