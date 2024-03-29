use crate::file::{self, Assets};
use crate::problem::Problem;
use bumpalo::Bump;
use core::slice::Iter;
use roc_can::scope::Scope;
use roc_collections::VecSet;
use roc_docs_render::{BodyEntry, Docs, SidebarEntry, TypeRenderer};
use roc_load::docs::{DocEntry, TypeAnnotation};
use roc_load::docs::{ModuleDocumentation, RecordField};
use roc_load::{ExecutionMode, LoadConfig, LoadedModule, LoadingProblem, Threading};
use roc_module::symbol::{IdentId, Interns, ModuleId, Symbol};
use roc_packaging::cache::{self, RocCacheDir};
use roc_packaging::tarball::build;
use roc_parse::ident::{parse_ident, Accessor, Ident};
use roc_parse::keyword;
use roc_parse::state::State;
use roc_region::all::Region;
use roc_types::types::{Alias, Type};
use std::fs;
use std::path::{Path, PathBuf};

pub fn generate_docs_html<'a>(
    arena: &'a Bump,
    pkg_name: &'a str,
    root_file: PathBuf,
    build_dir: impl AsRef<Path>,
) -> Result<(), Problem> {
    let loaded_module = load_module_for_docs(root_file);

    // Copy over the assets
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
            favicon_svg,
            raw_template_html,
        }
    };

    #[cfg(debug_assertions)]
    let assets = {
        // Construct the absolute path to the static assets
        let workspace_dir = std::env!("ROC_WORKSPACE_DIR");
        let static_dir = Path::new(workspace_dir).join("crates/docs/src/static");

        // Read the assets from the filesystem
        let search_js = fs::read_to_string(static_dir.join("search.js")).unwrap();
        let styles_css = fs::read_to_string(static_dir.join("styles.css")).unwrap();
        let favicon_svg =
            fs::read_to_string(static_dir.join("../../../www/public/favicon.svg")).unwrap();
        let raw_template_html = fs::read_to_string(static_dir.join("index.html")).unwrap();

        Assets {
            search_js,
            styles_css,
            favicon_svg,
            raw_template_html,
        }
    };

    // Clear out generated-docs dir and populate it with static assets
    // (.js, .css, etc.) - note that this does not actually delete the
    // directory if it already existed, but rather deletes all the files
    // inside it. That way, if you have a shell open to that directory,
    // it doesn't get messed up from the dir having been deleted out from
    // under the shell.
    file::populate_build_dir(arena, build_dir.as_ref(), &assets)?;

    IoDocs {
        arena,
        loaded_module,
        raw_template_html: assets.raw_template_html.as_ref(),
        // TODO get this from the platform's source file rather than hardcoding it!
        // github.com/roc-lang/roc/issues/5712
        pkg_name: "Documentation",
    }
    .generate(build_dir)
}

struct IoDocs<'a> {
    arena: &'a Bump,
    loaded_module: LoadedModule,
    raw_template_html: &'a str,
    pkg_name: &'a str,
}

impl<'a> IoDocs<'a> {
    fn generate(self, build_dir: impl AsRef<Path>) -> Result<(), Problem> {
        let arena = &self.arena;
        let build_dir = build_dir.as_ref();

        self.render_to_disk(
            self.arena,
            // Takes the module name to be used as the directory name
            // (or None if this is the root index.html),
            // as well as the contents of the file.
            |opt_module_name, contents| {
                let dir = match opt_module_name {
                    Some(module_name) => PathBuf::from(build_dir)
                        // Replace dots in the module name with slashes
                        .join(module_name.replace('.', std::path::MAIN_SEPARATOR_STR)),
                    None => PathBuf::from(build_dir),
                };

                // TODO mkdir -p on dir_buf
                /////////////////////////////////////////////////

                let path_buf = dir.join("index.html");

                file::write(arena, path_buf, contents)
            },
        )
    }
}

impl<'a>
    Docs<
        'a,
        ModuleId,
        IdentId,
        Type,
        Alias,
        TypeRenderer<'a>,
        Iter<'a, (ModuleId, &'a str)>,
        Iter<'a, SidebarEntry<'a, Iter<'a, &'a str>>>,
        Iter<'a, &'a str>,
        Iter<'a, BodyEntry<'a, Type, IdentId>>,
        Iter<'a, (&'a str, Iter<'a, Type>)>,
        Iter<'a, Type>,
    > for IoDocs<'a>
{
    fn package_name(&self) -> &'a str {
        self.pkg_name
    }

    fn raw_template_html(&self) -> &'a str {
        self.raw_template_html
    }

    fn user_specified_base_url(&self) -> Option<&'a str> {
        todo!()
    }

    fn package_doc_comment_html(&self) -> &'a str {
        todo!()
    }

    fn package_sidebar_entries(&self) -> Iter<'a, SidebarEntry<'a, Iter<'a, &'a str>>> {
        todo!()
    }

    fn body_entries(&self) -> Iter<'a, BodyEntry<'a, Type, IdentId>> {
        todo!()
    }

    fn base_url(&self, module_id: ModuleId) -> &'a str {
        todo!()
    }

    fn module_name(&self, module_id: ModuleId) -> &'a str {
        todo!()
    }

    fn ident_name(&self, module_id: ModuleId, ident_id: IdentId) -> &'a str {
        todo!()
    }

    fn opt_type(&self, module_id: ModuleId, ident_id: IdentId) -> Option<Type> {
        todo!()
    }

    fn opt_alias(&self, module_id: ModuleId, ident_id: IdentId) -> Option<Alias> {
        todo!()
    }

    fn module_names(&self) -> Iter<'a, (ModuleId, &'a str)> {
        todo!()
    }

    fn visit_type(&self, renderer: &mut TypeRenderer<'_>, typ: Type) {
        todo!()
    }
}

// fn render_package_index(root_module: &LoadedModule) -> String {
//     // The list items containing module links
//     let mut module_list_buf = String::new();

//     for (_, module) in root_module.docs_by_module.iter() {
//         // The anchor tag containing the module link
//         let mut link_buf = String::new();

//         push_html(
//             &mut link_buf,
//             "a",
//             vec![("href", module.name.as_str())],
//             module.name.as_str(),
//         );

//         push_html(&mut module_list_buf, "li", vec![], link_buf.as_str());
//     }

//     // The HTML for the index page
//     let mut index_buf = String::new();

//     push_html(
//         &mut index_buf,
//         "h2",
//         vec![("class", "module-name")],
//         "Exposed Modules",
//     );
//     push_html(
//         &mut index_buf,
//         "ul",
//         vec![("class", "index-module-links")],
//         module_list_buf.as_str(),
//     );

//     index_buf
// }

// fn render_module_documentation(
//     module: &ModuleDocumentation,
//     root_module: &LoadedModule,
//     all_exposed_symbols: &VecSet<Symbol>,
// ) -> String {
//     let mut buf = String::new();
//     let module_name = module.name.as_str();

//     push_html(&mut buf, "h2", vec![("class", "module-name")], {
//         let mut link_buf = String::new();

//         push_html(&mut link_buf, "a", vec![("href", "/")], module_name);

//         link_buf
//     });

//     for entry in &module.entries {
//         match entry {
//             DocEntry::DocDef(doc_def) => {
//                 // Only render entries that are exposed
//                 if all_exposed_symbols.contains(&doc_def.symbol) {
//                     buf.push_str("<section>");

//                     let def_name = doc_def.name.as_str();
//                     let href = format!("{module_name}#{def_name}");
//                     let mut content = String::new();

//                     push_html(&mut content, "a", vec![("href", href.as_str())], LINK_SVG);
//                     push_html(&mut content, "strong", vec![], def_name);

//                     for type_var in &doc_def.type_vars {
//                         content.push(' ');
//                         content.push_str(type_var.as_str());
//                     }

//                     let type_ann = &doc_def.type_annotation;

//                     if !matches!(type_ann, TypeAnnotation::NoTypeAnn) {
//                         // Ability declarations don't have ":" after the name, just `implements`
//                         if !matches!(type_ann, TypeAnnotation::Ability { .. }) {
//                             content.push_str(" :");
//                         }

//                         content.push(' ');

//                         type_annotation_to_html(0, &mut content, type_ann, false);
//                     }

//                     push_html(
//                         &mut buf,
//                         "h3",
//                         vec![("id", def_name), ("class", "entry-name")],
//                         content.as_str(),
//                     );

//                     if let Some(docs) = &doc_def.docs {
//                         markdown_to_html(
//                             &mut buf,
//                             all_exposed_symbols,
//                             &module.scope,
//                             docs,
//                             root_module,
//                         );
//                     }

//                     buf.push_str("</section>");
//                 }
//             }
//             DocEntry::DetachedDoc(docs) => {
//                 markdown_to_html(
//                     &mut buf,
//                     all_exposed_symbols,
//                     &module.scope,
//                     docs,
//                     root_module,
//                 );
//             }
//         };
//     }

//     buf
// }

// fn push_html(buf: &mut String, tag_name: &str, attrs: Vec<(&str, &str)>, content: impl AsRef<str>) {
//     buf.push('<');
//     buf.push_str(tag_name);

//     for (key, value) in &attrs {
//         buf.push(' ');
//         buf.push_str(key);
//         buf.push_str("=\"");
//         buf.push_str(value);
//         buf.push('"');
//     }

//     if !&attrs.is_empty() {
//         buf.push(' ');
//     }

//     buf.push('>');

//     buf.push_str(content.as_ref());

//     buf.push_str("</");
//     buf.push_str(tag_name);
//     buf.push('>');
// }

// fn base_url() -> String {
//     // e.g. "builtins/" in "https://roc-lang.org/builtins/Str"
//     //
//     // TODO make this a CLI flag to the `docs` subcommand instead of an env var
//     match std::env::var("ROC_DOCS_URL_ROOT") {
//         Ok(root_builtins_path) => {
//             let mut url_str = String::with_capacity(root_builtins_path.len() + 64);

//             if !root_builtins_path.starts_with('/') {
//                 url_str.push('/');
//             }

//             url_str.push_str(&root_builtins_path);

//             if !root_builtins_path.ends_with('/') {
//                 url_str.push('/');
//             }

//             url_str
//         }
//         _ => {
//             let mut url_str = String::with_capacity(64);

//             url_str.push('/');

//             url_str
//         }
//     }
// }

// // TODO render version as well
// fn render_name_link(name: &str) -> String {
//     let mut buf = String::new();

//     push_html(&mut buf, "h1", vec![("class", "pkg-full-name")], {
//         let mut link_buf = String::new();

//         // link to root (= docs overview page)
//         push_html(
//             &mut link_buf,
//             "a",
//             vec![("href", base_url().as_str())],
//             name,
//         );

//         link_buf
//     });

//     buf
// }

// fn render_sidebar<'a, I: Iterator<Item = &'a ModuleDocumentation>>(modules: I) -> String {
//     let mut buf = String::new();

//     for module in modules {
//         let href = module.name.as_str();
//         let mut sidebar_entry_content = String::new();

//         push_html(
//             &mut sidebar_entry_content,
//             "a",
//             vec![("class", "sidebar-module-link"), ("href", href)],
//             module.name.as_str(),
//         );

//         let entries = {
//             let mut entries_buf = String::new();

//             for entry in &module.entries {
//                 if let DocEntry::DocDef(doc_def) = entry {
//                     if module.exposed_symbols.contains(&doc_def.symbol) {
//                         let mut entry_href = String::new();

//                         entry_href.push_str(href);
//                         entry_href.push('#');
//                         entry_href.push_str(doc_def.name.as_str());

//                         push_html(
//                             &mut entries_buf,
//                             "a",
//                             vec![("href", entry_href.as_str())],
//                             doc_def.name.as_str(),
//                         );
//                     }
//                 }
//             }

//             entries_buf
//         };

//         push_html(
//             &mut sidebar_entry_content,
//             "div",
//             vec![("class", "sidebar-sub-entries")],
//             entries.as_str(),
//         );

//         push_html(
//             &mut buf,
//             "div",
//             vec![("class", "sidebar-entry")],
//             sidebar_entry_content.as_str(),
//         );
//     }

//     buf
// }

pub fn load_module_for_docs(filename: PathBuf) -> LoadedModule {
    let arena = Bump::new();
    let load_config = LoadConfig {
        target_info: roc_target::TargetInfo::default_x86_64(), // This is just type-checking for docs, so "target" doesn't matter
        function_kind: roc_solve::FunctionKind::LambdaSet,
        render: roc_reporting::report::RenderTarget::ColorTerminal,
        palette: roc_reporting::report::DEFAULT_PALETTE,
        threading: Threading::AllAvailable,
        exec_mode: ExecutionMode::Check,
    };
    match roc_load::load_and_typecheck(
        &arena,
        filename,
        RocCacheDir::Persistent(cache::roc_cache_dir().as_path()),
        load_config,
    ) {
        Ok(loaded) => loaded,
        Err(LoadingProblem::FormattedReport(report)) => {
            eprintln!("{report}");
            std::process::exit(1);
        }
        Err(e) => panic!("{e:?}"),
    }
}

// const INDENT: &str = "    ";

// fn indent(buf: &mut String, times: usize) {
//     for _ in 0..times {
//         buf.push_str(INDENT);
//     }
// }

// fn new_line(buf: &mut String) {
//     buf.push('\n');
// }

// // html is written to buf
// fn type_annotation_to_html(
//     indent_level: usize,
//     buf: &mut String,
//     type_ann: &TypeAnnotation,
//     needs_parens: bool,
// ) {
//     let is_multiline = should_be_multiline(type_ann);
//     match type_ann {
//         TypeAnnotation::TagUnion { tags, extension } => {
//             if tags.is_empty() {
//                 buf.push_str("[]");
//             } else {
//                 let tags_len = tags.len();

//                 let tag_union_indent = indent_level + 1;

//                 if is_multiline {
//                     new_line(buf);

//                     indent(buf, tag_union_indent);
//                 }

//                 buf.push('[');

//                 if is_multiline {
//                     new_line(buf);
//                 }

//                 let next_indent_level = tag_union_indent + 1;

//                 for (index, tag) in tags.iter().enumerate() {
//                     if is_multiline {
//                         indent(buf, next_indent_level);
//                     }

//                     buf.push_str(tag.name.as_str());

//                     for type_value in &tag.values {
//                         buf.push(' ');
//                         type_annotation_to_html(next_indent_level, buf, type_value, true);
//                     }

//                     if is_multiline {
//                         if index < (tags_len - 1) {
//                             buf.push(',');
//                         }

//                         new_line(buf);
//                     }
//                 }

//                 if is_multiline {
//                     indent(buf, tag_union_indent);
//                 }

//                 buf.push(']');
//             }

//             type_annotation_to_html(indent_level, buf, extension, true);
//         }
//         TypeAnnotation::BoundVariable(var_name) => {
//             buf.push_str(var_name);
//         }
//         TypeAnnotation::Apply { name, parts } => {
//             if parts.is_empty() {
//                 buf.push_str(name);
//             } else {
//                 if needs_parens {
//                     buf.push('(');
//                 }

//                 buf.push_str(name);
//                 for part in parts {
//                     buf.push(' ');
//                     type_annotation_to_html(indent_level, buf, part, true);
//                 }

//                 if needs_parens {
//                     buf.push(')');
//                 }
//             }
//         }
//         TypeAnnotation::Record { fields, extension } => {
//             if fields.is_empty() {
//                 buf.push_str("{}");
//             } else {
//                 let fields_len = fields.len();
//                 let record_indent = indent_level + 1;

//                 if is_multiline {
//                     new_line(buf);
//                     indent(buf, record_indent);
//                 }

//                 buf.push('{');

//                 if is_multiline {
//                     new_line(buf);
//                 }

//                 let next_indent_level = record_indent + 1;

//                 for (index, field) in fields.iter().enumerate() {
//                     if is_multiline {
//                         indent(buf, next_indent_level);
//                     } else {
//                         buf.push(' ');
//                     }

//                     let fields_name = match field {
//                         RecordField::RecordField { name, .. } => name,
//                         RecordField::OptionalField { name, .. } => name,
//                         RecordField::LabelOnly { name } => name,
//                     };

//                     buf.push_str(fields_name.as_str());

//                     match field {
//                         RecordField::RecordField {
//                             type_annotation, ..
//                         } => {
//                             buf.push_str(" : ");
//                             type_annotation_to_html(next_indent_level, buf, type_annotation, false);
//                         }
//                         RecordField::OptionalField {
//                             type_annotation, ..
//                         } => {
//                             buf.push_str(" ? ");
//                             type_annotation_to_html(next_indent_level, buf, type_annotation, false);
//                         }
//                         RecordField::LabelOnly { .. } => {}
//                     }

//                     if is_multiline {
//                         if index < (fields_len - 1) {
//                             buf.push(',');
//                         }

//                         new_line(buf);
//                     }
//                 }

//                 if is_multiline {
//                     indent(buf, record_indent);
//                 } else {
//                     buf.push(' ');
//                 }

//                 buf.push('}');
//             }

//             type_annotation_to_html(indent_level, buf, extension, true);
//         }
//         TypeAnnotation::Function { args, output } => {
//             let mut paren_is_open = false;
//             let mut peekable_args = args.iter().peekable();

//             while let Some(arg) = peekable_args.next() {
//                 if is_multiline {
//                     if !should_be_multiline(arg) {
//                         new_line(buf);
//                     }
//                     indent(buf, indent_level + 1);
//                 }
//                 if needs_parens && !paren_is_open {
//                     buf.push('(');
//                     paren_is_open = true;
//                 }

//                 let child_needs_parens = matches!(arg, TypeAnnotation::Function { .. });
//                 type_annotation_to_html(indent_level, buf, arg, child_needs_parens);

//                 if peekable_args.peek().is_some() {
//                     buf.push_str(", ");
//                 }
//             }

//             if is_multiline {
//                 new_line(buf);
//                 indent(buf, indent_level + 1);
//             } else {
//                 buf.push(' ');
//             }

//             buf.push_str("-> ");

//             let mut next_indent_level = indent_level;

//             if should_be_multiline(output) {
//                 next_indent_level += 1;
//             }

//             type_annotation_to_html(next_indent_level, buf, output, false);
//             if needs_parens && paren_is_open {
//                 buf.push(')');
//             }
//         }
//         TypeAnnotation::Ability { members } => {
//             buf.push_str(keyword::IMPLEMENTS);

//             for member in members {
//                 new_line(buf);
//                 indent(buf, indent_level + 1);

//                 // TODO use member.docs somehow. This doesn't look good though:
//                 // if let Some(docs) = &member.docs {
//                 //     buf.push_str("## ");
//                 //     buf.push_str(docs);

//                 //     new_line(buf);
//                 //     indent(buf, indent_level + 1);
//                 // }

//                 buf.push_str(&member.name);
//                 buf.push_str(" : ");

//                 type_annotation_to_html(indent_level + 1, buf, &member.type_annotation, false);

//                 if !member.able_variables.is_empty() {
//                     new_line(buf);
//                     indent(buf, indent_level + 2);
//                     buf.push_str(keyword::WHERE);

//                     for (index, (name, type_anns)) in member.able_variables.iter().enumerate() {
//                         if index != 0 {
//                             buf.push(',');
//                         }

//                         buf.push(' ');
//                         buf.push_str(name);
//                         buf.push(' ');
//                         buf.push_str(keyword::IMPLEMENTS);

//                         for (index, ann) in type_anns.iter().enumerate() {
//                             if index != 0 {
//                                 buf.push_str(" &");
//                             }

//                             buf.push(' ');

//                             type_annotation_to_html(indent_level + 2, buf, ann, false);
//                         }
//                     }
//                 }
//             }
//         }
//         TypeAnnotation::ObscuredTagUnion => {
//             buf.push_str("[@..]");
//         }
//         TypeAnnotation::ObscuredRecord => {
//             buf.push_str("{ @.. }");
//         }
//         TypeAnnotation::NoTypeAnn => {}
//         TypeAnnotation::Wildcard => buf.push('*'),
//         TypeAnnotation::Tuple { elems, extension } => {
//             let elems_len = elems.len();
//             let tuple_indent = indent_level + 1;

//             if is_multiline {
//                 new_line(buf);
//                 indent(buf, tuple_indent);
//             }

//             buf.push('(');

//             if is_multiline {
//                 new_line(buf);
//             }

//             let next_indent_level = tuple_indent + 1;

//             for (index, elem) in elems.iter().enumerate() {
//                 if is_multiline {
//                     indent(buf, next_indent_level);
//                 }

//                 type_annotation_to_html(next_indent_level, buf, elem, false);

//                 if is_multiline {
//                     if index < (elems_len - 1) {
//                         buf.push(',');
//                     }

//                     new_line(buf);
//                 }
//             }

//             if is_multiline {
//                 indent(buf, tuple_indent);
//             }

//             buf.push(')');

//             type_annotation_to_html(indent_level, buf, extension, true);
//         }
//         TypeAnnotation::Where { ann, implements } => {
//             type_annotation_to_html(indent_level, buf, ann, false);

//             new_line(buf);
//             indent(buf, indent_level + 1);

//             buf.push_str(keyword::WHERE);

//             let multiline_implements = implements
//                 .iter()
//                 .any(|imp| imp.abilities.iter().any(should_be_multiline));

//             for (index, imp) in implements.iter().enumerate() {
//                 if index != 0 {
//                     buf.push(',');
//                 }

//                 if multiline_implements {
//                     new_line(buf);
//                     indent(buf, indent_level + 2);
//                 } else {
//                     buf.push(' ')
//                 }

//                 buf.push_str(&imp.name);
//                 buf.push(' ');
//                 buf.push_str(keyword::IMPLEMENTS);
//                 buf.push(' ');

//                 for (index, ability) in imp.abilities.iter().enumerate() {
//                     if index != 0 {
//                         buf.push_str(" & ");
//                     }

//                     type_annotation_to_html(indent_level, buf, ability, false);
//                 }
//             }
//         }
//         TypeAnnotation::As { ann, name, vars } => {
//             type_annotation_to_html(indent_level, buf, ann, true);
//             buf.push(' ');
//             buf.push_str(name);

//             for var in vars {
//                 buf.push(' ');
//                 buf.push_str(var);
//             }
//         }
//     }
// }

// fn should_be_multiline(type_ann: &TypeAnnotation) -> bool {
//     match type_ann {
//         TypeAnnotation::TagUnion { tags, extension } => {
//             tags.len() > 1
//                 || should_be_multiline(extension)
//                 || tags
//                     .iter()
//                     .any(|tag| tag.values.iter().any(should_be_multiline))
//         }
//         TypeAnnotation::Function { args, output } => {
//             args.len() > 2 || should_be_multiline(output) || args.iter().any(should_be_multiline)
//         }
//         TypeAnnotation::ObscuredTagUnion => false,
//         TypeAnnotation::ObscuredRecord => false,
//         TypeAnnotation::BoundVariable(_) => false,
//         TypeAnnotation::Apply { parts, .. } => parts.iter().any(should_be_multiline),
//         TypeAnnotation::Record { fields, extension } => {
//             fields.len() > 1
//                 || should_be_multiline(extension)
//                 || fields.iter().any(|field| match field {
//                     RecordField::RecordField {
//                         type_annotation, ..
//                     } => should_be_multiline(type_annotation),
//                     RecordField::OptionalField {
//                         type_annotation, ..
//                     } => should_be_multiline(type_annotation),
//                     RecordField::LabelOnly { .. } => false,
//                 })
//         }
//         TypeAnnotation::Ability { .. } => true,
//         TypeAnnotation::Wildcard => false,
//         TypeAnnotation::NoTypeAnn => false,
//         TypeAnnotation::Tuple { elems, extension } => {
//             elems.len() > 1
//                 || should_be_multiline(extension)
//                 || elems.iter().any(should_be_multiline)
//         }
//         TypeAnnotation::Where { ann, implements } => {
//             should_be_multiline(ann)
//                 || implements
//                     .iter()
//                     .any(|imp| imp.abilities.iter().any(should_be_multiline))
//         }
//         TypeAnnotation::As {
//             ann,
//             name: _,
//             vars: _,
//         } => should_be_multiline(ann),
//     }
// }

// struct DocUrl {
//     url: String,
//     title: String,
// }

// fn doc_url<'a>(
//     all_exposed_symbols: &VecSet<Symbol>,
//     scope: &Scope,
//     interns: &'a Interns,
//     mut module_name: &'a str,
//     ident: &str,
// ) -> DocUrl {
//     if module_name.is_empty() {
//         // This is an unqualified lookup, so look for the ident
//         // in scope!
//         match scope.lookup_str(ident, Region::zero()) {
//             Ok(symbol) => {
//                 // Get the exact module_name from scope. It could be the
//                 // current module's name, but it also could be a different
//                 // module - for example, if this is in scope from an
//                 // unqualified import.
//                 module_name = symbol.module_string(interns);
//             }
//             Err(_) => {
//                 // TODO return Err here
//                 panic!(
//                     "Tried to generate an automatic link in docs for symbol `{ident}`, but that symbol was not in scope in this module."
//                 );
//             }
//         }
//     } else {
//         match interns.module_ids.get_id(&module_name.into()) {
//             Some(module_id) => {
//                 let symbol = interns.symbol(module_id, ident.into());

//                 if symbol.is_builtin() {
//                     // We can always generate links for builtin modules.
//                     // TODO add a `--include-builtins` CLI flag for generating offline docs locally
//                     // which include builtins; if that flag is omitted, have this code path generate
//                     // a link directly to the builtin docs on roc-lang.org instead of to a localhost
//                     // URL that will 404.
//                     module_name = symbol.module_string(interns);
//                 }
//                 // Note: You can do qualified lookups on your own module, e.g.
//                 // if I'm in the Foo module, I can do a `Foo.bar` lookup.
//                 else if !all_exposed_symbols.contains(&symbol) {
//                     // TODO return Err here
//                     panic!(
//                             "Tried to generate an automatic link in docs for `{module_name}.{ident}`, but `{module_name}` does not expose `{ident}`.");
//                 }

//                 // This is a valid symbol for this dependency,
//                 // so proceed using the current module's name.
//                 //
//                 // TODO: In the future, this is where we'll
//                 // incorporate the package name into the link.
//             }
//             None => {
//                 // TODO return Err here
//                 panic!("Tried to generate a doc link for `{module_name}.{ident}` but the `{module_name}` module was not imported!");
//             }
//         }
//     }

//     let mut url = base_url();

//     // Example:
//     //
//     // module_name: "Str", ident: "join" => "/Str#join"
//     url.push_str(module_name);
//     url.push('#');
//     url.push_str(ident);

//     DocUrl {
//         url,
//         title: format!("Docs for {module_name}.{ident}"),
//     }
// }

// fn markdown_to_html(
//     buf: &mut String,
//     all_exposed_symbols: &VecSet<Symbol>,
//     scope: &Scope,
//     markdown: &str,
//     loaded_module: &LoadedModule,
// ) {
//     use pulldown_cmark::{BrokenLink, CodeBlockKind, CowStr, Event, LinkType, Tag::*};

//     let mut arena = Bump::new();
//     let mut broken_link_callback = |link: BrokenLink| {
//         // A shortcut link - see https://spec.commonmark.org/0.30/#shortcut-reference-link -
//         // is something like `[foo]` in markdown. If you have a shortcut link
//         // without a corresponding `[foo]: https://foo.com` entry
//         // at the end of the document, we resolve it as an identifier based on
//         // what's currently in scope, so you write things like [Str.join] or
//         // [myFunction] and have them resolve to the docs for what you wrote.
//         match link.link_type {
//             LinkType::Shortcut => {
//                 let state = State::new(link.reference.as_bytes());

//                 // Reset the bump arena so we aren't constantly reallocating
//                 // more memory as we iterate through these.
//                 arena.reset();

//                 match parse_ident(&arena, state, 0) {
//                     Ok((_, Ident::Access { module_name, parts }, _)) => {
//                         let mut iter = parts.iter();

//                         match iter.next() {
//                             Some(Accessor::RecordField(symbol_name)) if iter.next().is_none() => {
//                                 let DocUrl { url, title } = doc_url(
//                                     all_exposed_symbols,
//                                     scope,
//                                     &loaded_module.interns,
//                                     module_name,
//                                     symbol_name,
//                                 );

//                                 Some((url.into(), title.into()))
//                             }
//                             _ => {
//                                 // This had record field access,
//                                 // e.g. [foo.bar] - which we
//                                 // can't create a doc link to!
//                                 None
//                             }
//                         }
//                     }
//                     Ok((_, Ident::Tag(type_name), _)) => {
//                         // This looks like a tag name, but it could
//                         // be a type alias that's in scope, e.g. [I64]
//                         let DocUrl { url, title } = doc_url(
//                             all_exposed_symbols,
//                             scope,
//                             &loaded_module.interns,
//                             "",
//                             type_name,
//                         );

//                         Some((url.into(), title.into()))
//                     }
//                     _ => None,
//                 }
//             }
//             _ => None,
//         }
//     };

//     let markdown_options =
//         pulldown_cmark::Options::ENABLE_TABLES | pulldown_cmark::Options::ENABLE_HEADING_ATTRIBUTES;

//     let mut in_code_block: Option<CowStr> = None;
//     let mut to_highlight = String::new();

//     let mut docs_parser = vec![];
//     let parser = pulldown_cmark::Parser::new_with_broken_link_callback(
//         markdown,
//         markdown_options,
//         Some(&mut broken_link_callback),
//     );

//     for event in parser {
//         match event {
//             Event::Code(code_str) => {
//                 let inline_code = pulldown_cmark::CowStr::from(format!("<code>{code_str}</code>"));
//                 docs_parser.push(pulldown_cmark::Event::Html(inline_code));
//             }
//             Event::End(Link(LinkType::ShortcutUnknown, ref _url, ref _title)) => {
//                 // Replace the preceding Text node with a Code node, so it
//                 // renders as the equivalent of [`List.len`] instead of [List.len]
//                 match docs_parser.pop() {
//                     Some(Event::Text(string)) => {
//                         docs_parser.push(Event::Code(string));
//                     }
//                     Some(first) => {
//                         docs_parser.push(first);
//                     }
//                     None => {}
//                 }

//                 docs_parser.push(event);
//             }
//             Event::Start(CodeBlock(CodeBlockKind::Fenced(code_str))) => {
//                 in_code_block = Some(code_str);
//             }
//             Event::End(CodeBlock(_)) => {
//                 match in_code_block {
//                     Some(code_str) => {
//                         if code_str.contains("unchecked") {
//                             // TODO HANDLE UNCHECKED
//                         }

//                         if code_str.contains("repl") {
//                             // TODO HANDLE REPL
//                         }

//                         // TODO HANDLE CHECKING BY DEFAULT
//                         let highlighted_html = roc_highlight::highlight_roc_code(&to_highlight);
//                         docs_parser.push(Event::Html(CowStr::from(highlighted_html)));
//                     }
//                     None => {
//                         // Indented code block

//                         let highlighted_html = roc_highlight::highlight_roc_code(&to_highlight);
//                         docs_parser.push(Event::Html(CowStr::from(highlighted_html)));
//                     }
//                 }

//                 // Reset codeblock buffer
//                 to_highlight = String::new();
//                 in_code_block = None;

//                 // Push Event::End(CodeBlock)
//                 docs_parser.push(event);
//             }
//             Event::Text(t) => {
//                 match in_code_block {
//                     Some(_) => {
//                         // If we're in a code block, build up the string of text
//                         to_highlight.push_str(&t);
//                     }
//                     None => {
//                         docs_parser.push(Event::Text(t));
//                     }
//                 }
//             }
//             Event::Html(html) => {
//                 docs_parser.push(Event::Text(html));
//             }
//             e => {
//                 docs_parser.push(e);
//             }
//         }
//     }

//     pulldown_cmark::html::push_html(buf, docs_parser.into_iter());
// }

// fn name_from_ident_id(&self, ident_id: IdentId, ident_ids: &'a IdentIds) -> &'a str {
//     ident_ids.get_name(ident_id).unwrap_or_else(|| {
//             if cfg!(debug_assertions) {
//                 unreachable!("docs generation tried to render a relative URL for IdentId {:?} but it was not found in home_identids, which should never happen!", ident_id);
//             }

//             // In release builds, don't panic, just gracefully continue
//             // by not writing the url. It'll be a bug, but at least
//             // it won't block the user from seeing *some* docs rendered.
//             ""
//         })
// }

// fn write_to_disk<'a>(
//     arena: &'a Bump,
//     opt_module_name: Option<&'a str>,
//     contents: impl AsRef<[u8]>,
// ) -> Result<(), Problem> {
//     if let Some(module_name) = opt_module_name {
//         let module_dir = build_dir.join(module_name.replace('.', "/").as_str());

//         file::create_dir_all(arena, &module_dir)?;

//         file::write(arena, &module_dir.join("index.html"), contents)
//     } else {
//         file::write(arena, &build_dir.join("index.html"), contents)
//     }
// }
