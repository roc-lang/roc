// use crate::render_doc_url::{AutolinkProblem, DocUrl, ModuleInfo};
use bumpalo::{
    collections::{String, Vec},
    Bump,
};
use pulldown_cmark::{BrokenLink, CodeBlockKind, CowStr, Event, LinkType, Tag::*};
use roc_can::scope::Scope;
use roc_module::symbol::Interns;
use roc_parse::{ident::Accessor, state::State};

pub enum LookupOrTag<'a> {
    Lookup {
        opt_module_name: Option<&'a str>,
        ident_name: &'a str,
    },
    Tag {
        tag_name: &'a str,
    },
}

pub fn markdown_to_html<'a>(
    buf_arena: &'a Bump,
    // The buffer we'll be appending to
    buf: &mut String,
    // This is used for some temporary allocations and can be reset once this function returns.
    scratch_arena: &'a Bump,
    // Needed by render_doc_url::DocUrl::new
    scope: &Scope,
    interns: &Interns,
    markdown: &str,
) {
    let mut broken_link_callback = |link: BrokenLink| {
        // A shortcut link - see https://spec.commonmark.org/0.30/#shortcut-reference-link -
        // is something like `[foo]` in markdown. If you have a shortcut link
        // without a corresponding `[foo]: https://foo.com` entry
        // at the end of the document, we resolve it as an identifier based on
        // what's currently in scope, so you write things like [Str.join] or
        // [myFunction] and have them resolve to the docs for what you wrote.
        match link.link_type {
            LinkType::Shortcut => {
                match parse_ident(buf_arena, link.reference.as_bytes()) {
                    Some(lookup_or_tag) => {
                        let todo = (); // TODO use doc_url

                        Some(("".into(), "".into()))
                        // let doc_url_result = match lookup_or_tag {
                        //     LookupOrTag::Lookup {
                        //         opt_module_name,
                        //         ident_name,
                        //     } => DocUrl::new(
                        //         scratch_arena,
                        //         opt_module_name,
                        //         ident_name,
                        //         scope,
                        //         interns,
                        //     ),
                        //     LookupOrTag::Tag { tag_name } => {
                        //         DocUrl::new(scratch_arena, None, tag_name, scope, interns)
                        //     }
                        // };

                        // match doc_url_result {
                        //     Ok(DocUrl { url, title }) => Some((url.into(), title.into())),
                        //     Err(problem) => {
                        //         // TODO report the problem to stderr immediately.
                        //         // It's annoying that this callback doesn't return anything
                        //         // that would let us propagate the error, but such is the API.
                        //         None
                        //     }
                        // }
                    }
                    _ => None,
                }
            }
            _ => None,
        }
    };

    let markdown_options =
        pulldown_cmark::Options::ENABLE_TABLES | pulldown_cmark::Options::ENABLE_HEADING_ATTRIBUTES;

    let mut in_code_block: Option<CowStr> = None;
    let mut to_highlight = String::new_in(&scratch_arena);

    let parser = pulldown_cmark::Parser::new_with_broken_link_callback(
        markdown,
        markdown_options,
        Some(&mut broken_link_callback),
    );

    let mut events = Vec::with_capacity_in(parser.size_hint().1.unwrap_or_default(), scratch_arena);

    for event in parser {
        match event {
            Event::Code(code_str) => {
                let inline_code = pulldown_cmark::CowStr::from(format!("<code>{code_str}</code>"));
                events.push(pulldown_cmark::Event::Html(inline_code));
            }
            Event::End(Link(LinkType::ShortcutUnknown, ref _url, ref _title)) => {
                // Replace the preceding Text node with a Code node, so it
                // renders as the equivalent of [`List.len`] instead of [List.len]
                match events.pop() {
                    Some(Event::Text(string)) => {
                        events.push(Event::Code(string));
                    }
                    Some(first) => {
                        events.push(first);
                    }
                    None => {}
                }

                events.push(event);
            }
            Event::Start(CodeBlock(CodeBlockKind::Fenced(code_str))) => {
                in_code_block = Some(code_str);
            }
            Event::End(CodeBlock(_)) => {
                match in_code_block {
                    Some(code_str) => {
                        if code_str.contains("unchecked") {
                            // TODO HANDLE UNCHECKED
                        }

                        if code_str.contains("repl") {
                            // TODO HANDLE REPL
                        }

                        // TODO HANDLE CHECKING BY DEFAULT
                        let highlighted_html = roc_highlight::highlight_roc_code(&to_highlight);
                        events.push(Event::Html(CowStr::from(highlighted_html)));
                    }
                    None => {
                        // Indented code block

                        let highlighted_html = roc_highlight::highlight_roc_code(&to_highlight);
                        events.push(Event::Html(CowStr::from(highlighted_html)));
                    }
                }

                // Reset codeblock buffer
                to_highlight.clear();
                in_code_block = None;

                // Push Event::End(CodeBlock)
                events.push(event);
            }
            Event::Text(txt) => {
                match in_code_block {
                    Some(_) => {
                        // If we're in a code block, build up the string of text
                        to_highlight.push_str(&txt);
                    }
                    None => {
                        events.push(Event::Text(txt));
                    }
                }
            }
            Event::Html(html) => {
                events.push(Event::Text(html));
            }
            e => {
                events.push(e);
            }
        }
    }

    {
        // TODO we can avoid this std::string allocation if we do our own iteration
        // over the markdown Events and generate our own HTML that way.
        // This also gives us a lot mroe control over what we accept and what's being emitted,
        // e.g. we may not want to support footnote references.
        //
        // See https://docs.rs/pulldown-cmark/latest/src/pulldown_cmark/html.rs.html#38
        // for how this is done in pulldown_cmark - it's not very complicated.
        let mut temp_buf = std::string::String::new();

        pulldown_cmark::html::push_html(&mut temp_buf, events.into_iter());

        buf.push_str(&temp_buf);
    }
}

fn parse_ident<'a>(arena: &'a Bump, bytes: &[u8]) -> Option<LookupOrTag<'a>> {
    use roc_parse::ident::Ident;

    match roc_parse::ident::parse_ident(arena, State::new(bytes), 0)
        .ok()?
        .1
    {
        Ident::Tag(tag_name) => Some(LookupOrTag::Tag {
            tag_name: &*arena.alloc_str(tag_name),
        }),
        Ident::Access { module_name, parts } => {
            if module_name.is_empty() {
                match parts.first() {
                    Some(Accessor::RecordField(ident_name)) => Some(LookupOrTag::Lookup {
                        opt_module_name: None,
                        ident_name: &*arena.alloc_str(ident_name),
                    }),
                    Some(Accessor::TupleIndex(_)) | None => None,
                }
            } else {
                match parts.first() {
                    Some(Accessor::RecordField(ident_name)) => Some(LookupOrTag::Lookup {
                        opt_module_name: Some(&*arena.alloc_str(module_name)),
                        ident_name: &*arena.alloc_str(ident_name),
                    }),
                    Some(Accessor::TupleIndex(_)) => None,
                    None => Some(LookupOrTag::Tag {
                        tag_name: &*arena.alloc_str(module_name),
                    }),
                }
            }
        }
        Ident::OpaqueRef(_)
        | Ident::AccessorFunction(_)
        | Ident::RecordUpdaterFunction(_)
        | Ident::Malformed(_, _) => None,
    }
}

#[derive(Debug)]
pub enum LinkProblem {
    MalformedAutoLink,
    AutoLinkIdentNotInScope,
    AutoLinkNotExposed,
    AutoLinkModuleNotImported,
}

// fn doc_url<'a>(
//     arena: &'a Bump,
//     base_url: &'a str,
//     is_exposed: impl Fn(Symbol) -> bool,
//     scope: &Scope,
//     interns: &'a Interns,
//     mut module_name: &'a str,
//     ident: &'a str,
// ) -> Result<DocUrl<'a>, ((Option<&'a str>, &'a str), LinkProblem)> {
//     if module_name.is_empty() {
//         // This is an unqualified lookup, so look for the ident
//         // in scope!
//         match scope.lookup_str(ident, Region::zero()) {
//             Ok(symbol) => {
//                 // Get the exact module_name from scope. It could be the
//                 // current module's name, but it also could be a different
//                 // module - for example, if this is in scope from an
//                 // unqualified import.
//                 module_name = symbol.symbol.module_string(interns);
//             }
//             Err(_) => {
//                 return Err(((None, ident), LinkProblem::AutoLinkIdentNotInScope));
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
//                 else if !is_exposed(symbol) {
//                     return Err(((Some(module_name), ident), LinkProblem::AutoLinkNotExposed));
//                 }

//                 // This is a valid symbol for this dependency,
//                 // so proceed using the current module's name.
//                 //
//                 // TODO: In the future, this is where we'll
//                 // incorporate the package name into the link.
//             }
//             None => {
//                 return Err((
//                     (Some(module_name), ident),
//                     LinkProblem::AutoLinkModuleNotImported,
//                 ));
//             }
//         }
//     }

//     let mut url = bumpalo::collections::string::String::with_capacity_in(24, arena);

//     url.push_str(base_url);

//     // Example:
//     //
//     // module_name: "Str", ident: "join" => "/Str#join"
//     url.push_str(module_name);
//     url.push('#');
//     url.push_str(ident);

//     let mut title = bumpalo::collections::string::String::with_capacity_in(24, arena);

//     title.push_str("Docs for ");
//     title.push_str(module_name);
//     title.push('.');
//     title.push_str(ident);

//     Ok(DocUrl {
//         url: url.into_bump_str(),
//         title: title.into_bump_str(),
//     })
// }

// fn doc_url<'a>(
//     all_exposed_symbols: &VecSet<Symbol>,
//     scope: &Scope,
//     interns: &'a Interns,
//     mut module_name: &'a str,
//     ident: &str,
// ) -> Result<DocUrl, (String, LinkProblem)> {
//     if module_name.is_empty() {
//         // This is an unqualified lookup, so look for the ident
//         // in scope!
//         match scope.lookup_str(ident, Region::zero()) {
//             Ok(symbol) => {
//                 // Get the exact module_name from scope. It could be the
//                 // current module's name, but it also could be a different
//                 // module - for example, if this is in scope from an
//                 // unqualified import.
//                 module_name = symbol.symbol.module_string(interns);
//             }
//             Err(_) => {
//                 return Err((format!("[{ident}]"), LinkProblem::AutoLinkIdentNotInScope));
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
//                     return Err((
//                         format!("[{module_name}.{ident}]"),
//                         LinkProblem::AutoLinkNotExposed,
//                     ));
//                 }

//                 // This is a valid symbol for this dependency,
//                 // so proceed using the current module's name.
//                 //
//                 // TODO: In the future, this is where we'll
//                 // incorporate the package name into the link.
//             }
//             None => {
//                 return Err((
//                     format!("[{module_name}.{ident}]"),
//                     LinkProblem::AutoLinkModuleNotImported,
//                 ));
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

//     Ok(DocUrl {
//         url,
//         title: format!("Docs for {module_name}.{ident}"),
//     })
// }
