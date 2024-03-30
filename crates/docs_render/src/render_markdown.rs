use crate::render_doc_url::{AutolinkProblem, DocUrl, ModuleInfo};
use bumpalo::{
    collections::{String, Vec},
    Bump,
};
use pulldown_cmark::{BrokenLink, CodeBlockKind, CowStr, Event, LinkType, Tag::*};

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
    // TODO This arena will be needed in the future when the arena API changes
    _buf_arena: &'a Bump,
    // The buffer we'll be appending to
    buf: &'a mut String,
    // This is used for some temporary allocations and can be reset once this function returns.
    scratch_arena: &'a Bump,
    // Needed by render_doc_url::DocUrl::new
    lookup: impl Fn(Option<&'a str>, &str) -> Result<ModuleInfo<'a>, AutolinkProblem<'a>>,
    // Note: this should return None for things that are neither lookups nor tags,
    // *but also* for things that are more than lookups, e.g. record access like `foo.bar`
    parse_ident: impl Fn(&[u8]) -> Option<LookupOrTag<'a>>,
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
                match parse_ident(link.reference.as_bytes()) {
                    Some(lookup_or_tag) => {
                        let doc_url_result = match lookup_or_tag {
                            LookupOrTag::Lookup {
                                opt_module_name,
                                ident_name,
                            } => DocUrl::new(scratch_arena, opt_module_name, ident_name, &lookup),
                            LookupOrTag::Tag { tag_name } => {
                                DocUrl::new(scratch_arena, None, tag_name, &lookup)
                            }
                        };

                        match doc_url_result {
                            Ok(DocUrl { url, title }) => Some((url.into(), title.into())),
                            Err(problem) => {
                                // TODO report the problem to stderr immediately.
                                // It's annoying that this callback doesn't return anything
                                // that would let us propagate the error, but such is the API.
                                None
                            }
                        }
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
