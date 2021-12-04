use bumpalo::collections::String;
use roc_parse::ast::Collection;

use crate::{
    annotation::{Formattable, Newlines, Parens},
    spaces::{fmt_comments_only, newline, NewlineAt, INDENT},
};

pub struct CollectionConfig {
    pub begin: char,
    pub end: char,
    pub delimiter: char,
}

pub fn fmt_collection<'a, F: Formattable<'a>>(
    buf: &mut String<'a>,
    items: Collection<'_, F>,
    indent: u16,
    config: CollectionConfig,
) {
    let loc_items = items.items;
    let final_comments = items.final_comments();
    buf.push(config.begin);
    if !loc_items.is_empty() || !final_comments.iter().all(|c| c.is_newline()) {
        let is_multiline = loc_items.iter().any(|item| item.is_multiline());
        if is_multiline {
            let item_indent = indent + INDENT;
            for item in loc_items.iter() {
                newline(buf, item_indent);
                item.format_with_options(buf, Parens::NotNeeded, Newlines::Yes, item_indent);
                buf.push(config.delimiter);
            }
            fmt_comments_only(buf, final_comments.iter(), NewlineAt::Top, item_indent);
            newline(buf, indent);
        } else {
            // is_multiline == false
            let mut iter = loc_items.iter().peekable();
            while let Some(item) = iter.next() {
                buf.push(' ');
                item.format_with_options(buf, Parens::NotNeeded, Newlines::Yes, indent);
                if iter.peek().is_some() {
                    buf.push(config.delimiter);
                }
            }
            buf.push(' ');
        }
    }
    buf.push(config.end);
}
