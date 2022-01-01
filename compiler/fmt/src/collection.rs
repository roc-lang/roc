use roc_parse::ast::{Collection, ExtractSpaces};

use crate::{
    annotation::{Formattable, Newlines},
    spaces::{fmt_comments_only, NewlineAt, INDENT},
    Buf,
};

pub fn fmt_collection<'a, 'buf, T: ExtractSpaces<'a> + Formattable>(
    buf: &mut Buf<'buf>,
    indent: u16,
    start: char,
    end: char,
    items: Collection<'a, T>,
    newline: Newlines,
) where
    <T as ExtractSpaces<'a>>::Item: Formattable,
{
    buf.indent(indent);
    let is_multiline =
        items.iter().any(|item| item.is_multiline()) || !items.final_comments().is_empty();

    if is_multiline {
        let braces_indent = indent;
        let item_indent = braces_indent + INDENT;
        if newline == Newlines::Yes {
            buf.newline();
        }
        buf.indent(braces_indent);
        buf.push(start);

        for item in items.iter() {
            let item = item.extract_spaces();

            buf.newline();
            if !item.before.is_empty() {
                fmt_comments_only(buf, item.before.iter(), NewlineAt::Bottom, item_indent);
            }

            item.item.format(buf, item_indent);

            buf.push(',');

            if !item.after.is_empty() {
                fmt_comments_only(buf, item.after.iter(), NewlineAt::Top, item_indent);
            }
        }
        fmt_comments_only(
            buf,
            items.final_comments().iter(),
            NewlineAt::Top,
            item_indent,
        );
        buf.newline();
    } else {
        // is_multiline == false
        // there is no comment to add
        buf.push(start);
        let mut iter = items.iter().peekable();
        while let Some(item) = iter.next() {
            buf.spaces(1);
            item.format(buf, indent);
            if iter.peek().is_some() {
                buf.push(',');
            }
        }

        if !items.is_empty() {
            buf.spaces(1);
        }
    }
    buf.indent(indent);
    buf.push(end);
}
