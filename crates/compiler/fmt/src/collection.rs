use roc_parse::ast::{Collection, ExtractSpaces};

use crate::{
    annotation::{Formattable, Newlines},
    spaces::{count_leading_newlines, fmt_comments_only, NewlineAt, INDENT},
    Buf,
};

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum Braces {
    Square,
    Curly,
}

pub fn fmt_collection<'a, 'buf, T: ExtractSpaces<'a> + Formattable>(
    buf: &mut Buf<'buf>,
    indent: u16,
    braces: Braces,
    items: Collection<'a, T>,
    newline: Newlines,
) where
    <T as ExtractSpaces<'a>>::Item: Formattable,
{
    let start = match braces {
        Braces::Curly => '{',
        Braces::Square => '[',
    };

    let end = match braces {
        Braces::Curly => '}',
        Braces::Square => ']',
    };

    if items.is_multiline() {
        let braces_indent = indent;
        let item_indent = braces_indent + INDENT;
        if newline == Newlines::Yes {
            buf.newline();
        }
        buf.indent(braces_indent);
        buf.push(start);

        for (index, item) in items.iter().enumerate() {
            let item = item.extract_spaces();
            let is_first_item = index == 0;

            buf.newline();

            if !item.before.is_empty() {
                let is_only_newlines = item.before.iter().all(|s| s.is_newline());

                if !is_first_item
                    && !is_only_newlines
                    && count_leading_newlines(item.before.iter()) > 1
                {
                    buf.newline();
                }

                fmt_comments_only(buf, item.before.iter(), NewlineAt::Bottom, item_indent);

                if !is_only_newlines && count_leading_newlines(item.before.iter().rev()) > 0 {
                    buf.newline();
                }
            }

            item.item.format(buf, item_indent);

            buf.push(',');

            if !item.after.is_empty() {
                fmt_comments_only(buf, item.after.iter(), NewlineAt::Top, item_indent);
            }
        }

        if count_leading_newlines(items.final_comments().iter()) > 1 {
            buf.newline();
        }

        fmt_comments_only(
            buf,
            items.final_comments().iter(),
            NewlineAt::Top,
            item_indent,
        );
        buf.newline();
        buf.indent(braces_indent);
    } else {
        // is_multiline == false
        // there is no comment to add
        buf.indent(indent);
        buf.push(start);
        let mut iter = items.iter().enumerate().peekable();
        while let Some((index, item)) = iter.next() {
            if braces == Braces::Curly || index != 0 {
                buf.spaces(1);
            }

            item.format(buf, indent);
            if iter.peek().is_some() {
                buf.push(',');
            }
        }

        if !items.is_empty() && braces == Braces::Curly {
            buf.spaces(1);
        }
    }

    buf.push(end);
}
