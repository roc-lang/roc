use roc_parse::{
    ast::{Collection, CommentOrNewline, ExtractSpaces},
    expr::merge_spaces,
};

use crate::{
    annotation::{is_collection_multiline, Formattable, Newlines},
    spaces::{fmt_comments_only, NewlineAt, INDENT},
    Buf,
};
#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum Braces {
    Round,
    Square,
    Curly,
}

impl Braces {
    pub fn start(self) -> char {
        match self {
            Braces::Round => '(',
            Braces::Curly => '{',
            Braces::Square => '[',
        }
    }

    pub fn end(self) -> char {
        match self {
            Braces::Round => ')',
            Braces::Curly => '}',
            Braces::Square => ']',
        }
    }
}

pub fn fmt_collection<'a, 'buf, T: ExtractSpaces<'a> + Formattable + std::fmt::Debug>(
    buf: &mut Buf<'buf>,

    indent: u16,
    braces: Braces,
    items: Collection<'a, T>,
    newline: Newlines,
) where
    <T as ExtractSpaces<'a>>::Item: Formattable + std::fmt::Debug,
{
    let start = braces.start();
    let end = braces.end();

    if is_collection_multiline(&items) {
        let braces_indent = indent;
        let item_indent = braces_indent + INDENT;
        if newline == Newlines::Yes {
            buf.ensure_ends_with_newline();
        }
        buf.indent(braces_indent);
        buf.push(start);

        let mut last_after: &[CommentOrNewline<'_>] = &[];

        for (index, item) in items.iter().enumerate() {
            let is_first_item = index == 0;
            let item = item.extract_spaces();
            let is_only_newlines = item.before.iter().all(|s| s.is_newline());
            let last_after_was_only_newlines = last_after.iter().all(|s| s.is_newline());

            if !last_after.is_empty() {
                if last_after.iter().any(|s| s.is_newline()) {
                    buf.newline();
                }

                fmt_comments_only(buf, last_after.iter(), NewlineAt::None, item_indent);
            }

            if item.before.is_empty() || is_only_newlines {
                buf.ensure_ends_with_newline();
            } else {
                if is_first_item {
                    // The first item in a multiline collection always begins with exactly
                    // one newline (so the delimiter is at the end of its own line),
                    // and that newline appears before the first comment (if there is one).
                    buf.ensure_ends_with_newline();
                } else {
                    if item.before.starts_with(&[CommentOrNewline::Newline]) {
                        buf.ensure_ends_with_newline();
                    }

                    if item
                        .before
                        .starts_with(&[CommentOrNewline::Newline, CommentOrNewline::Newline])
                        && last_after_was_only_newlines
                    {
                        // If there's a comment, and it's not on the first item,
                        // and it's preceded by at least one blank line, maintain 1 blank line.
                        // (We already ensured that it ends in a newline, so this will turn that
                        // into a blank line.)

                        buf.ensure_ends_with_blank_line();
                    }
                }

                fmt_comments_only(buf, item.before.iter(), NewlineAt::None, item_indent);

                if !is_only_newlines {
                    if item.before.ends_with(&[CommentOrNewline::Newline]) {
                        buf.newline();
                    }

                    buf.newline();
                }
            }

            buf.indent(item_indent);
            item.item.format(buf, item_indent);

            buf.indent(item_indent);
            buf.push(',');

            last_after = item.after;
        }

        let final_comments = if !last_after.is_empty() {
            if last_after.iter().any(|s| s.is_newline()) {
                buf.newline();
            }

            merge_spaces(buf.text.bump(), last_after, items.final_comments())
        } else {
            if items.final_comments().iter().any(|s| s.is_newline()) {
                buf.newline();
            }

            items.final_comments()
        };

        if has_comments(final_comments)
            && final_comments.starts_with(&[CommentOrNewline::Newline, CommentOrNewline::Newline])
        {
            buf.ensure_ends_with_blank_line();
        }

        fmt_comments_only(buf, final_comments.iter(), NewlineAt::None, item_indent);

        buf.ensure_ends_with_newline();
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

fn has_comments(spaces: &[CommentOrNewline<'_>]) -> bool {
    for space in spaces {
        match space {
            CommentOrNewline::Newline => {}
            CommentOrNewline::LineComment(_) | CommentOrNewline::DocComment(_) => return true,
        }
    }
    false
}
