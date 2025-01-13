use bumpalo::Bump;
use roc_parse::{
    ast::{AssignedField, Collection, CommentOrNewline, ExtractSpaces, Spaces},
    expr::merge_spaces,
};
use roc_region::all::Loc;

use crate::{
    annotation::{is_collection_multiline, FieldValue, Formattable, Newlines, Parens},
    spaces::{
        count_leading_newlines, fmt_comments_only, fmt_spaces_with_newline_mode, NewlineAt,
        SpacesNewlineMode, INDENT,
    },
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

pub(crate) enum RecordPrefix<'a, Field: Formattable + std::fmt::Debug> {
    Update(&'a Loc<Field>),
    Mapper(&'a Loc<Field>),
}

pub(crate) fn fmt_record_like<'a, 'b: 'a, Field, ToSpacesAround>(
    buf: &'a mut Buf,
    prefix: Option<RecordPrefix<'b, Field>>,
    fields: Collection<'b, Loc<AssignedField<'b, Field>>>,
    indent: u16,
    to_space_around: ToSpacesAround,
) where
    Field: Formattable + FieldValue + std::fmt::Debug,
    ToSpacesAround:
        Fn(&'a Bump, &'b AssignedField<'b, Field>) -> Spaces<'a, AssignedField<'b, Field>>,
{
    let loc_fields = fields.items;
    let final_comments = fields.final_comments();
    buf.indent(indent);
    if loc_fields.is_empty() && final_comments.is_empty() && prefix.is_none() {
        buf.push_str("{}");
    } else {
        buf.push('{');

        match prefix {
            None => {}
            // We are presuming this to be a Var()
            // If it wasnt a Var() we would not have made
            // it this far. For example "{ 4 & hello = 9 }"
            // doesnt make sense.
            Some(RecordPrefix::Update(record_var)) => {
                buf.spaces(1);
                record_var.format(buf, indent + INDENT);
                buf.indent(indent + INDENT);
                buf.ensure_ends_with_whitespace();
                buf.push_str("&");
            }
            Some(RecordPrefix::Mapper(mapper_var)) => {
                buf.spaces(1);
                mapper_var.format(buf, indent + INDENT);
                buf.indent(indent + INDENT);
                buf.ensure_ends_with_whitespace();
                buf.push_str("<-");
            }
        }

        let is_multiline = loc_fields.iter().any(|loc_field| loc_field.is_multiline())
            || !final_comments.is_empty();

        if is_multiline {
            let field_indent = indent + INDENT;

            let mut last_after: &[CommentOrNewline<'_>] = &[];

            for (iter, field) in loc_fields.iter().enumerate() {
                // comma addition is handled by the `format_field_multiline` function
                // since we can have stuff like:
                // { x # comment
                // , y
                // }
                // In this case, we have to move the comma before the comment.

                let field_lifted = to_space_around(buf.text.bump(), &field.value);

                let before = merge_spaces(buf.text.bump(), last_after, field_lifted.before);

                if iter == 0 || count_leading_newlines(before.iter()) == 0 {
                    buf.ensure_ends_with_newline();
                }

                let newline_mode = if iter == 0 {
                    if loc_fields.len() == 1 {
                        SpacesNewlineMode::SkipNewlinesAtBoth
                    } else {
                        SpacesNewlineMode::SkipNewlinesAtStart
                    }
                } else {
                    SpacesNewlineMode::Normal
                };

                fmt_spaces_with_newline_mode(buf, before, field_indent, newline_mode);
                field_lifted.item.format_with_options(
                    buf,
                    Parens::NotNeeded,
                    Newlines::No,
                    field_indent,
                );
                buf.indent(field_indent);
                buf.push_str(",");
                last_after = field_lifted.after;
            }

            let after = merge_spaces(buf.text.bump(), last_after, final_comments);

            if count_leading_newlines(after.iter()) == 0 {
                buf.ensure_ends_with_newline();
            }

            fmt_spaces_with_newline_mode(
                buf,
                after,
                field_indent,
                SpacesNewlineMode::SkipNewlinesAtEnd,
            );

            buf.ensure_ends_with_newline();
        } else {
            // is_multiline == false
            buf.spaces(1);
            let field_indent = indent;
            let mut iter = loc_fields.iter().peekable();
            while let Some(field) = iter.next() {
                field.format_with_options(buf, Parens::NotNeeded, Newlines::No, field_indent);

                if iter.peek().is_some() {
                    buf.push_str(",");
                    buf.spaces(1);
                }
            }
            buf.spaces(1);
            // if we are here, that means that `final_comments` is empty, thus we don't have
            // to add a comment. Anyway, it is not possible to have a single line record with
            // a comment in it.
        };

        // closes the initial bracket
        buf.indent(indent);
        buf.push('}');
    }
}
