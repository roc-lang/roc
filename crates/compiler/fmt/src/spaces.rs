use bumpalo::{collections::Vec, Bump};
use roc_parse::ast::CommentOrNewline;

use crate::Buf;

/// The number of spaces to indent.
pub const INDENT: u16 = 4;

pub fn fmt_default_spaces(buf: &mut Buf, spaces: &[CommentOrNewline], indent: u16) {
    if spaces.is_empty() {
        buf.spaces(1);
    } else {
        fmt_spaces(buf, spaces.iter(), indent);
    }
}
pub fn fmt_default_newline(buf: &mut Buf, spaces: &[CommentOrNewline], indent: u16) {
    if spaces.is_empty() {
        buf.newline();
    } else {
        fmt_spaces(buf, spaces.iter(), indent);
    }
}

pub enum SpacesNewlineMode {
    Normal,
    SkipNewlinesAtStart,
    SkipNewlinesAtEnd,
    SkipNewlinesAtBoth,
}

pub fn fmt_spaces_with_newline_mode(
    buf: &mut Buf<'_>,
    mut spaces: &[CommentOrNewline<'_>],
    indent: u16,
    mode: SpacesNewlineMode,
) {
    if matches!(
        mode,
        SpacesNewlineMode::SkipNewlinesAtStart | SpacesNewlineMode::SkipNewlinesAtBoth
    ) {
        let skip_count = spaces
            .iter()
            .take_while(|s| *s == &CommentOrNewline::Newline)
            .count();
        spaces = &spaces[skip_count..];
    }
    if matches!(
        mode,
        SpacesNewlineMode::SkipNewlinesAtEnd | SpacesNewlineMode::SkipNewlinesAtBoth
    ) {
        let skip_count = spaces
            .iter()
            .rev()
            .take_while(|s| *s == &CommentOrNewline::Newline)
            .count();
        spaces = &spaces[..spaces.len() - skip_count];
    }
    fmt_spaces(buf, spaces.iter(), indent);
}

/// Like fmt_spaces, but disallows two consecutive newlines.
pub fn fmt_spaces_no_blank_lines<'a, 'buf, I>(buf: &mut Buf<'buf>, spaces: I, indent: u16)
where
    I: Iterator<Item = &'a CommentOrNewline<'a>>,
{
    fmt_spaces_max_consecutive_newlines(buf, spaces, 1, indent)
}

pub fn fmt_spaces<'a, 'buf, I>(buf: &mut Buf<'buf>, spaces: I, indent: u16)
where
    I: Iterator<Item = &'a CommentOrNewline<'a>>,
{
    fmt_spaces_max_consecutive_newlines(buf, spaces, 2, indent)
}

fn fmt_spaces_max_consecutive_newlines<'a, 'buf, I>(
    buf: &mut Buf<'buf>,
    spaces: I,
    max_consecutive_newlines: usize,
    indent: u16,
) where
    I: Iterator<Item = &'a CommentOrNewline<'a>>,
{
    use self::CommentOrNewline::*;

    // Only ever print two newlines back to back.
    // (Two newlines renders as one blank line.)
    let mut consecutive_newlines = 0;

    for space in spaces {
        match space {
            Newline => {
                if consecutive_newlines < max_consecutive_newlines {
                    buf.newline();

                    // Don't bother incrementing it if we're already over the limit.
                    // There's no upside, and it might eventually overflow.
                    consecutive_newlines += 1;
                }
            }
            LineComment(comment) => {
                buf.indent(indent);
                fmt_comment(buf, comment);
                buf.newline();

                consecutive_newlines = 1;
            }
            DocComment(docs) => {
                buf.indent(indent);
                fmt_docs(buf, docs);
                buf.newline();

                consecutive_newlines = 1;
            }
        }
    }
}

#[derive(Eq, PartialEq, Debug)]
pub enum NewlineAt {
    Top,
    Bottom,
    Both,
    None,
}

/// Like format_spaces, but remove newlines and keep only comments.
/// The `new_line_at` argument describes how new lines should be inserted
/// at the beginning or at the end of the block
/// in the case of there is some comment in the `spaces` argument.
pub fn fmt_comments_only<'a, 'buf, I>(
    buf: &mut Buf<'buf>,
    spaces: I,
    new_line_at: NewlineAt,
    indent: u16,
) where
    I: Iterator<Item = &'a CommentOrNewline<'a>>,
{
    use self::CommentOrNewline::*;
    use NewlineAt::*;

    let mut comment_seen = false;

    for space in spaces {
        match space {
            Newline => {}
            LineComment(comment) => {
                if comment_seen || new_line_at == Top || new_line_at == Both {
                    buf.newline();
                }
                buf.indent(indent);
                fmt_comment(buf, comment);
                comment_seen = true;
            }
            DocComment(docs) => {
                if comment_seen || new_line_at == Top || new_line_at == Both {
                    buf.newline();
                }
                buf.indent(indent);
                fmt_docs(buf, docs);
                comment_seen = true;
            }
        }
    }
    if comment_seen && (new_line_at == Bottom || new_line_at == Both) {
        buf.newline();
    }
}

fn fmt_comment(buf: &mut Buf, comment: &str) {
    // Format shebangs without whitespace. We look for " !" as well to fix incorrect formatting from
    // the past.
    if buf.is_empty() && (comment.starts_with('!') || comment.starts_with(" !")) {
        buf.push('#');
        buf.push_str(comment.trim());
        return;
    }

    // The '#' in a comment should always be preceded by a newline or a space,
    // unless it's the very beginning of the buffer.
    if !buf.is_empty() && !buf.ends_with_space() && !buf.ends_with_newline() {
        buf.spaces(1);
    }

    buf.push('#');
    // Add a space between the starting `#` and the rest of the comment,
    // unless there already is a space or the comment is of the form `#### something`.
    if !comment.starts_with(' ') && !comment.starts_with('#') {
        buf.spaces(1);
    }
    buf.push_str(comment.trim_end());
}

pub fn count_leading_newlines<'a, I>(data: I) -> u16
where
    I: Iterator<Item = &'a CommentOrNewline<'a>>,
{
    let mut count = 0;
    let mut allow_counting = false;

    for (index, val) in data.enumerate() {
        let is_first = index == 0;
        let is_newline = matches!(val, CommentOrNewline::Newline);

        if is_first && is_newline {
            allow_counting = true
        }

        if is_newline && allow_counting {
            count += 1;
        } else {
            break;
        }
    }

    count
}

pub fn merge_spaces_conservative<'a>(
    arena: &'a Bump,
    a: &'a [CommentOrNewline<'a>],
    b: &'a [CommentOrNewline<'a>],
) -> &'a [CommentOrNewline<'a>] {
    if a.is_empty() {
        b
    } else if b.is_empty() {
        a
    } else {
        let mut merged = Vec::with_capacity_in(a.len() + b.len(), arena);
        merged.extend_from_slice(a);
        let mut it = b.iter();
        for item in it.by_ref() {
            if item.is_comment() {
                merged.push(*item);
                break;
            }
        }
        merged.extend(it);
        merged.into_bump_slice()
    }
}

fn fmt_docs(buf: &mut Buf, docs: &str) {
    // The "##" in a doc comment should always be preceded by a newline or a space,
    // unless it's the very beginning of the buffer.
    if !buf.is_empty() && !buf.ends_with_space() && !buf.ends_with_newline() {
        buf.spaces(1);
    }

    buf.push_str("##");
    if !docs.is_empty() {
        buf.spaces(1);
    }
    buf.push_str(docs.trim_end());
}
