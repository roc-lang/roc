use roc_parse::ast::CommentOrNewline;

use crate::Buf;

/// The number of spaces to indent.
pub const INDENT: u16 = 4;

pub fn fmt_default_spaces<'a, 'buf>(
    buf: &mut Buf<'buf>,
    spaces: &[CommentOrNewline<'a>],
    indent: u16,
) {
    if spaces.is_empty() {
        buf.spaces(1);
    } else {
        fmt_spaces(buf, spaces.iter(), indent);
    }
}

pub fn fmt_spaces<'a, 'buf, I>(buf: &mut Buf<'buf>, spaces: I, indent: u16)
where
    I: Iterator<Item = &'a CommentOrNewline<'a>>,
{
    use self::CommentOrNewline::*;

    // Only ever print two newlines back to back.
    // (Two newlines renders as one blank line.)
    let mut consecutive_newlines = 0;

    let mut encountered_comment = false;

    for space in spaces {
        match space {
            Newline => {
                if !encountered_comment && (consecutive_newlines < 2) {
                    buf.newline();

                    // Don't bother incrementing it if we're already over the limit.
                    // There's no upside, and it might eventually overflow,
                    consecutive_newlines += 1;
                }
            }
            LineComment(comment) => {
                buf.indent(indent);
                fmt_comment(buf, comment);
                buf.newline();

                encountered_comment = true;
            }
            DocComment(docs) => {
                buf.indent(indent);
                fmt_docs(buf, docs);
                buf.newline();

                encountered_comment = true;
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

fn fmt_comment<'buf>(buf: &mut Buf<'buf>, comment: &str) {
    buf.push('#');
    if !comment.starts_with(' ') {
        buf.spaces(1);
    }
    buf.push_str(comment.trim_end());
}

fn fmt_docs<'buf>(buf: &mut Buf<'buf>, docs: &str) {
    buf.push_str("##");
    if !docs.starts_with(' ') {
        buf.spaces(1);
    }
    buf.push_str(docs);
}
