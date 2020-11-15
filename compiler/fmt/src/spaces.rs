use bumpalo::collections::String;
use roc_parse::ast::CommentOrNewline;

/// The number of spaces to indent.
pub const INDENT: u16 = 4;

pub fn newline<'a>(buf: &mut String<'a>, indent: u16) {
    buf.push('\n');

    add_spaces(buf, indent);
}

pub fn add_spaces<'a>(buf: &mut String<'a>, spaces: u16) {
    for _ in 0..spaces {
        buf.push(' ');
    }
}

pub fn fmt_spaces<'a, I>(buf: &mut String<'a>, spaces: I, indent: u16)
where
    I: Iterator<Item = &'a CommentOrNewline<'a>>,
{
    use self::CommentOrNewline::*;

    // Only ever print two newlines back to back.
    // (Two newlines renders as one blank line.)
    let mut consecutive_newlines = 0;
    let mut iter = spaces.peekable();

    let mut encountered_comment = false;

    while let Some(space) = iter.next() {
        match space {
            Newline => {
                if !encountered_comment && (consecutive_newlines < 2) {
                    if iter.peek() == Some(&&Newline) {
                        buf.push('\n');
                    } else {
                        newline(buf, indent);
                    }

                    // Don't bother incrementing it if we're already over the limit.
                    // There's no upside, and it might eventually overflow,
                    consecutive_newlines += 1;
                }
            }
            LineComment(comment) => {
                fmt_comment(buf, comment);
                newline(buf, indent);

                encountered_comment = true;
            }
            DocComment(docs) => {
                fmt_docs(buf, docs);
                newline(buf, indent);

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
pub fn fmt_comments_only<'a, I>(
    buf: &mut String<'a>,
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
                    newline(buf, indent);
                }
                fmt_comment(buf, comment);
                comment_seen = true;
            }
            DocComment(docs) => {
                if comment_seen || new_line_at == Top || new_line_at == Both {
                    newline(buf, indent);
                }
                fmt_docs(buf, docs);
                comment_seen = true;
            }
        }
    }
    if comment_seen && (new_line_at == Bottom || new_line_at == Both) {
        newline(buf, indent);
    }
}

fn fmt_comment<'a>(buf: &mut String<'a>, comment: &'a str) {
    buf.push('#');
    if !comment.starts_with(" ") {
        buf.push(' ');
    }
    buf.push_str(comment);
}

fn fmt_docs<'a>(buf: &mut String<'a>, docs: &'a str) {
    buf.push_str("##");
    if !docs.starts_with(" ") {
        buf.push(' ');
    }
    buf.push_str(docs);
}
