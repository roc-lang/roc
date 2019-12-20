use crate::parse::ast::CommentOrNewline;
use bumpalo::collections::String;

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

    let mut consecutive_newlines = 0;
    let mut iter = spaces.peekable();

    while let Some(space) = iter.next() {
        match space {
            Newline => {
                // Only ever print two newlines back to back.
                // (Two newlines renders as one blank line.)
                if consecutive_newlines < 2 {
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
                fmt_comment(buf, comment, indent);

                // Reset to 1 because we just printed a \n
                consecutive_newlines = 1;
            }
        }
    }
}

/// Like format_spaces, but remove newlines and keep only comments.
pub fn fmt_comments_only<'a, I>(buf: &mut String<'a>, spaces: I, indent: u16)
where
    I: Iterator<Item = &'a CommentOrNewline<'a>>,
{
    use self::CommentOrNewline::*;

    for space in spaces {
        match space {
            Newline => {}
            LineComment(comment) => {
                fmt_comment(buf, comment, indent);
            }
        }
    }
}

fn fmt_comment<'a>(buf: &mut String<'a>, comment: &'a str, indent:u16) {
    buf.push('#');
    buf.push_str(comment);

    newline(buf, indent);
}