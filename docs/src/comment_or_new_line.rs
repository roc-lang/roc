use crate::html::ToHtml;
use roc_parse::ast::CommentOrNewline;

impl<'a> ToHtml<'a> for CommentOrNewline<'a> {
    fn css_class(&self) -> Option<&'a str> {
        match self {
            CommentOrNewline::Newline => None,
            CommentOrNewline::LineComment(_) => Some("comment"),
            CommentOrNewline::DocComment(_) => Some("comment"),
        }
    }
    fn html_body(&self, buf: &mut bumpalo::collections::String<'a>) {
        match self {
            CommentOrNewline::Newline => {
                buf.push('\n');
            }
            CommentOrNewline::LineComment(comment) => {
                buf.push('#');
                buf.push_str(comment);
                buf.push('\n');
            }
            CommentOrNewline::DocComment(comment) => {
                buf.push_str("##");
                buf.push_str(comment);
                buf.push('\n');
            }
        }
    }
}
