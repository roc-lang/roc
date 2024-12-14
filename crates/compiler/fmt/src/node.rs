use bumpalo::Bump;
use roc_parse::ast::{CommentOrNewline, Spaces, TypeAnnotation};

use crate::{
    annotation::{Formattable, Newlines, Parens},
    collection::Braces,
    spaces::fmt_spaces,
    Buf,
};

pub type Sp<'a> = &'a [CommentOrNewline<'a>];

#[derive(Copy, Clone, Debug)]
pub enum Node<'a> {
    Literal(&'a str),
    Sequence(&'a Node<'a>, &'a [(Sp<'a>, Node<'a>)]),
    DelimitedSequence(Braces, &'a [(Sp<'a>, Node<'a>)], Sp<'a>),
    TypeAnnotation(TypeAnnotation<'a>),
}

pub trait Nodify<'a> {
    fn to_node<'b>(&'a self, arena: &'b Bump, parens: Parens) -> Spaces<'b, Node<'b>>
    where
        'a: 'b;
}

impl<'a> Formattable for Node<'a> {
    fn is_multiline(&self) -> bool {
        match self {
            Node::DelimitedSequence(_braces, lefts, right) => {
                right.is_empty()
                    && lefts
                        .iter()
                        .any(|(sp, l)| l.is_multiline() || !sp.is_empty())
            }
            Node::Sequence(first, rest) => {
                first.is_multiline()
                    || rest
                        .iter()
                        .any(|(sp, l)| l.is_multiline() || !sp.is_empty())
            }
            Node::TypeAnnotation(type_annotation) => type_annotation.is_multiline(),
            Node::Literal(_) => false,
        }
    }

    fn format_with_options(&self, buf: &mut Buf, parens: Parens, newlines: Newlines, indent: u16) {
        match self {
            Node::DelimitedSequence(braces, lefts, right) => {
                buf.indent(indent);
                buf.push(braces.start());

                for (sp, l) in *lefts {
                    if !sp.is_empty() {
                        fmt_spaces(buf, sp.iter(), indent);
                    }

                    l.format_with_options(buf, parens, newlines, indent);
                }

                if !right.is_empty() {
                    fmt_spaces(buf, right.iter(), indent);
                }

                buf.indent(indent);
                buf.push(braces.end());
            }
            Node::Sequence(first, rest) => {
                first.format_with_options(buf, parens, newlines, indent);

                for (sp, l) in *rest {
                    if !sp.is_empty() {
                        fmt_spaces(buf, sp.iter(), indent);
                    } else {
                        buf.spaces(1);
                    }

                    l.format_with_options(buf, parens, newlines, indent);
                }
            }
            Node::TypeAnnotation(type_annotation) => {
                type_annotation.format_with_options(buf, parens, newlines, indent);
            }
            Node::Literal(text) => {
                buf.indent(indent);
                buf.push_str(text);
            }
        }
    }
}
