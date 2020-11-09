use crate::annotation::{Formattable, Newlines, Parens};
use crate::pattern::fmt_pattern;
use crate::spaces::{fmt_spaces, is_comment, newline, INDENT};
use bumpalo::collections::String;
use roc_parse::ast::{Def, Expr, Pattern};

/// A Located formattable value is also formattable
impl<'a> Formattable<'a> for Def<'a> {
    fn is_multiline(&self) -> bool {
        use roc_parse::ast::Def::*;

        match self {
            Alias { ann, .. } => ann.is_multiline(),
            Annotation(loc_pattern, loc_annotation) => {
                loc_pattern.is_multiline() || loc_annotation.is_multiline()
            }
            Body(loc_pattern, loc_expr) => loc_pattern.is_multiline() || loc_expr.is_multiline(),
            AnnotatedBody { .. } => true,
            SpaceBefore(sub_def, spaces) | SpaceAfter(sub_def, spaces) => {
                spaces.iter().any(|s| is_comment(s)) || sub_def.is_multiline()
            }
            Nested(def) => def.is_multiline(),
            NotYetImplemented(s) => todo!("{}", s),
        }
    }

    fn format_with_options(
        &self,
        buf: &mut String<'a>,
        _parens: Parens,
        _newlines: Newlines,
        indent: u16,
    ) {
        use roc_parse::ast::Def::*;

        match self {
            Annotation(loc_pattern, loc_annotation) => {
                loc_pattern.format(buf, indent);
                buf.push_str(" : ");
                loc_annotation.format(buf, indent);
            }
            Alias { name, vars, ann } => {
                buf.push_str(name.value);

                if vars.is_empty() {
                    buf.push(' ');
                } else {
                    for var in *vars {
                        buf.push(' ');
                        fmt_pattern(buf, &var.value, indent, Parens::NotNeeded);
                    }
                }

                buf.push_str(" : ");

                ann.format(buf, indent)
            }
            Body(loc_pattern, loc_expr) => {
                fmt_body(buf, &loc_pattern.value, &loc_expr.value, indent);
            }
            AnnotatedBody {
                ann_pattern,
                ann_type,
                comment,
                body_pattern,
                body_expr,
            } => {
                ann_pattern.format(buf, indent);
                buf.push_str(" : ");
                ann_type.format(buf, indent);
                if let Some(comment_str) = comment {
                    buf.push_str(" # ");
                    buf.push_str(comment_str.trim());
                }
                buf.push_str("\n");
                fmt_body(buf, &body_pattern.value, &body_expr.value, indent);
            }

            SpaceBefore(sub_def, spaces) => {
                fmt_spaces(buf, spaces.iter(), indent);
                sub_def.format(buf, indent);
            }
            SpaceAfter(sub_def, spaces) => {
                sub_def.format(buf, indent);
                fmt_spaces(buf, spaces.iter(), indent);
            }
            Nested(def) => def.format(buf, indent),
            NotYetImplemented(s) => todo!("{}", s),
        }
    }
}

pub fn fmt_def<'a>(buf: &mut String<'a>, def: &Def<'a>, indent: u16) {
    def.format(buf, indent);
}

pub fn fmt_body<'a>(
    buf: &mut String<'a>,
    pattern: &'a Pattern<'a>,
    body: &'a Expr<'a>,
    indent: u16,
) {
    pattern.format_with_options(buf, Parens::InApply, Newlines::No, indent);
    buf.push_str(" =");
    if body.is_multiline() {
        match body {
            Expr::SpaceBefore(_, _) => {
                body.format_with_options(buf, Parens::NotNeeded, Newlines::Yes, indent + INDENT);
            }
            Expr::Record { .. } | Expr::List(_) => {
                newline(buf, indent + INDENT);
                body.format_with_options(buf, Parens::NotNeeded, Newlines::Yes, indent + INDENT);
            }
            _ => {
                buf.push(' ');
                body.format_with_options(buf, Parens::NotNeeded, Newlines::Yes, indent);
            }
        }
    } else {
        buf.push(' ');
        body.format_with_options(buf, Parens::NotNeeded, Newlines::Yes, indent);
    }
}
