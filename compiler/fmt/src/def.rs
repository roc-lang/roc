use crate::annotation::{fmt_annotation, Formattable, Newlines, Parens};
use crate::expr::fmt_expr;
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

            TypedBody(_loc_pattern, _loc_annotation, _loc_expr) => {
                unreachable!("annotations and bodies have not yet been merged into TypedBody");
            }

            SpaceBefore(sub_def, spaces) | SpaceAfter(sub_def, spaces) => {
                spaces.iter().any(|s| is_comment(s)) || sub_def.is_multiline()
            }
            Nested(def) => def.is_multiline(),
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
            TypedBody(_loc_pattern, _loc_annotation, _loc_expr) => {
                unreachable!("annotations and bodies have not yet been merged into TypedBody");
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
    fmt_pattern(buf, pattern, indent, Parens::InApply);
    buf.push_str(" =");
    if body.is_multiline() {
        match body {
            Expr::Record { .. } | Expr::List(_) => {
                newline(buf, indent + INDENT);
                fmt_expr(buf, body, indent + INDENT, false, true);
            }
            _ => {
                buf.push(' ');
                fmt_expr(buf, body, indent, false, true);
            }
        }
    } else {
        buf.push(' ');
        fmt_expr(buf, body, indent, false, true);
    }
}
