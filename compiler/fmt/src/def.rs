use crate::annotation::{fmt_annotation, Parens};
use crate::expr::{fmt_expr, is_multiline_expr};
use crate::pattern::fmt_pattern;
use crate::spaces::{fmt_spaces, newline, INDENT};
use bumpalo::collections::String;
use roc_parse::ast::{Def, Expr, Pattern, TypeAnnotation};

pub fn fmt_def<'a>(buf: &mut String<'a>, def: &'a Def<'a>, indent: u16) {
    use roc_parse::ast::Def::*;

    match def {
        Annotation(loc_pattern, loc_annotation) => {
            fmt_type_annotation(buf, &loc_pattern.value, &loc_annotation.value, indent);
        }
        Alias { name, vars, ann } => {
            buf.push_str(name.value);

            if vars.is_empty() {
                buf.push(' ');
            } else {
                for var in *vars {
                    buf.push(' ');
                    fmt_pattern(buf, &var.value, indent, Parens::NotNeeded, false);
                }
            }

            buf.push_str(" : ");

            fmt_annotation(buf, &ann.value, indent);
        }
        Body(loc_pattern, loc_expr) => {
            fmt_body(buf, &loc_pattern.value, &loc_expr.value, indent);
        }
        TypedBody(_loc_pattern, _loc_annotation, _loc_expr) => {
            unreachable!("annotations and bodies have not yet been merged into TypedBody");
        }
        SpaceBefore(sub_def, spaces) => {
            fmt_spaces(buf, spaces.iter(), indent);
            fmt_def(buf, sub_def, indent);
        }
        SpaceAfter(sub_def, spaces) => {
            fmt_def(buf, sub_def, indent);

            fmt_spaces(buf, spaces.iter(), indent);
        }
        Nested(def) => fmt_def(buf, def, indent),
    }
}

pub fn fmt_body<'a>(
    buf: &mut String<'a>,
    pattern: &'a Pattern<'a>,
    body: &'a Expr<'a>,
    indent: u16,
) {
    fmt_pattern(buf, pattern, indent, Parens::InApply, false);
    buf.push_str(" =");
    if is_multiline_expr(body) {
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

pub fn fmt_type_annotation<'a>(
    buf: &mut String<'a>,
    pattern: &'a Pattern<'a>,
    annotation: &'a TypeAnnotation<'a>,
    indent: u16,
) {
    fmt_pattern(buf, pattern, indent, Parens::NotNeeded, false);
    buf.push_str(" : ");
    fmt_annotation(buf, annotation, indent);
}
