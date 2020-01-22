use crate::fmt::expr::{fmt_expr, is_multiline_expr};
use crate::fmt::pattern::fmt_pattern;
use crate::fmt::spaces::{fmt_spaces, newline, INDENT};
use crate::parse::ast::{Def, Expr};
use bumpalo::collections::String;

pub fn fmt_def<'a>(buf: &mut String<'a>, def: &'a Def<'a>, indent: u16) {
    use crate::parse::ast::Def::*;

    match def {
        Annotation(_, _) => panic!("TODO have format_def support Annotation"),
        Body(loc_pattern, loc_expr) => {
            fmt_pattern(buf, &loc_pattern.value, indent, true, false);
            buf.push_str(" =");
            if is_multiline_expr(&loc_expr.value) {
                match &loc_expr.value {
                    Expr::Record { .. } | Expr::List(_) => {
                        newline(buf, indent + INDENT);
                        fmt_expr(buf, &loc_expr.value, indent + INDENT, false, true);
                    }
                    _ => {
                        buf.push(' ');
                        fmt_expr(buf, &loc_expr.value, indent, false, true);
                    }
                }
            } else {
                buf.push(' ');
                fmt_expr(buf, &loc_expr.value, indent, false, true);
            }
        }
        TypedDef(_loc_pattern, _loc_annotation, _loc_expr) => {
            panic!("TODO support Annotation in TypedDef");
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
