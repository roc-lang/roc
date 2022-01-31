use crate::annotation::{Formattable, Newlines, Parens};
use crate::pattern::fmt_pattern;
use crate::spaces::{fmt_spaces, INDENT};
use crate::Buf;
use roc_parse::ast::{AliasHeader, Def, Expr, Pattern};
use roc_region::all::Loc;

/// A Located formattable value is also formattable
impl<'a> Formattable for Def<'a> {
    fn is_multiline(&self) -> bool {
        use roc_parse::ast::Def::*;

        match self {
            Alias { ann, .. } => ann.is_multiline(),
            Annotation(loc_pattern, loc_annotation) => {
                loc_pattern.is_multiline() || loc_annotation.is_multiline()
            }
            Body(loc_pattern, loc_expr) => loc_pattern.is_multiline() || loc_expr.is_multiline(),
            AnnotatedBody { .. } => true,
            Expect(loc_expr) => loc_expr.is_multiline(),
            SpaceBefore(sub_def, spaces) | SpaceAfter(sub_def, spaces) => {
                spaces.iter().any(|s| s.is_comment()) || sub_def.is_multiline()
            }
            NotYetImplemented(s) => todo!("{}", s),
        }
    }

    fn format_with_options<'buf>(
        &self,
        buf: &mut Buf<'buf>,
        _parens: Parens,
        _newlines: Newlines,
        indent: u16,
    ) {
        use roc_parse::ast::Def::*;

        match self {
            Annotation(loc_pattern, loc_annotation) => {
                loc_pattern.format(buf, indent);
                if loc_annotation.is_multiline() {
                    buf.push_str(" :");
                    loc_annotation.format_with_options(
                        buf,
                        Parens::NotNeeded,
                        Newlines::Yes,
                        indent + INDENT,
                    );
                } else {
                    buf.push_str(" : ");
                    loc_annotation.format_with_options(
                        buf,
                        Parens::NotNeeded,
                        Newlines::No,
                        indent,
                    );
                }
            }
            Alias {
                header: AliasHeader { name, vars },
                ann,
            } => {
                buf.indent(indent);
                buf.push_str(name.value);

                for var in *vars {
                    buf.spaces(1);
                    fmt_pattern(buf, &var.value, indent, Parens::NotNeeded);
                }

                buf.push_str(" :");
                buf.spaces(1);

                ann.format(buf, indent + INDENT)
            }
            Body(loc_pattern, loc_expr) => {
                fmt_body(buf, &loc_pattern.value, &loc_expr.value, indent);
            }
            Expect(condition) => fmt_expect(buf, condition, self.is_multiline(), indent),
            AnnotatedBody {
                ann_pattern,
                ann_type,
                comment,
                body_pattern,
                body_expr,
            } => {
                ann_pattern.format(buf, indent);
                buf.push_str(" :");
                buf.spaces(1);
                ann_type.format(buf, indent);
                if let Some(comment_str) = comment {
                    buf.push_str(" #");
                    buf.spaces(1);
                    buf.push_str(comment_str.trim());
                }
                buf.newline();
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
            NotYetImplemented(s) => todo!("{}", s),
        }
    }
}

fn fmt_expect<'a, 'buf>(
    buf: &mut Buf<'buf>,
    condition: &'a Loc<Expr<'a>>,
    is_multiline: bool,
    indent: u16,
) {
    let return_indent = if is_multiline {
        indent + INDENT
    } else {
        indent
    };

    buf.push_str("expect");
    condition.format(buf, return_indent);
}

pub fn fmt_def<'a, 'buf>(buf: &mut Buf<'buf>, def: &Def<'a>, indent: u16) {
    def.format(buf, indent);
}

pub fn fmt_body<'a, 'buf>(
    buf: &mut Buf<'buf>,
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
            Expr::Record { .. } | Expr::List { .. } => {
                buf.newline();
                body.format_with_options(buf, Parens::NotNeeded, Newlines::Yes, indent + INDENT);
            }
            _ => {
                buf.spaces(1);
                body.format_with_options(buf, Parens::NotNeeded, Newlines::Yes, indent);
            }
        }
    } else {
        buf.spaces(1);
        body.format_with_options(buf, Parens::NotNeeded, Newlines::Yes, indent);
    }
}
