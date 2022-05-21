use crate::annotation::{Formattable, Newlines, Parens};
use crate::pattern::fmt_pattern;
use crate::spaces::{fmt_spaces, INDENT};
use crate::Buf;
use roc_parse::ast::{AbilityMember, Def, Expr, ExtractSpaces, Pattern, TypeHeader};
use roc_region::all::Loc;

/// A Located formattable value is also formattable
impl<'a> Formattable for Def<'a> {
    fn is_multiline(&self) -> bool {
        use roc_parse::ast::Def::*;
        use roc_parse::ast::TypeDef::*;
        use roc_parse::ast::ValueDef::*;

        match self {
            Type(def) => match def {
                Alias { ann, .. } => ann.is_multiline(),
                Opaque { typ, .. } => typ.is_multiline(),
                Ability { members, .. } => members.iter().any(|d| d.is_multiline()),
            },
            Value(def) => match def {
                Annotation(loc_pattern, loc_annotation) => {
                    loc_pattern.is_multiline() || loc_annotation.is_multiline()
                }
                Body(loc_pattern, loc_expr) => {
                    loc_pattern.is_multiline() || loc_expr.is_multiline()
                }
                AnnotatedBody { .. } => true,
                Expect(loc_expr) => loc_expr.is_multiline(),
            },
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
        use roc_parse::ast::TypeDef::*;
        use roc_parse::ast::ValueDef::*;

        match self {
            Type(def) => match def {
                Alias {
                    header: TypeHeader { name, vars },
                    ann,
                }
                | Opaque {
                    header: TypeHeader { name, vars },
                    typ: ann,
                } => {
                    buf.indent(indent);
                    buf.push_str(name.value);

                    for var in *vars {
                        buf.spaces(1);
                        fmt_pattern(buf, &var.value, indent, Parens::NotNeeded);
                    }

                    buf.push_str(match def {
                        Alias { .. } => " :",
                        Opaque { .. } => " :=",
                        _ => unreachable!(),
                    });
                    buf.spaces(1);

                    ann.format(buf, indent + INDENT)
                }
                Ability {
                    header: TypeHeader { name, vars },
                    loc_has: _,
                    members,
                } => {
                    buf.indent(indent);
                    buf.push_str(name.value);
                    for var in *vars {
                        buf.spaces(1);
                        fmt_pattern(buf, &var.value, indent, Parens::NotNeeded);
                    }

                    buf.push_str(" has");

                    if !self.is_multiline() {
                        debug_assert_eq!(members.len(), 1);
                        buf.push_str(" ");
                        members[0].format(buf, indent + INDENT);
                    } else {
                        for demand in members.iter() {
                            buf.newline();
                            buf.indent(indent + INDENT);
                            demand.format(buf, indent + INDENT);
                        }
                    }
                }
            },
            Value(def) => match def {
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
                        buf.spaces(1);
                        buf.push_str(":");
                        buf.spaces(1);
                        loc_annotation.format_with_options(
                            buf,
                            Parens::NotNeeded,
                            Newlines::No,
                            indent,
                        );
                    }
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
            },

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
            Expr::SpaceBefore(sub_def, spaces) => {
                let should_outdent = match sub_def {
                    Expr::Record { .. } | Expr::List { .. } => {
                        let is_only_newlines = spaces.iter().all(|s| s.is_newline());
                        is_only_newlines && sub_def.is_multiline()
                    }
                    _ => false,
                };

                if should_outdent {
                    buf.spaces(1);
                    sub_def.format_with_options(buf, Parens::NotNeeded, Newlines::Yes, indent);
                } else {
                    body.format_with_options(
                        buf,
                        Parens::NotNeeded,
                        Newlines::Yes,
                        indent + INDENT,
                    );
                }
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

impl<'a> Formattable for AbilityMember<'a> {
    fn is_multiline(&self) -> bool {
        self.name.value.is_multiline() || self.typ.is_multiline()
    }

    fn format<'buf>(&self, buf: &mut Buf<'buf>, indent: u16) {
        buf.push_str(self.name.value.extract_spaces().item);
        buf.push_str(" : ");
        self.typ.value.format(buf, indent + INDENT);
    }
}
