use crate::annotation::{Formattable, Newlines, Parens};
use crate::pattern::fmt_pattern;
use crate::spaces::{fmt_spaces, INDENT};
use crate::Buf;
use roc_parse::ast::{
    AbilityMember, Def, Defs, Expr, ExtractSpaces, Pattern, TypeAnnotation, TypeDef, TypeHeader,
    ValueDef,
};
use roc_region::all::Loc;

/// A Located formattable value is also formattable

impl<'a> Formattable for Defs<'a> {
    fn is_multiline(&self) -> bool {
        use roc_parse::ast::TypeDef::*;

        match self {
            Alias { ann, .. } => ann.is_multiline(),
            Opaque { typ, .. } => typ.is_multiline(),
            Ability { members, .. } => members.iter().any(|d| d.is_multiline()),
        }
    }

    fn format_with_options<'buf>(
        &self,
        buf: &mut Buf<'buf>,
        _parens: Parens,
        _newlines: Newlines,
        indent: u16,
    ) {
        for (index, def) in self.defs().enumerate() {
            let spaces_before = &self.spaces[self.space_before[index].indices()];
            let spaces_after = &self.spaces[self.space_after[index].indices()];

            fmt_spaces(buf, spaces_before.iter(), indent);

            match def {
                Ok(type_def) => type_def.format(buf, indent),
                Err(value_def) => value_def.format(buf, indent),
            }

            fmt_spaces(buf, spaces_after.iter(), indent);
        }
    }
}

impl<'a> Formattable for TypeDef<'a> {
    fn is_multiline(&self) -> bool {
        use roc_parse::ast::TypeDef::*;

        match self {
            Alias { ann, .. } => ann.is_multiline(),
            Opaque { typ, .. } => typ.is_multiline(),
            Ability { members, .. } => members.iter().any(|d| d.is_multiline()),
        }
    }

    fn format_with_options<'buf>(
        &self,
        buf: &mut Buf<'buf>,
        _parens: Parens,
        _newlines: Newlines,
        indent: u16,
    ) {
        use roc_parse::ast::TypeDef::*;

        match self {
            Alias {
                header: TypeHeader { name, vars },
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
            Opaque {
                header: TypeHeader { name, vars },
                typ: ann,
                derived,
            } => {
                buf.indent(indent);
                buf.push_str(name.value);

                for var in *vars {
                    buf.spaces(1);
                    fmt_pattern(buf, &var.value, indent, Parens::NotNeeded);
                }

                buf.push_str(" :=");
                buf.spaces(1);

                let ann_is_where_clause =
                    matches!(ann.extract_spaces().item, TypeAnnotation::Where(..));

                let ann_has_spaces_before = matches!(&ann.value, TypeAnnotation::SpaceBefore(..));

                // Always put the has-derived clause on a newline if it is itself multiline, or
                // the annotation has a where-has clause.
                let derived_multiline = if let Some(derived) = derived {
                    !derived.value.is_empty() && (derived.is_multiline() || ann_is_where_clause)
                } else {
                    false
                };

                let make_multiline = ann.is_multiline() || derived_multiline;

                // If the annotation has spaces before, a newline will already be printed.
                if make_multiline && !ann_has_spaces_before {
                    buf.newline();
                    buf.indent(indent + INDENT);
                }

                ann.format(buf, indent + INDENT);

                if let Some(derived) = derived {
                    if !make_multiline {
                        buf.spaces(1);
                    }

                    derived.format_with_options(
                        buf,
                        Parens::NotNeeded,
                        Newlines::from_bool(make_multiline),
                        indent + INDENT,
                    );
                }
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
        }
    }
}

impl<'a> Formattable for ValueDef<'a> {
    fn is_multiline(&self) -> bool {
        use roc_parse::ast::ValueDef::*;

        match self {
            Annotation(loc_pattern, loc_annotation) => {
                loc_pattern.is_multiline() || loc_annotation.is_multiline()
            }
            Body(loc_pattern, loc_expr) => loc_pattern.is_multiline() || loc_expr.is_multiline(),
            AnnotatedBody { .. } => true,
            Expect(loc_expr) => loc_expr.is_multiline(),
        }
    }

    fn format_with_options<'buf>(
        &self,
        buf: &mut Buf<'buf>,
        _parens: Parens,
        _newlines: Newlines,
        indent: u16,
    ) {
        use roc_parse::ast::ValueDef::*;
        match self {
            Annotation(loc_pattern, loc_annotation) => {
                loc_pattern.format(buf, indent);

                if loc_annotation.is_multiline() {
                    buf.push_str(" :");

                    let should_outdent = match loc_annotation.value {
                        TypeAnnotation::SpaceBefore(sub_def, spaces) => match sub_def {
                            TypeAnnotation::Record { .. } | TypeAnnotation::TagUnion { .. } => {
                                let is_only_newlines = spaces.iter().all(|s| s.is_newline());
                                is_only_newlines && sub_def.is_multiline()
                            }
                            _ => false,
                        },
                        TypeAnnotation::Record { .. } | TypeAnnotation::TagUnion { .. } => true,
                        _ => false,
                    };

                    if should_outdent {
                        buf.spaces(1);
                        match loc_annotation.value {
                            TypeAnnotation::SpaceBefore(sub_def, _) => {
                                sub_def.format_with_options(
                                    buf,
                                    Parens::NotNeeded,
                                    Newlines::No,
                                    indent,
                                );
                            }
                            _ => {
                                loc_annotation.format_with_options(
                                    buf,
                                    Parens::NotNeeded,
                                    Newlines::No,
                                    indent,
                                );
                            }
                        }
                    } else {
                        loc_annotation.format_with_options(
                            buf,
                            Parens::NotNeeded,
                            Newlines::Yes,
                            indent + INDENT,
                        );
                    }
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
                let is_type_multiline = ann_type.is_multiline();
                let is_type_function = matches!(
                    ann_type.value,
                    TypeAnnotation::Function(..)
                        | TypeAnnotation::SpaceBefore(TypeAnnotation::Function(..), ..)
                        | TypeAnnotation::SpaceAfter(TypeAnnotation::Function(..), ..)
                );

                let next_indent = if is_type_multiline {
                    indent + INDENT
                } else {
                    indent
                };

                ann_pattern.format(buf, indent);
                buf.push_str(" :");

                if is_type_multiline && is_type_function {
                    ann_type.format_with_options(
                        buf,
                        Parens::NotNeeded,
                        Newlines::Yes,
                        next_indent,
                    );
                } else {
                    buf.spaces(1);
                    ann_type.format(buf, indent);
                }

                if let Some(comment_str) = comment {
                    buf.push_str(" #");
                    buf.spaces(1);
                    buf.push_str(comment_str.trim());
                }

                buf.newline();
                fmt_body(buf, &body_pattern.value, &body_expr.value, indent);
            }
        }
    }
}

impl<'a> Formattable for Def<'a> {
    fn is_multiline(&self) -> bool {
        use roc_parse::ast::Def::*;

        match self {
            Type(def) => def.is_multiline(),
            Value(def) => def.is_multiline(),
            SpaceBefore(sub_def, spaces) | SpaceAfter(sub_def, spaces) => {
                spaces.iter().any(|s| s.is_comment()) || sub_def.is_multiline()
            }
            NotYetImplemented(s) => todo!("{}", s),
        }
    }

    fn format_with_options<'buf>(
        &self,
        buf: &mut Buf<'buf>,
        parens: Parens,
        newlines: Newlines,
        indent: u16,
    ) {
        use roc_parse::ast::Def::*;

        match self {
            Type(def) => def.format_with_options(buf, parens, newlines, indent),
            Value(def) => def.format_with_options(buf, parens, newlines, indent),

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

pub fn fmt_value_def<'a, 'buf>(
    buf: &mut Buf<'buf>,
    def: &roc_parse::ast::ValueDef<'a>,
    indent: u16,
) {
    def.format(buf, indent);
}

pub fn fmt_type_def<'a, 'buf>(buf: &mut Buf<'buf>, def: &roc_parse::ast::TypeDef<'a>, indent: u16) {
    def.format(buf, indent);
}

pub fn fmt_toplevel_def<'a, 'buf>(buf: &mut Buf<'buf>, defs: &Defs<'a>, indent: u16) {
    defs.format(buf, indent);
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
        buf.spaces(1);
        buf.push(':');
        buf.spaces(1);
        self.typ.value.format(buf, indent + INDENT);
    }
}
