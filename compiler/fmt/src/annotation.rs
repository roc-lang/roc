use crate::spaces::{fmt_comments_only, fmt_condition_spaces, newline, INDENT};
use bumpalo::collections::String;
use roc_parse::ast::{AssignedField, Tag, TypeAnnotation};
use roc_region::all::Located;

#[derive(PartialEq, Eq)]
pub enum Parens {
    NotNeeded,
    InFunctionType,
    InApply,
}

pub fn fmt_annotation<'a>(buf: &mut String<'a>, annotation: &'a TypeAnnotation<'a>, indent: u16) {
    annotation.format_with_parens(buf, Parens::NotNeeded, indent);
}

pub trait Formattable<'a> {
    fn is_multiline(&self) -> bool;

    fn format_with_parens(&self, buf: &mut String<'a>, _parens: Parens, indent: u16) {
        self.format(buf, indent);
    }

    fn format(&self, buf: &mut String<'a>, indent: u16) {
        self.format_with_parens(buf, Parens::NotNeeded, indent);
    }
}

impl<'a> Formattable<'a> for TypeAnnotation<'a> {
    fn is_multiline(&self) -> bool {
        use roc_parse::ast::TypeAnnotation::*;

        match self {
            // Return whether these spaces contain any Newlines
            SpaceBefore(_, spaces) | SpaceAfter(_, spaces) => {
                debug_assert!(!spaces.is_empty());

                // "spaces" always contain either a newline or comment, and comments have newlines
                true
            }

            Wildcard | BoundVariable(_) | Malformed(_) => false,
            Function(args, result) => {
                (&result.value).is_multiline()
                    || args.iter().any(|loc_arg| (&loc_arg.value).is_multiline())
            }
            Apply(_, _, args) => args.iter().any(|loc_arg| loc_arg.value.is_multiline()),
            As(lhs, _, rhs) => lhs.value.is_multiline() || rhs.value.is_multiline(),

            Record { fields, ext } => {
                match ext {
                    Some(ann) if ann.value.is_multiline() => return true,
                    _ => {}
                }

                fields.iter().any(|field| field.value.is_multiline())
            }

            TagUnion { tags, ext } => {
                match ext {
                    Some(ann) if ann.value.is_multiline() => return true,
                    _ => {}
                }

                tags.iter().any(|tag| tag.value.is_multiline())
            }
        }
    }

    fn format_with_parens(&self, buf: &mut String<'a>, parens: Parens, indent: u16) {
        use roc_parse::ast::TypeAnnotation::*;

        match self {
            Function(arguments, result) => {
                let write_parens = parens != Parens::NotNeeded;

                if write_parens {
                    buf.push('(')
                }

                let mut it = arguments.iter().peekable();

                while let Some(argument) = it.next() {
                    (&argument.value).format_with_parens(buf, Parens::InFunctionType, indent);

                    if it.peek().is_some() {
                        buf.push_str(", ");
                    }
                }

                buf.push_str(" -> ");

                (&result.value).format_with_parens(buf, Parens::InFunctionType, indent);

                if write_parens {
                    buf.push(')')
                }
            }
            Apply(_, name, arguments) => {
                // NOTE apply is never multiline
                let write_parens = parens == Parens::InApply && !arguments.is_empty();

                if write_parens {
                    buf.push('(')
                }

                buf.push_str(name);

                for argument in *arguments {
                    buf.push(' ');
                    (&argument.value).format_with_parens(buf, Parens::InApply, indent);
                }

                if write_parens {
                    buf.push(')')
                }
            }
            BoundVariable(v) => buf.push_str(v),
            Wildcard => buf.push('*'),

            TagUnion { tags, ext } => {
                tags.format_with_parens(buf, Parens::NotNeeded, indent);

                if let Some(loc_ext_ann) = *ext {
                    loc_ext_ann.value.format(buf, indent);
                }
            }

            Record { fields, ext } => {
                fields.format_with_parens(buf, Parens::NotNeeded, indent);

                if let Some(loc_ext_ann) = *ext {
                    loc_ext_ann.value.format(buf, indent);
                }
            }

            As(lhs, _spaces, rhs) => {
                // TODO use spaces?
                lhs.value.format(buf, indent);
                buf.push_str(" as ");
                rhs.value.format(buf, indent);
            }

            SpaceBefore(ann, _spaces) | SpaceAfter(ann, _spaces) => {
                ann.format_with_parens(buf, parens, indent)
            }

            Malformed(raw) => buf.push_str(raw),
        }
    }
}

impl<'a, T: Formattable<'a>> Formattable<'a> for AssignedField<'a, T> {
    fn is_multiline(&self) -> bool {
        use self::AssignedField::*;

        match self {
            LabeledValue(_, spaces, ann) => !spaces.is_empty() || ann.value.is_multiline(),
            LabelOnly(_) => false,
            AssignedField::SpaceBefore(_, _) | AssignedField::SpaceAfter(_, _) => true,
            Malformed(text) => text.chars().any(|c| c == '\n'),
        }
    }

    fn format_with_parens(&self, buf: &mut String<'a>, _parens: Parens, indent: u16) {
        // TODO multiline?

        use self::AssignedField::*;

        match self {
            LabeledValue(name, _spaces, ann) => {
                // TODO use spaces?
                buf.push_str(name.value);
                buf.push_str(" : ");
                ann.value.format(buf, indent);
            }
            LabelOnly(name) => {
                buf.push_str(name.value);
            }
            AssignedField::SpaceBefore(_, _) | AssignedField::SpaceAfter(_, _) => unreachable!(),
            Malformed(raw) => {
                buf.push_str(raw);
            }
        }
    }
}

impl<'a> Formattable<'a> for Tag<'a> {
    fn is_multiline(&self) -> bool {
        use self::Tag::*;

        match self {
            Global { args, .. } | Private { args, .. } => {
                args.iter().any(|arg| (&arg.value).is_multiline())
            }
            Tag::SpaceBefore(_, _) | Tag::SpaceAfter(_, _) => true,
            Malformed(text) => text.chars().any(|c| c == '\n'),
        }
    }

    fn format_with_parens(&self, buf: &mut String<'a>, _parens: Parens, indent: u16) {
        let is_multiline = self.is_multiline();

        match self {
            Tag::Global { name, args } => {
                buf.push_str(name.value);
                if is_multiline {
                    let arg_indent = indent + INDENT;

                    for arg in *args {
                        newline(buf, arg_indent);
                        (&arg.value).format_with_parens(buf, Parens::InApply, arg_indent);
                    }
                } else {
                    for arg in *args {
                        buf.push(' ');
                        (&arg.value).format_with_parens(buf, Parens::InApply, indent);
                    }
                }
            }
            Tag::Private { name, args } => {
                buf.push('@');
                buf.push_str(name.value);
                if is_multiline {
                    let arg_indent = indent + INDENT;

                    for arg in *args {
                        newline(buf, arg_indent);
                        (&arg.value).format_with_parens(buf, Parens::InApply, arg_indent);
                    }
                } else {
                    for arg in *args {
                        buf.push(' ');
                        (&arg.value).format_with_parens(buf, Parens::InApply, indent);
                    }
                }
            }
            Tag::SpaceBefore(_, _) | Tag::SpaceAfter(_, _) => unreachable!(),
            Tag::Malformed(raw) => buf.push_str(raw),
        }
    }
}

macro_rules! implement_format_sequence {
    ($start:expr, $end:expr, $t:ident) => {
        fn format_with_parens(&self, buf: &mut String<'a>, _parens: Parens, indent: u16) {
            buf.push($start);

            let mut iter = self.iter().peekable();

            let is_multiline = self.is_multiline();

            let item_indent = if is_multiline {
                indent + INDENT
            } else {
                indent
            };

            while let Some(item) = iter.next() {
                if is_multiline {
                    match &item.value {
                        $t::SpaceBefore(expr_below, spaces_above_expr) => {
                            newline(buf, item_indent);
                            fmt_comments_only(buf, spaces_above_expr.iter(), item_indent);

                            match &expr_below {
                                $t::SpaceAfter(expr_above, spaces_below_expr) => {
                                    expr_above.format_with_parens(
                                        buf,
                                        Parens::NotNeeded,
                                        item_indent,
                                    );

                                    if iter.peek().is_some() {
                                        buf.push(',');
                                    }

                                    fmt_condition_spaces(
                                        buf,
                                        spaces_below_expr.iter(),
                                        item_indent,
                                    );
                                }
                                _ => {
                                    expr_below.format_with_parens(
                                        buf,
                                        Parens::NotNeeded,
                                        item_indent,
                                    );
                                    if iter.peek().is_some() {
                                        buf.push(',');
                                    }
                                }
                            }
                        }

                        $t::SpaceAfter(sub_expr, spaces) => {
                            newline(buf, item_indent);

                            sub_expr.format_with_parens(buf, Parens::NotNeeded, item_indent);
                            if iter.peek().is_some() {
                                buf.push(',');
                            }

                            fmt_condition_spaces(buf, spaces.iter(), item_indent);
                        }

                        _ => {
                            newline(buf, item_indent);
                            (&item.value).format_with_parens(buf, Parens::NotNeeded, item_indent);
                            if iter.peek().is_some() {
                                buf.push(',');
                            }
                        }
                    }
                } else {
                    buf.push(' ');
                    (&item.value).format_with_parens(buf, Parens::NotNeeded, item_indent);
                    if iter.peek().is_some() {
                        buf.push(',');
                    }
                }
            }

            if is_multiline {
                newline(buf, indent);
            }

            if !self.is_empty() && !is_multiline {
                buf.push(' ');
            }
            buf.push($end);
        }
    };
}

impl<'a> Formattable<'a> for &'a [Located<Tag<'a>>] {
    fn is_multiline(&self) -> bool {
        self.iter().any(|t| t.value.is_multiline())
    }

    implement_format_sequence!('[', ']', Tag);
}

impl<'a, T: Formattable<'a>> Formattable<'a> for &'a [Located<AssignedField<'a, T>>] {
    fn is_multiline(&self) -> bool {
        self.iter().any(|f| f.value.is_multiline())
    }

    implement_format_sequence!('{', '}', AssignedField);
}
