use crate::spaces::{fmt_comments_only, fmt_condition_spaces, fmt_spaces, newline, INDENT};
use bumpalo::collections::String;
use roc_parse::ast::{AssignedField, Expr, Tag, TypeAnnotation};
use roc_region::all::Located;

/// Does an AST node need parens around it?
///
/// Usually not, but there are two cases where it may be required
///
/// 1. In a function type, function types are in parens
///
///      a -> b,  c -> d
///     (a -> b), c -> d
///
/// 2. In applications, applications are in brackets
///     This is true in patterns, type annotations and expressions
///
///     Just (Just a)
///     List (List a)
///     reverse (reverse l)
#[derive(PartialEq, Eq, Clone, Copy)]
pub enum Parens {
    NotNeeded,
    InFunctionType,
    InApply,
}

/// In an AST node, do we show newlines around it
///
/// Sometimes, we only want to show comments, at other times
/// we also want to show newlines. By default the formatter
/// takes care of inserting newlines, but sometimes the user's
/// newlines are taken into account.
#[derive(PartialEq, Eq, Clone, Copy)]
pub enum Newlines {
    Yes,
    No,
}

pub fn fmt_annotation<'a>(buf: &mut String<'a>, annotation: &'a TypeAnnotation<'a>, indent: u16) {
    annotation.format(buf, indent);
}

pub trait Formattable<'a> {
    fn is_multiline(&self) -> bool;

    fn format_with_options(
        &self,
        buf: &mut String<'a>,
        _parens: Parens,
        _newlines: Newlines,
        indent: u16,
    ) {
        self.format(buf, indent);
    }

    fn format(&self, buf: &mut String<'a>, indent: u16) {
        self.format_with_options(buf, Parens::NotNeeded, Newlines::No, indent);
    }
}

/// A Located formattable value is also formattable
impl<'a, T> Formattable<'a> for Located<T>
where
    T: Formattable<'a>,
{
    fn is_multiline(&self) -> bool {
        self.value.is_multiline()
    }

    fn format_with_options(
        &self,
        buf: &mut String<'a>,
        parens: Parens,
        newlines: Newlines,
        indent: u16,
    ) {
        self.value
            .format_with_options(buf, parens, newlines, indent)
    }

    fn format(&self, buf: &mut String<'a>, indent: u16) {
        self.value.format(buf, indent)
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

    fn format_with_options(
        &self,
        buf: &mut String<'a>,
        parens: Parens,
        newlines: Newlines,
        indent: u16,
    ) {
        use roc_parse::ast::TypeAnnotation::*;

        match self {
            Function(arguments, result) => {
                let write_parens = parens != Parens::NotNeeded;

                if write_parens {
                    buf.push('(')
                }

                let mut it = arguments.iter().peekable();

                while let Some(argument) = it.next() {
                    (&argument.value).format_with_options(
                        buf,
                        Parens::InFunctionType,
                        Newlines::No,
                        indent,
                    );

                    if it.peek().is_some() {
                        buf.push_str(", ");
                    }
                }

                buf.push_str(" -> ");

                (&result.value).format_with_options(
                    buf,
                    Parens::InFunctionType,
                    Newlines::No,
                    indent,
                );

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
                    (&argument.value).format_with_options(
                        buf,
                        Parens::InApply,
                        Newlines::No,
                        indent,
                    );
                }

                if write_parens {
                    buf.push(')')
                }
            }
            BoundVariable(v) => buf.push_str(v),
            Wildcard => buf.push('*'),

            TagUnion { tags, ext } => {
                tags.format(buf, indent);

                if let Some(loc_ext_ann) = *ext {
                    loc_ext_ann.value.format(buf, indent);
                }
            }

            Record { fields, ext } => {
                fields.format(buf, indent);

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
                ann.format_with_options(buf, parens, newlines, indent)
            }

            Malformed(raw) => buf.push_str(raw),
        }
    }
}

/// Fields are subtly different on the type and term level:
///
/// >   type: { x : Int, y : Bool }
/// >   term: { x: 100, y: True }
///
/// So we need two instances, each having the specific separator
impl<'a> Formattable<'a> for AssignedField<'a, TypeAnnotation<'a>> {
    fn is_multiline(&self) -> bool {
        is_multiline_assigned_field_help(self)
    }

    fn format_with_options(
        &self,
        buf: &mut String<'a>,
        parens: Parens,
        newlines: Newlines,
        indent: u16,
    ) {
        // we abuse the `Newlines` type to decide between multiline or single-line layout
        format_assigned_field_help(self, buf, parens, indent, " ", newlines == Newlines::Yes);
    }
}

impl<'a> Formattable<'a> for AssignedField<'a, Expr<'a>> {
    fn is_multiline(&self) -> bool {
        is_multiline_assigned_field_help(self)
    }

    fn format_with_options(
        &self,
        buf: &mut String<'a>,
        parens: Parens,
        newlines: Newlines,
        indent: u16,
    ) {
        // we abuse the `Newlines` type to decide between multiline or single-line layout
        format_assigned_field_help(self, buf, parens, indent, "", newlines == Newlines::Yes);
    }
}

fn is_multiline_assigned_field_help<'a, T: Formattable<'a>>(afield: &AssignedField<'a, T>) -> bool {
    use self::AssignedField::*;

    match afield {
        RequiredValue(_, spaces, ann) | OptionalValue(_, spaces, ann) => {
            !spaces.is_empty() || ann.value.is_multiline()
        }
        LabelOnly(_) => false,
        AssignedField::SpaceBefore(_, _) | AssignedField::SpaceAfter(_, _) => true,
        Malformed(text) => text.chars().any(|c| c == '\n'),
    }
}

fn format_assigned_field_help<'a, T>(
    zelf: &AssignedField<'a, T>,
    buf: &mut String<'a>,
    parens: Parens,
    indent: u16,
    separator_prefix: &str,
    is_multiline: bool,
) where
    T: Formattable<'a>,
{
    use self::AssignedField::*;

    match zelf {
        RequiredValue(name, spaces, ann) => {
            if is_multiline {
                newline(buf, indent);
            }

            buf.push_str(name.value);

            if !spaces.is_empty() {
                fmt_spaces(buf, spaces.iter(), indent);
            }

            buf.push_str(separator_prefix);
            buf.push(':');
            ann.value.format(buf, indent);
        }
        OptionalValue(name, spaces, ann) => {
            if is_multiline {
                newline(buf, indent);
            }

            buf.push_str(name.value);

            if !spaces.is_empty() {
                fmt_spaces(buf, spaces.iter(), indent);
            }

            buf.push_str(separator_prefix);
            buf.push('?');
            ann.value.format(buf, indent);
        }
        LabelOnly(name) => {
            if is_multiline {
                newline(buf, indent);
            }

            buf.push_str(name.value);
        }
        AssignedField::SpaceBefore(sub_field, spaces) => {
            fmt_comments_only(buf, spaces.iter(), indent);
            format_assigned_field_help(
                sub_field,
                buf,
                parens,
                indent,
                separator_prefix,
                is_multiline,
            );
        }
        AssignedField::SpaceAfter(sub_field, spaces) => {
            format_assigned_field_help(
                sub_field,
                buf,
                parens,
                indent,
                separator_prefix,
                is_multiline,
            );
            fmt_comments_only(buf, spaces.iter(), indent);
        }
        Malformed(raw) => {
            buf.push_str(raw);
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

    fn format_with_options(
        &self,
        buf: &mut String<'a>,
        _parens: Parens,
        _newlines: Newlines,
        indent: u16,
    ) {
        let is_multiline = self.is_multiline();

        match self {
            Tag::Global { name, args } => {
                buf.push_str(name.value);
                if is_multiline {
                    let arg_indent = indent + INDENT;

                    for arg in *args {
                        newline(buf, arg_indent);
                        arg.format_with_options(buf, Parens::InApply, Newlines::No, arg_indent);
                    }
                } else {
                    for arg in *args {
                        buf.push(' ');
                        arg.format_with_options(buf, Parens::InApply, Newlines::No, indent);
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
                        arg.format_with_options(buf, Parens::InApply, Newlines::No, arg_indent);
                    }
                } else {
                    for arg in *args {
                        buf.push(' ');
                        arg.format_with_options(buf, Parens::InApply, Newlines::No, indent);
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
        fn format_with_options(
            &self,
            buf: &mut String<'a>,
            _parens: Parens,
            _newlines: Newlines,
            indent: u16,
        ) {
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
                                    expr_above.format(buf, item_indent);

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
                                    expr_below.format(buf, item_indent);
                                    if iter.peek().is_some() {
                                        buf.push(',');
                                    }
                                }
                            }
                        }

                        $t::SpaceAfter(sub_expr, spaces) => {
                            newline(buf, item_indent);

                            sub_expr.format(buf, item_indent);
                            if iter.peek().is_some() {
                                buf.push(',');
                            }

                            fmt_condition_spaces(buf, spaces.iter(), item_indent);
                        }

                        _ => {
                            newline(buf, item_indent);
                            item.format(buf, item_indent);
                            if iter.peek().is_some() {
                                buf.push(',');
                            }
                        }
                    }
                } else {
                    buf.push(' ');
                    item.format(buf, item_indent);
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

impl<'a> Formattable<'a> for &'a [Located<AssignedField<'a, TypeAnnotation<'a>>>] {
    fn is_multiline(&self) -> bool {
        self.iter().any(|f| f.value.is_multiline())
    }

    implement_format_sequence!('{', '}', AssignedField);
}
