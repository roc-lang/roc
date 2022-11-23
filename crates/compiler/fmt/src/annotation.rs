use crate::{
    collection::{fmt_collection, Braces},
    spaces::{fmt_comments_only, fmt_spaces, NewlineAt, INDENT},
    Buf,
};
use roc_parse::ast::{
    AssignedField, Collection, Expr, ExtractSpaces, HasAbilities, HasAbility, HasClause, HasImpls,
    Tag, TypeAnnotation, TypeHeader,
};
use roc_parse::ident::UppercaseIdent;
use roc_region::all::Loc;

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
#[derive(PartialEq, Eq, Clone, Copy, Debug)]
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
#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum Newlines {
    No,
    Yes,
}

impl Newlines {
    pub fn from_bool(yes: bool) -> Self {
        if yes {
            Self::Yes
        } else {
            Self::No
        }
    }
}

pub trait Formattable {
    fn is_multiline(&self) -> bool;

    fn format_with_options<'buf>(
        &self,
        buf: &mut Buf<'buf>,
        _parens: Parens,
        _newlines: Newlines,
        indent: u16,
    ) {
        self.format(buf, indent);
    }

    fn format<'buf>(&self, buf: &mut Buf<'buf>, indent: u16) {
        self.format_with_options(buf, Parens::NotNeeded, Newlines::No, indent);
    }
}

/// A reference to a formattable value is also formattable
impl<'a, T> Formattable for &'a T
where
    T: Formattable,
{
    fn is_multiline(&self) -> bool {
        (*self).is_multiline()
    }

    fn format_with_options<'buf>(
        &self,
        buf: &mut Buf<'buf>,
        parens: Parens,
        newlines: Newlines,
        indent: u16,
    ) {
        (*self).format_with_options(buf, parens, newlines, indent)
    }

    fn format<'buf>(&self, buf: &mut Buf<'buf>, indent: u16) {
        (*self).format(buf, indent)
    }
}

impl<'a, T> Formattable for Collection<'a, T>
where
    T: Formattable,
{
    fn is_multiline(&self) -> bool {
        // if there are any comments, they must go on their own line
        // because otherwise they'd comment out the closing delimiter
        !self.final_comments().is_empty() ||
        // if any of the items in the collection are multiline,
        // then the whole collection must be multiline
        self.items.iter().any(Formattable::is_multiline)
    }
}

/// A Located formattable value is also formattable
impl<T> Formattable for Loc<T>
where
    T: Formattable,
{
    fn is_multiline(&self) -> bool {
        self.value.is_multiline()
    }

    fn format_with_options<'buf>(
        &self,
        buf: &mut Buf<'buf>,
        parens: Parens,
        newlines: Newlines,
        indent: u16,
    ) {
        self.value
            .format_with_options(buf, parens, newlines, indent)
    }

    fn format<'buf>(&self, buf: &mut Buf<'buf>, indent: u16) {
        self.value.format(buf, indent)
    }
}

impl<'a> Formattable for UppercaseIdent<'a> {
    fn is_multiline(&self) -> bool {
        false
    }

    fn format_with_options<'buf>(
        &self,
        buf: &mut Buf<'buf>,
        _parens: Parens,
        _newlines: Newlines,
        _indent: u16,
    ) {
        buf.push_str((*self).into())
    }
}

impl<'a> Formattable for TypeAnnotation<'a> {
    fn is_multiline(&self) -> bool {
        use roc_parse::ast::TypeAnnotation::*;

        match self {
            // Return whether these spaces contain any Newlines
            SpaceBefore(_, spaces) | SpaceAfter(_, spaces) => {
                debug_assert!(!spaces.is_empty());

                // "spaces" always contain either a newline or comment, and comments have newlines
                true
            }

            Wildcard | Inferred | BoundVariable(_) | Malformed(_) => false,
            Function(args, result) => {
                result.value.is_multiline()
                    || args.iter().any(|loc_arg| loc_arg.value.is_multiline())
            }
            Apply(_, _, args) => args.iter().any(|loc_arg| loc_arg.value.is_multiline()),
            As(lhs, _, _) => lhs.value.is_multiline(),

            Where(annot, has_clauses) => {
                annot.is_multiline() || has_clauses.iter().any(|has| has.is_multiline())
            }

            Tuple { fields, ext } => {
                match ext {
                    Some(ann) if ann.value.is_multiline() => return true,
                    _ => {}
                }

                fields.items.iter().any(|field| field.value.is_multiline())
            }

            Record { fields, ext } => {
                match ext {
                    Some(ann) if ann.value.is_multiline() => return true,
                    _ => {}
                }

                fields.items.iter().any(|field| field.value.is_multiline())
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

    fn format_with_options<'buf>(
        &self,
        buf: &mut Buf<'buf>,
        parens: Parens,
        newlines: Newlines,
        indent: u16,
    ) {
        use roc_parse::ast::TypeAnnotation::*;

        match self {
            Function(args, ret) => {
                let needs_parens = parens != Parens::NotNeeded;

                buf.indent(indent);

                if needs_parens {
                    buf.push('(')
                }

                let mut it = args.iter().enumerate().peekable();
                let should_add_newlines = newlines == Newlines::Yes;

                while let Some((index, argument)) = it.next() {
                    let is_first = index == 0;
                    let is_multiline = &argument.value.is_multiline();

                    if !is_first && !is_multiline && should_add_newlines {
                        buf.newline();
                    }

                    argument.value.format_with_options(
                        buf,
                        Parens::InFunctionType,
                        Newlines::No,
                        indent,
                    );

                    if it.peek().is_some() {
                        buf.push_str(",");
                        if !should_add_newlines {
                            buf.spaces(1);
                        }
                    }
                }

                if should_add_newlines {
                    buf.newline();
                    buf.indent(indent);
                } else {
                    buf.spaces(1);
                }

                buf.push_str("->");
                buf.spaces(1);

                ret.value
                    .format_with_options(buf, Parens::InFunctionType, Newlines::No, indent);

                if needs_parens {
                    buf.push(')')
                }
            }
            Apply(pkg, name, arguments) => {
                buf.indent(indent);
                // NOTE apply is never multiline
                let write_parens = parens == Parens::InApply && !arguments.is_empty();

                if write_parens {
                    buf.push('(')
                }

                if !pkg.is_empty() {
                    buf.push_str(pkg);
                    buf.push('.');
                }

                buf.push_str(name);

                for argument in *arguments {
                    buf.spaces(1);
                    argument
                        .value
                        .format_with_options(buf, Parens::InApply, Newlines::No, indent);
                }

                if write_parens {
                    buf.push(')')
                }
            }
            BoundVariable(v) => buf.push_str(v),
            Wildcard => buf.push('*'),
            Inferred => buf.push('_'),

            TagUnion { tags, ext } => {
                fmt_collection(buf, indent, Braces::Square, *tags, newlines);

                if let Some(loc_ext_ann) = *ext {
                    loc_ext_ann.value.format(buf, indent);
                }
            }

            Tuple { fields, ext } => {
                fmt_collection(buf, indent, Braces::Round, *fields, newlines);

                if let Some(loc_ext_ann) = *ext {
                    loc_ext_ann.value.format(buf, indent);
                }
            }

            Record { fields, ext } => {
                fmt_collection(buf, indent, Braces::Curly, *fields, newlines);

                if let Some(loc_ext_ann) = *ext {
                    loc_ext_ann.value.format(buf, indent);
                }
            }

            As(lhs, _spaces, TypeHeader { name, vars }) => {
                // TODO use _spaces?
                lhs.value
                    .format_with_options(buf, Parens::InFunctionType, Newlines::No, indent);
                buf.spaces(1);
                buf.push_str("as");
                buf.spaces(1);
                buf.push_str(name.value);
                for var in *vars {
                    buf.spaces(1);
                    var.value
                        .format_with_options(buf, Parens::NotNeeded, Newlines::No, indent);
                }
            }

            Where(annot, has_clauses) => {
                annot.format_with_options(buf, parens, newlines, indent);
                buf.spaces(1);
                for (i, has) in has_clauses.iter().enumerate() {
                    buf.push(if i == 0 { '|' } else { ',' });
                    buf.spaces(1);
                    has.format_with_options(buf, parens, newlines, indent);
                }
            }

            SpaceBefore(ann, spaces) => {
                let is_function = matches!(ann, TypeAnnotation::Function(..));
                let next_newlines = if is_function && newlines == Newlines::Yes {
                    Newlines::Yes
                } else {
                    Newlines::No
                };

                if !buf.ends_with_newline() {
                    buf.newline();
                    buf.indent(indent);
                }
                fmt_comments_only(buf, spaces.iter(), NewlineAt::Bottom, indent);
                ann.format_with_options(buf, parens, next_newlines, indent)
            }
            SpaceAfter(ann, spaces) => {
                ann.format_with_options(buf, parens, newlines, indent);
                fmt_comments_only(buf, spaces.iter(), NewlineAt::Bottom, indent);
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
impl<'a> Formattable for AssignedField<'a, TypeAnnotation<'a>> {
    fn is_multiline(&self) -> bool {
        is_multiline_assigned_field_help(self)
    }

    fn format_with_options<'buf>(
        &self,
        buf: &mut Buf<'buf>,
        _parens: Parens,
        newlines: Newlines,
        indent: u16,
    ) {
        // we abuse the `Newlines` type to decide between multiline or single-line layout
        format_assigned_field_help(self, buf, indent, 1, newlines == Newlines::Yes);
    }
}

impl<'a> Formattable for AssignedField<'a, Expr<'a>> {
    fn is_multiline(&self) -> bool {
        is_multiline_assigned_field_help(self)
    }

    fn format_with_options<'buf>(
        &self,
        buf: &mut Buf<'buf>,
        _parens: Parens,
        newlines: Newlines,
        indent: u16,
    ) {
        // we abuse the `Newlines` type to decide between multiline or single-line layout
        format_assigned_field_help(self, buf, indent, 0, newlines == Newlines::Yes);
    }
}

fn is_multiline_assigned_field_help<T: Formattable>(afield: &AssignedField<'_, T>) -> bool {
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

fn format_assigned_field_help<'a, 'buf, T>(
    zelf: &AssignedField<'a, T>,
    buf: &mut Buf<'buf>,
    indent: u16,
    separator_spaces: usize,
    is_multiline: bool,
) where
    T: Formattable,
{
    use self::AssignedField::*;

    match zelf {
        RequiredValue(name, spaces, ann) => {
            if is_multiline {
                buf.newline();
            }

            buf.indent(indent);
            buf.push_str(name.value);

            if !spaces.is_empty() {
                fmt_spaces(buf, spaces.iter(), indent);
            }

            buf.spaces(separator_spaces);
            buf.push(':');
            buf.spaces(1);
            ann.value.format(buf, indent);
        }
        OptionalValue(name, spaces, ann) => {
            if is_multiline {
                buf.newline();
                buf.indent(indent);
            }

            buf.push_str(name.value);

            if !spaces.is_empty() {
                fmt_spaces(buf, spaces.iter(), indent);
            }

            buf.spaces(separator_spaces);
            buf.push('?');
            ann.value.format(buf, indent);
        }
        LabelOnly(name) => {
            if is_multiline {
                buf.newline();
                buf.indent(indent);
            }

            buf.push_str(name.value);
        }
        AssignedField::SpaceBefore(sub_field, spaces) => {
            fmt_comments_only(buf, spaces.iter(), NewlineAt::Bottom, indent);
            format_assigned_field_help(sub_field, buf, indent, separator_spaces, is_multiline);
        }
        AssignedField::SpaceAfter(sub_field, spaces) => {
            format_assigned_field_help(sub_field, buf, indent, separator_spaces, is_multiline);
            fmt_comments_only(buf, spaces.iter(), NewlineAt::Bottom, indent);
        }
        Malformed(raw) => {
            buf.push_str(raw);
        }
    }
}

impl<'a> Formattable for Tag<'a> {
    fn is_multiline(&self) -> bool {
        use self::Tag::*;

        match self {
            Apply { args, .. } => args.iter().any(|arg| arg.value.is_multiline()),
            Tag::SpaceBefore(_, _) | Tag::SpaceAfter(_, _) => true,
            Malformed(text) => text.chars().any(|c| c == '\n'),
        }
    }

    fn format_with_options<'buf>(
        &self,
        buf: &mut Buf<'buf>,
        _parens: Parens,
        _newlines: Newlines,
        indent: u16,
    ) {
        let is_multiline = self.is_multiline();

        match self {
            Tag::Apply { name, args } => {
                buf.indent(indent);
                buf.push_str(name.value);
                if is_multiline {
                    let arg_indent = indent + INDENT;

                    for arg in *args {
                        buf.newline();
                        arg.value.format_with_options(
                            buf,
                            Parens::InApply,
                            Newlines::No,
                            arg_indent,
                        );
                    }
                } else {
                    for arg in *args {
                        buf.spaces(1);
                        arg.format_with_options(buf, Parens::InApply, Newlines::No, indent);
                    }
                }
            }
            Tag::SpaceBefore(_, _) | Tag::SpaceAfter(_, _) => unreachable!(),
            Tag::Malformed(raw) => {
                buf.indent(indent);
                buf.push_str(raw);
            }
        }
    }
}

impl<'a> Formattable for HasClause<'a> {
    fn is_multiline(&self) -> bool {
        // No, always put abilities in a "has" clause on one line
        false
    }

    fn format_with_options<'buf>(
        &self,
        buf: &mut Buf<'buf>,
        parens: Parens,
        newlines: Newlines,
        indent: u16,
    ) {
        buf.push_str(self.var.value.extract_spaces().item);
        buf.spaces(1);
        buf.push_str("has");
        buf.spaces(1);

        for (i, ab) in self.abilities.iter().enumerate() {
            if i > 0 {
                buf.spaces(1);
                buf.push('&');
                buf.spaces(1);
            }
            ab.format_with_options(buf, parens, newlines, indent);
        }
    }
}

impl<'a> Formattable for HasImpls<'a> {
    fn is_multiline(&self) -> bool {
        match self {
            HasImpls::SpaceBefore(_, _) | HasImpls::SpaceAfter(_, _) => true,
            HasImpls::HasImpls(impls) => impls.is_multiline(),
        }
    }

    fn format_with_options<'buf>(
        &self,
        buf: &mut Buf<'buf>,
        parens: Parens,
        newlines: Newlines,
        indent: u16,
    ) {
        match self {
            HasImpls::HasImpls(impls) => {
                if newlines == Newlines::Yes {
                    buf.newline();
                    buf.indent(indent);
                }
                fmt_collection(buf, indent, Braces::Curly, *impls, Newlines::No);
            }
            HasImpls::SpaceBefore(impls, spaces) => {
                buf.newline();
                buf.indent(indent);
                fmt_comments_only(buf, spaces.iter(), NewlineAt::Bottom, indent);
                impls.format_with_options(buf, parens, Newlines::No, indent);
            }
            HasImpls::SpaceAfter(impls, spaces) => {
                impls.format_with_options(buf, parens, newlines, indent);
                fmt_comments_only(buf, spaces.iter(), NewlineAt::Bottom, indent);
            }
        }
    }
}

impl<'a> Formattable for HasAbility<'a> {
    fn is_multiline(&self) -> bool {
        match self {
            HasAbility::SpaceAfter(..) | HasAbility::SpaceBefore(..) => true,
            HasAbility::HasAbility { ability, impls } => {
                ability.is_multiline() || impls.map(|i| i.is_multiline()).unwrap_or(false)
            }
        }
    }

    fn format_with_options<'buf>(
        &self,
        buf: &mut Buf<'buf>,
        parens: Parens,
        newlines: Newlines,
        indent: u16,
    ) {
        match self {
            HasAbility::HasAbility { ability, impls } => {
                if newlines == Newlines::Yes {
                    buf.newline();
                    buf.indent(indent);
                }
                ability.format_with_options(buf, parens, newlines, indent);
                if let Some(impls) = impls {
                    buf.spaces(1);
                    impls.format_with_options(buf, parens, newlines, indent);
                }
            }
            HasAbility::SpaceBefore(ab, spaces) => {
                buf.newline();
                buf.indent(indent);
                fmt_comments_only(buf, spaces.iter(), NewlineAt::Bottom, indent);
                ab.format_with_options(buf, parens, Newlines::No, indent)
            }
            HasAbility::SpaceAfter(ab, spaces) => {
                ab.format_with_options(buf, parens, newlines, indent);
                fmt_comments_only(buf, spaces.iter(), NewlineAt::Bottom, indent);
            }
        }
    }
}

impl<'a> Formattable for HasAbilities<'a> {
    fn is_multiline(&self) -> bool {
        match self {
            HasAbilities::SpaceAfter(..) | HasAbilities::SpaceBefore(..) => true,
            HasAbilities::Has(has_abilities) => has_abilities.is_multiline(),
        }
    }

    fn format_with_options<'buf>(
        &self,
        buf: &mut Buf<'buf>,
        parens: Parens,
        newlines: Newlines,
        indent: u16,
    ) {
        match self {
            HasAbilities::Has(has_abilities) => {
                if newlines == Newlines::Yes {
                    buf.newline();
                    buf.indent(indent);
                }
                buf.push_str("has");
                buf.spaces(1);
                fmt_collection(buf, indent, Braces::Square, *has_abilities, Newlines::No);
            }
            HasAbilities::SpaceBefore(has_abilities, spaces) => {
                buf.newline();
                buf.indent(indent);
                fmt_comments_only(buf, spaces.iter(), NewlineAt::Bottom, indent);
                has_abilities.format_with_options(buf, parens, Newlines::No, indent)
            }
            HasAbilities::SpaceAfter(has_abilities, spaces) => {
                has_abilities.format_with_options(buf, parens, newlines, indent);
                fmt_comments_only(buf, spaces.iter(), NewlineAt::Bottom, indent);
            }
        }
    }
}
