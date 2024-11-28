use crate::{
    collection::{fmt_collection, Braces},
    spaces::{fmt_comments_only, fmt_spaces, NewlineAt, INDENT},
    Buf,
};
use roc_parse::ast::{
    AbilityImpls, AssignedField, Collection, Expr, ExtractSpaces, FunctionArrow,
    ImplementsAbilities, ImplementsAbility, ImplementsClause, Tag, TypeAnnotation, TypeHeader,
};
use roc_parse::ident::UppercaseIdent;
use roc_region::all::Loc;

/// Does an AST node need parens around it?
///
/// Usually not, but there are a few cases where it may be required
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
///
///  3. In a chain of binary operators, things like nested defs require parens.
///
///    a + (
///       x = 3
///       x + 1
///    )
#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum Parens {
    NotNeeded,
    InFunctionType,
    InApply,
    InOperator,
    InAsPattern,
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

    fn format_with_options(&self, buf: &mut Buf, _parens: Parens, _newlines: Newlines, indent: u16);

    fn format(&self, buf: &mut Buf, indent: u16) {
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

    fn format_with_options(&self, buf: &mut Buf, parens: Parens, newlines: Newlines, indent: u16) {
        (*self).format_with_options(buf, parens, newlines, indent)
    }

    fn format(&self, buf: &mut Buf, indent: u16) {
        (*self).format(buf, indent)
    }
}

pub fn is_collection_multiline<T: Formattable>(collection: &Collection<'_, T>) -> bool {
    // if there are any comments, they must go on their own line
    // because otherwise they'd comment out the closing delimiter
    !collection.final_comments().is_empty() ||
    // if any of the items in the collection are multiline,
    // then the whole collection must be multiline
    collection.items.iter().any(Formattable::is_multiline)
}

/// A Located formattable value is also formattable
impl<T> Formattable for Loc<T>
where
    T: Formattable,
{
    fn is_multiline(&self) -> bool {
        self.value.is_multiline()
    }

    fn format_with_options(&self, buf: &mut Buf, parens: Parens, newlines: Newlines, indent: u16) {
        self.value
            .format_with_options(buf, parens, newlines, indent)
    }

    fn format(&self, buf: &mut Buf, indent: u16) {
        self.value.format(buf, indent)
    }
}

impl<'a> Formattable for UppercaseIdent<'a> {
    fn is_multiline(&self) -> bool {
        false
    }

    fn format_with_options(
        &self,
        buf: &mut Buf,
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
            Function(args, _arrow, result) => {
                result.value.is_multiline()
                    || args.iter().any(|loc_arg| loc_arg.value.is_multiline())
            }
            Apply(_, _, args) => args.iter().any(|loc_arg| loc_arg.value.is_multiline()),
            As(lhs, _, _) => lhs.value.is_multiline(),

            Where(annot, has_clauses) => {
                annot.is_multiline() || has_clauses.iter().any(|has| has.is_multiline())
            }

            Tuple { elems: fields, ext } => {
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

    fn format_with_options(&self, buf: &mut Buf, parens: Parens, newlines: Newlines, indent: u16) {
        use roc_parse::ast::TypeAnnotation::*;

        let self_is_multiline = self.is_multiline();

        match self {
            Function(args, arrow, ret) => {
                let needs_parens = parens != Parens::NotNeeded;

                buf.indent(indent);

                if needs_parens {
                    buf.push('(')
                }

                let mut it = args.iter().enumerate().peekable();

                while let Some((index, argument)) = it.next() {
                    let is_first = index == 0;
                    let is_multiline = &argument.value.is_multiline();

                    if !is_first && !is_multiline && self_is_multiline {
                        buf.newline();
                    }

                    argument.value.format_with_options(
                        buf,
                        Parens::InFunctionType,
                        Newlines::Yes,
                        indent,
                    );

                    if it.peek().is_some() {
                        buf.push_str(",");
                        if !self_is_multiline {
                            buf.spaces(1);
                        }
                    }
                }

                if self_is_multiline {
                    buf.newline();
                    buf.indent(indent);
                } else {
                    buf.spaces(1);
                }

                match arrow {
                    FunctionArrow::Pure => buf.push_str("->"),
                    FunctionArrow::Effectful => buf.push_str("=>"),
                }

                buf.spaces(1);

                ret.value
                    .format_with_options(buf, Parens::InFunctionType, Newlines::No, indent);

                if needs_parens {
                    buf.push(')')
                }
            }
            Apply(pkg, name, arguments) => {
                buf.indent(indent);
                let write_parens = parens == Parens::InApply && !arguments.is_empty();

                if write_parens {
                    buf.push('(')
                }

                if !pkg.is_empty() {
                    buf.push_str(pkg);
                    buf.push('.');
                }

                buf.push_str(name);

                let needs_indent = except_last(arguments).any(|a| a.is_multiline())
                    || arguments
                        .last()
                        .map(|a| {
                            a.is_multiline()
                                && (!a.extract_spaces().before.is_empty()
                                    || !is_outdentable(&a.value))
                        })
                        .unwrap_or_default();

                let arg_indent = if needs_indent {
                    indent + INDENT
                } else {
                    indent
                };

                for arg in arguments.iter() {
                    if needs_indent {
                        let arg = arg.extract_spaces();
                        fmt_spaces(buf, arg.before.iter(), arg_indent);
                        buf.ensure_ends_with_newline();
                        arg.item.format_with_options(
                            buf,
                            Parens::InApply,
                            Newlines::Yes,
                            arg_indent,
                        );
                        fmt_spaces(buf, arg.after.iter(), arg_indent);
                    } else {
                        buf.spaces(1);
                        arg.format_with_options(buf, Parens::InApply, Newlines::No, arg_indent);
                    }
                }

                if write_parens {
                    buf.push(')')
                }
            }
            BoundVariable(v) => {
                buf.indent(indent);
                buf.push_str(v)
            }
            Wildcard => {
                buf.indent(indent);
                buf.push('*')
            }
            Inferred => {
                buf.indent(indent);
                buf.push('_')
            }

            TagUnion { tags, ext } => {
                fmt_collection(buf, indent, Braces::Square, *tags, newlines);

                if let Some(loc_ext_ann) = *ext {
                    loc_ext_ann.value.format(buf, indent);
                }
            }

            Tuple { elems: fields, ext } => {
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

            Where(annot, implements_clauses) => {
                annot.format_with_options(buf, parens, newlines, indent);
                if implements_clauses
                    .iter()
                    .any(|implements| implements.is_multiline())
                {
                    buf.newline();
                    buf.indent(indent);
                } else {
                    buf.spaces(1);
                }
                for (i, has) in implements_clauses.iter().enumerate() {
                    buf.push_str(if i == 0 {
                        roc_parse::keyword::WHERE
                    } else {
                        ","
                    });
                    buf.spaces(1);
                    has.format_with_options(buf, parens, newlines, indent);
                }
            }

            SpaceBefore(ann, spaces) => {
                buf.ensure_ends_with_newline();
                fmt_comments_only(buf, spaces.iter(), NewlineAt::Bottom, indent);
                ann.format_with_options(buf, parens, newlines, indent)
            }
            SpaceAfter(ann, spaces) => {
                ann.format_with_options(buf, parens, newlines, indent);
                fmt_comments_only(buf, spaces.iter(), NewlineAt::Bottom, indent);
            }
            Malformed(raw) => {
                buf.indent(indent);
                buf.push_str(raw)
            }
        }
    }
}

fn is_outdentable(ann: &TypeAnnotation) -> bool {
    matches!(
        ann.extract_spaces().item,
        TypeAnnotation::Tuple { .. } | TypeAnnotation::Record { .. }
    )
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

    fn format_with_options(&self, buf: &mut Buf, _parens: Parens, newlines: Newlines, indent: u16) {
        // we abuse the `Newlines` type to decide between multiline or single-line layout
        format_assigned_field_help(self, buf, indent, 1, newlines == Newlines::Yes);
    }
}

impl<'a> Formattable for AssignedField<'a, Expr<'a>> {
    fn is_multiline(&self) -> bool {
        is_multiline_assigned_field_help(self)
    }

    fn format_with_options(&self, buf: &mut Buf, _parens: Parens, newlines: Newlines, indent: u16) {
        // we abuse the `Newlines` type to decide between multiline or single-line layout
        format_assigned_field_help(self, buf, indent, 0, newlines == Newlines::Yes);
    }
}

fn is_multiline_assigned_field_help<T: Formattable>(afield: &AssignedField<'_, T>) -> bool {
    use self::AssignedField::*;

    match afield {
        RequiredValue(_, spaces, ann)
        | OptionalValue(_, spaces, ann)
        | IgnoredValue(_, spaces, ann) => !spaces.is_empty() || ann.value.is_multiline(),
        LabelOnly(_) => false,
        AssignedField::SpaceBefore(_, _) | AssignedField::SpaceAfter(_, _) => true,
    }
}

fn format_assigned_field_help<T>(
    zelf: &AssignedField<T>,
    buf: &mut Buf,
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
            buf.spaces(1);
            ann.value.format(buf, indent);
        }
        IgnoredValue(name, spaces, ann) => {
            if is_multiline {
                buf.newline();
            }

            buf.indent(indent);
            buf.push('_');
            buf.push_str(name.value);

            if !spaces.is_empty() {
                fmt_spaces(buf, spaces.iter(), indent);
            }

            buf.spaces(separator_spaces);
            buf.push(':');
            buf.spaces(1);
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
    }
}

impl<'a> Formattable for Tag<'a> {
    fn is_multiline(&self) -> bool {
        use self::Tag::*;

        match self {
            Apply { args, .. } => args.iter().any(|arg| arg.value.is_multiline()),
            Tag::SpaceBefore(_, _) | Tag::SpaceAfter(_, _) => true,
        }
    }

    fn format_with_options(
        &self,
        buf: &mut Buf,
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
        }
    }
}

impl<'a> Formattable for ImplementsClause<'a> {
    fn is_multiline(&self) -> bool {
        // No, always put abilities in an "implements" clause on one line
        false
    }

    fn format_with_options(&self, buf: &mut Buf, parens: Parens, newlines: Newlines, indent: u16) {
        buf.push_str(self.var.value.extract_spaces().item);
        buf.spaces(1);
        buf.push_str(roc_parse::keyword::IMPLEMENTS);
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

impl<'a> Formattable for AbilityImpls<'a> {
    fn is_multiline(&self) -> bool {
        match self {
            AbilityImpls::SpaceBefore(_, _) | AbilityImpls::SpaceAfter(_, _) => true,
            AbilityImpls::AbilityImpls(impls) => is_collection_multiline(impls),
        }
    }

    fn format_with_options(&self, buf: &mut Buf, parens: Parens, newlines: Newlines, indent: u16) {
        match self {
            AbilityImpls::AbilityImpls(impls) => {
                if newlines == Newlines::Yes {
                    buf.newline();
                    buf.indent(indent);
                }
                fmt_collection(buf, indent, Braces::Curly, *impls, Newlines::No);
            }
            AbilityImpls::SpaceBefore(impls, spaces) => {
                buf.newline();
                buf.indent(indent);
                fmt_comments_only(buf, spaces.iter(), NewlineAt::Bottom, indent);
                impls.format_with_options(buf, parens, Newlines::No, indent);
            }
            AbilityImpls::SpaceAfter(impls, spaces) => {
                impls.format_with_options(buf, parens, newlines, indent);
                fmt_comments_only(buf, spaces.iter(), NewlineAt::Bottom, indent);
            }
        }
    }
}

impl<'a> Formattable for ImplementsAbility<'a> {
    fn is_multiline(&self) -> bool {
        match self {
            ImplementsAbility::SpaceAfter(..) | ImplementsAbility::SpaceBefore(..) => true,
            ImplementsAbility::ImplementsAbility { ability, impls } => {
                ability.is_multiline() || impls.map(|i| i.is_multiline()).unwrap_or(false)
            }
        }
    }

    fn format_with_options(&self, buf: &mut Buf, parens: Parens, newlines: Newlines, indent: u16) {
        match self {
            ImplementsAbility::ImplementsAbility { ability, impls } => {
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
            ImplementsAbility::SpaceBefore(ab, spaces) => {
                buf.newline();
                buf.indent(indent);
                fmt_comments_only(buf, spaces.iter(), NewlineAt::Bottom, indent);
                ab.format_with_options(buf, parens, Newlines::No, indent)
            }
            ImplementsAbility::SpaceAfter(ab, spaces) => {
                ab.format_with_options(buf, parens, newlines, indent);
                fmt_comments_only(buf, spaces.iter(), NewlineAt::Bottom, indent);
            }
        }
    }
}

impl<'a> Formattable for ImplementsAbilities<'a> {
    fn is_multiline(&self) -> bool {
        match self {
            ImplementsAbilities::SpaceAfter(..) | ImplementsAbilities::SpaceBefore(..) => true,
            ImplementsAbilities::Implements(has_abilities) => {
                is_collection_multiline(has_abilities)
            }
        }
    }

    fn format_with_options(&self, buf: &mut Buf, parens: Parens, newlines: Newlines, indent: u16) {
        match self {
            ImplementsAbilities::Implements(has_abilities) => {
                if newlines == Newlines::Yes {
                    buf.newline();
                    buf.indent(indent);
                }
                buf.push_str(roc_parse::keyword::IMPLEMENTS);
                buf.spaces(1);
                fmt_collection(buf, indent, Braces::Square, *has_abilities, Newlines::No);
            }
            ImplementsAbilities::SpaceBefore(has_abilities, spaces) => {
                buf.newline();
                buf.indent(indent);
                fmt_comments_only(buf, spaces.iter(), NewlineAt::Bottom, indent);
                has_abilities.format_with_options(buf, parens, Newlines::No, indent)
            }
            ImplementsAbilities::SpaceAfter(has_abilities, spaces) => {
                has_abilities.format_with_options(buf, parens, newlines, indent);
                fmt_comments_only(buf, spaces.iter(), NewlineAt::Bottom, indent);
            }
        }
    }
}

pub fn except_last<T>(items: &[T]) -> impl Iterator<Item = &T> {
    if items.is_empty() {
        items.iter()
    } else {
        items[..items.len() - 1].iter()
    }
}
