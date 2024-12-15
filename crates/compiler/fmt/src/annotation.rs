use crate::{
    collection::{fmt_collection, Braces},
    expr::{format_spaces, merge_spaces_conservative},
    node::{Node, Nodify, Sp},
    pattern::pattern_lift_spaces_after,
    spaces::{fmt_comments_only, fmt_spaces, NewlineAt, INDENT},
    Buf,
};
use bumpalo::{collections::Vec, Bump};
use roc_parse::ast::{
    AbilityImpls, AssignedField, Collection, CommentOrNewline, Expr, ExtractSpaces, FunctionArrow,
    ImplementsAbilities, ImplementsAbility, ImplementsClause, Spaceable, Spaces, SpacesAfter,
    SpacesBefore, Tag, TypeAnnotation, TypeHeader,
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
    InCollection,
    InFunctionType,
    InApply,
    InOperator,
    InAsPattern,
    InApplyLastArg,
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

            TypeAnnotation::Wildcard
            | TypeAnnotation::Inferred
            | BoundVariable(_)
            | Malformed(_) => false,
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
                is_collection_multiline(fields)
            }

            Record { fields, ext } => {
                match ext {
                    Some(ann) if ann.value.is_multiline() => return true,
                    _ => {}
                }

                is_collection_multiline(fields)
            }

            TagUnion { tags, ext } => {
                match ext {
                    Some(ann) if ann.value.is_multiline() => return true,
                    _ => {}
                }

                !tags.final_comments().is_empty() || tags.iter().any(|tag| tag.value.is_multiline())
            }
        }
    }

    fn format_with_options(&self, buf: &mut Buf, parens: Parens, newlines: Newlines, indent: u16) {
        fmt_ty_ann(self, buf, indent, parens, newlines, false);
    }
}

fn fmt_ty_ann(
    me: &TypeAnnotation<'_>,
    buf: &mut Buf<'_>,

    indent: u16,
    parens: Parens,
    newlines: Newlines,
    newline_at_top: bool,
) {
    let me = ann_lift_spaces(buf.text.bump(), me);

    let self_is_multiline = me.item.is_multiline();

    if !me.before.is_empty() {
        buf.ensure_ends_with_newline();
        fmt_comments_only(buf, me.before.iter(), NewlineAt::Bottom, indent);
    }
    if newline_at_top {
        buf.ensure_ends_with_newline();
    }

    match &me.item {
        TypeAnnotation::SpaceBefore(_ann, _spaces) | TypeAnnotation::SpaceAfter(_ann, _spaces) => {
            unreachable!()
        }
        TypeAnnotation::Function(args, arrow, ret) => {
            let needs_parens = parens != Parens::NotNeeded;

            buf.indent(indent);

            if needs_parens {
                buf.push('(')
            }

            for (index, argument) in args.iter().enumerate() {
                let is_first = index == 0;

                if !is_first {
                    buf.indent(indent);
                    buf.push_str(",");
                    if !self_is_multiline {
                        buf.spaces(1);
                    }
                }

                let newline_at_top = !is_first && self_is_multiline;

                fmt_ty_ann(
                    &argument.value,
                    buf,
                    indent,
                    Parens::InFunctionType,
                    Newlines::Yes,
                    newline_at_top,
                );
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
        TypeAnnotation::Apply(pkg, name, arguments) => {
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
                                || !ty_is_outdentable(&a.value))
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
                    arg.item
                        .format_with_options(buf, Parens::InApply, Newlines::Yes, arg_indent);
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
        TypeAnnotation::BoundVariable(v) => {
            buf.indent(indent);
            buf.push_str(v)
        }
        TypeAnnotation::Wildcard => {
            buf.indent(indent);
            buf.push('*')
        }
        TypeAnnotation::Inferred => {
            buf.indent(indent);
            buf.push('_')
        }

        TypeAnnotation::TagUnion { tags, ext } => {
            fmt_tag_collection(buf, indent, *tags, newlines);
            fmt_ext(ext, buf, indent);
        }

        TypeAnnotation::Tuple { elems: fields, ext } => {
            fmt_ty_collection(buf, indent, Braces::Round, *fields, newlines);
            fmt_ext(ext, buf, indent);
        }

        TypeAnnotation::Record { fields, ext } => {
            fmt_collection(buf, indent, Braces::Curly, *fields, newlines);
            fmt_ext(ext, buf, indent);
        }

        TypeAnnotation::As(lhs, spaces, TypeHeader { name, vars }) => {
            let write_parens = parens == Parens::InAsPattern;

            if write_parens {
                buf.push('(')
            }

            buf.indent(indent);
            let lhs_indent = buf.cur_line_indent();
            lhs.value
                .format_with_options(buf, Parens::InAsPattern, Newlines::No, indent);
            buf.spaces(1);
            format_spaces(buf, spaces, newlines, indent);
            buf.indent(lhs_indent + INDENT);
            buf.push_str("as");
            buf.spaces(1);
            buf.push_str(name.value);
            for var in *vars {
                buf.spaces(1);
                var.value.format_with_options(
                    buf,
                    Parens::NotNeeded,
                    Newlines::No,
                    lhs_indent + INDENT,
                );
            }

            if write_parens {
                buf.push(')')
            }
        }

        TypeAnnotation::Where(annot, implements_clauses) => {
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
        TypeAnnotation::Malformed(raw) => {
            buf.indent(indent);
            buf.push_str(raw)
        }
    }
    if !me.after.is_empty() {
        fmt_comments_only(buf, me.after.iter(), NewlineAt::Bottom, indent);
    }
}

fn fmt_tag_collection<'a>(
    buf: &mut Buf<'_>,
    indent: u16,
    tags: Collection<'a, Loc<Tag<'a>>>,
    newlines: Newlines,
) {
    let arena = buf.text.bump();
    let mut new_items: Vec<'_, NodeSpaces<'_, Node<'_>>> = Vec::with_capacity_in(tags.len(), arena);

    let mut last_after: &[CommentOrNewline<'_>] = &[];

    for item in tags.items.iter() {
        let lifted = item.value.to_node(arena, Parens::NotNeeded);
        let before = merge_spaces_conservative(arena, last_after, lifted.before);
        last_after = lifted.after;
        new_items.push(NodeSpaces {
            before,
            item: lifted.item,
            after: &[],
        });
    }

    let final_comments = merge_spaces_conservative(arena, last_after, tags.final_comments());

    let new_items =
        Collection::with_items_and_comments(arena, new_items.into_bump_slice(), final_comments);

    fmt_collection(buf, indent, Braces::Square, new_items, newlines);
}

impl<'a> Nodify<'a> for Tag<'a> {
    fn to_node<'b>(&'a self, arena: &'b Bump, parens: Parens) -> Spaces<'b, Node<'b>>
    where
        'a: 'b,
    {
        match self {
            Tag::Apply { name, args } => {
                if args.is_empty() {
                    Spaces {
                        before: &[],
                        item: Node::Literal(name.value),
                        after: &[],
                    }
                } else {
                    let first = Node::Literal(name.value);
                    let mut new_args: Vec<'b, (Sp<'b>, Node<'b>)> =
                        Vec::with_capacity_in(args.len(), arena);
                    let mut last_after: &[CommentOrNewline<'_>] = &[];

                    for arg in args.iter() {
                        let lifted = arg.value.to_node(arena, Parens::InApply);
                        let before = merge_spaces_conservative(arena, last_after, lifted.before);
                        last_after = lifted.after;
                        new_args.push((before, lifted.item));
                    }

                    Spaces {
                        before: &[],
                        item: Node::Sequence(arena.alloc(first), new_args.into_bump_slice()),
                        after: last_after,
                    }
                }
            }
            Tag::SpaceBefore(inner, sp) => {
                let mut inner = inner.to_node(arena, parens);
                inner.before = merge_spaces_conservative(arena, sp, inner.before);
                inner
            }
            Tag::SpaceAfter(inner, sp) => {
                let mut inner = inner.to_node(arena, parens);
                inner.after = merge_spaces_conservative(arena, inner.after, sp);
                inner
            }
        }
    }
}

fn lower<'a, 'b: 'a>(
    arena: &'b Bump,
    lifted: Spaces<'b, TypeAnnotation<'b>>,
) -> TypeAnnotation<'b> {
    if lifted.before.is_empty() && lifted.after.is_empty() {
        return lifted.item;
    }
    if lifted.before.is_empty() {
        return TypeAnnotation::SpaceAfter(arena.alloc(lifted.item), lifted.after);
    }
    if lifted.after.is_empty() {
        return TypeAnnotation::SpaceBefore(arena.alloc(lifted.item), lifted.before);
    }
    TypeAnnotation::SpaceBefore(
        arena.alloc(TypeAnnotation::SpaceAfter(
            arena.alloc(lifted.item),
            lifted.after,
        )),
        lifted.before,
    )
}

fn fmt_ty_collection(
    buf: &mut Buf<'_>,
    indent: u16,
    braces: Braces,
    items: Collection<'_, Loc<TypeAnnotation<'_>>>,
    newlines: Newlines,
) {
    let arena = buf.text.bump();
    let mut new_items: Vec<'_, NodeSpaces<'_, Node<'_>>> =
        Vec::with_capacity_in(items.len(), arena);

    let mut last_after: &[CommentOrNewline<'_>] = &[];

    for (i, item) in items.items.iter().enumerate() {
        let parens = if i > 0 {
            Parens::InCollection
        } else {
            Parens::NotNeeded
        };
        let lifted = item.value.to_node(arena, parens);
        let before = merge_spaces_conservative(arena, last_after, lifted.before);
        last_after = lifted.after;
        new_items.push(NodeSpaces {
            before,
            item: lifted.item,
            after: &[],
        });
    }

    let final_comments = merge_spaces_conservative(arena, last_after, items.final_comments());

    let new_items =
        Collection::with_items_and_comments(arena, new_items.into_bump_slice(), final_comments);

    fmt_collection(buf, indent, braces, new_items, newlines)
}

fn fmt_ext(ext: &Option<&Loc<TypeAnnotation<'_>>>, buf: &mut Buf<'_>, indent: u16) {
    if let Some(loc_ext_ann) = *ext {
        let me = ann_lift_spaces(buf.text.bump(), &loc_ext_ann.value);
        let parens_needed = !me.before.is_empty() || ext_needs_parens(me.item);
        if parens_needed {
            // We need to make sure to not have whitespace before the ext of a type,
            // since that would make it parse as something else.
            buf.push('(');
            loc_ext_ann.value.format(buf, indent + INDENT);
            buf.indent(indent);
            buf.push(')');
        } else {
            loc_ext_ann.value.format(buf, indent + INDENT);
        }
    }
}

fn ext_needs_parens(item: TypeAnnotation<'_>) -> bool {
    match item {
        TypeAnnotation::Record { .. }
        | TypeAnnotation::TagUnion { .. }
        | TypeAnnotation::Tuple { .. }
        | TypeAnnotation::BoundVariable(..)
        | TypeAnnotation::Wildcard
        | TypeAnnotation::Inferred => false,
        TypeAnnotation::Apply(_module, _func, args) => !args.is_empty(),
        _ => true,
    }
}

pub fn ty_is_outdentable(mut rhs: &TypeAnnotation) -> bool {
    loop {
        match rhs {
            TypeAnnotation::SpaceBefore(sub_def, spaces) => {
                let is_only_newlines = spaces.iter().all(|s| s.is_newline());
                if !is_only_newlines || !sub_def.is_multiline() {
                    return false;
                }
                rhs = sub_def;
            }
            TypeAnnotation::SpaceAfter(sub_def, _) => {
                rhs = sub_def;
            }
            TypeAnnotation::Where(ann, _clauses) => {
                if !ann.is_multiline() {
                    return false;
                }
                rhs = &ann.value;
            }
            TypeAnnotation::Record { .. }
            | TypeAnnotation::TagUnion { .. }
            | TypeAnnotation::Tuple { .. } => return rhs.is_multiline(),
            _ => return false,
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
            buf.indent(indent);
            buf.push(':');
            buf.spaces(1);
            ann.value.format(buf, indent);
        }
        OptionalValue(name, spaces, ann) => {
            if is_multiline {
                buf.newline();
            }

            buf.indent(indent);
            buf.push_str(name.value);

            if !spaces.is_empty() {
                fmt_spaces(buf, spaces.iter(), indent);
            }

            buf.spaces(separator_spaces);
            buf.indent(indent);
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
            buf.indent(indent);
            buf.push(':');
            buf.spaces(1);
            ann.value.format(buf, indent);
        }
        LabelOnly(name) => {
            if is_multiline {
                buf.newline();
            }

            buf.indent(indent);
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
                }
                buf.indent(indent);
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

pub fn ann_lift_spaces<'a, 'b: 'a>(
    arena: &'a Bump,
    ann: &TypeAnnotation<'b>,
) -> Spaces<'a, TypeAnnotation<'a>> {
    match ann {
        TypeAnnotation::Apply(module, func, args) => {
            if args.is_empty() {
                return Spaces {
                    item: *ann,
                    before: &[],
                    after: &[],
                };
            }
            let mut new_args = Vec::with_capacity_in(args.len(), arena);

            if !args.is_empty() {
                for arg in args.iter().take(args.len() - 1) {
                    let lifted = ann_lift_spaces(arena, &arg.value);
                    new_args.push(Loc::at(arg.region, lower(arena, lifted)));
                }
            }

            let after = if let Some(last) = args.last() {
                let lifted = ann_lift_spaces(arena, &last.value);
                if lifted.before.is_empty() {
                    new_args.push(Loc::at(last.region, lifted.item));
                } else {
                    new_args.push(Loc::at(
                        last.region,
                        TypeAnnotation::SpaceBefore(arena.alloc(lifted.item), lifted.before),
                    ));
                }
                lifted.after
            } else {
                &[]
            };

            Spaces {
                before: &[],
                item: TypeAnnotation::Apply(module, func, new_args.into_bump_slice()),
                after,
            }
        }
        TypeAnnotation::Function(args, purity, res) => {
            let new_args = arena.alloc_slice_copy(args);
            let before = if let Some(first) = new_args.first_mut() {
                let lifted = ann_lift_spaces_before(arena, &first.value);
                first.value = lifted.item;
                lifted.before
            } else {
                &[]
            };
            let new_res = ann_lift_spaces_after(arena, &res.value);
            let new_ann = TypeAnnotation::Function(
                new_args,
                *purity,
                arena.alloc(Loc::at_zero(new_res.item)),
            );
            Spaces {
                before,
                item: new_ann,
                after: new_res.after,
            }
        }
        TypeAnnotation::SpaceBefore(expr, spaces) => {
            let mut inner = ann_lift_spaces(arena, expr);
            inner.before = merge_spaces_conservative(arena, spaces, inner.before);
            inner
        }
        TypeAnnotation::SpaceAfter(expr, spaces) => {
            let mut inner = ann_lift_spaces(arena, expr);
            inner.after = merge_spaces_conservative(arena, inner.after, spaces);
            inner
        }
        TypeAnnotation::Tuple { elems, ext } => {
            if let Some(ext) = ext {
                let lifted = ann_lift_spaces_after(arena, &ext.value);
                Spaces {
                    before: &[],
                    item: TypeAnnotation::Tuple {
                        elems: *elems,
                        ext: Some(arena.alloc(Loc::at_zero(lifted.item))),
                    },
                    after: lifted.after,
                }
            } else {
                Spaces {
                    before: &[],
                    item: *ann,
                    after: &[],
                }
            }
        }
        TypeAnnotation::Record { fields, ext } => {
            if let Some(ext) = ext {
                let lifted = ann_lift_spaces_after(arena, &ext.value);
                Spaces {
                    before: &[],
                    item: TypeAnnotation::Record {
                        fields: *fields,
                        ext: Some(arena.alloc(Loc::at_zero(lifted.item))),
                    },
                    after: lifted.after,
                }
            } else {
                Spaces {
                    before: &[],
                    item: *ann,
                    after: &[],
                }
            }
        }
        TypeAnnotation::TagUnion { ext, tags } => {
            if let Some(ext) = ext {
                let lifted = ann_lift_spaces_after(arena, &ext.value);
                Spaces {
                    before: &[],
                    item: TypeAnnotation::TagUnion {
                        ext: Some(arena.alloc(Loc::at_zero(lifted.item))),
                        tags: *tags,
                    },
                    after: lifted.after,
                }
            } else {
                Spaces {
                    before: &[],
                    item: *ann,
                    after: &[],
                }
            }
        }
        TypeAnnotation::BoundVariable(_)
        | TypeAnnotation::Inferred
        | TypeAnnotation::Wildcard
        | TypeAnnotation::Malformed(_) => Spaces {
            before: &[],
            item: *ann,
            after: &[],
        },
        TypeAnnotation::Where(inner, clauses) => {
            let new_inner = ann_lift_spaces_before(arena, &inner.value);
            let new_clauses = arena.alloc_slice_copy(clauses);
            let after = if let Some(last) = new_clauses.last_mut() {
                let lifted = implements_clause_lift_spaces_after(arena, &last.value);
                last.value = lifted.item;
                lifted.after
            } else {
                &[]
            };
            Spaces {
                before: new_inner.before,
                item: TypeAnnotation::Where(arena.alloc(Loc::at_zero(new_inner.item)), new_clauses),
                after,
            }
        }
        TypeAnnotation::As(ann, comments, type_header) => {
            let new_ann = ann_lift_spaces_before(arena, &ann.value);
            let new_header = type_head_lift_spaces_after(arena, type_header);
            Spaces {
                before: new_ann.before,
                item: TypeAnnotation::As(
                    arena.alloc(Loc::at_zero(new_ann.item)),
                    comments,
                    new_header.item,
                ),
                after: new_header.after,
            }
        }
    }
}

fn implements_clause_lift_spaces_after<'a, 'b: 'a>(
    arena: &'a Bump,
    value: &ImplementsClause<'b>,
) -> SpacesAfter<'a, ImplementsClause<'a>> {
    let new_abilities = arena.alloc_slice_copy(value.abilities);
    let after = if let Some(last) = new_abilities.last_mut() {
        let lifted = ann_lift_spaces_after(arena, &last.value);
        last.value = lifted.item;
        lifted.after
    } else {
        &[]
    };
    SpacesAfter {
        item: ImplementsClause {
            var: value.var,
            abilities: new_abilities,
        },
        after,
    }
}

pub fn ann_lift_spaces_before<'a, 'b: 'a>(
    arena: &'a Bump,
    ann: &TypeAnnotation<'b>,
) -> SpacesBefore<'a, TypeAnnotation<'a>> {
    let lifted = ann_lift_spaces(arena, ann);
    SpacesBefore {
        before: lifted.before,
        item: lifted.item.maybe_after(arena, lifted.after),
    }
}

pub fn ann_lift_spaces_after<'a, 'b: 'a>(
    arena: &'a Bump,
    ann: &TypeAnnotation<'b>,
) -> SpacesAfter<'a, TypeAnnotation<'a>> {
    let lifted = ann_lift_spaces(arena, ann);
    SpacesAfter {
        item: lifted.item.maybe_before(arena, lifted.before),
        after: lifted.after,
    }
}

pub fn type_head_lift_spaces_after<'a, 'b: 'a>(
    arena: &'a Bump,
    header: &TypeHeader<'b>,
) -> SpacesAfter<'a, TypeHeader<'a>> {
    let new_vars = arena.alloc_slice_copy(header.vars);
    let after = if let Some(last) = new_vars.last_mut() {
        let lifted = pattern_lift_spaces_after(arena, &last.value);
        last.value = lifted.item;
        lifted.after
    } else {
        &[]
    };
    SpacesAfter {
        item: TypeHeader {
            name: header.name,
            vars: new_vars,
        },
        after,
    }
}

impl<'a> Nodify<'a> for TypeAnnotation<'a> {
    fn to_node<'b>(&'a self, arena: &'b Bump, parens: Parens) -> Spaces<'b, Node<'b>>
    where
        'a: 'b,
    {
        match self {
            TypeAnnotation::Apply(module, func, args) => {
                if args.is_empty() {
                    return Spaces {
                        item: Node::TypeAnnotation(*self),
                        before: &[],
                        after: &[],
                    };
                }
                let mut new_args = Vec::with_capacity_in(args.len(), arena);

                if !args.is_empty() {
                    for arg in args.iter().take(args.len() - 1) {
                        let lifted = ann_lift_spaces(arena, &arg.value);
                        new_args.push(Loc::at(arg.region, lower(arena, lifted)));
                    }
                }

                let after = if let Some(last) = args.last() {
                    let lifted = ann_lift_spaces(arena, &last.value);
                    if lifted.before.is_empty() {
                        new_args.push(Loc::at(last.region, lifted.item));
                    } else {
                        new_args.push(Loc::at(
                            last.region,
                            TypeAnnotation::SpaceBefore(arena.alloc(lifted.item), lifted.before),
                        ));
                    }
                    lifted.after
                } else {
                    &[]
                };

                let item = Node::TypeAnnotation(TypeAnnotation::Apply(
                    module,
                    func,
                    new_args.into_bump_slice(),
                ));

                if parens == Parens::InApply {
                    parens_around_node(
                        arena,
                        Spaces {
                            before: &[],
                            item,
                            after,
                        },
                    )
                } else {
                    Spaces {
                        before: &[],
                        item,
                        after,
                    }
                }
            }
            TypeAnnotation::SpaceBefore(expr, spaces) => {
                let mut inner = expr.to_node(arena, parens);
                inner.before = merge_spaces_conservative(arena, spaces, inner.before);
                inner
            }
            TypeAnnotation::SpaceAfter(expr, spaces) => {
                let mut inner = expr.to_node(arena, parens);
                inner.after = merge_spaces_conservative(arena, inner.after, spaces);
                inner
            }
            TypeAnnotation::Function(args, purity, res) => {
                let new_args = arena.alloc_slice_copy(args);
                let before = if let Some(first) = new_args.first_mut() {
                    let lifted = ann_lift_spaces_before(arena, &first.value);
                    first.value = lifted.item;
                    lifted.before
                } else {
                    &[]
                };
                let new_res = ann_lift_spaces_after(arena, &res.value);
                let new_ann = TypeAnnotation::Function(
                    new_args,
                    *purity,
                    arena.alloc(Loc::at_zero(new_res.item)),
                );
                let inner = Spaces {
                    before,
                    item: Node::TypeAnnotation(new_ann),
                    after: new_res.after,
                };
                if parens == Parens::InCollection || parens == Parens::InApply {
                    parens_around_node(arena, inner)
                } else {
                    inner
                }
            }
            _ => {
                let lifted = ann_lift_spaces(arena, self);
                Spaces {
                    before: lifted.before,
                    item: Node::TypeAnnotation(lifted.item),
                    after: lifted.after,
                }
            }
        }
    }
}

fn parens_around_node<'a, 'b: 'a>(
    arena: &'a Bump,
    item: Spaces<'b, Node<'b>>,
) -> Spaces<'a, Node<'a>> {
    Spaces {
        before: &[],
        item: Node::DelimitedSequence(
            Braces::Round,
            arena.alloc_slice_copy(&[(item.before, item.item)]),
            item.after,
        ),
        after: &[],
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct NodeSpaces<'a, T> {
    pub before: &'a [CommentOrNewline<'a>],
    pub item: T,
    pub after: &'a [CommentOrNewline<'a>],
}

impl<'a, T: Copy> ExtractSpaces<'a> for NodeSpaces<'a, T> {
    type Item = T;

    fn extract_spaces(&self) -> Spaces<'a, T> {
        Spaces {
            before: self.before,
            item: self.item,
            after: self.after,
        }
    }
}

impl<'a, V: Formattable> Formattable for NodeSpaces<'a, V> {
    fn is_multiline(&self) -> bool {
        !self.before.is_empty() || !self.after.is_empty() || self.item.is_multiline()
    }

    fn format_with_options(
        &self,
        buf: &mut Buf,
        parens: crate::annotation::Parens,
        newlines: Newlines,

        indent: u16,
    ) {
        fmt_spaces(buf, self.before.iter(), indent);
        self.item.format_with_options(buf, parens, newlines, indent);
        fmt_spaces(buf, self.after.iter(), indent);
    }
}
