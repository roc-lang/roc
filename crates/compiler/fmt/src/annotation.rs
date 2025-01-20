use crate::{
    collection::{fmt_collection, Braces},
    expr::{expr_lift_spaces, expr_lift_spaces_after, expr_prec, merge_spaces_conservative},
    node::{
        parens_around_node, DelimitedItem, Item, Node, NodeInfo, NodeSequenceBuilder, Nodify, Prec,
        Sp,
    },
    pattern::snakify_camel_ident,
    spaces::{fmt_comments_only, fmt_spaces, NewlineAt, INDENT},
    Buf, MigrationFlags,
};
use bumpalo::{
    collections::{String, Vec},
    Bump,
};
use roc_parse::{
    ast::{
        AbilityImpls, AssignedField, Collection, CommentOrNewline, Expr, ExtractSpaces,
        FunctionArrow, ImplementsAbilities, ImplementsAbility, ImplementsClause, Spaceable, Spaces,
        SpacesAfter, SpacesBefore, Tag, TypeAnnotation, TypeHeader,
    },
    expr::{merge_spaces, RecordValuePrefix},
};
use roc_parse::{
    ast::{Spaced, TypeVar},
    ident::UppercaseIdent,
};
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
    InClosurePattern,
    InPncApplyFunc,
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

    fn format_with_options(&self, buf: &mut Buf, parens: Parens, _newlines: Newlines, indent: u16) {
        fmt_ty_ann(self, buf, indent, parens, false);
    }
}

fn fmt_ty_ann(
    me: &TypeAnnotation<'_>,
    buf: &mut Buf<'_>,
    indent: u16,
    parens: Parens,
    newline_at_top: bool,
) {
    let me = ann_lift_spaces(buf.text.bump(), me);

    if !me.before.is_empty() {
        buf.ensure_ends_with_newline();
        fmt_comments_only(buf, me.before.iter(), NewlineAt::Bottom, indent);
    }
    if newline_at_top {
        buf.ensure_ends_with_newline();
    }

    me.item
        .to_node(buf.text.bump(), buf.flags())
        .add_parens(buf.text.bump(), parens)
        .node
        .format(buf, indent);

    if !me.after.is_empty() {
        fmt_comments_only(buf, me.after.iter(), NewlineAt::Bottom, indent);
    }
}

impl<'a> Nodify<'a> for Tag<'a> {
    fn to_node<'b>(&'a self, arena: &'b Bump, flags: MigrationFlags) -> NodeInfo<'b>
    where
        'a: 'b,
    {
        match self {
            Tag::Apply { name, args } => {
                if args.is_empty() {
                    NodeInfo::item(Node::Literal(name.value))
                } else {
                    let first = Node::Literal(name.value);
                    let mut new_args: Vec<'b, (Sp<'b>, Node<'b>)> =
                        Vec::with_capacity_in(args.len(), arena);
                    let mut last_after: &[CommentOrNewline<'_>] = &[];

                    for arg in args.iter() {
                        let lifted = arg
                            .value
                            .to_node(arena, flags)
                            .add_parens(arena, Parens::InApply);
                        let before = merge_spaces_conservative(arena, last_after, lifted.before);
                        last_after = lifted.after;
                        new_args.push((Sp::with_space(before), lifted.node));
                    }

                    NodeInfo {
                        before: &[],
                        node: Node::Sequence {
                            first: arena.alloc(first),
                            extra_indent_for_rest: true,
                            rest: new_args.into_bump_slice(),
                        },
                        after: last_after,
                        needs_indent: true,
                        prec: Prec::Apply,
                    }
                }
            }
            Tag::SpaceBefore(inner, sp) => {
                let mut inner = inner.to_node(arena, flags);
                inner.before = merge_spaces_conservative(arena, sp, inner.before);
                inner
            }
            Tag::SpaceAfter(inner, sp) => {
                let mut inner = inner.to_node(arena, flags);
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

impl<'a> Nodify<'a> for AssignedField<'a, TypeAnnotation<'a>> {
    fn to_node<'b>(&'a self, arena: &'b Bump, flags: MigrationFlags) -> NodeInfo<'b>
    where
        'a: 'b,
    {
        match self {
            AssignedField::WithValue {
                ignored,
                loc_label,
                before_prefix,
                prefix,
                loc_val,
            } => {
                let label = if *ignored {
                    let mut label = String::with_capacity_in(loc_label.value.len() + 1, arena);
                    label.push('_');
                    label.push_str(loc_label.value);
                    label.into_bump_str()
                } else {
                    loc_label.value
                };
                let prefix_text = match prefix {
                    RecordValuePrefix::Colon => ":",
                    RecordValuePrefix::DoubleQuestion => "??",
                };

                assigned_field_value_to_node(
                    label,
                    arena,
                    &before_prefix,
                    &loc_val.value,
                    prefix_text,
                    flags,
                )
            }

            AssignedField::WithoutValue { ignored, loc_label } => {
                let label = if *ignored {
                    let mut label = String::with_capacity_in(loc_label.value.len() + 1, arena);
                    label.push('_');
                    label.push_str(loc_label.value);
                    label.into_bump_str()
                } else {
                    loc_label.value
                };

                NodeInfo {
                    before: &[],
                    node: Node::Literal(label),
                    after: &[],
                    needs_indent: true,
                    prec: Prec::Term,
                }
            }

            AssignedField::SpreadValue(opt_spread) => NodeInfo {
                before: &[],
                node: Node::Spread {
                    item: opt_spread
                        .map(|spread| &*arena.alloc(Node::TypeAnnotation(spread.value))),
                },
                after: &[],
                // TODO: validate with Joshua or Anthony that this is what should be done
                needs_indent: true,
                prec: Prec::Term,
            },

            AssignedField::SpaceBefore(inner, sp) => {
                let mut inner = inner.to_node(arena, flags);
                inner.before = merge_spaces_conservative(arena, sp, inner.before);
                inner
            }
            AssignedField::SpaceAfter(inner, sp) => {
                let mut inner = inner.to_node(arena, flags);
                inner.after = merge_spaces_conservative(arena, inner.after, sp);
                inner
            }
        }
    }
}

fn assigned_field_value_to_node<'a, 'b>(
    name: &'b str,
    arena: &'b Bump,
    before_sep: &'a [CommentOrNewline<'a>],
    value: &'a TypeAnnotation<'a>,
    sep: &'static str,
    flags: MigrationFlags,
) -> NodeInfo<'b>
where
    'a: 'b,
{
    let field_name = if flags.snakify {
        let mut buf = Buf::new_in(arena, flags);
        buf.indent(0); // Take out of beginning of line
        snakify_camel_ident(&mut buf, name);
        let s: &str = arena.alloc_str(buf.as_str());
        s
    } else {
        name
    };
    let first = Node::Literal(field_name);

    let mut b = NodeSequenceBuilder::new(arena, first, 2, false);

    b.push(Sp::with_space(before_sep), Node::Literal(sep));

    let value_lifted = value.to_node(arena, flags);

    b.push(Sp::with_space(value_lifted.before), value_lifted.node);

    NodeInfo {
        before: &[],
        node: b.build(),
        after: value_lifted.after,
        needs_indent: true,
        prec: Prec::Term,
    }
}

fn is_multiline_assigned_field_help<T: Formattable>(afield: &AssignedField<'_, T>) -> bool {
    match afield {
        AssignedField::WithValue {
            before_prefix,
            loc_val,
            ..
        } => !before_prefix.is_empty() || loc_val.is_multiline(),
        AssignedField::WithoutValue { .. } => false,
        AssignedField::SpreadValue(opt_spread) => {
            opt_spread.is_some_and(|spread| spread.is_multiline())
        }
        AssignedField::SpaceBefore(_, _) | AssignedField::SpaceAfter(_, _) => true,
    }
}

fn format_assigned_field_help<T>(
    assigned_field: &AssignedField<T>,
    buf: &mut Buf,
    indent: u16,
    separator_spaces: usize,
    is_multiline: bool,
) where
    T: Formattable,
{
    match assigned_field {
        AssignedField::WithValue {
            ignored,
            loc_label,
            before_prefix,
            prefix,
            loc_val,
        } => {
            if is_multiline {
                buf.newline();
            }

            buf.indent(indent);
            if *ignored {
                buf.push('_');
            }

            if buf.flags().snakify {
                snakify_camel_ident(buf, loc_label.value);
            } else {
                buf.push_str(loc_label.value);
            }

            if !before_prefix.is_empty() {
                fmt_spaces(buf, before_prefix.iter(), indent);
            }

            buf.spaces(separator_spaces);
            buf.indent(indent);

            match prefix {
                RecordValuePrefix::Colon => buf.push(':'),
                RecordValuePrefix::DoubleQuestion => buf.push_str("??"),
            }

            buf.spaces(1);

            loc_val.value.format(buf, indent);
        }
        AssignedField::WithoutValue { ignored, loc_label } => {
            if is_multiline {
                buf.newline();
            }

            buf.indent(indent);

            if *ignored {
                buf.push('_');
            }
            if buf.flags().snakify {
                snakify_camel_ident(buf, loc_label.value);
            } else {
                buf.push_str(loc_label.value);
            }
        }
        AssignedField::SpreadValue(opt_spread) => {
            if is_multiline {
                buf.newline();
            }

            buf.indent(indent);

            buf.push_str("..");

            if let Some(spread) = opt_spread {
                spread.format(buf, indent);
            }
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
        self.before_implements_kw.iter().any(|s| s.is_comment())
            || self.after_implements_kw.iter().any(|s| s.is_comment())
            || is_collection_multiline(&self.item.value)
    }

    fn format_with_options(&self, buf: &mut Buf, _parens: Parens, newlines: Newlines, indent: u16) {
        if !self.before_implements_kw.is_empty() {
            buf.newline();
            buf.indent(indent);
            fmt_comments_only(
                buf,
                self.before_implements_kw.iter(),
                NewlineAt::Bottom,
                indent,
            );
        }
        if newlines == Newlines::Yes {
            buf.ensure_ends_with_newline();
        }
        buf.indent(indent);
        buf.push_str(roc_parse::keyword::IMPLEMENTS);
        if !self.after_implements_kw.is_empty() {
            fmt_comments_only(
                buf,
                self.after_implements_kw.iter(),
                NewlineAt::Bottom,
                indent,
            );
        }
        buf.ensure_ends_with_whitespace();
        fmt_collection(buf, indent, Braces::Square, self.item.value, Newlines::No);
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
        let lifted = type_var_lift_spaces_after(arena, last.value);
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

fn type_var_lift_spaces_after<'a, 'b: 'a>(
    arena: &'a Bump,
    var: TypeVar<'b>,
) -> SpacesAfter<'a, TypeVar<'a>> {
    match var {
        item @ TypeVar::Identifier(_) => SpacesAfter { item, after: &[] },
        TypeVar::Malformed(expr) => {
            let lifted = expr_lift_spaces_after(Parens::NotNeeded, arena, expr);
            SpacesAfter {
                item: TypeVar::Malformed(arena.alloc(lifted.item)),
                after: lifted.after,
            }
        }
        TypeVar::SpaceBefore(inner, spaces) => {
            let lifted = type_var_lift_spaces_after(arena, *inner);
            SpacesAfter {
                item: TypeVar::SpaceBefore(arena.alloc(lifted.item), spaces),
                after: lifted.after,
            }
        }
        TypeVar::SpaceAfter(inner, spaces) => {
            let mut lifted = type_var_lift_spaces_after(arena, *inner);
            lifted.after = merge_spaces_conservative(arena, lifted.after, spaces);
            lifted
        }
    }
}

impl<'a> Formattable for TypeHeader<'a> {
    fn is_multiline(&self) -> bool {
        self.vars.iter().any(|v| v.is_multiline())
    }

    fn format_with_options(
        &self,
        buf: &mut Buf,
        _parens: Parens,
        _newlines: Newlines,
        indent: u16,
    ) {
        let node = self.to_node(buf.text.bump(), buf.flags());
        node.format(buf, indent);
    }
}

impl<'a> Formattable for TypeVar<'a> {
    fn is_multiline(&self) -> bool {
        match self {
            TypeVar::Identifier(_) => false,
            TypeVar::Malformed(expr) => expr.is_multiline(),
            TypeVar::SpaceBefore(inner, spaces) | TypeVar::SpaceAfter(inner, spaces) => {
                inner.is_multiline() || !spaces.is_empty()
            }
        }
    }

    fn format_with_options(
        &self,
        buf: &mut Buf,
        _parens: Parens,
        _newlines: Newlines,
        indent: u16,
    ) {
        let node = self.to_node(buf.text.bump(), buf.flags());
        node.format(buf, indent);
    }
}

impl<'a> Nodify<'a> for TypeHeader<'a> {
    fn to_node<'b>(&'a self, arena: &'b Bump, flags: MigrationFlags) -> NodeInfo<'b>
    where
        'a: 'b,
    {
        NodeInfo::apply(
            arena,
            NodeInfo::item(Node::Literal(self.name.value)),
            self.vars.iter().map(|v| v.value.to_node(arena, flags)),
        )
    }
}

impl<'a> Nodify<'a> for TypeVar<'a> {
    fn to_node<'b>(&'a self, arena: &'b Bump, flags: MigrationFlags) -> NodeInfo<'b>
    where
        'a: 'b,
    {
        match self {
            TypeVar::SpaceBefore(inner, spaces) => {
                let mut inner = inner.to_node(arena, flags);
                inner.before = merge_spaces_conservative(arena, spaces, inner.before);
                inner
            }
            TypeVar::SpaceAfter(inner, spaces) => {
                let mut inner = inner.to_node(arena, flags);
                inner.after = merge_spaces_conservative(arena, inner.after, spaces);
                inner
            }
            TypeVar::Identifier(text) => {
                let var_name = if flags.snakify {
                    let mut buf = Buf::new_in(arena, flags);
                    buf.indent(0); // Take out of beginning of line
                    snakify_camel_ident(&mut buf, text);
                    let s: &str = arena.alloc_str(buf.as_str());
                    s
                } else {
                    text
                };
                let item = NodeInfo::item(Node::Literal(var_name));

                if *text == "implements" {
                    parens_around_node(arena, item, false)
                } else {
                    item
                }
            }
            TypeVar::Malformed(expr) => {
                let lifted = expr_lift_spaces(Parens::InApply, arena, expr);
                NodeInfo {
                    before: lifted.before,
                    node: Node::Expr(lifted.item),
                    after: lifted.after,
                    needs_indent: true,
                    prec: expr_prec(**expr),
                }
            }
        }
    }
}

impl<'a> Nodify<'a> for TypeAnnotation<'a> {
    fn to_node<'b>(&'a self, arena: &'b Bump, flags: MigrationFlags) -> NodeInfo<'b>
    where
        'a: 'b,
    {
        match self {
            TypeAnnotation::Apply(module, func, args) => {
                let first = if module.is_empty() {
                    Node::Literal(func)
                } else {
                    Node::Literal(arena.alloc_str(&format!("{}.{}", module, func)))
                };

                NodeInfo::apply(
                    arena,
                    NodeInfo::item(first),
                    args.iter().map(|arg| arg.value.to_node(arena, flags)),
                )
            }
            TypeAnnotation::SpaceBefore(expr, spaces) => {
                let mut inner = expr.to_node(arena, flags);
                inner.before = merge_spaces_conservative(arena, spaces, inner.before);
                inner
            }
            TypeAnnotation::SpaceAfter(expr, spaces) => {
                let mut inner = expr.to_node(arena, flags);
                inner.after = merge_spaces_conservative(arena, inner.after, spaces);
                inner
            }
            TypeAnnotation::Function(args, purity, res) => {
                let (first, rest) = args.split_first().expect("args must not be empty");
                let first_node = first
                    .value
                    .to_node(arena, flags)
                    .add_parens(arena, Parens::InFunctionType);
                let mut last_after: &'_ [CommentOrNewline<'_>] = &[];
                let mut rest_nodes = Vec::with_capacity_in(rest.len() + 2, arena);
                let mut multiline = first_node.node.is_multiline() || !first_node.after.is_empty();

                for item in rest {
                    let node = item
                        .value
                        .to_node(arena, flags)
                        .add_parens(arena, Parens::InFunctionType);
                    let before = merge_spaces_conservative(arena, last_after, node.before);
                    multiline |= node.node.is_multiline() || !before.is_empty();
                    last_after = node.after;
                    rest_nodes.push(Item {
                        before,
                        comma_before: true,
                        newline: false,
                        space: true,
                        node: node.node,
                    });
                }

                let res_node = res
                    .value
                    .to_node(arena, flags)
                    .add_parens(arena, Parens::InFunctionType);
                multiline |= res_node.node.is_multiline()
                    || !last_after.is_empty()
                    || !res_node.before.is_empty();

                if multiline {
                    for item in rest_nodes.iter_mut() {
                        item.newline = true;
                    }
                }

                rest_nodes.push(Item {
                    before: last_after,
                    comma_before: false,
                    newline: multiline,
                    space: true,
                    node: Node::Literal(match purity {
                        FunctionArrow::Pure => "->",
                        FunctionArrow::Effectful => "=>",
                    }),
                });

                rest_nodes.push(Item {
                    before: res_node.before,
                    comma_before: false,
                    newline: false,
                    space: true,
                    node: res_node.node,
                });

                NodeInfo {
                    before: first_node.before,
                    node: Node::CommaSequence {
                        allow_blank_lines: false,
                        indent_rest: false,
                        first: arena.alloc(first_node.node),
                        rest: rest_nodes.into_bump_slice(),
                    },
                    after: res_node.after,
                    needs_indent: true,
                    prec: Prec::FunctionType,
                }
            }
            TypeAnnotation::As(left, sp, right) => {
                let left = left
                    .value
                    .to_node(arena, flags)
                    .add_parens(arena, Parens::InAsPattern);
                let right = right
                    .to_node(arena, flags)
                    .add_parens(arena, Parens::InAsPattern);
                let before_as = merge_spaces(arena, left.after, sp);
                let mut b = NodeSequenceBuilder::new(arena, left.node, 2, true);
                b.push(Sp::with_space(before_as), Node::Literal("as"));
                b.push(Sp::with_space(right.before), right.node);

                NodeInfo {
                    before: left.before,
                    node: b.build(),
                    after: right.after,
                    needs_indent: true,
                    prec: Prec::AsType,
                }
            }
            TypeAnnotation::BoundVariable(text) => {
                let var_name = if flags.snakify {
                    let mut buf = Buf::new_in(arena, flags);
                    buf.indent(0); // Take out of beginning of line
                    snakify_camel_ident(&mut buf, text);
                    let s: &str = arena.alloc_str(buf.as_str());
                    s
                } else {
                    text
                };
                let item = NodeInfo::item(Node::Literal(var_name));

                if *text == "implements" {
                    parens_around_node(arena, item, false)
                } else {
                    item
                }
            }
            TypeAnnotation::Inferred => NodeInfo::item(Node::Literal("_")),
            TypeAnnotation::Wildcard => NodeInfo::item(Node::Literal("*")),
            TypeAnnotation::Malformed(text) => NodeInfo::item(Node::Literal(text)),
            TypeAnnotation::Record { fields, ext } => {
                let coll =
                    collection_to_node(arena, Braces::Curly, true, fields, |_is_first, f| {
                        f.value
                            .to_node(arena, flags)
                            .add_parens(arena, Parens::InCollection)
                    });
                maybe_add_ext(
                    arena,
                    coll,
                    ext,
                    fields.is_empty() && fields.final_comments().is_empty(),
                    flags,
                )
            }
            TypeAnnotation::TagUnion { ext, tags } => {
                let coll =
                    collection_to_node(arena, Braces::Square, false, tags, |_is_first, t| {
                        t.value
                            .to_node(arena, flags)
                            .add_parens(arena, Parens::InCollection)
                    });
                maybe_add_ext(
                    arena,
                    coll,
                    ext,
                    tags.is_empty() && tags.final_comments().is_empty(),
                    flags,
                )
            }
            TypeAnnotation::Tuple { elems, ext } => {
                let coll = collection_to_node(arena, Braces::Round, false, elems, |is_first, e| {
                    let v = e.value.to_node(arena, flags);
                    if is_first {
                        v
                    } else {
                        v.add_parens(arena, Parens::InCollection)
                    }
                });
                maybe_add_ext(
                    arena,
                    coll,
                    ext,
                    elems.is_empty() && elems.final_comments().is_empty(),
                    flags,
                )
            }
            TypeAnnotation::Where(annot, implements_clauses) => {
                let mut items = Vec::with_capacity_in(implements_clauses.len() + 2, arena);

                let annot = annot
                    .value
                    .to_node(arena, flags)
                    .add_parens(arena, Parens::NotNeeded);

                let before = filter_newlines(arena, annot.after);
                let mut needs_indent = annot.needs_indent || !before.is_empty();

                items.push(Item {
                    comma_before: false,
                    before,
                    newline: false,
                    space: true,
                    node: Node::Literal(roc_parse::keyword::WHERE),
                });

                let mut last_after: &[CommentOrNewline<'_>] = &[];

                for (i, clause) in implements_clauses.iter().enumerate() {
                    let node = clause.value.to_node(arena, flags);
                    let before =
                        filter_newlines(arena, merge_spaces(arena, last_after, node.before));
                    last_after = node.after;
                    items.push(Item {
                        before,
                        comma_before: i > 0,
                        newline: false,
                        space: true,
                        node: node.node,
                    });

                    needs_indent |= node.node.is_multiline() || !before.is_empty();
                }

                NodeInfo {
                    before: annot.before,
                    node: Node::CommaSequence {
                        first: arena.alloc(annot.node),
                        rest: arena.alloc_slice_copy(&items),
                        allow_blank_lines: false,
                        indent_rest: true,
                    },
                    after: last_after,
                    needs_indent,
                    prec: Prec::FunctionType,
                }
            }
        }
    }
}

fn filter_newlines<'a, 'b: 'a>(
    arena: &'a Bump,
    items: &'b [CommentOrNewline<'b>],
) -> &'a [CommentOrNewline<'a>] {
    let count = items.iter().filter(|i| i.is_newline()).count();
    if count > 0 {
        let mut new_items = Vec::with_capacity_in(items.len() - count, arena);
        for item in items {
            if !item.is_newline() {
                new_items.push(*item);
            }
        }
        arena.alloc_slice_copy(&new_items)
    } else {
        items
    }
}

impl<'a> Nodify<'a> for &'a str {
    fn to_node<'b>(&'a self, arena: &'b Bump, flags: MigrationFlags) -> NodeInfo<'b>
    where
        'a: 'b,
    {
        if flags.snakify {
            let mut buf = Buf::new_in(arena, flags);
            buf.indent(0); // Take out of beginning of line
            snakify_camel_ident(&mut buf, self);
            let s: &str = arena.alloc_str(buf.as_str());
            NodeInfo::item(Node::Literal(s))
        } else {
            NodeInfo::item(Node::Literal(self))
        }
    }
}

impl<'a, T: Nodify<'a>> Nodify<'a> for Spaced<'a, T> {
    fn to_node<'b>(&'a self, arena: &'b Bump, flags: MigrationFlags) -> NodeInfo<'b>
    where
        'a: 'b,
    {
        match self {
            Spaced::Item(item) => item.to_node(arena, flags),
            Spaced::SpaceBefore(inner, sp) => {
                let mut inner = inner.to_node(arena, flags);
                inner.before = merge_spaces_conservative(arena, sp, inner.before);
                inner
            }
            Spaced::SpaceAfter(inner, sp) => {
                let mut inner = inner.to_node(arena, flags);
                inner.after = merge_spaces_conservative(arena, inner.after, sp);
                inner
            }
        }
    }
}

impl<'a> Nodify<'a> for ImplementsClause<'a> {
    fn to_node<'b>(&'a self, arena: &'b Bump, flags: MigrationFlags) -> NodeInfo<'b>
    where
        'a: 'b,
    {
        let mut items = Vec::with_capacity_in(self.abilities.len() + 2, arena);

        let var = self
            .var
            .value
            .to_node(arena, flags)
            .add_parens(arena, Parens::InAsPattern);

        items.push(Item {
            comma_before: false,
            before: var.after,
            newline: false,
            space: true,
            node: Node::Literal(roc_parse::keyword::IMPLEMENTS),
        });

        let mut last_after: &[CommentOrNewline<'_>] = &[];

        for (i, clause) in self.abilities.iter().enumerate() {
            let node = clause.value.to_node(arena, flags);
            let before = merge_spaces_conservative(arena, last_after, node.before);
            last_after = node.after;

            if i > 0 {
                // push the '&' separator
                items.push(Item {
                    before: &[],
                    comma_before: false,
                    newline: false,
                    space: true,
                    node: Node::Literal("&"),
                });
            }

            // push the item
            items.push(Item {
                before,
                comma_before: false,
                newline: false,
                space: true,
                node: node.node,
            });
        }

        NodeInfo {
            before: var.before,
            node: Node::CommaSequence {
                first: arena.alloc(var.node),
                rest: arena.alloc_slice_copy(&items),
                allow_blank_lines: false,
                indent_rest: false,
            },
            after: last_after,
            needs_indent: true,
            prec: Prec::Term,
        }
    }
}

fn collection_to_node<'b, 'a: 'b, T>(
    arena: &'b Bump,
    braces: Braces,
    spaces_before_and_after: bool,
    fields: &Collection<'a, T>,
    field_to_node: impl Fn(bool, &'a T) -> NodeInfo<'b>,
) -> Node<'b> {
    let mut items = Vec::with_capacity_in(fields.len(), arena);
    let mut last_after: &[CommentOrNewline<'_>] = &[];
    let mut multiline = false;
    for (i, field) in fields.iter().enumerate() {
        let is_first = i == 0;
        let node = field_to_node(is_first, field);
        let before = if is_first {
            remove_leading_blank_lines(node.before)
        } else {
            merge_spaces_conservative(arena, last_after, node.before)
        };
        multiline |= node.node.is_multiline() || !before.is_empty();
        last_after = node.after;
        items.push(DelimitedItem {
            before,
            newline: false,
            space: !is_first || spaces_before_and_after,
            node: node.node,
            comma_after: true,
        });
    }
    let final_comments = remove_trailing_blank_lines(merge_spaces_conservative(
        arena,
        last_after,
        fields.final_comments(),
    ));

    multiline |= !final_comments.is_empty();

    if multiline {
        for item in items.iter_mut() {
            item.newline = true;
        }
    } else if let Some(last) = items.last_mut() {
        last.comma_after = false;
    }
    Node::DelimitedSequence {
        braces,
        after: Sp {
            default_space: !items.is_empty() && spaces_before_and_after,
            force_newline: multiline,
            comments: final_comments,
        },
        items: items.into_bump_slice(),
        indent_items: multiline,
    }
}

fn remove_leading_blank_lines<'a>(sp: &'a [CommentOrNewline<'a>]) -> &'a [CommentOrNewline<'a>] {
    let chomp = sp.iter().take_while(|c| c.is_newline()).count();
    if chomp > 1 {
        &sp[chomp - 1..]
    } else {
        sp
    }
}

fn remove_trailing_blank_lines<'a>(sp: &'a [CommentOrNewline<'a>]) -> &'a [CommentOrNewline<'a>] {
    // Yes! This is not symmetric!
    // Neither are CommentOrNewline's, since they all end in newlines, but they don't all start with newlines.
    let chomp = sp.iter().rev().take_while(|c| c.is_newline()).count();
    if chomp == sp.len() && !sp.is_empty() {
        &sp[..1]
    } else {
        &sp[..sp.len() - chomp]
    }
}

fn maybe_add_ext<'a>(
    arena: &'a Bump,
    delim: Node<'a>,
    ext: &Option<&'a Loc<TypeAnnotation<'a>>>,
    needs_indent: bool,
    flags: MigrationFlags,
) -> NodeInfo<'a> {
    if let Some(ext) = ext {
        let ext = ext.value.to_node(arena, flags).add_ty_ext_parens(arena);
        debug_assert_eq!(ext.before, &[]);
        let item = Node::Sequence {
            first: arena.alloc(delim),
            extra_indent_for_rest: false,
            rest: arena.alloc_slice_copy(&[(Sp::empty(), ext.node)]),
        };
        NodeInfo {
            before: &[],
            node: item,
            after: ext.after,
            needs_indent,
            prec: Prec::Term,
        }
    } else {
        NodeInfo {
            before: &[],
            node: delim,
            after: &[],
            needs_indent,
            prec: Prec::Term,
        }
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

    fn without_spaces(&self) -> T {
        self.item
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
