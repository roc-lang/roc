use bumpalo::collections::vec::Vec;
use bumpalo::Bump;
use roc_module::called_via::{BinOp, UnaryOp};
use roc_parse::{
    ast::{
        AbilityMember, AssignedField, Collection, CommentOrNewline, Defs, Expr, Has, HasAbilities,
        HasAbility, HasClause, HasImpls, Header, Module, Pattern, Spaced, Spaces, StrLiteral,
        StrSegment, Tag, TypeAnnotation, TypeDef, TypeHeader, ValueDef, WhenBranch,
    },
    header::{
        AppHeader, ExposedName, HostedHeader, ImportsEntry, InterfaceHeader, KeywordItem,
        ModuleName, PackageEntry, PackageHeader, PackageName, PlatformHeader, PlatformRequires,
        ProvidesTo, To, TypedIdent,
    },
    ident::UppercaseIdent,
};
use roc_region::all::{Loc, Region};

use crate::{Ast, Buf};

/// The number of spaces to indent.
pub const INDENT: u16 = 4;

pub fn fmt_default_spaces<'a, 'buf>(
    buf: &mut Buf<'buf>,
    spaces: &[CommentOrNewline<'a>],
    indent: u16,
) {
    if spaces.is_empty() {
        buf.spaces(1);
    } else {
        fmt_spaces(buf, spaces.iter(), indent);
    }
}

/// Like fmt_spaces, but disallows two consecutive newlines.
pub fn fmt_spaces_no_blank_lines<'a, 'buf, I>(buf: &mut Buf<'buf>, spaces: I, indent: u16)
where
    I: Iterator<Item = &'a CommentOrNewline<'a>>,
{
    fmt_spaces_max_consecutive_newlines(buf, spaces, 1, indent)
}

pub fn fmt_spaces<'a, 'buf, I>(buf: &mut Buf<'buf>, spaces: I, indent: u16)
where
    I: Iterator<Item = &'a CommentOrNewline<'a>>,
{
    fmt_spaces_max_consecutive_newlines(buf, spaces, 2, indent)
}

fn fmt_spaces_max_consecutive_newlines<'a, 'buf, I>(
    buf: &mut Buf<'buf>,
    spaces: I,
    max_consecutive_newlines: usize,
    indent: u16,
) where
    I: Iterator<Item = &'a CommentOrNewline<'a>>,
{
    use self::CommentOrNewline::*;

    // Only ever print two newlines back to back.
    // (Two newlines renders as one blank line.)
    let mut consecutive_newlines = 0;
    let mut encountered_comment = false;

    for space in spaces {
        match space {
            Newline => {
                if !encountered_comment && (consecutive_newlines < max_consecutive_newlines) {
                    buf.newline();

                    // Don't bother incrementing it if we're already over the limit.
                    // There's no upside, and it might eventually overflow.
                    consecutive_newlines += 1;
                }
            }
            LineComment(comment) => {
                buf.indent(indent);
                fmt_comment(buf, comment);
                buf.newline();

                encountered_comment = true;
            }
            DocComment(docs) => {
                buf.indent(indent);
                fmt_docs(buf, docs);
                buf.newline();

                encountered_comment = true;
            }
        }
    }
}

#[derive(Eq, PartialEq, Debug)]
pub enum NewlineAt {
    Top,
    Bottom,
    Both,
    None,
}

/// Like format_spaces, but remove newlines and keep only comments.
/// The `new_line_at` argument describes how new lines should be inserted
/// at the beginning or at the end of the block
/// in the case of there is some comment in the `spaces` argument.
pub fn fmt_comments_only<'a, 'buf, I>(
    buf: &mut Buf<'buf>,
    spaces: I,
    new_line_at: NewlineAt,
    indent: u16,
) where
    I: Iterator<Item = &'a CommentOrNewline<'a>>,
{
    use self::CommentOrNewline::*;
    use NewlineAt::*;

    let mut comment_seen = false;

    for space in spaces {
        match space {
            Newline => {}
            LineComment(comment) => {
                if comment_seen || new_line_at == Top || new_line_at == Both {
                    buf.newline();
                }
                buf.indent(indent);
                fmt_comment(buf, comment);
                comment_seen = true;
            }
            DocComment(docs) => {
                if comment_seen || new_line_at == Top || new_line_at == Both {
                    buf.newline();
                }
                buf.indent(indent);
                fmt_docs(buf, docs);
                comment_seen = true;
            }
        }
    }
    if comment_seen && (new_line_at == Bottom || new_line_at == Both) {
        buf.newline();
    }
}

fn fmt_comment<'buf>(buf: &mut Buf<'buf>, comment: &str) {
    // The '#' in a comment should always be preceded by a newline or a space,
    // unless it's the very beginning of the buffer.
    if !buf.is_empty() && !buf.ends_with_space() && !buf.ends_with_newline() {
        buf.spaces(1);
    }

    buf.push('#');
    if !comment.starts_with(' ') {
        buf.spaces(1);
    }
    buf.push_str(comment.trim_end());
}

pub fn count_leading_newlines<'a, I>(data: I) -> u16
where
    I: Iterator<Item = &'a CommentOrNewline<'a>>,
{
    let mut count = 0;
    let mut allow_counting = false;

    for (index, val) in data.enumerate() {
        let is_first = index == 0;
        let is_newline = matches!(val, CommentOrNewline::Newline);

        if is_first && is_newline {
            allow_counting = true
        }

        if is_newline && allow_counting {
            count += 1;
        } else {
            break;
        }
    }

    count
}

fn fmt_docs<'buf>(buf: &mut Buf<'buf>, docs: &str) {
    // The "##" in a doc comment should always be preceded by a newline or a space,
    // unless it's the very beginning of the buffer.
    if !buf.is_empty() && !buf.ends_with_space() && !buf.ends_with_newline() {
        buf.spaces(1);
    }

    buf.push_str("##");
    if !docs.is_empty() {
        buf.spaces(1);
    }
    buf.push_str(docs.trim_end());
}

/// RemoveSpaces normalizes the ast to something that we _expect_ to be invariant under formatting.
///
/// Currently this consists of:
/// * Removing newlines
/// * Removing comments
/// * Removing parens in Exprs
///
/// Long term, we actuall want this transform to preserve comments (so we can assert they're maintained by formatting)
/// - but there are currently several bugs where they're _not_ preserved.
/// TODO: ensure formatting retains comments
pub trait RemoveSpaces<'a> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self;
}

impl<'a> RemoveSpaces<'a> for Ast<'a> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        Ast {
            module: self.module.remove_spaces(arena),
            defs: self.defs.remove_spaces(arena),
        }
    }
}

impl<'a> RemoveSpaces<'a> for Defs<'a> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        let mut defs = self.clone();

        defs.spaces.clear();
        defs.space_before.clear();
        defs.space_after.clear();

        for type_def in defs.type_defs.iter_mut() {
            *type_def = type_def.remove_spaces(arena);
        }

        for value_def in defs.value_defs.iter_mut() {
            *value_def = value_def.remove_spaces(arena);
        }

        for region_def in defs.regions.iter_mut() {
            *region_def = region_def.remove_spaces(arena);
        }

        defs
    }
}

impl<'a, V: RemoveSpaces<'a>> RemoveSpaces<'a> for Spaces<'a, V> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        Spaces {
            before: &[],
            item: self.item.remove_spaces(arena),
            after: &[],
        }
    }
}

impl<'a, K: RemoveSpaces<'a>, V: RemoveSpaces<'a>> RemoveSpaces<'a> for KeywordItem<'a, K, V> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        KeywordItem {
            keyword: self.keyword.remove_spaces(arena),
            item: self.item.remove_spaces(arena),
        }
    }
}

impl<'a> RemoveSpaces<'a> for ProvidesTo<'a> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        ProvidesTo {
            provides_keyword: self.provides_keyword.remove_spaces(arena),
            entries: self.entries.remove_spaces(arena),
            types: self.types.remove_spaces(arena),
            to_keyword: self.to_keyword.remove_spaces(arena),
            to: self.to.remove_spaces(arena),
        }
    }
}

impl<'a> RemoveSpaces<'a> for Module<'a> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        let header = match &self.header {
            Header::Interface(header) => Header::Interface(InterfaceHeader {
                before_name: &[],
                name: header.name.remove_spaces(arena),
                exposes: header.exposes.remove_spaces(arena),
                imports: header.imports.remove_spaces(arena),
            }),
            Header::App(header) => Header::App(AppHeader {
                before_name: &[],
                name: header.name.remove_spaces(arena),
                packages: header.packages.remove_spaces(arena),
                imports: header.imports.remove_spaces(arena),
                provides: header.provides.remove_spaces(arena),
            }),
            Header::Package(header) => Header::Package(PackageHeader {
                before_name: &[],
                name: header.name.remove_spaces(arena),
                exposes: header.exposes.remove_spaces(arena),
                packages: header.packages.remove_spaces(arena),
                imports: header.imports.remove_spaces(arena),
            }),
            Header::Platform(header) => Header::Platform(PlatformHeader {
                before_name: &[],
                name: header.name.remove_spaces(arena),
                requires: header.requires.remove_spaces(arena),
                exposes: header.exposes.remove_spaces(arena),
                packages: header.packages.remove_spaces(arena),
                imports: header.imports.remove_spaces(arena),
                provides: header.provides.remove_spaces(arena),
            }),
            Header::Hosted(header) => Header::Hosted(HostedHeader {
                before_name: &[],
                name: header.name.remove_spaces(arena),
                exposes: header.exposes.remove_spaces(arena),
                imports: header.imports.remove_spaces(arena),
                generates: header.generates.remove_spaces(arena),
                generates_with: header.generates_with.remove_spaces(arena),
            }),
        };
        Module {
            comments: &[],
            header,
        }
    }
}

impl<'a> RemoveSpaces<'a> for Region {
    fn remove_spaces(&self, _arena: &'a Bump) -> Self {
        Region::zero()
    }
}

impl<'a> RemoveSpaces<'a> for &'a str {
    fn remove_spaces(&self, _arena: &'a Bump) -> Self {
        self
    }
}

impl<'a, T: RemoveSpaces<'a> + Copy> RemoveSpaces<'a> for Spaced<'a, T> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        match *self {
            Spaced::Item(a) => Spaced::Item(a.remove_spaces(arena)),
            Spaced::SpaceBefore(a, _) => a.remove_spaces(arena),
            Spaced::SpaceAfter(a, _) => a.remove_spaces(arena),
        }
    }
}

impl<'a> RemoveSpaces<'a> for ExposedName<'a> {
    fn remove_spaces(&self, _arena: &'a Bump) -> Self {
        *self
    }
}

impl<'a> RemoveSpaces<'a> for ModuleName<'a> {
    fn remove_spaces(&self, _arena: &'a Bump) -> Self {
        *self
    }
}

impl<'a> RemoveSpaces<'a> for PackageName<'a> {
    fn remove_spaces(&self, _arena: &'a Bump) -> Self {
        *self
    }
}

impl<'a> RemoveSpaces<'a> for To<'a> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        match *self {
            To::ExistingPackage(a) => To::ExistingPackage(a),
            To::NewPackage(a) => To::NewPackage(a.remove_spaces(arena)),
        }
    }
}

impl<'a> RemoveSpaces<'a> for TypedIdent<'a> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        TypedIdent {
            ident: self.ident.remove_spaces(arena),
            spaces_before_colon: &[],
            ann: self.ann.remove_spaces(arena),
        }
    }
}

impl<'a> RemoveSpaces<'a> for PlatformRequires<'a> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        PlatformRequires {
            rigids: self.rigids.remove_spaces(arena),
            signature: self.signature.remove_spaces(arena),
        }
    }
}

impl<'a> RemoveSpaces<'a> for UppercaseIdent<'a> {
    fn remove_spaces(&self, _arena: &'a Bump) -> Self {
        *self
    }
}

impl<'a> RemoveSpaces<'a> for PackageEntry<'a> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        PackageEntry {
            shorthand: self.shorthand,
            spaces_after_shorthand: &[],
            package_name: self.package_name.remove_spaces(arena),
        }
    }
}

impl<'a> RemoveSpaces<'a> for ImportsEntry<'a> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        match *self {
            ImportsEntry::Module(a, b) => ImportsEntry::Module(a, b.remove_spaces(arena)),
            ImportsEntry::Package(a, b, c) => ImportsEntry::Package(a, b, c.remove_spaces(arena)),
        }
    }
}

impl<'a, T: RemoveSpaces<'a>> RemoveSpaces<'a> for Option<T> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        self.as_ref().map(|a| a.remove_spaces(arena))
    }
}

impl<'a, T: RemoveSpaces<'a> + std::fmt::Debug> RemoveSpaces<'a> for Loc<T> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        let res = self.value.remove_spaces(arena);
        Loc::at(Region::zero(), res)
    }
}

impl<'a, A: RemoveSpaces<'a>, B: RemoveSpaces<'a>> RemoveSpaces<'a> for (A, B) {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        (self.0.remove_spaces(arena), self.1.remove_spaces(arena))
    }
}

impl<'a, T: RemoveSpaces<'a>> RemoveSpaces<'a> for Collection<'a, T> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        let mut items = Vec::with_capacity_in(self.items.len(), arena);
        for item in self.items {
            items.push(item.remove_spaces(arena));
        }
        Collection::with_items(items.into_bump_slice())
    }
}

impl<'a, T: RemoveSpaces<'a> + std::fmt::Debug> RemoveSpaces<'a> for &'a [T] {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        let mut items = Vec::with_capacity_in(self.len(), arena);
        for item in *self {
            let res = item.remove_spaces(arena);
            items.push(res);
        }
        items.into_bump_slice()
    }
}

impl<'a> RemoveSpaces<'a> for UnaryOp {
    fn remove_spaces(&self, _arena: &'a Bump) -> Self {
        *self
    }
}

impl<'a> RemoveSpaces<'a> for BinOp {
    fn remove_spaces(&self, _arena: &'a Bump) -> Self {
        *self
    }
}

impl<'a, T: RemoveSpaces<'a>> RemoveSpaces<'a> for &'a T {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        arena.alloc((*self).remove_spaces(arena))
    }
}

impl<'a> RemoveSpaces<'a> for TypeDef<'a> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        use TypeDef::*;

        match *self {
            Alias {
                header: TypeHeader { name, vars },
                ann,
            } => Alias {
                header: TypeHeader {
                    name: name.remove_spaces(arena),
                    vars: vars.remove_spaces(arena),
                },
                ann: ann.remove_spaces(arena),
            },
            Opaque {
                header: TypeHeader { name, vars },
                typ,
                derived,
            } => Opaque {
                header: TypeHeader {
                    name: name.remove_spaces(arena),
                    vars: vars.remove_spaces(arena),
                },
                typ: typ.remove_spaces(arena),
                derived: derived.remove_spaces(arena),
            },
            Ability {
                header: TypeHeader { name, vars },
                loc_has,
                members,
            } => Ability {
                header: TypeHeader {
                    name: name.remove_spaces(arena),
                    vars: vars.remove_spaces(arena),
                },
                loc_has: loc_has.remove_spaces(arena),
                members: members.remove_spaces(arena),
            },
        }
    }
}

impl<'a> RemoveSpaces<'a> for ValueDef<'a> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        use ValueDef::*;

        match *self {
            Annotation(a, b) => Annotation(a.remove_spaces(arena), b.remove_spaces(arena)),
            Body(a, b) => Body(
                arena.alloc(a.remove_spaces(arena)),
                arena.alloc(b.remove_spaces(arena)),
            ),
            AnnotatedBody {
                ann_pattern,
                ann_type,
                comment: _,
                body_pattern,
                body_expr,
            } => AnnotatedBody {
                ann_pattern: arena.alloc(ann_pattern.remove_spaces(arena)),
                ann_type: arena.alloc(ann_type.remove_spaces(arena)),
                comment: None,
                body_pattern: arena.alloc(body_pattern.remove_spaces(arena)),
                body_expr: arena.alloc(body_expr.remove_spaces(arena)),
            },
            Dbg {
                condition,
                preceding_comment: _,
            } => Dbg {
                condition: arena.alloc(condition.remove_spaces(arena)),
                preceding_comment: Region::zero(),
            },
            Expect {
                condition,
                preceding_comment: _,
            } => Expect {
                condition: arena.alloc(condition.remove_spaces(arena)),
                preceding_comment: Region::zero(),
            },
            ExpectFx {
                condition,
                preceding_comment: _,
            } => ExpectFx {
                condition: arena.alloc(condition.remove_spaces(arena)),
                preceding_comment: Region::zero(),
            },
        }
    }
}

impl<'a> RemoveSpaces<'a> for Has<'a> {
    fn remove_spaces(&self, _arena: &'a Bump) -> Self {
        Has::Has
    }
}

impl<'a> RemoveSpaces<'a> for AbilityMember<'a> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        AbilityMember {
            name: self.name.remove_spaces(arena),
            typ: self.typ.remove_spaces(arena),
        }
    }
}

impl<'a> RemoveSpaces<'a> for WhenBranch<'a> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        WhenBranch {
            patterns: self.patterns.remove_spaces(arena),
            value: self.value.remove_spaces(arena),
            guard: self.guard.remove_spaces(arena),
        }
    }
}

impl<'a, T: RemoveSpaces<'a> + Copy + std::fmt::Debug> RemoveSpaces<'a> for AssignedField<'a, T> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        match *self {
            AssignedField::RequiredValue(a, _, c) => AssignedField::RequiredValue(
                a.remove_spaces(arena),
                arena.alloc([]),
                arena.alloc(c.remove_spaces(arena)),
            ),
            AssignedField::OptionalValue(a, _, c) => AssignedField::OptionalValue(
                a.remove_spaces(arena),
                arena.alloc([]),
                arena.alloc(c.remove_spaces(arena)),
            ),
            AssignedField::LabelOnly(a) => AssignedField::LabelOnly(a.remove_spaces(arena)),
            AssignedField::Malformed(a) => AssignedField::Malformed(a),
            AssignedField::SpaceBefore(a, _) => a.remove_spaces(arena),
            AssignedField::SpaceAfter(a, _) => a.remove_spaces(arena),
        }
    }
}

impl<'a> RemoveSpaces<'a> for StrLiteral<'a> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        match *self {
            StrLiteral::PlainLine(t) => StrLiteral::PlainLine(t),
            StrLiteral::Line(t) => StrLiteral::Line(t.remove_spaces(arena)),
            StrLiteral::Block(t) => StrLiteral::Block(t.remove_spaces(arena)),
        }
    }
}

impl<'a> RemoveSpaces<'a> for StrSegment<'a> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        match *self {
            StrSegment::Plaintext(t) => StrSegment::Plaintext(t),
            StrSegment::Unicode(t) => StrSegment::Unicode(t.remove_spaces(arena)),
            StrSegment::EscapedChar(c) => StrSegment::EscapedChar(c),
            StrSegment::Interpolated(t) => StrSegment::Interpolated(t.remove_spaces(arena)),
        }
    }
}

impl<'a> RemoveSpaces<'a> for Expr<'a> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        match *self {
            Expr::Float(a) => Expr::Float(a),
            Expr::Num(a) => Expr::Num(a),
            Expr::NonBase10Int {
                string,
                base,
                is_negative,
            } => Expr::NonBase10Int {
                string,
                base,
                is_negative,
            },
            Expr::Str(a) => Expr::Str(a.remove_spaces(arena)),
            Expr::RecordAccess(a, b) => Expr::RecordAccess(arena.alloc(a.remove_spaces(arena)), b),
            Expr::RecordAccessorFunction(a) => Expr::RecordAccessorFunction(a),
            Expr::TupleAccess(a, b) => Expr::TupleAccess(arena.alloc(a.remove_spaces(arena)), b),
            Expr::TupleAccessorFunction(a) => Expr::TupleAccessorFunction(a),
            Expr::List(a) => Expr::List(a.remove_spaces(arena)),
            Expr::RecordUpdate { update, fields } => Expr::RecordUpdate {
                update: arena.alloc(update.remove_spaces(arena)),
                fields: fields.remove_spaces(arena),
            },
            Expr::Record(a) => Expr::Record(a.remove_spaces(arena)),
            Expr::Tuple(a) => Expr::Tuple(a.remove_spaces(arena)),
            Expr::Var { module_name, ident } => Expr::Var { module_name, ident },
            Expr::Underscore(a) => Expr::Underscore(a),
            Expr::Tag(a) => Expr::Tag(a),
            Expr::OpaqueRef(a) => Expr::OpaqueRef(a),
            Expr::Closure(a, b) => Expr::Closure(
                arena.alloc(a.remove_spaces(arena)),
                arena.alloc(b.remove_spaces(arena)),
            ),
            Expr::Crash => Expr::Crash,
            Expr::Defs(a, b) => {
                let mut defs = a.clone();
                defs.space_before = vec![Default::default(); defs.len()];
                defs.space_after = vec![Default::default(); defs.len()];
                defs.regions = vec![Region::zero(); defs.len()];
                defs.spaces.clear();

                for type_def in defs.type_defs.iter_mut() {
                    *type_def = type_def.remove_spaces(arena);
                }

                for value_def in defs.value_defs.iter_mut() {
                    *value_def = value_def.remove_spaces(arena);
                }

                Expr::Defs(arena.alloc(defs), arena.alloc(b.remove_spaces(arena)))
            }
            Expr::Backpassing(a, b, c) => Expr::Backpassing(
                arena.alloc(a.remove_spaces(arena)),
                arena.alloc(b.remove_spaces(arena)),
                arena.alloc(c.remove_spaces(arena)),
            ),
            Expr::Expect(a, b) => Expr::Expect(
                arena.alloc(a.remove_spaces(arena)),
                arena.alloc(b.remove_spaces(arena)),
            ),
            Expr::Dbg(a, b) => Expr::Dbg(
                arena.alloc(a.remove_spaces(arena)),
                arena.alloc(b.remove_spaces(arena)),
            ),
            Expr::Apply(a, b, c) => Expr::Apply(
                arena.alloc(a.remove_spaces(arena)),
                b.remove_spaces(arena),
                c,
            ),
            Expr::BinOps(a, b) => {
                Expr::BinOps(a.remove_spaces(arena), arena.alloc(b.remove_spaces(arena)))
            }
            Expr::UnaryOp(a, b) => {
                Expr::UnaryOp(arena.alloc(a.remove_spaces(arena)), b.remove_spaces(arena))
            }
            Expr::If(a, b) => Expr::If(a.remove_spaces(arena), arena.alloc(b.remove_spaces(arena))),
            Expr::When(a, b) => {
                Expr::When(arena.alloc(a.remove_spaces(arena)), b.remove_spaces(arena))
            }
            Expr::ParensAround(a) => {
                // The formatter can remove redundant parentheses, so also remove these when normalizing for comparison.
                a.remove_spaces(arena)
            }
            Expr::MalformedIdent(a, b) => Expr::MalformedIdent(a, b),
            Expr::MalformedClosure => Expr::MalformedClosure,
            Expr::PrecedenceConflict(a) => Expr::PrecedenceConflict(a),
            Expr::SpaceBefore(a, _) => a.remove_spaces(arena),
            Expr::SpaceAfter(a, _) => a.remove_spaces(arena),
            Expr::SingleQuote(a) => Expr::Num(a),
        }
    }
}

impl<'a> RemoveSpaces<'a> for Pattern<'a> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        match *self {
            Pattern::Identifier(a) => Pattern::Identifier(a),
            Pattern::Tag(a) => Pattern::Tag(a),
            Pattern::OpaqueRef(a) => Pattern::OpaqueRef(a),
            Pattern::Apply(a, b) => Pattern::Apply(
                arena.alloc(a.remove_spaces(arena)),
                arena.alloc(b.remove_spaces(arena)),
            ),
            Pattern::RecordDestructure(a) => Pattern::RecordDestructure(a.remove_spaces(arena)),
            Pattern::RequiredField(a, b) => {
                Pattern::RequiredField(a, arena.alloc(b.remove_spaces(arena)))
            }
            Pattern::OptionalField(a, b) => {
                Pattern::OptionalField(a, arena.alloc(b.remove_spaces(arena)))
            }
            Pattern::NumLiteral(a) => Pattern::NumLiteral(a),
            Pattern::NonBase10Literal {
                string,
                base,
                is_negative,
            } => Pattern::NonBase10Literal {
                string,
                base,
                is_negative,
            },
            Pattern::FloatLiteral(a) => Pattern::FloatLiteral(a),
            Pattern::StrLiteral(a) => Pattern::StrLiteral(a),
            Pattern::Underscore(a) => Pattern::Underscore(a),
            Pattern::Malformed(a) => Pattern::Malformed(a),
            Pattern::MalformedIdent(a, b) => Pattern::MalformedIdent(a, b),
            Pattern::QualifiedIdentifier { module_name, ident } => {
                Pattern::QualifiedIdentifier { module_name, ident }
            }
            Pattern::SpaceBefore(a, _) => a.remove_spaces(arena),
            Pattern::SpaceAfter(a, _) => a.remove_spaces(arena),
            Pattern::SingleQuote(a) => Pattern::SingleQuote(a),
            Pattern::List(pats) => Pattern::List(pats.remove_spaces(arena)),
            Pattern::Tuple(pats) => Pattern::Tuple(pats.remove_spaces(arena)),
            Pattern::ListRest => Pattern::ListRest,
        }
    }
}

impl<'a> RemoveSpaces<'a> for TypeAnnotation<'a> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        match *self {
            TypeAnnotation::Function(a, b) => TypeAnnotation::Function(
                arena.alloc(a.remove_spaces(arena)),
                arena.alloc(b.remove_spaces(arena)),
            ),
            TypeAnnotation::Apply(a, b, c) => TypeAnnotation::Apply(a, b, c.remove_spaces(arena)),
            TypeAnnotation::BoundVariable(a) => TypeAnnotation::BoundVariable(a),
            TypeAnnotation::As(a, _, TypeHeader { name, vars }) => TypeAnnotation::As(
                arena.alloc(a.remove_spaces(arena)),
                &[],
                TypeHeader {
                    name: name.remove_spaces(arena),
                    vars: vars.remove_spaces(arena),
                },
            ),
            TypeAnnotation::Tuple { fields, ext } => TypeAnnotation::Tuple {
                fields: fields.remove_spaces(arena),
                ext: ext.remove_spaces(arena),
            },
            TypeAnnotation::Record { fields, ext } => TypeAnnotation::Record {
                fields: fields.remove_spaces(arena),
                ext: ext.remove_spaces(arena),
            },
            TypeAnnotation::TagUnion { ext, tags } => TypeAnnotation::TagUnion {
                ext: ext.remove_spaces(arena),
                tags: tags.remove_spaces(arena),
            },
            TypeAnnotation::Inferred => TypeAnnotation::Inferred,
            TypeAnnotation::Wildcard => TypeAnnotation::Wildcard,
            TypeAnnotation::Where(annot, has_clauses) => TypeAnnotation::Where(
                arena.alloc(annot.remove_spaces(arena)),
                arena.alloc(has_clauses.remove_spaces(arena)),
            ),
            TypeAnnotation::SpaceBefore(a, _) => a.remove_spaces(arena),
            TypeAnnotation::SpaceAfter(a, _) => a.remove_spaces(arena),
            TypeAnnotation::Malformed(a) => TypeAnnotation::Malformed(a),
        }
    }
}

impl<'a> RemoveSpaces<'a> for HasClause<'a> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        HasClause {
            var: self.var.remove_spaces(arena),
            abilities: self.abilities.remove_spaces(arena),
        }
    }
}

impl<'a> RemoveSpaces<'a> for Tag<'a> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        match *self {
            Tag::Apply { name, args } => Tag::Apply {
                name: name.remove_spaces(arena),
                args: args.remove_spaces(arena),
            },
            Tag::Malformed(a) => Tag::Malformed(a),
            Tag::SpaceBefore(a, _) => a.remove_spaces(arena),
            Tag::SpaceAfter(a, _) => a.remove_spaces(arena),
        }
    }
}

impl<'a> RemoveSpaces<'a> for HasImpls<'a> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        match *self {
            HasImpls::HasImpls(impls) => HasImpls::HasImpls(impls.remove_spaces(arena)),
            HasImpls::SpaceBefore(has, _) | HasImpls::SpaceAfter(has, _) => {
                has.remove_spaces(arena)
            }
        }
    }
}

impl<'a> RemoveSpaces<'a> for HasAbility<'a> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        match *self {
            HasAbility::HasAbility { ability, impls } => HasAbility::HasAbility {
                ability: ability.remove_spaces(arena),
                impls: impls.remove_spaces(arena),
            },
            HasAbility::SpaceBefore(has, _) | HasAbility::SpaceAfter(has, _) => {
                has.remove_spaces(arena)
            }
        }
    }
}

impl<'a> RemoveSpaces<'a> for HasAbilities<'a> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        match *self {
            HasAbilities::Has(derived) => HasAbilities::Has(derived.remove_spaces(arena)),
            HasAbilities::SpaceBefore(derived, _) | HasAbilities::SpaceAfter(derived, _) => {
                derived.remove_spaces(arena)
            }
        }
    }
}
