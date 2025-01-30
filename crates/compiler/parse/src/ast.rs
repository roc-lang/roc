use std::fmt::Debug;

use crate::expr::merge_spaces;
use crate::header::{
    self, AppHeader, HostedHeader, ModuleHeader, ModuleName, PackageHeader, PlatformHeader,
};
use crate::ident::Accessor;
use crate::parser::{ESingleQuote, EString};
use bumpalo::collections::{String, Vec};
use bumpalo::Bump;
use roc_collections::soa::{index_push_new, slice_extend_new};
use roc_error_macros::internal_error;
use roc_module::called_via::{BinOp, CalledVia, UnaryOp};
use roc_module::ident::QualifiedModuleName;
use roc_region::all::{Loc, Position, Region};
use soa::{EitherIndex, Slice};

#[derive(Debug, Clone)]
pub struct FullAst<'a> {
    pub header: SpacesBefore<'a, Header<'a>>,
    pub defs: Defs<'a>,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Spaces<'a, T> {
    pub before: &'a [CommentOrNewline<'a>],
    pub item: T,
    pub after: &'a [CommentOrNewline<'a>],
}

impl<'a, T: Copy> ExtractSpaces<'a> for Spaces<'a, T> {
    type Item = T;

    fn extract_spaces(&self) -> Spaces<'a, T> {
        *self
    }

    fn without_spaces(&self) -> T {
        self.item
    }
}

impl<'a, T> Spaces<'a, T> {
    pub fn item(item: T) -> Self {
        Self {
            before: &[],
            item,
            after: &[],
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct SpacesBefore<'a, T> {
    pub before: &'a [CommentOrNewline<'a>],
    pub item: T,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct SpacesAfter<'a, T> {
    pub after: &'a [CommentOrNewline<'a>],
    pub item: T,
}

#[derive(Copy, Clone, PartialEq)]
pub enum Spaced<'a, T> {
    Item(T),

    // Spaces
    SpaceBefore(&'a Spaced<'a, T>, &'a [CommentOrNewline<'a>]),
    SpaceAfter(&'a Spaced<'a, T>, &'a [CommentOrNewline<'a>]),
}

impl<'a, T> Spaced<'a, T> {
    /// A `Spaced` is multiline if it has newlines or comments before or after the item, since
    /// comments induce newlines!
    pub fn is_multiline(&self) -> bool {
        match self {
            Spaced::Item(_) => false,
            Spaced::SpaceBefore(_, spaces) | Spaced::SpaceAfter(_, spaces) => {
                debug_assert!(!spaces.is_empty());
                true
            }
        }
    }

    pub fn item(&self) -> &T {
        match self {
            Spaced::Item(answer) => answer,
            Spaced::SpaceBefore(next, _spaces) | Spaced::SpaceAfter(next, _spaces) => next.item(),
        }
    }

    pub fn map<U, F: Fn(&T) -> U>(&self, arena: &'a Bump, f: F) -> Spaced<'a, U> {
        match self {
            Spaced::Item(item) => Spaced::Item(f(item)),
            Spaced::SpaceBefore(next, spaces) => {
                Spaced::SpaceBefore(arena.alloc(next.map(arena, f)), spaces)
            }
            Spaced::SpaceAfter(next, spaces) => {
                Spaced::SpaceAfter(arena.alloc(next.map(arena, f)), spaces)
            }
        }
    }
}

impl<'a, T: Debug> Debug for Spaced<'a, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Item(item) => item.fmt(f),
            Self::SpaceBefore(item, space) => f
                .debug_tuple("SpaceBefore")
                .field(item)
                .field(space)
                .finish(),
            Self::SpaceAfter(item, space) => f
                .debug_tuple("SpaceAfter")
                .field(item)
                .field(space)
                .finish(),
        }
    }
}

pub trait ExtractSpaces<'a>: Sized + Copy {
    type Item;
    fn extract_spaces(&self) -> Spaces<'a, Self::Item>;
    fn without_spaces(&self) -> Self::Item;
}

impl<'a, T: ExtractSpaces<'a>> ExtractSpaces<'a> for &'a T {
    type Item = T::Item;
    fn extract_spaces(&self) -> Spaces<'a, Self::Item> {
        (*self).extract_spaces()
    }

    fn without_spaces(&self) -> Self::Item {
        (*self).without_spaces()
    }
}

impl<'a, T: ExtractSpaces<'a>> ExtractSpaces<'a> for Loc<T> {
    type Item = T::Item;
    fn extract_spaces(&self) -> Spaces<'a, Self::Item> {
        let spaces = self.value.extract_spaces();
        Spaces {
            before: spaces.before,
            item: spaces.item,
            after: spaces.after,
        }
    }

    fn without_spaces(&self) -> Self::Item {
        self.value.without_spaces()
    }
}

impl<'a> Header<'a> {
    pub fn upgrade_header_imports(self, arena: &'a Bump) -> (Self, Defs<'a>) {
        let (header, defs) = match self {
            Header::Module(header) => (
                Header::Module(ModuleHeader {
                    interface_imports: None,
                    ..header
                }),
                Self::header_imports_to_defs(arena, header.interface_imports),
            ),
            Header::App(header) => (
                Header::App(AppHeader {
                    old_imports: None,
                    ..header
                }),
                Self::header_imports_to_defs(arena, header.old_imports),
            ),
            Header::Hosted(header) => (
                Header::Hosted(HostedHeader {
                    old_imports: None,
                    ..header
                }),
                Self::header_imports_to_defs(arena, header.old_imports),
            ),
            Header::Package(_) | Header::Platform(_) => (self, Defs::default()),
        };

        (header, defs)
    }

    pub fn header_imports_to_defs(
        arena: &'a Bump,
        imports: Option<
            header::KeywordItem<'a, header::ImportsKeyword, header::ImportsCollection<'a>>,
        >,
    ) -> Defs<'a> {
        let mut defs = Defs::default();

        if let Some(imports) = imports {
            let len = imports.item.len();

            for (index, import) in imports.item.iter().enumerate() {
                let spaced = import.extract_spaces();

                let value_def = match spaced.item {
                    header::ImportsEntry::Package(pkg_name, name, exposed) => {
                        Self::header_import_to_value_def(
                            Some(pkg_name),
                            name,
                            exposed,
                            import.region,
                        )
                    }
                    header::ImportsEntry::Module(name, exposed) => {
                        Self::header_import_to_value_def(None, name, exposed, import.region)
                    }
                    header::ImportsEntry::IngestedFile(path, typed_ident) => {
                        let typed_ident = typed_ident.extract_spaces();

                        ValueDef::IngestedFileImport(IngestedFileImport {
                            before_path: &[],
                            path: Loc {
                                value: path,
                                region: import.region,
                            },
                            name: header::KeywordItem {
                                keyword: Spaces {
                                    before: &[],
                                    item: ImportAsKeyword,
                                    after: &[],
                                },
                                item: typed_ident.item.ident,
                            },
                            annotation: Some(IngestedFileAnnotation {
                                before_colon: merge_spaces(
                                    arena,
                                    typed_ident.before,
                                    typed_ident.item.spaces_before_colon,
                                ),
                                annotation: typed_ident.item.ann,
                            }),
                        })
                    }
                };

                defs.push_value_def(
                    value_def,
                    import.region,
                    if index == 0 {
                        let mut before = vec![CommentOrNewline::Newline, CommentOrNewline::Newline];
                        before.extend(spaced.before);
                        arena.alloc(before)
                    } else {
                        spaced.before
                    },
                    if index == len - 1 {
                        let mut after = spaced.after.to_vec();
                        after.extend_from_slice(imports.item.final_comments());
                        after.push(CommentOrNewline::Newline);
                        after.push(CommentOrNewline::Newline);
                        arena.alloc(after)
                    } else {
                        spaced.after
                    },
                );
            }
        }
        defs
    }

    fn header_import_to_value_def(
        pkg_name: Option<&'a str>,
        name: header::ModuleName<'a>,
        exposed: Collection<'a, Loc<Spaced<'a, header::ExposedName<'a>>>>,
        region: Region,
    ) -> ValueDef<'a> {
        use crate::header::KeywordItem;

        let new_exposed = if exposed.is_empty() {
            None
        } else {
            Some(KeywordItem {
                keyword: Spaces {
                    before: &[],
                    item: ImportExposingKeyword,
                    after: &[],
                },
                item: exposed,
            })
        };

        ValueDef::ModuleImport(ModuleImport {
            before_name: &[],
            name: Loc {
                region,
                value: ImportedModuleName {
                    package: pkg_name,
                    name,
                },
            },
            params: None,
            alias: None,
            exposed: new_exposed,
        })
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Header<'a> {
    Module(ModuleHeader<'a>),
    App(AppHeader<'a>),
    Package(PackageHeader<'a>),
    Platform(PlatformHeader<'a>),
    Hosted(HostedHeader<'a>),
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct WhenBranch<'a> {
    pub patterns: &'a [Loc<Pattern<'a>>],
    pub value: Loc<Expr<'a>>,
    pub guard: Option<Loc<Expr<'a>>>,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct WhenPattern<'a> {
    pub pattern: Loc<Pattern<'a>>,
    pub guard: Option<Loc<Expr<'a>>>,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum StrSegment<'a> {
    Plaintext(&'a str),              // e.g. "foo"
    Unicode(Loc<&'a str>),           // e.g. "00A0" in "\u(00A0)"
    EscapedChar(EscapedChar),        // e.g. '\n' in "Hello!\n"
    Interpolated(Loc<&'a Expr<'a>>), // e.g. "$(expr)"
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SingleQuoteSegment<'a> {
    Plaintext(&'a str),    // e.g. 'f'
    Unicode(Loc<&'a str>), // e.g. '00A0' in '\u(00A0)'
    EscapedChar(EscapedChar), // e.g. '\n'
                           // No interpolated expressions in single-quoted strings
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum EscapedChar {
    Newline,        // \n
    Tab,            // \t
    DoubleQuote,    // \"
    SingleQuote,    // \'
    Backslash,      // \\
    CarriageReturn, // \r
    Dollar,         // \$
}

impl EscapedChar {
    /// Returns the char that would have been originally parsed to
    pub fn to_parsed_char(self) -> char {
        use EscapedChar::*;

        match self {
            Backslash => '\\',
            SingleQuote => '\'',
            DoubleQuote => '"',
            CarriageReturn => 'r',
            Tab => 't',
            Newline => 'n',
            Dollar => '$',
        }
    }

    pub fn unescape(self) -> char {
        use EscapedChar::*;

        match self {
            Backslash => '\\',
            SingleQuote => '\'',
            DoubleQuote => '"',
            CarriageReturn => '\r',
            Tab => '\t',
            Newline => '\n',
            Dollar => '$',
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SingleQuoteLiteral<'a> {
    /// The most common case: a plain character with no escapes
    PlainLine(&'a str),
    Line(&'a [SingleQuoteSegment<'a>]),
}

impl<'a> SingleQuoteLiteral<'a> {
    pub fn to_str_in(&self, arena: &'a Bump) -> Result<&'a str, EString<'a>> {
        match self {
            SingleQuoteLiteral::PlainLine(s) => Ok(s),
            SingleQuoteLiteral::Line(segments) => {
                let mut s = String::new_in(arena);
                for segment in *segments {
                    match segment {
                        SingleQuoteSegment::Plaintext(s2) => s.push_str(s2),
                        SingleQuoteSegment::Unicode(loc) => {
                            let s2 = loc.value;
                            let c = u32::from_str_radix(s2, 16)
                                .map_err(|_| EString::UnicodeEscapeTooLarge(loc.region))?;
                            s.push(
                                char::from_u32(c)
                                    .ok_or(EString::InvalidUnicodeCodepoint(loc.region))?,
                            );
                        }
                        SingleQuoteSegment::EscapedChar(c) => {
                            s.push(c.unescape());
                        }
                    }
                }
                Ok(s.into_bump_str())
            }
        }
    }
}

impl<'a> TryFrom<StrSegment<'a>> for SingleQuoteSegment<'a> {
    type Error = ESingleQuote;

    fn try_from(value: StrSegment<'a>) -> Result<Self, Self::Error> {
        match value {
            StrSegment::Plaintext(s) => Ok(SingleQuoteSegment::Plaintext(s)),
            StrSegment::Unicode(s) => Ok(SingleQuoteSegment::Unicode(s)),
            StrSegment::EscapedChar(s) => Ok(SingleQuoteSegment::EscapedChar(s)),
            StrSegment::Interpolated(_) => Err(ESingleQuote::InterpolationNotAllowed),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum StrLiteral<'a> {
    /// The most common case: a plain string with no escapes or interpolations
    PlainLine(&'a str),
    Line(&'a [StrSegment<'a>]),
    Block(&'a [&'a [StrSegment<'a>]]),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ResultTryKind {
    KeywordPrefix,
    OperatorSuffix,
}

/// A parsed expression. This uses lifetimes extensively for two reasons:
///
/// 1. It uses Bump::alloc for all allocations, which returns a reference.
/// 2. It often stores references into the input string instead of allocating.
///
/// This dramatically reduces allocations during parsing. Once parsing is done,
/// we move on to canonicalization, which often needs to allocate more because
/// it's doing things like turning local variables into fully qualified symbols.
/// Once canonicalization is done, the arena and the input string get dropped.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Expr<'a> {
    // Number Literals
    Float(&'a str),
    Num(&'a str),
    NonBase10Int {
        string: &'a str,
        base: Base,
        is_negative: bool,
    },

    /// String Literals
    Str(StrLiteral<'a>), // string without escapes in it
    /// eg 'b'
    SingleQuote(&'a str),

    /// Look up exactly one field on a record, e.g. `x.foo`.
    RecordAccess(&'a Expr<'a>, &'a str),

    /// e.g. `.foo` or `.0`
    AccessorFunction(Accessor<'a>),

    /// Update the value of a field in a record, e.g. `&foo`
    RecordUpdater(&'a str),

    /// Look up exactly one field on a tuple, e.g. `(x, y).1`.
    TupleAccess(&'a Expr<'a>, &'a str),

    /// Early return on failures - e.g. the ? in `File.read_utf8(path)?`
    TrySuffix(&'a Expr<'a>),

    // Collection Literals
    List(Collection<'a, &'a Loc<Expr<'a>>>),

    RecordUpdate {
        update: &'a Loc<Expr<'a>>,
        fields: Collection<'a, Loc<AssignedField<'a, Expr<'a>>>>,
    },

    Record(Collection<'a, Loc<AssignedField<'a, Expr<'a>>>>),

    Tuple(Collection<'a, &'a Loc<Expr<'a>>>),

    /// Mapper-based record builders, e.g.
    /// { Result.parallel <-
    ///     foo: Http.get_data(Foo),
    ///     bar: Http.get_data(Bar),
    /// }
    RecordBuilder {
        mapper: &'a Loc<Expr<'a>>,
        fields: Collection<'a, Loc<AssignedField<'a, Expr<'a>>>>,
    },

    // Lookups
    Var {
        module_name: &'a str, // module_name will only be filled if the original Roc code stated something like `5 + SomeModule.my_var`, module_name will be blank if it was `5 + my_var`
        ident: &'a str,
    },

    Underscore(&'a str),

    // The "crash" keyword
    Crash,

    // Tags
    Tag(&'a str),

    // Reference to an opaque type, e.g. @Opaq
    OpaqueRef(&'a str),

    // Pattern Matching
    Closure(&'a [Loc<Pattern<'a>>], &'a Loc<Expr<'a>>),
    /// Multiple defs in a row
    Defs(&'a Defs<'a>, &'a Loc<Expr<'a>>),

    Dbg,
    DbgStmt {
        first: &'a Loc<Expr<'a>>,
        extra_args: &'a [&'a Loc<Expr<'a>>],
        continuation: &'a Loc<Expr<'a>>,
        pnc_style: bool,
    },

    /// The `try` keyword that performs early return on errors
    Try,
    // This form of try is a desugared Result unwrapper
    LowLevelTry(&'a Loc<Expr<'a>>, ResultTryKind),

    // This form of debug is a desugared call to roc_dbg
    LowLevelDbg(&'a (&'a str, &'a str), &'a Loc<Expr<'a>>, &'a Loc<Expr<'a>>),

    // Application
    /// To apply by name, do Apply(Var(...), ...)
    /// To apply a tag by name, do Apply(Tag(...), ...)
    Apply(&'a Loc<Expr<'a>>, &'a [&'a Loc<Expr<'a>>], CalledVia),
    PncApply(&'a Loc<Expr<'a>>, Collection<'a, &'a Loc<Expr<'a>>>),
    BinOps(&'a [(Loc<Expr<'a>>, Loc<BinOp>)], &'a Loc<Expr<'a>>),
    UnaryOp(&'a Loc<Expr<'a>>, Loc<UnaryOp>),

    // Conditionals
    If {
        if_thens: &'a [(Loc<Expr<'a>>, Loc<Expr<'a>>)],
        final_else: &'a Loc<Expr<'a>>,
        indented_else: bool,
    },
    When(
        /// The condition
        &'a Loc<Expr<'a>>,
        /// A | B if bool -> expression
        /// <Pattern 1> | <Pattern 2> if <Guard> -> <Expr>
        /// Vec, because there may be many patterns, and the guard
        /// is Option<Expr> because each branch may be preceded by
        /// a guard (".. if ..").
        &'a [&'a WhenBranch<'a>],
    ),

    Return(
        /// The return value
        &'a Loc<Expr<'a>>,
        /// The unused code after the return statement
        Option<&'a Loc<Expr<'a>>>,
    ),

    // Blank Space (e.g. comments, spaces, newlines) before or after an expression.
    // We preserve this for the formatter; canonicalization ignores it.
    SpaceBefore(&'a Expr<'a>, &'a [CommentOrNewline<'a>]),
    SpaceAfter(&'a Expr<'a>, &'a [CommentOrNewline<'a>]),
    ParensAround(&'a Expr<'a>),

    // Problems
    MalformedIdent(&'a str, crate::ident::BadIdent),
    // Both operators were non-associative, e.g. (True == False == False).
    // We should tell the author to disambiguate by grouping them with parens.
    PrecedenceConflict(&'a PrecedenceConflict<'a>),
    EmptyRecordBuilder(&'a Loc<Expr<'a>>),
    SingleFieldRecordBuilder(&'a Loc<Expr<'a>>),
    OptionalFieldInRecordBuilder(&'a Loc<&'a str>, &'a Loc<Expr<'a>>),
}

impl Expr<'_> {
    pub fn get_region_spanning_binops(&self) -> Region {
        match self {
            Expr::BinOps(firsts, last) => {
                let mut region = last.region;

                for (loc_expr, _) in firsts.iter() {
                    region = Region::span_across(&loc_expr.region, &region);
                }

                region
            }
            _ => internal_error!("other expr types not supported"),
        }
    }
}

pub fn split_loc_exprs_around<'a>(
    items: &'a [&Loc<Expr<'a>>],
    index: usize,
) -> (&'a [&'a Loc<Expr<'a>>], &'a [&'a Loc<Expr<'a>>]) {
    let (before, rest) = items.split_at(index);
    let after = &rest[1..]; // Skip the index element

    (before, after)
}

pub fn split_around<T>(items: &[T], target: usize) -> (&[T], &[T]) {
    let (before, rest) = items.split_at(target);
    let after = &rest[1..];

    (before, after)
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct PrecedenceConflict<'a> {
    pub whole_region: Region,
    pub binop1_position: Position,
    pub binop2_position: Position,
    pub binop1: BinOp,
    pub binop2: BinOp,
    pub expr: &'a Loc<Expr<'a>>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct TypeHeader<'a> {
    pub name: Loc<&'a str>,
    pub vars: &'a [Loc<TypeVar<'a>>],
}

impl<'a> TypeHeader<'a> {
    pub fn region(&self) -> Region {
        Region::across_all(
            [self.name.region]
                .iter()
                .chain(self.vars.iter().map(|v| &v.region)),
        )
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TypeVar<'a> {
    Identifier(&'a str),
    SpaceBefore(&'a TypeVar<'a>, &'a [CommentOrNewline<'a>]),
    SpaceAfter(&'a TypeVar<'a>, &'a [CommentOrNewline<'a>]),

    // These are syntactically parsed as exprs first, so if there's anything else here,
    // we consider it malformed but preserve it for error reporting and more resilient parsing.
    Malformed(&'a Expr<'a>),
}

/// The `implements` keyword associated with ability definitions.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Implements<'a> {
    Implements,
    SpaceBefore(&'a Implements<'a>, &'a [CommentOrNewline<'a>]),
    SpaceAfter(&'a Implements<'a>, &'a [CommentOrNewline<'a>]),
}

/// An ability demand is a value defining the ability; for example `hash : a -> U64 where a implements Hash`
/// for a `Hash` ability.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct AbilityMember<'a> {
    pub name: Loc<Spaced<'a, &'a str>>,
    pub typ: Loc<TypeAnnotation<'a>>,
}

impl AbilityMember<'_> {
    pub fn region(&self) -> Region {
        Region::across_all([self.name.region, self.typ.region].iter())
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TypeDef<'a> {
    /// A type alias. This is like a standalone annotation, except the pattern
    /// must be a capitalized Identifier, e.g.
    ///
    /// Foo : Bar Baz
    Alias {
        header: TypeHeader<'a>,
        ann: Loc<TypeAnnotation<'a>>,
    },

    /// An opaque type, wrapping its inner type. E.g. Age := U64.
    Opaque {
        header: TypeHeader<'a>,
        typ: Loc<TypeAnnotation<'a>>,
        derived: Option<&'a ImplementsAbilities<'a>>,
    },

    /// An ability definition. E.g.
    ///   Hash implements
    ///     hash : a -> U64 where a implements Hash
    Ability {
        header: TypeHeader<'a>,
        loc_implements: Loc<Implements<'a>>,
        members: &'a [AbilityMember<'a>],
    },
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ValueDef<'a> {
    // TODO in canonicalization, validate the pattern; only certain patterns
    // are allowed in annotations.
    Annotation(Loc<Pattern<'a>>, Loc<TypeAnnotation<'a>>),

    // TODO in canonicalization, check to see if there are any newlines after the
    // annotation; if not, and if it's followed by a Body, then the annotation
    // applies to that expr! (TODO: verify that the pattern for both annotation and body match.)
    // No need to track that relationship in any data structure.
    Body(&'a Loc<Pattern<'a>>, &'a Loc<Expr<'a>>),

    AnnotatedBody {
        ann_pattern: &'a Loc<Pattern<'a>>,
        ann_type: &'a Loc<TypeAnnotation<'a>>,
        lines_between: &'a [CommentOrNewline<'a>],
        body_pattern: &'a Loc<Pattern<'a>>,
        body_expr: &'a Loc<Expr<'a>>,
    },

    Dbg {
        condition: &'a Loc<Expr<'a>>,
        preceding_comment: Region,
    },

    Expect {
        condition: &'a Loc<Expr<'a>>,
        preceding_comment: Region,
    },

    /// e.g. `import InternalHttp as Http exposing [Req]`.
    ModuleImport(ModuleImport<'a>),

    /// e.g. `import "path/to/my/file.txt" as myFile : Str`
    IngestedFileImport(IngestedFileImport<'a>),

    Stmt(&'a Loc<Expr<'a>>),

    StmtAfterExpr,
}

impl<'a> ValueDef<'a> {
    pub fn replace_expr(&mut self, new_expr: &'a Loc<Expr<'a>>) {
        match self {
            ValueDef::Body(_, expr) => *expr = new_expr,
            ValueDef::AnnotatedBody { body_expr, .. } => *body_expr = new_expr,
            _ => internal_error!("replacing expr in unsupported ValueDef"),
        }
    }
}

pub struct RecursiveValueDefIter<'a, 'b> {
    current: &'b Defs<'a>,
    index: usize,
    pending: std::vec::Vec<&'b Defs<'a>>,
}

impl<'a, 'b> RecursiveValueDefIter<'a, 'b> {
    pub fn new(defs: &'b Defs<'a>) -> Self {
        Self {
            current: defs,
            index: 0,
            pending: vec![],
        }
    }

    pub fn push_pending_from_expr(&mut self, expr: &'b Expr<'a>) {
        let mut expr_stack = vec![expr];

        use Expr::*;

        macro_rules! push_stack_from_record_fields {
            ($fields:expr) => {
                for field in $fields.items {
                    let mut current = field.value;

                    loop {
                        use AssignedField::*;

                        match current {
                            RequiredValue(_, _, loc_val)
                            | OptionalValue(_, _, loc_val)
                            | IgnoredValue(_, _, loc_val) => break expr_stack.push(&loc_val.value),
                            SpaceBefore(next, _) | SpaceAfter(next, _) => current = *next,
                            LabelOnly(_) => break,
                        }
                    }
                }
            };
        }

        while let Some(next) = expr_stack.pop() {
            match next {
                Defs(defs, cont) => {
                    self.pending.push(defs);
                    // We purposefully don't push the exprs inside defs here
                    // because they will be traversed when the iterator
                    // gets to their parent def.
                    expr_stack.push(&cont.value);
                }
                List(list) => {
                    expr_stack.reserve(list.len());
                    for loc_expr in list.items {
                        expr_stack.push(&loc_expr.value);
                    }
                }
                RecordUpdate { update, fields } => {
                    expr_stack.reserve(fields.len() + 1);
                    expr_stack.push(&update.value);
                    push_stack_from_record_fields!(fields);
                }
                Record(fields) => {
                    expr_stack.reserve(fields.len());
                    push_stack_from_record_fields!(fields);
                }
                Tuple(fields) => {
                    expr_stack.reserve(fields.len());
                    for loc_expr in fields.items {
                        expr_stack.push(&loc_expr.value);
                    }
                }
                RecordBuilder {
                    mapper: map2,
                    fields,
                } => {
                    expr_stack.reserve(fields.len() + 1);
                    expr_stack.push(&map2.value);
                    push_stack_from_record_fields!(fields);
                }
                Closure(_, body) => expr_stack.push(&body.value),
                DbgStmt {
                    first,
                    extra_args,
                    continuation,
                    pnc_style: _,
                } => {
                    expr_stack.reserve(2);
                    expr_stack.push(&first.value);
                    for arg in extra_args.iter() {
                        expr_stack.push(&arg.value);
                    }
                    expr_stack.push(&continuation.value);
                }
                LowLevelDbg(_, condition, cont) => {
                    expr_stack.reserve(2);
                    expr_stack.push(&condition.value);
                    expr_stack.push(&cont.value);
                }
                LowLevelTry(loc_expr, _) => {
                    expr_stack.push(&loc_expr.value);
                }
                Return(return_value, after_return) => {
                    if let Some(after_return) = after_return {
                        expr_stack.reserve(2);
                        expr_stack.push(&return_value.value);
                        expr_stack.push(&after_return.value);
                    } else {
                        expr_stack.push(&return_value.value);
                    }
                }
                Apply(fun, args, _) => {
                    expr_stack.reserve(args.len() + 1);
                    expr_stack.push(&fun.value);

                    for loc_expr in args.iter() {
                        expr_stack.push(&loc_expr.value);
                    }
                }
                PncApply(fun, args) => {
                    expr_stack.reserve(args.len() + 1);
                    expr_stack.push(&fun.value);

                    for loc_expr in args.iter() {
                        expr_stack.push(&loc_expr.value);
                    }
                }
                BinOps(ops, expr) => {
                    expr_stack.reserve(ops.len() + 1);

                    for (a, _) in ops.iter() {
                        expr_stack.push(&a.value);
                    }
                    expr_stack.push(&expr.value);
                }
                UnaryOp(expr, _) => expr_stack.push(&expr.value),
                If {
                    if_thens,
                    final_else,
                    ..
                } => {
                    expr_stack.reserve(if_thens.len() * 2 + 1);

                    for (condition, consequent) in if_thens.iter() {
                        expr_stack.push(&condition.value);
                        expr_stack.push(&consequent.value);
                    }
                    expr_stack.push(&final_else.value);
                }
                When(condition, branches) => {
                    expr_stack.reserve(branches.len() + 1);
                    expr_stack.push(&condition.value);

                    for WhenBranch {
                        patterns: _,
                        value,
                        guard,
                    } in branches.iter()
                    {
                        expr_stack.push(&value.value);

                        match guard {
                            None => {}
                            Some(guard) => expr_stack.push(&guard.value),
                        }
                    }
                }
                RecordAccess(expr, _)
                | TupleAccess(expr, _)
                | TrySuffix(expr)
                | SpaceBefore(expr, _)
                | SpaceAfter(expr, _)
                | ParensAround(expr) => expr_stack.push(expr),

                EmptyRecordBuilder(loc_expr)
                | SingleFieldRecordBuilder(loc_expr)
                | OptionalFieldInRecordBuilder(_, loc_expr) => expr_stack.push(&loc_expr.value),

                Float(_)
                | Num(_)
                | NonBase10Int { .. }
                | Str(_)
                | SingleQuote(_)
                | AccessorFunction(_)
                | RecordUpdater(_)
                | Var { .. }
                | Underscore(_)
                | Crash
                | Dbg
                | Try
                | Tag(_)
                | OpaqueRef(_)
                | MalformedIdent(_, _)
                | PrecedenceConflict(_) => { /* terminal */ }
            }
        }
    }
}

impl<'a, 'b> Iterator for RecursiveValueDefIter<'a, 'b> {
    type Item = (&'b ValueDef<'a>, &'b Region);

    fn next(&mut self) -> Option<Self::Item> {
        match self.current.tags.get(self.index) {
            Some(tag) => {
                if let Err(def_index) = tag.split() {
                    let def = &self.current.value_defs[def_index.index()];
                    let region = &self.current.regions[self.index];

                    match def {
                        ValueDef::Body(_, body) => self.push_pending_from_expr(&body.value),

                        ValueDef::AnnotatedBody {
                            ann_pattern: _,
                            ann_type: _,
                            lines_between: _,
                            body_pattern: _,
                            body_expr,
                        } => self.push_pending_from_expr(&body_expr.value),

                        ValueDef::Dbg {
                            condition,
                            preceding_comment: _,
                        }
                        | ValueDef::Expect {
                            condition,
                            preceding_comment: _,
                        } => self.push_pending_from_expr(&condition.value),

                        ValueDef::ModuleImport(ModuleImport {
                            before_name: _,
                            name: _,
                            alias: _,
                            exposed: _,
                            params,
                        }) => {
                            if let Some(ModuleImportParams { before: _, params }) = params {
                                for loc_assigned_field in params.value.items {
                                    if let Some(expr) = loc_assigned_field.value.value() {
                                        self.push_pending_from_expr(&expr.value);
                                    }
                                }
                            }
                        }
                        ValueDef::Stmt(loc_expr) => self.push_pending_from_expr(&loc_expr.value),
                        ValueDef::Annotation(_, _)
                        | ValueDef::IngestedFileImport(_)
                        | ValueDef::StmtAfterExpr => {}
                    }

                    self.index += 1;

                    Some((def, region))
                } else {
                    // Not a value def, try next
                    self.index += 1;
                    self.next()
                }
            }

            None => {
                self.current = self.pending.pop()?;
                self.index = 0;
                self.next()
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ModuleImport<'a> {
    pub before_name: &'a [CommentOrNewline<'a>],
    pub name: Loc<ImportedModuleName<'a>>,
    pub params: Option<ModuleImportParams<'a>>,
    pub alias: Option<header::KeywordItem<'a, ImportAsKeyword, Loc<ImportAlias<'a>>>>,
    pub exposed: Option<
        header::KeywordItem<
            'a,
            ImportExposingKeyword,
            Collection<'a, Loc<Spaced<'a, header::ExposedName<'a>>>>,
        >,
    >,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ModuleImportParams<'a> {
    pub before: &'a [CommentOrNewline<'a>],
    pub params: Loc<Collection<'a, Loc<AssignedField<'a, Expr<'a>>>>>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct IngestedFileImport<'a> {
    pub before_path: &'a [CommentOrNewline<'a>],
    pub path: Loc<StrLiteral<'a>>,
    pub name: header::KeywordItem<'a, ImportAsKeyword, Loc<&'a str>>,
    pub annotation: Option<IngestedFileAnnotation<'a>>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct IngestedFileAnnotation<'a> {
    pub before_colon: &'a [CommentOrNewline<'a>],
    pub annotation: Loc<TypeAnnotation<'a>>,
}

impl<'a> Malformed for IngestedFileAnnotation<'a> {
    fn is_malformed(&self) -> bool {
        self.annotation.value.is_malformed()
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct ImportAsKeyword;

impl header::Keyword for ImportAsKeyword {
    const KEYWORD: &'static str = "as";
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct ImportExposingKeyword;

impl header::Keyword for ImportExposingKeyword {
    const KEYWORD: &'static str = "exposing";
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ImportedModuleName<'a> {
    pub package: Option<&'a str>,
    pub name: ModuleName<'a>,
}

impl<'a> From<ImportedModuleName<'a>> for QualifiedModuleName<'a> {
    fn from(imported: ImportedModuleName<'a>) -> Self {
        Self {
            opt_package: imported.package,
            module: imported.name.into(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ImportAlias<'a>(&'a str);

impl<'a> ImportAlias<'a> {
    pub const fn new(name: &'a str) -> Self {
        ImportAlias(name)
    }

    pub const fn as_str(&'a self) -> &'a str {
        self.0
    }
}

#[derive(Debug, Clone, PartialEq, Default)]
pub struct Defs<'a> {
    pub tags: std::vec::Vec<EitherIndex<TypeDef<'a>, ValueDef<'a>>>,
    pub regions: std::vec::Vec<Region>,
    pub space_before: std::vec::Vec<Slice<CommentOrNewline<'a>>>,
    pub space_after: std::vec::Vec<Slice<CommentOrNewline<'a>>>,
    pub spaces: std::vec::Vec<CommentOrNewline<'a>>,
    pub type_defs: std::vec::Vec<TypeDef<'a>>,
    pub value_defs: std::vec::Vec<ValueDef<'a>>,
}

impl<'a> Defs<'a> {
    pub fn is_empty(&self) -> bool {
        self.tags.is_empty()
    }

    pub fn len(&self) -> usize {
        self.tags.len()
    }

    pub fn defs(&self) -> impl Iterator<Item = Result<&TypeDef<'a>, &ValueDef<'a>>> {
        self.tags.iter().map(|tag| match tag.split() {
            Ok(type_index) => Ok(&self.type_defs[type_index.index()]),
            Err(value_index) => Err(&self.value_defs[value_index.index()]),
        })
    }

    pub fn loc_defs<'b>(
        &'b self,
    ) -> impl Iterator<Item = Result<Loc<TypeDef<'a>>, Loc<ValueDef<'a>>>> + 'b {
        self.tags
            .iter()
            .enumerate()
            .map(|(i, tag)| match tag.split() {
                Ok(type_index) => Ok(Loc::at(self.regions[i], self.type_defs[type_index.index()])),
                Err(value_index) => Err(Loc::at(
                    self.regions[i],
                    self.value_defs[value_index.index()],
                )),
            })
    }

    pub fn list_value_defs(&self) -> impl Iterator<Item = (usize, &ValueDef<'a>)> {
        self.tags
            .iter()
            .enumerate()
            .filter_map(|(tag_index, tag)| match tag.split() {
                Ok(_) => None,
                Err(value_index) => Some((tag_index, &self.value_defs[value_index.index()])),
            })
    }

    pub fn last(&self) -> Option<Result<&TypeDef<'a>, &ValueDef<'a>>> {
        self.tags.last().map(|tag| match tag.split() {
            Ok(type_index) => Ok(&self.type_defs[type_index.index()]),
            Err(value_index) => Err(&self.value_defs[value_index.index()]),
        })
    }

    pub fn pop_last_value(&mut self) -> Option<&'a Loc<Expr<'a>>> {
        let last_value_suffix = self
            .tags
            .iter()
            .enumerate()
            .rev()
            .find_map(|(tag_index, tag)| match tag.split() {
                Ok(_) => None,
                Err(value_index) => match self.value_defs[value_index.index()] {
                    ValueDef::Body(
                        Loc {
                            value: Pattern::RecordDestructure(collection),
                            ..
                        },
                        loc_expr,
                    ) if collection.is_empty() => Some((tag_index, loc_expr)),
                    ValueDef::Stmt(loc_expr) => Some((tag_index, loc_expr)),
                    _ => None,
                },
            });

        if let Some((tag_index, loc_expr)) = last_value_suffix {
            self.remove_tag(tag_index);
            Some(loc_expr)
        } else {
            None
        }
    }

    pub fn remove_tag(&mut self, tag_index: usize) {
        match self
            .tags
            .get(tag_index)
            .expect("got an invalid index for Defs")
            .split()
        {
            Ok(type_index) => {
                // remove from vec
                self.type_defs.remove(type_index.index());

                // update all of the remaining indexes in type_defs
                for (current_tag_index, tag) in self.tags.iter_mut().enumerate() {
                    // only update later indexes into type_defs
                    if current_tag_index > tag_index && tag.split().is_ok() {
                        tag.decrement_index();
                    }
                }
            }
            Err(value_index) => {
                // remove from vec
                self.value_defs.remove(value_index.index());

                // update all of the remaining indexes in value_defs
                for (current_tag_index, tag) in self.tags.iter_mut().enumerate() {
                    // only update later indexes into value_defs
                    if current_tag_index > tag_index && tag.split().is_err() {
                        tag.decrement_index();
                    }
                }
            }
        }
        self.tags.remove(tag_index);
        self.regions.remove(tag_index);
        self.space_after.remove(tag_index);
        self.space_before.remove(tag_index);
    }

    /// NOTE assumes the def itself is pushed already!
    fn push_def_help(
        &mut self,
        tag: EitherIndex<TypeDef<'a>, ValueDef<'a>>,
        region: Region,
        spaces_before: &[CommentOrNewline<'a>],
        spaces_after: &[CommentOrNewline<'a>],
    ) {
        self.tags.push(tag);

        self.regions.push(region);

        let before = slice_extend_new(&mut self.spaces, spaces_before.iter().copied());
        self.space_before.push(before);

        let after = slice_extend_new(&mut self.spaces, spaces_after.iter().copied());
        self.space_after.push(after);
    }

    pub fn push_value_def(
        &mut self,
        value_def: ValueDef<'a>,
        region: Region,
        spaces_before: &[CommentOrNewline<'a>],
        spaces_after: &[CommentOrNewline<'a>],
    ) {
        let value_def_index = index_push_new(&mut self.value_defs, value_def);
        let tag = EitherIndex::from_right(value_def_index);
        self.push_def_help(tag, region, spaces_before, spaces_after)
    }

    /// Replace with `value_def` at the given index
    pub fn replace_with_value_def(
        &mut self,
        tag_index: usize,
        value_def: ValueDef<'a>,
        region: Region,
    ) {
        // split() converts `EitherIndex<TypeDef<'a>, ValueDef<'a>>` to:
        // `Result<Index<TypeDef<'a>>, Index<ValueDef<'a>>>`
        //
        match self.tags[tag_index].split() {
            Ok(_type_index) => {
                self.remove_tag(tag_index);
                self.push_value_def(value_def, region, &[], &[]);
            }
            Err(value_index) => {
                self.regions[tag_index] = region;
                self.value_defs[value_index.index()] = value_def;
            }
        }
    }

    pub fn push_type_def(
        &mut self,
        type_def: TypeDef<'a>,
        region: Region,
        spaces_before: &[CommentOrNewline<'a>],
        spaces_after: &[CommentOrNewline<'a>],
    ) {
        let type_def_index = index_push_new(&mut self.type_defs, type_def);
        let tag = EitherIndex::from_left(type_def_index);
        self.push_def_help(tag, region, spaces_before, spaces_after)
    }

    /// Split the defs around a given target index
    ///
    /// This is useful for unwrapping suffixed `!`
    pub fn split_defs_around(&self, target: usize) -> SplitDefsAround<'a> {
        let mut before = Defs::default();
        let mut after = Defs::default();

        for (tag_index, tag) in self.tags.iter().enumerate() {
            let region = self.regions[tag_index];
            let space_before = {
                let start = self.space_before[tag_index].start() as usize;
                let len = self.space_before[tag_index].len();

                &self.spaces[start..(start + len)]
            };
            let space_after = {
                let start = self.space_after[tag_index].start() as usize;
                let len = self.space_after[tag_index].len();

                &self.spaces[start..(start + len)]
            };

            match tag.split() {
                Ok(type_def_index) => {
                    let type_def = self.type_defs[type_def_index.index()];

                    match tag_index.cmp(&target) {
                        std::cmp::Ordering::Less => {
                            // before
                            let type_def_index = index_push_new(&mut before.type_defs, type_def);
                            let tag = EitherIndex::from_left(type_def_index);
                            before.push_def_help(tag, region, space_before, space_after);
                        }
                        std::cmp::Ordering::Greater => {
                            // after
                            let type_def_index = index_push_new(&mut after.type_defs, type_def);
                            let tag = EitherIndex::from_left(type_def_index);
                            after.push_def_help(tag, region, space_before, space_after);
                        }
                        std::cmp::Ordering::Equal => {
                            // target, do nothing
                        }
                    }
                }
                Err(value_def_index) => {
                    let value_def = self.value_defs[value_def_index.index()];

                    match tag_index.cmp(&target) {
                        std::cmp::Ordering::Less => {
                            // before
                            let new_value_def_index =
                                index_push_new(&mut before.value_defs, value_def);
                            let tag = EitherIndex::from_right(new_value_def_index);
                            before.push_def_help(tag, region, space_before, space_after);
                        }
                        std::cmp::Ordering::Greater => {
                            // after
                            let new_value_def_index =
                                index_push_new(&mut after.value_defs, value_def);
                            let tag = EitherIndex::from_right(new_value_def_index);
                            after.push_def_help(tag, region, space_before, space_after);
                        }
                        std::cmp::Ordering::Equal => {
                            // target, do nothing
                        }
                    }
                }
            }
        }

        SplitDefsAround { before, after }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct SplitDefsAround<'a> {
    pub before: Defs<'a>,
    pub after: Defs<'a>,
}

/// Should always be a zero-argument `Apply`; we'll check this in canonicalization
pub type AbilityName<'a> = Loc<TypeAnnotation<'a>>;

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct ImplementsClause<'a> {
    pub var: Loc<Spaced<'a, &'a str>>,
    pub abilities: &'a [AbilityName<'a>],
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum AbilityImpls<'a> {
    // `{ eq: myEq }`
    AbilityImpls(Collection<'a, Loc<AssignedField<'a, Expr<'a>>>>),

    // We preserve this for the formatter; canonicalization ignores it.
    SpaceBefore(&'a AbilityImpls<'a>, &'a [CommentOrNewline<'a>]),
    SpaceAfter(&'a AbilityImpls<'a>, &'a [CommentOrNewline<'a>]),
}

/// `Eq` or `Eq { eq: myEq }`
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum ImplementsAbility<'a> {
    ImplementsAbility {
        /// Should be a zero-argument `Apply` or an error; we'll check this in canonicalization
        ability: Loc<TypeAnnotation<'a>>,
        impls: Option<Loc<AbilityImpls<'a>>>,
    },

    // We preserve this for the formatter; canonicalization ignores it.
    SpaceBefore(&'a ImplementsAbility<'a>, &'a [CommentOrNewline<'a>]),
    SpaceAfter(&'a ImplementsAbility<'a>, &'a [CommentOrNewline<'a>]),
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct ImplementsAbilities<'a> {
    pub before_implements_kw: &'a [CommentOrNewline<'a>],
    pub implements: Region,
    pub after_implements_kw: &'a [CommentOrNewline<'a>],
    pub item: Loc<Collection<'a, Loc<ImplementsAbility<'a>>>>,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum FunctionArrow {
    /// ->
    Pure,
    /// =>
    Effectful,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum TypeAnnotation<'a> {
    /// A function. The types of its arguments, the type of arrow used, then the type of its return value.
    Function(
        &'a [Loc<TypeAnnotation<'a>>],
        FunctionArrow,
        &'a Loc<TypeAnnotation<'a>>,
    ),

    /// Applying a type to some arguments (e.g. Map.Map String Int)
    Apply(&'a str, &'a str, &'a [Loc<TypeAnnotation<'a>>]),

    /// A bound type variable, e.g. `a` in `(a -> a)`
    BoundVariable(&'a str),

    /// Inline type alias, e.g. `as List a` in `[Cons a (List a), Nil] as List a`
    As(
        &'a Loc<TypeAnnotation<'a>>,
        &'a [CommentOrNewline<'a>],
        TypeHeader<'a>,
    ),

    Record {
        fields: Collection<'a, Loc<AssignedField<'a, TypeAnnotation<'a>>>>,
        /// The row type variable in an open record, e.g. the `r` in `{ name: Str }r`.
        /// This is None if it's a closed record annotation like `{ name: Str }`.
        ext: Option<&'a Loc<TypeAnnotation<'a>>>,
    },

    Tuple {
        elems: Collection<'a, Loc<TypeAnnotation<'a>>>,
        /// The row type variable in an open tuple, e.g. the `r` in `( Str, Str )r`.
        /// This is None if it's a closed tuple annotation like `( Str, Str )`.
        ext: Option<&'a Loc<TypeAnnotation<'a>>>,
    },

    /// A tag union, e.g. `[
    TagUnion {
        /// The row type variable in an open tag union, e.g. the `a` in `[Foo, Bar]a`.
        /// This is None if it's a closed tag union like `[Foo, Bar]`.
        ext: Option<&'a Loc<TypeAnnotation<'a>>>,
        tags: Collection<'a, Loc<Tag<'a>>>,
    },

    /// '_', indicating the compiler should infer the type
    Inferred,

    /// The `*` type variable, e.g. in (List *)
    Wildcard,

    /// A "where" clause demanding abilities designated by a `where`, e.g. `a -> U64 where a implements Hash`
    Where(&'a Loc<TypeAnnotation<'a>>, &'a [Loc<ImplementsClause<'a>>]),

    // We preserve this for the formatter; canonicalization ignores it.
    SpaceBefore(&'a TypeAnnotation<'a>, &'a [CommentOrNewline<'a>]),
    SpaceAfter(&'a TypeAnnotation<'a>, &'a [CommentOrNewline<'a>]),

    /// A malformed type annotation, which will code gen to a runtime error
    Malformed(&'a str),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Tag<'a> {
    Apply {
        name: Loc<&'a str>,
        args: &'a [Loc<TypeAnnotation<'a>>],
    },

    // We preserve this for the formatter; canonicalization ignores it.
    SpaceBefore(&'a Tag<'a>, &'a [CommentOrNewline<'a>]),
    SpaceAfter(&'a Tag<'a>, &'a [CommentOrNewline<'a>]),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum AssignedField<'a, Val> {
    // A required field with a label, e.g. `{ name: "blah" }` or `{ name : Str }`
    RequiredValue(Loc<&'a str>, &'a [CommentOrNewline<'a>], &'a Loc<Val>),

    // An optional field with a label, e.g. `{ name ? "blah" }`
    //
    // NOTE: This only comes up in type annotations (e.g. `name ? Str`)
    // and in destructuring patterns (e.g. `{ name ? "blah" }`)
    OptionalValue(Loc<&'a str>, &'a [CommentOrNewline<'a>], &'a Loc<Val>),

    // An ignored field, e.g. `{ _name: "blah" }` or `{ _ : Str }`
    IgnoredValue(Loc<&'a str>, &'a [CommentOrNewline<'a>], &'a Loc<Val>),

    // A label with no value, e.g. `{ name }` (this is sugar for { name: name })
    LabelOnly(Loc<&'a str>),

    // We preserve this for the formatter; canonicalization ignores it.
    SpaceBefore(&'a AssignedField<'a, Val>, &'a [CommentOrNewline<'a>]),
    SpaceAfter(&'a AssignedField<'a, Val>, &'a [CommentOrNewline<'a>]),
}

impl<'a, Val> AssignedField<'a, Val> {
    pub fn value(&self) -> Option<&Loc<Val>> {
        let mut current = self;

        loop {
            match current {
                Self::RequiredValue(_, _, val)
                | Self::OptionalValue(_, _, val)
                | Self::IgnoredValue(_, _, val) => break Some(val),
                Self::LabelOnly(_) => break None,
                Self::SpaceBefore(next, _) | Self::SpaceAfter(next, _) => current = *next,
            }
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CommentOrNewline<'a> {
    Newline,
    LineComment(&'a str),
    DocComment(&'a str),
}

impl<'a> CommentOrNewline<'a> {
    pub fn is_comment(&self) -> bool {
        use CommentOrNewline::*;
        match self {
            Newline => false,
            LineComment(_) => true,
            DocComment(_) => true,
        }
    }

    pub fn is_newline(&self) -> bool {
        use CommentOrNewline::*;
        match self {
            Newline => true,
            LineComment(_) => false,
            DocComment(_) => false,
        }
    }

    pub fn comment_str(&'a self) -> Option<&'a str> {
        match self {
            CommentOrNewline::LineComment(s) => Some(*s),
            CommentOrNewline::DocComment(s) => Some(*s),
            _ => None,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct PatternAs<'a> {
    pub spaces_before: &'a [CommentOrNewline<'a>],
    pub identifier: Loc<&'a str>,
}

impl<'a> PatternAs<'a> {
    pub fn equivalent(&self, other: &Self) -> bool {
        self.identifier.value == other.identifier.value
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Pattern<'a> {
    // Identifier
    Identifier {
        ident: &'a str,
    },
    QualifiedIdentifier {
        module_name: &'a str,
        ident: &'a str,
    },

    Tag(&'a str),

    OpaqueRef(&'a str),

    Apply(&'a Loc<Pattern<'a>>, &'a [Loc<Pattern<'a>>]),

    PncApply(&'a Loc<Pattern<'a>>, Collection<'a, Loc<Pattern<'a>>>),

    /// This is Located<Pattern> rather than Located<str> so we can record comments
    /// around the destructured names, e.g. { x ### x does stuff ###, y }
    /// In practice, these patterns will always be Identifier
    RecordDestructure(Collection<'a, Loc<Pattern<'a>>>),

    /// A required field pattern, e.g. { x: Just 0 } -> ...
    /// Can only occur inside of a RecordDestructure
    RequiredField(&'a str, &'a Loc<Pattern<'a>>),

    /// An optional field pattern, e.g. { x ? Just 0 } -> ...
    /// Can only occur inside of a RecordDestructure
    OptionalField(&'a str, &'a Loc<Expr<'a>>),

    // Literal
    NumLiteral(&'a str),
    NonBase10Literal {
        string: &'a str,
        base: Base,
        is_negative: bool,
    },
    FloatLiteral(&'a str),
    StrLiteral(StrLiteral<'a>),

    /// Underscore pattern
    /// Contains the name of underscore pattern (e.g. "a" is for "_a" in code)
    /// Empty string is unnamed pattern ("" is for "_" in code)
    Underscore(&'a str),
    SingleQuote(&'a str),

    /// A tuple pattern, e.g. (Just x, 1)
    Tuple(Collection<'a, Loc<Pattern<'a>>>),

    /// A list pattern like [_, x, ..]
    List(Collection<'a, Loc<Pattern<'a>>>),

    /// A list-rest pattern ".."
    /// Can only occur inside of a [Pattern::List]
    ListRest(Option<(&'a [CommentOrNewline<'a>], PatternAs<'a>)>),

    As(&'a Loc<Pattern<'a>>, PatternAs<'a>),

    // Space
    SpaceBefore(&'a Pattern<'a>, &'a [CommentOrNewline<'a>]),
    SpaceAfter(&'a Pattern<'a>, &'a [CommentOrNewline<'a>]),

    // Malformed
    Malformed(&'a str),
    MalformedIdent(&'a str, crate::ident::BadIdent),
    MalformedExpr(&'a Expr<'a>),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Base {
    Octal,
    Binary,
    Hex,
    Decimal,
}

impl<'a> Pattern<'a> {
    /// Check that patterns are equivalent, meaning they have the same shape, but may have
    /// different locations/whitespace
    pub fn equivalent(&self, other: &Self) -> bool {
        use Pattern::*;

        match other {
            SpaceBefore(y, _) | SpaceAfter(y, _) => {
                return self.equivalent(y);
            }
            _ => {}
        }

        match self {
            Tag(x) => {
                if let Tag(y) = other {
                    x == y
                } else {
                    false
                }
            }
            Apply(constructor_x, args_x) => {
                if let Apply(constructor_y, args_y) = other {
                    let equivalent_args = args_x
                        .iter()
                        .zip(args_y.iter())
                        .all(|(p, q)| p.value.equivalent(&q.value));

                    constructor_x.value.equivalent(&constructor_y.value) && equivalent_args
                } else if let PncApply(constructor_y, args_y) = other {
                    let equivalent_args = args_x
                        .iter()
                        .zip(args_y.iter())
                        .all(|(p, q)| p.value.equivalent(&q.value));

                    constructor_x.value.equivalent(&constructor_y.value) && equivalent_args
                } else {
                    false
                }
            }
            PncApply(constructor_x, args_x) => {
                if let PncApply(constructor_y, args_y) = other {
                    let equivalent_args = args_x
                        .iter()
                        .zip(args_y.iter())
                        .all(|(p, q)| p.value.equivalent(&q.value));

                    constructor_x.value.equivalent(&constructor_y.value) && equivalent_args
                } else if let Apply(constructor_y, args_y) = other {
                    let equivalent_args = args_x
                        .iter()
                        .zip(args_y.iter())
                        .all(|(p, q)| p.value.equivalent(&q.value));

                    constructor_x.value.equivalent(&constructor_y.value) && equivalent_args
                } else {
                    false
                }
            }
            RecordDestructure(fields_x) => {
                if let RecordDestructure(fields_y) = other {
                    fields_x
                        .iter()
                        .zip(fields_y.iter())
                        .all(|(p, q)| p.value.equivalent(&q.value))
                } else {
                    false
                }
            }
            RequiredField(x, inner_x) => {
                if let RequiredField(y, inner_y) = other {
                    x == y && inner_x.value.equivalent(&inner_y.value)
                } else {
                    false
                }
            }

            // optional record fields can be annotated as:
            //      { x, y } : { x : Int, y ? Bool }
            //      { x, y ? False } = rec
            OptionalField(x, _) => match other {
                Identifier { ident: y } | OptionalField(y, _) => x == y,
                _ => false,
            },
            Identifier { ident: x } => match other {
                Identifier { ident: y } => x == y,
                OptionalField(y, _) => x == y,
                _ => false,
            },
            NumLiteral(x) => {
                if let NumLiteral(y) = other {
                    x == y
                } else {
                    false
                }
            }
            NonBase10Literal {
                string: string_x,
                base: base_x,
                is_negative: is_negative_x,
            } => {
                if let NonBase10Literal {
                    string: string_y,
                    base: base_y,
                    is_negative: is_negative_y,
                } = other
                {
                    string_x == string_y && base_x == base_y && is_negative_x == is_negative_y
                } else {
                    false
                }
            }
            FloatLiteral(x) => {
                if let FloatLiteral(y) = other {
                    x == y
                } else {
                    false
                }
            }
            StrLiteral(x) => {
                if let StrLiteral(y) = other {
                    x == y
                } else {
                    false
                }
            }
            Underscore(x) => {
                if let Underscore(y) = other {
                    x == y
                } else {
                    false
                }
            }
            SpaceBefore(x, _) | SpaceAfter(x, _) => match other {
                SpaceBefore(y, _) | SpaceAfter(y, _) => x.equivalent(y),
                y => x.equivalent(y),
            },
            Malformed(x) => {
                if let Malformed(y) = other {
                    x == y
                } else {
                    false
                }
            }
            QualifiedIdentifier {
                module_name: a,
                ident: x,
            } => {
                if let QualifiedIdentifier {
                    module_name: b,
                    ident: y,
                } = other
                {
                    a == b && x == y
                } else {
                    false
                }
            }
            OpaqueRef(a) => {
                if let OpaqueRef(b) = other {
                    a == b
                } else {
                    false
                }
            }
            SingleQuote(a) => {
                if let SingleQuote(b) = other {
                    a == b
                } else {
                    false
                }
            }
            Tuple(items_x) => {
                if let Tuple(items_y) = other {
                    items_x
                        .iter()
                        .zip(items_y.iter())
                        .all(|(p, q)| p.value.equivalent(&q.value))
                } else {
                    false
                }
            }
            List(items_x) => {
                if let List(items_y) = other {
                    items_x
                        .iter()
                        .zip(items_y.iter())
                        .all(|(p, q)| p.value.equivalent(&q.value))
                } else {
                    false
                }
            }

            ListRest(pattern_as) => match other {
                ListRest(other_pattern_as) => match (pattern_as, other_pattern_as) {
                    (Some((_, a)), Some((_, b))) => a.equivalent(b),
                    _ => false,
                },
                _ => false,
            },

            As(pattern, pattern_as) => match other {
                As(other_pattern, other_pattern_as) => {
                    pattern_as.equivalent(other_pattern_as)
                        && pattern.value.equivalent(&other_pattern.value)
                }
                _ => false,
            },

            MalformedIdent(str_x, _) => {
                if let MalformedIdent(str_y, _) = other {
                    str_x == str_y
                } else {
                    false
                }
            }

            MalformedExpr(_expr_x) => {
                // conservatively assume all malformed expr patterns are not-equivalient
                false
            }
        }
    }
}
#[derive(Copy, Clone)]
pub struct Collection<'a, T> {
    pub items: &'a [T],
    // Use a pointer to a slice (rather than just a slice), in order to avoid bloating
    // Ast variants. The final_comments field is rarely accessed in the hot path, so
    // this shouldn't matter much for perf.
    // Use an Option, so it's possible to initialize without allocating.
    final_comments: Option<&'a &'a [CommentOrNewline<'a>]>,
}

impl<'a, T> Collection<'a, T> {
    pub fn empty() -> Collection<'a, T> {
        Collection {
            items: &[],
            final_comments: None,
        }
    }

    pub const fn with_items(items: &'a [T]) -> Collection<'a, T> {
        Collection {
            items,
            final_comments: None,
        }
    }

    pub fn with_items_and_comments(
        arena: &'a Bump,
        items: &'a [T],
        comments: &'a [CommentOrNewline<'a>],
    ) -> Collection<'a, T> {
        Collection {
            items,
            final_comments: if comments.is_empty() {
                None
            } else {
                Some(arena.alloc(comments))
            },
        }
    }

    pub fn replace_items<V>(&self, new_items: &'a [V]) -> Collection<'a, V> {
        Collection {
            items: new_items,
            final_comments: self.final_comments,
        }
    }

    pub fn ptrify_items(&self, arena: &'a Bump) -> Collection<'a, &'a T> {
        let mut allocated = Vec::with_capacity_in(self.len(), arena);

        for parsed_elem in self.items {
            allocated.push(parsed_elem);
        }

        self.replace_items(allocated.into_bump_slice())
    }

    pub fn map_items<V: 'a>(&self, arena: &'a Bump, f: impl Fn(&'a T) -> V) -> Collection<'a, V> {
        let mut allocated = Vec::with_capacity_in(self.len(), arena);

        for parsed_elem in self.items {
            allocated.push(f(parsed_elem));
        }

        self.replace_items(allocated.into_bump_slice())
    }

    pub fn map_items_result<V: 'a, E>(
        &self,
        arena: &'a Bump,
        f: impl Fn(&T) -> Result<V, E>,
    ) -> Result<Collection<'a, V>, E> {
        let mut allocated = Vec::with_capacity_in(self.len(), arena);

        for parsed_elem in self.items {
            allocated.push(f(parsed_elem)?);
        }

        Ok(self.replace_items(allocated.into_bump_slice()))
    }

    pub fn final_comments(&self) -> &'a [CommentOrNewline<'a>] {
        if let Some(final_comments) = self.final_comments {
            final_comments
        } else {
            &[]
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = &'a T> {
        self.items.iter()
    }

    pub fn len(&self) -> usize {
        self.items.len()
    }

    pub fn is_empty(&self) -> bool {
        self.items.is_empty()
    }
}

impl<'a, T: PartialEq> PartialEq for Collection<'a, T> {
    fn eq(&self, other: &Self) -> bool {
        self.items == other.items && self.final_comments() == other.final_comments()
    }
}

impl<'a, T: Debug> Debug for Collection<'a, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.final_comments().is_empty() {
            f.debug_list().entries(self.items.iter()).finish()
        } else {
            f.debug_struct("Collection")
                .field("items", &self.items)
                .field("final_comments", &self.final_comments())
                .finish()
        }
    }
}

impl<'a, T> Default for Collection<'a, T> {
    fn default() -> Self {
        Self::empty()
    }
}

pub trait Spaceable<'a> {
    fn before(&'a self, _: &'a [CommentOrNewline<'a>]) -> Self;
    fn after(&'a self, _: &'a [CommentOrNewline<'a>]) -> Self;

    fn maybe_before(self, arena: &'a Bump, spaces: &'a [CommentOrNewline<'a>]) -> Self
    where
        Self: Sized + 'a,
    {
        if spaces.is_empty() {
            self
        } else {
            arena.alloc(self).before(spaces)
        }
    }

    fn maybe_after(self, arena: &'a Bump, spaces: &'a [CommentOrNewline<'a>]) -> Self
    where
        Self: Sized + 'a,
    {
        if spaces.is_empty() {
            self
        } else {
            arena.alloc(self).after(spaces)
        }
    }

    fn maybe_around_loc(
        mut me: Loc<Self>,
        arena: &'a Bump,
        before: &'a [CommentOrNewline<'a>],
        after: &'a [CommentOrNewline<'a>],
    ) -> Loc<Self>
    where
        Self: Sized + 'a,
    {
        if !after.is_empty() {
            me.value = arena.alloc(me.value).after(after);
        }

        if !before.is_empty() {
            me.value = arena.alloc(me.value).before(before);
        }

        me
    }

    fn with_spaces_before(&'a self, spaces: &'a [CommentOrNewline<'a>], region: Region) -> Loc<Self>
    where
        Self: Sized,
    {
        Loc {
            region,
            value: self.before(spaces),
        }
    }

    fn with_spaces_after(&'a self, spaces: &'a [CommentOrNewline<'a>], region: Region) -> Loc<Self>
    where
        Self: Sized,
    {
        Loc {
            region,
            value: self.after(spaces),
        }
    }
}

impl<'a, T> Spaceable<'a> for Spaced<'a, T> {
    fn before(&'a self, spaces: &'a [CommentOrNewline<'a>]) -> Self {
        Spaced::SpaceBefore(self, spaces)
    }
    fn after(&'a self, spaces: &'a [CommentOrNewline<'a>]) -> Self {
        Spaced::SpaceAfter(self, spaces)
    }
}

impl<'a> Spaceable<'a> for Expr<'a> {
    fn before(&'a self, spaces: &'a [CommentOrNewline<'a>]) -> Self {
        Expr::SpaceBefore(self, spaces)
    }
    fn after(&'a self, spaces: &'a [CommentOrNewline<'a>]) -> Self {
        Expr::SpaceAfter(self, spaces)
    }
}

impl<'a> Spaceable<'a> for Pattern<'a> {
    fn before(&'a self, spaces: &'a [CommentOrNewline<'a>]) -> Self {
        Pattern::SpaceBefore(self, spaces)
    }
    fn after(&'a self, spaces: &'a [CommentOrNewline<'a>]) -> Self {
        Pattern::SpaceAfter(self, spaces)
    }
}

impl<'a> Spaceable<'a> for TypeAnnotation<'a> {
    fn before(&'a self, spaces: &'a [CommentOrNewline<'a>]) -> Self {
        TypeAnnotation::SpaceBefore(self, spaces)
    }
    fn after(&'a self, spaces: &'a [CommentOrNewline<'a>]) -> Self {
        TypeAnnotation::SpaceAfter(self, spaces)
    }
}

impl<'a, Val> Spaceable<'a> for AssignedField<'a, Val> {
    fn before(&'a self, spaces: &'a [CommentOrNewline<'a>]) -> Self {
        AssignedField::SpaceBefore(self, spaces)
    }
    fn after(&'a self, spaces: &'a [CommentOrNewline<'a>]) -> Self {
        AssignedField::SpaceAfter(self, spaces)
    }
}

impl<'a> Spaceable<'a> for Tag<'a> {
    fn before(&'a self, spaces: &'a [CommentOrNewline<'a>]) -> Self {
        Tag::SpaceBefore(self, spaces)
    }
    fn after(&'a self, spaces: &'a [CommentOrNewline<'a>]) -> Self {
        Tag::SpaceAfter(self, spaces)
    }
}

impl<'a> Spaceable<'a> for Implements<'a> {
    fn before(&'a self, spaces: &'a [CommentOrNewline<'a>]) -> Self {
        Implements::SpaceBefore(self, spaces)
    }
    fn after(&'a self, spaces: &'a [CommentOrNewline<'a>]) -> Self {
        Implements::SpaceAfter(self, spaces)
    }
}

impl<'a> Spaceable<'a> for AbilityImpls<'a> {
    fn before(&'a self, spaces: &'a [CommentOrNewline<'a>]) -> Self {
        AbilityImpls::SpaceBefore(self, spaces)
    }
    fn after(&'a self, spaces: &'a [CommentOrNewline<'a>]) -> Self {
        AbilityImpls::SpaceAfter(self, spaces)
    }
}

impl<'a> Spaceable<'a> for ImplementsAbility<'a> {
    fn before(&'a self, spaces: &'a [CommentOrNewline<'a>]) -> Self {
        ImplementsAbility::SpaceBefore(self, spaces)
    }
    fn after(&'a self, spaces: &'a [CommentOrNewline<'a>]) -> Self {
        ImplementsAbility::SpaceAfter(self, spaces)
    }
}

impl<'a> Expr<'a> {
    pub const REPL_OPAQUE_FUNCTION: Self = Expr::Var {
        module_name: "",
        ident: "<function>",
    };

    pub const REPL_RUNTIME_CRASH: Self = Expr::Var {
        module_name: "",
        ident: "*",
    };

    pub fn loc_ref(&'a self, region: Region) -> Loc<&'a Self> {
        Loc {
            region,
            value: self,
        }
    }

    pub fn loc(self, region: Region) -> Loc<Self> {
        Loc {
            region,
            value: self,
        }
    }

    pub fn is_tag(&self) -> bool {
        matches!(self, Expr::Tag(_))
    }

    pub fn is_opaque(&self) -> bool {
        matches!(self, Expr::OpaqueRef(_))
    }
}

macro_rules! impl_extract_spaces {
    ($t:ident $(< $($generic_args:ident),* >)?) => {

        impl<'a, $($($generic_args: Copy),*)?> ExtractSpaces<'a> for $t<'a, $($($generic_args),*)?> {
            type Item = Self;
            fn extract_spaces(&self) -> Spaces<'a, Self::Item> {
                match self {
                    $t::SpaceBefore(item, before) => {
                        match item {
                            $t::SpaceBefore(_, _) => todo!(),
                            $t::SpaceAfter(item, after) => {
                                Spaces {
                                    before,
                                    item: **item,
                                    after,
                                }
                            }
                            _ => {
                                Spaces {
                                    before,
                                    item: **item,
                                    after: &[],
                                }
                            }
                        }
                    },
                    $t::SpaceAfter(item, after) => {
                        match item {
                            $t::SpaceBefore(item, before) => {
                                Spaces {
                                    before,
                                    item: **item,
                                    after,
                                }
                            }
                            $t::SpaceAfter(_, _) => todo!(),
                            _ => {
                                Spaces {
                                    before: &[],
                                    item: **item,
                                    after,
                                }
                            }
                        }
                    },
                    _ => {
                        Spaces {
                            before: &[],
                            item: *self,
                            after: &[],
                        }
                    }
                }
            }
            fn without_spaces(&self) -> Self::Item {
                match self {
                    $t::SpaceBefore(item, _) => {
                        item.without_spaces()
                    },
                    $t::SpaceAfter(item, _) => {
                        item.without_spaces()
                    },
                    _ => *self,
                }
            }
        }
    };
}

impl_extract_spaces!(Expr);
impl_extract_spaces!(Pattern);
impl_extract_spaces!(Tag);
impl_extract_spaces!(AssignedField<T>);
impl_extract_spaces!(TypeAnnotation);
impl_extract_spaces!(ImplementsAbility);
impl_extract_spaces!(Implements);
impl_extract_spaces!(TypeVar);

impl<'a, T: Copy> ExtractSpaces<'a> for Spaced<'a, T> {
    type Item = T;

    fn extract_spaces(&self) -> Spaces<'a, T> {
        match self {
            Spaced::SpaceBefore(item, before) => match item {
                Spaced::SpaceBefore(_, _) => todo!(),
                Spaced::SpaceAfter(item, after) => {
                    if let Spaced::Item(item) = item {
                        Spaces {
                            before,
                            item: *item,
                            after,
                        }
                    } else {
                        todo!();
                    }
                }
                Spaced::Item(item) => Spaces {
                    before,
                    item: *item,
                    after: &[],
                },
            },
            Spaced::SpaceAfter(item, after) => match item {
                Spaced::SpaceBefore(item, before) => {
                    if let Spaced::Item(item) = item {
                        Spaces {
                            before,
                            item: *item,
                            after,
                        }
                    } else {
                        todo!();
                    }
                }
                Spaced::SpaceAfter(_, _) => todo!(),
                Spaced::Item(item) => Spaces {
                    before: &[],
                    item: *item,
                    after,
                },
            },
            Spaced::Item(item) => Spaces {
                before: &[],
                item: *item,
                after: &[],
            },
        }
    }

    fn without_spaces(&self) -> T {
        match self {
            Spaced::SpaceBefore(item, _) => item.without_spaces(),
            Spaced::SpaceAfter(item, _) => item.without_spaces(),
            Spaced::Item(item) => *item,
        }
    }
}

impl<'a> ExtractSpaces<'a> for AbilityImpls<'a> {
    type Item = Collection<'a, Loc<AssignedField<'a, Expr<'a>>>>;

    fn extract_spaces(&self) -> Spaces<'a, Self::Item> {
        match self {
            AbilityImpls::AbilityImpls(inner) => Spaces {
                before: &[],
                item: *inner,
                after: &[],
            },
            AbilityImpls::SpaceBefore(item, before) => match item {
                AbilityImpls::AbilityImpls(inner) => Spaces {
                    before,
                    item: *inner,
                    after: &[],
                },
                AbilityImpls::SpaceBefore(_, _) => todo!(),
                AbilityImpls::SpaceAfter(AbilityImpls::AbilityImpls(inner), after) => Spaces {
                    before,
                    item: *inner,
                    after,
                },
                AbilityImpls::SpaceAfter(_, _) => todo!(),
            },
            AbilityImpls::SpaceAfter(item, after) => match item {
                AbilityImpls::AbilityImpls(inner) => Spaces {
                    before: &[],
                    item: *inner,
                    after,
                },
                AbilityImpls::SpaceBefore(AbilityImpls::AbilityImpls(inner), before) => Spaces {
                    before,
                    item: *inner,
                    after,
                },
                AbilityImpls::SpaceBefore(_, _) => todo!(),
                AbilityImpls::SpaceAfter(_, _) => todo!(),
            },
        }
    }

    fn without_spaces(&self) -> Self::Item {
        match self {
            AbilityImpls::AbilityImpls(inner) => *inner,
            AbilityImpls::SpaceBefore(item, _) => item.without_spaces(),
            AbilityImpls::SpaceAfter(item, _) => item.without_spaces(),
        }
    }
}

pub trait Malformed {
    /// Returns whether this node is malformed, or contains a malformed node (recursively).
    fn is_malformed(&self) -> bool;
}

impl<'a> Malformed for FullAst<'a> {
    fn is_malformed(&self) -> bool {
        self.header.item.is_malformed() || self.defs.is_malformed()
    }
}

impl<'a> Malformed for Header<'a> {
    fn is_malformed(&self) -> bool {
        match self {
            Header::Module(header) => header.is_malformed(),
            Header::App(header) => header.is_malformed(),
            Header::Package(header) => header.is_malformed(),
            Header::Platform(header) => header.is_malformed(),
            Header::Hosted(header) => header.is_malformed(),
        }
    }
}

impl<'a, T: Malformed> Malformed for Spaces<'a, T> {
    fn is_malformed(&self) -> bool {
        self.item.is_malformed()
    }
}

impl<'a, T: Malformed> Malformed for SpacesBefore<'a, T> {
    fn is_malformed(&self) -> bool {
        self.item.is_malformed()
    }
}

impl<'a> Malformed for Expr<'a> {
    fn is_malformed(&self) -> bool {
        use Expr::*;

        match self {
            Float(_) |
            Num(_) |
            NonBase10Int { .. } |
            AccessorFunction(_) |
            RecordUpdater(_) |
            Var { .. } |
            Underscore(_) |
            Tag(_) |
            OpaqueRef(_) |
            SingleQuote(_) | // This is just a &str - not a bunch of segments
            Crash => false,

            Str(inner) => inner.is_malformed(),

            RecordAccess(inner, _) |
            TupleAccess(inner, _) |
            TrySuffix(inner) => inner.is_malformed(),

            List(items) => items.is_malformed(),

            RecordUpdate { update, fields } => update.is_malformed() || fields.is_malformed(),
            Record(items) => items.is_malformed(),
            Tuple(items) => items.is_malformed(),

            RecordBuilder { mapper: map2, fields } => map2.is_malformed() || fields.is_malformed(),

            Closure(args, body) => args.iter().any(|arg| arg.is_malformed()) || body.is_malformed(),
            Defs(defs, body) => defs.is_malformed() || body.is_malformed(),
            Dbg => false,
            DbgStmt { first, extra_args, continuation, pnc_style: _ } => first.is_malformed() || extra_args.iter().any(|a| a.is_malformed()) || continuation.is_malformed(),
            LowLevelDbg(_, condition, continuation) => condition.is_malformed() || continuation.is_malformed(),
            Try => false,
            LowLevelTry(loc_expr, _) => loc_expr.is_malformed(),
            Return(return_value, after_return) => return_value.is_malformed() || after_return.is_some_and(|ar| ar.is_malformed()),
            Apply(func, args, _) => func.is_malformed() || args.iter().any(|arg| arg.is_malformed()),
            PncApply(func, args) => func.is_malformed() || args.iter().any(|arg| arg.is_malformed()),
            BinOps(firsts, last) => firsts.iter().any(|(expr, _)| expr.is_malformed()) || last.is_malformed(),
            UnaryOp(expr, _) => expr.is_malformed(),
            If { if_thens, final_else, ..} => if_thens.iter().any(|(cond, body)| cond.is_malformed() || body.is_malformed()) || final_else.is_malformed(),
            When(cond, branches) => cond.is_malformed() || branches.iter().any(|branch| branch.is_malformed()),

            SpaceBefore(expr, _) |
            SpaceAfter(expr, _) |
            ParensAround(expr) => expr.is_malformed(),

            MalformedIdent(_, _) |
            PrecedenceConflict(_) |
            EmptyRecordBuilder(_) |
            SingleFieldRecordBuilder(_) |
            OptionalFieldInRecordBuilder(_, _) => true,
        }
    }
}

impl<'a> Malformed for WhenBranch<'a> {
    fn is_malformed(&self) -> bool {
        self.patterns.iter().any(|pat| pat.is_malformed())
            || self.value.is_malformed()
            || self.guard.map(|g| g.is_malformed()).unwrap_or_default()
    }
}

impl<'a, T: Malformed> Malformed for Collection<'a, T> {
    fn is_malformed(&self) -> bool {
        self.iter().any(|item| item.is_malformed())
    }
}

impl<'a> Malformed for StrLiteral<'a> {
    fn is_malformed(&self) -> bool {
        match self {
            StrLiteral::PlainLine(_) => false,
            StrLiteral::Line(segs) => segs.iter().any(|seg| seg.is_malformed()),
            StrLiteral::Block(lines) => lines
                .iter()
                .any(|segs| segs.iter().any(|seg| seg.is_malformed())),
        }
    }
}

impl<'a> Malformed for StrSegment<'a> {
    fn is_malformed(&self) -> bool {
        match self {
            StrSegment::Plaintext(_) | StrSegment::Unicode(_) | StrSegment::EscapedChar(_) => false,
            StrSegment::Interpolated(expr) => expr.is_malformed(),
        }
    }
}

impl<'a, T: Malformed> Malformed for &'a T {
    fn is_malformed(&self) -> bool {
        (*self).is_malformed()
    }
}

impl<T: Malformed> Malformed for Loc<T> {
    fn is_malformed(&self) -> bool {
        self.value.is_malformed()
    }
}

impl<T: Malformed> Malformed for Option<T> {
    fn is_malformed(&self) -> bool {
        self.as_ref()
            .map(|value| value.is_malformed())
            .unwrap_or_default()
    }
}

impl<'a, T: Malformed> Malformed for AssignedField<'a, T> {
    fn is_malformed(&self) -> bool {
        match self {
            AssignedField::RequiredValue(_, _, val)
            | AssignedField::OptionalValue(_, _, val)
            | AssignedField::IgnoredValue(_, _, val) => val.is_malformed(),
            AssignedField::LabelOnly(_) => false,
            AssignedField::SpaceBefore(field, _) | AssignedField::SpaceAfter(field, _) => {
                field.is_malformed()
            }
        }
    }
}

impl<'a> Malformed for Pattern<'a> {
    fn is_malformed(&self) -> bool {
        use Pattern::*;

        match self {
            Identifier{ .. } |
            Tag(_) |
            OpaqueRef(_) => false,
            Apply(func, args) => func.is_malformed() || args.iter().any(|arg| arg.is_malformed()),
            PncApply(func, args) => func.is_malformed() || args.iter().any(|arg| arg.is_malformed()),
            RecordDestructure(items) => items.iter().any(|item| item.is_malformed()),
            RequiredField(_, pat) => pat.is_malformed(),
            OptionalField(_, expr) => expr.is_malformed(),

            NumLiteral(_) |
            NonBase10Literal { .. } |
            Underscore(_) |
            SingleQuote(_) | // This is just a &str - not a bunch of segments
            FloatLiteral(_) => false,

            StrLiteral(lit) => lit.is_malformed(),
            Tuple(items) => items.iter().any(|item| item.is_malformed()),
            List(items) => items.iter().any(|item| item.is_malformed()),
            ListRest(_) =>false,
            As(pat, _) => pat.is_malformed(),
            SpaceBefore(pat, _) |
            SpaceAfter(pat, _) => pat.is_malformed(),

            Malformed(_) |
            MalformedIdent(_, _) |
            MalformedExpr(_) |
            QualifiedIdentifier { .. } => true,
        }
    }
}
impl<'a> Malformed for Defs<'a> {
    fn is_malformed(&self) -> bool {
        self.type_defs.iter().any(|def| def.is_malformed())
            || self.value_defs.iter().any(|def| def.is_malformed())
    }
}

impl<'a> Malformed for TypeDef<'a> {
    fn is_malformed(&self) -> bool {
        match self {
            TypeDef::Alias { header, ann } => header.is_malformed() || ann.is_malformed(),
            TypeDef::Opaque {
                header,
                typ,
                derived,
            } => {
                header.is_malformed()
                    || typ.is_malformed()
                    || derived.map(|d| d.item.is_malformed()).unwrap_or_default()
            }
            TypeDef::Ability {
                header,
                loc_implements,
                members,
            } => {
                header.is_malformed()
                    || loc_implements.is_malformed()
                    || members.iter().any(|member| member.is_malformed())
            }
        }
    }
}

impl<'a> Malformed for AbilityMember<'a> {
    fn is_malformed(&self) -> bool {
        self.typ.is_malformed()
    }
}

impl<'a> Malformed for Implements<'a> {
    fn is_malformed(&self) -> bool {
        match self {
            Implements::Implements => false,
            Implements::SpaceBefore(has, _) | Implements::SpaceAfter(has, _) => has.is_malformed(),
        }
    }
}

impl<'a> Malformed for ImplementsAbility<'a> {
    fn is_malformed(&self) -> bool {
        match self {
            ImplementsAbility::ImplementsAbility { ability, impls } => {
                ability.is_malformed() || impls.iter().any(|impl_| impl_.is_malformed())
            }
            ImplementsAbility::SpaceBefore(has, _) | ImplementsAbility::SpaceAfter(has, _) => {
                has.is_malformed()
            }
        }
    }
}

impl<'a> Malformed for AbilityImpls<'a> {
    fn is_malformed(&self) -> bool {
        match self {
            AbilityImpls::AbilityImpls(impls) => impls.iter().any(|ability| ability.is_malformed()),
            AbilityImpls::SpaceBefore(has, _) | AbilityImpls::SpaceAfter(has, _) => {
                has.is_malformed()
            }
        }
    }
}

impl<'a> Malformed for ValueDef<'a> {
    fn is_malformed(&self) -> bool {
        match self {
            ValueDef::Annotation(pat, annotation) => {
                pat.is_malformed() || annotation.is_malformed()
            }
            ValueDef::Body(pat, expr) => pat.is_malformed() || expr.is_malformed(),
            ValueDef::AnnotatedBody {
                ann_pattern,
                ann_type,
                lines_between: _,
                body_pattern,
                body_expr,
            } => {
                ann_pattern.is_malformed()
                    || ann_type.is_malformed()
                    || body_pattern.is_malformed()
                    || body_expr.is_malformed()
            }
            ValueDef::Dbg {
                condition,
                preceding_comment: _,
            }
            | ValueDef::Expect {
                condition,
                preceding_comment: _,
            } => condition.is_malformed(),
            ValueDef::ModuleImport(ModuleImport {
                before_name: _,
                name: _,
                params,
                alias: _,
                exposed: _,
            }) => params.is_malformed(),
            ValueDef::IngestedFileImport(IngestedFileImport {
                before_path: _,
                path,
                name: _,
                annotation,
            }) => path.is_malformed() || annotation.is_malformed(),
            ValueDef::Stmt(loc_expr) => loc_expr.is_malformed(),
            ValueDef::StmtAfterExpr => false,
        }
    }
}

impl<'a> Malformed for ModuleImportParams<'a> {
    fn is_malformed(&self) -> bool {
        let Self { before: _, params } = self;

        params.is_malformed()
    }
}

impl<'a> Malformed for TypeAnnotation<'a> {
    fn is_malformed(&self) -> bool {
        match self {
            TypeAnnotation::Function(args, _arrow, ret) => {
                args.iter().any(|arg| arg.is_malformed()) || ret.is_malformed()
            }
            TypeAnnotation::Apply(_, _, args) => args.iter().any(|arg| arg.is_malformed()),
            TypeAnnotation::BoundVariable(_)
            | TypeAnnotation::Inferred
            | TypeAnnotation::Wildcard => false,
            TypeAnnotation::As(ty, _, head) => ty.is_malformed() || head.is_malformed(),
            TypeAnnotation::Record { fields, ext } => {
                fields.iter().any(|field| field.is_malformed())
                    || ext.map(|ext| ext.is_malformed()).unwrap_or_default()
            }
            TypeAnnotation::Tuple { elems: fields, ext } => {
                fields.iter().any(|field| field.is_malformed())
                    || ext.map(|ext| ext.is_malformed()).unwrap_or_default()
            }
            TypeAnnotation::TagUnion { ext, tags } => {
                tags.iter().any(|field| field.is_malformed())
                    || ext.map(|ext| ext.is_malformed()).unwrap_or_default()
            }
            TypeAnnotation::Where(ann, clauses) => {
                ann.is_malformed() || clauses.iter().any(|clause| clause.is_malformed())
            }
            TypeAnnotation::SpaceBefore(ty, _) | TypeAnnotation::SpaceAfter(ty, _) => {
                ty.is_malformed()
            }
            TypeAnnotation::Malformed(_) => true,
        }
    }
}

impl<'a> Malformed for TypeHeader<'a> {
    fn is_malformed(&self) -> bool {
        self.vars.iter().any(|var| var.is_malformed())
    }
}

impl<'a> Malformed for TypeVar<'a> {
    fn is_malformed(&self) -> bool {
        match self {
            TypeVar::Identifier(_) => false,
            TypeVar::Malformed(_) => true,
            TypeVar::SpaceBefore(var, _) | TypeVar::SpaceAfter(var, _) => var.is_malformed(),
        }
    }
}

impl<'a> Malformed for Tag<'a> {
    fn is_malformed(&self) -> bool {
        match self {
            Tag::Apply { name: _, args } => args.iter().any(|arg| arg.is_malformed()),
            Tag::SpaceBefore(tag, _) | Tag::SpaceAfter(tag, _) => tag.is_malformed(),
        }
    }
}

impl<'a> Malformed for ImplementsClause<'a> {
    fn is_malformed(&self) -> bool {
        self.abilities.iter().any(|ability| ability.is_malformed())
    }
}

impl<'a, T: Malformed> Malformed for Spaced<'a, T> {
    fn is_malformed(&self) -> bool {
        match self {
            Spaced::Item(t) => t.is_malformed(),
            Spaced::SpaceBefore(t, _) | Spaced::SpaceAfter(t, _) => t.is_malformed(),
        }
    }
}
