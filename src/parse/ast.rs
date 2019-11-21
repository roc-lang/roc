use bumpalo::collections::String;
use bumpalo::collections::Vec;
use bumpalo::Bump;
use fmt::{self, is_multiline_expr};
use operator::CalledVia;
use operator::{BinOp, UnaryOp};
use parse::ident::Ident;
use region::{Loc, Region};

/// The number of spaces to indent.
const INDENT: u16 = 4;

#[derive(Clone, Debug, PartialEq)]
pub enum Module<'a> {
    Interface {
        header: InterfaceHeader<'a>,
        defs: Vec<'a, Def<'a>>,
    },
    App {
        header: AppHeader<'a>,
        defs: Vec<'a, Def<'a>>,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub struct InterfaceHeader<'a> {
    pub name: Loc<(&'a [&'a str], &'a str)>,
    pub exposes: Vec<'a, Loc<HeaderEntry<'a>>>,
    pub imports: Vec<'a, Loc<HeaderEntry<'a>>>,

    // Potential comments and newlines - these will typically all be empty.
    pub after_interface: &'a [CommentOrNewline<'a>],
    pub before_exposes: &'a [CommentOrNewline<'a>],
    pub after_exposes: &'a [CommentOrNewline<'a>],
    pub before_imports: &'a [CommentOrNewline<'a>],
    pub after_imports: &'a [CommentOrNewline<'a>],
}

#[derive(Clone, Debug, PartialEq)]
pub struct AppHeader<'a> {
    pub imports: Vec<'a, Loc<HeaderEntry<'a>>>,

    // Potential comments and newlines - these will typically all be empty.
    pub after_app: &'a [CommentOrNewline<'a>],
    pub before_imports: &'a [CommentOrNewline<'a>],
    pub after_imports: &'a [CommentOrNewline<'a>],
}

#[derive(Clone, Debug, PartialEq)]
pub enum HeaderEntry<'a> {
    Val(&'a str),
    TypeOnly(&'a str),
    TypeAndVariants(&'a str),
    SpaceBefore(&'a HeaderEntry<'a>, &'a [CommentOrNewline<'a>]),
    SpaceAfter(&'a HeaderEntry<'a>, &'a [CommentOrNewline<'a>]),
}

/// An optional qualifier (the `Foo.Bar` in `Foo.Bar.baz`).
/// If module_parts is empty, this is unqualified.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MaybeQualified<'a, Val> {
    pub module_parts: &'a [&'a str],
    pub value: Val,
}

impl<'a> MaybeQualified<'a, &'a str> {
    pub fn len(&self) -> usize {
        let mut answer = self.value.len();

        for part in self.module_parts {
            answer += part.len();
        }

        answer
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

impl<'a> MaybeQualified<'a, &'a [&'a str]> {
    pub fn len(&self) -> usize {
        let mut answer = 0;

        for module_part in self.module_parts {
            answer += module_part.len();
        }

        for value_part in self.module_parts {
            answer += value_part.len();
        }

        answer
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
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
///
/// Because we need to store references, which each take 2 machine words, the
/// smallest this data structure can be in memory is 3 machine words (the third
/// machine word stores the 1-byte union tag in a memory-aligned way). We have
/// a test verifying that it never accidentally exceeds 3 machine words in size.
#[derive(Clone, Debug, PartialEq)]
pub enum Expr<'a> {
    // Number Literals
    Float(&'a str),
    Int(&'a str),
    HexInt(&'a str),
    OctalInt(&'a str),
    BinaryInt(&'a str),

    // String Literals
    Str(&'a str),
    BlockStr(&'a [&'a str]),
    /// e.g. `(expr).foo.bar` - we rule out nested lookups in canonicalization,
    /// but we want to keep the nesting here to give a nicer error message.
    Field(&'a Loc<Expr<'a>>, Vec<'a, &'a str>),
    /// e.g. `Foo.Bar.baz.qux`
    QualifiedField(&'a [&'a str], &'a [&'a str]),
    /// e.g. `.foo`
    AccessorFunction(&'a str),

    // Collection Literals
    List(Vec<'a, &'a Loc<Expr<'a>>>),
    Record(Vec<'a, Loc<AssignedField<'a, Expr<'a>>>>),

    // Lookups
    Var(&'a [&'a str], &'a str),
    Variant(&'a [&'a str], &'a str),

    // Pattern Matching
    Closure(&'a Vec<'a, Loc<Pattern<'a>>>, &'a Loc<Expr<'a>>),
    /// Multiple defs in a row
    Defs(Vec<'a, &'a Loc<Def<'a>>>, &'a Loc<Expr<'a>>),

    // Application
    /// To apply by name, do Apply(Var(...), ...)
    /// To apply a variant by name, do Apply(Variant(...), ...)
    Apply(&'a Loc<Expr<'a>>, Vec<'a, &'a Loc<Expr<'a>>>, CalledVia),
    BinOp(&'a (Loc<Expr<'a>>, Loc<BinOp>, Loc<Expr<'a>>)),
    UnaryOp(&'a Loc<Expr<'a>>, Loc<UnaryOp>),

    // Conditionals
    If(&'a (Loc<Expr<'a>>, Loc<Expr<'a>>, Loc<Expr<'a>>)),
    Case(
        &'a Loc<Expr<'a>>,
        Vec<'a, &'a (Loc<Pattern<'a>>, Loc<Expr<'a>>)>,
    ),

    // Blank Space (e.g. comments, spaces, newlines) before or after an expression.
    // We preserve this for the formatter; canonicalization ignores it.
    SpaceBefore(&'a Expr<'a>, &'a [CommentOrNewline<'a>]),
    SpaceAfter(&'a Expr<'a>, &'a [CommentOrNewline<'a>]),

    // Problems
    MalformedIdent(&'a str),
    MalformedClosure,
    // Both operators were non-associative, e.g. (True == False == False).
    // We should tell the author to disambiguate by grouping them with parens.
    PrecedenceConflict(Loc<BinOp>, Loc<BinOp>, &'a Loc<Expr<'a>>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Def<'a> {
    // TODO in canonicalization, validate the pattern; only certain patterns
    // are allowed in annotations.
    Annotation(Loc<Pattern<'a>>, Loc<TypeAnnotation<'a>>),
    // TODO in canonicalization, check to see if there are any newlines after the
    // annotation; if not, and if it's followed by a Body, then the annotation
    // applies to that expr! (TODO: verify that the pattern for both annotation and body match.)
    // No need to track that relationship in any data structure.
    Body(Loc<Pattern<'a>>, &'a Loc<Expr<'a>>),
    // TODO also in canonicalization, if there is a CustomType or TypeAlias
    // inside an Expr, give an error like "hey you need to move this to the
    // top level" - it'll parse fine, we just won't accept it there.
    CustomType(Loc<TypeAnnotation<'a>>, Vec<'a, Loc<TypeAnnotation<'a>>>),
    TypeAlias(Loc<TypeAnnotation<'a>>, Loc<TypeAnnotation<'a>>),

    // Blank Space (e.g. comments, spaces, newlines) before or after a def.
    // We preserve this for the formatter; canonicalization ignores it.
    SpaceBefore(&'a Def<'a>, &'a [CommentOrNewline<'a>]),
    SpaceAfter(&'a Def<'a>, &'a [CommentOrNewline<'a>]),
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeAnnotation<'a> {
    /// A function. The types of its arguments, then the type of its return value.
    Function(&'a [TypeAnnotation<'a>], &'a TypeAnnotation<'a>),

    /// Applying a type to some arguments (e.g. Map.Map String Int)
    Apply(&'a [&'a str], &'a str, &'a [Loc<TypeAnnotation<'a>>]),

    /// A bound type variable, e.g. `a` in `(a -> a)`
    BoundVariable(&'a str),

    /// A plain record, e.g. `{ name: String, email: Email }`
    Record(Vec<'a, Loc<AssignedField<'a, TypeAnnotation<'a>>>>),

    /// A record fragment, e.g. `{ name: String, email: Email }...r`
    RecordFragment(
        Vec<'a, Loc<AssignedField<'a, TypeAnnotation<'a>>>>,
        // the fragment type variable, e.g. the `r` in `{ name: String }...r`
        &'a Loc<TypeAnnotation<'a>>,
    ),

    /// The `*` type variable, e.g. in (List *)
    Wildcard,

    // We preserve this for the formatter; canonicalization ignores it.
    SpaceBefore(&'a TypeAnnotation<'a>, &'a [CommentOrNewline<'a>]),
    SpaceAfter(&'a TypeAnnotation<'a>, &'a [CommentOrNewline<'a>]),

    /// A malformed type annotation, which will code gen to a runtime error
    Malformed(&'a str),
}

#[derive(Debug, Clone, PartialEq)]
pub enum AssignedField<'a, Val> {
    // Both a label and a value, e.g. `{ name: "blah" }`
    LabeledValue(Loc<&'a str>, &'a [CommentOrNewline<'a>], &'a Loc<Val>),

    // A label with no value, e.g. `{ name }` (this is sugar for { name: name })
    LabelOnly(Loc<&'a str>),

    // We preserve this for the formatter; canonicalization ignores it.
    SpaceBefore(&'a AssignedField<'a, Val>, &'a [CommentOrNewline<'a>]),
    SpaceAfter(&'a AssignedField<'a, Val>, &'a [CommentOrNewline<'a>]),

    /// A malformed assigned field, which will code gen to a runtime error
    Malformed(&'a str),
}

#[derive(Debug, PartialEq)]
pub enum CommentOrNewline<'a> {
    Newline,
    LineComment(&'a str),
}

impl<'a> CommentOrNewline<'a> {
    pub fn contains_newline(&self) -> bool {
        use self::CommentOrNewline::*;

        match self {
            // Line comments have an implicit newline at the end
            Newline | LineComment(_) => true,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Pattern<'a> {
    // Identifier
    Identifier(&'a str),

    // Variant, optionally qualified
    Variant(&'a [&'a str], &'a str),
    Apply(&'a Loc<Pattern<'a>>, &'a [Loc<Pattern<'a>>]),
    /// This is Loc<Pattern> rather than Loc<str> so we can record comments
    /// around the destructured names, e.g. { x ### x does stuff ###, y }
    /// In practice, these patterns will always be Identifier
    RecordDestructure(Vec<'a, Loc<Pattern<'a>>>),
    /// A field pattern, e.g. { x: Just 0 } -> ...
    /// can only occur inside of a RecordDestructure
    RecordField(&'a str, &'a Loc<Pattern<'a>>),

    // Literal
    IntLiteral(&'a str),
    HexIntLiteral(&'a str),
    OctalIntLiteral(&'a str),
    BinaryIntLiteral(&'a str),
    FloatLiteral(&'a str),
    StrLiteral(&'a str),
    BlockStrLiteral(&'a [&'a str]),
    EmptyRecordLiteral,
    Underscore,

    // Space
    SpaceBefore(&'a Pattern<'a>, &'a [CommentOrNewline<'a>]),
    SpaceAfter(&'a Pattern<'a>, &'a [CommentOrNewline<'a>]),

    // Malformed
    Malformed(&'a str),
    QualifiedIdentifier(MaybeQualified<'a, &'a str>),
}

impl<'a> Pattern<'a> {
    pub fn from_ident(arena: &'a Bump, ident: Ident<'a>) -> Pattern<'a> {
        match ident {
            Ident::Var(maybe_qualified) => {
                if maybe_qualified.module_parts.is_empty() {
                    Pattern::Identifier(maybe_qualified.value)
                } else {
                    Pattern::Variant(maybe_qualified.module_parts, maybe_qualified.value)
                }
            }
            Ident::Variant(maybe_qualified) => {
                Pattern::Variant(maybe_qualified.module_parts, maybe_qualified.value)
            }
            Ident::Field(maybe_qualified) => {
                let mut buf = String::with_capacity_in(
                    maybe_qualified.module_parts.len() + maybe_qualified.value.len(),
                    arena,
                );

                for part in maybe_qualified.module_parts.iter() {
                    buf.push_str(part);
                    buf.push('.');
                }

                let mut iter = maybe_qualified.value.iter().peekable();

                while let Some(part) = iter.next() {
                    buf.push_str(part);

                    // If there are more fields to come, add a "."
                    if iter.peek().is_some() {
                        buf.push('.');
                    }
                }

                Pattern::Malformed(buf.into_bump_str())
            }
            Ident::AccessorFunction(string) => Pattern::Malformed(string),
            Ident::Malformed(string) => Pattern::Malformed(string),
        }
    }
}

pub trait Spaceable<'a> {
    fn before(&'a self, &'a [CommentOrNewline<'a>]) -> Self;
    fn after(&'a self, &'a [CommentOrNewline<'a>]) -> Self;

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

impl<'a> Spaceable<'a> for HeaderEntry<'a> {
    fn before(&'a self, spaces: &'a [CommentOrNewline<'a>]) -> Self {
        HeaderEntry::SpaceBefore(self, spaces)
    }
    fn after(&'a self, spaces: &'a [CommentOrNewline<'a>]) -> Self {
        HeaderEntry::SpaceAfter(self, spaces)
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

impl<'a> Spaceable<'a> for Def<'a> {
    fn before(&'a self, spaces: &'a [CommentOrNewline<'a>]) -> Self {
        Def::SpaceBefore(self, spaces)
    }
    fn after(&'a self, spaces: &'a [CommentOrNewline<'a>]) -> Self {
        Def::SpaceAfter(self, spaces)
    }
}

#[test]
fn expr_size() {
    // The size of the Expr data structure should be exactly 6 machine words.
    // This test helps avoid regressions wich accidentally increase its size!
    assert_eq!(
        std::mem::size_of::<Expr>(),
        // TODO [move this comment to an issue] We should be able to get this
        // down to 2, which would mean we could fit 4 of these nodes in a single
        // 64-byte cache line instead of only being able to fit 1.
        //
        // Doing this would require, among other things:
        // 1. Making a str replacement where the length is stored as u32 instead of usize,
        //    to leave room for the tagged union's u8 tag.
        //    (Alternatively could store it as (&'a &'a str), but ew.)
        // 2. Similarly, making a slice replacement like that str replacement, and
        //    also where it doesn't share the bytes with anything else - so its
        //    elements can be consumed without having to clone them (unlike a slice).
        //    That's the only reason we're using Vec right now instead of slices -
        //    if we used slices, we'd have to clone their elements during canonicalization
        //    just to iterate over them and canonicalize them normally.
        // 3. Figuring out why (&'a (Foo, Bar)) by default takes up 24 bytes in Rust.
        //    I assume it's because the struct is being stored inline instead of
        //    as a pointer, but in this case we actually do want the pointer!
        //    We want to have the lifetime and we want to avoid using the unsafe keyword,
        //    but we also want this to only store 1 pointer in the AST node.
        //    Hopefully there's a way!
        //
        // It's also possible that 4 machine words might yield better performance
        // than 2, due to more data structures being inlinable, and therefore
        // having fewer pointers to chase. This seems worth investigating as well.
        std::mem::size_of::<usize>() * 6
    );
}

#[test]
fn pattern_size() {
    // The size of the Pattern data structure should be exactly 4 machine words.
    // This test helps avoid regressions wich accidentally increase its size!
    assert_eq!(
        std::mem::size_of::<Pattern>(),
        // TODO [move this comment to an issue] We should be able to get this
        // down to 2, which would mean we could fit 4 of these nodes in a single
        // 64-byte cache line instead of only being able to fit 2.
        //
        // Doing this would require, among other things:
        // 1. Making a str replacement where the length is stored as u32 instead of usize,
        //    to leave room for the tagged union's u8 tag.
        //    (Alternatively could store it as (&'a &'a str), but ew.)
        // 2. Figuring out why &'a (Foo, Bar) by default takes up 24 bytes in Rust.
        //    I assume it's because the struct is being stored inline instead of
        //    as a pointer, but in this case we actually do want the pointer!
        //    We want to have the lifetime and we want to avoid using the unsafe keyword,
        //    but we also want this to only store 1 pointer in the AST node.
        //    Hopefully there's a way!
        //
        // It's also possible that going up to 4 machine words might yield even
        // better performance, due to more data structures being inlinable,
        // and therefore having fewer pointers to chase. This seems worth
        // investigating as well.
        std::mem::size_of::<usize>() * 5
    );
}

/// What we're currently attempting to parse, e.g.
/// "currently attempting to parse a list." This helps error messages!
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Attempting {
    List,
    Keyword,
    StringLiteral,
    RecordLiteral,
    RecordFieldLabel,
    InterpolatedString,
    NumberLiteral,
    UnicodeEscape,
    ClosureParams,
    ClosureBody,
    Def,
    Module,
    Record,
    Identifier,
    ConcreteType,
    TypeVariable,
    CaseCondition,
    CaseBranch,
}

impl<'a> Expr<'a> {
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
}

pub fn format<'a>(
    arena: &'a Bump,
    expr: &'a Expr<'a>,
    indent: u16,
    apply_needs_parens: bool,
) -> String<'a> {
    use self::Expr::*;

    let mut buf = String::new_in(arena);

    match expr {
        SpaceBefore(sub_expr, spaces) => {
            buf.push_str(&format_spaces(arena, spaces.iter(), indent));
            buf.push_str(&format(arena, sub_expr, indent, apply_needs_parens));
        }
        SpaceAfter(sub_expr, spaces) => {
            buf.push_str(&format(arena, sub_expr, indent, apply_needs_parens));
            buf.push_str(&format_spaces(arena, spaces.iter(), indent));
        }
        Str(string) => {
            buf.push('"');
            buf.push_str(string);
            buf.push('"');
        }
        Var(module_parts, name) => {
            for part in module_parts.iter() {
                buf.push_str(part);
                buf.push('.');
            }

            buf.push_str(name);
        }
        Apply(loc_expr, loc_args, _) => {
            if apply_needs_parens {
                buf.push('(');
            }

            buf.push_str(&format(arena, &loc_expr.value, indent, true));

            for loc_arg in loc_args {
                buf.push(' ');

                buf.push_str(&format(arena, &loc_arg.value, indent, true));
            }

            if apply_needs_parens {
                buf.push(')');
            }
        }
        BlockStr(lines) => {
            buf.push_str("\"\"\"");
            for line in lines.iter() {
                buf.push_str(line);
            }
            buf.push_str("\"\"\"");
        }
        Int(string) => buf.push_str(string),
        Float(string) => buf.push_str(string),
        HexInt(string) => {
            buf.push('0');
            buf.push('x');
            buf.push_str(string);
        }
        BinaryInt(string) => {
            buf.push('0');
            buf.push('b');
            buf.push_str(string);
        }
        OctalInt(string) => {
            buf.push('0');
            buf.push('o');
            buf.push_str(string);
        }
        Record(loc_fields) => {
            buf.push('{');

            let is_multiline = loc_fields
                .iter()
                .any(|loc_field| fmt::is_multiline_field(&loc_field.value));

            let mut iter = loc_fields.iter().peekable();
            let field_indent = if is_multiline {
                indent + INDENT
            } else {
                if !loc_fields.is_empty() {
                    buf.push(' ');
                }

                indent
            };

            while let Some(field) = iter.next() {
                buf.push_str(&format_field(
                    arena,
                    &field.value,
                    is_multiline,
                    field_indent,
                    apply_needs_parens,
                ));

                if let Some(_) = iter.peek() {
                    buf.push(',');

                    if !is_multiline {
                        buf.push(' ');
                    }
                }
            }

            if is_multiline {
                buf.push('\n');
            } else if !loc_fields.is_empty() {
                buf.push(' ');
            }

            buf.push('}');
        }
        Closure(loc_patterns, loc_ret) => {
            buf.push('\\');

            for loc_pattern in loc_patterns.iter() {
                buf.push_str(&format_pattern(arena, &loc_pattern.value, indent, true));

                buf.push(' ');
            }

            let is_multiline = is_multiline_expr(&loc_ret.value);

            // If the body is multiline, go down a line and indent.
            let indent = if is_multiline {
                indent + INDENT
            } else {
                indent
            };

            buf.push_str("->");

            let newline_is_next = match &loc_ret.value {
                SpaceBefore(_, _) => true,
                _ => false,
            };

            if !newline_is_next {
                // Push a space after the "->" preceding this.
                buf.push(' ');
            }

            buf.push_str(&format(arena, &loc_ret.value, indent, false));
        }
        Defs(defs, ret) => {
            // The first def is actually at the end of the list, because
            // it gets added there with .push() for efficiency. (The order of parsed defs doesn't
            // matter because canonicalization sorts them anyway.) The other
            // defs in the list are in their usual order.
            let loc_first_def = defs.last().unwrap_or_else(|| {
                panic!("Tried to format Defs which somehow had an empty list of defs!")
            });
            let other_spaced_defs = &defs[0..defs.len() - 1];

            buf.push_str(&format_def(arena, &loc_first_def.value, indent));

            for loc_def in other_spaced_defs.iter() {
                buf.push_str(&format_def(arena, &loc_def.value, indent));
            }

            buf.push_str(&format(arena, &ret.value, indent, false));
        }
        If((loc_condition, loc_then, loc_else)) => {
            buf.push_str("if ");
            buf.push_str(&format(arena, &loc_condition.value, indent, false));
            buf.push_str(" then ");
            buf.push_str(&format(arena, &loc_then.value, indent, false));
            buf.push_str(" else ");
            buf.push_str(&format(arena, &loc_else.value, indent, false));
        }
        Case(loc_condition, branches) => {
            buf.push_str("case ");
            buf.push_str(&format(arena, &loc_condition.value, indent, false));
            buf.push_str(" when\n");

            let mut it = branches.iter().peekable();
            while let Some((pattern, expr)) = it.next() {
                add_spaces(&mut buf, indent + INDENT);

                match pattern.value {
                    Pattern::SpaceBefore(nested, spaces) => {
                        buf.push_str(&format_comments_only(arena, spaces.iter(), indent + INDENT));
                        buf.push_str(&format_pattern(arena, nested, indent + INDENT, false));
                    }
                    _ => {
                        buf.push_str(&format_pattern(
                            arena,
                            &pattern.value,
                            indent + INDENT,
                            false,
                        ));
                    }
                }

                buf.push_str(" ->\n");

                add_spaces(&mut buf, indent + (INDENT * 2));
                match expr.value {
                    Expr::SpaceBefore(nested, spaces) => {
                        buf.push_str(&format_comments_only(
                            arena,
                            spaces.iter(),
                            indent + (INDENT * 2),
                        ));
                        buf.push_str(&format(arena, &nested, indent + (INDENT * 2), false));
                    }
                    _ => {
                        buf.push_str(&format(arena, &expr.value, indent + (INDENT * 2), false));
                    }
                }

                if let Some(_) = it.peek() {
                    buf.push('\n');
                    buf.push('\n');
                }
            }
        }
        other => panic!("TODO implement Display for AST variant {:?}", other),
    }

    buf
}

pub fn format_def<'a>(arena: &'a Bump, def: &'a Def<'a>, indent: u16) -> String<'a> {
    let mut buf = String::new_in(arena);

    match def {
        Def::Annotation(_, _) => panic!("TODO have format_def support Annotation"),
        Def::Body(loc_pattern, loc_expr) => {
            buf.push_str(&format_pattern(arena, &loc_pattern.value, indent, true));
            buf.push_str(" = ");
            buf.push_str(&format(arena, &loc_expr.value, indent, false));
        }
        Def::CustomType(_, _) => panic!("TODO have format_def support CustomType"),
        Def::TypeAlias(_, _) => panic!("TODO have format_def support TypeAlias"),
        Def::SpaceBefore(sub_def, spaces) => {
            buf.push_str(&format_spaces(arena, spaces.iter(), indent));
            buf.push_str(&format_def(arena, sub_def, indent));
        }
        Def::SpaceAfter(sub_def, spaces) => {
            buf.push_str(&format_def(arena, sub_def, indent));

            buf.push_str(&format_spaces(arena, spaces.iter(), indent));
        }
    }

    buf
}

fn format_pattern<'a>(
    arena: &'a Bump,
    pattern: &'a Pattern<'a>,
    indent: u16,
    apply_needs_parens: bool,
) -> String<'a> {
    use self::Pattern::*;

    let mut buf = String::new_in(arena);

    match pattern {
        Identifier(string) => buf.push_str(string),
        Variant(module_parts, name) => {
            for part in module_parts.iter() {
                buf.push_str(part);
                buf.push('.');
            }

            buf.push_str(name);
        }
        Apply(loc_pattern, loc_arg_patterns) => {
            if apply_needs_parens {
                buf.push('(');
            }

            buf.push_str(&format_pattern(arena, &loc_pattern.value, indent, true));

            for loc_arg in loc_arg_patterns.iter() {
                buf.push(' ');
                buf.push_str(&format_pattern(arena, &loc_arg.value, indent, true));
            }

            if apply_needs_parens {
                buf.push(')');
            }
        }
        RecordDestructure(loc_patterns) => {
            buf.push_str("{ ");

            let mut is_first = true;

            for loc_pattern in loc_patterns {
                if is_first {
                    is_first = false;
                } else {
                    buf.push_str(", ");
                }

                buf.push_str(&format_pattern(arena, &loc_pattern.value, indent, true));
            }

            buf.push_str(" }");
        }

        RecordField(name, loc_pattern) => {
            buf.push_str(name);
            buf.push_str(": ");
            buf.push_str(&format_pattern(arena, &loc_pattern.value, indent, true));
        }

        IntLiteral(string) => buf.push_str(string),
        HexIntLiteral(string) => buf.push_str(string),
        OctalIntLiteral(string) => buf.push_str(string),
        BinaryIntLiteral(string) => buf.push_str(string),
        FloatLiteral(string) => buf.push_str(string),
        StrLiteral(string) => buf.push_str(string),
        BlockStrLiteral(lines) => {
            for line in *lines {
                buf.push_str(line)
            }
        }
        EmptyRecordLiteral => buf.push_str("{}"),
        Underscore => buf.push('_'),

        // Space
        SpaceBefore(sub_pattern, spaces) => {
            buf.push_str(&format_spaces(arena, spaces.iter(), indent));
            buf.push_str(&format_pattern(
                arena,
                sub_pattern,
                indent,
                apply_needs_parens,
            ));
        }
        SpaceAfter(sub_pattern, spaces) => {
            buf.push_str(&format_pattern(
                arena,
                sub_pattern,
                indent,
                apply_needs_parens,
            ));
            buf.push_str(&format_spaces(arena, spaces.iter(), indent));
        }

        // Malformed
        Malformed(string) => buf.push_str(string),
        QualifiedIdentifier(maybe_qualified) => {
            for part in maybe_qualified.module_parts.iter() {
                buf.push_str(part);
                buf.push('.');
            }

            buf.push_str(maybe_qualified.value);
        }
    }

    buf
}

fn format_spaces<'a, I>(arena: &'a Bump, spaces: I, indent: u16) -> String<'a>
where
    I: Iterator<Item = &'a CommentOrNewline<'a>>,
{
    use self::CommentOrNewline::*;

    let mut buf = String::new_in(arena);
    let mut consecutive_newlines = 0;
    let mut iter = spaces.peekable();

    while let Some(space) = iter.next() {
        match space {
            Newline => {
                // Only ever print two newlines back to back.
                // (Two newlines renders as one blank line.)
                if consecutive_newlines < 2 {
                    if iter.peek() == Some(&&Newline) {
                        buf.push('\n');
                    } else {
                        newline(&mut buf, indent);
                    }

                    // Don't bother incrementing it if we're already over the limit.
                    // There's no upside, and it might eventually overflow,
                    consecutive_newlines += 1;
                }
            }
            LineComment(comment) => {
                buf.push('#');
                buf.push_str(comment);

                newline(&mut buf, indent);

                // Reset to 1 because we just printed a \n
                consecutive_newlines = 1;
            }
        }
    }

    buf
}

/// Like format_spaces, but remove newlines and keep only comments.
fn format_comments_only<'a, I>(arena: &'a Bump, spaces: I, indent: u16) -> String<'a>
where
    I: Iterator<Item = &'a CommentOrNewline<'a>>,
{
    use self::CommentOrNewline::*;

    let mut buf = String::new_in(arena);

    for space in spaces {
        match space {
            Newline => {}
            LineComment(comment) => {
                buf.push('#');
                buf.push_str(comment);

                newline(&mut buf, indent);
            }
        }
    }

    buf
}

pub fn format_field<'a>(
    arena: &'a Bump,
    assigned_field: &'a AssignedField<'a, Expr<'a>>,
    is_multiline: bool,
    indent: u16,
    apply_needs_parens: bool,
) -> String<'a> {
    use self::AssignedField::*;

    let mut buf = String::new_in(arena);

    match assigned_field {
        LabeledValue(name, spaces, value) => {
            if is_multiline {
                newline(&mut buf, indent);
            }

            buf.push_str(name.value);

            if !spaces.is_empty() {
                buf.push_str(&format_spaces(arena, spaces.iter(), indent));
            }

            buf.push(':');
            buf.push(' ');
            buf.push_str(&format(arena, &value.value, indent, apply_needs_parens));
        }
        LabelOnly(name) => {
            if is_multiline {
                newline(&mut buf, indent);
            }

            buf.push_str(name.value);
        }
        AssignedField::SpaceBefore(sub_expr, spaces) => {
            buf.push_str(&format_comments_only(arena, spaces.iter(), indent));
            buf.push_str(&format_field(
                arena,
                sub_expr,
                is_multiline,
                indent,
                apply_needs_parens,
            ));
        }
        AssignedField::SpaceAfter(sub_expr, spaces) => {
            buf.push_str(&format_field(
                arena,
                sub_expr,
                is_multiline,
                indent,
                apply_needs_parens,
            ));
            buf.push_str(&format_comments_only(arena, spaces.iter(), indent));
        }
        Malformed(string) => buf.push_str(string),
    }

    buf
}

fn newline<'a>(buf: &mut String<'a>, indent: u16) {
    buf.push('\n');

    add_spaces(buf, indent);
}

fn add_spaces<'a>(buf: &mut String<'a>, spaces: u16) {
    for _ in 0..spaces {
        buf.push(' ');
    }
}
