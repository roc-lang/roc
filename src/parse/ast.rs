use bumpalo::collections::String;
use bumpalo::collections::Vec;
use bumpalo::Bump;
use operator::Operator;
use parse::ident::{Ident, MaybeQualified};
use region::{Loc, Region};

#[derive(Clone, Debug, PartialEq)]
pub enum Module<'a> {
    Api(&'a [&'a str], &'a str, Vec<'a, Def<'a>>),
    App(&'a [&'a str], &'a str),
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
    /// e.g. `(expr).foo.bar`
    Field(&'a Loc<Expr<'a>>, Vec<'a, &'a str>),
    /// e.g. `Foo.Bar.baz.qux`
    QualifiedField(&'a [&'a str], &'a [&'a str]),
    /// e.g. `.foo`
    AccessorFunction(&'a str),

    // Collection Literals
    List(Vec<'a, Loc<Expr<'a>>>),
    Record(Vec<'a, Loc<Expr<'a>>>),
    AssignField(Loc<&'a str>, &'a Loc<Expr<'a>>),

    // Lookups
    Var(&'a [&'a str], &'a str),
    Variant(&'a [&'a str], &'a str),

    // Pattern Matching
    When(&'a [(Loc<Pattern<'a>>, Loc<Expr<'a>>)]),
    Closure(&'a (Vec<'a, Loc<Pattern<'a>>>, Loc<Expr<'a>>)),
    /// Multiple defs in a row
    Defs(
        &'a (
            Vec<'a, (&'a [CommentOrNewline<'a>], Def<'a>)>,
            Loc<Expr<'a>>,
        ),
    ),

    // Application
    /// To apply by name, do Apply(Var(...), ...)
    /// To apply a variant by name, do Apply(Variant(...), ...)
    Apply(&'a (Loc<Expr<'a>>, Vec<'a, Loc<Expr<'a>>>)),
    Operator(&'a (Loc<Expr<'a>>, Loc<Operator>, Loc<Expr<'a>>)),

    // Conditionals
    If(&'a Loc<Expr<'a>>),
    Then(&'a Loc<Expr<'a>>),
    Else(&'a Loc<Expr<'a>>),
    Case(&'a Loc<Expr<'a>>),

    // Blank Space (e.g. comments, spaces, newlines) before or after an expression.
    // We preserve this for the formatter; canonicalization ignores it.
    SpaceBefore(&'a Expr<'a>, &'a [CommentOrNewline<'a>]),
    SpaceAfter(&'a Expr<'a>, &'a [CommentOrNewline<'a>]),

    // Problems
    MalformedIdent(&'a str),
    MalformedClosure,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Def<'a> {
    AnnotationOnly(Region),
    BodyOnly(Loc<Pattern<'a>>, &'a Loc<Expr<'a>>),
    AnnotatedBody(Loc<Pattern<'a>>, &'a Loc<Expr<'a>>),
}

#[derive(Debug, PartialEq)]
pub enum CommentOrNewline<'a> {
    Newline,
    LineComment(&'a str),
    BlockComment(&'a [&'a str]),
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

    // Literal
    IntLiteral(&'a str),
    FloatLiteral(&'a str),
    StrLiteral(&'a str),
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
    Expression,
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

pub fn format<'a>(arena: &'a Bump, expr: &'a Expr<'a>, indent: u16) -> String<'a> {
    use self::Expr::*;

    let mut buf = String::new_in(arena);

    match expr {
        SpaceBefore(sub_expr, spaces) => {
            buf.push_str(&format_spaces(arena, spaces.iter(), indent));
            buf.push_str(&format(arena, sub_expr, indent));
        }
        SpaceAfter(sub_expr, spaces) => {
            buf.push_str(&format(arena, sub_expr, indent));

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
        Apply((loc_expr, loc_args)) => {
            buf.push_str(&format(arena, &loc_expr.value, indent));

            for loc_arg in loc_args {
                buf.push(' ');

                buf.push_str(&format(arena, &loc_arg.value, indent));
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
        Record(fields) => {
            buf.push('{');

            for _field in fields {
                panic!("TODO implement Display for record fields.");
            }

            buf.push('}');
        }
        Closure((loc_patterns, loc_ret)) => {
            buf.push('\\');

            for loc_pattern in loc_patterns {
                buf.push_str(&format_pattern(arena, &loc_pattern.value, indent, true));

                buf.push(' ');
            }

            buf.push_str("-> ");

            buf.push_str(&format(arena, &loc_ret.value, indent));
        }
        Defs((defs, ret)) => {
            // The first def is actually at the end of the list, because
            // it gets added there with .push() for efficiency. (The order of parsed defs doesn't
            // matter because canonicalization sorts them anyway.) The other
            // defs in the list are in their usual order.
            let (first_spaces, first_def) = defs.last().unwrap_or_else(|| {
                panic!("Tried to format Defs which somehow had an empty list of defs!")
            });
            let other_spaced_defs = &defs[0..defs.len() - 1];

            buf.push_str(&format_spaces(arena, first_spaces.iter(), indent));

            buf.push_str(&format_def(arena, first_def, indent));

            for (spaces, def) in other_spaced_defs.iter() {
                buf.push_str(&format_spaces(arena, spaces.iter(), indent));
                buf.push_str(&format_def(arena, def, indent));
            }

            buf.push_str(&format(arena, &ret.value, indent));
        }
        other => panic!("TODO implement Display for AST variant {:?}", other),
    }

    buf
}

pub fn format_def<'a>(arena: &'a Bump, def: &'a Def<'a>, indent: u16) -> String<'a> {
    use self::Def::*;

    let mut buf = String::new_in(arena);

    match def {
        Def::AnnotationOnly(_region) => panic!("TODO have format_def support AnnotationOnly"),
        BodyOnly(loc_pattern, loc_expr) => {
            buf.push_str(&format_pattern(arena, &loc_pattern.value, indent, true));
            buf.push_str(" = ");
            buf.push_str(&format(arena, &loc_expr.value, indent));
        }
        AnnotatedBody(_loc_pattern, _loc_expr) => {
            panic!("TODO have format_def support AnnotationOnly")
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

        IntLiteral(string) => buf.push_str(string),
        FloatLiteral(string) => buf.push_str(string),
        StrLiteral(string) => buf.push_str(string),
        EmptyRecordLiteral => buf.push_str("{}"),
        Underscore => buf.push('_'),

        // Space
        SpaceBefore(sub_pattern, spaces) => {
            buf.push_str(&format_spaces(arena, spaces.iter(), indent));
            buf.push_str(&format_pattern(arena, sub_pattern, indent, true));
        }
        SpaceAfter(sub_pattern, spaces) => {
            buf.push_str(&format_pattern(arena, sub_pattern, indent, true));
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

fn format_spaces<'a, I>(arena: &'a Bump, spaces: I, _indent: u16) -> String<'a>
where
    I: Iterator<Item = &'a CommentOrNewline<'a>>,
{
    use self::CommentOrNewline::*;

    let mut buf = String::new_in(arena);
    let mut consecutive_newlines = 0;

    for space in spaces {
        match space {
            Newline => {
                // Only ever print two newlines back to back.
                // (Two newlines renders as one blank line.)
                if consecutive_newlines < 2 {
                    buf.push('\n');

                    // Don't bother incrementing it if we're already over the limit.
                    // There's no upside, and it might eventually overflow,
                    consecutive_newlines += 1;
                }
            }
            LineComment(comment) => {
                buf.push('#');
                buf.push_str(comment);
                buf.push('\n');

                // Reset to 1 because we just printed a \n
                consecutive_newlines = 1;
            }
            BlockComment(lines) => {
                buf.push_str("###");

                for line in lines.iter() {
                    buf.push_str(line);
                }

                buf.push_str("###");

                consecutive_newlines = 0;
            }
        }
    }

    buf
}
