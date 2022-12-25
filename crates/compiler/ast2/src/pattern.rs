use bumpalo::{collections::String, Bump};
use roc_region::all::Loc;

use crate::{Base, Collection, CommentOrNewline, EIdent, Expr, Ident, StrLiteral, TypeAnnotation};

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Tag<'a> {
    Apply {
        name: Loc<&'a str>,
        args: &'a [Loc<TypeAnnotation<'a>>],
    },

    // We preserve this for the formatter; canonicalization ignores it.
    SpaceBefore(&'a Tag<'a>, &'a [CommentOrNewline<'a>]),
    SpaceAfter(&'a Tag<'a>, &'a [CommentOrNewline<'a>]),

    /// A malformed tag, which will code gen to a runtime error
    Malformed(&'a str),
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Pattern<'a> {
    // Identifier
    Identifier(&'a str),

    Tag(&'a str),

    OpaqueRef(&'a str),

    Apply(&'a Loc<Pattern<'a>>, &'a [Loc<Pattern<'a>>]),

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
    Underscore(&'a str),
    SingleQuote(&'a str),

    /// A tuple pattern, e.g. (Just x, 1)
    Tuple(Collection<'a, Loc<Pattern<'a>>>),

    /// A list pattern like [_, x, ..]
    List(Collection<'a, Loc<Pattern<'a>>>),

    /// A list-rest pattern ".."
    /// Can only occur inside of a [Pattern::List]
    ListRest,

    // Space
    SpaceBefore(&'a Pattern<'a>, &'a [CommentOrNewline<'a>]),
    SpaceAfter(&'a Pattern<'a>, &'a [CommentOrNewline<'a>]),

    // Malformed
    Malformed(&'a str),
    MalformedIdent(&'a str, EIdent),
    QualifiedIdentifier {
        module_name: &'a str,
        ident: &'a str,
    },
}

impl<'a> Pattern<'a> {
    pub fn from_ident(arena: &'a Bump, ident: Ident<'a>) -> Pattern<'a> {
        match ident {
            Ident::Tag(string) => Pattern::Tag(string),
            Ident::OpaqueRef(string) => Pattern::OpaqueRef(string),
            Ident::Access { module_name, parts } => {
                if parts.len() == 1 {
                    // This is valid iff there is no module.
                    let ident = parts.iter().next().unwrap();

                    if module_name.is_empty() {
                        Pattern::Identifier(ident)
                    } else {
                        Pattern::QualifiedIdentifier { module_name, ident }
                    }
                } else {
                    // This is definitely malformed.
                    let mut buf =
                        String::with_capacity_in(module_name.len() + (2 * parts.len()), arena);
                    let mut any_parts_printed = if module_name.is_empty() {
                        false
                    } else {
                        buf.push_str(module_name);

                        true
                    };

                    for part in parts.iter() {
                        if any_parts_printed {
                            buf.push('.');
                        } else {
                            any_parts_printed = true;
                        }

                        buf.push_str(part);
                    }

                    Pattern::Malformed(buf.into_bump_str())
                }
            }
            Ident::RecordAccessorFunction(string) => Pattern::Malformed(string),
            Ident::TupleAccessorFunction(string) => Pattern::Malformed(string),
            Ident::Malformed(string, _problem) => Pattern::Malformed(string),
        }
    }

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
                Identifier(y) | OptionalField(y, _) => x == y,
                _ => false,
            },
            Identifier(x) => match other {
                Identifier(y) | OptionalField(y, _) => x == y,
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
            ListRest => matches!(other, ListRest),
            MalformedIdent(str_x, _) => {
                if let MalformedIdent(str_y, _) = other {
                    str_x == str_y
                } else {
                    false
                }
            }
        }
    }
}
