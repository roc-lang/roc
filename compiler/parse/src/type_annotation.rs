use crate::ast::{AssignedField, Attempting, CommentOrNewline, Tag, TypeAnnotation};
use crate::blankspace::{space0_around, space0_before, space1, space1_before};
use crate::expr::{global_tag, private_tag};
use crate::ident::join_module_parts;
use crate::keyword;
use crate::parser::{
    allocated, ascii_char, ascii_string, not, optional, peek_utf8_char, unexpected, Either, Fail,
    FailReason, ParseResult, Parser, State,
};
use bumpalo::collections::string::String;
use bumpalo::collections::vec::Vec;
use bumpalo::Bump;
use roc_collections::all::arena_join;
use roc_region::all::{Located, Region};

pub fn located<'a>(min_indent: u16) -> impl Parser<'a, Located<TypeAnnotation<'a>>> {
    expression(min_indent)
}

macro_rules! tag_union {
    ($min_indent:expr) => {
        map!(
            and!(
                collection!(
                    ascii_char('['),
                    loc!(tag_type($min_indent)),
                    ascii_char(','),
                    ascii_char(']'),
                    $min_indent
                ),
                optional(
                    // This could be an open tag union, e.g. `[ Foo, Bar ]a`
                    move |arena, state| allocated(term($min_indent)).parse(arena, state)
                )
            ),
            |(tags, ext): (
                Vec<'a, Located<Tag<'a>>>,
                Option<&'a Located<TypeAnnotation<'a>>>,
            )| TypeAnnotation::TagUnion {
                tags: tags.into_bump_slice(),
                ext,
            }
        )
    };
}

#[allow(clippy::type_complexity)]
pub fn term<'a>(min_indent: u16) -> impl Parser<'a, Located<TypeAnnotation<'a>>> {
    map_with_arena!(
        and!(
            one_of!(
                loc_wildcard(),
                loc_parenthetical_type(min_indent),
                loc!(record_type(min_indent)),
                loc!(tag_union!(min_indent)),
                loc!(applied_type(min_indent)),
                loc!(parse_type_variable)
            ),
            optional(
                // Inline type annotation, e.g. [ Nil, Cons a (List a) ] as List a
                and!(
                    space1(min_indent),
                    skip_first!(
                        ascii_string(keyword::AS),
                        space1_before(term(min_indent), min_indent)
                    )
                )
            )
        ),
        |arena: &'a Bump,
         (loc_ann, opt_as): (
            Located<TypeAnnotation<'a>>,
            Option<(&'a [CommentOrNewline<'a>], Located<TypeAnnotation<'a>>)>
        )| {
            match opt_as {
                Some((spaces, loc_as)) => {
                    let region = Region::span_across(&loc_ann.region, &loc_as.region);
                    let value =
                        TypeAnnotation::As(arena.alloc(loc_ann), spaces, arena.alloc(loc_as));

                    Located { value, region }
                }

                None => loc_ann,
            }
        }
    )
}

/// The `*` type variable, e.g. in (List *) Wildcard,
fn loc_wildcard<'a>() -> impl Parser<'a, Located<TypeAnnotation<'a>>> {
    map!(loc!(ascii_char('*')), |loc_val: Located<()>| {
        loc_val.map(|_| TypeAnnotation::Wildcard)
    })
}

pub fn loc_applied_arg<'a>(min_indent: u16) -> impl Parser<'a, Located<TypeAnnotation<'a>>> {
    skip_first!(
        // Once we hit an "as", stop parsing args
        not(ascii_string(keyword::AS)),
        one_of!(
            loc_wildcard(),
            loc_parenthetical_type(min_indent),
            loc!(record_type(min_indent)),
            loc!(tag_union!(min_indent)),
            loc!(parse_concrete_type),
            loc!(parse_type_variable)
        )
    )
}

#[inline(always)]
fn loc_parenthetical_type<'a>(min_indent: u16) -> impl Parser<'a, Located<TypeAnnotation<'a>>> {
    between!(
        ascii_char('('),
        space0_around(
            move |arena, state| expression(min_indent).parse(arena, state),
            min_indent,
        ),
        ascii_char(')')
    )
}

#[inline(always)]
#[allow(clippy::type_complexity)]
fn tag_type<'a>(min_indent: u16) -> impl Parser<'a, Tag<'a>> {
    map!(
        and!(
            either!(loc!(private_tag()), loc!(global_tag())),
            // Optionally parse space-separated arguments for the constructor,
            // e.g. `ok err` in `Result ok err`
            zero_or_more!(space1_before(
                move |arena, state| loc_applied_arg(min_indent).parse(arena, state),
                min_indent,
            ))
        ),
        |(either_name, args): (
            Either<Located<&'a str>, Located<&'a str>>,
            Vec<'a, Located<TypeAnnotation<'a>>>
        )| match either_name {
            Either::First(name) => Tag::Private {
                name,
                args: args.into_bump_slice()
            },
            Either::Second(name) => Tag::Global {
                name,
                args: args.into_bump_slice()
            },
        }
    )
}

#[inline(always)]
fn record_type<'a>(min_indent: u16) -> impl Parser<'a, TypeAnnotation<'a>> {
    use crate::type_annotation::TypeAnnotation::*;

    map!(
        and!(
            record_without_update!(
                move |arena, state| term(min_indent).parse(arena, state),
                min_indent
            ),
            optional(
                // This could be an open record, e.g. `{ name: Str }r`
                move |arena, state| allocated(term(min_indent)).parse(arena, state)
            )
        ),
        |(fields, ext): (
            Vec<'a, Located<AssignedField<'a, TypeAnnotation<'a>>>>,
            Option<&'a Located<TypeAnnotation<'a>>>,
        )| Record {
            fields: fields.into_bump_slice(),
            ext
        }
    )
}

fn applied_type<'a>(min_indent: u16) -> impl Parser<'a, TypeAnnotation<'a>> {
    map!(
        and!(
            parse_concrete_type,
            // Optionally parse space-separated arguments for the constructor,
            // e.g. `Str Float` in `Map Str Float`
            zero_or_more!(space1_before(
                move |arena, state| loc_applied_arg(min_indent).parse(arena, state),
                min_indent,
            ))
        ),
        |(ctor, args): (TypeAnnotation<'a>, Vec<'a, Located<TypeAnnotation<'a>>>)| {
            match &ctor {
                TypeAnnotation::Apply(ref module_name, ref name, _) => {
                    if args.is_empty() {
                        // ctor is already an Apply with no args, so return it directly.
                        ctor
                    } else {
                        TypeAnnotation::Apply(*module_name, *name, args.into_bump_slice())
                    }
                }
                TypeAnnotation::Malformed(_) => ctor,
                _ => unreachable!(),
            }
        }
    )
}

fn expression<'a>(min_indent: u16) -> impl Parser<'a, Located<TypeAnnotation<'a>>> {
    use crate::blankspace::space0;
    move |arena, state: State<'a>| {
        let (first, state) = space0_before(term(min_indent), min_indent).parse(arena, state)?;
        let (rest, state) = zero_or_more!(skip_first!(
            ascii_char(','),
            space0_around(term(min_indent), min_indent)
        ))
        .parse(arena, state)?;

        // TODO this space0 is dropped, so newlines just before the function arrow when there
        // is only one argument are not seen by the formatter. Can we do better?
        let (is_function, state) =
            optional(skip_first!(space0(min_indent), ascii_string("->"))).parse(arena, state)?;

        if is_function.is_some() {
            let (return_type, state) =
                space0_before(term(min_indent), min_indent).parse(arena, state)?;

            // prepare arguments
            let mut arguments = Vec::with_capacity_in(rest.len() + 1, &arena);
            arguments.push(first);
            arguments.extend(rest);
            let output = arena.alloc(arguments);

            let result = Located {
                region: return_type.region,
                value: TypeAnnotation::Function(output, arena.alloc(return_type)),
            };
            Ok((result, state))
        } else {
            // if there is no function arrow, there cannot be more than 1 "argument"
            if rest.is_empty() {
                Ok((first, state))
            } else {
                // e.g. `Int,Int` without an arrow and return type
                Err((
                    Fail {
                        attempting: state.attempting,
                        reason: FailReason::NotYetImplemented("TODO: Decide the correct error to return for 'Invalid function signature'".to_string()),
                    },
                    state,
                ))
            }
        }
    }
}

/// Parse a basic type annotation that's a combination of variables
/// (which are lowercase and unqualified, e.g. `a` in `List a`),
/// type applications (which are uppercase and optionally qualified, e.g.
/// `Int`, or the `List` in `List a` or the qualified application `Set.Set Float`),
/// and function types like `(a -> b)`.
///
/// Type annotations can also contain records, parentheses, and the `*` character,
/// but this function is not responsible for parsing those.
// Function(&'a [TypeAnnotation<'a>], &'a TypeAnnotation<'a>),

// /// Applying a type to some arguments (e.g. Map.Map String Int)
// Apply(&'a [&'a str], &'a str, &'a [&'a TypeAnnotation<'a>]),

// /// A bound type variable, e.g. `a` in `(a -> a)`
// BoundVariable(&'a str),

fn parse_concrete_type<'a>(
    arena: &'a Bump,
    mut state: State<'a>,
) -> ParseResult<'a, TypeAnnotation<'a>> {
    let mut part_buf = String::new_in(arena); // The current "part" (parts are dot-separated.)
    let mut parts: Vec<&'a str> = Vec::new_in(arena);

    // Qualified types must start with a capitalized letter.
    match peek_utf8_char(&state) {
        Ok((first_letter, bytes_parsed)) => {
            if first_letter.is_alphabetic() && first_letter.is_uppercase() {
                part_buf.push(first_letter);
            } else {
                return Err(unexpected(0, state, Attempting::ConcreteType));
            }

            state = state.advance_without_indenting(bytes_parsed)?;
        }
        Err(reason) => return state.fail(reason),
    }

    let mut next_char = None;

    while !state.bytes.is_empty() {
        match peek_utf8_char(&state) {
            Ok((ch, bytes_parsed)) => {
                // After the first character, only these are allowed:
                //
                // * Unicode alphabetic chars - you might name a variable `鹏` if that's clear to your readers
                // * ASCII digits - e.g. `1` but not `¾`, both of which pass .is_numeric()
                // * A dot ('.')
                if ch.is_alphabetic() {
                    if part_buf.is_empty() && !ch.is_uppercase() {
                        // Each part must begin with a capital letter.
                        return malformed(Some(ch), arena, state, parts);
                    }

                    part_buf.push(ch);
                } else if ch.is_ascii_digit() {
                    // Parts may not start with numbers!
                    if part_buf.is_empty() {
                        return malformed(Some(ch), arena, state, parts);
                    }

                    part_buf.push(ch);
                } else if ch == '.' {
                    // Having two consecutive dots is an error.
                    if part_buf.is_empty() {
                        return malformed(Some(ch), arena, state, parts);
                    }

                    parts.push(part_buf.into_bump_str());

                    // Now that we've recorded the contents of the current buffer, reset it.
                    part_buf = String::new_in(arena);
                } else {
                    // This must be the end of the type. We're done!
                    next_char = Some(ch);

                    break;
                }

                state = state.advance_without_indenting(bytes_parsed)?;
            }
            Err(reason) => return state.fail(reason),
        }
    }

    if part_buf.is_empty() {
        // We probably had a trailing dot, e.g. `Foo.bar.` - this is malformed!
        //
        // This condition might also occur if we encounter a malformed accessor like `.|`
        //
        // If we made it this far and don't have a next_char, then necessarily
        // we have consumed a '.' char previously.
        return malformed(next_char.or_else(|| Some('.')), arena, state, parts);
    }

    if part_buf.is_empty() {
        // We had neither capitalized nor noncapitalized parts,
        // yet we made it this far. The only explanation is that this was
        // a stray '.' drifting through the cosmos.
        return Err(unexpected(1, state, Attempting::Identifier));
    }

    let answer = TypeAnnotation::Apply(
        join_module_parts(arena, parts.into_bump_slice()),
        part_buf.into_bump_str(),
        &[],
    );

    Ok((answer, state))
}

fn parse_type_variable<'a>(
    arena: &'a Bump,
    mut state: State<'a>,
) -> ParseResult<'a, TypeAnnotation<'a>> {
    let mut buf = String::new_in(arena);

    match peek_utf8_char(&state) {
        Ok((first_letter, bytes_parsed)) => {
            // Type variables must start with a lowercase letter.
            if first_letter.is_alphabetic() && first_letter.is_lowercase() {
                buf.push(first_letter);
            } else {
                return Err(unexpected(0, state, Attempting::TypeVariable));
            }

            state = state.advance_without_indenting(bytes_parsed)?;
        }
        Err(reason) => return state.fail(reason),
    }

    while !state.bytes.is_empty() {
        match peek_utf8_char(&state) {
            Ok((ch, bytes_parsed)) => {
                // After the first character, only these are allowed:
                //
                // * Unicode alphabetic chars - you might name a variable `鹏` if that's clear to your readers
                // * ASCII digits - e.g. `1` but not `¾`, both of which pass .is_numeric()
                if ch.is_alphabetic() || ch.is_ascii_digit() {
                    buf.push(ch);
                } else {
                    // This must be the end of the type. We're done!
                    break;
                }

                state = state.advance_without_indenting(bytes_parsed)?;
            }
            Err(reason) => return state.fail(reason),
        }
    }

    let answer = TypeAnnotation::BoundVariable(buf.into_bump_str());

    Ok((answer, state))
}

fn malformed<'a>(
    opt_bad_char: Option<char>,
    arena: &'a Bump,
    mut state: State<'a>,
    parts: Vec<&'a str>,
) -> ParseResult<'a, TypeAnnotation<'a>> {
    // Reconstruct the original string that we've been parsing.
    let mut full_string = String::new_in(arena);

    full_string.push_str(arena_join(arena, &mut parts.into_iter(), ".").into_bump_str());

    if let Some(bad_char) = opt_bad_char {
        full_string.push(bad_char);
    }

    // Consume the remaining chars in the identifier.
    while !state.bytes.is_empty() {
        match peek_utf8_char(&state) {
            Ok((ch, bytes_parsed)) => {
                // We can't use ch.is_alphanumeric() here because that passes for
                // things that are "numeric" but not ASCII digits, like `¾`
                if ch == '.' || ch.is_alphabetic() || ch.is_ascii_digit() {
                    full_string.push(ch);
                } else {
                    break;
                }

                state = state.advance_without_indenting(bytes_parsed)?;
            }
            Err(reason) => return state.fail(reason),
        }
    }

    Ok((
        TypeAnnotation::Malformed(full_string.into_bump_str()),
        state,
    ))
}
