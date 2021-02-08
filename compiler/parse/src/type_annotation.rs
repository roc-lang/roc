use crate::ast::{AssignedField, Attempting, CommentOrNewline, Tag, TypeAnnotation};
use crate::blankspace::{space0_around, space0_before, space1, space1_before};
use crate::expr::{global_tag, private_tag};
use crate::ident::join_module_parts;
use crate::keyword;
use crate::parser::{
    allocated, ascii_char, ascii_string, chomp_and_check_indent, not, optional, peek_utf8_char,
    specialize, unexpected, word1, BadInputError, Either, ParseResult, Parser,
    Progress::{self, *},
    State, SyntaxError, TRecord, Type,
};
use bumpalo::collections::string::String;
use bumpalo::collections::vec::Vec;
use bumpalo::Bump;
use roc_collections::all::arena_join;
use roc_region::all::{Located, Region};

pub fn located<'a>(
    min_indent: u16,
) -> impl Parser<'a, Located<TypeAnnotation<'a>>, SyntaxError<'a>> {
    expression(min_indent)
}

macro_rules! tag_union {
    ($min_indent:expr) => {
        map!(
            and!(
                collection_trailing_sep!(
                    ascii_char(b'['),
                    loc!(tag_type($min_indent)),
                    ascii_char(b','),
                    ascii_char(b']'),
                    $min_indent
                ),
                optional(
                    // This could be an open tag union, e.g. `[ Foo, Bar ]a`
                    move |arena: &'a Bump, state: State<'a>| allocated(term($min_indent))
                        .parse(arena, state)
                )
            ),
            |((tags, final_comments), ext): (
                (Vec<'a, Located<Tag<'a>>>, &'a [CommentOrNewline<'a>]),
                Option<&'a Located<TypeAnnotation<'a>>>,
            )| TypeAnnotation::TagUnion {
                tags: tags.into_bump_slice(),
                ext,
                final_comments
            }
        )
    };
}

#[allow(clippy::type_complexity)]
pub fn term<'a>(min_indent: u16) -> impl Parser<'a, Located<TypeAnnotation<'a>>, SyntaxError<'a>> {
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
            |a, s| {
                optional(
                    // Inline type annotation, e.g. [ Nil, Cons a (List a) ] as List a
                    and!(
                        space1(min_indent),
                        skip_first!(
                            crate::parser::keyword(keyword::AS, min_indent),
                            space1_before(term(min_indent), min_indent)
                        )
                    ),
                )
                .parse(a, s)
            }
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
fn loc_wildcard<'a>() -> impl Parser<'a, Located<TypeAnnotation<'a>>, SyntaxError<'a>> {
    map!(loc!(ascii_char(b'*')), |loc_val: Located<()>| {
        loc_val.map(|_| TypeAnnotation::Wildcard)
    })
}

fn loc_applied_arg<'a>(
    min_indent: u16,
) -> impl Parser<'a, Located<TypeAnnotation<'a>>, SyntaxError<'a>> {
    skip_first!(
        // Once we hit an "as", stop parsing args
        // and roll back parsing of preceding spaces
        not(and!(
            space1(min_indent),
            crate::parser::keyword(keyword::AS, min_indent)
        )),
        space1_before(
            one_of!(
                loc_wildcard(),
                loc_parenthetical_type(min_indent),
                loc!(record_type(min_indent)),
                loc!(tag_union!(min_indent)),
                loc!(parse_concrete_type),
                loc!(parse_type_variable)
            ),
            min_indent
        )
    )
}

fn loc_applied_args<'a>(
    min_indent: u16,
) -> impl Parser<'a, Vec<'a, Located<TypeAnnotation<'a>>>, SyntaxError<'a>> {
    zero_or_more!(loc_applied_arg(min_indent))
}

#[inline(always)]
fn loc_parenthetical_type<'a>(
    min_indent: u16,
) -> impl Parser<'a, Located<TypeAnnotation<'a>>, SyntaxError<'a>> {
    between!(
        ascii_char(b'('),
        space0_around(
            move |arena, state| expression(min_indent).parse(arena, state),
            min_indent,
        ),
        ascii_char(b')')
    )
}

#[inline(always)]
#[allow(clippy::type_complexity)]
fn tag_type<'a>(min_indent: u16) -> impl Parser<'a, Tag<'a>, SyntaxError<'a>> {
    map!(
        and!(
            either!(loc!(private_tag()), loc!(global_tag())),
            // Optionally parse space-separated arguments for the constructor,
            // e.g. `ok err` in `Result ok err`
            loc_applied_args(min_indent)
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

// #[macro_export]
// macro_rules! zero_or_more {
//     ($parser:expr) => {
//         move |arena, state: State<'a>| {
//             use bumpalo::collections::Vec;
//
//             let start_bytes_len = state.bytes.len();
//
//             match $parser.parse(arena, state) {
//                 Ok((_, first_output, next_state)) => {
//                     let mut state = next_state;
//                     let mut buf = Vec::with_capacity_in(1, arena);
//
//                     buf.push(first_output);
//
//                     loop {
//                         match $parser.parse(arena, state) {
//                             Ok((_, next_output, next_state)) => {
//                                 state = next_state;
//                                 buf.push(next_output);
//                             }
//                             Err((fail_progress, fail, old_state)) => {
//                                 match fail_progress {
//                                     MadeProgress => {
//                                         // made progress on an element and then failed; that's an error
//                                         return Err((MadeProgress, fail, old_state));
//                                     }
//                                     NoProgress => {
//                                         // the next element failed with no progress
//                                         // report whether we made progress before
//                                         let progress = Progress::from_lengths(start_bytes_len, old_state.bytes.len());
//                                         return Ok((progress, buf, old_state));
//                                     }
//                                 }
//                             }
//                         }
//                     }
//                 }
//                 Err((fail_progress, fail, new_state)) => {
//                     match fail_progress {
//                         MadeProgress => {
//                             // made progress on an element and then failed; that's an error
//                             Err((MadeProgress, fail, new_state))
//                         }
//                         NoProgress => {
//                             // the first element failed (with no progress), but that's OK
//                             // because we only need to parse 0 elements
//                             Ok((NoProgress, Vec::new_in(arena), new_state))
//                         }
//                     }
//                 }
//             }
//         }
//     };
// }

#[macro_export]
macro_rules! record_type_field {
    ($val_parser:expr, $min_indent:expr) => {
        move |arena: &'a bumpalo::Bump,
              state: $crate::parser::State<'a>|
              -> $crate::parser::ParseResult<'a, $crate::ast::AssignedField<'a, _>, TRecord<'a>> {
            use $crate::ast::AssignedField::*;
            use $crate::blankspace::{space0_before_e, space0_e};
            use $crate::ident::lowercase_ident;
            use $crate::parser::Either::*;

            // You must have a field name, e.g. "email"
            let row = state.line;
            let col = state.column;
            let (progress, loc_label, state) = loc!(lowercase_ident()).parse(arena, state).map_err(|(p, _, s)| (p, TRecord::Field(row, col), s))?;
            debug_assert_eq!(progress, MadeProgress);

            let (_, spaces, state) =
                space0_e($min_indent, TRecord::Space, TRecord::IndentEnd).parse(arena, state)?;


            // Having a value is optional; both `{ email }` and `{ email: blah }` work.
            // (This is true in both literals and types.)
            let (_, opt_loc_val, state) = $crate::parser::optional(either!(
                skip_first!(word1(b':', TRecord::Colon), space0_before_e($val_parser, $min_indent, TRecord::Space, TRecord::IndentColon)),
                skip_first!(word1(b'?', TRecord::Optional), space0_before_e($val_parser, $min_indent, TRecord::Space, TRecord::IndentOptional))
            ))
            .parse(arena, state)?;

            let answer = match opt_loc_val {
                Some(either) => match either {
                    First(loc_val) => RequiredValue(loc_label, spaces, arena.alloc(loc_val)),
                    Second(loc_val) => OptionalValue(loc_label, spaces, arena.alloc(loc_val)),
                },
                // If no value was provided, record it as a Var.
                // Canonicalize will know what to do with a Var later.
                None => {
                    if !spaces.is_empty() {
                        SpaceAfter(arena.alloc(LabelOnly(loc_label)), spaces)
                    } else {
                        LabelOnly(loc_label)
                    }
                }
            };

            Ok((MadeProgress, answer, state))
        }
    };
}

macro_rules! collection_trailing_sep_e {
    ($opening_brace:expr, $elem:expr, $delimiter:expr, $closing_brace:expr, $min_indent:expr, $space_problem:expr, $indent_problem:expr) => {
        skip_first!(
            $opening_brace,
            skip_first!(
                // We specifically allow space characters inside here, so that
                // `[  ]` can be successfully parsed as an empty list, and then
                // changed by the formatter back into `[]`.
                //
                // We don't allow newlines or comments in the middle of empty
                // roc_collections because those are normally stored in an Expr,
                // and there's no Expr in which to store them in an empty collection!
                //
                // We could change the AST to add extra storage specifically to
                // support empty literals containing newlines or comments, but this
                // does not seem worth even the tiniest regression in compiler performance.
                zero_or_more!($crate::parser::word1(b' ', |row, col| TRecord::Space(
                    BadInputError::LineTooLong,
                    row,
                    col
                ))),
                skip_second!(
                    and!(
                        $crate::parser::trailing_sep_by0(
                            $delimiter,
                            $crate::blankspace::space0_around_e(
                                $elem,
                                $min_indent,
                                $space_problem,
                                $indent_problem
                            )
                        ),
                        $crate::blankspace::space0_e($min_indent, $space_problem, $indent_problem)
                    ),
                    $closing_brace
                )
            )
        )
    };
}

#[inline(always)]
fn record_type<'a>(min_indent: u16) -> impl Parser<'a, TypeAnnotation<'a>, SyntaxError<'a>> {
    let f = |x, row, col| SyntaxError::Type(Type::TRecord(x, row, col));
    specialize(f, record_type_internal(min_indent))
}

#[inline(always)]
fn record_type_internal<'a>(min_indent: u16) -> impl Parser<'a, TypeAnnotation<'a>, TRecord<'a>> {
    use crate::type_annotation::TypeAnnotation::*;

    let field_term = move |a, s| match term(min_indent).parse(a, s) {
        Ok(t) => Ok(t),
        Err((p, error, s)) => Err((p, TRecord::Syntax(a.alloc(error), s.line, s.column), s)),
    };

    move |arena, state| {
        let (_, (fields, final_comments), state) = collection_trailing_sep_e!(
            word1(b'{', TRecord::Open),
            loc!(record_type_field!(field_term, min_indent)),
            word1(b',', TRecord::End),
            word1(b'}', TRecord::End),
            min_indent,
            TRecord::Space,
            TRecord::IndentEnd
        )
        .parse(arena, state)?;

        let (_, ext, state) = optional(allocated(field_term)).parse(arena, state)?;

        let result = Record {
            fields: fields.into_bump_slice(),
            ext,
            final_comments,
        };

        Ok((MadeProgress, result, state))
    }
}

fn applied_type<'a>(min_indent: u16) -> impl Parser<'a, TypeAnnotation<'a>, SyntaxError<'a>> {
    map!(
        and!(
            parse_concrete_type,
            // Optionally parse space-separated arguments for the constructor,
            // e.g. `Str Float` in `Map Str Float`
            loc_applied_args(min_indent)
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

fn expression<'a>(
    min_indent: u16,
) -> impl Parser<'a, Located<TypeAnnotation<'a>>, SyntaxError<'a>> {
    use crate::blankspace::space0;
    move |arena, state: State<'a>| {
        let (p1, first, state) = space0_before(term(min_indent), min_indent).parse(arena, state)?;
        let (p2, rest, state) = zero_or_more!(skip_first!(
            ascii_char(b','),
            space0_around(term(min_indent), min_indent)
        ))
        .parse(arena, state)?;

        // TODO this space0 is dropped, so newlines just before the function arrow when there
        // is only one argument are not seen by the formatter. Can we do better?
        let (p3, is_function, state) =
            optional(skip_first!(space0(min_indent), ascii_string("->"))).parse(arena, state)?;

        if is_function.is_some() {
            let (p4, return_type, state) =
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
            let progress = p1.or(p2).or(p3).or(p4);
            Ok((progress, result, state))
        } else {
            let progress = p1.or(p2).or(p3);
            // if there is no function arrow, there cannot be more than 1 "argument"
            if rest.is_empty() {
                Ok((progress, first, state))
            } else {
                // e.g. `Int,Int` without an arrow and return type
                let msg =
                    "TODO: Decide the correct error to return for 'Invalid function signature'"
                        .to_string();
                Err((progress, SyntaxError::NotYetImplemented(msg), state))
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
) -> ParseResult<'a, TypeAnnotation<'a>, SyntaxError<'a>> {
    let mut part_buf = String::new_in(arena); // The current "part" (parts are dot-separated.)
    let mut parts: Vec<&'a str> = Vec::new_in(arena);

    let start_bytes_len = state.bytes.len();

    // Qualified types must start with a capitalized letter.
    match peek_utf8_char(&state) {
        Ok((first_letter, bytes_parsed)) => {
            if first_letter.is_alphabetic() && first_letter.is_uppercase() {
                part_buf.push(first_letter);
            } else {
                return Err(unexpected(arena, 0, Attempting::ConcreteType, state));
            }

            state = state.advance_without_indenting(arena, bytes_parsed)?;
        }
        Err(reason) => return state.fail(arena, NoProgress, reason),
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

                state = state.advance_without_indenting(arena, bytes_parsed)?;
            }
            Err(reason) => {
                let progress = Progress::from_lengths(start_bytes_len, state.bytes.len());

                return state.fail(arena, progress, reason);
            }
        }
    }

    if part_buf.is_empty() {
        // We probably had a trailing dot, e.g. `Foo.bar.` - this is malformed!
        //
        // This condition might also occur if we encounter a malformed accessor like `.|`
        //
        // If we made it this far and don't have a next_char, then necessarily
        // we have consumed a '.' char previously.
        return malformed(next_char.or(Some('.')), arena, state, parts);
    }

    if part_buf.is_empty() {
        // We had neither capitalized nor noncapitalized parts,
        // yet we made it this far. The only explanation is that this was
        // a stray '.' drifting through the cosmos.
        return Err(unexpected(arena, 1, Attempting::Identifier, state));
    }

    let answer = TypeAnnotation::Apply(
        join_module_parts(arena, parts.into_bump_slice()),
        part_buf.into_bump_str(),
        &[],
    );

    let progress = Progress::from_lengths(start_bytes_len, state.bytes.len());
    Ok((progress, answer, state))
}

fn parse_type_variable<'a>(
    arena: &'a Bump,
    mut state: State<'a>,
) -> ParseResult<'a, TypeAnnotation<'a>, SyntaxError<'a>> {
    let mut buf = String::new_in(arena);

    let start_bytes_len = state.bytes.len();

    match peek_utf8_char(&state) {
        Ok((first_letter, bytes_parsed)) => {
            // Type variables must start with a lowercase letter.
            if first_letter.is_alphabetic() && first_letter.is_lowercase() {
                buf.push(first_letter);
            } else {
                return Err(unexpected(arena, 0, Attempting::TypeVariable, state));
            }

            state = state.advance_without_indenting(arena, bytes_parsed)?;
        }
        Err(reason) => {
            let progress = Progress::from_lengths(start_bytes_len, state.bytes.len());
            return state.fail(arena, progress, reason);
        }
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

                state = state.advance_without_indenting(arena, bytes_parsed)?;
            }
            Err(reason) => {
                let progress = Progress::from_lengths(start_bytes_len, state.bytes.len());
                return state.fail(arena, progress, reason);
            }
        }
    }

    let answer = TypeAnnotation::BoundVariable(buf.into_bump_str());

    let progress = Progress::from_lengths(start_bytes_len, state.bytes.len());
    Ok((progress, answer, state))
}

fn malformed<'a>(
    opt_bad_char: Option<char>,
    arena: &'a Bump,
    mut state: State<'a>,
    parts: Vec<&'a str>,
) -> ParseResult<'a, TypeAnnotation<'a>, SyntaxError<'a>> {
    // assumption: progress was made to conclude that the annotation is malformed

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

                state = state.advance_without_indenting(arena, bytes_parsed)?;
            }
            Err(reason) => return state.fail(arena, MadeProgress, reason),
        }
    }

    Ok((
        MadeProgress,
        TypeAnnotation::Malformed(full_string.into_bump_str()),
        state,
    ))
}

// type Field = ();
//
// fn chomp_record_end<'a>(
//     mut fields: Vec<'a, Field>,
// ) -> impl Parser<'a, Vec<'a, Field>, TRecord<'a>> {
//     one_of_with_error!(TRecord::End;
//     | arena, state | {
//         let (_,_,state) = word1(b',', TRecord::End).parse(arena, state)?;
//         let (_, _spaces,state) = chomp_and_check_indent(TRecord::Space, TRecord::IndentField).parse(arena, state)?;
//         let (_, field,state) = chomp_field().parse(arena, state)?;
//         fields.push(field);
//
//         chomp_record_end(fields)
//     },
//     | arena, state | {
//         let (_,_,state) = word1(b'}', TRecord::End).parse(arena, state)?;
//
//         Ok((MadeProgress, fields, state ))
//     }
//     )
// }
//
// fn chomp_field<'a>() -> impl Parser<'a, Field, TRecord<'a>> {
//     todo!()
// }
