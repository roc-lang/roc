use crate::ast::{AssignedField, CommentOrNewline, Tag, TypeAnnotation};
use crate::blankspace::{
    space0, space0_around, space0_around_e, space0_before, space0_before_e, space0_e,
};
use crate::expr::{global_tag, private_tag};
use crate::ident::join_module_parts;
use crate::keyword;
use crate::parser::{
    allocated, ascii_char, ascii_string, backtrackable, not_e, optional, peek_utf8_char_e,
    specialize, specialize_ref, word1, BadInputError, Col, Either, ParseResult, Parser,
    Progress::{self, *},
    Row, State, SyntaxError, TApply, TInParens, TRecord, TTagUnion, TVariable, Type,
};
use bumpalo::collections::string::String;
use bumpalo::collections::vec::Vec;
use bumpalo::Bump;
use roc_region::all::{Located, Region};

pub fn located<'a>(
    min_indent: u16,
) -> impl Parser<'a, Located<TypeAnnotation<'a>>, SyntaxError<'a>> {
    expression(min_indent)
}

#[inline(always)]
fn tag_union_type<'a>(min_indent: u16) -> impl Parser<'a, TypeAnnotation<'a>, TTagUnion<'a>> {
    move |arena, state| {
        let (_, (tags, final_comments), state) = collection_trailing_sep_e!(
            word1(b'[', TTagUnion::Open),
            loc!(tag_type(min_indent)),
            word1(b',', TTagUnion::End),
            word1(b']', TTagUnion::End),
            min_indent,
            TTagUnion::Open,
            TTagUnion::Space,
            TTagUnion::IndentEnd
        )
        .parse(arena, state)?;

        // This could be an open tag union, e.g. `[ Foo, Bar ]a`
        let (_, ext, state) = optional(allocated(specialize_ref(
            TTagUnion::Syntax,
            term(min_indent),
        )))
        .parse(arena, state)?;

        let result = TypeAnnotation::TagUnion {
            tags: tags.into_bump_slice(),
            ext,
            final_comments,
        };

        Ok((MadeProgress, result, state))
    }
}

fn check_indent<'a, TE, E>(min_indent: u16, to_problem: TE) -> impl Parser<'a, (), E>
where
    TE: Fn(Row, Col) -> E,
    E: 'a,
{
    move |_arena, state: State<'a>| {
        if state.indent_col < min_indent {
            Err((NoProgress, to_problem(state.line, state.column), state))
        } else {
            Ok((NoProgress, (), state))
        }
    }
}

#[allow(clippy::type_complexity)]
fn term<'a>(min_indent: u16) -> impl Parser<'a, Located<TypeAnnotation<'a>>, SyntaxError<'a>> {
    specialize(|x, _, _| SyntaxError::Type(x), term_help(min_indent))
}

#[allow(clippy::type_complexity)]
fn term_help<'a>(min_indent: u16) -> impl Parser<'a, Located<TypeAnnotation<'a>>, Type<'a>> {
    map_with_arena!(
        and!(
            one_of!(
                loc_wildcard(),
                specialize(Type::TInParens, loc_type_in_parens(min_indent)),
                loc!(specialize(Type::TRecord, record_type(min_indent))),
                loc!(specialize(Type::TTagUnion, tag_union_type(min_indent))),
                loc!(applied_type(min_indent)),
                loc!(specialize(Type::TVariable, parse_type_variable))
            ),
            // Inline alias notation, e.g. [ Nil, Cons a (List a) ] as List a
            one_of![
                map!(
                    and!(
                        skip_second!(
                            backtrackable(space0_e(min_indent, Type::TSpace, Type::TIndentEnd)),
                            crate::parser::keyword_e(keyword::AS, Type::TEnd(0, 0))
                        ),
                        space0_before_e(
                            term_help(min_indent),
                            min_indent,
                            Type::TSpace,
                            Type::TAsIndentStart
                        )
                    ),
                    Some
                ),
                |_, state| Ok((NoProgress, None, state))
            ]
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
fn loc_wildcard<'a>() -> impl Parser<'a, Located<TypeAnnotation<'a>>, Type<'a>> {
    map!(loc!(word1(b'*', Type::TWildcard)), |loc_val: Located<()>| {
        loc_val.map(|_| TypeAnnotation::Wildcard)
    })
}

fn loc_applied_arg<'a>(
    min_indent: u16,
) -> impl Parser<'a, Located<TypeAnnotation<'a>>, SyntaxError<'a>> {
    specialize(
        |x, _, _| SyntaxError::Type(x),
        loc_applied_arg_help(min_indent),
    )
}

fn loc_applied_arg_help<'a>(
    min_indent: u16,
) -> impl Parser<'a, Located<TypeAnnotation<'a>>, Type<'a>> {
    use crate::ast::Spaceable;

    map_with_arena!(
        and!(
            backtrackable(space0_e(min_indent, Type::TSpace, Type::TIndentStart)),
            skip_first!(
                // Once we hit an "as", stop parsing args
                // and roll back parsing of preceding spaces
                debug!(not_e(
                    debug!(crate::parser::keyword(keyword::AS, min_indent)),
                    Type::TStart
                )),
                one_of!(
                    loc_wildcard(),
                    specialize(Type::TInParens, loc_type_in_parens(min_indent)),
                    loc!(specialize(Type::TRecord, record_type(min_indent))),
                    loc!(specialize(Type::TTagUnion, tag_union_type(min_indent))),
                    loc!(specialize(Type::TApply, parse_concrete_type)),
                    loc!(specialize(Type::TVariable, parse_type_variable))
                )
            )
        ),
        |arena: &'a Bump, (spaces, argument): (_, Located<TypeAnnotation<'a>>)| {
            let Located { region, value } = argument;
            arena.alloc(value).with_spaces_before(spaces, region)
        }
    )
}

fn loc_applied_args<'a>(
    min_indent: u16,
) -> impl Parser<'a, Vec<'a, Located<TypeAnnotation<'a>>>, SyntaxError<'a>> {
    zero_or_more!(loc_applied_arg(min_indent))
}

fn loc_type_in_parens<'a>(
    min_indent: u16,
) -> impl Parser<'a, Located<TypeAnnotation<'a>>, TInParens<'a>> {
    // TODO what if the middle parser returns EOF?
    between!(
        word1(b'(', TInParens::Open),
        space0_around_e(
            move |arena, state| specialize_ref(TInParens::Syntax, expression(min_indent))
                .parse(arena, state),
            min_indent,
            TInParens::Space,
            TInParens::IndentEnd,
        ),
        word1(b')', TInParens::End)
    )
}

#[inline(always)]
fn tag_type<'a>(min_indent: u16) -> impl Parser<'a, Tag<'a>, TTagUnion<'a>> {
    move |arena, state: State<'a>| {
        let (_, either_name, state) = specialize_ref(
            TTagUnion::Syntax,
            either!(loc!(private_tag()), loc!(global_tag())),
        )
        .parse(arena, state)?;

        let (_, args, state) =
            specialize_ref(TTagUnion::Syntax, loc_applied_args(min_indent)).parse(arena, state)?;

        let result = match either_name {
            Either::First(name) => Tag::Private {
                name,
                args: args.into_bump_slice(),
            },
            Either::Second(name) => Tag::Global {
                name,
                args: args.into_bump_slice(),
            },
        };

        Ok((MadeProgress, result, state))
    }
}

fn record_type_field<'a>(
    min_indent: u16,
) -> impl Parser<'a, AssignedField<'a, TypeAnnotation<'a>>, TRecord<'a>> {
    use crate::ident::lowercase_ident;
    use crate::parser::Either::*;
    use AssignedField::*;

    move |arena, state: State<'a>| {
        // You must have a field name, e.g. "email"
        // using the initial row/col is important for error reporting
        let row = state.line;
        let col = state.column;
        let (progress, loc_label, state) = loc!(specialize(
            move |_, _, _| TRecord::Field(row, col),
            lowercase_ident()
        ))
        .parse(arena, state)?;
        debug_assert_eq!(progress, MadeProgress);

        let (_, spaces, state) =
            space0_e(min_indent, TRecord::Space, TRecord::IndentEnd).parse(arena, state)?;

        // Having a value is optional; both `{ email }` and `{ email: blah }` work.
        // (This is true in both literals and types.)
        let (_, opt_loc_val, state) = optional(either!(
            word1(b':', TRecord::Colon),
            word1(b'?', TRecord::Optional)
        ))
        .parse(arena, state)?;

        let val_parser = specialize_ref(TRecord::Syntax, term(min_indent));

        match opt_loc_val {
            Some(First(_)) => {
                let (_, loc_val, state) =
                    space0_before_e(val_parser, min_indent, TRecord::Space, TRecord::IndentColon)
                        .parse(arena, state)?;

                Ok((
                    MadeProgress,
                    RequiredValue(loc_label, spaces, arena.alloc(loc_val)),
                    state,
                ))
            }
            Some(Second(_)) => {
                let (_, loc_val, state) = space0_before_e(
                    val_parser,
                    min_indent,
                    TRecord::Space,
                    TRecord::IndentOptional,
                )
                .parse(arena, state)?;

                Ok((
                    MadeProgress,
                    OptionalValue(loc_label, spaces, arena.alloc(loc_val)),
                    state,
                ))
            }
            // If no value was provided, record it as a Var.
            // Canonicalize will know what to do with a Var later.
            None => {
                let value = if !spaces.is_empty() {
                    SpaceAfter(arena.alloc(LabelOnly(loc_label)), spaces)
                } else {
                    LabelOnly(loc_label)
                };

                Ok((MadeProgress, value, state))
            }
        }
    }
}

#[inline(always)]
fn record_type<'a>(min_indent: u16) -> impl Parser<'a, TypeAnnotation<'a>, TRecord<'a>> {
    use crate::type_annotation::TypeAnnotation::*;

    let field_term = move |a, s| match term(min_indent).parse(a, s) {
        Ok(t) => Ok(t),
        Err((p, error, s)) => Err((p, TRecord::Syntax(a.alloc(error), s.line, s.column), s)),
    };

    move |arena, state| {
        let (_, (fields, final_comments), state) = collection_trailing_sep_e!(
            word1(b'{', TRecord::Open),
            loc!(record_type_field(min_indent)),
            word1(b',', TRecord::End),
            word1(b'}', TRecord::End),
            min_indent,
            TRecord::Open,
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

fn applied_type<'a>(min_indent: u16) -> impl Parser<'a, TypeAnnotation<'a>, Type<'a>> {
    map!(
        and!(
            specialize(Type::TApply, parse_concrete_type),
            // Optionally parse space-separated arguments for the constructor,
            // e.g. `Str Float` in `Map Str Float`
            loc_applied_args_e(min_indent)
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

fn loc_applied_args_e<'a>(
    min_indent: u16,
) -> impl Parser<'a, Vec<'a, Located<TypeAnnotation<'a>>>, Type<'a>> {
    zero_or_more!(loc_applied_arg_help(min_indent))
}

fn expression<'a>(
    min_indent: u16,
) -> impl Parser<'a, Located<TypeAnnotation<'a>>, SyntaxError<'a>> {
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
) -> ParseResult<'a, TypeAnnotation<'a>, TApply> {
    let mut part_buf = String::new_in(arena); // The current "part" (parts are dot-separated.)
    let mut parts: Vec<&'a str> = Vec::new_in(arena);

    // Qualified types must start with a capitalized letter.
    match peek_utf8_char_e(&state, TApply::StartNotUppercase, TApply::Space) {
        Ok((first_letter, bytes_parsed)) => {
            if first_letter.is_alphabetic() && first_letter.is_uppercase() {
                part_buf.push(first_letter);
            } else {
                let problem = TApply::StartNotUppercase(state.line, state.column + 1);
                return Err((NoProgress, problem, state));
            }

            state = state.advance_without_indenting_e(arena, bytes_parsed, TApply::Space)?;
        }
        Err(reason) => return Err((NoProgress, reason, state)),
    }

    while !state.bytes.is_empty() {
        match peek_utf8_char_e(&state, TApply::End, TApply::Space) {
            Ok((ch, bytes_parsed)) => {
                // After the first character, only these are allowed:
                //
                // * Unicode alphabetic chars - you might name a variable `鹏` if that's clear to your readers
                // * ASCII digits - e.g. `1` but not `¾`, both of which pass .is_numeric()
                // * A dot ('.')
                if ch.is_alphabetic() {
                    if part_buf.is_empty() && !ch.is_uppercase() {
                        // Each part must begin with a capital letter.
                        return Err((
                            MadeProgress,
                            TApply::StartNotUppercase(state.line, state.column),
                            state,
                        ));
                    }

                    part_buf.push(ch);
                } else if ch.is_ascii_digit() {
                    // Parts may not start with numbers!
                    if part_buf.is_empty() {
                        return Err((
                            MadeProgress,
                            TApply::StartIsNumber(state.line, state.column),
                            state,
                        ));
                    }

                    part_buf.push(ch);
                } else if ch == '.' {
                    // Having two consecutive dots is an error.
                    if part_buf.is_empty() {
                        return Err((
                            MadeProgress,
                            TApply::DoubleDot(state.line, state.column),
                            state,
                        ));
                    }

                    parts.push(part_buf.into_bump_str());

                    // Now that we've recorded the contents of the current buffer, reset it.
                    part_buf = String::new_in(arena);
                } else {
                    // This must be the end of the type. We're done!
                    break;
                }

                state = state.advance_without_indenting_e(arena, bytes_parsed, TApply::Space)?;
            }
            Err(reason) => {
                return Err((MadeProgress, reason, state));
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
        return Err((
            MadeProgress,
            TApply::TrailingDot(state.line, state.column),
            state,
        ));
    }

    let answer = TypeAnnotation::Apply(
        join_module_parts(arena, parts.into_bump_slice()),
        part_buf.into_bump_str(),
        &[],
    );

    Ok((MadeProgress, answer, state))
}

fn parse_type_variable<'a>(
    arena: &'a Bump,
    mut state: State<'a>,
) -> ParseResult<'a, TypeAnnotation<'a>, TVariable> {
    let mut buf = String::new_in(arena);

    let start_bytes_len = state.bytes.len();

    match peek_utf8_char_e(&state, TVariable::StartNotLowercase, TVariable::Space) {
        Ok((first_letter, bytes_parsed)) => {
            // Type variables must start with a lowercase letter.
            if first_letter.is_alphabetic() && first_letter.is_lowercase() {
                buf.push(first_letter);
            } else {
                return Err((
                    NoProgress,
                    TVariable::StartNotLowercase(state.line, state.column),
                    state,
                ));
            }

            state = state.advance_without_indenting_e(arena, bytes_parsed, TVariable::Space)?;
        }
        Err(reason) => return Err((NoProgress, reason, state)),
    }

    while !state.bytes.is_empty() {
        match peek_utf8_char_e(&state, TVariable::End, TVariable::Space) {
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

                state = state.advance_without_indenting_e(arena, bytes_parsed, TVariable::Space)?;
            }
            Err(reason) => {
                return state.fail(arena, MadeProgress, reason);
            }
        }
    }

    let answer = TypeAnnotation::BoundVariable(buf.into_bump_str());

    let progress = Progress::from_lengths(start_bytes_len, state.bytes.len());
    Ok((progress, answer, state))
}
