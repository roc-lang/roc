use crate::ast::{AssignedField, Tag, TypeAnnotation};
use crate::blankspace::{space0_around_ee, space0_before_e, space0_e};
use crate::keyword;
use crate::parser::{
    allocated, backtrackable, optional, specialize, specialize_ref, word1, word2, ParseResult,
    Parser,
    Progress::{self, *},
    State, TApply, TInParens, TRecord, TTagUnion, Type,
};
use bumpalo::collections::vec::Vec;
use bumpalo::Bump;
use roc_region::all::{Located, Region};

pub fn located_help<'a>(min_indent: u16) -> impl Parser<'a, Located<TypeAnnotation<'a>>, Type<'a>> {
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
        let (_, ext, state) =
            optional(allocated(specialize_ref(TTagUnion::Type, term(min_indent))))
                .parse(arena, state)?;

        let result = TypeAnnotation::TagUnion {
            tags: tags.into_bump_slice(),
            ext,
            final_comments,
        };

        Ok((MadeProgress, result, state))
    }
}

fn fail_type_start<'a, T: 'a>() -> impl Parser<'a, T, Type<'a>> {
    |_arena, state: State<'a>| Err((NoProgress, Type::TStart(state.line, state.column), state))
}

fn term<'a>(min_indent: u16) -> impl Parser<'a, Located<TypeAnnotation<'a>>, Type<'a>> {
    map_with_arena!(
        and!(
            one_of!(
                loc_wildcard(),
                specialize(Type::TInParens, loc_type_in_parens(min_indent)),
                loc!(specialize(Type::TRecord, record_type(min_indent))),
                loc!(specialize(Type::TTagUnion, tag_union_type(min_indent))),
                loc!(applied_type(min_indent)),
                loc!(parse_type_variable),
                fail_type_start(),
            ),
            // Inline alias notation, e.g. [ Nil, Cons a (List a) ] as List a
            one_of![
                map!(
                    and!(
                        skip_second!(
                            backtrackable(space0_e(min_indent, Type::TSpace, Type::TIndentEnd)),
                            crate::parser::keyword_e(keyword::AS, Type::TEnd)
                        ),
                        space0_before_e(
                            term(min_indent),
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
            Option<(&'a [_], Located<TypeAnnotation<'a>>)>
        )| {
            match opt_as {
                Some((spaces, loc_as)) => {
                    let region = Region::span_across(&loc_ann.region, &loc_as.region);
                    let value =
                        TypeAnnotation::As(arena.alloc(loc_ann), spaces, arena.alloc(loc_as));

                    Located { region, value }
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

fn loc_applied_arg<'a>(min_indent: u16) -> impl Parser<'a, Located<TypeAnnotation<'a>>, Type<'a>> {
    use crate::ast::Spaceable;

    map_with_arena!(
        and!(
            backtrackable(space0_e(min_indent, Type::TSpace, Type::TIndentStart)),
            one_of!(
                loc_wildcard(),
                specialize(Type::TInParens, loc_type_in_parens(min_indent)),
                loc!(specialize(Type::TRecord, record_type(min_indent))),
                loc!(specialize(Type::TTagUnion, tag_union_type(min_indent))),
                loc!(specialize(Type::TApply, parse_concrete_type)),
                loc!(parse_type_variable)
            )
        ),
        |arena: &'a Bump, (spaces, argument): (&'a [_], Located<TypeAnnotation<'a>>)| {
            if spaces.is_empty() {
                argument
            } else {
                let Located { region, value } = argument;
                arena.alloc(value).with_spaces_before(spaces, region)
            }
        }
    )
}

fn loc_type_in_parens<'a>(
    min_indent: u16,
) -> impl Parser<'a, Located<TypeAnnotation<'a>>, TInParens<'a>> {
    between!(
        word1(b'(', TInParens::Open),
        space0_around_ee(
            move |arena, state| specialize_ref(TInParens::Type, expression(min_indent))
                .parse(arena, state),
            min_indent,
            TInParens::Space,
            TInParens::IndentOpen,
            TInParens::IndentEnd,
        ),
        word1(b')', TInParens::IndentEnd)
    )
}

#[inline(always)]
fn tag_type<'a>(min_indent: u16) -> impl Parser<'a, Tag<'a>, TTagUnion<'a>> {
    move |arena, state: State<'a>| {
        let (_, name, state) = loc!(parse_tag_name(TTagUnion::End)).parse(arena, state)?;

        let (_, args, state) =
            specialize_ref(TTagUnion::Type, loc_applied_args_e(min_indent)).parse(arena, state)?;

        let result = if name.value.starts_with('@') {
            Tag::Private {
                name,
                args: args.into_bump_slice(),
            }
        } else {
            Tag::Global {
                name,
                args: args.into_bump_slice(),
            }
        };

        Ok((MadeProgress, result, state))
    }
}

use crate::parser::{Col, Row};
fn parse_tag_name<'a, F, E>(to_problem: F) -> impl Parser<'a, &'a str, E>
where
    F: Fn(Row, Col) -> E,
    E: 'a,
{
    move |arena, state: State<'a>| match crate::ident::tag_name().parse(arena, state) {
        Ok(good) => Ok(good),
        Err((progress, _, state)) => Err((progress, to_problem(state.line, state.column), state)),
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

        let val_parser = specialize_ref(TRecord::Type, term(min_indent));

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

    move |arena, state| {
        let (_, (fields, final_comments), state) = collection_trailing_sep_e!(
            // word1_check_indent!(b'{', TRecord::Open, min_indent, TRecord::IndentOpen),
            word1(b'{', TRecord::Open),
            loc!(record_type_field(min_indent)),
            word1(b',', TRecord::End),
            // word1_check_indent!(b'}', TRecord::End, min_indent, TRecord::IndentEnd),
            word1(b'}', TRecord::End),
            min_indent,
            TRecord::Open,
            TRecord::Space,
            TRecord::IndentEnd
        )
        .parse(arena, state)?;

        let field_term = specialize_ref(TRecord::Type, term(min_indent));
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
                TypeAnnotation::Apply(module_name, name, _) => {
                    if args.is_empty() {
                        // ctor is already an Apply with no args, so return it directly.
                        ctor
                    } else {
                        TypeAnnotation::Apply(module_name, name, args.into_bump_slice())
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
    zero_or_more!(loc_applied_arg(min_indent))
}

fn expression<'a>(min_indent: u16) -> impl Parser<'a, Located<TypeAnnotation<'a>>, Type<'a>> {
    move |arena, state: State<'a>| {
        let (p1, first, state) = space0_before_e(
            term(min_indent),
            min_indent,
            Type::TSpace,
            Type::TIndentStart,
        )
        .parse(arena, state)?;

        let (p2, rest, state) = zero_or_more!(skip_first!(
            word1(b',', Type::TFunctionArgument),
            one_of![
                space0_around_ee(
                    term(min_indent),
                    min_indent,
                    Type::TSpace,
                    Type::TIndentStart,
                    Type::TIndentEnd
                ),
                |_, state: State<'a>| Err((
                    NoProgress,
                    Type::TFunctionArgument(state.line, state.column),
                    state
                ))
            ]
        ))
        .parse(arena, state)?;

        // TODO this space0 is dropped, so newlines just before the function arrow when there
        // is only one argument are not seen by the formatter. Can we do better?
        let (p3, is_function, state) = optional(skip_first!(
            space0_e(min_indent, Type::TSpace, Type::TIndentStart),
            word2(b'-', b'>', Type::TStart)
        ))
        .parse(arena, state)?;

        if is_function.is_some() {
            let (p4, return_type, state) = space0_before_e(
                term(min_indent),
                min_indent,
                Type::TSpace,
                Type::TIndentStart,
            )
            .parse(arena, state)?;

            // prepare arguments
            let mut arguments = Vec::with_capacity_in(rest.len() + 1, arena);
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
                panic!()
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
    state: State<'a>,
) -> ParseResult<'a, TypeAnnotation<'a>, TApply> {
    let initial_bytes = state.bytes;

    match crate::ident::concrete_type().parse(arena, state) {
        Ok((_, (module_name, type_name), state)) => {
            let answer = TypeAnnotation::Apply(module_name, type_name, &[]);

            Ok((MadeProgress, answer, state))
        }
        Err((NoProgress, _, state)) => {
            Err((NoProgress, TApply::End(state.line, state.column), state))
        }
        Err((MadeProgress, _, mut state)) => {
            // we made some progress, but ultimately failed.
            // that means a malformed type name
            let chomped = crate::ident::chomp_malformed(state.bytes);
            let delta = initial_bytes.len() - state.bytes.len();
            let parsed_str =
                unsafe { std::str::from_utf8_unchecked(&initial_bytes[..chomped + delta]) };

            state = state.advance_without_indenting_ee(chomped, |r, c| {
                TApply::Space(crate::parser::BadInputError::LineTooLong, r, c)
            })?;

            Ok((MadeProgress, TypeAnnotation::Malformed(parsed_str), state))
        }
    }
}

fn parse_type_variable<'a>(
    arena: &'a Bump,
    state: State<'a>,
) -> ParseResult<'a, TypeAnnotation<'a>, Type<'a>> {
    match crate::ident::lowercase_ident().parse(arena, state) {
        Ok((_, name, state)) => {
            let answer = TypeAnnotation::BoundVariable(name);

            Ok((MadeProgress, answer, state))
        }
        Err((progress, _, state)) => Err((
            progress,
            Type::TBadTypeVariable(state.line, state.column),
            state,
        )),
    }
}
