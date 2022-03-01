use crate::ast::{AssignedField, Pattern, Tag, TypeAnnotation, TypeHeader};
use crate::blankspace::{space0_around_ee, space0_before_e, space0_e};
use crate::keyword;
use crate::parser::{
    allocated, backtrackable, optional, specialize, specialize_ref, word1, word2, EType,
    ETypeApply, ETypeInParens, ETypeInlineAlias, ETypeRecord, ETypeTagUnion, ParseResult, Parser,
    Progress::{self, *},
};
use crate::state::State;
use bumpalo::collections::vec::Vec;
use bumpalo::Bump;
use roc_region::all::{Loc, Position, Region};

pub fn located_help<'a>(
    min_indent: u32,
    is_trailing_comma_valid: bool,
) -> impl Parser<'a, Loc<TypeAnnotation<'a>>, EType<'a>> {
    expression(min_indent, is_trailing_comma_valid)
}

#[inline(always)]
fn tag_union_type<'a>(min_indent: u32) -> impl Parser<'a, TypeAnnotation<'a>, ETypeTagUnion<'a>> {
    move |arena, state| {
        let (_, tags, state) = collection_trailing_sep_e!(
            word1(b'[', ETypeTagUnion::Open),
            loc!(tag_type(min_indent)),
            word1(b',', ETypeTagUnion::End),
            word1(b']', ETypeTagUnion::End),
            min_indent,
            ETypeTagUnion::Open,
            ETypeTagUnion::IndentEnd,
            Tag::SpaceBefore
        )
        .parse(arena, state)?;

        // This could be an open tag union, e.g. `[ Foo, Bar ]a`
        let (_, ext, state) = optional(allocated(specialize_ref(
            ETypeTagUnion::Type,
            term(min_indent),
        )))
        .parse(arena, state)?;

        let result = TypeAnnotation::TagUnion { tags, ext };

        Ok((MadeProgress, result, state))
    }
}

fn check_type_alias(
    p: Progress,
    annot: Loc<TypeAnnotation>,
) -> impl Parser<TypeHeader, ETypeInlineAlias> {
    move |arena, state| match annot.value {
        TypeAnnotation::Apply("", tag_name, vars) => {
            let mut var_names = Vec::new_in(arena);
            var_names.reserve(vars.len());
            for var in vars {
                if let TypeAnnotation::BoundVariable(v) = var.value {
                    var_names.push(Loc::at(var.region, Pattern::Identifier(v)));
                } else {
                    return Err((
                        p,
                        ETypeInlineAlias::ArgumentNotLowercase(var.region.start()),
                        state,
                    ));
                }
            }

            let name_start = annot.region.start();
            let name_region =
                Region::between(name_start, name_start.bump_column(tag_name.len() as u32));

            let header = TypeHeader {
                name: Loc::at(name_region, tag_name),
                vars: var_names.into_bump_slice(),
            };

            Ok((p, header, state))
        }
        TypeAnnotation::Apply(_, _, _) => {
            Err((p, ETypeInlineAlias::Qualified(annot.region.start()), state))
        }
        _ => Err((p, ETypeInlineAlias::NotAnAlias(annot.region.start()), state)),
    }
}

fn parse_type_alias_after_as<'a>(min_indent: u32) -> impl Parser<'a, TypeHeader<'a>, EType<'a>> {
    move |arena, state| {
        space0_before_e(term(min_indent), min_indent, EType::TAsIndentStart)
            .parse(arena, state)
            .and_then(|(p, annot, state)| {
                specialize(EType::TInlineAlias, check_type_alias(p, annot)).parse(arena, state)
            })
    }
}

fn fail_type_start<'a, T: 'a>() -> impl Parser<'a, T, EType<'a>> {
    |_arena, state: State<'a>| Err((NoProgress, EType::TStart(state.pos()), state))
}

fn term<'a>(min_indent: u32) -> impl Parser<'a, Loc<TypeAnnotation<'a>>, EType<'a>> {
    map_with_arena!(
        and!(
            one_of!(
                loc_wildcard(),
                loc_inferred(),
                specialize(EType::TInParens, loc_type_in_parens(min_indent)),
                loc!(specialize(EType::TRecord, record_type(min_indent))),
                loc!(specialize(EType::TTagUnion, tag_union_type(min_indent))),
                loc!(applied_type(min_indent)),
                loc!(parse_type_variable),
                fail_type_start(),
            ),
            // Inline alias notation, e.g. [ Nil, Cons a (List a) ] as List a
            one_of![
                map!(
                    and!(
                        skip_second!(
                            backtrackable(space0_e(min_indent, EType::TIndentEnd)),
                            crate::parser::keyword_e(keyword::AS, EType::TEnd)
                        ),
                        parse_type_alias_after_as(min_indent)
                    ),
                    Some
                ),
                |_, state| Ok((NoProgress, None, state))
            ]
        ),
        |arena: &'a Bump,
         (loc_ann, opt_as): (Loc<TypeAnnotation<'a>>, Option<(&'a [_], TypeHeader<'a>)>)| {
            match opt_as {
                Some((spaces, alias)) => {
                    let alias_vars_region =
                        Region::across_all(alias.vars.iter().map(|v| &v.region));
                    let region = Region::span_across(&loc_ann.region, &alias_vars_region);
                    let value = TypeAnnotation::As(arena.alloc(loc_ann), spaces, alias);

                    Loc { region, value }
                }

                None => loc_ann,
            }
        }
    )
    .trace("type_annotation:term")
}

/// The `*` type variable, e.g. in (List *) Wildcard,
fn loc_wildcard<'a>() -> impl Parser<'a, Loc<TypeAnnotation<'a>>, EType<'a>> {
    map!(loc!(word1(b'*', EType::TWildcard)), |loc_val: Loc<()>| {
        loc_val.map(|_| TypeAnnotation::Wildcard)
    })
}

/// The `_` indicating an inferred type, e.g. in (List _)
fn loc_inferred<'a>() -> impl Parser<'a, Loc<TypeAnnotation<'a>>, EType<'a>> {
    map!(loc!(word1(b'_', EType::TInferred)), |loc_val: Loc<()>| {
        loc_val.map(|_| TypeAnnotation::Inferred)
    })
}

fn loc_applied_arg<'a>(min_indent: u32) -> impl Parser<'a, Loc<TypeAnnotation<'a>>, EType<'a>> {
    use crate::ast::Spaceable;

    map_with_arena!(
        and!(
            backtrackable(space0_e(min_indent, EType::TIndentStart)),
            one_of!(
                loc_wildcard(),
                loc_inferred(),
                specialize(EType::TInParens, loc_type_in_parens(min_indent)),
                loc!(specialize(EType::TRecord, record_type(min_indent))),
                loc!(specialize(EType::TTagUnion, tag_union_type(min_indent))),
                loc!(specialize(EType::TApply, parse_concrete_type)),
                loc!(parse_type_variable)
            )
        ),
        |arena: &'a Bump, (spaces, argument): (&'a [_], Loc<TypeAnnotation<'a>>)| {
            if spaces.is_empty() {
                argument
            } else {
                let Loc { region, value } = argument;
                arena.alloc(value).with_spaces_before(spaces, region)
            }
        }
    )
}

fn loc_type_in_parens<'a>(
    min_indent: u32,
) -> impl Parser<'a, Loc<TypeAnnotation<'a>>, ETypeInParens<'a>> {
    between!(
        word1(b'(', ETypeInParens::Open),
        space0_around_ee(
            move |arena, state| specialize_ref(ETypeInParens::Type, expression(min_indent, true))
                .parse(arena, state),
            min_indent,
            ETypeInParens::IndentOpen,
            ETypeInParens::IndentEnd,
        ),
        word1(b')', ETypeInParens::IndentEnd)
    )
}

#[inline(always)]
fn tag_type<'a>(min_indent: u32) -> impl Parser<'a, Tag<'a>, ETypeTagUnion<'a>> {
    move |arena, state: State<'a>| {
        let (_, name, state) = loc!(parse_tag_name(ETypeTagUnion::End)).parse(arena, state)?;

        let (_, args, state) = specialize_ref(ETypeTagUnion::Type, loc_applied_args_e(min_indent))
            .parse(arena, state)?;

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

fn parse_tag_name<'a, F, E>(to_problem: F) -> impl Parser<'a, &'a str, E>
where
    F: Fn(Position) -> E,
    E: 'a,
{
    move |arena, state: State<'a>| match crate::ident::tag_name().parse(arena, state) {
        Ok(good) => Ok(good),
        Err((progress, _, state)) => Err((progress, to_problem(state.pos()), state)),
    }
}

fn record_type_field<'a>(
    min_indent: u32,
) -> impl Parser<'a, AssignedField<'a, TypeAnnotation<'a>>, ETypeRecord<'a>> {
    use crate::ident::lowercase_ident;
    use crate::parser::Either::*;
    use AssignedField::*;

    (move |arena, state: State<'a>| {
        // You must have a field name, e.g. "email"
        // using the initial pos is important for error reporting
        let pos = state.pos();
        let (progress, loc_label, state) = loc!(specialize(
            move |_, _| ETypeRecord::Field(pos),
            lowercase_ident()
        ))
        .parse(arena, state)?;
        debug_assert_eq!(progress, MadeProgress);

        let (_, spaces, state) =
            space0_e(min_indent, ETypeRecord::IndentEnd).parse(arena, state)?;

        // Having a value is optional; both `{ email }` and `{ email: blah }` work.
        // (This is true in both literals and types.)
        let (_, opt_loc_val, state) = optional(either!(
            word1(b':', ETypeRecord::Colon),
            word1(b'?', ETypeRecord::Optional)
        ))
        .parse(arena, state)?;

        let val_parser = specialize_ref(ETypeRecord::Type, expression(min_indent, true));

        match opt_loc_val {
            Some(First(_)) => {
                let (_, loc_val, state) =
                    space0_before_e(val_parser, min_indent, ETypeRecord::IndentColon)
                        .parse(arena, state)?;

                Ok((
                    MadeProgress,
                    RequiredValue(loc_label, spaces, arena.alloc(loc_val)),
                    state,
                ))
            }
            Some(Second(_)) => {
                let (_, loc_val, state) =
                    space0_before_e(val_parser, min_indent, ETypeRecord::IndentOptional)
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
    })
    .trace("type_annotation:record_type_field")
}

#[inline(always)]
fn record_type<'a>(min_indent: u32) -> impl Parser<'a, TypeAnnotation<'a>, ETypeRecord<'a>> {
    use crate::type_annotation::TypeAnnotation::*;

    (move |arena, state| {
        let (_, fields, state) = collection_trailing_sep_e!(
            // word1_check_indent!(b'{', TRecord::Open, min_indent, TRecord::IndentOpen),
            word1(b'{', ETypeRecord::Open),
            loc!(record_type_field(min_indent)),
            word1(b',', ETypeRecord::End),
            // word1_check_indent!(b'}', TRecord::End, min_indent, TRecord::IndentEnd),
            word1(b'}', ETypeRecord::End),
            min_indent,
            ETypeRecord::Open,
            ETypeRecord::IndentEnd,
            AssignedField::SpaceBefore
        )
        .parse(arena, state)?;

        let field_term = specialize_ref(ETypeRecord::Type, term(min_indent));
        let (_, ext, state) = optional(allocated(field_term)).parse(arena, state)?;

        let result = Record { fields, ext };

        Ok((MadeProgress, result, state))
    })
    .trace("type_annotation:record_type")
}

fn applied_type<'a>(min_indent: u32) -> impl Parser<'a, TypeAnnotation<'a>, EType<'a>> {
    map!(
        and!(
            specialize(EType::TApply, parse_concrete_type),
            // Optionally parse space-separated arguments for the constructor,
            // e.g. `Str Float` in `Map Str Float`
            loc_applied_args_e(min_indent)
        ),
        |(ctor, args): (TypeAnnotation<'a>, Vec<'a, Loc<TypeAnnotation<'a>>>)| {
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
    .trace("type_annotation:applied_type")
}

fn loc_applied_args_e<'a>(
    min_indent: u32,
) -> impl Parser<'a, Vec<'a, Loc<TypeAnnotation<'a>>>, EType<'a>> {
    zero_or_more!(loc_applied_arg(min_indent))
}

fn expression<'a>(
    min_indent: u32,
    is_trailing_comma_valid: bool,
) -> impl Parser<'a, Loc<TypeAnnotation<'a>>, EType<'a>> {
    (move |arena, state: State<'a>| {
        let (p1, first, state) = space0_before_e(term(min_indent), min_indent, EType::TIndentStart)
            .parse(arena, state)?;

        let result = and![
            zero_or_more!(skip_first!(
                word1(b',', EType::TFunctionArgument),
                one_of![
                    space0_around_ee(
                        term(min_indent),
                        min_indent,
                        EType::TIndentStart,
                        EType::TIndentEnd
                    ),
                    |_, state: State<'a>| Err((
                        NoProgress,
                        EType::TFunctionArgument(state.pos()),
                        state
                    ))
                ]
            ))
            .trace("type_annotation:expression:rest_args"),
            // TODO this space0 is dropped, so newlines just before the function arrow when there
            // is only one argument are not seen by the formatter. Can we do better?
            skip_second!(
                space0_e(min_indent, EType::TIndentStart),
                word2(b'-', b'>', EType::TStart)
            )
            .trace("type_annotation:expression:arrow")
        ]
        .parse(arena, state.clone());

        match result {
            Ok((p2, (rest, _dropped_spaces), state)) => {
                let (p3, return_type, state) =
                    space0_before_e(term(min_indent), min_indent, EType::TIndentStart)
                        .parse(arena, state)?;

                // prepare arguments
                let mut arguments = Vec::with_capacity_in(rest.len() + 1, arena);
                arguments.push(first);
                arguments.extend(rest);
                let output = arena.alloc(arguments);

                let result = Loc {
                    region: return_type.region,
                    value: TypeAnnotation::Function(output, arena.alloc(return_type)),
                };
                let progress = p1.or(p2).or(p3);
                Ok((progress, result, state))
            }
            Err(err) => {
                if !is_trailing_comma_valid {
                    let (_, comma, _) = optional(skip_first!(
                        space0_e(min_indent, EType::TIndentStart),
                        word1(b',', EType::TStart)
                    ))
                    .trace("check trailing comma")
                    .parse(arena, state.clone())?;

                    if comma.is_some() {
                        // If the surrounding scope has declared that a trailing comma is not a valid state
                        // for a type annotation - and we found one anyway - return an error so that we can
                        // produce a more useful error message, knowing that the user was probably writing a
                        // function type and messed up the syntax somehow.
                        return Err(err);
                    }
                }

                // We ran into trouble parsing the function bits; just return the single term
                Ok((p1, first, state))
            }
        }
    })
    .trace("type_annotation:expression")
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
) -> ParseResult<'a, TypeAnnotation<'a>, ETypeApply> {
    let initial_bytes = state.bytes();

    match crate::ident::concrete_type().parse(arena, state) {
        Ok((_, (module_name, type_name), state)) => {
            let answer = TypeAnnotation::Apply(module_name, type_name, &[]);

            Ok((MadeProgress, answer, state))
        }
        Err((NoProgress, _, state)) => Err((NoProgress, ETypeApply::End(state.pos()), state)),
        Err((MadeProgress, _, mut state)) => {
            // we made some progress, but ultimately failed.
            // that means a malformed type name
            let chomped = crate::ident::chomp_malformed(state.bytes());
            let delta = initial_bytes.len() - state.bytes().len();
            let parsed_str =
                unsafe { std::str::from_utf8_unchecked(&initial_bytes[..chomped + delta]) };

            state = state.advance(chomped);

            Ok((MadeProgress, TypeAnnotation::Malformed(parsed_str), state))
        }
    }
}

fn parse_type_variable<'a>(
    arena: &'a Bump,
    state: State<'a>,
) -> ParseResult<'a, TypeAnnotation<'a>, EType<'a>> {
    match crate::ident::lowercase_ident().parse(arena, state) {
        Ok((_, name, state)) => {
            let answer = TypeAnnotation::BoundVariable(name);

            Ok((MadeProgress, answer, state))
        }
        Err((progress, _, state)) => Err((progress, EType::TBadTypeVariable(state.pos()), state)),
    }
}
