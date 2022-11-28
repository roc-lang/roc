use crate::ast::{
    AssignedField, CommentOrNewline, HasAbilities, HasAbility, HasClause, HasImpls, Pattern,
    Spaced, Tag, TypeAnnotation, TypeHeader,
};
use crate::blankspace::{
    space0_around_ee, space0_before_e, space0_before_optional_after, space0_e,
};
use crate::expr::record_value_field;
use crate::ident::{lowercase_ident, lowercase_ident_keyword_e};
use crate::keyword;
use crate::parser::{
    absolute_column_min_indent, increment_min_indent, then, ERecord, ETypeAbilityImpl,
};
use crate::parser::{
    allocated, backtrackable, fail, optional, specialize, specialize_ref, word1, word2, word3,
    EType, ETypeApply, ETypeInParens, ETypeInlineAlias, ETypeRecord, ETypeTagUnion, Parser,
    Progress::{self, *},
};
use crate::state::State;
use bumpalo::collections::vec::Vec;
use bumpalo::Bump;
use roc_region::all::{Loc, Position, Region};

pub fn located<'a>(
    is_trailing_comma_valid: bool,
) -> impl Parser<'a, Loc<TypeAnnotation<'a>>, EType<'a>> {
    expression(is_trailing_comma_valid, false)
}

pub fn located_opaque_signature<'a>(
    is_trailing_comma_valid: bool,
) -> impl Parser<'a, Loc<TypeAnnotation<'a>>, EType<'a>> {
    expression(is_trailing_comma_valid, true)
}

#[inline(always)]
fn tag_union_type<'a>(
    stop_at_surface_has: bool,
) -> impl Parser<'a, TypeAnnotation<'a>, ETypeTagUnion<'a>> {
    move |arena, state, min_indent| {
        let (_, tags, state) = collection_trailing_sep_e!(
            word1(b'[', ETypeTagUnion::Open),
            loc!(tag_type(false)),
            word1(b',', ETypeTagUnion::End),
            word1(b']', ETypeTagUnion::End),
            ETypeTagUnion::IndentEnd,
            Tag::SpaceBefore
        )
        .parse(arena, state, min_indent)?;

        // This could be an open tag union, e.g. `[Foo, Bar]a`
        let (_, ext, state) = optional(allocated(specialize_ref(
            ETypeTagUnion::Type,
            term(stop_at_surface_has),
        )))
        .parse(arena, state, min_indent)?;

        let result = TypeAnnotation::TagUnion { tags, ext };

        Ok((MadeProgress, result, state))
    }
}

fn check_type_alias<'a>(
    arena: &'a Bump,
    annot: Loc<TypeAnnotation<'a>>,
) -> Result<TypeHeader<'a>, ETypeInlineAlias> {
    match annot.value {
        TypeAnnotation::Apply("", tag_name, vars) => {
            let mut var_names = Vec::new_in(arena);
            var_names.reserve(vars.len());
            for var in vars {
                if let TypeAnnotation::BoundVariable(v) = var.value {
                    var_names.push(Loc::at(var.region, Pattern::Identifier(v)));
                } else {
                    return Err(ETypeInlineAlias::ArgumentNotLowercase(var.region.start()));
                }
            }

            let name_start = annot.region.start();
            let name_region =
                Region::between(name_start, name_start.bump_column(tag_name.len() as u32));

            let header = TypeHeader {
                name: Loc::at(name_region, tag_name),
                vars: var_names.into_bump_slice(),
            };

            Ok(header)
        }
        TypeAnnotation::Apply(_, _, _) => Err(ETypeInlineAlias::Qualified(annot.region.start())),
        _ => Err(ETypeInlineAlias::NotAnAlias(annot.region.start())),
    }
}

fn parse_type_alias_after_as<'a>() -> impl Parser<'a, TypeHeader<'a>, EType<'a>> {
    then(
        space0_before_e(term(false), EType::TAsIndentStart),
        // TODO: introduce a better combinator for this.
        // `check_type_alias` doesn't need to modify the state or progress, but it needs to access `state.pos()`
        |arena, state, progress, output| {
            let res = check_type_alias(arena, output);

            match res {
                Ok(header) => Ok((progress, header, state)),
                Err(err) => Err((progress, EType::TInlineAlias(err, state.pos()))),
            }
        },
    )
}

fn term<'a>(stop_at_surface_has: bool) -> impl Parser<'a, Loc<TypeAnnotation<'a>>, EType<'a>> {
    map_with_arena!(
        and!(
            one_of!(
                loc_wildcard(),
                loc_inferred(),
                specialize(EType::TInParens, loc_type_in_parens(stop_at_surface_has)),
                loc!(specialize(EType::TRecord, record_type(stop_at_surface_has))),
                loc!(specialize(
                    EType::TTagUnion,
                    tag_union_type(stop_at_surface_has)
                )),
                loc!(applied_type(stop_at_surface_has)),
                loc!(parse_type_variable(stop_at_surface_has)),
                fail(EType::TStart),
            ),
            // Inline alias notation, e.g. [Nil, Cons a (List a)] as List a
            one_of![
                map!(
                    and!(
                        skip_second!(
                            backtrackable(space0_e(EType::TIndentEnd)),
                            crate::parser::keyword_e(keyword::AS, EType::TEnd)
                        ),
                        parse_type_alias_after_as()
                    ),
                    Some
                ),
                succeed!(None)
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

fn loc_applied_arg<'a>(
    stop_at_surface_has: bool,
) -> impl Parser<'a, Loc<TypeAnnotation<'a>>, EType<'a>> {
    use crate::ast::Spaceable;

    map_with_arena!(
        and!(
            backtrackable(space0_e(EType::TIndentStart)),
            one_of!(
                loc_wildcard(),
                loc_inferred(),
                specialize(EType::TInParens, loc_type_in_parens(stop_at_surface_has)),
                loc!(specialize(EType::TRecord, record_type(stop_at_surface_has))),
                loc!(specialize(
                    EType::TTagUnion,
                    tag_union_type(stop_at_surface_has)
                )),
                loc!(specialize(EType::TApply, concrete_type())),
                loc!(parse_type_variable(stop_at_surface_has))
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
    stop_at_surface_has: bool,
) -> impl Parser<'a, Loc<TypeAnnotation<'a>>, ETypeInParens<'a>> {
    then(
        loc!(and!(
            collection_trailing_sep_e!(
                word1(b'(', ETypeInParens::Open),
                specialize_ref(ETypeInParens::Type, expression(true, false)),
                word1(b',', ETypeInParens::End),
                word1(b')', ETypeInParens::End),
                ETypeInParens::IndentEnd,
                TypeAnnotation::SpaceBefore
            ),
            optional(allocated(specialize_ref(
                ETypeInParens::Type,
                term(stop_at_surface_has)
            )))
        )),
        |_arena, state, progress, item| {
            let Loc {
                region,
                value: (fields, ext),
            } = item;
            if fields.len() > 1 || ext.is_some() {
                Ok((
                    MadeProgress,
                    Loc::at(region, TypeAnnotation::Tuple { fields, ext }),
                    state,
                ))
            } else if fields.len() == 1 {
                Ok((MadeProgress, fields.items[0], state))
            } else {
                debug_assert!(fields.is_empty());
                Err((progress, ETypeInParens::Empty(state.pos())))
            }
        },
    )
    .trace("type_annotation:type_in_parens")
}

#[inline(always)]
fn tag_type<'a>(stop_at_surface_has: bool) -> impl Parser<'a, Tag<'a>, ETypeTagUnion<'a>> {
    move |arena, state: State<'a>, min_indent: u32| {
        let (_, name, state) =
            loc!(parse_tag_name(ETypeTagUnion::End)).parse(arena, state, min_indent)?;

        let (_, args, state) =
            specialize_ref(ETypeTagUnion::Type, loc_applied_args_e(stop_at_surface_has))
                .parse(arena, state, min_indent)?;

        let result = Tag::Apply {
            name,
            args: args.into_bump_slice(),
        };

        Ok((MadeProgress, result, state))
    }
}

fn parse_tag_name<'a, F, E>(to_problem: F) -> impl Parser<'a, &'a str, E>
where
    F: Fn(Position) -> E,
    E: 'a,
{
    move |arena, state: State<'a>, min_indent: u32| match crate::ident::tag_name().parse(
        arena,
        state.clone(),
        min_indent,
    ) {
        Ok(good) => Ok(good),
        Err((progress, _)) => Err((progress, to_problem(state.pos()))),
    }
}

fn record_type_field<'a>() -> impl Parser<'a, AssignedField<'a, TypeAnnotation<'a>>, ETypeRecord<'a>>
{
    use crate::parser::Either::*;
    use AssignedField::*;

    (move |arena, state: State<'a>, min_indent: u32| {
        // You must have a field name, e.g. "email"
        // using the initial pos is important for error reporting
        let pos = state.pos();
        let (progress, loc_label, state) = loc!(specialize(
            move |_, _| ETypeRecord::Field(pos),
            lowercase_ident_keyword_e()
        ))
        .parse(arena, state, min_indent)?;
        debug_assert_eq!(progress, MadeProgress);

        let (_, spaces, state) =
            space0_e(ETypeRecord::IndentEnd).parse(arena, state, min_indent)?;

        // Having a value is optional; both `{ email }` and `{ email: blah }` work.
        // (This is true in both literals and types.)
        let (_, opt_loc_val, state) = optional(either!(
            word1(b':', ETypeRecord::Colon),
            word1(b'?', ETypeRecord::Optional)
        ))
        .parse(arena, state, min_indent)?;

        let val_parser = specialize_ref(ETypeRecord::Type, expression(true, false));

        match opt_loc_val {
            Some(First(_)) => {
                let (_, loc_val, state) = space0_before_e(val_parser, ETypeRecord::IndentColon)
                    .parse(arena, state, min_indent)?;

                Ok((
                    MadeProgress,
                    RequiredValue(loc_label, spaces, arena.alloc(loc_val)),
                    state,
                ))
            }
            Some(Second(_)) => {
                let (_, loc_val, state) = space0_before_e(val_parser, ETypeRecord::IndentOptional)
                    .parse(arena, state, min_indent)?;

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
fn record_type<'a>(
    stop_at_surface_has: bool,
) -> impl Parser<'a, TypeAnnotation<'a>, ETypeRecord<'a>> {
    map!(
        and!(
            collection_trailing_sep_e!(
                word1(b'{', ETypeRecord::Open),
                loc!(record_type_field()),
                word1(b',', ETypeRecord::End),
                word1(b'}', ETypeRecord::End),
                ETypeRecord::IndentEnd,
                AssignedField::SpaceBefore
            ),
            optional(allocated(specialize_ref(
                ETypeRecord::Type,
                term(stop_at_surface_has)
            )))
        ),
        |(fields, ext)| { TypeAnnotation::Record { fields, ext } }
    )
    .trace("type_annotation:record_type")
}

fn applied_type<'a>(stop_at_surface_has: bool) -> impl Parser<'a, TypeAnnotation<'a>, EType<'a>> {
    map!(
        and!(
            specialize(EType::TApply, concrete_type()),
            // Optionally parse space-separated arguments for the constructor,
            // e.g. `Str Float` in `Map Str Float`
            loc_applied_args_e(stop_at_surface_has)
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
    stop_at_surface_has: bool,
) -> impl Parser<'a, Vec<'a, Loc<TypeAnnotation<'a>>>, EType<'a>> {
    zero_or_more!(loc_applied_arg(stop_at_surface_has))
}

// Hash & Eq & ...
fn ability_chain<'a>() -> impl Parser<'a, Vec<'a, Loc<TypeAnnotation<'a>>>, EType<'a>> {
    map!(
        and!(
            space0_before_optional_after(
                specialize(EType::TApply, loc!(concrete_type())),
                EType::TIndentStart,
                EType::TIndentEnd,
            ),
            zero_or_more!(skip_first!(
                word1(b'&', EType::THasClause),
                space0_before_optional_after(
                    specialize(EType::TApply, loc!(concrete_type())),
                    EType::TIndentStart,
                    EType::TIndentEnd,
                )
            ))
        ),
        |(first_ability, mut other_abilities): (
            Loc<TypeAnnotation<'a>>,
            Vec<'a, Loc<TypeAnnotation<'a>>>
        )| {
            other_abilities.insert(0, first_ability);
            other_abilities
        }
    )
}

fn has_clause<'a>() -> impl Parser<'a, Loc<HasClause<'a>>, EType<'a>> {
    map!(
        // Suppose we are trying to parse "a has Hash"
        and!(
            space0_around_ee(
                // Parse "a", with appropriate spaces
                specialize(
                    |_, pos| EType::TBadTypeVariable(pos),
                    loc!(map!(lowercase_ident(), Spaced::Item)),
                ),
                EType::TIndentStart,
                EType::TIndentEnd
            ),
            skip_first!(
                // Parse "has"; we don't care about this keyword
                word3(b'h', b'a', b's', EType::THasClause),
                // Parse "Hash & ..."; this may be qualified from another module like "Hash.Hash"
                absolute_column_min_indent(ability_chain())
            )
        ),
        |(var, abilities): (Loc<Spaced<'a, &'a str>>, Vec<'a, Loc<TypeAnnotation<'a>>>)| {
            let abilities_region = Region::span_across(
                &abilities.first().unwrap().region,
                &abilities.last().unwrap().region,
            );
            let region = Region::span_across(&var.region, &abilities_region);
            let has_clause = HasClause {
                var,
                abilities: abilities.into_bump_slice(),
            };
            Loc::at(region, has_clause)
        }
    )
}

/// Parse a chain of `has` clauses, e.g. " | a has Hash, b has Eq".
/// Returns the clauses and spaces before the starting "|", if there were any.
fn has_clause_chain<'a>(
) -> impl Parser<'a, (&'a [CommentOrNewline<'a>], &'a [Loc<HasClause<'a>>]), EType<'a>> {
    move |arena, state: State<'a>, min_indent: u32| {
        let (_, (spaces_before, ()), state) =
            and!(space0_e(EType::TIndentStart), word1(b'|', EType::TWhereBar))
                .parse(arena, state, min_indent)?;

        // Parse the first clause (there must be one), then the rest
        let (_, first_clause, state) = has_clause().parse(arena, state, min_indent)?;

        let (_, mut clauses, state) =
            zero_or_more!(skip_first!(word1(b',', EType::THasClause), has_clause()))
                .parse(arena, state, min_indent)?;

        // Usually the number of clauses shouldn't be too large, so this is okay
        clauses.insert(0, first_clause);

        Ok((
            MadeProgress,
            (spaces_before, clauses.into_bump_slice()),
            state,
        ))
    }
}

/// Parse a has-abilities clause, e.g. `has [Eq, Hash]`.
pub fn has_abilities<'a>() -> impl Parser<'a, Loc<HasAbilities<'a>>, EType<'a>> {
    increment_min_indent(skip_first!(
        // Parse "has"; we don't care about this keyword
        word3(b'h', b'a', b's', EType::THasClause),
        // Parse "Hash"; this may be qualified from another module like "Hash.Hash"
        space0_before_e(
            loc!(map!(
                collection_trailing_sep_e!(
                    word1(b'[', EType::TStart),
                    loc!(parse_has_ability()),
                    word1(b',', EType::TEnd),
                    word1(b']', EType::TEnd),
                    EType::TIndentEnd,
                    HasAbility::SpaceBefore
                ),
                HasAbilities::Has
            )),
            EType::TIndentEnd,
        )
    ))
}

fn parse_has_ability<'a>() -> impl Parser<'a, HasAbility<'a>, EType<'a>> {
    increment_min_indent(map!(
        and!(
            loc!(specialize(EType::TApply, concrete_type())),
            optional(space0_before_e(
                loc!(map!(
                    specialize(
                        EType::TAbilityImpl,
                        collection_trailing_sep_e!(
                            word1(b'{', ETypeAbilityImpl::Open),
                            specialize(|e: ERecord<'_>, _| e.into(), loc!(record_value_field())),
                            word1(b',', ETypeAbilityImpl::End),
                            word1(b'}', ETypeAbilityImpl::End),
                            ETypeAbilityImpl::IndentEnd,
                            AssignedField::SpaceBefore
                        )
                    ),
                    HasImpls::HasImpls
                )),
                EType::TIndentEnd
            ))
        ),
        |(ability, impls): (_, Option<_>)| { HasAbility::HasAbility { ability, impls } }
    ))
}

fn expression<'a>(
    is_trailing_comma_valid: bool,
    stop_at_surface_has: bool,
) -> impl Parser<'a, Loc<TypeAnnotation<'a>>, EType<'a>> {
    (move |arena, state: State<'a>, min_indent: u32| {
        let (p1, first, state) = space0_before_e(term(stop_at_surface_has), EType::TIndentStart)
            .parse(arena, state, min_indent)?;

        let result = and![
            zero_or_more!(skip_first!(
                word1(b',', EType::TFunctionArgument),
                one_of![
                    space0_around_ee(
                        term(stop_at_surface_has),
                        EType::TIndentStart,
                        EType::TIndentEnd
                    ),
                    fail(EType::TFunctionArgument)
                ]
            ))
            .trace("type_annotation:expression:rest_args"),
            // TODO this space0 is dropped, so newlines just before the function arrow when there
            // is only one argument are not seen by the formatter. Can we do better?
            skip_second!(
                space0_e(EType::TIndentStart),
                word2(b'-', b'>', EType::TStart)
            )
            .trace("type_annotation:expression:arrow")
        ]
        .parse(arena, state.clone(), min_indent);

        let (progress, annot, state) = match result {
            Ok((p2, (rest, _dropped_spaces), state)) => {
                let (p3, return_type, state) =
                    space0_before_e(term(stop_at_surface_has), EType::TIndentStart)
                        .parse(arena, state, min_indent)?;

                let region = Region::span_across(&first.region, &return_type.region);

                // prepare arguments
                let mut arguments = Vec::with_capacity_in(rest.len() + 1, arena);
                arguments.push(first);
                arguments.extend(rest);
                let output = arena.alloc(arguments);

                let result = Loc {
                    region,
                    value: TypeAnnotation::Function(output, arena.alloc(return_type)),
                };
                let progress = p1.or(p2).or(p3);
                (progress, result, state)
            }
            Err(err) => {
                if !is_trailing_comma_valid {
                    let (_, comma, _) = optional(skip_first!(
                        space0_e(EType::TIndentStart),
                        word1(b',', EType::TStart)
                    ))
                    .trace("check trailing comma")
                    .parse(arena, state.clone(), min_indent)?;

                    if comma.is_some() {
                        // If the surrounding scope has declared that a trailing comma is not a valid state
                        // for a type annotation - and we found one anyway - return an error so that we can
                        // produce a more useful error message, knowing that the user was probably writing a
                        // function type and messed up the syntax somehow.
                        return Err(err);
                    }
                }

                // We ran into trouble parsing the function bits; just return the single term
                (p1, first, state)
            }
        };

        // Finally, try to parse a where clause if there is one.
        // The where clause must be at least as deep as where the type annotation started.
        match has_clause_chain().parse(arena, state.clone(), min_indent) {
            Ok((where_progress, (spaces_before, has_chain), state)) => {
                use crate::ast::Spaceable;

                let region = Region::span_across(&annot.region, &has_chain.last().unwrap().region);
                let type_annot = if !spaces_before.is_empty() {
                    let spaced = arena
                        .alloc(annot.value)
                        .with_spaces_before(spaces_before, annot.region);
                    &*arena.alloc(spaced)
                } else {
                    &*arena.alloc(annot)
                };
                let where_annot = TypeAnnotation::Where(type_annot, has_chain);
                Ok((
                    where_progress.or(progress),
                    Loc::at(region, where_annot),
                    state,
                ))
            }
            Err(_) => {
                // Ran into a problem parsing a where clause; don't suppose there is one.
                Ok((progress, annot, state))
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

fn concrete_type<'a>() -> impl Parser<'a, TypeAnnotation<'a>, ETypeApply> {
    move |arena: &'a Bump, state: State<'a>, min_indent: u32| {
        let initial_bytes = state.bytes();

        match crate::ident::concrete_type().parse(arena, state.clone(), min_indent) {
            Ok((_, (module_name, type_name), state)) => {
                let answer = TypeAnnotation::Apply(module_name, type_name, &[]);

                Ok((MadeProgress, answer, state))
            }
            Err((NoProgress, _)) => Err((NoProgress, ETypeApply::End(state.pos()))),
            Err((MadeProgress, _)) => {
                let mut state = state.clone();
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
}

fn parse_type_variable<'a>(
    stop_at_surface_has: bool,
) -> impl Parser<'a, TypeAnnotation<'a>, EType<'a>> {
    move |arena, state: State<'a>, min_indent: u32| match crate::ident::lowercase_ident().parse(
        arena,
        state.clone(),
        min_indent,
    ) {
        Ok((_, name, state)) => {
            if name == "has" && stop_at_surface_has {
                Err((NoProgress, EType::TEnd(state.pos())))
            } else {
                let answer = TypeAnnotation::BoundVariable(name);

                Ok((MadeProgress, answer, state))
            }
        }
        Err((progress, _)) => Err((progress, EType::TBadTypeVariable(state.pos()))),
    }
}
