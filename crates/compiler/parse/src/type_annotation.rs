use crate::ast::{
    AbilityImpls, AssignedField, Expr, ImplementsAbilities, ImplementsAbility, ImplementsClause,
    Pattern, Spaceable, Spaced, Tag, TypeAnnotation, TypeHeader,
};
use crate::blankspace::{
    eat_space_check, parse_space, space0_before_e, with_spaces, with_spaces_after,
    with_spaces_before,
};
use crate::expr::parse_record_field;
use crate::ident::{
    chomp_concrete_type, chomp_uppercase_part, lowercase_ident_keyword_e, parse_lowercase_ident,
};
use crate::keyword;
use crate::parser::{
    at_keyword, collection_inner, collection_trailing_sep_e, increment_min_indent, indented_seq,
    loc, map, reset_min_indent, skip_first, skip_second, ERecord, ETypeAbilityImpl, ParseResult,
    Progress,
};
use crate::parser::{
    backtrackable, byte, optional, specialize_err, specialize_err_ref, word, EType, ETypeApply,
    ETypeInParens, ETypeInlineAlias, ETypeRecord, ETypeTagUnion, Parser, Progress::*,
};
use crate::state::State;
use bumpalo::collections::vec::Vec;
use bumpalo::Bump;
use roc_region::all::{Loc, Position, Region};

fn tag_union_type<'a>(
    stop_at_first_impl: bool,
) -> impl Parser<'a, TypeAnnotation<'a>, ETypeTagUnion<'a>> {
    move |arena, state, min_indent| {
        let (_, tags, state) = collection_trailing_sep_e(
            byte(b'[', ETypeTagUnion::Open),
            tag_type(),
            byte(b']', ETypeTagUnion::End),
            Tag::SpaceBefore,
        )
        .parse(arena, state, min_indent)?;

        // This could be an open tag union, e.g. `[Foo, Bar]a`
        let ext_pos = state.pos();
        let (ext, state) = match parse_term(stop_at_first_impl, arena, state.clone(), min_indent) {
            Ok((_, out, state)) => (Some(&*arena.alloc(out)), state),
            Err((NoProgress, _)) => (None, state),
            Err((_, fail)) => {
                let fail = ETypeTagUnion::Type(arena.alloc(fail), ext_pos);
                return Err((MadeProgress, fail));
            }
        };

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
                    var_names.push(Loc::at(var.region, Pattern::Identifier { ident: v }));
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

fn parse_term<'a>(
    stop_at_first_impl: bool,
    arena: &'a Bump,
    state: State<'a>,
    min_indent: u32,
) -> ParseResult<'a, Loc<TypeAnnotation<'a>>, EType<'a>> {
    let start = state.pos();
    let res = match state.bytes().first() {
        Some(b) => match b {
            b'(' => {
                let state = state.inc();
                match rest_of_type_in_parens(start, stop_at_first_impl, arena, state, min_indent) {
                    Ok(ok) => Some(ok),
                    Err((p, fail)) => return Err((p, EType::TInParens(fail, start))),
                }
            }
            b'{' => match record_type(stop_at_first_impl).parse(arena, state, min_indent) {
                Ok((p, out, state)) => Some((p, Loc::pos(start, state.pos(), out), state)),
                Err((p, fail)) => return Err((p, EType::TRecord(fail, start))),
            },
            b'[' => match tag_union_type(stop_at_first_impl).parse(arena, state, min_indent) {
                Ok((p, out, state)) => Some((p, Loc::pos(start, state.pos(), out), state)),
                Err((p, fail)) => return Err((p, EType::TTagUnion(fail, start))),
            },
            b'*' => {
                // The `*` type variable, e.g. in (List *)
                let out = Loc::pos(start, start.next(), TypeAnnotation::Wildcard);
                Some((MadeProgress, out, state.inc()))
            }
            b'_' if !matches!(
                state.bytes().get(1),
                Some(b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' | b'_')
            ) =>
            {
                // The `_` indicating an inferred type, e.g. in (List _)
                let out = Loc::pos(start, start.next(), TypeAnnotation::Inferred);
                Some((MadeProgress, out, state.inc()))
            }
            _ => {
                let out = match parse_lowercase_ident(state.clone()) {
                    Ok((_, name, state)) => {
                        if name == keyword::WHERE
                            || (stop_at_first_impl && name == keyword::IMPLEMENTS)
                        {
                            None
                        } else {
                            let type_ann = TypeAnnotation::BoundVariable(name);
                            let type_ann = Loc::pos(start, state.pos(), type_ann);
                            Some((MadeProgress, type_ann, state))
                        }
                    }
                    Err((NoProgress, _)) => None,
                    Err(_) => return Err((MadeProgress, EType::TBadTypeVariable(start))),
                };

                match out {
                    None => {
                        match applied_type(stop_at_first_impl).parse(arena, state, min_indent) {
                            Ok((p, ann, state)) => {
                                Some((p, Loc::pos(start, state.pos(), ann), state))
                            }
                            Err((NoProgress, _)) => None,
                            Err(err) => return Err(err),
                        }
                    }
                    some => some,
                }
            }
        },
        _ => None,
    };

    let (type_ann, state) = match res {
        Some((_, out, state)) => (out, state),
        None => return Err((NoProgress, EType::TStart(start))),
    };

    let type_ann_state = state.clone();
    let (spaces_before_as, state) =
        match eat_space_check(EType::TIndentEnd, arena, state, min_indent, false) {
            Ok((_, sp, state)) => (sp, state),
            Err(_) => return Ok((MadeProgress, type_ann, type_ann_state)),
        };

    if !at_keyword(keyword::AS, &state) {
        return Ok((MadeProgress, type_ann, type_ann_state));
    }
    let state = state.advance(keyword::AS.len());

    let (sp, spaces_after_as, state) =
        eat_space_check(EType::TAsIndentStart, arena, state, min_indent, false)?;

    let (mut alias_ann, state) = match parse_term(false, arena, state, min_indent) {
        Ok((_, out, state)) => (out, state),
        Err((ep, fail)) => return Err((ep.or(sp), fail)),
    };

    alias_ann = with_spaces_before(arena, alias_ann, spaces_after_as);

    let as_alias = match check_type_alias(arena, alias_ann) {
        Ok(header) => header,
        Err(err) => return Err((MadeProgress, EType::TInlineAlias(err, state.pos()))),
    };

    let mut region = type_ann.region;
    if let Some(alias_var) = as_alias.vars.last() {
        region = Region::span_across(&region, &alias_var.region);
    }
    let value = TypeAnnotation::As(arena.alloc(type_ann), spaces_before_as, as_alias);
    Ok((MadeProgress, Loc { region, value }, state))
}

fn loc_applied_arg<'a>(
    stop_at_first_impl: bool,
    arena: &'a Bump,
    state: State<'a>,
    min_indent: u32,
) -> ParseResult<'a, Loc<TypeAnnotation<'a>>, EType<'a>> {
    let (spaces, state) = match eat_space_check(EType::TIndentStart, arena, state, min_indent, true)
    {
        Ok((_, sp, state)) => (sp, state),
        Err((_, fail)) => return Err((NoProgress, fail)),
    };

    let start = state.pos();
    let (type_ann, state) = match state.bytes().first() {
        Some(b) => match b {
            b'(' => {
                let state = state.inc();
                match rest_of_type_in_parens(start, stop_at_first_impl, arena, state, min_indent) {
                    Ok((_, out, state)) => (out, state),
                    Err((p, fail)) => return Err((p, EType::TInParens(fail, start))),
                }
            }
            b'{' => match record_type(stop_at_first_impl).parse(arena, state, min_indent) {
                Ok((_, out, state)) => (Loc::pos(start, state.pos(), out), state),
                Err((p, fail)) => return Err((p, EType::TRecord(fail, start))),
            },
            b'[' => match tag_union_type(stop_at_first_impl).parse(arena, state, min_indent) {
                Ok((_, out, state)) => (Loc::pos(start, state.pos(), out), state),
                Err((p, fail)) => return Err((p, EType::TTagUnion(fail, start))),
            },
            b'*' => {
                // The `*` type variable, e.g. in (List *)
                let out = Loc::pos(start, start.next(), TypeAnnotation::Wildcard);
                (out, state.inc())
            }
            b'_' if !matches!(
                state.bytes().get(1),
                Some(b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' | b'_')
            ) =>
            {
                let out = Loc::pos(start, start.next(), TypeAnnotation::Inferred);
                (out, state.inc())
            }
            _ => {
                let out = match parse_lowercase_ident(state.clone()) {
                    Ok((_, name, state)) => {
                        if name == keyword::WHERE
                            || (stop_at_first_impl && name == keyword::IMPLEMENTS)
                        {
                            None
                        } else {
                            let type_ann = TypeAnnotation::BoundVariable(name);
                            let type_ann = Loc::pos(start, state.pos(), type_ann);
                            Some((type_ann, state))
                        }
                    }
                    Err((NoProgress, _)) => None,
                    Err(_) => return Err((MadeProgress, EType::TBadTypeVariable(start))),
                };

                match out {
                    Some(ok) => ok,
                    None => match concrete_type().parse(arena, state, min_indent) {
                        Ok((_, out, state)) => (Loc::pos(start, state.pos(), out), state),
                        Err((NoProgress, _)) => return Err((NoProgress, EType::TStart(start))),
                        Err((_, fail)) => return Err((MadeProgress, EType::TApply(fail, start))),
                    },
                }
            }
        },
        _ => return Err((NoProgress, EType::TStart(start))),
    };

    let type_ann = with_spaces_before(arena, type_ann, spaces);
    Ok((MadeProgress, type_ann, state))
}

fn rest_of_type_in_parens<'a>(
    start: Position,
    stop_at_first_impl: bool,
    arena: &'a Bump,
    state: State<'a>,
    min_indent: u32,
) -> ParseResult<'a, Loc<TypeAnnotation<'a>>, ETypeInParens<'a>> {
    let (_, fields, state) = skip_second(
        reset_min_indent(collection_inner(
            specialize_err_ref(ETypeInParens::Type, type_expr(true, false)),
            TypeAnnotation::SpaceBefore,
        )),
        byte(b')', ETypeInParens::End),
    )
    .parse(arena, state.clone(), min_indent)?;

    let ext_pos = state.pos();
    let (ext, state) = match parse_term(stop_at_first_impl, arena, state.clone(), min_indent) {
        Ok((_, out, state)) => (Some(&*arena.alloc(out)), state),
        Err((NoProgress, _)) => (None, state),
        Err((_, fail)) => {
            let fail = ETypeInParens::Type(arena.alloc(fail), ext_pos);
            return Err((MadeProgress, fail));
        }
    };

    let region = Region::new(start, state.pos());
    if fields.len() > 1 || ext.is_some() {
        let out = Loc::at(region, TypeAnnotation::Tuple { elems: fields, ext });
        Ok((MadeProgress, out, state))
    } else if fields.len() == 1 {
        Ok((MadeProgress, fields.items[0], state))
    } else {
        debug_assert!(fields.is_empty());
        Err((MadeProgress, ETypeInParens::Empty(state.pos())))
    }
}

fn tag_type<'a>() -> impl Parser<'a, Loc<Tag<'a>>, ETypeTagUnion<'a>> {
    move |arena, state: State<'a>, min_indent: u32| {
        let start = state.pos();
        let (name, state) = match chomp_uppercase_part(state.bytes()) {
            Ok(out) => (out, state.advance(out.len())),
            Err(p) => return Err((p, ETypeTagUnion::End(state.pos()))),
        };

        let name = Loc::pos(start, state.pos(), name);

        let args_pos = state.pos();
        let mut state = state;
        let mut args = Vec::with_capacity_in(1, arena);
        loop {
            let prev_state = state.clone();
            match loc_applied_arg(false, arena, state, min_indent) {
                Ok((_, arg, next_state)) => {
                    state = next_state;
                    args.push(arg);
                }
                Err((NoProgress, _)) => {
                    state = prev_state;
                    break;
                }
                Err((_, fail)) => {
                    let fail = ETypeTagUnion::Type(arena.alloc(fail), args_pos);
                    return Err((MadeProgress, fail));
                }
            }
        }

        let args = args.into_bump_slice();
        let result = Loc::pos(start, state.pos(), Tag::Apply { name, args });
        Ok((MadeProgress, result, state))
    }
}

fn record_type_field<'a>() -> impl Parser<'a, AssignedField<'a, TypeAnnotation<'a>>, ETypeRecord<'a>>
{
    use AssignedField::*;

    (move |arena, state: State<'a>, min_indent: u32| {
        // You must have a field name, e.g. "email"
        // using the initial pos is important for error reporting
        let pos = state.pos();
        let (progress, loc_label, state) = loc(specialize_err(
            move |_, _| ETypeRecord::Field(pos),
            lowercase_ident_keyword_e(),
        ))
        .parse(arena, state, min_indent)?;
        debug_assert_eq!(progress, MadeProgress);

        let (_, spaces, state) = parse_space(ETypeRecord::IndentEnd, arena, state, min_indent)?;

        // Having a value is optional; both `{ email }` and `{ email: blah }` work.
        // (This is true in both literals and types.)
        if state.bytes().first() == Some(&b':') {
            let (_, loc_val, state) = space0_before_e(
                specialize_err_ref(ETypeRecord::Type, type_expr(true, false)),
                ETypeRecord::IndentColon,
            )
            .parse(arena, state.inc(), min_indent)?;

            let req_val = RequiredValue(loc_label, spaces, arena.alloc(loc_val));
            Ok((MadeProgress, req_val, state))
        } else if state.bytes().first() == Some(&b'?') {
            let (_, loc_val, state) = space0_before_e(
                specialize_err_ref(ETypeRecord::Type, type_expr(true, false)),
                ETypeRecord::IndentOptional,
            )
            .parse(arena, state.inc(), min_indent)?;

            let opt_val = OptionalValue(loc_label, spaces, arena.alloc(loc_val));
            Ok((MadeProgress, opt_val, state))
        } else {
            // If no value was provided, record it as a Var.
            // Canonicalize will know what to do with a Var later.
            let value = if !spaces.is_empty() {
                SpaceAfter(arena.alloc(LabelOnly(loc_label)), spaces)
            } else {
                LabelOnly(loc_label)
            };

            Ok((MadeProgress, value, state))
        }
    })
    .trace("type_annotation:record_type_field")
}

#[inline(always)]
fn record_type<'a>(
    stop_at_first_impl: bool,
) -> impl Parser<'a, TypeAnnotation<'a>, ETypeRecord<'a>> {
    move |arena: &'a Bump, state: State<'a>, min_indent: u32| {
        let (_, fields, state) = collection_trailing_sep_e(
            byte(b'{', ETypeRecord::Open),
            loc(record_type_field()),
            byte(b'}', ETypeRecord::End),
            AssignedField::SpaceBefore,
        )
        .parse(arena, state.clone(), min_indent)?;

        let ext_pos = state.pos();
        let (ext, state) = match parse_term(stop_at_first_impl, arena, state.clone(), min_indent) {
            Ok((_, ext, state)) => (Some(&*arena.alloc(ext)), state),
            Err((NoProgress, _)) => (None, state),
            Err((_, fail)) => {
                let fail = ETypeRecord::Type(arena.alloc(fail), ext_pos);
                return Err((MadeProgress, fail));
            }
        };

        let record = TypeAnnotation::Record { fields, ext };
        Ok((MadeProgress, record, state))
    }
}

fn applied_type<'a>(stop_at_first_impl: bool) -> impl Parser<'a, TypeAnnotation<'a>, EType<'a>> {
    map(
        indented_seq(
            specialize_err(EType::TApply, concrete_type()),
            // Optionally parse space-separated arguments for the constructor,
            // e.g. `Str Float` in `Map Str Float`
            loc_applied_args_e(stop_at_first_impl),
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
        },
    )
    .trace("type_annotation:applied_type")
}

fn loc_applied_args_e<'a>(
    stop_at_first_impl: bool,
) -> impl Parser<'a, Vec<'a, Loc<TypeAnnotation<'a>>>, EType<'a>> {
    move |arena, mut state: State<'a>, min_indent: u32| {
        let mut buf = Vec::with_capacity_in(1, arena);
        loop {
            let prev_state = state.clone();
            match loc_applied_arg(stop_at_first_impl, arena, state, min_indent) {
                Ok((_, next_elem, next_state)) => {
                    state = next_state;
                    buf.push(next_elem);
                }
                Err((NoProgress, _)) => {
                    break Ok((Progress::when(buf.len() != 0), buf, prev_state))
                }
                Err(err) => break Err(err),
            }
        }
    }
}

fn implements_clause<'a>() -> impl Parser<'a, Loc<ImplementsClause<'a>>, EType<'a>> {
    move |arena: &'a Bump, state: State<'a>, min_indent: u32| {
        let (sp_p, spaces_before, state) =
            eat_space_check(EType::TIndentStart, arena, state, min_indent, false)?;

        let ident_pos = state.pos();
        let (ident, state) = match parse_lowercase_ident(state) {
            Ok((_, out, state)) => (out, state),
            Err((ep, _)) => return Err((ep.or(sp_p), EType::TBadTypeVariable(ident_pos))),
        };

        let ident = Loc::pos(ident_pos, state.pos(), Spaced::Item(ident));

        let (_, spaces_after, state) =
            eat_space_check(EType::TIndentEnd, arena, state, min_indent, true)?;

        let ident = with_spaces(arena, spaces_before, ident, spaces_after);

        if !state.bytes().starts_with(keyword::IMPLEMENTS.as_bytes()) {
            return Err((MadeProgress, EType::TImplementsClause(state.pos())));
        }
        let state = state.advance(keyword::IMPLEMENTS.len());

        // Parse ability chain e.g. `Hash & Eq &..`, this may be qualified from another module like `Hash.Hash`
        let min_indent = state.column() + 1;
        let (_, spaces_before, state) =
            eat_space_check(EType::TIndentStart, arena, state, min_indent, true)?;

        let first_pos = state.pos();
        let (first_ability, state) = match concrete_type().parse(arena, state, min_indent) {
            Ok((_, out, state)) => (out, state),
            Err((_, fail)) => return Err((MadeProgress, EType::TApply(fail, first_pos))),
        };

        let mut last_ability_at = Region::new(first_pos, state.pos());
        let mut first_ability = Loc::at(last_ability_at, first_ability);

        let (spaces_after, state) =
            match eat_space_check(EType::TIndentEnd, arena, state.clone(), min_indent, false) {
                Ok((_, sp, state)) => (sp, state),
                Err(_) => (&[] as &[_], state),
            };

        first_ability = with_spaces(arena, spaces_before, first_ability, spaces_after);

        let mut abilities = Vec::with_capacity_in(1, arena);
        abilities.push(first_ability);

        let mut state = state;
        loop {
            if state.bytes().first() != Some(&b'&') {
                break;
            }

            let news = state.inc();
            let (_, spaces_before, news) =
                eat_space_check(EType::TIndentStart, arena, news, min_indent, true)?;

            let ability_pos = news.pos();
            let (ability, news) = match concrete_type().parse(arena, news, min_indent) {
                Ok((_, out, state)) => (out, state),
                Err((_, fail)) => return Err((MadeProgress, EType::TApply(fail, ability_pos))),
            };

            last_ability_at = Region::new(ability_pos, news.pos());
            let mut ability = Loc::at(last_ability_at, ability);

            let (spaces_after, news) =
                match eat_space_check(EType::TIndentEnd, arena, news.clone(), min_indent, false) {
                    Ok((_, sp, state)) => (sp, state),
                    Err(_) => (&[] as &[_], news),
                };

            ability = with_spaces(arena, spaces_before, ability, spaces_after);
            abilities.push(ability);
            state = news;
        }

        let region: Region = Region::span_across(&ident.region, &last_ability_at);
        let implements = ImplementsClause {
            var: ident,
            abilities: abilities.into_bump_slice(),
        };
        Ok((MadeProgress, Loc::at(region, implements), state))
    }
}

/// Parse a implements-abilities clause, e.g. `implements [Eq, Hash]`.
pub fn implements_abilities<'a>() -> impl Parser<'a, Loc<ImplementsAbilities<'a>>, EType<'a>> {
    increment_min_indent(skip_first(
        // Parse "implements"; we don't care about this keyword
        word(crate::keyword::IMPLEMENTS, EType::TImplementsClause),
        // Parse "Hash"; this may be qualified from another module like "Hash.Hash"
        space0_before_e(
            loc(map(
                collection_trailing_sep_e(
                    byte(b'[', EType::TStart),
                    loc(parse_implements_ability()),
                    byte(b']', EType::TEnd),
                    ImplementsAbility::SpaceBefore,
                ),
                ImplementsAbilities::Implements,
            )),
            EType::TIndentEnd,
        ),
    ))
}

fn parse_implements_ability<'a>() -> impl Parser<'a, ImplementsAbility<'a>, EType<'a>> {
    increment_min_indent(record!(ImplementsAbility::ImplementsAbility {
        ability: loc(specialize_err(EType::TApply, concrete_type())),
        impls: optional(backtrackable(space0_before_e(
            loc(map(
                specialize_err(
                    EType::TAbilityImpl,
                    collection_trailing_sep_e(
                        byte(b'{', ETypeAbilityImpl::Open),
                        specialize_err(|e: ERecord<'_>, _| e.into(), loc(ability_impl_field())),
                        byte(b'}', ETypeAbilityImpl::End),
                        AssignedField::SpaceBefore
                    )
                ),
                AbilityImpls::AbilityImpls
            )),
            EType::TIndentEnd
        )))
    }))
}

fn ability_impl_field<'a>() -> impl Parser<'a, AssignedField<'a, Expr<'a>>, ERecord<'a>> {
    move |arena: &'a Bump, state: State<'a>, min_indent: u32| {
        let (_, field, state) = parse_record_field(arena, state, min_indent)?;
        match field.to_assigned_field(arena) {
            AssignedField::IgnoredValue(_, _, _) => {
                Err((MadeProgress, ERecord::Field(state.pos())))
            }
            assigned_field => Ok((MadeProgress, assigned_field, state)),
        }
    }
}

pub(crate) fn type_expr<'a>(
    is_trailing_comma_valid: bool,
    stop_at_first_impl: bool,
) -> impl Parser<'a, Loc<TypeAnnotation<'a>>, EType<'a>> {
    (move |arena, state: State<'a>, min_indent: u32| {
        // todo: @wip in some calls to expression we already checking for space before via the same function. Remove double check!

        let (sp_p, spaces_before, state) =
            eat_space_check(EType::TIndentStart, arena, state, min_indent, false)?;

        let (_, first_type, state) = match parse_term(stop_at_first_impl, arena, state, min_indent)
        {
            Ok(ok) => ok,
            Err((p, fail)) => return Err((p.or(sp_p), fail)),
        };

        let first_type = with_spaces_before(arena, first_type, spaces_before);

        let first_state = state.clone();
        let mut state = state;
        let mut more_args = Vec::with_capacity_in(1, arena);
        let more_args_res = loop {
            if state.bytes().first() != Some(&b',') {
                // if no more type args then add the space after the first type annotation here
                let (p, sp_after_single_ann, state) = if more_args.len() == 0 {
                    match eat_space_check(EType::TIndentStart, arena, state, min_indent, false) {
                        Ok((_, sp, state)) => (NoProgress, sp, state),
                        Err(err) => break Err(err),
                    }
                } else {
                    (MadeProgress, &[] as &[_], state)
                };

                break if state.bytes().starts_with(b"->") {
                    let state = state.advance(2);
                    Ok((MadeProgress, (more_args, sp_after_single_ann), state))
                } else {
                    Err((p, EType::TStart(state.pos())))
                };
            }

            let news = state.inc();
            let space_pos = news.pos();
            let (spaces_before, news) =
                match eat_space_check(EType::TIndentStart, arena, news, min_indent, false) {
                    Ok((_, sp, news)) => (sp, news),
                    Err((NoProgress, _)) => {
                        break Err((MadeProgress, EType::TFunctionArgument(space_pos)))
                    }
                    Err(err) => break Err(err),
                };

            let arg_pos = news.pos();
            let (_, arg, news) = match parse_term(stop_at_first_impl, arena, news, min_indent) {
                Ok(ok) => ok,
                Err((NoProgress, _)) => {
                    break Err((MadeProgress, EType::TFunctionArgument(arg_pos)))
                }
                Err(err) => break Err(err),
            };

            let (spaces_after, news) =
                match eat_space_check(EType::TIndentEnd, arena, news, min_indent, true) {
                    Ok((_, sp, news)) => (sp, news),
                    Err(err) => break Err(err),
                };

            let arg = with_spaces(arena, spaces_before, arg, spaces_after);
            more_args.push(arg);
            state = news;
        };

        let (types_pr, types, state) = match more_args_res {
            Ok((_, (more_args, sp_after_single_ann), state)) => {
                let (p, spaces_before_ret, state) =
                    eat_space_check(EType::TIndentStart, arena, state, min_indent, false)?;

                let (_, return_type, state) =
                    match parse_term(stop_at_first_impl, arena, state, min_indent) {
                        Ok(ok) => ok,
                        Err((ep, fail)) => return Err((ep.or(p), fail)),
                    };

                let return_type = with_spaces_before(arena, return_type, spaces_before_ret);
                let region = Region::span_across(&first_type.region, &return_type.region);

                // prepare arguments
                let mut arguments = Vec::with_capacity_in(more_args.len() + 1, arena);
                arguments.push(first_type);
                arguments.extend(more_args);
                // add space to the single type argument only if it is part of the function signature, and not a standalone type
                if !sp_after_single_ann.is_empty() {
                    debug_assert!(arguments.len() == 1);
                    if let Some(last) = arguments.last_mut() {
                        let new_value = arena.alloc(last.value).after(sp_after_single_ann);
                        last.value = new_value;
                    }
                }

                let args_out = arena.alloc(arguments);
                let result = TypeAnnotation::Function(args_out, arena.alloc(return_type));
                let result = Loc::at(region, result);

                (MadeProgress, result, state)
            }
            Err(err) => {
                if !is_trailing_comma_valid {
                    if let Ok((.., state)) = eat_space_check(
                        EType::TIndentStart,
                        arena,
                        first_state.clone(),
                        min_indent,
                        false,
                    ) {
                        if state.bytes().first() == Some(&b',') {
                            // If the surrounding scope has declared that a trailing comma is not a valid state
                            // for a type annotation - and we found one anyway - return an error so that we can
                            // produce a more useful error message, knowing that the user was probably writing a
                            // function type and messed up the syntax somehow.
                            return Err(err);
                        }
                    }
                }

                // We ran into trouble parsing the function bits; just return the single term
                (MadeProgress, first_type, first_state)
            }
        };

        // Finally, try to parse a where clause if there is one or more,
        // e.g. " where a implements Hash, b implements Eq".
        // The where clause must be at least as deep as where the type annotation started.
        let types_state = state.clone();

        let (spaces_before, mut state) =
            match eat_space_check(EType::TIndentStart, arena, state, min_indent, true) {
                Ok((_, sp, state)) => (sp, state),
                Err(_) => return Ok((types_pr, types, types_state)),
            };

        if !state.bytes().starts_with(crate::keyword::WHERE.as_bytes()) {
            return Ok((types_pr, types, types_state));
        }
        state.advance_mut(keyword::WHERE.len());

        // Parse the first clause (there must be one), then the rest
        let (first_impl, mut state) = match implements_clause().parse(arena, state, min_indent) {
            Ok((_, out, state)) => (out, state),
            Err(_) => return Ok((types_pr, types, types_state)),
        };

        let mut implements = Vec::with_capacity_in(1, arena);
        implements.push(first_impl);
        loop {
            let prev_state = state.clone();
            match skip_first(byte(b',', EType::TImplementsClause), implements_clause())
                .parse(arena, state, min_indent)
            {
                Ok((_, next_impl, next_state)) => {
                    state = next_state;
                    implements.push(next_impl);
                }
                Err((NoProgress, _)) => {
                    state = prev_state;
                    break;
                }
                Err(_) => return Ok((types_pr, types, types_state)),
            }
        }

        let implements = implements.into_bump_slice();
        let region = Region::span_across(&types.region, &implements.last().unwrap().region);

        // We're transforming the spaces_before the 'where'
        // into spaces_after the thing before the 'where'
        let types = with_spaces_after(arena, types, spaces_before);
        let types = &*arena.alloc(types);

        let types_where = TypeAnnotation::Where(types, implements);
        Ok((MadeProgress, Loc::at(region, types_where), state))
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
    move |_: &'a Bump, state: State<'a>, _: u32| {
        let initial_bytes = state.bytes();

        match chomp_concrete_type(state.bytes()) {
            Ok((module_name, type_name, width)) => {
                let answer = TypeAnnotation::Apply(module_name, type_name, &[]);
                Ok((MadeProgress, answer, state.advance(width)))
            }
            Err(NoProgress) => Err((NoProgress, ETypeApply::End(state.pos()))),
            Err(_) => {
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
