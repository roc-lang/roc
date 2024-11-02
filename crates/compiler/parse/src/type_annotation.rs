use crate::ast::{
    AbilityImpls, AssignedField, Expr, ImplementsAbilities, ImplementsAbility, ImplementsClause,
    Pattern, Spaceable, Spaced, Tag, TypeAnnotation, TypeHeader,
};
use crate::blankspace::{eat_nc_check, SpacedBuilder};
use crate::expr::parse_record_field;
use crate::ident::{
    chomp_concrete_type, chomp_lowercase_part, chomp_uppercase_part, parse_lowercase_ident,
};
use crate::keyword::{self, is_keyword};
use crate::parser::{
    at_keyword, collection_inner, loc, ERecord, EType, ETypeApply, ETypeInParens, ETypeInlineAlias,
    ETypeRecord, ETypeTagUnion, ParseResult, Parser, Progress::*,
};
use crate::state::State;
use bumpalo::collections::vec::Vec;
use bumpalo::Bump;
use roc_region::all::{Loc, Position, Region};

fn rest_of_tag_union_type<'a>(
    flags: TypeExprFlags,
    arena: &'a Bump,
    state: State<'a>,
    min_indent: u32,
) -> ParseResult<'a, TypeAnnotation<'a>, ETypeTagUnion<'a>> {
    let (tags, state) = match collection_inner(tag_type(), Tag::SpaceBefore).parse(arena, state, 0)
    {
        Ok((_, out, state)) => (out, state),
        Err((_, fail)) => return Err((MadeProgress, fail)),
    };

    if state.bytes().first() != Some(&b']') {
        return Err((MadeProgress, ETypeTagUnion::End(state.pos())));
    }
    let state = state.inc();

    // This could be an open tag union, e.g. `[Foo, Bar]a`
    let ext_pos = state.pos();
    let (ext, state) = match parse_term(flags, arena, state.clone(), min_indent) {
        Ok((_, out, state)) => (Some(&*arena.alloc(out)), state),
        Err((NoProgress, _)) => (None, state),
        Err((_, fail)) => {
            let fail = ETypeTagUnion::Type(arena.alloc(fail), ext_pos);
            return Err((MadeProgress, fail));
        }
    };

    let out = TypeAnnotation::TagUnion { tags, ext };
    Ok((MadeProgress, out, state))
}

fn check_type_alias<'a>(
    arena: &'a Bump,
    annot: Loc<TypeAnnotation<'a>>,
) -> Result<TypeHeader<'a>, ETypeInlineAlias> {
    let start = annot.region.start();
    match annot.value {
        TypeAnnotation::Apply("", tag_name, vars) => {
            let mut var_names: Vec<'_, Loc<Pattern<'_>>> = Vec::new_in(arena);
            var_names.reserve(vars.len());
            for var in vars {
                if let TypeAnnotation::BoundVariable(v) = var.value {
                    var_names.push(Loc::at(var.region, Pattern::Identifier { ident: v }));
                } else {
                    return Err(ETypeInlineAlias::ArgumentNotLowercase(var.region.start()));
                }
            }

            let name_at = Region::between(start, start.bump_column(tag_name.len() as u32));
            let name = Loc::at(name_at, tag_name);
            let vars = var_names.into_bump_slice();
            Ok(TypeHeader { name, vars })
        }
        TypeAnnotation::Apply(_, _, _) => Err(ETypeInlineAlias::Qualified(start)),
        _ => Err(ETypeInlineAlias::NotAnAlias(start)),
    }
}

fn parse_term<'a>(
    flags: TypeExprFlags,
    arena: &'a Bump,
    state: State<'a>,
    min_indent: u32,
) -> ParseResult<'a, Loc<TypeAnnotation<'a>>, EType<'a>> {
    let parse_applied_arg = flags.is_set(PARSE_APPLIED_ARG);
    let flags = flags.without(PARSE_APPLIED_ARG);

    let start = state.pos();
    let (type_ann, state) = match state.bytes().first() {
        Some(b) => match b {
            b'(' => match rest_of_type_in_parens(start, flags, arena, state.inc(), min_indent) {
                Ok((_, out, state)) => (out, state),
                Err((p, fail)) => return Err((p, EType::TInParens(fail, start))),
            },
            b'{' => match rest_of_record_type(flags, arena, state.inc(), min_indent) {
                Ok((_, out, state)) => (Loc::pos(start, state.pos(), out), state),
                Err((p, fail)) => return Err((p, EType::TRecord(fail, start))),
            },
            b'[' => match rest_of_tag_union_type(flags, arena, state.inc(), min_indent) {
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
                // The `_` indicating an inferred type, e.g. in (List _)
                let out = Loc::pos(start, start.next(), TypeAnnotation::Inferred);
                (out, state.inc())
            }
            _ => {
                // todo: @perf can we use the same check as for Inferred above to quickly check for one character identifier, plus it's not required to lookup for it in keywords, cause they start from 2 chars?
                let ident_res = match parse_lowercase_ident(state.clone()) {
                    Ok((_, name, state)) => {
                        if name == keyword::WHERE
                            || (flags.is_set(STOP_AT_FIRST_IMPL) && name == keyword::IMPLEMENTS)
                        {
                            None
                        } else {
                            let type_ann = TypeAnnotation::BoundVariable(name);
                            let type_ann = Loc::pos(start, state.pos(), type_ann);
                            Some((type_ann, state))
                        }
                    }
                    Err((MadeProgress, _)) => {
                        return Err((MadeProgress, EType::TBadTypeVariable(start)))
                    }
                    Err(_) => None,
                };

                match ident_res {
                    Some(ok) => ok,
                    None => {
                        if parse_applied_arg {
                            match parse_concrete_type(state) {
                                Ok((_, out, state)) => (Loc::pos(start, state.pos(), out), state),
                                Err((NoProgress, _)) => {
                                    return Err((NoProgress, EType::TStart(start)))
                                }
                                Err((_, fail)) => {
                                    return Err((MadeProgress, EType::TApply(fail, start)))
                                }
                            }
                        } else {
                            match parse_applied_type(flags, arena, state) {
                                Ok((_, ann, state)) => (Loc::pos(start, state.pos(), ann), state),
                                Err((NoProgress, _)) => {
                                    return Err((NoProgress, EType::TStart(start)))
                                }
                                Err(err) => return Err(err),
                            }
                        }
                    }
                }
            }
        },
        _ => return Err((NoProgress, EType::TStart(start))),
    };

    if parse_applied_arg {
        return Ok((MadeProgress, type_ann, state));
    }

    let type_ann_state = state.clone();
    let (spaces_before_as, state) =
        match eat_nc_check(EType::TIndentEnd, arena, state, min_indent, false) {
            Ok((_, sp, state)) => (sp, state),
            Err(_) => return Ok((MadeProgress, type_ann, type_ann_state)),
        };

    if !at_keyword(keyword::AS, &state) {
        return Ok((MadeProgress, type_ann, type_ann_state));
    }
    let state = state.advance(keyword::AS.len());

    let (sp, spaces_after_as, state) =
        eat_nc_check(EType::TAsIndentStart, arena, state, min_indent, false)?;

    let (mut alias_ann, state) = match parse_term(NO_TYPE_EXPR_FLAGS, arena, state, min_indent) {
        Ok((_, out, state)) => (out, state),
        Err((ep, fail)) => return Err((ep.or(sp), fail)),
    };

    alias_ann = alias_ann.spaced_before(arena, spaces_after_as);

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

fn rest_of_type_in_parens<'a>(
    start: Position,
    flags: TypeExprFlags,
    arena: &'a Bump,
    state: State<'a>,
    min_indent: u32,
) -> ParseResult<'a, Loc<TypeAnnotation<'a>>, ETypeInParens<'a>> {
    let elem_p = move |a: &'a Bump, state: State<'a>, min_indent: u32| {
        let type_pos = state.pos();
        match type_expr(TRAILING_COMMA_VALID | SKIP_PARSING_SPACES_BEFORE)
            .parse(a, state, min_indent)
        {
            Ok(ok) => Ok(ok),
            Err((p, fail)) => Err((p, ETypeInParens::Type(a.alloc(fail), type_pos))),
        }
    };
    let (fields, state) =
        match collection_inner(elem_p, TypeAnnotation::SpaceBefore).parse(arena, state, 0) {
            Ok((_, out, state)) => (out, state),
            Err((_, fail)) => return Err((MadeProgress, fail)),
        };

    if state.bytes().first() != Some(&b')') {
        return Err((MadeProgress, ETypeInParens::End(state.pos())));
    }
    let state = state.inc();

    let ext_pos = state.pos();
    let (ext, state) = match parse_term(flags, arena, state.clone(), min_indent) {
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

            let (spaces_before, next_state) =
                match eat_nc_check(EType::TIndentStart, arena, state, min_indent, false) {
                    Ok((_, sp, st)) => (sp, st),
                    Err(_) => {
                        state = prev_state;
                        break;
                    }
                };

            match parse_term(PARSE_APPLIED_ARG, arena, next_state, min_indent) {
                Ok((_, type_ann, next_state)) => {
                    let type_ann = type_ann.spaced_before(arena, spaces_before);
                    args.push(type_ann);
                    state = next_state;
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

fn record_type_field<'a>(
) -> impl Parser<'a, Loc<AssignedField<'a, TypeAnnotation<'a>>>, ETypeRecord<'a>> {
    use AssignedField::*;
    move |arena, state: State<'a>, min_indent: u32| {
        // You must have a field name, e.g. "email"
        // using the initial pos is important for error reporting
        let start = state.pos();
        let (label, state) = match chomp_lowercase_part(state.bytes()) {
            Err(p) => return Err((p, ETypeRecord::Field(start))),
            Ok(ident) => {
                if is_keyword(ident) {
                    return Err((MadeProgress, ETypeRecord::Field(start)));
                }
                (ident, state.advance(ident.len()))
            }
        };

        let loc_label = Loc::pos(start, state.pos(), label);

        let (_, spaces, state) =
            eat_nc_check(ETypeRecord::IndentEnd, arena, state, min_indent, false)?;

        // Having a value is optional; both `{ email }` and `{ email: blah }` work.
        // (This is true in both literals and types.)
        if state.bytes().first() == Some(&b':') {
            let state = state.inc();
            let (sp, spaces_before, state) =
                eat_nc_check(ETypeRecord::IndentColon, arena, state, min_indent, false)?;

            let val_pos = state.pos();
            let (loc_val, state) =
                match type_expr(TRAILING_COMMA_VALID | SKIP_PARSING_SPACES_BEFORE)
                    .parse(arena, state, min_indent)
                {
                    Ok((_, out, state)) => (out, state),
                    Err((ep, fail)) => {
                        return Err((ep.or(sp), ETypeRecord::Type(arena.alloc(fail), val_pos)))
                    }
                };

            let loc_val = loc_val.spaced_before(arena, spaces_before);
            let req_val = RequiredValue(loc_label, spaces, arena.alloc(loc_val));
            let req_val = Loc::pos(start, state.pos(), req_val);
            Ok((MadeProgress, req_val, state))
        } else if state.bytes().first() == Some(&b'?') {
            let state = state.inc();
            let (sp, spaces_before, state) =
                eat_nc_check(ETypeRecord::IndentOptional, arena, state, min_indent, false)?;

            let val_pos = state.pos();
            let (loc_val, state) =
                match type_expr(TRAILING_COMMA_VALID | SKIP_PARSING_SPACES_BEFORE)
                    .parse(arena, state, min_indent)
                {
                    Ok((_, out, state)) => (out, state),
                    Err((ep, fail)) => {
                        return Err((ep.or(sp), ETypeRecord::Type(arena.alloc(fail), val_pos)))
                    }
                };
            let loc_val = loc_val.spaced_before(arena, spaces_before);

            let opt_val = OptionalValue(loc_label, spaces, arena.alloc(loc_val));
            let opt_val = Loc::pos(start, state.pos(), opt_val);
            Ok((MadeProgress, opt_val, state))
        } else {
            // If no value was provided, record it as a Var.
            // Canonicalize will know what to do with a Var later.
            let value = if !spaces.is_empty() {
                SpaceAfter(arena.alloc(LabelOnly(loc_label)), spaces)
            } else {
                LabelOnly(loc_label)
            };

            let value = Loc::pos(start, state.pos(), value);
            Ok((MadeProgress, value, state))
        }
    }
}

fn rest_of_record_type<'a>(
    flags: TypeExprFlags,
    arena: &'a Bump,
    state: State<'a>,
    min_indent: u32,
) -> ParseResult<'a, TypeAnnotation<'a>, ETypeRecord<'a>> {
    let (fields, state) = match collection_inner(record_type_field(), AssignedField::SpaceBefore)
        .parse(arena, state, 0)
    {
        Ok((_, out, state)) => (out, state),
        Err((_, fail)) => return Err((MadeProgress, fail)),
    };

    if state.bytes().first() != Some(&b'}') {
        return Err((MadeProgress, ETypeRecord::End(state.pos())));
    }
    let state = state.inc();

    let ext_pos = state.pos();
    let (ext, state) = match parse_term(flags, arena, state.clone(), min_indent) {
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

fn parse_applied_type<'a>(
    flags: TypeExprFlags,
    arena: &'a Bump,
    state: State<'a>,
) -> ParseResult<'a, TypeAnnotation<'a>, EType<'a>> {
    let start_indent = state.line_indent();
    let start = state.pos();
    let (ctor, state) = match parse_concrete_type(state) {
        Ok((_, out, state)) => (out, state),
        Err((p, fail)) => return Err((p, EType::TApply(fail, start))),
    };

    // Optionally parse space-separated arguments for the constructor,
    // e.g. `Str Float` in `Map Str Float`
    let inc_indent = start_indent + 1;
    let mut state = state;
    let mut args = Vec::with_capacity_in(1, arena);
    loop {
        let prev_state = state.clone();

        let (spaces_before, next_state) =
            match eat_nc_check(EType::TIndentStart, arena, state, inc_indent, false) {
                Ok((_, sp, st)) => (sp, st),
                Err(_) => {
                    state = prev_state;
                    break;
                }
            };

        match parse_term(flags | PARSE_APPLIED_ARG, arena, next_state, inc_indent) {
            Ok((_, type_ann, next_state)) => {
                let type_ann = type_ann.spaced_before(arena, spaces_before);
                args.push(type_ann);
                state = next_state;
            }
            Err((NoProgress, _)) => {
                state = prev_state;
                break;
            }
            Err((_, fail)) => return Err((MadeProgress, fail)),
        }
    }

    let type_ann = match &ctor {
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
    };
    Ok((MadeProgress, type_ann, state))
}

fn parse_implements_clause<'a>(
    arena: &'a Bump,
    state: State<'a>,
    min_indent: u32,
) -> ParseResult<'a, Loc<ImplementsClause<'a>>, EType<'a>> {
    let (sp_p, spaces_before, state) =
        eat_nc_check(EType::TIndentStart, arena, state, min_indent, false)?;

    let ident_pos = state.pos();
    let (ident, state) = match parse_lowercase_ident(state) {
        Ok((_, out, state)) => (out, state),
        Err((ep, _)) => return Err((ep.or(sp_p), EType::TBadTypeVariable(ident_pos))),
    };

    let ident = Loc::pos(ident_pos, state.pos(), Spaced::Item(ident));

    let (_, spaces_after, state) = eat_nc_check(EType::TIndentEnd, arena, state, min_indent, true)?;

    let ident = ident.spaced_around(arena, spaces_before, spaces_after);

    if !state.bytes().starts_with(keyword::IMPLEMENTS.as_bytes()) {
        return Err((MadeProgress, EType::TImplementsClause(state.pos())));
    }
    let state = state.advance(keyword::IMPLEMENTS.len());

    // Parse ability chain e.g. `Hash & Eq &..`, this may be qualified from another module like `Hash.Hash`
    let min_indent = state.column() + 1;
    let (_, spaces_before, state) =
        eat_nc_check(EType::TIndentStart, arena, state, min_indent, true)?;

    let first_pos = state.pos();
    let (first_ability, state) = match parse_concrete_type(state) {
        Ok((_, out, state)) => (out, state),
        Err((_, fail)) => return Err((MadeProgress, EType::TApply(fail, first_pos))),
    };

    let mut last_ability_at = Region::new(first_pos, state.pos());
    let mut first_ability = Loc::at(last_ability_at, first_ability);

    let (spaces_after, state) =
        match eat_nc_check(EType::TIndentEnd, arena, state.clone(), min_indent, false) {
            Ok((_, sp, state)) => (sp, state),
            Err(_) => (&[] as &[_], state),
        };

    first_ability = first_ability.spaced_around(arena, spaces_before, spaces_after);

    let mut abilities = Vec::with_capacity_in(1, arena);
    abilities.push(first_ability);

    let mut state = state;
    loop {
        if state.bytes().first() != Some(&b'&') {
            break;
        }

        let news = state.inc();
        let (_, spaces_before, news) =
            eat_nc_check(EType::TIndentStart, arena, news, min_indent, true)?;

        let ability_pos = news.pos();
        let (ability, news) = match parse_concrete_type(news) {
            Ok((_, out, state)) => (out, state),
            Err((_, fail)) => return Err((MadeProgress, EType::TApply(fail, ability_pos))),
        };

        last_ability_at = Region::new(ability_pos, news.pos());
        let mut ability = Loc::at(last_ability_at, ability);

        let (spaces_after, news) =
            match eat_nc_check(EType::TIndentEnd, arena, news.clone(), min_indent, false) {
                Ok((_, sp, state)) => (sp, state),
                Err(_) => (&[] as &[_], news),
            };

        ability = ability.spaced_around(arena, spaces_before, spaces_after);
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

/// Parse a implements-abilities clause, e.g. `implements [Eq, Hash]`.
pub fn parse_implements_abilities<'a>(
    arena: &'a Bump,
    state: State<'a>,
    min_indent: u32,
) -> ParseResult<'a, Loc<ImplementsAbilities<'a>>, EType<'a>> {
    if !state.bytes().starts_with(keyword::IMPLEMENTS.as_bytes()) {
        return Err((NoProgress, EType::TImplementsClause(state.pos())));
    }
    let state = state.advance(keyword::IMPLEMENTS.len());

    let inc_indent = min_indent + 1;
    let (_, spaces_after_impl, state) =
        eat_nc_check(EType::TIndentEnd, arena, state, inc_indent, true)?;

    let ab_pos = state.pos();
    if state.bytes().first() != Some(&b'[') {
        return Err((MadeProgress, EType::TStart(ab_pos)));
    }
    let state = state.inc();

    let (abilities, state) = match collection_inner(
        loc(parse_implements_ability()),
        ImplementsAbility::SpaceBefore,
    )
    .parse(arena, state, 0)
    {
        Ok((_, out, state)) => (out, state),
        Err((_, fail)) => return Err((MadeProgress, fail)),
    };

    if state.bytes().first() != Some(&b']') {
        return Err((MadeProgress, EType::TEnd(state.pos())));
    }
    let state = state.inc();

    let abilities = ImplementsAbilities::Implements(abilities);
    let abilities = Loc::pos(ab_pos, state.pos(), abilities);
    let abilities = abilities.spaced_before(arena, spaces_after_impl);
    Ok((MadeProgress, abilities, state))
}

fn parse_implements_ability<'a>() -> impl Parser<'a, ImplementsAbility<'a>, EType<'a>> {
    move |arena: &'a Bump, state: State<'a>, min_indent: u32| {
        let start = state.pos();
        let inc_indent = min_indent + 1;
        let (type_ann, state) = match parse_concrete_type(state) {
            Ok((_, out, state)) => (out, state),
            Err((p, fail)) => return Err((p, EType::TApply(fail, start))),
        };

        let ability = Loc::pos(start, state.pos(), type_ann);

        let olds = state.clone();
        let (impls, state) = match eat_nc_check(EType::TIndentEnd, arena, state, inc_indent, false)
        {
            Err(_) => (None, olds),
            Ok((_, spaces_before, state)) => {
                let impls_pos = state.pos();
                if state.bytes().first() != Some(&b'{') {
                    (None, olds)
                } else {
                    let state = state.inc();
                    match collection_inner(ability_impl_field(), AssignedField::SpaceBefore)
                        .parse(arena, state, 0)
                    {
                        Err(_) => (None, olds),
                        Ok((_, impls, state)) => {
                            if state.bytes().first() != Some(&b'}') {
                                (None, olds)
                            } else {
                                let state = state.inc();
                                let impls = AbilityImpls::AbilityImpls(impls);
                                let impls = Loc::pos(impls_pos, state.pos(), impls);
                                (Some(impls.spaced_before(arena, spaces_before)), state)
                            }
                        }
                    }
                }
            }
        };

        let out = ImplementsAbility::ImplementsAbility { ability, impls };
        Ok((MadeProgress, out, state))
    }
}

fn ability_impl_field<'a>() -> impl Parser<'a, Loc<AssignedField<'a, Expr<'a>>>, ERecord<'a>> {
    move |arena: &'a Bump, state: State<'a>, min_indent: u32| {
        let start = state.pos();
        let (_, field, state) = parse_record_field(arena, state, min_indent)?;
        match field.to_assigned_field(arena) {
            AssignedField::IgnoredValue(_, _, _) => {
                Err((MadeProgress, ERecord::Field(state.pos())))
            }
            assigned_field => {
                let assigned_field = Loc::pos(start, state.pos(), assigned_field);
                Ok((MadeProgress, assigned_field, state))
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TypeExprFlags(u8);

pub const NO_TYPE_EXPR_FLAGS: TypeExprFlags = TypeExprFlags(0);
pub const TRAILING_COMMA_VALID: TypeExprFlags = TypeExprFlags(1);
pub const STOP_AT_FIRST_IMPL: TypeExprFlags = TypeExprFlags(1 << 1);
pub const SKIP_PARSING_SPACES_BEFORE: TypeExprFlags = TypeExprFlags(1 << 2);
pub const PARSE_APPLIED_ARG: TypeExprFlags = TypeExprFlags(1 << 3);

impl TypeExprFlags {
    pub const fn is_set(&self, flag: Self) -> bool {
        (self.0 & flag.0) != 0
    }

    pub const fn without(&self, flag: Self) -> Self {
        Self(self.0 & !flag.0)
    }
}

impl std::ops::BitOr for TypeExprFlags {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {
        Self(self.0 | rhs.0)
    }
}

pub(crate) fn type_expr<'a>(
    flags: TypeExprFlags,
) -> impl Parser<'a, Loc<TypeAnnotation<'a>>, EType<'a>> {
    move |arena, state: State<'a>, min_indent: u32| {
        let (first_type, state) = if flags.is_set(SKIP_PARSING_SPACES_BEFORE) {
            let (_, first_type, state) = parse_term(flags, arena, state, min_indent)?;
            (first_type, state)
        } else {
            let (sp_p, spaces_before, state) =
                eat_nc_check(EType::TIndentStart, arena, state, min_indent, false)?;

            let (_, first_type, state) = match parse_term(flags, arena, state, min_indent) {
                Ok(ok) => ok,
                Err((p, fail)) => return Err((p.or(sp_p), fail)),
            };

            (first_type.spaced_before(arena, spaces_before), state)
        };

        let first_state = state.clone();
        let mut state = state;
        let mut more_args = Vec::with_capacity_in(1, arena);
        let more_args_res = loop {
            if state.bytes().first() != Some(&b',') {
                // if no more type args then add the space after the first type annotation here
                let (p, sp_after_single_ann, state) = if more_args.is_empty() {
                    match eat_nc_check(EType::TIndentStart, arena, state, min_indent, false) {
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
                match eat_nc_check(EType::TIndentStart, arena, news, min_indent, false) {
                    Ok((_, sp, news)) => (sp, news),
                    Err((NoProgress, _)) => {
                        break Err((MadeProgress, EType::TFunctionArgument(space_pos)))
                    }
                    Err(err) => break Err(err),
                };

            let arg_pos = news.pos();
            let (_, arg, news) = match parse_term(flags, arena, news, min_indent) {
                Ok(ok) => ok,
                Err((NoProgress, _)) => {
                    break Err((MadeProgress, EType::TFunctionArgument(arg_pos)))
                }
                Err(err) => break Err(err),
            };

            let (spaces_after, news) =
                match eat_nc_check(EType::TIndentEnd, arena, news, min_indent, true) {
                    Ok((_, sp, news)) => (sp, news),
                    Err(err) => break Err(err),
                };

            let arg = arg.spaced_around(arena, spaces_before, spaces_after);
            more_args.push(arg);
            state = news;
        };

        let (types_pr, types, state) = match more_args_res {
            Ok((_, (more_args, sp_after_single_ann), state)) => {
                let (p, spaces_before_ret, state) =
                    eat_nc_check(EType::TIndentStart, arena, state, min_indent, false)?;

                let (_, return_type, state) = match parse_term(flags, arena, state, min_indent) {
                    Ok(ok) => ok,
                    Err((ep, fail)) => return Err((ep.or(p), fail)),
                };

                let return_type = return_type.spaced_before(arena, spaces_before_ret);
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
                if !flags.is_set(TRAILING_COMMA_VALID) {
                    if let Ok((.., state)) = eat_nc_check(
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
            match eat_nc_check(EType::TIndentStart, arena, state, min_indent, true) {
                Ok((_, sp, state)) => (sp, state),
                Err(_) => return Ok((types_pr, types, types_state)),
            };

        if !state.bytes().starts_with(crate::keyword::WHERE.as_bytes()) {
            return Ok((types_pr, types, types_state));
        }
        state.advance_mut(keyword::WHERE.len());

        // Parse the first clause (there must be one), then the rest
        let (first_impl, mut state) = match parse_implements_clause(arena, state, min_indent) {
            Ok((_, out, state)) => (out, state),
            Err(_) => return Ok((types_pr, types, types_state)),
        };

        let mut implements = Vec::with_capacity_in(1, arena);
        implements.push(first_impl);
        loop {
            if state.bytes().first() != Some(&b',') {
                break;
            }

            match parse_implements_clause(arena, state.inc(), min_indent) {
                Ok((_, next_impl, next_state)) => {
                    state = next_state;
                    implements.push(next_impl);
                }
                Err(_) => return Ok((types_pr, types, types_state)),
            }
        }

        let implements = implements.into_bump_slice();
        let region = Region::span_across(&types.region, &implements.last().unwrap().region);

        // We're transforming the spaces_before the 'where'
        // into spaces_after the thing before the 'where'
        let types = types.spaced_after(arena, spaces_before);
        let types = &*arena.alloc(types);

        let types_where = TypeAnnotation::Where(types, implements);
        Ok((MadeProgress, Loc::at(region, types_where), state))
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

fn parse_concrete_type(state: State<'_>) -> ParseResult<'_, TypeAnnotation<'_>, ETypeApply> {
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
