use crate::ast::{Collection, Implements, Pattern, PatternAs};
use crate::blankspace::{parse_space, space0_e, spaces, spaces_before, with_spaces_after};
use crate::ident::{lowercase_ident, parse_ident, parse_lowercase_ident, Accessor, Ident};
use crate::keyword;
use crate::parser::{at_keyword, Progress::*};
use crate::parser::{
    byte, collection_inner, collection_trailing_sep_e, fail_when, loc, map, map_with_arena,
    specialize_err, specialize_err_ref, then, three_bytes, two_bytes, zero_or_more, EPattern,
    PInParens, PList, PRecord, ParseResult, Parser,
};
use crate::state::State;
use crate::string_literal::StrLikeLiteral;
use bumpalo::collections::string::String;
use bumpalo::collections::Vec;
use bumpalo::Bump;
use roc_region::all::{Loc, Region};

/// Different patterns are supported in different circumstances.
/// For example, when branches can pattern match on number literals, but
/// assignments and function args can't. Underscore is supported in function
/// arg patterns and in when branch patterns, but not in assignments.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PatternType {
    TopLevelDef,
    DefExpr,
    FunctionArg,
    WhenBranch,
    ModuleParams,
}

pub fn parse_closure_param<'a>(
    arena: &'a Bump,
    state: State<'a>,
    min_indent: u32,
) -> ParseResult<'a, Loc<Pattern<'a>>, EPattern<'a>> {
    match state.bytes().first() {
        Some(&b'_') => {
            // Underscore is also common, e.g. \_ -> ...
            parse_underscore_pattern(arena, state.clone(), min_indent)
        }
        Some(&b'{') => {
            // You can destructure records in params, e.g. \{ x, y } -> ...
            parse_record_pattern(arena, state.clone(), min_indent)
        }
        Some(&b'(') => {
            // e.g. \(User.UserId userId) -> ...
            parse_pattern_in_parens(arena, state.clone())
        }
        Some(_) => parse_ident_pattern(true, arena, state.clone(), min_indent),
        None => Err((NoProgress, EPattern::Start(state.pos()))),
    }
}

/// If Ok it always returns MadeProgress
pub fn loc_pattern_help<'a>() -> impl Parser<'a, Loc<Pattern<'a>>, EPattern<'a>> {
    move |arena, state: State<'a>, min_indent| {
        let (_, pattern, state) = parse_loc_pattern_etc(true, arena, state, min_indent)?;

        let pattern_state = state.clone();

        let (pattern_spaces, state) =
            match parse_space(EPattern::AsKeyword, arena, state, min_indent) {
                Err(_) => return Ok((MadeProgress, pattern, pattern_state)),
                Ok((_, pattern_spaces, state)) => (pattern_spaces, state),
            };

        match pattern_as().parse(arena, state, min_indent) {
            Err((progress, e)) => match progress {
                MadeProgress => Err((MadeProgress, e)),
                NoProgress => Ok((MadeProgress, pattern, pattern_state)),
            },
            Ok((_, pattern_as, state)) => {
                let region = Region::span_across(&pattern.region, &pattern_as.identifier.region);

                let pattern = with_spaces_after(pattern, pattern_spaces, arena);
                let as_pattern = Pattern::As(arena.alloc(pattern), pattern_as);

                Ok((MadeProgress, Loc::at(region, as_pattern), state))
            }
        }
    }
}

fn parse_loc_pattern_etc<'a>(
    can_have_arguments: bool,
    arena: &'a Bump,
    state: State<'a>,
    min_indent: u32,
) -> ParseResult<'a, Loc<Pattern<'a>>, EPattern<'a>> {
    let start = state.pos();
    match state.bytes().first() {
        Some(&b'_') => parse_underscore_pattern(arena, state.clone(), min_indent),
        Some(&b'{') => parse_record_pattern(arena, state.clone(), min_indent),
        Some(&b'(') => parse_pattern_in_parens(arena, state.clone()),
        Some(&b'[') => match list_pattern_help().parse(arena, state.clone(), min_indent) {
            Ok((p, expr, state)) => Ok((p, Loc::pos(start, state.pos(), expr), state)),
            Err((MadeProgress, fail)) => Err((MadeProgress, EPattern::List(fail, start))),
            Err(_) => Err((NoProgress, EPattern::Start(state.pos()))),
        },
        Some(_) => {
            match number_pattern_help().parse(arena, state.clone(), min_indent) {
                Err((NoProgress, _)) => {}
                Ok((p, expr, state)) => return Ok((p, Loc::pos(start, state.pos(), expr), state)),
                Err(fail) => return Err(fail),
            }

            match string_like_pattern_help().parse(arena, state.clone(), min_indent) {
                Err((NoProgress, _)) => {}
                Ok((p, expr, state)) => return Ok((p, Loc::pos(start, state.pos(), expr), state)),
                Err(fail) => return Err(fail),
            }

            parse_ident_pattern(can_have_arguments, arena, state.clone(), min_indent)
        }
        None => Err((NoProgress, EPattern::Start(state.pos()))),
    }
}

fn pattern_as<'a>() -> impl Parser<'a, PatternAs<'a>, EPattern<'a>> {
    move |arena, state: State<'a>, min_indent| {
        if !at_keyword(keyword::AS, &state) {
            return Err((NoProgress, EPattern::AsKeyword(state.pos())));
        }
        let state = state.advance(keyword::AS.len());

        let (_, spaces_before, state) =
            parse_space(EPattern::AsIdentifier, arena, state, min_indent)?;

        let pos = state.pos();
        match parse_lowercase_ident(state) {
            Ok((_, ident, state)) => {
                let pattern = PatternAs {
                    spaces_before,
                    identifier: Loc::pos(pos, state.pos(), ident),
                };
                Ok((MadeProgress, pattern, state))
            }
            Err((_, ())) => Err((MadeProgress, EPattern::AsIdentifier(pos))),
        }
    }
}

fn loc_tag_pattern_args_help<'a>() -> impl Parser<'a, Vec<'a, Loc<Pattern<'a>>>, EPattern<'a>> {
    zero_or_more(loc_tag_pattern_arg(false))
}

/// Like `loc_tag_pattern_args_help`, but stops if a "implements" keyword is seen (indicating an ability).
fn loc_type_def_tag_pattern_args_help<'a>(
) -> impl Parser<'a, Vec<'a, Loc<Pattern<'a>>>, EPattern<'a>> {
    zero_or_more(loc_tag_pattern_arg(true))
}

fn loc_tag_pattern_arg<'a>(
    stop_on_has_kw: bool,
) -> impl Parser<'a, Loc<Pattern<'a>>, EPattern<'a>> {
    // Don't parse operators, because they have a higher precedence than function application.
    // If we encounter one, we're done parsing function args!
    move |arena, state: State<'a>, min_indent| {
        let start = state.pos();
        let (_, spaces, state) =
            parse_space(EPattern::IndentStart, arena, state.clone(), min_indent)
                .map_err(|(_, fail)| (NoProgress, fail))?;

        // Cannot have arguments here, pass `false` to make sure `Foo Bar 1` is parsed as `Foo (Bar) 1`, and not `Foo (Bar 1)`
        let (_, loc_pat, state) = parse_loc_pattern_etc(false, arena, state, min_indent)?;

        let Loc { region, value } = loc_pat;

        if stop_on_has_kw
            && matches!(
                value,
                Pattern::Identifier {
                    ident: crate::keyword::IMPLEMENTS,
                    ..
                }
            )
        {
            Err((NoProgress, EPattern::End(start)))
        } else {
            Ok((
                MadeProgress,
                if spaces.is_empty() {
                    Loc::at(region, value)
                } else {
                    Loc::at(region, Pattern::SpaceBefore(arena.alloc(value), spaces))
                },
                state,
            ))
        }
    }
}

pub fn loc_implements_parser<'a>() -> impl Parser<'a, Loc<Implements<'a>>, EPattern<'a>> {
    then(
        loc_tag_pattern_arg(false),
        |_arena, state, progress, pattern| {
            if matches!(
                pattern.value,
                Pattern::Identifier {
                    ident: crate::keyword::IMPLEMENTS,
                    ..
                }
            ) {
                Ok((
                    progress,
                    Loc::at(pattern.region, Implements::Implements),
                    state,
                ))
            } else {
                Err((progress, EPattern::End(state.pos())))
            }
        },
    )
}

fn parse_pattern_in_parens<'a>(
    arena: &'a Bump,
    state: State<'a>,
) -> ParseResult<'a, Loc<Pattern<'a>>, EPattern<'a>> {
    let start = state.pos();
    if state.bytes().first() != Some(&b'(') {
        let fail = PInParens::Open(start);
        return Err((NoProgress, EPattern::PInParens(fail, start)));
    }
    let state = state.advance(1);

    let elem_parser = specialize_err_ref(PInParens::Pattern, loc_pattern_help());
    let parser = collection_inner(
        elem_parser,
        byte(b',', PInParens::End),
        Pattern::SpaceBefore,
    );
    let (_, pats, state) = parser
        .parse(arena, state, 0)
        .map_err(|(_, fail)| (MadeProgress, EPattern::PInParens(fail, start)))?;

    if state.bytes().first() != Some(&b')') {
        let fail = PInParens::End(state.pos());
        return Err((MadeProgress, EPattern::PInParens(fail, start)));
    }
    let state = state.advance(1);

    if pats.is_empty() {
        let fail = PInParens::Empty(state.pos());
        return Err((NoProgress, EPattern::PInParens(fail, start)));
    }

    let pats = if pats.len() > 1 {
        Loc::pos(start, state.pos(), Pattern::Tuple(pats))
    } else {
        // TODO: don't discard comments before/after
        // (stored in the Collection)
        // TODO: add Pattern::ParensAround to faithfully represent the input, see the `parse_expr_in_parens_etc`
        pats.items[0]
    };
    Ok((MadeProgress, pats, state))
}

fn number_pattern_help<'a>() -> impl Parser<'a, Pattern<'a>, EPattern<'a>> {
    specialize_err(
        EPattern::NumLiteral,
        map(crate::number_literal::number_literal(), |literal| {
            use crate::number_literal::NumLiteral::*;

            match literal {
                Num(s) => Pattern::NumLiteral(s),
                Float(s) => Pattern::FloatLiteral(s),
                NonBase10Int {
                    string,
                    base,
                    is_negative,
                } => Pattern::NonBase10Literal {
                    string,
                    base,
                    is_negative,
                },
            }
        }),
    )
}

fn string_like_pattern_help<'a>() -> impl Parser<'a, Pattern<'a>, EPattern<'a>> {
    specialize_err(
        |_, pos| EPattern::Start(pos),
        map_with_arena(
            crate::string_literal::parse_str_like_literal(),
            |arena, lit| match lit {
                StrLikeLiteral::Str(s) => Pattern::StrLiteral(s),
                StrLikeLiteral::SingleQuote(s) => {
                    // TODO: preserve the original escaping
                    Pattern::SingleQuote(s.to_str_in(arena))
                }
            },
        ),
    )
}

fn list_pattern_help<'a>() -> impl Parser<'a, Pattern<'a>, PList<'a>> {
    map(
        collection_trailing_sep_e(
            byte(b'[', PList::Open),
            list_element_pattern(),
            byte(b',', PList::End),
            byte(b']', PList::End),
            Pattern::SpaceBefore,
        ),
        Pattern::List,
    )
}

fn list_element_pattern<'a>() -> impl Parser<'a, Loc<Pattern<'a>>, PList<'a>> {
    one_of!(
        three_list_rest_pattern_error(),
        list_rest_pattern(),
        specialize_err_ref(PList::Pattern, loc_pattern_help()),
    )
}

fn three_list_rest_pattern_error<'a>() -> impl Parser<'a, Loc<Pattern<'a>>, PList<'a>> {
    fail_when(PList::Rest, loc(three_bytes(b'.', b'.', b'.', PList::Rest)))
}

fn list_rest_pattern<'a>() -> impl Parser<'a, Loc<Pattern<'a>>, PList<'a>> {
    move |arena: &'a Bump, state: State<'a>, min_indent: u32| {
        let (_, loc_word, state) =
            loc(two_bytes(b'.', b'.', PList::Open)).parse(arena, state, min_indent)?;

        let no_as = Loc::at(loc_word.region, Pattern::ListRest(None));

        let pattern_state = state.clone();

        let (pattern_spaces, state) =
            match space0_e(EPattern::AsKeyword).parse(arena, state, min_indent) {
                Err(_) => return Ok((MadeProgress, no_as, pattern_state)),
                Ok((_, pattern_spaces, state)) => (pattern_spaces, state),
            };

        let position = state.pos();
        match pattern_as().parse(arena, state, min_indent) {
            Err((progress, e)) => match progress {
                MadeProgress => Err((MadeProgress, PList::Pattern(arena.alloc(e), position))),
                NoProgress => Ok((MadeProgress, no_as, pattern_state)),
            },
            Ok((_, pattern_as, state)) => {
                let region = Region::span_across(&loc_word.region, &pattern_as.identifier.region);

                let as_pattern = Pattern::ListRest(Some((pattern_spaces, pattern_as)));

                Ok((MadeProgress, Loc::at(region, as_pattern), state))
            }
        }
    }
}

fn parse_ident_pattern<'a>(
    can_have_arguments: bool,
    arena: &'a Bump,
    state: State<'a>,
    min_indent: u32,
) -> ParseResult<'a, Loc<Pattern<'a>>, EPattern<'a>> {
    let start = state.pos();
    let (_, ident, state) =
        parse_ident(arena, state, min_indent).map_err(|(p, _)| (p, EPattern::Start(start)))?;

    let ident_loc = Region::new(start, state.pos());
    match ident {
        Ident::Tag(tag) => {
            let loc_tag = Loc::at(ident_loc, Pattern::Tag(tag));

            // Make sure `Foo Bar 1` is parsed as `Foo (Bar) 1`, and not `Foo (Bar 1)`
            if can_have_arguments {
                let (_, loc_args, state) =
                    loc_type_def_tag_pattern_args_help().parse(arena, state, min_indent)?;

                if loc_args.is_empty() {
                    Ok((MadeProgress, loc_tag, state))
                } else {
                    let region = Region::across_all(
                        std::iter::once(&ident_loc)
                            .chain(loc_args.iter().map(|loc_arg| &loc_arg.region)),
                    );
                    let value = Pattern::Apply(&*arena.alloc(loc_tag), loc_args.into_bump_slice());
                    Ok((MadeProgress, Loc { region, value }, state))
                }
            } else {
                Ok((MadeProgress, loc_tag, state))
            }
        }
        Ident::OpaqueRef(name) => {
            let loc_pat = Loc::at(ident_loc, Pattern::OpaqueRef(name));

            // Make sure `@Foo Bar 1` is parsed as `@Foo (Bar) 1`, and not `@Foo (Bar 1)`
            if can_have_arguments {
                let (_, loc_args, state) =
                    loc_tag_pattern_args_help().parse(arena, state, min_indent)?;

                if loc_args.is_empty() {
                    Ok((MadeProgress, loc_pat, state))
                } else {
                    let region = Region::across_all(
                        std::iter::once(&ident_loc)
                            .chain(loc_args.iter().map(|loc_arg| &loc_arg.region)),
                    );
                    let value = Pattern::Apply(&*arena.alloc(loc_pat), loc_args.into_bump_slice());
                    Ok((MadeProgress, Loc { region, value }, state))
                }
            } else {
                Ok((MadeProgress, loc_pat, state))
            }
        }
        Ident::Access {
            module_name, parts, ..
        } => {
            // Plain identifiers (e.g. `foo`) are allowed in patterns, but
            // more complex ones (e.g. `Foo.bar` or `foo.bar.baz`) are not.
            if module_name.is_empty() && parts.len() == 1 {
                if let Accessor::RecordField(var) = &parts[0] {
                    let ident = Loc::at(ident_loc, Pattern::Identifier { ident: var });
                    return Ok((MadeProgress, ident, state));
                }
            }

            let mut malformed_str = String::new_in(arena);
            if !module_name.is_empty() {
                malformed_str.push_str(module_name);
            };
            for part in parts {
                if !malformed_str.is_empty() {
                    malformed_str.push('.');
                }
                malformed_str.push_str(part.as_inner());
            }

            let bad_ident = Loc::at(ident_loc, Pattern::Malformed(malformed_str.into_bump_str()));
            Ok((MadeProgress, bad_ident, state))
        }
        Ident::AccessorFunction(_string) => Err((MadeProgress, EPattern::AccessorFunction(start))),
        Ident::RecordUpdaterFunction(_string) => {
            Err((MadeProgress, EPattern::RecordUpdaterFunction(start)))
        }
        Ident::Malformed(malformed, problem) => {
            debug_assert!(!malformed.is_empty());
            let loc = Loc::at(ident_loc, Pattern::MalformedIdent(malformed, problem));
            Ok((MadeProgress, loc, state))
        }
    }
}

fn parse_underscore_pattern<'a>(
    arena: &'a Bump,
    state: State<'a>,
    min_indent: u32,
) -> ParseResult<'a, Loc<Pattern<'a>>, EPattern<'a>> {
    if state.bytes().first() != Some(&b'_') {
        return Err((NoProgress, EPattern::Underscore(state.pos())));
    }

    let start = state.pos();
    let state = state.advance(1);
    let before_ident = state.clone();
    match lowercase_ident().parse(arena, state, min_indent) {
        Ok((_, name, state)) => {
            let ident = Loc::pos(start, state.pos(), Pattern::Underscore(name));
            Ok((MadeProgress, ident, state))
        }
        Err((NoProgress, _)) => {
            let ident = Loc::pos(start, before_ident.pos(), Pattern::Underscore(""));
            Ok((MadeProgress, ident, before_ident))
        }
        Err(_) => Err((MadeProgress, EPattern::End(before_ident.pos()))),
    }
}

fn parse_record_pattern<'a>(
    arena: &'a Bump,
    state: State<'a>,
    min_indent: u32,
) -> ParseResult<'a, Loc<Pattern<'a>>, EPattern<'a>> {
    let start = state.pos();
    match record_pattern_fields().parse(arena, state, min_indent) {
        Ok((p, pats, state)) => {
            let pat = Loc::pos(start, state.pos(), Pattern::RecordDestructure(pats));
            Ok((p, pat, state))
        }
        Err((p, fail)) => Err((p, EPattern::Record(fail, start))),
    }
}

pub fn record_pattern_fields<'a>() -> impl Parser<'a, Collection<'a, Loc<Pattern<'a>>>, PRecord<'a>>
{
    let inner = collection_inner(
        record_pattern_field(),
        byte(b',', PRecord::End),
        Pattern::SpaceBefore,
    );
    move |arena, state: crate::state::State<'a>, _: u32| {
        if state.bytes().first() != Some(&b'{') {
            return Err((NoProgress, PRecord::Open(state.pos())));
        }
        let state = state.advance(1);

        let (out, state) = match inner.parse(arena, state, 0) {
            Ok((_, out, state)) => (out, state),
            Err((_, fail)) => return Err((MadeProgress, fail)),
        };

        if state.bytes().first() != Some(&b'}') {
            return Err((MadeProgress, PRecord::End(state.pos())));
        }
        let state = state.advance(1);

        return Ok((MadeProgress, out, state));
    }
}

fn record_pattern_field<'a>() -> impl Parser<'a, Loc<Pattern<'a>>, PRecord<'a>> {
    move |arena, state: State<'a>, min_indent: u32| {
        // You must have a field name, e.g. "email"
        // using the initial pos is important for error reporting
        let start = state.pos();
        let (label_progress, label, state) =
            parse_lowercase_ident(state).map_err(|(p, _)| (p, PRecord::Field(start)))?;
        debug_assert_eq!(label_progress, MadeProgress);
        let label_at = Region::new(start, state.pos());

        let (_, spaces, state) = spaces().parse(arena, state, min_indent)?;

        // Having a value is optional; both `{ email }` and `{ email: blah }` work.
        // (This is true in both literals and types.)
        if state.bytes().first() == Some(&b':') {
            let state = state.advance(1);

            let val_parser = specialize_err_ref(PRecord::Pattern, loc_pattern_help());

            let (_, loc_val, state) = spaces_before(val_parser).parse(arena, state, min_indent)?;

            let region = Region::span_across(&label_at, &loc_val.region);

            // TODO spaces are dropped here
            // arena.alloc(arena.alloc(value).with_spaces_before(spaces, region)),
            let req_field = Pattern::RequiredField(label, arena.alloc(loc_val));
            return Ok((MadeProgress, Loc::at(region, req_field), state));
        }

        if state.bytes().first() == Some(&b'?') {
            let state = state.advance(1);

            let val_parser = specialize_err_ref(PRecord::Expr, crate::expr::loc_expr(false));

            let (_, loc_val, state) = spaces_before(val_parser).parse(arena, state, min_indent)?;

            let region = Region::span_across(&label_at, &loc_val.region);

            // TODO spaces are dropped
            // arena.alloc(arena.alloc(value).with_spaces_before(spaces, region)),
            let opt_field = Pattern::OptionalField(label, arena.alloc(loc_val));
            return Ok((MadeProgress, Loc::at(region, opt_field), state));
        }

        let value = if !spaces.is_empty() {
            Pattern::SpaceAfter(arena.alloc(Pattern::Identifier { ident: label }), spaces)
        } else {
            Pattern::Identifier { ident: label }
        };

        Ok((MadeProgress, Loc::at(label_at, value), state))
    }
}
