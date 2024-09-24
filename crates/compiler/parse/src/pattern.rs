use crate::ast::{Collection, Implements, Pattern, PatternAs};
use crate::blankspace::{eat_space, parse_space, with_spaces_after, with_spaces_before};
use crate::expr::{parse_expr_start, ExprParseOptions};
use crate::ident::{parse_ident, parse_lowercase_ident, Accessor, Ident};
use crate::keyword;
use crate::number_literal::parse_number_base;
use crate::parser::{at_keyword, Progress::*};
use crate::parser::{
    collection_inner, specialize_err_ref, then, zero_or_more, EPattern, PInParens, PList, PRecord,
    ParseResult, Parser,
};
use crate::state::State;
use crate::string_literal::{rest_of_str_like, StrLikeLiteral};
use bumpalo::collections::string::String;
use bumpalo::collections::Vec;
use bumpalo::Bump;
use roc_region::all::{Loc, Position, Region};

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
    if let Some(b) = state.bytes().first() {
        let start = state.pos();
        match b {
            b'_' => {
                // Underscore is also common, e.g. \_ -> ...
                rest_of_underscore_pattern(start, state.inc())
            }
            b'{' => {
                // You can destructure records in params, e.g. \{ x, y } -> ...
                rest_of_record_pattern(start, arena, state.inc())
            }
            b'(' => {
                // If you wrap it in parens, you can match any arbitrary pattern at all. But what about the list pattern?
                // e.g. \(User.UserId userId) -> ...
                rest_of_pattern_in_parens(start, arena, state.inc())
            }
            // b'[' => parse_list_pattern(arena, state.clone()),
            _ => parse_ident_pattern(start, true, arena, state, min_indent),
        }
    } else {
        Err((NoProgress, EPattern::Start(state.pos())))
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

        match parse_pattern_as(arena, state, min_indent) {
            Err((MadeProgress, e)) => Err((MadeProgress, e)),
            Err(_) => Ok((MadeProgress, pattern, pattern_state)),
            Ok((_, pattern_as, state)) => {
                let region = Region::span_across(&pattern.region, &pattern_as.identifier.region);

                let pattern = with_spaces_after(arena, pattern, pattern_spaces);
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
    if let Some(b) = state.bytes().first() {
        let start = state.pos();
        match b {
            b'_' => rest_of_underscore_pattern(start, state.inc()),
            b'{' => rest_of_record_pattern(start, arena, state.inc()),
            b'(' => rest_of_pattern_in_parens(start, arena, state.inc()),
            b'[' => rest_of_list_pattern(start, arena, state.inc()),
            b'"' | b'\'' => {
                let column = state.column();
                match rest_of_str_like(*b == b'\'', column, arena, state.inc(), min_indent) {
                    Ok((p, literal, state)) => {
                        let literal = match literal {
                            StrLikeLiteral::Str(s) => Pattern::StrLiteral(s),
                            StrLikeLiteral::SingleQuote(s) => {
                                // TODO: preserve the original escaping
                                Pattern::SingleQuote(s.to_str_in(arena))
                            }
                        };
                        Ok((p, Loc::pos(start, state.pos(), literal), state))
                    }
                    Err((p, _)) => Err((p, EPattern::Start(start))),
                }
            }
            b'0'..=b'9' => {
                let (p, literal, state) = parse_number_base(false, state.bytes(), state)
                    .map_err(|(p, fail)| (p, EPattern::NumLiteral(fail, start)))?;
                let pattern = literal_to_pattern(literal);
                Ok((p, Loc::pos(start, state.pos(), pattern), state))
            }
            b'-' => match parse_number_base(true, &state.bytes()[1..], state) {
                Ok((p, literal, state)) => {
                    let pattern = literal_to_pattern(literal);
                    Ok((p, Loc::pos(start, state.pos(), pattern), state))
                }
                Err((MadeProgress, fail)) => Err((MadeProgress, EPattern::NumLiteral(fail, start))),
                Err(_) => {
                    // it may be the case with split arrow `- >` or similar,
                    // so it should not considered as bad number, let's keep parsing until we find the closest error.
                    Err((NoProgress, EPattern::Start(start)))
                }
            },
            _ => parse_ident_pattern(start, can_have_arguments, arena, state.clone(), min_indent),
        }
    } else {
        Err((NoProgress, EPattern::Start(state.pos())))
    }
}

fn parse_pattern_as<'a>(
    arena: &'a Bump,
    state: State<'a>,
    min_indent: u32,
) -> ParseResult<'a, PatternAs<'a>, EPattern<'a>> {
    if !at_keyword(keyword::AS, &state) {
        return Err((NoProgress, EPattern::AsKeyword(state.pos())));
    }
    let state = state.advance(keyword::AS.len());

    let (_, spaces_before, state) = parse_space(EPattern::AsIdentifier, arena, state, min_indent)?;

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

fn rest_of_pattern_in_parens<'a>(
    start: Position,
    arena: &'a Bump,
    state: State<'a>,
) -> ParseResult<'a, Loc<Pattern<'a>>, EPattern<'a>> {
    let parser = collection_inner(
        specialize_err_ref(PInParens::Pattern, loc_pattern_help()),
        Pattern::SpaceBefore,
    );

    let (_, pats, state) = parser
        .parse(arena, state, 0)
        .map_err(|(_, fail)| (MadeProgress, EPattern::PInParens(fail, start)))?;

    if state.bytes().first() != Some(&b')') {
        let fail = PInParens::End(state.pos());
        return Err((MadeProgress, EPattern::PInParens(fail, start)));
    }
    let state = state.inc();

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

fn literal_to_pattern(literal: crate::number_literal::NumLiteral<'_>) -> Pattern<'_> {
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
}

fn rest_of_list_pattern<'a>(
    start: Position,
    arena: &'a Bump,
    state: State<'a>,
) -> ParseResult<'a, Loc<Pattern<'a>>, EPattern<'a>> {
    let inner = collection_inner(list_element_pattern(), Pattern::SpaceBefore);

    let (elems, state) = match inner.parse(arena, state, 0) {
        Ok((_, out, state)) => (out, state),
        Err((_, fail)) => return Err((MadeProgress, EPattern::List(fail, start))),
    };

    if state.bytes().first() != Some(&b']') {
        let fail = PList::End(state.pos());
        return Err((MadeProgress, EPattern::List(fail, start)));
    }
    let state = state.inc();

    let pattern = Loc::pos(start, state.pos(), Pattern::List(elems));
    return Ok((MadeProgress, pattern, state));
}

fn list_element_pattern<'a>() -> impl Parser<'a, Loc<Pattern<'a>>, PList<'a>> {
    move |arena: &'a Bump, state: State<'a>, min_indent: u32| {
        let start = state.pos();
        if state.bytes().starts_with(b"...") {
            return Err((MadeProgress, PList::Rest(start)));
        }

        match parse_list_rest_pattern(start, arena, state.clone(), min_indent) {
            Err((NoProgress, _)) => {}
            res => return res,
        }

        match loc_pattern_help().parse(arena, state.clone(), min_indent) {
            Ok(ok) => Ok(ok),
            Err((p, fail)) => Err((p, PList::Pattern(arena.alloc(fail), start))),
        }
    }
}

fn parse_list_rest_pattern<'a>(
    start: Position,
    arena: &'a Bump,
    state: State<'a>,
    min_indent: u32,
) -> ParseResult<'a, Loc<Pattern<'a>>, PList<'a>> {
    if !state.bytes().starts_with(b"..") {
        return Err((NoProgress, PList::Open(start)));
    }
    let state = state.advance(2);
    let dots_at = Region::new(start, state.pos());

    let no_as = Loc::at(dots_at, Pattern::ListRest(None));

    let pattern_state = state.clone();
    let (pattern_spaces, state) = match parse_space(EPattern::AsKeyword, arena, state, min_indent) {
        Ok((_, pattern_spaces, state)) => (pattern_spaces, state),
        Err(_) => return Ok((MadeProgress, no_as, pattern_state)),
    };

    let position = state.pos();
    match parse_pattern_as(arena, state, min_indent) {
        Err((MadeProgress, e)) => Err((MadeProgress, PList::Pattern(arena.alloc(e), position))),
        Err(_) => Ok((MadeProgress, no_as, pattern_state)),
        Ok((_, pattern_as, state)) => {
            let region = Region::span_across(&dots_at, &pattern_as.identifier.region);

            let as_pattern = Pattern::ListRest(Some((pattern_spaces, pattern_as)));
            Ok((MadeProgress, Loc::at(region, as_pattern), state))
        }
    }
}

fn parse_ident_pattern<'a>(
    start: Position,
    can_have_arguments: bool,
    arena: &'a Bump,
    state: State<'a>,
    min_indent: u32,
) -> ParseResult<'a, Loc<Pattern<'a>>, EPattern<'a>> {
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

fn rest_of_underscore_pattern<'a>(
    start: Position,
    state: State<'a>,
) -> ParseResult<'a, Loc<Pattern<'a>>, EPattern<'a>> {
    let after_underscore = state.clone();
    match parse_lowercase_ident(state) {
        Ok((_, name, state)) => {
            let ident = Loc::pos(start, state.pos(), Pattern::Underscore(name));
            Ok((MadeProgress, ident, state))
        }
        Err((NoProgress, _)) => {
            let ident = Loc::pos(start, after_underscore.pos(), Pattern::Underscore(""));
            Ok((MadeProgress, ident, after_underscore))
        }
        Err(_) => Err((MadeProgress, EPattern::End(after_underscore.pos()))),
    }
}

fn rest_of_record_pattern<'a>(
    start: Position,
    arena: &'a Bump,
    state: State<'a>,
) -> ParseResult<'a, Loc<Pattern<'a>>, EPattern<'a>> {
    let inner = collection_inner(record_pattern_field(), Pattern::SpaceBefore);

    let (fields, state) = match inner.parse(arena, state, 0) {
        Ok((_, fields, state)) => (fields, state),
        Err((_, fail)) => return Err((MadeProgress, EPattern::Record(fail, start))),
    };

    if state.bytes().first() != Some(&b'}') {
        let fail = PRecord::End(state.pos());
        return Err((MadeProgress, EPattern::Record(fail, start)));
    }
    let state = state.inc();

    let pattern = Pattern::RecordDestructure(fields);
    Ok((MadeProgress, Loc::pos(start, state.pos(), pattern), state))
}

pub fn parse_record_pattern_fields<'a>(
    arena: &'a Bump,
    state: State<'a>,
) -> ParseResult<'a, Collection<'a, Loc<Pattern<'a>>>, PRecord<'a>> {
    if state.bytes().first() != Some(&b'{') {
        return Err((NoProgress, PRecord::Open(state.pos())));
    }
    let state = state.inc();

    let inner = collection_inner(record_pattern_field(), Pattern::SpaceBefore);

    let (out, state) = match inner.parse(arena, state, 0) {
        Ok((_, out, state)) => (out, state),
        Err((_, fail)) => return Err((MadeProgress, fail)),
    };

    if state.bytes().first() != Some(&b'}') {
        return Err((MadeProgress, PRecord::End(state.pos())));
    }

    Ok((MadeProgress, out, state.inc()))
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

        let (_, (label_spaces, _), state) = eat_space(arena, state, true)?;

        // Having a value is optional; both `{ email }` and `{ email: blah }` work.
        // (This is true in both literals and types.)
        if state.bytes().first() == Some(&b':') {
            let state = state.inc();

            let (_, (colon_spaces, _), state) = eat_space(arena, state, true)?;

            let pattern_pos = state.pos();
            let (pattern_val, state) = match loc_pattern_help().parse(arena, state, min_indent) {
                Ok((_, out, state)) => (out, state),
                Err((_, fail)) => {
                    let fail = PRecord::Pattern(arena.alloc(fail), pattern_pos);
                    return Err((MadeProgress, fail));
                }
            };

            let pattern_val = with_spaces_before(arena, pattern_val, colon_spaces);

            let region = Region::span_across(&label_at, &pattern_val.region);

            // TODO spaces are dropped here
            // arena.alloc(arena.alloc(value).with_spaces_before(spaces, region)),
            let req_field = Pattern::RequiredField(label, arena.alloc(pattern_val));
            return Ok((MadeProgress, Loc::at(region, req_field), state));
        }

        if state.bytes().first() == Some(&b'?') {
            let state = state.inc();

            let (_, (question_spaces, _), state) = eat_space(arena, state, true)?;

            let optional_val_pos = state.pos();
            let (optional_val, state) =
                match parse_expr_start(ExprParseOptions::NO_BACK_ARROW, arena, state, min_indent) {
                    Ok((_, out, state)) => (out, state),
                    Err((_, fail)) => {
                        let fail = PRecord::Expr(arena.alloc(fail), optional_val_pos);
                        return Err((MadeProgress, fail));
                    }
                };

            let optional_val = with_spaces_before(arena, optional_val, question_spaces);

            let region = Region::span_across(&label_at, &optional_val.region);

            // TODO spaces are dropped
            // arena.alloc(arena.alloc(value).with_spaces_before(spaces, region)),
            let opt_field = Pattern::OptionalField(label, arena.alloc(optional_val));
            return Ok((MadeProgress, Loc::at(region, opt_field), state));
        }

        let value = if !label_spaces.is_empty() {
            Pattern::SpaceAfter(
                arena.alloc(Pattern::Identifier { ident: label }),
                label_spaces,
            )
        } else {
            Pattern::Identifier { ident: label }
        };

        Ok((MadeProgress, Loc::at(label_at, value), state))
    }
}
