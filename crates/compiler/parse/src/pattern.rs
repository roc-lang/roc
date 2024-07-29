use crate::ast::{Collection, Implements, Pattern, PatternAs, Spaceable};
use crate::blankspace::{space0_e, spaces, spaces_before};
use crate::ident::{lowercase_ident, parse_ident, Accessor, Ident};
use crate::keyword;
use crate::parser::{
    self, backtrackable, byte, collection_trailing_sep_e, fail_when, loc, map, map_with_arena,
    optional, specialize_err, specialize_err_ref, then, three_bytes, two_bytes, zero_or_more,
    EPattern, PInParens, PList, PRecord, Parser,
};
use crate::parser::{either, Progress::*};
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
}

pub fn closure_param<'a>() -> impl Parser<'a, Loc<Pattern<'a>>, EPattern<'a>> {
    one_of!(
        // An ident is the most common param, e.g. \foo -> ...
        loc_ident_pattern_help(true),
        // Underscore is also common, e.g. \_ -> ...
        loc_underscore_pattern_help(),
        // You can destructure records in params, e.g. \{ x, y } -> ...
        loc_record_pattern_help(),
        // If you wrap it in parens, you can match any arbitrary pattern at all.
        // e.g. \User.UserId userId -> ...
        loc_pattern_in_parens_help()
    )
}

pub fn loc_pattern_help<'a>() -> impl Parser<'a, Loc<Pattern<'a>>, EPattern<'a>> {
    move |arena, state: State<'a>, min_indent| {
        let (_, pattern, state) = loc_pattern_help_help(true).parse(arena, state, min_indent)?;

        let pattern_state = state.clone();

        let (pattern_spaces, state) =
            match space0_e(EPattern::AsKeyword).parse(arena, state, min_indent) {
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

                let mut pattern = pattern;
                if !pattern_spaces.is_empty() {
                    pattern = arena
                        .alloc(pattern.value)
                        .with_spaces_after(pattern_spaces, pattern.region)
                }

                let as_pattern = Pattern::As(arena.alloc(pattern), pattern_as);

                Ok((MadeProgress, Loc::at(region, as_pattern), state))
            }
        }
    }
}

fn loc_pattern_help_help<'a>(
    can_have_arguments: bool,
) -> impl Parser<'a, Loc<Pattern<'a>>, EPattern<'a>> {
    one_of!(
        loc_pattern_in_parens_help(),
        loc_underscore_pattern_help(),
        loc_ident_pattern_help(can_have_arguments),
        loc_record_pattern_help(),
        loc(specialize_err(EPattern::List, list_pattern_help())),
        loc(number_pattern_help()),
        loc(string_like_pattern_help()),
    )
}

fn pattern_as<'a>() -> impl Parser<'a, PatternAs<'a>, EPattern<'a>> {
    move |arena, state: State<'a>, min_indent| {
        let (_, _, state) =
            parser::keyword(keyword::AS, EPattern::AsKeyword).parse(arena, state, min_indent)?;

        let (_, spaces, state) =
            space0_e(EPattern::AsIdentifier).parse(arena, state, min_indent)?;

        let position = state.pos();

        match loc(lowercase_ident()).parse(arena, state, min_indent) {
            Ok((_, identifier, state)) => Ok((
                MadeProgress,
                PatternAs {
                    spaces_before: spaces,
                    identifier,
                },
                state,
            )),
            Err((_, ())) => Err((MadeProgress, EPattern::AsIdentifier(position))),
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
    move |arena, original_state: State<'a>, min_indent| {
        let (_, spaces, state) = backtrackable(space0_e(EPattern::IndentStart)).parse(
            arena,
            original_state.clone(),
            min_indent,
        )?;

        // Cannot have arguments here, pass `false` to make sure `Foo Bar 1` is parsed as `Foo (Bar) 1`, and not `Foo (Bar 1)`
        let (_, loc_pat, state) = loc_pattern_help_help(false).parse(arena, state, min_indent)?;

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
            Err((NoProgress, EPattern::End(original_state.pos())))
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

fn loc_pattern_in_parens_help<'a>() -> impl Parser<'a, Loc<Pattern<'a>>, EPattern<'a>> {
    (move |arena, state: State<'a>, _min_indent| {
        let patterns_parser = parser::collection_inner(
            specialize_err_ref(PInParens::Pattern, loc_pattern_help()),
            byte(b',', PInParens::End),
            Pattern::SpaceBefore,
        );

        let start = state.pos();
        if state.bytes().first() != Some(&b'(') {
            let fail = PInParens::Open(state.pos());
            Err((NoProgress, EPattern::PInParens(fail, start)))
        } else {
            let state = state.advance(1);
            match patterns_parser.parse(arena, state, 0) {
                Ok((_, pats, state)) => {
                    if state.bytes().first() != Some(&b')') {
                        let fail = PInParens::End(state.pos());
                        Err((MadeProgress, EPattern::PInParens(fail, start)))
                    } else {
                        let state = state.advance(1);
                        if pats.len() > 1 {
                            let pats = Loc::pos(start, state.pos(), Pattern::Tuple(pats));
                            Ok((MadeProgress, pats, state))
                        } else if pats.is_empty() {
                            let fail = PInParens::Empty(state.pos());
                            Err((NoProgress, EPattern::PInParens(fail, start)))
                        } else {
                            // TODO: don't discard comments before/after
                            // (stored in the Collection)
                            // TODO: add Pattern::ParensAround to faithfully represent the input
                            Ok((MadeProgress, pats.items[0], state))
                        }
                    }
                }
                Err((_, fail)) => Err((MadeProgress, EPattern::PInParens(fail, start))),
            }
        }
    })
    .trace("pat_in_parens")
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

fn loc_ident_pattern_help<'a>(
    can_have_arguments: bool,
) -> impl Parser<'a, Loc<Pattern<'a>>, EPattern<'a>> {
    move |arena: &'a Bump, state: State<'a>, min_indent: u32| {
        let original_state = state.clone();

        let (_, loc_ident, state) = specialize_err(|_, pos| EPattern::Start(pos), loc(parse_ident))
            .parse(arena, state, min_indent)?;

        match loc_ident.value {
            Ident::Tag(tag) => {
                let loc_tag = Loc {
                    region: loc_ident.region,
                    value: Pattern::Tag(tag),
                };

                // Make sure `Foo Bar 1` is parsed as `Foo (Bar) 1`, and not `Foo (Bar 1)`
                if can_have_arguments {
                    let (_, loc_args, state) =
                        loc_type_def_tag_pattern_args_help().parse(arena, state, min_indent)?;

                    if loc_args.is_empty() {
                        Ok((MadeProgress, loc_tag, state))
                    } else {
                        let region = Region::across_all(
                            std::iter::once(&loc_ident.region)
                                .chain(loc_args.iter().map(|loc_arg| &loc_arg.region)),
                        );
                        let value =
                            Pattern::Apply(&*arena.alloc(loc_tag), loc_args.into_bump_slice());

                        Ok((MadeProgress, Loc { region, value }, state))
                    }
                } else {
                    Ok((MadeProgress, loc_tag, state))
                }
            }
            Ident::OpaqueRef(name) => {
                let loc_pat = Loc {
                    region: loc_ident.region,
                    value: Pattern::OpaqueRef(name),
                };

                // Make sure `@Foo Bar 1` is parsed as `@Foo (Bar) 1`, and not `@Foo (Bar 1)`
                if can_have_arguments {
                    let (_, loc_args, state) =
                        loc_tag_pattern_args_help().parse(arena, state, min_indent)?;

                    if loc_args.is_empty() {
                        Ok((MadeProgress, loc_pat, state))
                    } else {
                        let region = Region::across_all(
                            std::iter::once(&loc_ident.region)
                                .chain(loc_args.iter().map(|loc_arg| &loc_arg.region)),
                        );
                        let value =
                            Pattern::Apply(&*arena.alloc(loc_pat), loc_args.into_bump_slice());

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

                for keyword in crate::keyword::KEYWORDS.iter() {
                    if parts[0] == Accessor::RecordField(keyword) {
                        return Err((NoProgress, EPattern::End(original_state.pos())));
                    }
                }

                if module_name.is_empty() && parts.len() == 1 {
                    if let Accessor::RecordField(var) = &parts[0] {
                        return Ok((
                            MadeProgress,
                            Loc {
                                region: loc_ident.region,
                                value: Pattern::Identifier { ident: var },
                            },
                            state,
                        ));
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

                Ok((
                    MadeProgress,
                    Loc {
                        region: loc_ident.region,
                        value: Pattern::Malformed(malformed_str.into_bump_str()),
                    },
                    state,
                ))
            }
            Ident::AccessorFunction(_string) => Err((
                MadeProgress,
                EPattern::AccessorFunction(loc_ident.region.start()),
            )),
            Ident::Malformed(malformed, problem) => {
                debug_assert!(!malformed.is_empty());

                Ok((
                    MadeProgress,
                    Loc {
                        region: loc_ident.region,
                        value: Pattern::MalformedIdent(malformed, problem),
                    },
                    state,
                ))
            }
        }
    }
}

fn loc_underscore_pattern_help<'a>() -> impl Parser<'a, Loc<Pattern<'a>>, EPattern<'a>> {
    move |arena, state: State<'a>, min_indent| {
        let start = state.pos();
        if state.bytes().first() == Some(&b'_') {
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
        } else {
            Err((NoProgress, EPattern::Underscore(state.pos())))
        }
    }
}

fn loc_record_pattern_help<'a>() -> impl Parser<'a, Loc<Pattern<'a>>, EPattern<'a>> {
    move |arena, state: State<'a>, min_indent| {
        let start = state.pos();
        match record_pattern_fields().parse(arena, state, min_indent) {
            Ok((p, pats, state)) => {
                let pat = Loc::pos(start, state.pos(), Pattern::RecordDestructure(pats));
                Ok((p, pat, state))
            }
            Err((p, fail)) => Err((p, EPattern::Record(fail, start))),
        }
    }
}

pub fn record_pattern_fields<'a>() -> impl Parser<'a, Collection<'a, Loc<Pattern<'a>>>, PRecord<'a>>
{
    collection_trailing_sep_e(
        byte(b'{', PRecord::Open),
        record_pattern_field(),
        byte(b',', PRecord::End),
        byte(b'}', PRecord::End),
        Pattern::SpaceBefore,
    )
}

fn record_pattern_field<'a>() -> impl Parser<'a, Loc<Pattern<'a>>, PRecord<'a>> {
    use crate::parser::Either::*;

    move |arena, state: State<'a>, min_indent: u32| {
        // You must have a field name, e.g. "email"
        // using the initial pos is important for error reporting
        let pos = state.pos();
        let (progress, loc_label, state) = loc(specialize_err(
            move |_, _| PRecord::Field(pos),
            lowercase_ident(),
        ))
        .parse(arena, state, min_indent)?;
        debug_assert_eq!(progress, MadeProgress);

        let (_, spaces, state) = spaces().parse(arena, state, min_indent)?;

        // Having a value is optional; both `{ email }` and `{ email: blah }` work.
        // (This is true in both literals and types.)
        let (_, opt_loc_val, state) = optional(either(
            byte(b':', PRecord::Colon),
            byte(b'?', PRecord::Optional),
        ))
        .parse(arena, state, min_indent)?;

        match opt_loc_val {
            Some(First(_)) => {
                let val_parser = specialize_err_ref(PRecord::Pattern, loc_pattern_help());
                let (_, loc_val, state) =
                    spaces_before(val_parser).parse(arena, state, min_indent)?;

                let Loc {
                    value: label,
                    region,
                } = loc_label;

                let region = Region::span_across(&region, &loc_val.region);

                Ok((
                    MadeProgress,
                    Loc::at(
                        region,
                        Pattern::RequiredField(
                            label,
                            // TODO spaces are dropped here
                            // arena.alloc(arena.alloc(value).with_spaces_before(spaces, region)),
                            arena.alloc(loc_val),
                        ),
                    ),
                    state,
                ))
            }
            Some(Second(_)) => {
                let val_parser = specialize_err_ref(PRecord::Expr, crate::expr::loc_expr(false));

                let (_, loc_val, state) =
                    spaces_before(val_parser).parse(arena, state, min_indent)?;

                let Loc {
                    value: label,
                    region,
                } = loc_label;

                let region = Region::span_across(&region, &loc_val.region);

                Ok((
                    MadeProgress,
                    Loc::at(
                        region,
                        Pattern::OptionalField(
                            label,
                            // TODO spaces are dropped
                            // arena.alloc(arena.alloc(value).with_spaces_before(spaces, region)),
                            arena.alloc(loc_val),
                        ),
                    ),
                    state,
                ))
            }
            // If no value was provided, record it as a Var.
            // Canonicalize will know what to do with a Var later.
            None => {
                let Loc { value, region } = loc_label;
                let value = if !spaces.is_empty() {
                    Pattern::SpaceAfter(arena.alloc(Pattern::Identifier { ident: value }), spaces)
                } else {
                    Pattern::Identifier { ident: value }
                };

                Ok((MadeProgress, Loc::at(region, value), state))
            }
        }
    }
}
