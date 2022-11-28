use crate::ast::{Has, Pattern};
use crate::blankspace::{space0_before_e, space0_e};
use crate::ident::{lowercase_ident, parse_ident, Ident};
use crate::parser::Progress::{self, *};
use crate::parser::{
    backtrackable, fail_when, optional, specialize, specialize_ref, then, word1, word2, word3,
    EPattern, PInParens, PList, PRecord, Parser,
};
use crate::state::State;
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
        loc!(underscore_pattern_help()),
        // You can destructure records in params, e.g. \{ x, y } -> ...
        loc!(specialize(
            EPattern::Record,
            crate::pattern::record_pattern_help()
        )),
        // If you wrap it in parens, you can match any arbitrary pattern at all.
        // e.g. \User.UserId userId -> ...
        specialize(EPattern::PInParens, loc_pattern_in_parens_help())
    )
}

pub fn loc_pattern_help<'a>() -> impl Parser<'a, Loc<Pattern<'a>>, EPattern<'a>> {
    one_of!(
        specialize(EPattern::PInParens, loc_pattern_in_parens_help()),
        loc!(underscore_pattern_help()),
        loc_ident_pattern_help(true),
        loc!(specialize(
            EPattern::Record,
            crate::pattern::record_pattern_help()
        )),
        loc!(specialize(EPattern::List, list_pattern_help())),
        loc!(number_pattern_help()),
        loc!(string_pattern_help()),
        loc!(single_quote_pattern_help()),
    )
}

fn loc_tag_pattern_args_help<'a>() -> impl Parser<'a, Vec<'a, Loc<Pattern<'a>>>, EPattern<'a>> {
    zero_or_more!(loc_tag_pattern_arg(false))
}

/// Like `loc_tag_pattern_args_help`, but stops if a "has" keyword is seen (indicating an ability).
fn loc_type_def_tag_pattern_args_help<'a>(
) -> impl Parser<'a, Vec<'a, Loc<Pattern<'a>>>, EPattern<'a>> {
    zero_or_more!(loc_tag_pattern_arg(true))
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

        let (_, loc_pat, state) = loc_parse_tag_pattern_arg().parse(arena, state, min_indent)?;

        let Loc { region, value } = loc_pat;

        if stop_on_has_kw && matches!(value, Pattern::Identifier("has")) {
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

pub fn loc_has_parser<'a>() -> impl Parser<'a, Loc<Has<'a>>, EPattern<'a>> {
    then(
        loc_tag_pattern_arg(false),
        |_arena, state, progress, pattern| {
            if matches!(pattern.value, Pattern::Identifier("has")) {
                Ok((progress, Loc::at(pattern.region, Has::Has), state))
            } else {
                Err((progress, EPattern::End(state.pos())))
            }
        },
    )
}

fn loc_parse_tag_pattern_arg<'a>() -> impl Parser<'a, Loc<Pattern<'a>>, EPattern<'a>> {
    one_of!(
        specialize(EPattern::PInParens, loc_pattern_in_parens_help()),
        loc!(underscore_pattern_help()),
        // Make sure `Foo Bar 1` is parsed as `Foo (Bar) 1`, and not `Foo (Bar 1)`
        loc_ident_pattern_help(false),
        loc!(specialize(
            EPattern::Record,
            crate::pattern::record_pattern_help()
        )),
        loc!(string_pattern_help()),
        loc!(single_quote_pattern_help()),
        loc!(number_pattern_help())
    )
}

fn loc_pattern_in_parens_help<'a>() -> impl Parser<'a, Loc<Pattern<'a>>, PInParens<'a>> {
    then(
        loc!(collection_trailing_sep_e!(
            word1(b'(', PInParens::Open),
            specialize_ref(PInParens::Pattern, loc_pattern_help()),
            word1(b',', PInParens::End),
            word1(b')', PInParens::End),
            PInParens::IndentOpen,
            Pattern::SpaceBefore
        )),
        move |_arena, state, _, loc_elements| {
            let elements = loc_elements.value;
            let region = loc_elements.region;

            if elements.len() > 1 {
                Ok((
                    MadeProgress,
                    Loc::at(region, Pattern::Tuple(elements)),
                    state,
                ))
            } else if elements.is_empty() {
                Err((NoProgress, PInParens::Empty(state.pos())))
            } else {
                // TODO: don't discard comments before/after
                // (stored in the Collection)
                // TODO: add Pattern::ParensAround to faithfully represent the input
                Ok((MadeProgress, elements.items[0], state))
            }
        },
    )
    .trace("pat_in_parens")
}

fn number_pattern_help<'a>() -> impl Parser<'a, Pattern<'a>, EPattern<'a>> {
    specialize(
        EPattern::NumLiteral,
        map!(crate::number_literal::number_literal(), |literal| {
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

fn string_pattern_help<'a>() -> impl Parser<'a, Pattern<'a>, EPattern<'a>> {
    specialize(
        |_, pos| EPattern::Start(pos),
        map!(crate::string_literal::parse(), Pattern::StrLiteral),
    )
}

fn single_quote_pattern_help<'a>() -> impl Parser<'a, Pattern<'a>, EPattern<'a>> {
    specialize(
        |_, pos| EPattern::Start(pos),
        map!(
            crate::string_literal::parse_single_quote(),
            Pattern::SingleQuote
        ),
    )
}

fn list_pattern_help<'a>() -> impl Parser<'a, Pattern<'a>, PList<'a>> {
    map!(
        collection_trailing_sep_e!(
            word1(b'[', PList::Open),
            list_element_pattern(),
            word1(b',', PList::End),
            word1(b']', PList::End),
            PList::IndentEnd,
            Pattern::SpaceBefore
        ),
        Pattern::List
    )
}

fn list_element_pattern<'a>() -> impl Parser<'a, Loc<Pattern<'a>>, PList<'a>> {
    one_of!(
        three_list_rest_pattern_error(),
        list_rest_pattern(),
        specialize_ref(PList::Pattern, loc_pattern_help()),
    )
}

fn three_list_rest_pattern_error<'a>() -> impl Parser<'a, Loc<Pattern<'a>>, PList<'a>> {
    fail_when(PList::Rest, loc!(word3(b'.', b'.', b'.', PList::Rest)))
}

fn list_rest_pattern<'a>() -> impl Parser<'a, Loc<Pattern<'a>>, PList<'a>> {
    map!(loc!(word2(b'.', b'.', PList::Open)), |loc_word: Loc<_>| {
        loc_word.map(|_| Pattern::ListRest)
    })
}

fn loc_ident_pattern_help<'a>(
    can_have_arguments: bool,
) -> impl Parser<'a, Loc<Pattern<'a>>, EPattern<'a>> {
    move |arena: &'a Bump, state: State<'a>, min_indent: u32| {
        let original_state = state.clone();

        let (_, loc_ident, state) = specialize(|_, pos| EPattern::Start(pos), loc!(parse_ident))
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
            Ident::Access { module_name, parts } => {
                // Plain identifiers (e.g. `foo`) are allowed in patterns, but
                // more complex ones (e.g. `Foo.bar` or `foo.bar.baz`) are not.
                if crate::keyword::KEYWORDS.contains(&parts[0]) {
                    Err((NoProgress, EPattern::End(original_state.pos())))
                } else if module_name.is_empty() && parts.len() == 1 {
                    Ok((
                        MadeProgress,
                        Loc {
                            region: loc_ident.region,
                            value: Pattern::Identifier(parts[0]),
                        },
                        state,
                    ))
                } else {
                    let malformed_str = if module_name.is_empty() {
                        parts.join(".")
                    } else {
                        format!("{}.{}", module_name, parts.join("."))
                    };
                    Ok((
                        MadeProgress,
                        Loc {
                            region: loc_ident.region,
                            value: Pattern::Malformed(
                                String::from_str_in(&malformed_str, arena).into_bump_str(),
                            ),
                        },
                        state,
                    ))
                }
            }
            Ident::RecordAccessorFunction(string) | Ident::TupleAccessorFunction(string) => Ok((
                MadeProgress,
                Loc {
                    region: loc_ident.region,
                    value: Pattern::Malformed(string),
                },
                state,
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

fn underscore_pattern_help<'a>() -> impl Parser<'a, Pattern<'a>, EPattern<'a>> {
    map!(
        skip_first!(
            word1(b'_', EPattern::Underscore),
            optional(lowercase_ident_pattern())
        ),
        |output| match output {
            Some(name) => Pattern::Underscore(name),
            None => Pattern::Underscore(""),
        }
    )
}

fn lowercase_ident_pattern<'a>() -> impl Parser<'a, &'a str, EPattern<'a>> {
    specialize(move |_, pos| EPattern::End(pos), lowercase_ident())
}

#[inline(always)]
fn record_pattern_help<'a>() -> impl Parser<'a, Pattern<'a>, PRecord<'a>> {
    map!(
        collection_trailing_sep_e!(
            word1(b'{', PRecord::Open),
            record_pattern_field(),
            word1(b',', PRecord::End),
            word1(b'}', PRecord::End),
            PRecord::IndentEnd,
            Pattern::SpaceBefore
        ),
        Pattern::RecordDestructure
    )
}

fn record_pattern_field<'a>() -> impl Parser<'a, Loc<Pattern<'a>>, PRecord<'a>> {
    use crate::parser::Either::*;

    move |arena, state: State<'a>, min_indent: u32| {
        // You must have a field name, e.g. "email"
        // using the initial pos is important for error reporting
        let pos = state.pos();
        let (progress, loc_label, state) = loc!(specialize(
            move |_, _| PRecord::Field(pos),
            lowercase_ident()
        ))
        .parse(arena, state, min_indent)?;
        debug_assert_eq!(progress, MadeProgress);

        let (_, spaces, state) = space0_e(PRecord::IndentEnd).parse(arena, state, min_indent)?;

        // Having a value is optional; both `{ email }` and `{ email: blah }` work.
        // (This is true in both literals and types.)
        let (_, opt_loc_val, state) = optional(either!(
            word1(b':', PRecord::Colon),
            word1(b'?', PRecord::Optional)
        ))
        .parse(arena, state, min_indent)?;

        match opt_loc_val {
            Some(First(_)) => {
                let val_parser = specialize_ref(PRecord::Pattern, loc_pattern_help());
                let (_, loc_val, state) = space0_before_e(val_parser, PRecord::IndentColon)
                    .parse(arena, state, min_indent)?;

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
                let val_parser =
                    specialize_ref(PRecord::Expr, crate::expr::loc_expr_no_multi_backpassing());

                let (_, loc_val, state) = space0_before_e(val_parser, PRecord::IndentColon)
                    .parse(arena, state, min_indent)?;

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
                    Pattern::SpaceAfter(arena.alloc(Pattern::Identifier(value)), spaces)
                } else {
                    Pattern::Identifier(value)
                };

                Ok((MadeProgress, Loc::at(region, value), state))
            }
        }
    }
}
