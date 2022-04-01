use crate::ast::Pattern;
use crate::blankspace::{space0_around_ee, space0_before_e, space0_e};
use crate::ident::{lowercase_ident, parse_ident, Ident};
use crate::parser::Progress::{self, *};
use crate::parser::{
    backtrackable, optional, specialize, specialize_ref, word1, EPattern, PInParens, PRecord,
    ParseResult, Parser,
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

pub fn loc_closure_param<'a>(min_indent: u32) -> impl Parser<'a, Loc<Pattern<'a>>, EPattern<'a>> {
    move |arena, state| parse_closure_param(arena, state, min_indent)
}

fn parse_closure_param<'a>(
    arena: &'a Bump,
    state: State<'a>,
    min_indent: u32,
) -> ParseResult<'a, Loc<Pattern<'a>>, EPattern<'a>> {
    one_of!(
        // An ident is the most common param, e.g. \foo -> ...
        loc_ident_pattern_help(min_indent, true),
        // Underscore is also common, e.g. \_ -> ...
        loc!(underscore_pattern_help()),
        // You can destructure records in params, e.g. \{ x, y } -> ...
        loc!(specialize(
            EPattern::Record,
            crate::pattern::record_pattern_help(min_indent)
        )),
        // If you wrap it in parens, you can match any arbitrary pattern at all.
        // e.g. \User.UserId userId -> ...
        specialize(EPattern::PInParens, loc_pattern_in_parens_help(min_indent))
    )
    .parse(arena, state)
}

pub fn loc_pattern_help<'a>(min_indent: u32) -> impl Parser<'a, Loc<Pattern<'a>>, EPattern<'a>> {
    one_of!(
        specialize(EPattern::PInParens, loc_pattern_in_parens_help(min_indent)),
        loc!(underscore_pattern_help()),
        loc_ident_pattern_help(min_indent, true),
        loc!(specialize(
            EPattern::Record,
            crate::pattern::record_pattern_help(min_indent)
        )),
        loc!(number_pattern_help()),
        loc!(string_pattern_help()),
        loc!(single_quote_pattern_help()),
    )
}

fn loc_tag_pattern_args_help<'a>(
    min_indent: u32,
) -> impl Parser<'a, Vec<'a, Loc<Pattern<'a>>>, EPattern<'a>> {
    zero_or_more!(loc_tag_pattern_arg(min_indent))
}

fn loc_tag_pattern_arg<'a>(min_indent: u32) -> impl Parser<'a, Loc<Pattern<'a>>, EPattern<'a>> {
    // Don't parse operators, because they have a higher precedence than function application.
    // If we encounter one, we're done parsing function args!
    move |arena, state| {
        let (_, spaces, state) =
            backtrackable(space0_e(min_indent, EPattern::IndentStart)).parse(arena, state)?;

        let (_, loc_pat, state) = loc_parse_tag_pattern_arg(min_indent, arena, state)?;

        let Loc { region, value } = loc_pat;

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

fn loc_parse_tag_pattern_arg<'a>(
    min_indent: u32,
    arena: &'a Bump,
    state: State<'a>,
) -> ParseResult<'a, Loc<Pattern<'a>>, EPattern<'a>> {
    one_of!(
        specialize(EPattern::PInParens, loc_pattern_in_parens_help(min_indent)),
        loc!(underscore_pattern_help()),
        // Make sure `Foo Bar 1` is parsed as `Foo (Bar) 1`, and not `Foo (Bar 1)`
        loc_ident_pattern_help(min_indent, false),
        loc!(specialize(
            EPattern::Record,
            crate::pattern::record_pattern_help(min_indent)
        )),
        loc!(string_pattern_help()),
        loc!(single_quote_pattern_help()),
        loc!(number_pattern_help())
    )
    .parse(arena, state)
}

fn loc_pattern_in_parens_help<'a>(
    min_indent: u32,
) -> impl Parser<'a, Loc<Pattern<'a>>, PInParens<'a>> {
    between!(
        word1(b'(', PInParens::Open),
        space0_around_ee(
            move |arena, state| specialize_ref(PInParens::Pattern, loc_pattern_help(min_indent))
                .parse(arena, state),
            min_indent,
            PInParens::IndentOpen,
            PInParens::IndentEnd,
        ),
        word1(b')', PInParens::End)
    )
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

fn loc_ident_pattern_help<'a>(
    min_indent: u32,
    can_have_arguments: bool,
) -> impl Parser<'a, Loc<Pattern<'a>>, EPattern<'a>> {
    move |arena: &'a Bump, state: State<'a>| {
        let original_state = state.clone();

        let (_, loc_ident, state) =
            specialize(|_, pos| EPattern::Start(pos), loc!(parse_ident)).parse(arena, state)?;

        match loc_ident.value {
            Ident::GlobalTag(tag) => {
                let loc_tag = Loc {
                    region: loc_ident.region,
                    value: Pattern::GlobalTag(tag),
                };

                // Make sure `Foo Bar 1` is parsed as `Foo (Bar) 1`, and not `Foo (Bar 1)`
                if can_have_arguments {
                    let (_, loc_args, state) =
                        loc_tag_pattern_args_help(min_indent).parse(arena, state)?;

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
            Ident::PrivateTag(name) | Ident::OpaqueRef(name) => {
                let loc_pat = Loc {
                    region: loc_ident.region,
                    value: if matches!(loc_ident.value, Ident::PrivateTag(..)) {
                        Pattern::PrivateTag(name)
                    } else {
                        Pattern::OpaqueRef(name)
                    },
                };

                // Make sure `@Foo Bar 1` is parsed as `@Foo (Bar) 1`, and not `@Foo (Bar 1)`
                if can_have_arguments {
                    let (_, loc_args, state) =
                        loc_tag_pattern_args_help(min_indent).parse(arena, state)?;

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
                    Err((
                        NoProgress,
                        EPattern::End(original_state.pos()),
                        original_state,
                    ))
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
            Ident::AccessorFunction(string) => Ok((
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
    move |arena: &'a Bump, state: State<'a>| {
        let (_, _, next_state) = word1(b'_', EPattern::Underscore).parse(arena, state)?;

        let (_, output, final_state) =
            optional(lowercase_ident_pattern).parse(arena, next_state)?;

        match output {
            Some(name) => Ok((MadeProgress, Pattern::Underscore(name), final_state)),
            None => Ok((MadeProgress, Pattern::Underscore(""), final_state)),
        }
    }
}

fn lowercase_ident_pattern<'a>(
    arena: &'a Bump,
    state: State<'a>,
) -> ParseResult<'a, &'a str, EPattern<'a>> {
    let pos = state.pos();

    specialize(move |_, _| EPattern::End(pos), lowercase_ident()).parse(arena, state)
}

#[inline(always)]
fn record_pattern_help<'a>(min_indent: u32) -> impl Parser<'a, Pattern<'a>, PRecord<'a>> {
    move |arena, state| {
        let (_, fields, state) = collection_trailing_sep_e!(
            // word1_check_indent!(b'{', PRecord::Open, min_indent, PRecord::IndentOpen),
            word1(b'{', PRecord::Open),
            record_pattern_field(min_indent),
            word1(b',', PRecord::End),
            // word1_check_indent!(b'}', PRecord::End, min_indent, PRecord::IndentEnd),
            word1(b'}', PRecord::End),
            min_indent,
            PRecord::Open,
            PRecord::IndentEnd,
            Pattern::SpaceBefore
        )
        .parse(arena, state)?;

        let result = Pattern::RecordDestructure(fields);

        Ok((MadeProgress, result, state))
    }
}

fn record_pattern_field<'a>(min_indent: u32) -> impl Parser<'a, Loc<Pattern<'a>>, PRecord<'a>> {
    use crate::parser::Either::*;

    move |arena, state: State<'a>| {
        // You must have a field name, e.g. "email"
        // using the initial pos is important for error reporting
        let pos = state.pos();
        let (progress, loc_label, state) = loc!(specialize(
            move |_, _| PRecord::Field(pos),
            lowercase_ident()
        ))
        .parse(arena, state)?;
        debug_assert_eq!(progress, MadeProgress);

        let (_, spaces, state) = space0_e(min_indent, PRecord::IndentEnd).parse(arena, state)?;

        // Having a value is optional; both `{ email }` and `{ email: blah }` work.
        // (This is true in both literals and types.)
        let (_, opt_loc_val, state) = optional(either!(
            word1(b':', PRecord::Colon),
            word1(b'?', PRecord::Optional)
        ))
        .parse(arena, state)?;

        match opt_loc_val {
            Some(First(_)) => {
                let val_parser = specialize_ref(PRecord::Pattern, loc_pattern_help(min_indent));
                let (_, loc_val, state) =
                    space0_before_e(val_parser, min_indent, PRecord::IndentColon)
                        .parse(arena, state)?;

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
                let val_parser = specialize_ref(PRecord::Expr, move |a, s| {
                    crate::expr::parse_loc_expr_no_multi_backpassing(min_indent, a, s)
                });

                let (_, loc_val, state) =
                    space0_before_e(val_parser, min_indent, PRecord::IndentColon)
                        .parse(arena, state)?;

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
