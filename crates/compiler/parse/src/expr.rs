use crate::ast::{
    AssignedField, Collection, CommentOrNewline, Defs, Expr, ExtractSpaces, Has, HasAbilities,
    Pattern, Spaceable, TypeAnnotation, TypeDef, TypeHeader, ValueDef,
};
use crate::blankspace::{
    space0_after_e, space0_around_e_no_after_indent_check, space0_around_ee, space0_before_e,
    space0_before_optional_after, space0_e,
};
use crate::ident::{integer_ident, lowercase_ident, parse_ident, Accessor, Ident};
use crate::keyword;
use crate::parser::{
    self, backtrackable, increment_min_indent, line_min_indent, optional, reset_min_indent,
    sep_by1, sep_by1_e, set_min_indent, specialize, specialize_ref, then, trailing_sep_by0, word1,
    word1_indent, word2, EClosure, EExpect, EExpr, EIf, EInParens, EList, ENumber, EPattern,
    ERecord, EString, ETuple, EType, EWhen, Either, ParseResult, Parser,
};
use crate::pattern::{closure_param, loc_has_parser};
use crate::state::State;
use crate::type_annotation;
use bumpalo::collections::Vec;
use bumpalo::Bump;
use roc_collections::soa::Slice;
use roc_module::called_via::{BinOp, CalledVia, UnaryOp};
use roc_region::all::{Loc, Position, Region};

use crate::parser::Progress::{self, *};

fn expr_end<'a>() -> impl Parser<'a, (), EExpr<'a>> {
    |_arena, state: State<'a>, _min_indent: u32| {
        if state.has_reached_end() {
            Ok((NoProgress, (), state))
        } else {
            Err((NoProgress, EExpr::BadExprEnd(state.pos())))
        }
    }
}

pub fn test_parse_expr<'a>(
    min_indent: u32,
    arena: &'a bumpalo::Bump,
    state: State<'a>,
) -> Result<Loc<Expr<'a>>, EExpr<'a>> {
    let parser = skip_second!(space0_before_e(loc_expr(), EExpr::IndentStart,), expr_end());

    match parser.parse(arena, state, min_indent) {
        Ok((_, expression, _)) => Ok(expression),
        Err((_, fail)) => Err(fail),
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ExprParseOptions {
    /// Check for and accept multi-backpassing syntax
    /// This is usually true, but false within list/record literals
    /// because the comma separating backpassing arguments conflicts
    /// with the comma separating literal elements
    pub accept_multi_backpassing: bool,

    /// Check for the `->` token, and raise an error if found
    /// This is usually true, but false in if-guards
    ///
    /// > Just foo if foo == 2 -> ...
    pub check_for_arrow: bool,
}

impl Default for ExprParseOptions {
    fn default() -> Self {
        ExprParseOptions {
            accept_multi_backpassing: true,
            check_for_arrow: true,
        }
    }
}

pub fn expr_help<'a>() -> impl Parser<'a, Expr<'a>, EExpr<'a>> {
    move |arena, state: State<'a>, min_indent: u32| {
        loc_expr()
            .parse(arena, state, min_indent)
            .map(|(a, b, c)| (a, b.value, c))
    }
}

fn loc_expr_in_parens_help<'a>() -> impl Parser<'a, Loc<Expr<'a>>, EInParens<'a>> {
    then(
        loc!(collection_trailing_sep_e!(
            word1(b'(', EInParens::Open),
            specialize_ref(EInParens::Expr, loc_expr_no_multi_backpassing()),
            word1(b',', EInParens::End),
            word1(b')', EInParens::End),
            EInParens::IndentEnd,
            Expr::SpaceBefore
        )),
        move |arena, state, _, loc_elements| {
            let elements = loc_elements.value;
            let region = loc_elements.region;

            if elements.len() > 1 {
                Ok((
                    MadeProgress,
                    Loc::at(region, Expr::Tuple(elements.ptrify_items(arena))),
                    state,
                ))
            } else if elements.is_empty() {
                Err((NoProgress, EInParens::Empty(state.pos())))
            } else {
                // TODO: don't discard comments before/after
                // (stored in the Collection)
                Ok((
                    MadeProgress,
                    Loc::at(
                        elements.items[0].region,
                        Expr::ParensAround(&elements.items[0].value),
                    ),
                    state,
                ))
            }
        },
    )
    .trace("in_parens")
}

fn loc_expr_in_parens_etc_help<'a>() -> impl Parser<'a, Loc<Expr<'a>>, EExpr<'a>> {
    map_with_arena!(
        loc!(and!(
            specialize(EExpr::InParens, loc_expr_in_parens_help()),
            record_field_access_chain()
        )),
        move |arena: &'a Bump, value: Loc<(Loc<Expr<'a>>, Vec<'a, Accessor<'a>>)>| {
            let Loc {
                mut region,
                value: (loc_expr, field_accesses),
            } = value;

            let mut value = loc_expr.value;

            // if there are field accesses, include the parentheses in the region
            // otherwise, don't include the parentheses
            if field_accesses.is_empty() {
                region = loc_expr.region;
            } else {
                value = apply_expr_access_chain(arena, value, field_accesses);
            }

            Loc::at(region, value)
        }
    )
}

fn record_field_access_chain<'a>() -> impl Parser<'a, Vec<'a, Accessor<'a>>, EExpr<'a>> {
    zero_or_more!(skip_first!(
        word1(b'.', EExpr::Access),
        specialize(
            |_, pos| EExpr::Access(pos),
            one_of!(
                map!(lowercase_ident(), Accessor::RecordField),
                map!(integer_ident(), Accessor::TupleIndex),
            )
        )
    ))
}

/// In some contexts we want to parse the `_` as an expression, so it can then be turned into a
/// pattern later
fn loc_term_or_underscore_or_conditional<'a>(
    options: ExprParseOptions,
) -> impl Parser<'a, Loc<Expr<'a>>, EExpr<'a>> {
    one_of!(
        loc_expr_in_parens_etc_help(),
        loc!(specialize(EExpr::If, if_expr_help(options))),
        loc!(specialize(EExpr::When, when::expr_help(options))),
        loc!(specialize(EExpr::Str, string_literal_help())),
        loc!(specialize(EExpr::SingleQuote, single_quote_literal_help())),
        loc!(specialize(EExpr::Number, positive_number_literal_help())),
        loc!(specialize(EExpr::Closure, closure_help(options))),
        loc!(crash_kw()),
        loc!(underscore_expression()),
        loc!(record_literal_help()),
        loc!(specialize(EExpr::List, list_literal_help())),
        loc!(map_with_arena!(
            assign_or_destructure_identifier(),
            ident_to_expr
        )),
    )
}

/// In some contexts we want to parse the `_` as an expression, so it can then be turned into a
/// pattern later
fn loc_term_or_underscore<'a>(
    options: ExprParseOptions,
) -> impl Parser<'a, Loc<Expr<'a>>, EExpr<'a>> {
    one_of!(
        loc_expr_in_parens_etc_help(),
        loc!(specialize(EExpr::Str, string_literal_help())),
        loc!(specialize(EExpr::SingleQuote, single_quote_literal_help())),
        loc!(specialize(EExpr::Number, positive_number_literal_help())),
        loc!(specialize(EExpr::Closure, closure_help(options))),
        loc!(underscore_expression()),
        loc!(record_literal_help()),
        loc!(specialize(EExpr::List, list_literal_help())),
        loc!(map_with_arena!(
            assign_or_destructure_identifier(),
            ident_to_expr
        )),
    )
}

fn loc_term<'a>(options: ExprParseOptions) -> impl Parser<'a, Loc<Expr<'a>>, EExpr<'a>> {
    one_of!(
        loc_expr_in_parens_etc_help(),
        loc!(specialize(EExpr::Str, string_literal_help())),
        loc!(specialize(EExpr::SingleQuote, single_quote_literal_help())),
        loc!(specialize(EExpr::Number, positive_number_literal_help())),
        loc!(specialize(EExpr::Closure, closure_help(options))),
        loc!(record_literal_help()),
        loc!(specialize(EExpr::List, list_literal_help())),
        loc!(map_with_arena!(
            assign_or_destructure_identifier(),
            ident_to_expr
        )),
    )
}

fn underscore_expression<'a>() -> impl Parser<'a, Expr<'a>, EExpr<'a>> {
    move |arena: &'a Bump, state: State<'a>, min_indent: u32| {
        let start = state.pos();

        let (_, _, next_state) = word1(b'_', EExpr::Underscore).parse(arena, state, min_indent)?;

        let lowercase_ident_expr = { specialize(move |_, _| EExpr::End(start), lowercase_ident()) };

        let (_, output, final_state) =
            optional(lowercase_ident_expr).parse(arena, next_state, min_indent)?;

        match output {
            Some(name) => Ok((MadeProgress, Expr::Underscore(name), final_state)),
            None => Ok((MadeProgress, Expr::Underscore(""), final_state)),
        }
    }
}

fn crash_kw<'a>() -> impl Parser<'a, Expr<'a>, EExpr<'a>> {
    move |arena: &'a Bump, state: State<'a>, min_indent: u32| {
        let (_, _, next_state) = crate::parser::keyword_e(crate::keyword::CRASH, EExpr::Crash)
            .parse(arena, state, min_indent)?;

        Ok((MadeProgress, Expr::Crash, next_state))
    }
}

fn loc_possibly_negative_or_negated_term<'a>(
    options: ExprParseOptions,
) -> impl Parser<'a, Loc<Expr<'a>>, EExpr<'a>> {
    one_of![
        |arena, state: State<'a>, min_indent: u32| {
            let initial = state.clone();

            let (_, (loc_op, loc_expr), state) =
                and!(loc!(unary_negate()), loc_term(options)).parse(arena, state, min_indent)?;

            let loc_expr = numeric_negate_expression(arena, initial, loc_op, loc_expr, &[]);

            Ok((MadeProgress, loc_expr, state))
        },
        // this will parse negative numbers, which the unary negate thing up top doesn't (for now)
        loc!(specialize(EExpr::Number, number_literal_help())),
        loc!(map_with_arena!(
            and!(loc!(word1(b'!', EExpr::Start)), loc_term(options)),
            |arena: &'a Bump, (loc_op, loc_expr): (Loc<_>, _)| {
                Expr::UnaryOp(arena.alloc(loc_expr), Loc::at(loc_op.region, UnaryOp::Not))
            }
        )),
        loc_term_or_underscore_or_conditional(options)
    ]
}

fn fail_expr_start_e<'a, T: 'a>() -> impl Parser<'a, T, EExpr<'a>> {
    |_arena, state: State<'a>, _min_indent: u32| Err((NoProgress, EExpr::Start(state.pos())))
}

fn unary_negate<'a>() -> impl Parser<'a, (), EExpr<'a>> {
    move |_arena: &'a Bump, state: State<'a>, _min_indent: u32| {
        // a minus is unary iff
        //
        // - it is preceded by whitespace (spaces, newlines, comments)
        // - it is not followed by whitespace
        let followed_by_whitespace = state
            .bytes()
            .get(1)
            .map(|c| c.is_ascii_whitespace() || *c == b'#')
            .unwrap_or(false);

        if state.bytes().starts_with(b"-") && !followed_by_whitespace {
            // the negate is only unary if it is not followed by whitespace
            let state = state.advance(1);
            Ok((MadeProgress, (), state))
        } else {
            // this is not a negated expression
            Err((NoProgress, EExpr::UnaryNot(state.pos())))
        }
    }
}

fn expr_start<'a>(options: ExprParseOptions) -> impl Parser<'a, Loc<Expr<'a>>, EExpr<'a>> {
    one_of![
        loc!(specialize(EExpr::If, if_expr_help(options))),
        loc!(specialize(EExpr::When, when::expr_help(options))),
        loc!(specialize(EExpr::Expect, expect_help(options))),
        loc!(specialize(EExpr::Dbg, dbg_help(options))),
        loc!(specialize(EExpr::Closure, closure_help(options))),
        loc!(expr_operator_chain(options)),
        fail_expr_start_e()
    ]
    .trace("expr_start")
}

fn expr_operator_chain<'a>(options: ExprParseOptions) -> impl Parser<'a, Expr<'a>, EExpr<'a>> {
    line_min_indent(move |arena, state: State<'a>, min_indent: u32| {
        let (_, expr, state) =
            loc_possibly_negative_or_negated_term(options).parse(arena, state, min_indent)?;

        let initial_state = state.clone();
        let end = state.pos();

        match space0_e(EExpr::IndentEnd).parse(arena, state.clone(), min_indent) {
            Err((_, _)) => Ok((MadeProgress, expr.value, state)),
            Ok((_, spaces_before_op, state)) => {
                let expr_state = ExprState {
                    operators: Vec::new_in(arena),
                    arguments: Vec::new_in(arena),
                    expr,
                    spaces_after: spaces_before_op,
                    end,
                };

                parse_expr_end(min_indent, options, expr_state, arena, state, initial_state)
            }
        }
    })
}

#[derive(Debug)]
struct ExprState<'a> {
    operators: Vec<'a, (Loc<Expr<'a>>, Loc<BinOp>)>,
    arguments: Vec<'a, &'a Loc<Expr<'a>>>,
    expr: Loc<Expr<'a>>,
    spaces_after: &'a [CommentOrNewline<'a>],
    end: Position,
}

impl<'a> ExprState<'a> {
    fn consume_spaces(&mut self, arena: &'a Bump) {
        if !self.spaces_after.is_empty() {
            if let Some(last) = self.arguments.pop() {
                let new = last.value.with_spaces_after(self.spaces_after, last.region);

                self.arguments.push(arena.alloc(new));
            } else {
                let region = self.expr.region;

                let mut value = Expr::Num("");
                std::mem::swap(&mut self.expr.value, &mut value);

                self.expr = arena
                    .alloc(value)
                    .with_spaces_after(self.spaces_after, region);
            };

            self.spaces_after = &[];
        }
    }

    fn validate_assignment_or_backpassing<F>(
        mut self,
        arena: &'a Bump,
        loc_op: Loc<BinOp>,
        argument_error: F,
    ) -> Result<Loc<Expr<'a>>, EExpr<'a>>
    where
        F: Fn(Region, Position) -> EExpr<'a>,
    {
        if !self.operators.is_empty() {
            // this `=` or `<-` likely occurred inline; treat it as an invalid operator
            let opchar = match loc_op.value {
                BinOp::Assignment => "=",
                BinOp::Backpassing => "<-",
                _ => unreachable!(),
            };

            let fail = EExpr::BadOperator(opchar, loc_op.region.start());

            Err(fail)
        } else if !self.expr.value.is_tag()
            && !self.expr.value.is_opaque()
            && !self.arguments.is_empty()
        {
            let region = Region::across_all(self.arguments.iter().map(|v| &v.region));

            Err(argument_error(region, loc_op.region.start()))
        } else {
            self.consume_spaces(arena);
            Ok(to_call(arena, self.arguments, self.expr))
        }
    }

    fn validate_is_type_def(
        mut self,
        arena: &'a Bump,
        loc_op: Loc<BinOp>,
        kind: AliasOrOpaque,
    ) -> Result<(Loc<Expr<'a>>, Vec<'a, &'a Loc<Expr<'a>>>), EExpr<'a>> {
        debug_assert_eq!(
            loc_op.value,
            match kind {
                AliasOrOpaque::Alias => BinOp::IsAliasType,
                AliasOrOpaque::Opaque => BinOp::IsOpaqueType,
            }
        );

        if !self.operators.is_empty() {
            // this `:`/`:=` likely occurred inline; treat it as an invalid operator
            let op = match kind {
                AliasOrOpaque::Alias => ":",
                AliasOrOpaque::Opaque => ":=",
            };
            let fail = EExpr::BadOperator(op, loc_op.region.start());

            Err(fail)
        } else {
            self.consume_spaces(arena);
            Ok((self.expr, self.arguments))
        }
    }
}

#[allow(clippy::unnecessary_wraps)]
fn parse_expr_final<'a>(expr_state: ExprState<'a>, arena: &'a Bump) -> Expr<'a> {
    let right_arg = to_call(arena, expr_state.arguments, expr_state.expr);

    if expr_state.operators.is_empty() {
        right_arg.value
    } else {
        Expr::BinOps(
            expr_state.operators.into_bump_slice(),
            arena.alloc(right_arg),
        )
    }
}

fn to_call<'a>(
    arena: &'a Bump,
    mut arguments: Vec<'a, &'a Loc<Expr<'a>>>,
    loc_expr1: Loc<Expr<'a>>,
) -> Loc<Expr<'a>> {
    if arguments.is_empty() {
        loc_expr1
    } else {
        let last = arguments.last().map(|x| x.region).unwrap_or_default();
        let region = Region::span_across(&loc_expr1.region, &last);

        let spaces = if let Some(last) = arguments.last_mut() {
            let spaces = last.value.extract_spaces();

            if spaces.after.is_empty() {
                &[]
            } else {
                let inner = if !spaces.before.is_empty() {
                    arena.alloc(spaces.item).before(spaces.before)
                } else {
                    spaces.item
                };
                *last = arena.alloc(Loc::at(last.region, inner));

                spaces.after
            }
        } else {
            &[]
        };

        let mut apply = Expr::Apply(
            arena.alloc(loc_expr1),
            arguments.into_bump_slice(),
            CalledVia::Space,
        );

        if !spaces.is_empty() {
            apply = arena.alloc(apply).after(spaces)
        }

        Loc::at(region, apply)
    }
}

fn numeric_negate_expression<'a, T>(
    arena: &'a Bump,
    state: State<'a>,
    loc_op: Loc<T>,
    expr: Loc<Expr<'a>>,
    spaces: &'a [CommentOrNewline<'a>],
) -> Loc<Expr<'a>> {
    debug_assert_eq!(state.bytes().first(), Some(&b'-'));
    // for overflow reasons, we must make the unary minus part of the number literal.
    let start = state.pos();
    let region = Region::new(start, expr.region.end());

    let new_expr = match expr.value {
        Expr::Num(string) => {
            let new_string =
                unsafe { std::str::from_utf8_unchecked(&state.bytes()[..string.len() + 1]) };

            Expr::Num(new_string)
        }
        Expr::Float(string) => {
            let new_string =
                unsafe { std::str::from_utf8_unchecked(&state.bytes()[..string.len() + 1]) };

            Expr::Float(new_string)
        }
        Expr::NonBase10Int {
            string,
            base,
            is_negative,
        } => {
            // don't include the minus sign here; it will not be parsed right
            Expr::NonBase10Int {
                is_negative: !is_negative,
                string,
                base,
            }
        }
        _ => Expr::UnaryOp(arena.alloc(expr), Loc::at(loc_op.region, UnaryOp::Negate)),
    };

    let new_loc_expr = Loc::at(region, new_expr);

    if spaces.is_empty() {
        new_loc_expr
    } else {
        arena
            .alloc(new_loc_expr.value)
            .with_spaces_before(spaces, new_loc_expr.region)
    }
}

pub fn parse_single_def<'a>(
    _options: ExprParseOptions,
    min_indent: u32,
    arena: &'a Bump,
    state: State<'a>,
) -> ParseResult<'a, Option<SingleDef<'a>>, EExpr<'a>> {
    let initial = state.clone();

    let mut spaces_before_current = &[] as &[_];
    let spaces_before_current_start = state.pos();

    let state = match space0_e(EExpr::IndentStart).parse(arena, state, min_indent) {
        Err((MadeProgress, _)) => {
            return Err((MadeProgress, EExpr::DefMissingFinalExpr(initial.pos())));
        }
        Ok((_, spaces, state)) => {
            spaces_before_current = spaces;
            state
        }
        Err((NoProgress, _)) => initial.clone(),
    };

    let start = state.pos();

    let parse_dbg = crate::parser::keyword_e(crate::keyword::DBG, EExpect::Dbg);
    let parse_expect_vanilla = crate::parser::keyword_e(crate::keyword::EXPECT, EExpect::Expect);
    let parse_expect_fx = crate::parser::keyword_e(crate::keyword::EXPECT_FX, EExpect::Expect);
    let parse_expect = either!(parse_expect_fx, parse_expect_vanilla);

    match space0_after_e(crate::pattern::loc_pattern_help(), EPattern::IndentEnd).parse(
        arena,
        state.clone(),
        min_indent,
    ) {
        Err((NoProgress, _)) => {
            match parse_expect.parse(arena, state.clone(), min_indent) {
                Err((_, _)) => {
                    match parse_dbg.parse(arena, state, min_indent) {
                        Ok((_, _, state)) => parse_statement_inside_def(
                            arena,
                            state,
                            min_indent,
                            start,
                            spaces_before_current_start,
                            spaces_before_current,
                            |preceding_comment, loc_def_expr| ValueDef::Dbg {
                                condition: arena.alloc(loc_def_expr),
                                preceding_comment,
                            },
                        ),
                        Err((_, _)) => {
                            // a hacky way to get expression-based error messages. TODO fix this
                            Ok((NoProgress, None, initial))
                        }
                    }
                }
                Ok((_, expect_flavor, state)) => parse_statement_inside_def(
                    arena,
                    state,
                    min_indent,
                    start,
                    spaces_before_current_start,
                    spaces_before_current,
                    |preceding_comment, loc_def_expr| match expect_flavor {
                        Either::Second(_) => ValueDef::Expect {
                            condition: arena.alloc(loc_def_expr),
                            preceding_comment,
                        },
                        Either::First(_) => ValueDef::ExpectFx {
                            condition: arena.alloc(loc_def_expr),
                            preceding_comment,
                        },
                    },
                ),
            }
        }
        Err((MadeProgress, _)) => {
            // a hacky way to get expression-based error messages. TODO fix this
            Ok((NoProgress, None, initial))
        }
        Ok((_, loc_pattern, state)) => {
            // First let's check whether this is an ability definition.
            let opt_tag_and_args: Option<(&str, Region, &[Loc<Pattern>])> = match loc_pattern.value
            {
                Pattern::Apply(
                    Loc {
                        value: Pattern::Tag(name),
                        region,
                    },
                    args,
                ) => Some((name, *region, args)),
                Pattern::Tag(name) => Some((name, loc_pattern.region, &[])),
                _ => None,
            };

            if let Some((name, name_region, args)) = opt_tag_and_args {
                if let Ok((_, loc_has, state)) =
                    loc_has_parser().parse(arena, state.clone(), min_indent)
                {
                    let (_, (type_def, def_region), state) = finish_parsing_ability_def_help(
                        min_indent,
                        Loc::at(name_region, name),
                        args,
                        loc_has,
                        arena,
                        state,
                    )?;

                    return Ok((
                        MadeProgress,
                        Some(SingleDef {
                            type_or_value: Either::First(type_def),
                            region: def_region,
                            spaces_before: spaces_before_current,
                        }),
                        state,
                    ));
                }
            }

            // Otherwise, this is a def or alias.
            match operator().parse(arena, state, min_indent) {
                Ok((_, BinOp::Assignment, state)) => {
                    let parse_def_expr =
                        space0_before_e(increment_min_indent(loc_expr()), EExpr::IndentEnd);

                    let (_, loc_def_expr, state) =
                        parse_def_expr.parse(arena, state, min_indent)?;
                    let value_def =
                        ValueDef::Body(arena.alloc(loc_pattern), &*arena.alloc(loc_def_expr));
                    let region = Region::span_across(&loc_pattern.region, &loc_def_expr.region);

                    Ok((
                        MadeProgress,
                        Some(SingleDef {
                            type_or_value: Either::Second(value_def),
                            region,
                            spaces_before: spaces_before_current,
                        }),
                        state,
                    ))
                }
                Ok((_, BinOp::IsAliasType, state)) => {
                    // the increment_min_indent here is probably _wrong_, since alias_signature_with_space_before does
                    // that internally.
                    // TODO: re-evaluate this
                    let parser = increment_min_indent(alias_signature_with_space_before());
                    let (_, ann_type, state) = parser.parse(arena, state, min_indent)?;
                    let region = Region::span_across(&loc_pattern.region, &ann_type.region);

                    match &loc_pattern.value {
                        Pattern::Apply(
                            Loc {
                                value: Pattern::Tag(name),
                                ..
                            },
                            alias_arguments,
                        ) => {
                            let name = Loc::at(loc_pattern.region, *name);
                            let header = TypeHeader {
                                name,
                                vars: alias_arguments,
                            };

                            let type_def = TypeDef::Alias {
                                header,
                                ann: ann_type,
                            };

                            Ok((
                                MadeProgress,
                                Some(SingleDef {
                                    type_or_value: Either::First(type_def),
                                    region,
                                    spaces_before: spaces_before_current,
                                }),
                                state,
                            ))
                        }
                        Pattern::Tag(name) => {
                            let name = Loc::at(loc_pattern.region, *name);
                            let pattern_arguments: &'a [Loc<Pattern<'a>>] = &[];
                            let header = TypeHeader {
                                name,
                                vars: pattern_arguments,
                            };

                            let type_def = TypeDef::Alias {
                                header,
                                ann: ann_type,
                            };

                            Ok((
                                MadeProgress,
                                Some(SingleDef {
                                    type_or_value: Either::First(type_def),
                                    region,
                                    spaces_before: spaces_before_current,
                                }),
                                state,
                            ))
                        }
                        _ => {
                            let value_def = ValueDef::Annotation(loc_pattern, ann_type);

                            Ok((
                                MadeProgress,
                                Some(SingleDef {
                                    type_or_value: Either::Second(value_def),
                                    region,
                                    spaces_before: spaces_before_current,
                                }),
                                state,
                            ))
                        }
                    }
                }
                Ok((_, BinOp::IsOpaqueType, state)) => {
                    let (_, (signature, derived), state) =
                        opaque_signature_with_space_before().parse(arena, state, min_indent + 1)?;
                    let region = Region::span_across(&loc_pattern.region, &signature.region);

                    match &loc_pattern.value {
                        Pattern::Apply(
                            Loc {
                                value: Pattern::Tag(name),
                                ..
                            },
                            alias_arguments,
                        ) => {
                            let name = Loc::at(loc_pattern.region, *name);
                            let header = TypeHeader {
                                name,
                                vars: alias_arguments,
                            };

                            let type_def = TypeDef::Opaque {
                                header,
                                typ: signature,
                                derived,
                            };

                            Ok((
                                MadeProgress,
                                Some(SingleDef {
                                    type_or_value: Either::First(type_def),
                                    region,
                                    spaces_before: spaces_before_current,
                                }),
                                state,
                            ))
                        }
                        Pattern::Tag(name) => {
                            let name = Loc::at(loc_pattern.region, *name);
                            let pattern_arguments: &'a [Loc<Pattern<'a>>] = &[];
                            let header = TypeHeader {
                                name,
                                vars: pattern_arguments,
                            };

                            let type_def = TypeDef::Opaque {
                                header,
                                typ: signature,
                                derived,
                            };

                            Ok((
                                MadeProgress,
                                Some(SingleDef {
                                    type_or_value: Either::First(type_def),
                                    region,
                                    spaces_before: spaces_before_current,
                                }),
                                state,
                            ))
                        }
                        _ => {
                            let value_def = ValueDef::Annotation(loc_pattern, signature);

                            Ok((
                                MadeProgress,
                                Some(SingleDef {
                                    type_or_value: Either::Second(value_def),
                                    region,
                                    spaces_before: spaces_before_current,
                                }),
                                state,
                            ))
                        }
                    }
                }
                _ => Ok((MadeProgress, None, initial)),
            }
        }
    }
}

/// e.g. Things that can be on their own line in a def, e.g. `expect`, `expect-fx`, or `dbg`
fn parse_statement_inside_def<'a>(
    arena: &'a Bump,
    state: State<'a>,
    min_indent: u32,
    start: Position,
    spaces_before_current_start: Position,
    spaces_before_current: &'a [CommentOrNewline<'a>],
    get_value_def: impl Fn(Region, Loc<Expr<'a>>) -> ValueDef<'a>,
) -> Result<(Progress, Option<SingleDef<'a>>, State<'a>), (Progress, EExpr<'a>)> {
    let parse_def_expr = space0_before_e(increment_min_indent(loc_expr()), EExpr::IndentEnd);
    let (_, loc_def_expr, state) = parse_def_expr.parse(arena, state, min_indent)?;
    let end = loc_def_expr.region.end();
    let region = Region::new(start, end);

    // drop newlines before the preceding comment
    let spaces_before_start = spaces_before_current_start.offset as usize;
    let spaces_before_end = start.offset as usize;
    let mut spaces_before_current_start = spaces_before_current_start;

    for byte in &state.original_bytes()[spaces_before_start..spaces_before_end] {
        match byte {
            b' ' | b'\n' => {
                spaces_before_current_start.offset += 1;
            }
            _ => break,
        }
    }

    let preceding_comment = Region::new(spaces_before_current_start, start);
    let value_def = get_value_def(preceding_comment, loc_def_expr);

    Ok((
        MadeProgress,
        Some(SingleDef {
            type_or_value: Either::Second(value_def),
            region,
            spaces_before: spaces_before_current,
        }),
        state,
    ))
}

// This is a macro only because trying to make it be a function caused lifetime issues.
#[macro_export]
macro_rules! join_ann_to_body {
    ($arena:expr, $loc_pattern:expr, $loc_def_expr:expr, $ann_pattern:expr, $ann_type:expr, $spaces_before_current:expr, $region:expr) => {{
        // join this body with the preceding annotation

        let value_def = ValueDef::AnnotatedBody {
            ann_pattern: $arena.alloc(*$ann_pattern),
            ann_type: $arena.alloc(*$ann_type),
            comment: $spaces_before_current
                .first()
                .and_then($crate::ast::CommentOrNewline::comment_str),
            body_pattern: $arena.alloc($loc_pattern),
            body_expr: *$arena.alloc($loc_def_expr),
        };

        (
            value_def,
            roc_region::all::Region::span_across(&$ann_pattern.region, &$region),
        )
    }};
}

// This is a macro only because trying to make it be a function caused lifetime issues.
#[macro_export]
macro_rules! join_alias_to_body {
    ($arena:expr, $loc_pattern:expr, $loc_def_expr:expr, $header:expr, $ann_type:expr, $spaces_before_current:expr, $region:expr) => {{
        use roc_region::all::Region;

        // This is a case like
        //   UserId x : [UserId Int]
        //   UserId x = UserId 42
        // We optimistically parsed the first line as an alias; we now turn it
        // into an annotation.

        let loc_name = $arena.alloc($header.name.map(|x| Pattern::Tag(x)));
        let ann_pattern = Pattern::Apply(loc_name, $header.vars);

        let vars_region = Region::across_all($header.vars.iter().map(|v| &v.region));
        let region_ann_pattern = Region::span_across(&loc_name.region, &vars_region);
        let loc_ann_pattern = Loc::at(region_ann_pattern, ann_pattern);

        let value_def = ValueDef::AnnotatedBody {
            ann_pattern: $arena.alloc(loc_ann_pattern),
            ann_type: $arena.alloc(*$ann_type),
            comment: $spaces_before_current
                .first()
                .and_then($crate::ast::CommentOrNewline::comment_str),
            body_pattern: $arena.alloc($loc_pattern),
            body_expr: *$arena.alloc($loc_def_expr),
        };

        (
            value_def,
            Region::span_across(&$header.name.region, &$region),
        )
    }};
}

fn parse_defs_end<'a>(
    _options: ExprParseOptions,
    min_indent: u32,
    mut defs: Defs<'a>,
    arena: &'a Bump,
    state: State<'a>,
) -> ParseResult<'a, Defs<'a>, EExpr<'a>> {
    let mut global_state = state;

    loop {
        let state = global_state;

        global_state = match parse_single_def(_options, min_indent, arena, state) {
            Ok((_, Some(single_def), next_state)) => {
                let region = single_def.region;
                let spaces_before_current = single_def.spaces_before;

                match single_def.type_or_value {
                    Either::First(type_def) => {
                        defs.push_type_def(type_def, region, spaces_before_current, &[]);
                    }
                    Either::Second(value_def) => {
                        // If we got a ValueDef::Body, check if a type annotation preceded it.
                        // If so, we may need to combine them into an AnnotatedBody.
                        let joined = match value_def {
                            ValueDef::Body(loc_pattern, loc_def_expr)
                                if spaces_before_current.len() <= 1 =>
                            {
                                let region =
                                    Region::span_across(&loc_pattern.region, &loc_def_expr.region);

                                match defs.last() {
                                    Some(Err(ValueDef::Annotation(ann_pattern, ann_type))) => {
                                        let (value_def, region) = join_ann_to_body!(
                                            arena,
                                            loc_pattern,
                                            loc_def_expr,
                                            ann_pattern,
                                            ann_type,
                                            spaces_before_current,
                                            region
                                        );

                                        defs.replace_with_value_def(
                                            defs.tags.len() - 1,
                                            value_def,
                                            region,
                                        );

                                        true
                                    }
                                    Some(Ok(TypeDef::Alias {
                                        header,
                                        ann: ann_type,
                                    })) => {
                                        let (value_def, region) = join_alias_to_body!(
                                            arena,
                                            loc_pattern,
                                            loc_def_expr,
                                            header,
                                            ann_type,
                                            spaces_before_current,
                                            region
                                        );

                                        defs.replace_with_value_def(
                                            defs.tags.len() - 1,
                                            value_def,
                                            region,
                                        );

                                        true
                                    }
                                    _ => false,
                                }
                            }
                            _ => false,
                        };

                        if !joined {
                            // the previous and current def can't be joined up
                            defs.push_value_def(value_def, region, spaces_before_current, &[]);
                        }
                    }
                }

                next_state
            }
            Ok((progress, None, s)) => return Ok((progress, defs, s)),
            Err((progress, err)) => return Err((progress, err)),
        };
    }
}

pub struct SingleDef<'a> {
    pub type_or_value: Either<TypeDef<'a>, ValueDef<'a>>,
    pub region: Region,
    pub spaces_before: &'a [CommentOrNewline<'a>],
}

fn parse_defs_expr<'a>(
    options: ExprParseOptions,
    min_indent: u32,
    defs: Defs<'a>,
    arena: &'a Bump,
    state: State<'a>,
) -> ParseResult<'a, Expr<'a>, EExpr<'a>> {
    match parse_defs_end(options, min_indent, defs, arena, state) {
        Err(bad) => Err(bad),
        Ok((_, def_state, state)) => {
            // this is no def, because there is no `=` or `:`; parse as an expr
            let parse_final_expr = space0_before_e(loc_expr(), EExpr::IndentEnd);

            match parse_final_expr.parse(arena, state.clone(), min_indent) {
                Err((_, fail)) => {
                    return Err((
                        MadeProgress,
                        EExpr::DefMissingFinalExpr2(arena.alloc(fail), state.pos()),
                    ));
                }
                Ok((_, loc_ret, state)) => {
                    return Ok((
                        MadeProgress,
                        Expr::Defs(arena.alloc(def_state), arena.alloc(loc_ret)),
                        state,
                    ));
                }
            }
        }
    }
}

fn alias_signature_with_space_before<'a>() -> impl Parser<'a, Loc<TypeAnnotation<'a>>, EExpr<'a>> {
    increment_min_indent(specialize(
        EExpr::Type,
        space0_before_e(type_annotation::located(false), EType::TIndentStart),
    ))
}

fn opaque_signature_with_space_before<'a>(
) -> impl Parser<'a, (Loc<TypeAnnotation<'a>>, Option<Loc<HasAbilities<'a>>>), EExpr<'a>> {
    and!(
        specialize(
            EExpr::Type,
            space0_before_e(
                type_annotation::located_opaque_signature(true),
                EType::TIndentStart,
            ),
        ),
        optional(specialize(
            EExpr::Type,
            space0_before_e(type_annotation::has_abilities(), EType::TIndentStart,),
        ))
    )
}

#[derive(Copy, Clone, PartialEq, Eq)]
enum AliasOrOpaque {
    Alias,
    Opaque,
}

#[allow(clippy::too_many_arguments)]
fn finish_parsing_alias_or_opaque<'a>(
    min_indent: u32,
    options: ExprParseOptions,
    expr_state: ExprState<'a>,
    loc_op: Loc<BinOp>,
    arena: &'a Bump,
    state: State<'a>,
    spaces_after_operator: &'a [CommentOrNewline<'a>],
    kind: AliasOrOpaque,
) -> ParseResult<'a, Expr<'a>, EExpr<'a>> {
    let expr_region = expr_state.expr.region;
    let indented_more = min_indent + 1;

    let (expr, arguments) = expr_state
        .validate_is_type_def(arena, loc_op, kind)
        .map_err(|fail| (MadeProgress, fail))?;

    let mut defs = Defs::default();

    let state = match &expr.value {
        Expr::Tag(name) => {
            let mut type_arguments = Vec::with_capacity_in(arguments.len(), arena);

            for argument in arguments {
                match expr_to_pattern_help(arena, &argument.value) {
                    Ok(good) => {
                        type_arguments.push(Loc::at(argument.region, good));
                    }
                    Err(_) => panic!(),
                }
            }

            match kind {
                AliasOrOpaque::Alias => {
                    let (_, signature, state) =
                        alias_signature_with_space_before().parse(arena, state, min_indent)?;

                    let def_region = Region::span_across(&expr.region, &signature.region);

                    let header = TypeHeader {
                        name: Loc::at(expr.region, name),
                        vars: type_arguments.into_bump_slice(),
                    };

                    let def = TypeDef::Alias {
                        header,
                        ann: signature,
                    };

                    defs.push_type_def(def, def_region, &[], &[]);

                    state
                }

                AliasOrOpaque::Opaque => {
                    let (_, (signature, derived), state) =
                        opaque_signature_with_space_before().parse(arena, state, indented_more)?;

                    let def_region = Region::span_across(&expr.region, &signature.region);

                    let header = TypeHeader {
                        name: Loc::at(expr.region, name),
                        vars: type_arguments.into_bump_slice(),
                    };

                    let def = TypeDef::Opaque {
                        header,
                        typ: signature,
                        derived,
                    };

                    defs.push_type_def(def, def_region, &[], &[]);

                    state
                }
            }
        }

        _ => {
            let call = to_call(arena, arguments, expr);

            match expr_to_pattern_help(arena, &call.value) {
                Ok(good) => {
                    let parser = specialize(
                        EExpr::Type,
                        space0_before_e(
                            set_min_indent(indented_more, type_annotation::located(false)),
                            EType::TIndentStart,
                        ),
                    );

                    match parser.parse(arena, state.clone(), min_indent) {
                        Err((_, fail)) => return Err((MadeProgress, fail)),
                        Ok((_, mut ann_type, state)) => {
                            // put the spaces from after the operator in front of the call
                            if !spaces_after_operator.is_empty() {
                                ann_type = arena
                                    .alloc(ann_type.value)
                                    .with_spaces_before(spaces_after_operator, ann_type.region);
                            }

                            let def_region = Region::span_across(&call.region, &ann_type.region);

                            let value_def =
                                ValueDef::Annotation(Loc::at(expr_region, good), ann_type);

                            defs.push_value_def(value_def, def_region, &[], &[]);

                            state
                        }
                    }
                }
                Err(_) => {
                    // this `:`/`:=` likely occurred inline; treat it as an invalid operator
                    let op = match kind {
                        AliasOrOpaque::Alias => ":",
                        AliasOrOpaque::Opaque => ":=",
                    };
                    let fail = EExpr::BadOperator(op, loc_op.region.start());

                    return Err((MadeProgress, fail));
                }
            }
        }
    };

    parse_defs_expr(options, min_indent, defs, arena, state)
}

mod ability {
    use super::*;
    use crate::{
        ast::{AbilityMember, Spaceable, Spaced},
        parser::EAbility,
    };

    /// Parses a single ability demand line; see `parse_demand`.
    fn parse_demand_help<'a>() -> impl Parser<'a, AbilityMember<'a>, EAbility<'a>> {
        map!(
            // Require the type to be more indented than the name
            absolute_indented_seq!(
                specialize(|_, pos| EAbility::DemandName(pos), loc!(lowercase_ident())),
                skip_first!(
                    and!(
                        // TODO: do we get anything from picking up spaces here?
                        space0_e(EAbility::DemandName),
                        word1(b':', EAbility::DemandColon)
                    ),
                    specialize(EAbility::Type, type_annotation::located(true))
                )
            ),
            |(name, typ): (Loc<&'a str>, Loc<TypeAnnotation<'a>>)| {
                AbilityMember {
                    name: name.map_owned(Spaced::Item),
                    typ,
                }
            }
        )
    }

    pub enum IndentLevel {
        PendingMin(u32),
        Exact(u32),
    }

    /// Parses an ability demand like `hash : a -> U64 | a has Hash`, in the context of a larger
    /// ability definition.
    /// This is basically the same as parsing a free-floating annotation, but with stricter rules.
    pub fn parse_demand<'a>(
        indent: IndentLevel,
    ) -> impl Parser<'a, (u32, AbilityMember<'a>), EAbility<'a>> {
        move |arena, state: State<'a>, min_indent: u32| {
            // Put no restrictions on the indent after the spaces; we'll check it manually.
            match space0_e(EAbility::DemandName).parse(arena, state, 0) {
                Err((MadeProgress, fail)) => Err((NoProgress, fail)),
                Err((NoProgress, fail)) => Err((NoProgress, fail)),

                Ok((_progress, spaces, state)) => {
                    match indent {
                        IndentLevel::PendingMin(min_indent) if state.column() < min_indent => {
                            let indent_difference = state.column() as i32 - min_indent as i32;
                            Err((
                                MadeProgress,
                                EAbility::DemandAlignment(indent_difference, state.pos()),
                            ))
                        }
                        IndentLevel::Exact(wanted) if state.column() < wanted => {
                            // This demand is not indented correctly
                            let indent_difference = state.column() as i32 - wanted as i32;
                            Err((
                                // Rollback because the deindent may be because there is a next
                                // expression
                                NoProgress,
                                EAbility::DemandAlignment(indent_difference, state.pos()),
                            ))
                        }
                        IndentLevel::Exact(wanted) if state.column() > wanted => {
                            // This demand is not indented correctly
                            let indent_difference = state.column() as i32 - wanted as i32;

                            // We might be trying to parse at EOF, at which case the indent level
                            // will be off, but there is actually nothing left.
                            let progress = if state.has_reached_end() {
                                NoProgress
                            } else {
                                MadeProgress
                            };

                            Err((
                                progress,
                                EAbility::DemandAlignment(indent_difference, state.pos()),
                            ))
                        }
                        _ => {
                            let indent_column = state.column();

                            let parser = parse_demand_help();

                            match parser.parse(arena, state.clone(), min_indent) {
                                Err((MadeProgress, fail)) => Err((MadeProgress, fail)),
                                Err((NoProgress, fail)) => {
                                    // We made progress relative to the entire ability definition,
                                    // so this is an error.
                                    Err((MadeProgress, fail))
                                }

                                Ok((_, mut demand, state)) => {
                                    // Tag spaces onto the parsed demand name
                                    if !spaces.is_empty() {
                                        demand.name = arena
                                            .alloc(demand.name.value)
                                            .with_spaces_before(spaces, demand.name.region);
                                    }

                                    Ok((MadeProgress, (indent_column, demand), state))
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

fn finish_parsing_ability_def_help<'a>(
    start_column: u32,
    name: Loc<&'a str>,
    args: &'a [Loc<Pattern<'a>>],
    loc_has: Loc<Has<'a>>,
    arena: &'a Bump,
    state: State<'a>,
) -> ParseResult<'a, (TypeDef<'a>, Region), EExpr<'a>> {
    let mut demands = Vec::with_capacity_in(2, arena);

    let min_indent_for_demand = start_column + 1;

    // Parse the first demand. This will determine the indentation level all the
    // other demands must observe.
    let start = state.pos();
    let (_, (demand_indent_level, first_demand), mut state) =
        ability::parse_demand(ability::IndentLevel::PendingMin(min_indent_for_demand))
            .parse(arena, state, min_indent_for_demand)
            .map_err(|(progress, err)| (progress, EExpr::Ability(err, start)))?;
    demands.push(first_demand);

    let demand_indent = ability::IndentLevel::Exact(demand_indent_level);
    let demand_parser = ability::parse_demand(demand_indent);

    loop {
        match demand_parser.parse(arena, state.clone(), min_indent_for_demand) {
            Ok((_, (_indent, demand), next_state)) => {
                state = next_state;
                demands.push(demand);
            }
            Err((MadeProgress, problem)) => {
                return Err((MadeProgress, EExpr::Ability(problem, state.pos())));
            }
            Err((NoProgress, _)) => {
                break;
            }
        }
    }

    let def_region = Region::span_across(&name.region, &demands.last().unwrap().typ.region);
    let type_def = TypeDef::Ability {
        header: TypeHeader { name, vars: args },
        loc_has,
        members: demands.into_bump_slice(),
    };

    Ok((MadeProgress, (type_def, def_region), state))
}

fn parse_expr_operator<'a>(
    min_indent: u32,
    options: ExprParseOptions,
    mut expr_state: ExprState<'a>,
    loc_op: Loc<BinOp>,
    arena: &'a Bump,
    state: State<'a>,
    initial_state: State<'a>,
) -> ParseResult<'a, Expr<'a>, EExpr<'a>> {
    let (_, spaces_after_operator, state) =
        space0_e(EExpr::IndentEnd).parse(arena, state, min_indent)?;

    // a `-` is unary if it is preceded by a space and not followed by a space

    let op = loc_op.value;
    let op_start = loc_op.region.start();
    let op_end = loc_op.region.end();
    let new_start = state.pos();
    match op {
        BinOp::Minus if expr_state.end != op_start && op_end == new_start => {
            // negative terms

            let (_, negated_expr, state) = loc_term(options).parse(arena, state, min_indent)?;
            let new_end = state.pos();

            let arg = numeric_negate_expression(
                arena,
                initial_state,
                loc_op,
                negated_expr,
                expr_state.spaces_after,
            );

            let initial_state = state.clone();

            let (spaces, state) =
                match space0_e(EExpr::IndentEnd).parse(arena, state.clone(), min_indent) {
                    Err((_, _)) => (&[] as &[_], state),
                    Ok((_, spaces, state)) => (spaces, state),
                };

            expr_state.arguments.push(arena.alloc(arg));
            expr_state.spaces_after = spaces;
            expr_state.end = new_end;

            parse_expr_end(min_indent, options, expr_state, arena, state, initial_state)
        }
        BinOp::Assignment => {
            let expr_region = expr_state.expr.region;
            let indented_more = min_indent + 1;

            let call = expr_state
                .validate_assignment_or_backpassing(arena, loc_op, EExpr::ElmStyleFunction)
                .map_err(|fail| (MadeProgress, fail))?;

            let (value_def, def_region, state) = {
                match expr_to_pattern_help(arena, &call.value) {
                    Ok(good) => {
                        let (_, mut body, state) = loc_expr().parse(arena, state, indented_more)?;

                        // put the spaces from after the operator in front of the call
                        if !spaces_after_operator.is_empty() {
                            body = arena
                                .alloc(body.value)
                                .with_spaces_before(spaces_after_operator, body.region);
                        }

                        let body_region = Region::span_across(&call.region, &body.region);

                        let alias = ValueDef::Body(
                            arena.alloc(Loc::at(expr_region, good)),
                            arena.alloc(body),
                        );

                        (alias, body_region, state)
                    }
                    Err(_) => {
                        // this `=` likely occurred inline; treat it as an invalid operator
                        let fail = EExpr::BadOperator(arena.alloc("="), loc_op.region.start());

                        return Err((MadeProgress, fail));
                    }
                }
            };

            let mut defs = Defs::default();
            defs.push_value_def(value_def, def_region, &[], &[]);

            parse_defs_expr(options, min_indent, defs, arena, state)
        }
        BinOp::Backpassing => {
            let expr_region = expr_state.expr.region;
            let indented_more = min_indent + 1;

            let call = expr_state
                .validate_assignment_or_backpassing(arena, loc_op, |_, pos| {
                    EExpr::BadOperator("<-", pos)
                })
                .map_err(|fail| (MadeProgress, fail))?;

            let (loc_pattern, loc_body, state) = {
                match expr_to_pattern_help(arena, &call.value) {
                    Ok(good) => {
                        let (_, mut ann_type, state) =
                            loc_expr().parse(arena, state, indented_more)?;

                        // put the spaces from after the operator in front of the call
                        if !spaces_after_operator.is_empty() {
                            ann_type = arena
                                .alloc(ann_type.value)
                                .with_spaces_before(spaces_after_operator, ann_type.region);
                        }

                        (Loc::at(expr_region, good), ann_type, state)
                    }
                    Err(_) => {
                        // this `=` likely occurred inline; treat it as an invalid operator
                        let fail = EExpr::BadOperator("=", loc_op.region.start());

                        return Err((MadeProgress, fail));
                    }
                }
            };

            let parse_cont = space0_before_e(loc_expr(), EExpr::IndentEnd);

            let (_, loc_cont, state) = parse_cont.parse(arena, state, min_indent)?;

            let ret = Expr::Backpassing(
                arena.alloc([loc_pattern]),
                arena.alloc(loc_body),
                arena.alloc(loc_cont),
            );

            Ok((MadeProgress, ret, state))
        }
        BinOp::IsAliasType | BinOp::IsOpaqueType => finish_parsing_alias_or_opaque(
            min_indent,
            options,
            expr_state,
            loc_op,
            arena,
            state,
            spaces_after_operator,
            match op {
                BinOp::IsAliasType => AliasOrOpaque::Alias,
                BinOp::IsOpaqueType => AliasOrOpaque::Opaque,
                _ => unreachable!(),
            },
        ),
        _ => match loc_possibly_negative_or_negated_term(options).parse(
            arena,
            state.clone(),
            min_indent,
        ) {
            Err((MadeProgress, f)) => Err((MadeProgress, f)),
            Ok((_, mut new_expr, state)) => {
                let new_end = state.pos();

                let initial_state = state.clone();

                // put the spaces from after the operator in front of the new_expr
                if !spaces_after_operator.is_empty() {
                    new_expr = arena
                        .alloc(new_expr.value)
                        .with_spaces_before(spaces_after_operator, new_expr.region);
                }

                match space0_e(EExpr::IndentEnd).parse(arena, state.clone(), min_indent) {
                    Err((_, _)) => {
                        let args = std::mem::replace(&mut expr_state.arguments, Vec::new_in(arena));

                        let call = to_call(arena, args, expr_state.expr);

                        expr_state.operators.push((call, loc_op));
                        expr_state.expr = new_expr;
                        expr_state.end = new_end;
                        expr_state.spaces_after = &[];

                        let expr = parse_expr_final(expr_state, arena);
                        Ok((MadeProgress, expr, state))
                    }
                    Ok((_, spaces, state)) => {
                        let args = std::mem::replace(&mut expr_state.arguments, Vec::new_in(arena));

                        let call = to_call(arena, args, expr_state.expr);

                        expr_state.operators.push((call, loc_op));
                        expr_state.expr = new_expr;
                        expr_state.end = new_end;
                        expr_state.spaces_after = spaces;

                        // TODO new start?
                        parse_expr_end(min_indent, options, expr_state, arena, state, initial_state)
                    }
                }
            }
            Err((NoProgress, expr)) => {
                todo!("{:?} {:?}", expr, state)
            }
        },
    }
}

fn parse_expr_end<'a>(
    min_indent: u32,
    options: ExprParseOptions,
    mut expr_state: ExprState<'a>,
    arena: &'a Bump,
    state: State<'a>,
    initial_state: State<'a>,
) -> ParseResult<'a, Expr<'a>, EExpr<'a>> {
    let parser = skip_first!(
        crate::blankspace::check_indent(EExpr::IndentEnd),
        loc_term_or_underscore(options)
    );

    match parser.parse(arena, state.clone(), min_indent) {
        Err((MadeProgress, f)) => Err((MadeProgress, f)),
        Ok((
            _,
            has @ Loc {
                value:
                    Expr::Var {
                        module_name: "",
                        ident: "has",
                    },
                ..
            },
            state,
        )) if matches!(expr_state.expr.value, Expr::Tag(..)) => {
            // This is an ability definition, `Ability arg1 ... has ...`.

            let name = expr_state.expr.map_owned(|e| match e {
                Expr::Tag(name) => name,
                _ => unreachable!(),
            });

            let mut arguments = Vec::with_capacity_in(expr_state.arguments.len(), arena);
            for argument in expr_state.arguments {
                match expr_to_pattern_help(arena, &argument.value) {
                    Ok(good) => {
                        arguments.push(Loc::at(argument.region, good));
                    }
                    Err(_) => {
                        let start = argument.region.start();
                        let err = &*arena.alloc(EPattern::Start(start));
                        return Err((MadeProgress, EExpr::Pattern(err, argument.region.start())));
                    }
                }
            }

            // Attach any spaces to the `has` keyword
            let has = if !expr_state.spaces_after.is_empty() {
                arena
                    .alloc(Has::Has)
                    .with_spaces_before(expr_state.spaces_after, has.region)
            } else {
                Loc::at(has.region, Has::Has)
            };

            let args = arguments.into_bump_slice();
            let (_, (type_def, def_region), state) =
                finish_parsing_ability_def_help(min_indent, name, args, has, arena, state)?;

            let mut defs = Defs::default();

            defs.push_type_def(type_def, def_region, &[], &[]);

            parse_defs_expr(options, min_indent, defs, arena, state)
        }
        Ok((_, mut arg, state)) => {
            let new_end = state.pos();

            // now that we have `function arg1 ... <spaces> argn`, attach the spaces to the `argn`
            if !expr_state.spaces_after.is_empty() {
                arg = arena
                    .alloc(arg.value)
                    .with_spaces_before(expr_state.spaces_after, arg.region);

                expr_state.spaces_after = &[];
            }
            let initial_state = state.clone();

            match space0_e(EExpr::IndentEnd).parse(arena, state.clone(), min_indent) {
                Err((_, _)) => {
                    expr_state.arguments.push(arena.alloc(arg));
                    expr_state.end = new_end;
                    expr_state.spaces_after = &[];

                    let expr = parse_expr_final(expr_state, arena);
                    Ok((MadeProgress, expr, state))
                }
                Ok((_, new_spaces, state)) => {
                    expr_state.arguments.push(arena.alloc(arg));
                    expr_state.end = new_end;
                    expr_state.spaces_after = new_spaces;

                    parse_expr_end(min_indent, options, expr_state, arena, state, initial_state)
                }
            }
        }
        Err((NoProgress, _)) => {
            let before_op = state.clone();
            // try an operator
            match loc!(operator()).parse(arena, state.clone(), min_indent) {
                Err((MadeProgress, f)) => Err((MadeProgress, f)),
                Ok((_, loc_op, state)) => {
                    expr_state.consume_spaces(arena);
                    let initial_state = before_op;
                    parse_expr_operator(
                        min_indent,
                        options,
                        expr_state,
                        loc_op,
                        arena,
                        state,
                        initial_state,
                    )
                }
                Err((NoProgress, _)) => {
                    let mut state = state;
                    // try multi-backpassing
                    if options.accept_multi_backpassing && state.bytes().starts_with(b",") {
                        state = state.advance(1);

                        let (_, mut patterns, state) = specialize_ref(
                            EExpr::Pattern,
                            crate::parser::sep_by0(
                                word1(b',', EPattern::Start),
                                space0_around_ee(
                                    crate::pattern::loc_pattern_help(),
                                    EPattern::Start,
                                    EPattern::IndentEnd,
                                ),
                            ),
                        )
                        .parse(arena, state, min_indent)?;

                        expr_state.consume_spaces(arena);
                        let call = to_call(arena, expr_state.arguments, expr_state.expr);

                        let loc_pattern = Loc::at(
                            call.region,
                            expr_to_pattern_help(arena, &call.value).unwrap(),
                        );

                        patterns.insert(0, loc_pattern);

                        match word2(b'<', b'-', EExpr::BackpassArrow).parse(
                            arena,
                            state.clone(),
                            min_indent,
                        ) {
                            Err((_, fail)) => Err((MadeProgress, fail)),
                            Ok((_, _, state)) => {
                                let parse_body = space0_before_e(
                                    increment_min_indent(loc_expr()),
                                    EExpr::IndentEnd,
                                );

                                let (_, loc_body, state) =
                                    parse_body.parse(arena, state, min_indent)?;

                                let parse_cont = space0_before_e(loc_expr(), EExpr::IndentEnd);

                                let (_, loc_cont, state) =
                                    parse_cont.parse(arena, state, min_indent)?;

                                let ret = Expr::Backpassing(
                                    patterns.into_bump_slice(),
                                    arena.alloc(loc_body),
                                    arena.alloc(loc_cont),
                                );

                                Ok((MadeProgress, ret, state))
                            }
                        }
                    } else if options.check_for_arrow && state.bytes().starts_with(b"->") {
                        Err((MadeProgress, EExpr::BadOperator("->", state.pos())))
                    } else {
                        let expr = parse_expr_final(expr_state, arena);

                        // roll back space parsing
                        Ok((MadeProgress, expr, initial_state))
                    }
                }
            }
        }
    }
}

pub fn loc_expr<'a>() -> impl Parser<'a, Loc<Expr<'a>>, EExpr<'a>> {
    expr_start(ExprParseOptions {
        accept_multi_backpassing: true,
        ..Default::default()
    })
}

pub fn loc_expr_no_multi_backpassing<'a>() -> impl Parser<'a, Loc<Expr<'a>>, EExpr<'a>> {
    expr_start(ExprParseOptions {
        accept_multi_backpassing: false,
        ..Default::default()
    })
}

/// If the given Expr would parse the same way as a valid Pattern, convert it.
/// Example: (foo) could be either an Expr::Var("foo") or Pattern::Identifier("foo")
fn expr_to_pattern_help<'a>(arena: &'a Bump, expr: &Expr<'a>) -> Result<Pattern<'a>, ()> {
    match expr {
        Expr::Var { module_name, ident } => {
            if module_name.is_empty() {
                Ok(Pattern::Identifier(ident))
            } else {
                Ok(Pattern::QualifiedIdentifier { module_name, ident })
            }
        }
        Expr::Underscore(opt_name) => Ok(Pattern::Underscore(opt_name)),
        Expr::Tag(value) => Ok(Pattern::Tag(value)),
        Expr::OpaqueRef(value) => Ok(Pattern::OpaqueRef(value)),
        Expr::Apply(loc_val, loc_args, _) => {
            let region = loc_val.region;
            let value = expr_to_pattern_help(arena, &loc_val.value)?;
            let val_pattern = arena.alloc(Loc { region, value });

            let mut arg_patterns = Vec::with_capacity_in(loc_args.len(), arena);

            for loc_arg in loc_args.iter() {
                let region = loc_arg.region;
                let value = expr_to_pattern_help(arena, &loc_arg.value)?;

                arg_patterns.push(Loc { region, value });
            }

            let pattern = Pattern::Apply(val_pattern, arg_patterns.into_bump_slice());

            Ok(pattern)
        }

        Expr::SpaceBefore(sub_expr, spaces) => Ok(Pattern::SpaceBefore(
            arena.alloc(expr_to_pattern_help(arena, sub_expr)?),
            spaces,
        )),
        Expr::SpaceAfter(sub_expr, spaces) => Ok(Pattern::SpaceAfter(
            arena.alloc(expr_to_pattern_help(arena, sub_expr)?),
            spaces,
        )),

        Expr::ParensAround(sub_expr) => expr_to_pattern_help(arena, sub_expr),

        Expr::Record(fields) => {
            let patterns = fields.map_items_result(arena, |loc_assigned_field| {
                let region = loc_assigned_field.region;
                let value = assigned_expr_field_to_pattern_help(arena, &loc_assigned_field.value)?;
                Ok(Loc { region, value })
            })?;

            Ok(Pattern::RecordDestructure(patterns))
        }

        Expr::Tuple(fields) => Ok(Pattern::Tuple(fields.map_items_result(
            arena,
            |loc_expr| {
                Ok(Loc {
                    region: loc_expr.region,
                    value: expr_to_pattern_help(arena, &loc_expr.value)?,
                })
            },
        )?)),

        &Expr::Float(string) => Ok(Pattern::FloatLiteral(string)),
        &Expr::Num(string) => Ok(Pattern::NumLiteral(string)),
        Expr::NonBase10Int {
            string,
            base,
            is_negative,
        } => Ok(Pattern::NonBase10Literal {
            string,
            base: *base,
            is_negative: *is_negative,
        }),
        // These would not have parsed as patterns
        Expr::RecordAccessorFunction(_)
        | Expr::RecordAccess(_, _)
        | Expr::TupleAccessorFunction(_)
        | Expr::TupleAccess(_, _)
        | Expr::List { .. }
        | Expr::Closure(_, _)
        | Expr::Backpassing(_, _, _)
        | Expr::BinOps { .. }
        | Expr::Defs(_, _)
        | Expr::If(_, _)
        | Expr::When(_, _)
        | Expr::Expect(_, _)
        | Expr::Dbg(_, _)
        | Expr::MalformedClosure
        | Expr::PrecedenceConflict { .. }
        | Expr::RecordUpdate { .. }
        | Expr::UnaryOp(_, _)
        | Expr::Crash => Err(()),

        Expr::Str(string) => Ok(Pattern::StrLiteral(*string)),
        Expr::SingleQuote(string) => Ok(Pattern::SingleQuote(string)),
        Expr::MalformedIdent(string, _problem) => Ok(Pattern::Malformed(string)),
    }
}

fn assigned_expr_field_to_pattern_help<'a>(
    arena: &'a Bump,
    assigned_field: &AssignedField<'a, Expr<'a>>,
) -> Result<Pattern<'a>, ()> {
    // the assigned fields always store spaces, but this slice is often empty
    Ok(match assigned_field {
        AssignedField::RequiredValue(name, spaces, value) => {
            let pattern = expr_to_pattern_help(arena, &value.value)?;
            let result = arena.alloc(Loc {
                region: value.region,
                value: pattern,
            });
            if spaces.is_empty() {
                Pattern::RequiredField(name.value, result)
            } else {
                Pattern::SpaceAfter(
                    arena.alloc(Pattern::RequiredField(name.value, result)),
                    spaces,
                )
            }
        }
        AssignedField::OptionalValue(name, spaces, value) => {
            let result = arena.alloc(Loc {
                region: value.region,
                value: value.value,
            });
            if spaces.is_empty() {
                Pattern::OptionalField(name.value, result)
            } else {
                Pattern::SpaceAfter(
                    arena.alloc(Pattern::OptionalField(name.value, result)),
                    spaces,
                )
            }
        }
        AssignedField::LabelOnly(name) => Pattern::Identifier(name.value),
        AssignedField::SpaceBefore(nested, spaces) => Pattern::SpaceBefore(
            arena.alloc(assigned_expr_field_to_pattern_help(arena, nested)?),
            spaces,
        ),
        AssignedField::SpaceAfter(nested, spaces) => Pattern::SpaceAfter(
            arena.alloc(assigned_expr_field_to_pattern_help(arena, nested)?),
            spaces,
        ),
        AssignedField::Malformed(string) => Pattern::Malformed(string),
    })
}

pub fn toplevel_defs<'a>() -> impl Parser<'a, Defs<'a>, EExpr<'a>> {
    move |arena, state: State<'a>, min_indent: u32| {
        let (_, initial_space, state) =
            space0_e(EExpr::IndentEnd).parse(arena, state, min_indent)?;

        let start_column = state.column();

        let options = ExprParseOptions {
            accept_multi_backpassing: false,
            check_for_arrow: true,
        };

        let mut output = Defs::default();
        let before = Slice::extend_new(&mut output.spaces, initial_space.iter().copied());

        let (_, mut output, state) = parse_defs_end(options, start_column, output, arena, state)?;

        let (_, final_space, state) =
            space0_e(EExpr::IndentEnd).parse(arena, state, start_column)?;

        if !output.tags.is_empty() {
            // add surrounding whitespace
            let after = Slice::extend_new(&mut output.spaces, final_space.iter().copied());

            debug_assert!(output.space_before[0].is_empty());
            output.space_before[0] = before;

            let last = output.tags.len() - 1;
            debug_assert!(output.space_after[last].is_empty() || after.is_empty());
            output.space_after[last] = after;
        }

        Ok((MadeProgress, output, state))
    }
}

// PARSER HELPERS

fn closure_help<'a>(options: ExprParseOptions) -> impl Parser<'a, Expr<'a>, EClosure<'a>> {
    // closure_help_help(options)
    map_with_arena!(
        // After the first token, all other tokens must be indented past the start of the line
        indented_seq!(
            // All closures start with a '\' - e.g. (\x -> x + 1)
            word1_indent(b'\\', EClosure::Start),
            // Once we see the '\', we're committed to parsing this as a closure.
            // It may turn out to be malformed, but it is definitely a closure.
            and!(
                // Parse the params
                // Params are comma-separated
                sep_by1_e(
                    word1(b',', EClosure::Comma),
                    space0_around_ee(
                        specialize(EClosure::Pattern, closure_param()),
                        EClosure::IndentArg,
                        EClosure::IndentArrow,
                    ),
                    EClosure::Arg,
                ),
                skip_first!(
                    // Parse the -> which separates params from body
                    word2(b'-', b'>', EClosure::Arrow),
                    // Parse the body
                    space0_before_e(
                        specialize_ref(EClosure::Body, expr_start(options)),
                        EClosure::IndentBody
                    )
                )
            )
        ),
        |arena: &'a Bump, (params, body)| {
            let params: Vec<'a, Loc<Pattern<'a>>> = params;
            let params: &'a [Loc<Pattern<'a>>] = params.into_bump_slice();
            Expr::Closure(params, arena.alloc(body))
        }
    )
}

mod when {
    use super::*;
    use crate::ast::WhenBranch;

    /// Parser for when expressions.
    pub fn expr_help<'a>(options: ExprParseOptions) -> impl Parser<'a, Expr<'a>, EWhen<'a>> {
        map_with_arena!(
            and!(
                indented_seq!(
                    parser::keyword_e(keyword::WHEN, EWhen::When),
                    space0_around_e_no_after_indent_check(
                        specialize_ref(EWhen::Condition, expr_start(options)),
                        EWhen::IndentCondition,
                    )
                ),
                // Note that we allow the `is` to be at any indent level, since this doesn't introduce any
                // ambiguity. The formatter will fix it up.
                //
                // We require that branches are indented relative to the line containing the `is`.
                indented_seq!(
                    parser::keyword_e(keyword::IS, EWhen::Is),
                    branches(options)
                )
            ),
            move |arena: &'a Bump, (loc_condition, branches): (Loc<Expr<'a>>, Vec<'a, &'a WhenBranch<'a>>)| {
                Expr::When(arena.alloc(loc_condition), branches.into_bump_slice())
            }
        )
    }

    fn branches<'a>(
        options: ExprParseOptions,
    ) -> impl Parser<'a, Vec<'a, &'a WhenBranch<'a>>, EWhen<'a>> {
        move |arena, state: State<'a>, min_indent: u32| {
            let mut branches: Vec<'a, &'a WhenBranch<'a>> = Vec::with_capacity_in(2, arena);

            // 1. Parse the first branch and get its indentation level. (It must be >= min_indent.)
            // 2. Parse the other branches. Their indentation levels must be == the first branch's.

            let (_, ((pattern_indent_level, loc_first_patterns), loc_first_guard), state): (
                _,
                ((_, _), _),
                State<'a>,
            ) = branch_alternatives(options, None).parse(arena, state, min_indent)?;

            let original_indent = pattern_indent_level;

            // Parse the first "->" and the expression after it.
            let (_, loc_first_expr, mut state) =
                branch_result(original_indent + 1).parse(arena, state, original_indent + 1)?;

            // Record this as the first branch, then optionally parse additional branches.
            branches.push(arena.alloc(WhenBranch {
                patterns: loc_first_patterns.into_bump_slice(),
                value: loc_first_expr,
                guard: loc_first_guard,
            }));

            let branch_parser = map!(
                and!(
                    then(
                        branch_alternatives(options, Some(pattern_indent_level)),
                        move |_arena, state, _, ((indent_column, loc_patterns), loc_guard)| {
                            if pattern_indent_level == indent_column {
                                Ok((MadeProgress, (loc_patterns, loc_guard), state))
                            } else {
                                let indent = pattern_indent_level - indent_column;
                                Err((MadeProgress, EWhen::PatternAlignment(indent, state.pos())))
                            }
                        },
                    ),
                    branch_result(original_indent + 1)
                ),
                |((patterns, guard), expr)| {
                    let patterns: Vec<'a, _> = patterns;
                    WhenBranch {
                        patterns: patterns.into_bump_slice(),
                        value: expr,
                        guard,
                    }
                }
            );

            while !state.bytes().is_empty() {
                match branch_parser.parse(arena, state.clone(), min_indent) {
                    Ok((_, next_output, next_state)) => {
                        state = next_state;

                        branches.push(arena.alloc(next_output));
                    }
                    Err((MadeProgress, problem)) => {
                        return Err((MadeProgress, problem));
                    }
                    Err((NoProgress, _)) => {
                        break;
                    }
                }
            }

            Ok((MadeProgress, branches, state))
        }
    }

    /// Parsing alternative patterns in `when` branches.
    fn branch_alternatives<'a>(
        options: ExprParseOptions,
        pattern_indent_level: Option<u32>,
    ) -> impl Parser<'a, ((u32, Vec<'a, Loc<Pattern<'a>>>), Option<Loc<Expr<'a>>>), EWhen<'a>> {
        let options = ExprParseOptions {
            check_for_arrow: false,
            ..options
        };
        and!(
            branch_alternatives_help(pattern_indent_level),
            one_of![
                map!(
                    skip_first!(
                        parser::keyword_e(keyword::IF, EWhen::IfToken),
                        // TODO we should require space before the expression but not after
                        space0_around_ee(
                            specialize_ref(
                                EWhen::IfGuard,
                                increment_min_indent(expr_start(options))
                            ),
                            EWhen::IndentIfGuard,
                            EWhen::IndentArrow,
                        )
                    ),
                    Some
                ),
                |_, s, _| Ok((NoProgress, None, s))
            ]
        )
    }

    fn branch_single_alternative<'a>() -> impl Parser<'a, Loc<Pattern<'a>>, EWhen<'a>> {
        move |arena, state, min_indent| {
            let (_, spaces, state) =
                backtrackable(space0_e(EWhen::IndentPattern)).parse(arena, state, min_indent)?;

            let (_, loc_pattern, state) = space0_after_e(
                specialize(EWhen::Pattern, crate::pattern::loc_pattern_help()),
                EWhen::IndentPattern,
            )
            .parse(arena, state, min_indent)?;

            Ok((
                MadeProgress,
                if spaces.is_empty() {
                    loc_pattern
                } else {
                    arena
                        .alloc(loc_pattern.value)
                        .with_spaces_before(spaces, loc_pattern.region)
                },
                state,
            ))
        }
    }

    fn branch_alternatives_help<'a>(
        pattern_indent_level: Option<u32>,
    ) -> impl Parser<'a, (u32, Vec<'a, Loc<Pattern<'a>>>), EWhen<'a>> {
        move |arena, state: State<'a>, min_indent: u32| {
            // put no restrictions on the indent after the spaces; we'll check it manually
            match space0_e(EWhen::IndentPattern).parse(arena, state, 0) {
                Err((MadeProgress, fail)) => Err((NoProgress, fail)),
                Err((NoProgress, fail)) => Err((NoProgress, fail)),
                Ok((_progress, spaces, state)) => {
                    match pattern_indent_level {
                        Some(wanted) if state.column() > wanted => {
                            // this branch is indented too much
                            Err((NoProgress, EWhen::IndentPattern(state.pos())))
                        }
                        Some(wanted) if state.column() < wanted => {
                            let indent = wanted - state.column();
                            Err((NoProgress, EWhen::PatternAlignment(indent, state.pos())))
                        }
                        _ => {
                            let pattern_indent =
                                min_indent.max(pattern_indent_level.unwrap_or(min_indent));
                            // the region is not reliable for the indent column in the case of
                            // parentheses around patterns
                            let pattern_indent_column = state.column();

                            let parser =
                                sep_by1(word1(b'|', EWhen::Bar), branch_single_alternative());

                            match parser.parse(arena, state.clone(), pattern_indent) {
                                Err((MadeProgress, fail)) => Err((MadeProgress, fail)),
                                Err((NoProgress, fail)) => {
                                    // roll back space parsing if the pattern made no progress
                                    Err((NoProgress, fail))
                                }

                                Ok((_, mut loc_patterns, state)) => {
                                    // tag spaces onto the first parsed pattern
                                    if !spaces.is_empty() {
                                        if let Some(first) = loc_patterns.get_mut(0) {
                                            *first = arena
                                                .alloc(first.value)
                                                .with_spaces_before(spaces, first.region);
                                        }
                                    }

                                    Ok((MadeProgress, (pattern_indent_column, loc_patterns), state))
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    /// Parsing the righthandside of a branch in a when conditional.
    fn branch_result<'a>(indent: u32) -> impl Parser<'a, Loc<Expr<'a>>, EWhen<'a>> {
        move |arena, state, _min_indent| {
            skip_first!(
                word2(b'-', b'>', EWhen::Arrow),
                space0_before_e(
                    specialize_ref(EWhen::Branch, loc_expr()),
                    EWhen::IndentBranch,
                )
            )
            .parse(arena, state, indent)
        }
    }
}

fn if_branch<'a>() -> impl Parser<'a, (Loc<Expr<'a>>, Loc<Expr<'a>>), EIf<'a>> {
    skip_second!(
        and!(
            skip_second!(
                space0_around_ee(
                    specialize_ref(EIf::Condition, loc_expr()),
                    EIf::IndentCondition,
                    EIf::IndentThenToken,
                ),
                parser::keyword_e(keyword::THEN, EIf::Then)
            ),
            space0_around_ee(
                specialize_ref(EIf::ThenBranch, loc_expr()),
                EIf::IndentThenBranch,
                EIf::IndentElseToken,
            )
        ),
        parser::keyword_e(keyword::ELSE, EIf::Else)
    )
}

fn expect_help<'a>(options: ExprParseOptions) -> impl Parser<'a, Expr<'a>, EExpect<'a>> {
    move |arena: &'a Bump, state: State<'a>, min_indent| {
        let start_column = state.column();

        let (_, _, state) =
            parser::keyword_e(keyword::EXPECT, EExpect::Expect).parse(arena, state, min_indent)?;

        let (_, condition, state) = space0_before_e(
            specialize_ref(
                EExpect::Condition,
                set_min_indent(start_column + 1, expr_start(options)),
            ),
            EExpect::IndentCondition,
        )
        .parse(arena, state, start_column + 1)
        .map_err(|(_, f)| (MadeProgress, f))?;

        let parse_cont = specialize_ref(
            EExpect::Continuation,
            space0_before_e(loc_expr(), EExpr::IndentEnd),
        );

        let (_, loc_cont, state) = parse_cont.parse(arena, state, min_indent)?;

        let expr = Expr::Expect(arena.alloc(condition), arena.alloc(loc_cont));

        Ok((MadeProgress, expr, state))
    }
}

fn dbg_help<'a>(options: ExprParseOptions) -> impl Parser<'a, Expr<'a>, EExpect<'a>> {
    move |arena: &'a Bump, state: State<'a>, min_indent| {
        let start_column = state.column();

        let (_, _, state) =
            parser::keyword_e(keyword::DBG, EExpect::Dbg).parse(arena, state, min_indent)?;

        let (_, condition, state) = space0_before_e(
            specialize_ref(
                EExpect::Condition,
                set_min_indent(start_column + 1, expr_start(options)),
            ),
            EExpect::IndentCondition,
        )
        .parse(arena, state, start_column + 1)
        .map_err(|(_, f)| (MadeProgress, f))?;

        let parse_cont = specialize_ref(
            EExpect::Continuation,
            space0_before_e(loc_expr(), EExpr::IndentEnd),
        );

        let (_, loc_cont, state) = parse_cont.parse(arena, state, min_indent)?;

        let expr = Expr::Dbg(arena.alloc(condition), arena.alloc(loc_cont));

        Ok((MadeProgress, expr, state))
    }
}

fn if_expr_help<'a>(options: ExprParseOptions) -> impl Parser<'a, Expr<'a>, EIf<'a>> {
    move |arena: &'a Bump, state, min_indent| {
        let (_, _, state) =
            parser::keyword_e(keyword::IF, EIf::If).parse(arena, state, min_indent)?;

        let mut branches = Vec::with_capacity_in(1, arena);

        let mut loop_state = state;

        let state_final_else = loop {
            let (_, (cond, then_branch), state) =
                if_branch().parse(arena, loop_state, min_indent)?;

            branches.push((cond, then_branch));

            // try to parse another `if`
            // NOTE this drops spaces between the `else` and the `if`
            let optional_if = and!(
                backtrackable(space0_e(EIf::IndentIf)),
                parser::keyword_e(keyword::IF, EIf::If)
            );

            match optional_if.parse(arena, state.clone(), min_indent) {
                Err((_, _)) => break state,
                Ok((_, _, state)) => {
                    loop_state = state;
                    continue;
                }
            }
        };

        let (_, else_branch, state) = space0_before_e(
            specialize_ref(EIf::ElseBranch, expr_start(options)),
            EIf::IndentElseBranch,
        )
        .parse(arena, state_final_else, min_indent)
        .map_err(|(_, f)| (MadeProgress, f))?;

        let expr = Expr::If(branches.into_bump_slice(), arena.alloc(else_branch));

        Ok((MadeProgress, expr, state))
    }
}

/// This is a helper function for parsing function args.
/// The rules for (-) are special-cased, and they come up in function args.
///
/// They work like this:
///
/// x - y  # "x minus y"
/// x-y    # "x minus y"
/// x- y   # "x minus y" (probably written in a rush)
/// x -y   # "call x, passing (-y)"
///
/// Since operators have higher precedence than function application,
/// any time we encounter a '-' it is unary iff it is both preceded by spaces
/// and is *not* followed by a whitespace character.

/// When we parse an ident like `foo ` it could be any of these:
///
/// 1. A standalone variable with trailing whitespace (e.g. because an operator is next)
/// 2. The beginning of a function call (e.g. `foo bar baz`)
/// 3. The beginning of a definition (e.g. `foo =`)
/// 4. The beginning of a type annotation (e.g. `foo :`)
/// 5. A reserved keyword (e.g. `if ` or `case `), meaning we should do something else.

fn assign_or_destructure_identifier<'a>() -> impl Parser<'a, Ident<'a>, EExpr<'a>> {
    crate::ident::parse_ident
}

#[allow(dead_code)]
fn with_indent<'a, E, T, P>(parser: P) -> impl Parser<'a, u32, E>
where
    P: Parser<'a, T, E>,
    E: 'a,
{
    move |arena, state: State<'a>, min_indent: u32| {
        let indent_column = state.column();

        let (progress, _, state) = parser.parse(arena, state, min_indent)?;

        Ok((progress, indent_column, state))
    }
}

fn ident_to_expr<'a>(arena: &'a Bump, src: Ident<'a>) -> Expr<'a> {
    match src {
        Ident::Tag(string) => Expr::Tag(string),
        Ident::OpaqueRef(string) => Expr::OpaqueRef(string),
        Ident::Access { module_name, parts } => {
            let mut iter = parts.iter();

            // The first value in the iterator is the variable name,
            // e.g. `foo` in `foo.bar.baz`
            let mut answer = match iter.next() {
                Some(ident) => Expr::Var { module_name, ident },
                None => {
                    panic!("Parsed an Ident::Access with no parts");
                }
            };

            // The remaining items in the iterator are record field accesses,
            // e.g. `bar` in `foo.bar.baz`, followed by `baz`
            for field in iter {
                // Wrap the previous answer in the new one, so we end up
                // with a nested Expr. That way, `foo.bar.baz` gets represented
                // in the AST as if it had been written (foo.bar).baz all along.
                answer = Expr::RecordAccess(arena.alloc(answer), field);
            }

            answer
        }
        Ident::RecordAccessorFunction(string) => Expr::RecordAccessorFunction(string),
        Ident::TupleAccessorFunction(string) => Expr::TupleAccessorFunction(string),
        Ident::Malformed(string, problem) => Expr::MalformedIdent(string, problem),
    }
}

fn list_literal_help<'a>() -> impl Parser<'a, Expr<'a>, EList<'a>> {
    map_with_arena!(
        collection_trailing_sep_e!(
            word1(b'[', EList::Open),
            specialize_ref(EList::Expr, loc_expr_no_multi_backpassing()),
            word1(b',', EList::End),
            word1(b']', EList::End),
            EList::IndentEnd,
            Expr::SpaceBefore
        ),
        |arena, elements: Collection<'a, _>| {
            let elements = elements.ptrify_items(arena);
            Expr::List(elements)
        }
    )
    .trace("list_literal")
}

pub fn tuple_value_field<'a>() -> impl Parser<'a, Loc<Expr<'a>>, ETuple<'a>> {
    space0_before_e(
        specialize_ref(ETuple::Expr, loc_expr_no_multi_backpassing()),
        ETuple::IndentEnd,
    )
}

pub fn record_value_field<'a>() -> impl Parser<'a, AssignedField<'a, Expr<'a>>, ERecord<'a>> {
    use AssignedField::*;

    move |arena, state: State<'a>, min_indent| {
        // You must have a field name, e.g. "email"
        let (progress, loc_label, state) =
            specialize(|_, pos| ERecord::Field(pos), loc!(lowercase_ident()))
                .parse(arena, state, min_indent)?;
        debug_assert_eq!(progress, MadeProgress);

        let (_, spaces, state) = space0_e(ERecord::IndentColon).parse(arena, state, min_indent)?;

        // Having a value is optional; both `{ email }` and `{ email: blah }` work.
        // (This is true in both literals and types.)
        let (_, opt_loc_val, state) = optional(and!(
            either!(
                word1(b':', ERecord::Colon),
                word1(b'?', ERecord::QuestionMark)
            ),
            space0_before_e(
                specialize_ref(ERecord::Expr, loc_expr_no_multi_backpassing()),
                ERecord::IndentEnd,
            )
        ))
        .parse(arena, state, min_indent)?;

        let answer = match opt_loc_val {
            Some((Either::First(_), loc_val)) => {
                RequiredValue(loc_label, spaces, arena.alloc(loc_val))
            }

            Some((Either::Second(_), loc_val)) => {
                OptionalValue(loc_label, spaces, arena.alloc(loc_val))
            }

            // If no value was provided, record it as a Var.
            // Canonicalize will know what to do with a Var later.
            None => {
                if !spaces.is_empty() {
                    SpaceAfter(arena.alloc(LabelOnly(loc_label)), spaces)
                } else {
                    LabelOnly(loc_label)
                }
            }
        };

        Ok((MadeProgress, answer, state))
    }
}

fn record_updateable_identifier<'a>() -> impl Parser<'a, Expr<'a>, ERecord<'a>> {
    specialize(
        |_, pos| ERecord::Updateable(pos),
        map_with_arena!(parse_ident, ident_to_expr),
    )
}

fn record_help<'a>() -> impl Parser<
    'a,
    (
        Option<Loc<Expr<'a>>>,
        Loc<(
            Vec<'a, Loc<AssignedField<'a, Expr<'a>>>>,
            &'a [CommentOrNewline<'a>],
        )>,
    ),
    ERecord<'a>,
> {
    skip_first!(
        word1(b'{', ERecord::Open),
        and!(
            // You can optionally have an identifier followed by an '&' to
            // make this a record update, e.g. { Foo.user & username: "blah" }.
            optional(skip_second!(
                space0_around_ee(
                    // We wrap the ident in an Expr here,
                    // so that we have a Spaceable value to work with,
                    // and then in canonicalization verify that it's an Expr::Var
                    // (and not e.g. an `Expr::Access`) and extract its string.
                    loc!(record_updateable_identifier()),
                    ERecord::IndentEnd,
                    ERecord::IndentAmpersand,
                ),
                word1(b'&', ERecord::Ampersand)
            )),
            loc!(skip_first!(
                // We specifically allow space characters inside here, so that
                // `{  }` can be successfully parsed as an empty record, and then
                // changed by the formatter back into `{}`.
                zero_or_more!(word1(b' ', ERecord::End)),
                skip_second!(
                    and!(
                        trailing_sep_by0(
                            word1(b',', ERecord::End),
                            space0_before_optional_after(
                                loc!(record_value_field()),
                                ERecord::IndentEnd,
                                ERecord::IndentEnd
                            ),
                        ),
                        // Allow outdented closing braces
                        reset_min_indent(space0_e(ERecord::IndentEnd))
                    ),
                    word1(b'}', ERecord::End)
                )
            ))
        )
    )
}

fn record_literal_help<'a>() -> impl Parser<'a, Expr<'a>, EExpr<'a>> {
    then(
        and!(
            loc!(specialize(EExpr::Record, record_help())),
            // there can be field access, e.g. `{ x : 4 }.x`
            record_field_access_chain()
        ),
        move |arena, state, _, (loc_record, accessors)| {
            let (opt_update, loc_assigned_fields_with_comments) = loc_record.value;

            // This is a record literal, not a destructure.
            let value = match opt_update {
                Some(update) => Expr::RecordUpdate {
                    update: &*arena.alloc(update),
                    fields: Collection::with_items_and_comments(
                        arena,
                        loc_assigned_fields_with_comments.value.0.into_bump_slice(),
                        arena.alloc(loc_assigned_fields_with_comments.value.1),
                    ),
                },
                None => Expr::Record(Collection::with_items_and_comments(
                    arena,
                    loc_assigned_fields_with_comments.value.0.into_bump_slice(),
                    loc_assigned_fields_with_comments.value.1,
                )),
            };

            let value = apply_expr_access_chain(arena, value, accessors);

            Ok((MadeProgress, value, state))
        },
    )
}

fn apply_expr_access_chain<'a>(
    arena: &'a Bump,
    value: Expr<'a>,
    accessors: Vec<'a, Accessor<'a>>,
) -> Expr<'a> {
    accessors
        .into_iter()
        .fold(value, |value, accessor| match accessor {
            Accessor::RecordField(field) => Expr::RecordAccess(arena.alloc(value), field),
            Accessor::TupleIndex(field) => Expr::TupleAccess(arena.alloc(value), field),
        })
}

fn string_literal_help<'a>() -> impl Parser<'a, Expr<'a>, EString<'a>> {
    map!(crate::string_literal::parse(), Expr::Str)
}

fn single_quote_literal_help<'a>() -> impl Parser<'a, Expr<'a>, EString<'a>> {
    map!(
        crate::string_literal::parse_single_quote(),
        Expr::SingleQuote
    )
}

fn positive_number_literal_help<'a>() -> impl Parser<'a, Expr<'a>, ENumber> {
    map!(
        crate::number_literal::positive_number_literal(),
        |literal| {
            use crate::number_literal::NumLiteral::*;

            match literal {
                Num(s) => Expr::Num(s),
                Float(s) => Expr::Float(s),
                NonBase10Int {
                    string,
                    base,
                    is_negative,
                } => Expr::NonBase10Int {
                    string,
                    base,
                    is_negative,
                },
            }
        }
    )
}

fn number_literal_help<'a>() -> impl Parser<'a, Expr<'a>, ENumber> {
    map!(crate::number_literal::number_literal(), |literal| {
        use crate::number_literal::NumLiteral::*;

        match literal {
            Num(s) => Expr::Num(s),
            Float(s) => Expr::Float(s),
            NonBase10Int {
                string,
                base,
                is_negative,
            } => Expr::NonBase10Int {
                string,
                base,
                is_negative,
            },
        }
    })
}

const BINOP_CHAR_SET: &[u8] = b"+-/*=.<>:&|^?%!";

const BINOP_CHAR_MASK: [bool; 125] = {
    let mut result = [false; 125];

    let mut i = 0;
    while i < BINOP_CHAR_SET.len() {
        let index = BINOP_CHAR_SET[i] as usize;

        result[index] = true;

        i += 1;
    }

    result
};

fn operator<'a>() -> impl Parser<'a, BinOp, EExpr<'a>> {
    |_, state, _m| operator_help(EExpr::Start, EExpr::BadOperator, state)
}

#[inline(always)]
fn operator_help<'a, F, G, E>(
    to_expectation: F,
    to_error: G,
    mut state: State<'a>,
) -> ParseResult<'a, BinOp, E>
where
    F: Fn(Position) -> E,
    G: Fn(&'a str, Position) -> E,
    E: 'a,
{
    let chomped = chomp_ops(state.bytes());

    macro_rules! good {
        ($op:expr, $width:expr) => {{
            state = state.advance($width);

            Ok((MadeProgress, $op, state))
        }};
    }

    macro_rules! bad_made_progress {
        ($op:expr) => {{
            Err((MadeProgress, to_error($op, state.pos())))
        }};
    }

    match chomped {
        "" => Err((NoProgress, to_expectation(state.pos()))),
        "+" => good!(BinOp::Plus, 1),
        "-" => good!(BinOp::Minus, 1),
        "*" => good!(BinOp::Star, 1),
        "/" => good!(BinOp::Slash, 1),
        "%" => good!(BinOp::Percent, 1),
        "^" => good!(BinOp::Caret, 1),
        ">" => good!(BinOp::GreaterThan, 1),
        "<" => good!(BinOp::LessThan, 1),
        "." => {
            // a `.` makes no progress, so it does not interfere with `.foo` access(or)
            Err((NoProgress, to_error(".", state.pos())))
        }
        "=" => good!(BinOp::Assignment, 1),
        ":=" => good!(BinOp::IsOpaqueType, 2),
        ":" => good!(BinOp::IsAliasType, 1),
        "|>" => good!(BinOp::Pizza, 2),
        "==" => good!(BinOp::Equals, 2),
        "!=" => good!(BinOp::NotEquals, 2),
        ">=" => good!(BinOp::GreaterThanOrEq, 2),
        "<=" => good!(BinOp::LessThanOrEq, 2),
        "&&" => good!(BinOp::And, 2),
        "||" => good!(BinOp::Or, 2),
        "//" => good!(BinOp::DoubleSlash, 2),
        "->" => {
            // makes no progress, so it does not interfere with `_ if isGood -> ...`
            Err((NoProgress, to_error("->", state.pos())))
        }
        "<-" => good!(BinOp::Backpassing, 2),
        _ => bad_made_progress!(chomped),
    }
}

fn chomp_ops(bytes: &[u8]) -> &str {
    let mut chomped = 0;

    for c in bytes.iter() {
        if let Some(true) = BINOP_CHAR_MASK.get(*c as usize) {
            chomped += 1;
        } else {
            break;
        }
    }

    unsafe {
        // Safe because BINOP_CHAR_SET only contains ascii chars
        std::str::from_utf8_unchecked(&bytes[..chomped])
    }
}
