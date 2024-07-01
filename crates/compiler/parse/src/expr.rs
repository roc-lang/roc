use crate::ast::{
    is_expr_suffixed, is_top_level_suffixed, AssignedField, Collection, CommentOrNewline, Defs,
    Expr, ExtractSpaces, Implements, ImplementsAbilities, ImportAlias, ImportAsKeyword,
    ImportExposingKeyword, ImportedModuleName, IngestedFileAnnotation, IngestedFileImport,
    ModuleImport, ModuleImportParams, Pattern, RecordBuilderField, Spaceable, Spaced, Spaces,
    TypeAnnotation, TypeDef, TypeHeader, ValueDef,
};
use crate::blankspace::{
    space0_after_e, space0_around_e_no_after_indent_check, space0_around_ee, space0_before_e,
    space0_before_optional_after, space0_e, spaces, spaces_around, spaces_before,
};
use crate::ident::{
    integer_ident, lowercase_ident, parse_ident, unqualified_ident, Accessor, Ident, Suffix,
};
use crate::module::module_name_help;
use crate::parser::{
    self, and, backtrackable, between, byte, byte_indent, collection_inner,
    collection_trailing_sep_e, either, increment_min_indent, indented_seq_skip_first,
    line_min_indent, loc, map, map_with_arena, optional, reset_min_indent, sep_by1, sep_by1_e,
    set_min_indent, skip_first, skip_second, specialize_err, specialize_err_ref, then, two_bytes,
    zero_or_more, EClosure, EExpect, EExpr, EIf, EImport, EImportParams, EInParens, EList, ENumber,
    EPattern, ERecord, EString, EType, EWhen, Either, ParseResult, Parser,
};
use crate::pattern::{closure_param, loc_implements_parser};
use crate::state::State;
use crate::string_literal::{self, StrLikeLiteral};
use crate::{header, keyword};
use crate::{module, type_annotation};
use bumpalo::collections::Vec;
use bumpalo::Bump;
use roc_collections::soa::Slice;
use roc_error_macros::internal_error;
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
    let parser = skip_second(
        space0_before_optional_after(loc_expr(true), EExpr::IndentStart, EExpr::IndentEnd),
        expr_end(),
    );

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

pub fn expr_help<'a>() -> impl Parser<'a, Expr<'a>, EExpr<'a>> {
    move |arena, state: State<'a>, min_indent: u32| {
        loc_expr(true)
            .parse(arena, state, min_indent)
            .map(|(a, b, c)| (a, b.value, c))
    }
}

fn loc_expr_in_parens_help<'a>() -> impl Parser<'a, Loc<Expr<'a>>, EInParens<'a>> {
    then(
        loc(collection_trailing_sep_e(
            byte(b'(', EInParens::Open),
            specialize_err_ref(EInParens::Expr, loc_expr(false)),
            byte(b',', EInParens::End),
            byte(b')', EInParens::End),
            Expr::SpaceBefore,
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
    map_with_arena(
        loc(and(
            specialize_err(EExpr::InParens, loc_expr_in_parens_help()),
            record_field_access_chain(),
        )),
        move |arena: &'a Bump, value: Loc<(Loc<Expr<'a>>, Vec<'a, Suffix<'a>>)>| {
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
        },
    )
}

fn record_field_access_chain<'a>() -> impl Parser<'a, Vec<'a, Suffix<'a>>, EExpr<'a>> {
    zero_or_more(one_of!(
        skip_first(
            byte(b'.', EExpr::Access),
            specialize_err(
                |_, pos| EExpr::Access(pos),
                one_of!(
                    map(lowercase_ident(), |x| Suffix::Accessor(
                        Accessor::RecordField(x)
                    )),
                    map(integer_ident(), |x| Suffix::Accessor(Accessor::TupleIndex(
                        x
                    ))),
                )
            )
        ),
        map(byte(b'!', EExpr::Access), |_| Suffix::TaskAwaitBang),
    ))
}

/// In some contexts we want to parse the `_` as an expression, so it can then be turned into a
/// pattern later
fn loc_term_or_underscore_or_conditional<'a>(
    options: ExprParseOptions,
) -> impl Parser<'a, Loc<Expr<'a>>, EExpr<'a>> {
    one_of!(
        loc_expr_in_parens_etc_help(),
        loc(specialize_err(EExpr::If, if_expr_help(options))),
        loc(specialize_err(EExpr::When, when::expr_help(options))),
        loc(specialize_err(EExpr::Str, string_like_literal_help())),
        loc(specialize_err(
            EExpr::Number,
            positive_number_literal_help()
        )),
        loc(specialize_err(EExpr::Closure, closure_help(options))),
        loc(crash_kw()),
        loc(underscore_expression()),
        loc(record_literal_help()),
        loc(specialize_err(EExpr::List, list_literal_help())),
        ident_seq(),
    )
}

/// In some contexts we want to parse the `_` as an expression, so it can then be turned into a
/// pattern later
fn loc_term_or_underscore<'a>(
    options: ExprParseOptions,
) -> impl Parser<'a, Loc<Expr<'a>>, EExpr<'a>> {
    one_of!(
        loc_expr_in_parens_etc_help(),
        loc(specialize_err(EExpr::Str, string_like_literal_help())),
        loc(specialize_err(
            EExpr::Number,
            positive_number_literal_help()
        )),
        loc(specialize_err(EExpr::Closure, closure_help(options))),
        loc(underscore_expression()),
        loc(record_literal_help()),
        loc(specialize_err(EExpr::List, list_literal_help())),
        ident_seq(),
    )
}

fn loc_term<'a>(options: ExprParseOptions) -> impl Parser<'a, Loc<Expr<'a>>, EExpr<'a>> {
    one_of!(
        loc_expr_in_parens_etc_help(),
        loc(specialize_err(EExpr::Str, string_like_literal_help())),
        loc(specialize_err(
            EExpr::Number,
            positive_number_literal_help()
        )),
        loc(specialize_err(EExpr::Closure, closure_help(options))),
        loc(record_literal_help()),
        loc(specialize_err(EExpr::List, list_literal_help())),
        ident_seq(),
    )
}

fn ident_seq<'a>() -> impl Parser<'a, Loc<Expr<'a>>, EExpr<'a>> {
    parse_ident_seq.trace("ident_seq")
}

fn parse_ident_seq<'a>(
    arena: &'a Bump,
    state: State<'a>,
    min_indent: u32,
) -> ParseResult<'a, Loc<Expr<'a>>, EExpr<'a>> {
    let (_, loc_ident, state) =
        loc(assign_or_destructure_identifier()).parse(arena, state, min_indent)?;
    let expr = ident_to_expr(arena, loc_ident.value);
    let (_p, suffixes, state) = record_field_access_chain()
        .trace("record_field_access_chain")
        .parse(arena, state, min_indent)
        .map_err(|(_p, e)| (MadeProgress, e))?;
    let expr = apply_expr_access_chain(arena, expr, suffixes);
    Ok((MadeProgress, Loc::at(loc_ident.region, expr), state))
}

fn underscore_expression<'a>() -> impl Parser<'a, Expr<'a>, EExpr<'a>> {
    move |arena: &'a Bump, state: State<'a>, min_indent: u32| {
        let start = state.pos();

        let (_, _, next_state) = byte(b'_', EExpr::Underscore).parse(arena, state, min_indent)?;

        let lowercase_ident_expr =
            { specialize_err(move |_, _| EExpr::End(start), lowercase_ident()) };

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
        let (_, _, next_state) = crate::parser::keyword(crate::keyword::CRASH, EExpr::Crash)
            .parse(arena, state, min_indent)?;

        Ok((MadeProgress, Expr::Crash, next_state))
    }
}

fn loc_possibly_negative_or_negated_term<'a>(
    options: ExprParseOptions,
) -> impl Parser<'a, Loc<Expr<'a>>, EExpr<'a>> {
    let parse_unary_negate = move |arena, state: State<'a>, min_indent: u32| {
        let initial = state.clone();

        let (_, (loc_op, loc_expr), state) =
            and(loc(unary_negate()), loc_term(options)).parse(arena, state, min_indent)?;

        let loc_expr = numeric_negate_expression(arena, initial, loc_op, loc_expr, &[]);

        Ok((MadeProgress, loc_expr, state))
    };

    one_of![
        parse_unary_negate,
        // this will parse negative numbers, which the unary negate thing up top doesn't (for now)
        loc(specialize_err(EExpr::Number, number_literal_help())),
        loc(map_with_arena(
            and(
                loc(byte(b'!', EExpr::Start)),
                space0_before_e(loc_term(options), EExpr::IndentStart)
            ),
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
        loc(specialize_err(EExpr::If, if_expr_help(options))),
        loc(specialize_err(EExpr::When, when::expr_help(options))),
        loc(specialize_err(EExpr::Expect, expect_help(options))),
        loc(specialize_err(EExpr::Dbg, dbg_help(options))),
        loc(import_help(options)),
        loc(specialize_err(EExpr::Closure, closure_help(options))),
        loc(expr_operator_chain(options)),
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

        let new_min_indent = if is_expr_suffixed(&expr.value) {
            min_indent + 1
        } else {
            min_indent
        };

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

                match parse_expr_end(
                    new_min_indent,
                    options,
                    expr_state,
                    arena,
                    state,
                    initial_state,
                ) {
                    Err(err) => Err(err),
                    Ok((progress, expr, new_state)) => {
                        // We need to check if we have just parsed a suffixed statement,
                        // if so, this is a defs node.
                        if is_top_level_suffixed(&expr) {
                            let def_region = Region::new(end, new_state.pos());
                            let value_def = ValueDef::Stmt(arena.alloc(Loc::at(def_region, expr)));

                            let mut defs = Defs::default();
                            defs.push_value_def(value_def, def_region, &[], &[]);

                            return parse_defs_expr(options, min_indent, defs, arena, new_state);
                        } else {
                            Ok((progress, expr, new_state))
                        }
                    }
                }
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
            && !is_expr_suffixed(&self.expr.value)
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
    options: ExprParseOptions,
    min_indent: u32,
    arena: &'a Bump,
    state: State<'a>,
) -> ParseResult<'a, Option<SingleDef<'a>>, EExpr<'a>> {
    let initial = state.clone();

    let mut spaces_before_current = &[] as &[_];
    let spaces_before_current_start = state.pos();

    let state = match space0_e(EExpr::IndentStart).parse(arena, state, min_indent) {
        Err((MadeProgress, bad_input @ EExpr::Space(_, _))) => {
            return Err((MadeProgress, bad_input));
        }
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

    let parse_expect_vanilla = crate::parser::keyword(crate::keyword::EXPECT, EExpect::Expect);
    let parse_expect_fx = crate::parser::keyword(crate::keyword::EXPECT_FX, EExpect::Expect);
    let parse_expect = either(parse_expect_fx, parse_expect_vanilla);

    match space0_after_e(crate::pattern::loc_pattern_help(), EPattern::IndentEnd).parse(
        arena,
        state.clone(),
        min_indent,
    ) {
        Err((NoProgress, _)) => {
            let pos_before_import = state.pos();
            match import().parse(arena, state.clone(), min_indent) {
                Err((NoProgress, _)) => {
                    match parse_expect.parse(arena, state.clone(), min_indent) {
                        Err((_, _)) => {
                            // a hacky way to get expression-based error messages. TODO fix this
                            Ok((NoProgress, None, initial))
                        }
                        Ok((_, expect_flavor, state)) => parse_statement_inside_def(
                            arena,
                            state,
                            min_indent,
                            options,
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
                Err((MadeProgress, err)) => {
                    Err((MadeProgress, EExpr::Import(err, pos_before_import)))
                }
                Ok((_, (loc_import, spaces_after), state)) => Ok((
                    MadeProgress,
                    Some(SingleDef {
                        type_or_value: Either::Second(loc_import.value),
                        region: loc_import.region,
                        spaces_before: spaces_before_current,
                        spaces_after,
                    }),
                    state,
                )),
            }
        }
        Err((MadeProgress, _)) => {
            // Try to parse as a Statement
            match parse_statement_inside_def(
                arena,
                initial.clone(),
                min_indent,
                options,
                start,
                spaces_before_current_start,
                // TODO including spaces_before_current here doubles things up
                &[],
                |_, loc_def_expr| -> ValueDef<'a> { ValueDef::Stmt(arena.alloc(loc_def_expr)) },
            ) {
                Ok((_, Some(single_def), state)) => match single_def.type_or_value {
                    Either::Second(ValueDef::Stmt(loc_expr))
                        if is_expr_suffixed(&loc_expr.value) =>
                    {
                        Ok((MadeProgress, Some(single_def), state))
                    }
                    _ => Ok((NoProgress, None, initial)), // a hacky way to get expression-based error messages. TODO fix this
                },
                _ => Ok((NoProgress, None, initial)), // a hacky way to get expression-based error messages. TODO fix this
            }
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
                if let Ok((_, loc_implements, state)) =
                    loc_implements_parser().parse(arena, state.clone(), min_indent)
                {
                    let (_, (type_def, def_region), state) = finish_parsing_ability_def_help(
                        min_indent,
                        Loc::at(name_region, name),
                        args,
                        loc_implements,
                        arena,
                        state,
                    )?;

                    return Ok((
                        MadeProgress,
                        Some(SingleDef {
                            type_or_value: Either::First(type_def),
                            region: def_region,
                            spaces_before: spaces_before_current,
                            spaces_after: &[],
                        }),
                        state,
                    ));
                }
            }

            // This may be a def or alias.
            let operator_result = operator().parse(arena, state.clone(), min_indent);

            if let Ok((_, BinOp::Assignment, operator_result_state)) = operator_result {
                return parse_single_def_assignment(
                    options,
                    // to support statements we have to increase the indent here so that we can parse a child def
                    // within a def and still continue to parse the final expression for this def
                    // e.g.
                    // main =
                    //     Stdout.line! "Bar"
                    //     a=Stdout.line! "Foo"
                    //     Task.ok {}
                    &operator_result_state.line_indent() + 1,
                    arena,
                    operator_result_state,
                    loc_pattern,
                    spaces_before_current,
                );
            };

            if let Ok((_, BinOp::IsAliasType, state)) = operator_result {
                // the increment_min_indent here is probably _wrong_, since alias_signature_with_space_before does
                // that internally.
                // TODO: re-evaluate this
                let parser = increment_min_indent(alias_signature_with_space_before());
                let (_, ann_type, state) = parser.parse(arena, state, min_indent)?;
                let region = Region::span_across(&loc_pattern.region, &ann_type.region);

                match &loc_pattern.value.extract_spaces().item {
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

                        return Ok((
                            MadeProgress,
                            Some(SingleDef {
                                type_or_value: Either::First(type_def),
                                region,
                                spaces_before: spaces_before_current,
                                spaces_after: &[],
                            }),
                            state,
                        ));
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

                        return Ok((
                            MadeProgress,
                            Some(SingleDef {
                                type_or_value: Either::First(type_def),
                                region,
                                spaces_before: spaces_before_current,
                                spaces_after: &[],
                            }),
                            state,
                        ));
                    }
                    _ => {
                        let value_def = ValueDef::Annotation(loc_pattern, ann_type);

                        return Ok((
                            MadeProgress,
                            Some(SingleDef {
                                type_or_value: Either::Second(value_def),
                                region,
                                spaces_before: spaces_before_current,
                                spaces_after: &[],
                            }),
                            state,
                        ));
                    }
                }
            };

            if let Ok((_, BinOp::IsOpaqueType, state)) = operator_result {
                let (_, (signature, derived), state) =
                    opaque_signature_with_space_before().parse(arena, state, min_indent + 1)?;
                let region = Region::span_across(&loc_pattern.region, &signature.region);

                match &loc_pattern.value.extract_spaces().item {
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

                        return Ok((
                            MadeProgress,
                            Some(SingleDef {
                                type_or_value: Either::First(type_def),
                                region,
                                spaces_before: spaces_before_current,
                                spaces_after: &[],
                            }),
                            state,
                        ));
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

                        return Ok((
                            MadeProgress,
                            Some(SingleDef {
                                type_or_value: Either::First(type_def),
                                region,
                                spaces_before: spaces_before_current,
                                spaces_after: &[],
                            }),
                            state,
                        ));
                    }
                    _ => {
                        let value_def = ValueDef::Annotation(loc_pattern, signature);

                        return Ok((
                            MadeProgress,
                            Some(SingleDef {
                                type_or_value: Either::Second(value_def),
                                region,
                                spaces_before: spaces_before_current,
                                spaces_after: &[],
                            }),
                            state,
                        ));
                    }
                }
            };

            // Otherwise try to re-parse as a Statement
            match parse_statement_inside_def(
                arena,
                initial.clone(),
                min_indent,
                options,
                start,
                spaces_before_current_start,
                // TODO figure out why including spaces_before_current here doubles things up
                &[],
                |_, loc_def_expr| -> ValueDef<'a> { ValueDef::Stmt(arena.alloc(loc_def_expr)) },
            ) {
                Ok((_, Some(single_def), state)) => match single_def.type_or_value {
                    Either::Second(ValueDef::Stmt(loc_expr))
                        if is_expr_suffixed(&loc_expr.value) =>
                    {
                        Ok((MadeProgress, Some(single_def), state))
                    }
                    _ => Ok((NoProgress, None, initial)),
                },
                _ => Ok((NoProgress, None, initial)),
            }
        }
    }
}

fn import<'a>() -> impl Parser<'a, (Loc<ValueDef<'a>>, &'a [CommentOrNewline<'a>]), EImport<'a>> {
    then(
        and(
            loc(skip_first(
                parser::keyword(keyword::IMPORT, EImport::Import),
                increment_min_indent(one_of!(import_body(), import_ingested_file_body())),
            )),
            space0_e(EImport::EndNewline),
        ),
        |_arena, state, progress, (import, spaces_after)| {
            if !spaces_after.is_empty() || state.has_reached_end() {
                Ok((progress, (import, spaces_after), state))
            } else {
                // We require EOF, comment, or newline after import
                Err((progress, EImport::EndNewline(state.pos())))
            }
        },
    )
}

fn import_body<'a>() -> impl Parser<'a, ValueDef<'a>, EImport<'a>> {
    map(
        record!(ModuleImport {
            before_name: space0_e(EImport::IndentStart),
            name: loc(imported_module_name()),
            params: optional(specialize_err(EImport::Params, import_params())),
            alias: optional(import_as()),
            exposed: optional(import_exposing())
        }),
        ValueDef::ModuleImport,
    )
}

fn import_params<'a>() -> impl Parser<'a, ModuleImportParams<'a>, EImportParams<'a>> {
    then(
        and(
            backtrackable(space0_e(EImportParams::Indent)),
            specialize_err(EImportParams::Record, record_help()),
        ),
        |arena, state, _, (before, record): (_, RecordHelp<'a>)| {
            if let Some(update) = record.update {
                return Err((
                    MadeProgress,
                    EImportParams::RecordUpdateFound(update.region),
                ));
            }

            let params = record.fields.map_items_result(arena, |loc_field| {
                match loc_field.value.to_assigned_field(arena) {
                    Ok(field) => Ok(Loc::at(loc_field.region, field)),
                    Err(FoundApplyValue) => Err((
                        MadeProgress,
                        EImportParams::RecordApplyFound(loc_field.region),
                    )),
                }
            })?;

            let import_params = ModuleImportParams { before, params };

            Ok((MadeProgress, import_params, state))
        },
    )
}

#[inline(always)]
fn imported_module_name<'a>() -> impl Parser<'a, ImportedModuleName<'a>, EImport<'a>> {
    record!(ImportedModuleName {
        package: optional(skip_second(
            specialize_err(|_, pos| EImport::PackageShorthand(pos), lowercase_ident()),
            byte(b'.', EImport::PackageShorthandDot)
        )),
        name: module_name_help(EImport::ModuleName)
    })
}

#[inline(always)]
fn import_as<'a>(
) -> impl Parser<'a, header::KeywordItem<'a, ImportAsKeyword, Loc<ImportAlias<'a>>>, EImport<'a>> {
    record!(header::KeywordItem {
        keyword: module::spaces_around_keyword(
            ImportAsKeyword,
            EImport::As,
            EImport::IndentAs,
            EImport::IndentAlias
        ),
        item: then(
            specialize_err(|_, pos| EImport::Alias(pos), loc(unqualified_ident())),
            |_arena, state, _progress, loc_ident| {
                match loc_ident.value.chars().next() {
                    Some(first) if first.is_uppercase() => Ok((
                        MadeProgress,
                        loc_ident.map(|ident| ImportAlias::new(ident)),
                        state,
                    )),
                    Some(_) => Err((MadeProgress, EImport::LowercaseAlias(loc_ident.region))),
                    None => Err((MadeProgress, EImport::Alias(state.pos()))),
                }
            }
        )
    })
}

#[inline(always)]
fn import_exposing<'a>() -> impl Parser<
    'a,
    header::KeywordItem<
        'a,
        ImportExposingKeyword,
        Collection<'a, Loc<Spaced<'a, header::ExposedName<'a>>>>,
    >,
    EImport<'a>,
> {
    record!(header::KeywordItem {
        keyword: module::spaces_around_keyword(
            ImportExposingKeyword,
            EImport::Exposing,
            EImport::IndentExposing,
            EImport::ExposingListStart,
        ),
        item: collection_trailing_sep_e(
            byte(b'[', EImport::ExposingListStart),
            loc(import_exposed_name()),
            byte(b',', EImport::ExposingListEnd),
            byte(b']', EImport::ExposingListEnd),
            Spaced::SpaceBefore
        )
    })
}

#[inline(always)]
fn import_exposed_name<'a>(
) -> impl Parser<'a, crate::ast::Spaced<'a, crate::header::ExposedName<'a>>, EImport<'a>> {
    map(
        specialize_err(|_, pos| EImport::ExposedName(pos), unqualified_ident()),
        |n| Spaced::Item(crate::header::ExposedName::new(n)),
    )
}

#[inline(always)]
fn import_ingested_file_body<'a>() -> impl Parser<'a, ValueDef<'a>, EImport<'a>> {
    map(
        record!(IngestedFileImport {
            before_path: space0_e(EImport::IndentStart),
            path: loc(specialize_err(
                |_, pos| EImport::IngestedPath(pos),
                string_literal::parse_str_literal()
            )),
            name: import_ingested_file_as(),
            annotation: optional(import_ingested_file_annotation())
        }),
        ValueDef::IngestedFileImport,
    )
}

#[inline(always)]
fn import_ingested_file_as<'a>(
) -> impl Parser<'a, header::KeywordItem<'a, ImportAsKeyword, Loc<&'a str>>, EImport<'a>> {
    record!(header::KeywordItem {
        keyword: module::spaces_around_keyword(
            ImportAsKeyword,
            EImport::As,
            EImport::IndentAs,
            EImport::IndentIngestedName
        ),
        item: specialize_err(|(), pos| EImport::IngestedName(pos), loc(lowercase_ident()))
    })
}

#[inline(always)]
fn import_ingested_file_annotation<'a>() -> impl Parser<'a, IngestedFileAnnotation<'a>, EImport<'a>>
{
    record!(IngestedFileAnnotation {
        before_colon: skip_second(
            backtrackable(space0_e(EImport::IndentColon)),
            byte(b':', EImport::Colon)
        ),
        annotation: specialize_err(EImport::Annotation, type_annotation::located(false))
    })
}

pub fn parse_single_def_assignment<'a>(
    options: ExprParseOptions,
    min_indent: u32,
    arena: &'a Bump,
    initial_state: State<'a>,
    def_loc_pattern: Loc<Pattern<'a>>,
    spaces_before_current: &'a [CommentOrNewline<'a>],
) -> ParseResult<'a, Option<SingleDef<'a>>, EExpr<'a>> {
    // Try and parse the expression
    let parse_def_expr =
        space0_before_e(increment_min_indent(expr_start(options)), EExpr::IndentEnd);
    let (progress_after_first, first_loc_expr, state_after_first_expression) =
        parse_def_expr.parse(arena, initial_state, min_indent)?;

    let region = Region::span_across(&def_loc_pattern.region, &first_loc_expr.region);

    // If the expression is actually a suffixed statement, then we need to continue
    // to parse the rest of the expression
    if is_top_level_suffixed(&first_loc_expr.value) {
        let mut defs = Defs::default();
        // Take the suffixed value and make it a e.g. Body(`{}=`, Apply(Var(...)))
        // we will keep the pattern `def_loc_pattern` for the new Defs
        defs.push_value_def(
            ValueDef::Stmt(arena.alloc(first_loc_expr)),
            region,
            spaces_before_current,
            &[],
        );

        // Try to parse the rest of the expression as multiple defs, which may contain sub-assignments
        match parse_defs_expr(
            options,
            min_indent,
            defs,
            arena,
            state_after_first_expression,
        ) {
            Ok((progress_after_rest_of_def, expr, state_after_rest_of_def)) => {
                let final_loc_expr = arena.alloc(Loc::at(region, expr));

                let value_def = ValueDef::Body(arena.alloc(def_loc_pattern), final_loc_expr);

                Ok((
                    progress_after_rest_of_def,
                    Some(SingleDef {
                        type_or_value: Either::Second(value_def),
                        region,
                        spaces_before: spaces_before_current,
                        spaces_after: &[],
                    }),
                    state_after_rest_of_def,
                ))
            }
            Err((progress, err)) => Err((progress, err)),
        }
    } else {
        let value_def = ValueDef::Body(arena.alloc(def_loc_pattern), arena.alloc(first_loc_expr));

        Ok((
            progress_after_first,
            Some(SingleDef {
                type_or_value: Either::Second(value_def),
                region,
                spaces_before: spaces_before_current,
                spaces_after: &[],
            }),
            state_after_first_expression,
        ))
    }
}

/// e.g. Things that can be on their own line in a def, e.g. `expect`, `expect-fx`, or `dbg`
#[allow(clippy::too_many_arguments)]
fn parse_statement_inside_def<'a>(
    arena: &'a Bump,
    state: State<'a>,
    min_indent: u32,
    options: ExprParseOptions,
    start: Position,
    spaces_before_current_start: Position,
    spaces_before_current: &'a [CommentOrNewline<'a>],
    get_value_def: impl Fn(Region, Loc<Expr<'a>>) -> ValueDef<'a>,
) -> Result<(Progress, Option<SingleDef<'a>>, State<'a>), (Progress, EExpr<'a>)> {
    let parse_def_expr =
        space0_before_e(increment_min_indent(expr_start(options)), EExpr::IndentEnd);
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
            spaces_after: &[],
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
    options: ExprParseOptions,
    min_indent: u32,
    mut defs: Defs<'a>,
    arena: &'a Bump,
    state: State<'a>,
) -> ParseResult<'a, Defs<'a>, EExpr<'a>> {
    let mut global_state = state;

    loop {
        // keep a copy in the event we get an EExpr::DefMissingFinalExpr
        let state_before = global_state.clone();

        let state = global_state;

        global_state = match parse_single_def(options, min_indent, arena, state) {
            Ok((_, Some(single_def), next_state)) => {
                let region = single_def.region;
                let spaces_before_current = single_def.spaces_before;
                let spaces_after_current = single_def.spaces_after;

                match single_def.type_or_value {
                    Either::First(type_def) => {
                        defs.push_type_def(
                            type_def,
                            region,
                            spaces_before_current,
                            spaces_after_current,
                        );
                    }
                    Either::Second(value_def) => {
                        // If we got a ValueDef::Body, check if a type annotation preceded it.
                        // If so, we may need to combine them into an AnnotatedBody.
                        let joined_def = match value_def {
                            ValueDef::Body(loc_pattern, loc_def_expr) => {
                                let region =
                                    Region::span_across(&loc_pattern.region, &loc_def_expr.region);

                                let signature_must_match_value = spaces_before_current.len() <= 1;
                                let value_name = &loc_pattern.value;

                                match defs.last() {
                                    Some(Err(ValueDef::Annotation(ann_pattern, ann_type)))
                                        if signature_must_match_value
                                            || ann_pattern.value.equivalent(value_name) =>
                                    {
                                        Some(join_ann_to_body!(
                                            arena,
                                            loc_pattern,
                                            loc_def_expr,
                                            ann_pattern,
                                            ann_type,
                                            spaces_before_current,
                                            region
                                        ))
                                    }
                                    Some(Ok(TypeDef::Alias {
                                        header,
                                        ann: ann_type,
                                    })) if signature_must_match_value
                                        || header
                                            .vars
                                            .first()
                                            .map(|var| var.value.equivalent(value_name))
                                            .unwrap_or(false) =>
                                    {
                                        Some(join_alias_to_body!(
                                            arena,
                                            loc_pattern,
                                            loc_def_expr,
                                            header,
                                            ann_type,
                                            spaces_before_current,
                                            region
                                        ))
                                    }
                                    _ => None,
                                }
                            }
                            _ => None,
                        };
                        if let Some((joined_def, region)) = joined_def {
                            defs.replace_with_value_def(defs.tags.len() - 1, joined_def, region);
                        } else {
                            defs.push_value_def(
                                value_def,
                                region,
                                spaces_before_current,
                                spaces_after_current,
                            );
                        }
                    }
                }

                next_state
            }
            Ok((progress, None, s)) => return Ok((progress, defs, s)),
            Err((MadeProgress, EExpr::DefMissingFinalExpr(..)))
            | Err((MadeProgress, EExpr::DefMissingFinalExpr2(..))) => {
                return Ok((MadeProgress, defs, state_before))
            }
            Err((progress, err)) => return Err((progress, err)),
        };
    }
}

#[derive(Debug)]
pub struct SingleDef<'a> {
    pub type_or_value: Either<TypeDef<'a>, ValueDef<'a>>,
    pub region: Region,
    pub spaces_before: &'a [CommentOrNewline<'a>],
    pub spaces_after: &'a [CommentOrNewline<'a>],
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
            match space0_before_e(expr_start(options), EExpr::IndentEnd).parse(
                arena,
                state.clone(),
                min_indent,
            ) {
                Err((_, fail)) => {
                    let mut def_state = def_state;
                    match def_state.pop_last_value() {
                        Some(loc_ret) => {
                            // If the poped value was the only item in defs - just return it as an expression
                            if def_state.is_empty() {
                                Ok((MadeProgress, loc_ret.value, state))
                            } else {
                                Ok((
                                    MadeProgress,
                                    Expr::Defs(arena.alloc(def_state), arena.alloc(loc_ret)),
                                    state,
                                ))
                            }
                        }
                        None => Err((
                            MadeProgress,
                            EExpr::DefMissingFinalExpr2(arena.alloc(fail), state.pos()),
                        )),
                    }
                }
                Ok((_, loc_ret, state)) => Ok((
                    MadeProgress,
                    Expr::Defs(arena.alloc(def_state), arena.alloc(loc_ret)),
                    state,
                )),
            }
        }
    }
}

fn alias_signature_with_space_before<'a>() -> impl Parser<'a, Loc<TypeAnnotation<'a>>, EExpr<'a>> {
    increment_min_indent(specialize_err(
        EExpr::Type,
        space0_before_e(type_annotation::located(false), EType::TIndentStart),
    ))
}

fn opaque_signature_with_space_before<'a>() -> impl Parser<
    'a,
    (
        Loc<TypeAnnotation<'a>>,
        Option<Loc<ImplementsAbilities<'a>>>,
    ),
    EExpr<'a>,
> {
    and(
        specialize_err(
            EExpr::Type,
            space0_before_e(
                type_annotation::located_opaque_signature(true),
                EType::TIndentStart,
            ),
        ),
        optional(backtrackable(specialize_err(
            EExpr::Type,
            space0_before_e(type_annotation::implements_abilities(), EType::TIndentStart),
        ))),
    )
}

#[derive(Copy, Clone, PartialEq, Eq)]
enum AliasOrOpaque {
    Alias,
    Opaque,
}

fn extract_tag_and_spaces<'a>(arena: &'a Bump, expr: Expr<'a>) -> Option<Spaces<'a, &'a str>> {
    let mut expr = expr.extract_spaces();

    loop {
        match &expr.item {
            Expr::ParensAround(inner_expr) => {
                let inner_expr = inner_expr.extract_spaces();
                expr.item = inner_expr.item;
                expr.before = merge_spaces(arena, expr.before, inner_expr.before);
                expr.after = merge_spaces(arena, inner_expr.after, expr.after);
            }
            Expr::Tag(tag) => {
                return Some(Spaces {
                    before: expr.before,
                    item: tag,
                    after: expr.after,
                });
            }
            _ => return None,
        }
    }
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

    let state = if let Some(tag) = extract_tag_and_spaces(arena, expr.value) {
        let name = tag.item;
        let mut type_arguments = Vec::with_capacity_in(arguments.len(), arena);

        for argument in arguments {
            match expr_to_pattern_help(arena, &argument.value) {
                Ok(good) => {
                    type_arguments.push(Loc::at(argument.region, good));
                }
                Err(()) => {
                    return Err((
                        MadeProgress,
                        EExpr::Pattern(
                            arena.alloc(EPattern::NotAPattern(state.pos())),
                            state.pos(),
                        ),
                    ));
                }
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
    } else {
        let call = to_call(arena, arguments, expr);

        match expr_to_pattern_help(arena, &call.value) {
            Ok(good) => {
                let parser = specialize_err(
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

                        let value_def = ValueDef::Annotation(Loc::at(expr_region, good), ann_type);

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
    };

    parse_defs_expr(options, min_indent, defs, arena, state)
}

mod ability {
    use super::*;
    use crate::{
        ast::{AbilityMember, Spaceable, Spaced},
        parser::{absolute_indented_seq, EAbility},
    };

    /// Parses a single ability demand line; see `parse_demand`.
    fn parse_demand_help<'a>() -> impl Parser<'a, AbilityMember<'a>, EAbility<'a>> {
        map(
            // Require the type to be more indented than the name
            absolute_indented_seq(
                specialize_err(|_, pos| EAbility::DemandName(pos), loc(lowercase_ident())),
                skip_first(
                    and(
                        // TODO: do we get anything from picking up spaces here?
                        space0_e(EAbility::DemandName),
                        byte(b':', EAbility::DemandColon),
                    ),
                    specialize_err(EAbility::Type, type_annotation::located(true)),
                ),
            ),
            |(name, typ): (Loc<&'a str>, Loc<TypeAnnotation<'a>>)| AbilityMember {
                name: name.map_owned(Spaced::Item),
                typ,
            },
        )
    }

    pub enum IndentLevel {
        PendingMin(u32),
        Exact(u32),
    }

    /// Parses an ability demand like `hash : a -> U64 where a implements Hash`, in the context of a larger
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
    loc_implements: Loc<Implements<'a>>,
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
        loc_implements,
        members: demands.into_bump_slice(),
    };

    Ok((MadeProgress, (type_def, def_region), state))
}

#[allow(clippy::too_many_arguments)]
fn parse_expr_operator<'a>(
    min_indent: u32,
    options: ExprParseOptions,
    mut expr_state: ExprState<'a>,
    loc_op: Loc<BinOp>,
    line_indent: u32,
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

            let indented_more = line_indent + 1;

            let call = expr_state
                .validate_assignment_or_backpassing(arena, loc_op, EExpr::ElmStyleFunction)
                .map_err(|fail| (MadeProgress, fail))?;

            let (value_def, def_region, state) = {
                match expr_to_pattern_help(arena, &call.value) {
                    Ok(good) => {
                        let (_, mut body, state) =
                            expr_start(options).parse(arena, state, indented_more)?;

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
                            expr_start(options).parse(arena, state, indented_more)?;

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

            let parse_cont = space0_before_e(expr_start(options), EExpr::IndentEnd);

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

                        let new_min_indent = if is_expr_suffixed(&new_expr.value) {
                            min_indent + 1
                        } else {
                            min_indent
                        };

                        match parse_expr_end(
                            new_min_indent,
                            options,
                            expr_state,
                            arena,
                            state,
                            initial_state,
                        ) {
                            Ok((progress, expr, state)) => {
                                if let Expr::BinOps(..) = expr {
                                    let def_region = expr.get_region_spanning_binops();
                                    let mut new_expr = Loc::at(def_region, expr);

                                    if is_expr_suffixed(&new_expr.value) {
                                        // We have parsed a statement such as `"hello" |> line!`
                                        // put the spaces from after the operator in front of the call
                                        if !spaces_after_operator.is_empty() {
                                            new_expr = arena.alloc(expr).with_spaces_before(
                                                spaces_after_operator,
                                                def_region,
                                            );
                                        }

                                        let value_def = ValueDef::Stmt(arena.alloc(new_expr));

                                        let mut defs = Defs::default();
                                        defs.push_value_def(value_def, def_region, &[], &[]);

                                        return parse_defs_expr(
                                            options, min_indent, defs, arena, state,
                                        );
                                    }
                                }

                                // else return the parsed expression
                                Ok((progress, expr, state))
                            }
                            Err(err) => Err(err),
                        }
                    }
                }
            }
            Err((NoProgress, _e)) => {
                return Err((MadeProgress, EExpr::TrailingOperator(state.pos())));
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
    let parser = skip_first(
        crate::blankspace::check_indent(EExpr::IndentEnd),
        loc_term_or_underscore(options),
    );

    match parser.parse(arena, state.clone(), min_indent) {
        Err((MadeProgress, f)) => Err((MadeProgress, f)),
        Ok((
            _,
            implements @ Loc {
                value:
                    Expr::Var {
                        module_name: "",
                        ident: crate::keyword::IMPLEMENTS,
                        ..
                    },
                ..
            },
            state,
        )) if matches!(expr_state.expr.value, Expr::Tag(..)) => {
            // This is an ability definition, `Ability arg1 ... implements ...`.

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

            // Attach any spaces to the `implements` keyword
            let implements = if !expr_state.spaces_after.is_empty() {
                arena
                    .alloc(Implements::Implements)
                    .with_spaces_before(expr_state.spaces_after, implements.region)
            } else {
                Loc::at(implements.region, Implements::Implements)
            };

            let args = arguments.into_bump_slice();
            let (_, (type_def, def_region), state) =
                finish_parsing_ability_def_help(min_indent, name, args, implements, arena, state)?;

            let mut defs = Defs::default();

            defs.push_type_def(type_def, def_region, &[], &[]);

            parse_defs_expr(options, min_indent, defs, arena, state)
        }
        Ok((_, mut arg, state)) => {
            let new_end = state.pos();

            let min_indent = if is_expr_suffixed(&arg.value) {
                min_indent + 1
            } else {
                min_indent
            };

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
            let line_indent = state.line_indent();
            match loc(operator()).parse(arena, state.clone(), min_indent) {
                Err((MadeProgress, f)) => Err((MadeProgress, f)),
                Ok((_, loc_op, state)) => {
                    expr_state.consume_spaces(arena);
                    let initial_state = before_op;
                    parse_expr_operator(
                        min_indent,
                        options,
                        expr_state,
                        loc_op,
                        line_indent,
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

                        let (_, mut patterns, state) = specialize_err_ref(
                            EExpr::Pattern,
                            crate::parser::sep_by0(
                                byte(b',', EPattern::Start),
                                space0_around_ee(
                                    crate::pattern::loc_pattern_help(),
                                    EPattern::Start,
                                    EPattern::IndentEnd,
                                ),
                            ),
                        )
                        .parse(arena, state, min_indent)
                        .map_err(|(progress, err)| {
                            // We were expecting the end of an expression, and parsed a comma
                            // therefore we are either on the LHS of backpassing or this is was
                            // in an invalid position.
                            if let EExpr::Pattern(EPattern::IndentEnd(_), pos) = err {
                                (progress, EExpr::UnexpectedComma(pos.sub(1)))
                            } else {
                                (progress, err)
                            }
                        })?;

                        expr_state.consume_spaces(arena);
                        let call = to_call(arena, expr_state.arguments, expr_state.expr);

                        let pattern = expr_to_pattern_help(arena, &call.value).map_err(|()| {
                            (
                                MadeProgress,
                                EExpr::Pattern(
                                    arena.alloc(EPattern::NotAPattern(state.pos())),
                                    state.pos(),
                                ),
                            )
                        })?;

                        let loc_pattern = Loc::at(call.region, pattern);

                        patterns.insert(0, loc_pattern);

                        match two_bytes(b'<', b'-', EExpr::BackpassArrow).parse(
                            arena,
                            state.clone(),
                            min_indent,
                        ) {
                            Err((_, fail)) => Err((MadeProgress, fail)),
                            Ok((_, _, state)) => {
                                let parse_body = space0_before_e(
                                    increment_min_indent(expr_start(options)),
                                    EExpr::IndentEnd,
                                );

                                let (_, loc_body, state) =
                                    parse_body.parse(arena, state, min_indent)?;

                                let parse_cont =
                                    space0_before_e(expr_start(options), EExpr::IndentEnd);

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

pub fn loc_expr<'a>(accept_multi_backpassing: bool) -> impl Parser<'a, Loc<Expr<'a>>, EExpr<'a>> {
    space0_before_e(
        expr_start(ExprParseOptions {
            accept_multi_backpassing,
            check_for_arrow: true,
        }),
        EExpr::IndentEnd,
    )
}

pub fn merge_spaces<'a>(
    arena: &'a Bump,
    a: &'a [CommentOrNewline<'a>],
    b: &'a [CommentOrNewline<'a>],
) -> &'a [CommentOrNewline<'a>] {
    if a.is_empty() {
        b
    } else if b.is_empty() {
        a
    } else {
        let mut merged = Vec::with_capacity_in(a.len() + b.len(), arena);
        merged.extend_from_slice(a);
        merged.extend_from_slice(b);
        merged.into_bump_slice()
    }
}

/// If the given Expr would parse the same way as a valid Pattern, convert it.
/// Example: (foo) could be either an Expr::Var("foo") or Pattern::Identifier("foo")
fn expr_to_pattern_help<'a>(arena: &'a Bump, expr: &Expr<'a>) -> Result<Pattern<'a>, ()> {
    let mut expr = expr.extract_spaces();

    if let Expr::ParensAround(loc_expr) = &expr.item {
        let expr_inner = loc_expr.extract_spaces();

        expr.before = merge_spaces(arena, expr.before, expr_inner.before);
        expr.after = merge_spaces(arena, expr_inner.after, expr.after);
        expr.item = expr_inner.item;
    }

    let mut pat = match expr.item {
        Expr::Var { module_name, ident } => {
            if module_name.is_empty() {
                Pattern::Identifier { ident }
            } else {
                Pattern::QualifiedIdentifier { module_name, ident }
            }
        }
        Expr::Underscore(opt_name) => Pattern::Underscore(opt_name),
        Expr::Tag(value) => Pattern::Tag(value),
        Expr::OpaqueRef(value) => Pattern::OpaqueRef(value),
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

            pattern
        }

        Expr::SpaceBefore(..)
        | Expr::SpaceAfter(..)
        | Expr::ParensAround(..)
        | Expr::RecordBuilder(..) => unreachable!(),

        Expr::Record(fields) => {
            let patterns = fields.map_items_result(arena, |loc_assigned_field| {
                let region = loc_assigned_field.region;
                let value = assigned_expr_field_to_pattern_help(arena, &loc_assigned_field.value)?;
                Ok(Loc { region, value })
            })?;

            Pattern::RecordDestructure(patterns)
        }

        Expr::Tuple(fields) => Pattern::Tuple(fields.map_items_result(arena, |loc_expr| {
            Ok(Loc {
                region: loc_expr.region,
                value: expr_to_pattern_help(arena, &loc_expr.value)?,
            })
        })?),

        Expr::Float(string) => Pattern::FloatLiteral(string),
        Expr::Num(string) => Pattern::NumLiteral(string),
        Expr::NonBase10Int {
            string,
            base,
            is_negative,
        } => Pattern::NonBase10Literal {
            string,
            base,
            is_negative,
        },
        // These would not have parsed as patterns
        Expr::AccessorFunction(_)
        | Expr::RecordAccess(_, _)
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
        | Expr::LowLevelDbg(_, _, _)
        | Expr::MalformedClosure
        | Expr::MalformedSuffixed(..)
        | Expr::PrecedenceConflict { .. }
        | Expr::MultipleRecordBuilders { .. }
        | Expr::UnappliedRecordBuilder { .. }
        | Expr::RecordUpdate { .. }
        | Expr::UnaryOp(_, _)
        | Expr::TaskAwaitBang(..)
        | Expr::Crash => return Err(()),

        Expr::Str(string) => Pattern::StrLiteral(string),
        Expr::SingleQuote(string) => Pattern::SingleQuote(string),
        Expr::MalformedIdent(string, problem) => Pattern::MalformedIdent(string, problem),
    };

    // Now we re-add the spaces

    if !expr.before.is_empty() {
        pat = Pattern::SpaceBefore(arena.alloc(pat), expr.before);
    }
    if !expr.after.is_empty() {
        pat = Pattern::SpaceAfter(arena.alloc(pat), expr.after);
    }

    Ok(pat)
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
        AssignedField::LabelOnly(name) => Pattern::Identifier { ident: name.value },
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

pub fn parse_top_level_defs<'a>(
    arena: &'a bumpalo::Bump,
    state: State<'a>,
    mut output: Defs<'a>,
) -> ParseResult<'a, Defs<'a>, EExpr<'a>> {
    let (_, initial_space, state) = space0_e(EExpr::IndentEnd).parse(arena, state, 0)?;

    let start_column = state.column();

    let options = ExprParseOptions {
        accept_multi_backpassing: true,
        check_for_arrow: true,
    };

    let existing_len = output.tags.len();

    let before = Slice::extend_new(&mut output.spaces, initial_space.iter().copied());

    let (_, mut output, state) = parse_defs_end(options, start_column, output, arena, state)?;

    let (_, final_space, state) = space0_e(EExpr::IndentEnd).parse(arena, state, start_column)?;

    if output.tags.len() > existing_len {
        // add surrounding whitespace
        let after = Slice::extend_new(&mut output.spaces, final_space.iter().copied());

        debug_assert!(output.space_before[existing_len].is_empty());
        output.space_before[existing_len] = before;

        let last = output.tags.len() - 1;
        debug_assert!(output.space_after[last].is_empty() || after.is_empty());
        output.space_after[last] = after;
    }

    Ok((MadeProgress, output, state))
}

// PARSER HELPERS

fn closure_help<'a>(options: ExprParseOptions) -> impl Parser<'a, Expr<'a>, EClosure<'a>> {
    // closure_help_help(options)
    map_with_arena(
        // After the first token, all other tokens must be indented past the start of the line
        indented_seq_skip_first(
            // All closures start with a '\' - e.g. (\x -> x + 1)
            byte_indent(b'\\', EClosure::Start),
            // Once we see the '\', we're committed to parsing this as a closure.
            // It may turn out to be malformed, but it is definitely a closure.
            and(
                // Parse the params
                // Params are comma-separated
                sep_by1_e(
                    byte(b',', EClosure::Comma),
                    space0_around_ee(
                        specialize_err(EClosure::Pattern, closure_param()),
                        EClosure::IndentArg,
                        EClosure::IndentArrow,
                    ),
                    EClosure::Arg,
                ),
                skip_first(
                    // Parse the -> which separates params from body
                    two_bytes(b'-', b'>', EClosure::Arrow),
                    // Parse the body
                    space0_before_e(
                        specialize_err_ref(EClosure::Body, expr_start(options)),
                        EClosure::IndentBody,
                    ),
                ),
            ),
        ),
        |arena: &'a Bump, (params, body)| {
            let params: Vec<'a, Loc<Pattern<'a>>> = params;
            let params: &'a [Loc<Pattern<'a>>] = params.into_bump_slice();
            Expr::Closure(params, arena.alloc(body))
        },
    )
}

mod when {
    use super::*;
    use crate::ast::WhenBranch;

    /// Parser for when expressions.
    pub fn expr_help<'a>(options: ExprParseOptions) -> impl Parser<'a, Expr<'a>, EWhen<'a>> {
        map_with_arena(
            and(
                indented_seq_skip_first(
                    parser::keyword(keyword::WHEN, EWhen::When),
                    space0_around_e_no_after_indent_check(
                        specialize_err_ref(EWhen::Condition, expr_start(options)),
                        EWhen::IndentCondition,
                    )
                ),
                // Note that we allow the `is` to be at any indent level, since this doesn't introduce any
                // ambiguity. The formatter will fix it up.
                //
                // We require that branches are indented relative to the line containing the `is`.
                indented_seq_skip_first(
                    parser::keyword(keyword::IS, EWhen::Is),
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

            let branch_parser = map(
                and(
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
                    branch_result(original_indent + 1),
                ),
                |((patterns, guard), expr)| {
                    let patterns: Vec<'a, _> = patterns;
                    WhenBranch {
                        patterns: patterns.into_bump_slice(),
                        value: expr,
                        guard,
                    }
                },
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
        and(
            branch_alternatives_help(pattern_indent_level),
            one_of![
                map(
                    skip_first(
                        parser::keyword(keyword::IF, EWhen::IfToken),
                        // TODO we should require space before the expression but not after
                        space0_around_ee(
                            specialize_err_ref(
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
            ],
        )
    }

    fn branch_single_alternative<'a>() -> impl Parser<'a, Loc<Pattern<'a>>, EWhen<'a>> {
        move |arena, state, min_indent| {
            let (_, spaces, state) =
                backtrackable(space0_e(EWhen::IndentPattern)).parse(arena, state, min_indent)?;

            let (_, loc_pattern, state) = space0_after_e(
                specialize_err(EWhen::Pattern, crate::pattern::loc_pattern_help()),
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
                                sep_by1(byte(b'|', EWhen::Bar), branch_single_alternative());

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
            skip_first(
                two_bytes(b'-', b'>', EWhen::Arrow),
                space0_before_e(
                    specialize_err_ref(EWhen::Branch, loc_expr(true)),
                    EWhen::IndentBranch,
                ),
            )
            .parse(arena, state, indent)
        }
    }
}

fn if_branch<'a>() -> impl Parser<'a, (Loc<Expr<'a>>, Loc<Expr<'a>>), EIf<'a>> {
    skip_second(
        and(
            skip_second(
                space0_around_ee(
                    specialize_err_ref(EIf::Condition, loc_expr(true)),
                    EIf::IndentCondition,
                    EIf::IndentThenToken,
                ),
                parser::keyword(keyword::THEN, EIf::Then),
            ),
            space0_around_ee(
                specialize_err_ref(EIf::ThenBranch, loc_expr(true)),
                EIf::IndentThenBranch,
                EIf::IndentElseToken,
            ),
        ),
        parser::keyword(keyword::ELSE, EIf::Else),
    )
}

fn expect_help<'a>(options: ExprParseOptions) -> impl Parser<'a, Expr<'a>, EExpect<'a>> {
    move |arena: &'a Bump, state: State<'a>, min_indent| {
        let start_column = state.column();

        let (_, _, state) =
            parser::keyword(keyword::EXPECT, EExpect::Expect).parse(arena, state, min_indent)?;

        let (_, condition, state) = space0_before_e(
            specialize_err_ref(
                EExpect::Condition,
                set_min_indent(start_column + 1, expr_start(options)),
            ),
            EExpect::IndentCondition,
        )
        .parse(arena, state, start_column + 1)
        .map_err(|(_, f)| (MadeProgress, f))?;

        let parse_cont = specialize_err_ref(
            EExpect::Continuation,
            space0_before_e(expr_start(options), EExpr::IndentEnd),
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
            parser::keyword(keyword::DBG, EExpect::Dbg).parse(arena, state, min_indent)?;

        let (_, condition, state) = space0_before_e(
            specialize_err_ref(
                EExpect::Condition,
                set_min_indent(start_column + 1, expr_start(options)),
            ),
            EExpect::IndentCondition,
        )
        .parse(arena, state, start_column + 1)
        .map_err(|(_, f)| (MadeProgress, f))?;

        let parse_cont = specialize_err_ref(
            EExpect::Continuation,
            space0_before_e(expr_start(options), EExpr::IndentEnd),
        );

        let (_, loc_cont, state) = parse_cont.parse(arena, state, min_indent)?;

        let expr = Expr::Dbg(arena.alloc(condition), arena.alloc(loc_cont));

        Ok((MadeProgress, expr, state))
    }
}

fn import_help<'a>(options: ExprParseOptions) -> impl Parser<'a, Expr<'a>, EExpr<'a>> {
    move |arena: &'a Bump, state: State<'a>, min_indent: u32| {
        let (_, (import_def, spaces_after), state) =
            specialize_err(EExpr::Import, import()).parse(arena, state, min_indent)?;

        let mut defs = Defs::default();
        defs.push_value_def(import_def.value, import_def.region, &[], spaces_after);

        parse_defs_expr(options, min_indent, defs, arena, state)
    }
}

fn if_expr_help<'a>(options: ExprParseOptions) -> impl Parser<'a, Expr<'a>, EIf<'a>> {
    move |arena: &'a Bump, state, min_indent| {
        let (_, _, state) =
            parser::keyword(keyword::IF, EIf::If).parse(arena, state, min_indent)?;

        let mut branches = Vec::with_capacity_in(1, arena);

        let mut loop_state = state;

        let state_final_else = loop {
            let (_, (cond, then_branch), state) =
                if_branch().parse(arena, loop_state, min_indent)?;

            branches.push((cond, then_branch));

            // try to parse another `if`
            // NOTE this drops spaces between the `else` and the `if`
            let optional_if = and(
                backtrackable(space0_e(EIf::IndentIf)),
                parser::keyword(keyword::IF, EIf::If),
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
            specialize_err_ref(EIf::ElseBranch, expr_start(options)),
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
                Some(Accessor::RecordField(ident)) => Expr::Var { module_name, ident },
                Some(Accessor::TupleIndex(_)) => {
                    // TODO: make this state impossible to represent in Ident::Access,
                    // by splitting out parts[0] into a separate field with a type of `&'a str`,
                    // rather than a `&'a [Accessor<'a>]`.
                    internal_error!("Parsed an Ident::Access with a first part of a tuple index");
                }
                None => {
                    internal_error!("Parsed an Ident::Access with no parts");
                }
            };

            // The remaining items in the iterator are record field accesses,
            // e.g. `bar` in `foo.bar.baz`, followed by `baz`
            for field in iter {
                // Wrap the previous answer in the new one, so we end up
                // with a nested Expr. That way, `foo.bar.baz` gets represented
                // in the AST as if it had been written (foo.bar).baz all along.
                match field {
                    Accessor::RecordField(field) => {
                        answer = Expr::RecordAccess(arena.alloc(answer), field);
                    }
                    Accessor::TupleIndex(index) => {
                        answer = Expr::TupleAccess(arena.alloc(answer), index);
                    }
                }
            }

            answer
        }
        Ident::AccessorFunction(string) => Expr::AccessorFunction(string),
        Ident::Malformed(string, problem) => Expr::MalformedIdent(string, problem),
    }
}

fn list_literal_help<'a>() -> impl Parser<'a, Expr<'a>, EList<'a>> {
    map_with_arena(
        collection_trailing_sep_e(
            byte(b'[', EList::Open),
            specialize_err_ref(EList::Expr, loc_expr(false)),
            byte(b',', EList::End),
            byte(b']', EList::End),
            Expr::SpaceBefore,
        ),
        |arena, elements: Collection<'a, _>| {
            let elements = elements.ptrify_items(arena);
            Expr::List(elements)
        },
    )
    .trace("list_literal")
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum RecordField<'a> {
    RequiredValue(Loc<&'a str>, &'a [CommentOrNewline<'a>], &'a Loc<Expr<'a>>),
    OptionalValue(Loc<&'a str>, &'a [CommentOrNewline<'a>], &'a Loc<Expr<'a>>),
    LabelOnly(Loc<&'a str>),
    SpaceBefore(&'a RecordField<'a>, &'a [CommentOrNewline<'a>]),
    SpaceAfter(&'a RecordField<'a>, &'a [CommentOrNewline<'a>]),
    ApplyValue(
        Loc<&'a str>,
        &'a [CommentOrNewline<'a>],
        &'a [CommentOrNewline<'a>],
        &'a Loc<Expr<'a>>,
    ),
}

#[derive(Debug)]
pub struct FoundApplyValue;

#[derive(Debug)]
struct FoundOptionalValue;

impl<'a> RecordField<'a> {
    fn is_apply_value(&self) -> bool {
        let mut current = self;

        loop {
            match current {
                RecordField::ApplyValue(_, _, _, _) => break true,
                RecordField::SpaceBefore(field, _) | RecordField::SpaceAfter(field, _) => {
                    current = *field;
                }
                _ => break false,
            }
        }
    }

    pub fn to_assigned_field(
        self,
        arena: &'a Bump,
    ) -> Result<AssignedField<'a, Expr<'a>>, FoundApplyValue> {
        use AssignedField::*;

        match self {
            RecordField::RequiredValue(loc_label, spaces, loc_expr) => {
                Ok(RequiredValue(loc_label, spaces, loc_expr))
            }

            RecordField::OptionalValue(loc_label, spaces, loc_expr) => {
                Ok(OptionalValue(loc_label, spaces, loc_expr))
            }

            RecordField::LabelOnly(loc_label) => Ok(LabelOnly(loc_label)),

            RecordField::ApplyValue(_, _, _, _) => Err(FoundApplyValue),

            RecordField::SpaceBefore(field, spaces) => {
                let assigned_field = field.to_assigned_field(arena)?;

                Ok(SpaceBefore(arena.alloc(assigned_field), spaces))
            }

            RecordField::SpaceAfter(field, spaces) => {
                let assigned_field = field.to_assigned_field(arena)?;

                Ok(SpaceAfter(arena.alloc(assigned_field), spaces))
            }
        }
    }

    fn to_builder_field(
        self,
        arena: &'a Bump,
    ) -> Result<RecordBuilderField<'a>, FoundOptionalValue> {
        use RecordBuilderField::*;

        match self {
            RecordField::RequiredValue(loc_label, spaces, loc_expr) => {
                Ok(Value(loc_label, spaces, loc_expr))
            }

            RecordField::OptionalValue(_, _, _) => Err(FoundOptionalValue),

            RecordField::LabelOnly(loc_label) => Ok(LabelOnly(loc_label)),

            RecordField::ApplyValue(loc_label, colon_spaces, arrow_spaces, loc_expr) => {
                Ok(ApplyValue(loc_label, colon_spaces, arrow_spaces, loc_expr))
            }

            RecordField::SpaceBefore(field, spaces) => {
                let builder_field = field.to_builder_field(arena)?;

                Ok(SpaceBefore(arena.alloc(builder_field), spaces))
            }

            RecordField::SpaceAfter(field, spaces) => {
                let builder_field = field.to_builder_field(arena)?;

                Ok(SpaceAfter(arena.alloc(builder_field), spaces))
            }
        }
    }
}

impl<'a> Spaceable<'a> for RecordField<'a> {
    fn before(&'a self, spaces: &'a [CommentOrNewline<'a>]) -> Self {
        RecordField::SpaceBefore(self, spaces)
    }
    fn after(&'a self, spaces: &'a [CommentOrNewline<'a>]) -> Self {
        RecordField::SpaceAfter(self, spaces)
    }
}

pub fn record_field<'a>() -> impl Parser<'a, RecordField<'a>, ERecord<'a>> {
    use RecordField::*;

    map_with_arena(
        and(
            specialize_err(|_, pos| ERecord::Field(pos), loc(lowercase_ident())),
            and(
                spaces(),
                optional(either(
                    and(byte(b':', ERecord::Colon), record_field_expr()),
                    and(
                        byte(b'?', ERecord::QuestionMark),
                        spaces_before(specialize_err_ref(ERecord::Expr, loc_expr(false))),
                    ),
                )),
            ),
        ),
        |arena: &'a bumpalo::Bump, (loc_label, (spaces, opt_loc_val))| {
            match opt_loc_val {
                Some(Either::First((_, RecordFieldExpr::Value(loc_val)))) => {
                    RequiredValue(loc_label, spaces, arena.alloc(loc_val))
                }

                Some(Either::First((_, RecordFieldExpr::Apply(arrow_spaces, loc_val)))) => {
                    ApplyValue(loc_label, spaces, arrow_spaces, arena.alloc(loc_val))
                }

                Some(Either::Second((_, loc_val))) => {
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
            }
        },
    )
}

enum RecordFieldExpr<'a> {
    Apply(&'a [CommentOrNewline<'a>], Loc<Expr<'a>>),
    Value(Loc<Expr<'a>>),
}

fn record_field_expr<'a>() -> impl Parser<'a, RecordFieldExpr<'a>, ERecord<'a>> {
    map_with_arena(
        and(
            spaces(),
            either(
                and(
                    two_bytes(b'<', b'-', ERecord::Arrow),
                    spaces_before(specialize_err_ref(ERecord::Expr, loc_expr(false))),
                ),
                specialize_err_ref(ERecord::Expr, loc_expr(false)),
            ),
        ),
        |arena: &'a bumpalo::Bump, (spaces, either)| match either {
            Either::First((_, loc_expr)) => RecordFieldExpr::Apply(spaces, loc_expr),
            Either::Second(loc_expr) => RecordFieldExpr::Value({
                if spaces.is_empty() {
                    loc_expr
                } else {
                    arena
                        .alloc(loc_expr.value)
                        .with_spaces_before(spaces, loc_expr.region)
                }
            }),
        },
    )
}

fn record_updateable_identifier<'a>() -> impl Parser<'a, Expr<'a>, ERecord<'a>> {
    specialize_err(
        |_, pos| ERecord::Updateable(pos),
        map_with_arena(parse_ident, ident_to_expr),
    )
}

struct RecordHelp<'a> {
    update: Option<Loc<Expr<'a>>>,
    fields: Collection<'a, Loc<RecordField<'a>>>,
}

fn record_help<'a>() -> impl Parser<'a, RecordHelp<'a>, ERecord<'a>> {
    between(
        byte(b'{', ERecord::Open),
        reset_min_indent(record!(RecordHelp {
            // You can optionally have an identifier followed by an '&' to
            // make this a record update, e.g. { Foo.user & username: "blah" }.
            update: optional(backtrackable(skip_second(
                spaces_around(
                    // We wrap the ident in an Expr here,
                    // so that we have a Spaceable value to work with,
                    // and then in canonicalization verify that it's an Expr::Var
                    // (and not e.g. an `Expr::Access`) and extract its string.
                    loc(record_updateable_identifier()),
                ),
                byte(b'&', ERecord::Ampersand)
            ))),
            fields: collection_inner(
                loc(record_field()),
                byte(b',', ERecord::End),
                RecordField::SpaceBefore
            ),
        })),
        byte(b'}', ERecord::End),
    )
}

fn record_literal_help<'a>() -> impl Parser<'a, Expr<'a>, EExpr<'a>> {
    then(
        and(
            specialize_err(EExpr::Record, record_help()),
            // there can be field access, e.g. `{ x : 4 }.x`
            record_field_access_chain(),
        ),
        move |arena, state, _, (record, accessors)| {
            let expr_result = match record.update {
                Some(update) => record_update_help(arena, update, record.fields),
                None => {
                    let is_record_builder = record
                        .fields
                        .iter()
                        .any(|field| field.value.is_apply_value());

                    if is_record_builder {
                        record_builder_help(arena, record.fields)
                    } else {
                        let fields = record.fields.map_items(arena, |loc_field| {
                            loc_field.map(|field| field.to_assigned_field(arena).unwrap())
                        });

                        Ok(Expr::Record(fields))
                    }
                }
            };

            match expr_result {
                Ok(expr) => {
                    let value = apply_expr_access_chain(arena, expr, accessors);

                    Ok((MadeProgress, value, state))
                }
                Err(err) => Err((MadeProgress, err)),
            }
        },
    )
}

fn record_update_help<'a>(
    arena: &'a Bump,
    update: Loc<Expr<'a>>,
    fields: Collection<'a, Loc<RecordField<'a>>>,
) -> Result<Expr<'a>, EExpr<'a>> {
    let result = fields.map_items_result(arena, |loc_field| {
        match loc_field.value.to_assigned_field(arena) {
            Ok(builder_field) => Ok(Loc {
                region: loc_field.region,
                value: builder_field,
            }),
            Err(FoundApplyValue) => Err(EExpr::RecordUpdateBuilder(loc_field.region)),
        }
    });

    result.map(|fields| Expr::RecordUpdate {
        update: &*arena.alloc(update),
        fields,
    })
}

fn record_builder_help<'a>(
    arena: &'a Bump,
    fields: Collection<'a, Loc<RecordField<'a>>>,
) -> Result<Expr<'a>, EExpr<'a>> {
    let result = fields.map_items_result(arena, |loc_field| {
        match loc_field.value.to_builder_field(arena) {
            Ok(builder_field) => Ok(Loc {
                region: loc_field.region,
                value: builder_field,
            }),
            Err(FoundOptionalValue) => Err(EExpr::OptionalValueInRecordBuilder(loc_field.region)),
        }
    });

    result.map(Expr::RecordBuilder)
}

fn apply_expr_access_chain<'a>(
    arena: &'a Bump,
    value: Expr<'a>,
    accessors: Vec<'a, Suffix<'a>>,
) -> Expr<'a> {
    accessors
        .into_iter()
        .fold(value, |value, accessor| match accessor {
            Suffix::Accessor(Accessor::RecordField(field)) => {
                Expr::RecordAccess(arena.alloc(value), field)
            }
            Suffix::Accessor(Accessor::TupleIndex(field)) => {
                Expr::TupleAccess(arena.alloc(value), field)
            }
            Suffix::TaskAwaitBang => Expr::TaskAwaitBang(arena.alloc(value)),
        })
}

fn string_like_literal_help<'a>() -> impl Parser<'a, Expr<'a>, EString<'a>> {
    map_with_arena(
        crate::string_literal::parse_str_like_literal(),
        |arena, lit| match lit {
            StrLikeLiteral::Str(s) => Expr::Str(s),
            StrLikeLiteral::SingleQuote(s) => {
                // TODO: preserve the original escaping
                Expr::SingleQuote(s.to_str_in(arena))
            }
        },
    )
}

fn positive_number_literal_help<'a>() -> impl Parser<'a, Expr<'a>, ENumber> {
    map(
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
        },
    )
}

fn number_literal_help<'a>() -> impl Parser<'a, Expr<'a>, ENumber> {
    map(crate::number_literal::number_literal(), |literal| {
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
        "!" => Err((NoProgress, to_error("!", state.pos()))),
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
