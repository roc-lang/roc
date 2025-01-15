use crate::ast::{
    AssignedField, Collection, CommentOrNewline, Defs, Expr, ExtractSpaces, Implements,
    ImplementsAbilities, ImportAlias, ImportAsKeyword, ImportExposingKeyword, ImportedModuleName,
    IngestedFileAnnotation, IngestedFileImport, ModuleImport, ModuleImportParams, Pattern,
    Spaceable, Spaced, Spaces, SpacesBefore, TypeAnnotation, TypeDef, TypeHeader, TypeVar,
    ValueDef,
};
use crate::blankspace::{
    loc_space0_e, require_newline_or_eof, space0_after_e, space0_around_ee, space0_before_e,
    space0_before_optional_after, space0_e, spaces, spaces_around, spaces_before,
};
use crate::header::module_name_help;
use crate::ident::{
    integer_ident, lowercase_ident, parse_ident, unqualified_ident, Accessor, Ident, Suffix,
};
use crate::parser::{
    self, and, backtrackable, between, byte, byte_indent, capture_line_indent, collection_inner,
    collection_trailing_sep_e, either, increment_min_indent, indented_seq_skip_first, loc, map,
    map_with_arena, optional, reset_min_indent, sep_by1, sep_by1_e, set_min_indent, skip_first,
    skip_second, specialize_err, specialize_err_ref, then, two_bytes, zero_or_more, EClosure,
    EExpect, EExpr, EIf, EImport, EImportParams, EInParens, EList, ENumber, ERecord, EReturn,
    EString, EType, EWhen, Either, ParseResult, Parser, SpaceProblem,
};
use crate::pattern::closure_param;
use crate::state::State;
use crate::string_literal::{self, StrLikeLiteral};
use crate::type_annotation;
use crate::{header, keyword};
use bumpalo::collections::Vec;
use bumpalo::Bump;
use roc_collections::soa::slice_extend_new;
use roc_error_macros::internal_error;
use roc_module::called_via::{BinOp, CalledVia, UnaryOp};
use roc_region::all::{Loc, Position, Region};

use crate::parser::Progress::{self, *};

pub fn expr_end<'a>() -> impl Parser<'a, (), EExpr<'a>> {
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
        space0_before_optional_after(loc_expr_block(false), EExpr::IndentStart, EExpr::IndentEnd),
        expr_end(),
    );

    match parser.parse(arena, state, min_indent) {
        Ok((_, expression, _)) => Ok(expression),
        Err((_, fail)) => Err(fail),
    }
}

/// Check for the `->` token, and raise an error if found
/// This is usually true, but false in if-guards
///
/// > Just foo if foo == 2 -> ...
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct CheckForArrow(pub bool);

pub fn expr_help<'a>() -> impl Parser<'a, Expr<'a>, EExpr<'a>> {
    move |arena, state: State<'a>, min_indent: u32| {
        loc_expr(false)
            .parse(arena, state, min_indent)
            .map(|(a, b, c)| (a, b.value, c))
    }
}

fn loc_expr_in_parens_help<'a>() -> impl Parser<'a, Loc<Expr<'a>>, EInParens<'a>> {
    then(
        loc(collection_trailing_sep_e(
            byte(b'(', EInParens::Open),
            specialize_err_ref(EInParens::Expr, loc_expr_block(true)),
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
        map(byte(b'?', EExpr::Access), |()| Suffix::TrySuffix)
    ))
}

/// In some contexts we want to parse the `_` as an expression, so it can then be turned into a
/// pattern later
fn loc_term_or_underscore_or_conditional<'a>(
    check_for_arrow: CheckForArrow,
    allow_conditional: bool,
) -> impl Parser<'a, Loc<Expr<'a>>, EExpr<'a>> {
    move |arena: &'a Bump, state: State<'a>, min_indent: u32| {
        if allow_conditional {
            match loc_conditional(check_for_arrow).parse(arena, state.clone(), min_indent) {
                Ok((_, expr, state)) => return Ok((MadeProgress, expr, state)),
                Err((MadeProgress, e)) => return Err((MadeProgress, e)),
                Err((NoProgress, _)) => {}
            }
        }

        loc_term_or_closure(check_for_arrow).parse(arena, state, min_indent)
    }
}
fn loc_conditional<'a>(
    check_for_arrow: CheckForArrow,
) -> impl Parser<'a, Loc<Expr<'a>>, EExpr<'a>> {
    one_of!(
        loc(specialize_err(EExpr::If, if_expr_help(check_for_arrow))),
        loc(specialize_err(
            EExpr::When,
            when::when_expr_help(check_for_arrow)
        )),
    )
}

/// In some contexts we want to parse the `_` as an expression, so it can then be turned into a
/// pattern later
fn loc_term_or_closure<'a>(
    check_for_arrow: CheckForArrow,
) -> impl Parser<'a, Loc<Expr<'a>>, EExpr<'a>> {
    one_of!(
        loc(specialize_err(
            EExpr::Closure,
            closure_help(check_for_arrow)
        )),
        loc_term(),
    )
    .trace("term_or_closure")
}

fn loc_term<'a>() -> impl Parser<'a, Loc<Expr<'a>>, EExpr<'a>> {
    map_with_arena(
        and(
            one_of!(
                loc_expr_in_parens_etc_help(),
                loc(specialize_err(EExpr::Str, string_like_literal_help())),
                loc(specialize_err(
                    EExpr::Number,
                    positive_number_literal_help()
                )),
                loc(crash_kw()),
                loc(specialize_err(EExpr::Dbg, dbg_kw())),
                loc(try_kw()),
                // In some contexts we want to parse the `_` as an expression, so it can then be turned into a
                // pattern later
                loc(underscore_expression()),
                loc(record_literal_help()),
                loc(specialize_err(EExpr::List, list_literal_help())),
                ident_seq(),
            ),
            zero_or_more(pnc_args()),
        ),
        #[allow(clippy::type_complexity)]
        |arena,
         (expr, arg_locs_with_suffixes_vec): (
            Loc<Expr<'a>>,
            bumpalo::collections::Vec<
                'a,
                (
                    Loc<Collection<'a, &'a Loc<Expr>>>,
                    Option<Vec<'a, Suffix<'a>>>,
                ),
            >,
        )| {
            let mut e = expr;
            let orig_region = e.region;
            for (args_loc, maybe_suffixes) in arg_locs_with_suffixes_vec.iter() {
                let value = if let Some(suffixes) = maybe_suffixes {
                    apply_expr_access_chain(
                        arena,
                        Expr::PncApply(arena.alloc(e), args_loc.value),
                        suffixes.clone(),
                    )
                } else {
                    Expr::PncApply(arena.alloc(e), args_loc.value)
                };
                e = Loc {
                    value,
                    region: Region::span_across(&orig_region, &args_loc.region),
                };
            }
            e
        },
    )
    .trace("term")
}

fn pnc_args<'a>() -> impl Parser<
    'a,
    (
        Loc<Collection<'a, &'a Loc<Expr<'a>>>>,
        Option<Vec<'a, Suffix<'a>>>,
    ),
    EExpr<'a>,
> {
    |arena: &'a Bump, state: State<'a>, min_indent: u32| {
        let args_then_suffixes = and(
            specialize_err(
                EExpr::InParens,
                loc(collection_trailing_sep_e(
                    byte(b'(', EInParens::Open),
                    specialize_err_ref(EInParens::Expr, loc_expr(true)),
                    byte(b',', EInParens::End),
                    byte(b')', EInParens::End),
                    Expr::SpaceBefore,
                )),
            ),
            optional(record_field_access_chain()),
        );
        map_with_arena(
            args_then_suffixes,
            |arena: &'a Bump,
             (loc_args_coll, maybe_suffixes): (
                Loc<Collection<'a, Loc<Expr<'a>>>>,
                Option<Vec<'a, Suffix<'a>>>,
            )| {
                let args = loc_args_coll.value.ptrify_items(arena);
                (
                    Loc {
                        region: loc_args_coll.region,
                        value: args,
                    },
                    maybe_suffixes,
                )
            },
        )
        .parse(arena, state, min_indent)
    }
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
    (move |arena: &'a Bump, state: State<'a>, min_indent: u32| {
        let (_, _, next_state) = crate::parser::keyword(crate::keyword::CRASH, EExpr::Crash)
            .parse(arena, state, min_indent)?;

        Ok((MadeProgress, Expr::Crash, next_state))
    })
    .trace("crash_kw")
}

fn loc_possibly_negative_or_negated_term<'a>(
    check_for_arrow: CheckForArrow,
    allow_negate: bool,
    allow_conditional: bool,
) -> impl Parser<'a, Loc<Expr<'a>>, EExpr<'a>> {
    let parse_unary_negate = move |arena, state: State<'a>, min_indent: u32| {
        let initial = state.clone();

        if !allow_negate {
            return Err((NoProgress, EExpr::UnaryNegate(state.pos())));
        }

        let (_, (loc_op, loc_expr), state) = and(
            loc(unary_negate()),
            loc_possibly_negative_or_negated_term(check_for_arrow, true, false),
        )
        .parse(arena, state, min_indent)?;

        let loc_expr = numeric_negate_expression(arena, initial, loc_op, loc_expr, &[]);

        Ok((MadeProgress, loc_expr, state))
    };

    one_of![
        parse_unary_negate.trace("negate_expr"),
        // this will parse negative numbers, which the unary negate thing up top doesn't (for now)
        // loc(specialize_err(EExpr::Number, number_literal_help())),
        loc(map_with_arena(
            and(
                loc(unary_not()).trace("not"),
                space0_before_e(
                    loc_possibly_negative_or_negated_term(check_for_arrow, true, false),
                    EExpr::IndentStart
                )
                .trace("not_expr")
            ),
            |arena: &'a Bump, (loc_op, loc_expr): (Loc<_>, _)| {
                Expr::UnaryOp(arena.alloc(loc_expr), Loc::at(loc_op.region, UnaryOp::Not))
            }
        ))
        .trace("not_expr"),
        loc_term_or_underscore_or_conditional(check_for_arrow, allow_conditional)
    ]
    .trace("loc_possibly_negative_or_negated_term")
}

fn fail_expr_start_e<'a, T: 'a>() -> impl Parser<'a, T, EExpr<'a>> {
    |_arena, state: State<'a>, _min_indent: u32| Err((NoProgress, EExpr::Start(state.pos())))
}

fn unary_negate<'a>() -> impl Parser<'a, (), EExpr<'a>> {
    move |_arena: &'a Bump, state: State<'a>, min_indent: u32| {
        // a minus is unary iff
        //
        // - it is preceded by whitespace (spaces, newlines, comments)
        // - it is not followed by whitespace
        // - it is not followed by >, making ->
        let followed_by_illegal_char = state
            .bytes()
            .get(1)
            .map(|c| c.is_ascii_whitespace() || *c == b'#' || *c == b'>')
            .unwrap_or(false);

        if state.bytes().starts_with(b"-")
            && !followed_by_illegal_char
            && state.column() >= min_indent
        {
            // the negate is only unary if it is not followed by whitespace
            let state = state.advance(1);
            Ok((MadeProgress, (), state))
        } else {
            // this is not a negated expression
            Err((NoProgress, EExpr::UnaryNot(state.pos())))
        }
    }
}

fn unary_not<'a>() -> impl Parser<'a, (), EExpr<'a>> {
    move |_arena: &'a Bump, state: State<'a>, min_indent: u32| {
        let followed_by_equals = state.bytes().get(1).map(|c| *c == b'=').unwrap_or(false);

        if state.bytes().starts_with(b"!") && !followed_by_equals && state.column() >= min_indent {
            let state = state.advance(1);
            Ok((MadeProgress, (), state))
        } else {
            Err((NoProgress, EExpr::UnaryNot(state.pos())))
        }
    }
}

/// Entry point for parsing an expression.
fn expr_start<'a>(
    check_for_arrow: CheckForArrow,
    allow_any_indent: bool,
) -> impl Parser<'a, Loc<Expr<'a>>, EExpr<'a>> {
    one_of![
        loc(specialize_err(EExpr::If, if_expr_help(check_for_arrow))),
        loc(specialize_err(
            EExpr::When,
            when::when_expr_help(check_for_arrow)
        )),
        loc(specialize_err(
            EExpr::Closure,
            closure_help(check_for_arrow)
        )),
        loc(expr_operator_chain(check_for_arrow, allow_any_indent)),
        fail_expr_start_e()
    ]
    .trace("expr_start")
}

/// Parse a chain of expressions separated by operators. Also handles function application.
fn expr_operator_chain<'a>(
    check_for_arrow: CheckForArrow,
    allow_any_indent: bool,
) -> impl Parser<'a, Expr<'a>, EExpr<'a>> {
    (move |arena, state: State<'a>, min_indent: u32| {
        parse_expr_operator_chain(arena, state, min_indent, check_for_arrow, allow_any_indent)
    })
    .trace("expr_operator_chain")
}

fn parse_expr_operator_chain<'a>(
    arena: &'a Bump,
    state: State<'a>,
    min_indent: u32,
    check_for_arrow: CheckForArrow,
    allow_any_indent: bool,
) -> Result<(Progress, Expr<'a>, State<'a>), (Progress, EExpr<'a>)> {
    let line_indent = state.line_indent();

    let (_, expr, state) = loc_possibly_negative_or_negated_term(check_for_arrow, true, true)
        .parse(arena, state, min_indent)?;

    let mut initial_state = state.clone();
    let mut end = state.pos();

    let (spaces_before_op, state) =
        match space0_e(EExpr::IndentEnd).parse(arena, state.clone(), min_indent) {
            Err((_, _)) => return Ok((MadeProgress, expr.value, state)),
            Ok((_, spaces_before_op, state)) => (spaces_before_op, state),
        };

    let mut expr_state = ExprState {
        operators: Vec::new_in(arena),
        arguments: Vec::new_in(arena),
        expr,
        spaces_after: spaces_before_op,
        end: initial_state.pos(),
    };

    let mut state = state;

    let call_min_indent = if allow_any_indent { 0 } else { line_indent + 1 };

    loop {
        let allow_negate = state.pos() > end;
        let parser = skip_first(
            crate::blankspace::check_indent(EExpr::IndentEnd),
            loc_possibly_negative_or_negated_term(check_for_arrow, allow_negate, false),
        )
        .trace("term_or_underscore");
        end = state.pos();
        match parser.parse(arena, state.clone(), call_min_indent) {
            Err((MadeProgress, f)) => return Err((MadeProgress, f)),
            Err((NoProgress, _)) => {
                let before_op = state.clone();
                // try an operator
                return parse_expr_after_apply(
                    arena,
                    state,
                    min_indent,
                    call_min_indent,
                    check_for_arrow,
                    true,
                    expr_state,
                    before_op,
                    initial_state,
                );
            }
            Ok((_, arg, new_state)) => {
                state = new_state;
                initial_state = state.clone();

                if parse_after_expr_arg_and_check_final(
                    arena,
                    &mut state,
                    min_indent,
                    &mut expr_state,
                    arg,
                ) {
                    let expr = parse_expr_final(expr_state, arena);
                    return Ok((MadeProgress, expr, state));
                }

                continue;
            }
        }
    }
}

/// We're part way thru parsing an expression, e.g. `bar foo `.
/// We just tried parsing an argument and determined we couldn't -
/// so we're going to try parsing an operator.
#[allow(clippy::too_many_arguments)]
fn parse_expr_after_apply<'a>(
    arena: &'a Bump,
    state: State<'a>,
    min_indent: u32,
    call_min_indent: u32,
    check_for_arrow: CheckForArrow,
    check_for_defs: bool,
    mut expr_state: ExprState<'a>,
    before_op: State<'a>,
    initial_state: State<'a>,
) -> Result<(Progress, Expr<'a>, State<'a>), (Progress, EExpr<'a>)> {
    match loc(bin_op(check_for_defs)).parse(arena, state.clone(), call_min_indent) {
        Err((MadeProgress, f)) => Err((MadeProgress, f)),
        Ok((_, loc_op, state)) => {
            expr_state.consume_spaces(arena);
            let initial_state = before_op;
            parse_expr_operator(
                arena,
                state,
                min_indent,
                call_min_indent,
                check_for_arrow,
                check_for_defs,
                expr_state,
                loc_op,
                initial_state,
            )
        }
        Err((NoProgress, _)) => {
            let expr = parse_expr_final(expr_state, arena);
            // roll back space parsing
            Ok((MadeProgress, expr, initial_state))
        }
    }
}

fn expr_to_stmt(expr: Loc<Expr<'_>>) -> Loc<Stmt<'_>> {
    Loc::at(expr.region, Stmt::Expr(expr.value))
}

pub fn parse_repl_defs_and_optional_expr<'a>(
    arena: &'a Bump,
    state: State<'a>,
) -> ParseResult<'a, (Defs<'a>, Option<Loc<Expr<'a>>>), EExpr<'a>> {
    let initial_state = state.clone();
    let (spaces_before, state) = match loc(space0_e(EExpr::IndentEnd)).parse(arena, state, 0) {
        Err((NoProgress, _)) => return Ok((NoProgress, (Defs::default(), None), initial_state)),
        Err((MadeProgress, e)) => return Err((MadeProgress, e)),
        Ok((_, sp, state)) => (sp, state),
    };

    let (_, stmts, state) = parse_stmt_seq(
        arena,
        state,
        |e, _| e.clone(),
        CheckForArrow(true),
        0,
        spaces_before,
        EExpr::IndentEnd,
    )?;

    let state = match space0_e(EExpr::IndentEnd).parse(arena, state.clone(), 0) {
        Err((NoProgress, _)) => state,
        Err((MadeProgress, e)) => return Err((MadeProgress, e)),
        Ok((_, _, state)) => state,
    };

    if !state.has_reached_end() {
        return Err((MadeProgress, EExpr::End(state.pos())));
    }

    let (defs, last_expr) =
        stmts_to_defs(&stmts, Defs::default(), false, arena).map_err(|e| (MadeProgress, e))?;

    Ok((MadeProgress, (defs, last_expr), state))
}

fn stmt_start<'a>(
    check_for_arrow: CheckForArrow,
    preceding_comment: Region,
) -> impl Parser<'a, Loc<Stmt<'a>>, EExpr<'a>> {
    one_of![
        map(
            loc(specialize_err(EExpr::If, if_expr_help(check_for_arrow))),
            expr_to_stmt
        ),
        map(
            loc(specialize_err(
                EExpr::When,
                when::when_expr_help(check_for_arrow)
            )),
            expr_to_stmt
        ),
        loc(specialize_err(
            EExpr::Expect,
            expect_help(check_for_arrow, preceding_comment)
        )),
        loc(specialize_err(EExpr::Return, return_help(check_for_arrow))),
        loc(specialize_err(EExpr::Import, map(import(), Stmt::ValueDef))),
        map(
            loc(specialize_err(
                EExpr::Closure,
                closure_help(check_for_arrow)
            )),
            expr_to_stmt
        ),
        loc(stmt_operator_chain(check_for_arrow)),
        fail_expr_start_e()
    ]
    .trace("stmt_start")
}

fn stmt_operator_chain<'a>(check_for_arrow: CheckForArrow) -> impl Parser<'a, Stmt<'a>, EExpr<'a>> {
    (move |arena, state: State<'a>, min_indent: u32| {
        parse_stmt_operator_chain(arena, state, min_indent, check_for_arrow)
    })
    .trace("stmt_operator_chain")
}

fn parse_stmt_operator_chain<'a>(
    arena: &'a Bump,
    state: State<'a>,
    min_indent: u32,
    check_for_arrow: CheckForArrow,
) -> Result<(Progress, Stmt<'a>, State<'a>), (Progress, EExpr<'a>)> {
    let line_indent = state.line_indent();

    let (_, expr, state) = loc_possibly_negative_or_negated_term(check_for_arrow, true, true)
        .parse(arena, state, min_indent)?;

    let mut initial_state = state.clone();
    let mut end = state.pos();

    let (spaces_before_op, state) =
        match space0_e(EExpr::IndentEnd).parse(arena, state.clone(), min_indent) {
            Err((_, _)) => return Ok((MadeProgress, Stmt::Expr(expr.value), state)),
            Ok((_, spaces_before_op, state)) => (spaces_before_op, state),
        };

    let mut expr_state = ExprState {
        operators: Vec::new_in(arena),
        arguments: Vec::new_in(arena),
        expr,
        spaces_after: spaces_before_op,
        end,
    };

    let mut state = state;

    let call_min_indent = line_indent + 1;

    loop {
        let allow_negate = state.pos() > end;
        let parser = skip_first(
            crate::blankspace::check_indent(EExpr::IndentEnd),
            loc_possibly_negative_or_negated_term(check_for_arrow, allow_negate, false),
        );
        end = state.pos();
        match parser.parse(arena, state.clone(), call_min_indent) {
            Err((MadeProgress, f)) => return Err((MadeProgress, f)),
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
            )) if matches!(
                expr_state.expr.value,
                Expr::Tag(..)
                    | Expr::Apply(
                        Loc {
                            region: _,
                            value: Expr::Tag(..)
                        },
                        &[],
                        _
                    )
            ) =>
            {
                return parse_ability_def(expr_state, state, arena, implements, call_min_indent)
                    .map(|(td, s)| (MadeProgress, Stmt::TypeDef(td), s));
            }
            Err((NoProgress, _)) => {
                // try an operator
                return parse_stmt_after_apply(
                    arena,
                    state,
                    min_indent,
                    call_min_indent,
                    expr_state,
                    check_for_arrow,
                    initial_state,
                );
            }
            Ok((_, arg, new_state)) => {
                state = new_state;
                initial_state = state.clone();

                if parse_after_expr_arg_and_check_final(
                    arena,
                    &mut state,
                    min_indent,
                    &mut expr_state,
                    arg,
                ) {
                    let expr = parse_expr_final(expr_state, arena);
                    return Ok((MadeProgress, Stmt::Expr(expr), state));
                }

                continue;
            }
        }
    }
}

fn parse_after_expr_arg_and_check_final<'a>(
    arena: &'a Bump,
    state: &mut State<'a>,
    min_indent: u32,
    expr_state: &mut ExprState<'a>,
    mut arg: Loc<Expr<'a>>,
) -> bool {
    let new_end = state.pos();

    if !expr_state.spaces_after.is_empty() {
        arg = arena
            .alloc(arg.value)
            .with_spaces_before(expr_state.spaces_after, arg.region);

        expr_state.spaces_after = &[];
    }
    let new_spaces = match space0_e(EExpr::IndentEnd).parse(arena, state.clone(), min_indent) {
        Err((_, _)) => {
            expr_state.arguments.push(arena.alloc(arg));
            expr_state.end = new_end;
            expr_state.spaces_after = &[];

            return true;
        }
        Ok((_, new_spaces, new_state)) => {
            *state = new_state;
            new_spaces
        }
    };
    expr_state.arguments.push(arena.alloc(arg));
    expr_state.end = new_end;
    expr_state.spaces_after = new_spaces;

    false
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

    fn validate_assignment<F>(
        mut self,
        arena: &'a Bump,
        loc_op: Loc<OperatorOrDef>,
        argument_error: F,
    ) -> Result<Loc<Expr<'a>>, EExpr<'a>>
    where
        F: Fn(Region, Position) -> EExpr<'a>,
    {
        if !self.operators.is_empty() {
            // this `=` likely occurred inline; treat it as an invalid operator
            Err(EExpr::BadOperator("=", loc_op.region.start()))
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
        kind: Loc<AliasOrOpaque>,
    ) -> Result<(Loc<Expr<'a>>, Vec<'a, &'a Loc<Expr<'a>>>), EExpr<'a>> {
        if !self.operators.is_empty() {
            // this `:`/`:=` likely occurred inline; treat it as an invalid operator
            let op = match kind.value {
                AliasOrOpaque::Alias => ":",
                AliasOrOpaque::Opaque => ":=",
            };
            let fail = EExpr::BadOperator(op, kind.region.start());

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
        Expr::Num(string) if !string.starts_with('-') => {
            let new_string =
                unsafe { std::str::from_utf8_unchecked(&state.bytes()[..string.len() + 1]) };

            Expr::Num(new_string)
        }
        Expr::Float(string) if !string.starts_with('-') => {
            let new_string =
                unsafe { std::str::from_utf8_unchecked(&state.bytes()[..string.len() + 1]) };

            Expr::Float(new_string)
        }
        Expr::NonBase10Int {
            string,
            base,
            is_negative: false,
        } => {
            // don't include the minus sign here; it will not be parsed right
            Expr::NonBase10Int {
                is_negative: true,
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
            specialize_err(EImportParams::Record, loc(record_help())),
        ),
        |arena, state, _, (before, loc_record): (_, Loc<RecordHelp<'a>>)| {
            if let Some(prefix) = loc_record.value.prefix {
                match prefix {
                    (update, RecordHelpPrefix::Update) => {
                        return Err((
                            MadeProgress,
                            EImportParams::RecordUpdateFound(update.region),
                        ))
                    }
                    (mapper, RecordHelpPrefix::Mapper) => {
                        return Err((
                            MadeProgress,
                            EImportParams::RecordBuilderFound(mapper.region),
                        ))
                    }
                }
            }

            let params = loc_record
                .value
                .fields
                .map_items_result(arena, |loc_field| {
                    match loc_field.value.to_assigned_field(arena) {
                        AssignedField::IgnoredValue(_, _, _) => Err((
                            MadeProgress,
                            EImportParams::RecordIgnoredFieldFound(loc_field.region),
                        )),
                        field => Ok(Loc::at(loc_field.region, field)),
                    }
                })?;

            let import_params = ModuleImportParams {
                before,
                params: Loc::at(loc_record.region, params),
            };

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
        keyword: header::spaces_around_keyword(
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
        keyword: header::spaces_around_keyword(
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
        keyword: header::spaces_around_keyword(
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

fn alias_signature<'a>() -> impl Parser<'a, Loc<TypeAnnotation<'a>>, EExpr<'a>> {
    increment_min_indent(specialize_err(EExpr::Type, type_annotation::located(false)))
}

fn opaque_signature<'a>(
) -> impl Parser<'a, (Loc<TypeAnnotation<'a>>, Option<&'a ImplementsAbilities<'a>>), EExpr<'a>> {
    and(
        specialize_err(EExpr::Type, type_annotation::located_opaque_signature(true)),
        optional(map_with_arena(
            specialize_err(EExpr::Type, type_annotation::implements_abilities()),
            |arena, item| &*arena.alloc(item),
        )),
    )
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum AliasOrOpaque {
    Alias,  // ':'
    Opaque, // ':='
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

/// We just saw a ':' or ':=', and we're trying to parse an alias or opaque type definition.
#[allow(clippy::too_many_arguments)]
fn parse_stmt_alias_or_opaque<'a>(
    arena: &'a Bump,
    state: State<'a>,
    min_indent: u32,
    expr_state: ExprState<'a>,
    kind: Loc<AliasOrOpaque>,
    spaces_after_operator: &'a [CommentOrNewline<'a>],
) -> ParseResult<'a, Stmt<'a>, EExpr<'a>> {
    let expr_region = expr_state.expr.region;
    let indented_more = min_indent + 1;

    let (expr, arguments) = expr_state
        .validate_is_type_def(arena, kind)
        .map_err(|fail| (MadeProgress, fail))?;

    let (res, state) = if let Some(tag) = extract_tag_and_spaces(arena, expr.value) {
        let name = tag.item;
        let mut type_arguments = Vec::with_capacity_in(arguments.len(), arena);

        for argument in arguments {
            type_arguments.push(Loc::at(
                argument.region,
                expr_to_type_var(arena, &argument.value),
            ));
        }

        match kind.value {
            AliasOrOpaque::Alias => {
                let (_, signature, state) = alias_signature().parse(arena, state, min_indent)?;

                let signature = signature.map(|v| v.maybe_before(arena, spaces_after_operator));

                let header = TypeHeader {
                    name: Loc::at(expr.region, name),
                    vars: type_arguments.into_bump_slice(),
                };

                let def = TypeDef::Alias {
                    header,
                    ann: signature,
                };

                (Stmt::TypeDef(def), state)
            }

            AliasOrOpaque::Opaque => {
                let (_, (signature, derived), state) =
                    opaque_signature().parse(arena, state, indented_more)?;

                let signature = signature.map(|v| v.maybe_before(arena, spaces_after_operator));

                let header = TypeHeader {
                    name: Loc::at(expr.region, name),
                    vars: type_arguments.into_bump_slice(),
                };

                let def = TypeDef::Opaque {
                    header,
                    typ: signature,
                    derived,
                };

                (Stmt::TypeDef(def), state)
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

                        let value_def = ValueDef::Annotation(Loc::at(expr_region, good), ann_type);

                        (Stmt::ValueDef(value_def), state)
                    }
                }
            }
            Err(_) => {
                // this `:`/`:=` likely occurred inline; treat it as an invalid operator
                let op = match kind.value {
                    AliasOrOpaque::Alias => ":",
                    AliasOrOpaque::Opaque => ":=",
                };
                let fail = EExpr::BadOperator(op, kind.region.start());

                return Err((MadeProgress, fail));
            }
        }
    };

    Ok((MadeProgress, res, state))
}

fn expr_to_type_var<'a>(arena: &'a Bump, expr: &'a Expr<'a>) -> TypeVar<'a> {
    let expr = expr.extract_spaces();

    let mut ty = match expr.item {
        Expr::Var {
            module_name: "",
            ident,
        } => TypeVar::Identifier(ident),
        _ => TypeVar::Malformed(arena.alloc(expr.item)),
    };

    // Now we re-add the spaces

    if !expr.before.is_empty() {
        ty = TypeVar::SpaceBefore(arena.alloc(ty), expr.before);
    }
    if !expr.after.is_empty() {
        ty = TypeVar::SpaceAfter(arena.alloc(ty), expr.after);
    }

    ty
}

mod ability {
    use parser::absolute_indented_seq;

    use super::*;
    use crate::{
        ast::{AbilityMember, Spaceable, Spaced},
        parser::EAbility,
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

/// Parse the series of "demands" (e.g. similar to methods in a rust trait), for an ability definition.
fn finish_parsing_ability_def_help<'a>(
    call_min_indent: u32,
    name: Loc<&'a str>,
    args: &'a [Loc<TypeVar<'a>>],
    loc_implements: Loc<Implements<'a>>,
    arena: &'a Bump,
    state: State<'a>,
) -> ParseResult<'a, (TypeDef<'a>, Region), EExpr<'a>> {
    let mut demands = Vec::with_capacity_in(2, arena);

    // Parse the first demand. This will determine the indentation level all the
    // other demands must observe.
    let start = state.pos();
    let (_, (demand_indent_level, first_demand), mut state) =
        ability::parse_demand(ability::IndentLevel::PendingMin(call_min_indent))
            .trace("ability_demand")
            .parse(arena, state, call_min_indent)
            .map_err(|(progress, err)| (progress, EExpr::Ability(err, start)))?;
    demands.push(first_demand);

    let demand_indent = ability::IndentLevel::Exact(demand_indent_level);
    let demand_parser = ability::parse_demand(demand_indent).trace("ability_demand");

    loop {
        match demand_parser.parse(arena, state.clone(), call_min_indent) {
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

/// A Stmt is an intermediate representation used only during parsing.
/// It consists of a fragment of code that hasn't been fully stitched together yet.
/// For example, each of the following lines is a Stmt:
/// - `foo bar` (Expr)
/// - `Foo : [A, B, C]` (TypeDef)
/// - `foo = \x -> x + 1` (ValueDef)
#[derive(Debug, Clone, Copy)]
pub enum Stmt<'a> {
    Expr(Expr<'a>),
    TypeDef(TypeDef<'a>),
    ValueDef(ValueDef<'a>),
}

/// Having just parsed an operator, we need to dispatch to the appropriate
/// parsing function based on the operator.
///
/// Note, this function is very similar to `parse_expr_operator`, but it
/// handles additional cases to allow assignments / type annotations / etc.
#[allow(clippy::too_many_arguments)]
fn parse_stmt_operator<'a>(
    arena: &'a Bump,
    state: State<'a>,
    min_indent: u32,
    call_min_indent: u32,
    check_for_arrow: CheckForArrow,
    expr_state: ExprState<'a>,
    loc_op: Loc<OperatorOrDef>,
    initial_state: State<'a>,
) -> ParseResult<'a, Stmt<'a>, EExpr<'a>> {
    let (_, spaces_after_operator, state) =
        loc_space0_e(EExpr::IndentEnd).parse(arena, state, min_indent)?;

    // a `-` is unary if it is preceded by a space and not followed by a space

    let op = loc_op.value;
    let op_start = loc_op.region.start();
    let op_end = loc_op.region.end();
    let new_start = state.pos();

    match op {
        OperatorOrDef::BinOp(BinOp::Minus) if expr_state.end != op_start && op_end == new_start => {
            parse_negated_term(
                arena,
                state,
                min_indent,
                call_min_indent,
                expr_state,
                check_for_arrow,
                initial_state,
                loc_op.with_value(BinOp::Minus),
            )
            .map(|(progress, expr, state)| (progress, Stmt::Expr(expr), state))
        }
        OperatorOrDef::BinOp(op) => parse_after_binop(
            arena,
            state,
            min_indent,
            call_min_indent,
            check_for_arrow,
            true,
            spaces_after_operator.value,
            expr_state,
            loc_op.with_value(op),
        )
        .map(|(progress, expr, state)| (progress, Stmt::Expr(expr), state)),
        OperatorOrDef::Assignment => parse_stmt_assignment(
            arena,
            state,
            call_min_indent,
            expr_state,
            loc_op,
            check_for_arrow,
            spaces_after_operator,
        ),
        OperatorOrDef::AliasOrOpaque(kind) => parse_stmt_alias_or_opaque(
            arena,
            state,
            call_min_indent,
            expr_state,
            loc_op.with_value(kind),
            spaces_after_operator.value,
        ),
    }
}

/// We just parsed an operator. Parse the expression that follows, taking special care
/// that this might be a negated term. (`-x` is a negated term, not a binary operation)
#[allow(clippy::too_many_arguments)]
fn parse_expr_operator<'a>(
    arena: &'a Bump,
    state: State<'a>,
    min_indent: u32,
    call_min_indent: u32,
    check_for_arrow: CheckForArrow,
    check_for_defs: bool,
    expr_state: ExprState<'a>,
    loc_op: Loc<BinOp>,
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
        BinOp::Minus if expr_state.end != op_start && op_end == new_start => parse_negated_term(
            arena,
            state,
            min_indent,
            call_min_indent,
            expr_state,
            check_for_arrow,
            initial_state,
            loc_op,
        ),
        _ => parse_after_binop(
            arena,
            state,
            min_indent,
            call_min_indent,
            check_for_arrow,
            check_for_defs,
            spaces_after_operator,
            expr_state,
            loc_op,
        ),
    }
}

/// Continue parsing terms after we just parsed a binary operator
#[allow(clippy::too_many_arguments)]
fn parse_after_binop<'a>(
    arena: &'a Bump,
    state: State<'a>,
    min_indent: u32,
    call_min_indent: u32,
    check_for_arrow: CheckForArrow,
    check_for_defs: bool,
    spaces_after_operator: &'a [CommentOrNewline],
    mut expr_state: ExprState<'a>,
    loc_op: Loc<BinOp>,
) -> ParseResult<'a, Expr<'a>, EExpr<'a>> {
    match loc_possibly_negative_or_negated_term(check_for_arrow, true, true).parse(
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

                    parse_expr_end(
                        arena,
                        state,
                        min_indent,
                        call_min_indent,
                        check_for_arrow,
                        check_for_defs,
                        expr_state,
                        initial_state,
                    )
                }
            }
        }
        Err((NoProgress, _e)) => {
            return Err((MadeProgress, EExpr::TrailingOperator(state.pos())));
        }
    }
}

/// We just saw the '=' operator of an assignment stmt. Continue parsing from there.
fn parse_stmt_assignment<'a>(
    arena: &'a Bump,
    state: State<'a>,
    call_min_indent: u32,
    expr_state: ExprState<'a>,
    loc_op: Loc<OperatorOrDef>,
    check_for_arrow: CheckForArrow,
    spaces_after_operator: Loc<&'a [CommentOrNewline]>,
) -> ParseResult<'a, Stmt<'a>, EExpr<'a>> {
    let call = expr_state
        .validate_assignment(arena, loc_op, EExpr::ElmStyleFunction)
        .map_err(|fail| (MadeProgress, fail))?;

    let (value_def, state) = {
        match expr_to_pattern_help(arena, &call.value) {
            Ok(good) => {
                let (_, body, state) = parse_block_inner(
                    check_for_arrow,
                    arena,
                    state,
                    call_min_indent,
                    EExpr::IndentEnd,
                    |a, _| a.clone(),
                    spaces_after_operator,
                    !spaces_after_operator.value.is_empty(),
                    false,
                )?;

                let alias =
                    ValueDef::Body(arena.alloc(Loc::at(call.region, good)), arena.alloc(body));

                (alias, state)
            }
            Err(_) => {
                // this `=` likely occurred inline; treat it as an invalid operator
                let fail = EExpr::BadOperator(arena.alloc("="), loc_op.region.start());

                return Err((MadeProgress, fail));
            }
        }
    };

    Ok((MadeProgress, Stmt::ValueDef(value_def), state))
}

/// We just saw a unary negation operator, and now we need to parse the expression.
#[allow(clippy::too_many_arguments)]
fn parse_negated_term<'a>(
    arena: &'a Bump,
    state: State<'a>,
    min_indent: u32,
    call_min_indent: u32,
    mut expr_state: ExprState<'a>,
    check_for_arrow: CheckForArrow,
    initial_state: State<'a>,
    loc_op: Loc<BinOp>,
) -> ParseResult<'a, Expr<'a>, EExpr<'a>> {
    let (_, negated_expr, state) =
        loc_term_or_closure(check_for_arrow).parse(arena, state, min_indent)?;
    let new_end = state.pos();

    let arg = numeric_negate_expression(
        arena,
        initial_state,
        loc_op,
        negated_expr,
        expr_state.spaces_after,
    );

    let initial_state = state.clone();

    let (spaces, state) = match space0_e(EExpr::IndentEnd).parse(arena, state.clone(), min_indent) {
        Err((_, _)) => (&[] as &[_], state),
        Ok((_, spaces, state)) => (spaces, state),
    };

    expr_state.arguments.push(arena.alloc(arg));
    expr_state.spaces_after = spaces;
    expr_state.end = new_end;

    // TODO: this should probably be handled in the caller, not here
    parse_expr_end(
        arena,
        state,
        min_indent,
        call_min_indent,
        check_for_arrow,
        true,
        expr_state,
        initial_state,
    )
}

/// Parse an expression, not allowing `if`/`when`/etc.
/// TODO: this should probably be subsumed into `parse_expr_operator_chain`
#[allow(clippy::too_many_arguments)]
fn parse_expr_end<'a>(
    arena: &'a Bump,
    state: State<'a>,
    min_indent: u32,
    call_min_indent: u32,
    check_for_arrow: CheckForArrow,
    check_for_defs: bool,
    mut expr_state: ExprState<'a>,
    initial_state: State<'a>,
) -> ParseResult<'a, Expr<'a>, EExpr<'a>> {
    let parser = skip_first(
        crate::blankspace::check_indent(EExpr::IndentEnd),
        loc_term_or_closure(check_for_arrow),
    );

    match parser.parse(arena, state.clone(), call_min_indent) {
        Err((MadeProgress, f)) => Err((MadeProgress, f)),
        Ok((_, arg, state)) => parse_apply_arg(
            arena,
            state,
            min_indent,
            call_min_indent,
            expr_state,
            arg,
            check_for_arrow,
            check_for_defs,
        ),
        Err((NoProgress, _)) => {
            let before_op = state.clone();
            // try an operator
            match loc(bin_op(check_for_defs)).parse(arena, state.clone(), call_min_indent) {
                Err((MadeProgress, f)) => Err((MadeProgress, f)),
                Ok((_, loc_op, state)) => {
                    expr_state.consume_spaces(arena);
                    let initial_state = before_op;
                    parse_expr_operator(
                        arena,
                        state,
                        min_indent,
                        call_min_indent,
                        check_for_arrow,
                        check_for_defs,
                        expr_state,
                        loc_op,
                        initial_state,
                    )
                }
                Err((NoProgress, _)) => {
                    let expr = parse_expr_final(expr_state, arena);
                    // roll back space parsing
                    Ok((MadeProgress, expr, initial_state))
                }
            }
        }
    }
}

/// We're part way thru parsing an expression, e.g. `bar foo `.
/// We just tried parsing an argument and determined we couldn't -
/// so we're going to try parsing an operator.
///
/// Note that this looks a lot like `parse_expr_after_apply`, except
/// we handle the additional case of backpassing, which is valid
/// at the statement level but not at the expression level.
fn parse_stmt_after_apply<'a>(
    arena: &'a Bump,
    state: State<'a>,
    min_indent: u32,
    call_min_indent: u32,
    mut expr_state: ExprState<'a>,
    check_for_arrow: CheckForArrow,
    initial_state: State<'a>,
) -> ParseResult<'a, Stmt<'a>, EExpr<'a>> {
    let before_op = state.clone();
    match loc(operator()).parse(arena, state.clone(), call_min_indent) {
        Err((MadeProgress, f)) => Err((MadeProgress, f)),
        Ok((_, loc_op, state)) => {
            expr_state.consume_spaces(arena);
            let initial_state = before_op;
            parse_stmt_operator(
                arena,
                state,
                min_indent,
                call_min_indent,
                check_for_arrow,
                expr_state,
                loc_op,
                initial_state,
            )
        }
        Err((NoProgress, _)) => {
            if check_for_arrow.0 && state.bytes().starts_with(b"->") {
                Err((MadeProgress, EExpr::BadOperator("->", state.pos())))
            } else {
                let expr = parse_expr_final(expr_state, arena);

                // roll back space parsing
                Ok((MadeProgress, Stmt::Expr(expr), initial_state))
            }
        }
    }
}

// #[allow(clippy::too_many_arguments)]
// fn parse_expr_after_apply<'a>(
//     arena: &'a Bump,
//     state: State<'a>,
//     min_indent: u32,
//     call_min_indent: u32,
//     check_for_arrow: CheckForArrow,
//     check_for_defs: bool,
//     mut expr_state: ExprState<'a>,
//     before_op: State<'a>,
//     initial_state: State<'a>,
// ) -> Result<(Progress, Expr<'a>, State<'a>), (Progress, EExpr<'a>)> {
//     match loc(bin_op(check_for_defs)).parse(arena, state.clone(), call_min_indent) {
//         Err((MadeProgress, f)) => Err((MadeProgress, f)),
//         Ok((_, loc_op, state)) => {
//             expr_state.consume_spaces(arena);
//             let initial_state = before_op;
//             parse_expr_operator(
//                 arena,
//                 state,
//                 min_indent,
//                 call_min_indent,
//                 options,
//                 check_for_defs,
//                 expr_state,
//                 loc_op,
//                 initial_state,
//             )
//         }
//         Err((NoProgress, _)) => {
//             let expr = parse_expr_final(expr_state, arena);
//             // roll back space parsing
//             Ok((MadeProgress, expr, initial_state))
//         }
//     }
// }

#[allow(clippy::too_many_arguments)]
fn parse_apply_arg<'a>(
    arena: &'a Bump,
    state: State<'a>,
    min_indent: u32,
    call_min_indent: u32,
    mut expr_state: ExprState<'a>,
    mut arg: Loc<Expr<'a>>,
    check_for_arrow: CheckForArrow,
    check_for_defs: bool,
) -> ParseResult<'a, Expr<'a>, EExpr<'a>> {
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

            parse_expr_end(
                arena,
                state,
                min_indent,
                call_min_indent,
                check_for_arrow,
                check_for_defs,
                expr_state,
                initial_state,
            )
        }
    }
}

fn parse_ability_def<'a>(
    expr_state: ExprState<'a>,
    state: State<'a>,
    arena: &'a Bump,
    implements: Loc<Expr<'a>>,
    call_min_indent: u32,
) -> Result<(TypeDef<'a>, State<'a>), (Progress, EExpr<'a>)> {
    // This is an ability definition, `Ability arg1 ... implements ...`.

    let name = expr_state.expr.map_owned(|e| match e {
        Expr::Tag(name) => name,
        Expr::Apply(
            Loc {
                region: _,
                value: Expr::Tag(name),
            },
            &[],
            _,
        ) => name,
        _ => unreachable!(),
    });

    let mut arguments = Vec::with_capacity_in(expr_state.arguments.len(), arena);
    for argument in expr_state.arguments {
        arguments.push(Loc::at(
            argument.region,
            expr_to_type_var(arena, &argument.value),
        ));
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
    let (_, (type_def, _), state) =
        finish_parsing_ability_def_help(call_min_indent, name, args, implements, arena, state)?;

    Ok((type_def, state))
}

pub fn loc_expr_block<'a>(allow_any_indent: bool) -> impl Parser<'a, Loc<Expr<'a>>, EExpr<'a>> {
    space0_after_e(
        move |arena: &'a Bump, state: State<'a>, min_indent: u32| {
            let check_for_arrow = CheckForArrow(true);

            let (_, loc_first_space, state) =
                loc_space0_e(EExpr::IndentStart).parse(arena, state, min_indent)?;

            parse_block_inner(
                check_for_arrow,
                arena,
                state,
                min_indent,
                EExpr::IndentStart,
                |a, _| a.clone(),
                loc_first_space,
                true,
                allow_any_indent,
            )
        },
        EExpr::IndentEnd,
    )
    .trace("loc_expr_block")
}

pub fn loc_expr<'a>(allow_any_indent: bool) -> impl Parser<'a, Loc<Expr<'a>>, EExpr<'a>> {
    space0_before_e(
        expr_start(CheckForArrow(true), allow_any_indent),
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

    while let Expr::ParensAround(loc_expr) = &expr.item {
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
        Expr::PncApply(loc_val, args) => {
            let region = loc_val.region;
            let value = expr_to_pattern_help(arena, &loc_val.value)?;
            let val_pattern = arena.alloc(Loc { region, value });
            let pattern_args = args.map_items_result(arena, |arg| {
                let region = arg.region;
                let value = expr_to_pattern_help(arena, &arg.value)?;
                Ok(Loc { region, value })
            })?;

            Pattern::PncApply(val_pattern, pattern_args)
        }

        Expr::Try => Pattern::Identifier { ident: "try" },

        Expr::SpaceBefore(..) | Expr::SpaceAfter(..) | Expr::ParensAround(..) => unreachable!(),

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
        | Expr::BinOps { .. }
        | Expr::Defs(_, _)
        | Expr::If { .. }
        | Expr::When(_, _)
        | Expr::Dbg
        | Expr::DbgStmt { .. }
        | Expr::LowLevelDbg(_, _, _)
        | Expr::LowLevelTry(_, _)
        | Expr::Return(_, _)
        | Expr::PrecedenceConflict { .. }
        | Expr::EmptyRecordBuilder(_)
        | Expr::SingleFieldRecordBuilder(_)
        | Expr::OptionalFieldInRecordBuilder(_, _)
        | Expr::RecordUpdate { .. }
        | Expr::RecordUpdater(_)
        | Expr::UnaryOp(_, _)
        | Expr::TrySuffix { .. }
        | Expr::Crash
        | Expr::RecordBuilder { .. } => return Err(()),

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
        AssignedField::IgnoredValue(_, _, _) => return Err(()),
    })
}

pub fn parse_top_level_defs<'a>(
    arena: &'a bumpalo::Bump,
    state: State<'a>,
    output: Defs<'a>,
) -> ParseResult<'a, Defs<'a>, EExpr<'a>> {
    let (_, loc_first_space, state) = loc_space0_e(EExpr::IndentStart).parse(arena, state, 0)?;

    let (_, stmts, state) = parse_stmt_seq(
        arena,
        state,
        |e, _| e.clone(),
        CheckForArrow(true),
        0,
        loc_first_space,
        EExpr::IndentEnd,
    )?;

    let (_, last_space, state) = space0_e(EExpr::IndentStart).parse(arena, state, 0)?;

    let existing_len = output.tags.len();

    let (mut output, last_expr) =
        stmts_to_defs(&stmts, output, false, arena).map_err(|e| (MadeProgress, e))?;

    if let Some(expr) = last_expr {
        return Err((
            MadeProgress,
            EExpr::UnexpectedTopLevelExpr(expr.region.start()),
        ));
    }

    if output.tags.len() > existing_len {
        let after = slice_extend_new(&mut output.spaces, last_space.iter().copied());
        let last = output.tags.len() - 1;
        debug_assert!(output.space_after[last].is_empty() || after.is_empty());
        output.space_after[last] = after;
    }

    Ok((MadeProgress, output, state))
}

// PARSER HELPERS

fn closure_help<'a>(check_for_arrow: CheckForArrow) -> impl Parser<'a, Expr<'a>, EClosure<'a>> {
    one_of!(
        closure_new_syntax_help(),
        closure_old_syntax_help(check_for_arrow),
    )
}

fn closure_new_syntax_help<'a>() -> impl Parser<'a, Expr<'a>, EClosure<'a>> {
    move |arena: &'a Bump, state: State<'a>, min_indent: u32| {
        let parser = map_with_arena(
            indented_seq_skip_first(
                error_on_pizza(byte_indent(b'|', EClosure::Bar), EClosure::Start),
                and(
                    sep_by1_e(
                        byte_indent(b',', EClosure::Comma),
                        space0_around_ee(
                            specialize_err(EClosure::Pattern, closure_param()),
                            EClosure::IndentArg,
                            EClosure::IndentArrow,
                        ),
                        EClosure::Arg,
                    ),
                    skip_first(
                        // Parse the -> which separates params from body
                        byte(b'|', EClosure::Bar),
                        // Parse the body
                        block(
                            CheckForArrow(false),
                            true,
                            EClosure::IndentBody,
                            EClosure::Body,
                        ),
                    ),
                ),
            ),
            |arena: &'a Bump, (params, body)| {
                let params: Vec<'a, Loc<Pattern<'a>>> = params;
                let params: &'a [Loc<Pattern<'a>>] = params.into_bump_slice();
                Expr::Closure(params, arena.alloc(body))
            },
        );
        parser.parse(arena, state, min_indent)
    }
}

fn closure_old_syntax_help<'a>(
    check_for_arrow: CheckForArrow,
) -> impl Parser<'a, Expr<'a>, EClosure<'a>> {
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
                    block(check_for_arrow, true, EClosure::IndentBody, EClosure::Body),
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

fn error_on_pizza<'a, T, E: 'a>(
    p: impl Parser<'a, T, E>,
    f: impl Fn(Position) -> E,
) -> impl Parser<'a, T, E> {
    move |arena: &'a Bump, state: State<'a>, min_indent: u32| {
        if state.bytes().starts_with(b"|>") || state.bytes().starts_with(b"||") {
            Err((NoProgress, f(state.pos())))
        } else {
            p.parse(arena, state, min_indent)
        }
    }
}

mod when {
    use parser::indented_seq_skip_first;

    use super::*;
    use crate::{ast::WhenBranch, blankspace::space0_around_e_no_after_indent_check};

    /// Parser for when expressions.
    pub fn when_expr_help<'a>(
        check_for_arrow: CheckForArrow,
    ) -> impl Parser<'a, Expr<'a>, EWhen<'a>> {
        map_with_arena(
            and(
                indented_seq_skip_first(
                    parser::keyword(keyword::WHEN, EWhen::When),
                    space0_around_e_no_after_indent_check(
                        specialize_err_ref(EWhen::Condition, expr_start(check_for_arrow, true)),
                        EWhen::IndentCondition,
                    )
                ),
                // Note that we allow the `is` to be at any indent level, since this doesn't introduce any
                // ambiguity. The formatter will fix it up.
                //
                // We require that branches are indented relative to the line containing the `is`.
                indented_seq_skip_first(
                    parser::keyword(keyword::IS, EWhen::Is),
                    branches()
                )
            ),
            move |arena: &'a Bump, (loc_condition, branches): (Loc<Expr<'a>>, Vec<'a, &'a WhenBranch<'a>>)| {
                Expr::When(arena.alloc(loc_condition), branches.into_bump_slice())
            }
        ).trace("when")
    }

    fn branches<'a>() -> impl Parser<'a, Vec<'a, &'a WhenBranch<'a>>, EWhen<'a>> {
        move |arena, state: State<'a>, min_indent: u32| {
            let mut branches: Vec<'a, &'a WhenBranch<'a>> = Vec::with_capacity_in(2, arena);

            // 1. Parse the first branch and get its indentation level. (It must be >= min_indent.)
            // 2. Parse the other branches. Their indentation levels must be == the first branch's.

            let (_, ((pattern_indent_level, loc_first_patterns), loc_first_guard), state): (
                _,
                ((_, _), _),
                State<'a>,
            ) = branch_alternatives(None).parse(arena, state, min_indent)?;

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
                        branch_alternatives(Some(pattern_indent_level)),
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
        pattern_indent_level: Option<u32>,
    ) -> impl Parser<'a, ((u32, Vec<'a, Loc<Pattern<'a>>>), Option<Loc<Expr<'a>>>), EWhen<'a>> {
        let check_for_arrow = CheckForArrow(false);

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
                                increment_min_indent(expr_start(check_for_arrow, true))
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

    fn error_on_arrow<'a, T, E: 'a>(f: impl Fn(Position) -> E) -> impl Parser<'a, T, E> {
        move |_, state: State<'a>, _| {
            if state.bytes().starts_with(b"->") {
                Err((MadeProgress, f(state.pos())))
            } else {
                Err((NoProgress, f(state.pos())))
            }
        }
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
            let (spaces, state) = match space0_e(EWhen::IndentPattern).parse(arena, state, 0) {
                Err((MadeProgress, fail)) => return Err((NoProgress, fail)),
                Err((NoProgress, fail)) => return Err((NoProgress, fail)),
                Ok((_progress, spaces, state)) => (spaces, state),
            };

            match pattern_indent_level {
                Some(wanted) if state.column() > wanted => {
                    return error_on_arrow(EWhen::IndentPattern).parse(arena, state, min_indent);
                }
                Some(wanted) if state.column() < wanted => {
                    let indent = wanted - state.column();
                    return Err((NoProgress, EWhen::PatternAlignment(indent, state.pos())));
                }
                _ => {}
            }

            let pattern_indent = min_indent.max(pattern_indent_level.unwrap_or(min_indent));
            // the region is not reliable for the indent column in the case of
            // parentheses around patterns
            let pattern_indent_column = state.column();

            let parser = sep_by1(byte(b'|', EWhen::Bar), branch_single_alternative());

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

    /// Parsing the righthandside of a branch in a when conditional.
    fn branch_result<'a>(indent: u32) -> impl Parser<'a, Loc<Expr<'a>>, EWhen<'a>> {
        move |arena, state, _min_indent| {
            skip_first(
                two_bytes(b'-', b'>', EWhen::Arrow),
                block(
                    CheckForArrow(true),
                    true,
                    EWhen::IndentBranch,
                    EWhen::Branch,
                ),
            )
            .parse(arena, state, indent)
        }
    }
}

fn if_branch<'a>() -> impl Parser<'a, ((Loc<Expr<'a>>, u32), Loc<Expr<'a>>), EIf<'a>> {
    let check_for_arrow = CheckForArrow(true);

    skip_second(
        and(
            and(
                space0_around_ee(
                    specialize_err_ref(EIf::Condition, loc_expr(false)),
                    EIf::IndentCondition,
                    EIf::IndentThenToken,
                )
                .trace("if_condition"),
                skip_second(
                    capture_line_indent(),
                    parser::keyword(keyword::THEN, EIf::Then),
                ),
            ),
            map_with_arena(
                space0_after_e(
                    block(
                        check_for_arrow,
                        false,
                        EIf::IndentThenBranch,
                        EIf::ThenBranch,
                    ),
                    EIf::IndentElseToken,
                ),
                |arena: &'a Bump, block: Loc<Expr<'a>>| match block.value {
                    Expr::SpaceAfter(&Expr::SpaceBefore(x, before), after) => block.with_value(
                        Expr::SpaceBefore(arena.alloc(Expr::SpaceAfter(x, after)), before),
                    ),
                    _ => block,
                },
            ),
        ),
        parser::keyword(keyword::ELSE, EIf::Else),
    )
    .trace("if_branch")
}

fn expect_help<'a>(
    check_for_arrow: CheckForArrow,
    preceding_comment: Region,
) -> impl Parser<'a, Stmt<'a>, EExpect<'a>> {
    move |arena: &'a Bump, state: State<'a>, min_indent| {
        let parse_expect = crate::parser::keyword(crate::keyword::EXPECT, EExpect::Expect);

        let (_, _kw, state) = parse_expect.parse(arena, state, min_indent)?;

        let (_, condition, state) = parse_block(
            check_for_arrow,
            arena,
            state,
            true,
            EExpect::IndentCondition,
            EExpect::Condition,
        )
        .map_err(|(_, f)| (MadeProgress, f))?;

        let vd = ValueDef::Expect {
            condition: arena.alloc(condition),
            preceding_comment,
        };

        Ok((MadeProgress, Stmt::ValueDef(vd), state))
    }
}

fn return_help<'a>(check_for_arrow: CheckForArrow) -> impl Parser<'a, Stmt<'a>, EReturn<'a>> {
    (move |arena: &'a Bump, state: State<'a>, min_indent| {
        let (_, return_kw, state) = loc(parser::keyword(keyword::RETURN, EReturn::Return))
            .parse(arena, state, min_indent)?;

        let (_, return_value, state) = parse_block(
            check_for_arrow,
            arena,
            state,
            true,
            EReturn::IndentReturnValue,
            EReturn::ReturnValue,
        )
        .map_err(|(_, f)| (MadeProgress, f))?;

        let region = Region::span_across(&return_kw.region, &return_value.region);

        let stmt = Stmt::Expr(Expr::Return(
            arena.alloc(Loc::at(region, return_value.value)),
            None,
        ));

        Ok((MadeProgress, stmt, state))
    })
    .trace("return_help")
}

fn dbg_kw<'a>() -> impl Parser<'a, Expr<'a>, EExpect<'a>> {
    (move |arena: &'a Bump, state: State<'a>, min_indent: u32| {
        let (_, _, next_state) =
            parser::keyword(keyword::DBG, EExpect::Dbg).parse(arena, state, min_indent)?;

        Ok((MadeProgress, Expr::Dbg, next_state))
    })
    .trace("dbg_kw")
}

fn try_kw<'a>() -> impl Parser<'a, Expr<'a>, EExpr<'a>> {
    (move |arena: &'a Bump, state: State<'a>, min_indent: u32| {
        let (_, _, next_state) =
            parser::keyword("try", EExpr::Try).parse(arena, state, min_indent)?;

        Ok((MadeProgress, Expr::Try, next_state))
    })
    .trace("try_kw")
}

fn import<'a>() -> impl Parser<'a, ValueDef<'a>, EImport<'a>> {
    skip_second(
        indented_seq_skip_first(
            parser::keyword(keyword::IMPORT, EImport::Import),
            one_of!(import_body(), import_ingested_file_body()),
        ),
        require_newline_or_eof(EImport::EndNewline),
    )
}

fn if_expr_help<'a>(check_for_arrow: CheckForArrow) -> impl Parser<'a, Expr<'a>, EIf<'a>> {
    (move |arena: &'a Bump, state, min_indent| {
        let (_, _, state) = parser::keyword(keyword::IF, EIf::If)
            .trace("if_kw")
            .parse(arena, state, min_indent)?;

        let if_indent = state.line_indent();

        let mut branches = Vec::with_capacity_in(1, arena);

        let mut loop_state = state;

        let (state_final_else, then_indent) = loop {
            let (_, ((cond, then_indent), then_branch), state) = if_branch()
                .parse(arena, loop_state, if_indent)
                .map_err(|(_p, err)| (MadeProgress, err))?;

            branches.push((cond, then_branch));

            // try to parse another `if`
            // NOTE this drops spaces between the `else` and the `if`
            let optional_if = and(
                backtrackable(space0_e(EIf::IndentIf)),
                parser::keyword(keyword::IF, EIf::If),
            );

            match optional_if.parse(arena, state.clone(), min_indent) {
                Err((_, _)) => break (state, then_indent),
                Ok((_, _, state)) => {
                    loop_state = state;
                    continue;
                }
            }
        };

        let has_newline_next = require_newline_or_eof(EExpr::IndentEnd)
            .parse(arena, state_final_else.clone(), min_indent)
            .is_ok();

        let else_indent = state_final_else.line_indent();
        let indented_else = else_indent > then_indent && has_newline_next;

        let min_indent = if indented_else {
            std::cmp::min(if_indent, std::cmp::min(then_indent, else_indent))
        } else {
            else_indent + 1
        };

        let (_, loc_first_space, state_final_else) =
            loc_space0_e(EIf::IndentElseBranch).parse(arena, state_final_else, min_indent)?;

        let allow_defs = !loc_first_space.value.is_empty();

        // use parse_block_inner so we can set min_indent
        let (_, else_branch, state) = parse_block_inner(
            check_for_arrow,
            arena,
            state_final_else,
            min_indent,
            EIf::IndentElseBranch,
            EIf::ElseBranch,
            loc_first_space,
            allow_defs,
            false,
        )
        .map_err(|(_, err)| (MadeProgress, err))?;

        let expr = Expr::If {
            if_thens: branches.into_bump_slice(),
            final_else: arena.alloc(else_branch),
            indented_else,
        };

        Ok((MadeProgress, expr, state))
    })
    .trace("if")
}

/// Parse a block of statements (parser combinator version of `parse_block`)
fn block<'a, E>(
    check_for_arrow: CheckForArrow,
    require_indent: bool,
    indent_problem: fn(Position) -> E,
    wrap_error: fn(&'a EExpr<'a>, Position) -> E,
) -> impl Parser<'a, Loc<Expr<'a>>, E>
where
    E: 'a + SpaceProblem,
{
    (move |arena: &'a Bump, state, _min_indent| {
        parse_block(
            check_for_arrow,
            arena,
            state,
            require_indent,
            indent_problem,
            wrap_error,
        )
    })
    .trace("block")
}

/// Parse a block of statements.
/// For example, the then and else branches of an `if` expression are both blocks.
/// There are two cases here:
/// 1. If there is a preceding newline, then the block must be indented and is allowed to have definitions.
/// 2. If there is no preceding newline, then the block must consist of a single expression (no definitions).
fn parse_block<'a, E>(
    check_for_arrow: CheckForArrow,
    arena: &'a Bump,
    state: State<'a>,
    require_indent: bool,
    indent_problem: fn(Position) -> E,
    wrap_error: fn(&'a EExpr<'a>, Position) -> E,
) -> ParseResult<'a, Loc<Expr<'a>>, E>
where
    E: 'a + SpaceProblem,
{
    let min_indent = if require_indent {
        state.line_indent() + 1
    } else {
        0
    };

    let (_, loc_first_space, state) =
        loc_space0_e(indent_problem).parse(arena, state, min_indent)?;

    let allow_defs = !loc_first_space.value.is_empty();

    parse_block_inner(
        check_for_arrow,
        arena,
        state,
        min_indent,
        indent_problem,
        wrap_error,
        loc_first_space,
        allow_defs,
        false,
    )
}

/// Parse a block of statements, and process that into an Expr.
/// Assumes the caller has already parsed the optional first "space" (newline),
/// and decided whether to allow definitions.
#[allow(clippy::too_many_arguments)]
fn parse_block_inner<'a, E>(
    check_for_arrow: CheckForArrow,
    arena: &'a Bump,
    state: State<'a>,
    min_indent: u32,
    indent_problem: fn(Position) -> E,
    wrap_error: fn(&'a EExpr<'a>, Position) -> E,
    first_space: Loc<&'a [CommentOrNewline<'a>]>,
    allow_defs: bool,
    allow_any_indent: bool,
) -> ParseResult<'a, Loc<Expr<'a>>, E>
where
    E: 'a + SpaceProblem,
{
    if allow_defs {
        let (_, stmts, state) = parse_stmt_seq(
            arena,
            state,
            wrap_error,
            check_for_arrow,
            min_indent,
            Loc::at(first_space.region, &[]),
            indent_problem,
        )?;

        if stmts.is_empty() {
            return Err((
                NoProgress,
                wrap_error(arena.alloc(EExpr::Start(state.pos())), state.pos()),
            ));
        }

        let last_pos = state.pos();

        let loc_expr = stmts_to_expr(&stmts, arena)
            .map_err(|e| (MadeProgress, wrap_error(arena.alloc(e), last_pos)))?;

        let loc_expr = if first_space.value.is_empty() {
            loc_expr
        } else {
            arena
                .alloc(loc_expr.value)
                .with_spaces_before(first_space.value, loc_expr.region)
        };

        Ok((MadeProgress, loc_expr, state))
    } else {
        let (p2, loc_expr, state) =
            specialize_err_ref(wrap_error, expr_start(check_for_arrow, allow_any_indent))
                .parse(arena, state, min_indent)?;

        let loc_expr = if first_space.value.is_empty() {
            loc_expr
        } else {
            arena
                .alloc(loc_expr.value)
                .with_spaces_before(first_space.value, loc_expr.region)
        };

        Ok((p2, loc_expr, state))
    }
}

/// Parse a sequence of statements, which we'll later process into an expression.
/// Statements can include:
/// - assignments
/// - type annotations
/// - expressions
///
/// This function doesn't care about whether the order of those statements makes any sense.
/// e.g. it will happily parse two expressions in a row.
fn parse_stmt_seq<'a, E: SpaceProblem + 'a>(
    arena: &'a Bump,
    mut state: State<'a>,
    wrap_error: fn(&'a EExpr<'a>, Position) -> E,
    check_for_arrow: CheckForArrow,
    min_indent: u32,
    mut last_space: Loc<&'a [CommentOrNewline<'a>]>,
    indent_problem: fn(Position) -> E,
) -> ParseResult<'a, Vec<'a, SpacesBefore<'a, Loc<Stmt<'a>>>>, E> {
    let mut stmts = Vec::new_in(arena);
    let mut state_before_space = state.clone();
    loop {
        if at_terminator(&state) {
            state = state_before_space;
            break;
        }

        let loc_stmt =
            match specialize_err_ref(wrap_error, stmt_start(check_for_arrow, last_space.region))
                .parse(arena, state.clone(), min_indent)
            {
                Ok((_p, s, new_state)) => {
                    state_before_space = new_state.clone();
                    state = new_state;
                    s
                }
                Err((NoProgress, _)) => {
                    if stmts.is_empty() {
                        return Err((
                            NoProgress,
                            wrap_error(arena.alloc(EExpr::Start(state.pos())), state.pos()),
                        ));
                    }

                    state = state_before_space;
                    break;
                }
                Err((MadeProgress, e)) => {
                    return Err((MadeProgress, e));
                }
            };

        stmts.push(SpacesBefore {
            before: last_space.value,
            item: loc_stmt,
        });

        match loc_space0_e(indent_problem).parse(arena, state.clone(), min_indent) {
            Ok((_p, s_loc, new_state)) => {
                if s_loc.value.is_empty() {
                    // require a newline or a terminator after the statement
                    if at_terminator(&new_state) {
                        state = state_before_space;
                        break;
                    }

                    // If this expr might be followed by an arrow (e.g. in a when branch guard),
                    // then we also need to treat that as a terminator.
                    if !check_for_arrow.0 && new_state.bytes().starts_with(b"->") {
                        state = state_before_space;
                        break;
                    }

                    return Err((
                        MadeProgress,
                        wrap_error(arena.alloc(EExpr::BadExprEnd(state.pos())), state.pos()),
                    ));
                }
                last_space = s_loc;
                state = new_state;
            }
            Err(_) => {
                break;
            }
        };
    }
    Ok((MadeProgress, stmts, state))
}

/// Check if the current byte is a terminator for a sequence of statements
fn at_terminator(state: &State<'_>) -> bool {
    matches!(
        state.bytes().first(),
        None | Some(b']' | b'}' | b')' | b',')
    )
}

/// Convert a sequence of statements into a `Expr::Defs` expression
/// (which is itself a Defs struct and final expr)
fn stmts_to_expr<'a>(
    stmts: &[SpacesBefore<'a, Loc<Stmt<'a>>>],
    arena: &'a Bump,
) -> Result<Loc<Expr<'a>>, EExpr<'a>> {
    if stmts.len() > 1 {
        let first_pos = stmts.first().unwrap().item.region.start();
        let last_pos = stmts.last().unwrap().item.region.end();

        let (defs, last_expr) = stmts_to_defs(stmts, Defs::default(), true, arena)?;

        let final_expr = match last_expr {
            Some(e) => e,
            None => return Err(EExpr::DefMissingFinalExpr(last_pos)),
        };

        let region = Region::new(first_pos, last_pos);

        if defs.is_empty() {
            Ok(final_expr)
        } else {
            Ok(Loc::at(
                region,
                Expr::Defs(arena.alloc(defs), arena.alloc(final_expr)),
            ))
        }
    } else {
        let SpacesBefore {
            before: space,
            item: loc_stmt,
        } = *stmts.last().unwrap();
        let expr = match loc_stmt.value {
            Stmt::Expr(e) => {
                if space.is_empty() {
                    e
                } else {
                    arena.alloc(e).before(space)
                }
            }
            Stmt::ValueDef(ValueDef::Dbg { condition, .. }) => {
                // If we parse a `dbg` as the last thing in a series of statements then it's
                // actually an expression.
                Expr::Apply(
                    arena.alloc(Loc {
                        value: Expr::Dbg,
                        region: loc_stmt.region,
                    }),
                    arena.alloc([condition]),
                    CalledVia::Space,
                )
            }
            Stmt::ValueDef(ValueDef::Expect { .. }) => {
                return Err(EExpr::Expect(
                    EExpect::Continuation(
                        arena.alloc(EExpr::IndentEnd(loc_stmt.region.end())),
                        loc_stmt.region.end(),
                    ),
                    loc_stmt.region.start(),
                ));
            }
            Stmt::TypeDef(_) | Stmt::ValueDef(_) => {
                return Err(EExpr::IndentEnd(loc_stmt.region.end()))
            }
        };

        Ok(loc_stmt.with_value(expr))
    }
}

/// Convert a sequence of `Stmt` into a Defs and an optional final expression.
/// Future refactoring opportunity: push this logic directly into where we're
/// parsing the statements.
fn stmts_to_defs<'a>(
    stmts: &[SpacesBefore<'a, Loc<Stmt<'a>>>],
    mut defs: Defs<'a>,
    exprify_dbg: bool,
    arena: &'a Bump,
) -> Result<(Defs<'a>, Option<Loc<Expr<'a>>>), EExpr<'a>> {
    let mut last_expr = None;
    let mut i = 0;
    while i < stmts.len() {
        let sp_stmt = stmts[i];
        match sp_stmt.item.value {
            Stmt::Expr(Expr::Return(return_value, _after_return)) => {
                if i == stmts.len() - 1 {
                    let region = sp_stmt.item.region;
                    last_expr = Some(Loc::at(
                        region,
                        arena
                            .alloc(Expr::Return(return_value, None))
                            .maybe_before(arena, sp_stmt.before),
                    ));
                } else {
                    let region = Region::span_across(
                        &sp_stmt.item.region,
                        &stmts[stmts.len() - 1].item.region,
                    );
                    let rest = stmts_to_expr(&stmts[i + 1..], arena)?;
                    last_expr = Some(Loc::at(
                        region,
                        arena
                            .alloc(Expr::Return(return_value, Some(arena.alloc(rest))))
                            .maybe_before(arena, sp_stmt.before),
                    ));
                }

                // don't re-process the rest of the statements, they got consumed by the early return
                break;
            }
            Stmt::Expr(e) => {
                if i + 1 < stmts.len() {
                    if let Expr::Apply(
                        Loc {
                            value: Expr::Dbg, ..
                        },
                        args,
                        _,
                    ) = e
                    {
                        if let Some((first, extra_args)) = args.split_first() {
                            let rest = stmts_to_expr(&stmts[i + 1..], arena)?;
                            let e = Expr::DbgStmt {
                                first,
                                extra_args,
                                continuation: arena.alloc(rest),
                                pnc_style: false,
                            };

                            let e = if sp_stmt.before.is_empty() {
                                e
                            } else {
                                arena.alloc(e).before(sp_stmt.before)
                            };

                            last_expr = Some(Loc::at(sp_stmt.item.region, e));

                            // don't re-process the rest of the statements; they got consumed by the dbg expr
                            break;
                        } else {
                            defs.push_value_def(
                                ValueDef::Stmt(arena.alloc(Loc::at(sp_stmt.item.region, e))),
                                sp_stmt.item.region,
                                sp_stmt.before,
                                &[],
                            );
                        }
                    } else {
                        defs.push_value_def(
                            ValueDef::Stmt(arena.alloc(Loc::at(sp_stmt.item.region, e))),
                            sp_stmt.item.region,
                            sp_stmt.before,
                            &[],
                        );
                    }
                } else {
                    let e = if sp_stmt.before.is_empty() {
                        e
                    } else {
                        arena.alloc(e).before(sp_stmt.before)
                    };

                    last_expr = Some(sp_stmt.item.with_value(e));
                }
            }
            Stmt::TypeDef(td) => {
                if let (
                    TypeDef::Alias {
                        header,
                        ann: ann_type,
                    },
                    Some((
                        spaces_middle,
                        Stmt::ValueDef(ValueDef::Body(loc_pattern, loc_def_expr)),
                    )),
                ) = (td, stmts.get(i + 1).map(|s| (s.before, s.item.value)))
                {
                    if (spaces_middle.len() <= 1
                        && !ends_with_spaces_conservative(&ann_type.value)
                        && !starts_with_spaces_conservative(&loc_pattern.value))
                        || type_header_equivalent_to_pat(&header, &loc_pattern.value)
                    {
                        // This is a case like
                        //   UserId x : [UserId Int]
                        //   UserId x = UserId 42
                        // We optimistically parsed the first line as an alias; we now turn it
                        // into an annotation.

                        let region = Region::span_across(&loc_pattern.region, &loc_def_expr.region);

                        let value_def = join_alias_to_body(
                            arena,
                            header,
                            ann_type,
                            spaces_middle,
                            loc_pattern,
                            loc_def_expr,
                        );

                        defs.push_value_def(
                            value_def,
                            Region::span_across(&header.name.region, &region),
                            sp_stmt.before,
                            &[],
                        );

                        i += 1;
                    } else {
                        defs.push_type_def(td, sp_stmt.item.region, sp_stmt.before, &[])
                    }
                } else {
                    defs.push_type_def(td, sp_stmt.item.region, sp_stmt.before, &[])
                }
            }
            Stmt::ValueDef(vd) => {
                // NOTE: it shouldn't be necessary to convert ValueDef::Dbg into an expr, but
                // it turns out that ValueDef::Dbg exposes some bugs in the rest of the compiler.
                // In particular, it seems that the solver thinks the dbg expr must be a bool.
                if let ValueDef::Dbg {
                    condition,
                    preceding_comment: _,
                } = vd
                {
                    if exprify_dbg {
                        let e = if i + 1 < stmts.len() {
                            let rest = stmts_to_expr(&stmts[i + 1..], arena)?;
                            Expr::DbgStmt {
                                first: arena.alloc(condition),
                                extra_args: &[],
                                continuation: arena.alloc(rest),
                                pnc_style: false,
                            }
                        } else {
                            Expr::Apply(
                                arena.alloc(Loc {
                                    value: Expr::Dbg,
                                    region: sp_stmt.item.region,
                                }),
                                arena.alloc([condition]),
                                CalledVia::Space,
                            )
                        };

                        let e = if sp_stmt.before.is_empty() {
                            e
                        } else {
                            arena.alloc(e).before(sp_stmt.before)
                        };

                        last_expr = Some(Loc::at(sp_stmt.item.region, e));

                        // don't re-process the rest of the statements; they got consumed by the dbg expr
                        break;
                    }
                }

                if let (
                    ValueDef::Annotation(ann_pattern, ann_type),
                    Some((
                        spaces_middle,
                        Stmt::ValueDef(ValueDef::Body(loc_pattern, loc_def_expr)),
                    )),
                ) = (vd, stmts.get(i + 1).map(|s| (s.before, s.item.value)))
                {
                    if (spaces_middle.len() <= 1
                        && !ends_with_spaces_conservative(&ann_type.value)
                        && !starts_with_spaces_conservative(&loc_pattern.value))
                        || ann_pattern.value.equivalent(&loc_pattern.value)
                    {
                        let region = Region::span_across(&loc_pattern.region, &loc_def_expr.region);

                        let value_def = ValueDef::AnnotatedBody {
                            ann_pattern: arena.alloc(ann_pattern),
                            ann_type: arena.alloc(ann_type),
                            lines_between: spaces_middle,
                            body_pattern: loc_pattern,
                            body_expr: loc_def_expr,
                        };

                        defs.push_value_def(
                            value_def,
                            roc_region::all::Region::span_across(&ann_pattern.region, &region),
                            sp_stmt.before,
                            &[],
                        );
                        i += 1;
                    } else {
                        defs.push_value_def(vd, sp_stmt.item.region, sp_stmt.before, &[])
                    }
                } else {
                    defs.push_value_def(vd, sp_stmt.item.region, sp_stmt.before, &[])
                }
            }
        }

        i += 1;
    }
    Ok((defs, last_expr))
}

fn starts_with_spaces_conservative(value: &Pattern<'_>) -> bool {
    match value {
        Pattern::Identifier { .. }
        | Pattern::QualifiedIdentifier { .. }
        | Pattern::Tag(_)
        | Pattern::NumLiteral(_)
        | Pattern::FloatLiteral(_)
        | Pattern::StrLiteral(_)
        | Pattern::Underscore(_)
        | Pattern::SingleQuote(_)
        | Pattern::Tuple(_)
        | Pattern::List(_)
        | Pattern::NonBase10Literal { .. }
        | Pattern::ListRest(_)
        | Pattern::OpaqueRef(_) => false,
        Pattern::As(left, _) => starts_with_spaces_conservative(&left.value),
        Pattern::Apply(left, _) => starts_with_spaces_conservative(&left.value),
        Pattern::PncApply(left, _) => starts_with_spaces_conservative(&left.value),
        Pattern::RecordDestructure(_) => false,
        Pattern::RequiredField(_, _) | Pattern::OptionalField(_, _) => false,
        Pattern::SpaceBefore(_, _) => true,
        Pattern::SpaceAfter(inner, _) => starts_with_spaces_conservative(inner),
        Pattern::Malformed(_) | Pattern::MalformedIdent(_, _) | Pattern::MalformedExpr(_) => true,
    }
}

fn type_header_equivalent_to_pat<'a>(header: &TypeHeader<'a>, pat: &Pattern<'a>) -> bool {
    match pat {
        Pattern::Apply(func, args) => {
            if !matches!(func.value, Pattern::Tag(tag) if header.name.value == tag) {
                return false;
            }
            if args.len() != header.vars.len() {
                return false;
            }
            for (arg, var) in (*args).iter().zip(header.vars) {
                match (arg.value, var.value) {
                    (Pattern::Identifier { ident: left }, TypeVar::Identifier(right)) => {
                        if left != right {
                            return false;
                        }
                    }
                    _ => return false,
                }
            }
            true
        }
        Pattern::Tag(tag) => header.vars.is_empty() && header.name.value == *tag,
        _ => false,
    }
}

fn ends_with_spaces_conservative(ty: &TypeAnnotation<'_>) -> bool {
    match ty {
        TypeAnnotation::Function(_, _, res) => ends_with_spaces_conservative(&res.value),
        TypeAnnotation::Apply(_, _, args) => args
            .last()
            .map_or(false, |a| ends_with_spaces_conservative(&a.value)),
        TypeAnnotation::As(_, _, type_header) => type_header
            .vars
            .last()
            .map_or(false, |v| type_var_ends_with_spaces_conservative(&v.value)),
        TypeAnnotation::Record { fields: _, ext }
        | TypeAnnotation::Tuple { elems: _, ext }
        | TypeAnnotation::TagUnion { ext, tags: _ } => {
            ext.map_or(false, |e| ends_with_spaces_conservative(&e.value))
        }
        TypeAnnotation::BoundVariable(_) | TypeAnnotation::Inferred | TypeAnnotation::Wildcard => {
            false
        }
        TypeAnnotation::Where(_, clauses) => clauses.last().map_or(false, |c| {
            c.value
                .abilities
                .last()
                .map_or(false, |a| ends_with_spaces_conservative(&a.value))
        }),
        TypeAnnotation::SpaceBefore(inner, _) => ends_with_spaces_conservative(inner),
        TypeAnnotation::SpaceAfter(_, _) => true,
        TypeAnnotation::Malformed(_) => true,
    }
}

fn type_var_ends_with_spaces_conservative(value: &TypeVar<'_>) -> bool {
    match value {
        TypeVar::Identifier(_) => false,
        TypeVar::Malformed(_) => {
            // conservativly assume it might end in a space
            true
        }
        TypeVar::SpaceBefore(inner, _sp) => type_var_ends_with_spaces_conservative(inner),
        TypeVar::SpaceAfter(_inner, _sp) => true,
    }
}

/// Given a type alias and a value definition, join them into a AnnotatedBody
pub fn join_alias_to_body<'a>(
    arena: &'a Bump,
    header: TypeHeader<'a>,
    ann_type: Loc<TypeAnnotation<'a>>,
    spaces_middle: &'a [CommentOrNewline<'a>],
    body_pattern: &'a Loc<Pattern<'a>>,
    body_expr: &'a Loc<Expr<'a>>,
) -> ValueDef<'a> {
    let loc_name = arena.alloc(header.name.map(|x| Pattern::Tag(x)));
    let ann_pattern = Pattern::Apply(loc_name, type_vars_to_patterns(arena, header.vars));

    let vars_region = Region::across_all(header.vars.iter().map(|v| &v.region));
    let region_ann_pattern = Region::span_across(&loc_name.region, &vars_region);
    let loc_ann_pattern = Loc::at(region_ann_pattern, ann_pattern);

    ValueDef::AnnotatedBody {
        ann_pattern: arena.alloc(loc_ann_pattern),
        ann_type: arena.alloc(ann_type),
        lines_between: spaces_middle,
        body_pattern,
        body_expr,
    }
}

fn type_vars_to_patterns<'a>(
    arena: &'a Bump,
    vars: &'a [Loc<TypeVar<'a>>],
) -> &'a [Loc<Pattern<'a>>] {
    let mut result = Vec::with_capacity_in(vars.len(), arena);
    for var in vars {
        let pat = type_var_to_pat(arena, &var.value);
        result.push(Loc::at(var.region, pat));
    }

    result.into_bump_slice()
}

fn type_var_to_pat<'a>(arena: &'a Bump, var: &TypeVar<'a>) -> Pattern<'a> {
    match var {
        TypeVar::Identifier(ident) => Pattern::Identifier { ident },
        TypeVar::Malformed(expr) => match expr_to_pattern_help(arena, expr) {
            Ok(pat) => pat,
            Err(()) => Pattern::MalformedExpr(expr),
        },
        TypeVar::SpaceBefore(inner, sp) => {
            Pattern::SpaceBefore(arena.alloc(type_var_to_pat(arena, inner)), sp)
        }
        TypeVar::SpaceAfter(inner, sp) => {
            Pattern::SpaceAfter(arena.alloc(type_var_to_pat(arena, inner)), sp)
        }
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
/// 5. A reserved keyword (e.g. `if ` or `when `), meaning we should do something else.

fn assign_or_destructure_identifier<'a>() -> impl Parser<'a, Ident<'a>, EExpr<'a>> {
    parse_ident
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
        Ident::RecordUpdaterFunction(string) => Expr::RecordUpdater(string),
        Ident::Malformed(string, problem) => Expr::MalformedIdent(string, problem),
    }
}

fn list_literal_help<'a>() -> impl Parser<'a, Expr<'a>, EList<'a>> {
    map_with_arena(
        collection_trailing_sep_e(
            byte(b'[', EList::Open),
            specialize_err_ref(EList::Expr, loc_expr(true)),
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
    IgnoredValue(Loc<&'a str>, &'a [CommentOrNewline<'a>], &'a Loc<Expr<'a>>),
    LabelOnly(Loc<&'a str>),
    SpaceBefore(&'a RecordField<'a>, &'a [CommentOrNewline<'a>]),
    SpaceAfter(&'a RecordField<'a>, &'a [CommentOrNewline<'a>]),
}

impl<'a> RecordField<'a> {
    fn is_ignored_value(&self) -> bool {
        let mut current = self;

        loop {
            match current {
                RecordField::IgnoredValue(_, _, _) => break true,
                RecordField::SpaceBefore(field, _) | RecordField::SpaceAfter(field, _) => {
                    current = *field;
                }
                _ => break false,
            }
        }
    }

    pub fn to_assigned_field(self, arena: &'a Bump) -> AssignedField<'a, Expr<'a>> {
        use AssignedField::*;

        match self {
            RecordField::RequiredValue(loc_label, spaces, loc_expr) => {
                RequiredValue(loc_label, spaces, loc_expr)
            }

            RecordField::OptionalValue(loc_label, spaces, loc_expr) => {
                OptionalValue(loc_label, spaces, loc_expr)
            }

            RecordField::IgnoredValue(loc_label, spaces, loc_expr) => {
                IgnoredValue(loc_label, spaces, loc_expr)
            }

            RecordField::LabelOnly(loc_label) => LabelOnly(loc_label),

            RecordField::SpaceBefore(field, spaces) => {
                let assigned_field = field.to_assigned_field(arena);

                SpaceBefore(arena.alloc(assigned_field), spaces)
            }

            RecordField::SpaceAfter(field, spaces) => {
                let assigned_field = field.to_assigned_field(arena);

                SpaceAfter(arena.alloc(assigned_field), spaces)
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
        either(
            and(
                specialize_err(|_, pos| ERecord::Field(pos), loc(lowercase_ident())),
                and(
                    spaces(),
                    optional(either(
                        and(byte(b':', ERecord::Colon), record_field_expr()),
                        and(
                            and(
                                byte(b'?', ERecord::QuestionMark),
                                optional(byte(b'?', ERecord::SecondQuestionMark)),
                            ),
                            spaces_before(specialize_err_ref(ERecord::Expr, loc_expr(true))),
                        ),
                    )),
                ),
            ),
            and(
                loc(skip_first(
                    byte(b'_', ERecord::UnderscoreField),
                    optional(specialize_err(
                        |_, pos| ERecord::Field(pos),
                        lowercase_ident(),
                    )),
                )),
                and(
                    spaces(),
                    skip_first(
                        byte(b':', ERecord::Colon),
                        spaces_before(specialize_err_ref(ERecord::Expr, loc_expr(false))),
                    ),
                ),
            ),
        ),
        |arena: &'a bumpalo::Bump, field_data| {
            match field_data {
                Either::First((loc_label, (spaces, opt_loc_val))) => {
                    match opt_loc_val {
                        Some(Either::First((_, loc_val))) => {
                            RequiredValue(loc_label, spaces, arena.alloc(loc_val))
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
                }
                Either::Second((loc_opt_label, (spaces, loc_val))) => {
                    let loc_label = loc_opt_label
                        .map(|opt_label| opt_label.unwrap_or_else(|| arena.alloc_str("")));

                    IgnoredValue(loc_label, spaces, arena.alloc(loc_val))
                }
            }
        },
    )
}

fn record_field_expr<'a>() -> impl Parser<'a, Loc<Expr<'a>>, ERecord<'a>> {
    map_with_arena(
        and(spaces(), specialize_err_ref(ERecord::Expr, loc_expr(false))),
        |arena: &'a bumpalo::Bump, (spaces, loc_expr)| {
            if spaces.is_empty() {
                loc_expr
            } else {
                arena
                    .alloc(loc_expr.value)
                    .with_spaces_before(spaces, loc_expr.region)
            }
        },
    )
}

enum RecordHelpPrefix {
    Update,
    Mapper,
}

fn record_prefix_identifier<'a>() -> impl Parser<'a, Expr<'a>, ERecord<'a>> {
    specialize_err(
        |_, pos| ERecord::Prefix(pos),
        map_with_arena(parse_ident, ident_to_expr),
    )
}

struct RecordHelp<'a> {
    prefix: Option<(Loc<Expr<'a>>, RecordHelpPrefix)>,
    fields: Collection<'a, Loc<RecordField<'a>>>,
}

fn record_help<'a>() -> impl Parser<'a, RecordHelp<'a>, ERecord<'a>> {
    between(
        byte(b'{', ERecord::Open),
        reset_min_indent(record!(RecordHelp {
            // You can optionally have an identifier followed by an '&' to
            // make this a record update, e.g. { Foo.user & username: "blah" }.
            prefix: optional(backtrackable(and(
                // We wrap the ident in an Expr here,
                // so that we have a Spaceable value to work with,
                // and then in canonicalization verify that it's an Expr::Var
                // (and not e.g. an `Expr::Access`) and extract its string.
                spaces_around(loc(record_prefix_identifier())),
                map_with_arena(
                    either(
                        byte(b'&', ERecord::Ampersand),
                        two_bytes(b'<', b'-', ERecord::Arrow),
                    ),
                    |_arena, output| match output {
                        Either::First(()) => RecordHelpPrefix::Update,
                        Either::Second(()) => RecordHelpPrefix::Mapper,
                    }
                )
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
            let expr_result = match record.prefix {
                Some((update, RecordHelpPrefix::Update)) => {
                    record_update_help(arena, update, record.fields)
                }
                Some((mapper, RecordHelpPrefix::Mapper)) => {
                    record_builder_help(arena, mapper, record.fields)
                }
                None => {
                    let special_field_found = record.fields.iter().find_map(|field| {
                        if field.value.is_ignored_value() {
                            Some(Err(EExpr::RecordUpdateIgnoredField(field.region)))
                        } else {
                            None
                        }
                    });

                    special_field_found.unwrap_or_else(|| {
                        let fields = record.fields.map_items(arena, |loc_field| {
                            loc_field.map(|field| field.to_assigned_field(arena))
                        });

                        Ok(Expr::Record(fields))
                    })
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
            AssignedField::IgnoredValue(_, _, _) => {
                Err(EExpr::RecordUpdateIgnoredField(loc_field.region))
            }
            builder_field => Ok(Loc {
                region: loc_field.region,
                value: builder_field,
            }),
        }
    });

    result.map(|fields| Expr::RecordUpdate {
        update: &*arena.alloc(update),
        fields,
    })
}

fn record_builder_help<'a>(
    arena: &'a Bump,
    mapper: Loc<Expr<'a>>,
    fields: Collection<'a, Loc<RecordField<'a>>>,
) -> Result<Expr<'a>, EExpr<'a>> {
    let result = fields.map_items_result(arena, |loc_field| {
        let builder_field = loc_field.value.to_assigned_field(arena);

        Ok(Loc {
            region: loc_field.region,
            value: builder_field,
        })
    });

    result.map(|fields| Expr::RecordBuilder {
        mapper: &*arena.alloc(mapper),
        fields,
    })
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
            Suffix::TrySuffix => Expr::TrySuffix(arena.alloc(value)),
        })
}

fn string_like_literal_help<'a>() -> impl Parser<'a, Expr<'a>, EString<'a>> {
    then(
        crate::string_literal::parse_str_like_literal(),
        |arena, state, progress, lit| match lit {
            StrLikeLiteral::Str(s) => Ok((progress, Expr::Str(s), state)),
            StrLikeLiteral::SingleQuote(s) => {
                // TODO: preserve the original escaping
                Ok((
                    progress,
                    Expr::SingleQuote(s.to_str_in(arena).map_err(|e| (MadeProgress, e))?),
                    state,
                ))
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

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
enum OperatorOrDef {
    BinOp(BinOp),
    Assignment,
    AliasOrOpaque(AliasOrOpaque),
}

fn bin_op<'a>(check_for_defs: bool) -> impl Parser<'a, BinOp, EExpr<'a>> {
    move |_, state: State<'a>, min_indent| {
        let start = state.pos();
        let (_, op, state) = operator_help(EExpr::Start, EExpr::BadOperator, state, min_indent)?;
        let err_progress = if check_for_defs {
            MadeProgress
        } else {
            NoProgress
        };
        match op {
            OperatorOrDef::BinOp(op) => Ok((MadeProgress, op, state)),
            OperatorOrDef::Assignment => Err((err_progress, EExpr::BadOperator("=", start))),
            OperatorOrDef::AliasOrOpaque(AliasOrOpaque::Alias) => {
                Err((err_progress, EExpr::BadOperator(":", start)))
            }
            OperatorOrDef::AliasOrOpaque(AliasOrOpaque::Opaque) => {
                Err((err_progress, EExpr::BadOperator(":=", start)))
            }
        }
    }
}

fn operator<'a>() -> impl Parser<'a, OperatorOrDef, EExpr<'a>> {
    (move |_, state, min_indent| operator_help(EExpr::Start, EExpr::BadOperator, state, min_indent))
        .trace("operator")
}

#[inline(always)]
fn operator_help<'a, F, G, E>(
    to_expectation: F,
    to_error: G,
    mut state: State<'a>,
    min_indent: u32,
) -> ParseResult<'a, OperatorOrDef, E>
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
        "+" => good!(OperatorOrDef::BinOp(BinOp::Plus), 1),
        "-" => {
            // A unary minus must only match if we are at the correct indent level; indent level doesn't
            // matter for the rest of the operators.

            // Note that a unary minus is distinguished by not having a space after it
            let has_whitespace = matches!(
                state.bytes().get(1),
                Some(b' ' | b'#' | b'\n' | b'\r' | b'\t') | None
            );
            if !has_whitespace && state.column() < min_indent {
                return Err((NoProgress, to_expectation(state.pos())));
            }

            good!(OperatorOrDef::BinOp(BinOp::Minus), 1)
        }
        "*" => good!(OperatorOrDef::BinOp(BinOp::Star), 1),
        "/" => good!(OperatorOrDef::BinOp(BinOp::Slash), 1),
        "%" => good!(OperatorOrDef::BinOp(BinOp::Percent), 1),
        "^" => good!(OperatorOrDef::BinOp(BinOp::Caret), 1),
        ">" => good!(OperatorOrDef::BinOp(BinOp::GreaterThan), 1),
        "<" => good!(OperatorOrDef::BinOp(BinOp::LessThan), 1),
        "." => {
            // a `.` makes no progress, so it does not interfere with `.foo` access(or)
            Err((NoProgress, to_error(".", state.pos())))
        }
        "=" => good!(OperatorOrDef::Assignment, 1),
        ":=" => good!(OperatorOrDef::AliasOrOpaque(AliasOrOpaque::Opaque), 2),
        ":" => good!(OperatorOrDef::AliasOrOpaque(AliasOrOpaque::Alias), 1),
        "??" => good!(OperatorOrDef::BinOp(BinOp::DoubleQuestion), 2),
        "|>" => good!(OperatorOrDef::BinOp(BinOp::Pizza), 2),
        "==" => good!(OperatorOrDef::BinOp(BinOp::Equals), 2),
        "!=" => good!(OperatorOrDef::BinOp(BinOp::NotEquals), 2),
        ">=" => good!(OperatorOrDef::BinOp(BinOp::GreaterThanOrEq), 2),
        "<=" => good!(OperatorOrDef::BinOp(BinOp::LessThanOrEq), 2),
        "&&" => good!(OperatorOrDef::BinOp(BinOp::And), 2),
        "||" => good!(OperatorOrDef::BinOp(BinOp::Or), 2),
        "//" => good!(OperatorOrDef::BinOp(BinOp::DoubleSlash), 2),
        "->" => {
            // makes no progress, so it does not interfere with `_ if isGood -> ...`
            Err((NoProgress, to_error("->", state.pos())))
        }
        "!" => Err((NoProgress, to_error("!", state.pos()))),
        "&" => {
            // makes no progress, so it does not interfere with record updaters / `&foo`
            Err((NoProgress, to_error("&", state.pos())))
        }
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
