use crate::ast::{
    is_expr_suffixed, AssignedField, Collection, CommentOrNewline, Defs, Expr, ExtractSpaces,
    Implements, ImportAlias, ImportAsKeyword, ImportExposingKeyword, ImportedModuleName,
    IngestedFileAnnotation, IngestedFileImport, ModuleImport, ModuleImportParams,
    OldRecordBuilderField, Pattern, Spaceable, Spaced, Spaces, SpacesBefore, TryTarget,
    TypeAnnotation, TypeDef, TypeHeader, ValueDef,
};
use crate::blankspace::{
    eat_space, eat_space_check, eat_space_loc_comments, parse_space, space0_around_ee, space0_e,
    with_spaces, with_spaces_after, with_spaces_before,
};
use crate::header::module_name_help;
use crate::ident::{
    integer_ident, lowercase_ident, parse_ident, parse_lowercase_ident, unqualified_ident,
    Accessor, Ident, Suffix,
};
use crate::number_literal::parse_number_base;
use crate::parser::{
    self, and, at_keyword, backtrackable, byte, collection_inner, collection_trailing_sep_e, loc,
    map, optional, skip_first, skip_second, specialize_err, specialize_err_ref, then, EClosure,
    EExpect, EExpr, EIf, EImport, EImportParams, EInParens, EList, EPattern, ERecord, EType, EWhen,
    ParseResult, Parser, SpaceProblem,
};
use crate::pattern::parse_closure_param;
use crate::state::State;
use crate::string_literal::{self, parse_rest_of_str_like, StrLikeLiteral};
use crate::type_annotation::{self, implements_abilities, type_expr};
use crate::{header, keyword};
use bumpalo::collections::Vec;
use bumpalo::Bump;
use roc_collections::soa::Slice;
use roc_error_macros::internal_error;
use roc_module::called_via::{BinOp, CalledVia, UnaryOp};
use roc_region::all::{Loc, Position, Region};

use crate::parser::Progress::{self, *};

pub fn test_parse_expr<'a>(
    min_indent: u32,
    arena: &'a bumpalo::Bump,
    state: State<'a>,
) -> Result<Loc<Expr<'a>>, EExpr<'a>> {
    let (_, spaces_before, state) =
        parse_space(EExpr::IndentStart, arena, state, min_indent).map_err(|(_, f)| f)?;

    let (expr, state) = match loc_expr_block(ExprParseOptions::ALL).parse(arena, state, min_indent)
    {
        Ok((_, out, state)) => (out, state),
        Err((_, f)) => return Err(f),
    };

    let (spaces_after, state) =
        match parse_space(EExpr::IndentEnd, arena, state.clone(), min_indent) {
            Ok((_, spaces_after, state)) => (spaces_after, state),
            Err(_) => (&[] as &[_], state),
        };

    if state.has_reached_end() {
        Ok(with_spaces(arena, spaces_before, expr, spaces_after))
    } else {
        Err(EExpr::BadExprEnd(state.pos()))
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

impl ExprParseOptions {
    pub const ALL: ExprParseOptions = ExprParseOptions {
        accept_multi_backpassing: true,
        check_for_arrow: true,
    };

    pub const NO_BACK_ARROW: ExprParseOptions = ExprParseOptions {
        accept_multi_backpassing: false,
        check_for_arrow: true,
    };
}

fn parse_rest_of_expr_in_parens_etc<'a>(
    start: Position,
    arena: &'a Bump,
    state: State<'a>,
    min_indent: u32,
) -> ParseResult<'a, Loc<Expr<'a>>, EExpr<'a>> {
    let parser = collection_inner(
        specialize_err_ref(
            EInParens::Expr,
            loc_expr_block(ExprParseOptions::NO_BACK_ARROW),
        ),
        Expr::SpaceBefore,
    );

    let (_, elems, state) = parser
        .parse(arena, state.clone(), 0)
        .map_err(|(p, fail)| (p, EExpr::InParens(fail, start)))?;

    if state.bytes().first() != Some(&b')') {
        let fail = EInParens::End(state.pos());
        return Err((MadeProgress, EExpr::InParens(fail, start)));
    }
    let state = state.advance(1);

    if elems.is_empty() {
        let fail = EInParens::Empty(state.pos());
        return Err((NoProgress, EExpr::InParens(fail, start)));
    }

    let mut loc_elems = if elems.len() > 1 {
        Loc::pos(start, state.pos(), Expr::Tuple(elems.ptrify_items(arena)))
    } else {
        // TODO: don't discard comments before/after
        // (stored in the Collection)
        Loc::at(
            elems.items[0].region,
            Expr::ParensAround(&elems.items[0].value),
        )
    };

    let (field_accesses, state) = match parse_record_field_access_chain(arena, state, min_indent) {
        Ok((_, out, state)) => (out, state),
        Err((_, e)) => return Err((MadeProgress, e)),
    };

    // if there are field accesses, include the parentheses in the region
    // otherwise, don't include the parentheses
    if !field_accesses.is_empty() {
        let elems = apply_expr_access_chain(arena, loc_elems.value, field_accesses);
        loc_elems = Loc::pos(start, state.pos(), elems)
    };

    Ok((MadeProgress, loc_elems, state))
}

fn parse_record_field_access_chain<'a>(
    arena: &'a Bump,
    mut state: State<'a>,
    min_indent: u32,
) -> ParseResult<'a, Vec<'a, Suffix<'a>>, EExpr<'a>> {
    let mut fields = Vec::with_capacity_in(1, arena);
    loop {
        let prev_state = state.clone();
        let (next_field, next_state) = match state.bytes().first() {
            Some(b) => match b {
                b'.' => {
                    let state = state.inc();
                    let before_ident = state.pos();
                    match parse_lowercase_ident(state.clone()) {
                        Ok((_, x, state)) => (Suffix::Accessor(Accessor::RecordField(x)), state),
                        Err((NoProgress, _)) => {
                            match integer_ident().parse(arena, state, min_indent) {
                                Ok((_, x, state)) => {
                                    (Suffix::Accessor(Accessor::TupleIndex(x)), state)
                                }
                                Err(_) => return Err((MadeProgress, EExpr::Access(before_ident))),
                            }
                        }
                        Err(_) => return Err((MadeProgress, EExpr::Access(before_ident))),
                    }
                }
                b'!' => (Suffix::TrySuffix(TryTarget::Task), state.inc()),
                b'?' => (Suffix::TrySuffix(TryTarget::Result), state.inc()),
                _ => return Ok((Progress::when(fields.len() != 0), fields, prev_state)),
            },
            _ => return Ok((Progress::when(fields.len() != 0), fields, prev_state)),
        };

        state = next_state;
        fields.push(next_field);
    }
}

/// In some contexts we want to parse the `_` as an expression, so it can then be turned into a
/// pattern later
fn parse_underscore_or_term<'a>(
    options: ExprParseOptions,
    arena: &'a Bump,
    state: State<'a>,
    min_indent: u32,
) -> ParseResult<'a, Loc<Expr<'a>>, EExpr<'a>> {
    let start = state.pos();
    if state.column() < min_indent {
        return Err((NoProgress, EExpr::Start(start)));
    }

    if state.bytes().first() == Some(&b'_') {
        let state = state.inc();
        match parse_lowercase_ident(state.clone()) {
            Ok((_, name, state)) => {
                let expr = Loc::pos(start, state.pos(), Expr::Underscore(name));
                Ok((MadeProgress, expr, state))
            }
            Err((NoProgress, _)) => {
                let expr = Loc::pos(start, state.pos(), Expr::Underscore(""));
                Ok((MadeProgress, expr, state))
            }
            Err(_) => Err((MadeProgress, EExpr::End(start))),
        }
    } else {
        parse_term(options, arena, state, min_indent)
    }
}

fn parse_term<'a>(
    options: ExprParseOptions,
    arena: &'a Bump,
    state: State<'a>,
    min_indent: u32,
) -> ParseResult<'a, Loc<Expr<'a>>, EExpr<'a>> {
    let start = state.pos();
    if let Some(b) = state.bytes().first() {
        match b {
            b'\\' => match parse_rest_of_closure(options, arena, state.inc()) {
                Ok((p, expr, state)) => Ok((p, Loc::pos(start, state.pos(), expr), state)),
                Err((p, fail)) => Err((p, EExpr::Closure(fail, start))),
            },
            b'(' => parse_rest_of_expr_in_parens_etc(start, arena, state.inc(), min_indent),
            b'{' => parse_record_expr(start, arena, state, min_indent),
            b'[' => parse_rest_of_list_expr(start, arena, state.inc()),
            b'"' | b'\'' => {
                let column = state.column();
                match parse_rest_of_str_like(*b == b'\'', column, arena, state.inc(), min_indent) {
                    Ok((p, literal, state)) => {
                        let literal_expr = match literal {
                            StrLikeLiteral::Str(s) => Expr::Str(s),
                            StrLikeLiteral::SingleQuote(s) => Expr::SingleQuote(s.to_str_in(arena)),
                        };
                        Ok((p, Loc::pos(start, state.pos(), literal_expr), state))
                    }
                    Err((p, fail)) => Err((p, EExpr::Str(fail, start))),
                }
            }
            b'0'..=b'9' => match parse_number_base(false, state.bytes(), state) {
                Ok((p, literal, state)) => {
                    let expr = literal_to_expr(literal);
                    Ok((p, Loc::pos(start, state.pos(), expr), state))
                }
                Err((p, fail)) => Err((p, EExpr::Number(fail, start))),
            },
            _ => {
                if at_keyword(keyword::DBG, &state) {
                    let state = state.advance(keyword::DBG.len());
                    let dbg_expr = Loc::pos(start, state.pos(), Expr::Dbg);
                    return Ok((MadeProgress, dbg_expr, state));
                }

                let (_, ident, state) = parse_ident(arena, state, min_indent)?;
                let ident_end = state.pos();

                match parse_record_field_access_chain(arena, state, min_indent) {
                    Ok((_, suffixes, state)) => {
                        let ident = ident_to_expr(arena, ident);
                        let expr = apply_expr_access_chain(arena, ident, suffixes);
                        Ok((MadeProgress, Loc::pos(start, ident_end, expr), state))
                    }
                    Err((_, e)) => Err((MadeProgress, e)),
                }
            }
        }
    } else {
        Err((NoProgress, EExpr::Start(start)))
    }
}

fn parse_if_when_closure<'a>(
    options: ExprParseOptions,
    arena: &'a Bump,
    mut state: State<'a>,
    min_indent: u32,
) -> ParseResult<'a, Loc<Expr<'a>>, EExpr<'a>> {
    let start = state.pos();
    // All closures start with a '\' - e.g. (\x -> x + 1)
    if state.bytes().first() == Some(&b'\\') {
        match parse_rest_of_closure(options, arena, state.inc()) {
            Ok((p, expr, state)) => Ok((p, Loc::pos(start, state.pos(), expr), state)),
            Err((p, fail)) => Err((p, EExpr::Closure(fail, start))),
        }
    } else if at_keyword(keyword::IF, &state) {
        state.advance_mut(keyword::IF.len());
        match parse_rest_of_if_expr(options, arena, state, min_indent) {
            Ok((p, expr, state)) => Ok((p, Loc::pos(start, state.pos(), expr), state)),
            Err((p, err)) => Err((p, EExpr::If(err, start))),
        }
    } else if at_keyword(keyword::WHEN, &state) {
        state.advance_mut(keyword::WHEN.len());
        let indent = state.line_indent();
        match when::parse_rest_of_when_expr(options, arena, state, indent) {
            Ok((p, expr, state)) => Ok((p, Loc::pos(start, state.pos(), expr), state)),
            Err((p, err)) => Err((p, EExpr::When(err, start))),
        }
    } else {
        Err((NoProgress, EExpr::Start(start)))
    }
}

fn parse_negative_or_term<'a>(
    options: ExprParseOptions,
    arena: &'a Bump,
    state: State<'a>,
    min_indent: u32,
) -> ParseResult<'a, Loc<Expr<'a>>, EExpr<'a>> {
    let start = state.pos();
    if let Some(b) = state.bytes().first() {
        match b {
            b'_' => {
                let state = state.inc();
                match parse_lowercase_ident(state.clone()) {
                    Ok((_, name, state)) => {
                        let expr = Loc::pos(start, state.pos(), Expr::Underscore(name));
                        Ok((MadeProgress, expr, state))
                    }
                    Err((NoProgress, _)) => {
                        let expr = Loc::pos(start, state.pos(), Expr::Underscore(""));
                        Ok((MadeProgress, expr, state))
                    }
                    Err(_) => Err((MadeProgress, EExpr::End(start))),
                }
            }
            b'-' if !state
                .bytes()
                .get(1)
                .map(|b| b.is_ascii_whitespace() || *b == b'#')
                .unwrap_or(false) =>
            {
                parse_unary_minus(start, options, arena, state, min_indent)
            }
            b'-' => {
                // drop the minus
                match parse_number_base(true, &state.bytes()[1..], state) {
                    Ok((p, literal, state)) => {
                        let expr = literal_to_expr(literal);
                        Ok((p, Loc::pos(start, state.pos(), expr), state))
                    }
                    Err((MadeProgress, fail)) => Err((MadeProgress, EExpr::Number(fail, start))),
                    Err(_) => {
                        // it may be the case with split arrow `- >` or similar,
                        // so it should not considered as bad number, let's keep parsing until we find the closest error.
                        Err((NoProgress, EExpr::Start(start)))
                    }
                }
            }
            b'0'..=b'9' => {
                let (p, literal, state) = parse_number_base(false, state.bytes(), state)
                    .map_err(|(p, fail)| (p, EExpr::Number(fail, start)))?;
                let expr = literal_to_expr(literal);
                Ok((p, Loc::pos(start, state.pos(), expr), state))
            }
            b'!' => parse_rest_of_logical_not(start, options, arena, state.inc(), min_indent),
            b'(' => parse_rest_of_expr_in_parens_etc(start, arena, state.inc(), min_indent),
            b'{' => parse_record_expr(start, arena, state, min_indent),
            b'[' => parse_rest_of_list_expr(start, arena, state.inc()),
            b'"' | b'\'' => {
                let column = state.column();
                match parse_rest_of_str_like(*b == b'\'', column, arena, state.inc(), min_indent) {
                    Ok((p, literal, state)) => {
                        let literal_expr = match literal {
                            StrLikeLiteral::Str(s) => Expr::Str(s),
                            StrLikeLiteral::SingleQuote(s) => Expr::SingleQuote(s.to_str_in(arena)),
                        };
                        Ok((p, Loc::pos(start, state.pos(), literal_expr), state))
                    }
                    Err((p, fail)) => Err((p, EExpr::Str(fail, start))),
                }
            }
            _ => {
                if at_keyword(keyword::CRASH, &state) {
                    let state = state.advance(keyword::CRASH.len());
                    let expr = Loc::pos(start, state.pos(), Expr::Crash);
                    return Ok((MadeProgress, expr, state));
                }

                if at_keyword(keyword::DBG, &state) {
                    let state = state.advance(keyword::DBG.len());
                    let dbg_expr = Loc::pos(start, state.pos(), Expr::Dbg);
                    return Ok((MadeProgress, dbg_expr, state));
                }

                let (_, ident, state) = parse_ident(arena, state, min_indent)?;
                let ident_end = state.pos();

                match parse_record_field_access_chain(arena, state, min_indent) {
                    Ok((_, suffixes, state)) => {
                        let ident = ident_to_expr(arena, ident);
                        let expr = apply_expr_access_chain(arena, ident, suffixes);
                        Ok((MadeProgress, Loc::pos(start, ident_end, expr), state))
                    }
                    Err((_, e)) => Err((MadeProgress, e)),
                }
            }
        }
    } else {
        Err((NoProgress, EExpr::Start(start)))
    }
}

/// Entry point for parsing an expression.
/// If Ok it always returns MadeProgress
pub(crate) fn parse_expr_start<'a>(
    options: ExprParseOptions,
    arena: &'a Bump,
    state: State<'a>,
    min_indent: u32,
) -> ParseResult<'a, Loc<Expr<'a>>, EExpr<'a>> {
    match parse_if_when_closure(options, arena, state.clone(), min_indent) {
        Err((NoProgress, _)) => {}
        res => return res,
    }

    // Parse a chain of expressions separated by operators. Also handles function application.
    let start = state.pos();
    let call_min_indent = state.line_indent() + 1;

    let (_, term, state) = parse_negative_or_term(options, arena, state, min_indent)?;

    let mut prev_state = state.clone();
    let (spaces_before_op, state) =
        match parse_space(EExpr::IndentEnd, arena, state.clone(), min_indent) {
            Ok((_, spaces_before_op, state)) => (spaces_before_op, state),
            Err(_) => {
                let loc_term = Loc::pos(start, state.pos(), term.value);
                return Ok((MadeProgress, loc_term, state));
            }
        };

    let mut expr_state = ExprState {
        operators: Vec::new_in(arena),
        arguments: Vec::new_in(arena),
        expr: term,
        spaces_after: spaces_before_op,
        end: prev_state.pos(),
    };

    let mut state = state;
    loop {
        match parse_underscore_or_term(options, arena, state.clone(), call_min_indent) {
            Err((MadeProgress, f)) => return Err((MadeProgress, f)),
            Err((NoProgress, _)) => {
                let before_op = state.clone();
                // We're part way thru parsing an expression, e.g. `bar foo `.
                // We just tried parsing an argument and determined we couldn't -
                // so we're going to try parsing an operator.
                let op_res = match parse_bin_op(MadeProgress, state.clone()) {
                    Err((NoProgress, _)) => {
                        // roll back space parsing
                        let expr = parse_expr_final(expr_state, arena);
                        Ok((MadeProgress, expr, prev_state))
                    }
                    Err(err) => Err(err),
                    Ok((_, op, state)) => {
                        let op_start = before_op.pos();
                        let op_end = state.pos();

                        expr_state.consume_spaces(arena);
                        let (_, spaces_after_op, state) =
                            parse_space(EExpr::IndentEnd, arena, state, min_indent)?;

                        // a `-` is unary if it is preceded by a space and not followed by a space
                        let loc_op = Loc::pos(op_start, op_end, op);
                        match op {
                            BinOp::Minus if expr_state.end != op_start && op_end == state.pos() => {
                                parse_negated_term(
                                    arena,
                                    state,
                                    min_indent,
                                    call_min_indent,
                                    expr_state,
                                    options,
                                    before_op,
                                    loc_op,
                                )
                            }
                            _ => parse_after_binop(
                                arena,
                                state,
                                min_indent,
                                call_min_indent,
                                options,
                                true,
                                spaces_after_op,
                                expr_state,
                                loc_op,
                            ),
                        }
                    }
                };
                return op_res.map(|(_, expr, state)| {
                    (MadeProgress, Loc::pos(start, state.pos(), expr), state)
                });
            }
            Ok((_, mut arg, new_state)) => {
                state = new_state;
                prev_state = state.clone();

                if !expr_state.spaces_after.is_empty() {
                    arg = with_spaces_before(arena, arg, expr_state.spaces_after);
                    expr_state.spaces_after = &[];
                }
                expr_state.arguments.push(arena.alloc(arg));
                expr_state.end = state.pos();

                match parse_space(EExpr::IndentEnd, arena, state.clone(), min_indent) {
                    Err(_) => {
                        expr_state.spaces_after = &[];

                        let expr = parse_expr_final(expr_state, arena);
                        let expr = Loc::pos(start, state.pos(), expr);
                        return Ok((MadeProgress, expr, state));
                    }
                    Ok((_, new_spaces, new_state)) => {
                        expr_state.spaces_after = new_spaces;

                        state = new_state;
                    }
                }

                continue;
            }
        }
    }
}

pub fn parse_repl_defs_and_optional_expr<'a>(
    arena: &'a Bump,
    state: State<'a>,
) -> ParseResult<'a, (Defs<'a>, Option<Loc<Expr<'a>>>), EExpr<'a>> {
    let start = state.pos();
    let (spaces_before, state) = match parse_space(EExpr::IndentEnd, arena, state.clone(), 0) {
        Err((NoProgress, _)) => return Ok((NoProgress, (Defs::default(), None), state)),
        Err((MadeProgress, e)) => return Err((MadeProgress, e)),
        Ok((_, sp, state)) => (Loc::pos(start, state.pos(), sp), state),
    };

    let (_, stmts, state) = parse_stmt_seq(
        arena,
        state,
        |e, _| e.clone(),
        ExprParseOptions::ALL,
        0,
        spaces_before,
        EExpr::IndentEnd,
    )?;

    let state = match parse_space(EExpr::IndentEnd, arena, state.clone(), 0) {
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

fn parse_stmt_start<'a>(
    options: ExprParseOptions,
    preceding_comment: Region,
    arena: &'a Bump,
    mut state: State<'a>,
    min_indent: u32,
) -> ParseResult<'a, Loc<Stmt<'a>>, EExpr<'a>> {
    match parse_if_when_closure(options, arena, state.clone(), min_indent) {
        Err((NoProgress, _)) => {}
        Ok((p, loc, state)) => return Ok((p, Loc::at(loc.region, Stmt::Expr(loc.value)), state)),
        Err(err) => return Err(err),
    }

    let start = state.pos();

    if at_keyword(keyword::EXPECT, &state) {
        state.advance_mut(keyword::EXPECT.len());
        parse_rest_of_expect_stmt(false, start, options, preceding_comment, arena, state)
    } else if at_keyword(&keyword::EXPECT_FX, &state) {
        state.advance_mut(keyword::EXPECT_FX.len());
        parse_rest_of_expect_stmt(true, start, options, preceding_comment, arena, state)
    } else if at_keyword(keyword::DBG, &state) {
        state.advance_mut(keyword::DBG.len());
        parse_rest_of_dbg_stmt(start, options, preceding_comment, arena, state)
    } else if at_keyword(keyword::IMPORT, &state) {
        state.advance_mut(keyword::IMPORT.len());
        parse_rest_of_import(start, arena, state, min_indent)
    } else {
        match parse_stmt_operator_chain(options, arena, state, min_indent) {
            Ok((p, stmt, state)) => Ok((p, Loc::pos(start, state.pos(), stmt), state)),
            Err((NoProgress, _)) => Err((NoProgress, EExpr::Start(start))),
            Err(err) => Err(err),
        }
    }
}

fn parse_stmt_operator_chain<'a>(
    options: ExprParseOptions,
    arena: &'a Bump,
    state: State<'a>,
    min_indent: u32,
) -> Result<(Progress, Stmt<'a>, State<'a>), (Progress, EExpr<'a>)> {
    let line_indent = state.line_indent();

    let (_, expr, state) = parse_negative_or_term(options, arena, state, min_indent)?;

    let mut prev_state = state.clone();
    let end = state.pos();

    let (spaces_before_op, state) =
        match parse_space(EExpr::IndentEnd, arena, state.clone(), min_indent) {
            Ok((_, sp, state)) => (sp, state),
            Err(_) => return Ok((MadeProgress, Stmt::Expr(expr.value), state)),
        };

    let mut expr_state = ExprState {
        operators: Vec::new_in(arena),
        arguments: Vec::new_in(arena),
        expr,
        spaces_after: spaces_before_op,
        end,
    };

    let call_min_indent = line_indent + 1;
    let mut state = state;
    loop {
        match parse_underscore_or_term(options, arena, state.clone(), call_min_indent) {
            Err((MadeProgress, f)) => return Err((MadeProgress, f)),
            Ok((
                _,
                implements @ Loc {
                    value:
                        Expr::Var {
                            module_name: "",
                            ident: keyword::IMPLEMENTS,
                            ..
                        },
                    ..
                },
                state,
            )) if matches!(expr_state.expr.value, Expr::Tag(..)) => {
                return parse_ability_def(expr_state, state, arena, implements, call_min_indent)
                    .map(|(td, s)| (MadeProgress, Stmt::TypeDef(td), s));
            }
            Err((NoProgress, _)) => {
                // We're part way thru parsing an expression, e.g. `bar foo `.
                // We just tried parsing an argument and determined we couldn't -
                // so we're going to try parsing an operator.
                //
                // This is very similar to the logic in `expr_operator_chain`, except
                // we handle the additional case of backpassing, which is valid
                // at the statement level but not at the expression level.
                let before_op = state.clone();
                let op_res = match parse_operator(EExpr::Start, EExpr::BadOperator, state.clone()) {
                    Err((MadeProgress, f)) => Err((MadeProgress, f)),
                    Ok((_, op, state)) => {
                        expr_state.consume_spaces(arena);
                        let loc_op = Loc::pos(before_op.pos(), state.pos(), op);
                        parse_stmt_operator(
                            arena,
                            state,
                            min_indent,
                            call_min_indent,
                            options,
                            expr_state,
                            loc_op,
                            before_op,
                        )
                    }
                    Err((NoProgress, _)) => {
                        // try multi-backpassing
                        if options.accept_multi_backpassing && state.bytes().starts_with(b",") {
                            state = state.advance(1);
                            parse_stmt_multi_backpassing(
                                expr_state, arena, state, min_indent, options,
                            )
                        } else if options.check_for_arrow && state.bytes().starts_with(b"->") {
                            Err((MadeProgress, EExpr::BadOperator("->", state.pos())))
                        } else {
                            // roll back space parsing
                            let expr = parse_expr_final(expr_state, arena);
                            Ok((MadeProgress, Stmt::Expr(expr), prev_state))
                        }
                    }
                };
                return op_res;
            }
            Ok((_, mut arg, new_state)) => {
                state = new_state;
                prev_state = state.clone();

                if !expr_state.spaces_after.is_empty() {
                    arg = with_spaces_before(arena, arg, expr_state.spaces_after);
                    expr_state.spaces_after = &[];
                }
                expr_state.arguments.push(arena.alloc(arg));
                expr_state.end = state.pos();

                match parse_space(EExpr::IndentEnd, arena, state.clone(), min_indent) {
                    Err(_) => {
                        expr_state.spaces_after = &[];

                        let expr = parse_expr_final(expr_state, arena);
                        return Ok((MadeProgress, Stmt::Expr(expr), state));
                    }
                    Ok((_, new_spaces, new_state)) => {
                        expr_state.spaces_after = new_spaces;

                        state = new_state;
                    }
                }

                continue;
            }
        }
    }
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
        loc_op: Loc<OperatorOrDef>,
        argument_error: F,
    ) -> Result<Loc<Expr<'a>>, EExpr<'a>>
    where
        F: Fn(Region, Position) -> EExpr<'a>,
    {
        if !self.operators.is_empty() {
            // this `=` or `<-` likely occurred inline; treat it as an invalid operator
            let opchar = match loc_op.value {
                OperatorOrDef::Assignment => "=",
                OperatorOrDef::Backpassing => "<-",
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

fn numeric_negate_expression<'a>(
    arena: &'a Bump,
    state: &State<'a>,
    loc_op: Region,
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
        _ => Expr::UnaryOp(arena.alloc(expr), Loc::at(loc_op, UnaryOp::Negate)),
    };

    let new_loc_expr = Loc::at(region, new_expr);
    with_spaces_before(arena, new_loc_expr, spaces)
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
                        Ok(AssignedField::IgnoredValue(_, _, _)) => Err((
                            MadeProgress,
                            EImportParams::RecordIgnoredFieldFound(loc_field.region),
                        )),
                        Ok(field) => Ok(Loc::at(loc_field.region, field)),
                        Err(FoundApplyValue) => Err((
                            MadeProgress,
                            EImportParams::RecordApplyFound(loc_field.region),
                        )),
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
        annotation: specialize_err(
            EImport::Annotation,
            type_annotation::type_expr(false, false)
        )
    })
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
    let inc_indent = min_indent + 1;

    let expr_region = expr_state.expr.region;
    let (expr, arguments) = expr_state
        .validate_is_type_def(arena, kind)
        .map_err(|fail| (MadeProgress, fail))?;

    let (res, state) = if let Some(tag) = extract_tag_and_spaces(arena, expr.value) {
        let name = tag.item;
        let mut type_arguments = Vec::with_capacity_in(arguments.len(), arena);

        for argument in arguments {
            match expr_to_pattern_help(arena, &argument.value) {
                Ok(good) => {
                    type_arguments.push(Loc::at(argument.region, good));
                }
                Err(()) => {
                    let pos = state.pos();
                    let fail = EExpr::Pattern(arena.alloc(EPattern::NotAPattern(pos)), pos);
                    return Err((MadeProgress, fail));
                }
            }
        }

        match kind.value {
            AliasOrOpaque::Alias => {
                // TODO @check later that here we skip `spaces_after_operator`
                let ann_pos = state.pos();
                let (ann, state) = match type_expr(false, false).parse(arena, state, inc_indent) {
                    Ok((_, ann, state)) => (ann, state),
                    Err((p, fail)) => return Err((p, EExpr::Type(fail, ann_pos))),
                };

                let header = TypeHeader {
                    name: Loc::at(expr.region, name),
                    vars: type_arguments.into_bump_slice(),
                };

                let def = TypeDef::Alias { header, ann };
                (Stmt::TypeDef(def), state)
            }

            AliasOrOpaque::Opaque => {
                // TODO @check later that here we skip `spaces_after_operator`
                let ann_pos = state.pos();
                let (ann, state) = match type_expr(true, true).parse(arena, state, inc_indent) {
                    Ok((_, out, state)) => (out, state),
                    Err((p, fail)) => return Err((p, EExpr::Type(fail, ann_pos))),
                };

                let olds = state.clone();
                let (derived, state) =
                    match eat_space_check(EType::TIndentStart, arena, state, inc_indent, false) {
                        Err(_) => (None, olds),
                        Ok((_, sp, state)) => {
                            match implements_abilities().parse(arena, state, inc_indent) {
                                Err(_) => (None, olds),
                                Ok((_, out, state)) => {
                                    (Some(with_spaces_before(arena, out, sp)), state)
                                }
                            }
                        }
                    };

                let header = TypeHeader {
                    name: Loc::at(expr.region, name),
                    vars: type_arguments.into_bump_slice(),
                };

                let def = TypeDef::Opaque {
                    header,
                    typ: ann,
                    derived,
                };
                (Stmt::TypeDef(def), state)
            }
        }
    } else {
        let call = to_call(arena, arguments, expr);

        match expr_to_pattern_help(arena, &call.value) {
            Ok(pat) => {
                let ann_pos = state.pos();
                match type_expr(false, false).parse(arena, state, inc_indent) {
                    Ok((_, mut type_ann, state)) => {
                        // put the spaces from after the operator in front of the call
                        type_ann = with_spaces_before(arena, type_ann, spaces_after_operator);
                        let value_def = ValueDef::Annotation(Loc::at(expr_region, pat), type_ann);
                        (Stmt::ValueDef(value_def), state)
                    }
                    Err((_, fail)) => return Err((MadeProgress, EExpr::Type(fail, ann_pos))),
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

mod ability {
    use parser::absolute_indented_seq;

    use super::*;
    use crate::{
        ast::{AbilityMember, Spaced},
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
                    specialize_err(EAbility::Type, type_annotation::type_expr(true, false)),
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
                            let progress = Progress::when(!state.has_reached_end());

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
                                    demand.name = with_spaces_before(arena, demand.name, spaces);
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
    args: &'a [Loc<Pattern<'a>>],
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
/// - `foo, bar <- baz` (Backpassing)
/// - `Foo : [A, B, C]` (TypeDef)
/// - `foo = \x -> x + 1` (ValueDef)
///
/// Note in particular that the Backpassing Stmt doesn't make any sense on its own;
/// we need to link it up with the following stmts to make a complete expression.
#[derive(Debug, Clone, Copy)]
pub enum Stmt<'a> {
    Expr(Expr<'a>),
    Backpassing(&'a [Loc<Pattern<'a>>], &'a Loc<Expr<'a>>),
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
    options: ExprParseOptions,
    expr_state: ExprState<'a>,
    loc_op: Loc<OperatorOrDef>,
    initial_state: State<'a>,
) -> ParseResult<'a, Stmt<'a>, EExpr<'a>> {
    let (_, spaces_after_op, state) =
        eat_space_loc_comments(EExpr::IndentEnd, arena, state, min_indent)?;

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
                options,
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
            options,
            true,
            spaces_after_op.value,
            expr_state,
            loc_op.with_value(op),
        )
        .map(|(progress, expr, state)| (progress, Stmt::Expr(expr), state)),
        OperatorOrDef::Assignment => {
            // We just saw the '=' operator of an assignment stmt. Continue parsing from there.
            let call = expr_state
                .validate_assignment_or_backpassing(arena, loc_op, EExpr::ElmStyleFunction)
                .map_err(|fail| (MadeProgress, fail))?;
            let loc = call.region;

            let (value_def, state) = {
                match expr_to_pattern_help(arena, &call.value) {
                    Ok(pattern) => {
                        let (_, body, state) = parse_block_inner(
                            options,
                            arena,
                            state,
                            call_min_indent,
                            EExpr::IndentEnd,
                            |a, _| a.clone(),
                            spaces_after_op,
                            !spaces_after_op.value.is_empty(),
                        )?;

                        let alias =
                            ValueDef::Body(arena.alloc(Loc::at(loc, pattern)), arena.alloc(body));
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
        OperatorOrDef::Backpassing => {
            // Parse the rest of a backpassing statement, after the <- operator
            let expr_region = expr_state.expr.region;
            let call = expr_state
                .validate_assignment_or_backpassing(arena, loc_op, |_, pos| {
                    EExpr::BadOperator("<-", pos)
                })
                .map_err(|fail| (MadeProgress, fail))?;

            let (loc_pattern, loc_body, state) = {
                match expr_to_pattern_help(arena, &call.value) {
                    Ok(pat) => {
                        let (_, mut type_ann, state) =
                            parse_expr_start(options, arena, state, call_min_indent)?;

                        // put the spaces from after the operator in front of the call
                        type_ann = with_spaces_before(&arena, type_ann, spaces_after_op.value);
                        (Loc::at(expr_region, pat), type_ann, state)
                    }
                    Err(_) => {
                        // this `=` likely occurred inline; treat it as an invalid operator
                        let fail = EExpr::BadOperator("=", loc_op.region.start());
                        return Err((MadeProgress, fail));
                    }
                }
            };

            let stmt = Stmt::Backpassing(arena.alloc([loc_pattern]), arena.alloc(loc_body));
            Ok((MadeProgress, stmt, state))
        }
        OperatorOrDef::AliasOrOpaque(kind) => parse_stmt_alias_or_opaque(
            arena,
            state,
            call_min_indent,
            expr_state,
            loc_op.with_value(kind),
            spaces_after_op.value,
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
    options: ExprParseOptions,
    check_for_defs: bool,
    spaces_after_operator: &'a [CommentOrNewline],
    mut expr_state: ExprState<'a>,
    loc_op: Loc<BinOp>,
) -> ParseResult<'a, Expr<'a>, EExpr<'a>> {
    let res: Result<(Progress, Loc<Expr<'_>>, State<'_>), (Progress, EExpr<'_>)> =
        match parse_if_when_closure(options, arena, state.clone(), min_indent) {
            Err((NoProgress, _)) => {
                parse_negative_or_term(options, arena, state.clone(), min_indent)
            }
            res => res,
        };

    let (right_expr, state) = match res {
        Ok((_, expr, state)) => (expr, state),
        Err((NoProgress, _)) => return Err((MadeProgress, EExpr::TrailingOperator(state.pos()))),
        Err(err) => return Err(err),
    };

    // put the spaces from after the operator in front of the new_expr
    let right_expr = with_spaces_before(&arena, right_expr, spaces_after_operator);

    let args = std::mem::replace(&mut expr_state.arguments, Vec::new_in(arena));
    let call = to_call(arena, args, expr_state.expr);
    expr_state.operators.push((call, loc_op));
    expr_state.expr = right_expr;
    expr_state.end = state.pos();

    let initial_state = state.clone();
    match parse_space(EExpr::IndentEnd, arena, state.clone(), min_indent) {
        Err(_) => {
            expr_state.spaces_after = &[];
            let expr = parse_expr_final(expr_state, arena);
            Ok((MadeProgress, expr, state))
        }
        Ok((_, spaces_after, state)) => {
            expr_state.spaces_after = spaces_after;
            parse_expr_end(
                arena,
                state,
                min_indent,
                call_min_indent,
                options,
                check_for_defs,
                expr_state,
                initial_state,
            )
        }
    }
}

/// We just saw a `,` that we think is part of a backpassing statement.
/// Parse the rest of the statement.
fn parse_stmt_multi_backpassing<'a>(
    mut expr_state: ExprState<'a>,
    arena: &'a Bump,
    state: State<'a>,
    min_indent: u32,
    options: ExprParseOptions,
) -> ParseResult<'a, Stmt<'a>, EExpr<'a>> {
    // called after parsing the first , in `a, b <- c` (e.g.)
    let start = state.pos();

    let parser = space0_around_ee(
        crate::pattern::loc_pattern_help(),
        EPattern::Start,
        EPattern::IndentEnd,
    );

    let original_state = state.clone();
    let start_bytes_len = state.bytes().len();

    let (mut patterns, state) = match parser.parse(arena, state, min_indent) {
        Ok((_, first_pat, next_state)) => {
            let mut state = next_state;
            let mut pats = Vec::with_capacity_in(1, arena);
            pats.push(first_pat);

            let result = loop {
                if state.bytes().first() != Some(&b',') {
                    break (pats, state);
                }
                let next_state = state.inc();

                // If the delimiter passed, check the element parser.
                match parser.parse(arena, next_state.clone(), min_indent) {
                    Ok((_, next_pat, next_state)) => {
                        state = next_state;
                        pats.push(next_pat);
                    }
                    Err((_, fail)) => {
                        // If the delimiter parsed, but the following
                        // element did not, that's a fatal error.
                        let progress =
                            Progress::from_lengths(start_bytes_len, next_state.bytes().len());
                        let fail = if let EPattern::IndentEnd(_) = fail {
                            EExpr::UnexpectedComma(start.prev())
                        } else {
                            EExpr::Pattern(arena.alloc(fail), start)
                        };
                        return Err((progress, fail));
                    }
                }
            };
            result
        }
        Err((NoProgress, _)) => (Vec::new_in(arena), original_state),
        Err((p, fail)) => {
            let fail = if let EPattern::IndentEnd(_) = fail {
                EExpr::UnexpectedComma(start.prev())
            } else {
                EExpr::Pattern(arena.alloc(fail), start)
            };
            return Err((p, fail));
        }
    };

    expr_state.consume_spaces(arena);
    let call = to_call(arena, expr_state.arguments, expr_state.expr);

    let pattern = expr_to_pattern_help(arena, &call.value).map_err(|()| {
        (
            MadeProgress,
            EExpr::Pattern(arena.alloc(EPattern::NotAPattern(state.pos())), state.pos()),
        )
    })?;

    let loc_pattern = Loc::at(call.region, pattern);

    patterns.insert(0, loc_pattern);

    let line_indent = state.line_indent();

    if !state.bytes().starts_with(b"<-") {
        return Err((MadeProgress, EExpr::BackpassArrow(state.pos())));
    }
    let state = state.advance(2);

    let min_indent = line_indent + 1;
    let (ps, spaces_before, state) = parse_space(EExpr::IndentEnd, arena, state, min_indent)?;

    let (loc_body, state) = match parse_expr_start(options, arena, state, min_indent) {
        Ok((_, loc_body, state)) => (loc_body, state),
        Err((pe, fail)) => return Err((ps.or(pe), fail)),
    };

    let loc_body = with_spaces_before(arena, loc_body, spaces_before);

    let ret = Stmt::Backpassing(patterns.into_bump_slice(), arena.alloc(loc_body));
    Ok((MadeProgress, ret, state))
}

/// We just saw a unary negation operator, and now we need to parse the expression.
#[allow(clippy::too_many_arguments)]
fn parse_negated_term<'a>(
    arena: &'a Bump,
    state: State<'a>,
    min_indent: u32,
    call_min_indent: u32,
    mut expr_state: ExprState<'a>,
    options: ExprParseOptions,
    initial_state: State<'a>,
    loc_op: Loc<BinOp>,
) -> ParseResult<'a, Expr<'a>, EExpr<'a>> {
    let (_, negated_expr, state) = parse_term(options, arena, state, min_indent)?;

    let arg = numeric_negate_expression(
        arena,
        &initial_state,
        loc_op.region,
        negated_expr,
        expr_state.spaces_after,
    );
    expr_state.arguments.push(arena.alloc(arg));
    expr_state.end = state.pos();

    let initial_state = state.clone();
    let (spaces, state) = match parse_space(EExpr::IndentEnd, arena, state.clone(), min_indent) {
        Ok((_, spaces, state)) => (spaces, state),
        Err(_) => (&[] as &[_], state),
    };
    expr_state.spaces_after = spaces;

    // TODO: this should probably be handled in the caller, not here
    parse_expr_end(
        arena,
        state,
        min_indent,
        call_min_indent,
        options,
        true,
        expr_state,
        initial_state,
    )
}

/// Parse an expression, not allowing `if`/`when`/etc.
/// TODO: this should probably be subsumed into `expr_operator_chain`
/// If Ok always returns MadeProgress
#[allow(clippy::too_many_arguments)]
fn parse_expr_end<'a>(
    arena: &'a Bump,
    state: State<'a>,
    min_indent: u32,
    call_min_indent: u32,
    options: ExprParseOptions,
    check_for_defs: bool,
    mut expr_state: ExprState<'a>,
    initial_state: State<'a>,
) -> ParseResult<'a, Expr<'a>, EExpr<'a>> {
    match parse_underscore_or_term(options, arena, state.clone(), call_min_indent) {
        Err((MadeProgress, f)) => Err((MadeProgress, f)),
        Ok((_, mut arg, state)) => {
            // now that we have `function arg1 ... <spaces> argn`, attach the spaces to the `argn`
            if !expr_state.spaces_after.is_empty() {
                arg = with_spaces_before(arena, arg, expr_state.spaces_after);
                expr_state.spaces_after = &[];
            }
            expr_state.arguments.push(arena.alloc(arg));
            expr_state.end = state.pos();

            let initial_state = state.clone();
            match parse_space(EExpr::IndentEnd, arena, state.clone(), min_indent) {
                Err(_) => {
                    expr_state.spaces_after = &[];
                    let expr = parse_expr_final(expr_state, arena);
                    Ok((MadeProgress, expr, state))
                }
                Ok((_, spaces_after, state)) => {
                    expr_state.spaces_after = spaces_after;
                    parse_expr_end(
                        arena,
                        state,
                        min_indent,
                        call_min_indent,
                        options,
                        check_for_defs,
                        expr_state,
                        initial_state,
                    )
                }
            }
        }
        Err((NoProgress, _)) => {
            // We're part way thru parsing an expression, e.g. `bar foo `.
            // We just tried parsing an argument and determined we couldn't -
            // so we're going to try parsing an operator.
            let before_op = state.clone();
            let err_progress = if check_for_defs {
                MadeProgress
            } else {
                NoProgress
            };
            match parse_bin_op(err_progress, state.clone()) {
                Err((MadeProgress, f)) => Err((MadeProgress, f)),
                Ok((_, op, state)) => {
                    let op_start = before_op.pos();
                    let op_end = state.pos();

                    expr_state.consume_spaces(arena);
                    let (_, spaces_after_operator, state) =
                        parse_space(EExpr::IndentEnd, arena, state, min_indent)?;

                    // a `-` is unary if it is preceded by a space and not followed by a space
                    let loc_op = Loc::pos(op_start, op_end, op);
                    match op {
                        BinOp::Minus if expr_state.end != op_start && op_end == state.pos() => {
                            parse_negated_term(
                                arena,
                                state,
                                min_indent,
                                call_min_indent,
                                expr_state,
                                options,
                                before_op,
                                loc_op,
                            )
                        }
                        _ => parse_after_binop(
                            arena,
                            state,
                            min_indent,
                            call_min_indent,
                            options,
                            check_for_defs,
                            spaces_after_operator,
                            expr_state,
                            loc_op,
                        ),
                    }
                }
                Err((NoProgress, _)) => {
                    // roll back space parsing
                    let expr = parse_expr_final(expr_state, arena);
                    Ok((MadeProgress, expr, initial_state))
                }
            }
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
    let implements = Loc::at(implements.region, Implements::Implements);
    let implements = with_spaces_before(arena, implements, expr_state.spaces_after);

    let args = arguments.into_bump_slice();
    let (_, (type_def, _), state) =
        finish_parsing_ability_def_help(call_min_indent, name, args, implements, arena, state)?;

    Ok((type_def, state))
}

pub fn loc_expr_block<'a>(options: ExprParseOptions) -> impl Parser<'a, Loc<Expr<'a>>, EExpr<'a>> {
    move |arena: &'a Bump, state: State<'a>, min_indent: u32| {
        let start = state.pos();
        let (_, stmts, state) = parse_stmt_seq(
            arena,
            state,
            |fail, _| fail.clone(),
            options,
            min_indent,
            Loc::pos(start, start, &[]),
            EExpr::IndentStart,
        )?;

        let err_pos = state.pos();
        if stmts.is_empty() {
            let fail = arena.alloc(EExpr::Start(err_pos)).clone();
            return Err((NoProgress, fail));
        }

        let expr =
            stmts_to_expr(&stmts, arena).map_err(|e| (MadeProgress, arena.alloc(e).clone()))?;

        match parse_space(EExpr::IndentEnd, arena, state, min_indent) {
            Ok((_, spaces_after, state)) => {
                let expr = with_spaces_after(arena, expr, spaces_after);
                Ok((MadeProgress, expr, state))
            }
            Err((_, fail)) => Err((MadeProgress, fail)),
        }
    }
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
        | Expr::Closure(_, _, _)
        | Expr::Backpassing(_, _, _)
        | Expr::BinOps { .. }
        | Expr::Defs(_, _)
        | Expr::If { .. }
        | Expr::When(_, _)
        | Expr::Expect(_, _)
        | Expr::Dbg
        | Expr::DbgStmt(_, _)
        | Expr::LowLevelDbg(_, _, _)
        | Expr::MalformedClosure
        | Expr::MalformedSuffixed(..)
        | Expr::PrecedenceConflict { .. }
        | Expr::MultipleOldRecordBuilders { .. }
        | Expr::UnappliedOldRecordBuilder { .. }
        | Expr::EmptyRecordBuilder(_)
        | Expr::SingleFieldRecordBuilder(_)
        | Expr::OptionalFieldInRecordBuilder(_, _)
        | Expr::RecordUpdate { .. }
        | Expr::RecordUpdater(_)
        | Expr::UnaryOp(_, _)
        | Expr::TrySuffix { .. }
        | Expr::Crash
        | Expr::OldRecordBuilder(..)
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
        AssignedField::Malformed(string) => Pattern::Malformed(string),
        AssignedField::IgnoredValue(_, _, _) => return Err(()),
    })
}

// todo: @wip inline me
pub fn parse_top_level_defs<'a>(
    arena: &'a bumpalo::Bump,
    state: State<'a>,
    output: Defs<'a>,
) -> ParseResult<'a, Defs<'a>, EExpr<'a>> {
    let (_, loc_first_space, state) = eat_space_loc_comments(EExpr::IndentStart, arena, state, 0)?;

    let (_, stmts, state) = parse_stmt_seq(
        arena,
        state,
        |e, _| e.clone(),
        ExprParseOptions::ALL,
        0,
        loc_first_space,
        EExpr::IndentEnd,
    )?;

    let (_, last_space, state) = parse_space(EExpr::IndentStart, arena, state, 0)?;

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
        let after = Slice::extend_new(&mut output.spaces, last_space.iter().copied());
        let last = output.tags.len() - 1;
        debug_assert!(output.space_after[last].is_empty() || after.is_empty());
        output.space_after[last] = after;
    }

    Ok((MadeProgress, output, state))
}

// todo: @wip is it possible to create a special unique name, like use some symbol at end, that normally rejected by ident names?
const CLOSURE_ARG_BINOP_LEFT: &str = "un";

/// If Ok it always returns MadeProgress
fn parse_rest_of_closure<'a>(
    options: ExprParseOptions,
    arena: &'a Bump,
    state: State<'a>,
) -> ParseResult<'a, Expr<'a>, EClosure<'a>> {
    // After the first token, all other tokens must be indented past the start of the line
    let slash_indent = state.line_indent();
    if slash_indent > state.column() {
        return Err((NoProgress, EClosure::Start(state.pos())));
    }

    // note: @feat closure+binop shortcut
    // todo: @wip Fun feature for expanding:
    // - [X] `\|> f` into the `\p -> p |> f`,
    // - [X] the rest of BinOp's, e.g. `\+ 1` into `\p -> p + 1`,
    // - [@wip] `\.foo.bar + 1` into `\p -> p.foo.bar + 1`
    // - [ ] `\?> Ok _ -> 1, Err _ -> 0` into ???
    //
    let after_slash = state.pos();
    if let Ok((_, binop, state)) = parse_bin_op(MadeProgress, state.clone()) {
        let after_binop = state.pos();

        let param = Pattern::Identifier {
            ident: &CLOSURE_ARG_BINOP_LEFT,
        };
        let loc_param = Loc::pos(after_slash, after_binop, param);
        let mut params = Vec::with_capacity_in(1, arena);
        params.push(loc_param);

        // the closure parameter is the left value of binary operator
        let term = Expr::Var {
            module_name: "",
            ident: CLOSURE_ARG_BINOP_LEFT,
        };
        let loc_term = Loc::pos(after_slash, after_binop, term);

        let expr_state = ExprState {
            operators: Vec::new_in(arena),
            arguments: Vec::new_in(arena),
            expr: loc_term,
            spaces_after: &[],
            end: after_slash,
        };

        let loc_op = Loc::pos(after_slash, after_binop, binop);

        let min_indent = slash_indent + 1;
        let (_, spaces_after_op, state) =
            parse_space(EExpr::IndentEnd, arena, state, min_indent)
                .map_err(|(_, e)| (MadeProgress, EClosure::Body(arena.alloc(e), after_binop)))?;

        let (_, body, state) = parse_after_binop(
            arena,
            state,
            min_indent,
            min_indent,
            options,
            true,
            spaces_after_op,
            expr_state,
            loc_op,
        )
        .map_err(|(_, e)| (MadeProgress, EClosure::Body(arena.alloc(e), after_binop)))?;

        let loc_body = Loc::pos(after_binop, state.pos(), body);

        let short_closure = Expr::Closure(params.into_bump_slice(), arena.alloc(loc_body), true);
        return Ok((MadeProgress, short_closure, state));
    }

    // Once we see the '\', we're committed to parsing this as a closure.
    // It may turn out to be malformed, but it is definitely a closure.
    let min_indent: u32 = slash_indent + 1;

    // Parse the params, params are comma-separated
    let param_pos = state.pos();
    let (spaces_before, state) = match parse_space(EClosure::IndentArg, arena, state, min_indent) {
        Ok((_, out, state)) => (out, state),
        Err((NoProgress, _)) => return Err((MadeProgress, EClosure::Arg(param_pos))),
        Err(err) => return Err(err),
    };

    let param_ident_pos = state.pos();
    let (param, state) = match parse_closure_param(arena, state, min_indent) {
        Ok((_, out, state)) => (out, state),
        Err((NoProgress, _)) => return Err((MadeProgress, EClosure::Arg(param_pos))),
        Err((_, fail)) => return Err((MadeProgress, EClosure::Pattern(fail, param_ident_pos))),
    };

    let (spaces_after, state) = match parse_space(EClosure::IndentArrow, arena, state, min_indent) {
        Ok((_, out, state)) => (out, state),
        Err((NoProgress, _)) => return Err((MadeProgress, EClosure::Arg(param_pos))),
        Err(err) => return Err(err),
    };

    let first_param = with_spaces(arena, spaces_before, param, spaces_after);
    let mut params = Vec::with_capacity_in(1, arena);
    params.push(first_param);

    let mut state = state;
    loop {
        let prev_state = state.clone();
        if state.bytes().first() == Some(&b',') {
            state.advance_mut(1);

            // After delimiter found, parse the parameter
            let param_pos = state.pos();
            let (spaces_before, next_state) =
                match parse_space(EClosure::IndentArg, arena, state, min_indent) {
                    Ok((_, out, state)) => (out, state),
                    Err((NoProgress, _)) => return Err((MadeProgress, EClosure::Arg(param_pos))),
                    Err(err) => return Err(err),
                };

            let param_ident_pos = next_state.pos();
            let (param, next_state) = match parse_closure_param(arena, next_state, min_indent) {
                Ok((_, out, state)) => (out, state),
                Err((NoProgress, _)) => return Err((MadeProgress, EClosure::Arg(param_pos))),
                Err((_, fail)) => {
                    return Err((MadeProgress, EClosure::Pattern(fail, param_ident_pos)))
                }
            };

            let (spaces_after, next_state) =
                match parse_space(EClosure::IndentArrow, arena, next_state, min_indent) {
                    Ok((_, out, state)) => (out, state),
                    Err((NoProgress, _)) => return Err((MadeProgress, EClosure::Arg(param_pos))),
                    Err(err) => return Err(err),
                };

            let next_param = with_spaces(arena, spaces_before, param, spaces_after);
            params.push(next_param);
            state = next_state;
        } else {
            // Successfully completed the loop if no more delimiters found, restoring the previous state
            state = prev_state;
            break;
        }
    }

    // Parse the arrow which separates params from body, only then parse the body
    if !state.bytes().starts_with(b"->") {
        return Err((MadeProgress, EClosure::Arrow(state.pos())));
    }
    state.advance_mut(2);

    let body_indent = state.line_indent() + 1;
    let (_, first_space, state) =
        eat_space_loc_comments(EClosure::IndentBody, arena, state, body_indent)
            .map_err(|(_, fail)| (MadeProgress, fail))?;

    let (body, state) = if first_space.value.is_empty() {
        let err_pos = state.pos();
        let (_, body, state) = parse_expr_start(options, arena, state, min_indent)
            .map_err(|(_, e)| (MadeProgress, EClosure::Body(arena.alloc(e), err_pos)))?;
        (body, state)
    } else {
        let (_, stmts, state) = parse_stmt_seq(
            arena,
            state,
            EClosure::Body,
            options,
            min_indent,
            Loc::at(first_space.region, &[]),
            EClosure::IndentBody,
        )?;

        let err_pos = state.pos();
        if stmts.is_empty() {
            let fail = EClosure::Body(arena.alloc(EExpr::Start(err_pos)), err_pos);
            return Err((MadeProgress, fail));
        }

        let body = stmts_to_expr(&stmts, arena)
            .map_err(|e| (MadeProgress, EClosure::Body(arena.alloc(e), err_pos)))?;
        let body = with_spaces_before(arena, body, first_space.value);
        (body, state)
    };

    let closure = Expr::Closure(params.into_bump_slice(), arena.alloc(body), false);
    Ok((MadeProgress, closure, state))
}

mod when {
    use super::*;
    use crate::{
        ast::WhenBranch,
        blankspace::{eat_space, with_spaces, with_spaces_before},
    };

    /// If Ok it always returns MadeProgress
    pub fn parse_rest_of_when_expr<'a>(
        options: ExprParseOptions,
        arena: &'a Bump,
        state: State<'a>,
        min_indent: u32,
    ) -> ParseResult<'a, Expr<'a>, EWhen<'a>> {
        let (_, spaces_before, state) =
            parse_space(EWhen::IndentCondition, arena, state, min_indent)
                .map_err(|(_, fail)| (MadeProgress, fail))?;

        let at_cond = state.pos();
        let (_, cond, state) = parse_expr_start(options, arena, state, min_indent)
            .map_err(|(_, fail)| (MadeProgress, EWhen::Condition(arena.alloc(fail), at_cond)))?;

        let (_, (spaces_after, _), state) = eat_space(arena, state, true)?;

        if !at_keyword(keyword::IS, &state) {
            return Err((MadeProgress, EWhen::Is(state.pos())));
        }

        // Note that we allow the `is` to be at any indent level, since this doesn't introduce any
        // ambiguity. The formatter will fix it up.
        // We require that branches are indented relative to the line containing the `is`.
        let branch_indent = state.line_indent() + 1;
        let state = state.advance(keyword::IS.len());

        // 1. Parse the first branch and get its indentation level (it must be >= branch_indent).
        // 2. Parse the other branches. Their indentation levels must be == the first branch's.
        let (_, ((pattern_indent, first_patterns), guard), state) =
            parse_branch_alternatives(options, None, arena, state, branch_indent)
                .map_err(|(_, fail)| (MadeProgress, fail))?;

        // Parse the first "->" and the expression after it.
        let (_, value, mut state) = parse_branch_result(arena, state)?;

        // Record this as the first branch, then optionally parse additional branches.
        let mut branches: Vec<'a, &'a WhenBranch<'a>> = Vec::with_capacity_in(2, arena);
        branches.push(arena.alloc(WhenBranch {
            patterns: first_patterns.into_bump_slice(),
            value,
            guard,
        }));

        while !state.bytes().is_empty() {
            match parse_branch_alternatives(
                options,
                Some(pattern_indent),
                arena,
                state.clone(),
                branch_indent,
            ) {
                Ok((_, ((indent_column, patterns), guard), m_state)) => {
                    if pattern_indent == indent_column {
                        let (_, value, next_state) = parse_branch_result(arena, m_state)?;

                        let branch = WhenBranch {
                            patterns: patterns.into_bump_slice(),
                            value,
                            guard,
                        };
                        branches.push(arena.alloc(branch));
                        state = next_state;
                    } else {
                        let indent = pattern_indent - indent_column;
                        let fail = EWhen::PatternAlignment(indent, m_state.pos());
                        return Err((MadeProgress, fail));
                    }
                }
                Err((NoProgress, _)) => break,
                Err(err) => return Err(err),
            }
        }

        let cond = with_spaces(arena, spaces_before, cond, spaces_after);
        let when = Expr::When(arena.alloc(cond), branches.into_bump_slice());
        Ok((MadeProgress, when, state))
    }

    /// Parsing alternative patterns in `when` branches.
    fn parse_branch_alternatives<'a>(
        options: ExprParseOptions,
        pattern_indent: Option<u32>,
        arena: &'a Bump,
        state: State<'a>,
        min_indent: u32,
    ) -> ParseResult<'a, ((u32, Vec<'a, Loc<Pattern<'a>>>), Option<Loc<Expr<'a>>>), EWhen<'a>> {
        let options = ExprParseOptions {
            check_for_arrow: false,
            ..options
        };

        // put no restrictions on the indent after the spaces; we'll check it manually
        let (_, indent_spaces, state) = space0_e(EWhen::IndentPattern).parse(arena, state, 0)?;

        // the region is not reliable for the indent column in the case of
        // parentheses around patterns
        let pattern_column = state.column();

        if let Some(wanted) = pattern_indent {
            if pattern_column > wanted {
                let err_progress = if state.bytes().starts_with(b"->") {
                    MadeProgress
                } else {
                    NoProgress
                };
                return Err((err_progress, EWhen::IndentPattern(state.pos())));
            }
            if pattern_column < wanted {
                let indent = wanted - pattern_column;
                return Err((NoProgress, EWhen::PatternAlignment(indent, state.pos())));
            }
        }

        let pattern_indent = min_indent.max(pattern_indent.unwrap_or(min_indent));

        let (p1, spaces_before, state) =
            parse_space(EWhen::IndentPattern, arena, state, pattern_indent)
                .map_err(|(_, fail)| (NoProgress, fail))?;

        let pattern_pos = state.pos();
        let (_, pattern, state) = crate::pattern::loc_pattern_help()
            .parse(arena, state, pattern_indent)
            .map_err(|(p2, fail)| (p1.or(p2), EWhen::Pattern(fail, pattern_pos)))?;

        let (_, spaces_after, mut state) =
            parse_space(EWhen::IndentPattern, arena, state, pattern_indent)
                .map_err(|(_, fail)| (MadeProgress, fail))?;

        let first_pattern = with_spaces(arena, spaces_before, pattern, spaces_after);
        let mut patterns = Vec::with_capacity_in(1, arena);
        patterns.push(first_pattern);

        loop {
            let prev_state = state.clone();
            if state.bytes().first() == Some(&b'|') {
                state.advance_mut(1);

                let (_, spaces_before, next_state) =
                    parse_space(EWhen::IndentPattern, arena, state, pattern_indent)
                        .map_err(|(_, fail)| (MadeProgress, fail))?;

                let pattern_pos = next_state.pos();
                let (_, pat, next_state) = crate::pattern::loc_pattern_help()
                    .parse(arena, next_state, pattern_indent)
                    .map_err(|(_, fail)| (MadeProgress, EWhen::Pattern(fail, pattern_pos)))?;

                let (_, spaces_after, next_state) =
                    parse_space(EWhen::IndentPattern, arena, next_state, pattern_indent)
                        .map_err(|(_, fail)| (MadeProgress, fail))?;

                let pattern = with_spaces(arena, spaces_before, pat, spaces_after);
                state = next_state;
                patterns.push(pattern);
            } else {
                state = prev_state;
                break;
            }
        }

        // tag spaces onto the first parsed pattern
        if let Some(first) = patterns.get_mut(0) {
            *first = with_spaces_before(arena, *first, indent_spaces);
        }

        let column_patterns = (pattern_column, patterns);
        let original_state = state.clone();

        if !at_keyword(keyword::IF, &state) {
            return Ok((MadeProgress, (column_patterns, None), original_state));
        }
        state.advance_mut(keyword::IF.len());

        // TODO we should require space before the expression but not after
        let (_, spaces_before, state) = space0_e(EWhen::IndentIfGuard)
            .parse(arena, state, min_indent)
            .map_err(|(_, fail)| (MadeProgress, fail))?;

        let guard_pos = state.pos();
        let (_, guard, state) = parse_expr_start(options, arena, state, min_indent + 1)
            .map_err(|(_, fail)| (MadeProgress, EWhen::IfGuard(arena.alloc(fail), guard_pos)))?;

        let (_, spaces_after, state) = space0_e(EWhen::IndentArrow)
            .parse(arena, state, min_indent)
            .map_err(|(_, fail)| (MadeProgress, fail))?;

        let guard = with_spaces(arena, spaces_before, guard, spaces_after);
        Ok((MadeProgress, (column_patterns, Some(guard)), state))
    }

    /// Parsing the righthandside of a branch in a when conditional.
    /// Always makes progress because called in the middle of parsing when and does not make sense alone
    fn parse_branch_result<'a>(
        arena: &'a Bump,
        state: State<'a>,
    ) -> ParseResult<'a, Loc<Expr<'a>>, EWhen<'a>> {
        if !state.bytes().starts_with(b"->") {
            return Err((MadeProgress, EWhen::Arrow(state.pos())));
        }
        let state = state.advance(2);

        match parse_block(
            ExprParseOptions::ALL,
            arena,
            state,
            true,
            EWhen::IndentBranch,
            EWhen::Branch,
        ) {
            Ok((_, value, state)) => Ok((MadeProgress, value, state)),
            Err((_, fail)) => Err((MadeProgress, fail)),
        }
    }
}

fn parse_rest_of_expect_stmt<'a>(
    is_fx: bool,
    start: Position,
    options: ExprParseOptions,
    preceding_comment: Region,
    arena: &'a Bump,
    state: State<'a>,
) -> ParseResult<'a, Loc<Stmt<'a>>, EExpr<'a>> {
    
    match parse_block(
        options,
        arena,
        state,
        true,
        EExpect::IndentCondition,
        EExpect::Condition,
    ) {
        Ok((_, condition, state)) => {
            let vd = if !is_fx {
                ValueDef::Expect {
                    condition: arena.alloc(condition),
                    preceding_comment,
                }
            } else {
                ValueDef::ExpectFx {
                    condition: arena.alloc(condition),
                    preceding_comment,
                }
            };
            let stmt = Loc::pos(start, state.pos(), Stmt::ValueDef(vd));
            Ok((MadeProgress, stmt, state))
        }
        Err((_, fail)) => Err((MadeProgress, EExpr::Expect(fail, start))),
    }
}

fn parse_rest_of_dbg_stmt<'a>(
    start: Position,
    options: ExprParseOptions,
    preceding_comment: Region,
    arena: &'a Bump,
    state: State<'a>,
) -> ParseResult<'a, Loc<Stmt<'a>>, EExpr<'a>> {
    match parse_block(
        options,
        arena,
        state,
        true,
        EExpect::IndentCondition,
        EExpect::Condition,
    ) {
        Ok((_, condition, state)) => {
            let vd = ValueDef::Dbg {
                condition: arena.alloc(condition),
                preceding_comment,
            };
            let stmt = Loc::pos(start, state.pos(), Stmt::ValueDef(vd));
            Ok((MadeProgress, stmt, state))
        }
        Err((_, fail)) => Err((MadeProgress, EExpr::Dbg(fail, start))),
    }
}

fn parse_rest_of_import<'a>(
    start: Position,
    arena: &'a Bump,
    state: State<'a>,
    min_indent: u32,
) -> ParseResult<'a, Loc<Stmt<'a>>, EExpr<'a>> {
    let import_body = record!(ModuleImport {
        before_name: space0_e(EImport::IndentStart),
        name: loc(imported_module_name()),
        params: optional(specialize_err(EImport::Params, import_params())),
        alias: optional(import_as()),
        exposed: optional(import_exposing())
    });

    let (vd, state) = match import_body.parse(arena, state.clone(), min_indent + 1) {
        Ok((_, vd, state)) => (ValueDef::ModuleImport(vd), state),
        Err((MadeProgress, fail)) => return Err((MadeProgress, EExpr::Import(fail, start))),
        Err(_) => {
            let import_ingested_file_body = record!(IngestedFileImport {
                before_path: space0_e(EImport::IndentStart),
                path: loc(specialize_err(
                    |_, pos| EImport::IngestedPath(pos),
                    string_literal::parse_str_literal()
                )),
                name: import_ingested_file_as(),
                annotation: optional(import_ingested_file_annotation())
            });

            match import_ingested_file_body.parse(arena, state.clone(), min_indent + 1) {
                Ok((_, vd, state)) => (ValueDef::IngestedFileImport(vd), state),
                Err((_, fail)) => return Err((MadeProgress, EExpr::Import(fail, start))),
            }
        }
    };

    let has_reached_new_line_or_eof = state.has_reached_end();
    let (_, spaces_after, _) = parse_space(EImport::EndNewline, arena, state.clone(), min_indent)
        .map_err(|(_, fail)| (MadeProgress, EExpr::Import(fail, start)))?;

    if !has_reached_new_line_or_eof && spaces_after.is_empty() {
        return Err((
            MadeProgress,
            EExpr::Import(EImport::EndNewline(state.pos()), start),
        ));
    }

    let stmt = Loc::pos(start, state.pos(), Stmt::ValueDef(vd));
    Ok((MadeProgress, stmt, state))
}

/// If Ok it always returns MadeProgress
fn parse_rest_of_if_expr<'a>(
    options: ExprParseOptions,
    arena: &'a Bump,
    state: State<'a>,
    min_indent: u32,
) -> ParseResult<'a, Expr<'a>, EIf<'a>> {
    let if_indent = state.line_indent();

    let mut branches = Vec::with_capacity_in(1, arena);
    let mut loop_state = state;

    let state_final_else = loop {
        let (_, spaces_before_cond, state) =
            parse_space(EIf::IndentCondition, arena, loop_state.clone(), min_indent)?;

        let cond_pos = state.pos();
        let (cond, state) = match parse_expr_start(ExprParseOptions::ALL, arena, state, min_indent)
        {
            Ok((_, cond, state)) => (cond, state),
            Err((p, fail)) => return Err((p, EIf::Condition(arena.alloc(fail), cond_pos))),
        };

        let (spaces_after_cond, mut state) =
            match parse_space(EIf::IndentThenToken, arena, state.clone(), min_indent) {
                Ok((_, out, state)) => (out, state),
                Err((_, fail)) => return Err((MadeProgress, fail)),
            };

        if !at_keyword(keyword::THEN, &state) {
            return Err((MadeProgress, EIf::Then(state.pos())));
        }
        state.advance_mut(keyword::THEN.len());

        let cond = with_spaces(arena, spaces_before_cond, cond, spaces_after_cond);

        let (_, then_expr, state) = parse_block(
            ExprParseOptions::ALL,
            arena,
            state,
            false,
            EIf::IndentThenBranch,
            EIf::ThenBranch,
        )
        .map_err(|(_, fail)| (MadeProgress, fail))?;

        let (_, spaces_after_then, state) =
            parse_space(EIf::IndentElseToken, arena, state, min_indent)
                .map_err(|(_, fail)| (MadeProgress, fail))?;

        let then_expr = if spaces_after_then.is_empty() {
            then_expr
        } else {
            let expr = if let Expr::SpaceBefore(x, before) = then_expr.value {
                Expr::SpaceBefore(arena.alloc(Expr::SpaceAfter(x, spaces_after_then)), before)
            } else {
                Expr::SpaceAfter(arena.alloc(then_expr.value), spaces_after_then)
            };
            Loc::at(then_expr.region, expr)
        };

        if !at_keyword(keyword::ELSE, &state) {
            return Err((MadeProgress, EIf::Else(state.pos())));
        }
        let state = state.advance(keyword::ELSE.len());

        branches.push((cond, then_expr));

        // try to parse another `if`
        // NOTE this drops spaces between the `else` and the `if`
        if let Ok((_, _, state)) = parse_space(EIf::IndentIf, arena, state.clone(), min_indent) {
            if at_keyword(keyword::IF, &state) {
                loop_state = state.advance(keyword::IF.len());
                continue;
            }
        }
        break state;
    };

    let else_indent = state_final_else.line_indent();
    let indented_else = else_indent > if_indent;

    let min_indent = if !indented_else {
        else_indent + 1
    } else {
        if_indent
    };

    let (_, loc_first_space, state_final_else) =
        eat_space_loc_comments(EIf::IndentElseBranch, arena, state_final_else, min_indent)?;

    let allow_defs = !loc_first_space.value.is_empty();

    // use parse_block_inner so we can set min_indent
    let (_, else_branch, state) = parse_block_inner(
        options,
        arena,
        state_final_else,
        min_indent,
        EIf::IndentElseBranch,
        EIf::ElseBranch,
        loc_first_space,
        allow_defs,
    )?;

    let expr = Expr::If {
        if_thens: branches.into_bump_slice(),
        final_else: arena.alloc(else_branch),
        indented_else,
    };

    Ok((MadeProgress, expr, state))
}

/// Parse a block of statements.
/// For example, the then and else branches of an `if` expression are both blocks.
/// There are two cases here:
/// 1. If there is a preceding newline, then the block must be indented and is allowed to have definitions.
/// 2. If there is no preceding newline, then the block must consist of a single expression (no definitions).
fn parse_block<'a, E>(
    options: ExprParseOptions,
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
        eat_space_loc_comments(indent_problem, arena, state, min_indent)?;

    let allow_defs = !loc_first_space.value.is_empty();
    parse_block_inner(
        options,
        arena,
        state,
        min_indent,
        indent_problem,
        wrap_error,
        loc_first_space,
        allow_defs,
    )
}

/// Parse a block of statements, and process that into an Expr.
/// Assumes the caller has already parsed the optional first "space" (newline),
/// and decided whether to allow definitions.
#[allow(clippy::too_many_arguments)]
fn parse_block_inner<'a, E>(
    options: ExprParseOptions,
    arena: &'a Bump,
    state: State<'a>,
    min_indent: u32,
    indent_problem: fn(Position) -> E,
    wrap_error: fn(&'a EExpr<'a>, Position) -> E,
    first_space: Loc<&'a [CommentOrNewline<'a>]>,
    allow_defs: bool,
) -> ParseResult<'a, Loc<Expr<'a>>, E>
where
    E: 'a + SpaceProblem,
{
    if allow_defs {
        let (_, stmts, state) = parse_stmt_seq(
            arena,
            state,
            wrap_error,
            options,
            min_indent,
            Loc::at(first_space.region, &[]),
            indent_problem,
        )?;

        let last_pos = state.pos();
        if stmts.is_empty() {
            let fail = wrap_error(arena.alloc(EExpr::Start(last_pos)), last_pos);
            return Err((NoProgress, fail));
        }

        match stmts_to_expr(&stmts, arena) {
            Ok(expr) => {
                let expr = with_spaces_before(arena, expr, first_space.value);
                Ok((MadeProgress, expr, state))
            }
            Err(e) => Err((MadeProgress, wrap_error(arena.alloc(e), last_pos))),
        }
    } else {
        let prev_pos = state.pos();
        match parse_expr_start(options, arena, state, min_indent) {
            Ok((_, expr, state)) => {
                let expr = with_spaces_before(arena, expr, first_space.value);
                Ok((MadeProgress, expr, state))
            }
            Err((_, e)) => Err((MadeProgress, wrap_error(arena.alloc(e), prev_pos))),
        }
    }
}

/// Parse a sequence of statements, which we'll later process into an expression.
/// Statements can include:
/// - assignments
/// - type annotations
/// - expressions
/// - [multi]backpassing
///
/// This function doesn't care about whether the order of those statements makes any sense.
/// e.g. it will happily parse two expressions in a row, or backpassing with nothing following it.
fn parse_stmt_seq<'a, E: SpaceProblem + 'a>(
    arena: &'a Bump,
    mut state: State<'a>,
    wrap_error: fn(&'a EExpr<'a>, Position) -> E,
    options: ExprParseOptions,
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
        let start = state.pos();
        let stmt =
            match parse_stmt_start(options, last_space.region, arena, state.clone(), min_indent) {
                Ok((_, stmt, new_state)) => {
                    state_before_space = new_state.clone();
                    state = new_state;
                    stmt
                }
                Err((NoProgress, _)) => {
                    if stmts.is_empty() {
                        let fail = wrap_error(arena.alloc(EExpr::Start(start)), start);
                        return Err((NoProgress, fail));
                    }

                    state = state_before_space;
                    break;
                }
                Err((_, fail)) => {
                    return Err((MadeProgress, wrap_error(arena.alloc(fail), start)));
                }
            };

        stmts.push(SpacesBefore {
            before: last_space.value,
            item: stmt,
        });

        match eat_space_loc_comments(indent_problem, arena, state.clone(), min_indent) {
            Ok((_, space, new_state)) => {
                if space.value.is_empty() {
                    // require a newline or a terminator after the statement
                    if at_terminator(&new_state) {
                        state = state_before_space;
                        break;
                    }
                    let last_pos = state.pos();
                    let fail = wrap_error(arena.alloc(EExpr::BadExprEnd(last_pos)), last_pos);
                    return Err((MadeProgress, fail));
                }
                last_space = space;
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
            Stmt::Backpassing(..) | Stmt::TypeDef(_) | Stmt::ValueDef(_) => {
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
            Stmt::Expr(e) => {
                if is_expr_suffixed(&e) && i + 1 < stmts.len() {
                    defs.push_value_def(
                        ValueDef::Stmt(arena.alloc(Loc::at(sp_stmt.item.region, e))),
                        sp_stmt.item.region,
                        sp_stmt.before,
                        &[],
                    );
                } else {
                    if last_expr.is_some() {
                        return Err(EExpr::StmtAfterExpr(sp_stmt.item.region.start()));
                    }

                    let e = if sp_stmt.before.is_empty() {
                        e
                    } else {
                        arena.alloc(e).before(sp_stmt.before)
                    };

                    last_expr = Some(sp_stmt.item.with_value(e));
                }
            }
            Stmt::Backpassing(pats, call) => {
                if last_expr.is_some() {
                    return Err(EExpr::StmtAfterExpr(sp_stmt.item.region.start()));
                }

                if i + 1 >= stmts.len() {
                    return Err(EExpr::BackpassContinue(sp_stmt.item.region.end()));
                }

                let rest = stmts_to_expr(&stmts[i + 1..], arena)?;

                let e = Expr::Backpassing(arena.alloc(pats), arena.alloc(call), arena.alloc(rest));

                let e = if sp_stmt.before.is_empty() {
                    e
                } else {
                    arena.alloc(e).before(sp_stmt.before)
                };

                let region = Region::new(sp_stmt.item.region.start(), rest.region.end());

                last_expr = Some(Loc::at(region, e));

                // don't re-process the rest of the statements; they got consumed by the backpassing
                break;
            }

            Stmt::TypeDef(td) => {
                if last_expr.is_some() {
                    return Err(EExpr::StmtAfterExpr(sp_stmt.item.region.start()));
                }

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
                    if spaces_middle.len() <= 1
                        || header
                            .vars
                            .first()
                            .map(|var| var.value.equivalent(&loc_pattern.value))
                            .unwrap_or(false)
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
                if last_expr.is_some() {
                    return Err(EExpr::StmtAfterExpr(sp_stmt.item.region.start()));
                }

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
                            Expr::DbgStmt(arena.alloc(condition), arena.alloc(rest))
                        } else {
                            Expr::Apply(
                                arena.alloc(Loc::at(sp_stmt.item.region, Expr::Dbg)),
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
                    if spaces_middle.len() <= 1 || ann_pattern.value.equivalent(&loc_pattern.value)
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
    let ann_pattern = Pattern::Apply(loc_name, header.vars);

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

fn parse_rest_of_list_expr<'a>(
    start: Position,
    arena: &'a Bump,
    state: State<'a>,
) -> ParseResult<'a, Loc<Expr<'a>>, EExpr<'a>> {
    let parser = move |arena, state: State<'a>, min_indent: u32| {
        parse_expr_start(ExprParseOptions::NO_BACK_ARROW, arena, state, min_indent)
    };

    let inner = collection_inner(specialize_err_ref(EList::Expr, parser), Expr::SpaceBefore);

    let (elems, state) = match inner.parse(arena, state, 0) {
        Ok((_, elems, state)) => (elems, state),
        Err((_, fail)) => return Err((MadeProgress, EExpr::List(fail, start))),
    };

    if state.bytes().first() != Some(&b']') {
        let fail = EList::End(state.pos());
        return Err((MadeProgress, EExpr::List(fail, start)));
    }
    let state = state.inc();

    let elems = elems.ptrify_items(arena);
    let elems = Loc::pos(start, state.pos(), Expr::List(elems));
    Ok((MadeProgress, elems, state))
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum RecordField<'a> {
    RequiredValue(Loc<&'a str>, &'a [CommentOrNewline<'a>], &'a Loc<Expr<'a>>),
    OptionalValue(Loc<&'a str>, &'a [CommentOrNewline<'a>], &'a Loc<Expr<'a>>),
    IgnoredValue(Loc<&'a str>, &'a [CommentOrNewline<'a>], &'a Loc<Expr<'a>>),
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
pub enum NotOldBuilderFieldValue {
    FoundOptionalValue,
    FoundIgnoredValue,
}

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

            RecordField::IgnoredValue(loc_label, spaces, loc_expr) => {
                Ok(IgnoredValue(loc_label, spaces, loc_expr))
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
    ) -> Result<OldRecordBuilderField<'a>, NotOldBuilderFieldValue> {
        use OldRecordBuilderField::*;

        match self {
            RecordField::RequiredValue(loc_label, spaces, loc_expr) => {
                Ok(Value(loc_label, spaces, loc_expr))
            }

            RecordField::OptionalValue(_, _, _) => Err(NotOldBuilderFieldValue::FoundOptionalValue),

            RecordField::IgnoredValue(_, _, _) => Err(NotOldBuilderFieldValue::FoundIgnoredValue),

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

pub fn parse_record_field<'a>(
    arena: &'a Bump,
    state: State<'a>,
    min_indent: u32,
) -> ParseResult<'a, RecordField<'a>, ERecord<'a>> {
    use RecordField::*;
    let start = state.pos();
    match parse_lowercase_ident(state.clone()) {
        Err((NoProgress, _)) => { /* skip below */ }
        Err(_) => return Err((MadeProgress, ERecord::Field(start))),
        Ok((_, label, state)) => {
            let field_label = Loc::pos(start, state.pos(), label);

            let (_, (label_spaces, _), mut state) = eat_space(arena, state, true)?;

            if state.bytes().first() == Some(&b':') {
                state.advance_mut(1);

                let (_, (colon_spaces, _), mut state) = eat_space(arena, state, true)?;

                let has_back_arrow = state.bytes().starts_with(b"<-");

                let (arrow_spaces, state) = if has_back_arrow {
                    state.advance_mut(2);

                    let back_arrow_pos = state.pos();
                    match parse_space(EExpr::IndentEnd, arena, state, min_indent) {
                        Ok((_, out, state)) => (out, state),
                        Err((_, fail)) => {
                            let fail = ERecord::Expr(arena.alloc(fail), back_arrow_pos);
                            return Err((MadeProgress, fail));
                        }
                    }
                } else {
                    (&[] as &[_], state)
                };

                let field_val_pos = state.pos();
                let (field_val, state) = match parse_expr_start(
                    ExprParseOptions::NO_BACK_ARROW,
                    arena,
                    state,
                    min_indent,
                ) {
                    Ok((_, expr, state)) => (expr, state),
                    Err((_, fail)) => {
                        let fail = ERecord::Expr(arena.alloc(fail), field_val_pos);
                        return Err((MadeProgress, fail));
                    }
                };

                let field = if has_back_arrow {
                    let field = with_spaces_before(arena, field_val, arrow_spaces);
                    ApplyValue(field_label, label_spaces, colon_spaces, arena.alloc(field))
                } else {
                    let field = with_spaces_before(arena, field_val, colon_spaces);
                    RequiredValue(field_label, label_spaces, arena.alloc(field))
                };
                return Ok((MadeProgress, field, state));
            }

            if state.bytes().first() == Some(&b'?') {
                state.advance_mut(1);

                let (_, (question_spaces, _), state) = eat_space(arena, state, true)?;

                let field_val_pos = state.pos();
                match parse_expr_start(ExprParseOptions::NO_BACK_ARROW, arena, state, min_indent) {
                    Ok((_, field_val, state)) => {
                        let field_val = with_spaces_before(arena, field_val, question_spaces);
                        let field =
                            OptionalValue(field_label, label_spaces, arena.alloc(field_val));
                        return Ok((MadeProgress, field, state));
                    }
                    Err((_, fail)) => {
                        let fail = ERecord::Expr(arena.alloc(fail), field_val_pos);
                        return Err((MadeProgress, fail));
                    }
                }
            }

            let field = if !label_spaces.is_empty() {
                SpaceAfter(arena.alloc(LabelOnly(field_label)), label_spaces)
            } else {
                LabelOnly(field_label)
            };
            return Ok((MadeProgress, field, state));
        }
    }

    // Or parse the ignored field, e.g. `_ : val_expr` or `_foo: val_expr`
    if state.bytes().first() != Some(&b'_') {
        return Err((NoProgress, ERecord::UnderscoreField(start)));
    }
    let state = state.inc();

    let after_underscore = state.pos();
    let (field_label, state) = match parse_lowercase_ident(state.clone()) {
        Ok((_, label, state)) => (label, state),
        Err((NoProgress, _)) => ("", state),
        Err(_) => return Err((MadeProgress, ERecord::Field(after_underscore))),
    };

    let field_label_end = state.pos();

    let (_, (label_spaces, _), mut state) = eat_space(arena, state, true)?;

    let colon_pos = state.pos();
    if state.bytes().first() != Some(&b':') {
        return Err((MadeProgress, ERecord::Colon(colon_pos)));
    }
    state.advance_mut(1);

    let (_, (colon_spaces, _), state) = eat_space(arena, state, true)?;

    let field_val_pos = state.pos();
    let (field_val, state) =
        match parse_expr_start(ExprParseOptions::NO_BACK_ARROW, arena, state, min_indent) {
            Ok((_, out, state)) => (out, state),
            Err((_, fail)) => {
                let fail = ERecord::Expr(arena.alloc(fail), field_val_pos);
                return Err((MadeProgress, fail));
            }
        };

    let field_val = with_spaces_before(arena, field_val, colon_spaces);

    let field_label = Loc::pos(start, field_label_end, field_label);

    let field = IgnoredValue(field_label, label_spaces, arena.alloc(field_val));
    Ok((MadeProgress, field, state))
}

enum RecordHelpPrefix {
    Update,
    Mapper,
}

struct RecordHelp<'a> {
    prefix: Option<(Loc<Expr<'a>>, RecordHelpPrefix)>,
    fields: Collection<'a, Loc<RecordField<'a>>>,
}

fn record_help<'a>() -> impl Parser<'a, RecordHelp<'a>, ERecord<'a>> {
    let fields_parser = collection_inner(loc(parse_record_field), RecordField::SpaceBefore);

    move |arena: &'a Bump, state: State<'a>, _: u32| {
        let start = state.pos();
        if state.bytes().first() != Some(&b'{') {
            return Err((NoProgress, ERecord::Open(start)));
        }
        let state = state.inc();

        // You can optionally have an identifier followed by an '&' to
        // make this a record update, e.g. { Foo.user & username: "blah" }.

        // We wrap the ident in an Expr here,
        // so that we have a Spaceable value to work with,
        // and then in canonicalization verify that it's an Expr::Var
        // (and not e.g. an `Expr::Access`) and extract its string.
        let before_prefix = state.clone();
        let (prefix, state) = match eat_space::<'_, ERecord<'_>>(arena, state, false) {
            Err(_) => (None, before_prefix),
            Ok((_, (spaces_before, _), state)) => {
                let ident_at = state.pos();
                match parse_ident.parse(arena, state, 0) {
                    Err(_) => (None, before_prefix),
                    Ok((_, ident, state)) => {
                        let ident = Loc::pos(ident_at, state.pos(), ident_to_expr(arena, ident));
                        match eat_space::<'_, ERecord<'_>>(arena, state, false) {
                            Err(_) => (None, before_prefix),
                            Ok((_, (spaces_after, _), state)) => {
                                let ident = with_spaces(arena, spaces_before, ident, spaces_after);

                                if state.bytes().first() == Some(&b'&') {
                                    (Some((ident, RecordHelpPrefix::Update)), state.inc())
                                } else if state.bytes().starts_with(b"<-") {
                                    (Some((ident, RecordHelpPrefix::Mapper)), state.advance(2))
                                } else {
                                    (None, before_prefix)
                                }
                            }
                        }
                    }
                }
            }
        };

        let (fields, state) = match fields_parser.parse(arena, state, 0) {
            Ok((_, f, state)) => (f, state),
            Err((_, fail)) => return Err((MadeProgress, fail)),
        };

        if state.bytes().first() != Some(&b'}') {
            return Err((MadeProgress, ERecord::End(state.pos())));
        }
        let state = state.inc();

        let record = RecordHelp { prefix, fields };
        Ok((MadeProgress, record, state))
    }
}

fn parse_record_expr<'a>(
    start: Position,
    arena: &'a Bump,
    state: State<'a>,
    min_indent: u32,
) -> ParseResult<'a, Loc<Expr<'a>>, EExpr<'a>> {
    let (_, record, state) =
        specialize_err(EExpr::Record, record_help()).parse(arena, state, min_indent)?;

    let (accessors, state) = match parse_record_field_access_chain(arena, state, min_indent) {
        Ok((_, accessors, state)) => (accessors, state),
        Err((_, fail)) => return Err((MadeProgress, fail)),
    };

    let expr_res = match record.prefix {
        Some((update, RecordHelpPrefix::Update)) => {
            record_update_help(arena, update, record.fields)
        }
        Some((mapper, RecordHelpPrefix::Mapper)) => {
            new_record_builder_help(arena, mapper, record.fields)
        }
        None => {
            let special_field_found = record.fields.iter().find_map(|field| {
                if field.value.is_apply_value() {
                    Some(old_record_builder_help(arena, record.fields))
                } else if field.value.is_ignored_value() {
                    Some(Err(EExpr::RecordUpdateIgnoredField(field.region)))
                } else {
                    None
                }
            });

            special_field_found.unwrap_or_else(|| {
                let fields = record.fields.map_items(arena, |loc_field| {
                    loc_field.map(|field| field.to_assigned_field(arena).unwrap())
                });

                Ok(Expr::Record(fields))
            })
        }
    };

    match expr_res {
        Ok(expr) => {
            let expr = apply_expr_access_chain(arena, expr, accessors);
            Ok((MadeProgress, Loc::pos(start, state.pos(), expr), state))
        }
        Err(fail) => Err((MadeProgress, fail)),
    }
}

fn record_update_help<'a>(
    arena: &'a Bump,
    update: Loc<Expr<'a>>,
    fields: Collection<'a, Loc<RecordField<'a>>>,
) -> Result<Expr<'a>, EExpr<'a>> {
    let result = fields.map_items_result(arena, |loc_field| {
        match loc_field.value.to_assigned_field(arena) {
            Ok(AssignedField::IgnoredValue(_, _, _)) => {
                Err(EExpr::RecordUpdateIgnoredField(loc_field.region))
            }
            Ok(builder_field) => Ok(Loc {
                region: loc_field.region,
                value: builder_field,
            }),
            Err(FoundApplyValue) => Err(EExpr::RecordUpdateOldBuilderField(loc_field.region)),
        }
    });

    result.map(|fields| Expr::RecordUpdate {
        update: &*arena.alloc(update),
        fields,
    })
}

fn new_record_builder_help<'a>(
    arena: &'a Bump,
    mapper: Loc<Expr<'a>>,
    fields: Collection<'a, Loc<RecordField<'a>>>,
) -> Result<Expr<'a>, EExpr<'a>> {
    let result = fields.map_items_result(arena, |loc_field| {
        match loc_field.value.to_assigned_field(arena) {
            Ok(builder_field) => Ok(Loc {
                region: loc_field.region,
                value: builder_field,
            }),
            Err(FoundApplyValue) => Err(EExpr::RecordBuilderOldBuilderField(loc_field.region)),
        }
    });

    result.map(|fields| Expr::RecordBuilder {
        mapper: &*arena.alloc(mapper),
        fields,
    })
}

fn old_record_builder_help<'a>(
    arena: &'a Bump,
    fields: Collection<'a, Loc<RecordField<'a>>>,
) -> Result<Expr<'a>, EExpr<'a>> {
    let result = fields.map_items_result(arena, |loc_field| {
        match loc_field.value.to_builder_field(arena) {
            Ok(builder_field) => Ok(Loc {
                region: loc_field.region,
                value: builder_field,
            }),
            Err(NotOldBuilderFieldValue::FoundOptionalValue) => {
                Err(EExpr::OptionalValueInOldRecordBuilder(loc_field.region))
            }
            Err(NotOldBuilderFieldValue::FoundIgnoredValue) => {
                Err(EExpr::IgnoredValueInOldRecordBuilder(loc_field.region))
            }
        }
    });

    result.map(Expr::OldRecordBuilder)
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
            Suffix::TrySuffix(target) => Expr::TrySuffix {
                target,
                expr: arena.alloc(value),
            },
        })
}

/// A minus is unary if:
/// - it is preceded by whitespace (spaces, newlines, comments)
/// - it is not followed by whitespace
fn parse_unary_minus<'a>(
    start: Position,
    options: ExprParseOptions,
    arena: &'a Bump,
    state: State<'a>,
    min_indent: u32,
) -> ParseResult<'a, Loc<Expr<'a>>, EExpr<'a>> {
    let initial = state.clone();
    let state = state.inc();
    let loc_op = Region::new(start, state.pos());

    let (_, loc_expr, state) =
        parse_term(options, arena, state, min_indent).map_err(|(_, fail)| (MadeProgress, fail))?;

    let expr = numeric_negate_expression(arena, &initial, loc_op, loc_expr, &[]);
    Ok((MadeProgress, expr, state))
}

fn literal_to_expr(literal: crate::number_literal::NumLiteral<'_>) -> Expr<'_> {
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

fn parse_rest_of_logical_not<'a>(
    start: Position,
    options: ExprParseOptions,
    arena: &'a Bump,
    state: State<'a>,
    min_indent: u32,
) -> ParseResult<'a, Loc<Expr<'a>>, EExpr<'a>> {
    let after_not = state.pos();
    return match parse_space(EExpr::IndentStart, arena, state, min_indent) {
        Ok((_, spaces_before, state)) => match parse_term(options, arena, state, min_indent) {
            Ok((_, loc_expr, state)) => {
                let loc_expr = with_spaces_before(arena, loc_expr, spaces_before);
                let op = Loc::pos(start, after_not, UnaryOp::Not);
                let op = Expr::UnaryOp(arena.alloc(loc_expr), op);
                let op = Loc::pos(start, state.pos(), op);
                Ok((MadeProgress, op, state))
            }
            Err((_, fail)) => Err((MadeProgress, fail)),
        },
        Err((_, fail)) => Err((MadeProgress, fail)),
    };
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
    Backpassing,
}

fn parse_bin_op<'a>(err_progress: Progress, state: State<'a>) -> ParseResult<'a, BinOp, EExpr<'a>> {
    let start = state.pos();
    let (_, op, state) = parse_operator(EExpr::Start, EExpr::BadOperator, state)?;
    match op {
        OperatorOrDef::BinOp(op) => Ok((MadeProgress, op, state)),
        OperatorOrDef::Assignment => Err((err_progress, EExpr::BadOperator("=", start))),
        OperatorOrDef::AliasOrOpaque(AliasOrOpaque::Alias) => {
            Err((err_progress, EExpr::BadOperator(":", start)))
        }
        OperatorOrDef::AliasOrOpaque(AliasOrOpaque::Opaque) => {
            Err((err_progress, EExpr::BadOperator(":=", start)))
        }
        OperatorOrDef::Backpassing => Err((err_progress, EExpr::BadOperator("<-", start))),
    }
}

fn parse_operator<'a, F, G, E>(
    to_expectation: F,
    to_error: G,
    mut state: State<'a>,
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
        "-" => good!(OperatorOrDef::BinOp(BinOp::Minus), 1),
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
        "<-" => good!(OperatorOrDef::Backpassing, 2),
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
