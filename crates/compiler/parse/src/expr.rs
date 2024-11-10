use std::cell::Cell;

use crate::ast::{
    is_expr_suffixed, AccessShortcut, AssignedField, ClosureShortcut, Collection, CommentOrNewline,
    Defs, Expr, ExtractSpaces, Implements, ImportAlias, ImportAsKeyword, ImportExposingKeyword,
    ImportedModuleName, IngestedFileAnnotation, IngestedFileImport, ModuleImport,
    ModuleImportParams, Pattern, Spaceable, Spaced, Spaces, SpacesBefore, TryTarget,
    TypeAnnotation, TypeDef, TypeHeader, ValueDef, WhenShortcut,
};
use crate::blankspace::{eat_nc, eat_nc_check, eat_space_loc_comments, SpacedBuilder};
use crate::header::{chomp_module_name, ModuleName};
use crate::ident::{
    chomp_access_chain, chomp_integer_part, chomp_lowercase_part, malformed_ident,
    parse_anycase_ident, parse_ident_chain, parse_lowercase_ident, Accessor, BadIdent, Ident,
    Suffix,
};
use crate::number_literal::parse_number_base;
use crate::parser::{
    at_keyword, collection_inner, EClosure, EExpect, EExpr, EIf, EImport, EImportParams, EInParens,
    EList, EPattern, ERecord, EReturn, EType, EWhen, ParseResult, Parser, SpaceProblem,
    SyntaxError,
};
use crate::pattern::parse_closure_param;
use crate::state::State;
use crate::string_literal::{self, rest_of_str_like, StrLikeLiteral};
use crate::type_annotation::{
    self, parse_implements_abilities, type_expr, NO_TYPE_EXPR_FLAGS, STOP_AT_FIRST_IMPL,
    TRAILING_COMMA_VALID,
};
use crate::{header, keyword};
use bumpalo::collections::Vec;
use bumpalo::Bump;
use roc_collections::soa::slice_extend_new;
use roc_error_macros::internal_error;
use roc_module::called_via::{BinOp, CalledVia, UnaryOp};
use roc_region::all::{Loc, Position, Region};

use crate::parser::Progress::{self, *};

pub fn test_parse_expr<'a>(
    arena: &'a bumpalo::Bump,
    state: State<'a>,
) -> Result<Loc<Expr<'a>>, SyntaxError<'a>> {
    let (spaces_before, state) = match eat_nc_check(EExpr::IndentStart, arena, state, 0, false) {
        Ok((_, sp, state)) => (sp, state),
        Err((_, fail)) => return Err(SyntaxError::Expr(fail, Position::default())),
    };

    let flags = CHECK_FOR_ARROW | ACCEPT_MULTI_BACKPASSING;
    let (expr, state) = match parse_expr_block(flags, arena, state, 0) {
        Ok((_, out, state)) => (out, state),
        Err((_, fail)) => return Err(SyntaxError::Expr(fail, Position::default())),
    };

    let (spaces_after, state) = match eat_nc_check(EExpr::IndentEnd, arena, state.clone(), 0, false)
    {
        Ok((_, spaces_after, state)) => (spaces_after, state),
        Err(_) => (&[] as &[_], state),
    };

    if state.has_reached_end() {
        Ok(expr.spaced_around(arena, spaces_before, spaces_after))
    } else {
        let fail = EExpr::BadExprEnd(state.pos());
        Err(SyntaxError::Expr(fail, Position::default()))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ExprParseFlags(u8);

pub const NO_EXPR_PARSE_FLAGS: ExprParseFlags = ExprParseFlags(0);

/// Check for and accept multi-backpassing syntax
/// This is usually true, but false within list/record literals
/// because the comma separating backpassing arguments conflicts
/// with the comma separating literal elements
pub const ACCEPT_MULTI_BACKPASSING: ExprParseFlags = ExprParseFlags(1);

/// Check for the `->` token, and raise an error if found
/// This is usually true, but false in if-guards
///
/// > Just foo if foo == 2 -> ...
pub const CHECK_FOR_ARROW: ExprParseFlags = ExprParseFlags(1 << 1);

impl ExprParseFlags {
    pub const fn is_set(&self, flag: Self) -> bool {
        (self.0 & flag.0) != 0
    }

    #[must_use]
    pub const fn unset(&self, flag: Self) -> Self {
        Self(self.0 & !flag.0)
    }
}

impl std::ops::BitOr for ExprParseFlags {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {
        Self(self.0 | rhs.0)
    }
}

fn rest_of_expr_in_parens_etc<'a>(
    start: Position,
    arena: &'a Bump,
    state: State<'a>,
) -> ParseResult<'a, Loc<Expr<'a>>, EExpr<'a>> {
    let elem_p = move |a: &'a Bump, state: State<'a>, min_indent: u32| {
        let block_pos = state.pos();
        match parse_expr_block(CHECK_FOR_ARROW, a, state, min_indent) {
            Ok(ok) => Ok(ok),
            Err((p, fail)) => return Err((p, EInParens::Expr(a.alloc(fail), block_pos))),
        }
    };
    let (elems, state) = match collection_inner(elem_p, Expr::SpaceBefore).parse(arena, state, 0) {
        Ok((_, out, state)) => (out, state),
        Err((p, fail)) => return Err((p, EExpr::InParens(fail, start))),
    };

    if state.bytes().first() != Some(&b')') {
        let fail = EInParens::End(state.pos());
        return Err((MadeProgress, EExpr::InParens(fail, start)));
    }
    let state = state.inc();

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

    let (field_accesses, state) = match parse_field_task_result_suffixes(arena, state) {
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

fn parse_field_task_result_suffixes<'a>(
    arena: &'a Bump,
    mut state: State<'a>,
) -> ParseResult<'a, Vec<'a, Suffix<'a>>, EExpr<'a>> {
    let mut fields = Vec::with_capacity_in(1, arena);
    loop {
        let prev_state = state.clone();
        let (field, next_state) = match state.bytes().first() {
            Some(b) => match b {
                b'.' => {
                    let state = state.inc();
                    let ident_pos = state.pos();
                    match parse_lowercase_ident(state.clone()) {
                        Ok((_, name, state)) => {
                            (Suffix::Accessor(Accessor::RecordField(name)), state)
                        }
                        Err((NoProgress, _)) => {
                            // This is a tuple accessor, e.g. "1" in `.1`
                            match chomp_integer_part(state.bytes()) {
                                Ok(name) => (
                                    Suffix::Accessor(Accessor::TupleIndex(name)),
                                    state.advance(name.len()),
                                ),
                                Err(_) => return Err((MadeProgress, EExpr::Access(ident_pos))),
                            }
                        }
                        Err(_) => return Err((MadeProgress, EExpr::Access(ident_pos))),
                    }
                }
                b'!' => (Suffix::TrySuffix(TryTarget::Task), state.inc()),
                b'?' => (Suffix::TrySuffix(TryTarget::Result), state.inc()),
                _ => return Ok((Progress::when(!fields.is_empty()), fields, prev_state)),
            },
            _ => return Ok((Progress::when(!fields.is_empty()), fields, prev_state)),
        };

        fields.push(field);
        state = next_state;
    }
}

fn parse_negative_number<'a>(
    start: Position,
    flags: ExprParseFlags,
    arena: &'a Bump,
    state: State<'a>,
    min_indent: u32,
) -> Result<(Progress, Loc<Expr<'a>>, State<'a>), (Progress, EExpr<'a>)> {
    // unary minus should not be followed by whitespace or comment
    if !state
        .bytes()
        .get(1)
        .map(|b| b.is_ascii_whitespace() || *b == b'#')
        .unwrap_or(false)
    {
        let initial = state.clone();
        let state = state.inc();
        let loc_op = Region::new(start, state.pos());

        match parse_term(PARSE_DEFAULT, flags, arena, state, min_indent) {
            Ok((_, out, state)) => {
                let expr = numeric_negate_expr(arena, &initial, loc_op, out, &[]);
                Ok((MadeProgress, expr, state))
            }
            Err((_, fail)) => Err((MadeProgress, fail)),
        }
    } else {
        // drop the minus and parse '0b', '0o', '0x', etc.
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
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ParseTermOpts(u8);

impl ParseTermOpts {
    pub const fn is_set(&self, opt: Self) -> bool {
        (self.0 & opt.0) != 0
    }
}

impl std::ops::BitOr for ParseTermOpts {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {
        Self(self.0 | rhs.0)
    }
}

pub const PARSE_DEFAULT: ParseTermOpts = ParseTermOpts(0);

pub const PARSE_UNDERSCORE: ParseTermOpts = ParseTermOpts(0b1);
pub const PARSE_NEGATIVE: ParseTermOpts = ParseTermOpts(0b10);
pub const PARSE_IF_WHEN: ParseTermOpts = ParseTermOpts(0b100);
pub const PARSE_ALL: ParseTermOpts = ParseTermOpts(0b111);

pub const PARSE_NO_CLOSURE: ParseTermOpts = ParseTermOpts(0b1000);

fn parse_term<'a>(
    opts: ParseTermOpts,
    flags: ExprParseFlags,
    arena: &'a Bump,
    state: State<'a>,
    min_indent: u32,
) -> ParseResult<'a, Loc<Expr<'a>>, EExpr<'a>> {
    let start = state.pos();
    if let Some(b) = state.bytes().first() {
        match b {
            b'\\' => {
                if !opts.is_set(PARSE_NO_CLOSURE) {
                    match rest_of_closure(flags, arena, state.inc()) {
                        Ok((p, expr, state)) => Ok((p, Loc::pos(start, state.pos(), expr), state)),
                        Err((p, fail)) => Err((p, EExpr::Closure(fail, start))),
                    }
                } else {
                    Err((NoProgress, EExpr::Start(start)))
                }
            }
            b'_' => {
                if opts.is_set(PARSE_UNDERSCORE) {
                    let state = state.inc();
                    match chomp_lowercase_part(state.bytes()) {
                        Ok((name, _)) => {
                            let state = state.advance(name.len());
                            let expr = Loc::pos(start, state.pos(), Expr::Underscore(name));
                            Ok((MadeProgress, expr, state))
                        }
                        Err(NoProgress) => {
                            let expr = Loc::pos(start, state.pos(), Expr::Underscore(""));
                            Ok((MadeProgress, expr, state))
                        }
                        Err(_) => Err((MadeProgress, EExpr::End(start))),
                    }
                } else {
                    Err((NoProgress, EExpr::Start(start)))
                }
            }
            b'-' => {
                if opts.is_set(PARSE_NEGATIVE) {
                    parse_negative_number(start, flags, arena, state, min_indent)
                } else {
                    Err((NoProgress, EExpr::Start(start)))
                }
            }
            b'!' => {
                if opts.is_set(PARSE_NEGATIVE) {
                    rest_of_logical_not(start, flags, arena, state.inc(), min_indent)
                } else {
                    Err((NoProgress, EExpr::Start(start)))
                }
            }
            b'(' => rest_of_expr_in_parens_etc(start, arena, state.inc()),
            b'{' => parse_record_expr(start, arena, state, min_indent),
            b'[' => rest_of_list_expr(start, arena, state.inc()),
            b'"' | b'\'' => {
                let column = state.column();
                match rest_of_str_like(*b == b'\'', column, arena, state.inc(), min_indent) {
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
                if opts.is_set(PARSE_IF_WHEN) {
                    if at_keyword(keyword::IF, &state) {
                        let state = state.advance(keyword::IF.len());
                        return match rest_of_if_expr(flags, arena, state, min_indent) {
                            Ok((p, expr, state)) => {
                                Ok((p, Loc::pos(start, state.pos(), expr), state))
                            }
                            Err((p, err)) => Err((p, EExpr::If(err, start))),
                        };
                    }

                    if at_keyword(keyword::WHEN, &state) {
                        let state = state.advance(keyword::WHEN.len());
                        let indent = state.line_indent();
                        return match when::rest_of_when_expr(None, flags, arena, state, indent) {
                            Ok((p, expr, state)) => {
                                Ok((p, Loc::pos(start, state.pos(), expr), state))
                            }
                            Err((p, err)) => Err((p, EExpr::When(err, start))),
                        };
                    }
                }

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

                // todo: @ask why "try" is not included into KEYWORDS in keyword.rs
                if at_keyword("try", &state) {
                    let state = state.advance("try".len());
                    let try_expr = Loc::pos(start, state.pos(), Expr::Try);
                    return Ok((MadeProgress, try_expr, state));
                }

                let (_, ident, state) = parse_ident_chain(arena, state)?;

                let ident_end = state.pos();
                let (suffixes, state) = match parse_field_task_result_suffixes(arena, state) {
                    Ok((_, out, state)) => (out, state),
                    Err((_, fail)) => return Err((MadeProgress, fail)),
                };

                let mut ident = ident_to_expr(arena, ident, None);
                if !suffixes.is_empty() {
                    ident = apply_expr_access_chain(arena, ident, suffixes);
                }
                Ok((MadeProgress, Loc::pos(start, ident_end, ident), state))
            }
        }
    } else {
        Err((NoProgress, EExpr::Start(start)))
    }
}

/// Entry point for parsing an expression.
/// If Ok it always returns MadeProgress
pub(crate) fn parse_expr_start<'a>(
    flags: ExprParseFlags,
    start_state_and_term: Option<(State<'a>, Loc<Expr<'a>>)>,
    arena: &'a Bump,
    state: State<'a>,
    min_indent: u32,
) -> ParseResult<'a, Loc<Expr<'a>>, EExpr<'a>> {
    let (start_state, term, state) = match start_state_and_term {
        None => {
            let (_, term, news) = parse_term(PARSE_ALL, flags, arena, state.clone(), min_indent)?;
            (state, term, news)
        }
        Some((start_state, term)) => (start_state, term, state),
    };

    let start = start_state.pos();
    let inc_indent = start_state.line_indent() + 1;

    // Parse a chain of expressions separated by operators. Also handles function application.
    let mut prev_state = state.clone();
    let (spaces_before_op, state) =
        match eat_nc_check(EExpr::IndentEnd, arena, state.clone(), min_indent, false) {
            Ok((_, sp, state)) => (sp, state),
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
        let term_res = if state.column() >= inc_indent {
            parse_term(PARSE_UNDERSCORE, flags, arena, state.clone(), inc_indent)
        } else {
            Err((NoProgress, EExpr::Start(state.pos())))
        };

        match term_res {
            Ok((_, mut arg, new_state)) => {
                state = new_state;
                prev_state = state.clone();

                if !expr_state.spaces_after.is_empty() {
                    arg = arg.spaced_before(arena, expr_state.spaces_after);
                    expr_state.spaces_after = &[];
                }
                expr_state.arguments.push(arena.alloc(arg));
                expr_state.end = state.pos();

                match eat_nc_check(EExpr::IndentEnd, arena, state.clone(), min_indent, false) {
                    Err(_) => {
                        expr_state.spaces_after = &[];

                        let expr = finalize_expr(expr_state, arena);
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
            Err((MadeProgress, f)) => return Err((MadeProgress, f)),
            Err((NoProgress, _)) => {
                let before_op = state.clone();
                // We're part way thru parsing an expression, e.g. `bar foo `.
                // We just tried parsing an argument and determined we couldn't -
                // so we're going to try parsing an operator.
                let op_res = match parse_bin_op(MadeProgress, state.clone()) {
                    Err((NoProgress, _)) => {
                        // roll back space parsing
                        let expr = finalize_expr(expr_state, arena);
                        Ok((MadeProgress, expr, prev_state))
                    }
                    Err(err) => Err(err),
                    Ok((_, op, state)) => {
                        let op_start = before_op.pos();
                        let op_end = state.pos();

                        expr_state.consume_spaces(arena);

                        if let BinOp::When = op {
                            let when_pos = state.pos();
                            let cond = Some((expr_state.expr, WhenShortcut::BinOp));
                            match when::rest_of_when_expr(cond, flags, arena, state, min_indent) {
                                Ok(ok) => Ok(ok),
                                Err((p, fail)) => Err((p, EExpr::When(fail, when_pos))),
                            }
                        } else {
                            let (_, spaces_after_op, state) =
                                eat_nc_check(EExpr::IndentEnd, arena, state, min_indent, false)?;

                            let loc_op = Loc::pos(op_start, op_end, op);
                            match op {
                                BinOp::Minus
                                    if expr_state.end != op_start && op_end == state.pos() =>
                                {
                                    // a `-` is unary if it is preceded by a space and not followed by a space
                                    parse_negative_term(
                                        start, arena, state, min_indent, inc_indent, expr_state,
                                        flags, before_op, loc_op,
                                    )
                                }
                                _ => parse_after_binop(
                                    start,
                                    arena,
                                    state,
                                    min_indent,
                                    inc_indent,
                                    flags,
                                    spaces_after_op,
                                    expr_state,
                                    loc_op,
                                ),
                            }
                        }
                    }
                };
                return op_res.map(|(_, expr, state)| {
                    (MadeProgress, Loc::pos(start, state.pos(), expr), state)
                });
            }
        }
    }
}

pub fn parse_repl_defs_and_optional_expr<'a>(
    arena: &'a Bump,
    state: State<'a>,
) -> ParseResult<'a, (Defs<'a>, Option<Loc<Expr<'a>>>), EExpr<'a>> {
    let start = state.pos();
    let (spaces_before, state) = match eat_nc(arena, state.clone(), false) {
        Err((NoProgress, _)) => return Ok((NoProgress, (Defs::default(), None), state)),
        Err(err) => return Err(err),
        Ok((_, (sp, _), state)) => (Loc::pos(start, state.pos(), sp), state),
    };

    let (_, stmts, state) = parse_stmt_seq(
        arena,
        state,
        |e, _| e.clone(),
        CHECK_FOR_ARROW | ACCEPT_MULTI_BACKPASSING,
        0,
        spaces_before,
        EExpr::IndentEnd,
    )?;

    let state = match eat_nc(arena, state.clone(), false) {
        Err((NoProgress, _)) => state,
        Err(err) => return Err(err),
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
    flags: ExprParseFlags,
    comment_region: Region,
    arena: &'a Bump,
    mut state: State<'a>,
    min_indent: u32,
) -> ParseResult<'a, Loc<Stmt<'a>>, EExpr<'a>> {
    let start = state.pos();
    match state.bytes().first() {
        Some(b) => match b {
            b'\\' => match rest_of_closure(flags, arena, state.inc()) {
                Ok((p, expr, state)) => {
                    Ok((p, Loc::pos(start, state.pos(), Stmt::Expr(expr)), state))
                }
                Err((p, fail)) => Err((p, EExpr::Closure(fail, start))),
            },
            b'i' => {
                if at_keyword(keyword::IF, &state) {
                    state.advance_mut(keyword::IF.len());
                    match rest_of_if_expr(flags, arena, state, min_indent) {
                        Ok((p, expr, state)) => {
                            Ok((p, Loc::pos(start, state.pos(), Stmt::Expr(expr)), state))
                        }
                        Err((p, err)) => Err((p, EExpr::If(err, start))),
                    }
                } else if at_keyword(keyword::IMPORT, &state) {
                    state.advance_mut(keyword::IMPORT.len());
                    rest_of_import(start, arena, state, min_indent)
                } else {
                    parse_stmt_operator_chain(flags, arena, state, min_indent)
                }
            }
            b'e' => {
                if at_keyword(keyword::EXPECT, &state) {
                    state.advance_mut(keyword::EXPECT.len());
                    rest_of_expect_stmt(false, start, flags, comment_region, arena, state)
                } else if at_keyword(keyword::EXPECT_FX, &state) {
                    state.advance_mut(keyword::EXPECT_FX.len());
                    rest_of_expect_stmt(true, start, flags, comment_region, arena, state)
                } else {
                    parse_stmt_operator_chain(flags, arena, state, min_indent)
                }
            }
            b'd' => {
                if at_keyword(keyword::DBG, &state) {
                    state.advance_mut(keyword::DBG.len());
                    rest_of_dbg_stmt(start, flags, comment_region, arena, state)
                } else {
                    parse_stmt_operator_chain(flags, arena, state, min_indent)
                }
            }
            b'r' => {
                if at_keyword(keyword::RETURN, &state) {
                    state.advance_mut(keyword::RETURN.len());
                    rest_of_return_stmt(start, flags, arena, state)
                } else {
                    parse_stmt_operator_chain(flags, arena, state, min_indent)
                }
            }
            b'w' => {
                if at_keyword(keyword::WHEN, &state) {
                    state.advance_mut(keyword::WHEN.len());
                    let indent = state.line_indent();
                    match when::rest_of_when_expr(None, flags, arena, state, indent) {
                        Ok((p, expr, state)) => {
                            Ok((p, Loc::pos(start, state.pos(), Stmt::Expr(expr)), state))
                        }
                        Err((p, err)) => Err((p, EExpr::When(err, start))),
                    }
                } else {
                    parse_stmt_operator_chain(flags, arena, state, min_indent)
                }
            }
            _ => parse_stmt_operator_chain(flags, arena, state, min_indent),
        },
        None => Err((NoProgress, EExpr::Start(start))),
    }
}

fn parse_stmt_operator_chain<'a>(
    flags: ExprParseFlags,
    arena: &'a Bump,
    state: State<'a>,
    min_indent: u32,
) -> Result<(Progress, Loc<Stmt<'a>>, State<'a>), (Progress, EExpr<'a>)> {
    let start = state.pos();
    let line_indent = state.line_indent();

    let opts = PARSE_NO_CLOSURE | PARSE_UNDERSCORE | PARSE_NEGATIVE;
    let (_, expr, state) = parse_term(opts, flags, arena, state, min_indent)?;

    let mut prev_state = state.clone();
    let end = state.pos();

    let (spaces_before_op, state) =
        match eat_nc_check(EExpr::IndentEnd, arena, state.clone(), min_indent, false) {
            Ok((_, sp, state)) => (sp, state),
            Err(_) => {
                let expr = Loc::pos(start, state.pos(), Stmt::Expr(expr.value));
                return Ok((MadeProgress, expr, state));
            }
        };

    let mut expr_state = ExprState {
        operators: Vec::new_in(arena),
        arguments: Vec::new_in(arena),
        expr,
        spaces_after: spaces_before_op,
        end,
    };

    let inc_indent = line_indent + 1;
    let mut state = state;
    loop {
        let term_res = if state.column() >= inc_indent {
            parse_term(PARSE_UNDERSCORE, flags, arena, state.clone(), inc_indent)
        } else {
            Err((NoProgress, EExpr::Start(state.pos())))
        };

        match term_res {
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
                return match parse_ability_def(expr_state, state, arena, implements, inc_indent) {
                    Ok((td, state)) => {
                        let expr = Loc::pos(start, state.pos(), Stmt::TypeDef(td));
                        Ok((MadeProgress, expr, state))
                    }
                    Err((NoProgress, _)) => Err((NoProgress, EExpr::Start(start))),
                    Err(err) => Err(err),
                };
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
                        // adds the spaces before operator to the preceding expression term
                        expr_state.consume_spaces(arena);

                        if let OperatorOrDef::BinOp(BinOp::When) = op {
                            // the `~` when operator is handled here to handle the spaces specific to the when branches
                            // instead of generic space handling in `parse_stmt_operator`
                            let when_pos = state.pos();
                            let cond = Some((expr_state.expr, WhenShortcut::BinOp));
                            match when::rest_of_when_expr(cond, flags, arena, state, min_indent) {
                                Ok((p, out, state)) => Ok((p, Stmt::Expr(out), state)),
                                Err((p, fail)) => Err((p, EExpr::When(fail, when_pos))),
                            }
                        } else {
                            let loc_op = Loc::pos(before_op.pos(), state.pos(), op);
                            parse_stmt_operator(
                                start, arena, state, min_indent, inc_indent, flags, expr_state,
                                loc_op, before_op,
                            )
                        }
                    }
                    Err((NoProgress, _)) => {
                        if flags.is_set(ACCEPT_MULTI_BACKPASSING) && state.bytes().starts_with(b",")
                        {
                            state = state.inc();
                            parse_stmt_multi_backpassing(
                                expr_state, flags, arena, state, min_indent,
                            )
                        } else if flags.is_set(CHECK_FOR_ARROW) && state.bytes().starts_with(b"->")
                        {
                            Err((MadeProgress, EExpr::BadOperator("->", state.pos())))
                        } else {
                            // roll back space parsing
                            let expr = finalize_expr(expr_state, arena);
                            Ok((MadeProgress, Stmt::Expr(expr), prev_state))
                        }
                    }
                };
                return match op_res {
                    Ok((p, out, state)) => Ok((p, Loc::pos(start, state.pos(), out), state)),
                    Err((NoProgress, _)) => Err((NoProgress, EExpr::Start(start))),
                    Err(err) => Err(err),
                };
            }
            Ok((_, mut arg, new_state)) => {
                state = new_state;
                prev_state = state.clone();

                if !expr_state.spaces_after.is_empty() {
                    arg = arg.spaced_before(arena, expr_state.spaces_after);
                    expr_state.spaces_after = &[];
                }
                expr_state.arguments.push(arena.alloc(arg));
                expr_state.end = state.pos();

                match eat_nc_check(EExpr::IndentEnd, arena, state.clone(), min_indent, false) {
                    Err(_) => {
                        expr_state.spaces_after = &[];
                        let expr = finalize_expr(expr_state, arena);
                        let expr = Loc::pos(start, state.pos(), Stmt::Expr(expr));
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
            if !self.arguments.is_empty() {
                Ok(to_call(arena, self.arguments, self.expr))
            } else {
                Ok(self.expr)
            }
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

fn finalize_expr<'a>(expr_state: ExprState<'a>, arena: &'a Bump) -> Expr<'a> {
    let right_arg = if !expr_state.arguments.is_empty() {
        to_call(arena, expr_state.arguments, expr_state.expr)
    } else {
        expr_state.expr
    };
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
    mut args: Vec<'a, &'a Loc<Expr<'a>>>,
    loc_expr: Loc<Expr<'a>>,
) -> Loc<Expr<'a>> {
    debug_assert!(!args.is_empty());
    let last = args.last().map(|x| x.region).unwrap_or_default();
    let region = Region::span_across(&loc_expr.region, &last);

    let spaces = if let Some(last) = args.last_mut() {
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
        arena.alloc(loc_expr),
        args.into_bump_slice(),
        CalledVia::Space,
    );

    if !spaces.is_empty() {
        apply = arena.alloc(apply).after(spaces)
    }

    Loc::at(region, apply)
}

fn numeric_negate_expr<'a>(
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

    Loc::at(region, new_expr).spaced_before(arena, spaces)
}

fn parse_import_params<'a>(
    arena: &'a Bump,
    state: State<'a>,
    min_indent: u32,
) -> ParseResult<'a, ModuleImportParams<'a>, EImportParams<'a>> {
    let (before, state) = match eat_nc_check(EImportParams::Indent, arena, state, min_indent, false)
    {
        Ok((_, sp, state)) => (sp, state),
        Err((_, fail)) => return Err((NoProgress, fail)),
    };

    let record_pos = state.pos();
    let (record, state) = match record_help().parse(arena, state, min_indent) {
        Ok((_, out, state)) => (out, state),
        Err((p, fail)) => return Err((p, EImportParams::Record(fail, record_pos))),
    };
    let record_at = Region::new(record_pos, state.pos());

    if let Some(prefix) = record.prefix {
        let fail = match prefix {
            (update, RecordHelpPrefix::Update) => EImportParams::RecordUpdateFound(update.region),
            (mapper, RecordHelpPrefix::Mapper) => EImportParams::RecordBuilderFound(mapper.region),
        };
        return Err((MadeProgress, fail));
    }

    let params = record.fields.map_items_result(arena, |loc_field| {
        match loc_field.value.to_assigned_field(arena) {
            AssignedField::IgnoredValue(_, _, _) => Err((
                MadeProgress,
                EImportParams::RecordIgnoredFieldFound(loc_field.region),
            )),
            field => Ok(Loc::at(loc_field.region, field)),
        }
    })?;

    let params = Loc::at(record_at, params);
    let import_params = ModuleImportParams { before, params };
    Ok((MadeProgress, import_params, state))
}

fn parse_imported_module_name(
    state: State<'_>,
) -> ParseResult<'_, ImportedModuleName<'_>, EImport<'_>> {
    let package_pos = state.pos();
    let (package, state) = match parse_lowercase_ident(state.clone()) {
        Ok((_, name, state)) => match state.bytes().first() {
            Some(&b'.') => (Some(name), state.inc()),
            _ => return Err((MadeProgress, EImport::PackageShorthandDot(package_pos))),
        },
        Err((NoProgress, _)) => (None, state),
        Err(_) => return Err((MadeProgress, EImport::PackageShorthand(package_pos))),
    };

    let name_pos = state.pos();
    let (name, state) = match chomp_module_name(state.bytes()) {
        Ok(name) => (ModuleName::new(name), state.advance(name.len())),
        Err(p) => return Err((p, EImport::ModuleName(name_pos))),
    };

    let module_name = ImportedModuleName { package, name };
    Ok((MadeProgress, module_name, state))
}

fn import_as<'a>(
) -> impl Parser<'a, header::KeywordItem<'a, ImportAsKeyword, Loc<ImportAlias<'a>>>, EImport<'a>> {
    move |arena: &'a Bump, state: State<'a>, min_indent: u32| {
        let (_, keyword, state) = header::spaces_around_keyword(
            ImportAsKeyword,
            EImport::As,
            EImport::IndentAs,
            EImport::IndentAlias,
        )
        .parse(arena, state, min_indent)?;

        let ident_pos = state.pos();
        let (item, state) = match parse_anycase_ident(state) {
            Ok((_, ident, state)) => {
                let ident_at = Region::new(ident_pos, state.pos());
                match ident.chars().next() {
                    Some(first) if first.is_uppercase() => {
                        let ident = Loc::at(ident_at, ImportAlias::new(ident));
                        (ident, state)
                    }
                    Some(_) => return Err((MadeProgress, EImport::LowercaseAlias(ident_at))),
                    None => return Err((MadeProgress, EImport::Alias(state.pos()))),
                }
            }
            Err((p, _)) => return Err((p, EImport::Alias(ident_pos))),
        };

        let keyword = header::KeywordItem { keyword, item };
        Ok((MadeProgress, keyword, state))
    }
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
    move |arena: &'a Bump, state: State<'a>, min_indent: u32| {
        let (_, keyword, state) = header::spaces_around_keyword(
            ImportExposingKeyword,
            EImport::Exposing,
            EImport::IndentExposing,
            EImport::ExposingListStart,
        )
        .parse(arena, state, min_indent)?;

        if state.bytes().first() != Some(&b'[') {
            return Err((NoProgress, EImport::ExposingListStart(state.pos())));
        }
        let state = state.inc();

        let elem_p = move |_: &'a Bump, state: State<'a>, _: u32| {
            let pos: Position = state.pos();
            match parse_anycase_ident(state) {
                Ok((p, ident, state)) => {
                    let ident = Spaced::Item(crate::header::ExposedName::new(ident));
                    Ok((p, Loc::pos(pos, state.pos(), ident), state))
                }
                Err((p, _)) => Err((p, EImport::ExposedName(pos))),
            }
        };
        let (item, state) =
            match collection_inner(elem_p, Spaced::SpaceBefore).parse(arena, state, 0) {
                Ok((_, out, state)) => (out, state),
                Err((_, fail)) => return Err((MadeProgress, fail)),
            };

        if state.bytes().first() != Some(&b']') {
            return Err((MadeProgress, EImport::ExposingListEnd(state.pos())));
        }
        let state = state.inc();

        let keyword = header::KeywordItem { keyword, item };
        Ok((MadeProgress, keyword, state))
    }
}

#[inline(always)]
fn import_ingested_file_as<'a>(
    arena: &'a Bump,
    state: State<'a>,
    min_indent: u32,
) -> ParseResult<'a, header::KeywordItem<'a, ImportAsKeyword, Loc<&'a str>>, EImport<'a>> {
    let (_, keyword, state) = header::spaces_around_keyword(
        ImportAsKeyword,
        EImport::As,
        EImport::IndentAs,
        EImport::IndentIngestedName,
    )
    .parse(arena, state, min_indent)?;

    let item_pos = state.pos();
    let (item, state) = match parse_lowercase_ident(state) {
        Ok((_, out, state)) => (out, state),
        Err(_) => return Err((MadeProgress, EImport::IngestedName(item_pos))),
    };
    let item = Loc::pos(item_pos, state.pos(), item);

    let keyword_item = header::KeywordItem { keyword, item };
    Ok((MadeProgress, keyword_item, state))
}

#[inline(always)]
fn import_ingested_file_annotation<'a>() -> impl Parser<'a, IngestedFileAnnotation<'a>, EImport<'a>>
{
    move |arena: &'a bumpalo::Bump, state: State<'a>, min_indent: u32| {
        let (before_colon, state) =
            match eat_nc_check(EImport::IndentColon, arena, state, min_indent, false) {
                Ok((_, sp, state)) => (sp, state),
                Err((_, fail)) => return Err((NoProgress, fail)),
            };

        if state.bytes().first() != Some(&b':') {
            return Err((NoProgress, EImport::Colon(state.pos())));
        }
        let state = state.inc();

        let ann_pos = state.pos();
        let (annotation, state) =
            match type_annotation::type_expr(NO_TYPE_EXPR_FLAGS).parse(arena, state, min_indent) {
                Ok((_, out, state)) => (out, state),
                Err((p, fail)) => return Err((p, EImport::Annotation(fail, ann_pos))),
            };

        let ann = IngestedFileAnnotation {
            before_colon,
            annotation,
        };
        Ok((MadeProgress, ann, state))
    }
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
                let (ann, state) =
                    match type_expr(NO_TYPE_EXPR_FLAGS).parse(arena, state, inc_indent) {
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
                let (ann, state) = match type_expr(TRAILING_COMMA_VALID | STOP_AT_FIRST_IMPL)
                    .parse(arena, state, inc_indent)
                {
                    Ok((_, out, state)) => (out, state),
                    Err((p, fail)) => return Err((p, EExpr::Type(fail, ann_pos))),
                };

                let olds = state.clone();
                let (derived, state) =
                    match eat_nc_check(EType::TIndentStart, arena, state, inc_indent, false) {
                        Err(_) => (None, olds),
                        Ok((_, sp, state)) => {
                            match parse_implements_abilities(arena, state, inc_indent) {
                                Err(_) => (None, olds),
                                Ok((_, abilities, state)) => {
                                    (Some(abilities.spaced_before(arena, sp)), state)
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
        let call = if !arguments.is_empty() {
            to_call(arena, arguments, expr)
        } else {
            expr
        };

        match expr_to_pattern_help(arena, &call.value) {
            Ok(pat) => {
                let ann_pos = state.pos();
                match type_expr(NO_TYPE_EXPR_FLAGS).parse(arena, state, inc_indent) {
                    Ok((_, mut type_ann, state)) => {
                        // put the spaces from after the operator in front of the call
                        type_ann = type_ann.spaced_before(arena, spaces_after_operator);
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
    use super::*;
    use crate::{
        ast::{AbilityMember, Spaced},
        parser::EAbility,
    };

    pub enum IndentLevel {
        PendingMin,
        Exact,
    }

    /// Parses an ability demand like `hash : a -> U64 where a implements Hash`, in the context of a larger
    /// ability definition.
    /// This is basically the same as parsing a free-floating annotation, but with stricter rules.
    pub fn parse_demand<'a>(
        indent_type: IndentLevel,
        indent: u32,
        arena: &'a Bump,
        state: State<'a>,
    ) -> ParseResult<'a, (u32, AbilityMember<'a>), EAbility<'a>> {
        // Put no restrictions on the indent after the spaces; we'll check it manually.
        let (spaces_before, state) = match eat_nc(arena, state, false) {
            Ok((_, (sp, _), state)) => (sp, state),
            Err((_, fail)) => return Err((NoProgress, fail)),
        };

        match indent_type {
            IndentLevel::PendingMin if state.column() < indent => {
                let indent_difference = state.column() as i32 - indent as i32;
                Err((
                    MadeProgress,
                    EAbility::DemandAlignment(indent_difference, state.pos()),
                ))
            }
            IndentLevel::Exact if state.column() < indent => {
                // This demand is not indented correctly
                let indent_difference = state.column() as i32 - indent as i32;
                Err((
                    // Rollback because the deindent may be because there is a next
                    // expression
                    NoProgress,
                    EAbility::DemandAlignment(indent_difference, state.pos()),
                ))
            }
            IndentLevel::Exact if state.column() > indent => {
                // This demand is not indented correctly
                let indent_difference = state.column() as i32 - indent as i32;

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

                let start = state.pos();
                let (name, state) = match parse_lowercase_ident(state) {
                    Ok((_, out, state)) => (out, state),
                    Err(_) => return Err((MadeProgress, EAbility::DemandName(start))),
                };

                let name = Loc::pos(start, state.pos(), Spaced::Item(name));
                let name = name.spaced_before(arena, spaces_before);

                let inc_indent = indent_column + 1;

                // TODO: do we get anything from picking up spaces here?
                let (_, _spaces_after_name, state) =
                    eat_nc_check(EAbility::DemandName, arena, state, inc_indent, true)?;

                if state.bytes().first() != Some(&b':') {
                    return Err((MadeProgress, EAbility::DemandColon(state.pos())));
                }
                let state = state.inc();

                let type_pos = state.pos();
                let (typ, state) = match type_expr(TRAILING_COMMA_VALID)
                    .parse(arena, state, inc_indent)
                {
                    Ok((_, out, state)) => (out, state),
                    Err((_, fail)) => return Err((MadeProgress, EAbility::Type(fail, type_pos))),
                };

                let demand = AbilityMember { name, typ };
                Ok((MadeProgress, (indent_column, demand), state))
            }
        }
    }
}

/// Parse the series of "demands" (e.g. similar to methods in a rust trait), for an ability definition.
fn finish_parsing_ability_def_help<'a>(
    inc_indent: u32,
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
    let ((demand_indent_level, first_demand), mut state) =
        match ability::parse_demand(ability::IndentLevel::PendingMin, inc_indent, arena, state) {
            Ok((_, out, state)) => (out, state),
            Err((p, fail)) => return Err((p, EExpr::Ability(fail, start))),
        };
    demands.push(first_demand);

    loop {
        match ability::parse_demand(
            ability::IndentLevel::Exact,
            demand_indent_level,
            arena,
            state.clone(),
        ) {
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
    start: Position,
    arena: &'a Bump,
    state: State<'a>,
    min_indent: u32,
    inc_indent: u32,
    flags: ExprParseFlags,
    expr_state: ExprState<'a>,
    loc_op: Loc<OperatorOrDef>,
    initial_state: State<'a>,
) -> ParseResult<'a, Stmt<'a>, EExpr<'a>> {
    let (_, spaces_after_op, state) =
        eat_space_loc_comments(EExpr::IndentEnd, arena, state, min_indent, false)?;

    let op = loc_op.value;
    let op_start = loc_op.region.start();
    let op_end = loc_op.region.end();
    let new_start = state.pos();

    match op {
        OperatorOrDef::BinOp(BinOp::When) => unreachable!("the case handled by the caller"),
        OperatorOrDef::BinOp(BinOp::Minus) if expr_state.end != op_start && op_end == new_start => {
            // a `-` is unary if it is preceded by a space and not followed by a space
            parse_negative_term(
                start,
                arena,
                state,
                min_indent,
                inc_indent,
                expr_state,
                flags,
                initial_state,
                loc_op.with_value(BinOp::Minus),
            )
            .map(|(progress, expr, state)| (progress, Stmt::Expr(expr), state))
        }
        OperatorOrDef::BinOp(op) => parse_after_binop(
            start,
            arena,
            state,
            min_indent,
            inc_indent,
            flags,
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
                        let (_, body, state) = parse_block(
                            flags,
                            arena,
                            state,
                            inc_indent,
                            EExpr::IndentEnd,
                            |a, _| a.clone(),
                            Some(spaces_after_op),
                        )?;

                        let alias =
                            ValueDef::Body(arena.alloc(Loc::at(loc, pattern)), arena.alloc(body));
                        (alias, state)
                    }
                    Err(_) => {
                        // this `=` likely occurred inline; treat it as an invalid operator
                        let fail = EExpr::BadOperator(arena.alloc("="), op_start);
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
                            parse_expr_start(flags, None, arena, state, inc_indent)?;

                        // put the spaces from after the operator in front of the call
                        type_ann = type_ann.spaced_before(arena, spaces_after_op.value);
                        (Loc::at(expr_region, pat), type_ann, state)
                    }
                    Err(_) => {
                        // this `=` likely occurred inline; treat it as an invalid operator
                        let fail = EExpr::BadOperator("=", op_start);
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
            inc_indent,
            expr_state,
            loc_op.with_value(kind),
            spaces_after_op.value,
        ),
    }
}

/// Continue parsing terms after we just parsed a binary operator
#[allow(clippy::too_many_arguments)]
fn parse_after_binop<'a>(
    start: Position,
    arena: &'a Bump,
    state: State<'a>,
    min_indent: u32,
    inc_indent: u32,
    flags: ExprParseFlags,
    spaces_after_op: &'a [CommentOrNewline],
    mut expr_state: ExprState<'a>,
    loc_op: Loc<BinOp>,
) -> ParseResult<'a, Expr<'a>, EExpr<'a>> {
    let (right_expr, state) = match parse_term(PARSE_ALL, flags, arena, state.clone(), min_indent) {
        Ok((_, expr, state)) => (expr, state),
        Err((NoProgress, _)) => return Err((MadeProgress, EExpr::TrailingOperator(state.pos()))),
        Err(err) => return Err(err),
    };

    // put the spaces from after the operator in front of the new_expr
    let right_expr = right_expr.spaced_before(arena, spaces_after_op);

    let call_or_left = if !expr_state.arguments.is_empty() {
        let args = std::mem::replace(&mut expr_state.arguments, Vec::new_in(arena));
        to_call(arena, args, expr_state.expr)
    } else {
        expr_state.expr
    };
    expr_state.operators.push((call_or_left, loc_op));
    expr_state.expr = right_expr;
    expr_state.end = state.pos();

    let prev = state.clone();
    match eat_nc_check(EExpr::IndentEnd, arena, state, min_indent, false) {
        Err(_) => {
            expr_state.spaces_after = &[];
            let expr = finalize_expr(expr_state, arena);
            Ok((MadeProgress, expr, prev))
        }
        Ok((_, spaces_after, state)) => {
            expr_state.spaces_after = spaces_after;
            parse_expr_end(
                start, arena, state, min_indent, inc_indent, flags, expr_state, prev,
            )
        }
    }
}

/// We just saw a `,` that we think is part of a backpassing statement.
/// Parse the rest of the statement.
fn parse_stmt_multi_backpassing<'a>(
    mut expr_state: ExprState<'a>,
    flags: ExprParseFlags,
    arena: &'a Bump,
    state: State<'a>,
    min_indent: u32,
) -> ParseResult<'a, Stmt<'a>, EExpr<'a>> {
    // called after parsing the first , in `a, b <- c` (e.g.)

    let parser = move |arena: &'a Bump, state: State<'a>, min_indent: u32| {
        let (sp, sp_before, state) =
            eat_nc_check(EPattern::Start, arena, state, min_indent, false)?;

        let (pat, state) = match crate::pattern::parse_pattern(arena, state, min_indent) {
            Ok((_, out, state)) => (out, state),
            Err((p, fail)) => return Err((p.or(sp), fail)),
        };

        let (sp_after, state) =
            match eat_nc_check(EPattern::IndentEnd, arena, state, min_indent, false) {
                Ok((_, out, state)) => (out, state),
                Err((_, fail)) => return Err((MadeProgress, fail)),
            };

        let pat = pat.spaced_around(arena, sp_before, sp_after);
        Ok((MadeProgress, pat, state))
    };

    let start = state.pos();
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

    let call = if !expr_state.arguments.is_empty() {
        to_call(arena, expr_state.arguments, expr_state.expr)
    } else {
        expr_state.expr
    };

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
    let (ps, spaces_before, state) =
        eat_nc_check(EExpr::IndentEnd, arena, state, min_indent, false)?;

    let (loc_body, state) = match parse_expr_start(flags, None, arena, state, min_indent) {
        Ok((_, out, state)) => (out, state),
        Err((pe, fail)) => return Err((ps.or(pe), fail)),
    };

    let loc_body = loc_body.spaced_before(arena, spaces_before);
    let ret = Stmt::Backpassing(patterns.into_bump_slice(), arena.alloc(loc_body));
    Ok((MadeProgress, ret, state))
}

// todo: @ask what is the difference with `parse_negative_number`?
/// We just saw a unary negation operator, and now we need to parse the expression.
#[allow(clippy::too_many_arguments)]
fn parse_negative_term<'a>(
    start: Position,
    arena: &'a Bump,
    state: State<'a>,
    min_indent: u32,
    inc_indent: u32,
    mut expr_state: ExprState<'a>,
    flags: ExprParseFlags,
    initial_state: State<'a>,
    loc_op: Loc<BinOp>,
) -> ParseResult<'a, Expr<'a>, EExpr<'a>> {
    let (_, negated_expr, state) = parse_term(PARSE_DEFAULT, flags, arena, state, min_indent)?;

    let arg = numeric_negate_expr(
        arena,
        &initial_state,
        loc_op.region,
        negated_expr,
        expr_state.spaces_after,
    );
    expr_state.arguments.push(arena.alloc(arg));
    expr_state.end = state.pos();

    let initial_state = state.clone();
    let (spaces, state) =
        match eat_nc_check(EExpr::IndentEnd, arena, state.clone(), min_indent, false) {
            Ok((_, spaces, state)) => (spaces, state),
            Err(_) => (&[] as &[_], state),
        };
    expr_state.spaces_after = spaces;

    // TODO: this should probably be handled in the caller, not here
    parse_expr_end(
        start,
        arena,
        state,
        min_indent,
        inc_indent,
        flags,
        expr_state,
        initial_state,
    )
}

/// Parse an expression, not allowing `if`/`when`/etc.
/// TODO: this should probably be subsumed into `expr_operator_chain`
/// If Ok always returns MadeProgress
#[allow(clippy::too_many_arguments)]
fn parse_expr_end<'a>(
    start: Position,
    arena: &'a Bump,
    state: State<'a>,
    min_indent: u32,
    inc_indent: u32,
    flags: ExprParseFlags,
    mut expr_state: ExprState<'a>,
    initial_state: State<'a>,
) -> ParseResult<'a, Expr<'a>, EExpr<'a>> {
    let term_res = if state.column() >= inc_indent {
        parse_term(PARSE_UNDERSCORE, flags, arena, state.clone(), inc_indent)
    } else {
        Err((NoProgress, EExpr::Start(state.pos())))
    };

    match term_res {
        Err((MadeProgress, f)) => Err((MadeProgress, f)),
        Ok((_, mut arg, state)) => {
            // now that we have `function arg1 ... <spaces> argn`, attach the spaces to the `argn`
            if !expr_state.spaces_after.is_empty() {
                arg = arg.spaced_before(arena, expr_state.spaces_after);
                expr_state.spaces_after = &[];
            }
            expr_state.arguments.push(arena.alloc(arg));
            expr_state.end = state.pos();

            let initial_state = state.clone();
            match eat_nc_check(EExpr::IndentEnd, arena, state.clone(), min_indent, false) {
                Err(_) => {
                    expr_state.spaces_after = &[];
                    let expr = finalize_expr(expr_state, arena);
                    Ok((MadeProgress, expr, state))
                }
                Ok((_, spaces_after, state)) => {
                    expr_state.spaces_after = spaces_after;
                    parse_expr_end(
                        start,
                        arena,
                        state,
                        min_indent,
                        inc_indent,
                        flags,
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
            match parse_bin_op(MadeProgress, state) {
                Err((MadeProgress, f)) => Err((MadeProgress, f)),
                Ok((_, op, state)) => {
                    let op_start = before_op.pos();
                    let op_end = state.pos();

                    expr_state.consume_spaces(arena);

                    if let BinOp::When = op {
                        // the chain of operators finishes at the when statement,
                        // so their value will be the condition/value for the when pattern matching
                        let ops_val = finalize_expr(expr_state, arena);
                        let ops_val = Loc::pos(start, state.pos(), ops_val);

                        let when_pos = state.pos();
                        let cond = Some((ops_val, WhenShortcut::BinOp));
                        match when::rest_of_when_expr(cond, flags, arena, state, min_indent) {
                            Ok(ok) => Ok(ok),
                            Err((p, fail)) => Err((p, EExpr::When(fail, when_pos))),
                        }
                    } else {
                        let (_, spaces_after_op, state) =
                            eat_nc_check(EExpr::IndentEnd, arena, state, min_indent, false)?;

                        // a `-` is unary if it is preceded by a space and not followed by a space
                        let loc_op = Loc::pos(op_start, op_end, op);
                        match op {
                            BinOp::Minus if expr_state.end != op_start && op_end == state.pos() => {
                                parse_negative_term(
                                    start, arena, state, min_indent, inc_indent, expr_state, flags,
                                    before_op, loc_op,
                                )
                            }
                            _ => parse_after_binop(
                                start,
                                arena,
                                state,
                                min_indent,
                                inc_indent,
                                flags,
                                spaces_after_op,
                                expr_state,
                                loc_op,
                            ),
                        }
                    }
                }
                Err((NoProgress, _)) => {
                    // roll back space parsing
                    let expr = finalize_expr(expr_state, arena);
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
    inc_indent: u32,
) -> Result<(TypeDef<'a>, State<'a>), (Progress, EExpr<'a>)> {
    // This is an ability definition, `Ability arg1 ... implements ...`.

    let name = expr_state.expr.map_owned(|e| match e {
        Expr::Tag(name) => name,
        _ => unreachable!(),
    });

    let mut args = Vec::with_capacity_in(expr_state.arguments.len(), arena);
    for arg in expr_state.arguments {
        match expr_to_pattern_help(arena, &arg.value) {
            Ok(pat) => {
                args.push(Loc::at(arg.region, pat));
            }
            Err(_) => {
                let start = arg.region.start();
                let fail = &*arena.alloc(EPattern::Start(start));
                return Err((MadeProgress, EExpr::Pattern(fail, arg.region.start())));
            }
        }
    }

    // Attach any spaces to the `implements` keyword
    let implements = Loc::at(implements.region, Implements::Implements)
        .spaced_before(arena, expr_state.spaces_after);

    let args = args.into_bump_slice();
    let (_, (type_def, _), state) =
        finish_parsing_ability_def_help(inc_indent, name, args, implements, arena, state)?;

    Ok((type_def, state))
}

pub fn parse_expr_block<'a>(
    flags: ExprParseFlags,
    arena: &'a Bump,
    state: State<'a>,
    min_indent: u32,
) -> ParseResult<'a, Loc<Expr<'a>>, EExpr<'a>> {
    let start = state.pos();
    let (_, stmts, state) = parse_stmt_seq(
        arena,
        state,
        |fail, _| fail.clone(),
        flags,
        min_indent,
        Loc::pos(start, start, &[]),
        EExpr::IndentStart,
    )?;

    let err_pos = state.pos();
    if stmts.is_empty() {
        let fail = arena.alloc(EExpr::Start(err_pos)).clone();
        return Err((NoProgress, fail));
    }

    let expr = stmts_to_expr(&stmts, arena).map_err(|e| (MadeProgress, arena.alloc(e).clone()))?;

    let (_, spaces_after, state) = eat_nc_check(EExpr::IndentEnd, arena, state, min_indent, true)?;

    let expr = expr.spaced_after(arena, spaces_after);
    Ok((MadeProgress, expr, state))
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
        Expr::Var {
            module_name, ident, ..
        } => {
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
        | Expr::RecordAccess(_, _, _)
        | Expr::TupleAccess(_, _, _)
        | Expr::List { .. }
        | Expr::Closure(_, _, _)
        | Expr::Backpassing(_, _, _)
        | Expr::BinOps { .. }
        | Expr::Defs(_, _)
        | Expr::If { .. }
        | Expr::When(..)
        | Expr::Expect(_, _)
        | Expr::Dbg
        | Expr::DbgStmt(_, _)
        | Expr::LowLevelDbg(_, _, _)
        | Expr::Return(_, _)
        | Expr::MalformedClosure
        | Expr::MalformedSuffixed(..)
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
        AssignedField::Malformed(string) => Pattern::Malformed(string),
        AssignedField::IgnoredValue(_, _, _) => return Err(()),
    })
}

pub fn parse_top_level_defs<'a>(
    arena: &'a bumpalo::Bump,
    state: State<'a>,
    output: Defs<'a>,
) -> ParseResult<'a, Defs<'a>, EExpr<'a>> {
    let (_, (first_spaces, sp_at), state) = eat_nc(arena, state, false)?;

    let (_, stmts, state) = parse_stmt_seq(
        arena,
        state,
        |e, _| e.clone(),
        CHECK_FOR_ARROW | ACCEPT_MULTI_BACKPASSING,
        0,
        Loc::at(sp_at, first_spaces),
        EExpr::IndentEnd,
    )?;

    let (_, (last_spaces, _), state) = eat_nc(arena, state, false)?;

    let existing_len = output.tags.len();

    let (mut output, last_expr) =
        stmts_to_defs(&stmts, output, false, arena).map_err(|e| (MadeProgress, e))?;

    if let Some(expr) = last_expr {
        let fail = EExpr::UnexpectedTopLevelExpr(expr.region.start());
        return Err((MadeProgress, fail));
    }

    if output.tags.len() > existing_len {
        let after = slice_extend_new(&mut output.spaces, last_spaces.iter().copied());
        let last = output.tags.len() - 1;
        debug_assert!(output.space_after[last].is_empty() || after.is_empty());
        output.space_after[last] = after;
    }

    Ok((MadeProgress, output, state))
}

thread_local! {
    // we use a thread_local here so that tests consistently give the same pattern
    static SUFFIXED_ANSWER_COUNTER: Cell<usize> = const { Cell::new(0) };
}

// Took this approach from the `next_unique_suffixed_ident`.
fn next_unique_closure_shortcut_arg(arena: &'_ Bump) -> &str {
    SUFFIXED_ANSWER_COUNTER.with(|counter| {
        let count = counter.get();
        counter.set(count + 1);
        arena.alloc(format!("nu{}", count))
    })
}

pub(crate) fn reset_unique_closure_shortcut_arg_generator() {
    SUFFIXED_ANSWER_COUNTER.with(|counter| {
        counter.set(0);
    });
}

// todo: @ask is it convention too smart, get feedback from the real usage
// For the space guided format of the closure shortcut, we use a nice short name for the argument,
// it is no need to be unique and expected to be modified by the User
pub const FMT_CLOSURE_SHORTCUT_ARG: &str = "x";

/// If Ok it always returns MadeProgress
fn rest_of_closure<'a>(
    flags: ExprParseFlags,
    arena: &'a Bump,
    mut state: State<'a>,
) -> ParseResult<'a, Expr<'a>, EClosure<'a>> {
    // After the first token, all other tokens must be indented past the start of the line
    let slash_indent = state.line_indent();
    if slash_indent > state.column() {
        return Err((NoProgress, EClosure::Start(state.pos())));
    }

    let after_slash = state.pos();

    // @feat entry point for parsing the closure-shortcut
    // Expand the shortcut into the full code guided by presence of space after slash (only if actual shortcut follows)
    // e.g. this one with th space after slash `\ .foo.bar` will expand to `\x -> x.foo.bar`, but this one `\.foo.bar` will stay as-is in the formatted output
    let mut fmt_keep_shortcut = true;
    if state.bytes().first() == Some(&b' ') {
        state.advance_mut(1);
        fmt_keep_shortcut = false;
    };

    // Parses `\.foo.bar + 1` into `\p -> p.foo.bar + 1`
    if state.bytes().first() == Some(&b'.') {
        let ident = if fmt_keep_shortcut {
            next_unique_closure_shortcut_arg(arena)
        } else {
            FMT_CLOSURE_SHORTCUT_ARG
        };

        let param = Loc::at(Region::point(after_slash), Pattern::Identifier { ident });
        let mut params = Vec::with_capacity_in(1, arena);
        params.push(param);

        let mut parts = Vec::with_capacity_in(2, arena);
        parts.push(Accessor::RecordField(ident));

        let ident_state = state.clone();
        let bytes = state.bytes();
        let pos = state.pos();
        let module_name = "";
        let (ident, state) = match chomp_access_chain(bytes, &mut parts) {
            Ok(width) => {
                let parts = parts.into_bump_slice();
                let ident = Ident::Access { module_name, parts };
                (ident, state.advance(width))
            }
            Err(1) => {
                // Handling the identity function `\.`, where the `.` was found but nothing after it, therefore the width is 1.
                (Ident::Plain(ident), state.inc())
            }
            Err(width) => {
                let fail = BadIdent::WeirdDotAccess(pos.bump_offset(width));
                malformed_ident(bytes, fail, state.advance(width))
            }
        };

        let ident_end = state.pos();
        let (suffixes, state) = match parse_field_task_result_suffixes(arena, state) {
            Ok((_, out, state)) => (out, state),
            Err((_, fail)) => {
                return Err((MadeProgress, EClosure::Body(arena.alloc(fail), ident_end)))
            }
        };

        let mut closure_shortcut = None;
        if fmt_keep_shortcut {
            closure_shortcut = Some(AccessShortcut::Closure);
        }
        let mut ident = ident_to_expr(arena, ident, closure_shortcut);
        if !suffixes.is_empty() {
            ident = apply_expr_access_chain(arena, ident, suffixes);
        }
        let ident = Loc::pos(after_slash, ident_end, ident);

        // todo: @wip for the identity function and Unary Negate and Not, should we consider to stop here and return early
        // to avoid putting the lambda in parens in the middle of the Apply list,
        // e.g, instead of `Foo.bar (\.) 42` it enable us to write `Foo.bar \. 42`

        // todo: @wip current RecordAccessor '.foo { foo: 3 }', RecordUpdater '&foo { foo: 1 } 2' functions are stopped here,
        // basically implying the parens. I need to test the same cases with the closure shortcut.

        let err_pos = state.pos();
        let inc_indent: u32 = slash_indent + 1;
        let (body, state) =
            match parse_expr_start(flags, Some((ident_state, ident)), arena, state, inc_indent) {
                Ok((_, out, state)) => (out, state),
                Err((_, fail)) => {
                    return Err((MadeProgress, EClosure::Body(arena.alloc(fail), err_pos)))
                }
            };

        let mut shortcut = None;
        if fmt_keep_shortcut {
            shortcut = Some(ClosureShortcut::Access)
        }
        let closure = Expr::Closure(params.into_bump_slice(), arena.alloc(body), shortcut);
        return Ok((MadeProgress, closure, state));
    }

    // Either pipe shortcut `\|> f` into `\p -> p |> f`, or the rest of BinOp's, e.g. `\+ 1` into `\p -> p + 1`,
    // Excluding the operators for which the shortcut does not make sense, assignment '=', Type Alias ':', ':=', etc.
    if let Ok((_, binop, state)) = parse_bin_op(NoProgress, state.clone()) {
        let ident = if fmt_keep_shortcut {
            next_unique_closure_shortcut_arg(arena)
        } else {
            FMT_CLOSURE_SHORTCUT_ARG
        };

        let after_binop = state.pos();
        let param = Pattern::Identifier { ident };
        let param = Loc::pos(after_slash, after_binop, param);
        let mut params = Vec::with_capacity_in(1, arena);
        params.push(param);

        let inc_indent = slash_indent + 1;

        // a single closure parameter is the left value of the binary operator
        let left_var = Expr::new_var("", ident);
        let left_var = Loc::pos(after_slash, after_binop, left_var);

        // special handling of the `~` when operator
        let (body, state, what) = if binop == BinOp::When {
            let what = if fmt_keep_shortcut {
                WhenShortcut::Closure
            } else {
                WhenShortcut::BinOp
            };
            let cond = Some((left_var, what));
            match when::rest_of_when_expr(cond, flags, arena, state, inc_indent) {
                Ok((_, out, state)) => (out, state, ClosureShortcut::WhenBinOp),
                Err((_, fail)) => {
                    let fail = EExpr::When(fail, after_binop);
                    return Err((MadeProgress, EClosure::Body(arena.alloc(fail), after_binop)));
                }
            }
        } else {
            // "normal" handling of the rest of the binary operators
            let expr_state = ExprState {
                operators: Vec::new_in(arena),
                arguments: Vec::new_in(arena),
                expr: left_var,
                spaces_after: &[],
                end: after_slash,
            };

            let loc_op = Loc::pos(after_slash, after_binop, binop);
            let (spaces_after_op, state) =
                match eat_nc_check(EExpr::IndentEnd, arena, state, inc_indent, false) {
                    Ok((_, out, state)) => (out, state),
                    Err((_, fail)) => {
                        return Err((MadeProgress, EClosure::Body(arena.alloc(fail), after_binop)))
                    }
                };

            match parse_after_binop(
                after_slash,
                arena,
                state,
                inc_indent,
                inc_indent,
                flags,
                spaces_after_op,
                expr_state,
                loc_op,
            ) {
                Ok((_, out, state)) => (out, state, ClosureShortcut::BinOp),
                Err((_, fail)) => {
                    return Err((MadeProgress, EClosure::Body(arena.alloc(fail), after_binop)))
                }
            }
        };

        let body = Loc::pos(after_binop, state.pos(), body);

        let shortcut = if fmt_keep_shortcut { Some(what) } else { None };
        let closure = Expr::Closure(params.into_bump_slice(), arena.alloc(body), shortcut);
        return Ok((MadeProgress, closure, state));
    }

    // Parse the params, params are comma-separated
    let inc_indent: u32 = slash_indent + 1;
    let param_pos = state.pos();
    let (spaces_before, state) =
        match eat_nc_check(EClosure::IndentArg, arena, state, inc_indent, false) {
            Ok((_, out, state)) => (out, state),
            Err((NoProgress, _)) => return Err((MadeProgress, EClosure::Arg(param_pos))),
            Err(err) => return Err(err),
        };

    let param_ident_pos = state.pos();
    let (param, state) = match parse_closure_param(arena, state, inc_indent) {
        Ok((_, out, state)) => (out, state),
        Err((NoProgress, _)) => return Err((MadeProgress, EClosure::Arg(param_pos))),
        Err((_, fail)) => return Err((MadeProgress, EClosure::Pattern(fail, param_ident_pos))),
    };

    let (spaces_after, state) =
        match eat_nc_check(EClosure::IndentArrow, arena, state, inc_indent, false) {
            Ok((_, out, state)) => (out, state),
            Err((NoProgress, _)) => return Err((MadeProgress, EClosure::Arg(param_pos))),
            Err(err) => return Err(err),
        };

    let first_param = param.spaced_around(arena, spaces_before, spaces_after);
    let mut params = Vec::with_capacity_in(1, arena);
    params.push(first_param);

    let mut state = state;
    loop {
        if state.bytes().first() != Some(&b',') {
            break;
        }
        state.advance_mut(1);

        // After delimiter found, parse the parameter
        let param_pos = state.pos();
        let (spaces_before, next_state) =
            match eat_nc_check(EClosure::IndentArg, arena, state, inc_indent, false) {
                Ok((_, out, state)) => (out, state),
                Err((NoProgress, _)) => return Err((MadeProgress, EClosure::Arg(param_pos))),
                Err(err) => return Err(err),
            };

        let param_ident_pos = next_state.pos();
        let (param, next_state) = match parse_closure_param(arena, next_state, inc_indent) {
            Ok((_, out, state)) => (out, state),
            Err((NoProgress, _)) => return Err((MadeProgress, EClosure::Arg(param_pos))),
            Err((_, fail)) => return Err((MadeProgress, EClosure::Pattern(fail, param_ident_pos))),
        };

        let (spaces_after, next_state) =
            match eat_nc_check(EClosure::IndentArrow, arena, next_state, inc_indent, false) {
                Ok((_, out, state)) => (out, state),
                Err((NoProgress, _)) => return Err((MadeProgress, EClosure::Arg(param_pos))),
                Err(err) => return Err(err),
            };

        let next_param = param.spaced_around(arena, spaces_before, spaces_after);
        params.push(next_param);
        state = next_state;
    }

    // Parse the arrow which separates params from body, only then parse the body
    if !state.bytes().starts_with(b"->") {
        return Err((MadeProgress, EClosure::Arrow(state.pos())));
    }
    state.advance_mut(2);

    let body_indent = state.line_indent() + 1;
    let (_, first_nl, state) =
        eat_space_loc_comments(EClosure::IndentBody, arena, state, body_indent, true)?;

    let (body, state) = if first_nl.value.is_empty() {
        let err_pos = state.pos();
        match parse_expr_start(flags, None, arena, state, inc_indent) {
            Ok((_, out, state)) => (out, state),
            Err((_, fail)) => {
                return Err((MadeProgress, EClosure::Body(arena.alloc(fail), err_pos)))
            }
        }
    } else {
        let (_, stmts, state) = parse_stmt_seq(
            arena,
            state,
            EClosure::Body,
            flags,
            inc_indent,
            Loc::at(first_nl.region, &[]),
            EClosure::IndentBody,
        )?;

        let err_pos = state.pos();
        if stmts.is_empty() {
            let fail = EClosure::Body(arena.alloc(EExpr::Start(err_pos)), err_pos);
            return Err((MadeProgress, fail));
        }

        match stmts_to_expr(&stmts, arena) {
            Ok(out) => (out.spaced_before(arena, first_nl.value), state),
            Err(fail) => return Err((MadeProgress, EClosure::Body(arena.alloc(fail), err_pos))),
        }
    };

    let closure = Expr::Closure(params.into_bump_slice(), arena.alloc(body), None);
    Ok((MadeProgress, closure, state))
}

mod when {
    use super::*;
    use crate::{
        ast::{WhenBranch, WhenShortcut},
        blankspace::eat_nc,
    };

    /// If Ok it always returns MadeProgress
    pub fn rest_of_when_expr<'a>(
        cond: Option<(Loc<Expr<'a>>, WhenShortcut)>,
        flags: ExprParseFlags,
        arena: &'a Bump,
        state: State<'a>,
        min_indent: u32,
    ) -> ParseResult<'a, Expr<'a>, EWhen<'a>> {
        let (shortcut, cond, state) = if let Some((cond, what)) = cond {
            (Some(what), cond, state)
        } else {
            let (_, spaces_before, state) =
                eat_nc_check(EWhen::IndentCondition, arena, state, min_indent, true)?;

            let at_cond = state.pos();
            let (cond, state) = match parse_expr_start(flags, None, arena, state, min_indent) {
                Ok((_, out, state)) => (out, state),
                Err((_, fail)) => {
                    return Err((MadeProgress, EWhen::Condition(arena.alloc(fail), at_cond)))
                }
            };

            let (_, (spaces_after, _), state) = eat_nc(arena, state, true)?;
            let cond = cond.spaced_around(arena, spaces_before, spaces_after);

            if !at_keyword(keyword::IS, &state) {
                return Err((MadeProgress, EWhen::Is(state.pos())));
            }

            (None, cond, state.advance(keyword::IS.len()))
        };

        // Note that we allow the `is` to be at any indent level, since this doesn't introduce any
        // ambiguity. The formatter will fix it up.
        // We require that branches are indented relative to the line containing the `is`.
        let branch_indent = state.line_indent() + 1;

        // 1. Parse the first branch and get its indentation level (it must be >= branch_indent).
        // 2. Parse the other branches. Their indentation levels must be == the first branch's.
        let (((pattern_indent, first_patterns), guard), state) =
            match parse_branch_alternatives(flags, None, arena, state, branch_indent) {
                Ok((_, out, state)) => (out, state),
                Err((_, fail)) => return Err((MadeProgress, fail)),
            };

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
                flags,
                Some(pattern_indent),
                arena,
                state.clone(),
                branch_indent,
            ) {
                Ok((_, ((indent_column, patterns), guard), g_state)) => {
                    if pattern_indent == indent_column {
                        let (_, value, next_state) = parse_branch_result(arena, g_state)?;

                        let branch = WhenBranch {
                            patterns: patterns.into_bump_slice(),
                            value,
                            guard,
                        };
                        branches.push(arena.alloc(branch));
                        state = next_state;
                    } else {
                        let indent = pattern_indent - indent_column;
                        let fail = EWhen::PatternAlignment(indent, g_state.pos());
                        return Err((MadeProgress, fail));
                    }
                }
                Err((NoProgress, _)) => break,
                Err(err) => return Err(err),
            }
        }

        let when = Expr::When(arena.alloc(cond), branches.into_bump_slice(), shortcut);
        Ok((MadeProgress, when, state))
    }

    /// Parsing alternative patterns in `when` branches.
    #[allow(clippy::type_complexity)]
    fn parse_branch_alternatives<'a>(
        flags: ExprParseFlags,
        pattern_indent: Option<u32>,
        arena: &'a Bump,
        state: State<'a>,
        min_indent: u32,
    ) -> ParseResult<'a, ((u32, Vec<'a, Loc<Pattern<'a>>>), Option<Loc<Expr<'a>>>), EWhen<'a>> {
        let flags = flags.unset(CHECK_FOR_ARROW);

        // put no restrictions on the indent after the spaces; we'll check it manually
        let (_, (indent_spaces, _), state) = eat_nc(arena, state, false)?;

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

        let pat_indent = min_indent.max(pattern_indent.unwrap_or(min_indent));

        let (sp, spaces_before, state) =
            match eat_nc_check(EWhen::IndentPattern, arena, state, pat_indent, false) {
                Ok(ok) => ok,
                Err((_, fail)) => return Err((NoProgress, fail)),
            };

        let pattern_pos = state.pos();
        let (pattern, state) = match crate::pattern::parse_pattern(arena, state, pat_indent) {
            Ok((_, out, state)) => (out, state),
            Err((ep, fail)) => return Err((ep.or(sp), EWhen::Pattern(fail, pattern_pos))),
        };

        let (_, spaces_after, mut state) =
            eat_nc_check(EWhen::IndentPattern, arena, state, pat_indent, true)?;

        let first_pattern = pattern.spaced_around(arena, spaces_before, spaces_after);
        let mut patterns = Vec::with_capacity_in(1, arena);
        patterns.push(first_pattern);

        loop {
            let prev_state = state.clone();
            if state.bytes().first() == Some(&b'|') {
                state.advance_mut(1);

                let (_, spaces_before, next_state) =
                    eat_nc_check(EWhen::IndentPattern, arena, state, pat_indent, true)?;

                let pattern_pos = next_state.pos();
                let (pat, next_state) =
                    match crate::pattern::parse_pattern(arena, next_state, pat_indent) {
                        Ok((_, out, state)) => (out, state),
                        Err((_, fail)) => {
                            return Err((MadeProgress, EWhen::Pattern(fail, pattern_pos)))
                        }
                    };

                let (_, spaces_after, next_state) =
                    eat_nc_check(EWhen::IndentPattern, arena, next_state, pat_indent, true)?;

                let pattern = pat.spaced_around(arena, spaces_before, spaces_after);
                patterns.push(pattern);
                state = next_state;
            } else {
                state = prev_state;
                break;
            }
        }

        // tag spaces onto the first parsed pattern
        if let Some(first) = patterns.get_mut(0) {
            *first = (*first).spaced_before(arena, indent_spaces);
        }

        let column_patterns = (pattern_column, patterns);
        let original_state = state.clone();

        if !at_keyword(keyword::IF, &state) {
            return Ok((MadeProgress, (column_patterns, None), original_state));
        }
        state.advance_mut(keyword::IF.len());

        // TODO we should require space before the expression but not after
        let (_, spaces_before, state) =
            eat_nc_check(EWhen::IndentIfGuard, arena, state, min_indent, true)?;

        let guard_pos = state.pos();
        let (_, guard, state) = parse_expr_start(flags, None, arena, state, min_indent + 1)
            .map_err(|(_, fail)| (MadeProgress, EWhen::IfGuard(arena.alloc(fail), guard_pos)))?;

        let (_, spaces_after, state) =
            eat_nc_check(EWhen::IndentArrow, arena, state, min_indent, true)?;

        let guard = guard.spaced_around(arena, spaces_before, spaces_after);
        Ok((MadeProgress, (column_patterns, Some(guard)), state))
    }

    /// Parsing the righthandside of a branch in a when conditional.
    fn parse_branch_result<'a>(
        arena: &'a Bump,
        state: State<'a>,
    ) -> ParseResult<'a, Loc<Expr<'a>>, EWhen<'a>> {
        if !state.bytes().starts_with(b"->") {
            return Err((MadeProgress, EWhen::Arrow(state.pos())));
        }
        let state = state.advance(2);
        let inc_indent = state.line_indent() + 1;
        match parse_block(
            CHECK_FOR_ARROW | ACCEPT_MULTI_BACKPASSING,
            arena,
            state,
            inc_indent,
            EWhen::IndentBranch,
            EWhen::Branch,
            None,
        ) {
            Ok((_, value, state)) => Ok((MadeProgress, value, state)),
            Err((_, fail)) => Err((MadeProgress, fail)),
        }
    }
}

fn rest_of_expect_stmt<'a>(
    is_fx: bool,
    start: Position,
    flags: ExprParseFlags,
    preceding_comment: Region,
    arena: &'a Bump,
    state: State<'a>,
) -> ParseResult<'a, Loc<Stmt<'a>>, EExpr<'a>> {
    let inc_indent = state.line_indent() + 1;
    match parse_block(
        flags,
        arena,
        state,
        inc_indent,
        EExpect::IndentCondition,
        EExpect::Condition,
        None,
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

fn rest_of_return_stmt<'a>(
    start: Position,
    flags: ExprParseFlags,
    arena: &'a Bump,
    state: State<'a>,
) -> ParseResult<'a, Loc<Stmt<'a>>, EExpr<'a>> {
    let inc_indent = state.line_indent() + 1;
    match parse_block(
        flags,
        arena,
        state,
        inc_indent,
        EReturn::IndentReturnValue,
        EReturn::ReturnValue,
        None,
    ) {
        Ok((_, expr, state)) => {
            let expr = Loc::pos(start, expr.region.end(), expr.value);
            let stmt = Stmt::Expr(Expr::Return(arena.alloc(expr), None));
            Ok((MadeProgress, Loc::pos(start, state.pos(), stmt), state))
        }
        Err((_, fail)) => Err((MadeProgress, EExpr::Return(fail, start))),
    }
}

fn rest_of_dbg_stmt<'a>(
    start: Position,
    flags: ExprParseFlags,
    preceding_comment: Region,
    arena: &'a Bump,
    state: State<'a>,
) -> ParseResult<'a, Loc<Stmt<'a>>, EExpr<'a>> {
    let inc_indent = state.line_indent() + 1;
    match parse_block(
        flags,
        arena,
        state,
        inc_indent,
        EExpect::IndentCondition,
        EExpect::Condition,
        None,
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

fn parse_module_import<'a>(
    arena: &'a Bump,
    state: State<'a>,
    min_indent: u32,
) -> ParseResult<'a, ModuleImport<'a>, EImport<'a>> {
    let (_, before_name, state) =
        eat_nc_check(EImport::IndentStart, arena, state, min_indent, false)?;

    let name_pos = state.pos();
    let (_, name, state) = parse_imported_module_name(state)?;
    let name = Loc::pos(name_pos, state.pos(), name);

    let params_pos = state.pos();
    let (params, state) = match parse_import_params(arena, state.clone(), min_indent) {
        Ok((_, out, state)) => (Some(out), state),
        Err((NoProgress, _)) => (None, state),
        Err((_, fail)) => return Err((MadeProgress, EImport::Params(fail, params_pos))),
    };

    let (alias, state) = match import_as().parse(arena, state.clone(), min_indent) {
        Ok((_, out, state)) => (Some(out), state),
        Err((NoProgress, _)) => (None, state),
        Err(err) => return Err(err),
    };

    let (exposed, state) = match import_exposing().parse(arena, state.clone(), min_indent) {
        Ok((_, out, state)) => (Some(out), state),
        Err((NoProgress, _)) => (None, state),
        Err(err) => return Err(err),
    };

    let import = ModuleImport {
        before_name,
        name,
        params,
        alias,
        exposed,
    };
    Ok((MadeProgress, import, state))
}

fn rest_of_import<'a>(
    start: Position,
    arena: &'a Bump,
    state: State<'a>,
    min_indent: u32,
) -> ParseResult<'a, Loc<Stmt<'a>>, EExpr<'a>> {
    let inc_indent = min_indent + 1;
    let (vd, state) = match parse_module_import(arena, state.clone(), inc_indent) {
        Ok((_, module_import, state)) => (ValueDef::ModuleImport(module_import), state),
        Err((MadeProgress, fail)) => return Err((MadeProgress, EExpr::Import(fail, start))),
        Err(_) => {
            let (before_path, state) =
                match eat_nc_check(EImport::IndentStart, arena, state, inc_indent, false) {
                    Ok((_, sp, state)) => (sp, state),
                    Err((_, fail)) => return Err((MadeProgress, EExpr::Import(fail, start))),
                };

            let path_pos = state.pos();
            let (path, state) =
                match string_literal::parse_str_literal().parse(arena, state, inc_indent) {
                    Ok((_, out, state)) => (out, state),
                    Err(_) => {
                        let fail = EImport::IngestedPath(path_pos);
                        return Err((MadeProgress, EExpr::Import(fail, start)));
                    }
                };

            let path = Loc::pos(path_pos, state.pos(), path);

            let (name, state) = match import_ingested_file_as(arena, state, inc_indent) {
                Ok((_, out, state)) => (out, state),
                Err((_, fail)) => return Err((MadeProgress, EExpr::Import(fail, start))),
            };

            let (annotation, state) =
                match import_ingested_file_annotation().parse(arena, state.clone(), inc_indent) {
                    Ok((_, out, state)) => (Some(out), state),
                    Err((NoProgress, _)) => (None, state),
                    Err((_, fail)) => return Err((MadeProgress, EExpr::Import(fail, start))),
                };

            let import = IngestedFileImport {
                before_path,
                path,
                name,
                annotation,
            };
            (ValueDef::IngestedFileImport(import), state)
        }
    };

    let has_reached_new_line_or_eof = state.has_reached_end();
    let (spaces_after, _) =
        match eat_nc_check(EImport::EndNewline, arena, state.clone(), min_indent, false) {
            Ok((_, out, state)) => (out, state),
            Err((_, fail)) => return Err((MadeProgress, EExpr::Import(fail, start))),
        };

    if !has_reached_new_line_or_eof && spaces_after.is_empty() {
        let fail = EImport::EndNewline(state.pos());
        return Err((MadeProgress, EExpr::Import(fail, start)));
    }

    let stmt = Loc::pos(start, state.pos(), Stmt::ValueDef(vd));
    Ok((MadeProgress, stmt, state))
}

/// If Ok it always returns MadeProgress
fn rest_of_if_expr<'a>(
    flags: ExprParseFlags,
    arena: &'a Bump,
    state: State<'a>,
    min_indent: u32,
) -> ParseResult<'a, Expr<'a>, EIf<'a>> {
    let if_indent = state.line_indent();

    let mut branches = Vec::with_capacity_in(1, arena);
    let mut loop_state = state;

    let at_final_else = loop {
        let (_, spaces_before_cond, state) = eat_nc_check(
            EIf::IndentCondition,
            arena,
            loop_state.clone(),
            min_indent,
            false,
        )?;

        let cond_pos = state.pos();
        let (cond, state) = match parse_expr_start(
            CHECK_FOR_ARROW | ACCEPT_MULTI_BACKPASSING,
            None,
            arena,
            state,
            min_indent,
        ) {
            Ok((_, out, state)) => (out, state),
            Err((p, fail)) => return Err((p, EIf::Condition(arena.alloc(fail), cond_pos))),
        };

        let (_, spaces_after_cond, mut state) =
            eat_nc_check(EIf::IndentThenToken, arena, state.clone(), min_indent, true)?;

        if !at_keyword(keyword::THEN, &state) {
            return Err((MadeProgress, EIf::Then(state.pos())));
        }
        state.advance_mut(keyword::THEN.len());

        let cond = cond.spaced_around(arena, spaces_before_cond, spaces_after_cond);

        let (_, then_expr, state) = parse_block(
            CHECK_FOR_ARROW | ACCEPT_MULTI_BACKPASSING,
            arena,
            state,
            0,
            EIf::IndentThenBranch,
            EIf::ThenBranch,
            None,
        )
        .map_err(|(_, fail)| (MadeProgress, fail))?;

        let (_, spaces_after_then, state) =
            eat_nc_check(EIf::IndentElseToken, arena, state, min_indent, true)?;

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
        if let Ok((_, _, state)) =
            eat_nc_check(EIf::IndentIf, arena, state.clone(), min_indent, false)
        {
            if at_keyword(keyword::IF, &state) {
                loop_state = state.advance(keyword::IF.len());
                continue;
            }
        }
        break state;
    };

    let else_indent = at_final_else.line_indent();
    let indented_else = else_indent > if_indent;

    let min_indent = if !indented_else {
        else_indent + 1
    } else {
        if_indent
    };

    let (_, else_branch, state) = parse_block(
        flags,
        arena,
        at_final_else,
        min_indent,
        EIf::IndentElseBranch,
        EIf::ElseBranch,
        None,
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
/// 3. If you pass `first_spaces: None` this function will parse the spaces itself
#[allow(clippy::too_many_arguments)]
fn parse_block<'a, E>(
    flags: ExprParseFlags,
    arena: &'a Bump,
    state: State<'a>,
    min_indent: u32,
    indent_problem: fn(Position) -> E,
    wrap_error: fn(&'a EExpr<'a>, Position) -> E,
    first_spaces: Option<Loc<&'a [CommentOrNewline<'a>]>>,
) -> ParseResult<'a, Loc<Expr<'a>>, E>
where
    E: 'a + SpaceProblem,
{
    let (first_spaces, state) = if let Some(first_spaces) = first_spaces {
        (first_spaces, state)
    } else {
        // if no spaces are provided, parse them here
        let (_, first_spaces, state) =
            eat_space_loc_comments(indent_problem, arena, state, min_indent, false)?;
        (first_spaces, state)
    };

    if !first_spaces.value.is_empty() {
        let (_, stmts, state) = parse_stmt_seq(
            arena,
            state,
            wrap_error,
            flags,
            min_indent,
            Loc::at(first_spaces.region, &[]),
            indent_problem,
        )?;

        let last_pos = state.pos();
        if stmts.is_empty() {
            let fail = wrap_error(arena.alloc(EExpr::Start(last_pos)), last_pos);
            return Err((NoProgress, fail));
        }

        match stmts_to_expr(&stmts, arena) {
            Ok(expr) => {
                let expr = expr.spaced_before(arena, first_spaces.value);
                Ok((MadeProgress, expr, state))
            }
            Err(e) => Err((MadeProgress, wrap_error(arena.alloc(e), last_pos))),
        }
    } else {
        let prev_pos = state.pos();
        match parse_expr_start(flags, None, arena, state, min_indent) {
            Ok((_, expr, state)) => {
                let expr = expr.spaced_before(arena, first_spaces.value);
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
    flags: ExprParseFlags,
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
            match parse_stmt_start(flags, last_space.region, arena, state.clone(), min_indent) {
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

        match eat_space_loc_comments(indent_problem, arena, state.clone(), min_indent, false) {
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
            Stmt::Expr(Expr::Return(return_value, _after_return)) => {
                if i == stmts.len() - 1 {
                    last_expr = Some(Loc::at_zero(Expr::Return(return_value, None)));
                } else {
                    let rest = stmts_to_expr(&stmts[i + 1..], arena)?;
                    last_expr = Some(Loc::at_zero(Expr::Return(
                        return_value,
                        Some(arena.alloc(rest)),
                    )));
                }

                // don't re-process the rest of the statements, they got consumed by the early return
                break;
            }
            Stmt::Expr(e) => {
                if i + 1 < stmts.len() {
                    defs.push_value_def(
                        ValueDef::Stmt(arena.alloc(Loc::at(sp_stmt.item.region, e))),
                        sp_stmt.item.region,
                        sp_stmt.before,
                        &[],
                    );
                } else {
                    let e = if sp_stmt.before.is_empty() {
                        e
                    } else {
                        arena.alloc(e).before(sp_stmt.before)
                    };

                    last_expr = Some(sp_stmt.item.with_value(e));
                }
            }
            Stmt::Backpassing(pats, call) => {
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

fn ident_to_expr<'a>(
    arena: &'a Bump,
    src: Ident<'a>,
    shortcut: Option<AccessShortcut>,
) -> Expr<'a> {
    match src {
        Ident::Plain(ident) => Expr::new_var_shortcut("", ident, shortcut),
        Ident::Tag(string) => Expr::Tag(string),
        Ident::OpaqueRef(string) => Expr::OpaqueRef(string),
        Ident::Access { module_name, parts } => {
            // The first value in the iterator is the variable name,
            // e.g. `foo` in `foo.bar.baz`
            let mut iter = parts.iter();
            let mut answer = match iter.next() {
                Some(Accessor::RecordField(ident)) => {
                    Expr::new_var_shortcut(module_name, ident, shortcut)
                }
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
            let mut first = true;
            for field in iter {
                let shortcut = if first { shortcut } else { None };
                first = false;
                // Wrap the previous answer in the new one, so we end up
                // with a nested Expr. That way, `foo.bar.baz` gets represented
                // in the AST as if it had been written (foo.bar).baz all along.
                match field {
                    Accessor::RecordField(field) => {
                        answer = Expr::RecordAccess(arena.alloc(answer), field, shortcut);
                    }
                    Accessor::TupleIndex(index) => {
                        answer = Expr::TupleAccess(arena.alloc(answer), index, shortcut);
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

fn rest_of_list_expr<'a>(
    start: Position,
    arena: &'a Bump,
    state: State<'a>,
) -> ParseResult<'a, Loc<Expr<'a>>, EExpr<'a>> {
    let inner = collection_inner(
        move |a, state: State<'a>, min_indent: u32| {
            let expr_pos = state.pos();
            match parse_expr_start(CHECK_FOR_ARROW, None, a, state, min_indent) {
                Ok(ok) => Ok(ok),
                Err((p, fail)) => Err((p, EList::Expr(a.alloc(fail), expr_pos))),
            }
        },
        Expr::SpaceBefore,
    );

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
}

#[derive(Debug)]
pub struct FoundApplyValue;

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

pub fn parse_record_field<'a>(
    arena: &'a Bump,
    state: State<'a>,
    min_indent: u32,
) -> ParseResult<'a, RecordField<'a>, ERecord<'a>> {
    use RecordField::*;

    let start = state.pos();
    match parse_lowercase_ident(state.clone()) {
        Err((NoProgress, _)) => { /* goto below :) */ }
        Err(_) => return Err((MadeProgress, ERecord::Field(start))),
        Ok((_, label, state)) => {
            let field_label = Loc::pos(start, state.pos(), label);

            let (_, (label_spaces, _), state) = eat_nc(arena, state, true)?;

            let olds = state.clone();
            match state.bytes().first() {
                Some(b @ (&b':' | &b'?')) => {
                    let (_, (spaces, _), state) = eat_nc(arena, state.inc(), true)?;

                    let val_pos = state.pos();
                    let (val_expr, state) =
                        match parse_expr_start(CHECK_FOR_ARROW, None, arena, state, min_indent) {
                            Ok((_, out, state)) => (out, state),
                            Err((_, fail)) => {
                                let fail = ERecord::Expr(arena.alloc(fail), val_pos);
                                return Err((MadeProgress, fail));
                            }
                        };
                    let val_expr = val_expr.spaced_before(arena, spaces);

                    let out = if *b == b':' {
                        RequiredValue(field_label, label_spaces, arena.alloc(val_expr))
                    } else {
                        OptionalValue(field_label, label_spaces, arena.alloc(val_expr))
                    };

                    return Ok((MadeProgress, out, state));
                }
                _ => {
                    let out = if !label_spaces.is_empty() {
                        SpaceAfter(arena.alloc(LabelOnly(field_label)), label_spaces)
                    } else {
                        LabelOnly(field_label)
                    };
                    return Ok((MadeProgress, out, olds));
                }
            };
        }
    }

    match state.bytes().first() {
        Some(&b'_') => {
            let state = state.inc();
            let name_pos = state.pos();
            let (opt_field_label, state) = match chomp_lowercase_part(state.bytes()) {
                Ok((name, _)) => (name, state.advance(name.len())),
                Err(NoProgress) => ("", state),
                Err(_) => return Err((MadeProgress, ERecord::Field(name_pos))),
            };

            let opt_field_label = Loc::pos(start, state.pos(), opt_field_label);

            let (_, (label_spaces, _), state) = eat_nc(arena, state, true)?;

            match state.bytes().first() {
                Some(&b':') => {
                    let (_, (colon_spaces, _), state) = eat_nc(arena, state.inc(), true)?;

                    let val_pos = state.pos();
                    let (field_val, state) =
                        match parse_expr_start(CHECK_FOR_ARROW, None, arena, state, min_indent) {
                            Ok((_, out, state)) => (out, state),
                            Err((_, fail)) => {
                                let fail = ERecord::Expr(arena.alloc(fail), val_pos);
                                return Err((MadeProgress, fail));
                            }
                        };

                    let field_val = field_val.spaced_before(arena, colon_spaces);
                    let out = IgnoredValue(opt_field_label, label_spaces, arena.alloc(field_val));
                    Ok((MadeProgress, out, state))
                }
                _ => Err((MadeProgress, ERecord::Colon(start))),
            }
        }
        _ => Err((NoProgress, ERecord::UnderscoreField(start))),
    }
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
        let (prefix, state) = match eat_nc::<'_, ERecord<'_>>(arena, state, false) {
            Err(_) => (None, before_prefix),
            Ok((_, (spaces_before, _), state)) => {
                let ident_at = state.pos();
                match parse_ident_chain(arena, state) {
                    Err(_) => (None, before_prefix),
                    Ok((_, ident, state)) => {
                        let ident =
                            Loc::pos(ident_at, state.pos(), ident_to_expr(arena, ident, None));
                        match eat_nc::<'_, ERecord<'_>>(arena, state, false) {
                            Err(_) => (None, before_prefix),
                            Ok((_, (spaces_after, _), state)) => {
                                let ident = ident.spaced_around(arena, spaces_before, spaces_after);

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

        let elem_p = move |a: &'a Bump, state: State<'a>, min_indent: u32| {
            let field_pos = state.pos();
            match parse_record_field(a, state, min_indent) {
                Ok((p, out, state)) => Ok((p, Loc::pos(field_pos, state.pos(), out), state)),
                Err(err) => Err(err),
            }
        };
        let (fields, state) =
            match collection_inner(elem_p, RecordField::SpaceBefore).parse(arena, state, 0) {
                Ok((_, out, state)) => (out, state),
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
    let (record, state) = match record_help().parse(arena, state, min_indent) {
        Ok((_, record, state)) => (record, state),
        Err((p, fail)) => return Err((p, EExpr::Record(fail, start))),
    };

    // there can be field access, e.g. `{ x : 4 }.x`
    let (accessors, state) = match parse_field_task_result_suffixes(arena, state) {
        Ok((_, accessors, state)) => (accessors, state),
        Err((_, fail)) => return Err((MadeProgress, fail)),
    };

    let expr_result = match record.prefix {
        Some((update, RecordHelpPrefix::Update)) => {
            let result = record.fields.map_items_result(arena, |loc_field| {
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

            let update = &*arena.alloc(update);
            result.map(|fields| Expr::RecordUpdate { update, fields })
        }
        Some((mapper, RecordHelpPrefix::Mapper)) => {
            let result = record.fields.map_items_result(arena, |loc_field| {
                let region = loc_field.region;
                let value = loc_field.value.to_assigned_field(arena);
                Ok(Loc { region, value })
            });

            let mapper = &*arena.alloc(mapper);
            result.map(|fields| Expr::RecordBuilder { mapper, fields })
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
            let expr = apply_expr_access_chain(arena, expr, accessors);
            Ok((MadeProgress, Loc::pos(start, state.pos(), expr), state))
        }
        Err(fail) => Err((MadeProgress, fail)),
    }
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
                Expr::RecordAccess(arena.alloc(value), field, None)
            }
            Suffix::Accessor(Accessor::TupleIndex(field)) => {
                Expr::TupleAccess(arena.alloc(value), field, None)
            }
            Suffix::TrySuffix(target) => Expr::TrySuffix {
                target,
                expr: arena.alloc(value),
            },
        })
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

fn rest_of_logical_not<'a>(
    start: Position,
    flags: ExprParseFlags,
    arena: &'a Bump,
    state: State<'a>,
    min_indent: u32,
) -> ParseResult<'a, Loc<Expr<'a>>, EExpr<'a>> {
    let after_not = state.pos();
    return match eat_nc_check(EExpr::IndentStart, arena, state, min_indent, true) {
        Ok((_, spaces_before, state)) => {
            match parse_term(PARSE_DEFAULT, flags, arena, state, min_indent) {
                Ok((_, loc_expr, state)) => {
                    let loc_expr = loc_expr.spaced_before(arena, spaces_before);
                    let op = Loc::pos(start, after_not, UnaryOp::Not);
                    let op = Expr::UnaryOp(arena.alloc(loc_expr), op);
                    let op = Loc::pos(start, state.pos(), op);
                    Ok((MadeProgress, op, state))
                }
                Err((_, fail)) => Err((MadeProgress, fail)),
            }
        }
        Err(err) => Err(err),
    };
}

const BINOP_CHAR_SET: &[u8] = b"+-/*=.<>:&|^?%!~";

const BINOP_CHAR_MASK: u128 = {
    let mut result = 0u128;

    let mut i = 0;
    while i < BINOP_CHAR_SET.len() {
        let index = BINOP_CHAR_SET[i] as usize;
        result |= 1 << index;

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

fn parse_bin_op(err_progress: Progress, state: State<'_>) -> ParseResult<'_, BinOp, EExpr<'_>> {
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
    state: State<'a>,
) -> ParseResult<'a, OperatorOrDef, E>
where
    F: Fn(Position) -> E,
    G: Fn(&'a str, Position) -> E,
    E: 'a,
{
    let chomped = chomp_ops(state.bytes());

    macro_rules! good {
        ($op:expr, $width:expr) => {{
            Ok((MadeProgress, $op, state.advance($width)))
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
        "~" => good!(OperatorOrDef::BinOp(BinOp::When), 1),
        _ => Err((MadeProgress, to_error(chomped, state.pos()))),
    }
}

fn chomp_ops(bytes: &[u8]) -> &str {
    let mut chomped = 0;

    for c in bytes.iter() {
        let index = *c as usize;
        if index > 127 || (BINOP_CHAR_MASK & (1 << index) == 0) {
            break;
        }
        chomped += 1;
    }

    unsafe {
        // Safe because BINOP_CHAR_SET only contains ascii chars
        std::str::from_utf8_unchecked(&bytes[..chomped])
    }
}
