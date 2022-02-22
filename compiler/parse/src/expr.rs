use crate::ast::{
    AssignedField, Collection, CommentOrNewline, Def, Expr, ExtractSpaces, Pattern, Spaceable,
    TypeAnnotation, TypeHeader,
};
use crate::blankspace::{space0_after_e, space0_around_ee, space0_before_e, space0_e};
use crate::ident::{lowercase_ident, parse_ident, Ident};
use crate::keyword;
use crate::parser::{
    self, backtrackable, optional, sep_by1, sep_by1_e, specialize, specialize_ref, then,
    trailing_sep_by0, word1, word2, EExpect, EExpr, EIf, EInParens, ELambda, EList, ENumber,
    EPattern, ERecord, EString, EType, EWhen, Either, ParseResult, Parser,
};
use crate::pattern::loc_closure_param;
use crate::state::State;
use crate::type_annotation;
use bumpalo::collections::Vec;
use bumpalo::Bump;
use roc_module::called_via::{BinOp, CalledVia, UnaryOp};
use roc_region::all::{Loc, Position, Region};

use crate::parser::Progress::{self, *};

fn expr_end<'a>() -> impl Parser<'a, (), EExpr<'a>> {
    |_arena, state: State<'a>| {
        if state.has_reached_end() {
            Ok((NoProgress, (), state))
        } else {
            Err((NoProgress, EExpr::BadExprEnd(state.pos()), state))
        }
    }
}

pub fn test_parse_expr<'a>(
    min_indent: u32,
    arena: &'a bumpalo::Bump,
    state: State<'a>,
) -> Result<Loc<Expr<'a>>, EExpr<'a>> {
    let parser = skip_second!(
        space0_before_e(
            move |a, s| parse_loc_expr(min_indent, a, s),
            min_indent,
            EExpr::IndentStart,
        ),
        expr_end()
    );

    match parser.parse(arena, state) {
        Ok((_, expression, _)) => Ok(expression),
        Err((_, fail, _)) => Err(fail),
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ExprParseOptions {
    /// Check for and accept multi-backpassing syntax
    /// This is usually true, but false within list/record literals
    /// because the comma separating backpassing arguments conflicts
    /// with the comma separating literal elements
    accept_multi_backpassing: bool,

    /// Check for the `->` token, and raise an error if found
    /// This is usually true, but false in if-guards
    ///
    /// > Just foo if foo == 2 -> ...
    check_for_arrow: bool,
}

impl Default for ExprParseOptions {
    fn default() -> Self {
        ExprParseOptions {
            accept_multi_backpassing: true,
            check_for_arrow: true,
        }
    }
}

pub fn expr_help<'a>(min_indent: u32) -> impl Parser<'a, Expr<'a>, EExpr<'a>> {
    move |arena, state: State<'a>| {
        parse_loc_expr(min_indent, arena, state).map(|(a, b, c)| (a, b.value, c))
    }
}

fn loc_expr_in_parens_help<'a>(min_indent: u32) -> impl Parser<'a, Loc<Expr<'a>>, EInParens<'a>> {
    move |arena, state| {
        let (_, loc_expr, state) = loc_expr_in_parens_help_help(min_indent).parse(arena, state)?;

        Ok((
            MadeProgress,
            Loc {
                region: loc_expr.region,
                value: Expr::ParensAround(arena.alloc(loc_expr.value)),
            },
            state,
        ))
    }
}

fn loc_expr_in_parens_help_help<'a>(
    min_indent: u32,
) -> impl Parser<'a, Loc<Expr<'a>>, EInParens<'a>> {
    between!(
        word1(b'(', EInParens::Open),
        space0_around_ee(
            specialize_ref(EInParens::Expr, move |arena, state| parse_loc_expr(
                min_indent, arena, state
            )),
            min_indent,
            EInParens::IndentOpen,
            EInParens::IndentEnd,
        ),
        word1(b')', EInParens::End)
    )
}

fn loc_expr_in_parens_etc_help<'a>(min_indent: u32) -> impl Parser<'a, Loc<Expr<'a>>, EExpr<'a>> {
    move |arena, state: State<'a>| {
        let parser = loc!(and!(
            specialize(EExpr::InParens, loc_expr_in_parens_help(min_indent)),
            one_of![record_field_access_chain(), |a, s| Ok((
                NoProgress,
                Vec::new_in(a),
                s
            ))]
        ));

        let (
            _,
            Loc {
                mut region,
                value: (loc_expr, field_accesses),
            },
            state,
        ) = parser.parse(arena, state)?;

        let mut value = loc_expr.value;

        // if there are field accesses, include the parentheses in the region
        // otherwise, don't include the parentheses
        if field_accesses.is_empty() {
            region = loc_expr.region;
        } else {
            for field in field_accesses {
                // Wrap the previous answer in the new one, so we end up
                // with a nested Expr. That way, `foo.bar.baz` gets represented
                // in the AST as if it had been written (foo.bar).baz all along.
                value = Expr::Access(arena.alloc(value), field);
            }
        }

        let loc_expr = Loc::at(region, value);

        Ok((MadeProgress, loc_expr, state))
    }
}

fn record_field_access_chain<'a>() -> impl Parser<'a, Vec<'a, &'a str>, EExpr<'a>> {
    |arena, state| match record_field_access().parse(arena, state) {
        Ok((_, initial, state)) => {
            let mut accesses = Vec::with_capacity_in(1, arena);

            accesses.push(initial);

            let mut loop_state = state;
            loop {
                match record_field_access().parse(arena, loop_state) {
                    Ok((_, next, state)) => {
                        accesses.push(next);
                        loop_state = state;
                    }
                    Err((MadeProgress, fail, state)) => return Err((MadeProgress, fail, state)),
                    Err((NoProgress, _, state)) => return Ok((MadeProgress, accesses, state)),
                }
            }
        }
        Err((MadeProgress, fail, state)) => Err((MadeProgress, fail, state)),
        Err((NoProgress, _, state)) => Err((NoProgress, EExpr::Access(state.pos()), state)),
    }
}

fn record_field_access<'a>() -> impl Parser<'a, &'a str, EExpr<'a>> {
    skip_first!(
        word1(b'.', EExpr::Access),
        specialize(|_, pos| EExpr::Access(pos), lowercase_ident())
    )
}

/// In some contexts we want to parse the `_` as an expression, so it can then be turned into a
/// pattern later
fn parse_loc_term_or_underscore<'a>(
    min_indent: u32,
    options: ExprParseOptions,
    arena: &'a Bump,
    state: State<'a>,
) -> ParseResult<'a, Loc<Expr<'a>>, EExpr<'a>> {
    one_of!(
        loc_expr_in_parens_etc_help(min_indent),
        loc!(specialize(EExpr::Str, string_literal_help())),
        loc!(specialize(EExpr::Number, positive_number_literal_help())),
        loc!(specialize(EExpr::Lambda, closure_help(min_indent, options))),
        loc!(underscore_expression()),
        loc!(record_literal_help(min_indent)),
        loc!(specialize(EExpr::List, list_literal_help(min_indent))),
        loc!(map_with_arena!(
            assign_or_destructure_identifier(),
            ident_to_expr
        )),
    )
    .parse(arena, state)
}

fn parse_loc_term<'a>(
    min_indent: u32,
    options: ExprParseOptions,
    arena: &'a Bump,
    state: State<'a>,
) -> ParseResult<'a, Loc<Expr<'a>>, EExpr<'a>> {
    one_of!(
        loc_expr_in_parens_etc_help(min_indent),
        loc!(specialize(EExpr::Str, string_literal_help())),
        loc!(specialize(EExpr::Number, positive_number_literal_help())),
        loc!(specialize(EExpr::Lambda, closure_help(min_indent, options))),
        loc!(record_literal_help(min_indent)),
        loc!(specialize(EExpr::List, list_literal_help(min_indent))),
        loc!(map_with_arena!(
            assign_or_destructure_identifier(),
            ident_to_expr
        )),
    )
    .parse(arena, state)
}

fn underscore_expression<'a>() -> impl Parser<'a, Expr<'a>, EExpr<'a>> {
    move |arena: &'a Bump, state: State<'a>| {
        let start = state.pos();

        let (_, _, next_state) = word1(b'_', EExpr::Underscore).parse(arena, state)?;

        let lowercase_ident_expr = { specialize(move |_, _| EExpr::End(start), lowercase_ident()) };

        let (_, output, final_state) = optional(lowercase_ident_expr).parse(arena, next_state)?;

        match output {
            Some(name) => Ok((MadeProgress, Expr::Underscore(name), final_state)),
            None => Ok((MadeProgress, Expr::Underscore(""), final_state)),
        }
    }
}

fn loc_possibly_negative_or_negated_term<'a>(
    min_indent: u32,
    options: ExprParseOptions,
) -> impl Parser<'a, Loc<Expr<'a>>, EExpr<'a>> {
    one_of![
        |arena, state: State<'a>| {
            let initial = state.clone();

            let (_, (loc_op, loc_expr), state) = and!(loc!(unary_negate()), |a, s| parse_loc_term(
                min_indent, options, a, s
            ))
            .parse(arena, state)?;

            let loc_expr = numeric_negate_expression(arena, initial, loc_op, loc_expr, &[]);

            Ok((MadeProgress, loc_expr, state))
        },
        // this will parse negative numbers, which the unary negate thing up top doesn't (for now)
        loc!(specialize(EExpr::Number, number_literal_help())),
        loc!(map_with_arena!(
            and!(loc!(word1(b'!', EExpr::Start)), |a, s| {
                parse_loc_term(min_indent, options, a, s)
            }),
            |arena: &'a Bump, (loc_op, loc_expr): (Loc<_>, _)| {
                Expr::UnaryOp(arena.alloc(loc_expr), Loc::at(loc_op.region, UnaryOp::Not))
            }
        )),
        |arena, state| { parse_loc_term_or_underscore(min_indent, options, arena, state) }
    ]
}

fn fail_expr_start_e<'a, T: 'a>() -> impl Parser<'a, T, EExpr<'a>> {
    |_arena, state: State<'a>| Err((NoProgress, EExpr::Start(state.pos()), state))
}

fn unary_negate<'a>() -> impl Parser<'a, (), EExpr<'a>> {
    move |_arena: &'a Bump, state: State<'a>| {
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
            Err((NoProgress, EExpr::UnaryNot(state.pos()), state))
        }
    }
}

fn parse_expr_start<'a>(
    min_indent: u32,
    options: ExprParseOptions,
    start_column: u32,
    arena: &'a Bump,
    state: State<'a>,
) -> ParseResult<'a, Loc<Expr<'a>>, EExpr<'a>> {
    one_of![
        loc!(specialize(EExpr::If, if_expr_help(min_indent, options))),
        loc!(specialize(
            EExpr::When,
            when::expr_help(min_indent, options)
        )),
        loc!(specialize(EExpr::Expect, expect_help(min_indent, options))),
        loc!(specialize(EExpr::Lambda, closure_help(min_indent, options))),
        loc!(move |a, s| parse_expr_operator_chain(min_indent, options, start_column, a, s)),
        fail_expr_start_e()
    ]
    .parse(arena, state)
}

fn parse_expr_operator_chain<'a>(
    min_indent: u32,
    options: ExprParseOptions,
    start_column: u32,
    arena: &'a Bump,
    state: State<'a>,
) -> ParseResult<'a, Expr<'a>, EExpr<'a>> {
    let (_, expr, state) =
        loc_possibly_negative_or_negated_term(min_indent, options).parse(arena, state)?;

    let initial = state.clone();
    let end = state.pos();

    match space0_e(min_indent, EExpr::IndentEnd).parse(arena, state) {
        Err((_, _, state)) => Ok((MadeProgress, expr.value, state)),
        Ok((_, spaces_before_op, state)) => {
            let expr_state = ExprState {
                operators: Vec::new_in(arena),
                arguments: Vec::new_in(arena),
                expr,
                spaces_after: spaces_before_op,
                initial,
                end,
            };

            parse_expr_end(min_indent, options, start_column, expr_state, arena, state)
        }
    }
}

#[derive(Debug)]
struct ExprState<'a> {
    operators: Vec<'a, (Loc<Expr<'a>>, Loc<BinOp>)>,
    arguments: Vec<'a, &'a Loc<Expr<'a>>>,
    expr: Loc<Expr<'a>>,
    spaces_after: &'a [CommentOrNewline<'a>],
    initial: State<'a>,
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
        } else if !self.expr.value.is_tag() && !self.arguments.is_empty() {
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
        kind: TypeKind,
    ) -> Result<(Loc<Expr<'a>>, Vec<'a, &'a Loc<Expr<'a>>>), EExpr<'a>> {
        debug_assert_eq!(
            loc_op.value,
            match kind {
                TypeKind::Alias => BinOp::IsAliasType,
                TypeKind::Opaque => BinOp::IsOpaqueType,
            }
        );

        if !self.operators.is_empty() {
            // this `:`/`:=` likely occurred inline; treat it as an invalid operator
            let op = match kind {
                TypeKind::Alias => ":",
                TypeKind::Opaque => ":=",
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
fn parse_expr_final<'a>(
    expr_state: ExprState<'a>,
    arena: &'a Bump,
    state: State<'a>,
) -> ParseResult<'a, Expr<'a>, EExpr<'a>> {
    let right_arg = to_call(arena, expr_state.arguments, expr_state.expr);

    let expr = if expr_state.operators.is_empty() {
        right_arg.value
    } else {
        Expr::BinOps(
            expr_state.operators.into_bump_slice(),
            arena.alloc(right_arg),
        )
    };

    Ok((MadeProgress, expr, state))
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
    debug_assert_eq!(state.bytes().get(0), Some(&b'-'));
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

fn append_body_definition<'a>(
    arena: &'a Bump,
    defs: &mut Vec<'a, &'a Loc<Def<'a>>>,
    spaces: &'a [CommentOrNewline<'a>],
    loc_pattern: Loc<Pattern<'a>>,
    loc_def_body: Loc<Expr<'a>>,
) {
    let region = Region::span_across(&loc_pattern.region, &loc_def_body.region);

    if spaces.len() <= 1 {
        let last = defs.pop();
        match last.map(|d| d.value.unroll_spaces_before()) {
            Some((before_ann_spaces, Def::Annotation(ann_pattern, ann_type))) => {
                return append_body_definition_help(
                    arena,
                    defs,
                    region,
                    before_ann_spaces,
                    spaces,
                    loc_pattern,
                    loc_def_body,
                    ann_pattern,
                    ann_type,
                );
            }
            Some((
                before_ann_spaces,
                Def::Alias {
                    header,
                    ann: ann_type,
                },
            )) => {
                // This is a case like
                //   UserId x : [ UserId Int ]
                //   UserId x = UserId 42
                // We optimistically parsed the first line as an alias; we now turn it
                // into an annotation.
                let loc_name = arena.alloc(header.name.map(|x| Pattern::GlobalTag(x)));
                let ann_pattern = Pattern::Apply(loc_name, header.vars);
                let vars_region = Region::across_all(header.vars.iter().map(|v| &v.region));
                let region_ann_pattern = Region::span_across(&loc_name.region, &vars_region);
                let loc_ann_pattern = Loc::at(region_ann_pattern, ann_pattern);

                return append_body_definition_help(
                    arena,
                    defs,
                    region,
                    before_ann_spaces,
                    spaces,
                    loc_pattern,
                    loc_def_body,
                    arena.alloc(loc_ann_pattern),
                    ann_type,
                );
            }
            _ => {
                defs.extend(last);
            }
        }
    }

    // the previous and current def can't be joined up
    let mut loc_def = Loc::at(
        region,
        Def::Body(arena.alloc(loc_pattern), &*arena.alloc(loc_def_body)),
    );

    if !spaces.is_empty() {
        loc_def = arena
            .alloc(loc_def.value)
            .with_spaces_before(spaces, loc_def.region);
    }

    defs.push(arena.alloc(loc_def));
}

#[allow(clippy::too_many_arguments)]
fn append_body_definition_help<'a>(
    arena: &'a Bump,
    defs: &mut Vec<'a, &'a Loc<Def<'a>>>,
    region: Region,
    before_ann_spaces: &'a [CommentOrNewline<'a>],
    before_body_spaces: &'a [CommentOrNewline<'a>],
    loc_pattern_body: Loc<Pattern<'a>>,
    loc_def_body: Loc<Expr<'a>>,
    loc_pattern_ann: &'a Loc<Pattern<'a>>,
    loc_ann: &'a Loc<TypeAnnotation<'a>>,
) {
    let comment = match before_body_spaces.get(0) {
        Some(CommentOrNewline::LineComment(s)) => Some(*s),
        Some(CommentOrNewline::DocComment(s)) => Some(*s),
        _ => None,
    };

    let mut loc_def = Loc::at(
        region,
        Def::AnnotatedBody {
            ann_pattern: loc_pattern_ann,
            ann_type: loc_ann,
            comment,
            body_pattern: arena.alloc(loc_pattern_body),
            body_expr: &*arena.alloc(loc_def_body),
        },
    );

    if !before_ann_spaces.is_empty() {
        loc_def = arena
            .alloc(loc_def.value)
            .with_spaces_before(before_ann_spaces, loc_def.region);
    }

    defs.push(arena.alloc(loc_def));
}

fn append_annotation_definition<'a>(
    arena: &'a Bump,
    defs: &mut Vec<'a, &'a Loc<Def<'a>>>,
    spaces: &'a [CommentOrNewline<'a>],
    loc_pattern: Loc<Pattern<'a>>,
    loc_ann: Loc<TypeAnnotation<'a>>,
    kind: TypeKind,
) {
    let region = Region::span_across(&loc_pattern.region, &loc_ann.region);

    // the previous and current def can't be joined up
    match &loc_pattern.value {
        Pattern::Apply(
            Loc {
                value: Pattern::GlobalTag(name),
                ..
            },
            alias_arguments,
        ) => append_type_definition(
            arena,
            defs,
            region,
            spaces,
            Loc::at(loc_pattern.region, name),
            alias_arguments,
            loc_ann,
            kind,
        ),
        Pattern::GlobalTag(name) => append_type_definition(
            arena,
            defs,
            region,
            spaces,
            Loc::at(loc_pattern.region, name),
            &[],
            loc_ann,
            kind,
        ),
        _ => {
            let mut loc_def = Loc::at(region, Def::Annotation(loc_pattern, loc_ann));
            if !spaces.is_empty() {
                loc_def = arena
                    .alloc(loc_def.value)
                    .with_spaces_before(spaces, loc_def.region);
            }

            defs.push(arena.alloc(loc_def));
        }
    }
}

fn append_expect_definition<'a>(
    arena: &'a Bump,
    defs: &mut Vec<'a, &'a Loc<Def<'a>>>,
    start: Position,
    spaces: &'a [CommentOrNewline<'a>],
    loc_expect_body: Loc<Expr<'a>>,
) {
    let def = Def::Expect(arena.alloc(loc_expect_body));

    let end = loc_expect_body.region.end();
    let region = Region::new(start, end);

    let mut loc_def = Loc::at(region, def);

    if !spaces.is_empty() {
        loc_def = arena
            .alloc(loc_def.value)
            .with_spaces_before(spaces, loc_def.region);
    }

    defs.push(arena.alloc(loc_def));
}

#[allow(clippy::too_many_arguments)]
fn append_type_definition<'a>(
    arena: &'a Bump,
    defs: &mut Vec<'a, &'a Loc<Def<'a>>>,
    region: Region,
    spaces: &'a [CommentOrNewline<'a>],
    name: Loc<&'a str>,
    pattern_arguments: &'a [Loc<Pattern<'a>>],
    loc_ann: Loc<TypeAnnotation<'a>>,
    kind: TypeKind,
) {
    let header = TypeHeader {
        name,
        vars: pattern_arguments,
    };
    let def = match kind {
        TypeKind::Alias => Def::Alias {
            header,
            ann: loc_ann,
        },
        TypeKind::Opaque => Def::Opaque {
            header,
            typ: loc_ann,
        },
    };
    let mut loc_def = Loc::at(region, def);

    if !spaces.is_empty() {
        loc_def = arena
            .alloc(loc_def.value)
            .with_spaces_before(spaces, loc_def.region);
    }

    defs.push(arena.alloc(loc_def));
}

#[derive(Debug)]
struct DefState<'a> {
    defs: Vec<'a, &'a Loc<Def<'a>>>,
    spaces_after: &'a [CommentOrNewline<'a>],
}

fn parse_defs_end<'a>(
    options: ExprParseOptions,
    start_column: u32,
    mut def_state: DefState<'a>,
    arena: &'a Bump,
    state: State<'a>,
) -> ParseResult<'a, DefState<'a>, EExpr<'a>> {
    let min_indent = start_column;
    let initial = state.clone();

    let state = match space0_e(min_indent, EExpr::IndentStart).parse(arena, state) {
        Err((MadeProgress, _, s)) => {
            return Err((MadeProgress, EExpr::DefMissingFinalExpr(s.pos()), s));
        }
        Ok((_, spaces, state)) => {
            def_state.spaces_after = spaces;
            state
        }
        Err((NoProgress, _, state)) => state,
    };

    let start = state.pos();
    let column = state.column();

    match space0_after_e(
        crate::pattern::loc_pattern_help(min_indent),
        min_indent,
        EPattern::IndentEnd,
    )
    .parse(arena, state.clone())
    {
        Err((NoProgress, _, _)) => {
            match crate::parser::keyword_e(crate::keyword::EXPECT, EExpect::Expect)
                .parse(arena, state)
            {
                Err((_, _, _)) => {
                    // a hacky way to get expression-based error messages. TODO fix this
                    Ok((NoProgress, def_state, initial))
                }
                Ok((_, _, state)) => {
                    let parse_def_expr = space0_before_e(
                        move |a, s| parse_loc_expr(min_indent + 1, a, s),
                        min_indent,
                        EExpr::IndentEnd,
                    );

                    let (_, loc_def_expr, state) = parse_def_expr.parse(arena, state)?;

                    append_expect_definition(
                        arena,
                        &mut def_state.defs,
                        start,
                        def_state.spaces_after,
                        loc_def_expr,
                    );

                    parse_defs_end(options, column, def_state, arena, state)
                }
            }
        }
        Err((MadeProgress, _, _)) => {
            // a hacky way to get expression-based error messages. TODO fix this
            Ok((NoProgress, def_state, initial))
        }
        Ok((_, loc_pattern, state)) => match operator().parse(arena, state) {
            Ok((_, BinOp::Assignment, state)) => {
                let parse_def_expr = space0_before_e(
                    move |a, s| parse_loc_expr(min_indent + 1, a, s),
                    min_indent,
                    EExpr::IndentEnd,
                );

                let (_, loc_def_expr, state) = parse_def_expr.parse(arena, state)?;

                append_body_definition(
                    arena,
                    &mut def_state.defs,
                    def_state.spaces_after,
                    loc_pattern,
                    loc_def_expr,
                );

                parse_defs_end(options, column, def_state, arena, state)
            }
            Ok((_, op @ (BinOp::IsAliasType | BinOp::IsOpaqueType), state)) => {
                let (_, ann_type, state) = specialize(
                    EExpr::Type,
                    space0_before_e(
                        type_annotation::located_help(min_indent + 1, false),
                        min_indent + 1,
                        EType::TIndentStart,
                    ),
                )
                .parse(arena, state)?;

                append_annotation_definition(
                    arena,
                    &mut def_state.defs,
                    def_state.spaces_after,
                    loc_pattern,
                    ann_type,
                    match op {
                        BinOp::IsAliasType => TypeKind::Alias,
                        BinOp::IsOpaqueType => TypeKind::Opaque,
                        _ => unreachable!(),
                    },
                );

                parse_defs_end(options, column, def_state, arena, state)
            }

            _ => Ok((MadeProgress, def_state, initial)),
        },
    }
}

fn parse_defs_expr<'a>(
    options: ExprParseOptions,
    start_column: u32,
    def_state: DefState<'a>,
    arena: &'a Bump,
    state: State<'a>,
) -> ParseResult<'a, Expr<'a>, EExpr<'a>> {
    let min_indent = start_column;

    match parse_defs_end(options, start_column, def_state, arena, state) {
        Err(bad) => Err(bad),
        Ok((_, def_state, state)) => {
            // this is no def, because there is no `=` or `:`; parse as an expr
            let parse_final_expr = space0_before_e(
                move |a, s| parse_loc_expr(min_indent, a, s),
                min_indent,
                EExpr::IndentEnd,
            );

            match parse_final_expr.parse(arena, state) {
                Err((_, fail, state)) => {
                    return Err((
                        MadeProgress,
                        EExpr::DefMissingFinalExpr2(arena.alloc(fail), state.pos()),
                        state,
                    ));
                }
                Ok((_, loc_ret, state)) => {
                    return Ok((
                        MadeProgress,
                        Expr::Defs(def_state.defs.into_bump_slice(), arena.alloc(loc_ret)),
                        state,
                    ));
                }
            }
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq)]
enum TypeKind {
    Alias,
    Opaque,
}

#[allow(clippy::too_many_arguments)]
fn finish_parsing_alias_or_opaque<'a>(
    min_indent: u32,
    options: ExprParseOptions,
    start_column: u32,
    expr_state: ExprState<'a>,
    loc_op: Loc<BinOp>,
    arena: &'a Bump,
    state: State<'a>,
    spaces_after_operator: &'a [CommentOrNewline<'a>],
    kind: TypeKind,
) -> ParseResult<'a, Expr<'a>, EExpr<'a>> {
    let expr_region = expr_state.expr.region;
    let indented_more = start_column + 1;

    let (expr, arguments) = expr_state
        .validate_is_type_def(arena, loc_op, kind)
        .map_err(|fail| (MadeProgress, fail, state.clone()))?;

    let (loc_def, state) = match &expr.value {
        Expr::GlobalTag(name) => {
            let mut type_arguments = Vec::with_capacity_in(arguments.len(), arena);

            for argument in arguments {
                match expr_to_pattern_help(arena, &argument.value) {
                    Ok(good) => {
                        type_arguments.push(Loc::at(argument.region, good));
                    }
                    Err(_) => panic!(),
                }
            }

            let (_, ann_type, state) = specialize(
                EExpr::Type,
                space0_before_e(
                    type_annotation::located_help(indented_more, true),
                    min_indent,
                    EType::TIndentStart,
                ),
            )
            .parse(arena, state)?;

            let def_region = Region::span_across(&expr.region, &ann_type.region);

            let header = TypeHeader {
                name: Loc::at(expr.region, name),
                vars: type_arguments.into_bump_slice(),
            };
            let type_def = match kind {
                TypeKind::Alias => Def::Alias {
                    header,
                    ann: ann_type,
                },
                TypeKind::Opaque => Def::Opaque {
                    header,
                    typ: ann_type,
                },
            };

            (&*arena.alloc(Loc::at(def_region, type_def)), state)
        }

        _ => {
            let call = to_call(arena, arguments, expr);

            match expr_to_pattern_help(arena, &call.value) {
                Ok(good) => {
                    let parser = specialize(
                        EExpr::Type,
                        space0_before_e(
                            type_annotation::located_help(indented_more, false),
                            min_indent,
                            EType::TIndentStart,
                        ),
                    );

                    match parser.parse(arena, state) {
                        Err((_, fail, state)) => return Err((MadeProgress, fail, state)),
                        Ok((_, mut ann_type, state)) => {
                            // put the spaces from after the operator in front of the call
                            if !spaces_after_operator.is_empty() {
                                ann_type = arena
                                    .alloc(ann_type.value)
                                    .with_spaces_before(spaces_after_operator, ann_type.region);
                            }

                            let def_region = Region::span_across(&call.region, &ann_type.region);

                            let alias = Def::Annotation(Loc::at(expr_region, good), ann_type);

                            (&*arena.alloc(Loc::at(def_region, alias)), state)
                        }
                    }
                }
                Err(_) => {
                    // this `:`/`:=` likely occurred inline; treat it as an invalid operator
                    let op = match kind {
                        TypeKind::Alias => ":",
                        TypeKind::Opaque => ":=",
                    };
                    let fail = EExpr::BadOperator(op, loc_op.region.start());

                    return Err((MadeProgress, fail, state));
                }
            }
        }
    };

    let def_state = DefState {
        defs: bumpalo::vec![in arena; loc_def],
        spaces_after: &[],
    };

    parse_defs_expr(options, start_column, def_state, arena, state)
}

fn parse_expr_operator<'a>(
    min_indent: u32,
    options: ExprParseOptions,
    start_column: u32,
    mut expr_state: ExprState<'a>,
    loc_op: Loc<BinOp>,
    arena: &'a Bump,
    state: State<'a>,
) -> ParseResult<'a, Expr<'a>, EExpr<'a>> {
    let (_, spaces_after_operator, state) =
        space0_e(min_indent, EExpr::IndentEnd).parse(arena, state)?;

    // a `-` is unary if it is preceded by a space and not followed by a space

    let op = loc_op.value;
    let op_start = loc_op.region.start();
    let op_end = loc_op.region.end();
    let new_start = state.pos();
    match op {
        BinOp::Minus if expr_state.end != op_start && op_end == new_start => {
            // negative terms

            let (_, negated_expr, state) = parse_loc_term(min_indent, options, arena, state)?;
            let new_end = state.pos();

            let arg = numeric_negate_expression(
                arena,
                expr_state.initial,
                loc_op,
                negated_expr,
                expr_state.spaces_after,
            );

            expr_state.initial = state.clone();

            let (spaces, state) = match space0_e(min_indent, EExpr::IndentEnd).parse(arena, state) {
                Err((_, _, state)) => (&[] as &[_], state),
                Ok((_, spaces, state)) => (spaces, state),
            };

            expr_state.arguments.push(arena.alloc(arg));
            expr_state.spaces_after = spaces;
            expr_state.end = new_end;

            parse_expr_end(min_indent, options, start_column, expr_state, arena, state)
        }
        BinOp::Assignment => {
            let expr_region = expr_state.expr.region;
            let indented_more = start_column + 1;

            let call = expr_state
                .validate_assignment_or_backpassing(arena, loc_op, EExpr::ElmStyleFunction)
                .map_err(|fail| (MadeProgress, fail, state.clone()))?;

            let (loc_def, state) = {
                match expr_to_pattern_help(arena, &call.value) {
                    Ok(good) => {
                        let (_, mut ann_type, state) = parse_loc_expr(indented_more, arena, state)?;

                        // put the spaces from after the operator in front of the call
                        if !spaces_after_operator.is_empty() {
                            ann_type = arena
                                .alloc(ann_type.value)
                                .with_spaces_before(spaces_after_operator, ann_type.region);
                        }

                        let alias_region = Region::span_across(&call.region, &ann_type.region);

                        let alias = Def::Body(
                            arena.alloc(Loc::at(expr_region, good)),
                            arena.alloc(ann_type),
                        );

                        (&*arena.alloc(Loc::at(alias_region, alias)), state)
                    }
                    Err(_) => {
                        // this `=` likely occurred inline; treat it as an invalid operator
                        let fail = EExpr::BadOperator(arena.alloc("="), loc_op.region.start());

                        return Err((MadeProgress, fail, state));
                    }
                }
            };

            let def_state = DefState {
                defs: bumpalo::vec![in arena; loc_def],
                spaces_after: &[],
            };

            parse_defs_expr(options, start_column, def_state, arena, state)
        }
        BinOp::Backpassing => {
            let expr_region = expr_state.expr.region;
            let indented_more = start_column + 1;

            let call = expr_state
                .validate_assignment_or_backpassing(arena, loc_op, |_, pos| {
                    EExpr::BadOperator("<-", pos)
                })
                .map_err(|fail| (MadeProgress, fail, state.clone()))?;

            let (loc_pattern, loc_body, state) = {
                match expr_to_pattern_help(arena, &call.value) {
                    Ok(good) => {
                        let (_, mut ann_type, state) = parse_loc_expr(indented_more, arena, state)?;

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

                        return Err((MadeProgress, fail, state));
                    }
                }
            };

            let parse_cont = space0_before_e(
                move |a, s| parse_loc_expr(min_indent, a, s),
                min_indent,
                EExpr::IndentEnd,
            );

            let (_, loc_cont, state) = parse_cont.parse(arena, state)?;

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
            start_column,
            expr_state,
            loc_op,
            arena,
            state,
            spaces_after_operator,
            match op {
                BinOp::IsAliasType => TypeKind::Alias,
                BinOp::IsOpaqueType => TypeKind::Opaque,
                _ => unreachable!(),
            },
        ),
        _ => match loc_possibly_negative_or_negated_term(min_indent, options).parse(arena, state) {
            Err((MadeProgress, f, s)) => Err((MadeProgress, f, s)),
            Ok((_, mut new_expr, state)) => {
                let new_end = state.pos();

                expr_state.initial = state.clone();

                // put the spaces from after the operator in front of the new_expr
                if !spaces_after_operator.is_empty() {
                    new_expr = arena
                        .alloc(new_expr.value)
                        .with_spaces_before(spaces_after_operator, new_expr.region);
                }

                match space0_e(min_indent, EExpr::IndentEnd).parse(arena, state) {
                    Err((_, _, state)) => {
                        let args = std::mem::replace(&mut expr_state.arguments, Vec::new_in(arena));

                        let call = to_call(arena, args, expr_state.expr);

                        expr_state.operators.push((call, loc_op));
                        expr_state.expr = new_expr;
                        expr_state.end = new_end;
                        expr_state.spaces_after = &[];

                        parse_expr_final(expr_state, arena, state)
                    }
                    Ok((_, spaces, state)) => {
                        let args = std::mem::replace(&mut expr_state.arguments, Vec::new_in(arena));

                        let call = to_call(arena, args, expr_state.expr);

                        expr_state.operators.push((call, loc_op));
                        expr_state.expr = new_expr;
                        expr_state.end = new_end;
                        expr_state.spaces_after = spaces;

                        // TODO new start?
                        parse_expr_end(min_indent, options, start_column, expr_state, arena, state)
                    }
                }
            }
            Err((NoProgress, _, _)) => {
                todo!()
            }
        },
    }
}

fn parse_expr_end<'a>(
    min_indent: u32,
    options: ExprParseOptions,
    start_column: u32,
    mut expr_state: ExprState<'a>,
    arena: &'a Bump,
    state: State<'a>,
) -> ParseResult<'a, Expr<'a>, EExpr<'a>> {
    let parser = skip_first!(
        crate::blankspace::check_indent(min_indent, EExpr::IndentEnd),
        move |a, s| parse_loc_term(min_indent, options, a, s)
    );

    match parser.parse(arena, state.clone()) {
        Err((MadeProgress, f, s)) => Err((MadeProgress, f, s)),
        Ok((_, mut arg, state)) => {
            let new_end = state.pos();

            // now that we have `function arg1 ... <spaces> argn`, attach the spaces to the `argn`
            if !expr_state.spaces_after.is_empty() {
                arg = arena
                    .alloc(arg.value)
                    .with_spaces_before(expr_state.spaces_after, arg.region);

                expr_state.spaces_after = &[];
            }
            expr_state.initial = state.clone();

            match space0_e(min_indent, EExpr::IndentEnd).parse(arena, state) {
                Err((_, _, state)) => {
                    expr_state.arguments.push(arena.alloc(arg));
                    expr_state.end = new_end;
                    expr_state.spaces_after = &[];

                    parse_expr_final(expr_state, arena, state)
                }
                Ok((_, new_spaces, state)) => {
                    expr_state.arguments.push(arena.alloc(arg));
                    expr_state.end = new_end;
                    expr_state.spaces_after = new_spaces;

                    parse_expr_end(min_indent, options, start_column, expr_state, arena, state)
                }
            }
        }
        Err((NoProgress, _, _)) => {
            let before_op = state.clone();
            // try an operator
            match loc!(operator()).parse(arena, state) {
                Err((MadeProgress, f, s)) => Err((MadeProgress, f, s)),
                Ok((_, loc_op, state)) => {
                    expr_state.consume_spaces(arena);
                    expr_state.initial = before_op;
                    parse_expr_operator(
                        min_indent,
                        options,
                        start_column,
                        expr_state,
                        loc_op,
                        arena,
                        state,
                    )
                }
                Err((NoProgress, _, mut state)) => {
                    // try multi-backpassing
                    if options.accept_multi_backpassing && state.bytes().starts_with(b",") {
                        state = state.advance(1);

                        let (_, mut patterns, state) = specialize_ref(
                            EExpr::Pattern,
                            crate::parser::sep_by0(
                                word1(b',', EPattern::Start),
                                space0_around_ee(
                                    crate::pattern::loc_pattern_help(min_indent),
                                    min_indent,
                                    EPattern::Start,
                                    EPattern::IndentEnd,
                                ),
                            ),
                        )
                        .parse(arena, state)?;

                        expr_state.consume_spaces(arena);
                        let call = to_call(arena, expr_state.arguments, expr_state.expr);

                        let loc_pattern = Loc::at(
                            call.region,
                            expr_to_pattern_help(arena, &call.value).unwrap(),
                        );

                        patterns.insert(0, loc_pattern);

                        match word2(b'<', b'-', EExpr::BackpassArrow).parse(arena, state) {
                            Err((_, fail, state)) => Err((MadeProgress, fail, state)),
                            Ok((_, _, state)) => {
                                let min_indent = start_column;

                                let parse_body = space0_before_e(
                                    move |a, s| parse_loc_expr(min_indent + 1, a, s),
                                    min_indent,
                                    EExpr::IndentEnd,
                                );

                                let (_, loc_body, state) = parse_body.parse(arena, state)?;

                                let parse_cont = space0_before_e(
                                    move |a, s| parse_loc_expr(min_indent, a, s),
                                    min_indent,
                                    EExpr::IndentEnd,
                                );

                                let (_, loc_cont, state) = parse_cont.parse(arena, state)?;

                                let ret = Expr::Backpassing(
                                    patterns.into_bump_slice(),
                                    arena.alloc(loc_body),
                                    arena.alloc(loc_cont),
                                );

                                Ok((MadeProgress, ret, state))
                            }
                        }
                    } else if options.check_for_arrow && state.bytes().starts_with(b"->") {
                        Err((MadeProgress, EExpr::BadOperator("->", state.pos()), state))
                    } else {
                        // roll back space parsing
                        let state = expr_state.initial.clone();

                        parse_expr_final(expr_state, arena, state)
                    }
                }
            }
        }
    }
}

pub fn parse_loc_expr<'a>(
    min_indent: u32,
    arena: &'a Bump,
    state: State<'a>,
) -> ParseResult<'a, Loc<Expr<'a>>, EExpr<'a>> {
    parse_loc_expr_with_options(
        min_indent,
        ExprParseOptions {
            accept_multi_backpassing: true,
            ..Default::default()
        },
        arena,
        state,
    )
}

pub fn parse_loc_expr_no_multi_backpassing<'a>(
    min_indent: u32,
    arena: &'a Bump,
    state: State<'a>,
) -> ParseResult<'a, Loc<Expr<'a>>, EExpr<'a>> {
    parse_loc_expr_with_options(
        min_indent,
        ExprParseOptions {
            accept_multi_backpassing: false,
            ..Default::default()
        },
        arena,
        state,
    )
}

fn parse_loc_expr_with_options<'a>(
    min_indent: u32,
    options: ExprParseOptions,
    arena: &'a Bump,
    state: State<'a>,
) -> ParseResult<'a, Loc<Expr<'a>>, EExpr<'a>> {
    let column = state.column();
    parse_expr_start(min_indent, options, column, arena, state)
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
        Expr::GlobalTag(value) => Ok(Pattern::GlobalTag(value)),
        Expr::PrivateTag(value) => Ok(Pattern::PrivateTag(value)),
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
        Expr::AccessorFunction(_)
        | Expr::Access(_, _)
        | Expr::List { .. }
        | Expr::Closure(_, _)
        | Expr::Backpassing(_, _, _)
        | Expr::BinOps { .. }
        | Expr::Defs(_, _)
        | Expr::If(_, _)
        | Expr::When(_, _)
        | Expr::Expect(_, _)
        | Expr::MalformedClosure
        | Expr::PrecedenceConflict { .. }
        | Expr::RecordUpdate { .. }
        | Expr::UnaryOp(_, _) => Err(()),

        Expr::Str(string) => Ok(Pattern::StrLiteral(*string)),
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

pub fn defs<'a>(min_indent: u32) -> impl Parser<'a, Vec<'a, Loc<Def<'a>>>, EExpr<'a>> {
    move |arena, state: State<'a>| {
        let def_state = DefState {
            defs: Vec::new_in(arena),
            spaces_after: &[],
        };

        let (_, initial_space, state) =
            space0_e(min_indent, EExpr::IndentEnd).parse(arena, state)?;

        let start_column = state.column();

        let options = ExprParseOptions {
            accept_multi_backpassing: false,
            check_for_arrow: true,
        };

        let (_, def_state, state) = parse_defs_end(options, start_column, def_state, arena, state)?;

        let (_, final_space, state) =
            space0_e(start_column, EExpr::IndentEnd).parse(arena, state)?;

        let mut output = Vec::with_capacity_in(def_state.defs.len(), arena);

        if !def_state.defs.is_empty() {
            let first = 0;
            let last = def_state.defs.len() - 1;

            for (i, ref_def) in def_state.defs.into_iter().enumerate() {
                let mut def = *ref_def;

                if i == first {
                    def = arena
                        .alloc(def.value)
                        .with_spaces_before(initial_space, def.region)
                }

                if i == last {
                    def = arena
                        .alloc(def.value)
                        .with_spaces_after(final_space, def.region)
                }

                output.push(def);
            }
        }

        Ok((MadeProgress, output, state))
    }
}

// PARSER HELPERS

fn closure_help<'a>(
    min_indent: u32,
    options: ExprParseOptions,
) -> impl Parser<'a, Expr<'a>, ELambda<'a>> {
    map_with_arena!(
        skip_first!(
            // All closures start with a '\' - e.g. (\x -> x + 1)
            word1(b'\\', ELambda::Start),
            // Once we see the '\', we're committed to parsing this as a closure.
            // It may turn out to be malformed, but it is definitely a closure.
            and!(
                // Parse the params
                // Params are comma-separated
                sep_by1_e(
                    word1(b',', ELambda::Comma),
                    space0_around_ee(
                        specialize(ELambda::Pattern, loc_closure_param(min_indent)),
                        min_indent,
                        ELambda::IndentArg,
                        ELambda::IndentArrow
                    ),
                    ELambda::Arg,
                ),
                skip_first!(
                    // Parse the -> which separates params from body
                    word2(b'-', b'>', ELambda::Arrow),
                    // Parse the body
                    space0_before_e(
                        specialize_ref(ELambda::Body, move |arena, state| {
                            parse_loc_expr_with_options(min_indent, options, arena, state)
                        }),
                        min_indent,
                        ELambda::IndentBody
                    )
                )
            )
        ),
        |arena: &'a Bump, (params, loc_body)| {
            let params: Vec<'a, Loc<Pattern<'a>>> = params;
            let params: &'a [Loc<Pattern<'a>>] = params.into_bump_slice();

            Expr::Closure(params, arena.alloc(loc_body))
        }
    )
}

mod when {
    use super::*;
    use crate::ast::WhenBranch;

    /// Parser for when expressions.
    pub fn expr_help<'a>(
        min_indent: u32,
        options: ExprParseOptions,
    ) -> impl Parser<'a, Expr<'a>, EWhen<'a>> {
        then(
            and!(
                when_with_indent(),
                skip_second!(
                    space0_around_ee(
                        specialize_ref(EWhen::Condition, move |arena, state| {
                            parse_loc_expr_with_options(min_indent, options, arena, state)
                        }),
                        min_indent,
                        EWhen::IndentCondition,
                        EWhen::IndentIs,
                    ),
                    parser::keyword_e(keyword::IS, EWhen::Is)
                )
            ),
            move |arena, state, progress, (case_indent, loc_condition)| {
                if case_indent < min_indent {
                    return Err((
                        progress,
                        // TODO maybe pass case_indent here?
                        EWhen::PatternAlignment(5, state.pos()),
                        state,
                    ));
                }

                // Everything in the branches must be indented at least as much as the case itself.
                let min_indent = case_indent;

                let (p1, branches, state) = branches(min_indent, options).parse(arena, state)?;

                Ok((
                    progress.or(p1),
                    Expr::When(arena.alloc(loc_condition), branches.into_bump_slice()),
                    state,
                ))
            },
        )
    }

    /// Parsing when with indentation.
    fn when_with_indent<'a>() -> impl Parser<'a, u32, EWhen<'a>> {
        move |arena, state: State<'a>| {
            parser::keyword_e(keyword::WHEN, EWhen::When)
                .parse(arena, state)
                .map(|(progress, (), state)| (progress, state.indent_column, state))
        }
    }

    fn branches<'a>(
        min_indent: u32,
        options: ExprParseOptions,
    ) -> impl Parser<'a, Vec<'a, &'a WhenBranch<'a>>, EWhen<'a>> {
        move |arena, state: State<'a>| {
            let when_indent = state.indent_column;

            let mut branches: Vec<'a, &'a WhenBranch<'a>> = Vec::with_capacity_in(2, arena);

            // 1. Parse the first branch and get its indentation level. (It must be >= min_indent.)
            // 2. Parse the other branches. Their indentation levels must be == the first branch's.

            let (_, ((pattern_indent_level, loc_first_patterns), loc_first_guard), mut state): (
                _,
                ((_, _), _),
                State<'a>,
            ) = branch_alternatives(min_indent, options, None).parse(arena, state)?;
            let original_indent = pattern_indent_level;

            state.indent_column = pattern_indent_level;

            // Parse the first "->" and the expression after it.
            let (_, loc_first_expr, mut state) =
                branch_result(original_indent + 1).parse(arena, state)?;

            // Record this as the first branch, then optionally parse additional branches.
            branches.push(arena.alloc(WhenBranch {
                patterns: loc_first_patterns.into_bump_slice(),
                value: loc_first_expr,
                guard: loc_first_guard,
            }));

            let branch_parser = map!(
                and!(
                    then(
                        branch_alternatives(min_indent, options, Some(pattern_indent_level)),
                        move |_arena, state, _, ((indent_column, loc_patterns), loc_guard)| {
                            if pattern_indent_level == indent_column {
                                Ok((MadeProgress, (loc_patterns, loc_guard), state))
                            } else {
                                let indent = pattern_indent_level - indent_column;
                                Err((
                                    MadeProgress,
                                    EWhen::PatternAlignment(indent, state.pos()),
                                    state,
                                ))
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
                match branch_parser.parse(arena, state) {
                    Ok((_, next_output, next_state)) => {
                        state = next_state;

                        branches.push(arena.alloc(next_output));
                    }
                    Err((MadeProgress, problem, old_state)) => {
                        return Err((MadeProgress, problem, old_state));
                    }
                    Err((NoProgress, _, old_state)) => {
                        state = old_state;

                        break;
                    }
                }
            }

            let mut state = state;
            state.indent_column = when_indent;

            Ok((MadeProgress, branches, state))
        }
    }

    /// Parsing alternative patterns in when branches.
    fn branch_alternatives<'a>(
        min_indent: u32,
        options: ExprParseOptions,
        pattern_indent_level: Option<u32>,
    ) -> impl Parser<'a, ((u32, Vec<'a, Loc<Pattern<'a>>>), Option<Loc<Expr<'a>>>), EWhen<'a>> {
        let options = ExprParseOptions {
            check_for_arrow: false,
            ..options
        };
        and!(
            branch_alternatives_help(min_indent, pattern_indent_level),
            one_of![
                map!(
                    skip_first!(
                        parser::keyword_e(keyword::IF, EWhen::IfToken),
                        // TODO we should require space before the expression but not after
                        space0_around_ee(
                            specialize_ref(EWhen::IfGuard, move |arena, state| {
                                parse_loc_expr_with_options(min_indent + 1, options, arena, state)
                            }),
                            min_indent,
                            EWhen::IndentIfGuard,
                            EWhen::IndentArrow,
                        )
                    ),
                    Some
                ),
                |_, s| Ok((NoProgress, None, s))
            ]
        )
    }

    fn branch_single_alternative<'a>(
        min_indent: u32,
    ) -> impl Parser<'a, Loc<Pattern<'a>>, EWhen<'a>> {
        move |arena, state| {
            let (_, spaces, state) =
                backtrackable(space0_e(min_indent, EWhen::IndentPattern)).parse(arena, state)?;

            let (_, loc_pattern, state) = space0_after_e(
                specialize(EWhen::Pattern, crate::pattern::loc_pattern_help(min_indent)),
                min_indent,
                EWhen::IndentPattern,
            )
            .parse(arena, state)?;

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
        min_indent: u32,
        pattern_indent_level: Option<u32>,
    ) -> impl Parser<'a, (u32, Vec<'a, Loc<Pattern<'a>>>), EWhen<'a>> {
        move |arena, state: State<'a>| {
            let initial = state.clone();

            // put no restrictions on the indent after the spaces; we'll check it manually
            match space0_e(0, EWhen::IndentPattern).parse(arena, state) {
                Err((MadeProgress, fail, _)) => Err((NoProgress, fail, initial)),
                Err((NoProgress, fail, _)) => Err((NoProgress, fail, initial)),
                Ok((_progress, spaces, state)) => {
                    match pattern_indent_level {
                        Some(wanted) if state.column() > wanted => {
                            // this branch is indented too much
                            Err((NoProgress, EWhen::IndentPattern(state.pos()), initial))
                        }
                        Some(wanted) if state.column() < wanted => {
                            let indent = wanted - state.column();
                            Err((
                                NoProgress,
                                EWhen::PatternAlignment(indent, state.pos()),
                                initial,
                            ))
                        }
                        _ => {
                            let pattern_indent =
                                min_indent.max(pattern_indent_level.unwrap_or(min_indent));
                            // the region is not reliable for the indent column in the case of
                            // parentheses around patterns
                            let pattern_indent_column = state.column();

                            let parser = sep_by1(
                                word1(b'|', EWhen::Bar),
                                branch_single_alternative(pattern_indent + 1),
                            );

                            match parser.parse(arena, state) {
                                Err((MadeProgress, fail, state)) => {
                                    Err((MadeProgress, fail, state))
                                }
                                Err((NoProgress, fail, _)) => {
                                    // roll back space parsing if the pattern made no progress
                                    Err((NoProgress, fail, initial))
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
        skip_first!(
            word2(b'-', b'>', EWhen::Arrow),
            space0_before_e(
                specialize_ref(EWhen::Branch, move |arena, state| parse_loc_expr(
                    indent, arena, state
                )),
                indent,
                EWhen::IndentBranch,
            )
        )
    }
}

fn if_branch<'a>(min_indent: u32) -> impl Parser<'a, (Loc<Expr<'a>>, Loc<Expr<'a>>), EIf<'a>> {
    move |arena, state| {
        // NOTE: only parse spaces before the expression
        let (_, cond, state) = space0_around_ee(
            specialize_ref(EIf::Condition, move |arena, state| {
                parse_loc_expr(min_indent, arena, state)
            }),
            min_indent,
            EIf::IndentCondition,
            EIf::IndentThenToken,
        )
        .parse(arena, state)
        .map_err(|(_, f, s)| (MadeProgress, f, s))?;

        let (_, _, state) = parser::keyword_e(keyword::THEN, EIf::Then)
            .parse(arena, state)
            .map_err(|(_, f, s)| (MadeProgress, f, s))?;

        let (_, then_branch, state) = space0_around_ee(
            specialize_ref(EIf::ThenBranch, move |arena, state| {
                parse_loc_expr(min_indent, arena, state)
            }),
            min_indent,
            EIf::IndentThenBranch,
            EIf::IndentElseToken,
        )
        .parse(arena, state)
        .map_err(|(_, f, s)| (MadeProgress, f, s))?;

        let (_, _, state) = parser::keyword_e(keyword::ELSE, EIf::Else)
            .parse(arena, state)
            .map_err(|(_, f, s)| (MadeProgress, f, s))?;

        Ok((MadeProgress, (cond, then_branch), state))
    }
}

fn expect_help<'a>(
    min_indent: u32,
    options: ExprParseOptions,
) -> impl Parser<'a, Expr<'a>, EExpect<'a>> {
    move |arena: &'a Bump, state: State<'a>| {
        let start_column = state.column();

        let (_, _, state) =
            parser::keyword_e(keyword::EXPECT, EExpect::Expect).parse(arena, state)?;

        let (_, condition, state) = space0_before_e(
            specialize_ref(EExpect::Condition, move |arena, state| {
                parse_loc_expr_with_options(start_column + 1, options, arena, state)
            }),
            start_column + 1,
            EExpect::IndentCondition,
        )
        .parse(arena, state)
        .map_err(|(_, f, s)| (MadeProgress, f, s))?;

        let parse_cont = specialize_ref(
            EExpect::Continuation,
            space0_before_e(
                move |a, s| parse_loc_expr(min_indent, a, s),
                min_indent,
                EExpr::IndentEnd,
            ),
        );

        let (_, loc_cont, state) = parse_cont.parse(arena, state)?;

        let expr = Expr::Expect(arena.alloc(condition), arena.alloc(loc_cont));

        Ok((MadeProgress, expr, state))
    }
}

fn if_expr_help<'a>(
    min_indent: u32,
    options: ExprParseOptions,
) -> impl Parser<'a, Expr<'a>, EIf<'a>> {
    move |arena: &'a Bump, state| {
        let (_, _, state) = parser::keyword_e(keyword::IF, EIf::If).parse(arena, state)?;

        let mut branches = Vec::with_capacity_in(1, arena);

        let mut loop_state = state;

        let state_final_else = loop {
            let (_, (cond, then_branch), state) = if_branch(min_indent).parse(arena, loop_state)?;

            branches.push((cond, then_branch));

            // try to parse another `if`
            // NOTE this drops spaces between the `else` and the `if`
            let optional_if = and!(
                backtrackable(space0_e(min_indent, EIf::IndentIf)),
                parser::keyword_e(keyword::IF, EIf::If)
            );

            match optional_if.parse(arena, state) {
                Err((_, _, state)) => break state,
                Ok((_, _, state)) => {
                    loop_state = state;
                    continue;
                }
            }
        };

        let (_, else_branch, state) = space0_before_e(
            specialize_ref(EIf::ElseBranch, move |arena, state| {
                parse_loc_expr_with_options(min_indent, options, arena, state)
            }),
            min_indent,
            EIf::IndentElseBranch,
        )
        .parse(arena, state_final_else)
        .map_err(|(_, f, s)| (MadeProgress, f, s))?;

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
    move |arena, state: State<'a>| {
        let indent_column = state.indent_column;

        let (progress, _, state) = parser.parse(arena, state)?;

        Ok((progress, indent_column, state))
    }
}

fn ident_to_expr<'a>(arena: &'a Bump, src: Ident<'a>) -> Expr<'a> {
    match src {
        Ident::GlobalTag(string) => Expr::GlobalTag(string),
        Ident::PrivateTag(string) => Expr::PrivateTag(string),
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
                answer = Expr::Access(arena.alloc(answer), field);
            }

            answer
        }
        Ident::AccessorFunction(string) => Expr::AccessorFunction(string),
        Ident::Malformed(string, problem) => Expr::MalformedIdent(string, problem),
    }
}

fn list_literal_help<'a>(min_indent: u32) -> impl Parser<'a, Expr<'a>, EList<'a>> {
    move |arena, state| {
        let (_, elements, state) = collection_trailing_sep_e!(
            word1(b'[', EList::Open),
            specialize_ref(
                EList::Expr,
                move |a, s| parse_loc_expr_no_multi_backpassing(min_indent, a, s)
            ),
            word1(b',', EList::End),
            word1(b']', EList::End),
            min_indent,
            EList::Open,
            EList::IndentEnd,
            Expr::SpaceBefore
        )
        .parse(arena, state)?;

        let elements = elements.ptrify_items(arena);
        let expr = Expr::List(elements);

        Ok((MadeProgress, expr, state))
    }
}

fn record_field_help<'a>(
    min_indent: u32,
) -> impl Parser<'a, AssignedField<'a, Expr<'a>>, ERecord<'a>> {
    use AssignedField::*;

    move |arena, state: State<'a>| {
        // You must have a field name, e.g. "email"
        let (progress, loc_label, state) =
            specialize(|_, pos| ERecord::Field(pos), loc!(lowercase_ident()))
                .parse(arena, state)?;
        debug_assert_eq!(progress, MadeProgress);

        let (_, spaces, state) = space0_e(min_indent, ERecord::IndentColon).parse(arena, state)?;

        // Having a value is optional; both `{ email }` and `{ email: blah }` work.
        // (This is true in both literals and types.)
        let (_, opt_loc_val, state) = optional(and!(
            either!(
                word1(b':', ERecord::Colon),
                word1(b'?', ERecord::QuestionMark)
            ),
            space0_before_e(
                specialize_ref(ERecord::Expr, move |a, s| {
                    parse_loc_expr_no_multi_backpassing(min_indent, a, s)
                }),
                min_indent,
                ERecord::IndentEnd,
            )
        ))
        .parse(arena, state)?;

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

fn record_help<'a>(
    min_indent: u32,
) -> impl Parser<
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
                    min_indent,
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
                            space0_around_ee(
                                loc!(record_field_help(min_indent)),
                                min_indent,
                                ERecord::IndentEnd,
                                ERecord::IndentEnd
                            ),
                        ),
                        space0_e(min_indent, ERecord::IndentEnd)
                    ),
                    word1(b'}', ERecord::End)
                )
            ))
        )
    )
}

fn record_literal_help<'a>(min_indent: u32) -> impl Parser<'a, Expr<'a>, EExpr<'a>> {
    then(
        loc!(specialize(EExpr::Record, record_help(min_indent))),
        move |arena, state, _, loc_record| {
            let (opt_update, loc_assigned_fields_with_comments) = loc_record.value;

            // This is a record literal, not a destructure.
            let mut value = match opt_update {
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

            // there can be field access, e.g. `{ x : 4 }.x`
            let (_, accesses, state) = optional(record_field_access_chain()).parse(arena, state)?;

            if let Some(fields) = accesses {
                for field in fields {
                    // Wrap the previous answer in the new one, so we end up
                    // with a nested Expr. That way, `foo.bar.baz` gets represented
                    // in the AST as if it had been written (foo.bar).baz all along.
                    value = Expr::Access(arena.alloc(value), field);
                }
            }

            Ok((MadeProgress, value, state))
        },
    )
}

fn string_literal_help<'a>() -> impl Parser<'a, Expr<'a>, EString<'a>> {
    map!(crate::string_literal::parse(), Expr::Str)
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

fn operator<'a>() -> impl Parser<'a, BinOp, EExpr<'a>> {
    |_, state| operator_help(EExpr::Start, EExpr::BadOperator, state)
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
            Err((MadeProgress, to_error($op, state.pos()), state))
        }};
    }

    match chomped {
        "" => Err((NoProgress, to_expectation(state.pos()), state)),
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
            Err((NoProgress, to_error(".", state.pos()), state))
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
        "%%" => good!(BinOp::DoublePercent, 2),
        "->" => {
            // makes no progress, so it does not interfere with `_ if isGood -> ...`
            Err((NoProgress, to_error("->", state.pos()), state))
        }
        "<-" => good!(BinOp::Backpassing, 2),
        _ => bad_made_progress!(chomped),
    }
}

fn chomp_ops(bytes: &[u8]) -> &str {
    let mut chomped = 0;

    for c in bytes.iter() {
        if !BINOP_CHAR_SET.contains(c) {
            break;
        }
        chomped += 1;
    }

    unsafe {
        // Safe because BINOP_CHAR_SET only contains ascii chars
        std::str::from_utf8_unchecked(&bytes[..chomped])
    }
}
