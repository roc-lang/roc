use crate::ast::CommentOrNewline;
use crate::ast::Spaceable;
use crate::parser::{self, and, backtrackable, BadInputError, Parser, Progress::*};
use crate::state::State;
use bumpalo::collections::vec::Vec;
use bumpalo::Bump;
use roc_region::all::Loc;
use roc_region::all::Position;

pub fn space0_around_ee<'a, P, S, E>(
    parser: P,
    min_indent: u32,
    space_problem: fn(BadInputError, Position) -> E,
    indent_before_problem: fn(Position) -> E,
    indent_after_problem: fn(Position) -> E,
) -> impl Parser<'a, Loc<S>, E>
where
    S: Spaceable<'a>,
    S: 'a,
    P: Parser<'a, Loc<S>, E>,
    P: 'a,
    E: 'a,
{
    parser::map_with_arena(
        and(
            space0_e(min_indent, space_problem, indent_before_problem),
            and(
                parser,
                space0_e(min_indent, space_problem, indent_after_problem),
            ),
        ),
        spaces_around_help,
    )
}

pub fn space0_before_optional_after<'a, P, S, E>(
    parser: P,
    min_indent: u32,
    space_problem: fn(BadInputError, Position) -> E,
    indent_before_problem: fn(Position) -> E,
    indent_after_problem: fn(Position) -> E,
) -> impl Parser<'a, Loc<S>, E>
where
    S: Spaceable<'a>,
    S: 'a,
    P: Parser<'a, Loc<S>, E>,
    P: 'a,
    E: 'a,
{
    parser::map_with_arena(
        and(
            space0_e(min_indent, space_problem, indent_before_problem),
            and(
                parser,
                one_of![
                    backtrackable(space0_e(min_indent, space_problem, indent_after_problem)),
                    succeed!(&[] as &[_]),
                ],
            ),
        ),
        spaces_around_help,
    )
}

fn spaces_around_help<'a, S>(
    arena: &'a Bump,
    tuples: (
        &'a [CommentOrNewline<'a>],
        (Loc<S>, &'a [CommentOrNewline<'a>]),
    ),
) -> Loc<S>
where
    S: Spaceable<'a>,
    S: 'a,
{
    let (spaces_before, (loc_val, spaces_after)) = tuples;

    if spaces_before.is_empty() {
        if spaces_after.is_empty() {
            loc_val
        } else {
            arena
                .alloc(loc_val.value)
                .with_spaces_after(spaces_after, loc_val.region)
        }
    } else if spaces_after.is_empty() {
        arena
            .alloc(loc_val.value)
            .with_spaces_before(spaces_before, loc_val.region)
    } else {
        let wrapped_expr = arena
            .alloc(loc_val.value)
            .with_spaces_after(spaces_after, loc_val.region);

        arena
            .alloc(wrapped_expr.value)
            .with_spaces_before(spaces_before, wrapped_expr.region)
    }
}

pub fn space0_before_e<'a, P, S, E>(
    parser: P,
    min_indent: u32,
    space_problem: fn(BadInputError, Position) -> E,
    indent_problem: fn(Position) -> E,
) -> impl Parser<'a, Loc<S>, E>
where
    S: Spaceable<'a>,
    S: 'a,
    P: Parser<'a, Loc<S>, E>,
    P: 'a,
    E: 'a,
{
    parser::map_with_arena(
        and!(space0_e(min_indent, space_problem, indent_problem), parser),
        |arena: &'a Bump, (space_list, loc_expr): (&'a [CommentOrNewline<'a>], Loc<S>)| {
            if space_list.is_empty() {
                loc_expr
            } else {
                arena
                    .alloc(loc_expr.value)
                    .with_spaces_before(space_list, loc_expr.region)
            }
        },
    )
}

pub fn space0_after_e<'a, P, S, E>(
    parser: P,
    min_indent: u32,
    space_problem: fn(BadInputError, Position) -> E,
    indent_problem: fn(Position) -> E,
) -> impl Parser<'a, Loc<S>, E>
where
    S: Spaceable<'a>,
    S: 'a,
    P: Parser<'a, Loc<S>, E>,
    P: 'a,
    E: 'a,
{
    parser::map_with_arena(
        and!(parser, space0_e(min_indent, space_problem, indent_problem)),
        |arena: &'a Bump, (loc_expr, space_list): (Loc<S>, &'a [CommentOrNewline<'a>])| {
            if space_list.is_empty() {
                loc_expr
            } else {
                arena
                    .alloc(loc_expr.value)
                    .with_spaces_after(space_list, loc_expr.region)
            }
        },
    )
}

pub fn check_indent<'a, E>(
    min_indent: u32,
    indent_problem: fn(Position) -> E,
) -> impl Parser<'a, (), E>
where
    E: 'a,
{
    move |_, state: State<'a>| {
        if state.column() >= min_indent {
            Ok((NoProgress, (), state))
        } else {
            Err((NoProgress, indent_problem(state.pos()), state))
        }
    }
}

pub fn space0_e<'a, E>(
    min_indent: u32,
    space_problem: fn(BadInputError, Position) -> E,
    indent_problem: fn(Position) -> E,
) -> impl Parser<'a, &'a [CommentOrNewline<'a>], E>
where
    E: 'a,
{
    spaces_help_help(min_indent, space_problem, indent_problem)
}

#[inline(always)]
fn spaces_help_help<'a, E>(
    min_indent: u32,
    space_problem: fn(BadInputError, Position) -> E,
    indent_problem: fn(Position) -> E,
) -> impl Parser<'a, &'a [CommentOrNewline<'a>], E>
where
    E: 'a,
{
    use SpaceState::*;

    move |arena, state: State<'a>| {
        let comments_and_newlines = Vec::new_in(arena);
        match eat_spaces(state.clone(), false, comments_and_newlines) {
            HasTab(state) => Err((
                MadeProgress,
                space_problem(BadInputError::HasTab, state.pos()),
                state,
            )),
            Good {
                state: mut new_state,
                multiline,
                comments_and_newlines,
            } => {
                if new_state.bytes() == state.bytes() {
                    Ok((NoProgress, &[] as &[_], state))
                } else if multiline {
                    // we parsed at least one newline

                    new_state.indent_column = new_state.column();

                    if new_state.column() >= min_indent {
                        Ok((
                            MadeProgress,
                            comments_and_newlines.into_bump_slice(),
                            new_state,
                        ))
                    } else {
                        Err((MadeProgress, indent_problem(state.pos()), state))
                    }
                } else {
                    Ok((
                        MadeProgress,
                        comments_and_newlines.into_bump_slice(),
                        new_state,
                    ))
                }
            }
        }
    }
}

enum SpaceState<'a> {
    Good {
        state: State<'a>,
        multiline: bool,
        comments_and_newlines: Vec<'a, CommentOrNewline<'a>>,
    },
    HasTab(State<'a>),
}

fn eat_spaces<'a>(
    mut state: State<'a>,
    mut multiline: bool,
    mut comments_and_newlines: Vec<'a, CommentOrNewline<'a>>,
) -> SpaceState<'a> {
    use SpaceState::*;

    for c in state.bytes() {
        match c {
            b' ' => {
                state = state.advance(1);
            }
            b'\n' => {
                state = state.advance_newline();
                multiline = true;
                comments_and_newlines.push(CommentOrNewline::Newline);
            }
            b'\r' => {
                state = state.advance_newline();
            }
            b'\t' => {
                return HasTab(state);
            }
            b'#' => {
                state = state.advance(1);
                return eat_line_comment(state, multiline, comments_and_newlines);
            }
            _ => break,
        }
    }

    Good {
        state,
        multiline,
        comments_and_newlines,
    }
}

fn eat_line_comment<'a>(
    mut state: State<'a>,
    mut multiline: bool,
    mut comments_and_newlines: Vec<'a, CommentOrNewline<'a>>,
) -> SpaceState<'a> {
    use SpaceState::*;

    let is_doc_comment = if let Some(b'#') = state.bytes().get(0) {
        match state.bytes().get(1) {
            Some(b' ') => {
                state = state.advance(2);

                true
            }
            Some(b'\n') => {
                // consume the second # and the \n
                state = state.advance(1);
                state = state.advance_newline();

                comments_and_newlines.push(CommentOrNewline::DocComment(""));
                multiline = true;
                return eat_spaces(state, multiline, comments_and_newlines);
            }
            None => {
                // consume the second #
                state = state.advance(1);

                return Good {
                    state,
                    multiline,
                    comments_and_newlines,
                };
            }

            _ => false,
        }
    } else {
        false
    };

    let initial = state.bytes();

    for c in state.bytes() {
        match c {
            b'\t' => return HasTab(state),
            b'\n' => {
                let delta = initial.len() - state.bytes().len();
                let comment = unsafe { std::str::from_utf8_unchecked(&initial[..delta]) };

                if is_doc_comment {
                    comments_and_newlines.push(CommentOrNewline::DocComment(comment));
                } else {
                    comments_and_newlines.push(CommentOrNewline::LineComment(comment));
                }
                state = state.advance_newline();
                multiline = true;
                return eat_spaces(state, multiline, comments_and_newlines);
            }
            b'\r' => {
                state = state.advance_newline();
            }
            _ => {
                state = state.advance(1);
            }
        }
    }

    // We made it to the end of the bytes. This means there's a comment without a trailing newline.
    let delta = initial.len() - state.bytes().len();
    let comment = unsafe { std::str::from_utf8_unchecked(&initial[..delta]) };

    if is_doc_comment {
        comments_and_newlines.push(CommentOrNewline::DocComment(comment));
    } else {
        comments_and_newlines.push(CommentOrNewline::LineComment(comment));
    }

    Good {
        state,
        multiline,
        comments_and_newlines,
    }
}
