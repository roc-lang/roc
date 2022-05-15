use crate::ast::CommentOrNewline;
use crate::ast::Spaceable;
use crate::parser::SpaceProblem;
use crate::parser::{self, and, backtrackable, BadInputError, Parser, Progress::*};
use crate::state::State;
use bumpalo::collections::vec::Vec;
use bumpalo::Bump;
use roc_region::all::Loc;
use roc_region::all::Position;

pub fn space0_around_ee<'a, P, S, E>(
    parser: P,
    min_indent: u32,
    indent_before_problem: fn(Position) -> E,
    indent_after_problem: fn(Position) -> E,
) -> impl Parser<'a, Loc<S>, E>
where
    S: Spaceable<'a>,
    S: 'a,
    P: Parser<'a, Loc<S>, E>,
    P: 'a,
    E: 'a + SpaceProblem,
{
    parser::map_with_arena(
        and(
            space0_e(min_indent, indent_before_problem),
            and(parser, space0_e(min_indent, indent_after_problem)),
        ),
        spaces_around_help,
    )
}

pub fn space0_before_optional_after<'a, P, S, E>(
    parser: P,
    min_indent: u32,
    indent_before_problem: fn(Position) -> E,
    indent_after_problem: fn(Position) -> E,
) -> impl Parser<'a, Loc<S>, E>
where
    S: Spaceable<'a>,
    S: 'a,
    P: Parser<'a, Loc<S>, E>,
    P: 'a,
    E: 'a + SpaceProblem,
{
    parser::map_with_arena(
        and(
            space0_e(min_indent, indent_before_problem),
            and(
                parser,
                one_of![
                    backtrackable(space0_e(min_indent, indent_after_problem)),
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
    indent_problem: fn(Position) -> E,
) -> impl Parser<'a, Loc<S>, E>
where
    S: Spaceable<'a>,
    S: 'a,
    P: Parser<'a, Loc<S>, E>,
    P: 'a,
    E: 'a + SpaceProblem,
{
    parser::map_with_arena(
        and!(space0_e(min_indent, indent_problem), parser),
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
    indent_problem: fn(Position) -> E,
) -> impl Parser<'a, Loc<S>, E>
where
    S: Spaceable<'a>,
    S: 'a,
    P: Parser<'a, Loc<S>, E>,
    P: 'a,
    E: 'a + SpaceProblem,
{
    parser::map_with_arena(
        and!(parser, space0_e(min_indent, indent_problem)),
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
    indent_problem: fn(Position) -> E,
) -> impl Parser<'a, &'a [CommentOrNewline<'a>], E>
where
    E: 'a + SpaceProblem,
{
    spaces_help_help(min_indent, indent_problem)
}

#[inline(always)]
fn spaces_help_help<'a, E>(
    min_indent: u32,
    indent_problem: fn(Position) -> E,
) -> impl Parser<'a, &'a [CommentOrNewline<'a>], E>
where
    E: 'a + SpaceProblem,
{
    move |arena, state: State<'a>| {
        match fast_eat_spaces(&state) {
            FastSpaceState::HasTab(position) => Err((
                MadeProgress,
                E::space_problem(BadInputError::HasTab, position),
                state,
            )),
            FastSpaceState::Good {
                newlines,
                consumed,
                column,
            } => {
                if consumed == 0 {
                    Ok((NoProgress, &[] as &[_], state))
                } else if column < min_indent {
                    Err((MadeProgress, indent_problem(state.pos()), state))
                } else {
                    let comments_and_newlines =
                        Vec::with_capacity_in(newlines.saturating_sub(1), arena);
                    let spaces = eat_spaces(state, false, comments_and_newlines);
                    let mut state = spaces.state;

                    if spaces.multiline {
                        // we parsed at least one newline

                        state.indent_column = state.column();

                        debug_assert!(state.column() >= min_indent);
                    }

                    Ok((
                        MadeProgress,
                        spaces.comments_and_newlines.into_bump_slice(),
                        state,
                    ))
                }
            }
        }
    }
}

enum FastSpaceState {
    Good {
        newlines: usize,
        consumed: usize,
        column: u32,
    },
    HasTab(Position),
}

fn fast_eat_spaces(state: &State) -> FastSpaceState {
    use FastSpaceState::*;

    let mut newlines = 0;
    let mut index = 0;
    let mut line_start = state.line_start.offset as usize;
    let base_offset = state.pos().offset as usize;

    let bytes = state.bytes();
    let length = bytes.len();

    'outer: while index < length {
        match bytes[index] {
            b' ' => {
                index += 1;
            }
            b'\n' => {
                newlines += 1;
                index += 1;
                line_start = base_offset + index;
            }
            b'\r' => {
                index += 1;
                line_start = base_offset + index;
            }
            b'\t' => {
                return HasTab(Position::new(index as u32));
            }
            b'#' => {
                index += 1;

                while index < length {
                    match bytes[index] {
                        b'\n' | b'\t' | b'\r' => {
                            continue 'outer;
                        }

                        _ => {
                            index += 1;
                        }
                    }
                }
            }
            _ => break,
        }
    }

    Good {
        newlines,
        consumed: index,
        column: ((base_offset + index) - line_start) as u32,
    }
}

struct SpaceState<'a> {
    state: State<'a>,
    multiline: bool,
    comments_and_newlines: Vec<'a, CommentOrNewline<'a>>,
}

fn eat_spaces<'a>(
    mut state: State<'a>,
    mut multiline: bool,
    mut comments_and_newlines: Vec<'a, CommentOrNewline<'a>>,
) -> SpaceState<'a> {
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
            b'\t' => unreachable!(),

            b'#' => {
                state = state.advance(1);
                return eat_line_comment(state, multiline, comments_and_newlines);
            }
            _ => break,
        }
    }

    SpaceState {
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
    let mut index = 0;
    let bytes = state.bytes();
    let length = bytes.len();

    'outer: loop {
        let is_doc_comment = if let Some(b'#') = bytes.get(index) {
            match bytes.get(index + 1) {
                Some(b' ') => {
                    state = state.advance(2);
                    index += 2;

                    true
                }
                Some(b'\n') => {
                    // consume the second # and the \n
                    state = state.advance(1);
                    state = state.advance_newline();
                    index += 2;

                    comments_and_newlines.push(CommentOrNewline::DocComment(""));
                    multiline = true;

                    for c in state.bytes() {
                        match c {
                            b' ' => {
                                state = state.advance(1);
                            }
                            b'\n' => {
                                state = state.advance_newline();
                                index += 1;
                                multiline = true;
                                comments_and_newlines.push(CommentOrNewline::Newline);
                            }
                            b'\r' => {
                                state = state.advance_newline();
                            }
                            b'\t' => unreachable!(),
                            b'#' => {
                                state = state.advance(1);
                                index += 1;
                                continue 'outer;
                            }
                            _ => break,
                        }

                        index += 1;
                    }

                    return SpaceState {
                        state,
                        multiline,
                        comments_and_newlines,
                    };
                }
                None => {
                    // consume the second #
                    state = state.advance(1);

                    return SpaceState {
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

        let loop_start = index;

        while index < length {
            match bytes[index] {
                b'\t' => unreachable!(),
                b'\n' => {
                    let comment =
                        unsafe { std::str::from_utf8_unchecked(&bytes[loop_start..index]) };

                    if is_doc_comment {
                        comments_and_newlines.push(CommentOrNewline::DocComment(comment));
                    } else {
                        comments_and_newlines.push(CommentOrNewline::LineComment(comment));
                    }
                    state = state.advance_newline();
                    multiline = true;

                    index += 1;
                    while index < length {
                        match bytes[index] {
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
                            b'\t' => unreachable!(),
                            b'#' => {
                                state = state.advance(1);
                                index += 1;
                                continue 'outer;
                            }
                            _ => break,
                        }

                        index += 1;
                    }

                    return SpaceState {
                        state,
                        multiline,
                        comments_and_newlines,
                    };
                }
                b'\r' => {
                    state = state.advance_newline();
                }
                _ => {
                    state = state.advance(1);
                }
            }

            index += 1;
        }

        // We made it to the end of the bytes. This means there's a comment without a trailing newline.
        let comment = unsafe { std::str::from_utf8_unchecked(&bytes[loop_start..index]) };

        if is_doc_comment {
            comments_and_newlines.push(CommentOrNewline::DocComment(comment));
        } else {
            comments_and_newlines.push(CommentOrNewline::LineComment(comment));
        }

        return SpaceState {
            state,
            multiline,
            comments_and_newlines,
        };
    }
}
