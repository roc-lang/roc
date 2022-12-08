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
    indent_before_problem: fn(Position) -> E,
    indent_after_problem: fn(Position) -> E,
) -> impl Parser<'a, Loc<S>, E>
where
    S: 'a + Spaceable<'a>,
    P: 'a + Parser<'a, Loc<S>, E>,
    E: 'a + SpaceProblem,
{
    parser::map_with_arena(
        and(
            space0_e(indent_before_problem),
            and(parser, space0_e(indent_after_problem)),
        ),
        spaces_around_help,
    )
}

pub fn space0_around_e_no_after_indent_check<'a, P, S, E>(
    parser: P,
    indent_before_problem: fn(Position) -> E,
) -> impl Parser<'a, Loc<S>, E>
where
    S: 'a + Spaceable<'a>,
    P: 'a + Parser<'a, Loc<S>, E>,
    E: 'a + SpaceProblem,
{
    parser::map_with_arena(
        and(space0_e(indent_before_problem), and(parser, spaces())),
        spaces_around_help,
    )
}

pub fn space0_before_optional_after<'a, P, S, E>(
    parser: P,
    indent_before_problem: fn(Position) -> E,
    indent_after_problem: fn(Position) -> E,
) -> impl Parser<'a, Loc<S>, E>
where
    S: 'a + Spaceable<'a>,
    P: 'a + Parser<'a, Loc<S>, E>,
    E: 'a + SpaceProblem,
{
    parser::map_with_arena(
        and(
            space0_e(indent_before_problem),
            and(
                parser,
                one_of![
                    backtrackable(space0_e(indent_after_problem)),
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
    S: 'a + Spaceable<'a>,
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
    indent_problem: fn(Position) -> E,
) -> impl Parser<'a, Loc<S>, E>
where
    S: 'a + Spaceable<'a>,
    P: 'a + Parser<'a, Loc<S>, E>,
    E: 'a + SpaceProblem,
{
    parser::map_with_arena(
        and!(space0_e(indent_problem), parser),
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
    indent_problem: fn(Position) -> E,
) -> impl Parser<'a, Loc<S>, E>
where
    S: 'a + Spaceable<'a>,
    P: 'a + Parser<'a, Loc<S>, E>,
    E: 'a + SpaceProblem,
{
    parser::map_with_arena(
        and!(parser, space0_e(indent_problem)),
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

pub fn check_indent<'a, E>(indent_problem: fn(Position) -> E) -> impl Parser<'a, (), E>
where
    E: 'a,
{
    move |_, state: State<'a>, min_indent: u32| {
        if state.column() >= min_indent {
            Ok((NoProgress, (), state))
        } else {
            Err((NoProgress, indent_problem(state.pos())))
        }
    }
}

fn eat_whitespace(bytes: &[u8]) -> usize {
    let mut i = 0;
    while i < bytes.len() {
        match bytes[i] {
            b' ' => i += 1,
            _ => break,
        }
    }
    i
}

fn eat_until_newline(bytes: &[u8]) -> usize {
    let mut i = 0;
    while i < bytes.len() {
        if bytes[i] < b' ' {
            break;
        } else {
            i += 1;
        }
    }
    i
}

pub fn space0_e<'a, E>(
    indent_problem: fn(Position) -> E,
) -> impl Parser<'a, &'a [CommentOrNewline<'a>], E>
where
    E: 'a + SpaceProblem,
{
    move |arena, state: State<'a>, min_indent: u32| {
        let start = state.pos();
        match spaces().parse(arena, state, min_indent) {
            Ok((progress, spaces, state)) => {
                if progress == NoProgress || state.column() >= min_indent {
                    Ok((progress, spaces, state))
                } else {
                    Err((progress, indent_problem(start)))
                }
            }
            Err((progress, err)) => Err((progress, err)),
        }
    }
}

fn spaces<'a, E>() -> impl Parser<'a, &'a [CommentOrNewline<'a>], E>
where
    E: 'a + SpaceProblem,
{
    move |arena, mut state: State<'a>, _min_indent: u32| {
        let mut newlines = Vec::new_in(arena);
        let mut progress = NoProgress;
        loop {
            let whitespace = eat_whitespace(state.bytes());
            if whitespace > 0 {
                state.advance_mut(whitespace);
                progress = MadeProgress;
            }

            match state.bytes().first() {
                Some(b'#') => {
                    state.advance_mut(1);

                    let is_doc_comment = state.bytes().first() == Some(&b'#')
                        && (state.bytes().get(1) == Some(&b' ')
                            || state.bytes().get(1) == Some(&b'\n')
                            || state.bytes().get(1) == None);

                    if is_doc_comment {
                        state.advance_mut(1);
                        if state.bytes().first() == Some(&b' ') {
                            state.advance_mut(1);
                        }
                    }

                    let len = eat_until_newline(state.bytes());

                    // We already checked that the string is valid UTF-8
                    debug_assert!(std::str::from_utf8(&state.bytes()[..len]).is_ok());
                    let text = unsafe { std::str::from_utf8_unchecked(&state.bytes()[..len]) };

                    let comment = if is_doc_comment {
                        CommentOrNewline::DocComment(text)
                    } else {
                        CommentOrNewline::LineComment(text)
                    };
                    newlines.push(comment);
                    state.advance_mut(len);

                    if state.bytes().first() == Some(&b'\n') {
                        state = state.advance_newline();
                    }

                    progress = MadeProgress;
                }
                Some(b'\r') => {
                    if state.bytes().get(1) == Some(&b'\n') {
                        newlines.push(CommentOrNewline::Newline);
                        state.advance_mut(1);
                        state = state.advance_newline();
                        progress = MadeProgress;
                    } else {
                        return Err((
                            progress,
                            E::space_problem(
                                BadInputError::HasMisplacedCarriageReturn,
                                state.pos(),
                            ),
                        ));
                    }
                }
                Some(b'\n') => {
                    newlines.push(CommentOrNewline::Newline);
                    state = state.advance_newline();
                    progress = MadeProgress;
                }
                Some(b'\t') => {
                    return Err((
                        progress,
                        E::space_problem(BadInputError::HasTab, state.pos()),
                    ));
                }
                Some(x) if *x < b' ' => {
                    return Err((
                        progress,
                        E::space_problem(BadInputError::HasAsciiControl, state.pos()),
                    ));
                }
                _ => {
                    if !newlines.is_empty() {
                        state = state.mark_current_indent();
                    }
                    break;
                }
            }
        }

        Ok((progress, newlines.into_bump_slice(), state))
    }
}
