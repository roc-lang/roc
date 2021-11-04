use crate::ast::CommentOrNewline;
use crate::ast::Spaceable;
use crate::parser::{
    self, and, backtrackable, BadInputError, Col, Parser, Progress::*, Row, State,
};
use bumpalo::collections::vec::Vec;
use bumpalo::Bump;
use roc_region::all::Located;

pub fn space0_around_ee<'a, P, S, E>(
    parser: P,
    min_indent: u16,
    space_problem: fn(BadInputError, Row, Col) -> E,
    indent_before_problem: fn(Row, Col) -> E,
    indent_after_problem: fn(Row, Col) -> E,
) -> impl Parser<'a, Located<S>, E>
where
    S: Spaceable<'a>,
    S: 'a,
    P: Parser<'a, Located<S>, E>,
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
    min_indent: u16,
    space_problem: fn(BadInputError, Row, Col) -> E,
    indent_before_problem: fn(Row, Col) -> E,
    indent_after_problem: fn(Row, Col) -> E,
) -> impl Parser<'a, Located<S>, E>
where
    S: Spaceable<'a>,
    S: 'a,
    P: Parser<'a, Located<S>, E>,
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
        (Located<S>, &'a [CommentOrNewline<'a>]),
    ),
) -> Located<S>
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
    min_indent: u16,
    space_problem: fn(BadInputError, Row, Col) -> E,
    indent_problem: fn(Row, Col) -> E,
) -> impl Parser<'a, Located<S>, E>
where
    S: Spaceable<'a>,
    S: 'a,
    P: Parser<'a, Located<S>, E>,
    P: 'a,
    E: 'a,
{
    parser::map_with_arena(
        and!(space0_e(min_indent, space_problem, indent_problem), parser),
        |arena: &'a Bump, (space_list, loc_expr): (&'a [CommentOrNewline<'a>], Located<S>)| {
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
    min_indent: u16,
    space_problem: fn(BadInputError, Row, Col) -> E,
    indent_problem: fn(Row, Col) -> E,
) -> impl Parser<'a, Located<S>, E>
where
    S: Spaceable<'a>,
    S: 'a,
    P: Parser<'a, Located<S>, E>,
    P: 'a,
    E: 'a,
{
    parser::map_with_arena(
        and!(parser, space0_e(min_indent, space_problem, indent_problem)),
        |arena: &'a Bump, (loc_expr, space_list): (Located<S>, &'a [CommentOrNewline<'a>])| {
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
    min_indent: u16,
    indent_problem: fn(Row, Col) -> E,
) -> impl Parser<'a, (), E>
where
    E: 'a,
{
    move |_, state: State<'a>| {
        if state.column >= min_indent {
            Ok((NoProgress, (), state))
        } else {
            Err((NoProgress, indent_problem(state.line, state.column), state))
        }
    }
}

pub fn space0_e<'a, E>(
    min_indent: u16,
    space_problem: fn(BadInputError, Row, Col) -> E,
    indent_problem: fn(Row, Col) -> E,
) -> impl Parser<'a, &'a [CommentOrNewline<'a>], E>
where
    E: 'a,
{
    spaces_help_help(min_indent, space_problem, indent_problem)
}

#[inline(always)]
fn spaces_help_help<'a, E>(
    min_indent: u16,
    space_problem: fn(BadInputError, Row, Col) -> E,
    indent_problem: fn(Row, Col) -> E,
) -> impl Parser<'a, &'a [CommentOrNewline<'a>], E>
where
    E: 'a,
{
    use SpaceState::*;

    move |arena, mut state: State<'a>| {
        let comments_and_newlines = Vec::new_in(arena);

        match eat_spaces(state.bytes, state.line, state.column, comments_and_newlines) {
            HasTab { row, col } => {
                // there was a tab character
                Err((
                    MadeProgress,
                    space_problem(BadInputError::HasTab, row, col),
                    State {
                        line: row,
                        column: col,
                        ..state
                    },
                ))
            }
            Good {
                row,
                col,
                bytes,
                comments_and_newlines,
            } => {
                if bytes == state.bytes {
                    Ok((NoProgress, &[] as &[_], state))
                } else if state.line != row {
                    // we parsed at least one newline

                    state.indent_col = col;

                    if col >= min_indent {
                        state.line = row;
                        state.column = col;
                        state.bytes = bytes;

                        Ok((MadeProgress, comments_and_newlines.into_bump_slice(), state))
                    } else {
                        Err((
                            MadeProgress,
                            indent_problem(state.line, state.column),
                            state,
                        ))
                    }
                } else {
                    state.column = col;
                    state.bytes = bytes;

                    Ok((MadeProgress, comments_and_newlines.into_bump_slice(), state))
                }
            }
        }
    }
}

enum SpaceState<'a> {
    Good {
        row: Row,
        col: Col,
        bytes: &'a [u8],
        comments_and_newlines: Vec<'a, CommentOrNewline<'a>>,
    },
    HasTab {
        row: Row,
        col: Col,
    },
}

fn eat_spaces<'a>(
    mut bytes: &'a [u8],
    mut row: Row,
    mut col: Col,
    mut comments_and_newlines: Vec<'a, CommentOrNewline<'a>>,
) -> SpaceState<'a> {
    use SpaceState::*;

    for c in bytes {
        match c {
            b' ' => {
                bytes = &bytes[1..];
                col += 1;
            }
            b'\n' => {
                bytes = &bytes[1..];
                row += 1;
                col = 0;
                comments_and_newlines.push(CommentOrNewline::Newline);
            }
            b'\r' => {
                bytes = &bytes[1..];
            }
            b'\t' => {
                return HasTab { row, col };
            }
            b'#' => {
                return eat_line_comment(&bytes[1..], row, col + 1, comments_and_newlines);
            }
            _ => break,
        }
    }

    Good {
        row,
        col,
        bytes,
        comments_and_newlines,
    }
}

fn eat_line_comment<'a>(
    mut bytes: &'a [u8],
    row: Row,
    mut col: Col,
    mut comments_and_newlines: Vec<'a, CommentOrNewline<'a>>,
) -> SpaceState<'a> {
    use SpaceState::*;

    let is_doc_comment = if let Some(b'#') = bytes.get(0) {
        match bytes.get(1) {
            Some(b' ') => {
                bytes = &bytes[2..];
                col += 2;

                true
            }
            Some(b'\n') => {
                // consume the second # and the \n
                bytes = &bytes[2..];

                comments_and_newlines.push(CommentOrNewline::DocComment(""));
                return eat_spaces(bytes, row + 1, 0, comments_and_newlines);
            }
            None => {
                // consume the second #
                col += 1;
                bytes = &bytes[1..];

                return Good {
                    row,
                    col,
                    bytes,
                    comments_and_newlines,
                };
            }

            _ => false,
        }
    } else {
        false
    };

    let initial = bytes;
    let initial_col = col;

    for c in bytes {
        match c {
            b'\t' => return HasTab { row, col },
            b'\n' => {
                let delta = (col - initial_col) as usize;
                let comment = unsafe { std::str::from_utf8_unchecked(&initial[..delta]) };

                if is_doc_comment {
                    comments_and_newlines.push(CommentOrNewline::DocComment(comment));
                } else {
                    comments_and_newlines.push(CommentOrNewline::LineComment(comment));
                }
                return eat_spaces(&bytes[1..], row + 1, 0, comments_and_newlines);
            }
            _ => {
                bytes = &bytes[1..];
                col += 1;
            }
        }
    }

    Good {
        row,
        col,
        bytes,
        comments_and_newlines,
    }
}
