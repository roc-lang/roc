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
        and(
            space0_e(indent_before_problem),
            and(parser, space0_no_after_indent_check()),
        ),
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

pub fn space0_e<'a, E>(
    indent_problem: fn(Position) -> E,
) -> impl Parser<'a, &'a [CommentOrNewline<'a>], E>
where
    E: 'a + SpaceProblem,
{
    spaces_help_help(indent_problem)
}

#[inline(always)]
fn spaces_help_help<'a, E>(
    indent_problem: fn(Position) -> E,
) -> impl Parser<'a, &'a [CommentOrNewline<'a>], E>
where
    E: 'a + SpaceProblem,
{
    move |arena, state: State<'a>, min_indent: u32| match fast_eat_spaces(&state) {
        FastSpaceState::HasTab(position) => Err((
            MadeProgress,
            E::space_problem(BadInputError::HasTab, position),
        )),
        FastSpaceState::Good {
            newlines,
            consumed,
            column,
        } => {
            if consumed == 0 {
                Ok((NoProgress, &[] as &[_], state))
            } else if column < min_indent {
                Err((MadeProgress, indent_problem(state.pos())))
            } else {
                let comments_and_newlines = Vec::with_capacity_in(newlines, arena);
                let spaces = eat_spaces(state, comments_and_newlines);

                Ok((
                    MadeProgress,
                    spaces.comments_and_newlines.into_bump_slice(),
                    spaces.state,
                ))
            }
        }
    }
}

#[inline(always)]
fn space0_no_after_indent_check<'a, E>() -> impl Parser<'a, &'a [CommentOrNewline<'a>], E>
where
    E: 'a + SpaceProblem,
{
    move |arena, state: State<'a>, _min_indent: u32| match fast_eat_spaces(&state) {
        FastSpaceState::HasTab(position) => Err((
            MadeProgress,
            E::space_problem(BadInputError::HasTab, position),
        )),
        FastSpaceState::Good {
            newlines,
            consumed,
            column: _,
        } => {
            if consumed == 0 {
                Ok((NoProgress, &[] as &[_], state))
            } else {
                let comments_and_newlines = Vec::with_capacity_in(newlines, arena);
                let spaces = eat_spaces(state, comments_and_newlines);

                Ok((
                    MadeProgress,
                    spaces.comments_and_newlines.into_bump_slice(),
                    spaces.state,
                ))
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
    let mut line_start = state.line_start.offset as usize;
    let base_offset = state.pos().offset as usize;

    let mut index = base_offset;
    let bytes = state.original_bytes();
    let length = bytes.len();

    'outer: while index < length {
        match bytes[index] {
            b' ' => {
                index += 1;
            }
            b'\n' => {
                newlines += 1;
                index += 1;
                line_start = index;
            }
            b'\r' => {
                index += 1;
                line_start = index;
            }
            b'\t' => {
                return HasTab(Position::new(index as u32));
            }
            b'#' => {
                index += 1;

                // try to use SIMD instructions explicitly
                // run with RUSTFLAGS="-C target-cpu=native" to enable
                #[cfg(all(
                    target_arch = "x86_64",
                    target_feature = "sse2",
                    target_feature = "sse4.2"
                ))]
                {
                    use std::arch::x86_64::*;

                    // a bytestring with the three characters we're looking for (the rest is ignored)
                    let needle = b"\r\n\t=============";
                    let needle = unsafe { _mm_loadu_si128(needle.as_ptr() as *const _) };

                    while index < length {
                        let remaining = length - index;
                        let length = if remaining < 16 { remaining as i32 } else { 16 };

                        // the source bytes we'll be looking at
                        let haystack =
                            unsafe { _mm_loadu_si128(bytes.as_ptr().add(index) as *const _) };

                        // use first 3 characters of needle, first `length` characters of haystack
                        // finds the first index where one of the `needle` characters occurs
                        // or 16 when none of the needle characters occur
                        let first_special_char = unsafe {
                            _mm_cmpestri(needle, 3, haystack, length, _SIDD_CMP_EQUAL_ANY)
                        };

                        // we've made `first_special_char` characters of progress
                        index += usize::min(first_special_char as usize, remaining);

                        // if we found a special char, let the outer loop handle it
                        if first_special_char != 16 {
                            continue 'outer;
                        }
                    }
                }

                #[cfg(not(all(
                    target_arch = "x86_64",
                    target_feature = "sse2",
                    target_feature = "sse4.2"
                )))]
                {
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
            }
            _ => break,
        }
    }

    Good {
        newlines,
        consumed: index - base_offset,
        column: (index - line_start) as u32,
    }
}

struct SpaceState<'a> {
    state: State<'a>,
    comments_and_newlines: Vec<'a, CommentOrNewline<'a>>,
}

fn eat_spaces<'a>(
    mut state: State<'a>,
    mut comments_and_newlines: Vec<'a, CommentOrNewline<'a>>,
) -> SpaceState<'a> {
    for c in state.bytes() {
        match c {
            b' ' => {
                state = state.advance(1);
            }
            b'\n' => {
                state = state.advance_newline();
                comments_and_newlines.push(CommentOrNewline::Newline);
            }
            b'\r' => {
                state = state.advance_newline();
            }
            b'\t' => unreachable!(),

            b'#' => {
                state = state.advance(1);
                return eat_line_comment(state, comments_and_newlines);
            }
            _ => {
                if !comments_and_newlines.is_empty() {
                    state = state.mark_current_indent();
                }
                break;
            }
        }
    }

    SpaceState {
        state,
        comments_and_newlines,
    }
}

fn eat_line_comment<'a>(
    mut state: State<'a>,
    mut comments_and_newlines: Vec<'a, CommentOrNewline<'a>>,
) -> SpaceState<'a> {
    let mut index = state.pos().offset as usize;
    let bytes = state.original_bytes();
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

                    for c in state.bytes() {
                        match c {
                            b' ' => {
                                state = state.advance(1);
                            }
                            b'\n' => {
                                state = state.advance_newline();
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
                            _ => {
                                state = state.mark_current_indent();
                                break;
                            }
                        }

                        index += 1;
                    }

                    return SpaceState {
                        state,
                        comments_and_newlines,
                    };
                }
                None => {
                    // consume the second #
                    state = state.advance(1);

                    return SpaceState {
                        state,
                        comments_and_newlines,
                    };
                }

                Some(_) => false,
            }
        } else {
            false
        };

        let loop_start = index;

        #[cfg(all(
            target_arch = "x86_64",
            target_feature = "sse2",
            target_feature = "sse4.2"
        ))]
        {
            use std::arch::x86_64::*;

            // a bytestring with the three characters we're looking for (the rest is ignored)
            let needle = b"\r\n\t=============";
            let needle = unsafe { _mm_loadu_si128(needle.as_ptr() as *const _) };

            while index < length {
                let remaining = length - index;
                let chunk = if remaining < 16 { remaining as i32 } else { 16 };

                // the source bytes we'll be looking at
                let haystack = unsafe { _mm_loadu_si128(bytes.as_ptr().add(index) as *const _) };

                // use first 3 characters of needle, first  chunk` characters of haystack
                // finds the first index where one of the `needle` characters occurs
                // or 16 when none of the needle characters occur
                let first_special_char =
                    unsafe { _mm_cmpestri(needle, 3, haystack, chunk, _SIDD_CMP_EQUAL_ANY) };

                // we've made `first_special_char` characters of progress
                let progress = usize::min(first_special_char as usize, remaining);
                index += progress;
                state = state.advance(progress);

                if first_special_char != 16 {
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

                            index += 1;
                            while index < length {
                                match bytes[index] {
                                    b' ' => {
                                        state = state.advance(1);
                                    }
                                    b'\n' => {
                                        state = state.advance_newline();
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
                                    _ => {
                                        state = state.mark_current_indent();
                                        break;
                                    }
                                }

                                index += 1;
                            }

                            return SpaceState {
                                state,
                                comments_and_newlines,
                            };
                        }
                        b'\r' => {
                            state = state.advance_newline();
                            index += 1;
                        }
                        odd_character => {
                            unreachable!(
                                "unexpected_character {} {}",
                                odd_character, odd_character as char
                            )
                        }
                    }
                }
            }
        }

        #[cfg(not(all(
            target_arch = "x86_64",
            target_feature = "sse2",
            target_feature = "sse4.2"
        )))]
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

                    index += 1;
                    while index < length {
                        match bytes[index] {
                            b' ' => {
                                state = state.advance(1);
                            }
                            b'\n' => {
                                state = state.advance_newline();
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
                            _ => {
                                state = state.mark_current_indent();
                                break;
                            }
                        }

                        index += 1;
                    }

                    return SpaceState {
                        state,
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
            comments_and_newlines,
        };
    }
}
