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

pub fn spaces_around<'a, P, S, E>(parser: P) -> impl Parser<'a, Loc<S>, E>
where
    S: 'a + Spaceable<'a>,
    P: 'a + Parser<'a, Loc<S>, E>,
    E: 'a + SpaceProblem,
{
    parser::map_with_arena(and(spaces(), and(parser, spaces())), spaces_around_help)
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

pub fn spaces_before_optional_after<'a, P, S, E>(parser: P) -> impl Parser<'a, Loc<S>, E>
where
    S: 'a + Spaceable<'a>,
    P: 'a + Parser<'a, Loc<S>, E>,
    E: 'a + SpaceProblem,
{
    parser::map_with_arena(
        and(
            spaces(),
            and(
                parser,
                one_of![backtrackable(spaces()), succeed!(&[] as &[_]),],
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

pub fn spaces_before<'a, P, S, E>(parser: P) -> impl Parser<'a, Loc<S>, E>
where
    S: 'a + Spaceable<'a>,
    P: 'a + Parser<'a, Loc<S>, E>,
    E: 'a + SpaceProblem,
{
    parser::map_with_arena(
        and!(spaces(), parser),
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

pub fn simple_eat_whitespace(bytes: &[u8]) -> usize {
    let mut i = 0;
    while i < bytes.len() {
        match bytes[i] {
            b' ' => i += 1,
            _ => break,
        }
    }
    i
}

pub fn fast_eat_whitespace(bytes: &[u8]) -> usize {
    // Load 8 bytes at a time, keeping in mind that the initial offset may not be aligned
    let mut i = 0;
    while i + 8 <= bytes.len() {
        let chunk = unsafe {
            // Safe because we know the pointer is in bounds
            (bytes.as_ptr().add(i) as *const u64)
                .read_unaligned()
                .to_le()
        };

        // Space character is 0x20, which has a single bit set
        // We can check for any space character by checking if any other bit is set
        let spaces = 0x2020_2020_2020_2020;

        // First, generate a mask where each byte is 0xff if the byte is a space,
        // and some other bit sequence otherwise
        let mask = !(chunk ^ spaces);

        // Now mask off the high bit, so there's some place to carry into without
        // overflowing into the next byte.
        let mask = mask & !0x8080_8080_8080_8080;

        // Now add 0x0101_0101_0101_0101 to each byte, which will carry into the high bit
        // if and only if the byte is a space.
        let mask = mask + 0x0101_0101_0101_0101;

        // Now mask off areas where the original bytes had the high bit set, so that
        // 0x80|0x20 = 0xa0 will not be considered a space.
        let mask = mask & !(chunk & 0x8080_8080_8080_8080);

        // Make sure all the _other_ bits aside from the high bit are set,
        // and count the number of trailing one bits, dividing by 8 to get the number of
        // bytes that are spaces.
        let count = ((mask | !0x8080_8080_8080_8080).trailing_ones() as usize) / 8;

        if count == 8 {
            i += 8;
        } else {
            return i + count;
        }
    }

    // Check the remaining bytes
    simple_eat_whitespace(&bytes[i..]) + i
}

pub fn simple_eat_until_control_character(bytes: &[u8]) -> usize {
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

pub fn fast_eat_until_control_character(bytes: &[u8]) -> usize {
    // Load 8 bytes at a time, keeping in mind that the initial offset may not be aligned
    let mut i = 0;
    while i + 8 <= bytes.len() {
        let chunk = unsafe {
            // Safe because we know the pointer is in bounds
            (bytes.as_ptr().add(i) as *const u64)
                .read_unaligned()
                .to_le()
        };

        // Control characters are 0x00-0x1F, and don't have any high bits set.
        // They only have bits set that fall under the 0x1F mask.
        let control = 0x1F1F_1F1F_1F1F_1F1F;

        // First we set up a value where, if a given byte is a control character,
        // it'll have a all the non-control bits set to 1. All control bits are set to zero.
        let mask = !(chunk & !control) & !control;

        // Now, down shift by one bit. This will leave room for the following add to
        // carry, without impacting the next byte.
        let mask = mask >> 1;

        // Add one (shifted by the right amount), causing all the one bits in the control
        // characters to cascade, and put a one in the high bit.
        let mask = mask.wrapping_add(0x1010_1010_1010_1010);

        // Now, we can count the number of trailing zero bits, dividing by 8 to get the
        // number of bytes before the first control character.
        let count = (mask & 0x8080_8080_8080_8080).trailing_zeros() as usize / 8;

        if count == 8 {
            i += 8;
        } else {
            return i + count;
        }
    }

    // Check the remaining bytes
    simple_eat_until_control_character(&bytes[i..]) + i
}

#[cfg(test)]
mod tests {
    use super::*;
    use proptest::prelude::*;

    #[test]
    fn test_eat_whitespace_simple() {
        let bytes = &[0, 0, 0, 0, 0, 0, 0, 0];
        assert_eq!(simple_eat_whitespace(bytes), fast_eat_whitespace(bytes));
    }

    proptest! {
        #[test]
        fn test_eat_whitespace(bytes in proptest::collection::vec(any::<u8>(), 0..100)) {
            prop_assert_eq!(simple_eat_whitespace(&bytes), fast_eat_whitespace(&bytes));
        }
    }

    #[test]
    fn test_eat_until_control_character_simple() {
        let bytes = &[32, 0, 0, 0, 0, 0, 0, 0];
        assert_eq!(
            simple_eat_until_control_character(bytes),
            fast_eat_until_control_character(bytes)
        );
    }

    proptest! {
        #[test]
        fn test_eat_until_control_character(bytes in proptest::collection::vec(any::<u8>(), 0..100)) {
            prop_assert_eq!(
                simple_eat_until_control_character(&bytes),
                fast_eat_until_control_character(&bytes));
        }
    }
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

pub fn spaces<'a, E>() -> impl Parser<'a, &'a [CommentOrNewline<'a>], E>
where
    E: 'a + SpaceProblem,
{
    move |arena, mut state: State<'a>, _min_indent: u32| {
        let mut newlines = Vec::new_in(arena);
        let mut progress = NoProgress;
        loop {
            let whitespace = fast_eat_whitespace(state.bytes());
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

                    let len = fast_eat_until_control_character(state.bytes());

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
