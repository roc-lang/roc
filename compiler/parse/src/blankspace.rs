use crate::ast::CommentOrNewline::{self, *};
use crate::ast::Spaceable;
use crate::parser::{
    self, and, peek_utf8_char, BadInputError, Col, Parser,
    Progress::{self, *},
    Row, State, SyntaxError,
};
use bumpalo::collections::string::String;
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
        move |arena: &'a Bump,
              tuples: (
            &'a [CommentOrNewline<'a>],
            (Located<S>, &'a [CommentOrNewline<'a>]),
        )| {
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
        },
    )
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

pub fn space0_e<'a, E>(
    min_indent: u16,
    space_problem: fn(BadInputError, Row, Col) -> E,
    indent_problem: fn(Row, Col) -> E,
) -> impl Parser<'a, &'a [CommentOrNewline<'a>], E>
where
    E: 'a,
{
    spaces_help(false, min_indent, space_problem, indent_problem, |_, _| {
        unreachable!("no spaces are required, so this is unreachable")
    })
}

pub fn space1_e<'a, E>(
    min_indent: u16,
    space_problem: fn(BadInputError, Row, Col) -> E,
    indent_problem: fn(Row, Col) -> E,
    no_parse_problem: fn(Row, Col) -> E,
) -> impl Parser<'a, &'a [CommentOrNewline<'a>], E>
where
    E: 'a,
{
    move |arena, state| match space0_e(min_indent, space_problem, indent_problem)
        .parse(arena, state)
    {
        Ok((NoProgress, _, state)) => Err((
            NoProgress,
            no_parse_problem(state.line, state.column),
            state,
        )),
        Ok((MadeProgress, spaces, state)) => Ok((MadeProgress, spaces, state)),
        Err(bad) => Err(bad),
    }
}

#[derive(Debug, Clone, Copy)]
enum LineState {
    Normal,
    Comment,
    DocComment,
}

pub fn line_comment<'a>() -> impl Parser<'a, &'a str, SyntaxError<'a>> {
    |_, state: State<'a>| match chomp_line_comment(state.bytes) {
        Ok(comment) => {
            let width = 1 + comment.len();
            let state = state.advance_without_indenting(width + 1)?;

            Ok((MadeProgress, comment, state))
        }
        Err(progress) => Err((progress, SyntaxError::ConditionFailed, state)),
    }
}

fn chomp_line_comment<'a>(buffer: &'a [u8]) -> Result<&'a str, Progress> {
    if let Some(b'#') = buffer.get(0) {
        if (&buffer[1..]).starts_with(b"# ") {
            // this is a doc comment, not a line comment
            Err(NoProgress)
        } else {
            use encode_unicode::CharExt;

            let mut chomped = 1;

            while let Ok((ch, width)) = char::from_utf8_slice_start(&buffer[chomped..]) {
                if ch == '\n' {
                    break;
                } else {
                    chomped += width;
                }
            }

            let comment_bytes = &buffer[1..chomped];
            let comment = unsafe { std::str::from_utf8_unchecked(comment_bytes) };

            Ok(comment)
        }
    } else {
        Err(NoProgress)
    }
}

#[inline(always)]
pub fn spaces_exactly_e<'a>(spaces_expected: u16) -> impl Parser<'a, (), parser::EExpr<'a>> {
    use parser::EExpr;

    move |arena: &'a Bump, state: State<'a>| {
        if spaces_expected == 0 {
            return Ok((NoProgress, (), state));
        }

        let mut spaces_seen: u16 = 0;

        for c in state.bytes {
            match c {
                b' ' => {
                    spaces_seen += 1;
                    if spaces_seen == spaces_expected {
                        let state = state.advance_spaces_e(
                            arena,
                            spaces_expected as usize,
                            EExpr::IndentStart,
                        )?;
                        return Ok((MadeProgress, (), state));
                    }
                }
                _ => {
                    return Err((
                        NoProgress,
                        EExpr::IndentStart(state.line, state.column + spaces_seen),
                        state,
                    ))
                }
            }
        }

        Err((
            NoProgress,
            EExpr::IndentStart(state.line, state.column + spaces_seen),
            state,
        ))
    }
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

                    state.is_indenting = true;
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

    return Good {
        row,
        col,
        bytes,
        comments_and_newlines,
    };
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

    return Good {
        row,
        col,
        bytes,
        comments_and_newlines,
    };
}

#[inline(always)]
fn spaces_help<'a, E>(
    require_at_least_one: bool,
    min_indent: u16,
    space_problem: fn(BadInputError, Row, Col) -> E,
    indent_problem: fn(Row, Col) -> E,
    missing_space_problem: fn(Row, Col) -> E,
) -> impl Parser<'a, &'a [CommentOrNewline<'a>], E>
where
    E: 'a,
{
    move |arena, state: State<'a>| {
        if !require_at_least_one {
            match spaces_help_help(min_indent, space_problem, indent_problem).parse(arena, state) {
                Ok((a, b, c)) => Ok((a, b, c)),
                Err((a, b, c)) => Err((a, b, c)),
            }
        } else {
            match spaces_help_help(min_indent, space_problem, indent_problem).parse(arena, state) {
                Ok((MadeProgress, b, c)) => Ok((MadeProgress, b, c)),
                Ok((NoProgress, b, state)) => Err((
                    NoProgress,
                    missing_space_problem(state.line, state.column),
                    state,
                )),
                Err((a, b, c)) => Err((a, b, c)),
            }
            /*
            match spaces_help_help_help(
                require_at_least_one,
                min_indent,
                space_problem,
                indent_problem,
                missing_space_problem,
            )
            .parse(arena, state)
            {
                Ok((a, b, c)) => {
                    //dbg!(&c);
                    Ok((a, b, c))
                }
                Err((a, b, c)) => {
                    //dbg!(&c);
                    Err((a, b, c))
                }
            }
            */
        }
    }
}

#[inline(always)]
fn spaces_help_help_help<'a, E>(
    require_at_least_one: bool,
    min_indent: u16,
    space_problem: fn(BadInputError, Row, Col) -> E,
    indent_problem: fn(Row, Col) -> E,
    missing_space_problem: fn(Row, Col) -> E,
) -> impl Parser<'a, &'a [CommentOrNewline<'a>], E>
where
    E: 'a,
{
    move |arena: &'a Bump, state: State<'a>| {
        let original_state = state.clone();
        let mut space_list = Vec::new_in(arena);
        let mut bytes_parsed = 0;
        let mut comment_line_buf = String::new_in(arena);
        let mut line_state = LineState::Normal;
        let mut state = state;
        let mut any_newlines = false;

        let start_row = original_state.line;
        let start_col = original_state.column;

        let start_bytes_len = state.bytes.len();

        while !state.bytes.is_empty() {
            match peek_utf8_char(&state) {
                Ok((ch, utf8_len)) => {
                    bytes_parsed += utf8_len;

                    match line_state {
                        LineState::Normal => {
                            match ch {
                                ' ' => {
                                    // Don't check indentation here; it might not be enough
                                    // indentation yet, but maybe it will be after more spaces happen!
                                    state = state.advance_spaces_e(arena, 1, |r, c| {
                                        space_problem(BadInputError::LineTooLong, r, c)
                                    })?;
                                }
                                '\r' => {
                                    // Ignore carriage returns.
                                    state = state.advance_spaces_e(arena, 1, |r, c| {
                                        space_problem(BadInputError::LineTooLong, r, c)
                                    })?;
                                }
                                '\n' => {
                                    // don't need to check the indent here since we'll reset it
                                    // anyway

                                    state = state.newline_e(arena, space_problem)?;

                                    // Newlines only get added to the list when they're outside comments.
                                    space_list.push(Newline);

                                    any_newlines = true;
                                }
                                '\t' => {
                                    return Err((
                                        MadeProgress,
                                        space_problem(
                                            BadInputError::HasTab,
                                            state.line,
                                            state.column,
                                        ),
                                        state,
                                    ));
                                }
                                '#' => {
                                    // Check indentation to make sure we were indented enough
                                    // before this comment began.
                                    let progress =
                                        Progress::from_lengths(start_bytes_len, state.bytes.len());
                                    state = state
                                        .check_indent_e(
                                            arena,
                                            min_indent,
                                            indent_problem,
                                            start_row,
                                            start_col,
                                        )
                                        .map_err(|(fail, _)| {
                                            (progress, fail, original_state.clone())
                                        })?
                                        .advance_without_indenting_e(1, space_problem)?;

                                    // We're now parsing a line comment!
                                    line_state = LineState::Comment;
                                }
                                _ => {
                                    return if require_at_least_one && bytes_parsed <= 1 {
                                        // We've parsed 1 char and it was not a space,
                                        // but we require parsing at least one space!
                                        Err((
                                            NoProgress,
                                            missing_space_problem(state.line, state.column),
                                            state,
                                        ))
                                    } else {
                                        // First make sure we were indented enough!
                                        //
                                        // (We only do this if we've encountered any newlines.
                                        // Otherwise, we assume indentation is already correct.
                                        // It's actively important for correctness that we skip
                                        // this check if there are no newlines, because otherwise
                                        // we would have false positives for single-line defs.)
                                        let progress = Progress::from_lengths(
                                            start_bytes_len,
                                            state.bytes.len(),
                                        );
                                        if any_newlines {
                                            state = state
                                                .check_indent_e(
                                                    arena,
                                                    min_indent,
                                                    indent_problem,
                                                    start_row,
                                                    start_col,
                                                )
                                                .map_err(|(fail, _)| {
                                                    (progress, fail, original_state.clone())
                                                })?;
                                        }

                                        Ok((progress, space_list.into_bump_slice(), state))
                                    };
                                }
                            }
                        }
                        LineState::Comment => {
                            match ch {
                                ' ' => {
                                    // If we're in a line comment, this won't affect indentation anyway.
                                    state = state.advance_without_indenting_e(1, space_problem)?;

                                    if comment_line_buf.len() == 1 {
                                        match comment_line_buf.chars().next() {
                                            Some('#') => {
                                                // This is a comment begining with `## ` - that is,
                                                // a doc comment.
                                                //
                                                // (The space is important; otherwise, this is not
                                                // a doc comment, but rather something like a
                                                // big separator block, e.g. ############)
                                                line_state = LineState::DocComment;

                                                // This is now the beginning of the doc comment.
                                                comment_line_buf.clear();
                                            }
                                            _ => {
                                                comment_line_buf.push(ch);
                                            }
                                        }
                                    } else {
                                        comment_line_buf.push(ch);
                                    }
                                }
                                '\n' => {
                                    state = state.newline_e(arena, space_problem)?;

                                    match (comment_line_buf.len(), comment_line_buf.chars().next())
                                    {
                                        (1, Some('#')) => {
                                            // This is a line with `##` - that is,
                                            // a doc comment new line.
                                            space_list.push(DocComment(""));
                                            comment_line_buf = String::new_in(arena);

                                            line_state = LineState::Normal;
                                        }
                                        _ => {
                                            // This was a newline, so end this line comment.
                                            space_list.push(LineComment(
                                                comment_line_buf.into_bump_str(),
                                            ));
                                            comment_line_buf = String::new_in(arena);

                                            line_state = LineState::Normal;
                                        }
                                    }
                                }
                                '\t' => {
                                    return Err((
                                        MadeProgress,
                                        space_problem(
                                            BadInputError::HasTab,
                                            state.line,
                                            state.column,
                                        ),
                                        state,
                                    ));
                                }
                                nonblank => {
                                    // Chars can have btye lengths of more than 1!
                                    state = state.advance_without_indenting_e(
                                        nonblank.len_utf8(),
                                        space_problem,
                                    )?;

                                    comment_line_buf.push(nonblank);
                                }
                            }
                        }
                        LineState::DocComment => {
                            match ch {
                                ' ' => {
                                    // If we're in a doc comment, this won't affect indentation anyway.
                                    state = state.advance_without_indenting_e(1, space_problem)?;

                                    comment_line_buf.push(ch);
                                }
                                '\n' => {
                                    state = state.newline_e(arena, space_problem)?;

                                    // This was a newline, so end this doc comment.
                                    space_list.push(DocComment(comment_line_buf.into_bump_str()));
                                    comment_line_buf = String::new_in(arena);

                                    line_state = LineState::Normal;
                                }
                                '\t' => {
                                    return Err((
                                        MadeProgress,
                                        space_problem(
                                            BadInputError::HasTab,
                                            state.line,
                                            state.column,
                                        ),
                                        state,
                                    ));
                                }
                                nonblank => {
                                    state = state
                                        .advance_without_indenting_e(utf8_len, space_problem)?;

                                    comment_line_buf.push(nonblank);
                                }
                            }
                        }
                    }
                }
                Err(SyntaxError::BadUtf8) => {
                    // If we hit an invalid UTF-8 character, bail out immediately.
                    let progress = Progress::from_lengths(start_bytes_len, state.bytes.len());
                    let row = state.line;
                    let col = state.column;
                    return state.fail(
                        arena,
                        progress,
                        space_problem(BadInputError::BadUtf8, row, col),
                    );
                }
                Err(_) => {
                    if require_at_least_one && bytes_parsed == 0 {
                        return Err((
                            NoProgress,
                            missing_space_problem(state.line, state.column),
                            state,
                        ));
                    } else {
                        let space_slice = space_list.into_bump_slice();

                        // First make sure we were indented enough!
                        //
                        // (We only do this if we've encountered any newlines.
                        // Otherwise, we assume indentation is already correct.
                        // It's actively important for correctness that we skip
                        // this check if there are no newlines, because otherwise
                        // we would have false positives for single-line defs.)
                        let progress = Progress::from_lengths(start_bytes_len, state.bytes.len());
                        if any_newlines {
                            return Ok((
                                progress,
                                space_slice,
                                state
                                    .check_indent_e(
                                        arena,
                                        min_indent,
                                        indent_problem,
                                        start_row,
                                        start_col,
                                    )
                                    .map_err(|(fail, _)| (progress, fail, original_state))?,
                            ));
                        }

                        return Ok((progress, space_slice, state));
                    }
                }
            };
        }

        if require_at_least_one && original_state.bytes.len() == state.bytes.len() {
            Err((
                NoProgress,
                missing_space_problem(state.line, state.column),
                state,
            ))
        } else {
            // First make sure we were indented enough!
            //
            // (We only do this if we've encountered any newlines.
            // Otherwise, we assume indentation is already correct.
            // It's actively important for correctness that we skip
            // this check if there are no newlines, because otherwise
            // we would have false positives for single-line defs.)
            let progress = Progress::from_lengths(start_bytes_len, state.bytes.len());
            if any_newlines {
                state = state
                    .check_indent_e(arena, min_indent, indent_problem, start_row, start_col)
                    .map_err(|(fail, _)| (progress, fail, original_state))?;
            }

            Ok((progress, space_list.into_bump_slice(), state))
        }
    }
}
