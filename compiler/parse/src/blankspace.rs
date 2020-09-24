use crate::ast::CommentOrNewline::{self, *};
use crate::ast::{Attempting, Spaceable};
use crate::parser::{
    self, and, ascii_char, ascii_string, optional, parse_utf8, peek_utf8_char, then, unexpected,
    unexpected_eof, FailReason, Parser, State,
};
use bumpalo::collections::string::String;
use bumpalo::collections::vec::Vec;
use bumpalo::Bump;
use roc_region::all::Located;

/// Parses the given expression with 0 or more (spaces/comments/newlines) before and/or after it.
/// Returns a Located<Expr> where the location is around the Expr, ignoring the spaces.
/// If any newlines or comments were found, the Expr will be wrapped in a SpaceBefore and/or
/// SpaceAfter as appropriate.
pub fn space0_around<'a, P, S>(parser: P, min_indent: u16) -> impl Parser<'a, Located<S>>
where
    S: Spaceable<'a>,
    S: 'a,
    S: Sized,
    P: Parser<'a, Located<S>>,
    P: 'a,
{
    parser::map_with_arena(
        and(space0(min_indent), and(parser, space0(min_indent))),
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

/// Parses the given expression with 1 or more (spaces/comments/newlines) before it,
/// and also 1 or more spaces after it.
/// Returns a Located<Expr> where the location is around the Expr, ignoring the spaces.
/// If any newlines or comments were found, the Expr will be wrapped in a SpaceBefore and/or
/// SpaceAfter as appropriate.
pub fn space1_around<'a, P, S>(parser: P, min_indent: u16) -> impl Parser<'a, Located<S>>
where
    S: Spaceable<'a>,
    S: 'a,
    P: Parser<'a, Located<S>>,
    P: 'a,
{
    parser::map_with_arena(
        and(space1(min_indent), and(parser, space1(min_indent))),
        |arena, (spaces_before, (loc_expr, spaces_after))| {
            if spaces_before.is_empty() {
                if spaces_after.is_empty() {
                    loc_expr
                } else {
                    arena
                        .alloc(loc_expr.value)
                        .with_spaces_after(spaces_after, loc_expr.region)
                }
            } else if spaces_after.is_empty() {
                arena
                    .alloc(loc_expr.value)
                    .with_spaces_before(spaces_before, loc_expr.region)
            } else {
                let loc_wrapped_expr = arena
                    .alloc(loc_expr.value)
                    .with_spaces_after(spaces_after, loc_expr.region);

                arena
                    .alloc(loc_wrapped_expr.value)
                    .with_spaces_before(spaces_before, loc_wrapped_expr.region)
            }
        },
    )
}

/// Parses the given expression with 0 or more (spaces/comments/newlines) after it.
/// Returns a Located<Expr> where the location is around the Expr, ignoring the spaces.
/// The Expr will be wrapped in a SpaceBefore if there were any newlines or comments found.
pub fn space0_before<'a, P, S>(parser: P, min_indent: u16) -> impl Parser<'a, Located<S>>
where
    S: Spaceable<'a>,
    S: 'a,
    P: Parser<'a, Located<S>>,
    P: 'a,
{
    parser::map_with_arena(
        and!(space0(min_indent), parser),
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

/// Parses the given expression with 1 or more (spaces/comments/newlines) after it.
/// Returns a Located<Expr> where the location is around the Expr, ignoring the spaces.
/// The Expr will be wrapped in a SpaceBefore if there were any newlines or comments found.
pub fn space1_before<'a, P, S>(parser: P, min_indent: u16) -> impl Parser<'a, Located<S>>
where
    S: Spaceable<'a>,
    S: 'a,
    P: Parser<'a, Located<S>>,
    P: 'a,
{
    parser::map_with_arena(
        and!(space1(min_indent), parser),
        |arena, (space_list, loc_expr)| {
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

/// Parses the given expression with 0 or more (spaces/comments/newlines) after it.
/// Returns a Located<Expr> where the location is around the Expr, ignoring the spaces.
/// The Expr will be wrapped in a SpaceAfter if there were any newlines or comments found.
pub fn space0_after<'a, P, S>(parser: P, min_indent: u16) -> impl Parser<'a, Located<S>>
where
    S: Spaceable<'a>,
    S: 'a,
    P: Parser<'a, Located<S>>,
    P: 'a,
{
    parser::map_with_arena(
        and!(parser, space0(min_indent)),
        |arena, (loc_expr, space_list)| {
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

/// Parses the given expression with 1 or more (spaces/comments/newlines) after it.
/// Returns a Located<Expr> where the location is around the Expr, ignoring the spaces.
/// The Expr will be wrapped in a SpaceAfter if there were any newlines or comments found.
pub fn space1_after<'a, P, S>(parser: P, min_indent: u16) -> impl Parser<'a, Located<S>>
where
    S: Spaceable<'a>,
    S: 'a,
    P: Parser<'a, Located<S>>,
    P: 'a,
{
    parser::map_with_arena(
        and!(parser, space1(min_indent)),
        |arena, (loc_expr, space_list)| {
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

/// Zero or more (spaces/comments/newlines).
pub fn space0<'a>(min_indent: u16) -> impl Parser<'a, &'a [CommentOrNewline<'a>]> {
    spaces(false, min_indent)
}

/// One or more (spaces/comments/newlines).
pub fn space1<'a>(min_indent: u16) -> impl Parser<'a, &'a [CommentOrNewline<'a>]> {
    // TODO try benchmarking a short-circuit for the typical case: see if there is
    // exactly one space followed by char that isn't [' ', '\n', or '#'], and
    // if so, return empty slice. The case where there's exactly 1 space should
    // be by far the most common.
    spaces(true, min_indent)
}

#[derive(Debug, Clone, Copy)]
enum LineState {
    Normal,
    Comment,
    DocComment,
}

pub fn line_comment<'a>() -> impl Parser<'a, &'a str> {
    then(
        and!(ascii_char('#'), optional(ascii_string("# "))),
        |_arena: &'a Bump, state: State<'a>, (_, opt_doc)| {
            if opt_doc != None {
                return Err(unexpected(3, state, Attempting::LineComment));
            }
            let mut length = 0;

            for &byte in state.bytes.iter() {
                if byte != b'\n' {
                    length += 1;
                } else {
                    break;
                }
            }

            let comment = &state.bytes[..length];
            let state = state.advance_without_indenting(length + 1)?;
            match parse_utf8(comment) {
                Ok(comment_str) => Ok((comment_str, state)),
                Err(reason) => state.fail(reason),
            }
        },
    )
}

#[inline(always)]
fn spaces<'a>(
    require_at_least_one: bool,
    min_indent: u16,
) -> impl Parser<'a, &'a [CommentOrNewline<'a>]> {
    move |arena: &'a Bump, state: State<'a>| {
        let original_state = state.clone();
        let mut space_list = Vec::new_in(arena);
        let mut bytes_parsed = 0;
        let mut comment_line_buf = String::new_in(arena);
        let mut line_state = LineState::Normal;
        let mut state = state;
        let mut any_newlines = false;

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
                                    state = state.advance_spaces(1)?;
                                }
                                '\r' => {
                                    // Ignore carriage returns.
                                    state = state.advance_spaces(1)?;
                                }
                                '\n' => {
                                    // No need to check indentation because we're about to reset it anyway.
                                    state = state.newline()?;

                                    // Newlines only get added to the list when they're outside comments.
                                    space_list.push(Newline);

                                    any_newlines = true;
                                }
                                '#' => {
                                    // Check indentation to make sure we were indented enough
                                    // before this comment began.
                                    state = state
                                        .check_indent(min_indent)
                                        .map_err(|(fail, _)| (fail, original_state.clone()))?
                                        .advance_without_indenting(1)?;

                                    // We're now parsing a line comment!
                                    line_state = LineState::Comment;
                                }
                                _ => {
                                    return if require_at_least_one && bytes_parsed <= 1 {
                                        // We've parsed 1 char and it was not a space,
                                        // but we require parsing at least one space!
                                        Err(unexpected(0, state.clone(), state.attempting))
                                    } else {
                                        // First make sure we were indented enough!
                                        //
                                        // (We only do this if we've encountered any newlines.
                                        // Otherwise, we assume indentation is already correct.
                                        // It's actively important for correctness that we skip
                                        // this check if there are no newlines, because otherwise
                                        // we would have false positives for single-line defs.)
                                        if any_newlines {
                                            state = state
                                                .check_indent(min_indent)
                                                .map_err(|(fail, _)| (fail, original_state))?;
                                        }

                                        Ok((space_list.into_bump_slice(), state))
                                    };
                                }
                            }
                        }
                        LineState::Comment => {
                            match ch {
                                ' ' => {
                                    // If we're in a line comment, this won't affect indentation anyway.
                                    state = state.advance_without_indenting(1)?;

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
                                    state = state.newline()?;

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
                                nonblank => {
                                    // Chars can have btye lengths of more than 1!
                                    state = state.advance_without_indenting(nonblank.len_utf8())?;

                                    comment_line_buf.push(nonblank);
                                }
                            }
                        }
                        LineState::DocComment => {
                            match ch {
                                ' ' => {
                                    // If we're in a doc comment, this won't affect indentation anyway.
                                    state = state.advance_without_indenting(1)?;

                                    comment_line_buf.push(ch);
                                }
                                '\n' => {
                                    state = state.newline()?;

                                    // This was a newline, so end this doc comment.
                                    space_list.push(DocComment(comment_line_buf.into_bump_str()));
                                    comment_line_buf = String::new_in(arena);

                                    line_state = LineState::Normal;
                                }
                                nonblank => {
                                    state = state.advance_without_indenting(utf8_len)?;

                                    comment_line_buf.push(nonblank);
                                }
                            }
                        }
                    }
                }
                Err(FailReason::BadUtf8) => {
                    // If we hit an invalid UTF-8 character, bail out immediately.
                    return state.fail(FailReason::BadUtf8);
                }
                Err(_) => {
                    if require_at_least_one && bytes_parsed == 0 {
                        return Err(unexpected_eof(0, state.attempting, state));
                    } else {
                        let space_slice = space_list.into_bump_slice();

                        // First make sure we were indented enough!
                        //
                        // (We only do this if we've encountered any newlines.
                        // Otherwise, we assume indentation is already correct.
                        // It's actively important for correctness that we skip
                        // this check if there are no newlines, because otherwise
                        // we would have false positives for single-line defs.)
                        if any_newlines {
                            return Ok((
                                space_slice,
                                state
                                    .check_indent(min_indent)
                                    .map_err(|(fail, _)| (fail, original_state))?,
                            ));
                        }

                        return Ok((space_slice, state));
                    }
                }
            };
        }

        // If we didn't parse anything, return unexpected EOF
        if require_at_least_one && original_state.bytes.len() == state.bytes.len() {
            Err(unexpected_eof(0, state.attempting, state))
        } else {
            // First make sure we were indented enough!
            //
            // (We only do this if we've encountered any newlines.
            // Otherwise, we assume indentation is already correct.
            // It's actively important for correctness that we skip
            // this check if there are no newlines, because otherwise
            // we would have false positives for single-line defs.)
            if any_newlines {
                state = state
                    .check_indent(min_indent)
                    .map_err(|(fail, _)| (fail, original_state))?;
            }

            Ok((space_list.into_bump_slice(), state))
        }
    }
}
