use bumpalo::collections::string::String;
use bumpalo::collections::vec::Vec;
use bumpalo::Bump;
use parse::ast::{Expr, Space};
use parse::parser::{and, loc, map_with_arena, unexpected, unexpected_eof, Parser, State};
use region::Located;

/// What type of comment (if any) are we currently parsing?
#[derive(Debug, PartialEq, Eq)]
enum CommentParsing {
    Line,
    Block,
    No,
}

/// Parse the given expression with 0 or more spaces before it.
/// Returns a Located<Expr> where the location is around the Expr, ignoring the spaces.
/// The Expr will be wrapped in a SpaceBefore if there were any interesting spaces found.
pub fn space0_before<'a, P>(parser: P, min_indent: u16) -> impl Parser<'a, Located<Expr<'a>>>
where
    P: Parser<'a, Expr<'a>>,
{
    map_with_arena(
        and(space0(min_indent), loc(parser)),
        |arena, (space_list, loc_expr)| {
            if space_list.is_empty() {
                loc_expr
            } else {
                Located {
                    region: loc_expr.region.clone(),
                    value: Expr::SpaceBefore(space_list, arena.alloc(loc_expr)),
                }
            }
        },
    )
}

/// Parse the given expression with 1 or more spaces before it.
/// Returns a Located<Expr> where the location is around the Expr, ignoring the spaces.
/// The Expr will be wrapped in a SpaceBefore if there were any interesting spaces found.
pub fn space1_before<'a, P>(parser: P, min_indent: u16) -> impl Parser<'a, Located<Expr<'a>>>
where
    P: Parser<'a, Expr<'a>>,
{
    map_with_arena(
        and(space1(min_indent), loc(parser)),
        |arena, (space_list, loc_expr)| {
            if space_list.is_empty() {
                loc_expr
            } else {
                Located {
                    region: loc_expr.region.clone(),
                    value: Expr::SpaceBefore(space_list, arena.alloc(loc_expr)),
                }
            }
        },
    )
}

/// Parse the given expression with 0 or more spaces after it.
/// Returns a Located<Expr> where the location is around the Expr, ignoring the spaces.
/// The Expr will be wrapped in a SpaceAfter if there were any interesting spaces found.
pub fn space0_after<'a, P>(parser: P, min_indent: u16) -> impl Parser<'a, Located<Expr<'a>>>
where
    P: Parser<'a, Expr<'a>>,
{
    map_with_arena(
        and(space0(min_indent), loc(parser)),
        |arena, (space_list, loc_expr)| {
            if space_list.is_empty() {
                loc_expr
            } else {
                Located {
                    region: loc_expr.region.clone(),
                    value: Expr::SpaceAfter(arena.alloc(loc_expr), space_list),
                }
            }
        },
    )
}

/// Parse the given expression with 1 or more spaces after it.
/// Returns a Located<Expr> where the location is around the Expr, ignoring the spaces.
/// The Expr will be wrapped in a SpaceAfter if there were any interesting spaces found.
pub fn space1_after<'a, P>(parser: P, min_indent: u16) -> impl Parser<'a, Located<Expr<'a>>>
where
    P: Parser<'a, Expr<'a>>,
{
    map_with_arena(
        and(space1(min_indent), loc(parser)),
        |arena, (space_list, loc_expr)| {
            if space_list.is_empty() {
                loc_expr
            } else {
                Located {
                    region: loc_expr.region.clone(),
                    value: Expr::SpaceAfter(arena.alloc(loc_expr), space_list),
                }
            }
        },
    )
}

pub fn space0<'a>(min_indent: u16) -> impl Parser<'a, &'a [Space<'a>]> {
    spaces(false, min_indent)
}

pub fn space1<'a>(min_indent: u16) -> impl Parser<'a, &'a [Space<'a>]> {
    // TODO try doing a short-circuit for the typical case: see if there is
    // exactly one space followed by char that isn't [' ', '\n', or '#'], and
    // if so, return empty slice. The case where there's exactly 1 space should
    // be by far the most common.
    spaces(true, min_indent)
}

#[inline(always)]
fn spaces<'a>(require_at_least_one: bool, _min_indent: u16) -> impl Parser<'a, &'a [Space<'a>]> {
    move |arena: &'a Bump, state: State<'a>| {
        let mut chars = state.input.chars().peekable();
        let mut space_list = Vec::new_in(arena);
        let mut chars_parsed = 0;
        let mut comment_lines: Vec<'a, &'a str> = Vec::new_in(arena);
        let mut comment_line_buf = String::new_in(arena);
        let mut comment_parsing = CommentParsing::No;
        let mut state = state;

        while let Some(ch) = chars.next() {
            chars_parsed += 1;

            match comment_parsing {
                CommentParsing::No => match ch {
                    ' ' => {
                        state = state.advance_spaces(1)?;
                    }
                    '\n' => {
                        state = state.newline()?;

                        // Newlines only get added to the list when they're outside comments.
                        space_list.push(Space::Newline);
                    }
                    '#' => {
                        // We're now parsing a line comment!
                        comment_parsing = CommentParsing::Line;
                    }
                    nonblank => {
                        return if require_at_least_one && chars_parsed <= 1 {
                            // We've parsed 1 char and it was not a space,
                            // but we require parsing at least one space!
                            Err(unexpected(
                                nonblank,
                                chars_parsed,
                                state.clone(),
                                state.attempting,
                            ))
                        } else {
                            Ok((space_list.into_bump_slice(), state))
                        };
                    }
                },
                CommentParsing::Line => {
                    match ch {
                        ' ' => {
                            state = state.advance_spaces(1)?;

                            comment_line_buf.push(ch);
                        }
                        '\n' => {
                            state = state.newline()?;

                            // This was a newline, so end this line comment.
                            space_list.push(Space::LineComment(comment_line_buf.into_bump_str()));
                            comment_line_buf = String::new_in(arena);

                            comment_parsing = CommentParsing::No;
                        }
                        '#' if comment_line_buf.is_empty() => {
                            if chars.peek() == Some(&'#') {
                                // Consume the '#' we peeked in the conditional.
                                chars.next();

                                // Advance past the '#' we parsed and the one
                                // we peeked (and then consumed manually).
                                state = state.advance_without_indenting(2)?;

                                // This must be the start of a block comment,
                                // since we are parsing a LineComment with an empty buffer
                                // (meaning the previous char must have been '#'),
                                // then we parsed a '#' right after it, and finally
                                // we peeked and saw a third '#' after that.
                                // "###" begins a block comment!
                                comment_parsing = CommentParsing::Block;
                            } else {
                                state = state.advance_without_indenting(1)?;

                                comment_line_buf.push('#');
                            }
                        }
                        nonblank => {
                            state = state.advance_without_indenting(1)?;

                            comment_line_buf.push(nonblank);
                        }
                    }
                }
                CommentParsing::Block => {
                    match ch {
                        ' ' => {
                            state = state.advance_spaces(1)?;

                            comment_line_buf.push(ch);
                        }
                        '\n' => {
                            state = state.newline()?;

                            // End the current line and start a fresh one.
                            comment_lines.push(comment_line_buf.into_bump_str());

                            comment_line_buf = String::new_in(arena);
                        }
                        '#' => {
                            // Three '#' in a row means the comment is finished.
                            //
                            // We want to peek ahead two characters to see if there
                            // are another two '#' there. If so, this comment is done.
                            // Otherwise, we want to proceed as normal.
                            //
                            // Since we can only peek one character at a time,
                            // we need to be careful with how we use peek() and next()
                            // here to avoid accidentally recording extraneous '#' characters
                            // while also making sure not to drop them if we don't
                            // encounter the full "###" after all.
                            match chars.peek() {
                                Some('#') => {
                                    // Consume the second '#'.
                                    chars.next();

                                    // We've now seen two '#' in a row. Is a third next?
                                    match chars.peek() {
                                        Some('#') => {
                                            // Consume the third '#'.
                                            chars.next();

                                            // We're done! This is the end of the block comment.
                                            state = state.advance_without_indenting(3)?;

                                            // End the current line and start a fresh one.
                                            comment_lines.push(comment_line_buf.into_bump_str());

                                            comment_line_buf = String::new_in(arena);

                                            // Add the block comment to the list.
                                            space_list.push(Space::BlockComment(
                                                comment_lines.into_bump_slice(),
                                            ));

                                            // Start a fresh comment line list.
                                            comment_lines = Vec::new_in(arena);

                                            comment_parsing = CommentParsing::No;
                                        }
                                        _ => {
                                            // It was only two '#' in a row, so record them
                                            // and move on as normal.
                                            state = state.advance_without_indenting(2)?;

                                            comment_line_buf.push_str("##");
                                        }
                                    }
                                }
                                _ => {
                                    // This was a standalone '#' not followed by a second '#',
                                    // so record it and move on as normal.
                                    state = state.advance_without_indenting(1)?;

                                    comment_line_buf.push('#');
                                }
                            }
                        }
                        nonblank => {
                            state = state.advance_without_indenting(1)?;

                            comment_line_buf.push(nonblank);
                        }
                    }
                }
            }
        }

        if require_at_least_one && chars_parsed == 0 {
            Err(unexpected_eof(chars_parsed, state.attempting, state))
        } else {
            Ok((space_list.into_bump_slice(), state))
        }
    }
}
