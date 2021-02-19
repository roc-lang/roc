use crate::ast::{
    AssignedField, Attempting, CommentOrNewline, Def, Expr, Pattern, Spaceable, TypeAnnotation,
};
use crate::blankspace::{
    line_comment, space0, space0_after, space0_around, space0_before, space1, space1_around,
    space1_before, spaces_exactly,
};
use crate::ident::{global_tag_or_ident, ident, lowercase_ident, Ident};
use crate::keyword;
use crate::number_literal::number_literal;
use crate::parser::Progress::{self, *};
use crate::parser::{
    self, allocated, and_then_with_indent_level, ascii_char, ascii_string, attempt, backtrackable,
    fail, map, newline_char, not, not_followed_by, optional, peek_utf8_char_e, sep_by1, specialize,
    then, unexpected, unexpected_eof, word1, EPattern, Either, ParseResult, Parser, State,
    SyntaxError,
};
use crate::type_annotation;
use bumpalo::collections::string::String;
use bumpalo::collections::Vec;
use bumpalo::Bump;
use roc_module::operator::{BinOp, CalledVia, UnaryOp};
use roc_region::all::{Located, Region};

/// Different patterns are supported in different circumstances.
/// For example, when branches can pattern match on number literals, but
/// assignments and function args can't. Underscore is supported in function
/// arg patterns and in when branch patterns, but not in assignments.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PatternType {
    TopLevelDef,
    DefExpr,
    FunctionArg,
    WhenBranch,
}

pub fn underscore_pattern<'a>() -> impl Parser<'a, Pattern<'a>, SyntaxError<'a>> {
    specialize(|e, _, _| SyntaxError::Pattern(e), underscore_pattern_help())
}

fn underscore_pattern_help<'a>() -> impl Parser<'a, Pattern<'a>, EPattern<'a>> {
    move |arena: &'a Bump, state: State<'a>| {
        let (_, _, next_state) = word1(b'_', EPattern::Underscore).parse(arena, state)?;

        let (_, output, final_state) =
            optional(|a, s| lowercase_ident_pattern(a, s)).parse(arena, next_state)?;

        match output {
            Some(name) => Ok((MadeProgress, Pattern::Underscore(name), final_state)),
            None => Ok((MadeProgress, Pattern::Underscore(&""), final_state)),
        }
    }
}

fn lowercase_ident_pattern<'a>(
    arena: &'a Bump,
    mut state: State<'a>,
) -> ParseResult<'a, &'a str, EPattern<'a>> {
    let mut buf = String::new_in(arena);

    let start_bytes_len = state.bytes.len();

    match peek_utf8_char_e(&state, EPattern::Start, EPattern::Space) {
        Ok((first_letter, bytes_parsed)) => {
            // Type variables must start with a lowercase letter.
            if first_letter.is_alphabetic() && first_letter.is_lowercase() {
                buf.push(first_letter);
            } else {
                return Err((NoProgress, EPattern::Start(state.line, state.column), state));
            }

            state = state.advance_without_indenting_e(arena, bytes_parsed, EPattern::Space)?;
        }
        Err(reason) => return Err((NoProgress, reason, state)),
    }

    while !state.bytes.is_empty() {
        match peek_utf8_char_e(&state, EPattern::End, EPattern::Space) {
            Ok((ch, bytes_parsed)) => {
                // After the first character, only these are allowed:
                //
                // * Unicode alphabetic chars - you might name a variable `鹏` if that's clear to your readers
                // * ASCII digits - e.g. `1` but not `¾`, both of which pass .is_numeric()
                if ch.is_alphabetic() || ch.is_ascii_digit() {
                    buf.push(ch);
                } else {
                    // This must be the end of the type. We're done!
                    break;
                }

                state = state.advance_without_indenting_e(arena, bytes_parsed, EPattern::Space)?;
            }
            Err(reason) => {
                return state.fail(arena, MadeProgress, reason);
            }
        }
    }

    let progress = Progress::from_lengths(start_bytes_len, state.bytes.len());
    Ok((progress, buf.into_bump_str(), state))
}
