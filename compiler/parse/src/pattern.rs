use crate::ast::{
    AssignedField, Attempting, CommentOrNewline, Def, Expr, Pattern, Spaceable, TypeAnnotation,
};
use crate::blankspace::{
    line_comment, space0, space0_after, space0_around, space0_around_e, space0_before,
    space0_before_e, space0_e, space1, space1_around, space1_before, spaces_exactly,
};
use crate::expr::loc_pattern;
use crate::ident::{global_tag_or_ident, ident, lowercase_ident, Ident};
use crate::keyword;
use crate::number_literal::number_literal;
use crate::parser::Progress::{self, *};
use crate::parser::{
    self, allocated, fail, map, newline_char, not, not_followed_by, optional, peek_utf8_char_e,
    sep_by1, specialize, specialize_ref, then, unexpected, unexpected_eof, word1, BadInputError,
    EPattern, Either, PRecord, ParseResult, Parser, State, SyntaxError,
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

pub fn record_pattern<'a>(min_indent: u16) -> impl Parser<'a, Pattern<'a>, SyntaxError<'a>> {
    specialize(
        |e, r, c| SyntaxError::Pattern(EPattern::Record(e, r, c)),
        record_pattern_help(min_indent),
    )
}

#[inline(always)]
fn record_pattern_help<'a>(min_indent: u16) -> impl Parser<'a, Pattern<'a>, PRecord<'a>> {
    move |arena, state| {
        let (_, (fields, final_comments), state) = collection_trailing_sep_e!(
            // word1_check_indent!(b'{', PRecord::Open, min_indent, PRecord::IndentOpen),
            word1(b'{', PRecord::Open),
            loc!(record_pattern_field(min_indent)),
            word1(b',', PRecord::End),
            // word1_check_indent!(b'}', PRecord::End, min_indent, PRecord::IndentEnd),
            word1(b'}', PRecord::End),
            min_indent,
            PRecord::Open,
            PRecord::Space,
            PRecord::IndentEnd
        )
        .parse(arena, state)?;

        // TODO
        let _unused = final_comments;

        let result = Pattern::RecordDestructure(fields.into_bump_slice());

        Ok((MadeProgress, result, state))
    }
}

fn record_pattern_field<'a>(min_indent: u16) -> impl Parser<'a, Pattern<'a>, PRecord<'a>> {
    use crate::parser::Either::*;

    move |arena, state: State<'a>| {
        // You must have a field name, e.g. "email"
        // using the initial row/col is important for error reporting
        let row = state.line;
        let col = state.column;
        let (progress, loc_label, state) = loc!(specialize(
            move |_, _, _| PRecord::Field(row, col),
            lowercase_ident()
        ))
        .parse(arena, state)?;
        debug_assert_eq!(progress, MadeProgress);

        let (_, spaces, state) =
            space0_e(min_indent, PRecord::Space, PRecord::IndentEnd).parse(arena, state)?;

        // Having a value is optional; both `{ email }` and `{ email: blah }` work.
        // (This is true in both literals and types.)
        let (_, opt_loc_val, state) = optional(either!(
            word1(b':', PRecord::Colon),
            word1(b'?', PRecord::Optional)
        ))
        .parse(arena, state)?;

        match opt_loc_val {
            Some(First(_)) => {
                let val_parser = specialize_ref(PRecord::Syntax, loc_pattern(min_indent));
                let (_, loc_val, state) =
                    space0_before_e(val_parser, min_indent, PRecord::Space, PRecord::IndentColon)
                        .parse(arena, state)?;

                let Located { value, region } = loc_val;

                Ok((
                    MadeProgress,
                    Pattern::RequiredField(
                        loc_label.value,
                        arena.alloc(arena.alloc(value).with_spaces_before(spaces, region)),
                    ),
                    state,
                ))
            }
            Some(Second(_)) => {
                let val_parser =
                    specialize_ref(PRecord::Syntax, loc!(crate::expr::expr(min_indent)));

                let (_, loc_val, state) =
                    space0_before_e(val_parser, min_indent, PRecord::Space, PRecord::IndentColon)
                        .parse(arena, state)?;

                let Located { value, region } = loc_val;

                Ok((
                    MadeProgress,
                    Pattern::OptionalField(
                        loc_label.value,
                        arena.alloc(arena.alloc(value).with_spaces_before(spaces, region)),
                    ),
                    state,
                ))
            }
            // If no value was provided, record it as a Var.
            // Canonicalize will know what to do with a Var later.
            None => {
                let value = if !spaces.is_empty() {
                    let Located { value, .. } = loc_label;

                    Pattern::SpaceAfter(arena.alloc(Pattern::Identifier(value)), spaces)
                } else {
                    let Located { value, .. } = loc_label;
                    Pattern::Identifier(value)
                };

                Ok((MadeProgress, value, state))
            }
        }
    }
}
