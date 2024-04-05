// use bumpalo::collections::string::String;
// use bumpalo::collections::vec::Vec;
use bumpalo::Bump;
use roc_error_macros::internal_error;
use roc_parse::ast::Expr;
// use roc_parse::ast::{Attempting, Expr};
// use roc_parse::ident;
// use roc_parse::parser::{unexpected, unexpected_eof, Fail, Parser, State};
// use roc_parse::problems::{Problem, Problems};
// use roc_region::all::{Loc, Region};
use roc_region::all::Region;
// use std::char;
// use std::iter::Peekable;

pub fn canonical_string_literal<'a>(_arena: &Bump, _raw: &'a str, _region: Region) -> Expr<'a> {
    internal_error!("TODO restore canonicalization");
}
// let mut problems = std::vec::Vec::new();

// // Stores the accumulated string characters
// let mut buf = String::new_in(arena);

// // This caches the total string length of interpolated_pairs. Every
// // time we add a new pair to interpolated_pairs, we increment this
// // by the sum of whatever we parsed in order to obtain that pair.
// let mut buf_col_offset: usize = 0;

// // Stores interpolated identifiers, if any.
// let mut interpolated_pairs = Vec::new_in(arena);

// let mut chars = raw.chars();

// while let Some(ch) = chars.next() {
//     match ch {
//         // If it's a backslash, escape things.
//         '\\' => match chars.next() {
//             Some(next_ch) => {
//                 if let Some(ident) = handle_escaped_char(
//                     arena,
//                     &state,
//                     next_ch,
//                     &mut chars,
//                     &mut buf,
//                     &mut problems,
//                 )? {
//                     let expr = Expr::Var(ident);

//                     // +2 for `$(` and then another +1 for `)` at the end
//                     let parsed_length = buf.len() + 2 + ident.len() + 1;

//                     // Casting should always succeed in this section, because
//                     // if this string literal overflowed our maximum
//                     // line length, that would have already happened back
//                     // in the parsing step, and we never would have reached
//                     // this code. Still, debug_assert that they won't!
//                     debug_assert!(buf_col_offset <= u16::MAX as usize);
//                     debug_assert!(ident.len() <= u16::MAX as usize);
//                     debug_assert!((parsed_length - ident.len() - 1) <= u16::MAX as usize);

//                     let start_line = state.line;

//                     // Subtract ident length and another 1 for the `)`
//                     let start_col = state.column
//                         + buf_col_offset as u16
//                         + (parsed_length - ident.len() - 1) as u16;
//                     let ident_region = Region {
//                         start_line,
//                         start_col,
//                         end_line: start_line,
//                         end_col: start_col + ident.len() as u16 - 1,
//                     };
//                     let loc_expr = Loc {
//                         region: ident_region,
//                         value: expr,
//                     };

//                     // Push the accumulated string into the pairs list,
//                     // along with the ident that came after it.
//                     interpolated_pairs.push((buf.into_bump_str(), loc_expr));

//                     // Reset the buffer so we start working on a new string.
//                     buf = String::new_in(arena);

//                     // Advance the cached offset of how many chars we've parsed,
//                     // so the next time we see an interpolated ident, we can
//                     // correctly calculate its region.
//                     buf_col_offset += parsed_length;
//                 }
//             }
//             None => {
//                 problems.push(loc_char(Problem::TrailingBackslash, &state, buf.len()));
//             }
//         },
//         '\t' => {
//             // Tabs are syntax errors.
//             problems.push(loc_char(Problem::Tab, &state, buf.len()));
//         }
//         '\r' => {
//             // Carriage returns aren't allowed in string literals.
//             problems.push(loc_char(Problem::CarriageReturn, &state, buf.len()));
//         }
//         normal_char => buf.push(normal_char),
//     }
// }

// // We ran out of characters; this is the end of the string!
// if problems.is_empty() {
//     let final_str = buf.into_bump_str();

//     if interpolated_pairs.is_empty() {
//         Expr::Str(final_str)
//     } else {
//         let tuple_ref = arena.alloc((interpolated_pairs.into_bump_slice(), final_str));

//         Expr::InterpolatedStr(tuple_ref)
//     }
// } else {
//     Expr::MalformedStr(problems.into_boxed_slice())
// }
// }

// fn loc_char<'a, V>(value: V, state: &State<'a>, buf_len: usize) -> Located<V> {
// let start_line = state.line;
// let start_col = state.column + buf_len as u16;
// let end_line = start_line;
// // All invalid chars should have a length of 1
// let end_col = state.column + 1;

// let region = Region {
//     start_line,
//     start_col,
//     end_line,
//     end_col,
// };

// Loc { region, value }
// }

// fn loc_escaped_char<'a, V>(value: V, state: &State<'a>, buf_len: usize) -> Located<V> {
// let start_line = state.line;
// let start_col = state.column + buf_len as u16;
// let end_line = start_line;
// // escapes should all be 2 chars long
// let end_col = state.column + 1;

// let region = Region {
//     start_line,
//     start_col,
//     end_line,
//     end_col,
// };

// Loc { region, value }
// }

// fn loc_escaped_unicode<'a, V>(
// value: V,
// state: &State<'a>,
// buf_len: usize,
// hex_str_len: usize,
// ) -> Located<V> {
// let start_line = state.line;
// // +1 due to the `"` which precedes buf.
// let start_col = state.column + buf_len as u16 + 1;
// let end_line = start_line;
// // +3 due to the `\u{` and another + 1 due to the `}`
// // -1 to prevent overshooting because end col is inclusive.
// let end_col = start_col + 3 + hex_str_len as u16 + 1 - 1;

// let region = Region {
//     start_line,
//     start_col,
//     end_line,
//     end_col,
// };

// Loc { region, value }
// }

// #[inline(always)]
// fn handle_escaped_char<'a, I>(
// arena: &'a Bump,
// state: &State<'a>,
// ch: char,
// chars: &mut Peekable<I>,
// buf: &mut String<'a>,
// problems: &mut Problems,
// ) -> Result<Option<&'a str>, (Fail, State<'a>)>
// where
// I: Iterator<Item = char>,
// {
// match ch {
//     '\\' => buf.push('\\'),
//     '"' => buf.push('"'),
//     't' => buf.push('\t'),
//     'n' => buf.push('\n'),
//     'r' => buf.push('\r'),
//     '0' => buf.push('\0'), // We explicitly support null characters, as we
//     // can't be sure we won't receive them from Rust.
//     'u' => handle_escaped_unicode(arena, &state, chars, buf, problems)?,
//     '(' => {
//         let ident = parse_interpolated_ident(arena, state, chars)?;

//         return Ok(Some(ident));
//     }
//     '\t' => {
//         // Report and continue.
//         // Tabs are syntax errors, but maybe the rest of the string is fine!
//         problems.push(loc_escaped_char(Problem::Tab, &state, buf.len()));
//     }
//     '\r' => {
//         // Report and continue.
//         // Carriage returns aren't allowed in string literals,
//         // but maybe the rest of the string is fine!
//         problems.push(loc_escaped_char(Problem::CarriageReturn, &state, buf.len()));
//     }
//     '\n' => {
//         // Report and bail out.
//         // We can't safely assume where the string was supposed to end.
//         problems.push(loc_escaped_char(
//             Problem::NewlineInLiteral,
//             &state,
//             buf.len(),
//         ));

//         return Err(unexpected_eof(
//             buf.len(),
//             Attempting::UnicodeEscape,
//             state.clone(),
//         ));
//     }
//     _ => {
//         // Report and continue.
//         // An unsupported escaped char (e.g. \q) shouldn't halt parsing.
//         problems.push(loc_escaped_char(
//             Problem::UnsupportedEscapedChar,
//             &state,
//             buf.len(),
//         ));
//     }
// }

// Ok(None)
// }

// #[inline(always)]
// fn handle_escaped_unicode<'a, I>(
// arena: &'a Bump,
// state: &State<'a>,
// chars: &mut Peekable<I>,
// buf: &mut String<'a>,
// problems: &mut Problems,
// ) -> Result<(), (Fail, State<'a>)>
// where
// I: Iterator<Item = char>,
// {
// // \u{00A0} is how you specify a Unicode code point,
// // so we should always see a '{' next.
// if chars.next() != Some('{') {
//     let start_line = state.line;
//     // +1 due to the `"` which precedes buf
//     let start_col = state.column + 1 + buf.len() as u16;
//     let end_line = start_line;

//     // All we parsed was `\u`, so end on the column after `\`'s column.
//     let end_col = start_col + 1;

//     let region = Region {
//         start_line,
//         start_col,
//         end_line,
//         end_col,
//     };

//     problems.push(Loc {
//         region,
//         value: Problem::NoUnicodeDigits,
//     });

//     // The rest of the string literal might be fine. Keep parsing!
//     return Ok(());
// }

// // Record the point in the string literal where we started parsing `\u`
// let start_of_unicode = buf.len();

// // Stores the accumulated unicode digits
// let mut hex_str = String::new_in(arena);

// while let Some(hex_char) = chars.next() {
//     match hex_char {
//         '}' => {
//             // Done! Validate and add it to the buffer.
//             match u32::from_str_radix(&hex_str, 16) {
//                 Ok(code_pt) => {
//                     if code_pt > 0x10FFFF {
//                         let start_line = state.line;
//                         // +1 due to the `"` which precedes buf
//                         // +3 due to the `\u{` which precedes the hex digits
//                         let start_col = state.column + 1 + buf.len() as u16 + 3;
//                         let end_line = start_line;

//                         // We want to underline only the number. That's the error!
//                         // -1 because we want to end on the last digit, not
//                         // overshoot it.
//                         let end_col = start_col + hex_str.len() as u16 - 1;

//                         let region = Region {
//                             start_line,
//                             start_col,
//                             end_line,
//                             end_col,
//                         };

//                         problems.push(Loc {
//                             region,
//                             value: Problem::UnicodeCodePtTooLarge,
//                         });
//                     } else {
//                         // If it all checked out, add it to
//                         // the main buffer.
//                         match char::from_u32(code_pt) {
//                             Some(ch) => buf.push(ch),
//                             None => {
//                                 problems.push(loc_escaped_unicode(
//                                     Problem::InvalidUnicodeCodePt,
//                                     &state,
//                                     start_of_unicode,
//                                     hex_str.len(),
//                                 ));
//                             }
//                         }
//                     }
//                 }
//                 Err(_) => {
//                     let problem = if hex_str.is_empty() {
//                         Problem::NoUnicodeDigits
//                     } else {
//                         Problem::NonHexCharsInUnicodeCodePt
//                     };

//                     problems.push(loc_escaped_unicode(
//                         problem,
//                         &state,
//                         start_of_unicode,
//                         hex_str.len(),
//                     ));
//                 }
//             }

//             // We are now done processing the unicode portion of the string,
//             // so exit the loop without further advancing the iterator.
//             return Ok(());
//         }
//         '\t' => {
//             // Report and continue.
//             // Tabs are syntax errors, but maybe the rest of the string is fine!
//             problems.push(loc_escaped_unicode(
//                 Problem::Tab,
//                 &state,
//                 start_of_unicode,
//                 hex_str.len(),
//             ));
//         }
//         '\r' => {
//             // Report and continue.
//             // Carriage returns aren't allowed in string literals,
//             // but maybe the rest of the string is fine!
//             problems.push(loc_escaped_unicode(
//                 Problem::CarriageReturn,
//                 &state,
//                 start_of_unicode,
//                 hex_str.len(),
//             ));
//         }
//         '\n' => {
//             // Report and bail out.
//             // We can't safely assume where the string was supposed to end.
//             problems.push(loc_escaped_unicode(
//                 Problem::NewlineInLiteral,
//                 &state,
//                 start_of_unicode,
//                 hex_str.len(),
//             ));

//             return Err(unexpected_eof(
//                 buf.len(),
//                 Attempting::UnicodeEscape,
//                 state.clone(),
//             ));
//         }
//         normal_char => hex_str.push(normal_char),
//     }

//     // If we're about to hit the end of the string, and we didn't already
//     // complete parsing a valid unicode escape sequence, this is a malformed
//     // escape sequence - it wasn't terminated!
//     if chars.peek() == Some(&'"') {
//         // Record a problem and exit the loop early, so the string literal
//         // parsing logic can consume the quote and do its job as normal.
//         let start_line = state.line;
//         // +1 due to the `"` which precedes buf.
//         let start_col = state.column + buf.len() as u16 + 1;
//         let end_line = start_line;
//         // +3 due to the `\u{`
//         // -1 to prevent overshooting because end col is inclusive.
//         let end_col = start_col + 3 + hex_str.len() as u16 - 1;

//         let region = Region {
//             start_line,
//             start_col,
//             end_line,
//             end_col,
//         };

//         problems.push(Loc {
//             region,
//             value: Problem::MalformedEscapedUnicode,
//         });

//         return Ok(());
//     }
// }

// Ok(())
// }

// #[inline(always)]
// fn parse_interpolated_ident<'a, I>(
// arena: &'a Bump,
// state: &State<'a>,
// chars: &mut Peekable<I>,
// ) -> Result<&'a str, (Fail, State<'a>)>
// where
// I: Iterator<Item = char>,
// {
// // This will return Err on invalid identifiers like "if"
// let ((string, next_char), state) = ident::parse_into(arena, chars, state.clone())?;

// // Make sure we got a closing ) to end the interpolation.
// match next_char {
//     Some(')') => Ok(string),
//     Some(ch) => Err(unexpected(ch, 0, state, Attempting::InterpolatedString)),
//     None => Err(unexpected_eof(0, Attempting::InterpolatedString, state)),
// }
// }
