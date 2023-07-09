use std::iter::Iterator;

use crate::bitmask::{Bitmask, Chunk};
use packed_simd::{u8x64, Simd};

pub struct Env {
    in_progress: InProgress,
    structural_chars: Vec<Bitmask>,
    /// The number of trailing '"' characters at the end of the previous chunk
    trailing_quote_chars: u8,
}

// 1 bits for each even index, or each odd index.
// See Fig. 3 in "Parsing Gigabytes of JSON per Second" https://arxiv.org/pdf/1902.08318.pdf
const EVENS: Bitmask =
    Bitmask::new(0b1010101010101010101010101010101010101010101010101010101010101010);
const ODDS: Bitmask =
    Bitmask::new(0b0101010101010101010101010101010101010101010101010101010101010101);

#[repr(u8)]
#[derive(Copy, Clone, PartialEq, Eq)]
enum LexingOptions {
    MultiLineStr,
    NoMultiLine,
}

#[repr(u8)]
#[derive(Copy, Clone, PartialEq, Eq)]
enum InProgress {
    Nothing = 0,
    MultiLineStr = 1,  // must be odd (for is_str)
    SingleLineStr = 3, // must be odd (for is_str), and must be the second-highest number (for is_comment_or_single_line_str)
    Comment = 4, // must be even (for is_str), and must be the highest number (for is_comment_or_single_line_str)
}

impl InProgress {
    /// Branchlessly returns true iff this is either SingleLineStr or Comment
    fn is_comment_or_single_line_str(&self) -> bool {
        *self as u8 >= Self::SingleLineStr as u8
    }

    /// Branchlessly returns true iff this is either SingleLineStr or MultiLineStr
    fn is_str(&self) -> bool {
        // MutliLineStr and SingleLineStr are both odd, so just return whether this is odd
        (*self as u8) & 1 != 0
    }
}

impl Env {
    pub fn handle_chunk(&mut self, chunk: Chunk) {
        let simd_chunk = u8x64::from_slice_unaligned(&chunk);
        let backslashes = Bitmask::from(simd_chunk.eq(u8x64::splat(b'\\')));

        // A bitmask of all the characters that are immediately preceded by an escape
        // (so, an odd number of contiguous backslashes) - e.g. \\\" or \'
        // See Fig. 3 in "Parsing Gigabytes of JSON per Second" https://arxiv.org/pdf/1902.08318.pdf
        //
        // We use this for detecting escapes in both double quotes and single quotes.
        let preceded_by_escape = {
            // Start of each contiguous sequence of backslashes
            //
            //   input: 0001110000000000000000111100000000000000001000000000000111110000
            // shifted: 0000111000000000000000011110000000000000000100000000000011111000
            // negated: 1111000111111111111111100001111111111111111011111111111100000111
            //   anded: 0001000000000000000000100000000000000000001000000000000100000000
            //
            // Note: Fig. 3 of the paper uses `<<` instead of `>>` here, but that produces
            // a different output (the last backslash in the sequence rather than the first)
            // than what the figure shows. Presumably `>>` was intended.
            let backslash_starts = backslashes & !(backslashes >> 1);

            // I don't understand this part of the paper, but apparently it works.
            let starts_on_even = backslash_starts & EVENS;
            let starts_on_odd = backslash_starts & ODDS;
            let evens_carries = backslashes.wrapping_add(starts_on_even) & !backslashes;
            let odds_carries = backslashes.wrapping_add(starts_on_odd) & !backslashes;

            // The character following each backslash sequence whose length is odd.
            // If this is a quote, then it's an escaped quote!
            (evens_carries & ODDS) | (odds_carries & EVENS)
        };

        // Bitmask of all the non-escaped double quotes in the chunk.
        let quotes = Bitmask::from(simd_chunk.eq(u8x64::splat(b'"'))) & !preceded_by_escape;

        // Bitmask of all the non-escaped single quotes ("ticks") in the chunk.
        let ticks = Bitmask::from(simd_chunk.eq(u8x64::splat(b'\''))) & !preceded_by_escape;

        // Three consecutive quotes are multiline strings. (We already removed \" from `quotes`.)
        let triple_quotes =
            // Start of each contiguous sequence of 3+ double quotes. These can overlap
            // (e.g. `"""` becomes 100 and `""""` becomes 1100), which in this case is
            // desirable because it means a sequence like `""""""` can become 111100,
            // which we can use to easily detect the two back-to-back triple quotes: each
            // time we encounter a 1 when iterating, make sure we always advance the index
            // by a minimum of 3. This will skip over the redundant two 1s and take us to
            // the next group of 3, which is right where we want to be.
            //
            //   input: 0001110111001111110000111100000011000000001010110000000111111100
            // shift 1: 0011101110011111100001111000000110000000010101100000001111111000
            // shift 2: 0111011100111111000011110000001100000000101011000000011111110000
            //   anded: 0001000100001111000000110000000000000000000000000000000111110000
            //
            // This algorithm was adapted from the backslash-escaping algorithm from the paper.
            //
            // Thanks, Daniel Lemire and Geoff Langdale!
            quotes & (quotes << 1) & (quotes << 2);

        if triple_quotes.is_zero() {
            self.lex(quotes, simd_chunk, chunk, LexingOptions::NoMultiLine);
        } else {
            self.lex(quotes, simd_chunk, chunk, LexingOptions::MultiLineStr);
        }
    }

    fn lex(
        &mut self,
        quotes: Bitmask,
        simd_chunk: Simd<[u8; 64]>,
        chunk: [u8; 64],
        options: LexingOptions,
    ) {
        use InProgress::{Comment, MultiLineStr, Nothing, SingleLineStr};

        let pounds = Bitmask::from(simd_chunk.eq(u8x64::splat(b'#')));
        let newlines = Bitmask::from(simd_chunk.eq(u8x64::splat(b'\n')));

        let mut strings = Bitmask::from(0);
        let mut comments = Bitmask::from(!0);
        let quote_pound_newline = Bitmask::from(quotes | pounds | newlines);
        let mut in_progress = self.in_progress;
        let mut start_index = 0;
        let mut prev_byte = b'\0';
        for index in quote_pound_newline.ones() {
            // This is safe because this index is from a u64 bitmask, and Chunk has length 64
            let byte = unsafe { *chunk.get_unchecked(index as usize) };
            let prev_in_progress = in_progress;
            let byte_is_quote = byte == b'"';

            // Use a branch to check for the end of a multiline string. Branch because multiline strings are uncommon.
            if branchless_and(in_progress == MultiLineStr, byte_is_quote) {
                todo!(
                    r#"
                        peek at the next two chars;
                        if they are both quotes, we're done with this multiline string.
                        Advance index by 2 (this will require no longer using an iterator) and update the state.
                        "#
                );
            }

            // Use a branch to check for the start of a multiline string, because multiline strings are uncommon.
            // If both this byte and the previous one was a quote, but we had nothing in progress (meaning we just
            // closed a string, although we don't know if it was single-line or multiline) then this might be the
            // start of a multiline string. (It might also be a snytax error.)
            if branchless_and(
                byte_is_quote,
                branchless_and(in_progress == Nothing, prev_byte == b'"'),
            ) {
                todo!(
                    r#"
                        Inspect the previous two bytes (they might be in the previous chunk, so must handle that
                        using carryover state regarding number of trailing quotes in a chunk) to verify that they
                        are both quotes. Also, it could be that there are more than 3 consecutive quotes there.
                        Need to figure out what to do in that scenario.
                        "#
                );
            }

            // Branchlessly update in_progress
            {
                in_progress = if branchless_and(byte_is_quote, in_progress == Nothing) {
                    // We had nothing going on, but now we've hit a quote; we're working on a single-line string now.
                    SingleLineStr
                } else {
                    in_progress
                };

                in_progress = if branchless_and(byte == b'#', in_progress == Nothing) {
                    // We had nothing going on, but now we've hit a pound; we're working on a comment now.
                    Comment
                } else {
                    in_progress
                };

                // We were working on a comment or single-line str and encountered a newline; either way, it's done.
                // (Single-line strings can't contain newlines; if they do, it's both done and Erroneous. We want
                // stop now for graceful error handling, so we don't mark the rest of the document as a string.)
                in_progress =
                    if branchless_and(byte == b'\n', in_progress.is_comment_or_single_line_str()) {
                        Nothing
                    } else {
                        in_progress
                    };

                // If none of these conditions applied, then we keep going. This covers cases like (for example):
                // - Encountering a '#' while we're working on a string
                // - Encountering a '"' while we're working on a comment
                // - Encountering a '\n' while we're working on a multiline string
            }

            // If we had nothing in progress, but now do, then we just started something.
            start_index = if branchless_and(prev_in_progress == Nothing, in_progress != Nothing) {
                index
            } else {
                start_index
            };

            // Apply masks if we just finished a span (string or comment).
            {
                let span_len = index.wrapping_sub(start_index);
                // A mask for the span between start_index and index
                let span_mask_zeroes = ((1u64 << span_len) - 1) << start_index;
                let span_mask_ones = !span_mask_zeroes;

                // If we just finished a string, branchlessly apply it to our main bitmask.
                {
                    // If we had a string in progress, but no longer do, then we just ended a string.
                    let str_mask =
                        if branchless_and(prev_in_progress.is_str(), in_progress == Nothing) {
                            span_mask_ones // 1s only where the string we just made was; 0s elsewhere.
                        } else {
                            !0u64 // all 1s; if this is AND'd with the existing bitmask, it will be a no-op.
                        };

                    strings &= Bitmask::from(str_mask);
                }

                // If we just finished a comment, branchlessly zero out its span in our main bitmask.
                {
                    let comment_mask =
                        if branchless_and(prev_in_progress == Comment, in_progress == Nothing) {
                            span_mask_zeroes // 0s only where the comment we just made was; 1s elsewhere.
                        } else {
                            0u64 // all 0s; if this is OR'd with the existing bitmask, it will be a no-op.
                        };

                    comments |= Bitmask::from(comment_mask);

                    // TODO when we have a trait for what to do in various scenarios, this is roughly where to
                    // have it receive a "just finished a comment" event. (Only the formatter cares about comments,
                    // although other operations care about doc comments - e.g. `docs` but also editor tooling that
                    // wants to store docs in memory for hovers and such.)
                }
            }

            prev_byte = byte;
        }
        self.in_progress = in_progress;
    }
}

fn _stage2(structural_chars: &[Bitmask], src: &[u8]) {
    #[derive(Clone, Copy, PartialEq, Eq, Debug)]
    #[repr(u8)]
    enum InterpolatingIn {
        Nothing = 0,       // This must be the same number as what InProgress uses for it
        SingleLineStr = 1, // This must be the same number as what InProgress uses for it
        MultiLineStr = 2,  // This must be the same number as what InProgress uses for it
    }

    #[repr(u8)]
    enum InProgress {
        Nothing = 0,       // This must be the same number as what InterpolatingIn uses for it
        SingleLineStr = 1, // This must be the same number as what InterpolatingIn uses for it
        MultiLineStr = 2,  // This must be the same number as what InterpolatingIn uses for it
        Comment,
    }

    let mut span_start_index = 0;
    let mut prev_index: isize = -1;
    let mut cur_line_indent = 0;
    let mut interpolating_in = InterpolatingIn::Nothing;
    let mut multiline_str_indent = 0;
    let mut just_parsed_empty_str = false;
    let mut prev_char = b'\0';

    // TODO write tests that the relevant InterpolatingIn and InProgress variants are the same
    // when cast to u8, so we can transmute from InterpolatingIn to InProgress.

    let mut in_progress = InProgress::Nothing;
    let mut tape = Vec::new();

    for (index, bitmask) in structural_chars.iter().copied().enumerate() {
        for offset in bitmask.ones() {
            let index = index + offset as usize;

            // TODO in release builds, don't do a bounds check here. This should never be out of bounds.
            let char = src[index];

            // Branchlessly update cur_line_indent.
            cur_line_indent = {
                let prev_was_newline = prev_char == b'\n';
                // Branchless way of writing:
                // if prev_char == b'\n' { index - last_newline_index } else { cur_ine_indent }
                let starting_point = if prev_was_newline {
                    index
                } else {
                    cur_line_indent
                };

                let to_subtract = if prev_was_newline {
                    // This will only be -1 at the start, and at that point, prev_was_newline is false,
                    // so this cast will always be lossless in practice.
                    prev_index as usize
                } else {
                    0
                };

                starting_point - to_subtract
            };

            match in_progress {
                InProgress::MultiLineStr => {
                    match char {
                        b'\"' => {
                            // Check if the next two chars are also quotes. If so, we're done!
                            if src.len() > index + 2
                                && unsafe {
                                    branchless_and(
                                        *src.get_unchecked(index + 1) == b'\"',
                                        *src.get_unchecked(index + 2) == b'\"',
                                    )
                                }
                            {
                                just_parsed_empty_str = false;
                                in_progress = InProgress::Nothing;
                            }
                        }
                        b'(' if prev_char == b'\\' => {
                            if interpolating_in == InterpolatingIn::Nothing {
                                // We're beginning string interpolation, so end the string.
                                in_progress = InProgress::Nothing;
                                interpolating_in = InterpolatingIn::MultiLineStr;
                            } else {
                                todo!(
                                        "Report error: Interpolation inside interpolation is not allowed!"
                                    );
                            }
                        }
                        b'\n' => {
                            // Newlines inside multiline strings never do anything (aside from indentation implications).
                        }
                        _ => {
                            // All other structural characters go inside the string, but first we have to
                            // check indentation level if we're the first structural (non-space) char after a newline.
                            // (We don't care if a blank line has some trailing spaces inside a multiline str.)
                            //
                            // We'll do this branchlessly.
                            let prev_was_newline = prev_char == b'\n';
                            let expected_indentation = if prev_was_newline {
                                multiline_str_indent
                            } else {
                                0
                            };

                            if branchless_and(
                                prev_was_newline,
                                cur_line_indent < expected_indentation,
                            ) {
                                eprintln!(
                                    "Error: outdent on a non-blank line inside a multiline str."
                                );
                            }

                            // Otherwise, we do nothing, and just let the structural char join the mutliline string.
                            //
                            // In terms of structural characters, this means that we store a bunch of unnecessary
                            // indices inside mutliline strings, thinking they're structurally relevent when
                            // actually they're part of the string. So we iterate over them needlessly.
                            // Not ideal but not the end of the world either.
                        }
                    }
                }
                InProgress::SingleLineStr => {
                    match char {
                        b'\"' => {
                            let str_slice = unsafe {
                                std::str::from_utf8_unchecked(&src[span_start_index..(index - 1)])
                            };
                            let node = Ast::StrLiteral(str_slice.to_string());
                            just_parsed_empty_str = str_slice.is_empty();

                            tape.push(node);

                            in_progress = InProgress::Nothing;
                        }
                        b'(' if prev_char == b'\\' => {
                            if interpolating_in == InterpolatingIn::Nothing {
                                // We're beginning string interpolation, so end the string.
                                in_progress = InProgress::Nothing;
                                interpolating_in = InterpolatingIn::SingleLineStr;
                            } else {
                                todo!(
                                        "Report error: Interpolation inside interpolation is not allowed!"
                                    );
                            }
                        }
                        b'\n' => {
                            just_parsed_empty_str = false;
                            eprintln!("String literal hit a newline before it was closed.");
                            in_progress = InProgress::Nothing;
                        }
                        _ => {
                            // All other characters just go inside the string.
                        }
                    }
                }
                InProgress::Comment => {
                    just_parsed_empty_str = false;

                    // Newline ends the comment
                    if char == b'\n' {
                        let node = Ast::Comment(
                            unsafe {
                                std::str::from_utf8_unchecked(&src[span_start_index..(index - 1)])
                            }
                            .to_string(),
                        );

                        tape.push(node);
                        in_progress = InProgress::Nothing;
                    }
                }
                InProgress::Nothing => {
                    span_start_index = index;

                    if branchless_and(char == b'\n', interpolating_in != InterpolatingIn::Nothing) {
                        eprintln!("Error: Newlines are not allowed inside string interpolation.");
                    }

                    let is_beginning_of_multiline_str =
                        branchless_and(just_parsed_empty_str, char == b'"');

                    // Much more likely that we aren't exactly about to encounter a multiline str,
                    // so put that scenario in the `else` branch.
                    if !is_beginning_of_multiline_str {
                        just_parsed_empty_str = false;

                        // If we're interpolating and hit a close paren, we go back to whatever we were
                        // doing before.
                        if branchless_and(
                            interpolating_in != InterpolatingIn::Nothing,
                            char == b')',
                        ) {
                            // This should be safe becasue we hardcode the backing numbers to
                            // match up.
                            in_progress = unsafe {
                                std::mem::transmute::<InterpolatingIn, InProgress>(interpolating_in)
                            };

                            continue;
                        }

                        // TODO try to handle commas here. How can we distinguish between multi-backpassing
                        // and list elements? e.g.
                        //
                        // Scenario 1:
                        //     list = [
                        //         a, b, c
                        //         d, e, f
                        //     ]
                        //
                        // Scenario 2:
                        //     list = [
                        //         a, b <- c
                        //         d, e, f
                        //     ]
                        //
                        // One simple idea: when the preceding token is a comma, we are in a zone of "blocks require parens"
                        // So no defs, no backpassing, no statements - not unless you surround them by parens.

                        match char {
                            b'\"' => {
                                in_progress = InProgress::SingleLineStr;
                            }
                            b'#' => {
                                in_progress = InProgress::Comment;
                            }
                            b'\n' => {
                                in_progress = InProgress::Nothing; // Setting explicitly for when redoing this with simd classifier
                            }
                            b',' => {
                                in_progress = InProgress::Nothing; // Setting explicitly for when redoing this with simd classifier
                            }
                            b'(' => {
                                todo!("push parens onto the stack or something");
                            }
                            b')' => {
                                todo!("close parens currently on the stack or something");
                            }
                            byte => {
                                unreachable!(
                                "somehow a {:?} was recorded as a structural char, but it isn't",
                                byte as char
                            )
                            }
                        }
                    } else {
                        multiline_str_indent = index - span_start_index;
                        in_progress = InProgress::MultiLineStr;
                    }
                }
            }

            // We'll never iterate past isize::MAX, so this cast will never overflow.
            prev_index = index as isize;
            prev_char = char;
        }
    }
}

enum Ast {
    StrLiteral(String),
    Comment(String),
}

fn branchless_and(a: bool, b: bool) -> bool {
    (a as u8 & b as u8) != 0
}
