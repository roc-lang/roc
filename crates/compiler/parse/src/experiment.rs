use crate::bitmask::{Bitmask, Chunk};
use crate::token::{MaybeToken, Region, Token, TokenMap};

/// This is pretty big on the stack because of TokenMap; callers should consider using a Box<Env>.
#[derive(Default)]
pub struct Env {
    in_progress: InProgress,
    /// The number of trailing '"' characters at the end of the previous chunk
    trailing_quote_chars: u8,
    /// The current chunk's offset relative to the start of the src.
    /// This must be specified in terms of the chunk's offset (that is,
    /// its offset from the beginning of the source) rather than the index into
    /// the chunk. This is because sometimes it can start earlier than the first
    /// byte of the chunk, for example if the previous chunk ended in quotes, which
    /// turned out to be the beginning of a multiline string.
    chunk_offset: u32,
    span_start_offset: u32,
    token_map: TokenMap,
}

impl From<InProgress> for MaybeToken {
    #[inline(always)]
    fn from(in_progress: InProgress) -> Self {
        debug_assert_eq!(
            match in_progress {
                InProgress::Nothing => 0,
                InProgress::Comment => Token::Comment as u8,
                InProgress::Lambda => Token::Lambda as u8,
                InProgress::MultiLineStr => Token::MultiLineStr as u8,
                InProgress::SingleLineStr => Token::SingleLineStr as u8,
                InProgress::SingleQuoteChar => Token::SingleLineStr as u8,
            },
            in_progress as u8
        );

        unsafe { std::mem::transmute::<InProgress, MaybeToken>(in_progress) }
    }
}

// Edge cases in quotes ending on chunk boundaries:
//
//            v-------- chunk boundary
//      """"""|""yz
//      a"""""|""yz
//      ab""""|""yz
//      abc"""|""yz
//      abcd""|""yz
//      abcd""|"xyz
//      abcde"|"xyz
//      abcde"|""yz
//      abcde"|"""z
//      abcde"|""""
//
// In general, 1, 2, and 3 quotes in a row are valid, but more than 3 quotes in a row is invalid.
// (You can't do "open and then immediately close multiline string" - there's only one "empty string" syntax!)
// We can rely on this to conclude that we only care about exactly 1 or 2 quotes at the end of the chunk.
//
// Crucially, if we have exactly 2 quotes at the end, we must not interpret them as quotes, because if we do,
// we might end up concluding it's an empty string. However, we don't have enough info yet to conclude that;
// it might turn out to be the beginning of a (triple-quoted) multiline string.

// 1 for each even index, or each odd index.
// See Fig. 3 in "Parsing Gigabytes of JSON per Second" https://arxiv.org/pdf/1902.08318.pdf
const EVENS: Bitmask =
    Bitmask::new(0b1010101010101010101010101010101010101010101010101010101010101010);
const ODDS: Bitmask =
    Bitmask::new(0b0101010101010101010101010101010101010101010101010101010101010101);

#[repr(u8)]
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
enum InProgress {
    Nothing = 0,
    Comment = 1,
    Lambda = 2,
    // The strings must have the highest numeric values, for `is_str` to work
    MultiLineStr = 7, // This must have the lowest numeric value of the strings, for `is_str` to work
    SingleLineStr = 8,
    SingleQuoteChar = 9, // We consider this a "string"
}

impl InProgress {
    pub fn is_str(self) -> bool {
        self as u8 >= Self::MultiLineStr as u8
    }
}

impl Default for InProgress {
    fn default() -> Self {
        Self::Nothing
    }
}

pub trait Listener {
    fn emit(&mut self, token: Token, start_offset: u32, end_offset: u32);
}

impl Env {
    pub fn handle_chunk(&mut self, chunk: Chunk, listener: &mut impl Listener) {
        let backslashes = Bitmask::for_byte(chunk, b'\\');

        // A bitmask of all the characters that are immediately preceded by an escape
        // (so, an odd number of contiguous backslashes) - e.g. \\\" or \'
        // See Fig. 3 in "Parsing Gigabytes of JSON per Second" https://arxiv.org/pdf/1902.08318.pdf
        //
        // We use this for detecting escapes in both double quotes and single quotes.
        let preceded_by_escape = {
            // Start of each contiguous sequence of backslashes
            //
            //  input str: ___\\\________________\\\\________________\____________\\\\\____
            // input bits: 0001110000000000000000111100000000000000001000000000000111110000
            //    shifted: 0000111000000000000000011110000000000000000100000000000011111000
            //    negated: 1111000111111111111111100001111111111111111011111111111100000111
            //      anded: 0001000000000000000000100000000000000000001000000000000100000000
            //  input str: ___\\\________________\\\\________________\____________\\\\\____
            //
            // Note: Fig. 3 of the paper uses `<<` instead of `>>` here, but that produces
            // a different output (the last backslash in the sequence rather than the first)
            // than what the figure shows. Presumably `>>` was intended.
            let backslash_starts = backslashes & !(backslashes >> 1);

            // I don't understand this part of the paper, but evidently it works.
            let starts_on_even = backslash_starts & EVENS;
            let starts_on_odd = backslash_starts & ODDS;
            let non_backslashes = !backslashes;
            let evens_carries = backslashes.wrapping_add(starts_on_even) & non_backslashes;
            let odds_carries = backslashes.wrapping_add(starts_on_odd) & non_backslashes;

            // The character following each backslash sequence whose length is odd.
            // If this is a quote, then it's an escaped quote!
            !((evens_carries & ODDS) | (odds_carries & EVENS))
        };

        // Bitmask of all the single quotes (`'`) in the chunk, excluding escaped ones (`\'`, `\\\'`, etc.).
        let single_quotes = Bitmask::for_byte(chunk, b'\'') & preceded_by_escape;

        // Bitmask of all the double quotes (`"`) in the chunk, excluding escaped ones (`\"`, `\\\"`, etc.).
        let quotes = {
            let quotes = Bitmask::for_byte(chunk, b'"') & preceded_by_escape;

            // Trailing quotes caculation, based on the ending bit pattern of `quotes`:
            //
            //         0 => 0 trailing quotes
            //        01 => 1 trailing quote
            //       011 => 2 trailing quotes
            //      0111 => 0 and multiline string is in progress
            //     01111 => 1 and multiline string is in progress
            //    011111 => 2 and multiline string is in progress
            //   0111111 => 0 (multiline string opened and immediately closed)
            //  01111111 => 1 (multiline string opened and immediately closed, then a stray quote)
            // 011111111 => 2 (multiline string opened and immediately closed, then 2 stray quotes)
            //
            // The pattern repeats from here: we always have 0, 1, or 2 trailing quotes, and then a
            // multiline string is either in progress or not. (Multiline string in-progress is handled elsewhere.)
            let trailing_quote_chars = (quotes.trailing_ones() % 3) as u8;

            self.trailing_quote_chars = trailing_quote_chars;

            // Remove the trailing quotes from consideration in the `quotes` mask,
            // since they'll be handled with the next chunk rather than with this one.
            (quotes >> trailing_quote_chars) << trailing_quote_chars
        };

        // Bitmask for triple quotes (used in multiline strings). Note that we already removed \" from `quotes`.
        let triple_quotes =
            // Start of each contiguous sequence of 3+ double quotes. These can overlap
            // (e.g. `"""` becomes 100 and `""""` becomes 1100), which in this case is
            // desirable because it means a sequence like `""""""` can become 111100,
            // which we can use to easily detect the two back-to-back triple quotes: each
            // time we encounter a 1 when iterating, make sure we always advance the index
            // by a minimum of 3. This will skip over the redundant two 1s and take us to
            // the next group of 3, which is right where we want to be.
            //
            //   input str: ___"""_"""__""""""____""""______""________"_"_""_______"""""""__
            //  input bits: 0001110111001111110000111100000011000000001010110000000111111100
            //     shift 1: 0011101110011111100001111000000110000000010101100000001111111000
            //     shift 2: 0111011100111111000011110000001100000000101011000000011111110000
            //       anded: 0001000100001111000000110000000000000000000000000000000111110000
            //   input str: ___"""_"""__""""""____""""______""________"_"_""_______"""""""__
            //
            // This algorithm was adapted from the backslash-escaping algorithm from the paper.
            // Thanks, Daniel Lemire and Geoff Langdale!
            quotes & (quotes << 1) & (quotes << 2);

        self.strings_comments_indents(
            backslashes,
            quotes,
            single_quotes,
            triple_quotes,
            chunk,
            listener,
        );
    }

    /// Given the masks for the locations of quotes, single quotes, and triple quotes (which have already
    /// had backslash-escaped versions of themselves removed), compute the ranges for strings, comments, and indents/outdents.
    fn strings_comments_indents(
        &mut self,
        backslashes: Bitmask,
        quotes: Bitmask,
        single_quotes: Bitmask,
        triple_quotes: Bitmask,
        chunk: Chunk,
        listener: &mut impl Listener,
    ) {
        use InProgress::{Comment, MultiLineStr, Nothing, SingleLineStr, SingleQuoteChar};

        let open_parens = Bitmask::for_byte(chunk, b'(');
        let close_parens = Bitmask::for_byte(chunk, b')');

        let chunk_offset = self.chunk_offset;
        let mut span_start_offset = self.span_start_offset;
        let mut in_progress;

        match self.trailing_quote_chars {
            1 => {
                span_start_offset = chunk_offset - 1;

                // 1 trailing quote means we need to figure out if we're working on
                // a single-line string vs a multiline string. That in turn depends
                // on whether our first two bytes are both quotes. If either isn't,
                // this must not be a triple quote!
                in_progress = if branchless_and(chunk[0] == b'"', chunk[1] == b'"') {
                    MultiLineStr
                } else {
                    SingleLineStr
                };
            }
            2 => {
                if chunk[0] == b'"' {
                    // 2 trailing quotes plus this one starting with a quote means
                    // we encountered a triple quote.
                    let will_be_multiline_str = self.in_progress == Nothing;

                    in_progress = if will_be_multiline_str {
                        MultiLineStr
                    } else {
                        // Since in_progress wasn't Nothing, it must have been MultiLineStr
                        // (or else an earlier step has a bug, because if there were 2 quotes
                        // that were considered trailing, the only two in_progress values that
                        // should be possible are MultiLineStr and Nothing), so therefore
                        // this must be a closing triple-quote. That means we end the MultiLineStr.
                        debug_assert_eq!(self.in_progress, MultiLineStr, "Previous chunk ended in 2 quotes, yet its in_progress was neither MutliLineStr nor Nothing. This should never happen!");

                        // Record the MultiLineStr, between the start offset and the offset of the final quote.
                        self.token_map.insert(
                            0,
                            Token::MultiLineStr,
                            Region {
                                start_offset: span_start_offset,
                                end_offset: chunk_offset + 1,
                            },
                        );

                        Nothing
                    };

                    // If we will be starting a multiline string, it should start 2 bytes
                    // before the beginning of this chunk. (This is all done branchlessly.)
                    //
                    // Importantly, this must be set *after* updating in_progress, because
                    // the in_progress update logic may need to use the old span_start_offset.
                    span_start_offset = chunk_offset - if will_be_multiline_str { 2 } else { 0 };
                } else {
                    // 2 trailing quotes followed by a non-quote means
                    // we had an empty string at the end.
                    self.token_map.insert(
                        0,
                        Token::SingleLineStr,
                        Region {
                            start_offset: chunk_offset,
                            end_offset: chunk_offset + 1,
                        },
                    );

                    in_progress = Nothing;
                }
            }
            _ => {
                debug_assert_eq!(
                    self.trailing_quote_chars, 0,
                    "trailing_quote_chars should always be 0, 1, or 2, but here it was {:?}",
                    self.trailing_quote_chars
                );

                // Since there were 0 trailing quote chars, just continue whatever we were doing before.
                span_start_offset = self.span_start_offset;
                in_progress = self.in_progress;
            }
        };

        let mask_contains_single_or_triple_quotes =
            branchless_or(single_quotes.is_nonzero(), triple_quotes.is_nonzero());
        let pounds = Bitmask::for_byte(chunk, b'#');
        let newlines = Bitmask::for_byte(chunk, b'\n');
        // Note: this includes multiline strings, single-line strings, AND single-quoted chars!
        let mut strings = Bitmask::from(0);
        // This excludes parens at first, but may end up having close-parens injected when handling string interpolation,
        // to mark the end of the interpolation.
        let mut loop_mask = Bitmask::from(backslashes | quotes | pounds | newlines);

        while !loop_mask.is_zero() {
            let index = loop_mask.trailing_zeros() as u8;
            let offset = index as u32 + chunk_offset;

            loop_mask &= loop_mask.wrapping_sub(1); // clear the bit we just encountered

            // This is safe because this index is from a u64 bitmask, and Chunk has length 64
            let byte = unsafe { *chunk.get_unchecked(index as usize) };
            let prev_in_progress = in_progress;
            let byte_is_quote = byte == b'"';
            let byte_is_pound = byte == b'#';

            if byte == b'\\' {
                if in_progress.is_str() {
                    todo!("interpolation or escape! (Remember, could be an escaped backslash)");
                } else {
                    // If we encounter a backslash outside a string, it's a lambda.
                    self.token_map.insert(
                        index,
                        Token::Lambda,
                        Region {
                            start_offset: offset,
                            end_offset: offset,
                        },
                    );
                }
            }

            // This is both a convenience and a way to avoid consecutive conditionals triggering incorrectly
            // when updating in_progress.
            // (We must compare to nothing_in_progress after potentially setting in_progress to Nothing!)
            let nothing_in_progress = in_progress == Nothing;

            // Start with single and triple quote checks, so that later on when we go to branchlessly update in_progress,
            // if we've already e.g. set in_progress to MultiLineStr, won't accidentally overwrite it with the
            // normal quote check.
            //
            // Put these checks behind a branch, because most chunks won't contain any single quotes or triple quotes.
            // (If there is one of these in the mask, this will probably get mispredicted once and then never again.)
            if mask_contains_single_or_triple_quotes {
                // If the triple_quotes mask has a 1 here, then there's a triple quote here.
                let byte_is_triple_quote = (triple_quotes & (1 << index).into()).is_nonzero();
                let byte_is_single_quote = byte == b'\'';

                in_progress =
                    if branchless_and(byte_is_single_quote, in_progress == SingleQuoteChar) {
                        // We hit a closing single quote
                        Nothing
                    } else {
                        in_progress
                    };

                in_progress = if branchless_and(byte_is_single_quote, nothing_in_progress) {
                    // We had nothing going on, but now we've hit a single quote; we're working on a single-quote char now.
                    SingleQuoteChar
                } else {
                    in_progress
                };

                in_progress = if branchless_and(byte_is_triple_quote, in_progress == MultiLineStr) {
                    // We hit a closing triple quote
                    Nothing
                } else {
                    in_progress
                };

                in_progress = if branchless_and(byte_is_triple_quote, nothing_in_progress) {
                    // We had nothing going on, but now we've hit a triple quote; we're working on a multi-line string now.
                    MultiLineStr
                } else {
                    in_progress
                };
            }

            // Branchlessly update in_progress for single-line strings and comments.
            {
                in_progress = if branchless_and(byte_is_quote, in_progress == SingleLineStr) {
                    // We hit a closing quote
                    Nothing
                } else {
                    in_progress
                };

                in_progress = if branchless_and(byte_is_quote, nothing_in_progress) {
                    // We had nothing going on, but now we've hit a quote; we're working on a single-line string now.
                    SingleLineStr
                } else {
                    in_progress
                };

                // We were working on a comment or single-line str and encountered a newline; either way, it's done.
                // (Single-line strings can't contain newlines; if they do, it's both done and Erroneous. We want
                // stop now for graceful error handling, so we don't mark the rest of the document as a string.)
                in_progress = if branchless_and(
                    byte == b'\n',
                    branchless_or(in_progress == SingleLineStr, in_progress == Comment),
                ) {
                    Nothing
                } else {
                    in_progress
                };

                in_progress = if branchless_and(byte_is_pound, nothing_in_progress) {
                    // We had nothing going on, but now we've hit a pound; we're working on a comment now.
                    Comment
                } else {
                    in_progress
                };

                // If none of these conditions applied, then we keep going. This covers cases like (for example):
                // - Encountering a '#' while we're working on a string
                // - Encountering a '"' while we're working on a comment
                // - Encountering a '\n' while we're working on a multiline string
            }

            // Apply masks and update token_map with whatever we just finished (if anything).
            // A branch is necessary here because we might or might not need to invoke the listener,
            // but all the logic inside this block can be branchless.
            {
                // Convert the start offset to a start index for this part, even though
                // offset is what we want to store later. (Working in terms of offset over
                // index is the better default, because it works with triple quotes that happen
                // to cross chunk boundaries, but in this particular case we're doing masking,
                // so we want to work in terms of index.)
                let start_index = span_start_offset.saturating_sub(chunk_offset) as u8;
                let span_len_in_mask = index.wrapping_sub(start_index);

                // A mask for the span between start_index and index
                let span_mask_zeroes = ((1u64 << span_len_in_mask) - 1) << start_index;
                let span_mask_ones = !span_mask_zeroes;

                // If we had a string in progress, but no longer do, then we just ended a string.
                // Branchlessly apply it to our main bitmask.
                strings |= if prev_in_progress.is_str() {
                    Bitmask::from(span_mask_ones) // 1s only where the string we just made was; 0s elsewhere.
                } else {
                    Bitmask::ZERO // no-op with `|=`
                };

                // If we had a comment in progress, but no longer do, then we just ended a comment.
                // Branchlessly zero out its span in our main bitmask.
                strings &= if prev_in_progress == Comment {
                    Bitmask::from(span_mask_zeroes)
                } else {
                    Bitmask::from(!0u64) // all 1s; if this is AND'd with the existing bitmask, it will be a no-op.
                };

                let end_offset = {
                    // Branchlessly add 2 to the ending offset if this is a multiline string.
                    // (Otherwise it will end on the first of the 3 closing quotes.)
                    let end_offset_extra = if prev_in_progress == MultiLineStr {
                        2
                    } else {
                        0
                    };

                    chunk_offset + index as u32 + end_offset_extra
                };

                let maybe_token =
                    if branchless_and(in_progress == Nothing, prev_in_progress != Nothing) {
                        // This is branchless because in a release build, this is just a mem::transmute()
                        prev_in_progress.into()
                    } else {
                        MaybeToken::NONE
                    };

                // Update the token map.
                self.token_map.set(
                    index,
                    maybe_token,
                    Region {
                        start_offset: span_start_offset,
                        end_offset,
                    },
                );

                // If we had nothing in progress, but now do, then we just started something.
                span_start_offset =
                    if branchless_and(prev_in_progress == Nothing, in_progress != Nothing) {
                        offset
                    } else {
                        span_start_offset
                    };
            }
        }

        self.span_start_offset = span_start_offset;
        self.in_progress = in_progress;

        for (token, region) in self.token_map.iter() {
            listener.emit(token, region.start_offset, region.end_offset);
        }
    }
}

/// NOTE: This is all old and obsolete, but I'm keeping it around as a reference for when it gets rewritten.
/*
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
                            // indices inside mutliline strings, thinking they're structurally relevant when
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
*/

fn branchless_and(a: bool, b: bool) -> bool {
    (a as u8 & b as u8) != 0
}

fn branchless_or(a: bool, b: bool) -> bool {
    (a as u8 | b as u8) != 0
}
