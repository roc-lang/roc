use crate::bitmask::{Bitmask, Chunk};
use crate::token::{MaybeToken, Region, Token, TokenMap};

/// This is pretty big on the stack because of TokenMap; callers should consider using a Box<Env>.
#[derive(Default)]
pub struct Env {
    in_progress: InProgress,
    /// The byte at the end of the previous chunk
    prev_byte: u8,
    /// The number of trailing '"' characters at the end of the previous chunk, modulo 3.
    /// (So, this number will always be 0, 1, or 2.) This is because we only care about these
    /// for purposes of determining whether the current chunk begins with a triple quote or not.
    trailing_quote_chars: u8,
    /// The number of trailing '\\' characters at the end of the previous chunk(s), modulo 2.
    /// (So, this number will always be 0 or 1.) This is because double backslash always means
    /// either an escaped backalash or an error, so we can handle those immediately.
    trailing_backslash_chars: u8,
    /// The number of trailing ' ' characters at the end of the previous chunk(s),
    /// which are all preceded by a newline character. (So, the indentation is
    /// crossing a chunk boundary.) Note that multiple entire chunks can be all spaces.
    trailing_indent_spaces: u32,
    /// The current chunk's offset relative to the start of the src.
    /// This must be specified in terms of the chunk's offset (that is,
    /// its offset from the beginning of the source) rather than the index into
    /// the chunk. This is because sometimes it can start earlier than the first
    /// byte of the chunk, for example if the previous chunk ended in quotes, which
    /// turned out to be the beginning of a multiline string.
    chunk_offset: u32,
    span_start_offset: u32,
    current_indent: u32,
    /// This is the fallback ("slow path") which we only use if we encounter an indent of 5+ spaces at once.
    /// (If all indents are exactly 4 spaces, which `roc format` enforces, then we don't need to use this stack.)
    indent_stack: Vec<u32>,
}

impl From<InProgress> for MaybeToken {
    #[inline(always)]
    fn from(in_progress: InProgress) -> Self {
        debug_assert_eq!(
            match in_progress {
                InProgress::Nothing => 0,
                InProgress::Comment => Token::Comment as u8,
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
    Comment = 2,
    // The strings must have the highest numeric values, for `is_str` to work
    MultiLineStr = 253, // This must have the lowest numeric value of the strings, for `is_str` to work
    SingleLineStr = 254,
    SingleQuoteChar = 255, // We consider single-quote character literals to be "strings"
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

impl Env {
    pub fn handle_chunk(&mut self, chunk: Chunk) -> TokenMap {
        let spaces = Bitmask::for_byte(chunk, b' ');
        let is_all_spaces = spaces == Bitmask::from(!0);

        // These conditionals are written with the most likely branch first, to avoid mispredictions
        if !is_all_spaces {
            // TODO check for \t and give errors for them.
            self.handle_nontrivial_chunk(chunk, spaces)
        } else {
            // We have encountered the extremely unlikely event that our entire chunk is all spaces,
            // so we may need to extend the trailing_indent_spaces count from the previous chunk instead
            // of resetting it like normal.
            if self.trailing_indent_spaces > 0 {
                // Continue the trailing_indent_spaces from the previous chunk.
                self.trailing_indent_spaces += 64;
            } else if self.prev_byte == b'\n' {
                // This all-spaces chunk is immediately preceded by a newline, so it's all trailing_indent_spaces.
                self.trailing_indent_spaces = 64;
            }

            // We're all spaces, so we definitely know what prev_byte is.
            self.prev_byte = b' ';

            // Spaces are only ever included in a TokenMap as indentations, and that
            // possibility will be handled later by trailing_indent_spaces checks.
            TokenMap::default()
        }
    }

    #[inline(always)]
    /// Handle a chunk that has already been verified to be nontrivial (e.g. it's not all spaces,
    /// which is a special case we handled already with a branch.)
    fn handle_nontrivial_chunk(&mut self, chunk: Chunk, mut spaces: Bitmask) -> TokenMap {
        // In case someone is editing on Windows and has CRLF line endings, we treat
        // carriage returns (\r) as ending a sequence of spaces (for purposes of indentation
        // checking), just like we do for \n characters.
        let carriage_returns = Bitmask::for_byte(chunk, b'\r');
        let newlines = Bitmask::for_byte(chunk, b'\n');
        let newlines_or_carriage_rets = newlines | carriage_returns;
        // Add to the bitmask one space per sequence of spaces following a newline.
        let spaces_after_newline = {
            // Start of each contiguous sequence of spaces
            //
            //  input str: xxx   xxxxxxxxxxxxxxxx    xxxxxxxxxxxxxxxx xxxxxxxxxxxx     xxxx
            // input bits: 0001110000000000000000111100000000000000001000000000000111110000
            //    shifted: 0000111000000000000000011110000000000000000100000000000011111000
            //    negated: 1111000111111111111111100001111111111111111011111111111100000111
            //      anded: 0001000000000000000000100000000000000000001000000000000100000000
            //  input str: xxx   xxxxxxxxxxxxxxxx    xxxxxxxxxxxxxxxx xxxxxxxxxxxx     xxxx
            let starts = spaces & !(spaces >> 1);
            let spaces_after_newline = starts & (newlines >> 1);

            // If we have a sequence of spaces that follows a newline and also runs into the end of the chunk,
            // we need to handle that in the next chunk instead of in this one - because there might be even
            // more spaces in this sequence!
            let spaces_after_newline = {
                let trailing_spaces = spaces.trailing_ones();
                let index_of_potential_newline = 63 - trailing_spaces;
                let is_blank_line = newlines_or_carriage_rets.is_1_at(index_of_potential_newline);
                let trailing_indent_spaces = if is_blank_line {
                    // the spaces are on a blank line; disregard them!
                    0
                } else {
                    trailing_spaces
                };

                // We already handled the case where this chunk is all spaces, so we definitely want to overwrite this here.
                self.trailing_indent_spaces = trailing_indent_spaces;

                let trailing_indent_spaces = trailing_indent_spaces as u8;

                // Remove trailing spaces from `spaces` as well, so they aren't counted as indents later.
                // (If this is a blank line, trailing_indent_spaces will be 0 and this will be a no-op.)
                spaces = (spaces << trailing_indent_spaces) >> trailing_indent_spaces;

                // Remove the trailing spaces from consideration in the `spaces_after_newline` mask,
                // since they'll be handled with the next chunk rather than with this one.
                (spaces_after_newline >> trailing_indent_spaces) << trailing_indent_spaces
            };

            // If the previous chunk ended in a newline, and this one starts with some spaces
            // but doesn't end in a carriage return or newline (which would mean a blank line,
            // which we disregard), then set the first bit in our spaces_after_newline bitmask,
            // because there is a sequence of spaces there that we care about!
            let leading_spaces = spaces.leading_ones();

            debug_assert!(leading_spaces < 64);
            // For leadings_spaces to be 64 or more, the chunk would have to be all spaces -
            // and this function doesn't even get called in that special case, because
            // we branched to handle it differently earlier.

            // See if there's a newline or carriage return at index (1 + index of last space)
            // in the bitmask. This will never overflow, because leading_spaces is always less than 64.
            let is_blank_line = branchless_or(
                newlines.is_1_at(leading_spaces),
                carriage_returns.is_1_at(leading_spaces),
            );
            let begins_with_spaces = branchless_and(
                self.prev_byte == b'\n',
                branchless_and(!is_blank_line, leading_spaces != 0),
            ) as u64;

            spaces_after_newline | Bitmask::from(begins_with_spaces)
        };

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

        let all_single_quotes = Bitmask::for_byte(chunk, b'\'');
        // Bitmask of all the single quotes (`'`) in the chunk, excluding escaped ones (`\'`, `\\\'`, etc.).
        let unescaped_single_quotes = all_single_quotes & preceded_by_escape;

        let all_quotes = Bitmask::for_byte(chunk, b'"');
        // Bitmask of all the double quotes (`"`) in the chunk, excluding escaped ones (`\"`, `\\\"`, etc.).
        let unescaped_quotes = {
            let quotes = all_quotes & preceded_by_escape;

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
        let unescaped_triple_quotes =
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
            unescaped_quotes & (unescaped_quotes << 1) & (unescaped_quotes << 2);

        self.strings_comments_indents(
            chunk,
            spaces,
            newlines,
            newlines_or_carriage_rets,
            backslashes,
            unescaped_quotes,
            unescaped_single_quotes,
            unescaped_triple_quotes,
            spaces_after_newline,
        )
    }

    #[inline(always)]
    /// Given the masks for the locations of quotes, single quotes, and triple quotes (which have already
    /// had backslash-escaped versions of themselves removed), compute the ranges for strings, comments, and indents/outdents.
    fn strings_comments_indents(
        &mut self,
        chunk: Chunk,
        spaces: Bitmask,
        newlines: Bitmask,
        newlines_or_carriage_rets: Bitmask,
        backslashes: Bitmask,
        unescaped_quotes: Bitmask,
        unescaped_single_quotes: Bitmask,
        unescaped_triple_quotes: Bitmask,
        spaces_after_newline: Bitmask,
    ) -> TokenMap {
        use InProgress::{Comment, MultiLineStr, Nothing, SingleLineStr, SingleQuoteChar};
        let open_parens = Bitmask::for_byte(chunk, b'(');
        let close_parens = Bitmask::for_byte(chunk, b')');

        let chunk_offset = self.chunk_offset;
        let mut token_map = TokenMap::default();
        let mut span_start_offset = self.span_start_offset;
        let mut in_progress;

        // We do a lot of signed arithmetic on this, so store it as i32 for the duration of this function.
        let mut current_indent = self.current_indent as i32;

        // TODO make this whole `match` branchless. It's definitely doable!
        match self.trailing_quote_chars {
            1 => {
                span_start_offset = chunk_offset - 1;

                // 1 trailing quote means we need to figure out if we're working on
                // a single-line string vs a multiline string. That in turn depends
                // on whether our first two bytes are both quotes. If they both are,
                // this must be a triple quote!
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
                    let starting_multiline_str = self.in_progress == Nothing;

                    in_progress = if starting_multiline_str {
                        MultiLineStr
                    } else {
                        // Since in_progress wasn't Nothing, it must have been MultiLineStr
                        // (or else an earlier step has a bug, because if there were 2 quotes
                        // that were considered trailing, the only two in_progress values that
                        // should be possible are MultiLineStr and Nothing), so therefore
                        // this must be a closing triple-quote. That means we end the MultiLineStr.
                        debug_assert_eq!(self.in_progress, MultiLineStr, "Previous chunk ended in 2 quotes, yet its in_progress was neither MutliLineStr nor Nothing. This should never happen!");

                        Nothing
                    };

                    // If we just finished a MultiLineStr, then we should insert a token for it.
                    // (We are either starting a new one or finishing the previous one.)
                    let maybe_token = if starting_multiline_str {
                        MaybeToken::none()
                    } else {
                        MaybeToken::some(Token::MultiLineStr)
                    };

                    // Record the MultiLineStr (or Nothing), between the start offset and the offset of the final quote.
                    token_map.set(
                        0,
                        maybe_token,
                        Region {
                            start_offset: span_start_offset,
                            end_offset: chunk_offset + 1,
                        },
                    );

                    // If we will be starting a multiline string, it should start 2 bytes
                    // before the beginning of this chunk. (This is all done branchlessly.)
                    //
                    // Importantly, this must be set *after* updating in_progress, because
                    // the in_progress update logic may need to use the old span_start_offset.
                    span_start_offset = chunk_offset - if starting_multiline_str { 2 } else { 0 };
                } else {
                    // 2 trailing quotes followed by a non-quote means
                    // we had an empty string at the end of the previous chunk.
                    token_map.insert(
                        0,
                        Token::SingleLineStr,
                        Region {
                            start_offset: chunk_offset.wrapping_sub(2),
                            end_offset: chunk_offset.wrapping_sub(1),
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

        let mask_contains_single_or_triple_quotes = branchless_or(
            unescaped_single_quotes.is_nonzero(),
            unescaped_triple_quotes.is_nonzero(),
        );
        let pounds = Bitmask::for_byte(chunk, b'#');
        // Note: this includes multiline strings, single-line strings, AND single-quoted chars!
        let mut strings = Bitmask::from(0);
        // This excludes parens at first, but may end up having close-parens injected when handling string interpolation,
        // to mark the end of the interpolation.
        let mut loop_mask = Bitmask::from(
            backslashes | unescaped_quotes | pounds | newlines | spaces_after_newline,
        );

        while !loop_mask.is_zero() {
            let index = loop_mask.trailing_zeros() as u8;
            let offset = index as u32 + chunk_offset;

            loop_mask &= loop_mask.wrapping_sub(1); // clear the bit we just encountered

            // This is safe because this index is from a u64 bitmask, and Chunk has length 64
            let byte = unsafe { *chunk.get_unchecked(index as usize) };

            if byte == b'\\' {
                if in_progress.is_str() {
                    todo!("interpolation or escape! (Remember, could be an escaped backslash)");
                } else {
                    // If we encounter a backslash outside a string, it's a lambda.
                    token_map.insert(
                        index,
                        Token::Lambda,
                        Region {
                            start_offset: offset,
                            end_offset: offset,
                        },
                    );

                    // TODO if we end up wanting to continue in the other branch too,
                    // maybe put everything else in `else` instead?
                    continue;
                }
            }

            let prev_in_progress = in_progress;
            let byte_is_quote = byte == b'"';
            let byte_is_pound = byte == b'#';

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
                let byte_is_triple_quote =
                    (unescaped_triple_quotes & (1 << index).into()).is_nonzero();
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

                // This may be an indent or outdent if our indentation level changed.
                // None of this affects in_progress.
                let token_if_is_space = {
                    // However many spaces we encounter here (which will include the current byte, if it's a space)
                    // represents the new indentation level of this line (unless this is a blank line).
                    // This is because if there are any spaces here at all (e.g. byte == ' '),
                    // they must have been immediately following a newline, because
                    // the mask we're looping over includes `spaces_after_newline` but not `spaces`.
                    let indent_spaces = (spaces << index).leading_ones();

                    // Check for a newline or carriage return at (index + 1) where index
                    // is the last index of a space in this sequence. This will never overflow,
                    // because if the spaces run into the end of the chunk, then they would already
                    // have been moved to `trailing_spaces` and removed from `spaces`.
                    let is_blank_line = newlines_or_carriage_rets.is_1_at(indent_spaces);

                    let indent_delta = indent_spaces as i32 - current_indent;
                    let mut indent_delta = if is_blank_line { 0 } else { indent_delta };

                    // if indent_stack is empty, we're on the fast path; that means
                    // all indentations have been single indents that are multiples of 4.
                    // To continue on the fast path, this must be either an indent of exactly 4 spaces,
                    // or else an outdent that's a multiple of 4.
                    let is_4_space_indent = indent_delta == 4;
                    let is_outdent_multiple_of_4 =
                        branchless_and(indent_delta < 0, indent_delta % 4 == 0);
                    let is_fast_path_eligible = branchless_and(
                        self.indent_stack.is_empty(),
                        branchless_or(is_4_space_indent, is_outdent_multiple_of_4),
                    );

                    // It should normally be the case that we're on the fast path,
                    // because `roc format` puts us on that path every time.
                    if is_fast_path_eligible {
                        // For multiline strings, any amount of outdent is an error. We only want
                        // to record one error per outdent!
                        let needs_another_outdent =
                            branchless_and(indent_delta < -4, in_progress != MultiLineStr);
                        let next_outdent_index = index + 4;

                        // If we need another outdent, this bitmask has a 1 at the next outdent's location.
                        let next_outdent_mask =
                            Bitmask::from((needs_another_outdent as u64) << next_outdent_index);

                        // Do some extra assertions in debug builds
                        #[cfg(debug_assertions)]
                        if needs_another_outdent {
                            // If we are on the fast path, we know the outdent is a multiple of 4, and also
                            // that we detected it inside the current chunk. So there must be at least 4 more
                            // slots available!
                            debug_assert!(next_outdent_index < 64);

                            // The next outdent should currently be a zero in loop_mask.
                            debug_assert_eq!(loop_mask & next_outdent_mask, Bitmask::ZERO);

                            // It should also be a space in the chunk.
                            debug_assert_eq!(chunk[next_outdent_index as usize], b' ');
                        }

                        // Set a 1 for loop_mask at the location of the next outdent, so next time
                        // we come through the loop, we process it too (and discover it's an oudent,
                        // and possibly repeat this process if necessary. (next_outdent_mask will be 0
                        // if we aren't outdenting, so this will become a no-op.)
                        loop_mask |= next_outdent_mask;

                        current_indent += if is_blank_line { 0 } else { indent_delta };
                        debug_assert!(current_indent >= 0);

                        // In multiline strings, an "indent" just means this particular
                        // line of the string began with some spaces. Don't record a token for that.
                        let maybe_indent_token = if in_progress == MultiLineStr {
                            MaybeToken::none()
                        } else {
                            MaybeToken::some(Token::Indent)
                        };

                        // In multiline strings, an "outdent" is an error. Each line of a multiline string
                        // must have the same indentation level as the opening quotes!
                        let maybe_outdent_token = if in_progress == MultiLineStr {
                            MaybeToken::some(Token::ErrOutdentInMultilineStr)
                        } else {
                            MaybeToken::some(Token::Outdent)
                        };

                        if is_4_space_indent {
                            maybe_indent_token
                        } else {
                            // If we need multiple outdents, we've already handled that scenario.
                            // The next time we come through the loop, we will repeat this path!
                            maybe_outdent_token
                        }
                    } else {
                        todo!("slow path with indent_stack, loop to emit multiple outdents")
                    }
                };

                let maybe_token = if byte == b' ' {
                    token_if_is_space
                } else {
                    maybe_token
                };

                // Write to the token map.
                token_map.set(
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
        self.prev_byte = chunk[63];
        self.current_indent = current_indent as u32;

        token_map
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
