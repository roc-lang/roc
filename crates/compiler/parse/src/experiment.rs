use std::iter::Iterator;

use crate::ast::StrLiteral;
pub struct Env {
    in_progress: InProgress,
    structural_chars: Vec<Bitmask>,
}
enum InProgress {
    StrLiteral,
    Comment,
    Nothing,
}

type Chunk = [u8; 64];

impl Env {
    pub fn handle_chunk(&mut self, chunk: Chunk) {
        let newlines = Bitmask::find(b'\n', chunk);
        let quotes = Bitmask::find(b'"', chunk);
        let pounds = Bitmask::find(b'#', chunk);
        let open_parens = Bitmask::find(b'(', chunk);
        let close_parens = Bitmask::find(b')', chunk);

        // Here are all the interesting structural chars
        self.structural_chars.push(
            newlines
                .or(quotes)
                .or(pounds)
                .or(open_parens)
                .or(close_parens),
        );
    }
}

fn _stage2(structural_chars: &[Bitmask], src: &[u8]) {
    let mut span_start_index = 0;
    let mut prev_index: isize = -1;
    let mut cur_line_indent = 0;
    let mut min_indent = 0;
    let mut just_parsed_empty_str = false;
    let mut prev_char = b'\0';

    enum InProgress {
        SingleLineStr,
        MultiLineStr,
        Comment,
        Nothing,
    }

    let mut in_progress = InProgress::Nothing;
    let mut tape = Vec::new();

    for (index, bitmask) in structural_chars.iter().copied().enumerate() {
        for offset in bitmask.ones() {
            let index = index + offset;

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
                    if char == b'\"' {
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
                }
                InProgress::SingleLineStr => {
                    if char == b'\"' {
                        let str_slice = unsafe {
                            std::str::from_utf8_unchecked(&src[span_start_index..(index - 1)])
                        };
                        let node = Ast::StrLiteral(str_slice.to_string());
                        just_parsed_empty_str = str_slice.is_empty();

                        tape.push(node);

                        in_progress = InProgress::Nothing;
                    } else if char == b'\n' {
                        just_parsed_empty_str = false;
                        eprintln!("String literal hit a newline before it was closed.");
                        in_progress = InProgress::Nothing;
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

                    let is_multiline_str = branchless_and(just_parsed_empty_str, char == b'"');

                    // Much more likely that we aren't exactly about to encounter a multiline str
                    if !is_multiline_str {
                        just_parsed_empty_str = false;

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
                            byte => {
                                unreachable!(
                                "somehow a {:?} was recorded as a structural char, but it isn't",
                                byte as char
                            )
                            }
                        }
                    } else {
                        // span_start_index
                        min_indent = index - span_start_index;
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

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct Bitmask(u64);

impl Bitmask {
    #[inline(always)]
    pub fn find(needle: u8, chunk: Chunk) -> Self {
        // TODO use ~1 simd instruction for this. Just trying to prove out the architecture right now!
        let mut bitmask = 0;

        for (index, byte) in chunk.iter().copied().enumerate() {
            if byte == needle {
                // record a 1 in the bitmask at this index
                if byte == needle {
                    bitmask |= 1 << index;
                }
            }
        }

        Self(bitmask)
    }

    pub fn or(self, other: Self) -> Self {
        Bitmask(self.0 | other.0)
    }

    pub fn ones(&self) -> BitmaskIterator {
        BitmaskIterator {
            mask: self.0,
            index: 0,
        }
    }
}

pub struct BitmaskIterator {
    mask: u64,
    index: usize,
}

impl Iterator for BitmaskIterator {
    type Item = usize;

    fn next(&mut self) -> Option<Self::Item> {
        while self.index < 64 {
            if (self.mask & (1 << self.index)) != 0 {
                let result = self.index;
                self.index += 1;
                return Some(result);
            }
            self.index += 1;
        }
        None
    }
}

fn branchless_and(a: bool, b: bool) -> bool {
    (a as u8 & b as u8) != 0
}
