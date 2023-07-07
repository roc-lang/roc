use std::iter::Iterator;

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
