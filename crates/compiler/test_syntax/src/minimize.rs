//! Generate a minimized version of a given input, by removing parts of it.
//! This is useful for debugging, when you have a large input that causes a failure,
//! and you want to find the smallest input that still causes the failure.
//!
//! Most users will want to use the binary instead of this module directly.
//! e.g. `cargo run --release --bin minimize -- full <file_that_triggers_parsing_bug>`

use crate::test_helpers::{Input, InputKind};
use bumpalo::Bump;
use roc_fmt::MigrationFlags;
use roc_parse::{ast::Malformed, normalize::Normalize};

#[derive(Copy, Clone, Debug)]
pub struct Options {
    pub kind: InputKind,
    pub minimize_full_error: bool,
    pub minimize_initial_parse_error: bool,
}

pub fn print_minimizations(text: &str, options: Options) -> bool {
    let Some(original_error) = round_trip_once_and_extract_error(text, options) else {
        eprintln!("No error found");
        return false;
    };

    eprintln!("Error found: {}", original_error);
    eprintln!("Proceeding with minimization");

    let mut s = text.to_string();

    loop {
        let mut found = false;
        for update in candidate_minimizations(s.clone()) {
            let Some(new_s) = make_replacements(&s, &update) else {
                continue;
            };

            assert!(
                new_s.len() < s.len(),
                "replacements: {:?}",
                update.replacements
            );

            if let Some(result) = round_trip_once_and_extract_error(&new_s, options) {
                if result == original_error {
                    eprintln!("Successfully minimized, new length: {}", new_s.len());
                    s = new_s;
                    found = true;
                    break;
                }
            }
        }

        if !found {
            eprintln!("No more minimizations found");
            break;
        }
    }

    eprintln!("Final result:");
    println!("{}", s);

    true
}

fn make_replacements(s: &str, update: &Update) -> Option<String> {
    let mut new_s = String::with_capacity(s.len());
    let mut offset = 0;
    for (start, end, replacement) in update.replacements.clone() {
        // check that start and end are not in the middle of a utf-8 character
        if !s.is_char_boundary(start) || !s.is_char_boundary(end) {
            return None;
        }
        new_s.push_str(&s[offset..start]);
        new_s.push_str(&replacement);
        offset = end;
    }
    new_s.push_str(&s[offset..]);

    Some(new_s)
}

fn round_trip_once_and_extract_error(text: &str, options: Options) -> Option<String> {
    let input = options.kind.with_text(text);
    let res = std::panic::catch_unwind(|| round_trip_once(input, options));

    match res {
        Ok(res) => res,
        Err(e) => {
            if let Some(s) = e.downcast_ref::<&'static str>() {
                return Some(s.to_string());
            }
            if let Some(s) = e.downcast_ref::<String>() {
                return Some(s.clone());
            }
            Some("Panic during parsing".to_string())
        }
    }
}

fn round_trip_once(input: Input<'_>, options: Options) -> Option<String> {
    let arena = Bump::new();

    let actual = match input.parse_in(&arena) {
        Ok(a) => a,
        Err(e) => {
            if options.minimize_initial_parse_error {
                return Some(format!("Initial parse failed: {:?}", e.normalize(&arena)));
            } else {
                return None;
            }
        }
    };

    if actual.is_malformed() {
        if options.minimize_initial_parse_error {
            return Some("Initial parse is malformed".to_string());
        } else {
            return None;
        }
    }

    let flags = MigrationFlags {
        snakify: false,
        parens_and_commas: false,
    };

    let output = actual.format(flags);

    let reparsed_ast = match output.as_ref().parse_in(&arena) {
        Ok(r) => r,
        Err(e) => {
            if options.minimize_full_error {
                return Some(format!("Reparse failed: {:?}", e.normalize(&arena)));
            } else {
                return Some("Reparse failed".to_string());
            }
        }
    };

    let ast_normalized = actual.normalize(&arena);
    let reparsed_ast_normalized = reparsed_ast.normalize(&arena);

    if format!("{ast_normalized:?}") != format!("{reparsed_ast_normalized:?}") {
        return Some("Different ast".to_string());
    }

    let reformatted = reparsed_ast.format(flags);

    if output != reformatted {
        return Some("Formatting not stable".to_string());
    }

    None
}

struct Update {
    replacements: Vec<(usize, usize, String)>,
}

fn candidate_minimizations(s: String) -> Box<dyn Iterator<Item = Update>> {
    let mut line_offsets = vec![0];
    line_offsets.extend(s.match_indices('\n').map(|(i, _)| i + 1));
    let line_count = line_offsets.len();
    let s_len = s.len();

    let line_indents = line_offsets
        .iter()
        .map(|&offset| s[offset..].chars().take_while(|&c| c == ' ').count())
        .collect::<Vec<_>>();

    let line_offsets_clone = line_offsets.clone();

    // first, try to remove every group of 1, 2, 3, ... lines - in reverse order (so, trying removing n lines first, then n-1, etc)
    let line_removals = (1..=line_count).rev().flat_map(move |n| {
        let line_offsets_clone = line_offsets.clone();
        (0..line_count - n).map(move |start| {
            let end = start + n;
            let start_offset = line_offsets_clone[start];
            let end_offset = line_offsets_clone[end];
            let replacement = String::new();
            let replacements = vec![(start_offset, end_offset, replacement)];
            Update { replacements }
        })
    });

    let line_offsets = line_offsets_clone;
    let line_offsets_clone = line_offsets.clone();

    // then, try to dedent every group of 1, 2, 3, ... lines - in reverse order (so, trying dedenting n lines first, then n-1, etc)
    // just remove one space at a time, for now
    let line_dedents = (1..=line_count).rev().flat_map(move |n| {
        let line_offsets_clone = line_offsets.clone();
        let line_indents_clone = line_indents.clone();
        (0..line_count - n).filter_map(move |start| {
            // first check if all lines are either zero-width or have greater than zero indent
            let end = start + n;
            for i in start..end {
                if line_indents_clone[i] == 0
                    && line_offsets_clone[i] + 1
                        < line_offsets_clone.get(i + 1).cloned().unwrap_or(s_len)
                {
                    return None;
                }
            }

            let mut replacements = vec![];
            for i in start..end {
                let offset = line_offsets_clone[i];
                let indent = line_indents_clone[i];
                if indent > 0 {
                    replacements.push((offset, offset + 1, String::new()));
                }
            }
            Some(Update { replacements })
        })
    });

    // then, try to select every range of 1, 2, 3, ... lines - in normal order this time!
    // we remove the lines before and after the range
    let line_selects = (1..line_count - 1).flat_map(move |n| {
        assert!(n > 0);
        let line_offsets_clone = line_offsets_clone.clone();
        (0..line_count - n).map(move |start| {
            let end = start + n;
            let start_offset = line_offsets_clone[start];
            let end_offset = line_offsets_clone[end];
            assert!(end_offset > start_offset);
            assert!(start_offset > 0 || end_offset < s_len);
            let replacements = vec![
                (0, start_offset, String::new()),
                (end_offset, s_len, String::new()),
            ];
            Update { replacements }
        })
    });

    // then, try to remove every range of 1, 2, 3, ... characters - in reverse order (so, trying removing n characters first, then n-1, etc)
    let charseq_removals = (1..s.len()).rev().flat_map(move |n| {
        (0..s.len() - n).map(move |start| {
            let end = start + n;
            let replacement = String::new();
            let replacements = vec![(start, end, replacement)];
            Update { replacements }
        })
    });

    Box::new(
        line_removals
            .chain(line_dedents)
            .chain(line_selects)
            .chain(charseq_removals)
            .filter(|u| !u.replacements.is_empty()),
    )
}
