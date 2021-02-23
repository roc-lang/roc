use crate::editor::colors as ed_colors;
use ed_colors::SyntaxHighlightTheme;
use crate::graphics::colors as gr_colors;
use crate::graphics::primitives;
use gr_colors::RgbaTup;

//TODO optimize memory allocation
//TODO this is a demo function, the AST should be used for highlighting, see #904.
pub fn highlight_code(
    code_text: &primitives::text::Text,
    all_text_tups: &mut Vec<(String, RgbaTup)>,
    syntax_theme: &SyntaxHighlightTheme,
) {
    let split_code = split_inclusive(&code_text.text);

    let mut active_color = gr_colors::WHITE;
    let mut same_type_str = String::new();

    for token_seq in split_code {
        let new_word_color = if token_seq.contains(&'\"'.to_string()) {
            syntax_theme.string
        } else if token_seq.contains(&'='.to_string()) {
            syntax_theme.operator
        } else {
            syntax_theme.code
        };

        if new_word_color != active_color {
            all_text_tups.push((same_type_str, active_color));

            active_color = new_word_color;
            same_type_str = String::new();
        }

        same_type_str.push_str(&token_seq);
    }

    if !same_type_str.is_empty() {
        all_text_tups.push((same_type_str, active_color));
    }
}

//TODO use rust's split_inclusive once rust 1.50 is released
fn split_inclusive(code_str: &str) -> Vec<String> {
    let mut split_vec: Vec<String> = Vec::new();
    let mut temp_str = String::new();
    let mut non_space_encountered = false;

    for token in code_str.chars() {
        if token != ' ' && token != '\n' {
            non_space_encountered = true;
            temp_str.push(token);
        } else if non_space_encountered {
            split_vec.push(temp_str);
            temp_str = String::new();
            temp_str.push(token);
            non_space_encountered = false;
        } else {
            temp_str.push(token);
        }
    }

    if !temp_str.is_empty() {
        split_vec.push(temp_str);
    }

    split_vec
}
