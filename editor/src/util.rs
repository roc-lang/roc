pub fn is_newline(char_ref: &char) -> bool {
    let newline_codes = vec!['\u{d}'];

    newline_codes.contains(char_ref)
}
