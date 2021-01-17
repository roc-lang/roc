pub fn is_newline(char_ref: &char) -> bool {
    let newline_codes = vec!['\u{d}', '\n'];

    newline_codes.contains(char_ref)
}
