#![no_main]
use bumpalo::Bump;
use libfuzzer_sys::fuzz_target;
use roc_parse::test_helpers::parse_defs_with;

fuzz_target!(|data: &[u8]| {
    if let Ok(input) = std::str::from_utf8(data) {
        let arena = Bump::new();
        let _actual = parse_defs_with(&arena, input.trim());
    }
});
