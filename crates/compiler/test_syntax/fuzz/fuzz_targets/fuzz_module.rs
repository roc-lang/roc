#![no_main]
use bumpalo::Bump;
use libfuzzer_sys::fuzz_target;
use test_syntax::test_helpers::Input;

fuzz_target!(|data: &[u8]| {
    if let Ok(input) = std::str::from_utf8(data) {
        let input = Input::Full(input);
        let arena = Bump::new();
        if input.parse_in(&arena).is_ok() {
            input.check_invariants(|_| (), true, None);
        }
    }
});
