#![no_main]
use bumpalo::Bump;
use libfuzzer_sys::fuzz_target;
use roc_parse::ast::Malformed;
use test_syntax::test_helpers::Input;

fuzz_target!(|data: &[u8]| {
    let canonicalize_fuzz_config = if cfg!(feature = "fuzz_canonicalize") {
        Some(false)
    } else {
        None
    };

    if let Ok(input) = std::str::from_utf8(data) {
        let input = Input::Expr(input);
        let arena = Bump::new();
        let ast = input.parse_in(&arena);
        if let Ok(ast) = ast {
            if !ast.is_malformed() {
                input.check_invariants(|_| (), true, canonicalize_fuzz_config);
            }
        }
    };
});
