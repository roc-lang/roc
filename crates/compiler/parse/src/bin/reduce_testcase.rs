use bumpalo::Bump;
use roc_parse::test_helpers::parse_expr_with;

fn main() {
    let arg = std::env::args().nth(1).unwrap();

    let mut contents = std::fs::read_to_string(&arg).unwrap();

    std::panic::set_hook(Box::new(|_| {
        println!("<panic>");
    }));

    let signature = isolated_parse(&contents);
    println!("signature: {}", signature);

    let mut made_progress = true;
    while made_progress {
        made_progress = false;
        for reduction in reductions(&contents) {
            let new_sig = isolated_parse(&reduction);
            if new_sig == signature {
                contents = reduction;
                made_progress = true;
                break;
            }
        }
    }

    println!("reduced: {}", contents);
    std::fs::write(arg, &contents).unwrap();
}

fn reductions(text: &str) -> impl Iterator<Item = String> {
    let text0 = text.to_string();
    let remove_chars = (0..text.len()).map(move |i| {
        let mut new_text = text0.clone();
        new_text.remove(i);
        new_text
    });

    let text0 = text.to_string();
    let replace_control_chars_with_space = (0..text0.len()).filter_map(move |i| {
        if text0.as_bytes()[i] < b' ' && text0.as_bytes()[i] != b'\n' {
            let mut new_text = text0.clone();
            new_text.replace_range(i..i + 1, " ");
            Some(new_text)
        } else {
            None
        }
    });

    remove_chars.chain(replace_control_chars_with_space)
}

fn isolated_parse(input: &str) -> String {
    let res = std::panic::catch_unwind(|| {
        let arena = Bump::new();
        let _actual = parse_expr_with(&arena, input.trim());
    });

    // TODO: also capture some aspects of the panic or error
    // e.g. what the sequence of nested errors was, or
    // what the panic message was (allowing for some fuzziness)
    match res {
        Ok(_) => "ok".to_string(),
        Err(_) => "panic".to_string(),
    }
}
