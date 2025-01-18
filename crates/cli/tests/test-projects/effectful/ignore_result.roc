app [main!] { pf: platform "../test-platform-effects-zig/main.roc" }

import pf.Effect

main! : () => {}
main! = ||
    _ = Effect.get_line!()
    Effect.put_line!("I asked for input and I ignored it. Deal with it! 😎")
