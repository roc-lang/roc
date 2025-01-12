app [main!] { pf: platform "../test-platform-effects-zig/main.roc" }

import pf.Effect

main! : () => ()
main! = \() ->
    logged!("hello", \() -> Effect.put_line!("Hello, World!"))

logged! = \name, fx! ->
    Effect.put_line!("Before ${name}")
    fx!()
    Effect.put_line!("After ${name}")
