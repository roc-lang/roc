app [main!] { pf: platform "../test-platform-effects-zig/main.roc" }

import pf.Effect

Fx : {
    get_line! : {} => Str,
}

main! : {} => {}
main! = \{} ->
    not_effectful : Fx
    not_effectful = {
        get_line!: \{} -> "hardcoded",
    }

    effectful : Fx
    effectful = {
        get_line!: Effect.get_line!,
    }

    Effect.put_line!("not_effectful: ${not_effectful.get_line!({})}")
    Effect.put_line!("effectful: ${effectful.get_line!({})}")
