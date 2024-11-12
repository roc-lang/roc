app [main!] { pf: platform "../test-platform-effects-zig/main.roc" }

import pf.Effect

Fx : {
    getLine!: {} => Str,
}

main! : {} => {}
main! = \{} ->
    notEffectful : Fx
    notEffectful = {
        getLine!: \{} -> "hardcoded"
    }

    effectful : Fx
    effectful = {
        getLine!: Effect.getLine!
    }

    Effect.putLine! "notEffectful: $(notEffectful.getLine! {})"
    Effect.putLine! "effectful: $(effectful.getLine! {})"
