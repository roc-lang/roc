app [main!] { pf: platform "../test-platform-effects-zig/main.roc" }

import pf.Effect

main! : {} => {}
main! = \{} ->
    good = [0, 2, 4] |> List.forEachTry! validate!
    expect good == Ok {}

    bad = [6, 8, 9, 10] |> List.forEachTry! validate!
    expect bad == Err 9

    {}

validate! : U32 => Result {} U32
validate! = \x ->
    if Num.isEven x then
        Effect.putLine! "✅ $(Num.toStr x)"
        Ok {}

    else
        Effect.putLine! "$(Num.toStr x) is not even! ABORT!"
        Err x
