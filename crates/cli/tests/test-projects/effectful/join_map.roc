app [main!] { pf: platform "../test-platform-effects-zig/main.roc" }

import pf.Effect

main! : {} => {}
main! = \{} ->
    flattened = List.join_map!([1, 2, 3], duplicate!)
    expect flattened == Ok([1, 1, 2, 2, 3, 3])

    with_zero = List.join_map!([0, 2], duplicate!)
    expect with_zero == Ok([0, 0, 2, 2])

    empty = List.join_map!([], duplicate!)
    expect empty == Ok([])

    {}

duplicate! : U64 => Result (List U64) _
duplicate! = \num ->
    value = Effect.id_effectful!(num)
    Ok([value, value])
