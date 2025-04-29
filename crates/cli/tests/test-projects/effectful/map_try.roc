app [main!] { pf: platform "../test-platform-effects-zig/main.roc" }

import pf.Effect

main! : {} => {}
main! = \{} ->
    sanity_check = err_if_zero!(1)
    expect sanity_check == Ok(1)

    good = List.map_try!([1, 2, 3], |num| err_if_zero!(num))
    expect good == Ok([1, 2, 3])

    good_empty = List.map_try!([], |num| err_if_zero!(num))
    expect good_empty == Ok([])

    bad_a = List.map_try!([1, 2, 3, 0], |num| err_if_zero!(num))
    expect bad_a == Err({})

    bad_b = List.map_try!([0, 1, 2, 3], |num| err_if_zero!(num))
    expect bad_b == Err({})

    bad_c = List.map_try!([1, 0, 2, 3], |num| err_if_zero!(num))
    expect bad_c == Err({})

    {}

err_if_zero! : U64 => Result U64 {}
err_if_zero! = \num ->
    if num != 0 then
        Ok(Effect.id_effectful!(num))
    else
        _ = Effect.id_effectful!(num)
        Err({})