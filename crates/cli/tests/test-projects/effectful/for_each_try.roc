app [main!] { pf: platform "../test-platform-effects-zig/main.roc" }

import pf.Effect

main! : () => {}
main! = ||
    good = [0, 2, 4] |> List.for_each_try!(validate!)
    expect good == Ok({})

    bad = [6, 8, 9, 10] |> List.for_each_try!(validate!)
    expect bad == Err(9)

    {}

validate! : U32 => Result {} U32
validate! = \x ->
    if Num.is_even(x) then
        Effect.put_line!("✅ ${Num.to_str(x)}")
        Ok({})
    else
        Effect.put_line!("${Num.to_str(x)} is not even! ABORT!")
        Err(x)
