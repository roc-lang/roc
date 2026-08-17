app [main!] { pf: platform "./main.roc" }

import pf.CliHost

# A string long enough to need the heap, built from host-supplied input so it
# cannot be folded into static data. Static data has a constant refcount, which
# no release can move, so it would hide every ownership mistake below.
heap_label : Str, Str -> Str
heap_label = |seed, tag| Str.repeat(Str.concat(seed, tag), 3)

main! : List(Str) => Try({}, [Exit(I32)])
main! = |args| {
    first_arg = match args.get(0) {
        Ok(arg) => arg
        Err(_) => ""
    }
    input = CliHost.read!({})
    CliHost.log!("roc saw ${input} argc=${args.len().to_str()} first=${first_arg}")

    # `dying` has no use after the call, so the host holds the only reference
    # and its release must drop the element strings.
    dying = [heap_label(input, "a"), heap_label(input, "b")]
    dying_sum = CliHost.checksum!(dying)

    # `kept` is handed over twice, so Roc retains it across the first transfer.
    # The first release must leave the element strings alone—dropping them
    # while Roc still holds the list frees them twice—and the second, which
    # takes the count to zero, must drop them. The second checksum reads the
    # element bytes again, so a first release that freed them reads back freed
    # memory and disagrees.
    kept = [heap_label(input, "c"), heap_label(input, "d")]
    first_sum = CliHost.checksum!(kept)
    second_sum = CliHost.checksum!(kept)

    if dying_sum > 0 and first_sum > 0 and first_sum == second_sum {
        Ok({})
    } else {
        Err(Exit(1))
    }
}
