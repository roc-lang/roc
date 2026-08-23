app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdin
import pf.Stdout

# Lists whose element type is zero-sized still have to track their length
# through reserve, append and concat, and must not strand an allocation on the
# way. The fx host's tracking allocator turns any leak here into a failure.
# Every value is derived from stdin so nothing folds at compile time.

zst : { inner : [Outer({ nested : { one_field : [OneTag({})] } })] }
zst = { inner: Outer({ nested: { one_field: OneTag({}) } }) }

main! = || {
    n = match U64.from_str(Stdin.line!()) {
        Ok(number) => number
        Err(_) => 0
    }

    # with_capacity is the one path that could hand a zero-sized list an
    # allocation; appending and concatenating must not lose track of it.
    reserved = List.with_capacity(4)
    one = List.append(reserved, zst)
    two = List.append(one, zst)
    Stdout.line!("append: ${List.len(two).to_str()}")

    other = if n > 0 { [zst, zst, zst] } else { [] }
    Stdout.line!("literal: ${List.len(other).to_str()}")

    joined = List.concat(two, other)
    Stdout.line!("concat: ${List.len(joined).to_str()}")

    repeated = List.repeat(zst, n)
    Stdout.line!("repeat: ${List.len(repeated).to_str()}")

    match List.first(joined) {
        Ok(_) => Stdout.line!("first: ok")
        Err(ListWasEmpty) => Stdout.line!("first: empty")
    }
}
