app [main!] { pf: platform "./platform/main.roc" }

import pf.Host
import pf.Stdout

known_iter : Box(Iter(U8))
known_iter = Box.box([1, 2, 3].iter())

known_stream : Box(Stream(U8))
known_stream = Box.box(Stream.from_iter([4, 5].iter()))

make_captured = |text| {
    advance = |(done, left, middle, right)| {
        if done Err(NoMore) else Ok((
            Str.count_utf8_bytes(left) + Str.count_utf8_bytes(middle) + Str.count_utf8_bytes(right),
            (True, left, middle, right),
        ))
    }
    Box.box(Iter.custom((False, text.concat("a"), text.concat("b"), text.concat("c")), Ok(1), advance))
}

captured : Box(Iter(U64))
captured = make_captured("123456789012345678901234567890")
repacked : Box(Iter(U64))
repacked = Box.box(Box.unbox(captured))
contextual = Box.box(Box.unbox(captured))

main! = || {
    Host.store_seed!(known_iter)
    returned_iter : Box(Iter(U8))
    returned_iter = Host.take_seed!()
    Stdout.line!(Str.inspect(List.from_iter(Box.unbox(returned_iter))))

    Host.store_seed!(known_stream)
    returned_stream : Box(Stream(U8))
    returned_stream = Host.take_seed!()
    Stdout.line!(Str.inspect(Stream.collect!(Box.unbox(returned_stream))))

    Host.store_seed!(repacked)
    returned_captured : Box(Iter(U64))
    returned_captured = Host.take_seed!()
    Stdout.line!(Str.inspect(List.from_iter(Box.unbox(returned_captured))))

    Host.store_seed!(contextual)
    returned_contextual : Box(Iter(U64))
    returned_contextual = Host.take_seed!()
    Stdout.line!(Str.inspect(List.from_iter(Box.unbox(returned_contextual))))
}
