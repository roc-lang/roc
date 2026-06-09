app [main!] { pf: platform "./platform/main.roc" }
import pf.Plant exposing [Plant]

Stream(item) := {
    len_if_known : [Known(U64), Unknown],
    step! : () => [One({ item : item, rest : Stream(item) }), Skip({ rest : Stream(item) }), Done],
}.{
    # Bridge a pure Iter into a Stream, applying an effectful transform lazily.
    from_iter_map! : Iter(a), (a => b) => Stream(b)
    from_iter_map! = |iterator, transform!|
        {
            len_if_known: Unknown,
            step!: || match Iter.next(iterator) {
                Done => Done
                Skip({ rest }) => Skip({ rest: Stream.from_iter_map!(rest, transform!) })
                One({ item, rest }) => One({ item: transform!(item), rest: Stream.from_iter_map!(rest, transform!) })
            },
        }

    map! : Stream(a), (a => b) => Stream(b)
    map! = |stream, transform!|
        match stream {
            { len_if_known, step! } => {
                len_if_known,
                step!: || match step!() {
                    Done => Done
                    Skip({ rest }) => Skip({ rest: Stream.map!(rest, transform!) })
                    One({ item, rest }) => One({ item: transform!(item), rest: Stream.map!(rest, transform!) })
                },
            }
        }

    next! : Stream(item) => [One({ item : item, rest : Stream(item) }), Skip({ rest : Stream(item) }), Done]
    next! = |stream| match stream { { step!, .. } => step!() }

    collect! : Stream(item) => List(item)
    collect! = |stream| {
        cap = match stream { { len_if_known, .. } => match len_if_known { Known(n) => n, Unknown => 0 } }
        Stream.drain!(stream, List.with_capacity(cap))
    }

    drain! : Stream(item), List(item) => List(item)
    drain! = |stream, acc|
        match Stream.next!(stream) {
            Done => acc
            Skip({ rest }) => Stream.drain!(rest, acc)
            One({ item, rest }) => Stream.drain!(rest, acc.append(item))
        }
}

starting_plants! : () => List(Plant)
starting_plants! = || {
    lo : I32
    lo = 0
    hi : I32
    hi = 14
    Stream.from_iter_map!(lo.to(hi), |i| Plant.random!(i * 12)).collect!()
}

main! : () => List(Plant)
main! = || starting_plants!()
