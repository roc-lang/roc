app [main!] { pf: platform "./platform/main.roc" }

import pf.Host
import pf.Stdin
import pf.Stdout

main! = || {
    text = Stdin.line!()
    iterator = Box.box(text.iter_utf8())
    Host.store_seed!(iterator)
    retained_bytes = List.from_iter(Box.unbox(iterator))
    returned_iterator : Box(Iter(U8))
    returned_iterator = Host.take_seed!()
    returned_bytes = List.from_iter(Box.unbox(returned_iterator))
    Stdout.line!("iter ${U64.to_str(retained_bytes.len())} ${U64.to_str(returned_bytes.len())}")

    stream = Box.box(Stream.from_iter(text.iter_utf8()))
    Host.store_seed!(stream)
    retained_items = Stream.collect!(Box.unbox(stream))
    returned_stream : Box(Stream(U8))
    returned_stream = Host.take_seed!()
    returned_items = Stream.collect!(Box.unbox(returned_stream))
    Stdout.line!("stream ${U64.to_str(retained_items.len())} ${U64.to_str(returned_items.len())}")
}
