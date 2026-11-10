app [main!] { pf: platform "./platform/main.roc" }

import pf.Host
import pf.Stdin
import pf.Stdout

main! = || {
    Host.reset_boxed_drop_report!()
    text = Stdin.line!()
    callback = Host.boxed_add!(1)
    iterator = Box.box(text.iter_utf8().map(|byte| {
        f = Box.unbox(callback)
        if f(0) > 0 byte else 0
    }))
    Host.release_boxed_iter!(iterator)

    stream_callback = Host.boxed_add!(2)
    stream = Box.box(Stream.from_iter(text.iter_utf8().map(|byte| {
        f = Box.unbox(stream_callback)
        if f(0) > 0 byte else 0
    })))
    Host.release_boxed_stream!(stream)
    Stdout.line!(Host.boxed_drop_report!())
}
