# A nominal record's callable field carries different lambda-set
# specializations behind one public type: `EchoFrame.default()` sees only the
# capture-free default handler while `register` builds the `plus` closure,
# whose captures make the handler's lambda set recursive. This shape drives
# equivalent-named layout selection for callable-bearing nominal records and
# ABI-borrow liveness across the dispatch loop's ARC join summaries; larger
# programs with this shape (puri's Frame/Handler) hit both when layout reuse
# keyed on public instead of representation equivalence.
import EchoFrame
import EchoHandler

base : EchoFrame(U64)
base = EchoFrame.default()

main! = |_args| {
    silent = EchoFrame.run!([1, 2, 3], 0, base)
    counted = EchoFrame.register(EchoHandler.(|n| Handled(n * 2)), base)
    doubled = EchoFrame.run!([1, 2, 3], 0, counted)
    echo!(if silent == 0 and doubled == 22 "ok" else "bad")
    Ok({})
}
