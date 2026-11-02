app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout
import pf.Padded

## End-to-end host-interop test for generic nominal records with unnamed padding
## fields. Each record is built by a generic body and read by the host through a
## C `extern struct`, so a generic body has to give it the offsets of its
## declaration and not those of its record row.

## A generic helper that knows only the row. Its record reaches the host as a
## `Padded`, so it has to change order on the way there.
from_row = |z, a| { z, a }

read_a = |row| row.a

stored : Padded
stored = Padded.new(88, 99)

main! = || {
    slot = Padded.Slot.new(11, 22)
    Stdout.line!(Padded.check_slot!(slot))

    spaced : Padded.Spaced(U64)
    spaced = Padded.Spaced.new(33, 44)
    Stdout.line!(Padded.check_spaced!(spaced))

    held = Padded.check_held!(Padded.Held.new(55, "held across the host"))
    Stdout.line!(Padded.Held.unbox(held))
    Stdout.line!(Padded.Held.doubled(held).to_str())

    mixed = Padded.check_mixed!(Padded.Mixed.new(1.5, "mixed across the host"))
    Stdout.line!(Padded.Mixed.unbox(mixed))
    Stdout.line!(Str.inspect(Padded.Mixed.scaled(mixed)))

    lifted : Padded
    lifted = from_row(66, 77)
    Stdout.line!(lifted.check!())
    Stdout.line!(read_a(lifted).to_str())
    Stdout.line!(stored.check!())
}
