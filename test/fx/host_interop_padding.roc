app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout
import pf.Padded

## End-to-end host-interop test for nominal-record layout: a nominal record with
## an unnamed padding field is constructed in Roc and passed by value to a host
## function. The host reads its fields through a C `extern struct` whose offsets
## (z@0, a@8, with four reserved padding bytes at @4) only match if Roc honors
## the declared field order and reserves the unnamed padding. A correct layout
## prints "1122" (z=11 -> 1100, plus a=22).
main! = || {
    padded = Padded.new(11, 22)
    Stdout.line!(padded.check!())
}
