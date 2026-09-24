# A negative control for the alias type-argument walk.
# `OkRes(a) : Try(a, [IoErr])` puts its formal in the `Try`'s OK position, and
# the result-row widening adapter NEVER re-tags that cell:
# `resultRowWideningOrNull` widens only `args[1]` and relates every other
# argument exactly (src/postcheck/monotype/lower.zig:1806-1812), and
# `hostedTryReturnInjectionExpr` rejects an Ok-type change outright.
#
# So `applyTryErrorArgIndex` must decline this alias and the row must be
# contributed as written: the body use that widens it is an ordinary type
# mismatch at the use rather than a widening no lowering can express. Without
# this control the alias walk would be indistinguishable from "open every
# argument of every alias over `Try`".
WidenAliasOkFormalRow := {}

OkRes(a) : Try(a, [IoErr])

describe : a -> OkRes([Red, Green, Blue]) where [a.status : a -> OkRes([Red, Green])]
describe = |x| x.status()

# `closed_value` is deliberately UNANNOTATED, and its row is read out of a nominal
# field. Neither an annotation nor a forwarding function can produce a closed
# row any more: an annotated value's implicitly opened row is quantified
# (design.md "Polarity"), and a top-level function that FORWARDS a closed value
# has its result row coerced open again at every use (design.md "Row
# Subsumption"). A nominal declaration's body closes its rows as written, so a
# field of `Closed` is a closed source that no coercion reopens, and `closed_value`
# is a top-level constant whose row is closed, which is what this fixture needs.
Closed := { v : OkRes([Red, Green]) }

closed_value = Closed.{ v: Ok(Red) }.v

Job := [Pending].{
    status : Job -> OkRes([Red, Green])
    status = |_| closed_value
}

main = describe(Job.Pending)
