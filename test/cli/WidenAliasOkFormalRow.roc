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

# `closed_value` is deliberately UNANNOTATED. An annotated value's implicitly
# opened row is quantified now (design.md "Polarity"), so an annotation can no
# longer produce a closed row at all. `closed` takes the row in an INPUT
# position, where it is generated as written, and returns it, so its result row
# is bound to `[]` by its own body: an input-position parameter is one of the
# closed sources design.md names. `closed_value` is therefore still a top-level
# constant whose row is closed, which is what this fixture needs.
closed : OkRes([Red, Green]) -> OkRes([Red, Green])
closed = |v| v

closed_value = closed(Ok(Red))

Job := [Pending].{
    status : Job -> OkRes([Red, Green])
    status = |_| closed_value
}

main = describe(Job.Pending)
