# polarity_phase_two.md W6b negative control for the alias type-argument walk.
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

closed_value : OkRes([Red, Green])
closed_value = Ok(Red)

Job := [Pending].{
    status : Job -> OkRes([Red, Green])
    status = |_| closed_value
}

main = describe(Job.Pending)
