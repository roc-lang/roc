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

# `seal` forwards its closed input, which closes its output row, and so the
# row of every value built from it (design.md "Deferred: Row Subsumption").
# This depends on that known limitation (forwarding closes the row): once
# row subsumption lands, this fixture must close its impl row another way.
seal : OkRes([Red, Green]) -> OkRes([Red, Green])
seal = |v| v

closed_value : OkRes([Red, Green])
closed_value = seal(Ok(Red))

Job := [Pending].{
    status : Job -> OkRes([Red, Green])
    status = |_| closed_value
}

main = describe(Job.Pending)
