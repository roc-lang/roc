# A CLOSED implementation reached through NESTED EVIDENCE.
# `Wrap.status` carries its own where-clause (`a.name`), so the
# obligation that reaches it drags a second requirement along, while its
# published result row is closed by the top-level `closed_ok` / `closed_err`.
# `describe` uses the method twice in one body—once exhaustively at the
# declared row and once widened—so the adapter is minted beside an ordinary
# declared-row specialization of the same template.
#
# Both constructors are observed through `show`, and `Extra` sorts between
# `Err` and `Ok`, so a re-tag that used the declared discriminants would show
# up as a wrong branch on either backend.
WidenNestedEvidenceClosedImpl := {}

# `closed_ok` / `closed_err` is deliberately UNANNOTATED, and its row is read out of a nominal
# field. Neither an annotation nor a forwarding function can produce a closed
# row any more: an annotated value's implicitly opened row is quantified
# (design.md "Polarity"), and a top-level function that FORWARDS a closed value
# has its result row coerced open again at every use (design.md "Row
# Subsumption"). A nominal declaration's body closes its rows as written, so a
# field of `Closed` is a closed source that no coercion reopens, and
# `closed_ok` / `closed_err` are top-level constants whose rows are closed,
# which is what this fixture needs.
Closed := { v : [Ok(Str), Err(Str)] }

closed_ok = Closed.{ v: Ok("ok") }.v

closed_err = Closed.{ v: Err("err") }.v

Wrap(a) := [W(a)].{
    status : Wrap(a) -> [Ok(Str), Err(Str)] where [a.name : a -> Str]
    status = |w| match w { W(inner) => if inner.name() == "thing" closed_ok else closed_err }
}

Thing := [T].{
    name : Thing -> Str
    name = |_| "thing"
}

Other := [O].{
    name : Other -> Str
    name = |_| "other"
}

describe : x -> [Ok(Str), Err(Str), Extra] where [x.status : x -> [Ok(Str), Err(Str)]]
describe = |x| {
    first = match x.status() {
        Ok(s) => s
        Err(e) => e
    }
    if first == "" Extra else x.status()
}

show : [Ok(Str), Err(Str), Extra] -> Str
show = |v| match v { Ok(s) => "Ok(${s})", Err(e) => "Err(${e})", Extra => "Extra" }

expect show(describe(Wrap.W(Thing.T))) == "Ok(ok)"
expect show(describe(Wrap.W(Other.O))) == "Err(err)"
