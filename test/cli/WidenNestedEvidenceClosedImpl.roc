# polarity_phase_two.md W6b: a CLOSED implementation reached through NESTED
# EVIDENCE. `Wrap.status` carries its own where-clause (`a.name`), so the
# obligation that reaches it drags a second requirement along, while its
# published result row is closed by the top-level `closed_ok` / `closed_err`.
# `describe` uses the method twice in one body—once exhaustively at the
# declared row and once widened (Appendix B's "Both" shape)—so the adapter
# is minted beside an ordinary declared-row specialization of the same
# template.
#
# Both constructors are observed through `show`, and `Extra` sorts between
# `Err` and `Ok`, so a re-tag that used the declared discriminants would show
# up as a wrong branch on either backend.
WidenNestedEvidenceClosedImpl := {}

closed_ok : [Ok(Str), Err(Str)]
closed_ok = Ok("ok")

closed_err : [Ok(Str), Err(Str)]
closed_err = Err("err")

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
