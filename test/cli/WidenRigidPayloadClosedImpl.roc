# polarity_phase_two.md W6b: a CLOSED implementation whose row payloads are
# RIGID. `Relay(a).route` publishes `[Ok(a), Err(a)]`, closed because it
# returns its own input-position parameter. `lowerType` of the checked root
# seals a rigid to the empty tag union, so an adapter that took its narrowed
# source type from the lowered declared type would give both payloads a
# zero-sized representation; the narrowed row's payloads must come from the
# REQUEST instead (`Str` here).
#
# Both constructors are routed through the adapter and observed through
# `show`, where `Extra` sorts between `Err` and `Ok`, so a wrong discriminant
# or a wrong payload representation is visible on either backend.
WidenRigidPayloadClosedImpl := {}

Relay(a) := [R(a)].{
    route : Relay(a), [Ok(a), Err(a)] -> [Ok(a), Err(a)]
    route = |_, v| v
}

describe : r, [Ok(Str), Err(Str)] -> [Ok(Str), Err(Str), Extra] where [r.route : r, [Ok(Str), Err(Str)] -> [Ok(Str), Err(Str)]]
describe = |x, v| x.route(v)

show : [Ok(Str), Err(Str), Extra] -> Str
show = |v| match v { Ok(s) => "Ok(${s})", Err(e) => "Err(${e})", Extra => "Extra" }

expect show(describe(Relay.R("seed"), Ok("arg"))) == "Ok(arg)"
expect show(describe(Relay.R("seed"), Err("bad"))) == "Err(bad)"
