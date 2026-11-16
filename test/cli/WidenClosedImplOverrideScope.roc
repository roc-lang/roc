# A requirement's dictionary handed on to a requirement resolved from a type
# rather than from checked evidence: a nominal's override in a descriptor.
# `Holder(a)`'s `is_eq` calls `a.status` at a wider row than the generic
# functions below state for their own variable, and structural equality of a
# record, tuple, or tag union holding a `Holder(b)` reaches that override
# through the descriptor of `Holder(b)`, whose requirement on `b` is the
# enclosing function's dictionary. Under `.boxy` the function's scope holds
# that dictionary at its own requirement, so it reaches the override adapted
# to the wider row, or `Ok` is read back under the wider row's numbering as
# `Extra`.
WidenClosedImplOverrideScope := {}

Job := [Pending, Failed].{
    status : Job -> [Ok(Str), Err(Str)]
    status = |job| match job {
        Pending => Ok("cv")
        Failed => Err("broken")
    }
}

show : [Ok(Str), Err(Str), Extra] -> Str
show = |v| match v { Ok(s) => "Ok(${s})", Err(e) => "Err(${e})", Extra => "Extra" }

Holder(a) := [Held(a)].{
    is_eq : Holder(a), Holder(a) -> Bool where [a.status : a -> [Ok(Str), Err(Str), Extra]]
    is_eq = |Holder.Held(x), Holder.Held(y)| show(x.status()) == "Ok(cv)" and show(y.status()) == "Ok(cv)"

    held_status : Holder(a) -> [Ok(Str), Err(Str), Extra] where [a.status : a -> [Ok(Str), Err(Str), Extra]]
    held_status = |Holder.Held(x)| x.status()
}

# The override's requirement is one of another override's target: `Outer`'s
# `is_eq` requires `held_status` of `Holder(b)`, whose own requirement on `b`
# is the enclosing function's dictionary.
Outer(a) := [Outer(a)].{
    is_eq : Outer(a), Outer(a) -> Bool where [a.held_status : a -> [Ok(Str), Err(Str), Extra]]
    is_eq = |Outer.Outer(x), Outer.Outer(_y)| show(x.held_status()) == "Ok(cv)"
}

in_record : b -> Bool where [b.status : b -> [Ok(Str), Err(Str)]]
in_record = |x| { held: Holder.Held(x) } == { held: Holder.Held(x) }

in_tuple : b -> Bool where [b.status : b -> [Ok(Str), Err(Str)]]
in_tuple = |x| (Holder.Held(x), 1.U8) == (Holder.Held(x), 1.U8)

in_tag : b -> Bool where [b.status : b -> [Ok(Str), Err(Str)]]
in_tag = |x| {
    left : [Some(Holder(b)), None]
    left = Some(Holder.Held(x))
    right : [Some(Holder(b)), None]
    right = Some(Holder.Held(x))
    left == right
}

nested : b -> Bool where [b.status : b -> [Ok(Str), Err(Str)]]
nested = |x| { outer: Outer.Outer(Holder.Held(x)) } == { outer: Outer.Outer(Holder.Held(x)) }

repeat : b, U64 -> Bool where [b.status : b -> [Ok(Str), Err(Str)]]
repeat = |x, remaining| if remaining == 0 in_record(x) else repeat(x, remaining - 1)

expect in_record(Job.Pending)
expect !in_record(Job.Failed)
expect in_tuple(Job.Pending)
expect !in_tuple(Job.Failed)
expect in_tag(Job.Pending)
expect !in_tag(Job.Failed)
expect nested(Job.Pending)
expect !nested(Job.Failed)
expect repeat(Job.Pending, 3)
expect !repeat(Job.Failed, 3)
