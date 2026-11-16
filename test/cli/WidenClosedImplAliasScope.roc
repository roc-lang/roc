# A requirement's dictionary handed on from a scope the lowering enters rather
# than from the enclosing function's own: an alias whose annotation states the
# requirement at the implementation's closed row forwards to a function that
# calls the method at a wider row. Under `.boxy` the alias's scope holds the
# dictionary at the alias's requirement, so it reaches the forwarded function
# adapted to the wider row, or `Ok` is read back under the wider row's
# numbering as `Extra`.
WidenClosedImplAliasScope := {}

Job := [Pending, Failed].{
    status : Job -> [Ok(Str), Err(Str)]
    status = |job| match job {
        Pending => Ok("cv")
        Failed => Err("broken")
    }
}

show : [Ok(Str), Err(Str), Extra] -> Str
show = |v| match v { Ok(s) => "Ok(${s})", Err(e) => "Err(${e})", Extra => "Extra" }

apply = |f, v| f(v)

# A local alias of a generalized local, called and used as a value.
local_alias : a -> Str where [a.status : a -> [Ok(Str), Err(Str)]]
local_alias = |x| {
    widen = |y| y.status()
    narrow : b -> [Ok(Str), Err(Str), Extra] where [b.status : b -> [Ok(Str), Err(Str)]]
    narrow = widen
    show(narrow(x))
}

local_alias_value : a -> Str where [a.status : a -> [Ok(Str), Err(Str)]]
local_alias_value = |x| {
    widen = |y| y.status()
    narrow : b -> [Ok(Str), Err(Str), Extra] where [b.status : b -> [Ok(Str), Err(Str)]]
    narrow = widen
    show(apply(narrow, x))
}

top_widen : c -> [Ok(Str), Err(Str), Extra] where [c.status : c -> [Ok(Str), Err(Str), Extra]]
top_widen = |y| y.status()

# A local alias of a top-level function.
local_alias_of_top : a -> Str where [a.status : a -> [Ok(Str), Err(Str)]]
local_alias_of_top = |x| {
    narrow : b -> [Ok(Str), Err(Str), Extra] where [b.status : b -> [Ok(Str), Err(Str)]]
    narrow = top_widen
    show(narrow(x))
}

# A chain of top-level aliases. Only `top_narrow` states the narrower
# requirement; the aliases either side of it restate their target's.
top_mid = top_widen

top_narrow : b -> [Ok(Str), Err(Str), Extra] where [b.status : b -> [Ok(Str), Err(Str)]]
top_narrow = top_mid

top_last = top_narrow

through_top : a -> Str where [a.status : a -> [Ok(Str), Err(Str)]]
through_top = |x| show(top_last(x))

through_top_value : a -> Str where [a.status : a -> [Ok(Str), Err(Str)]]
through_top_value = |x| show(apply(top_last, x))

repeat : a, U64 -> Str where [a.status : a -> [Ok(Str), Err(Str)]]
repeat = |x, remaining| if remaining == 0 through_top(x) else repeat(x, remaining - 1)

# A method bound to an alias, reached by a direct dispatch and through a
# dictionary.
Holder(a) := [Held(a)].{
    widened : Holder(a) -> [Ok(Str), Err(Str), Extra] where [a.status : a -> [Ok(Str), Err(Str), Extra]]
    widened = |Holder.Held(inner)| inner.status()

    held_status : Holder(a) -> [Ok(Str), Err(Str), Extra] where [a.status : a -> [Ok(Str), Err(Str)]]
    held_status = widened
}

through_dictionary : h -> Str where [h.held_status : h -> [Ok(Str), Err(Str), Extra]]
through_dictionary = |holder| show(holder.held_status())

pending : Holder(Job)
pending = Holder.Held(Job.Pending)

failed : Holder(Job)
failed = Holder.Held(Job.Failed)

expect local_alias(Job.Pending) == "Ok(cv)"
expect local_alias(Job.Failed) == "Err(broken)"
expect local_alias_value(Job.Pending) == "Ok(cv)"
expect local_alias_value(Job.Failed) == "Err(broken)"
expect local_alias_of_top(Job.Pending) == "Ok(cv)"
expect local_alias_of_top(Job.Failed) == "Err(broken)"
expect show(top_last(Job.Pending)) == "Ok(cv)"
expect show(top_last(Job.Failed)) == "Err(broken)"
expect through_top(Job.Pending) == "Ok(cv)"
expect through_top(Job.Failed) == "Err(broken)"
expect through_top_value(Job.Pending) == "Ok(cv)"
expect through_top_value(Job.Failed) == "Err(broken)"
expect repeat(Job.Pending, 3) == "Ok(cv)"
expect repeat(Job.Failed, 3) == "Err(broken)"
expect show(pending.held_status()) == "Ok(cv)"
expect show(failed.held_status()) == "Err(broken)"
expect through_dictionary(pending) == "Ok(cv)"
expect through_dictionary(failed) == "Err(broken)"
