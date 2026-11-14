# A method bound to an alias of a function with a closed result row, handed
# as a dictionary to a consumer that calls the method at a wider row. The
# dictionary's method is the function the alias forwards to, which returns
# its own row, so the dictionary reaches the consumer adapted to the wider
# row, or `Ok` is read back under the wider row's numbering as `Extra`.
WidenClosedImplMethodAlias := {}

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
    inner : Holder(a) -> [Ok(Str), Err(Str)] where [a.status : a -> [Ok(Str), Err(Str)]]
    inner = |Holder.Held(x)| x.status()

    held = inner
}

# The consumer's requirement takes the row its use fixes.
unannotated = |x| x.held()

annotated : h -> [Ok(Str), Err(Str), Extra] where [h.held : h -> [Ok(Str), Err(Str)]]
annotated = |x| x.held()

local_scope : h -> Str where [h.held : h -> [Ok(Str), Err(Str)]]
local_scope = |x| {
    widen = |y| y.held()
    show(widen(x))
}

pending : Holder(Job)
pending = Holder.Held(Job.Pending)

failed : Holder(Job)
failed = Holder.Held(Job.Failed)

expect show(unannotated(pending)) == "Ok(cv)"
expect show(unannotated(failed)) == "Err(broken)"
expect show(annotated(pending)) == "Ok(cv)"
expect show(annotated(failed)) == "Err(broken)"
expect local_scope(pending) == "Ok(cv)"
expect local_scope(failed) == "Err(broken)"
expect show(pending.held()) == "Ok(cv)"
expect show(failed.held()) == "Err(broken)"
