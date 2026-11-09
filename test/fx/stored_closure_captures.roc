app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

# Each closure's body reads its capture only through something an optimizing
# build removes, so its stored value must hold the capture regardless: a
# restore lowers the checked body, which still names it.
apply : (Str, Str -> Str), Str, Str -> Str
apply = |function, first, second| function(first, second)

through_inlined_call : Str -> (Str -> Str)
through_inlined_call = |captured| |text| apply(|_, kept| kept, captured, text)

through_field_read : Str -> (Str -> Str)
through_field_read = |captured| |text| {
    pair = { first: captured, second: text }
    pair.second
}

long_text = "a captured string long enough to live on the heap"

stored_call : Str -> Str
stored_call = through_inlined_call(long_text)

stored_field : Str -> Str
stored_field = through_field_read(long_text)

# Compile-time evaluation restores these two closures and stores them again.
held : { call : (Str -> Str), field : (Str -> Str) }
held = { call: stored_call, field: stored_field }

outer : { inner : { call : (Str -> Str), field : (Str -> Str) } }
outer = { inner: held }

run_call : Str -> Str
run_call = |text| stored_call(text)

run_field : Str -> Str
run_field = |text| stored_field(text)

run_held_call : Str -> Str
run_held_call = |text| (outer.inner.call)(text)

run_held_field : Str -> Str
run_held_field = |text| (outer.inner.field)(text)

main! = || {
    Stdout.line!(run_call("call"))
    Stdout.line!(run_field("field"))
    Stdout.line!(run_held_call("held call"))
    Stdout.line!(run_held_field("held field"))
}
