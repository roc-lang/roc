# Run with: roc test main.roc --no-cache --specialize=no

# repro for https://github.com/roc-lang/roc/issues/11559
#
# Under `--specialize=no`, a Dict or Set that appears as a type argument of
# another generic type must lower and run exactly like it does under the
# default specialization strategy. Expected: all expects below pass. Actual:
# the module panics in boxy lowering with "unresolved bare dynamic
# representation required a static descriptor" as soon as it uses Str.inspect
# alongside an insert into a nested Dict, and with a Set(List(Str)) element
# the run instead panics with "concrete payload layout needed
# descriptor-guided boxing".

insert_inner : Dict(U64, Dict(Str, U64)) -> U64
insert_inner = |d| d.insert(1, Dict.empty()).len()

expect insert_inner(Dict.empty()) == 1

typed : U64
typed = 5

expect Str.inspect(typed) == "5"

set_len : Set(List(Str)) -> U64
set_len = |s| s.len()

expect set_len(Set.empty().insert(["a"])) == 1