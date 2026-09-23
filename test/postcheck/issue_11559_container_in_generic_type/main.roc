# Run with: roc test main.roc --no-cache --specialize=no

# Regression tests for https://github.com/roc-lang/roc/issues/11559
#
# Under `--specialize=no`, a Dict or Set that appears as a type argument of
# another generic type, or that is inspected, must lower and run exactly like
# it does under the default specialization strategy.

insert_inner : Dict(U64, Dict(Str, U64)) -> U64
insert_inner = |d| d.insert(1, Dict.empty()).len()

expect insert_inner(Dict.empty()) == 1

typed : U64
typed = 5

expect Str.inspect(typed) == "5"

set_len : Set(List(Str)) -> U64
set_len = |s| s.len()

expect set_len(Set.empty().insert(["a"])) == 1

set_list_len : Set(List(U64)) -> U64
set_list_len = |s| s.len()

expect set_list_len(Set.empty().insert([1]).insert([2])) == 2

show_set : Set(a) -> Str
show_set = |s| Str.inspect(s)

show_dict : Dict(k, v) -> Str
show_dict = |d| Str.inspect(d)

expect show_set(Set.empty().insert("a")) == "Set.from_list([\"a\"])"

expect show_dict(Dict.empty().insert("a", 1.U64)) == "Dict.from_list([(\"a\", 1)])"

expect show_dict(Dict.empty().insert(1.U64, Dict.empty().insert("z", [1.U8]))) == "Dict.from_list([(1, Dict.from_list([(\"z\", [1])]))])"

expect Str.inspect(Ok(Set.empty().insert(Set.empty().insert(1.U64)))) == "Ok(Set.from_list([Set.from_list([1])]))"

nested : Dict(U64, Dict(Str, U64))
nested = Dict.empty().insert(1, Dict.empty().insert("z", 9))

expect Str.inspect(nested) == "Dict.from_list([(1, Dict.from_list([(\"z\", 9)]))])"

expect nested.get(1) == Ok(Dict.empty().insert("z", 9))

records : Set({ a: Str })
records = Set.empty().insert({ a: "a" })

expect records.len() == 1

lists : Set(List(Str))
lists = Set.empty().insert(["x", "y"])

expect lists.contains(["x", "y"])

nested_set : Dict(U64, Set(U64))
nested_set = Dict.empty().insert(1, Set.empty().insert(3))

expect Str.inspect(nested_set) == "Dict.from_list([(1, Set.from_list([3]))])"

wrapped : Try(Dict(Str, U64), U64)
wrapped = Ok(Dict.empty().insert("a", 1))

expect Str.inspect(wrapped) == "Ok(Dict.from_list([(\"a\", 1)]))"

expect Dict.empty().insert(1.U64, Dict.empty().insert("a", 2.U8)) == Dict.empty().insert(1, Dict.empty().insert("a", 2))

expect Set.empty().insert(Set.empty().insert(1.U64)) != Set.empty().insert(Set.empty().insert(2))
