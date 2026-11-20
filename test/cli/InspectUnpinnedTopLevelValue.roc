InspectUnpinnedTopLevelValue := {}

import InspectUnpinnedTopLevelValueDep

# Each of these top-level values has a type with a part that nothing pins and
# that carries no method requirement: an empty list's item type, an absent
# payload, a type argument no value mentions. `Str.inspect` renders every one,
# however the value reaches it.
Wrap(a) := [W(List(a))].{
	to_inspect : Wrap(a) -> Str
	to_inspect = |Wrap.(W(items))| "Wrap<${Str.inspect(items)}>"
}

Phantom(a) := [P(U64)]

Tree(a) := [Leaf, Node(a, Tree(a), Tree(a))]

my_inspect = |v| Str.inspect(v)

empty = []

ok_empty = Ok([])

pair = ([], Ok([]))

rec = { items: [] }

nested = { a: [], b: ([], [[]]), c: Ok({ d: [] }) }

first = List.first([])

dict = Dict.empty()

set = Set.empty()

keyed = Dict.empty().insert("k", [])

wrapped = Wrap.W([])

phantom = Phantom.P(1)

leaf = Tree.Leaf

alias = empty

(left, right) = ([], Ok([]))

shown = Str.inspect(empty)

shown_pair = (Str.inspect(pair), Str.inspect(wrapped))

in_function = |_| Str.inspect(empty)

expect Str.inspect(empty) == "[]"
expect Str.inspect(ok_empty) == "Ok([])"
expect Str.inspect(pair) == "([], Ok([]))"
expect Str.inspect(rec) == "{ items: [] }"
expect Str.inspect(nested) == "{ a: [], b: ([], [[]]), c: Ok({ d: [] }) }"
expect Str.inspect(first) == "Err(ListWasEmpty)"
expect Str.inspect(dict) == "Dict.from_list([])"
expect Str.inspect(set) == "Set.from_list([])"
expect Str.inspect(keyed) == "Dict.from_list([(\"k\", [])])"
expect Str.inspect(wrapped) == "Wrap<[]>"
expect Str.inspect(phantom) == "P(1)"
expect Str.inspect(leaf) == "Leaf"
expect Str.inspect(alias) == "[]"
expect Str.inspect(left) == "[]"
expect Str.inspect(right) == "Ok([])"

# The same values through interpolation, a wrapper function, a field, a
# literal, a compile-time constant, a function body and another module.
expect "${Str.inspect(empty)} and ${Str.inspect(wrapped)}" == "[] and Wrap<[]>"
expect my_inspect(empty) == "[]"
expect my_inspect(pair) == "([], Ok([]))"
expect Str.inspect(rec.items) == "[]"
expect Str.inspect([]) == "[]"
expect shown == "[]"
expect shown_pair == ("([], Ok([]))", "Wrap<[]>")
expect in_function({}) == "[]"
expect Str.inspect(InspectUnpinnedTopLevelValueDep.empty) == "[]"
expect Str.inspect(InspectUnpinnedTopLevelValueDep.pair) == "([], Ok([]))"
expect InspectUnpinnedTopLevelValueDep.shown == "[]"
