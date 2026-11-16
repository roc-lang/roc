Repro :: [].{
    Attribute : { name : Str, value : Str }
    Store :: [Store({ names : Tree, seed_values : Try(List(Attribute), [Unknown]) })]
    Index :: [SourceIndex(Store)]
    index = |_store| crash "reduced"

    tag_attributes : Index -> List(Attribute)
    tag_attributes = |_index_value| crash "reduced"
    new_parsed = |_namespace, _attributes| crash "reduced"
    materialize : Store -> List(Attribute)
    materialize = |Store(s)| match s.seed_values {
        Ok(values) => values
        Err(_) => all_entries(s.names, []).map(|entry| { name: entry.name, value: entry.value })
    }
    set_parsed : Store, Str, Str -> Store
    set_parsed = |_store, _name, _value| crash "reduced"
}

Entry : { name : Str, value : Str }

Tree := [Tree(List({ entry : Entry }))]

all_entries : Tree, List(Entry) -> List(Entry)
all_entries = |_tree, _entries| crash "reduced"

expect {
    initial = Repro.new_parsed("bounded-recovery", [])
    var $store = Repro.set_parsed(initial, "Aa", "one")
    Store(expanded) = $store
    expected = [{ name: "B@", value: "two" }]
    Store(final) = $store
    expanded.seed_values == Err(Unknown) and final.seed_values == Ok(expected) and Repro.materialize($store) == expected
}

expect {
    initial = Repro.new_parsed("lazy-classification", [{ name: "a", value: "one" }])
    restored = Repro.set_parsed(initial, "a", "one")
    Store(positive) = restored
    positive.seed_values == Ok([{ name: "b", value: "two" }])
}

expect {
    var $store = Repro.new_parsed("buffer-tag-removal", [])
    var $expected = []
    checkpoint = $store
    original = $expected
    Repro.tag_attributes(Repro.index(checkpoint)) == original and Repro.materialize($store) == $expected
}
