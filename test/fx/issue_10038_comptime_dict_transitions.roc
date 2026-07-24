# Regression coverage for https://github.com/roc-lang/roc/issues/10038
app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout
import pf.Stdin

top_dict = Dict.with_capacity(8).insert("a", "A").insert("b", "B")
top_inner = Dict.single("a", "inner")
top_outer = Dict.single(top_inner, "nested")

print_lookup! = |label, dict, key|
    match Dict.get(dict, key) {
        Ok(value) => Stdout.line!("${label}:${value}")
        Err(_) => Stdout.line!("${label}:missing")
    }

main! = || {
    key = Stdin.line!()
    other_key = if key == "a" "b" else "a"

    print_lookup!("direct", top_dict, key)

    inserted_key = "${key}c"
    inserted = Dict.insert(top_dict, inserted_key, "C")
    print_lookup!("insert-old", inserted, key)
    print_lookup!("insert-new", inserted, inserted_key)

    updated_key = "${key}d"
    updated = Dict.update(top_dict, updated_key, |_| Ok("D"))
    print_lookup!("update-new", updated, updated_key)

    released = Dict.release_excess_capacity(top_dict)
    print_lookup!("release", released, key)

    kept = Dict.keep_if(top_dict, |(candidate, _)| candidate == key)
    print_lookup!("keep", kept, key)

    reserved = Dict.reserve(top_dict, 100)
    print_lookup!("reserve", reserved, key)

    refilled = Dict.insert(Dict.clear(top_dict), key, "refilled")
    print_lookup!("clear", refilled, key)

    mapped = Dict.map(top_dict, |_, value| "${value}!")
    print_lookup!("map", mapped, key)

    removed = Dict.remove(top_dict, key)
    print_lookup!("remove", removed, other_key)

    runtime_inner = Dict.single(key, "inner")
    print_lookup!("nested", top_outer, runtime_inner)

    runtime_dict = Dict.single(key, "runtime")
    print_lookup!("runtime", runtime_dict, key)
}
