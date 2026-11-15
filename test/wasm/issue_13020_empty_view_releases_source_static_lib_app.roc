app [main!] { pf: platform "./static-lib-platform/main.roc" }

# https://github.com/roc-lang/roc/issues/13020
# An empty sublist of a shared list holds nothing of it: once `outer` is
# dropped, `view` keeps neither its allocation nor its items alive. `inner` is
# then uniquely owned again, so appending within its capacity allocates
# nothing: the program allocates only `inner` and `outer`.
main! = |seed| {
    inner = List.with_capacity(8).append(seed.to_u8_wrap())
    outer = [inner]
    view = outer.sublist({ start: 5, len: 1 })
    count = outer.fold(0, |acc, items| acc + items.len())
    grown = inner.append(1)
    found = view.contains(grown)

    if !found and count == 1 and grown.len() == 2 "ok" else "bad"
}
