app [main!] { pf: platform "./static-lib-platform/main.roc" }

# https://github.com/roc-lang/roc/issues/13020
# Dropping every item of a uniquely owned list releases the dropped items
# right away, even while the emptied list stays live. `inner` is then
# uniquely owned again, so appending within its capacity allocates nothing:
# the program allocates only `inner` and `outer`.
main! = |seed| {
    inner = List.with_capacity(8).append(seed.to_u8_wrap())
    outer = [inner]
    emptied = outer.drop_last(outer.len())
    grown = inner.append(1)
    found = emptied.contains(grown)

    if !found and grown.len() == 2 "ok" else "bad"
}
