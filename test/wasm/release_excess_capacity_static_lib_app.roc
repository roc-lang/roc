app [main!] { pf: platform "./static-lib-platform/main.roc" }

# List.release_excess_capacity consumes its list: a copy into an allocation
# of exactly the items' count releases the original, an empty list releases
# into the empty list with no allocation, and a shared list of refcounted
# items keeps every item alive for both owners. Every allocation is released.
main! = |seed| {
    full = List.with_capacity(16).append(seed + 1).append(seed + 2)
    trimmed = full.release_excess_capacity()

    emptied = List.with_capacity(8 + seed).release_excess_capacity()

    words = List.with_capacity(4).append("a word long enough to live on the heap").append("another")
    kept = words.release_excess_capacity()

    if
        trimmed == [1, 2]
        and trimmed.capacity() == 2
        and emptied.len() == 0
        and emptied.capacity() == 0
        and kept == words
        and kept.capacity() == 2
        "ok"
    else
        "bad"
}
