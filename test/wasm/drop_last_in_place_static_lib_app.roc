app [main!] { pf: platform "./static-lib-platform/main.roc" }

# Dropping the last items of a uniquely owned list of plain numbers shrinks it
# in place and keeps its capacity, so refilling it never reallocates: the
# program allocates only `$list` and `kept`. A shared list is cut to views
# that keep the original intact.
main! = |seed| {
    var $list = List.with_capacity(16).append(seed + 1).append(seed + 2).append(seed + 3)
    var $i = 0
    while $i < 100 {
        $list = $list.drop_last(2).append($i).append($i + 1)
        $i = $i + 1
    }
    final_len = $list.len()
    emptied = $list.drop_last(final_len)
    refilled = emptied.append(7)

    kept = [seed + 10, seed + 11, seed + 12]
    front = kept.drop_last(1)
    back = kept.drop_first(1)

    if
        final_len == 3
        and refilled == [7]
        and front == [10, 11]
        and back == [11, 12]
        and kept.len() == 3
        "ok"
    else
        "bad"
}
