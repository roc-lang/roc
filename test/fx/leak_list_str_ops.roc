app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdin
import pf.Stdout

# Leak regression coverage for the refcounted-element list builtins
# (roc_builtins_list_{concat,sublist,drop_at,replace,swap,append,
# release_excess_capacity} in src/builtins/dev_wrappers.zig), which pass
# address-taken element incref/decref callbacks — the same pattern the
# lld-COFF linker misresolved for strJoinWithC's &strDecref.
#
# `base` is used several times, so the early ops see a shared (rc > 1) list
# and take the incref-elements/copy branches; the later chain operates on
# unique lists and takes the in-place + element-decref branches. All strings
# come from stdin and are >= 24 bytes, so the element decrefs free real heap
# allocations that the fx host's tracking allocator would report as leaks.

heap_str : Str, Str -> Str
heap_str = |seed, tag| Str.repeat(Str.concat(seed, tag), 4)

main! = || {
    seed = Stdin.line!()
    base = [heap_str(seed, "-a"), heap_str(seed, "-b"), heap_str(seed, "-c"), heap_str(seed, "-d")]

    dropped = base.drop_at(1)
    sub = base.sublist({ start: 1, len: 2 })
    grown = base.append(heap_str(seed, "-e"))

    both = dropped.concat(sub)
    swapped = match both.swap(0, 1) {
        Ok(l) => l
        Err(_) => []
    }
    replaced = match swapped.replace(2, heap_str(seed, "-f")) {
        Ok(r) => r.list # r.prev (a heap Str) is dropped here
        Err(_) => []
    }
    trimmed = replaced.take_first(3).release_excess_capacity()
    Stdout.line!("ops done: ${trimmed.len().to_str()} ${grown.len().to_str()}")
}
