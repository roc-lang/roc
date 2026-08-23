app [main!] { pf: platform "./platform/main.roc" }

import pf.Host
import pf.Stdin
import pf.Stdout

# Regression coverage for https://github.com/roc-lang/roc/issues/10774: a
# hosted function whose argument is a container with refcounted elements.
#
# `Host.sum_str_bytes!` owns its `List(Str)` argument and releases one
# ownership unit of it, elements included, before returning. `items` is read
# again after the call, so the caller holds a second reference across the
# transfer and the element strings must survive the host's release.
#
# Every string comes from stdin and is well past the small-string threshold,
# so the elements are real heap allocations that the fx host's tracking
# allocator reports on if either side drops one too many or too few times.

heap_str : Str, Str -> Str
heap_str = |seed, tag| Str.repeat(Str.concat(seed, tag), 4)

main! = || {
    seed = Stdin.line!()
    items = [heap_str(seed, "-a"), heap_str(seed, "-b"), heap_str(seed, "-c")]

    hosted_bytes = Host.sum_str_bytes!(items)

    var $local_bytes = 0
    for item in items {
        $local_bytes = $local_bytes + Str.count_utf8_bytes(item)
    }

    Stdout.line!("hosted bytes: ${hosted_bytes.to_str()} local bytes: ${$local_bytes.to_str()}")
}
