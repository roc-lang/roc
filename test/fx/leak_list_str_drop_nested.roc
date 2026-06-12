app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdin
import pf.Stdout

# Leak regression coverage for the pure list-decref builtins:
# - a heap List(Str) dropped unconsumed exercises roc_builtins_list_decref_str
#   (&strListElementDecref in src/builtins/dev_wrappers.zig)
# - a dropped List(List(Str)) exercises roc_builtins_list_decref_flat_list
#   (&flatListElementDecref, which decrefs the inner lists and their strings)
# Both pass address-taken decref callbacks — the pattern the lld-COFF linker
# misresolved for strJoinWithC's &strDecref. Strings come from stdin and are
# >= 24 bytes so the drops free real heap allocations.

heap_str : Str, Str -> Str
heap_str = |seed, tag| Str.repeat(Str.concat(seed, tag), 4)

main! = || {
    seed = Stdin.line!()
    _ = [heap_str(seed, "-x"), heap_str(seed, "-y")]
    nested = [[heap_str(seed, "-n1")], [heap_str(seed, "-n2"), heap_str(seed, "-n3")]]
    _ = nested.drop_at(0)
    Stdout.line!("drop done")
}
