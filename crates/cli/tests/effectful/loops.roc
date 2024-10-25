app [main!] { pf: platform "../../../../examples/cli/effects-platform/main.roc" }

import pf.Effect

main! : {} => {}
main! = \{} ->
    friends = ["Lu", "Marce", "Joaquin", "Chloé", "Mati", "Pedro"]
    printAll! friends

printAll! : List Str => {}
printAll! = \friends ->
    when friends is
        [] -> {}
        [first, .. as remaining] ->
            Effect.putLine! first
            printAll! remaining
