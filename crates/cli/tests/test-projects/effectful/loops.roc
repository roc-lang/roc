app [main!] { pf: platform "../test-platform-effects-zig/main.roc" }

import pf.Effect

main! : {} => {}
main! = \{} ->
    friends = ["Lu", "Marce", "Joaquin", "Chloé", "Mati", "Pedro"]
    print_all!(friends)

print_all! : List Str => {}
print_all! = \friends ->
    when friends is
        [] -> {}
        [first, .. as remaining] ->
            Effect.put_line!(first)
            print_all!(remaining)
