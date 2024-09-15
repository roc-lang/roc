app [main] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.15.0/SlwdbJ-3GR7uBWQo6zlmYWNYOxnvo8r6YABXD-45UOw.tar.br" }

import pf.Stdout

main =
    Stdout.line! "Hello, World!"

iterCustom :
    source,
    [Known U64, Unknown],
    (source -> Result (elem, Iter elem) [NoMore])
    -> Iter elem
iterCustom = \source, lenIfKnown, next -> @Iter {
        lenIfKnown,
        # create a thunk which captures source and next
        # (we can't store them directly without
        # adding a second type parameter to Iter)
        next: \{} ->
            when next source is
                # translate Result into [One …, Done, Skip …]
                # (this difference will come up later)
                Ok (elem, iter) -> One elem iter
                Err NoMore -> Done,
    }

Iter elem := {
    lenIfKnown : [Known U64, Unknown],
    next : {} -> [One elem (Iter elem), Skip U64, Done],
}
