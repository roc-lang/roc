app "helloZig"
    packages { pf: "main.roc" }
    imports []
    provides [main] to pf


Effect a := {} -> a


effectAfter : Effect a, (a -> Effect b) -> Effect b
effectAfter = \@Effect effect, toEffect ->
    @Effect \{} ->
        when toEffect (effect {}) is
            @Effect thunk -> thunk {}


main = "Hello, World!\n"
