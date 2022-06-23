app "app"
    packages { pf: "platform.roc" }
    imports []
    provides [main] to pf

main = Cons "World!" (Cons "Hello " Nil)
