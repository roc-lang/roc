app "app"
    packages { pf: "." }
    imports []
    provides [main] to pf

main = Cons "World!" (Cons "Hello " Nil)
