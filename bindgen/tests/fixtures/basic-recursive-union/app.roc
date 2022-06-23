app "app"
    packages { pf: "platform.roc" }
    imports []
    provides [main] to pf

main = Concat (String "Hello, ") (String "World!")
