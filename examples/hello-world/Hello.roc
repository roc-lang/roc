app "hello-world"
    packages { pf: "platform" }
    imports []
    provides [ main ] to pf

main =
    get : {a: Bool} -> Bool
    get = \{a} -> a
    get {b: ""}
