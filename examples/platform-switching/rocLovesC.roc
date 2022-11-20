app "rocLovesC"
    packages { pf: "c-platform/main.roc" }
    imports []
    provides [main] to pf

main = foo B

foo = \A -> "abcde"

bar = ""
