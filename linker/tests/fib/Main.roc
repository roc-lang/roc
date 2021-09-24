app "fib"
    packages { base: "platform" }
    imports []
    provides [ main ] to base

main : U64 -> U64
main = \index ->
    fibHelp index 0 1

fibHelp : U64, U64, U64 -> U64
fibHelp = \index, parent, grandparent ->
    if index == 0 then
        parent
    else
        fibHelp (index - 1) grandparent (parent + grandparent)