app "effect-example"
    packages { base: "platform" }
    imports [base.Cmd]
    provides [ main ] to base

main : I64 
main = 42 
