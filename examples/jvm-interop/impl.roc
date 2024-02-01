app "rocdemo"
    packages { pf: "platform.roc" }
    provides [program] to pf

interpolateString : Str -> Str
interpolateString = \name ->
    "Hello from Roc \(name)!!!🤘🤘🤘"

# jint is i32
mulArrByScalar : List I32, I32 -> List I32
mulArrByScalar = \arr, scalar ->
    List.map arr \x -> x * scalar

# java doesn't have unsigned numbers so we cope with long
# factorial : I64 -> I64
factorial = \n ->
    if n < 0 then
        # while we get the chance,  exemplify a roc panic in an interop
        crash "No negatives here!!!"
    else if n == 0 then
        1
    else
        n * (factorial (n - 1))

program = { interpolateString, factorial, mulArrByScalar }
