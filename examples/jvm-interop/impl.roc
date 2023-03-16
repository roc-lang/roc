app "rocdemo"
    packages { pf: "platform.roc" }
    imports []

    # uncomment to test record
    # provides [program] to pf
    provides [interpolateString] to pf


interpolateString : Str -> Str
interpolateString = \name ->
    "Hello from Roc \(name)!!!🤘🤘🤘"


# jint is i32
# mulArrByScalar : List I32 -> List I32
# mulArrByScalar  = \arr ->
#     List.map arr \x -> x * 2

# program = { interpolateString, mulArrByScalar }

# uncomment to test record
# program = { interpolateString }
