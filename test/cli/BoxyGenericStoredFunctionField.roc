# A generic nominal record stores a callable and crosses an erased call. Its
# backing argument is first retained as an outer formal, then instantiated at
# the call site; Boxy must resolve both substitution paths to the same exact
# descriptor source.
Functions(value) := { inspect : value -> value }.{}

functions : Functions(Str)
functions = Functions.{ inspect: Str.inspect }

make : value -> Functions(value)
make = |_ignored| Functions.{ inspect: |value| value }

main! = |_args| Ok({})

expect (functions.inspect)("hello") == "\"hello\""
expect (make("ignored").inspect)("hello") == "hello"
