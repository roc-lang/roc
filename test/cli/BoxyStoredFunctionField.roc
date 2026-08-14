# A compile-time stored nominal record may contain a function value. Boxy must
# use the stored function's runtime representation without consulting the
# enclosing record's checked type as though it were the function type.
Functions := { inspect : Str -> Str }.{}

functions : Functions
functions = Functions.{ inspect: Str.inspect }

main! = |_args| Ok({})

expect (functions.inspect)("hello") == "\"hello\""
