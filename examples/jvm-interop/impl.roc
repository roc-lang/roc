app [program] { pf: platform "platform.roc" }

interpolate_string : Str -> Str
interpolate_string = \name ->
    "Hello from Roc $(name)!!!🤘🤘🤘"

# jint is i32
mul_arr_by_scalar : List I32, I32 -> List I32
mul_arr_by_scalar = \arr, scalar ->
    List.map(arr, \x -> x * scalar)

# java doesn't have unsigned numbers so we cope with long
# factorial : I64 -> I64
factorial = \n ->
    if n < 0 then
        # while we get the chance,  exemplify a roc panic in an interop
        crash("No negatives here!!!")
    else if n == 0 then
        1
    else
        n * (factorial((n - 1)))

program = { interpolate_string, factorial, mul_arr_by_scalar }
