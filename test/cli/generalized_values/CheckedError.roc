# `made` is a generalized value whose body reaches code checking rejected.
# Evaluating its specialization stops at that checked error, which checking
# already reported, so no compile-time crash is reported on top of it.
CheckedError := [].{}

none : List(Str)
none = []

# The checked error is on the path evaluation takes, but not on every path, so
# `made` is still evaluated per specialization.
made : List(a)
made = if List.is_empty(none) {
    _ignored = not_defined_anywhere
    []
} else {
    []
}

expect List.len(List.append(made, 1.U64)) == 1
