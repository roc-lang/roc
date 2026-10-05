Callbacks := [].{
    one : U64 -> Str
    one = |value| if value == 0 "one-zero" else "one"

    two : U64 -> Str
    two = |value| if value == 0 "two-zero" else "two"

    make : U64 -> (U64 -> Str)
    make = |offset| |value| Str.inspect(value + offset)
}
