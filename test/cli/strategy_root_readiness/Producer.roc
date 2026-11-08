# The annotated record publishes both closures as compile-time values.
# Their local nominal's custom descriptor must retain its producer scope.
make : Str -> ({} -> Str)
make = |value| {
    Local := [Local(Str)].{
        to_inspect = |_value| "producer scope"
    }
    captured = Local.Local(value)
    |_| Str.inspect([captured])
}

saved : { first : ({} -> Str), second : ({} -> Str) }
saved = { first: make("first"), second: make("second") }

Producer := [].{
    values = saved
}
