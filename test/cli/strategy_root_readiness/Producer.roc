# Both closures capture a local nominal at its producer's instantiation.
# Its custom descriptor must not be recreated in the importing consumer.
make : a -> ({} -> Str)
make = |value| {
    Local(a) := [Local(a)].{
        to_inspect = |_value| "producer scope"
    }
    captured = Local.Local(value)
    |_| Str.inspect([captured])
}

saved = { first: make(5.U64), second: make("captured text") }

Producer := [].{
    values = saved
}
