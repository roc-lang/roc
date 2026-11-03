Maybe(a) := [
        Nothing,
        Just(a),
].{
        is_just : Maybe(a) -> Bool
        is_just = |m|
                match m {
                        Just(_) => Bool.True
                        Nothing => Bool.False
                }
        just : a -> Maybe(a)
        just = |x| Just(x)
}
