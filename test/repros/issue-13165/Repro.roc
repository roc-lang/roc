# https://github.com/roc-lang/roc/issues/13165
Repro := []

candidate : Bool -> U16
candidate = |take_early| {
    (val, _state) = match take_early {
        True => return val
        False => (123, Unchanged)
    }
    val
}
