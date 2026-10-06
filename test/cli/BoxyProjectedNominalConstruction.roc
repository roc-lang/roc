BoxyProjectedNominalConstruction :: [].{}

Choice(a) := [Empty, Value(a)].{
    is_eq : _
}
Hidden(a) :: [Empty, Value(a)]

read_hidden : Hidden(U64) -> U64
read_hidden = |value| match value {
    Hidden.Empty => 0
    Hidden.Value(payload) => payload
}

expect {
    value : Choice(Choice(U64))
    value = Choice.Value(Choice.Empty)
    match value {
        Choice.Value(Choice.Empty) => Bool.True
        _ => Bool.False
    }
}

expect {
    value : Hidden(U64)
    value = Hidden.Value(42)
    read_hidden(value) == 42
}

expect {
    value : Hidden(U64)
    value = Hidden.Empty
    read_hidden(value) == 0
}

expect {
    left : Choice(Choice(U64))
    left = Choice.Value(Choice.Empty)
    right : Choice(Choice(U64))
    right = Choice.Value(Choice.Value(42))
    left != right
}
