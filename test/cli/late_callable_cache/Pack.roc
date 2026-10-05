Pack := [].{
    mapped : List(U64) -> List(U64)
    mapped = |values| List.map(values, |value| value + 1)
}
