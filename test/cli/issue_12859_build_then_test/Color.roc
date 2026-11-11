Color := U64.{
    is_eq : _

    make : U64 -> Color
    make = |value| Color.(value)

    blue = make(42)
}

expect Color.blue == Color.(42)
