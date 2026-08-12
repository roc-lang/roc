FieldKinds := [].{
    Config : { count : U8 ?? 10 }

    make : value -> { value : value }
    make = |value| { value: value }
}
