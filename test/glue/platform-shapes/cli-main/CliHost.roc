CliHost := [].{
    Point := {
        x : I32,
        y : I32,
    }

    NamedRecord := {
        label : Str,
        count : U64,
        active : Bool,
    }

    Shape : [
        Circle({ radius : F64 }),
        Rect({ width : F64, height : F64 }),
        Empty,
    ]

    log! : Str => {}

    read! : {} => Str

    many! : U8, U16, U32, U64, I8, I16, I32, I64, F32, F64, Bool, Str => Try(Str, [BadInput(Str), Exit(I32)])

    shape! : Shape, { label : Str, point : Point, nested : { enabled : Bool, count : U64 } } => NamedRecord
}
