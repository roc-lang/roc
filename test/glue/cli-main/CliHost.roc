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

    ## Sum every UTF-8 byte of an owned list of strings. The host releases the
    ## list before returning, so this exercises the release of a container
    ## whose elements are refcounted. Summing the bytes rather than the lengths
    ## is what makes the payloads observable: a length lives in the list
    ## buffer, so it survives even a release that wrongly frees the strings.
    checksum! : List(Str) => U64

    many! : U8, U16, U32, U64, U128, I8, I16, I32, I64, I128, F32, F64, Dec, Bool, Str => Try(Str, [BadInput(Str), Exit(I32)])

    shape! : Shape, { label : Str, point : Point, nested : { enabled : Bool, count : U64 } } => NamedRecord

    wide! : Dec, I128, U128 => { decimal : Dec, signed : I128, unsigned : U128 }
}
