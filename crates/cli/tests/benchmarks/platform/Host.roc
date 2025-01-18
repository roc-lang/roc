hosted Host
    exposes [put_line!, put_int!, get_int!]
    imports []

put_line! : Str => {}

put_int! : I64 => {}

get_int! : () => { value : I64, is_error : Bool }
