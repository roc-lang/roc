hosted Host
    exposes [open_file!, close_file!, get_file_line!, get_file_bytes!, put_line!, put_raw!, get_line!, get_char!]
    imports []

open_file! : Str => U64

close_file! : U64 => ()

get_file_line! : U64 => Str

get_file_bytes! : U64 => List U8

put_line! : Str => ()

put_raw! : Str => ()

get_line! : () => Str

get_char! : () => U8
