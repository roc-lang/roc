hosted [put_line!, get_line!, id_effectful!, foo_line!]

put_line! : Str => {}

get_line! : {} => Str

id_effectful! : U64 => U64

foo_line! : Str => Result {} {}
foo_line! = |s| 
    put_line!(s)
    Ok({})