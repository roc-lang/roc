import B

C := {}.{
    size : Str -> U64
    size = |s| B.run(s).count_utf8_bytes()
}
