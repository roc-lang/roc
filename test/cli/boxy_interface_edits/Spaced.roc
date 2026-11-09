Spaced := { a : U64, _ : U8, z : U64 }.{
    new : U64 -> Spaced
    new = |n| { a: n + 1, z: 2 }
}
