Point := { x : U64, y : U64 ?? 5 }.{
    sum : Point -> U64
    sum = |p| p.x + p.y
}
