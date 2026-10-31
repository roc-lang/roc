Shared := { n : U64 }.{
    make : U64 -> Shared
    make = |n| { n: n }

    tell : Shared -> Str
    tell = |s| "right ${s.n.to_str()}"
}
