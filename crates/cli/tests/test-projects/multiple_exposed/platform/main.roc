platform "multiple_exposed"
    requires {} { exposed1 : I64 -> I64, exposed2 : I64 -> I64, add1 : I64 -> I64, sub1 : I64 -> I64 }
    exposes []
    packages {}
    imports []
    provides [exposedForHost1, exposedForHost2]

exposedForHost1 : I64 -> I64
exposedForHost1 = \a -> exposed1 a |> sub1 |> add1

exposedForHost2 : I64 -> I64
exposedForHost2 = \a -> exposed2 a |> add1 |> sub1
