platform "multiple_exposed"
    requires {} { exposed1 : I64 -> I64, exposed2 : I64 -> I64, add1 : I64 -> I64, sub1 : I64 -> I64 }
    exposes []
    packages {}
    imports []
    provides [exposed_for_host1, exposed_for_host2]

exposed_for_host1 : I64 -> I64
exposed_for_host1 = \a -> exposed1(a) |> sub1 |> add1

exposed_for_host2 : I64 -> I64
exposed_for_host2 = \a -> exposed2(a) |> add1 |> sub1
