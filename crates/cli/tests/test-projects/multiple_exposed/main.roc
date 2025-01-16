app [exposed1, exposed2, add1, sub1] { pf: platform "platform/main.roc" }

exposed1 = \n -> fib(n, 0, 1)

fib = \n, a, b ->
    if n == 0 then
        a
    else
        fib((n - 1), b, (a + b))

exposed2 = \n -> fact(n, 1)

fact = \n, x ->
    if n == 0 then
        x
    else
        fact((n - 1), (n * x))

add1 = \n -> n + 1
sub1 = \n -> n - 1
