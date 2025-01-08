app [main] { pf: platform "fibonacci-platform/main.roc" }

main = \n -> fib(n, 0, 1)

# the clever implementation requires join points
fib = \n, a, b ->
    if n == 0 then
        a
    else
        fib((n - 1), b, (a + b))
