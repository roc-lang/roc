platform "fibonacci"
    requires {} { main : I64 -> I64 }
    exposes []
    packages {}
    imports []
    provides [main_for_host]

main_for_host : I64 -> I64
main_for_host = \a -> main(a)
