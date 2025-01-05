platform "benchmarks"
    requires {} { main : Task {} [] }
    exposes []
    packages {}
    imports []
    provides [main_for_host]

main_for_host : Task {} []
main_for_host = main
