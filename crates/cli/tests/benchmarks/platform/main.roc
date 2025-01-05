platform "benchmarks"
    requires {} { main! : {} => {} }
    exposes []
    packages {}
    imports []
    provides [main_for_host!]

main_for_host! : {} => {}
main_for_host! = \{} -> main! {}
