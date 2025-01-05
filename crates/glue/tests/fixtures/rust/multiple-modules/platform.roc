platform "test-platform"
    requires {} { main : _ }
    exposes []
    packages {}
    imports [Dep1, Dep2]
    provides [main_for_host]

Combined : { s1 : Dep1.DepStr1, s2 : Dep2.DepStr2 }

main_for_host : Combined
main_for_host = main
