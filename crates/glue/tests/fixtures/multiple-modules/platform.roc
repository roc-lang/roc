platform "test-platform"
    requires {} { main : _ }
    exposes []
    packages {}
    imports [Dep1, Dep2]
    provides [mainForHost]

Combined : {s1: Dep1.DepStr1, s2: Dep2.DepStr2}

mainForHost : Combined
mainForHost = main
