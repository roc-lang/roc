platform "test-platform"
    requires {} { main : _ }
    exposes []
    packages {}
    provides [mainForHost]

import Dep1
import Dep2

Combined : { s1 : Dep1.DepStr1, s2 : Dep2.DepStr2 }

mainForHost : Combined
mainForHost = main
