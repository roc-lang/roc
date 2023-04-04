platform "test-platform"
    requires {} { main : _ }
    exposes []
    packages {}
    imports []
    provides [mainForHost]

StrFingerTree : [Empty, Single Str, More Str StrFingerTree]

mainForHost : {} -> StrFingerTree
mainForHost = \{} -> main
