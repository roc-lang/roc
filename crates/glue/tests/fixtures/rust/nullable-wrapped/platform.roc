platform "test-platform"
    requires {} { main : _ }
    exposes []
    packages {}
    imports []
    provides [main_for_host]

StrFingerTree : [Empty, Single Str, More Str StrFingerTree]

main_for_host : () -> StrFingerTree
main_for_host = \() -> main
