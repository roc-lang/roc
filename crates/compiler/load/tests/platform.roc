platform "test-platform"
    requires {} { main : * }
    exposes []
    packages {}
    imports []
    provides [mainForHost]

mainForHost : {} -> {}
mainForHost = \{} -> {}
