platform "test-platform"
    requires {} { main : * }
    exposes []
    packages {}
    provides [mainForHost]

mainForHost : {} -> {}
mainForHost = \{} -> {}
