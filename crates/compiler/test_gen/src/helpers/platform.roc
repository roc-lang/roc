
platform "test-platform"
    requires {} { main: _ }
    exposes []
    packages {}
    provides [mainForHost]

mainForHost = \{} -> main
