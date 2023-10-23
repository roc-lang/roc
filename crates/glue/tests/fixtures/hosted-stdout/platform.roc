platform "test-platform"
    requires {} { main : _ }
    exposes []
    packages {}
    imports [Task.{ Task }]
    provides [mainForHost]

mainForHost : Str -> Task {} []
mainForHost = \_ -> main
