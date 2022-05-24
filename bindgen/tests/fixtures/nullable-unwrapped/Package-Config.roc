platform "test-platform"
    requires {} { main : _ }
    exposes []
    packages {}
    imports []
    provides [mainForHost]

StrConsList : [Nil, Cons Str StrConsList]

mainForHost : StrConsList
mainForHost = main
