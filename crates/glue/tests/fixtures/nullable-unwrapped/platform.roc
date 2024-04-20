platform "test-platform"
    requires {} { main : _ }
    exposes []
    packages {}
    provides [mainForHost]

StrConsList : [Nil, Cons Str StrConsList]

mainForHost : StrConsList
mainForHost = main
