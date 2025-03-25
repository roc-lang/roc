platform "test-platform"
    requires {} { main : _ }
    exposes []
    packages {}
    imports []
    provides [main_for_host]

StrConsList : [Nil, Cons Str StrConsList]

main_for_host : StrConsList
main_for_host = main
