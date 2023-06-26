platform "echo-in-rust"
    requires {} {main : U64}
    exposes []
    packages {}
    imports []
    provides [mainForHost]

Op : [
    StdoutLine Str ({} -> Op),
    StdinLine (Str -> Op),
    None,
]

Task ok err : (Result ok err -> Op) -> Op

stdoutLine : Str -> Task {} *
stdoutLine = \line ->
     \toNext ->
        StdoutLine line \{} -> toNext (Ok {})

# stdinLine : Task Str *
# stdinLine =
#      \toNext ->
#         StdinLine \line -> toNext (Ok line)

mainForHost : Task {} []
mainForHost =
    _ = main

    stdoutLine "foo"
