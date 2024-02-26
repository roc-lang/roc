app "helloWorld"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.8.1/x8URkvfyi9I0QhmVG98roKBUs_AZRkLFwFJVJ3942YA.tar.br" }
    imports [pf.Stdout]
    provides [main] to pf

main =
    Stdout.line "Hello, World!"

filterMap : List a, {
        map : (a -> b),
        keepIf ? (b -> Bool),
        dropIf ? (b -> Bool),
        take ? [First U64, Last U64, All]
    } -> List b
filterMap = \list,
    {
        map,
        keepIf ? (\_ -> Bool.true),
        dropIf ? (\_ -> Bool.false),
        take ? All,
    } ->
        beforeTake =
            list
            |> List.map map
            |> List.keepIf keepIf
            |> List.dropIf dropIf

        when take is
            All -> beforeTake
            First n -> beforeTake |> List.takeFirst n
            Last n -> beforeTake |> List.takeLast n
