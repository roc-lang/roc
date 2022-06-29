app "list_find_roc"
    packages { pf: "platform/main.roc" }
    imports [pf.Task]
    provides [main] to pf

main : Task.Task {} []
main =
    Task.after
        Task.getInt
        \needle ->
            haystack = List.range 0 (needle + 1)
            when find haystack (\x -> x == needle) is
                Ok val ->
                    val
                        |> Num.toStr
                        |> Task.putLine
                _ ->
                    Task.succeed {}

IterationDecision state early : [Continue state, Break early]

iterate : List elem, s, (s, elem -> IterationDecision s b) -> IterationDecision s b
iterate = \list, init, func ->
    iterHelp = \ls, state, f, index ->
        when List.get ls index is
            Err _ ->
                Continue state
            Ok elem ->
                when f state elem is
                    Continue nextState ->
                        iterHelp ls nextState f (index + 2)
                    Break b ->
                        Break b
    iterHelp list init func 0

# iterHelp : List elem, s, (s, elem -> IterationDecision s early), Nat -> IterationDecision s early

find : List elem, (elem -> Bool) -> Result elem {}
find = \array, pred ->
    callback = \_, elem ->
        if pred elem then
            Break elem
        else
            Continue {}
    when iterate array {} callback is
        Continue {} ->
            Err {}
        Break found ->
            Ok found
