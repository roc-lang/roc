app "quicksort"
    packages { pf: "quicksort-platform/main.roc" }
    imports []
    provides [quicksort] to pf

quicksort = \originalList ->
    when find originalList (\x -> Str.concat x "!" == "I am a very long string so must be refcounted") is
        Ok _ -> originalList
        Err _ -> List.reverse originalList 


find : List elem, (elem -> Bool) -> Result elem {}
find = \array, pred ->
    callback = \_, elem ->
        if pred elem then
            Break elem
        else
            Continue {}

    when List.iterate array {} callback is
        Continue {} ->
            Err {}
        Break found ->
            Ok found

