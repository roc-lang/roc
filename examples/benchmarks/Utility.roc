interface Utility exposes [ concatStrList, concatStrListWithSeparator, spaceAboveBelow , showPair, showU8] imports [ ]


spaceAboveBelow: Str -> Str
spaceAboveBelow = \str -> concatStrList ["\n", str, "\n"]


concatStrList : List Str -> Str 
concatStrList = \list -> 
    when List.first list is 
       Ok head -> 
           Str.concat head (concatStrList (List.drop list 1))
       Err _ -> "" 


concatStrListWithSeparator : List Str, Str -> Str 
concatStrListWithSeparator = \list, separator -> 
    when List.first list is 
       Ok head -> 
            # TODO: Not great code following
            rest = List.drop list 1
              if List.len rest > 0 then 
                Str.concat (Str.concat head separator) (concatStrListWithSeparator rest separator)
              else 
                Str.concat head (concatStrListWithSeparator rest separator)
       Err _ -> "" 


showPair : [Pair U8 (List U8)] -> Str
showPair = \(Pair a b) -> 
    when Str.fromUtf8 b is 
        Ok bb -> 
            Str.concat (Str.concat (showU8 a) "::") bb
        _ -> "Could not convert (List U8)"

showU8 : U8 -> Str
showU8 = 
   \u -> when Str.fromUtf8 [u] is 
      Ok str -> str
      _ -> "Oops, could not convert U8"
